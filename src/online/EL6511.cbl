00001  ID DIVISION.
00003  PROGRAM-ID.                 EL6511.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 13:05:16.
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00008 *                            VMOD=2.019.
00009
00010 *AUTHOR.     LOGIC,INC.
00011 *            DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *                                                                *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025 *REMARKS.    TRANSACTION - EXD2 - REINSURANCE MASTER MAINT
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
00027
00028      EJECT
00029  ENVIRONMENT DIVISION.
00030  DATA DIVISION.
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL6511 WORKING STORAGE    *'.
00034  77  FILLER  PIC X(32)  VALUE '********* VMOD=2.019 ***********'.
00035
00036  01  WS-DATE-AREA.
00037      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00038      05  SAVE-BIN-DATE               PIC X(2)    VALUE SPACES.
00039
00040  01  STANDARD-AREAS.
00041      05  GETMAIN-SPACE               PIC X       VALUE SPACE.
00042      05  MAP-B-NAME                  PIC X(8)    VALUE 'EL6511B'.
00043      05  MAP-C-NAME                  PIC X(8)    VALUE 'EL6511C'.
00044      05  MAPSET-NAME                 PIC X(8)    VALUE 'EL6511S'.
00045      05  TRANS-ID                    PIC X(4)    VALUE 'EXD2'.
00046      05  THIS-PGM                    PIC X(8)    VALUE 'EL6511'.
00047      05  PGM-NAME                    PIC X(8).
00048      05  TIME-IN                     PIC S9(7).
00049      05  TIME-OUT-R  REDEFINES TIME-IN.
00050          10  FILLER                  PIC X.
00051          10  TIME-OUT                PIC 99V99.
00052          10  FILLER                  PIC X(2).
00053      05  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00054      05  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00055      05  XCTL-626                    PIC X(8)    VALUE 'EL126'.
00056      05  XCTL-651                    PIC X(8)    VALUE 'EL651'.
00057      05  LINK-001                    PIC X(8)    VALUE 'EL001'.
00058      05  LINK-004                    PIC X(8)    VALUE 'EL004'.
00059      05  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00060      05  FILE-ID                     PIC X(8)    VALUE  SPACES.
00061      05  REIN-FILE-ID                PIC X(8)    VALUE 'ERREIN'.
00062      05  CNTL-FILE-ID                PIC X(8)    VALUE 'ELCNTL'.
00063      05  BIN-CURRENT-SAVE            PIC XX      VALUE SPACES.
00064
00065      05  WS-FEE-METHOD               PIC X       VALUE SPACES.
00066          88  VALID-FEE-METHOD
00067                        VALUES ARE ' ' '1' '2' 'P'.
00068      05  WS-FEE-BASIS                PIC X       VALUE SPACES.
00069          88  VALID-FEE-BASIS
00070               VALUES ARE ' ' '1' '2' '3' '4' '5' '6' '7' '8'.
00071
00072      05  WS-SAVE-KEY.
00073          10  WS-SAVE-CO-CD           PIC X       VALUE SPACE.
00074          10  WS-SAVE-CODE            PIC X       VALUE SPACE.
00075          10  WS-SAVE-COMP            PIC X(3)    VALUE SPACES.
00076          10  WS-SAVE-COMP-SUB        PIC X(3)    VALUE SPACES.
00077
00078      05  WS-CARRIER-FOUND-SW         PIC X       VALUE SPACE.
00079          88  CARRIER-FOUND                       VALUE 'Y'.
00080
00081      05  CARRIER-ACCESS.
00082          10  FILLER                  PIC X(3)    VALUE SPACES.
00083          10  WS-CARRIER              PIC X       VALUE SPACES.
00084
00085  01  MISC-WORK-AREAS.
00086      05  WS-DATE.
00087          10 WS-MO                    PIC XX.
00088          10 WS-DA                    PIC XX.
00089          10 WS-YR                    PIC XX.
00090      05  WS-PHONE-IN                 PIC 9(10).
00091      05  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.
00092          10  WSPI-AREA               PIC X(3).
00093          10  WSPI-PFX                PIC X(3).
00094          10  WSPI-SFX                PIC X(4).
00095      05  WS-PHONE-OUT.
00096          10  WSPO-AREA               PIC X(3).
00097          10  FILLER                  PIC X       VALUE '-'.
00098          10  WSPO-PFX                PIC X(3).
00099          10  FILLER                  PIC X       VALUE '-'.
00100          10  WSPO-SFX                PIC X(4).
00101
00102      05  DEEDIT-FIELD                PIC X(15).
00103      05  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD PIC S9(15).
00104      05  DEEDIT-FIELD-V1  REDEFINES DEEDIT-FIELD PIC S9(13)V99.
00105
00106      05  WS-TOT-PERCENT              PIC S9V9999 VALUE +0.
00107      05  SUB1                        PIC S9(4)   VALUE +0    COMP.
00108      05  SUB2                        PIC S9(4)   VALUE +0    COMP.
00109      05  SC-ITEM                     PIC S9(4)   VALUE +1    COMP.
00110      05  ERREIN-LENGTH               PIC S9(4)   VALUE +4023 COMP.
00111      05  ELCNTL-LENGTH               PIC S9(4)   VALUE +773  COMP.
00112      05  WS-JOURNAL-FILE-LENGTH      PIC S9(4)   VALUE +0    COMP.
00113      05  MORTCD-LENGTH               PIC S9(4)   VALUE +4    COMP.
00114 **   05  DATE-TEST-AREA              PIC 9(6).
00115 **   05  DATE-TEST-AREA-R  REDEFINES DATE-TEST-AREA.
00116 **       10  DATE-TEST-MM            PIC 99.
00117 **       10  DATE-TEST-DD            PIC 99.
00118 **       10  DATE-TEST-YY            PIC 99.
00119 **   05  WS-DISP-DATE                PIC X(8)    VALUE '00/00/00'.
00120 **   05  WS-INC-DATE                 PIC X(6)    VALUE ZEROS.
00121 **   05  WS-ERN-START                PIC X(6)    VALUE ZEROS.
00122 **   05  WS-ERN-STOP                 PIC X(6)    VALUE ZEROS.
LGC192     05  WS-INC-DATE                 PIC 9(11)  COMP-3
LGC192                                                VALUE ZEROS.
LGC192     05  WS-ERN-START                PIC 9(11)  COMP-3
LGC192                                                VALUE ZEROS.
LGC192     05  WS-ERN-STOP                 PIC 9(11)  COMP-3
LGC192                                                VALUE ZEROS.
00123      05  DIVIDE-RESULT               PIC 99.
00124      05  DIVIDE-REMAINDER            PIC 9.
00125      05  WS-LF-THRU1                 PIC S9(07)V99   VALUE +0.
00126      05  WS-LF-THRU2                 PIC S9(07)V99   VALUE +0.
00127      05  WS-LF-THRU3                 PIC S9(07)V99   VALUE +0.
00128      05  WS-LF-THRU4                 PIC S9(07)V99   VALUE +0.
00129      05  WS-LF-THRU5                 PIC S9(07)V99   VALUE +0.
00130      05  WS-LF-THRU6                 PIC S9(07)V99   VALUE +0.
00131      05  WS-AH-THRU1                 PIC S9(07)V99   VALUE +0.
00132      05  WS-AH-THRU2                 PIC S9(07)V99   VALUE +0.
00133      05  WS-AH-THRU3                 PIC S9(07)V99   VALUE +0.
00134      05  WS-AH-THRU4                 PIC S9(07)V99   VALUE +0.
00135      05  WS-AH-THRU5                 PIC S9(07)V99   VALUE +0.
00136      05  WS-AH-THRU6                 PIC S9(07)V99   VALUE +0.
00137      05  WS-CUSTODIAL-BAL            PIC S9(07)V99   VALUE +0.
00138      05  WS-LF-CLM-MAX               PIC S9(07)V99   VALUE +0.
00139      05  WS-AH-CLM-MAX               PIC S9(07)V99   VALUE +0.
00140
00141      05  WS-CHECK-METHOD             PIC X       VALUE SPACE.
00142          88  VALID-LIFE-METHOD              VALUE 'P' 'E' 'M' ' '.
00143          88  VALID-AH-METHOD                VALUE 'P' 'E' ' '.
00144
00145      05  WS-CHECK-ZERO-FEE           PIC X       VALUE SPACE.
00146          88  VALID-ZERO-FEE                 VALUE 'Y' 'N' 'P'
00147                                                   'E' ' '.
00148      05  WS-CHECK-TAX                PIC X      VALUE SPACE.
00149          88  VALID-TAX                          VALUE 'P' 'E'
111413                                                  'M' ' '.
00150
00151      05  WS-CHECK-TAX-OPTION         PIC X      VALUE SPACE.
00152          88  VALID-TAX-OPTION                   VALUE 'Y' 'N'
00153                                                       'F' ' '.
00154
00155      05  WS-CHECK-CLAIM              PIC X      VALUE SPACE.
00156          88  VALID-CLAIM                        VALUE 'P' 'I' 'X'
00157                                                       'Y' ' '.
00158      05  WS-CHECK-COMMISSION         PIC X      VALUE SPACE.
00159          88  VALID-COMMISSION                   VALUE 'P' 'E' ' '.
00160
00161      05  WS-CHECK-PRINT-OPT          PIC X      VALUE SPACE.
00162          88  VALID-PRINT-OPT                    VALUE 'Y' 'F' 'A'
00163                                                       'N' ' '.
00164      05  WS-CHECK-RPT-OPTION         PIC X      VALUE SPACE.
00165          88  VALID-REPORT-OPTION                VALUE ' ' 'Y' 'N'.
00166
00167      05  WS-CESSION-TYPE             PIC X      VALUE SPACE.
00168          88  VALID-CESSION-TYPE                 VALUE ' ' 'C' 'A'
00169                                                       'P'.
00170      EJECT
00171      05  ERROR-MESSAGES.
00172          10  ER-0000                 PIC X(4)    VALUE '0000'.
00173          10  ER-0004                 PIC X(4)    VALUE '0004'.
00174          10  ER-0008                 PIC X(4)    VALUE '0008'.
00175          10  ER-0029                 PIC X(4)    VALUE '0029'.
00176          10  ER-0050                 PIC X(4)    VALUE '0050'.
00177          10  ER-0068                 PIC X(4)    VALUE '0068'.
00178          10  ER-0070                 PIC X(4)    VALUE '0070'.
00179          10  ER-0142                 PIC X(4)    VALUE '0142'.
00180          10  ER-0589                 PIC X(4)    VALUE '0589'.
00181          10  ER-0648                 PIC X(4)    VALUE '0648'.
00182          10  ER-0649                 PIC X(4)    VALUE '0649'.
00183          10  ER-0650                 PIC X(4)    VALUE '0650'.
00184          10  ER-0651                 PIC X(4)    VALUE '0651'.
00185          10  ER-0652                 PIC X(4)    VALUE '0652'.
00186          10  ER-0653                 PIC X(4)    VALUE '0653'.
00187          10  ER-0763                 PIC X(4)    VALUE '0763'.
032707         10  ER-0875                 PIC X(4)    VALUE '0875'.
00188          10  ER-2039                 PIC X(4)    VALUE '2039'.
00189          10  ER-2055                 PIC X(4)    VALUE '2055'.
00190          10  ER-2056                 PIC X(4)    VALUE '2056'.
00191          10  ER-2067                 PIC X(4)    VALUE '2067'.
00192          10  ER-2103                 PIC X(4)    VALUE '2103'.
00193          10  ER-2139                 PIC X(4)    VALUE '2139'.
00194          10  ER-2140                 PIC X(4)    VALUE '2140'.
00195          10  ER-2143                 PIC X(4)    VALUE '2143'.
00196          10  ER-2144                 PIC X(4)    VALUE '2144'.
00197          10  ER-2145                 PIC X(4)    VALUE '2145'.
00198          10  ER-2146                 PIC X(4)    VALUE '2146'.
00199          10  ER-2147                 PIC X(4)    VALUE '2147'.
00200          10  ER-2148                 PIC X(4)    VALUE '2148'.
00201          10  ER-2149                 PIC X(4)    VALUE '2149'.
00202          10  ER-2208                 PIC X(4)    VALUE '2208'.
00203          10  ER-2237                 PIC X(4)    VALUE '2237'.
00204          10  ER-2301                 PIC X(4)    VALUE '2301'.
00205          10  ER-2302                 PIC X(4)    VALUE '2302'.
00206          10  ER-2303                 PIC X(4)    VALUE '2303'.
00207          10  ER-2304                 PIC X(4)    VALUE '2304'.
00208          10  ER-2305                 PIC X(4)    VALUE '2305'.
00209          10  ER-2306                 PIC X(4)    VALUE '2306'.
00210          10  ER-2307                 PIC X(4)    VALUE '2307'.
00211          10  ER-2308                 PIC X(4)    VALUE '2308'.
00212          10  ER-2309                 PIC X(4)    VALUE '2309'.
00213          10  ER-2312                 PIC X(4)    VALUE '2312'.
00214          10  ER-2313                 PIC X(4)    VALUE '2313'.
00215          10  ER-2314                 PIC X(4)    VALUE '2314'.
00216          10  ER-2315                 PIC X(4)    VALUE '2315'.
00217          10  ER-2340                 PIC X(4)    VALUE '2340'.
00218          10  ER-2355                 PIC X(4)    VALUE '2355'.
00219          10  ER-2386                 PIC X(4)    VALUE '2386'.
00220          10  ER-2391                 PIC X(4)    VALUE '2391'.
00221          10  ER-7098                 PIC X(4)    VALUE '7098'.
00222          10  ER-7349                 PIC X(4)    VALUE '7349'.
00223          10  ER-7350                 PIC X(4)    VALUE '7350'.
00224          10  ER-7351                 PIC X(4)    VALUE '7351'.
00225          10  ER-7805                 PIC X(4)    VALUE '7805'.
00226
00227      05  ELCNTL-KEY.
00228          10  CNTL-COMP-ID            PIC X(3)    VALUE SPACES.
00229          10  CNTL-REC-TYPE           PIC X       VALUE SPACES.
00230          10  CNTL-ACCESS             PIC X(4)    VALUE SPACES.
00231          10  CNTL-SEQ-NO             PIC S9(4)   VALUE +0  COMP.
00232
00233 /
00234 *                          COPY ELCREINV.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREINV                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.001                          *
00006 *                                                                *
00007 *   WORKING STORAGE FOR DATE VARIABLES CREATED                   *
00008 *   FOR THE YEAR 2000 DATE MODIFICATION                          *
00009 *                                                                *
00010 ******************************************************************
00011
00012  01  WS-REINSURANCE-RECORD-DATE.
00013      05  WS-RE-LO-DATE.
00014          10 FILLER                     PIC 999.
00015          10 RE-LO-CCYY                 PIC 9(04).
00016          10 RE-LO-CCYR  REDEFINES  RE-LO-CCYY.
00017             15 RE-LO-CC                PIC 99.
00018             15 RE-LO-YR                PIC 99.
00019          10 RE-LO-MO                   PIC 99.
00020          10 RE-LO-DA                   PIC 99.
00021      05  WS-RE-LO-DATE-N REDEFINES
00022             WS-RE-LO-DATE              PIC 9(11).
00023      05  WS-RE-HI-DATE.
00024          10 FILLER                     PIC 999.
00025          10 RE-HI-CCYY                 PIC 9(04).
00026          10 RE-HI-CCYR  REDEFINES  RE-HI-CCYY.
00027             15 RE-HI-CC                PIC 99.
00028             15 RE-HI-YR                PIC 99.
00029          10 RE-HI-MO                   PIC 99.
00030          10 RE-HI-DA                   PIC 99.
00031      05  WS-RE-HI-DATE-N REDEFINES
00032             WS-RE-HI-DATE              PIC 9(11).
00033      05  WS-RE-CLM-INCURRED-LIM.
00034          10 FILLER                     PIC 999.
00035          10 RE-CLM-CCYY                PIC 9(04).
00036          10 RE-CLM-CCYR  REDEFINES  RE-CLM-CCYY.
00037             15 RE-CLM-CC               PIC 99.
00038             15 RE-CLM-YR               PIC 99.
00039          10 RE-CLM-MO                  PIC 99.
00040          10 RE-CLM-DA                  PIC 99.
00041      05  WS-RE-CLM-INCURRED-LIM-N REDEFINES
00042             WS-RE-CLM-INCURRED-LIM     PIC 9(11).
00043      05  WS-RE-EARNING-START-DT.
00044          10  FILLER                    PIC 999.
00045          10  RE-EARN-CCYY              PIC 9(04).
00046          10  RE-EARN-CCYR  REDEFINES  RE-EARN-CCYY.
00047              15  RE-EARN-CC            PIC 99.
00048              15  RE-EARN-YR            PIC 99.
00049          10  RE-EARN-MO                PIC 99.
00050          10  RE-EARN-DA                PIC 99.
00051      05  WS-RE-EARNING-START-DT-N REDEFINES
00052             WS-RE-EARNING-START-DT     PIC 9(11).
00053      05  WS-RE-EARNING-STOP-DT.
00054          10  FILLER                    PIC 999.
00055          10  RE-EARN-STOP-CCYY         PIC 9(04).
00056          10  RE-EARN-STOP-CCYR  REDEFINES  RE-EARN-STOP-CCYY.
00057              15  RE-EARN-STOP-CC       PIC 99.
00058              15  RE-EARN-STOP-YR       PIC 99.
00059          10  RE-EARN-STOP-MO           PIC 99.
00060          10  RE-EARN-STOP-DA           PIC 99.
00061      05  WS-RE-EARNING-STOP-DT-N REDEFINES
00062             WS-RE-EARNING-STOP-DT      PIC 9(11).
00235  EJECT
00236 *                          COPY ELCSCTM.
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
00237  EJECT
00238 *                          COPY ELCSCRTY.
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
00239      EJECT
00240 *                                    COPY ELCDATE.
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
00241      EJECT
00242 *                                    COPY ELCLOGOF.
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
00243      EJECT
00244 *                                    COPY ELCATTR.
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
00245      EJECT
00246 *                                    COPY ELCEMIB.
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
00247      EJECT
00248 *                                    COPY ELCINTF.
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
00249      12  PI-WORK-AREA REDEFINES PI-PROGRAM-WORK-AREA.
00250          16  PI-CHECK-MAINT-TYPE     PIC X.
00251              88  VALID-MAINT-TYPE                VALUE 'S' 'A'
00252                                                        'C' 'D'.
00253              88  ADD-FUNCTION                    VALUE 'A'.
00254              88  SHOW-FUNCTION                   VALUE 'S'.
00255              88  DELETE-FUNCTION                 VALUE 'D'.
00256              88  CHANGE-FUNCTION                 VALUE 'C'.
00257
00258          16  PI-PREV-MAINTYP         PIC X.
00259
00260          16  PI-ERREIN-KEY.
00261              20  PI-CRR-COMPANY-CD   PIC X.
00262              20  PI-CRR-CODE         PIC X.
00263              20  PI-CRR-TABLE        PIC X(3).
00264              20  PI-CRR-TABLE-SUB    PIC X(3).
00265
00266          16  PI-START-LEVEL          PIC 9(2).
00267
00268          16  PI-SAVE-ERREIN-KEY      PIC X(8).
00269
00270          16  PI-FIRST-TIME-SW        PIC X.
00271              88  FIRST-TIME                      VALUE 'Y'.
00272          16  PI-ENTRY-SW             PIC X.
00273              88  NOT-FIRST-ENTRY            VALUE 'Y'.
00274          16  PI-BROWSE-SW            PIC X.
00275              88  BROWSE-STARTED             VALUE 'Y'.
00276          16  PI-ERREIN-EOF-SW        PIC X.
00277              88  ERREIN-EOF                 VALUE 'Y'.
00278          16  PI-EXCESS-SW            PIC X.
00279              88  EXCESS-LEVEL-EXISTS        VALUE 'X'.
00280          16  PI-COMPANY-ADD-SW       PIC X.
00281              88  COMPANY-RECORD-ADDED       VALUE 'Y'.
00282
00283          16  PI-SUB                  PIC S99.
00284          16  PI-LAST-LEVEL           PIC S99.
00285
00286          16  PI-SAVE-TABLE           PIC X(3).
00287          16  PI-SAVE-COMPANY         PIC X(3).
00288          16  PI-SAVE-COMP-SUB        PIC X(3).
00289          16  PI-MAPNAME              PIC X(8).
00290              88  PI-MAP-B                   VALUE 'EL6511B '.
00291              88  PI-MAP-C                   VALUE 'EL6511C '.
00292
00293          16  FILLER                  PIC X(593).
00294      EJECT
00295 *                                    COPY ELCJPFX.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCJPFX.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *    USER DATA FOR SYSTEM JOURNAL RECORDS  JOURNAL I.D. = "EL"   *
00008 *                                                                *
00009 *     ALL RECORDS ARE JOURNALED FOR ERROR RECOVERY               *
00010 *     FILES JOURNALED FOR AUDIT TRAIL (BEFORE CHANGE) ARE -      *
00011 *        ELCNTL - CONTROL FILE                                   *
00012 *        ELMSTR - CLAIM MASTERS                                  *
00013 *        ELTRLR - ACTIVITY TRAILERS                              *
00014 *        ELCHKQ - CHECK QUE                                      *
00015 ******************************************************************
00016  01  JOURNAL-RECORD.
           12  jp-date                     pic s9(5) comp-3.
           12  jp-time                     pic s9(7) comp-3.
00017      12  JP-USER-ID                  PIC X(4).
00018      12  JP-FILE-ID                  PIC X(8).
00019      12  JP-PROGRAM-ID               PIC X(8).
00020      12  JP-RECORD-TYPE              PIC X.
00021          88 JP-ADD              VALUE 'A'.
00022          88 JP-BEFORE-CHANGE    VALUE 'B'.
00023          88 JP-AFTER-CHANGE     VALUE 'C'.
00024          88 JP-DELETE           VALUE 'D'.
00025          88 JP-GENERIC-DELETE   VALUE 'G'.
00026          88 JP-KEY-CHG-DELETE   VALUE 'K'.
00027          88 JP-KEY-CHG-ADD      VALUE 'N'.
00028      12  JP-GENERIC-KEY-LENGTH       PIC S9(4)   COMP.
00029      12  JP-RECORD-AREA
00030
00031
00296                                      PIC X(4000).
00297
00298      EJECT
00299 *                                    COPY ELCAID.
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
051007*00039    02  DFHPF22   PIC  X  VALUE  '?'.
051007   02  DFHPF22   PIC  X  VALUE  '['.
00040    02  DFHPF23   PIC  X  VALUE  '.'.
00041    02  DFHPF24   PIC  X  VALUE  '<'.
00042    02  DFHMSRE   PIC  X  VALUE  'X'.
00043    02  DFHSTRF   PIC  X  VALUE  'h'.
00044    02  DFHTRIG   PIC  X  VALUE  '"'.
00300
00301  01  FILLER    REDEFINES DFHAID.
00302      05  FILLER                      PIC X(8).
00303      05  PF-VALUES                   PIC X       OCCURS 2.
00304
00305      EJECT
00306 *                                    COPY EL6511S.
       01  EL6511CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  CDATEL PIC S9(0004) COMP.
           05  CDATEF PIC  X(0001).
           05  FILLER REDEFINES CDATEF.
               10  CDATEA PIC  X(0001).
           05  CDATEI PIC  X(0008).
      *    -------------------------------
           05  CTIMEL PIC S9(0004) COMP.
           05  CTIMEF PIC  X(0001).
           05  FILLER REDEFINES CTIMEF.
               10  CTIMEA PIC  X(0001).
           05  CTIMEI PIC  X(0005).
      *    -------------------------------
           05  COMPL PIC S9(0004) COMP.
           05  COMPF PIC  X(0001).
           05  FILLER REDEFINES COMPF.
               10  COMPA PIC  X(0001).
           05  COMPI PIC  X(0003).
      *    -------------------------------
           05  SUBL PIC S9(0004) COMP.
           05  SUBF PIC  X(0001).
           05  FILLER REDEFINES SUBF.
               10  SUBA PIC  X(0001).
           05  SUBI PIC  X(0003).
      *    -------------------------------
           05  CCARL PIC S9(0004) COMP.
           05  CCARF PIC  X(0001).
           05  FILLER REDEFINES CCARF.
               10  CCARA PIC  X(0001).
           05  CCARI PIC  X(0001).
      *    -------------------------------
           05  DESC1L PIC S9(0004) COMP.
           05  DESC1F PIC  X(0001).
           05  FILLER REDEFINES DESC1F.
               10  DESC1A PIC  X(0001).
           05  DESC1I PIC  X(0079).
      *    -------------------------------
           05  DESC2L PIC S9(0004) COMP.
           05  DESC2F PIC  X(0001).
           05  FILLER REDEFINES DESC2F.
               10  DESC2A PIC  X(0001).
           05  DESC2I PIC  X(0079).
      *    -------------------------------
           05  DESC3L PIC S9(0004) COMP.
           05  DESC3F PIC  X(0001).
           05  FILLER REDEFINES DESC3F.
               10  DESC3A PIC  X(0001).
           05  DESC3I PIC  X(0079).
      *    -------------------------------
           05  DESC4L PIC S9(0004) COMP.
           05  DESC4F PIC  X(0001).
           05  FILLER REDEFINES DESC4F.
               10  DESC4A PIC  X(0001).
           05  DESC4I PIC  X(0079).
      *    -------------------------------
           05  DESC5L PIC S9(0004) COMP.
           05  DESC5F PIC  X(0001).
           05  FILLER REDEFINES DESC5F.
               10  DESC5A PIC  X(0001).
           05  DESC5I PIC  X(0079).
      *    -------------------------------
           05  DESC6L PIC S9(0004) COMP.
           05  DESC6F PIC  X(0001).
           05  FILLER REDEFINES DESC6F.
               10  DESC6A PIC  X(0001).
           05  DESC6I PIC  X(0079).
      *    -------------------------------
           05  DESC7L PIC S9(0004) COMP.
           05  DESC7F PIC  X(0001).
           05  FILLER REDEFINES DESC7F.
               10  DESC7A PIC  X(0001).
           05  DESC7I PIC  X(0079).
      *    -------------------------------
           05  DESC8L PIC S9(0004) COMP.
           05  DESC8F PIC  X(0001).
           05  FILLER REDEFINES DESC8F.
               10  DESC8A PIC  X(0001).
           05  DESC8I PIC  X(0079).
      *    -------------------------------
           05  DESC9L PIC S9(0004) COMP.
           05  DESC9F PIC  X(0001).
           05  FILLER REDEFINES DESC9F.
               10  DESC9A PIC  X(0001).
           05  DESC9I PIC  X(0079).
      *    -------------------------------
           05  DESC10L PIC S9(0004) COMP.
           05  DESC10F PIC  X(0001).
           05  FILLER REDEFINES DESC10F.
               10  DESC10A PIC  X(0001).
           05  DESC10I PIC  X(0079).
      *    -------------------------------
           05  DESC11L PIC S9(0004) COMP.
           05  DESC11F PIC  X(0001).
           05  FILLER REDEFINES DESC11F.
               10  DESC11A PIC  X(0001).
           05  DESC11I PIC  X(0079).
      *    -------------------------------
           05  DESC12L PIC S9(0004) COMP.
           05  DESC12F PIC  X(0001).
           05  FILLER REDEFINES DESC12F.
               10  DESC12A PIC  X(0001).
           05  DESC12I PIC  X(0079).
      *    -------------------------------
           05  DESC13L PIC S9(0004) COMP.
           05  DESC13F PIC  X(0001).
           05  FILLER REDEFINES DESC13F.
               10  DESC13A PIC  X(0001).
           05  DESC13I PIC  X(0079).
      *    -------------------------------
           05  DESC14L PIC S9(0004) COMP.
           05  DESC14F PIC  X(0001).
           05  FILLER REDEFINES DESC14F.
               10  DESC14A PIC  X(0001).
           05  DESC14I PIC  X(0079).
      *    -------------------------------
           05  DESC15L PIC S9(0004) COMP.
           05  DESC15F PIC  X(0001).
           05  FILLER REDEFINES DESC15F.
               10  DESC15A PIC  X(0001).
           05  DESC15I PIC  X(0079).
      *    -------------------------------
           05  DESC16L PIC S9(0004) COMP.
           05  DESC16F PIC  X(0001).
           05  FILLER REDEFINES DESC16F.
               10  DESC16A PIC  X(0001).
           05  DESC16I PIC  X(0079).
      *    -------------------------------
           05  DESC17L PIC S9(0004) COMP.
           05  DESC17F PIC  X(0001).
           05  FILLER REDEFINES DESC17F.
               10  DESC17A PIC  X(0001).
           05  DESC17I PIC  X(0079).
      *    -------------------------------
           05  DESC18L PIC S9(0004) COMP.
           05  DESC18F PIC  X(0001).
           05  FILLER REDEFINES DESC18F.
               10  DESC18A PIC  X(0001).
           05  DESC18I PIC  X(0079).
      *    -------------------------------
           05  CERRMSGL PIC S9(0004) COMP.
           05  CERRMSGF PIC  X(0001).
           05  FILLER REDEFINES CERRMSGF.
               10  CERRMSGA PIC  X(0001).
           05  CERRMSGI PIC  X(0077).
      *    -------------------------------
           05  CENTERL PIC S9(0004) COMP.
           05  CENTERF PIC  X(0001).
           05  FILLER REDEFINES CENTERF.
               10  CENTERA PIC  X(0001).
           05  CENTERI PIC  99.
       01  EL6511CO REDEFINES EL6511CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUBO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC3O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC4O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC5O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC6O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC7O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC8O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC9O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC10O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC11O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC12O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC13O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC14O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC15O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC16O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC17O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DESC18O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERRMSGO PIC  X(0077).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CENTERO PIC  99.
      *    -------------------------------
       01  EL6511BI REDEFINES EL6511CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  BDATEL PIC S9(0004) COMP.
           05  BDATEF PIC  X(0001).
           05  FILLER REDEFINES BDATEF.
               10  BDATEA PIC  X(0001).
           05  BDATEI PIC  X(0008).
      *    -------------------------------
           05  BTIMEL PIC S9(0004) COMP.
           05  BTIMEF PIC  X(0001).
           05  FILLER REDEFINES BTIMEF.
               10  BTIMEA PIC  X(0001).
           05  BTIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTYPL PIC S9(0004) COMP.
           05  MAINTYPF PIC  X(0001).
           05  FILLER REDEFINES MAINTYPF.
               10  MAINTYPA PIC  X(0001).
           05  MAINTYPI PIC  X(0001).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  COMPSUBL PIC S9(0004) COMP.
           05  COMPSUBF PIC  X(0001).
           05  FILLER REDEFINES COMPSUBF.
               10  COMPSUBA PIC  X(0001).
           05  COMPSUBI PIC  X(0003).
      *    -------------------------------
           05  REIGRPL PIC S9(0004) COMP.
           05  REIGRPF PIC  X(0001).
           05  FILLER REDEFINES REIGRPF.
               10  REIGRPA PIC  X(0001).
           05  REIGRPI PIC  X(0006).
      *    -------------------------------
           05  BCARL PIC S9(0004) COMP.
           05  BCARF PIC  X(0001).
           05  FILLER REDEFINES BCARF.
               10  BCARA PIC  X(0001).
           05  BCARI PIC  X(0001).
      *    -------------------------------
           05  CUSTDSCL PIC S9(0004) COMP.
           05  CUSTDSCF PIC  X(0001).
           05  FILLER REDEFINES CUSTDSCF.
               10  CUSTDSCA PIC  X(0001).
           05  CUSTDSCI PIC  X(0014).
      *    -------------------------------
           05  CUSTAMTL PIC S9(0004) COMP.
           05  CUSTAMTF PIC  X(0001).
           05  FILLER REDEFINES CUSTAMTF.
               10  CUSTAMTA PIC  X(0001).
           05  CUSTAMTI PIC  9(11).
      *    -------------------------------
           05  CONAMEL PIC S9(0004) COMP.
           05  CONAMEF PIC  X(0001).
           05  FILLER REDEFINES CONAMEF.
               10  CONAMEA PIC  X(0001).
           05  CONAMEI PIC  X(0030).
      *    -------------------------------
           05  CMLFHDL PIC S9(0004) COMP.
           05  CMLFHDF PIC  X(0001).
           05  FILLER REDEFINES CMLFHDF.
               10  CMLFHDA PIC  X(0001).
           05  CMLFHDI PIC  X(0006).
      *    -------------------------------
           05  CMLIFEL PIC S9(0004) COMP.
           05  CMLIFEF PIC  X(0001).
           05  FILLER REDEFINES CMLIFEF.
               10  CMLIFEA PIC  X(0001).
           05  CMLIFEI PIC  X(0001).
      *    -------------------------------
           05  LIFEFEEL PIC S9(0004) COMP.
           05  LIFEFEEF PIC  X(0001).
           05  FILLER REDEFINES LIFEFEEF.
               10  LIFEFEEA PIC  X(0001).
           05  LIFEFEEI PIC  9V9999.
      *    -------------------------------
           05  LFPRL PIC S9(0004) COMP.
           05  LFPRF PIC  X(0001).
           05  FILLER REDEFINES LFPRF.
               10  LFPRA PIC  X(0001).
           05  LFPRI PIC  9V9999.
      *    -------------------------------
           05  LFR78L PIC S9(0004) COMP.
           05  LFR78F PIC  X(0001).
           05  FILLER REDEFINES LFR78F.
               10  LFR78A PIC  X(0001).
           05  LFR78I PIC  9V9999.
      *    -------------------------------
           05  RPTAL PIC S9(0004) COMP.
           05  RPTAF PIC  X(0001).
           05  FILLER REDEFINES RPTAF.
               10  RPTAA PIC  X(0001).
           05  RPTAI PIC  X(0001).
      *    -------------------------------
           05  RPTBL PIC S9(0004) COMP.
           05  RPTBF PIC  X(0001).
           05  FILLER REDEFINES RPTBF.
               10  RPTBA PIC  X(0001).
           05  RPTBI PIC  X(0001).
      *    -------------------------------
           05  RPTCL PIC S9(0004) COMP.
           05  RPTCF PIC  X(0001).
           05  FILLER REDEFINES RPTCF.
               10  RPTCA PIC  X(0001).
           05  RPTCI PIC  X(0001).
      *    -------------------------------
           05  RPTDL PIC S9(0004) COMP.
           05  RPTDF PIC  X(0001).
           05  FILLER REDEFINES RPTDF.
               10  RPTDA PIC  X(0001).
           05  RPTDI PIC  X(0001).
      *    -------------------------------
           05  RPTEL PIC S9(0004) COMP.
           05  RPTEF PIC  X(0001).
           05  FILLER REDEFINES RPTEF.
               10  RPTEA PIC  X(0001).
           05  RPTEI PIC  X(0001).
      *    -------------------------------
           05  RPTFL PIC S9(0004) COMP.
           05  RPTFF PIC  X(0001).
           05  FILLER REDEFINES RPTFF.
               10  RPTFA PIC  X(0001).
           05  RPTFI PIC  X(0001).
      *    -------------------------------
           05  CMAHHDL PIC S9(0004) COMP.
           05  CMAHHDF PIC  X(0001).
           05  FILLER REDEFINES CMAHHDF.
               10  CMAHHDA PIC  X(0001).
           05  CMAHHDI PIC  X(0006).
      *    -------------------------------
           05  CMAHL PIC S9(0004) COMP.
           05  CMAHF PIC  X(0001).
           05  FILLER REDEFINES CMAHF.
               10  CMAHA PIC  X(0001).
           05  CMAHI PIC  X(0001).
      *    -------------------------------
           05  AHFEEL PIC S9(0004) COMP.
           05  AHFEEF PIC  X(0001).
           05  FILLER REDEFINES AHFEEF.
               10  AHFEEA PIC  X(0001).
           05  AHFEEI PIC  9V9999.
      *    -------------------------------
           05  AHPRL PIC S9(0004) COMP.
           05  AHPRF PIC  X(0001).
           05  FILLER REDEFINES AHPRF.
               10  AHPRA PIC  X(0001).
           05  AHPRI PIC  9V9999.
      *    -------------------------------
           05  AHR78L PIC S9(0004) COMP.
           05  AHR78F PIC  X(0001).
           05  FILLER REDEFINES AHR78F.
               10  AHR78A PIC  X(0001).
           05  AHR78I PIC  9V9999.
      *    -------------------------------
           05  CESTYPEL PIC S9(0004) COMP.
           05  CESTYPEF PIC  X(0001).
           05  FILLER REDEFINES CESTYPEF.
               10  CESTYPEA PIC  X(0001).
           05  CESTYPEI PIC  X(0001).
      *    -------------------------------
           05  GLHDRL PIC S9(0004) COMP.
           05  GLHDRF PIC  X(0001).
           05  FILLER REDEFINES GLHDRF.
               10  GLHDRA PIC  X(0001).
           05  GLHDRI PIC  X(0012).
      *    -------------------------------
           05  GLCNTRL PIC S9(0004) COMP.
           05  GLCNTRF PIC  X(0001).
           05  FILLER REDEFINES GLCNTRF.
               10  GLCNTRA PIC  X(0001).
           05  GLCNTRI PIC  X(0004).
      *    -------------------------------
           05  LIBNRHDL PIC S9(0004) COMP.
           05  LIBNRHDF PIC  X(0001).
           05  FILLER REDEFINES LIBNRHDF.
               10  LIBNRHDA PIC  X(0001).
           05  LIBNRHDI PIC  X(0002).
      *    -------------------------------
           05  AIBNRHDL PIC S9(0004) COMP.
           05  AIBNRHDF PIC  X(0001).
           05  FILLER REDEFINES AIBNRHDF.
               10  AIBNRHDA PIC  X(0001).
           05  AIBNRHDI PIC  X(0002).
      *    -------------------------------
           05  FEELFHDL PIC S9(0004) COMP.
           05  FEELFHDF PIC  X(0001).
           05  FILLER REDEFINES FEELFHDF.
               10  FEELFHDA PIC  X(0001).
           05  FEELFHDI PIC  X(0001).
      *    -------------------------------
           05  FEEAHHDL PIC S9(0004) COMP.
           05  FEEAHHDF PIC  X(0001).
           05  FILLER REDEFINES FEEAHHDF.
               10  FEEAHHDA PIC  X(0001).
           05  FEEAHHDI PIC  X(0001).
      *    -------------------------------
           05  COMLFHDL PIC S9(0004) COMP.
           05  COMLFHDF PIC  X(0001).
           05  FILLER REDEFINES COMLFHDF.
               10  COMLFHDA PIC  X(0001).
           05  COMLFHDI PIC  X(0001).
      *    -------------------------------
           05  COMAHHDL PIC S9(0004) COMP.
           05  COMAHHDF PIC  X(0001).
           05  FILLER REDEFINES COMAHHDF.
               10  COMAHHDA PIC  X(0001).
           05  COMAHHDI PIC  X(0001).
      *    -------------------------------
           05  TAXLFHDL PIC S9(0004) COMP.
           05  TAXLFHDF PIC  X(0001).
           05  FILLER REDEFINES TAXLFHDF.
               10  TAXLFHDA PIC  X(0001).
           05  TAXLFHDI PIC  X(0001).
      *    -------------------------------
           05  TAXAHHDL PIC S9(0004) COMP.
           05  TAXAHHDF PIC  X(0001).
           05  FILLER REDEFINES TAXAHHDF.
               10  TAXAHHDA PIC  X(0001).
           05  TAXAHHDI PIC  X(0001).
      *    -------------------------------
           05  CEDNAMEL PIC S9(0004) COMP.
           05  CEDNAMEF PIC  X(0001).
           05  FILLER REDEFINES CEDNAMEF.
               10  CEDNAMEA PIC  X(0001).
           05  CEDNAMEI PIC  X(0030).
      *    -------------------------------
           05  PRTAXL PIC S9(0004) COMP.
           05  PRTAXF PIC  X(0001).
           05  FILLER REDEFINES PRTAXF.
               10  PRTAXA PIC  X(0001).
           05  PRTAXI PIC  X(0001).
      *    -------------------------------
           05  PRTAXOWL PIC S9(0004) COMP.
           05  PRTAXOWF PIC  X(0001).
           05  FILLER REDEFINES PRTAXOWF.
               10  PRTAXOWA PIC  X(0001).
           05  PRTAXOWI PIC  X(0001).
      *    -------------------------------
           05  PRTCRSVL PIC S9(0004) COMP.
           05  PRTCRSVF PIC  X(0001).
           05  FILLER REDEFINES PRTCRSVF.
               10  PRTCRSVA PIC  X(0001).
           05  PRTCRSVI PIC  X(0001).
      *    -------------------------------
           05  MORTCDL PIC S9(0004) COMP.
           05  MORTCDF PIC  X(0001).
           05  FILLER REDEFINES MORTCDF.
               10  MORTCDA PIC  X(0001).
           05  MORTCDI PIC  X(0004).
      *    -------------------------------
           05  MORTSWL PIC S9(0004) COMP.
           05  MORTSWF PIC  X(0001).
           05  FILLER REDEFINES MORTSWF.
               10  MORTSWA PIC  X(0001).
           05  MORTSWI PIC  X(0001).
      *    -------------------------------
           05  LFIBNRL PIC S9(0004) COMP.
           05  LFIBNRF PIC  X(0001).
           05  FILLER REDEFINES LFIBNRF.
               10  LFIBNRA PIC  X(0001).
           05  LFIBNRI PIC  V999.
      *    -------------------------------
           05  AHIBNRL PIC S9(0004) COMP.
           05  AHIBNRF PIC  X(0001).
           05  FILLER REDEFINES AHIBNRF.
               10  AHIBNRA PIC  X(0001).
           05  AHIBNRI PIC  V999.
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  X(0001).
      *    -------------------------------
           05  FEELIFEL PIC S9(0004) COMP.
           05  FEELIFEF PIC  X(0001).
           05  FILLER REDEFINES FEELIFEF.
               10  FEELIFEA PIC  X(0001).
           05  FEELIFEI PIC  X(0001).
      *    -------------------------------
           05  FEEAHL PIC S9(0004) COMP.
           05  FEEAHF PIC  X(0001).
           05  FILLER REDEFINES FEEAHF.
               10  FEEAHA PIC  X(0001).
           05  FEEAHI PIC  X(0001).
      *    -------------------------------
           05  COMLIFEL PIC S9(0004) COMP.
           05  COMLIFEF PIC  X(0001).
           05  FILLER REDEFINES COMLIFEF.
               10  COMLIFEA PIC  X(0001).
           05  COMLIFEI PIC  X(0001).
      *    -------------------------------
           05  COMAHL PIC S9(0004) COMP.
           05  COMAHF PIC  X(0001).
           05  FILLER REDEFINES COMAHF.
               10  COMAHA PIC  X(0001).
           05  COMAHI PIC  X(0001).
      *    -------------------------------
           05  TAXLIFEL PIC S9(0004) COMP.
           05  TAXLIFEF PIC  X(0001).
           05  FILLER REDEFINES TAXLIFEF.
               10  TAXLIFEA PIC  X(0001).
           05  TAXLIFEI PIC  X(0001).
      *    -------------------------------
           05  TAXAHL PIC S9(0004) COMP.
           05  TAXAHF PIC  X(0001).
           05  FILLER REDEFINES TAXAHF.
               10  TAXAHA PIC  X(0001).
           05  TAXAHI PIC  X(0001).
      *    -------------------------------
           05  EXTAXL PIC S9(0004) COMP.
           05  EXTAXF PIC  X(0001).
           05  FILLER REDEFINES EXTAXF.
               10  EXTAXA PIC  X(0001).
           05  EXTAXI PIC  9V9999.
      *    -------------------------------
           05  LFCLMHDL PIC S9(0004) COMP.
           05  LFCLMHDF PIC  X(0001).
           05  FILLER REDEFINES LFCLMHDF.
               10  LFCLMHDA PIC  X(0001).
           05  LFCLMHDI PIC  X(0006).
      *    -------------------------------
           05  AHCLMHDL PIC S9(0004) COMP.
           05  AHCLMHDF PIC  X(0001).
           05  FILLER REDEFINES AHCLMHDF.
               10  AHCLMHDA PIC  X(0001).
           05  AHCLMHDI PIC  X(0006).
      *    -------------------------------
           05  LFCVTYPL PIC S9(0004) COMP.
           05  LFCVTYPF PIC  X(0001).
           05  FILLER REDEFINES LFCVTYPF.
               10  LFCVTYPA PIC  X(0001).
           05  LFCVTYPI PIC  X(0002).
      *    -------------------------------
           05  AHCVTYPL PIC S9(0004) COMP.
           05  AHCVTYPF PIC  X(0001).
           05  FILLER REDEFINES AHCVTYPF.
               10  AHCVTYPA PIC  X(0001).
           05  AHCVTYPI PIC  X(0002).
      *    -------------------------------
           05  LFMETHL PIC S9(0004) COMP.
           05  LFMETHF PIC  X(0001).
           05  FILLER REDEFINES LFMETHF.
               10  LFMETHA PIC  X(0001).
           05  LFMETHI PIC  X(0001).
      *    -------------------------------
           05  AHMETHL PIC S9(0004) COMP.
           05  AHMETHF PIC  X(0001).
           05  FILLER REDEFINES AHMETHF.
               10  AHMETHA PIC  X(0001).
           05  AHMETHI PIC  X(0001).
      *    -------------------------------
           05  LCLMPCTL PIC S9(0004) COMP.
           05  LCLMPCTF PIC  X(0001).
           05  FILLER REDEFINES LCLMPCTF.
               10  LCLMPCTA PIC  X(0001).
           05  LCLMPCTI PIC  9V9999.
      *    -------------------------------
           05  LCLMMAXL PIC S9(0004) COMP.
           05  LCLMMAXF PIC  X(0001).
           05  FILLER REDEFINES LCLMMAXF.
               10  LCLMMAXA PIC  X(0001).
           05  LCLMMAXI PIC  9(11).
      *    -------------------------------
           05  ACLMPCTL PIC S9(0004) COMP.
           05  ACLMPCTF PIC  X(0001).
           05  FILLER REDEFINES ACLMPCTF.
               10  ACLMPCTA PIC  X(0001).
           05  ACLMPCTI PIC  9V9999.
      *    -------------------------------
           05  ACLMMAXL PIC S9(0004) COMP.
           05  ACLMMAXF PIC  X(0001).
           05  FILLER REDEFINES ACLMMAXF.
               10  ACLMMAXA PIC  X(0001).
           05  ACLMMAXI PIC  9(11).
      *    -------------------------------
           05  CLINCDTL PIC S9(0004) COMP.
           05  CLINCDTF PIC  X(0001).
           05  FILLER REDEFINES CLINCDTF.
               10  CLINCDTA PIC  X(0001).
           05  CLINCDTI PIC  X(0006).
      *    -------------------------------
           05  ERBEGDTL PIC S9(0004) COMP.
           05  ERBEGDTF PIC  X(0001).
           05  FILLER REDEFINES ERBEGDTF.
               10  ERBEGDTA PIC  X(0001).
           05  ERBEGDTI PIC  X(0006).
      *    -------------------------------
           05  ERENDDTL PIC S9(0004) COMP.
           05  ERENDDTF PIC  X(0001).
           05  FILLER REDEFINES ERENDDTF.
               10  ERENDDTA PIC  X(0001).
           05  ERENDDTI PIC  X(0006).
      *    -------------------------------
           05  ERENDCDL PIC S9(0004) COMP.
           05  ERENDCDF PIC  X(0001).
           05  FILLER REDEFINES ERENDCDF.
               10  ERENDCDA PIC  X(0001).
           05  ERENDCDI PIC  X(0001).
      *    -------------------------------
           05  LFBASISL PIC S9(0004) COMP.
           05  LFBASISF PIC  X(0001).
           05  FILLER REDEFINES LFBASISF.
               10  LFBASISA PIC  X(0001).
           05  LFBASISI PIC  X(0001).
      *    -------------------------------
           05  AHBASISL PIC S9(0004) COMP.
           05  AHBASISF PIC  X(0001).
           05  FILLER REDEFINES AHBASISF.
               10  AHBASISA PIC  X(0001).
           05  AHBASISI PIC  X(0001).
      *    -------------------------------
           05  LFFEEHGL PIC S9(0004) COMP.
           05  LFFEEHGF PIC  X(0001).
           05  FILLER REDEFINES LFFEEHGF.
               10  LFFEEHGA PIC  X(0001).
           05  LFFEEHGI PIC  X(0006).
      *    -------------------------------
           05  LFPCT1L PIC S9(0004) COMP.
           05  LFPCT1F PIC  X(0001).
           05  FILLER REDEFINES LFPCT1F.
               10  LFPCT1A PIC  X(0001).
           05  LFPCT1I PIC  9V9999.
      *    -------------------------------
           05  LFTHRU1L PIC S9(0004) COMP.
           05  LFTHRU1F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU1F.
               10  LFTHRU1A PIC  X(0001).
           05  LFTHRU1I PIC  9(11).
      *    -------------------------------
           05  LFPCT2L PIC S9(0004) COMP.
           05  LFPCT2F PIC  X(0001).
           05  FILLER REDEFINES LFPCT2F.
               10  LFPCT2A PIC  X(0001).
           05  LFPCT2I PIC  9V9999.
      *    -------------------------------
           05  LFTHRU2L PIC S9(0004) COMP.
           05  LFTHRU2F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU2F.
               10  LFTHRU2A PIC  X(0001).
           05  LFTHRU2I PIC  9(11).
      *    -------------------------------
           05  LFPCT3L PIC S9(0004) COMP.
           05  LFPCT3F PIC  X(0001).
           05  FILLER REDEFINES LFPCT3F.
               10  LFPCT3A PIC  X(0001).
           05  LFPCT3I PIC  9V9999.
      *    -------------------------------
           05  LFTHRU3L PIC S9(0004) COMP.
           05  LFTHRU3F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU3F.
               10  LFTHRU3A PIC  X(0001).
           05  LFTHRU3I PIC  9(11).
      *    -------------------------------
           05  LFPCT4L PIC S9(0004) COMP.
           05  LFPCT4F PIC  X(0001).
           05  FILLER REDEFINES LFPCT4F.
               10  LFPCT4A PIC  X(0001).
           05  LFPCT4I PIC  9V9999.
      *    -------------------------------
           05  LFTHRU4L PIC S9(0004) COMP.
           05  LFTHRU4F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU4F.
               10  LFTHRU4A PIC  X(0001).
           05  LFTHRU4I PIC  9(11).
      *    -------------------------------
           05  LFPCT5L PIC S9(0004) COMP.
           05  LFPCT5F PIC  X(0001).
           05  FILLER REDEFINES LFPCT5F.
               10  LFPCT5A PIC  X(0001).
           05  LFPCT5I PIC  9V9999.
      *    -------------------------------
           05  LFTHRU5L PIC S9(0004) COMP.
           05  LFTHRU5F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU5F.
               10  LFTHRU5A PIC  X(0001).
           05  LFTHRU5I PIC  9(11).
      *    -------------------------------
           05  LFPCT6L PIC S9(0004) COMP.
           05  LFPCT6F PIC  X(0001).
           05  FILLER REDEFINES LFPCT6F.
               10  LFPCT6A PIC  X(0001).
           05  LFPCT6I PIC  9V9999.
      *    -------------------------------
           05  LFTHRU6L PIC S9(0004) COMP.
           05  LFTHRU6F PIC  X(0001).
           05  FILLER REDEFINES LFTHRU6F.
               10  LFTHRU6A PIC  X(0001).
           05  LFTHRU6I PIC  9(11).
      *    -------------------------------
           05  AHFEEHGL PIC S9(0004) COMP.
           05  AHFEEHGF PIC  X(0001).
           05  FILLER REDEFINES AHFEEHGF.
               10  AHFEEHGA PIC  X(0001).
           05  AHFEEHGI PIC  X(0006).
      *    -------------------------------
           05  AHPCT1L PIC S9(0004) COMP.
           05  AHPCT1F PIC  X(0001).
           05  FILLER REDEFINES AHPCT1F.
               10  AHPCT1A PIC  X(0001).
           05  AHPCT1I PIC  9V9999.
      *    -------------------------------
           05  AHTHRU1L PIC S9(0004) COMP.
           05  AHTHRU1F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU1F.
               10  AHTHRU1A PIC  X(0001).
           05  AHTHRU1I PIC  9(11).
      *    -------------------------------
           05  AHPCT2L PIC S9(0004) COMP.
           05  AHPCT2F PIC  X(0001).
           05  FILLER REDEFINES AHPCT2F.
               10  AHPCT2A PIC  X(0001).
           05  AHPCT2I PIC  9V9999.
      *    -------------------------------
           05  AHTHRU2L PIC S9(0004) COMP.
           05  AHTHRU2F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU2F.
               10  AHTHRU2A PIC  X(0001).
           05  AHTHRU2I PIC  9(11).
      *    -------------------------------
           05  AHPCT3L PIC S9(0004) COMP.
           05  AHPCT3F PIC  X(0001).
           05  FILLER REDEFINES AHPCT3F.
               10  AHPCT3A PIC  X(0001).
           05  AHPCT3I PIC  9V9999.
      *    -------------------------------
           05  AHTHRU3L PIC S9(0004) COMP.
           05  AHTHRU3F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU3F.
               10  AHTHRU3A PIC  X(0001).
           05  AHTHRU3I PIC  9(11).
      *    -------------------------------
           05  AHPCT4L PIC S9(0004) COMP.
           05  AHPCT4F PIC  X(0001).
           05  FILLER REDEFINES AHPCT4F.
               10  AHPCT4A PIC  X(0001).
           05  AHPCT4I PIC  9V9999.
      *    -------------------------------
           05  AHTHRU4L PIC S9(0004) COMP.
           05  AHTHRU4F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU4F.
               10  AHTHRU4A PIC  X(0001).
           05  AHTHRU4I PIC  9(11).
      *    -------------------------------
           05  AHPCT5L PIC S9(0004) COMP.
           05  AHPCT5F PIC  X(0001).
           05  FILLER REDEFINES AHPCT5F.
               10  AHPCT5A PIC  X(0001).
           05  AHPCT5I PIC  9V9999.
      *    -------------------------------
           05  AHTHRU5L PIC S9(0004) COMP.
           05  AHTHRU5F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU5F.
               10  AHTHRU5A PIC  X(0001).
           05  AHTHRU5I PIC  9(11).
      *    -------------------------------
           05  AHPCT6L PIC S9(0004) COMP.
           05  AHPCT6F PIC  X(0001).
           05  FILLER REDEFINES AHPCT6F.
               10  AHPCT6A PIC  X(0001).
           05  AHPCT6I PIC  9V9999.
      *    -------------------------------
           05  AHTHRU6L PIC S9(0004) COMP.
           05  AHTHRU6F PIC  X(0001).
           05  FILLER REDEFINES AHTHRU6F.
               10  AHTHRU6A PIC  X(0001).
           05  AHTHRU6I PIC  9(11).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  BENTERL PIC S9(0004) COMP.
           05  BENTERF PIC  X(0001).
           05  FILLER REDEFINES BENTERF.
               10  BENTERA PIC  X(0001).
           05  BENTERI PIC  99.
      *    -------------------------------
           05  ALUDATEL PIC S9(0004) COMP.
           05  ALUDATEF PIC  X(0001).
           05  FILLER REDEFINES ALUDATEF.
               10  ALUDATEA PIC  X(0001).
           05  ALUDATEI PIC  X(0008).
      *    -------------------------------
           05  ALUTIMEL PIC S9(0004) COMP.
           05  ALUTIMEF PIC  X(0001).
           05  FILLER REDEFINES ALUTIMEF.
               10  ALUTIMEA PIC  X(0001).
           05  ALUTIMEI PIC  X(0005).
      *    -------------------------------
           05  ALUBYL PIC S9(0004) COMP.
           05  ALUBYF PIC  X(0001).
           05  FILLER REDEFINES ALUBYF.
               10  ALUBYA PIC  X(0001).
           05  ALUBYI PIC  X(0004).
       01  EL6511BO REDEFINES EL6511CI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPSUBO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REIGRPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CUSTDSCO PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CUSTAMTO PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CONAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMLFHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMLIFEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIFEFEEO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPRO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFR78O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTAO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTBO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPTFO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMAHHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHFEEO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPRO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHR78O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CESTYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLHDRO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GLCNTRO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LIBNRHDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AIBNRHDO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEELFHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEEAHHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMLFHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMAHHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXLFHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXAHHDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CEDNAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTAXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTAXOWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTCRSVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTCDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORTSWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFIBNRO PIC  V999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHIBNRO PIC  V999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEELIFEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FEEAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMLIFEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXLIFEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TAXAHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXTAXO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFCLMHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHCLMHDO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFCVTYPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHCVTYPO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFMETHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHMETHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCLMPCTO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LCLMMAXO PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLMPCTO PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLMMAXO PIC  ZZZZ,ZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLINCDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERBEGDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERENDDTO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERENDCDO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHBASISO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFFEEHGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT1O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU1O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT2O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU2O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT3O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU3O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT4O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU4O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT5O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU5O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFPCT6O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LFTHRU6O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHFEEHGO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT1O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU1O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT2O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU2O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT3O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU3O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT4O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU4O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT5O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU5O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHPCT6O PIC  9V9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AHTHRU6O PIC  ZZZZ,ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTERO PIC  99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUBYO PIC  X(0004).
      *    -------------------------------
00307
00308  01  FILLER                  REDEFINES
00309      EL6511CI.
00310      12  FILLER                      PIC X(47).
00311      12  DESC-OCCURS OCCURS 18 TIMES
00312                      INDEXED BY DO-INDX.
00313          16  DESCL                   PIC S9(4)       COMP.
00314          16  DESCA                   PIC X.
00315          16  DESCO                   PIC X(79).
00316      12  FILLER                      PIC X(85).
00317
00318      EJECT
00319
      ****************************************************************
      *                                                               
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 
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
         02  DFHEIV97              PIC S9(7) COMP-3 VALUE ZERO.       
         02  DFHEIV98              PIC S9(4) COMP SYNC VALUE ZERO.    
         02  FILLER                PIC X(02).                         
         02  DFHEIV99              PIC X(08) VALUE SPACE.             
         02  DFHEIVL0              PIC X(48) VALUE SPACE.             
         02  DFHEIVL1              PIC X(48) VALUE SPACE.             
         02  DFHEIVL2              PIC X(48) VALUE SPACE.             
         02  DFHEIVL3              PIC X(48) VALUE SPACE.             
       LINKAGE  SECTION.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007 by Clerity Solutions, Inc.                 *
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
           02  eibresp          pic 9(09) comp.
           02  eibresp2         pic 9(09) comp.
           02  dfheigdj         pic s9(4) comp.
           02  dfheigdk         pic s9(4) comp.
00321
00322  01  DFHCOMMAREA                     PIC X(1024).
00323
00324 *01 PARMLIST .
00325 *    02  FILLER                      PIC S9(8)   COMP.
00326 *    02  ERREIN-POINTER              PIC S9(8)   COMP.
00327 *    02  ELCNTL-POINTER              PIC S9(8)   COMP.
00328      EJECT
00329 *                                    COPY ERCREIN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCREIN                             *
00003 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00004 *                            VMOD=2.010                          *
00005 *                                                                *
00006 *   ONLINE CREDIT SYSTEM                                         *
00007 *                                                                *
00008 *   FILE DESCRIPTION = REINSURANCE MASTER FILE                   *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 4000  RECFORM = FIXED                          *
00012 *                                                                *
00013 *   BASE CLUSTER NAME = ERREIN                   RKP=2,LEN=8     *
00014 *       ALTERNATE PATH = NONE                                    *
00015 *                                                                *
00016 *   LOG = NO                                                     *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 *                                                                *
00019 ******************************************************************
103101*                   C H A N G E   L O G
103101*
103101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103101*-----------------------------------------------------------------
103101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103101* EFFECTIVE    NUMBER
103101*-----------------------------------------------------------------
103101* 103101    2001100100006  SMVA  ADD STATE EXHIBIT REPORT OPTION F
032707* 032707    2007032100006  PEMA  ADD EXCISE TAX CAPABILITY
103101******************************************************************
00021  01  REINSURANCE-RECORD.
00022      12  RE-RECORD-ID                      PIC XX.
00023          88  VALID-RE-ID                      VALUE 'RE'.
00024
00025      12  RE-CONTROL-PRIMARY.
00026          16  RE-COMPANY-CD                 PIC X.
00027          16  RE-KEY.
00028              20  RE-CODE                   PIC X.
00029                  88  RE-TABLE-RECORD          VALUE 'A'.
00030                  88  RE-COMPANY-RECORD        VALUE 'B'.
00031              20  RE-TABLE                  PIC XXX.
00032              20  FILLER                    PIC XXX.
00033          16  RE-COMPANY-KEY REDEFINES RE-KEY.
00034              20  FILLER                    PIC X.
00035              20  RE-COMPANY.
00036                  24  RE-COMP-PRIME         PIC XXX.
00037                  24  RE-COMP-SUB           PIC XXX.
00038
00039      12  RE-MAINT-INFORMATION.
00040          16  RE-LAST-MAINT-DT              PIC XX.
00041          16  RE-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00042          16  RE-LAST-MAINT-USER            PIC X(4).
00043          16  FILLER                        PIC X(10).
00044
00045      12  RE-TABLE-DATA.
00046          16  RE-100-COMP                   PIC 99.
00047
00048          16  RE-COMP-INFO    OCCURS 30 TIMES.
00049              20  RE-REI-COMP-NO.
00050                  24  RE-REI-COMP           PIC XXX.
00051                  24  RE-REI-COMP-SUB       PIC XXX.
00052              20  RE-LF-QC                  PIC X.
00053              20  RE-AH-QC                  PIC X.
00054              20  RE-LO-DATE                PIC 9(11)     COMP-3.
00055              20  RE-HI-DATE                PIC 9(11)     COMP-3.
00056              20  RE-LFAGE-LO               PIC 99.
00057              20  RE-LFAGE-HI               PIC 99.
00058              20  RE-AHAGE-LO               PIC 99.
00059              20  RE-AHAGE-HI               PIC 99.
00060              20  RE-LFTRM-LO               PIC S999       COMP-3.
00061              20  RE-LFTRM-HI               PIC S999       COMP-3.
00062              20  RE-AHTRM-LO               PIC S999       COMP-3.
00063              20  RE-AHTRM-HI               PIC S999       COMP-3.
00064              20  RE-LF-PCT                 PIC S9V9999    COMP-3.
00065              20  RE-AH-PCT                 PIC S9V9999    COMP-3.
00066              20  RE-LF-LIM-LO              PIC S9(9)V99   COMP-3.
00067              20  RE-LF-LIM-HI              PIC S9(9)V99   COMP-3.
00068              20  RE-LF-LO                  PIC S9(9)V99   COMP-3.
00069              20  RE-LF-HI                  PIC S9(9)V99   COMP-3.
00070              20  RE-AHBEN-LIM-LO           PIC S9(7)V99   COMP-3.
00071              20  RE-AHBEN-LIM-HI           PIC S9(7)V99   COMP-3.
00072              20  RE-AHBEN-LO               PIC S9(7)V99   COMP-3.
00073              20  RE-AHBEN-HI               PIC S9(7)V99   COMP-3.
00074              20  RE-AHMOA-LIM-LO           PIC S9(7)V99   COMP-3.
00075              20  RE-AHMOA-LIM-HI           PIC S9(7)V99   COMP-3.
00076              20  RE-AHMOA-LO               PIC S9(7)V99   COMP-3.
00077              20  RE-AHMOA-HI               PIC S9(7)V99   COMP-3.
00078              20  RE-LF-BEN-CODE            PIC X.
00079              20  RE-AH-BEN-CODE            PIC X.
00080              20  RE-INTERACTIVE            PIC X.
00081              20  RE-REMAINING              PIC X.
CIDMOD             20  RE-LF-RUNOFF-SW           PIC X.
CIDMOD             20  RE-AH-RUNOFF-SW           PIC X.
CIDMOD             20  FILLER                    PIC X(19).
00083
00084          16  RE-COMP-INFO-END              PIC X(6).
00085          16  RE-NSP-ST-CD-LF               PIC XX.
00086          16  RE-NSP-ST-CD-AH               PIC XX.
00087          16  RE-TABLE-CARRIER-SECURITY     PIC X.
00088              88  NO-TABLE-CARRIER-SECURITY    VALUE SPACE.
00089
00090          16  FILLER                        PIC X(27).
00091
00092      12  RE-COMPANY-DATA   REDEFINES   RE-TABLE-DATA.
00093          16  RE-NAME                       PIC X(30).
00094          16  RE-LF-PE                      PIC X.
00095          16  RE-AH-PE                      PIC X.
00096          16  RE-LF-FEE                     PIC S9V9999    COMP-3.
00097          16  RE-AH-FEE                     PIC S9V9999    COMP-3.
00098          16  RE-AH-PR-PCT                  PIC S9V9999    COMP-3.
00099          16  RE-AH-78-PCT                  PIC S9V9999    COMP-3.
00100          16  RE-PRT-ST                     PIC X.
00101          16  RE-PRT-OW                     PIC X.
00102          16  RE-MORT-CODE                  PIC X(4).
00103          16  RE-CLAIM-CODE                 PIC X.
00104          16  RE-ZERO-LF-FEE                PIC X.
00105          16  RE-ZERO-AH-FEE                PIC X.
00106          16  RE-CEDE-NAME                  PIC X(30).
00107          16  RE-LF-COMM                    PIC X.
00108          16  RE-AH-COMM                    PIC X.
00109          16  RE-LF-TAX                     PIC X.
00110          16  RE-AH-TAX                     PIC X.
00111          16  RE-CLM-INCURRED-LIM           PIC 9(11)  COMP-3.
00116          16  RE-LF-IBNR-PCT                PIC SV999      COMP-3.
00117          16  RE-AH-IBNR-PCT                PIC SV999      COMP-3.
00118
00119          16  RE-COMP-CARRIER-SECURITY      PIC X.
00120              88  NO-COMP-CARRIER-SECURITY     VALUE SPACE.
00121
00122          16  RE-LF-CEDING-FEE-BRACKETS.
00123              20  RE-LF-FEE-METHOD          PIC X.
00124                  88  RE-LF-FEE-BRACKETED         VALUE '1' '2'.
00125                  88  RE-LF-FEE-METHOD-1          VALUE '1'.
00126                  88  RE-LF-FEE-METHOD-2          VALUE '2'.
00127                  88  RE-LF-FEE-PERCENT           VALUE ' ' 'P'.
00128              20  RE-LF-FEE-BASIS           PIC X.
00129                  88  RE-LF-GROSS-CEDED             VALUE '1'.
00130                  88  RE-LF-NET-CEDED               VALUE '2'.
00131                  88  RE-LF-GROSS-WRITTEN           VALUE '3'.
00132                  88  RE-LF-NET-WRITTEN             VALUE '4'.
00133                  88  RE-LF-COMBINE-GROSS-CEDED     VALUE '5'.
00134                  88  RE-LF-COMBINE-NET-CEDED       VALUE '6'.
00135                  88  RE-LF-COMBINE-GROSS-WRITTEN   VALUE '7'.
00136                  88  RE-LF-COMBINE-NET-WRITTEN     VALUE '8'.
00137              20  FILLER                    PIC XXX.
00138              20  RE-LF-FEE-RANGES  OCCURS 6 TIMES.
00139                  24  RE-LF-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00140                  24  RE-LF-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00141
00142          16  RE-AH-CEDING-FEE-BRACKETS.
00143              20  RE-AH-FEE-METHOD          PIC X.
00144                  88  RE-AH-FEE-BRACKETED         VALUE '1' '2'.
00145                  88  RE-AH-FEE-METHOD-1          VALUE '1'.
00146                  88  RE-AH-FEE-METHOD-2          VALUE '2'.
00147                  88  RE-AH-FEE-PERCENT           VALUE ' ' 'P'.
00148              20  RE-AH-FEE-BASIS           PIC X.
00149                  88  RE-AH-GROSS-CEDED             VALUE '1'.
00150                  88  RE-AH-NET-CEDED               VALUE '2'.
00151                  88  RE-AH-GROSS-WRITTEN           VALUE '3'.
00152                  88  RE-AH-NET-WRITTEN             VALUE '4'.
00153                  88  RE-AH-COMBINE-GROSS-CEDED     VALUE '5'.
00154                  88  RE-AH-COMBINE-NET-CEDED       VALUE '6'.
00155                  88  RE-AH-COMBINE-GROSS-WRITTEN   VALUE '7'.
00156                  88  RE-AH-COMBINE-NET-WRITTEN     VALUE '8'.
00157              20  FILLER                    PIC XXX.
00158              20  RE-AH-FEE-RANGES  OCCURS 6 TIMES.
00159                  24  RE-AH-FEE-RANGE-PCT   PIC S9V9999    COMP-3.
00160                  24  RE-AH-FEE-THRU-AMT    PIC S9(7)V99   COMP-3.
00161
00162          16  RE-EARNING-START-DT           PIC 9(11)  COMP-3.
00166
00167          16  RE-OLD-CEDING-STMT            PIC X.
00168
00169          16  RE-LF-CLM-PCT                 PIC S9V9999    COMP-3.
00170          16  RE-AH-CLM-PCT                 PIC S9V9999    COMP-3.
00171          16  RE-LF-CLM-MAX                 PIC S9(7)V99   COMP-3.
00172          16  RE-AH-CLM-MAX                 PIC S9(7)V99   COMP-3.
00173          16  RE-LF-PR-PCT                  PIC S9V9999    COMP-3.
00174          16  RE-LF-78-PCT                  PIC S9V9999    COMP-3.
00175          16  RE-REINS-GROUPING-CODE        PIC X(6).
00176          16  RE-MORT-SW                    PIC X.
00177          16  RE-CEDING-TYPE-FLAG           PIC X.
00178              88  RE-NO-CESSION-TYPE                VALUE ' '.
00179              88  RE-CEDED                          VALUE 'C'.
00180              88  RE-ASSUMED                        VALUE 'A'.
00181              88  RE-PHANTOM                        VALUE 'P'.
00182
00183          16  RE-CEDING-STMT-OPT-A          PIC X.
00184              88  REPORT-A-WANTED    VALUE ' ' 'Y'.
00185          16  RE-CEDING-STMT-OPT-B          PIC X.
00186              88  REPORT-B-WANTED    VALUE ' ' 'Y'.
00187          16  RE-CEDING-STMT-OPT-C          PIC X.
00188              88  REPORT-C-WANTED    VALUE ' ' 'Y'.
00189          16  RE-CEDING-STMT-OPT-D          PIC X.
00190              88  REPORT-D-WANTED    VALUE ' ' 'Y'.
00191          16  RE-CEDING-STMT-OPT-E          PIC X.
00192              88  REPORT-E-WANTED    VALUE ' ' 'Y'.
00193
00194          16  RE-PRT-CRSV                   PIC X.
00195
00196          16  RE-GL-CENTER                  PIC X(4).
00197
00198          16  RE-CUSTODIAL-BAL              PIC S9(7)V99   COMP-3.
00199
00200          16  RE-EARNING-STOP-DT            PIC 9(11)  COMP-3.
00204
00205          16  RE-EARN-STOP-CODE             PIC X.
00206              88  STOP-LIFE-EARNING  VALUE 'L' 'B'.
00207              88  STOP-AH-EARNING    VALUE 'A' 'B'.
00208
103101         16  RE-STATE-EXHIBIT-OPT-F        PIC X.
103101             88  RPTF-ECS152-WANTED VALUE ' ' 'Y'.
103101
032707         16  RE-EXCISE-TAX                 PIC S9V9999 COMP-3.
032707         16  FILLER                        PIC X(2281).
00210
00211          16  RE-DESC OCCURS 18 TIMES       PIC X(79).
00212
00213 ******************************************************************
00330      EJECT
00331 *                                    COPY ELCCNTL.
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
00607              20  FILLER                     PIC X.
00608
00609          16  CF-ST-COMMISSION-CAPS.
00610              20  CF-ST-COMM-CAP-SL          PIC S9V9(4) COMP-3.
00611              20  CF-ST-COMM-CAP-JL          PIC S9V9(4) COMP-3.
00612              20  CF-ST-COMM-CAP-SA          PIC S9V9(4) COMP-3.
00613              20  CF-ST-COMM-CAP-JA          PIC S9V9(4) COMP-3.
00614          16  CF-COMM-CAP-LIMIT-TO           PIC X.
00615                  88  ST-LIMIT-TO-ACCOUNT        VALUE 'A'.
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
012913         16  FILLER                         PIC X(185).
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
082603             20  FILLER                     PIC X(11).
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
00332      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA
                                REINSURANCE-RECORD CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6511' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00334      CONTINUE.
00335
00336      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00337      MOVE '5'                    TO  DC-OPTION-CODE.
00338      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
00339      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00340      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00341
00342      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00343
00344  1000-START.
00345      IF EIBCALEN = 0
00346          GO TO 8800-UNAUTHORIZED-ACCESS.
00347
00348      MOVE 1                      TO  EMI-NUMBER-OF-LINES.
00349
00350      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00351          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00352              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00353              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00354              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00355              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00356              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00357              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00358              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00359              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00360          ELSE
00361              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00362              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00363              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00364              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00365              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00366              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00367              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00368              MOVE SPACES               TO PI-SAVED-PROGRAM-6.
00369
00370      
      * EXEC CICS HANDLE CONDITION
00371 *        NOTOPEN  (9990-ABEND)
00372 *        NOTFND   (8880-NOT-FOUND)
00373 *        PGMIDERR (9600-PGMID-ERROR)
00374 *        ERROR    (9990-ABEND)
00375 *        END-EXEC.
      *    MOVE '"$JIL.                ! " #00004149' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034313439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00376
00377      IF EIBTRNID NOT = TRANS-ID
00378          IF PI-MAP-C
00379              MOVE LOW-VALUES     TO  EL6511CI
00380              MOVE 'B'            TO  PI-CRR-CODE
00381              GO TO 5500-BUILD-INITIAL-SCREEN
00382          ELSE
00383              MOVE LOW-VALUES     TO  EL6511BI
00384              MOVE 'B'            TO  PI-CRR-CODE
00385              GO TO 5000-BUILD-INITIAL-SCREEN.
00386
00387      IF EIBAID = DFHCLEAR
00388          MOVE LOW-VALUES         TO  PI-ERREIN-KEY
00389          MOVE PI-SAVE-TABLE      TO  PI-CRR-TABLE
00390          MOVE 'A'                TO  PI-CRR-CODE
00391          GO TO 9400-CLEAR.
00392
00393      IF PI-MAP-C
00394          GO TO 3000-RECEIVE.
00395
00396      EJECT
00397  2000-RECEIVE.
00398      MOVE LOW-VALUES             TO  EL6511BI.
00399      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00400          MOVE ER-0008            TO  EMI-ERROR
00401          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00402          MOVE -1                 TO  MAINTYPL
00403          GO TO 8200-SEND-DATAONLY.
00404
00405      
      * EXEC CICS RECEIVE
00406 *        MAP    (MAP-B-NAME)
00407 *        MAPSET (MAPSET-NAME)
00408 *        INTO   (EL6511BI)
00409 *    END-EXEC.
           MOVE LENGTH OF
            EL6511BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004184' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-B-NAME, 
                 EL6511BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00410
00411      IF BENTERL = 0
00412          GO TO 2100-CHECK-BENTERS.
00413
00414      IF EIBAID NOT = DFHENTER
00415          MOVE ER-0004            TO  EMI-ERROR
00416          GO TO 2200-INPUT-ERROR.
00417
00418      IF (BENTERI NUMERIC) AND (BENTERI > 0 AND < 25)
00419          MOVE PF-VALUES (BENTERI)    TO  EIBAID
00420      ELSE
00421          MOVE ER-0029                TO  EMI-ERROR
00422          GO TO 2200-INPUT-ERROR.
00423
00424      EJECT
00425
00426  2100-CHECK-BENTERS.
00427      IF EIBAID = DFHPF23
00428          GO TO 8810-PF23.
00429
00430      IF EIBAID = DFHPF24
00431          GO TO 9200-RETURN-MAIN-MENU.
00432
00433      IF EIBAID = DFHPF12
00434          GO TO 9500-PF12.
00435
00436      IF MAINTYPL GREATER ZERO
00437          IF MAINTYPI NOT = SPACE
00438              IF EIBAID NOT = DFHENTER
00439                  MOVE ER-0050    TO  EMI-ERROR
00440                  GO TO 2200-INPUT-ERROR.
00441
00442      IF EIBAID = DFHPF1
00443          GO TO 7500-PAGE-FORWARD.
00444
00445      IF EIBAID = DFHPF2
00446          GO TO 7600-PAGE-BACKWARD.
00447
00448      IF EIBAID = DFHPF7
00449          MOVE LOW-VALUES         TO  PI-ERREIN-KEY
00450          MOVE PI-SAVE-TABLE      TO  PI-CRR-TABLE
00451          MOVE 'A'                TO  PI-CRR-CODE
00452          MOVE XCTL-651           TO  PGM-NAME
00453          GO TO 9300-XCTL.
00454
00455      IF EIBAID = DFHPF9
00456          MOVE 'EL6511C'          TO  PI-MAPNAME
00457          MOVE 'B'                TO  PI-CRR-CODE
00458          GO TO 5500-BUILD-INITIAL-SCREEN.
00459
00460      IF EIBAID = DFHENTER
00461          GO TO 4000-EDIT-MAINT.
00462
00463      MOVE ER-0029                TO  EMI-ERROR.
00464
00465  2200-INPUT-ERROR.
00466      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00467      MOVE AL-UNBON               TO  BENTERA.
00468      MOVE -1                     TO  BENTERL.
00469      GO TO 8200-SEND-DATAONLY.
00470
00471      EJECT
00472
00473  3000-RECEIVE.
00474      MOVE LOW-VALUES             TO  EL6511CI.
00475      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00476          MOVE ER-0008            TO  EMI-ERROR
00477          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00478          MOVE -1                 TO  DESC1L
00479          GO TO 8210-SEND-DATAONLY.
00480
00481      
      * EXEC CICS RECEIVE
00482 *        MAP    (MAP-C-NAME)
00483 *        MAPSET (MAPSET-NAME)
00484 *        INTO   (EL6511CI)
00485 *    END-EXEC.
           MOVE LENGTH OF
            EL6511CI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004260' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-C-NAME, 
                 EL6511CI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00486
00487      IF CENTERL = 0
00488          GO TO 3100-CHECK-CENTERS.
00489
00490      IF EIBAID NOT = DFHENTER
00491          MOVE ER-0004            TO  EMI-ERROR
00492          GO TO 3200-INPUT-ERROR.
00493
00494      IF (CENTERI NUMERIC) AND (CENTERI > 0 AND < 25)
00495          MOVE PF-VALUES (CENTERI)    TO  EIBAID
00496      ELSE
00497          MOVE ER-0029                TO  EMI-ERROR
00498          GO TO 3200-INPUT-ERROR.
00499
00500      EJECT
00501
00502  3100-CHECK-CENTERS.
00503      IF EIBAID = DFHPF23
00504          GO TO 8810-PF23.
00505
00506      IF EIBAID = DFHPF24
00507          GO TO 9200-RETURN-MAIN-MENU.
00508
00509      IF EIBAID = DFHPF12
00510          GO TO 9500-PF12.
00511
00512      IF EIBAID = DFHPF1
00513          GO TO 7500-PAGE-FORWARD.
00514
00515      IF EIBAID = DFHPF2
00516          GO TO 7600-PAGE-BACKWARD.
00517
00518      IF EIBAID = DFHPF7
00519          MOVE LOW-VALUES         TO  PI-ERREIN-KEY
00520          MOVE PI-SAVE-TABLE      TO  PI-CRR-TABLE
00521          MOVE 'A'                TO  PI-CRR-CODE
00522          MOVE XCTL-651           TO  PGM-NAME
00523          GO TO 9300-XCTL.
00524
00525      IF EIBAID = DFHPF8
00526          MOVE 'EL6511B'          TO  PI-MAPNAME
00527          MOVE 'B'                TO  PI-CRR-CODE
00528          GO TO 5000-BUILD-INITIAL-SCREEN.
00529
00530      IF EIBAID = DFHENTER
00531          GO TO 4400-CHANGE.
00532
00533      MOVE ER-0029                TO  EMI-ERROR.
00534
00535  3200-INPUT-ERROR.
00536      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00537      MOVE AL-UNBON               TO  CENTERA.
00538      MOVE -1                     TO  CENTERL.
00539      GO TO 8210-SEND-DATAONLY.
00540
00541      EJECT
00542  4000-EDIT-MAINT.
00543      IF MAINTYPL GREATER THAN ZERO
00544          MOVE MAINTYPI           TO  PI-CHECK-MAINT-TYPE
00545          IF VALID-MAINT-TYPE
00546              MOVE AL-UANON       TO  MAINTYPA
00547          ELSE
00548              MOVE -1             TO  MAINTYPL
00549              MOVE AL-UABON       TO  MAINTYPA
00550              MOVE ER-2039        TO  EMI-ERROR
00551              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00552      ELSE
00553          MOVE -1                 TO  MAINTYPL
00554          MOVE AL-UABON           TO  MAINTYPA
00555          MOVE ER-2039            TO  EMI-ERROR
00556          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00557
00558      IF NOT MODIFY-CAP
00559          IF SHOW-FUNCTION
00560              NEXT SENTENCE
00561          ELSE
00562              MOVE 'UPDATE'       TO SM-READ
00563              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00564              MOVE ER-0070        TO  EMI-ERROR
00565              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00566              GO TO 8100-SEND-INITIAL-MAP.
00567
00568      IF COMPANYL GREATER ZERO
00569          MOVE ZERO TO TALLY
00570          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES
00571          IF TALLY GREATER ZERO
00572              MOVE -1             TO  COMPANYL
00573              MOVE AL-UABON       TO  COMPANYA
00574              MOVE ER-2340        TO  EMI-ERROR
00575              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00576          ELSE
00577              MOVE AL-UANON       TO  COMPANYA
00578              MOVE COMPANYI       TO  PI-CRR-TABLE
00579      ELSE
00580          MOVE -1                 TO  COMPANYL
00581          MOVE AL-UABON           TO  COMPANYA
00582          MOVE ER-2140            TO  EMI-ERROR
00583          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00584
00585      IF COMPSUBL IS GREATER THAN ZERO
00586          MOVE COMPSUBI           TO  PI-CRR-TABLE-SUB
00587          MOVE AL-UANON           TO  COMPSUBA
00588      ELSE
00589          MOVE ZEROS              TO  PI-CRR-TABLE-SUB.
00590
00591      IF NOT MODIFY-CAP
00592          IF SHOW-FUNCTION
00593              NEXT SENTENCE
00594          ELSE
00595              MOVE 'UPDATE'       TO SM-READ
00596              PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00597              MOVE ER-0070        TO  EMI-ERROR
00598              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00599              GO TO 8100-SEND-INITIAL-MAP.
00600
00601      IF EMI-NO-ERRORS
00602          NEXT SENTENCE
00603      ELSE
00604          GO TO 8200-SEND-DATAONLY.
00605
00606      IF CHANGE-FUNCTION
00607          GO TO 4400-CHANGE.
00608
00609      IF DELETE-FUNCTION
00610          GO TO 4600-DELETE.
00611
00612      IF SHOW-FUNCTION
00613          GO TO 5000-BUILD-INITIAL-SCREEN.
00614
00615      IF ADD-FUNCTION
00616          GO TO 4200-ADD.
00617
00618      MOVE -1                     TO  MAINTYPL.
00619      MOVE ER-2056                TO  EMI-ERROR.
00620      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00621      GO TO 8200-SEND-DATAONLY.
00622
00623  4000-EXIT.
00624      EXIT.
00625      EJECT
00626
00627  4200-ADD.
00628      PERFORM 6400-EDIT THRU 6400-EXIT.
00629
00630      IF EMI-NO-ERRORS
00631          NEXT SENTENCE
00632      ELSE
00633          GO TO 8200-SEND-DATAONLY.
00634
00635      
      * EXEC CICS HANDLE CONDITION
00636 *        NOTOPEN  (9990-ABEND)
00637 *        NOTFND   (4250-CONT)
00638 *    END-EXEC.
      *    MOVE '"$JI                  ! # #00004414' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034343134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00639
00640      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.
00641
00642      MOVE ER-2139                TO  EMI-ERROR.
00643      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00644      MOVE -1                     TO  MAINTYPL.
00645      GO TO 8200-SEND-DATAONLY.
00646
00647  4250-CONT.
00648      PERFORM 7300-ERREIN-GETMAIN THRU 7300-EXIT.
00649
00650      MOVE ZEROS                  TO  RE-LF-FEE
00651                                      RE-AH-FEE
00652                                      RE-AH-PR-PCT
00653                                      RE-AH-78-PCT
00654                                      RE-LF-PR-PCT
00655                                      RE-LF-78-PCT
00656                                      RE-LF-IBNR-PCT
00657                                      RE-AH-IBNR-PCT
00658                                      RE-LF-CLM-PCT
00659                                      RE-LF-CLM-MAX
00660                                      RE-AH-CLM-PCT
00661                                      RE-AH-CLM-MAX
00662                                      RE-LF-FEE-RANGE-PCT (1)
00663                                      RE-LF-FEE-RANGE-PCT (2)
00664                                      RE-LF-FEE-RANGE-PCT (3)
00665                                      RE-LF-FEE-RANGE-PCT (4)
00666                                      RE-LF-FEE-RANGE-PCT (5)
00667                                      RE-LF-FEE-RANGE-PCT (6)
00668                                      RE-LF-FEE-THRU-AMT (1)
00669                                      RE-LF-FEE-THRU-AMT (2)
00670                                      RE-LF-FEE-THRU-AMT (3)
00671                                      RE-LF-FEE-THRU-AMT (4)
00672                                      RE-LF-FEE-THRU-AMT (5)
00673                                      RE-LF-FEE-THRU-AMT (6)
00674                                      RE-AH-FEE-RANGE-PCT (1)
00675                                      RE-AH-FEE-RANGE-PCT (2)
00676                                      RE-AH-FEE-RANGE-PCT (3)
00677                                      RE-AH-FEE-RANGE-PCT (4)
00678                                      RE-AH-FEE-RANGE-PCT (5)
00679                                      RE-AH-FEE-RANGE-PCT (6)
00680                                      RE-AH-FEE-THRU-AMT (1)
00681                                      RE-AH-FEE-THRU-AMT (2)
00682                                      RE-AH-FEE-THRU-AMT (3)
00683                                      RE-AH-FEE-THRU-AMT (4)
00684                                      RE-AH-FEE-THRU-AMT (5)
00685                                      RE-AH-FEE-THRU-AMT (6)
00686                                      RE-CLM-INCURRED-LIM
00687                                      RE-CUSTODIAL-BAL
00688                                      RE-EARNING-START-DT
00689                                      RE-EARNING-STOP-DT
032707                                     RE-EXCISE-TAX
00690
00691      IF PI-CARRIER-SECURITY GREATER SPACE
00692          MOVE PI-CARRIER-SECURITY    TO  RE-COMP-CARRIER-SECURITY.
00693
00694      PERFORM 6000-CHECK-FOR-UPDATE THRU 6000-EXIT.
00695
00696      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
00697      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
00698      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00699
00700      MOVE '5'                    TO  DC-OPTION-CODE.
00701      MOVE LINK-ELDATCV           TO  PGM-NAME.
00702
00703      
      * EXEC CICS LINK
00704 *        PROGRAM   (PGM-NAME)
00705 *        COMMAREA  (DATE-CONVERSION-DATA)
00706 *        LENGTH    (DC-COMM-LENGTH)
00707 *    END-EXEC.
      *    MOVE '."C                   (   #00004483' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00708
00709      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT
00710                                      BIN-CURRENT-SAVE.
00711      MOVE PI-COMPANY-CD          TO  RE-COMPANY-CD.
00712      MOVE 'RE'                   TO  RE-RECORD-ID.
00713 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00714 *    MOVE 'A'                    TO  JP-RECORD-TYPE.
00715 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00716 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00717
00718      
      * EXEC CICS WRITE
00719 *        DATASET (REIN-FILE-ID)
00720 *        FROM    (REINSURANCE-RECORD)
00721 *        RIDFLD  (RE-CONTROL-PRIMARY)
00722 *        END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004498' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 RE-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00723
00724 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00725      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00726      MOVE ER-0000                TO  EMI-ERROR.
00727      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00728
00729      MOVE LOW-VALUES             TO  EL6511BO.
00730
00731      MOVE PI-CRR-TABLE           TO  COMPANYO.
00732      MOVE PI-CRR-TABLE-SUB       TO  COMPSUBO.
00733      MOVE AL-UANON               TO  COMPANYA
00734                                      COMPSUBA.
00735
00736      GO TO 5000-BUILD-INITIAL-SCREEN.
00737
00738  4200-EXIT.
00739      EXIT.
00740      EJECT
00741
00742  4400-CHANGE.
00743      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY
00744          NEXT SENTENCE
00745      ELSE
00746          MOVE ER-2056            TO  EMI-ERROR
00747          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00748          IF PI-MAP-B
00749              MOVE -1             TO  MAINTYPL
00750              GO TO 8200-SEND-DATAONLY
00751          ELSE
00752              MOVE -1             TO  DESC1L
00753              GO TO 8210-SEND-DATAONLY.
00754
00755      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.
00756
00757      IF PI-MAP-B
00758          PERFORM 6400-EDIT THRU 6400-EXIT.
00759
00760      IF EMI-NO-ERRORS
00761          NEXT SENTENCE
00762      ELSE
00763          IF PI-MAP-C
00764              GO TO 8210-SEND-DATAONLY
00765          ELSE
00766              GO TO 8200-SEND-DATAONLY.
00767
00768      PERFORM 7400-READ-ERREIN-UPDATE THRU 7400-EXIT
00769
00770 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA
00771
00772      IF PI-MAP-C
00773          PERFORM 6200-CHECK-FOR-UPDATE THRU 6200-EXIT
00774      ELSE
00775          PERFORM 6000-CHECK-FOR-UPDATE THRU 6000-EXIT.
00776
00777      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR
00778         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00779          NEXT SENTENCE
00780      ELSE
00781          
      * EXEC CICS UNLOCK
00782 *             DATASET  (REIN-FILE-ID)
00783 *             END-EXEC
      *    MOVE '&*                    #   #00004561' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00784          MOVE ER-0068            TO  EMI-ERROR
00785          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00786          IF PI-MAP-C
00787              GO TO 5500-BUILD-INITIAL-SCREEN
00788          ELSE
00789              GO TO 5000-BUILD-INITIAL-SCREEN.
00790
00791      MOVE PI-PROCESSOR-ID        TO  RE-LAST-MAINT-USER.
00792      MOVE EIBTIME                TO  RE-LAST-MAINT-HHMMSS.
00793      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00794      MOVE '5'                    TO  DC-OPTION-CODE.
00795      MOVE LINK-ELDATCV           TO  PGM-NAME.
00796
00797      
      * EXEC CICS LINK
00798 *        PROGRAM   (PGM-NAME)
00799 *        COMMAREA  (DATE-CONVERSION-DATA)
00800 *        LENGTH    (DC-COMM-LENGTH)
00801 *    END-EXEC.
      *    MOVE '."C                   (   #00004577' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00802
00803      MOVE DC-BIN-DATE-1          TO  RE-LAST-MAINT-DT
00804                                      BIN-CURRENT-SAVE.
00805 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00806 *    MOVE 'B'                    TO  JP-RECORD-TYPE.
00807 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00808 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00809 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00810
00811      
      * EXEC CICS REWRITE
00812 *        DATASET  (REIN-FILE-ID)
00813 *        FROM     (REINSURANCE-RECORD)
00814 *        END-EXEC.
           MOVE LENGTH OF
            REINSURANCE-RECORD
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004591' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 REINSURANCE-RECORD, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00815
00816 *    MOVE 'C'                    TO  JP-RECORD-TYPE.
00817 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00818 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00819 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00820      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00821      MOVE ER-0000                TO  EMI-ERROR.
00822      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00823
00824      IF PI-MAP-C
00825          MOVE LOW-VALUES         TO  EL6511CO
00826          GO TO 5500-BUILD-INITIAL-SCREEN
00827      ELSE
00828          MOVE LOW-VALUES         TO  EL6511BO
00829          MOVE PI-CRR-TABLE       TO  COMPANYO
00830          MOVE PI-CRR-TABLE-SUB   TO  COMPSUBO
00831          MOVE AL-UANON           TO  COMPANYA
00832                                      COMPSUBA
00833          GO TO 5000-BUILD-INITIAL-SCREEN.
00834
00835  4400-EXIT.
00836      EXIT.
00837      EJECT
00838  4600-DELETE.
00839      IF PI-ERREIN-KEY = PI-SAVE-ERREIN-KEY
00840          NEXT SENTENCE
00841      ELSE
00842          MOVE ER-2056            TO  EMI-ERROR
00843          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00844          MOVE -1                 TO  MAINTYPL
00845          GO TO 8200-SEND-DATAONLY.
00846
00847      PERFORM 7400-READ-ERREIN-UPDATE THRU 7400-EXIT.
00848
00849 *    MOVE REINSURANCE-RECORD     TO  JP-RECORD-AREA.
00850
00851      IF RE-LAST-MAINT-USER = PI-UPDATE-BY OR
00852         RE-LAST-MAINT-HHMMSS = PI-UPDATE-HHMMSS
00853          NEXT SENTENCE
00854      ELSE
00855          
      * EXEC CICS UNLOCK
00856 *             DATASET  (REIN-FILE-ID)
00857 *        END-EXEC
      *    MOVE '&*                    #   #00004635' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00858          MOVE ER-0068            TO  EMI-ERROR
00859          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00860          GO TO 5000-BUILD-INITIAL-SCREEN.
00861
00862      
      * EXEC CICS DELETE
00863 *         DATASET  (REIN-FILE-ID)
00864 *    END-EXEC.
      *    MOVE '&(                    &   #00004642' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00865
00866 *    MOVE 'D'                    TO  JP-RECORD-TYPE.
00867 *    MOVE ERREIN-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
00868 *    MOVE REIN-FILE-ID           TO  FILE-ID.
00869 *    PERFORM 8400-LOG-JOURNAL-RECORD.
00870
00871      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00872      MOVE '5'                    TO  DC-OPTION-CODE.
00873      MOVE LINK-ELDATCV           TO  PGM-NAME.
00874
00875      
      * EXEC CICS LINK
00876 *        PROGRAM   (PGM-NAME)
00877 *        COMMAREA  (DATE-CONVERSION-DATA)
00878 *        LENGTH    (DC-COMM-LENGTH)
00879 *    END-EXEC.
      *    MOVE '."C                   (   #00004655' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00880
00881      MOVE DC-BIN-DATE-1          TO  BIN-CURRENT-SAVE.
00882      PERFORM 8000-UPDATE-MAINT-DATE THRU 8000-EXIT.
00883
00884      MOVE ER-0000                TO  EMI-ERROR.
00885      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00886
00887      MOVE LOW-VALUES             TO  EL6511BO.
00888
00889      MOVE PI-CRR-TABLE           TO  COMPANYO.
00890      MOVE PI-CRR-TABLE-SUB       TO  COMPSUBO.
00891      MOVE AL-UANON               TO  COMPANYA
00892                                      COMPSUBA.
00893
00894      GO TO 8100-SEND-INITIAL-MAP.
00895
00896  4600-EXIT.
00897      EXIT.
00898      EJECT
00899
00900  5000-BUILD-INITIAL-SCREEN.
00901      MOVE LOW-VALUES             TO  EL6511BO.
00902
00903      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.
00904
00905      IF PI-CRR-TABLE = LOW-VALUES
00906          GO TO 8100-SEND-INITIAL-MAP.
00907
00908      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.
00909
00910 ******************************************************************
00911 *    ADD CODE TO SHOW RECORDS WHICH HAVE A MATCHING CARRIER IF   *
00912 *    USER HAS CARRIER SECURITY.                                  *
00913 ******************************************************************
00914
00915      IF PI-CARRIER-SECURITY GREATER SPACE
00916          IF PI-CARRIER-SECURITY = RE-COMP-CARRIER-SECURITY
00917              NEXT SENTENCE
00918          ELSE
00919              GO TO 8880-NOT-FOUND.
00920
00921  5050-SET-UP-SCREEN.
00922
00923      IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
00924         IF RE-CUSTODIAL-BAL NOT NUMERIC
00925             MOVE ZEROS              TO  RE-CUSTODIAL-BAL
00926         ELSE
00927             IF RE-CUSTODIAL-BAL  =  ZEROS
00928                 NEXT SENTENCE
00929             ELSE
00930                 MOVE RE-CUSTODIAL-BAL   TO  CUSTAMTO
00931                 MOVE AL-UNNON           TO  CUSTAMTA
00932      ELSE
00933          MOVE AL-SANOF              TO  CUSTAMTA
00934          MOVE AL-SADOF              TO  CUSTDSCA.
00935
00936      IF RE-LF-FEE = ZEROS
00937          NEXT SENTENCE
00938      ELSE
00939          MOVE RE-LF-FEE          TO  LIFEFEEO
00940          MOVE AL-UNNON           TO  LIFEFEEA.
00941
00942      IF RE-AH-FEE = ZEROS
00943          NEXT SENTENCE
00944      ELSE
00945          MOVE RE-AH-FEE          TO  AHFEEO
00946          MOVE AL-UNNON           TO  AHFEEA.
00947
00948      IF RE-AH-PR-PCT NUMERIC
00949          MOVE RE-AH-PR-PCT       TO  AHPRO
00950          MOVE AL-UNNON           TO  AHPRA.
00951
00952      IF RE-AH-78-PCT NUMERIC
00953          MOVE RE-AH-78-PCT       TO  AHR78O
00954          MOVE AL-UNNON           TO  AHR78A.
00955
00956      IF RE-LF-PR-PCT NOT NUMERIC
00957          MOVE ZEROS              TO RE-LF-PR-PCT.
00958
00959      IF RE-LF-PR-PCT NUMERIC
00960          MOVE RE-LF-PR-PCT       TO  LFPRO
00961          MOVE AL-UNNON           TO  LFPRA.
00962
00963      IF RE-LF-78-PCT NOT NUMERIC
00964          MOVE ZEROS              TO RE-LF-78-PCT.
00965
00966      IF RE-LF-78-PCT NUMERIC
00967          MOVE RE-LF-78-PCT       TO  LFR78O
00968          MOVE AL-UNNON           TO  LFR78A.
00969
00970      IF RE-LF-IBNR-PCT NOT = ZEROS
00971          MOVE RE-LF-IBNR-PCT     TO  LFIBNRO
00972          MOVE AL-UNNON           TO  LFIBNRA.
00973
00974      IF RE-AH-IBNR-PCT NOT = ZEROS
00975          MOVE RE-AH-IBNR-PCT     TO  AHIBNRO
00976          MOVE AL-UNNON           TO  AHIBNRA.
00977
00978      IF RE-LF-CLM-PCT NUMERIC
00979         MOVE RE-LF-CLM-PCT       TO  LCLMPCTO
00980         MOVE AL-UNNON            TO  LCLMPCTA.
00981
           IF RE-EXCISE-TAX NOT NUMERIC
              MOVE ZEROS               TO RE-EXCISE-TAX
           END-IF
032707     MOVE RE-EXCISE-TAX          TO EXTAXO
032707     MOVE AL-UNNON               TO EXTAXA
00982      IF RE-LF-CLM-MAX NUMERIC
00983         MOVE RE-LF-CLM-MAX       TO  LCLMMAXO
00984         MOVE AL-UNNON            TO  LCLMMAXA.
00985
00986      IF RE-AH-CLM-PCT NUMERIC
00987         MOVE RE-AH-CLM-PCT       TO  ACLMPCTO
00988         MOVE AL-UNNON            TO  ACLMPCTA.
00989
00990      IF RE-AH-CLM-MAX NUMERIC
00991         MOVE RE-AH-CLM-MAX       TO  ACLMMAXO
00992         MOVE AL-UNNON            TO  ACLMMAXA.
00993
00994      IF (RE-LF-FEE-RANGE-PCT (1) NUMERIC)
00995         MOVE RE-LF-FEE-RANGE-PCT (1) TO  LFPCT1O
00996         MOVE AL-UNNON                TO  LFPCT1A.
00997
00998      IF (RE-LF-FEE-RANGE-PCT (2) NUMERIC)
00999         MOVE RE-LF-FEE-RANGE-PCT (2) TO  LFPCT2O
01000         MOVE AL-UNNON                TO  LFPCT2A.
01001
01002      IF (RE-LF-FEE-RANGE-PCT (3) NUMERIC)
01003         MOVE RE-LF-FEE-RANGE-PCT (3) TO  LFPCT3O
01004         MOVE AL-UNNON                TO  LFPCT3A.
01005
01006      IF (RE-LF-FEE-RANGE-PCT (4) NUMERIC)
01007         MOVE RE-LF-FEE-RANGE-PCT (4) TO  LFPCT4O
01008         MOVE AL-UNNON                TO  LFPCT4A.
01009
01010      IF (RE-LF-FEE-RANGE-PCT (5) NUMERIC)
01011         MOVE RE-LF-FEE-RANGE-PCT (5) TO  LFPCT5O
01012         MOVE AL-UNNON                TO  LFPCT5A.
01013
01014      IF (RE-LF-FEE-RANGE-PCT (6) NUMERIC)
01015         MOVE RE-LF-FEE-RANGE-PCT (6) TO  LFPCT6O
01016         MOVE AL-UNNON                TO  LFPCT6A.
01017
01018      IF (RE-LF-FEE-THRU-AMT  (1) NUMERIC)
01019         MOVE RE-LF-FEE-THRU-AMT (1)  TO  LFTHRU1O
01020         MOVE AL-UNNON                TO  LFTHRU1A.
01021
01022      IF (RE-LF-FEE-THRU-AMT  (2) NUMERIC)
01023         MOVE RE-LF-FEE-THRU-AMT (2)  TO  LFTHRU2O
01024         MOVE AL-UNNON                TO  LFTHRU2A.
01025
01026      IF (RE-LF-FEE-THRU-AMT  (3) NUMERIC)
01027         MOVE RE-LF-FEE-THRU-AMT (3)  TO  LFTHRU3O
01028         MOVE AL-UNNON                TO  LFTHRU3A.
01029
01030      IF (RE-LF-FEE-THRU-AMT  (4) NUMERIC)
01031         MOVE RE-LF-FEE-THRU-AMT (4)  TO  LFTHRU4O
01032         MOVE AL-UNNON                TO  LFTHRU4A.
01033
01034      IF (RE-LF-FEE-THRU-AMT  (5) NUMERIC)
01035         MOVE RE-LF-FEE-THRU-AMT (5)  TO  LFTHRU5O
01036         MOVE AL-UNNON                TO  LFTHRU5A.
01037
01038      IF (RE-LF-FEE-THRU-AMT  (6) NUMERIC)
01039         MOVE RE-LF-FEE-THRU-AMT (6)  TO  LFTHRU6O
01040         MOVE AL-UNNON                TO  LFTHRU6A.
01041
01042      IF (RE-AH-FEE-RANGE-PCT (1) NUMERIC)
01043         MOVE RE-AH-FEE-RANGE-PCT (1) TO  AHPCT1O
01044         MOVE AL-UNNON                TO  AHPCT1A.
01045
01046      IF (RE-AH-FEE-RANGE-PCT (2) NUMERIC)
01047         MOVE RE-AH-FEE-RANGE-PCT (2) TO  AHPCT2O
01048         MOVE AL-UNNON                TO  AHPCT2A.
01049
01050      IF (RE-AH-FEE-RANGE-PCT (3) NUMERIC)
01051         MOVE RE-AH-FEE-RANGE-PCT (3) TO  AHPCT3O
01052         MOVE AL-UNNON                TO  AHPCT3A.
01053
01054      IF (RE-AH-FEE-RANGE-PCT (4) NUMERIC)
01055         MOVE RE-AH-FEE-RANGE-PCT (4) TO  AHPCT4O
01056         MOVE AL-UNNON                TO  AHPCT4A.
01057
01058      IF (RE-AH-FEE-RANGE-PCT (5) NUMERIC)
01059         MOVE RE-AH-FEE-RANGE-PCT (5) TO  AHPCT5O
01060         MOVE AL-UNNON                TO  AHPCT5A.
01061
01062      IF (RE-AH-FEE-RANGE-PCT (6) NUMERIC)
01063         MOVE RE-AH-FEE-RANGE-PCT (6) TO  AHPCT6O
01064         MOVE AL-UNNON                TO  AHPCT6A.
01065
01066      IF (RE-AH-FEE-THRU-AMT  (1) NUMERIC)
01067         MOVE RE-AH-FEE-THRU-AMT (1)  TO  AHTHRU1O
01068         MOVE AL-UNNON                TO  AHTHRU1A.
01069
01070      IF (RE-AH-FEE-THRU-AMT  (2) NUMERIC)
01071         MOVE RE-AH-FEE-THRU-AMT (2)  TO  AHTHRU2O
01072         MOVE AL-UNNON                TO  AHTHRU2A.
01073
01074      IF (RE-AH-FEE-THRU-AMT  (3) NUMERIC)
01075         MOVE RE-AH-FEE-THRU-AMT (3)  TO  AHTHRU3O
01076         MOVE AL-UNNON                TO  AHTHRU3A.
01077
01078      IF (RE-AH-FEE-THRU-AMT  (4) NUMERIC)
01079         MOVE RE-AH-FEE-THRU-AMT (4)  TO  AHTHRU4O
01080         MOVE AL-UNNON                TO  AHTHRU4A.
01081
01082      IF (RE-AH-FEE-THRU-AMT  (5) NUMERIC)
01083         MOVE RE-AH-FEE-THRU-AMT (5)  TO  AHTHRU5O
01084         MOVE AL-UNNON                TO  AHTHRU5A.
01085
01086      IF (RE-AH-FEE-THRU-AMT  (6) NUMERIC)
01087         MOVE RE-AH-FEE-THRU-AMT (6)  TO  AHTHRU6O
01088         MOVE AL-UNNON                TO  AHTHRU6A.
01089
01090 ***  Y2K PROJ 7744
01091      IF RE-CLM-INCURRED-LIM = ZEROS
01092          CONTINUE
01093      ELSE
LGC192         MOVE RE-CLM-INCURRED-LIM
LGC192                                 TO  WS-RE-CLM-INCURRED-LIM-N
01094          MOVE RE-CLM-YR          TO  WS-YR
01095          MOVE RE-CLM-MO          TO  WS-MO
01096          MOVE RE-CLM-DA          TO  WS-DA
01097          MOVE WS-DATE            TO  CLINCDTO
01098          MOVE AL-UNNON           TO  CLINCDTA
01099      END-IF.
01100
01101      IF RE-EARNING-START-DT = ZEROS
01102          CONTINUE
01103      ELSE
LGC190         MOVE RE-EARNING-START-DT
LGC190                                 TO  WS-RE-EARNING-START-DT-N
01104          MOVE RE-EARN-YR         TO  WS-YR
01105          MOVE RE-EARN-MO         TO  WS-MO
01106          MOVE RE-EARN-DA         TO  WS-DA
01107          MOVE WS-DATE            TO  ERBEGDTO
01108          MOVE AL-UNNON           TO  ERBEGDTA
01109      END-IF.
01110
01111      IF RE-EARNING-STOP-DT NOT NUMERIC
01112          MOVE ZEROS              TO  RE-EARNING-STOP-DT
01113      END-IF.
01114
01115      IF RE-EARNING-STOP-DT NOT = ZEROS
LGC190         MOVE RE-EARNING-STOP-DT
LGC190                                 TO  WS-RE-EARNING-STOP-DT-N
01116          MOVE RE-EARN-STOP-YR    TO  WS-YR
01117          MOVE RE-EARN-STOP-MO    TO  WS-MO
01118          MOVE RE-EARN-STOP-DA    TO  WS-DA
01119          MOVE WS-DATE            TO  ERENDDTO
01120          MOVE AL-UNNON           TO  ERENDDTA
01121      END-IF.
01122 ***  Y2K PROJ 7744
01123
01124      MOVE RE-EARN-STOP-CODE      TO  ERENDCDO.
01125      MOVE AL-UANON               TO  ERENDCDA.
01126
01127      MOVE PI-LIFE-OVERRIDE-L2    TO  LFCVTYPO.
01128      MOVE PI-AH-OVERRIDE-L2      TO  AHCVTYPO.
CIDMOD     MOVE PI-LIFE-OVERRIDE-L6    TO  LFCVTYPO.
CIDMOD     MOVE PI-AH-OVERRIDE-L6      TO  AHCVTYPO.
01129      MOVE AL-SANON               TO  LFCVTYPA
01130                                      AHCVTYPA.
01131      MOVE RE-LF-FEE-METHOD       TO  LFMETHO.
01132      MOVE RE-LF-FEE-BASIS        TO  LFBASISO.
01133      MOVE RE-AH-FEE-METHOD       TO  AHMETHO.
01134      MOVE RE-AH-FEE-BASIS        TO  AHBASISO.
01135      MOVE AL-UANON               TO  LFMETHA
01136                                      LFBASISA
01137                                      AHMETHA
01138                                      AHBASISA.
01139      MOVE RE-NAME                TO  CONAMEO.
01140      MOVE RE-CEDE-NAME           TO  CEDNAMEO.
01141      MOVE RE-CEDING-TYPE-FLAG    TO  CESTYPEO.
01142      MOVE RE-GL-CENTER           TO  GLCNTRO.
01143      MOVE RE-LF-PE               TO  CMLIFEO.
01144      MOVE RE-AH-PE               TO  CMAHO.
01145      MOVE RE-MORT-CODE           TO  MORTCDO.
01146      MOVE RE-MORT-SW             TO  MORTSWO.
01147      MOVE RE-ZERO-LF-FEE         TO  FEELIFEO.
01148      MOVE RE-ZERO-AH-FEE         TO  FEEAHO.
01149      MOVE RE-PRT-ST              TO  PRTAXO.
01150      MOVE RE-LF-COMM             TO  COMLIFEO.
01151      MOVE RE-AH-COMM             TO  COMAHO.
01152      MOVE RE-PRT-OW              TO  PRTAXOWO.
01153      MOVE RE-PRT-CRSV            TO  PRTCRSVO.
01154      MOVE RE-CEDING-STMT-OPT-A   TO  RPTAO.
01155      MOVE RE-CEDING-STMT-OPT-B   TO  RPTBO.
01156      MOVE RE-CEDING-STMT-OPT-C   TO  RPTCO.
01157      MOVE RE-CEDING-STMT-OPT-D   TO  RPTDO.
01158      MOVE RE-CEDING-STMT-OPT-E   TO  RPTEO.
110601     MOVE RE-STATE-EXHIBIT-OPT-F TO  RPTFO.
01159      MOVE RE-CLAIM-CODE          TO  CLAIMO.
01160      MOVE RE-LF-TAX              TO  TAXLIFEO.
01161      MOVE RE-AH-TAX              TO  TAXAHO.
01162      MOVE RE-COMP-CARRIER-SECURITY
01163                                  TO  BCARO.
01164      MOVE RE-REINS-GROUPING-CODE TO  REIGRPO.
01165
01166      MOVE RE-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
01167      MOVE SPACE                  TO  DC-OPTION-CODE.
01168      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
01169      MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO.
01170      MOVE RE-LAST-MAINT-HHMMSS   TO  TIME-IN.
01171      MOVE TIME-OUT               TO  ALUTIMEO.
01172      MOVE RE-LAST-MAINT-USER     TO  ALUBYO.
01173
01174
01175      MOVE AL-UANON               TO  CONAMEA
01176                                      REIGRPA
01177                                      PRTAXA
01178                                      PRTAXOWA
01179                                      PRTCRSVA
01180                                      RPTAA
01181                                      RPTBA
01182                                      RPTCA
01183                                      RPTDA
01184                                      RPTEA
110601                                     RPTFA
01185                                      CEDNAMEA
01186                                      CESTYPEA
01187                                      CMLIFEA
01188                                      CMAHA
01189                                      MORTCDA
01190                                      MORTSWA
01191                                      FEELIFEA
01192                                      FEEAHA
01193                                      COMLIFEA
01194                                      COMAHA
01195                                      CLAIMA
01196                                      TAXLIFEA
01197                                      TAXAHA
01198                                      COMPANYA
01199                                      COMPSUBA
032707                                     EXTAXA
01200
01201      IF PI-COMPANY-ID = 'NCL' OR 'LGX'
01202         MOVE AL-UANON            TO  GLCNTRA.
01203
01204      IF PI-NO-CARRIER-SECURITY
01205          MOVE AL-UANON           TO  BCARA
01206      ELSE
01207          MOVE AL-SANOF           TO  BCARA.
01208
01209      MOVE PI-CRR-TABLE           TO  COMPANYO.
01210      MOVE PI-CRR-TABLE-SUB       TO  COMPSUBO.
01211      MOVE PI-CHECK-MAINT-TYPE    TO  PI-PREV-MAINTYP.
01212
01213      GO TO 8100-SEND-INITIAL-MAP.
01214
01215      EJECT
01216  5500-BUILD-INITIAL-SCREEN.
01217      MOVE LOW-VALUES             TO  EL6511CO.
01218
01219      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.
01220
01221      IF PI-CRR-TABLE = LOW-VALUES
01222          GO TO 8110-SEND-INITIAL-MAP.
01223
01224      PERFORM 7200-READ-ERREIN THRU 7200-EXIT.
01225
01226 ******************************************************************
01227 *    ADD CODE TO SHOW RECORDS WHICH HAVE A MATCHING CARRIER IF   *
01228 *    USER HAS CARRIER SECURITY.                                  *
01229 ******************************************************************
01230
01231      IF PI-CARRIER-SECURITY GREATER SPACE
01232          IF PI-CARRIER-SECURITY = RE-COMP-CARRIER-SECURITY
01233              NEXT SENTENCE
01234          ELSE
01235              GO TO 8880-NOT-FOUND.
01236
01237  5550-SET-UP-SCREEN.
01238      MOVE PI-CRR-TABLE           TO  COMPO.
01239      MOVE PI-CRR-TABLE-SUB       TO  SUBO.
01240      MOVE RE-COMP-CARRIER-SECURITY
01241                                  TO  BCARO.
01242
01243      MOVE +0                     TO  SUB1.
01244      SET DO-INDX                 TO  +1.
01245      SET DO-INDX DOWN BY +1.
01246
01247  5525-CONT.
01248      SET DO-INDX UP BY +1.
01249      ADD +1                      TO  SUB1.
01250
01251      IF DO-INDX GREATER +18
01252          GO TO 5550-SET-ATTR.
01253
01254      MOVE RE-DESC (SUB1)         TO  DESCO (DO-INDX).
01255      MOVE AL-UANON               TO  DESCA (DO-INDX).
01256
01257      GO TO 5525-CONT.
01258
01259  5550-SET-ATTR.
01260      MOVE RE-COMP-CARRIER-SECURITY
01261                                  TO  CCARO.
01262      MOVE -1                     TO  DESC1L.
01263      MOVE AL-SANON               TO  COMPA
01264                                      SUBA.
01265      GO TO 8110-SEND-INITIAL-MAP.
01266
01267      EJECT
01268  6000-CHECK-FOR-UPDATE.
01269       IF CHANGE-FUNCTION
01270           GO TO 6010-CONT.
01271
01272       IF COMPANYL GREATER ZERO
01273           MOVE COMPANYI          TO  RE-COMP-PRIME.
01274
01275       IF COMPSUBL GREATER ZERO
01276           MOVE COMPSUBI          TO  RE-COMP-SUB.
01277
01278  6010-CONT.
01279
01280       IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
01281          IF CUSTAMTL  GREATER  ZERO
01282              MOVE WS-CUSTODIAL-BAL  TO  RE-CUSTODIAL-BAL.
01283
01284       IF REIGRPL GREATER ZERO
01285           MOVE REIGRPI           TO  RE-REINS-GROUPING-CODE.
01286
01287       IF BCARL GREATER ZERO
01288           MOVE BCARI             TO  RE-COMP-CARRIER-SECURITY.
01289
01290       IF CONAMEL GREATER ZERO
01291           MOVE CONAMEI           TO  RE-NAME.
01292
01293       IF CEDNAMEL GREATER ZERO
01294           MOVE CEDNAMEI          TO  RE-CEDE-NAME.
01295
01296       IF CESTYPEL GREATER ZERO
01297           MOVE CESTYPEI          TO  RE-CEDING-TYPE-FLAG.
01298
01299       IF GLCNTRL  GREATER ZERO
01300          MOVE GLCNTRI            TO  RE-GL-CENTER.
01301
01302       IF CMLIFEL GREATER ZERO
01303           MOVE CMLIFEI           TO  RE-LF-PE.
01304
01305       IF CMAHL GREATER ZERO
01306           MOVE CMAHI             TO  RE-AH-PE.
01307
01308       IF AHPRL GREATER ZERO
01309           MOVE AHPRI             TO  RE-AH-PR-PCT.
01310
01311       IF AHR78L GREATER ZERO
01312           MOVE AHR78I            TO  RE-AH-78-PCT.
01313
01314       IF LFPRL GREATER ZERO
01315           MOVE LFPRI             TO  RE-LF-PR-PCT.
01316
01317       IF LFR78L GREATER ZERO
01318           MOVE LFR78I            TO  RE-LF-78-PCT.
01319
01320       IF MORTCDL GREATER ZERO
01321           MOVE MORTCDI           TO  RE-MORT-CODE.
01322
01323       IF MORTSWL GREATER ZERO
01324           MOVE MORTSWI           TO  RE-MORT-SW.
01325
01326       IF LFIBNRL GREATER ZERO
01327           MOVE LFIBNRI           TO  RE-LF-IBNR-PCT.
01328
01329       IF AHIBNRL GREATER ZERO
01330           MOVE AHIBNRI           TO  RE-AH-IBNR-PCT.
01331
01332       IF LIFEFEEL GREATER ZERO
01333           MOVE LIFEFEEI          TO  RE-LF-FEE.
01334
01335       IF AHFEEL GREATER ZERO
01336           MOVE AHFEEI            TO  RE-AH-FEE.
01337
01338       IF FEELIFEL GREATER ZERO
01339           MOVE FEELIFEI          TO  RE-ZERO-LF-FEE.
01340
01341       IF FEEAHL GREATER ZERO
01342           MOVE FEEAHI            TO  RE-ZERO-AH-FEE.
01343
01344       IF PRTAXL GREATER ZERO
01345           MOVE PRTAXI            TO  RE-PRT-ST.
01346
01347       IF COMLIFEL GREATER ZERO
01348           MOVE COMLIFEI          TO  RE-LF-COMM.
01349
01350       IF COMAHL GREATER ZERO
01351           MOVE COMAHI            TO  RE-AH-COMM.
01352
01353       IF PRTAXOWL GREATER ZERO
01354           MOVE PRTAXOWI          TO  RE-PRT-OW.
01355
01356       IF PRTCRSVL GREATER ZERO
01357           MOVE PRTCRSVI          TO  RE-PRT-CRSV.
01358
01359       IF RPTAL GREATER ZERO
01360           MOVE RPTAI             TO  RE-CEDING-STMT-OPT-A.
01361
01362       IF RPTBL GREATER ZERO
01363           MOVE RPTBI             TO  RE-CEDING-STMT-OPT-B.
01364
01365       IF RPTCL GREATER ZERO
01366           MOVE RPTCI             TO  RE-CEDING-STMT-OPT-C.
01367
01368       IF RPTDL GREATER ZERO
01369           MOVE RPTDI             TO  RE-CEDING-STMT-OPT-D.
01370
01371       IF RPTEL GREATER ZERO
01372           MOVE RPTEI             TO  RE-CEDING-STMT-OPT-E.
110601
110601      IF RPTFL GREATER ZERO
110601          MOVE RPTFI             TO  RE-STATE-EXHIBIT-OPT-F.
01373
01374       IF CLAIML GREATER ZERO
01375           MOVE CLAIMI            TO  RE-CLAIM-CODE.
01376
01377       IF TAXLIFEL GREATER ZERO
01378           MOVE TAXLIFEI          TO  RE-LF-TAX.
01379
01380       IF TAXAHL GREATER ZERO
01381           MOVE TAXAHI            TO  RE-AH-TAX.
01382
01383      IF LCLMPCTL GREATER THAN ZERO
01384         MOVE LCLMPCTI            TO  RE-LF-CLM-PCT.
01385
032707     IF EXTAXL > 0
032707        MOVE EXTAXI              TO  RE-EXCISE-TAX
032707     END-IF
01386      IF LCLMMAXL GREATER THAN ZERO
01387         MOVE WS-LF-CLM-MAX       TO  RE-LF-CLM-MAX.
01388
01389      IF ACLMPCTL GREATER THAN ZERO
01390         MOVE ACLMPCTI            TO  RE-AH-CLM-PCT.
01391
01392      IF ACLMMAXL GREATER THAN ZERO
01393         MOVE WS-AH-CLM-MAX       TO  RE-AH-CLM-MAX.
01394
01395      IF LFMETHL GREATER THAN ZERO
01396         MOVE LFMETHI             TO  RE-LF-FEE-METHOD.
01397
01398      IF LFBASISL GREATER THAN ZERO
01399         MOVE LFBASISI            TO  RE-LF-FEE-BASIS.
01400
01401      IF AHMETHL GREATER THAN ZERO
01402         MOVE AHMETHI             TO  RE-AH-FEE-METHOD.
01403
01404      IF AHBASISL GREATER THAN ZERO
01405         MOVE AHBASISI            TO  RE-AH-FEE-BASIS.
01406
01407      IF LFPCT1L GREATER THAN ZERO
01408         MOVE LFPCT1I             TO  RE-LF-FEE-RANGE-PCT (1).
01409
01410      IF LFPCT2L GREATER THAN ZERO
01411         MOVE LFPCT2I             TO  RE-LF-FEE-RANGE-PCT (2).
01412
01413      IF LFPCT3L GREATER THAN ZERO
01414         MOVE LFPCT3I             TO  RE-LF-FEE-RANGE-PCT (3).
01415
01416      IF LFPCT4L GREATER THAN ZERO
01417         MOVE LFPCT4I             TO  RE-LF-FEE-RANGE-PCT (4).
01418
01419      IF LFPCT5L GREATER THAN ZERO
01420         MOVE LFPCT5I             TO  RE-LF-FEE-RANGE-PCT (5).
01421
01422      IF LFPCT6L GREATER THAN ZERO
01423         MOVE LFPCT6I             TO  RE-LF-FEE-RANGE-PCT (6).
01424
01425      IF LFTHRU1L GREATER THAN ZERO
01426         MOVE WS-LF-THRU1         TO  RE-LF-FEE-THRU-AMT (1).
01427
01428      IF LFTHRU2L GREATER THAN ZERO
01429         MOVE WS-LF-THRU2         TO  RE-LF-FEE-THRU-AMT (2).
01430
01431      IF LFTHRU3L GREATER THAN ZERO
01432         MOVE WS-LF-THRU3         TO  RE-LF-FEE-THRU-AMT (3).
01433
01434      IF LFTHRU4L GREATER THAN ZERO
01435         MOVE WS-LF-THRU4         TO  RE-LF-FEE-THRU-AMT (4).
01436
01437      IF LFTHRU5L GREATER THAN ZERO
01438         MOVE WS-LF-THRU5         TO  RE-LF-FEE-THRU-AMT (5).
01439
01440      IF LFTHRU6L GREATER THAN ZERO
01441         MOVE WS-LF-THRU6         TO  RE-LF-FEE-THRU-AMT (6).
01442
01443      IF AHPCT1L GREATER THAN ZERO
01444         MOVE AHPCT1I             TO  RE-AH-FEE-RANGE-PCT (1).
01445
01446      IF AHPCT2L GREATER THAN ZERO
01447         MOVE AHPCT2I             TO  RE-AH-FEE-RANGE-PCT (2).
01448
01449      IF AHPCT3L GREATER THAN ZERO
01450         MOVE AHPCT3I             TO  RE-AH-FEE-RANGE-PCT (3).
01451
01452      IF AHPCT4L GREATER THAN ZERO
01453         MOVE AHPCT4I             TO  RE-AH-FEE-RANGE-PCT (4).
01454
01455      IF AHPCT5L GREATER THAN ZERO
01456         MOVE AHPCT5I             TO  RE-AH-FEE-RANGE-PCT (5).
01457
01458      IF AHPCT6L GREATER THAN ZERO
01459         MOVE AHPCT6I             TO  RE-AH-FEE-RANGE-PCT (6).
01460
01461      IF AHTHRU1L GREATER THAN ZERO
01462         MOVE WS-AH-THRU1         TO  RE-AH-FEE-THRU-AMT (1).
01463
01464      IF AHTHRU2L GREATER THAN ZERO
01465         MOVE WS-AH-THRU2         TO  RE-AH-FEE-THRU-AMT (2).
01466
01467      IF AHTHRU3L GREATER THAN ZERO
01468         MOVE WS-AH-THRU3         TO  RE-AH-FEE-THRU-AMT (3).
01469
01470      IF AHTHRU4L GREATER THAN ZERO
01471         MOVE WS-AH-THRU4         TO  RE-AH-FEE-THRU-AMT (4).
01472
01473      IF AHTHRU5L GREATER THAN ZERO
01474         MOVE WS-AH-THRU5         TO  RE-AH-FEE-THRU-AMT (5).
01475
01476      IF AHTHRU6L GREATER THAN ZERO
01477         MOVE WS-AH-THRU6         TO  RE-AH-FEE-THRU-AMT (6).
01478
01479      IF CLINCDTL GREATER ZERO
01480          MOVE WS-INC-DATE        TO  RE-CLM-INCURRED-LIM.
01481
01482      IF ERBEGDTL GREATER ZERO
01483          MOVE WS-ERN-START       TO  RE-EARNING-START-DT.
01484
01485      IF ERENDDTL GREATER ZERO
01486          MOVE WS-ERN-STOP        TO  RE-EARNING-STOP-DT.
01487
01488      IF ERENDCDL GREATER THAN ZERO
01489         MOVE ERENDCDI            TO  RE-EARN-STOP-CODE.
01490
01491      MOVE 'B'                    TO  RE-CODE.
01492
01493  6000-EXIT.
01494      EXIT.
01495      EJECT
01496  6200-CHECK-FOR-UPDATE.
01497      MOVE +0                     TO  SUB1.
01498      SET DO-INDX                 TO  +1.
01499      SET DO-INDX DOWN BY +1.
01500
01501  6225-CONT.
01502      SET DO-INDX UP BY +1.
01503      ADD +1                      TO  SUB1.
01504
01505      IF DO-INDX GREATER +18
01506          GO TO 6200-EXIT.
01507
01508      MOVE DESCO (DO-INDX)        TO  RE-DESC (SUB1).
01509
01510      GO TO 6225-CONT.
01511
01512  6200-EXIT.
01513      EXIT.
01514      EJECT
01515  6400-EDIT.
01516      IF PI-CARRIER-SECURITY GREATER SPACE
01517          MOVE PI-CARRIER-SECURITY    TO  BCARI
01518          MOVE AL-SANON               TO  BCARA.
01519
01520      IF REIGRPL GREATER THAN ZERO
01521          MOVE AL-UANON           TO  REIGRPA.
01522
01523      IF ADD-FUNCTION OR CHANGE-FUNCTION
01524          IF (COMPANYI EQUAL ZEROS) AND
01525                (COMPSUBI EQUAL ZEROS)
01526                  MOVE -1         TO  COMPANYL
01527                  MOVE AL-UABON   TO  COMPANYA
01528                                      COMPSUBA
01529                  MOVE ER-7805    TO  EMI-ERROR
01530                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01531
01532      IF BCARL GREATER ZERO
01533          MOVE BCARI              TO  WS-CARRIER
01534          PERFORM 7700-CHECK-CARRIER THRU 7700-EXIT
01535          IF CARRIER-FOUND
01536              MOVE AL-UANON       TO  BCARA
01537          ELSE
01538              IF BCARI = SPACES
01539                  MOVE AL-UANON   TO  BCARA
01540              ELSE
01541                  MOVE -1         TO  BCARL
01542                  MOVE AL-UABON   TO  BCARA
01543                  MOVE ER-2208    TO  EMI-ERROR
01544                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01545
01546      IF CONAMEL GREATER ZERO
01547          MOVE AL-UANON           TO  CONAMEA.
01548
01549      IF CESTYPEL GREATER ZERO
01550          MOVE CESTYPEI        TO  WS-CESSION-TYPE
01551          IF VALID-CESSION-TYPE
01552              MOVE AL-UANON    TO  CESTYPEA
01553          ELSE
01554              MOVE AL-UABON    TO  CESTYPEA
01555              MOVE -1          TO  CESTYPEL
01556              MOVE ER-0589     TO  EMI-ERROR
01557              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01558
01559      IF CMLIFEL GREATER ZERO
01560          MOVE CMLIFEI            TO  WS-CHECK-METHOD
01561          IF VALID-LIFE-METHOD
01562              MOVE AL-UANON       TO  CMLIFEA
01563          ELSE
01564              MOVE -1             TO  CMLIFEL
01565              MOVE AL-UABON       TO  CMLIFEA
01566              MOVE ER-2143        TO  EMI-ERROR
01567              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01568
01569      MOVE +1                     TO  CMAHL.
01570      IF CMAHL GREATER ZERO
01571          MOVE CMAHI              TO  WS-CHECK-METHOD
01572          IF VALID-AH-METHOD
01573              MOVE AL-UANON       TO  CMAHA
01574          ELSE
01575              MOVE -1             TO  CMAHL
01576              MOVE AL-UABON       TO  CMAHA
01577              MOVE ER-2144        TO  EMI-ERROR
01578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01579
01580      IF LIFEFEEL GREATER ZERO
01581          IF LIFEFEEI NUMERIC
01582              MOVE AL-UANON       TO  LIFEFEEA
01583          ELSE
01584              MOVE -1             TO  LIFEFEEL
01585              MOVE AL-UABON       TO  LIFEFEEA
01586              MOVE ER-2147        TO  EMI-ERROR
01587              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01588
01589      IF AHFEEL GREATER ZERO
01590          IF AHFEEI NUMERIC
01591              MOVE AL-UANON       TO  AHFEEA
01592          ELSE
01593              MOVE -1             TO  AHFEEL
01594              MOVE AL-UABON       TO  AHFEEA
01595              MOVE ER-2148        TO  EMI-ERROR
01596              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01597
01598      MOVE +1                     TO LFPRL  LFR78L
01599                                     AHPRL  AHR78L.
01600
01601      IF LFPRL GREATER ZERO
01602          IF LFPRI NUMERIC
01603              MOVE AL-UNNON       TO  LFPRA
01604              ADD LFPRI           TO  WS-TOT-PERCENT
01605          ELSE
01606              MOVE -1             TO  LFPRL
01607              MOVE AL-UABON       TO  LFPRA
01608              MOVE ER-2145        TO  EMI-ERROR
01609              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01610
01611      IF LFR78L GREATER ZERO
01612          IF LFR78I NUMERIC
01613              MOVE AL-UNNON       TO  LFR78A
01614              ADD LFR78I          TO  WS-TOT-PERCENT
01615          ELSE
01616              MOVE -1             TO  LFR78L
01617              MOVE AL-UABON       TO  LFR78A
01618              MOVE ER-2145        TO  EMI-ERROR
01619              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01620
01621      IF WS-TOT-PERCENT NOT = +0  AND
01622         WS-TOT-PERCENT NOT = +1
01623          MOVE -1                 TO  LFPRL
01624          MOVE AL-UNBON           TO  LFPRA
01625                                      LFR78A
01626          MOVE ER-2308            TO  EMI-ERROR
01627          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01628
01629      MOVE ZEROS                  TO WS-TOT-PERCENT.
01630
01631      IF AHPRL GREATER ZERO
01632          IF AHPRI NUMERIC
01633              MOVE AL-UNNON       TO  AHPRA
01634              ADD AHPRI           TO  WS-TOT-PERCENT
01635          ELSE
01636              MOVE -1             TO  AHPRL
01637              MOVE AL-UABON       TO  AHPRA
01638              MOVE ER-2146        TO  EMI-ERROR
01639              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01640
01641      IF AHR78L GREATER ZERO
01642          IF AHR78I NUMERIC
01643              MOVE AL-UNNON       TO  AHR78A
01644              ADD AHR78I          TO  WS-TOT-PERCENT
01645          ELSE
01646              MOVE -1             TO  AHR78L
01647              MOVE AL-UABON       TO  AHR78A
01648              MOVE ER-2146        TO  EMI-ERROR
01649              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01650
01651      IF WS-CHECK-METHOD = 'P'  OR  'E'
01652          IF WS-TOT-PERCENT NOT = +1
01653              MOVE -1             TO  AHPRL
01654              MOVE AL-UNBON       TO  AHPRA
01655                                      AHR78A
01656              MOVE ER-2308        TO  EMI-ERROR
01657              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01658
01659      IF PRTAXL GREATER ZERO
01660          MOVE PRTAXI             TO  WS-CHECK-TAX-OPTION
01661          IF VALID-TAX-OPTION
01662              MOVE AL-UANON       TO  PRTAXA
01663          ELSE
01664              MOVE -1             TO  PRTAXL
01665              MOVE AL-UABON       TO  PRTAXA
01666              MOVE ER-2301        TO  EMI-ERROR
01667              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01668
01669      IF PRTAXOWL GREATER ZERO
01670          MOVE PRTAXOWI           TO  WS-CHECK-PRINT-OPT
01671          IF VALID-PRINT-OPT
01672              MOVE AL-UANON       TO  PRTAXOWA
01673          ELSE
01674              MOVE -1             TO  PRTAXOWL
01675              MOVE AL-UABON       TO  PRTAXOWA
01676              MOVE ER-2306        TO  EMI-ERROR
01677              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01678
01679      IF PRTCRSVL GREATER ZERO
01680          MOVE PRTCRSVI           TO  WS-CHECK-PRINT-OPT
01681          IF VALID-PRINT-OPT
01682              MOVE AL-UANON       TO  PRTCRSVA
01683          ELSE
01684              MOVE -1             TO  PRTCRSVL
01685              MOVE AL-UABON       TO  PRTCRSVA
01686              MOVE ER-2306        TO  EMI-ERROR
01687              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01688
01689      IF RPTAL GREATER ZERO
01690          MOVE RPTAI              TO  WS-CHECK-RPT-OPTION
01691          IF VALID-REPORT-OPTION
01692              MOVE AL-UANON       TO  RPTAA
01693          ELSE
01694              MOVE -1             TO  RPTAL
01695              MOVE AL-UABON       TO  RPTAA
01696              MOVE ER-2391        TO  EMI-ERROR
01697              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01698
01699      IF RPTBL GREATER ZERO
01700          MOVE RPTBI              TO  WS-CHECK-RPT-OPTION
01701          IF VALID-REPORT-OPTION
01702              MOVE AL-UANON       TO  RPTBA
01703          ELSE
01704              MOVE -1             TO  RPTBL
01705              MOVE AL-UABON       TO  RPTBA
01706              MOVE ER-2391        TO  EMI-ERROR
01707              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01708
01709      IF RPTCL GREATER ZERO
01710          MOVE RPTCI              TO  WS-CHECK-RPT-OPTION
01711          IF VALID-REPORT-OPTION
01712              MOVE AL-UANON       TO  RPTCA
01713          ELSE
01714              MOVE -1             TO  RPTCL
01715              MOVE AL-UABON       TO  RPTCA
01716              MOVE ER-2391        TO  EMI-ERROR
01717              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01718
01719      IF RPTDL GREATER ZERO
01720          MOVE RPTDI              TO  WS-CHECK-RPT-OPTION
01721          IF VALID-REPORT-OPTION
01722              MOVE AL-UANON       TO  RPTDA
01723          ELSE
01724              MOVE -1             TO  RPTDL
01725              MOVE AL-UABON       TO  RPTDA
01726              MOVE ER-2391        TO  EMI-ERROR
01727              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01728
01729      IF RPTEL GREATER ZERO
01730          MOVE RPTEI              TO  WS-CHECK-RPT-OPTION
01731          IF VALID-REPORT-OPTION
01732              MOVE AL-UANON       TO  RPTEA
01733          ELSE
01734              MOVE -1             TO  RPTEL
01735              MOVE AL-UABON       TO  RPTEA
01736              MOVE ER-2391        TO  EMI-ERROR
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
110601             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01738
01739      IF MORTCDL GREATER ZERO
01740          PERFORM 7100-EDIT-MORTALITY THRU 7199-EXIT.
01741
01742      IF MORTSWL GREATER ZERO
01743          IF MORTSWI = 'Y' OR 'N' OR ' '
01744              MOVE AL-UANON       TO  MORTSWA
01745          ELSE
01746              MOVE -1             TO  MORTSWL
01747              MOVE AL-UABON       TO  MORTSWA
01748              MOVE ER-7098        TO  EMI-ERROR
01749              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01750
01751      IF LFIBNRL GREATER ZERO
01752          IF LFIBNRI NUMERIC
01753              MOVE AL-UNNON       TO  LFIBNRA
01754          ELSE
01755              MOVE -1             TO  LFIBNRL
01756              MOVE AL-UNBON       TO  LFIBNRA
01757              MOVE ER-2314        TO  EMI-ERROR
01758              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01759
01760      IF AHIBNRL GREATER ZERO
01761          IF AHIBNRI NUMERIC
01762              MOVE AL-UNNON       TO  AHIBNRA
01763          ELSE
01764              MOVE -1             TO  AHIBNRL
01765              MOVE AL-UNBON       TO  AHIBNRA
01766              MOVE ER-2315        TO  EMI-ERROR
01767              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01768
01769      IF CLAIML GREATER ZERO
01770          MOVE CLAIMI             TO  WS-CHECK-CLAIM
01771          IF VALID-CLAIM
01772              MOVE AL-UANON       TO  CLAIMA
01773          ELSE
01774              MOVE -1             TO  CLAIML
01775              MOVE AL-UABON       TO  CLAIMA
01776              MOVE ER-2307        TO  EMI-ERROR
01777              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01778
01779      IF FEELIFEL GREATER ZERO
01780          MOVE FEELIFEI           TO  WS-CHECK-ZERO-FEE
01781          IF VALID-ZERO-FEE
01782              MOVE AL-UANON       TO  FEELIFEA
01783          ELSE
01784              MOVE -1             TO  FEELIFEL
01785              MOVE AL-UABON       TO  FEELIFEA
01786              MOVE ER-2149        TO  EMI-ERROR
01787              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01788
01789      IF FEEAHL GREATER ZERO
01790          MOVE FEEAHI             TO  WS-CHECK-ZERO-FEE
01791          IF VALID-ZERO-FEE
01792              MOVE AL-UANON       TO  FEEAHA
01793          ELSE
01794              MOVE -1             TO  FEEAHL
01795              MOVE AL-UABON       TO  FEEAHA
01796              MOVE ER-2309        TO  EMI-ERROR
01797              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01798
01799      IF COMLIFEL GREATER ZERO
01800          MOVE COMLIFEI           TO  WS-CHECK-COMMISSION
01801          IF VALID-COMMISSION
01802              MOVE AL-UANON       TO  COMLIFEA
01803          ELSE
01804              MOVE -1             TO  COMLIFEL
01805              MOVE AL-UABON       TO  COMLIFEA
01806              MOVE ER-2304        TO  EMI-ERROR
01807              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01808
01809      IF COMAHL GREATER ZERO
01810          MOVE COMAHI             TO  WS-CHECK-COMMISSION
01811          IF VALID-COMMISSION
01812              MOVE AL-UANON       TO  COMAHA
01813          ELSE
01814              MOVE -1             TO  COMAHL
01815              MOVE AL-UABON       TO  COMAHA
01816              MOVE ER-2305        TO  EMI-ERROR
01817              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01818
01819      IF TAXLIFEL GREATER ZERO
01820          MOVE TAXLIFEI           TO  WS-CHECK-TAX
01821          IF VALID-TAX
01822              MOVE AL-UANON       TO  TAXLIFEA
01823          ELSE
01824              MOVE -1             TO  TAXLIFEL
01825              MOVE AL-UABON       TO  TAXLIFEA
01826              MOVE ER-2302        TO  EMI-ERROR
01827              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01828
01829      IF TAXAHL GREATER ZERO
01830          MOVE TAXAHI             TO  WS-CHECK-TAX
01831          IF VALID-TAX
01832              MOVE AL-UANON       TO  TAXAHA
01833          ELSE
01834              MOVE -1             TO  TAXAHL
01835              MOVE AL-UABON       TO  TAXAHA
01836              MOVE ER-2303        TO  EMI-ERROR
01837              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01838
01839      IF LCLMPCTL GREATER ZERO
01840          IF LCLMPCTI NUMERIC
01841              IF LCLMPCTI GREATER +1
01842                  MOVE -1         TO  LCLMPCTL
01843                  MOVE AL-UNBON   TO  LCLMPCTA
01844                  MOVE ER-0649    TO  EMI-ERROR
01845                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01846              ELSE
01847                  MOVE AL-UNNON   TO  LCLMPCTA
01848          ELSE
01849              MOVE -1             TO  LCLMPCTL
01850              MOVE AL-UNBON       TO  LCLMPCTA
01851              MOVE ER-0648        TO  EMI-ERROR
01852              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01853
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
01853
01854      IF PI-COMPANY-ID  =  'NCL' OR 'LGX'
01855         IF CUSTAMTL GREATER ZERO
01856             MOVE CUSTAMTI        TO  DEEDIT-FIELD
01857             PERFORM 7450-DEEDIT THRU 7450-EXIT
01858             IF DEEDIT-FIELD-V0 IS NOT GREATER +999999999
01859                 MOVE DEEDIT-FIELD-V1 TO  CUSTAMTO
01860                                          WS-CUSTODIAL-BAL
01861                 MOVE AL-UNNON        TO  CUSTAMTA
01862             ELSE
01863                 MOVE -1              TO  CUSTAMTL
01864                 MOVE AL-UNBON        TO  CUSTAMTA
01865                 MOVE ER-0763         TO  EMI-ERROR
01866                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01867
01868      IF LCLMMAXL GREATER ZERO
01869          MOVE LCLMMAXI           TO  DEEDIT-FIELD
01870          PERFORM 7450-DEEDIT THRU 7450-EXIT
01871          IF DEEDIT-FIELD-V0 NUMERIC
01872              MOVE DEEDIT-FIELD-V1    TO  LCLMMAXO
01873                                          WS-LF-CLM-MAX
01874              MOVE AL-UNNON           TO  LCLMMAXA
01875          ELSE
01876              MOVE -1             TO  LCLMMAXL
01877              MOVE AL-UNBON       TO  LCLMMAXA
01878              MOVE ER-0650        TO  EMI-ERROR
01879              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01880
01881      IF ACLMPCTL GREATER ZERO
01882          IF ACLMPCTI NUMERIC
01883              IF ACLMPCTI GREATER +1
01884                  MOVE -1         TO  ACLMPCTL
01885                  MOVE AL-UNBON   TO  ACLMPCTA
01886                  MOVE ER-0652    TO  EMI-ERROR
01887                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01888              ELSE
01889                  MOVE AL-UNNON   TO  ACLMPCTA
01890          ELSE
01891              MOVE -1             TO  ACLMPCTL
01892              MOVE AL-UNBON       TO  ACLMPCTA
01893              MOVE ER-0651        TO  EMI-ERROR
01894              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01895
01896      IF ACLMMAXL GREATER ZERO
01897          MOVE ACLMMAXI           TO  DEEDIT-FIELD
01898          PERFORM 7450-DEEDIT THRU 7450-EXIT
01899          IF DEEDIT-FIELD-V0 NUMERIC
01900              MOVE DEEDIT-FIELD-V1    TO  ACLMMAXO
01901                                          WS-AH-CLM-MAX
01902              MOVE AL-UNNON           TO  ACLMMAXA
01903          ELSE
01904              MOVE -1             TO  ACLMMAXL
01905               MOVE AL-UNBON      TO  ACLMMAXA
01906               MOVE ER-0653       TO  EMI-ERROR
01907               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01908
01909      IF CLINCDTL GREATER ZERO
01910          MOVE CLINCDTI           TO  DEEDIT-FIELD
01911          
      * EXEC CICS BIF
01912 *            DEEDIT
01913 *            FIELD  (DEEDIT-FIELD)
01914 *            LENGTH (15)
01915 *            END-EXEC
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005743' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01916          IF DEEDIT-FIELD-V0 GREATER ZERO
01917              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01918              MOVE '4'                TO  DC-OPTION-CODE
01919              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01920              IF NO-CONVERSION-ERROR
01921                  MOVE AL-UANON               TO  CLINCDTA
01922 **               MOVE DC-GREG-DATE-1-YMD     TO  WS-INC-DATE
LGC192                 MOVE DC-GREG-DATE-CYMD      TO  WS-INC-DATE
01923                  MOVE DC-GREG-DATE-1-MDY     TO  CLINCDTO
01924              ELSE
01925                  MOVE -1         TO  CLINCDTL
01926                  MOVE AL-UABON   TO  CLINCDTA
01927                  MOVE ER-2312    TO  EMI-ERROR
01928                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01929          ELSE
01930              MOVE ZEROS          TO  WS-INC-DATE
01931              MOVE AL-UANON       TO  CLINCDTA.
01932
01933      IF ERBEGDTL GREATER ZERO
01934          MOVE ERBEGDTI           TO  DEEDIT-FIELD
01935          
      * EXEC CICS BIF
01936 *            DEEDIT
01937 *            FIELD  (DEEDIT-FIELD)
01938 *            LENGTH (15)
01939 *            END-EXEC
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005768' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01940          IF DEEDIT-FIELD-V0 GREATER ZERO
01941              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01942              MOVE '4'                TO  DC-OPTION-CODE
01943              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01944              IF NO-CONVERSION-ERROR
01945                  MOVE AL-UANON               TO  ERBEGDTA
01946 **               MOVE DC-GREG-DATE-1-YMD     TO  WS-ERN-START
LGC192                 MOVE DC-GREG-DATE-CYMD      TO  WS-ERN-START
01947                  MOVE DC-GREG-DATE-1-MDY     TO  ERBEGDTO
01948              ELSE
01949                  MOVE -1         TO  ERBEGDTL
01950                  MOVE AL-UABON   TO  ERBEGDTA
01951                  MOVE ER-2312    TO  EMI-ERROR
01952                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01953          ELSE
01954              MOVE ZEROS          TO  WS-ERN-START
01955              MOVE AL-UANON       TO  ERBEGDTA.
01956
01957      IF ERENDDTL GREATER ZERO
01958          IF ERENDDTI = '999999'
01959              MOVE ZEROS          TO  ERENDDTI.
01960
01961      IF ERENDDTL GREATER ZERO
01962          MOVE ERENDDTI           TO  DEEDIT-FIELD
01963          
      * EXEC CICS BIF
01964 *            DEEDIT
01965 *            FIELD  (DEEDIT-FIELD)
01966 *            LENGTH (15)
01967 *            END-EXEC
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005797' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01968          IF DEEDIT-FIELD-V0 GREATER ZERO
01969              MOVE DEEDIT-FIELD-V0    TO  DC-GREG-DATE-1-MDY
01970              MOVE '4'                TO  DC-OPTION-CODE
01971              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
01972              IF NO-CONVERSION-ERROR
01973                  MOVE AL-UANON               TO  ERENDDTA
01974 **               MOVE DC-GREG-DATE-1-YMD     TO  WS-ERN-STOP
LGC192                 MOVE DC-GREG-DATE-CYMD      TO  WS-ERN-STOP
01975                  MOVE DC-GREG-DATE-1-MDY     TO  ERENDDTO
01976              ELSE
01977                  MOVE -1         TO  ERENDDTL
01978                  MOVE AL-UABON   TO  ERENDDTA
01979                  MOVE ER-2313    TO  EMI-ERROR
01980                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01981          ELSE
01982              MOVE ZEROS          TO  WS-ERN-STOP
01983              MOVE AL-UANON       TO  ERENDDTA.
01984
01985      IF ERENDCDL GREATER ZERO
01986          IF ERENDCDI = 'A' OR 'B' OR 'L' OR ' '
01987              MOVE AL-UANON       TO  ERENDCDA
01988          ELSE
01990              MOVE AL-UABON       TO  ERENDCDA
01991              MOVE ER-7349        TO  EMI-ERROR
01992              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01993
01994      IF LFMETHL GREATER THAN ZERO
01995          MOVE LFMETHI            TO  WS-FEE-METHOD
01996          IF VALID-FEE-METHOD
01997              MOVE AL-UANON       TO  LFMETHA
01998          ELSE
01999              MOVE -1             TO  LFMETHL
02000              MOVE AL-UABON       TO  LFMETHA
02001              MOVE ER-7350        TO  EMI-ERROR
02002              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02003
02004      IF LFBASISL GREATER THAN ZERO
02005          MOVE LFBASISI           TO  WS-FEE-BASIS
02006          IF VALID-FEE-BASIS
02007              MOVE AL-UANON       TO  LFBASISA
02008          ELSE
02009              MOVE -1             TO  LFBASISL
02010              MOVE AL-UABON       TO  LFBASISA
02011              MOVE ER-7351        TO  EMI-ERROR
02012              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02013
02014      IF AHMETHL GREATER THAN ZERO
02015          MOVE AHMETHI            TO  WS-FEE-METHOD
02016          IF VALID-FEE-METHOD
02017              MOVE AL-UANON       TO  AHMETHA
02018          ELSE
02019              MOVE -1             TO  AHMETHL
02020              MOVE AL-UABON       TO  AHMETHA
02021              MOVE ER-7350        TO  EMI-ERROR
02022              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02023
02024      IF AHBASISL GREATER THAN ZERO
02025          MOVE AHBASISI           TO  WS-FEE-BASIS
02026          IF VALID-FEE-BASIS
02027              MOVE AL-UANON       TO  AHBASISA
02028          ELSE
02029              MOVE -1             TO  AHBASISL
02030              MOVE AL-UABON       TO  AHBASISA
02031              MOVE ER-7351        TO  EMI-ERROR
02032              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02033
02034      IF LFPCT1L GREATER ZERO
02035          IF LFPCT1I NUMERIC
02036              MOVE AL-UNNON       TO  LFPCT1A
02037          ELSE
02038              MOVE -1             TO  LFPCT1L
02039              MOVE AL-UNBON       TO  LFPCT1A
02040              MOVE ER-2315        TO  EMI-ERROR
02041              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02042
02043      IF LFPCT2L GREATER ZERO
02044          IF LFPCT2I NUMERIC
02045              MOVE AL-UNNON       TO  LFPCT2A
02046          ELSE
02047              MOVE -1             TO  LFPCT2L
02048              MOVE AL-UNBON       TO  LFPCT2A
02049              MOVE ER-2315        TO  EMI-ERROR
02050              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02051
02052      IF LFPCT3L GREATER ZERO
02053          IF LFPCT3I NUMERIC
02054              MOVE AL-UNNON       TO  LFPCT3A
02055          ELSE
02056              MOVE -1             TO  LFPCT3L
02057              MOVE AL-UNBON       TO  LFPCT3A
02058              MOVE ER-2315        TO  EMI-ERROR
02059              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02060
02061      IF LFPCT4L GREATER ZERO
02062          IF LFPCT4I NUMERIC
02063              MOVE AL-UNNON       TO  LFPCT4A
02064          ELSE
02065              MOVE -1             TO  LFPCT4L
02066              MOVE AL-UNBON       TO  LFPCT4A
02067              MOVE ER-2315        TO  EMI-ERROR
02068              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02069
02070      IF LFPCT5L GREATER ZERO
02071          IF LFPCT5I NUMERIC
02072              MOVE AL-UNNON       TO  LFPCT5A
02073          ELSE
02074              MOVE -1             TO  LFPCT5L
02075              MOVE AL-UNBON       TO  LFPCT5A
02076              MOVE ER-2315        TO  EMI-ERROR
02077              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02078
02079      IF LFPCT6L GREATER ZERO
02080          IF LFPCT6I NUMERIC
02081              MOVE AL-UNNON       TO  LFPCT6A
02082          ELSE
02083              MOVE -1             TO  LFPCT6L
02084              MOVE AL-UNBON       TO  LFPCT6A
02085              MOVE ER-2315        TO  EMI-ERROR
02086              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02087
02088      IF LFTHRU1L GREATER ZERO
02089          MOVE LFTHRU1I           TO  DEEDIT-FIELD
02090          PERFORM 7450-DEEDIT THRU 7450-EXIT
02091          IF DEEDIT-FIELD-V0 NUMERIC
02092              MOVE DEEDIT-FIELD-V1    TO  LFTHRU1O
02093                                          WS-LF-THRU1
02094              MOVE AL-UNNON           TO  LFTHRU1A
02095          ELSE
02096              MOVE -1             TO  LFTHRU1L
02097              MOVE AL-UNBON       TO  LFTHRU1A
02098              MOVE ER-2355        TO  EMI-ERROR
02099              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02100
02101      IF LFTHRU2L GREATER ZERO
02102          MOVE LFTHRU2I           TO  DEEDIT-FIELD
02103          PERFORM 7450-DEEDIT THRU 7450-EXIT
02104          IF DEEDIT-FIELD-V0 NUMERIC
02105              MOVE DEEDIT-FIELD-V1    TO  LFTHRU2O
02106                                          WS-LF-THRU2
02107              MOVE AL-UNNON           TO  LFTHRU2A
02108          ELSE
02109              MOVE -1             TO  LFTHRU2L
02110              MOVE AL-UNBON       TO  LFTHRU2A
02111              MOVE ER-2355        TO  EMI-ERROR
02112              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02113
02114      IF LFTHRU3L GREATER ZERO
02115          MOVE LFTHRU3I           TO  DEEDIT-FIELD
02116          PERFORM 7450-DEEDIT THRU 7450-EXIT
02117          IF DEEDIT-FIELD-V0 NUMERIC
02118              MOVE DEEDIT-FIELD-V1    TO  LFTHRU3O
02119                                          WS-LF-THRU3
02120              MOVE AL-UNNON           TO  LFTHRU3A
02121          ELSE
02122              MOVE -1             TO  LFTHRU3L
02123              MOVE AL-UNBON       TO  LFTHRU3A
02124              MOVE ER-2355        TO  EMI-ERROR
02125              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02126
02127      IF LFTHRU4L GREATER ZERO
02128          MOVE LFTHRU4I           TO  DEEDIT-FIELD
02129          PERFORM 7450-DEEDIT THRU 7450-EXIT
02130          IF DEEDIT-FIELD-V0 NUMERIC
02131              MOVE DEEDIT-FIELD-V1    TO  LFTHRU4O
02132                                          WS-LF-THRU4
02133              MOVE AL-UNNON           TO  LFTHRU4A
02134          ELSE
02135              MOVE -1             TO  LFTHRU4L
02136              MOVE AL-UNBON       TO  LFTHRU4A
02137              MOVE ER-2355        TO  EMI-ERROR
02138              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02139
02140      IF LFTHRU5L GREATER ZERO
02141          MOVE LFTHRU5I           TO  DEEDIT-FIELD
02142          PERFORM 7450-DEEDIT THRU 7450-EXIT
02143          IF DEEDIT-FIELD-V0 NUMERIC
02144              MOVE DEEDIT-FIELD-V1    TO  LFTHRU5O
02145                                          WS-LF-THRU5
02146              MOVE AL-UNNON           TO  LFTHRU5A
02147          ELSE
02148              MOVE -1             TO  LFTHRU5L
02149              MOVE AL-UNBON       TO  LFTHRU5A
02150              MOVE ER-2355        TO  EMI-ERROR
02151              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02152
02153      IF LFTHRU6L GREATER ZERO
02154          MOVE LFTHRU6I           TO  DEEDIT-FIELD
02155          PERFORM 7450-DEEDIT THRU 7450-EXIT
02156          IF DEEDIT-FIELD-V0 NUMERIC
02157              MOVE DEEDIT-FIELD-V1    TO  LFTHRU6O
02158                                          WS-LF-THRU6
02159              MOVE AL-UNNON           TO  LFTHRU6A
02160          ELSE
02161              MOVE -1             TO  LFTHRU6L
02162              MOVE AL-UNBON       TO  LFTHRU6A
02163              MOVE ER-2355        TO  EMI-ERROR
02164              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02165
02166      IF AHPCT1L GREATER ZERO
02167          IF AHPCT1I NUMERIC
02168              MOVE AL-UNNON       TO  AHPCT1A
02169          ELSE
02170              MOVE -1             TO  AHPCT1L
02171              MOVE AL-UNBON       TO  AHPCT1A
02172              MOVE ER-2315        TO  EMI-ERROR
02173              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02174
02175      IF AHPCT2L GREATER ZERO
02176          IF AHPCT2I NUMERIC
02177              MOVE AL-UNNON       TO  AHPCT2A
02178          ELSE
02179              MOVE -1             TO  AHPCT2L
02180              MOVE AL-UNBON       TO  AHPCT2A
02181              MOVE ER-2315        TO  EMI-ERROR
02182              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02183
02184      IF AHPCT3L GREATER ZERO
02185          IF AHPCT3I NUMERIC
02186              MOVE AL-UNNON       TO  AHPCT3A
02187          ELSE
02188              MOVE -1             TO  AHPCT3L
02189              MOVE AL-UNBON       TO  AHPCT3A
02190              MOVE ER-2315        TO  EMI-ERROR
02191              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02192
02193      IF AHPCT4L GREATER ZERO
02194          IF AHPCT4I NUMERIC
02195              MOVE AL-UNNON       TO  AHPCT4A
02196           ELSE
02197              MOVE -1             TO  AHPCT4L
02198              MOVE AL-UNBON       TO  AHPCT4A
02199              MOVE ER-2315        TO  EMI-ERROR
02200              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02201
02202      IF AHPCT5L GREATER ZERO
02203          IF AHPCT5I NUMERIC
02204              MOVE AL-UNNON       TO  AHPCT5A
02205          ELSE
02206              MOVE -1             TO  AHPCT5L
02207              MOVE AL-UNBON       TO  AHPCT5A
02208              MOVE ER-2315        TO  EMI-ERROR
02209              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02210
02211      IF AHPCT6L GREATER ZERO
02212          IF AHPCT6I NUMERIC
02213              MOVE AL-UNNON       TO  AHPCT6A
02214          ELSE
02215              MOVE -1             TO  AHPCT6L
02216              MOVE AL-UNBON       TO  AHPCT6A
02217              MOVE ER-2315        TO  EMI-ERROR
02218              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02219
02220      IF AHTHRU1L GREATER ZERO
02221          MOVE AHTHRU1I           TO  DEEDIT-FIELD
02222          PERFORM 7450-DEEDIT THRU 7450-EXIT
02223          IF DEEDIT-FIELD-V0 NUMERIC
02224              MOVE DEEDIT-FIELD-V1    TO  AHTHRU1O
02225                                          WS-AH-THRU1
02226              MOVE AL-UNNON           TO  AHTHRU1A
02227          ELSE
02228              MOVE -1             TO  AHTHRU1L
02229              MOVE AL-UNBON       TO  AHTHRU1A
02230              MOVE ER-2355        TO  EMI-ERROR
02231              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02232
02233      IF AHTHRU2L GREATER ZERO
02234          MOVE AHTHRU2I           TO  DEEDIT-FIELD
02235          PERFORM 7450-DEEDIT THRU 7450-EXIT
02236          IF DEEDIT-FIELD-V0 NUMERIC
02237              MOVE DEEDIT-FIELD-V1    TO  AHTHRU2O
02238                                          WS-AH-THRU2
02239              MOVE AL-UNNON           TO  AHTHRU2A
02240          ELSE
02241              MOVE -1             TO  AHTHRU2L
02242              MOVE AL-UNBON       TO  AHTHRU2A
02243              MOVE ER-2355        TO  EMI-ERROR
02244              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02245
02246      IF AHTHRU3L GREATER ZERO
02247          MOVE AHTHRU3I           TO  DEEDIT-FIELD
02248          PERFORM 7450-DEEDIT THRU 7450-EXIT
02249          IF DEEDIT-FIELD-V0 NUMERIC
02250              MOVE DEEDIT-FIELD-V1    TO  AHTHRU3O
02251                                          WS-AH-THRU3
02252              MOVE AL-UNNON           TO  AHTHRU3A
02253          ELSE
02254              MOVE -1             TO  AHTHRU3L
02255              MOVE AL-UNBON       TO  AHTHRU3A
02256              MOVE ER-2355        TO  EMI-ERROR
02257              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02258
02259      IF AHTHRU4L GREATER ZERO
02260          MOVE AHTHRU4I           TO  DEEDIT-FIELD
02261          PERFORM 7450-DEEDIT THRU 7450-EXIT
02262          IF DEEDIT-FIELD-V0 NUMERIC
02263              MOVE DEEDIT-FIELD-V1    TO  AHTHRU4O
02264                                          WS-AH-THRU4
02265              MOVE AL-UNNON           TO  AHTHRU4A
02266          ELSE
02267              MOVE -1             TO  AHTHRU4L
02268              MOVE AL-UNBON       TO  AHTHRU4A
02269              MOVE ER-2355        TO  EMI-ERROR
02270              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02271
02272      IF AHTHRU5L GREATER ZERO
02273          MOVE AHTHRU5I           TO  DEEDIT-FIELD
02274          PERFORM 7450-DEEDIT THRU 7450-EXIT
02275          IF DEEDIT-FIELD-V0 NUMERIC
02276              MOVE DEEDIT-FIELD-V1    TO  AHTHRU5O
02277                                          WS-AH-THRU5
02278              MOVE AL-UNNON           TO  AHTHRU5A
02279          ELSE
02280              MOVE -1             TO  AHTHRU5L
02281              MOVE AL-UNBON       TO  AHTHRU5A
02282              MOVE ER-2355        TO  EMI-ERROR
02283              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02284
02285      IF AHTHRU6L GREATER ZERO
02286          MOVE AHTHRU6I           TO  DEEDIT-FIELD
02287          PERFORM 7450-DEEDIT THRU 7450-EXIT
02288          IF DEEDIT-FIELD-V0 NUMERIC
02289              MOVE DEEDIT-FIELD-V1    TO  AHTHRU6O
02290                                          WS-AH-THRU6
02291              MOVE AL-UNNON           TO  AHTHRU6A
02292          ELSE
02293              MOVE -1             TO  AHTHRU6L
02294              MOVE AL-UNBON       TO  AHTHRU6A
02295              MOVE ER-2355        TO  EMI-ERROR
02296              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02297
02298      IF CEDNAMEL GREATER ZERO
02299          MOVE AL-UANON           TO  CEDNAMEA.
02300
02301  6400-EXIT.
02302      EXIT.
02303      EJECT
02304  7100-EDIT-MORTALITY.
02305      IF  MORTCDI = SPACES
02306          GO TO 7199-EXIT.
02307
02308      MOVE SPACES                 TO  ELCNTL-KEY.
02309      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02310      MOVE '7'                    TO  CNTL-REC-TYPE.
02311      MOVE +0                     TO  CNTL-SEQ-NO.
02312
02313      
      * EXEC CICS HANDLE CONDITION
02314 *        NOTFND  (7190-NOT-FOUND)
02315 *        ENDFILE (7192-END-OF-FILE)
02316 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00006147' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303036313437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02317
02318  7109-READ-NEXT.
02319
02320      
      * EXEC CICS READ
02321 *         DATASET  (CNTL-FILE-ID)
02322 *         SET      (ADDRESS OF CONTROL-FILE)
02323 *         RIDFLD   (ELCNTL-KEY)
02324 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006154' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02325
02326      CONTINUE.
02327
02328      IF  PI-COMPANY-ID NOT EQUAL TO CF-COMPANY-ID
02329              OR
02330          CF-RECORD-TYPE NOT EQUAL '7'
02331          GO TO 7190-NOT-FOUND
02332
02333      ELSE
02334          MOVE +1                 TO  SUB2
02335          GO TO 7110-SEARCH-MORTAL-TABLE.
02336
02337  7110-SEARCH-MORTAL-TABLE.
02338
02339      IF  CF-MORT-TABLE-CODE (SUB2) = MORTCDI
02340          GO TO 7199-EXIT.
02341
02342      IF  CF-MORT-TABLE-CODE (SUB2) = LOW-VALUES
02343              OR
02344          CF-MORT-TABLE-CODE (SUB2) GREATER THAN MORTCDI
02345          GO TO 7190-NOT-FOUND.
02346
02347      ADD +1                      TO  SUB2.
02348
02349      IF  SUB2 GREATER +9
02350          ADD +1                  TO  CNTL-SEQ-NO
02351          GO TO 7109-READ-NEXT
02352
02353      ELSE
02354          GO TO 7110-SEARCH-MORTAL-TABLE.
02355
02356  7190-NOT-FOUND.
02357
02358      MOVE -1                     TO  MORTCDL.
02359      MOVE AL-UABON               TO  MORTCDA.
02360      MOVE ER-2103                TO  EMI-ERROR
02361      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02362      GO TO 7199-EXIT.
02363
02364  7192-END-OF-FILE.
02365
02366      MOVE -1                     TO  MORTCDL.
02367      MOVE AL-UABON               TO  MORTCDA.
02368      MOVE ER-2237                TO  EMI-ERROR
02369      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02370      GO TO 7199-EXIT.
02371
02372  7199-EXIT.
02373      EXIT.
02374      EJECT
02375  7200-READ-ERREIN.
02376      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.
02377      MOVE 'B'                    TO  PI-CRR-CODE.
02378
02379      
      * EXEC CICS READ
02380 *         DATASET  (REIN-FILE-ID)
02381 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02382 *         RIDFLD   (PI-ERREIN-KEY)
02383 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006213' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02384
02385      CONTINUE.
02386
02387      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02388      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02389
02390      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02391
02392  7200-EXIT.
02393      EXIT.
02394      EJECT
02395  7300-ERREIN-GETMAIN.
02396      
      * EXEC CICS GETMAIN
02397 *         SET     (ADDRESS OF REINSURANCE-RECORD)
02398 *         LENGTH  (ERREIN-LENGTH)
02399 *         INITIMG (GETMAIN-SPACE)
02400 *    END-EXEC.
      *    MOVE ',"IL                  $   #00006230' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERREIN-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02401
02402      CONTINUE.
02403
02404  7300-EXIT.
02405      EXIT.
02406      EJECT
02407  7400-READ-ERREIN-UPDATE.
02408      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.
02409      MOVE 'B'                    TO  PI-CRR-CODE.
02410
02411      
      * EXEC CICS READ
02412 *         DATASET  (REIN-FILE-ID)
02413 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02414 *         RIDFLD   (PI-ERREIN-KEY)
02415 *         UPDATE
02416 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006245' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02417
02418      CONTINUE.
02419
02420  7400-EXIT.
02421      EXIT.
02422      EJECT
02423  7450-DEEDIT.
02424      
      * EXEC CICS BIF
02425 *         DEEDIT
02426 *         FIELD  (DEEDIT-FIELD)
02427 *         LENGTH (15)
02428 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00006258' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02429
02430  7450-EXIT.
02431      EXIT.
02432      EJECT
02433  7500-PAGE-FORWARD.
02434      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.
02435
02436      IF PI-MAP-C
02437          GO TO 7510-MAP-C.
02438
02439      IF COMPANYL GREATER ZERO
02440          MOVE ZERO TO TALLY
02441          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES
02442          IF TALLY GREATER ZERO
02443              MOVE -1             TO  COMPANYL
02444              MOVE AL-UABON       TO  COMPANYA
02445              MOVE ER-2340        TO  EMI-ERROR
02446              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02447          ELSE
02448              MOVE SPACE          TO  PI-FIRST-TIME-SW
02449              MOVE AL-UANON       TO  COMPANYA
02450              MOVE COMPANYI       TO  PI-CRR-TABLE
02451      ELSE
02452          MOVE 'Y'                TO  PI-FIRST-TIME-SW
02453          MOVE LOW-VALUES         TO  PI-ERREIN-KEY.
02454
02455      IF PI-FIRST-TIME-SW = 'Y'
02456          NEXT SENTENCE
02457      ELSE
02458          IF COMPSUBL IS GREATER THAN 0
02459              MOVE AL-SANON       TO  COMPSUBA
02460              MOVE COMPSUBI       TO  PI-CRR-TABLE-SUB
02461          ELSE
02462              MOVE ZEROS          TO  PI-CRR-TABLE-SUB.
02463
02464      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.
02465
02466      GO TO 7520-START.
02467
02468  7510-MAP-C.
02469      IF COMPL GREATER ZERO
02470          MOVE AL-SANON           TO  COMPA
02471          MOVE COMPI              TO  PI-CRR-TABLE
02472          MOVE SPACE              TO  PI-FIRST-TIME-SW
02473      ELSE
02474          MOVE 'Y'                TO  PI-FIRST-TIME-SW
02475          MOVE LOW-VALUES         TO  PI-ERREIN-KEY.
02476
02477      IF PI-FIRST-TIME-SW  = 'Y'
02478          NEXT SENTENCE
02479      ELSE
02480          IF SUBL IS GREATER THAN 0
02481              MOVE AL-SANON       TO  SUBA
02482              MOVE SUBI           TO  PI-CRR-TABLE-SUB
02483          ELSE
02484              MOVE ZEROS          TO  PI-CRR-TABLE-SUB.
02485
02486      MOVE PI-ERREIN-KEY          TO  WS-SAVE-KEY.
02487
02488  7520-START.
02489      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.
02490      MOVE 'B'                    TO  PI-CRR-CODE.
02491
02492      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
02493
02494      
      * EXEC CICS HANDLE CONDITION
02495 *        ENDFILE  (7550-ENDFILE)
02496 *        NOTFND   (7575-NOTFOUND)
02497 *    END-EXEC.
      *    MOVE '"$''I                  ! % #00006328' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02498
02499  7530-READ-NEXT.
02500      PERFORM 7850-READNEXT THRU 7850-EXIT.
02501
02502      IF ERREIN-EOF
02503          GO TO 7550-ENDFILE.
02504
02505      IF PI-ERREIN-KEY = WS-SAVE-KEY
02506          GO TO 7530-READ-NEXT.
02507
02508      IF PI-CARRIER-SECURITY GREATER SPACE
02509          IF PI-CARRIER-SECURITY = RE-COMP-CARRIER-SECURITY
02510              NEXT SENTENCE
02511          ELSE
02512              GO TO 7530-READ-NEXT.
02513
02514      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02515      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02516      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02517
02518      IF PI-MAP-C
02519          MOVE LOW-VALUES         TO  EL6511CO
02520          GO TO 5550-SET-UP-SCREEN
02521      ELSE
02522          MOVE LOW-VALUES         TO  EL6511BO
02523          GO TO 5050-SET-UP-SCREEN.
02524
02525  7540-CHECK-MAP.
02526      IF FIRST-TIME
02527          IF PI-MAP-C
02528              MOVE LOW-VALUES     TO  EL6511CO
02529              MOVE -1             TO  DESC1L
02530              GO TO 8110-SEND-INITIAL-MAP
02531          ELSE
02532              MOVE LOW-VALUES     TO  EL6511BO
02533              MOVE -1             TO  COMPANYL
02534              GO TO 8100-SEND-INITIAL-MAP
02535      ELSE
02536          IF PI-MAP-C
02537              MOVE LOW-VALUES     TO  EL6511CO
02538          ELSE
02539              MOVE LOW-VALUES     TO  EL6511BO.
02540
02541      GO TO 7500-PAGE-FORWARD.
02542
02543  7550-ENDFILE.
02544      IF BROWSE-STARTED
02545          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02546
02547      IF FIRST-TIME
02548          MOVE ER-2386            TO  EMI-ERROR
02549      ELSE
02550          MOVE ER-2067            TO  EMI-ERROR.
02551
02552      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02553      GO TO 7540-CHECK-MAP.
02554
02555  7575-NOTFOUND.
02556      IF BROWSE-STARTED
02557          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02558
02559      GO TO 8880-NOT-FOUND.
02560
02561  7599-EXIT.
02562      EXIT.
02563      EJECT
02564
02565  7600-PAGE-BACKWARD.
02566      MOVE SPACES                 TO  PI-ERREIN-EOF-SW.
02567
02568      IF PI-MAP-C
02569          GO TO 7610-MAP-C.
02570
02571      IF COMPANYL GREATER ZERO
02572          MOVE ZERO TO TALLY
02573          INSPECT COMPANYI TALLYING TALLY FOR ALL SPACES
02574          IF TALLY GREATER ZERO
02575              MOVE -1             TO  COMPANYL
02576              MOVE AL-UABON       TO  COMPANYA
02577              MOVE ER-2340        TO  EMI-ERROR
02578              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02579          ELSE
02580              MOVE AL-UANON       TO  COMPANYA
02581              MOVE COMPANYI       TO  PI-CRR-TABLE
02582      ELSE
02583          GO TO 7675-NOTFOUND.
02584
02585      IF COMPSUBL IS GREATER THAN 0
02586          MOVE AL-UANON           TO  COMPSUBA
02587          MOVE COMPSUBI           TO  PI-CRR-TABLE-SUB
02588      ELSE
02589          MOVE ZEROS              TO  PI-CRR-TABLE-SUB.
02590
02591      GO TO 7620-START.
02592
02593  7610-MAP-C.
02594      IF COMPL GREATER ZERO
02595          MOVE AL-SANON           TO  COMPA
02596          MOVE COMPI              TO  PI-CRR-TABLE
02597      ELSE
02598          GO TO 7675-NOTFOUND.
02599
02600      IF SUBL IS GREATER THAN 0
02601          MOVE AL-SANON           TO  SUBA
02602          MOVE SUBI               TO  PI-CRR-TABLE-SUB
02603      ELSE
02604          MOVE ZEROS              TO  PI-CRR-TABLE-SUB.
02605
02606  7620-START.
02607      MOVE 'B'                    TO  PI-CRR-CODE.
02608      MOVE PI-COMPANY-CD          TO  PI-CRR-COMPANY-CD.
02609
02610      PERFORM 7800-START-BROWSE THRU 7800-EXIT.
02611
02612      
      * EXEC CICS HANDLE CONDITION
02613 *        ENDFILE  (7650-ENDFILE)
02614 *        NOTFND   (7675-NOTFOUND)
02615 *     END-EXEC.
      *    MOVE '"$''I                  ! & #00006446' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02616
02617      PERFORM 7850-READNEXT THRU 7850-EXIT.
02618
02619      IF ERREIN-EOF
02620          IF BROWSE-STARTED
02621              PERFORM 7950-END-BROWSE THRU 7950-EXIT
02622              GO TO 7630-CHECK-MAP.
02623
02624  7625-READ-PREV.
02625      PERFORM 7900-READPREV THRU 7900-EXIT.
02626      PERFORM 7900-READPREV THRU 7900-EXIT.
02627
02628      IF ERREIN-EOF
02629          GO TO 7650-ENDFILE.
02630
02631      IF PI-ERREIN-KEY = WS-SAVE-KEY
02632          GO TO 7625-READ-PREV.
02633
02634      IF PI-CARRIER-SECURITY GREATER SPACE
02635         IF PI-CARRIER-SECURITY NOT = RE-COMP-CARRIER-SECURITY
02636              GO TO 7625-READ-PREV.
02637
02638      MOVE RE-LAST-MAINT-USER     TO  PI-UPDATE-BY.
02639      MOVE RE-LAST-MAINT-HHMMSS   TO  PI-UPDATE-HHMMSS.
02640      MOVE PI-ERREIN-KEY          TO  PI-SAVE-ERREIN-KEY.
02641
02642      IF PI-MAP-C
02643          MOVE RE-COMP-PRIME      TO  COMPO
02644          MOVE RE-COMP-SUB        TO  SUBO
02645          MOVE AL-SANON           TO  COMPA
02646                                      SUBA
02647          GO TO 5550-SET-UP-SCREEN
02648      ELSE
02649          MOVE RE-COMP-PRIME      TO  COMPANYO
02650          MOVE RE-COMP-SUB        TO  COMPSUBO
02651          MOVE AL-UANON           TO  COMPANYA
02652                                      COMPSUBA
02653          GO TO 5050-SET-UP-SCREEN.
02654
02655  7630-CHECK-MAP.
02656      IF PI-MAP-C
02657          MOVE LOW-VALUES         TO  EL6511CO
02658      ELSE
02659          MOVE LOW-VALUES         TO  EL6511BO.
02660
02661      GO TO 7500-PAGE-FORWARD.
02662
02663  7650-ENDFILE.
02664      IF BROWSE-STARTED
02665          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02666
02667      MOVE ER-2067                TO  EMI-ERROR
02668      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02669      GO TO 7630-CHECK-MAP.
02670
02671  7675-NOTFOUND.
02672      IF BROWSE-STARTED
02673          PERFORM 7950-END-BROWSE THRU 7950-EXIT.
02674
02675      GO TO 8880-NOT-FOUND.
02676
02677  7699-EXIT.
02678      EXIT.
02679      EJECT
02680
02681  7700-CHECK-CARRIER.
02682      MOVE SPACES                 TO  WS-CARRIER-FOUND-SW
02683                                      ELCNTL-KEY.
02684      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02685      MOVE '6'                    TO  CNTL-REC-TYPE.
02686      MOVE CARRIER-ACCESS         TO  CNTL-ACCESS.
02687      MOVE +0                     TO  CNTL-SEQ-NO.
02688
02689      
      * EXEC CICS HANDLE CONDITION
02690 *        NOTFND   (7700-EXIT)
02691 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00006523' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036353233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02692
02693      
      * EXEC CICS READ
02694 *        DATASET   (CNTL-FILE-ID)
02695 *        SET       (ADDRESS OF CONTROL-FILE)
02696 *        RIDFLD    (ELCNTL-KEY)
02697 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006527' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02698
02699      MOVE 'Y'                    TO  WS-CARRIER-FOUND-SW.
02700
02701  7700-EXIT.
02702      EXIT.
02703      EJECT
02704  7800-START-BROWSE.
02705      
      * EXEC CICS STARTBR
02706 *         DATASET  (REIN-FILE-ID)
02707 *         RIDFLD   (PI-ERREIN-KEY)
02708 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006539' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02709
02710      MOVE 'Y'                    TO  PI-BROWSE-SW.
02711
02712  7800-EXIT.
02713      EXIT.
02714      EJECT
02715
02716  7850-READNEXT.
02717      
      * EXEC CICS READNEXT
02718 *         DATASET  (REIN-FILE-ID)
02719 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02720 *         RIDFLD   (PI-ERREIN-KEY)
02721 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006551' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02722
02723      CONTINUE.
02724
02725      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR
02726         PI-CRR-CODE NOT = 'B'
02727          MOVE 'Y'                TO  PI-ERREIN-EOF-SW.
02728
02729  7850-EXIT.
02730      EXIT.
02731      EJECT
02732  7900-READPREV.
02733      
      * EXEC CICS READPREV
02734 *         DATASET  (REIN-FILE-ID)
02735 *         SET      (ADDRESS OF REINSURANCE-RECORD)
02736 *         RIDFLD   (PI-ERREIN-KEY)
02737 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006567' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 PI-ERREIN-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF REINSURANCE-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02738
02739      CONTINUE.
02740
02741      IF PI-COMPANY-CD NOT = RE-COMPANY-CD OR
02742         PI-CRR-CODE NOT = 'B'
02743          MOVE 'Y'                TO  PI-ERREIN-EOF-SW.
02744
02745  7900-EXIT.
02746      EXIT.
02747      EJECT
02748  7950-END-BROWSE.
02749      
      * EXEC CICS ENDBR
02750 *         DATASET  (REIN-FILE-ID)
02751 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006583' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036353833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REIN-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02752
02753      MOVE SPACE                  TO  PI-BROWSE-SW.
02754
02755  7950-EXIT.
02756      EXIT.
02757      EJECT
02758  8000-UPDATE-MAINT-DATE.
02759      MOVE SPACES                 TO  ELCNTL-KEY.
02760
02761      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
02762      MOVE '1'                    TO  CNTL-REC-TYPE.
02763      MOVE +0                     TO  CNTL-SEQ-NO.
02764
02765      
      * EXEC CICS HANDLE CONDITION
02766 *        NOTFND   (8000-EXIT)
02767 *    END-EXEC.
      *    MOVE '"$I                   ! ( #00006599' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02768
02769      
      * EXEC CICS READ
02770 *        UPDATE
02771 *        DATASET   (CNTL-FILE-ID)
02772 *        SET       (ADDRESS OF CONTROL-FILE)
02773 *        RIDFLD    (ELCNTL-KEY)
02774 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006603' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02775
02776      CONTINUE.
02777
02778      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
02779      MOVE 'B'                    TO  JP-RECORD-TYPE.
02780      MOVE CNTL-FILE-ID           TO  FILE-ID.
02781      MOVE ELCNTL-LENGTH          TO  WS-JOURNAL-FILE-LENGTH.
02782      PERFORM 8400-LOG-JOURNAL-RECORD.
02783
02784      MOVE BIN-CURRENT-SAVE       TO  CF-REINSURANCE-TAB-MAINT-DT.
02785
02786      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
02787      MOVE 'B'                    TO  JP-RECORD-TYPE.
02788      MOVE CNTL-FILE-ID           TO  FILE-ID.
02789
02790      
      * EXEC CICS REWRITE
02791 *        DATASET   (CNTL-FILE-ID)
02792 *        FROM      (CONTROL-FILE)
02793 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006624' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02794
02795      PERFORM 8400-LOG-JOURNAL-RECORD.
02796
02797  8000-EXIT.
02798       EXIT.
02799      EJECT
02800  8100-SEND-INITIAL-MAP.
02801      MOVE SAVE-DATE              TO  BDATEO.
02802      MOVE EIBTIME                TO  TIME-IN.
02803      MOVE TIME-OUT               TO  BTIMEO.
02804      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02805      MOVE -1                     TO  MAINTYPL.
02806
02807      PERFORM 8250-SET-SCREEN-HEADINGS THRU 8250-EXIT.
02808
02809      
      * EXEC CICS SEND
02810 *        MAP     (MAP-B-NAME)
02811 *        MAPSET  (MAPSET-NAME)
02812 *        FROM    (EL6511BO)
02813 *        ERASE
02814 *        CURSOR
02815 *    END-EXEC.
           MOVE LENGTH OF
            EL6511BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006643' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-B-NAME, 
                 EL6511BO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02816
02817      GO TO 9100-RETURN-TRAN.
02818
02819  8110-SEND-INITIAL-MAP.
02820      MOVE SAVE-DATE              TO  CDATEO.
02821      MOVE EIBTIME                TO  TIME-IN.
02822      MOVE TIME-OUT               TO  CTIMEO.
02823      MOVE EMI-MESSAGE-AREA (1)   TO  CERRMSGO.
02824      MOVE -1                     TO  DESC1L.
02825
02826      
      * EXEC CICS SEND
02827 *        MAP     (MAP-C-NAME)
02828 *        MAPSET  (MAPSET-NAME)
02829 *        FROM    (EL6511CO)
02830 *        ERASE
02831 *        CURSOR
02832 *    END-EXEC.
           MOVE LENGTH OF
            EL6511CO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006660' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-C-NAME, 
                 EL6511CO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02833
02834      GO TO 9100-RETURN-TRAN.
02835
02836  8200-SEND-DATAONLY.
02837      MOVE SAVE-DATE              TO  BDATEO.
02838      MOVE EIBTIME                TO  TIME-IN.
02839      MOVE TIME-OUT               TO  BTIMEO.
02840      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
02841
02842      PERFORM 8250-SET-SCREEN-HEADINGS THRU 8250-EXIT.
02843
02844      
      * EXEC CICS SEND
02845 *        MAP     (MAP-B-NAME)
02846 *        MAPSET  (MAPSET-NAME)
02847 *        FROM    (EL6511BO)
02848 *        DATAONLY
02849 *        CURSOR
02850 *    END-EXEC.
           MOVE LENGTH OF
            EL6511BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006678' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-B-NAME, 
                 EL6511BO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02851
02852      GO TO 9100-RETURN-TRAN.
02853
02854  8210-SEND-DATAONLY.
02855      MOVE SAVE-DATE              TO  CDATEO.
02856      MOVE EIBTIME                TO  TIME-IN.
02857      MOVE TIME-OUT               TO  CTIMEO.
02858      MOVE EMI-MESSAGE-AREA (1)   TO  CERRMSGO.
02859
02860      
      * EXEC CICS SEND
02861 *        MAP     (MAP-C-NAME)
02862 *        MAPSET  (MAPSET-NAME)
02863 *        FROM    (EL6511CO)
02864 *        DATAONLY
02865 *        CURSOR
02866 *    END-EXEC.
           MOVE LENGTH OF
            EL6511CO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006694' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-C-NAME, 
                 EL6511CO, 
                 DFHEIV12, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02867
02868      GO TO 9100-RETURN-TRAN.
02869
02870  8250-SET-SCREEN-HEADINGS.
02871      MOVE PI-LIFE-OVERRIDE-L1    TO  FEELFHDO
02872                                      COMLFHDO
02873                                      TAXLFHDO.
02874      MOVE PI-AH-OVERRIDE-L1      TO  FEEAHHDO
02875                                      COMAHHDO
02876                                      TAXAHHDO.
02877      MOVE PI-LIFE-OVERRIDE-L2    TO  LIBNRHDO
02878                                      LFCVTYPO.
02879      MOVE PI-AH-OVERRIDE-L2      TO  AIBNRHDO
02880                                      AHCVTYPO.
02881      MOVE PI-LIFE-OVERRIDE-L6    TO  LFCLMHDO
02882                                      LFFEEHGO
02883                                      CMLFHDO.
02884      MOVE PI-AH-OVERRIDE-L6      TO  AHCLMHDO
02885                                      CMAHHDO
02886                                      AHFEEHGO.
02887
02888      MOVE AL-SANON               TO  FEELFHDA  COMLFHDA  TAXLFHDA
02889                                      AHCVTYPA  COMAHHDA  TAXAHHDA
02890                                      LFCVTYPA  CMLFHDA   LIBNRHDA
02891                                      CMAHHDA   AIBNRHDA  LFCLMHDA
02892                                      LFFEEHGA  AHCLMHDA  AHFEEHGA.
02893
02894      IF PI-COMPANY-ID = 'NCL' OR 'LGX'
02895         NEXT SENTENCE
02896      ELSE
02897         MOVE AL-SADOF            TO  GLHDRA
02898                                      GLCNTRA
02899                                      CUSTDSCA
02900         MOVE AL-SANOF            TO  CUSTAMTA.
02901
02902  8250-EXIT.
02903      EXIT.
02904
02905  8300-SEND-TEXT.
02906      
      * EXEC CICS SEND TEXT
02907 *        FROM    (LOGOFF-TEXT)
02908 *        LENGTH  (LOGOFF-LENGTH)
02909 *        ERASE
02910 *        FREEKB
02911 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006740' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373430' TO DFHEIV0(25:11)
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
           
02912
02913      
      * EXEC CICS RETURN
02914 *    END-EXEC.
      *    MOVE '.(                    ''   #00006747' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02915
02916  8400-LOG-JOURNAL-RECORD.
02917      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
02918      MOVE FILE-ID                TO  JP-FILE-ID.
02919      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
02920 *    IF PI-JOURNAL-FILE-ID NOT = ZERO
02921 *        EXEC CICS JOURNAL
02922 *            JFILEID  (PI-JOURNAL-FILE-ID)
02923 *            JTYPEID  ('EL')
02924 *            FROM     (JOURNAL-RECORD)
02925 *            LENGTH   (WS-JOURNAL-FILE-LENGTH)
02926 *        END-EXEC.
02927
02928  8800-UNAUTHORIZED-ACCESS.
02929      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
02930      GO TO 8300-SEND-TEXT.
02931
02932  8810-PF23.
02933      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02934      MOVE XCTL-005               TO  PGM-NAME.
02935      GO TO 9300-XCTL.
02936
02937  8870-NOTOPEN.
02938      MOVE ER-2055                TO  EMI-ERROR.
02939      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02940      MOVE -1                     TO  BENTERL.
02941      IF EIBTRNID NOT = TRANS-ID
02942          GO TO 8100-SEND-INITIAL-MAP.
02943
02944      GO TO 8200-SEND-DATAONLY.
02945
02946  8880-NOT-FOUND.
02947      MOVE ER-0142                TO  EMI-ERROR.
02948      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02949      MOVE -1                     TO  MAINTYPL.
02950      IF EIBTRNID NOT = TRANS-ID
02951          GO TO 8100-SEND-INITIAL-MAP.
02952
02953      GO TO 8200-SEND-DATAONLY.
02954
02955  9000-RETURN-CICS.
02956      
      * EXEC CICS RETURN
02957 *    END-EXEC.
      *    MOVE '.(                    ''   #00006790' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02958
02959  9100-RETURN-TRAN.
02960      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02961      MOVE '651B'                 TO  PI-CURRENT-SCREEN-NO.
02962      
      * EXEC CICS RETURN
02963 *        TRANSID   (TRANS-ID)
02964 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02965 *        LENGTH    (PI-COMM-LENGTH)
02966 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00006796' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02967
02968  9200-RETURN-MAIN-MENU.
02969      MOVE XCTL-626               TO  PGM-NAME.
02970      GO TO 9300-XCTL.
02971
02972  9300-XCTL.
02973      
      * EXEC CICS XCTL
02974 *        PROGRAM   (PGM-NAME)
02975 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
02976 *        LENGTH    (PI-COMM-LENGTH)
02977 *    END-EXEC.
      *    MOVE '.$C                   %   #00006807' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02978
02979  9400-CLEAR.
02980      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
02981      GO TO 9300-XCTL.
02982
02983  9500-PF12.
02984      MOVE XCTL-010               TO  PGM-NAME.
02985      GO TO 9300-XCTL.
02986
02987  9600-PGMID-ERROR.
02988      
      * EXEC CICS HANDLE CONDITION
02989 *        PGMIDERR  (8300-SEND-TEXT)
02990 *    END-EXEC.
      *    MOVE '"$L                   ! ) #00006822' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02991
02992      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
02993      MOVE ' '                    TO  PI-ENTRY-CD-1.
02994      MOVE XCTL-005               TO  PGM-NAME.
02995      MOVE PGM-NAME               TO  LOGOFF-PGM.
02996      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02997      GO TO 9300-XCTL.
02998
02999  9700-LINK-DATE-CONVERT.
03000      MOVE LINK-ELDATCV           TO  PGM-NAME.
03001
03002      
      * EXEC CICS LINK
03003 *        PROGRAM   (PGM-NAME)
03004 *        COMMAREA  (DATE-CONVERSION-DATA)
03005 *        LENGTH    (DC-COMM-LENGTH)
03006 *    END-EXEC.
      *    MOVE '."C                   (   #00006836' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03007
03008  9700-EXIT.
03009      EXIT.
03010
03011  9900-ERROR-FORMAT.
03012      IF NOT EMI-ERRORS-COMPLETE
03013          MOVE LINK-001           TO  PGM-NAME
03014          
      * EXEC CICS LINK
03015 *            PROGRAM   (PGM-NAME)
03016 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
03017 *            LENGTH    (EMI-COMM-LENGTH)
03018 *        END-EXEC.
      *    MOVE '."C                   (   #00006848' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03019
03020  9900-EXIT.
03021      EXIT.
03022
03023  9990-ABEND.
03024      MOVE LINK-004               TO  PGM-NAME.
03025      MOVE DFHEIBLK               TO  EMI-LINE1.
03026      
      * EXEC CICS LINK
03027 *        PROGRAM   (PGM-NAME)
03028 *        COMMAREA  (EMI-LINE1)
03029 *        LENGTH    (72)
03030 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006860' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03031
03032      IF PI-MAP-B
03033          GO TO 8200-SEND-DATAONLY
03034      ELSE
03035          GO TO 8210-SEND-DATAONLY.
03036
03037      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6511' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
03038
03039  9995-SECURITY-VIOLATION.
03040 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00006891' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383931' TO DFHEIV0(25:11)
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
03041
03042  9995-EXIT.
03043       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6511' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9990-ABEND,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9990-ABEND,
                     4250-CONT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7190-NOT-FOUND,
                     7192-END-OF-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7550-ENDFILE,
                     7575-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 7650-ENDFILE,
                     7675-NOTFOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8000-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6511' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
