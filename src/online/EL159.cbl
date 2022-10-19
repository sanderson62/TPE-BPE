       ID DIVISION.
       PROGRAM-ID.                 EL159.
      *
      *AUTHOR.     PABLO
      *            OMAHA NE
      *DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CENTRAL STATES IS EXPRESSLY PROHIBITED       *
      *            *   WITHOUT THE PRIOR WRITTEN PERMISSION OF         *
      *            *   CENTRAL STATES                                  *
      *            *                                                   *
      *            *****************************************************
      *REMARKS.
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
      *    FOR PRODUCT DEFINITIONS.
      *    SCREENS     - EL159A - PRODUCT DEFINITION
      *    ENTERED BY  - EL601 - MAINTENANCE MENU
      *    EXIT TO     - EL601 - MAINTENANCE MENU
      *    INPUT FILE  - ERPDEF -              - PRODUCT DEFINITION
      *    OUTPUT FILE - ERPDEF -              - PRODUCT DEFINITION
      *    COMMAREA    - PASSED
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA E031) THE SCREEN
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE
      *                  MAINTENANCE TYPE INDICATED.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD BENEFIT PERCENT
110618* 110618  CR2018100400001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
      ******************************************************************
       ENVIRONMENT DIVISION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
       77  FILLER  PIC X(32)  VALUE '********************************'.
       77  FILLER  PIC X(32)  VALUE '*    EL159 WORKING STORAGE     *'.
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.
       77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.
           88  FIRST-READ-PREV                         VALUE 'Y'.
       77  M1                              PIC S999 COMP-3 VALUE +0.
080322 77  R1                              PIC S999 COMP-3 VALUE +0.
080322 77  PAGE-NBR                        PIC S999 COMP-3 VALUE +0.
080322 01  WS-LINE-NBR-CHAR.
080322     12  WS-LINE-NBR.
080322         16  WS-LINE-NBR-1           PIC 9.
080322         16  WS-LINE-NBR-P           PIC X(02).
080322     12  WS-LINE-NBR-R REDEFINES WS-LINE-NBR.
080322         16  WS-LINE-NBR-2           PIC 9(02).
080322         16  WS-LINE-NBR-P2          PIC X(01).
       01  ACCESS-KEYS.
           12  ERPDEF-KEY.
               16  ERPDEF-COMPANY-CD       PIC X.
               16  ERPDEF-STATE            PIC XX.
               16  ERPDEF-PROD-CD          PIC XXX.
               16  FILLER                  PIC X(7).
               16  ERPDEF-BEN-TYPE         PIC X.
               16  ERPDEF-BEN-CODE         PIC XX.
               16  ERPDEF-EXP-DT           PIC XX.
           12  ELCNTL-KEY.
               16  ELCNTL-COMPANY-ID       PIC X(03).
               16  ELCNTL-RECORD-TYPE      PIC X(01).
               16  ELCNTL-ACCESS           PIC X(04).
               16  ELCNTL-STATE-ACCESS REDEFINES ELCNTL-ACCESS.
                   20  ELCNTL-STATE-CD     PIC  X(02).
                   20  FILLER              PIC  X(02).
               16  ELCNTL-BENEFIT-ACCESS REDEFINES ELCNTL-ACCESS.
                   20  FILLER              PIC  X(02).
                   20  ELCNTL-BENE-CD      PIC  X(02).
               16  ELCNTL-SEQUENCE-NO      PIC S9(04)      COMP.
       01  WS-DATE-AREA.
           05  SAVE-DATE                   PIC X(08)   VALUE SPACES.
           05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.
       01  MISC-WORK-AREAS.
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
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-999V99-OF-DEEDIT REDEFINES
                   WS-NUMVAL-OF-DEEDIT     PIC 9(9)V99.
100314         16  WS-9V999-OF-DEEDIT REDEFINES
100314             WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
           12  DEEDIT-FIELD                PIC X(11).
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD
                                           PIC S9(11).
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD
                                           PIC S9(09)V99.
           12  WS-BENEFIT-FIELD.
               16  WS-BENE-TYPE            PIC X(01).
               16  WS-BENE-CODE            PIC X(02).
           12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.
       01  STANDARD-AREAS.
           12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.
           12  TRANS-ID                    PIC X(04)   VALUE 'E031'.
           12  EL150-TRANS-ID              PIC X(04)   VALUE 'EX23'.
           12  EL1591-TRANS-ID             PIC X(04)   VALUE 'E032'.
           12  PGM-NAME                    PIC X(08).
           12  TIME-IN                     PIC S9(07).
           12  TIME-OUT-R  REDEFINES TIME-IN.
               16  FILLER                  PIC X(01).
               16  TIME-OUT                PIC 99V99.
               16  FILLER                  PIC X(02).
           12  XCTL-005                    PIC X(08)   VALUE 'EL005'.
           12  XCTL-010                    PIC X(08)   VALUE 'EL010'.
           12  XCTL-155                    PIC X(08)   VALUE 'EL155'.
           12  XCTL-626                    PIC X(08)   VALUE 'EL626'.
           12  LINK-001                    PIC X(08)   VALUE 'EL001'.
           12  LINK-004                    PIC X(08)   VALUE 'EL004'.
           12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'.
           12  THIS-PGM                    PIC X(08)   VALUE 'EL159'.
           12  ERPDEF-FILE-ID              PIC X(08)   VALUE 'ERPDEF'.
           12  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.
           12  ERPDEF-LENGTH               PIC S9(04)  VALUE +1319 COMP.
           12  SUB                         PIC 9(02).
           12  SUB-1                       PIC 9(02).
           12  SUB2                        PIC 9(02).
           12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.
           12  MAPSET-NAME                 PIC X(08)   VALUE 'EL159S'.
           12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL159A'.
           12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.
           12  WS-CNTL-REC-FOUND-SW        PIC X(01)   VALUE 'N'.
               88  CNTL-RECORD-FOUND                   VALUE 'Y'.
           12  WS-PRE-EXIST-CODES          PIC X(02)   VALUE ZEROS.
               88  VALID-PRE-EXIST-CODE                VALUES ARE '00'
                                                             THRU '99'.
           12  WS-DISABILITY-CODES         PIC X(02)   VALUE ZEROS.
               88  VALID-DISABILITY-CODE               VALUES ARE '00'
                                                             THRU '99'.
           12  WS-REFUND-METHOD            PIC X       VALUE ZEROS.
               88  VALID-REFUND-METHOD                 VALUES ARE ' ',
                                                         '1' THRU '9'.
           12  WS-BENEFIT-SW               PIC X       VALUE SPACE.
               88  BENEFIT-FOUND                       VALUE 'Y'.
               88  BENEFIT-NOT-FOUND                   VALUE 'N'.
           12  WS-MAXAMT                   PIC S9(9)    VALUE +0 COMP-3.
           12  WS-MAXATTAGE                PIC S999     VALUE +0 COMP-3.
           12  WS-MINAGE                   PIC S999     VALUE +0 COMP-3.
           12  WS-MAXAGE                   PIC S999     VALUE +0 COMP-3.
           12  WS-MAXTERM                  PIC S999     VALUE +0 COMP-3.
           12  WS-EXCL                     PIC S999     VALUE +0 COMP-3.
           12  WS-COV-ENDS                 PIC S999     VALUE +0 COMP-3.
           12  WS-ACC-ONLY                 PIC S999     VALUE +0 COMP-3.
           12  WS-CRIT-PER                 PIC S999     VALUE +0 COMP-3.
      *                                    COPY ELCSCTM.
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
      *                                    COPY ELCSCRTY.
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
       01  ERROR-MESSAGES.
           12  ER-0000                     PIC X(04)   VALUE '0000'.
           12  ER-0023                     PIC X(04)   VALUE '0023'.
           12  ER-0029                     PIC X(04)   VALUE '0029'.
           12  ER-0050                     PIC X(04)   VALUE '0050'.
           12  ER-0067                     PIC X(04)   VALUE '0067'.
           12  ER-0068                     PIC X(04)   VALUE '0068'.
           12  ER-0070                     PIC X(04)   VALUE '0070'.
           12  ER-0073                     PIC X(04)   VALUE '0073'.
           12  ER-0130                     PIC X(04)   VALUE '0130'.
           12  ER-0131                     PIC X(04)   VALUE '0131'.
           12  ER-0132                     PIC X(04)   VALUE '0132'.
           12  ER-0138                     PIC X(04)   VALUE '0138'.
           12  ER-0144                     PIC X(04)   VALUE '0144'.
           12  ER-0145                     PIC X(04)   VALUE '0145'.
           12  ER-0418                     PIC X(04)   VALUE '0418'.
           12  ER-0582                     PIC X(04)   VALUE '0582'.
           12  ER-0701                     PIC X(04)   VALUE '0701'.
           12  ER-0702                     PIC X(04)   VALUE '0702'.
           12  ER-0703                     PIC X(04)   VALUE '0703'.
           12  ER-0704                     PIC X(04)   VALUE '0704'.
           12  ER-0705                     PIC X(04)   VALUE '0705'.
           12  ER-0706                     PIC X(04)   VALUE '0706'.
           12  ER-0707                     PIC X(04)   VALUE '0707'.
           12  ER-0708                     PIC X(04)   VALUE '0708'.
           12  ER-0709                     PIC X(04)   VALUE '0709'.
           12  ER-0710                     PIC X(04)   VALUE '0710'.
           12  ER-0711                     PIC X(04)   VALUE '0711'.
           12  ER-0712                     PIC X(04)   VALUE '0712'.
           12  ER-0713                     PIC X(04)   VALUE '0713'.
           12  ER-0717                     PIC X(04)   VALUE '0717'.
           12  ER-0718                     PIC X(04)   VALUE '0718'.
           12  ER-0719                     PIC X(04)   VALUE '0719'.
           12  ER-0720                     PIC X(04)   VALUE '0720'.
           12  ER-0721                     PIC X(04)   VALUE '0721'.
           12  ER-0722                     PIC X(04)   VALUE '0722'.
           12  ER-0723                     PIC X(04)   VALUE '0723'.
           12  ER-0724                     PIC X(04)   VALUE '0724'.
           12  ER-0725                     PIC X(04)   VALUE '0725'.
           12  ER-0726                     PIC X(04)   VALUE '0726'.
           12  ER-0727                     PIC X(04)   VALUE '0727'.
           12  ER-0729                     PIC X(04)   VALUE '0729'.
           12  ER-0754                     PIC X(04)   VALUE '0754'.
080322     12  ER-1164                     PIC X(04)   VALUE '1164'.
           12  ER-2241                     PIC X(04)   VALUE '2241'.
           12  ER-2276                     PIC X(04)   VALUE '2276'.
           12  ER-7008                     PIC X(04)   VALUE '7008'.
           12  ER-7031                     PIC X(04)   VALUE '7031'.
           12  ER-7123                     PIC X(04)   VALUE '7123'.
100314     12  ER-7132                     PIC X(04)   VALUE '7132'.
           12  ER-8150                     PIC X(04)   VALUE '8150'.
           12  ER-9999                     PIC XXXX    VALUE '9999'.
      *                                    COPY ELCDATE.
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
      *                                    COPY ELCLOGOF.
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
      *                                    COPY ELCATTR.
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
      *                                    COPY ELCEMIB.
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
      *                                    COPY ELCJPFX.
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
                                           PIC X(530).
      *                                    COPY ELCINTF.
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
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.
               16  PI-FORM-NUMBER          PIC X(12).
               16  PI-PREV-PROD-KEY.
                   20  PI-PREV-COMPANY-CD  PIC X(01).
                   20  PI-PREV-STATE       PIC XX.
                   20  PI-PREV-PROD-CD     PIC XXX.
                   20  FILLER              PIC X(7).
                   20  PI-PREV-BEN-TYPE    PIC X.
                   20  PI-PREV-BEN-CODE    PIC XX.
                   20  PI-PREV-EXP-DT      PIC XX.
               16  PI-PROD-KEY.
                   20  PI-PK-COMPANY-CD    PIC X.
                   20  PI-PK-STATE         PIC XX.
                   20  PI-PK-PROD-CD       PIC XXX.
                   20  FILLER              PIC X(7).
                   20  PI-BEN-TYPE         PIC X.
                   20  PI-BEN-CODE         PIC XX.
                   20  PI-EXP-DT           PIC XX.
               16  FILLER                  PIC X(589).
      *                                    COPY ELCAID.
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
       01  FILLER    REDEFINES DFHAID.
           12  FILLER                      PIC X(08).
           12  PF-VALUES                   PIC X         OCCURS 24.
      *                                    COPY EL159S.
       01  EL159AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  TIMEL PIC S9(0004) COMP.
           05  TIMEF PIC  X(0001).
           05  FILLER REDEFINES TIMEF.
               10  TIMEA PIC  X(0001).
           05  TIMEI PIC  X(0005).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  MAINTBYL PIC S9(0004) COMP.
           05  MAINTBYF PIC  X(0001).
           05  FILLER REDEFINES MAINTBYF.
               10  MAINTBYA PIC  X(0001).
           05  MAINTBYI PIC  X(0004).
      *    -------------------------------
           05  MAINTONL PIC S9(0004) COMP.
           05  MAINTONF PIC  X(0001).
           05  FILLER REDEFINES MAINTONF.
               10  MAINTONA PIC  X(0001).
           05  MAINTONI PIC  X(0008).
      *    -------------------------------
           05  MAINTATL PIC S9(0004) COMP.
           05  MAINTATF PIC  X(0001).
           05  FILLER REDEFINES MAINTATF.
               10  MAINTATA PIC  X(0001).
           05  MAINTATI PIC  X(0005).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  PRODCDL PIC S9(0004) COMP.
           05  PRODCDF PIC  X(0001).
           05  FILLER REDEFINES PRODCDF.
               10  PRODCDA PIC  X(0001).
           05  PRODCDI PIC  X(0003).
      *    -------------------------------
           05  BENTYPL PIC S9(0004) COMP.
           05  BENTYPF PIC  X(0001).
           05  FILLER REDEFINES BENTYPF.
               10  BENTYPA PIC  X(0001).
           05  BENTYPI PIC  X(0001).
      *    -------------------------------
           05  BENCODEL PIC S9(0004) COMP.
           05  BENCODEF PIC  X(0001).
           05  FILLER REDEFINES BENCODEF.
               10  BENCODEA PIC  X(0001).
           05  BENCODEI PIC  X(0002).
      *    -------------------------------
           05  EXPDTL PIC S9(0004) COMP.
           05  EXPDTF PIC  X(0001).
           05  FILLER REDEFINES EXPDTF.
               10  EXPDTA PIC  X(0001).
           05  EXPDTI PIC  X(0010).
      *    -------------------------------
           05  PDESCL PIC S9(0004) COMP.
           05  PDESCF PIC  X(0001).
           05  FILLER REDEFINES PDESCF.
               10  PDESCA PIC  X(0001).
           05  PDESCI PIC  X(0080).
      *    -------------------------------
           05  AFAL PIC S9(0004) COMP.
           05  AFAF PIC  X(0001).
           05  FILLER REDEFINES AFAF.
               10  AFAA PIC  X(0001).
           05  AFAI PIC  X(0006).
      *    -------------------------------
           05  TRUNCL PIC S9(0004) COMP.
           05  TRUNCF PIC  X(0001).
           05  FILLER REDEFINES TRUNCF.
               10  TRUNCA PIC  X(0001).
           05  TRUNCI PIC  X(0001).
      *    -------------------------------
           05  LINE1L PIC S9(0004) COMP.
           05  LINE1F PIC  X(0001).
           05  FILLER REDEFINES LINE1F.
               10  LINE1A PIC  X(0001).
           05  LINE1I PIC  X(0003).
      *    -------------------------------
           05  PROD1L PIC S9(0004) COMP.
           05  PROD1F PIC  X(0001).
           05  FILLER REDEFINES PROD1F.
               10  PROD1A PIC  X(0001).
           05  PROD1I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE1L PIC S9(0004) COMP.
           05  ATTAGE1F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE1F.
               10  ATTAGE1A PIC  X(0001).
           05  ATTAGE1I PIC  X(0002).
      *    -------------------------------
           05  WAITPR1L PIC S9(0004) COMP.
           05  WAITPR1F PIC  X(0001).
           05  FILLER REDEFINES WAITPR1F.
               10  WAITPR1A PIC  X(0001).
           05  WAITPR1I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM1L PIC S9(0004) COMP.
           05  MAXTRM1F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM1F.
               10  MAXTRM1A PIC  X(0001).
           05  MAXTRM1I PIC  X(0003).
      *    -------------------------------
           05  BENPCT1L PIC S9(0004) COMP.
           05  BENPCT1F PIC  X(0001).
           05  FILLER REDEFINES BENPCT1F.
               10  BENPCT1A PIC  X(0001).
           05  BENPCT1I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT1L PIC S9(0004) COMP.
           05  MAXAMT1F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT1F.
               10  MAXAMT1A PIC  X(0001).
           05  MAXAMT1I PIC  X(0009).
      *    -------------------------------
           05  PRE1L PIC S9(0004) COMP.
           05  PRE1F PIC  X(0001).
           05  FILLER REDEFINES PRE1F.
               10  PRE1A PIC  X(0001).
           05  PRE1I PIC  X(0002).
      *    -------------------------------
           05  EXCL1L PIC S9(0004) COMP.
           05  EXCL1F PIC  X(0001).
           05  FILLER REDEFINES EXCL1F.
               10  EXCL1A PIC  X(0001).
           05  EXCL1I PIC  X(0003).
      *    -------------------------------
           05  COVEND1L PIC S9(0004) COMP.
           05  COVEND1F PIC  X(0001).
           05  FILLER REDEFINES COVEND1F.
               10  COVEND1A PIC  X(0001).
           05  COVEND1I PIC  X(0003).
      *    -------------------------------
           05  ACCPER1L PIC S9(0004) COMP.
           05  ACCPER1F PIC  X(0001).
           05  FILLER REDEFINES ACCPER1F.
               10  ACCPER1A PIC  X(0001).
           05  ACCPER1I PIC  9999.
      *    -------------------------------
           05  CRITPD1L PIC S9(0004) COMP.
           05  CRITPD1F PIC  X(0001).
           05  FILLER REDEFINES CRITPD1F.
               10  CRITPD1A PIC  X(0001).
           05  CRITPD1I PIC  999.
      *    -------------------------------
           05  RECCP1L PIC S9(0004) COMP.
           05  RECCP1F PIC  X(0001).
           05  FILLER REDEFINES RECCP1F.
               10  RECCP1A PIC  X(0001).
           05  RECCP1I PIC  X(0002).
      *    -------------------------------
           05  RTW1L PIC S9(0004) COMP.
           05  RTW1F PIC  X(0001).
           05  FILLER REDEFINES RTW1F.
               10  RTW1A PIC  X(0001).
           05  RTW1I PIC  999.
      *    -------------------------------
           05  MEXT1L PIC S9(0004) COMP.
           05  MEXT1F PIC  X(0001).
           05  FILLER REDEFINES MEXT1F.
               10  MEXT1A PIC  X(0001).
           05  MEXT1I PIC  999.
      *    -------------------------------
           05  LINE2L PIC S9(0004) COMP.
           05  LINE2F PIC  X(0001).
           05  FILLER REDEFINES LINE2F.
               10  LINE2A PIC  X(0001).
           05  LINE2I PIC  X(0003).
      *    -------------------------------
           05  PROD2L PIC S9(0004) COMP.
           05  PROD2F PIC  X(0001).
           05  FILLER REDEFINES PROD2F.
               10  PROD2A PIC  X(0001).
           05  PROD2I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE2L PIC S9(0004) COMP.
           05  ATTAGE2F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE2F.
               10  ATTAGE2A PIC  X(0001).
           05  ATTAGE2I PIC  X(0002).
      *    -------------------------------
           05  WAITPR2L PIC S9(0004) COMP.
           05  WAITPR2F PIC  X(0001).
           05  FILLER REDEFINES WAITPR2F.
               10  WAITPR2A PIC  X(0001).
           05  WAITPR2I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM2L PIC S9(0004) COMP.
           05  MAXTRM2F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM2F.
               10  MAXTRM2A PIC  X(0001).
           05  MAXTRM2I PIC  X(0003).
      *    -------------------------------
           05  BENPCT2L PIC S9(0004) COMP.
           05  BENPCT2F PIC  X(0001).
           05  FILLER REDEFINES BENPCT2F.
               10  BENPCT2A PIC  X(0001).
           05  BENPCT2I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT2L PIC S9(0004) COMP.
           05  MAXAMT2F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT2F.
               10  MAXAMT2A PIC  X(0001).
           05  MAXAMT2I PIC  X(0009).
      *    -------------------------------
           05  PRE2L PIC S9(0004) COMP.
           05  PRE2F PIC  X(0001).
           05  FILLER REDEFINES PRE2F.
               10  PRE2A PIC  X(0001).
           05  PRE2I PIC  X(0002).
      *    -------------------------------
           05  EXCL2L PIC S9(0004) COMP.
           05  EXCL2F PIC  X(0001).
           05  FILLER REDEFINES EXCL2F.
               10  EXCL2A PIC  X(0001).
           05  EXCL2I PIC  X(0003).
      *    -------------------------------
           05  COVEND2L PIC S9(0004) COMP.
           05  COVEND2F PIC  X(0001).
           05  FILLER REDEFINES COVEND2F.
               10  COVEND2A PIC  X(0001).
           05  COVEND2I PIC  X(0003).
      *    -------------------------------
           05  ACCPER2L PIC S9(0004) COMP.
           05  ACCPER2F PIC  X(0001).
           05  FILLER REDEFINES ACCPER2F.
               10  ACCPER2A PIC  X(0001).
           05  ACCPER2I PIC  9999.
      *    -------------------------------
           05  CRITPD2L PIC S9(0004) COMP.
           05  CRITPD2F PIC  X(0001).
           05  FILLER REDEFINES CRITPD2F.
               10  CRITPD2A PIC  X(0001).
           05  CRITPD2I PIC  999.
      *    -------------------------------
           05  RECCP2L PIC S9(0004) COMP.
           05  RECCP2F PIC  X(0001).
           05  FILLER REDEFINES RECCP2F.
               10  RECCP2A PIC  X(0001).
           05  RECCP2I PIC  X(0002).
      *    -------------------------------
           05  RTW2L PIC S9(0004) COMP.
           05  RTW2F PIC  X(0001).
           05  FILLER REDEFINES RTW2F.
               10  RTW2A PIC  X(0001).
           05  RTW2I PIC  999.
      *    -------------------------------
           05  MEXT2L PIC S9(0004) COMP.
           05  MEXT2F PIC  X(0001).
           05  FILLER REDEFINES MEXT2F.
               10  MEXT2A PIC  X(0001).
           05  MEXT2I PIC  999.
      *    -------------------------------
           05  LINE3L PIC S9(0004) COMP.
           05  LINE3F PIC  X(0001).
           05  FILLER REDEFINES LINE3F.
               10  LINE3A PIC  X(0001).
           05  LINE3I PIC  X(0003).
      *    -------------------------------
           05  PROD3L PIC S9(0004) COMP.
           05  PROD3F PIC  X(0001).
           05  FILLER REDEFINES PROD3F.
               10  PROD3A PIC  X(0001).
           05  PROD3I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE3L PIC S9(0004) COMP.
           05  ATTAGE3F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE3F.
               10  ATTAGE3A PIC  X(0001).
           05  ATTAGE3I PIC  X(0002).
      *    -------------------------------
           05  WAITPR3L PIC S9(0004) COMP.
           05  WAITPR3F PIC  X(0001).
           05  FILLER REDEFINES WAITPR3F.
               10  WAITPR3A PIC  X(0001).
           05  WAITPR3I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM3L PIC S9(0004) COMP.
           05  MAXTRM3F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM3F.
               10  MAXTRM3A PIC  X(0001).
           05  MAXTRM3I PIC  X(0003).
      *    -------------------------------
           05  BENPCT3L PIC S9(0004) COMP.
           05  BENPCT3F PIC  X(0001).
           05  FILLER REDEFINES BENPCT3F.
               10  BENPCT3A PIC  X(0001).
           05  BENPCT3I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT3L PIC S9(0004) COMP.
           05  MAXAMT3F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT3F.
               10  MAXAMT3A PIC  X(0001).
           05  MAXAMT3I PIC  X(0009).
      *    -------------------------------
           05  PRE3L PIC S9(0004) COMP.
           05  PRE3F PIC  X(0001).
           05  FILLER REDEFINES PRE3F.
               10  PRE3A PIC  X(0001).
           05  PRE3I PIC  X(0002).
      *    -------------------------------
           05  EXCL3L PIC S9(0004) COMP.
           05  EXCL3F PIC  X(0001).
           05  FILLER REDEFINES EXCL3F.
               10  EXCL3A PIC  X(0001).
           05  EXCL3I PIC  X(0003).
      *    -------------------------------
           05  COVEND3L PIC S9(0004) COMP.
           05  COVEND3F PIC  X(0001).
           05  FILLER REDEFINES COVEND3F.
               10  COVEND3A PIC  X(0001).
           05  COVEND3I PIC  X(0003).
      *    -------------------------------
           05  ACCPER3L PIC S9(0004) COMP.
           05  ACCPER3F PIC  X(0001).
           05  FILLER REDEFINES ACCPER3F.
               10  ACCPER3A PIC  X(0001).
           05  ACCPER3I PIC  9999.
      *    -------------------------------
           05  CRITPD3L PIC S9(0004) COMP.
           05  CRITPD3F PIC  X(0001).
           05  FILLER REDEFINES CRITPD3F.
               10  CRITPD3A PIC  X(0001).
           05  CRITPD3I PIC  999.
      *    -------------------------------
           05  RECCP3L PIC S9(0004) COMP.
           05  RECCP3F PIC  X(0001).
           05  FILLER REDEFINES RECCP3F.
               10  RECCP3A PIC  X(0001).
           05  RECCP3I PIC  X(0002).
      *    -------------------------------
           05  RTW3L PIC S9(0004) COMP.
           05  RTW3F PIC  X(0001).
           05  FILLER REDEFINES RTW3F.
               10  RTW3A PIC  X(0001).
           05  RTW3I PIC  999.
      *    -------------------------------
           05  MEXT3L PIC S9(0004) COMP.
           05  MEXT3F PIC  X(0001).
           05  FILLER REDEFINES MEXT3F.
               10  MEXT3A PIC  X(0001).
           05  MEXT3I PIC  999.
      *    -------------------------------
           05  LINE4L PIC S9(0004) COMP.
           05  LINE4F PIC  X(0001).
           05  FILLER REDEFINES LINE4F.
               10  LINE4A PIC  X(0001).
           05  LINE4I PIC  X(0003).
      *    -------------------------------
           05  PROD4L PIC S9(0004) COMP.
           05  PROD4F PIC  X(0001).
           05  FILLER REDEFINES PROD4F.
               10  PROD4A PIC  X(0001).
           05  PROD4I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE4L PIC S9(0004) COMP.
           05  ATTAGE4F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE4F.
               10  ATTAGE4A PIC  X(0001).
           05  ATTAGE4I PIC  X(0002).
      *    -------------------------------
           05  WAITPR4L PIC S9(0004) COMP.
           05  WAITPR4F PIC  X(0001).
           05  FILLER REDEFINES WAITPR4F.
               10  WAITPR4A PIC  X(0001).
           05  WAITPR4I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM4L PIC S9(0004) COMP.
           05  MAXTRM4F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM4F.
               10  MAXTRM4A PIC  X(0001).
           05  MAXTRM4I PIC  X(0003).
      *    -------------------------------
           05  BENPCT4L PIC S9(0004) COMP.
           05  BENPCT4F PIC  X(0001).
           05  FILLER REDEFINES BENPCT4F.
               10  BENPCT4A PIC  X(0001).
           05  BENPCT4I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT4L PIC S9(0004) COMP.
           05  MAXAMT4F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT4F.
               10  MAXAMT4A PIC  X(0001).
           05  MAXAMT4I PIC  X(0009).
      *    -------------------------------
           05  PRE4L PIC S9(0004) COMP.
           05  PRE4F PIC  X(0001).
           05  FILLER REDEFINES PRE4F.
               10  PRE4A PIC  X(0001).
           05  PRE4I PIC  X(0002).
      *    -------------------------------
           05  EXCL4L PIC S9(0004) COMP.
           05  EXCL4F PIC  X(0001).
           05  FILLER REDEFINES EXCL4F.
               10  EXCL4A PIC  X(0001).
           05  EXCL4I PIC  X(0003).
      *    -------------------------------
           05  COVEND4L PIC S9(0004) COMP.
           05  COVEND4F PIC  X(0001).
           05  FILLER REDEFINES COVEND4F.
               10  COVEND4A PIC  X(0001).
           05  COVEND4I PIC  X(0003).
      *    -------------------------------
           05  ACCPER4L PIC S9(0004) COMP.
           05  ACCPER4F PIC  X(0001).
           05  FILLER REDEFINES ACCPER4F.
               10  ACCPER4A PIC  X(0001).
           05  ACCPER4I PIC  9999.
      *    -------------------------------
           05  CRITPD4L PIC S9(0004) COMP.
           05  CRITPD4F PIC  X(0001).
           05  FILLER REDEFINES CRITPD4F.
               10  CRITPD4A PIC  X(0001).
           05  CRITPD4I PIC  999.
      *    -------------------------------
           05  RECCP4L PIC S9(0004) COMP.
           05  RECCP4F PIC  X(0001).
           05  FILLER REDEFINES RECCP4F.
               10  RECCP4A PIC  X(0001).
           05  RECCP4I PIC  X(0002).
      *    -------------------------------
           05  RTW4L PIC S9(0004) COMP.
           05  RTW4F PIC  X(0001).
           05  FILLER REDEFINES RTW4F.
               10  RTW4A PIC  X(0001).
           05  RTW4I PIC  999.
      *    -------------------------------
           05  MEXT4L PIC S9(0004) COMP.
           05  MEXT4F PIC  X(0001).
           05  FILLER REDEFINES MEXT4F.
               10  MEXT4A PIC  X(0001).
           05  MEXT4I PIC  999.
      *    -------------------------------
           05  LINE5L PIC S9(0004) COMP.
           05  LINE5F PIC  X(0001).
           05  FILLER REDEFINES LINE5F.
               10  LINE5A PIC  X(0001).
           05  LINE5I PIC  X(0003).
      *    -------------------------------
           05  PROD5L PIC S9(0004) COMP.
           05  PROD5F PIC  X(0001).
           05  FILLER REDEFINES PROD5F.
               10  PROD5A PIC  X(0001).
           05  PROD5I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE5L PIC S9(0004) COMP.
           05  ATTAGE5F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE5F.
               10  ATTAGE5A PIC  X(0001).
           05  ATTAGE5I PIC  X(0002).
      *    -------------------------------
           05  WAITPR5L PIC S9(0004) COMP.
           05  WAITPR5F PIC  X(0001).
           05  FILLER REDEFINES WAITPR5F.
               10  WAITPR5A PIC  X(0001).
           05  WAITPR5I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM5L PIC S9(0004) COMP.
           05  MAXTRM5F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM5F.
               10  MAXTRM5A PIC  X(0001).
           05  MAXTRM5I PIC  X(0003).
      *    -------------------------------
           05  BENPCT5L PIC S9(0004) COMP.
           05  BENPCT5F PIC  X(0001).
           05  FILLER REDEFINES BENPCT5F.
               10  BENPCT5A PIC  X(0001).
           05  BENPCT5I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT5L PIC S9(0004) COMP.
           05  MAXAMT5F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT5F.
               10  MAXAMT5A PIC  X(0001).
           05  MAXAMT5I PIC  X(0009).
      *    -------------------------------
           05  PRE5L PIC S9(0004) COMP.
           05  PRE5F PIC  X(0001).
           05  FILLER REDEFINES PRE5F.
               10  PRE5A PIC  X(0001).
           05  PRE5I PIC  X(0002).
      *    -------------------------------
           05  EXCL5L PIC S9(0004) COMP.
           05  EXCL5F PIC  X(0001).
           05  FILLER REDEFINES EXCL5F.
               10  EXCL5A PIC  X(0001).
           05  EXCL5I PIC  X(0003).
      *    -------------------------------
           05  COVEND5L PIC S9(0004) COMP.
           05  COVEND5F PIC  X(0001).
           05  FILLER REDEFINES COVEND5F.
               10  COVEND5A PIC  X(0001).
           05  COVEND5I PIC  X(0003).
      *    -------------------------------
           05  ACCPER5L PIC S9(0004) COMP.
           05  ACCPER5F PIC  X(0001).
           05  FILLER REDEFINES ACCPER5F.
               10  ACCPER5A PIC  X(0001).
           05  ACCPER5I PIC  9999.
      *    -------------------------------
           05  CRITPD5L PIC S9(0004) COMP.
           05  CRITPD5F PIC  X(0001).
           05  FILLER REDEFINES CRITPD5F.
               10  CRITPD5A PIC  X(0001).
           05  CRITPD5I PIC  999.
      *    -------------------------------
           05  RECCP5L PIC S9(0004) COMP.
           05  RECCP5F PIC  X(0001).
           05  FILLER REDEFINES RECCP5F.
               10  RECCP5A PIC  X(0001).
           05  RECCP5I PIC  X(0002).
      *    -------------------------------
           05  RTW5L PIC S9(0004) COMP.
           05  RTW5F PIC  X(0001).
           05  FILLER REDEFINES RTW5F.
               10  RTW5A PIC  X(0001).
           05  RTW5I PIC  999.
      *    -------------------------------
           05  MEXT5L PIC S9(0004) COMP.
           05  MEXT5F PIC  X(0001).
           05  FILLER REDEFINES MEXT5F.
               10  MEXT5A PIC  X(0001).
           05  MEXT5I PIC  999.
      *    -------------------------------
           05  LINE6L PIC S9(0004) COMP.
           05  LINE6F PIC  X(0001).
           05  FILLER REDEFINES LINE6F.
               10  LINE6A PIC  X(0001).
           05  LINE6I PIC  X(0003).
      *    -------------------------------
           05  PROD6L PIC S9(0004) COMP.
           05  PROD6F PIC  X(0001).
           05  FILLER REDEFINES PROD6F.
               10  PROD6A PIC  X(0001).
           05  PROD6I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE6L PIC S9(0004) COMP.
           05  ATTAGE6F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE6F.
               10  ATTAGE6A PIC  X(0001).
           05  ATTAGE6I PIC  X(0002).
      *    -------------------------------
           05  WAITPR6L PIC S9(0004) COMP.
           05  WAITPR6F PIC  X(0001).
           05  FILLER REDEFINES WAITPR6F.
               10  WAITPR6A PIC  X(0001).
           05  WAITPR6I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM6L PIC S9(0004) COMP.
           05  MAXTRM6F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM6F.
               10  MAXTRM6A PIC  X(0001).
           05  MAXTRM6I PIC  X(0003).
      *    -------------------------------
           05  BENPCT6L PIC S9(0004) COMP.
           05  BENPCT6F PIC  X(0001).
           05  FILLER REDEFINES BENPCT6F.
               10  BENPCT6A PIC  X(0001).
           05  BENPCT6I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT6L PIC S9(0004) COMP.
           05  MAXAMT6F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT6F.
               10  MAXAMT6A PIC  X(0001).
           05  MAXAMT6I PIC  X(0009).
      *    -------------------------------
           05  PRE6L PIC S9(0004) COMP.
           05  PRE6F PIC  X(0001).
           05  FILLER REDEFINES PRE6F.
               10  PRE6A PIC  X(0001).
           05  PRE6I PIC  X(0002).
      *    -------------------------------
           05  EXCL6L PIC S9(0004) COMP.
           05  EXCL6F PIC  X(0001).
           05  FILLER REDEFINES EXCL6F.
               10  EXCL6A PIC  X(0001).
           05  EXCL6I PIC  X(0003).
      *    -------------------------------
           05  COVEND6L PIC S9(0004) COMP.
           05  COVEND6F PIC  X(0001).
           05  FILLER REDEFINES COVEND6F.
               10  COVEND6A PIC  X(0001).
           05  COVEND6I PIC  X(0003).
      *    -------------------------------
           05  ACCPER6L PIC S9(0004) COMP.
           05  ACCPER6F PIC  X(0001).
           05  FILLER REDEFINES ACCPER6F.
               10  ACCPER6A PIC  X(0001).
           05  ACCPER6I PIC  9999.
      *    -------------------------------
           05  CRITPD6L PIC S9(0004) COMP.
           05  CRITPD6F PIC  X(0001).
           05  FILLER REDEFINES CRITPD6F.
               10  CRITPD6A PIC  X(0001).
           05  CRITPD6I PIC  999.
      *    -------------------------------
           05  RECCP6L PIC S9(0004) COMP.
           05  RECCP6F PIC  X(0001).
           05  FILLER REDEFINES RECCP6F.
               10  RECCP6A PIC  X(0001).
           05  RECCP6I PIC  X(0002).
      *    -------------------------------
           05  RTW6L PIC S9(0004) COMP.
           05  RTW6F PIC  X(0001).
           05  FILLER REDEFINES RTW6F.
               10  RTW6A PIC  X(0001).
           05  RTW6I PIC  999.
      *    -------------------------------
           05  MEXT6L PIC S9(0004) COMP.
           05  MEXT6F PIC  X(0001).
           05  FILLER REDEFINES MEXT6F.
               10  MEXT6A PIC  X(0001).
           05  MEXT6I PIC  999.
      *    -------------------------------
           05  LINE7L PIC S9(0004) COMP.
           05  LINE7F PIC  X(0001).
           05  FILLER REDEFINES LINE7F.
               10  LINE7A PIC  X(0001).
           05  LINE7I PIC  X(0003).
      *    -------------------------------
           05  PROD7L PIC S9(0004) COMP.
           05  PROD7F PIC  X(0001).
           05  FILLER REDEFINES PROD7F.
               10  PROD7A PIC  X(0001).
           05  PROD7I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE7L PIC S9(0004) COMP.
           05  ATTAGE7F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE7F.
               10  ATTAGE7A PIC  X(0001).
           05  ATTAGE7I PIC  X(0002).
      *    -------------------------------
           05  WAITPR7L PIC S9(0004) COMP.
           05  WAITPR7F PIC  X(0001).
           05  FILLER REDEFINES WAITPR7F.
               10  WAITPR7A PIC  X(0001).
           05  WAITPR7I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM7L PIC S9(0004) COMP.
           05  MAXTRM7F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM7F.
               10  MAXTRM7A PIC  X(0001).
           05  MAXTRM7I PIC  X(0003).
      *    -------------------------------
           05  BENPCT7L PIC S9(0004) COMP.
           05  BENPCT7F PIC  X(0001).
           05  FILLER REDEFINES BENPCT7F.
               10  BENPCT7A PIC  X(0001).
           05  BENPCT7I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT7L PIC S9(0004) COMP.
           05  MAXAMT7F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT7F.
               10  MAXAMT7A PIC  X(0001).
           05  MAXAMT7I PIC  X(0009).
      *    -------------------------------
           05  PRE7L PIC S9(0004) COMP.
           05  PRE7F PIC  X(0001).
           05  FILLER REDEFINES PRE7F.
               10  PRE7A PIC  X(0001).
           05  PRE7I PIC  X(0002).
      *    -------------------------------
           05  EXCL7L PIC S9(0004) COMP.
           05  EXCL7F PIC  X(0001).
           05  FILLER REDEFINES EXCL7F.
               10  EXCL7A PIC  X(0001).
           05  EXCL7I PIC  X(0003).
      *    -------------------------------
           05  COVEND7L PIC S9(0004) COMP.
           05  COVEND7F PIC  X(0001).
           05  FILLER REDEFINES COVEND7F.
               10  COVEND7A PIC  X(0001).
           05  COVEND7I PIC  X(0003).
      *    -------------------------------
           05  ACCPER7L PIC S9(0004) COMP.
           05  ACCPER7F PIC  X(0001).
           05  FILLER REDEFINES ACCPER7F.
               10  ACCPER7A PIC  X(0001).
           05  ACCPER7I PIC  9999.
      *    -------------------------------
           05  CRITPD7L PIC S9(0004) COMP.
           05  CRITPD7F PIC  X(0001).
           05  FILLER REDEFINES CRITPD7F.
               10  CRITPD7A PIC  X(0001).
           05  CRITPD7I PIC  999.
      *    -------------------------------
           05  RECCP17L PIC S9(0004) COMP.
           05  RECCP17F PIC  X(0001).
           05  FILLER REDEFINES RECCP17F.
               10  RECCP17A PIC  X(0001).
           05  RECCP17I PIC  X(0002).
      *    -------------------------------
           05  RTW7L PIC S9(0004) COMP.
           05  RTW7F PIC  X(0001).
           05  FILLER REDEFINES RTW7F.
               10  RTW7A PIC  X(0001).
           05  RTW7I PIC  999.
      *    -------------------------------
           05  MEXT7L PIC S9(0004) COMP.
           05  MEXT7F PIC  X(0001).
           05  FILLER REDEFINES MEXT7F.
               10  MEXT7A PIC  X(0001).
           05  MEXT7I PIC  999.
      *    -------------------------------
           05  LINE8L PIC S9(0004) COMP.
           05  LINE8F PIC  X(0001).
           05  FILLER REDEFINES LINE8F.
               10  LINE8A PIC  X(0001).
           05  LINE8I PIC  X(0003).
      *    -------------------------------
           05  PROD8L PIC S9(0004) COMP.
           05  PROD8F PIC  X(0001).
           05  FILLER REDEFINES PROD8F.
               10  PROD8A PIC  X(0001).
           05  PROD8I PIC  X(0001).
      *    -------------------------------
           05  ATTAGE8L PIC S9(0004) COMP.
           05  ATTAGE8F PIC  X(0001).
           05  FILLER REDEFINES ATTAGE8F.
               10  ATTAGE8A PIC  X(0001).
           05  ATTAGE8I PIC  X(0002).
      *    -------------------------------
           05  WAITPR8L PIC S9(0004) COMP.
           05  WAITPR8F PIC  X(0001).
           05  FILLER REDEFINES WAITPR8F.
               10  WAITPR8A PIC  X(0001).
           05  WAITPR8I PIC  X(0003).
      *    -------------------------------
           05  MAXTRM8L PIC S9(0004) COMP.
           05  MAXTRM8F PIC  X(0001).
           05  FILLER REDEFINES MAXTRM8F.
               10  MAXTRM8A PIC  X(0001).
           05  MAXTRM8I PIC  X(0003).
      *    -------------------------------
           05  BENPCT8L PIC S9(0004) COMP.
           05  BENPCT8F PIC  X(0001).
           05  FILLER REDEFINES BENPCT8F.
               10  BENPCT8A PIC  X(0001).
           05  BENPCT8I PIC  X(0004).
      *    -------------------------------
           05  MAXAMT8L PIC S9(0004) COMP.
           05  MAXAMT8F PIC  X(0001).
           05  FILLER REDEFINES MAXAMT8F.
               10  MAXAMT8A PIC  X(0001).
           05  MAXAMT8I PIC  X(0009).
      *    -------------------------------
           05  PRE8L PIC S9(0004) COMP.
           05  PRE8F PIC  X(0001).
           05  FILLER REDEFINES PRE8F.
               10  PRE8A PIC  X(0001).
           05  PRE8I PIC  X(0002).
      *    -------------------------------
           05  EXCL8L PIC S9(0004) COMP.
           05  EXCL8F PIC  X(0001).
           05  FILLER REDEFINES EXCL8F.
               10  EXCL8A PIC  X(0001).
           05  EXCL8I PIC  X(0003).
      *    -------------------------------
           05  COVEND8L PIC S9(0004) COMP.
           05  COVEND8F PIC  X(0001).
           05  FILLER REDEFINES COVEND8F.
               10  COVEND8A PIC  X(0001).
           05  COVEND8I PIC  X(0003).
      *    -------------------------------
           05  ACCPER8L PIC S9(0004) COMP.
           05  ACCPER8F PIC  X(0001).
           05  FILLER REDEFINES ACCPER8F.
               10  ACCPER8A PIC  X(0001).
           05  ACCPER8I PIC  9999.
      *    -------------------------------
           05  CRITPD8L PIC S9(0004) COMP.
           05  CRITPD8F PIC  X(0001).
           05  FILLER REDEFINES CRITPD8F.
               10  CRITPD8A PIC  X(0001).
           05  CRITPD8I PIC  999.
      *    -------------------------------
           05  RECCP8L PIC S9(0004) COMP.
           05  RECCP8F PIC  X(0001).
           05  FILLER REDEFINES RECCP8F.
               10  RECCP8A PIC  X(0001).
           05  RECCP8I PIC  X(0002).
      *    -------------------------------
           05  RTW8L PIC S9(0004) COMP.
           05  RTW8F PIC  X(0001).
           05  FILLER REDEFINES RTW8F.
               10  RTW8A PIC  X(0001).
           05  RTW8I PIC  999.
      *    -------------------------------
           05  MEXT8L PIC S9(0004) COMP.
           05  MEXT8F PIC  X(0001).
           05  FILLER REDEFINES MEXT8F.
               10  MEXT8A PIC  X(0001).
           05  MEXT8I PIC  999.
      *    -------------------------------
           05  MORERECL PIC S9(0004) COMP.
           05  MORERECF PIC  X(0001).
           05  FILLER REDEFINES MORERECF.
               10  MORERECA PIC  X(0001).
           05  MORERECI PIC  X(0003).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL159AO REDEFINES EL159AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTONO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTATO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRODCDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTYPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENCODEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXPDTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDESCO PIC  X(0080).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AFAO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TRUNCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT1O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT1O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER1O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD1O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW1O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT1O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT2O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT2O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER2O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD2O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW2O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT2O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT3O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT3O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER3O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD3O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW3O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT3O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT4O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT4O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER4O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD4O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW4O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT4O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT5O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT5O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER5O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD5O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW5O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT5O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT6O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT6O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER6O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD6O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW6O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT6O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT7O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT7O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER7O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD7O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP17O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW7O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT7O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LINE8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROD8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATTAGE8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  WAITPR8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXTRM8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENPCT8O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAXAMT8O PIC  Z,ZZZ,ZZZ.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRE8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EXCL8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COVEND8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCPER8O PIC  ZZ99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRITPD8O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECCP8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RTW8O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MEXT8O PIC  Z99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MORERECO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  99.
      *    -------------------------------
       01  EL159AI-R REDEFINES EL159AI.
           12  FILLER                      PIC X(177).
           12  AFA-LEN                     PIC S9(04)  COMP.
           12  AFA-ATTRB                   PIC X.
           12  AFA-AMT                     PIC X(6).
           12  AFA-AMT-IN REDEFINES AFA-AMT PIC 9(4)V99.
           12  AFA-AMT-OUT REDEFINES AFA-AMT
                                           PIC ZZ9.99.
           12  F                           PIC X(4).
           12  EL159-PROD-TABLE OCCURS 8.
               16  LINE-NBR-LEN            PIC S9(04)  COMP.
               16  LINE-NBR-ATTRB          PIC X(01).
               16  LINE-NBR                PIC X(03).
               16  PROD-CODE-LEN           PIC S9(04)  COMP.
               16  PROD-CODE-ATTRB         PIC X(01).
               16  PROD-CODE               PIC X.
               16  MAX-ATT-AGE-LEN         PIC S9(04)  COMP.
               16  MAX-ATT-AGE-ATTRB       PIC X(01).
               16  MAX-ATT-AGE             PIC 99.
080322*        16  MIN-AGE-LEN             PIC S9(04)  COMP.
080322*        16  MIN-AGE-ATTRB           PIC X(01).
080322*        16  MIN-AGE                 PIC 99.
               16  WAIT-PR-LEN             PIC S9(04)  COMP.
               16  WAIT-PR-ATTRB           PIC X(01).
               16  WAIT-PR                 PIC X(03).
               16  MAX-TERM-LEN            PIC S9(04)  COMP.
               16  MAX-TERM-ATTRB          PIC X(01).
               16  MAX-TERM                PIC 999.
100314         16  ben-pct-len             pic s9(04)  comp.
100314         16  ben-pct-attrb           pic x.
100314         16  ben-pct-in              pic v9(4).
100314         16  ben-pct-out redefines
100314             ben-pct-in              pic .999.
               16  MAX-AMT-LEN             PIC S9(04)  COMP.
               16  MAX-AMT-ATTRB           PIC X(01).
               16  MAX-AMT-IN              PIC 9(9).
               16  MAX-AMT-OUT REDEFINES
                   MAX-AMT-IN              PIC Z,ZZZ,999.
               16  PRE-EXIST-LEN           PIC S9(04)  COMP.
               16  PRE-EXIST-ATTRB         PIC X(01).
               16  PRE-EXIST               PIC 99.
               16  EXCL-PER-LEN            PIC S9(04)  COMP.
               16  EXCL-PER-ATTRB          PIC X(01).
               16  EXCL-PERIOD             PIC 999.
               16  COV-ENDS-LEN            PIC S9(04)  COMP.
               16  COV-ENDS-ATTRB          PIC X(01).
               16  COV-ENDS                PIC 999.
               16  ACC-PER-ENDS-LEN        PIC S9(04)  COMP.
               16  ACC-PER-ENDS-ATTRB      PIC X(01).
               16  ACC-PER-ENDS            PIC 9999.
               16  ACC-PER-ENDS-OUT REDEFINES ACC-PER-ENDS
                                           PIC ZZ99.
               16  CRIT-PER-LEN            PIC S9(04)  COMP.
               16  CRIT-PER-ATTRB          PIC X(01).
               16  CRIT-PER                PIC 999.
               16  CRIT-PER-OUT REDEFINES CRIT-PER
                                           PIC Z99.
               16  REC-CP-LEN              PIC S9(04)  COMP.
               16  REC-CP-ATTRB            PIC X(01).
               16  REC-CRIT-PER            PIC XX.
               16  REC-CRIT-PER-N REDEFINES REC-CRIT-PER
                                           PIC 99.
               16  RTW-MOS-LEN             PIC S9(04)  COMP.
               16  RTW-MOS-ATTRB           PIC X(01).
               16  RTW-MOS                 PIC 999.
               16  RTW-MOS-OUT REDEFINES RTW-MOS
                                           PIC Z99.
               16  MAX-EXT-LEN             PIC S9(04)  COMP.
               16  MAX-EXT-ATTRB           PIC X(01).
               16  MAX-EXT                 PIC 999.
               16  MAX-EXT-OUT REDEFINES MAX-EXT
                                           PIC Z99.
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
       01  DFHCOMMAREA                     PIC X(1024).
      *                                    COPY ERCPDEF.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCPDEF.                            *
      *                                                                *
      *    FILE DESCRIPTION = PRODUCT DEFINITION MASTER                *
      *                                                                *
      *    FILE TYPE = VSAM,KSDS                                       *
      *    RECORD SIZE = 1319 RECFORM = FIXED                          *
      *                                                                *
      *    BASE CLUSTER = ERPDEF                      RKP=02,LEN=18    *
      *                                                                *
      *    LOG = YES                                                   *
      *    SEVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
051414*                   C H A N G E   L O G
051414*
051414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
051414*-----------------------------------------------------------------
051414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
051414* EFFECTIVE    NUMBER
051414*-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD PCT OF BENEFIT
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  TANA  Add B and H claim types
      ******************************************************************
       01  PRODUCT-MASTER.
          12  PD-RECORD-ID                 PIC X(02).
              88  VALID-PD-ID                  VALUE 'PD'.
          12  PD-CONTROL-PRIMARY.
              16  PD-COMPANY-CD            PIC X.
              16  PD-STATE                 PIC XX.
              16  PD-PRODUCT-CD            PIC XXX.
              16  PD-FILLER                PIC X(7).
              16  PD-BEN-TYPE              PIC X.
              16  PD-BEN-CODE              PIC XX.
              16  PD-PROD-EXP-DT           PIC XX.
          12  FILLER                       PIC X(6).
          12  PD-PRODUCT-DATA OCCURS 11.
              16  PD-PROD-CODE             PIC X.
                  88  PD-PROD-LIFE           VALUE 'L'.
                  88  PD-PROD-PROP           VALUE 'P'.
                  88  PD-PROD-AH             VALUE 'A'.
                  88  PD-PROD-IU             VALUE 'I'.
                  88  PD-PROD-GAP            VALUE 'G'.
052614            88  PD-PROD-FAML           VALUE 'F'.
100518            88  PD-PROD-OTH            VALUE 'O'.
022122            88  PD-PROD-BRV            VALUE 'B'.
022122            88  PD-PROD-HOSP           VALUE 'H'.
              16  PD-MAX-ATT-AGE           PIC S999        COMP-3.
022122        16  PD-WAIT-PERIOD.
                  20  pd-wait-days         pic 99.
022122            20  PD-RET-ELIM          PIC X.
022122        16  FILLER                   PIC X.
021222*       16  PD-MIN-ISSUE-AGE         PIC S999        COMP-3.
021222*       16  PD-MAX-ISSUE-AGE         PIC S999        COMP-3.
              16  PD-MAX-TERM              PIC S999        COMP-3.
              16  PD-MAX-AMT               PIC S9(07)      COMP-3.
              16  FILLER                   PIC X.
              16  PD-PRE-EXIST-EXCL-TYPE   PIC 99.
              16  PD-EXCLUSION-PERIOD-DAYS PIC S999        COMP-3.
              16  PD-COVERAGE-ENDS-MOS     PIC S999        COMP-3.
              16  PD-ACCIDENT-ONLY-MOS     PIC S999        COMP-3.
              16  PD-CRIT-PERIOD           PIC S999        COMP-3.
              16  PD-REC-CRIT-PERIOD       PIC 99.
              16  PD-REC-CP-ALPHA  REDEFINES PD-REC-CRIT-PERIOD.
                  20  PD-RECURRING-YN      PIC X.
                  20  FILLER               PIC X.
              16  PD-RTW-MOS               PIC 99.
051414        16  PD-MAX-EXTENSION         PIC 99.
100314        16  pd-ben-pct               pic sv999 comp-3.
100314*       16  FILLER                   PIC XX.
          12  PD-1ST-YR-ADMIN-ALLOW        PIC S9(3)V99    COMP-3.
          12  PD-TERM-LIMITS OCCURS 15.
              16  PD-LOW-TERM              PIC S999        COMP-3.
              16  PD-HI-TERM               PIC S999        COMP-3.
      *  THE LOAN AMT LIMITS CORRESPOND TO THE TERM LIMITS ABOVE
          12  PD-LOAN-AMT-LIMITS OCCURS 15.
              16  PD-LOW-AMT               PIC S9(5)       COMP-3.
              16  PD-HI-AMT                PIC S9(7)       COMP-3.
          12  PD-EARN-FACTORS.
              16  FILLER OCCURS 15.
                  20  FILLER OCCURS 15.
                      24  PD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
          12  PD-PRODUCT-DESC              PIC X(80).
          12  PD-TRUNCATED                 PIC X.
          12  FILLER                       PIC X(7).
          12  PD-MAINT-INFORMATION.
              16  PD-LAST-MAINT-DT         PIC X(02).
              16  PD-LAST-MAINT-HHMMSS     PIC S9(07)      COMP-3.
              16  PD-LAST-MAINT-BY         PIC X(04).
      *                                    COPY ELCCNTL.
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
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PRODUCT-MASTER
                                CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL159' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE
           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS.
           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
           MOVE 1                      TO EMI-NUMBER-OF-LINES
           
      * EXEC CICS HANDLE CONDITION
      *        NOTOPEN    (8870-NOTOPEN)
      *        NOTFND     (8880-NOT-FOUND)
      *        PGMIDERR   (9600-PGMID-ERROR)
      *        ERROR      (9990-ABEND)
      *    END-EXEC.
      *    MOVE '"$JIL.                ! " #00004045' TO DFHEIV0
           MOVE X'22244A494C2E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303034303435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PI-CALLING-PROGRAM = 'EL150'
              MOVE SPACES                 TO ERPDEF-KEY
              MOVE PI-COMPANY-CD          TO ERPDEF-COMPANY-CD
              MOVE PI-FORM-NUMBER (1:2)   TO ERPDEF-STATE
              MOVE PI-FORM-NUMBER (3:3)   TO ERPDEF-PROD-CD
              MOVE PI-FORM-NUMBER (6:1)   TO ERPDEF-BEN-TYPE
              MOVE PI-FORM-NUMBER (7:2)   TO ERPDEF-BEN-CODE
              MOVE PI-CERT-EFF-DT         TO ERPDEF-EXP-DT
              
      * EXEC CICS READ
      *           DATASET    (ERPDEF-FILE-ID)
      *           SET        (ADDRESS OF PRODUCT-MASTER)
      *           RIDFLD     (ERPDEF-KEY)
      *           GTEQ
      *           RESP       (WS-RESPONSE)
      *       END-EXEC
      *    MOVE '&"S        G          (  N#00004059' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034303539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF RESP-NORMAL
                 AND PI-COMPANY-CD   =  PD-COMPANY-CD
                 AND PD-STATE        =  PI-FORM-NUMBER (1:2)
                 AND PD-PRODUCT-CD   =  PI-FORM-NUMBER (3:3)
                 AND PD-BEN-TYPE     =  PI-FORM-NUMBER (6:1)
                 AND PD-BEN-CODE     =  PI-FORM-NUMBER (7:2)
                 GO TO 0150-SET-PROGRAM-SAVES
              ELSE
                 MOVE 'EL159'          TO PGM-NAME
                 GO TO 9300-XCTL
              END-IF
           END-IF
           .
       0150-SET-PROGRAM-SAVES.
           IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
               IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM
               ELSE
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6
           ELSE
               GO TO 0200-RECEIVE.
           IF EIBTRNID = EL150-TRANS-ID
              MOVE 'S'                     TO MAINTI
              MOVE +1                      TO MAINTL
              MOVE DFHENTER                TO EIBAID
              MOVE PI-FORM-NUMBER (1:2)    TO STATEI
              MOVE +2                      TO STATEL
              MOVE PI-FORM-NUMBER (3:3)    TO PRODCDI
              MOVE +3                      TO PRODCDL
              MOVE PI-FORM-NUMBER (6:1)    TO BENTYPI
              MOVE +1                      TO BENTYPL
              MOVE PI-FORM-NUMBER (7:2)    TO BENCODEI
              MOVE +2                      TO BENCODEL
              MOVE PD-PROD-EXP-DT          TO DC-BIN-DATE-1
              MOVE ' '                     TO DC-OPTION-CODE
              MOVE +0                      TO DC-ELAPSED-DAYS
                                              DC-ELAPSED-MONTHS
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-A-EDIT  TO EXPDTO
                 GO TO 1000-SHOW-PROD-RECORD
              ELSE
                 MOVE LOW-VALUES           TO EXPDTO
                 GO TO 1000-SHOW-PROD-RECORD.
           IF EIBTRNID = EL1591-TRANS-ID
              MOVE PI-PROD-KEY         TO ERPDEF-KEY
              GO TO 1000-CONTINUE-SHOW
           END-IF
           MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.
           MOVE LOW-VALUES                 TO  EL159AO.
           MOVE -1                         TO  MAINTL.
           GO TO 8100-SEND-INITIAL-MAP
           .
       0200-RECEIVE.
           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR.
           IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
               MOVE LOW-VALUES             TO  EL159AI
               MOVE ER-7008                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                     TO  MAINTL
               GO TO 8200-SEND-DATAONLY.
           IF PI-PROCESSOR-ID EQUAL 'LGXX'
               NEXT SENTENCE
           ELSE
               
      * EXEC CICS READQ TS
      *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
      *            INTO   (SECURITY-CONTROL)
      *            LENGTH (SC-COMM-LENGTH)
      *            ITEM   (SC-ITEM)
      *        END-EXEC
      *    MOVE '*$II   L              ''   #00004145' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP
               MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.
           
      * EXEC CICS RECEIVE
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        INTO     (EL159AI)
      *    END-EXEC.
           MOVE LENGTH OF
            EL159AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00004153' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PFKEYL = +0
               GO TO 0300-CHECK-PFKEYS.
           IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)
               MOVE PF-VALUES (PFKEYI)     TO  EIBAID
           ELSE
               MOVE ER-0029                TO  EMI-ERROR
               GO TO 0320-INPUT-ERROR.
       0300-CHECK-PFKEYS.
           IF EIBAID EQUAL DFHPF23
               GO TO 8810-PF23.
           IF EIBAID EQUAL DFHPF24
               GO TO 9200-RETURN-MAIN-MENU.
           IF EIBAID EQUAL DFHPF12
               GO TO 9500-PF12.
           IF (MAINTL <> 0) AND (EIBAID <> DFHENTER)
               MOVE ER-0050            TO EMI-ERROR
               GO TO 0320-INPUT-ERROR.
           IF EIBAID EQUAL DFHPF1
               GO TO 5000-FIND-NEXT-PROD-RECORD.
           IF EIBAID EQUAL DFHPF2
               GO TO 5100-FIND-PREV-PROD-RECORD.
           IF EIBAID = DFHPF3
              MOVE 'EL1591'            TO PGM-NAME
              GO TO 9300-XCTL
           END-IF
080322     IF EIBAID = DFHPF4
080322        GO TO 5200-PAGE-FORWARD.
080322
080322     IF EIBAID = DFHPF5
080322        GO TO 5300-PAGE-BACKWARD.
           IF EIBAID EQUAL DFHENTER
               GO TO 0330-EDIT-DATA.
           MOVE ER-0029                    TO  EMI-ERROR.
       0320-INPUT-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE AL-UNBON                   TO  PFKEYA.
           MOVE -1                         TO  PFKEYL.
           GO TO 8200-SEND-DATAONLY.
           EJECT
       0330-EDIT-DATA.
080322     MOVE LINE-NBR (1) TO WS-LINE-NBR-CHAR.
080322     IF WS-LINE-NBR-1 = 1
080322        MOVE 1 TO PAGE-NBR
080322     ELSE
080322        MOVE 2 TO PAGE-NBR
080322     END-IF
           IF NOT DISPLAY-CAP
               MOVE 'READ'                 TO  SM-READ
               PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
               MOVE ER-0070                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               MOVE -1                     TO  MAINTL
               GO TO 8100-SEND-INITIAL-MAP.
           IF (STATEL > +0 AND
               PRODCDL > +0 AND
               BENTYPL > +0 AND
               BENCODEL > +0 AND
               EXPDTL > +0)
               NEXT SENTENCE
           ELSE
               IF (MAINTI    EQUAL   'S'   AND
                   EXPDTL    EQUAL   +0)
                   GO TO 1000-SHOW-PROD-RECORD
               ELSE
                   MOVE ER-0754        TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   MOVE -1             TO STATEL
                   MOVE AL-UABON       TO BENTYPA
                                          PRODCDA
                                          STATEA
                                          BENCODEA
                                          EXPDTA
                   GO TO 8200-SEND-DATAONLY.
           IF MAINTI EQUAL 'S'
               GO TO 1000-SHOW-PROD-RECORD.
           IF MAINTI = 'A' OR 'C' OR 'D'OR 'K'
              IF NOT MODIFY-CAP
                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
                  MOVE ER-0070             TO  EMI-ERROR
                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                  MOVE LOW-VALUES          TO  EL159AO
                  MOVE -1                  TO  MAINTL
                  GO TO 8100-SEND-INITIAL-MAP.
           IF MAINTI EQUAL 'C'
               GO TO 2000-CHANGE-PROD-RECORD.
           IF MAINTI EQUAL 'A'
               GO TO 3000-ADD-PROD-RECORD.
           IF MAINTI EQUAL 'D'
               GO TO 4000-DELETE-PROD-RECORD.
           IF MAINTI EQUAL 'K'
               GO TO 2500-COPY-PROD-RECORD.
           MOVE ER-0023                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  MAINTL.
           MOVE AL-UABON                   TO  MAINTA.
           GO TO 8200-SEND-DATAONLY.
       1000-SHOW-PROD-RECORD.
           MOVE SPACES                     TO  ERPDEF-KEY
           MOVE PI-COMPANY-CD              TO  ERPDEF-COMPANY-CD
           MOVE STATEI                     TO  ERPDEF-STATE
           MOVE PRODCDI                    TO  ERPDEF-PROD-CD
           MOVE BENTYPI                    TO  ERPDEF-BEN-TYPE
           MOVE BENCODEI                   TO  ERPDEF-BEN-CODE
           IF MAINTI = 'S'
              IF EXPDTL = +0
                 MOVE  HIGH-VALUES     TO ERPDEF-EXP-DT
                 GO TO 1000-CONTINUE-SHOW
              END-IF
           ELSE
              MOVE PI-PREV-PROD-KEY    TO ERPDEF-KEY
              GO TO 1000-CONTINUE-SHOW
           END-IF
           MOVE EXPDTI                 TO  DEEDIT-FIELD
           PERFORM 8600-DEEDIT         THRU 8600-EXIT
           IF WS-NUMVAL-OF-DEEDIT >= 999999
              MOVE HIGH-VALUES         TO ERPDEF-EXP-DT
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
              MOVE 'L'                 TO DC-OPTION-CODE
              MOVE +0                  TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
              PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-1    TO ERPDEF-EXP-DT
              ELSE
                 MOVE LOW-VALUES       TO ERPDEF-EXP-DT
              END-IF
           END-IF
080322     MOVE 1 TO R1
           .
       1000-CONTINUE-SHOW.
080322     IF PAGE-NBR = 1
080322        MOVE 1 TO R1
080322     ELSE
080322        MOVE 9 TO R1
080322     END-IF
           MOVE ERPDEF-KEY             TO PI-PROD-KEY
                                          PI-PREV-PROD-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00004299' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034323939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              MOVE ER-0073             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              MOVE AL-UABON            TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA
              GO TO 8200-SEND-DATAONLY
           END-IF
           .
       2000-CHANGE-PROD-RECORD.
           IF STATEL > +0
              MOVE STATEI              TO PI-PK-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO PI-PK-PROD-CD
           END-IF
           IF BENTYPL > +0
              MOVE BENTYPI             TO PI-BEN-TYPE
           END-IF
           IF BENCODEL > +0
              MOVE BENCODEI            TO PI-BEN-CODE
           END-IF
           IF EXPDTL > +0
              MOVE EXPDTI              TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT      THRU 8600-EXIT
              IF WS-NUMVAL-OF-DEEDIT >= 999999
                 MOVE HIGH-VALUES      TO WS-EXP-DT
                                          PI-EXP-DT
              ELSE
                 STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                    DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                 END-STRING
                 MOVE 'L'              TO DC-OPTION-CODE
                 MOVE +0               TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
                 PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
                 MOVE DC-BIN-DATE-1    TO WS-EXP-DT
                                          PI-EXP-DT
              END-IF
           END-IF
           IF PI-PROD-KEY NOT = PI-PREV-PROD-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        UPDATE
      *        RESP       (WS-RESPONSE)
      *    END-EXEC
      *    MOVE '&"S        EU         (  N#00004360' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034333630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-0073             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              MOVE AL-UABON            TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA
              GO TO 8200-SEND-DATAONLY
           END-IF
           IF PD-LAST-MAINT-BY     NOT = PI-UPDATE-BY OR
              PD-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS
               
      * EXEC CICS UNLOCK
      *            DATASET   (ERPDEF-FILE-ID)
      *        END-EXEC
      *    MOVE '&*                    #   #00004382' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE ER-0068                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 1000-SHOW-PROD-RECORD.
           MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.
           MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.
           MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.
           IF PDESCL > +0
              MOVE PDESCI              TO PD-PRODUCT-DESC
           END-IF
           IF AFA-LEN > +0
              MOVE AFA-AMT-IN          TO PD-1ST-YR-ADMIN-ALLOW
           END-IF
080322     IF PAGE-NBR = 1
080322        MOVE 1 TO R1
080322     ELSE
080322        MOVE 9 TO R1
080322     END-IF
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF PROD-CODE-LEN (M1) > +0
080322           MOVE PROD-CODE (M1)   TO PD-PROD-CODE (R1)
              END-IF
              IF MAX-ATT-AGE-LEN (M1) > +0
080322           MOVE MAX-ATT-AGE (M1) TO PD-MAX-ATT-AGE (R1)
              END-IF
080322*       IF MIN-AGE-LEN (M1) > +0
080322*          MOVE MIN-AGE (M1)     TO PD-MIN-ISSUE-AGE (R1)
080322*       END-IF
080322        IF WAIT-PR-LEN (M1) > +0
080322           MOVE WAIT-PR (M1)     TO PD-WAIT-PERIOD (R1)
080322        END-IF
              IF MAX-TERM-LEN (M1) > +0
080322           MOVE MAX-TERM (M1)    TO PD-MAX-TERM (R1)
              END-IF
              IF MAX-AMT-LEN (M1) > +0
080322           MOVE MAX-AMT-IN (M1)  TO PD-MAX-AMT (R1)
              END-IF
100314        IF ben-pct-LEN (M1) > +0
100314           MOVE ben-pct-IN (M1)  TO PD-ben-pct (R1)
100314        END-IF
              IF PRE-EXIST-LEN (M1) > +0
080322           MOVE PRE-EXIST (M1)   TO PD-PRE-EXIST-EXCL-TYPE (R1)
              END-IF
              IF EXCL-PER-LEN (M1) > +0
080322           MOVE EXCL-PERIOD (M1) TO PD-EXCLUSION-PERIOD-DAYS (R1)
              END-IF
              IF COV-ENDS-LEN (M1) > +0
080322           MOVE COV-ENDS (M1)    TO PD-COVERAGE-ENDS-MOS (R1)
              END-IF
              IF ACC-PER-ENDS-LEN (M1) > +0
080322           MOVE ACC-PER-ENDS (M1) TO PD-ACCIDENT-ONLY-MOS (R1)
              END-IF
              IF CRIT-PER-LEN (M1) > +0
080322           MOVE CRIT-PER (M1) TO PD-CRIT-PERIOD (R1)
              END-IF
              IF REC-CP-LEN (M1) > +0
                 IF REC-CRIT-PER (M1) NUMERIC
080322              MOVE REC-CRIT-PER-N (M1) TO PD-REC-CRIT-PERIOD (R1)
                 ELSE
080322              MOVE REC-CRIT-PER (M1) TO PD-REC-CP-ALPHA (R1)
                 END-IF
              END-IF
              IF RTW-MOS-LEN (M1) > +0
080322           MOVE RTW-MOS (M1)     TO PD-RTW-MOS (R1)
              END-IF
              IF MAX-EXT-LEN (M1) > +0
080322           MOVE MAX-EXT (M1)     TO PD-MAX-EXTENSION (R1)
              END-IF
080322        ADD 1 TO R1
           END-PERFORM
           IF TRUNCL > ZEROS
              MOVE TRUNCI              TO PD-TRUNCATED
           END-IF
           .
       2000-CONTINUE-CHANGE.
           
      * EXEC CICS REWRITE
      *        DATASET   (ERPDEF-FILE-ID)
      *        FROM      (PRODUCT-MASTER)
      *    END-EXEC.
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00004460' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE PD-CONTROL-PRIMARY     TO ERPDEF-KEY
           GO TO 1000-CONTINUE-SHOW
           .
       2500-COPY-PROD-RECORD.
           MOVE EXPDTI                 TO  DEEDIT-FIELD.
           PERFORM 8600-DEEDIT THRU 8600-EXIT.
           IF WS-NUMVAL-OF-DEEDIT >= 999999
              MOVE HIGH-VALUES         TO  WS-EXP-DT
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
              MOVE 'L'                 TO  DC-OPTION-CODE
              MOVE +0                  TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
              MOVE DC-BIN-DATE-1       TO  WS-EXP-DT
           END-IF
      ***********  need to edit the key here
      **** PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.
           MOVE PI-PROD-KEY            TO ERPDEF-KEY
           
      * EXEC CICS READ
      *        DATASET    (ERPDEF-FILE-ID)
      *        SET        (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD     (ERPDEF-KEY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
      *    MOVE '&"S        E          (  N#00004489' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-0073             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              MOVE AL-UABON            TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PI-PROCESSOR-ID        TO PD-LAST-MAINT-BY
           MOVE EIBTIME                TO PD-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO PD-LAST-MAINT-DT
           IF STATEL > +0
              MOVE STATEI              TO PD-STATE
                                          PI-PK-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO PD-PRODUCT-CD
                                          PI-PK-PROD-CD
           END-IF
           IF BENTYPL > +0
              MOVE BENTYPI             TO PD-BEN-TYPE
                                          PI-BEN-TYPE
           END-IF
           IF BENCODEL > +0
              MOVE BENCODEI            TO PD-BEN-CODE
                                          PI-BEN-CODE
           END-IF
           MOVE EXPDTI                 TO DEEDIT-FIELD
           PERFORM 8600-DEEDIT THRU 8600-EXIT
           IF WS-NUMVAL-OF-DEEDIT >= 999999
               MOVE HIGH-VALUES            TO  PD-PROD-EXP-DT
                                               PI-EXP-DT
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
               MOVE 'L'                    TO  DC-OPTION-CODE
               MOVE +0                     TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
               IF NO-CONVERSION-ERROR
                   MOVE DC-BIN-DATE-1      TO  PD-PROD-EXP-DT
                                               PI-EXP-DT
                ELSE
                   MOVE LOW-VALUES         TO  PD-PROD-EXP-DT
                                               PI-EXP-DT.
           
      * EXEC CICS WRITE
      *        DATASET    (ERPDEF-FILE-ID)
      *        FROM       (PRODUCT-MASTER)
      *        RIDFLD     (PD-CONTROL-PRIMARY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00004546' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303034353436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 PD-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              MOVE ER-0132             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           MOVE PD-CONTROL-PRIMARY     TO ERPDEF-KEY
           GO TO 1000-CONTINUE-SHOW
           .
       3000-ADD-PROD-RECORD.
           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.
           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.
           
      * EXEC CICS GETMAIN
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        LENGTH    (ERPDEF-LENGTH)
      *        INITIMG   (GETMAIN-SPACE)
      *    END-EXEC.
      *    MOVE ',"IL                  $   #00004567' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPDEF-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           INITIALIZE PRODUCT-MASTER
           MOVE 'PD'                       TO  PD-RECORD-ID
           MOVE PI-COMPANY-CD              TO  PD-COMPANY-CD
           MOVE STATEI                     TO  PD-STATE
                                               PI-PK-STATE
           MOVE PRODCDI                    TO  PD-PRODUCT-CD
                                               PI-PK-PROD-CD
           MOVE BENTYPI                    TO  PD-BEN-TYPE
                                               PI-BEN-TYPE
           MOVE BENCODEI                   TO  PD-BEN-CODE
                                               PI-BEN-CODE
           MOVE EXPDTI                     TO  DEEDIT-FIELD.
           PERFORM 8600-DEEDIT THRU 8600-EXIT.
           IF WS-NUMVAL-OF-DEEDIT >= 999999
               MOVE HIGH-VALUES            TO  PD-PROD-EXP-DT
                                               PI-EXP-DT
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
               MOVE 'L'                    TO  DC-OPTION-CODE
               MOVE +0                     TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
               IF NO-CONVERSION-ERROR
                   MOVE DC-BIN-DATE-1      TO  PD-PROD-EXP-DT
                                               PI-EXP-DT
                ELSE
                   MOVE LOW-VALUES         TO  PD-PROD-EXP-DT
                                               PI-EXP-DT.
           IF PDESCL > +0
              MOVE PDESCI              TO PD-PRODUCT-DESC
           END-IF
           IF AFA-LEN > +0
              MOVE AFA-AMT-IN          TO PD-1ST-YR-ADMIN-ALLOW
           END-IF
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF PROD-CODE-LEN (M1) > +0
                 MOVE PROD-CODE (M1)   TO PD-PROD-CODE (M1)
              END-IF
              IF MAX-ATT-AGE-LEN (M1) > +0
                 MOVE MAX-ATT-AGE (M1) TO PD-MAX-ATT-AGE (M1)
              END-IF
080322*       IF MIN-AGE-LEN (M1) > +0
080322*          MOVE MIN-AGE (M1)     TO PD-MIN-ISSUE-AGE (M1)
080322*       END-IF
080322        IF WAIT-PR-LEN (M1) > +0
080322           MOVE WAIT-PR (M1)     TO PD-WAIT-PERIOD (M1)
080322        END-IF
              IF MAX-TERM-LEN (M1) > +0
                 MOVE MAX-TERM (M1)    TO PD-MAX-TERM (M1)
              END-IF
              IF MAX-AMT-LEN (M1) > +0
                 MOVE MAX-AMT-IN (M1)  TO PD-MAX-AMT (M1)
              END-IF
100314        IF ben-pct-LEN (M1) > +0
100314           MOVE ben-pct-IN (M1)  TO PD-ben-pct (M1)
100314        END-IF
              IF PRE-EXIST-LEN (M1) > +0
                 MOVE PRE-EXIST (M1)   TO PD-PRE-EXIST-EXCL-TYPE (M1)
              END-IF
              IF EXCL-PER-LEN (M1) > +0
                 MOVE EXCL-PERIOD (M1) TO PD-EXCLUSION-PERIOD-DAYS (M1)
              END-IF
              IF COV-ENDS-LEN (M1) > +0
                 MOVE COV-ENDS (M1)    TO PD-COVERAGE-ENDS-MOS (M1)
              END-IF
              IF ACC-PER-ENDS-LEN (M1) > +0
                 MOVE ACC-PER-ENDS (M1) TO PD-ACCIDENT-ONLY-MOS (M1)
              END-IF
              IF CRIT-PER-LEN (M1) > +0
                 MOVE CRIT-PER (M1) TO PD-CRIT-PERIOD (M1)
              END-IF
              IF REC-CP-LEN (M1) > +0
                 IF REC-CRIT-PER (M1) NUMERIC
                    MOVE REC-CRIT-PER-N (M1) TO PD-REC-CRIT-PERIOD (m1)
                 ELSE
                    MOVE REC-CRIT-PER (M1) TO PD-REC-CP-ALPHA (M1)
                 END-IF
              END-IF
              IF RTW-MOS-LEN (M1) > +0
                 MOVE RTW-MOS (M1)     TO PD-RTW-MOS (M1)
              END-IF
              IF MAX-EXT-LEN (M1) > +0
                 MOVE MAX-EXT (M1)     TO PD-MAX-EXTENSION (M1)
              END-IF
           END-PERFORM
           MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.
           MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.
           MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.
           .
       3005-WRITE-ERPDEF-FILE.
           
      * EXEC CICS WRITE
      *        DATASET    (ERPDEF-FILE-ID)
      *        FROM       (PRODUCT-MASTER)
      *        RIDFLD     (PD-CONTROL-PRIMARY)
      *        RESP       (WS-RESPONSE)
      *    END-EXEC.
           MOVE LENGTH OF
            PRODUCT-MASTER
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00004665' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303034363635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 PRODUCT-MASTER, 
                 DFHEIV11, 
                 PD-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF NOT RESP-NORMAL
              MOVE ER-0132             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF
           MOVE PD-CONTROL-PRIMARY     TO PI-PROD-KEY
                                          PI-PREV-PROD-KEY
                                          ERPDEF-KEY
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE LOW-VALUES                 TO  EL159AO
           MOVE -1                         TO  MAINTL
           GO TO 1000-CONTINUE-SHOW
           .
       4000-DELETE-PROD-RECORD.
           MOVE SPACES                     TO ERPDEF-KEY
           MOVE PI-COMPANY-CD              TO  ERPDEF-COMPANY-CD.
           MOVE STATEI                     TO ERPDEF-STATE
           MOVE PRODCDI                    TO ERPDEF-PROD-CD
           MOVE BENTYPI                    TO  ERPDEF-BEN-TYPE
           MOVE BENCODEI                   TO  ERPDEF-BEN-CODE
           MOVE EXPDTI                     TO  DEEDIT-FIELD.
           PERFORM 8600-DEEDIT THRU 8600-EXIT.
           IF WS-NUMVAL-OF-DEEDIT >= 999999
               MOVE HIGH-VALUES            TO  ERPDEF-EXP-DT
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
               MOVE 'L'                    TO  DC-OPTION-CODE
               MOVE +0                     TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
               IF NO-CONVERSION-ERROR
                   MOVE DC-BIN-DATE-1      TO  ERPDEF-EXP-DT
               ELSE
                   MOVE LOW-VALUES         TO  ERPDEF-EXP-DT.
           
      * EXEC CICS READ
      *        DATASET   (ERPDEF-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ERPDEF-KEY)
      *        UPDATE
      *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004709' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PD-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR
              PD-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS
               
      * EXEC CICS UNLOCK
      *            DATASET   (ERPDEF-FILE-ID)
      *            END-EXEC
      *    MOVE '&*                    #   #00004717' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
               MOVE ER-0068                TO  EMI-ERROR
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
               GO TO 1000-SHOW-PROD-RECORD.
           
      * EXEC CICS DELETE
      *        DATASET   (ERPDEF-FILE-ID)
      *        END-EXEC.
      *    MOVE '&(                    &   #00004723' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE ER-0000                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE LOW-VALUES                 TO  EL159AO.
           MOVE -1                         TO  MAINTL.
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       5000-FIND-NEXT-PROD-RECORD.
           MOVE PI-COMPANY-CD              TO  ERPDEF-KEY
           IF STATEL > +0
              MOVE STATEI              TO ERPDEF-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO ERPDEF-PROD-CD
           END-IF
           IF BENTYPL > +0
               MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.
           IF BENCODEL > +0
               MOVE BENCODEI               TO  ERPDEF-BEN-CODE.
           IF EXPDTL IS GREATER THAN +0
               MOVE EXPDTI                 TO  DEEDIT-FIELD
               PERFORM 8600-DEEDIT THRU 8600-EXIT
               IF WS-NUMVAL-OF-DEEDIT >= 999999
                   MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT
               ELSE
                 STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                    DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                 END-STRING
                  MOVE 'L'                    TO  DC-OPTION-CODE
                   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
                   IF NO-CONVERSION-ERROR
                       MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT
                   ELSE
                       MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.
           
      * EXEC CICS HANDLE CONDITION
      *        ENDFILE (5000-UNSUCCESSFUL-SEARCH)
      *    END-EXEC.
      *    MOVE '"$''                   ! # #00004759' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303034373539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS STARTBR
      *        DATASET   (ERPDEF-FILE-ID)
      *        RIDFLD    (ERPDEF-KEY)
      *        GTEQ
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004762' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5000-READNEXT-LOOP.
           
      * EXEC CICS READNEXT
      *        DATASET   (ERPDEF-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ERPDEF-KEY)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00004768' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
               GO TO 5000-UNSUCCESSFUL-SEARCH.
           IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
              display ' key = prev key '
               GO TO 5000-READNEXT-LOOP.
           MOVE ERPDEF-KEY             TO PI-PROD-KEY
           PERFORM 5000-END-BROWSE
080322     MOVE 1 TO R1
           GO TO 7000-BUILD-OUTPUT-MAP
           .
       5000-END-BROWSE.
           
      * EXEC CICS ENDBR
      *        DATASET   (ERPDEF-FILE-ID)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004784' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5000-UNSUCCESSFUL-SEARCH.
           PERFORM 5000-END-BROWSE
           MOVE -1                         TO  PFKEYL
           MOVE ER-0130                    TO  EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           GO TO 1000-SHOW-PROD-RECORD
           .
       5100-FIND-PREV-PROD-RECORD.
           MOVE PI-PREV-PROD-KEY           TO  ERPDEF-KEY
           IF STATEL > +0
              MOVE STATEI              TO ERPDEF-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO ERPDEF-PROD-CD
           END-IF
           IF BENTYPL > +0
               MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.
           IF BENCODEL > +0
               MOVE BENCODEI               TO  ERPDEF-BEN-CODE.
           IF EXPDTL IS GREATER THAN +0
               MOVE EXPDTI                 TO  DEEDIT-FIELD
               PERFORM 8600-DEEDIT THRU 8600-EXIT
               IF WS-NUMVAL-OF-DEEDIT >= 999999
                   MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT
               ELSE
                 STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                    DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                 END-STRING
                  MOVE 'L'                    TO  DC-OPTION-CODE
                   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
                   IF NO-CONVERSION-ERROR
                       MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT
                   ELSE
                       MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.
           
      * EXEC CICS HANDLE CONDITION
      *        ENDFILE (5100-UNSUCCESSFUL-SEARCH)
      *    END-EXEC.
      *    MOVE '"$''                   ! $ #00004821' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303034383231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS STARTBR
      *        DATASET   (ERPDEF-FILE-ID)
      *        RIDFLD    (ERPDEF-KEY)
      *        GTEQ
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00004824' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5100-READPREV-LOOP.
           
      * EXEC CICS READPREV
      *        DATASET   (ERPDEF-FILE-ID)
      *        SET       (ADDRESS OF PRODUCT-MASTER)
      *        RIDFLD    (ERPDEF-KEY)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00004830' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPDEF-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD
               GO TO 5100-UNSUCCESSFUL-SEARCH.
           IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
               GO TO 5100-READPREV-LOOP.
           MOVE ERPDEF-KEY             TO PI-PROD-KEY
080322     MOVE 1 TO R1
           GO TO 7000-BUILD-OUTPUT-MAP.
       5100-END-BROWSE.
           
      * EXEC CICS ENDBR
      *        DATASET   (ERPDEF-FILE-ID)
      *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004843' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ERPDEF-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       5100-UNSUCCESSFUL-SEARCH.
           PERFORM 5100-END-BROWSE.
           MOVE -1                         TO  PFKEYL.
           MOVE ER-0131                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 1000-SHOW-PROD-RECORD.
           .
080322 5200-PAGE-FORWARD.
080322     IF PROD-CODE (8) NOT > SPACES
080322        MOVE ER-1164              TO EMI-ERROR
080322        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080322        MOVE -1                   TO MAINTL
080322        MOVE AL-UABON             TO MAINTA
080322        GO TO 8200-SEND-DATAONLY
080322     END-IF
080322     MOVE 9 TO R1
080322     MOVE PI-PROD-KEY            TO ERPDEF-KEY
080322
080322     GO TO 1000-CONTINUE-SHOW.
080322 5300-PAGE-BACKWARD.
080322     MOVE 1 TO PAGE-NBR
080322
080322     MOVE LINE-NBR (1) TO WS-LINE-NBR-CHAR.
080322     IF WS-LINE-NBR-1 = 1
080322        MOVE ER-0067              TO EMI-ERROR
080322        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080322        MOVE -1                   TO MAINTL
080322        MOVE AL-UABON             TO MAINTA
080322        GO TO 8200-SEND-DATAONLY
080322     END-IF
080322     MOVE PI-PROD-KEY            TO ERPDEF-KEY
080322
080322     GO TO 1000-CONTINUE-SHOW.
       6000-EDIT-INPUT-DATA.
           IF MAINTI = 'A'
              AND +0 = BENTYPL OR BENCODEL OR EXPDTL
                    OR STATEL OR PRODCDL
              MOVE ER-0144             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO BENTYPL
              MOVE AL-UABON            TO BENTYPA
              GO TO 6000-EXIT
           END-IF
           IF MAINTI = 'A'
              IF STATEL > +0
                 MOVE PI-COMPANY-ID    TO ELCNTL-COMPANY-ID
                 MOVE '3'              TO ELCNTL-RECORD-TYPE
                 MOVE STATEI           TO ELCNTL-ACCESS
                 MOVE +0               TO ELCNTL-SEQUENCE-NO
                 
      * EXEC CICS READ
      *             DATASET   (ELCNTL-FILE-ID)
      *             RIDFLD    (ELCNTL-KEY)
      *             SET       (ADDRESS OF CONTROL-FILE)
      *             RESP      (WS-RESPONSE)
      *          END-EXEC
      *    MOVE '&"S        E          (  N#00004895' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034383935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
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
                 IF NOT RESP-NORMAL
                    MOVE ER-0144       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    MOVE -1            TO STATEL
                    MOVE AL-UABON      TO STATEA
                 END-IF
              END-IF
              IF BENTYPL > +0
                 IF BENTYPI = PI-LIFE-OVERRIDE-L1 OR PI-AH-OVERRIDE-L1
                    CONTINUE
                 ELSE
                    MOVE ER-0713       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    MOVE -1            TO BENTYPL
                    MOVE AL-UABON      TO BENTYPA
                 END-IF
              END-IF
              IF BENCODEL > +0
                 MOVE SPACES           TO ELCNTL-KEY
                 IF BENTYPI = PI-LIFE-OVERRIDE-L1
                    MOVE '4'           TO ELCNTL-RECORD-TYPE
                 ELSE
                    MOVE '5'           TO ELCNTL-RECORD-TYPE
                 END-IF
                 MOVE BENCODEI         TO ELCNTL-BENE-CD
                                          WS-BENE-CODE
                 MOVE PI-COMPANY-ID    TO ELCNTL-COMPANY-ID
                 MOVE ZEROS            TO ELCNTL-SEQUENCE-NO
                 PERFORM 7100-READ-BENEFIT THRU 7199-EXIT
                 IF CNTL-RECORD-FOUND
                    MOVE AL-UANON      TO BENCODEA
                 ELSE
                    MOVE ER-7123       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    MOVE -1            TO BENCODEL
                    MOVE AL-UABON      TO BENCODEA
                 END-IF
              END-IF
              IF EXPDTL > +0
                 MOVE EXPDTI           TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT >= 999999
                    MOVE '99/99/9999'  TO EXPDTO
                    MOVE AL-UANON      TO EXPDTA
                 ELSE
                    STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2)
                       WS-NUMVAL (6:2)
                       DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                    END-STRING
                    MOVE 'L'           TO DC-OPTION-CODE
                    MOVE +0            TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
                    PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
                    IF NO-CONVERSION-ERROR
                       MOVE DC-GREG-DATE-A-EDIT TO  EXPDTO
                       MOVE AL-UANON   TO EXPDTA
                    ELSE
                       MOVE ER-0705    TO EMI-ERROR
                       perform 9900-ERROR-FORMAT thru 9900-exit
                       MOVE -1         TO EXPDTL
                       MOVE AL-UABON   TO EXPDTA
                    END-IF
                 END-IF
              END-IF
           END-IF
           IF PDESCL > +0
              MOVE AL-UANON            TO PDESCA
           END-IF
           IF AFA-LEN > +0
              MOVE AFA-AMT             TO DEEDIT-FIELD
              PERFORM 8500-DEEDIT      THRU 8500-EXIT
              MOVE WS-999V99-OF-DEEDIT TO AFA-AMT-IN
              MOVE AL-UNNON            TO AFA-ATTRB
           END-IF
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This was added so the user can remove an entire occurance ***
      ***  if they space out the prod code then we will zero the     ***
      ***  rest of the stuff out.                                    ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF PROD-CODE-LEN (M1) > +0
                 if prod-code (m1) = spaces
                    move +1            to max-att-age-len  (m1)
080322*                                   min-age-len      (m1)
080322                                    WAIT-PR-len      (m1)
080322                                    max-amt-len      (m1)
100314                                    ben-pct-len      (m1)
080322                                    EXCL-PER-LEN     (M1)
080322                                    cov-ends-len     (m1)
080322                                    acc-per-ends-len (m1)
080322                                    crit-per-len     (m1)
080322                                    rec-cp-len       (m1)
080322                                    rtw-mos-len      (m1)
080322                                    max-ext-len      (m1)
080322              move zeros         to max-att-age  (m1)
080322*                                   min-age      (m1)
080322                                    WAIT-PR      (m1)
080322                                    max-amt-in   (m1)
100314                                    ben-pct-in   (m1)
080322                                    EXCL-PERiod  (M1)
080322                                    cov-ends     (m1)
080322                                    acc-per-ends (m1)
080322                                    crit-per     (m1)
080322                                    rec-crit-per (m1)
080322                                    rtw-mos      (m1)
080322                                    max-ext      (m1)
                 end-if
                 IF PROD-CODE (M1) = 'P' OR 'L' OR 'A'
110618                            OR 'I' OR 'G' or ' ' or 'F' OR 'O'
080322                            OR 'B' OR 'H'
                    MOVE AL-UANON      TO PROD-CODE-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO PROD-CODE-LEN (M1)
                    MOVE AL-UABON      TO PROD-CODE-ATTRB (M1)
                 END-IF
              END-IF
              IF MAX-ATT-AGE-LEN (M1) > +0
                 MOVE MAX-ATT-AGE (M1)  TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO MAX-ATT-AGE (M1)
                    MOVE AL-UANON      TO MAX-ATT-AGE-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-ATT-AGE-LEN (M1)
                    MOVE AL-UABON      TO MAX-ATT-AGE-ATTRB (M1)
                 END-IF
              END-IF
080322*       IF MIN-AGE-LEN (M1) > +0
080322*          MOVE MIN-AGE (M1)      TO DEEDIT-FIELD
080322*          PERFORM 8600-DEEDIT   THRU 8600-EXIT
080322*          IF WS-NUMVAL-OF-DEEDIT NUMERIC
080322*             MOVE WS-NUMVAL-OF-DEEDIT TO MIN-AGE (M1)
080322*             MOVE AL-UANON      TO MIN-AGE-ATTRB (M1)
080322*          ELSE
080322*             MOVE ER-9999       TO EMI-ERROR
080322*             perform 9900-ERROR-FORMAT thru 9900-exit
080322*             MOVE -1            TO MIN-AGE-LEN (M1)
080322*             MOVE AL-UABON      TO MIN-AGE-ATTRB (M1)
080322*          END-IF
080322*       END-IF
080322*       IF WAIT-PR-LEN (M1) > +0
080322*          MOVE WAIT-PR (M1)      TO DEEDIT-FIELD
080322*          PERFORM 8600-DEEDIT   THRU 8600-EXIT
080322*          IF WS-NUMVAL-OF-DEEDIT NUMERIC
080322*             MOVE WS-NUMVAL-OF-DEEDIT TO WAIT-PR (M1)
080322*             MOVE AL-UANON      TO WAIT-PR-ATTRB (M1)
080322*          ELSE
080322*             MOVE ER-9999       TO EMI-ERROR
080322*             perform 9900-ERROR-FORMAT thru 9900-exit
080322*             MOVE -1            TO WAIT-PR-LEN (M1)
080322*             MOVE AL-UABON      TO WAIT-PR-ATTRB (M1)
080322*          END-IF
080322*       END-IF
              IF MAX-TERM-LEN (M1) > +0
                 MOVE MAX-TERM (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO MAX-TERM (M1)
                    MOVE AL-UANON      TO MAX-TERM-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-TERM-LEN (M1)
                    MOVE AL-UABON      TO MAX-TERM-ATTRB (M1)
                 END-IF
              END-IF
              IF MAX-AMT-LEN (M1) > +0
                 MOVE MAX-AMT-IN (M1)  TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT
                                       TO MAX-AMT-IN (M1)
                    MOVE AL-UANON      TO MAX-AMT-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-AMT-LEN (M1)
                    MOVE AL-UABON      TO MAX-AMT-ATTRB (M1)
                 END-IF
              END-IF
100314        IF ben-pct-LEN (M1) > +0
100314           MOVE ben-pct-IN (M1)  TO DEEDIT-FIELD
100314           compute ws-9v999-of-deedit =
100314              function numval(deedit-field)
100314           IF WS-9V999-OF-DEEDIT NUMERIC
100314              MOVE WS-9v999-OF-DEEDIT
100314                                 TO ben-pct-IN (M1)
100314              MOVE AL-UANON      TO ben-pct-ATTRB (M1)
100314           ELSE
100314              MOVE ER-7132       TO EMI-ERROR
100314              perform 9900-ERROR-FORMAT thru 9900-exit
100314              MOVE -1            TO ben-pct-LEN (M1)
100314              MOVE AL-UABON      TO ben-pct-ATTRB (M1)
100314           END-IF
100314        END-IF
              IF PRE-EXIST-LEN (M1) > +0
                 MOVE AL-UANON         TO PRE-EXIST-ATTRB (M1)
              END-IF
              IF EXCL-PER-LEN (M1) > +0
                 MOVE EXCL-PERIOD (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO EXCL-PERIOD (M1)
                    MOVE AL-UANON      TO EXCL-PER-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO EXCL-PER-LEN (M1)
                    MOVE AL-UABON      TO EXCL-PER-ATTRB (M1)
                 END-IF
              END-IF
              IF COV-ENDS-LEN (M1) > +0
                 MOVE COV-ENDS (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO COV-ENDS (M1)
                    MOVE AL-UANON      TO COV-ENDS-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO COV-ENDS-LEN (M1)
                    MOVE AL-UABON      TO COV-ENDS-ATTRB (M1)
                 END-IF
              END-IF
              IF ACC-PER-ENDS-LEN (M1) > +0
                 MOVE ACC-PER-ENDS (M1) TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO ACC-PER-ENDS-OUT (M1)
                    MOVE AL-UANON      TO ACC-PER-ENDS-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO ACC-PER-ENDS-LEN (M1)
                    MOVE AL-UABON      TO ACC-PER-ENDS-ATTRB (M1)
                 END-IF
              END-IF
              IF CRIT-PER-LEN (M1) > +0
                 MOVE CRIT-PER (M1)    TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO CRIT-PER-OUT (M1)
                    MOVE AL-UANON      TO CRIT-PER-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO CRIT-PER-LEN (M1)
                    MOVE AL-UABON      TO CRIT-PER-ATTRB (M1)
                 END-IF
              END-IF
              IF REC-CP-LEN (M1) > +0
                 EVALUATE TRUE
                    WHEN REC-CRIT-PER (M1) = ' Y' OR ' N'
                       MOVE REC-CRIT-PER (M1) (2:1)
                                       TO REC-CRIT-PER (M1) (1:2)
                       MOVE SPACES     TO REC-CRIT-PER (M1) (2:1)
                       MOVE AL-UANON   TO REC-CP-ATTRB (M1)
                    WHEN REC-CRIT-PER (M1) = 'Y ' OR 'N ' OR '  '
                       MOVE AL-UANON   TO REC-CP-ATTRB (M1)
                    WHEN OTHER
                       MOVE REC-CRIT-PER (M1)
                                       TO DEEDIT-FIELD
                       PERFORM 8600-DEEDIT
                                       THRU 8600-EXIT
                       IF WS-NUMVAL-OF-DEEDIT NUMERIC
                          MOVE WS-NUMVAL-OF-DEEDIT
                                       TO REC-CRIT-PER-N (M1)
                          MOVE AL-UANON TO REC-CP-ATTRB (M1)
                       ELSE
                          MOVE ER-9999 TO EMI-ERROR
                          PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
                          MOVE -1      TO REC-CP-LEN (M1)
                          MOVE AL-UABON TO REC-CP-ATTRB (M1)
                       END-IF
                 END-EVALUATE
              END-IF
              IF RTW-MOS-LEN (M1) > +0
                 MOVE RTW-MOS  (M1)    TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO RTW-MOS-OUT (M1)
                    MOVE AL-UANON      TO RTW-MOS-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO RTW-MOS-LEN (M1)
                    MOVE AL-UABON      TO RTW-MOS-ATTRB (M1)
                 END-IF
              END-IF
              IF MAX-EXT-LEN (M1) > +0
                 MOVE MAX-EXT  (M1)    TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO MAX-EXT-OUT  (M1)
                    MOVE AL-UANON      TO MAX-EXT-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-EXT-LEN (M1)
                    MOVE AL-UABON      TO MAX-EXT-ATTRB (M1)
                 END-IF
              END-IF
           END-PERFORM
           IF TRUNCL > ZEROS
              IF TRUNCI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 perform 9900-ERROR-FORMAT thru 9900-exit
                 MOVE -1               TO TRUNCL
                 MOVE AL-UABON         TO TRUNCA
              END-IF
           END-IF
           .
       6000-EXIT.
           EXIT.
           EJECT
       7000-BUILD-OUTPUT-MAP.
           MOVE LOW-VALUES                 TO  EL159AO
           MOVE PI-COMPANY-CD              TO  PI-PREV-COMPANY-CD
           MOVE PD-STATE                   TO  STATEO
                                               PI-PREV-STATE
           MOVE PD-PRODUCT-CD              TO  PRODCDO
                                               PI-PREV-PROD-CD
           MOVE PD-BEN-TYPE                TO  BENTYPO
                                               PI-PREV-BEN-TYPE
           MOVE PD-BEN-CODE                TO  BENCODEO
                                               PI-PREV-BEN-CODE
           IF PD-PROD-EXP-DT = HIGH-VALUES
               MOVE '99/99/9999'           TO  EXPDTO
               MOVE HIGH-VALUES            TO  PI-PREV-EXP-DT
           ELSE
               MOVE PD-PROD-EXP-DT         TO  DC-BIN-DATE-1
                                               PI-PREV-EXP-DT
               MOVE ' '                    TO  DC-OPTION-CODE
               MOVE +0                     TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
               IF NO-CONVERSION-ERROR
                   MOVE DC-GREG-DATE-A-EDIT TO EXPDTO
               ELSE
                   MOVE LOW-VALUES         TO  EXPDTO.
           MOVE PD-PRODUCT-DESC        TO PDESCO
           MOVE PD-1ST-YR-ADMIN-ALLOW  TO AFA-AMT-OUT
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
080322        IF R1 < 10
080322           MOVE R1                   TO WS-LINE-NBR-1
080322           MOVE ')'                  TO WS-LINE-NBR-P
080322        ELSE
080322           IF R1 > 11
                    MOVE LOW-VALUES           TO EL159-PROD-TABLE (M1)
080322              MOVE SPACES               TO LINE-NBR (M1)
                                                 WS-LINE-NBR-CHAR
080322              MOVE AL-SADON             TO PROD-CODE-ATTRB (M1)
080322                                           MAX-ATT-AGE-ATTRB (M1)
080322*                                          MIN-AGE-ATTRB  (M1)
080322                                           WAIT-PR-ATTRB  (M1)
080322                                           MAX-TERM-ATTRB (M1)
080322                                           ben-pct-attrb  (M1)
080322                                           MAX-AMT-ATTRB  (M1)
080322                                           PRE-EXIST-ATTRB(M1)
080322                                           EXCL-PER-ATTRB (M1)
080322                                           COV-ENDS-ATTRB (M1)
080322                                           ACC-PER-ENDS-ATTRB (M1)
080322                                           CRIT-PER-ATTRB (M1)
080322                                           REC-CP-ATTRB   (M1)
080322                                           RTW-MOS-ATTRB  (M1)
                                                 MAX-EXT-ATTRB  (M1)
080322           ELSE
080322              MOVE R1                   TO WS-LINE-NBR-2
080322              MOVE ')'                  TO WS-LINE-NBR-P2
080322           END-IF
080322        END-IF
080322        MOVE WS-LINE-NBR-CHAR        TO LINE-NBR (M1)
              IF PD-PROD-CODE (R1) NOT = SPACES
080322          AND R1 NOT > 11
080322           MOVE PD-PROD-CODE (R1)       TO PROD-CODE (M1)
080322           MOVE PD-MAX-ATT-AGE (R1)     TO MAX-ATT-AGE (M1)
080322*          MOVE PD-MIN-ISSUE-AGE (R1)   TO MIN-AGE (M1)
080322           MOVE PD-WAIT-PERIOD (R1)     TO WAIT-PR (M1)
080322           MOVE PD-MAX-TERM (R1)        TO MAX-TERM (M1)
080322           MOVE PD-MAX-AMT (R1)         TO MAX-AMT-OUT (M1)
100314           if pd-ben-pct (R1) not numeric
100314              move zeros to pd-ben-pct (R1)
100314           end-if
100314           move pd-ben-pct (R1)         to ben-pct-out (m1)
080322           MOVE PD-PRE-EXIST-EXCL-TYPE (R1) TO PRE-EXIST (M1)
080322           MOVE PD-EXCLUSION-PERIOD-DAYS (R1) TO EXCL-PERIOD (M1)
080322           MOVE PD-COVERAGE-ENDS-MOS (R1) TO COV-ENDS (M1)
080322           MOVE PD-ACCIDENT-ONLY-MOS (R1) TO ACC-PER-ENDS-OUT (M1)
080322           MOVE PD-CRIT-PERIOD (R1)     TO CRIT-PER-OUT (M1)
080322           MOVE PD-REC-CP-ALPHA    (R1) TO REC-CRIT-PER (M1)
080322           MOVE PD-RTW-MOS (R1)         TO RTW-MOS-OUT (M1)
080322           MOVE PD-MAX-EXTENSION (R1)   TO MAX-EXT-OUT (M1)
              END-IF
080322        ADD 1 TO R1
           END-PERFORM
080322     IF PAGE-NBR = 1
080322       AND PD-PROD-CODE (9) > SPACES
080322        MOVE 'YES' TO MORERECO
080322     ELSE
080322        MOVE 'NO'  TO MORERECO
080322     END-IF
           IF PD-TRUNCATED = 'Y'
              MOVE 'Y'                     TO TRUNCO
           ELSE
              MOVE 'N'                     TO TRUNCO
           END-IF
           MOVE PD-LAST-MAINT-BY           TO  MAINTBYO
                                               PI-UPDATE-BY.
           MOVE PD-LAST-MAINT-HHMMSS       TO  TIME-IN
                                               PI-UPDATE-HHMMSS.
           MOVE TIME-OUT                   TO  MAINTATO.
           MOVE PD-LAST-MAINT-DT           TO  DC-BIN-DATE-1.
           MOVE ' '                        TO  DC-OPTION-CODE.
           MOVE +0                         TO  DC-ELAPSED-DAYS
                                               DC-ELAPSED-MONTHS.
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO
           ELSE
               MOVE LOW-VALUES             TO  MAINTONO.
           MOVE -1                         TO  MAINTL.
           MOVE AL-UANON                   TO  STATEA
                                               PRODCDA
                                               BENTYPA
                                               BENCODEA
                                               EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.
       7100-READ-BENEFIT.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND   (7120-NOT-FOUND)
      *    END-EXEC.
      *    MOVE '"$I                   ! % #00005340' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        DATASET   (ELCNTL-FILE-ID)
      *        RIDFLD    (ELCNTL-KEY)
      *        SET       (ADDRESS OF CONTROL-FILE)
      *        GTEQ
      *    END-EXEC.
      *    MOVE '&"S        G          (   #00005343' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           IF PI-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR
              ELCNTL-RECORD-TYPE IS NOT EQUAL TO CF-RECORD-TYPE
               GO TO 7120-NOT-FOUND.
           MOVE +1                         TO  SUB-1.
       7110-LOOP.
           IF SUB-1 IS EQUAL TO +9
               GO TO 7120-NOT-FOUND.
           IF WS-BENE-CODE IS NOT EQUAL TO CF-BENEFIT-CODE (SUB-1)
               ADD +1                      TO  SUB-1
               GO TO 7110-LOOP.
           MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.
           GO TO 7199-EXIT.
       7120-NOT-FOUND.
           MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.
       7199-EXIT.
           EXIT.
           EJECT
       8000-READ-CNTL.
           
      * EXEC CICS HANDLE CONDITION
      *        NOTFND   (8009-NOTFND)
      *    END-EXEC.
      *    MOVE '"$I                   ! & #00005367' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303035333637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           
      * EXEC CICS READ
      *        DATASET   (ELCNTL-FILE-ID)
      *        RIDFLD    (ELCNTL-KEY)
      *        SET       (ADDRESS OF CONTROL-FILE)
      *    END-EXEC.
      *    MOVE '&"S        E          (   #00005370' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE 'Y'                         TO  WS-CNTL-REC-FOUND-SW.
           GO TO 8010-EXIT.
       8009-NOTFND.
           MOVE 'N'                         TO  WS-CNTL-REC-FOUND-SW.
       8010-EXIT.
           EXIT.
           EJECT
       8100-SEND-INITIAL-MAP.
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
           MOVE EIBTIME                    TO  TIME-IN.
           MOVE SAVE-DATE                  TO  DATEO.
           MOVE TIME-OUT                   TO  TIMEO.
           
      * EXEC CICS SEND
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL159AO)
      *        ERASE
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL159AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00005387' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159AO, 
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
           
           MOVE '159A'                 TO PI-CURRENT-SCREEN-NO
           GO TO 9100-RETURN-TRAN
           .
       8200-SEND-DATAONLY.
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.
           MOVE EIBTIME                    TO  TIME-IN.
           MOVE SAVE-DATE                  TO  DATEO.
           MOVE TIME-OUT                   TO  TIMEO.
           
      * EXEC CICS SEND
      *        MAP      (WS-MAP-NAME)
      *        MAPSET   (MAPSET-NAME)
      *        FROM     (EL159AO)
      *        DATAONLY
      *        CURSOR
      *    END-EXEC.
           MOVE LENGTH OF
            EL159AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00005402' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL159AO, 
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
           
           GO TO 9100-RETURN-TRAN
           .
       8300-SEND-TEXT.
           
      * EXEC CICS SEND TEXT
      *        FROM  (LOGOFF-TEXT)
      *        LENGTH(LOGOFF-LENGTH)
      *        ERASE
      *        FREEKB
      *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00005412' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343132' TO DFHEIV0(25:11)
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
           
           
      * EXEC CICS RETURN
      *        END-EXEC.
      *    MOVE '.(                    ''   #00005418' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           .
       8500-DEEDIT.
           display ' before ' deedit-field
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-999V99-OF-DEEDIT
           display ' after  ' WS-999V99-OF-DEEDIT
           .
       8500-EXIT.
           EXIT.
       8600-DEEDIT.
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-NUMVAL-OF-DEEDIT
           .
       8600-EXIT.
           EXIT.
       8800-UNAUTHORIZED-ACCESS.
           MOVE UNACCESS-MSG               TO  LOGOFF-MSG.
           GO TO 8300-SEND-TEXT.
       8810-PF23.
           MOVE EIBAID                     TO  PI-ENTRY-CD-1.
           MOVE XCTL-005                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       8870-NOTOPEN.
           MOVE LOW-VALUES                 TO  EL159AO.
           MOVE -1                         TO  MAINTL.
           MOVE ER-0701                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           GO TO 8100-SEND-INITIAL-MAP.
       8880-NOT-FOUND.
           MOVE ER-0702                    TO  EMI-ERROR.
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           MOVE -1                         TO  BENTYPL
           MOVE AL-UABON                   TO  BENTYPA BENCODEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.
           .
       9100-RETURN-TRAN.
           MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.
           
      * EXEC CICS RETURN
      *        TRANSID    (TRANS-ID)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.(CT                  ''   #00005457' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9200-RETURN-MAIN-MENU.
           MOVE XCTL-626                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       9300-XCTL.
           
      * EXEC CICS XCTL
      *        PROGRAM    (PGM-NAME)
      *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
      *        LENGTH     (PI-COMM-LENGTH)
      *    END-EXEC.
      *    MOVE '.$C                   %   #00005466' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9400-CLEAR.
           MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.
           GO TO 9300-XCTL.
       9500-PF12.
           MOVE XCTL-010                   TO  PGM-NAME.
           GO TO 9300-XCTL.
       9600-PGMID-ERROR.
           
      * EXEC CICS HANDLE CONDITION
      *        PGMIDERR   (8300-SEND-TEXT)
      *    END-EXEC.
      *    MOVE '"$L                   ! '' #00005478' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303035343738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.
           MOVE ' '                        TO  PI-ENTRY-CD-1.
           MOVE XCTL-005                   TO  PGM-NAME.
           MOVE PGM-NAME                   TO  LOGOFF-PGM.
           MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.
           GO TO 9300-XCTL.
           EJECT
       9700-LINK-DATE-CONVERT.
           
      * EXEC CICS LINK
      *        PROGRAM    ('ELDATCV')
      *        COMMAREA   (DATE-CONVERSION-DATA)
      *        LENGTH     (DC-COMM-LENGTH)
      *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00005489' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9700-EXIT.
           EXIT.
       9900-ERROR-FORMAT.
           IF NOT EMI-ERRORS-COMPLETE
               MOVE LINK-001               TO  PGM-NAME
               
      * EXEC CICS LINK
      *            PROGRAM    (PGM-NAME)
      *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
      *            LENGTH     (EMI-COMM-LENGTH)
      *        END-EXEC.
      *    MOVE '."C                   (   #00005499' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
       9900-EXIT.
           EXIT.
       9990-ABEND.
           MOVE LINK-004                   TO  PGM-NAME.
           MOVE DFHEIBLK                   TO  EMI-LINE1.
           
      * EXEC CICS LINK
      *        PROGRAM   (PGM-NAME)
      *        COMMAREA  (EMI-LINE1)
      *        LENGTH    (72)
      *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00005509' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           GO TO 8100-SEND-INITIAL-MAP.
           EJECT
       9995-SECURITY-VIOLATION.
      *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00005534' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353334' TO DFHEIV0(25:11)
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
       9995-EXIT.
           EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL159' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8870-NOTOPEN,
                     8880-NOT-FOUND,
                     9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 5000-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 5100-UNSUCCESSFUL-SEARCH
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7120-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8009-NOTFND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL159' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
