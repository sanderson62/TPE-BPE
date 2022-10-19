00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL6331.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/12/96 09:49:43.
00007 *                            VMOD=2.008
00008 *
00008 *
00009 *AUTHOR.        LOGIC,INC.
00010 *               DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023 *
00024 *REMARKS.
00025 *        TRANSACTION - EXB8 - COMPENSATION PAYMENTS/ADJUSTMENTS.
00023 *
101101******************************************************************
101101*                   C H A N G E   L O G
101101*
101101* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101101*-----------------------------------------------------------------
101101*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101101* EFFECTIVE    NUMBER
101101*-----------------------------------------------------------------
101101* 101101    2001100100006  SMVA  ADD USERID & COMPANY ID(CMPNYID)
101101*                              ADJUSTED REDEFINES EL633BI FILLER
010803* 010803                   PEMA  ADD 1825013200 FOR DCC
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
022504* 022504                   PEMA ADD GL CODE FOR DCC
093004* 093004                   PEMA ADD NEW GL CODE FOR DCC
120204* 120204                   PEMA NOTICED LOW VALS IN BATCH REPORTS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
061605* 061605    2005051300001  PEMA ADD GL NUM EDIT FOR DCC
122105* 122105    2005033100001  PEMA ADD GL NUM EDIT FOR CSI
032806* 032806                   PEMA ADD MORE GL NUMBERS FOR CSI
071806* 071806    2006012600002  PEMA CHANGE DCC GL NUMBERS
080206* 080206    2006012600002  PEMA ADD GL NUMBER FOR CID & DCC
092506* 092506                   PEMA ADD NEW GL CODE FOR DCC
031909* 031909    2009030300001  AJRA ADD NEW GL CODE FOR CID
040109* 040109    2008050500001  AJRA ADD GL NUMBERS FOR CCC
031710* 031710  CR2009100700001  PEMA ADD RUNNING NET TOTAL
120711* 120711  CR2011120100004  PEMA ADD GL NUMBER FOR CCC
031912* 031912  CR2011120900003  AJRA AHL COMPANY CODE
100713* 100713  CR2013100700001  AJRA ADD 1825091000 FOR CID
081414* 081414    2014012300001  PEMA  ADD PROCESSING FOR CARRIER 7 DCC
110315* 110315  CR2015101400001  PEMA ADD NEW DCC G/L #'S FOR ACH
111715* 111715  IR2015111600001  PEMA  CHG G/L ACCTNO ON CID ACH
021716* 021716  CR2016021000003  PEMA  ADD NEW G/L FOR MACHENS ACCTS
111016* 111016  CR2016110900002  TANA  REPLACE GL NUMBERS FOR CCC
060817* 060817  CR2017060700005  PEMA  ADD NEW G/L FOR VPP
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
101101******************************************************************
00026
00027  ENVIRONMENT DIVISION.
00028  DATA DIVISION.
00029  EJECT
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL6331 WORKING STORAGE    *'.
00033  77  FILLER  PIC X(32)  VALUE '************ V/M 2.008 *********'.
CIDMOD
CIDMOD 77  HLD-XX  PIC X(32)  VALUE '********************************'.
CIDMOD 77  FILLER  PIC X(32)  VALUE '/// SAVE COMPENSATION MASTER ///'.
CIDMOD 77  SV-COMP PIC X(400) VALUE SPACE.
060205 77  C1      PIC S999 COMP-3 VALUE +0.
CIDMOD*77  FILLER  PIC X(32)  VALUE '/////// SAVE COFA MASTER ///////'.
CIDMOD*77  SV-COFA PIC X(42)  VALUE SPACE.
CIDMOD*77  K-SPACE PIC X(11)  VALUE SPACES.
00034
00035 *                            COPY ELCSCTM.
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
00036 *                            COPY ELCSCRTY.
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
00037
CIDMOD
CIDMOD*01  FORCE-DUMP-X            PIC X(1)        VALUE SPACE.
CIDMOD*01  FORCE-DUMP  REDEFINES FORCE-DUMP-X
CIDMOD*                            PIC S9.
00038     EJECT
00039
00040  01  STANDARD-AREAS.
00041      12  GETMAIN-SPACE       PIC  X          VALUE SPACE.
00042      12  MAP-NAME            PIC  X(8)       VALUE 'EL633B '.
00043      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL6331S'.
00044      12  SCREEN-NUMBER       PIC  X(4)       VALUE '633B'.
00045      12  TRANS-ID            PIC  X(4)       VALUE 'EXB8'.
00046      12  THIS-PGM            PIC  X(8)       VALUE 'EL6331'.
00047      12  PGM-NAME            PIC  X(8).
00048      12  TIME-IN             PIC S9(7).
00049      12  TIME-OUT-R  REDEFINES  TIME-IN.
00050          16  FILLER          PIC  X.
00051          16  TIME-OUT        PIC  9(2)V9(2).
00052          16  FILLER          PIC  X(2).
00053      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.
00054      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.
00055      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.
00056      12  XCTL-652            PIC  X(8)       VALUE 'EL652'.
00057      12  LINK-001            PIC  X(8)       VALUE 'EL001'.
00058      12  LINK-004            PIC  X(8)       VALUE 'EL004'.
00059      12  LINK-CLDATCV        PIC  X(8)       VALUE 'ELDATCV'.
00060      12  PYAJ-FILE-ID        PIC  X(8)       VALUE 'ERPYAJ'.
00061      12  COMP-FILE-ID        PIC  X(8)       VALUE 'ERCOMP'.
00062      12  WS-CURRENT-DT       PIC  X(8)       VALUE SPACES.
00063      12  WS-CURRENT-BIN-DT   PIC  X(2)       VALUE SPACES.
00064      12  WORK-SEQ-NO         PIC S9(9)                  COMP-3.
031710     12  TOTAL-AMOUNT        PIC S9(9)V99      VALUE ZEROS.
00065      12  CHECK-REC-TYPE      PIC  X          VALUE SPACE.
CIDMOD         88  VALID-REC-TYPE                  VALUE  'R' 'D' 'C'
CIDMOD                                                    'S' 'T' 'U'
CIDMOD                                                    'X' 'Y' 'Z'
CIDMOD                                                    'F'.
CIDMOD     12  CHECK-CANC-TYPE         PIC X       VALUE SPACE.
CIDMOD         88  VALID-CANC-TYPE                 VALUE 'N' 'Y'.
00070      12  DEEDIT-FIELD            PIC X(11).
CIDMOD     12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD PIC S9(9)V99.
CIDMOD     12  WS-EDITED-AMTS OCCURS 15 TIMES
00072                                 INDEXED BY WS-INDX.
00073          16  WS-EDITED-AMT       PIC S9(9)V99.
CIDMOD*    12  COFA-FILE-ID            PIC  X(8)   VALUE 'COFAXXX'.
CIDMOD*01  FILLER                      PIC  X(15)
CIDMOD*                                VALUE '* ACCT BREAK  *'.
CIDMOD*01  WS-ACCT-BREAK.
CIDMOD*    05  WS-ACCT-1               PIC  X.
CIDMOD*    05  FILLER                  PIC  X(5).
CIDMOD
060205 01  WS-ERCOMP-TYPE-TYPE.
060205     12  WS-ERCOMP-TYPE OCCURS 15   PIC X.
CIDMOD******************************************************************
CIDMOD*                      TABLE OF STATE NAMES
CIDMOD******************************************************************
CIDMOD
CIDMOD 01  CHECK-STATE-CODE            PIC  XX     VALUE SPACE.
CIDMOD     88  VALID-STATE-CODE        VALUE 'AK' 'AL' 'AR' 'AZ' 'CA'
CIDMOD                                       'CD' 'CO' 'CT' 'DC' 'DE'
CIDMOD                                       'FL' 'GA' 'GM' 'HI' 'IA'
CIDMOD                                       'ID' 'IL' 'IN' 'KS' 'KY'
CIDMOD                                       'LA' 'MA' 'MD' 'ME' 'MI'
CIDMOD                                       'MN' 'MO' 'MS' 'MT' 'MX'
CIDMOD                                       'NC' 'ND' 'NE' 'NH' 'NJ'
CIDMOD                                       'NM' 'NV' 'NY' 'OF' 'OH'
CIDMOD                                       'OK' 'OR' 'PA' 'PI' 'PR'
CIDMOD                                       'RI' 'SC' 'SD' 'TN' 'TX'
CIDMOD                                       'UT' 'VA' 'VI' 'VT' 'WA'
CIDMOD                                       'WI' 'WV' 'WY'.
CIDMOD
CIDMOD 01  CHECK-GL-ACCT         PIC X(10)  VALUE SPACE.
CIDMOD     88  VALID-GL-ACCOUNT  VALUE '1108121010'
111715                                 '1108124700'
CIDMOD                                 '1108125100'
CIDMOD                                 '1721211400'
CIDMOD                                 '1825011200'
CIDMOD                                 '1825011300'
100713                                 '1825091000'
CIDMOD                                 '1825099050'
031909                                 '8505700033'
CIDMOD                                 '8506400030'
CIDMOD                                 '8507200020'
080206                                 '8507200010'
021716                                 '2725010160'.
071806     88  VALID-DCC-GL-ACCOUNT  VALUE '2725040300'
071806                                     '2725040320'
071806                                     '2725040330'
071806                                     '2725040310'
071806                                     '8506400030'
080206                                     '8507200010'
092506                                     '1108121250'.
111016*    88  VALID-CSI-GL-ACCOUNT  VALUE '2725040100'
111016*                                    '2725040110'
111016*                                    '2725040120'
111016*                                    '2725040130'
111016*                                    '2725020400'.
111016     88  VALID-CSI-GL-ACCOUNT  VALUE '1108121010'
111016                                     '1825013200'
111016                                     '1825013300'
111016                                     '1825013400'.
040109
040109     88  VALID-CCC-GL-ACCOUNT  VALUE '1825013100'
040109                                     '1825013200'
040109                                     '1825013300'
040109                                     '1825013400'
040109                                     '8506400030'
040109                                     '8507200010'
040109                                     '8507200020'
120711                                     '1108121010'.
           88  VALID-VPP-GL-ACCOUNT  VALUE '2725040510'
                                           '2725040520'
060817                                     '7206100400'
060817                                     '7206104100'.
062121     88  VALID-FNL-GL-ACCOUNT  VALUE '1108121010'
062121                                     '1721211400'
062121                                     '1825011100'
062121                                     '1825011200'
062121                                     '1825011300'
062121                                     '1825099050'
062121                                     '2718000110'
062121                                     '2718000120'
062121                                     '8506400030'
062121                                     '8507200010'.
00074
00075  01  ACCESS-KEYS.
00076      12  ERPYAJ-KEY.
00077          16  PYAJ-COMP-CD        PIC  X      VALUE SPACE.
00078          16  PYAJ-CARRIER        PIC  X      VALUE SPACES.
00079          16  PYAJ-GROUPING       PIC  X(6)   VALUE SPACES.
00080          16  PYAJ-FIN-RESP       PIC  X(10)  VALUE SPACES.
00081          16  PYAJ-ACCOUNT        PIC  X(10)  VALUE SPACES.
00082          16  PYAJ-FILE-SEQ-NO    PIC S9(8)   VALUE +0   COMP.
00083          16  PYAJ-RECORD-TYPE    PIC  X      VALUE SPACES.
00084
00085      12  ERPYAJ-RECORD-LENGTH    PIC S9(4)   VALUE +200 COMP.
00086      12  ERPYAJ-JOURNAL-LENGTH   PIC S9(4)   VALUE +223 COMP.
00087
00088      12  ERCOMP-KEY.
00089          16  COMP-COMP-CD        PIC  X      VALUE SPACE.
00090          16  COMP-CARRIER        PIC  X      VALUE SPACES.
00091          16  COMP-GROUPING       PIC  X(6)   VALUE SPACES.
00092          16  COMP-FIN-RESP       PIC  X(10)  VALUE SPACES.
00093          16  COMP-ACCOUNT        PIC  X(10)  VALUE SPACES.
00094          16  COMP-RECORD-TYPE    PIC  X      VALUE SPACES.
CIDMOD*    12  COFA-KEY-X.
CIDMOD*        16  COFA-COMPANY-X      PIC  X(4)   VALUE SPACES.
CIDMOD*        16  COFA-ACCOUNT.
CIDMOD*            20  COFA-FILLER     PIC  X(11)  VALUE SPACES.
CIDMOD*            20  COFA-MSA-ACCT   PIC  X(07)  VALUE SPACES.
CIDMOD*
00095  EJECT
00096  01  ERROR-NUMBERS.
00097      12  ER-0000             PIC  X(4)       VALUE '0000'.
00098      12  ER-0008             PIC  X(4)       VALUE '0008'.
00099      12  ER-0029             PIC  X(4)       VALUE '0029'.
00100      12  ER-0070             PIC  X(4)       VALUE '0070'.
00101      12  ER-0194             PIC  X(4)       VALUE '0194'.
00102      12  ER-0195             PIC  X(4)       VALUE '0195'.
00103      12  ER-0197             PIC  X(4)       VALUE '0197'.
00104      12  ER-2230             PIC  X(4)       VALUE '2230'.
00105      12  ER-2232             PIC  X(4)       VALUE '2232'.
00106      12  ER-2233             PIC  X(4)       VALUE '2233'.
00107      12  ER-2234             PIC  X(4)       VALUE '2234'.
00108      12  ER-2235             PIC  X(4)       VALUE '2235'.
00109      12  ER-2236             PIC  X(4)       VALUE '2236'.
00110      12  ER-2245             PIC  X(4)       VALUE '2245'.
00111      12  ER-2562             PIC  X(4)       VALUE '2562'.
00112      12  ER-2587             PIC  X(4)       VALUE '2587'.
00113      12  ER-2588             PIC  X(4)       VALUE '2588'.
00114      12  ER-2595             PIC  X(4)       VALUE '2595'.
00115      12  ER-2596             PIC  X(4)       VALUE '2596'.
00116      12  ER-2763             PIC  X(4)       VALUE '2763'.
CIDMOD     12  ER-2956             PIC  X(4)       VALUE '2956'.
CIDMOD     12  ER-2957             PIC  X(4)       VALUE '2957'.
CIDMOD     12  ER-2958             PIC  X(4)       VALUE '2958'.
CIDMOD     12  ER-2959             PIC  X(4)       VALUE '2959'.
CIDMOD     12  ER-2960             PIC  X(4)       VALUE '2960'.
CIDMOD     12  ER-2961             PIC  X(4)       VALUE '2961'.
CIDMOD     12  ER-9030             PIC  X(4)       VALUE '9030'.
CIDMOD
00117  EJECT
00118 *                                    COPY ELCDATE.
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
00119  EJECT
00120 *                                    COPY ELCLOGOF.
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
00121  EJECT
00122 *                                    COPY ELCATTR.
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
00123  EJECT
00124 *                                    COPY ELCEMIB.
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
00125  EJECT
00126 *                                    COPY ELCINTF.
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
00127      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00128          16  PI-PYAJ-FILE-SW         PIC  X.
00129              88  END-OF-ACCT                 VALUE 'A'.
00130              88  END-OF-FILE                 VALUE 'X'.
00131              88  NO-RECORDS                  VALUE 'Y'.
00132              88  NOT-OPEN                    VALUE 'Z'.
00133          16  PI-PREV-FUNCTION        PIC  X.
00134          16  PI-SAV-FUNCTION         PIC  X.
00135          16  PI-SEQ-NOS.
00136              20  FILLER  OCCURS 13 TIMES
00137                              INDEXED BY NDX.
00138                  24  PI-REC-TYPE     PIC  X.
00139                  24  PI-FILE-SEQ-NO  PIC S9(8).
00140          16  PI-SAV-ENDING-PYAJ-KEY.
00141              20  PI-SAV-COMP-CD      PIC  X.
00142              20  PI-SAV-CARRIER      PIC  X.
00143              20  PI-SAV-GROUPING     PIC  X(3).
00144              20  PI-SAV-FIN-RESP     PIC  X(6).
00145              20  PI-SAV-ACCOUNT      PIC  X(6).
00146              20  PI-SAV-FILE-SEQ-NO  PIC S9(8)          COMP.
00147              20  PI-SAV-RECORD-TYPE  PIC  X.
00148          16  FILLER                  PIC  X(498).
00149  EJECT
00150 *                            COPY ELCJPFX.
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
00151                              PIC  X(223).
00152  EJECT
00153 *                            COPY ELCAID.
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
00154
00155  01  FILLER    REDEFINES DFHAID.
00156      12  FILLER              PIC  X(8).
00157      12  PF-VALUES           PIC  X          OCCURS 2 TIMES.
00158  EJECT
00159 *                            COPY EL6331S.
       01  EL633BI.
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
           05  CMPNYIDL PIC S9(0004) COMP.
           05  CMPNYIDF PIC  X(0001).
           05  FILLER REDEFINES CMPNYIDF.
               10  CMPNYIDA PIC  X(0001).
           05  CMPNYIDI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  CAR1L PIC S9(0004) COMP.
           05  CAR1F PIC  X(0001).
           05  FILLER REDEFINES CAR1F.
               10  CAR1A PIC  X(0001).
           05  CAR1I PIC  X(0001).
      *    -------------------------------
           05  GRP1L PIC S9(0004) COMP.
           05  GRP1F PIC  X(0001).
           05  FILLER REDEFINES GRP1F.
               10  GRP1A PIC  X(0001).
           05  GRP1I PIC  X(0006).
      *    -------------------------------
           05  FRESP1L PIC S9(0004) COMP.
           05  FRESP1F PIC  X(0001).
           05  FILLER REDEFINES FRESP1F.
               10  FRESP1A PIC  X(0001).
           05  FRESP1I PIC  X(0010).
      *    -------------------------------
           05  ACCT1L PIC S9(0004) COMP.
           05  ACCT1F PIC  X(0001).
           05  FILLER REDEFINES ACCT1F.
               10  ACCT1A PIC  X(0001).
           05  ACCT1I PIC  X(0010).
      *    -------------------------------
           05  TYPE1L PIC S9(0004) COMP.
           05  TYPE1F PIC  X(0001).
           05  FILLER REDEFINES TYPE1F.
               10  TYPE1A PIC  X(0001).
           05  TYPE1I PIC  X(0001).
      *    -------------------------------
           05  AMT1L PIC S9(0004) COMP.
           05  AMT1F PIC  X(0001).
           05  FILLER REDEFINES AMT1F.
               10  AMT1A PIC  X(0001).
           05  AMT1I PIC  X(0011).
      *    -------------------------------
           05  MSAAC1L PIC S9(0004) COMP.
           05  MSAAC1F PIC  X(0001).
           05  FILLER REDEFINES MSAAC1F.
               10  MSAAC1A PIC  X(0001).
           05  MSAAC1I PIC  X(0010).
      *    -------------------------------
           05  MSAST1L PIC S9(0004) COMP.
           05  MSAST1F PIC  X(0001).
           05  FILLER REDEFINES MSAST1F.
               10  MSAST1A PIC  X(0001).
           05  MSAST1I PIC  X(0002).
      *    -------------------------------
           05  MSACN1L PIC S9(0004) COMP.
           05  MSACN1F PIC  X(0001).
           05  FILLER REDEFINES MSACN1F.
               10  MSACN1A PIC  X(0001).
           05  MSACN1I PIC  X(0001).
      *    -------------------------------
           05  MSACM1L PIC S9(0004) COMP.
           05  MSACM1F PIC  X(0001).
           05  FILLER REDEFINES MSACM1F.
               10  MSACM1A PIC  X(0001).
           05  MSACM1I PIC  X(0010).
      *    -------------------------------
           05  CAR2L PIC S9(0004) COMP.
           05  CAR2F PIC  X(0001).
           05  FILLER REDEFINES CAR2F.
               10  CAR2A PIC  X(0001).
           05  CAR2I PIC  X(0001).
      *    -------------------------------
           05  GRP2L PIC S9(0004) COMP.
           05  GRP2F PIC  X(0001).
           05  FILLER REDEFINES GRP2F.
               10  GRP2A PIC  X(0001).
           05  GRP2I PIC  X(0006).
      *    -------------------------------
           05  FRESP2L PIC S9(0004) COMP.
           05  FRESP2F PIC  X(0001).
           05  FILLER REDEFINES FRESP2F.
               10  FRESP2A PIC  X(0001).
           05  FRESP2I PIC  X(0010).
      *    -------------------------------
           05  ACCT2L PIC S9(0004) COMP.
           05  ACCT2F PIC  X(0001).
           05  FILLER REDEFINES ACCT2F.
               10  ACCT2A PIC  X(0001).
           05  ACCT2I PIC  X(0010).
      *    -------------------------------
           05  TYPE2L PIC S9(0004) COMP.
           05  TYPE2F PIC  X(0001).
           05  FILLER REDEFINES TYPE2F.
               10  TYPE2A PIC  X(0001).
           05  TYPE2I PIC  X(0001).
      *    -------------------------------
           05  AMT2L PIC S9(0004) COMP.
           05  AMT2F PIC  X(0001).
           05  FILLER REDEFINES AMT2F.
               10  AMT2A PIC  X(0001).
           05  AMT2I PIC  X(0011).
      *    -------------------------------
           05  MSAAC2L PIC S9(0004) COMP.
           05  MSAAC2F PIC  X(0001).
           05  FILLER REDEFINES MSAAC2F.
               10  MSAAC2A PIC  X(0001).
           05  MSAAC2I PIC  X(0010).
      *    -------------------------------
           05  MSAST2L PIC S9(0004) COMP.
           05  MSAST2F PIC  X(0001).
           05  FILLER REDEFINES MSAST2F.
               10  MSAST2A PIC  X(0001).
           05  MSAST2I PIC  X(0002).
      *    -------------------------------
           05  MSACN2L PIC S9(0004) COMP.
           05  MSACN2F PIC  X(0001).
           05  FILLER REDEFINES MSACN2F.
               10  MSACN2A PIC  X(0001).
           05  MSACN2I PIC  X(0001).
      *    -------------------------------
           05  MSACM2L PIC S9(0004) COMP.
           05  MSACM2F PIC  X(0001).
           05  FILLER REDEFINES MSACM2F.
               10  MSACM2A PIC  X(0001).
           05  MSACM2I PIC  X(0010).
      *    -------------------------------
           05  CAR3L PIC S9(0004) COMP.
           05  CAR3F PIC  X(0001).
           05  FILLER REDEFINES CAR3F.
               10  CAR3A PIC  X(0001).
           05  CAR3I PIC  X(0001).
      *    -------------------------------
           05  GRP3L PIC S9(0004) COMP.
           05  GRP3F PIC  X(0001).
           05  FILLER REDEFINES GRP3F.
               10  GRP3A PIC  X(0001).
           05  GRP3I PIC  X(0006).
      *    -------------------------------
           05  FRESP3L PIC S9(0004) COMP.
           05  FRESP3F PIC  X(0001).
           05  FILLER REDEFINES FRESP3F.
               10  FRESP3A PIC  X(0001).
           05  FRESP3I PIC  X(0010).
      *    -------------------------------
           05  ACCT3L PIC S9(0004) COMP.
           05  ACCT3F PIC  X(0001).
           05  FILLER REDEFINES ACCT3F.
               10  ACCT3A PIC  X(0001).
           05  ACCT3I PIC  X(0010).
      *    -------------------------------
           05  TYPE3L PIC S9(0004) COMP.
           05  TYPE3F PIC  X(0001).
           05  FILLER REDEFINES TYPE3F.
               10  TYPE3A PIC  X(0001).
           05  TYPE3I PIC  X(0001).
      *    -------------------------------
           05  AMT3L PIC S9(0004) COMP.
           05  AMT3F PIC  X(0001).
           05  FILLER REDEFINES AMT3F.
               10  AMT3A PIC  X(0001).
           05  AMT3I PIC  X(0011).
      *    -------------------------------
           05  MSAAC3L PIC S9(0004) COMP.
           05  MSAAC3F PIC  X(0001).
           05  FILLER REDEFINES MSAAC3F.
               10  MSAAC3A PIC  X(0001).
           05  MSAAC3I PIC  X(0010).
      *    -------------------------------
           05  MSAST3L PIC S9(0004) COMP.
           05  MSAST3F PIC  X(0001).
           05  FILLER REDEFINES MSAST3F.
               10  MSAST3A PIC  X(0001).
           05  MSAST3I PIC  X(0002).
      *    -------------------------------
           05  MSACN3L PIC S9(0004) COMP.
           05  MSACN3F PIC  X(0001).
           05  FILLER REDEFINES MSACN3F.
               10  MSACN3A PIC  X(0001).
           05  MSACN3I PIC  X(0001).
      *    -------------------------------
           05  MSACM3L PIC S9(0004) COMP.
           05  MSACM3F PIC  X(0001).
           05  FILLER REDEFINES MSACM3F.
               10  MSACM3A PIC  X(0001).
           05  MSACM3I PIC  X(0010).
      *    -------------------------------
           05  CAR4L PIC S9(0004) COMP.
           05  CAR4F PIC  X(0001).
           05  FILLER REDEFINES CAR4F.
               10  CAR4A PIC  X(0001).
           05  CAR4I PIC  X(0001).
      *    -------------------------------
           05  GRP4L PIC S9(0004) COMP.
           05  GRP4F PIC  X(0001).
           05  FILLER REDEFINES GRP4F.
               10  GRP4A PIC  X(0001).
           05  GRP4I PIC  X(0006).
      *    -------------------------------
           05  FRESP4L PIC S9(0004) COMP.
           05  FRESP4F PIC  X(0001).
           05  FILLER REDEFINES FRESP4F.
               10  FRESP4A PIC  X(0001).
           05  FRESP4I PIC  X(0010).
      *    -------------------------------
           05  ACCT4L PIC S9(0004) COMP.
           05  ACCT4F PIC  X(0001).
           05  FILLER REDEFINES ACCT4F.
               10  ACCT4A PIC  X(0001).
           05  ACCT4I PIC  X(0010).
      *    -------------------------------
           05  TYPE4L PIC S9(0004) COMP.
           05  TYPE4F PIC  X(0001).
           05  FILLER REDEFINES TYPE4F.
               10  TYPE4A PIC  X(0001).
           05  TYPE4I PIC  X(0001).
      *    -------------------------------
           05  AMT4L PIC S9(0004) COMP.
           05  AMT4F PIC  X(0001).
           05  FILLER REDEFINES AMT4F.
               10  AMT4A PIC  X(0001).
           05  AMT4I PIC  X(0011).
      *    -------------------------------
           05  MSAAC4L PIC S9(0004) COMP.
           05  MSAAC4F PIC  X(0001).
           05  FILLER REDEFINES MSAAC4F.
               10  MSAAC4A PIC  X(0001).
           05  MSAAC4I PIC  X(0010).
      *    -------------------------------
           05  MSAST4L PIC S9(0004) COMP.
           05  MSAST4F PIC  X(0001).
           05  FILLER REDEFINES MSAST4F.
               10  MSAST4A PIC  X(0001).
           05  MSAST4I PIC  X(0002).
      *    -------------------------------
           05  MSACN4L PIC S9(0004) COMP.
           05  MSACN4F PIC  X(0001).
           05  FILLER REDEFINES MSACN4F.
               10  MSACN4A PIC  X(0001).
           05  MSACN4I PIC  X(0001).
      *    -------------------------------
           05  MSACM4L PIC S9(0004) COMP.
           05  MSACM4F PIC  X(0001).
           05  FILLER REDEFINES MSACM4F.
               10  MSACM4A PIC  X(0001).
           05  MSACM4I PIC  X(0010).
      *    -------------------------------
           05  CAR5L PIC S9(0004) COMP.
           05  CAR5F PIC  X(0001).
           05  FILLER REDEFINES CAR5F.
               10  CAR5A PIC  X(0001).
           05  CAR5I PIC  X(0001).
      *    -------------------------------
           05  GRP5L PIC S9(0004) COMP.
           05  GRP5F PIC  X(0001).
           05  FILLER REDEFINES GRP5F.
               10  GRP5A PIC  X(0001).
           05  GRP5I PIC  X(0006).
      *    -------------------------------
           05  FRESP5L PIC S9(0004) COMP.
           05  FRESP5F PIC  X(0001).
           05  FILLER REDEFINES FRESP5F.
               10  FRESP5A PIC  X(0001).
           05  FRESP5I PIC  X(0010).
      *    -------------------------------
           05  ACCT5L PIC S9(0004) COMP.
           05  ACCT5F PIC  X(0001).
           05  FILLER REDEFINES ACCT5F.
               10  ACCT5A PIC  X(0001).
           05  ACCT5I PIC  X(0010).
      *    -------------------------------
           05  TYPE5L PIC S9(0004) COMP.
           05  TYPE5F PIC  X(0001).
           05  FILLER REDEFINES TYPE5F.
               10  TYPE5A PIC  X(0001).
           05  TYPE5I PIC  X(0001).
      *    -------------------------------
           05  AMT5L PIC S9(0004) COMP.
           05  AMT5F PIC  X(0001).
           05  FILLER REDEFINES AMT5F.
               10  AMT5A PIC  X(0001).
           05  AMT5I PIC  X(0011).
      *    -------------------------------
           05  MSAAC5L PIC S9(0004) COMP.
           05  MSAAC5F PIC  X(0001).
           05  FILLER REDEFINES MSAAC5F.
               10  MSAAC5A PIC  X(0001).
           05  MSAAC5I PIC  X(0010).
      *    -------------------------------
           05  MSAST5L PIC S9(0004) COMP.
           05  MSAST5F PIC  X(0001).
           05  FILLER REDEFINES MSAST5F.
               10  MSAST5A PIC  X(0001).
           05  MSAST5I PIC  X(0002).
      *    -------------------------------
           05  MSACN5L PIC S9(0004) COMP.
           05  MSACN5F PIC  X(0001).
           05  FILLER REDEFINES MSACN5F.
               10  MSACN5A PIC  X(0001).
           05  MSACN5I PIC  X(0001).
      *    -------------------------------
           05  MSACM5L PIC S9(0004) COMP.
           05  MSACM5F PIC  X(0001).
           05  FILLER REDEFINES MSACM5F.
               10  MSACM5A PIC  X(0001).
           05  MSACM5I PIC  X(0010).
      *    -------------------------------
           05  CAR6L PIC S9(0004) COMP.
           05  CAR6F PIC  X(0001).
           05  FILLER REDEFINES CAR6F.
               10  CAR6A PIC  X(0001).
           05  CAR6I PIC  X(0001).
      *    -------------------------------
           05  GRP6L PIC S9(0004) COMP.
           05  GRP6F PIC  X(0001).
           05  FILLER REDEFINES GRP6F.
               10  GRP6A PIC  X(0001).
           05  GRP6I PIC  X(0006).
      *    -------------------------------
           05  FRESP6L PIC S9(0004) COMP.
           05  FRESP6F PIC  X(0001).
           05  FILLER REDEFINES FRESP6F.
               10  FRESP6A PIC  X(0001).
           05  FRESP6I PIC  X(0010).
      *    -------------------------------
           05  ACCT6L PIC S9(0004) COMP.
           05  ACCT6F PIC  X(0001).
           05  FILLER REDEFINES ACCT6F.
               10  ACCT6A PIC  X(0001).
           05  ACCT6I PIC  X(0010).
      *    -------------------------------
           05  TYPE6L PIC S9(0004) COMP.
           05  TYPE6F PIC  X(0001).
           05  FILLER REDEFINES TYPE6F.
               10  TYPE6A PIC  X(0001).
           05  TYPE6I PIC  X(0001).
      *    -------------------------------
           05  AMT6L PIC S9(0004) COMP.
           05  AMT6F PIC  X(0001).
           05  FILLER REDEFINES AMT6F.
               10  AMT6A PIC  X(0001).
           05  AMT6I PIC  X(0011).
      *    -------------------------------
           05  MSAAC6L PIC S9(0004) COMP.
           05  MSAAC6F PIC  X(0001).
           05  FILLER REDEFINES MSAAC6F.
               10  MSAAC6A PIC  X(0001).
           05  MSAAC6I PIC  X(0010).
      *    -------------------------------
           05  MSAST6L PIC S9(0004) COMP.
           05  MSAST6F PIC  X(0001).
           05  FILLER REDEFINES MSAST6F.
               10  MSAST6A PIC  X(0001).
           05  MSAST6I PIC  X(0002).
      *    -------------------------------
           05  MSACN6L PIC S9(0004) COMP.
           05  MSACN6F PIC  X(0001).
           05  FILLER REDEFINES MSACN6F.
               10  MSACN6A PIC  X(0001).
           05  MSACN6I PIC  X(0001).
      *    -------------------------------
           05  MSACM6L PIC S9(0004) COMP.
           05  MSACM6F PIC  X(0001).
           05  FILLER REDEFINES MSACM6F.
               10  MSACM6A PIC  X(0001).
           05  MSACM6I PIC  X(0010).
      *    -------------------------------
           05  CAR7L PIC S9(0004) COMP.
           05  CAR7F PIC  X(0001).
           05  FILLER REDEFINES CAR7F.
               10  CAR7A PIC  X(0001).
           05  CAR7I PIC  X(0001).
      *    -------------------------------
           05  GRP7L PIC S9(0004) COMP.
           05  GRP7F PIC  X(0001).
           05  FILLER REDEFINES GRP7F.
               10  GRP7A PIC  X(0001).
           05  GRP7I PIC  X(0006).
      *    -------------------------------
           05  FRESP7L PIC S9(0004) COMP.
           05  FRESP7F PIC  X(0001).
           05  FILLER REDEFINES FRESP7F.
               10  FRESP7A PIC  X(0001).
           05  FRESP7I PIC  X(0010).
      *    -------------------------------
           05  ACCT7L PIC S9(0004) COMP.
           05  ACCT7F PIC  X(0001).
           05  FILLER REDEFINES ACCT7F.
               10  ACCT7A PIC  X(0001).
           05  ACCT7I PIC  X(0010).
      *    -------------------------------
           05  TYPE7L PIC S9(0004) COMP.
           05  TYPE7F PIC  X(0001).
           05  FILLER REDEFINES TYPE7F.
               10  TYPE7A PIC  X(0001).
           05  TYPE7I PIC  X(0001).
      *    -------------------------------
           05  AMT7L PIC S9(0004) COMP.
           05  AMT7F PIC  X(0001).
           05  FILLER REDEFINES AMT7F.
               10  AMT7A PIC  X(0001).
           05  AMT7I PIC  X(0011).
      *    -------------------------------
           05  MSAAC7L PIC S9(0004) COMP.
           05  MSAAC7F PIC  X(0001).
           05  FILLER REDEFINES MSAAC7F.
               10  MSAAC7A PIC  X(0001).
           05  MSAAC7I PIC  X(0010).
      *    -------------------------------
           05  MSAST7L PIC S9(0004) COMP.
           05  MSAST7F PIC  X(0001).
           05  FILLER REDEFINES MSAST7F.
               10  MSAST7A PIC  X(0001).
           05  MSAST7I PIC  X(0002).
      *    -------------------------------
           05  MSACN7L PIC S9(0004) COMP.
           05  MSACN7F PIC  X(0001).
           05  FILLER REDEFINES MSACN7F.
               10  MSACN7A PIC  X(0001).
           05  MSACN7I PIC  X(0001).
      *    -------------------------------
           05  MSACM7L PIC S9(0004) COMP.
           05  MSACM7F PIC  X(0001).
           05  FILLER REDEFINES MSACM7F.
               10  MSACM7A PIC  X(0001).
           05  MSACM7I PIC  X(0010).
      *    -------------------------------
           05  CAR8L PIC S9(0004) COMP.
           05  CAR8F PIC  X(0001).
           05  FILLER REDEFINES CAR8F.
               10  CAR8A PIC  X(0001).
           05  CAR8I PIC  X(0001).
      *    -------------------------------
           05  GRP8L PIC S9(0004) COMP.
           05  GRP8F PIC  X(0001).
           05  FILLER REDEFINES GRP8F.
               10  GRP8A PIC  X(0001).
           05  GRP8I PIC  X(0006).
      *    -------------------------------
           05  FRESP8L PIC S9(0004) COMP.
           05  FRESP8F PIC  X(0001).
           05  FILLER REDEFINES FRESP8F.
               10  FRESP8A PIC  X(0001).
           05  FRESP8I PIC  X(0010).
      *    -------------------------------
           05  ACCT8L PIC S9(0004) COMP.
           05  ACCT8F PIC  X(0001).
           05  FILLER REDEFINES ACCT8F.
               10  ACCT8A PIC  X(0001).
           05  ACCT8I PIC  X(0010).
      *    -------------------------------
           05  TYPE8L PIC S9(0004) COMP.
           05  TYPE8F PIC  X(0001).
           05  FILLER REDEFINES TYPE8F.
               10  TYPE8A PIC  X(0001).
           05  TYPE8I PIC  X(0001).
      *    -------------------------------
           05  AMT8L PIC S9(0004) COMP.
           05  AMT8F PIC  X(0001).
           05  FILLER REDEFINES AMT8F.
               10  AMT8A PIC  X(0001).
           05  AMT8I PIC  X(0011).
      *    -------------------------------
           05  MSAAC8L PIC S9(0004) COMP.
           05  MSAAC8F PIC  X(0001).
           05  FILLER REDEFINES MSAAC8F.
               10  MSAAC8A PIC  X(0001).
           05  MSAAC8I PIC  X(0010).
      *    -------------------------------
           05  MSAST8L PIC S9(0004) COMP.
           05  MSAST8F PIC  X(0001).
           05  FILLER REDEFINES MSAST8F.
               10  MSAST8A PIC  X(0001).
           05  MSAST8I PIC  X(0002).
      *    -------------------------------
           05  MSACN8L PIC S9(0004) COMP.
           05  MSACN8F PIC  X(0001).
           05  FILLER REDEFINES MSACN8F.
               10  MSACN8A PIC  X(0001).
           05  MSACN8I PIC  X(0001).
      *    -------------------------------
           05  MSACM8L PIC S9(0004) COMP.
           05  MSACM8F PIC  X(0001).
           05  FILLER REDEFINES MSACM8F.
               10  MSACM8A PIC  X(0001).
           05  MSACM8I PIC  X(0010).
      *    -------------------------------
           05  CAR9L PIC S9(0004) COMP.
           05  CAR9F PIC  X(0001).
           05  FILLER REDEFINES CAR9F.
               10  CAR9A PIC  X(0001).
           05  CAR9I PIC  X(0001).
      *    -------------------------------
           05  GRP9L PIC S9(0004) COMP.
           05  GRP9F PIC  X(0001).
           05  FILLER REDEFINES GRP9F.
               10  GRP9A PIC  X(0001).
           05  GRP9I PIC  X(0006).
      *    -------------------------------
           05  FRESP9L PIC S9(0004) COMP.
           05  FRESP9F PIC  X(0001).
           05  FILLER REDEFINES FRESP9F.
               10  FRESP9A PIC  X(0001).
           05  FRESP9I PIC  X(0010).
      *    -------------------------------
           05  ACCT9L PIC S9(0004) COMP.
           05  ACCT9F PIC  X(0001).
           05  FILLER REDEFINES ACCT9F.
               10  ACCT9A PIC  X(0001).
           05  ACCT9I PIC  X(0010).
      *    -------------------------------
           05  TYPE9L PIC S9(0004) COMP.
           05  TYPE9F PIC  X(0001).
           05  FILLER REDEFINES TYPE9F.
               10  TYPE9A PIC  X(0001).
           05  TYPE9I PIC  X(0001).
      *    -------------------------------
           05  AMT9L PIC S9(0004) COMP.
           05  AMT9F PIC  X(0001).
           05  FILLER REDEFINES AMT9F.
               10  AMT9A PIC  X(0001).
           05  AMT9I PIC  X(0011).
      *    -------------------------------
           05  MSAAC9L PIC S9(0004) COMP.
           05  MSAAC9F PIC  X(0001).
           05  FILLER REDEFINES MSAAC9F.
               10  MSAAC9A PIC  X(0001).
           05  MSAAC9I PIC  X(0010).
      *    -------------------------------
           05  MSAST9L PIC S9(0004) COMP.
           05  MSAST9F PIC  X(0001).
           05  FILLER REDEFINES MSAST9F.
               10  MSAST9A PIC  X(0001).
           05  MSAST9I PIC  X(0002).
      *    -------------------------------
           05  MSACN9L PIC S9(0004) COMP.
           05  MSACN9F PIC  X(0001).
           05  FILLER REDEFINES MSACN9F.
               10  MSACN9A PIC  X(0001).
           05  MSACN9I PIC  X(0001).
      *    -------------------------------
           05  MSACM9L PIC S9(0004) COMP.
           05  MSACM9F PIC  X(0001).
           05  FILLER REDEFINES MSACM9F.
               10  MSACM9A PIC  X(0001).
           05  MSACM9I PIC  X(0010).
      *    -------------------------------
           05  CAR10L PIC S9(0004) COMP.
           05  CAR10F PIC  X(0001).
           05  FILLER REDEFINES CAR10F.
               10  CAR10A PIC  X(0001).
           05  CAR10I PIC  X(0001).
      *    -------------------------------
           05  GRP10L PIC S9(0004) COMP.
           05  GRP10F PIC  X(0001).
           05  FILLER REDEFINES GRP10F.
               10  GRP10A PIC  X(0001).
           05  GRP10I PIC  X(0006).
      *    -------------------------------
           05  FRESP10L PIC S9(0004) COMP.
           05  FRESP10F PIC  X(0001).
           05  FILLER REDEFINES FRESP10F.
               10  FRESP10A PIC  X(0001).
           05  FRESP10I PIC  X(0010).
      *    -------------------------------
           05  ACCT10L PIC S9(0004) COMP.
           05  ACCT10F PIC  X(0001).
           05  FILLER REDEFINES ACCT10F.
               10  ACCT10A PIC  X(0001).
           05  ACCT10I PIC  X(0010).
      *    -------------------------------
           05  TYPE10L PIC S9(0004) COMP.
           05  TYPE10F PIC  X(0001).
           05  FILLER REDEFINES TYPE10F.
               10  TYPE10A PIC  X(0001).
           05  TYPE10I PIC  X(0001).
      *    -------------------------------
           05  AMT10L PIC S9(0004) COMP.
           05  AMT10F PIC  X(0001).
           05  FILLER REDEFINES AMT10F.
               10  AMT10A PIC  X(0001).
           05  AMT10I PIC  X(0011).
      *    -------------------------------
           05  MSAAC10L PIC S9(0004) COMP.
           05  MSAAC10F PIC  X(0001).
           05  FILLER REDEFINES MSAAC10F.
               10  MSAAC10A PIC  X(0001).
           05  MSAAC10I PIC  X(0010).
      *    -------------------------------
           05  MSAST10L PIC S9(0004) COMP.
           05  MSAST10F PIC  X(0001).
           05  FILLER REDEFINES MSAST10F.
               10  MSAST10A PIC  X(0001).
           05  MSAST10I PIC  X(0002).
      *    -------------------------------
           05  MSACN10L PIC S9(0004) COMP.
           05  MSACN10F PIC  X(0001).
           05  FILLER REDEFINES MSACN10F.
               10  MSACN10A PIC  X(0001).
           05  MSACN10I PIC  X(0001).
      *    -------------------------------
           05  MSACM10L PIC S9(0004) COMP.
           05  MSACM10F PIC  X(0001).
           05  FILLER REDEFINES MSACM10F.
               10  MSACM10A PIC  X(0001).
           05  MSACM10I PIC  X(0010).
      *    -------------------------------
           05  CAR11L PIC S9(0004) COMP.
           05  CAR11F PIC  X(0001).
           05  FILLER REDEFINES CAR11F.
               10  CAR11A PIC  X(0001).
           05  CAR11I PIC  X(0001).
      *    -------------------------------
           05  GRP11L PIC S9(0004) COMP.
           05  GRP11F PIC  X(0001).
           05  FILLER REDEFINES GRP11F.
               10  GRP11A PIC  X(0001).
           05  GRP11I PIC  X(0006).
      *    -------------------------------
           05  FRESP11L PIC S9(0004) COMP.
           05  FRESP11F PIC  X(0001).
           05  FILLER REDEFINES FRESP11F.
               10  FRESP11A PIC  X(0001).
           05  FRESP11I PIC  X(0010).
      *    -------------------------------
           05  ACCT11L PIC S9(0004) COMP.
           05  ACCT11F PIC  X(0001).
           05  FILLER REDEFINES ACCT11F.
               10  ACCT11A PIC  X(0001).
           05  ACCT11I PIC  X(0010).
      *    -------------------------------
           05  TYPE11L PIC S9(0004) COMP.
           05  TYPE11F PIC  X(0001).
           05  FILLER REDEFINES TYPE11F.
               10  TYPE11A PIC  X(0001).
           05  TYPE11I PIC  X(0001).
      *    -------------------------------
           05  AMT11L PIC S9(0004) COMP.
           05  AMT11F PIC  X(0001).
           05  FILLER REDEFINES AMT11F.
               10  AMT11A PIC  X(0001).
           05  AMT11I PIC  X(0011).
      *    -------------------------------
           05  MSAAC11L PIC S9(0004) COMP.
           05  MSAAC11F PIC  X(0001).
           05  FILLER REDEFINES MSAAC11F.
               10  MSAAC11A PIC  X(0001).
           05  MSAAC11I PIC  X(0010).
      *    -------------------------------
           05  MSAST11L PIC S9(0004) COMP.
           05  MSAST11F PIC  X(0001).
           05  FILLER REDEFINES MSAST11F.
               10  MSAST11A PIC  X(0001).
           05  MSAST11I PIC  X(0002).
      *    -------------------------------
           05  MSACN11L PIC S9(0004) COMP.
           05  MSACN11F PIC  X(0001).
           05  FILLER REDEFINES MSACN11F.
               10  MSACN11A PIC  X(0001).
           05  MSACN11I PIC  X(0001).
      *    -------------------------------
           05  MSACM11L PIC S9(0004) COMP.
           05  MSACM11F PIC  X(0001).
           05  FILLER REDEFINES MSACM11F.
               10  MSACM11A PIC  X(0001).
           05  MSACM11I PIC  X(0010).
      *    -------------------------------
           05  CAR12L PIC S9(0004) COMP.
           05  CAR12F PIC  X(0001).
           05  FILLER REDEFINES CAR12F.
               10  CAR12A PIC  X(0001).
           05  CAR12I PIC  X(0001).
      *    -------------------------------
           05  GRP12L PIC S9(0004) COMP.
           05  GRP12F PIC  X(0001).
           05  FILLER REDEFINES GRP12F.
               10  GRP12A PIC  X(0001).
           05  GRP12I PIC  X(0006).
      *    -------------------------------
           05  FRESP12L PIC S9(0004) COMP.
           05  FRESP12F PIC  X(0001).
           05  FILLER REDEFINES FRESP12F.
               10  FRESP12A PIC  X(0001).
           05  FRESP12I PIC  X(0010).
      *    -------------------------------
           05  ACCT12L PIC S9(0004) COMP.
           05  ACCT12F PIC  X(0001).
           05  FILLER REDEFINES ACCT12F.
               10  ACCT12A PIC  X(0001).
           05  ACCT12I PIC  X(0010).
      *    -------------------------------
           05  TYPE12L PIC S9(0004) COMP.
           05  TYPE12F PIC  X(0001).
           05  FILLER REDEFINES TYPE12F.
               10  TYPE12A PIC  X(0001).
           05  TYPE12I PIC  X(0001).
      *    -------------------------------
           05  AMT12L PIC S9(0004) COMP.
           05  AMT12F PIC  X(0001).
           05  FILLER REDEFINES AMT12F.
               10  AMT12A PIC  X(0001).
           05  AMT12I PIC  X(0011).
      *    -------------------------------
           05  MSAAC12L PIC S9(0004) COMP.
           05  MSAAC12F PIC  X(0001).
           05  FILLER REDEFINES MSAAC12F.
               10  MSAAC12A PIC  X(0001).
           05  MSAAC12I PIC  X(0010).
      *    -------------------------------
           05  MSAST12L PIC S9(0004) COMP.
           05  MSAST12F PIC  X(0001).
           05  FILLER REDEFINES MSAST12F.
               10  MSAST12A PIC  X(0001).
           05  MSAST12I PIC  X(0002).
      *    -------------------------------
           05  MSACN12L PIC S9(0004) COMP.
           05  MSACN12F PIC  X(0001).
           05  FILLER REDEFINES MSACN12F.
               10  MSACN12A PIC  X(0001).
           05  MSACN12I PIC  X(0001).
      *    -------------------------------
           05  MSACM12L PIC S9(0004) COMP.
           05  MSACM12F PIC  X(0001).
           05  FILLER REDEFINES MSACM12F.
               10  MSACM12A PIC  X(0001).
           05  MSACM12I PIC  X(0010).
      *    -------------------------------
           05  CAR13L PIC S9(0004) COMP.
           05  CAR13F PIC  X(0001).
           05  FILLER REDEFINES CAR13F.
               10  CAR13A PIC  X(0001).
           05  CAR13I PIC  X(0001).
      *    -------------------------------
           05  GRP13L PIC S9(0004) COMP.
           05  GRP13F PIC  X(0001).
           05  FILLER REDEFINES GRP13F.
               10  GRP13A PIC  X(0001).
           05  GRP13I PIC  X(0006).
      *    -------------------------------
           05  FRESP13L PIC S9(0004) COMP.
           05  FRESP13F PIC  X(0001).
           05  FILLER REDEFINES FRESP13F.
               10  FRESP13A PIC  X(0001).
           05  FRESP13I PIC  X(0010).
      *    -------------------------------
           05  ACCT13L PIC S9(0004) COMP.
           05  ACCT13F PIC  X(0001).
           05  FILLER REDEFINES ACCT13F.
               10  ACCT13A PIC  X(0001).
           05  ACCT13I PIC  X(0010).
      *    -------------------------------
           05  TYPE13L PIC S9(0004) COMP.
           05  TYPE13F PIC  X(0001).
           05  FILLER REDEFINES TYPE13F.
               10  TYPE13A PIC  X(0001).
           05  TYPE13I PIC  X(0001).
      *    -------------------------------
           05  AMT13L PIC S9(0004) COMP.
           05  AMT13F PIC  X(0001).
           05  FILLER REDEFINES AMT13F.
               10  AMT13A PIC  X(0001).
           05  AMT13I PIC  X(0011).
      *    -------------------------------
           05  MSAAC13L PIC S9(0004) COMP.
           05  MSAAC13F PIC  X(0001).
           05  FILLER REDEFINES MSAAC13F.
               10  MSAAC13A PIC  X(0001).
           05  MSAAC13I PIC  X(0010).
      *    -------------------------------
           05  MSAST13L PIC S9(0004) COMP.
           05  MSAST13F PIC  X(0001).
           05  FILLER REDEFINES MSAST13F.
               10  MSAST13A PIC  X(0001).
           05  MSAST13I PIC  X(0002).
      *    -------------------------------
           05  MSACN13L PIC S9(0004) COMP.
           05  MSACN13F PIC  X(0001).
           05  FILLER REDEFINES MSACN13F.
               10  MSACN13A PIC  X(0001).
           05  MSACN13I PIC  X(0001).
      *    -------------------------------
           05  MSACM13L PIC S9(0004) COMP.
           05  MSACM13F PIC  X(0001).
           05  FILLER REDEFINES MSACM13F.
               10  MSACM13A PIC  X(0001).
           05  MSACM13I PIC  X(0010).
      *    -------------------------------
           05  CAR14L PIC S9(0004) COMP.
           05  CAR14F PIC  X(0001).
           05  FILLER REDEFINES CAR14F.
               10  CAR14A PIC  X(0001).
           05  CAR14I PIC  X(0001).
      *    -------------------------------
           05  GRP14L PIC S9(0004) COMP.
           05  GRP14F PIC  X(0001).
           05  FILLER REDEFINES GRP14F.
               10  GRP14A PIC  X(0001).
           05  GRP14I PIC  X(0006).
      *    -------------------------------
           05  FRESP14L PIC S9(0004) COMP.
           05  FRESP14F PIC  X(0001).
           05  FILLER REDEFINES FRESP14F.
               10  FRESP14A PIC  X(0001).
           05  FRESP14I PIC  X(0010).
      *    -------------------------------
           05  ACCT14L PIC S9(0004) COMP.
           05  ACCT14F PIC  X(0001).
           05  FILLER REDEFINES ACCT14F.
               10  ACCT14A PIC  X(0001).
           05  ACCT14I PIC  X(0010).
      *    -------------------------------
           05  TYPE14L PIC S9(0004) COMP.
           05  TYPE14F PIC  X(0001).
           05  FILLER REDEFINES TYPE14F.
               10  TYPE14A PIC  X(0001).
           05  TYPE14I PIC  X(0001).
      *    -------------------------------
           05  AMT14L PIC S9(0004) COMP.
           05  AMT14F PIC  X(0001).
           05  FILLER REDEFINES AMT14F.
               10  AMT14A PIC  X(0001).
           05  AMT14I PIC  X(0011).
      *    -------------------------------
           05  MSAAC14L PIC S9(0004) COMP.
           05  MSAAC14F PIC  X(0001).
           05  FILLER REDEFINES MSAAC14F.
               10  MSAAC14A PIC  X(0001).
           05  MSAAC14I PIC  X(0010).
      *    -------------------------------
           05  MSAST14L PIC S9(0004) COMP.
           05  MSAST14F PIC  X(0001).
           05  FILLER REDEFINES MSAST14F.
               10  MSAST14A PIC  X(0001).
           05  MSAST14I PIC  X(0002).
      *    -------------------------------
           05  MSACN14L PIC S9(0004) COMP.
           05  MSACN14F PIC  X(0001).
           05  FILLER REDEFINES MSACN14F.
               10  MSACN14A PIC  X(0001).
           05  MSACN14I PIC  X(0001).
      *    -------------------------------
           05  MSACM14L PIC S9(0004) COMP.
           05  MSACM14F PIC  X(0001).
           05  FILLER REDEFINES MSACM14F.
               10  MSACM14A PIC  X(0001).
           05  MSACM14I PIC  X(0010).
      *    -------------------------------
           05  CAR15L PIC S9(0004) COMP.
           05  CAR15F PIC  X(0001).
           05  FILLER REDEFINES CAR15F.
               10  CAR15A PIC  X(0001).
           05  CAR15I PIC  X(0001).
      *    -------------------------------
           05  GRP15L PIC S9(0004) COMP.
           05  GRP15F PIC  X(0001).
           05  FILLER REDEFINES GRP15F.
               10  GRP15A PIC  X(0001).
           05  GRP15I PIC  X(0006).
      *    -------------------------------
           05  FRESP15L PIC S9(0004) COMP.
           05  FRESP15F PIC  X(0001).
           05  FILLER REDEFINES FRESP15F.
               10  FRESP15A PIC  X(0001).
           05  FRESP15I PIC  X(0010).
      *    -------------------------------
           05  ACCT15L PIC S9(0004) COMP.
           05  ACCT15F PIC  X(0001).
           05  FILLER REDEFINES ACCT15F.
               10  ACCT15A PIC  X(0001).
           05  ACCT15I PIC  X(0010).
      *    -------------------------------
           05  TYPE15L PIC S9(0004) COMP.
           05  TYPE15F PIC  X(0001).
           05  FILLER REDEFINES TYPE15F.
               10  TYPE15A PIC  X(0001).
           05  TYPE15I PIC  X(0001).
      *    -------------------------------
           05  AMT15L PIC S9(0004) COMP.
           05  AMT15F PIC  X(0001).
           05  FILLER REDEFINES AMT15F.
               10  AMT15A PIC  X(0001).
           05  AMT15I PIC  X(0011).
      *    -------------------------------
           05  MSAAC15L PIC S9(0004) COMP.
           05  MSAAC15F PIC  X(0001).
           05  FILLER REDEFINES MSAAC15F.
               10  MSAAC15A PIC  X(0001).
           05  MSAAC15I PIC  X(0010).
      *    -------------------------------
           05  MSAST15L PIC S9(0004) COMP.
           05  MSAST15F PIC  X(0001).
           05  FILLER REDEFINES MSAST15F.
               10  MSAST15A PIC  X(0001).
           05  MSAST15I PIC  X(0002).
      *    -------------------------------
           05  MSACN15L PIC S9(0004) COMP.
           05  MSACN15F PIC  X(0001).
           05  FILLER REDEFINES MSACN15F.
               10  MSACN15A PIC  X(0001).
           05  MSACN15I PIC  X(0001).
      *    -------------------------------
           05  MSACM15L PIC S9(0004) COMP.
           05  MSACM15F PIC  X(0001).
           05  FILLER REDEFINES MSACM15F.
               10  MSACM15A PIC  X(0001).
           05  MSACM15I PIC  X(0010).
      *    -------------------------------
           05  ERRMSG1L PIC S9(0004) COMP.
           05  ERRMSG1F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG1F.
               10  ERRMSG1A PIC  X(0001).
           05  ERRMSG1I PIC  X(0079).
      *    -------------------------------
           05  ERRMSG2L PIC S9(0004) COMP.
           05  ERRMSG2F PIC  X(0001).
           05  FILLER REDEFINES ERRMSG2F.
               10  ERRMSG2A PIC  X(0001).
           05  ERRMSG2I PIC  X(0079).
      *    -------------------------------
           05  PFENTERL PIC S9(0004) COMP.
           05  PFENTERF PIC  X(0001).
           05  FILLER REDEFINES PFENTERF.
               10  PFENTERA PIC  X(0001).
           05  PFENTERI PIC  9(2).
      *    -------------------------------
           05  GAMTL PIC S9(0004) COMP.
           05  GAMTF PIC  X(0001).
           05  FILLER REDEFINES GAMTF.
               10  GAMTA PIC  X(0001).
           05  GAMTI PIC  X(0011).
       01  EL633BO REDEFINES EL633BI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CMPNYIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP1O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT1O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST1O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP2O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT2O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST2O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM2O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP3O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT3O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST3O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN3O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM3O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP4O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT4O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST4O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN4O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM4O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP5O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT5O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST5O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN5O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM5O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP6O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT6O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST6O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN6O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM6O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP7O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT7O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST7O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN7O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM7O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP8O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT8O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST8O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN8O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM8O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP9O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT9O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST9O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN9O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP10O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT10O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST10O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN10O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM10O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP11O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT11O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST11O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN11O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM11O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP12O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT12O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST12O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN12O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM12O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP13O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT13O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST13O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN13O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM13O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP14O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST14O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN14O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM14O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CAR15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GRP15O PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FRESP15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCT15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPE15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMT15O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAAC15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSAST15O PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACN15O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSACM15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSG2O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFENTERO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GAMTO PIC  X(0011).
      *    -------------------------------
00160
00161  01  MAP-EL633B   REDEFINES  EL633BI.
101101     12  FILLER                  PIC  X(44).
060205*    12  DATA-AREA       OCCURS  6 TIMES
060205     12  DATA-AREA       OCCURS 15 TIMES
00164                              INDEXED BY INDX.
00165          16  CARR-LEN            PIC S9(4)              COMP.
00166          16  CARR-ATTRB          PIC  X.
00167          16  CARRIER             PIC  X.
00168          16  GRP-LEN             PIC S9(4)              COMP.
00169          16  GRP-ATTRB           PIC  X.
00170          16  GROUPING            PIC  X(6).
00171          16  FIN-LEN             PIC S9(4)              COMP.
00172          16  FIN-ATTRB           PIC  X.
00173          16  FIN-RESP            PIC  X(10).
00174          16  ACCT-LEN            PIC S9(4)              COMP.
00175          16  ACCT-ATTRB          PIC  X.
00176          16  ACCT                PIC  X(10).
00177          16  RTYPE-LEN           PIC S9(4)              COMP.
00178          16  RTYPE-ATTRB         PIC  X.
00179          16  RTYPE               PIC  X.
00180          16  AMT-LEN             PIC S9(4)              COMP.
00181          16  AMT-ATTRB           PIC  X.
00182          16  AMT                 PIC S9(9)V99.
00183          16  AMTO  REDEFINES
00184              AMT                 PIC Z(7).9(2)-.
CIDMOD         16  GL-ACCT-LEN         PIC S9(4)              COMP.
CIDMOD         16  GL-ACCT-ATTRB       PIC  X.
CIDMOD         16  GL-ACCT             PIC  X(10).
CIDMOD         16  WSL-COMM  REDEFINES GL-ACCT.
CIDMOD             20  WSL-COMM-DTE.
CIDMOD                 24  WSL-MO      PIC  X(2).
CIDMOD                 24  WSL-DA      PIC  X(2).
CIDMOD                 24  WSL-YR      PIC  X(2).
CIDMOD             20  FILLER          PIC  X(4).
CIDMOD         16  GL-STATE-LEN        PIC S9(4)              COMP.
CIDMOD         16  GL-STATE-ATTRB      PIC  X.
CIDMOD         16  GL-STATE            PIC  X(02).
CIDMOD         16  GL-CANC-LEN         PIC S9(4)              COMP.
CIDMOD         16  GL-CANC-ATTRB       PIC  X.
CIDMOD         16  GL-CANC             PIC  X(01).
CIDMOD         16  GL-COMM-LEN         PIC S9(4)              COMP.
CIDMOD         16  GL-COMM-ATTRB       PIC  X.
CIDMOD         16  GL-COMM             PIC  X(10).
CIDMOD         16  FILLER  REDEFINES GL-COMM.
CIDMOD             20  GL-CHECK-NO     PIC  9(06).
CIDMOD             20  FILLER          PIC  X(04).
031710     12  MSG1-LEN                PIC S9(4)  COMP.
031710     12  MSG1-ATTRB              PIC X.
031710     12  MSG1                    PIC X(79).
031710     12  MSG2-LEN                PIC S9(4)  COMP.
031710     12  MSG2-ATTRB              PIC X.
031710     12  MSG2                    PIC X(79).
031710     12  FILLER                  PIC X(5).
031710     12  GAMT-LEN                PIC S9(4)  COMP.
031710     12  GAMT-ATTRB              PIC X.
031710     12  GROSS-AMT               PIC S9(9)V99.
031710     12  GROSS-AMTO REDEFINES GROSS-AMT
031710                                 PIC Z(7).9(2)-.
CIDMOD
CIDMOD/
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
00197  01  DFHCOMMAREA             PIC  X(1024).
00198  EJECT
00204 *                            COPY ERCPYAJ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCPYAJ                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.015                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING PAYMENT AND ADJUSTMENTS           *
00008 *                                                                *
00009 *                                                                *
00010 *   FILE TYPE = VSAM,KSDS                                        *
00011 *   RECORD SIZE = 200  RECFORM = FIXED                           *
00012 *                                                                *
00013 *   BASE CLUSTER = ERPYAJ                         RKP=2,LEN=33   *
00014 *       ALTERNATE PATHS = NONE                                   *
00015 *                                                                *
00016 *   LOG = YES                                                    *
00017 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00018 ******************************************************************
042303******************************************************************
042303*                   C H A N G E   L O G
042303*
042303* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
042303*-----------------------------------------------------------------
042303*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
042303* EFFECTIVE    NUMBER
042303*-----------------------------------------------------------------
042303* 042303                   PEMA ADD PROCESSING FOR DUE PREM ADJS
060205* 060205                   PEMA ADD ERCOMP TYPE TO ERPYAJ
042303******************************************************************
00019
00020  01  PENDING-PAY-ADJ.
00021      12  PY-RECORD-ID                     PIC XX.
00022          88  VALID-PY-ID                        VALUE 'PY'.
00023
00024      12  PY-CONTROL-PRIMARY.
00025          16  PY-COMPANY-CD                PIC X.
00026          16  PY-CARRIER                   PIC X.
00027          16  PY-GROUPING                  PIC X(6).
00028          16  PY-FIN-RESP                  PIC X(10).
00029          16  PY-ACCOUNT                   PIC X(10).
00030          16  PY-PRODUCER REDEFINES PY-ACCOUNT
00031                                           PIC X(10).
00032          16  PY-FILE-SEQ-NO               PIC S9(8)     COMP.
00033          16  PY-RECORD-TYPE               PIC X.
00034              88  PY-REMIT-RECEIVED            VALUE 'R'.
00035              88  PY-DEPOSIT                   VALUE 'D'.
00036              88  PY-CHARGE-TO-AGENT           VALUE 'C'.
00037              88  PY-ADJ-REM-RECEIVED          VALUE 'S'.
00038              88  PY-ADJ-DEPOSIT               VALUE 'T'.
00039              88  PY-ADJ-CHG-TO-AGT            VALUE 'U'.
00040              88  PY-ADD-TO-YTD-COMP           VALUE 'X'.
00041              88  PY-SUBTRACT-YTD-COMP         VALUE 'Y'.
00042              88  PY-ADD-TO-BALANCE            VALUE 'Z'.
00043              88  PY-FICA-ENTRY                VALUE 'F'.
00044              88  PY-REMIT-IND-GROUPING        VALUE 'G'.
00045              88  PY-POLICY-FEE                VALUE 'W'.
042303             88  PY-DUE-PREM-ADJ              VALUE 'P'.
00046
00047      12  PY-PYMT-TYPE                     PIC X.
00048              88  PY-NEW-BUS-PYMT              VALUE 'B'.
00049              88  PY-REINS-PYMT                VALUE 'R'.
00050              88  PY-EXP-PYMT                  VALUE 'E'.
00051
00052      12  PY-BIL-INV                       PIC X(6).
00053      12  PY-REF-NO                        PIC X(12).
00054
00055      12  PY-LAST-MAINT-DT                 PIC XX.
00056      12  PY-LAST-MAINT-BY                 PIC X(4).
00057      12  PY-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00058
00059      12  PY-PYADJ-RECORD.
00060          16  PY-ENTRY-AMT                 PIC S9(7)V99  COMP-3.
00061          16  PY-ENTRY-COMMENT             PIC X(30).
CIDMOD         16  PY-GL-DATA      REDEFINES PY-ENTRY-COMMENT.
CIDMOD             20  PY-GL-ACCOUNT            PIC X(10).
CIDMOD             20  PY-GL-STATE              PIC X(02).
CIDMOD             20  PY-GL-CANC-SW            PIC X(01).
CIDMOD                 88  PY-GL-CANC-SW-ON     VALUE 'Y'.
CIDMOD                 88  PY-GL-CANC-SW-OFF    VALUE 'N'.
CIDMOD             20  PY-GL-COMMENT            PIC X(10).
CIDMOD             20  FILLER      REDEFINES PY-GL-COMMENT.
CIDMOD                 24  PY-GL-CHECK-NO       PIC 9(06).
CIDMOD                 24  FILLER               PIC X(04).
CIDMOD             20  FILLER                   PIC X(07).
00074          16  PY-SAVE-ACCOUNT              PIC X(10).
00075          16  PY-SAVE-TYPE                 PIC X(01).
00076
00077          16  PY-LETTERS.
00078              20  PY-LETTER OCCURS 3 TIMES
00079                            INDEXED BY PY-LET-NDX
00080                                           PIC X(04).
00081
060205         16  PY-ERCOMP-TYPE               PIC X.
060205             88  PY-ACCOUNT-TYPE              VALUE 'A'.
060205             88  PY-GA-TYPE                   VALUE 'G'.
060205             88  PY-BANK-TYPE                 VALUE 'B'.
060205         16  FILLER                       PIC X(05).
00083
00084      12  PY-RECORD-STATUS.
00085          16  PY-CREDIT-SELECT-DT          PIC XX.
00086          16  PY-CREDIT-ACCEPT-DT          PIC XX.
00087          16  PY-BILLED-DATE               PIC XX.
00088          16  PY-REPORTED-DT               PIC XX.
00089          16  PY-PMT-APPLIED               PIC X.
00090              88  PY-ACCOUNT-PMT               VALUE 'A'.
00091              88  PY-GA-PMT                    VALUE 'G'.
00092              88  PY-OVWRITE-PMT               VALUE 'O'.
00093              88  PY-NON-AR-PMT                VALUE 'N'.
00094          16  FILLER                       PIC X(5).
00095          16  PY-INPUT-DT                  PIC XX.
00096          16  PY-CHECK-NUMBER              PIC X(6).
00097          16  PY-VOID-SW                   PIC X.
00098              88  PY-CHECK-VOIDED              VALUE 'V'.
00099          16  PY-CHECK-ORIGIN-SW           PIC X.
00100              88  PY-BILLING-CHECK             VALUE 'B'.
00101              88  PY-REFUND-CHECK              VALUE 'R'.
00102              88  PY-GA-CHECK                  VALUE 'G'.
00103              88  PY-CHECK-WRITTEN             VALUE 'W'.
00104              88  PY-CHECK-REVERSAL            VALUE 'V'.
00105          16  PY-CHECK-WRITTEN-DT          PIC XX.
00106          16  PY-CHECK-QUE-CONTROL         PIC S9(8) COMP.
00107          16  PY-CHECK-QUE-SEQUENCE        PIC S9(4) COMP.
00108          16  PY-BILL-FLAG                 PIC X.
00109              88  PY-BILLED                    VALUE 'B'.
00110          16  PY-AR-FLAG                   PIC X.
00111              88  PY-AR-CYCLE                  VALUE 'C'.
00112              88  PY-AR-MONTH-END              VALUE 'M'.
00113          16  PY-AR-DATE                   PIC XX.
00114
00115      12  PY-GL-CODES.
00116          16  PY-GL-DB                     PIC X(14).
00117          16  PY-GL-CR                     PIC X(14).
00118          16  PY-GL-FLAG                   PIC X.
00119          16  PY-GL-DATE                   PIC XX.
00120
00121      12  PY-CANCEL-FEE-FLAG               PIC X(2).
00122      12  FILLER                           PIC X(3).
00123 ******************************************************************
00205  EJECT
00206 *                            COPY ERCCOMP.
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
00207  EJECT
CIDMOD*
CIDMOD*                            COPY AIRL0009.
CIDMOD/
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-PAY-ADJ
                                COMPENSATION-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL6331' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00209
00210      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00211      MOVE 2                      TO  EMI-NUMBER-OF-LINES.
00212
00213      IF EIBCALEN = ZERO
00214          GO TO 8800-UNAUTHORIZED-ACCESS.
00215
00216      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00217      MOVE '5'                    TO  DC-OPTION-CODE.
00218
00219      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00220
00221      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-BIN-DT.
00222      MOVE DC-GREG-DATE-1-EDIT    TO  WS-CURRENT-DT.
00223
00224      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00225          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00226              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6
00227              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5
00228              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4
00229              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3
00230              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2
00231              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1
00232              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM
00233              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM
00234          ELSE
00235              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM
00236              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM
00237              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1
00238              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2
00239              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3
00240              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4
00241              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5
00242              MOVE SPACES                TO  PI-SAVED-PROGRAM-6.
00243
00244      MOVE LOW-VALUES             TO  EL633BI.
00245
00246      COMPUTE WORK-SEQ-NO  =  EIBTIME  *  10.
00247
00248      IF EIBTRNID NOT = TRANS-ID
00249          MOVE SPACE              TO  PI-PYAJ-FILE-SW
00250          GO TO 8100-SEND-INITIAL-MAP.
00251
00252      
      * EXEC CICS HANDLE CONDITION
00253 *        PGMIDERR  (9600-PGMID-ERROR)
00254 *        ERROR     (9990-ABEND)
00255 *        END-EXEC.
      *    MOVE '"$L.                  ! " #00003160' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00256
00257      IF EIBAID = DFHCLEAR
00258          GO TO 9400-CLEAR.
00259  EJECT
00260  0200-RECEIVE.
00261      IF EIBAID = DFHPA1              OR  DFHPA2  OR  DFHPA3
00262          MOVE ER-0008            TO  EMI-ERROR
00263          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00264          MOVE -1                 TO  PFENTERL
00265          GO TO 8200-SEND-DATAONLY.
00266
00267      
      * EXEC CICS RECEIVE
00268 *        MAP     (MAP-NAME)
00269 *        MAPSET  (MAPSET-NAME)
00270 *        INTO    (EL633BI)
00271 *        END-EXEC.
           MOVE LENGTH OF
            EL633BI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003175' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033313735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633BI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00272
00273      IF PFENTERL = ZERO
00274          GO TO 0300-CHECK-PFKEYS.
00275
00276      IF (PFENTERI  IS NUMERIC)
00277        AND (PFENTERI  IS GREATER THAN  ZERO
00278        AND  IS LESS THAN  25)
00279          MOVE PF-VALUES (PFENTERI)  TO  EIBAID
00280      ELSE
00281          MOVE ER-0029               TO  EMI-ERROR
00282          GO TO 0320-INPUT-ERROR.
00283
00284  0300-CHECK-PFKEYS.
00285      IF EIBAID = DFHPF23
00286          GO TO 8810-PF23.
00287
00288      IF EIBAID = DFHPF24
00289          GO TO 9200-RETURN-MAIN-MENU.
00290
00291      IF EIBAID = DFHPF12
00292          GO TO 9500-PF12.
00293
00294      IF EIBAID = DFHENTER
00295          GO TO 1000-EDIT-DATA.
CIDMOD
CIDMOD     IF EIBAID = DFHPF1
CIDMOD        GO TO 1000-EDIT-DATA.
00296
00297  0320-INPUT-ERROR.
00298      MOVE ER-0029                TO  EMI-ERROR.
00299
00300      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00301
00302      MOVE AL-UNBON               TO  PFENTERA.
00303
00304      IF PFENTERL = ZERO
00305          MOVE -1                 TO  PFENTERL
00306      ELSE
00307          MOVE -1                 TO  PFENTERL.
00308
00309      GO TO 8200-SEND-DATAONLY.
00310  EJECT
00311  1000-EDIT-DATA.
00312      IF NOT MODIFY-CAP
00313          MOVE 'UPDATE'       TO SM-READ
00314          PERFORM 9995-SECURITY-VIOLATION
00315          MOVE ER-0070        TO EMI-ERROR
00316          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00317          GO TO 8100-SEND-INITIAL-MAP.
00318
00319      MOVE PI-COMPANY-CD          TO  PI-SAV-COMP-CD
00320                                      COMP-COMP-CD.
00321
00322      SET INDX                    TO  1.
060205     MOVE +1                     TO C1
031710     MOVE ZEROS                  TO TOTAL-AMOUNT
00323
           .
00324  1010-EDIT-LOOP.
00325      IF CARR-LEN (INDX) = ZEROS
00326        AND GRP-LEN (INDX) = ZEROS
00327        AND FIN-LEN (INDX) = ZEROS
00328        AND ACCT-LEN (INDX) = ZEROS
CIDMOD       AND GL-ACCT-LEN (INDX) = ZEROS
CIDMOD       AND GL-STATE-LEN (INDX) = ZEROS
CIDMOD       AND GL-CANC-LEN (INDX) = ZEROS
CIDMOD       AND GL-COMM-LEN (INDX) = ZEROS
00330        AND RTYPE-LEN (INDX) = ZEROS
00331        AND AMT-LEN (INDX) = ZEROS
00332          GO TO 1040-INCREMENT-INDX.
00333
00334      IF CARR-LEN (INDX) NOT = ZEROS
00335          MOVE AL-UANON           TO  CARR-ATTRB (INDX)
00336          MOVE CARRIER (INDX)     TO  COMP-CARRIER
00337                                      PI-SAV-CARRIER
00338          IF CARRIER (INDX) NOT = ZEROS
00339            AND (PI-ZERO-CARRIER  OR  PI-ZERO-CAR-GROUP)
00340              MOVE ER-2587        TO  EMI-ERROR
00341              MOVE -1             TO  CARR-LEN (INDX)
00342              MOVE AL-UABON       TO  CARR-ATTRB (INDX)
00343              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00344          ELSE
00345              NEXT SENTENCE
00346      ELSE
00347          MOVE ER-0194            TO  EMI-ERROR
00348          MOVE -1                 TO  CARR-LEN (INDX)
00349          MOVE AL-UABON           TO  CARR-ATTRB (INDX)
00350          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00351
00352      IF GRP-LEN (INDX) NOT = ZEROS
00353          MOVE AL-UANON           TO  GRP-ATTRB (INDX)
00354          MOVE GROUPING (INDX)    TO  COMP-GROUPING
00355                                      PI-SAV-GROUPING
00356          IF GROUPING (INDX) NOT = ZEROS
00357            AND (PI-ZERO-GROUPING  OR  PI-ZERO-CAR-GROUP)
00358              MOVE ER-2588        TO  EMI-ERROR
00359              MOVE -1             TO  GRP-LEN (INDX)
00360              MOVE AL-UABON       TO  GRP-ATTRB (INDX)
00361              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00362          ELSE
00363              NEXT SENTENCE
00364      ELSE
00365          MOVE ER-0195            TO  EMI-ERROR
00366          MOVE -1                 TO  GRP-LEN (INDX)
00367          MOVE AL-UABON           TO  GRP-ATTRB (INDX)
00368          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00369
00370      IF FIN-LEN (INDX) NOT = ZEROS
00371          MOVE AL-UANON           TO  FIN-ATTRB (INDX)
00372          MOVE FIN-RESP (INDX)    TO  COMP-FIN-RESP
00373                                      PI-SAV-FIN-RESP
00374                                      PI-CR-FIN-RESP
00375      ELSE
00376          MOVE ER-2562            TO  EMI-ERROR
00377          MOVE -1                 TO  FIN-LEN (INDX)
00378          MOVE AL-UABON           TO  FIN-ATTRB (INDX)
00379          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00380
00381      IF ACCT-LEN (INDX) NOT = ZEROS
00382          MOVE AL-UANON           TO  ACCT-ATTRB (INDX)
00383          MOVE ACCT (INDX)        TO  COMP-ACCOUNT
00384                                      PI-SAV-ACCOUNT
00385      ELSE
00386          MOVE LOW-VALUES         TO  COMP-ACCOUNT
00387                                      PI-SAV-ACCOUNT.
00388
00389      
      * EXEC CICS HANDLE CONDITION
00390 *        NOTFND   (1020-NO-COMP-MSTR)
00391 *        NOTOPEN  (7100-COMP-FILE-NOTOPEN)
00392 *        END-EXEC.
      *    MOVE '"$IJ                  ! # #00003306' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033333036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00393
00394      MOVE SPACES                 TO  COMP-RECORD-TYPE.
00395
00396      
      * EXEC CICS READ
00397 *        DATASET  (COMP-FILE-ID)
00398 *        SET      (ADDRESS OF COMPENSATION-MASTER)
00399 *        RIDFLD   (ERCOMP-KEY)
00400 *        GTEQ
00401 *        END-EXEC.
      *    MOVE '&"S        G          (   #00003313' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00402
CIDMOD     MOVE COMPENSATION-MASTER    TO SV-COMP.
00403      IF PI-COMPANY-CD = CO-COMPANY-CD
00404        AND COMP-CARRIER = CO-CARRIER
00405        AND COMP-GROUPING = CO-GROUPING
00406        AND COMP-FIN-RESP = CO-RESP-NO
00407        AND COMP-ACCOUNT = CO-ACCOUNT
00408          GO TO 1025-CHECK-STATUS.
00409
00410  1020-NO-COMP-MSTR.
00411      MOVE ER-2230                TO  EMI-ERROR.
00412      MOVE -1                     TO  CARR-LEN (INDX)
00413      MOVE AL-UABON               TO  CARR-ATTRB (INDX)
00414                                      GRP-ATTRB (INDX)
00415                                      FIN-ATTRB (INDX)
00416                                      ACCT-ATTRB (INDX)
00417
00418      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00419
00420      GO TO 1030-CONTINUE-EDIT.
00421
00422  1025-CHECK-STATUS.
060205     MOVE CO-TYPE                TO WS-ERCOMP-TYPE (C1)
00423      IF PI-COMPANY-ID = 'NCL'
00424          IF CO-GA-INACTIVE
00425              MOVE ER-2763            TO  EMI-ERROR
00426              MOVE -1                 TO  CARR-LEN   (INDX)
00427              MOVE AL-UABON           TO  CARR-ATTRB (INDX)
00428                                          GRP-ATTRB  (INDX)
00429                                          FIN-ATTRB  (INDX)
00430                                          ACCT-ATTRB (INDX)
00431              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00432
00433  1030-CONTINUE-EDIT.
CIDMOD     IF GL-ACCT-LEN (INDX) NOT = ZEROS
CIDMOD         MOVE AL-UANON           TO  GL-ACCT-ATTRB (INDX)
00436          IF PI-COMPANY-ID NOT = 'WSL'
00437              NEXT SENTENCE
00438          ELSE
00439              MOVE WSL-COMM-DTE (INDX)  TO  DC-GREG-DATE-1-MDY
00440              MOVE '4'                  TO  DC-OPTION-CODE
00441              PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT
00442              IF NO-CONVERSION-ERROR
00443                  NEXT SENTENCE
00444              ELSE
00445                  MOVE ER-2595    TO  EMI-ERROR
CIDMOD                 MOVE -1         TO  GL-ACCT-LEN (INDX)
CIDMOD                 MOVE AL-UABON   TO  GL-ACCT-ATTRB (INDX)
00448                  PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00449      ELSE
00450          IF PI-COMPANY-ID = 'WSL'
00451              MOVE ER-2596        TO  EMI-ERROR
CIDMOD             MOVE -1             TO  GL-ACCT-LEN (INDX)
CIDMOD             MOVE AL-UABON       TO  GL-ACCT-ATTRB (INDX)
00454              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00455
00456      IF RTYPE-LEN (INDX) NOT = ZEROS
00457          MOVE RTYPE (INDX)       TO  CHECK-REC-TYPE
021716         IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303            AND (RTYPE (INDX) = 'P')
042303            SET VALID-REC-TYPE   TO TRUE
042303         END-IF
00458          IF NOT VALID-REC-TYPE
00459              MOVE -1             TO  RTYPE-LEN (INDX)
00460              MOVE ER-2234        TO  EMI-ERROR
00461              MOVE AL-UABON       TO  RTYPE-ATTRB (INDX)
00462              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00463          ELSE
00464              MOVE AL-UANON       TO  RTYPE-ATTRB (INDX)
00465      ELSE
00466          MOVE -1                 TO  RTYPE-LEN (INDX)
00467          MOVE ER-2235            TO  EMI-ERROR
00468          MOVE AL-UABON           TO  RTYPE-ATTRB (INDX)
00469          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00470
00471      IF AMT-LEN (INDX) NOT = ZEROS
00472          MOVE AL-UNNON           TO  AMT-ATTRB (INDX)
00473          
      * EXEC CICS BIF DEEDIT
00474 *             FIELD (AMT (INDX))
00475 *             LENGTH (11)
00476 *        END-EXEC
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003396' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AMT(INDX), 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00477          IF AMT (INDX) = ZEROS
00478              MOVE ER-2245        TO  EMI-ERROR
00479              MOVE -1             TO  AMT-LEN(INDX)
00480              MOVE AL-UNBON       TO  AMT-ATTRB (INDX)
00481              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00482          ELSE
00483              SET WS-INDX          TO INDX
00484              MOVE AMT(INDX)      TO WS-EDITED-AMT (WS-INDX)
031710             IF RTYPE (INDX) = 'R' OR 'D' OR 'S' OR 'Z'
031710                ADD WS-EDITED-AMT (WS-INDX)
031710                                 TO TOTAL-AMOUNT
031710             ELSE
031710                SUBTRACT WS-EDITED-AMT (WS-INDX)
031710                                 FROM TOTAL-AMOUNT
031710             END-IF
CIDMOD         END-IF
00485      ELSE
00486          MOVE -1                 TO  AMT-LEN (INDX)
00487          MOVE ER-2236            TO  EMI-ERROR
00488          MOVE AL-UNBON           TO  AMT-ATTRB (INDX)
00489          PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00490
021716     IF (PI-COMPANY-ID = 'DCC' or 'VPP')
042303        AND (RTYPE (INDX) = 'P')
042303        CONTINUE
042303     ELSE
CIDMOD        IF GL-ACCT-LEN (INDX) = ZEROES
CIDMOD           MOVE -1               TO GL-ACCT-LEN (INDX)
CIDMOD           MOVE ER-2956          TO EMI-ERROR
CIDMOD           MOVE AL-UABON         TO GL-ACCT-ATTRB (INDX)
CIDMOD           PERFORM 9900-ERROR-FORMAT
CIDMOD                                 THRU 9900-EXIT
CIDMOD           GO TO 1040-CONTINUE-EDIT
042303        END-IF
042303     END-IF
120711     IF GL-ACCT-LEN (INDX) NOT = ZEROS
120711        MOVE GL-ACCT (INDX)      TO CHECK-GL-ACCT
120711     ELSE
120711        MOVE SPACES              TO CHECK-GL-ACCT
120711     END-IF
120711
120711     EVALUATE TRUE
021716        WHEN (PI-COMPANY-ID = 'DCC')
110315           AND (CO-CARRIER = '1' OR '2' or '9')
120711           AND (VALID-DCC-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
021716        WHEN (PI-COMPANY-ID = 'DCC')
120711           AND (CO-CARRIER = '3' OR '4')
120711           AND VALID-CSI-GL-ACCOUNT
120711           GO TO 1040-CONTINUE-EDIT
021716        WHEN (PI-COMPANY-ID = 'DCC')
081414           AND (CO-CARRIER = '5' OR '6' or '7')
120711           AND (VALID-CCC-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
062121        WHEN (PI-COMPANY-ID = 'CID' OR 'AHL')
120711           AND (VALID-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
031912        WHEN (PI-COMPANY-ID = 'VPP')
120711           AND (VALID-VPP-GL-ACCOUNT)
120711           GO TO 1040-CONTINUE-EDIT
062121        WHEN (PI-COMPANY-ID = 'FNL')
062121           AND (VALID-FNL-GL-ACCOUNT)
062121           GO TO 1040-CONTINUE-EDIT
120711     END-EVALUATE
CIDMOD*    EXEC CICS HANDLE CONDITION
CIDMOD*         NOTFND (1040-NO-COFA-MSTR)
CIDMOD*         NOTOPEN (7100-COFA-FILE-NOTOPEN)
CIDMOD*    END-EXEC.
CIDMOD*
CIDMOD*    EXEC CICS READ
CIDMOD*         DATASET (COFA-FILE-ID)
CIDMOD*         SET     (ADDRESS OF CHART-OF-ACCOUNTS)
CIDMOD*         RIDFLD  (COFA-KEY-X)
CIDMOD*    END-EXEC.
CIDMOD*
CIDMOD*    MOVE CHART-OF-ACCOUNTS      TO SV-COFA.
CIDMOD*    GO TO 1040-CONTINUE-EDIT.
           .
CIDMOD 1040-NO-COFA-MSTR.
CIDMOD
CIDMOD     MOVE ER-2960                TO EMI-ERROR.
CIDMOD     MOVE -1                     TO GL-ACCT-LEN (INDX).
CIDMOD     MOVE AL-UABON               TO GL-ACCT-ATTRB (INDX).
CIDMOD     PERFORM 9900-ERROR-FORMAT THRU
CIDMOD             9900-EXIT.
CIDMOD
CIDMOD 1040-CONTINUE-EDIT.
CIDMOD
CIDMOD*    MOVE MSA-ACCT (INDX)        TO WS-ACCT-BREAK.
CIDMOD     IF GL-STATE-LEN (INDX) NOT EQUAL ZEROS
CIDMOD         MOVE GL-STATE (INDX)    TO CHECK-STATE-CODE
CIDMOD         IF NOT VALID-STATE-CODE
CIDMOD             MOVE -1             TO GL-STATE-LEN (INDX)
CIDMOD             MOVE ER-2957        TO EMI-ERROR
CIDMOD             MOVE AL-UABON       TO GL-STATE-ATTRB (INDX)
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                     9900-EXIT
CIDMOD         ELSE
CIDMOD             MOVE AL-UANON       TO GL-STATE-ATTRB (INDX)
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
CIDMOD 1040-CSO-SKIP.
CIDMOD
CIDMOD     IF GL-CANC (INDX) = LOW-VALUES
CIDMOD         MOVE SPACES             TO GL-CANC (INDX)
CIDMOD     END-IF
CIDMOD     IF GL-CANC-LEN (INDX) NOT EQUAL ZEROS
CIDMOD         MOVE GL-CANC (INDX)     TO CHECK-CANC-TYPE
CIDMOD         IF NOT VALID-CANC-TYPE
CIDMOD             MOVE -1             TO GL-CANC-LEN (INDX)
CIDMOD             MOVE ER-2958        TO EMI-ERROR
CIDMOD             MOVE AL-UABON       TO GL-CANC-ATTRB (INDX)
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                     9900-EXIT
CIDMOD         ELSE
CIDMOD             MOVE AL-UANON       TO GL-CANC-ATTRB (INDX)
CIDMOD         END-IF
CIDMOD     END-IF.
CIDMOD
CIDMOD     IF (CARRIER (INDX) EQUAL '6' AND
CIDMOD         GL-ACCT (INDX) EQUAL '1082202')
CIDMOD         CONTINUE
CIDMOD     ELSE
CIDMOD         GO TO 1040-INCREMENT-INDX
CIDMOD     END-IF.
CIDMOD
CIDMOD     IF GL-CHECK-NO (INDX) NOT NUMERIC
CIDMOD         MOVE -1                 TO GL-COMM-LEN (INDX)
CIDMOD         MOVE ER-2961            TO EMI-ERROR
CIDMOD         MOVE AL-UABON           TO GL-COMM-ATTRB (INDX)
CIDMOD         PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                 9900-EXIT
CIDMOD     END-IF.
CIDMOD
00491  1040-INCREMENT-INDX.
00492      SET INDX  UP  BY  1.
060205     ADD +1                      TO C1
00493
CIDMOD     IF INDX  IS NOT GREATER THAN  +15
00495          GO TO 1010-EDIT-LOOP.
00496
00497      IF EMI-ERROR = ZEROS
00498          GO TO 2000-UPDATE-THE-FILE
00499      ELSE
00500          GO TO 8200-SEND-DATAONLY.
00501  EJECT
00502  2000-UPDATE-THE-FILE.
CIDMOD
CIDMOD     IF EIBAID = DFHPF1
CIDMOD        CONTINUE
CIDMOD     ELSE
CIDMOD        MOVE 'PRESS PF1 TO UPDATE FILE' TO EMI-MESSAGE-AREA (1)
CIDMOD        MOVE SPACES TO EMI-MESSAGE-AREA (2)
CIDMOD        MOVE -1 TO  CAR1L
031710        MOVE TOTAL-AMOUNT        TO GROSS-AMTO
CIDMOD        GO TO 8200-SEND-DATAONLY.
CIDMOD
00503      SET INDX                    TO  1.
060205     MOVE +1                     TO C1
00504
           .
00505  2100-UPDATE-LOOP.
00506      IF INDX  IS GREATER THAN  +15
00507          GO TO 2200-UPDATE-COMPLETE.
00508
00509      IF CARR-LEN (INDX) = ZEROS
00510          SET INDX  UP  BY  1
060205         ADD +1        TO C1
00511          GO TO 2100-UPDATE-LOOP.
00512
00513      
      * EXEC CICS GETMAIN
00514 *        SET      (ADDRESS OF PENDING-PAY-ADJ)
00515 *        LENGTH   (ERPYAJ-RECORD-LENGTH)
00516 *        INITIMG  (GETMAIN-SPACE)
00517 *        END-EXEC.
      *    MOVE ',"IL                  $   #00003571' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033353731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ERPYAJ-RECORD-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00518
00519      MOVE 'PY'                   TO  PY-RECORD-ID.
00520      MOVE PI-COMPANY-CD          TO  PY-COMPANY-CD.
00521      MOVE CARRIER     (INDX)     TO  PY-CARRIER.
00522      MOVE GROUPING    (INDX)     TO  PY-GROUPING.
00523      MOVE FIN-RESP    (INDX)     TO  PY-FIN-RESP.
00524      MOVE ACCT        (INDX)     TO  PY-ACCOUNT.
00525      MOVE RTYPE       (INDX)     TO  PY-RECORD-TYPE.
00526
00527      MOVE WORK-SEQ-NO            TO  PY-FILE-SEQ-NO.
00528
00529      ADD +1                      TO  WORK-SEQ-NO.
00530
120204     IF GL-ACCT-LEN (INDX) NOT = ZEROS
120204        MOVE GL-ACCT (INDX)      TO  PY-GL-ACCOUNT
120204     END-IF
120204     IF GL-STATE-LEN (INDX) NOT = ZEROS
120204        MOVE GL-STATE (INDX)     TO  PY-GL-STATE
120204     END-IF
120204
120204     IF GL-CANC-LEN (INDX) NOT = ZEROS
120204        MOVE GL-CANC (INDX)      TO  PY-GL-CANC-SW
120204     END-IF
120204
120204     IF GL-COMM-LEN (INDX) NOT = ZEROS
120204        MOVE GL-COMM (INDX)      TO  PY-GL-COMMENT
120204     END-IF
060205     MOVE WS-ERCOMP-TYPE (C1)    TO  PY-ERCOMP-TYPE
00533      SET WS-INDX                 TO  INDX.
00534      MOVE WS-EDITED-AMT(WS-INDX) TO  PY-ENTRY-AMT.
00535      MOVE PI-PROCESSOR-ID        TO  PY-LAST-MAINT-BY.
00536      MOVE EIBTIME                TO  PY-LAST-MAINT-HHMMSS.
00537      MOVE WS-CURRENT-BIN-DT      TO  PY-LAST-MAINT-DT
00538                                      PY-INPUT-DT.
00539      MOVE ZEROS                  TO  PY-CHECK-QUE-CONTROL
00540                                      PY-CHECK-QUE-SEQUENCE.
00541      MOVE LOW-VALUES             TO  PY-CREDIT-ACCEPT-DT
00542                                      PY-BILLED-DATE
00543                                      PY-AR-DATE
00544                                      PY-REPORTED-DT
00545                                      PY-CHECK-WRITTEN-DT.
00546      MOVE PI-CR-MONTH-END-DT     TO  PY-CREDIT-SELECT-DT.
00547      MOVE 'A'                    TO  JP-RECORD-TYPE.
00548      MOVE PENDING-PAY-ADJ        TO  JP-RECORD-AREA.
00549
00550      
      * EXEC CICS WRITE
00551 *        DATASET  (PYAJ-FILE-ID)
00552 *        FROM     (PENDING-PAY-ADJ)
00553 *        RIDFLD   (PY-CONTROL-PRIMARY)
00554 *        END-EXEC.
           MOVE LENGTH OF
            PENDING-PAY-ADJ
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003621' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PYAJ-FILE-ID, 
                 PENDING-PAY-ADJ, 
                 DFHEIV11, 
                 PY-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00555
00556      PERFORM 8400-LOG-JOURNAL-RECORD.
00557
00558      MOVE LOW-VALUES             TO  DATA-AREA (INDX).
00559
00560      GO TO 2100-UPDATE-LOOP.
00561
00562  2200-UPDATE-COMPLETE.
00563      MOVE LOW-VALUES             TO  EL633BI.
00564      MOVE ER-0000                TO  EMI-ERROR.
00565      MOVE -1                     TO  CAR1L.
00566
00567      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
00568
00569      GO TO 8100-SEND-INITIAL-MAP.
00570  EJECT
00571  7000-PYAJ-FILE-NOTOPEN.
00572      MOVE -1                     TO  PFENTERL.
00573      MOVE ER-2232                TO  EMI-ERROR.
00574
00575      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00576
00577      GO TO 8200-SEND-DATAONLY.
00578
00579  7100-COMP-FILE-NOTOPEN.
00580      MOVE -1                     TO  PFENTERL.
00581      MOVE ER-2233                TO  EMI-ERROR.
00582
00583      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
00584
00585      GO TO 8200-SEND-DATAONLY.
CIDMOD/
CIDMOD*
CIDMOD*7100-COFA-FILE-NOTOPEN.
CIDMOD*    MOVE -1                     TO PFENTERL.
CIDMOD*    MOVE ER-2959                TO EMI-ERROR.
CIDMOD*    PERFORM 9900-ERROR-FORMAT THRU
CIDMOD*            9900-EXIT.
CIDMOD*    GO TO 8200-SEND-DATAONLY.
CIDMOD*
00587  8100-SEND-INITIAL-MAP.
00588      MOVE WS-CURRENT-DT          TO  DATEO.
00589      MOVE EIBTIME                TO  TIME-IN.
00590      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00591      MOVE -1                     TO  CAR1L.
00592      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
00593      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
00594
00595      
      * EXEC CICS SEND
00596 *        MAP     (MAP-NAME)
00597 *        MAPSET  (MAPSET-NAME)
00598 *        FROM    (EL633BO)
00599 *        ERASE
00600 *        CURSOR
00601 *        END-EXEC.
           MOVE LENGTH OF
            EL633BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00003676' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633BO, 
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
           
00602
00603      GO TO 9100-RETURN-TRAN.
00604  EJECT
00605  8200-SEND-DATAONLY.
00606      MOVE WS-CURRENT-DT          TO  DATEO.
00607      MOVE EIBTIME                TO  TIME-IN.
00608      MOVE TIME-OUT               TO  TIMEO.
101101     MOVE PI-COMPANY-ID          TO  CMPNYIDO.
101101     MOVE PI-PROCESSOR-ID        TO  USERIDO.
00609      MOVE EMI-MESSAGE-AREA (1)   TO  ERRMSG1O.
00610      MOVE EMI-MESSAGE-AREA (2)   TO  ERRMSG2O.
00611
00612      
      * EXEC CICS SEND
00613 *        MAP     (MAP-NAME)
00614 *        MAPSET  (MAPSET-NAME)
00615 *        FROM    (EL633BO)
00616 *        DATAONLY
00617 *        CURSOR
00618 *        END-EXEC.
           MOVE LENGTH OF
            EL633BO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00003695' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL633BO, 
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
           
00619
00620      GO TO 9100-RETURN-TRAN.
00621
00622  8300-SEND-TEXT.
00623      
      * EXEC CICS SEND TEXT
00624 *        FROM    (LOGOFF-TEXT)
00625 *        LENGTH  (LOGOFF-LENGTH)
00626 *        ERASE
00627 *        FREEKB
00628 *        END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00003706' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373036' TO DFHEIV0(25:11)
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
           
00629
00630      
      * EXEC CICS RETURN
00631 *        END-EXEC.
      *    MOVE '.(                    ''   #00003713' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00632  EJECT
00633  8400-LOG-JOURNAL-RECORD.
00634 *    MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
00635 *    MOVE THIS-PGM               TO  JP-PROGRAM-ID.
00636 *    MOVE PYAJ-FILE-ID           TO  JP-FILE-ID.
00637 *    MOVE ZERO                   TO  JP-GENERIC-KEY-LENGTH.
00638
00639 *    EXEC CICS JOURNAL
00640 *        JFILEID  (PI-JOURNAL-FILE-ID)
00641 *        JTYPEID  ('EL')
00642 *        FROM     (JOURNAL-RECORD)
00643 *        LENGTH   (223)
00644 *        END-EXEC.
00645
00646  8500-DATE-CONVERT.
00647      
      * EXEC CICS LINK
00648 *        PROGRAM   (LINK-CLDATCV)
00649 *        COMMAREA  (DATE-CONVERSION-DATA)
00650 *        LENGTH    (DC-COMM-LENGTH)
00651 *        END-EXEC.
      *    MOVE '."C                   (   #00003730' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LINK-CLDATCV, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00652
00653  8500-EXIT.
00654      EXIT.
00655
00656  8600-DEEDIT.
00657      
      * EXEC CICS BIF DEEDIT
00658 *        FIELD   (DEEDIT-FIELD)
00659 *        LENGTH  (11)
00660 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '@"L                   #   #00003740' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00661
00662  8600-EXIT.
00663      EXIT.
00664  EJECT
00665  8800-UNAUTHORIZED-ACCESS.
00666      MOVE UNACCESS-MSG           TO  LOGOFF-MSG.
00667
00668      GO TO 8300-SEND-TEXT.
00669
00670  8810-PF23.
00671      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
00672      MOVE XCTL-005               TO  PGM-NAME.
00673
00674      GO TO 9300-XCTL.
00675
00676  9000-RETURN-CICS.
00677      
      * EXEC CICS RETURN
00678 *        END-EXEC.
      *    MOVE '.(                    ''   #00003760' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00679
00680  9100-RETURN-TRAN.
00681      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
00682      MOVE SCREEN-NUMBER          TO  PI-CURRENT-SCREEN-NO.
00683
00684      
      * EXEC CICS RETURN
00685 *        TRANSID   (TRANS-ID)
00686 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00687 *        LENGTH    (PI-COMM-LENGTH)
00688 *        END-EXEC.
      *    MOVE '.(CT                  ''   #00003767' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00689
00690  9200-RETURN-MAIN-MENU.
00691      MOVE XCTL-626               TO  PGM-NAME.
00692
00693      GO TO 9300-XCTL.
00694
00695  9300-XCTL.
00696      
      * EXEC CICS XCTL
00697 *        PROGRAM   (PGM-NAME)
00698 *        COMMAREA  (PROGRAM-INTERFACE-BLOCK)
00699 *        LENGTH    (PI-COMM-LENGTH)
00700 *        END-EXEC.
      *    MOVE '.$C                   %   #00003779' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00701
00702  9400-CLEAR.
00703      MOVE PI-RETURN-TO-PROGRAM   TO  PGM-NAME.
00704
00705      GO TO 9300-XCTL.
00706
00707  9500-PF12.
00708      MOVE XCTL-010               TO  PGM-NAME.
00709
00710      GO TO 9300-XCTL.
00711
00712  9600-PGMID-ERROR.
00713      
      * EXEC CICS HANDLE CONDITION
00714 *        PGMIDERR  (8300-SEND-TEXT)
00715 *        END-EXEC.
      *    MOVE '"$L                   ! $ #00003796' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00716
00717      MOVE PGM-NAME               TO  PI-CALLING-PROGRAM.
00718      MOVE ' '                    TO  PI-ENTRY-CD-1.
00719      MOVE XCTL-005               TO  PGM-NAME.
00720      MOVE PGM-NAME               TO  LOGOFF-PGM.
00721      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00722
00723      GO TO 9300-XCTL.
00724
00725  9900-ERROR-FORMAT.
00726      IF NOT EMI-ERRORS-COMPLETE
00727          MOVE LINK-001           TO  PGM-NAME
00728          
      * EXEC CICS LINK
00729 *            PROGRAM   (PGM-NAME)
00730 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
00731 *            LENGTH    (EMI-COMM-LENGTH)
00732 *            END-EXEC.
      *    MOVE '."C                   (   #00003811' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00733
00734  9900-EXIT.
00735      EXIT.
00736
00737  9990-ABEND.
00738      MOVE LINK-004               TO  PGM-NAME.
00739      MOVE DFHEIBLK               TO  EMI-LINE1.
00740
00741      
      * EXEC CICS LINK
00742 *        PROGRAM   (PGM-NAME)
00743 *        COMMAREA  (EMI-LINE1)
00744 *        LENGTH    (72)
00745 *        END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00003824' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00746
00747      MOVE -1                     TO  PFENTERL.
00748
00749      GO TO 8200-SEND-DATAONLY.
00750
00751      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6331' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
00752
00753  9995-SECURITY-VIOLATION.
00754 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00003854' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383534' TO DFHEIV0(25:11)
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
00755
00756  9995-EXIT.
00757      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6331' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 1020-NO-COMP-MSTR,
                     7100-COMP-FILE-NOTOPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL6331' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
