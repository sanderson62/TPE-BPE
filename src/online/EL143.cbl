00001  ID DIVISION.
00002
00003  PROGRAM-ID.                 EL143.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/13/96 09:51:40.
00007 *                            VMOD=2.018.
00008 *
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00012 *DATE-COMPILED.
00013
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
00025 *REMARKS.    TRANSACTION - EX30 - PAYMENT APPROVAL.
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
031808* 031808    2006032200004  AJRA  ADD APPROVAL LEVEL 4
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
091813* 091813    2013082900001  AJRA  ADD APPROVAL LEVEL 5
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00026
00027      EJECT
00028  ENVIRONMENT DIVISION.
00029  DATA DIVISION.
00030
00031  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00032  77  FILLER  PIC X(32)  VALUE '********************************'.
00033  77  FILLER  PIC X(32)  VALUE '*    EL143  WORKING STORAGE    *'.
00034  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.018 *********'.
061013 77  S1                          PIC S999 COMP-3 VALUE +0.
061013 77  S2                          PIC S999 COMP-3 VALUE +0.
       77  ws-max-bens                 pic s999 comp-3 value +0.
       77  ws-prev-days-paid           pic s9(5) comp-3 value +0.
       77  ws-prev-amt-paid            pic s9(9)v99 comp-3 value +0.
       77  ws-tot-days-paid            pic s9(5) comp-3 value +0.
       77  ws-tot-amt-paid             pic s9(9)v99 comp-3 value +0.
       77  ws-pd-bens                  pic s9(5) comp-3 value +0.
       77  ws-at-amount-paid           pic s9(7)v99 comp-4 value +0.
       77  ws-at-days-in-period        pic s9(5) comp-3 value +0.
       77  ws-at-payment-type          pic x          value ' '.
       77  ws-cm-ah-orig-term          pic s999 comp-3 value +0.
       77  ws-cm-ah-benefit-amt        pic s9(7)v99 comp-3 value +0.
00035
00036      EJECT
00037  01  WS-DATE-AREA.
00038      05  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00039      05  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00040
00041  01  WS-QID.
00042      05  WS-QID-TERM                 PIC X(4)    VALUE SPACES.
00043      05  FILLER                      PIC X(4)    VALUE '143A'.
00044
00045  01  STANDARD-AREAS.
061013     12  WS-RESPONSE         PIC S9(8)   COMP.
061013         88  RESP-NORMAL              VALUE +00.
061013         88  RESP-ERROR               VALUE +01.
061013         88  RESP-NOTFND              VALUE +13.
061013         88  RESP-DUPREC              VALUE +14.
061013         88  RESP-ENDFILE             VALUE +20.
00046      12  WS-BROWSE-SW                PIC X       VALUE SPACES.
00047      12  MAP-NAME                    PIC X(8)    VALUE 'EL143A  '.
00048      12  MAPSET-NAME                 PIC X(8)    VALUE 'EL143S  '.
00049      12  SCREEN-NUMBER               PIC X(4)    VALUE '143A'.
00050      12  TRANS-ID                    PIC X(4)    VALUE 'EX30'.
00051      12  THIS-PGM                    PIC X(8)    VALUE 'EL143'.
00052      12  PGM-NAME                    PIC X(8).
00053      12  TIME-IN                     PIC S9(7).
00054      12  TIME-OUT-R  REDEFINES TIME-IN.
00055          16  FILLER                  PIC X.
00056          16  TIME-OUT                PIC 99V99.
00057          16  FILLER                  PIC XX.
00058      12  XCTL-005                    PIC X(8)    VALUE 'EL005'.
00059      12  XCTL-010                    PIC X(8)    VALUE 'EL010'.
00060      12  XCTL-126                    PIC X(8)    VALUE 'EL126'.
00061      12  XCTL-150                    PIC X(8)    VALUE 'EL150'.
00062      12  LINK-001                    PIC X(8)    VALUE 'EL001'.
00063      12  LINK-004                    PIC X(8)    VALUE 'EL004'.
00064      12  LINK-ELDATCV                PIC X(8)    VALUE 'ELDATCV'.
00065      12  ELCNTL-FILE-ID              PIC X(8)    VALUE 'ELCNTL'.
00066      12  ELACTQ-FILE-ID              PIC X(8)    VALUE 'ELACTQ'.
00067      12  ELMSTR-FILE-ID              PIC X(8)    VALUE 'ELMSTR'.
00068      12  ELTRLR-FILE-ID              PIC X(8)    VALUE 'ELTRLR'.
00069      12  ELCERT-FILE-ID              PIC X(8)    VALUE 'ELCERT'.
00070      12  EMPLCY-FILE-ID              PIC X(8)    VALUE 'MPPLCY'.
00071
00072      12  DEEDIT-FIELD                PIC X(15).
00073      12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD   PIC S9(15).
00074      12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD   PIC S9(13)V99.
00075
00076      12  RETURN-FROM                 PIC X(8).
00077
00078      12  WS-LF-COVERAGE-TYPE         PIC X(01)   VALUE SPACE.
00079      12  WS-BEN-SEARCH-SW            PIC X(01)   VALUE 'N'.
00080          88  BENEFIT-FOUND                       VALUE 'Y'.
00081          88  NO-BENEFIT-FOUND                    VALUE 'N'.
00082      12  WS-ACCESS.
00083          16  FILLER                  PIC X(02)   VALUE SPACES.
00084          16  WS-BEN-CD               PIC X(02)   VALUE SPACES.
00085      12  SUB                         PIC 9(01)   VALUE ZEROS.
00086      12  SUB-1                       PIC S9(04)  VALUE +0  COMP.
00087
00088      12  WS-CV-PMT-CODE              PIC X(01)   VALUE ' '.
CIDMOD     12  WS-BLANK                    PIC X       VALUE ' '.
00089
00090      EJECT
00091  01   ERROR-MESSAGES.
00092      12  ER-0000                     PIC  X(4)   VALUE '0000'.
00093      12  ER-0004                     PIC  X(4)   VALUE '0004'.
00094      12  ER-0005                     PIC  X(4)   VALUE '0005'.
00095      12  ER-0023                     PIC  X(4)   VALUE '0023'.
00096      12  ER-0029                     PIC  X(4)   VALUE '0029'.
00097      12  ER-0050                     PIC  X(4)   VALUE '0050'.
00098      12  ER-0068                     PIC  X(4)   VALUE '0068'.
00099      12  ER-0070                     PIC  X(4)   VALUE '0070'.
00100      12  ER-0138                     PIC  X(4)   VALUE '0138'.
00101      12  ER-0142                     PIC  X(4)   VALUE '0142'.
00102      12  ER-0282                     PIC  X(4)   VALUE '0282'.
00103      12  ER-0303                     PIC  X(4)   VALUE '0303'.
00104      12  ER-0627                     PIC  X(4)   VALUE '0627'.
00105      12  ER-0628                     PIC  X(4)   VALUE '0628'.
00106      12  ER-0629                     PIC  X(4)   VALUE '0629'.
00107      12  ER-2237                     PIC  X(4)   VALUE '2237'.
00108      12  ER-2238                     PIC  X(4)   VALUE '2238'.
00109      12  ER-2779                     PIC  X(4)   VALUE '2779'.
00110      12  ER-3342                     PIC  X(4)   VALUE '3342'.
00111      12  ER-3343                     PIC  X(4)   VALUE '3343'.
00112      12  ER-7008                     PIC  X(4)   VALUE '7008'.
00113      12  ER-9211                     PIC  X(4)   VALUE '9211'.
00114      12  ER-9819                     PIC  X(4)   VALUE '9819'.
00115
00116      EJECT
00117  01  MISC.
00118      12  WS-HOLD-KEY.
00119          16  WS-HOLD-COMPANY-CD     PIC X.
00120          16  WS-HOLD-CARR           PIC X.
00121          16  WS-HOLD-CLAIM          PIC X(7).
00122          16  WS-HOLD-CERT.
00123              20  WS-HOLD-CERT-PRIME PIC X(10).
00124              20  WS-HOLD-CERT-SFX   PIC X.
00125      12  WS-PAY-TYPE                PIC X.
00126      12  WS-APPROVAL-LEVEL          PIC X.
00127      12  WS-PAYMENT-APPROVAL-SW     PIC X.
00128          88 WS-GRADUATED-APPROVAL          VALUE 'G'.
00129      12  SC-ITEM                    PIC S9(4)    COMP   VALUE +1.
00130      12  WS-AMOUNT-PAID             PIC S9(7)V99 COMP-3 VALUE +0.
CIDMOD
CIDMOD 01  CSO-WORK-FIELDS.
CIDMOD     05  ERROR-ON-OUTPUT-SW             PIC X        VALUE 'N'.
CIDMOD       88  ERROR-ON-OUTPUT                           VALUE 'Y'.
00131
00132  01  ACCESS-KEYS.
00133      12  WS-HOLD-ELTRLR-KEY             PIC X(20).
00134      12  ELCNTL-KEY.
00135          16  ELCNTL-COMPANY-ID          PIC  X(3).
00136          16  ELCNTL-REC-TYPE            PIC  X.
00137          16  ELCNTL-ACCESS              PIC  X(4).
00138          16  ELCNTL-SEQ-NO              PIC  S9(4)   COMP.
00139      12  ELMSTR-KEY.
00140          16  ELMSTR-COMPANY-CD          PIC X.
00141          16  ELMSTR-CARRIER             PIC X.
00142          16  ELMSTR-CLAIM-NO            PIC X(7).
00143          16  ELMSTR-CERT-NO.
00144              20  ELMSTR-CERT-PRIME      PIC X(10).
00145              20  ELMSTR-CERT-SFX        PIC X.
00146      12  ELTRLR-KEY.
00147          16  ELTRLR-COMPANY-CD          PIC X.
00148          16  ELTRLR-CARRIER             PIC X.
00149          16  ELTRLR-CLAIM-NO            PIC X(7).
00150          16  ELTRLR-CERT-NO.
00151              20  ELTRLR-CERT-PRIME      PIC X(10).
00152              20  ELTRLR-CERT-SFX        PIC X.
00153          16  ELTRLR-SEQ-NO              PIC S9(4)   COMP.
00154      12  ELACTQ-KEY.
00155          16  ELACTQ-COMPANY-CD          PIC X.
00156          16  ELACTQ-CARRIER             PIC X.
00157          16  ELACTQ-CLAIM-NO            PIC X(7).
00158          16  ELACTQ-CERT-NO.
00159              20  ELACTQ-CERT-PRIME      PIC X(10).
00160              20  ELACTQ-CERT-SFX        PIC X.
00161      12  ELCERT-KEY.
00162          16  ELCERT-COMPANY-CD          PIC X.
00163          16  ELCERT-CARRIER             PIC X.
00164          16  ELCERT-GROUP               PIC X(6).
00165          16  ELCERT-STATE               PIC X(2).
00166          16  ELCERT-ACCOUNT             PIC X(10).
00167          16  ELCERT-EFF-DATE            PIC X(2).
00168          16  ELCERT-CERT-NO             PIC X(11).
00169      12  EMPLCY-KEY.
00170          16  EMPLCY-COMPANY-CD          PIC X(01).
00171          16  EMPLCY-CARRIER             PIC X(01).
00172          16  EMPLCY-GROUPING            PIC X(06).
00173          16  EMPLCY-STATE               PIC X(02).
00174          16  EMPLCY-PRODUCER            PIC X(10).
00175          16  EMPLCY-EFF-DATE            PIC X(02).
00176          16  EMPLCY-REFERENCE-NO        PIC X(20).
00177
061013 01  ELCRTT-KEY.
061013     05  CTRLR-COMP-CD       PIC X.
061013     05  CTRLR-CARRIER       PIC X.
061013     05  CTRLR-GROUPING      PIC X(6).
061013     05  CTRLR-STATE         PIC X(2).
061013     05  CTRLR-ACCOUNT       PIC X(10).
061013     05  CTRLR-EFF-DT        PIC XX.
061013     05  CTRLR-CERT-NO       PIC X(11).
061013     05  CTRLR-REC-TYPE      PIC X.
00178      EJECT
00179 *    COPY ELCSCTM.
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
00180      EJECT
00181 *    COPY ELCSCRTY.
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
00182      EJECT
00183 *    COPY ELCDATE.
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
00184      EJECT
00185 *    COPY ELCLOGOF.
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
00186      EJECT
00187 *    COPY ELCATTR.
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
00188      EJECT
00189 *    COPY ELCEMIB.
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
00190      EJECT
00191 *    COPY MPCPOLUP.
00001 ******************************************************************
00002 *                                                                *
00003 *                           MPCPOLUP                             *
00004 *                            VMOD=1.031                          *
00005 *                                                                *
00006 *   THIS COPY BOOK IS USED TO PASS DATA TO THE POLICY UPDATE     *
00007 *   PROGRAM (EMPLCY).  WHEN USING COBOL-2 IT IS NECESSARY TO     *
00008 *   INITIALIZE THE ENTIRE COPYBOOK TO LOW-VALUES, EXCEPT FOR THE *
00009 *   LENGTH FIELD.  BY MOVING LOW-VALUES INTO THE GROUP FIELD     *
00010 *   WS-POLICY-UPDATE-WORKING-GRPS, THIS REQUIREMENT CAN BE       *
00011 *   SATISFIED.                                                   *
00012 *   THE ACTUAL DATA BEING USED TO UPDATE THE POLICY AREA IS      *
00013 *   FOUND IN THE WS-MPPLCY-AREA.  DATA SHOULD ONLY MOVED INTO    *
00014 *   THESE FIELDS WHEN THAT DATA DIFFERS FROM THE VALUES FOUND ON *
00015 *   THE POLICY RECORD, OTHERWISE THEY SHOULD CONTAIN LOW-VALUES. *
00016 *   EACH UPDATED FIELD WILL CREATE A POLICY HISTORY RECORD       *
00017 *   CONTAINING THE BEFORE CHANGE IMAGE.                          *
00018 *                                                                *
00019 *   THE CALLING PROGRAM MUST ALSO PROVIDE THE FOLLOWING DATA:    *
00020 *      WS-LAST-CHANGE-PROCESSOR                                  *
00021 *      WS-CONTROL-PRIMARY                                        *
00022 *                                                                *
00023 *   PROGRAMS THAT ARE DOING OTHER UPDATE I/O FUNCTIONS WILL USE  *
00024 *   'WS-EMPLCY-FUNCTION'. (ALL BATCH PGM'S MUST USE THIS).       *
00025 *   THE PROGRAMS THAT USE THE FUNCTION FIELD CAN USE ONE OF      *
00026 *   THE FOLLOWING CODES:                                         *
00027 *                                                                *
00028 *      OO - OPEN FILES (I-O MODE)                                *
00029 *      OI - OPEN FILES (INPUT MODE)                              *
00030 *      CC - CLOSE FILES                                          *
00031 *      SS - START                                                *
00032 *      RN - READ NEXT                                            *
00033 *      RW - REWRITE                                              *
00034 *      WW - WRITE                                                *
00035 *      DD - DELETE                                               *
00036 *      AC - AGENT COMMISSION ACCOUNTING                          *
00037 *      ED - EFFECTIVE DATE CHANGE                                *
00038 *      KC - KEY CHANGE                                           *
00039 *      NC - NAME CHANGE                                          *
00040 *                                                                *
00041 *   IF THE FUNCTION FIELD WAS USED, CHECK                        *
00042 *   'WS-EMPLCY-RETURN-CODE' FOR LOW VALUES TO ASSURE A           *
00043 *   SUCCESSFUL RETURN.                                           *
00044 *                                                                *
00045 *   **** NOTE ****
00046 *   MAKE SURE PROGRAMS CLEAR THE 'WS-MPPLCY-AREA' IF YOUR        *
00047 *   PROGRAM CALLS EMPLCY TWICE, BY LEAVING DATA IN THE 'WS'      *
00048 *   FIELDS EMPLCY WILL UPDATE AND CREATE HISTORY TWICE FOR       *
00049 *   THE LEFT OVER FIELDS.  WHEN CHANGING FUNCTIONS, ETC., MAKE   *
00050 *   SURE TO CLEAR OR CHANGE THE REQUIRED CONTROL FIELDS.         *
00051 *                                                                *
00052 ******************************************************************
00053
00054 ******************************************************************
00055 *                    PRIMARY CONTROL                             *
00056 ******************************************************************
00057  01  WS-POLICY-MASTER-UPDATE-AREA.
00058 ******************************************************************
00059
00060   12 WS-PM-COMM-LNGTH            PIC S9(4) COMP VALUE +1500.
00061   12 WS-POLICY-UPDATE-WORKING-GRPS.
00062   16 WS-MONTH-END-DT             PIC XX.
00063
00064   16 WS-EMPLCY-FUNCTION          PIC XX.
00065      88  WS-OPEN                   VALUE 'OO'.
00066      88  WS-OPEN-INPUT             VALUE 'OI'.
00067      88  WS-CLOSE                  VALUE 'CC'.
00068      88  WS-START                  VALUE 'SS'.
00069      88  WS-READ                   VALUE 'RR'.
00070      88  WS-READNEXT               VALUE 'RN'.
00071      88  WS-REWRITE                VALUE 'RW' SPACES LOW-VALUES.
00072      88  WS-WRITE                  VALUE 'WW'.
00073      88  WS-DELETE                 VALUE 'DD'.
00074      88  WS-AGENT-COMM-ACTG        VALUE 'AG'.
00075      88  WS-EFFDT-CHG              VALUE 'ED'.
00076      88  WS-KEY-CHG                VALUE 'KC'.
00077      88  WS-NAME-CHANGE            VALUE 'NC'.
00078
00079   16 WS-CALCULATION-AREA.
00080      20  WS-C-INS-MONTH-PREMIUM  PIC S9(5)V999999  COMP-3.
00081
00082   16 WS-ACCOUNTING-ACTIVITY      PIC X(3).
00083      88  PREMIUM-ACCOUNTING
00084                            VALUES ARE '101' '102' '103' '104'
00085                                       '105' '106' '107' '108'
00086                                       '201' '202' '203' '204'
00087                                       '205' '206' '207' '208'
00088                                       '301' '302' '303'
00089                                       '401' '402' '403' '404'
00090                                       '601' '602' '603'.
00091      88  COMMISSION-ACCOUNTING
00092                                   VALUES ARE '501' '502' '503'.
00093 * CASH WITH APP (CWA) ACTIVITY
00094   16 WS-ACTG-CWA-ACTIVITY        PIC X(3).
00095      88  CREATE-PM-WITH-CWA                        VALUE '101'.
00096      88  DELETE-PM-WITH-CWA                        VALUE '102'.
00097      88  CHANGE-PM-CWA                             VALUE '103'.
00098      88  ISSUE-PM-WITH-CWA                         VALUE '104'.
00099      88  CANCEL-PM-WITH-CWA                        VALUE '105'.
00100      88  REINSTATE-PM-WITH-CWA                     VALUE '106'.
00101      88  DECLINE-PM-WITH-CWA                       VALUE '107'.
00102      88  UNDECLINE-PM-WITH-CWA                     VALUE '108'.
00103      88  CWA-ACTIVITY                   VALUES ARE '101' '102'
00104                                                    '103' '104'
00105                                                    '105' '106'
00106                                                    '107' '108'.
00107 * POLICY FEE ACTIVITY
00108   16 WS-ACTG-PLCY-FEE-ACTIVITY   PIC X(3).
00109      88  CREATE-PM-WITH-PLCY-FEE                   VALUE '201'.
00110      88  DELETE-PM-WITH-PLCY-FEE                   VALUE '202'.
00111      88  CHANGE-PM-PLCY-FEE                        VALUE '203'.
00112      88  ISSUE-PM-WITH-PLCY-FEE                    VALUE '204'.
00113      88  CANCEL-PM-WITH-PLCY-FEE                   VALUE '205'.
00114      88  REINSTATE-PM-WITH-PLCY-FEE                VALUE '206'.
00115      88  DECLINE-PM-WITH-PLCY-FEE                  VALUE '207'.
00116      88  UNDECLINE-PM-WITH-PLCY-FEE                VALUE '208'.
00117      88  POLICY-FEE-ACITIVY             VALUES ARE '201' '202'
00118                                                    '203' '204'
00119                                                    '205' '206'
00120                                                    '207' '208'.
00121 * PAYMENT ACTIVITY
00122   16 WS-ACTG-PYMT-ACTIVITY       PIC X(3).
00123      88  APPLY-PAYMENT                             VALUE '301'.
00124      88  REVERSE-PAYMENT                           VALUE '302'.
00125      88  GROUP-PAYMENT-WITH-OVER-SHORT             VALUE '303'.
00126      88  PAYMENT-ACITIVY                VALUES ARE '301' '302'
00127                                                          '303'.
00128 * REFUND ACTIVITY
00129   16 WS-ACTG-REFUND-ACTIVITY     PIC X(3).
00130      88  CREATE-REFUND                             VALUE '401'.
00131      88  DELETE-REFUND                             VALUE '402'.
00132      88  CHANGE-REFUND                             VALUE '403'.
00133      88  REINSTATE-REFUND                          VALUE '404'.
00134      88  REFUND-ACTIVITY                VALUES ARE '401' '402'
00135                                                    '403' '404'.
00136
00137 * COMMISSION ACTIVITY
00138   16 WS-ACTG-COMMISSION-ACTIVITY PIC X(3).
00139      88  COMMISSION-ADVANCE                        VALUE '501'.
00140      88  COMMISSION-PAYMENT                        VALUE '502'.
00141      88  COMMISSION-REVERSAL                       VALUE '503'.
00142      88  COMMISSION-ACTIVITY            VALUES ARE '501' '502'
00143                                                          '503'.
00144 * EXPENSE ACTIVITY
00145   16 WS-ACTG-EXPENSE-ACTIVITY    PIC X(3).
00146      88  CREATE-EXPENSE                            VALUE '601'.
00147      88  DELETE-EXPENSE                            VALUE '602'.
00148      88  CHANGE-EXPENSE                            VALUE '603'.
00149      88  EXPENSE-ACTIVITY               VALUES ARE '601' '602'
00150                                                          '603'.
00151
00152   16 WS-AGENT-KEY.
00153      20  WS-AGT-COMPANY-CD                 PIC X.
00154      20  WS-AGT-CARRIER                    PIC X.
00155      20  WS-AGT-GROUPING                   PIC X(6).
00156      20  WS-AGT-FINCL-RESPONSE             PIC X(10).
00157      20  WS-AGT-PROD-AGT                   PIC X(10).
00158      20  WS-AGT-TYPE                       PIC X.
00159   16  WS-COMM-ADJUSTMENT-AMT               PIC S9(7)V99 COMP-3.
00160   16  WS-1STYR-COMM-AMT                    PIC S9(7)V99 COMP-3.
00161   16  WS-RENEW-COMM-AMT                    PIC S9(7)V99 COMP-3.
00162
00163   16  WS-OLD-EFF-DT                        PIC X(2).
00164   16  WS-NEW-EFF-DT                        PIC X(2).
00165
00166   16  WS-NEW-CONTROL-PRIMARY.
00167       20  WS-NEW-COMPANY-CD                PIC X.
00168       20  WS-NEW-CARRIER                   PIC X.
00169       20  WS-NEW-GROUPING.
00170           24  WS-NEW-GROUPING-PREFIX       PIC X(3).
00171           24  WS-NEW-GROUPING-PRIME        PIC X(3).
00172       20  WS-NEW-STATE                     PIC XX.
00173       20  WS-NEW-PRODUCER.
00174           24  WS-NEW-PRODUCER-PREFIX       PIC X(4).
00175           24  WS-NEW-PRODUCER-PRIME        PIC X(6).
00176
00177   16  WS-NEW-SECURITY-ACCESS-CODE          PIC X.
00178
00179   16  WS-CALLING-PGM                       PIC X(8).
00180
00181   16  WS-EXPENSE-DT                        PIC XX.
00182   16  WS-EXPENSE-AMT                       PIC S9(5)V99   COMP-3.
00183   16  WS-OLD-EXPENSE-AMT                   PIC S9(5)V99   COMP-3.
00184
00185   16 WS-POINTER-AREA.
00186      20  WS-POINTER-SW                     PIC  X.
00187          88 WS-KEEP-POINTERS               VALUE 'Y'.
00188      20  WS-ACTY-POINTER                   PIC S9(08) COMP.
00189      20  WS-ACTG-POINTER                   PIC S9(08) COMP.
00190      20  WS-ALPH-POINTER                   PIC S9(08) COMP.
00191      20  WS-DPND-POINTER                   PIC S9(08) COMP.
00192      20  WS-MIB-POINTER                    PIC S9(08) COMP.
00193      20  WS-NOTE-POINTER                   PIC S9(08) COMP.
00194      20  WS-PLCY-POINTER                   PIC S9(08) COMP.
00195      20  WS-PHSTC-POINTER                  PIC S9(08) COMP.
00196      20  WS-PHST-POINTER                   PIC S9(08) COMP.
00197      20  WS-PREM-POINTER                   PIC S9(08) COMP.
00198      20  WS-UHST-POINTER                   PIC S9(08) COMP.
00199   16 WS-RECORDS-PROCESSED                  PIC S9(04) COMP.
00200   16 WS-USE-POLICY-LINKAGE-IND             PIC  X.
00201      88  WS-USE-POLICY-LINKAGE                 VALUE 'Y'.
00202   16 WS-COMPANY-ID                         PIC  X(03).
00203   16 WS-ACTIVE-RECORDS-IND                 PIC  X.
00204      88  WS-OTHER-ACTIVE-RCRDS-EXISTS          VALUE 'Y'.
00205   16 WS-FATAL-ERROR                        PIC  9(04).
00206   16 WS-STOP-PROCESSING-IND                PIC  X.
00207      88  WS-STOP-PROCESSING                    VALUE 'Y'.
00208   16 FILLER                                PIC X(123).
00209
00210   16 WS-MPPLCY-AREA.
00211      20  WS-EMPLCY-RETURN-CODE             PIC XX.
00212      20  WS-JOURNAL-FILE-ID                PIC S9(4) COMP.
00213
00214      20  WS-CONTROL-PRIMARY.
00215          24  WS-COMPANY-CD                 PIC X.
00216          24  WS-CARRIER                    PIC X.
00217          24  WS-GROUPING.
00218              28  WS-GROUPING-PREFIX        PIC X(3).
00219              28  WS-GROUPING-PRIME         PIC X(3).
00220          24  WS-STATE                      PIC XX.
00221          24  WS-PRODUCER.
00222              28  WS-PRODUCER-PREFIX        PIC X(4).
00223              28  WS-PRODUCER-PRIME         PIC X(6).
00224          24  WS-POLICY-EFF-DT              PIC XX.
00225          24  WS-REFERENCE-NUMBER.
00226              28  WS-REFNO-PRIME            PIC X(18).
00227              28  WS-REFNO-SFX              PIC XX.
00228
00229 ******************************************************************
00230 *              CONTROL BY SOC. SEC. NUMBER                       *
00231 ******************************************************************
00232      20  WS-CONTROL-BY-SSN.
00233          24  WS-COMPANY-CD-A3              PIC X.
00234          24  WS-SOC-SEC-NO.
00235              28  WS-SSN-STATE              PIC XX.
00236              28  WS-SSN-PRODUCER           PIC X(6).
00237              28  WS-SSN-LN3.
00238                  32  WS-INSURED-INITIALS-A3.
00239                      36 WS-INSURED-INITIAL1-A3   PIC X.
00240                      36 WS-INSURED-INITIAL2-A3   PIC X.
00241                  32 WS-PART-LAST-NAME-A3         PIC X.
00242
00243 ******************************************************************
00244 *              CONTROL BY REFERENCE NUMBER                       *
00245 ******************************************************************
00246      20  WS-CONTROL-BY-POLICY-NO.
00247          24  WS-COMPANY-CD-A4              PIC X.
00248          24  WS-POLICY-NO-A4.
00249              28  WS-POLICY-PRIME-A4        PIC X(18).
00250              28  WS-POLICY-SFX-A4          PIC XX.
00251
00252 ******************************************************************
00253 *                 FILE SYNCHRONIZATION DATA                      *
00254 ******************************************************************
00255      20  WS-FILE-SYNCH-DATA.
00256          24  WS-LAST-CHANGE-DT             PIC XX.
00257          24  WS-LAST-CHANGE-TIME           PIC S9(7).
00258          24  WS-LAST-CHANGE-PROCESSOR      PIC X(4).
00259
00260 ******************************************************************
00261 *                    INSUREDS PROFILE DATA                       *
00262 ******************************************************************
00263      20  WS-INSURED-PROFILE-DATA.
00264          24  WS-INSURED-NAME.
00265              28  WS-INSURED-LAST-NAME      PIC X(15).
00266              28  WS-INSURED-FIRST-NAME.
00267                  32  WS-INSURED-1ST-INIT   PIC X.
00268                  32  FILLER                PIC X(9).
00269              28  WS-INSURED-MIDDLE-INIT    PIC X.
00270          24  WS-INSURED-ADDRESS.
00271              28  WS-ADDRESS-LINE-1         PIC X(30).
00272              28  WS-ADDRESS-LINE-2         PIC X(30).
00273              28  WS-CITY                   PIC X(25).
00274              28  WS-RESIDENT-STATE         PIC XX.
00275              28  WS-ZIP-CD.
00276                  32  WS-ZIP-FIRST-FIVE     PIC X(5).
00277                  32  WS-ZIP-PLUS-FOUR      PIC X(4).
00278          24  WS-INSURED-PERSONAL.
00279              28  WS-INSURED-OCC-CLASS      PIC X.
00280              28  WS-INSURED-OCC-CD         PIC X(3).
00281              28  WS-INSURED-SEX            PIC X.
00282              28  WS-INSURED-BIRTH-DT       PIC XX.
00283              28  WS-INSURED-ISSUE-AGE      PIC S9(3).
00284              28  WS-INSURED-ISSUE-AGE-X REDEFINES
00285                  WS-INSURED-ISSUE-AGE      PIC  X(3).
00286              28  WS-INSURED-HEIGHT-FT      PIC S9(3).
00287              28  WS-INSURED-HEIGHT-FT-X REDEFINES
00288                  WS-INSURED-HEIGHT-FT      PIC  X(3).
00289              28  WS-INSURED-HEIGHT-IN      PIC S9(3).
00290              28  WS-INSURED-HEIGHT-IN-X REDEFINES
00291                  WS-INSURED-HEIGHT-IN      PIC  X(3).
00292              28  WS-INSURED-WEIGHT         PIC S9(3).
00293              28  WS-INSURED-WEIGHT-X    REDEFINES
00294                  WS-INSURED-WEIGHT         PIC  X(3).
00295              28  WS-INSURED-BIRTH-STATE    PIC XX.
00296              28  WS-INSURED-PHONE-NO       PIC X(13).
00297              28  WS-INSURED-RATED-AGE      PIC S9(3).
00298              28  WS-INSURED-RATED-AGE-X REDEFINES
00299                  WS-INSURED-RATED-AGE      PIC  X(3).
00300          24  WS-INS-LANGUAGE-IND           PIC X.
00301          24  WS-INSURED-TOT-BENEFIT        PIC S9(7)V99.
00302          24  WS-INSURED-AGE-IND            PIC X.
00303      20  FILLER                            PIC X(02).
00304
00305 ******************************************************************
00306 *                JOINT INSUREDS PROFILE DATA                     *
00307 ******************************************************************
00308      20  WS-JOINT-PROFILE-DATA.
00309          24  WS-JOINT-NAME.
00310              28  WS-JOINT-LAST-NAME         PIC X(15).
00311              28  WS-JOINT-FIRST-NAME.
00312                  32  WS-JOINT-1ST-INIT      PIC X.
00313                  32  FILLER                 PIC X(9).
00314              28  WS-JOINT-MIDDLE-INIT       PIC X.
00315          24  WS-JOINT-SOC-SEC-NO           PIC X(11).
00316          24  WS-JOINT-PERSONAL.
00317              28  WS-JOINT-OCC-CLASS        PIC X.
00318              28  WS-JOINT-OCC-CD           PIC X(3).
00319              28  WS-JOINT-SEX              PIC X.
00320              28  WS-JOINT-BIRTH-DT         PIC XX.
00321              28  WS-JOINT-ISSUE-AGE        PIC S9(3).
00322              28  WS-JOINT-ISSUE-AGE-X   REDEFINES
00323                  WS-JOINT-ISSUE-AGE        PIC  X(3).
00324              28  WS-JOINT-HEIGHT-FT        PIC S9(3).
00325              28  WS-JOINT-HEIGHT-FT-X   REDEFINES
00326                  WS-JOINT-HEIGHT-FT        PIC  X(3).
00327              28  WS-JOINT-HEIGHT-IN        PIC S9(3).
00328              28  WS-JOINT-HEIGHT-IN-X   REDEFINES
00329                  WS-JOINT-HEIGHT-IN        PIC  X(3).
00330              28  WS-JOINT-WEIGHT           PIC S9(3).
00331              28  WS-JOINT-WEIGHT-X      REDEFINES
00332                  WS-JOINT-WEIGHT           PIC  X(3).
00333              28  WS-JOINT-BIRTH-STATE      PIC XX.
00334              28  WS-JOINT-RATED-AGE        PIC S9(3).
00335              28  WS-JOINT-RATED-AGE-X   REDEFINES
00336                  WS-JOINT-RATED-AGE        PIC  X(3).
00337          24  WS-JOINT-TOT-BENEFIT          PIC S9(7)V99.
00338          24  WS-JOINT-AGE-IND              PIC X.
00339      20  FILLER                            PIC X(03).
00340
00341 ******************************************************************
00342 *                  INSURANCE COVERAGE DATA                       *
00343 ******************************************************************
00344      20  WS-INS-COVERAGE-DATA.
00345          24  WS-FREE-PERIOD                PIC S9(03) COMP-3.
00346          24  WS-LOAN-NUMBER                PIC X(20).
00347          24  WS-LOAN-TERM                  PIC S9(3).
00348          24  WS-LOAN-APR                   PIC S9V9999.
00349          24  WS-LOAN-DT                    PIC XX.
00350          24  WS-LOAN-PYMT                  PIC S9(5)V99.
00351          24  WS-LOAN-BALC                  PIC S9(7)V99.
00352          24  WS-INS-MONTH-BENEFIT          PIC S9(7)V99.
00353          24  WS-INS-BENEFIT-MONTHS         PIC S9(3).
00354          24  WS-INS-TOTAL-BENEFIT          PIC S9(7)V99.
00355          24  WS-INS-PLAN-TYPE              PIC X.
00356          24  WS-INS-PLAN-CD                PIC XX.
00357          24  WS-INS-PLAN-REVISION          PIC X(3).
00358          24  WS-INS-POLICY-FORM            PIC X(12).
00359          24  WS-INS-MSTR-POLICY            PIC X(12).
00360          24  WS-INS-MSTR-APP               PIC X(12).
00361          24  WS-INS-RATE-CD                PIC X(5).
00362          24  WS-INS-SEX-RATING             PIC X.
00363          24  WS-INS-SUBSTANDARD-PCT        PIC S9V9999.
00364          24  WS-INS-SUBSTANDARD-TYPE       PIC X.
00365          24  WS-INS-TERMINATION-DT         PIC XX.
00366          24  WS-INS-MONTH-PREMIUM          PIC S9(5)V999999.
00367          24  WS-INS-CALC-MO-PREM           PIC S9(5)V999999.
00368          24  WS-REINSURANCE-TABLE          PIC X(3).
00369          24  WS-MORTALITY-CD               PIC X(4).
00370          24  WS-INS-TYPE                   PIC X.
00371          24  WS-LOAN-OFFICER               PIC X(5).
00372          24  WS-POLICY-FEE                 PIC S9(3)V99.
00373          24  WS-DEPENDENT-COUNT            PIC S99.
00374          24  WS-CWA-AMOUNT                 PIC S9(5)V99.
00375          24  WS-LAST-AUTO-RERATE-DT        PIC XX.
00376          24  WS-PREM-FINANCED-SW           PIC X.
00377          24  WS-INS-TERM-LETTER-IND        PIC X.
00378          24  WS-INS-UNDERWRITER-MAX-BEN    PIC S9(7)V99.
00379
00380 ******************************************************************
00381 *                    POLICY BILLING DATA                         *
00382 ******************************************************************
00383      20  WS-BILLING-DATA.
00384          24  WS-BILLING-MODE               PIC X(1).
00385          24  WS-BILLING-SCHEDULE           PIC X(1).
00386          24  WS-BILLING-SW                 PIC X(1).
00387          24  WS-BILLING-TYPE               PIC X(1).
00388          24  WS-PAYMENT-AMT                PIC S9(5)V99.
00389          24  WS-PAYMENT-AMT-X          REDEFINES
00390              WS-PAYMENT-AMT                PIC  X(7).
00391          24  WS-OVER-SHORT-AMT             PIC S9(5)V99.
00392          24  WS-OVER-SHORT-AMT-X       REDEFINES
00393              WS-OVER-SHORT-AMT             PIC  X(7).
00394          24  WS-LAST-BILL-DT               PIC XX.
00395          24  WS-LAST-BILL-AMT              PIC S9(5)V99.
00396          24  WS-LAST-BILL-AMT-X        REDEFINES
00397              WS-LAST-BILL-AMT              PIC  X(7).
00398          24  WS-BILL-TO-DT                 PIC XX.
00399          24  WS-LAST-PYMT-DT               PIC XX.
00400          24  WS-PAID-TO-DT                 PIC XX.
00401          24  WS-PYMT-INVOICE-NUMBER        PIC X(6).
00402          24  WS-MONTHS-PAID                PIC S9(3).
00403          24  WS-MONTHS-PAID-X          REDEFINES
00404              WS-MONTHS-PAID                PIC  X(3).
00405          24  WS-TOTAL-PREM-RECVD           PIC S9(7)V99.
00406          24  WS-TOTAL-PREM-RECVD-X     REDEFINES
00407              WS-TOTAL-PREM-RECVD           PIC  X(9).
00408          24  WS-BANK-TRANSIT-NUMBER.
00409              28  WS-FEDERAL-NUMBER         PIC X(4).
00410              28  WS-BANK-NUMBER            PIC X(4).
00411          24  WS-BANK-ACCOUNT-NUMBER        PIC X(20).
00412          24  WS-BILLING-GROUPING-CODE      PIC X(6).
00413          24  WS-CHARGE-CARD-EXP-DT         PIC X(2).
00414          24  WS-CHARGE-CARD-TYPE           PIC X(2).
00415          24  WS-BILL-INVOICE-NUMBER        PIC X(6).
00416          24  WS-BILL-DAY                   PIC S99.
00417          24  WS-RES-PREM-TAX               PIC S9(3)V999999.
00418      20  FILLER                            PIC X(06).
00419
00420 ******************************************************************
00421 *                     CLAIM PAYMENT DATA                         *
00422 ******************************************************************
00423      20  WS-CLAIM-PAYMENT-DATA.
00424          24  WS-CLAIM-BENEFICIARY-NAME     PIC X(25).
00425          24  WS-CLAIM-INCURRED-DT          PIC XX.
00426          24  WS-CLAIM-PAID-TO-DT           PIC XX.
00427          24  WS-CLAIM-PAYMENT-CNT          PIC S9(3).
00428          24  WS-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99.
00429          24  WS-CLAIM-EXPENSES-ITD         PIC S9(7)V99.
00430          24  WS-CLAIM-PAYMENTS-ITD         PIC S9(7)V99.
00431          24  WS-CLAIM-ACCUMULATOR          PIC S9(7)V99.
00432          24  WS-CLAIM-INTERFACE-SW         PIC X.
00433      20  FILLER                            PIC X(10).
00434
00435 ******************************************************************
00436 *                POLICY STATUS AND DISPOSITION                   *
00437 ******************************************************************
00438      20  WS-STATUS-DISPOSITION-DATA.
00439          24  WS-ISSUE-EOM-DT               PIC XX.
00440          24  WS-REPLACEMENT-SWITCH         PIC X.
00441          24  WS-APPL-SIGN-DT               PIC XX.
00442          24  WS-UNDERWRITER                PIC X(3).
00443          24  WS-ENTRY-PROCESSOR            PIC X(4).
00444          24  WS-ENTRY-STATUS               PIC X.
00445          24  WS-ENTRY-DT                   PIC XX.
00446          24  WS-ENTRY-TIME                 PIC S9(7).
00447          24  WS-EXIT-DT                    PIC XX.
00448          24  WS-CURRENT-STATUS             PIC X.
00449              88  WS-LAPSE                     VALUE '0'.
00450              88  WS-ACTIVE                    VALUE '1'.
00451              88  WS-PENDING-ISSUE             VALUE '2'.
00452              88  WS-DECLINED                  VALUE '3'.
00453              88  WS-PENDING-CANCEL            VALUE '4'.
00454              88  WS-PENDING-ISSUE-ERROR       VALUE '5'.
00455              88  WS-CLAIM-APPLIED             VALUE '6'.
00456              88  WS-CANCEL                    VALUE '7'.
00457              88  WS-PENDING-UNWTR-REVW        VALUE '8'.
00458              88  WS-PENDING-CANCEL-ERROR      VALUE '9'.
00459              88  WS-CANCEL-TRANSFER           VALUE 'C'.
00460              88  WS-CLAIM-SETTLEMENT          VALUE 'F'.
00461              88  WS-TERMINATE                 VALUE 'T'.
00462              88  WS-BILLABLE-STATUS   VALUES ARE '0' '1' '6'.
00463              88  WS-PENDING-STATUS
00464                                 VALUES ARE '2' '4' '5' '8' '9'.
00465              88  WS-PENDING-ISSUE-STATUS
00466                                 VALUES ARE '2' '5' '8'.
00467              88  WS-CANCEL-STATUS
00468                                 VALUES ARE '4' '7' '9' 'C'.
00469          24  WS-CANCEL-CAUSE-CD            PIC X(3).
00470          24  WS-CANCEL-DT                  PIC XX.
00471          24  WS-REFUND-FIELDS.
00472              28  WS-REFUND-AMT             PIC S9(5)V99.
00473              28  WS-CALC-REFUND-AMT        PIC S9(5)V99.
00474          24  WS-DECLINE-CD                 PIC X(3).
00475          24  WS-DECLINE-DT                 PIC XX.
00476          24  WS-LAST-LAPSE-DT              PIC XX.
00477          24  WS-LAST-REINSTATE-DT          PIC XX.
00478          24  WS-SECURITY-ACCESS-CODE       PIC X.
00479          24  WS-PREV-CONTROL-PRIMARY.
00480              28  WS-PREV-COMPANY-CD             PIC X.
00481              28  WS-PREV-CARRIER                PIC X.
00482              28  WS-PREV-GROUPING.
00483                  32  WS-PREV-GROUPING-PREFIX    PIC X(3).
00484                  32  WS-PREV-GROUPING-PRIME     PIC X(3).
00485              28  WS-PREV-STATE                  PIC XX.
00486              28  WS-PREV-PRODUCER.
00487                  32  WS-PREV-PRODUCER-PREFIX    PIC X(4).
00488                  32  WS-PREV-PRODUCER-PRIME     PIC X(6).
00489              28  WS-PREV-POLICY-EFF-DT          PIC XX.
00490              28  WS-PREV-REFERENCE-NUMBER.
00491                  32  WS-PREV-REFNO-PRIME        PIC X(18).
00492                  32  WS-PREV-REFNO-SFX          PIC XX.
00493          24  WS-ACTION-DT                       PIC XX.
00494          24  WS-ACTION-CODE                     PIC X(3).
00495          24  WS-ACTION-DT-2                     PIC XX.
00496          24  WS-ACTION-CODE-2                   PIC X(3).
00497          24  WS-ACTION-DT-3                     PIC XX.
00498          24  WS-ACTION-CODE-3                   PIC X(3).
00499          24  WS-ACTION-DT-4                     PIC XX.
00500          24  WS-ACTION-CODE-4                   PIC X(3).
00501          24  WS-ACTION-DT-5                     PIC XX.
00502          24  WS-ACTION-CODE-5                   PIC X(3).
00503          24  WS-KEY-CHANGE                      PIC X.
00504          24  WS-KEY-CHANGE-DT                   PIC XX.
00505          24  WS-RTI-INDICATOR                   PIC X.
00506          24  WS-REASON-CODE                     PIC X(3).
00507          24  WS-IN-OUT-PROCESSING-IND           PIC X(1).
00508      20  FILLER                            PIC X(07).
00509
00510 ******************************************************************
00511 *                 AGENT AND COMMISSION DATA                      *
00512 ******************************************************************
00513      20  WS-COMMISSION-DATA.
00514          24  WS-REMIT-TO                   PIC S9(3) COMP-3.
00515          24  WS-COMM-CHANGE-SW             PIC X.
00516          24  WS-AGENT-INFO-GRP.
00517              28  WS-AGENT-INFORMATION   OCCURS   5 TIMES
00518                                         INDEXED BY WS-COMM-NDX
00519                                                    WS-COMM-NDX2.
00520                  32  WS-AGENT-NUMBER       PIC X(10).
00521                  32  WS-AGENT-TYPE         PIC X.
00522                  32  WS-COMMISSION-BILLED-PAID
00523                                            PIC X(1).
00524                  32  WS-AGENT-COMP-1ST-YEAR
00525                                            PIC S99V999.
00526                  32  WS-COMP-1ST-YEAR-TYPE PIC X(1).
00527                  32  WS-AGENT-RENEWAL-DATA OCCURS 6 TIMES
00528                                         INDEXED BY WS-RENEW-NDX
00529                                                    WS-RENEW-NDX2.
00530                      36 WS-RENEW-MONTHS      PIC S999 COMP-3.
00531                      36 WS-RENEW-COMMISSION PIC S99V999 COMP-3.
00532                      36 WS-RENEW-TYPE        PIC X(1).
00533                  32  WS-COMP-RECALC-FLAG   PIC X(1).
00534
00535 ******************************************************************
00536 *               ADDITIONAL CLAIM DATA                            *
00537 ******************************************************************
00538      20  WS-ADDL-CLAIM-DATA.
00539          24  WS-CLAIM-ATTACH-CNT           PIC S9(3).
00540          24  WS-CLAIM-LIFE-ITD             PIC S9(7)V99.
00541          24  WS-CLAIM-AH-ITD               PIC S9(7)V99.
00542          24  WS-CLAIM-RIDER-ITD            PIC S9(7)V99.
00543
00544 ******************************************************************
00545 *                 FILLER                                         *
00546 ******************************************************************
00547      20  FILLER                            PIC X(40).
00548
00549 ******************************************************************
00550 *                 FILLER                                         *
00551 ******************************************************************
00552   16 WS-POLICY-MASTER-RECORD
00553        REDEFINES WS-MPPLCY-AREA            PIC X(1200).
00554
00192      EJECT
00193 *    COPY ELCINTF.
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
00194      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.
00195          16  PI-LAST-ELACTQ-KEY      PIC X(20).
00196          16  PI-LAST-TRLR-SEQ-NO     PIC S9(4)   COMP.
00197          16  PI-ELTRLR-UPDATE-BY     PIC X(4).
00198          16  PI-ELTRLR-UPDATE-HHMMSS PIC S9(6)   COMP-3.
00199          16  PI-UNAPPROVED-COUNT     PIC S9      COMP-3.
00200          16  PI-DIAGNOSIS            PIC X(25).
00201          16  PI-FIRST-TIME-SW        PIC X(01).
00202          16  PI-LAST-ELTRLR-KEY      PIC X(22).
00203          16  PI-PAY-TYPE             PIC X(01).
00204          16  FILLER                  PIC X(560).
00205
00206      EJECT
00207 *    COPY ELCAID.
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
00208  01  FILLER    REDEFINES DFHAID.
00209      12  FILLER              PIC X(8).
00210      12  PF-VALUES           PIC X       OCCURS 24 TIMES.
00211
00212      EJECT
00213 *    COPY EL143S.
       01  EL143AI.
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
           05  CARRDL PIC S9(0004) COMP.
           05  CARRDF PIC  X(0001).
           05  FILLER REDEFINES CARRDF.
               10  CARRDA PIC  X(0001).
           05  CARRDI PIC  X(0016).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0006).
      *    -------------------------------
           05  STATUSL PIC S9(0004) COMP.
           05  STATUSF PIC  X(0001).
           05  FILLER REDEFINES STATUSF.
               10  STATUSA PIC  X(0001).
           05  STATUSI PIC  X(0006).
      *    -------------------------------
           05  CLAIML PIC S9(0004) COMP.
           05  CLAIMF PIC  X(0001).
           05  FILLER REDEFINES CLAIMF.
               10  CLAIMA PIC  X(0001).
           05  CLAIMI PIC  X(0007).
      *    -------------------------------
           05  INCDTEL PIC S9(0004) COMP.
           05  INCDTEF PIC  X(0001).
           05  FILLER REDEFINES INCDTEF.
               10  INCDTEA PIC  X(0001).
           05  INCDTEI PIC  X(0008).
      *    -------------------------------
           05  REPDTEL PIC S9(0004) COMP.
           05  REPDTEF PIC  X(0001).
           05  FILLER REDEFINES REPDTEF.
               10  REPDTEA PIC  X(0001).
           05  REPDTEI PIC  X(0008).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  SUFFIXL PIC S9(0004) COMP.
           05  SUFFIXF PIC  X(0001).
           05  FILLER REDEFINES SUFFIXF.
               10  SUFFIXA PIC  X(0001).
           05  SUFFIXI PIC  X(0001).
      *    -------------------------------
           05  PDTHHD1L PIC S9(0004) COMP.
           05  PDTHHD1F PIC  X(0001).
           05  FILLER REDEFINES PDTHHD1F.
               10  PDTHHD1A PIC  X(0001).
           05  PDTHHD1I PIC  X(0010).
      *    -------------------------------
           05  PAYTHRL PIC S9(0004) COMP.
           05  PAYTHRF PIC  X(0001).
           05  FILLER REDEFINES PAYTHRF.
               10  PAYTHRA PIC  X(0001).
           05  PAYTHRI PIC  X(0008).
      *    -------------------------------
           05  LSTPAIDL PIC S9(0004) COMP.
           05  LSTPAIDF PIC  X(0001).
           05  FILLER REDEFINES LSTPAIDF.
               10  LSTPAIDA PIC  X(0001).
           05  LSTPAIDI PIC  X(0008).
      *    -------------------------------
           05  PROCL PIC S9(0004) COMP.
           05  PROCF PIC  X(0001).
           05  FILLER REDEFINES PROCF.
               10  PROCA PIC  X(0001).
           05  PROCI PIC  X(0004).
      *    -------------------------------
           05  NOPMTSL PIC S9(0004) COMP.
           05  NOPMTSF PIC  X(0001).
           05  FILLER REDEFINES NOPMTSF.
               10  NOPMTSA PIC  X(0001).
           05  NOPMTSI PIC  X(0003).
      *    -------------------------------
           05  DAYPAIDL PIC S9(0004) COMP.
           05  DAYPAIDF PIC  X(0001).
           05  FILLER REDEFINES DAYPAIDF.
               10  DAYPAIDA PIC  X(0001).
           05  DAYPAIDI PIC  X(0003).
      *    -------------------------------
           05  LNAMEL PIC S9(0004) COMP.
           05  LNAMEF PIC  X(0001).
           05  FILLER REDEFINES LNAMEF.
               10  LNAMEA PIC  X(0001).
           05  LNAMEI PIC  X(0012).
      *    -------------------------------
           05  DIAGNL PIC S9(0004) COMP.
           05  DIAGNF PIC  X(0001).
           05  FILLER REDEFINES DIAGNF.
               10  DIAGNA PIC  X(0001).
           05  DIAGNI PIC  X(0025).
      *    -------------------------------
           05  UCOUNTL PIC S9(0004) COMP.
           05  UCOUNTF PIC  X(0001).
           05  FILLER REDEFINES UCOUNTF.
               10  UCOUNTA PIC  X(0001).
           05  UCOUNTI PIC  9.
      *    -------------------------------
           05  PMTTYPL PIC S9(0004) COMP.
           05  PMTTYPF PIC  X(0001).
           05  FILLER REDEFINES PMTTYPF.
               10  PMTTYPA PIC  X(0001).
           05  PMTTYPI PIC  X(0010).
      *    -------------------------------
           05  CHECKL PIC S9(0004) COMP.
           05  CHECKF PIC  X(0001).
           05  FILLER REDEFINES CHECKF.
               10  CHECKA PIC  X(0001).
           05  CHECKI PIC  X(0007).
      *    -------------------------------
           05  AMTPDL PIC S9(0004) COMP.
           05  AMTPDF PIC  X(0001).
           05  FILLER REDEFINES AMTPDF.
               10  AMTPDA PIC  X(0001).
           05  AMTPDI PIC  X(0010).
      *    -------------------------------
           05  SELDTEL PIC S9(0004) COMP.
           05  SELDTEF PIC  X(0001).
           05  FILLER REDEFINES SELDTEF.
               10  SELDTEA PIC  X(0001).
           05  SELDTEI PIC  X(0008).
      *    -------------------------------
           05  PDFROML PIC S9(0004) COMP.
           05  PDFROMF PIC  X(0001).
           05  FILLER REDEFINES PDFROMF.
               10  PDFROMA PIC  X(0001).
           05  PDFROMI PIC  X(0008).
      *    -------------------------------
           05  COMMENTL PIC S9(0004) COMP.
           05  COMMENTF PIC  X(0001).
           05  FILLER REDEFINES COMMENTF.
               10  COMMENTA PIC  X(0001).
           05  COMMENTI PIC  X(0025).
      *    -------------------------------
           05  PDTHHD2L PIC S9(0004) COMP.
           05  PDTHHD2F PIC  X(0001).
           05  FILLER REDEFINES PDTHHD2F.
               10  PDTHHD2A PIC  X(0001).
           05  PDTHHD2I PIC  X(0016).
      *    -------------------------------
           05  PDTHRUL PIC S9(0004) COMP.
           05  PDTHRUF PIC  X(0001).
           05  FILLER REDEFINES PDTHRUF.
               10  PDTHRUA PIC  X(0001).
           05  PDTHRUI PIC  X(0008).
      *    -------------------------------
           05  SEQL PIC S9(0004) COMP.
           05  SEQF PIC  X(0001).
           05  FILLER REDEFINES SEQF.
               10  SEQA PIC  X(0001).
           05  SEQI PIC  X(0004).
      *    -------------------------------
           05  AREQL PIC S9(0004) COMP.
           05  AREQF PIC  X(0001).
           05  FILLER REDEFINES AREQF.
               10  AREQA PIC  X(0001).
           05  AREQI PIC  X(0001).
      *    -------------------------------
           05  PDTOL PIC S9(0004) COMP.
           05  PDTOF PIC  X(0001).
           05  FILLER REDEFINES PDTOF.
               10  PDTOA PIC  X(0001).
           05  PDTOI PIC  X(0010).
      *    -------------------------------
           05  PAIDBYL PIC S9(0004) COMP.
           05  PAIDBYF PIC  X(0001).
           05  FILLER REDEFINES PAIDBYF.
               10  PAIDBYA PIC  X(0001).
           05  PAIDBYI PIC  X(0004).
      *    -------------------------------
           05  ALEVL PIC S9(0004) COMP.
           05  ALEVF PIC  X(0001).
           05  FILLER REDEFINES ALEVF.
               10  ALEVA PIC  X(0001).
           05  ALEVI PIC  X(0001).
      *    -------------------------------
           05  RECDTEL PIC S9(0004) COMP.
           05  RECDTEF PIC  X(0001).
           05  FILLER REDEFINES RECDTEF.
               10  RECDTEA PIC  X(0001).
           05  RECDTEI PIC  X(0008).
      *    -------------------------------
           05  FORCEL PIC S9(0004) COMP.
           05  FORCEF PIC  X(0001).
           05  FILLER REDEFINES FORCEF.
               10  FORCEA PIC  X(0001).
           05  FORCEI PIC  X(0001).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0079).
      *    -------------------------------
           05  PFKEYL PIC S9(0004) COMP.
           05  PFKEYF PIC  X(0001).
           05  FILLER REDEFINES PFKEYF.
               10  PFKEYA PIC  X(0001).
           05  PFKEYI PIC  99.
       01  EL143AO REDEFINES EL143AI.
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
           05  CARRDO PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATUSO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLAIMO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INCDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REPDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SUFFIXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHHD1O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAYTHRO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LSTPAIDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOPMTSO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DAYPAIDO PIC  ZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  LNAMEO PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DIAGNO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  UCOUNTO PIC  9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PMTTYPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CHECKO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMTPDO PIC  ZZZZZZZ.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SELDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDFROMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMMENTO PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHHD2O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTHRUO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQO PIC  ZZZ9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AREQO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PDTOO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PAIDBYO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALEVO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RECDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORCEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEYO PIC  X(0002).
      *    -------------------------------
00214
00215      EJECT
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
00217  01  DFHCOMMAREA             PIC X(1024).
00218
00219      EJECT
00220 *    COPY ELCCNTL.
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
00221      EJECT
00222 *    COPY ELCMSTR.
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
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
022122*            88  hospital-claim         value 'H'.
022122*            88  bereavement-claim      value 'B'.
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
00223      EJECT
00224 *    COPY ELCTRLR.
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
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
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
022122             88  PAID-FOR-BRV                   VALUE 'B'.
022122             88  PAID-FOR-HOS                   VALUE 'H'.
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
00225      EJECT
00226 *    COPY ELCACTQ.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCACTQ.                            *
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.004                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = ACTIVITY QUE FILE                         *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 60     RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELACTQ             RKP=2,LEN=20          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCACTQ                          *
00017 ******************************************************************
00018
00019  01  ACTIVITY-QUE.
00020      12  AQ-RECORD-ID                PIC XX.
00021          88  VALID-AQ-ID                VALUE 'AQ'.
00022
00023      12  AQ-CONTROL-PRIMARY.
00024          16  AQ-COMPANY-CD           PIC X.
00025          16  AQ-CARRIER              PIC X.
00026          16  AQ-CLAIM-NO             PIC X(7).
00027          16  AQ-CERT-NO.
00028              20  AQ-CERT-PRIME       PIC X(10).
00029              20  AQ-CERT-SFX         PIC X.
00030
00031      12  AQ-PENDING-ACTIVITY-FLAGS.
00032          88  NO-PENDING-ACTIVITY        VALUE SPACES.
00033          16  AQ-PENDING-PAYMENT-FLAG PIC X.
00034              88  PENDING-PAYMENTS       VALUE '1'.
00035          16  AQ-PENDING-STATUS-FLAG  PIC X.
00036              88  PENDING-FULL-PRINT     VALUE '1'.
00037              88  PENDING-PART-PRINT     VALUE '2'.
00038          16  AQ-PENDING-LETTER-FLAG  PIC X.
00039              88  PENDING-LETTERS        VALUE '1'.
00040          16  AQ-PENDING-CLAIM-RESTORE PIC X.
00041              88  PENDING-RESTORE        VALUE 'C'.
00042              88  PENDING-RESTORE-LETTER VALUE 'L'.
00043
00044      12  FILLER                      PIC X(20).
00045
00046      12  AQ-RESEND-DATE              PIC XX.
00047      12  AQ-FOLLOWUP-DATE            PIC XX.
00048      12  AQ-PAYMENT-COUNTER          PIC S9        COMP-3.
00049      12  AQ-PMT-UNAPPROVED-COUNT     PIC S9        COMP-3.
00050      12  AQ-AUTO-LETTER              PIC X(4).
00051      12  FILLER                      PIC XX.
00052      12  AQ-LAST-UPDATED-BY          PIC S9(4)     COMP.
00053 *****************************************************************
00227      EJECT
00228 *    COPY ELCCERT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCERT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.013                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE MASTER                        *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 450  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCERT                         RKP=2,LEN=33   *
00013 *       ALTERNATE PATH1 = ELCERT2 (BY NAME)       RKP=35,LEN=18  *
00014 *       ALTERNATE PATH2 = ELCERT3 (BY SOC SEC NO) RKP=53,LEN=12  *
00015 *       ALTERNATE PATH3 = ELCERT5 (BY CERT NO.)   RKP=65,LEN=12  *
00016 *       ALTERNATE PATH4 = ELCERT6 (BY MEMBER NO.) RKP=77,LEN=13  *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
040504* 040504  CR2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
061405* 061405  CR2005060300001  PEMA  ADD CLP STATE PROCESS FOR DCC
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
102109* 102109  CR2008100900003  AJRA  ADD CLAIM CERT NOTE IND
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
032612* 032612  CR2011110200001  PEMA  AHL CHANGES
090314* 090314  CR2014081300001  PEMA  LOAD CERTS INVOLVED IN THAO
010716* 010716  CR2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
122002******************************************************************
00021
00022  01  CERTIFICATE-MASTER.
00023      12  CM-RECORD-ID                      PIC XX.
00024          88  VALID-CM-ID                      VALUE 'CM'.
00025
00026      12  CM-CONTROL-PRIMARY.
00027          16  CM-COMPANY-CD                 PIC X.
00028          16  CM-CARRIER                    PIC X.
00029          16  CM-GROUPING.
00030              20  CM-GROUPING-PREFIX        PIC X(3).
00031              20  CM-GROUPING-PRIME         PIC X(3).
00032          16  CM-STATE                      PIC XX.
00033          16  CM-ACCOUNT.
00034              20  CM-ACCOUNT-PREFIX         PIC X(4).
00035              20  CM-ACCOUNT-PRIME          PIC X(6).
00036          16  CM-CERT-EFF-DT                PIC XX.
00037          16  CM-CERT-NO.
00038              20  CM-CERT-PRIME             PIC X(10).
00039              20  CM-CERT-SFX               PIC X.
00040
00041      12  CM-CONTROL-BY-NAME.
00042          16  CM-COMPANY-CD-A1              PIC X.
00043          16  CM-INSURED-LAST-NAME          PIC X(15).
00044          16  CM-INSURED-INITIALS.
00045              20  CM-INSURED-INITIAL1       PIC X.
00046              20  CM-INSURED-INITIAL2       PIC X.
00047
00048      12  CM-CONTROL-BY-SSN.
00049          16  CM-COMPANY-CD-A2              PIC X.
00050          16  CM-SOC-SEC-NO.
00051              20  CM-SSN-STATE              PIC XX.
00052              20  CM-SSN-ACCOUNT            PIC X(6).
00053              20  CM-SSN-LN3.
00054                  25  CM-INSURED-INITIALS-A2.
00055                      30 CM-INSURED-INITIAL1-A2   PIC X.
00056                      30 CM-INSURED-INITIAL2-A2   PIC X.
00057                  25 CM-PART-LAST-NAME-A2         PIC X.
00058
00059      12  CM-CONTROL-BY-CERT-NO.
00060          16  CM-COMPANY-CD-A4              PIC X.
00061          16  CM-CERT-NO-A4                 PIC X(11).
00062
00063      12  CM-CONTROL-BY-MEMB.
00064          16  CM-COMPANY-CD-A5              PIC X.
00065          16  CM-MEMBER-NO.
00066              20  CM-MEMB-STATE             PIC XX.
00067              20  CM-MEMB-ACCOUNT           PIC X(6).
00068              20  CM-MEMB-LN4.
00069                  25  CM-INSURED-INITIALS-A5.
00070                      30 CM-INSURED-INITIAL1-A5   PIC X.
00071                      30 CM-INSURED-INITIAL2-A5   PIC X.
00072                  25 CM-PART-LAST-NAME-A5         PIC XX.
00073
00074      12  CM-INSURED-PROFILE-DATA.
00075          16  CM-INSURED-FIRST-NAME.
00076              20  CM-INSURED-1ST-INIT       PIC X.
00077              20  FILLER                    PIC X(9).
00078          16  CM-INSURED-ISSUE-AGE          PIC 99.
00079          16  CM-INSURED-SEX                PIC X.
00080              88  CM-SEX-MALE                  VALUE 'M'.
00081              88  CM-SEX-FEMAL                 VALUE 'F'.
00082          16  CM-INSURED-JOINT-AGE          PIC 99.
00083          16  CM-JOINT-INSURED-NAME.
00084              20  CM-JT-LAST-NAME           PIC X(15).
00085              20  CM-JT-FIRST-NAME.
00086                  24  CM-JT-1ST-INIT        PIC X.
00087                  24  FILLER                PIC X(9).
00088              20  CM-JT-INITIAL             PIC X.
00089
00090      12  CM-LIFE-DATA.
00091          16  CM-LF-BENEFIT-CD              PIC XX.
00092          16  CM-LF-ORIG-TERM               PIC S999      COMP-3.
00093          16  CM-LF-CRITICAL-PERIOD         PIC S999      COMP-3.
00094          16  CM-LF-TERM-IN-DAYS            PIC S9(5)     COMP-3.
00095          16  CM-LF-DEV-CODE                PIC XXX.
00096          16  CM-LF-DEV-PCT                 PIC S9V9(6)   COMP-3.
00097          16  CM-LF-BENEFIT-AMT             PIC S9(9)V99  COMP-3.
00098          16  CM-LF-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00099          16  CM-LF-ALT-BENEFIT-AMT         PIC S9(9)V99  COMP-3.
00100          16  CM-LF-ALT-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00101          16  CM-LF-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00102          16  CM-LF-REMAINING-AMT           PIC S9(9)V99  COMP-3.
00103          16  CM-LF-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00104          16  CM-LF-ITD-DEATH-AMT           PIC S9(9)V99  COMP-3.
00105          16  CM-LF-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
00106          16  CM-LF-POLICY-FEE              PIC S9(3)V99  COMP-3.
00107          16  CM-LF-ALT-PREMIUM-RATE        PIC S99V9(5)  COMP-3.
090314         16  cm-temp-epiq                  pic xx.
090314             88  EPIQ-CLASS                  value 'EQ'.
090314*        16  FILLER                        PIC XX.
00109
00110      12  CM-AH-DATA.
00111          16  CM-AH-BENEFIT-CD              PIC XX.
00112          16  CM-AH-ORIG-TERM               PIC S999      COMP-3.
00113          16  CM-AH-CRITICAL-PERIOD         PIC S999      COMP-3.
00114          16  CM-AH-DEV-CODE                PIC XXX.
00115          16  CM-AH-DEV-PCT                 PIC S9V9(6)   COMP-3.
00116          16  CM-AH-BENEFIT-AMT             PIC S9(7)V99  COMP-3.
00117          16  CM-AH-PREMIUM-AMT             PIC S9(7)V99  COMP-3.
00118          16  CM-AH-NSP-PREMIUM-AMT         PIC S9(7)V99  COMP-3.
00119          16  CM-AH-ITD-CANCEL-AMT          PIC S9(7)V99  COMP-3.
00120          16  CM-AH-ITD-LUMP-PMT            PIC S9(7)V99  COMP-3.
00121          16  CM-AH-ITD-AH-PMT              PIC S9(9)V99  COMP-3.
00122          16  CM-AH-PAID-THRU-DT            PIC XX.
00123              88  NO-AH-CLAIMS-PAID            VALUE LOW-VALUE.
00124          16  CM-AH-PREMIUM-RATE            PIC S99V9(5)  COMP-3.
010716         16  CM-CANCEL-FEE                 PIC S9(3)V99  COMP-3.
00126          16  CM-AH-CEDED-BENEFIT           PIC S9(7)V99  COMP-3.
00127          16  FILLER                        PIC X.
00128
00129      12  CM-LOAN-INFORMATION.
00130          16  CM-LIVES                      PIC S9(7)     COMP-3.
011410         16  CM-DDF-IU-RATE-UP REDEFINES CM-LIVES
011410                                           PIC S9(5)V99  COMP-3.
00131          16  CM-BILLED                     PIC S9(7)     COMP-3.
00132          16  CM-LOAN-APR                   PIC S999V9(4) COMP-3.
00133          16  CM-PAY-FREQUENCY              PIC S99.
00134          16  CM-LOAN-TERM                  PIC S999      COMP-3.
00135          16  CM-RATE-CLASS                 PIC XX.
00136          16  CM-BENEFICIARY                PIC X(25).
00137          16  CM-POLICY-FORM-NO             PIC X(12).
00138          16  CM-PMT-EXTENSION-DAYS         PIC S999      COMP-3.
00139          16  CM-LAST-ADD-ON-DT             PIC XX.
00140          16  CM-DEDUCTIBLE-AMOUNTS.
00141              20  CM-CLAIM-DEDUCT-WITHHELD  PIC S9(5)V99  COMP-3.
00142              20  CM-CANCEL-DEDUCT-WITHHELD PIC S9(5)V99  COMP-3.
00143          16  CM-RESIDENT-RATE REDEFINES CM-DEDUCTIBLE-AMOUNTS.
00144              20  CM-RESIDENT-STATE         PIC XX.
00145              20  CM-RATE-CODE              PIC X(4).
00146              20  FILLER                    PIC XX.
110105         16  FILLER REDEFINES CM-DEDUCTIBLE-AMOUNTS.
110105             20  CM-LOAN-OFFICER           PIC X(5).
110105             20  FILLER                    PIC XXX.
00147          16  CM-CSR-CODE                   PIC XXX.
00148          16  CM-UNDERWRITING-CODE          PIC X.
00149              88  CM-POLICY-UNDERWRITTEN       VALUE 'Y'.
081606         16  CM-POST-CARD-IND              PIC X.
062017         16  CM-REF-INTERFACE-SW           PIC X.
00151          16  CM-PREMIUM-TYPE               PIC X.
00152              88  CM-SING-PRM                  VALUE '1'.
00153              88  CM-O-B-COVERAGE              VALUE '2'.
00154              88  CM-OPEN-END                  VALUE '3'.
00155          16  CM-IND-GRP-TYPE               PIC X.
00156              88  CM-INDIVIDUAL                VALUE 'I'.
00157              88  CM-GROUP                     VALUE 'G'.
00158          16  CM-SKIP-CODE                  PIC X.
00159              88  NO-MONTHS-SKIPPED            VALUE SPACE.
00160              88  SKIP-JULY                    VALUE '1'.
00161              88  SKIP-AUGUST                  VALUE '2'.
00162              88  SKIP-SEPTEMBER               VALUE '3'.
00163              88  SKIP-JULY-AUG                VALUE '4'.
00164              88  SKIP-AUG-SEPT                VALUE '5'.
00165              88  SKIP-JULY-AUG-SEPT           VALUE '6'.
00166              88  SKIP-JUNE-JULY-AUG           VALUE '7'.
00167              88  SKIP-JUNE                    VALUE '8'.
00168              88  SKIP-JUNE-JULY               VALUE '9'.
00169              88  SKIP-AUG-SEPT-OCT            VALUE 'A'.
00170              88  SKIP-BI-WEEKLY-3RD-PMT       VALUE 'X'.
00171          16  CM-PAYMENT-MODE               PIC X.
00172              88  PAY-MONTHLY                  VALUE SPACE.
00173              88  PAY-WEEKLY                   VALUE '1'.
00174              88  PAY-SEMI-MONTHLY             VALUE '2'.
00175              88  PAY-BI-WEEKLY                VALUE '3'.
00176              88  PAY-SEMI-ANUALLY             VALUE '4'.
00177          16  CM-LOAN-NUMBER                PIC X(8).
00178          16  CM-LOAN-BALANCE               PIC S9(7)V99  COMP-3.
110105         16  CM-OLD-LOF                    PIC XXX.
00179 *        16  CM-LOAN-OFFICER               PIC XXX.
00180          16  CM-REIN-TABLE                 PIC XXX.
00181          16  CM-SPECIAL-REIN-CODE          PIC X.
00182          16  CM-LF-LOAN-EXPIRE-DT          PIC XX.
00183          16  CM-AH-LOAN-EXPIRE-DT          PIC XX.
00184          16  CM-LOAN-1ST-PMT-DT            PIC XX.
00185
00186      12  CM-STATUS-DATA.
00187          16  CM-ENTRY-STATUS               PIC X.
00188          16  CM-ENTRY-DT                   PIC XX.
00189
00190          16  CM-LF-STATUS-AT-CANCEL        PIC X.
00191          16  CM-LF-CANCEL-DT               PIC XX.
00192          16  CM-LF-CANCEL-EXIT-DT          PIC XX.
00193
00194          16  CM-LF-STATUS-AT-DEATH         PIC X.
00195          16  CM-LF-DEATH-DT                PIC XX.
00196          16  CM-LF-DEATH-EXIT-DT           PIC XX.
00197
00198          16  CM-LF-CURRENT-STATUS          PIC X.
00199              88  CM-LF-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00200                                                 'M' '4' '5' '9'.
00201              88  CM-LF-NORMAL-ENTRY           VALUE '1'.
00202              88  CM-LF-POLICY-PENDING         VALUE '2'.
00203              88  CM-LF-POLICY-IS-RESTORE      VALUE '3'.
00204              88  CM-LF-CONVERSION-ENTRY       VALUE '4'.
00205              88  CM-LF-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-LF-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00206              88  CM-LF-LUMP-SUM-DISAB         VALUE '6'.
00207              88  CM-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00208              88  CM-LF-CANCEL-APPLIED         VALUE '8'.
00209              88  CM-LF-IS-REIN-ONLY           VALUE '9'.
00210              88  CM-LF-DECLINED               VALUE 'D'.
00211              88  CM-LF-VOIDED                 VALUE 'V'.
00212
00213          16  CM-AH-STATUS-AT-CANCEL        PIC X.
00214          16  CM-AH-CANCEL-DT               PIC XX.
00215          16  CM-AH-CANCEL-EXIT-DT          PIC XX.
00216
00217          16  CM-AH-STATUS-AT-SETTLEMENT    PIC X.
00218          16  CM-AH-SETTLEMENT-DT           PIC XX.
00219          16  CM-AH-SETTLEMENT-EXIT-DT      PIC XX.
00220
00221          16  CM-AH-CURRENT-STATUS          PIC X.
00222              88  CM-AH-POLICY-IS-ACTIVE       VALUE '1' '2' '3'
00223                                                 'M' '4' '5' '9'.
00224              88  CM-AH-NORMAL-ENTRY           VALUE '1'.
00225              88  CM-AH-POLICY-PENDING         VALUE '2'.
00226              88  CM-AH-POLICY-IS-RESTORE      VALUE '3'.
00227              88  CM-AH-CONVERSION-ENTRY       VALUE '4'.
00228              88  CM-AH-POLICY-IS-REISSUE      VALUE '5'.
                   88  CM-AH-POLICY-IS-CASH         VALUE 'C'.
122002             88  CM-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00229              88  CM-AH-LUMP-SUM-DISAB         VALUE '6'.
00230              88  CM-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00231              88  CM-AH-CANCEL-APPLIED         VALUE '8'.
00232              88  CM-AH-IS-REIN-ONLY           VALUE '9'.
00233              88  CM-AH-DECLINED               VALUE 'D'.
00234              88  CM-AH-VOIDED                 VALUE 'V'.
00235
00236          16  CM-CLAIM-INTERFACE-SW         PIC X.
00237              88  NO-CLAIM-ATTACHED            VALUE SPACE.
00238              88  CERT-AND-CLAIM-ONLINE        VALUE '1'.
00239              88  CERT-WAS-CREATED-FOR-CLAIM   VALUE '2'.
00240          16  CM-CLAIM-ATTACHED-COUNT       PIC S9(4)     COMP.
00241
00242          16  CM-ENTRY-BATCH                PIC X(6).
00243          16  CM-LF-EXIT-BATCH              PIC X(6).
00244          16  CM-AH-EXIT-BATCH              PIC X(6).
00245          16  CM-LAST-MONTH-END             PIC XX.
00246
00247      12  CM-NOTE-SW                        PIC X.
00248          88  CERT-NOTES-ARE-NOT-PRESENT       VALUE ' '.
00249          88  CERT-NOTES-PRESENT               VALUE '1'.
00250          88  BILLING-NOTES-PRESENT            VALUE '2'.
00251          88  CERT-BILLING-NOTES-PRESENT       VALUE '3'.
102109         88  CLAIM-NOTES-PRESENT              VALUE '4'.
102109         88  CLAIM-CERT-NOTES-PRESENT         VALUE '5'.
102109         88  CLAIM-BILLING-NOTES-PRESENT      VALUE '6'.
102109         88  CLAIM-CERT-BILL-NOTES-PRESENT    VALUE '7'.
00252      12  CM-COMP-EXCP-SW                   PIC X.
00253          88  COMPENSATION-SAME-AS-ACCT        VALUE ' '.
00254          88  THIS-CERT-HAS-ERCOMM-ENTRY       VALUE '1'.
00255      12  CM-INSURED-ADDRESS-SW             PIC X.
00256          88  INSURED-ADDR-NOT-PRESENT         VALUE ' '.
00257          88  INSURED-ADDR-PRESENT             VALUE '1'.
00258
011410*    12  CM-LF-CEDED-BENEFIT               PIC S9(7)V99   COMP-3.
011410     12  CM-LF-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
00260
011410*    12  CM-ISS-MICROFILM-NO               PIC S9(9)      COMP-3.
011410     12  CM-AH-CLP                         PIC S9(5)V99   COMP-3.
011410     12  FILLER                            PIC X.
072308*    12  CM-CAN-MICROFILM-NO               PIC S9(9)      COMP-3.
062017     12  CM-INT-ON-REFS                    PIC S9(7)V99   COMP-3.
00263
00264      12  CM-CREDIT-INTERFACE-SW-1          PIC X.
00265          88  CERT-ADDED-BATCH                 VALUE ' '.
00266          88  CERT-ADDED-ONLINE                VALUE '1'.
00267          88  CERT-PEND-ISSUE-ERROR            VALUE '2'.
00268          88  CERT-PURGED-OFFLINE              VALUE '3'.
00269          88  CERT-PEND-ISSUE-RETURNED         VALUE '4'.
00270      12  CM-CREDIT-INTERFACE-SW-2          PIC X.
00271          88  CERT-AS-LOADED                   VALUE ' '.
00272          88  CERT-CANCELLED-ONLINE            VALUE '1'.
00273          88  CERT-CLAIM-ONLINE                VALUE '2'.
00274          88  CERT-CLAIM-CANCEL-ONLINE         VALUE '3'.
00275          88  CERT-PEND-CANCEL-ERROR           VALUE '4'.
00276          88  CERT-PEND-CANCEL-VOID            VALUE '5'.
00277          88  CERT-PEND-CAN-VOID-ERROR         VALUE '6'.
00278          88  CERT-PEND-CANCEL-RETURNED        VALUE '7'.
00279
00280      12  CM-ACCOUNT-COMM-PCTS.
00281          16  CM-LIFE-COMM-PCT              PIC SV9(5)    COMP-3.
00282          16  CM-AH-COMM-PCT                PIC SV9(5)    COMP-3.
00283
00284      12  CM-USER-FIELD                     PIC X.
040504     12  CM-ADDL-CLP                       PIC S9(5)V99  COMP-3.
061405     12  CM-CLP-STATE                      PIC XX.
032612     12  CM-LF-CLASS-CD REDEFINES CM-CLP-STATE PIC XX.
061405     12  CM-USER-RESERVED                  PIC XXX.
032612     12  FILLER REDEFINES CM-USER-RESERVED.
032612         16  CM-AH-CLASS-CD                PIC XX.
032612         16  F                             PIC X.
00286 ******************************************************************
061013*                                    copy ELCCRTT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCCRTT.                            *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CERTIFICATE TRAILERS                      *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 552  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELCRTT                         RKP=2,LEN=34   *
00017 *                                                                *
00018 *   LOG = YES                                                    *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
111204******************************************************************
111204*                   C H A N G E   L O G
111204*
111204* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111204*-----------------------------------------------------------------
111204*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111204* EFFECTIVE    NUMBER
111204*-----------------------------------------------------------------
111204* 111204                   PEMA  NEW FILE TO SPLIT BANK COMM
040109* 040109  2009031600001    AJRA  ADD NEW TRAILER TYPE AND REDEFINE
012010* 012010  2009061500002    AJRA  ADD FLAG FOR REFUND WITH OPEN CLA
121712* 121712  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
061013* 061013  CR2012113000002  PEMA  SPP CLAIM RELATED CHANGES
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
022715* 022715  CR2015010800003  PEMA  AGENT SIGNATURE
020816* 020816  CR2015082500001  PEMA  ADD NEW VPP COMPANY
012918* 012918  CR2017062000002  PEMA  AUDIT NB FOR PREV CLAIMS
091318* 091318  CR2018073000001  PEMA  ADD Refund methods
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
111204******************************************************************
00021
00022  01  CERTIFICATE-TRAILERS.
00023      12  CS-RECORD-ID                      PIC XX.
00024          88  VALID-CS-ID                      VALUE 'CS'.
00025
00026      12  CS-CONTROL-PRIMARY.
00027          16  CS-COMPANY-CD                 PIC X.
00028          16  CS-CARRIER                    PIC X.
00029          16  CS-GROUPING                   PIC X(6).
00032          16  CS-STATE                      PIC XX.
00033          16  CS-ACCOUNT                    PIC X(10).
00036          16  CS-CERT-EFF-DT                PIC XX.
00037          16  CS-CERT-NO.
00038              20  CS-CERT-PRIME             PIC X(10).
00039              20  CS-CERT-SFX               PIC X.
               16  CS-TRAILER-TYPE               PIC X.
                   88  COMM-TRLR           VALUE 'A'.
061013             88  CLAIM-HISTORY-TRLR  VALUE 'B'.
040109             88  CERT-DATA-TRLR      VALUE 'C'.
00040
040109     12  CS-DATA-AREA                      PIC X(516).
040109
040109     12  CS-BANK-COMMISSIONS REDEFINES CS-DATA-AREA.
040109         16  CS-BANK-COMMISSION-AREA.
040109             20  CS-BANK-COMMS       OCCURS 10.
040109                 24  CS-AGT                PIC X(10).
040109                 24  CS-COM-TYP            PIC X.
040109                 24  CS-SPP-FEES           PIC S9(5)V99   COMP-3.
040109                 24  CS-RECALC-LV-INDIC    PIC X.
040109                 24  FILLER                PIC X(10).
040109         16  FILLER                        PIC X(256).
040109
061013     12  CS-CLAIM-HISTORY-TRAILER REDEFINES CS-DATA-AREA.
061013****  TO CALC NO OF BENEFITS PAID = (CS-DAYS-PAID / 30)
               16  CS-MB-CLAIM-DATA OCCURS 24.
                   20  CS-CLAIM-NO               PIC X(7).
                   20  CS-CLAIM-TYPE             PIC X.
                       88  CS-AH-CLM               VALUE 'A'.
                       88  CS-IU-CLM               VALUE 'I'.
                       88  CS-GP-CLM               VALUE 'G'.
                       88  CS-LF-CLM               VALUE 'L'.
                       88  CS-PR-CLM               VALUE 'P'.
052614                 88  CS-FL-CLM               VALUE 'F'.
100518                 88  CS-OT-CLM               VALUE 'O'.
080322                 88  CS-BR-CLM               VALUE 'B'.
080322                 88  CS-HS-CLM               VALUE 'H'.
                   20  CS-INSURED-TYPE           PIC X.
                       88  CS-PRIM-INSURED          VALUE 'P'.
                       88  CS-CO-BORROWER           VALUE 'C'.
                   20  CS-BENEFIT-PERIOD         PIC 99.
                   20  CS-DAYS-PAID              PIC S9(5) COMP-3.
                   20  CS-TOTAL-PAID             PIC S9(7)V99 COMP-3.
                   20  CS-REMAINING-BENS         PIC S999 COMP-3.
               16  FILLER                        PIC X(12).
040109     12  CS-CERT-DATA REDEFINES CS-DATA-AREA.
040109         16  CS-VIN-NUMBER                 PIC X(17).
012010         16  CS-REFUND-CLAIM-FLAG          PIC X(01).
121712         16  CS-INS-AGE-DEFAULT-FLAG       PIC X(01).
121712         16  CS-JNT-AGE-DEFAULT-FLAG       PIC X(01).
022715         16  cs-agent-name.
022715             20  cs-agent-fname            pic x(20).
022715             20  cs-agent-mi               pic x.
022715             20  cs-agent-lname            pic x(25).
022715         16  cs-license-no                 pic x(15).
022715         16  cs-npn-number                 pic x(10).
022715         16  cs-agent-edit-status          pic x.
022715             88  cs-ae-refer-to-manager      value 'M'.
022715             88  cs-ae-cover-sheet           value 'C'.
022715             88  cs-ae-sig-form              value 'S'.
022715             88  cs-ae-verified              value 'V'.
022715             88  cs-unidentified-signature   value 'U'.
022715             88  cs-cert-returned            value 'R'.
022715             88  cs-accept-no-commission     value 'N'.
020816         16  cs-year                       pic 9999.
020816         16  cs-make                       pic x(20).
020816         16  cs-model                      pic x(20).
020816         16  cs-future                     pic x(20).
020816         16  cs-vehicle-odometer           pic s9(7) comp-3.
012918         16  cs-claim-verification-status  pic x.
012918             88  cs-clm-ver-eligible         value 'A'.
012918             88  cs-clm-ver-partial-elig     value 'B'.
012918             88  cs-clm-ver-not-eligible     value 'C'.
012918             88  cs-clm-ver-not-elig-opn-clm value 'D'.
012918             88  cs-clm-ver-not-part-elig-rw value 'E'.
012918             88  cs-clm-ver-ND-CERT          value 'F'.
012918             88  cs-clm-ver-spec-other       value 'G'.
012918             88  cs-clam-ver-pratial-corrected
012918                                             value 'H'.
012918             88  cs-clm-ver-no-matches       value 'I'.
012918             88  cs-clm-ver-not-elig-corrected
012918                                             value 'J'.
012918             88  cs-clm-ver-needs-review     value 'R'.
012918             88  cs-clm-ver-sent-to-claims   value 'W'.
091318         16  CS-LF-REFUND-METHOD           PIC X.
091318         16  CS-AH-REFUND-METHOD           PIC X.
020816         16  FILLER                        PIC X(353). *> was 420
121712*        16  FILLER                        PIC X(496).
CIDMOD/
CIDMOD*    COPY ELCDAR.
00001 ******************************************************************
00002 *                                                                *
00003 *   FILE DESC. = DAILY ACTIVITY FILE, FOR PROCESSING NITELY      *
00004 *   FILE TYPE = VSAM,KSDS                                        *
00005 *   RECORD SIZE = 25   RECFORM = FIXED                           *
00006 *   BASE CLUSTER = DLYACTV                                       *
00007 *   LOG = YES                                                    *
00008 *   NARRATIVE - FILE IS BUILT DURING DAYTIME CICS PROCESSING AND *
00009 *               IS THEN PROCESSED BY CYCLE PROCESSING AT NIGHT.  *
00010 *               THIS IS USED TO BUILD THE LOGIC "F" EXTRACT      *
00011 *               RECORDS FOR THOSE CLAIMS WHICH HAVE HAD ACTIVITY *
00012 *               DURING THE DAY. THE EXTRACTS THEN GET READ IN    *
00013 *               BY PROGRAM "LGINFCE".                            *
00014 *                                                                *
00015 ******************************************************************
00016  01  DAILY-ACTIVITY-RECORD.
00017      05  DA-KEY.
00018          10  DA-COMP-CD          PIC X.
00019          10  DA-CARRIER          PIC X.
00020          10  DA-CLAIM-NO         PIC X(7).
00021          10  DA-CERT-NO.
00022              15  DA-CERT-PRIME   PIC X(10).
00023              15  DA-CERT-SFX     PIC X.
00024      05  DA-TRAILER-SEQ-NO       PIC S9(4)  COMP.
00025      05  DA-RECORD-TYPE          PIC X.
00026      05  FILLER                  PIC X(2).
00027 ******************************************************************
00229      EJECT
00230 *    COPY MPCPLCY.
00001 ******************************************************************
00002 *                                                                *
00003 *                           MPCPLCY                              *
00004 *                            VMOD=1.024                          *
00005 *                                                                *
00006 *   FILE DESCRIPTION = POLICY MASTER                             *
00007 *                                                                *
00008 *   FILE TYPE = VSAM,KSDS                                        *
00009 *   RECORD SIZE = 1200 RECFORM = FIXED                           *
00010 *                                                                *
00011 *   BASE CLUSTER = MPPLCY                         RKP=2,LEN=42   *
00012 *       ALTERNATE PATH2 = ** NOT USED **                         *
00013 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00014 *       ALTERNATE PATH4 = MPPLCY4 (BY REF. NO.)   RKP=60,LEN=25  *
00015 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT )   RKP=85,LEN=27  *
00016 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT )   RKP=112,LEN=15 *
00017 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO.)   RKP=127,LEN=27 *
00018 *                                                                *
00019 *   LOG = YES                                                    *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 ******************************************************************
00022 **WARNING*********************************************************
00023 **ANY CHANGES TO THIS COPY BOOK MAY NEED CORRESPONDING CHANGES****
00024 **TO THE FOLLOWING COPY BOOKS: MPCPOLUP                          *
00025 **                             MPCPHSTD                          *
00026 **                             MPCPHSTC                          *
00027 **                             MPCPHSTT                          *
00028 **                                                               *
00029 ******************************************************************
00030
00031  01  POLICY-MASTER.
00032      12  PM-RECORD-ID                      PIC XX.
00033          88  VALID-PM-ID                      VALUE 'PM'.
00034
00035 ******************************************************************
00036 *   BASE CLUSTER = MPPLCY         (BASE KEY)      RKP=2,LEN=42   *
00037 ******************************************************************
00038
00039      12  PM-CONTROL-PRIMARY.
00040          16  PM-PRODUCER-PRIMARY.
00041              20  PM-PROD-PRIMARY.
00042                  24  PM-COMPANY-CD         PIC X.
00043                  24  PM-CGSP-KEY.
00044                      28  PM-CARRIER        PIC X.
00045                      28  PM-GROUPING.
00046                          32  PM-GROUPING-PREFIX
00047                                            PIC X(3).
00048                          32  PM-GROUPING-PRIME
00049                                            PIC X(3).
00050                      28  PM-STATE          PIC X(2).
00051                      28  PM-PRODUCER.
00052                          32  PM-PRODUCER-PREFIX
00053                                            PIC X(4).
00054                          32  PM-PRODUCER-PRIME
00055                                            PIC X(6).
00056              20  PM-POLICY-EFF-DT              PIC XX.
00057          16  PM-REFERENCE-NUMBER.
00058              20  PM-REFNO-PRIME            PIC X(18).
00059              20  PM-REFNO-SFX              PIC XX.
00060
00061 ******************************************************************
00062 *       ALTERNATE PATH3 = MPPLCY3 (BY INSD SS NO) RKP=44,LEN=16  *
00063 ******************************************************************
00064
00065      12  PM-CONTROL-BY-SSN.
00066          16  PM-COMPANY-CD-A3              PIC X.
00067          16  PM-SOC-SEC-NO.
00068              20  PM-SSN-STATE              PIC XX.
00069              20  PM-SSN-PRODUCER           PIC X(6).
00070              20  PM-SSN-LN3.
00071                  25  PM-INSURED-INITIALS-A3.
00072                      30 PM-INSURED-INITIAL1-A3 PIC X.
00073                      30 PM-INSURED-INITIAL2-A3 PIC X.
00074                  25 PM-PART-LAST-NAME-A3         PIC X.
00075          16  PM-DATE-A3                     PIC XX.
00076          16  PM-TIME-A3                     PIC S9(04)   COMP.
00077
00078 ******************************************************************
00079 *       ALTERNATE PATH4 = MPPLCY4 (BY REFRENCE)   RKP=60,LEN=25  *
00080 ******************************************************************
00081
00082      12  PM-CONTROL-BY-POLICY-NO.
00083          16  PM-COMPANY-CD-A4              PIC X.
00084          16  PM-POLICY-NO-A4.
00085              20  PM-POLICY-PRIME-A4        PIC X(18).
00086              20  PM-POLICY-SFX-A4          PIC XX.
00087          16  PM-DATE-A4                    PIC XX.
00088          16  PM-TIME-A4                    PIC S9(04)   COMP.
00089
00090 ******************************************************************
00091 *       ALTERNATE PATH5 = MPPLCY5 (BY ACCOUNT NO) RKP=85,LEN=27  *
00092 ******************************************************************
00093
00094      12  PM-CONTROL-BY-ACCOUNT.
00095          16  PM-COMPANY-CD-A5              PIC X.
00096          16  PM-BANK-ACCOUNT-NUMBER        PIC X(20).
00097          16  PM-DATE-A5                    PIC XX.
00098          16  PM-TIME-A5                    PIC S9(07)   COMP.
00099
00100 ******************************************************************
00101 *       ALTERNATE PATH6 = MPPLCY6 (BY TRANSIT NO) RKP=112,LEN=15 *
00102 ******************************************************************
00103
00104      12  PM-CONTROL-BY-TRANSIT.
00105          16  PM-COMPANY-CD-A6              PIC X.
00106          16  PM-BANK-TRANSIT-NUMBER.
00107              20  PM-FEDERAL-NUMBER         PIC X(4).
00108              20  PM-BANK-NUMBER            PIC X(4).
00109          16  PM-DATE-A6                    PIC XX.
00110          16  PM-TIME-A6                    PIC S9(07)   COMP.
00111
00112 ******************************************************************
00113 *       ALTERNATE PATH7 = MPPLCY7 (BY LOAN NO)    RKP=127,LEN=27 *
00114 ******************************************************************
00115
00116      12  PM-CONTROL-BY-LOAN-NO.
00117          16  PM-COMPANY-CD-A7              PIC X.
00118          16  PM-LOAN-NUMBER                PIC X(20).
00119          16  PM-DATE-A7                    PIC XX.
00120          16  PM-TIME-A7                    PIC S9(07)   COMP.
00121
00122 ******************************************************************
00123 *                 FILE SYNCHRONIZATION DATA                      *
00124 ******************************************************************
00125
00126      12  FILLER                            PIC X(05).
00127      12  PM-FILE-SYNCH-DATA.
00128          16  PM-LAST-CHANGE-DT             PIC XX.
00129          16  PM-LAST-CHANGE-TIME           PIC S9(7)    COMP.
00130          16  PM-LAST-CHANGE-PROCESSOR      PIC X(4).
00131      12  FILLER                            PIC X(05).
00132
00133 ******************************************************************
00134 *                    INSUREDS PROFILE DATA                       *
00135 ******************************************************************
00136
00137      12  PM-INSURED-PROFILE-DATA.
00138          16  PM-INSURED-NAME.
00139              20  PM-INSURED-LAST-NAME     PIC X(15).
00140              20  PM-INSURED-FIRST-NAME.
00141                  24  PM-INSURED-1ST-INIT PIC X.
00142                  24  FILLER               PIC X(9).
00143              20  PM-INSURED-MIDDLE-INIT PIC X.
00144          16  PM-INSURED-ADDRESS.
00145              20  PM-ADDRESS-LINE-1         PIC X(30).
00146              20  PM-ADDRESS-LINE-2         PIC X(30).
00147              20  PM-CITY                   PIC X(25).
00148              20  PM-RESIDENT-STATE         PIC XX.
00149              20  PM-ZIP-CD.
00150                  24  PM-ZIP-FIRST-FIVE     PIC X(5).
00151                  24  PM-ZIP-PLUS-FOUR      PIC X(4).
00152          16  PM-INSURED-PERSONAL.
00153              20  PM-INSURED-OCC-CLASS      PIC X.
00154                  88  PM-PREFERRED            VALUE '1'.
00155                  88  PM-STANDARD             VALUE '2'.
00156                  88  PM-HAZARDOUS            VALUE '3'.
00157                  88  PM-VERY-HAZARDOUS       VALUE '4'.
00158                  88  PM-EXTREME-HAZARDOUS VALUE '5'.
00159                  88  PM-NOT-OCC              VALUE '6'.
00160                  88  PM-OCC-UNKNOWN          VALUE '9'.
00161              20  PM-INSURED-OCC-CD         PIC X(3).
00162              20  PM-INSURED-OCC-CD-NUM REDEFINES
00163                  PM-INSURED-OCC-CD         PIC 9(3).
00164              20  PM-INSURED-SEX            PIC X.
00165                  88  PM-INSURED-SEX-MALE      VALUE 'M'.
00166                  88  PM-INSURED-SEX-FEMALE VALUE 'F'.
00167              20  PM-INSURED-BIRTH-DT       PIC XX.
00168              20  PM-INSURED-ISSUE-AGE      PIC S9(3)     COMP-3.
00169              20  PM-INSURED-HEIGHT-FT      PIC S9(3)     COMP-3.
00170              20  PM-INSURED-HEIGHT-IN      PIC S9(3)     COMP-3.
00171              20  PM-INSURED-WEIGHT         PIC S9(3)     COMP-3.
00172              20  PM-INSURED-BIRTH-STATE PIC XX.
00173              20  PM-INSURED-PHONE-NO       PIC X(13).
00174              20  PM-INSURED-RATED-AGE      PIC S9(3)     COMP-3.
00175          16  PM-INS-LANGUAGE-IND           PIC X(01).
00176              88  PM-ENGLISH                           VALUE 'E'.
00177              88  PM-FRENCH                            VALUE 'F'.
00178              88  PM-SPANISH                           VALUE 'S'.
00179          16  PM-INSURED-TOT-BENEFIT        PIC S9(7)V99  COMP-3.
00180
00181          16  PM-INSURED-AGE-IND            PIC X(01).
00182              88  PM-INSURED-AGE-75-REACHED            VALUE 'Y'.
00183      12  FILLER                            PIC X(13).
00184
00185 ******************************************************************
00186 *                JOINT INSUREDS PROFILE DATA                     *
00187 ******************************************************************
00188
00189      12  PM-JOINT-PROFILE-DATA.
00190          16  PM-JOINT-NAME.
00191              20  PM-JOINT-LAST-NAME        PIC X(15).
00192              20  PM-JOINT-FIRST-NAME.
00193                  24  PM-JOINT-1ST-INIT     PIC X.
00194                  24  FILLER                PIC X(9).
00195              20  PM-JOINT-MIDDLE-INIT      PIC X.
00196          16  PM-JOINT-SOC-SEC-NO.
00197              20  PM-JT-SSN-STATE           PIC XX.
00198              20  PM-JT-SSN-PRODUCER        PIC X(6).
00199              20  PM-JT-SSN-LN3.
00200                  25  PM-JT-INSURED-INITIALS-A3.
00201                      30 PM-JT-INSURED-INITIAL1-A3 PIC X.
00202                      30 PM-JT-INSURED-INITIAL2-A3 PIC X.
00203                  25 PM-JT-PART-LAST-NAME-A3        PIC X.
00204          16  PM-JOINT-PERSONAL.
00205              20  PM-JOINT-OCC-CLASS        PIC X.
00206                  88 PM-JNT-PREFERRED          VALUE '1'.
00207                  88 PM-JNT-STANDARD           VALUE '2'.
00208                  88 PM-JNT-HAZARDOUS          VALUE '3'.
00209                  88 PM-JNT-VERY-HAZARDOUS     VALUE '4'.
00210                  88 PM-JNT-EXTREME-HAZARDOUS VALUE '5'.
00211                  88 PM-JNT-NOT-OCC            VALUE '6'.
00212                  88 PM-JNT-OCC-UNKNOWN        VALUE '9'.
00213              20  PM-JOINT-OCC-CD           PIC X(3).
00214              20  PM-JOINT-SEX              PIC X.
00215                  88  PM-JOINT-SEX-MALE        VALUE 'M'.
00216                  88  PM-JOINT-SEX-FEMALE      VALUE 'F'.
00217              20  PM-JOINT-BIRTH-DT         PIC XX.
00218              20  PM-JOINT-ISSUE-AGE        PIC S9(3)     COMP-3.
00219              20  PM-JOINT-HEIGHT-FT        PIC S9(3)     COMP-3.
00220              20  PM-JOINT-HEIGHT-IN        PIC S9(3)     COMP-3.
00221              20  PM-JOINT-WEIGHT           PIC S9(3)     COMP-3.
00222              20  PM-JOINT-BIRTH-STATE      PIC XX.
00223              20  PM-JOINT-RATED-AGE        PIC S9(3)     COMP-3.
00224          16  PM-JOINT-TOT-BENEFIT          PIC S9(7)V99  COMP-3.
00225          16  PM-JOINT-AGE-IND              PIC X(01).
00226              88  PM-JOINT-AGE-75-REACHED              VALUE 'Y'.
00227
00228      12  FILLER                            PIC X(12).
00229
00230 ******************************************************************
00231 *                  INSURANCE COVERAGE DATA                       *
00232 ******************************************************************
00233
00234      12  PM-INS-COVERAGE-DATA.
00235          16  PM-FREE-PERIOD                PIC S9(03)    COMP-3.
00236          16  PM-LOAN-TERM                  PIC S9(3)     COMP-3.
00237          16  PM-LOAN-APR                   PIC S9V9999   COMP-3.
00238          16  PM-LOAN-DT                    PIC XX.
00239          16  PM-LOAN-PYMT                  PIC S9(5)V99  COMP-3.
00240          16  PM-LOAN-BALC                  PIC S9(7)V99  COMP-3.
00241          16  PM-INS-BENEFIT-MONTHS         PIC S9(3)     COMP-3.
00242          16  PM-INS-MONTH-BENEFIT          PIC S9(7)V99  COMP-3.
00243          16  PM-INS-TOTAL-BENEFIT          PIC S9(7)V99  COMP-3.
00244          16  PM-INS-PLAN-TYPE              PIC X.
00245              88  PM-AH-MORT-PLAN              VALUE 'A'.
00246              88  PM-AD-D-MORT-PLAN            VALUE 'E'.
00247              88  PM-DISMEM-MORT-PLAN          VALUE 'D'.
00248              88  PM-LIFE-MORT-PLAN            VALUE 'L'.
00249          16  PM-INS-PLAN-CD                PIC XX.
00250          16  PM-INS-PLAN-REVISION          PIC X(3).
00251          16  PM-INS-POLICY-FORM            PIC X(12).
00252          16  PM-INS-MSTR-POLICY.
00253              20  PM-FREE-TYPE              PIC X(04).
00254              20  FILLER                    PIC X(08).
00255          16  PM-INS-MSTR-APP.
00256              20  FILLER                    PIC X(11).
00257              20  PM-INS-B-C-TYPE           PIC X(01).
00258          16  PM-INS-RATE-CD                PIC X(5).
00259          16  PM-INS-SEX-RATING             PIC X.
00260              88  PM-NOT-SEX-RATED              VALUE '1'.
00261              88  PM-SEX-RATED                  VALUE '2'.
00262          16  PM-INS-SUBSTANDARD-PCT        PIC S9V9999   COMP-3.
00263          16  PM-INS-SUBSTANDARD-TYPE       PIC X.
00264          16  PM-INS-TERMINATION-DT         PIC XX.
00265          16  PM-INS-MONTH-PREMIUM      PIC S9(5)V999999  COMP-3.
00266          16  PM-INS-CALC-MO-PREM       PIC S9(5)V999999  COMP-3.
00267          16  PM-REINSURANCE-TABLE          PIC X(3).
00268          16  PM-MORTALITY-CD               PIC X(4).
00269          16  PM-INS-TYPE                   PIC X.
00270              88  PM-INDIVIDUAL                VALUES ARE '1' 'I'.
00271              88  PM-GROUP                     VALUES ARE '2' 'G'.
00272          16  PM-LOAN-OFFICER               PIC X(5).
00273          16  PM-POLICY-FEE                 PIC S9(3)V99 COMP-3.
00274          16  PM-DEPENDENT-COUNT            PIC S99      COMP-3.
00275          16  PM-CWA-AMOUNT                 PIC S9(5)V99  COMP-3.
00276          16  PM-LAST-AUTO-RERATE-DT        PIC XX.
00277          16  PM-PREM-FINANCED-SW           PIC X.
00278              88  PM-PREM-FINANCED              VALUE 'Y'.
00279              88  PM-PREM-NOT-FINANCED          VALUE 'N'.
00280
00281          16  PM-INS-TERM-LETTER-IND        PIC X.
00282              88  PM-TERM-INITIALIZED           VALUE 'Y'.
00283          16  PM-INS-UNDERWRITER-MAX-BEN PIC S9(7)V99     COMP-3.
00284      12  FILLER                            PIC X(11).
00285
00286 ******************************************************************
00287 *                    POLICY BILLING DATA                         *
00288 ******************************************************************
00289
00290      12  PM-BILLING-DATA.
00291          16  PM-BILLING-MODE               PIC X(1).
00292              88  PM-ANNUAL                    VALUE '1'.
00293              88  PM-SEMI-ANNUAL               VALUE '2'.
00294              88  PM-QUARTERLY                 VALUE '3'.
00295              88  PM-MONTHLY                   VALUE '4'.
00296              88  PM-BI-MONTHLY                VALUE '5'.
00297              88  PM-SINGLE-PREM               VALUE '6'.
00298          16  PM-BILLING-SCHEDULE           PIC X(1).
00299          16  PM-BILLING-SW                 PIC X(1).
00300              88  PM-FIRST-BILLING             VALUE 'Y'.
00301              88  PM-PAID-IN-ADVANCE           VALUE 'A'.
00302              88  PM-POLICY-FEE-REFUNDED       VALUE 'F'.
00303          16  PM-BILLING-TYPE               PIC X(1).
00304              88  PM-LIST-BILL                 VALUE '1'.
00305              88  PM-TAPE-BILL                 VALUE '2'.
00306              88  PM-TAPE-LIST-BILL            VALUE '3'.
00307              88  PM-GROUP-BILL          VALUE ARE '1' '2' '3'.
00308              88  PM-DIRECT-BILL               VALUE '4'.
00309              88  PM-PAC-BILL            VALUE ARE '5' 'C' 'S'.
00310              88  PM-CHARGE-CARD-BILL          VALUE '6'.
00311              88  PM-INDIV-BILL
00312                                   VALUE ARE '4' '5' '6' 'C' 'S'.
00313              88  PM-GRP-PLCY-BILL             VALUE '7'.
00314              88  PM-GRP-PLCY-PAC              VALUE '8'.
00315              88  PM-GRP-PLCY-CR-CRD           VALUE '9'.
00316              88  PM-GRP-PLCY            VALUE ARE '7' '8' '9'.
00317              88  PM-GRP-PROD                  VALUE 'A'.
00318              88  PM-EFT-CHECKING              VALUE 'C'.
00319              88  PM-EFT-SAVINGS               VALUE 'S'.
00320          16  PM-PAYMENT-AMT                PIC S9(5)V99  COMP-3.
00321          16  PM-OVER-SHORT-AMT             PIC S9(5)V99  COMP-3.
00322          16  PM-LAST-BILL-DT               PIC XX.
00323          16  PM-LAST-BILL-AMT              PIC S9(5)V99  COMP-3.
00324          16  PM-BILL-TO-DT                 PIC XX.
00325          16  PM-LAST-PYMT-DT               PIC XX.
00326          16  PM-PAID-TO-DT                 PIC XX.
00327          16  PM-PYMT-INVOICE-NUMBER        PIC X(6).
00328          16  PM-MONTHS-PAID                PIC S9(3)     COMP-3.
00329          16  PM-TOTAL-PREM-RECVD           PIC S9(7)V99  COMP-3.
00330          16  PM-BILLING-GROUPING-CODE      PIC X(6).
00331          16  PM-CHARGE-CARD-EXP-DT         PIC X(2).
00332          16  PM-CHARGE-CARD-TYPE           PIC X(2).
00333              88  PM-VISA                      VALUE 'VI'.
00334              88  PM-MSTR-CARD                 VALUE 'MC'.
00335              88  PM-DINERS-CLUB               VALUE 'DN'.
00336              88  PM-DISCOVER                  VALUE 'DS'.
00337              88  PM-CARTE-BLANCHE             VALUE 'CB'.
00338              88  PM-AMERICAN-EXPRESS          VALUE 'AE'.
00339          16  PM-BILL-INVOICE-NUMBER        PIC X(6).
00340          16  PM-BILL-DAY                   PIC S99       COMP-3.
00341          16  PM-RES-PREM-TAX           PIC S9(3)V999999  COMP-3.
00342      12  FILLER                            PIC X(15).
00343
00344 ******************************************************************
00345 *                     CLAIM PAYMENT DATA                         *
00346 ******************************************************************
00347
00348      12  PM-CLAIM-PAYMENT-DATA.
00349          16  PM-CLAIM-BENEFICIARY-NAME     PIC X(25).
00350          16  PM-CLAIM-INTERFACE-SW         PIC X.
00351              88  PM-NO-CLAIM-ATTACHED         VALUE SPACE.
00352              88  PM-POLICY-AND-CLAIM-ONLINE VALUE '1'.
00353              88  PM-POLICY-CREATED-FOR-CLAIM VALUE '2'.
00354              88  PM-CLAIM-CLOSED              VALUE '3'.
00355              88  PM-ACTIVE-CLAIM              VALUE '1' '2'.
00356              88  PM-CLAIM-ATTACHED            VALUE '1' '2' '3'.
00357          16  PM-CLAIM-INCURRED-DT          PIC XX.
00358          16  PM-CLAIM-PAID-TO-DT           PIC XX.
00359          16  PM-CLAIM-PAYMENT-CNT          PIC S9(3)     COMP-3.
00360          16  PM-CLAIM-LAST-PAYMENT-AMT     PIC S9(7)V99  COMP-3.
00361          16  PM-CLAIM-EXPENSES-ITD         PIC S9(7)V99  COMP-3.
00362          16  PM-CLAIM-PAYMENTS-ITD         PIC S9(7)V99  COMP-3.
00363          16  PM-CLAIM-ACCUMULATOR          PIC S9(7)V99  COMP-3.
00364          16  PM-CLAIM-ATTACH-CNT           PIC S9(3)     COMP-3.
00365          16  PM-CLAIM-LIFE-ITD             PIC S9(7)V99  COMP-3.
00366          16  PM-CLAIM-AH-ITD               PIC S9(7)V99  COMP-3.
00367          16  PM-CLAIM-RIDER-ITD            PIC S9(7)V99  COMP-3.
00368
00369      12  FILLER                            PIC X(03).
00370
00371 ******************************************************************
00372 *                POLICY STATUS AND DISPOSITION                   *
00373 ******************************************************************
00374
00375      12  PM-STATUS-DISPOSITION-DATA.
00376          16  PM-ISSUE-EOM-DT               PIC XX.
00377          16  PM-REPLACEMENT-SWITCH         PIC X.
00378          16  PM-APPL-SIGN-DT               PIC XX.
00379          16  PM-UNDERWRITER                PIC X(3).
00380          16  PM-ENTRY-PROCESSOR            PIC X(4).
00381          16  PM-ENTRY-STATUS               PIC X.
00382              88  PM-NORMAL                    VALUE '1'.
00383              88  PM-TAKE-OVER                 VALUE '2'.
00384              88  PM-CONVERSION                VALUE '4'.
00385              88  PM-RE-ISSUE                  VALUE '5'.
00386              88  PM-REINSURANCE-ONLY          VALUE '9'.
00387          16  PM-ENTRY-DT                   PIC XX.
00388          16  PM-ENTRY-TIME                 PIC S9(7) COMP-3.
00389          16  PM-EXIT-DT                    PIC XX.
00390          16  PM-CURRENT-STATUS             PIC X.
00391              88  PM-LAPSE                     VALUE '0'.
00392              88  PM-ACTIVE                    VALUE '1'.
00393              88  PM-PENDING-ISSUE             VALUE '2'.
00394              88  PM-DECLINED                  VALUE '3'.
00395              88  PM-PENDING-CANCEL            VALUE '4'.
00396              88  PM-PENDING-ISSUE-ERROR       VALUE '5'.
00397              88  PM-CLAIM-APPLIED             VALUE '6'.
00398              88  PM-CANCEL                    VALUE '7'.
00399              88  PM-PENDING-UNWTR-REVW        VALUE '8'.
00400              88  PM-PENDING-CANCEL-ERROR      VALUE '9'.
00401              88  PM-CANCEL-TRANSFER           VALUE 'C'.
00402              88  PM-CLAIM-SETTLEMENT          VALUE 'F'.
00403              88  PM-TERMINATE                 VALUE 'T'.
00404 ** NOTE TYPE 1 IS ANYTHING THAT IS OR HAS BEEN ACTIVE.  TYPE 2 IS
00405 ** EVERYTHING ELSE.  IF YOU ADD A STATUS ADD THE VALUE TO ONE OF
00406 ** THESE GROUPS.
00407              88  PM-TYPE-STAT-1
00408                      VALUES ARE '0' '1' '4' '6' '7' '9'
00409                                 'C' 'F' 'T'.
00410              88  PM-TYPE-STAT-2
00411                      VALUES ARE '2' '3' '5' '8'.
00412              88  PM-BILLABLE-STATUS VALUES ARE '0' '1' '6'.
00413              88  PM-PENDING-STATUS
00414                                 VALUES ARE '2' '4' '5' '8' '9'.
00415              88  PM-PENDING-ISSUE-STATUS
00416                                 VALUES ARE '2' '5' '8'.
00417              88  PM-CANCEL-STATUS
00418                                 VALUES ARE '4' '7' '9' 'C'.
00419          16  PM-CANCEL-CAUSE-CD            PIC X(3).
00420          16  PM-CANCEL-DT                  PIC XX.
00421          16  PM-REFUND-AMT                 PIC S9(5)V99  COMP-3.
00422          16  PM-CALC-REFUND-AMT            PIC S9(5)V99  COMP-3.
00423          16  PM-DECLINE-CD                 PIC X(3).
00424          16  PM-DECLINE-DT                 PIC XX.
00425          16  PM-LAST-LAPSE-DT              PIC XX.
00426          16  PM-LAST-REINSTATE-DT          PIC XX.
00427          16  PM-SECURITY-ACCESS-CODE       PIC X.
00428          16  PM-PREV-CONTROL-PRIMARY.
00429              20  PM-PREV-COMPANY-CD             PIC X.
00430              20  PM-PREV-CARRIER                PIC X.
00431              20  PM-PREV-GROUPING.
00432                  24  PM-PREV-GROUPING-PREFIX PIC X(3).
00433                  24  PM-PREV-GROUPING-PRIME     PIC X(3).
00434              20  PM-PREV-STATE                  PIC XX.
00435              20  PM-PREV-PRODUCER.
00436                  24  PM-PREV-PRODUCER-PREFIX PIC X(4).
00437                  24  PM-PREV-PRODUCER-PRIME     PIC X(6).
00438              20  PM-PREV-POLICY-EFF-DT          PIC XX.
00439              20  PM-PREV-REFERENCE-NUMBER.
00440                  24  PM-PREV-REFNO-PRIME        PIC X(18).
00441                  24  PM-PREV-REFNO-SFX          PIC XX.
00442          16  PM-ACTION-DT                  PIC XX.
00443          16  PM-ACTION-CODE                PIC X(3).
00444          16  PM-ACTION-DT-2                PIC XX.
00445          16  PM-ACTION-CODE-2              PIC X(3).
00446          16  PM-ACTION-DT-3                PIC XX.
00447          16  PM-ACTION-CODE-3              PIC X(3).
00448          16  PM-ACTION-DT-4                PIC XX.
00449          16  PM-ACTION-CODE-4              PIC X(3).
00450          16  PM-ACTION-DT-5                PIC XX.
00451          16  PM-ACTION-CODE-5              PIC X(3).
00452
00453          16  PM-KEY-CHANGE                 PIC X.
00454                  88  PM-NO-KEY-CHG      VALUES ARE ' ' 'N'.
00455                  88  PM-KEY-CHG              VALUE 'Y'.
00456          16  PM-KEY-CHANGE-DT              PIC XX.
00457
00458          16  PM-RTI-INDICATOR              PIC X.
00459          16  PM-REASON-CODE                PIC X(3).
00460          16  PM-IN-OUT-PROCESSING-IND      PIC X(1).
00461              88  PM-IN-OUT-PROCESSING      VALUE 'Y'.
00462              88  PM-NOT-IN-OUT-PROCESSING  VALUE SPACES.
00463
00464      12  FILLER                            PIC X(12).
00465
00466 ******************************************************************
00467 *                 AGENT AND COMMISSION DATA                      *
00468 ******************************************************************
00469
00470      12  PM-COMMISSION-DATA.
00471          16  PM-REMIT-TO                   PIC S9(3) COMP-3.
00472          16  PM-COMM-CHANGE-SW             PIC X.
00473                  88  PM-COMMISSION-CHANGE     VALUE 'Y'.
00474          16  PM-AGENT-INFORMATION OCCURS     5 TIMES.
00475              20  PM-AGENT-NUMBER           PIC X(10).
00476              20  PM-AGENT-TYPE             PIC X.
00477                  88  PM-PRODUCER-LEVEL-AGENT
00478                                               VALUES ARE 'C' 'D'.
00479                  88  PM-AGENT-GROSS           VALUE 'C'.
00480                  88  PM-AGENT-REINS           VALUE 'R'.
00481                  88  PM-AGENT-GROSS-REINS     VALUE 'D'.
00482                  88  PM-OVERWRITE-GROSS       VALUE 'O'.
00483                  88  PM-OVERWRITE-GROSS-REINS VALUE 'P'.
00484                  88  PM-OVERWRITE-REINS       VALUE 'T'.
00485                  88  PM-REINS-ONLY            VALUE 'W'.
00486              20  PM-COMMISSION-BILL-PAID PIC X(1).
00487                  88  PM-GENERATE-BILL         VALUE 'B'.
00488                  88  PM-GENERATE-PAID         VALUE 'P'.
00489              20  PM-AGENT-COMP-1ST-YEAR PIC S99V999.
00490              20  PM-COMP-1ST-YEAR-TYPE     PIC X(1).
00491                  88  PM-COMP-1ST-YEAR-PERCENT VALUE '1'.
00492                  88  PM-COMP-1ST-YEAR-DOLLARS VALUE '2'.
00493                  88  PM-COMP-1ST-YEAR-NOT-USED VALUE '3'.
00494              20  PM-RENEWAL-DATA.
00495                  24  PM-AGENT-RENEWAL-DATA OCCURS 6 TIMES.
00496                      28  PM-RENEW-MONTHS     PIC S999    COMP-3.
00497                      28  PM-RENEW-COMMISSION
00498                                              PIC S99V999 COMP-3.
00499                      28  PM-RENEW-TYPE       PIC X(1).
00500                          88  PM-COMP-RENEW-PERCENT      VALUE '1'.
00501                          88  PM-COMP-RENEW-DOLLARS      VALUE '2'.
00502                          88  PM-COMP-RENEW-NOT-USED     VALUE '3'.
00503              20  PM-COMP-RECALC-FLAG       PIC X(1).
00504                  88  PM-BYPASS-RECALC         VALUE 'N'.
00505      12  FILLER                            PIC X(20).
00506 ******************************************************************
00507 *             CUSTOMER DATA                                      *
00508 ******************************************************************
00509      12  PM-CUSTOMER-ID                    PIC X(20).
00510 ******************************************************************
00511      12  FILLER                            PIC X(43).
00512 ******************************************************************
00231
00232      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE
                                CLAIM-MASTER ACTIVITY-TRAILERS
                                ACTIVITY-QUE CERTIFICATE-MASTER
                                CERTIFICATE-TRAILERS
                                DAILY-ACTIVITY-RECORD POLICY-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL143' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00234
00235      MOVE EIBTRMID               TO WS-QID-TERM.
00236
00237      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00238      MOVE '5'                    TO DC-OPTION-CODE.
00239      PERFORM 9700-DATE-LINK.
00240      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.
00241      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.
00242
00243      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00244      MOVE 1                      TO EMI-NUMBER-OF-LINES.
00245
00246      IF EIBCALEN EQUAL 0
00247          GO TO 8800-UNAUTHORIZED-ACCESS.
00248
00249      IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM
00250          IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM
00251              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00252              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00253              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00254              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00255              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00256              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00257              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00258              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00259          ELSE
00260              MOVE PI-CALLING-PROGRAM   TO RETURN-FROM
00261              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00262              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00263              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00264              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00265              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00266              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00267              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00268              MOVE SPACES               TO PI-SAVED-PROGRAM-6
00269              PERFORM 0600-RECOVER-TEMP-STORAGE THRU 0699-EXIT
00270              GO TO 3000-SHOW-CLAIM-PAYMENT.
00271
00272      
      * EXEC CICS    HANDLE    CONDITION
00273 *         PGMIDERR          (9600-PGMID-ERROR)
00274 *         ERROR             (9990-ABEND)
00275 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00005437' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303035343337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00276
00277  0100-SEND-NEW.
00278      IF  EIBTRNID NOT EQUAL TRANS-ID
00279          MOVE LOW-VALUES         TO  EL143AI PI-LAST-ELACTQ-KEY
00280          MOVE 'Y'                TO  PI-FIRST-TIME-SW
00281          MOVE +0                 TO  PI-UNAPPROVED-COUNT
00282                                      PI-LAST-TRLR-SEQ-NO
00283                                      PI-ELTRLR-UPDATE-HHMMSS
00284          MOVE SPACES             TO  PI-ELTRLR-UPDATE-BY
00285                                      PI-DIAGNOSIS
00286          GO TO 8100-SEND-INITIAL-MAP.
00287
00288      IF EIBAID EQUAL DFHCLEAR
00289          GO TO 9400-CLEAR.
00290
00291      IF PI-PROCESSOR-ID EQUAL 'LGXX'
00292          GO TO 0200-RECEIVE.
00293
00294      
      * EXEC CICS  READQ TS
00295 *        QUEUE   (PI-SECURITY-TEMP-STORE-ID)
00296 *        INTO    (SECURITY-CONTROL)
00297 *        LENGTH  (SC-COMM-LENGTH)
00298 *        ITEM    (SC-ITEM)
00299 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00005459' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00300
00301      MOVE SC-CLAIMS-DISPLAY (28)  TO  PI-DISPLAY-CAP.
00302      MOVE SC-CLAIMS-UPDATE  (28)  TO  PI-MODIFY-CAP.
00303
00304      IF NOT DISPLAY-CAP
00305          MOVE 'READ'          TO SM-READ
00306          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
00307          MOVE ER-0070        TO  EMI-ERROR
00308          PERFORM 9900-ERROR-FORMAT
00309          GO TO 8100-SEND-INITIAL-MAP.
00310
00311  0200-RECEIVE.
00312
00313      IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3
00314         MOVE ER-7008            TO EMI-ERROR
00315         PERFORM 9900-ERROR-FORMAT
00316         MOVE -1                 TO MAINTL
00317         GO TO 8200-SEND-DATAONLY.
00318
00319      
      * EXEC CICS RECEIVE
00320 *        MAP      (MAP-NAME)
00321 *        MAPSET   (MAPSET-NAME)
00322 *        INTO     (EL143AI)
00323 *    END-EXEC.
           MOVE LENGTH OF
            EL143AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00005484' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL143AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00324
00325      IF  PFKEYL EQUAL +0
00326          GO TO 0300-CHECK-PFKEYS.
00327
00328      IF  EIBAID NOT EQUAL DFHENTER
00329          MOVE ER-0004            TO EMI-ERROR
00330          GO TO 0320-INPUT-ERROR.
00331
00332      IF PFKEYI NOT NUMERIC
00333         MOVE ER-0029     TO EMI-ERROR
00334         GO TO 0320-INPUT-ERROR.
00335
00336      IF PFKEYI GREATER THAN 01 AND
00337                   LESS THAN 25
00338         MOVE PF-VALUES (PFKEYI) TO EIBAID
00339      ELSE
00340         MOVE ER-0029        TO EMI-ERROR
00341         GO TO 0320-INPUT-ERROR.
00342
00343  0300-CHECK-PFKEYS.
00344      IF EIBAID EQUAL DFHPF23
00345          GO TO 8810-PF23.
00346
00347      IF EIBAID EQUAL DFHPF24
00348          GO TO 9200-RETURN-MAIN-MENU.
00349
00350      IF EIBAID EQUAL DFHPF12
00351          GO TO 9500-PF12.
00352
00353      IF EIBAID = DFHPF6 AND
00354          PI-LAST-ELACTQ-KEY  NOT EQUAL TO LOW-VALUES
00355          PERFORM 0500-CREATE-TEMP-STORAGE THRU 0599-EXIT
00356          MOVE XCTL-150               TO  PGM-NAME
00357          GO TO 9300-XCTL.
00358
00359      IF MAINTL GREATER THAN +0 AND
00360               EIBAID NOT EQUAL DFHENTER
00361         MOVE -1             TO  MAINTL
00362         MOVE  ER-0050       TO  EMI-ERROR
00363         PERFORM 9900-ERROR-FORMAT
00364         GO TO 8200-SEND-DATAONLY.
00365
00366      PERFORM 7600-READ-COMPANY-REC THRU 7600-EXIT.
00367      PERFORM 7700-READ-USER-REC    THRU 7700-EXIT.
00368
00369      IF  EIBAID EQUAL DFHPF1
00370          GO TO 7000-BROWSE-FWRD-NEXT-CLAIM.
00371
00372      IF  EIBAID EQUAL DFHPF2
00373          GO TO 7100-BROWSE-BWRD-NEXT-CLAIM.
00374
00375      IF  EIBAID EQUAL DFHPF3
00376          GO TO 7200-BROWSE-FWRD-NEXT-PAYMENT.
00377
00378      IF  EIBAID EQUAL DFHPF4
00379          GO TO 7300-BROWSE-BWRD-NEXT-PAYMENT.
00380
00381      IF EIBAID = DFHPF5
00382          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
00383
00384      IF EIBAID EQUAL DFHENTER
00385          GO TO 0400-EDIT-INPUT-DATA.
00386
00387      MOVE ER-0029                TO EMI-ERROR.
00388
00389  0320-INPUT-ERROR.
00390
00391      PERFORM 9900-ERROR-FORMAT.
00392      MOVE AL-UNBON               TO PFKEYA.
00393      IF PFKEYL EQUAL 0
00394          MOVE -1                 TO MAINTL
00395      ELSE
00396          MOVE -1                 TO PFKEYL.
00397
00398      GO TO 8200-SEND-DATAONLY.
00399
00400      EJECT
00401  0400-EDIT-INPUT-DATA.
00402
00403      IF MAINTI EQUAL 'S'
00404         GO TO 3000-SHOW-CLAIM-PAYMENT.
00405
00406      IF NOT  MODIFY-CAP
00407         MOVE 'UPDATE'           TO  SM-READ
00408         PERFORM 9995-SECURITY-VIOLATION  THRU  9995-EXIT
00409         MOVE ER-0070            TO  EMI-ERROR
00410         MOVE -1                 TO MAINTL
00411         PERFORM 9900-ERROR-FORMAT
00412         GO TO 8100-SEND-INITIAL-MAP.
00413
00414      IF MAINTI EQUAL 'A'
00415         GO TO 1000-APPROVE-PAYMENT.
00416
00417      IF MAINTI EQUAL 'V'
00418         GO TO 2000-VOID-PAYMENT.
00419
00420      MOVE  ER-0023            TO EMI-ERROR
00421      MOVE -1                  TO MAINTL
00422      MOVE AL-UABON            TO MAINTA
00423      PERFORM 9900-ERROR-FORMAT
00424      GO TO 8200-SEND-DATAONLY.
00425
00426      EJECT
00427  0500-CREATE-TEMP-STORAGE.
00428
00429      
      * EXEC CICS WRITEQ TS
00430 *        QUEUE    (WS-QID)
00431 *        FROM     (PROGRAM-INTERFACE-BLOCK)
00432 *        LENGTH   (PI-COMM-LENGTH)
00433 *    END-EXEC.
      *    MOVE '*"     L              ''   #00005594' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035353934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00434
00435  0599-EXIT.
00436       EXIT.
00437
00438      EJECT
00439  0600-RECOVER-TEMP-STORAGE.
00440
00441      MOVE LOW-VALUES                   TO EL143AI.
00442      
      * EXEC CICS HANDLE CONDITION
00443 *        NOTFND  (0100-SEND-NEW)
00444 *        QIDERR  (0100-SEND-NEW)
00445 *    END-EXEC.
      *    MOVE '"$IN                  ! # #00005607' TO DFHEIV0
           MOVE X'2224494E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035363037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00446
00447
00448      
      * EXEC CICS READQ TS
00449 *         QUEUE    (WS-QID)
00450 *         INTO    (PROGRAM-INTERFACE-BLOCK)
00451 *         LENGTH  (PI-COMM-LENGTH)
00452 *        END-EXEC.
      *    MOVE '*$I    L              ''   #00005613' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00453
00454      
      * EXEC CICS DELETEQ TS
00455 *         QUEUE    (WS-QID)
00456 *    END-EXEC.
      *    MOVE '*&                    #   #00005619' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00457
00458      MOVE PI-LAST-ELTRLR-KEY     TO ELACTQ-KEY.
00459      MOVE ELACTQ-CLAIM-NO        TO CLAIMO.
00460      MOVE ELACTQ-CARRIER         TO CARRO.
00461      MOVE ELACTQ-CERT-PRIME      TO CERTO.
00462      MOVE ELACTQ-CERT-SFX        TO SUFFIXO.
00463
00464  0699-EXIT.
00465      EXIT.
00466
00467      EJECT
00468  1000-APPROVE-PAYMENT.
00469
00470      MOVE CARRI         TO WS-HOLD-CARR.
00471      MOVE CLAIMI        TO WS-HOLD-CLAIM.
00472      MOVE CERTI         TO WS-HOLD-CERT-PRIME.
00473      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
00474      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
00475
00476      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
00477         MOVE ER-0138     TO EMI-ERROR
00478         MOVE -1          TO MAINTL
00479         MOVE AL-UNBON    TO MAINTA
00480         PERFORM 9900-ERROR-FORMAT
00481         GO TO 8200-SEND-DATAONLY.
00482
00483      MOVE PI-LAST-ELACTQ-KEY    TO ELACTQ-KEY.
00484
00485      
      * EXEC CICS HANDLE CONDITION
00486 *         NOTFND    (1010-NOT-FOUND)
00487 *         ENDFILE   (1010-NOT-FOUND)
00488 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005650' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035363530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00489
00490      
      * EXEC CICS READ
00491 *         DATASET    (ELACTQ-FILE-ID)
00492 *         RIDFLD     (ELACTQ-KEY)
00493 *         SET        (ADDRESS OF ACTIVITY-QUE)
00494 *         UPDATE
00495 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005655' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00496
00497      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC OR
00498         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
00499          MOVE -1                     TO  MAINTL
00500          MOVE  ER-0627               TO  EMI-ERROR
00501          PERFORM 9900-ERROR-FORMAT
00502          GO TO 8200-SEND-DATAONLY.
00503
00504      MOVE ELACTQ-KEY          TO ELTRLR-KEY.
00505      MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.
00506
00507      
      * EXEC CICS READ
00508 *         DATASET   (ELTRLR-FILE-ID)
00509 *         RIDFLD    (ELTRLR-KEY)
00510 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
00511 *         UPDATE
00512 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005672' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363732' TO DFHEIV0(25:11)
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
           
00513
00514      IF AT-PAYMENT-LAST-UPDATED-BY EQUAL PI-ELTRLR-UPDATE-BY AND
00515         AT-LAST-MAINT-HHMMSS EQUAL PI-ELTRLR-UPDATE-HHMMSS
00516         NEXT SENTENCE
00517      ELSE
00518         MOVE ER-0068        TO EMI-ERROR
00519         MOVE AL-UABON       TO MAINTA
00520         MOVE -1             TO MAINTL
00521         PERFORM 9900-ERROR-FORMAT
00522         GO TO 8200-SEND-DATAONLY.
00523
00524      IF (AT-TRAILER-TYPE NOT EQUAL '2')
00525             OR
00526         (AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U')
00527         GO TO 1010-NOT-FOUND.
00528
00529      IF (AT-RECORDED-BY EQUAL PI-PROCESSOR-ID)
00530       AND
00531         (PI-PROCESSOR-ID NOT EQUAL 'LGXX')
00532         MOVE ER-0629       TO EMI-ERROR
00533         MOVE AL-UABON      TO MAINTA
00534         MOVE -1            TO MAINTL
00535         PERFORM 9900-ERROR-FORMAT
00536         GO TO 8200-SEND-DATAONLY.
00537
00538      IF NOT WS-GRADUATED-APPROVAL
00539          GO TO 1005-APPROVE.
00540
031808* If user approval level > trailer, make equal.
031808* This eliminates the need for intermediate approvals.
031808     IF AT-APPROVED-LEVEL < WS-APPROVAL-LEVEL
031808        MOVE WS-APPROVAL-LEVEL TO AT-APPROVED-LEVEL
031808     END-IF.
031808
00541      IF AT-APPROVED-LEVEL = WS-APPROVAL-LEVEL
00542          NEXT SENTENCE
00543        ELSE
00544          MOVE ER-2779       TO EMI-ERROR
00545          MOVE AL-UABON      TO MAINTA
00546          MOVE -1            TO MAINTL
00547          PERFORM 9900-ERROR-FORMAT
00548          GO TO 8200-SEND-DATAONLY.
00549
00550      IF AT-APPROVED-LEVEL = '1'
00551          MOVE '2'             TO AT-APPROVED-LEVEL
00552       ELSE
00553      IF AT-APPROVED-LEVEL = '2'
00554          MOVE '3'             TO AT-APPROVED-LEVEL
00555       ELSE
00556      IF AT-APPROVED-LEVEL = '3'
031808*         MOVE '4'             TO AT-APPROVED-LEVEL.
031808         MOVE '4'             TO AT-APPROVED-LEVEL
031808      ELSE
031808     IF AT-APPROVED-LEVEL = '4'
091813         MOVE '5'             TO AT-APPROVED-LEVEL
091813      ELSE
091813     IF AT-APPROVED-LEVEL = '5'
091813         MOVE '6'             TO AT-APPROVED-LEVEL.
00558
00559      IF AT-APPROVED-LEVEL GREATER AT-APPROVAL-LEVEL-REQD
00560          GO TO 1005-APPROVE.
00561
00562      MOVE PI-PROCESSOR-ID  TO AT-PAYMENT-LAST-UPDATED-BY.
00563      MOVE SAVE-BIN-DATE    TO AT-PAYMENT-LAST-MAINT-DT.
00564      MOVE EIBTIME          TO AT-LAST-MAINT-HHMMSS.
00565
00566      
      * EXEC CICS REWRITE
00567 *        DATASET (ELTRLR-FILE-ID)
00568 *        FROM    (ACTIVITY-TRAILERS)
00569 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005744' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00570
00571      MOVE ER-3342          TO EMI-ERROR
00572
00573      GO TO 1006-INTERMEDIATE-APPROVAL.
00574
00575  1005-APPROVE.
00576      MOVE 'A'              TO AT-PAYMENT-APPROVAL-SW.
00577
00578      MOVE PI-PROCESSOR-ID  TO AT-PAYMENT-LAST-UPDATED-BY.
031808     MOVE PI-PROCESSOR-ID  TO AT-PMT-APPROVED-BY.
00579      MOVE SAVE-BIN-DATE    TO AT-PAYMENT-LAST-MAINT-DT.
00580      MOVE EIBTIME          TO AT-LAST-MAINT-HHMMSS.
00581
00582      
      * EXEC CICS REWRITE
00583 *         DATASET   (ELTRLR-FILE-ID)
00584 *         FROM      (ACTIVITY-TRAILERS)
00585 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005761' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00586
00587      IF AQ-PMT-UNAPPROVED-COUNT GREATER THAN +0
00588         SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT.
00589
00590      
      * EXEC CICS REWRITE
00591 *         DATASET   (ELACTQ-FILE-ID)
00592 *         FROM      (ACTIVITY-QUE)
00593 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005769' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00594
00595      SUBTRACT +1    FROM PI-UNAPPROVED-COUNT.
00596      MOVE PI-UNAPPROVED-COUNT    TO UCOUNTO.
00597      MOVE ER-3343                TO  EMI-ERROR.
00598
00599  1006-INTERMEDIATE-APPROVAL.
00600      MOVE -1                     TO MAINTL.
00601      MOVE SPACES                 TO MAINTO.
00602      MOVE AL-UANOF               TO MAINTA.
00603      PERFORM 9900-ERROR-FORMAT
00604      GO TO 8200-SEND-DATAONLY.
00605
00606      EJECT
00607  1010-NOT-FOUND.
00608
00609      MOVE  ER-0142               TO EMI-ERROR.
00610      MOVE AL-UNBON               TO CLAIMA
00611                                     CERTA
00612                                     CARRA.
00613      MOVE -1                     TO CARRL.
00614
00615      PERFORM 9900-ERROR-FORMAT.
00616      GO TO 8200-SEND-DATAONLY.
00617
00618      EJECT
00619  2000-VOID-PAYMENT.
00620
00621      MOVE CARRI         TO WS-HOLD-CARR.
00622      MOVE CLAIMI        TO WS-HOLD-CLAIM.
00623      MOVE CERTI         TO WS-HOLD-CERT-PRIME.
00624      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
00625      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
00626
00627      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
00628         MOVE ER-0138     TO EMI-ERROR
00629         MOVE -1          TO MAINTL
00630         MOVE AL-UNBON    TO MAINTA
00631         PERFORM 9900-ERROR-FORMAT
00632         GO TO 8200-SEND-DATAONLY.
00633
00634      MOVE PI-LAST-ELACTQ-KEY    TO ELACTQ-KEY.
00635
00636      
      * EXEC CICS HANDLE CONDITION
00637 *         NOTFND    (2095-NOT-FOUND)
00638 *         ENDFILE   (2095-NOT-FOUND)
00639 *    END-EXEC.
      *    MOVE '"$I''                  ! % #00005815' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303035383135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00640
00641      
      * EXEC CICS READ
00642 *         DATASET    (ELACTQ-FILE-ID)
00643 *         RIDFLD     (ELACTQ-KEY)
00644 *         SET        (ADDRESS OF ACTIVITY-QUE)
00645 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005820' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00646
00647      MOVE AQ-CONTROL-PRIMARY   TO ELMSTR-KEY.
00648
00649      
      * EXEC CICS READ
00650 *         DATASET    (ELMSTR-FILE-ID)
00651 *         RIDFLD     (ELMSTR-KEY)
00652 *         SET        (ADDRESS OF CLAIM-MASTER)
00653 *         UPDATE
00654 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005828' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383238' TO DFHEIV0(25:11)
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
           
00655
00656      MOVE CL-CONTROL-PRIMARY   TO ELTRLR-KEY.
00657      MOVE PI-LAST-TRLR-SEQ-NO  TO ELTRLR-SEQ-NO.
00658
00659      
      * EXEC CICS READ
00660 *         DATASET    (ELTRLR-FILE-ID)
00661 *         RIDFLD     (ELTRLR-KEY)
00662 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
00663 *         UPDATE
00664 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005838' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035383338' TO DFHEIV0(25:11)
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
           
00665
00666      IF AT-PAYMENT-LAST-UPDATED-BY EQUAL PI-ELTRLR-UPDATE-BY AND
00667         AT-LAST-MAINT-HHMMSS EQUAL PI-ELTRLR-UPDATE-HHMMSS
00668         NEXT SENTENCE
00669      ELSE
00670         MOVE ER-0068        TO EMI-ERROR
00671         MOVE AL-UABON       TO MAINTA
00672         MOVE -1             TO MAINTL
00673         PERFORM 9900-ERROR-FORMAT
00674         GO TO 8200-SEND-DATAONLY.
00675
00676      IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'
00677         GO TO 2095-NOT-FOUND.
00678
00679      IF (AT-RECORDED-BY EQUAL PI-PROCESSOR-ID)
00680       AND
00681         (PI-PROCESSOR-ID NOT EQUAL 'LGXX')
00682         MOVE ER-0629       TO EMI-ERROR
00683         MOVE AL-UABON      TO MAINTA
00684         MOVE -1            TO MAINTL
00685         PERFORM 9900-ERROR-FORMAT
00686         GO TO 8200-SEND-DATAONLY.
00687
00688      MOVE '7'                  TO PI-PAY-TYPE.
00689      MOVE AT-PAYMENT-TYPE      TO WS-PAY-TYPE.
00690      MOVE AT-AMOUNT-PAID       TO WS-AMOUNT-PAID.
00691      MOVE AT-CV-PMT-CODE       TO WS-CV-PMT-CODE.
00692
00693      IF (AT-PAYMENT-TYPE NOT EQUAL '5' AND '6')
00694          SUBTRACT AT-AMOUNT-PAID     FROM CL-TOTAL-PAID-AMT
00695          SUBTRACT AT-DAYS-IN-PERIOD  FROM CL-NO-OF-DAYS-PAID
00696          IF (AT-PAYMENT-TYPE NOT EQUAL '4')
00697             SUBTRACT +1                 FROM CL-NO-OF-PMTS-MADE
00698             IF AT-PAID-THRU-DT NOT EQUAL CL-PAID-THRU-DT OR
00699                AT-RECORDED-BY EQUAL 'ZZZZ'
00700                NEXT SENTENCE
00701             ELSE
00702                MOVE AT-PREV-LAST-PMT-DT    TO CL-LAST-PMT-DT
00703                MOVE AT-PREV-PAID-THRU-DT   TO CL-PAID-THRU-DT
00704                MOVE AT-PREV-LAST-PMT-AMT   TO CL-LAST-PMT-AMT.
00705
00706      IF CL-TOTAL-PAID-AMT LESS THAN +0
00707         MOVE +0                  TO CL-TOTAL-PAID-AMT.
00708
00709      IF CL-NO-OF-DAYS-PAID LESS THAN +0
00710         MOVE +0                  TO CL-NO-OF-DAYS-PAID.
00711
00712      IF CL-NO-OF-PMTS-MADE LESS THAN +0
00713         MOVE +0                  TO CL-NO-OF-PMTS-MADE.
00714
00715      MOVE SAVE-BIN-DATE          TO AT-VOID-DT
00716                                     CL-LAST-REOPEN-DT.
00717
00718      MOVE 'PAYMENT DISAPPROVED'  TO AT-VOID-REASON.
00719      MOVE 'V'                    TO AT-PAYMENT-APPROVAL-SW.
00720
00721      MOVE LOW-VALUES             TO AT-PMT-SELECT-DT
00722                                     AT-VOID-SELECT-DT.
00723
00724      MOVE PI-PROCESSOR-ID        TO AT-PAYMENT-LAST-UPDATED-BY.
00725      MOVE SAVE-BIN-DATE          TO AT-PAYMENT-LAST-MAINT-DT.
00726      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
00727
062121     IF PI-COMPANY-ID EQUAL 'CID' OR 'AHL' or 'FNL'
CIDMOD         PERFORM 9870-OUTPUT-ACTIVITY-RECORD THRU
CIDMOD                 9870-EXIT
CIDMOD         IF ERROR-ON-OUTPUT
CIDMOD             MOVE -1             TO PFKEYL
CIDMOD             MOVE AL-UANON       TO PFKEYA
CIDMOD*            MOVE MAP-NAMEA      TO MAP-NAME
CIDMOD             PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                     9995-EXIT
CIDMOD             GO TO 8200-SEND-DATAONLY
CIDMOD         END-IF
CIDMOD     END-IF.
           move at-amount-paid         to ws-at-amount-paid
           move at-days-in-period      to ws-at-days-in-period
           move at-payment-type        to ws-at-payment-type
00728      
      * EXEC CICS REWRITE
00729 *         DATASET  (ELTRLR-FILE-ID)
00730 *         FROM     (ACTIVITY-TRAILERS)
00731 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005922' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00732
00733  2010-UPDATE-ZERO-TRAILER.
00734
00735      MOVE CL-CONTROL-PRIMARY    TO ELTRLR-KEY.
00736      MOVE +0                    TO ELTRLR-SEQ-NO.
00737
00738      
      * EXEC CICS READ
00739 *         DATASET    (ELTRLR-FILE-ID)
00740 *         RIDFLD     (ELTRLR-KEY)
00741 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
00742 *         UPDATE
00743 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005932' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393332' TO DFHEIV0(25:11)
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
           
00744
00745      IF WS-PAY-TYPE EQUAL '5'
00746         SUBTRACT WS-AMOUNT-PAID  FROM AT-ITD-CHARGEABLE-EXPENSE.
00747
00748      IF WS-PAY-TYPE EQUAL '6'
00749         SUBTRACT WS-AMOUNT-PAID  FROM AT-ITD-PAID-EXPENSES.
00750
00751      IF AT-INITIAL-MANUAL-RESERVE NOT EQUAL +0
00752         ADD WS-AMOUNT-PAID       TO AT-CURRENT-MANUAL-RESERVE.
00753
00754  2010-CHECK-OPEN-CLOSE.
00755
00756      IF (PI-PAY-TYPE IS EQUAL TO '5' OR '6')
00757          GO TO 2010-REWRITE-ZERO-TRAILER.
00758
00759      IF (PI-PAY-TYPE IS EQUAL TO '1' OR '4' OR '7') AND
00760         CLAIM-IS-OPEN
00761          GO TO 2010-REWRITE-ZERO-TRAILER.
00762
00763      IF (PI-PAY-TYPE IS EQUAL TO '2' OR '3') AND
00764         CLAIM-IS-CLOSED
00765          GO TO 2010-REWRITE-ZERO-TRAILER.
00766
00767      MOVE +1                     TO  SUB-1.
00768
00769  2010-OPEN-CLOSE-LOOP.
00770
00771      IF AT-OPEN-CLOSE-TYPE (SUB-1) IS EQUAL TO SPACES
00772          MOVE SAVE-BIN-DATE      TO  AT-OPEN-CLOSE-DATE (SUB-1)
00773          IF (PI-PAY-TYPE IS EQUAL TO '1' OR '4' OR '7')
00774              MOVE 'O'            TO  AT-OPEN-CLOSE-TYPE (SUB-1)
00775              MOVE 'FORCE'        TO  AT-OPEN-CLOSE-REASON (SUB-1)
00776              GO TO 2010-REWRITE-ZERO-TRAILER
00777          ELSE
00778              MOVE 'C'            TO  AT-OPEN-CLOSE-TYPE   (SUB-1)
00779              MOVE 'FINAL'        TO  AT-OPEN-CLOSE-REASON (SUB-1)
00780              GO TO 2010-REWRITE-ZERO-TRAILER.
00781
00782      IF SUB-1 IS EQUAL TO 6
00783        MOVE AT-OPEN-CLOSE-HISTORY (2) TO AT-OPEN-CLOSE-HISTORY (1)
00784        MOVE AT-OPEN-CLOSE-HISTORY (3) TO AT-OPEN-CLOSE-HISTORY (2)
00785        MOVE AT-OPEN-CLOSE-HISTORY (4) TO AT-OPEN-CLOSE-HISTORY (3)
00786        MOVE AT-OPEN-CLOSE-HISTORY (5) TO AT-OPEN-CLOSE-HISTORY (4)
00787        MOVE AT-OPEN-CLOSE-HISTORY (6) TO AT-OPEN-CLOSE-HISTORY (5)
00788        MOVE SPACES                    TO AT-OPEN-CLOSE-HISTORY (6)
00789        GO TO 2010-OPEN-CLOSE-LOOP.
00790
00791      ADD +1                      TO  SUB-1.
00792      GO TO 2010-OPEN-CLOSE-LOOP.
00793
00794  2010-REWRITE-ZERO-TRAILER.
00795
00796      MOVE PI-PROCESSOR-ID        TO AT-RESERVES-LAST-UPDATED-BY.
00797      MOVE SAVE-BIN-DATE          TO AT-RESERVES-LAST-MAINT-DT.
00798      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS.
00799
00800      
      * EXEC CICS REWRITE
00801 *         DATASET  (ELTRLR-FILE-ID)
00802 *         FROM     (ACTIVITY-TRAILERS)
00803 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005994' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00804
00805  2020-UPDATE-ELCERT.
00806
00807      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
00808          GO TO 2050-UPDATE-EMPLCY.
00809
00810      MOVE PI-COMPANY-CD    TO ELCERT-COMPANY-CD.
00811      MOVE CL-CERT-CARRIER  TO ELCERT-CARRIER.
00812      MOVE CL-CERT-GROUPING TO ELCERT-GROUP.
00813      MOVE CL-CERT-STATE    TO ELCERT-STATE.
00814      MOVE CL-CERT-ACCOUNT  TO ELCERT-ACCOUNT.
00815      MOVE CL-CERT-EFF-DT   TO ELCERT-EFF-DATE.
00816      MOVE CL-CERT-NO       TO ELCERT-CERT-NO.
00817
00818      
      * EXEC CICS READ
00819 *         DATASET     (ELCERT-FILE-ID)
00820 *         RIDFLD      (ELCERT-KEY)
00821 *         SET         (ADDRESS OF CERTIFICATE-MASTER)
00822 *         UPDATE
00823 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006012' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00824
           move cm-ah-benefit-amt      to ws-cm-ah-benefit-amt
           move cm-ah-orig-term        to ws-cm-ah-orig-term
100518     IF CL-CLAIM-TYPE NOT EQUAL PI-LIFE-OVERRIDE-L1 AND 'O'
00826         GO TO 2020-AH-VOID.
00827
00828      MOVE CM-LF-BENEFIT-CD       TO  WS-BEN-CD.
00829      MOVE WS-ACCESS              TO  ELCNTL-ACCESS.
00830      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
00831      MOVE '4'                    TO  ELCNTL-REC-TYPE.
00832      MOVE ZEROS                  TO  ELCNTL-SEQ-NO.
00833      PERFORM 7500-FIND-BENEFIT THRU 7500-EXIT.
00834      IF NO-BENEFIT-FOUND
00835          GO TO 8500-NOT-FOUND.
00836      MOVE CF-LF-COVERAGE-TYPE (SUB)  TO  WS-LF-COVERAGE-TYPE.
00837
00838      IF PI-LIFE-OVERRIDE-L1 IS EQUAL TO 'P' OR
00839         WS-LF-COVERAGE-TYPE IS EQUAL TO 'P'
00840          IF WS-PAY-TYPE IS EQUAL TO '4'
00841              SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
00842              IF CM-LF-CURRENT-STATUS IS EQUAL TO '1' OR '2'
00843                  GO TO 2020-REWRITE-CERT
00844              ELSE
00845                  MOVE CM-LF-STATUS-AT-DEATH
00846                                   TO CM-LF-CURRENT-STATUS
00847                  MOVE SPACES      TO CM-LF-STATUS-AT-DEATH
00848                  MOVE LOW-VALUES  TO CM-LF-DEATH-EXIT-DT
00849                                      CM-LF-DEATH-DT
00850                  GO TO 2020-REWRITE-CERT.
00851
00852      IF WS-PAY-TYPE EQUAL '4'
00853         SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
00854         GO TO 2020-REWRITE-CERT.
00855
00856      IF WS-PAY-TYPE EQUAL '2'
00857         IF CM-LF-CURRENT-STATUS IS EQUAL TO '1' OR '2'
00858             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
00859             GO TO 2020-REWRITE-CERT
00860         ELSE
00861             MOVE CM-LF-STATUS-AT-DEATH TO CM-LF-CURRENT-STATUS
00862             MOVE SPACES        TO CM-LF-STATUS-AT-DEATH
00863             SUBTRACT WS-AMOUNT-PAID FROM CM-LF-ITD-DEATH-AMT
00864             MOVE LOW-VALUES  TO CM-LF-DEATH-EXIT-DT CM-LF-DEATH-DT
00865             GO TO 2020-REWRITE-CERT
00866      ELSE
00867          GO TO 2030-UNLOCK-CERT.
00868
00869  2020-AH-VOID.
00870
00871      IF WS-PAY-TYPE EQUAL '4'
00872         SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
00873         GO TO 2020-REWRITE-CERT.
00874
00875      IF WS-PAY-TYPE EQUAL '3'
00876         MOVE CM-AH-STATUS-AT-SETTLEMENT TO CM-AH-CURRENT-STATUS
00877         MOVE SPACES        TO CM-AH-STATUS-AT-SETTLEMENT
00878         SUBTRACT WS-AMOUNT-PAID FROM CM-AH-ITD-LUMP-PMT
00879         MOVE LOW-VALUES TO CM-AH-SETTLEMENT-DT
00880                            CM-AH-SETTLEMENT-EXIT-DT
00881         GO TO 2020-REWRITE-CERT
00882      ELSE
00883         GO TO 2030-UNLOCK-CERT.
00884
00885  2020-REWRITE-CERT.
00886
00887      
      * EXEC CICS REWRITE
00888 *         DATASET  (ELCERT-FILE-ID)
00889 *         FROM     (CERTIFICATE-MASTER)
00890 *    END-EXEC.
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006083' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00891
00892      GO TO 2090-UPDATE-ELACTQ.
00893
00894  2030-UNLOCK-CERT.
00895
00896      
      * EXEC CICS UNLOCK
00897 *         DATASET  (ELCERT-FILE-ID)
00898 *    END-EXEC.
      *    MOVE '&*                    #   #00006092' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELCERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00899
00900      GO TO 2090-UPDATE-ELACTQ.
00901
00902      EJECT
00903  2050-UPDATE-EMPLCY.
00904
00905      MOVE PI-COMPANY-CD          TO  EMPLCY-COMPANY-CD.
00906      MOVE CL-CERT-CARRIER        TO  EMPLCY-CARRIER.
00907      MOVE CL-CERT-GROUPING       TO  EMPLCY-GROUPING.
00908      MOVE CL-CERT-STATE          TO  EMPLCY-STATE.
00909      MOVE CL-CERT-ACCOUNT        TO  EMPLCY-PRODUCER.
00910      MOVE CL-CERT-EFF-DT         TO  EMPLCY-EFF-DATE.
00911      MOVE CL-CV-REFERENCE-NO     TO  EMPLCY-REFERENCE-NO.
00912
00913      
      * EXEC CICS READ
00914 *        DATASET   (EMPLCY-FILE-ID)
00915 *        RIDFLD    (EMPLCY-KEY)
00916 *        SET       (ADDRESS OF POLICY-MASTER)
00917 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006109' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 EMPLCY-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 EMPLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00918
00919      MOVE LOW-VALUES             TO WS-POLICY-UPDATE-WORKING-GRPS.
00920      MOVE PM-COMPANY-CD          TO  WS-COMPANY-CD.
00921      MOVE PM-CARRIER             TO  WS-CARRIER.
00922      MOVE PM-GROUPING            TO  WS-GROUPING.
00923      MOVE PM-STATE               TO  WS-STATE.
00924      MOVE PM-PRODUCER            TO  WS-PRODUCER.
00925      MOVE PM-POLICY-EFF-DT       TO  WS-POLICY-EFF-DT.
00926      MOVE PM-REFERENCE-NUMBER    TO  WS-REFERENCE-NUMBER.
00927
00928      MOVE 'RW'                   TO  WS-EMPLCY-FUNCTION.
00929      MOVE PI-PROCESSOR-ID        TO  WS-LAST-CHANGE-PROCESSOR.
00930      MOVE SAVE-BIN-DATE          TO  WS-LAST-CHANGE-DT.
00931      MOVE EIBTIME                TO  WS-LAST-CHANGE-TIME.
00932
080322     IF CL-CLAIM-TYPE = 'A' OR 'I' OR 'G' OR 'F' OR 'B' OR 'H'
00934          GO TO 2050-UPDATE-AH-POLICY-DATA.
00935
00936  2050-UPDATE-LF-POLICY-DATA.
00937
00938      IF WS-PAY-TYPE IS EQUAL TO '2'
00939 *** FROM AT-PAYMENT-TYPE - TYPES 1-6 IN CONVENIENCE VALUES
00940        IF (WS-CV-PMT-CODE IS EQUAL TO '1' OR '2' OR '3' OR '4')
00941 *** LIFE / HALF LIFE / ADD / HALF ADD - CONV VALUES
00942          COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
00943                                          WS-AMOUNT-PAID
00944          COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -
00945                                      WS-AMOUNT-PAID
00946          COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
00947
00948      IF WS-PAY-TYPE IS EQUAL TO '2'
00949        IF (WS-CV-PMT-CODE IS EQUAL TO '5' OR '6')
00950 *** RIDER AND HALF RIDER - CONV VALUES
00951          COMPUTE WS-CLAIM-RIDER-ITD = PM-CLAIM-RIDER-ITD -
00952                                       WS-AMOUNT-PAID.
00953
00954      IF WS-PAY-TYPE IS EQUAL TO '4'
00955 *** ADDITIONAL PAYMENT - TYPE 8 CONV VALUES
00956          COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
00957                                          WS-AMOUNT-PAID
00958          COMPUTE WS-CLAIM-LIFE-ITD = PM-CLAIM-LIFE-ITD -
00959                                      WS-AMOUNT-PAID
00960          COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
00961
00962      IF WS-PAY-TYPE = '6'
00963 *** NON CHARGEABLE EXPENSE - TYPE 7 IN CONVENIENCE
00964          COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -
00965                                          WS-AMOUNT-PAID.
00966
00967      IF  PM-CLAIM-SETTLEMENT
00968
00969          IF  WS-CV-PMT-CODE = '1' OR '2' OR '3' OR '4'
00970              MOVE '6'            TO WS-CURRENT-STATUS
00971
00972              IF  PM-EXIT-DT GREATER THAN LOW-VALUES
00973                  MOVE HIGH-VALUES
00974                                  TO WS-EXIT-DT
00975              END-IF.
00976
00977      GO TO 2050-UPDATE-CLAIM-HISTORY.
00978
00979  2050-UPDATE-AH-POLICY-DATA.
00980
00981      IF (WS-PAY-TYPE IS EQUAL TO '1' OR '4')
00982          COMPUTE WS-CLAIM-PAYMENTS-ITD = PM-CLAIM-PAYMENTS-ITD -
00983                                          WS-AMOUNT-PAID
00984          COMPUTE WS-CLAIM-AH-ITD = PM-CLAIM-AH-ITD -
00985                                    WS-AMOUNT-PAID
00986          COMPUTE WS-CLAIM-PAYMENT-CNT = PM-CLAIM-PAYMENT-CNT - 1.
00987
00988      IF WS-PAY-TYPE = '6'
00989          COMPUTE WS-CLAIM-EXPENSES-ITD = PM-CLAIM-EXPENSES-ITD -
00990                                          WS-AMOUNT-PAID.
00991
00992      IF  PM-CLAIM-SETTLEMENT
00993
00994          IF  WS-CV-PMT-CODE = '2'
00995              MOVE '6'            TO WS-CURRENT-STATUS
00996
00997          ELSE
00998              IF  WS-CV-PMT-CODE = '1'
00999                      AND
01000                  WS-CLAIM-PAYMENTS-ITD LESS THAN
01001                      PM-INS-TOTAL-BENEFIT
01002                  MOVE '6'        TO WS-CURRENT-STATUS.
01003
01004  2050-UPDATE-CLAIM-HISTORY.
01005
01006      IF PM-CLAIM-ATTACH-CNT IS EQUAL TO +1 OR
01007         PM-CLAIM-INCURRED-DT IS EQUAL TO CL-INCURRED-DT
01008          NEXT SENTENCE
01009      ELSE
01010          GO TO 2050-FINISH-POLICY-UPDATE.
01011
01012      IF (PM-CLAIM-ATTACH-CNT IS EQUAL TO +1) AND
01013         (CL-NO-OF-PMTS-MADE IS EQUAL TO +0)
01014          OR
01015         (PM-CLAIM-PAYMENT-CNT IS EQUAL TO +0)
01016          MOVE +0                 TO  WS-CLAIM-LAST-PAYMENT-AMT
01017          MOVE '1'                TO  WS-CLAIM-INTERFACE-SW
01018          MOVE HIGH-VALUES        TO  WS-CLAIM-INCURRED-DT
01019                                      WS-CLAIM-PAID-TO-DT
01020      ELSE
01021          MOVE CL-PAID-THRU-DT    TO  WS-CLAIM-PAID-TO-DT
01022          MOVE CL-LAST-PMT-AMT    TO  WS-CLAIM-LAST-PAYMENT-AMT.
01023
01024  2050-FINISH-POLICY-UPDATE.
01025
01026      IF WS-CLAIM-PAYMENT-CNT IS NEGATIVE
01027          MOVE +0                 TO  WS-CLAIM-PAYMENT-CNT.
01028
01029      
      * EXEC CICS LINK
01030 *        PROGRAM    ('EMPLCY')
01031 *        COMMAREA   (WS-POLICY-MASTER-UPDATE-AREA)
01032 *        LENGTH     (WS-PM-COMM-LNGTH)
01033 *    END-EXEC.
           MOVE 'EMPLCY' TO DFHEIV1
      *    MOVE '."C                   (   #00006225' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-POLICY-MASTER-UPDATE-AREA, 
                 WS-PM-COMM-LNGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01034
01035      IF WS-EMPLCY-RETURN-CODE IS EQUAL TO LOW-VALUES
01036          NEXT SENTENCE
01037      ELSE
01038          MOVE ER-9211            TO  EMI-ERROR
01039          MOVE -1                 TO  MAINTO
01040          MOVE AL-UABON           TO  MAINTA
01041          PERFORM 9900-ERROR-FORMAT
01042          
      * EXEC CICS SYNCPOINT
01043 *            ROLLBACK
01044 *        END-EXEC
      *    MOVE '6"R                   !   #00006238' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01045          GO TO 8200-SEND-DATAONLY.
01046
01047  2090-UPDATE-ELACTQ.
01048
01049      
      * EXEC CICS READ
01050 *         DATASET    (ELACTQ-FILE-ID)
01051 *         RIDFLD     (ELACTQ-KEY)
01052 *         SET        (ADDRESS OF ACTIVITY-QUE)
01053 *         UPDATE
01054 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00006245' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01055
01056      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC
01057          MOVE +0                 TO AQ-PAYMENT-COUNTER.
01058
01059      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC
01060          MOVE +0                 TO  AQ-PMT-UNAPPROVED-COUNT.
01061
01062      IF AQ-PMT-UNAPPROVED-COUNT GREATER THAN +0
01063         SUBTRACT +1 FROM AQ-PMT-UNAPPROVED-COUNT.
01064
01065      SUBTRACT +1    FROM PI-UNAPPROVED-COUNT.
01066      MOVE PI-UNAPPROVED-COUNT    TO UCOUNTO.
01067
01068      IF AQ-PAYMENT-COUNTER IS GREATER THAN +0
01069          SUBTRACT +1 FROM AQ-PAYMENT-COUNTER.
01070
01071      IF AQ-PAYMENT-COUNTER IS EQUAL TO +0
01072          MOVE SPACE               TO AQ-PENDING-PAYMENT-FLAG.
01073
01074      IF AQ-PENDING-ACTIVITY-FLAGS IS EQUAL TO SPACES
01075          
      * EXEC CICS DELETE
01076 *            DATASET   (ELACTQ-FILE-ID)
01077 *        END-EXEC
      *    MOVE '&(                    &   #00006271' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01078      ELSE
01079          
      * EXEC CICS REWRITE
01080 *            DATASET  (ELACTQ-FILE-ID)
01081 *            FROM     (ACTIVITY-QUE)
01082 *        END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-QUE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006275' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ACTIVITY-QUE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01083
01084  2090-REWRITE-ELMSTR.
01085
01086      IF (WS-PAY-TYPE IS EQUAL TO '4' OR '5' OR '6')
01087          GO TO 2092-CONT-REWRITE.
01088
01089      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
01090          IF CL-NO-OF-PMTS-MADE IS GREATER THAN +0
01091              GO TO 2092-CONT-REWRITE.
01092
01093      MOVE 'O'                    TO  CL-CLAIM-STATUS.
01094
01095  2092-CONT-REWRITE.
01096
           if ws-at-payment-type not = '5' and '6' and 'I'
              perform 2100-upd-cert-trlr thru 2100-exit
           end-if
01097      
      * EXEC CICS REWRITE
01098 *         DATASET  (ELMSTR-FILE-ID)
01099 *         FROM     (CLAIM-MASTER)
01100 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00006296' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELMSTR-FILE-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01101
01102      MOVE ZEROS                  TO EMI-ERROR
01103      MOVE -1                     TO MAINTL.
01104      MOVE SPACES                 TO MAINTO.
01105      MOVE AL-UANOF               TO MAINTA.
01106      PERFORM 9900-ERROR-FORMAT
01107      GO TO 8200-SEND-DATAONLY.
01108
01109  2095-NOT-FOUND.
01110
01111      MOVE  ER-0142               TO EMI-ERROR.
01112      MOVE AL-UNBON               TO CLAIMA
01113                                     CERTA.
01114      MOVE -1                     TO CARRL.
01115      MOVE AL-UNBON               TO CARRA.
01116
01117      PERFORM 9900-ERROR-FORMAT.
01118      GO TO 8200-SEND-DATAONLY.
061013 2100-UPD-CERT-TRLR.
061013
061013     MOVE CL-COMPANY-CD          TO CTRLR-COMP-CD
061013     MOVE CL-CERT-KEY-DATA       TO ELCRTT-KEY (2:21)
061013     MOVE CL-CERT-NO             TO CTRLR-CERT-NO
061013     MOVE 'B'                    TO CTRLR-REC-TYPE
061013
061013     
      * EXEC CICS READ
061013*       UPDATE
061013*       DATASET   ('ELCRTT')
061013*       RIDFLD    (ELCRTT-KEY)
061013*       set       (address of CERTIFICATE-TRAILERS)
061013*       RESP      (WS-RESPONSE)
061013*    END-EXEC
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&"S        EU         (  N#00006325' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCRTT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-TRAILERS TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013
061013     IF RESP-NORMAL
              perform varying s1 from +1 by +1 until
                 (s1 > +24)
                 or (cl-claim-no = cs-claim-no (s1))
              end-perform
           if s1 < +25
061013        subtract ws-at-amount-paid from cs-total-paid (s1)
061013        if cs-total-paid (s1) < zeros
061013           move zeros            to cs-total-paid (s1)
061013        end-if
061013        subtract at-days-in-period from cs-days-paid (s1)
061013        if cs-days-paid (s1) < zeros
061013           move zeros            to cs-days-paid (s1)
061013        end-if
              if cl-claim-type not = 'L' and 'P'
                 perform 2110-calc-rem-bens
                                       thru 2110-exit
              end-if
061013        
      * exec cics rewrite
061013*          dataset    ('ELCRTT')
061013*          from       (certificate-trailers)
061013*          resp       (ws-response)
061013*       end-exec
           MOVE LENGTH OF
            certificate-trailers
             TO DFHEIV11
           MOVE 'ELCRTT' TO DFHEIV1
      *    MOVE '&& L                  %  N#00006351' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303036333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 certificate-trailers, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
061013     end-if
061013     .
061013 2100-EXIT.
061013     EXIT.
       2110-calc-rem-bens.
           move cm-ah-orig-term        to ws-max-bens
           if cl-critical-period not = zeros and spaces
              move cl-critical-period  to ws-max-bens
           end-if
           move zeros to ws-tot-days-paid ws-tot-amt-paid
           perform varying s2 from +1 by +1 until
              (s2 > +24)
              or (cs-claim-no (s2) = spaces)
              if (cs-benefit-period (s2) = cl-benefit-period)
                 and (cs-insured-type (s2) = cl-insured-type)
                 and (cs-claim-type (s2) = cl-claim-type)
                 compute ws-tot-days-paid =
                    ws-tot-days-paid + cs-days-paid (s2)
                 compute ws-tot-amt-paid =
                    ws-tot-amt-paid + cs-total-paid (s2)
              end-if
           end-perform
           compute cs-remaining-bens (s1) =
              ws-max-bens / cm-ah-benefit-amt
           if cs-remaining-bens (s1) < zeros
              move zeros            to cs-remaining-bens (s1)
           end-if
           .
       2110-exit.
           exit.
01121  3000-SHOW-CLAIM-PAYMENT.
01122
01123      IF MAINTI EQUAL 'S'
01124         IF CLAIML GREATER THAN +0 AND
01125            CERTL  GREATER THAN +0
01126            NEXT SENTENCE
01127         ELSE
01128            MOVE ER-0005     TO EMI-ERROR
01129            MOVE -1          TO MAINTL
01130            MOVE AL-UNBON    TO MAINTA
01131            PERFORM 9900-ERROR-FORMAT
01132            GO TO 8200-SEND-DATAONLY.
01133
01134      IF MAINTI EQUAL 'S'
01135         MOVE SPACES              TO ELACTQ-KEY
01136         MOVE PI-COMPANY-CD       TO ELACTQ-COMPANY-CD
01137         MOVE CARRI               TO ELACTQ-CARRIER
01138         MOVE CLAIMI              TO ELACTQ-CLAIM-NO
01139         MOVE CERTI               TO ELACTQ-CERT-PRIME
01140         MOVE SUFFIXI             TO ELACTQ-CERT-SFX.
01141
01142      
      * EXEC CICS HANDLE CONDITION
01143 *         NOTFND     (3100-NOT-FOUND)
01144 *         ENDFILE    (3100-NOT-FOUND)
01145 *    END-EXEC.
      *    MOVE '"$I''                  ! & #00006407' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303036343037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01146
01147      
      * EXEC CICS READ
01148 *         DATASET     (ELACTQ-FILE-ID)
01149 *         RIDFLD      (ELACTQ-KEY)
01150 *         SET         (ADDRESS OF ACTIVITY-QUE)
01151 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006412' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01152
01153      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
01154         AQ-PAYMENT-COUNTER IS EQUAL TO +0
01155          GO TO 3100-NOT-FOUND.
01156
01157      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC  OR
01158         AQ-PMT-UNAPPROVED-COUNT EQUAL +0
01159           GO TO 3100-NOT-FOUND.
01160
01161      MOVE AQ-CONTROL-PRIMARY           TO PI-LAST-ELACTQ-KEY
01162                                           ELMSTR-KEY
01163                                           ELTRLR-KEY.
01164
01165      IF MAINTI EQUAL 'S'
01166         MOVE +90                          TO ELTRLR-SEQ-NO
01167         MOVE SPACES                       TO  PI-DIAGNOSIS
01168      ELSE
01169         MOVE PI-LAST-TRLR-SEQ-NO          TO ELTRLR-SEQ-NO.
01170
01171      
      * EXEC CICS READ
01172 *         DATASET    (ELMSTR-FILE-ID)
01173 *         RIDFLD     (ELMSTR-KEY)
01174 *         SET        (ADDRESS OF CLAIM-MASTER)
01175 *    END-EXEC.
      *    MOVE '&"S        E          (   #00006436' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343336' TO DFHEIV0(25:11)
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
           
01176
01177      MOVE CL-CARRIER                 TO  PI-CARRIER.
01178      MOVE CL-CLAIM-NO                TO  PI-CLAIM-NO.
01179      MOVE CL-CERT-NO                 TO  PI-CERT-NO.
01180      MOVE CL-CERT-GROUPING           TO  PI-GROUPING.
01181      MOVE CL-CERT-STATE              TO  PI-STATE.
01182      MOVE CL-CERT-ACCOUNT            TO  PI-ACCOUNT.
01183      MOVE CL-CERT-EFF-DT             TO  PI-CERT-EFF-DT.
01184
01185
01186  3010-READ-NEXT-ELTRLR.
01187
01188      
      * EXEC CICS READ
01189 *         DATASET    (ELTRLR-FILE-ID)
01190 *         RIDFLD     (ELTRLR-KEY)
01191 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
01192 *         GTEQ
01193 *    END-EXEC.
      *    MOVE '&"S        G          (   #00006453' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036343533' TO DFHEIV0(25:11)
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
           
01194
01195      MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.
01196      IF WS-HOLD-ELTRLR-KEY NOT EQUAL CL-CONTROL-PRIMARY
01197         GO TO 3100-NOT-FOUND.
01198
01199      IF AT-TRAILER-TYPE EQUAL '6'
01200         IF AT-SEQUENCE-NO EQUAL +90
01201            MOVE AT-INFO-LINE-1   TO  PI-DIAGNOSIS.
01202
01203      IF (AT-TRAILER-TYPE NOT EQUAL '2')
01204              OR
01205         (AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U')
01206         MOVE AT-CONTROL-PRIMARY TO ELTRLR-KEY
01207         ADD +1 TO ELTRLR-SEQ-NO
01208         GO TO 3010-READ-NEXT-ELTRLR.
01209
01210      MOVE LOW-VALUES                   TO EL143AI.
01211
01212      MOVE PI-DIAGNOSIS          TO DIAGNO.
01213      MOVE CL-CARRIER            TO CARRO.
01214      MOVE CL-CLAIM-NO           TO CLAIMO.
01215      MOVE CL-CERT-PRIME         TO CERTO.
01216      MOVE CL-CERT-SFX           TO SUFFIXO.
01217
01218      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC
01219          MOVE +0                       TO  PI-UNAPPROVED-COUNT
01220                                            UCOUNTO
01221      ELSE
01222          MOVE AQ-PMT-UNAPPROVED-COUNT  TO  PI-UNAPPROVED-COUNT
01223                                            UCOUNTO.
01224
121802     EVALUATE TRUE
121802
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802        MOVE PI-AH-OVERRIDE-L6   TO TYPEO
121802
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802        MOVE PI-LIFE-OVERRIDE-L6 TO TYPEO
121802
121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE '  IU  '            TO TYPEO
121802
121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO TYPEO
052614
052614     WHEN CL-CLAIM-TYPE = 'F'
052614        MOVE ' FAM  '            TO TYPEO
080322     WHEN CL-CLAIM-TYPE = 'B'
080322        MOVE ' BRV  '            TO TYPEO
080322     WHEN CL-CLAIM-TYPE = 'H'
080322        MOVE ' HSP '             TO TYPEO
080322
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO TYPEO
121802
121802     END-EVALUATE
01230      IF CL-CLAIM-STATUS  EQUAL 'O'
01231         MOVE ' OPEN '           TO STATUSO
01232      ELSE
01233         MOVE 'CLOSED'           TO STATUSO.
01234
01235      MOVE CL-INCURRED-DT        TO DC-BIN-DATE-1.
01236      MOVE ' '                   TO DC-OPTION-CODE.
01237      PERFORM 9700-DATE-LINK.
01238      IF NO-CONVERSION-ERROR
01239         MOVE DC-GREG-DATE-1-EDIT     TO INCDTEO.
01240
01241      MOVE CL-REPORTED-DT        TO DC-BIN-DATE-1.
01242      MOVE ' '                   TO DC-OPTION-CODE.
01243      PERFORM 9700-DATE-LINK.
01244      IF NO-CONVERSION-ERROR
01245         MOVE DC-GREG-DATE-1-EDIT     TO REPDTEO.
01246
01247      MOVE CL-PAID-THRU-DT       TO DC-BIN-DATE-1.
01248      MOVE ' '                   TO DC-OPTION-CODE.
01249      PERFORM 9700-DATE-LINK.
01250
01251      IF NO-CONVERSION-ERROR
01252         MOVE DC-GREG-DATE-1-EDIT     TO PAYTHRO
01253         IF PI-USES-PAID-TO
01254            MOVE CL-PAID-THRU-DT      TO DC-BIN-DATE-1
01255            MOVE '6'                  TO DC-OPTION-CODE
01256            MOVE +1                   TO DC-ELAPSED-DAYS
01257            MOVE +0                   TO DC-ELAPSED-MONTHS
01258            PERFORM 9700-DATE-LINK
01259            IF NO-CONVERSION-ERROR
01260               MOVE DC-GREG-DATE-1-EDIT     TO PAYTHRO.
01261
01262      MOVE CL-LAST-PMT-DT        TO DC-BIN-DATE-1.
01263      MOVE ' '                   TO DC-OPTION-CODE.
01264      PERFORM 9700-DATE-LINK
01265      IF NO-CONVERSION-ERROR
01266         MOVE DC-GREG-DATE-1-EDIT     TO LSTPAIDO.
01267
01268      IF CL-NO-OF-PMTS-MADE NUMERIC
01269         MOVE CL-NO-OF-PMTS-MADE      TO NOPMTSO
01270      ELSE
01271         MOVE ZEROS                   TO NOPMTSO.
01272
01273      MOVE CL-NO-OF-DAYS-PAID         TO DAYPAIDO.
01274
01275      MOVE CL-INSURED-LAST-NAME       TO LNAMEO.
01276      MOVE CL-PROCESSOR-ID            TO PROCO.
01277
01278      IF AT-PAYMENT-TYPE EQUAL '1'
01279         MOVE 'PARTIAL'               TO PMTTYPO
01280      ELSE
01281      IF AT-PAYMENT-TYPE EQUAL '2'
01282         MOVE 'FINAL'                 TO PMTTYPO
01283      ELSE
01284      IF AT-PAYMENT-TYPE EQUAL '3'
01285         MOVE 'SETTLEMENT'            TO PMTTYPO
01286      ELSE
01287      IF AT-PAYMENT-TYPE EQUAL '4'
01288         MOVE 'ADDITIONAL'            TO PMTTYPO
01289      ELSE
01290      IF AT-PAYMENT-TYPE EQUAL '5'
01291         MOVE 'CHG EXP'               TO PMTTYPO
01292      ELSE
01293      IF AT-PAYMENT-TYPE EQUAL '6'
01294         MOVE 'N-CHG EXP'             TO PMTTYPO
01295      ELSE
01296         MOVE AT-PAYMENT-TYPE         TO PMTTYPO.
01297
01298      MOVE AT-CHECK-NO                TO CHECKO.
01299      MOVE AT-AMOUNT-PAID             TO AMTPDO.
01300
01301      MOVE AT-PMT-SELECT-DT           TO DC-BIN-DATE-1.
01302      MOVE ' '                        TO DC-OPTION-CODE.
01303      PERFORM 9700-DATE-LINK.
01304      IF NO-CONVERSION-ERROR
01305         MOVE DC-GREG-DATE-1-EDIT     TO SELDTEO.
01306
01307      MOVE AT-PAID-FROM-DT            TO DC-BIN-DATE-1.
01308      MOVE ' '                        TO DC-OPTION-CODE.
01309      PERFORM 9700-DATE-LINK.
01310      IF NO-CONVERSION-ERROR
01311         MOVE DC-GREG-DATE-1-EDIT     TO PDFROMO.
01312
01313      MOVE AT-PAID-THRU-DT            TO DC-BIN-DATE-1.
01314      MOVE ' '                        TO DC-OPTION-CODE.
01315      PERFORM 9700-DATE-LINK.
01316      IF NO-CONVERSION-ERROR
01317         MOVE DC-GREG-DATE-1-EDIT     TO PDTHRUO
01318         IF PI-USES-PAID-TO
01319            MOVE AT-PAID-THRU-DT            TO DC-BIN-DATE-1
01320            MOVE '6'                        TO DC-OPTION-CODE
01321            MOVE +1                         TO DC-ELAPSED-DAYS
01322            MOVE +0                         TO DC-ELAPSED-MONTHS
01323            PERFORM 9700-DATE-LINK
01324            IF NO-CONVERSION-ERROR
01325               MOVE DC-GREG-DATE-1-EDIT     TO PDTHRUO.
01326
01327      MOVE AT-VOID-REASON             TO COMMENTO.
01328      MOVE AT-RECORDED-BY             TO PAIDBYO.
01329      MOVE AT-SEQUENCE-NO             TO SEQO.
01330
01331      MOVE AT-APPROVAL-LEVEL-REQD     TO AREQO.
01332      MOVE AT-APPROVED-LEVEL          TO ALEVO.
01333
01334      IF AT-PAYEE-TYPE EQUAL 'I'
01335         MOVE 'INSURED   '            TO PDTOO
01336      ELSE
01337      IF AT-PAYEE-TYPE EQUAL 'B'
01338         MOVE 'BENEFICARY'            TO PDTOO
01339      ELSE
01340      IF AT-PAYEE-TYPE EQUAL 'A'
01341         MOVE 'ACCOUNT   '            TO PDTOO
01342      ELSE
01343      IF AT-PAYEE-TYPE EQUAL 'O'
01344         MOVE 'OTHER-1   '            TO PDTOO
01345      ELSE
01346      IF AT-PAYEE-TYPE EQUAL 'Q'
01347         MOVE 'OTHER-2   '            TO PDTOO
01348      ELSE
01349      IF AT-PAYEE-TYPE EQUAL 'P'
01350         MOVE 'PHYSICIAN '            TO PDTOO
01351      ELSE
01352         MOVE AT-PAYEE-TYPE-CD        TO PDTOO.
01353
01354      MOVE AT-RECORDED-DT             TO DC-BIN-DATE-1.
01355      MOVE ' '                        TO DC-OPTION-CODE.
01356      PERFORM 9700-DATE-LINK.
01357      IF NO-CONVERSION-ERROR
01358         MOVE DC-GREG-DATE-1-EDIT     TO RECDTEO.
01359
01360      IF AT-FORCE-CONTROL EQUAL '1'
01361         MOVE 'Y'                     TO FORCEO
01362      ELSE
01363         MOVE 'N'                     TO FORCEO.
01364
01365      MOVE AT-SEQUENCE-NO              TO PI-LAST-TRLR-SEQ-NO.
01366      MOVE AT-PAYMENT-LAST-UPDATED-BY  TO PI-ELTRLR-UPDATE-BY.
01367      MOVE AT-LAST-MAINT-HHMMSS        TO PI-ELTRLR-UPDATE-HHMMSS.
01368      MOVE AT-CONTROL-PRIMARY          TO  PI-LAST-ELTRLR-KEY.
01369
01370      GO TO 8100-SEND-INITIAL-MAP.
01371
01372  3100-NOT-FOUND.
01373
01374      IF EIBAID EQUAL DFHPF1
01375         MOVE WS-HOLD-KEY TO ELACTQ-KEY
01376         GO TO 7005-CONTINUE-BROWSE.
01377
01378      IF EIBAID EQUAL DFHPF2
01379         MOVE WS-HOLD-KEY TO ELACTQ-KEY
01380         GO TO 7105-CONTINUE-BROWSE.
01381
01382      MOVE  ER-0142               TO EMI-ERROR.
01383      MOVE -1                     TO CARRL.
01384      MOVE AL-UNBON               TO CARRA
01385                                     CLAIMA
01386                                     CERTA.
01387      PERFORM 9900-ERROR-FORMAT.
01388      GO TO 8100-SEND-INITIAL-MAP.
01389
01390      EJECT
01391 ******************************************************************
01392 *      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *
01393 *      ACTIVITY QUE FILE.  WHEN AN ACTIVITY QUE RECORD WITH AN   *
01394 *      UNAPPROVED PAYMENT COUNTER GREATER THAN +0 IS FOUND THE   *
01395 *      FIRST UNAPPROVED PAYMENT FOR THAT CLAIM IS DISPLAYED.     *
01396 ******************************************************************
01397  7000-BROWSE-FWRD-NEXT-CLAIM.
01398
01399      MOVE PI-COMPANY-CD     TO ELACTQ-COMPANY-CD.
01400      MOVE CARRI             TO ELACTQ-CARRIER.
01401      MOVE CLAIMI            TO ELACTQ-CLAIM-NO.
01402      MOVE CERTI             TO ELACTQ-CERT-PRIME.
01403      MOVE SUFFIXI           TO ELACTQ-CERT-SFX.
01404
01405  7005-CONTINUE-BROWSE.
01406
01407      
      * EXEC CICS HANDLE CONDITION
01408 *         NOTFND    (7030-END-FILE)
01409 *         ENDFILE   (7030-END-FILE)
01410 *    END-EXEC.
      *    MOVE '"$I''                  ! '' #00006692' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303036363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01411
01412      
      * EXEC CICS STARTBR
01413 *         DATASET   (ELACTQ-FILE-ID)
01414 *         RIDFLD    (ELACTQ-KEY)
01415 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006697' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01416
01417      MOVE 'Y' TO WS-BROWSE-SW.
01418
01419      MOVE ELACTQ-KEY     TO  WS-HOLD-KEY.
01420
01421  7010-READ-NEXT-ELACTQ.
01422
01423      
      * EXEC CICS READNEXT
01424 *         DATASET   (ELACTQ-FILE-ID)
01425 *         RIDFLD    (ELACTQ-KEY)
01426 *         SET       (ADDRESS OF ACTIVITY-QUE)
01427 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006708' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01428
01429      IF ELACTQ-KEY EQUAL WS-HOLD-KEY
01430         GO TO 7010-READ-NEXT-ELACTQ.
01431
01432      IF AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD
01433         GO TO 7030-END-FILE.
01434
01435      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
01436         AQ-PAYMENT-COUNTER IS EQUAL TO +0
01437          GO TO 7010-READ-NEXT-ELACTQ.
01438
01439      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC OR
01440         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
01441          GO TO 7010-READ-NEXT-ELACTQ.
01442
01443      MOVE ELACTQ-KEY     TO  WS-HOLD-KEY.
01444
01445      MOVE AQ-CONTROL-PRIMARY  TO ELACTQ-KEY.
01446      MOVE +1                  TO PI-LAST-TRLR-SEQ-NO.
01447
01448      IF WS-BROWSE-SW EQUAL 'Y'
01449          PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
01450
01451      GO TO 3000-SHOW-CLAIM-PAYMENT.
01452
01453  7030-END-FILE.
01454
01455      IF WS-BROWSE-SW EQUAL 'Y'
01456          PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
01457
01458      MOVE -1                     TO MAINTL.
01459      MOVE  ER-2237               TO EMI-ERROR.
01460      PERFORM 9900-ERROR-FORMAT.
01461      GO TO 8200-SEND-DATAONLY.
01462
01463      EJECT
01464 ******************************************************************
01465 *      THIS ROUTINE BROWSES BACKWARD SEQUENTIALLY THROUGH THE    *
01466 *      ACTIVITY QUE FILE.  WHEN AN ACTIVITY QUE RECORD WITH AN   *
01467 *      UNAPPROVED PAYMENT COUNTER GREATER THAN +0 IS FOUND THE   *
01468 *      FIRST UNAPPROVED PAYMENT FOR THAT CLAIM IS DISPLAYED.     *
01469 ******************************************************************
01470  7100-BROWSE-BWRD-NEXT-CLAIM.
01471
01472      MOVE PI-COMPANY-CD     TO ELACTQ-COMPANY-CD.
01473      MOVE CARRI             TO ELACTQ-CARRIER.
01474      MOVE CLAIMI            TO ELACTQ-CLAIM-NO.
01475      MOVE CERTI             TO ELACTQ-CERT-PRIME.
01476      MOVE SUFFIXI           TO ELACTQ-CERT-SFX.
01477
01478  7105-CONTINUE-BROWSE.
01479
01480      
      * EXEC CICS HANDLE CONDITION
01481 *         NOTFND    (7130-END-FILE)
01482 *         ENDFILE   (7130-END-FILE)
01483 *    END-EXEC.
      *    MOVE '"$I''                  ! ( #00006765' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303036373635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01484
01485      
      * EXEC CICS STARTBR
01486 *         DATASET   (ELACTQ-FILE-ID)
01487 *         RIDFLD    (ELACTQ-KEY)
01488 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006770' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01489
01490      MOVE 'Y'     TO WS-BROWSE-SW.
01491
01492  7110-READ-NEXT-ELACTQ.
01493
01494      
      * EXEC CICS READNEXT
01495 *         DATASET   (ELACTQ-FILE-ID)
01496 *         RIDFLD    (ELACTQ-KEY)
01497 *         SET       (ADDRESS OF ACTIVITY-QUE)
01498 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006779' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01499
01500      
      * EXEC CICS READPREV
01501 *         DATASET   (ELACTQ-FILE-ID)
01502 *         RIDFLD    (ELACTQ-KEY)
01503 *         SET       (ADDRESS OF ACTIVITY-QUE)
01504 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006785' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01505
01506  7120-READ-PREV-ELACTQ.
01507
01508      
      * EXEC CICS READPREV
01509 *         DATASET   (ELACTQ-FILE-ID)
01510 *         RIDFLD    (ELACTQ-KEY)
01511 *         SET       (ADDRESS OF ACTIVITY-QUE)
01512 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006793' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01513
01514      IF AQ-COMPANY-CD NOT EQUAL PI-COMPANY-CD
01515         GO TO 7130-END-FILE.
01516
01517      MOVE ELACTQ-KEY             TO  WS-HOLD-KEY.
01518
01519      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
01520         AQ-PAYMENT-COUNTER IS EQUAL TO +0
01521          GO TO 7120-READ-PREV-ELACTQ.
01522
01523      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC OR
01524         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
01525          GO TO 7120-READ-PREV-ELACTQ.
01526
01527      MOVE AQ-CONTROL-PRIMARY  TO ELACTQ-KEY.
01528      MOVE +1                  TO PI-LAST-TRLR-SEQ-NO.
01529
01530      IF WS-BROWSE-SW EQUAL 'Y'
01531         MOVE ' ' TO WS-BROWSE-SW
01532         PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
01533
01534      GO TO 3000-SHOW-CLAIM-PAYMENT.
01535
01536  7130-END-FILE.
01537
01538      IF WS-BROWSE-SW EQUAL 'Y'
01539         MOVE ' ' TO WS-BROWSE-SW
01540         PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
01541
01542      MOVE -1                     TO MAINTL.
01543      MOVE  ER-2238               TO EMI-ERROR.
01544      PERFORM 9900-ERROR-FORMAT.
01545      GO TO 8200-SEND-DATAONLY.
01546
01547      EJECT
01548 ******************************************************************
01549 *      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *
01550 *      ACTIVITY TRAILER FILE SEARCHING FOR AND DISPLAYING        *
01551 *      UNAPPROVED PAYMENT TRAILER DATA ASSOCIATED WITH A         *
01552 *      PARTICULAR CLAIM.                                         *
01553 ******************************************************************
01554  7200-BROWSE-FWRD-NEXT-PAYMENT.
01555
01556      MOVE CARRI         TO WS-HOLD-CARR.
01557      MOVE CLAIMI        TO WS-HOLD-CLAIM.
01558      MOVE CERTI         TO WS-HOLD-CERT-PRIME.
01559      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
01560      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
01561
01562      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
01563         MOVE ER-0628     TO EMI-ERROR
01564         MOVE -1          TO MAINTL
01565         MOVE AL-UNBON    TO MAINTA
01566         PERFORM 9900-ERROR-FORMAT
01567         GO TO 8200-SEND-DATAONLY.
01568
01569      MOVE PI-LAST-ELACTQ-KEY  TO ELTRLR-KEY.
01570      MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.
01571      ADD +1   TO ELTRLR-SEQ-NO.
01572
01573      
      * EXEC CICS HANDLE CONDITION
01574 *         NOTFND    (7230-END-FILE)
01575 *         ENDFILE   (7230-END-FILE)
01576 *    END-EXEC.
      *    MOVE '"$I''                  ! ) #00006858' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303036383538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01577
01578  7210-READ-NEXT-ELTRLR.
01579
01580      
      * EXEC CICS READ
01581 *         DATASET   (ELTRLR-FILE-ID)
01582 *         RIDFLD    (ELTRLR-KEY)
01583 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01584 *         GTEQ
01585 *    END-EXEC.
      *    MOVE '&"S        G          (   #00006865' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383635' TO DFHEIV0(25:11)
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
           
01586
01587      MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.
01588      IF WS-HOLD-ELTRLR-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
01589         GO TO 7230-END-FILE.
01590
01591      IF AT-TRAILER-TYPE NOT EQUAL '2'
01592         ADD +1 TO ELTRLR-SEQ-NO
01593         GO TO 7210-READ-NEXT-ELTRLR.
01594
01595      IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'
01596         ADD +1 TO ELTRLR-SEQ-NO
01597         GO TO 7210-READ-NEXT-ELTRLR.
01598
01599      MOVE AT-CONTROL-PRIMARY  TO ELACTQ-KEY.
01600      MOVE AT-SEQUENCE-NO      TO PI-LAST-TRLR-SEQ-NO.
01601
01602      GO TO 3000-SHOW-CLAIM-PAYMENT.
01603
01604  7230-END-FILE.
01605
01606      MOVE -1                     TO MAINTL.
01607      MOVE  ER-0303               TO EMI-ERROR.
01608      PERFORM 9900-ERROR-FORMAT.
01609      GO TO 8200-SEND-DATAONLY.
01610
01611      EJECT
01612 ******************************************************************
01613 *      THIS ROUTINE BROWSES BACKWARD SEQUENTIALLY THROUGH THE    *
01614 *      ACTIVITY TRAILER FILE SEARCHING FOR AND DISPLAYING        *
01615 *      UNAPPROVED PAYMENT TRAILER DATA ASSOCIATED WITH A         *
01616 *      PARTICULAR CLAIM.                                         *
01617 ******************************************************************
01618  7300-BROWSE-BWRD-NEXT-PAYMENT.
01619
01620      MOVE CARRI         TO WS-HOLD-CARR.
01621      MOVE CLAIMI        TO WS-HOLD-CLAIM.
01622      MOVE CERTI         TO WS-HOLD-CERT-PRIME.
01623      MOVE SUFFIXI       TO WS-HOLD-CERT-SFX.
01624      MOVE PI-COMPANY-CD TO WS-HOLD-COMPANY-CD.
01625
01626      IF WS-HOLD-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
01627         MOVE ER-0628     TO EMI-ERROR
01628         MOVE -1          TO MAINTL
01629         MOVE AL-UNBON    TO MAINTA
01630         PERFORM 9900-ERROR-FORMAT
01631         GO TO 8200-SEND-DATAONLY.
01632
01633      MOVE PI-LAST-ELACTQ-KEY  TO ELTRLR-KEY.
01634      MOVE PI-LAST-TRLR-SEQ-NO TO ELTRLR-SEQ-NO.
01635
01636      
      * EXEC CICS HANDLE CONDITION
01637 *         NOTFND    (7330-END-FILE)
01638 *         ENDFILE   (7330-END-FILE)
01639 *    END-EXEC.
      *    MOVE '"$I''                  ! * #00006921' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303036393231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01640
01641      
      * EXEC CICS STARTBR
01642 *         DATASET   (ELTRLR-FILE-ID)
01643 *         RIDFLD    (ELTRLR-KEY)
01644 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006926' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01645
01646  7310-READ-NEXT-ELTRLR.
01647
01648      
      * EXEC CICS READNEXT
01649 *         DATASET   (ELTRLR-FILE-ID)
01650 *         RIDFLD    (ELTRLR-KEY)
01651 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01652 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006933' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393333' TO DFHEIV0(25:11)
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
           
01653
01654      
      * EXEC CICS READPREV
01655 *         DATASET   (ELTRLR-FILE-ID)
01656 *         RIDFLD    (ELTRLR-KEY)
01657 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01658 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006939' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393339' TO DFHEIV0(25:11)
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
           
01659
01660  7320-READ-PREV-ELTRLR.
01661
01662      
      * EXEC CICS READPREV
01663 *         DATASET   (ELTRLR-FILE-ID)
01664 *         RIDFLD    (ELTRLR-KEY)
01665 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
01666 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&0S                   )   #00006947' TO DFHEIV0
           MOVE X'263053202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393437' TO DFHEIV0(25:11)
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
           
01667
01668      MOVE AT-CONTROL-PRIMARY   TO WS-HOLD-ELTRLR-KEY.
01669      IF WS-HOLD-ELTRLR-KEY NOT EQUAL PI-LAST-ELACTQ-KEY
01670         GO TO 7330-END-FILE.
01671
01672      IF AT-TRAILER-TYPE NOT EQUAL '2'
01673         GO TO 7320-READ-PREV-ELTRLR.
01674
01675      IF AT-PAYMENT-APPROVAL-SW NOT EQUAL 'U'
01676         GO TO 7320-READ-PREV-ELTRLR.
01677
01678      MOVE AT-CONTROL-PRIMARY  TO ELACTQ-KEY.
01679      MOVE AT-SEQUENCE-NO      TO PI-LAST-TRLR-SEQ-NO.
01680
01681      GO TO 3000-SHOW-CLAIM-PAYMENT.
01682
01683  7330-END-FILE.
01684
01685      MOVE -1                     TO MAINTL.
01686      MOVE  ER-0303               TO EMI-ERROR.
01687      PERFORM 9900-ERROR-FORMAT.
01688      GO TO 8200-SEND-DATAONLY.
01689
01690      EJECT
01691  7500-FIND-BENEFIT.
01692
01693      
      * EXEC CICS HANDLE CONDITION
01694 *        ENDFILE   (7500-EXIT)
01695 *        NOTFND    (7500-EXIT)
01696 *    END-EXEC.
      *    MOVE '"$''I                  ! + #00006978' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303036393738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01697
01698      
      * EXEC CICS READ
01699 *        DATASET   ('ELCNTL')
01700 *        RIDFLD    (ELCNTL-KEY)
01701 *        SET       (ADDRESS OF CONTROL-FILE)
01702 *        GTEQ
01703 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        G          (   #00006983' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01704
01705      IF ELCNTL-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR
01706         ELCNTL-REC-TYPE   IS NOT EQUAL TO CF-RECORD-TYPE
01707          GO TO 7500-EXIT.
01708
01709      PERFORM 7500-BENEFIT-DUMMY THRU 7500-DUMMY-EXIT
01710          VARYING SUB FROM 1 BY 1 UNTIL
01711              ((SUB IS GREATER THAN 8) OR
01712              (CF-BENEFIT-CODE (SUB) IS EQUAL TO WS-BEN-CD)).
01713
01714      IF SUB IS NOT EQUAL TO 9
01715          MOVE 'Y'                TO  WS-BEN-SEARCH-SW.
01716
01717      GO TO 7500-EXIT.
01718
01719  7500-BENEFIT-DUMMY.
01720  7500-DUMMY-EXIT.
01721      EXIT.
01722
01723  7500-EXIT.
01724      EXIT.
01725      EJECT
01726  7600-READ-COMPANY-REC.
01727      
      * EXEC CICS HANDLE CONDITION
01728 *        ENDFILE   (7600-EXIT)
01729 *        NOTFND    (7600-EXIT)
01730 *    END-EXEC.
      *    MOVE '"$''I                  ! , #00007012' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303037303132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01731
01732      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
01733      MOVE '1'                    TO  ELCNTL-REC-TYPE.
01734      MOVE SPACES                 TO  ELCNTL-ACCESS.
01735      MOVE +0                     TO  ELCNTL-SEQ-NO.
01736
01737      
      * EXEC CICS READ
01738 *        DATASET   ('ELCNTL')
01739 *        RIDFLD    (ELCNTL-KEY)
01740 *        SET       (ADDRESS OF CONTROL-FILE)
01741 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007022' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01742
01743      MOVE CF-PAYMENT-APPROVAL-SW TO WS-PAYMENT-APPROVAL-SW.
01744
01745  7600-EXIT.
01746      EXIT.
01747
01748  7700-READ-USER-REC.
01749      IF PI-PROCESSOR-ID = 'LGXX'
01750         GO TO 7700-EXIT.
01751
01752      
      * EXEC CICS HANDLE CONDITION
01753 *        ENDFILE   (7700-EXIT)
01754 *        NOTFND    (7700-EXIT)
01755 *    END-EXEC.
      *    MOVE '"$''I                  ! - #00007037' TO DFHEIV0
           MOVE X'222427492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303037303337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01756
01757      MOVE PI-COMPANY-ID          TO  ELCNTL-COMPANY-ID.
01758      MOVE PI-PROCESSOR-ID        TO  ELCNTL-ACCESS.
01759      MOVE '2'                    TO  ELCNTL-REC-TYPE.
01760
01761      
      * EXEC CICS READ
01762 *        DATASET   ('ELCNTL')
01763 *        RIDFLD    (ELCNTL-KEY)
01764 *        SET       (ADDRESS OF CONTROL-FILE)
01765 *    END-EXEC.
           MOVE 'ELCNTL' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007046' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01766
01767      MOVE CF-APPROVAL-LEVEL TO WS-APPROVAL-LEVEL.
01768
01769  7700-EXIT.
01770      EXIT.
01771      EJECT
01772 ******************************************************************
01773 *      THIS ROUTINE BROWSES FORWARD SEQUENTIALLY THROUGH THE     *
01774 *      ACTIVITY QUE AND ACTIVITY TRAILER FILES SEARCHING FOR     *
01775 *      AND DISPLAYING UNAPPROVED PAYMENT DATA RELATED TO A       *
01776 *      USERS SPECIFIC APPROVAL LEVEL.                            *
01777 ******************************************************************
01778  8000-BROWSE-FWRD-NEXT-APPROVAL.
01779
01780      IF PI-UNAPPROVED-COUNT IS GREATER THAN +0
01781          MOVE PI-LAST-ELTRLR-KEY     TO  ELTRLR-KEY
01782          MOVE PI-LAST-ELACTQ-KEY     TO  ELACTQ-KEY
01783          SUBTRACT +1 FROM PI-UNAPPROVED-COUNT
01784          GO TO 8020-BROWSE-ACTIVITY-TRAILERS.
01785
01786      IF PI-FIRST-TIME-SW IS EQUAL TO 'Y'
01787          MOVE LOW-VALUES             TO  ELACTQ-KEY
01788          MOVE PI-COMPANY-CD          TO  ELACTQ-COMPANY-CD
01789          MOVE 'N'                    TO  PI-FIRST-TIME-SW
01790      ELSE
01791          MOVE PI-LAST-ELACTQ-KEY     TO  ELACTQ-KEY.
01792
01793      MOVE SPACES                     TO  PI-DIAGNOSIS.
01794
01795      
      * EXEC CICS HANDLE CONDITION
01796 *        NOTFND    (8040-END-FILE)
01797 *        ENDFILE   (8040-END-FILE)
01798 *    END-EXEC.
      *    MOVE '"$I''                  ! . #00007080' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303037303830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01799
01800      
      * EXEC CICS STARTBR
01801 *        DATASET   (ELACTQ-FILE-ID)
01802 *        RIDFLD    (ELACTQ-KEY)
01803 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007085' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01804
01805  8010-READNEXT.
01806      
      * EXEC CICS READNEXT
01807 *        DATASET   (ELACTQ-FILE-ID)
01808 *        RIDFLD    (ELACTQ-KEY)
01809 *        SET       (ADDRESS OF ACTIVITY-QUE)
01810 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007091' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELACTQ-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-QUE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01811
01812      IF ELACTQ-KEY IS EQUAL TO PI-LAST-ELACTQ-KEY
01813          GO TO 8010-READNEXT.
01814
01815      IF AQ-COMPANY-CD IS NOT EQUAL TO PI-COMPANY-CD
01816          GO TO 8040-END-FILE.
01817
01818      MOVE ELACTQ-KEY             TO  PI-LAST-ELACTQ-KEY.
01819
01820      IF AQ-PENDING-PAYMENT-FLAG IS EQUAL TO '1'
01821          NEXT SENTENCE
01822      ELSE
01823          GO TO 8010-READNEXT.
01824
01825      IF AQ-PAYMENT-COUNTER IS NOT NUMERIC OR
01826         AQ-PAYMENT-COUNTER IS EQUAL TO +0
01827          GO TO 8010-READNEXT.
01828
01829      IF AQ-PMT-UNAPPROVED-COUNT IS NOT NUMERIC OR
01830         AQ-PMT-UNAPPROVED-COUNT IS EQUAL TO +0
01831          GO TO 8010-READNEXT.
01832
01833      MOVE AQ-PMT-UNAPPROVED-COUNT    TO  PI-UNAPPROVED-COUNT.
01834      MOVE ELACTQ-KEY                 TO  ELTRLR-KEY.
01835      MOVE +0                         TO  ELTRLR-SEQ-NO.
01836
01837      PERFORM 8050-ENDBR-ELACTQ THRU 8050-EXIT.
01838
01839  8020-BROWSE-ACTIVITY-TRAILERS.
01840
01841      
      * EXEC CICS STARTBR
01842 *        DATASET   (ELTRLR-FILE-ID)
01843 *        RIDFLD    (ELTRLR-KEY)
01844 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007126' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 ELTRLR-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01845
01846  8030-READNEXT-ELTRLR.
01847
01848      
      * EXEC CICS HANDLE CONDITION
01849 *        NOTFND    (8045-END-FILE)
01850 *        ENDFILE   (8045-END-FILE)
01851 *    END-EXEC.
      *    MOVE '"$I''                  ! / #00007133' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303037313333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01852
01853      
      * EXEC CICS READNEXT
01854 *        DATASET   (ELTRLR-FILE-ID)
01855 *        RIDFLD    (ELTRLR-KEY)
01856 *        SET       (ADDRESS OF ACTIVITY-TRAILERS)
01857 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007138' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313338' TO DFHEIV0(25:11)
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
           
01858
01859      IF ELTRLR-COMPANY-CD IS NOT EQUAL TO PI-COMPANY-CD
01860          PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT
01861          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
01862
01863      IF ELTRLR-CLAIM-NO IS NOT EQUAL TO ELACTQ-CLAIM-NO
01864          PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT
01865          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
01866
01867      IF ELTRLR-CERT-NO IS NOT EQUAL TO ELACTQ-CERT-NO
01868          PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT
01869          GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
01870
01871      IF ELTRLR-KEY IS EQUAL TO PI-LAST-ELTRLR-KEY
01872          GO TO 8030-READNEXT-ELTRLR.
01873
01874      IF AT-TRAILER-TYPE EQUAL '6'
01875         IF AT-SEQUENCE-NO EQUAL +90
01876            MOVE AT-INFO-LINE-1   TO  PI-DIAGNOSIS.
01877
01878      IF AT-TRAILER-TYPE IS EQUAL TO '2'
01879          NEXT SENTENCE
01880      ELSE
01881          GO TO 8030-READNEXT-ELTRLR.
01882
01883      IF AT-PAYMENT-APPROVAL-SW IS NOT EQUAL TO 'U'
01884          GO TO 8030-READNEXT-ELTRLR.
01885
031808*01886      IF AT-APPROVED-LEVEL IS NOT EQUAL TO WS-APPROVAL-LEVEL
031808     IF AT-APPROVED-LEVEL IS NOT LESS THAN WS-APPROVAL-LEVEL
01887          GO TO 8030-READNEXT-ELTRLR.
01888
01889      MOVE AT-SEQUENCE-NO         TO  PI-LAST-TRLR-SEQ-NO.
01890      MOVE AT-CONTROL-PRIMARY     TO  PI-LAST-ELTRLR-KEY.
01891
01892      PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT.
01893
01894      GO TO 3000-SHOW-CLAIM-PAYMENT.
01895
01896  8040-END-FILE.
01897      MOVE -1                     TO MAINTL.
01898      MOVE  ER-0303               TO EMI-ERROR.
01899      PERFORM 9900-ERROR-FORMAT.
01900      GO TO 8200-SEND-DATAONLY.
01901
01902  8045-END-FILE.
01903
01904      PERFORM 8060-ENDBR-ELTRLR THRU 8060-EXIT.
01905      GO TO 8000-BROWSE-FWRD-NEXT-APPROVAL.
01906
01907      EJECT
01908  8050-ENDBR-ELACTQ.
01909
01910      
      * EXEC CICS ENDBR
01911 *        DATASET   (ELACTQ-FILE-ID)
01912 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007196' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELACTQ-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01913
01914  8050-EXIT.
01915      EXIT.
01916
01917  8060-ENDBR-ELTRLR.
01918
01919      
      * EXEC CICS ENDBR
01920 *        DATASET   (ELTRLR-FILE-ID)
01921 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007205' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ELTRLR-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01922
01923  8060-EXIT.
01924      EXIT.
01925
01926      EJECT
01927  8100-SEND-INITIAL-MAP.
01928      MOVE SAVE-DATE              TO DATEO.
01929      MOVE EIBTIME                TO TIME-IN.
01930      MOVE TIME-OUT               TO TIMEO.
01931      MOVE LOW-VALUES             TO MAINTO.
01932      MOVE -1                     TO MAINTL.
01933      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
01934
01935  8150-SEND-INITIAL-MAP.
01936
01937      IF PI-USES-PAID-TO
01938         MOVE 'PAID  TO :'       TO PDTHHD1O
01939         MOVE 'PAID  TO       :' TO PDTHHD2O.
01940
01941      
      * EXEC CICS SEND
01942 *        MAP      (MAP-NAME)
01943 *        MAPSET   (MAPSET-NAME)
01944 *        FROM     (EL143AO)
01945 *        ERASE
01946 *        CURSOR
01947 *    END-EXEC.
           MOVE LENGTH OF
            EL143AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00007227' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL143AO, 
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
           
01948
01949      GO TO 9100-RETURN-TRAN.
01950
01951  8200-SEND-DATAONLY.
01952
01953      MOVE SAVE-DATE              TO DATEO.
01954      MOVE EIBTIME                TO TIME-IN.
01955      MOVE TIME-OUT               TO TIMEO.
01956      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.
01957
01958      IF PI-USES-PAID-TO
01959         MOVE 'PAID  TO :'       TO PDTHHD1O
01960         MOVE 'PAID  TO       :' TO PDTHHD2O.
01961
01962      
      * EXEC CICS SEND
01963 *        MAP      (MAP-NAME)
01964 *        MAPSET   (MAPSET-NAME)
01965 *        FROM     (EL143AO)
01966 *        DATAONLY
01967 *        CURSOR
01968 *    END-EXEC.
           MOVE LENGTH OF
            EL143AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00007248' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL143AO, 
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
           
01969
01970      GO TO 9100-RETURN-TRAN.
01971
01972      EJECT
01973  8300-SEND-TEXT.
01974
01975      
      * EXEC CICS SEND TEXT
01976 *        FROM     (LOGOFF-TEXT)
01977 *        LENGTH   (LOGOFF-LENGTH)
01978 *        ERASE
01979 *        FREEKB
01980 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00007261' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323631' TO DFHEIV0(25:11)
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
           
01981
01982      
      * EXEC CICS RETURN
01983 *    END-EXEC.
      *    MOVE '.(                    ''   #00007268' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01984
01985      EJECT
01986  8500-NOT-FOUND.
01987
01988      MOVE ER-0282                TO  EMI-ERROR.
01989      MOVE -1                     TO  MAINTL.
01990      PERFORM 9900-ERROR-FORMAT.
01991      GO TO 8200-SEND-DATAONLY.
01992
01993  8800-UNAUTHORIZED-ACCESS.
01994
01995      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
01996      GO TO 8300-SEND-TEXT.
01997
01998  8810-PF23.
01999
02000      MOVE EIBAID                 TO PI-ENTRY-CD-1.
02001      MOVE XCTL-005               TO PGM-NAME.
02002      GO TO 9300-XCTL.
02003
02004  9100-RETURN-TRAN.
02005
02006      MOVE EMI-ERROR-NUMBER (1)      TO PI-LAST-ERROR-NO.
02007      MOVE SCREEN-NUMBER             TO PI-CURRENT-SCREEN-NO.
02008      
      * EXEC CICS RETURN
02009 *        TRANSID    (TRANS-ID)
02010 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02011 *        LENGTH     (PI-COMM-LENGTH)
02012 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00007294' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02013
02014  9200-RETURN-MAIN-MENU.
02015
02016      MOVE XCTL-126               TO PGM-NAME.
02017      GO TO 9300-XCTL.
02018
02019  9300-XCTL.
02020
02021      
      * EXEC CICS XCTL
02022 *        PROGRAM    (PGM-NAME)
02023 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
02024 *        LENGTH     (PI-COMM-LENGTH)
02025 *    END-EXEC.
      *    MOVE '.$C                   %   #00007307' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02026
02027  9400-CLEAR.
02028
02029      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.
02030      GO TO 9300-XCTL.
02031
02032  9500-PF12.
02033
02034      MOVE XCTL-010               TO PGM-NAME.
02035      GO TO 9300-XCTL.
02036
02037  9600-PGMID-ERROR.
02038
02039      
      * EXEC CICS HANDLE CONDITION
02040 *        PGMIDERR    (8300-SEND-TEXT)
02041 *    END-EXEC.
      *    MOVE '"$L                   ! 0 #00007325' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303037333235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02042
02043      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
02044      MOVE ' '                    TO PI-ENTRY-CD-1.
02045      MOVE XCTL-005               TO PGM-NAME.
02046      MOVE PGM-NAME               TO LOGOFF-PGM.
02047      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
02048      GO TO 9300-XCTL.
02049
02050  9700-DATE-LINK.
02051
02052      MOVE LINK-ELDATCV           TO PGM-NAME.
02053      
      * EXEC CICS LINK
02054 *        PROGRAM    (PGM-NAME)
02055 *        COMMAREA   (DATE-CONVERSION-DATA)
02056 *        LENGTH     (DC-COMM-LENGTH)
02057 *    END-EXEC.
      *    MOVE '."C                   (   #00007339' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02058
CIDMOD 9870-OUTPUT-ACTIVITY-RECORD.
CIDMOD
CIDMOD     
      * EXEC CICS GETMAIN
CIDMOD*        SET (ADDRESS OF DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (25)
CIDMOD*        INITIMG (WS-BLANK)
CIDMOD*    END-EXEC.
           MOVE 25
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00007347' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 WS-BLANK
           SET ADDRESS OF DAILY-ACTIVITY-RECORD TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.
CIDMOD     MOVE ELMSTR-KEY             TO DA-KEY.
CIDMOD     MOVE CL-TRAILER-SEQ-CNT     TO DA-TRAILER-SEQ-NO.
CIDMOD     IF WS-PAY-TYPE EQUAL '7'
CIDMOD         MOVE 'V'                TO DA-RECORD-TYPE
CIDMOD     ELSE
CIDMOD         MOVE 'P'                TO DA-RECORD-TYPE.
CIDMOD
CIDMOD     
      * EXEC CICS HANDLE CONDITION
CIDMOD*        NOTOPEN (9870-NOTOPEN)
CIDMOD*        DUPREC (9870-EXIT)
CIDMOD*    END-EXEC.
      *    MOVE '"$J%                  ! 1 #00007361' TO DFHEIV0
           MOVE X'22244A252020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303037333631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     
      * EXEC CICS WRITE
CIDMOD*        DATASET ('DLYACTV')
CIDMOD*        RIDFLD (DA-KEY)
CIDMOD*        FROM (DAILY-ACTIVITY-RECORD)
CIDMOD*        LENGTH (25)
CIDMOD*    END-EXEC.
           MOVE 'DLYACTV' TO DFHEIV1
           MOVE 25
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007366' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DAILY-ACTIVITY-RECORD, 
                 DFHEIV11, 
                 DA-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
CIDMOD
CIDMOD     MOVE 'N'                    TO ERROR-ON-OUTPUT-SW.
CIDMOD     GO TO 9870-EXIT.
CIDMOD
CIDMOD 9870-NOTOPEN.
CIDMOD
CIDMOD     MOVE '2955'                 TO EMI-ERROR.
CIDMOD     MOVE 'Y'                    TO ERROR-ON-OUTPUT-SW.
CIDMOD
CIDMOD 9870-EXIT.
CIDMOD     EXIT.
CIDMOD
02059  9900-ERROR-FORMAT.
02060
02061      IF NOT EMI-ERRORS-COMPLETE
02062          MOVE LINK-001           TO PGM-NAME
02063          
      * EXEC CICS LINK
02064 *            PROGRAM    (PGM-NAME)
02065 *            COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
02066 *            LENGTH     (EMI-COMM-LENGTH)
02067 *        END-EXEC.
      *    MOVE '."C                   (   #00007388' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02068
02069  9990-ABEND.
02070
02071      MOVE LINK-004               TO PGM-NAME.
02072      MOVE DFHEIBLK               TO EMI-LINE1.
02073
02074      
      * EXEC CICS LINK
02075 *        PROGRAM   (PGM-NAME)
02076 *        COMMAREA  (EMI-LINE1)
02077 *        LENGTH    (72)
02078 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00007399' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02079
02080      MOVE -1                     TO MAINTL.
02081      GO TO 8200-SEND-DATAONLY.
02082
02083  9995-SECURITY-VIOLATION.
02084 *    COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00007426' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037343236' TO DFHEIV0(25:11)
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
02085
02086  9995-EXIT.
02087       EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL143' TO DFHEIV1
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
               GO TO 0100-SEND-NEW,
                     0100-SEND-NEW
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 1010-NOT-FOUND,
                     1010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 2095-NOT-FOUND,
                     2095-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3100-NOT-FOUND,
                     3100-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 7030-END-FILE,
                     7030-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 7130-END-FILE,
                     7130-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 7230-END-FILE,
                     7230-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 7330-END-FILE,
                     7330-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 7500-EXIT,
                     7500-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 7600-EXIT,
                     7600-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 7700-EXIT,
                     7700-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8040-END-FILE,
                     8040-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8045-END-FILE,
                     8045-END-FILE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 9870-NOTOPEN,
                     9870-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL143' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
