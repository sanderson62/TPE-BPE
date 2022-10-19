00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL153 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 06/20/94 09:17:09.
00007 *                            VMOD=2.019
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
00023
00024 *REMARKS.
00025 *    SCREENS     - EL153S - NOTES AND REMINDERS
00026
00027 *    ENTERED BY  - EL150 - STATUS AND DISPOSITION
00028
00029 *    EXIT TO     - EL150 - CALLING PROGRAM
00030
00031 *    INPUT FILE  - ELMSTR - CLAIM MASTER
00032 *                - ELTRLR - ACTIVITY TRAILERS
00033
00034 *    OUTPUT FILE - ELMSTR - CLAIM MASTER
00035 *                - ELTRLR - ACTIVITY TRAILERS
00036
00037 *    COMMAREA    - PASSED CLAIM NUMBER FROM PROG INTERFACE BLK
00038
00039 *    ERROR-CODES ACCESSED - 132, 314, 133, 137, 29, 50, 315,
00040 *                            316, 317, 08
00041 *    NARRATIVE   - PROVIDE CREATION OF AUTO-PROMPT-TRAILER
00042 *                  AND GENERAL-INFO-TRAILER
00043 *                  IF THE AUTO PROMPT DATE GIVEN  IS LOWER
00044 *                  THAN THE NEXT-REMINDER DATE OF THE CLAIM MASTER
00045 *                  THIS FIELD IS UPDATED IN THE CLAIM MASTER
00046
102901******************************************************************
102901*                   C H A N G E   L O G
102901*
102901* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102901*-----------------------------------------------------------------
102901*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102901* EFFECTIVE    NUMBER
102901*-----------------------------------------------------------------
102901* 031102    2002022100003  SMVA  ADD CERT# TO EL153A SCREEN HEADER
062602* 062602    2002030700006  PEMA  Add note type of 'S'
062602*                                  (special review)
080106* 080106    2006052500001  AJRA  ADD NOTE TYPE 'N'(NOTE AND FILE)
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
102418* 102418  CR2018083000001  TANA  ADD NEW CALL TYPE
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
102901******************************************************************
00047      EJECT
00048  ENVIRONMENT DIVISION.
00049
00050  DATA DIVISION.
00051
00052  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00053
00054  77  FILLER  PIC X(32)  VALUE '********************************'.
00055  77  FILLER  PIC X(32)  VALUE '*   EL153  WORKING STORAGE     *'.
00056  77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.019 *********'.
00057
00058 *                                    COPY ELCSCTM.
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
00059
00060 *                                    COPY ELCSCRTY.
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
00061
00062  01  WS-DATE-AREA.
00063      12  SAVE-DATE                   PIC X(8)    VALUE SPACES.
00064      12  SAVE-BIN-DATE               PIC XX      VALUE SPACES.
00065      12  CURRENT-PLUS3-SAVE          PIC XX      VALUE SPACES.
00066
00067  01  WS-SCRATCH-AREA.
00068      12  GETMAIN-SPACE               PIC X       VALUE SPACE.
00069      12  WS-TRLR-LENGTH              PIC S9(4)   VALUE +200  COMP.
00070      12  SC-ITEM                     PIC S9(4)   VALUE +0001 COMP.
00071
00072      12  WS-JOURNAL-TYPE-ID          PIC XX      VALUE 'EL'.
00073
00074      12  WS-CURSOR                   PIC S9(4)   VALUE -1    COMP.
00075
00076      12  WS-MAP-NAME                 PIC X(8)    VALUE 'EL153A'.
00077      12  WS-MAPSET-NAME              PIC X(8)    VALUE 'EL153S'.
00078
00079      12  WS-TRANS-ID                 PIC X(4)    VALUE 'EX26'.
00080      12  THIS-PGM                    PIC X(8)    VALUE  'EL153'.
00081
00082      12  TIME-OUT.
00083          16  WS-TRANS-HOUR           PIC XX      VALUE SPACE.
00084          16  FILLER                  PIC X       VALUE '.'.
00085          16  WS-TRANS-MINUTE         PIC XX      VALUE SPACE.
00086
00087      12  WS-DATE-UNEDIT.
00088          16  FILLER                  PIC XX.
00089          16  WS-DATE-MDY.
00090              20  WS-DATE-MM          PIC XX.
00091              20  WS-DATE-DD          PIC XX.
00092              20  WS-DATE-YY          PIC XX.
00093
00094      12  WS-STR-DATE-BIN             PIC XX      VALUE LOW-VALUES.
00095      12  WS-END-DATE-BIN             PIC XX      VALUE LOW-VALUES.
00096
00097      12  SUB                         PIC S99     VALUE ZEROS.
00098      12  SUB-1                       PIC S99     VALUE ZEROS.
00099
00100      12  WS-FIRST-TIME-SW            PIC X       VALUE 'Y'.
00101
00102      12  WS-EDIT-NOTE.
00103          16  WS-EDIT-NOTE-1-4.
00104              20  WS-EDIT-NOTE-1-3    PIC X(3).
00105              20  FILLER              PIC X.
00106          16  FILLER                  PIC X(56).
00107
00108      12  WS-DATE-ERROR-SW            PIC X       VALUE SPACE.
00109          88  DATE-ERROR                          VALUE 'X'.
00110
062602     12  W-NOTE-TYPE                 PIC X       VALUE SPACE.
00111      12  W-CALL-TYPE                 PIC X       VALUE SPACE.
00112      12  W-NOTE-TYPE-IND             PIC X       VALUE SPACE.
00113
00114      12  TIME-IN                     PIC S9(7).
00115      12  WS-TIME  REDEFINES TIME-IN.
00116          16  FILLER                  PIC 9.
00117          16  WS-HOUR                 PIC 99.
00118          16  WS-MINUTE               PIC 99.
00119          16  FILLER                  PIC 99.
00120
00121      12  WS-TRAILER-KEY.
00122          16  WS-CLAIM-KEY.
00123              20  WS-KEY-COMPANY-CD       PIC X.
00124              20  WS-KEY-CARRIER          PIC X.
00125              20  WS-KEY-CLAIM-NO         PIC X(7).
00126              20  WS-KEY-CERT-NO.
00127                  24  WS-KEY-CERT-PRIME   PIC X(10).
00128                  24  WS-KEY-CERT-SFX     PIC X.
00129          16  WS-KEY-SEQUENCE-NO          PIC S9(4) COMP.
00130
00131      EJECT
00132  01  ERROR-MESSAGES.
00133      12  ER-0000                     PIC X(4)    VALUE '0000'.
00134      12  ER-0004                     PIC X(4)    VALUE '0004'.
00135      12  ER-0008                     PIC X(4)    VALUE '0008'.
00136      12  ER-0029                     PIC X(4)    VALUE '0029'.
00137      12  ER-0050                     PIC X(4)    VALUE '0050'.
00138      12  ER-0070                     PIC X(4)    VALUE '0070'.
00139      12  ER-0132                     PIC X(4)    VALUE '0132'.
00140      12  ER-0133                     PIC X(4)    VALUE '0133'.
00141      12  ER-0137                     PIC X(4)    VALUE '0137'.
00142      12  ER-0154                     PIC X(4)    VALUE '0154'.
00143      12  ER-0172                     PIC X(4)    VALUE '0172'.
00144      12  ER-0314                     PIC X(4)    VALUE '0314'.
00145      12  ER-0316                     PIC X(4)    VALUE '0316'.
00146      12  ER-0317                     PIC X(4)    VALUE '0317'.
00147      12  ER-0483                     PIC X(4)    VALUE '0483'.
00148      12  ER-0694                     PIC X(4)    VALUE '0694'.
00149      12  ER-0914                     PIC X(4)    VALUE '0914'.
00150      12  ER-0915                     PIC X(4)    VALUE '0915'.
00151      12  ER-0916                     PIC X(4)    VALUE '0916'.
00152      12  ER-0917                     PIC X(4)    VALUE '0917'.
00153      12  ER-0924                     PIC X(4)    VALUE '0924'.
00154      12  ER-7840                     PIC X(4)    VALUE '7840'.
062602     12  ER-7846                     PIC X(4)    VALUE '7846'.
00155      EJECT
00156 *                                    COPY ELCAID.
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
00157  01  PF-AID REDEFINES DFHAID.
00158      05  FILLER                      PIC X(8).
00159      05  PF-VALUES  OCCURS 24        PIC X.
00160      EJECT
00161 *                                    COPY ELCINTF.
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
00162      EJECT
00163 *                                    COPY ELCATTR.
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
00164      EJECT
00165 *                                    COPY ELCLOGOF.
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
00166      EJECT
00167 *                                    COPY ELCDATE.
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
00168      EJECT
00169 *                                    COPY ELCEMIB.
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
00170      EJECT
00171 *                                    COPY EL153S.
       01  EL153AI.
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
           05  MSTRDTL PIC S9(0004) COMP.
           05  MSTRDTF PIC  X(0001).
           05  FILLER REDEFINES MSTRDTF.
               10  MSTRDTA PIC  X(0001).
           05  MSTRDTI PIC  X(0008).
      *    -------------------------------
           05  MENDDTL PIC S9(0004) COMP.
           05  MENDDTF PIC  X(0001).
           05  FILLER REDEFINES MENDDTF.
               10  MENDDTA PIC  X(0001).
           05  MENDDTI PIC  X(0008).
      *    -------------------------------
           05  CALLTPL PIC S9(0004) COMP.
           05  CALLTPF PIC  X(0001).
           05  FILLER REDEFINES CALLTPF.
               10  CALLTPA PIC  X(0001).
           05  CALLTPI PIC  X(0001).
      *    -------------------------------
           05  NOTETPL PIC S9(0004) COMP.
           05  NOTETPF PIC  X(0001).
           05  FILLER REDEFINES NOTETPF.
               10  NOTETPA PIC  X(0001).
           05  NOTETPI PIC  X(0001).
      *    -------------------------------
           05  MLINE1L PIC S9(0004) COMP.
           05  MLINE1F PIC  X(0001).
           05  FILLER REDEFINES MLINE1F.
               10  MLINE1A PIC  X(0001).
           05  MLINE1I PIC  X(0060).
      *    -------------------------------
           05  MLINE2L PIC S9(0004) COMP.
           05  MLINE2F PIC  X(0001).
           05  FILLER REDEFINES MLINE2F.
               10  MLINE2A PIC  X(0001).
           05  MLINE2I PIC  X(0060).
      *    -------------------------------
           05  MLINE3L PIC S9(0004) COMP.
           05  MLINE3F PIC  X(0001).
           05  FILLER REDEFINES MLINE3F.
               10  MLINE3A PIC  X(0001).
           05  MLINE3I PIC  X(0060).
      *    -------------------------------
           05  MLINE4L PIC S9(0004) COMP.
           05  MLINE4F PIC  X(0001).
           05  FILLER REDEFINES MLINE4F.
               10  MLINE4A PIC  X(0001).
           05  MLINE4I PIC  X(0060).
      *    -------------------------------
           05  MLINE5L PIC S9(0004) COMP.
           05  MLINE5F PIC  X(0001).
           05  FILLER REDEFINES MLINE5F.
               10  MLINE5A PIC  X(0001).
           05  MLINE5I PIC  X(0060).
      *    -------------------------------
           05  MLINE6L PIC S9(0004) COMP.
           05  MLINE6F PIC  X(0001).
           05  FILLER REDEFINES MLINE6F.
               10  MLINE6A PIC  X(0001).
           05  MLINE6I PIC  X(0060).
      *    -------------------------------
           05  MLINE7L PIC S9(0004) COMP.
           05  MLINE7F PIC  X(0001).
           05  FILLER REDEFINES MLINE7F.
               10  MLINE7A PIC  X(0001).
           05  MLINE7I PIC  X(0060).
      *    -------------------------------
           05  MLINE8L PIC S9(0004) COMP.
           05  MLINE8F PIC  X(0001).
           05  FILLER REDEFINES MLINE8F.
               10  MLINE8A PIC  X(0001).
           05  MLINE8I PIC  X(0060).
      *    -------------------------------
           05  MLINE9L PIC S9(0004) COMP.
           05  MLINE9F PIC  X(0001).
           05  FILLER REDEFINES MLINE9F.
               10  MLINE9A PIC  X(0001).
           05  MLINE9I PIC  X(0060).
      *    -------------------------------
           05  MLINE10L PIC S9(0004) COMP.
           05  MLINE10F PIC  X(0001).
           05  FILLER REDEFINES MLINE10F.
               10  MLINE10A PIC  X(0001).
           05  MLINE10I PIC  X(0060).
      *    -------------------------------
           05  MLINE11L PIC S9(0004) COMP.
           05  MLINE11F PIC  X(0001).
           05  FILLER REDEFINES MLINE11F.
               10  MLINE11A PIC  X(0001).
           05  MLINE11I PIC  X(0060).
      *    -------------------------------
           05  MLINE12L PIC S9(0004) COMP.
           05  MLINE12F PIC  X(0001).
           05  FILLER REDEFINES MLINE12F.
               10  MLINE12A PIC  X(0001).
           05  MLINE12I PIC  X(0060).
      *    -------------------------------
           05  MLINE13L PIC S9(0004) COMP.
           05  MLINE13F PIC  X(0001).
           05  FILLER REDEFINES MLINE13F.
               10  MLINE13A PIC  X(0001).
           05  MLINE13I PIC  X(0060).
      *    -------------------------------
           05  MLINE14L PIC S9(0004) COMP.
           05  MLINE14F PIC  X(0001).
           05  FILLER REDEFINES MLINE14F.
               10  MLINE14A PIC  X(0001).
           05  MLINE14I PIC  X(0060).
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
      *    -------------------------------
           05  PFKEY4L PIC S9(0004) COMP.
           05  PFKEY4F PIC  X(0001).
           05  FILLER REDEFINES PFKEY4F.
               10  PFKEY4A PIC  X(0001).
           05  PFKEY4I PIC  X(0014).
       01  EL153AO REDEFINES EL153AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRNDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MRNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MCERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MSTRDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MENDDTO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CALLTPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  NOTETPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE1O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE2O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE3O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE4O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE5O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE6O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE7O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE8O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE9O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE10O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE11O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE12O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE13O PIC  X(0060).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MLINE14O PIC  X(0060).
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
           05  FILLER            PIC  X(0003).
           05  PFKEY4O PIC  X(0014).
      *    -------------------------------
00172  01  EL153AI-R REDEFINES EL153AI.
062602*    12  FILLER                      PIC X(70).
062602     12  FILLER                      PIC X(74).
00174      12  EL153AI-OCCURS OCCURS 14 TIMES.
00175          16  EL153A-NOTE-LENGTH      PIC S9(4)     COMP.
00176          16  EL153A-NOTE-ATTRB       PIC X.
00177          16  EL153A-NOTE             PIC X(60).
00178
00179      EJECT
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
00181
00182  01  DFHCOMMAREA                     PIC X(1024).
00183      EJECT
00184 *                                    COPY ELCMSTR.
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
00185      EJECT
00186 *                                    COPY ELCTRLR.
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
00187      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CLAIM-MASTER
                                ACTIVITY-TRAILERS.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL153' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00189
00190      MOVE EIBDATE               TO  DC-JULIAN-YYDDD.
00191      MOVE '5'                   TO  DC-OPTION-CODE.
00192      PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT.
00193      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00194      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00195
00196      MOVE DFHCOMMAREA           TO  PROGRAM-INTERFACE-BLOCK.
00197
00198      IF EIBCALEN NOT GREATER THAN ZEROS
00199        GO TO 9000-UNAUTHERR.
00200
00201      IF PI-COMPANY-ID = 'DMD'
00202          MOVE +3                TO  DC-ELAPSED-MONTHS
00203          MOVE +0                TO  DC-ELAPSED-DAYS
00204          MOVE '6'               TO  DC-OPTION-CODE
00205          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00206          MOVE DC-BIN-DATE-2     TO  CURRENT-PLUS3-SAVE.
00207
00208      
      * EXEC CICS  HANDLE CONDITION
00209 *           ERROR    (9990-ERROR)
00210 *           PGMIDERR (9600-PGMIDERR)
00211 *    END-EXEC.
      *    MOVE '"$.L                  ! " #00002087' TO DFHEIV0
           MOVE X'22242E4C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303032303837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00212
00213      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00214          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00215              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
00216              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
00217              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
00218              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
00219              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
00220              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
00221              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
00222              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
00223          ELSE
00224              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
00225              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
00226              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
00227              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
00228              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
00229              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
00230              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
00231              MOVE SPACES               TO PI-SAVED-PROGRAM-6
00232      ELSE
00233          GO TO 0100-RECEIVE.
00234
00235      MOVE LOW-VALUES             TO  EL153AO.
00236
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' or 'FNL'
00238         MOVE -1                  TO CALLTPL
00239      ELSE
CIDMOD*       IF PI-COMPANY-ID = 'CID'
CIDMOD*          MOVE -1               TO MLINE1L
CIDMOD*       ELSE
00240            MOVE -1               TO MSTRDTL
CIDMOD*       END-IF
CIDMOD     END-IF
00241
00242      MOVE +2                     TO  EMI-NUMBER-OF-LINES.
00243      MOVE SPACES                 TO  MERMSG1O
00244                                      MERMSG2O.
00245      MOVE '153A'                 TO  PI-CURRENT-SCREEN-NO.
00246      MOVE ER-0694                TO  EMI-ERROR.
00247      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00248      GO TO 8100-SEND-INITIAL-MAP.
00249
00250      EJECT
00251  0100-RECEIVE.
00252
00253      IF EIBAID = DFHCLEAR
00254          GO TO 9400-CLEAR.
00255
00256      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00257          MOVE LOW-VALUES         TO  EL153AO
00258          MOVE ER-0008            TO  EMI-ERROR
00259          MOVE -1                 TO  MLINE1L
00260          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00261          GO TO 8200-SEND-DATAONLY.
00262
00263      IF PI-PROCESSOR-ID = 'LGXX'
00264          NEXT SENTENCE
00265      ELSE
00266          
      * EXEC CICS READQ TS
00267 *            QUEUE    (PI-SECURITY-TEMP-STORE-ID)
00268 *            INTO     (SECURITY-CONTROL)
00269 *            LENGTH   (SC-COMM-LENGTH)
00270 *            ITEM     (SC-ITEM)
00271 *        END-EXEC
      *    MOVE '*$II   L              ''   #00002150' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00272          MOVE SC-CLAIMS-DISPLAY (8)  TO  PI-DISPLAY-CAP
00273          MOVE SC-CLAIMS-UPDATE  (8)  TO  PI-MODIFY-CAP.
00274
00275      
      * EXEC CICS RECEIVE
00276 *        MAP      (WS-MAP-NAME)
00277 *        MAPSET   (WS-MAPSET-NAME)
00278 *        INTO     (EL153AI)
00279 *    END-EXEC.
           MOVE LENGTH OF
            EL153AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00002159' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032313539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL153AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00280
00281      IF MPFNUMBL > +0
00282          IF EIBAID NOT = DFHENTER
00283              MOVE ER-0004        TO  EMI-ERROR
00284              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00285              MOVE AL-UNBOF       TO  MPFNUMBA
00286              MOVE -1             TO  MPFNUMBL
00287              GO TO 8200-SEND-DATAONLY.
00288
00289      IF MPFNUMBI IS NUMERIC
00290          IF MPFNUMBO > ZERO AND
00291             MPFNUMBO < 25
00292              MOVE PF-VALUES (MPFNUMBI)   TO  EIBAID
00293          ELSE
00294              MOVE ER-0029                TO  EMI-ERROR
00295              MOVE AL-UNBOF               TO  MPFNUMBA
00296              MOVE -1                     TO  MPFNUMBL
00297              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00298              GO TO 8200-SEND-DATAONLY.
00299
00300      IF EIBAID = DFHPF12
00301          MOVE 'EL010   '         TO  THIS-PGM
00302          GO TO 9300-XCTL.
00303
00304      IF EIBAID = DFHPF23
00305          MOVE EIBAID             TO  PI-ENTRY-CD-1
00306          MOVE 'EL005   '         TO  THIS-PGM
00307          GO TO 9300-XCTL.
00308
00309      IF EIBAID = DFHPF24
00310          MOVE 'EL126   '         TO  THIS-PGM
00311          GO TO 9300-XCTL.
00312
00313      IF (EIBAID = DFHENTER)
00314                OR
00315         (EIBAID = DFHPF4 AND
00316          PI-COMPANY-ID = 'DMD')
00317          NEXT SENTENCE
00318      ELSE
00319          MOVE ER-0008            TO  EMI-ERROR
00320          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00321          MOVE -1                 TO  MPFNUMBL
00322          MOVE AL-UNBON           TO  MPFNUMBA
00323          GO TO 8200-SEND-DATAONLY.
00324
00325  0200-PROCESSING-MAINLINE.
00326
00327      IF NOT MODIFY-CAP
00328          MOVE 'UPDATE'           TO  SM-READ
00329          PERFORM 9995-SECURITY-VIOLATION
00330          MOVE ER-0070            TO  EMI-ERROR
00331          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00332          MOVE -1                 TO  MPFNUMBL
00333          GO TO 8100-SEND-INITIAL-MAP.
00334
00335      MOVE +0                     TO  SUB.
00336      PERFORM 4000-CHECK-INPUT-LOOP THRU 4000-CHECK-EXIT.
00337
00338      IF NOT EMI-NO-ERRORS
00339          GO TO 8200-SEND-DATAONLY.
00340
00341      MOVE PI-COMPANY-CD          TO  WS-KEY-COMPANY-CD.
00342      MOVE PI-CARRIER             TO  WS-KEY-CARRIER.
00343      MOVE PI-CLAIM-NO            TO  WS-KEY-CLAIM-NO.
00344      MOVE PI-CERT-NO             TO  WS-KEY-CERT-NO.
00345
062602     IF (NOTETPL > +0)
062602*       AND (NOTETPI = 'S')
080106        IF (NOTETPI = 'S')
062602          GO TO 1200-BUILD-SPEC-REVIEW
080106        ELSE
080106          GO TO 1000-BUILD-NOTES
080106        END-IF
062602     ELSE
00346        IF MSTRDTI = (LOW-VALUES OR SPACES) AND
00347          MENDDTI = (LOW-VALUES OR SPACES)
00348          GO TO 1000-BUILD-NOTES
00349        ELSE
00350           GO TO 1500-BUILD-REMINDERS
062602       END-IF
062602     END-IF
00351      .
00352      EJECT
00353  1000-BUILD-NOTES.
00354
00355      MOVE SPACES TO W-CALL-TYPE W-NOTE-TYPE-IND W-NOTE-TYPE.
00356
00357      IF CALLTPI NOT = SPACES AND LOW-VALUES
102418         IF CALLTPI = 'I' OR 'O' OR 'N'
00359              MOVE CALLTPI    TO W-CALL-TYPE
00360              MOVE 'C'        TO W-NOTE-TYPE-IND
00361          ELSE
00362              MOVE ER-0915    TO EMI-ERROR
00363              MOVE AL-UABON   TO CALLTPA
00364              MOVE -1         TO CALLTPL
00365              MOVE 'X'        TO WS-DATE-ERROR-SW
00366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00367              GO TO 8200-SEND-DATAONLY.
080106
080106     IF NOTETPI NOT = SPACES AND LOW-VALUES
080106         IF NOTETPI = 'N'
080106             MOVE NOTETPI    TO W-NOTE-TYPE
080106             MOVE 'N'        TO W-NOTE-TYPE-IND
080106         ELSE
080106             MOVE ER-7846    TO EMI-ERROR
080106             MOVE AL-UABON   TO NOTETPA
080106             MOVE -1         TO NOTETPL
080106             MOVE 'X'        TO WS-DATE-ERROR-SW
080106             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080106             GO TO 8200-SEND-DATAONLY
080106         END-IF
080106     END-IF.
00368
00369      MOVE +1                     TO  SUB   SUB-1.
00370      PERFORM 4100-SQUASH-SCREEN THRU 4100-EXIT.
00371
00372      IF PI-COMPANY-ID = 'AIG' OR 'AUK'
00373          MOVE +0                 TO  SUB
00374          PERFORM 6000-EXPAND-AIG-NOTES THRU 6000-EXIT.
00375
00376      PERFORM 7000-READ-UP-CLAIM THRU 7050-EXIT.
00377      PERFORM 7100-GETMAIN-TRLR  THRU 7100-EXIT.
00378
00379      MOVE +15                    TO  SUB.
00380      PERFORM 3200-BUILD-GI-TRLR-LOOP THRU 3200-EXIT.
00381      MOVE +0                     TO  SUB   SUB-1.
00382
00383      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
00384      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.
00385      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
00386      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
00387
00388      PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.
00389
00390      MOVE ER-0000                TO  EMI-ERROR.
00391      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00392      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
00393
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
00395          MOVE -1                 TO  CALLTPL.
00396
00397      GO TO 8200-SEND-DATAONLY.
00398
00399      EJECT
062602 1200-BUILD-SPEC-REVIEW.
062602
062602     IF NOTETPI NOT = SPACES AND LOW-VALUES
062602         IF NOTETPI = 'S'
00359              MOVE NOTETPI    TO W-NOTE-TYPE
00360              MOVE 'S'        TO W-NOTE-TYPE-IND
00361          ELSE
00362              MOVE ER-7846    TO EMI-ERROR
00363              MOVE AL-UABON   TO NOTETPA
00364              MOVE -1         TO NOTETPL
00365              MOVE 'X'        TO WS-DATE-ERROR-SW
00366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
062602             GO TO 8200-SEND-DATAONLY.
062602
00369      MOVE +1                     TO  SUB   SUB-1.
00370      PERFORM 4100-SQUASH-SCREEN THRU 4100-EXIT.
00371
00376      PERFORM 7000-READ-UP-CLAIM THRU 7050-EXIT.
00409      PERFORM 7100-GETMAIN-TRLR     THRU 7100-EXIT.
00411      PERFORM 3150-BUILD-TRAILER-SR THRU 3150-EXIT.
00412
00413      PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.
00423
00383      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
00384      MOVE SAVE-BIN-DATE          TO  CL-LAST-MAINT-DT.
00385      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
00386      MOVE '3'                    TO  CL-LAST-MAINT-TYPE.
00387
00388      PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.
00389
00424      MOVE ER-0000                TO  EMI-ERROR.
00425      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00426      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
00427
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
00429          MOVE -1                 TO  CALLTPL.
00430
062602     GO TO 8200-SEND-DATAONLY.
062602
062602     EJECT
00400  1500-BUILD-REMINDERS.
00401
00402      PERFORM 2000-EDIT-SCREEN THRU 2000-EXIT.
00403
00404      IF NOT EMI-NO-ERRORS
00405          GO TO 8200-SEND-DATAONLY.
00406
00407      PERFORM 7000-READ-UP-CLAIM    THRU 7050-EXIT.
00408      PERFORM 2200-CHECK-DATES      THRU 2200-EXIT.
00409      PERFORM 7100-GETMAIN-TRLR     THRU 7100-EXIT.
00410      PERFORM 3000-REDUCE-SEQ       THRU 3000-EXIT.
00411      PERFORM 3100-BUILD-TRAILER-AP THRU 3100-EXIT.
00412
00413      PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.
00414
00415      IF CL-NEXT-FOLLOWUP-DT = LOW-VALUES
00416          MOVE WS-STR-DATE-BIN        TO  CL-NEXT-FOLLOWUP-DT
00417      ELSE
00418          IF WS-STR-DATE-BIN < CL-NEXT-FOLLOWUP-DT AND
00419             WS-STR-DATE-BIN NOT = LOW-VALUES
00420              MOVE WS-STR-DATE-BIN    TO  CL-NEXT-FOLLOWUP-DT.
00421
00422      PERFORM 7055-REWRITE-CLAIM THRU 7055-EXIT.
00423
00424      MOVE ER-0000                TO  EMI-ERROR.
00425      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00426      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
00427
062121     IF PI-COMPANY-ID = 'DMD' or 'CID' OR 'AHL' OR 'FNL'
00429          MOVE -1                 TO  CALLTPL.
00430
00431      GO TO 8200-SEND-DATAONLY.
00432
00433      EJECT
00434  2000-EDIT-SCREEN.
00435      MOVE SPACES                 TO  WS-DATE-ERROR-SW.
00436
00437      IF MSTRDTI = SPACES OR LOW-VALUES
00438          MOVE LOW-VALUES         TO  WS-STR-DATE-BIN
00439      ELSE
00440          MOVE MSTRDTI            TO  WS-DATE-UNEDIT
00441          
      * EXEC CICS  BIF  DEEDIT
00442 *            FIELD (WS-DATE-UNEDIT)
00443 *            LENGTH (8)
00444 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002389' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DATE-UNEDIT, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00445          MOVE '4'                TO  DC-OPTION-CODE
00446          MOVE WS-DATE-MDY        TO  DC-GREG-DATE-1-MDY
00447          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00448          IF DATE-CONVERSION-ERROR
00449              MOVE ER-0314        TO  EMI-ERROR
00450              MOVE AL-UABON       TO  MSTRDTA
00451              MOVE -1             TO  MSTRDTL
00452              MOVE 'X'            TO  WS-DATE-ERROR-SW
00453              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00454          ELSE
00455              IF (PI-COMPANY-ID = 'DMD') AND
00456                 (EIBAID NOT = DFHPF4)   AND
00457                 (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)
00458                  MOVE 'PF4=FORCE 7840' TO PFKEY4O
00459                  MOVE AL-SABON         TO PFKEY4A
00460                  MOVE ER-7840          TO EMI-ERROR
00461                  MOVE AL-UABON         TO MSTRDTA
00462                  MOVE -1               TO MSTRDTL
00463                  MOVE 'X'              TO WS-DATE-ERROR-SW
00464                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00465              ELSE
00466                MOVE SPACES               TO PFKEY4O
00467                MOVE AL-SADON             TO PFKEY4A
00468                MOVE AL-UANON             TO MSTRDTA
00469                MOVE DC-BIN-DATE-1        TO WS-STR-DATE-BIN
00470                MOVE DC-GREG-DATE-1-EDIT  TO MSTRDTI.
00471
00472      IF MENDDTI = SPACES OR = LOW-VALUES
00473          MOVE LOW-VALUES         TO  WS-END-DATE-BIN
00474      ELSE
00475          MOVE MENDDTI            TO  WS-DATE-UNEDIT
00476          
      * EXEC CICS  BIF  DEEDIT
00477 *            FIELD (WS-DATE-UNEDIT)
00478 *            LENGTH (8)
00479 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00002424' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303032343234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-DATE-UNEDIT, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00480          MOVE '4'                TO  DC-OPTION-CODE
00481          MOVE WS-DATE-MDY        TO  DC-GREG-DATE-1-MDY
00482          PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
00483          IF  DATE-CONVERSION-ERROR
00484              MOVE ER-0314        TO  EMI-ERROR
00485              MOVE AL-UABON       TO  MENDDTA
00486              MOVE -1             TO  MENDDTL
00487              MOVE 'X'            TO  WS-DATE-ERROR-SW
00488              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00489          ELSE
00490              MOVE AL-UANON             TO  MENDDTA
00491              MOVE DC-BIN-DATE-1        TO  WS-END-DATE-BIN
00492              MOVE DC-GREG-DATE-1-EDIT  TO  MENDDTI.
00493  2000-EXIT.
00494       EXIT.
00495
00496      EJECT
00497  2200-CHECK-DATES.
00498      IF WS-STR-DATE-BIN  LESS THAN SAVE-BIN-DATE AND
00499         WS-END-DATE-BIN  LESS THAN SAVE-BIN-DATE
00500          MOVE ER-0316            TO  EMI-ERROR
00501          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00502          MOVE -1                 TO  MSTRDTL
00503          GO TO 8100-SEND-INITIAL-MAP.
00504
00505      IF WS-STR-DATE-BIN  GREATER THAN WS-END-DATE-BIN
00506          MOVE ER-0317            TO  EMI-ERROR
00507          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00508          MOVE -1                 TO  MSTRDTL
00509          GO TO 8100-SEND-INITIAL-MAP.
00510
00511  2200-EXIT.
00512       EXIT.
00513
00514      EJECT
00515  3000-REDUCE-SEQ.
00516      IF CL-LAST-TRL-AVAIL
00517          MOVE ER-0137            TO  EMI-ERROR
00518          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00519          MOVE -1                 TO  EL153A-NOTE-LENGTH (1)
00520          GO TO 8100-SEND-INITIAL-MAP.
00521
00522      SUBTRACT  1 FROM CL-TRAILER-SEQ-CNT.
00523      MOVE CL-TRAILER-SEQ-CNT     TO  WS-KEY-SEQUENCE-NO
00524                                      AT-SEQUENCE-NO.
00525
00526  3000-EXIT.
00527       EXIT.
00528
00529  3100-BUILD-TRAILER-AP.
00530      MOVE 'AT'                   TO  AT-RECORD-ID.
00531      MOVE '7'                    TO  AT-TRAILER-TYPE.
00532
00533      IF EL153A-NOTE-LENGTH (1) NOT > +0
00534          MOVE SPACES             TO  AT-PROMPT-LINE-1
00535      ELSE
00536          MOVE EL153A-NOTE  (1)   TO  AT-PROMPT-LINE-1.
00537
00538      IF EL153A-NOTE-LENGTH (2) NOT > +0
00539          MOVE SPACES             TO  AT-PROMPT-LINE-2
00540      ELSE
00541          MOVE EL153A-NOTE  (2)   TO  AT-PROMPT-LINE-2.
00542
00543      MOVE WS-STR-DATE-BIN        TO  AT-PROMPT-START-DT.
00544      MOVE WS-END-DATE-BIN        TO  AT-PROMPT-END-DT.
00545      MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
00546      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
00547                                      AT-PROMPT-LAST-UPDATED-BY.
00548      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
00549      MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT
00550                                      AT-PROMPT-LAST-MAINT-DT.
00551
00552  3100-EXIT.
00553       EXIT.
00554
00555      EJECT
062602 3150-BUILD-TRAILER-SR.
062602     MOVE 'AT'                   TO  AT-RECORD-ID.
00531      MOVE '6'                    TO  AT-TRAILER-TYPE.
00532
00533      IF EL153A-NOTE-LENGTH (1) NOT > +0
00534          MOVE SPACES             TO  AT-info-line-1
00535      ELSE
00536          MOVE EL153A-NOTE  (1)   TO  AT-info-line-1.
00537
00538      IF EL153A-NOTE-LENGTH (2) NOT > +0
00539          MOVE SPACES             TO  AT-info-line-2
00540      ELSE
00541          MOVE EL153A-NOTE  (2)   TO  AT-info-line-2.
00542
062602     MOVE W-NOTE-TYPE-IND        TO  AT-INFO-TRAILER-TYPE.
062602     move +92                    to  ws-key-sequence-no
00545      MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
00546      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
00572                                      AT-GEN-INFO-LAST-UPDATED-BY.
00548      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
00549      MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT
00577                                      AT-GEN-INFO-LAST-MAINT-DT
00551      .
00552  3150-EXIT.
00553       EXIT.
00554
00555      EJECT
00556  3200-BUILD-GI-TRLR-LOOP.
00557
00558      SUBTRACT +1 FROM SUB.
00559
00560      IF SUB < +1
00561          GO TO 3200-EXIT.
00562
00563      IF EL153A-NOTE-LENGTH (SUB) = +0
00564          GO TO 3200-BUILD-GI-TRLR-LOOP.
00565
00566      MOVE SPACES                 TO ACTIVITY-TRAILERS.
00567      MOVE 'AT'                   TO  AT-RECORD-ID.
00568      MOVE '6'                    TO  AT-TRAILER-TYPE.
00569
00570      MOVE WS-TRAILER-KEY         TO  AT-CONTROL-PRIMARY.
00571      MOVE PI-PROCESSOR-ID        TO  AT-RECORDED-BY
00572                                      AT-GEN-INFO-LAST-UPDATED-BY.
00573      MOVE W-NOTE-TYPE-IND        TO  AT-INFO-TRAILER-TYPE.
00574      MOVE W-CALL-TYPE            TO  AT-CALL-TYPE.
00575      MOVE EIBTIME                TO  AT-LAST-MAINT-HHMMSS.
00576      MOVE SAVE-BIN-DATE          TO  AT-RECORDED-DT
00577                                      AT-GEN-INFO-LAST-MAINT-DT.
00578
00579      IF WS-FIRST-TIME-SW = 'Y'
00580          MOVE 'N'                TO  WS-FIRST-TIME-SW
00581          IF SUB NOT = 1 AND 2
00582              MOVE 'X'            TO AT-NOTE-CONTINUATION
00583          END-IF
00584          IF SUB = 1 OR 3 OR 5 OR 7 OR 9 OR 11 OR 13
00585              MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-1
00586              PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT
00587              MOVE CL-TRAILER-SEQ-CNT TO  AT-SEQUENCE-NO
00588              PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT
00589              GO TO 3200-BUILD-GI-TRLR-LOOP
00590          ELSE
00591              MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-2
00592              SUBTRACT +1             FROM SUB
00593              MOVE EL153A-NOTE (SUB)  TO  AT-INFO-LINE-1
00594              PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT
00595              MOVE CL-TRAILER-SEQ-CNT TO  AT-SEQUENCE-NO
00596              PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT
00597              GO TO 3200-BUILD-GI-TRLR-LOOP.
00598
00599      MOVE EL153A-NOTE (SUB)      TO  AT-INFO-LINE-2.
00600      SUBTRACT +1 FROM SUB.
00601      MOVE EL153A-NOTE (SUB)      TO  AT-INFO-LINE-1.
00602
00603      IF SUB NOT = 1
00604          MOVE 'X'                TO AT-NOTE-CONTINUATION.
00605
00606      PERFORM 3000-REDUCE-SEQ THRU 3000-EXIT.
00607      MOVE CL-TRAILER-SEQ-CNT     TO  AT-SEQUENCE-NO.
00608      PERFORM 7150-WRITE-TRAILER THRU 7199-EXIT.
00609
00610      GO TO 3200-BUILD-GI-TRLR-LOOP.
00611
00612  3200-EXIT.
00613       EXIT.
00614
00615      EJECT
00616  4000-CHECK-INPUT-LOOP.
00617
00618       ADD +1 TO SUB.
00619
00620       IF SUB > +14
00621           MOVE ER-0483           TO  EMI-ERROR
00622           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00623           MOVE -1                TO  EL153A-NOTE-LENGTH (1)
00624           GO TO 4000-CHECK-EXIT.
00625
00626       IF EL153A-NOTE-LENGTH (SUB) NOT > +0
00627           GO TO 4000-CHECK-INPUT-LOOP.
00628
00629  4000-CHECK-EXIT.
00630      EXIT.
00631
00632  4100-SQUASH-SCREEN.
00633
00634      IF SUB > +14
00635          GO TO 4100-EXIT.
00636
00637      IF EL153A-NOTE-LENGTH (SUB) > +0
00638          IF SUB = SUB-1
00639              ADD +1              TO  SUB   SUB-1
00640              GO TO 4100-SQUASH-SCREEN.
00641
00642      IF EL153A-NOTE-LENGTH (SUB) > +0
00643          MOVE EL153A-NOTE-LENGTH (SUB)   TO
00644                                       EL153A-NOTE-LENGTH (SUB-1)
00645          MOVE EL153A-NOTE-ATTRB  (SUB)   TO
00646                                       EL153A-NOTE-ATTRB  (SUB-1)
00647          MOVE EL153A-NOTE        (SUB)   TO  EL153A-NOTE (SUB-1)
00648          MOVE +0                         TO
00649                                       EL153A-NOTE-LENGTH (SUB)
00650          MOVE LOW-VALUES                 TO  EL153A-NOTE (SUB)
00651          MOVE AL-UANOF                   TO
00652                                       EL153A-NOTE-ATTRB  (SUB)
00653          ADD +1                          TO  SUB   SUB-1
00654      ELSE
00655          ADD +1                          TO  SUB.
00656
00657      GO TO 4100-SQUASH-SCREEN.
00658
00659  4100-EXIT.
00660      EXIT.
00661
00662      EJECT
00663  6000-EXPAND-AIG-NOTES.
00664
00665      ADD +1 TO SUB.
00666
00667      IF SUB > +14
00668          GO TO 6000-EXIT.
00669
00670      IF EL153A-NOTE-LENGTH (SUB) > +0
00671          MOVE EL153A-NOTE (SUB)          TO  WS-EDIT-NOTE
00672          INSPECT WS-EDIT-NOTE REPLACING ALL LOW-VALUES BY SPACES
00673          IF WS-EDIT-NOTE-1-4 = 'APC '
00674              MOVE 'AUTO-PAY CLAIM'       TO  EL153A-NOTE (SUB)
00675          ELSE
00676          IF WS-EDIT-NOTE-1-4 = 'MGR '
00677              MOVE 'MANAGEMENT REVIEW'    TO  EL153A-NOTE (SUB)
00678          ELSE
00679          IF WS-EDIT-NOTE-1-4 = 'SVR '
00680              MOVE 'SUPERVISOR REVIEW'    TO  EL153A-NOTE (SUB)
00681          ELSE
00682          IF WS-EDIT-NOTE-1-4 = 'OIO '
00683              MOVE 'OUTSIDE INVESTIGATION ORDERED'
00684                                          TO  EL153A-NOTE (SUB)
00685          ELSE
00686          IF WS-EDIT-NOTE-1-4 = 'LGR '
00687              MOVE 'LEGAL REVIEW'         TO  EL153A-NOTE (SUB)
00688          ELSE
00689          IF WS-EDIT-NOTE-1-4 = 'FRN '
00690              MOVE 'FORM REVIEWED/NO ADDITIONAL PAYMENT DUE AT THIS
00691 -                 ' TIME'                TO  EL153A-NOTE (SUB)
00692          ELSE
00693          IF WS-EDIT-NOTE-1-4 = 'PRB '
00694              MOVE 'PHONE CALL RECEIVED--BRANCH'
00695                                          TO  EL153A-NOTE (SUB)
00696          ELSE
00697          IF WS-EDIT-NOTE-1-4 = 'PRG '
00698              MOVE 'PHONE CALL RECEIVED--GROUP'
00699                                          TO  EL153A-NOTE (SUB)
00700          ELSE
00701          IF WS-EDIT-NOTE-1-4 = 'PRH '
00702              MOVE 'PHONE CALL RECEIVED--HOSPITAL'
00703                                          TO  EL153A-NOTE (SUB)
00704          ELSE
00705          IF WS-EDIT-NOTE-1-4 = 'PRI '
00706              MOVE 'PHONE CALL RECEIVED--INSURED'
00707                                          TO  EL153A-NOTE (SUB)
00708          ELSE
00709          IF WS-EDIT-NOTE-1-4 = 'PRO '
00710              MOVE 'PHONE CALL RECEIVED--OTHER'
00711                                          TO  EL153A-NOTE (SUB)
00712          ELSE
00713          IF WS-EDIT-NOTE-1-4 = 'PRP '
00714              MOVE 'PHONE CALL RECEIVED--PHYSICIAN'
00715                                          TO  EL153A-NOTE (SUB)
00716          ELSE
00717          IF WS-EDIT-NOTE-1-4 = 'PMB '
00718              MOVE 'PHONE CALL MADE--BRANCH'
00719                                          TO  EL153A-NOTE (SUB)
00720          ELSE
00721          IF WS-EDIT-NOTE-1-4 = 'PMG '
00722              MOVE 'PHONE CALL MADE--GROUP'
00723                                          TO  EL153A-NOTE (SUB)
00724          ELSE
00725          IF WS-EDIT-NOTE-1-4 = 'PMH '
00726              MOVE 'PHONE CALL MADE--HOSPITAL'
00727                                          TO  EL153A-NOTE (SUB)
00728          ELSE
00729          IF WS-EDIT-NOTE-1-4 = 'PMI '
00730              MOVE 'PHONE CALL MADE--INSURED'
00731                                          TO  EL153A-NOTE (SUB)
00732          ELSE
00733          IF WS-EDIT-NOTE-1-4 = 'PMO '
00734              MOVE 'PHONE CALL MADE--OTHER'
00735                                          TO  EL153A-NOTE (SUB)
00736          ELSE
00737          IF WS-EDIT-NOTE-1-4 = 'PMP '
00738              MOVE 'PHONE CALL MADE--PHYSICIAN'
00739                                          TO  EL153A-NOTE (SUB)
00740          ELSE
00741          IF WS-EDIT-NOTE-1-3 = '01 '
00742              MOVE 'CONTINUING CLAIM FORM RCVD'
00743                                          TO  EL153A-NOTE (SUB)
00744          ELSE
00745          IF WS-EDIT-NOTE-1-3 = '02 '
00746              MOVE 'ACCOUNT INFORMATION RCVD'
00747                                          TO  EL153A-NOTE (SUB)
00748          ELSE
00749          IF WS-EDIT-NOTE-1-3 = '03 '
00750              MOVE 'MEDICAL HISTORY RCVD'
00751                                          TO  EL153A-NOTE (SUB)
00752          ELSE
00753          IF WS-EDIT-NOTE-1-3 = '04 '
00754              MOVE 'INSURED INFO RCVD'
00755                                          TO  EL153A-NOTE (SUB)
00756          ELSE
00757          IF WS-EDIT-NOTE-1-3 = '05 '
00758              MOVE 'EMPLOYER INFO RCVD'
00759                                          TO  EL153A-NOTE (SUB)
00760          ELSE
00761          IF WS-EDIT-NOTE-1-3 = '06 '
00762              MOVE 'PARTIAL INVESTIGATION RCVD.  CLAIM IS STILL PEN
00763 -                 'DING'                 TO  EL153A-NOTE (SUB)
00764          ELSE
00765          IF WS-EDIT-NOTE-1-3 = '07 '
00766              MOVE 'DEATH CERTIFICATE RCVD'
00767                                          TO  EL153A-NOTE (SUB)
00768          ELSE
00769          IF WS-EDIT-NOTE-1-3 = '08 '
00770              MOVE 'ATTORNEY LTR RCVD'
00771                                          TO  EL153A-NOTE (SUB)
00772          ELSE
00773          IF WS-EDIT-NOTE-1-3 = '09 '
00774              MOVE 'EXAM RESULTS RCVD'
00775                                          TO  EL153A-NOTE (SUB)
00776          ELSE
00777          IF WS-EDIT-NOTE-1-3 = '10 '
00778              MOVE 'INS DEPT INQUIRY RCVD'
00779                                          TO  EL153A-NOTE (SUB)
00780          ELSE
00781          IF WS-EDIT-NOTE-1-3 = '11 '
00782              MOVE 'RETURNED MAIL RCVD'
00783                                          TO  EL153A-NOTE (SUB)
00784          ELSE
00785          IF WS-EDIT-NOTE-1-3 = '12 '
00786              MOVE 'INITIAL CLAIM RCVD'
00787                                          TO  EL153A-NOTE (SUB).
00788
00789      GO TO 6000-EXPAND-AIG-NOTES.
00790
00791  6000-EXIT.
00792      EXIT.
00793
00794      EJECT
00795  7000-READ-UP-CLAIM.
00796
00797      
      * EXEC CICS HANDLE CONDITION
00798 *        NOTFND    (7040-NOTFND)
00799 *        NOTOPEN   (9981-NOTOPEN-MSTR)
00800 *    END-EXEC.
      *    MOVE '"$IJ                  ! # #00002772' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303032373732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00801
00802      
      * EXEC CICS READ
00803 *        DATASET   ('ELMSTR')
00804 *        RIDFLD    (WS-CLAIM-KEY)
00805 *        SET       (ADDRESS OF CLAIM-MASTER)
00806 *        UPDATE
00807 *    END-EXEC.
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&"S        EU         (   #00002777' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032373737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CLAIM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00808
00809      GO TO 7050-EXIT.
00810
00811  7040-NOTFND.
00812
00813      MOVE ER-0133                TO  EMI-ERROR.
00814      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
00815      MOVE -1                     TO  EL153A-NOTE-LENGTH (1).
00816      GO TO 8100-SEND-INITIAL-MAP.
00817
00818  7050-EXIT.
00819      EXIT.
00820
00821  7055-REWRITE-CLAIM.
00822
00823      
      * EXEC CICS HANDLE CONDITION
00824 *        DUPKEY   (7055-EXIT)
00825 *    END-EXEC.
      *    MOVE '"$$                   ! $ #00002798' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303032373938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00826
00827      
      * EXEC CICS REWRITE
00828 *        DATASET   ('ELMSTR')
00829 *        FROM      (CLAIM-MASTER)
00830 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
           MOVE 'ELMSTR' TO DFHEIV1
      *    MOVE '&& L                  %   #00002802' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00831
00832  7055-EXIT.
00833      EXIT.
00834
00835  7100-GETMAIN-TRLR.
00836      
      * EXEC CICS GETMAIN
00837 *        SET      (ADDRESS OF ACTIVITY-TRAILERS)
00838 *        INITIMG  (GETMAIN-SPACE)
00839 *        LENGTH   (WS-TRLR-LENGTH)
00840 *    END-EXEC.
      *    MOVE ',"IL                  $   #00002811' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 WS-TRLR-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00841
00842  7100-EXIT.
00843       EXIT.
00844
00845  7150-WRITE-TRAILER.
00846
00847      
      * EXEC CICS HANDLE CONDITION
00848 *        DUPREC    (7190-DUPREC)
00849 *        NOTOPEN   (9982-NOTOPEN-TRLR)
00850 *    END-EXEC.
      *    MOVE '"$%J                  ! % #00002822' TO DFHEIV0
           MOVE X'2224254A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303032383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00851
00852      
      * EXEC CICS WRITE
00853 *        DATASET   ('ELTRLR')
00854 *        RIDFLD    (WS-TRAILER-KEY)
00855 *        FROM      (ACTIVITY-TRAILERS)
00856 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
           MOVE 'ELTRLR' TO DFHEIV1
      *    MOVE '&$ L                  ''   #00002827' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 WS-TRAILER-KEY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00857
00858      GO TO 7199-EXIT.
00859
00860  7190-DUPREC.
062602
062602     if w-note-type = 'S'
062602        MOVE ER-7846             TO EMI-ERROR
062602        MOVE -1                  TO notetpl
062602        PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
062602        GO TO 8200-SEND-DATAONLY
062602     else
00861         PERFORM 3000-REDUCE-SEQ  THRU 3000-EXIT
00862         GO TO 7150-WRITE-TRAILER
           end-if
00863      .
00864  7199-EXIT.
00865      EXIT.
00866
00867       EJECT
00868  8100-SEND-INITIAL-MAP.
00869      MOVE EIBTIME                TO  TIME-IN.
00870      MOVE WS-HOUR                TO  WS-TRANS-HOUR.
00871      MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.
00872      MOVE TIME-OUT               TO  MRNTIMEO.
00873      MOVE SAVE-DATE              TO  MRNDATEO.
031102     MOVE PI-CERT-NO             TO  MCERTO.
00874
00875      
      * EXEC CICS SEND
00876 *        MAP      (WS-MAP-NAME)
00877 *        MAPSET   (WS-MAPSET-NAME)
00878 *        FROM     (EL153AI)
00879 *        FREEKB
00880 *        ERASE
00881 *        CURSOR
00882 *    END-EXEC.
           MOVE LENGTH OF
            EL153AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E F  H L F ,   #00002860' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL153AI, 
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
           
00883
00884      GO TO 9100-RETURN-TRANS.
00885  8100-EXIT.
00886      EXIT.
00887
00888  8200-SEND-DATAONLY.
00889      MOVE EIBTIME                TO  TIME-IN.
00890      MOVE WS-HOUR                TO  WS-TRANS-HOUR.
00891      MOVE WS-MINUTE              TO  WS-TRANS-MINUTE.
00892      MOVE TIME-OUT               TO  MRNTIMEO.
00893      MOVE SAVE-DATE              TO  MRNDATEO.
031102     MOVE PI-CERT-NO             TO  MCERTO.
00894
00895      
      * EXEC CICS SEND
00896 *        MAP      (WS-MAP-NAME)
00897 *        MAPSET   (WS-MAPSET-NAME)
00898 *        FROM     (EL153AI)
00899 *        FREEKB
00900 *        DATAONLY
00901 *        CURSOR
00902 *    END-EXEC.
           MOVE LENGTH OF
            EL153AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT    F  H L F ,   #00002881' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2046202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL153AI, 
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
           
00903
00904      GO TO 9100-RETURN-TRANS.
00905
00906  8200-EXIT.
00907      EXIT.
00908
00909      EJECT
00910  8300-SEND-TEXT.
00911      
      * EXEC CICS SEND TEXT
00912 *        FROM     (LOGOFF-TEXT)
00913 *        ERASE
00914 *        FREEKB
00915 *        LENGTH   (LOGOFF-LENGTH)
00916 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00002897' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303032383937' TO DFHEIV0(25:11)
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
           
00917
00918      
      * EXEC CICS RETURN
00919 *    END-EXEC.
      *    MOVE '.(                    ''   #00002904' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00920
00921  8300-EXIT.
00922       EXIT.
00923
00924  8500-DATE-CONVERSION.
00925      
      * EXEC CICS LINK
00926 *           PROGRAM  ('ELDATCV')
00927 *           COMMAREA (DATE-CONVERSION-DATA)
00928 *           LENGTH   (DC-COMM-LENGTH)
00929 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00002911' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00930
00931  8500-EXIT.
00932       EXIT.
00933
00934      EJECT
00935  9000-UNAUTHERR.
00936      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
00937      GO TO 8300-SEND-TEXT.
00938
00939  9100-RETURN-TRANS.
00940      
      * EXEC CICS RETURN
00941 *        TRANSID  (WS-TRANS-ID)
00942 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00943 *        LENGTH   (PI-COMM-LENGTH)
00944 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00002926' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00945
00946  9100-EXIT.
00947       EXIT.
00948
00949  9300-XCTL.
00950      
      * EXEC CICS XCTL
00951 *        PROGRAM  (THIS-PGM)
00952 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
00953 *        LENGTH   (PI-COMM-LENGTH)
00954 *    END-EXEC.
      *    MOVE '.$C                   %   #00002936' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00955
00956  9300-EXIT.
00957       EXIT.
00958
00959  9400-CLEAR.
00960      MOVE DFHENTER              TO  EIBAID.
00961      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
00962      GO TO 9300-XCTL.
00963
00964  9400-EXIT.
00965      EXIT.
00966
00967      EJECT
00968  9600-PGMIDERR.
00969      
      * EXEC CICS HANDLE CONDITION
00970 *        PGMIDERR (8300-SEND-TEXT)
00971 *    END-EXEC.
      *    MOVE '"$L                   ! & #00002955' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303032393535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00972
00973      MOVE THIS-PGM               TO  LOGOFF-PGM
00974                                      PI-CALLING-PROGRAM.
00975      MOVE SPACES                 TO  PI-ENTRY-CD-1.
00976      MOVE 'EL005'                TO  THIS-PGM.
00977      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
00978      GO TO 9300-XCTL.
00979
00980  9600-EXIT.
00981       EXIT.
00982
00983      EJECT
00984  9900-ERROR-FORMAT.
00985      
      * EXEC CICS LINK
00986 *        PROGRAM  ('EL001')
00987 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
00988 *        LENGTH   (EMI-COMM-LENGTH)
00989 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00002971' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00990
00991      MOVE EMI-LINE1          TO MERMSG1O.
00992      MOVE EMI-LINE2          TO MERMSG2O.
00993
00994  9900-EXIT.
00995       EXIT.
00996
00997  9981-NOTOPEN-MSTR.
00998      MOVE ER-0154                TO  EMI-ERROR.
00999      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01000      MOVE -1                     TO  MPFNUMBL.
01001      GO TO 8100-SEND-INITIAL-MAP.
01002
01003  9981-EXIT.
01004       EXIT.
01005
01006  9982-NOTOPEN-TRLR.
01007      MOVE ER-0172                TO  EMI-ERROR.
01008      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01009      MOVE -1                     TO  MPFNUMBL.
01010      GO TO 8100-SEND-INITIAL-MAP.
01011
01012  9990-ERROR.
01013      
      * EXEC CICS LINK
01014 *        PROGRAM  ('EL004')
01015 *        COMMAREA (DFHEIBLK)
01016 *        LENGTH   (64)
01017 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 64
             TO DFHEIV11
      *    MOVE '."C                   (   #00002999' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303032393939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIBLK, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01018
01019      GO TO 8200-SEND-DATAONLY.
01020
01021  9995-SECURITY-VIOLATION.
01022 *                            COPY ELCSCTP SUPPRESS.
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
      *    MOVE '."C                   (   #00003025' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033303235' TO DFHEIV0(25:11)
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
01023

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL153' TO DFHEIV1
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
               GO TO 7040-NOTFND,
                     9981-NOTOPEN-MSTR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 7055-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 7190-DUPREC,
                     9982-NOTOPEN-TRLR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL153' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
