00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL105 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 12/13/94 09:22:24.
00007 *                            VMOD=2.017
00008 *
00008 *
00009
00010 *AUTHOR.    LOGIC, INC.
00011 *           DALLAS, TEXAS.
00012
00013 *DATE-COMPILED.
00014
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024
00025
00026 *REMARKS.    TRANSACTION - EX12
00027
00028 *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED
00029 *    FOR THE CARRIER CONTROL RECORDS.
00030
00031 *    SCREENS     - EL105A - CARRIER MAINTENANCE
00032
00033 *    ENTERED BY  - EL101 OR EL601 - MAINTENANCE MENU
00034
00035 *    EXIT TO     - EL101 OR EL601 - MAINTENANCE MENU
00036
00037 *    INPUT FILE  - ELCNTL - CONTROL FILE - CARRIER RECORDS
00038
00039 *    OUTPUT FILE - ELCNTL - CONTROL FILE - CARRIER RECORDS
00040
00041 *    COMMAREA    - PASSED
00042
00043 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101 OR EL601
00044 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE
00045 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
00046 *                  ENTRIES (XCTL FROM CICS VIA EX12) THE SCREEN
00047 *                  WILL BE READ AND ACTION WILL BE BASED ON THE
00048 *                  MAINTENANCE TYPE INDICATED.
112103******************************************************************
112103*                   C H A N G E   L O G
112103*
112103* Changes are marked by the Change Effective date.
112103*-----------------------------------------------------------------
112103*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
112103* EFFECTIVE    NUMBER
112103*-----------------------------------------------------------------
112103* 112103    2003080800002  SMVA  ADD CLP TOLERANCE % FOR SECURE PA
092705* 092705  CR2005050300006  PEMA  ADD SPP LEASES
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
032813* 032813  CR2012051000001  AJRA  DISPLAY NEXT REAUDIT CHECK NUMBER
062121* 062121  CR2021021600001  PEMA  ADD PROCESSING FOR NEW COMP FNL
112103******************************************************************
00049
00050      EJECT
00051  ENVIRONMENT DIVISION.
00052
00053  DATA DIVISION.
00054
00055  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00056  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.
00057
00058  77  FILLER  PIC X(32)  VALUE '********************************'.
00059  77  FILLER  PIC X(32)  VALUE '*    EL105 WORKING STORAGE     *'.
00060  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.017 **********'.
00061
00062 *                            COPY ELCSCTM.
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
00063 *                            COPY ELCSCRTY.
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
00064
00065  01  WS-DATE-AREA.
00066      05  SAVE-DATE           PIC X(8)    VALUE SPACES.
00067      05  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.
00068
00069  01  WS-ZIP-CODE-AREA.
00070      12  WS-ZIP-CODE.
00071          16  WS-ZIP-1            PIC X.
00072              88  WS-CANADIAN-ZIP    VALUE 'A' THRU 'Z'.
00073          16  WS-ZIP-2-3          PIC XX.
00074          16  WS-ZIP-4            PIC X.
00075          16  WS-ZIP-5            PIC X.
00076          16  WS-ZIP-6            PIC X.
00077          16  FILLER              PIC X(4).
00078      12  WS-ZIP-AM-1  REDEFINES  WS-ZIP-CODE.
00079          16  WS-ZIP-AM-1-CODE    PIC X(5).
00080          16  WS-ZIP-AM-1-PLUS4   PIC X(4).
00081          16  FILLER              PIC X.
00082      12  WS-ZIP-AM-2  REDEFINES  WS-ZIP-CODE.
00083          16  WS-ZIP-AM-2-CODE    PIC X(5).
00084          16  WS-ZIP-AM-2-DASH    PIC X.
00085          16  WS-ZIP-AM-2-PLUS4   PIC X(4).
00086      12  WS-ZIP-CAN-1  REDEFINES  WS-ZIP-CODE.
00087          16  WS-ZIP-CAN-1-POST1  PIC XXX.
00088          16  WS-ZIP-CAN-1-POST2  PIC XXX.
00089          16  FILLER              PIC X(4).
00090      12  WS-ZIP-CAN-2  REDEFINES  WS-ZIP-CODE.
00091          16  WS-ZIP-CAN-2-POST1  PIC XXX.
00092          16  FILLER              PIC X.
00093          16  WS-ZIP-CAN-2-POST2  PIC XXX.
00094          16  FILLER              PIC XXX.
00095
00096      12  WS-ZIP-CODE-NUM         PIC 9(9).
00097
00098  01  FILLER                          COMP-3.
00099      05  TIME-IN                     PIC S9(7)       VALUE ZERO.
00100      05  TIME-OUT                    REDEFINES
00101          TIME-IN                     PIC S9(3)V9(4).
00102
00103      05  WS-EXPENSE-DOLLAR           PIC S9(3)V99 VALUE ZERO.
00104      05  WS-EXPENSE-PERCENT          PIC S9(3)V99 VALUE ZERO.
00105
092705     05  WS-SPP-LEASE-COMM           PIC S9(5)V99    VALUE +0.
112103     05  WS-CLP-TOL-PCT              PIC S9(1)V9(4)  VALUE ZEROS.
00106      05  WS-TOL-PREM-PCT             PIC S9(1)V9(4)  VALUE ZEROS.
00107      05  WS-TOL-REF-PCT              PIC S9(1)V9(4)  VALUE ZEROS.
00108      05  WS-CR-OVR-SHT-PCT           PIC S9(1)V9(4)  VALUE ZEROS.
00109
00110      05  WS-IBNR-UEP-PCT             PIC S9(3)V9(4)  VALUE ZEROS.
00111      05  WS-IBNR-R78-PCT             PIC S9(3)V9(4)  VALUE ZEROS.
00112      05  WS-IBNR-PRO-PCT             PIC S9(3)V9(4)  VALUE ZEROS.
00113
00114  01  FILLER                          COMP  SYNC.
00115      05  WS-JOURNAL-FILE-ID          PIC S9(4)       VALUE +1.
00116      05  WS-JOURNAL-RECORD-LENGTH    PIC S9(4)       VALUE +773.
00117
00118      05  APHONE-LENGTH               PIC S9(4)       VALUE +12.
00119      05  AEXPCA-LENGTH               PIC S9(4)       VALUE +7.
00120      05  AEXPCP-LENGTH               PIC S9(4)       VALUE +7.
00121      05  ALQCA-LENGTH                PIC S9(4)       VALUE +7.
00122      05  ALMRP-LENGTH                PIC S9(4)       VALUE +13.
00123      05  ALQCD-LENGTH                PIC S9(4)       VALUE +4.
00124      05  ALMDPP-LENGTH               PIC S9(4)       VALUE +4.
00125      05  ALDBC-LENGTH                PIC S9(4)       VALUE +4.
00126      05  ALMAP-LENGTH                PIC S9(4)       VALUE +13.
00127      05  ALMBP-LENGTH                PIC S9(4)       VALUE +4.
00128      05  ALMAPM-LENGTH               PIC S9(4)       VALUE +4.
00129      05  APCTCDT-LENGTH              PIC S9(4)       VALUE +7.
00130      05  IBNRPCT-LENGTH              PIC S9(4)       VALUE +7.
00131      05  AUEPPCT-LENGTH              PIC S9(4)       VALUE +7.
00132      05  AR78PCT-LENGTH              PIC S9(4)       VALUE +7.
00133      05  APROPCT-LENGTH              PIC S9(4)       VALUE +7.
00134
00135  01  FILLER.
00136      05  XCTL-EL126                  PIC X(5)    VALUE 'EL126'.
00137      05  XCTL-EL626                  PIC X(5)    VALUE 'EL626'.
00138      05  XCTL-EM626                  PIC X(5)    VALUE 'EM626'.
00139      05  XCTL-GL800                  PIC X(5)    VALUE 'GL800'.
00140      05  GETMAIN-SPACE               PIC X  VALUE SPACE.
00141      05  WS-CONTROL-FILE-KEY.
00142          10  WS-CFK-COMPANY-ID       PIC X(3).
00143          10  WS-CFK-RECORD-TYPE      PIC X.
00144 *          88  LF-BENEFIT-MASTER                     VALUE '4'.
00145 *          88  AH-BENEFIT-MASTER                     VALUE '5'.
00146          10  FILLER                  PIC XXX.
00147          10  WS-CFK-CARRIER-NO       PIC X.
00148          10  WS-CFK-SEQUENCE-NO      PIC S9(4)  COMP.
00149
00150      05  WS-MAPSET-NAME              PIC X(8)      VALUE 'EL105S'.
00151      05  WS-MAP-NAME                 PIC X(8)      VALUE 'EL105A'.
00152
00153      05  FILLER                      REDEFINES
00154          WS-MAP-NAME.
00155          10  FILLER                  PIC XX.
00156          10  WS-MAP-NUMBER           PIC X(4).
00157          10  FILLER                  PIC XX.
00158
00159      05  THIS-PGM                    PIC X(8)      VALUE 'EL105'.
00160
00161      05  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.
00162
00163      05  WS-JOURNAL-TYPE-ID          PIC XX          VALUE 'EL'.
00164
00165      05  WS-TRANS-ID                 PIC X(4)        VALUE 'EX12'.
00166
00167      EJECT
00168  01  ERROR-MESSAGES.
00169      12  ER-0000                 PIC X(4)  VALUE '0000'.
00170      12  ER-0004                 PIC X(4)  VALUE '0004'.
00171      12  ER-0006                 PIC X(4)  VALUE '0006'.
00172      12  ER-0023                 PIC X(4)  VALUE '0023'.
00173      12  ER-0029                 PIC X(4)  VALUE '0029'.
00174      12  ER-0042                 PIC X(4)  VALUE '0042'.
00175      12  ER-0050                 PIC X(4)  VALUE '0050'.
00176      12  ER-0052                 PIC X(4)  VALUE '0052'.
00177      12  ER-0053                 PIC X(4)  VALUE '0053'.
00178      12  ER-0070                 PIC X(4)  VALUE '0070'.
00179      12  ER-0090                 PIC X(4)  VALUE '0090'.
00180      12  ER-0091                 PIC X(4)  VALUE '0091'.
00181      12  ER-0092                 PIC X(4)  VALUE '0092'.
00182      12  ER-0093                 PIC X(4)  VALUE '0093'.
00183      12  ER-0094                 PIC X(4)  VALUE '0094'.
00184      12  ER-0095                 PIC X(4)  VALUE '0095'.
00185      12  ER-0096                 PIC X(4)  VALUE '0096'.
00186      12  ER-0097                 PIC X(4)  VALUE '0097'.
00187      12  ER-0098                 PIC X(4)  VALUE '0098'.
00188      12  ER-0099                 PIC X(4)  VALUE '0099'.
00189      12  ER-0100                 PIC X(4)  VALUE '0100'.
00190      12  ER-0101                 PIC X(4)  VALUE '0101'.
00191      12  ER-0102                 PIC X(4)  VALUE '0102'.
00192      12  ER-0103                 PIC X(4)  VALUE '0103'.
00193      12  ER-0104                 PIC X(4)  VALUE '0104'.
00194      12  ER-0105                 PIC X(4)  VALUE '0105'.
00195      12  ER-0106                 PIC X(4)  VALUE '0106'.
00196      12  ER-0107                 PIC X(4)  VALUE '0107'.
00197      12  ER-0108                 PIC X(4)  VALUE '0108'.
00198      12  ER-0109                 PIC X(4)  VALUE '0109'.
00199      12  ER-0110                 PIC X(4)  VALUE '0110'.
00200      12  ER-0111                 PIC X(4)  VALUE '0111'.
00201      12  ER-0112                 PIC X(4)  VALUE '0112'.
00202      12  ER-0113                 PIC X(4)  VALUE '0113'.
00203      12  ER-0114                 PIC X(4)  VALUE '0114'.
00204      12  ER-0117                 PIC X(4)  VALUE '0117'.
00205      12  ER-0118                 PIC X(4)  VALUE '0118'.
00206      12  ER-0119                 PIC X(4)  VALUE '0119'.
00207      12  ER-0120                 PIC X(4)  VALUE '0120'.
00208      12  ER-0121                 PIC X(4)  VALUE '0121'.
00209      12  ER-0122                 PIC X(4)  VALUE '0122'.
00210      12  ER-0123                 PIC X(4)  VALUE '0123'.
00211      12  ER-0124                 PIC X(4)  VALUE '0124'.
00212      12  ER-0125                 PIC X(4)  VALUE '0125'.
00213      12  ER-0145                 PIC X(4)  VALUE '0145'.
00214      12  ER-0173                 PIC X(4)  VALUE '0173'.
00215      12  ER-0193                 PIC X(4)  VALUE '0193'.
00216      12  ER-0497                 PIC X(4)  VALUE '0497'.
00217      12  ER-0529                 PIC X(4)  VALUE '0529'.
00218      12  ER-0637                 PIC X(4)  VALUE '0637'.
00219      12  ER-0638                 PIC X(4)  VALUE '0638'.
112103     12  ER-1778                 PIC X(4)  VALUE '1778'.
00220      12  ER-2010                 PIC X(4)  VALUE '2010'.
00221      12  ER-2014                 PIC X(4)  VALUE '2014'.
00222      12  ER-2308                 PIC X(4)  VALUE '2308'.
112103     12  ER-3270                 PIC X(4)  VALUE '3270'.
00223      12  ER-7008                 PIC X(4)  VALUE '7008'.
00224      12  ER-7532                 PIC X(4)  VALUE '7532'.
00225      12  ER-8017                 PIC X(4)  VALUE '8017'.
00226      12  ER-8127                 PIC X(4)  VALUE '8127'.
00227      12  ER-8128                 PIC X(4)  VALUE '8128'.
00228
00229      EJECT
00230 *                                    COPY ELCINTF.
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
00231      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.
00232          16  PI-1ST-TIME-SW          PIC S9    COMP-3.
00233          16  PI-MODE                 PIC X.
00234          16  PI-CARRIER-NUMBER       PIC X.
00235          16  PI-NEXT-CARRIER-NUMBER  PIC X.
00236          16  PI-LINE-COUNT           PIC S9(3) COMP-3.
00237          16  PI-BROWSE-SW            PIC S9    COMP-3.
00238          16  PI-END-OF-FILE          PIC S9    COMP-3.
00239          16  PI-PREV-MODE            PIC X.
00240          16  PI-PREV-CARRIER         PIC X.
00241          16  FILLER                  PIC X(630).
00242
00243      EJECT
00244 *                                    COPY ELCJPFX.
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
00245                                      PIC X(750).
00246      EJECT
00247 *                                    COPY ELCEMIB.
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
00248      EJECT
00249 *                                    COPY ELCDATE.
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
00250      EJECT
00251 *                                    COPY EL105S.
       01  EL105AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  ADATEL PIC S9(0004) COMP.
           05  ADATEF PIC  X(0001).
           05  FILLER REDEFINES ADATEF.
               10  ADATEA PIC  X(0001).
           05  ADATEI PIC  X(0008).
      *    -------------------------------
           05  ATIMEL PIC S9(0004) COMP.
           05  ATIMEF PIC  X(0001).
           05  FILLER REDEFINES ATIMEF.
               10  ATIMEA PIC  X(0001).
           05  ATIMEI PIC  X(0005).
      *    -------------------------------
           05  AMAINTL PIC S9(0004) COMP.
           05  AMAINTF PIC  X(0001).
           05  FILLER REDEFINES AMAINTF.
               10  AMAINTA PIC  X(0001).
           05  AMAINTI PIC  X(0001).
      *    -------------------------------
           05  ACARIERL PIC S9(0004) COMP.
           05  ACARIERF PIC  X(0001).
           05  FILLER REDEFINES ACARIERF.
               10  ACARIERA PIC  X(0001).
           05  ACARIERI PIC  X(0001).
      *    -------------------------------
           05  ACONAMEL PIC S9(0004) COMP.
           05  ACONAMEF PIC  X(0001).
           05  FILLER REDEFINES ACONAMEF.
               10  ACONAMEA PIC  X(0001).
           05  ACONAMEI PIC  X(0030).
      *    -------------------------------
           05  FD1L PIC S9(0004) COMP.
           05  FD1F PIC  X(0001).
           05  FILLER REDEFINES FD1F.
               10  FD1A PIC  X(0001).
           05  FD1I PIC  X(0019).
      *    -------------------------------
           05  ALPHLL PIC S9(0004) COMP.
           05  ALPHLF PIC  X(0001).
           05  FILLER REDEFINES ALPHLF.
               10  ALPHLA PIC  X(0001).
           05  ALPHLI PIC  X(0006).
      *    -------------------------------
           05  ACAREOFL PIC S9(0004) COMP.
           05  ACAREOFF PIC  X(0001).
           05  FILLER REDEFINES ACAREOFF.
               10  ACAREOFA PIC  X(0001).
           05  ACAREOFI PIC  X(0030).
      *    -------------------------------
           05  FD2L PIC S9(0004) COMP.
           05  FD2F PIC  X(0001).
           05  FILLER REDEFINES FD2F.
               10  FD2A PIC  X(0001).
           05  FD2I PIC  X(0008).
      *    -------------------------------
           05  ACLAIML PIC S9(0004) COMP.
           05  ACLAIMF PIC  X(0001).
           05  FILLER REDEFINES ACLAIMF.
               10  ACLAIMA PIC  X(0001).
           05  ACLAIMI PIC  X(0008).
      *    -------------------------------
           05  ALPHCHL PIC S9(0004) COMP.
           05  ALPHCHF PIC  X(0001).
           05  FILLER REDEFINES ALPHCHF.
               10  ALPHCHA PIC  X(0001).
           05  ALPHCHI PIC  X(0001).
      *    -------------------------------
           05  AADDR1L PIC S9(0004) COMP.
           05  AADDR1F PIC  X(0001).
           05  FILLER REDEFINES AADDR1F.
               10  AADDR1A PIC  X(0001).
           05  AADDR1I PIC  X(0030).
      *    -------------------------------
           05  FD3L PIC S9(0004) COMP.
           05  FD3F PIC  X(0001).
           05  FILLER REDEFINES FD3F.
               10  FD3A PIC  X(0001).
           05  FD3I PIC  X(0008).
      *    -------------------------------
           05  ACHECKL PIC S9(0004) COMP.
           05  ACHECKF PIC  X(0001).
           05  FILLER REDEFINES ACHECKF.
               10  ACHECKA PIC  X(0001).
           05  ACHECKI PIC  X(0008).
      *    -------------------------------
           05  AADDR2L PIC S9(0004) COMP.
           05  AADDR2F PIC  X(0001).
           05  FILLER REDEFINES AADDR2F.
               10  AADDR2A PIC  X(0001).
           05  AADDR2I PIC  X(0030).
      *    -------------------------------
           05  APHONEL PIC S9(0004) COMP.
           05  APHONEF PIC  X(0001).
           05  FILLER REDEFINES APHONEF.
               10  APHONEA PIC  X(0001).
           05  APHONEI PIC  S9(12).
      *    -------------------------------
           05  ACITYSTL PIC S9(0004) COMP.
           05  ACITYSTF PIC  X(0001).
           05  FILLER REDEFINES ACITYSTF.
               10  ACITYSTA PIC  X(0001).
           05  ACITYSTI PIC  X(0030).
      *    -------------------------------
           05  AZIPL PIC S9(0004) COMP.
           05  AZIPF PIC  X(0001).
           05  FILLER REDEFINES AZIPF.
               10  AZIPA PIC  X(0001).
           05  AZIPI PIC  X(0010).
      *    -------------------------------
           05  ADOMSTL PIC S9(0004) COMP.
           05  ADOMSTF PIC  X(0001).
           05  FILLER REDEFINES ADOMSTF.
               10  ADOMSTA PIC  X(0001).
           05  ADOMSTI PIC  X(0002).
      *    -------------------------------
           05  ANXTAUDL PIC S9(0004) COMP.
           05  ANXTAUDF PIC  X(0001).
           05  FILLER REDEFINES ANXTAUDF.
               10  ANXTAUDA PIC  X(0001).
           05  ANXTAUDI PIC  X(0008).
      *    -------------------------------
           05  FD4L PIC S9(0004) COMP.
           05  FD4F PIC  X(0001).
           05  FILLER REDEFINES FD4F.
               10  FD4A PIC  X(0001).
           05  FD4I PIC  X(0032).
      *    -------------------------------
           05  ACLNAML PIC S9(0004) COMP.
           05  ACLNAMF PIC  X(0001).
           05  FILLER REDEFINES ACLNAMF.
               10  ACLNAMA PIC  X(0001).
           05  ACLNAMI PIC  X(0001).
      *    -------------------------------
           05  FD5L PIC S9(0004) COMP.
           05  FD5F PIC  X(0001).
           05  FILLER REDEFINES FD5F.
               10  FD5A PIC  X(0001).
           05  FD5I PIC  X(0018).
      *    -------------------------------
           05  ALAL PIC S9(0004) COMP.
           05  ALAF PIC  X(0001).
           05  FILLER REDEFINES ALAF.
               10  ALAA PIC  X(0001).
           05  ALAI PIC  X(0001).
      *    -------------------------------
           05  FD6L PIC S9(0004) COMP.
           05  FD6F PIC  X(0001).
           05  FILLER REDEFINES FD6F.
               10  FD6A PIC  X(0001).
           05  FD6I PIC  X(0032).
      *    -------------------------------
           05  ACKNAML PIC S9(0004) COMP.
           05  ACKNAMF PIC  X(0001).
           05  FILLER REDEFINES ACKNAMF.
               10  ACKNAMA PIC  X(0001).
           05  ACKNAMI PIC  X(0001).
      *    -------------------------------
           05  FD7L PIC S9(0004) COMP.
           05  FD7F PIC  X(0001).
           05  FILLER REDEFINES FD7F.
               10  FD7A PIC  X(0001).
           05  FD7I PIC  X(0012).
      *    -------------------------------
           05  ACDTAL PIC S9(0004) COMP.
           05  ACDTAF PIC  X(0001).
           05  FILLER REDEFINES ACDTAF.
               10  ACDTAA PIC  X(0001).
           05  ACDTAI PIC  X(0001).
      *    -------------------------------
           05  FD8L PIC S9(0004) COMP.
           05  FD8F PIC  X(0001).
           05  FILLER REDEFINES FD8F.
               10  FD8A PIC  X(0001).
           05  FD8I PIC  X(0026).
      *    -------------------------------
           05  ACLCML PIC S9(0004) COMP.
           05  ACLCMF PIC  X(0001).
           05  FILLER REDEFINES ACLCMF.
               10  ACLCMA PIC  X(0001).
           05  ACLCMI PIC  X(0001).
      *    -------------------------------
           05  FD9L PIC S9(0004) COMP.
           05  FD9F PIC  X(0001).
           05  FILLER REDEFINES FD9F.
               10  FD9A PIC  X(0001).
           05  FD9I PIC  X(0010).
      *    -------------------------------
           05  APCTCDTL PIC S9(0004) COMP.
           05  APCTCDTF PIC  X(0001).
           05  FILLER REDEFINES APCTCDTF.
               10  APCTCDTA PIC  X(0001).
           05  APCTCDTI PIC  S9(5)V99.
      *    -------------------------------
           05  FD9AL PIC S9(0004) COMP.
           05  FD9AF PIC  X(0001).
           05  FILLER REDEFINES FD9AF.
               10  FD9AA PIC  X(0001).
           05  FD9AI PIC  X(0008).
      *    -------------------------------
           05  IBNRPCTL PIC S9(0004) COMP.
           05  IBNRPCTF PIC  X(0001).
           05  FILLER REDEFINES IBNRPCTF.
               10  IBNRPCTA PIC  X(0001).
           05  IBNRPCTI PIC  S999V9(4).
      *    -------------------------------
           05  FD10L PIC S9(0004) COMP.
           05  FD10F PIC  X(0001).
           05  FILLER REDEFINES FD10F.
               10  FD10A PIC  X(0001).
           05  FD10I PIC  X(0028).
      *    -------------------------------
           05  AEXPCML PIC S9(0004) COMP.
           05  AEXPCMF PIC  X(0001).
           05  FILLER REDEFINES AEXPCMF.
               10  AEXPCMA PIC  X(0001).
           05  AEXPCMI PIC  X(0001).
      *    -------------------------------
           05  FD11L PIC S9(0004) COMP.
           05  FD11F PIC  X(0001).
           05  FILLER REDEFINES FD11F.
               10  FD11A PIC  X(0001).
           05  FD11I PIC  X(0009).
      *    -------------------------------
           05  AEXPCPL PIC S9(0004) COMP.
           05  AEXPCPF PIC  X(0001).
           05  FILLER REDEFINES AEXPCPF.
               10  AEXPCPA PIC  X(0001).
           05  AEXPCPI PIC  S9(5)V99.
      *    -------------------------------
           05  FD12L PIC S9(0004) COMP.
           05  FD12F PIC  X(0001).
           05  FILLER REDEFINES FD12F.
               10  FD12A PIC  X(0001).
           05  FD12I PIC  X(0008).
      *    -------------------------------
           05  AEXPCAL PIC S9(0004) COMP.
           05  AEXPCAF PIC  X(0001).
           05  FILLER REDEFINES AEXPCAF.
               10  AEXPCAA PIC  X(0001).
           05  AEXPCAI PIC  S9(5)V99.
      *    -------------------------------
           05  ABRETRL PIC S9(0004) COMP.
           05  ABRETRF PIC  X(0001).
           05  FILLER REDEFINES ABRETRF.
               10  ABRETRA PIC  X(0001).
           05  ABRETRI PIC  99.
      *    -------------------------------
           05  FD13L PIC S9(0004) COMP.
           05  FD13F PIC  X(0001).
           05  FILLER REDEFINES FD13F.
               10  FD13A PIC  X(0001).
           05  FD13I PIC  X(0016).
      *    -------------------------------
           05  ARESMANL PIC S9(0004) COMP.
           05  ARESMANF PIC  X(0001).
           05  FILLER REDEFINES ARESMANF.
               10  ARESMANA PIC  X(0001).
           05  ARESMANI PIC  X(0001).
      *    -------------------------------
           05  FD14L PIC S9(0004) COMP.
           05  FD14F PIC  X(0001).
           05  FILLER REDEFINES FD14F.
               10  FD14A PIC  X(0001).
           05  FD14I PIC  X(0011).
      *    -------------------------------
           05  ARESCDTL PIC S9(0004) COMP.
           05  ARESCDTF PIC  X(0001).
           05  FILLER REDEFINES ARESCDTF.
               10  ARESCDTA PIC  X(0001).
           05  ARESCDTI PIC  X(0001).
      *    -------------------------------
           05  FD15L PIC S9(0004) COMP.
           05  FD15F PIC  X(0001).
           05  FILLER REDEFINES FD15F.
               10  FD15A PIC  X(0001).
           05  FD15I PIC  X(0010).
      *    -------------------------------
           05  ARESIBNL PIC S9(0004) COMP.
           05  ARESIBNF PIC  X(0001).
           05  FILLER REDEFINES ARESIBNF.
               10  ARESIBNA PIC  X(0001).
           05  ARESIBNI PIC  X(0001).
      *    -------------------------------
           05  FD16L PIC S9(0004) COMP.
           05  FD16F PIC  X(0001).
           05  FILLER REDEFINES FD16F.
               10  FD16A PIC  X(0001).
           05  FD16I PIC  X(0004).
      *    -------------------------------
           05  ARESPTCL PIC S9(0004) COMP.
           05  ARESPTCF PIC  X(0001).
           05  FILLER REDEFINES ARESPTCF.
               10  ARESPTCA PIC  X(0001).
           05  ARESPTCI PIC  X(0001).
      *    -------------------------------
           05  FD17L PIC S9(0004) COMP.
           05  FD17F PIC  X(0001).
           05  FILLER REDEFINES FD17F.
               10  FD17A PIC  X(0001).
           05  FD17I PIC  X(0014).
      *    -------------------------------
           05  FD17AL PIC S9(0004) COMP.
           05  FD17AF PIC  X(0001).
           05  FILLER REDEFINES FD17AF.
               10  FD17AA PIC  X(0001).
           05  FD17AI PIC  X(0028).
      *    -------------------------------
           05  AUEPPCTL PIC S9(0004) COMP.
           05  AUEPPCTF PIC  X(0001).
           05  FILLER REDEFINES AUEPPCTF.
               10  AUEPPCTA PIC  X(0001).
           05  AUEPPCTI PIC  S999V9(4).
      *    -------------------------------
           05  FD17BL PIC S9(0004) COMP.
           05  FD17BF PIC  X(0001).
           05  FILLER REDEFINES FD17BF.
               10  FD17BA PIC  X(0001).
           05  FD17BI PIC  X(0006).
      *    -------------------------------
           05  AR78PCTL PIC S9(0004) COMP.
           05  AR78PCTF PIC  X(0001).
           05  FILLER REDEFINES AR78PCTF.
               10  AR78PCTA PIC  X(0001).
           05  AR78PCTI PIC  S999V9(4).
      *    -------------------------------
           05  FD17CL PIC S9(0004) COMP.
           05  FD17CF PIC  X(0001).
           05  FILLER REDEFINES FD17CF.
               10  FD17CA PIC  X(0001).
           05  FD17CI PIC  X(0006).
      *    -------------------------------
           05  APROPCTL PIC S9(0004) COMP.
           05  APROPCTF PIC  X(0001).
           05  FILLER REDEFINES APROPCTF.
               10  APROPCTA PIC  X(0001).
           05  APROPCTI PIC  S999V9(4).
      *    -------------------------------
           05  FD18L PIC S9(0004) COMP.
           05  FD18F PIC  X(0001).
           05  FILLER REDEFINES FD18F.
               10  FD18A PIC  X(0001).
           05  FD18I PIC  X(0027).
      *    -------------------------------
           05  ALQCAL PIC S9(0004) COMP.
           05  ALQCAF PIC  X(0001).
           05  FILLER REDEFINES ALQCAF.
               10  ALQCAA PIC  X(0001).
           05  ALQCAI PIC  S9(5)V99.
      *    -------------------------------
           05  FD19L PIC S9(0004) COMP.
           05  FD19F PIC  X(0001).
           05  FILLER REDEFINES FD19F.
               10  FD19A PIC  X(0001).
           05  FD19I PIC  X(0026).
      *    -------------------------------
           05  ALMRPL PIC S9(0004) COMP.
           05  ALMRPF PIC  X(0001).
           05  FILLER REDEFINES ALMRPF.
               10  ALMRPA PIC  X(0001).
           05  ALMRPI PIC  S9(11)V99.
      *    -------------------------------
           05  FD20L PIC S9(0004) COMP.
           05  FD20F PIC  X(0001).
           05  FILLER REDEFINES FD20F.
               10  FD20A PIC  X(0001).
           05  FD20I PIC  X(0018).
      *    -------------------------------
           05  ALQCDL PIC S9(0004) COMP.
           05  ALQCDF PIC  X(0001).
           05  FILLER REDEFINES ALQCDF.
               10  ALQCDA PIC  X(0001).
           05  ALQCDI PIC  S9(4).
      *    -------------------------------
           05  FD21L PIC S9(0004) COMP.
           05  FD21F PIC  X(0001).
           05  FILLER REDEFINES FD21F.
               10  FD21A PIC  X(0001).
           05  FD21I PIC  X(0026).
      *    -------------------------------
           05  ALMDPPL PIC S9(0004) COMP.
           05  ALMDPPF PIC  X(0001).
           05  FILLER REDEFINES ALMDPPF.
               10  ALMDPPA PIC  X(0001).
           05  ALMDPPI PIC  S9(4).
      *    -------------------------------
           05  FD22L PIC S9(0004) COMP.
           05  FD22F PIC  X(0001).
           05  FILLER REDEFINES FD22F.
               10  FD22A PIC  X(0001).
           05  FD22I PIC  X(0020).
      *    -------------------------------
           05  ALDBCL PIC S9(0004) COMP.
           05  ALDBCF PIC  X(0001).
           05  FILLER REDEFINES ALDBCF.
               10  ALDBCA PIC  X(0001).
           05  ALDBCI PIC  S9(4).
      *    -------------------------------
           05  FD23L PIC S9(0004) COMP.
           05  FD23F PIC  X(0001).
           05  FILLER REDEFINES FD23F.
               10  FD23A PIC  X(0001).
           05  FD23I PIC  X(0027).
      *    -------------------------------
           05  ALMAPL PIC S9(0004) COMP.
           05  ALMAPF PIC  X(0001).
           05  FILLER REDEFINES ALMAPF.
               10  ALMAPA PIC  X(0001).
           05  ALMAPI PIC  S9(11)V99.
      *    -------------------------------
           05  FD24L PIC S9(0004) COMP.
           05  FD24F PIC  X(0001).
           05  FILLER REDEFINES FD24F.
               10  FD24A PIC  X(0001).
           05  FD24I PIC  X(0022).
      *    -------------------------------
           05  ALMBPL PIC S9(0004) COMP.
           05  ALMBPF PIC  X(0001).
           05  FILLER REDEFINES ALMBPF.
               10  ALMBPA PIC  X(0001).
           05  ALMBPI PIC  S9(4).
      *    -------------------------------
           05  FD25L PIC S9(0004) COMP.
           05  FD25F PIC  X(0001).
           05  FILLER REDEFINES FD25F.
               10  FD25A PIC  X(0001).
           05  FD25I PIC  X(0025).
      *    -------------------------------
           05  ALMAPML PIC S9(0004) COMP.
           05  ALMAPMF PIC  X(0001).
           05  FILLER REDEFINES ALMAPMF.
               10  ALMAPMA PIC  X(0001).
           05  ALMAPMI PIC  S9(4).
      *    -------------------------------
           05  AEMSG1L PIC S9(0004) COMP.
           05  AEMSG1F PIC  X(0001).
           05  FILLER REDEFINES AEMSG1F.
               10  AEMSG1A PIC  X(0001).
           05  AEMSG1I PIC  X(0079).
      *    -------------------------------
           05  APFKL PIC S9(0004) COMP.
           05  APFKF PIC  X(0001).
           05  FILLER REDEFINES APFKF.
               10  APFKA PIC  X(0001).
           05  APFKI PIC  99.
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
           05  ALUTIMEI PIC  X(0008).
      *    -------------------------------
           05  ALUBYL PIC S9(0004) COMP.
           05  ALUBYF PIC  X(0001).
           05  FILLER REDEFINES ALUBYF.
               10  ALUBYA PIC  X(0001).
           05  ALUBYI PIC  X(0004).
       01  EL105AO REDEFINES EL105AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ATIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACONAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD1O PIC  X(0019).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALPHLO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACAREOFO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD2O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLAIMO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALPHCHO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD3O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACHECKO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADOMSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ANXTAUDO PIC  9(8).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD4O PIC  X(0032).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLNAMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD5O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALAO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD6O PIC  X(0032).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACKNAMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD7O PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACDTAO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD8O PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACLCMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD9O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APCTCDTO PIC  ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD9AO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  IBNRPCTO PIC  Z.9(4)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD10O PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEXPCMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD11O PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEXPCPO PIC  ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD12O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEXPCAO PIC  ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ABRETRO PIC  Z9.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD13O PIC  X(0016).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARESMANO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD14O PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARESCDTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD15O PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARESIBNO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD16O PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARESPTCO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD17O PIC  X(0014).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD17AO PIC  X(0028).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AUEPPCTO PIC  Z.9(4)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD17BO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AR78PCTO PIC  Z.9(4)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD17CO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APROPCTO PIC  Z.9(4)-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD18O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALQCAO PIC  ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD19O PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALMRPO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD20O PIC  X(0018).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALQCDO PIC  ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD21O PIC  X(0026).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALMDPPO PIC  ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD22O PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALDBCO PIC  ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD23O PIC  X(0027).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALMAPO PIC  Z,ZZZ,ZZ9.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD24O PIC  X(0022).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALMBPO PIC  ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FD25O PIC  X(0025).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALMAPMO PIC  ZZ9-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  AEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  APFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUTIMEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ALUBYO PIC  X(0004).
      *    -------------------------------
       01  EL105BI REDEFINES EL105AI.
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
           05  BMAINTL PIC S9(0004) COMP.
           05  BMAINTF PIC  X(0001).
           05  FILLER REDEFINES BMAINTF.
               10  BMAINTA PIC  X(0001).
           05  BMAINTI PIC  X(0001).
      *    -------------------------------
           05  BCARIERL PIC S9(0004) COMP.
           05  BCARIERF PIC  X(0001).
           05  FILLER REDEFINES BCARIERF.
               10  BCARIERA PIC  X(0001).
           05  BCARIERI PIC  X(0001).
      *    -------------------------------
           05  BSPLABLL PIC S9(0004) COMP.
           05  BSPLABLF PIC  X(0001).
           05  FILLER REDEFINES BSPLABLF.
               10  BSPLABLA PIC  X(0001).
           05  BSPLABLI PIC  X(0011).
      *    -------------------------------
           05  BSECPAYL PIC S9(0004) COMP.
           05  BSECPAYF PIC  X(0001).
           05  FILLER REDEFINES BSECPAYF.
               10  BSECPAYA PIC  X(0001).
           05  BSECPAYI PIC  X(0001).
      *    -------------------------------
           05  BCONAMEL PIC S9(0004) COMP.
           05  BCONAMEF PIC  X(0001).
           05  FILLER REDEFINES BCONAMEF.
               10  BCONAMEA PIC  X(0001).
           05  BCONAMEI PIC  X(0030).
      *    -------------------------------
           05  BCAREOFL PIC S9(0004) COMP.
           05  BCAREOFF PIC  X(0001).
           05  FILLER REDEFINES BCAREOFF.
               10  BCAREOFA PIC  X(0001).
           05  BCAREOFI PIC  X(0030).
      *    -------------------------------
           05  BADDR1L PIC S9(0004) COMP.
           05  BADDR1F PIC  X(0001).
           05  FILLER REDEFINES BADDR1F.
               10  BADDR1A PIC  X(0001).
           05  BADDR1I PIC  X(0030).
      *    -------------------------------
           05  BADDR2L PIC S9(0004) COMP.
           05  BADDR2F PIC  X(0001).
           05  FILLER REDEFINES BADDR2F.
               10  BADDR2A PIC  X(0001).
           05  BADDR2I PIC  X(0030).
      *    -------------------------------
           05  BPHONEL PIC S9(0004) COMP.
           05  BPHONEF PIC  X(0001).
           05  FILLER REDEFINES BPHONEF.
               10  BPHONEA PIC  X(0001).
           05  BPHONEI PIC  S9(12).
      *    -------------------------------
           05  BCITYSTL PIC S9(0004) COMP.
           05  BCITYSTF PIC  X(0001).
           05  FILLER REDEFINES BCITYSTF.
               10  BCITYSTA PIC  X(0001).
           05  BCITYSTI PIC  X(0030).
      *    -------------------------------
           05  BZIPL PIC S9(0004) COMP.
           05  BZIPF PIC  X(0001).
           05  FILLER REDEFINES BZIPF.
               10  BZIPA PIC  X(0001).
           05  BZIPI PIC  X(0010).
      *    -------------------------------
           05  BDOMSTL PIC S9(0004) COMP.
           05  BDOMSTF PIC  X(0001).
           05  FILLER REDEFINES BDOMSTF.
               10  BDOMSTA PIC  X(0001).
           05  BDOMSTI PIC  X(0002).
      *    -------------------------------
           05  BCTLABLL PIC S9(0004) COMP.
           05  BCTLABLF PIC  X(0001).
           05  FILLER REDEFINES BCTLABLF.
               10  BCTLABLA PIC  X(0001).
           05  BCTLABLI PIC  X(0009).
      *    -------------------------------
           05  BCLPTOLL PIC S9(0004) COMP.
           05  BCLPTOLF PIC  X(0001).
           05  FILLER REDEFINES BCLPTOLF.
               10  BCLPTOLA PIC  X(0001).
           05  BCLPTOLI PIC  S9(3)V9(4).
      *    -------------------------------
           05  BLCLABLL PIC S9(0004) COMP.
           05  BLCLABLF PIC  X(0001).
           05  FILLER REDEFINES BLCLABLF.
               10  BLCLABLA PIC  X(0001).
           05  BLCLABLI PIC  X(0011).
      *    -------------------------------
           05  BLCOMML PIC S9(0004) COMP.
           05  BLCOMMF PIC  X(0001).
           05  FILLER REDEFINES BLCOMMF.
               10  BLCOMMA PIC  X(0001).
           05  BLCOMMI PIC  S9(6)V9(2).
      *    -------------------------------
           05  BPRMTOLL PIC S9(0004) COMP.
           05  BPRMTOLF PIC  X(0001).
           05  FILLER REDEFINES BPRMTOLF.
               10  BPRMTOLA PIC  X(0001).
           05  BPRMTOLI PIC  S9(4)V99.
      *    -------------------------------
           05  BREFTOLL PIC S9(0004) COMP.
           05  BREFTOLF PIC  X(0001).
           05  FILLER REDEFINES BREFTOLF.
               10  BREFTOLA PIC  X(0001).
           05  BREFTOLI PIC  S9(4)V99.
      *    -------------------------------
           05  BOVSAMTL PIC S9(0004) COMP.
           05  BOVSAMTF PIC  X(0001).
           05  FILLER REDEFINES BOVSAMTF.
               10  BOVSAMTA PIC  X(0001).
           05  BOVSAMTI PIC  S9(4)V99.
      *    -------------------------------
           05  BPRMPCTL PIC S9(0004) COMP.
           05  BPRMPCTF PIC  X(0001).
           05  FILLER REDEFINES BPRMPCTF.
               10  BPRMPCTA PIC  X(0001).
           05  BPRMPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  BREFPCTL PIC S9(0004) COMP.
           05  BREFPCTF PIC  X(0001).
           05  FILLER REDEFINES BREFPCTF.
               10  BREFPCTA PIC  X(0001).
           05  BREFPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  BOVSPCTL PIC S9(0004) COMP.
           05  BOVSPCTF PIC  X(0001).
           05  FILLER REDEFINES BOVSPCTF.
               10  BOVSPCTA PIC  X(0001).
           05  BOVSPCTI PIC  S9(1)V9(4).
      *    -------------------------------
           05  DMDSW2L PIC S9(0004) COMP.
           05  DMDSW2F PIC  X(0001).
           05  FILLER REDEFINES DMDSW2F.
               10  DMDSW2A PIC  X(0001).
           05  DMDSW2I PIC  X(0020).
      *    -------------------------------
           05  BCLCPRML PIC S9(0004) COMP.
           05  BCLCPRMF PIC  X(0001).
           05  FILLER REDEFINES BCLCPRMF.
               10  BCLCPRMA PIC  X(0001).
           05  BCLCPRMI PIC  X(0001).
      *    -------------------------------
           05  BEMSG1L PIC S9(0004) COMP.
           05  BEMSG1F PIC  X(0001).
           05  FILLER REDEFINES BEMSG1F.
               10  BEMSG1A PIC  X(0001).
           05  BEMSG1I PIC  X(0079).
      *    -------------------------------
           05  BPFKL PIC S9(0004) COMP.
           05  BPFKF PIC  X(0001).
           05  FILLER REDEFINES BPFKF.
               10  BPFKA PIC  X(0001).
           05  BPFKI PIC  99.
      *    -------------------------------
           05  BLUDATEL PIC S9(0004) COMP.
           05  BLUDATEF PIC  X(0001).
           05  FILLER REDEFINES BLUDATEF.
               10  BLUDATEA PIC  X(0001).
           05  BLUDATEI PIC  X(0008).
      *    -------------------------------
           05  BLUTIMEL PIC S9(0004) COMP.
           05  BLUTIMEF PIC  X(0001).
           05  FILLER REDEFINES BLUTIMEF.
               10  BLUTIMEA PIC  X(0001).
           05  BLUTIMEI PIC  X(0008).
      *    -------------------------------
           05  BLUBYL PIC S9(0004) COMP.
           05  BLUBYF PIC  X(0001).
           05  FILLER REDEFINES BLUBYF.
               10  BLUBYA PIC  X(0001).
           05  BLUBYI PIC  X(0004).
       01  EL105BO REDEFINES EL105AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BMAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCARIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSPLABLO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BSECPAYO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCONAMEO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCAREOFO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDR1O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BADDR2O PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPHONEO PIC  999B999B9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCITYSTO PIC  X(0030).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BZIPO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BDOMSTO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCTLABLO PIC  X(0009).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLPTOLO PIC  Z.9999-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLCLABLO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLCOMMO PIC  ZZ99.99-.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRMTOLO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREFTOLO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BOVSAMTO PIC  ZZ9.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPRMPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BREFPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BOVSPCTO PIC  .9999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DMDSW2O PIC  X(0020).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCLCPRMO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BEMSG1O PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BPFKO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLUDATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLUTIMEO PIC  99B99B99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BLUBYO PIC  X(0004).
      *    -------------------------------
00252      EJECT
00253 *                                    COPY ELCLOGOF.
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
00254      EJECT
00255 *                                    COPY ELCATTR.
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
00256      EJECT
00257 *                                    COPY ELCAID.
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
00258  01  FILLER  REDEFINES DFHAID.
00259      05  FILLER                      PIC X(8).
00260      05  PF-VALUES                   PIC X   OCCURS 24.
00261
00262      EJECT
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
00264  01  DFHCOMMAREA                     PIC X(1024).
00265
00266      EJECT
00267 *                                    COPY ELCCNTL.
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
00268      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA CONTROL-FILE.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL105' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00270
00271  0000-MAINLINE SECTION.
00272
00273      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
00274      MOVE '5'                   TO DC-OPTION-CODE.
00275      PERFORM 8500-DATE-CONVERSION.
00276      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.
00277      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.
00278
00279      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00280
00281      IF CREDIT-SESSION
00282          MOVE 'EL105B  '  TO  WS-MAP-NAME
00283      ELSE
00284          MOVE 'EL105A  '  TO  WS-MAP-NAME.
00285
00286 *    NOTE *******************************************************
00287 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *
00288 *         *  FROM ANOTHER MODULE.                               *
00289 *         *******************************************************.
00290
00291      IF EIBCALEN NOT GREATER ZERO
00292          MOVE UNACCESS-MSG       TO  LOGOFF-MSG
00293          GO TO 8300-SEND-TEXT.
00294
00295      
      * EXEC CICS HANDLE CONDITION
00296 *        PGMIDERR (9600-PGMIDERR)
00297 *        NOTOPEN  (8700-NOT-OPEN)
00298 *        DUPREC   (8800-DUPREC)
00299 *        ERROR    (9990-ERROR)
00300 *    END-EXEC.
      *    MOVE '"$LJ%.                ! " #00003670' TO DFHEIV0
           MOVE X'22244C4A252E202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033363730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00301
00302      EJECT
00303  0010-MAIN-LOGIC.
00304      IF PI-CALLING-PROGRAM NOT = THIS-PGM
00305          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
00306              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6
00307              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5
00308              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4
00309              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3
00310              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2
00311              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1
00312              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM
00313              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM
00314            ELSE
00315              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM
00316              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM
00317              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1
00318              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2
00319              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3
00320              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4
00321              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5
00322              MOVE SPACES               TO  PI-SAVED-PROGRAM-6
00323        ELSE
00324          GO TO 0020-MAIN-LOGIC.
00325
00326 *    NOTE *******************************************************
00327 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      *
00328 *         *  INTERFACE BLOCK FOR THIS MODULE.                   *
00329 *         *******************************************************.
00330      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA
00331                                      PI-PREV-MODE
00332                                      PI-PREV-CARRIER.
00333      MOVE ZERO                   TO  PI-1ST-TIME-SW
00334                                      PI-LINE-COUNT
00335                                      PI-BROWSE-SW
00336                                      PI-END-OF-FILE.
00337
00338 *    NOTE *******************************************************
00339 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   *
00340 *         *  FOR EL105.                                         *
00341 *         *******************************************************.
00342      MOVE LOW-VALUES             TO  EL105AI.
00343      MOVE -1                     TO  AMAINTL.
00344      PERFORM 8100-SEND-INITIAL-MAP.
00345
00346      GO TO 9100-RETURN-TRAN.
00347
00348      EJECT
00349  0020-MAIN-LOGIC.
00350 *    NOTE *******************************************************
00351 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *
00352 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *
00353 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *
00354 *         *******************************************************.
00355      IF EIBAID = DFHCLEAR
00356          GO TO 9400-CLEAR.
00357
00358      IF NOT SYSTEM-DISPLAY-CAP
00359          MOVE 'READ'         TO SM-READ
00360          PERFORM 9995-SECURITY-VIOLATION
00361          MOVE ER-0070        TO EMI-ERROR
00362          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00363          PERFORM 8100-SEND-INITIAL-MAP
00364          GO TO 9100-RETURN-TRAN.
00365
00366      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3
00367          MOVE LOW-VALUES         TO  EL105AI
00368          MOVE ER-7008            TO  EMI-ERROR
00369          IF CREDIT-SESSION
00370              MOVE -1                 TO  BPFKL
00371              PERFORM 9900-ERROR-FORMAT
00372              PERFORM 8200-SEND-DATAONLY
00373              GO TO 9100-RETURN-TRAN
00374          ELSE
00375              MOVE -1                 TO  APFKL
00376              PERFORM 9900-ERROR-FORMAT
00377              PERFORM 8200-SEND-DATAONLY
00378              GO TO 9100-RETURN-TRAN.
00379
00380      
      * EXEC CICS RECEIVE
00381 *        INTO   (EL105AI)
00382 *        MAPSET (WS-MAPSET-NAME)
00383 *        MAP    (WS-MAP-NAME)
00384 *    END-EXEC.
           MOVE LENGTH OF
            EL105AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00003755' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL105AI, 
                 DFHEIV11, 
                 WS-MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00385
00386      IF NOT CREDIT-SESSION
00387          IF APFKL GREATER ZERO
00388             IF EIBAID NOT = DFHENTER
00389                 MOVE ER-0004           TO  EMI-ERROR
00390                 MOVE AL-UNBON          TO  APFKA
00391                 MOVE -1                TO  APFKL
00392                 PERFORM 9900-ERROR-FORMAT
00393                 PERFORM 8200-SEND-DATAONLY
00394                 GO TO 9100-RETURN-TRAN
00395             ELSE
00396              IF APFKO GREATER ZERO AND LESS '25'
00397                  MOVE PF-VALUES (APFKI)  TO  EIBAID
00398                ELSE
00399                  MOVE ER-0029            TO  EMI-ERROR
00400                  MOVE AL-UNBOF           TO  APFKA
00401                  MOVE -1                 TO  APFKL
00402                  PERFORM 9900-ERROR-FORMAT
00403                  PERFORM 8200-SEND-DATAONLY
00404                  GO TO 9100-RETURN-TRAN
00405          ELSE
00406             MOVE AL-UNNOF            TO APFKA.
00407
00408      IF CREDIT-SESSION
00409          IF BPFKL GREATER ZERO
00410             IF EIBAID NOT = DFHENTER
00411                 MOVE ER-0004           TO  EMI-ERROR
00412                 MOVE AL-UNBON          TO  BPFKA
00413                 MOVE -1                TO  BPFKL
00414                 PERFORM 9900-ERROR-FORMAT
00415                 PERFORM 8200-SEND-DATAONLY
00416                 GO TO 9100-RETURN-TRAN
00417             ELSE
00418              IF BPFKO GREATER ZERO AND LESS '25'
00419                  MOVE PF-VALUES (BPFKI)  TO  EIBAID
00420                ELSE
00421                  MOVE ER-0029            TO  EMI-ERROR
00422                  MOVE AL-UNBOF           TO  BPFKA
00423                  MOVE -1                 TO  BPFKL
00424                  PERFORM 9900-ERROR-FORMAT
00425                  PERFORM 8200-SEND-DATAONLY
00426                  GO TO 9100-RETURN-TRAN
00427          ELSE
00428             MOVE AL-UNNOF            TO BPFKA.
00429
00430      IF EIBAID = DFHPF12
00431          MOVE 'EL010   '         TO  THIS-PGM
00432          GO TO 9300-XCTL.
00433
00434      IF EIBAID = DFHPF23
00435          GO TO 9000-RETURN-CICS.
00436
00437      IF EIBAID = DFHPF24
00438          IF  CREDIT-SESSION
00439              MOVE XCTL-EL626     TO THIS-PGM
00440              GO TO 9300-XCTL
00441          ELSE
00442              IF  CLAIM-SESSION
00443                  MOVE XCTL-EL126 TO THIS-PGM
00444                  GO TO 9300-XCTL
00445              ELSE
00446                  IF  MORTGAGE-SESSION
00447                      MOVE XCTL-EM626
00448                                  TO THIS-PGM
00449                      GO TO 9300-XCTL
00450                  ELSE
00451                      IF  GENERAL-LEDGER-SESSION
00452                          MOVE XCTL-GL800
00453                                  TO THIS-PGM
00454                          GO TO 9300-XCTL.
00455
00456      IF EIBAID = DFHENTER OR DFHPF1
00457          NEXT SENTENCE
00458        ELSE
00459          MOVE ER-7008               TO  EMI-ERROR
00460         IF CREDIT-SESSION
00461             MOVE -1                    TO  BPFKL
00462             PERFORM 9900-ERROR-FORMAT
00463             PERFORM 8200-SEND-DATAONLY
00464             GO TO 9100-RETURN-TRAN
00465         ELSE
00466             MOVE -1                    TO  APFKL
00467             PERFORM 9900-ERROR-FORMAT
00468             PERFORM 8200-SEND-DATAONLY
00469             GO TO 9100-RETURN-TRAN.
00470
00471      IF EIBAID = DFHPF1
00472        AND PI-END-OF-FILE = +1
00473          MOVE LOW-VALUES         TO  PI-NEXT-CARRIER-NUMBER
00474          MOVE ZERO               TO  PI-BROWSE-SW
00475                                      PI-END-OF-FILE
00476          GO TO 8000-DISPLAY-RECORDS.
00477
00478      EJECT
00479  0025-MAIN-LOGIC.
00480      IF AMAINTL NOT GREATER ZERO
00481        AND EIBAID = DFHPF1
00482          MOVE 'S'                TO  PI-MODE
00483                                      AMAINTO
00484          MOVE AL-UANON           TO  AMAINTA
00485          MOVE SPACES             TO  WS-CONTROL-FILE-KEY
00486          MOVE PI-COMPANY-ID      TO  WS-CFK-COMPANY-ID
00487          MOVE PI-CARRIER-NUMBER  TO  WS-CFK-CARRIER-NO
00488          MOVE '6'                TO  WS-CFK-RECORD-TYPE
00489          GO TO 8000-DISPLAY-RECORDS.
00490
00491      MOVE PI-MODE                TO PI-PREV-MODE.
00492      MOVE PI-CARRIER-NUMBER      TO PI-PREV-CARRIER.
00493
00494      IF AMAINTI = 'S' OR ' '
00495          NEXT SENTENCE
00496         ELSE
00497          IF EIBAID = DFHPF1
00498             MOVE ER-0050               TO  EMI-ERROR
00499             IF CREDIT-SESSION
00500                 MOVE -1                TO  BPFKL
00501                 PERFORM 9900-ERROR-FORMAT
00502             ELSE
00503                 MOVE -1                TO  APFKL
00504                 PERFORM 9900-ERROR-FORMAT.
00505
00506      IF AMAINTL GREATER ZERO
00507          IF AMAINTI = 'A' OR 'C' OR 'D' OR 'S'
00508              MOVE AMAINTI        TO  PI-MODE
00509              MOVE AL-UANON       TO  AMAINTA
00510            ELSE
00511              MOVE AL-UABOF       TO  AMAINTA
00512              MOVE -1             TO  AMAINTL
00513              MOVE ER-0023        TO EMI-ERROR
00514              PERFORM 9900-ERROR-FORMAT
00515        ELSE
00516          IF PI-1ST-TIME-SW NOT = ZERO
00517              NEXT SENTENCE
00518            ELSE
00519              IF EIBAID = DFHPF1
00520                  MOVE 'S'        TO PI-MODE
00521                ELSE
00522                  MOVE AL-UABOF   TO  AMAINTA
00523                  MOVE -1         TO  AMAINTL
00524                  MOVE ER-0023    TO EMI-ERROR
00525                  PERFORM 9900-ERROR-FORMAT.
00526
00527      IF ACARIERL GREATER  ZERO
00528          IF ACARIERI NOT = SPACES
00529              MOVE AL-UANON       TO  ACARIERA
00530              MOVE ACARIERI       TO  PI-CARRIER-NUMBER
00531            ELSE
00532              MOVE AL-UABOF       TO  ACARIERA
00533              MOVE -1             TO  ACARIERL
00534              MOVE ER-0193        TO EMI-ERROR
00535              PERFORM 9900-ERROR-FORMAT
00536        ELSE
00537          IF PI-1ST-TIME-SW NOT = ZERO
00538              NEXT SENTENCE
00539            ELSE
00540              IF EIBAID = DFHPF1
00541                  MOVE LOW-VALUES     TO  PI-NEXT-CARRIER-NUMBER
00542                ELSE
00543                  MOVE AL-UABOF       TO  ACARIERA
00544                  MOVE -1             TO  ACARIERL
00545                  MOVE ER-0193    TO EMI-ERROR
00546                  PERFORM 9900-ERROR-FORMAT.
00547
00548      IF EMI-FATAL-CTR GREATER ZERO
00549          PERFORM 8200-SEND-DATAONLY
00550          GO TO 9100-RETURN-TRAN.
00551
00552      MOVE +1                     TO  PI-1ST-TIME-SW.
00553
00554      IF  PI-MODE NOT = 'S'
00555          MOVE +1                 TO  PI-BROWSE-SW.
00556
00557      EJECT
00558  0100-MAIN-LOGIC.
00559      IF PI-MODE NOT = 'S'
00560          GO TO 0200-MAIN-LOGIC.
00561
00562      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
00563      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00564
00565      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00566      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
00567
00568      IF EIBAID = DFHPF1
00569          MOVE PI-NEXT-CARRIER-NUMBER  TO  WS-CFK-CARRIER-NO
00570          GO TO 8000-DISPLAY-RECORDS.
00571
00572      GO TO 6000-SHOW-RECORD.
00573
00574      EJECT
00575  0200-MAIN-LOGIC.
00576      IF SYSTEM-MODIFY-CAP
00577         NEXT SENTENCE
00578        ELSE
00579         IF PI-MODE = 'A' OR 'C' OR 'D'
00580            MOVE 'UPDATE'              TO SM-READ
00581            PERFORM 9995-SECURITY-VIOLATION
00582            MOVE ER-0070               TO EMI-ERROR
00583            MOVE -1                    TO  AMAINTL
00584            MOVE AL-UABON              TO  AMAINTA
00585            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
00586            PERFORM 8200-SEND-DATAONLY
00587            GO TO 9100-RETURN-TRAN.
00588
00589      IF PI-MODE NOT = 'C'
00590          GO TO 0300-MAIN-LOGIC.
00591
00592      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
00593
00594      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
00595      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
00596      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
00597      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
00598
00599      
      * EXEC CICS READ
00600 *        DATASET (WS-CONTROL-FILE-DSID)
00601 *        RIDFLD  (WS-CONTROL-FILE-KEY)
00602 *        SET     (ADDRESS OF CONTROL-FILE)
00603 *   END-EXEC.
      *    MOVE '&"S        E          (   #00003974' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033393734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00604
00605      MOVE CF-EXPENSE-DOLLAR      TO  WS-EXPENSE-DOLLAR.
00606      MOVE CF-EXPENSE-PERCENT     TO  WS-EXPENSE-PERCENT.
00607
00608      IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC
00609          MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.
00610      IF CF-IBNR-R78-PERCENT NOT NUMERIC
00611          MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.
00612      IF CF-IBNR-PRO-PERCENT NOT NUMERIC
00613          MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.
00614
00615      MOVE CF-IBNR-UEPRM-PERCENT  TO  WS-IBNR-UEP-PCT.
00616      MOVE CF-IBNR-R78-PERCENT    TO  WS-IBNR-R78-PCT.
00617      MOVE CF-IBNR-PRO-PERCENT    TO  WS-IBNR-PRO-PCT.
00618
00619      PERFORM 4000-EDIT-MAP-FIELDS.
00620
00621      IF EMI-FATAL-CTR  GREATER ZERO
00622          PERFORM 8200-SEND-DATAONLY
00623          GO TO 9100-RETURN-TRAN.
00624
00625      PERFORM 5100-CHANGE-RECORD.
00626
00627      MOVE ER-0000                TO EMI-ERROR.
00628      PERFORM 9900-ERROR-FORMAT.
00629      MOVE LOW-VALUES             TO  EL105AO.
00630      MOVE -1                     TO  AMAINTL.
00631      MOVE ZERO                   TO  PI-1ST-TIME-SW.
00632      GO TO 6000-SHOW-RECORD.
00633
00634      EJECT
00635  0300-MAIN-LOGIC.
00636 *    NOTE *******************************************************
00637 *         *          P R O C E S S   T H E   A D D S            *
00638 *         *******************************************************.
00639      IF PI-MODE NOT = 'A'
00640          GO TO 0400-MAIN-LOGIC.
00641
00642      MOVE ZEROS                  TO  WS-IBNR-UEP-PCT
00643                                      WS-IBNR-R78-PCT
00644                                      WS-IBNR-PRO-PCT.
00645
00646      PERFORM 4000-EDIT-MAP-FIELDS.
00647
00648      IF EMI-FATAL-CTR GREATER ZERO
00649          PERFORM 8200-SEND-DATAONLY
00650          GO TO 9100-RETURN-TRAN.
00651
00652      PERFORM 5000-ADD-RECORD.
00653
00654      MOVE ER-0000                TO EMI-ERROR.
00655      PERFORM 9900-ERROR-FORMAT.
00656      MOVE LOW-VALUES             TO  EL105AO.
00657      MOVE -1                     TO  AMAINTL.
00658      PERFORM 8100-SEND-INITIAL-MAP.
00659      MOVE ZERO                   TO  PI-1ST-TIME-SW.
00660      GO TO 9100-RETURN-TRAN.
00661
00662      EJECT
00663  0400-MAIN-LOGIC.
00664 *    NOTE *******************************************************
00665 *         *         P R O C E S S   T H E   D E L E T E S       *
00666 *         *******************************************************.
00667      IF PI-PREV-MODE    = 'S' AND
00668         PI-PREV-CARRIER = PI-CARRIER-NUMBER
00669            PERFORM 5200-DELETE-RECORD
00670         ELSE
00671            MOVE AL-UABOF       TO AMAINTA
00672            MOVE -1             TO AMAINTL
00673            MOVE ER-0145        TO EMI-ERROR
00674            PERFORM 9900-ERROR-FORMAT
00675            PERFORM 8200-SEND-DATAONLY
00676            GO TO 9100-RETURN-TRAN.
00677
00678      MOVE ER-0000                TO EMI-ERROR.
00679      PERFORM 9900-ERROR-FORMAT.
00680      MOVE LOW-VALUES             TO  EL105AO.
00681      PERFORM 8100-SEND-INITIAL-MAP.
00682      MOVE ZERO                   TO  PI-1ST-TIME-SW.
00683      GO TO 9100-RETURN-TRAN.
00684      EJECT
00685  4000-EDIT-MAP-FIELDS SECTION.
112103*    NOTE *******************************************************
112103*         *              EDIT SECURE PAY SWITCH.                *
112103*         *******************************************************.
112103     IF CREDIT-SESSION
062121         IF PI-COMPANY-ID = 'CID' OR 'AHL' or 'FNL'
112103             MOVE AL-SADOF           TO BSPLABLA
112103             MOVE AL-SADOF           TO BSECPAYA
112103         ELSE
112103             IF BSECPAYL GREATER ZERO
112103                 IF BSECPAYI = ' ' OR 'Y' OR 'N'
112103                     MOVE AL-UANON       TO  BSECPAYA
112103                     MOVE BSECPAYI       TO  BSECPAYO
112103                 ELSE
112103                     MOVE ER-3270        TO  EMI-ERROR
112103                     PERFORM 9900-ERROR-FORMAT
112103                     MOVE AL-UNBON       TO  BSECPAYA
112103                     MOVE -1             TO  BSECPAYL
112103                 END-IF
112103             END-IF
112103         END-IF
112103     END-IF.
112103
112103*    IF NOT CREDIT-SESSION
112103*        IF PI-COMPANY-ID = 'CID'
112103*            MOVE AL-SADOF           TO ASPLABLA
112103*            MOVE AL-SADOF           TO ASECPAYA
112103*        ELSE
112103*            IF ASECPAYL GREATER ZERO
112103*                IF ASECPAYI = ' ' OR 'Y' OR 'N'
112103*                    MOVE AL-UANON       TO  ASECPAYA
112103*                    MOVE ASECPAYI       TO  ASECPAYO
112103*                ELSE
112103*                    MOVE ER-3270        TO  EMI-ERROR
112103*                    PERFORM 9900-ERROR-FORMAT
112103*                    MOVE AL-UNBON       TO  ASECPAYA
112103*                    MOVE -1             TO  ASECPAYL
112103*                END-IF
112103*            END-IF
112103*        END-IF
112103*    END-IF.
00704
00686 *    NOTE *******************************************************
00687 *         *              EDIT THE PHONE NUMBER.                 *
00688 *         *******************************************************.
00689      IF CREDIT-SESSION
00690          IF BPHONEL GREATER ZERO
00691              
      * EXEC CICS BIF DEEDIT
00692 *                FIELD  (BPHONEI)
00693 *                LENGTH (APHONE-LENGTH)
00694 *            END-EXEC
      *    MOVE '@"L                   #   #00004107' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00695              IF BPHONEI NUMERIC
00696                  MOVE AL-UNNON       TO  BPHONEA
00697                  MOVE BPHONEI        TO  BPHONEO
00698                  INSPECT BPHONEO CONVERTING ' '    TO '-'
00699              ELSE
00700                  MOVE ER-0053           TO  EMI-ERROR
00701                  PERFORM 9900-ERROR-FORMAT
00702                  MOVE AL-UNBON       TO  BPHONEA
00703                  MOVE -1             TO  BPHONEL.
00704
00705      IF NOT CREDIT-SESSION
00706          IF APHONEL GREATER ZERO
00707              
      * EXEC CICS BIF DEEDIT
00708 *                FIELD  (APHONEI)
00709 *                LENGTH (APHONE-LENGTH)
00710 *            END-EXEC
      *    MOVE '@"L                   #   #00004123' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00711              IF APHONEI NUMERIC
00712                  MOVE AL-UNNON       TO  APHONEA
00713                  MOVE APHONEI        TO  APHONEO
00714                  INSPECT APHONEO CONVERTING SPACES TO '-'
00715              ELSE
00716                  MOVE ER-0053           TO  EMI-ERROR
00717                  PERFORM 9900-ERROR-FORMAT
00718                  MOVE AL-UNBON       TO  APHONEA
00719                  MOVE -1             TO  APHONEL.
00720
00721 *    NOTE *******************************************************
00722 *         *              EDIT THE ZIP CODE.                     *
00723 *         *******************************************************.
00724      IF CREDIT-SESSION
00725          IF BZIPL GREATER ZERO
00726              MOVE AL-UANON          TO  BZIPA.
00727
00728      IF NOT CREDIT-SESSION
00729          IF AZIPL GREATER ZERO
00730              MOVE AL-UANON          TO  AZIPA.
00731
00732 *    NOTE *******************************************************
00733 *         *              EDIT THE DOMICILE STATE CODE.          *
00734 *         *******************************************************.
00735      IF CREDIT-SESSION
00736          IF BDOMSTL GREATER ZERO
00737             IF BDOMSTI ALPHABETIC-UPPER
00738                 MOVE AL-UNNON       TO  BDOMSTA
00739              ELSE
00740                 MOVE AL-UNBON       TO  BDOMSTA
00741                 MOVE -1             TO  BDOMSTL
00742                 MOVE ER-0529 TO  EMI-ERROR
00743                 PERFORM 9900-ERROR-FORMAT.
00744
112103*    NOTE *******************************************************
112103*         *              EDIT THE CLP PERCENT.                  *
112103*         *******************************************************.
112103     IF CREDIT-SESSION
062121         IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103             MOVE AL-SADOF          TO  BCTLABLA
112103             MOVE AL-SADOF          TO  BCLPTOLA
112103         ELSE
112103             IF BCLPTOLL GREATER ZERO
112103                 
      * EXEC CICS BIF
112103*                    DEEDIT
112103*                    FIELD  (BCLPTOLI)
112103*                    LENGTH (7)
112103*                END-EXEC
           MOVE 7
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004170' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BCLPTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
112103                 IF BCLPTOLI NUMERIC
112103                     MOVE AL-UNNON      TO  BCLPTOLA
112103                     MOVE BCLPTOLI      TO  WS-CLP-TOL-PCT
112103                 ELSE
112103                     MOVE AL-UNBON      TO  BCLPTOLA
112103                     MOVE -1            TO  BCLPTOLL
112103                     MOVE ER-1778       TO  EMI-ERROR
112103                     PERFORM 9900-ERROR-FORMAT
112103                 END-IF
112103             END-IF
112103         END-IF
112103     END-IF.
112103
092705*    NOTE *******************************************************
092705*         *              EDIT THE LEASE COMM AMOUNT             *
092705*         *******************************************************
092705     IF CREDIT-SESSION
092705        IF BLCOMML > ZERO
092705           
      * EXEC CICS BIF
092705*               DEEDIT
092705*               FIELD  (BLCOMMI)
092705*               LENGTH (8)
092705*          END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004193' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034313933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BLCOMMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
092705           IF BLCOMMI NUMERIC
092705              MOVE AL-UNNON      TO BLCOMMA
092705              MOVE BLCOMMI       TO WS-SPP-LEASE-COMM
092705           ELSE
092705              MOVE AL-UNBON      TO BLCOMMA
092705              MOVE -1            TO BLCOMML
092705              MOVE ER-1778       TO EMI-ERROR
092705              PERFORM 9900-ERROR-FORMAT
092705           END-IF
092705        END-IF
092705     END-IF
092705
00745 *    NOTE *******************************************************
00746 *         *              EDIT THE PREMIUM TOLERANCE PERCENTAGE  *
00747 *         *******************************************************.
00748      IF CREDIT-SESSION
00749          IF BPRMTOLL GREATER ZERO
00750              
      * EXEC CICS BIF DEEDIT
00751 *                FIELD  (BPRMTOLI)
00752 *                LENGTH (6)
00753 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004215' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPRMTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00754              IF BPRMTOLI NOT NUMERIC
00755                  MOVE -1             TO  BPRMTOLL
00756                  MOVE AL-UNBON       TO  BPRMTOLA
00757                  MOVE ER-2010           TO  EMI-ERROR
00758                  PERFORM 9900-ERROR-FORMAT
00759              ELSE
00760              IF BPRMTOLI GREATER THAN 9999
00761                  MOVE -1             TO  BPRMTOLL
00762                  MOVE AL-UNBON       TO  BPRMTOLA
00763                  MOVE ER-2010           TO  EMI-ERROR
00764                  PERFORM 9900-ERROR-FORMAT
00765              ELSE
00766                  MOVE AL-UNNON       TO  BPRMTOLA
00767                  MOVE BPRMTOLI       TO  BPRMTOLO.
00768
00769      IF CREDIT-SESSION
00770          IF BREFTOLL GREATER ZERO
00771              
      * EXEC CICS BIF DEEDIT
00772 *                FIELD  (BREFTOLI)
00773 *                LENGTH (6)
00774 *            END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004236' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BREFTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00775              IF BREFTOLI NOT NUMERIC
00776                  MOVE -1             TO  BREFTOLL
00777                  MOVE AL-UNBON       TO  BREFTOLA
00778                  MOVE ER-2014           TO  EMI-ERROR
00779                  PERFORM 9900-ERROR-FORMAT
00780              ELSE
00781              IF BREFTOLI GREATER THAN 9999
00782                  MOVE -1             TO  BREFTOLL
00783                  MOVE AL-UNBON       TO  BREFTOLA
00784                  MOVE ER-2014           TO  EMI-ERROR
00785                  PERFORM 9900-ERROR-FORMAT
00786              ELSE
00787                  MOVE AL-UNNON       TO  BREFTOLA
00788                  MOVE BREFTOLI       TO  BREFTOLO.
00789
00790      IF CREDIT-SESSION
00791          IF BPRMPCTL GREATER ZERO
00792              
      * EXEC CICS BIF DEEDIT
00793 *                FIELD  (BPRMPCTI)
00794 *                LENGTH (5)
00795 *            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004257' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPRMPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00796              IF BPRMPCTI NUMERIC
00797                  MOVE AL-UNNON       TO  BPRMPCTA
00798                  MOVE BPRMPCTI       TO  WS-TOL-PREM-PCT
00799              ELSE
00800                  MOVE -1             TO  BPRMPCTL
00801                  MOVE AL-UNBON       TO  BPRMPCTA
00802                  MOVE ER-7532           TO  EMI-ERROR
00803                  PERFORM 9900-ERROR-FORMAT.
00804
00805      IF CREDIT-SESSION
00806          IF BREFPCTL GREATER ZERO
00807              
      * EXEC CICS BIF DEEDIT
00808 *                FIELD  (BREFPCTI)
00809 *                LENGTH (5)
00810 *            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004272' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BREFPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00811              IF BREFPCTI NUMERIC
00812                  MOVE AL-UNNON       TO  BREFPCTA
00813                  MOVE BREFPCTI       TO  WS-TOL-REF-PCT
00814              ELSE
00815                  MOVE -1             TO  BREFPCTL
00816                  MOVE AL-UNBON       TO  BREFPCTA
00817                  MOVE ER-7532           TO  EMI-ERROR
00818                  PERFORM 9900-ERROR-FORMAT.
00819
00820      IF CREDIT-SESSION
00821          IF BOVSPCTL GREATER ZERO
00822              
      * EXEC CICS BIF DEEDIT
00823 *                FIELD  (BOVSPCTI)
00824 *                LENGTH (5)
00825 *            END-EXEC
           MOVE 5
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004287' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034323837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BOVSPCTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00826              IF BOVSPCTI NUMERIC
00827                  MOVE AL-UNNON       TO  BOVSPCTA
00828                  MOVE BOVSPCTI       TO  WS-CR-OVR-SHT-PCT
00829              ELSE
00830                  MOVE -1             TO  BOVSPCTL
00831                  MOVE AL-UNBON       TO  BOVSPCTA
00832                  MOVE ER-7532           TO  EMI-ERROR
00833                  PERFORM 9900-ERROR-FORMAT
00834              END-IF
00835          END-IF.
00836
00837      IF CREDIT-SESSION
00838          GO TO 4900-EXIT.
00839
00840      IF CLAIM-SESSION
00841         IF ALPHCHL GREATER ZERO
00842            IF ALPHCHI ALPHABETIC
00843               MOVE AL-UANON       TO  ALPHCHA
00844            ELSE
00845               MOVE AL-UNBON       TO  ALPHCHA
00846               MOVE -1             TO  ALPHCHL
00847               MOVE ER-8128        TO  EMI-ERROR
00848               PERFORM 9900-ERROR-FORMAT.
00849
00850      IF ADOMSTL GREATER ZERO
00851          IF ADOMSTI ALPHABETIC-UPPER
00852              MOVE AL-UNNON       TO  ADOMSTA
00853          ELSE
00854              MOVE AL-UNBON       TO  ADOMSTA
00855              MOVE -1             TO  ADOMSTL
00856              MOVE ER-0529 TO  EMI-ERROR
00857              PERFORM 9900-ERROR-FORMAT.
00858
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF           TO ACTLABLA
112103*        MOVE AL-SADOF           TO ACLPTOLA
112103*    ELSE
112103*        IF ACLPTOLL GREATER ZERO
112103*            EXEC CICS BIF
112103*                DEEDIT
112103*                FIELD  (ACLPTOLI)
112103*                LENGTH (6)
112103*            END-EXEC
112103*            IF ACLPTOLI NUMERIC
112103*                MOVE AL-UNNON       TO  ACLPTOLA
112103*                MOVE ACLPTOLI       TO  WS-CLP-TOL-PCT
112103*            ELSE
112103*                MOVE AL-UNBON       TO  ACLPTOLA
112103*                MOVE -1             TO  ACLPTOLL
112103*                MOVE ER-1778 TO  EMI-ERROR
112103*                PERFORM 9900-ERROR-FORMAT
112103*            END-IF
112103*        END-IF
112103*    END-IF.
00860
00861 *    NOTE *******************************************************
00862 *         *      EDIT THE CLAIM NUMBER ASSIGNMENT METHOD.       *
00863 *         *******************************************************.
00864      IF ACLNAML GREATER ZERO
00865          IF (ACLNAMI = '1' OR '2' OR '3' OR '4' OR '5')
00866              MOVE AL-UNNON       TO  ACLNAMA
00867            ELSE
00868              MOVE AL-UNBON       TO  ACLNAMA
00869              MOVE -1             TO  ACLNAML
00870              MOVE ER-0090        TO EMI-ERROR
00871              PERFORM 9900-ERROR-FORMAT
00872        ELSE
00873          IF PI-MODE = 'A'
00874              MOVE AL-UNBOF       TO  ACLNAMA
00875              MOVE -1             TO  ACLNAML
00876              MOVE ER-0090        TO EMI-ERROR
00877              PERFORM 9900-ERROR-FORMAT.
00878
00879 *    NOTE *******************************************************
00880 *         *      EDIT THE CHECK NUMBER ASSIGNMENT METHOD.       *
00881 *         *******************************************************.
00882      IF ACKNAML GREATER ZERO
00883          IF ACKNAMI GREATER ZERO
00884            AND ACKNAMI LESS '5'
00885              MOVE AL-UNNON       TO  ACKNAMA
00886            ELSE
00887              MOVE AL-UNBON       TO  ACKNAMA
00888              MOVE -1             TO  ACKNAML
00889              MOVE ER-0091        TO EMI-ERROR
00890              PERFORM 9900-ERROR-FORMAT
00891        ELSE
00892          IF PI-MODE = 'A'
00893              MOVE AL-UNBOF       TO  ACKNAMA
00894              MOVE -1             TO  ACKNAML
00895              MOVE ER-0091        TO EMI-ERROR
00896              PERFORM 9900-ERROR-FORMAT.
00897
00898 *    NOTE *******************************************************
00899 *         *            EDIT THE CLAIM STARTING NUMBER.          *
00900 *         *******************************************************.
00901      IF ACLAIML GREATER ZERO
00902          
      * EXEC CICS BIF DEEDIT
00903 *            FIELD  (ACLAIMI)
00904 *            LENGTH (8)
00905 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004387' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034333837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACLAIMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00906          IF ACLAIMI NUMERIC
00907              MOVE AL-UNNON       TO  ACLAIMA
00908            ELSE
00909              MOVE AL-UNBON       TO  ACLAIMA
00910              MOVE -1             TO  ACLAIML
00911              MOVE ER-0637        TO EMI-ERROR
00912              PERFORM 9900-ERROR-FORMAT
00913
00914 *    NOTE *******************************************************
00915 *         *            EDIT THE CHECK STARTING NUMBER.          *
00916 *         *******************************************************.
00917      IF ACHECKL GREATER ZERO
00918          
      * EXEC CICS BIF DEEDIT
00919 *            FIELD  (ACHECKI)
00920 *            LENGTH (8)
00921 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00004403' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACHECKI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00922          IF ACHECKI NUMERIC
00923              MOVE AL-UNNON       TO  ACHECKA
00924            ELSE
00925              MOVE AL-UNBON       TO  ACHECKA
00926              MOVE -1             TO  ACHECKL
00927              MOVE ER-0638           TO  EMI-ERROR
00928              PERFORM 9900-ERROR-FORMAT
00929
00930 *    NOTE *******************************************************
00931 *         *              EDIT THE LETTER ARCHIVE.               *
00932 *         *******************************************************.
00933      IF ALAL GREATER ZERO
00934          IF ALAI = 'Y' OR 'N'
00935              MOVE AL-UANON       TO  ALAA
00936            ELSE
00937              MOVE AL-UABON       TO  ALAA
00938              MOVE -1             TO  ALAL
00939              MOVE ER-0092        TO EMI-ERROR
00940              PERFORM 9900-ERROR-FORMAT
00941        ELSE
00942          IF PI-MODE = 'A'
00943              MOVE AL-UABOF       TO  ALAA
00944              MOVE -1             TO  ALAL
00945              MOVE ER-0092        TO EMI-ERROR
00946              PERFORM 9900-ERROR-FORMAT.
00947
00948      IF AEXPCML NOT GREATER ZERO
00949          GO TO 4200-EDIT-MAP-FIELDS.
00950
00951      IF AEXPCML GREATER ZERO
00952          IF AEXPCMI GREATER ZERO
00953            AND AEXPCMI LESS '5'
00954              MOVE AL-UNNON           TO  AEXPCMA
00955            ELSE
00956              MOVE AL-UNBON           TO  AEXPCMA
00957              MOVE -1                 TO  AEXPCML
00958              MOVE ER-0093            TO EMI-ERROR
00959              PERFORM 9900-ERROR-FORMAT.
00960
00961      IF AEXPCAL GREATER ZERO
00962          
      * EXEC CICS BIF DEEDIT
00963 *            FIELD  (AEXPCAI)
00964 *            LENGTH (AEXPCA-LENGTH)
00965 *        END-EXEC
      *    MOVE '@"L                   #   #00004447' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCAI, 
                 AEXPCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00966          IF AEXPCAI NUMERIC
00967              MOVE AL-UNNON       TO  AEXPCAA
00968              MOVE AEXPCAI        TO  WS-EXPENSE-DOLLAR
00969                                      AEXPCAO
00970            ELSE
00971              MOVE AL-UNBON       TO  AEXPCAA
00972              MOVE -1             TO  AEXPCAL
00973              MOVE ER-0097        TO EMI-ERROR
00974              PERFORM 9900-ERROR-FORMAT.
00975
00976      IF AEXPCPL GREATER ZERO
00977          
      * EXEC CICS BIF DEEDIT
00978 *            FIELD  (AEXPCPI)
00979 *            LENGTH (AEXPCP-LENGTH)
00980 *        END-EXEC
      *    MOVE '@"L                   #   #00004462' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCPI, 
                 AEXPCP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00981          IF AEXPCPI NUMERIC
00982              MOVE AEXPCPI        TO  WS-EXPENSE-PERCENT
00983                                      AEXPCPO
00984              MOVE AL-UNNON       TO  AEXPCPA
00985            ELSE
00986              MOVE AL-UNBON       TO  AEXPCPA
00987              MOVE -1             TO  AEXPCPL
00988              MOVE ER-0099        TO EMI-ERROR
00989              PERFORM 9900-ERROR-FORMAT.
00990
00991      IF CLAIM-SESSION
00992         IF ABRETRL GREATER ZERO
00993            IF ABRETRI NUMERIC
00994               MOVE ABRETRI   TO CF-BUILD-RETRIEVE-AFTER-MONTHS
00995            ELSE
00996               MOVE AL-UNBON           TO ABRETRA
00997               MOVE -1                 TO ABRETRL
00998               MOVE ER-8127            TO EMI-ERROR
00999               PERFORM 9900-ERROR-FORMAT.
01000
01001      IF AEXPCMI NOT = '1'
01002          GO TO 4120-EDIT-MAP-FIELDS.
01003
01004      IF WS-EXPENSE-PERCENT NOT = ZERO
01005          MOVE AL-UNBON           TO  AEXPCPA
01006          MOVE -1                 TO  AEXPCPL
01007          MOVE ER-0094            TO EMI-ERROR
01008          PERFORM 9900-ERROR-FORMAT.
01009
01010      IF WS-EXPENSE-DOLLAR NOT = ZERO
01011          MOVE AL-UNBON           TO  AEXPCAA
01012          MOVE -1                 TO  AEXPCAL
01013          MOVE ER-0095            TO EMI-ERROR
01014          PERFORM 9900-ERROR-FORMAT.
01015
01016      GO TO 4200-EDIT-MAP-FIELDS.
01017
01018  4120-EDIT-MAP-FIELDS.
01019      IF AEXPCMI NOT = '2'
01020          GO TO 4130-EDIT-MAP-FIELDS.
01021
01022      IF WS-EXPENSE-PERCENT NOT = ZERO
01023          MOVE AL-UNBON           TO  AEXPCPA
01024          MOVE -1                 TO  AEXPCPL
01025          MOVE ER-0096            TO EMI-ERROR
01026          PERFORM 9900-ERROR-FORMAT.
01027
01028      IF WS-EXPENSE-DOLLAR = ZERO
01029          MOVE AL-UNBON           TO  AEXPCAA
01030          MOVE -1                 TO  AEXPCAL
01031          MOVE ER-0098            TO EMI-ERROR
01032          PERFORM 9900-ERROR-FORMAT.
01033
01034      GO TO 4200-EDIT-MAP-FIELDS.
01035
01036  4130-EDIT-MAP-FIELDS.
01037      IF AEXPCMI NOT = '3'
01038          GO TO 4140-EDIT-MAP-FIELDS.
01039
01040      IF WS-EXPENSE-PERCENT = ZERO
01041          MOVE AL-UNBON           TO  AEXPCPA
01042          MOVE -1                 TO  AEXPCPL
01043          MOVE ER-0100            TO EMI-ERROR
01044          PERFORM 9900-ERROR-FORMAT.
01045
01046      IF WS-EXPENSE-DOLLAR NOT = ZERO
01047          MOVE AL-UNBON           TO  AEXPCAA
01048          MOVE -1                 TO  AEXPCAL
01049          MOVE ER-0117            TO EMI-ERROR
01050          PERFORM 9900-ERROR-FORMAT.
01051
01052      GO TO 4200-EDIT-MAP-FIELDS.
01053
01054  4140-EDIT-MAP-FIELDS.
01055      IF WS-EXPENSE-PERCENT NOT = ZERO
01056          MOVE AL-UNBON           TO  AEXPCPA
01057          MOVE -1                 TO  AEXPCPL
01058          MOVE ER-0101            TO EMI-ERROR
01059          PERFORM 9900-ERROR-FORMAT.
01060
01061      IF WS-EXPENSE-DOLLAR = ZERO
01062          MOVE AL-UABON           TO  AEXPCAA
01063          MOVE -1                 TO  AEXPCAL
01064          MOVE ER-0102            TO EMI-ERROR
01065          PERFORM 9900-ERROR-FORMAT.
01066
01067      GO TO 4200-EDIT-MAP-FIELDS.
01068
01069  4200-EDIT-MAP-FIELDS.
01070 *    NOTE *******************************************************
01071 *         *          EDIT THE CLAIM CALCULATION METHOD.         *
01072 *         *******************************************************.
01073      IF ACLCML GREATER ZERO
01074          IF ACLCMI GREATER ZERO
01075            AND ACLCMI LESS '7'
01076              MOVE AL-UNNON       TO  ACLCMA
01077            ELSE
01078              MOVE AL-UNBON       TO  ACLCMA
01079              MOVE -1             TO  ACLCML
01080              MOVE ER-0103        TO EMI-ERROR
01081              PERFORM 9900-ERROR-FORMAT.
01082
01083 *    NOTE *******************************************************
01084 *         *      EDIT THE MANUAL RESERVES INDICATOR.            *
01085 *         *******************************************************.
01086      IF ARESMANL GREATER ZERO
01087          IF ARESMANI = 'Y' OR 'N'
01088              MOVE AL-UANON       TO  ARESMANA
01089            ELSE
01090              MOVE AL-UABON       TO  ARESMANA
01091              MOVE -1             TO  ARESMANL
01092              MOVE ER-0107        TO EMI-ERROR
01093              PERFORM 9900-ERROR-FORMAT
01094        ELSE
01095          IF PI-MODE = 'A'
01096              MOVE AL-UABOF       TO  ARESMANA
01097              MOVE -1             TO  ARESMANL
01098              MOVE ER-0108        TO EMI-ERROR
01099              PERFORM 9900-ERROR-FORMAT.
01100
01101 *    NOTE *******************************************************
01102 *         *      EDIT THE CDT/FUTURE RESERVES INDICATOR.        *
01103 *         *******************************************************.
01104      IF ARESCDTL GREATER ZERO
01105          IF ARESCDTI = 'Y' OR 'N'
01106              MOVE AL-UANON       TO  ARESCDTA
01107            ELSE
01108              MOVE AL-UABON       TO  ARESCDTA
01109              MOVE -1             TO  ARESCDTL
01110              MOVE ER-0109        TO EMI-ERROR
01111              PERFORM 9900-ERROR-FORMAT
01112        ELSE
01113          IF PI-MODE = 'A'
01114              MOVE AL-UABOF       TO  ARESCDTA
01115              MOVE -1             TO  ARESCDTL
01116              MOVE ER-0110        TO EMI-ERROR
01117              PERFORM 9900-ERROR-FORMAT.
01118
01119 *    NOTE *******************************************************
01120 *         *        EDIT THE IBN RESERVES INDICATOR.             *
01121 *         *******************************************************.
01122      IF ARESIBNL GREATER ZERO
01123          IF ARESIBNI = 'Y' OR 'N' OR '1' OR '2'
01124              MOVE AL-UANON       TO  ARESIBNA
01125            ELSE
01126              MOVE AL-UABON       TO  ARESIBNA
01127              MOVE -1             TO  ARESIBNL
01128              MOVE ER-0111        TO EMI-ERROR
01129              PERFORM 9900-ERROR-FORMAT
01130        ELSE
01131          IF PI-MODE = 'A'
01132              MOVE AL-UABOF       TO  ARESIBNA
01133              MOVE -1             TO  ARESIBNL
01134              MOVE ER-0112        TO EMI-ERROR
01135              PERFORM 9900-ERROR-FORMAT.
01136
01137 *    NOTE *******************************************************
01138 *         *      EDIT THE PAY-TO-CURRENT RESERVES INDICATOR.    *
01139 *         *******************************************************.
01140      IF ARESPTCL GREATER ZERO
01141          IF ARESPTCI = 'Y' OR 'N'
01142              MOVE AL-UANON       TO  ARESPTCA
01143            ELSE
01144              MOVE AL-UABON       TO  ARESPTCA
01145              MOVE -1             TO  ARESPTCL
01146              MOVE ER-0113        TO EMI-ERROR
01147              PERFORM 9900-ERROR-FORMAT
01148        ELSE
01149          IF PI-MODE = 'A'
01150              MOVE AL-UABOF       TO  ARESPTCA
01151              MOVE -1             TO  ARESPTCL
01152              MOVE ER-0114        TO EMI-ERROR
01153              PERFORM 9900-ERROR-FORMAT.
01154
01155 *    NOTE *******************************************************
01156 *         *      IF THE PERCENT OF CDT IS ENTERED THE CDT ACCESS*
01157 *         *  MUST ALSO BE ENTERED ON ADD.                       *
01158 *         *******************************************************.
01159      IF PI-MODE = 'A'
01160          IF ACDTAL NOT GREATER ZERO
01161            AND APCTCDTL GREATER ZERO
01162              MOVE -1             TO  ACDTAL
01163              MOVE AL-UNBOF       TO  ACDTAA
01164              MOVE AL-UNBON       TO  APCTCDTA
01165              MOVE ER-0104        TO EMI-ERROR
01166              PERFORM 9900-ERROR-FORMAT.
01167
01168 *    NOTE *******************************************************
01169 *         *      EDIT THE PERCENT OF CDT.                       *
01170 *         *******************************************************.
01171      IF APCTCDTL GREATER ZERO
01172          
      * EXEC CICS BIF DEEDIT
01173 *            FIELD  (APCTCDTI)
01174 *            LENGTH (APCTCDT-LENGTH)
01175 *        END-EXEC
      *    MOVE '@"L                   #   #00004657' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APCTCDTI, 
                 APCTCDT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01176          IF APCTCDTI NUMERIC
01177              MOVE AL-UNNON       TO  APCTCDTA
01178              MOVE APCTCDTI         TO  APCTCDTO
01179            ELSE
01180              MOVE AL-UNBON       TO  APCTCDTA
01181              MOVE -1             TO  APCTCDTL
01182              MOVE ER-0106           TO  EMI-ERROR
01183              PERFORM 9900-ERROR-FORMAT.
01184
01185 *    NOTE *******************************************************
01186 *         *      EDIT THE CDT ACCESS METHOD                     *
01187 *         *******************************************************.
01188      IF ACDTAL GREATER THAN +0
01189          IF (ACDTAI = '1' OR '2' OR '3')
01190             OR
01191             (PI-COMPANY-ID EQUAL 'FLA' AND
01192              ACDTAI EQUAL '1' OR '2' OR '3' OR '4')
01193              MOVE AL-UANON       TO  ARESIBNA
01194            ELSE
01195              MOVE AL-UABON       TO  ACDTAA
01196              MOVE -1             TO  ACDTAL
01197              MOVE ER-0105        TO EMI-ERROR
01198              PERFORM 9900-ERROR-FORMAT.
01199
01200 *    NOTE *******************************************************
01201 *         *      EDIT THE IBNR PERCENT.                         *
01202 *         *******************************************************.
01203      IF IBNRPCTL GREATER ZERO
01204          
      * EXEC CICS BIF DEEDIT
01205 *            FIELD  (IBNRPCTI)
01206 *            LENGTH (IBNRPCT-LENGTH)
01207 *        END-EXEC
      *    MOVE '@"L                   #   #00004689' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRPCTI, 
                 IBNRPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01208          IF IBNRPCTI NUMERIC
01209              MOVE AL-UNNON       TO  IBNRPCTA
01210              MOVE IBNRPCTI       TO  IBNRPCTO
01211            ELSE
01212              MOVE AL-UNBON       TO  IBNRPCTA
01213              MOVE -1             TO  IBNRPCTL
01214              MOVE ER-0106        TO  EMI-ERROR
01215              PERFORM 9900-ERROR-FORMAT.
01216
01217 *    NOTE *******************************************************
01218 *         *      EDIT THE IBNR UNEARNED PREMIUM PERCENT         *
01219 *         *******************************************************.
01220      IF AUEPPCTL GREATER ZERO
01221          
      * EXEC CICS BIF DEEDIT
01222 *            FIELD  (AUEPPCTI)
01223 *            LENGTH (AUEPPCT-LENGTH)
01224 *        END-EXEC
      *    MOVE '@"L                   #   #00004706' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AUEPPCTI, 
                 AUEPPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01225          IF AUEPPCTI NUMERIC
01226              MOVE AL-UNNON       TO  AUEPPCTA
01227              MOVE AUEPPCTI       TO  WS-IBNR-UEP-PCT
01228                                      AUEPPCTO
01229            ELSE
01230              MOVE AL-UNBON       TO  AUEPPCTA
01231              MOVE -1             TO  AUEPPCTL
01232              MOVE ER-0106        TO  EMI-ERROR
01233              PERFORM 9900-ERROR-FORMAT.
01234
01235 *    NOTE *******************************************************
01236 *         *      EDIT THE IBNR UNEARNED RULE 78 PERCENT         *
01237 *         *******************************************************.
01238      IF AR78PCTL GREATER ZERO
01239          
      * EXEC CICS BIF DEEDIT
01240 *            FIELD  (AR78PCTI)
01241 *            LENGTH (AR78PCT-LENGTH)
01242 *        END-EXEC
      *    MOVE '@"L                   #   #00004724' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AR78PCTI, 
                 AR78PCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01243          IF AR78PCTI NUMERIC
01244              MOVE AL-UNNON       TO  AR78PCTA
01245              MOVE AR78PCTI       TO  WS-IBNR-R78-PCT
01246                                      AR78PCTO
01247            ELSE
01248              MOVE AL-UNBON       TO  AR78PCTA
01249              MOVE -1             TO  AR78PCTL
01250              MOVE ER-0106        TO  EMI-ERROR
01251              PERFORM 9900-ERROR-FORMAT.
01252
01253 *    NOTE *******************************************************
01254 *         *      EDIT THE IBNR UNEARNED PRORATA PERCENT         *
01255 *         *******************************************************.
01256      IF APROPCTL GREATER ZERO
01257          
      * EXEC CICS BIF DEEDIT
01258 *            FIELD  (APROPCTI)
01259 *            LENGTH (APROPCT-LENGTH)
01260 *        END-EXEC
      *    MOVE '@"L                   #   #00004742' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APROPCTI, 
                 APROPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01261          IF APROPCTI NUMERIC
01262              MOVE AL-UNNON       TO  APROPCTA
01263              MOVE APROPCTI       TO  WS-IBNR-PRO-PCT
01264                                      APROPCTO
01265            ELSE
01266              MOVE AL-UNBON       TO  APROPCTA
01267              MOVE -1             TO  APROPCTL
01268              MOVE ER-0106        TO  EMI-ERROR
01269              PERFORM 9900-ERROR-FORMAT.
01270
01271 *    NOTE *******************************************************
01272 *         *      EDIT THE IBNR R78 AND PRO TOTAL PERCENT        *
01273 *         *******************************************************.
01274      IF WS-IBNR-UEP-PCT NOT = ZEROS
01275          IF (WS-IBNR-R78-PCT + WS-IBNR-PRO-PCT) NOT = +1.0
01276              MOVE AL-UNBON       TO  AR78PCTA
01277                                      APROPCTA
01278              MOVE -1             TO  AR78PCTL
01279                                      APROPCTL
01280              MOVE ER-2308        TO  EMI-ERROR
01281              PERFORM 9900-ERROR-FORMAT.
01282
01283 *    NOTE *******************************************************
01284 *         *      EDIT THE LIMITS - QUOTED/CALCULATED AMOUNT.    *
01285 *         *******************************************************.
01286      IF ALQCAL GREATER ZERO
01287          
      * EXEC CICS BIF DEEDIT
01288 *            FIELD  (ALQCAI)
01289 *            LENGTH (ALQCA-LENGTH)
01290 *        END-EXEC
      *    MOVE '@"L                   #   #00004772' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCAI, 
                 ALQCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01291          IF ALQCAI NUMERIC
01292              MOVE AL-UNNON       TO  ALQCAA
01293              MOVE ALQCAI         TO  ALQCAO
01294            ELSE
01295              MOVE AL-UNBON       TO  ALQCAA
01296              MOVE -1             TO  ALQCAL
01297              MOVE ER-0118        TO EMI-ERROR
01298              PERFORM 9900-ERROR-FORMAT.
01299
01300 *    NOTE *******************************************************
01301 *         *      EDIT THE LIMITS - MAXIMUM REGULAR PAYMENT.     *
01302 *         *******************************************************.
01303      IF ALMRPL GREATER ZERO
01304          
      * EXEC CICS BIF DEEDIT
01305 *            FIELD  (ALMRPI)
01306 *            LENGTH (ALMRP-LENGTH)
01307 *        END-EXEC
      *    MOVE '@"L                   #   #00004789' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMRPI, 
                 ALMRP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01308          IF ALMRPI NUMERIC
01309              MOVE AL-UNNON       TO  ALMRPA
01310              MOVE ALMRPI         TO  ALMRPO
01311            ELSE
01312              MOVE AL-UNBON       TO  ALMRPA
01313              MOVE -1             TO  ALMRPL
01314              MOVE ER-0119        TO EMI-ERROR
01315              PERFORM 9900-ERROR-FORMAT.
01316
01317 *    NOTE *******************************************************
01318 *         *      EDIT THE LIMITS - QUOTED/CALCULATION DAYS.     *
01319 *         *******************************************************.
01320      IF ALQCDL GREATER ZERO
01321          
      * EXEC CICS BIF DEEDIT
01322 *            FIELD  (ALQCDI)
01323 *            LENGTH (ALQCD-LENGTH)
01324 *        END-EXEC
      *    MOVE '@"L                   #   #00004806' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCDI, 
                 ALQCD-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01325          IF ALQCDI NUMERIC
01326              MOVE AL-UNNON       TO  ALQCDA
01327              MOVE ALQCDI         TO  ALQCDO
01328            ELSE
01329              MOVE AL-UNBON       TO  ALQCDA
01330              MOVE -1             TO  ALQCDL
01331              MOVE ER-0120        TO EMI-ERROR
01332              PERFORM 9900-ERROR-FORMAT.
01333
01334 *    NOTE *******************************************************
01335 *         *      EDIT THE LIMITS - MAXIMUM DAYS PER PAYMENT.    *
01336 *         *******************************************************.
01337      IF ALMDPPL GREATER ZERO
01338          
      * EXEC CICS BIF DEEDIT
01339 *            FIELD  (ALMDPPI)
01340 *            LENGTH (ALMDPP-LENGTH)
01341 *        END-EXEC
      *    MOVE '@"L                   #   #00004823' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMDPPI, 
                 ALMDPP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01342          IF ALMDPPI NUMERIC
01343              MOVE AL-UNNON       TO  ALMDPPA
01344              MOVE ALMDPPI        TO  ALMDPPO
01345            ELSE
01346              MOVE AL-UNBON       TO  ALMDPPA
01347              MOVE -1             TO  ALMDPPL
01348              MOVE ER-0121        TO EMI-ERROR
01349              PERFORM 9900-ERROR-FORMAT.
01350
01351 *    NOTE *******************************************************
01352 *         *      EDIT THE LIMITS - DAYS BEFORE CLOSED.          *
01353 *         *******************************************************.
01354      IF ALDBCL GREATER ZERO
01355          
      * EXEC CICS BIF DEEDIT
01356 *            FIELD  (ALDBCI)
01357 *            LENGTH (ALDBC-LENGTH)
01358 *        END-EXEC
      *    MOVE '@"L                   #   #00004840' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALDBCI, 
                 ALDBC-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01359          IF ALDBCI NUMERIC
01360              MOVE AL-UNNON       TO  ALDBCA
01361              MOVE ALDBCI         TO  ALDBCO
01362            ELSE
01363              MOVE AL-UNBON       TO  ALDBCA
01364              MOVE -1             TO  ALDBCL
01365              MOVE ER-0122        TO EMI-ERROR
01366              PERFORM 9900-ERROR-FORMAT.
01367
01368 *    NOTE *******************************************************
01369 *         *      EDIT THE LIMITS - MAXIMUM AUTOMATIC PAYMENT.   *
01370 *         *******************************************************.
01371      IF ALMAPL GREATER ZERO
01372          
      * EXEC CICS BIF DEEDIT
01373 *            FIELD  (ALMAPI)
01374 *            LENGTH (ALMAP-LENGTH)
01375 *        END-EXEC
      *    MOVE '@"L                   #   #00004857' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383537' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPI, 
                 ALMAP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01376          IF ALMAPI NUMERIC
01377              MOVE AL-UNNON       TO  ALMAPA
01378              MOVE ALMAPI         TO  ALMAPO
01379            ELSE
01380              MOVE AL-UNBON       TO  ALMAPA
01381              MOVE -1             TO  ALMAPL
01382              MOVE ER-0123            TO EMI-ERROR
01383              PERFORM 9900-ERROR-FORMAT.
01384
01385 *    NOTE *******************************************************
01386 *         *      EDIT THE LIMITS - MONTHS BEFORE PURGED.        *
01387 *         *******************************************************.
01388      IF ALMBPL GREATER ZERO
01389          
      * EXEC CICS BIF DEEDIT
01390 *            FIELD  (ALMBPI)
01391 *            LENGTH (ALMBP-LENGTH)
01392 *        END-EXEC
      *    MOVE '@"L                   #   #00004874' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMBPI, 
                 ALMBP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01393          IF ALMBPI NUMERIC
01394              MOVE AL-UNNON       TO  ALMBPA
01395              MOVE ALMBPI         TO  ALMBPO
01396            ELSE
01397              MOVE AL-UNBON       TO  ALMBPA
01398              MOVE -1             TO  ALMBPL
01399              MOVE ER-0124        TO EMI-ERROR
01400              PERFORM 9900-ERROR-FORMAT.
01401
01402 *    NOTE *******************************************************
01403 *         *      EDIT THE LIMITS - MAXIMUM AUTO-PAY MONTHS.     *
01404 *         *******************************************************.
01405      IF ALMAPML GREATER ZERO
01406          
      * EXEC CICS BIF DEEDIT
01407 *            FIELD  (ALMAPMI)
01408 *            LENGTH (ALMAPM-LENGTH)
01409 *        END-EXEC
      *    MOVE '@"L                   #   #00004891' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPMI, 
                 ALMAPM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01410          IF ALMAPMI NUMERIC
01411              MOVE AL-UNNON       TO  ALMAPMA
01412              MOVE ALMAPMI        TO  ALMAPMO
01413            ELSE
01414              MOVE AL-UNBON       TO  ALMAPMA
01415              MOVE -1             TO  ALMAPML
01416              MOVE ER-0125        TO EMI-ERROR
01417              PERFORM 9900-ERROR-FORMAT.
01418
01419  4900-EXIT.
01420      EXIT.
01421
01422      EJECT
01423  5000-ADD-RECORD SECTION.
01424      
      * EXEC CICS GETMAIN
01425 *        SET     (ADDRESS OF CONTROL-FILE)
01426 *        LENGTH  (750)
01427 *        INITIMG (GETMAIN-SPACE)
01428 *    END-EXEC.
           MOVE 750
             TO DFHEIV11
      *    MOVE ',"IL                  $   #00004909' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 DFHEIV11, 
                 GETMAIN-SPACE
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01429
01430      MOVE 'CF'                   TO  CF-RECORD-ID.
01431
01432      MOVE PI-COMPANY-ID          TO  CF-COMPANY-ID.
01433      MOVE '6'                    TO  CF-RECORD-TYPE.
01434      MOVE PI-CARRIER-NUMBER      TO  CF-CARRIER-CNTL.
01435      MOVE ZERO                   TO  CF-SEQUENCE-NO.
01436
01437      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.
01438      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01439      MOVE '5'                    TO  DC-OPTION-CODE.
01440      PERFORM 8500-DATE-CONVERSION.
01441      MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.
01442      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.
01443
01444      MOVE ZERO                   TO  CF-ZIP-CODE
01445                                      CF-ZIP-CODE-NUM
01446                                      CF-PHONE-NO
01447                                      CF-CLAIM-COUNTER
01448                                      CF-CHECK-COUNTER
01449                                      CF-EXPENSE-PERCENT
01450                                      CF-EXPENSE-DOLLAR
01451                                      CF-PERCENT-OF-CDT
01452                                      CF-IBNR-PERCENT
01453                                      CF-IBNR-UEPRM-PERCENT
01454                                      CF-IBNR-R78-PERCENT
01455                                      CF-IBNR-PRO-PERCENT
01456                                      CF-CALC-AMT-TOL
01457                                      CF-MAX-REG-PMT
01458                                      CF-MAX-REG-DAYS
01459                                      CF-MAX-AUTO-PMT
01460                                      CF-MAX-AUTO-MOS
01461                                      CF-CALC-DAYS-TOL
01462                                      CF-DAYS-BEFORE-CLOSED
01463                                      CF-MONTHS-BEFORE-PURGED
01464                                      CF-CR-TOL-PREM
01465                                      CF-CR-TOL-REFUND
01466                                      CF-CR-TOL-PREM-PCT
01467                                      CF-CR-TOL-REFUND-PCT
092705                                     CF-CARRIER-CLP-TOL-PCT
092705                                     CF-CARRIER-LEASE-COMM
032813                                     CF-CARRIER-NEXT-AUDIT-CHK-NO
01468
01469      MOVE '1'                    TO  CF-CLAIM-NO-METHOD
01470                                      CF-CHECK-NO-METHOD
01471                                      CF-EXPENSE-METHOD
01472                                      CF-CDT-ACCESS-METHOD
01473                                      CF-CLAIM-CALC-METHOD.
01474
01475      PERFORM 5900-MOVE-MAP-DATA.
01476
01477      MOVE 'A'                    TO  JP-RECORD-TYPE.
01478      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01479
01480      
      * EXEC CICS WRITE
01481 *        DATASET (WS-CONTROL-FILE-DSID)
01482 *        RIDFLD  (CF-CONTROL-PRIMARY)
01483 *        FROM    (CONTROL-FILE)
01484 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004968' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 CF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01485
01486      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
01487
01488  5090-EXIT.
01489      EXIT.
01490
01491      EJECT
01492  5100-CHANGE-RECORD SECTION.
01493      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01494
01495      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01496      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
01497      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
01498      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01499
01500      
      * EXEC CICS READ UPDATE
01501 *        DATASET (WS-CONTROL-FILE-DSID)
01502 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01503 *        SET     (ADDRESS OF CONTROL-FILE)
01504 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00004988' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01505
01506      MOVE 'B'                    TO  JP-RECORD-TYPE.
01507      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01508
01509      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
01510
01511      PERFORM 5900-MOVE-MAP-DATA.
01512
01513      MOVE 'C'                    TO  JP-RECORD-TYPE.
01514      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01515
01516      MOVE EIBTIME                TO  CF-LAST-MAINT-HHMMSS.
01517      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
01518      MOVE '5'                    TO  DC-OPTION-CODE.
01519      PERFORM 8500-DATE-CONVERSION.
01520      MOVE DC-BIN-DATE-1          TO  CF-LAST-MAINT-DT.
01521      MOVE PI-PROCESSOR-ID        TO  CF-LAST-MAINT-BY.
01522
01523      
      * EXEC CICS REWRITE
01524 *        DATASET (WS-CONTROL-FILE-DSID)
01525 *        FROM    (CONTROL-FILE)
01526 *    END-EXEC.
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00005011' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01527
01528      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
01529
01530  5190-EXIT.
01531      EXIT.
01532
01533      EJECT
01534  5200-DELETE-RECORD SECTION.
01535      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01536
01537      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01538      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
01539      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
01540      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01541
01542      
      * EXEC CICS READ UPDATE
01543 *        DATASET (WS-CONTROL-FILE-DSID)
01544 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01545 *        SET     (ADDRESS OF CONTROL-FILE)
01546 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00005030' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01547
01548      MOVE 'D'                    TO  JP-RECORD-TYPE.
01549      MOVE CONTROL-FILE           TO  JP-RECORD-AREA.
01550
01551      
      * EXEC CICS DELETE
01552 *        DATASET (WS-CONTROL-FILE-DSID)
01553 *    END-EXEC.
      *    MOVE '&(                    &   #00005039' TO DFHEIV0
           MOVE X'262820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01554
01555      PERFORM 8400-LOG-JOURNAL-RECORD THRU 8400-EXIT.
01556
01557  5290-EXIT.
01558      EXIT.
01559
01560      EJECT
01561  5900-MOVE-MAP-DATA SECTION.
01562
01563      IF NOT CREDIT-SESSION
01564          GO TO 5910-NOT-CREDIT-SESSION.
01565
112103     IF BSECPAYL GREATER ZERO
112103         MOVE BSECPAYI       TO  CF-SECPAY-SWITCH
112103     END-IF.
112103
01566      IF BCONAMEL GREATER ZERO
01567          MOVE BCONAMEI           TO  CF-MAIL-TO-NAME.
01568
01569      IF BCAREOFL GREATER ZERO
01570          MOVE BCAREOFI           TO  CF-IN-CARE-OF.
01571
01572      IF BADDR1L GREATER ZERO
01573          MOVE BADDR1I            TO  CF-ADDRESS-LINE-1.
01574
01575      IF BADDR2L GREATER ZERO
01576          MOVE BADDR2I            TO  CF-ADDRESS-LINE-2.
01577
01578      IF BCITYSTL GREATER ZERO
01579          MOVE BCITYSTI           TO  CF-CITY-STATE.
01580
01581      IF BPHONEL GREATER ZERO
01582          
      * EXEC CICS BIF DEEDIT
01583 *            FIELD  (BPHONEI)
01584 *            LENGTH (APHONE-LENGTH)
01585 *        END-EXEC
      *    MOVE '@"L                   #   #00005074' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035303734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01586          MOVE BPHONEI            TO  CF-PHONE-NO
01587                                      BPHONEO
01588          INSPECT BPHONEO CONVERTING SPACES TO '-'.
01589
01590      IF CF-ZIP-CODE-NUM NUMERIC  AND
01591         CF-ZIP-CODE-NUM NOT = ZEROS
01592          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
01593          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
01594
01595      IF BZIPL NOT = ZERO
01596          MOVE BZIPI              TO  WS-ZIP-CODE
01597          MOVE ZEROS              TO  CF-ZIP-CODE-NUM
01598      ELSE
01599          GO TO 5905-CONTINUE.
01600
01601      IF WS-CANADIAN-ZIP
01602          IF WS-ZIP-4 = SPACE  OR  '-'
01603              MOVE WS-ZIP-CAN-2-POST1   TO CF-CAN-POSTAL-1
01604              MOVE WS-ZIP-CAN-2-POST2   TO CF-CAN-POSTAL-2
01605          ELSE
01606              MOVE WS-ZIP-CAN-1-POST1   TO CF-CAN-POSTAL-1
01607              MOVE WS-ZIP-CAN-1-POST2   TO CF-CAN-POSTAL-2
01608      ELSE
01609          IF WS-ZIP-6 = SPACE  OR  '-'
01610              MOVE WS-ZIP-AM-2-CODE     TO CF-ZIP-PRIME
01611              MOVE WS-ZIP-AM-2-PLUS4    TO CF-ZIP-PLUS4
01612          ELSE
01613              MOVE WS-ZIP-AM-1-CODE     TO CF-ZIP-PRIME
01614              MOVE WS-ZIP-AM-1-PLUS4    TO CF-ZIP-PLUS4.
01615
01616  5905-CONTINUE.
01617
01618      IF BPRMTOLL GREATER ZERO
01619          
      * EXEC CICS BIF DEEDIT
01620 *            FIELD  (BPRMTOLI)
01621 *            LENGTH (6)
01622 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005111' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BPRMTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01623          MOVE BPRMTOLI           TO  CF-CR-TOL-PREM.
01624
01625      IF BREFTOLL GREATER ZERO
01626          
      * EXEC CICS BIF DEEDIT
01627 *            FIELD  (BREFTOLI)
01628 *            LENGTH (6)
01629 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005118' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BREFTOLI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01630          MOVE BREFTOLI           TO  CF-CR-TOL-REFUND.
01631
01632      IF BOVSAMTL GREATER ZERO
01633          
      * EXEC CICS BIF DEEDIT
01634 *            FIELD  (BOVSAMTI)
01635 *            LENGTH (6)
01636 *        END-EXEC
           MOVE 6
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005125' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035313235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BOVSAMTI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01637          MOVE BOVSAMTI           TO  CF-CR-OVR-SHT-AMT
01638      END-IF.
01639
01640      IF BPRMPCTL GREATER ZERO
01641          MOVE WS-TOL-PREM-PCT    TO  CF-CR-TOL-PREM-PCT.
01642
01643      IF BREFPCTL GREATER ZERO
01644          MOVE WS-TOL-REF-PCT     TO  CF-CR-TOL-REFUND-PCT.
01645
01646      IF BOVSPCTL > 0
01647          MOVE WS-CR-OVR-SHT-PCT  TO  CF-CR-OVR-SHT-PCT
01648      END-IF.
01649
01650      IF BDOMSTL GREATER ZERO
01651          MOVE BDOMSTI            TO  CF-DOMICILE-STATE.
112103
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE ZERO               TO  CF-CARRIER-CLP-TOL-PCT
092705                                     CF-CARRIER-LEASE-COMM
112103     ELSE
112103         IF BCLPTOLL GREATER ZERO
112103             MOVE WS-CLP-TOL-PCT TO  CF-CARRIER-CLP-TOL-PCT
112103*        ELSE
112103*            MOVE ZERO           TO  CF-CARRIER-CLP-TOL-PCT
112103         END-IF
092705         IF BLCOMML > ZEROS
092705            MOVE WS-SPP-LEASE-COMM TO CF-CARRIER-LEASE-COMM
092705         END-IF
112103     END-IF
01652
01653 *    NOTE *******************************************************
01654 * DMD CUSTOM CODE        EDIT CALCULATE PREMIUM FLAG            *
01655 *         *******************************************************.
01656      IF CREDIT-SESSION
01657       IF PI-COMPANY-ID NOT = 'DMD'
01658         MOVE 'Y'                 TO BCLCPRMI
01659         MOVE +1                  TO BCLCPRML.
01660
01661      IF CREDIT-SESSION
01662         IF BCLCPRMI = 'Y' OR 'N'
01663            MOVE BCLCPRMI       TO CF-RATING-SWITCH
01664         ELSE
01665            MOVE AL-UABON       TO BCLCPRMA
01666            MOVE -1             TO BCLCPRML
01667            MOVE ER-8017        TO EMI-ERROR
01668            PERFORM 9900-ERROR-FORMAT
01669            PERFORM 8200-SEND-DATAONLY
01670            GO TO 9100-RETURN-TRAN.
01671
01672      GO TO 5990-EXIT.
01673
01674  5910-NOT-CREDIT-SESSION.
112103*    IF ASECPAYL GREATER ZERO
112103*        MOVE ASECPAYI           TO  CF-SECPAY-SWITCH.
112103
01675      IF ACONAMEL GREATER ZERO
01676          MOVE ACONAMEI           TO  CF-MAIL-TO-NAME.
01677
01678      IF ACAREOFL GREATER ZERO
01679          MOVE ACAREOFI           TO  CF-IN-CARE-OF.
01680
01681      IF AADDR1L GREATER ZERO
01682          MOVE AADDR1I            TO  CF-ADDRESS-LINE-1.
01683
01684      IF AADDR2L GREATER ZERO
01685          MOVE AADDR2I            TO  CF-ADDRESS-LINE-2.
01686
01687      IF ACITYSTL GREATER ZERO
01688          MOVE ACITYSTI           TO  CF-CITY-STATE.
01689
01690      IF APHONEL GREATER ZERO
01691          
      * EXEC CICS BIF DEEDIT
01692 *            FIELD  (APHONEI)
01693 *            LENGTH (APHONE-LENGTH)
01694 *        END-EXEC
      *    MOVE '@"L                   #   #00005200' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APHONEI, 
                 APHONE-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01695          MOVE APHONEI            TO  CF-PHONE-NO
01696                                      APHONEO
01697          INSPECT APHONEO CONVERTING SPACES TO '-'.
01698
01699      IF CF-ZIP-CODE-NUM NUMERIC  AND
01700         CF-ZIP-CODE-NUM NOT = ZEROS
01701          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
01702          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
01703
01704      IF AZIPL NOT = ZERO
01705          MOVE AZIPI              TO  WS-ZIP-CODE
01706          MOVE ZEROS              TO  CF-ZIP-CODE-NUM
01707      ELSE
01708          GO TO 5915-CONTINUE.
01709
01710      IF WS-CANADIAN-ZIP
01711          IF WS-ZIP-4 = SPACE  OR  '-'
01712              MOVE WS-ZIP-CAN-2-POST1   TO CF-CAN-POSTAL-1
01713              MOVE WS-ZIP-CAN-2-POST2   TO CF-CAN-POSTAL-2
01714          ELSE
01715              MOVE WS-ZIP-CAN-1-POST1   TO CF-CAN-POSTAL-1
01716              MOVE WS-ZIP-CAN-1-POST2   TO CF-CAN-POSTAL-2
01717      ELSE
01718          IF WS-ZIP-6 = SPACE  OR  '-'
01719              MOVE WS-ZIP-AM-2-CODE     TO CF-ZIP-PRIME
01720              MOVE WS-ZIP-AM-2-PLUS4    TO CF-ZIP-PLUS4
01721          ELSE
01722              MOVE WS-ZIP-AM-1-CODE     TO CF-ZIP-PRIME
01723              MOVE WS-ZIP-AM-1-PLUS4    TO CF-ZIP-PLUS4.
01724
01725  5915-CONTINUE.
01726
01727      IF ADOMSTL GREATER ZERO
01728          MOVE ADOMSTI            TO  CF-DOMICILE-STATE.
112103
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE ZERO               TO  CF-CARRIER-CLP-TOL-PCT
112103*    ELSE
112103*        IF ACLPTOLL GREATER ZERO
112103*            MOVE ACLPTOLI       TO  CF-CARRIER-CLP-TOL-PCT
112103*        ELSE
112103*            MOVE ZERO           TO  CF-CARRIER-CLP-TOL-PCT
112103*        END-IF
112103*    END-IF.
01729
01730      IF CLAIM-SESSION
01731         IF ALPHCHL GREATER ZERO
01732             IF ALPHCHI ALPHABETIC
01733                MOVE AL-UANON       TO ALPHCHA
01734                MOVE  ALPHCHI       TO CF-LAST-ALPHA-CHARACTER
01735             ELSE
01736                MOVE AL-UNBON       TO ALPHCHA
01737                MOVE -1             TO ALPHCHL
01738                MOVE ER-8128        TO EMI-ERROR
01739                PERFORM 9900-ERROR-FORMAT.
01740
01741      EJECT
01742      IF ACLNAML GREATER ZERO
01743          MOVE ACLNAMI            TO  CF-CLAIM-NO-METHOD.
01744
01745      IF ACKNAML GREATER ZERO
01746          MOVE ACKNAMI            TO  CF-CHECK-NO-METHOD.
01747
01748      IF ACLAIML GREATER ZERO
01749          
      * EXEC CICS BIF DEEDIT
01750 *            FIELD  (ACLAIMI)
01751 *            LENGTH (8)
01752 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005268' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACLAIMI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01753          MOVE ACLAIMO            TO  CF-CLAIM-COUNTER.
01754
01755      IF ACHECKL GREATER ZERO
01756          
      * EXEC CICS BIF DEEDIT
01757 *            FIELD  (ACHECKI)
01758 *            LENGTH (8)
01759 *        END-EXEC
           MOVE 8
             TO DFHEIV11
      *    MOVE '@"L                   #   #00005275' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACHECKI, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01760          MOVE ACHECKO            TO  CF-CHECK-COUNTER.
01761
01762      IF ALAL GREATER ZERO
01763          MOVE ALAI               TO  CF-LETTER-RESEND-OPT
01764          INSPECT CF-LETTER-RESEND-OPT CONVERTING 'YN' TO '1 '.
01765
01766      IF AEXPCML GREATER ZERO
01767          MOVE AEXPCMI            TO  CF-EXPENSE-METHOD.
01768
01769      IF AEXPCPL GREATER ZERO
01770          
      * EXEC CICS BIF DEEDIT
01771 *            FIELD  (AEXPCPI)
01772 *            LENGTH (AEXPCP-LENGTH)
01773 *        END-EXEC
      *    MOVE '@"L                   #   #00005289' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCPI, 
                 AEXPCP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01774          MOVE AEXPCPI            TO  CF-EXPENSE-PERCENT
01775                                      AEXPCPO.
01776
01777      IF AEXPCAL GREATER ZERO
01778          
      * EXEC CICS BIF DEEDIT
01779 *            FIELD  (AEXPCAI)
01780 *            LENGTH (AEXPCA-LENGTH)
01781 *        END-EXEC
      *    MOVE '@"L                   #   #00005297' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035323937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AEXPCAI, 
                 AEXPCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01782          MOVE AEXPCAI            TO  CF-EXPENSE-DOLLAR
01783                                      AEXPCAO.
01784      IF CLAIM-SESSION
01785         IF ABRETRL GREATER ZERO
01786            IF ABRETRI NUMERIC
01787               MOVE ABRETRI   TO CF-BUILD-RETRIEVE-AFTER-MONTHS
01788            ELSE
01789               MOVE AL-UNBON           TO ABRETRA
01790               MOVE -1                 TO ABRETRL
01791               MOVE ER-8127            TO EMI-ERROR
01792               PERFORM 9900-ERROR-FORMAT.
01793
01794      IF ACLCML GREATER ZERO
01795          MOVE ACLCMI             TO  CF-CLAIM-CALC-METHOD.
01796
01797      IF ARESMANL GREATER ZERO
01798          MOVE ARESMANI           TO  CF-MANUAL-SW
01799          INSPECT CF-MANUAL-SW CONVERTING 'YN' TO '1 '.
01800
01801      IF ARESCDTL GREATER ZERO
01802          MOVE ARESCDTI           TO  CF-FUTURE-SW
01803          INSPECT CF-FUTURE-SW CONVERTING 'YN' TO '1 '.
01804
01805      IF ARESIBNL GREATER ZERO
01806          MOVE ARESIBNI           TO  CF-IBNR-SW
01807          INSPECT CF-IBNR-SW CONVERTING 'YN' TO '1 '.
01808
01809      IF ARESPTCL GREATER ZERO
01810          MOVE ARESPTCI           TO  CF-PTC-SW
01811                                      CF-PTC-LF-SW
01812          INSPECT CF-PTC-SW CONVERTING 'YN' TO '1 '
01813          INSPECT CF-PTC-LF-SW CONVERTING 'YN' TO '1 '.
01814
01815      IF ACDTAL GREATER ZERO
01816          MOVE ACDTAI             TO  CF-CDT-ACCESS-METHOD.
01817
01818      IF APCTCDTL GREATER ZERO
01819          
      * EXEC CICS BIF DEEDIT
01820 *            FIELD  (APCTCDTI)
01821 *            LENGTH (APCTCDT-LENGTH)
01822 *        END-EXEC
      *    MOVE '@"L                   #   #00005338' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APCTCDTI, 
                 APCTCDT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01823          MOVE APCTCDTI           TO  CF-PERCENT-OF-CDT
01824                                      APCTCDTO.
01825
01826      IF IBNRPCTL GREATER ZERO
01827          
      * EXEC CICS BIF DEEDIT
01828 *            FIELD  (IBNRPCTI)
01829 *            LENGTH (IBNRPCT-LENGTH)
01830 *        END-EXEC
      *    MOVE '@"L                   #   #00005346' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 IBNRPCTI, 
                 IBNRPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01831          MOVE IBNRPCTI           TO  CF-IBNR-PERCENT
01832                                      IBNRPCTO.
01833
01834      IF AUEPPCTL GREATER ZERO
01835          
      * EXEC CICS BIF DEEDIT
01836 *            FIELD  (AUEPPCTI)
01837 *            LENGTH (AUEPPCT-LENGTH)
01838 *        END-EXEC
      *    MOVE '@"L                   #   #00005354' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AUEPPCTI, 
                 AUEPPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01839          MOVE AUEPPCTI           TO  CF-IBNR-UEPRM-PERCENT
01840                                      AUEPPCTO.
01841
01842      IF AR78PCTL GREATER ZERO
01843          
      * EXEC CICS BIF DEEDIT
01844 *            FIELD  (AR78PCTI)
01845 *            LENGTH (AR78PCT-LENGTH)
01846 *        END-EXEC
      *    MOVE '@"L                   #   #00005362' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 AR78PCTI, 
                 AR78PCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01847          MOVE AR78PCTI           TO  CF-IBNR-R78-PERCENT
01848                                      AR78PCTO.
01849
01850      IF APROPCTL GREATER ZERO
01851          
      * EXEC CICS BIF DEEDIT
01852 *            FIELD  (APROPCTI)
01853 *            LENGTH (APROPCT-LENGTH)
01854 *        END-EXEC
      *    MOVE '@"L                   #   #00005370' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 APROPCTI, 
                 APROPCT-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01855          MOVE APROPCTI           TO  CF-IBNR-PRO-PERCENT
01856                                      APROPCTO.
01857
01858      IF ALQCAL GREATER ZERO
01859          
      * EXEC CICS BIF DEEDIT
01860 *            FIELD  (ALQCAI)
01861 *            LENGTH (ALQCA-LENGTH)
01862 *        END-EXEC
      *    MOVE '@"L                   #   #00005378' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCAI, 
                 ALQCA-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01863          MOVE ALQCAI             TO  CF-CALC-AMT-TOL
01864                                      ALQCAO.
01865
01866      IF ALMRPL GREATER ZERO
01867          
      * EXEC CICS BIF DEEDIT
01868 *            FIELD  (ALMRPI)
01869 *            LENGTH (ALMRP-LENGTH)
01870 *        END-EXEC
      *    MOVE '@"L                   #   #00005386' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMRPI, 
                 ALMRP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01871          MOVE ALMRPI             TO  CF-MAX-REG-PMT
01872                                      ALMRPO.
01873
01874      IF ALQCDL GREATER ZERO
01875          
      * EXEC CICS BIF DEEDIT
01876 *            FIELD  (ALQCDI)
01877 *            LENGTH (ALQCD-LENGTH)
01878 *        END-EXEC
      *    MOVE '@"L                   #   #00005394' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALQCDI, 
                 ALQCD-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01879          MOVE ALQCDI             TO  CF-CALC-DAYS-TOL
01880                                      ALQCDO.
01881      IF ALMDPPL GREATER ZERO
01882          
      * EXEC CICS BIF DEEDIT
01883 *            FIELD  (ALMDPPI)
01884 *            LENGTH (ALMDPP-LENGTH)
01885 *        END-EXEC
      *    MOVE '@"L                   #   #00005401' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMDPPI, 
                 ALMDPP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01886          MOVE ALMDPPI            TO  CF-MAX-REG-DAYS
01887                                      ALMDPPO.
01888
01889      IF ALDBCL GREATER ZERO
01890          
      * EXEC CICS BIF DEEDIT
01891 *            FIELD  (ALDBCI)
01892 *            LENGTH (ALDBC-LENGTH)
01893 *        END-EXEC
      *    MOVE '@"L                   #   #00005409' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALDBCI, 
                 ALDBC-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01894          MOVE ALDBCI             TO  CF-DAYS-BEFORE-CLOSED
01895                                      ALDBCO.
01896
01897      IF ALMAPL GREATER ZERO
01898          
      * EXEC CICS BIF DEEDIT
01899 *            FIELD  (ALMAPI)
01900 *            LENGTH (ALMAP-LENGTH)
01901 *        END-EXEC
      *    MOVE '@"L                   #   #00005417' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPI, 
                 ALMAP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01902          MOVE ALMAPI             TO  CF-MAX-AUTO-PMT
01903                                      ALMAPO.
01904
01905      IF ALMBPL GREATER ZERO
01906          
      * EXEC CICS BIF DEEDIT
01907 *            FIELD  (ALMBPI)
01908 *            LENGTH (ALMBP-LENGTH)
01909 *        END-EXEC
      *    MOVE '@"L                   #   #00005425' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMBPI, 
                 ALMBP-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01910          MOVE ALMBPI             TO  CF-MONTHS-BEFORE-PURGED
01911                                      ALMBPO.
01912
01913      IF ALMAPML GREATER ZERO
01914          
      * EXEC CICS BIF DEEDIT
01915 *            FIELD  (ALMAPMI)
01916 *            LENGTH (ALMAPM-LENGTH)
01917 *        END-EXEC
      *    MOVE '@"L                   #   #00005433' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ALMAPMI, 
                 ALMAPM-LENGTH
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01918          MOVE ALMAPMI            TO  CF-MAX-AUTO-MOS
01919                                      ALMAPMO.
01920
01921  5990-EXIT.
01922      EXIT.
01923  EJECT
01924  6000-SHOW-RECORD SECTION.
01925      
      * EXEC CICS HANDLE CONDITION
01926 *        NOTFND  (6060-NOT-FOUND)
01927 *    END-EXEC.
      *    MOVE '"$I                   ! # #00005444' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303035343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01928
01929      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.
01930
01931      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.
01932      MOVE '6'                    TO  WS-CFK-RECORD-TYPE.
01933      MOVE PI-CARRIER-NUMBER      TO  WS-CFK-CARRIER-NO.
01934      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.
01935
01936      
      * EXEC CICS READ
01937 *        DATASET (WS-CONTROL-FILE-DSID)
01938 *        RIDFLD  (WS-CONTROL-FILE-KEY)
01939 *        SET     (ADDRESS OF CONTROL-FILE)
01940 *    END-EXEC.
      *    MOVE '&"S        E          (   #00005455' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303035343535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01941
01942      IF NOT CREDIT-SESSION
01943          GO TO 6010-NOT-CREDIT-SESSION.
01944
01945      MOVE CF-CARRIER-CNTL        TO  BCARIERO.
01946      MOVE AL-UANON               TO  BCARIERA.
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF           TO  BSPLABLA
112103         MOVE AL-SADOF           TO  BSECPAYA
112103     ELSE
112103         MOVE CF-SECPAY-SWITCH   TO  BSECPAYO
112103     END-IF.
112103
01947      MOVE CF-MAIL-TO-NAME        TO  BCONAMEO.
01948      MOVE CF-IN-CARE-OF          TO  BCAREOFO.
01949      MOVE CF-ADDRESS-LINE-1      TO  BADDR1O.
01950      MOVE CF-ADDRESS-LINE-2      TO  BADDR2O.
01951      MOVE CF-CITY-STATE          TO  BCITYSTO.
01952
01953      IF CREDIT-SESSION
01954         MOVE CF-RATING-SWITCH    TO  BCLCPRMO.
01955
01956      IF CF-ZIP-CODE-NUM NUMERIC  AND
01957         CF-ZIP-CODE-NUM NOT = ZEROS
01958          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
01959          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
01960
01961      MOVE SPACES                   TO WS-ZIP-CODE.
01962      IF CF-CANADIAN-POST-CODE
01963          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
01964          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
01965      ELSE
01966          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
01967          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
01968              MOVE '-'              TO WS-ZIP-AM-2-DASH
01969              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
01970
01971      MOVE WS-ZIP-CODE            TO  BZIPO.
01972
01973      MOVE CF-DOMICILE-STATE      TO  BDOMSTO.
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF               TO  BCTLABLA
112103                                         BCLPTOLA
092705                                         BLCOMMA
112103     ELSE
112103         MOVE CF-CARRIER-CLP-TOL-PCT TO  BCLPTOLO
092705         MOVE CF-CARRIER-LEASE-COMM  TO  BLCOMMO
112103     END-IF.
01974      MOVE CF-PHONE-NO            TO  BPHONEO.
01975      INSPECT BPHONEO CONVERTING SPACES TO '-'.
01976
01977      IF CF-CR-TOL-PREM NUMERIC
01978          IF CF-CR-TOL-PREM NOT = ZEROS
01979              MOVE CF-CR-TOL-PREM        TO  BPRMTOLO.
01980
01981      IF CF-CR-TOL-REFUND NUMERIC
01982          IF CF-CR-TOL-REFUND NOT = ZEROS
01983              MOVE CF-CR-TOL-REFUND      TO  BREFTOLO.
01984
01985      IF CF-CR-OVR-SHT-AMT  NUMERIC
01986           IF CF-CR-OVR-SHT-AMT > +0
01987               MOVE CF-CR-OVR-SHT-AMT TO BOVSAMTO
01988           END-IF
01989      END-IF.
01990
01991      IF CF-CR-TOL-PREM-PCT NUMERIC
01992          IF CF-CR-TOL-PREM-PCT NOT = ZEROS
01993              MOVE CF-CR-TOL-PREM-PCT    TO  BPRMPCTO.
01994
01995      IF CF-CR-TOL-REFUND-PCT NUMERIC
01996          IF CF-CR-TOL-REFUND-PCT NOT = ZEROS
01997              MOVE CF-CR-TOL-REFUND-PCT  TO  BREFPCTO.
01998
01999      IF CF-CR-OVR-SHT-PCT  NUMERIC
02000           IF CF-CR-OVR-SHT-PCT > +0
02001              MOVE CF-CR-OVR-SHT-PCT TO  BOVSPCTO
02002           END-IF
02003      END-IF.
02004
02005      IF CREDIT-SESSION
02006        MOVE CF-RATING-SWITCH            TO  BCLCPRMO.
02007
02008      MOVE AL-UNNON                      TO  BPRMTOLA  BREFTOLA
02009                                             BPRMPCTA  BREFPCTA.
02010
02011      GO TO 6030-DISPLAY-MAINT.
02012
02013  6010-NOT-CREDIT-SESSION.
02014      MOVE CF-CARRIER-CNTL        TO  ACARIERO.
02015      MOVE AL-UANON               TO  ACARIERA.
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF           TO  ASPLABLA
112103*        MOVE AL-SADOF           TO  ASECPAYA
112103*    ELSE
112103*        MOVE CF-SECPAY-SWITCH   TO  ASECPAYO
112103*    END-IF.
02016      MOVE CF-MAIL-TO-NAME        TO  ACONAMEO.
02017      MOVE CF-IN-CARE-OF          TO  ACAREOFO.
02018      MOVE CF-ADDRESS-LINE-1      TO  AADDR1O.
02019      MOVE CF-ADDRESS-LINE-2      TO  AADDR2O.
02020      MOVE CF-CITY-STATE          TO  ACITYSTO.
02021
02022      IF CF-ZIP-CODE-NUM NUMERIC  AND
02023         CF-ZIP-CODE-NUM NOT = ZEROS
02024          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
02025          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
02026
02027      MOVE SPACES                   TO WS-ZIP-CODE.
02028      IF CF-CANADIAN-POST-CODE
02029          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
02030          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
02031      ELSE
02032          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
02033          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
02034              MOVE '-'              TO WS-ZIP-AM-2-DASH
02035              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
02036
02037      MOVE WS-ZIP-CODE            TO  AZIPO.
02039      MOVE CF-DOMICILE-STATE      TO  ADOMSTO.
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF               TO  ACTLABLA
112103*        MOVE AL-SADOF               TO  ACLPTOLA
112103*    ELSE
112103*        MOVE CF-CARRIER-CLP-TOL-PCT TO  ACLPTOLO
112103*    END-IF.
02040
02041      IF CLAIM-SESSION
02042         MOVE CF-LAST-ALPHA-CHARACTER TO ALPHCHO.
02043
02044      MOVE CF-PHONE-NO            TO  APHONEO.
02045 *    INSPECT APHONEI CONVERTING SPACES TO '-'.
02046      INSPECT APHONEO CONVERTING SPACES TO '-'.
02047      EJECT
02048
02049      MOVE CF-CLAIM-NO-METHOD     TO  ACLNAMO.
02050      MOVE CF-CLAIM-COUNTER       TO  ACLAIMO.
02051
02052      IF PI-PROCESSOR-ID = 'LGXX'
02053          MOVE AL-UNNOF           TO  ACLAIMA.
02054
02055      MOVE CF-CHECK-NO-CONTROL    TO  ACKNAMO.
02056
02057      MOVE CF-CHECK-COUNTER       TO  ACHECKO.
02058
02059      IF PI-PROCESSOR-ID = 'LGXX'
02060          MOVE AL-UNNOF           TO  ACHECKA.
02061
02062      MOVE CF-EXPENSE-METHOD      TO  AEXPCMO.
02063      MOVE CF-EXPENSE-PERCENT     TO  AEXPCPO.
02064      MOVE CF-EXPENSE-DOLLAR      TO  AEXPCAO.
02065
02066      IF CLAIM-SESSION
02067         MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS TO ABRETRO.
02068
02069      MOVE CF-LETTER-RESEND-OPT   TO  ALAO.
02070      INSPECT ALAO CONVERTING ' 1' TO 'NY'.
02071
02072      MOVE CF-MANUAL-SW           TO  ARESMANO.
02073      INSPECT ARESMANO CONVERTING ' 1' TO 'NY'.
02074
02075      MOVE CF-FUTURE-SW           TO  ARESCDTO.
02076      INSPECT ARESCDTO CONVERTING ' 1' TO 'NY'.
02077
02078      MOVE CF-PTC-SW              TO  ARESPTCO.
02079      INSPECT ARESPTCO CONVERTING ' 1' TO 'NY'.
02080
02081      MOVE CF-IBNR-SW             TO  ARESIBNO.
02082      INSPECT ARESIBNO CONVERTING ' 1' TO 'NY'.
02083
02084      MOVE CF-CDT-ACCESS-METHOD   TO  ACDTAO.
02085      MOVE CF-PERCENT-OF-CDT      TO  APCTCDTO.
02086      MOVE CF-IBNR-PERCENT        TO  IBNRPCTO.
02087
02088      IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC
02089          MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.
02090      IF CF-IBNR-R78-PERCENT NOT NUMERIC
02091          MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.
02092      IF CF-IBNR-PRO-PERCENT NOT NUMERIC
02093          MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.
02094
02095      MOVE CF-IBNR-UEPRM-PERCENT  TO  AUEPPCTO.
02096      MOVE CF-IBNR-R78-PERCENT    TO  AR78PCTO.
02097      MOVE CF-IBNR-PRO-PERCENT    TO  APROPCTO.
02098
02099      MOVE CF-CLAIM-CALC-METHOD   TO  ACLCMO.
02100
02101      MOVE CF-CALC-AMT-TOL        TO  ALQCAO.
02102      MOVE CF-MAX-REG-PMT         TO  ALMRPO.
02103      MOVE CF-MAX-REG-DAYS        TO  ALMDPPO.
02104      MOVE CF-MAX-AUTO-PMT        TO  ALMAPO.
02105      MOVE CF-MAX-AUTO-MOS        TO  ALMAPMO.
02106      MOVE CF-CALC-DAYS-TOL       TO  ALQCDO.
02107
02108      MOVE CF-DAYS-BEFORE-CLOSED  TO  ALDBCO.
02109      MOVE CF-MONTHS-BEFORE-PURGED  TO  ALMBPO.
032813
032813     MOVE CF-CARRIER-NEXT-AUDIT-CHK-NO TO ANXTAUDO.
02110
02111  6030-DISPLAY-MAINT.
02112      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
02113      MOVE SPACES                 TO  DC-OPTION-CODE.
02114      PERFORM 8500-DATE-CONVERSION.
02115
02116      IF CREDIT-SESSION
02117          MOVE DC-GREG-DATE-1-EDIT    TO  BLUDATEO
02118          MOVE CF-LAST-MAINT-HHMMSS   TO  BLUTIMEO
02119          INSPECT BLUTIMEI CONVERTING SPACES TO '.'
02120          MOVE CF-LAST-MAINT-BY       TO  BLUBYO
02121          MOVE -1                     TO  BMAINTL
02122      ELSE
02123          MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO
02124          MOVE CF-LAST-MAINT-HHMMSS   TO  ALUTIMEO
02125          INSPECT ALUTIMEI CONVERTING SPACES TO '.'
02126          MOVE CF-LAST-MAINT-BY       TO  ALUBYO
02127          MOVE -1                     TO  AMAINTL.
02128
02129      MOVE +1                     TO  PI-BROWSE-SW.
02130      ADD  +1                     TO  WS-CFK-SEQUENCE-NO.
02131
02132      PERFORM 8000-DISPLAY-RECORDS THRU 8010-DISPLAY-RECORDS.
02133
02134      
      * EXEC CICS ENDBR
02135 *        DATASET (WS-CONTROL-FILE-DSID)
02136 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005681' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035363831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02137
02138      MOVE WS-CFK-CARRIER-NO      TO  PI-NEXT-CARRIER-NUMBER.
02139
02140      PERFORM 8100-SEND-INITIAL-MAP.
02141      GO TO 9100-RETURN-TRAN.
02142
02143  6060-NOT-FOUND.
02144      MOVE AL-UNBON               TO  ACARIERA.
02145      MOVE -1                     TO  ACARIERL.
02146      MOVE ER-0006                TO  EMI-ERROR.
02147      PERFORM 9900-ERROR-FORMAT.
02148
02149      PERFORM 8200-SEND-DATAONLY.
02150      GO TO 9100-RETURN-TRAN.
02151
02152      EJECT
02153
02154  8000-DISPLAY-RECORDS SECTION.
02155      
      * EXEC CICS HANDLE CONDITION
02156 *        NOTFND  (8060-DISPLAY-RECORDS)
02157 *        ENDFILE (8040-DISPLAY-RECORDS)
02158 *    END-EXEC.
      *    MOVE '"$I''                  ! $ #00005702' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303035373032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02159
02160      
      * EXEC CICS STARTBR
02161 *         DATASET (WS-CONTROL-FILE-DSID)
02162 *         RIDFLD  (WS-CONTROL-FILE-KEY)
02163 *         GTEQ
02164 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00005707' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02165
02166  8010-DISPLAY-RECORDS.
02167      
      * EXEC CICS READNEXT
02168 *        DATASET (WS-CONTROL-FILE-DSID)
02169 *        RIDFLD  (WS-CONTROL-FILE-KEY)
02170 *        SET     (ADDRESS OF CONTROL-FILE)
02171 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00005714' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303035373134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-CONTROL-FILE-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02172
02173      IF WS-CFK-COMPANY-ID NOT = PI-COMPANY-ID
02174          GO TO 8040-DISPLAY-RECORDS.
02175
02176      IF CF-RECORD-TYPE NOT = '6'
02177          MOVE ZERO               TO  PI-1ST-TIME-SW
02178          MOVE SPACES             TO  PI-NEXT-CARRIER-NUMBER
02179          MOVE ER-0173            TO  EMI-ERROR
02180          PERFORM 9900-ERROR-FORMAT
02181          GO TO 8050-DISPLAY-RECORDS.
02182
02183  8015-DISPLAY-RECORDS.
02184      IF LCP-ONCTR-01 =  0
02185          ADD 1 TO LCP-ONCTR-01
02186         GO TO 8020-DISPLAY-RECORDS.
02187
02188      MOVE WS-CFK-CARRIER-NO      TO  PI-NEXT-CARRIER-NUMBER.
02189      MOVE +1                     TO  PI-BROWSE-SW.
02190      GO TO 8050-DISPLAY-RECORDS.
02191
02192  8020-DISPLAY-RECORDS.
02193      IF NOT CREDIT-SESSION
02194          GO TO 8025-NOT-CREDIT-SESSION.
02195
02196      MOVE LOW-VALUES             TO  EL105BO.
02197
02198      MOVE 'S'                    TO  BMAINTO.
02199      MOVE AL-UANON               TO  BMAINTA.
02200      MOVE -1                     TO  BMAINTL.
02201      MOVE CF-CARRIER-CNTL        TO  BCARIERO
02202                                      PI-CARRIER-NUMBER.
02203      MOVE AL-UANON               TO  BCARIERA.
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF           TO  BSPLABLA
112103         MOVE AL-SADOF           TO  BSECPAYA
112103     ELSE
112103         MOVE CF-SECPAY-SWITCH   TO  BSECPAYO
112103     END-IF.
02204      MOVE CF-MAIL-TO-NAME        TO  BCONAMEO.
02205      MOVE CF-IN-CARE-OF          TO  BCAREOFO.
02206      MOVE CF-ADDRESS-LINE-1      TO  BADDR1O.
02207      MOVE CF-ADDRESS-LINE-2      TO  BADDR2O.
02208      MOVE CF-CITY-STATE          TO  BCITYSTO.
02209
02210      MOVE CF-RATING-SWITCH       TO  BCLCPRMO.
02211
02212      IF CF-ZIP-CODE-NUM NUMERIC  AND
02213         CF-ZIP-CODE-NUM NOT = ZEROS
02214          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
02215          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
02216
02217      MOVE SPACES                   TO WS-ZIP-CODE.
02218      IF CF-CANADIAN-POST-CODE
02219          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
02220          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
02221      ELSE
02222          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
02223          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
02224              MOVE '-'              TO WS-ZIP-AM-2-DASH
02225              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
02226
02227      MOVE WS-ZIP-CODE            TO  BZIPO.
02228
02229      MOVE CF-DOMICILE-STATE      TO  BDOMSTO.
062121     IF PI-COMPANY-ID = 'CID' OR 'AHL' OR 'FNL'
112103         MOVE AL-SADOF               TO  BCTLABLA
112103                                         BCLPTOLA
092705                                         BLCOMMA
112103     ELSE
112103         MOVE CF-CARRIER-CLP-TOL-PCT TO  BCLPTOLO
092705         MOVE CF-CARRIER-LEASE-COMM  TO  BLCOMMO
112103     END-IF.
02230      MOVE CF-PHONE-NO            TO  BPHONEO.
02231      INSPECT BPHONEO CONVERTING SPACES TO '-'.
02232
02233      IF CF-CR-TOL-PREM NUMERIC
02234          IF CF-CR-TOL-PREM NOT = ZEROS
02235              MOVE CF-CR-TOL-PREM        TO  BPRMTOLO.
02236
02237      IF CF-CR-TOL-REFUND NUMERIC
02238          IF CF-CR-TOL-REFUND NOT = ZEROS
02239              MOVE CF-CR-TOL-REFUND      TO  BREFTOLO.
02240
02241      IF CF-CR-OVR-SHT-AMT  NUMERIC AND
02242             CF-CR-OVR-SHT-AMT  > 0
02243         MOVE CF-CR-OVR-SHT-AMT TO  BOVSAMTO
02244      END-IF.
02245
02246      IF CF-CR-TOL-PREM-PCT NUMERIC
02247          IF CF-CR-TOL-PREM-PCT NOT = ZEROS
02248              MOVE CF-CR-TOL-PREM-PCT    TO  BPRMPCTO.
02249
02250      IF CF-CR-TOL-REFUND-PCT NUMERIC
02251          IF CF-CR-TOL-REFUND-PCT NOT = ZEROS
02252              MOVE CF-CR-TOL-REFUND-PCT  TO  BREFPCTO.
02253
02254      IF CF-CR-OVR-SHT-PCT NUMERIC AND
02255           CF-CR-OVR-SHT-PCT > 0
02256              MOVE CF-CR-OVR-SHT-PCT TO BOVSPCTO
02257      END-IF.
02258
02259      MOVE AL-UNNON                      TO  BPRMTOLA  BREFTOLA
02260                                             BPRMPCTA  BREFPCTA.
02261
02262      GO TO 8030-DISPLAY-MAINT.
02263
02264  8025-NOT-CREDIT-SESSION.
02265
02266      MOVE CF-CARRIER-CNTL        TO  ACARIERO.
02267      MOVE AL-UANON               TO  ACARIERA.
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF           TO  ASPLABLA
112103*        MOVE AL-SADOF           TO  ASECPAYA
112103*    ELSE
112103*        MOVE CF-SECPAY-SWITCH   TO  ASECPAYO
112103*    END-IF.
02268      MOVE CF-MAIL-TO-NAME        TO  ACONAMEO.
02269      MOVE CF-IN-CARE-OF          TO  ACAREOFO.
02270      MOVE CF-ADDRESS-LINE-1      TO  AADDR1O.
02271      MOVE CF-ADDRESS-LINE-2      TO  AADDR2O.
02272      MOVE CF-CITY-STATE          TO  ACITYSTO.
02273
02274      IF CF-ZIP-CODE-NUM NUMERIC  AND
02275         CF-ZIP-CODE-NUM NOT = ZEROS
02276          MOVE CF-ZIP-CODE-NUM    TO WS-ZIP-CODE-NUM
02277          MOVE WS-ZIP-CODE-NUM    TO CF-ZIP-CODE.
02278
02279      MOVE SPACES                   TO WS-ZIP-CODE.
02280      IF CF-CANADIAN-POST-CODE
02281          MOVE CF-CAN-POSTAL-1      TO WS-ZIP-CAN-2-POST1
02282          MOVE CF-CAN-POSTAL-2      TO WS-ZIP-CAN-2-POST2
02283      ELSE
02284          MOVE CF-ZIP-PRIME         TO WS-ZIP-AM-2-CODE
02285          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
02286              MOVE '-'              TO WS-ZIP-AM-2-DASH
02287              MOVE CF-ZIP-PLUS4     TO WS-ZIP-AM-2-PLUS4.
02288
02289      MOVE WS-ZIP-CODE            TO  AZIPO.
02291      MOVE CF-DOMICILE-STATE      TO  ADOMSTO.
112103*    IF PI-COMPANY-ID = 'CID'
112103*        MOVE AL-SADOF               TO  ACTLABLA
112103*        MOVE AL-SADOF               TO  ACLPTOLA
112103*    ELSE
112103*        MOVE CF-CARRIER-CLP-TOL-PCT TO  ACLPTOLO
112103*    END-IF.
02292
02293      IF CLAIM-SESSION
02294         MOVE CF-LAST-ALPHA-CHARACTER TO ALPHCHO.
02295
02296      MOVE CF-PHONE-NO            TO  APHONEO.
02297      INSPECT APHONEO CONVERTING SPACES TO '-'.
02298
02299      EJECT
02300
02301      MOVE CF-CLAIM-NO-METHOD     TO  ACLNAMO.
02302      MOVE CF-CLAIM-COUNTER       TO  ACLAIMO.
02303
02304      IF PI-PROCESSOR-ID = 'LGXX'
02305          MOVE AL-UNNOF           TO  ACLAIMA.
02306
02307      MOVE CF-CHECK-NO-CONTROL    TO  ACKNAMO.
02308
02309      MOVE CF-CHECK-COUNTER       TO  ACHECKO.
02310
02311      IF PI-PROCESSOR-ID = 'LGXX'
02312          MOVE AL-UNNOF           TO  ACHECKA.
02313
02314      MOVE CF-EXPENSE-METHOD      TO  AEXPCMO.
02315      MOVE CF-EXPENSE-PERCENT     TO  AEXPCPO.
02316      MOVE CF-EXPENSE-DOLLAR      TO  AEXPCAO.
02317
02318      IF CLAIM-SESSION
02319         MOVE CF-BUILD-RETRIEVE-AFTER-MONTHS TO ABRETRO.
032813
032813     MOVE CF-CARRIER-NEXT-AUDIT-CHK-NO TO ANXTAUDO.
02320
02321      MOVE CF-LETTER-RESEND-OPT   TO  ALAO.
02322      INSPECT ALAO CONVERTING ' 1' TO 'NY'.
02323
02324      MOVE CF-MANUAL-SW           TO  ARESMANO.
02325      INSPECT ARESMANO CONVERTING ' 1' TO 'NY'.
02326
02327      MOVE CF-FUTURE-SW           TO  ARESCDTO.
02328      INSPECT ARESCDTO CONVERTING ' 1' TO 'NY'.
02329
02330      MOVE CF-PTC-SW              TO  ARESPTCO.
02331      INSPECT ARESPTCO CONVERTING ' 1' TO 'NY'.
02332
02333      MOVE CF-IBNR-SW             TO  ARESIBNO.
02334      INSPECT ARESIBNO CONVERTING ' 1' TO 'NY'.
02335
02336      MOVE CF-CDT-ACCESS-METHOD   TO  ACDTAO.
02337      MOVE CF-PERCENT-OF-CDT      TO  APCTCDTO.
02338      MOVE CF-IBNR-PERCENT        TO  IBNRPCTO.
02339
02340      IF CF-IBNR-UEPRM-PERCENT NOT NUMERIC
02341          MOVE ZEROS              TO  CF-IBNR-UEPRM-PERCENT.
02342      IF CF-IBNR-R78-PERCENT NOT NUMERIC
02343          MOVE ZEROS              TO  CF-IBNR-R78-PERCENT.
02344      IF CF-IBNR-PRO-PERCENT NOT NUMERIC
02345          MOVE ZEROS              TO  CF-IBNR-PRO-PERCENT.
02346
02347      MOVE CF-IBNR-UEPRM-PERCENT  TO  AUEPPCTO.
02348      MOVE CF-IBNR-R78-PERCENT    TO  AR78PCTO.
02349      MOVE CF-IBNR-PRO-PERCENT    TO  APROPCTO.
02350
02351      MOVE CF-CLAIM-CALC-METHOD   TO  ACLCMO.
02352
02353      MOVE CF-CALC-AMT-TOL        TO  ALQCAO.
02354      MOVE CF-MAX-REG-PMT         TO  ALMRPO.
02355      MOVE CF-MAX-REG-DAYS        TO  ALMDPPO.
02356      MOVE CF-MAX-AUTO-PMT        TO  ALMAPO.
02357      MOVE CF-MAX-AUTO-MOS        TO  ALMAPMO.
02358      MOVE CF-CALC-DAYS-TOL       TO  ALQCDO.
02359
02360      MOVE CF-DAYS-BEFORE-CLOSED  TO  ALDBCO.
02361      MOVE CF-MONTHS-BEFORE-PURGED  TO  ALMBPO.
02362
02363  8030-DISPLAY-MAINT.
02364      MOVE CF-LAST-MAINT-DT       TO  DC-BIN-DATE-1.
02365      MOVE SPACES                 TO  DC-OPTION-CODE.
02366      PERFORM 8500-DATE-CONVERSION.
02367
02368      IF CREDIT-SESSION
02369          MOVE DC-GREG-DATE-1-EDIT    TO  BLUDATEO
02370          MOVE CF-LAST-MAINT-HHMMSS   TO  BLUTIMEO
02371          INSPECT BLUTIMEI CONVERTING SPACES TO '.'
02372          MOVE CF-LAST-MAINT-BY       TO  BLUBYO
02373      ELSE
02374          MOVE DC-GREG-DATE-1-EDIT    TO  ALUDATEO
02375          MOVE CF-LAST-MAINT-HHMMSS   TO  ALUTIMEO
02376          INSPECT ALUTIMEI CONVERTING SPACES TO '.'
02377          MOVE CF-LAST-MAINT-BY       TO  ALUBYO.
02378
02379      MOVE -1                     TO  AMAINTL.
02380
02381      GO TO 8010-DISPLAY-RECORDS.
02382
02383  8040-DISPLAY-RECORDS.
02384      MOVE +1                     TO  PI-END-OF-FILE.
02385      MOVE ER-0173                TO  EMI-ERROR.
02386      MOVE SPACES                 TO  PI-NEXT-CARRIER-NUMBER.
02387      PERFORM 9900-ERROR-FORMAT.
02388
02389  8050-DISPLAY-RECORDS.
02390      
      * EXEC CICS ENDBR
02391 *        DATASET (WS-CONTROL-FILE-DSID)
02392 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00005964' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303035393634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-CONTROL-FILE-DSID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02393
02394      PERFORM 8100-SEND-INITIAL-MAP.
02395      GO TO 9100-RETURN-TRAN.
02396
02397  8060-DISPLAY-RECORDS.
02398      MOVE AL-UNBON               TO  ACARIERA.
02399      MOVE -1                     TO  ACARIERL.
02400      MOVE ER-0006                TO  EMI-ERROR.
02401      PERFORM 9900-ERROR-FORMAT.
02402
02403      PERFORM 8200-SEND-DATAONLY.
02404      GO TO 9100-RETURN-TRAN.
02405
02406      EJECT
02407  8100-SEND-INITIAL-MAP SECTION.
02408      MOVE SAVE-DATE              TO  ADATEO.
02409      MOVE EIBTIME                TO  TIME-IN.
02410      MOVE TIME-OUT               TO  ATIMEO.
02411      MOVE -1                     TO  AMAINTL
02412
02413 ****DMD CUSTOM  CODE******************
02414      IF PI-COMPANY-ID = 'DMD'
02415      IF CREDIT-SESSION
02416         MOVE AL-SANOF           TO DMDSW2A
02417         MOVE AL-UANON           TO BCLCPRMA.
02418 ****DMD CUSTOM  CODE******************
02419
02420      EJECT
02421      IF CREDIT-SESSION
02422          MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O
02423      ELSE
02424          MOVE EMI-MESSAGE-AREA (1)    TO  AEMSG1O.
02425
02426      IF CLAIM-SESSION
02427         MOVE 'ALPHA'             TO ALPHLO.
02428
02429      
      * EXEC CICS SEND
02430 *        FROM   (EL105AI)
02431 *        MAPSET (WS-MAPSET-NAME)
02432 *        MAP    (WS-MAP-NAME)
02433 *        CURSOR ERASE
02434 *    END-EXEC.
           MOVE LENGTH OF
            EL105AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00006003' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL105AI, 
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
           
02435
02436  8100-EXIT.
02437      EXIT.
02438
02439      EJECT
02440  8200-SEND-DATAONLY SECTION.
02441      MOVE SAVE-DATE              TO  ADATEO.
02442      MOVE EIBTIME                TO  TIME-IN.
02443      MOVE TIME-OUT               TO  ATIMEO.
02444
02445      IF CREDIT-SESSION
02446          MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O
02447      ELSE
02448          MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.
02449
02450 ****DMD CUSTOM  CODE******************
02451      IF PI-COMPANY-ID = 'DMD'
02452      IF CREDIT-SESSION
02453         MOVE AL-SANOF           TO DMDSW2A
02454         MOVE AL-UANON           TO BCLCPRMA.
02455 ****DMD CUSTOM  CODE******************
02456
02457      IF CLAIM-SESSION
02458         MOVE 'ALPHA'             TO ALPHLO.
02459
02460      
      * EXEC CICS SEND DATAONLY
02461 *        FROM   (EL105AI)
02462 *        MAPSET (WS-MAPSET-NAME)
02463 *        MAP    (WS-MAP-NAME)
02464 *        CURSOR
02465 *    END-EXEC.
           MOVE LENGTH OF
            EL105AI
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00006034' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-MAP-NAME, 
                 EL105AI, 
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
           
02466
02467  8200-EXIT.
02468      EXIT.
02469
02470      EJECT
02471  8300-SEND-TEXT SECTION.
02472      
      * EXEC CICS SEND TEXT
02473 *        FROM   (LOGOFF-TEXT)
02474 *        LENGTH (LOGOFF-LENGTH)
02475 *        ERASE  FREEKB
02476 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00006046' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303436' TO DFHEIV0(25:11)
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
           
02477
02478      
      * EXEC CICS RETURN
02479 *    END-EXEC.
      *    MOVE '.(                    ''   #00006052' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02480
02481  8300-EXIT.
02482      EXIT.
02483
02484      EJECT
02485  8400-LOG-JOURNAL-RECORD SECTION.
02486      IF PI-JOURNAL-FILE-ID = ZERO
02487          GO TO 8400-EXIT.
02488
02489      MOVE PI-PROCESSOR-ID        TO  JP-USER-ID.
02490      MOVE WS-CONTROL-FILE-DSID   TO  JP-FILE-ID.
02491      MOVE THIS-PGM               TO  JP-PROGRAM-ID.
02492
pemuni*    EXEC CICS JOURNAL
pemuni*        JFILEID (PI-JOURNAL-FILE-ID)
pemuni*        JTYPEID (WS-JOURNAL-TYPE-ID)
pemuni*        FROM    (JOURNAL-RECORD)
pemuni*        LENGTH  (WS-JOURNAL-RECORD-LENGTH)
pemuni*    END-EXEC.
02499
02500  8400-EXIT.
02501      EXIT.
02502
02503  8500-DATE-CONVERSION SECTION.
02504      
      * EXEC CICS LINK
02505 *        PROGRAM  ('ELDATCV')
02506 *        COMMAREA (DATE-CONVERSION-DATA)
02507 *        LENGTH   (DC-COMM-LENGTH)
02508 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00006078' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036303738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02509
02510  8500-EXIT.
02511      EXIT.
02512
02513      EJECT
02514  8700-NOT-OPEN SECTION.
02515      MOVE ER-0042                TO EMI-ERROR.
02516      MOVE -1                     TO ACARIERL.
02517      PERFORM 9900-ERROR-FORMAT.
02518      PERFORM 8200-SEND-DATAONLY.
02519      GO TO 9100-RETURN-TRAN.
02520
02521  8700-EXIT.
02522       EXIT.
02523
02524  8800-DUPREC SECTION.
02525      MOVE ER-0497                TO EMI-ERROR.
02526      MOVE -1                     TO ACARIERL.
02527      PERFORM 9900-ERROR-FORMAT.
02528      PERFORM 8200-SEND-DATAONLY.
02529      GO TO 9100-RETURN-TRAN.
02530
02531  8800-EXIT.
02532       EXIT.
02533
02534      EJECT
02535  9000-RETURN-CICS SECTION.
02536      MOVE 'EL005   '             TO  THIS-PGM.
02537      MOVE EIBAID                 TO  PI-ENTRY-CD-1.
02538      PERFORM 9300-XCTL.
02539
02540  9000-EXIT.
02541      EXIT.
02542
02543  9100-RETURN-TRAN SECTION.
02544      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.
02545      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.
02546
02547      
      * EXEC CICS RETURN
02548 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02549 *        LENGTH   (PI-COMM-LENGTH)
02550 *        TRANSID  (WS-TRANS-ID)
02551 *    END-EXEC.
      *    MOVE '.(CT                  ''   #00006121' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02552      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL105' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
02553
02554  9100-EXIT.
02555      EXIT.
02556
02557  9300-XCTL SECTION.
02558      MOVE DFHENTER               TO  EIBAID
02559
02560      
      * EXEC CICS XCTL
02561 *        PROGRAM  (THIS-PGM)
02562 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
02563 *        LENGTH   (PI-COMM-LENGTH)
02564 *    END-EXEC.
      *    MOVE '.$C                   %   #00006134' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 THIS-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02565
02566  9300-EXIT.
02567      EXIT.
02568
02569      EJECT
02570  9400-CLEAR SECTION.
02571      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.
02572      PERFORM 9300-XCTL.
02573
02574  9400-EXIT.
02575      EXIT.
02576
02577  9600-PGMIDERR SECTION.
02578      
      * EXEC CICS HANDLE CONDITION
02579 *        PGMIDERR (8300-SEND-TEXT)
02580 *    END-EXEC.
      *    MOVE '"$L                   ! % #00006152' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303036313532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02581
02582      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM
02583                                      LOGOFF-PGM.
02584
02585      MOVE 'EL005   '             TO  THIS-PGM.
02586      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.
02587      MOVE SPACES                 TO  PI-ENTRY-CD-1.
02588      PERFORM 9300-XCTL.
02589
02590  9600-EXIT.
02591      EXIT.
02592
02593
02594      EJECT
02595  9900-ERROR-FORMAT SECTION.
02596      
      * EXEC CICS LINK
02597 *        PROGRAM  ('EL001')
02598 *        COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)
02599 *        LENGTH   (EMI-COMM-LENGTH)
02600 *    END-EXEC.
           MOVE 'EL001' TO DFHEIV1
      *    MOVE '."C                   (   #00006170' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02601
02602  9900-EXIT.
02603      EXIT.
02604
02605      EJECT
02606  9990-ERROR SECTION.
02607      MOVE DFHEIBLK               TO EMI-LINE1.
02608
02609      
      * EXEC CICS LINK
02610 *        PROGRAM  ('EL004')
02611 *        COMMAREA (EMI-LINE1)
02612 *        LENGTH   (72)
02613 *    END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00006183' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036313833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02614
02615      PERFORM 8200-SEND-DATAONLY.
02616      GO TO 9100-RETURN-TRAN.
02617
02618  9990-EXIT.
02619      EXIT.
02620
02621  9995-SECURITY-VIOLATION.
02622 *           COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00006213' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303036323133' TO DFHEIV0(25:11)
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

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL105' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMIDERR,
                     8700-NOT-OPEN,
                     8800-DUPREC,
                     9990-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 6060-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8060-DISPLAY-RECORDS,
                     8040-DISPLAY-RECORDS
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL105' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
