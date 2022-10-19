00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL1523.
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 07/18/94 11:29:57.
00007 *                            VMOD=2.006.
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
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023
00024 *REMARKS.    TRANSACTION - EXL1 - LETTER GENERATOR.
00025 *            THIS PROGRAM GENERATES LETTERS WHEN PROPERLY CALLED
00026 *            FROM ANOTHER ONLINE PROGRAM.  THIS PROGRAM DOES NOT
00027 *            CAUSE LETTERS TO BE PRINTED.  INSTEAD IT AUTOMATICALL
00028 *            ENTERS THEM INTO THE ARCHIVE FILE.
00029 *
00030 *
00031 *
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
031307* 031307                   PEMA  ADD 3.1 VARIABLE
090108* 090108    2007041300006  AJRA  ADD VARIABLE @@AUTOPYDT FOR AUTO
033110* 033110  CR2009122800001  AJRA  NAPERSOFT
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00032
00033  ENVIRONMENT DIVISION.
00034
00035                                  EJECT
00036  DATA DIVISION.
00037  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
pemuni*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
pemuni*77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
pemuni*                                  USAGE POINTER.
pemuni 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
pemuni 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
00040                                    USAGE POINTER.
00041  77  FILLER  PIC X(32) VALUE '********************************'.
00042  77  FILLER  PIC X(32) VALUE '*    EL1523 WORKING STORAGE    *'.
00043  77  FILLER  PIC X(32) VALUE '********** VMOD=2.006 **********'.
031307 77  B1                          PIC S999 COMP-3 VALUE +0.
00044
00045  01  W-PROGRAM-WORK-AREAS.
00046      12  FILLER                  PIC  X(17)
00047                                  VALUE 'PROGRAM WORK AREA'.
00048      12  W-ASKTIME-CTR           PIC S9(08) COMP.
00049      12  W-CORR-TRLR-SEQ         PIC S9(04) COMP.
00050      12  W-PI-ADDR-SEQ           PIC S9(04) COMP.
00051      12  W-POSITION2             PIC S9(04) COMP.
00052      12  W-POSITION21            PIC S9(04) COMP.
00053      12  W-SEQ-COUNTER           PIC S9(04) COMP.
00054
00055      12  W-CURRENT-LINE          PIC S9(03) COMP-3 VALUE +0.
00056      12  W-ROLL-COUNTER          PIC S9(03) VALUE +0     COMP-3.
00057      12  W-TOTAL-LINES           PIC S9(03) COMP-3 VALUE +0.
00058
00059      12  W-ARCH-SUPPRESS         PIC ZZZZZZZZ99.
00060      12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
00061                                  PIC  X(10).
00062
00063      12  W-CURRENT-ERROR         PIC  9(04).
00064
00065      12  W-DATE-WORK             PIC  9(07).
00066      12  W-DT-REDEF REDEFINES W-DATE-WORK.
00067          16  FILLER              PIC  X(02).
00068          16  W-DT-WORK           PIC  9(05).
00069
00070      12  W-NDX-WORK              PIC  9(02).
00071      12  W-SUB                   PIC  9(02).
00072      12  W-WORK-AMOUNT           PIC S9(09)V99 VALUE +0.
00073      12  W-ZIP-NUMERIC           PIC  9(09).
00074      12  W-ZIP-NONNUM   REDEFINES  W-ZIP-NUMERIC
00075                                  PIC  X(09).
00076
00077      12  W-ADDR-TYPE-CD.
00078          16  W-ADDR-TYPE         PIC  X(01).
00079          16  W-ADDR-SEQ          PIC  X(01).
00080          16  W-ADDR-SEQ-NUM REDEFINES
00081              W-ADDR-SEQ          PIC  9(01).
00082
00083      12  W-BEN-HOLD              PIC  X(02).
00084      12  W-BENEFIT-WORK          PIC  X(03).
00085      12  W-BEN-R REDEFINES W-BENEFIT-WORK.
00086          16  W-ELIM-DAYS         PIC  X(02).
00087          16  FILLER              PIC  X(01).
00088
00089      12  W-Z-CONTROL-DATA.
00090          16  W-NUMBER-OF-COPIES  PIC  9(01).
00091          16  FILLER              PIC  X(01).
00092          16  W-DAYS-TO-FOLLOW-UP PIC  9(03).
00093          16  FILLER              PIC  X(01).
00094          16  W-DAYS-TO-RESEND-1  PIC  9(03).
00095          16  FILLER              PIC  X(01).
040110         16  W-FORM-TO-RESEND    PIC  X(4).
040110         16  FILLER              PIC  X(1).
040110         16  W-PROMPT-LETTER     PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-ENCLOSURE-CD      PIC  X(3).
040110         16  FILLER              PIC  X(1).
040110         16  W-AUTO-CLOSE-IND    PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-LETTER-TO-BENE    PIC  X(1).
00099
00100      12  W-GROUPING.
00101          16  W-GROUP-3           PIC  X(03).
00102          16  FILLER              PIC  X(03).
00103
00104      12  W-NAME.
00105          16  W-FIRST-NAME        PIC  X(12).
00106          16  W-MIDDLE-NAME       PIC  X(12).
00107          16  W-LAST-NAME         PIC  X(15).
00108
00109      12  W-CREDIT-CARD-LOAN-NO.
00110          16  W-LOAN-NUMBER       PIC  X(08).
00111          16  W-CURRENT-LOAN-NO   PIC  X(12).
00112
00113      12  W-DEEDIT-FIELD          PIC  X(15).
00114      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD
00115                                  PIC S9(15).
00116
00117      12  W-CURRENT-SAVE          PIC  X(02).
00118      12  W-EDIT-DATE-1.
00119          16  W-ED1-MM            PIC X(02).
00120          16  FILLER              PIC X(01)    VALUE '/'.
00121          16  W-ED1-DD            PIC X(02).
00122          16  FILLER              PIC X(01)    VALUE '/'.
00123          16  W-ED1-YY            PIC X(02).
00124      12  W-EDIT-DATE-2.
00125          16  W-ED2-DD            PIC X(02).
00126          16  FILLER              PIC X(01)    VALUE '/'.
00127          16  W-ED2-MM            PIC X(02).
00128          16  FILLER              PIC X(01)    VALUE '/'.
00129          16  W-ED2-YY            PIC X(02).
00130
00131      12  W-INSURED-LAST-NAME     PIC  X(15).
00132      12  W-INSURED-MID-INIT      PIC  X(01).
00133      12  W-INSURED-1ST-NAME      PIC  X(12).
00134
00135      12  W-NAME-WORK.
00136          16  W-NW                PIC  X(01)
00137              OCCURS 30 TIMES INDEXED BY W-NWA-NDX.
00138
00139      12  W-NAME-WORK2.
00140          16  W-NW2               PIC  X(01)
00141              OCCURS 20 TIMES INDEXED BY W-NWA-NDX2 W-NWA-NDX3.
00142
00143
00144      12  W-PGM-NAME              PIC  X(08).
00145      12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.
00146      12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.
00147          16  FILLER              PIC  9(01).
00148          16  W-PI-AREA           PIC  9(03).
00149          16  W-PI-PFX            PIC  9(03).
00150          16  W-PI-SFX            PIC  9(04).
00151      12  W-PHONE-OUT.
00152          16  W-PO-AREA           PIC  X(03).
00153          16  FILLER              PIC  X(01) VALUE '-'.
00154          16  W-PO-PFX            PIC  X(03).
00155          16  FILLER              PIC  X(01) VALUE '-'.
00156          16  W-PO-SFX            PIC  X(04).
00157
00158      12  W-SAVE-PROD-RECORD      PIC  X(2000) VALUE SPACES.
00159      12  W-SAVE-ACCT-RECORD      PIC  X(2000) VALUE SPACES.
00160      12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
00161      12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
00162
00163      12  W-SINGLE-LINE           PIC  X(70).
00164      12  W-SINGLE-LINE-BY-1 REDEFINES W-SINGLE-LINE.
00165          16  W-ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2
00166                                  PIC  X(01).
00167      12  W-STATE-LINE            PIC  X(01) VALUE 'N'.
00168      12  W-TEMP-AREA1.
00169          16  W-TEMP-1 OCCURS 29 TIMES INDEXED BY W-TA1
00170                                  PIC  X(01).
00171      12  W-TEMP-AREA2.
00172          16  W-TEMP-2 OCCURS 30 TIMES INDEXED BY W-TA2 W-TA21
00173                                                  W-MOVE-NDX
00174                                  PIC  X(01).
00175
00176      12  W-TEMP-CURR-LINE        PIC S9(03)   COMP-3.
00177      12  W-TIME-IN               PIC S9(07).
00178      12  W-TIME-OUT-R REDEFINES W-TIME-IN.
00179          16  FILLER              PIC  X(01).
00180          16  W-TIME-OUT          PIC  9(02)V9(02).
00181          16  FILLER              PIC  X(02).
00182      12  W-VAR-HOLD.
00183          16  W-V1                PIC  X(01).
00184          16  W-V2                PIC  X(01).
00185          16  W-V3                PIC  X(01).
00186          16  W-V4                PIC  X(01).
00187      12  W-V-HOLD REDEFINES W-VAR-HOLD.
00188          16  W-V-NUM             PIC  9(02).
00189          16  W-V-PERIOD          PIC  X(01).
00190          16  W-V-DECIMAL         PIC  9(01).
00191
00192      12  W-WORD-LENGTH           PIC S9(04)  COMP-3.
00193
00194      12  W-ZIP-CODE.
00195          16  W-AM-ZIP-CODE       PIC  X(05).
00196          16  W-AM-ZIP-DASH       PIC  X(01).
00197          16  W-AM-ZIP-PLUS4      PIC  X(04).
00198      12  W-ZIP-CODE-CANADIAN   REDEFINES  W-ZIP-CODE.
00199          16  W-CAN-POSTAL-1      PIC  X(03).
00200          16  FILLER              PIC  X(01).
00201          16  W-CAN-POSTAL-2      PIC  X(03).
00202          16  FILLER              PIC  X(03).
00203
00204  01  W-PROGRAM-SWITCHES.
00205      12  FILLER                  PIC  X(16)
00206                                       VALUE 'PROGRAM SWITCHES'.
00207
00208      12  W-ACCT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
00209      12  W-ACCT-READ-SW          PIC  X(01) VALUE ' '.
00210      12  W-PROD-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
00211      12  W-PROD-READ-SW          PIC  X(01) VALUE ' '.
00212      12  W-ACTV-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
00213      12  W-COMP-READ-SW          PIC  X(01) VALUE ' '.
00214      12  W-DATA-FOUND-SW         PIC  X(01).
00215          88  NO-CHARACTERS-FOUND            VALUE 'N'.
00216      12  W-GETMAIN-SW            PIC  9(01) VALUE 0.
00217          88  W-NO-GETMAIN-DONE-YET          VALUE 0.
00218      12  W-LABELS-SW             PIC  X(01) VALUE SPACE.
00219      12  W-NAME-SW               PIC S9(01) COMP-3 VALUE ZERO.
00220      12  W-REVERSE-DATE-SW       PIC X(01)  VALUE SPACES.
00221          88  W-REVERSE-DATE                 VALUE 'Y'.
00222      12  W-TEXT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
00223
00224                                  EJECT
00225  01  W-ACCESS-KEYS.
00226      12  FILLER                  PIC  X(11)
00227                                  VALUE 'ACCESS KEYS'.
00228
00229      12  W-ACCT-KEY.
00230          16  W-ACCT-PARTIAL-KEY.
00231              20  W-ACCT-CO       PIC  X(01).
00232              20  W-ACCT-CARRIER  PIC  X(01).
00233              20  W-ACCT-GROUPING PIC  X(6).
00234              20  W-ACCT-STATE    PIC  X(02).
00235              20  W-ACCT-ACCOUNT  PIC  X(10).
00236          16  W-ACCT-EXP-DATE     PIC  X(02).
00237
00238      12  W-PROD-KEY.
00239          16  W-PROD-PARTIAL-KEY.
00240              20  W-PROD-CO       PIC  X(01).
00241              20  W-PROD-CARRIER  PIC  X(01).
00242              20  W-PROD-GROUPING PIC  X(6).
00243              20  W-PROD-STATE    PIC  X(02).
00244              20  W-PROD-PRODUCER PIC  X(10).
00245          16  W-PROD-EXP-DATE     PIC  X(02).
00246
00247      12  W-ACTV-KEY.
00248          16  W-ACTV-PARTIAL-KEY.
00249              20  W-ACTV-CO       PIC  X(01).
00250              20  W-ACTV-CARRIER  PIC  X(01).
00251              20  W-ACTV-CLAIM    PIC  X(07).
00252              20  W-ACTV-CERT-NUM PIC  X(11).
00253          16  W-ACTV-SEQ          PIC S9(04)   VALUE +0    COMP.
00254
00255      12  W-ARCH-KEY.
00256          16  W-ARCH-PARTIAL-KEY.
00257              20  W-ARCH-CO       PIC  X(01).
00258              20  W-ARCH-NUMBER   PIC S9(08)      COMP.
00259          16  W-ARCH-REC-TYPE     PIC  X(01).
00260          16  W-ARCH-SEQ          PIC S9(04)      COMP VALUE +0.
00261
00262      12  W-BENE-KEY.
00263          16  W-BENE-COMP-CD      PIC  X(01).
00264          16  W-BENE-REC-TYPE     PIC  X(01).
00265          16  W-BENE-NUMBER.
00266              20  W-BENE-CREDITOR PIC  X(03).
00267              20  W-BENE-FILLER   PIC  X(07).
00268
00269      12  W-CERT-KEY.
00270          16  W-CERT-CO           PIC  X(01).
00271          16  W-CERT-CARRIER      PIC  X(01).
00272          16  W-CERT-GROUPING     PIC  X(6).
00273          16  W-CERT-STATE        PIC  X(02).
00274          16  W-CERT-ACCOUNT      PIC  X(10).
00275          16  W-CERT-EFF-DT       PIC  X(02).
00276          16  W-CERT-CERT-NUM     PIC  X(11).
00277
00278      12  W-PLCY-KEY.
00279          16  W-PLCY-CO           PIC  X(01).
00280          16  W-PLCY-CARRIER      PIC  X(01).
00281          16  W-PLCY-GROUPING     PIC  X(6).
00282          16  W-PLCY-STATE        PIC  X(02).
00283          16  W-PLCY-PRODUCER     PIC  X(10).
00284          16  W-PLCY-EFF-DT       PIC  X(02).
00285          16  W-PLCY-REFERENCE-NO PIC  X(20).
00286
00287      12  W-CNTL-KEY.
00288          16  W-CNTL-CO           PIC  X(03).
00289          16  W-CNTL-RECORD-TYPE  PIC  X(01) VALUE '1'.
00290          16  W-CNTL-GENL.
00291              20  W-CNTL-GEN1     PIC  X(02) VALUE SPACES.
00292              20  W-CNTL-GEN2.
00293                 24  W-CNTL-GEN3  PIC  X(01) VALUE SPACES.
00294                 24  W-CNTL-GEN4  PIC  X(01) VALUE SPACES.
00295          16  W-CNTL-SEQ          PIC S9(04) VALUE +0     COMP.
00296
00297      12  W-CLM-KEY.
00298          16  W-CLM-CO            PIC  X(01).
00299          16  W-CLM-CARRIER       PIC  X(01).
00300          16  W-CLM-CLAIM         PIC  X(07).
00301          16  W-CLM-CERT-NUM      PIC  X(11).
00302
00303       12  W-COMP-KEY.
00304           16  W-COMP-COMPANY-CD  PIC  X(01).
00305           16  W-COMP-CARRIER     PIC  X(01).
00306           16  W-COMP-GROUPING    PIC  X(06).
00307           16  W-COMP-RESP-NO     PIC  X(10).
00308           16  W-COMP-ACCOUNT     PIC  X(10).
00309           16  W-COMP-TYPE        PIC  X(01).
00310
00311      12  W-TEXT-KEY.
00312          16  W-TEXT-PARTIAL-KEY.
00313              20  W-TEXT-CO       PIC  X(01).
00314              20  W-TEXT-LETTER   PIC  X(04).
00315          16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.
00316          16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.
00317
00318      12  W-PLAN-KEY.
00319          16  W-PLAN-CO           PIC  X(01).
00320          16  W-PLAN-CARRIER      PIC  X(01).
00321          16  W-PLAN-GROUPING     PIC  X(06).
00322          16  W-PLAN-STATE        PIC  X(02).
00323          16  W-PLAN-PRODUCER     PIC  X(10).
00324          16  W-PLAN-CODE         PIC  X(02).
00325          16  W-PLAN-REV-NO       PIC  9(03).
00326
00327      12  W-ARCH-SAVE-KEY         PIC  X(05).
00328      12  W-ACTV-SAVE-KEY         PIC  X(20).
00329      12  W-ACCT-SAVE-KEY         PIC  X(20).
00330      12  W-PROD-SAVE-KEY         PIC  X(20).
00331      12  W-TEXT-SAVE-KEY         PIC  X(05).
00332                                  EJECT
00333  01  W-PROGRAM-INTERFACE.
00334      12  FILLER                  PIC  X(14)
00335                                  VALUE 'INTERFACE AREA'.
00336 *                                COPY ELCINTF.
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
00337      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00338          16  PI-EL1523-WA.
00339              20  PI-FORM-NUMBER  PIC  X(04).
00340              20  PI-NUMBER-COPIES
00341                                  PIC  9(01).
00342              20  PI-ADDR-TYPE    PIC  X(02).
00343              20  PI-FOLLOW-UP-DATE
00344                                  PIC  X(02).
00345              20  PI-RESEND-DATE  PIC  X(02).
00346              20  PI-ERROR-CODE   PIC  9(04).
00347                  88  PI-NO-ERRORS-DETECTED VALUE 0000.
00348                  88  PI-FATAL-ERROR VALUES  0006 0013 0042
00349                                             0154 0168 0169
00350                                             0172 0179 0186
00351                                             0281 0332 2055
00352                                             3697 3698 3699
00353                                             3770 3771 3772
00354                                             7675 9106 9808
00355                                             9883 9887.
00356              20  PI-REASON       PIC  X(70).
00357              20  PI-ARCHIVE-NUMBER
00358                                  PIC  9(08).
00359              20  PI-ACCT-POINTER PIC S9(08) COMP.
00360              20  PI-ACTV-POINTER PIC S9(08) COMP.
00361              20  PI-ARCH-POINTER PIC S9(08) COMP.
00362              20  PI-VAR-POINTER  PIC S9(08) COMP.
00363              20  PI-PROD-POINTER PIC S9(08) COMP.
090108*00364          16  FILLER              PIC X(527).
090108         16  PI-AUTO-LETTER-DATE  PIC X(10).
040110         16  PI-RESEND-FORM-NUMBER PIC X(4).
040110         16  PI-PROMPT-LETTER     PIC X.
040110         16  PI-ENCLOSURE-CD      PIC X(3).
040110         16  PI-AUTO-CLOSE-IND    PIC X(1).
040110         16  PI-LETTER-TO-BENE    PIC X(1).
040110         16  FILLER               PIC X(507).
00365                                  EJECT
00366  01  W-PROGRAM-TABLE             PIC  X(14)
00367                                      VALUE 'PROGRAM TABLES'.
00368  01  W-LABEL-HOLD-AREA.
00369      12  W-LABEL-LINES OCCURS 6 TIMES
00370                         INDEXED BY W-NDX   W-NDX2.
00371          16  W-LABEL-ZIP.
00372              20  W-LABEL-1ST-ZIP
00373                                  PIC  X(05).
00374              20  FILLER          PIC  X(01).
00375              20  W-LABEL-2ND-ZIP
00376                                  PIC  X(04).
00377          16  FILLER              PIC  X(09).
00378          16  W-LAST-DIGIT        PIC  X(01).
00379          16  W-LAST-ZIP.
00380              20  W-LAST-1ST-ZIP  PIC  X(05).
00381              20  FILLER          PIC  X(01).
00382              20  W-LAST-2ND-ZIP PIC   X(04).
00383
00384
00385  01  W-RECORD-TABLE              PIC  X(21900) VALUE SPACES.
00386  01  W-REC-TABLE REDEFINES W-RECORD-TABLE.
00387      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-NDX
00388                                  PIC  X(3650).
00389
00390  01  W-REC-ENTRIES REDEFINES W-RECORD-TABLE.
00391      12  W-REC-ENT OCCURS 300 TIMES INDEXED BY W-TB-NDX W-TB-NDX1.
00392          16  W-REC-TEXT          PIC  X(70).
00393          16  W-REC-PC            PIC  9(02).
00394          16  FILLER              PIC  X(01).
00395
00396                                  EJECT
00397  01  W-PROGRAM-VARIABLE-AREA.
00398      12  FILLER                  PIC  X(13)
00399                                       VALUE 'VARIABLE AREA'.
00400 ****************************************************
00401 *       WHEN ADDING OR DELETING ENTRIES TO         *
00402 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *
00403 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
00404 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *
00405 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
00406 *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
00407 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
00408 ****************************************************
00409
00410 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
00411 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA
00412
00413      12  SS-NUM-ENTRIES          PIC  9(03) VALUE 116      COMP-3.
00414      12  SS-COUNTER              PIC  9(03)                COMP-3.
00415      12  SS-WORK-AREA-LENGTH     PIC S9(04) VALUE +3340    COMP.
00416
00417  01  W-VARIABLE-WORK-AREA.
00418      12  W-VAR-CODE              PIC  X(04).
00419      12  W-VAR-LEN               PIC  9(02).
00420      12  W-VAR-DATA          PIC  X(100).
00421      12  W-VAR-DATA-R REDEFINES W-VAR-DATA.
00422        16  W-VAR-W-ONE-CHAR OCCURS 100 TIMES
00423                         INDEXED BY W-NDXV
00424                                  PIC  X(01).
00425  01  SYSTEM-SUPPORTED-VARIABLES.
00426 *****COMPANY NAME
00427      12  SS01                    PIC  X(04) VALUE    '01.0'.
00428      12  SS01L                   PIC  9(02) VALUE 36.
00429      12  SS01D                   PIC  X(30) VALUE ALL '*'.
00430 *****FULL COMPANY ADDRESS
00431      12  SS02-1                  PIC  X(04) VALUE    '02.1'.
00432      12  SS02-1L                 PIC  9(02) VALUE 36.
00433      12  SS02-1D                 PIC  X(30) VALUE ALL '*'.
00434      12  SS02-2                  PIC  X(04) VALUE    '02.2'.
00435      12  SS02-2L                 PIC  9(02) VALUE 36.
00436      12  SS02-2D                 PIC  X(30) VALUE ALL '*'.
00437      12  SS02-3                  PIC  X(04) VALUE    '02.3'.
00438      12  SS02-3L                 PIC  9(02) VALUE 36.
00439      12  SS02-3D                 PIC  X(30) VALUE ALL '*'.
00440      12  SS02-4                  PIC  X(04) VALUE    '02.4'.
00441      12  SS02-4L                 PIC  9(02) VALUE 36.
00442      12  SS02-4D                 PIC  X(30) VALUE ALL '*'.
00443      12  SS02-5                  PIC  X(04) VALUE    '02.5'.
00444      12  SS02-5L                 PIC  9(02) VALUE 36.
00445      12  SS02-5D                 PIC  X(30) VALUE ALL '*'.
00446 *****CARRIER NAME
00447      12  SS03                    PIC  X(04) VALUE    '03.0'.
00448      12  SS03L                   PIC  9(02) VALUE 36.
00449      12  SS03D                   PIC  X(30) VALUE ALL '*'.
031307*****INVESTORS HERITAGE
031307* as Administrator for Investors Heritage Life Insurance Company
031307     12  SS03-1                  PIC X(4)  VALUE     '03.1'.
031307     12  SS03-1L                 PIC 99    VALUE 68.
031307     12  SS03-1D                 PIC X(62) VALUE ALL '*'.
00450 *****FULL CARRIER ADDRESS
00451      12  SS04-1                  PIC  X(04) VALUE    '04.1'.
00452      12  SS04-1L                 PIC  9(02) VALUE 36.
00453      12  SS04-1D                 PIC  X(30) VALUE ALL '*'.
00454      12  SS04-2                  PIC  X(04) VALUE    '04.2'.
00455      12  SS04-2L                 PIC  9(02) VALUE 36.
00456      12  SS04-2D                 PIC  X(30) VALUE ALL '*'.
00457      12  SS04-3                  PIC  X(04) VALUE    '04.3'.
00458      12  SS04-3L                 PIC  9(02) VALUE 36.
00459      12  SS04-3D                 PIC  X(30) VALUE ALL '*'.
00460      12  SS04-4                  PIC  X(04) VALUE    '04.4'.
00461      12  SS04-4L                 PIC  9(02) VALUE 36.
00462      12  SS04-4D                 PIC  X(30) VALUE ALL '*'.
00463      12  SS04-5                  PIC  X(04) VALUE    '04.5'.
00464      12  SS04-5L                 PIC  9(02) VALUE 36.
00465      12  SS04-5D                 PIC  X(30) VALUE ALL '*'.
00466 *****CARRIER PHONE NUMBER
00467      12  SS04-6                  PIC  X(04) VALUE    '04.6'.
00468      12  SS04-6L                 PIC  9(02) VALUE 18.
00469      12  SS04-6D                 PIC  X(12) VALUE ALL '*'.
00470 *****FULL ADDRESEE LABEL
00471      12  SS05-1                  PIC  X(04) VALUE    '05.1'.
00472      12  SS05-1L                 PIC  9(02) VALUE 36.
00473      12  SS05-1D                 PIC  X(30) VALUE ALL '*'.
00474      12  SS05-2                  PIC  X(04) VALUE    '05.2'.
00475      12  SS05-2L                 PIC  9(02) VALUE 36.
00476      12  SS05-2D                 PIC  X(30) VALUE ALL '*'.
00477      12  SS05-3                  PIC  X(04) VALUE    '05.3'.
00478      12  SS05-3L                 PIC  9(02) VALUE 36.
00479      12  SS05-3D                 PIC  X(30) VALUE ALL '*'.
00480      12  SS05-4                  PIC  X(04) VALUE    '05.4'.
00481      12  SS05-4L                 PIC  9(02) VALUE 36.
00482      12  SS05-4D                 PIC  X(30) VALUE ALL '*'.
00483      12  SS05-5                  PIC  X(04) VALUE    '05.5'.
00484      12  SS05-5L                 PIC  9(02) VALUE 36.
00485      12  SS05-5D                 PIC  X(30) VALUE ALL '*'.
00486      12  SS05-6                  PIC  X(04) VALUE    '05.6'.
00487      12  SS05-6L                 PIC  9(02) VALUE 36.
00488      12  SS05-6D                 PIC  X(30) VALUE ALL '*'.
00489 *****ACCOUNT NAME
00490      12  SS06                    PIC  X(04) VALUE    '06.0'.
00491      12  SS06L                   PIC  9(02) VALUE 36.
00492      12  SS06D                   PIC  X(30) VALUE ALL '*'.
00493 *****FULL ACCOUNT ADDRESS
00494      12  SS07-1                  PIC  X(04) VALUE    '07.1'.
00495      12  SS07-1L                 PIC  9(02) VALUE 36.
00496      12  SS07-1D                 PIC  X(30) VALUE ALL '*'.
00497      12  SS07-2                  PIC  X(04) VALUE    '07.2'.
00498      12  SS07-2L                 PIC  9(02) VALUE 36.
00499      12  SS07-2D                 PIC  X(30) VALUE ALL '*'.
00500      12  SS07-3                  PIC  X(04) VALUE    '07.3'.
00501      12  SS07-3L                 PIC  9(02) VALUE 36.
00502      12  SS07-3D                 PIC  X(30) VALUE ALL '*'.
00503      12  SS07-4                  PIC  X(04) VALUE    '07.4'.
00504      12  SS07-4L                 PIC  9(02) VALUE 36.
00505      12  SS07-4D                 PIC  X(30) VALUE ALL '*'.
00506      12  SS07-5                  PIC  X(04) VALUE    '07.5'.
00507      12  SS07-5L                 PIC  9(02) VALUE 36.
00508      12  SS07-5D                 PIC  X(30) VALUE ALL '*'.
00509 *****ACCOUNT PHONE NUMBER
00510      12  SS07-6                  PIC  X(04) VALUE    '07.6'.
00511      12  SS07-6L                 PIC  9(02) VALUE 18.
00512      12  SS07-6D                 PIC  X(12) VALUE ALL '*'.
00513 *****EXECUTING PROCESSOR NAME
00514      12  SS08                    PIC  X(04) VALUE    '08.0'.
00515      12  SS08L                   PIC  9(02) VALUE 36.
00516      12  SS08D                   PIC  X(30) VALUE ALL '*'.
00517 *****PROCESSOR TITLE
00518      12  SS09                    PIC  X(04) VALUE    '09.0'.
00519      12  SS09L                   PIC  9(02) VALUE 32.
00520      12  SS09D                   PIC  X(26) VALUE ALL '*'.
00521 *****INSUREDS NAME
00522      12  SS10                    PIC  X(04) VALUE    '10.0'.
00523      12  SS10L                   PIC  9(02) VALUE 36.
00524      12  SS10D                   PIC  X(30) VALUE ALL '*'.
00525 *****INSUREDS ADDRESS
00526      12  SS11-1                  PIC  X(04) VALUE    '11.1'.
00527      12  SS11-1L                 PIC  9(02) VALUE 36.
00528      12  SS11-1D                 PIC  X(30) VALUE ALL '*'.
00529      12  SS11-2                  PIC  X(04) VALUE    '11.2'.
00530      12  SS11-2L                 PIC  9(02) VALUE 36.
00531      12  SS11-2D                 PIC  X(30) VALUE ALL '*'.
00532      12  SS11-3                  PIC  X(04) VALUE    '11.3'.
00533      12  SS11-3L                 PIC  9(02) VALUE 36.
00534      12  SS11-3D                 PIC  X(30) VALUE ALL '*'.
00535      12  SS11-4                  PIC  X(04) VALUE    '11.4'.
00536      12  SS11-4L                 PIC  9(02) VALUE 36.
00537      12  SS11-4D                 PIC  X(30) VALUE ALL '*'.
00538 *****INSUREDS NAME FROM ADDR TRAILER
00539      12  SS11-5                  PIC  X(04) VALUE    '11.5'.
00540      12  SS11-5L                 PIC  9(02) VALUE 36.
00541      12  SS11-5D                 PIC  X(30) VALUE ALL '*'.
00542 *****INSUREDS PHONE NUMBER FROM ADDR TRAILER
00543      12  SS11-6                  PIC  X(04) VALUE    '11.6'.
00544      12  SS11-6L                 PIC  9(02) VALUE 18.
00545      12  SS11-6D                 PIC  X(12) VALUE ALL '*'.
00546 *****CLAIM TYPE NAME
00547      12  SS12                    PIC  X(04) VALUE    '12.0'.
00548      12  SS12L                   PIC  9(02) VALUE 12.
00549      12  SS12D                   PIC  X(6) VALUE ALL '*'.
00550 *****CLAIM INCURRED DATE
00551      12  SS13                    PIC  X(04) VALUE    '13.0'.
00552      12  SS13L                   PIC  9(02) VALUE 14.
00553      12  SS13D                   PIC  X(08) VALUE ALL '*'.
00554 *****CLAIM REPORTED DATE
00555      12  SS14                    PIC  X(04) VALUE    '14.0'.
00556      12  SS14L                   PIC  9(02) VALUE 14.
00557      12  SS14D                   PIC  X(08) VALUE ALL '*'.
00558 *****LAST PAYMENT DATE
00559      12  SS15                    PIC  X(04) VALUE    '15.0'.
00560      12  SS15L                   PIC  9(02) VALUE 14.
00561      12  SS15D                   PIC  X(08) VALUE ALL '*'.
00562 *****LAST PAYMENT AMOUNT
00563      12  SS16                    PIC  X(04) VALUE    '16.0'.
00564      12  SS16L                   PIC  9(02) VALUE 17.
00565      12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.
00566 *****CLAIM PAID THRU/TO DATE
00567      12  SS17                    PIC  X(04) VALUE    '17.0'.
00568      12  SS17L                   PIC  9(02) VALUE 14.
00569      12  SS17D                   PIC  X(08) VALUE ALL '*'.
00570 *****TOTAL PAID TO DATE
00571      12  SS18                    PIC  X(04) VALUE    '18.0'.
00572      12  SS18L                   PIC  9(02) VALUE 17.
00573      12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.
00574 *****DIAGNOSIS OR CAUSE
00575      12  SS19                    PIC  X(04) VALUE    '19.0'.
00576      12  SS19L                   PIC  9(02) VALUE 32.
00577      12  SS19D                   PIC  X(26) VALUE ALL '*'.
00578 *****CAUSE CODE
00579      12  SS19-1                  PIC  X(04) VALUE    '19.1'.
00580      12  SS19-1L                 PIC  9(02) VALUE 12.
00581      12  SS19-1D                 PIC  X(6) VALUE ALL '*'.
00582 *****CURRENT DATE
00583      12  SS20                    PIC  X(04) VALUE    '20.0'.
00584      12  SS20L                   PIC  9(02) VALUE 14.
00585      12  SS20D                   PIC  X(08) VALUE ALL '*'.
00586 *****FULL CURRENT DATE
00587      12  SS21                    PIC  X(04) VALUE    '21.0'.
00588      12  SS21L                   PIC  9(02) VALUE 24.
00589      12  SS21D                   PIC  X(18) VALUE ALL '*'.
00590 *****BENEFIT DESCRIPTION
00591      12  SS22                    PIC  X(04) VALUE    '22.0'.
00592      12  SS22L                   PIC  9(02) VALUE 16.
00593      12  SS22D                   PIC  X(10) VALUE ALL '*'.
00594 *****CARRIER CODE IN CERT
00595      12  SS23                    PIC  X(04) VALUE    '23.0'.
00596      12  SS23L                   PIC  9(02) VALUE 9.
00597      12  SS23D                   PIC  X(03) VALUE ALL '*'.
00598 *****GROUPING CODE IN CERT
00599      12  SS24                    PIC  X(04) VALUE    '24.0'.
00600      12  SS24L                   PIC  9(02) VALUE 12.
00601      12  SS24D                   PIC  X(6) VALUE ALL '*'.
00602 *****ACCOUNT NUMBER IN CERT
00603      12  SS25                    PIC  X(04) VALUE    '25.0'.
00604      12  SS25L                   PIC  9(02) VALUE 16.
00605      12  SS25D                   PIC  X(10) VALUE ALL '*'.
00606 *****CERTIFICATE NUMBER
00607      12  SS26                    PIC  X(04) VALUE    '26.0'.
00608      12  SS26L                   PIC  9(02) VALUE 17.
00609      12  SS26D                   PIC  X(11) VALUE ALL '*'.
00610 *****CERT EFFECTIVE DATE
00611      12  SS27                    PIC  X(04) VALUE    '27.0'.
00612      12  SS27L                   PIC  9(02) VALUE 14.
00613      12  SS27D                   PIC  X(08) VALUE ALL '*'.
00614 *****CERT EXPIRATION DATE
00615      12  SS28                    PIC  X(04) VALUE    '28.0'.
00616      12  SS28L                   PIC  9(02) VALUE 14.
00617      12  SS28D                   PIC  X(08) VALUE ALL '*'.
00618 *****APPLICABLE COVERAGE TERM
00619      12  SS29                    PIC  X(04) VALUE    '29.0'.
00620      12  SS29L                   PIC  9(02) VALUE 9.
00621      12  SS29D                   PIC  X(03) VALUE ALL '*'.
00622 *****APPLICABLE COVERAGE AMOUNT
00623      12  SS30                    PIC  X(04) VALUE    '30.0'.
00624      12  SS30L                   PIC  9(02) VALUE 18.
00625      12  SS30D                   PIC $$$$$,$$$.99 VALUE ZEROS.
00626 *****APPLICABLE COVERAGE CANCEL DATE
00627      12  SS31                    PIC  X(04) VALUE    '31.0'.
00628      12  SS31L                   PIC  9(02) VALUE 14.
00629      12  SS31D                   PIC  X(08) VALUE ALL '*'.
00630 *****APPLICABLE COVERAGE FORM NUMBER
00631      12  SS32                    PIC  X(04) VALUE    '32.0'.
00632      12  SS32L                   PIC  9(02) VALUE 18.
00633      12  SS32D                   PIC  X(12) VALUE ALL '*'.
00634 *****INSURES AGE AT POLICY ISSUE
00635      12  SS33                    PIC  X(04) VALUE    '33.0'.
00636      12  SS33L                   PIC  9(02) VALUE 9.
00637      12  SS33D                   PIC  X(03) VALUE ALL '*'.
00638 *****CLAIM NUMBER
00639      12  SS34                    PIC  X(04) VALUE    '34.0'.
00640      12  SS34L                   PIC  9(02) VALUE 13.
00641      12  SS34D                   PIC  X(07) VALUE ALL '*'.
00642 *****LAST DENIAL TEXT
00643      12  SS35-1                  PIC  X(04) VALUE    '35.1'.
00644      12  SS35-1L                 PIC  9(02) VALUE 66.
00645      12  SS35-1D                 PIC  X(60) VALUE ALL '*'.
00646      12  SS35-2                  PIC  X(04) VALUE    '35.2'.
00647      12  SS35-2L                 PIC  9(02) VALUE 66.
00648      12  SS35-2D                 PIC  X(60) VALUE ALL '*'.
00649 *****LOAN NUMBER
00650      12  SS36                    PIC  X(04) VALUE    '36.0'.
00651      12  SS36L                   PIC  9(02) VALUE 14.
00652      12  SS36D                   PIC  X(08) VALUE ALL '*'.
00653 *****CREDIT CARD LOAN NUMBER
00654      12  SS36-1                  PIC  X(04) VALUE    '36.1'.
00655      12  SS36-1L                 PIC  9(02) VALUE 26.
00656      12  SS36-1D                 PIC  X(20) VALUE ALL '*'.
00657 *****LOAN BALANCE
00658      12  SS37                    PIC  X(04) VALUE    '37.0'.
00659      12  SS37L                   PIC  9(02) VALUE 18.
00660      12  SS37D                   PIC $$$$$,$$$.99 VALUE ZEROS.
00661 *****MEMBER NUMBER
00662      12  SS38                    PIC  X(04) VALUE    '38.0'.
00663      12  SS38L                   PIC  9(02) VALUE 18.
00664      12  SS38D                   PIC  X(12) VALUE ALL '*'.
00665 *****INSURED NAME (FIRST M LAST)
00666      12  SS39                    PIC  X(04) VALUE    '39.0'.
00667      12  SS39L                   PIC  9(02) VALUE 36.
00668      12  SS39D                   PIC  X(30) VALUE ALL '*'.
00669 *****INSURED LAST NAME ONLY
00670      12  SS40                    PIC  X(04) VALUE    '40.0'.
00671      12  SS40L                   PIC  9(02) VALUE 21.
00672      12  SS40D                   PIC  X(15) VALUE ALL '*'.
00673 *****TITLE (MR/MS)
00674      12  SS41                    PIC  X(04) VALUE    '41.0'.
00675      12  SS41L                   PIC  9(02) VALUE 9.
00676      12  SS41D                   PIC  X(03) VALUE ALL '*'.
00677 *****ELIMINATION PERIOD
00678      12  SS42                    PIC  X(04) VALUE    '42.0'.
00679      12  SS42L                   PIC  9(02) VALUE 9.
00680      12  SS42D                   PIC  X(03) VALUE ALL '*'.
00681 *****BENEFICIARY NAME
00682      12  SS43                    PIC  X(04) VALUE    '43.0'.
00683      12  SS43L                   PIC  9(02) VALUE 36.
00684      12  SS43D                   PIC  X(30) VALUE ALL '*'.
00685 *****BENEFICIARY ADDRESS
00686      12  SS44-1                  PIC  X(04) VALUE    '44.1'.
00687      12  SS44-1L                 PIC  9(02) VALUE 36.
00688      12  SS44-1D                 PIC  X(30) VALUE ALL '*'.
00689      12  SS44-2                  PIC  X(04) VALUE    '44.2'.
00690      12  SS44-2L                 PIC  9(02) VALUE 36.
00691      12  SS44-2D                 PIC  X(30) VALUE ALL '*'.
00692      12  SS44-3                  PIC  X(04) VALUE    '44.3'.
00693      12  SS44-3L                 PIC  9(02) VALUE 36.
00694      12  SS44-3D                 PIC  X(30) VALUE ALL '*'.
00695      12  SS44-4                  PIC  X(04) VALUE    '44.4'.
00696      12  SS44-4L                 PIC  9(02) VALUE 36.
00697      12  SS44-4D                 PIC  X(30) VALUE ALL '*'.
00698      12  SS44-5                  PIC  X(04) VALUE    '44.5'.
00699      12  SS44-5L                 PIC  9(02) VALUE 18.
00700      12  SS44-5D                 PIC  X(12) VALUE ALL '*'.
00701 *****INSUREDS DATE OF BIRTH
00702      12  SS45                    PIC  X(04) VALUE    '45.0'.
00703      12  SS45L                   PIC  9(02) VALUE 14.
00704      12  SS45D                   PIC  X(08) VALUE ALL '*'.
00705 *****INSUREDS SOC SEC NUMBER
00706      12  SS46                    PIC  X(04) VALUE    '46.0'.
00707      12  SS46L                   PIC  9(02) VALUE 17.
00708      12  SS46D                   PIC  X(11) VALUE ALL '*'.
00709 *****PHYSICIANS  NAME
00710      12  SS47                    PIC  X(04) VALUE    '47.0'.
00711      12  SS47L                   PIC  9(02) VALUE 36.
00712      12  SS47D                   PIC  X(30) VALUE ALL '*'.
00713 *****PHYSICIANS  ADDRESS
00714      12  SS47-1                  PIC  X(04) VALUE    '47.1'.
00715      12  SS47-1L                 PIC  9(02) VALUE 36.
00716      12  SS47-1D                 PIC  X(30) VALUE ALL '*'.
00717      12  SS47-2                  PIC  X(04) VALUE    '47.2'.
00718      12  SS47-2L                 PIC  9(02) VALUE 36.
00719      12  SS47-2D                 PIC  X(30) VALUE ALL '*'.
00720      12  SS47-3                  PIC  X(04) VALUE    '47.3'.
00721      12  SS47-3L                 PIC  9(02) VALUE 36.
00722      12  SS47-3D                 PIC  X(30) VALUE ALL '*'.
00723      12  SS47-4                  PIC  X(04) VALUE    '47.4'.
00724      12  SS47-4L                 PIC  9(02) VALUE 36.
00725      12  SS47-4D                 PIC  X(30) VALUE ALL '*'.
00726      12  SS47-5                  PIC  X(04) VALUE    '47.5'.
00727      12  SS47-5L                 PIC  9(02) VALUE 18.
00728      12  SS47-5D                 PIC  X(12) VALUE ALL '*'.
00729 *****EMPLOYERS   NAME
00730      12  SS48                    PIC  X(04) VALUE    '48.0'.
00731      12  SS48L                   PIC  9(02) VALUE 36.
00732      12  SS48D                   PIC  X(30) VALUE ALL '*'.
00733 *****EMPLOYERS   ADDRESS
00734      12  SS48-1                  PIC  X(04) VALUE    '48.1'.
00735      12  SS48-1L                 PIC  9(02) VALUE 36.
00736      12  SS48-1D                 PIC  X(30) VALUE ALL '*'.
00737      12  SS48-2                  PIC  X(04) VALUE    '48.2'.
00738      12  SS48-2L                 PIC  9(02) VALUE 36.
00739      12  SS48-2D                 PIC  X(30) VALUE ALL '*'.
00740      12  SS48-3                  PIC  X(04) VALUE    '48.3'.
00741      12  SS48-3L                 PIC  9(02) VALUE 36.
00742      12  SS48-3D                 PIC  X(30) VALUE ALL '*'.
00743      12  SS48-4                  PIC  X(04) VALUE    '48.4'.
00744      12  SS48-4L                 PIC  9(02) VALUE 36.
00745      12  SS48-4D                 PIC  X(30) VALUE ALL '*'.
00746      12  SS48-5                  PIC  X(04) VALUE    '48.5'.
00747      12  SS48-5L                 PIC  9(02) VALUE 18.
00748      12  SS48-5D                 PIC  X(12) VALUE ALL '*'.
00749 *****OTHER1      NAME
00750      12  SS49                    PIC  X(04) VALUE    '49.0'.
00751      12  SS49L                   PIC  9(02) VALUE 36.
00752      12  SS49D                   PIC  X(30) VALUE ALL '*'.
00753 *****OTHER1      ADDRESS
00754      12  SS49-1                  PIC  X(04) VALUE    '49.1'.
00755      12  SS49-1L                 PIC  9(02) VALUE 36.
00756      12  SS49-1D                 PIC  X(30) VALUE ALL '*'.
00757      12  SS49-2                  PIC  X(04) VALUE    '49.2'.
00758      12  SS49-2L                 PIC  9(02) VALUE 36.
00759      12  SS49-2D                 PIC  X(30) VALUE ALL '*'.
00760      12  SS49-3                  PIC  X(04) VALUE    '49.3'.
00761      12  SS49-3L                 PIC  9(02) VALUE 36.
00762      12  SS49-3D                 PIC  X(30) VALUE ALL '*'.
00763      12  SS49-4                  PIC  X(04) VALUE    '49.4'.
00764      12  SS49-4L                 PIC  9(02) VALUE 36.
00765      12  SS49-4D                 PIC  X(30) VALUE ALL '*'.
00766      12  SS49-5                  PIC  X(04) VALUE    '49.5'.
00767      12  SS49-5L                 PIC  9(02) VALUE 18.
00768      12  SS49-5D                 PIC  X(12) VALUE ALL '*'.
00769 *****OTHER2      NAME
00770      12  SS50                    PIC  X(04) VALUE    '50.0'.
00771      12  SS50L                   PIC  9(02) VALUE 36.
00772      12  SS50D                   PIC  X(30) VALUE ALL '*'.
00773 *****OTHER2      ADDRESS
00774      12  SS50-1                  PIC  X(04) VALUE    '50.1'.
00775      12  SS50-1L                 PIC  9(02) VALUE 36.
00776      12  SS50-1D                 PIC  X(30) VALUE ALL '*'.
00777      12  SS50-2                  PIC  X(04) VALUE    '50.2'.
00778      12  SS50-2L                 PIC  9(02) VALUE 36.
00779      12  SS50-2D                 PIC  X(30) VALUE ALL '*'.
00780      12  SS50-3                  PIC  X(04) VALUE    '50.3'.
00781      12  SS50-3L                 PIC  9(02) VALUE 36.
00782      12  SS50-3D                 PIC  X(30) VALUE ALL '*'.
00783      12  SS50-4                  PIC  X(04) VALUE    '50.4'.
00784      12  SS50-4L                 PIC  9(02) VALUE 36.
00785      12  SS50-4D                 PIC  X(30) VALUE ALL '*'.
00786      12  SS50-5                  PIC  X(04) VALUE    '50.5'.
00787      12  SS50-5L                 PIC  9(02) VALUE 18.
00788      12  SS50-5D                 PIC  X(12) VALUE ALL '*'.
00789 *****A&H TERM TIMES MON. BEN.
00790      12  SS51                    PIC  X(04) VALUE    '51.0'.
00791      12  SS51L                   PIC  9(02) VALUE 17.
00792      12  SS51D                   PIC $$$$,$$$.99  VALUE ZEROS.
00793 *****THIRD PARTY NAME
00794      12  SS52                    PIC  X(04) VALUE    '52.0'.
00795      12  SS52L                   PIC  9(02) VALUE 36.
00796      12  SS52D                   PIC  X(30) VALUE ALL '*'.
00797 *****THIRD PARTY ADDRESS
00798      12  SS53-1                  PIC  X(04) VALUE    '53.1'.
00799      12  SS53-1L                 PIC  9(02) VALUE 36.
00800      12  SS53-1D                 PIC  X(30) VALUE ALL '*'.
00801      12  SS53-2                  PIC  X(04) VALUE    '53.2'.
00802      12  SS53-2L                 PIC  9(02) VALUE 36.
00803      12  SS53-2D                 PIC  X(30) VALUE ALL '*'.
00804      12  SS53-3                  PIC  X(04) VALUE    '53.3'.
00805      12  SS53-3L                 PIC  9(02) VALUE 36.
00806      12  SS53-3D                 PIC  X(30) VALUE ALL '*'.
00807      12  SS53-4                  PIC  X(04) VALUE    '53.4'.
00808      12  SS53-4L                 PIC  9(02) VALUE 36.
00809      12  SS53-4D                 PIC  X(30) VALUE ALL '*'.
00810      12  SS53-5                  PIC  X(04) VALUE    '53.5'.
00811      12  SS53-5L                 PIC  9(02) VALUE 36.
00812      12  SS53-5D                 PIC  X(30) VALUE ALL '*'.
00813 *****THIRD PARTY PHONE NUMBER
00814      12  SS53-6                  PIC  X(04) VALUE    '53.6'.
00815      12  SS53-6L                 PIC  9(02) VALUE 18.
00816      12  SS53-6D                 PIC  X(12) VALUE ALL '*'.
00817 *****CERTIFICATE SEQUENCE
00818      12  SS54                    PIC  X(04) VALUE    '54.0'.
00819      12  SS54L                   PIC  9(02) VALUE 09.
00820      12  SS54D                   PIC  X(03) VALUE ALL '*'.
00821 *****CERTIFICATE TOTAL  E
00822      12  SS55                    PIC  X(04) VALUE    '55.0'.
00823      12  SS55L                   PIC  9(02) VALUE 09.
00824      12  SS55D                   PIC  X(03) VALUE ALL '*'.
00825 *****CREDITOR ID
00826      12  SS56                    PIC  X(04) VALUE    '56.0'.
00827      12  SS56L                   PIC  9(02) VALUE 36.
00828      12  SS56D                   PIC  X(30) VALUE ALL '*'.
00829 *****INSUREDS NAME (CERTIFICATE)
00830      12  SS57                    PIC X(4)  VALUE     '57.0'.
00831      12  SS57L                   PIC 99    VALUE 36.
00832      12  SS57D                   PIC X(30) VALUE ALL '*'.
00833 *****JOINTS NAME (CERTIFICATE)
00834      12  SS58                    PIC X(4)  VALUE     '58.0'.
00835      12  SS58L                   PIC 99    VALUE 36.
00836      12  SS58D                   PIC X(30) VALUE ALL '*'.
00837 *****POLICY REFERENCE NUMBER
00838      12  SS59                    PIC X(4)  VALUE     '59.0'.
00839      12  SS59L                   PIC 99    VALUE 26.
00840      12  SS59D                   PIC X(20) VALUE ALL '*'.
00841
00842 ****************************************************
00843 *       WHEN ADDING OR DELETING ENTRIES TO         *
00844 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *
00845 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
00846 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *
00847 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
00848 *       ALSO YOU NEED           TO INCREASE THE LENGTH OF *
00849 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
00850 ****************************************************
00851                                  EJECT
00852 *                                COPY ELCDATE.
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
00853                                  EJECT
00854  01  W-PROGRAM-CONSTANTS.
00855      12  FILLER                  PIC  X(17)
00856                                  VALUE 'PROGRAM CONSTANTS'.
00857      12  W-ACCT-LENGTH           PIC S9(04) COMP  VALUE +2000.
00858      12  W-PROD-LENGTH           PIC S9(04) COMP  VALUE +2000.
00859      12  W-ACTV-LENGTH           PIC S9(04) COMP  VALUE +200.
00860      12  W-ARCH-LENGTH           PIC S9(04) COMP  VALUE +90.
033110     12  W-NAPS-LENGTH           PIC S9(04) COMP  VALUE +150.
00861
00862      12  W-ACCT-ID               PIC  X(08)   VALUE 'ERACCT'.
00863      12  W-ACTV-ID               PIC  X(08)   VALUE 'ELTRLR'.
00864      12  W-ARCH-ID               PIC  X(08)   VALUE 'ELARCH'.
00865      12  W-BENE-ID               PIC  X(08)   VALUE 'ELBENE'.
00866      12  W-CLM-ID                PIC  X(08)   VALUE 'ELMSTR'.
00867      12  W-CNTL-ID               PIC  X(08)   VALUE 'ELCNTL'.
00868      12  W-CERT-ID               PIC  X(08)   VALUE 'ELCERT'.
00869      12  W-PROD-ID               PIC  X(08)   VALUE 'MPPROD'.
00870      12  W-PLCY-ID               PIC  X(08)   VALUE 'MPPLCY'.
00871      12  W-PLAN-ID               PIC  X(08)   VALUE 'MPPLAN'.
033110     12  W-NAPS-ID               PIC  X(08)   VALUE 'ELNAPS'.
00872      12  W-GETMAINSPACE          PIC  X(01) VALUE SPACE.
00873      12  W-LINK-ELDATCV          PIC  X(07) VALUE 'ELDATCV'.
00874      12  W-LOWER-CASE
00875                     PIC  X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
00876      12  W-MAX-LINES             PIC  9(03) VALUE 300.
00877      12  W-TEXT-ID               PIC  X(08) VALUE 'ELLETR'.
00878      12  W-TOP-FORM              PIC  X(70)
00879          VALUE '*****TOP OF FORM *****'.
00880      12  W-TRANSACTION           PIC  X(04) VALUE 'EXL1'.
00881      12  W-UPPER-CASE
00882                     PIC  X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
00883                                  EJECT
00884  01  ERROR-MESSAGES.
00885      12  ER-0006                 PIC  X(04) VALUE '0006'.
00886      12  ER-0013                 PIC  X(04) VALUE '0013'.
00887      12  ER-0042                 PIC  X(04) VALUE '0042'.
00888      12  ER-0051                 PIC  X(04) VALUE '0051'.
00889      12  ER-0154                 PIC  X(04) VALUE '0154'.
00890      12  ER-0168                 PIC  X(04) VALUE '0168'.
00891      12  ER-0169                 PIC  X(04) VALUE '0169'.
00892      12  ER-0172                 PIC  X(04) VALUE '0172'.
00893      12  ER-0176                 PIC  X(04) VALUE '0176'.
00894      12  ER-0178                 PIC  X(04) VALUE '0178'.
00895      12  ER-0179                 PIC  X(04) VALUE '0179'.
00896      12  ER-0180                 PIC  X(04) VALUE '0180'.
00897      12  ER-0181                 PIC  X(04) VALUE '0181'.
00898      12  ER-0186                 PIC  X(04) VALUE '0186'.
00899      12  ER-0191                 PIC  X(04) VALUE '0191'.
00900      12  ER-0206                 PIC  X(04) VALUE '0206'.
00901      12  ER-0281                 PIC  X(04) VALUE '0281'.
00902      12  ER-0332                 PIC  X(04) VALUE '0332'.
00903      12  ER-0413                 PIC  X(04) VALUE '0413'.
00904      12  ER-2055                 PIC  X(04) VALUE '2055'.
00905      12  ER-3697                 PIC  X(04) VALUE '3697'.
00906      12  ER-3698                 PIC  X(04) VALUE '3698'.
00907      12  ER-3699                 PIC  X(04) VALUE '3699'.
00908      12  ER-3766                 PIC  X(04) VALUE '3766'.
00909      12  ER-3770                 PIC  X(04) VALUE '3770'.
00910      12  ER-3771                 PIC  X(04) VALUE '3771'.
00911      12  ER-3772                 PIC  X(04) VALUE '3772'.
00912      12  ER-7675                 PIC  X(04) VALUE '7675'.
00913      12  ER-9106                 PIC  X(04) VALUE '9106'.
00914      12  ER-9483                 PIC  X(04) VALUE '9483'.
00915      12  ER-9808                 PIC  X(04) VALUE '9808'.
00916      12  ER-9883                 PIC  X(04) VALUE '9883'.
00917      12  ER-9886                 PIC  X(04) VALUE '9886'.
00918      12  ER-9887                 PIC  X(04) VALUE '9887'.
00919
00920                                  EJECT
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
00922  01  DFHCOMMAREA                 PIC  X(1024).
00923
00924 *01 PARMLIST .
00925 *    02  FILLER                  PIC S9(08)  COMP.
00926 *    02  L-ACCT-POINTER          PIC S9(08)  COMP.
00927 *    02  L-ACTV-POINTER          PIC S9(08)  COMP.
00928 *    02  L-ARCH-POINTER          PIC S9(08)  COMP.
00929 *    02  L-BENE-POINTER          PIC S9(08)  COMP.
00930 *    02  L-CERT-POINTER          PIC S9(08)  COMP.
00931 *    02  L-CLM-POINTER           PIC S9(08)  COMP.
00932 *    02  L-CNTL-POINTER          PIC S9(08)  COMP.
00933 *    02  L-COMP-POINTER          PIC S9(08)  COMP.
00934 *    02  L-TEXT-POINTER          PIC S9(08)  COMP.
00935 *    02  L-PROD-POINTER          PIC S9(08)  COMP.
00936 *    02  L-PLCY-POINTER          PIC S9(08)  COMP.
00937 *    02  L-PLAN-POINTER          PIC S9(08)  COMP.
00938 *    02  L-VARIABLE-POINTER      PIC S9(08)  COMP.
00939
00940                                  EJECT
00941 *                                COPY ERCACCT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCACCT                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.031                          *
00006 *                                                                *
00007 *   CREDIT SYSTEM ACCOUNT MASTER FILE                            *
00008 *                                                                *
00009 *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
00010 *   VSAM ACCOUNT MASTER FILES.                                   *
00011 *                                                                *
00012 *   FILE DESCRIPTION = ACCOUNT OR PRODUCER FILES                 *
00013 *                                                                *
00014 *   FILE TYPE = VSAM,KSDS                                        *
00015 *   RECORD SIZE = 2000  RECFORM = FIX                            *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ERACCT                    RKP=2,LEN=26   *
00018 *       ALTERNATE PATH1 = ERACCT2 (ALT GROUPING) RKP=28,LEN=26   *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
092705* 092705    2005050300006  PEMA  ADD SPP LEASES
022808* 022808    2007083100002  PEMA  ADD FREEZE STATUS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
030211* 030211  CR2010012100001  PEMA  ADD EMAILS FROM RDS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
101711* 101711  CR2011092000001  PEMA  ADD UNEARNED FACTOR STATE FOR DCC
021916* 021916  CR2014010900001  TANA  ADD NEW STATUS CODE VALUES
102004******************************************************************
00025
00026  01  ACCOUNT-MASTER.
00027      12  AM-RECORD-ID                      PIC XX.
00028          88  VALID-AM-ID                      VALUE 'AM'.
00029
00030      12  AM-CONTROL-PRIMARY.
00031          16  AM-COMPANY-CD                 PIC X.
00032          16  AM-MSTR-CNTRL.
00033              20  AM-CONTROL-A.
00034                  24  AM-CARRIER            PIC X.
00035                  24  AM-GROUPING.
00036                      28 AM-GROUPING-PREFIX PIC XXX.
00037                      28 AM-GROUPING-PRIME  PIC XXX.
00038                  24  AM-STATE              PIC XX.
00039                  24  AM-ACCOUNT.
00040                      28  AM-ACCOUNT-PREFIX PIC X(4).
00041                      28  AM-ACCOUNT-PRIME  PIC X(6).
00042              20  AM-CNTRL-1   REDEFINES   AM-CONTROL-A
00043                                            PIC X(19).
00044              20  AM-CNTRL-B.
00045                  24  AM-EXPIRATION-DT      PIC XX.
00046                  24  FILLER                PIC X(4).
00047              20  AM-CNTRL-2 REDEFINES AM-CNTRL-B.
00048                  24  AM-EXPIRE-DT          PIC 9(11)  COMP-3.
00049
00050      12  AM-CONTROL-BY-VAR-GRP.
00051          16  AM-COMPANY-CD-A1              PIC X.
00052          16  AM-VG-CARRIER                 PIC X.
00053          16  AM-VG-GROUPING                PIC X(6).
00054          16  AM-VG-STATE                   PIC XX.
00055          16  AM-VG-ACCOUNT                 PIC X(10).
00056          16  AM-VG-DATE.
00057              20  AM-VG-EXPIRATION-DT       PIC XX.
00058              20  FILLER                    PIC X(4).
00059          16  AM-VG-EXP-DATE REDEFINES AM-VG-DATE
00060                                            PIC 9(11)      COMP-3.
030211     12  FILLER REDEFINES AM-CONTROL-BY-VAR-GRP.
030211         16  FILLER                        PIC X(10).
030211         16  AM-VG-KEY3.
030211             20  AM-VG3-ACCOUNT            PIC X(10).
030211             20  AM-VG3-EXP-DT             PIC XX.
030211         16  FILLER                        PIC X(4).
00061      12  AM-MAINT-INFORMATION.
00062          16  AM-LAST-MAINT-DT              PIC XX.
00063          16  AM-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
00064          16  AM-LAST-MAINT-USER            PIC X(4).
00065          16  FILLER                        PIC XX.
00066
00067      12  AM-EFFECTIVE-DT                   PIC XX.
00068      12  AM-EFFECT-DT                      PIC 9(11)      COMP-3.
00069
00070      12  AM-PREV-DATES  COMP-3.
00071          16  AM-PREV-EXP-DT                PIC 9(11).
00072          16  AM-PREV-EFF-DT                PIC 9(11).
00073
00074      12  AM-REPORT-CODE-1                  PIC X(10).
00075      12  AM-REPORT-CODE-2                  PIC X(10).
00076
00077      12  AM-CITY-CODE                      PIC X(4).
00078      12  AM-COUNTY-PARISH                  PIC X(6).
00079
00080      12  AM-NAME                           PIC X(30).
00081      12  AM-PERSON                         PIC X(30).
00082      12  AM-ADDRS                          PIC X(30).
00083      12  AM-CITY.
               16  AM-ADDR-CITY                  PIC X(28).
               16  AM-ADDR-STATE                 PIC XX.
00084      12  AM-ZIP.
00085          16  AM-ZIP-PRIME.
00086              20  AM-ZIP-PRI-1ST            PIC X.
00087                  88  AM-CANADIAN-POST-CODE    VALUE 'A' THRU 'Z'.
00088              20  FILLER                    PIC X(4).
00089          16  AM-ZIP-PLUS4                  PIC X(4).
00090      12  AM-CANADIAN-POSTAL-CODE  REDEFINES  AM-ZIP.
00091          16  AM-CAN-POSTAL-1               PIC XXX.
00092          16  AM-CAN-POSTAL-2               PIC XXX.
00093          16  FILLER                        PIC XXX.
00094      12  AM-TEL-NO.
00095          16  AM-AREA-CODE                  PIC 999.
00096          16  AM-TEL-PRE                    PIC 999.
00097          16  AM-TEL-NBR                    PIC 9(4).
00098      12  AM-TEL-LOC                        PIC X.
00099          88  AM-TEL-AT-HOME                   VALUE 'H'.
00100          88  AM-TEL-AT-BUSINESS               VALUE 'B'.
00101
00102      12  AM-COMM-STRUCTURE.
00103          16  AM-DEFN-1.
00104              20  AM-AGT-COMMS       OCCURS 10 TIMES.
00105                  24  AM-AGT.
00106                      28  AM-AGT-PREFIX     PIC X(4).
00107                      28  AM-AGT-PRIME      PIC X(6).
00108                  24  AM-COM-TYP            PIC X.
00109                  24  AM-L-COM              PIC SV9(5)     COMP-3.
00110                  24  AM-J-COM              PIC SV9(5)     COMP-3.
00111                  24  AM-A-COM              PIC SV9(5)     COMP-3.
00112                  24  AM-RECALC-LV-INDIC    PIC X.
00113                  24  AM-RETRO-LV-INDIC     PIC X.
00114                  24  AM-GL-CODES           PIC X.
00115                  24  AM-COMM-CHARGEBACK    PIC 9(02).
00116                  24  FILLER                PIC X(01).
00117          16  AM-DEFN-2   REDEFINES   AM-DEFN-1.
00118              20  AM-COM-TBLS        OCCURS 10 TIMES.
00119                  24  FILLER                PIC X(11).
00120                  24  AM-L-COMA             PIC XXX.
00121                  24  AM-J-COMA             PIC XXX.
00122                  24  AM-A-COMA             PIC XXX.
00123                  24  FILLER                PIC X(6).
00124
00125      12  AM-COMM-CHANGE-STATUS             PIC X.
00126          88  AM-COMMISSIONS-CHANGED           VALUE '*'.
00127
00128      12  AM-CSR-CODE                       PIC X(4).
00129
00130      12  AM-BILLING-STATUS                 PIC X.
00131          88  AM-ACCOUNT-BILLED                VALUE 'B'.
00132          88  AM-ACCOUNT-NOT-BILLED            VALUE ' '.
00133      12  AM-AUTO-REFUND-SW                 PIC X.
00134          88  AUTO-REFUNDS-USED                VALUE 'Y'.
00135          88  AUTO-REFUNDS-NOT-USED            VALUE 'N' ' '.
00136      12  AM-GPCD                           PIC 99.
00137      12  AM-IG                             PIC X.
00138          88  AM-HAS-INDIVIDUAL                VALUE '1'.
00139          88  AM-HAS-GROUP                     VALUE '2'.
00140      12  AM-STATUS                         PIC X.
00141          88  AM-ACCOUNT-ACTIVE                VALUE '0'.
00142          88  AM-ACCOUNT-INACTIVE              VALUE '1'.
00143          88  AM-ACCOUNT-TRANSFERRED           VALUE '2'.
102004         88  AM-ACCOUNT-CANCELLED             VALUE '3'.
022808         88  AM-ACCOUNT-FROZEN                VALUE '4'.
031811         88  AM-ACCOUNT-SUSPENDED             VALUE '5'.
021916         88  AM-ACCOUNT-DROPPED               VALUE '6'.
021916         88  AM-ACCOUNT-LAPSED                VALUE '7'.
021916         88  AM-ACCOUNT-RUN-OFF               VALUE '8'.
021916         88  AM-ACCOUNT-PENDING               VALUE '9'.
00144      12  AM-REMIT-TO                       PIC 99.
00145      12  AM-ID-NO                          PIC X(11).
00146
00147      12  AM-CAL-TABLE                      PIC XX.
00148      12  AM-LF-DEVIATION                   PIC XXX.
00149      12  AM-AH-DEVIATION                   PIC XXX.
00150      12  AM-LF-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00151      12  AM-AH-DEVIATION-PCT               PIC S9V9(6)    COMP-3.
00152      12  AM-LF-OB-RATE                     PIC S99V9(5)   COMP-3.
00153      12  AM-AH-OB-RATE                     PIC S99V9(5)   COMP-3.
00154      12  AM-LF-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00155      12  AM-AH-OB-RATE-JNT                 PIC S99V9(5)   COMP-3.
00156
00157      12  AM-USER-FIELDS.
00158          16  AM-FLD-1                      PIC XX.
00159          16  AM-FLD-2                      PIC XX.
00160          16  AM-FLD-3                      PIC XX.
00161          16  AM-FLD-4                      PIC XX.
00162          16  AM-FLD-5                      PIC XX.
00163
00164      12  AM-1ST-PROD-DATE.
00165          16  AM-1ST-PROD-YR                PIC XX.
00166          16  AM-1ST-PROD-MO                PIC XX.
00167          16  AM-1ST-PROD-DA                PIC XX.
00168      12  AM-ANNIVERSARY-DATE               PIC 9(11)  COMP-3.
00169      12  AM-CERTS-PURGED-DATE.
00170          16  AM-PUR-YR                     PIC XX.
00171          16  AM-PUR-MO                     PIC XX.
00172          16  AM-PUR-DA                     PIC XX.
00173      12  AM-HI-CERT-DATE                   PIC 9(11)  COMP-3.
00174      12  AM-LO-CERT-DATE                   PIC 9(11)  COMP-3.
00175      12  AM-ENTRY-DATE                     PIC 9(11)  COMP-3.
00176      12  AM-INACTIVE-DATE.
00177          16  AM-INA-MO                     PIC 99.
00178          16  AM-INA-DA                     PIC 99.
00179          16  AM-INA-YR                     PIC 99.
00180      12  AM-AR-HI-CERT-DATE                PIC XX.
00181
00182      12  AM-LF-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00183      12  AM-AH-PSI-FACTOR                  PIC S9V9(6)    COMP-3.
00184
00185      12  AM-OB-PAYMENT-MODE                PIC X.
00186          88  AM-OB-PAID-MONTHLY               VALUE 'M' ' '.
00187          88  AM-OB-PAID-QUARTERLY             VALUE 'Q'.
00188          88  AM-OB-PAID-SEMI-ANNUALLY         VALUE 'S'.
00189          88  AM-OB-PAID-ANNUALLY              VALUE 'A'.
00190
00191      12  AM-AH-ONLY-INDICATOR              PIC X.
00192          88  AM-AH-ONLY-ALLOWED               VALUE 'Y' ' '.
00193          88  AM-NO-AH-ONLY                    VALUE 'N'.
00194
00195      12  AM-EDIT-LOAN-OFC                  PIC X(01).
00196
00197      12  AM-OVER-SHORT.
00198          16 AM-OVR-SHT-AMT                 PIC S999V99    COMP-3.
00199          16 AM-OVR-SHT-PCT                 PIC S9V9(4)    COMP-3.
00200
011410     12  AM-DCC-PRODUCT-CODE               PIC XXX.
041910     12  AM-DCC-CLP-STATE                  PIC XX.
00202
00203      12  AM-RECALC-COMM                    PIC X.
00204      12  AM-RECALC-REIN                    PIC X.
00205
00206      12  AM-REI-TABLE                      PIC XXX.
00207      12  AM-REI-ET-LF                      PIC X.
00208      12  AM-REI-ET-AH                      PIC X.
00209      12  AM-REI-PE-LF                      PIC X.
00210      12  AM-REI-PE-AH                      PIC X.
00211      12  AM-REI-PRT-ST                     PIC X.
00212      12  AM-REI-FEE-LF                     PIC S9V9999    COMP-3.
00213      12  AM-REI-FEE-AH                     PIC S9V9999    COMP-3.
00214      12  AM-REI-LF-TAX                     PIC S9V9999    COMP-3.
00215      12  AM-REI-GROUP-A                    PIC X(6).
00216      12  AM-REI-MORT                       PIC X(4).
00217      12  AM-REI-PRT-OW                     PIC X.
00218      12  AM-REI-PR-PCT                     PIC S9V9999    COMP-3.
00219      12  AM-REI-78-PCT                     PIC S9V9999    COMP-3.
00220      12  AM-REI-AH-TAX                     PIC S9V9999    COMP-3.
00221      12  AM-REI-GROUP-B                    PIC X(6).
00222
00223      12  AM-TRUST-TYPE                     PIC X(2).
00224
00225      12  AM-EMPLOYER-STMT-USED             PIC X.
00226      12  AM-GROUPED-CHECKS-Y-N             PIC X.
00227
00228      12  AM-STD-AH-TYPE                    PIC XX.
00229      12  AM-EARN-METHODS.
00230          16  AM-EARN-METHOD-R              PIC X.
00231              88 AM-REF-RL-R78                 VALUE 'R'.
00232              88 AM-REF-RL-PR                  VALUE 'P'.
00233              88 AM-REF-RL-MEAN                VALUE 'M'.
00234              88 AM-REF-RL-ANTICIPATION        VALUE 'A'.
00235          16  AM-EARN-METHOD-L              PIC X.
00236              88 AM-REF-LL-R78                 VALUE 'R'.
00237              88 AM-REF-LL-PR                  VALUE 'P'.
00238              88 AM-REF-LL-MEAN                VALUE 'M'.
00239              88 AM-REF-LL-ANTICIPATION        VALUE 'A'.
00240          16  AM-EARN-METHOD-A              PIC X.
00241              88 AM-REF-AH-R78                 VALUE 'R'.
00242              88 AM-REF-AH-PR                  VALUE 'P'.
00243              88 AM-REF-AH-MEAN                VALUE 'M'.
00244              88 AM-REF-AH-ANTICIPATION        VALUE 'A'.
00245              88 AM-REF-AH-CALIF-SPEC          VALUE 'C'.
00246              88 AM-REF-AH-NET                 VALUE 'N'.
00247
00248      12  AM-TOL-PREM                       PIC S999V99    COMP-3.
00249      12  AM-TOL-REF                        PIC S999V99    COMP-3.
00250      12  AM-TOL-CLM                        PIC S999V99    COMP-3.
00251
00252      12  AM-RET-Y-N                        PIC X.
00253      12  AM-RET-P-E                        PIC X.
00254      12  AM-LF-RET                         PIC S9V9999    COMP-3.
00255      12  AM-AH-RET                         PIC S9V9999    COMP-3.
00256      12  AM-RET-GRP                        PIC X(6).
00257      12  AM-RETRO-POOL  REDEFINES  AM-RET-GRP.
00258          16  AM-POOL-PRIME                 PIC XXX.
00259          16  AM-POOL-SUB                   PIC XXX.
00260      12  AM-RETRO-EARNINGS.
00261          16  AM-RET-EARN-R                 PIC X.
00262          16  AM-RET-EARN-L                 PIC X.
00263          16  AM-RET-EARN-A                 PIC X.
00264      12  AM-RET-ST-TAX-USE                 PIC X.
00265          88  CHARGE-ST-TAXES-ON-RETRO         VALUE 'Y' 'E' 'P'.
00266          88  TAXES-NOT-IN-RETRO               VALUE 'N' ' '.
00267      12  AM-RETRO-BEG-EARNINGS.
00268          16  AM-RET-BEG-EARN-R             PIC X.
00269          16  AM-RET-BEG-EARN-L             PIC X.
00270          16  AM-RET-BEG-EARN-A             PIC X.
00271      12  AM-RET-MIN-LOSS-L                 PIC SV999      COMP-3.
00272      12  AM-RET-MIN-LOSS-A                 PIC SV999      COMP-3.
00273
00274      12  AM-USER-SELECT-OPTIONS.
00275          16  AM-USER-SELECT-1              PIC X(10).
00276          16  AM-USER-SELECT-2              PIC X(10).
00277          16  AM-USER-SELECT-3              PIC X(10).
00278          16  AM-USER-SELECT-4              PIC X(10).
00279          16  AM-USER-SELECT-5              PIC X(10).
00280
00281      12  AM-LF-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00282
00283      12  AM-AH-RPT021-EXP-PCT              PIC S9(3)V9(4) COMP-3.
00284
00285      12  AM-RPT045A-SWITCH                 PIC X.
00286          88  RPT045A-OFF                   VALUE 'N'.
00287
00288      12  AM-INSURANCE-LIMITS.
00289          16  AM-MAX-MON-BEN                PIC S9(7)      COMP-3.
00290          16  AM-MAX-TOT-BEN                PIC S9(7)      COMP-3.
00291
00292      12  AM-PROFILE-CHANGE-SWITCH          PIC X.
00293          88  AM-PROFILE-DATA-CHANGED          VALUE '*'.
00294
00295      12  AM-DISMBR-COVERAGE-SW             PIC X.
00296          88  AM-DISMBR-COVERAGE               VALUE 'Y'.
00297          88  AM-NO-DISMBR-COVERAGE            VALUE 'N'.
00298
00299      12  AM-CANCEL-FEE                     PIC S9(3)V9(2) COMP-3.
00300
00301      12  AM-TOL-REF-PCT                    PIC S9V9(4)    COMP-3.
090803     12  AM-CLP-TOL-PCT                    PIC S9V9(4)    COMP-3.
092705     12  AM-SPP-LEASE-COMM                 PIC S9(5)V99   COMP-3.
           12  AM-DCC-MAX-MARKETING-FEE          PIC S9(5)      COMP-3.
           12  AM-DCC-UEF-STATE                  PIC XX.
           12  FILLER                            PIC XXX.
120406     12  AM-REPORT-CODE-3                  PIC X(10).
090803*    12  FILLER                            PIC X(22).
00303
00304      12  AM-RESERVE-DATE.
00305          16  AM-TARGET-LOSS-RATIO          PIC S9V9(4) COMP-3.
00306          16  AM-LIFE-IBNR-PCT              PIC S9V9(4) COMP-3.
00307          16  AM-CRDT-MODIFICATION-PCT      PIC S9V9(4) COMP-3.
00308
00309      12  AM-3RD-PARTY-NOTIF-LEVEL          PIC 99.
00310      12  AM-NOTIFICATION-TYPES.
00311          16  AM-NOTIF-OF-LETTERS           PIC X.
00312          16  AM-NOTIF-OF-PAYMENTS          PIC X.
00313          16  AM-NOTIF-OF-REPORTS           PIC X.
00314          16  AM-NOTIF-OF-STATUS            PIC X.
00315
00316      12  AM-BENEFIT-TABLE-USAGE            PIC X.
00317          88  AM-BENEFIT-TABLE-USED            VALUE 'Y'.
00318          88  AM-USE-DEVIATIONS-ONLY           VALUE 'D'.
00319          88  AM-EDIT-BENEFITS-ONLY            VALUE 'E'.
00320          88  AM-EDITS-NOT-USED                VALUE ' '  'N'.
00321
00322      12  AM-BENEFIT-CONTROLS.
00323          16  AM-ALLOWABLE-BENEFITS  OCCURS  20  TIMES.
00324              20  AM-BENEFIT-CODE           PIC XX.
00325              20  AM-BENEFIT-TYPE           PIC X.
00326              20  AM-BENEFIT-REVISION       PIC XXX.
00327              20  AM-BENEFIT-REM-TERM       PIC X.
00328              20  AM-BENEFIT-RETRO-Y-N      PIC X.
00329              20  FILLER                    PIC XX.
00330          16  FILLER                        PIC X(80).
00331
00332      12  AM-TRANSFER-DATA.
00333          16  AM-TRANSFERRED-FROM.
00334              20  AM-TRNFROM-CARRIER        PIC X.
00335              20  AM-TRNFROM-GROUPING.
00336                  24  AM-TRNFROM-GRP-PREFIX PIC XXX.
00337                  24  AM-TRNFROM-GRP-PRIME  PIC XXX.
00338              20  AM-TRNFROM-STATE          PIC XX.
00339              20  AM-TRNFROM-ACCOUNT.
00340                  24  AM-TRNFROM-ACCT-PREFIX PIC X(4).
00341                  24  AM-TRNFROM-ACCT-PRIME PIC X(6).
00342              20  AM-TRNFROM-DTE            PIC XX.
00343          16  AM-TRANSFERRED-TO.
00344              20  AM-TRNTO-CARRIER          PIC X.
00345              20  AM-TRNTO-GROUPING.
00346                  24  AM-TRNTO-GRP-PREFIX   PIC XXX.
00347                  24  AM-TRNTO-GRP-PRIME    PIC XXX.
00348              20  AM-TRNTO-STATE            PIC XX.
00349              20  AM-TRNTO-ACCOUNT.
00350                  24  AM-TRNTO-ACCT-PREFIX  PIC X(4).
00351                  24  AM-TRNTO-ACCT-PRIME   PIC X(6).
00352              20  AM-TRNTO-DTE              PIC XX.
00353          16  FILLER                        PIC X(10).
00354
00355      12  AM-SAVED-REMIT-TO                 PIC 99.
00356
00357      12  AM-COMM-STRUCTURE-SAVED.
00358          16  AM-DEFN-1-SAVED.
00359              20  AM-AGT-COMMS-SAVED    OCCURS 10 TIMES.
00360                  24  AM-AGT-SV             PIC X(10).
00361                  24  AM-COM-TYP-SV         PIC X.
00362                  24  AM-L-COM-SV           PIC SV9(5)     COMP-3.
00363                  24  AM-J-COM-SV           PIC SV9(5)     COMP-3.
00364                  24  AM-A-COM-SV           PIC SV9(5)     COMP-3.
00365                  24  AM-RECALC-LV-INDIC-SV PIC X.
00366                  24  FILLER                PIC X.
00367                  24  AM-GL-CODES-SV        PIC X.
00368                  24  AM-COM-CHARGEBACK-SV  PIC 99.
00369                  24  FILLER                PIC X.
00370          16  AM-DEFN-2-SAVED   REDEFINES   AM-DEFN-1-SAVED.
00371              20  AM-COM-TBLS-SAVED    OCCURS 10 TIMES.
00372                  24  FILLER                PIC X(11).
00373                  24  AM-L-COMA-SV          PIC XXX.
00374                  24  AM-J-COMA-SV          PIC XXX.
00375                  24  AM-A-COMA-SV          PIC XXX.
00376                  24  FILLER                PIC X(6).
00377
00378      12  AM-FLC-NET-PREMIUM-ALLOWANCE.
00379          16 AM-ACCOUNT-ALLOWANCE OCCURS  5 TIMES.
00380             20  AM-ALLOW-BEGIN-RANGE       PIC S9(5)      COMP-3.
00381             20  AM-ALLOW-END-RANGE         PIC S9(5)      COMP-3.
00382             20  AM-ALLOWANCE-AMT           PIC S9(5)V99   COMP-3.
00383
122806     12  AM-ORIG-DEALER-NO                 PIC X(10).
122806     12  FILLER                            PIC X(120).
00385
00386      12  AM-ACCOUNT-EXECUTIVE-DATA.
00387          16  AM-CONTROL-NAME               PIC X(30).
00388          16  AM-EXECUTIVE-ONE.
00389              20  AM-EXEC1-NAME             PIC X(15).
00390              20  AM-EXEC1-DIS-PERCENT      PIC S9(01)V9(04)
00391                                                           COMP-3.
00392              20  AM-EXEC1-LIFE-PERCENT     PIC S9(01)V9(04)
00393                                                           COMP-3.
00394          16  AM-EXECUTIVE-TWO.
00395              20  AM-EXEC2-NAME             PIC X(15).
00396              20  AM-EXEC2-DIS-PERCENT      PIC S9(01)V9(04)
00397                                                           COMP-3.
00398              20  AM-EXEC2-LIFE-PERCENT     PIC S9(01)V9(04)
00399                                                           COMP-3.
00400
00401      12  AM-RETRO-ADDITIONAL-DATA.
00402          16  AM-RETRO-QUALIFY-LIMIT        PIC S9(7)      COMP-3.
00403          16  AM-RETRO-PREM-P-E             PIC X.
00404          16  AM-RETRO-CLMS-P-I             PIC X.
00405          16  AM-RETRO-RET-BRACKET-LF.
00406              20  AM-RETRO-RET-METHOD-LF    PIC X.
00407                  88  AM-RETRO-USE-PCT-LF      VALUE 'P' ' '.
00408                  88  AM-RETRO-USE-SCALE-LF    VALUE 'S'.
00409              20  AM-RETRO-RET-BASIS-LF     PIC X.
00410                  88  AM-RETRO-EARN-BASIS-LF   VALUE 'E' ' '.
00411                  88  AM-RETRO-PAID-BASIS-LF   VALUE 'P'.
00412              20  AM-RETRO-BRACKETS-LF  OCCURS  3 TIMES.
00413                  24  AM-RETRO-RET-PCT-LF   PIC S9V9999    COMP-3.
00414                  24  AM-RETRO-RET-THRU-LF  PIC S9(7)      COMP-3.
00415          16  AM-RETRO-RET-BRACKET-AH.
00416              20  AM-RETRO-RET-METHOD-AH    PIC X.
00417                  88  AM-RETRO-USE-PCT-AH      VALUE 'P' ' '.
00418                  88  AM-RETRO-USE-SCALE-AH    VALUE 'S'.
00419                  88  AM-RETRO-USE-LIFE-METHOD VALUE 'L'.
00420              20  AM-RETRO-RET-BASIS-AH     PIC X.
00421                  88  AM-RETRO-EARN-BASIS-AH   VALUE 'E' ' '.
00422                  88  AM-RETRO-PAID-BASIS-AH   VALUE 'P'.
00423              20  AM-RETRO-BRACKETS-AH  OCCURS  3 TIMES.
00424                  24  AM-RETRO-RET-PCT-AH   PIC S9V9999    COMP-3.
00425                  24  AM-RETRO-RET-THRU-AH  PIC S9(7)      COMP-3.
00426
00427      12  AM-COMMENTS.
00428          16  AM-COMMENT-LINE           PIC X(50)   OCCURS 5 TIMES.
00429
00430      12  AM-CLIENT-OVERLAY-FLI   REDEFINES   AM-COMMENTS.
00431          16  AM-FLI-RETRO-SHARE-CODE       PIC X.
00432          16  AM-FLI-BILLING-CODE           PIC X.
00433          16  AM-FLI-ALT-STATE-CODE         PIC XX.
00434          16  AM-FLI-UNITED-IDENT           PIC X.
00435          16  AM-FLI-INTEREST-LOST-DATA.
00436              20  AM-FLI-BANK-NO            PIC X(5).
00437              20  AM-FLI-BANK-BALANCE       PIC S9(9)V99   COMP-3.
00438              20  AM-FLI-BANK-1ST-6-PREM    PIC S9(9)V99   COMP-3.
00439              20  AM-FLI-BANK-CAP-AMT       PIC S9(9)V99   COMP-3.
00440          16  AM-FLI-ALT-AGENT-CODES   OCCURS 10 TIMES.
00441              20  AM-FLI-AGT                PIC X(9).
00442              20  AM-FLI-AGT-COMM-ACC       PIC X.
00443              20  AM-FLI-AGT-SHARE-PCT      PIC S9V99      COMP-3.
00444          16  FILLER                        PIC X(102).
00445
00446      12  AM-CLIENT-OVERLAY-DMD   REDEFINES   AM-COMMENTS.
00447          16  AM-ALLOWABLE-DMD-BENEFITS  OCCURS 30 TIMES.
00448              20  AM-BENEFIT-DMD-CODE         PIC XX.
00449              20  AM-BENEFIT-DMD-TYPE         PIC X.
00450              20  AM-BENEFIT-DMD-REVISION     PIC XXX.
00451              20  AM-BENEFIT-DMD-REM-TERM     PIC X.
00452              20  AM-BENEFIT-DMD-RETRO-Y-N    PIC X.
00453          16  FILLER                          PIC X(10).
00454 ******************************************************************
00942                                  EJECT
00943 *                                COPY ELCTRLR.
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
00944                                  EJECT
00945 *                                COPY ELCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.007                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCH                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCH2 (RECORD TYPE)  RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  THERE ARE CID MODS IN COPYBOOK ELCARCH                        *
00017 ******************************************************************
00018  01  LETTER-ARCHIVE.
00019      12  LA-RECORD-ID                PIC XX.
00020          88  VALID-LA-ID                VALUE 'LA'.
00021
00022      12  LA-CONTROL-PRIMARY.
00023          16  LA-COMPANY-CD           PIC X.
00024          16  LA-ARCHIVE-NO           PIC S9(8)     COMP.
00025          16  LA-RECORD-TYPE          PIC X.
00026              88  LA-HEADER-DATA         VALUE '1'.
00027              88  LA-ADDRESS-DATA        VALUE '2'.
00028              88  LA-TEXT-DATA           VALUE '3'.
00029              88  LA-FORM-CONTROL-HDR    VALUE '4'.
00030          16  LA-LINE-SEQ-NO          PIC S9(4)     COMP.
00031
00032      12  LA-CONTROL-BY-TYPE.
00033          16  LA-COMPANY-CD-A1        PIC X.
00034          16  LA-RECORD-TYPE-A1       PIC X.
00035          16  LA-ARCHIVE-NO-A1        PIC S9(8)     COMP.
00036          16  LA-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
00037
00038      12  LA-TEXT-RECORD.
00039          16  LA-SKIP-CONTROL         PIC XX.
00040              88  NO-LINES-SKIPPED       VALUE SPACES.
00041              88  SKIP-TO-NEXT-PAGE      VALUE '99'.
00042          16  LA-TEXT-LINE            PIC X(70).
00043
00044      12  LA-ADDRESS-RECORD  REDEFINES  LA-TEXT-RECORD.
00045          16  FILLER                  PIC XX.
00046          16  LA-ADDRESS-LINE         PIC X(30).
00047          16  FILLER                  PIC X(40).
00048
00049      12  LA-HEADER-RECORD  REDEFINES  LA-TEXT-RECORD.
00050          16  FILLER                  PIC XX.
00051          16  LA-CARRIER              PIC X.
00052          16  LA-CLAIM-NO             PIC X(7).
00053          16  LA-CERT-NO.
00054              20  LA-CERT-PRIME       PIC X(10).
00055              20  LA-CERT-SFX         PIC X.
00056          16  LA-NO-OF-COPIES         PIC S9.
00057          16  LA-RESEND-DATE          PIC XX.
00058          16  LA-PROCESSOR-CD         PIC X(4).
00059          16  LA-CREATION-DT          PIC XX.
00060          16  LA-INITIAL-PRINT-DATE   PIC XX.
00061          16  LA-RESEND-PRINT-DATE    PIC XX.
00062          16  LA-CORR-TRLR-SEQ        PIC S9(4)    COMP.
00063          16  LA-1ST-RESEND-PRINT-DT  PIC XX.
CIDMOD*
00064 * -----  16  LA-DMD-ADDITIONAL-FIELDS.
00065 *   I        20  LA-DMD-LETTER-FORM      PIC X(4).
00066 *   I        20  LA-DMD-PROD-CODE        PIC XX.
00067 *   I        20  LA-DMD-RES-ST           PIC XX.
00068 *   I        20  LA-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
00069 *   I        20  LA-DMD-LETTER-STATUS    PIC X.
00070 *  NEW           88  LA-DMD-LETTER-ONLINE   VALUE '1'.
00071 *  DMD           88  LA-DMD-LETTER-PURGED   VALUE '2'.
00072 *  CHGS          88  LA-DMD-LETTER-RELOADED VALUE '3'.
00073 *   I        20  LA-DMD-LETTER-PURGE-DT  PIC XX.
00074 *   I        20  LA-DMD-LETTER-RELOAD-DT PIC XX.
00075 *   I        20  LA-DMD-UND-CODE         PIC XX.
00076 *   I        20  LA-DMD-BEN-CODE         PIC XX.
00077 *   V    16  FILLER                  PIC X(15).
CIDMOD* -----
CIDMOD*
CIDMOD* REINSERTED  CSO  MODS
CIDMOD*
CIDMOD         16  FILLER.
CIDMOD             20  FILLER                  PIC X(29).
CIDMOD             20  LA-CSO-LETTER-STATUS    PIC X.
CIDMOD                 88  LA-CSO-LETTER-ONLINE   VALUE '1'.
CIDMOD                 88  LA-CSO-LETTER-PURGED   VALUE '2'.
CIDMOD                 88  LA-CSO-LETTER-RELOADED VALUE '3'.
CIDMOD             20  LA-CSO-LETTER-PURGE-DT  PIC XX.
CIDMOD             20  LA-CSO-LETTER-RELOAD-DT PIC XX.
CIDMOD*
00078
00079      12  LA-FORM-CONTROL-HEADER REDEFINES  LA-TEXT-RECORD.
00080          16  FILLER                  PIC XX.
00081          16  LA4-CARRIER             PIC X.
00082          16  LA4-CLAIM-NO            PIC X(7).
00083          16  LA4-CERT-NO.
00084              20  LA4-CERT-PRIME      PIC X(10).
00085              20  LA4-CERT-SFX        PIC X.
00086          16  LA4-NO-OF-COPIES        PIC S9.
00087          16  LA4-RESEND-DATE         PIC XX.
00088          16  LA4-PROCESSOR-CD        PIC X(4).
00089          16  LA4-CREATION-DT         PIC XX.
00090          16  LA4-INITIAL-PRINT-DATE  PIC XX.
00091          16  LA4-RESEND-PRINT-DATE   PIC XX.
00092          16  LA4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
00093          16  LA4-FORM-TYPE           PIC X.
00094              88  LA4-INITIAL-FORM    VALUE '1'.
00095              88  LA4-PROGRESS-FORM   VALUE '2'.
00096          16  LA4-FORM-REM-PRINT-DT   PIC X(02).
00097          16  LA4-STATE               PIC X(02).
00098          16  FILLER                  PIC X(31).
00099 ******************************************************************
00946                                  EJECT
00947 *                                COPY ELCBENE.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCBENE.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.006                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = BENEFICIARY FILE                          *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 500   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELBENE                   RKP=2,LEN=12    *
00013 *     ALTERNATE PATH1 = ELBENE2 (ALT BY NAME)    RKP=14,LEN=42   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 *                                                                *
CIDMOD*  NO  CID  MODS  TO  COPYBOOK  ELCBENE                          *
00018 ******************************************************************
013017*                   C H A N G E   L O G
013017*
013017* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
013017*-----------------------------------------------------------------
013017*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
013017* EFFECTIVE    NUMBER
013017*-----------------------------------------------------------------
013017* 013017  CR2016053100001  PEMA  ACH PROCESSING
082317* 082317  CR2017082100003  PEMA  Add sub type
032019* 032019  CR2019011400002  PEMA  Add email address for ach report
013017******************************************************************
00019
00020  01  BENEFICIARY-MASTER.
00021      12  BE-RECORD-ID                PIC XX.
00022          88  VALID-BE-ID                VALUE 'BE'.
00023
00024      12  BE-CONTROL-PRIMARY.
00025          16  BE-COMPANY-CD           PIC X.
00026          16  BE-RECORD-TYPE          PIC X.
00027              88  BENEFICIARY-RECORD  VALUE 'B'.
00028              88  ADJUSTOR-RECORD     VALUE 'A'.
00029          16  BE-BENEFICIARY          PIC X(10).
00030      12  BE-CONTROL-BY-NAME.
00031          16  BE-COMPANY-CD-A1        PIC X.
00032          16  BE-RECORD-TYPE-A1       PIC X.
00033          16  BE-MAIL-TO-NAME-A1      PIC X(30).
00034          16  BE-ALTERNATE-PRIME-A1   PIC X(10).
00035
00036      12  BE-LAST-MAINT-DT            PIC XX.
00037      12  BE-LAST-MAINT-BY            PIC X(4).
00038      12  BE-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
00039
00040      12  BE-ADDRESS-DATA.
00041          16  BE-MAIL-TO-NAME         PIC X(30).
00042          16  BE-ADDRESS-LINE-1       PIC X(30).
00043          16  BE-ADDRESS-LINE-2       PIC X(30).
00044          16  BE-ADDRESS-LINE-3       PIC X(30).
00045          16  BE-CITY-STATE.
051810             20  BE-CITY             PIC X(28).
051810             20  BE-STATE            PIC XX.
00046          16  BE-ZIP-CODE.
00047              20  BE-ZIP-PRIME.
00048                  24  BE-ZIP-1ST      PIC X.
00049                      88  BE-CANADIAN-POST-CODE
00050                                          VALUE 'A' THRU 'Z'.
00051                  24  FILLER          PIC X(4).
00052              20  BE-ZIP-PLUS4        PIC X(4).
00053          16  BE-CANADIAN-POSTAL-CODE  REDEFINES  BE-ZIP-CODE.
00054              20  BE-CAN-POSTAL-1     PIC XXX.
00055              20  BE-CAN-POSTAL-2     PIC XXX.
00056              20  FILLER              PIC XXX.
00057          16  BE-PHONE-NO             PIC 9(11)     COMP-3.
00058          16  BE-GROUP-CHECKS-Y-N     PIC X.
00059
00060 ******************************************************************
00061 *    THE BE-CARRIER FIELD IS USED BY 'AIG' TO DETERMINE HOW TO   *
00062 *    SET THE CARRIER CODE IN THE PENDING CLAIM FILE.             *
00063 ******************************************************************
00064      12  BE-CARRIER                  PIC X.
00065
00066      12  BE-ADDRESS-DATA2.
00067          16  BE-MAIL-TO-NAME2        PIC X(30).
00068          16  BE-ADDRESS-LINE-12      PIC X(30).
00069          16  BE-ADDRESS-LINE-22      PIC X(30).
00070          16  BE-ADDRESS-LINE-32      PIC X(30).
00071          16  BE-CITY-STATE2.
051810             20  BE-CITY2            PIC X(28).
051810             20  BE-STATE2           PIC XX.
00072          16  BE-ZIP-CODE2.
00073              20  BE-ZIP-PRIME2.
00074                  24  BE-ZIP-1ST2     PIC X.
00075                      88  BE-CANADIAN-POST-CODE2
00076                                          VALUE 'A' THRU 'Z'.
00077                  24  FILLER          PIC X(4).
00078              20  BE-ZIP-PLUS42       PIC X(4).
00079          16  BE-CANADIAN-POSTAL-CODE2 REDEFINES  BE-ZIP-CODE2.
00080              20  BE-CAN-POSTAL-12    PIC XXX.
00081              20  BE-CAN-POSTAL-22    PIC XXX.
00082              20  FILLER              PIC XXX.
00083          16  BE-PHONE-NO2            PIC 9(11)     COMP-3.
               16  BE-ACH-DATA.
                   20  BE-ACH-YES-OR-NO    PIC X.
                       88  BE-ON-ACH       VALUE 'Y'.
                       88  BE-NOT-ON-ACH   VALUE 'N' ' '.
                   20  BE-ACH-ABA-ROUTING-NUMBER
                                           PIC X(15).
                   20  BE-ACH-BANK-ACCOUNT-NUMBER
                                           PIC X(20).
                   20  BE-ACH-SUB-TYPE     PIC XX.
032019             20  BE-ACH-EMAIL-YN     PIC X.
032019                 88  BE-EMAIL-ACH-RPT  VALUE 'Y'.
032019             20  be-ach-email-addr   PIC X(40).
00084          16  BE-BILLING-STMT-DATA.
032019*            20  BE-BSR-PHONE-NUM    PIC 9(11)     COMP-3.
00091              20  BE-BSR-FAX-NUM      PIC 9(11)     COMP-3.
00092              20  BE-OUTPUT-TYPE      PIC X.
00093                  88  BE-FAX-OUTPUT         VALUE 'F'.
00094                  88  BE-PRINT-OUTPUT       VALUE 'P' ' '.
00095
032019     12  filler                      PIC X(16).
00097 ******************************************************************
00948                                  EJECT
00949 *                                COPY ELCCERT.
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
00950                                  EJECT
00951 *                                COPY ELCMSTR.
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
00952                                  EJECT
00953 *                                COPY ELCCNTL.
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
00954                                  EJECT
00955 *                                COPY ERCCOMP.
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
00956                                  EJECT
00957 *                                COPY ELCTEXT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCTEXT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT FILES FOR HELP DISPLAY,              *
00008 *                                     FORM LETTERS,              *
00009 *                                     CERT FORM DISPLAY.
00010 *                                                                *
00011 *   FILE TYPE = VSAM,KSDS                                        *
00012 *   RECORD SIZE = 100   RECFORM = FIXED                          *
00013 *                                                                *
00014 *   BASE CLUSTER NAME = ELLETR (LETTERS)   RKP=2,LEN=15          *
00015 *       ALTERNATE INDEX = NONE                                   *
00016 *                                                                *
00017 *   BASE CLUSTER NAME = ELFORM (FORMS)     RKP=2,LEN=15          *
00018 *       ALTERNATE INDEX = NONE                                   *
00019 *                                                                *
00020 *   BASE CLUSTER NAME = ELHELP (HELP)      RKP=2,LEN=15          *
00021 *       ALTERNATE INDEX = NONE                                   *
00022 *                                                                *
00023 *   LOG = NO                                                     *
00024 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00025 ******************************************************************
00026  01  TEXT-FILES.
00027      12  TEXT-FILE-ID                PIC XX.
00028          88  FORMS-FILE-TEXT            VALUE 'TF'.
00029          88  LETTER-FILE-TEXT           VALUE 'TL'.
00030          88  HELP-FILE-TEXT             VALUE 'TH'.
00031
00032      12  TX-CONTROL-PRIMARY.
00033          16  TX-COMPANY-CD           PIC X.
00034              88  TX-SYSTEM-WIDE-FILE    VALUE LOW-VALUE.
00035          16  TX-ACCESS-CD-GENL       PIC X(12).
00036
00037          16  TX-LETTER-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00038              20  TX-LETTER-NO        PIC X(4).
00039              20  FILLER              PIC X(8).
00040
00041          16  TX-FORM-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00042              20  TX-FORM-NO          PIC X(12).
00043
00044          16  TX-HELP-ACCESS  REDEFINES  TX-ACCESS-CD-GENL.
00045              20  TX-HELP-TYPE        PIC X.
00046                  88  HELP-FOR-GENERAL   VALUE ' '.
00047                  88  HELP-BY-SCREEN     VALUE 'S'.
00048                  88  HELP-BY-ERROR      VALUE 'E'.
00049              20  TX-SCREEN-OR-ERROR  PIC X(4).
00050                  88  GENERAL-INFO-HELP  VALUE '0000'.
00051              20  TX-HELP-FOR-COMPANY  PIC XXX.
00052                  88  NOT-COMPANY-SPECIFIC VALUE '   '.
00053              20  FILLER              PIC X(4).
00054
00055          16  TX-LINE-SEQUENCE        PIC S9(4)     COMP.
00056
00057      12  TX-PROCESS-CONTROL          PIC XX.
00058          88  LETTER-LINE-SKIPS          VALUE '01' THRU '99'.
00059
00060      12  TX-TEXT-LINE                PIC X(70).
00061
00062      12  TX-FORM-SQUEEZE-CONTROL     PIC X.
00063          88  TX-FORM-SQUEEZE-ON         VALUE 'Y'.
00064          88  TX-FORM-SQUEEZE-OFF        VALUE SPACES.
00065          88  TX-VALID-FORM-SQUEEZE-VALUE
00066                                         VALUE 'Y' ' '.
00067
00068      12  TX-LINE-SQUEEZE-CONTROL     PIC X.
00069          88  TX-ADJUST-TO-LINE-LENGTH   VALUE 'A'.
00070          88  TX-CONTINUE-PARAGRAPH      VALUE 'C'.
00071          88  TX-DO-NOT-ADJUST           VALUE 'N'.
00072          88  TX-FORM-CONTROL-LINE       VALUE 'K'.
00073          88  TX-NEW-PARAGRAPH           VALUE 'P'.
00074          88  TX-NO-SPECIAL-INSTRUCTION  VALUE ' '.
00075          88  TX-VALID-LINE-SQ-VALUE     VALUE 'A' 'C' 'P'
00076                                               'K' 'N' ' '
00077                                               'Z'.
00078
00079      12  TX-ARCHIVE-SW               PIC X.
00080          88  TX-ARCHIVE-THIS-LETTER     VALUE 'Y'.
00081          88  TX-DO-NOT-ARCHIVE          VALUE SPACES.
00082          88  TX-VALID-ARCHIVE-VALUE     VALUE 'Y' ' '.
00083
00084      12  TX-LAST-MAINTENANCED-BY     PIC X(4).
00085      12  TX-LAST-MAINTENANCED-DT     PIC X(2).
00086
00087      12  TX-BSR-CODE                 PIC X.
00088          88  TX-BSR-LETTER              VALUE 'B'.
00089          88  TX-NON-BSR-LETTER          VALUE ' '.
00090
00091      12  FILLER                      PIC X.
00092 *****************************************************************
00958                                  EJECT
00959 *                                COPY MPCPROD.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPROD                             *
00004 *                            VMOD=1.010                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER MASTER FILE                         *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   VSAM PRODUCER MASTER FILE.                                   *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER MASTER FILE                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 2000 RECFORM = FIXED                           *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPROD                    RKP=02,LEN=22  *
00017 *       ALTERNATE PATH1 = MPPROD2 (ALT GROUPING)  RKP=48,LEN=22  *
00018 *       ALTERNATE PATH2 = MPPROD3 (PRODUCER NAME) RKP=90,LEN=56  *
00019 *                                                                *
00020 *   LOG = NO                                                     *
00021 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00022 *                                                                *
00023 *                                                                *
00024 ******************************************************************
00025
00026  01  PRODUCER-MASTER.
00027      12  PD-RECORD-ID                 PIC  X(02).
00028          88  PD-VALID-ID                   VALUE 'PD'.
00029
00030 ******************************************************************
00031 *   BASE CLUSTER NAME = MPPROD                    RKP=2,LEN=22   *
00032 ******************************************************************
00033
00034      12  PD-CONTROL-PRIMARY-BATCH.
00035          16  FILLER                   PIC  X(20).
00036          16  PD-EXPIRE-DT.
00037              20  PD-EXPIRE-DT-YY      PIC  9(02).
00038              20  PD-EXPIRE-DT-MM      PIC  9(02).
00039              20  PD-EXPIRE-DT-DD      PIC  9(02).
00040      12  FILLER REDEFINES PD-CONTROL-PRIMARY-BATCH.
00041          16  PD-CONTROL-PRIMARY.
00042              20  PD-COMPANY-CD        PIC  X(01).
00043              20  PD-MSTR-CNTRL.
00044                  24  PD-CONTROL-A.
00045                      28  PD-CARRIER   PIC  X(01).
00046                      28  PD-GROUPING.
00047                          32 PD-GROUPING-PREFIX
00048                                       PIC  X(03).
00049                          32 PD-GROUPING-PRIME
00050                                       PIC  X(03).
00051                      28  PD-STATE     PIC  X(02).
00052                      28  PD-PRODUCER.
00053                          32  PD-PRODUCER-PREFIX
00054                                       PIC  X(04).
00055                          32  PD-PRODUCER-PRIME
00056                                       PIC  X(06).
00057                  24  PD-CNTRL-B.
00058                      28  PD-EXPIRE-DATE
00059                                       PIC  X(02).
00060          16  FILLER REDEFINES PD-CONTROL-PRIMARY.
00061              20  FILLER               PIC  X(01).
00062              20  PD-CGSPE-KEY         PIC  X(21).
00063          16  FILLER                   PIC  X(04).
00064      12  FILLER                       PIC  X(20).
00065
00066 ******************************************************************
00067 *      ALTERNATE PATH1 = MPPROD2 (ALT GROUPING) RKP=48,LEN=22    *
00068 ******************************************************************
00069
00070      12  PD-CONTROL-BY-VAR-GRP.
00071          16  PD-VG-CCGSP-KEYLET.
00072              20  PD-COMPANY-CD-A1     PIC  X(01).
00073              20  PD-VG-CARRIER        PIC  X(01).
00074              20  PD-VG-GROUPING       PIC  X(06).
00075              20  PD-VG-STATE          PIC  X(02).
00076              20  PD-VG-PRODUCER       PIC  X(10).
00077          16  PD-VG-DATE.
00078              24  PD-VG-EXPIRE-DATE    PIC  X(02).
00079      12  FILLER                       PIC  X(20).
00080
00081
00082 ******************************************************************
00083 *      ALTERNATE PATH2 = MPPROD3 (NAME)         RKP=90,LEN=56    *
00084 ******************************************************************
00085
00086      12  PD-CONTROL-BY-NAME.
00087          16  PD-COMPANY-CD-A2         PIC  X(01).
00088          16  PD-NAME-A2               PIC  X(30).
00089          16  PD-CGSPE-KEY-A2.
00090              20  PD-CARRIER-A2        PIC  X(01).
00091              20  PD-GROUPING-A2       PIC  X(06).
00092              20  PD-STATE-A2          PIC  X(02).
00093              20  PD-PRODUCER-A2       PIC  X(10).
00094              20  PD-EXPIRE-DATE-A2    PIC  X(02).
00095          16  PD-CURRENT-DATE-BIN-A2   PIC  X(02).
00096          16  PD-CURRENT-TIME-BIN-A2   PIC S9(04) COMP.
00097      12  FILLER                       PIC  X(20).
00098
00099 ******************************************************************
00100 *                FILE SYNCHRONIZATION DATA                       *
00101 ******************************************************************
00102
00103      12  PD-MAINT-INFORMATION.
00104          16  PD-LAST-MAINT-DATE       PIC  X(02).
00105          16  PD-LAST-MAINT-HHMMSS     PIC S9(07) COMP-3.
00106          16  PD-LAST-MAINT-USER       PIC  X(04).
00107
00108 ******************************************************************
00109 *                PRODUCER SECURITY DATA                          *
00110 ******************************************************************
00111
00112      12  PD-SECURITY-ACCESS-CODE      PIC  X(01).
00113
00114 ******************************************************************
00115 *                DATES                                           *
00116 ******************************************************************
00117
00118      12  PD-ANNIVERSARY-DATE          PIC  X(02).
00119
00120      12  PD-AR-HI-DATE.
00121          16  PD-AR-HI-POLICY-DATE     PIC  X(02).
00122          16  FILLER                   PIC  X(04).
00123      12  PD-AR-HI-POLICY-DT REDEFINES PD-AR-HI-DATE.
00124          16  PD-AR-HI-POLICY-DT-YY    PIC  9(02).
00125          16  PD-AR-HI-POLICY-DT-MM    PIC  9(02).
00126          16  PD-AR-HI-POLICY-DT-DD    PIC  9(02).
00127
00128      12  PD-ENTRY-DATE                PIC  X(02).
00129
00130      12  PD-EFFECT-DTE.
00131          16  PD-EFFECT-DATE           PIC  X(02).
00132          16  FILLER                   PIC  X(04).
00133      12  PD-EFFECT-DT REDEFINES PD-EFFECT-DTE.
00134          16  PD-EFFECT-DT-YY          PIC  9(02).
00135          16  PD-EFFECT-DT-MM          PIC  9(02).
00136          16  PD-EFFECT-DT-DD          PIC  9(02).
00137
00138      12  PD-HI-DATE.
00139          16  PD-HI-POLICY-DATE        PIC  X(02).
00140          16  FILLER                   PIC  X(04).
00141      12  PD-HI-POLICY-DT REDEFINES PD-HI-DATE.
00142          16  PD-HI-POLICY-DT-YY       PIC  9(02).
00143          16  PD-HI-POLICY-DT-MM       PIC  9(02).
00144          16  PD-HI-POLICY-DT-DD       PIC  9(02).
00145
00146      12  PD-INACTIVE-DATE             PIC  X(02).
00147
00148      12  PD-LO-DATE.
00149          16  PD-LO-POLICY-DATE        PIC  X(02).
00150          16  FILLER                   PIC  X(04).
00151      12  PD-LO-POLICY-DT REDEFINES PD-LO-DATE.
00152          16  PD-LO-POLICY-DT-YY       PIC  9(02).
00153          16  PD-LO-POLICY-DT-MM       PIC  9(02).
00154          16  PD-LO-POLICY-DT-DD       PIC  9(02).
00155
00156      12  PD-POLICIES-PURGED-DATE      PIC  X(02).
00157
00158      12  PD-PREV-DATES.
00159          16  PD-PREV-EFF-DATE         PIC  X(02).
00160          16  FILLER                   PIC  X(04).
00161          16  PD-PREV-EXP-DATE         PIC  X(02).
00162          16  FILLER                   PIC  X(04).
00163      12  PD-PREV-DTS REDEFINES PD-PREV-DATES.
00164          16  PD-PREV-EFF-DT.
00165              20  PD-PREV-EFF-DT-YY    PIC  9(02).
00166              20  PD-PREV-EFF-DT-MM    PIC  9(02).
00167              20  PD-PREV-EFF-DT-DD    PIC  9(02).
00168          16  PD-PREV-EXP-DT.
00169              20  PD-PREV-EXP-DT-YY    PIC  9(02).
00170              20  PD-PREV-EXP-DT-MM    PIC  9(02).
00171              20  PD-PREV-EXP-DT-DD    PIC  9(02).
00172
00173      12  PD-1ST-PROD-DATE             PIC  X(02).
00174
00175      12  FILLER                       PIC  X(20).
00176
00177 ******************************************************************
00178 *                MORTGAGE BILLING DATA                           *
00179 ******************************************************************
00180
00181      12  PD-CONTACT                   PIC  X(30).
00182      12  PD-BILLING-MONTHS.
00183          16  PD-BILLING-MONTH-ANNUAL  PIC  9(02).
00184          16  PD-BILLING-MONTH-SEMIANN PIC  9(02).
00185      12  PD-BILLING-ADVANCE-ARREARS   PIC  X(01).
00186          88  PD-BILL-ADVANCE              VALUE '1'.
00187          88  PD-BILL-ARREARS              VALUE '2'.
00188      12  PD-BILLING-MODE              PIC  X(01).
00189          88  PD-ANNUAL-BILL               VALUE '1'.
00190          88  PD-SEMI-ANNUAL-BILL          VALUE '2'.
00191          88  PD-QUARTERLY-BILL            VALUE '3'.
00192          88  PD-MONTHLY-BILL              VALUE '4'.
00193          88  PD-BI-MONTHLY-BILL           VALUE '5'.
00194          88  PD-SINGLE-PREM-BILL          VALUE '6'.
00195      12  PD-BILLING-GROUPING-CODE     PIC  X(06).
00196      12  PD-BILLING-SCHEDULE          PIC  X(01).
00197          88  PD-BILL-1ST-WEEK             VALUE '1'.
00198          88  PD-BILL-2ND-WEEK             VALUE '2'.
00199          88  PD-BILL-3RD-WEEK             VALUE '3'.
00200          88  PD-BILL-4TH-WEEK             VALUE '4'.
00201          88  PD-BILL-5TH-WEEK             VALUE '5'.
00202          88  PD-HOLD-BILL                 VALUE '6'.
00203          88  PD-NO-BILL                   VALUE '7'.
00204      12  PD-BILLING-SEQUENCE          PIC  X(01).
00205          88  PD-BILL-NAME-SEQU            VALUE '1'.
00206          88  PD-BILL-LOAN-SEQU            VALUE '2'.
00207          88  PD-BILL-PLCY-SEQU            VALUE '3'.
00208      12  PD-BILLING-TYPE              PIC  X(01).
00209          88  PD-LIST-BILL                 VALUE '1'.
00210          88  PD-TAPE-BILL                 VALUE '2'.
00211          88  PD-TAPE-LIST-BILL            VALUE '3'.
00212          88  PD-GROUP-BILL            VALUES ARE '1' '2' '3'.
00213          88  PD-DIRECT-BILL               VALUE '4'.
00214          88  PD-PAC                   VALUES ARE '5' 'C' 'S'.
00215          88  PD-CREDIT-CARD               VALUE '6'.
00216          88  PD-INDIV-BILL
00217                               VALUES ARE '4' '5' '6' 'C' 'S'.
00218          88  PD-GROUP-BY-POLICY           VALUE '7'.
00219          88  PD-GROUP-BY-POLICY-PAC       VALUE '8'.
00220          88  PD-GROUP-BY-POLICY-CRDC      VALUE '9'.
00221          88  PD-GROUP-BY-BILL             VALUE '7' '8' '9'.
00222          88  PD-GROUP-BY-PROD             VALUE 'A'.
00223          88  PD-EFT-CHECKING              VALUE 'C'.
00224          88  PD-EFT-SAVINGS               VALUE 'S'.
00225      12  PD-DATE-PAID                 PIC  X(02).
00226      12  PD-LAST-BILLING-DATE         PIC  X(02).
00227      12  PD-LAST-BILL-TO-DATE         PIC  X(02).
00228      12  PD-MAX-MONTHS-BILL           PIC S9(03)  COMP-3.
00229      12  PD-PAID-TO-DATE              PIC  X(02).
00230      12  PD-PREV-BILLING-DATE         PIC  X(02).
00231      12  PD-PREV-BILL-TO-DATE         PIC  X(02).
00232
00233      12  FILLER                       PIC  X(20).
00234
00235 ******************************************************************
00236 *                PERSONAL DATA                                   *
00237 ******************************************************************
00238
00239      12  PD-ADDRS                     PIC  X(30).
00240      12  PD-CITY                      PIC  X(30).
00241      12  PD-CITY-CODE                 PIC  X(04).
00242      12  PD-COUNTY-CODE               PIC  X(03).
00243      12  PD-NAME                      PIC  X(30).
00244      12  PD-PARRISH-CODE              PIC  X(03).
00245      12  PD-PERSON                    PIC  X(30).
00246      12  PD-TEL-NO.
00247          16  PD-AREA-CODE             PIC  9(03).
00248          16  PD-TEL-PRE               PIC  9(03).
00249          16  PD-TEL-NBR               PIC  9(04).
00250      12  PD-ZIP.
00251          16  PD-ZIP-PRIME             PIC  X(05).
00252          16  PD-ZIP-PLUS4             PIC  X(04).
00253      12  PD-LANGUAGE-IND              PIC  X(01).
00254          88  PD-ENGLISH                          VALUE 'E'.
00255          88  PD-FRENCH                           VALUE 'F'.
00256          88  PD-SPANISH                          VALUE 'S'.
00257
00258      12  FILLER                       PIC  X(19).
00259
00260 ******************************************************************
00261 *                REINSURANCE DATA                                *
00262 ******************************************************************
00263
00264      12  PD-REINS-TBL-CODE            PIC  X(03).
00265      12  PD-REIN-RECALC               PIC  X(01).
00266
00267      12  PD-REI-AH-FEE                PIC S9(01)V9(04) COMP-3.
00268      12  PD-REI-AH-PE                 PIC  X(01).
00269      12  PD-REI-AH-TAX                PIC S9(01)V9(04) COMP-3.
00270
00271      12  PD-REI-GROUP-A               PIC  X(06).
00272      12  PD-REI-GROUP-B               PIC  X(06).
00273
00274      12  PD-REI-LF-FEE                PIC S9(01)V9(04) COMP-3.
00275      12  PD-REI-LF-PE                 PIC  X(01).
00276      12  PD-REI-LF-TAX                PIC S9(01)V9(04) COMP-3.
00277
00278      12  PD-REI-MORT                  PIC  X(04).
00279      12  PD-REI-PRT-OW                PIC  X(01).
00280      12  PD-REI-PRT-ST                PIC  X(01).
00281
00282      12  PD-REI-ADD-FEE               PIC S9(01)V9(04) COMP-3.
00283      12  PD-REI-ADD-PE                PIC  X(01).
00284      12  PD-REI-ADD-TAX               PIC S9(01)V9(04) COMP-3.
00285
00286      12  PD-REI-DIS-FEE               PIC S9(01)V9(04) COMP-3.
00287      12  PD-REI-DIS-PE                PIC  X(01).
00288      12  PD-REI-DIS-TAX               PIC S9(01)V9(04) COMP-3.
00289
00290      12  FILLER                       PIC  X(10).
00291 ******************************************************************
00292 *                RETRO DATA                                      *
00293 ******************************************************************
00294
00295      12  PD-RET-AH                    PIC S9(01)V9(04) COMP-3.
00296      12  PD-RET-GRP                   PIC  X(06).
00297      12  PD-RET-LF                    PIC S9(01)V9(04) COMP-3.
00298      12  PD-RET-MIN-LOSS-A            PIC SV9(03)      COMP-3.
00299      12  PD-RET-MIN-LOSS-L            PIC SV9(03)      COMP-3.
00300      12  PD-RET-P-E                   PIC  X(01).
00301      12  PD-RET-ST-TAX-USE            PIC  X(01).
00302          88  PD-CHARGE-ST-TAXES-ON-RETRO      VALUE 'Y' 'E' 'P'.
00303          88  PD-TAXES-NOT-IN-RETRO            VALUE 'N' ' '.
00304      12  PD-RET-Y-N                   PIC  X(01).
00305      12  PD-RET-ADD                   PIC S9(01)V9(04) COMP-3.
00306      12  PD-RET-MIN-LOSS-ADD          PIC SV9(03)      COMP-3.
00307      12  PD-RET-DIS                   PIC S9(01)V9(04) COMP-3.
00308      12  PD-RET-MIN-LOSS-DIS          PIC SV9(03)      COMP-3.
00309
00310      12  FILLER                       PIC  X(10).
00311
00312 ******************************************************************
00313 *                     MANAGEMENT OPTIONS                         *
00314 ******************************************************************
00315
00316      12  PD-DEFAULT-UNWTR-CODE        PIC  X(03).
00317      12  PD-LAPSE-NOTICE-CNTL         PIC  X(01).
00318      12  PD-CORRESPONDENCE-CNTL       PIC  X(01).
00319      12  PD-RETAIN-BILLING-DATA-MTHS  PIC S9(03)  COMP-3.
00320      12  PD-RETAIN-CLAIM-DATA-MTHS    PIC S9(03)  COMP-3.
00321      12  PD-RETAIN-COMMISSION-MTHS    PIC S9(03)  COMP-3.
00322      12  PD-RETAIN-DELINQUENCY-MTHS   PIC S9(03)  COMP-3.
00323      12  PD-RETAIN-INSD-PROFILE-MTHS  PIC S9(03)  COMP-3.
00324      12  PD-RETAIN-INS-COVERAGE-MTHS  PIC S9(03)  COMP-3.
00325      12  PD-RETAIN-STATUS-DISP-MTHS   PIC S9(03)  COMP-3.
00326      12  PD-NUM-BILLING-CYCLES-RETAINED
00327                                       PIC S9(03)  COMP-3.
00328      12  PD-RETAIN-UNDERWRITER-HST-MTHS
00329                                       PIC S9(03)  COMP-3.
00330
00331      12  FILLER                       PIC X(098).
00332
00333
00334 ******************************************************************
00335 *                MISCELLANEOUS DATA                              *
00336 ******************************************************************
00337
00338      12  PD-AH-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00339      12  PD-AUTO-REFUND-SW            PIC  X(01).
00340          88  PD-AUTO-REFUNDS-USED             VALUE 'Y'.
00341          88  PD-AUTO-REFUNDS-NOT-USED         VALUE 'N' ' '.
00342      12  PD-BUSINESS-TYPE             PIC  9(02).
00343      12  PD-CAL-TABLE                 PIC  X(02).
00344      12  PD-COMMENTS.
00345          16  PD-COMMENT-LINE          PIC  X(50)
00346                                            OCCURS 5 TIMES.
00347      12  PD-EMPLOYER-STMT-USED        PIC  X(01).
00348      12  PD-GROUPED-CHECKS-Y-N        PIC  X(01).
00349      12  PD-IG                        PIC  X(01).
00350          88  PD-HAS-INDIVIDUAL                VALUE 'I'
00351                                                     '1'.
00352          88  PD-HAS-GROUP                     VALUE 'G'
00353                                                     '2'.
00354      12  PD-LF-RPT021-EXP-PCT         PIC S9(03)V9(04) COMP-3.
00355      12  PD-REPORT-CODE-1             PIC  X(10).
00356      12  PD-REPORT-CODE-2             PIC  X(10).
00357      12  PD-RPT045A-SWITCH            PIC  X(01).
00358          88  PD-RPT045A-OFF                VALUE 'N'.
00359      12  PD-SPECIAL-BILLING-FREQ      PIC  X(01).
00360          88  PD-HAS-SPECIAL-BILL-FREQ         VALUE 'Y'.
00361          88  PD-NO-SPECIAL-BILL-FREQ          VALUE 'N' ' '.
00362      12  PD-STATUS                    PIC  X(01).
00363          88  PD-STATUS-ACTIVE                 VALUE '0'.
00364          88  PD-STATUS-INACTIVE               VALUE '1'.
00365      12  PD-STD-AH-TYPE               PIC  X(02).
00366      12  PD-TAX-NUMBER                PIC  X(11).
00367      12  PD-TOL-CLM                   PIC S9(03)V9(02) COMP-3.
00368      12  PD-USER-FIELDS.
00369          16  PD-USER-FLD-1            PIC  X(02).
00370          16  PD-USER-FLD-2            PIC  X(02).
00371          16  PD-USER-FLD-3            PIC  X(02).
00372          16  PD-USER-FLD-4            PIC  X(02).
00373          16  PD-USER-FLD-5            PIC  X(02).
00374      12  PD-USER-SELECT-OPTIONS.
00375          16  PD-USER-SELECT-1         PIC  X(10).
00376          16  PD-USER-SELECT-2         PIC  X(10).
00377          16  PD-USER-SELECT-3         PIC  X(10).
00378          16  PD-USER-SELECT-4         PIC  X(10).
00379          16  PD-USER-SELECT-5         PIC  X(10).
00380      12  PD-DIS-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00381      12  PD-ADD-RPT021-EXP-PCT        PIC S9(03)V9(04) COMP-3.
00382      12  FILLER                       PIC  X(20).
00383
00384 ******************************************************************
00385 *                CLIENT USE AREAS                                *
00386 ******************************************************************
00387
00388      12  PD-CLIENT-USE-AREA-1         PIC  X(30).
00389      12  PD-CLIENT-USE-AREA-2         PIC  X(30).
00390      12  PD-CLIENT-USE-AREA-3         PIC  X(11).
00391      12  PD-CLIENT-USE-AREA-4         PIC  X(30).
00392      12  PD-CLIENT-USE-AREA-5         PIC  X(30).
00393      12  PD-CLIENT-USE-AREA-6         PIC  X(11).
00394      12  PD-CLIENT-USE-AREA-7         PIC  X(30).
00395      12  PD-CLIENT-USE-AREA-8         PIC  X(30).
00396      12  PD-CLIENT-USE-AREA-9         PIC  X(11).
00397
00398 ******************************************************************
00399 *                TRANSFER DATA                                   *
00400 ******************************************************************
00401      12  PD-TRANSFERRED-FROM.
00402          16  PD-TRNFROM-CARRIER       PIC  X(01).
00403          16  PD-TRNFROM-GROUPING.
00404              20  PD-TRNFROM-GRP-PREFIX
00405                                       PIC  X(03).
00406              20  PD-TRNFROM-GRP-PRIME PIC  X(03).
00407          16  PD-TRNFROM-STATE         PIC  X(02).
00408          16  PD-TRNFROM-PRODUCER.
00409              20  PD-TRNFROM-PROD-PREFIX
00410                                       PIC  X(04).
00411              20  PD-TRNFROM-PROD-PRIME
00412                                       PIC  X(06).
00413          16  PD-TRNFROM-DATE          PIC  X(02).
00414      12  PD-TRANSFERRED-TO.
00415          16  PD-TRNTO-CARRIER         PIC  X(01).
00416          16  PD-TRNTO-GROUPING.
00417              20  PD-TRNTO-GRP-PREFIX  PIC  X(03).
00418              20  PD-TRNTO-GRP-PRIME   PIC  X(03).
00419          16  PD-TRNTO-STATE           PIC  X(02).
00420          16  PD-TRNTO-PRODUCER.
00421              20  PD-TRNTO-PROD-PREFIX PIC  X(04).
00422              20  PD-TRNTO-PROD-PRIME  PIC  X(06).
00423          16  PD-TRNTO-DATE            PIC  X(02).
00424      12  FILLER                       PIC  X(20).
00425
00426 ******************************************************************
00427 *                MORTGAGE PLANS SOLD                             *
00428 ******************************************************************
00429
00430      12  PD-PLANS-SOLD.
00431          16  PD-PRODUCER-PLANS  OCCURS 40 TIMES
00432                                 INDEXED BY PD-PLAN-NDX
00433                                            PD-PLAN-NDX2.
00434              20  PD-INDIVIDUAL-PLAN.
00435                  24  PD-PLAN-CODE     PIC  X(02).
00436                  24  PD-PLAN-REVISION PIC  X(03).
00437              20  PD-IBNR-PERCENT      PIC S9(01)V9(04) COMP-3.
00438      12  FILLER                       PIC  X(54).
00439
00440 ******************************************************************
00441 *                 AGENT AND COMMISSION DATA                      *
00442 ******************************************************************
00443
00444      12  PD-COMMISSION-INFORMATION.
00445          16  PD-REMIT-TO              PIC S9(03)   COMP-3.
00446          16  PD-RECALCULATION-SW      PIC  X(01).
00447              88  PD-RECALC-DETAIL             VALUE 'Y'.
00448              88  PD-RECALC-NO-DETAIL          VALUE 'I'.
00449              88  PD-IGNORE-RECALC             VALUE 'N'.
00450              88  PD-VALID-RECALCULATION-SW    VALUE 'Y' 'I' 'N'.
00451          16  PD-AGENT-DATA.
00452              20  PD-AGENT-ENTRY       OCCURS 5 TIMES
00453                                     INDEXED BY PD-AGENT-NDX
00454                                                PD-AGENT-NDX2.
00455                  24  PD-AGENT-NUMBER  PIC  X(10).
00456                  24  PD-AGENT-TYPE    PIC  X(01).
00457                      88  PD-AGENT-TYPE-A      VALUE 'C' 'D'.
00458                      88  PD-AGENT-TYPE-G      VALUE 'O' 'R'
00459                                                     'P' 'T'
00460                                                     'W'.
00461                      88  PD-AGENT-GROSS       VALUE 'C'.
00462                      88  PD-AGENT-REINS       VALUE 'R'.
00463                      88  PD-AGENT-GROSS-REINS VALUE 'D'.
00464                      88  PD-OVERWRITE-GROSS   VALUE 'O'.
00465                      88  PD-OVERWRITE-GROSS-REINS
00466                                           VALUE 'P'.
00467                      88  PD-OVERWRITE-REINS   VALUE 'T'.
00468                      88  PD-REINS-ONLY        VALUE 'W'.
00469                      88  PD-VALID-AGENT-TYPE  VALUE 'C' 'R'
00470                                                 'D' 'O' 'P'
00471                                                 'T' 'W'.
00472                  24  PD-COMMISSION-BILLED-PAID
00473                                       PIC  X(01).
00474                      88  PD-AGENT-BILLED      VALUE 'B'.
00475                      88  PD-AGENT-PAID        VALUE 'P'.
00476                  24  PD-COMP-RECALC-FLAG
00477                                       PIC  X(01).
00478                      88  PD-BYPASS-RECALC     VALUE 'N'.
00479                      88  PD-VALID-RECALC-FLAG VALUE ' ' 'N'.
00480      12  FILLER                       PIC  X(55).
00481
00482 ******************************************************************
00483 *                BANK DATA                                       *
00484 ******************************************************************
00485
00486      12  PD-BANK-ACCOUNT-NUMBER       PIC  X(20).
00487      12  PD-BANK-TRANSIT-NUMBER.
00488          16  PD-FEDERAL-NUMBER        PIC  X(04).
00489          16  PD-BANK-NUMBER           PIC  X(04).
00490      12  PD-CHARGE-CARD-EXP-DT        PIC  X(02).
00491      12  PD-CHARGE-CARD-TYPE          PIC  X(02).
00492          88  PD-AMERICAN-EXPRESS                 VALUE 'AE'.
00493          88  PD-CARTE-BLANCHE                    VALUE 'CB'.
00494          88  PD-DINERS-CLUB                      VALUE 'DN'.
00495          88  PD-DISCOVER                         VALUE 'DS'.
00496          88  PD-MASTER-CARD                      VALUE 'MC'.
00497          88  PD-VISA                             VALUE 'VI'.
00498      12  PD-SIGNATURE-NAME            PIC  X(25).
00499      12  PD-AUTHORIZATION-SW          PIC  X(01).
00500 ******************************************************************
00501 *                GENERIC FILLER                                  *
00502 ******************************************************************
00503
00504      12  PD-DATE-TEST                 PIC S9(08) COMP.
00505      12  FILLER                       PIC  X(62).
00506
00507 ******************************************************************
00960                                  EJECT
00961 *                                COPY MPCPLCY.
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
00962                                  EJECT
00963 *                                COPY MPCPLAN.
00001 ******************************************************************
00002 *                                                                *
00003 *                            MPCPLAN                             *
00004 *                            VMOD=1.012                          *
00005 *                                                                *
00006 *   MORTGAGE SYSTEM PRODUCER PLAN MASTER FILE.                   *
00007 *                                                                *
00008 *   THIS COPYBOOK IS USED FOR THE ONLINE                         *
00009 *   PLAN CODE MASTER FILE.                                       *
00010 *                                                                *
00011 *   FILE DESCRIPTION = PRODUCER PLAN MASTER                      *
00012 *                                                                *
00013 *   FILE TYPE = VSAM,KSDS                                        *
00014 *   RECORD SIZE = 450  RECFORM = FIX                             *
00015 *                                                                *
00016 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00017 *       ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25   *
00018 *                                                                *
00019 *   LOG = NO                                                     *
00020 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00021 *                                                                *
00022 *                                                                *
00023 ******************************************************************
00024
00025  01  PRODUCER-PLANS.
00026      12  PP-RECORD-ID                      PIC  X(02).
00027          88  VALID-PP-ID                      VALUE 'PP'.
00028
00029 ******************************************************************
00030 *   BASE CLUSTER NAME = MPPLAN                    RKP=2,LEN=25   *
00031 ******************************************************************
00032
00033      12  PP-CONTROL-PRIMARY.
00034          16  PP-PROD-PRIMARY.
00035              20  PP-COMPANY-CD             PIC  X(01).
00036              20  PP-CONTROL-A.
00037                  24  PP-CARRIER            PIC  X(01).
00038                  24  PP-GROUPING.
00039                      28  PP-GROUPING-PREFIX
00040                                            PIC  X(03).
00041                      28  PP-GROUPING-PRIME PIC  X(03).
00042                  24  PP-STATE              PIC  X(02).
00043                  24  PP-PRODUCER.
00044                      28  PP-PRODUCER-PREFIX
00045                                            PIC  X(04).
00046                      28  PP-PRODUCER-PRIME PIC  X(06).
00047          16  PP-PRODUCER-PLAN.
00048              20  PP-PLAN-CODE              PIC  X(02).
00049              20  PP-PLAN-REVISION          PIC  9(03).
00050      12  FILLER                            PIC  X(20).
00051
00052 ******************************************************************
00053 *      ALTERNATE PATH1 = MPPLAN2 (ALT GROUPING) RKP=47,LEN=25    *
00054 ******************************************************************
00055
00056      12  PP-CONTROL-BY-VAR-GRP.
00057          16  PP-COMPANY-CD-A1              PIC  X(01).
00058          16  PP-VG-CARRIER                 PIC  X(01).
00059          16  PP-VG-GROUPING                PIC  X(06).
00060          16  PP-VG-STATE                   PIC  X(02).
00061          16  PP-VG-PRODUCER                PIC  X(10).
00062          16  PP-VG-PLAN-CODE               PIC  X(02).
00063          16  PP-VG-PLAN-REVISION           PIC  X(03).
00064      12  FILLER                            PIC  X(20).
00065
00066 ******************************************************************
00067 *                PRODUCER SECURITY DATA                          *
00068 ******************************************************************
00069
00070      12  PP-SECURITY-ACCESS-CODE           PIC  X(01).
00071      12  PP-POLICY-CNT                     PIC S9(07)    COMP-3.
00072
00073 ******************************************************************
00074 *                FILE SYNCHRONIZATION DATA                       *
00075 ******************************************************************
00076
00077      12  PP-MAINT-INFORMATION.
00078          16  PP-LAST-MAINT-DATE            PIC  X(02).
00079          16  PP-LAST-MAINT-HHMMSS          PIC S9(07)    COMP-3.
00080          16  PP-LAST-MAINT-USER            PIC  X(04).
00081      12  FILLER                            PIC  X(10).
00082
00083 ******************************************************************
00084 *                   CRITICAL FILE DATES                          *
00085 ******************************************************************
00086
00087      12  PP-PLAN-DATES.
00088          16  PP-PLAN-EFFECT-DATE           PIC  X(02).
00089          16  PP-PLAN-EXPIRE-DATE           PIC  X(02).
00090
00091      12  FILLER                            PIC  X(10).
00092
00093 ******************************************************************
00094 *                GENERAL INFORMATION                             *
00095 ******************************************************************
00096
00097      12  PP-GENERAL-INFORMATION.
00098          16  PP-ALPHA-SEARCH-SW            PIC  X(01).
00099              88  PP-MIB-ALPHA-ALL              VALUE '1'.
00100              88  PP-MIB-ALPHA-NONE             VALUE '2'.
00101              88  PP-MIB-ALPHA-EXCEEDED         VALUE '3'.
00102              88  PP-CLIENT-ALPHA-ALL           VALUE 'A'.
00103              88  PP-CLIENT-ALPHA-NONE          VALUE 'B'.
00104              88  PP-CLIENT-ALPHA-EXCEEDED      VALUE 'C'.
00105              88  PP-BOTH-ALPHA-ALL             VALUE 'X'.
00106              88  PP-BOTH-ALPHA-NONE            VALUE 'Y'.
00107              88  PP-BOTH-ALPHA-EXCEEDED        VALUE 'Z'.
00108              88  PP-ALPHA-SEARCH-VALID    VALUES ARE '1' '2' '3'
00109                                                      'A' 'B' 'C'
00110                                                      'X' 'Y' 'Z'.
00111          16  PP-BENEFIT-TYPE               PIC  X(01).
00112              88  PP-BENEFIT-IS-LEVEL            VALUE '1'.
00113              88  PP-BENEFIT-REDUCES             VALUE '2'.
00114          16  PP-DAYS-TO-1ST-NOTICE         PIC  9(02).
00115          16  PP-DAYS-TO-2ND-NOTICE         PIC  9(02).
00116          16  PP-DAYS-TO-3RD-NOTICE         PIC  9(02).
00117          16  PP-DAYS-TO-4TH-NOTICE         PIC  9(02).
00118          16  PP-EFF-DT-RULE-SW             PIC  X(01).
00119              88  PP-EFF-DT-ENTER               VALUE 'E'.
00120              88  PP-EFF-DT-MONTH               VALUE 'M'.
00121              88  PP-EFF-DT-QTR                 VALUE 'Q'.
00122              88  PP-EFF-DT-SEMI                VALUE 'S'.
00123              88  PP-EFF-DT-ANN                 VALUE 'A'.
00124          16  PP-FREE-EXAM-DAYS             PIC S9(03)   COMP-3.
00125          16  PP-GRACE-PERIOD               PIC S9(03)   COMP-3.
00126          16  PP-HEALTH-QUESTIONS           PIC  9(01).
00127          16  PP-NUMBER-LAPSE-NOTICES       PIC S9(03)   COMP-3.
00128          16  PP-MIB-SEARCH-SW              PIC  X(01).
00129              88  PP-MIB-SEARCH-ALL             VALUE '1'.
00130              88  PP-MIB-SEARCH-NONE            VALUE '2'.
00131              88  PP-MIB-SEARCH-EXCEEDED        VALUE '3'.
00132              88  PP-MIB-SEARCH-VALID      VALUES ARE '1' '2' '3'.
00133          16  PP-PLAN-ABBREV                PIC  X(03).
00134          16  PP-PLAN-AGES.
00135              20  PP-MINIMUM-AGE            PIC S9(03)   COMP-3.
00136              20  PP-MAXIMUM-AGE            PIC S9(03)   COMP-3.
00137              20  PP-MAXIMUM-ATTAIN-AGE     PIC S9(03)   COMP-3.
00138          16  PP-PLAN-BENEFITS.
00139              20  PP-CLAIM-CAP              PIC S9(07)V99 COMP-3.
00140              20  PP-MINIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
00141              20  PP-MAXIMUM-BENEFIT        PIC S9(07)V99 COMP-3.
00142              20  PP-MAXIMUM-MONTHLY-BENEFIT
00143                                            PIC S9(07)V99 COMP-3.
00144          16  PP-PLAN-DESCRIPTION           PIC  X(10).
00145          16  PP-POLICY-FEE                 PIC S9(03)V9(02)
00146                                                         COMP-3.
00147          16  PP-PLAN-IND-GRP               PIC  X(01).
00148          16  PP-PLAN-SNGL-JNT              PIC  X(01).
00149              88  PP-COMBINED-PLAN             VALUE 'C'.
00150              88  PP-JNT-PLAN                  VALUE 'J'.
00151              88  PP-SNGL-PLAN                 VALUE 'S'.
00152          16  PP-PLAN-TERMS.
00153              20  PP-MINIMUM-TERM           PIC S9(03)   COMP-3.
00154              20  PP-MAXIMUM-TERM           PIC S9(03)   COMP-3.
00155          16  PP-PLAN-TYPE                  PIC  X(01).
00156              88  PP-AH-MORT-PLAN              VALUE 'A'.
00157              88  PP-AD-D-MORT-PLAN            VALUE 'E'.
00158              88  PP-DISMEM-MORT-PLAN          VALUE 'D'.
00159              88  PP-LIFE-MORT-PLAN            VALUE 'L'.
00160          16  PP-PREMIUM-TOLERANCES.
00161              20  PP-PREM-TOLERANCE         PIC S9(03)   COMP-3.
00162              20  PP-PREM-TOLERANCE-PCT     PIC SV9(03)  COMP-3.
00163          16  PP-RATE-CODE                  PIC  X(05).
00164          16  PP-REOCCURRING-DISABILITY-PRD PIC S9(03)   COMP-3.
00165          16  PP-REPLACEMENT-LAW-SW         PIC  X(01).
00166              88  PP-NO-REPLACE                VALUE '1'.
00167              88  PP-REPLACE-APPLIES           VALUE '2'.
00168              88  PP-VALID-REPLACEMENT-LAW     VALUE '1' '2'.
00169          16  PP-RETRO-RETENTION            PIC S9V9(04) COMP-3.
00170          16  PP-RERATE-CNTL                PIC  X(01).
00171              88  PP-RERATE-WITH-ISSUE-AGE       VALUE '1'.
00172              88  PP-RERATE-WITH-CURRENT-AGE     VALUE '2'.
00173              88  PP-DO-NOT-RERATE               VALUE '3' ' '.
00174              88  PP-AUTO-RECALC                 VALUE '4'.
00175          16  PP-SEX-RATING                 PIC  X(01).
00176              88  PP-NOT-SEX-RATED             VALUE '1'.
00177              88  PP-SEX-RATED                 VALUE '2'.
00178          16  PP-SUBSTANDARD-DATA.
00179              20  PP-SUBSTANDARD-PERCENT    PIC S9(01)V9(04).
00180              20  PP-SUBSTANDARD-TYPE       PIC  X(01).
00181                  88  PP-PCT-OF-BENEFIT        VALUE '1'.
00182                  88  PP-PCT-OF-PREMIUM        VALUE '2'.
00183                  88  PP-NOT-APPLICABLE        VALUE '3'.
00184          16  PP-YEARS-TO-NEXT-RERATE       PIC  9(02).
00185          16  PP-DEPENDANT-COVERAGE         PIC  X(01).
00186              88  PP-DEP-COVERED               VALUE 'Y'.
00187              88  PP-DEP-NOT-COVERED           VALUE 'N' ' '.
00188          16  PP-REFUND-CALC                PIC  X(01).
00189              88  PP-RFND-MP-REFUND     VALUES ARE ' ' LOW-VALUES.
00190              88  PP-RFND-BY-R78               VALUE '1'.
00191              88  PP-RFND-BY-PRO-RATA          VALUE '2'.
00192              88  PP-RFND-AS-CALIF             VALUE '3'.
00193              88  PP-RFND-AS-TEXAS             VALUE '4'.
00194              88  PP-RFND-IS-NET-PAY           VALUE '5'.
00195              88  PP-RFND-ANTICIPATION         VALUE '6'.
00196              88  PP-RFND-MEAN                 VALUE '8'.
00197              88  PP-VALID-REFUND       VALUES ARE ' ' '1' '2' '3'
00198                                                   '4' '5' '6' '8'
00199                                                   LOW-VALUES.
00200          16  PP-ALT-RATE-CODE              PIC  X(05).
00201
00202      12  FILLER                            PIC  X(39).
00203
00204 ******************************************************************
00205 *                     PLAN FORMS AND LETTERS                     *
00206 ******************************************************************
00207
00208      12  PP-PLAN-MASTER-FORMS.
00209          16  PP-POLICY-FORM                PIC  X(12).
00210          16  PP-MASTER-APPLICATION         PIC  X(12).
00211          16  PP-MASTER-POLICY              PIC  X(12).
00212      12  PP-DELINQUENCY-NOTICE-FORMS.
00213          16  PP-1ST-NOTICE-FORM            PIC  X(04).
00214          16  PP-2ND-NOTICE-FORM            PIC  X(04).
00215          16  PP-3RD-NOTICE-FORM            PIC  X(04).
00216          16  PP-4TH-NOTICE-FORM            PIC  X(04).
00217      12  FILLER                            PIC  X(32).
00218      12  PP-TERMINATION-FORM               PIC  X(04).
00219      12  FILLER                            PIC  X(08).
00220      12  PP-ISSUE-LETTER                   PIC  X(04).
00221
00222      12  FILLER                            PIC  X(80).
00223 ******************************************************************
00964                                  EJECT
033110*                                COPY ELCNAPS.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCNAPS                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = NAPERSOFT LETTER FILE                *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 150    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ELNAPS        RKP=2,LEN=28               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
033110******************************************************************
033110*                   C H A N G E   L O G
033110*
033110* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
033110*-----------------------------------------------------------------
033110*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
033110* EFFECTIVE    NUMBER
033110*-----------------------------------------------------------------
033110* 033110  CR2009122800001  AJRA  NEW FILE FOR NAPERSOFT.
00017 ******************************************************************
00018
00019  01  NAPERSOFT-FILE.
00020      12  NA-RECORD-ID                PIC  XX.
00021          88  VALID-NA-ID                  VALUE 'NA'.
00022
00023      12  NA-CONTROL-PRIMARY.
00024          16  NA-COMPANY-CD           PIC X.
00025          16  NA-CARRIER              PIC X.
00026          16  NA-CLAIM-NO             PIC X(7).
00027          16  NA-CERT-NO.
00028              20  NA-CERT-PRIME       PIC X(10).
00029              20  NA-CERT-SFX         PIC X.
00030          16  NA-ARCHIVE-NO           PIC 9(8).
00031
00032      12  NA-LETTER-INFORMATION.
00033          16  NA-LETTER-ID            PIC X(4).
00034          16  NA-PROCESSOR-ID         PIC X(4).
00035          16  NA-CREATION-DT          PIC X(2).
00040          16  NA-INITIAL-PRINT-DT     PIC X(2).
00041          16  NA-FOLLOW-UP-DT         PIC X(2).
00042          16  NA-RESEND-DT            PIC X(2).
00043          16  NA-RESEND-LETTER-ID     PIC X(4).
00044          16  NA-NO-OF-COPIES         PIC 9(2).
00045          16  NA-ADDRESS-TYPE         PIC X(2).
               16  NA-CORR-TRLR-SEQ        PIC 9(4).
               16  NA-RESEND-PRINT-DT      PIC X(2).
               16  NA-1ST-LTR-PRINT-DT     PIC X(2).
               16  NA-NEXT-DUE-DT          PIC X(2).
               16  NA-AUTOPYDT             PIC X(2).
               16  NA-ENCLOSURE-CD         PIC X(3).
               16  NA-CREATED-IN-NAPERSOFT PIC X(1).
               16  NA-ORIG-ARCHIVE-NO      PIC 9(9).
               16  NA-RESEND-PROMPT-IND    PIC X(1).
               16  NA-ORIG-ENCLOSURE-CD    PIC X(3).
00047          16  FILLER                  PIC X(67).
00048 ******************************************************************
033110                                 EJECT
00965 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
00966 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA
00967
00968  01  SYSTEM-VARIABLES            PIC  X(3340).
00969  01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.
00970      12  SYS-VAR-CODE            PIC  X(04).
00971      12  SYS-VAR-LEN             PIC  9(02).
00972      12  SYS-VAR-DATA            PIC  X(100).
00973
00974                                  EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA ACCOUNT-MASTER
                                ACTIVITY-TRAILERS LETTER-ARCHIVE
                                BENEFICIARY-MASTER CERTIFICATE-MASTER
                                CLAIM-MASTER CONTROL-FILE
                                COMPENSATION-MASTER TEXT-FILES
                                PRODUCER-MASTER POLICY-MASTER
                                PRODUCER-PLANS NAPERSOFT-FILE
                                SYSTEM-VARIABLES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL1523' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00976
00977 *    SERVICE RELOAD PARMLIST.
00978
00979      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.
00980
00981      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
00982      MOVE '5'                    TO DC-OPTION-CODE.
00983      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
00984      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
00985      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.
00986
00987      MOVE W-SAVE-BIN-DATE        TO W-CURRENT-SAVE.
00988
00989      
      * EXEC CICS HANDLE CONDITION
00990 *        PGMIDERR (9600-PGMID-ERROR)
00991 *        ERROR    (9990-ABEND)
00992 *    END-EXEC.
      *    MOVE '"$L.                  ! " #00006704' TO DFHEIV0
           MOVE X'22244C2E2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303036373034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00993
00994
00995      MOVE ZEROS                  TO PI-ERROR-CODE
00996                                     W-PI-ADDR-SEQ
00997                                     W-ADDR-SEQ
00998                                     W-CURRENT-LINE
00999                                     W-TOTAL-LINES.
01000
01001      
      * EXEC CICS SYNCPOINT
01002 *    END-EXEC.
      *    MOVE '6"                    !   #00006716' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01003                                  EJECT
01004  0100-PROCESS-REQUEST.
01005
01006      PERFORM 1000-EDIT-ROUTINE THRU 1000-EXIT.
01007
01008      IF  PI-FATAL-ERROR
01009          GO TO 0200-RETURN-TO-CALLING-PGM.
01010
01011      PERFORM 2000-CREATE-LETTER THRU 2999-EXIT.
01012
01013      IF  PI-FATAL-ERROR
01014          GO TO 0200-RETURN-TO-CALLING-PGM.
01015
01016      PERFORM 6000-ARCHIVE-LETTER THRU 6000-EXIT.
01017
01018  0100-EXIT.
01019      EXIT.
01020                                  EJECT
01021  0200-RETURN-TO-CALLING-PGM.
01022
01023      IF  PI-FATAL-ERROR
01024          
      * EXEC CICS SYNCPOINT ROLLBACK
01025 *        END-EXEC.
      *    MOVE '6"R                   !   #00006739' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01026
01027      MOVE PROGRAM-INTERFACE-BLOCK
01028                                  TO DFHCOMMAREA.
01029
01030      
      * EXEC CICS RETURN
01031 *    END-EXEC.
      *    MOVE '.(                    ''   #00006745' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036373435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01032
01033  0200-EXIT.
01034      EXIT.
01035                                  EJECT
01036  1000-EDIT-ROUTINE.
01037
01038      PERFORM 1100-SET-CODES THRU 1100-EXIT.
01039
01040      IF  PI-RESEND-DATE EQUAL SPACES
01041          MOVE LOW-VALUES         TO PI-RESEND-DATE.
01042
01043      IF  PI-FOLLOW-UP-DATE   EQUAL SPACES
01044          MOVE LOW-VALUES         TO PI-FOLLOW-UP-DATE.
01045
01046      IF  PI-ADDR-TYPE GREATER THAN LOW-VALUES
01047          MOVE PI-ADDR-TYPE       TO W-ADDR-TYPE-CD
01048
01049          IF  PI-ADDR-TYPE EQUAL 'I' OR 'B' OR 'A' OR 'P'
01050                                OR 'O' OR 'Q' OR 'E'
01051              NEXT SENTENCE
01052
01053          ELSE
01054              MOVE ER-0176        TO PI-ERROR-CODE
01055              PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01056
01057  1000-EXIT.
01058       EXIT.
01059                                  EJECT
01060  1100-SET-CODES.
01061
01062      MOVE PI-COMPANY-ID          TO W-CNTL-CO.
01063
01064      MOVE PI-COMPANY-CD          TO W-CLM-CO
01065                                     W-TEXT-CO
01066                                     W-ACTV-CO
01067                                     W-CERT-CO
01068                                     W-BENE-COMP-CD
01069                                     W-COMP-COMPANY-CD
01070                                     W-ACCT-CO
01071                                     W-ARCH-CO
01072                                     W-PROD-CO
01073                                     W-PLCY-CO
01074                                     W-PLAN-CO.
01075
01076      MOVE PI-CARRIER             TO W-CLM-CARRIER
01077                                     W-ACTV-CARRIER
01078                                     W-CERT-CARRIER
01079                                     W-ACCT-CARRIER
01080                                     W-PROD-CARRIER
01081                                     W-PLCY-CARRIER
01082                                     W-PLAN-CARRIER.
01083
01084      MOVE PI-CERT-NO             TO W-CLM-CERT-NUM
01085                                     W-ACTV-CERT-NUM
01086                                     W-CERT-CERT-NUM.
01087
01088      MOVE PI-CLAIM-NO            TO W-CLM-CLAIM
01089                                     W-ACTV-CLAIM.
01090
01091      MOVE W-ACTV-KEY             TO W-ACTV-SAVE-KEY.
01092
01093  1100-EXIT.
01094       EXIT.
01095                                  EJECT
01096  2000-CREATE-LETTER.
01097 ***************************************************************
01098 *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
01099 *    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
01100 *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
01101 *    WILL BE DISPLAYED ONTO THE SCREEN.                       *
01102 *                                                             *
01103 ***************************************************************
01104
01105      MOVE SPACES                 TO W-RECORD-TABLE.
01106
01107      PERFORM 3000-READ-ADDR THRU 3999-EXIT.
01108
01109      IF  PI-FATAL-ERROR
01110          GO TO 2999-EXIT.
01111
01112      SET W-TB-NDX                TO 7.
01113      MOVE W-TOP-FORM             TO W-REC-TEXT (W-TB-NDX).
01114      SET W-TB-NDX UP BY 1.
01115
01116      MOVE PI-FORM-NUMBER         TO W-TEXT-LETTER.
01117      MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.
01118
01119      
      * EXEC CICS HANDLE CONDITION
01120 *         NOTFND     (2120-ENDBR)
01121 *         ENDFILE    (2120-ENDBR)
01122 *         NOTOPEN    (2900-TEXT-NOT-OPEN)
01123 *    END-EXEC
      *    MOVE '"$I''J                 ! # #00006834' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303036383334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01124
01125      
      * EXEC CICS STARTBR
01126 *         DATASET    (W-TEXT-ID)
01127 *         RIDFLD     (W-TEXT-KEY)
01128 *         GTEQ
01129 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00006840' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-ID, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01130
01131      MOVE 'Y'                    TO W-TEXT-BROWSE-STARTED.
01132
01133  2110-READ-NEXT.
01134
01135      IF  W-TB-NDX GREATER THAN W-MAX-LINES
01136          MOVE ER-0051            TO W-CURRENT-ERROR
01137          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT
01138          GO TO 2120-ENDBR.
01139
01140      
      * EXEC CICS READNEXT
01141 *         DATASET    (W-TEXT-ID)
01142 *         SET        (ADDRESS OF TEXT-FILES)
01143 *         RIDFLD     (W-TEXT-KEY)
01144 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00006855' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303036383535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01145
01146 *    SERVICE RELOAD TEXT-FILES.
01147
01148      IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY
01149          GO TO 2120-ENDBR.
01150
01151      IF  TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
01152          PERFORM 2800-PROCESS-Z-CONTROLS THRU 2800-EXIT
01153
01154          IF  PI-FATAL-ERROR
01155              GO TO 2999-EXIT
01156
01157          ELSE
01158              GO TO 2110-READ-NEXT.
01159
031307      IF (PI-COMPANY-ID = 'CID')
031307         AND (PI-CARRIER NOT = '8')
031307         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
031307            B1 > +20
031307            IF TX-TEXT-LINE (B1:5) = '@03.1'
031307               GO TO 2110-READ-NEXT
031307            END-IF
031307         END-PERFORM
031307      END-IF
090108      IF PI-AUTO-LETTER-DATE > SPACES
090108         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
090108           B1 > 61
090108           IF TX-TEXT-LINE (B1:10) = '@@AUTOPYDT' or '@@autopydt'
090108               MOVE PI-AUTO-LETTER-DATE TO TX-TEXT-LINE(B1:10)
090108           END-IF
090108         END-PERFORM
090108      END-IF.
01160      MOVE TX-TEXT-LINE           TO W-REC-TEXT (W-TB-NDX).
01161      MOVE TX-PROCESS-CONTROL     TO W-REC-PC (W-TB-NDX)
01162                                     W-NDX-WORK.
01163      SET W-TB-NDX UP BY 1.
01164
01165      IF  W-NDX-WORK = 99
01166          MOVE W-TOP-FORM         TO W-REC-TEXT (W-TB-NDX)
01167          SET W-TB-NDX UP BY 1
01168          GO TO 2110-READ-NEXT
01169
01170      ELSE
01171          SET W-TB-NDX UP BY W-NDX-WORK
01172          GO TO 2110-READ-NEXT.
01173
01174  2120-ENDBR.
01175
01176      IF  W-TEXT-BROWSE-STARTED = 'Y'
01177          MOVE 'N'                TO W-TEXT-BROWSE-STARTED
01178
01179          
      * EXEC CICS ENDBR
01180 *             DATASET (W-TEXT-ID)
01181 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00006911' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01182
01183      IF  W-TB-NDX = 8
01184          MOVE ER-0006            TO W-CURRENT-ERROR
01185          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT
01186          GO TO 2999-EXIT.
01187
01188      SET W-TB-NDX DOWN BY 1.
01189      SET W-TOTAL-LINES           TO W-TB-NDX.
01190      MOVE 1                      TO W-CURRENT-LINE.
01191
01192      IF  PI-COMPANY-ID EQUAL 'AUK'
01193          MOVE 'Y'                TO W-REVERSE-DATE-SW.
01194
01195      PERFORM 7000-RESOLVE-VARIABLES THRU 7399-EXIT.
01196
01197      IF  PI-COMPANY-ID EQUAL 'AUK'
01198          MOVE SPACES             TO W-REVERSE-DATE-SW.
01199
01200      PERFORM 7800-VARIABLE-SEARCH THRU 7899-EXIT
01201              VARYING
01202          W-TB-NDX FROM 7 BY 1
01203              UNTIL
01204          W-TB-NDX > W-TOTAL-LINES.
01205      GO TO 2999-EXIT.
01206                                  EJECT
01207  2800-PROCESS-Z-CONTROLS.
01208
01209      MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
01210
01211      IF  PI-RESEND-DATE EQUAL LOW-VALUES
01212
01213          IF  W-DAYS-TO-RESEND-1 NUMERIC
01214                  AND
01215              W-DAYS-TO-RESEND-1 GREATER THAN ZEROS
01216              MOVE '6'            TO DC-OPTION-CODE
01217              MOVE W-SAVE-BIN-DATE
01218                                  TO DC-BIN-DATE-1
01219              MOVE ZEROS          TO DC-ELAPSED-MONTHS
01220              MOVE W-DAYS-TO-RESEND-1
01221                                  TO DC-ELAPSED-DAYS
01222              PERFORM 9700-DATE-LINK THRU 9700-EXIT
01223
01224              IF  NO-CONVERSION-ERROR
01225                  MOVE DC-BIN-DATE-2
01226                                  TO PI-RESEND-DATE
01227
01228              ELSE
01229                  MOVE ER-3770    TO W-CURRENT-ERROR
01230                  PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01231
01232      IF  PI-FOLLOW-UP-DATE EQUAL LOW-VALUES
01233
01234          IF  W-DAYS-TO-FOLLOW-UP NUMERIC
01235                  AND
01236              W-DAYS-TO-FOLLOW-UP GREATER THAN ZEROS
01237              MOVE '6'            TO DC-OPTION-CODE
01238              MOVE W-SAVE-BIN-DATE
01239                                  TO DC-BIN-DATE-1
01240              MOVE ZEROS          TO DC-ELAPSED-MONTHS
01241              MOVE W-DAYS-TO-FOLLOW-UP
01242                                  TO DC-ELAPSED-DAYS
01243              PERFORM 9700-DATE-LINK THRU 9700-EXIT
01244
01245              IF  NO-CONVERSION-ERROR
01246                  MOVE DC-BIN-DATE-2
01247                                  TO PI-FOLLOW-UP-DATE
01248
01249              ELSE
01250                  MOVE ER-3771    TO W-CURRENT-ERROR
01251                  PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01252
01253      IF  PI-NUMBER-COPIES NOT NUMERIC
01254              OR
01255          PI-NUMBER-COPIES EQUAL ZEROS
01256
01257          IF  W-NUMBER-OF-COPIES NUMERIC
01258                  AND
01259              W-NUMBER-OF-COPIES NOT EQUAL ZEROS
01260              MOVE W-NUMBER-OF-COPIES
01261                                  TO PI-NUMBER-COPIES
01262
01263          ELSE
01264              MOVE +1             TO PI-NUMBER-COPIES.
01265
040110     IF W-FORM-TO-RESEND > SPACES
040110         MOVE W-FORM-TO-RESEND          TO PI-RESEND-FORM-NUMBER
040110     ELSE
040110         MOVE LOW-VALUES                TO PI-RESEND-FORM-NUMBER
040110     END-IF.
040110
040110     MOVE W-PROMPT-LETTER               TO PI-PROMPT-LETTER.
040110     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD.
040110     MOVE W-AUTO-CLOSE-IND              TO PI-AUTO-CLOSE-IND.
040110     MOVE W-LETTER-TO-BENE              TO PI-LETTER-TO-BENE.
040110
01266  2800-EXIT.
01267      EXIT.
01268
01269  2900-TEXT-NOT-OPEN.
01270
01271      MOVE ER-0013                TO W-CURRENT-ERROR.
01272      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01273
01274  2999-EXIT.
01275      EXIT.
01276                                  EJECT
01277  3000-READ-ADDR.
01278
01279      
      * EXEC CICS HANDLE CONDITION
01280 *         NOTOPEN    (3920-CLM-NOT-OPEN)
01281 *         NOTFND     (3900-CLAIM-NOT-FOUND)
01282 *    END-EXEC.
      *    MOVE '"$JI                  ! $ #00007022' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037303232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01283
01284      
      * EXEC CICS READ
01285 *         DATASET    (W-CLM-ID)
01286 *         SET        (ADDRESS OF CLAIM-MASTER)
01287 *         RIDFLD     (W-CLM-KEY)
01288 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007027' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CLM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CLM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01289
01290 *    SERVICE RELOAD CLAIM-MASTER.
01291
01292      IF  PI-ADDR-TYPE NOT GREATER THAN SPACES
01293          GO TO 3999-EXIT.
01294
01295      PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT.
01296
01297      MOVE PI-ADDR-TYPE           TO W-ADDR-TYPE-CD.
01298
01299      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
01300          GO TO 3200-READ-PRODUCER.
01301
01302  3100-READ-ACCT.
01303
01304      MOVE CL-CERT-GROUPING       TO W-ACCT-GROUPING.
01305      MOVE CL-CERT-STATE          TO W-ACCT-STATE.
01306      MOVE CL-CERT-ACCOUNT        TO W-ACCT-ACCOUNT.
01307      MOVE CL-CERT-EFF-DT         TO W-ACCT-EXP-DATE.
01308
01309      
      * EXEC CICS HANDLE CONDITION
01310 *         NOTOPEN    (3930-ACCT-NOT-OPEN)
01311 *         NOTFND     (3910-ACCT-NOT-FOUND)
01312 *         END-EXEC.
      *    MOVE '"$JI                  ! % #00007052' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037303532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01313
01314      PERFORM 8000-STARTBR-ERACCT THRU 8000-EXIT.
01315
01316      MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.
01317
01318  3110-READNEXT.
01319
01320      PERFORM 8010-READNEXT-ERACCT THRU 8010-EXIT.
01321
01322      IF  W-ACCT-PARTIAL-KEY NOT = W-ACCT-SAVE-KEY
01323
01324          IF  W-SAVE-ACCT-RECORD EQUAL SPACES
01325              GO TO 3910-ACCT-NOT-FOUND
01326
01327          ELSE
01328              MOVE 'Y'            TO W-ACCT-READ-SW
01329              MOVE AM-CONTROL-PRIMARY
01330                                  TO W-ACCT-KEY
01331              MOVE W-SAVE-ACCT-RECORD
01332                                  TO ACCOUNT-MASTER
01333              GO TO 3120-CONTINUE-BUILD-ADDR.
01334
01335      IF  AM-EXPIRATION-DT EQUAL HIGH-VALUES
01336          NEXT SENTENCE
01337
01338      ELSE
01339          MOVE ACCOUNT-MASTER     TO W-SAVE-ACCT-RECORD
01340          GO TO 3110-READNEXT.
01341
01342      MOVE AM-CONTROL-PRIMARY     TO W-ACCT-KEY.
01343      MOVE 'Y'                    TO W-ACCT-READ-SW.
01344
01345
01346  3120-CONTINUE-BUILD-ADDR.
01347
01348      MOVE SPACES                 TO W-SAVE-ACCT-RECORD.
01349
01350      IF  W-ADDR-TYPE-CD EQUAL 'A0'
01351          MOVE ZEROS              TO W-PI-ADDR-SEQ
01352
01353      ELSE
01354          GO TO 3350-CHECK-BENE-ADDR.
01355
01356      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
01357      MOVE AM-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
01358      MOVE AM-PERSON TO W-REC-TEXT (02) W-LABEL-LINES (02).
01359      MOVE AM-ADDRS  TO W-REC-TEXT (03) W-LABEL-LINES (03).
01360      MOVE AM-CITY   TO W-REC-TEXT (04) W-LABEL-LINES (04).
01361      MOVE SPACES    TO W-REC-TEXT (05) W-LABEL-LINES (05).
01362
01363      MOVE SPACES                 TO W-ZIP-CODE.
01364
01365      IF  AM-CANADIAN-POST-CODE
01366          MOVE AM-CAN-POSTAL-1    TO W-CAN-POSTAL-1
01367          MOVE AM-CAN-POSTAL-2    TO W-CAN-POSTAL-2
01368
01369      ELSE
01370          MOVE AM-ZIP-PRIME       TO W-AM-ZIP-CODE
01371
01372          IF  AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS
01373              MOVE '-'            TO W-AM-ZIP-DASH
01374              MOVE AM-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
01375
01376      MOVE W-ZIP-CODE             TO W-REC-TEXT (6)
01377                                     W-LABEL-LINES (06).
01378
01379      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
01380
01381      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.
01382      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.
01383      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.
01384      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.
01385      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.
01386      MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (06) SS05-6D.
01387
01388      GO TO 3999-EXIT.
01389
01390      EJECT
01391  3200-READ-PRODUCER.
01392
01393      MOVE CL-CERT-GROUPING       TO W-PROD-GROUPING.
01394      MOVE CL-CERT-STATE          TO W-PROD-STATE.
01395      MOVE CL-CERT-ACCOUNT        TO W-PROD-PRODUCER.
01396      MOVE CL-CERT-EFF-DT         TO W-PROD-EXP-DATE.
01397
01398      
      * EXEC CICS HANDLE CONDITION
01399 *         NOTOPEN    (3960-PROD-NOT-OPEN)
01400 *         NOTFND     (3950-PROD-NOT-FOUND)
01401 *         END-EXEC.
      *    MOVE '"$JI                  ! & #00007141' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037313431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01402
01403      PERFORM 8050-STARTBR-EMPROD THRU 8050-EXIT.
01404
01405      MOVE W-PROD-PARTIAL-KEY     TO W-PROD-SAVE-KEY.
01406
01407  3210-READNEXT.
01408
01409      PERFORM 8060-READNEXT-EMPROD THRU 8060-EXIT.
01410
01411      IF  W-PROD-PARTIAL-KEY NOT = W-PROD-SAVE-KEY
01412
01413          IF  W-SAVE-PROD-RECORD EQUAL SPACES
01414              GO TO 3950-PROD-NOT-FOUND
01415
01416          ELSE
01417              MOVE 'Y'            TO W-PROD-READ-SW
01418              MOVE PD-CONTROL-PRIMARY
01419                                  TO W-PROD-KEY
01420              MOVE W-SAVE-PROD-RECORD
01421                                  TO PRODUCER-MASTER
01422              GO TO 3220-CONTINUE-BUILD-ADDR.
01423
01424      IF  PD-EXPIRE-DATE EQUAL HIGH-VALUES
01425          NEXT SENTENCE
01426
01427      ELSE
01428          MOVE PRODUCER-MASTER    TO W-SAVE-PROD-RECORD
01429          GO TO 3210-READNEXT.
01430
01431      MOVE PD-CONTROL-PRIMARY     TO W-PROD-KEY.
01432      MOVE 'Y'                    TO W-PROD-READ-SW.
01433
01434  3220-CONTINUE-BUILD-ADDR.
01435
01436      MOVE SPACES                 TO W-SAVE-PROD-RECORD.
01437
01438      IF  W-ADDR-TYPE-CD EQUAL 'A0'
01439          MOVE ZEROS              TO W-PI-ADDR-SEQ
01440
01441      ELSE
01442          GO TO 3350-CHECK-BENE-ADDR.
01443
01444      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
01445      MOVE PD-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
01446      MOVE PD-PERSON TO W-REC-TEXT (02) W-LABEL-LINES (02).
01447      MOVE PD-ADDRS  TO W-REC-TEXT (03) W-LABEL-LINES (03).
01448      MOVE PD-CITY   TO W-REC-TEXT (04) W-LABEL-LINES (04).
01449      MOVE SPACES    TO W-REC-TEXT (05) W-LABEL-LINES (05).
01450
01451      MOVE SPACES                 TO W-ZIP-CODE.
01452
01453      MOVE PD-ZIP-PRIME           TO W-AM-ZIP-CODE.
01454
01455      IF  PD-ZIP-PLUS4 NOT = SPACES AND  ZEROS
01456          MOVE '-'                TO W-AM-ZIP-DASH
01457          MOVE PD-ZIP-PLUS4       TO W-AM-ZIP-PLUS4.
01458
01459      MOVE W-ZIP-CODE             TO W-REC-TEXT (6)
01460                                     W-LABEL-LINES (06).
01461
01462      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
01463
01464      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.
01465      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.
01466      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.
01467      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.
01468      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.
01469      MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (06) SS05-6D.
01470
01471      GO TO 3999-EXIT.
01472
01473      EJECT
01474  3350-CHECK-BENE-ADDR.
01475
01476      IF  W-ADDR-TYPE-CD NOT = 'B0'
01477          GO TO 3400-CONTINUE-BUILD-ADDR.
01478
01479      IF  CL-BENEFICIARY = SPACES
01480          GO TO 3450-ACTV-NOT-FOUND.
01481
01482      
      * EXEC CICS HANDLE CONDITION
01483 *         NOTFND (3450-ACTV-NOT-FOUND)
01484 *    END-EXEC.
      *    MOVE '"$I                   ! '' #00007225' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303037323235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01485
01486      MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.
01487      MOVE 'B'                    TO W-BENE-REC-TYPE.
01488      MOVE CL-BENEFICIARY         TO W-BENE-NUMBER.
01489
01490      
      * EXEC CICS READ
01491 *         DATASET    (W-BENE-ID)
01492 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
01493 *         RIDFLD     (W-BENE-KEY)
01494 *         END-EXEC.
      *    MOVE '&"S        E          (   #00007233' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01495
01496 *    SERVICE RELOAD BENEFICIARY-MASTER.
01497
01498      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
01499      MOVE BE-MAIL-TO-NAME        TO W-REC-TEXT (1)
01500                                     W-LABEL-LINES (01).
01501      MOVE BE-ADDRESS-LINE-1      TO W-REC-TEXT (02)
01502                                     W-LABEL-LINES (02).
01503      MOVE BE-ADDRESS-LINE-2      TO W-REC-TEXT (03)
01504                                     W-LABEL-LINES (03).
01505      MOVE BE-CITY-STATE          TO W-REC-TEXT (04)
01506                                     W-LABEL-LINES (04).
01507
01508      MOVE SPACES                 TO W-ZIP-CODE.
01509
01510      IF  BE-CANADIAN-POST-CODE
01511          MOVE BE-CAN-POSTAL-1    TO W-CAN-POSTAL-1
01512          MOVE BE-CAN-POSTAL-2    TO W-CAN-POSTAL-2
01513
01514      ELSE
01515          MOVE BE-ZIP-PRIME       TO W-AM-ZIP-CODE
01516
01517          IF  BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS
01518              MOVE '-'            TO W-AM-ZIP-DASH
01519              MOVE BE-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
01520
01521      MOVE W-ZIP-CODE             TO W-REC-TEXT (05)
01522                                     W-LABEL-LINES (05).
01523
01524      MOVE SPACES                 TO W-REC-TEXT (6).
01525
01526      GO TO 3420-SET-ADDR.
01527
01528  3400-CONTINUE-BUILD-ADDR.
01529
01530      IF  W-ACTV-SEQ = ZEROS
01531          GO TO 3450-ACTV-NOT-FOUND.
01532
01533      MOVE W-ACTV-SEQ             TO W-PI-ADDR-SEQ.
01534
01535      
      * EXEC CICS HANDLE CONDITION
01536 *         NOTOPEN    (3940-ACTV-NOT-OPEN)
01537 *         NOTFND     (3450-ACTV-NOT-FOUND)
01538 *    END-EXEC.
      *    MOVE '"$JI                  ! ( #00007278' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303037323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01539
01540      
      * EXEC CICS READ
01541 *         DATASET    (W-ACTV-ID)
01542 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
01543 *         RIDFLD     (W-ACTV-KEY)
01544 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007283' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037323833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01545
01546 *    SERVICE RELOAD ACTIVITY-TRAILERS.
01547
01548      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
01549      MOVE AT-MAIL-TO-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
01550      MOVE AT-ADDRESS-LINE-1 TO W-REC-TEXT (02) W-LABEL-LINES (02).
01551      MOVE AT-ADDRESS-LINE-2 TO W-REC-TEXT (03) W-LABEL-LINES (03).
01552      MOVE AT-CITY-STATE     TO W-REC-TEXT (04) W-LABEL-LINES (04).
01553
01554      MOVE SPACES                 TO W-ZIP-CODE.
01555
01556      IF  AT-CANADIAN-POST-CODE
01557          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
01558          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
01559
01560      ELSE
01561          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
01562          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
01563              MOVE '-'            TO W-AM-ZIP-DASH
01564              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
01565
01566      MOVE W-ZIP-CODE             TO W-REC-TEXT (05)
01567                                     W-LABEL-LINES (05).
01568
01569      MOVE SPACES                 TO W-REC-TEXT (6).
01570
01571  3420-SET-ADDR.
01572
01573      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
01574
01575      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.
01576      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.
01577      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.
01578      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.
01579      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.
01580      MOVE SPACES                 TO W-REC-TEXT (06) SS05-6D.
01581      GO TO 3999-EXIT.
01582
01583  3450-ACTV-NOT-FOUND.
01584
01585      IF  W-ACTV-SEQ EQUAL +29
01586          NEXT SENTENCE
01587
01588      ELSE
01589          GO TO 3480-CONTINUE-ACTV-ERROR.
01590
01591      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
01592          GO TO 3999-EXIT.
01593
01594      IF  AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
01595          MOVE ZEROS              TO AM-3RD-PARTY-NOTIF-LEVEL
01596          GO TO 3480-CONTINUE-ACTV-ERROR.
01597
01598      IF  AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS
01599          GO TO 3480-CONTINUE-ACTV-ERROR.
01600
01601      MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD.
01602      MOVE AM-CARRIER             TO W-COMP-CARRIER.
01603      MOVE AM-GROUPING            TO W-COMP-GROUPING.
01604      MOVE 'A'                    TO W-COMP-TYPE.
01605      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
01606                                  TO W-COMP-RESP-NO.
01607
01608      IF  AM-3RD-PARTY-NOTIF-LEVEL EQUAL AM-REMIT-TO
01609          IF AM-COM-TYP (AM-REMIT-TO) EQUAL 'O' OR 'P' OR
052814                                           'G' OR 'B' or 'S'
01611              MOVE 'G'            TO W-COMP-TYPE
01612              MOVE LOW-VALUES     TO W-COMP-ACCOUNT
01613          ELSE
01614              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
01615                                  TO W-COMP-ACCOUNT
01616      ELSE
01617          MOVE 'G'                TO W-COMP-TYPE
01618          MOVE LOW-VALUES         TO W-COMP-ACCOUNT.
01619
01620      IF  PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
01621          MOVE ZEROS              TO W-COMP-CARRIER.
01622
01623      IF  PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
01624          MOVE ZEROS              TO W-COMP-GROUPING.
01625
01626      
      * EXEC CICS HANDLE CONDITION
01627 *         NOTFND    (3480-CONTINUE-ACTV-ERROR)
01628 *         NOTOPEN   (3479-COMP-NOT-OPEN)
01629 *    END-EXEC.
      *    MOVE '"$IJ                  ! ) #00007369' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303037333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01630
01631      
      * EXEC CICS  READ
01632 *         SET      (ADDRESS OF COMPENSATION-MASTER)
01633 *         DATASET  ('ERCOMP')
01634 *         RIDFLD   (W-COMP-KEY)
01635 *    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00007374' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01636
01637 *    SERVICE RELOAD COMPENSATION-MASTER.
01638
01639      MOVE 'Y'                    TO W-COMP-READ-SW.
01640      MOVE SPACES                 TO W-LABEL-HOLD-AREA
01641      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)
01642
01643      IF  CO-ACCT-NAME EQUAL SPACES
01644          MOVE CO-MAIL-NAME       TO W-LABEL-LINES (01).
01645
01646      MOVE CO-ADDR-1              TO W-LABEL-LINES (02).
01647      MOVE CO-ADDR-2              TO W-LABEL-LINES (03).
01648      MOVE CO-ADDR-3              TO W-LABEL-LINES (04).
01649
01650      MOVE SPACES                 TO W-ZIP-CODE.
01651
01652      IF  CO-CANADIAN-POST-CODE
01653          MOVE CO-CAN-POSTAL-1    TO W-CAN-POSTAL-1
01654          MOVE CO-CAN-POSTAL-2    TO W-CAN-POSTAL-2
01655
01656      ELSE
01657          MOVE CO-ZIP-PRIME       TO W-AM-ZIP-CODE
01658
01659          IF  CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS
01660              MOVE '-'            TO W-AM-ZIP-DASH
01661              MOVE CO-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
01662
01663      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
01664
01665      MOVE ZEROS                  TO W-PHONE-IN.
01666      MOVE CO-AREA-CODE           TO W-PO-AREA.
01667      MOVE CO-PREFIX              TO W-PO-PFX.
01668      MOVE CO-PHONE               TO W-PO-SFX.
01669      MOVE W-PHONE-OUT            TO SS53-6D.
01670
01671      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
01672
01673      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (1) SS52D.
01674      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS53-1D.
01675      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS53-2D.
01676      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS53-3D.
01677      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS53-4D.
01678      MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (6) SS53-5D.
01679
01680      GO TO 3999-EXIT.
01681
01682  3479-COMP-NOT-OPEN.
01683
01684      MOVE ER-2055                TO W-CURRENT-ERROR.
01685      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01686      GO TO 3999-EXIT.
01687                                  EJECT
01688  3480-CONTINUE-ACTV-ERROR.
01689
01690      MOVE ER-0178                TO W-CURRENT-ERROR.
01691      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01692 *    MOVE SPACES                 TO PI-ADDR-TYPE.
01693      MOVE SPACES                 TO W-REC-TEXT (01)
01694                                     W-REC-TEXT (02)
01695                                     W-REC-TEXT (03)
01696                                     W-REC-TEXT (04)
01697                                     W-REC-TEXT (05)
01698                                     W-REC-TEXT (06).
01699      GO TO 3999-EXIT.
01700
01701  3900-CLAIM-NOT-FOUND.
01702
01703      MOVE ER-0186                TO W-CURRENT-ERROR.
01704      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01705      GO TO 3999-EXIT.
01706
01707  3910-ACCT-NOT-FOUND.
01708
01709      MOVE ER-0179                TO W-CURRENT-ERROR.
01710      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01711      GO TO 3999-EXIT.
01712
01713  3920-CLM-NOT-OPEN.
01714
01715      MOVE ER-0154                TO W-CURRENT-ERROR.
01716      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01717      GO TO 3999-EXIT.
01718
01719  3930-ACCT-NOT-OPEN.
01720
01721      MOVE ER-0168                TO W-CURRENT-ERROR.
01722      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01723      GO TO 3999-EXIT.
01724
01725  3940-ACTV-NOT-OPEN.
01726
01727      MOVE ER-0172                TO W-CURRENT-ERROR.
01728      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01729      GO TO 3999-EXIT.
01730
01731  3950-PROD-NOT-FOUND.
01732      MOVE ER-9887                TO W-CURRENT-ERROR.
01733      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01734      GO TO 3999-EXIT.
01735
01736  3960-PROD-NOT-OPEN.
01737      MOVE ER-9886                TO W-CURRENT-ERROR.
01738      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01739      GO TO 3999-EXIT.
01740
01741  3999-EXIT.
01742       EXIT.
01743                                  EJECT
01744  6000-ARCHIVE-LETTER.
01745 ***************************************************************
01746 *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
01747 *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *
01748 *    THE ARCHIVE FILE.                                        *
01749 *    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *
01750 *        1. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *
01751 *        2. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
01752 *        3. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *
01753 *        4. BUILD A CORRESPONDENCE TRAILER.                   *
01754 *        5. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *
01755 *           ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.      *
01756 *        6. RESET ALL CONTROL FIELDS AND RETURN THE           *
01757 *           ARCHIVE NUMBER USED TO FILE THE RECORDS.          *
01758 ***************************************************************
01759
01760      MOVE +0                     TO TALLY.
01761      INSPECT W-RECORD-TABLE TALLYING TALLY
01762                                  FOR CHARACTERS BEFORE '@'.
01763
01764      IF  TALLY LESS THAN +21900
01765          COMPUTE W-CURRENT-LINE = TALLY / 73
01766          MOVE ER-0191            TO W-CURRENT-ERROR
01767          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01768
01769      MOVE '1'                    TO W-CNTL-RECORD-TYPE.
01770      MOVE ZEROS                  TO W-CNTL-SEQ.
01771      MOVE SPACES                 TO W-CNTL-GENL.
01772
01773      
      * EXEC CICS HANDLE CONDITION
01774 *         NOTFND  (6100-NOT-FOUND)
01775 *         NOTOPEN (6110-CNTL-NOT-OPEN)
01776 *    END-EXEC.
      *    MOVE '"$IJ                  ! * #00007516' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303037353136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01777
01778      
      * EXEC CICS READ
01779 *         DATASET  (W-CNTL-ID)
01780 *         SET      (ADDRESS OF CONTROL-FILE)
01781 *         RIDFLD   (W-CNTL-KEY)
01782 *         UPDATE
01783 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00007521' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01784
01785 *    SERVICE RELOAD CONTROL-FILE.
01786      ADD 1                       TO CF-CO-ARCHIVE-COUNTER.
01787      MOVE CF-CO-ARCHIVE-COUNTER  TO W-ARCH-NUMBER
01788                                     PI-ARCHIVE-NUMBER.
01789      MOVE CF-PRINT-ADDRESS-LABELS
01790                                  TO W-LABELS-SW.
01791
01792      
      * EXEC CICS REWRITE
01793 *         FROM      (CONTROL-FILE)
01794 *         DATASET   (W-CNTL-ID)
01795 *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007535' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01796
01797      PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
01798
01799      
      * EXEC CICS HANDLE CONDITION
01800 *         NOTOPEN (6120-ARCH-NOT-OPEN)
01801 *    END-EXEC.
      *    MOVE '"$J                   ! + #00007542' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303037353432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01802
01803      IF  PI-ARCH-POINTER GREATER THAN ZEROS
01804          MOVE PI-ARCH-POINTER    TO LCP-WS-ADDR-COMP
01805          SET ADDRESS OF LETTER-ARCHIVE TO LCP-WS-ADDR-PNTR
01806 *        SERVICE RELOAD LETTER-ARCHIVE
01807
01808      ELSE
01809          
      * EXEC CICS GETMAIN
01810 *             SET      (ADDRESS OF LETTER-ARCHIVE)
01811 *             LENGTH   (W-ARCH-LENGTH)
01812 *        END-EXEC
      *    MOVE '," L                  $   #00007552' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01813          SET LCP-WS-ADDR-PNTR TO ADDRESS OF LETTER-ARCHIVE
01814
01815 *        SERVICE RELOAD LETTER-ARCHIVE
01816          MOVE LCP-WS-ADDR-COMP TO PI-ARCH-POINTER.
01817
01818      MOVE SPACES                 TO LETTER-ARCHIVE.
01819      MOVE 'LA'                   TO LA-RECORD-ID.
01820      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
01821                                     LA-ARCHIVE-NO-A1.
01822      MOVE '1'                    TO LA-RECORD-TYPE
01823                                     LA-RECORD-TYPE-A1.
01824      MOVE ZEROS                  TO LA-LINE-SEQ-NO
01825                                     LA-LINE-SEQ-NO-A1.
01826      MOVE W-ARCH-CO              TO LA-COMPANY-CD
01827                                     LA-COMPANY-CD-A1.
01828      MOVE W-CLM-CARRIER          TO LA-CARRIER.
01829      MOVE PI-CLAIM-NO            TO LA-CLAIM-NO.
01830      MOVE PI-CERT-NO             TO LA-CERT-NO.
01831
01832      IF  PI-NUMBER-COPIES NUMERIC
01833              AND
01834          PI-NUMBER-COPIES GREATER THAN ZEROS
01835          MOVE PI-NUMBER-COPIES
01836                                  TO LA-NO-OF-COPIES
01837
01838      ELSE
01839          MOVE  1                 TO LA-NO-OF-COPIES.
01840
01841      MOVE PI-RESEND-DATE         TO LA-RESEND-DATE.
01842
01843      MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.
01844      MOVE W-CURRENT-SAVE         TO LA-CREATION-DT.
01845      MOVE LOW-VALUES             TO LA-INITIAL-PRINT-DATE
01846                                     LA-RESEND-PRINT-DATE.
01847      MOVE W-CORR-TRLR-SEQ        TO LA-CORR-TRLR-SEQ.
01848      PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
033110
040110     IF PI-PROMPT-LETTER NOT EQUAL 'Y'
033110         PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT
040110     END-IF.
01849
01850      IF  W-LABELS-SW EQUAL       TO 'N'
01851          NEXT SENTENCE
01852
01853      ELSE
01854          SET W-TB-NDX            TO 1
01855          MOVE ZEROS              TO W-SEQ-COUNTER
01856          PERFORM 6300-FORMAT-ADDRESS THRU 6300-EXIT
01857                  VARYING
01858              W-TB-NDX FROM 1 BY 1
01859                  UNTIL
01860              W-TB-NDX GREATER THAN 6.
01861
01862      MOVE ZEROS                  TO W-SEQ-COUNTER.
01863      PERFORM 6200-FORMAT-TEXT THRU 6200-EXIT
01864              VARYING
01865          W-TB-NDX FROM 8 BY 1
01866              UNTIL
01867          W-TB-NDX GREATER THAN W-TOTAL-LINES.
01868
01869  6000-EXIT.
01870      EXIT.
01871
01872  6100-NOT-FOUND.
01873
01874      MOVE ER-0281                TO W-CURRENT-ERROR.
01875      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01876      GO TO 6000-EXIT.
01877
01878  6110-CNTL-NOT-OPEN.
01879
01880      MOVE ER-0042                TO W-CURRENT-ERROR.
01881      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01882      GO TO 6000-EXIT.
01883
01884  6120-ARCH-NOT-OPEN.
01885
01886      MOVE ER-0332                TO W-CURRENT-ERROR.
01887      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01888      GO TO 6000-EXIT.
01889
01890                                  EJECT
01891  6200-FORMAT-TEXT.
01892
01893      MOVE SPACES                 TO LETTER-ARCHIVE.
01894      MOVE '3'                    TO LA-RECORD-TYPE
01895                                     LA-RECORD-TYPE-A1.
01896      MOVE 'LA'                   TO LA-RECORD-ID.
01897      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
01898                                     LA-ARCHIVE-NO-A1.
01899      MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO
01900                                     LA-LINE-SEQ-NO-A1.
01901      MOVE W-ARCH-CO              TO LA-COMPANY-CD
01902                                     LA-COMPANY-CD-A1.
01903      MOVE W-REC-TEXT (W-TB-NDX)  TO LA-TEXT-LINE.
01904      SET W-TB-NDX1               TO W-TB-NDX.
01905      SET W-TB-NDX1 UP BY 1.
01906      MOVE ZEROS                  TO W-NDX-WORK.
01907
01908  6200-LOOP.
01909
01910      IF  W-TB-NDX1 LESS THAN W-TOTAL-LINES
01911              AND
01912          W-REC-TEXT (W-TB-NDX1) = SPACES
01913          SET W-TB-NDX1 UP BY 1
01914          ADD 1                   TO W-NDX-WORK
01915          GO TO 6200-LOOP.
01916
01917      IF  W-REC-TEXT (W-TB-NDX1) = W-TOP-FORM
01918          MOVE '99'               TO LA-SKIP-CONTROL
01919          SET W-TB-NDX1 UP BY 1
01920
01921      ELSE
01922          MOVE W-NDX-WORK         TO LA-SKIP-CONTROL.
01923
01924      SET W-TB-NDX                TO W-TB-NDX1.
01925      SET W-TB-NDX DOWN BY 1.
01926      PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
01927      ADD 1 TO W-SEQ-COUNTER.
01928
01929  6200-EXIT.
01930       EXIT.
01931                                  EJECT
01932  6300-FORMAT-ADDRESS.
01933
01934      MOVE SPACES                 TO LETTER-ARCHIVE.
01935      MOVE '2'                    TO LA-RECORD-TYPE
01936                                     LA-RECORD-TYPE-A1.
01937      MOVE 'LA'                   TO LA-RECORD-ID.
01938      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
01939                                     LA-ARCHIVE-NO-A1.
01940      MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO
01941                                     LA-LINE-SEQ-NO-A1.
01942      MOVE W-ARCH-CO              TO LA-COMPANY-CD
01943                                     LA-COMPANY-CD-A1.
01944      MOVE W-REC-TEXT (W-TB-NDX)  TO LA-ADDRESS-LINE.
01945      PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.
01946      ADD 1 TO W-SEQ-COUNTER.
01947
01948  6300-EXIT.
01949       EXIT.
01950                                  EJECT
01951  6400-WRITE-ARCHIVE.
01952
01953      
      * EXEC CICS HANDLE CONDITION
01954 *        DUPREC    (6420-ARCH-DUPREC)
01955 *        NOTOPEN   (6410-ARCH-NOT-OPEN)
01956 *        NOSPACE   (6425-ARCH-NOSPACE)
01957 *    END-EXEC.
      *    MOVE '"$%JE                 ! , #00007700' TO DFHEIV0
           MOVE X'2224254A4520202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303037373030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01958
01959      
      * EXEC CICS WRITE
01960 *         DATASET   (W-ARCH-ID)
01961 *         FROM      (LETTER-ARCHIVE)
01962 *         RIDFLD    (LA-CONTROL-PRIMARY)
01963 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007706' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01964
01965  6400-EXIT.
01966      EXIT.
01967
01968  6410-ARCH-NOT-OPEN.
01969
01970      MOVE ER-0332                TO W-CURRENT-ERROR.
01971      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01972      GO TO 6400-EXIT.
01973
01974  6420-ARCH-DUPREC.
01975
01976      MOVE ER-3766                TO W-CURRENT-ERROR.
01977      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01978      GO TO 6400-EXIT.
01979
01980  6425-ARCH-NOSPACE.
01981
01982      MOVE ER-3699                TO W-CURRENT-ERROR.
01983      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
01984      GO TO 6400-EXIT.
01985                                  EJECT
01986  6500-BUILD-CORRESPOND.
01987 ***************************************************************
01988 *    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *
01989 *    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *
01990 *    USING THE NEW SEQUENCE NUMBER.                           *
01991 *    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *
01992 *    TRAILER RECORD.                                          *
01993 ***************************************************************
01994
01995      MOVE PI-CLAIM-NO            TO W-CLM-CLAIM.
01996
01997      
      * EXEC CICS READ
01998 *         DATASET    (W-CLM-ID)
01999 *         SET        (ADDRESS OF CLAIM-MASTER)
02000 *         RIDFLD     (W-CLM-KEY)
02001 *         UPDATE
02002 *         END-EXEC.
      *    MOVE '&"S        EU         (   #00007744' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CLM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CLM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02003
02004 *    SERVICE RELOAD CLAIM-MASTER.
02005
02006      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
02007
02008      IF  PI-FOLLOW-UP-DATE GREATER THAN CL-NEXT-FOLLOWUP-DT
02009          MOVE PI-FOLLOW-UP-DATE  TO CL-NEXT-FOLLOWUP-DT.
02010
02011      IF  PI-RESEND-DATE GREATER THAN CL-NEXT-FOLLOWUP-DT
02012          MOVE PI-RESEND-DATE     TO CL-NEXT-FOLLOWUP-DT.
02013
02014      MOVE '2'                    TO CL-LAST-MAINT-TYPE.
02015
02016      IF  PI-ACTV-POINTER GREATER THAN ZEROS
02017          MOVE PI-ACTV-POINTER    TO LCP-WS-ADDR-COMP
02018          SET ADDRESS OF ACTIVITY-TRAILERS TO LCP-WS-ADDR-PNTR
02019 *        SERVICE RELOAD ACTIVITY-TRAILERS
02020
02021      ELSE
02022          
      * EXEC CICS GETMAIN
02023 *             SET      (ADDRESS OF ACTIVITY-TRAILERS)
02024 *             INITIMG  (W-GETMAINSPACE)
02025 *             LENGTH   (W-ACTV-LENGTH)
02026 *        END-EXEC
      *    MOVE ',"IL                  $   #00007769' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ACTV-LENGTH, 
                 W-GETMAINSPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02027          SET LCP-WS-ADDR-PNTR TO ADDRESS OF ACTIVITY-TRAILERS
02028
02029 *        SERVICE RELOAD ACTIVITY-TRAILERS
02030          MOVE LCP-WS-ADDR-COMP TO PI-ACTV-POINTER.
02031
02032      MOVE 'AT'                   TO AT-RECORD-ID.
02033      MOVE  4                     TO AT-TRAILER-TYPE.
02034      MOVE W-CURRENT-SAVE         TO AT-RECORDED-DT
02035                                     CL-LAST-MAINT-DT
02036                                     AT-CORR-LAST-MAINT-DT
02037      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
02038                                     CL-LAST-MAINT-USER
02039                                     AT-CORR-LAST-UPDATED-BY
02040      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
02041                                     CL-LAST-MAINT-HHMMSS.
02042      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.
02043 *    MOVE W-ACTV-KEY             TO AT-CONTROL-PRIMARY.
02044 *    MOVE W-ACTV-SAVE-KEY        TO AT-CONTROL-PRIMARY.
02045      MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.
02046      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
02047                                     W-CORR-TRLR-SEQ.
02048      MOVE W-CURRENT-SAVE         TO AT-LETTER-SENT-DT.
02049      MOVE PI-FOLLOW-UP-DATE      TO AT-RECEIPT-FOLLOW-UP.
02050      MOVE PI-RESEND-DATE         TO AT-AUTO-RE-SEND-DT.
02051      MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
02052                                     AT-LETTER-PURGED-DT.
02053      MOVE W-ARCH-NUMBER          TO AT-LETTER-ARCHIVE-NO.
02054      MOVE '1'                    TO AT-LETTER-ORIGIN.
02055
02056      MOVE PI-FORM-NUMBER         TO AT-STD-LETTER-FORM
040110     MOVE PI-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM.
040110     MOVE PI-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND.
040110     MOVE PI-LETTER-TO-BENE      TO AT-LETTER-TO-BENE.
02057
02058      IF  PI-REASON GREATER THAN LOW-VALUES
02059          MOVE PI-REASON          TO AT-REASON-TEXT
02060
02061      ELSE
02062          MOVE SPACES             TO AT-REASON-TEXT.
02063
02064      MOVE W-PI-ADDR-SEQ          TO AT-ADDRESS-REC-SEQ-NO.
02065
02066      IF  PI-ADDR-TYPE = LOW-VALUES
02067          MOVE SPACES             TO AT-ADDRESEE-TYPE
02068
02069      ELSE
02070          MOVE PI-ADDR-TYPE       TO AT-ADDRESEE-TYPE.
02071
02072      MOVE W-REC-TEXT (1)         TO AT-ADDRESSEE-NAME.
02073
02074      MOVE LOW-VALUES             TO AT-INITIAL-PRINT-DATE
02075                                     AT-RESEND-PRINT-DATE.
02076
02077      
      * EXEC CICS HANDLE CONDITION
02078 *        DUPREC    (6596-ACTV-DUPREC)
02079 *        NOSPACE   (6597-ACTV-NOSPACE)
02080 *    END-EXEC.
      *    MOVE '"$%E                  ! - #00007827' TO DFHEIV0
           MOVE X'222425452020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303037383237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02081
02082      
      * EXEC CICS WRITE
02083 *         DATASET    (W-ACTV-ID)
02084 *         FROM       (ACTIVITY-TRAILERS)
02085 *         RIDFLD     (AT-CONTROL-PRIMARY)
02086 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007832' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02087
02088      
      * EXEC CICS HANDLE CONDITION
02089 *        DUPREC      (6598-REWRITE-CLAIM)
02090 *    END-EXEC.
      *    MOVE '"$%                   ! . #00007838' TO DFHEIV0
           MOVE X'222425202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303037383338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02091
02092      
      * EXEC CICS REWRITE
02093 *         DATASET    (W-CLM-ID)
02094 *         FROM       (CLAIM-MASTER)
02095 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00007842' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CLM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02096
02097      GO TO 6599-EXIT.
02098
02099  6596-ACTV-DUPREC.
02100
02101      MOVE ER-3697                TO W-CURRENT-ERROR.
02102      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02103      GO TO 6599-EXIT.
02104
02105  6597-ACTV-NOSPACE.
02106
02107      MOVE ER-3698                TO W-CURRENT-ERROR.
02108      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02109      GO TO 6599-EXIT.
02110
02111  6598-REWRITE-CLAIM.
02112
02113  6599-EXIT.
02114       EXIT.
02115                                  EJECT
02116  6600-SET-ADDR-SEQ.
02117
02118      MOVE PI-ADDR-TYPE           TO W-ADDR-TYPE-CD.
02119
02120      IF  W-ADDR-SEQ NOT NUMERIC
02121          GO TO 6699-EXIT.
02122
02123      MOVE ZEROS                  TO W-PI-ADDR-SEQ.
02124
02125      IF  PI-ADDR-TYPE EQUAL 'I'
02126          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02127
02128      ELSE
02129      IF  PI-ADDR-TYPE EQUAL 'A'
02130          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02131          ADD +20                 TO W-ACTV-SEQ
02132
02133      ELSE
02134      IF  PI-ADDR-TYPE EQUAL 'B'
02135          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02136          ADD +10                 TO W-ACTV-SEQ
02137
02138      ELSE
02139      IF  PI-ADDR-TYPE EQUAL 'P'
02140          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02141          ADD +30                 TO W-ACTV-SEQ
02142
02143      ELSE
02144      IF  PI-ADDR-TYPE EQUAL 'E'
02145          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02146          ADD +40                 TO W-ACTV-SEQ
02147
02148      ELSE
02149      IF  PI-ADDR-TYPE EQUAL 'O'
02150          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02151          ADD +50                 TO W-ACTV-SEQ
02152
02153      ELSE
02154      IF  PI-ADDR-TYPE EQUAL 'Q'
02155          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ
02156          ADD +60                 TO W-ACTV-SEQ.
02157
02158  6699-EXIT.
02159       EXIT.
033110
033110 6700-BUILD-NAPERSOFT.
033110
033110     
      * EXEC CICS GETMAIN
033110*         SET      (ADDRESS OF NAPERSOFT-FILE)
033110*         LENGTH   (W-NAPS-LENGTH)
033110*    END-EXEC.
      *    MOVE '," L                  $   #00007913' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-NAPS-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF NAPERSOFT-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
033110
033110     MOVE LOW-VALUES            TO  NAPERSOFT-FILE.
033110     MOVE 'NA'                  TO  NA-RECORD-ID.
033110     MOVE PI-COMPANY-CD         TO  NA-COMPANY-CD.
033110     MOVE PI-CARRIER            TO  NA-CARRIER.
033110     MOVE PI-CLAIM-NO           TO  NA-CLAIM-NO.
033110     MOVE PI-CERT-NO            TO  NA-CERT-NO.
033110     MOVE W-ARCH-NUMBER         TO  NA-ARCHIVE-NO.
033110     MOVE PI-FORM-NUMBER        TO  NA-LETTER-ID.
033110     MOVE PI-PROCESSOR-ID       TO  NA-PROCESSOR-ID.
033110     MOVE W-CURRENT-SAVE        TO  NA-CREATION-DT.
033110     MOVE LOW-VALUES            TO  NA-INITIAL-PRINT-DT
033110     MOVE PI-FOLLOW-UP-DATE     TO  NA-FOLLOW-UP-DT.
033110     MOVE PI-RESEND-DATE        TO  NA-RESEND-DT
040110     MOVE PI-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
033110     IF PI-NUMBER-COPIES NOT = ZEROS
033110         MOVE PI-NUMBER-COPIES  TO  NA-NO-OF-COPIES
033110     ELSE
033110         MOVE 1                 TO  NA-NO-OF-COPIES
033110     END-IF.
033110     IF PI-ADDR-TYPE = ' 0'
033110         MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
033110     ELSE
033110         MOVE PI-ADDR-TYPE      TO  NA-ADDRESS-TYPE
033110     END-IF.
033110     MOVE W-CORR-TRLR-SEQ       TO  NA-CORR-TRLR-SEQ.
040110     MOVE PI-ENCLOSURE-CD       TO  NA-ENCLOSURE-CD.
033110     IF PI-AUTO-LETTER-DATE > SPACES
033110        MOVE PI-AUTO-LETTER-DATE TO DC-GREG-DATE-A-EDIT
033110        MOVE DC-EDITA-MONTH     TO DC-EDIT1-MONTH
033110        MOVE SLASHA-1           TO SLASH1-1
033110        MOVE DC-EDITA-DAY       TO DC-EDIT1-DAY
033110        MOVE SLASHA-2           TO SLASH1-2
033110        MOVE DC-EDITA-YEAR      TO DC-EDIT1-YEAR
033110        MOVE '2'                TO  DC-OPTION-CODE
033110        PERFORM 9700-DATE-LINK  THRU  9700-EXIT
033110        IF DATE-CONVERSION-ERROR
033110            MOVE LOW-VALUES     TO  NA-AUTOPYDT
033110        ELSE
033110            MOVE DC-BIN-DATE-1  TO  NA-AUTOPYDT
033110     ELSE
033110        MOVE LOW-VALUES         TO  NA-AUTOPYDT
033110     END-IF.
033110
033110     
      * EXEC CICS WRITE
033110*         DATASET    (W-NAPS-ID)
033110*         FROM       (NAPERSOFT-FILE)
033110*         RIDFLD     (NA-CONTROL-PRIMARY)
033110*    END-EXEC.
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00007961' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NAPS-ID, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 NA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
033110
033110 6799-EXIT.
033110      EXIT.
02160                                  EJECT
02161  7000-RESOLVE-VARIABLES.
02162 ***************************************************************
02163 *    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *
02164 *    WITH DATA PERTAINING        TO THE DESIGNATED CLAIM.     *
02165 *    THIS ROUTINE IS PERFORM THRU 7399-EXIT IN ORDER TO       *
02166 *    RESOLVE ALL OF THE SYMBOLS.                              *
02167 ***************************************************************
02168
02169      MOVE PI-CLAIM-NO            TO SS34D.
02170
121802     EVALUATE TRUE
121802
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802        MOVE PI-AH-OVERRIDE-L6   TO SS12D
121802
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802        MOVE PI-LIFE-OVERRIDE-L6 TO SS12D
121802
121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE '  IU  '            TO SS12D
121802
121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO SS12D
080322
080322     WHEN CL-CLAIM-TYPE = 'B'
080322        MOVE ' BRV  '            TO SS12D
080322
080322     WHEN CL-CLAIM-TYPE = 'H'
080322        MOVE ' HOSP '            TO SS12D
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO SS12D
121802
121802     END-EVALUATE
02178      MOVE CL-LAST-PMT-AMT        TO SS16D.
02179      MOVE CL-TOTAL-PAID-AMT      TO SS18D.
02180      MOVE CL-CAUSE-CD            TO SS19-1D.
02181      MOVE CL-ASSOC-CERT-SEQU     TO SS54D.
02182      MOVE CL-ASSOC-CERT-TOTAL    TO SS55D.
02183
02184      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.
02185      MOVE SPACES                 TO DC-OPTION-CODE.
02186      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02187
02188      IF  NO-CONVERSION-ERROR
02189          MOVE DC-GREG-DATE-1-EDIT
02190                                  TO SS13D.
02191
02192      MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.
02193      MOVE SPACES                 TO DC-OPTION-CODE.
02194      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02195
02196      IF  NO-CONVERSION-ERROR
02197          MOVE DC-GREG-DATE-1-EDIT
02198                                  TO SS14D.
02199
02200      MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.
02201      MOVE SPACES                 TO DC-OPTION-CODE.
02202      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02203
02204      IF  NO-CONVERSION-ERROR
02205          MOVE DC-GREG-DATE-1-EDIT
02206                                  TO SS15D.
02207
02208      IF  NOT PI-USES-PAID-TO
02209          MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
02210          MOVE SPACES             TO DC-OPTION-CODE
02211          PERFORM 9700-DATE-LINK THRU 9700-EXIT
02212
02213          IF  NO-CONVERSION-ERROR
02214              MOVE DC-GREG-DATE-1-EDIT
02215                                  TO SS17D
02216
02217          ELSE
02218              MOVE SPACES         TO SS17D
02219
02220      ELSE
02221          MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1
02222          MOVE '6'                TO DC-OPTION-CODE
02223          MOVE +1                 TO DC-ELAPSED-DAYS
02224          MOVE +0                 TO DC-ELAPSED-MONTHS
02225          PERFORM 9700-DATE-LINK THRU 9700-EXIT
02226
02227          IF  NO-CONVERSION-ERROR
02228              MOVE DC-GREG-DATE-1-EDIT
02229                                  TO SS17D.
02230
02231      MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.
02232      MOVE SPACES                 TO DC-OPTION-CODE.
02233      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02234
02235      IF  NO-CONVERSION-ERROR
02236          MOVE DC-GREG-DATE-1-EDIT
02237                                  TO SS45D
02238
02239      ELSE
02240          MOVE '@@DOB'            TO SS45D.
02241
02242      IF  CL-SSN-STATE = CL-CERT-STATE
02243
02244          IF  CL-SSN-ACCOUNT EQUAL CL-CERT-ACCOUNT-PRIME
02245              NEXT SENTENCE
02246
02247          ELSE
02248              MOVE CL-SOC-SEC-NO  TO SS46D
02249
02250      ELSE
02251          MOVE CL-SOC-SEC-NO      TO SS46D.
02252
02253      MOVE CL-CERT-GROUPING       TO W-CERT-GROUPING
02254                                     W-ACCT-GROUPING
02255                                     W-PROD-GROUPING
02256                                     W-PLCY-GROUPING
02257                                     W-PLAN-GROUPING.
02258      MOVE CL-CERT-STATE          TO W-CERT-STATE
02259                                     W-ACCT-STATE
02260                                     W-PROD-STATE
02261                                     W-PLCY-STATE
02262                                     W-PLAN-STATE.
02263      MOVE CL-CERT-ACCOUNT        TO W-CERT-ACCOUNT
02264                                     W-ACCT-ACCOUNT
02265                                     W-PROD-PRODUCER
02266                                     W-PLCY-PRODUCER
02267                                     W-PLAN-PRODUCER.
02268      MOVE CL-CERT-EFF-DT         TO W-CERT-EFF-DT
02269                                     W-PLCY-EFF-DT.
02270      MOVE CL-CV-REFERENCE-NO     TO W-PLCY-REFERENCE-NO.
02271
02272      PERFORM 7400-MOVE-NAME THRU 7400-EXIT.
02273
02274      IF  LOWER-CASE-LETTERS-USED
02275          MOVE W-NAME-WORK        TO W-TEMP-AREA2
02276          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
02277          MOVE W-TEMP-AREA2       TO SS10D
02278
02279      ELSE
02280          MOVE W-NAME-WORK        TO SS10D.
02281
02282      MOVE CL-INSURED-1ST-NAME    TO W-FIRST-NAME.
02283      MOVE CL-INSURED-LAST-NAME   TO W-LAST-NAME.
02284      MOVE CL-INSURED-MID-INIT    TO W-MIDDLE-NAME.
02285      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
02286
02287      IF  LOWER-CASE-LETTERS-USED
02288          MOVE W-NAME-WORK        TO W-TEMP-AREA2
02289          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
02290          MOVE W-TEMP-AREA2       TO SS39D
02291
02292      ELSE
02293          MOVE W-NAME-WORK        TO SS39D.
02294
02295      MOVE CL-INSURED-LAST-NAME   TO SS40D.
02296
02297      IF  INSURED-IS-FEMALE
02298          MOVE 'MS.'              TO SS41D
02299
02300      ELSE
02301          MOVE 'MR.'              TO SS41D.
02302
02303      IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'
02304          PERFORM 7370-RESOLVE-CREDITOR THRU 7370-EXIT.
02305
02306      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
02307          GO TO 7010-READ-POLICY-RECORD.
02308                                  EJECT
02309      
      * EXEC CICS HANDLE CONDITION
02310 *         NOTOPEN   (7022-CERT-NOT-OPEN)
02311 *         NOTFND    (7024-CERT-NOT-FOUND)
02312 *    END-EXEC.
      *    MOVE '"$JI                  ! / #00008135' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303038313335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02313
02314      
      * EXEC CICS READ
02315 *         DATASET   (W-CERT-ID)
02316 *         SET       (ADDRESS OF CERTIFICATE-MASTER)
02317 *         RIDFLD    (W-CERT-KEY)
02318 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008140' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02319
02320 *    SERVICE RELOAD CERTIFICATE-MASTER.
02321
02322      MOVE CM-CERT-NO             TO SS26D.
02323      MOVE CM-INSURED-ISSUE-AGE   TO SS33D.
02324      MOVE CM-LOAN-BALANCE        TO SS37D.
02325      MOVE CM-MEMBER-NO           TO SS38D.
02326
02327      IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'
02328          MOVE CL-CURRENT-CARRIER TO SS23D
02329          MOVE CL-CURRENT-GROUPING
02330                                  TO SS24D
02331          MOVE CL-CURRENT-ACCOUNT TO SS25D
02332          MOVE CM-MEMBER-NO       TO SS36D
02333          MOVE CM-GROUPING        TO W-GROUPING
02334
02335          IF  W-GROUP-3 EQUAL 'C01' OR 'C02'
02336              MOVE CM-MEMBER-NO   TO W-CURRENT-LOAN-NO
02337              MOVE CM-LOAN-NUMBER TO W-LOAN-NUMBER
02338              MOVE W-CREDIT-CARD-LOAN-NO
02339                                  TO SS36-1D
02340
02341          ELSE
02342              MOVE CM-MEMBER-NO   TO SS36-1D
02343
02344      ELSE
02345          MOVE CM-CARRIER         TO SS23D
02346          MOVE CM-GROUPING        TO SS24D
02347          MOVE CM-ACCOUNT         TO SS25D
02348          MOVE CM-LOAN-NUMBER     TO SS36D.
02349
02350      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
02351      MOVE SPACES                 TO DC-OPTION-CODE.
02352      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02353
02354      IF  NO-CONVERSION-ERROR
02355          MOVE DC-GREG-DATE-1-EDIT TO SS27D.
02356
02357      MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME.
02358      MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME.
02359      MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME.
02360      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
02361
02362      IF  LOWER-CASE-LETTERS-USED
02363          MOVE W-NAME-WORK        TO W-TEMP-AREA2
02364          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
02365          MOVE W-TEMP-AREA2       TO SS57D
02366
02367      ELSE
02368          MOVE W-NAME-WORK        TO SS57D.
02369
02370      MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME.
02371      MOVE CM-JT-LAST-NAME        TO W-LAST-NAME.
02372      MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME.
02373      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
02374
02375      IF  LOWER-CASE-LETTERS-USED
02376          MOVE W-NAME-WORK        TO W-TEMP-AREA2
02377          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
02378          MOVE W-TEMP-AREA2       TO SS58D
02379
02380      ELSE
02381          MOVE W-NAME-WORK        TO SS58D.
02382
100518     IF  CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
02384          MOVE CM-LF-BENEFIT-CD   TO W-BEN-HOLD
02385          MOVE CM-LF-ORIG-TERM    TO SS29D
02386          MOVE CM-LF-BENEFIT-AMT  TO SS30D
02387          MOVE CM-POLICY-FORM-NO  TO SS32D
02388          MOVE CM-LF-CANCEL-DT    TO DC-BIN-DATE-1
02389
02390      ELSE
02391          MOVE CM-AH-BENEFIT-CD   TO W-BEN-HOLD
02392          MOVE CM-AH-ORIG-TERM    TO SS29D
02393          MOVE CM-AH-BENEFIT-AMT  TO SS30D
02394          MOVE CM-POLICY-FORM-NO  TO SS32D
02395          MOVE CM-AH-CANCEL-DT    TO DC-BIN-DATE-1.
02396
02397      MOVE ZEROS                  TO W-WORK-AMOUNT.
02398      COMPUTE W-WORK-AMOUNT =
02399         (CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT).
02400      MOVE W-WORK-AMOUNT          TO SS51D.
02401
02402      MOVE SPACES                 TO DC-OPTION-CODE.
02403      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02404      IF  NO-CONVERSION-ERROR
02405          MOVE DC-GREG-DATE-1-EDIT TO SS31D.
02406
02407      MOVE ' '                    TO DC-OPTION-CODE.
02408      MOVE +0                     TO DC-ELAPSED-MONTHS
02409                                           DC-ELAPSED-DAYS.
02410
100518     IF  CL-CLAIM-TYPE EQUAL     TO PI-LIFE-OVERRIDE-L1 OR 'O'
02412          MOVE CM-LF-LOAN-EXPIRE-DT
02413                                  TO DC-BIN-DATE-1
02414          PERFORM 9700-DATE-LINK THRU 9700-EXIT
02415
02416          IF  NO-CONVERSION-ERROR
02417              MOVE DC-GREG-DATE-1-EDIT TO SS28D
02418
02419          ELSE
02420              NEXT SENTENCE
02421
02422      ELSE
02423          MOVE CM-AH-LOAN-EXPIRE-DT
02424                                  TO DC-BIN-DATE-1
02425          PERFORM 9700-DATE-LINK THRU 9700-EXIT
02426
02427          IF  NO-CONVERSION-ERROR
02428              MOVE DC-GREG-DATE-1-EDIT
02429                                  TO SS28D.
02430
02431      GO TO 7015-READ-DIAGNOSIS-TRAILER.
02432
02433      EJECT
02434  7010-READ-POLICY-RECORD.
02435
02436      
      * EXEC CICS HANDLE CONDITION
02437 *         NOTOPEN    (7026-PLCY-NOT-OPEN)
02438 *         NOTFND     (7028-PLCY-NOT-FOUND)
02439 *    END-EXEC.
      *    MOVE '"$JI                  ! 0 #00008262' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303038323632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02440
02441      
      * EXEC CICS READ
02442 *         DATASET  (W-PLCY-ID)
02443 *         SET      (ADDRESS OF POLICY-MASTER)
02444 *         RIDFLD   (W-PLCY-KEY)
02445 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008267' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PLCY-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PLCY-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF POLICY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02446
02447 *    SERVICE RELOAD POLICY-MASTER.
02448
02449      MOVE PM-REFERENCE-NUMBER    TO SS59D.
02450      MOVE PM-INSURED-ISSUE-AGE   TO SS33D.
02451      MOVE PM-LOAN-BALC           TO SS37D.
02452      MOVE PM-CARRIER             TO SS23D.
02453      MOVE PM-GROUPING            TO SS24D.
02454      MOVE PM-PRODUCER            TO SS25D.
02455      MOVE PM-LOAN-NUMBER         TO SS36-1D.
02456
02457      MOVE PM-POLICY-EFF-DT       TO DC-BIN-DATE-1.
02458      MOVE SPACES                 TO DC-OPTION-CODE.
02459      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02460
02461      IF NO-CONVERSION-ERROR
02462          MOVE DC-GREG-DATE-1-EDIT TO SS27D.
02463
02464      MOVE PM-INSURED-FIRST-NAME  TO W-FIRST-NAME.
02465      MOVE PM-INSURED-LAST-NAME   TO W-LAST-NAME.
02466      MOVE PM-INSURED-MIDDLE-INIT TO W-MIDDLE-NAME.
02467      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
02468
02469      IF LOWER-CASE-LETTERS-USED
02470          MOVE W-NAME-WORK        TO W-TEMP-AREA2
02471          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
02472          MOVE W-TEMP-AREA2       TO SS57D
02473      ELSE
02474          MOVE W-NAME-WORK        TO SS57D.
02475
02476      MOVE PM-JOINT-FIRST-NAME    TO W-FIRST-NAME.
02477      MOVE PM-JOINT-LAST-NAME     TO W-LAST-NAME.
02478      MOVE PM-JOINT-MIDDLE-INIT   TO W-MIDDLE-NAME.
02479      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.
02480
02481      IF LOWER-CASE-LETTERS-USED
02482          MOVE W-NAME-WORK        TO W-TEMP-AREA2
02483          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
02484          MOVE W-TEMP-AREA2       TO SS58D
02485      ELSE
02486          MOVE W-NAME-WORK        TO SS58D.
02487
02488      MOVE PM-INS-PLAN-CD         TO W-BEN-HOLD.
02489      MOVE PM-LOAN-TERM           TO SS29D.
02490      MOVE PM-INS-POLICY-FORM     TO SS32D.
02491
02492      IF PM-AH-MORT-PLAN
02493          MOVE PM-INS-MONTH-BENEFIT TO SS30D
02494          MOVE ZEROS                TO W-WORK-AMOUNT
02495          COMPUTE W-WORK-AMOUNT =
02496              (PM-INS-MONTH-BENEFIT * PM-LOAN-TERM)
02497          MOVE W-WORK-AMOUNT        TO SS51D
02498      ELSE
02499          MOVE PM-INS-TOTAL-BENEFIT TO SS30D.
02500
02501      MOVE PM-CANCEL-DT           TO DC-BIN-DATE-1.
02502      MOVE SPACES                 TO DC-OPTION-CODE.
02503      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02504
02505      IF NO-CONVERSION-ERROR
02506          MOVE DC-GREG-DATE-1-EDIT TO SS31D.
02507
02508      MOVE PM-INS-TERMINATION-DT  TO DC-BIN-DATE-1.
02509      MOVE SPACES                 TO DC-OPTION-CODE.
02510      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
02511
02512      IF NO-CONVERSION-ERROR
02513          MOVE DC-GREG-DATE-1-EDIT TO SS28D.
02514
02515      EJECT
02516  7015-READ-DIAGNOSIS-TRAILER.
02517      
      * EXEC CICS HANDLE CONDITION
02518 *         NOTOPEN    (7020-ACTV-NOT-OPEN)
02519 *         NOTFND     (7040-READ-BENEFICIARY)
02520 *    END-EXEC.
      *    MOVE '"$JI                  ! 1 #00008343' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303038333433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02521
02522      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-KEY.
02523      MOVE +90                    TO W-ACTV-SEQ.
02524
02525      
      * EXEC CICS READ
02526 *         DATASET  (W-ACTV-ID)
02527 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
02528 *         RIDFLD   (W-ACTV-KEY)
02529 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008351' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038333531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02530
02531 *    SERVICE RELOAD ACTIVITY-TRAILERS
02532
02533      IF  AT-TRAILER-TYPE EQUAL '6'
02534          MOVE AT-INFO-LINE-1     TO SS19D.
02535
02536      GO TO 7040-READ-BENEFICIARY.
02537                                  EJECT
02538  7020-ACTV-NOT-OPEN.
02539
02540      MOVE ER-0172                TO W-CURRENT-ERROR.
02541      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02542      GO TO 7399-EXIT.
02543
02544  7022-CERT-NOT-OPEN.
02545
02546      MOVE ER-0169                TO W-CURRENT-ERROR.
02547      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02548      GO TO 7399-EXIT.
02549
02550  7024-CERT-NOT-FOUND.
02551
02552      MOVE ER-0206                TO W-CURRENT-ERROR.
02553      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02554      GO TO 7399-EXIT.
02555
02556  7026-PLCY-NOT-OPEN.
02557
02558      MOVE ER-9883                TO W-CURRENT-ERROR.
02559      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02560      GO TO 7399-EXIT.
02561
02562  7028-PLCY-NOT-FOUND.
02563
02564      MOVE ER-9483                TO W-CURRENT-ERROR.
02565      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
02566      GO TO 7399-EXIT.
02567                                  EJECT
02568
02569  7040-READ-BENEFICIARY.
02570
02571      IF  CL-BENIF-ADDR-CNT EQUAL +0
02572              AND
02573          CL-BENEFICIARY = SPACES
02574          GO TO 7100-READ-PHYSICIAN-ADDR.
02575
02576      MOVE CL-BENIF-ADDR-CNT      TO W-ACTV-SEQ.
02577      ADD +10                     TO W-ACTV-SEQ.
02578
02579      
      * EXEC CICS HANDLE CONDITION
02580 *         NOTOPEN    (7380-BENE-NOT-OPEN)
02581 *         NOTFND     (7100-READ-PHYSICIAN-ADDR)
02582 *    END-EXEC.
      *    MOVE '"$JI                  ! 2 #00008405' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303038343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02583
02584      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02585
02586      IF  W-ACTV-SEQ NOT EQUAL +10
02587          GO TO 7060-GET-FROM-ACTIVITY.
02588
02589      MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.
02590      MOVE 'B'                    TO W-BENE-REC-TYPE.
02591      MOVE CL-BENEFICIARY         TO W-BENE-NUMBER.
02592
02593      
      * EXEC CICS READ
02594 *         DATASET    (W-BENE-ID)
02595 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
02596 *         RIDFLD     (W-BENE-KEY)
02597 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008419' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02598
02599 *    SERVICE RELOAD BENEFICIARY-MASTER.
02600
02601      MOVE BE-MAIL-TO-NAME        TO W-LABEL-LINES (01).
02602      MOVE BE-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02603      MOVE BE-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02604      MOVE BE-CITY-STATE          TO W-LABEL-LINES (04).
02605
02606      MOVE SPACES                 TO W-ZIP-CODE.
02607
02608      IF  BE-CANADIAN-POST-CODE
02609          MOVE BE-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02610          MOVE BE-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02611
02612      ELSE
02613          MOVE BE-ZIP-PRIME       TO W-AM-ZIP-CODE
02614
02615          IF  BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02616              MOVE '-'            TO W-AM-ZIP-DASH
02617              MOVE BE-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02618
02619      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02620
02621      MOVE BE-PHONE-NO            TO W-PHONE-IN.
02622
02623      GO TO 7080-SET-PHONE.
02624
02625  7060-GET-FROM-ACTIVITY.
02626
02627      
      * EXEC CICS HANDLE CONDITION
02628 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
02629 *         NOTFND     (7100-READ-PHYSICIAN-ADDR)
02630 *    END-EXEC.
      *    MOVE '"$JI                  ! 3 #00008453' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303038343533' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02631
02632      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02633
02634      
      * EXEC CICS READ
02635 *         DATASET  (W-ACTV-ID)
02636 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
02637 *         RIDFLD   (W-ACTV-KEY)
02638 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008460' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02639
02640 *    SERVICE RELOAD ACTIVITY-TRAILERS.
02641
02642      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
02643      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02644      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02645      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
02646
02647      MOVE SPACES                 TO W-ZIP-CODE.
02648
02649      IF  AT-CANADIAN-POST-CODE
02650          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02651          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02652
02653      ELSE
02654          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
02655
02656          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02657              MOVE '-'            TO W-AM-ZIP-DASH
02658              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02659
02660      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02661
02662      MOVE AT-PHONE-NO            TO W-PHONE-IN.
02663
02664  7080-SET-PHONE.
02665
02666      MOVE W-PI-AREA              TO W-PO-AREA.
02667      MOVE W-PI-PFX               TO W-PO-PFX.
02668      MOVE W-PI-SFX               TO W-PO-SFX.
02669      MOVE W-PHONE-OUT            TO SS44-5D.
02670
02671      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
02672
02673      MOVE W-LABEL-LINES (01)     TO SS43D.
02674      MOVE W-LABEL-LINES (02)     TO SS44-1D.
02675      MOVE W-LABEL-LINES (03)     TO SS44-2D.
02676      MOVE W-LABEL-LINES (04)     TO SS44-3D.
02677      MOVE W-LABEL-LINES (05)     TO SS44-4D.
02678                                  EJECT
02679  7100-READ-PHYSICIAN-ADDR.
02680
02681      MOVE CL-DOCTOR-ADDR-CNT     TO W-ACTV-SEQ.
02682      ADD +30                     TO W-ACTV-SEQ.
02683
02684      IF  W-ACTV-SEQ EQUAL +30
02685          GO TO 7120-READ-EMPLOYER-ADDR.
02686
02687      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02688
02689      
      * EXEC CICS HANDLE CONDITION
02690 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
02691 *         NOTFND     (7120-READ-EMPLOYER-ADDR)
02692 *    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00008515' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303038353135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02693
02694      
      * EXEC CICS READ
02695 *         DATASET    (W-ACTV-ID)
02696 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
02697 *         RIDFLD     (W-ACTV-KEY)
02698 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008520' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353230' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02699
02700 *    SERVICE RELOAD ACTIVITY-TRAILERS.
02701
02702      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02703      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
02704      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02705      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02706      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
02707
02708      MOVE SPACES                 TO W-ZIP-CODE.
02709
02710      IF  AT-CANADIAN-POST-CODE
02711          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02712          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02713
02714      ELSE
02715          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
02716
02717          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02718              MOVE '-'            TO W-AM-ZIP-DASH
02719              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02720
02721      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02722
02723      MOVE ZEROS                  TO W-PHONE-IN.
02724      MOVE AT-PHONE-NO            TO W-PHONE-IN.
02725      MOVE W-PI-AREA              TO W-PO-AREA.
02726      MOVE W-PI-PFX               TO W-PO-PFX.
02727      MOVE W-PI-SFX               TO W-PO-SFX.
02728      MOVE W-PHONE-OUT            TO SS47-5D.
02729
02730      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
02731
02732      MOVE W-LABEL-LINES (01)     TO SS47D.
02733      MOVE W-LABEL-LINES (02)     TO SS47-1D.
02734      MOVE W-LABEL-LINES (03)     TO SS47-2D.
02735      MOVE W-LABEL-LINES (04)     TO SS47-3D.
02736      MOVE W-LABEL-LINES (05)     TO SS47-4D.
02737      GO TO 7120-READ-EMPLOYER-ADDR.
02738                                  EJECT
02739
02740  7120-READ-EMPLOYER-ADDR.
02741
02742      MOVE CL-EMPLOYER-ADDR-CNT   TO W-ACTV-SEQ.
02743      ADD +40                     TO W-ACTV-SEQ.
02744
02745      IF  W-ACTV-SEQ EQUAL +40
02746          GO TO 7140-READ-INSURED-ADDR.
02747
02748      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02749
02750      
      * EXEC CICS HANDLE CONDITION
02751 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
02752 *         NOTFND     (7140-READ-INSURED-ADDR)
02753 *    END-EXEC.
      *    MOVE '"$JI                  ! 5 #00008576' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303038353736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02754
02755      
      * EXEC CICS READ
02756 *         DATASET    (W-ACTV-ID)
02757 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
02758 *         RIDFLD     (W-ACTV-KEY)
02759 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008581' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02760
02761 *    SERVICE RELOAD ACTIVITY-TRAILERS.
02762
02763      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02764      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
02765      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02766      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02767      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
02768
02769      MOVE SPACES                 TO W-ZIP-CODE.
02770
02771      IF  AT-CANADIAN-POST-CODE
02772          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02773          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02774
02775      ELSE
02776          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
02777
02778          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02779              MOVE '-'            TO W-AM-ZIP-DASH
02780              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02781
02782      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02783
02784      MOVE ZEROS                  TO W-PHONE-IN.
02785      MOVE AT-PHONE-NO            TO W-PHONE-IN.
02786      MOVE W-PI-AREA              TO W-PO-AREA.
02787      MOVE W-PI-PFX               TO W-PO-PFX.
02788      MOVE W-PI-SFX               TO W-PO-SFX.
02789      MOVE W-PHONE-OUT            TO SS48-5D.
02790
02791      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
02792
02793      MOVE W-LABEL-LINES (01)     TO SS48D.
02794      MOVE W-LABEL-LINES (02)     TO SS48-1D.
02795      MOVE W-LABEL-LINES (03)     TO SS48-2D.
02796      MOVE W-LABEL-LINES (04)     TO SS48-3D.
02797      MOVE W-LABEL-LINES (05)     TO SS48-4D.
02798
02799                                  EJECT
02800  7140-READ-INSURED-ADDR.
02801
02802      MOVE CL-INSURED-ADDR-CNT    TO W-ACTV-SEQ.
02803
02804      IF  W-ACTV-SEQ EQUAL +0
02805          GO TO 7160-READ-OTHER1-ADDR.
02806
02807      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02808
02809      
      * EXEC CICS HANDLE CONDITION
02810 *         NOTOPEN   (7390-ACTV-NOT-OPEN)
02811 *         NOTFND    (7160-READ-OTHER1-ADDR)
02812 *         END-EXEC.
      *    MOVE '"$JI                  ! 6 #00008635' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303038363335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02813
02814      
      * EXEC CICS READ
02815 *         DATASET   (W-ACTV-ID)
02816 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
02817 *         RIDFLD    (W-ACTV-KEY)
02818 *         END-EXEC.
      *    MOVE '&"S        E          (   #00008640' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02819
02820 *    SERVICE RELOAD ACTIVITY-TRAILERS.
02821
02822      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02823      MOVE SS10D                  TO W-LABEL-LINES (01).
02824      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02825      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02826      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
02827
02828      MOVE SPACES                 TO W-ZIP-CODE.
02829
02830      IF  AT-CANADIAN-POST-CODE
02831          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02832          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02833
02834      ELSE
02835          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
02836
02837          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02838              MOVE '-'            TO W-AM-ZIP-DASH
02839              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02840
02841      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02842
02843      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
02844
02845      MOVE W-LABEL-LINES (01)     TO SS10D.
02846      MOVE W-LABEL-LINES (02)     TO SS11-1D.
02847      MOVE W-LABEL-LINES (03)     TO SS11-2D.
02848      MOVE W-LABEL-LINES (04)     TO SS11-3D.
02849      MOVE W-LABEL-LINES (05)     TO SS11-4D.
02850      MOVE AT-MAIL-TO-NAME        TO SS11-5D.
02851
02852      MOVE ZEROS                  TO W-PHONE-IN.
02853      MOVE AT-PHONE-NO            TO W-PHONE-IN.
02854      MOVE W-PI-AREA              TO W-PO-AREA.
02855      MOVE W-PI-PFX               TO W-PO-PFX.
02856      MOVE W-PI-SFX               TO W-PO-SFX.
02857      MOVE W-PHONE-OUT            TO SS11-6D.
02858
02859                                  EJECT
02860  7160-READ-OTHER1-ADDR.
02861
02862      MOVE CL-OTHER-1-ADDR-CNT    TO W-ACTV-SEQ.
02863      ADD +50                     TO W-ACTV-SEQ.
02864
02865      IF  W-ACTV-SEQ EQUAL +50
02866          GO TO 7180-READ-OTHER2-ADDR.
02867
02868      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02869
02870      
      * EXEC CICS HANDLE CONDITION
02871 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
02872 *         NOTFND     (7180-READ-OTHER2-ADDR)
02873 *         END-EXEC.
      *    MOVE '"$JI                  ! 7 #00008696' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303038363936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02874
02875      
      * EXEC CICS READ
02876 *         DATASET    (W-ACTV-ID)
02877 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
02878 *         RIDFLD     (W-ACTV-KEY)
02879 *         END-EXEC.
      *    MOVE '&"S        E          (   #00008701' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02880
02881 *    SERVICE RELOAD ACTIVITY-TRAILERS.
02882
02883      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02884      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
02885      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02886      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02887      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
02888
02889      MOVE SPACES                 TO W-ZIP-CODE.
02890
02891      IF  AT-CANADIAN-POST-CODE
02892          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02893          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02894
02895      ELSE
02896          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
02897
02898          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02899              MOVE '-'            TO W-AM-ZIP-DASH
02900              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02901
02902      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02903
02904      MOVE ZEROS                  TO W-PHONE-IN.
02905      MOVE AT-PHONE-NO            TO W-PHONE-IN.
02906      MOVE W-PI-AREA              TO W-PO-AREA.
02907      MOVE W-PI-PFX               TO W-PO-PFX.
02908      MOVE W-PI-SFX               TO W-PO-SFX.
02909      MOVE W-PHONE-OUT            TO SS49-5D.
02910
02911      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
02912
02913      MOVE W-LABEL-LINES (01)     TO SS49D.
02914      MOVE W-LABEL-LINES (02)     TO SS49-1D.
02915      MOVE W-LABEL-LINES (03)     TO SS49-2D.
02916      MOVE W-LABEL-LINES (04)     TO SS49-3D.
02917      MOVE W-LABEL-LINES (05)     TO SS49-4D.
02918                                  EJECT
02919  7180-READ-OTHER2-ADDR.
02920
02921      MOVE CL-OTHER-2-ADDR-CNT    TO W-ACTV-SEQ.
02922      ADD +60                     TO W-ACTV-SEQ.
02923
02924      IF  W-ACTV-SEQ EQUAL +60
02925          GO TO 7200-NOT-FOUND.
02926
02927      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02928
02929      
      * EXEC CICS HANDLE CONDITION
02930 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
02931 *         NOTFND     (7200-NOT-FOUND)
02932 *         END-EXEC.
      *    MOVE '"$JI                  ! 8 #00008755' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303038373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02933
02934      
      * EXEC CICS READ
02935 *         DATASET    (W-ACTV-ID)
02936 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
02937 *         RIDFLD     (W-ACTV-KEY)
02938 *         END-EXEC.
      *    MOVE '&"S        E          (   #00008760' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02939
02940 *    SERVICE RELOAD ACTIVITY-TRAILERS.
02941
02942      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
02943      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
02944      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
02945      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
02946      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
02947
02948      MOVE SPACES                 TO W-ZIP-CODE.
02949
02950      IF  AT-CANADIAN-POST-CODE
02951          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
02952          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
02953
02954      ELSE
02955          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
02956
02957          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
02958              MOVE '-'            TO W-AM-ZIP-DASH
02959              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
02960
02961      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
02962
02963      MOVE ZEROS                  TO W-PHONE-IN.
02964      MOVE AT-PHONE-NO            TO W-PHONE-IN.
02965      MOVE W-PI-AREA              TO W-PO-AREA.
02966      MOVE W-PI-PFX               TO W-PO-PFX.
02967      MOVE W-PI-SFX               TO W-PO-SFX.
02968      MOVE W-PHONE-OUT            TO SS50-5D.
02969
02970      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
02971
02972      MOVE W-LABEL-LINES (01)     TO SS50D.
02973      MOVE W-LABEL-LINES (02)     TO SS50-1D.
02974      MOVE W-LABEL-LINES (03)     TO SS50-2D.
02975      MOVE W-LABEL-LINES (04)     TO SS50-3D.
02976      MOVE W-LABEL-LINES (05)     TO SS50-4D.
02977                                  EJECT
02978  7200-NOT-FOUND.
02979
02980      IF  ACCOUNT-IS-ONLINE
02981          GO TO 7220-READ-ACCOUNT.
02982
02983      MOVE CL-ACCOUNT-ADDR-CNT    TO W-ACTV-SEQ.
02984      ADD +20                     TO W-ACTV-SEQ.
02985
02986      IF  W-ACTV-SEQ EQUAL +20
02987          GO TO 7220-READ-ACCOUNT.
02988
02989      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
02990
02991      
      * EXEC CICS HANDLE CONDITION
02992 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
02993 *         NOTFND     (7220-READ-ACCOUNT)
02994 *         END-EXEC.
      *    MOVE '"$JI                  ! 9 #00008817' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303038383137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02995
02996      
      * EXEC CICS READ
02997 *         DATASET    (W-ACTV-ID)
02998 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
02999 *         RIDFLD     (W-ACTV-KEY)
03000 *         END-EXEC.
      *    MOVE '&"S        E          (   #00008822' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03001
03002 *    SERVICE RELOAD ACTIVITY-TRAILERS.
03003
03004      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03005      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
03006      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
03007      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
03008      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
03009
03010      MOVE SPACES                 TO W-ZIP-CODE.
03011
03012      IF  AT-CANADIAN-POST-CODE
03013          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
03014          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
03015
03016      ELSE
03017          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
03018
03019          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
03020              MOVE '-'            TO W-AM-ZIP-DASH
03021              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
03022
03023      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
03024
03025      MOVE ZEROS                  TO W-PHONE-IN.
03026      MOVE AT-PHONE-NO            TO W-PHONE-IN.
03027      MOVE W-PI-AREA              TO W-PO-AREA.
03028      MOVE W-PI-PFX               TO W-PO-PFX.
03029      MOVE W-PI-SFX               TO W-PO-SFX.
03030      MOVE W-PHONE-OUT            TO SS07-6D.
03031
03032      IF  PI-COMPANY-ID EQUAL     TO 'FLA'
03033          NEXT SENTENCE
03034
03035      ELSE
03036          PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03037
03038      MOVE W-LABEL-LINES (01)     TO SS06D.
03039      MOVE W-LABEL-LINES (02)     TO SS07-1D.
03040      MOVE W-LABEL-LINES (03)     TO SS07-2D.
03041      MOVE W-LABEL-LINES (04)     TO SS07-3D.
03042      MOVE W-LABEL-LINES (05)     TO SS07-4D.
03043      MOVE W-LABEL-LINES (06)     TO SS07-5D.
03044                                  EJECT
03045  7220-READ-ACCOUNT.
03046
03047      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
03048          GO TO 7230-READ-PRODUCER.
03049
03050      IF  W-ACCT-READ-SW EQUAL 'Y'
03051          GO TO 7228-BUILD-ACCT-ADDR.
03052
03053      MOVE CM-CERT-EFF-DT         TO W-ACCT-EXP-DATE.
03054
03055      
      * EXEC CICS HANDLE CONDITION
03056 *         NOTOPEN   (7375-ACCT-NOT-OPEN)
03057 *         NOTFND    (7240-READ-3RD-PARTY)
03058 *         END-EXEC.
      *    MOVE '"$JI                  ! : #00008881' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303038383831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03059
03060  7222-STARTBR-ACCOUNT.
03061
03062      MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.
03063      PERFORM 8000-STARTBR-ERACCT THRU 8000-EXIT.
03064
03065  7226-READNEXT-ACCOUNT.
03066
03067      PERFORM 8010-READNEXT-ERACCT THRU 8010-EXIT.
03068
03069      IF  W-ACCT-PARTIAL-KEY IS NOT EQUAL W-ACCT-SAVE-KEY
03070
03071          IF  W-SAVE-ACCT-RECORD EQUAL SPACES
03072              GO TO 7240-READ-3RD-PARTY
03073
03074          ELSE
03075              MOVE AM-CONTROL-PRIMARY  TO W-ACCT-KEY
03076              MOVE W-SAVE-ACCT-RECORD TO ACCOUNT-MASTER
03077              GO TO 7228-BUILD-ACCT-ADDR.
03078
03079      IF  AM-EXPIRATION-DT EQUAL HIGH-VALUES
03080          MOVE AM-CONTROL-PRIMARY TO W-ACCT-KEY
03081
03082      ELSE
03083          MOVE ACCOUNT-MASTER     TO W-SAVE-ACCT-RECORD
03084          GO TO 7226-READNEXT-ACCOUNT.
03085
03086  7228-BUILD-ACCT-ADDR.
03087
03088      MOVE SPACES                 TO W-SAVE-ACCT-RECORD.
03089
03090      IF  NOT ACCOUNT-IS-ONLINE
03091          GO TO 7240-READ-3RD-PARTY.
03092
03093      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03094      MOVE AM-NAME                TO W-LABEL-LINES (01).
03095      MOVE AM-PERSON              TO W-LABEL-LINES (02).
03096      MOVE AM-ADDRS               TO W-LABEL-LINES (03).
03097      MOVE AM-CITY                TO W-LABEL-LINES (04).
03098
03099      MOVE SPACES                 TO W-ZIP-CODE.
03100
03101      IF  AM-CANADIAN-POST-CODE
03102          MOVE AM-CAN-POSTAL-1    TO W-CAN-POSTAL-1
03103          MOVE AM-CAN-POSTAL-2    TO W-CAN-POSTAL-2
03104
03105      ELSE
03106          MOVE AM-ZIP-PRIME       TO W-AM-ZIP-CODE
03107
03108          IF  AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS
03109              MOVE '-'            TO W-AM-ZIP-DASH
03110              MOVE AM-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
03111
03112      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
03113
03114      MOVE ZEROS                  TO W-PHONE-IN.
03115      MOVE AM-AREA-CODE           TO W-PO-AREA.
03116      MOVE AM-TEL-PRE             TO W-PO-PFX.
03117      MOVE AM-TEL-NBR             TO W-PO-SFX.
03118      MOVE W-PHONE-OUT            TO SS07-6D.
03119
03120      IF  PI-COMPANY-ID EQUAL     TO 'FLA'
03121          NEXT SENTENCE
03122
03123      ELSE
03124          PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03125
03126      MOVE W-LABEL-LINES (01)     TO SS06D.
03127      MOVE W-LABEL-LINES (02)     TO SS07-1D.
03128      MOVE W-LABEL-LINES (03)     TO SS07-2D.
03129      MOVE W-LABEL-LINES (04)     TO SS07-3D.
03130      MOVE W-LABEL-LINES (05)     TO SS07-4D.
03131      MOVE W-LABEL-LINES (06)     TO SS07-5D.
03132
03133      GO TO 7240-READ-3RD-PARTY.
03134                                  EJECT
03135  7230-READ-PRODUCER.
03136
03137      IF W-PROD-READ-SW IS EQUAL TO 'Y'
03138          GO TO 7238-BUILD-PROD-ADDR.
03139
03140      MOVE PM-POLICY-EFF-DT       TO W-PROD-EXP-DATE.
03141
03142      
      * EXEC CICS HANDLE CONDITION
03143 *         NOTOPEN   (7395-PROD-NOT-OPEN)
03144 *         NOTFND    (7300-READ-DENIAL)
03145 *    END-EXEC.
      *    MOVE '"$JI                  ! ; #00008968' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303038393638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03146
03147  7232-STARTBR-PRODUCER.
03148
03149      MOVE W-PROD-PARTIAL-KEY     TO W-PROD-SAVE-KEY.
03150      PERFORM 8050-STARTBR-EMPROD THRU 8050-EXIT.
03151
03152  7236-READNEXT-PRODUCER.
03153
03154      PERFORM 8060-READNEXT-EMPROD THRU 8060-EXIT.
03155
03156      IF W-PROD-PARTIAL-KEY IS NOT EQUAL W-PROD-SAVE-KEY
03157          IF W-SAVE-PROD-RECORD IS EQUAL TO SPACES
03158              GO TO 7300-READ-DENIAL
03159          ELSE
03160              MOVE PD-CONTROL-PRIMARY TO W-PROD-KEY
03161              MOVE W-SAVE-PROD-RECORD TO PRODUCER-MASTER
03162              GO TO 7238-BUILD-PROD-ADDR.
03163
03164      IF PD-EXPIRE-DATE IS EQUAL TO HIGH-VALUES
03165          MOVE PD-CONTROL-PRIMARY TO W-PROD-KEY
03166      ELSE
03167          MOVE PRODUCER-MASTER    TO W-SAVE-PROD-RECORD
03168          GO TO 7236-READNEXT-PRODUCER.
03169
03170  7238-BUILD-PROD-ADDR.
03171
03172      MOVE SPACES                 TO W-SAVE-PROD-RECORD.
03173
03174      IF NOT ACCOUNT-IS-ONLINE
03175          GO TO 7300-READ-DENIAL.
03176
03177      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03178      MOVE PD-NAME                TO W-LABEL-LINES (01).
03179      MOVE PD-PERSON              TO W-LABEL-LINES (02).
03180      MOVE PD-ADDRS               TO W-LABEL-LINES (03).
03181      MOVE PD-CITY                TO W-LABEL-LINES (04).
03182
03183      MOVE SPACES                 TO W-ZIP-CODE.
03184
03185      MOVE PD-ZIP-PRIME           TO W-AM-ZIP-CODE.
03186      IF (PD-ZIP-PLUS4 IS NOT EQUAL TO SPACES AND ZEROS)
03187          MOVE '-'                TO W-AM-ZIP-DASH
03188          MOVE PD-ZIP-PLUS4       TO W-AM-ZIP-PLUS4.
03189
03190      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
03191
03192      MOVE ZEROS                  TO W-PHONE-IN.
03193      MOVE PD-AREA-CODE           TO W-PO-AREA.
03194      MOVE PD-TEL-PRE             TO W-PO-PFX.
03195      MOVE PD-TEL-NBR             TO W-PO-SFX.
03196      MOVE W-PHONE-OUT            TO SS07-6D.
03197
03198      IF PI-COMPANY-ID IS EQUAL TO 'FLA'
03199          NEXT SENTENCE
03200      ELSE
03201          PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03202
03203      MOVE W-LABEL-LINES (01)     TO SS06D.
03204      MOVE W-LABEL-LINES (02)     TO SS07-1D.
03205      MOVE W-LABEL-LINES (03)     TO SS07-2D.
03206      MOVE W-LABEL-LINES (04)     TO SS07-3D.
03207      MOVE W-LABEL-LINES (05)     TO SS07-4D.
03208      MOVE W-LABEL-LINES (06)     TO SS07-5D.
03209
03210      GO TO 7300-READ-DENIAL.
03211                                  EJECT
03212  7240-READ-3RD-PARTY.
03213
03214      MOVE +29                    TO W-ACTV-SEQ.
03215
03216      
      * EXEC CICS HANDLE CONDITION
03217 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
03218 *         NOTFND     (7260-READ-COMP)
03219 *    END-EXEC.
      *    MOVE '"$JI                  ! < #00009042' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303039303432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03220
03221      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
03222
03223      
      * EXEC CICS READ
03224 *         DATASET    (W-ACTV-ID)
03225 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
03226 *         RIDFLD     (W-ACTV-KEY)
03227 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009049' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039303439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03228
03229 *    SERVICE RELOAD ACTIVITY-TRAILERS.
03230
03231      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03232      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).
03233      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).
03234      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).
03235      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).
03236
03237      MOVE SPACES                 TO W-ZIP-CODE.
03238
03239      IF  AT-CANADIAN-POST-CODE
03240          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1
03241          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2
03242
03243      ELSE
03244          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE
03245
03246          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS
03247              MOVE '-'            TO W-AM-ZIP-DASH
03248              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
03249
03250      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
03251
03252      MOVE ZEROS                  TO W-PHONE-IN.
03253      MOVE AT-PHONE-NO            TO W-PHONE-IN.
03254      MOVE W-PI-AREA              TO W-PO-AREA.
03255      MOVE W-PI-PFX               TO W-PO-PFX.
03256      MOVE W-PI-SFX               TO W-PO-SFX.
03257      MOVE W-PHONE-OUT            TO SS53-6D.
03258
03259      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03260
03261      MOVE W-LABEL-LINES (01)     TO SS52D.
03262      MOVE W-LABEL-LINES (02)     TO SS53-1D.
03263      MOVE W-LABEL-LINES (03)     TO SS53-2D.
03264      MOVE W-LABEL-LINES (04)     TO SS53-3D.
03265      MOVE W-LABEL-LINES (05)     TO SS53-4D.
03266      MOVE W-LABEL-LINES (06)     TO SS53-5D.
03267
03268      GO TO 7300-READ-DENIAL.
03269                                  EJECT
03270  7260-READ-COMP.
03271
03272      IF  W-COMP-READ-SW EQUAL 'Y'
03273          GO TO 7280-BUILD-COMP-ADDR.
03274
03275      IF  AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
03276          MOVE ZEROS              TO AM-3RD-PARTY-NOTIF-LEVEL
03277          GO TO 7300-READ-DENIAL.
03278
03279      IF  AM-3RD-PARTY-NOTIF-LEVEL GREATER THAN 00 AND
03280              LESS THAN 11
03281          NEXT SENTENCE
03282
03283      ELSE
03284          GO TO 7300-READ-DENIAL.
03285
03286      IF  AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS
03287          GO TO 7300-READ-DENIAL.
03288
03289      MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD
03290      MOVE AM-CARRIER             TO W-COMP-CARRIER
03291      MOVE AM-GROUPING            TO W-COMP-GROUPING
03292      MOVE 'A'                    TO W-COMP-TYPE
03293      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
03294                                  TO W-COMP-RESP-NO.
03295
03296      IF  AM-3RD-PARTY-NOTIF-LEVEL EQUAL AM-REMIT-TO
03297          IF AM-COM-TYP (AM-REMIT-TO) EQUAL 'O' OR 'P' OR
052814                                           'G' OR 'B' or 'S'
03299              MOVE 'G'            TO W-COMP-TYPE
03300              MOVE LOW-VALUES     TO W-COMP-ACCOUNT
03301          ELSE
03302              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
03303                                  TO W-COMP-ACCOUNT
03304      ELSE
03305          MOVE 'G'                TO W-COMP-TYPE
03306          MOVE LOW-VALUES         TO W-COMP-ACCOUNT.
03307
03308      IF  PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
03309          MOVE ZEROS              TO W-COMP-CARRIER.
03310
03311      IF  PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
03312          MOVE ZEROS              TO W-COMP-GROUPING.
03313
03314      
      * EXEC CICS HANDLE CONDITION
03315 *         NOTFND    (7300-READ-DENIAL)
03316 *    END-EXEC.
      *    MOVE '"$I                   ! = #00009140' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303039313430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03317
03318      
      * EXEC CICS  READ
03319 *         SET      (ADDRESS OF COMPENSATION-MASTER)
03320 *         DATASET  ('ERCOMP')
03321 *         RIDFLD   (W-COMP-KEY)
03322 *    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009144' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03323
03324 *    SERVICE RELOAD COMPENSATION-MASTER.
03325
03326  7280-BUILD-COMP-ADDR.
03327
03328      MOVE SPACES                 TO W-LABEL-HOLD-AREA
03329      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)
03330
03331      IF  CO-ACCT-NAME EQUAL SPACES
03332          MOVE CO-MAIL-NAME       TO W-LABEL-LINES (01).
03333
03334      MOVE CO-ADDR-1              TO W-LABEL-LINES (02).
03335      MOVE CO-ADDR-2              TO W-LABEL-LINES (03).
03336      MOVE CO-ADDR-3              TO W-LABEL-LINES (04).
03337
03338      MOVE SPACES                 TO W-ZIP-CODE.
03339
03340      IF  CO-CANADIAN-POST-CODE
03341          MOVE CO-CAN-POSTAL-1    TO W-CAN-POSTAL-1
03342          MOVE CO-CAN-POSTAL-2    TO W-CAN-POSTAL-2
03343
03344      ELSE
03345          MOVE CO-ZIP-PRIME       TO W-AM-ZIP-CODE
03346
03347          IF  CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS
03348              MOVE '-'            TO W-AM-ZIP-DASH
03349              MOVE CO-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
03350
03351      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).
03352
03353      MOVE ZEROS                  TO W-PHONE-IN.
03354      MOVE CO-AREA-CODE           TO W-PO-AREA.
03355      MOVE CO-PREFIX              TO W-PO-PFX.
03356      MOVE CO-PHONE               TO W-PO-SFX.
03357      MOVE W-PHONE-OUT            TO SS53-6D.
03358
03359      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03360
03361      MOVE W-LABEL-LINES (01)     TO SS52D.
03362      MOVE W-LABEL-LINES (02)     TO SS53-1D.
03363      MOVE W-LABEL-LINES (03)     TO SS53-2D.
03364      MOVE W-LABEL-LINES (04)     TO SS53-3D.
03365      MOVE W-LABEL-LINES (05)     TO SS53-4D.
03366      MOVE W-LABEL-LINES (06)     TO SS53-5D.
03367                                  EJECT
03368  7300-READ-DENIAL.
03369
03370      MOVE +90                    TO W-ACTV-SEQ.
03371
03372      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.
03373
03374      
      * EXEC CICS HANDLE CONDITION
03375 *         NOTOPEN    (7390-ACTV-NOT-OPEN)
03376 *         NOTFND     (7340-READ-CNTL1)
03377 *         ENDFILE    (7340-READ-CNTL1)
03378 *         END-EXEC.
      *    MOVE '"$JI''                 ! > #00009200' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303039323030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03379
03380      
      * EXEC CICS STARTBR
03381 *         DATASET    (W-ACTV-ID)
03382 *         RIDFLD     (W-ACTV-KEY)
03383 *         GTEQ
03384 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00009206' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323036' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03385
03386      MOVE 'Y'                    TO W-ACTV-BROWSE-STARTED.
03387      MOVE W-ACTV-PARTIAL-KEY     TO W-ACTV-SAVE-KEY.
03388
03389  7304-READ-NEXT.
03390
03391      
      * EXEC CICS READNEXT
03392 *         SET     (ADDRESS OF ACTIVITY-TRAILERS)
03393 *         RIDFLD  (W-ACTV-KEY)
03394 *         DATASET (W-ACTV-ID)
03395 *         END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00009217' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03396
03397 *    SERVICE RELOAD ACTIVITY-TRAILERS.
03398
03399      IF  W-ACTV-PARTIAL-KEY NOT = W-ACTV-SAVE-KEY
03400          GO TO 7340-READ-CNTL1.
03401
03402      IF  AT-TRAILER-TYPE NOT = '8'
03403          GO TO 7304-READ-NEXT.
03404
03405      MOVE AT-DENIAL-INFO-1       TO SS35-1D.
03406      MOVE AT-DENIAL-INFO-2       TO SS35-2D.
03407                                  EJECT
03408  7340-READ-CNTL1.
03409
03410      IF  W-ACTV-BROWSE-STARTED = 'Y'
03411          MOVE 'N'                TO W-ACTV-BROWSE-STARTED
03412          
      * EXEC CICS ENDBR
03413 *             DATASET    (W-ACTV-ID)
03414 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00009238' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACTV-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03415
03416      IF  SS35-1D = ALL '*'
03417          MOVE '@@DENIAL1'        TO SS35-1D.
03418
03419      IF  SS35-2D = ALL '*'
03420          MOVE '@@DENIAL2'        TO SS35-2D.
03421
03422      MOVE '1'                    TO W-CNTL-RECORD-TYPE.
03423      MOVE ZEROS                  TO W-CNTL-SEQ.
03424
03425      
      * EXEC CICS HANDLE CONDITION
03426 *         NOTOPEN   (7385-CNTL-NOT-OPEN)
03427 *         NOTFND    (7340-READ-CNTL2)
03428 *    END-EXEC.
      *    MOVE '"$JI                  ! ? #00009251' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303039323531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03429
03430      
      * EXEC CICS READ
03431 *         DATASET   (W-CNTL-ID)
03432 *         SET       (ADDRESS OF CONTROL-FILE)
03433 *         RIDFLD    (W-CNTL-KEY)
03434 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009256' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03435
03436 *    SERVICE RELOAD CONTROL-FILE.
03437
03438      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03439      MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (01).
03440      MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (02).
03441      MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (03).
03442      MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (04).
03443      MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (05).
03444
03445      IF  CF-CL-ZIP-CODE-NUM NOT NUMERIC
03446          MOVE ZEROS              TO CF-CL-ZIP-CODE-NUM.
03447
03448      IF  CF-CL-ZIP-CODE-NUM NOT = ZEROS
03449          MOVE CF-CL-ZIP-CODE-NUM TO W-ZIP-NUMERIC
03450          MOVE W-ZIP-NONNUM       TO CF-CL-ZIP-CODE.
03451
03452      MOVE SPACES                 TO W-ZIP-CODE.
03453
03454      IF  CF-CL-CAN-POST-CODE
03455          MOVE CF-CL-CAN-POSTAL-1 TO W-CAN-POSTAL-1
03456          MOVE CF-CL-CAN-POSTAL-2 TO W-CAN-POSTAL-2
03457
03458      ELSE
03459          MOVE CF-CL-ZIP-PRIME    TO W-AM-ZIP-CODE
03460
03461          IF  CF-CL-ZIP-PLUS4 NOT = SPACES AND  ZEROS
03462              MOVE '-'            TO W-AM-ZIP-DASH
03463              MOVE CF-CL-ZIP-PLUS4
03464                                  TO W-AM-ZIP-PLUS4.
03465
03466      MOVE W-ZIP-CODE             TO W-LABEL-LINES (06).
03467
03468      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03469
03470      MOVE W-LABEL-LINES (01)     TO SS01D.
03471      MOVE W-LABEL-LINES (02)     TO SS02-1D.
03472      MOVE W-LABEL-LINES (03)     TO SS02-2D.
03473      MOVE W-LABEL-LINES (04)     TO SS02-3D.
03474      MOVE W-LABEL-LINES (05)     TO SS02-4D.
03475      MOVE W-LABEL-LINES (06)     TO SS02-5D.
03476
03477  7340-READ-CNTL2.
03478
03479      IF  PI-PROCESSOR-ID =  'LGXX'
03480          GO TO 7350-READ-CNTL4.
03481
03482      MOVE '2'                    TO W-CNTL-RECORD-TYPE.
03483      MOVE PI-PROCESSOR-ID        TO W-CNTL-GENL.
03484      MOVE ZEROS                  TO W-CNTL-SEQ.
03485
03486      
      * EXEC CICS HANDLE CONDITION
03487 *         NOTOPEN    (7385-CNTL-NOT-OPEN)
03488 *         NOTFND     (7350-READ-CNTL4)
03489 *    END-EXEC.
      *    MOVE '"$JI                  ! @ #00009312' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4020233030303039333132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03490
03491      
      * EXEC CICS READ
03492 *         DATASET    (W-CNTL-ID)
03493 *         SET        (ADDRESS OF CONTROL-FILE)
03494 *         RIDFLD     (W-CNTL-KEY)
03495 *     END-EXEC.
      *    MOVE '&"S        E          (   #00009317' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03496
03497 *    SERVICE RELOAD CONTROL-FILE.
03498
03499      MOVE CF-PROCESSOR-NAME      TO SS08D.
03500      MOVE CF-PROCESSOR-TITLE     TO SS09D.
03501                                  EJECT
03502  7350-READ-CNTL4.
03503
03504      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'
03505          GO TO 7350-READ-EMPLAN.
03506
03507      IF  W-BEN-HOLD = ZEROS
03508          GO TO 7350-READ-CNTL6.
03509
03510      MOVE W-BEN-HOLD             TO W-CNTL-GEN2.
03511      MOVE SPACES                 TO W-CNTL-GEN1.
03512
100518     IF  CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03514          MOVE '4'                TO W-CNTL-RECORD-TYPE
03515
03516      ELSE
03517          MOVE '5'                TO W-CNTL-RECORD-TYPE.
03518
03519      MOVE ZEROS                  TO W-CNTL-SEQ.
03520
03521      
      * EXEC CICS HANDLE CONDITION
03522 *         NOTOPEN    (7385-CNTL-NOT-OPEN)
03523 *         NOTFND     (7350-READ-CNTL6)
03524 *    END-EXEC.
      *    MOVE '"$JI                  ! A #00009347' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4120233030303039333437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03525
03526      
      * EXEC CICS READ
03527 *         DATASET    (W-CNTL-ID)
03528 *         SET        (ADDRESS OF CONTROL-FILE)
03529 *         RIDFLD     (W-CNTL-KEY)
03530 *         GTEQ
03531 *     END-EXEC.
      *    MOVE '&"S        G          (   #00009352' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03532
03533 *    SERVICE RELOAD CONTROL-FILE.
03534
03535      MOVE 1                      TO W-SUB.
03536
03537  7350-LOOP.
03538
03539      IF  W-SUB = 9
03540          GO TO 7350-READ-CNTL6.
03541
03542      IF  CF-BENEFIT-CODE (W-SUB) LESS THAN W-BEN-HOLD
03543          ADD 1                   TO W-SUB
03544          GO TO 7350-LOOP.
03545
03546      IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-SUB)
03547          MOVE CF-BENEFIT-DESCRIP (W-SUB) TO SS22D
03548
121802         IF  CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
080322                                               OR 'B' OR 'H'
03550              MOVE CF-BENEFIT-ALPHA (W-SUB)
03551                                  TO W-BENEFIT-WORK
03552              MOVE W-ELIM-DAYS    TO SS42D.
03553
03554      GO TO 7350-READ-CNTL6.
03555                                  EJECT
03556  7350-READ-EMPLAN.
03557
03558      IF W-BEN-HOLD IS EQUAL TO ZEROS
03559          GO TO 7350-READ-CNTL6.
03560
03561      MOVE PM-GROUPING            TO W-PLAN-GROUPING.
03562      MOVE PM-STATE               TO W-PLAN-STATE.
03563      MOVE PM-PRODUCER            TO W-PLAN-PRODUCER.
03564      MOVE PM-INS-PLAN-CD         TO W-PLAN-CODE.
03565      MOVE PM-INS-PLAN-REVISION   TO W-PLAN-REV-NO.
03566
03567      
      * EXEC CICS HANDLE CONDITION
03568 *         NOTOPEN  (7397-PLAN-NOT-OPEN)
03569 *         NOTFND   (7350-READ-CNTL6)
03570 *    END-EXEC.
      *    MOVE '"$JI                  ! B #00009394' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4220233030303039333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03571
03572      
      * EXEC CICS READ
03573 *         DATASET   (W-PLAN-ID)
03574 *         SET       (ADDRESS OF PRODUCER-PLANS)
03575 *         RIDFLD    (W-PLAN-KEY)
03576 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009399' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039333939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PLAN-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PLAN-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-PLANS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03577
03578 *    SERVICE RELOAD PRODUCER-PLANS.
03579
03580      MOVE PP-PLAN-DESCRIPTION    TO SS22D.
03581                                  EJECT
03582  7350-READ-CNTL6.
03583
03584      MOVE '6'                    TO W-CNTL-RECORD-TYPE.
03585      MOVE SPACES                 TO W-CNTL-GENL.
03586
03587      IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'
03588          MOVE CL-CURRENT-CARRIER TO W-CNTL-GEN4
03589
03590      ELSE
03591          MOVE PI-CARRIER         TO W-CNTL-GEN4.
03592
03593      MOVE ZEROS                  TO W-CNTL-SEQ.
03594
03595      
      * EXEC CICS HANDLE CONDITION
03596 *         NOTOPEN    (7385-CNTL-NOT-OPEN)
03597 *         NOTFND     (7360-SET-DATE)
03598 *    END-EXEC.
      *    MOVE '"$JI                  ! C #00009422' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4320233030303039343232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03599
03600      
      * EXEC CICS READ
03601 *         DATASET    (W-CNTL-ID)
03602 *         SET        (ADDRESS OF CONTROL-FILE)
03603 *         RIDFLD     (W-CNTL-KEY)
03604 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009427' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03605
03606 *    SERVICE RELOAD CONTROL-FILE.
03607
03608      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03609      MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (01).
03610      MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (02).
03611      MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (03).
03612      MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (04).
03613      MOVE CF-CITY-STATE          TO W-LABEL-LINES (05).
03614
03615      IF  CF-ZIP-CODE-NUM NOT NUMERIC
03616          MOVE ZEROS              TO CF-ZIP-CODE-NUM.
03617
03618      IF  CF-ZIP-CODE-NUM NOT = ZEROS
03619          MOVE CF-ZIP-CODE-NUM    TO W-ZIP-NUMERIC
03620          MOVE W-ZIP-NONNUM       TO CF-ZIP-CODE.
03621
03622      MOVE SPACES                 TO W-ZIP-CODE.
03623
03624      IF  CF-CANADIAN-POST-CODE
03625          MOVE CF-CAN-POSTAL-1    TO W-CAN-POSTAL-1
03626          MOVE CF-CAN-POSTAL-2    TO W-CAN-POSTAL-2
03627
03628      ELSE
03629          MOVE CF-ZIP-PRIME       TO W-AM-ZIP-CODE
03630
03631          IF  CF-ZIP-PLUS4 NOT = SPACES AND  ZEROS
03632              MOVE '-'            TO W-AM-ZIP-DASH
03633              MOVE CF-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.
03634
03635      MOVE W-ZIP-CODE             TO W-LABEL-LINES (06).
03636
03637      MOVE ZEROS                  TO W-PHONE-IN.
03638      MOVE CF-PHONE-NO            TO W-PHONE-IN.
03639      MOVE W-PI-AREA              TO W-PO-AREA.
03640      MOVE W-PI-PFX               TO W-PO-PFX.
03641      MOVE W-PI-SFX               TO W-PO-SFX.
03642      MOVE W-PHONE-OUT            TO SS04-6D.
03643
03644      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.
03645
03646      MOVE W-LABEL-LINES (01)     TO SS03D.
031307     IF PI-CARRIER = '8'
031307        MOVE 'as Administrator for Investors Heritage Life Insuran
031307-     'ce Company'               TO SS03-1D
031307     END-IF
03647      MOVE W-LABEL-LINES (02)     TO SS04-1D.
03648      MOVE W-LABEL-LINES (03)     TO SS04-2D.
03649      MOVE W-LABEL-LINES (04)     TO SS04-3D.
03650      MOVE W-LABEL-LINES (05)     TO SS04-4D.
03651      MOVE W-LABEL-LINES (06)     TO SS04-5D.
03652                                  EJECT
03653  7360-SET-DATE.
03654
03655      MOVE EIBDATE                TO W-DATE-WORK.
03656      MOVE W-DT-WORK              TO DC-JULIAN-YYDDD.
03657      MOVE '5'                    TO DC-OPTION-CODE.
03658      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03659      MOVE DC-GREG-DATE-1-EDIT    TO SS20D.
03660      MOVE DC-GREG-DATE-1-ALPHA   TO SS21D.
03661
03662      IF  LOWER-CASE-LETTERS-USED
03663          NEXT SENTENCE
03664
03665      ELSE
03666          GO TO 7399-EXIT.
03667
03668      INSPECT SS19D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
03669      INSPECT SS35-1D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
03670      INSPECT SS35-2D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
03671      INSPECT SS22D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
03672
03673      MOVE SS08D                  TO W-TEMP-AREA2
03674      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03675      MOVE W-TEMP-AREA2           TO SS08D
03676
03677      MOVE SS09D                  TO W-TEMP-AREA2
03678      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03679      MOVE W-TEMP-AREA2           TO SS09D
03680
03681      MOVE SS11-5D                TO W-TEMP-AREA2
03682      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03683      MOVE W-TEMP-AREA2           TO SS11-5D
03684
03685      MOVE SS21D                  TO W-TEMP-AREA2
03686      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03687      MOVE W-TEMP-AREA2           TO SS21D
03688
03689      MOVE SS40D                  TO W-TEMP-AREA2
03690      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03691      MOVE W-TEMP-AREA2           TO SS40D
03692
03693      MOVE SS41D                  TO W-TEMP-AREA2
03694      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03695      MOVE W-TEMP-AREA2           TO SS41D.
03696      GO TO 7399-EXIT.
03697                                  EJECT
03698  7370-RESOLVE-CREDITOR.
03699
03700      
      * EXEC CICS HANDLE CONDITION
03701 *         NOTOPEN    (7380-BENE-NOT-OPEN)
03702 *         NOTFND     (7370-EXIT)
03703 *    END-EXEC.
      *    MOVE '"$JI                  ! D #00009531' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4420233030303039353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03704
03705      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
03706
03707      MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.
03708      MOVE 'B'                    TO W-BENE-REC-TYPE.
03709      MOVE SPACES                 TO W-BENE-NUMBER.
03710      MOVE CL-CURRENT-GROUPING    TO W-BENE-CREDITOR.
03711
03712      
      * EXEC CICS READ
03713 *         DATASET    (W-BENE-ID)
03714 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
03715 *         RIDFLD     (W-BENE-KEY)
03716 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009543' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03717
03718 *    SERVICE RELOAD BENEFICIARY-MASTER.
03719
03720      IF  LOWER-CASE-LETTERS-USED
03721          MOVE BE-MAIL-TO-NAME    TO W-TEMP-AREA2
03722          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
03723          MOVE W-TEMP-AREA2       TO SS56D
03724
03725      ELSE
03726          MOVE BE-MAIL-TO-NAME    TO SS56D.
03727
03728  7370-EXIT.
03729      EXIT.
03730
03731  7375-ACCT-NOT-OPEN.
03732
03733      MOVE ER-0168                TO W-CURRENT-ERROR.
03734      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
03735      GO TO 7399-EXIT.
03736
03737  7380-BENE-NOT-OPEN.
03738
03739      MOVE ER-7675                TO W-CURRENT-ERROR.
03740      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
03741      GO TO 7399-EXIT.
03742
03743
03744  7385-CNTL-NOT-OPEN.
03745
03746      MOVE ER-0042                TO W-CURRENT-ERROR.
03747      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
03748      GO TO 7399-EXIT.
03749
03750  7390-ACTV-NOT-OPEN.
03751
03752      MOVE ER-0172                TO W-CURRENT-ERROR.
03753      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
03754      GO TO 7399-EXIT.
03755
03756  7395-PROD-NOT-OPEN.
03757
03758      MOVE ER-9106                TO W-CURRENT-ERROR.
03759      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
03760      GO TO 7399-EXIT.
03761
03762  7397-PLAN-NOT-OPEN.
03763
03764      MOVE ER-9808                TO W-CURRENT-ERROR.
03765      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
03766      GO TO 7399-EXIT.
03767
03768  7399-EXIT.
03769      EXIT.
03770                                  EJECT
03771  7400-MOVE-NAME.
03772
03773      MOVE SPACES                 TO W-NAME-WORK
03774                                     W-NAME-WORK2.
03775      MOVE ZERO                   TO W-NAME-SW.
03776      SET W-NWA-NDX               TO +1.
03777
03778      IF  CL-INSURED-1ST-NAME = SPACES
03779              AND
03780          CL-INSURED-MID-INIT = SPACES
03781          MOVE +1                 TO W-NAME-SW.
03782
03783      MOVE CL-INSURED-LAST-NAME   TO W-NAME-WORK2.
03784      PERFORM 7420-MOVE-NAME THRU 7429-EXIT.
03785
03786      MOVE CL-INSURED-1ST-NAME    TO W-NAME-WORK2.
03787      PERFORM 7420-MOVE-NAME THRU 7429-EXIT.
03788
03789      SET W-NWA-NDX UP BY +1.
03790      MOVE CL-INSURED-MID-INIT    TO W-NAME-WORK2.
03791      PERFORM 7420-MOVE-NAME THRU 7429-EXIT.
03792
03793  7400-EXIT.
03794      EXIT.
03795                                  EJECT
03796  7420-MOVE-NAME SECTION.
03797
03798      IF  W-NAME-SW GREATER THAN +1
03799          GO TO 7429-EXIT.
03800
03801      IF  W-NAME-WORK2 = SPACES
03802          GO TO 7429-EXIT.
03803
03804      SET W-NWA-NDX2              TO +1.
03805      SET W-NWA-NDX3              TO +2.
03806
03807  7422-MOVE-NAME.
03808
03809      MOVE W-NW2 (W-NWA-NDX2)   TO W-NW (W-NWA-NDX).
03810
03811      IF  W-NWA-NDX LESS THAN +30
03812          SET W-NWA-NDX UP BY +1
03813
03814      ELSE
03815          ADD +2                  TO W-NAME-SW
03816          GO TO 7429-EXIT.
03817
03818      IF  W-NWA-NDX2 LESS THAN +20
03819          SET W-NWA-NDX3 UP BY +1
03820          SET W-NWA-NDX2 UP BY +1.
03821
03822      IF  W-NW2 (W-NWA-NDX2) = SPACES
03823              AND
03824          W-NW2 (W-NWA-NDX3) = SPACES
03825
03826          IF  W-NAME-SW = ZERO
03827              MOVE ','            TO W-NW (W-NWA-NDX)
03828              SET W-NWA-NDX UP BY +2
03829              MOVE +1             TO W-NAME-SW
03830              GO TO 7429-EXIT
03831
03832          ELSE
03833              GO TO 7429-EXIT.
03834
03835      GO TO 7422-MOVE-NAME.
03836
03837  7429-EXIT.
03838      EXIT.
03839                                  EJECT
03840  7500-MOVE-NAME.
03841
03842      MOVE SPACES                 TO W-NAME-WORK
03843                                     W-NAME-WORK2.
03844      MOVE ZERO                   TO W-NAME-SW.
03845      SET W-NWA-NDX               TO +1.
03846
03847      IF  W-FIRST-NAME = SPACES
03848              AND
03849          W-MIDDLE-NAME = SPACES
03850          MOVE W-LAST-NAME
03851                                  TO W-NAME-WORK
03852          GO TO 7500-EXIT.
03853
03854      MOVE W-FIRST-NAME           TO W-NAME-WORK2.
03855      PERFORM 7520-MOVE-NAME THRU 7529-EXIT.
03856
03857      SET W-NWA-NDX UP BY +1.
03858
03859      IF  W-MIDDLE-NAME NOT = SPACES
03860          MOVE W-MIDDLE-NAME      TO W-NW (W-NWA-NDX)
03861          SET W-NWA-NDX UP BY +1
03862          MOVE '.'                TO W-NW (W-NWA-NDX)
03863          SET W-NWA-NDX UP BY +2.
03864
03865      MOVE W-LAST-NAME            TO W-NAME-WORK2.
03866      PERFORM 7520-MOVE-NAME THRU 7529-EXIT.
03867
03868  7500-EXIT.
03869      EXIT.
03870                                  EJECT
03871  7520-MOVE-NAME SECTION.
03872
03873      IF  W-NAME-SW GREATER THAN +1
03874          GO TO 7529-EXIT.
03875
03876      IF  W-NAME-WORK2 = SPACES
03877          GO TO 7529-EXIT.
03878
03879      SET W-NWA-NDX2              TO +1.
03880      SET W-NWA-NDX3              TO +2.
03881
03882  7522-MOVE-NAME.
03883
03884      MOVE W-NW2 (W-NWA-NDX2)     TO W-NW (W-NWA-NDX).
03885
03886      IF  W-NWA-NDX LESS THAN +30
03887          SET W-NWA-NDX UP BY +1
03888
03889      ELSE
03890          ADD +2                 TO W-NAME-SW
03891          GO TO 7529-EXIT.
03892
03893      IF  W-NWA-NDX2 LESS THAN +20
03894          SET W-NWA-NDX2 UP BY +1
03895          SET W-NWA-NDX3 UP BY +1.
03896
03897      IF  W-NW2 (W-NWA-NDX2) = SPACES
03898              AND
03899          W-NW2 (W-NWA-NDX3) = SPACES
03900          GO TO 7529-EXIT.
03901
03902      GO TO 7522-MOVE-NAME.
03903
03904  7529-EXIT.
03905      EXIT.
03906                                  EJECT
03907  7600-LABEL-MOVE.
03908
03909      IF  W-LABEL-HOLD-AREA = SPACES
03910          GO TO 7600-EXIT.
03911
03912      PERFORM 7700-TRANSLATE-LOWER THRU 7700-EXIT.
03913
03914      IF  W-LABEL-LINES (1) = SPACES
03915          MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)
03916          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
03917          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
03918          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
03919          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
03920          MOVE SPACES             TO W-LABEL-LINES (6)
03921          GO TO 7600-LABEL-MOVE.
03922
03923      IF  W-LABEL-LINES (2) = SPACES AND
03924          W-LABEL-LINES (3) = SPACES AND
03925          W-LABEL-LINES (4) = SPACES AND
03926          W-LABEL-LINES (5) = SPACES AND
03927          W-LABEL-LINES (6) = SPACES
03928          SET W-NDX               TO 1
03929          GO TO 7600-MOVE-ZIP.
03930
03931      IF  W-LABEL-LINES (2) = SPACES
03932          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
03933          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
03934          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
03935          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
03936          MOVE SPACES             TO W-LABEL-LINES (6)
03937          GO TO 7600-LABEL-MOVE.
03938
03939      IF  W-LABEL-LINES (3) = SPACES AND
03940          W-LABEL-LINES (4) = SPACES AND
03941          W-LABEL-LINES (5) = SPACES AND
03942          W-LABEL-LINES (6) = SPACES
03943          SET W-NDX               TO 2
03944          GO TO 7600-MOVE-ZIP.
03945
03946      IF  W-LABEL-LINES (3) = SPACES
03947          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
03948          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
03949          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
03950          MOVE SPACES             TO W-LABEL-LINES (6)
03951          GO TO 7600-LABEL-MOVE.
03952
03953      IF  W-LABEL-LINES (4) = SPACES AND
03954          W-LABEL-LINES (5) = SPACES AND
03955          W-LABEL-LINES (6) = SPACES
03956          SET W-NDX               TO 3
03957          GO TO 7600-MOVE-ZIP.
03958
03959      IF  W-LABEL-LINES (4) = SPACES
03960          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
03961          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
03962          MOVE SPACES             TO W-LABEL-LINES (6)
03963          GO TO 7600-LABEL-MOVE.
03964
03965      IF  W-LABEL-LINES (5) = SPACES AND
03966          W-LABEL-LINES (6) = SPACES
03967          SET W-NDX               TO 4
03968          GO TO 7600-MOVE-ZIP.
03969
03970      IF  W-LABEL-LINES (5) = SPACES
03971          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
03972          MOVE SPACES             TO W-LABEL-LINES (6)
03973          SET W-NDX               TO 5
03974          GO TO 7600-MOVE-ZIP
03975
03976      ELSE
03977          IF  W-LABEL-LINES (6) = SPACES
03978              SET W-NDX           TO 5
03979              GO TO 7600-MOVE-ZIP
03980
03981          ELSE
03982              SET W-NDX           TO 6.
03983
03984  7600-MOVE-ZIP.
03985
03986      SET W-NDX2                  TO W-NDX.
03987      SET W-NDX2 DOWN BY +1.
03988
03989      IF  W-LAST-ZIP (W-NDX2) = SPACES
03990              AND
03991          W-LAST-DIGIT (W-NDX2) EQUAL SPACES
03992 *****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
03993
03994          IF  W-LABEL-1ST-ZIP (W-NDX) NUMERIC
03995
03996              IF  PI-COMPANY-ID NOT = 'FLA'
03997                  MOVE W-LABEL-ZIP (W-NDX)
03998                                  TO W-LAST-ZIP (W-NDX2)
03999                  MOVE SPACES     TO W-LABEL-LINES (W-NDX).
04000
04001  7600-EXIT.
04002      EXIT.
04003                                  EJECT
04004  7700-TRANSLATE-LOWER.
04005
04006      IF  LOWER-CASE-LETTERS-USED
04007          NEXT SENTENCE
04008
04009      ELSE
04010          GO TO 7700-EXIT.
04011
04012      IF  W-LABEL-LINES (1) NOT = SPACES
04013          MOVE W-LABEL-LINES (1)  TO W-TEMP-AREA2
04014          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
04015          MOVE W-TEMP-AREA2       TO W-LABEL-LINES (1).
04016
04017      IF  W-LABEL-LINES (2) NOT = SPACES
04018          MOVE W-LABEL-LINES (2)  TO W-TEMP-AREA2
04019          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
04020          MOVE W-TEMP-AREA2       TO W-LABEL-LINES (2).
04021
04022      IF  W-LABEL-LINES (3) NOT = SPACES
04023          MOVE W-LABEL-LINES (3)  TO W-TEMP-AREA2
04024          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
04025          MOVE W-TEMP-AREA2       TO W-LABEL-LINES (3).
04026
04027 *****THE CITY STATE WILL BE ON LINE FOUR OR FIVE DEPENDING
04028 *****ON THE FORMAT USED.  THE SIXTH LINE BEING BLANK WILL TELL.
04029
04030      MOVE 'N'                    TO W-STATE-LINE.
04031
04032      IF  W-LABEL-LINES (4) NOT = SPACES
04033         IF  W-LABEL-LINES (6) = SPACES
04034            MOVE 'Y'              TO W-STATE-LINE
04035            MOVE W-LABEL-LINES (4)
04036                                  TO W-TEMP-AREA2
04037            PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
04038            MOVE W-TEMP-AREA2     TO W-LABEL-LINES (4).
04039
04040      IF  W-LABEL-LINES (5) NOT = SPACES
04041         IF  W-LABEL-LINES (6) NOT = SPACES
04042            MOVE 'Y'              TO W-STATE-LINE
04043            MOVE W-LABEL-LINES (5)
04044                                  TO W-TEMP-AREA2
04045            PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT
04046            MOVE W-TEMP-AREA2     TO W-LABEL-LINES (5).
04047
04048  7700-EXIT.
04049      EXIT.
04050                                  EJECT
04051
04052  7750-SEARCH-AND-TRANSLATE.
04053
04054      SET W-TA1                   TO +1.
04055
04056  7750-FIND-FIRST-NON-BLANK.
04057
04058      IF  W-TEMP-2 (W-TA1) = SPACES
04059          SET W-TA1 UP BY 1
04060          GO TO 7750-FIND-FIRST-NON-BLANK.
04061
04062 *****SET INDEX TO THE NEXT CHAR TO START THE TRANSLATE.
04063      SET W-TA2                   TO W-TA1
04064      SET W-TA2 UP BY 1
04065      SET W-TA21                  TO W-TA2
04066      SET W-TA21 UP BY 1.
04067
04068      MOVE 'N'                    TO W-DATA-FOUND-SW.
04069      PERFORM 7282-FIND-NEXT-BLANK THRU 7282-EXIT.
04070
04071  7750-EXIT.
04072      EXIT.
04073      EJECT
04074
04075  7282-FIND-NEXT-BLANK.
04076
04077      IF  W-TA21 GREATER THAN 31
04078          GO TO 7282-EXIT.
04079
04080      IF  W-TA21 EQUAL 31
04081
04082          IF  NO-CHARACTERS-FOUND
04083              GO TO 7282-EXIT
04084
04085          ELSE
04086              SET W-TA1           TO +1
04087              MOVE SPACES         TO W-TEMP-AREA1
04088
04089              PERFORM 7283-MOVE
04090                      VARYING
04091                  W-MOVE-NDX FROM W-TA2 BY 1
04092                      UNTIL
04093                  W-MOVE-NDX EQUAL W-TA21
04094
04095              PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT
04096              SET W-TA1           TO +1
04097
04098              PERFORM 7284-MOVE-BACK
04099                      VARYING
04100                  W-MOVE-NDX FROM W-TA2 BY 1
04101                      UNTIL
04102                  W-MOVE-NDX EQUAL W-TA21
04103
04104              GO TO 7282-EXIT.
04105
04106      IF  W-TEMP-2 (W-TA2) = SPACE OR ',' OR '/'
04107          GO TO 7282-NEXT-GROUP-SEARCH.
04108
04109      IF  W-TEMP-2 (W-TA21) = SPACES OR ',' OR '/'
04110          NEXT SENTENCE
04111
04112      ELSE
04113          MOVE 'Y'                TO W-DATA-FOUND-SW
04114          SET W-TA21 UP BY +1
04115          GO TO 7282-FIND-NEXT-BLANK.
04116
04117      SET W-TA1                   TO +1.
04118      MOVE SPACES                 TO W-TEMP-AREA1.
04119
04120      PERFORM 7283-MOVE
04121              VARYING
04122          W-MOVE-NDX FROM W-TA2 BY 1
04123              UNTIL
04124          W-MOVE-NDX EQUAL W-TA21.
04125
04126      PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT.
04127      SET W-TA1                   TO +1.
04128
04129      PERFORM 7284-MOVE-BACK
04130              VARYING
04131          W-MOVE-NDX FROM W-TA2 BY 1
04132              UNTIL
04133          W-MOVE-NDX EQUAL W-TA21.
04134
04135      SET W-TA2                  TO W-TA21.
04136      SET W-TA21 UP BY 1.
04137      MOVE 'N'                   TO W-DATA-FOUND-SW.
04138
04139  7282-NEXT-GROUP-SEARCH.
04140
04141      IF  W-TEMP-2 (W-TA2) EQUAL SPACES OR ',' OR '/'
04142          SET W-TA2 UP BY 1
04143
04144          IF  W-TA2 = 30
04145              GO TO 7282-EXIT
04146
04147          ELSE
04148              GO TO 7282-NEXT-GROUP-SEARCH.
04149
04150      SET W-TA2  UP BY 1.
04151      SET W-TA21                  TO W-TA2.
04152      SET W-TA21 UP BY 1.
04153      GO TO 7282-FIND-NEXT-BLANK.
04154
04155  7282-EXIT.
04156      EXIT.
04157
04158  7283-MOVE.
04159
04160      MOVE W-TEMP-2 (W-MOVE-NDX)  TO W-TEMP-1 (W-TA1).
04161      SET W-TA1 UP BY +1.
04162
04163  7284-MOVE-BACK.
04164
04165      MOVE W-TEMP-1 (W-TA1)       TO W-TEMP-2 (W-MOVE-NDX).
04166      SET W-TA1 UP BY +1.
04167
04168  7285-TEST-AND-TRANSLATE.
04169 ***BYPASS IF  THE AREA MAY BE A PO BOX, OR RR NUMBER
04170
04171      IF  W-TEMP-AREA1 = '.O.' OR 'RR'
04172          GO TO 7285-EXIT.
04173
04174 ***BYPASS IF IT IS A CITY/STATE LINE AND BEYOND CHARACTER 07
04175 ***AND IT APPEARS THAT IT MAY BE A ABREVIATION.
04176
04177      SET W-POSITION2             TO W-TA2.
04178      SET W-POSITION21            TO W-TA21.
04179      COMPUTE W-WORD-LENGTH = W-POSITION21 - W-POSITION2.
04180
04181      IF  W-WORD-LENGTH LESS THAN 3
04182              AND
04183          W-STATE-LINE = 'Y'
04184              AND
04185          W-TA2 GREATER THAN 07
04186          GO TO 7285-EXIT.
04187
04188      INSPECT W-TEMP-AREA1 CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
04189
04190  7285-EXIT.
04191      EXIT.
04192
04193  7800-VARIABLE-SEARCH.
04194 ***************************************************************
04195 *    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *
04196 *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
04197 *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
04198 *    DATA THAT WAS GENERATED IN PARAGRAPHS 7000-7299.         *
04199 *                                                             *
04200 *    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *
04201 *    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *
04202 *    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *
04203 ***************************************************************
04204
04205      MOVE W-REC-TEXT (W-TB-NDX)  TO W-SINGLE-LINE.
04206      SET NDX1                    TO 1.
04207
04208  7801-LOOP.
04209
04210      IF  NDX1 GREATER 70
04211          MOVE W-SINGLE-LINE      TO W-REC-TEXT (W-TB-NDX)
04212          GO TO 7899-EXIT.
04213
04214      IF  W-ONE-CHAR (NDX1) NOT = '@'
04215          SET NDX1 UP BY 1
04216          GO TO 7801-LOOP.
04217
04218      SET NDX2                    TO NDX1.
04219      SET NDX2 UP BY 1.
04220
04221      IF  W-ONE-CHAR (NDX2) = '@'
04222          SET NDX1 UP BY 2
04223          GO TO 7801-LOOP.
04224
04225      MOVE W-ONE-CHAR (NDX2)      TO W-V1.
04226      SET NDX2 UP BY 1.
04227      MOVE W-ONE-CHAR (NDX2)      TO W-V2.
04228      SET NDX2 UP BY 1.
04229      MOVE W-ONE-CHAR (NDX2)      TO W-V3.
04230      SET NDX2 UP BY 1.
04231      MOVE W-ONE-CHAR (NDX2)      TO W-V4.
04232
04233      IF  W-V-NUM NOT NUMERIC
04234          GO TO 7830-VAR-ERROR.
04235
04236      IF  W-V-PERIOD NOT = '.'
04237          MOVE '.'                TO W-V-PERIOD
04238          MOVE ZERO               TO W-V-DECIMAL
04239          GO TO 7840-TABLE-SEARCH.
04240
04241      IF  W-V-DECIMAL NUMERIC
04242          GO TO 7840-TABLE-SEARCH.
04243
04244  7830-VAR-ERROR.
04245
04246      MOVE ER-0180                TO W-CURRENT-ERROR.
04247      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
04248      SET NDX1 UP BY 1.
04249      GO TO 7801-LOOP.
04250
04251  7840-TABLE-SEARCH.
04252
04253      IF  W-NO-GETMAIN-DONE-YET
04254
04255          IF  PI-VAR-POINTER GREATER THAN ZEROS
04256              MOVE PI-VAR-POINTER TO LCP-WS-ADDR-COMP
04257              SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
04258 *            SERVICE RELOAD SYSTEM-VARIABLES
04259              MOVE SYSTEM-SUPPORTED-VARIABLES
04260                                  TO SYSTEM-VARIABLES
04261              MOVE 1              TO W-GETMAIN-SW
04262              MOVE 1              TO SS-COUNTER
04263
04264          ELSE
04265              
      * EXEC CICS GETMAIN
04266 *                 SET     (ADDRESS OF SYSTEM-VARIABLES)
04267 *                 LENGTH  (SS-WORK-AREA-LENGTH)
04268 *            END-EXEC
      *    MOVE '," L                  $   #00010096' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 SS-WORK-AREA-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF SYSTEM-VARIABLES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04269
04270 *            SERVICE RELOAD SYSTEM-VARIABLES
04271
04272              MOVE 1              TO W-GETMAIN-SW
04273              SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
04274              MOVE LCP-WS-ADDR-COMP
04275                                  TO PI-VAR-POINTER
04276              MOVE SYSTEM-SUPPORTED-VARIABLES
04277                                  TO SYSTEM-VARIABLES
04278              MOVE 1              TO SS-COUNTER
04279
04280      ELSE
04281          MOVE 1                  TO SS-COUNTER
04282          MOVE PI-VAR-POINTER     TO LCP-WS-ADDR-COMP
04283          SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR.
04284
04285  7850-TABLE-LOOP.
04286
04287 *    SERVICE RELOAD SYSTEM-VARIABLES.
04288
04289      IF  SS-COUNTER GREATER THAN  SS-NUM-ENTRIES
04290          GO TO 7830-VAR-ERROR.
04291
04292      IF  SYS-VAR-CODE NOT = W-VAR-HOLD
04293          SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
04294          ADD SYS-VAR-LEN         TO LCP-WS-ADDR-COMP
04295          SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
04296          ADD 1                   TO SS-COUNTER
04297          GO TO 7850-TABLE-LOOP.
04298
04299      MOVE SYS-VAR-ENTRY          TO W-VARIABLE-WORK-AREA.
04300      SET W-NDXV                  TO 1.
04301      SUBTRACT 6                  FROM W-VAR-LEN.
04302      PERFORM 7900-MOVE-VAR-DATA THRU 7900-EXIT
04303          W-VAR-LEN TIMES.
04304      GO TO 7801-LOOP.
04305
04306  7899-EXIT.
04307       EXIT.
04308
04309  7900-MOVE-VAR-DATA.
04310
04311      IF  NDX1 GREATER 70
04312          MOVE ER-0181            TO W-CURRENT-ERROR
04313          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT
04314          GO TO 7801-LOOP.
04315
04316      MOVE W-VAR-W-ONE-CHAR (W-NDXV) TO W-ONE-CHAR (NDX1).
04317      SET W-NDXV UP BY 1.
04318      SET NDX1 UP BY 1.
04319
04320  7900-EXIT.
04321       EXIT.
04322                                  EJECT
04323  8000-STARTBR-ERACCT.
04324
04325      IF  W-ACCT-READ-SW EQUAL    TO 'Y'
04326          NEXT SENTENCE
04327
04328      ELSE
04329          IF  PI-ACCT-POINTER GREATER THAN ZEROS
04330              MOVE PI-ACCT-POINTER
04331                                  TO LCP-WS-ADDR-COMP
04332              SET ADDRESS OF ACCOUNT-MASTER TO LCP-WS-ADDR-PNTR
04333 *            SERVICE RELOAD ACCOUNT-MASTER
04334
04335          ELSE
04336              
      * EXEC CICS GETMAIN
04337 *                 SET      (ADDRESS OF ACCOUNT-MASTER)
04338 *                 LENGTH   (W-ACCT-LENGTH)
04339 *            END-EXEC
      *    MOVE '," L                  $   #00010167' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-ACCT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04340              SET LCP-WS-ADDR-PNTR TO ADDRESS OF ACCOUNT-MASTER
04341
04342 *            SERVICE RELOAD ACCOUNT-MASTER
04343              MOVE LCP-WS-ADDR-COMP TO PI-ACCT-POINTER.
04344
04345      
      * EXEC CICS STARTBR
04346 *        RIDFLD      (W-ACCT-KEY)
04347 *        DATASET     (W-ACCT-ID)
04348 *        KEYLENGTH   (20)
04349 *        GENERIC
04350 *    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00010176' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-ID, 
                 W-ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04351
04352      MOVE 'Y'                   TO W-ACCT-BROWSE-STARTED.
04353
04354  8000-EXIT.
04355      EXIT.
04356
04357  8010-READNEXT-ERACCT.
04358
04359      
      * EXEC CICS READNEXT
04360 *        DATASET   (W-ACCT-ID)
04361 *        INTO      (ACCOUNT-MASTER)
04362 *        RIDFLD    (W-ACCT-KEY)
04363 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00010190' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04364
04365  8010-EXIT.
04366      EXIT.
04367
04368                                  EJECT
04369  8050-STARTBR-EMPROD.
04370
04371      IF  W-PROD-READ-SW EQUAL    TO 'Y'
04372          NEXT SENTENCE
04373
04374      ELSE
04375          IF  PI-PROD-POINTER GREATER THAN ZEROS
04376              MOVE PI-PROD-POINTER
04377                                  TO LCP-WS-ADDR-COMP
04378              SET ADDRESS OF PRODUCER-MASTER TO LCP-WS-ADDR-PNTR
04379 *            SERVICE RELOAD PRODUCER-MASTER
04380
04381          ELSE
04382              
      * EXEC CICS GETMAIN
04383 *                 SET      (ADDRESS OF PRODUCER-MASTER)
04384 *                 LENGTH   (W-PROD-LENGTH)
04385 *            END-EXEC
      *    MOVE '," L                  $   #00010213' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 W-PROD-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF PRODUCER-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
04386              SET LCP-WS-ADDR-PNTR TO ADDRESS OF PRODUCER-MASTER
04387
04388 *            SERVICE RELOAD PRODUCER-MASTER
04389              MOVE LCP-WS-ADDR-COMP TO PI-PROD-POINTER.
04390
04391      
      * EXEC CICS STARTBR
04392 *        RIDFLD      (W-PROD-KEY)
04393 *        DATASET     (W-PROD-ID)
04394 *        KEYLENGTH   (20)
04395 *        GENERIC
04396 *    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00010222' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PROD-ID, 
                 W-PROD-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04397
04398      MOVE 'Y'                   TO W-PROD-BROWSE-STARTED.
04399
04400  8050-EXIT.
04401      EXIT.
04402
04403  8060-READNEXT-EMPROD.
04404
04405      
      * EXEC CICS READNEXT
04406 *        DATASET   (W-PROD-ID)
04407 *        INTO      (PRODUCER-MASTER)
04408 *        RIDFLD    (W-PROD-KEY)
04409 *    END-EXEC.
           MOVE LENGTH OF
            PRODUCER-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00010236' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323336' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PROD-ID, 
                 PRODUCER-MASTER, 
                 DFHEIV12, 
                 W-PROD-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04410
04411  8060-EXIT.
04412      EXIT.
04413
04414                                 EJECT
04415  9600-PGMID-ERROR.
04416
04417      MOVE 9999                   TO W-CURRENT-ERROR.
04418      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
04419      GO TO 0200-RETURN-TO-CALLING-PGM.
04420
04421  9700-DATE-LINK.
04422
04423      MOVE W-LINK-ELDATCV         TO W-PGM-NAME.
04424
04425      
      * EXEC CICS LINK
04426 *        PROGRAM   (W-PGM-NAME)
04427 *        COMMAREA  (DATE-CONVERSION-DATA)
04428 *        LENGTH    (DC-COMM-LENGTH)
04429 *    END-EXEC.
      *    MOVE '."C                   (   #00010256' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130323536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04430
04431      IF  NO-CONVERSION-ERROR
04432              AND
04433          W-REVERSE-DATE
04434              AND
04435          PI-COMPANY-ID EQUAL 'AUK'
04436          MOVE DC-GREG-DATE-1-EDIT
04437                                  TO W-EDIT-DATE-1
04438          MOVE W-ED1-MM           TO W-ED2-MM
04439          MOVE W-ED1-DD           TO W-ED2-DD
04440          MOVE W-ED1-YY           TO W-ED2-YY
04441          MOVE W-EDIT-DATE-2      TO DC-GREG-DATE-1-EDIT.
04442
04443  9700-EXIT.
04444       EXIT.
04445
04446  9900-ERROR-PROCESS.
04447
04448      MOVE W-CURRENT-ERROR        TO PI-ERROR-CODE.
04449
04450      IF  PI-FATAL-ERROR
04451          GO TO 0200-RETURN-TO-CALLING-PGM.
04452
04453  9900-EXIT.
04454      EXIT.
04455
04456  9990-ABEND.
04457
04458      MOVE 9999                   TO W-CURRENT-ERROR.
04459      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.
04460      GO TO 0200-RETURN-TO-CALLING-PGM.
04461
04462  9990-EXIT.
04463      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1523' TO DFHEIV1
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
               GO TO 2120-ENDBR,
                     2120-ENDBR,
                     2900-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 3920-CLM-NOT-OPEN,
                     3900-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3930-ACCT-NOT-OPEN,
                     3910-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 3960-PROD-NOT-OPEN,
                     3950-PROD-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 3450-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 3940-ACTV-NOT-OPEN,
                     3450-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 3480-CONTINUE-ACTV-ERROR,
                     3479-COMP-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6100-NOT-FOUND,
                     6110-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6120-ARCH-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 6420-ARCH-DUPREC,
                     6410-ARCH-NOT-OPEN,
                     6425-ARCH-NOSPACE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 6596-ACTV-DUPREC,
                     6597-ACTV-NOSPACE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 6598-REWRITE-CLAIM
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7022-CERT-NOT-OPEN,
                     7024-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 7026-PLCY-NOT-OPEN,
                     7028-PLCY-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 7020-ACTV-NOT-OPEN,
                     7040-READ-BENEFICIARY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 7380-BENE-NOT-OPEN,
                     7100-READ-PHYSICIAN-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 7390-ACTV-NOT-OPEN,
                     7100-READ-PHYSICIAN-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 7390-ACTV-NOT-OPEN,
                     7120-READ-EMPLOYER-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 7390-ACTV-NOT-OPEN,
                     7140-READ-INSURED-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 7390-ACTV-NOT-OPEN,
                     7160-READ-OTHER1-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 7390-ACTV-NOT-OPEN,
                     7180-READ-OTHER2-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 7390-ACTV-NOT-OPEN,
                     7200-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 7390-ACTV-NOT-OPEN,
                     7220-READ-ACCOUNT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 7375-ACCT-NOT-OPEN,
                     7240-READ-3RD-PARTY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 7395-PROD-NOT-OPEN,
                     7300-READ-DENIAL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7390-ACTV-NOT-OPEN,
                     7260-READ-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 7300-READ-DENIAL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7390-ACTV-NOT-OPEN,
                     7340-READ-CNTL1,
                     7340-READ-CNTL1
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 7385-CNTL-NOT-OPEN,
                     7340-READ-CNTL2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 7385-CNTL-NOT-OPEN,
                     7350-READ-CNTL4
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 7385-CNTL-NOT-OPEN,
                     7350-READ-CNTL6
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 7397-PLAN-NOT-OPEN,
                     7350-READ-CNTL6
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 7385-CNTL-NOT-OPEN,
                     7360-SET-DATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 7380-BENE-NOT-OPEN,
                     7370-EXIT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL1523' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
