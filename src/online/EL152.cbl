00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL152 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 06/14/94 08:03:40.
00007 *                            VMOD=2.055.
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00011
00024 *REMARKS.    TRANSACTION - EX27 - CLAIMS LETTER WRITER.
00025
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
121802*                                REMOVE OBSOLETE CODE
102703* 102703                   SMVA  MOD COPY MEMBER ELCNAMET TO LEAVE
102703*                                CERTIFICATION DESIGNATION ON
102703*                                PROCESSOR NAME LINE IN CAPS
121203* 121203                   SMVA  ADD PROCESSING FOR NEW CLM TYP G
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
042604* 042605    2005042100002  PEMA  FIX TEMP STORAGE PROBLEMS
010407* 010407    2006111300003  PEMA  ADD PROCESSING FOR CARRIER 8
060109* 060109  CR2008102800002  PEMA  ADD VAR 16.1
033110* 033110  CR2009122800001  AJRA  NAPERSOFT
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
011212* 011212  IR2012011100002  AJRA  FIX DCC PROMPT LETTER
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
041513* 041513  CR2013011500003  AJRA  VALIDATE ENC CODE AGAINST ELENCC
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00026  ENVIRONMENT DIVISION.
00027
00028      EJECT
00029  DATA DIVISION.
00030  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00031  77  FILLER  PIC X(32)  VALUE '********************************'.
00032  77  FILLER  PIC X(32)  VALUE '*    EL152 WORKING STORAGE     *'.
00033  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.055 **********'.
00034
pemuni*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.
pemuni*77  LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP
pemuni*                                  USAGE POINTER.
pemuni 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
pemuni 77  LCP-WS-ADDR-PNTR REDEFINES LCP-WS-ADDR-COMP
pemuni                                   USAGE POINTER.
       77  B1                          PIC S9(5) COMP-3 VALUE +0.
       77  S1                          PIC S9 VALUE +0.
       77  S2                          PIC S9 VALUE +0.
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
CIDMOD 01  WS-HOLDING-ZIPS.
CIDMOD     05  FILLER              PIC X(10)  VALUE '--AM-ZIP--'.
CIDMOD     05  WS-AM-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(10)  VALUE '--AT-ZIP--'.
CIDMOD     05  WS-AT-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(10)  VALUE '--BE-ZIP--'.
CIDMOD     05  WS-BE-ZIP-CODE      PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(10)  VALUE '--CO-ZIP--'.
CIDMOD     05  WS-CO-ZIP           PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(17)  VALUE '--AM-ZIP-BEFORE--'.
CIDMOD     05  WS-AM-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(17)  VALUE '--AT-ZIP-BEFORE--'.
CIDMOD     05  WS-AT-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(17)  VALUE '--BE-ZIP-BEFORE--'.
CIDMOD     05  WS-BE-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(17)  VALUE '--CO-ZIP-BEFORE--'.
CIDMOD     05  WS-CO-ZIP-BEFORE    PIC X(9)   VALUE 'XXXXXXXXX'.
CIDMOD     05  FILLER              PIC X(17)  VALUE '-----------------'.
00038
00039 *                            COPY ELCSCTM.
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
00040
00041 *                            COPY ELCSCRTY.
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
00042
00043  01  WS-DATE-AREA.
00044      12  SAVE-DATE               PIC X(08)    VALUE SPACES.
00045      12  SAVE-BIN-DATE           PIC XX       VALUE SPACES.
00046      12  W-REVERSE-DATE-SW       PIC X        VALUE SPACES.
00047          88  W-REVERSE-DATE                   VALUE 'Y'.
00048      12  W-EDIT-DATE-1.
00049          16  W-ED1-MM            PIC XX.
00050          16  FILLER              PIC X        VALUE '/'.
00051          16  W-ED1-DD            PIC XX.
00052          16  FILLER              PIC X        VALUE '/'.
00053          16  W-ED1-YY            PIC XX.
00054      12  W-EDIT-DATE-2.
00055          16  W-ED2-DD            PIC XX.
00056          16  FILLER              PIC X        VALUE '/'.
00057          16  W-ED2-MM            PIC XX.
00058          16  FILLER              PIC X        VALUE '/'.
00059          16  W-ED2-YY            PIC XX.
       01  FILLER.
           05  WS-WORK-INT-RATE        PIC 99.999.
           05  WS-WORK-INT REDEFINES WS-WORK-INT-RATE
                                       PIC X(6).
       01  TRAN-DATA-LINE1             PIC X(80)    VALUE
           'BEGINJOB mode=''MVS'''.
       01  TRAN-DATA-LINE2.
033110*     05  FILLER                  PIC X(39)    VALUE
033110*     '"smtp -f EL152cl2 -t pema,kmsb,jmsb -s '.
033110     05  FILLER                  PIC X(29)    VALUE
033110     '"smtp -f EL152cl2 -t ajra -s '.
           05  TRAN-DETAIL.
               10  FILLER              PIC XX       VALUE
               ''' '.
               10  TRAN-DL2-USER       PIC X(05)    VALUE SPACES.
               10  TRAN-DL2-ARCHNO     PIC ZZZZ999  VALUE ZEROS.
               10  FILLER              PIC X        VALUE SPACES.
               10  TRAN-DL2-CCC        PIC X(22)    VALUE SPACES.
           05  FILLER                  PIC XX       VALUE '''"'.
           05  FILLER                  PIC X(30)    VALUE SPACES.
       01  TRAN-DATA-LINE3             PIC X(80)    VALUE
           'ENDJOB            '.
00060
042605 01  WS-PRINTER-ID               PIC X(4)    VALUE SPACES.
00061  01  STANDARD-AREAS.
00062      12  WS-ACCT-READ-SW         PIC X       VALUE ' '.
00063      12  WS-COMP-READ-SW         PIC X       VALUE ' '.
00064      12  WS-PROD-READ-SW         PIC X       VALUE ' '.
00065      12  SC-ITEM                 PIC S9(4)   VALUE +1  COMP.
00066      12  MAP-NAME.
00067          16  MAP-PREFIX          PIC XX      VALUE 'EL'.
00068          16  MAP-NUMBER          PIC X(4)    VALUE '152A'.
00069          16  MAP-FILLER          PIC XX      VALUE SPACES.
00070      12  GETMAIN-SPACE           PIC X       VALUE SPACE.
00071      12  MAPSET-NAME             PIC X(8)    VALUE 'EL152S'.
00072      12  TRANS-ID                PIC X(4)    VALUE 'EX27'.
00073      12  PRINT-TRANS             PIC X(4)    VALUE 'EX57'.
00074      12  LGXX-ID                 PIC X(4)    VALUE 'LGXX'.
00075      12  PGM-NAME                PIC X(8).
00076      12  TIME-IN                 PIC S9(7).
00077      12  TIME-OUT-R  REDEFINES TIME-IN.
00078          16  FILLER              PIC X.
00079          16  TIME-OUT            PIC 99V99.
00080          16  FILLER              PIC XX.
00081      12  XCTL-005                PIC X(5)    VALUE 'EL005'.
00082      12  XCTL-010                PIC X(5)    VALUE 'EL010'.
00083      12  XCTL-126                PIC X(5)    VALUE 'EL126'.
00084      12  LINK-001                PIC X(5)    VALUE 'EL001'.
00085      12  LINK-004                PIC X(5)    VALUE 'EL004'.
00086      12  LINK-ELDATCV            PIC X(7)    VALUE 'ELDATCV'.
00087      12  THIS-PGM                PIC X(8)    VALUE 'EL152'.
00088      12  LINK-EL1522             PIC X(8)    VALUE 'EL1522'.
00089      12  PGM-EL126               PIC X(8)    VALUE 'EL126'.
00090      12  PGM-EL150               PIC X(8)    VALUE 'EL150'.
00091      12  PGM-EL141               PIC X(8)    VALUE 'EL141'.
00092      12  PGM-EL1042              PIC X(8)    VALUE 'EL1042'.
00093      12  SUB                     PIC 99.
00094      12  WS-LABELS-SW            PIC X       VALUE SPACE.
00095
00096      12  WS-PI-QID.
00097          16  QID-TERM            PIC X(4)    VALUE SPACES.
00098          16  FILLER              PIC X(4)    VALUE '152A'.
00099
00100      12  W-LETTER-ADDRESS-TYPE.
00101          16  W-LETTER-ADDR-TYPE  PIC  X VALUE SPACES.
00102          16  W-LETTER-ADDR-SEQ   PIC  9 VALUE ZEROS.
00103
00104      12  W-ADDRESS-SELECTION.
00105          16  W-ACCOUNT           PIC  9 VALUE ZEROS.
00106          16  W-BENEFICIARY       PIC  9 VALUE ZEROS.
00107          16  W-EMPLOYER          PIC  9 VALUE ZEROS.
00108          16  W-INSURED           PIC  9 VALUE ZEROS.
00109          16  W-OTHER-1           PIC  9 VALUE ZEROS.
00110          16  W-OTHER-2           PIC  9 VALUE ZEROS.
00111          16  W-PHYSICIAN         PIC  9 VALUE ZEROS.
00112
00113      12  W-Z-CONTROL-DATA.
00114          16  W-NUMBER-OF-COPIES  PIC  9.
00115          16  FILLER              PIC  X.
00116          16  W-DAYS-TO-FOLLOW-UP PIC  999.
00117          16  FILLER              PIC  X.
00118          16  W-DAYS-TO-RESEND-1  PIC  999.
00119          16  FILLER              PIC  X.
040110         16  W-FORM-TO-RESEND    PIC  X(4).
040110         16  FILLER              PIC  X(1).
040110         16  W-PROMPT-LETTER     PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-ENCLOSURE-CD      PIC  X(3).
040110         16  FILLER              PIC  X(1).
040110         16  W-AUTO-CLOSE-IND    PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-LETTER-TO-BENE    PIC  X(1).
00123
00124      12  W-CREDIT-CARD-LOAN-NO.
00125          16  W-LOAN-NO           PIC  X(08).
00126          16  W-CURRENT-LOAN-NO   PIC  X(12).
00127
00128      12  W-GROUPING.
00129          16  W-GROUP-3           PIC  XXX.
00130          16  FILLER              PIC  XXX.
00131
00132      12  W-NAME.
00133          16  W-FIRST-NAME        PIC  X(12).
00134          16  W-MIDDLE-NAME       PIC  X(12).
00135          16  W-LAST-NAME         PIC  X(15).
00136
00137      12  WS-PHONE-IN             PIC 9(11)   VALUE ZEROS.
00138      12  WS-PHONE-IN-R  REDEFINES WS-PHONE-IN.
00139          16  FILLER              PIC 9.
00140          16  WSPI-AREA           PIC 9(3).
00141          16  WSPI-PFX            PIC 9(3).
00142          16  WSPI-SFX            PIC 9(4).
00143      12  WS-PHONE-OUT.
00144          16  WSPO-AREA           PIC X(3).
00145          16  FILLER              PIC X       VALUE '-'.
00146          16  WSPO-PFX            PIC X(3).
00147          16  FILLER              PIC X       VALUE '-'.
00148          16  WSPO-SFX            PIC X(4).
00149
00150      12  WS-ZIP-NUMERIC          PIC 9(9).
00151      12  WS-ZIP-NONNUM  REDEFINES  WS-ZIP-NUMERIC
00152                                  PIC X(9).
00153
00154      12  WS-ZIP-CODE.
00155          16  WS-AM-ZIP-CODE      PIC X(5).
00156          16  WS-AM-ZIP-DASH      PIC X.
00157          16  WS-AM-ZIP-PLUS4     PIC X(4).
00158      12  WS-ZIP-CODE-CANADIAN  REDEFINES  WS-ZIP-CODE.
00159          16  WS-CAN-POSTAL-1     PIC XXX.
00160          16  FILLER              PIC X.
00161          16  WS-CAN-POSTAL-2     PIC XXX.
00162          16  FILLER              PIC XXX.
00163
00164      12  WS-LABEL-HOLD-AREA.
00165          16  WS-LABEL-LINES OCCURS 6 TIMES
00166                             INDEXED BY WS-NDX  WS-NDX2.
00167              20  WS-LABEL-ZIP.
00168                  24  WS-LABEL-1ST-ZIP  PIC X(5).
00169                  24  FILLER            PIC X.
00170                  24  WS-LABEL-2ND-ZIP  PIC X(4).
00171              20  FILLER                PIC X(9).
00172              20  WS-LAST-DIGIT         PIC X.
00173              20  WS-LAST-ZIP.
00174                  24  WS-LAST-1ST-ZIP   PIC X(5).
00175                  24  FILLER            PIC X.
00176                  24  WS-LAST-2ND-ZIP   PIC X(4).
00177
00178      12  WS-DATA-FOUND-SW            PIC X.
00179          88  NO-CHARACTERS-FOUND           VALUE 'N'.
00180
033110     12  WS-SKIP-EMAIL           PIC X       VALUE 'N'.
00181      12  WS-STATE-LINE           PIC X       VALUE 'N'.
102703     12  WS-PROCESSOR-LINE       PIC X(01)   VALUE 'N'.
102703     12  WS-CAPS-SW              PIC X(01)   VALUE 'N'.
102703         88  THE-REST-R-CAPS                 VALUE 'Y'.
00182      12  WS-POSITION2            PIC S9(4)   COMP.
00183      12  WS-POSITION21           PIC S9(4)   COMP.
00184      12  WS-WORD-LENGTH          PIC S9(4)   COMP-3.
00185
00186      12  WS-TEMP-AREA1.
00187          16  WS-TEMP-1           PIC X OCCURS 29
00188                                        INDEXED BY TA1.
00189      12  WS-TEMP-AREA2.
00190          16  WS-TEMP-2           PIC X OCCURS 30
00191                                        INDEXED BY TA2
00192                                                   TA21
00193                                                   MOVE-INDX.
00194
00195      12  WS-SAVE-TEMP-AREA2      PIC X(30)   VALUE SPACES.
00196
00197      12  ACCT-BROWSE-STARTED     PIC X       VALUE 'N'.
00198      12  TEXT-BROWSE-STARTED     PIC X       VALUE 'N'.
00199      12  ARCH-BROWSE-STARTED     PIC X       VALUE 'N'.
00200      12  ACTV-BROWSE-STARTED     PIC X       VALUE 'N'.
00201      12  PROD-BROWSE-STARTED     PIC X       VALUE 'N'.
00202      12  INDX-WORK               PIC 99.
00203      12  TEMP-CURR-LINE          PIC S9(3)    COMP-3.
00204
00205      12  DATE-WORK               PIC 9(7).
00206      12  DT-REDEF REDEFINES DATE-WORK.
00207          16  FILLER              PIC XX.
00208          16  DT-WORK             PIC 9(5).
00209
00210      12  BEN-HOLD                PIC XX.
00211
00212      12  BENEFIT-WORK            PIC XXX.
00213      12  BEN-R REDEFINES BENEFIT-WORK.
00214          16  ELIM-DAYS           PIC XX.
00215          16  FILLER              PIC X.
00216
00217      12  GETMAIN-SWITCH          PIC 9        VALUE 0.
00218          88  NO-GETMAIN-DONE-YET              VALUE 0.
00219          88  REFRESH-GETMAIN-AREA             VALUE 1.
00220
00221      12  WORK-AMOUNT             PIC S9(9)V99 VALUE +0.
00222      12  CURRENT-SAVE            PIC XX.
00223      12  CURRENT-PLUS3-SAVE      PIC XX.
00224      12  RESEND-SAVE             PIC XX    VALUE LOW-VALUE.
00225      12  FOLLOW-UP-SAVE          PIC XX    VALUE LOW-VALUE.
00226      12  SEQ-COUNTER             PIC S9(4) COMP.
00227      12  CORR-TRLR-SEQ           PIC S9(4) COMP.
00228      12  DEEDIT-FIELD            PIC X(15).
00229      12  DEEDIT-FIELD-V0  REDEFINES DEEDIT-FIELD   PIC S9(15).
00230
00231      12  ARCH-SUPPRESS           PIC ZZZZZZZZ99.
00232      12  ARCH-EDIT REDEFINES ARCH-SUPPRESS    PIC X(10).
00233
00234      12  LOWER-CASE PIC X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
00235      12  UPPER-CASE PIC X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
00236
00237      12  WS-CCN-12               PIC X(12).
00238
00239      12  WS-ADDR-TYPE-CD.
00240          16  WS-ADDR-TYPE        PIC X.
00241          16  WS-ADDR-SEQ         PIC X.
00242          16  WS-ADDR-SEQ-NUM REDEFINES
00243              WS-ADDR-SEQ         PIC 9.
00244
00245      12  WS-DMD-CERT-STATE       PIC XX.
00246      12  WS-DMD-CERT-GROUPING    PIC XX.
00247      12  WS-DMD-BEN-CODE         PIC XX.
00248      12  WS-DMD-CORR-TRLR-SEQ    PIC S9(4) COMP.
00249      12  WS-DMD-LETTER-FORM      PIC X(4).
00250      12  WS-DMD-RES-ST           PIC XX.
00251
00252      12  WS-DMD-UND-STATEMENT    PIC X(50)  VALUE
00253           'Central States provides plan administration for'.
00254
00255      12  WS-DMD-UND-COMPANYA     PIC X(50)  VALUE
00256           'Central States Health and Life Co.'.
00257
00258      12  WS-DMD-UND-COMPANYB     PIC X(50)  VALUE
00259           'Central States Indemnity Co.'.
00260
00261 * DLO023
00262  01  DL23-COMM-LENGTH            PIC S9(4) COMP VALUE +132.
00263  01  WS-DLO-CODES-TABLE.
00264      12  DL23-SYSTEM-ID          PIC XX.
00265      12  DL23-RECORD-TYPE        PIC XX.
00266      12  DL23-RECORD-KEY         PIC X(6).
00267      12  DL23-RETURN-CODE        PIC XX.
00268      12  DL23-CODE-DESC          PIC X(60).
00269      12  DL23-GEN-DESC-1         PIC X(20).
00270      12  DL23-GEN-DESC-2         PIC X(20).
00271      12  DL23-GEN-DESC-3         PIC X(20).
CIDMOD     12  WS-LETTER-STATUS        PIC X        VALUE ' '.
CIDMOD     12  WS-BLANK                PIC X        VALUE ' '.
00272
00273  01  WS-SAVE-ACCT-RECORD         PIC X(2000)  VALUE SPACES.
00274  01  WS-SAVE-PRODUCER-RECORD     PIC X(2000)  VALUE SPACES.
00275
00276      EJECT
00277  01  ACCESS-KEYS-AND-FILE-IDS.
00278      12  CNTL-ID                  PIC X(8)    VALUE 'ELCNTL'.
00279      12  ACCT-ID                  PIC X(8)    VALUE 'ERACCT'.
00280      12  CERT-ID                  PIC X(8)    VALUE 'ELCERT'.
00281      12  ARCH-ID                  PIC X(8)    VALUE 'ELARCH'.
00282      12  ARCT-ID                  PIC X(8)    VALUE 'ELARCT'.
00283      12  CLAM-ID                  PIC X(8)    VALUE 'ELMSTR'.
00284      12  TEXT-ID                  PIC X(8)    VALUE 'ELLETR'.
00285      12  ACTV-ID                  PIC X(8)    VALUE 'ELTRLR'.
00286      12  BENE-ID                  PIC X(8)    VALUE 'ELBENE'.
00287      12  PROD-ID                  PIC X(8)    VALUE 'MPPROD'.
00288      12  PLCY-ID                  PIC X(8)    VALUE 'MPPLCY'.
00289      12  PLAN-ID                  PIC X(8)    VALUE 'MPPLAN'.
033110     12  NAPS-ID                  PIC X(8)    VALUE 'ELNAPS'.
041513     12  ENCC-ID                  PIC X(8)    VALUE 'ELENCC'.
00290
00291      12  CNTL-KEY.
00292          16  CNTL-CO              PIC X(3).
00293          16  CNTL-RECORD-TYPE     PIC X       VALUE '1'.
00294          16  CNTL-GENL.
00295            18 CNTL-GEN1           PIC XX      VALUE SPACES.
00296            18 CNTL-GEN2.
00297              20 CNTL-GEN3         PIC X       VALUE SPACES.
00298              20 CNTL-GEN4         PIC X       VALUE SPACES.
00299          16  CNTL-SEQ             PIC S9(4)   VALUE +0    COMP.
00300
00301       12  WS-ERCOMP-KEY.
00302           16  WS-ERCOMP-COMPANY-CD PIC X.
00303           16  WS-ERCOMP-CARRIER    PIC X.
00304           16  WS-ERCOMP-GROUPING   PIC X(6).
00305           16  WS-ERCOMP-RESP-NO    PIC X(10).
00306           16  WS-ERCOMP-ACCOUNT    PIC X(10).
00307           16  WS-ERCOMP-TYPE       PIC X.
00308
00309      12  CLAM-KEY.
00310          16  CLAM-CO              PIC X.
00311          16  CLAM-CARRIER         PIC X.
00312          16  CLAM-CLAIM           PIC X(7).
00313          16  CLAM-CERT-NUM        PIC X(11).
00314
00315      12  TEXT-KEY.
00316          16  TEXT-PARTIAL-KEY.
00317              20  TEXT-CO          PIC X.
00318              20  TEXT-LETTER      PIC X(4).
00319          16  TEXT-FILLER          PIC X(8)    VALUE SPACES.
00320          16  TEXT-SEQ             PIC S9(4)   VALUE +0    COMP.
00321
00322      12  ACTV-KEY.
00323          16  ACTV-PARTIAL-KEY.
00324              20  ACTV-CO          PIC X.
00325              20  ACTV-CARRIER     PIC X.
00326              20  ACTV-CLAIM       PIC X(7).
00327              20  ACTV-CERT-NUM    PIC X(11).
00328          16  ACTV-SEQ             PIC S9(4)   VALUE +0    COMP.
00329
00330      12  CERT-KEY.
00331          16  CERT-CO              PIC X.
00332          16  CERT-CARRIER         PIC X.
00333          16  CERT-GROUPING        PIC X(6).
00334          16  CERT-STATE           PIC XX.
00335          16  CERT-ACCOUNT         PIC X(10).
00336          16  CERT-EFF-DT          PIC XX.
00337          16  CERT-CERT-NUM        PIC X(11).
00338
00339      12  ACCT-KEY.
00340          16  ACCT-PARTIAL-KEY.
00341              20  ACCT-CO              PIC X.
00342              20  ACCT-CARRIER         PIC X.
00343              20  ACCT-GROUPING        PIC X(6).
00344              20  ACCT-STATE           PIC XX.
00345              20  ACCT-ACCOUNT         PIC X(10).
00346          16  ACCT-EXP-DATE            PIC XX.
00347
00348      12  ARCH-KEY.
00349          16  ARCH-PARTIAL-KEY.
00350              20  ARCH-CO          PIC X.
00351              20  ARCH-NUMBER      PIC S9(8)      COMP.
00352          16  ARCH-REC-TYPE        PIC X.
00353          16  ARCH-SEQ             PIC S9(4)      COMP VALUE +0.
00354
00355      12  BENE-KEY.
00356          16  BENE-COMP-CD         PIC X.
00357          16  BENE-REC-TYPE        PIC X.
00358          16  BENE-NUMBER.
00359              20  BENE-CREDITOR    PIC XXX.
00360              20  FILLER           PIC X(7).
00361
00362      12  PROD-KEY.
00363          16  PROD-PARTIAL-KEY.
00364              20  PROD-CO          PIC X.
00365              20  PROD-CARRIER     PIC X.
00366              20  PROD-GROUPING    PIC X(6).
00367              20  PROD-STATE       PIC XX.
00368              20  PROD-PRODUCER    PIC X(10).
00369          16  PROD-EXP-DATE        PIC XX.
00370
00371      12  PLCY-KEY.
00372          16  PLCY-CO              PIC X.
00373          16  PLCY-CARRIER         PIC X.
00374          16  PLCY-GROUPING        PIC X(06).
00375          16  PLCY-STATE           PIC XX.
00376          16  PLCY-PRODUCER        PIC X(10).
00377          16  PLCY-EFF-DT          PIC XX.
00378          16  PLCY-REFERENCE-NO    PIC X(20).
00379
00380      12  PLAN-KEY.
00381          16  PLAN-CO              PIC X.
00382          16  PLAN-CARRIER         PIC X.
00383          16  PLAN-GROUPING        PIC X(6).
00384          16  PLAN-STATE           PIC XX.
00385          16  PLAN-PRODUCER        PIC X(10).
00386          16  PLAN-CODE            PIC XX.
00387          16  PLAN-REV-NO          PIC 999.
041513
041513     12  ELENCC-KEY.
041513         16  ELENCC-COMPANY-CD    PIC X.
041513         16  ELENCC-REC-TYPE      PIC X.
041513         16  ELENCC-ENC-CODE      PIC X(5).
041513         16  F                    PIC X(09).
00388
00389      EJECT
00390      12  ARCH-SAVE-KEY           PIC X(5).
00391      12  ACTV-SAVE-KEY           PIC X(20).
00392      12  ACCT-SAVE-KEY           PIC X(20).
00393      12  PROD-SAVE-KEY           PIC X(20).
00394      12  ACCT-LENGTH             PIC S9(4)  COMP  VALUE +2000.
00395      12  ARCH-LENGTH             PIC S9(4)  COMP  VALUE +90.
00396      12  ARCT-LENGTH             PIC S9(4)  COMP  VALUE +90.
00397      12  ACTV-LENGTH             PIC S9(4)  COMP  VALUE +200.
00398      12  PROD-LENGTH             PIC S9(4)  COMP  VALUE +2000.
033110     12  NAPS-LENGTH             PIC S9(4)  COMP  VALUE +150.
041513     12  ENCC-LENGTH             PIC S9(4)  COMP  VALUE +400.
00399      12  TEXT-SAVE-KEY           PIC X(5).
00400      12  SAVE-VARIABLE-POINTER   PIC S9(8)   COMP.
00401      12  VAR-HOLD.
00402          16  V1                  PIC X.
00403          16  V2                  PIC X.
00404          16  V3                  PIC X.
00405          16  V4                  PIC X.
00406      12  V-HOLD REDEFINES VAR-HOLD.
00407          16  V-NUM               PIC 99.
00408          16  V-PERIOD            PIC X.
00409          16  V-DECIMAL           PIC 9.
00410
00411      12  MAX-LINES               PIC 999     VALUE 300.
00412      12  NUM-LINES-PER-SCREEN    PIC 99      VALUE 13.
00413      12  TS-NUM-REC-IN-GROUP     PIC 99      VALUE 50.
00414      12  TS-GROUP-WORK           PIC 9(5)    VALUE 0     COMP-3.
00415      12  TS-LENGTH               PIC S9(4)   VALUE +3650 COMP.
00416      12  TS-ITEM                 PIC S9(4)   VALUE +0    COMP.
00417      12  TS-MAP-LENGTH           PIC S9(4)   VALUE +1343 COMP.
00418      12  ROLL-COUNTER            PIC S999    VALUE +0    COMP-3.
00419      12  TS-NAME-TEXT.
00420          16  TS-ID-TEXT          PIC X(4)    VALUE '104A'.
00421          16  TS-ID-TIME REDEFINES TS-ID-TEXT  PIC S9(7) COMP-3.
00422          16  TS-TERM-TEXT.
00423           17 TS-TERM-PREFIX      PIC XX.
00424           17 FILLER              PIC XX.
00425      12  TS-NAME-SCREEN.
00426          16  FILLER              PIC X(4)    VALUE '152X'.
00427          16  TS-TERM-SCREEN      PIC X(4).
00428
00429      12  LINE-NUM.
00430          16  LINE1               PIC X.
00431          16  LINE23              PIC 99.
00432      12  LIN-NUM REDEFINES LINE-NUM  PIC 999.
00433      12  TOP-FORM                PIC X(70)
00434          VALUE '*****TOP OF FORM *****'.
00435      12  SINGLE-LINE             PIC X(70).
00436      12  SINGLE-LINE-BY-1 REDEFINES SINGLE-LINE.
00437          16  ONE-CHAR OCCURS 70 TIMES INDEXED BY INDX1 INDX2
00438                                  PIC X.
033110     12  NAPERSOFT-LETTER        PIC X(16)
033110         VALUE 'Napersoft letter'.
041513
041513     12  WS-RESPONSE             PIC S9(8)   COMP.
041513         88  RESP-NORMAL              VALUE +00.
041513         88  RESP-ERROR               VALUE +01.
041513         88  RESP-NOTFND              VALUE +13.
041513         88  RESP-NOTOPEN             VALUE +19.
041513         88  RESP-ENDFILE             VALUE +20.
00439
00440  01  HAN-LETTER-REASON-DATA.
00441      12  WS-REASON-TEXT.
00442          16  WS-RE-NDX           PIC 99.
00443          16  FILLER              PIC X(68).
00444
00445      12  HAN-REASON-TABLE.
00446          16  FILLER              PIC X(50) VALUE
00447            'ADDITIONAL INFO REQUESTED FROM PHYSICIAN          '.
00448          16  FILLER              PIC X(50) VALUE
00449            'CHECKING PRE-EXISTING CONDITION                   '.
00450          16  FILLER              PIC X(50) VALUE
00451            'ADDITIONAL INFO RECEIVED / CLAIM REOPENED         '.
00452          16  FILLER              PIC X(50) VALUE
00453            'LETTER TO INSURED                                 '.
00454          16  FILLER              PIC X(50) VALUE
00455            'LETTER TO CREDITOR                                '.
00456          16  FILLER              PIC X(50) VALUE
00457            'LETTER TO EMPLOYER                                '.
00458          16  FILLER              PIC X(50) VALUE
00459            'LETTER TO INSURED / 2ND REQUEST                   '.
00460          16  FILLER              PIC X(50) VALUE
00461            'LETTER TO CREDITOR / 2ND REQUEST                  '.
00462          16  FILLER              PIC X(50) VALUE
00463            'LETTER TO EMPLOYER / 2ND REQUEST                  '.
00464          16  FILLER              PIC X(50) VALUE
00465            'AWAITING INITIAL CLAIM FORM                       '.
00466          16  FILLER              PIC X(50) VALUE
00467            'AWAITING SUPPLEMENTAL INFORMATION                 '.
00468          16  FILLER              PIC X(50) VALUE
00469            'DENIED / PRE-EXISTING CONDITION                   '.
00470          16  FILLER              PIC X(50) VALUE
00471            'DENIED / WAITING PERIOD NOT MET                   '.
00472          16  FILLER              PIC X(50) VALUE
00473            'DENIED / NORMAL PREGNANCY                         '.
00474          16  FILLER              PIC X(50) VALUE
00475            'DENIED / ACT OF WAR                               '.
00476          16  FILLER              PIC X(50) VALUE
00477            'DENIED / NOT TOTALLY DISABLED                     '.
00478          16  FILLER              PIC X(50) VALUE
00479            'DENIED / NOT UNDER CARE & TREATMENT OF PHYSICIAN  '.
00480          16  FILLER              PIC X(50) VALUE
00481            'DENIED / NO COVERAGE INFORCE                      '.
00482          16  FILLER              PIC X(50) VALUE
00483            'DENIED / DISABLED ON DATE OF LOAN                 '.
00484          16  FILLER              PIC X(50) VALUE
00485            'DENIED / OVER MAXIMUM AGE                         '.
00486          16  FILLER              PIC X(50) VALUE
00487            'CLOSED / CLAIM INFO NOT PROVIDED                  '.
00488          16  FILLER              PIC X(50) VALUE
00489            'PHYSICIAN INFORMATION INCOMPLETE                  '.
00490          16  FILLER              PIC X(50) VALUE
00491            'ACKNOWLEDGEMENT LETTER TO INSURED                 '.
00492          16  FILLER              PIC X(50) VALUE
00493            'DENIED/SUICIDE EXCLUSION                          '.
00494          16  FILLER              PIC X(50) VALUE
00495            'DENIED/LOAN EFFECTIVE BEFORE POLICY EFFECTIVE DATE'.
00496          16  FILLER              PIC X(50) VALUE
00497            'DENIED/JOINT DEBTORS NOT COVERED                  '.
00498          16  FILLER              PIC X(50) VALUE
00499            'DENIED/GROUP POLICY LAPSED                        '.
00500          16  FILLER              PIC X(50) VALUE
00501            'DENIED/DECEASED PRIOR TO POLICY EFFECTIVE DATE    '.
00502          16  FILLER              PIC X(50) VALUE
00503            'DENIED/LOAN TERM IN EXCESS OF MAXIMUM LOAN TERM   '.
00504          16  FILLER              PIC X(50) VALUE
00505            'DENIED/INSURED NOT PERMANENTLY & TOTALLY DISABLED '.
00506          16  FILLER              PIC X(50) VALUE
00507            'DENIED/IME EXAM DOES NOT SUPPORT CONT. DISABILITY '.
00508          16  FILLER              PIC X(50) VALUE
00509            'DENIED/INSURED DID NOT APPEAR FOR IME EXAM        '.
00510
00511      12  HAN-LETTER-REASON-TABLE  REDEFINES  HAN-REASON-TABLE.
00512          16  HAN-TABLE-ENTRIES  OCCURS  32  TIMES.
00513              20  HAN-REASON-TEXT PIC X(50).
00514
00515  01  ERROR-MESSAGES.
00516      12  ER-0000                 PIC X(4)  VALUE '0000'.
00517      12  ER-0004                 PIC X(4)  VALUE '0004'.
00518      12  ER-0006                 PIC X(4)  VALUE '0006'.
00519      12  ER-0008                 PIC X(4)  VALUE '0008'.
00520      12  ER-0013                 PIC X(4)  VALUE '0013'.
00521      12  ER-0023                 PIC X(4)  VALUE '0023'.
00522      12  ER-0029                 PIC X(4)  VALUE '0029'.
00523      12  ER-0033                 PIC X(4)  VALUE '0033'.
00524      12  ER-0042                 PIC X(4)  VALUE '0042'.
00525      12  ER-0047                 PIC X(4)  VALUE '0047'.
00526      12  ER-0051                 PIC X(4)  VALUE '0051'.
00527      12  ER-0066                 PIC X(4)  VALUE '0066'.
00528      12  ER-0067                 PIC X(4)  VALUE '0067'.
00529      12  ER-0070                 PIC X(4)  VALUE '0070'.
00530      12  ER-0133                 PIC X(4)  VALUE '0133'.
00531      12  ER-0154                 PIC X(4)  VALUE '0154'.
00532      12  ER-0168                 PIC X(4)  VALUE '0168'.
00533      12  ER-0169                 PIC X(4)  VALUE '0169'.
00534      12  ER-0172                 PIC X(4)  VALUE '0172'.
00535      12  ER-0174                 PIC X(4)  VALUE '0174'.
00536      12  ER-0175                 PIC X(4)  VALUE '0175'.
00537      12  ER-0176                 PIC X(4)  VALUE '0176'.
00538      12  ER-0177                 PIC X(4)  VALUE '0177'.
00539      12  ER-0178                 PIC X(4)  VALUE '0178'.
00540      12  ER-0179                 PIC X(4)  VALUE '0179'.
00541      12  ER-0180                 PIC X(4)  VALUE '0180'.
00542      12  ER-0181                 PIC X(4)  VALUE '0181'.
00543      12  ER-0182                 PIC X(4)  VALUE '0182'.
00544      12  ER-0183                 PIC X(4)  VALUE '0183'.
00545      12  ER-0184                 PIC X(4)  VALUE '0184'.
00546      12  ER-0185                 PIC X(4)  VALUE '0185'.
00547      12  ER-0186                 PIC X(4)  VALUE '0186'.
00548      12  ER-0187                 PIC X(4)  VALUE '0187'.
00549      12  ER-0188                 PIC X(4)  VALUE '0188'.
00550      12  ER-0189                 PIC X(4)  VALUE '0189'.
00551      12  ER-0190                 PIC X(4)  VALUE '0190'.
00552      12  ER-0191                 PIC X(4)  VALUE '0191'.
00553      12  ER-0206                 PIC X(4)  VALUE '0206'.
00554      12  ER-0211                 PIC X(4)  VALUE '0211'.
00555      12  ER-0279                 PIC X(4)  VALUE '0279'.
00556      12  ER-0280                 PIC X(4)  VALUE '0280'.
00557      12  ER-0281                 PIC X(4)  VALUE '0281'.
00558      12  ER-0332                 PIC X(4)  VALUE '0332'.
00559      12  ER-0343                 PIC X(4)  VALUE '0343'.
00560      12  ER-0373                 PIC X(4)  VALUE '0373'.
00561      12  ER-0374                 PIC X(4)  VALUE '0374'.
00562      12  ER-0412                 PIC X(4)  VALUE '0412'.
00563      12  ER-0413                 PIC X(4)  VALUE '0413'.
00564      12  ER-0533                 PIC X(4)  VALUE '0533'.
00565      12  ER-0537                 PIC X(4)  VALUE '0537'.
00566      12  ER-0716                 PIC X(4)  VALUE '0716'.
00567      12  ER-0861                 PIC X(4)  VALUE '0861'.
040110     12  ER-0894                 PIC X(4)  VALUE '0894'.
00568      12  ER-0900                 PIC X(4)  VALUE '0900'.
00569      12  ER-0909                 PIC X(4)  VALUE '0909'.
00570      12  ER-0911                 PIC X(4)  VALUE '0911'.
00571      12  ER-0912                 PIC X(4)  VALUE '0912'.
           12  ER-1236                 PIC X(4)  VALUE '1236'.
           12  ER-1560                 PIC X(4)  VALUE '1560'.
00572      12  ER-2055                 PIC X(4)  VALUE '2055'.
00573      12  ER-2397                 PIC X(4)  VALUE '2397'.
00574      12  ER-2398                 PIC X(4)  VALUE '2398'.
00575      12  ER-3547                 PIC X(4)  VALUE '3547'.
00576      12  ER-3770                 PIC X(4)  VALUE '3770'.
00577      12  ER-3771                 PIC X(4)  VALUE '3771'.
00578      12  ER-3772                 PIC X(4)  VALUE '3772'.
00579      12  ER-7675                 PIC X(4)  VALUE '7675'.
00580      12  ER-7840                 PIC X(4)  VALUE '7840'.
00581      12  ER-7842                 PIC X(4)  VALUE '7842'.
00582      12  ER-7843                 PIC X(4)  VALUE '7843'.
00583      12  ER-8158                 PIC X(4)  VALUE '8158'.
00584      12  ER-9106                 PIC X(4)  VALUE '9106'.
00585      12  ER-9483                 PIC X(4)  VALUE '9483'.
00586      12  ER-9808                 PIC X(4)  VALUE '9808'.
00587      12  ER-9883                 PIC X(4)  VALUE '9883'.
00588      12  ER-9887                 PIC X(4)  VALUE '9887'.
00589
00590      EJECT
00591 ****************************************************
00592 *       WHEN ADDING OR DELETING ENTRIES TO         *
00593 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *
00594 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
00595 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *
00596 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
00597 *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
00598 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
00599 *                                                  *
00600 *   3 FIELDS TO CHANGE:                            *
00601 *      1 - SS-NUM-ENTRIES........(NO.OF ENTRIES)   *
00602 *      2 - SS-WORK-AREA-LENGTH...(TOTAL LENGTH)    *
00603 *      3 - SYSTEM-VARIABLES......( "      "   )    *
00604 ****************************************************
00605
00606 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
00607 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA
00608
010407     12  SS-NUM-ENTRIES          PIC 999    VALUE 130      COMP-3.
00610      12  SS-COUNTER              PIC 999                   COMP-3.
010407     12  SS-WORK-AREA-LENGTH     PIC S9(4)  VALUE +3865    COMP.
00612
00613  01  VARIABLE-WORK-AREA.
00614      12  VAR-CODE                PIC X(4).
00615      12  VAR-LEN                 PIC 99.
00616      12  VAR-DATA                PIC X(100).
00617      12  VAR-DATA-R REDEFINES VAR-DATA.
00618        16  VAR-ONE-CHAR OCCURS 100 TIMES INDEXED BY INDXV PIC X.
00619
00620  01  SYSTEM-SUPPORTED-VARIABLES.
00621 *****COMPANY NAME
00622      12  SS01                    PIC X(4)  VALUE     '01.0'.
00623      12  SS01L                   PIC 99    VALUE 36.
00624      12  SS01D                   PIC X(30) VALUE ALL '*'.
00625 *****FULL COMPANY ADDRESS
00626      12  SS02-1                  PIC X(4)  VALUE     '02.1'.
00627      12  SS02-1L                 PIC 99    VALUE 36.
00628      12  SS02-1D                 PIC X(30) VALUE ALL '*'.
00629      12  SS02-2                  PIC X(4)  VALUE     '02.2'.
00630      12  SS02-2L                 PIC 99    VALUE 36.
00631      12  SS02-2D                 PIC X(30) VALUE ALL '*'.
00632      12  SS02-3                  PIC X(4)  VALUE     '02.3'.
00633      12  SS02-3L                 PIC 99    VALUE 36.
00634      12  SS02-3D                 PIC X(30) VALUE ALL '*'.
00635      12  SS02-4                  PIC X(4)  VALUE     '02.4'.
00636      12  SS02-4L                 PIC 99    VALUE 36.
00637      12  SS02-4D                 PIC X(30) VALUE ALL '*'.
00638      12  SS02-5                  PIC X(4)  VALUE     '02.5'.
00639      12  SS02-5L                 PIC 99    VALUE 36.
00640      12  SS02-5D                 PIC X(30) VALUE ALL '*'.
00641 *****CARRIER NAME
00642      12  SS03                    PIC X(4)  VALUE     '03.0'.
00643      12  SS03L                   PIC 99    VALUE 36.
00644      12  SS03D                   PIC X(30) VALUE ALL '*'.
00645 *****INVESTORS HERITAGE
010407* as Administrator for Investors Heritage Life Insurance Company
00646      12  SS03-1                  PIC X(4)  VALUE     '03.1'.
00647      12  SS03-1L                 PIC 99    VALUE 68.
00648      12  SS03-1D                 PIC X(62) VALUE ALL '*'.
00645 *****FULL CARRIER ADDRESS
00646      12  SS04-1                  PIC X(4)  VALUE     '04.1'.
00647      12  SS04-1L                 PIC 99    VALUE 36.
00648      12  SS04-1D                 PIC X(30) VALUE ALL '*'.
00649      12  SS04-2                  PIC X(4)  VALUE     '04.2'.
00650      12  SS04-2L                 PIC 99    VALUE 36.
00651      12  SS04-2D                 PIC X(30) VALUE ALL '*'.
00652      12  SS04-3                  PIC X(4)  VALUE     '04.3'.
00653      12  SS04-3L                 PIC 99    VALUE 36.
00654      12  SS04-3D                 PIC X(30) VALUE ALL '*'.
00655      12  SS04-4                  PIC X(4)  VALUE     '04.4'.
00656      12  SS04-4L                 PIC 99    VALUE 36.
00657      12  SS04-4D                 PIC X(30) VALUE ALL '*'.
00658      12  SS04-5                  PIC X(4)  VALUE     '04.5'.
00659      12  SS04-5L                 PIC 99    VALUE 36.
00660      12  SS04-5D                 PIC X(30) VALUE ALL '*'.
00661 *****CARRIER PHONE NUMBER
00662      12  SS04-6                  PIC X(4)  VALUE     '04.6'.
00663      12  SS04-6L                 PIC 99    VALUE 18.
00664      12  SS04-6D                 PIC X(12) VALUE ALL '*'.
00665 *****FULL ADDRESEE LABEL
00666      12  SS05-1                  PIC X(4)  VALUE     '05.1'.
00667      12  SS05-1L                 PIC 99    VALUE 36.
00668      12  SS05-1D                 PIC X(30) VALUE ALL '*'.
00669      12  SS05-2                  PIC X(4)  VALUE     '05.2'.
00670      12  SS05-2L                 PIC 99    VALUE 36.
00671      12  SS05-2D                 PIC X(30) VALUE ALL '*'.
00672      12  SS05-3                  PIC X(4)  VALUE     '05.3'.
00673      12  SS05-3L                 PIC 99    VALUE 36.
00674      12  SS05-3D                 PIC X(30) VALUE ALL '*'.
00675      12  SS05-4                  PIC X(4)  VALUE     '05.4'.
00676      12  SS05-4L                 PIC 99    VALUE 36.
00677      12  SS05-4D                 PIC X(30) VALUE ALL '*'.
00678      12  SS05-5                  PIC X(4)  VALUE     '05.5'.
00679      12  SS05-5L                 PIC 99    VALUE 36.
00680      12  SS05-5D                 PIC X(30) VALUE ALL '*'.
00681      12  SS05-6                  PIC X(4)  VALUE     '05.6'.
00682      12  SS05-6L                 PIC 99    VALUE 36.
00683      12  SS05-6D                 PIC X(30) VALUE ALL '*'.
00684 *****ACCOUNT NAME
00685      12  SS06                    PIC X(4)  VALUE     '06.0'.
00686      12  SS06L                   PIC 99    VALUE 36.
00687      12  SS06D                   PIC X(30) VALUE ALL '*'.
00688 *****FULL ACCOUNT ADDRESS
00689      12  SS07-1                  PIC X(4)  VALUE     '07.1'.
00690      12  SS07-1L                 PIC 99    VALUE 36.
00691      12  SS07-1D                 PIC X(30) VALUE ALL '*'.
00692      12  SS07-2                  PIC X(4)  VALUE     '07.2'.
00693      12  SS07-2L                 PIC 99    VALUE 36.
00694      12  SS07-2D                 PIC X(30) VALUE ALL '*'.
00695      12  SS07-3                  PIC X(4)  VALUE     '07.3'.
00696      12  SS07-3L                 PIC 99    VALUE 36.
00697      12  SS07-3D                 PIC X(30) VALUE ALL '*'.
00698      12  SS07-4                  PIC X(4)  VALUE     '07.4'.
00699      12  SS07-4L                 PIC 99    VALUE 36.
00700      12  SS07-4D                 PIC X(30) VALUE ALL '*'.
00701      12  SS07-5                  PIC X(4)  VALUE     '07.5'.
00702      12  SS07-5L                 PIC 99    VALUE 36.
00703      12  SS07-5D                 PIC X(30) VALUE ALL '*'.
00704 *****ACCOUNT PHONE NUMBER
00705      12  SS07-6                  PIC X(4)  VALUE     '07.6'.
00706      12  SS07-6L                 PIC 99    VALUE 18.
00707      12  SS07-6D                 PIC X(12) VALUE ALL '*'.
00708 *****EXECUTING PROCESSOR NAME
00709      12  SS08                    PIC X(4)  VALUE     '08.0'.
00710      12  SS08L                   PIC 99    VALUE 36.
00711      12  SS08D                   PIC X(30) VALUE ALL '*'.
00712 *****PROCESSOR TITLE
00713      12  SS09                    PIC X(4)  VALUE     '09.0'.
00714      12  SS09L                   PIC 99    VALUE 32.
00715      12  SS09D                   PIC X(26) VALUE ALL '*'.
00716 *****INSUREDS NAME
00717      12  SS10                    PIC X(4)  VALUE     '10.0'.
00718      12  SS10L                   PIC 99    VALUE 36.
00719      12  SS10D                   PIC X(30) VALUE ALL '*'.
00720 *****INSUREDS ADDRESS
00721      12  SS11-1                  PIC X(4)  VALUE     '11.1'.
00722      12  SS11-1L                 PIC 99    VALUE 36.
00723      12  SS11-1D                 PIC X(30) VALUE ALL '*'.
00724      12  SS11-2                  PIC X(4)  VALUE     '11.2'.
00725      12  SS11-2L                 PIC 99    VALUE 36.
00726      12  SS11-2D                 PIC X(30) VALUE ALL '*'.
00727      12  SS11-3                  PIC X(4)  VALUE     '11.3'.
00728      12  SS11-3L                 PIC 99    VALUE 36.
00729      12  SS11-3D                 PIC X(30) VALUE ALL '*'.
00730      12  SS11-4                  PIC X(4)  VALUE     '11.4'.
00731      12  SS11-4L                 PIC 99    VALUE 36.
00732      12  SS11-4D                 PIC X(30) VALUE ALL '*'.
00733 *****INSUREDS NAME FROM ADDR TRAILER
00734      12  SS11-5                  PIC X(4)  VALUE     '11.5'.
00735      12  SS11-5L                 PIC 99    VALUE 36.
00736      12  SS11-5D                 PIC X(30) VALUE ALL '*'.
00737 *****INSUREDS PHONE NUMBER FROM ADDR TRAILER
00738      12  SS11-6                  PIC X(4)  VALUE     '11.6'.
00739      12  SS11-6L                 PIC 99    VALUE 18.
00740      12  SS11-6D                 PIC X(12) VALUE ALL '*'.
00741 *****CLAIM TYPE NAME
00742      12  SS12                    PIC X(4)  VALUE     '12.0'.
00743      12  SS12L                   PIC 99    VALUE 12.
00744      12  SS12D                   PIC X(6)  VALUE ALL '*'.
00745 *****CLAIM INCURRED DATE
00746      12  SS13                    PIC X(4)  VALUE     '13.0'.
00747      12  SS13L                   PIC 99    VALUE 14.
00748      12  SS13D                   PIC X(8)  VALUE ALL '*'.
00749 *****CLAIM REPORTED DATE
00750      12  SS14                    PIC X(4)  VALUE     '14.0'.
00751      12  SS14L                   PIC 99    VALUE 14.
00752      12  SS14D                   PIC X(8)  VALUE ALL '*'.
00753 *****LAST PAYMENT DATE
00754      12  SS15                    PIC X(4)  VALUE     '15.0'.
00755      12  SS15L                   PIC 99    VALUE 14.
00756      12  SS15D                   PIC X(8)  VALUE ALL '*'.
00757 *****LAST PAYMENT AMOUNT
00758      12  SS16                    PIC X(4)  VALUE     '16.0'.
00759      12  SS16L                   PIC 99    VALUE 17.
00760      12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.
060109*****CLAIM INTEREST RATE
060109     12  SS16-1                  PIC X(4)  VALUE     '16.1'.
060109     12  SS16-1L                 PIC 99    VALUE 13.
060109     12  SS16-1D                 PIC X(7)        VALUE SPACE.
00761 *****CLAIM PAID THRU/TO DATE
00762      12  SS17                    PIC X(4)  VALUE     '17.0'.
00763      12  SS17L                   PIC 99    VALUE 14.
00764      12  SS17D                   PIC X(8)  VALUE ALL '*'.
00765 *****TOTAL PAID TO DATE
00766      12  SS18                    PIC X(4)  VALUE     '18.0'.
00767      12  SS18L                   PIC 99    VALUE 17.
00768      12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.
00769 *****DIAGNOSIS OR CAUSE
00770      12  SS19                    PIC X(4)  VALUE     '19.0'.
00771      12  SS19L                   PIC 99    VALUE 32.
00772      12  SS19D                   PIC X(26) VALUE ALL '*'.
00773 *****CAUSE CODE
00774      12  SS19-1                  PIC X(4)  VALUE     '19.1'.
00775      12  SS19-1L                 PIC 99    VALUE 12.
00776      12  SS19-1D                 PIC X(6)  VALUE ALL '*'.
PEMMOD*****CID LOAN NUMBER
PEMMOD     12  SS19-2                  PIC X(4)  VALUE     '19.2'.
PEMMOD     12  SS19-2L                 PIC 99    VALUE 31.
PEMMOD     12  SS19-2D                 PIC X(25) VALUE ALL '*'.
00777 *****CURRENT DATE
00778      12  SS20                    PIC X(4)  VALUE     '20.0'.
00779      12  SS20L                   PIC 99    VALUE 14.
00780      12  SS20D                   PIC X(8)  VALUE ALL '*'.
00781 *****FULL CURRENT DATE
00782      12  SS21                    PIC X(4)  VALUE     '21.0'.
00783      12  SS21L                   PIC 99    VALUE 24.
00784      12  SS21D                   PIC X(18) VALUE ALL '*'.
00785 *****BENEFIT DESCRIPTION
00786      12  SS22                    PIC X(4)  VALUE     '22.0'.
00787      12  SS22L                   PIC 99    VALUE 16.
00788      12  SS22D                   PIC X(10) VALUE ALL '*'.
00789 *****CARRIER CODE IN CERT
00790      12  SS23                    PIC X(4)  VALUE     '23.0'.
00791      12  SS23L                   PIC 99    VALUE 9.
00792      12  SS23D                   PIC XXX   VALUE ALL '*'.
00793 *****GROUPING CODE IN CERT
00794      12  SS24                    PIC X(4)  VALUE     '24.0'.
00795      12  SS24L                   PIC 99    VALUE 12.
00796      12  SS24D                   PIC X(6)  VALUE ALL '*'.
00797 *****ACCOUNT NUMBER IN CERT
00798      12  SS25                    PIC X(4)  VALUE     '25.0'.
00799      12  SS25L                   PIC 99    VALUE 16.
00800      12  SS25D                   PIC X(10) VALUE ALL '*'.
00801 *****CERTIFICATE NUMBER
00802      12  SS26                    PIC X(4)  VALUE     '26.0'.
00803      12  SS26L                   PIC 99    VALUE 17.
00804      12  SS26D                   PIC X(11) VALUE ALL '*'.
00805 *****CERT EFFECTIVE DATE
00806      12  SS27                    PIC X(4)  VALUE     '27.0'.
00807      12  SS27L                   PIC 99    VALUE 14.
00808      12  SS27D                   PIC X(8)  VALUE ALL '*'.
00809 *****CERT EXPIRATION DATE
00810      12  SS28                    PIC X(4)  VALUE     '28.0'.
00811      12  SS28L                   PIC 99    VALUE 14.
00812      12  SS28D                   PIC X(8)  VALUE ALL '*'.
00813 *****APPLICABLE COVERAGE TERM
00814      12  SS29                    PIC X(4)  VALUE     '29.0'.
00815      12  SS29L                   PIC 99    VALUE 9.
00816      12  SS29D                   PIC XXX   VALUE ALL '*'.
00817 *****APPLICABLE COVERAGE AMOUNT
00818      12  SS30                    PIC X(4)  VALUE     '30.0'.
00819      12  SS30L                   PIC 99    VALUE 18.
00820      12  SS30D                   PIC $$$$$,$$$.99  VALUE ZEROS.
00821 *****APPLICABLE COVERAGE CANCEL DATE
00822      12  SS31                    PIC X(4)  VALUE     '31.0'.
00823      12  SS31L                   PIC 99    VALUE 14.
00824      12  SS31D                   PIC X(8)  VALUE ALL '*'.
00825 *****APPLICABLE COVERAGE FORM NUMBER
00826      12  SS32                    PIC X(4)  VALUE     '32.0'.
00827      12  SS32L                   PIC 99    VALUE 18.
00828      12  SS32D                   PIC X(12) VALUE ALL '*'.
00829 *****INSURES AGE AT POLICY ISSUE
00830      12  SS33                    PIC X(4)  VALUE     '33.0'.
00831      12  SS33L                   PIC 99    VALUE 9.
00832      12  SS33D                   PIC XXX   VALUE ALL '*'.
00833 *****CLAIM NUMBER
00834      12  SS34                    PIC X(4)  VALUE     '34.0'.
00835      12  SS34L                   PIC 99    VALUE 13.
00836      12  SS34D                   PIC X(7)  VALUE ALL '*'.
00837 *****LAST DENIAL TEXT
00838      12  SS35-1                  PIC X(4)  VALUE     '35.1'.
00839      12  SS35-1L                 PIC 99    VALUE 66.
00840      12  SS35-1D                 PIC X(60) VALUE ALL '*'.
00841      12  SS35-2                  PIC X(4)  VALUE     '35.2'.
00842      12  SS35-2L                 PIC 99    VALUE 66.
00843      12  SS35-2D                 PIC X(60) VALUE ALL '*'.
00844 *****LOAN NUMBER
00845      12  SS36                    PIC X(4)  VALUE     '36.0'.
00846      12  SS36L                   PIC 99    VALUE 14.
00847      12  SS36D                   PIC X(8)  VALUE ALL '*'.
00848 *****CURRENT LOAN NUMBER
00849      12  SS36-1                  PIC X(4)  VALUE     '36.1'.
00850      12  SS36-1L                 PIC 99    VALUE 26.
00851      12  SS36-1D                 PIC X(20) VALUE ALL '*'.
00852 *****LOAN BALANCE
00853      12  SS37                    PIC X(4)  VALUE     '37.0'.
00854      12  SS37L                   PIC 99    VALUE 18.
00855      12  SS37D                   PIC $$$$$,$$$.99  VALUE ZEROS.
00856 *****MEMBER NUMBER
00857      12  SS38                    PIC X(4)  VALUE     '38.0'.
00858      12  SS38L                   PIC 99    VALUE 18.
00859      12  SS38D                   PIC X(12) VALUE ALL '*'.
00860 *****INSURED NAME (FIRST M LAST)
00861      12  SS39                    PIC X(4)  VALUE     '39.0'.
00862      12  SS39L                   PIC 99    VALUE 36.
00863      12  SS39D                   PIC X(30) VALUE ALL '*'.
00864 *****INSURED LAST NAME ONLY
00865      12  SS40                    PIC X(4)  VALUE     '40.0'.
00866      12  SS40L                   PIC 99    VALUE 21.
00867      12  SS40D                   PIC X(15) VALUE ALL '*'.
00868 *****TITLE (MR/MS)
00869      12  SS41                    PIC X(4)  VALUE     '41.0'.
00870      12  SS41L                   PIC 99    VALUE 9.
00871      12  SS41D                   PIC X(3)  VALUE ALL '*'.
00872 *****ELIMINATION PERIOD
00873      12  SS42                    PIC X(4)  VALUE     '42.0'.
00874      12  SS42L                   PIC 99    VALUE 9.
00875      12  SS42D                   PIC X(3)  VALUE ALL '*'.
00876 *****BENEFICIARY NAME
00877      12  SS43                    PIC X(4)  VALUE     '43.0'.
00878      12  SS43L                   PIC 99    VALUE 36.
00879      12  SS43D                   PIC X(30) VALUE ALL '*'.
00880 *****BENEFICIARY ADDRESS
00881      12  SS44-1                  PIC X(4)  VALUE     '44.1'.
00882      12  SS44-1L                 PIC 99    VALUE 36.
00883      12  SS44-1D                 PIC X(30) VALUE ALL '*'.
00884      12  SS44-2                  PIC X(4)  VALUE     '44.2'.
00885      12  SS44-2L                 PIC 99    VALUE 36.
00886      12  SS44-2D                 PIC X(30) VALUE ALL '*'.
00887      12  SS44-3                  PIC X(4)  VALUE     '44.3'.
00888      12  SS44-3L                 PIC 99    VALUE 36.
00889      12  SS44-3D                 PIC X(30) VALUE ALL '*'.
00890      12  SS44-4                  PIC X(4)  VALUE     '44.4'.
00891      12  SS44-4L                 PIC 99    VALUE 36.
00892      12  SS44-4D                 PIC X(30) VALUE ALL '*'.
00893      12  SS44-5                  PIC X(4)  VALUE     '44.5'.
00894      12  SS44-5L                 PIC 99    VALUE 18.
00895      12  SS44-5D                 PIC X(12) VALUE ALL '*'.
00896      12  SS44-6                  PIC X(4)  VALUE     '44.6'.
00897      12  SS44-6L                 PIC 99    VALUE 36.
00898      12  SS44-6D                 PIC X(30) VALUE ALL '*'.
00899 *****INSUREDS DATE OF BIRTH
00900      12  SS45                    PIC X(4)  VALUE     '45.0'.
00901      12  SS45L                   PIC 99    VALUE 14.
00902      12  SS45D                   PIC X(8)  VALUE ALL '*'.
00903 *****INSUREDS SOC SEC NUMBER
00904      12  SS46                    PIC X(4)  VALUE     '46.0'.
00905      12  SS46L                   PIC 99    VALUE 17.
00906      12  SS46D                   PIC X(11) VALUE ALL '*'.
00907 *****PHYSICIANS  NAME
00908      12  SS47                    PIC X(4)  VALUE     '47.0'.
00909      12  SS47L                   PIC 99    VALUE 36.
00910      12  SS47D                   PIC X(30) VALUE ALL '*'.
00911 *****PHYSICIANS  ADDRESS
00912      12  SS47-1                  PIC X(4)  VALUE     '47.1'.
00913      12  SS47-1L                 PIC 99    VALUE 36.
00914      12  SS47-1D                 PIC X(30) VALUE ALL '*'.
00915      12  SS47-2                  PIC X(4)  VALUE     '47.2'.
00916      12  SS47-2L                 PIC 99    VALUE 36.
00917      12  SS47-2D                 PIC X(30) VALUE ALL '*'.
00918      12  SS47-3                  PIC X(4)  VALUE     '47.3'.
00919      12  SS47-3L                 PIC 99    VALUE 36.
00920      12  SS47-3D                 PIC X(30) VALUE ALL '*'.
00921      12  SS47-4                  PIC X(4)  VALUE     '47.4'.
00922      12  SS47-4L                 PIC 99    VALUE 36.
00923      12  SS47-4D                 PIC X(30) VALUE ALL '*'.
00924      12  SS47-5                  PIC X(4)  VALUE     '47.5'.
00925      12  SS47-5L                 PIC 99    VALUE 18.
00926      12  SS47-5D                 PIC X(12) VALUE ALL '*'.
00927 *****EMPLOYERS   NAME
00928      12  SS48                    PIC X(4)  VALUE     '48.0'.
00929      12  SS48L                   PIC 99    VALUE 36.
00930      12  SS48D                   PIC X(30) VALUE ALL '*'.
00931 *****EMPLOYERS   ADDRESS
00932      12  SS48-1                  PIC X(4)  VALUE     '48.1'.
00933      12  SS48-1L                 PIC 99    VALUE 36.
00934      12  SS48-1D                 PIC X(30) VALUE ALL '*'.
00935      12  SS48-2                  PIC X(4)  VALUE     '48.2'.
00936      12  SS48-2L                 PIC 99    VALUE 36.
00937      12  SS48-2D                 PIC X(30) VALUE ALL '*'.
00938      12  SS48-3                  PIC X(4)  VALUE     '48.3'.
00939      12  SS48-3L                 PIC 99    VALUE 36.
00940      12  SS48-3D                 PIC X(30) VALUE ALL '*'.
00941      12  SS48-4                  PIC X(4)  VALUE     '48.4'.
00942      12  SS48-4L                 PIC 99    VALUE 36.
00943      12  SS48-4D                 PIC X(30) VALUE ALL '*'.
00944      12  SS48-5                  PIC X(4)  VALUE     '48.5'.
00945      12  SS48-5L                 PIC 99    VALUE 18.
00946      12  SS48-5D                 PIC X(12) VALUE ALL '*'.
00947 *****OTHER1      NAME
00948      12  SS49                    PIC X(4)  VALUE     '49.0'.
00949      12  SS49L                   PIC 99    VALUE 36.
00950      12  SS49D                   PIC X(30) VALUE ALL '*'.
00951 *****OTHER1      ADDRESS
00952      12  SS49-1                  PIC X(4)  VALUE     '49.1'.
00953      12  SS49-1L                 PIC 99    VALUE 36.
00954      12  SS49-1D                 PIC X(30) VALUE ALL '*'.
00955      12  SS49-2                  PIC X(4)  VALUE     '49.2'.
00956      12  SS49-2L                 PIC 99    VALUE 36.
00957      12  SS49-2D                 PIC X(30) VALUE ALL '*'.
00958      12  SS49-3                  PIC X(4)  VALUE     '49.3'.
00959      12  SS49-3L                 PIC 99    VALUE 36.
00960      12  SS49-3D                 PIC X(30) VALUE ALL '*'.
00961      12  SS49-4                  PIC X(4)  VALUE     '49.4'.
00962      12  SS49-4L                 PIC 99    VALUE 36.
00963      12  SS49-4D                 PIC X(30) VALUE ALL '*'.
00964      12  SS49-5                  PIC X(4)  VALUE     '49.5'.
00965      12  SS49-5L                 PIC 99    VALUE 18.
00966      12  SS49-5D                 PIC X(12) VALUE ALL '*'.
00967 *****OTHER2      NAME
00968      12  SS50                    PIC X(4)  VALUE     '50.0'.
00969      12  SS50L                   PIC 99    VALUE 36.
00970      12  SS50D                   PIC X(30) VALUE ALL '*'.
00971 *****OTHER2      ADDRESS
00972      12  SS50-1                  PIC X(4)  VALUE     '50.1'.
00973      12  SS50-1L                 PIC 99    VALUE 36.
00974      12  SS50-1D                 PIC X(30) VALUE ALL '*'.
00975      12  SS50-2                  PIC X(4)  VALUE     '50.2'.
00976      12  SS50-2L                 PIC 99    VALUE 36.
00977      12  SS50-2D                 PIC X(30) VALUE ALL '*'.
00978      12  SS50-3                  PIC X(4)  VALUE     '50.3'.
00979      12  SS50-3L                 PIC 99    VALUE 36.
00980      12  SS50-3D                 PIC X(30) VALUE ALL '*'.
00981      12  SS50-4                  PIC X(4)  VALUE     '50.4'.
00982      12  SS50-4L                 PIC 99    VALUE 36.
00983      12  SS50-4D                 PIC X(30) VALUE ALL '*'.
00984      12  SS50-5                  PIC X(4)  VALUE     '50.5'.
00985      12  SS50-5L                 PIC 99    VALUE 18.
00986      12  SS50-5D                 PIC X(12) VALUE ALL '*'.
00987 *****A&H TERM TIMES MON. BEN.
00988      12  SS51                    PIC X(4)  VALUE     '51.0'.
00989      12  SS51L                   PIC 99    VALUE 17.
00990      12  SS51D                   PIC $$$$,$$$.99 VALUE ZEROS.
00991 *****THIRD PARTY NAME
00992      12  SS52                    PIC X(4)  VALUE     '52.0'.
00993      12  SS52L                   PIC 99    VALUE 36.
00994      12  SS52D                   PIC X(30) VALUE ALL '*'.
00995 *****THIRD PARTY ADDRESS
00996      12  SS53-1                  PIC X(4)  VALUE     '53.1'.
00997      12  SS53-1L                 PIC 99    VALUE 36.
00998      12  SS53-1D                 PIC X(30) VALUE ALL '*'.
00999      12  SS53-2                  PIC X(4)  VALUE     '53.2'.
01000      12  SS53-2L                 PIC 99    VALUE 36.
01001      12  SS53-2D                 PIC X(30) VALUE ALL '*'.
01002      12  SS53-3                  PIC X(4)  VALUE     '53.3'.
01003      12  SS53-3L                 PIC 99    VALUE 36.
01004      12  SS53-3D                 PIC X(30) VALUE ALL '*'.
01005      12  SS53-4                  PIC X(4)  VALUE     '53.4'.
01006      12  SS53-4L                 PIC 99    VALUE 36.
01007      12  SS53-4D                 PIC X(30) VALUE ALL '*'.
01008      12  SS53-5                  PIC X(4)  VALUE     '53.5'.
01009      12  SS53-5L                 PIC 99    VALUE 36.
01010      12  SS53-5D                 PIC X(30) VALUE ALL '*'.
01011 *****THIRD PARTY PHONE NUMBER
01012      12  SS53-6                  PIC X(4)  VALUE     '53.6'.
01013      12  SS53-6L                 PIC 99    VALUE 18.
01014      12  SS53-6D                 PIC X(12) VALUE ALL '*'.
01015 *****CERTIFICATE SEQUENCE
01016      12  SS54                    PIC X(4)  VALUE     '54.0'.
01017      12  SS54L                   PIC 99    VALUE 09.
01018      12  SS54D                   PIC XXX VALUE ALL '*'.
01019 *****CERTIFICATE TOTAL  E
01020      12  SS55                    PIC X(4)  VALUE     '55.0'.
01021      12  SS55L                   PIC 99    VALUE 09.
01022      12  SS55D                   PIC XXX VALUE ALL '*'.
01023 *****CREDITOR ID
01024      12  SS56                    PIC  X(04) VALUE    '56.0'.
01025      12  SS56L                   PIC  99 VALUE 36.
01026      12  SS56D                   PIC  X(30) VALUE ALL '*'.
01027 *****INSUREDS NAME (CERTIFICATE)
01028      12  SS57                    PIC X(4)  VALUE     '57.0'.
01029      12  SS57L                   PIC 99    VALUE 36.
01030      12  SS57D                   PIC X(30) VALUE ALL '*'.
01031 *****JOINT NAME (CERTIFICATE)
01032      12  SS58                    PIC X(4)  VALUE     '58.0'.
01033      12  SS58L                   PIC 99    VALUE 36.
01034      12  SS58D                   PIC X(30) VALUE ALL '*'.
01035 *****POLICY REFERENCE NUMBER
01036      12  SS59                    PIC X(4)  VALUE     '59.0'.
01037      12  SS59L                   PIC 99    VALUE 26.
01038      12  SS59D                   PIC X(20) VALUE ALL '*'.
01039 *****CREDIT CARD NUMBER (CLAIMS)
01040      12  SS60                    PIC X(4)  VALUE     '60.0'.
01041      12  SS60L                   PIC 99    VALUE 22.
01042      12  SS60D                   PIC X(16) VALUE ALL '*'.
01043 *****CORRESPONDENCE BENEFICIARY NAME
01044      12  SS61                    PIC X(4)  VALUE     '61.0'.
01045      12  SS61L                   PIC 99    VALUE 36.
01046      12  SS61D                   PIC X(30) VALUE ALL '*'.
01047 *****CORRESPONDENCE BENEFICIARY ADDRESS
01048      12  SS61-1                  PIC X(4)  VALUE     '61.1'.
01049      12  SS61-1L                 PIC 99    VALUE 36.
01050      12  SS61-1D                 PIC X(30) VALUE ALL '*'.
01051      12  SS61-2                  PIC X(4)  VALUE     '61.2'.
01052      12  SS61-2L                 PIC 99    VALUE 36.
01053      12  SS61-2D                 PIC X(30) VALUE ALL '*'.
01054      12  SS61-3                  PIC X(4)  VALUE     '61.3'.
01055      12  SS61-3L                 PIC 99    VALUE 36.
01056      12  SS61-3D                 PIC X(30) VALUE ALL '*'.
01057      12  SS61-4                  PIC X(4)  VALUE     '61.4'.
01058      12  SS61-4L                 PIC 99    VALUE 36.
01059      12  SS61-4D                 PIC X(30) VALUE ALL '*'.
01060      12  SS61-5                  PIC X(4)  VALUE     '61.5'.
01061      12  SS61-5L                 PIC 99    VALUE 36.
01062      12  SS61-5D                 PIC X(30) VALUE ALL '*'.
01063      12  SS61-6                  PIC X(4)  VALUE     '61.6'.
01064      12  SS61-6L                 PIC 99    VALUE 18.
01065      12  SS61-6D                 PIC X(12) VALUE ALL '*'.
01066
01067 *****DMD UNDERWRITER STATEMENT
01068      12  SS62                    PIC X(4)  VALUE     '62.0'.
01069      12  SS62L                   PIC 99    VALUE 56.
01070      12  SS62D                   PIC X(50) VALUE ALL '*'.
01071
01072 *****DMD UNDERWRITER NAME
01073      12  SS63                    PIC X(4)  VALUE     '63.0'.
01074      12  SS63L                   PIC 99    VALUE 66.
01075      12  SS63D                   PIC X(60) VALUE ALL '*'.
01076
01077 *****DMD UNDERWRITER NAME
01078      12  SS64                    PIC X(4)  VALUE     '64.0'.
01079      12  SS64L                   PIC 99    VALUE 66.
01080      12  SS64D                   PIC X(60) VALUE ALL '*'.
01081
01082 ****************************************************
01083 *       WHEN ADDING OR DELETING ENTRIES TO         *
01084 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *
01085 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *
01086 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *
01087 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *
01088 *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *
01089 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *
01090 ****************************************************
01208 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
01209 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA
01210
PEMMOD 01  SYSTEM-VARIABLES            PIC X(3865).
01212  01  SYS-VAR-ENTRY.
01213      12  SYS-VAR-CODE            PIC X(4).
01214      12  SYS-VAR-LEN             PIC 99.
01215      12  SYS-VAR-DATA            PIC X(100).
01216
01092 *                                COPY ELCDATE.
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
01093
01095 *                                COPY ELCLOGOF.
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
01096
01097      EJECT
01098 *                                COPY ELCNWA.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNWA.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                         *
00006 *                                                               *
00007 *            M O V E   N A M E   W O R K   A R E A.             *
00008 *                                                               *
00009 *****************************************************************.
00010
00011  01  WS-NAME-WORK-AREA.
00012      05  WS-INSURED-LAST-NAME        PIC X(15).
00013      05  WS-INSURED-1ST-NAME         PIC X(12).
00014      05  WS-INSURED-MID-INIT         PIC X.
00015
00016      05  WS-NAME-WORK.
00017          10  WS-NW                   PIC X
00018              OCCURS 30 TIMES INDEXED BY NWA-INDEX.
00019
00020      05  WS-NAME-WORK2.
00021          10  WS-NW2                  PIC X
00022              OCCURS 20 TIMES INDEXED BY NWA-INDEX2 NWA-INDEX3
00023                                         NWA-INDEX0.
00024
00025      05  WS-NAME-SW                  PIC S9          VALUE ZERO
00026                                      COMP-3.
00027
01099      EJECT
01100 *                                COPY ELCATTR.
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
01101      EJECT
01102 *                                COPY ELCEMIB.
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
01103  01  EMI-SAVE-AREA               PIC X(400).
01104      EJECT
01105 *                                COPY ELCINTF.
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
01106      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
01107 *        COPY ELC1042.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                           ELC1042                              *
00004 *                            VMOD=2.002                          *
00005 *                                                                *
00006 *    NOTE                                                        *
00007 *        THE WORK AREA IS USED BY EL152, EL1522, EL1042, EL153,  *
00008 *        EM152, EM1522, EL689, EL6892, EL6311, AND EL690.        *
00009 *        THIS COPYBOOK SHOULD NOT BE CHANGED WITHOUT REFERENCE   *
00010 *        TO THESE PROGRAMS.                                      *
00011 *                                                                *
00012 *    NOTE                                                        *
00013 *        THE FILLER AREA AT THE BOTTOM ARE FOR FUTURE EL1042     *
00014 *        USE ONLY!                                               *
00015 *                                                                *
00016 ******************************************************************
00017
00018          16  PI-1042-WA.
00019              20  PI-ACTION       PIC  X(01).
00020                  88 PI-SHOW-MODE           VALUE '1'.
00021                  88 PI-CLEAR-MODE          VALUE '2'.
00022                  88 PI-CREATE-MODE         VALUE '3'.
00023              20  PI-COMM-CONTROL PIC  X(12).
00024              20  PI-CURRENT-LINE PIC S9(03) COMP-3.
00025              20  PI-EOF-SW       PIC  X(01).
00026                  88  PI-FILE-EOF           VALUE 'Y'.
00027              20  PI-FILETYP      PIC  X(01).
00028              20  PI-FORM-SQUEEZE-CONTROL
00029                                  PIC  X(01).
00030                  88  PI-FORM-SQUEEZE-ON     VALUE 'Y'.
00031                  88  PI-FORM-SQUEEZE-OFF    VALUE ' '.
00032              20  PI-LAST-CONTROL PIC  X(12).
00033              20  PI-TEMP-STOR-ITEMS
00034                                  PIC S9(04) COMP.
00035              20  PI-TOTAL-LINES  PIC S9(03) COMP-3.
00036              20  PI-UPDATE-SW    PIC  9(01).
00037                  88 ANY-UPDATES            VALUE 1.
00038              20  PI-104-SCREEN-SENT-IND
00039                                  PIC  X(01).
00040                  88  PI-104-SCREEN-SENT    VALUE 'Y'.
00041                  88  PI-104-SCREEN-NOT-SENT VALUE 'N'.
00042              20  PI-1042-SCREEN-SENT-IND
00043                                  PIC  X(01).
00044                  88  PI-1042-SCREEN-SENT    VALUE 'Y'.
00045                  88  PI-1042-SCREEN-NOT-SENT VALUE 'N'.
00046              20  PI-1042-ARCHIVE-IND
00047                                  PIC  X(01).
00048                  88  PI-1042-ARCHIVE-LETTER VALUE 'Y'.
00049              20  FILLER          PIC  X(29).
01108 **********************************************************
01109 *    NOTE                                                *
01110 *        THE WORK AREA IS USED BY EL152 AND EL1522       *
01111 *        AND CANNOT BE REARRANGED WITHOUT COMPILING      *
01112 *        BOTH PROGRAMS.                                  *
01113 *                                                        *
01114 **********************************************************
01115          16  PI-EL152-WA.
01116              20  PI-ALT-PRINTER-ID        PIC X(4).
01117              20  PI-ARCHIVE-NUMBER        PIC 9(8).
01118              20  PI-FORM-NUMBER           PIC X(4).
01119              20  PI-ADDR-TYPE             PIC XX.
01120              20  PI-TEMP-STOR-ID          PIC X(8).
01121              20  PI-NUM-PRINT-COPIES      PIC 9.
01122              20  PI-ADDR-SEQ              PIC S9(4)   COMP.
01123              20  PI-PRINT-SW              PIC X.
01124                  88 PRINT-PERFORMED  VALUE '1'.
01125              20  PI-LETTER-ADDRESS-TYPE   PIC XX.
040110             20  PI-RESEND-FORM-NUMBER    PIC X(4).
040110             20  PI-PROMPT-LETTER         PIC X.
040110             20  PI-ENCLOSURE-CD          PIC X(3).
040110             20  PI-AUTO-CLOSE-IND        PIC X(1).
040110             20  PI-LETTER-TO-BENE        PIC X(1).
040110             20  PI-FILLER                PIC X(528).
01127              20  PI-FORCE-7840            PIC X.
01128                  88 FORCE-7840       VALUE '1'.
01129
01130              20  PI-BSR-LETTER-IND        PIC XX.
01131                  88  PI-BSR-AUTOMATED VALUE 'BA'.
01132              20  FILLER REDEFINES PI-BSR-LETTER-IND.
01133                  22  PI-ELLETR-BSR        PIC X.
01134                  22  PI-ELBENE-BSR        PIC X.
01135
01136      EJECT
      *                            COPY DFHBMSCA.
      *****************************************************************
      *                                                               *
      * Copyright (c) 2007-2013 Dell Inc.                             *
      * All rights reserved.                                          *
      *                                                               *
      *****************************************************************
       01    DFHBMSCA.
      *        VARIABLE  PROTECTION    INTENSITY    MODIFIED
      *                                             DATA TAG
      *
      *        DFHBMUNP  Unprotected   Normal       Off
           02  DFHBMUNP PIC X VALUE SPACE.
      *        DFHBMUNN  Numeric       Normal       Off
           02  DFHBMUNN PIC X VALUE "&".
      *        DFHBMPRO  Protected     Normal       Off
           02  DFHBMPRO PIC X VALUE "-".
      *        DFHBMASK  Autoskip      Normal       Off
           02  DFHBMASK PIC X VALUE "0".
      *************************************************
      *        DFHBMBRY  Unprotected   Bright       Off
           02  DFHBMBRY PIC X VALUE "H".
      *        DFHPROTI  Protected     Bright       Off
           02  DFHPROTI PIC X VALUE "Y".
      *        DFHBMASB  Autoskip      Bright       Off
           02  DFHBMASB PIC X VALUE "8".
      *************************************************
      *        DFHBMDAR  Unprotected   Non-Display  Off
           02  DFHBMDAR PIC X VALUE "<".
      *        DFHPROTN  Protected     Non-Display  Off
           02  DFHPROTN PIC X VALUE "%".
      *************************************************
      *        DFHBMFSE  Unprotected   Normal       On
           02  DFHBMFSE PIC X VALUE "A".
      *        DFHBMASF  Autoskip      Normal       On
           02  DFHBMASF  PIC X VALUE "1".
      *        DFHUNNUM  Numeric       Normal       On
           02  DFHUNNUM PIC X VALUE "J".
      *        DFHBMPRF  Protected     Normal       On
           02  DFHBMPRF PIC X VALUE "/".
      *************************************************
      *        DFHUNIMD  Unprotected   Bright       On
           02  DFHUNIMD PIC X VALUE "I".
      *        DFHUNINT  Numeric       Bright       On
           02  DFHUNINT PIC X VALUE "R".
      *************************************************
      *        DFHUNNOD  Unprotected   Non-Display  On
           02  DFHUNNOD PIC X VALUE "(".
      *        DFHUNNON  Numeric       Non-Display  On
           02  DFHUNNON PIC X VALUE ")".
      *************************************************
      *
      *     COLOURS
      *
           02  DFHDFCOL PIC X VALUE X"00".
           02  DFHBLUE  PIC X VALUE "1".
           02  DFHRED   PIC X VALUE "2".
           02  DFHPINK  PIC X VALUE "3".
           02  DFHGREEN PIC X VALUE "4".
           02  DFHTURQ  PIC X VALUE "5".
           02  DFHYELLO PIC X VALUE "6".
           02  DFHNEUTR PIC X VALUE "7".
           02  DFH3270  PIC X VALUE "{".
           02  DFHALL   PIC X VALUE LOW-VALUE.
           02  DFHBASE  PIC X VALUE LOW-VALUE.
           02  DFHBLINK PIC X VALUE "1".
           02  DFHBMDET PIC X VALUE LOW-VALUE.
           02  DFHBMEOF PIC X VALUE X"80".
           02  DFHBMPEM PIC X VALUE X"19".
      * Newline, DFHBMPNL, is changed to be an ASCII 0x0A
           02  DFHBMPNL PIC X VALUE X"0A".
           02  DFHCOLOR PIC X VALUE "!".
           02  DFHDFHI  PIC X VALUE LOW-VALUE.
           02  DFHDFT   PIC X VALUE X"FF".
           02  DFHERROR PIC X VALUE X"1A".
           02  DFHHLT   PIC X VALUE " ".
           02  DFHMENT  PIC X VALUE X"02".
           02  DFHBMCUR  PIC X VALUE X"02".
           02  DFHBMFLG  PIC X.
               88 DFHCURSR VALUES ARE X"02" , X"82".
           02  DFHMET   PIC X VALUE X"03".
           02  DFHMFE   PIC X VALUE X"06".
           02  DFHMFET  PIC X VALUE LOW-VALUE.
           02  DFHMFIL  PIC X VALUE X"1C".
           02  DFHMFT   PIC X VALUE X"09".
           02  DFHMT    PIC X VALUE X"01".
           02  DFHPS    PIC X VALUE '"'.
           02  DFHREVRS PIC X VALUE "2".
           02  DFHSA    PIC X VALUE X"08".
           02  DFHUNDLN PIC X VALUE "4".
           02  DFHVAL   PIC X VALUE "A".
           02  DFHLEFT  PIC X VALUE X"97".
           02  DFHOVER  PIC X VALUE X"9C".
           02  DFHRIGHT PIC X VALUE X"02".
           02  DFHUNDER PIC X VALUE X"01".
           02  DFHBOX-BIN  PIC 9(4) COMP VALUE 15.
           02  FILLER REDEFINES DFHBOX-BIN.
               03  FILLER PIC X.
               03  DFHBOX PIC X.
           02  DFHSOSI  PIC X VALUE X"01".
           02  DFHBMPSO-BIN    PIC 9(4) COMP VALUE 3599.
           02  FILLER REDEFINES DFHBMPSO-BIN.
               03 DFHBMPSO  PIC X.
               03 DFHBMPSI  PIC X.
           02  DFHBMEC  PIC X VALUE X"82".
      *    02  DFHDFFR  PIC X VALUE X"00".
      *    02  DFHOPAQ  PIC X VALUE X"00".
      *    02  DFHOUTLN PIC X VALUE X"00".
      *    02  DFHTRANS PIC X VALUE X"00".
      *    02  DFHBKTRN PIC X VALUE X"00".
01137 *                            COPY ELCAID.
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
01138  01  FILLER    REDEFINES DFHAID.
01139      12  FILLER              PIC X(8).
01140      12  PF-VALUES           PIC X       OCCURS 2.
01141      EJECT
01142 *                            COPY EL152S.
       01  EL152AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  DATEAL PIC S9(0004) COMP.
           05  DATEAF PIC  X(0001).
           05  FILLER REDEFINES DATEAF.
               10  DATEAA PIC  X(0001).
           05  DATEAI PIC  X(0008).
      *    -------------------------------
           05  TIMEAL PIC S9(0004) COMP.
           05  TIMEAF PIC  X(0001).
           05  FILLER REDEFINES TIMEAF.
               10  TIMEAA PIC  X(0001).
           05  TIMEAI PIC  X(0005).
      *    -------------------------------
           05  CARRL PIC S9(0004) COMP.
           05  CARRF PIC  X(0001).
           05  FILLER REDEFINES CARRF.
               10  CARRA PIC  X(0001).
           05  CARRI PIC  X(0001).
      *    -------------------------------
           05  CLMNOL PIC S9(0004) COMP.
           05  CLMNOF PIC  X(0001).
           05  FILLER REDEFINES CLMNOF.
               10  CLMNOA PIC  X(0001).
           05  CLMNOI PIC  X(0007).
      *    -------------------------------
           05  CRTNOL PIC S9(0004) COMP.
           05  CRTNOF PIC  X(0001).
           05  FILLER REDEFINES CRTNOF.
               10  CRTNOA PIC  X(0001).
           05  CRTNOI PIC  X(0011).
      *    -------------------------------
           05  PROCL PIC S9(0004) COMP.
           05  PROCF PIC  X(0001).
           05  FILLER REDEFINES PROCF.
               10  PROCA PIC  X(0001).
           05  PROCI PIC  X(0004).
      *    -------------------------------
           05  COMPIDL PIC S9(0004) COMP.
           05  COMPIDF PIC  X(0001).
           05  FILLER REDEFINES COMPIDF.
               10  COMPIDA PIC  X(0001).
           05  COMPIDI PIC  X(0003).
      *    -------------------------------
           05  CLEANL PIC S9(0004) COMP.
           05  CLEANF PIC  X(0001).
           05  FILLER REDEFINES CLEANF.
               10  CLEANA PIC  X(0001).
           05  CLEANI PIC  X(0001).
      *    -------------------------------
           05  SYSL PIC S9(0004) COMP.
           05  SYSF PIC  X(0001).
           05  FILLER REDEFINES SYSF.
               10  SYSA PIC  X(0001).
           05  SYSI PIC  X(0008).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  ARCHNUML PIC S9(0004) COMP.
           05  ARCHNUMF PIC  X(0001).
           05  FILLER REDEFINES ARCHNUMF.
               10  ARCHNUMA PIC  X(0001).
           05  ARCHNUMI PIC  99999999.
      *    -------------------------------
           05  FORML PIC S9(0004) COMP.
           05  FORMF PIC  X(0001).
           05  FILLER REDEFINES FORMF.
               10  FORMA PIC  X(0001).
           05  FORMI PIC  X(0004).
      *    -------------------------------
           05  FOLLOWL PIC S9(0004) COMP.
           05  FOLLOWF PIC  X(0001).
           05  FILLER REDEFINES FOLLOWF.
               10  FOLLOWA PIC  X(0001).
           05  FOLLOWI PIC  X(0008).
      *    -------------------------------
           05  RESENDL PIC S9(0004) COMP.
           05  RESENDF PIC  X(0001).
           05  FILLER REDEFINES RESENDF.
               10  RESENDA PIC  X(0001).
           05  RESENDI PIC  X(0008).
      *    -------------------------------
           05  PRINTL PIC S9(0004) COMP.
           05  PRINTF PIC  X(0001).
           05  FILLER REDEFINES PRINTF.
               10  PRINTA PIC  X(0001).
           05  PRINTI PIC  X(0001).
      *    -------------------------------
           05  COPIESL PIC S9(0004) COMP.
           05  COPIESF PIC  X(0001).
           05  FILLER REDEFINES COPIESF.
               10  COPIESA PIC  X(0001).
           05  COPIESI PIC  X(0001).
      *    -------------------------------
           05  ENCL PIC S9(0004) COMP.
           05  ENCF PIC  X(0001).
           05  FILLER REDEFINES ENCF.
               10  ENCA PIC  X(0001).
           05  ENCI PIC  X(0003).
      *    -------------------------------
           05  ADDRL PIC S9(0004) COMP.
           05  ADDRF PIC  X(0001).
           05  FILLER REDEFINES ADDRF.
               10  ADDRA PIC  X(0001).
           05  ADDRI PIC  X(0002).
      *    -------------------------------
           05  ACTL PIC S9(0004) COMP.
           05  ACTF PIC  X(0001).
           05  FILLER REDEFINES ACTF.
               10  ACTA PIC  X(0001).
           05  ACTI PIC  X(0001).
      *    -------------------------------
           05  BENL PIC S9(0004) COMP.
           05  BENF PIC  X(0001).
           05  FILLER REDEFINES BENF.
               10  BENA PIC  X(0001).
           05  BENI PIC  X(0001).
      *    -------------------------------
           05  EMPL PIC S9(0004) COMP.
           05  EMPF PIC  X(0001).
           05  FILLER REDEFINES EMPF.
               10  EMPA PIC  X(0001).
           05  EMPI PIC  X(0001).
      *    -------------------------------
           05  INSL PIC S9(0004) COMP.
           05  INSF PIC  X(0001).
           05  FILLER REDEFINES INSF.
               10  INSA PIC  X(0001).
           05  INSI PIC  X(0001).
      *    -------------------------------
           05  PHYSL PIC S9(0004) COMP.
           05  PHYSF PIC  X(0001).
           05  FILLER REDEFINES PHYSF.
               10  PHYSA PIC  X(0001).
           05  PHYSI PIC  X(0001).
      *    -------------------------------
           05  OTHR1L PIC S9(0004) COMP.
           05  OTHR1F PIC  X(0001).
           05  FILLER REDEFINES OTHR1F.
               10  OTHR1A PIC  X(0001).
           05  OTHR1I PIC  X(0001).
      *    -------------------------------
           05  OTHR2L PIC S9(0004) COMP.
           05  OTHR2F PIC  X(0001).
           05  FILLER REDEFINES OTHR2F.
               10  OTHR2A PIC  X(0001).
           05  OTHR2I PIC  X(0001).
      *    -------------------------------
           05  REL PIC S9(0004) COMP.
           05  REF PIC  X(0001).
           05  FILLER REDEFINES REF.
               10  REA PIC  X(0001).
           05  REI PIC  X(0070).
      *    -------------------------------
           05  L1L PIC S9(0004) COMP.
           05  L1F PIC  X(0001).
           05  FILLER REDEFINES L1F.
               10  L1A PIC  X(0001).
           05  L1I PIC  X(0003).
      *    -------------------------------
           05  TEXT1L PIC S9(0004) COMP.
           05  TEXT1F PIC  X(0001).
           05  FILLER REDEFINES TEXT1F.
               10  TEXT1A PIC  X(0001).
           05  TEXT1I PIC  X(0070).
      *    -------------------------------
           05  L2L PIC S9(0004) COMP.
           05  L2F PIC  X(0001).
           05  FILLER REDEFINES L2F.
               10  L2A PIC  X(0001).
           05  L2I PIC  X(0003).
      *    -------------------------------
           05  TEXT2L PIC S9(0004) COMP.
           05  TEXT2F PIC  X(0001).
           05  FILLER REDEFINES TEXT2F.
               10  TEXT2A PIC  X(0001).
           05  TEXT2I PIC  X(0070).
      *    -------------------------------
           05  L3L PIC S9(0004) COMP.
           05  L3F PIC  X(0001).
           05  FILLER REDEFINES L3F.
               10  L3A PIC  X(0001).
           05  L3I PIC  X(0003).
      *    -------------------------------
           05  TEXT3L PIC S9(0004) COMP.
           05  TEXT3F PIC  X(0001).
           05  FILLER REDEFINES TEXT3F.
               10  TEXT3A PIC  X(0001).
           05  TEXT3I PIC  X(0070).
      *    -------------------------------
           05  L4L PIC S9(0004) COMP.
           05  L4F PIC  X(0001).
           05  FILLER REDEFINES L4F.
               10  L4A PIC  X(0001).
           05  L4I PIC  X(0003).
      *    -------------------------------
           05  TEXT4L PIC S9(0004) COMP.
           05  TEXT4F PIC  X(0001).
           05  FILLER REDEFINES TEXT4F.
               10  TEXT4A PIC  X(0001).
           05  TEXT4I PIC  X(0070).
      *    -------------------------------
           05  L5L PIC S9(0004) COMP.
           05  L5F PIC  X(0001).
           05  FILLER REDEFINES L5F.
               10  L5A PIC  X(0001).
           05  L5I PIC  X(0003).
      *    -------------------------------
           05  TEXT5L PIC S9(0004) COMP.
           05  TEXT5F PIC  X(0001).
           05  FILLER REDEFINES TEXT5F.
               10  TEXT5A PIC  X(0001).
           05  TEXT5I PIC  X(0070).
      *    -------------------------------
           05  L6L PIC S9(0004) COMP.
           05  L6F PIC  X(0001).
           05  FILLER REDEFINES L6F.
               10  L6A PIC  X(0001).
           05  L6I PIC  X(0003).
      *    -------------------------------
           05  TEXT6L PIC S9(0004) COMP.
           05  TEXT6F PIC  X(0001).
           05  FILLER REDEFINES TEXT6F.
               10  TEXT6A PIC  X(0001).
           05  TEXT6I PIC  X(0070).
      *    -------------------------------
           05  L7L PIC S9(0004) COMP.
           05  L7F PIC  X(0001).
           05  FILLER REDEFINES L7F.
               10  L7A PIC  X(0001).
           05  L7I PIC  X(0003).
      *    -------------------------------
           05  TEXT7L PIC S9(0004) COMP.
           05  TEXT7F PIC  X(0001).
           05  FILLER REDEFINES TEXT7F.
               10  TEXT7A PIC  X(0001).
           05  TEXT7I PIC  X(0070).
      *    -------------------------------
           05  L8L PIC S9(0004) COMP.
           05  L8F PIC  X(0001).
           05  FILLER REDEFINES L8F.
               10  L8A PIC  X(0001).
           05  L8I PIC  X(0003).
      *    -------------------------------
           05  TEXT8L PIC S9(0004) COMP.
           05  TEXT8F PIC  X(0001).
           05  FILLER REDEFINES TEXT8F.
               10  TEXT8A PIC  X(0001).
           05  TEXT8I PIC  X(0070).
      *    -------------------------------
           05  L9L PIC S9(0004) COMP.
           05  L9F PIC  X(0001).
           05  FILLER REDEFINES L9F.
               10  L9A PIC  X(0001).
           05  L9I PIC  X(0003).
      *    -------------------------------
           05  TEXT9L PIC S9(0004) COMP.
           05  TEXT9F PIC  X(0001).
           05  FILLER REDEFINES TEXT9F.
               10  TEXT9A PIC  X(0001).
           05  TEXT9I PIC  X(0070).
      *    -------------------------------
           05  L10L PIC S9(0004) COMP.
           05  L10F PIC  X(0001).
           05  FILLER REDEFINES L10F.
               10  L10A PIC  X(0001).
           05  L10I PIC  X(0003).
      *    -------------------------------
           05  TEXT10L PIC S9(0004) COMP.
           05  TEXT10F PIC  X(0001).
           05  FILLER REDEFINES TEXT10F.
               10  TEXT10A PIC  X(0001).
           05  TEXT10I PIC  X(0070).
      *    -------------------------------
           05  L11L PIC S9(0004) COMP.
           05  L11F PIC  X(0001).
           05  FILLER REDEFINES L11F.
               10  L11A PIC  X(0001).
           05  L11I PIC  X(0003).
      *    -------------------------------
           05  TEXT11L PIC S9(0004) COMP.
           05  TEXT11F PIC  X(0001).
           05  FILLER REDEFINES TEXT11F.
               10  TEXT11A PIC  X(0001).
           05  TEXT11I PIC  X(0070).
      *    -------------------------------
           05  L12L PIC S9(0004) COMP.
           05  L12F PIC  X(0001).
           05  FILLER REDEFINES L12F.
               10  L12A PIC  X(0001).
           05  L12I PIC  X(0003).
      *    -------------------------------
           05  TEXT12L PIC S9(0004) COMP.
           05  TEXT12F PIC  X(0001).
           05  FILLER REDEFINES TEXT12F.
               10  TEXT12A PIC  X(0001).
           05  TEXT12I PIC  X(0070).
      *    -------------------------------
           05  L13L PIC S9(0004) COMP.
           05  L13F PIC  X(0001).
           05  FILLER REDEFINES L13F.
               10  L13A PIC  X(0001).
           05  L13I PIC  X(0003).
      *    -------------------------------
           05  TEXT13L PIC S9(0004) COMP.
           05  TEXT13F PIC  X(0001).
           05  FILLER REDEFINES TEXT13F.
               10  TEXT13A PIC  X(0001).
           05  TEXT13I PIC  X(0070).
      *    -------------------------------
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0072).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PRINTERL PIC S9(0004) COMP.
           05  PRINTERF PIC  X(0001).
           05  FILLER REDEFINES PRINTERF.
               10  PRINTERA PIC  X(0001).
           05  PRINTERI PIC  X(0004).
      *    -------------------------------
           05  PFKEY9L PIC S9(0004) COMP.
           05  PFKEY9F PIC  X(0001).
           05  FILLER REDEFINES PFKEY9F.
               10  PFKEY9A PIC  X(0001).
           05  PFKEY9I PIC  X(0014).
       01  EL152AO REDEFINES EL152AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEAO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TIMEAO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLMNOO PIC  X(0007).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CRTNOO PIC  X(0011).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PROCO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPIDO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CLEANO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SYSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHNUMO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLOWO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESENDO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COPIESO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  EMPO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  INSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PHYSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OTHR1O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  OTHR2O PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  REO PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L1O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT1O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L2O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT2O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L3O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT3O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L4O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT4O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L5O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT5O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L6O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT6O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L7O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT7O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L8O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT8O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L9O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT9O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L10O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT10O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L11O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT11O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L12O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT12O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  L13O PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TEXT13O PIC  X(0070).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ERRMSGO PIC  X(0072).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTERO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PFKEY9O PIC  X(0014).
      *    -------------------------------
01143  01  MAP-REDEF REDEFINES EL152AI.
           12  FILLER              PIC X(251).
01144 *    12  FILLER              PIC X(189).
01145      12  EL152RI.
01146        14  TEXT-LINES OCCURS 13 TIMES INDEXED BY SC-INDX.
01147          16  SC-LINEL        PIC S9(4)  COMP.
01148          16  SC-LINEA        PIC X.
01149          16  SC-LINE         PIC XXX.
01150          16  SC-TEXTL        PIC S9(4)  COMP.
01151          16  SC-TEXTA        PIC X.
01152          16  SC-TEXT         PIC X(70).
01153      EJECT
01154  01  RECORD-TABLE            PIC X(21900) VALUE SPACES.
01155
01156  01  REC-TABLE REDEFINES RECORD-TABLE.
01157      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-INDX
01158                              PIC X(3650).
01159
01160  01  REC-ENTRIES REDEFINES RECORD-TABLE.
01161      12  REC-ENT OCCURS 300 TIMES INDEXED BY TB-INDX TB-INDX1.
01162          16  REC-TEXT        PIC X(70).
01163          16  REC-PC          PIC 99.
01164          16  FILLER          PIC X.
01165
01166  01  TS-WORK-AREA            PIC X(3650).
01167      EJECT
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
01169  01  DFHCOMMAREA                 PIC X(1024).
01170
       01  var  pic x(30).
01171      EJECT
01172 *                                COPY ELCMSTR.
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
01173
01174      EJECT
01175 *                                COPY ELCCNTL.
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
01176
01177      EJECT
01178 *                                COPY ELCARCH.
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
01179
01180      EJECT
01181 *                                COPY ELCARCT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELCARCT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEMPORARY LETTER ARCHIVE FILE             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 090  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ELARCT                        RKP=2,LEN=8     *
00013 *       ALTERNATE PATH1 = ELARCT2  (RECORD TYPE) RKP=10,LEN=8    *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
CIDMOD*                                                                *
CIDMOD*  NO  CID  MODS  IN  COPYBOOK  ELCARCT                          *
00017 ******************************************************************
00018  01  LETTER-ARCHIVE-TEMP.
00019      12  LT-RECORD-ID                PIC XX.
00020          88  VALID-LT-ID                VALUE 'LT'.
00021
00022      12  LT-CONTROL-PRIMARY.
00023          16  LT-COMPANY-CD           PIC X.
00024          16  LT-ARCHIVE-NO           PIC S9(8)     COMP.
00025          16  LT-RECORD-TYPE          PIC X.
00026              88  LT-HEADER-DATA         VALUE '1'.
00027              88  LT-ADDRESS-DATA        VALUE '2'.
00028              88  LT-TEXT-DATA           VALUE '3'.
00029              88  LT-FORM-CONTROL-HDR    VALUE '4'.
00030          16  LT-LINE-SEQ-NO          PIC S9(4)     COMP.
00031
00032      12  LT-CONTROL-BY-TYPE.
00033          16  LT-COMPANY-CD-A1        PIC X.
00034          16  LT-RECORD-TYPE-A1       PIC X.
00035          16  LT-ARCHIVE-NO-A1        PIC S9(8)     COMP.
00036          16  LT-LINE-SEQ-NO-A1       PIC S9(4)     COMP.
00037
00038      12  LT-TEXT-RECORD.
00039          16  LT-SKIP-CONTROL         PIC XX.
00040          16  LT-TEXT-LINE            PIC X(70).
00041
00042      12  LT-ADDRESS-RECORD  REDEFINES  LT-TEXT-RECORD.
00043          16  FILLER                  PIC XX.
00044          16  LT-ADDRESS-LINE         PIC X(30).
00045          16  FILLER                  PIC X(40).
00046
00047      12  LT-HEADER-RECORD  REDEFINES  LT-TEXT-RECORD.
00048          16  FILLER                  PIC XX.
00049          16  LT-CARRIER              PIC X.
00050          16  LT-CLAIM-NO             PIC X(7).
00051          16  LT-CERT-NO.
00052              20  LT-CERT-PRIME       PIC X(10).
00053              20  LT-CERT-SFX         PIC X.
00054          16  LT-NO-OF-COPIES         PIC S9.
00055          16  LT-RESEND-DATE          PIC XX.
00056          16  LT-PROCESSOR-CD         PIC X(4).
00057          16  LT-CREATION-DT          PIC XX.
00058          16  LT-INITIAL-PRINT-DATE   PIC XX.
00059          16  LT-RESEND-PRINT-DATE    PIC XX.
00060          16  LT-CORR-TRLR-SEQ        PIC S9(4)    COMP.
00061          16  LT-1ST-RESEND-PRINT-DT  PIC XX.
00062          16  LT-DMD-ADDITIONAL-FIELDS.
00063              20  LT-DMD-LETTER-FORM      PIC X(4).
00064              20  LT-DMD-PROD-CODE        PIC XX.
00065              20  LT-DMD-RES-ST           PIC XX.
00066              20  LT-DMD-CORR-TRLR-SEQ    PIC S9(4)    COMP.
00067              20  LT-DMD-LETTER-STATUS    PIC X.
00068                  88  LT-DMD-LETTER-ONLINE   VALUE '1'.
00069                  88  LT-DMD-LETTER-PURGED   VALUE '2'.
00070                  88  LT-DMD-LETTER-RELOADED VALUE '3'.
00071              20  LT-DMD-LETTER-PURGE-DT  PIC XX.
00072              20  LT-DMD-LETTER-RELOAD-DT PIC XX.
00073              20  LT-DMD-UND-CODE         PIC XX.
00074              20  LT-DMD-BEN-CODE         PIC XX.
00075          16  FILLER                  PIC X(15).
00076
00077      12  LT-FORM-CONTROL-HEADER REDEFINES  LT-TEXT-RECORD.
00078          16  FILLER                  PIC XX.
00079          16  LT4-CARRIER             PIC X.
00080          16  LT4-CLAIM-NO            PIC X(7).
00081          16  LT4-CERT-NO.
00082              20  LT4-CERT-PRIME      PIC X(10).
00083              20  LT4-CERT-SFX        PIC X.
00084          16  LT4-NO-OF-COPIES        PIC S9.
00085          16  LT4-RESEND-DATE         PIC XX.
00086          16  LT4-PROCESSOR-CD        PIC X(4).
00087          16  LT4-CREATION-DT         PIC XX.
00088          16  LT4-INITIAL-PRINT-DATE  PIC XX.
00089          16  LT4-RESEND-PRINT-DATE   PIC XX.
00090          16  LT4-FORM-TRLR-SEQ       PIC S9(4)    COMP.
00091          16  LT4-FORM-TYPE           PIC X.
00092              88  LT4-INITIAL-FORM    VALUE '1'.
00093              88  LT4-PROGRESS-FORM   VALUE '2'.
00094          16  LT4-FORM-REM-PRINT-DT   PIC XX.
00095          16  LT4-STATE               PIC XX.
00096          16  FILLER                  PIC X(31).
00097 ******************************************************************
01182
01183      EJECT
01184 *                                COPY ELCCERT.
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
01185
01186      EJECT
01187 *                                COPY ERCACCT.
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
01188
01189      EJECT
01190 *                                COPY ELCTEXT.
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
01191
01192      EJECT
01193 *                                COPY ELCTRLR.
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
01194
01195      EJECT
01196 *                                COPY ELCBENE.
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
01197
01198      EJECT
01199 *                                COPY ERCCOMP.
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
01200
01201      EJECT
121802*                                COPY MPCPROD.
121802*    EJECT
121802*                                COPY MPCPLCY.
121802*    EJECT
121802*                                COPY MPCPLAN.
01207      EJECT
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
033110     EJECT
041513*                                COPY ELCENCC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCENCC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM ENCLOSURE CODE TABLE                            *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR THE ONLINE PROCESS OF CREATING     *
      *   A NAPERSOFT DOCUMENT                                         *
      *                                                                *
      *   FILE DESCRIPTION = ENCLOSURE CODE TABLE                      *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 400   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELENCC                    RKP=2,LEN=16   *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      *                                                                *
      *                                                                *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 082010    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 061217    2017060900001  TANA  INCREASE ATTACHMENTS FIELD SIZE
      ******************************************************************
       01  ENCLOSURE-CODES.
           12  NC-RECORD-ID                      PIC XX.
               88  VALID-NC-ID                      VALUE 'NC'.
           12  NC-CONTROL-PRIMARY.
               16  NC-COMPANY-CD                 PIC X.
               16  NC-REC-TYPE                   PIC X.
                   88  NC-CLAIMS                   VALUE '1'.
                   88  NC-ADMIN                    VALUE '2'.
               16  NC-ENC-CODE                   PIC X(5).
               16  FILLER                        PIC X(09).
           12  NC-MAINT-INFORMATION.
               16  NC-LAST-MAINT-DT              PIC XX.
               16  NC-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  NC-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  NC-OUTPUT-STACK                   PIC XXX.
           12  NC-ENCLOSURE-LINE                 PIC X(100).
           12  NC-ATTACHMENTS                    PIC X(255).
           12  NC-FUTURE                         PIC X(12).
      ******************************************************************
041513     EJECT
01208 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF
01209 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE VARIABLE-WORK-AREA
01210
010407*01  SYSTEM-VARIABLES            PIC X(3851).
010407*01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.
010407*    12  SYS-VAR-CODE            PIC X(4).
010407*    12  SYS-VAR-LEN             PIC 99.
010407*    12  SYS-VAR-DATA            PIC X(100).
01216
01217      EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR CLAIM-MASTER
                                CONTROL-FILE LETTER-ARCHIVE
                                LETTER-ARCHIVE-TEMP
                                CERTIFICATE-MASTER ACCOUNT-MASTER
                                TEXT-FILES ACTIVITY-TRAILERS
                                BENEFICIARY-MASTER
                                COMPENSATION-MASTER NAPERSOFT-FILE
                                ENCLOSURE-CODES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL152' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
01219
01220      MOVE EIBDATE               TO DC-JULIAN-YYDDD.
01221      MOVE '5'                   TO DC-OPTION-CODE.
01222      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
01223      MOVE DC-GREG-DATE-1-EDIT   TO SAVE-DATE.
01224      MOVE DC-BIN-DATE-1         TO SAVE-BIN-DATE.
01225
01226      MOVE DFHCOMMAREA           TO PROGRAM-INTERFACE-BLOCK.
01227      MOVE 1                     TO EMI-NUMBER-OF-LINES.
01228      MOVE ERROR-MESSAGE-INTERFACE-BLOCK
01229                                 TO EMI-SAVE-AREA.
01230
01231      MOVE EIBTRMID              TO TS-TERM-TEXT
01232                                    TS-TERM-SCREEN
01233                                    QID-TERM.
01234
01235      MOVE SAVE-BIN-DATE       TO CURRENT-SAVE.
01236
121802*    IF PI-COMPANY-ID = 'DMD'
121802*       MOVE CURRENT-SAVE           TO DC-BIN-DATE-1
121802*       MOVE '6'                    TO DC-OPTION-CODE
121802*       MOVE +3                     TO DC-ELAPSED-MONTHS
121802*       MOVE +0                     TO DC-ELAPSED-DAYS
121802*       PERFORM 9700-DATE-LINK THRU 9700-EXIT
121802*       MOVE DC-BIN-DATE-2          TO CURRENT-PLUS3-SAVE.
01244
01245      
      * EXEC CICS HANDLE CONDITION
01246 *        PGMIDERR (9600-PGMID-ERROR)
01247 *        MAPFAIL  (0325-MAPFAIL)
01248 *        ERROR    (9990-ABEND)
01249 *    END-EXEC.
      *    MOVE '"$L?.                 ! " #00006911' TO DFHEIV0
           MOVE X'22244C3F2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303036393131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01250
01251      IF PI-PROCESSOR-ID NOT = 'LGXX'
01252          
      * EXEC CICS READQ TS
01253 *            QUEUE   (PI-SECURITY-TEMP-STORE-ID)
01254 *            INTO    (SECURITY-CONTROL)
01255 *            LENGTH  (SC-COMM-LENGTH)
01256 *            ITEM    (SC-ITEM)
01257 *        END-EXEC
      *    MOVE '*$II   L              ''   #00006918' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303036393138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 SC-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01258          MOVE SC-CLAIMS-DISPLAY (7)  TO  PI-DISPLAY-CAP
01259          MOVE SC-CLAIMS-UPDATE  (7)  TO  PI-MODIFY-CAP.
01260
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if
01261      IF EIBCALEN = 0
01262          GO TO 8800-UNAUTHORIZED-ACCESS.
01263
01264      IF PI-CALLING-PROGRAM NOT = THIS-PGM
01265          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM
01266              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6
01267              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5
01268              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4
01269              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3
01270              MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2
01271              MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1
01272              MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM
01273              MOVE THIS-PGM             TO PI-CALLING-PROGRAM
01274              PERFORM 7750-DELETE-TEMP-STOR         THRU 7750-EXIT
01275              PERFORM 7760-DELETE-TEMP-STOR-SCREEN  THRU 7760-EXIT
01276              PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT
01277              MOVE SPACES              TO PI-WA
01278                                          PI-FORCE-7840
01279              MOVE ZEROS               TO PI-TOTAL-LINES
01280                                          PI-CURRENT-LINE
01281                                          PI-TEMP-STOR-ITEMS
01282                                          PI-UPDATE-SW
01283                                          PI-ADDR-SEQ
01284              MOVE '2'                 TO PI-ACTION
01285              MOVE LOW-VALUES          TO EL152AO
01286                                          PI-ADDR-TYPE
01287              GO TO 8100-SEND-INITIAL-MAP
01288          ELSE
01289              MOVE PI-CALLING-PROGRAM   TO PGM-NAME
01290              MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM
01291              MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM
01292              MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1
01293              MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2
01294              MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3
01295              MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4
01296              MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5
01297              MOVE SPACES               TO PI-SAVED-PROGRAM-6
01298              PERFORM 7500-READ-TS           THRU 7599-EXIT
01299              PERFORM 7600-READ-SCREEN-TS    THRU 7699-EXIT
01300              IF PGM-NAME = PGM-EL141
01301                 PERFORM 500-RECOVER-PI-TEMP-STORAGE THRU 599-EXIT
01302                 SET TB-INDX TO PI-CURRENT-LINE
01303                 PERFORM 400-SET-CODES       THRU 499-EXIT
01304                 PERFORM 7000-READ-ADDR      THRU 7099-EXIT
01305                 PERFORM 7170-FORMAT-SCREEN  THRU 7170-EXIT
01306                         VARYING SC-INDX FROM 1 BY 1 UNTIL
01307                         SC-INDX > NUM-LINES-PER-SCREEN
01308                 MOVE PI-ALT-PRINTER-ID  TO PRINTERO
01309                 MOVE AL-UANON           TO PRINTERA
01310                 GO TO 8100-SEND-INITIAL-MAP
01311              ELSE
01312                 SET TB-INDX TO PI-CURRENT-LINE
01313                 PERFORM 7170-FORMAT-SCREEN   THRU  7170-EXIT
01314                         VARYING SC-INDX FROM 1 BY 1 UNTIL
01315                         SC-INDX > NUM-LINES-PER-SCREEN
01316                 MOVE PI-ALT-PRINTER-ID  TO PRINTERO
01317                 MOVE AL-UANON           TO PRINTERA
01318                 GO TO 8100-SEND-INITIAL-MAP.
01319
01320 *0100-PA.
01321 *    MOVE ER-0008                TO EMI-ERROR.
01322 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01323 *    MOVE -1                     TO MAINTL.
01324 *    GO TO 8200-SEND-DATAONLY.
01325
01326      EJECT
01327  0200-RECEIVE.
01328      MOVE TRANS-ID         TO EIBTRNID.
01329      MOVE EMI-SAVE-AREA    TO ERROR-MESSAGE-INTERFACE-BLOCK.
01330      MOVE LOW-VALUES       TO EL152AI.
01331      MOVE '104A'           TO TS-ID-TEXT.
01332      MOVE EIBTRMID         TO TS-TERM-TEXT
01333                               TS-TERM-SCREEN
01334                               QID-TERM.
01335
01336      
      * EXEC CICS HANDLE AID
01337 *        CLEAR    (9400-CLEAR)
01338 *        PA1      (0100-PA)
01339 *        PA2      (0100-PA)
01340 *        PA3      (0100-PA)
01341 *    END-EXEC.
      *    MOVE '"&=!"#               V! # #00007016' TO DFHEIV0
           MOVE X'22263D212223202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2320233030303037303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01342
01343      
      * EXEC CICS HANDLE CONDITION
01344 *        MAPFAIL  (0325-MAPFAIL)
01345 *    END-EXEC.
      *    MOVE '"$?                   ! $ #00007023' TO DFHEIV0
           MOVE X'22243F202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303037303233' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01346
01347      
      * EXEC CICS SYNCPOINT
01348 *    END-EXEC.
      *    MOVE '6"                    !   #00007027' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01349
01350      IF LOWER-CASE-LETTERS-USED
01351         
      * EXEC CICS RECEIVE
01352 *            MAP      (MAP-NAME)
01353 *            MAPSET   (MAPSET-NAME)
01354 *            INTO     (EL152AI)
01355 *            ASIS
01356 *       END-EXEC
           MOVE LENGTH OF
            EL152AI
             TO DFHEIV11
      *    MOVE '8"TAI  L              ''   #00007031' TO DFHEIV0
           MOVE X'382254414920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01357       ELSE
01358         
      * EXEC CICS RECEIVE
01359 *            MAP      (MAP-NAME)
01360 *            MAPSET   (MAPSET-NAME)
01361 *            INTO     (EL152AI)
01362 *       END-EXEC.
           MOVE LENGTH OF
            EL152AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00007038' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037303338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AI, 
                 DFHEIV11, 
                 MAPSET-NAME, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01363
01364      IF NOT DISPLAY-CAP AND PI-PROCESSOR-ID NOT = 'LGXX'
01365          MOVE 'READ'             TO  SM-READ
01366          PERFORM 9995-SECURITY-VIOLATION
01367          MOVE ER-0070            TO  EMI-ERROR
01368          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01369          GO TO 8100-SEND-INITIAL-MAP.
01370
01371      INSPECT MAINTI CONVERTING LOWER-CASE TO UPPER-CASE.
           MOVE 'N'                    TO CLEANI
01372
01373      IF ENTERPFL = 0
01374          GO TO 0300-CHECK-PFKEYS.
01375      IF EIBAID NOT = DFHENTER
01376          MOVE ER-0004            TO EMI-ERROR
01377          GO TO 0320-INPUT-ERROR.
01378      IF (ENTERPFI NUMERIC) AND (ENTERPFI > 0 AND < 25)
01379          MOVE PF-VALUES (ENTERPFI) TO EIBAID
01380      ELSE
01381          MOVE ER-0029            TO EMI-ERROR
01382          GO TO 0320-INPUT-ERROR.
01383
01384  0300-CHECK-PFKEYS.
01385      IF EIBAID = DFHPF23
01386          GO TO 8810-PF23.
01387      IF EIBAID = DFHPF24
01388          GO TO 9200-RETURN-MAIN-MENU.
01389 *    IF EIBAID = DFHPF12
01390 *        GO TO 9500-PF12.
01391      IF EIBAID = DFHPF1
01392          MOVE 7                  TO ROLL-COUNTER
01393          GO TO 7900-ROLL-PAGE.
01394      IF EIBAID = DFHPF2
01395          MOVE -7                 TO ROLL-COUNTER
01396          GO TO 7900-ROLL-PAGE.
01397      IF EIBAID = DFHPF3
01398          GO TO 6100-ADDR-MAINT.
01399      IF EIBAID = DFHPF4
01400          IF MODIFY-CAP
01401              GO TO 6200-EDIT-MODE
01402          ELSE
01403              MOVE 'UPDATE'       TO  SM-READ
01404              PERFORM 9995-SECURITY-VIOLATION
01405              MOVE ER-0070        TO  EMI-ERROR
01406              GO TO 0320-INPUT-ERROR.
01407
01408      IF EIBAID = DFHPF5
01409          GO TO 0330-FUNCTION-CHECK.
01410
01411      IF EIBAID = DFHPF6
01412          GO TO 0330-FUNCTION-CHECK.
01413
01414      IF EIBAID = DFHPF7
01415         COMPUTE ROLL-COUNTER = ((PI-TOTAL-LINES - 1) * -1)
01416         GO TO 7900-ROLL-PAGE.
01417
01418      IF EIBAID = DFHPF8
01419         MOVE PI-TOTAL-LINES TO ROLL-COUNTER
01420         GO TO 7900-ROLL-PAGE.
01421
121802*    IF EIBAID = DFHPF9 AND
121802*       PI-COMPANY-ID = 'DMD'
121802*        MOVE '1'           TO PI-FORCE-7840
121802*        GO TO 0330-FUNCTION-CHECK.
01426
01427      IF EIBAID = DFHENTER
01428          GO TO 0330-FUNCTION-CHECK.
01429
030805     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
030805             VARYING SC-INDX FROM 1 BY 1
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
030805
01430      MOVE ER-0029                TO EMI-ERROR.
01431  0320-INPUT-ERROR.
01432      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01433      MOVE AL-UNBON               TO ENTERPFA.
01434      IF ENTERPFL = 0
01435          MOVE -1                 TO MAINTL
01436      ELSE
01437          MOVE -1                 TO ENTERPFL.
01438
01439      GO TO 8200-SEND-DATAONLY.
01440
081004 0100-PA.
081004     MOVE ER-0008                TO EMI-ERROR.
081004     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
081004     MOVE -1                     TO MAINTL.
081004     GO TO 8200-SEND-DATAONLY.
081004
01441  0325-MAPFAIL.
01442 ***********************************************************
01443 *      ROUTINE SHOULD ONLY BE PERFORMED WHEN PRINTING     *
01444 *      LETTERS ON A 3275 PRINTER.                         *
01445 ***********************************************************
01446
01447      PERFORM 7600-READ-SCREEN-TS    THRU 7699-EXIT.
01448
01449      SET TB-INDX         TO PI-CURRENT-LINE.
01450      PERFORM 7170-FORMAT-SCREEN   THRU  7170-EXIT
01451                   VARYING SC-INDX FROM 1 BY 1 UNTIL
01452                   SC-INDX > NUM-LINES-PER-SCREEN.
01453
01454      GO TO 8100-SEND-INITIAL-MAP.
01455
01456      EJECT
01457  0330-FUNCTION-CHECK.
01458      IF NOT MODIFY-CAP
01459          IF MAINTI = 'S'
01460              NEXT SENTENCE
01461          ELSE
01462              MOVE 'UPDATE'           TO  SM-READ
01463              PERFORM 9995-SECURITY-VIOLATION
01464              MOVE ER-0070            TO  EMI-ERROR
01465              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01466              GO TO 8100-SEND-INITIAL-MAP.
01467
01468      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
01469
01470      IF EIBAID = DFHPF5
011212      IF PI-PROMPT-LETTER = 'Y'
040110         MOVE ER-0894            TO EMI-ERROR
040110         MOVE -1             TO MAINTL
040110         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040110         MOVE AL-UABON       TO MAINTA
040110         GO TO 8200-SEND-DATAONLY
040110      ELSE
01471          IF MAINTI NOT = 'C'
01472              MOVE ER-0716        TO EMI-ERROR
01473              MOVE -1             TO MAINTL
01474              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01475              MOVE AL-UABON       TO MAINTA
01476              GO TO 8200-SEND-DATAONLY
01477          ELSE
01478              IF MODIFY-CAP
01479                  GO TO 6400-LETTER-RELEASE
01480              ELSE
01481                  IF PRINT-PERFORMED
01482                      GO TO 6400-LETTER-RELEASE
01483                  ELSE
01484                      MOVE AL-UNBON TO ENTERPFA
01485                      MOVE -1       TO ENTERPFL
01486                      MOVE ER-2398 TO EMI-ERROR
01487                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01488                      GO TO 8200-SEND-DATAONLY.
01489
01490      IF EIBAID = DFHPF6
011212      IF PI-PROMPT-LETTER = 'Y'
040110         MOVE ER-0894            TO EMI-ERROR
040110         MOVE -1             TO MAINTL
040110         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
040110         MOVE AL-UABON       TO MAINTA
040110         GO TO 8200-SEND-DATAONLY
040110      ELSE
01491          GO TO 7800-PRINT-LETTER-NOW.
01492
01493      IF MAINTI = 'S'
01494          GO TO 1000-SHOW.
01495
01496      IF MAINTI = 'C'
01497          GO TO 2000-CREATE.
01498
01499      IF MAINTI = 'R'
01500         IF NOT MODIFY-CAP
01501            MOVE ER-0070          TO EMI-ERROR
01502            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01503            MOVE LOW-VALUES       TO EL152AO
01504            GO TO 8100-SEND-INITIAL-MAP
01505           ELSE
01506            GO TO 3000-RECORD.
01507
PEMTST     PERFORM 7500-READ-TS        THRU 7599-EXIT
PEMTST     PERFORM 7950-SET-INDX       THRU 7950-EXIT
PEMTST     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN
                                       THRU 7960-EXIT
PEMTST        VARYING SC-INDX FROM 1 BY 1 UNTIL
PEMTST        SC-INDX > NUM-LINES-PER-SCREEN
01508      MOVE ER-0023                TO EMI-ERROR.
01509      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01510      MOVE -1                     TO MAINTL.
01511      MOVE AL-UABON               TO MAINTA.
01512      GO TO 8200-SEND-DATAONLY.
01513
01514      EJECT
01515  0350-EDIT-ROUTINE.
01516      PERFORM 400-SET-CODES THRU 499-EXIT.
01517      MOVE PI-CLAIM-NO    TO CLAM-CLAIM
01518                             ACTV-CLAIM.
01519
01520      IF ADDRL NOT = +0
01521         INSPECT ADDRI CONVERTING LOWER-CASE TO UPPER-CASE
01522         MOVE ADDRI           TO WS-ADDR-TYPE-CD
01523         IF WS-ADDR-TYPE = 'I' OR 'B' OR 'A' OR 'P' OR
01524                           'O' OR 'Q' OR 'E'
01525            MOVE AL-UANON         TO ADDRA
01526         ELSE
01527            MOVE -1               TO ADDRL
01528            MOVE AL-UABON         TO ADDRA
01529            MOVE ER-0176          TO EMI-ERROR
01530            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
121802*     ELSE
121802*       IF PI-COMPANY-ID = 'DMD'
121802*        IF MAINTI = 'C' OR 'R'
121802*          MOVE -1               TO ADDRL
121802*          MOVE AL-UABON         TO ADDRA
121802*          MOVE ER-0861          TO EMI-ERROR
121802*          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01538
01539      IF ADDRL NOT = +0
01540         IF WS-ADDR-SEQ NOT NUMERIC
01541            MOVE -1               TO ADDRL
01542            MOVE AL-UABON         TO ADDRA
01543            MOVE ER-0176          TO EMI-ERROR
01544            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01545
121802*    IF ADDRL NOT = +0
121802*       IF PI-COMPANY-ID = 'DMD'
121802*          IF WS-ADDR-TYPE = 'B' AND
121802*             WS-ADDR-SEQ NOT = '9'
121802*                MOVE -1               TO ADDRL
121802*                MOVE AL-UABON         TO ADDRA
121802*                MOVE ER-7842          TO EMI-ERROR
121802*                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01554
01555      IF FORML NOT = ZEROS
01556         INSPECT FORMI CONVERTING LOWER-CASE TO UPPER-CASE.
01557
01558      IF MAINTI = 'C' AND FORML = ZEROS
01559         MOVE -1                  TO FORML
01560         MOVE ER-0177             TO EMI-ERROR
01561         MOVE AL-UABON            TO FORMA
01562         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01563
01564      IF FOLLOWL NOT = ZEROS
01565         MOVE FOLLOWI             TO DEEDIT-FIELD
01566         PERFORM 8600-DEEDIT
01567         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
01568         MOVE '4'                 TO DC-OPTION-CODE
01569         PERFORM 9700-DATE-LINK  THRU  9700-EXIT
01570         IF DATE-CONVERSION-ERROR
01571            MOVE ER-0182          TO EMI-ERROR
01572            MOVE -1               TO FOLLOWL
01573            MOVE AL-UABON         TO FOLLOWA
01574            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01575         ELSE
121802*          IF (PI-COMPANY-ID = 'DMD') AND
121802*             (NOT FORCE-7840)        AND
121802*             (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)
121802*              MOVE 'PF9=FORCE 7840' TO PFKEY9O
121802*              MOVE AL-SABON         TO PFKEY9A
121802*              MOVE ER-7840          TO EMI-ERROR
121802*              MOVE -1               TO FOLLOWL
121802*              MOVE AL-UABON         TO FOLLOWA
121802*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
121802*          ELSE
01586              MOVE SPACES                TO PFKEY9O
01587              MOVE AL-SADON              TO PFKEY9A
01588              MOVE DC-GREG-DATE-1-EDIT   TO FOLLOWO
01589              MOVE AL-UANON              TO FOLLOWA
01590              MOVE DC-BIN-DATE-1         TO FOLLOW-UP-SAVE
01591      ELSE
01592         MOVE LOW-VALUES                 TO FOLLOW-UP-SAVE.
01593
01594      IF FOLLOW-UP-SAVE NOT = LOW-VALUES
01595         IF FOLLOW-UP-SAVE NOT > CURRENT-SAVE
01596            MOVE ER-0533          TO EMI-ERROR
01597            MOVE AL-UABON         TO FOLLOWA
01598            MOVE -1               TO FOLLOWL
01599            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01600
01601      IF RESENDL NOT = ZEROS
01602         MOVE RESENDI             TO DEEDIT-FIELD
01603         PERFORM 8600-DEEDIT
01604         MOVE DEEDIT-FIELD-V0     TO DC-GREG-DATE-1-MDY
01605         MOVE '4'                 TO DC-OPTION-CODE
01606         PERFORM 9700-DATE-LINK  THRU  9700-EXIT
01607         IF DATE-CONVERSION-ERROR
01608            MOVE ER-0185          TO EMI-ERROR
01609            MOVE -1               TO RESENDL
01610            MOVE AL-UABON         TO RESENDA
01611            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01612          ELSE
121802*          IF (PI-COMPANY-ID = 'DMD') AND
121802*             (NOT FORCE-7840)        AND
121802*             (DC-BIN-DATE-1 > CURRENT-PLUS3-SAVE)
121802*              MOVE 'PF9=FORCE 7840' TO PFKEY9O
121802*              MOVE AL-SABON         TO PFKEY9A
121802*              MOVE ER-7840          TO EMI-ERROR
121802*              MOVE -1               TO RESENDL
121802*              MOVE AL-UABON         TO RESENDA
121802*              PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
121802*          ELSE
01623              MOVE SPACES                 TO PFKEY9O
01624              MOVE AL-SADON               TO PFKEY9A
01625              MOVE DC-GREG-DATE-1-EDIT    TO RESENDO
01626              MOVE AL-UANON               TO RESENDA
01627              MOVE DC-BIN-DATE-1          TO RESEND-SAVE
01628      ELSE
01629         MOVE LOW-VALUE                   TO RESEND-SAVE.
01630
01631      IF RESEND-SAVE NOT = LOW-VALUES
01632         IF RESEND-SAVE NOT > CURRENT-SAVE
01633            MOVE ER-0537          TO EMI-ERROR
01634            MOVE AL-UABON         TO RESENDA
01635            MOVE -1               TO RESENDL
01636            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01637
01638      IF PRINTL NOT = ZEROS
01639         INSPECT PRINTI CONVERTING LOWER-CASE TO UPPER-CASE
01640         IF PRINTI = 'Y' OR = ' '
01641            MOVE AL-UANON         TO PRINTA
01642         ELSE
01643            MOVE ER-0183          TO EMI-ERROR
01644            MOVE -1               TO PRINTL
01645            MOVE AL-UABON         TO PRINTA
01646            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
01647
           MOVE 'Y'                    TO CLEANO
           IF ENCL <= ZEROS
              MOVE 'N'                 TO CLEANO
           END-IF
           IF ENCL > ZEROS
              MOVE FUNCTION UPPER-CASE(ENCI) TO ENCI
                                                PI-ENCLOSURE-CD
041513        MOVE SPACES             TO ELENCC-KEY
041513        MOVE PI-COMPANY-CD      TO ELENCC-COMPANY-CD
041513        MOVE '1'                TO ELENCC-REC-TYPE
041513        MOVE ENCI               TO ELENCC-ENC-CODE
041513
041513        
      * EXEC CICS READ
041513*           DATASET    (ENCC-ID)
041513*           SET        (ADDRESS OF ENCLOSURE-CODES)
041513*           RIDFLD     (ELENCC-KEY)
041513*           RESP       (WS-RESPONSE)
041513*       END-EXEC
      *    MOVE '&"S        E          (  N#00007374' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303037333734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ENCC-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
041513
041513        IF RESP-NORMAL
                 MOVE ENCI             TO PI-ENCLOSURE-CD
                                          W-ENCLOSURE-CD
                 MOVE AL-UANON         TO ENCA
              ELSE
                 MOVE 'N'              TO CLEANO
                 MOVE ER-1560          TO EMI-ERROR
                 MOVE -1               TO ENCL
                 MOVE AL-UABON         TO ENCA
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
           END-IF
01648      IF COPIESL NOT = ZEROS
01649         IF COPIESI NOT NUMERIC OR
01650            COPIESI = '0'
01651            MOVE ER-0184          TO EMI-ERROR
01652            MOVE -1               TO COPIESL
01653            MOVE AL-UABON         TO COPIESA
01654            PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
01655         ELSE
01656            MOVE AL-UANON         TO COPIESA.
01657
01658      IF PI-PROCESSOR-PRINTER NOT = SPACES
01659          MOVE PI-PROCESSOR-PRINTER   TO  PI-ALT-PRINTER-ID.
01660
01661      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
01662
01663      IF PRINTERL > ZERO
01664         INSPECT PRINTERI CONVERTING LOWER-CASE TO UPPER-CASE
01665         MOVE AL-UANON            TO PRINTERA
01666         MOVE PRINTERI            TO PI-ALT-PRINTER-ID
01667                                     PI-ALT-DMD-PRT-ID
01668      ELSE
01669         IF (PI-NO-CARRIER-SECURITY AND
01670             PI-NO-ACCOUNT-SECURITY) OR
01671             PI-PROCESSOR-PRINTER NOT = SPACES
01672              NEXT SENTENCE
01673         ELSE
01674             MOVE AL-UABON       TO PRINTERA
01675             MOVE -1             TO PRINTERL
01676             MOVE ER-2397        TO EMI-ERROR
01677             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01678
01679      IF ACTL NOT = +0
01680          MOVE ZEROS              TO W-ACCOUNT
01681          IF ACTI > SPACES
01682              IF ACTI NUMERIC
01683                      AND
01684                 ACTI NOT < 1
01685                      AND
01686                 ACTI NOT > 9
01687                  MOVE AL-UANON   TO ACTA
01688                  MOVE ACTI       TO W-ACCOUNT
01689                                     W-LETTER-ADDR-SEQ
01690                  MOVE 'A'        TO W-LETTER-ADDR-TYPE
01691              ELSE
01692                  MOVE -1         TO ACTL
01693                  MOVE AL-UABON   TO ACTA
01694                  MOVE ER-3547    TO EMI-ERROR
01695                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01696
01697      IF BENL NOT = +0
01698          MOVE ZEROS              TO W-BENEFICIARY
01699          IF BENI > SPACES
01700              IF BENI NUMERIC
01701                      AND
01702                 BENI NOT < 1
01703                      AND
01704                 BENI NOT > 9
01705                  MOVE AL-UANON   TO BENA
01706                  MOVE BENI       TO W-BENEFICIARY
01707                                     W-LETTER-ADDR-SEQ
01708                  MOVE 'B'        TO W-LETTER-ADDR-TYPE
01709              ELSE
01710                  MOVE -1         TO BENL
01711                  MOVE AL-UABON   TO BENA
01712                  MOVE ER-3547    TO EMI-ERROR
01713                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01714
01715      IF EMPL NOT = +0
01716          MOVE ZEROS              TO W-EMPLOYER
01717          IF EMPI > SPACES
01718              IF EMPI NUMERIC
01719                      AND
01720                 EMPI NOT < 1
01721                      AND
01722                 EMPI NOT > 9
01723                  MOVE AL-UANON   TO EMPA
01724                  MOVE EMPI       TO W-EMPLOYER
01725                                     W-LETTER-ADDR-SEQ
01726                  MOVE 'E'        TO W-LETTER-ADDR-TYPE
01727              ELSE
01728                  MOVE -1         TO EMPL
01729                  MOVE AL-UABON   TO EMPA
01730                  MOVE ER-3547    TO EMI-ERROR
01731                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01732
01733      IF INSL NOT = +0
01734          MOVE ZEROS              TO W-INSURED
01735          IF INSI > SPACES
01736              IF INSI NUMERIC
01737                      AND
01738                 INSI NOT < 1
01739                      AND
01740                 INSI NOT > 9
01741                  MOVE AL-UANON   TO INSA
01742                  MOVE INSI       TO W-INSURED
01743                                     W-LETTER-ADDR-SEQ
01744                  MOVE 'I'        TO W-LETTER-ADDR-TYPE
01745              ELSE
01746                  MOVE -1         TO INSL
01747                  MOVE AL-UABON   TO INSA
01748                  MOVE ER-3547    TO EMI-ERROR
01749                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01750
01751      IF PHYSL NOT = +0
01752          MOVE ZEROS              TO W-PHYSICIAN
01753          IF PHYSI > SPACES
01754              IF PHYSI NUMERIC
01755                      AND
01756                 PHYSI NOT < 1
01757                      AND
01758                 PHYSI NOT > 9
01759                  MOVE AL-UANON   TO PHYSA
01760                  MOVE PHYSI      TO W-PHYSICIAN
01761                                     W-LETTER-ADDR-SEQ
01762                  MOVE 'P'        TO W-LETTER-ADDR-TYPE
01763              ELSE
01764                  MOVE -1         TO PHYSL
01765                  MOVE AL-UABON   TO PHYSA
01766                  MOVE ER-3547    TO EMI-ERROR
01767                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01768
01769      IF OTHR1L NOT = +0
01770          MOVE ZEROS              TO W-OTHER-1
01771          IF OTHR1I > SPACES
01772              IF OTHR1I NUMERIC
01773                      AND
01774                 OTHR1I NOT < 1
01775                      AND
01776                 OTHR1I NOT > 9
01777                  MOVE AL-UANON   TO OTHR1A
01778                  MOVE OTHR1I     TO W-OTHER-1
01779                                     W-LETTER-ADDR-SEQ
01780                  MOVE 'O'        TO W-LETTER-ADDR-TYPE
01781              ELSE
01782                  MOVE -1         TO OTHR1L
01783                  MOVE AL-UABON   TO OTHR1A
01784                  MOVE ER-3547    TO EMI-ERROR
01785                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01786
01787      IF OTHR2L NOT = +0
01788          MOVE ZEROS              TO W-OTHER-2
01789          IF OTHR2I > SPACES
01790              IF OTHR2I NUMERIC
01791                      AND
01792                 OTHR2I NOT < 1
01793                      AND
01794                 OTHR2I NOT > 9
01795                  MOVE AL-UANON   TO OTHR2A
01796                  MOVE OTHR2I     TO W-OTHER-2
01797                                     W-LETTER-ADDR-SEQ
01798                  MOVE 'Q'        TO W-LETTER-ADDR-TYPE
01799              ELSE
01800                  MOVE -1         TO OTHR2L
01801                  MOVE AL-UABON   TO OTHR2A
01802                  MOVE ER-3547    TO EMI-ERROR
01803                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
01804
01805      IF NOT EMI-NO-ERRORS
01806         GO TO 8200-SEND-DATAONLY.
01807
01808  0350-EXIT.
01809       EXIT.
01810      EJECT
01811  400-SET-CODES.
01812      MOVE PI-COMPANY-ID          TO CNTL-CO.
01813      MOVE PI-COMPANY-CD          TO CLAM-CO
01814                                     TEXT-CO
01815                                     ACTV-CO
01816                                     CERT-CO
01817                                     ACCT-CO
01818                                     ARCH-CO
01819                                     PROD-CO
01820                                     PLCY-CO
01821                                     PLAN-CO.
01822
01823      MOVE PI-CARRIER             TO CLAM-CARRIER
01824                                     ACTV-CARRIER
01825                                     CERT-CARRIER
01826                                     ACCT-CARRIER
01827                                     PROD-CARRIER
01828                                     PLCY-CARRIER
01829                                     PLAN-CARRIER.
01830
01831      MOVE PI-CERT-NO             TO CLAM-CERT-NUM
01832                                     ACTV-CERT-NUM
01833                                     CERT-CERT-NUM.
01834  499-EXIT.
01835       EXIT.
01836
01837  500-RECOVER-PI-TEMP-STORAGE.
01838
01839      
      * EXEC CICS HANDLE CONDITION
01840 *        QIDERR   (590-QIDERR)
01841 *    END-EXEC.
      *    MOVE '"$N                   ! % #00007585' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303037353835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01842
01843      
      * EXEC CICS READQ TS
01844 *        QUEUE    (WS-PI-QID)
01845 *        INTO     (PROGRAM-INTERFACE-BLOCK)
01846 *        LENGTH   (PI-COMM-LENGTH)
01847 *    END-EXEC.
      *    MOVE '*$I    L              ''   #00007589' TO DFHEIV0
           MOVE X'2A2449202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303037353839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PI-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01848
01849      PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT.
01850
01851      GO TO 599-EXIT.
01852
01853  590-QIDERR.
01854
01855      IF EIBTRNID = TRANS-ID
01856          MOVE ER-0033                   TO  EMI-ERROR
01857          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01858          GO TO 8200-SEND-DATAONLY.
01859
01860  599-EXIT.
01861      EXIT.
01862
01863      EJECT
01864  1000-SHOW.
01865 ***************************************************************
01866 *     THIS ROUTINE WILL BROWSE THE ARCHIVE FILE WITH THE      *
01867 *     ARCHIVE NUMBER SPECIFIED FROM THE SCREEN. THE TEXT      *
01868 *     WILL BE INSERTED INTO THE TS-TABLE AND DISPLAYED TO     *
01869 *     OPERATOR.                                               *
01870 ***************************************************************
01871
01872      MOVE SPACES                 TO RECORD-TABLE.
01873      IF ARCHNUML = ZEROS
01874         MOVE -1                  TO ARCHNUML
01875         MOVE AL-UNBON            TO ARCHNUMA
01876         MOVE ER-0174             TO EMI-ERROR
01877         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01878         GO TO 8200-SEND-DATAONLY.
01879
01880      IF ARCHNUMI NOT NUMERIC
01881         MOVE -1                  TO ARCHNUML
01882         MOVE AL-UNBON            TO ARCHNUMA
01883         MOVE ER-0175             TO EMI-ERROR
01884         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
01885         GO TO 8200-SEND-DATAONLY.
01886
01887      IF PI-SHOW-MODE
042605*       IF PI-ARCHIVE-NUMBER = ARCHNUMI
042605*          MOVE -1               TO MAINTL
042605*          GO TO 8200-SEND-DATAONLY
042605*       ELSE
01892            PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
01893
01894      MOVE ARCHNUMI               TO ARCH-NUMBER
01895                                     PI-ARCHIVE-NUMBER.
01896      MOVE ' '                    TO ARCH-REC-TYPE.
01897      MOVE '1'                    TO PI-ACTION.
01898      MOVE ZEROS                  TO PI-ADDR-SEQ.
01899      MOVE LOW-VALUES             TO PI-ADDR-TYPE.
01900      MOVE SPACES                 TO PI-PRINT-SW
01901                                     PI-FORM-NUMBER.
01902      SET TB-INDX TO 1.
CIDMOD
CIDMOD     MOVE SPACE                  TO WS-LETTER-STATUS.
CIDMOD
01903      
      * EXEC CICS HANDLE CONDITION
01904 *         NOTOPEN    (8850-ARCH-NOT-OPEN)
01905 *         NOTFND     (1020-ENDBR)
01906 *         ENDFILE    (1020-ENDBR)
01907 *    END-EXEC.
      *    MOVE '"$JI''                 ! & #00007652' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303037363532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01908
01909      MOVE ARCH-PARTIAL-KEY       TO ARCH-SAVE-KEY.
01910
01911      
      * EXEC CICS STARTBR
01912 *         DATASET    (ARCH-ID)
01913 *         RIDFLD     (ARCH-KEY)
01914 *         GTEQ
01915 *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007660' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01916
01917      MOVE 'Y'                    TO ARCH-BROWSE-STARTED.
01918
01919  1010-READ-NEXT.
01920      
      * EXEC CICS READNEXT
01921 *         SET       (ADDRESS OF LETTER-ARCHIVE)
01922 *         DATASET   (ARCH-ID)
01923 *         RIDFLD    (ARCH-KEY)
01924 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007669' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037363639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01925
01926      IF LA-FORM-CONTROL-HDR
01927         GO TO 1020-ENDBR.
01928
CIDMOD
CIDMOD     IF NOT LA-HEADER-DATA
CIDMOD         NEXT SENTENCE
CIDMOD     ELSE
CIDMOD         IF NOT LA-CSO-LETTER-PURGED
CIDMOD             NEXT SENTENCE
CIDMOD         ELSE
CIDMOD             MOVE 'P'            TO WS-LETTER-STATUS
CIDMOD             GO TO 1020-ENDBR.
CIDMOD
01929      IF ARCH-PARTIAL-KEY = ARCH-SAVE-KEY
01930         IF LA-HEADER-DATA
01931            PERFORM 1040-FORMAT-RESEND
01932            MOVE LA-NO-OF-COPIES    TO COPIESO
01933            PERFORM 1050-GET-CORRESPOND THRU 1059-EXIT
01934            GO TO 1010-READ-NEXT
01935         ELSE
01936            IF LA-ADDRESS-DATA
01937               MOVE LA-ADDRESS-LINE   TO REC-TEXT (TB-INDX)
01938               MOVE ZEROS             TO REC-PC (TB-INDX)
01939               SET TB-INDX UP BY 1
01940               GO TO 1010-READ-NEXT
01941            ELSE
01942               PERFORM 1030-TEXT-BUILD
01943               GO TO 1010-READ-NEXT.
01944
01945  1020-ENDBR.
01946      IF ARCH-BROWSE-STARTED = 'Y'
01947         MOVE 'N'                TO ARCH-BROWSE-STARTED
01948         
      * EXEC CICS ENDBR
01949 *            DATASET   (ARCH-ID)
01950 *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007707' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
CIDMOD     END-IF.
CIDMOD
121802*    IF PI-COMPANY-ID = 'CID'
CIDMOD         IF TB-INDX = 1
CIDMOD             IF WS-LETTER-STATUS EQUAL 'P'
CIDMOD                 MOVE 9021           TO EMI-ERROR
CIDMOD                 MOVE -1             TO ARCHNUML
CIDMOD                 MOVE AL-UNBON       TO ARCHNUMA
CIDMOD                 PERFORM 9900-ERROR-FORMAT THRU
CIDMOD                         9900-EXIT
CIDMOD                 GO TO 8200-SEND-DATAONLY
CIDMOD             ELSE
CIDMOD                 MOVE ER-0006             TO EMI-ERROR
CIDMOD                 MOVE -1                  TO ARCHNUML
CIDMOD                 MOVE AL-UNBON            TO ARCHNUMA
CIDMOD                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
CIDMOD                 GO TO 8200-SEND-DATAONLY
CIDMOD             END-IF
CIDMOD         END-IF
121802*    ELSE
121802*        IF TB-INDX = 1
121802*            MOVE ER-0006             TO EMI-ERROR
121802*            MOVE -1                  TO ARCHNUML
121802*            MOVE AL-UNBON            TO ARCHNUMA
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            GO TO 8200-SEND-DATAONLY
121802*        END-IF
121802*    END-IF
CIDMOD
01958
01959      SET TB-INDX DOWN BY 1.
01960      SET PI-TOTAL-LINES          TO TB-INDX.
01961      MOVE 1                      TO PI-CURRENT-LINE.
01962      SET TB-INDX TO 1.
01963
01964      PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT
01965              VARYING SC-INDX FROM 1 BY 1
01966                UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
01967
01968      MOVE SPACES                 TO PI-ADDR-TYPE
01969                                     MAINTI.
01970      MOVE -1                     TO MAINTL.
01971
01972      GO TO 8100-SEND-INITIAL-MAP.
01973
01974  1030-TEXT-BUILD.
01975 ***** IF THERE ARE NO ADDRESS LINES OR LESS THAN 6 LINES, SET TO
01976 ***** TOP OF FORM.
01977      IF TB-INDX < 7
01978         SET TB-INDX   TO 7.
01979
01980      IF TB-INDX = 7
01981         MOVE TOP-FORM            TO REC-TEXT (TB-INDX)
01982         SET TB-INDX UP BY 1.
01983
01984      MOVE LA-TEXT-LINE           TO REC-TEXT (TB-INDX).
01985      MOVE LA-SKIP-CONTROL        TO REC-PC (TB-INDX)
01986                                     INDX-WORK.
01987
01988      IF INDX-WORK NOT = 99
01989         SET TB-INDX UP BY 1
01990         SET TB-INDX UP BY INDX-WORK
01991      ELSE
01992         SET TB-INDX UP BY 1
01993         MOVE TOP-FORM            TO REC-TEXT (TB-INDX)
01994         SET TB-INDX UP BY 1.
01995
01996  1040-FORMAT-RESEND.
01997      IF LA-RESEND-DATE = LOW-VALUES
01998         MOVE SPACES                 TO RESENDI
01999        ELSE
02000         MOVE LA-RESEND-DATE         TO DC-BIN-DATE-1
02001         MOVE ' '                    TO DC-OPTION-CODE
02002         PERFORM 9700-DATE-LINK  THRU 9700-EXIT
02003         MOVE DC-GREG-DATE-1-EDIT    TO RESENDO.
02004
02005  1050-GET-CORRESPOND.
02006      MOVE LA-CARRIER             TO ACTV-CARRIER.
02007      MOVE LA-CLAIM-NO            TO ACTV-CLAIM.
02008      MOVE LA-CERT-NO             TO ACTV-CERT-NUM.
02009      MOVE LA-CORR-TRLR-SEQ       TO ACTV-SEQ.
02010
02011      
      * EXEC CICS READ
02012 *         DATASET     (ACTV-ID)
02013 *         RIDFLD      (ACTV-KEY)
02014 *         SET         (ADDRESS OF ACTIVITY-TRAILERS)
02015 *    END-EXEC.
      *    MOVE '&"S        E          (   #00007792' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303037373932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02016
02017      IF AT-RECEIPT-FOLLOW-UP = LOW-VALUES
02018         MOVE SPACES                TO FOLLOWI
02019      ELSE
02020         MOVE SPACES                TO DC-OPTION-CODE
02021         MOVE AT-RECEIPT-FOLLOW-UP  TO DC-BIN-DATE-1
02022         PERFORM 9700-DATE-LINK THRU 9700-EXIT
02023         MOVE DC-GREG-DATE-1-EDIT   TO FOLLOWI.
02024
02025      MOVE SPACES                 TO ADDRI
02026      MOVE AT-STD-LETTER-FORM     TO FORMI
02027      MOVE AT-REASON-TEXT         TO REI.
02028
02029  1059-EXIT.
02030       EXIT.
02031      EJECT
02032  2000-CREATE.
02033 ***************************************************************
02034 *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
02035 *    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
02036 *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
02037 *    WILL BE DISPLAYED ONTO THE SCREEN.                       *
02038 *                                                             *
02039 ***************************************************************
02040
02041 ***************************************************************
02042 *    CHECK TO SEE IF SAME REQUEST OR NOT.                     *
02043 *    IF NEW REQUEST AND A LETTER WAS PRINTED, FORCE AN ERROR  *
02044 ***************************************************************
02045
02046      IF PI-CREATE-MODE
02047         IF PI-FORM-NUMBER = FORMI AND PI-ADDR-TYPE = ADDRI AND
02048            PI-LETTER-ADDRESS-TYPE = W-LETTER-ADDRESS-TYPE
110404           PERFORM 7500-READ-TS  THRU 7599-EXIT
110404           PERFORM 7950-SET-INDX THRU 7950-EXIT
110404           PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
110404             VARYING SC-INDX FROM 1 BY 1
110404             UNTIL SC-INDX > NUM-LINES-PER-SCREEN
02049            MOVE -1               TO MAINTL
      *          MOVE 'Y'              TO CLEANO
02050            GO TO 8200-SEND-DATAONLY
02051         ELSE
02052            IF PRINT-PERFORMED AND
02053               PI-RETURN-TO-PROGRAM  =  PGM-EL150 AND
02054               FORMI NOT = 9999
02055               MOVE ER-0279           TO EMI-ERROR
02056               MOVE -1                TO MAINTL
02057               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02058               GO TO 8200-SEND-DATAONLY.
02059
02060 ***************************************************************
02061 *   IF THE FORM NUMBER IS THE SAME BUT THE ADDRESS TYPE       *
02062 *   HAS CHANGED, THEN SAVE THE CURRENT SCREEN AND REBUILD     *
02063 *   THE ADDRESS DATA.                                         *
02064 ***************************************************************
02065
02066      IF PI-FORM-NUMBER = FORMI AND
02067         PI-ADDR-TYPE  =  ADDRI AND
02068         PI-LETTER-ADDRESS-TYPE = W-LETTER-ADDRESS-TYPE
02069            MOVE -1                  TO MAINTL
02070            GO TO 8200-SEND-DATAONLY.
02071
02072      IF PI-LETTER-ADDRESS-TYPE NOT = W-LETTER-ADDRESS-TYPE
02073          MOVE 0                  TO GETMAIN-SWITCH
02074          MOVE 'N'                TO WS-ACCT-READ-SW
02075                                     WS-PROD-READ-SW
02076                                     WS-COMP-READ-SW.
02077
02078      MOVE SPACES                 TO RECORD-TABLE.
02079
PEMTST     PERFORM 7000-READ-ADDR THRU 7099-EXIT.
02081
PEMTST     SET TB-INDX TO 1
02083      MOVE TOP-FORM               TO REC-TEXT (TB-INDX).
02084      SET TB-INDX UP BY 1.
02085
02086 ***************************************************************
02087 *    IF A NEW LETTER IS BEING CREATED FROM SCRATCH, SAVE      *
02088 *    THE EXISTING SCREEN AND PASS CONTROL TO THE TEXT EDITOR  *
02089 ***************************************************************
02090
02091      IF FORMI = '9999'
02092         MOVE 16                  TO PI-TOTAL-LINES
02093         MOVE 1                   TO PI-CURRENT-LINE
02094         PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
02095         PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT
02096         MOVE '3'                 TO PI-ACTION
02097         MOVE FORMI               TO PI-FORM-NUMBER
02098                                     PI-COMM-CONTROL
02099         MOVE ZEROS               TO PI-UPDATE-SW
02100         MOVE PGM-EL1042          TO PGM-NAME
02101         GO TO 9300-XCTL.
02102
02103      MOVE FORMI                  TO TEXT-LETTER.
02104      MOVE TEXT-PARTIAL-KEY       TO TEXT-SAVE-KEY.
02105
02106      
      * EXEC CICS HANDLE CONDITION
02107 *         NOTFND     (2120-ENDBR)
02108 *         ENDFILE    (2120-ENDBR)
02109 *         NOTOPEN    (8890-TEXT-NOT-OPEN)
02110 *    END-EXEC
      *    MOVE '"$I''J                 ! '' #00007893' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303037383933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02111
02112      
      * EXEC CICS STARTBR
02113 *         DATASET    (TEXT-ID)
02114 *         RIDFLD     (TEXT-KEY)
02115 *         GTEQ
02116 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00007899' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303037383939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-ID, 
                 TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02117
02118      MOVE 'Y'                    TO TEXT-BROWSE-STARTED.
02119
02120  2110-READ-NEXT.
02121      IF TB-INDX > MAX-LINES
02122         MOVE ER-0051             TO EMI-ERROR
02123         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02124         GO TO 2120-ENDBR.
02125
02126      
      * EXEC CICS READNEXT
02127 *         DATASET    (TEXT-ID)
02128 *         SET        (ADDRESS OF TEXT-FILES)
02129 *         RIDFLD     (TEXT-KEY)
02130 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00007913' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF TEXT-FILES TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02131
02132      IF TEXT-PARTIAL-KEY NOT = TEXT-SAVE-KEY
              IF TB-INDX = +2
                 MOVE SPACES           TO PI-FORM-NUMBER
                 MOVE -1               TO FORML
                 MOVE AL-UABON         TO FORMA
                 MOVE ER-1236          TO EMI-ERROR
                 MOVE 'N'              TO CLEANO
                 PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
              END-IF
02133         GO TO 2120-ENDBR
           END-IF
02134
PEMTST     MOVE FORMI                  TO PI-FORM-NUMBER.
02135      IF TX-LINE-SQUEEZE-CONTROL = 'Z'
02136          PERFORM 2800-PROCESS-Z-CONTROLS THRU 2800-EXIT
02137          GO TO 2110-READ-NEXT.
02138
02139       MOVE TX-BSR-CODE           TO PI-ELLETR-BSR.
02140
030612      IF (PI-COMPANY-ID = 'CID' OR 'AHL')
010407         AND (PI-CARRIER NOT = '8')
010407         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
010407            B1 > +20
010407            IF TX-TEXT-LINE (B1:5) = '@03.1'
010407               GO TO 2110-READ-NEXT
010407            END-IF
010407         END-PERFORM
010407      END-IF
02141       MOVE TX-TEXT-LINE          TO REC-TEXT (TB-INDX).
02142       MOVE TX-PROCESS-CONTROL    TO REC-PC (TB-INDX)
02143                                     INDX-WORK.
02144       SET TB-INDX UP BY 1.
02145       IF INDX-WORK = 99
02146          MOVE TOP-FORM           TO REC-TEXT (TB-INDX)
02147          SET TB-INDX UP BY 1
02148          GO TO 2110-READ-NEXT
02149       ELSE
02150          SET TB-INDX UP BY INDX-WORK
02151          GO TO 2110-READ-NEXT.
02152
02153  2120-ENDBR.
02154      IF TEXT-BROWSE-STARTED = 'Y'
02155         MOVE 'N'                 TO TEXT-BROWSE-STARTED
02156         
      * EXEC CICS ENDBR
02157 *            DATASET     (TEXT-ID)
02158 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00007963' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303037393633' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TEXT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02159
033110*02160      IF TB-INDX = 8
033110*02161         MOVE ER-0006             TO EMI-ERROR
033110*02162         MOVE -1                  TO FORML
033110*02163         MOVE AL-UABON            TO FORMA
033110*02164         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
033110*02165         GO TO 8200-SEND-DATAONLY-ERASEAUP.
02166
02167      SET TB-INDX DOWN BY 1.
02168      SET PI-TOTAL-LINES          TO TB-INDX.
02169      MOVE 1                      TO PI-CURRENT-LINE.
02170
02171 ***************************************************************
02172 *    IF IT IS A DESIGNATED CLAIM, THEN RESOLVE ALL            *
02173 *    VARIABLE SYMBOLS AND INSERT THEM INTO THE TEXT DATA.     *
02174 ***************************************************************
02175
02176      IF PI-RETURN-TO-PROGRAM = PGM-EL150
02177         PERFORM 7200-RESOLVE-VARIABLES THRU 7269-EXIT
02178         MOVE SPACES              TO W-REVERSE-DATE-SW
02179         PERFORM 7300-VARIABLE-SEARCH   THRU 7399-EXIT
02180                 VARYING TB-INDX FROM 7 BY 1 UNTIL
02181                 TB-INDX > PI-TOTAL-LINES
02182      ELSE
02183         MOVE ER-0373             TO EMI-ERROR
02184         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02185
02186      SET TB-INDX TO 1.
02187      PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT
02188              VARYING SC-INDX FROM 1 BY 1 UNTIL
02189              SC-INDX > NUM-LINES-PER-SCREEN.
02190
02191      MOVE '3'                    TO PI-ACTION.
PEMTST*    MOVE FORMI                  TO PI-FORM-NUMBER.
02193      MOVE W-LETTER-ADDRESS-TYPE  TO PI-LETTER-ADDRESS-TYPE.
02194      MOVE -1                     TO MAINTL.
02195      GO TO 8200-SEND-DATAONLY-ERASEAUP.
02196                                  EJECT
02197  2800-PROCESS-Z-CONTROLS.
02198
02199      MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
02200
02201      IF RESEND-SAVE = LOW-VALUES
02202          IF W-DAYS-TO-RESEND-1 NUMERIC
02203              IF W-DAYS-TO-RESEND-1 > ZEROS
02204                  MOVE '6'                    TO DC-OPTION-CODE
02205                  MOVE SAVE-BIN-DATE          TO DC-BIN-DATE-1
02206                  MOVE ZEROS                  TO DC-ELAPSED-MONTHS
02207                  MOVE W-DAYS-TO-RESEND-1     TO DC-ELAPSED-DAYS
02208                  PERFORM 9700-DATE-LINK THRU 9700-EXIT
02209                  IF NO-CONVERSION-ERROR
02210                      MOVE DC-BIN-DATE-2       TO RESEND-SAVE
02211                      MOVE DC-GREG-DATE-1-EDIT TO RESENDO
02212                      MOVE AL-UANON            TO RESENDA
02213                      MOVE +8                  TO RESENDL
02214                  ELSE
02215                      MOVE ER-3770             TO EMI-ERROR
02216                      MOVE -1                  TO MAINTL
02217                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02218                      GO TO 8200-SEND-DATAONLY.
02219
02220      IF FOLLOW-UP-SAVE = LOW-VALUES
02221          IF W-DAYS-TO-FOLLOW-UP NUMERIC
02222              IF W-DAYS-TO-FOLLOW-UP > ZEROS
02223                  MOVE '6'                 TO DC-OPTION-CODE
02224                  MOVE SAVE-BIN-DATE       TO DC-BIN-DATE-1
02225                  MOVE ZEROS               TO DC-ELAPSED-MONTHS
02226                  MOVE W-DAYS-TO-FOLLOW-UP TO DC-ELAPSED-DAYS
02227                  PERFORM 9700-DATE-LINK THRU 9700-EXIT
02228                  IF NO-CONVERSION-ERROR
02229                      MOVE DC-BIN-DATE-2       TO FOLLOW-UP-SAVE
02230                      MOVE DC-GREG-DATE-1-EDIT TO FOLLOWO
02231                      MOVE AL-UANON            TO FOLLOWA
02232                      MOVE +8                  TO FOLLOWL
02233                  ELSE
02234                      MOVE ER-3771             TO EMI-ERROR
02235                      MOVE -1                  TO MAINTL
02236                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02237                      GO TO 8200-SEND-DATAONLY.
02238
02239      IF COPIESI NOT NUMERIC
02240          IF W-NUMBER-OF-COPIES NUMERIC
02241              IF W-NUMBER-OF-COPIES > ZEROS
02242                  MOVE W-NUMBER-OF-COPIES  TO COPIESI
02243                  MOVE AL-UNNON            TO COPIESA
02244                  MOVE +1                  TO COPIESL
02245              ELSE
02246                  MOVE +1                  TO COPIESI
02247                  MOVE AL-UNNON            TO COPIESA
02248                  MOVE +1                  TO COPIESL.
02249
040110     IF W-FORM-TO-RESEND > SPACES
040110         MOVE W-FORM-TO-RESEND          TO PI-RESEND-FORM-NUMBER
040110     ELSE
040110         MOVE LOW-VALUES                TO PI-RESEND-FORM-NUMBER
040110     END-IF.
040110
040110     IF W-PROMPT-LETTER = 'Y'
040110         MOVE W-PROMPT-LETTER           TO PI-PROMPT-LETTER
040110         MOVE ER-0894                   TO EMI-ERROR
040110         PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT
040110     ELSE
040110         MOVE 'N'                       TO PI-PROMPT-LETTER
040110     END-IF.
040110
040110     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD
                                                 ENCO
           MOVE AL-UANON                      TO ENCA
           MOVE +3                            TO ENCL
           MOVE 'N'                           TO CLEANO
040110     MOVE W-AUTO-CLOSE-IND              TO PI-AUTO-CLOSE-IND.
040110     MOVE W-LETTER-TO-BENE              TO PI-LETTER-TO-BENE.
040110
02250  2800-EXIT.
02251      EXIT.
02252      EJECT
02253  3000-RECORD.
02254 ***************************************************************
02255 *    THIS ROUTINE WILL SAVE THE LETTER TEXT (IF ANY) AND      *
02256 *    BUILD A CORRESPONDENCE TRAILER WITH THE DATA FROM        *
02257 *    THE SCREEN.                                              *
02258 ***************************************************************
02259
02260      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
02261         MOVE ER-0211             TO EMI-ERROR
02262         MOVE -1                  TO MAINTL
02263         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02264         GO TO 8200-SEND-DATAONLY.
02265
02266      
      * EXEC CICS HANDLE CONDITION
02267 *         NOTOPEN    (8860-CLAM-NOT-OPEN)
02268 *         NOTFND     (3010-NOT-FOUND)
02269 *    END-EXEC.
      *    MOVE '"$JI                  ! ( #00008095' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303038303935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02270
02271      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
02272      PERFORM 7950-SET-INDX     THRU 7950-EXIT.
02273
02274      PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
02275              VARYING SC-INDX FROM 1 BY 1
02276              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
02277
02278      PERFORM 7000-READ-ADDR THRU 7099-EXIT.
02279
02280      IF NOT EMI-NO-ERRORS
02281         GO TO 8200-SEND-DATAONLY.
02282
02283      MOVE ZEROS               TO ARCH-NUMBER.
02284      PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
02285      MOVE ER-0000             TO EMI-ERROR.
02286      MOVE -1                  TO MAINTL.
02287      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02288      GO TO 8200-SEND-DATAONLY.
02289
02290  3010-NOT-FOUND.
02291      MOVE ER-0133             TO EMI-ERROR.
02292      MOVE -1                  TO MAINTL.
02293      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02294      GO TO 8200-SEND-DATAONLY.
02295
02296      EJECT
121802*4000-SET-BSR. Remove as dead code
121802*4001-READ-NEXT. Remove as dead code
121802*4002-ENDBR. Remove as dead code
121802*4000-EXIT. Remove as dead code
02357
02358      EJECT
02359  5000-MOVE-NAME.
02360 *    COPY ELCMNS.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCMNS.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                         *
00006 *                                                               *
00007 *                     M O V E   N A M E   R O U T I N E         *
00008 *                                                               *
00009 *                THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
00010 *            NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     *
00011 *            NO EMBEDDED BLANKS.                                *
00012 *                                                               *
00013 *                  FIELD               VALUE                    *
00014 *                                                               *
00015 *                LAST NAME (CL15)      SMITH                    *
00016 *                1ST NAME  (CL12)      JOHN                     *
00017 *                MID NAME  (CL12)      ALLEN                    *
00018 *                                                               *
00019 *                AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
00020 *                                                               *
00021 *                        SMITH, JOHN ALLEN                      *
00022 *                                                               *
00023 *                TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
00024 *            STORAGE COPYBOOK:                                  *
00025 *                                                               *
00026 *                01  WS-NAME-WORK-AREA COPY ELCNWA.             *
00027 *                                                               *
00028 *****************************************************************.
00029
00030      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
00031      MOVE ZERO                   TO  WS-NAME-SW.
00032      SET NWA-INDEX TO +1.
00033
00034      IF CL-INSURED-1ST-NAME = SPACES  AND
00035         CL-INSURED-MID-INIT = SPACES
00036          MOVE +1                 TO  WS-NAME-SW.
00037
00038      MOVE CL-INSURED-LAST-NAME  TO  WS-NAME-WORK2.
00039      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00040
00041      MOVE CL-INSURED-1ST-NAME   TO  WS-NAME-WORK2.
00042      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00043
00044      SET NWA-INDEX UP BY +1.
00045      MOVE CL-INSURED-MID-INIT   TO  WS-NAME-WORK2.
00046      PERFORM 5100-MOVE-NAME THRU 5190-EXIT.
00047
00048  5000-EXIT.
00049      EXIT.
00050
00051      EJECT
00052  5100-MOVE-NAME SECTION.
00053      IF WS-NAME-SW GREATER THAN +1
00054          GO TO 5190-EXIT.
00055
00056      IF WS-NAME-WORK2 = SPACES
00057          GO TO 5190-EXIT.
00058
00059      SET NWA-INDEX2 TO +1.
00060      SET NWA-INDEX3 TO +2.
00061
00062  5110-MOVE-NAME.
00063      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
00064
00065      IF NWA-INDEX LESS THAN +30
00066          SET NWA-INDEX UP BY +1
00067        ELSE
00068          ADD +2  TO  WS-NAME-SW
00069          GO TO 5190-EXIT.
00070
00071      IF NWA-INDEX2 LESS THAN +20
00072          SET NWA-INDEX3 UP BY +1
00073          SET NWA-INDEX2 UP BY +1.
00074
00075      IF WS-NW2 (NWA-INDEX2) = SPACES AND
00076         WS-NW2 (NWA-INDEX3) = SPACES
00077          IF WS-NAME-SW = ZERO
00078              MOVE ','            TO  WS-NW (NWA-INDEX)
00079              SET NWA-INDEX UP BY +2
00080              MOVE +1             TO  WS-NAME-SW
00081              GO TO 5190-EXIT
00082            ELSE
00083              GO TO 5190-EXIT.
00084
00085      GO TO 5110-MOVE-NAME.
00086
00087  5190-EXIT.
00088      EXIT.
00089
02361      EJECT
02362  5200-MOVE-NAME.
02363 *    NOTE *******************************************************
02364 *         *           M O V E   N A M E   R O U T I N E         *
02365 *         *                                                     *
02366 *         *      THE FOLLOWING ROUTINE MOVES THE INSURRED'S     *
02367 *         *  NAME FROM THE CLAIM MASTER TO A WORK AREA WITH     *
02368 *         *  NO EMBEDDED BLANKS.                                *
02369 *         *                                                     *
02370 *         *        FIELD               VALUE                    *
02371 *         *                                                     *
02372 *         *      LAST NAME (CL15)      SMITH                    *
02373 *         *      1ST NAME  (CL12)      JOHN                     *
02374 *         *      MID NAME  (CL1)       A                        *
02375 *         *                                                     *
02376 *         *      AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30)  *
02377 *         *                                                     *
02378 *         *              JOHN A. SMITH                          *
02379 *         *                                                     *
02380 *         *      TO USE THIS ROUTINE YOU ALSO NEED A WORKING    *
02381 *         *  STORAGE COPYBOOK:                                  *
02382 *         *                                                     *
02383 *         *      01  WS-NAME-WORK-AREA COPY ELCNWA.             *
02384 *         *                                                     *
02385 *         *******************************************************.
02386
02387      MOVE SPACES                 TO  WS-NAME-WORK-AREA.
02388      MOVE ZERO                   TO  WS-NAME-SW.
02389      SET NWA-INDEX TO +1.
02390
02391      IF W-FIRST-NAME = SPACES
02392              AND
02393         W-MIDDLE-NAME = SPACES
02394          MOVE W-LAST-NAME        TO WS-NAME-WORK
02395          GO TO 5200-EXIT.
02396
02397      MOVE W-FIRST-NAME           TO  WS-NAME-WORK2.
02398      PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
02399
02400      SET NWA-INDEX UP BY +1.
02401
02402      IF W-MIDDLE-NAME NOT = SPACES
02403         MOVE W-MIDDLE-NAME       TO  WS-NW (NWA-INDEX)
02404         SET NWA-INDEX UP BY +1
02405         MOVE '.'                 TO  WS-NW (NWA-INDEX)
02406         SET NWA-INDEX UP BY +2.
02407
02408      MOVE W-LAST-NAME            TO  WS-NAME-WORK2.
02409      PERFORM 5300-MOVE-NAME THRU 5390-EXIT.
02410
02411  5200-EXIT.
02412      EXIT.
02413
02414      EJECT
02415  5300-MOVE-NAME.
02416      IF WS-NAME-SW > +1
02417          GO TO 5390-EXIT.
02418
02419      IF WS-NAME-WORK2 = SPACES
02420          GO TO 5390-EXIT.
02421
02422      SET NWA-INDEX2 TO +1.
02423      SET NWA-INDEX3 TO +2.
02424
02425  5310-MOVE-NAME.
02426      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).
02427
02428      IF NWA-INDEX < +30
02429         SET NWA-INDEX UP BY +1
02430      ELSE
02431         ADD +2  TO  WS-NAME-SW
02432         GO TO 5390-EXIT.
02433
02434      IF NWA-INDEX2 < +20
02435          SET NWA-INDEX2 UP BY +1
02436          SET NWA-INDEX3 UP BY +1.
02437
02438      IF WS-NW2 (NWA-INDEX2) = SPACES AND
02439         WS-NW2 (NWA-INDEX3) = SPACES
02440         GO TO 5390-EXIT.
02441
02442      GO TO 5310-MOVE-NAME.
02443
02444  5390-EXIT.
02445      EXIT.
02446
02447      EJECT
02448  6100-ADDR-MAINT.
02449 ***************************************************************
02450 *    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *
02451 *    AND THE TS-TABLE OF LETTER TEXT.                         *
02452 *    IT WILL THEN XCTL TO THE ADDRESS MAINT PROGRAM.          *
02453 ***************************************************************
02454
030805     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
030805             VARYING SC-INDX FROM 1 BY 1
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
030805
02455      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
02456         MOVE ER-0343             TO EMI-ERROR
02457         MOVE -1                  TO MAINTL
02458         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02459         GO TO 8200-SEND-DATAONLY.
02460
081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT
02461 *    PERFORM 7950-SET-INDX THRU 7950-EXIT.
02462 *    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
02463 *            VARYING SC-INDX FROM 1 BY 1
02464 *            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
02465
02466      PERFORM 7700-PUT-TEMP-STOR    THRU 7749-EXIT.
02467      PERFORM 7790-WRITE-SCREEN-TS  THRU 7790-EXIT.
02468      PERFORM 7795-WRITE-PI-AREA-TS THRU 7795-EXIT.
02469      MOVE PGM-EL141 TO PGM-NAME.
02470
02471      GO TO 9300-XCTL.
02472      EJECT
02473  6200-EDIT-MODE.
02474 ***************************************************************
02475 *    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *
02476 *    AND THE TS-TABLE OF LETTER TEXT.                         *
02477 *    IT WILL THEN XCTL TO THE TEXT-EDITOR PROGRAM.            *
02478 ***************************************************************
PEMTST     IF PI-SHOW-MODE
PEMTST        MOVE ER-0188             TO EMI-ERROR
PEMTST        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
PEMTST        MOVE -1                  TO MAINTL
PEMTST        MOVE AL-UABON            TO MAINTA
PEMTST        GO TO 8200-SEND-DATAONLY
PEMTST     END-IF
02485
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        IF PI-BSR-AUTOMATED
121802*            MOVE ER-0912        TO EMI-ERROR
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            MOVE -1             TO MAINTL
121802*            MOVE AL-UABON       TO MAINTA
121802*            GO TO 8200-SEND-DATAONLY.
02493
02494      IF PI-CURRENT-LINE = ZEROS
02495         MOVE ER-0187             TO EMI-ERROR
02496         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02497         MOVE -1                  TO MAINTL
02498         GO TO 8200-SEND-DATAONLY.
02499
081004     PERFORM 7500-READ-TS        THRU 7599-EXIT
081004     PERFORM 7750-DELETE-TEMP-STOR
                                       THRU 7750-EXIT
02500      PERFORM 7950-SET-INDX THRU 7950-EXIT.
02501      PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
02502              VARYING SC-INDX FROM 1 BY 1
02503              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
02504
02505      PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT.
02506      PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT.
02507
02508      MOVE PI-FORM-NUMBER         TO PI-COMM-CONTROL.
02509      MOVE ZEROS                  TO PI-UPDATE-SW.
02510      MOVE '1'                    TO PI-ENTRY-CD-1.
02511      MOVE PGM-EL1042             TO PGM-NAME.
02512      GO TO 9300-XCTL.
02513
02514      EJECT
02515  6400-LETTER-RELEASE.
02516 ***************************************************************
02517 *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
02518 *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *
02519 *    THE ARCHIVE FILE.                                        *
02520 *    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *
02521 *        1. CHECK SECURITY AND IF IT IS A DESIGNATED LETTER.  *
02522 *        2. RE-EDIT DATA AND UPDATE TS-TABLE WITH CHANGES     *
02523 *        3. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *
02524 *        4. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
02525 *        5. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *
02526 *        7. BUILD A CORRESPONDENCE TRAILER                    *
02527 *        8. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *
02528 *                 ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.*
02529 *        4. RESET ALL CONTROL FIELDS AND RETURN THE           *
02530 *                 ARCHIVE NUMBER USED TO FILE THE RECORDS.    *
02531 ***************************************************************
02532
030805     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
030805             VARYING SC-INDX FROM 1 BY 1
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
030805
02533      IF NOT MODIFY-CAP
02534         MOVE ER-0070             TO EMI-ERROR
02535         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02536         MOVE -1                  TO MAINTL
02537         GO TO 8200-SEND-DATAONLY.
02538
02539      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
02540         MOVE ER-0211             TO EMI-ERROR
02541         MOVE -1                  TO MAINTL
02542         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02543         GO TO 8200-SEND-DATAONLY.
02544
081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT
02545 *    PERFORM 7950-SET-INDX THRU 7950-EXIT.
02546 *    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
02547 *            VARYING SC-INDX FROM 1 BY 1
02548 *            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
02549
02550      IF PI-ADDR-TYPE  NOT =  ADDRI
02551         PERFORM 7000-READ-ADDR THRU 7099-EXIT.
02552
02553      IF NOT EMI-NO-ERRORS
02554         GO TO 8200-SEND-DATAONLY.
02555
02556      MOVE +0                     TO TALLY.
02557      INSPECT RECORD-TABLE TALLYING TALLY
02558                                  FOR CHARACTERS BEFORE '@'.
02559
02560      IF TALLY < +21900
02561         COMPUTE PI-CURRENT-LINE = TALLY / 73
02562         MOVE ZEROS               TO ROLL-COUNTER
02563         MOVE ER-0191             TO EMI-ERROR
02564         MOVE -1                  TO MAINTL
02565         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02566         GO TO 7900-ROLL-PAGE.
02567
02568      MOVE '1'                    TO CNTL-RECORD-TYPE.
02569      MOVE ZEROS                  TO CNTL-SEQ.
02570      MOVE SPACES                 TO CNTL-GENL.
02571      
      * EXEC CICS READ
02572 *         DATASET    (CNTL-ID)
02573 *         SET        (ADDRESS OF CONTROL-FILE)
02574 *         RIDFLD     (CNTL-KEY)
02575 *         UPDATE
02576 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008454' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02577
02578      ADD 1                       TO CF-CO-ARCHIVE-COUNTER.
02579      MOVE CF-CO-ARCHIVE-COUNTER  TO ARCH-NUMBER.
02580      MOVE CF-PRINT-ADDRESS-LABELS   TO  WS-LABELS-SW.
02581
02582      
      * EXEC CICS REWRITE
02583 *         FROM      (CONTROL-FILE)
02584 *         DATASET   (CNTL-ID)
02585 *    END-EXEC
           MOVE LENGTH OF
            CONTROL-FILE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008465' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 CONTROL-FILE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02586
02587      PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.
02588
02589      
      * EXEC CICS HANDLE CONDITION
02590 *         NOTOPEN   (9990-ABEND)
02591 *    END-EXEC.
      *    MOVE '"$J                   ! ) #00008472' TO DFHEIV0
           MOVE X'22244A202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303038343732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02592
02593      
      * EXEC CICS GETMAIN
02594 *         SET      (ADDRESS OF LETTER-ARCHIVE)
02595 *         LENGTH   (ARCH-LENGTH)
02596 *    END-EXEC.
      *    MOVE '," L                  $   #00008476' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038343736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ARCH-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02597
02598      MOVE SPACES                 TO LETTER-ARCHIVE.
02599      MOVE 'LA'                   TO LA-RECORD-ID.
02600      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
02601                                     LA-ARCHIVE-NO-A1.
02602      MOVE '1'                    TO LA-RECORD-TYPE
02603                                     LA-RECORD-TYPE-A1.
02604      MOVE ZEROS                  TO LA-LINE-SEQ-NO
02605                                     LA-LINE-SEQ-NO-A1.
02606      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
02607                                     LA-COMPANY-CD-A1.
02608      MOVE CLAM-CARRIER           TO LA-CARRIER.
02609      MOVE PI-CLAIM-NO            TO LA-CLAIM-NO.
02610      MOVE PI-CERT-NO             TO LA-CERT-NO.
02611
CIDMOD*    IF PI-COMPANY-ID = 'DMD'
CIDMOD*        MOVE WS-DMD-CERT-STATE    TO LA-DMD-UND-CODE
CIDMOD*        MOVE WS-DMD-CERT-GROUPING TO LA-DMD-PROD-CODE
CIDMOD*        MOVE WS-DMD-BEN-CODE      TO LA-DMD-BEN-CODE
CIDMOD*        MOVE WS-DMD-CORR-TRLR-SEQ TO LA-DMD-CORR-TRLR-SEQ
CIDMOD*        MOVE WS-DMD-LETTER-FORM   TO LA-DMD-LETTER-FORM
CIDMOD*        MOVE WS-DMD-RES-ST        TO LA-DMD-RES-ST
CIDMOD*        MOVE LOW-VALUES           TO LA-DMD-LETTER-PURGE-DT
CIDMOD*                                     LA-DMD-LETTER-RELOAD-DT
CIDMOD*        MOVE '1'                  TO LA-DMD-LETTER-STATUS
CIDMOD*        EXEC CICS GETMAIN
CIDMOD*            SET    (ADDRESS OF LETTER-ARCHIVE-TEMP)
CIDMOD*            LENGTH (ARCT-LENGTH)
CIDMOD*        END-EXEC.
02626
02627      IF COPIESL NOT = ZEROS
02628         MOVE COPIESI             TO LA-NO-OF-COPIES
02629      ELSE
02630         MOVE  1                  TO LA-NO-OF-COPIES.
02631
02632      IF RESENDL NOT = ZEROS
02633         MOVE RESEND-SAVE         TO LA-RESEND-DATE
02634      ELSE
02635         MOVE LOW-VALUES          TO LA-RESEND-DATE.
02636
02637      MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.
02638      MOVE CURRENT-SAVE           TO LA-CREATION-DT.
02639
02640      IF PRINT-PERFORMED
02641         MOVE CURRENT-SAVE        TO LA-INITIAL-PRINT-DATE
02642      ELSE
02643         MOVE LOW-VALUES          TO LA-INITIAL-PRINT-DATE.
02644
02645      MOVE LOW-VALUES             TO LA-RESEND-PRINT-DATE.
02646      MOVE CORR-TRLR-SEQ          TO LA-CORR-TRLR-SEQ.
02647      PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.
033110
040110     IF PI-PROMPT-LETTER NOT EQUAL 'Y'
033110         PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT
040110     END-IF.
02648
02649      IF WS-LABELS-SW = 'N'
02650          NEXT SENTENCE
02651      ELSE
02652          SET TB-INDX TO 1
02653          MOVE ZEROS                  TO SEQ-COUNTER
02654          PERFORM 6480-FORMAT-ADDRESS
02655                VARYING TB-INDX FROM 1 BY 1 UNTIL
02656                TB-INDX > 6.
02657
02658      MOVE ZEROS                  TO SEQ-COUNTER.
033110     MOVE 'N'                    TO WS-SKIP-EMAIL
02659      PERFORM 6470-FORMAT-TEXT THRU 6479-EXIT
02660              VARYING TB-INDX FROM 8 BY 1
02661              UNTIL TB-INDX > PI-TOTAL-LINES.
033110     MOVE 'Y'                    TO WS-SKIP-EMAIL.
PEMTST     IF SEQ-COUNTER < +15
033110      AND WS-SKIP-EMAIL EQUAL 'N'
              MOVE ARCH-NUMBER         TO TRAN-DL2-ARCHNO
              MOVE PI-PROCESSOR-ID     TO TRAN-DL2-USER
              MOVE SPACES              TO TRAN-DL2-CCC
              STRING PI-CARRIER ' ' PI-CLAIM-NO ' ' PI-CERT-NO
                 DELIMITED BY SIZE
                 INTO TRAN-DL2-CCC
              END-STRING
              
      * EXEC CICS WRITEQ TD
      *          QUEUE ('BTCH')
      *          FROM (TRAN-DATA-LINE1)
      *          LENGTH (80)
      *       END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00008560' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              
      * EXEC CICS WRITEQ TD
      *          QUEUE ('BTCH')
      *          FROM (TRAN-DATA-LINE2)
      *          LENGTH (80)
      *       END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00008565' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE2, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              
      * EXEC CICS WRITEQ TD
      *          QUEUE ('BTCH')
      *          FROM (TRAN-DATA-LINE3)
      *          LENGTH (80)
      *       END-EXEC
           MOVE 80
             TO DFHEIV11
           MOVE 'BTCH' TO DFHEIV5
      *    MOVE '(" L   L              &   #00008570' TO DFHEIV0
           MOVE X'2822204C2020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303038353730' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV5, 
                 TRAN-DATA-LINE3, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           END-IF
02662      MOVE ER-0280                TO EMI-ERROR.
02663
02664      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02665      MOVE ARCH-NUMBER            TO ARCH-SUPPRESS
02666      MOVE ARCH-EDIT              TO EMI-TEXT-VARIABLE (1).
02667
02668      GO TO 9400-CLEAR.
02669
02670      EJECT
02671  6470-FORMAT-TEXT.
033110
033110     IF REC-TEXT (TB-INDX) (1:16) = NAPERSOFT-LETTER
033110         MOVE 'Y'                TO WS-SKIP-EMAIL
033110     END-IF.
033110
02672      MOVE SPACES                 TO LETTER-ARCHIVE.
02673      MOVE '3'                    TO LA-RECORD-TYPE
02674                                     LA-RECORD-TYPE-A1.
02675      MOVE 'LA'                   TO LA-RECORD-ID.
02676      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
02677                                     LA-ARCHIVE-NO-A1.
02678      MOVE SEQ-COUNTER            TO LA-LINE-SEQ-NO
02679                                     LA-LINE-SEQ-NO-A1.
02680      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
02681                                     LA-COMPANY-CD-A1.
02682      MOVE REC-TEXT (TB-INDX)     TO LA-TEXT-LINE.
02683      SET TB-INDX1 TO TB-INDX.
02684      SET TB-INDX1 UP BY 1.
02685      MOVE ZEROS                  TO INDX-WORK.
02686
02687  6472-LOOP.
02688      IF TB-INDX1 < PI-TOTAL-LINES AND
02689         REC-TEXT (TB-INDX1) = SPACES
02690            SET TB-INDX1 UP BY 1
02691            ADD 1                 TO INDX-WORK
02692            GO TO 6472-LOOP.
02693
02694      IF REC-TEXT (TB-INDX1) = TOP-FORM
02695         MOVE '99'                TO LA-SKIP-CONTROL
02696         SET TB-INDX1 UP BY 1
02697      ELSE
02698         MOVE INDX-WORK           TO LA-SKIP-CONTROL.
02699
02700      SET TB-INDX TO TB-INDX1.
02701      SET TB-INDX DOWN BY 1.
02702      PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.
02703      ADD 1 TO SEQ-COUNTER.
02704
02705  6479-EXIT.
02706       EXIT.
02707
02708      EJECT
02709  6480-FORMAT-ADDRESS.
02710      MOVE SPACES                 TO LETTER-ARCHIVE.
02711      MOVE '2'                    TO LA-RECORD-TYPE
02712                                     LA-RECORD-TYPE-A1.
02713      MOVE 'LA'                   TO LA-RECORD-ID.
02714      MOVE ARCH-NUMBER            TO LA-ARCHIVE-NO
02715                                     LA-ARCHIVE-NO-A1.
02716      MOVE SEQ-COUNTER            TO LA-LINE-SEQ-NO
02717                                     LA-LINE-SEQ-NO-A1.
02718      MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
02719                                     LA-COMPANY-CD-A1.
02720      MOVE REC-TEXT (TB-INDX)     TO LA-ADDRESS-LINE.
02721
02722      PERFORM 6490-WRITE-ARCHIVE THRU 6499-EXIT.
02723      ADD 1 TO SEQ-COUNTER.
02724
02725      EJECT
02726  6490-WRITE-ARCHIVE.
02727      
      * EXEC CICS HANDLE CONDITION
02728 *        DUPKEY    (6499-EXIT)
02729 *    END-EXEC.
      *    MOVE '"$$                   ! * #00008646' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303038363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02730
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE LETTER-ARCHIVE     TO LETTER-ARCHIVE-TEMP
121802*        MOVE 'LT'               TO LT-RECORD-ID
121802*        EXEC CICS WRITE
121802*            DATASET   (ARCT-ID)
121802*            FROM      (LETTER-ARCHIVE-TEMP)
121802*            RIDFLD    (LT-CONTROL-PRIMARY)
121802*        END-EXEC.
02739
02740      
      * EXEC CICS WRITE
02741 *         DATASET   (ARCH-ID)
02742 *         FROM      (LETTER-ARCHIVE)
02743 *         RIDFLD    (LA-CONTROL-PRIMARY)
02744 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008659' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ARCH-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02745
02746  6499-EXIT.
02747      EXIT.
02748
02749      EJECT
02750  6500-BUILD-CORRESPOND.
02751 ***************************************************************
02752 *    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *
02753 *    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *
02754 *    USING THE NEW SEQUENCE NUMBER.                           *
02755 *    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *
02756 *    TRAILER RECORD.                                          *
02757 ***************************************************************
02758
02759      MOVE PI-CLAIM-NO            TO CLAM-CLAIM.
02760
02761      
      * EXEC CICS READ
02762 *         DATASET    (CLAM-ID)
02763 *         SET        (ADDRESS OF CLAIM-MASTER)
02764 *         RIDFLD     (CLAM-KEY)
02765 *         UPDATE
02766 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00008680' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02767
02768      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.
02769
02770      IF FOLLOW-UP-SAVE > CL-NEXT-FOLLOWUP-DT
02771         MOVE FOLLOW-UP-SAVE      TO CL-NEXT-FOLLOWUP-DT.
02772
02773      IF RESEND-SAVE > CL-NEXT-FOLLOWUP-DT
02774         MOVE RESEND-SAVE         TO CL-NEXT-FOLLOWUP-DT.
02775
02776      IF MAINTI = 'C'
02777         MOVE '2'                 TO CL-LAST-MAINT-TYPE.
02778
02779      IF MAINTI = 'R'
02780         PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT
02781         MOVE ACTV-SEQ            TO PI-ADDR-SEQ.
02782
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE 04                 TO CL-ACTIVITY-CODE.
02785
02786      
      * EXEC CICS GETMAIN
02787 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
02788 *         INITIMG   (GETMAIN-SPACE)
02789 *         LENGTH    (ACTV-LENGTH)
02790 *    END-EXEC.
      *    MOVE ',"IL                  $   #00008705' TO DFHEIV0
           MOVE X'2C22494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038373035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACTV-LENGTH, 
                 GETMAIN-SPACE
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02791
02792      MOVE 'AT'                   TO AT-RECORD-ID.
02793      MOVE  4                     TO AT-TRAILER-TYPE.
02794      MOVE CURRENT-SAVE           TO AT-RECORDED-DT
02795                                     CL-LAST-MAINT-DT
02796                                     AT-CORR-LAST-MAINT-DT
02797      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY
02798                                     CL-LAST-MAINT-USER
02799                                     AT-CORR-LAST-UPDATED-BY
02800      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS
02801                                     CL-LAST-MAINT-HHMMSS.
02802      MOVE ACTV-KEY               TO AT-CONTROL-PRIMARY.
02803      MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.
02804      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO
02805                                     CORR-TRLR-SEQ.
02806      MOVE CURRENT-SAVE           TO AT-LETTER-SENT-DT.
02807      MOVE FOLLOW-UP-SAVE         TO AT-RECEIPT-FOLLOW-UP.
02808      MOVE RESEND-SAVE            TO AT-AUTO-RE-SEND-DT.
02809      MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT
02810                                     AT-LETTER-PURGED-DT.
02811      MOVE ARCH-NUMBER            TO AT-LETTER-ARCHIVE-NO.
02812      MOVE '1'                    TO AT-LETTER-ORIGIN.
040110     MOVE PI-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM.
040110     MOVE PI-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND.
040110     MOVE PI-LETTER-TO-BENE      TO AT-LETTER-TO-BENE.
02813
CIDMOD*    IF PI-COMPANY-ID = 'DMD'
CIDMOD*        MOVE '1'                TO AT-DMD-LETTER-STATUS
CIDMOD*        IF PI-BSR-AUTOMATED
CIDMOD*            MOVE 'A'            TO AT-DMD-BSR-CODE.
CIDMOD*
02819      IF FORML  NOT = ZEROS
02820         MOVE FORMI               TO AT-STD-LETTER-FORM
02821      ELSE
02822         MOVE SPACES              TO AT-STD-LETTER-FORM.
02823
02824      IF REL NOT = ZEROS
02825         MOVE REI                 TO AT-REASON-TEXT
02826      ELSE
02827         MOVE SPACES              TO AT-REASON-TEXT.
02828
121802*    IF PI-COMPANY-ID = 'HAN'  OR  'JHL'
121802*        MOVE AT-REASON-TEXT     TO WS-REASON-TEXT
121802*        IF WS-RE-NDX NUMERIC  AND
121802*           WS-RE-NDX > ZERO   AND
121802*           WS-RE-NDX < 33
121802*            MOVE HAN-REASON-TEXT (WS-RE-NDX)
121802*                                TO AT-REASON-TEXT.
02836
02837      MOVE PI-ADDR-SEQ            TO AT-ADDRESS-REC-SEQ-NO.
02838
02839      IF PI-ADDR-TYPE > SPACES
02840          MOVE PI-ADDR-TYPE          TO AT-ADDRESEE-TYPE
02841          MOVE REC-TEXT (1)          TO AT-ADDRESSEE-NAME
02842      ELSE
02843      IF W-ACCOUNT > ZEROS
02844          MOVE 'A'                   TO AT-ADDRESEE-TYPE
02845          MOVE SS06D                 TO AT-ADDRESSEE-NAME
02846      ELSE
02847      IF W-BENEFICIARY > ZEROS
02848           MOVE 'B'                  TO AT-ADDRESEE-TYPE
02849           IF W-BENEFICIARY = 9
02850               MOVE SS61D            TO AT-ADDRESSEE-NAME
02851             ELSE
02852               MOVE SS43D            TO AT-ADDRESSEE-NAME
02853      ELSE
02854      IF W-EMPLOYER > ZEROS
02855           MOVE 'E'                  TO AT-ADDRESEE-TYPE
02856           MOVE SS48D                TO AT-ADDRESSEE-NAME
02857      ELSE
02858      IF W-INSURED > ZEROS
02859           MOVE 'I'                  TO AT-ADDRESEE-TYPE
02860           MOVE SS57D                TO AT-ADDRESSEE-NAME
02861      ELSE
02862      IF W-PHYSICIAN > ZEROS
02863           MOVE 'P'                  TO AT-ADDRESEE-TYPE
02864           MOVE SS47D                TO AT-ADDRESSEE-NAME
02865      ELSE
02866      IF W-OTHER-1 > ZEROS
02867           MOVE 'O'                  TO AT-ADDRESEE-TYPE
02868           MOVE SS49D                TO AT-ADDRESSEE-NAME
02869      ELSE
02870      IF W-OTHER-2 > ZEROS
02871           MOVE 'Q'                  TO AT-ADDRESEE-TYPE
02872           MOVE SS50D                TO AT-ADDRESSEE-NAME
02873      ELSE
02874           MOVE SPACES               TO AT-ADDRESEE-TYPE
02875                                        AT-ADDRESSEE-NAME.
02876      IF PRINT-PERFORMED
02877         MOVE CURRENT-SAVE        TO AT-INITIAL-PRINT-DATE
02878      ELSE
02879         MOVE LOW-VALUES          TO AT-INITIAL-PRINT-DATE.
02880
02881      MOVE LOW-VALUES             TO AT-RESEND-PRINT-DATE.
02882
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE CL-CERT-STATE          TO WS-DMD-CERT-STATE
121802*        MOVE CL-CERT-GROUPING (5:2) TO WS-DMD-CERT-GROUPING
121802*        MOVE BEN-HOLD               TO WS-DMD-BEN-CODE
121802*        MOVE AT-SEQUENCE-NO         TO WS-DMD-CORR-TRLR-SEQ
121802*        MOVE AT-STD-LETTER-FORM     TO WS-DMD-LETTER-FORM.
02889
02890      
      * EXEC CICS WRITE
02891 *         DATASET    (ACTV-ID)
02892 *         FROM       (ACTIVITY-TRAILERS)
02893 *         RIDFLD     (AT-CONTROL-PRIMARY)
02894 *    END-EXEC.
           MOVE LENGTH OF
            ACTIVITY-TRAILERS
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008812' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTIVITY-TRAILERS, 
                 DFHEIV11, 
                 AT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02895
02896      
      * EXEC CICS HANDLE CONDITION
02897 *        DUPKEY      (6599-EXIT)
02898 *    END-EXEC.
      *    MOVE '"$$                   ! + #00008818' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303038383138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02899
02900      
      * EXEC CICS REWRITE
02901 *         DATASET    (CLAM-ID)
02902 *         FROM       (CLAIM-MASTER)
02903 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00008822' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02904
02905  6599-EXIT.
02906       EXIT.
02907      EJECT
02908  6600-SET-ADDR-SEQ.
02909      IF ADDRL = ZEROS
02910         GO TO 6699-EXIT.
02911
02912      MOVE ADDRI                  TO PI-ADDR-TYPE
02913                                     WS-ADDR-TYPE-CD.
02914      IF WS-ADDR-SEQ NOT NUMERIC
02915         GO TO 6699-EXIT.
02916
02917      MOVE ZEROS                  TO PI-ADDR-SEQ.
02918
02919      IF WS-ADDR-TYPE = 'I'
02920         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02921      ELSE
02922      IF WS-ADDR-TYPE = 'A'
02923         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02924         ADD +20                  TO ACTV-SEQ
02925      ELSE
02926      IF WS-ADDR-TYPE = 'B'
02927         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02928         ADD +10                  TO ACTV-SEQ
02929      ELSE
02930      IF WS-ADDR-TYPE = 'P'
02931         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02932         ADD +30                  TO ACTV-SEQ
02933      ELSE
02934      IF WS-ADDR-TYPE = 'E'
02935         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02936         ADD +40                  TO ACTV-SEQ
02937      ELSE
02938      IF WS-ADDR-TYPE = 'O'
02939         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02940         ADD +50                  TO ACTV-SEQ
02941      ELSE
02942      IF WS-ADDR-TYPE = 'Q'
02943         MOVE WS-ADDR-SEQ-NUM     TO ACTV-SEQ
02944         ADD +60                  TO ACTV-SEQ.
02945
02946  6699-EXIT.
02947       EXIT.
033110
033110 6700-BUILD-NAPERSOFT.
033110
033110     
      * EXEC CICS GETMAIN
033110*         SET      (ADDRESS OF NAPERSOFT-FILE)
033110*         LENGTH   (NAPS-LENGTH)
033110*    END-EXEC.
      *    MOVE '," L                  $   #00008873' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303038383733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 NAPS-LENGTH, 
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
033110     MOVE ARCH-NUMBER           TO  NA-ARCHIVE-NO.
033110     MOVE FORMI                 TO  NA-LETTER-ID.
033110     MOVE PI-PROCESSOR-ID       TO  NA-PROCESSOR-ID.
033110     MOVE CURRENT-SAVE          TO  NA-CREATION-DT.
033110     IF PRINT-PERFORMED
033110         MOVE CURRENT-SAVE      TO  NA-INITIAL-PRINT-DT
033110     ELSE
033110         MOVE LOW-VALUES        TO  NA-INITIAL-PRINT-DT
033110     END-IF.
033110     MOVE FOLLOW-UP-SAVE        TO  NA-FOLLOW-UP-DT.
033110     MOVE RESEND-SAVE           TO  NA-RESEND-DT
040110     MOVE PI-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
033110     IF COPIESL NOT = ZEROS
033110         MOVE COPIESI           TO  NA-NO-OF-COPIES
033110     ELSE
033110         MOVE 1                 TO  NA-NO-OF-COPIES
033110     END-IF.
033110     IF PI-LETTER-ADDRESS-TYPE = ' 0'
033110         MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
033110     ELSE
033110         MOVE PI-LETTER-ADDRESS-TYPE TO NA-ADDRESS-TYPE
033110     END-IF.
033110     MOVE CORR-TRLR-SEQ         TO  NA-CORR-TRLR-SEQ.
040110     MOVE PI-ENCLOSURE-CD       TO  NA-ENCLOSURE-CD.
033110
033110     
      * EXEC CICS WRITE
033110*         DATASET    (NAPS-ID)
033110*         FROM       (NAPERSOFT-FILE)
033110*         RIDFLD     (NA-CONTROL-PRIMARY)
033110*    END-EXEC.
           MOVE LENGTH OF
            NAPERSOFT-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00008909' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 NAPS-ID, 
                 NAPERSOFT-FILE, 
                 DFHEIV11, 
                 NA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
033110
033110 6799-EXIT.
033110      EXIT.
02948
02949      EJECT
02950  7000-READ-ADDR.
02951      IF PI-RETURN-TO-PROGRAM NOT = PGM-EL150
02952         IF ADDRL = ZEROS
02953            GO TO 7099-EXIT
02954         ELSE
02955            MOVE ER-0374             TO EMI-ERROR
02956            MOVE -1                  TO ADDRL
02957            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02958            GO TO 7099-EXIT.
02959
02960      MOVE PI-CLAIM-NO            TO CLAM-CLAIM
02961                                     ACTV-CLAIM.
02962
02963      
      * EXEC CICS HANDLE CONDITION
02964 *         NOTOPEN    (8860-CLAM-NOT-OPEN)
02965 *         NOTFND     (7090-CLAIM-NOT-FOUND)
02966 *     END-EXEC.
      *    MOVE '"$JI                  ! , #00008932' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303038393332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02967
02968      
      * EXEC CICS READ
02969 *         DATASET    (CLAM-ID)
02970 *         SET        (ADDRESS OF CLAIM-MASTER)
02971 *         RIDFLD     (CLAM-KEY)
02972 *    END-EXEC.
      *    MOVE '&"S        E          (   #00008937' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303038393337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02973
121802*    IF PI-COMPANY-ID = 'DMD'
121802*            AND
121802*       NOT SYSTEM-MODIFY-CAP
121802*            AND
121802*        HIGHEST-PRIORITY
121802*        IF MAINTI = 'R'
121802*            MOVE ER-0909        TO EMI-ERROR
121802*            MOVE -1             TO ADDRL
121802*            MOVE AL-UABON       TO ADDRA
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            GO TO 8200-SEND-DATAONLY
121802*        ELSE
121802*            MOVE ER-0900        TO EMI-ERROR
121802*            MOVE -1             TO ADDRL
121802*            MOVE AL-UABON       TO ADDRA
121802*            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*            GO TO 8200-SEND-DATAONLY.
02991
02992      IF ADDRL = ZEROS
02993         GO TO 7099-EXIT.
02994
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        IF MAINTI = 'C'
121802*           IF CL-BENEFICIARY = '0000B0CA10' OR '0000B0CAN0' OR
121802*                               '0000B0CAS0' OR '0000B0CA50' OR
121802*                               '0000B0CAA0' OR '0000B0CAB0' OR
121802*                               '0000B0CAC0' OR '0000B0CAD0' OR
121802*                               '0000B0CO20' OR '0000B0MN10' OR
121802*                               '0000B0MN20' OR '0000B0MN30' OR
121802*                               '0000B0MN40' OR '0000B0MN50' OR
121802*                               '0000B0MN60' OR '0000B0MN70'
121802*           IF ADDRI(1:1) = 'B'
121802*              MOVE ER-8158     TO EMI-ERROR
121802*              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*              MOVE -1          TO MAINTL
121802*              GO TO 8200-SEND-DATAONLY.
121802*
121802*    IF PI-COMPANY-ID = 'DMD'
121802*      PERFORM 4000-SET-BSR THRU 4000-EXIT
121802*      IF PI-BSR-AUTOMATED
121802*        IF MAINTI = 'C'
121802*           MOVE ER-7843          TO EMI-ERROR
121802*           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
121802*           MOVE -1               TO MAINTL
121802*           GO TO 8200-SEND-DATAONLY.
03019
03020      PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT.
03021
03022      MOVE ADDRI TO WS-ADDR-TYPE-CD.
03023
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        GO TO 7040-READ-PRODUCER.
03026
03027  7010-READ-ACCT.
03028
03029      MOVE CL-CERT-GROUPING       TO ACCT-GROUPING.
03030      MOVE CL-CERT-STATE          TO ACCT-STATE.
03031      MOVE CL-CERT-ACCOUNT        TO ACCT-ACCOUNT.
03032      MOVE CL-CERT-EFF-DT         TO ACCT-EXP-DATE.
03033
03034      
      * EXEC CICS HANDLE CONDITION
03035 *         NOTOPEN    (8880-ACCT-NOT-OPEN)
03036 *         NOTFND     (7080-ACCT-NOT-FOUND)
03037 *    END-EXEC.
      *    MOVE '"$JI                  ! - #00009003' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303039303033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03038
03039      PERFORM 8000-STARTBR-ERACCT THRU 8000-STARTBR-EXIT.
03040
03041      MOVE ACCT-PARTIAL-KEY       TO ACCT-SAVE-KEY.
03042
03043  7020-READNEXT.
03044
03045      PERFORM 8000-READNEXT-ERACCT THRU 8000-READNEXT-EXIT.
03046
03047      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
03048         IF WS-SAVE-ACCT-RECORD = SPACES
03049             GO TO 7080-ACCT-NOT-FOUND
03050         ELSE
03051             MOVE 'Y'                 TO  WS-ACCT-READ-SW
03052             MOVE AM-CONTROL-PRIMARY  TO  ACCT-KEY
03053             MOVE WS-SAVE-ACCT-RECORD TO  ACCOUNT-MASTER
03054             GO TO 7030-CONTINUE-BUILD-ADDR.
03055
03056      IF AM-EXPIRATION-DT = HIGH-VALUES
03057          NEXT SENTENCE
03058      ELSE
03059          MOVE ACCOUNT-MASTER         TO  WS-SAVE-ACCT-RECORD
03060          GO TO 7020-READNEXT.
03061
03062      MOVE AM-CONTROL-PRIMARY TO ACCT-KEY.
03063      MOVE 'Y'                TO WS-ACCT-READ-SW.
03064
03065  7030-CONTINUE-BUILD-ADDR.
03066
03067      MOVE SPACES             TO  WS-SAVE-ACCT-RECORD.
03068
03069      IF WS-ADDR-TYPE-CD = 'A0'
03070         MOVE ZEROS               TO PI-ADDR-SEQ
03071      ELSE
03072         GO TO 7067-CHECK-BENE-ADDR.
03073
03074      MOVE SPACES             TO               WS-LABEL-HOLD-AREA.
03075      MOVE AM-NAME            TO  REC-TEXT (1) WS-LABEL-LINES (1).
03076      MOVE AM-PERSON          TO  REC-TEXT (2) WS-LABEL-LINES (2).
03077      MOVE AM-ADDRS           TO  REC-TEXT (3) WS-LABEL-LINES (3).
03078 *    MOVE AM-CITY            TO  REC-TEXT (4) WS-LABEL-LINES (4).
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
           MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)
03079      MOVE SPACES             TO  REC-TEXT (5) WS-LABEL-LINES (5).
03080
03081      MOVE SPACES             TO  WS-ZIP-CODE.
03082
03083      IF AM-CANADIAN-POST-CODE
03084          MOVE AM-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
03085          MOVE AM-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
03086      ELSE
03087          MOVE AM-ZIP-PRIME       TO  WS-AM-ZIP-CODE
03088          IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
03089              MOVE '-'            TO  WS-AM-ZIP-DASH
03090              MOVE AM-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
03091
03092      MOVE WS-ZIP-CODE        TO  REC-TEXT (6) WS-LABEL-LINES (6).
03093
03094      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
03095
03096      MOVE WS-LABEL-LINES (1) TO  REC-TEXT (1)    SS05-1D.
03097      MOVE WS-LABEL-LINES (2) TO  REC-TEXT (2)    SS05-2D.
03098      MOVE WS-LABEL-LINES (3) TO  REC-TEXT (3)    SS05-3D.
03099      MOVE WS-LABEL-LINES (4) TO  REC-TEXT (4)    SS05-4D.
03100      MOVE WS-LABEL-LINES (5) TO  REC-TEXT (5)    SS05-5D.
03101      MOVE WS-LABEL-LINES (6) TO  REC-TEXT (6)    SS05-6D.
03102
03103      GO TO 7099-EXIT.
03104
03105      EJECT
121802*7040-READ-PRODUCER. Remove as dead code
121802*7050-READNEXT. Remove as dead code
121802*7060-CONTINUE-BUILD-ADDR. Remove as dead code
03181  7067-CHECK-BENE-ADDR.
03182
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        IF WS-ADDR-TYPE-CD = 'B9'
121802*            GO TO 7067-CHECK-BENE-ADDR-9.
03186
03187      IF WS-ADDR-TYPE-CD NOT = 'B0'
03188          GO TO 7068-CONTINUE-BUILD-ADDR.
03189
03190      IF CL-BENEFICIARY = SPACES
03191          GO TO 7070-ACTV-NOT-FOUND.
03192
03193      
      * EXEC CICS HANDLE CONDITION
03194 *         NOTFND     (7070-ACTV-NOT-FOUND)
03195 *    END-EXEC.
      *    MOVE '"$I                   ! . #00009094' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303039303934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03196
03197      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
03198      MOVE 'B'                    TO BENE-REC-TYPE.
03199      MOVE CL-BENEFICIARY         TO BENE-NUMBER.
03200
03201      
      * EXEC CICS READ
03202 *         DATASET    (BENE-ID)
03203 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
03204 *         RIDFLD     (BENE-KEY)
03205 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009102' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313032' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03206
03207      MOVE SPACES              TO  WS-LABEL-HOLD-AREA.
03208      MOVE BE-MAIL-TO-NAME     TO  REC-TEXT (1) WS-LABEL-LINES (1).
03209      MOVE BE-ADDRESS-LINE-1   TO  REC-TEXT (2) WS-LABEL-LINES (2).
03210      MOVE BE-ADDRESS-LINE-2   TO  REC-TEXT (3) WS-LABEL-LINES (3).
03211      MOVE BE-ADDRESS-LINE-3   TO  REC-TEXT (4) WS-LABEL-LINES (4).
03212 *    MOVE BE-CITY-STATE       TO  REC-TEXT (5) WS-LABEL-LINES (5).
           STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
           MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)
03213
03214      MOVE SPACES              TO  WS-ZIP-CODE.
03215      IF BE-CANADIAN-POST-CODE
03216          MOVE BE-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
03217          MOVE BE-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
03218      ELSE
03219          MOVE BE-ZIP-PRIME       TO  WS-AM-ZIP-CODE
03220          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
03221              MOVE '-'            TO  WS-AM-ZIP-DASH
03222              MOVE BE-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
03223
03224      MOVE WS-ZIP-CODE         TO  REC-TEXT (6) WS-LABEL-LINES (6).
03225
03226      GO TO 7069-SET-ADDR.
03227
03228  7067-CHECK-BENE-ADDR-9.
03229
03230      IF CL-BENEFICIARY = SPACES
03231          GO TO 7070-ACTV-NOT-FOUND.
03232
03233      
      * EXEC CICS HANDLE CONDITION
03234 *         NOTFND     (7070-ACTV-NOT-FOUND)
03235 *    END-EXEC.
      *    MOVE '"$I                   ! / #00009138' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303039313338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03236
03237      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
03238      MOVE 'B'                    TO BENE-REC-TYPE.
03239      MOVE CL-BENEFICIARY         TO BENE-NUMBER.
03240
03241      
      * EXEC CICS READ
03242 *         DATASET    (BENE-ID)
03243 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
03244 *         RIDFLD     (BENE-KEY)
03245 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009146' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03246
03247      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
03248      MOVE BE-MAIL-TO-NAME2       TO REC-TEXT (1)
03249                                     WS-LABEL-LINES (1).
03250      MOVE BE-ADDRESS-LINE-12     TO REC-TEXT (2)
03251                                     WS-LABEL-LINES (2).
03252      MOVE BE-ADDRESS-LINE-22     TO REC-TEXT (3)
03253                                     WS-LABEL-LINES (3).
03254      MOVE BE-ADDRESS-LINE-32     TO REC-TEXT (4)
03255                                     WS-LABEL-LINES (4).
03256 *    MOVE BE-CITY-STATE2         TO REC-TEXT (5)
03257 *                                   WS-LABEL-LINES (5).
           STRING BE-CITY2 ' ' BE-STATE2 DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
           MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)
03258
03259      MOVE SPACES                 TO WS-ZIP-CODE.
03260
03261      IF BE-CANADIAN-POST-CODE2
03262          MOVE BE-CAN-POSTAL-12   TO WS-CAN-POSTAL-1
03263          MOVE BE-CAN-POSTAL-22   TO WS-CAN-POSTAL-2
03264      ELSE
03265          MOVE BE-ZIP-PRIME2      TO WS-AM-ZIP-CODE
03266          IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS
03267              MOVE '-'            TO WS-AM-ZIP-DASH
03268              MOVE BE-ZIP-PLUS42  TO WS-AM-ZIP-PLUS4.
03269
03270      MOVE WS-ZIP-CODE            TO REC-TEXT (6)
03271                                     WS-LABEL-LINES (6).
03272
03273      GO TO 7069-SET-ADDR.
03274
03275  7068-CONTINUE-BUILD-ADDR.
03276
03277      IF ACTV-SEQ = ZEROS
03278         GO TO 7070-ACTV-NOT-FOUND.
03279
03280      MOVE ACTV-SEQ               TO PI-ADDR-SEQ.
03281
03282      
      * EXEC CICS HANDLE CONDITION
03283 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
03284 *         NOTFND     (7070-ACTV-NOT-FOUND)
03285 *    END-EXEC.
      *    MOVE '"$JI                  ! 0 #00009191' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303039313931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03286
03287      
      * EXEC CICS READ
03288 *         DATASET    (ACTV-ID)
03289 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
03290 *         RIDFLD     (ACTV-KEY)
03291 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009196' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03292
03293      MOVE SPACES            TO              WS-LABEL-HOLD-AREA.
03294      MOVE AT-MAIL-TO-NAME   TO REC-TEXT (1) WS-LABEL-LINES (1).
03295      MOVE AT-ADDRESS-LINE-1 TO REC-TEXT (2) WS-LABEL-LINES (2).
03296      MOVE AT-ADDRESS-LINE-2 TO REC-TEXT (3) WS-LABEL-LINES (3).
03297 *    MOVE AT-CITY-STATE     TO REC-TEXT (4) WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
           MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)
03298
03299      MOVE SPACES            TO  WS-ZIP-CODE.
03300      IF AT-CANADIAN-POST-CODE
03301          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
03302          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
03303      ELSE
03304          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
03305          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
03306              MOVE '-'            TO  WS-AM-ZIP-DASH
03307              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
03308
03309      MOVE WS-ZIP-CODE       TO  REC-TEXT (5) WS-LABEL-LINES (5).
03310
03311      MOVE SPACES            TO  REC-TEXT (6).
03312
03313  7069-SET-ADDR.
03314      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
03315
03316      MOVE WS-LABEL-LINES (1) TO  REC-TEXT (1)    SS05-1D.
03317      MOVE WS-LABEL-LINES (2) TO  REC-TEXT (2)    SS05-2D.
03318      MOVE WS-LABEL-LINES (3) TO  REC-TEXT (3)    SS05-3D.
03319      MOVE WS-LABEL-LINES (4) TO  REC-TEXT (4)    SS05-4D.
03320      MOVE WS-LABEL-LINES (5) TO  REC-TEXT (5)    SS05-5D.
03321      MOVE SPACES             TO  REC-TEXT (6)    SS05-6D.
03322
03323      GO TO 7099-EXIT.
03324
03325  7070-ACTV-NOT-FOUND.
03326
03327      IF ACTV-SEQ NOT = +29
03328         GO TO 7075-CONTINUE-ACTV-ERROR.
03329
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        GO TO 7099-EXIT.
03332
03333      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
03334         MOVE ZEROS         TO AM-3RD-PARTY-NOTIF-LEVEL
03335         GO TO 7075-CONTINUE-ACTV-ERROR.
03336
03337      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = SPACES OR ZEROS
03338         GO TO 7075-CONTINUE-ACTV-ERROR.
03339
03340      MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD
03341      MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER
03342      MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING
03343      MOVE 'A'             TO WS-ERCOMP-TYPE
03344      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
03345                           TO WS-ERCOMP-RESP-NO.
03346
03347      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
03348          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
052814                                       'G' OR 'B' or 'S'
03350              MOVE 'G'            TO WS-ERCOMP-TYPE
03351              MOVE LOW-VALUES     TO WS-ERCOMP-ACCOUNT
03352          ELSE
03353              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
03354                                  TO WS-ERCOMP-ACCOUNT
03355      ELSE
03356          MOVE 'G'                TO WS-ERCOMP-TYPE
03357          MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.
03358
03359      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
03360         MOVE ZEROS TO WS-ERCOMP-CARRIER.
03361
03362      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
03363         MOVE ZEROS TO WS-ERCOMP-GROUPING.
03364
03365      
      * EXEC CICS HANDLE CONDITION
03366 *         NOTFND    (7075-CONTINUE-ACTV-ERROR)
03367 *    END-EXEC.
      *    MOVE '"$I                   ! 1 #00009278' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303039323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03368
03369      
      * EXEC CICS  READ
03370 *         SET      (ADDRESS OF COMPENSATION-MASTER)
03371 *         DATASET  ('ERCOMP')
03372 *         RIDFLD   (WS-ERCOMP-KEY)
03373 *    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00009282' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039323832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03374
03375      MOVE 'Y'                   TO WS-COMP-READ-SW.
03376      MOVE SPACES                TO WS-LABEL-HOLD-AREA.
03377      MOVE CO-ACCT-NAME          TO WS-LABEL-LINES (1).
03378
03379      IF CO-ACCT-NAME = SPACES
03380         MOVE CO-MAIL-NAME       TO WS-LABEL-LINES (1).
03381
03382      MOVE CO-ADDR-1             TO WS-LABEL-LINES (2).
03383      MOVE CO-ADDR-2             TO WS-LABEL-LINES (3).
03384      MOVE CO-ADDR-3             TO WS-LABEL-LINES (4).
03385
03386      MOVE SPACES                TO  WS-ZIP-CODE.
03387      IF CO-CANADIAN-POST-CODE
03388          MOVE CO-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
03389          MOVE CO-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
03390      ELSE
03391          MOVE CO-ZIP-PRIME      TO  WS-AM-ZIP-CODE
03392          IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
03393              MOVE '-'           TO  WS-AM-ZIP-DASH
03394              MOVE CO-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
03395
03396      MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).
03397
03398      MOVE ZEROS                 TO WS-PHONE-IN.
03399      MOVE CO-AREA-CODE          TO WSPO-AREA.
03400      MOVE CO-PREFIX             TO WSPO-PFX.
03401      MOVE CO-PHONE              TO WSPO-SFX.
03402      MOVE WS-PHONE-OUT          TO SS53-6D.
03403
03404      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
03405
03406      MOVE WS-LABEL-LINES (1)     TO REC-TEXT (1)    SS52D.
03407      MOVE WS-LABEL-LINES (2)     TO REC-TEXT (2)    SS53-1D.
03408      MOVE WS-LABEL-LINES (3)     TO REC-TEXT (3)    SS53-2D.
03409      MOVE WS-LABEL-LINES (4)     TO REC-TEXT (4)    SS53-3D.
03410      MOVE WS-LABEL-LINES (5)     TO REC-TEXT (5)    SS53-4D.
03411      MOVE WS-LABEL-LINES (6)     TO REC-TEXT (6)    SS53-5D.
03412
03413      GO TO 7099-EXIT.
03414      EJECT
03415
03416  7075-CONTINUE-ACTV-ERROR.
03417      MOVE ER-0178                TO EMI-ERROR.
03418      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03419      MOVE -1                     TO ADDRL.
03420      MOVE AL-UABON               TO ADDRA.
03421      MOVE SPACES                 TO PI-ADDR-TYPE.
03422      MOVE SPACES                 TO REC-TEXT (1)
03423                                     REC-TEXT (2)
03424                                     REC-TEXT (3)
03425                                     REC-TEXT (4)
03426                                     REC-TEXT (5)
03427                                     REC-TEXT (6).
03428      GO TO 7099-EXIT.
03429
03430  7080-ACCT-NOT-FOUND.
03431      MOVE ER-0179                TO EMI-ERROR.
03432      MOVE -1                     TO ADDRL.
03433      MOVE AL-UABON               TO ADDRA.
03434      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03435      GO TO 8200-SEND-DATAONLY.
03436
03437  7090-CLAIM-NOT-FOUND.
03438      MOVE ER-0186                TO EMI-ERROR.
03439      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03440      GO TO 8200-SEND-DATAONLY.
03441
121802*7095-PROD-NOT-FOUND. Remove as dead code
03448
03449  7099-EXIT.
03450       EXIT.
03451      EJECT
03452
03453  7170-FORMAT-SCREEN.
03454      IF MAINTI = 'S'
03455         MOVE AL-PANOF            TO SC-TEXTA (SC-INDX)
03456        ELSE
03457         MOVE AL-UANOF            TO SC-TEXTA (SC-INDX).
03458
03459      IF TB-INDX < 7
03460         SET LINE23 TO TB-INDX
03461         MOVE 'A'                 TO LINE1
03462        ELSE
03463         IF TB-INDX = 7
03464            MOVE ZEROS            TO LINE-NUM
03465         ELSE
03466            SET LIN-NUM TO TB-INDX
03467            SUBTRACT 7            FROM LIN-NUM.
03468
03469      MOVE LINE-NUM               TO SC-LINE (SC-INDX).
03470      MOVE REC-TEXT (TB-INDX)     TO SC-TEXT (SC-INDX).
03471      SET TB-INDX UP BY 1.
03472
03473  7170-EXIT.
03474       EXIT.
03475
03476      EJECT
03477  7200-RESOLVE-VARIABLES.
03478 ***************************************************************
03479 *    THIS ROUTINE WILL FORMAT THE SYSTEM-DEFINED SYMBOLS      *
03480 *    WITH DATA PERTAINING TO THE DESIGNATED CLAIM.            *
03481 *    THIS ROUTINE IS PERFORMED THRU 7269-EXIT TO              *
03482 *    RESOLVE ALL OF THE SYMBOLS.                              *
03483 ***************************************************************
03484
121802*    IF PI-COMPANY-ID = 'AUK'
121802*        MOVE 'Y'                TO W-REVERSE-DATE-SW.
03487
03488      MOVE PI-CLAIM-NO            TO SS34D.
03489
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03491         MOVE PI-LIFE-OVERRIDE-L6 TO SS12D
03492      ELSE
03493         MOVE PI-AH-OVERRIDE-L6   TO SS12D.
03494
03495      MOVE CL-LAST-PMT-AMT        TO SS16D.
03496      MOVE CL-TOTAL-PAID-AMT      TO SS18D.
03497      MOVE CL-CAUSE-CD            TO SS19-1D.
03498      MOVE CL-ASSOC-CERT-SEQU     TO SS54D.
03499      MOVE CL-ASSOC-CERT-TOTAL    TO SS55D.
03500
121802*    IF PI-COMPANY-ID NOT = 'DMD'
121802*        GO TO 7200-CONTINUE.
03503
03504      IF CL-CCN-PREFIX-A5 = '1111' OR '3333' OR '6666' OR
03505                            '8888' OR '9876'
03506          MOVE CL-CCN-PRIME-A5    TO WS-CCN-12
03507          MOVE WS-CCN-12          TO CL-CCN.
03508
03509      IF CL-CERT-STATE NOT = '08' AND '09'
03510          MOVE SPACES             TO SS62D
03511                                     SS63D
03512          GO TO 7200-CONTINUE.
03513
03514      MOVE WS-DMD-UND-STATEMENT   TO SS62D.
03515
03516      MOVE 'CL'                   TO DL23-SYSTEM-ID.
03517      MOVE 'UN'                   TO DL23-RECORD-TYPE.
03518      MOVE CL-CERT-STATE          TO DL23-RECORD-KEY.
03519
03520      
      * EXEC CICS LINK
03521 *        PROGRAM    ('DLO023')
03522 *        COMMAREA   (WS-DLO-CODES-TABLE)
03523 *        LENGTH     (DL23-COMM-LENGTH)
03524 *    END-EXEC.
           MOVE 'DLO023' TO DFHEIV1
      *    MOVE '."C                   (   #00009428' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039343238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 WS-DLO-CODES-TABLE, 
                 DL23-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03525
03526      IF DL23-RETURN-CODE = 'OK'
03527          MOVE DL23-CODE-DESC     TO SS63D
03528        ELSE
03529          MOVE SPACES             TO SS63D.
03530
03531  7200-CONTINUE.
03532      MOVE CL-CCN                 TO SS60D.
03533
03534      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.
03535      MOVE SPACES                 TO DC-OPTION-CODE.
03536      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03537      IF NO-CONVERSION-ERROR
03538         MOVE DC-GREG-DATE-1-EDIT TO SS13D.
03539
03540      MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.
03541      MOVE SPACES                 TO DC-OPTION-CODE.
03542      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03543      IF NO-CONVERSION-ERROR
03544         MOVE DC-GREG-DATE-1-EDIT TO SS14D.
03545
03546      MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.
03547      MOVE SPACES                 TO DC-OPTION-CODE.
03548      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03549      IF NO-CONVERSION-ERROR
03550         MOVE DC-GREG-DATE-1-EDIT TO SS15D.
03551
03552      IF NOT PI-USES-PAID-TO
03553         MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
03554         MOVE SPACES                 TO DC-OPTION-CODE
03555         PERFORM 9700-DATE-LINK THRU 9700-EXIT
03556         IF NO-CONVERSION-ERROR
03557            MOVE DC-GREG-DATE-1-EDIT TO SS17D
03558         ELSE
03559            MOVE SPACES TO SS17D
03560      ELSE
03561         MOVE CL-PAID-THRU-DT        TO DC-BIN-DATE-1
03562         MOVE '6'                    TO DC-OPTION-CODE
03563         MOVE +1                     TO DC-ELAPSED-DAYS
03564         MOVE +0                     TO DC-ELAPSED-MONTHS
03565         PERFORM 9700-DATE-LINK THRU 9700-EXIT
03566         IF NO-CONVERSION-ERROR
03567            MOVE DC-GREG-DATE-1-EDIT TO SS17D.
03568
03569      MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.
03570      MOVE SPACES                 TO DC-OPTION-CODE.
03571      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03572      IF NO-CONVERSION-ERROR
03573         MOVE DC-GREG-DATE-1-EDIT TO SS45D
03574      ELSE
03575         MOVE '@@DOB'             TO SS45D.
03576
03577      IF CL-SSN-STATE   = CL-CERT-STATE  AND
03578         CL-SSN-ACCOUNT = CL-CERT-ACCOUNT-PRIME
03579              NEXT SENTENCE
03580          ELSE
03581              MOVE CL-SOC-SEC-NO  TO SS46D.
03582
03583      MOVE CL-CERT-GROUPING       TO CERT-GROUPING
03584                                     ACCT-GROUPING
03585                                     PROD-GROUPING
03586                                     PLCY-GROUPING
03587                                     PLAN-GROUPING.
03588      MOVE CL-CERT-STATE          TO CERT-STATE
03589                                     ACCT-STATE
03590                                     PROD-STATE
03591                                     PLCY-STATE
03592                                     PLAN-STATE.
03593      MOVE CL-CERT-ACCOUNT        TO CERT-ACCOUNT
03594                                     ACCT-ACCOUNT
03595                                     PROD-PRODUCER
03596                                     PLCY-PRODUCER
03597                                     PLAN-PRODUCER.
03598      MOVE CL-CERT-EFF-DT         TO CERT-EFF-DT
03599                                     PLCY-EFF-DT.
03600      MOVE CL-CV-REFERENCE-NO     TO PLCY-REFERENCE-NO.
03601
03602      PERFORM 5000-MOVE-NAME THRU 5000-EXIT.
03603
121802*    IF PI-COMPANY-ID = 'DMD'
121802*       MOVE WS-NAME-WORK           TO SS10D
121802*       GO TO 7200-CONTINUE-2.
03607
03608      IF LOWER-CASE-LETTERS-USED
03609         MOVE 'N'                    TO WS-STATE-LINE
03610         MOVE WS-NAME-WORK           TO WS-TEMP-AREA2
03611         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
03612         MOVE WS-TEMP-AREA2          TO SS10D
03613      ELSE
03614         MOVE WS-NAME-WORK           TO SS10D.
03615
03616  7200-CONTINUE-2.
03617      MOVE CL-INSURED-1ST-NAME     TO W-FIRST-NAME.
03618      MOVE CL-INSURED-LAST-NAME    TO W-LAST-NAME.
03619      MOVE CL-INSURED-MID-INIT     TO W-MIDDLE-NAME.
03620      PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
03621
03622      IF LOWER-CASE-LETTERS-USED
03623         MOVE 'N'                    TO WS-STATE-LINE
03624         MOVE WS-NAME-WORK           TO WS-TEMP-AREA2
03625         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
03626         MOVE WS-TEMP-AREA2          TO SS39D
03627      ELSE
03628         MOVE WS-NAME-WORK           TO SS39D.
03629
03630      MOVE CL-INSURED-LAST-NAME   TO SS40D.
03631
03632      IF INSURED-IS-FEMALE
03633         MOVE 'MS.'               TO SS41D
03634        ELSE
03635         MOVE 'MR.'               TO SS41D.
03636
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        PERFORM 7290-RESOLVE-CREDITOR THRU 7290-EXIT.
03639
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        GO TO 7200-READ-EMPLCY.
03642      EJECT
03643      
      * EXEC CICS HANDLE CONDITION
03644 *         NOTOPEN   (8900-CERT-NOT-OPEN)
03645 *         NOTFND    (8910-CERT-NOT-FOUND)
03646 *    END-EXEC.
      *    MOVE '"$JI                  ! 2 #00009551' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303039353531' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03647
03648      
      * EXEC CICS READ
03649 *         DATASET   (CERT-ID)
03650 *         SET       (ADDRESS OF CERTIFICATE-MASTER)
03651 *         RIDFLD    (CERT-KEY)
03652 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009556' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039353536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CERT-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03653
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE CM-RESIDENT-STATE  TO WS-DMD-RES-ST
121802*        IF CM-POLICY-FORM-NO(6:1) = '1'
121802*            MOVE WS-DMD-UND-COMPANYA    TO SS64D
121802*          ELSE
121802*        IF CM-POLICY-FORM-NO(6:1) = '2'
121802*            MOVE WS-DMD-UND-COMPANYB    TO SS64D
121802*          ELSE
121802*            MOVE SPACES                 TO SS64D.
03663
03664      MOVE CM-CERT-NO             TO SS26D.
03665      MOVE CM-INSURED-ISSUE-AGE   TO SS33D.
03666
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        MOVE CL-CURRENT-CARRIER TO SS23D
121802*        MOVE CL-CURRENT-GROUPING
121802*                                TO SS24D
121802*        MOVE CL-CURRENT-ACCOUNT TO SS25D
121802*        MOVE CM-MEMBER-NO       TO SS36D
121802*        MOVE CM-GROUPING        TO W-GROUPING
121802*        IF W-GROUP-3 = 'C01' OR 'C02'
121802*            MOVE CM-MEMBER-NO   TO W-CURRENT-LOAN-NO
121802*            MOVE CM-LOAN-NUMBER TO W-LOAN-NO
121802*            MOVE W-CREDIT-CARD-LOAN-NO
121802*                                TO SS36-1D
121802*        ELSE
121802*            MOVE CM-MEMBER-NO   TO SS36-1D
121802*    ELSE
03682          MOVE CM-CARRIER         TO SS23D.
03683          MOVE CM-GROUPING        TO SS24D.
03684          MOVE CM-ACCOUNT         TO SS25D.
03685          MOVE CM-LOAN-NUMBER     TO SS36D.
03686
03687      MOVE CM-LOAN-BALANCE        TO SS37D.
03688      MOVE CM-MEMBER-NO           TO SS38D.
03689
03690      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
03691      MOVE SPACES                 TO DC-OPTION-CODE.
03692      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03693      IF NO-CONVERSION-ERROR
03694         MOVE DC-GREG-DATE-1-EDIT TO SS27D.
03695
03696      MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME.
03697      MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME.
03698      MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME.
03699      PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
03700
03701      IF LOWER-CASE-LETTERS-USED
03702          MOVE 'N'                TO WS-STATE-LINE
03703          MOVE WS-NAME-WORK       TO WS-TEMP-AREA2
03704          PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
03705          MOVE WS-TEMP-AREA2      TO SS57D
03706      ELSE
03707          MOVE WS-NAME-WORK       TO SS57D.
03708
03709      MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME.
03710      MOVE CM-JT-LAST-NAME        TO W-LAST-NAME.
03711      MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME.
03712      PERFORM 5200-MOVE-NAME THRU 5200-EXIT.
03713
03714      IF LOWER-CASE-LETTERS-USED
03715          MOVE 'N'                TO WS-STATE-LINE
03716          MOVE WS-NAME-WORK       TO WS-TEMP-AREA2
03717          PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
03718          MOVE WS-TEMP-AREA2      TO SS58D
03719      ELSE
03720          MOVE WS-NAME-WORK       TO SS58D.
03721
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03723         MOVE CM-LF-BENEFIT-CD    TO BEN-HOLD
03724         MOVE CM-LF-ORIG-TERM     TO SS29D
03725         MOVE CM-LF-BENEFIT-AMT   TO SS30D
03726         MOVE CM-POLICY-FORM-NO   TO SS32D
03727         MOVE CM-LF-CANCEL-DT     TO DC-BIN-DATE-1
03728      ELSE
03729         MOVE CM-AH-BENEFIT-CD    TO BEN-HOLD
03730         MOVE CM-AH-ORIG-TERM     TO SS29D
03731         MOVE CM-AH-BENEFIT-AMT   TO SS30D
03732         MOVE CM-POLICY-FORM-NO   TO SS32D
03733         MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1.
03734
03735      COMPUTE WORK-AMOUNT = CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT.
03736
03737      MOVE WORK-AMOUNT            TO SS51D.
03738
03739      MOVE SPACES                 TO DC-OPTION-CODE.
03740      PERFORM 9700-DATE-LINK THRU 9700-EXIT.
03741      IF NO-CONVERSION-ERROR
03742         MOVE DC-GREG-DATE-1-EDIT TO SS31D.
03743
03744      MOVE ' '                         TO  DC-OPTION-CODE.
03745      MOVE +0                          TO  DC-ELAPSED-MONTHS
03746                                           DC-ELAPSED-DAYS.
03747
100518     IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03749          MOVE CM-LF-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
03750          PERFORM 9700-DATE-LINK THRU 9700-EXIT
03751          IF NO-CONVERSION-ERROR
03752              MOVE DC-GREG-DATE-1-EDIT TO  SS28D
03753          ELSE
03754              NEXT SENTENCE
03755      ELSE
03756          MOVE CM-AH-LOAN-EXPIRE-DT    TO  DC-BIN-DATE-1
03757          PERFORM 9700-DATE-LINK THRU 9700-EXIT
03758          IF NO-CONVERSION-ERROR
03759              MOVE DC-GREG-DATE-1-EDIT TO  SS28D.
03760
121802*    GO TO 7200-READ-DIAGNOSIS.
03762
03763      EJECT
121802*7200-READ-EMPLCY. Remove as dead code
03844  7200-READ-DIAGNOSIS.
03845      
      * EXEC CICS HANDLE CONDITION
03846 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
PEMMOD*         NOTFND     (7200-READ-LOAN-NUMBER)
03848 *    END-EXEC.
      *    MOVE '"$JI                  ! 3 #00009674' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303039363734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03849
03850      MOVE CL-CONTROL-PRIMARY    TO ACTV-KEY.
03851      MOVE +90                   TO ACTV-SEQ.
03852
03853      
      * EXEC CICS READ
03854 *         DATASET  (ACTV-ID)
03855 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
03856 *         RIDFLD   (ACTV-KEY)
03857 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009682' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03858
03859      IF AT-TRAILER-TYPE = '6'
03860         MOVE AT-INFO-LINE-1         TO SS19D.
03861
PEMMOD 7200-READ-LOAN-NUMBER.
PEMMOD
PEMMOD     
      * EXEC CICS HANDLE CONDITION
PEMMOD*         NOTOPEN    (8870-ACTV-NOT-OPEN)
PEMMOD*         NOTFND     (7201-READ-BENEFICIARY)
PEMMOD*    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00009693' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303039363933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
PEMMOD
PEMMOD     MOVE CL-CONTROL-PRIMARY    TO ACTV-KEY.
PEMMOD     MOVE +91                   TO ACTV-SEQ.
PEMMOD
PEMMOD     
      * EXEC CICS READ
PEMMOD*         DATASET  (ACTV-ID)
PEMMOD*         SET      (ADDRESS OF ACTIVITY-TRAILERS)
PEMMOD*         RIDFLD   (ACTV-KEY)
PEMMOD*    END-EXEC.
      *    MOVE '&"S        E          (   #00009701' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373031' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
PEMMOD
PEMMOD     IF AT-TRAILER-TYPE = '6'
PEMMOD        MOVE AT-INFO-LINE-1         TO SS19-2D.
PEMMOD
03862  7201-READ-BENEFICIARY.
03863      IF CL-BENIF-ADDR-CNT = +0  AND
03864         CL-BENEFICIARY = SPACES
03865           GO TO 7205-READ-PHYSICIAN-ADDR.
03866
03867      IF W-BENEFICIARY > ZERO
03868          COMPUTE ACTV-SEQ = W-BENEFICIARY + 10
03869      ELSE
03870          MOVE CL-BENIF-ADDR-CNT  TO ACTV-SEQ
03871          ADD +10                 TO ACTV-SEQ.
03872
03873      
      * EXEC CICS HANDLE CONDITION
03874 *         NOTOPEN    (7290-BENE-NOT-OPEN)
03875 *         NOTFND     (7205-READ-PHYSICIAN-ADDR)
03876 *    END-EXEC.
      *    MOVE '"$JI                  ! 5 #00009721' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303039373231' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03877
03878      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
03879
03880      IF ACTV-SEQ = +19
03881          NEXT SENTENCE
03882      ELSE
03883          IF ACTV-SEQ NOT = +10
03884              GO TO 7202-GET-FROM-ACTIVITY.
03885
03886      MOVE PI-COMPANY-CD        TO BENE-COMP-CD.
03887      MOVE 'B'                  TO BENE-REC-TYPE.
03888      MOVE CL-BENEFICIARY       TO BENE-NUMBER.
03889
03890      
      * EXEC CICS READ
03891 *         DATASET    (BENE-ID)
03892 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
03893 *         RIDFLD     (BENE-KEY)
03894 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009738' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03895
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE BE-BSR             TO PI-ELBENE-BSR.
03898
03899  7201-CONT.
03900      MOVE BE-MAIL-TO-NAME2       TO WS-LABEL-LINES (1).
03901      MOVE BE-ADDRESS-LINE-12     TO WS-LABEL-LINES (2).
03902      MOVE BE-ADDRESS-LINE-22     TO WS-LABEL-LINES (3).
03903      MOVE BE-ADDRESS-LINE-32     TO WS-LABEL-LINES (4).
03904 *    MOVE BE-CITY-STATE2         TO WS-LABEL-LINES (5).
           STRING BE-CITY2 ' ' BE-STATE2 DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
03905
03906      MOVE SPACES                 TO WS-ZIP-CODE.
03907
03908      IF BE-CANADIAN-POST-CODE2
03909          MOVE BE-CAN-POSTAL-12   TO WS-CAN-POSTAL-1
03910          MOVE BE-CAN-POSTAL-22   TO WS-CAN-POSTAL-2
03911      ELSE
03912          MOVE BE-ZIP-PRIME2      TO WS-AM-ZIP-CODE
03913          IF BE-ZIP-PLUS42 NOT = SPACES AND ZEROS
03914              MOVE '-'            TO WS-AM-ZIP-DASH
03915              MOVE BE-ZIP-PLUS42  TO WS-AM-ZIP-PLUS4.
03916
03917      MOVE WS-ZIP-CODE            TO WS-LABEL-LINES (6).
03918
03919      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
03920
03921      MOVE WS-LABEL-LINES (1)     TO SS61D.
03922      MOVE WS-LABEL-LINES (2)     TO SS61-1D.
03923      MOVE WS-LABEL-LINES (3)     TO SS61-2D.
03924      MOVE WS-LABEL-LINES (4)     TO SS61-3D.
03925      MOVE WS-LABEL-LINES (5)     TO SS61-4D.
03926      MOVE WS-LABEL-LINES (6)     TO SS61-5D.
03927
03928      MOVE BE-PHONE-NO2           TO WS-PHONE-IN.
03929      MOVE WSPI-AREA              TO WSPO-AREA.
03930      MOVE WSPI-PFX               TO WSPO-PFX.
03931      MOVE WSPI-SFX               TO WSPO-SFX.
03932      MOVE WS-PHONE-OUT           TO SS61-6D.
03933
03934  7201-CONT-PROCESS.
03935      MOVE BE-MAIL-TO-NAME      TO  WS-LABEL-LINES (1).
03936      MOVE BE-ADDRESS-LINE-1    TO  WS-LABEL-LINES (2).
03937      MOVE BE-ADDRESS-LINE-2    TO  WS-LABEL-LINES (3).
03938      MOVE BE-ADDRESS-LINE-3    TO  WS-LABEL-LINES (4).
03939 *    MOVE BE-CITY-STATE        TO  WS-LABEL-LINES (5).
           STRING BE-CITY ' ' BE-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (5)
           END-STRING
03940
03941      MOVE SPACES               TO  WS-ZIP-CODE.
03942      IF BE-CANADIAN-POST-CODE
03943          MOVE BE-CAN-POSTAL-1  TO  WS-CAN-POSTAL-1
03944          MOVE BE-CAN-POSTAL-2  TO  WS-CAN-POSTAL-2
03945      ELSE
03946          MOVE BE-ZIP-PRIME     TO  WS-AM-ZIP-CODE
03947          IF BE-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
03948              MOVE '-'          TO  WS-AM-ZIP-DASH
03949              MOVE BE-ZIP-PLUS4 TO  WS-AM-ZIP-PLUS4.
03950
03951      MOVE WS-ZIP-CODE          TO  WS-LABEL-LINES (6).
03952
03953      MOVE BE-PHONE-NO          TO  WS-PHONE-IN.
03954
03955      GO TO 7204-SET-PHONE.
03956
03957  7202-GET-FROM-ACTIVITY.
03958      
      * EXEC CICS READ
03959 *         DATASET  (ACTV-ID)
03960 *         SET      (ADDRESS OF ACTIVITY-TRAILERS)
03961 *         RIDFLD   (ACTV-KEY)
03962 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009812' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383132' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03963
03964      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
03965      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
03966      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
03967 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
03968
03969      MOVE SPACES                 TO  WS-ZIP-CODE.
03970      IF AT-CANADIAN-POST-CODE
03971          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
03972          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
03973      ELSE
03974          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
03975          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
03976              MOVE '-'            TO  WS-AM-ZIP-DASH
03977              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
03978
03979      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
03980
03981      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
03982
03983  7204-SET-PHONE.
03984      MOVE WSPI-AREA              TO WSPO-AREA.
03985      MOVE WSPI-PFX               TO WSPO-PFX.
03986      MOVE WSPI-SFX               TO WSPO-SFX.
03987      MOVE WS-PHONE-OUT           TO SS44-5D.
03988
03989      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
03990
03991      MOVE WS-LABEL-LINES (1)     TO SS43D.
03992      MOVE WS-LABEL-LINES (2)     TO SS44-1D.
03993      MOVE WS-LABEL-LINES (3)     TO SS44-2D.
03994      MOVE WS-LABEL-LINES (4)     TO SS44-3D.
03995      MOVE WS-LABEL-LINES (5)     TO SS44-4D.
03996      MOVE WS-LABEL-LINES (6)     TO SS44-6D.
03997
03998      EJECT
03999  7205-READ-PHYSICIAN-ADDR.
04000      IF W-PHYSICIAN > ZERO
04001          COMPUTE ACTV-SEQ = W-PHYSICIAN + 30
04002      ELSE
04003          MOVE CL-DOCTOR-ADDR-CNT TO ACTV-SEQ
04004          ADD +30                 TO ACTV-SEQ.
04005
04006      IF ACTV-SEQ = +30
04007         GO TO 7210-READ-EMPLOYER-ADDR.
04008
04009      
      * EXEC CICS HANDLE CONDITION
04010 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04011 *         NOTFND     (7210-READ-EMPLOYER-ADDR)
04012 *    END-EXEC.
      *    MOVE '"$JI                  ! 6 #00009866' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303039383636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04013
04014      
      * EXEC CICS READ
04015 *         DATASET    (ACTV-ID)
04016 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
04017 *         RIDFLD     (ACTV-KEY)
04018 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009871' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039383731' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04019
04020      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04021      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
04022      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04023      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04024 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04025
04026      MOVE SPACES                 TO  WS-ZIP-CODE.
04027      IF AT-CANADIAN-POST-CODE
04028          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04029          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04030      ELSE
04031          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04032          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04033              MOVE '-'            TO  WS-AM-ZIP-DASH
04034              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04035
04036      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04037
04038      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04039      MOVE WSPI-AREA              TO WSPO-AREA.
04040      MOVE WSPI-PFX               TO WSPO-PFX.
04041      MOVE WSPI-SFX               TO WSPO-SFX.
04042      MOVE WS-PHONE-OUT           TO SS47-5D.
04043
04044      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04045
04046      MOVE WS-LABEL-LINES (1)     TO SS47D.
04047      MOVE WS-LABEL-LINES (2)     TO SS47-1D.
04048      MOVE WS-LABEL-LINES (3)     TO SS47-2D.
04049      MOVE WS-LABEL-LINES (4)     TO SS47-3D.
04050      MOVE WS-LABEL-LINES (5)     TO SS47-4D.
04051
04052      EJECT
04053
04054  7210-READ-EMPLOYER-ADDR.
04055      IF W-EMPLOYER > ZERO
04056          COMPUTE ACTV-SEQ = W-EMPLOYER + 40
04057      ELSE
04058          MOVE CL-EMPLOYER-ADDR-CNT
04059                                  TO ACTV-SEQ
04060          ADD +40                 TO ACTV-SEQ.
04061
04062      IF ACTV-SEQ = +40
04063         GO TO 7220-READ-INSURED-ADDR.
04064
04065      
      * EXEC CICS HANDLE CONDITION
04066 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04067 *         NOTFND     (7220-READ-INSURED-ADDR)
04068 *    END-EXEC.
      *    MOVE '"$JI                  ! 7 #00009925' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303039393235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04069
04070      
      * EXEC CICS READ
04071 *         DATASET    (ACTV-ID)
04072 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
04073 *         RIDFLD     (ACTV-KEY)
04074 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009930' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04075
04076      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04077      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
04078      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04079      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04080 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04081
04082      MOVE SPACES                 TO  WS-ZIP-CODE.
04083      IF AT-CANADIAN-POST-CODE
04084          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04085          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04086      ELSE
04087          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04088          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04089              MOVE '-'            TO  WS-AM-ZIP-DASH
04090              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04091
04092      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04093
04094      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04095      MOVE WSPI-AREA              TO WSPO-AREA.
04096      MOVE WSPI-PFX               TO WSPO-PFX.
04097      MOVE WSPI-SFX               TO WSPO-SFX.
04098      MOVE WS-PHONE-OUT           TO SS48-5D.
04099
04100      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04101
04102      MOVE WS-LABEL-LINES (1)     TO SS48D.
04103      MOVE WS-LABEL-LINES (2)     TO SS48-1D.
04104      MOVE WS-LABEL-LINES (3)     TO SS48-2D.
04105      MOVE WS-LABEL-LINES (4)     TO SS48-3D.
04106      MOVE WS-LABEL-LINES (5)     TO SS48-4D.
04107
04108      EJECT
04109  7220-READ-INSURED-ADDR.
04110
04111      IF W-INSURED > ZERO
04112          COMPUTE ACTV-SEQ = W-INSURED
04113      ELSE
04114          MOVE CL-INSURED-ADDR-CNT TO ACTV-SEQ.
04115
04116      IF ACTV-SEQ = +0
04117         GO TO 7225-READ-OTHER1-ADDR.
04118
04119      
      * EXEC CICS HANDLE CONDITION
04120 *         NOTOPEN   (8870-ACTV-NOT-OPEN)
04121 *         NOTFND    (7225-READ-OTHER1-ADDR)
04122 *    END-EXEC.
      *    MOVE '"$JI                  ! 8 #00009982' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303039393832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04123
04124      
      * EXEC CICS READ
04125 *         DATASET   (ACTV-ID)
04126 *         SET       (ADDRESS OF ACTIVITY-TRAILERS)
04127 *         RIDFLD    (ACTV-KEY)
04128 *    END-EXEC.
      *    MOVE '&"S        E          (   #00009987' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039393837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04129
04130      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04131      MOVE SS10D                  TO WS-LABEL-LINES (1).
04132      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04133      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04134 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04135
04136      MOVE SPACES                 TO  WS-ZIP-CODE.
04137      IF AT-CANADIAN-POST-CODE
04138          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04139          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04140      ELSE
04141          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04142          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04143              MOVE '-'            TO  WS-AM-ZIP-DASH
04144              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04145
04146      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04147
04148      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04149
04150      MOVE WS-LABEL-LINES (1)     TO SS10D.
04151      MOVE WS-LABEL-LINES (2)     TO SS11-1D.
04152      MOVE WS-LABEL-LINES (3)     TO SS11-2D.
04153      MOVE WS-LABEL-LINES (4)     TO SS11-3D.
04154      MOVE WS-LABEL-LINES (5)     TO SS11-4D.
04155      MOVE AT-MAIL-TO-NAME        TO SS11-5D.
04156
04157      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04158      MOVE WSPI-AREA              TO WSPO-AREA.
04159      MOVE WSPI-PFX               TO WSPO-PFX.
04160      MOVE WSPI-SFX               TO WSPO-SFX.
04161      MOVE WS-PHONE-OUT           TO SS11-6D.
04162
04163      EJECT
04164
04165  7225-READ-OTHER1-ADDR.
04166
04167      IF W-OTHER-1 > ZERO
04168          COMPUTE ACTV-SEQ = W-OTHER-1 + 50
04169      ELSE
04170          MOVE CL-OTHER-1-ADDR-CNT TO ACTV-SEQ
04171          ADD +50                  TO ACTV-SEQ.
04172
04173      IF ACTV-SEQ = +50
04174         GO TO 7230-READ-OTHER2-ADDR.
04175
04176      
      * EXEC CICS HANDLE CONDITION
04177 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04178 *         NOTFND     (7230-READ-OTHER2-ADDR)
04179 *    END-EXEC.
      *    MOVE '"$JI                  ! 9 #00010042' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303130303432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04180
04181      
      * EXEC CICS READ
04182 *         DATASET    (ACTV-ID)
04183 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
04184 *         RIDFLD     (ACTV-KEY)
04185 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010047' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130303437' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04186
04187      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04188      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
04189      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04190      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04191 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04192
04193      MOVE SPACES                 TO  WS-ZIP-CODE.
04194      IF AT-CANADIAN-POST-CODE
04195          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04196          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04197      ELSE
04198          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04199          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04200              MOVE '-'            TO  WS-AM-ZIP-DASH
04201              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04202
04203      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04204
04205      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04206      MOVE WSPI-AREA              TO WSPO-AREA.
04207      MOVE WSPI-PFX               TO WSPO-PFX.
04208      MOVE WSPI-SFX               TO WSPO-SFX.
04209      MOVE WS-PHONE-OUT           TO SS49-5D.
04210
04211      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04212
04213      MOVE WS-LABEL-LINES (1)     TO SS49D.
04214      MOVE WS-LABEL-LINES (2)     TO SS49-1D.
04215      MOVE WS-LABEL-LINES (3)     TO SS49-2D.
04216      MOVE WS-LABEL-LINES (4)     TO SS49-3D.
04217      MOVE WS-LABEL-LINES (5)     TO SS49-4D.
04218
04219      EJECT
04220  7230-READ-OTHER2-ADDR.
04221
04222      IF W-OTHER-2 > ZERO
04223          COMPUTE ACTV-SEQ = W-OTHER-2 + 60
04224      ELSE
04225          MOVE CL-OTHER-2-ADDR-CNT TO ACTV-SEQ
04226          ADD +60                  TO ACTV-SEQ.
04227
04228      IF ACTV-SEQ = +60
04229         GO TO 7240-NOT-FOUND.
04230
04231      
      * EXEC CICS HANDLE CONDITION
04232 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04233 *         NOTFND     (7240-NOT-FOUND)
04234 *    END-EXEC.
      *    MOVE '"$JI                  ! : #00010100' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303130313030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04235
04236      
      * EXEC CICS READ
04237 *         DATASET    (ACTV-ID)
04238 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
04239 *         RIDFLD     (ACTV-KEY)
04240 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010105' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04241
04242      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04243      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
04244      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04245      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04246 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04247
04248      MOVE SPACES                 TO  WS-ZIP-CODE.
04249      IF AT-CANADIAN-POST-CODE
04250          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04251          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04252      ELSE
04253          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04254          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04255              MOVE '-'            TO  WS-AM-ZIP-DASH
04256              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04257
04258      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04259
04260      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04261      MOVE WSPI-AREA              TO WSPO-AREA.
04262      MOVE WSPI-PFX               TO WSPO-PFX.
04263      MOVE WSPI-SFX               TO WSPO-SFX.
04264      MOVE WS-PHONE-OUT           TO SS50-5D.
04265
04266      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04267
04268      MOVE WS-LABEL-LINES (1)     TO SS50D.
04269      MOVE WS-LABEL-LINES (2)     TO SS50-1D.
04270      MOVE WS-LABEL-LINES (3)     TO SS50-2D.
04271      MOVE WS-LABEL-LINES (4)     TO SS50-3D.
04272      MOVE WS-LABEL-LINES (5)     TO SS50-4D.
04273
04274      EJECT
04275  7240-NOT-FOUND.
04276      IF ACCOUNT-IS-ONLINE
04277         GO TO 7250-READ-ACCOUNT.
04278
04279      IF W-ACCOUNT > ZERO
04280          COMPUTE ACTV-SEQ = W-ACCOUNT + 20
04281      ELSE
04282          MOVE CL-ACCOUNT-ADDR-CNT TO ACTV-SEQ
04283          ADD +20                  TO ACTV-SEQ.
04284
04285      IF ACTV-SEQ = +20
04286         GO TO 7250-READ-ACCOUNT.
04287
04288      
      * EXEC CICS HANDLE CONDITION
04289 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04290 *         NOTFND     (7250-READ-ACCOUNT)
04291 *    END-EXEC.
      *    MOVE '"$JI                  ! ; #00010160' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303130313630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04292
04293      
      * EXEC CICS READ
04294 *         DATASET    (ACTV-ID)
04295 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
04296 *         RIDFLD     (ACTV-KEY)
04297 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010165' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130313635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04298
04299      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04300      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
04301      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04302      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04303 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04304
04305      MOVE SPACES                 TO  WS-ZIP-CODE.
04306      IF AT-CANADIAN-POST-CODE
04307          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04308          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04309      ELSE
04310          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04311          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04312              MOVE '-'            TO  WS-AM-ZIP-DASH
04313              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04314
04315      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04316
04317      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04318      MOVE WSPI-AREA              TO WSPO-AREA.
04319      MOVE WSPI-PFX               TO WSPO-PFX.
04320      MOVE WSPI-SFX               TO WSPO-SFX.
04321      MOVE WS-PHONE-OUT           TO SS07-6D.
04322
121802*    IF PI-COMPANY-ID NOT = 'FLA'
121802*        PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04325
04326      MOVE WS-LABEL-LINES (1)     TO SS06D.
04327      MOVE WS-LABEL-LINES (2)     TO SS07-1D.
04328      MOVE WS-LABEL-LINES (3)     TO SS07-2D.
04329      MOVE WS-LABEL-LINES (4)     TO SS07-3D.
04330      MOVE WS-LABEL-LINES (5)     TO SS07-4D.
04331      MOVE WS-LABEL-LINES (6)     TO SS07-5D.
04332
04333      EJECT
04334  7250-READ-ACCOUNT.
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        GO TO 7250-READ-PRODUCER.
04337
04338      IF WS-ACCT-READ-SW = 'Y'
04339         GO TO 7250-BUILD-ACCT-ADDR.
04340
04341      MOVE CM-CERT-EFF-DT TO ACCT-EXP-DATE.
04342
04343      
      * EXEC CICS HANDLE CONDITION
04344 *         NOTOPEN   (8880-ACCT-NOT-OPEN)
04345 *         NOTFND    (7251-READ-3RD-PARTY)
04346 *    END-EXEC.
      *    MOVE '"$JI                  ! < #00010218' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303130323138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04347
04348  7250-STARTBR-ACCOUNT.
04349      MOVE ACCT-PARTIAL-KEY       TO  ACCT-SAVE-KEY.
04350      PERFORM 8000-STARTBR-ERACCT THRU 8000-STARTBR-EXIT.
04351
04352  7250-READNEXT-ACCOUNT.
04353      PERFORM 8000-READNEXT-ERACCT THRU 8000-READNEXT-EXIT.
04354
04355      IF ACCT-PARTIAL-KEY NOT = ACCT-SAVE-KEY
04356          IF WS-SAVE-ACCT-RECORD = SPACES
04357              GO TO 7251-READ-3RD-PARTY
04358          ELSE
04359              MOVE AM-CONTROL-PRIMARY  TO  ACCT-KEY
04360              MOVE WS-SAVE-ACCT-RECORD TO  ACCOUNT-MASTER
04361              GO TO 7250-BUILD-ACCT-ADDR.
04362
04363      IF AM-EXPIRATION-DT = HIGH-VALUES
04364          MOVE AM-CONTROL-PRIMARY      TO  ACCT-KEY
04365      ELSE
04366          MOVE ACCOUNT-MASTER          TO  WS-SAVE-ACCT-RECORD
04367          GO TO 7250-READNEXT-ACCOUNT.
04368
04369  7250-BUILD-ACCT-ADDR.
04370      MOVE SPACES                TO  WS-SAVE-ACCT-RECORD.
04371
04372      IF NOT ACCOUNT-IS-ONLINE
04373         GO TO 7251-READ-3RD-PARTY.
04374
04375      MOVE SPACES                TO WS-LABEL-HOLD-AREA.
04376      MOVE AM-NAME               TO WS-LABEL-LINES (1).
04377      MOVE AM-PERSON             TO WS-LABEL-LINES (2).
04378      MOVE AM-ADDRS              TO WS-LABEL-LINES (3).
04379 *    MOVE AM-CITY               TO WS-LABEL-LINES (4).
           STRING AM-ADDR-CITY ' ' AM-ADDR-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04380
04381      MOVE SPACES                TO  WS-ZIP-CODE.
04382      IF AM-CANADIAN-POST-CODE
04383          MOVE AM-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
04384          MOVE AM-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
04385      ELSE
04386          MOVE AM-ZIP-PRIME      TO  WS-AM-ZIP-CODE
04387          IF AM-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04388              MOVE '-'           TO  WS-AM-ZIP-DASH
04389              MOVE AM-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
04390
04391      MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).
04392
04393      MOVE ZEROS                 TO WS-PHONE-IN.
04394      MOVE AM-AREA-CODE          TO WSPO-AREA.
04395      MOVE AM-TEL-PRE            TO WSPO-PFX.
04396      MOVE AM-TEL-NBR            TO WSPO-SFX.
04397      MOVE WS-PHONE-OUT          TO SS07-6D.
04398
121802*    IF PI-COMPANY-ID NOT = 'FLA'
121802*        PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04401
04402      MOVE WS-LABEL-LINES (1)     TO SS06D.
04403      MOVE WS-LABEL-LINES (2)     TO SS07-1D.
04404      MOVE WS-LABEL-LINES (3)     TO SS07-2D.
04405      MOVE WS-LABEL-LINES (4)     TO SS07-3D.
04406      MOVE WS-LABEL-LINES (5)     TO SS07-4D.
04407      MOVE WS-LABEL-LINES (6)     TO SS07-5D.
04408
121802*    GO TO 7251-READ-3RD-PARTY.
04410
04411      EJECT
121802*7250-READ-PRODUCER. Remove as dead code
121802*7250-STARTBR-PRODUCER. Remove as dead code
121802*7250-READNEXT-PRODUCER. Remove as dead code
121802*7250-BUILD-PROD-ADDR. Remove as dead code
04488  7251-READ-3RD-PARTY.
04489
04490      MOVE +29                    TO ACTV-SEQ.
04491
04492      
      * EXEC CICS HANDLE CONDITION
04493 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04494 *         NOTFND     (7252-READ-COMP)
04495 *    END-EXEC.
      *    MOVE '"$JI                  ! = #00010298' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303130323938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04496
04497      
      * EXEC CICS READ
04498 *         DATASET    (ACTV-ID)
04499 *         SET        (ADDRESS OF ACTIVITY-TRAILERS)
04500 *         RIDFLD     (ACTV-KEY)
04501 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010303' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04502
04503      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04504      MOVE AT-MAIL-TO-NAME        TO WS-LABEL-LINES (1).
04505      MOVE AT-ADDRESS-LINE-1      TO WS-LABEL-LINES (2).
04506      MOVE AT-ADDRESS-LINE-2      TO WS-LABEL-LINES (3).
04507 *    MOVE AT-CITY-STATE          TO WS-LABEL-LINES (4).
           STRING AT-CITY ' ' AT-STATE DELIMITED BY '  '
              INTO WS-LABEL-LINES (4)
           END-STRING
04508
04509      MOVE SPACES                 TO  WS-ZIP-CODE.
04510      IF AT-CANADIAN-POST-CODE
04511          MOVE AT-CAN-POSTAL-1    TO  WS-CAN-POSTAL-1
04512          MOVE AT-CAN-POSTAL-2    TO  WS-CAN-POSTAL-2
04513      ELSE
04514          MOVE AT-ZIP-CODE        TO  WS-AM-ZIP-CODE
04515          IF AT-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04516              MOVE '-'            TO  WS-AM-ZIP-DASH
04517              MOVE AT-ZIP-PLUS4   TO  WS-AM-ZIP-PLUS4.
04518
04519      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (5).
04520
04521      MOVE AT-PHONE-NO            TO WS-PHONE-IN.
04522      MOVE WSPI-AREA              TO WSPO-AREA.
04523      MOVE WSPI-PFX               TO WSPO-PFX.
04524      MOVE WSPI-SFX               TO WSPO-SFX.
04525      MOVE WS-PHONE-OUT           TO SS53-6D.
04526
04527      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04528
04529      MOVE WS-LABEL-LINES (1)     TO SS52D.
04530      MOVE WS-LABEL-LINES (2)     TO SS53-1D.
04531      MOVE WS-LABEL-LINES (3)     TO SS53-2D.
04532      MOVE WS-LABEL-LINES (4)     TO SS53-3D.
04533      MOVE WS-LABEL-LINES (5)     TO SS53-4D.
04534      MOVE WS-LABEL-LINES (6)     TO SS53-5D.
04535
04536      GO TO 7260-READ-DENIAL.
04537      EJECT
04538  7252-READ-COMP.
04539
04540      IF WS-COMP-READ-SW = 'Y'
04541         GO TO 7255-BUILD-COMP-ADDR.
04542
04543      IF AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC
04544         MOVE ZEROS         TO AM-3RD-PARTY-NOTIF-LEVEL
04545         GO TO 7260-READ-DENIAL.
04546
04547      IF AM-3RD-PARTY-NOTIF-LEVEL > 00 AND < 11
04548         NEXT SENTENCE
04549      ELSE
04550         GO TO 7260-READ-DENIAL.
04551
04552      IF AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) = SPACES OR ZEROS
04553         GO TO 7260-READ-DENIAL.
04554
04555      MOVE PI-COMPANY-CD   TO WS-ERCOMP-COMPANY-CD.
04556      MOVE AM-CARRIER      TO WS-ERCOMP-CARRIER.
04557      MOVE AM-GROUPING     TO WS-ERCOMP-GROUPING.
04558      MOVE 'A'             TO WS-ERCOMP-TYPE.
04559      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
04560                           TO WS-ERCOMP-RESP-NO.
04561
04562      IF AM-3RD-PARTY-NOTIF-LEVEL = AM-REMIT-TO
04563          IF AM-COM-TYP (AM-REMIT-TO) = 'O' OR 'P' OR
04564                                        'G' OR 'B' or 'S'
04565              MOVE 'G'            TO WS-ERCOMP-TYPE
04566              MOVE LOW-VALUES     TO WS-ERCOMP-ACCOUNT
04567          ELSE
04568              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)
04569                                  TO WS-ERCOMP-ACCOUNT
04570      ELSE
04571          MOVE 'G'                TO WS-ERCOMP-TYPE
04572          MOVE LOW-VALUES         TO WS-ERCOMP-ACCOUNT.
04573
04574      IF PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP
04575         MOVE ZEROS TO WS-ERCOMP-CARRIER.
04576
04577      IF PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP
04578         MOVE ZEROS TO WS-ERCOMP-GROUPING.
04579
04580      
      * EXEC CICS HANDLE CONDITION
04581 *         NOTFND    (7260-READ-DENIAL)
04582 *    END-EXEC.
      *    MOVE '"$I                   ! > #00010389' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303130333839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04583
04584      
      * EXEC CICS  READ
04585 *         SET      (ADDRESS OF COMPENSATION-MASTER)
04586 *         DATASET  ('ERCOMP')
04587 *         RIDFLD   (WS-ERCOMP-KEY)
04588 *    END-EXEC.
           MOVE 'ERCOMP' TO DFHEIV1
      *    MOVE '&"S        E          (   #00010393' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 WS-ERCOMP-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04589
04590  7255-BUILD-COMP-ADDR.
04591
04592      MOVE SPACES                TO WS-LABEL-HOLD-AREA.
04593      MOVE CO-ACCT-NAME          TO WS-LABEL-LINES (1).
04594      IF CO-ACCT-NAME = SPACES
04595         MOVE CO-MAIL-NAME       TO WS-LABEL-LINES (1).
04596      MOVE CO-ADDR-1             TO WS-LABEL-LINES (2).
04597      MOVE CO-ADDR-2             TO WS-LABEL-LINES (3).
04598      MOVE CO-ADDR-3             TO WS-LABEL-LINES (4).
04599
04600      MOVE SPACES                TO  WS-ZIP-CODE.
04601      IF CO-CANADIAN-POST-CODE
04602          MOVE CO-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
04603          MOVE CO-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
04604      ELSE
04605          MOVE CO-ZIP-PRIME      TO  WS-AM-ZIP-CODE
04606          IF CO-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04607              MOVE '-'           TO  WS-AM-ZIP-DASH
04608              MOVE CO-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
04609
04610      MOVE WS-ZIP-CODE           TO  WS-LABEL-LINES (5).
04611
04612      MOVE ZEROS                 TO WS-PHONE-IN.
04613      MOVE CO-AREA-CODE          TO WSPO-AREA.
04614      MOVE CO-PREFIX             TO WSPO-PFX.
04615      MOVE CO-PHONE              TO WSPO-SFX.
04616      MOVE WS-PHONE-OUT          TO SS53-6D.
04617
04618      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04619
04620      MOVE WS-LABEL-LINES (1)     TO SS52D.
04621      MOVE WS-LABEL-LINES (2)     TO SS53-1D.
04622      MOVE WS-LABEL-LINES (3)     TO SS53-2D.
04623      MOVE WS-LABEL-LINES (4)     TO SS53-3D.
04624      MOVE WS-LABEL-LINES (5)     TO SS53-4D.
04625      MOVE WS-LABEL-LINES (6)     TO SS53-5D.
04626
04627      EJECT
04628
04629  7260-READ-DENIAL.
04630
04631      MOVE +93                    TO ACTV-SEQ
04632
04633      
      * EXEC CICS HANDLE CONDITION
04634 *         NOTOPEN    (8870-ACTV-NOT-OPEN)
04635 *         NOTFND     (7265-READ-CNTL1)
04636 *         ENDFILE    (7265-READ-CNTL1)
04637 *    END-EXEC.
      *    MOVE '"$JI''                 ! ? #00010442' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3F20233030303130343432' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04638
04639      
      * EXEC CICS STARTBR
04640 *         DATASET    (ACTV-ID)
04641 *         RIDFLD     (ACTV-KEY)
04642 *         GTEQ
04643 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010448' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04644
04645      MOVE 'Y'                    TO ACTV-BROWSE-STARTED.
04646      MOVE ACTV-PARTIAL-KEY       TO ACTV-SAVE-KEY.
04647
04648  7262-READ-NEXT.
04649
04650      
      * EXEC CICS READNEXT
04651 *         SET     (ADDRESS OF ACTIVITY-TRAILERS)
04652 *         RIDFLD  (ACTV-KEY)
04653 *         DATASET (ACTV-ID)
04654 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010459' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ACTV-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACTIVITY-TRAILERS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04655
04656      IF ACTV-PARTIAL-KEY  NOT = ACTV-SAVE-KEY
04657         GO TO 7265-READ-CNTL1.
060109     IF AT-TRAILER-TYPE = '2'
060109        AND AT-PAYMENT-TYPE = 'I'
060109        AND AT-INT-RATE NUMERIC
060109        AND AT-INT-RATE NOT = ZEROS
060109        COMPUTE WS-WORK-INT-RATE = AT-INT-RATE * 100
060109        MOVE +1                  TO S2
060109        PERFORM VARYING S1 FROM +1 BY +1 UNTIL
060109           (S1 > +6)
060109           IF WS-WORK-INT (S1:1) NOT = '0'
060109              MOVE WS-WORK-INT (S1:1)
060109                                 TO SS16-1D (S2:1)
060109              ADD +1             TO S2
060109           END-IF
060109        END-PERFORM
060109        MOVE '%'                 TO SS16-1D (S2:1)
060109        GO TO 7262-READ-NEXT
060109     END-IF
04659      IF AT-TRAILER-TYPE = '8'
04662         MOVE AT-DENIAL-INFO-1    TO SS35-1D
04663         MOVE AT-DENIAL-INFO-2    TO SS35-2D
060109        IF (CL-TOTAL-INT-PAID = ZEROS)
060109           OR (CL-CLAIM-TYPE NOT = (PI-LIFE-OVERRIDE-L1 OR 'O'))
060109           GO TO 7265-READ-CNTL1
              END-IF
           END-IF
           GO TO 7262-READ-NEXT
           .
04666  7265-READ-CNTL1.
04667      IF ACTV-BROWSE-STARTED = 'Y'
04668         MOVE 'N'                  TO ACTV-BROWSE-STARTED
04669         
      * EXEC CICS ENDBR
04670 *            DATASET    (ACTV-ID)
04671 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010497' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACTV-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04672
04673      IF SS35-1D = ALL '*'
04674          MOVE '@@DENIAL1'        TO SS35-1D.
04675
04676      IF SS35-2D = ALL '*'
04677          MOVE '@@DENIAL2'        TO SS35-2D.
04678
04679      MOVE '1'                    TO CNTL-RECORD-TYPE.
04680      MOVE ZEROS                  TO CNTL-SEQ.
04681      
      * EXEC CICS HANDLE CONDITION
04682 *         NOTOPEN   (8840-CNTL-NOT-OPEN)
04683 *         NOTFND    (7266-READ-CNTL2)
04684 *    END-EXEC.
      *    MOVE '"$JI                  ! @ #00010509' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4020233030303130353039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04685
04686      
      * EXEC CICS READ
04687 *         DATASET   (CNTL-ID)
04688 *         SET       (ADDRESS OF CONTROL-FILE)
04689 *         RIDFLD    (CNTL-KEY)
04690 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010514' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04691
04692      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04693      MOVE CF-CL-MAIL-TO-NAME     TO WS-LABEL-LINES (1).
04694      MOVE CF-CL-IN-CARE-OF       TO WS-LABEL-LINES (2).
04695      MOVE CF-CL-ADDR-LINE-1      TO WS-LABEL-LINES (3).
04696      MOVE CF-CL-ADDR-LINE-2      TO WS-LABEL-LINES (4).
04697      MOVE CF-CL-CITY-STATE       TO WS-LABEL-LINES (5).
04698
04699      IF CF-CL-ZIP-CODE-NUM NOT NUMERIC
04700          MOVE ZEROS              TO  CF-CL-ZIP-CODE-NUM.
04701      IF CF-CL-ZIP-CODE-NUM NOT = ZEROS
04702          MOVE CF-CL-ZIP-CODE-NUM TO  WS-ZIP-NUMERIC
04703          MOVE WS-ZIP-NONNUM      TO  CF-CL-ZIP-CODE.
04704
04705      MOVE SPACES                 TO  WS-ZIP-CODE.
04706      IF CF-CL-CAN-POST-CODE
04707          MOVE CF-CL-CAN-POSTAL-1   TO  WS-CAN-POSTAL-1
04708          MOVE CF-CL-CAN-POSTAL-2   TO  WS-CAN-POSTAL-2
04709      ELSE
04710          MOVE CF-CL-ZIP-PRIME      TO  WS-AM-ZIP-CODE
04711          IF CF-CL-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04712              MOVE '-'              TO  WS-AM-ZIP-DASH
04713              MOVE CF-CL-ZIP-PLUS4  TO  WS-AM-ZIP-PLUS4.
04714
04715      MOVE WS-ZIP-CODE            TO  WS-LABEL-LINES (6).
04716
04717      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04718
04719      MOVE WS-LABEL-LINES (1)     TO SS01D.
04720      MOVE WS-LABEL-LINES (2)     TO SS02-1D.
04721      MOVE WS-LABEL-LINES (3)     TO SS02-2D.
04722      MOVE WS-LABEL-LINES (4)     TO SS02-3D.
04723      MOVE WS-LABEL-LINES (5)     TO SS02-4D.
04724      MOVE WS-LABEL-LINES (6)     TO SS02-5D.
04725
04726  7266-READ-CNTL2.
04727      IF PI-PROCESSOR-ID  =  LGXX-ID
04728         GO TO 7267-READ-CNTL4.
04729
04730      MOVE '2'                    TO CNTL-RECORD-TYPE.
04731      MOVE PI-PROCESSOR-ID        TO CNTL-GENL.
04732      MOVE ZEROS                  TO CNTL-SEQ.
04733
04734      
      * EXEC CICS HANDLE CONDITION
04735 *         NOTOPEN    (8840-CNTL-NOT-OPEN)
04736 *         NOTFND     (7267-READ-CNTL4)
04737 *    END-EXEC.
      *    MOVE '"$JI                  ! A #00010562' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4120233030303130353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04738
04739      
      * EXEC CICS READ
04740 *         DATASET    (CNTL-ID)
04741 *         SET        (ADDRESS OF CONTROL-FILE)
04742 *         RIDFLD     (CNTL-KEY)
04743 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010567' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04744
04745      MOVE CF-PROCESSOR-NAME      TO SS08D.
04746      MOVE CF-PROCESSOR-TITLE     TO SS09D.
04747
04748      EJECT
04749  7267-READ-CNTL4.
121802*    IF CL-SYSTEM-IDENTIFIER = 'CV'
121802*        GO TO 7267-READ-EMPLAN.
04752
04753      IF BEN-HOLD = ZEROS
04754         GO TO 7267-READ-CNTL6.
04755
04756      MOVE BEN-HOLD               TO CNTL-GEN2.
04757      MOVE SPACES                 TO CNTL-GEN1.
04758
04759      IF CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
04760         MOVE '4'                 TO CNTL-RECORD-TYPE
04761      ELSE
04762         MOVE '5'                 TO CNTL-RECORD-TYPE.
04763
04764      MOVE ZEROS                  TO CNTL-SEQ.
04765
04766      
      * EXEC CICS HANDLE CONDITION
04767 *         NOTOPEN    (8840-CNTL-NOT-OPEN)
04768 *         NOTFND     (7267-READ-CNTL6)
04769 *    END-EXEC.
      *    MOVE '"$JI                  ! B #00010594' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4220233030303130353934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04770
04771      
      * EXEC CICS READ
04772 *         DATASET    (CNTL-ID)
04773 *         SET        (ADDRESS OF CONTROL-FILE)
04774 *         RIDFLD     (CNTL-KEY)
04775 *         GTEQ
04776 *    END-EXEC.
      *    MOVE '&"S        G          (   #00010599' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04777
04778      MOVE 1                      TO SUB.
04779  7267-LOOP.
04780      IF SUB = 9
04781         GO TO 7267-READ-CNTL6.
04782
04783      IF CF-BENEFIT-CODE (SUB) < BEN-HOLD
04784         ADD 1 TO SUB
04785         GO TO 7267-LOOP.
04786
04787      IF BEN-HOLD = CF-BENEFIT-CODE (SUB)
04788         MOVE CF-BENEFIT-DESCRIP (SUB) TO SS22D
121802        IF CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802           OR CL-CLAIM-TYPE = 'I'
121203           OR CL-CLAIM-TYPE = 'G'
052614           OR CL-CLAIM-TYPE = 'F'
080322           OR CL-CLAIM-TYPE = 'B'
080322           OR CL-CLAIM-TYPE = 'H'
04790            MOVE CF-BENEFIT-ALPHA (SUB)   TO BENEFIT-WORK
04791            MOVE ELIM-DAYS                TO SS42D.
04792
04793      GO TO 7267-READ-CNTL6.
04794
04795      EJECT
121802*7267-READ-EMPLAN. Remove as dead code
04820  7267-READ-CNTL6.
04821      MOVE '6'                    TO CNTL-RECORD-TYPE.
04822      MOVE SPACES                 TO CNTL-GENL.
04823
121802*    IF PI-COMPANY-ID = 'AIG' OR 'AUK'
121802*        MOVE CL-CURRENT-CARRIER TO CNTL-GEN4
121802*    ELSE
04827          MOVE PI-CARRIER         TO CNTL-GEN4.
04828
04829      MOVE ZEROS                  TO CNTL-SEQ.
04830
04831      
      * EXEC CICS HANDLE CONDITION
04832 *         NOTOPEN    (8840-CNTL-NOT-OPEN)
04833 *         NOTFND     (7268-SET-DATE)
04834 *    END-EXEC.
      *    MOVE '"$JI                  ! C #00010641' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4320233030303130363431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04835
04836      
      * EXEC CICS READ
04837 *         DATASET    (CNTL-ID)
04838 *         SET        (ADDRESS OF CONTROL-FILE)
04839 *         RIDFLD     (CNTL-KEY)
04840 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010646' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130363436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04841
04842      MOVE SPACES              TO WS-LABEL-HOLD-AREA.
04843      MOVE CF-MAIL-TO-NAME     TO WS-LABEL-LINES (1).
04844      MOVE CF-IN-CARE-OF       TO WS-LABEL-LINES (2).
04845      MOVE CF-ADDRESS-LINE-1   TO WS-LABEL-LINES (3).
04846      MOVE CF-ADDRESS-LINE-2   TO WS-LABEL-LINES (4).
04847      MOVE CF-CITY-STATE       TO WS-LABEL-LINES (5).
04848
04849      IF CF-ZIP-CODE-NUM NOT NUMERIC
04850          MOVE ZEROS              TO  CF-ZIP-CODE-NUM.
04851      IF CF-ZIP-CODE-NUM NOT = ZEROS
04852          MOVE CF-ZIP-CODE-NUM    TO  WS-ZIP-NUMERIC
04853          MOVE WS-ZIP-NONNUM      TO  CF-ZIP-CODE.
04854
04855      MOVE SPACES              TO  WS-ZIP-CODE.
04856      IF CF-CANADIAN-POST-CODE
04857          MOVE CF-CAN-POSTAL-1  TO  WS-CAN-POSTAL-1
04858          MOVE CF-CAN-POSTAL-2  TO  WS-CAN-POSTAL-2
04859      ELSE
04860          MOVE CF-ZIP-PRIME     TO  WS-AM-ZIP-CODE
04861          IF CF-ZIP-PLUS4 NOT = SPACES  AND  ZEROS
04862              MOVE '-'          TO  WS-AM-ZIP-DASH
04863              MOVE CF-ZIP-PLUS4 TO  WS-AM-ZIP-PLUS4.
04864
04865      MOVE WS-ZIP-CODE         TO  WS-LABEL-LINES (6).
04866
04867      MOVE CF-PHONE-NO         TO WS-PHONE-IN.
04868      MOVE WSPI-AREA           TO WSPO-AREA.
04869      MOVE WSPI-PFX            TO WSPO-PFX.
04870      MOVE WSPI-SFX            TO WSPO-SFX.
04871      MOVE WS-PHONE-OUT        TO SS04-6D.
04872
04873      PERFORM 7270-LABEL-MOVE THRU 7279-EXIT.
04874
04875      MOVE WS-LABEL-LINES (1)  TO SS03D
010407     IF PI-CARRIER = '8'
010407        MOVE 'as Administrator for Investors Heritage Life Insuran
010407-     'ce Company'               TO SS03-1D
010407     END-IF
04876      MOVE WS-LABEL-LINES (2)  TO SS04-1D.
04877      MOVE WS-LABEL-LINES (3)  TO SS04-2D.
04878      MOVE WS-LABEL-LINES (4)  TO SS04-3D.
04879      MOVE WS-LABEL-LINES (5)  TO SS04-4D.
04880      MOVE WS-LABEL-LINES (6)  TO SS04-5D.
04881
04882      EJECT
04883  7268-SET-DATE.
04884      MOVE EIBDATE                TO DATE-WORK.
04885      MOVE DT-WORK                TO DC-JULIAN-YYDDD.
04886      MOVE '5'                    TO DC-OPTION-CODE.
04887      PERFORM 9700-DATE-LINK  THRU  9700-EXIT.
04888      MOVE DC-GREG-DATE-1-EDIT    TO SS20D.
04889      MOVE DC-GREG-DATE-1-ALPHA   TO SS21D.
04890
04891      IF NOT LOWER-CASE-LETTERS-USED
04892         GO TO 7269-EXIT.
04893
121802*    IF PI-COMPANY-ID = 'DMD'
121802*        MOVE SS21D              TO WS-TEMP-AREA2
121802*        PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
121802*        END-IF
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
121802*        END-IF
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
121802*        END-IF
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
121802*        END-IF
121802*        IF WS-TEMP-AREA2 (1:1) = SPACE
121802*           MOVE WS-TEMP-AREA2 (2:29) TO WS-SAVE-TEMP-AREA2
121802*           MOVE WS-SAVE-TEMP-AREA2   TO WS-TEMP-AREA2
121802*        END-IF
121802*        MOVE WS-TEMP-AREA2      TO SS21D
121802*        GO TO 7269-EXIT.
121802*
04920      INSPECT SS19D   CONVERTING UPPER-CASE TO LOWER-CASE.
04921      INSPECT SS35-1D CONVERTING UPPER-CASE TO LOWER-CASE.
04922      INSPECT SS35-2D CONVERTING UPPER-CASE TO LOWER-CASE.
04923      INSPECT SS22D   CONVERTING UPPER-CASE TO LOWER-CASE.
04924
04925      MOVE 'N'                    TO WS-STATE-LINE.
04926
102703     MOVE 'Y'                    TO WS-PROCESSOR-LINE.
04927      MOVE SS08D                  TO WS-TEMP-AREA2.
04928      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
04929      MOVE WS-TEMP-AREA2          TO SS08D.
102703     MOVE 'N'                    TO WS-PROCESSOR-LINE.
102703     MOVE 'N'                    TO WS-CAPS-SW.
04930
04931      MOVE SS09D                  TO WS-TEMP-AREA2.
04932      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
04933      MOVE WS-TEMP-AREA2          TO SS09D.
04934
04935      MOVE SS11-5D                TO WS-TEMP-AREA2.
04936      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
04937      MOVE WS-TEMP-AREA2          TO SS11-5D.
04938
04939      MOVE SS21D                  TO WS-TEMP-AREA2.
04940      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
04941      MOVE WS-TEMP-AREA2          TO SS21D.
04942
04943      MOVE SS40D                  TO WS-TEMP-AREA2.
04944      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
04945      MOVE WS-TEMP-AREA2          TO SS40D.
04946
04947      MOVE SS41D                  TO WS-TEMP-AREA2.
04948      PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT.
04949      MOVE WS-TEMP-AREA2          TO SS41D.
04950
04951  7269-EXIT.
04952      EXIT.
04953
04954      EJECT
04955 *            COPY ELCNAMET.
00001 *****************************************************************
00002 *                                                               *
00002 *                                                               *
00003 *                            ELCNAMET                           *
00004 *                            VMOD=2.003                         *
00005 *                                                               *
00006 *            TRANSLATION ROUTINE USED MAINLY FOR CONVERTING     *
00007 *            NAMES AND ADDRESSES AND OTHER VARIABLES TO         *
00008 *            LOWER CASE.  THE FIRST CHARACTER OF EACH WORD      *
00009 *            IS LEFT AS UPPER AND ALL OTHER LOWER CASE.         *
00010 *                                                               *
00011 *                                                               *
00012 *****************************************************************.
102703*                   C H A N G E   L O G
102703*
102703* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102703*-----------------------------------------------------------------
102703*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102703* EFFECTIVE    NUMBER
102703*-----------------------------------------------------------------
102703* 102703                   SMVA  LEAVE CERTIFICATION DESIGNATION O
102703*                                PROCESSOR NAME LINE IN CAPS
102703******************************************************************
00013
00014  7270-LABEL-MOVE.
00015      IF WS-LABEL-HOLD-AREA = SPACES
00016         GO TO 7279-EXIT.
00017
00018      IF PI-COMPANY-ID NOT = 'DMD'
00019          PERFORM 7280-TRANSLATE-LOWER THRU 7280-EXIT.
00020
00021  7270-LABEL-MOVE-1.
00022
00023      IF WS-LABEL-LINES (1) = SPACES
00024         MOVE WS-LABEL-LINES (2)  TO WS-LABEL-LINES (1)
00025         MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)
00026         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
00027         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00028         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00029         MOVE SPACES              TO WS-LABEL-LINES (6)
00030         GO TO 7270-LABEL-MOVE-1.
00031
00032      IF WS-LABEL-LINES (2) = SPACES AND
00033         WS-LABEL-LINES (3) = SPACES AND
00034         WS-LABEL-LINES (4) = SPACES AND
00035         WS-LABEL-LINES (5) = SPACES AND
00036         WS-LABEL-LINES (6) = SPACES
00037         SET WS-NDX               TO 1
00038         GO TO 7275-MOVE-ZIP.
00039
00040  7270-LABEL-MOVE-2.
00041
00042      IF WS-LABEL-LINES (2) = SPACES
00043         MOVE WS-LABEL-LINES (3)  TO WS-LABEL-LINES (2)
00044         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
00045         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00046         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00047         MOVE SPACES              TO WS-LABEL-LINES (6)
00048         GO TO 7270-LABEL-MOVE-2.
00049
00050      IF WS-LABEL-LINES (3) = SPACES AND
00051         WS-LABEL-LINES (4) = SPACES AND
00052         WS-LABEL-LINES (5) = SPACES AND
00053         WS-LABEL-LINES (6) = SPACES
00054         SET WS-NDX               TO 2
00055         GO TO 7275-MOVE-ZIP.
00056
00057  7270-LABEL-MOVE-3.
00058
00059      IF WS-LABEL-LINES (3) = SPACES
00060         MOVE WS-LABEL-LINES (4)  TO WS-LABEL-LINES (3)
00061         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00062         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00063         MOVE SPACES              TO WS-LABEL-LINES (6)
00064         GO TO 7270-LABEL-MOVE-3.
00065
00066      IF WS-LABEL-LINES (4) = SPACES AND
00067         WS-LABEL-LINES (5) = SPACES AND
00068         WS-LABEL-LINES (6) = SPACES
00069         SET WS-NDX               TO 3
00070         GO TO 7275-MOVE-ZIP.
00071
00072  7270-LABEL-MOVE-4.
00073
00074      IF WS-LABEL-LINES (4) = SPACES
00075         MOVE WS-LABEL-LINES (5)  TO WS-LABEL-LINES (4)
00076         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00077         MOVE SPACES              TO WS-LABEL-LINES (6)
00078         GO TO 7270-LABEL-MOVE-4.
00079
00080      IF WS-LABEL-LINES (5) = SPACES AND
00081         WS-LABEL-LINES (6) = SPACES
00082         SET WS-NDX               TO 4
00083         GO TO 7275-MOVE-ZIP.
00084
00085  7270-LABEL-MOVE-5.
00086
00087      IF WS-LABEL-LINES (5) = SPACES
00088         MOVE WS-LABEL-LINES (6)  TO WS-LABEL-LINES (5)
00089         MOVE SPACES              TO WS-LABEL-LINES (6)
00090         SET WS-NDX               TO 5
00091         GO TO 7275-MOVE-ZIP
00092
00093      ELSE
00094         IF WS-LABEL-LINES (6) = SPACES
00095            SET WS-NDX            TO 5
00096            GO TO 7275-MOVE-ZIP
00097
00098         ELSE
00099            SET WS-NDX            TO 6.
00100
00101  7275-MOVE-ZIP.
00102
00103      SET WS-NDX2                 TO WS-NDX.
00104      SET WS-NDX2 DOWN BY +1.
00105
00106      IF  WS-LAST-ZIP (WS-NDX2) = SPACES
00107 *****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
00108          IF WS-LABEL-1ST-ZIP (WS-NDX) NUMERIC
00109           IF PI-COMPANY-ID NOT = 'FLA'
00110             MOVE WS-LABEL-ZIP (WS-NDX)
00111                                      TO WS-LAST-ZIP (WS-NDX2)
00112             MOVE SPACES              TO WS-LABEL-LINES (WS-NDX).
00113
00114  7279-EXIT.
00115      EXIT.
00116      EJECT
00117
00118  7280-TRANSLATE-LOWER.
00119      IF LOWER-CASE-LETTERS-USED
00120         MOVE 'N'                     TO WS-STATE-LINE
00121      ELSE
00122         GO TO 7280-EXIT.
00123
00124      IF WS-LABEL-LINES (1) NOT = SPACES
00125         MOVE WS-LABEL-LINES (1)      TO WS-TEMP-AREA2
00126         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
00127         MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (1).
00128
00129      IF WS-LABEL-LINES (2) NOT = SPACES
00130         MOVE WS-LABEL-LINES (2)      TO WS-TEMP-AREA2
00131         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
00132         MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (2).
00133
00134      IF WS-LABEL-LINES (3) NOT = SPACES
00135         MOVE WS-LABEL-LINES (3)      TO WS-TEMP-AREA2
00136         PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
00137         MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (3).
00138
00139 *****THE CITY STATE WILL BE ON LINE FOUR OR FIVE DEPENDING
00140 *****ON THE FORMAT USED.  THE SIXTH LINE BEING BLANK WILL TELL.
00141
00142
00143      IF WS-LABEL-LINES (4) NOT = SPACES
00144         IF (WS-LABEL-LINES (6) = SPACES  OR
00145            WS-LABEL-LINES (5) = SPACES)
00146            MOVE 'Y'                     TO WS-STATE-LINE
00147            MOVE WS-LABEL-LINES (4)      TO WS-TEMP-AREA2
00148            PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
00149            MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (4)
00150         ELSE
00151            MOVE WS-LABEL-LINES (4)      TO WS-TEMP-AREA2
00152            PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
00153            MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (4).
00154
00155      IF WS-LABEL-LINES (5) NOT = SPACES
00156         IF WS-LABEL-LINES (6) NOT = SPACES
00157            MOVE 'Y'                     TO WS-STATE-LINE
00158            MOVE WS-LABEL-LINES (5)      TO WS-TEMP-AREA2
00159            PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
00160            MOVE WS-TEMP-AREA2           TO WS-LABEL-LINES (5).
00161
00162  7280-EXIT.
00163      EXIT.
00164      EJECT
00165
00166  7281-SEARCH-AND-TRANSLATE.
00167      SET TA1  TO +1.
00168
00169  7281-FIND-FIRST-NON-BLANK.
00170      IF WS-TEMP-2 (TA1) = SPACES
00171         SET TA1 UP BY 1
00172         GO TO 7281-FIND-FIRST-NON-BLANK.
00173
00174 *****SET INDEX TO THE NEXT CHAR TO START THE TRANSLATE.
00175      SET TA2   TO TA1
00176      SET TA2   UP BY 1
00177      SET TA21  TO TA2
00178      SET TA21  UP BY 1.
00179
00180      MOVE 'N'                   TO WS-DATA-FOUND-SW.
00181      PERFORM 7282-FIND-NEXT-BLANK THRU 7282-EXIT.
00182
00183  7281-EXIT.
00184      EXIT.
00185      EJECT
00186
00187  7282-FIND-NEXT-BLANK.
00188      IF TA21 GREATER THAN 31
00189         GO TO 7282-EXIT.
00190
00191      IF TA21 EQUAL TO 31
00192         IF NO-CHARACTERS-FOUND
00193            GO TO 7282-EXIT
00194            ELSE
00195            SET TA1   TO +1
00196            MOVE SPACES             TO WS-TEMP-AREA1
00197            PERFORM 7283-MOVE VARYING MOVE-INDX FROM TA2 BY 1
00198                    UNTIL MOVE-INDX EQUAL TO TA21
00199            PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT
00200            SET TA1   TO +1
00201            PERFORM 7284-MOVE-BACK VARYING MOVE-INDX FROM TA2 BY 1
00202                    UNTIL MOVE-INDX EQUAL TO TA21
00203            GO TO 7282-EXIT.
00204
00205      IF WS-TEMP-2 (TA2)  = SPACE OR ',' OR '/'
00206          GO TO 7282-NEXT-GROUP-SEARCH.
00207
00208      IF WS-TEMP-2 (TA21) = SPACES OR ',' OR '/'
102703         IF WS-TEMP-2 (TA21)  = ','
102703            AND WS-PROCESSOR-LINE = 'Y'
102703             SET THE-REST-R-CAPS TO TRUE
102703         ELSE
102703             CONTINUE
00210      ELSE
00211          MOVE 'Y'                  TO WS-DATA-FOUND-SW
00212          SET TA21 UP BY +1
00213          GO TO 7282-FIND-NEXT-BLANK.
00214
00215      SET TA1  TO +1
00216      MOVE SPACES                   TO WS-TEMP-AREA1.
00217      PERFORM 7283-MOVE VARYING MOVE-INDX FROM TA2 BY 1
00218              UNTIL MOVE-INDX EQUAL TO TA21
00219      PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT.
00220      SET TA1  TO +1
00221      PERFORM 7284-MOVE-BACK VARYING MOVE-INDX FROM TA2 BY 1
00222              UNTIL MOVE-INDX EQUAL TO TA21
102703     IF THE-REST-R-CAPS
102703         GO TO 7282-EXIT.
00223      SET TA2   TO TA21
00224      SET TA21  UP BY 1
00225      MOVE 'N'                   TO WS-DATA-FOUND-SW.
00226  7282-NEXT-GROUP-SEARCH.
00227      IF WS-TEMP-2 (TA2) EQUAL TO SPACES OR ',' OR '/'
00228         SET TA2   UP BY 1
00229         IF TA2 = 30
00230            GO TO 7282-EXIT
00231            ELSE
00232            GO TO 7282-NEXT-GROUP-SEARCH.
00233
00234      SET TA2    UP BY 1
00235      SET TA21   TO TA2
00236      SET TA21   UP BY 1
00237      GO TO 7282-FIND-NEXT-BLANK.
00238
00239  7282-EXIT.
00240      EXIT.
00241
00242
00243  7283-MOVE.
00244      MOVE WS-TEMP-2 (MOVE-INDX) TO WS-TEMP-1 (TA1)
00245      SET TA1 UP BY +1.
00246
00247
00248  7284-MOVE-BACK.
00249      MOVE WS-TEMP-1 (TA1)       TO WS-TEMP-2 (MOVE-INDX).
00250      SET TA1 UP BY +1.
00251
00252  7285-TEST-AND-TRANSLATE.
00253 ***BYPASS IF THE AREA MAY BE A PO BOX, OR RR NUMBER
102703     IF WS-TEMP-AREA1 = '.O.' OR 'RR' OR 'O'
00255         GO TO 7285-EXIT.
00256
00257 ***BYPASS IF IT IS A CITY/STATE LINE AND BEYOND CHARACTER 07
00258 ***AND IT APPEARS THAT IT MAY BE A ABREVIATION.
00259
00260      SET WS-POSITION2   TO TA2
00261      SET WS-POSITION21  TO TA21
00262      COMPUTE WS-WORD-LENGTH = WS-POSITION21 - WS-POSITION2
00263      IF WS-WORD-LENGTH LESS THAN 3 AND
00264         WS-STATE-LINE = 'Y' AND
00265         TA2 GREATER THAN 07
00266         GO TO 7285-EXIT.
00267
00268      INSPECT WS-TEMP-AREA1 CONVERTING UPPER-CASE TO LOWER-CASE.
00269
00270  7285-EXIT.
00271      EXIT.
00272
04956                                  EJECT
04957  7290-RESOLVE-CREDITOR.
04958
04959      
      * EXEC CICS HANDLE CONDITION
04960 *         NOTOPEN    (7290-BENE-NOT-OPEN)
04961 *         NOTFND     (7290-EXIT)
04962 *    END-EXEC.
      *    MOVE '"$JI                  ! D #00011065' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4420233030303131303635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04963
04964      MOVE SPACES                 TO WS-LABEL-HOLD-AREA.
04965
04966      MOVE PI-COMPANY-CD          TO BENE-COMP-CD.
04967      MOVE 'B'                    TO BENE-REC-TYPE.
04968      MOVE SPACES                 TO BENE-NUMBER.
04969      MOVE CL-CURRENT-GROUPING    TO BENE-CREDITOR.
04970
04971      
      * EXEC CICS READ
04972 *         DATASET    (BENE-ID)
04973 *         SET        (ADDRESS OF BENEFICIARY-MASTER)
04974 *         RIDFLD     (BENE-KEY)
04975 *    END-EXEC.
      *    MOVE '&"S        E          (   #00011077' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131303737' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 BENE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 BENE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF BENEFICIARY-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04976
04977      IF LOWER-CASE-LETTERS-USED
04978          MOVE BE-MAIL-TO-NAME    TO WS-TEMP-AREA2
04979          PERFORM 7281-SEARCH-AND-TRANSLATE THRU 7281-EXIT
04980          MOVE WS-TEMP-AREA2      TO SS56D
04981      ELSE
04982          MOVE BE-MAIL-TO-NAME    TO SS56D.
04983
04984      GO TO 7290-EXIT.
04985
04986  7290-BENE-NOT-OPEN.
04987
04988      MOVE ER-7675                TO EMI-ERROR.
04989      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04990      MOVE -1                     TO MAINTL.
04991      GO TO 8200-SEND-DATAONLY.
04992
04993  7290-EXIT.
04994      EXIT.
04995
04996      EJECT
04997  7300-VARIABLE-SEARCH.
04998 ***************************************************************
04999 *    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *
05000 *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
05001 *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
05002 *    DATA THAT WAS GENERATED IN PARAGRAPHS 7200-7299.         *
05003 *                                                             *
05004 *    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *
05005 *    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *
05006 *    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *
05007 ***************************************************************
05008
05009      MOVE REC-TEXT (TB-INDX)     TO SINGLE-LINE.
05010
05011      SET INDX1 TO 1.
05012  7301-LOOP.
05013      IF INDX1 > 70
05014         MOVE SINGLE-LINE         TO REC-TEXT (TB-INDX)
05015         GO TO 7399-EXIT.
05016
05017      IF ONE-CHAR (INDX1) NOT = '@'
05018         SET INDX1 UP BY 1
05019         GO TO 7301-LOOP.
05020
05021      SET INDX2 TO INDX1.
05022      SET INDX2 UP BY 1.
05023
05024      IF ONE-CHAR (INDX2) = '@'
05025         SET INDX1 UP BY 2
05026         GO TO 7301-LOOP.
05027
05028      MOVE ONE-CHAR (INDX2)       TO V1.
05029      SET INDX2 UP BY 1.
05030      MOVE ONE-CHAR (INDX2)       TO V2.
05031      SET INDX2 UP BY 1.
05032      MOVE ONE-CHAR (INDX2)       TO V3.
05033      SET INDX2 UP BY 1.
05034      MOVE ONE-CHAR (INDX2)       TO V4.
05035
05036      IF V-NUM NOT NUMERIC
05037         GO TO 7330-VAR-ERROR.
05038
05039      IF V-PERIOD NOT = '.'
05040         MOVE '.'                 TO V-PERIOD
05041         MOVE ZERO                TO V-DECIMAL
05042         GO TO 7340-TABLE-SEARCH.
05043
05044      IF V-DECIMAL NUMERIC
05045         GO TO 7340-TABLE-SEARCH.
05046
05047  7330-VAR-ERROR.
05048      MOVE ER-0180                TO EMI-ERROR.
05049      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05050      SET INDX1 UP BY 1.
05051      GO TO 7301-LOOP.
05052
05053  7340-TABLE-SEARCH.
05054
05055      IF REFRESH-GETMAIN-AREA
05056 *        MOVE SAVE-VARIABLE-POINTER      TO LCP-WS-ADDR-COMP
05057 *        SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
05058          MOVE SYSTEM-SUPPORTED-VARIABLES TO SYSTEM-VARIABLES
05059          MOVE 2                          TO GETMAIN-SWITCH
05060          MOVE 1                          TO SS-COUNTER
                                                  B1
05061      ELSE
05062          IF NO-GETMAIN-DONE-YET
05063 *            EXEC CICS GETMAIN
05064 *                 SET       (ADDRESS OF SYSTEM-VARIABLES)
05065 *                 LENGTH    (SS-WORK-AREA-LENGTH)
05066 *            END-EXEC
05067              MOVE 2              TO GETMAIN-SWITCH
05068 *            SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
05069 *            MOVE LCP-WS-ADDR-COMP
05070 *                                TO SAVE-VARIABLE-POINTER
05071              MOVE SYSTEM-SUPPORTED-VARIABLES
05072                                  TO SYSTEM-VARIABLES
05073              MOVE 1              TO SS-COUNTER
                                          B1
05074          ELSE
05075              MOVE 1              TO SS-COUNTER
                                          B1
05076 *            MOVE SAVE-VARIABLE-POINTER
05077 *                                TO LCP-WS-ADDR-COMP
05078 *            SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
               END-IF
05079      END-IF
           .
05080  7350-TABLE-LOOP.
05081
05082      IF SS-COUNTER  >  SS-NUM-ENTRIES
05083         GO TO 7330-VAR-ERROR.
05084
           MOVE SYSTEM-VARIABLES (B1:106)
                                       TO SYS-VAR-ENTRY
05085      IF SYS-VAR-CODE NOT = VAR-HOLD
              ADD SYS-VAR-LEN          TO B1
05086 *       SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES
05087 *       ADD SYS-VAR-LEN          TO LCP-WS-ADDR-COMP
05088 *       SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR
05089         ADD 1                    TO SS-COUNTER
05090         GO TO 7350-TABLE-LOOP
           END-IF
05091
05092      MOVE SYS-VAR-ENTRY          TO VARIABLE-WORK-AREA.
05093      SET INDXV TO 1.
05094      SUBTRACT 6                  FROM VAR-LEN.
05095      PERFORM 7400-MOVE-VAR-DATA VAR-LEN TIMES.
05096      GO TO 7301-LOOP.
05097
05098  7399-EXIT.
05099       EXIT.
05100
05101  7400-MOVE-VAR-DATA.
05102      IF INDX1 > 70
05103         MOVE ER-0181             TO EMI-ERROR
05104         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05105         GO TO 7301-LOOP.
05106
05107      MOVE VAR-ONE-CHAR (INDXV)   TO ONE-CHAR (INDX1).
05108      SET INDXV UP BY 1.
05109      SET INDX1 UP BY 1.
05110
05111      EJECT
05112  7500-READ-TS.
05113      
      * EXEC CICS HANDLE CONDITION
05114 *         QIDERR     (7590-TS-QIDERR)
05115 *         ITEMERR    (7585-TS-ITEMERR)
05116 *    END-EXEC.
      *    MOVE '"$N<                  ! E #00011228' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4520233030303131323238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05117
05118      SET TS-INDX TO 1.
05119      MOVE 1                      TO TS-ITEM.
05120
05121  7501-LOOP.
05122      
      * EXEC CICS READQ TS
05123 *         INTO     (TS-WORK-AREA)
05124 *         QUEUE    (TS-NAME-TEXT)
05125 *         LENGTH   (TS-LENGTH)
05126 *         ITEM     (TS-ITEM)
05127 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00011237' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-TEXT, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05128
05129      MOVE TS-WORK-AREA           TO TS-GROUP (TS-INDX).
05130      SET TS-INDX UP BY 1.
05131      ADD 1                       TO TS-ITEM.
05132      GO TO 7501-LOOP.
05133
05134  7585-TS-ITEMERR.
05135      IF EIBTRNID NOT = TRANS-ID
05136         SUBTRACT 1               FROM TS-ITEM
05137         MOVE TS-ITEM             TO PI-TEMP-STOR-ITEMS.
05138
05139      GO TO 7599-EXIT.
05140
05141  7590-TS-QIDERR.
05142      IF EIBTRNID = TRANS-ID
05143         MOVE ER-0033             TO EMI-ERROR
05144         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05145         GO TO 8200-SEND-DATAONLY.
05146
05147  7599-EXIT.
05148       EXIT.
05149
05150      EJECT
05151  7600-READ-SCREEN-TS.
05152      
      * EXEC CICS HANDLE CONDITION
05153 *         QIDERR     (7690-TS-QIDERR)
05154 *         ITEMERR    (7685-TS-ITEMERR)
05155 *    END-EXEC.
      *    MOVE '"$N<                  ! F #00011267' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4620233030303131323637' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05156
05157      MOVE 1                      TO TS-ITEM.
05158
05159      
      * EXEC CICS READQ TS
05160 *         INTO    (EL152AO)
05161 *         QUEUE   (TS-NAME-SCREEN)
05162 *         LENGTH  (TS-MAP-LENGTH)
05163 *         ITEM    (TS-ITEM)
05164 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00011274' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-SCREEN, 
                 EL152AO, 
                 TS-MAP-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05165
05166      IF MAINTL NOT = ZEROS
05167         MOVE AL-UANON            TO MAINTA.
05168      IF ARCHNUML NOT = ZEROS
05169         MOVE AL-UNNON            TO ARCHNUMA.
05170      IF FORML NOT = ZEROS
05171         MOVE AL-UANON            TO FORMA.
05172      IF FOLLOWL NOT = ZEROS
05173         MOVE AL-UANON            TO FOLLOWA.
05174      IF RESENDL NOT = ZEROS
05175         MOVE AL-UANON            TO RESENDA.
05176      IF PRINTL NOT = ZEROS
05177         MOVE AL-UANON            TO PRINTA.
05178      IF COPIESL NOT = ZEROS
05179         MOVE AL-UNNON            TO COPIESA.
05180      IF ADDRL NOT = ZEROS
05181         MOVE AL-UANON            TO ADDRA.
05182      IF REL NOT = ZEROS
05183         MOVE AL-UANON            TO REA.
05184
05185      GO TO 7699-EXIT.
05186
05187  7685-TS-ITEMERR.
05188      GO TO 7699-EXIT.
05189
05190  7690-TS-QIDERR.
05191      IF EIBTRNID = TRANS-ID
05192         MOVE ER-0033             TO EMI-ERROR
05193         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05194         GO TO 8200-SEND-DATAONLY.
05195
05196  7699-EXIT.
05197       EXIT.
05198      EJECT
05199
05200  7700-PUT-TEMP-STOR.
05201      PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
05202      PERFORM 7760-DELETE-TEMP-STOR-SCREEN THRU 7760-EXIT.
05203      SET TS-INDX TO 1.
05204      MOVE 0                      TO PI-TEMP-STOR-ITEMS.
05205      PERFORM 7780-WRITE-TS THRU 7780-EXIT
05206              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
05207              UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES.
05208
05209  7749-EXIT.
05210       EXIT.
05211
05212  7750-DELETE-TEMP-STOR.
05213      
      * EXEC CICS HANDLE CONDITION
05214 *         QIDERR      (7750-EXIT)
05215 *    END-EXEC.
      *    MOVE '"$N                   ! G #00011328' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4720233030303131333238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05216
05217      
      * EXEC CICS DELETEQ TS
05218 *         QUEUE       (TS-NAME-TEXT)
05219 *    END-EXEC.
      *    MOVE '*&                    #   #00011332' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-TEXT, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05220
05221  7750-EXIT.
05222      EXIT.
05223
05224  7760-DELETE-TEMP-STOR-SCREEN.
05225      
      * EXEC CICS HANDLE CONDITION
05226 *         QIDERR     (7760-EXIT)
05227 *    END-EXEC.
      *    MOVE '"$N                   ! H #00011340' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4820233030303131333430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05228
05229      
      * EXEC CICS DELETEQ TS
05230 *         QUEUE      (TS-NAME-SCREEN)
05231 *    END-EXEC.
      *    MOVE '*&                    #   #00011344' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-SCREEN, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05232
05233  7760-EXIT.
05234      EXIT.
05235
05236  7770-DELETE-TEMP-STOR-PI-AREA.
05237      
      * EXEC CICS HANDLE CONDITION
05238 *        QIDERR   (7770-EXIT)
05239 *    END-EXEC.
      *    MOVE '"$N                   ! I #00011352' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4920233030303131333532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05240
05241      
      * EXEC CICS DELETEQ TS
05242 *        QUEUE    (WS-PI-QID)
05243 *    END-EXEC.
      *    MOVE '*&                    #   #00011356' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PI-QID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05244
05245  7770-EXIT.
05246      EXIT.
05247
05248      EJECT
05249  7780-WRITE-TS.
05250      MOVE TS-GROUP (TS-INDX)     TO TS-WORK-AREA.
05251      SET TS-INDX UP BY 1.
05252      ADD 1                       TO PI-TEMP-STOR-ITEMS.
05253
05254      
      * EXEC CICS WRITEQ TS
05255 *         FROM    (TS-WORK-AREA)
05256 *         QUEUE   (TS-NAME-TEXT)
05257 *         LENGTH  (TS-LENGTH)
05258 *         ITEM    (PI-TEMP-STOR-ITEMS)
05259 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00011369' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-TEXT, 
                 TS-WORK-AREA, 
                 TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05260
05261  7780-EXIT.
05262      EXIT.
05263
05264      EJECT
05265  7790-WRITE-SCREEN-TS.
05266      MOVE 1                      TO TS-ITEM.
05267
05268      
      * EXEC CICS WRITEQ TS
05269 *         FROM    (EL152AI)
05270 *         QUEUE   (TS-NAME-SCREEN)
05271 *         LENGTH  (TS-MAP-LENGTH)
05272 *         ITEM    (TS-ITEM)
05273 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00011383' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333833' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TS-NAME-SCREEN, 
                 EL152AI, 
                 TS-MAP-LENGTH, 
                 TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05274
05275  7790-EXIT.
05276      EXIT.
05277
05278  7795-WRITE-PI-AREA-TS.
05279      
      * EXEC CICS WRITEQ TS
05280 *        QUEUE    (WS-PI-QID)
05281 *        FROM     (PROGRAM-INTERFACE-BLOCK)
05282 *        LENGTH   (PI-COMM-LENGTH)
05283 *    END-EXEC.
      *    MOVE '*"     L              ''   #00011394' TO DFHEIV0
           MOVE X'2A2220202020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131333934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 WS-PI-QID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05284
05285  7795-EXIT.
05286      EXIT.
05287
05288      EJECT
05289  7800-PRINT-LETTER-NOW.
05290 ***************************************************************
05291 *     THIS ROUTINE WILL CAUSE THE CURRENTLY CREATED LETTER    *
05292 *     TO BE PRINTED ON A HARDCOPY PRINTER.                    *
05293 *        THE TEXT IS EDITED FOR ANY UNRESOLVED SYMBOLS,       *
05294 *        THE PRINTER ID IS OBTAINED FROM THE CONTROL FILE,    *
05295 *        THE LETTER IS SAVED IN TEMP-STORAGE,                 *
05296 *        AND A START IS ISSUED FOR THE PRINT TRANSACTION.     *
05297 ***************************************************************
05298
05299      PERFORM 400-SET-CODES THRU 499-EXIT.
05300
081004     PERFORM 7500-READ-TS        THRU 7599-EXIT
030805     PERFORM 7950-SET-INDX THRU 7950-EXIT.
030805     PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
030805             VARYING SC-INDX FROM 1 BY 1
030805             UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
05324
05301      IF PI-TOTAL-LINES = 0
05302         MOVE ER-0187             TO EMI-ERROR
05303         MOVE -1                  TO MAINTL
05304         MOVE AL-UABON            TO MAINTA
05305         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05306         GO TO 8200-SEND-DATAONLY.
05307
05308      IF COPIESL NOT = ZEROS
05309         IF COPIESI NOT NUMERIC
05310            MOVE ER-0184          TO EMI-ERROR
05311            MOVE -1               TO COPIESL
05312            MOVE AL-UNBON         TO COPIESA
05313            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05314            GO TO 8200-SEND-DATAONLY
05315          ELSE
05316            MOVE COPIESI          TO PI-NUM-PRINT-COPIES
05317        ELSE
05318         MOVE 1                   TO PI-NUM-PRINT-COPIES.
05319
081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT
05320 *    PERFORM 7950-SET-INDX THRU 7950-EXIT.
05321 *    PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
05322 *            VARYING SC-INDX FROM 1 BY 1
05323 *            UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
05324
05325      MOVE +0                     TO TALLY.
05326      INSPECT RECORD-TABLE TALLYING TALLY
05327                                  FOR CHARACTERS BEFORE '@'.
05328
05329      IF TALLY < +21900
05330         COMPUTE PI-CURRENT-LINE = TALLY / 73
05331         MOVE ZEROS               TO ROLL-COUNTER
05332         MOVE ER-0191             TO EMI-ERROR
05333         MOVE -1                  TO MAINTL
05334         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05335         GO TO 7900-ROLL-PAGE.
05336
05337  7800-UPDATE-CLAIM.
05338
05339      IF PI-RETURN-TO-PROGRAM = PGM-EL126
05340          GO TO 7800-GET-PRINTER.
05341
05342      MOVE PI-COMPANY-CD          TO  CLAM-CO.
05343      MOVE PI-CARRIER             TO  CLAM-CARRIER.
05344      MOVE PI-CLAIM-NO            TO  CLAM-CLAIM.
05345      MOVE PI-CERT-NO             TO  CLAM-CERT-NUM.
05346
05347      
      * EXEC CICS HANDLE CONDITION
05348 *        NOTOPEN   (8860-CLAM-NOT-OPEN)
05349 *        NOTFND    (7800-CLAIM-NOT-FOUND)
05350 *    END-EXEC.
      *    MOVE '"$JI                  ! J #00011469' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4A20233030303131343639' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05351
05352      
      * EXEC CICS READ
05353 *        DATASET   (CLAM-ID)
05354 *        RIDFLD    (CLAM-KEY)
05355 *        SET       (ADDRESS OF CLAIM-MASTER)
05356 *        UPDATE
05357 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00011474' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CLAM-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CLAIM-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05358
05359      MOVE PI-PROCESSOR-ID        TO  CL-LAST-MAINT-USER.
05360      MOVE EIBTIME                TO  CL-LAST-MAINT-HHMMSS.
05361      MOVE CURRENT-SAVE           TO  CL-LAST-MAINT-DT.
05362      MOVE '2'                    TO  CL-LAST-MAINT-TYPE.
05363
05364      
      * EXEC CICS HANDLE CONDITION
05365 *        DUPKEY    (7800-GET-PRINTER)
05366 *    END-EXEC.
      *    MOVE '"$$                   ! K #00011486' TO DFHEIV0
           MOVE X'222424202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4B20233030303131343836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05367
05368      
      * EXEC CICS REWRITE
05369 *        DATASET   (CLAM-ID)
05370 *        FROM      (CLAIM-MASTER)
05371 *    END-EXEC.
           MOVE LENGTH OF
            CLAIM-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %   #00011490' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131343930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CLAM-ID, 
                 CLAIM-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05372
05373      GO TO 7800-GET-PRINTER.
05374
05375  7800-CLAIM-NOT-FOUND.
05376
05377      MOVE ER-0133                TO  EMI-ERROR.
05378      MOVE -1                     TO  MAINTL.
05379      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05380      GO TO 8200-SEND-DATAONLY.
05381
05382  7800-GET-PRINTER.
05383
05384      IF PI-ALT-PRINTER-ID NOT = SPACES
05385         GO TO 7800-CNTL-BYPASS.
05386
05387      MOVE '1'                    TO CNTL-RECORD-TYPE.
05388      MOVE SPACES                 TO CNTL-GENL.
05389      MOVE ZEROS                  TO CNTL-SEQ.
05390
05391      
      * EXEC CICS HANDLE CONDITION
05392 *         NOTOPEN    (8840-CNTL-NOT-OPEN)
05393 *         NOTFND     (7890-NOT-FOUND)
05394 *    END-EXEC.
      *    MOVE '"$JI                  ! L #00011513' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4C20233030303131353133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05395
05396      
      * EXEC CICS READ
05397 *         DATASET    (CNTL-ID)
05398 *         SET        (ADDRESS OF CONTROL-FILE)
05399 *         RIDFLD     (CNTL-KEY)
05400 *    END-EXEC.
      *    MOVE '&"S        E          (   #00011518' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05401
042605*    MOVE CF-FORMS-PRINTER-ID       TO TS-TERM-TEXT.
042605     MOVE CF-FORMS-PRINTER-ID    TO WS-PRINTER-ID
05403      GO TO 7800-SET-UP-TS.
05404
05405  7800-CNTL-BYPASS.
042605*    MOVE PI-ALT-PRINTER-ID         TO TS-TERM-TEXT.
042605     MOVE PI-ALT-PRINTER-ID      TO WS-PRINTER-ID.
05407
05408  7800-SET-UP-TS.
05409
05410 ***********************************************************
05411 *      CHECK TO SEE IF IT IS A PRINT REQUEST FOR PRINTING *
05412 *      LETTERS ON A 3275 PRINTER. IF SO, SAVE THE SCREEN  *
05413 ***********************************************************
05414
05415      IF TS-TERM-PREFIX = 'DU'
05416         PERFORM 7760-DELETE-TEMP-STOR-SCREEN THRU 7760-EXIT
05417         PERFORM 7790-WRITE-SCREEN-TS THRU 7790-EXIT.
05418
05419      SET TS-INDX TO 1.
05420
05421      
      * EXEC CICS ASKTIME
05422 *    END-EXEC.
      *    MOVE '0"                    "   #00011545' TO DFHEIV0
           MOVE X'302220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353435' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05423
05424      MOVE EIBTIME                TO TS-ID-TIME.
042605*    MOVE '152A'                 TO TS-ID-TEXT
05425      MOVE TS-NAME-TEXT           TO PI-TEMP-STOR-ID.
05426      MOVE 0                      TO PI-TEMP-STOR-ITEMS.
05427
05428      PERFORM 7780-WRITE-TS THRU 7780-EXIT
05429              VARYING TS-GROUP-WORK FROM 0 BY TS-NUM-REC-IN-GROUP
05430              UNTIL TS-GROUP-WORK NOT < PI-TOTAL-LINES
05431
05432      IF NOT PI-SHOW-MODE
05433         MOVE '1'                 TO PI-PRINT-SW.
05434
05435      
      * EXEC CICS HANDLE CONDITION
05436 *         TERMIDERR  (8820-TERM-ERROR)
05437 *         TRANSIDERR (8830-TRAN-ERROR)
05438 *    END-EXEC.
      *    MOVE '"$[\                  ! M #00011560' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4D20233030303131353630' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05439
121802*    IF PI-COMPANY-ID = 'DMD' OR 'XXX'
05441 *        MOVE EIBTRMID    TO TS-TERM-TEXT
121802*        EXEC CICS START
121802*             INTERVAL    (0)
121802*             TRANSID     (PRINT-TRANS)
121802*             FROM        (PROGRAM-INTERFACE-BLOCK)
121802*             LENGTH      (PI-COMM-LENGTH)
05447 *             TERMID      (TS-TERM-TEXT)
121802*        END-EXEC
121802*    ELSE
05450          
      * EXEC CICS START
05451 *             INTERVAL    (0)
05452 *             TRANSID     (PRINT-TRANS)
05453 *             FROM        (PROGRAM-INTERFACE-BLOCK)
05454 *             LENGTH      (PI-COMM-LENGTH)
042605*             TERMID      (TS-TERM-TEXT)
042605*             TERMID      (WS-PRINTER-ID)
05456 *        END-EXEC.
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILFT                1   #00011575' TO DFHEIV0
           MOVE X'3028494C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303131353735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 PRINT-TRANS, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 WS-PRINTER-ID, 
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
           
05457
050505     MOVE '104A'                 TO TS-ID-TEXT
05458      MOVE ER-0189                TO EMI-ERROR.
05459      MOVE -1                     TO MAINTL.
05460      PERFORM 9900-ERROR-FORMAT  THRU  9900-EXIT.
042605     MOVE ' PRINT TRANS STARTED ' TO EMI-MESSAGE-AREA (1)
042605     MOVE PI-TEMP-STOR-ID TO EMI-MESSAGE-AREA (1) (25:10)
05461
05462      GO TO 8200-SEND-DATAONLY.
05463
05464  7890-NOT-FOUND.
05465      MOVE ER-0190                TO EMI-ERROR.
05466      MOVE -1                     TO MAINTL.
05467      PERFORM 9900-ERROR-FORMAT  THRU 9900-EXIT.
05468      GO TO 8200-SEND-DATAONLY.
05469
05470      EJECT
05471  7900-ROLL-PAGE.
05472      IF ENTERPFL NOT = ZEROS
05473         MOVE -1                  TO ENTERPFL
05474        ELSE
05475         MOVE -1                  TO MAINTL.
05476
110404     PERFORM 7500-READ-TS     THRU 7599-EXIT
05499      PERFORM 7950-SET-INDX    THRU 7950-EXIT
05500      PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
05501         VARYING SC-INDX FROM 1 BY 1 UNTIL
05502         SC-INDX > NUM-LINES-PER-SCREEN
05477      IF PI-TOTAL-LINES = 0
05478         MOVE ER-0047             TO EMI-ERROR
05479         MOVE -1                  TO MAINTL
05480         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05481         GO TO 8200-SEND-DATAONLY.
05482
05483      COMPUTE TEMP-CURR-LINE = PI-CURRENT-LINE + ROLL-COUNTER.
05484
05485      IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
05486         MOVE ER-0067             TO EMI-ERROR
05487         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05488         MOVE 1                   TO TEMP-CURR-LINE.
05489
05490      IF TEMP-CURR-LINE > PI-TOTAL-LINES
05491         MOVE ER-0066             TO EMI-ERROR
05492         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05493         COMPUTE TEMP-CURR-LINE
05494             = PI-TOTAL-LINES + 1 - NUM-LINES-PER-SCREEN
05495         IF TEMP-CURR-LINE NEGATIVE OR TEMP-CURR-LINE = ZEROS
05496             MOVE 1 TO TEMP-CURR-LINE.
05497
05498 *    IF EMI-ERROR NOT = ER-0191
110404*       PERFORM 7500-READ-TS     THRU 7599-EXIT
05499 *       PERFORM 7950-SET-INDX    THRU 7950-EXIT
05500 *       PERFORM 7960-UPDATE-TABLE-FROM-SCREEN THRU 7960-EXIT
05501 *                VARYING SC-INDX FROM 1 BY 1 UNTIL
05502 *                SC-INDX > NUM-LINES-PER-SCREEN
      *    END-IF
05503
05504      IF EMI-ERROR = ER-0066 OR ER-0067 OR ER-0000 OR ER-0191
05505         NEXT SENTENCE
05506      ELSE
05507         GO TO 8200-SEND-DATAONLY.
05508
110404*    PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
05509      MOVE TEMP-CURR-LINE         TO PI-CURRENT-LINE.
05510      SET TB-INDX                 TO PI-CURRENT-LINE.
05511      MOVE LOW-VALUES             TO EL152RI.
05512
081004*    PERFORM 7500-READ-TS        THRU 7599-EXIT
05513      PERFORM 7170-FORMAT-SCREEN THRU 7170-EXIT
05514              VARYING SC-INDX FROM 1 BY 1
05515              UNTIL SC-INDX > NUM-LINES-PER-SCREEN.
05516
05517      GO TO 8200-SEND-DATAONLY.
05518
05519      EJECT
05520  7950-SET-INDX.
05521      IF PI-CURRENT-LINE = 0
05522         SET TB-INDX TO 1
05523        ELSE
05524         SET TB-INDX              TO PI-CURRENT-LINE.
05525
05526  7950-EXIT.
05527       EXIT.
05528
05529      EJECT
05530  7960-UPDATE-TABLE-FROM-SCREEN.
05531      IF SC-TEXTL (SC-INDX) NOT = ZEROS
05532         IF TB-INDX > PI-TOTAL-LINES
05533            IF PI-TOTAL-LINES = MAX-LINES
05534               MOVE ER-0051       TO EMI-ERROR
05535               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05536               GO TO 8200-SEND-DATAONLY
05537            ELSE
05538               MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX)
05539               ADD 1              TO PI-TOTAL-LINES
05540         ELSE
05541            MOVE SC-TEXT (SC-INDX) TO REC-TEXT (TB-INDX).
05542
05543      SET TB-INDX UP BY 1.
05544
05545  7960-EXIT.
05546       EXIT.
05547
05548      EJECT
05549  8000-STARTBR-ERACCT.
05550
05551      IF WS-ACCT-READ-SW = 'Y'
05552          NEXT SENTENCE
05553      ELSE
05554          
      * EXEC CICS GETMAIN
05555 *             SET       (ADDRESS OF ACCOUNT-MASTER)
05556 *             LENGTH    (ACCT-LENGTH)
05557 *        END-EXEC.
      *    MOVE '," L                  $   #00011692' TO DFHEIV0
           MOVE X'2C22204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV20, 
                 ACCT-LENGTH, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05558
05559      
      * EXEC CICS STARTBR
05560 *        RIDFLD      (ACCT-KEY)
05561 *        DATASET     (ACCT-ID)
05562 *        KEYLENGTH   (20)
05563 *        GENERIC
05564 *    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00011697' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303131363937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05565
05566      MOVE 'Y'         TO ACCT-BROWSE-STARTED.
05567  8000-STARTBR-EXIT.
05568      EXIT.
05569
05570  8000-READNEXT-ERACCT.
05571
05572      
      * EXEC CICS READNEXT
05573 *        DATASET   (ACCT-ID)
05574 *        INTO      (ACCOUNT-MASTER)
05575 *        RIDFLD    (ACCT-KEY)
05576 *    END-EXEC.
           MOVE LENGTH OF
            ACCOUNT-MASTER
             TO DFHEIV12
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )   #00011710' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 ACCOUNT-MASTER, 
                 DFHEIV12, 
                 ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05577
05578  8000-READNEXT-EXIT.
05579      EXIT.
05580
05581      EJECT
121802*8050-STARTBR-EMPROD. Remove as dead code
121802*8050-EXIT. Remove as dead code
121802*8060-READNEXT-EMPROD. Remove as dead code
121802*8060-EXIT. Remove as dead code
05614
05615      EJECT
05616  8100-SEND-INITIAL-MAP.
081004     PERFORM 7700-PUT-TEMP-STOR  THRU 7749-EXIT
05617      MOVE SAVE-DATE              TO DATEAO.
05618      MOVE EIBTIME                TO TIME-IN.
05619      MOVE TIME-OUT               TO TIMEAO.
05620
           MOVE PI-CARRIER             TO CARRO
           MOVE PI-CLAIM-NO            TO CLMNOO
           MOVE PI-CERT-NO             TO CRTNOO
           MOVE PI-PROCESSOR-ID        TO PROCO
           MOVE PI-COMPANY-ID          TO COMPIDO
           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE PI-ENCLOSURE-CD        TO ENCO
05621      IF NOT EMI-NO-ERRORS
05622          SET EMI-INDX TO 1
05623          MOVE EMI-MESSAGE-AREA (EMI-INDX) TO ERRMSGO
05624      ELSE
05625          MOVE SPACES             TO ERRMSGO.
05626
05627      MOVE -1                     TO MAINTL.
05628
05629      
      * EXEC CICS SEND
05630 *        MAP      (MAP-NAME)
05631 *        MAPSET   (MAPSET-NAME)
05632 *        FROM     (EL152AO)
05633 *        ERASE
05634 *        CURSOR
05635 *    END-EXEC.
           MOVE LENGTH OF
            EL152AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00011748' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373438' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AO, 
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
           
05636
05637      PERFORM 8210-ENDBR  THRU 8210-EXIT.
05638      PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.
05639
081004     GO TO 9000-RETURN-TRANS.
081004*    GO TO 0200-RECEIVE.
05641
05642  8200-SEND-DATAONLY.
081004     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
           MOVE PI-ENCLOSURE-CD        TO ENCO
05643      MOVE SAVE-DATE              TO DATEAO.
05644      MOVE EIBTIME                TO TIME-IN.
05645      MOVE TIME-OUT               TO TIMEAO.
05646      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
05647
121802*    IF PI-COMPANY-ID = 'DMD'
121802*       IF FORCE-7840
121802*          MOVE '7840 FORCED' TO PFKEY9O
121802*          MOVE AL-SABON      TO PFKEY9A.
05652
05653      
      * EXEC CICS SEND
05654 *        MAP      (MAP-NAME)
05655 *        MAPSET   (MAPSET-NAME)
05656 *        FROM     (EL152AO)
05657 *        DATAONLY
05658 *        CURSOR
05659 *    END-EXEC.
           MOVE LENGTH OF
            EL152AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00011775' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AO, 
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
           
05660
05661      PERFORM 8210-ENDBR  THRU 8210-EXIT.
05662      PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.
05663
081004     GO TO 9000-RETURN-TRANS.
081004*    GO TO 0200-RECEIVE.
05665
05666  8200-SEND-DATAONLY-ERASEAUP.
081004     PERFORM 7700-PUT-TEMP-STOR   THRU 7749-EXIT
           MOVE PI-ENCLOSURE-CD        TO ENCO
05667      MOVE SAVE-DATE              TO DATEAO.
05668      MOVE EIBTIME                TO TIME-IN.
05669      MOVE TIME-OUT               TO TIMEAO.
05670      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO
05671      
      * EXEC CICS SEND
05672 *        MAP      (MAP-NAME)
05673 *        MAPSET   (MAPSET-NAME)
05674 *        FROM     (EL152AO)
05675 *        DATAONLY
05676 *        ERASEAUP
05677 *        CURSOR
05678 *    END-EXEC.
           MOVE LENGTH OF
            EL152AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00011796' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 MAP-NAME, 
                 EL152AO, 
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
           
05679
05680      PERFORM 8210-ENDBR  THRU 8210-EXIT.
05681      PERFORM 8220-ENDBR-EMPROD THRU 8220-EXIT.
05682
081004     GO TO 9000-RETURN-TRANS.
081004*    GO TO 0200-RECEIVE.
05684
05685  8210-ENDBR.
05686      IF ACCT-BROWSE-STARTED = 'Y'
05687         MOVE 'N'                  TO ACCT-BROWSE-STARTED
05688         
      * EXEC CICS ENDBR
05689 *            DATASET    (ACCT-ID)
05690 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011814' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383134' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 ACCT-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05691
05692  8210-EXIT.
05693      EXIT.
05694
05695  8220-ENDBR-EMPROD.
05696      IF PROD-BROWSE-STARTED = 'Y'
05697         MOVE 'N'                  TO PROD-BROWSE-STARTED
05698         
      * EXEC CICS ENDBR
05699 *            DATASET    (PROD-ID)
05700 *       END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00011824' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROD-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05701
05702  8220-EXIT.
05703      EXIT.
05704
05705  8300-SEND-TEXT.
05706      
      * EXEC CICS SEND TEXT
05707 *        FROM    (LOGOFF-TEXT)
05708 *        LENGTH  (LOGOFF-LENGTH)
05709 *        ERASE
05710 *        FREEKB
05711 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00011832' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383332' TO DFHEIV0(25:11)
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
           
05712
05713      
      * EXEC CICS RETURN
05714 *    END-EXEC.
      *    MOVE '.(                    ''   #00011839' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383339' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05715
05716  8600-DEEDIT.
05717      
      * EXEC CICS BIF DEEDIT
05718 *         FIELD    (DEEDIT-FIELD)
05719 *         LENGTH   (15)
05720 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00011843' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05721
05722      EJECT
05723  8800-UNAUTHORIZED-ACCESS.
05724      MOVE UNACCESS-MSG           TO LOGOFF-MSG.
05725      GO TO 8300-SEND-TEXT.
05726
05727  8810-PF23.
05728      PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
05729      MOVE ZEROS                  TO PI-TOTAL-LINES.
05730      MOVE ZEROS                  TO PI-CURRENT-LINE.
05731      MOVE EIBAID                 TO PI-ENTRY-CD-1.
05732      MOVE XCTL-005               TO PGM-NAME.
05733      GO TO 9300-XCTL.
05734
05735  8820-TERM-ERROR.
05736      MOVE ER-0412                TO EMI-ERROR.
05737      MOVE SPACES                 TO PI-PRINT-SW.
05738      GO TO 8999-OPEN-ERROR.
05739
05740  8830-TRAN-ERROR.
05741      MOVE ER-0413                TO EMI-ERROR.
05742      MOVE SPACES                 TO PI-PRINT-SW.
05743      GO TO 8999-OPEN-ERROR.
05744
05745  8840-CNTL-NOT-OPEN.
05746      MOVE ER-0042                TO EMI-ERROR.
05747      GO TO 8999-OPEN-ERROR.
05748
05749  8850-ARCH-NOT-OPEN.
05750      MOVE ER-0332                TO EMI-ERROR.
05751      GO TO 8999-OPEN-ERROR.
05752
05753  8860-CLAM-NOT-OPEN.
05754      MOVE ER-0154                TO EMI-ERROR.
05755      GO TO 8999-OPEN-ERROR.
05756
05757  8870-ACTV-NOT-OPEN.
05758      MOVE ER-0172                TO EMI-ERROR.
05759      GO TO 8999-OPEN-ERROR.
05760
05761  8880-ACCT-NOT-OPEN.
05762      MOVE ER-0168                TO EMI-ERROR.
05763      GO TO 8999-OPEN-ERROR.
05764
05765  8890-TEXT-NOT-OPEN.
05766      MOVE ER-0013                TO EMI-ERROR.
05767      GO TO 8999-OPEN-ERROR.
05768
05769  8900-CERT-NOT-OPEN.
05770      MOVE ER-0169                TO EMI-ERROR.
05771      GO TO 8999-OPEN-ERROR.
05772
05773  8910-CERT-NOT-FOUND.
05774      MOVE ER-0206                TO EMI-ERROR.
05775      GO TO 8999-OPEN-ERROR.
05776
05777  8915-PROD-NOT-OPEN.
05778      MOVE ER-9106                TO EMI-ERROR.
05779      GO TO 8999-OPEN-ERROR.
05780
05781  8920-PLCY-NOT-OPEN.
05782      MOVE ER-9883                TO EMI-ERROR.
05783      GO TO 8999-OPEN-ERROR.
05784
05785  8925-PLAN-NOT-OPEN.
05786      MOVE ER-9808                TO EMI-ERROR.
05787      GO TO 8999-OPEN-ERROR.
05788
05789  8930-PLCY-NOT-FOUND.
05790      MOVE ER-9483                TO EMI-ERROR.
05791      GO TO 8999-OPEN-ERROR.
05792
05793  8999-OPEN-ERROR.
05794      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05795      MOVE -1                     TO MAINTL.
05796      GO TO 8200-SEND-DATAONLY.
05797
081004 9000-RETURN-TRANS.
081004
081004     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
081004     
      * EXEC CICS RETURN
081004*        TRANSID    (TRANS-ID)
081004*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
081004*        LENGTH     (PI-COMM-LENGTH)
081004*    END-EXEC.
      *    MOVE '.(CT                  ''   #00011927' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 TRANS-ID, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
081004
081004
081004 9000-EXIT.
081004     EXIT.
05798  9200-RETURN-MAIN-MENU.
05799      PERFORM 7750-DELETE-TEMP-STOR THRU 7750-EXIT.
05800
05801      MOVE ZEROS                  TO PI-TOTAL-LINES
05802                                     PI-CURRENT-LINE.
05803      MOVE XCTL-126               TO PGM-NAME.
05804
05805      GO TO 9300-XCTL.
05806
05807  9300-XCTL.
05808      
      * EXEC CICS XCTL
05809 *        PROGRAM    (PGM-NAME)
05810 *        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
05811 *        LENGTH     (PI-COMM-LENGTH)
05812 *    END-EXEC.
      *    MOVE '.$C                   %   #00011946' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05813
05814  9400-CLEAR.
05815      IF PI-CLEAR-MODE
05816          MOVE PI-RETURN-TO-PROGRAM TO PGM-NAME
05817          GO TO 9300-XCTL
05818      ELSE
05819          PERFORM 7750-DELETE-TEMP-STOR         THRU 7750-EXIT
05820          PERFORM 7760-DELETE-TEMP-STOR-SCREEN  THRU 7760-EXIT
05821          PERFORM 7770-DELETE-TEMP-STOR-PI-AREA THRU 7770-EXIT
05822          MOVE SPACES             TO PI-WA
05823                                     RECORD-TABLE
05824                                     TS-WORK-AREA
05825          MOVE ZEROS              TO PI-TOTAL-LINES
05826                                     PI-CURRENT-LINE
05827                                     PI-TEMP-STOR-ITEMS
05828                                     PI-UPDATE-SW
05829                                     PI-ADDR-SEQ
05830          MOVE '2'                TO PI-ACTION
05831          MOVE LOW-VALUES         TO EL152AO
05832                                     PI-ADDR-TYPE
05833          SET TS-INDX
05834              TA1
05835              TA2
05836              TA21
05837              TB-INDX
05838              TB-INDX1
05839              MOVE-INDX
05840              SC-INDX             TO +1
05841          IF GETMAIN-SWITCH = '2'
05842              MOVE 1              TO GETMAIN-SWITCH
05843              GO TO 8100-SEND-INITIAL-MAP
05844          ELSE
05845              GO TO 8100-SEND-INITIAL-MAP.
05846
05847  9600-PGMID-ERROR.
05848      
      * EXEC CICS HANDLE CONDITION
05849 *        PGMIDERR    (8300-SEND-TEXT)
05850 *    END-EXEC.
      *    MOVE '"$L                   ! N #00011986' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'4E20233030303131393836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05851
05852      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.
05853      MOVE ' '                    TO PI-ENTRY-CD-1.
05854      MOVE XCTL-005               TO PGM-NAME.
05855      MOVE PGM-NAME               TO LOGOFF-PGM.
05856      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
05857      GO TO 9300-XCTL.
05858
05859  9700-DATE-LINK.
05860      MOVE LINK-ELDATCV           TO PGM-NAME.
05861      
      * EXEC CICS LINK
05862 *        PROGRAM   (PGM-NAME)
05863 *        COMMAREA  (DATE-CONVERSION-DATA)
05864 *        LENGTH    (DC-COMM-LENGTH)
05865 *    END-EXEC.
      *    MOVE '."C                   (   #00011999' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05866
121802*    IF NO-CONVERSION-ERROR
121802*            AND
121802*       W-REVERSE-DATE
121802*            AND
121802*       PI-COMPANY-ID = 'AUK'
121802*           MOVE DC-GREG-DATE-1-EDIT TO W-EDIT-DATE-1
121802*           MOVE W-ED1-MM            TO W-ED2-MM
121802*           MOVE W-ED1-DD            TO W-ED2-DD
121802*           MOVE W-ED1-YY            TO W-ED2-YY
121802*           MOVE W-EDIT-DATE-2       TO DC-GREG-DATE-1-EDIT.
05877
05878  9700-EXIT.
05879       EXIT.
05880
05881  9900-ERROR-FORMAT.
05882      IF NOT EMI-ERRORS-COMPLETE
05883          MOVE LINK-001           TO PGM-NAME
05884          
      * EXEC CICS LINK
05885 *            PROGRAM   (PGM-NAME)
05886 *            COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)
05887 *            LENGTH    (EMI-COMM-LENGTH)
05888 *        END-EXEC.
      *    MOVE '."C                   (   #00012022' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05889
05890  9900-EXIT.
05891      EXIT.
05892
05893  9990-ABEND.
05894      MOVE LINK-004               TO PGM-NAME.
05895      MOVE DFHEIBLK               TO EMI-LINE1.
05896      
      * EXEC CICS LINK
05897 *        PROGRAM   (PGM-NAME)
05898 *        COMMAREA  (EMI-LINE1)
05899 *        LENGTH    (72)
05900 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00012034' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303334' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05901
05902      GO TO 8200-SEND-DATAONLY.
05903
05904  9995-SECURITY-VIOLATION.
05905 *                            COPY ELCSCTP.
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
      *    MOVE '."C                   (   #00012060' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303630' TO DFHEIV0(25:11)
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
05906

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL152' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9600-PGMID-ERROR,
                     0325-MAPFAIL,
                     9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9400-CLEAR,
                     0100-PA,
                     0100-PA,
                     0100-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 0325-MAPFAIL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 590-QIDERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 8850-ARCH-NOT-OPEN,
                     1020-ENDBR,
                     1020-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 2120-ENDBR,
                     2120-ENDBR,
                     8890-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8860-CLAM-NOT-OPEN,
                     3010-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 9990-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 6499-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 6599-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8860-CLAM-NOT-OPEN,
                     7090-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8880-ACCT-NOT-OPEN,
                     7080-ACCT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 7070-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 7070-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8870-ACTV-NOT-OPEN,
                     7070-ACTV-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 7075-CONTINUE-ACTV-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 8900-CERT-NOT-OPEN,
                     8910-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8870-ACTV-NOT-OPEN,
                     7200-READ-LOAN-NUMBER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8870-ACTV-NOT-OPEN,
                     7201-READ-BENEFICIARY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 7290-BENE-NOT-OPEN,
                     7205-READ-PHYSICIAN-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 8870-ACTV-NOT-OPEN,
                     7210-READ-EMPLOYER-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8870-ACTV-NOT-OPEN,
                     7220-READ-INSURED-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 8870-ACTV-NOT-OPEN,
                     7225-READ-OTHER1-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 8870-ACTV-NOT-OPEN,
                     7230-READ-OTHER2-ADDR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 8870-ACTV-NOT-OPEN,
                     7240-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 8870-ACTV-NOT-OPEN,
                     7250-READ-ACCOUNT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 8880-ACCT-NOT-OPEN,
                     7251-READ-3RD-PARTY
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 8870-ACTV-NOT-OPEN,
                     7252-READ-COMP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 7260-READ-DENIAL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 31
               GO TO 8870-ACTV-NOT-OPEN,
                     7265-READ-CNTL1,
                     7265-READ-CNTL1
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 32
               GO TO 8840-CNTL-NOT-OPEN,
                     7266-READ-CNTL2
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 33
               GO TO 8840-CNTL-NOT-OPEN,
                     7267-READ-CNTL4
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 34
               GO TO 8840-CNTL-NOT-OPEN,
                     7267-READ-CNTL6
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 35
               GO TO 8840-CNTL-NOT-OPEN,
                     7268-SET-DATE
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 36
               GO TO 7290-BENE-NOT-OPEN,
                     7290-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 37
               GO TO 7590-TS-QIDERR,
                     7585-TS-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 38
               GO TO 7690-TS-QIDERR,
                     7685-TS-ITEMERR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 39
               GO TO 7750-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 40
               GO TO 7760-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 41
               GO TO 7770-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 42
               GO TO 8860-CLAM-NOT-OPEN,
                     7800-CLAIM-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 43
               GO TO 7800-GET-PRINTER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 44
               GO TO 8840-CNTL-NOT-OPEN,
                     7890-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 45
               GO TO 8820-TERM-ERROR,
                     8830-TRAN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 46
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL152' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
