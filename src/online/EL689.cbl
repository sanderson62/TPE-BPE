      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL689 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 04/20/94 15:10:07.
00007 *                            VMOD=2.037
00008 *
00009 *AUTHOR.     LOGIC,INC.
00010 *            DALLAS, TEXAS.
00012 *DATE-COMPILED.
00024 *REMARKS.    TRANSACTION - EXH3 - CREDIT LETTER WRITER.
00023 *
031504******************************************************************
031504
031504* W-FILE-USED REFERENCE LIST:
031504* (1)  ELCNTL - COMPANY
031504* (2)  ELCNTL - LIFE BENEFIT
031504* (3)  ELCNTL - A&H BENEFIT
031504* (4)  ELCNTL - CARRIER
031504* (5)  MAIL
031504* (6)  ERACCT
031504* (7)  USED FOR SYSTEM VARIABLES ?
031504* (8)  ELCERT
031504* (9)  PENDING BUSINESS
031504* (10) COMPENSATION
031504* (11) ELCNTL - PROCESSOR DATA ?
031504* (12) ERCHEK
031504* (13) ERPYAJ
031504* (14) NOT USED
031504* (15) NOT USED
031504* (16) NOT USED
031504* (17) NOT USED
031504* (18) NOT USED
031504* (19) NOT USED
031504* (20) NOT USED
031504*
101501******************************************************************
101501*                   C H A N G E   L O G
101501*
101501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101501*-----------------------------------------------------------------
101501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101501* EFFECTIVE    NUMBER
101501*-----------------------------------------------------------------
101501* 101501    2001100100006  SMVA  ADD USERID to screen header
101501*                                ADJUSTED REDEFINES EL689AI FILLER
031504* 031504    2004020600009  SMVA  PULL MOST CURRENT ACCT ADDRESS
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
100705* 100705    2004072800003  PEMA  ADD NEW VARIABLES
072308* 072308    2008062700001  PEMA  ADD NEW VARIABLES
092908* 092908    2008013100006  PEMA  MOVE VAR 154
102408* 102408  CR2008091000001  PEMA  MOVE PNDB VARS TO CERT VARS
051209* 051209  CR2009021700003  PEMA  ADD CRED BENE VARIABLES
051209* 051209  CR2009042700002  PEMA  ADD ERACCT ACCT NUMBER VARIABLE
022510* 022510  IR2009062300003  PEMA  ADD ENDFILE TEST TO ERACCT READ
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
041811* 041811  CR2011010400001  PEMA  DISABLE PF8 FOR CERTAIN USERS
042011* 042011  CR2011010400001  PEMA  FIX CRED BENE ADDRESS LINES
061411* 061411  CR2011061300001  PEMA  ADD ALWA TO PF8 DISABLE LIST
092911* 092911  IR2011092900001  AJRA  CLEAR LABEL LINES BEFORE CRED BEN
022712* 022712  IR2012022700001  AJRA  DON'T COUNT &&&&&& IN LINES
122011* 122011  CR2011022800001  AJRA  NAPERSOFT
090612* 090612  IR2012090600001  AJRA  NAPERSOFT
091712* 091712  IR2012091400001  AJRA  NAPERSOFT CERT ID
091912* 091912  IR2012091900001  AJRA  ALLOW FOLLOW UP DATE TO = CURR DT
101812* 101812  CR2012101700002  AJRA  ADD ENDARCH AND SCREENID
110512* 110512  CR2012101700002  AJRA  EXIT LETTER WRITER AFTER PF7
120512* 120512  CR2012101700002  AJRA  ADD RE ENCL CODE
011013* 011013  CR2012101700002  AJRA  CHECK REQ REASON CODE IND
012413* 012413  CR2013012100001  AJRA  ADD AUTO BILLING NOTE
121213* 121213  CR2013090300001  AJRA  VALIDATE ENC CODE AGAINST ELENCC
123113* 123113  CR2013090300001  AJRA  USE NEXT BUS DT FOR RESEND DT CAL
010814* 010814  CR2013090300001  AJRA  CALL SQL STORED PROC FOR NEXT BUS
021214* 021214  IR2014021200002  AJRA  ADD SQL DISCONNECT
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
062017* 062017  CR2015091000001  PEMA  ADD PROCESSING FOR TN REF INTERES
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
101501******************************************************************
00025
00028  DATA DIVISION.
00029  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
pemuni 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.
pemuni 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP
pemuni                                   USAGE POINTER.
00033  77  FILLER  PIC X(32) VALUE '********************************'.
00034  77  FILLER  PIC X(32) VALUE '*    EL689 WORKING STORAGE     *'.
00035  77  FILLER  PIC X(32) VALUE '*********** VMOD=2.037 *********'.
010814
010814 EXEC SQL
010814    INCLUDE SQLDA
010814 END-EXEC
010814
010814 EXEC SQL
010814    INCLUDE SQLCA
010814 END-EXEC
010814
010814 EXEC SQL
010814    BEGIN DECLARE SECTION
010814 END-EXEC
010814
010814 01  SQLCMD                      PIC X(1024).
010814 01  SVR                         PIC X(32).
010814 01  USR                         PIC X(32).
010814 01  PASS                        PIC X(32).
010814 01  USR-PASS                    PIC X(64).
010814
010814 01  WS-SQL-DATA.
010814     05  WS-CYCLE-DATE           PIC X(10).
010814     05  WS-NEXT-BUS-DT          PIC X(10).
010814
010814 EXEC SQL
010814    END DECLARE SECTION
010814 END-EXEC
010814
       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  KIXHOST             pic x(9) value Z"HOSTNAME".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
       01  WS-KIXHOST                  PIC X(10).
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).
       01 srch-commarea.
      *                                copy ELCADLTRSPI.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 060611    2011022800001  PEMA  NEW COPYBOOK
101812* 101812    2012101700002  AJRA  ADD ENDT ARCHIVE NO, SCREENID
110612* 110612    2012101700002  AJRA  EXPAND PASSED DATA
      ******************************************************************
      ****************************************
      *  commarea for NaperSoft On Demand Admin services letters
      *  (business logic input & output)
      ****************************************
           03  BL-INPUT.
               05  BL-DATA-SRCE        PIC X.
               05  BL-LETTER-ID        PIC XXXX.
               05  BL-CARRIER          PIC X.
               05  BL-GROUP            PIC X(6).
               05  BL-STATE            PIC XX.
               05  BL-ACCOUNT          PIC X(10).
               05  BL-EFF-DT           PIC X(10).
               05  BL-CERT-NO          PIC X(11).
               05  BL-BATCH-NO         PIC X(6).
               05  BL-BATCH-SEQ        PIC 9(8).
               05  BL-RESP-NO          PIC X(10).
               05  BL-NO-OF-COPIES     PIC 99.
               05  BL-PROC-ID          PIC XXXX.
               05  BL-COMP-ID          PIC XXX.
               05  BL-PRINT-NOW-SW     PIC X.
               05  BL-ENC-CD           PIC XXX.
               05  BL-RESEND-DT        PIC X(10).
               05  BL-FOLLOW-UP-DT     PIC X(10).
               05  BL-ARCHIVE-NO       PIC 9(8).
               05  BL-FUNC             PIC X(8).
110612         05  BL-COMMENTS         PIC X(100).
               05  FILLER REDEFINES BL-COMMENTS.
                   10  BL-REASON-CODE OCCURS 12 PIC X(4).
                   10  BL-LETTER-TO-ACCT PIC X.
                   10  BL-LETTER-TO-BENE PIC X.
                   10  BL-WRITE-ERARCH   PIC X.
                       88  ERARCH-QWS      VALUE 'Q'.
                       88  ERARCH-BATCH    VALUE 'B'.
                       88  ERARCH-TEMP     VALUE 'T'.
                   10  BL-PROCESS-TYPE PIC X(07).
                   10  BL-CERT-FORM-ID PIC X(05).
101812             10  BL-ENDT-ARCH-NO PIC 9(08) BINARY.
101812             10  BL-SOURCE-SCREEN PIC X(8).
110612             10  FILLER          PIC X(25).
           03  BL-OUTPUT.
               05  BL-STATUS                   PIC X.
                   88  BL-OK                      VALUE "P".
                   88  BL-FAIL                  VALUE "F".
               05  BL-MESSAGE          PIC X(50).
110612     03  BL-RECORD-PASSED-DATA   PIC X(6200).
110612     03  FILLER                  PIC X(31).
00038  01  W-WORK-AREAS.
00039      12  FILLER                  PIC  X(18)
00040                                       VALUE 'PROGRAM WORK AREA:'.
00041
00042      12  W-ASKTIME-CTR           PIC S9(04)  COMP   VALUE +0.
00043      12  W-CWA-NDX               PIC S9(04)  COMP   VALUE +0.
00044      12  W-DISPLAY-NDX           PIC S9(04)  COMP   VALUE +0.
00045      12  W-FIRST-BAD-VARIABLE    PIC S9(04)  COMP   VALUE +0.
00046      12  W-NDX                   PIC S9(04)  COMP   VALUE +0.
00047      12  W-NDX2                  PIC S9(04)  COMP   VALUE +0.
00048      12  W-SEQ-CTR               PIC S9(04)  COMP   VALUE +0.
00049      12  W-TS-ITEM               PIC S9(04)  COMP   VALUE +0.
00050      12  W-TOTAL-LINE-LENGTH     PIC S9(04)  COMP   VALUE +0.
00051      12  W-WORK-NDX              PIC S9(04)  COMP   VALUE +0.
00052
00053      12  W-ADJUST-SHORT          PIC S9(03) VALUE +0   COMP-3.
00054      12  W-DIFFERENCE            PIC S9(07)V9(02)
00055                                             VALUE +0   COMP-3.
00056      12  W-INITIAL-COLUMN        PIC S9(03) VALUE +0   COMP-3.
00057      12  W-LAST-COLUMN           PIC S9(03) VALUE +70 COMP-3.
00058      12  W-LAST-SQUEEZED-SPACE   PIC S9(03) VALUE +0   COMP-3.
00059      12  W-LAST-TX-SPACE         PIC S9(03) VALUE +0   COMP-3.
00060      12  W-LAST-WC-SPACE         PIC S9(03) VALUE +0   COMP-3.
00061      12  W-LINE-COUNT            PIC S9(03) VALUE +0   COMP-3.
00062      12  W-LINE-INDENT-1         PIC  9(02) VALUE 0    COMP-3.
00063      12  W-LINE-INDENT-2         PIC  9(02) VALUE 0    COMP-3.
00064      12  W-LINE-INDENT-3         PIC  9(02) VALUE 0    COMP-3.
00065      12  W-LINE-INDENT-4         PIC  9(02) VALUE 0    COMP-3.
00066      12  W-LINE-INDENT-5         PIC  9(02) VALUE 0    COMP-3.
00067      12  W-MAX-LINES-PER-PAGE    PIC  9(02) VALUE 56   COMP-3.
00068      12  W-NEXT-INDENT           PIC  9(02) VALUE 0    COMP-3.
00069      12  W-PAGE                  PIC S9(02) VALUE +0   COMP-3.
00070      12  W-PAGE-LINE             PIC S9(03) VALUE +0   COMP-3.
00071      12  W-PARAGRAPH-INDENT      PIC  9(02) VALUE 0    COMP-3.
00072      12  W-ROLL-COUNTER          PIC S9(03) VALUE +0   COMP-3.
00073      12  W-START-COLUMN          PIC S9(03) VALUE +1   COMP-3.
00074      12  W-TEMP-CURRENT-LINE     PIC S9(03) VALUE +0   COMP-3.
00075      12  W-TOO-FAR               PIC S9(03) VALUE +71 COMP-3.
00076      12  W-TOP-MARGIN            PIC  9(02) VALUE 0    COMP-3.
00077      12  W-TOTAL-TX-LINES        PIC S9(03) VALUE +0   COMP-3.
00078      12  W-TS-GROUP-WORK         PIC  9(05) VALUE 0    COMP-3.
00079      12  W-WORK-INDENT           PIC  9(02) VALUE 0    COMP-3.
00080      12  W-WORK-AMOUNT           PIC S9(09)V9(02)
00081                                             VALUE +0   COMP-3.
00082
00083      12  W-DATA-SOURCE           PIC  9(01) VALUE 0.
00084      12  W-LABEL-SOURCE          PIC  9(01) value 0.
00085          88  W-LABEL-SOURCE-VALID           VALUE 1 THRU 7.
00086      12  W-LABEL-JOINT-NAME      PIC  X(30) VALUE SPACES.
00087      12  W-LAST-ERROR            PIC  9(04) VALUE 9999.
00088      12  W-NUMB-LABEL-LINES      PIC  9(01) VALUE 0.
00089      12  W-PRINT-CONTROL         PIC  9(02) VALUE 0.
00090      12  W-SAVE-REVISION         PIC  9(03) value 0.
00092      12  W-DISPLAY-3             PIC  9(03) value 0.
00093      12  W-DISPLAY-8             PIC  9(08) value 0.
00094      12  W-DISPLAY-7             PIC  9(07) value 0.
00095      12  W-EDIT-2-5-S            PIC  Z9.9(05).
00096      12  W-EDIT-3-0              PIC  ZZ9.
00097      12  W-EDIT-7-2              PIC  $$,$$$,$$9.99.
00098      12  W-EDIT-7-2-NEGATIVE     PIC  $$,$$$,$$9.99-.
00099      12  W-EDIT-9-2              PIC  $$$$,$$$,$$9.99.
00100      12  W-EDIT-12-2             PIC  $$$$,$$$,$$$,$$9.99.
031504
031504     12  WS-ACCT-PKEY-MATCH-SW   PIC  X(01) VALUE SPACE.
031504         88  PRIOR-MATCH-ACCT-PKEY          VALUE 'Y'.
00101
00102      12  W-ARCH-SUPPRESS         PIC 99999999.
00103      12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS
00104                                  PIC  X(08).
00106      12  W-COMP-WORK-AREA.
00107          16  W-CWA-CHAR OCCURS 10 TIMES
00108                                  PIC  X(01).
00109      12  W-INCOMING-ARCHIVE      PIC  X(08).
00110      12  W-INCOMING-ARCH-NO REDEFINES W-INCOMING-ARCHIVE
00111                                  PIC  9(08).
00112
00113      12  W-BATCH-BREAKDOWN.
00114          16  W-BATCH-CONTROL     PIC S9(08) COMP.
00115          16  W-BATCH-SEQ         PIC S9(04) COMP.
00116      12  W-BEN-HOLD              PIC  X(02).
00117      12  W-BENEFIT-WORK          PIC  X(03).
00118      12  W-BEN-R REDEFINES W-BENEFIT-WORK.
00119          16  W-ELIM-DAYS         PIC  X(02).
00120          16  FILLER              PIC  X(01).
00121      12  W-CALL-PGM              PIC  X(08).
00122      12  W-CURRENT-SAVE          PIC  X(02) VALUE SPACES.
00123
00124      12  FILLER                  PIC  X(09) VALUE 'LINE CNTL'.
00125      12  W-LINE-CONTROL-RECORD.
00126          16  W-LC-LINE-WIDTH     PIC  9(02).
00127          16  FILLER              PIC  X(01).
00128          16  W-LC-PARAGRAPH-INDENT
00129                                  PIC  9(02).
00130          16  FILLER              PIC  X(01).
00131          16  W-LC-MAX-LINES-PER-PAGE
00132                                  PIC  9(02).
00133          16  FILLER              PIC  X(01).
00134          16  W-LC-LINE-ADJUST    PIC  9(02).
00135          16  FILLER              PIC  X(01).
00136          16  W-LC-LINE-INDENT-1  PIC  9(02).
00137          16  FILLER              PIC  X(01).
00138          16  W-LC-LINE-INDENT-2  PIC  9(02).
00139          16  FILLER              PIC  X(01).
00140          16  W-LC-LINE-INDENT-3  PIC  9(02).
00141          16  FILLER              PIC  X(01).
00142          16  W-LC-LINE-INDENT-4  PIC  9(02).
00143          16  FILLER              PIC  X(01).
00144          16  W-LC-LINE-INDENT-5  PIC  9(02).
00145          16  FILLER              PIC  X(01).
00146          16  W-LC-TOP-MARGIN     PIC  9(02).
00147          16  FILLER              PIC  X(01).
00148          16  W-LC-CASE-IND       PIC  X(01).
00149              88  W-LC-USE-BOTH-CASES        VALUE 'Y'.
00150          16  FILLER              PIC  X(01).
00151          16  W-LC-PAGE-IND       PIC  X(01).
00152              88  W-LC-CREATE-PAGES          VALUE 'Y'.
00154      12  W-DATE-WORK             PIC  9(07).
00155      12  W-DT-REDEF REDEFINES W-DATE-WORK.
00156          16  FILLER              PIC  X(02).
00157          16  W-DT-WORK           PIC  9(05).
00158
00159      12  W-DEEDIT-FIELD          PIC  X(15).
00160      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD PIC S9(15).
00161
00162      12  W-EDIT-DATE-1.
00163          16  W-ED1-MM            PIC  X(02).
00164          16  FILLER              PIC  X(01) VALUE '/'.
00165          16  W-ED1-DD            PIC  X(02).
00166          16  FILLER              PIC  X(01) VALUE '/'.
00167          16  W-ED1-YY            PIC  X(02).
00168
00169      12  W-EDIT-DATE-2.
00170          16  W-ED2-DD            PIC  X(02).
00171          16  FILLER              PIC  X(01) VALUE '/'.
00172          16  W-ED2-MM            PIC  X(02).
00173          16  FILLER              PIC  X(01) VALUE '/'.
00174          16  W-ED2-YY            PIC  X(02).
010814
010814     12  W-EDIT-A-DATE.
010814         16  W-EDIT-A-MM                  PIC X(2).
010814         16  FILLER                       PIC X(1).
010814         16  W-EDIT-A-DD                  PIC X(2).
010814         16  FILLER                       PIC X(1).
010814         16  W-EDIT-A-CCYY.
010814             20  W-EDIT-A-CC              PIC X(2).
010814             20  W-EDIT-A-YY              PIC X(2).
00175
00176      12  W-LABEL-HOLD-AREA.
00177          16  W-LABEL-LINES OCCURS 6 TIMES.
00178              20  W-LABEL-ZIP.
00179                  24  W-LABEL-1ST-ZIP  PIC  X(05).
00180                  24  W-LABEL-DASH     PIC  X(01).
00181                  24  W-LABEL-2ND-ZIP  PIC  X(04).
00182              20  W-LAB-CAN-POSTAL-CODES REDEFINES W-LABEL-ZIP.
00183                  24  W-LAB-CAN-POSTAL-CD-1
00184                                       PIC  X(03).
00185                  24  W-LAB-CAN-DASH   PIC  X(01).
00186                  24  W-LAB-CAN-POSTAL-CD-2
00187                                       PIC  X(03).
00188                  24  W-LAB-CAN-FILLER PIC  X(03).
00189              20  FILLER               PIC  X(10).
00190              20  W-LAST-ZIP.
00191                  24  W-LAST-1ST-ZIP   PIC  X(05).
00192                  24  W-LABEL-DASH-LAST
00193                                       PIC  X(01).
00194                  24  W-LAST-2ND-ZIP   PIC  X(04).
00195              20  W-LAST-CAN-POSTAL-CODES REDEFINES W-LAST-ZIP.
00196                  24  W-LAST-CAN-POSTAL-CD-1
00197                                       PIC  X(03).
00198                  24  W-LAST-CAN-DASH  PIC  X(01).
00199                  24  W-LAST-CAN-POSTAL-CD-2
00200                                       PIC  X(03).
00201                  24  W-LAST-CAN-FILLER
00202                                       PIC  X(03).
00203
00204      12  W-LAST-CHAR             PIC  X(01).
00205          88  W-LAST-CHAR-PUNC    VALUE '-' '/'.
00206      12  W-LAST-SQ-CHAR          PIC  X(01).
00207
00208      12  W-LINE-NUM.
00209          16  W-LINE1             PIC  X(01).
00210          16  W-LINE23            PIC  9(02).
00211      12  W-LIN-NUM REDEFINES W-LINE-NUM
00212                                  PIC  9(03).
00213
00214      12  W-NAME-LAST             PIC  X(15).
00215      12  W-NAME-FIRST.
00216          16  W-NAME-FIRST-INIT   PIC  X(01).
00217          16  W-NAME-FIRST-REMAIN PIC  X(14).
00218      12  W-NAME-MIDDLE.
00219          16  FILLER              PIC  X(01).
00220          16  W-NAME-MIDDLE-2     PIC  X(01).
00221          16  FILLER              PIC  X(13).
00222
00223      12  W-PAGE-PRT.
00224          16  FILLER              PIC  X(34) VALUE SPACES.
00225          16  W-PAGE-NUMBER       PIC  Z9.
00226          16  FILLER              PIC  X(01) VALUE '.'.
00227          16  FILLER              PIC  X(33) VALUE SPACES.
00228
00229      12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.
00230      12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.
00231          16  FILLER              PIC  9(01).
00232          16  W-PHI-AREA          PIC  9(03).
00233          16  W-PHI-PFX           PIC  9(03).
00234          16  W-PHI-SFX           PIC  9(04).
00235      12  W-PHONE-OUT.
00236          16  W-PO-AREA           PIC  X(03).
00237          16  FILLER              PIC  X(01) VALUE '-'.
00238          16  W-PO-PFX            PIC  X(03).
00239          16  FILLER              PIC  X(01) VALUE '-'.
00240          16  W-PO-SFX            PIC  X(04).
00241
00242      12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.
123113     12  W-SAVE-BIN-NEXT-BUS-DT  PIC  X(02) VALUE SPACES.
010814     12  W-SAVE-NEXT-BUS-DT-EDIT-A PIC X(10)   VALUE SPACES.
010814     12  W-SAVE-CYCLE-DAY-OF-WEEK PIC S9   COMP-3 VALUE +0.
010814     12  W-SAVE-EDIT-A-DATE      PIC  X(10) VALUE SPACES.
00243      12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.
00244      12  W-SAVE-PLAN             PIC  X(02) VALUE SPACES.
00245
00246      12  W-SINGLE-LINE           PIC  X(70).
00247      12  W-SINGLE-LINE-BY REDEFINES W-SINGLE-LINE.
00248          16  ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2 NDXA
00249                                  PIC  X(01).
00250
00251      12  W-SQUEEZED-LINE.
00252          16  W-SQ-CHAR OCCURS 70 TIMES
00253                        INDEXED BY W-SQ-NDX
00254                                  PIC  X(01).
00255
00256      12  W-TIME-IN               PIC S9(07).
00257      12  W-TIME-OUT-R REDEFINES W-TIME-IN.
00258          16  FILLER              PIC  X(01).
00259          16  W-TIME-OUT          PIC  99V9(02).
00260          16  FILLER              PIC  X(02).
00261
00262      12  W-TS-NAME-TEXT.
00263          16  W-TS-ID-TEXT        PIC  X(04) VALUE '104A'.
00264          16  W-TS-ID-TIME
00265              REDEFINES W-TS-ID-TEXT
00266                                  PIC S9(07) COMP-3.
00267          16  W-TS-TERM-TEXT.
00268              20 W-TS-TERM-PREFIX PIC  X(02).
00269              20 FILLER           PIC  X(02).
00270      12  W-TS-NAME-SCREEN.
00271          16  FILLER              PIC  X(04) VALUE '689X'.
00272          16  W-TS-TERM-SCREEN    PIC  X(04).
00273
00274      12  W-WORK-LINE.
00275          16  W-WORK-CHAR OCCURS 70 TIMES
00276                        INDEXED BY W-WC-NDX
00277                                   W-WC-NDX2
00278                                   W-WC-NDX3
00279                                  PIC  X(01).
00280
00281      12  W-WORK-ZIP-NUMERIC      PIC  9(09).
00282      12  W-WORK-ZIP REDEFINES W-WORK-ZIP-NUMERIC.
00283          16  W-WORK-ZIP5         PIC  X(05).
00284          16  W-WORK-ZIP4         PIC  X(04).
00285      12  W-CANADIAN-POSTAL-CODES REDEFINES W-WORK-ZIP-NUMERIC.
00286          16  W-CAN-POSTAL-CD-1.
00287              20  FILLER          PIC  X(01).
00288                  88 W-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00289              20  FILLER          PIC  X(02).
00290          16  W-CAN-POSTAL-CD-2   PIC  X(03).
00291          16  W-FILLER            PIC  X(03).
00292
122011**** Z RECORD LAYOUT MOVED TO COPYBOOK ELCZREC
122011*                   COPY ELCZREC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCZREC.                            *
      *                                                                *
      *   FILE DESCRIPTION = Z CONTROL RECORD LAYOUT                   *
      *                                                                *
      ******************************************************************
      *-----------------------------------------------------------------
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 122011    2011022800001  AJRA  NEW FILE
073112* 073112    2011022800001  AJRA  ADD ACCT SUMM, CSO SUMM
122712* 122712    2012101700002  AJRA  ADD REASON CODE REQUIRED FLAG
072313* 072313    2013062000003  AJRA  ADD IND FOR INSERTION BAR CODE
091913* 091913    2013090300001  AJRA  ADD SIGNATURE FLAG DEFAULT
      *-----------------------------------------------------------------
       01  W-Z-CONTROL-DATA.
           05  W-NUMBER-OF-COPIES      PIC  9.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-FOLLOW-UP     PIC  999.
           05  FILLER                  PIC  X.
           05  W-DAYS-TO-RESEND        PIC  999.
           05  FILLER                  PIC  X.
           05  W-FORM-TO-RESEND        PIC  X(4).
072313     05  W-ADD-BAR-CODE          PIC  X.
           05  W-PROMPT-LETTER         PIC  X.
072313     05  W-HAS-RETURN-ENV        PIC  X.
           05  W-ENCLOSURE-CD          PIC  XXX.
091913     05  W-SIG-FLAG-DEFAULT      PIC  X.
           05  W-AUTO-CLOSE-IND        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-BENE        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TO-ACCT        PIC  X.
           05  FILLER                  PIC  X.
           05  W-LETTER-TYPE           PIC  X.
           05  FILLER                  PIC  X.
           05  W-PRINT-CERTIFICATE     PIC  X.
           05  FILLER                  PIC  X.
           05  W-REFUND-REQUIRED       PIC  X.
           05  FILLER                  PIC  X.
           05  W-ONBASE-CODE           PIC  XX.
073112     05  FILLER                  PIC  X.
073112     05  W-ACCT-SUMM             PIC  X.
073112     05  FILLER                  PIC  X.
073112     05  W-CSO-SUMM              PIC  X.
122712     05  FILLER                  PIC  X.
122712     05  W-REASONS-REQUIRED      PIC  X.
122712     05  FILLER                  PIC  X(29).
00310                                  EJECT
00311  01  W-SWITCH-INDICATORS-AREA.
00312      12  FILLER                  PIC  X(16)
00313                                       VALUE 'PROGRAM SWITCHS:'.
00314      12  W-ADDRESS-ONLY-SW       PIC  X(01) VALUE ' '.
00315          88  W-ADDRESS-ONLY      VALUE 'Y'.
00316          88  W-FULL-DATA         VALUE ' '.
00317      12  W-ACCT-BROWSE-STARTED-SW
00318                                  PIC  X(01) VALUE 'N'.
00319          88  W-ACCT-BROWSE-STARTED          VALUE 'Y'.
00320      12  W-ARCT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.
00321      12  W-CERT-FOUND-SW         PIC  X(01) VALUE 'N'.
00322          88  W-CERT-FOUND                   VALUE 'Y'.
00323      12  W-CHARACTER-TYPE        PIC  X(01).
00324          88  W-END-OF-SENTENCE   VALUE '.' '?' '!'.
00325          88  W-PUNCTUATION       VALUE '.' '?' '!' ',' ';'.
00326          88  W-SPACE             VALUE ' '.
00327      12  W-COMP-BROWSE-SW        PIC  X(01) VALUE 'N'.
00328          88  W-COMP-BROWSE-STARTED          VALUE 'Y'.
00329      12  W-DATA-SHRINK-IND       PIC  X(01) VALUE SPACES.
00330          88  W-DATA-SHRINKING    VALUE 'Y'.
00331      12  W-END-OF-SENTENCE-IND   PIC  X(01) VALUE SPACES.
00332          88  W-END-OF-SENTENCE-WORKING      VALUE 'Y'.
00333      12  W-FIRST-BAD-VARIABLE-IND
00334                                  PIC  X(01) VALUE SPACES.
00335          88  W-FIRST-BAD-VARIABLE-FOUND     VALUE 'Y'.
00336      12  W-FIRST-CHAR-FOUND-IND  PIC  X(01) VALUE SPACES.
00337          88  W-FIRST-CHAR-FOUND             VALUE 'Y'.
00338          88  W-FIRST-CHAR-NOT-FOUND         VALUE SPACES.
00339      12  W-FORM-CHANGED-IND      PIC  X(01) VALUE SPACES.
00340          88  W-FORM-CHANGED                 VALUE 'Y'.
00341      12  W-FORM-SQUEEZE-IND      PIC  X(01) VALUE SPACES.
00342          88  W-FORM-SQUEEZE-ON              VALUE 'Y'.
00343          88  W-FORM-SQUEEZE-OFF             VALUE ' '.
00344      12  W-HOLD-IND              PIC  X(01) VALUE SPACES.
00345          88  W-HOLD-ON                      VALUE 'Y'.
00346          88  W-HOLD-OFF                     VALUE ' '.
00347      12  W-INDIVIDUAL-DATA-SW    PIC  X(01) VALUE ' '.
00348          88  W-INDIVIDUAL-DATA-COMPLETED    VALUE 'Y'.
00349          88  W-INDIV-DATA-NOT-COMPLETED     VALUE ' '.
00350      12  W-KEY-FIELDS-CHANGED-IND
00351                                  PIC  X(01) VALUE ' '.
00352          88  W-KEY-FIELDS-CHANGED           VALUE 'Y'.
00353          88  W-KEY-FIELDS-NOT-CHANGED       VALUE ' '.
00354      12  W-LAST-ONE              PIC  X(01) VALUE HIGH-VALUES.
00355          88  W-LAST-ONE-A-SPACE  VALUE ' '.
00356      12  W-LINE-SQUEEZE-IND      PIC  X(01) VALUE SPACES.
00357          88  W-NEW-PARAGRAPH                VALUE 'P' 'Q' 'R' 'S'
00358                                                    'T' 'U'.
00359          88  W-CONTINUE-PARAGRAPH           VALUE 'C' 'D' 'E' 'F'
00360                                                    'G' 'H'.
00361          88  W-FORM-CONTROL-LINE            VALUE 'K'.
00362          88  W-DO-NOT-ADJUST                VALUE 'N'.
00363          88  W-ADJUST-TO-LINE-LENGTH        VALUE 'A'.
00364          88  W-AS-IS                        VALUE 'A' 'N'.
00365          88  W-CONTINUE-PREVIOUS-PROCESS    VALUE ' ' '1' '2' '3'
00366                                                    '4' '5'.
00367
00368      12  W-PNDB-FOUND-SW         PIC  X(01) VALUE 'N'.
00369          88  W-PNDB-FOUND                   VALUE 'Y'.
00370      12  W-REMAINING-VAR-SW      PIC  X(01) VALUE SPACES.
00371          88  W-REMAINING-VAR-FOUND          VALUE 'Y'.
00372      12  W-REVERSE-DATE-SW       PIC  X(01) VALUE SPACES.
00373          88  W-REVERSE-DATE                 VALUE 'Y'.
00374      12  W-TEXT-BROWSED-SW       PIC  X(01) VALUE 'N'.
00375          88  W-TEXT-BROWSE-STARTED          VALUE 'Y'.
00376          88  W-TEXT-BROWSE-NOT-STARTED      VALUE 'N'.
101812
101812     12  W-VALID-ENDT-SW         PIC  X(01) VALUE 'N'.
101812         88  W-VALID-ENDT                   VALUE 'Y'.
101812     12  W-RESPONSE              PIC S9(8)   COMP.
101812         88  RESP-NORMAL                  VALUE +00.
101812         88  RESP-NOTFND                  VALUE +13.
101812         88  RESP-DUPREC                  VALUE +14.
101812         88  RESP-DUPKEY                  VALUE +15.
101812         88  RESP-NOTOPEN                 VALUE +19.
101812         88  RESP-ENDFILE                 VALUE +20.
00377                                  EJECT
031504 01  WS-SAVE-AREA.
031504     12  WS-SAV-AM-DEFN-1.
031504         16  WS-SAV-AM-AGT-COMMS OCCURS 10 TIMES.
031504             20  WS-SAV-AM-AGT             PIC  X(10).
031504             20  WS-SAV-AM-COM-TYP         PIC  X(01).
031504             20  FILLER                    PIC  X(15).
031504     12  WS-SAV-AM-REMIT-TO                PIC  9(02).
072308     12  WS-SAV-AM-CSR-CODE                PIC  X(04).
031504     12  WS-SAV-AM-CARRIER                 PIC  X(01).
031504     12  WS-SAV-AM-ACCOUNT                 PIC  X(10).
031504     12  WS-SAV-AM-CONTROL-NAME            PIC  X(30).
031504     12  WS-SAV-AM-NAME                    PIC  X(30).
031504     12  WS-SAV-AM-PERSON                  PIC  X(30).
031504     12  WS-SAV-AM-ADDRS                   PIC  X(30).
031504     12  WS-SAV-AM-CITY                    PIC  X(30).
051109     12  WS-SAV-AM-ERACCT-ACCOUNT          PIC  X(10).
031504     12  WS-SAV-AM-ZIP.
031504         16  WS-SAV-AM-ZIP-PRIME.
031504             20  WS-SAV-AM-ZIP-PRI-1ST     PIC  X(01).
031504                 88  SAVE-AM-CANADIAN-POST-CODE       VALUE
031504                     'A' THRU 'Z'.
031504             20  FILLER                    PIC  X(04).
031504         16  FILLER                        PIC  X(04).
031504     12  WS-SAV-AM-CANADIAN-POSTAL-CODE REDEFINES
031504                        WS-SAV-AM-ZIP      PIC  X(09).
031504     12  WS-SAV-AM-TEL-NO.
031504         16  WS-SAV-AM-AREA-CODE           PIC 9(03).
031504         16  WS-SAV-AM-TEL-PRE             PIC 9(03).
031504         16  WS-SAV-AM-TEL-NBR             PIC 9(04).
00378  01  W-KEY-AREAS.
00379      12  FILLER                    PIC  X(13)
00380                                       VALUE 'PROGRAM KEYS:'.
00381
00382      12  W-ACCT-SAVE-KEY           PIC  X(20).
00383      12  W-ACCT-KEY.
00384          16  W-ACCT-PARTIAL-KEY.
00385              20  W-ACCT-COMPANY-CD PIC  X(01).
00387              20  W-ACCT-CARRIER    PIC  X(01).
00388              20  W-ACCT-GROUPING   PIC  X(06).
00389              20  W-ACCT-STATE      PIC  X(02).
00390              20  W-ACCT-ACCOUNT    PIC  X(10).
00391          16  W-ACCT-EXP-DT         PIC  X(02).
00392
00393      12  W-ARCH-SAVE-KEY         PIC  X(05).
00394      12  W-ARCH-KEY.
00395          16  W-ARCH-PARTIAL-KEY.
00396              20  W-ARCH-COMPANY-CD
00397                                  PIC  X(01).
00398              20  W-ARCH-NUMBER   PIC S9(08)      COMP.
00399          16  W-ARCH-SEQ-NO       PIC S9(04)      COMP VALUE +0.
00400
00401      12  W-ARCT-KEY.
00402          16  W-ARCT-PARTIAL-KEY.
00403              20  W-ARCT-COMPANY-CD
00404                                  PIC  X(01).
00405              20  W-ARCT-NUMBER   PIC S9(08)      COMP.
00406          16  W-ARCT-REC-TYPE     PIC  X(01).
00407          16  W-ARCT-SEQ-NO       PIC S9(04)      COMP VALUE +0.
00408
00409      12  W-CERT-KEY.
00410          16  W-CERT-COMPANY-CD   PIC  X(01).
00411          16  W-CERT-CARRIER      PIC  X(01).
00412          16  W-CERT-GROUPING     PIC  X(06).
00413          16  W-CERT-STATE        PIC  X(02).
00414          16  W-CERT-ACCOUNT      PIC  X(10).
00415          16  W-CERT-EFF-DT       PIC  X(02).
00416          16  W-CERT-CERT-NO.
00417              20  W-CERT-CERT-PRIME
00418                                  PIC  X(10).
00419              20  W-CERT-CERT-SFX PIC  X(01).
00420
00421      12  W-CHEK-KEY.
00422          16  W-CHEK-COMPANY-CD   PIC  X(01).
00423          16  W-CHEK-CARRIER      PIC  X(01).
00424          16  W-CHEK-GROUPING     PIC  X(06).
00425          16  W-CHEK-STATE        PIC  X(02).
00426          16  W-CHEK-ACCOUNT      PIC  X(10).
00427          16  W-CHEK-EFF-DT       PIC  X(02).
00428          16  W-CHEK-CERT-NO.
00429              20  W-CHEK-CERT-PRIME
00430                                  PIC  X(10).
00431              20  W-CHEK-CERT-SFX PIC  X(01).
00432          16  W-CHEK-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
00433
00434      12  W-CNTL-KEY.
00435          16  W-CNTL-COMPANY-ID   PIC  X(03).
00436          16  W-CNTL-RECORD-TYPE  PIC  X(01)   VALUE '1'.
00437          16  W-CNTL-GENL.
00438              20  W-CNTL-GEN1     PIC  X(02)   VALUE SPACES.
00439              20  W-CNTL-GEN2.
00440                  24 W-CNTL-GEN3  PIC  X(01)   VALUE SPACES.
00441                  24 W-CNTL-GEN4  PIC  X(01)   VALUE SPACES.
00442          16  W-CNTL-SEQ-NO       PIC S9(04)   VALUE +0    COMP.
00443
00444      12  W-COMP-SAVE-KEY         PIC  X(29).
00445      12  W-COMP-KEY.
00446          16  W-COMP-COMPANY-CD   PIC  X(01).
00447          16  W-COMP-CARRIER      PIC  X(01).
00448          16  W-COMP-GROUPING     PIC  X(06).
00449          16  W-COMP-RESP-PERSON.
00450              20  W-COMP-RP-CHAR OCCURS 10 TIMES
00451                                 INDEXED BY W-COMP-NDX
00452                                  PIC  X(01).
00453          16  W-COMP-ACCOUNT      PIC  X(10).
00454          16  W-COMP-TYPE         PIC  X(01).
00455
00456      12  W-DELETE-KEY.
00457          16  W-DELETE-PARTIAL-KEY.
00458              20  W-DELETE-COMPANY-CD
00459                                  PIC  X(01).
00460              20  W-DELETE-NUMBER PIC S9(08)      COMP.
00461          16  W-DELETE-RECORD-TYPE
00462                                  PIC  X(01).
00463          16  W-DELETE-SEQ        PIC S9(04)      COMP VALUE +0.
00464
00465      12  W-MAIL-KEY.
00466          16  W-MAIL-COMPANY-CD   PIC  X(01).
00467          16  W-MAIL-CARRIER      PIC  X(01).
00468          16  W-MAIL-GROUPING     PIC  X(06).
00469          16  W-MAIL-STATE        PIC  X(02).
00470          16  W-MAIL-ACCOUNT      PIC  X(10).
00471          16  W-MAIL-EFF-DT       PIC  X(02).
00472          16  W-MAIL-CERT-NO.
00473              20  W-MAIL-CERT-PRIME
00474                                  PIC  X(10).
00475              20  W-MAIL-CERT-SFX PIC  X(01).
00476
00477      12  W-PNDB-KEY.
00478          16  W-PNDB-COMPANY-CD   PIC  X(01).
00479          16  W-PNDB-ENTRY        PIC  X(06).
00480          16  W-PNDB-SEQ-NO       PIC S9(04)  COMP.
00481          16  W-PNDB-CHG-SEQ-NO   PIC S9(04)  COMP.
00482
00483      12  W-PNDB2-KEY.
00484          16  W-PNDB2-COMPANY-CD  PIC  X(01).
00485          16  W-PNDB2-CARRIER     PIC  X(01).
00486          16  W-PNDB2-GROUPING    PIC  X(06).
00487          16  W-PNDB2-STATE       PIC  X(02).
00488          16  W-PNDB2-ACCOUNT     PIC  X(10).
00489          16  W-PNDB2-EFF-DT      PIC  X(02).
00490          16  W-PNDB2-CERT-NO.
00491              20  W-PNDB2-CERT-PRIME
00492                                  PIC  X(10).
00493              20  W-PNDB2-CERT-SFX
00494                                  PIC  X(01).
00495          16  W-PNDB2-ALT-CHG-SEQ-NO
00496                                  PIC S9(04)  COMP.
00497          16  W-PNDB2-TYPE        PIC  X(01).
00498
00499      12  W-PYAJ-KEY.
00500          16  W-PYAJ-COMPANY-CD   PIC  X(01).
00501          16  W-PYAJ-CARRIER      PIC  X(01).
00502          16  W-PYAJ-GROUPING     PIC  X(06).
00503          16  W-PYAJ-FIN-RESP     PIC  X(10).
00504          16  W-PYAJ-ACCOUNT      PIC  X(10).
00505          16  W-PYAJ-FILE-SEQ-NO  PIC S9(08)    COMP.
00506          16  W-PYAJ-RECORD-TYPE  PIC  X(01).
00507
00508      12  W-SC-QUID-KEY.
00509          16  W-SC-QUID-TERMINAL  PIC  X(04).
00510          16  W-SC-QUID-SYSTEM    PIC  X(04).
00511
00512      12  W-TEXT-SAVE-KEY         PIC  X(05).
00513      12  W-TEXT-KEY.
00514          16  W-TEXT-PARTIAL-KEY.
00515              20  W-TEXT-COMPANY-CD
00516                                  PIC  X(01).
00517              20  W-TEXT-LETTER   PIC  X(04).
00518          16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.
00519          16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.
00520
101812     12  W-ERENDT-KEY-BY-ARCH.
101812         16  W-ERENDT-COMPANY-CD-A1 PIC X.
101812         16  W-ERENDT-ARCHIVE       PIC 9(8) BINARY.
101812
012413     12  W-ELEOBC-KEY.
012413         16  W-EOBC-COMPANY-CD   PIC X.
012413         16  W-EOBC-REC-TYPE     PIC X.
012413         16  W-EOBC-CODE         PIC X(4).
012413         16  FILLER              PIC X(9).
012413     12  W-ERNOTE-KEY.
012413         16  W-NOTE-COMPANY-CD   PIC X.
012413         16  W-NOTE-CARRIER      PIC X.
012413         16  W-NOTE-GROUPING     PIC X(6).
012413         16  W-NOTE-STATE        PIC XX.
012413         16  W-NOTE-ACCOUNT      PIC X(10).
012413         16  W-NOTE-CERT-EFF-DT  PIC XX.
012413         16  W-NOTE-CERT-PRIME   PIC X(10).
012413         16  W-NOTE-CERT-SFX     PIC X.
041320         16  w-note-record-type  pic x.
012413     12  W-SAVE-KEY.
012413         16  W-SV-COMPANY-CD     PIC X.
012413         16  W-SV-CARRIER        PIC X.
012413         16  W-SV-GROUPING       PIC X(6).
012413         16  W-SV-STATE          PIC XX.
012413         16  W-SV-ACCOUNT        PIC X(10).
012413         16  W-SV-CERT-EFF-DT    PIC XX.
012413         16  W-SV-CERT-PRIME     PIC X(10).
012413         16  W-SV-CERT-SFX       PIC X.
012413     12  W-BILLING-NOTE.
012413         16  W-BN-NOTE           PIC X(25).
012413         16  W-BN-LTRID          PIC X(4).
012413         16  FILLER              PIC X(3).
012413         16  W-BN-DATE           PIC X(8).
012413         16  FILLER              PIC X(3).
012413         16  W-BN-USERID         PIC X(4).
012413         16  FILLER              PIC X(30).
012413     12  W-LEN                   PIC S9(5) COMP-3 VALUE +0.
012413     12  NOTE-SUB                PIC S9(5) COMP-3 VALUE +0.
012413     12  W-CERT-UPDATE-SW        PIC X  VALUE ' '.
012413         88  NO-CERT-RW                 VALUE 'N'.
012413         88  CERT-RW                    VALUE 'Y'.
121213
121213     12  W-ELENCC-KEY.
121213         16  W-ENCC-COMPANY-CD   PIC X.
121213         16  W-ENCC-REC-TYPE     PIC X.
121213         16  W-ENCC-ENC-CODE     PIC X(5).
121213         16  FILLER              PIC X(09).
00521                                  EJECT
00522  01  FILLER                      PIC  X(22)
00523                                  VALUE 'INTERFACE AREA STARTS:'.
00524 *    COPY ELCINTF.
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
00525      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.
00526 *    COPY ELC1042.
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
00527 *    COPY ELC689PI.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ELC689PI                            *
00004 *                            VMOD=2.003                          *
00005 *                                                                *
00006 *    THIS IS THE PI-PROGRAM-WORK-AREA THAT IS USED FOR THE       *
00007 *    CREDIT CORRESPONDENCE SUB-SYSTEM.  ANY CHANGES WILL         *
00008 *    WILL EFFECT THE PROGRAMS OF THAT SUB-SYSTEM.                *
00009 *                                                                *
00010 *    IF THE LENGTH OF THIS PI-AREA CHANGES THE LENGTH MUST       *
00011 *    BE CHANGED FOR THE COMM-AREA WHEN PASSING THIS PI-AREA      *
00012 *    BETWEEN PROGRAMS.                                           *
00013 *                                                                *
00014 *    THE FOLLOWING PROGRAMS USE THIS COPYBOOK:                   *
00015 *                                                                *
00016 *               EL631 - EL689  - EL6891 - EL6892                 *
00017 *                                                                *
00018 ******************************************************************
081004*                   C H A N G E   L O G
081004*
081004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081004*-----------------------------------------------------------------
081004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081004* EFFECTIVE    NUMBER
081004*-----------------------------------------------------------------
081004* 081004                   PEMA  CONVERT TO PSUEDO CONVERSATIONAL
100705* 100705  CR2004072800004  PEMA  ADD LETTERS TO BE RESENT
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
081004******************************************************************
00019
00020
00021          16  PI-689-WORK-AREA.
00022              20  PI-689-ALT-PRINTER-ID
00023                                  PIC  X(04).
00024              20  PI-689-ARCHIVE-NUMBER
00025                                  PIC  9(08).
00026              20  PI-689-ARCHIVE-SW
00027                                  PIC  X(01).
00028                  88  PI-689-ARCHIVE-LETTER VALUE 'Y'.
00029              20  PI-689-DATA-SOURCE
00030                                  PIC  X(01).
00031                  88  PI-689-SRC-ACCOUNT        VALUE '1'.
00032                  88  PI-689-SRC-CERTIFICATE    VALUE '2'.
00033                  88  PI-689-SRC-COMPENSATION   VALUE '3'.
00034                  88  PI-689-SRC-PEND-BUSINESS  VALUE '4'.
00035                  88  PI-689-SRC-CHECKS         VALUE '5'.
00036              20  PI-689-ERROR-IND
00037                                  PIC  X(01).
00038                  88  PI-689-ERR-DETECTED-PREV  VALUE 'Y'.
00039              20  PI-689-ERROR    PIC  9(04).
00040                  88  PI-689-NO-ERRORS-DETECTED VALUE 0000.
00041                  88  PI-689-FATAL-ERROR
00042                      VALUES 0004 0006 0008 0013 0023 0029 0033
00043                             0042 0047 0051 0066 0067 0070
00044                             0168 0169 0174 0175 0176 0177 0179
00045                             0180 0181 0182 0184 0185 0189 0190
00046                             0191
00047                             0215 0279 0280
00048                             0412 0413 0454
00049                             0533 0537
00050                             2055 2114 2208 2209 2216 2232 2369
00051                             2398 2433 2908 2999
00052                             3000 3770 3771 3775
00053                             7250 7365 7367 7368 7369 7370 7371
00054                             7272 7373 7374 7376 7377 7378 7379
00055                             7381 7388 7390 7393 7395 7396 7398
00056                             9095 9096 9281 9298 9299 9320 9327
00057                             9426 9427.
00058                  88  PI-689-STOP-ERROR
00059                      VALUES 0004 0008 0013 0023 0029 0033
00060                             0042 0047 0066 0067 0070
00061                             0168 0169 0174 0175 0176 0177
00062                             0181 0182 0184 0185 0189 0190
00063                             0279 0280
00064                             0412 0413 0454
00065                             2055 2208 2209 2216 2232
00066                             2398 2999
00067                             3000 3770 3771 3775
00068                             7250 7365 7369 7370 7371
00069                             7272 7373 7374 7376 7377 7378 7379
00070                             7381 7388 7390 7393 7396 7398
00071                             9095 9096 9299 9320 9426.
00072              20  PI-689-FOLLOW-UP-DATE
00073                                  PIC  X(02).
00074              20  PI-689-FORM-NUMBER
00075                                  PIC  X(04).
00076              20  PI-689-LABEL-SOURCE
00077                                  PIC X(01).
00078                  88  PI-689-SOURCE-ACCOUNT  VALUE '1'.
00079                  88  PI-689-SOURCE-CARRIER  VALUE '2'.
00080                  88  PI-689-SOURCE-COMPANY  VALUE '3'.
00081                  88  PI-689-SOURCE-COMP     VALUE '4'.
00082                  88  PI-689-SOURCE-MAIL     VALUE '5'.
00083                  88  PI-689-SOURCE-CHECK    VALUE '6'.
00084                  88  PI-689-SOURCE-VARIABLE VALUE '7'.
00085              20  PI-689-NUMBER-COPIES
00086                                  PIC  9(01).
00087              20  PI-689-NUMBER-LABEL-LINES
00088                                  PIC  9(01).
00089              20  PI-689-NUMBER-TEXT-RECORDS
00090                                  PIC  9(03).
00091              20  PI-689-PRINT-ORDER-SW
00092                                  PIC  X(01).
00093                  88  PI-689-PRINT-FIRST     VALUE '1'.
00094                  88  PI-689-PRINT-SECOND    VALUE '2'.
00095                  88  PI-689-PRINT-LATER     VALUE '3'.
00096                  88  PI-689-PRINT-ONLY      VALUE '4'.
00097              20  PI-689-PRINT-RESTRICTION
00098                                  PIC  X(01).
00099                  88  PI-689-VALID-RESTRICT     VALUE 'C' 'F'.
00100                  88  PI-689-PRT-ONLY-WITH-CNTL VALUE 'C'.
00101                  88  PI-689-PRT-ONLY-WITH-FORM VALUE 'F'.
00102              20  PI-689-PRINT-SW PIC  X(01).
00103                  88  PI-689-PRINT-PERFORMED VALUE '1'.
00104              20  PI-689-RESEND-DATE-1
00105                                  PIC  X(02).
100705             20  PI-689-RESEND-LETR-1
                                       PIC X(4).
00110              20  PI-689-TEMP-STOR-ID
00111                                  PIC  X(08).
00112              20  PI-689-USE-SCREEN-IND
00113                                  PIC  X(01).
00114                  88  PI-689-CREATE-NO-SCREENS VALUE '1'.
00115              20  PI-689-ARCH-POINTER
00116                                  PIC S9(08) COMP.
00117                  88  PI-689-GET-ARCH-MAIN     VALUE +0.
00118              20  PI-689-ARCT-POINTER
00119                                  PIC S9(08) COMP.
00120                  88  PI-689-GET-ARCT-MAIN     VALUE +0.
00121              20  PI-689-VARIABLE-DATA-GRP.
00122                  24  PI-689-VARIABLE-DATA-1
00123                                  PIC  X(30).
00124                  24  PI-689-VARIABLE-DATA-2
00125                                  PIC  X(30).
00126                  24  PI-689-VARIABLE-DATA-3
00127                                  PIC  X(30).
00128                  24  PI-689-VARIABLE-DATA-4
00129                                  PIC  X(30).
00130
00131          16  PI-689-KEY-DATA-FIELDS.
00132              20  PI-689-ACCOUNT  PIC  X(10).
00133              20  PI-689-CARRIER  PIC  X(01).
00134              20  PI-689-CERT-NO.
00135                  24  PI-689-CERT-PRIME
00136                                  PIC  X(10).
00137                  24  PI-689-CERT-SFX
00138                                  PIC  X(01).
00139              20  PI-689-CHG-SEQ-NO
00140                                  PIC S9(04)    COMP.
00141              20  PI-689-CHG-SEQ-NOX REDEFINES PI-689-CHG-SEQ-NO
00142                                  PIC  X(02).
00143              20  PI-689-ENTRY-BATCH
00144                                  PIC  X(06).
00145              20  PI-689-EFF-DATE PIC  X(02).
00146              20  PI-689-EXP-DATE PIC  X(02).
00147              20  PI-689-GROUPING PIC  X(06).
00148              20  PI-689-RESP-PERSON
00149                                  PIC  X(10).
00150              20  PI-689-SEQ-NO   PIC S9(08)    COMP.
00151              20  PI-689-SEQ-NOX REDEFINES PI-689-SEQ-NO
00152                                  PIC  X(04).
00153              20  PI-689-STATE    PIC  X(02).
00154              20  PI-689-TYPE     PIC  X(01).
00155              20  PI-689-CONTROL  PIC S9(08)    COMP.
00156              20  PI-689-ALT-SEQ-NO
00157                                  PIC S9(04)    COMP.
00158          16  PI-689-DATE-EDIT    PIC  X(08).
00159          16  PI-689-FOLLOW-UP-EDIT
00160                                  PIC  X(08).
00161          16  PI-689-RESEND1-EDIT PIC  X(08).
00164          16  PI-689-SEQ-EDIT     PIC  X(08).
00165          16  PI-689-BCSEQ-EDIT   PIC  X(04).
00166          16  PI-689-LBL-OVERRIDE PIC  X(01).
00167              88  PI-689-LABELS-OVERRIDEN  VALUES 'N'.
081004         16  PI-689-FATAL-CTR    PIC 999     COMP-3.
081004         16  PI-689-FORCABLE-CTR PIC 999     COMP-3.
122011         16  PI-PROMPT-IND       PIC X(1).
122011         16  PI-CERT-FORM-ID     PIC X(5).
122011         16  PI-PRINT-NOW        PIC X(1).
061412         16  PI-ENCLOSURE-CD     PIC X(3).
091712         16  PI-CERT-REQ-IND     PIC X(1).
101812         16  PI-ENDT-ARCH-NO     PIC 9(8).
011013         16  PI-REASON-REQ-IND   PIC X(1).
041320         16  pi-iss-can-pend-rec pic x.
011013         16  FILLER              PIC X(243).
00528 *        16  FILLER                        PIC X(276).
00528 *        16  FILLER                        PIC X(280).
00529
00530  01  FILLER                      PIC  X(20)
00531                                  VALUE ':INTERFACE AREA ENDS'.
00532                                  EJECT
00533  01  FILLER                      PIC  X(16)
00534                         VALUE 'MAP AREA STARTS:'.
00535 *    COPY EL689S.
       01  EL689AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  RUNDTEL PIC S9(0004) COMP.
           05  RUNDTEF PIC  X(0001).
           05  FILLER REDEFINES RUNDTEF.
               10  RUNDTEA PIC  X(0001).
           05  RUNDTEI PIC  X(0008).
      *    -------------------------------
           05  RUNTIMEL PIC S9(0004) COMP.
           05  RUNTIMEF PIC  X(0001).
           05  FILLER REDEFINES RUNTIMEF.
               10  RUNTIMEA PIC  X(0001).
           05  RUNTIMEI PIC  X(0005).
      *    -------------------------------
           05  HOSTL PIC S9(0004) COMP.
           05  HOSTF PIC  X(0001).
           05  FILLER REDEFINES HOSTF.
               10  HOSTA PIC  X(0001).
           05  HOSTI PIC  X(0010).
      *    -------------------------------
           05  SYSL PIC S9(0004) COMP.
           05  SYSF PIC  X(0001).
           05  FILLER REDEFINES SYSF.
               10  SYSA PIC  X(0001).
           05  SYSI PIC  X(0008).
      *    -------------------------------
           05  COMPANYL PIC S9(0004) COMP.
           05  COMPANYF PIC  X(0001).
           05  FILLER REDEFINES COMPANYF.
               10  COMPANYA PIC  X(0001).
           05  COMPANYI PIC  X(0003).
      *    -------------------------------
           05  USERIDL PIC S9(0004) COMP.
           05  USERIDF PIC  X(0001).
           05  FILLER REDEFINES USERIDF.
               10  USERIDA PIC  X(0001).
           05  USERIDI PIC  X(0004).
      *    -------------------------------
           05  MAINTL PIC S9(0004) COMP.
           05  MAINTF PIC  X(0001).
           05  FILLER REDEFINES MAINTF.
               10  MAINTA PIC  X(0001).
           05  MAINTI PIC  X(0001).
      *    -------------------------------
           05  FORML PIC S9(0004) COMP.
           05  FORMF PIC  X(0001).
           05  FILLER REDEFINES FORMF.
               10  FORMA PIC  X(0001).
           05  FORMI PIC  X(0004).
      *    -------------------------------
           05  ARCHNUML PIC S9(0004) COMP.
           05  ARCHNUMF PIC  X(0001).
           05  FILLER REDEFINES ARCHNUMF.
               10  ARCHNUMA PIC  X(0001).
           05  ARCHNUMI PIC  99999999.
      *    -------------------------------
           05  PRTNOWL PIC S9(0004) COMP.
           05  PRTNOWF PIC  X(0001).
           05  FILLER REDEFINES PRTNOWF.
               10  PRTNOWA PIC  X(0001).
           05  PRTNOWI PIC  X(0001).
      *    -------------------------------
           05  CARRIERL PIC S9(0004) COMP.
           05  CARRIERF PIC  X(0001).
           05  FILLER REDEFINES CARRIERF.
               10  CARRIERA PIC  X(0001).
           05  CARRIERI PIC  X(0001).
      *    -------------------------------
           05  GROUPL PIC S9(0004) COMP.
           05  GROUPF PIC  X(0001).
           05  FILLER REDEFINES GROUPF.
               10  GROUPA PIC  X(0001).
           05  GROUPI PIC  X(0006).
      *    -------------------------------
           05  STATEL PIC S9(0004) COMP.
           05  STATEF PIC  X(0001).
           05  FILLER REDEFINES STATEF.
               10  STATEA PIC  X(0001).
           05  STATEI PIC  X(0002).
      *    -------------------------------
           05  ACCTL PIC S9(0004) COMP.
           05  ACCTF PIC  X(0001).
           05  FILLER REDEFINES ACCTF.
               10  ACCTA PIC  X(0001).
           05  ACCTI PIC  X(0010).
      *    -------------------------------
           05  CERTL PIC S9(0004) COMP.
           05  CERTF PIC  X(0001).
           05  FILLER REDEFINES CERTF.
               10  CERTA PIC  X(0001).
           05  CERTI PIC  X(0010).
      *    -------------------------------
           05  SFXL PIC S9(0004) COMP.
           05  SFXF PIC  X(0001).
           05  FILLER REDEFINES SFXF.
               10  SFXA PIC  X(0001).
           05  SFXI PIC  X(0001).
      *    -------------------------------
           05  TYPEL PIC S9(0004) COMP.
           05  TYPEF PIC  X(0001).
           05  FILLER REDEFINES TYPEF.
               10  TYPEA PIC  X(0001).
           05  TYPEI PIC  X(0001).
      *    -------------------------------
           05  DATEL PIC S9(0004) COMP.
           05  DATEF PIC  X(0001).
           05  FILLER REDEFINES DATEF.
               10  DATEA PIC  X(0001).
           05  DATEI PIC  X(0008).
      *    -------------------------------
           05  FOLLOWL PIC S9(0004) COMP.
           05  FOLLOWF PIC  X(0001).
           05  FILLER REDEFINES FOLLOWF.
               10  FOLLOWA PIC  X(0001).
           05  FOLLOWI PIC  X(0008).
      *    -------------------------------
           05  RESEND1L PIC S9(0004) COMP.
           05  RESEND1F PIC  X(0001).
           05  FILLER REDEFINES RESEND1F.
               10  RESEND1A PIC  X(0001).
           05  RESEND1I PIC  X(0008).
      *    -------------------------------
           05  ENCL PIC S9(0004) COMP.
           05  ENCF PIC  X(0001).
           05  FILLER REDEFINES ENCF.
               10  ENCA PIC  X(0001).
           05  ENCI PIC  X(0003).
      *    -------------------------------
           05  ENDARCHL PIC S9(0004) COMP.
           05  ENDARCHF PIC  X(0001).
           05  FILLER REDEFINES ENDARCHF.
               10  ENDARCHA PIC  X(0001).
           05  ENDARCHI PIC  99999999.
      *    -------------------------------
           05  CERTIDL PIC S9(0004) COMP.
           05  CERTIDF PIC  X(0001).
           05  FILLER REDEFINES CERTIDF.
               10  CERTIDA PIC  X(0001).
           05  CERTIDI PIC  X(0005).
      *    -------------------------------
           05  ADDRSL PIC S9(0004) COMP.
           05  ADDRSF PIC  X(0001).
           05  FILLER REDEFINES ADDRSF.
               10  ADDRSA PIC  X(0001).
           05  ADDRSI PIC  X(0001).
      *    -------------------------------
           05  PRINTERL PIC S9(0004) COMP.
           05  PRINTERF PIC  X(0001).
           05  FILLER REDEFINES PRINTERF.
               10  PRINTERA PIC  X(0001).
           05  PRINTERI PIC  X(0004).
      *    -------------------------------
           05  COPIESL PIC S9(0004) COMP.
           05  COPIESF PIC  X(0001).
           05  FILLER REDEFINES COPIESF.
               10  COPIESA PIC  X(0001).
           05  COPIESI PIC  X(0001).
      *    -------------------------------
           05  DATASORL PIC S9(0004) COMP.
           05  DATASORF PIC  X(0001).
           05  FILLER REDEFINES DATASORF.
               10  DATASORA PIC  X(0001).
           05  DATASORI PIC  X(0001).
      *    -------------------------------
           05  ADDRLBLL PIC S9(0004) COMP.
           05  ADDRLBLF PIC  X(0001).
           05  FILLER REDEFINES ADDRLBLF.
               10  ADDRLBLA PIC  X(0001).
           05  ADDRLBLI PIC  X(0001).
      *    -------------------------------
           05  RPERSONL PIC S9(0004) COMP.
           05  RPERSONF PIC  X(0001).
           05  FILLER REDEFINES RPERSONF.
               10  RPERSONA PIC  X(0001).
           05  RPERSONI PIC  X(0010).
      *    -------------------------------
           05  BENTRYL PIC S9(0004) COMP.
           05  BENTRYF PIC  X(0001).
           05  FILLER REDEFINES BENTRYF.
               10  BENTRYA PIC  X(0001).
           05  BENTRYI PIC  X(0006).
      *    -------------------------------
           05  SEQL PIC S9(0004) COMP.
           05  SEQF PIC  X(0001).
           05  FILLER REDEFINES SEQF.
               10  SEQA PIC  X(0001).
           05  SEQI PIC  X(0008).
      *    -------------------------------
           05  BCSEQL PIC S9(0004) COMP.
           05  BCSEQF PIC  X(0001).
           05  FILLER REDEFINES BCSEQF.
               10  BCSEQA PIC  X(0001).
           05  BCSEQI PIC  X(0004).
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
           05  ERRMSGL PIC S9(0004) COMP.
           05  ERRMSGF PIC  X(0001).
           05  FILLER REDEFINES ERRMSGF.
               10  ERRMSGA PIC  X(0001).
           05  ERRMSGI PIC  X(0079).
      *    -------------------------------
           05  ENTERPFL PIC S9(0004) COMP.
           05  ENTERPFF PIC  X(0001).
           05  FILLER REDEFINES ENTERPFF.
               10  ENTERPFA PIC  X(0001).
           05  ENTERPFI PIC  99.
      *    -------------------------------
           05  PF8HDL PIC S9(0004) COMP.
           05  PF8HDF PIC  X(0001).
           05  FILLER REDEFINES PF8HDF.
               10  PF8HDA PIC  X(0001).
           05  PF8HDI PIC  X(0022).
       01  EL689AO REDEFINES EL689AI.
           05  FILLER            PIC  X(0012).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNDTEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RUNTIMEO PIC  99.99.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  HOSTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SYSO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COMPANYO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  USERIDO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  MAINTO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FORMO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ARCHNUMO PIC  99999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRTNOWO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CARRIERO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  GROUPO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  STATEO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ACCTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SFXO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  TYPEO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATEO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  FOLLOWO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RESEND1O PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENCO PIC  X(0003).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENDARCHO PIC  99999999.
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  CERTIDO PIC  X(0005).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRSO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PRINTERO PIC  X(0004).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  COPIESO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  DATASORO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ADDRLBLO PIC  X(0001).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  RPERSONO PIC  X(0010).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BENTRYO PIC  X(0006).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  SEQO PIC  X(0008).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  BCSEQO PIC  X(0004).
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
           05  ERRMSGO PIC  X(0079).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  ENTERPFO PIC  X(0002).
      *    -------------------------------
           05  FILLER            PIC  X(0003).
           05  PF8HDO PIC  X(0022).
      *    -------------------------------
00536  01  W-MAP-REDEF REDEFINES EL689AI.
031011*    12  FILLER                  PIC X(218).
101812     12  FILLER                  PIC X(267).
00538      12  EL689RI.
00539          16  W-TEXT-LINES OCCURS 12 TIMES INDEXED BY W-SC-NDX.
00540              20  W-SC-LINEL      PIC S9(04) COMP.
00541              20  W-SC-LINEA      PIC  X(01).
00542              20  W-SC-LINE       PIC  X(03).
00543              20  W-SC-TEXTL      PIC S9(04) COMP.
00544              20  W-SC-TEXTA      PIC  X(01).
00545              20  W-SC-TEXT       PIC  X(70).
041811*    12  FILLER                  PIC  X(87).
041811     12  FILLER                  PIC  X(112).
00547                                  EJECT
00548  01  W-CONSTANT-AREA.
00549      12  FILLER                  PIC  X(18)
00550                                  VALUE 'PROGRAM CONSTANTS:'.
00551      12  W-APPL-SCRTY-NDX        PIC S9(04)  COMP  VALUE +03.
00552      12  W-ARCH-LENGTH           PIC S9(04)  COMP  VALUE +250.
00553      12  W-ARCT-LENGTH           PIC S9(04)  COMP  VALUE +1640.
101812     12  W-ENDT-LENGTH           PIC S9(04)  COMP  VALUE +579.
012413     12  W-NOTE-LENGTH           PIC S9(04)  COMP  VALUE +825.
012413     12  W-EOBC-LENGTH           PIC S9(04)  COMP  VALUE +350.
121213     12  W-ENCC-LENGTH           PIC S9(04)  COMP  VALUE +400.
00554      12  W-MAX-LINES             PIC S9(03) VALUE +300 COMP-3.
00555      12  W-NUM-LINES-PER-SCREEN  PIC  9(02)        VALUE 12.
00556      12  W-TS-NUM-REC-IN-GROUP   PIC  9(02)        VALUE 50.
00557      12  W-TS-LENGTH             PIC S9(04)  COMP  VALUE +3650.
00558      12  W-TS-MAP-LENGTH         PIC S9(04)  COMP  VALUE +1260.
00559      12  W-ZEROS                 PIC S9(03) VALUE +000 COMP-3.
00560
00561      12  W-ACCT-FILE-ID          PIC  X(08) VALUE 'ERACCT'.
00562      12  W-ACCT2-FILE-ID         PIC  X(08) VALUE 'ERACCT2'.
00563      12  W-ARCH-FILE-ID          PIC  X(08) VALUE 'ERARCH'.
00564      12  W-ARCH2-FILE-ID         PIC  X(08) VALUE 'ERARCH2'.
00565      12  W-ARCH3-FILE-ID         PIC  X(08) VALUE 'ERARCH3'.
00566      12  W-ARCH4-FILE-ID         PIC  X(08) VALUE 'ERARCH4'.
00567      12  W-ARCH5-FILE-ID         PIC  X(08) VALUE 'ERARCH5'.
00568      12  W-ARCT-FILE-ID          PIC  X(08) VALUE 'ERARCT'.
00569      12  W-CERT-FILE-ID          PIC  X(08) VALUE 'ELCERT'.
00570      12  W-CHEK-FILE-ID          PIC  X(08) VALUE 'ERCHEK'.
00571      12  W-CNTL-FILE-ID          PIC  X(08) VALUE 'ELCNTL'.
00572      12  W-COMP-FILE-ID          PIC  X(08) VALUE 'ERCOMP'.
012413     12  W-NOTE-FILE-ID          PIC  X(08) VALUE 'ERNOTE'.
012413     12  W-EOBC-FILE-ID          PIC  X(08) VALUE 'ELEOBC'.
121213     12  W-ENCC-FILE-ID          PIC  X(08) VALUE 'ELENCC'.
00573      12  W-GETMAIN-SPACE         PIC  X(01) VALUE SPACE.
00574      12  W-LGXX-ID               PIC  X(04) VALUE 'LGXX'.
00575      12  W-LINK-001              PIC  X(05) VALUE 'EL001'.
00576      12  W-LINK-004              PIC  X(05) VALUE 'EL004'.
00577      12  W-LINK-ELDATCV          PIC  X(07) VALUE 'ELDATCV'.
00578      12  W-LOWER-CASE            PIC  X(26)
00579          VALUE 'abcdefghijklmnopqrstuvwxyz'.
00580      12  W-MAIL-FILE-ID          PIC  X(08) VALUE 'ERMAIL'.
00581      12  W-MAP.
00582          16  W-MAP-PREFIX        PIC  X(02) VALUE 'EL'.
00583          16  W-MAP-NUM           PIC  X(04) VALUE '689A'.
00584          16  W-MAP-FILLER        PIC  X(02) VALUE SPACES.
00585      12  W-MAPSET                PIC  X(08) VALUE 'EL689S'.
00586      12  W-PGM-EL1042            PIC  X(08) VALUE 'EL1042'.
00587      12  W-PGM-EL690             PIC  X(08) VALUE 'EL690'.
00588      12  W-PGM-EL626             PIC  X(08) VALUE 'EL626'.
00589      12  W-PNDB-FILE-ID          PIC  X(08) VALUE 'ERPNDB'.
00590      12  W-PRINT-TRANS           PIC  X(04) VALUE 'EXH5'.
00591      12  W-PYAJ-FILE-ID          PIC  X(08) VALUE 'ERPYAJ'.
00592
00593      12  W-TEXT-FILE-ID          PIC  X(08) VALUE 'ELLETR'.
00594      12  W-THIS-PGM              PIC  X(08) VALUE 'EL689'.
00595      12  W-TOP-FORM              PIC  X(70)
00596                               VALUE '*****TOP OF FORM *****'.
00597      12  W-TRANSACTION           PIC  X(04) VALUE 'EXH3'.
00598      12  W-UPPER-CASE            PIC  X(26)
00599          VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
00600      12  W-XCTL-005              PIC  X(05) VALUE 'EL005'.
00601      12  W-XCTL-010              PIC  X(05) VALUE 'EL010'.
00602      12  W-XCTL-626              PIC  X(05) VALUE 'EL626'.
00603                                  EJECT
00604  01  W-VARIABLE-PROCESS-CNTLS.
00605      12  FILLER                  PIC  X(26)
00606                         VALUE 'VARIABLE WORK AREA STARTS:'.
100705     12  W-NUM-OF-VARIABLES      PIC S9(03) VALUE +189 COMP-3.
00608      12  W-VAR-HOLD.
00609          16  W-V1                PIC  X(01).
00610          16  W-V2                PIC  X(01).
00611          16  W-V3                PIC  X(01).
00612      12  FILLER REDEFINES W-VAR-HOLD.
00613          16  W-VAR-RELATIVE-NUM  PIC  9(03).
00614      12  W-FIELD-SQUEEZE-IND     PIC  X(01).
00615          88  W-SQUEEZE-FIELD          VALUE '#'.
00616
00617  01  W-SUPPORTED-VARIABLES.
00618
00619 *****************COMPANY VARIABLES - ELCNTL ********************
00620 *****COMPANY NAME
00621      12  FILLER                  PIC  X(03) VALUE '001'.
00622      12  FILLER                  PIC S9(04) COMP VALUE +30.
00623      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00624      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00625      12  FILLER                  PIC S9(04) COMP VALUE +01.
00626
00627 *****FULL COMPANY ADDRESS
00628      12  FILLER                  PIC  X(03) VALUE '002'.
00629      12  FILLER                  PIC S9(04) COMP VALUE +30.
00630      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00631      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00632      12  FILLER                  PIC S9(04) COMP VALUE +01.
00633
00634      12  FILLER                  PIC  X(03) VALUE '003'.
00635      12  FILLER                  PIC S9(04) COMP VALUE +30.
00636      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00637      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00638      12  FILLER                  PIC S9(04) COMP VALUE +01.
00639
00640      12  FILLER                  PIC  X(03) VALUE '004'.
00641      12  FILLER                  PIC S9(04) COMP VALUE +30.
00642      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00643      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00644      12  FILLER                  PIC S9(04) COMP VALUE +01.
00645
00646      12  FILLER                  PIC  X(03) VALUE '005'.
00647      12  FILLER                  PIC S9(04) COMP VALUE +30.
00648      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00649      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00650      12  FILLER                  PIC S9(04) COMP VALUE +01.
00651
00652      12  FILLER                  PIC  X(03) VALUE '006'.
00653      12  FILLER                  PIC S9(04) COMP VALUE +30.
00654      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00655      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00656      12  FILLER                  PIC S9(04) COMP VALUE +01.
00657
00658 *****REMAINING 4 ARE NOT CURRENTLY USED
00659      12  FILLER                  PIC  X(03) VALUE '007'.
00660      12  FILLER                  PIC S9(04) COMP VALUE +30.
00661      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00662      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00663      12  FILLER                  PIC S9(04) COMP VALUE +01.
00664
00665      12  FILLER                  PIC  X(03) VALUE '008'.
00666      12  FILLER                  PIC S9(04) COMP VALUE +30.
00667      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00668      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00669      12  FILLER                  PIC S9(04) COMP VALUE +01.
00670
00671      12  FILLER                  PIC  X(03) VALUE '009'.
00672      12  FILLER                  PIC S9(04) COMP VALUE +30.
00673      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00674      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00675      12  FILLER                  PIC S9(04) COMP VALUE +01.
00676
00677      12  FILLER                  PIC  X(03) VALUE '010'.
00678      12  FILLER                  PIC S9(04) COMP VALUE +30.
00679      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00680      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00681      12  FILLER                  PIC S9(04) COMP VALUE +01.
00682
00683 ************ LIFE BENEFIT VARIABLES - ELCNTL *******************
00684 *****LIFE BENEFIT DESCRIPTION
00685      12  FILLER                  PIC  X(03) VALUE '011'.
00686      12  FILLER                  PIC S9(04) COMP VALUE +10.
00687      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00688      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00689      12  FILLER                  PIC S9(04) COMP VALUE +02.
00690
00691 *****REMAINING 3 ARE NOT CURRENTLY USED
00692      12  FILLER                  PIC  X(03) VALUE '012'.
00693      12  FILLER                  PIC S9(04) COMP VALUE +30.
00694      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00695      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00696      12  FILLER                  PIC S9(04) COMP VALUE +02.
00697
00698      12  FILLER                  PIC  X(03) VALUE '013'.
00699      12  FILLER                  PIC S9(04) COMP VALUE +30.
00700      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00701      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00702      12  FILLER                  PIC S9(04) COMP VALUE +02.
00703
00704      12  FILLER                  PIC  X(03) VALUE '014'.
00705      12  FILLER                  PIC S9(04) COMP VALUE +30.
00706      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00707      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00708      12  FILLER                  PIC S9(04) COMP VALUE +02.
00709
00710 ************* A&H BENEFIT VARIABLES - ELCNTL *******************
00711 *****AH BENEFIT DESCRIPTION
00712      12  FILLER                  PIC  X(03) VALUE '015'.
00713      12  FILLER                  PIC S9(04) COMP VALUE +10.
00714      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00715      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00716      12  FILLER                  PIC S9(04) COMP VALUE +03.
00717
00718 *****ELIMINATION PERIOD
00719      12  FILLER                  PIC  X(03) VALUE '016'.
00720      12  FILLER                  PIC S9(04) COMP VALUE +2.
00721      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00722      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00723      12  FILLER                  PIC S9(04) COMP VALUE +03.
00724
00725 *****REMAINING 3 ARE NOT CURRENTLY USED
00726      12  FILLER                  PIC  X(03) VALUE '017'.
00727      12  FILLER                  PIC S9(04) COMP VALUE +30.
00728      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00729      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00730      12  FILLER                  PIC S9(04) COMP VALUE +03.
00731
00732      12  FILLER                  PIC  X(03) VALUE '018'.
00733      12  FILLER                  PIC S9(04) COMP VALUE +30.
00734      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00735      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00736      12  FILLER                  PIC S9(04) COMP VALUE +03.
00737
00738      12  FILLER                  PIC  X(03) VALUE '019'.
00739      12  FILLER                  PIC S9(04) COMP VALUE +30.
00740      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00741      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00742      12  FILLER                  PIC S9(04) COMP VALUE +03.
00743
00744 *****************CARRIER VARIABLES - ELCNTL ********************
00745 *****CARRIER NAME
00746      12  FILLER                  PIC  X(03) VALUE '020'.
00747      12  FILLER                  PIC S9(04) COMP VALUE +30.
00748      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00749      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00750      12  FILLER                  PIC S9(04) COMP VALUE +04.
00751
00752 *****FULL CARRIER ADDRESS
00753      12  FILLER                  PIC  X(03) VALUE '021'.
00754      12  FILLER                  PIC S9(04) COMP VALUE +30.
00755      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00756      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00757      12  FILLER                  PIC S9(04) COMP VALUE +04.
00758
00759      12  FILLER                  PIC  X(03) VALUE '022'.
00760      12  FILLER                  PIC S9(04) COMP VALUE +30.
00761      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00762      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00763      12  FILLER                  PIC S9(04) COMP VALUE +04.
00764
00765      12  FILLER                  PIC  X(03) VALUE '023'.
00766      12  FILLER                  PIC S9(04) COMP VALUE +30.
00767      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00768      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00769      12  FILLER                  PIC S9(04) COMP VALUE +04.
00770
00771      12  FILLER                  PIC  X(03) VALUE '024'.
00772      12  FILLER                  PIC S9(04) COMP VALUE +30.
00773      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00774      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00775      12  FILLER                  PIC S9(04) COMP VALUE +04.
00776
00777      12  FILLER                  PIC  X(03) VALUE '025'.
00778      12  FILLER                  PIC S9(04) COMP VALUE +30.
00779      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00780      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00781      12  FILLER                  PIC S9(04) COMP VALUE +04.
00782
00783 *****CARRIER PHONE NUMBER
00784      12  FILLER                  PIC  X(03) VALUE '026'.
00785      12  FILLER                  PIC S9(04) COMP VALUE +12.
00786      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00787      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00788      12  FILLER                  PIC S9(04) COMP VALUE +04.
00789
00790 *****REMAINING 4 ARE NOT CURRENTLY USED
00791      12  FILLER                  PIC  X(03) VALUE '027'.
00792      12  FILLER                  PIC S9(04) COMP VALUE +30.
00793      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00794      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00795      12  FILLER                  PIC S9(04) COMP VALUE +04.
00796
00797      12  FILLER                  PIC  X(03) VALUE '028'.
00798      12  FILLER                  PIC S9(04) COMP VALUE +30.
00799      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00800      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00801      12  FILLER                  PIC S9(04) COMP VALUE +04.
00802
00803      12  FILLER                  PIC  X(03) VALUE '029'.
00804      12  FILLER                  PIC S9(04) COMP VALUE +30.
00805      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00806      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00807      12  FILLER                  PIC S9(04) COMP VALUE +04.
00808
00809      12  FILLER                  PIC  X(03) VALUE '030'.
00810      12  FILLER                  PIC S9(04) COMP VALUE +30.
00811      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00812      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00813      12  FILLER                  PIC S9(04) COMP VALUE +04.
00814
00815 ***************** MAIL VARIABLES - ELMAIL **********************
00816 *****FULL MAIL ADDRESS
00817      12  FILLER                  PIC  X(03) VALUE '031'.
00818      12  FILLER                  PIC S9(04) COMP VALUE +30.
00819      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00820      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00821      12  FILLER                  PIC S9(04) COMP VALUE +05.
00822
00823      12  FILLER                  PIC  X(03) VALUE '032'.
00824      12  FILLER                  PIC S9(04) COMP VALUE +30.
00825      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00826      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00827      12  FILLER                  PIC S9(04) COMP VALUE +05.
00828
00829      12  FILLER                  PIC  X(03) VALUE '033'.
00830      12  FILLER                  PIC S9(04) COMP VALUE +30.
00831      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00832      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00833      12  FILLER                  PIC S9(04) COMP VALUE +05.
00834
00835      12  FILLER                  PIC  X(03) VALUE '034'.
00836      12  FILLER                  PIC S9(04) COMP VALUE +30.
00837      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00838      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00839      12  FILLER                  PIC S9(04) COMP VALUE +05.
00840
00841      12  FILLER                  PIC  X(03) VALUE '035'.
00842      12  FILLER                  PIC S9(04) COMP VALUE +30.
00843      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00844      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00845      12  FILLER                  PIC S9(04) COMP VALUE +05.
00846
00847      12  FILLER                  PIC  X(03) VALUE '036'.
00848      12  FILLER                  PIC S9(04) COMP VALUE +30.
00849      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00850      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00851      12  FILLER                  PIC S9(04) COMP VALUE +05.
      **** CRED BENE NAME FROM ERMAIL
00854      12  FILLER                  PIC  X(03) VALUE '037'.
00855      12  FILLER                  PIC S9(04) COMP VALUE +25.
00856      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00857      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00858      12  FILLER                  PIC S9(04) COMP VALUE +05.
00853 *****REMAINING 2 ARE NOT CURRENTLY USED
00860      12  FILLER                  PIC  X(03) VALUE '038'.
00861      12  FILLER                  PIC S9(04) COMP VALUE +30.
00862      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00863      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00864      12  FILLER                  PIC S9(04) COMP VALUE +05.
00865
00866      12  FILLER                  PIC  X(03) VALUE '039'.
00867      12  FILLER                  PIC S9(04) COMP VALUE +30.
00868      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00869      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00870      12  FILLER                  PIC S9(04) COMP VALUE +05.
00871
00872 *************** ACCOUNT VARIABLES - ERACCT *********************
00873 *****ACCOUNT NAME
00874      12  FILLER                  PIC  X(03) VALUE '040'.
00875      12  FILLER                  PIC S9(04) COMP VALUE +30.
00876      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00877      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00878      12  FILLER                  PIC S9(04) COMP VALUE +06.
00879
00880 *****FULL ACCOUNT ADDRESS
00881      12  FILLER                  PIC  X(03) VALUE '041'.
00882      12  FILLER                  PIC S9(04) COMP VALUE +30.
00883      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00884      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00885      12  FILLER                  PIC S9(04) COMP VALUE +06.
00886
00887      12  FILLER                  PIC  X(03) VALUE '042'.
00888      12  FILLER                  PIC S9(04) COMP VALUE +30.
00889      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00890      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00891      12  FILLER                  PIC S9(04) COMP VALUE +06.
00892
00893      12  FILLER                  PIC  X(03) VALUE '043'.
00894      12  FILLER                  PIC S9(04) COMP VALUE +30.
00895      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00896      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00897      12  FILLER                  PIC S9(04) COMP VALUE +06.
00898
00899      12  FILLER                  PIC  X(03) VALUE '044'.
00900      12  FILLER                  PIC S9(04) COMP VALUE +30.
00901      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00902      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00903      12  FILLER                  PIC S9(04) COMP VALUE +06.
00904
00905      12  FILLER                  PIC  X(03) VALUE '045'.
00906      12  FILLER                  PIC S9(04) COMP VALUE +30.
00907      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00908      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
00909      12  FILLER                  PIC S9(04) COMP VALUE +06.
00910
00911 *****ACCOUNT PHONE NUMBER
00912      12  FILLER                  PIC  X(03) VALUE '046'.
00913      12  FILLER                  PIC S9(04) COMP VALUE +12.
00914      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00915      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00916      12  FILLER                  PIC S9(04) COMP VALUE +06.
00917
00918 *****ACCOUNT CONTROL NAME AM-CONTROL-NAME
00919      12  FILLER                  PIC  X(03) VALUE '047'.
00920      12  FILLER                  PIC S9(04) COMP VALUE +30.
00921      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00922      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00923      12  FILLER                  PIC S9(04) COMP VALUE +06.
00924
072308****ACCOUNT CSR CODE
072308     12  FILLER                  PIC  X(03) VALUE '048'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +4..
072308     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072308     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +06.
      ****ACCOUNT ACCOUNT NUMBER
00931      12  FILLER                  PIC  X(03) VALUE '049'.
00932      12  FILLER                  PIC S9(04) COMP VALUE +10.
00933      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00934      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00935      12  FILLER                  PIC S9(04) COMP VALUE +06.
00936
      **** CRED BENE ADDRESS LINE 1 ERMAIL
00937      12  FILLER                  PIC  X(03) VALUE '050'.
00938      12  FILLER                  PIC S9(04) COMP VALUE +30.
00939      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00940      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00941      12  FILLER                  PIC S9(04) COMP VALUE +05.
00942
      **** CRED BENE ADDRESS LINE 2 ERMAIL
00943      12  FILLER                  PIC  X(03) VALUE '051'.
00944      12  FILLER                  PIC S9(04) COMP VALUE +30.
00945      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00946      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00947      12  FILLER                  PIC S9(04) COMP VALUE +05.
00948
      **** CRED BENE CITY STATE   ERMAIL
00949      12  FILLER                  PIC  X(03) VALUE '052'.
00950      12  FILLER                  PIC S9(04) COMP VALUE +30.
00951      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00952      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00953      12  FILLER                  PIC S9(04) COMP VALUE +05.
00954
      **** CRED BENE ZIP  1 ERMAIL
00955      12  FILLER                  PIC  X(03) VALUE '053'.
00956      12  FILLER                  PIC S9(04) COMP VALUE +30.
00957      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00958      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00959      12  FILLER                  PIC S9(04) COMP VALUE +05.
00960
00918 *****REMAINING 6 ARE NOT CURRENTLY USED
00961      12  FILLER                  PIC  X(03) VALUE '054'.
00962      12  FILLER                  PIC S9(04) COMP VALUE +30.
00963      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00964      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00965      12  FILLER                  PIC S9(04) COMP VALUE +06.
00966
00967      12  FILLER                  PIC  X(03) VALUE '055'.
00968      12  FILLER                  PIC S9(04) COMP VALUE +30.
00969      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00970      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00971      12  FILLER                  PIC S9(04) COMP VALUE +06.
00972
00973      12  FILLER                  PIC  X(03) VALUE '056'.
00974      12  FILLER                  PIC S9(04) COMP VALUE +30.
00975      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00976      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00977      12  FILLER                  PIC S9(04) COMP VALUE +06.
00978
00979      12  FILLER                  PIC  X(03) VALUE '057'.
00980      12  FILLER                  PIC S9(04) COMP VALUE +30.
00981      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00982      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00983      12  FILLER                  PIC S9(04) COMP VALUE +06.
00984
00985      12  FILLER                  PIC  X(03) VALUE '058'.
00986      12  FILLER                  PIC S9(04) COMP VALUE +30.
00987      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00988      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00989      12  FILLER                  PIC S9(04) COMP VALUE +06.
00990
00991      12  FILLER                  PIC  X(03) VALUE '059'.
00992      12  FILLER                  PIC S9(04) COMP VALUE +30.
00993      12  FILLER                  PIC  X(30) VALUE ALL '*'.
00994      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
00995      12  FILLER                  PIC S9(04) COMP VALUE +06.
00996
00997 *************** NON FILE VARIABLES *****************************
00998 *****CURRENT DATE
00999      12  FILLER                  PIC  X(03) VALUE '060'.
01000      12  FILLER                  PIC S9(04) COMP VALUE +08.
01001      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01002      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01003      12  FILLER                  PIC S9(04) COMP VALUE +07.
01004
01005 *****FULL CURRENT DATE
01006      12  FILLER                  PIC  X(03) VALUE '061'.
01007      12  FILLER                  PIC S9(04) COMP VALUE +18.
01008      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01009      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01010      12  FILLER                  PIC S9(04) COMP VALUE +07.
01011
01012 *****FORM
01013      12  FILLER                  PIC  X(03) VALUE '062'.
01014      12  FILLER                  PIC S9(04) COMP VALUE +04.
01015      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01016      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01017      12  FILLER                  PIC S9(04) COMP VALUE +07.
01018
01019 *****VARIABLE 1
01020      12  FILLER                  PIC  X(03) VALUE '063'.
01021      12  FILLER                  PIC S9(04) COMP VALUE +30.
01022      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01023      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01024      12  FILLER                  PIC S9(04) COMP VALUE +07.
01025
01026 *****VARIABLE 2
01027      12  FILLER                  PIC  X(03) VALUE '064'.
01028      12  FILLER                  PIC S9(04) COMP VALUE +30.
01029      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01030      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01031      12  FILLER                  PIC S9(04) COMP VALUE +07.
01032
01033 *****VARIABLE 3
01034      12  FILLER                  PIC  X(03) VALUE '065'.
01035      12  FILLER                  PIC S9(04) COMP VALUE +30.
01036      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01037      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01038      12  FILLER                  PIC S9(04) COMP VALUE +07.
01039
01040 *****VARIABLE 4
01041      12  FILLER                  PIC  X(03) VALUE '066'.
01042      12  FILLER                  PIC S9(04) COMP VALUE +30.
01043      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01044      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01045      12  FILLER                  PIC S9(04) COMP VALUE +07.
01046
01047 *****REMAINING 3 ARE NOT CURRENTLY USED
01048      12  FILLER                  PIC  X(03) VALUE '067'.
01049      12  FILLER                  PIC S9(04) COMP VALUE +30.
01050      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01051      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01052      12  FILLER                  PIC S9(04) COMP VALUE +07.
01053
01054      12  FILLER                  PIC  X(03) VALUE '068'.
01055      12  FILLER                  PIC S9(04) COMP VALUE +30.
01056      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01057      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01058      12  FILLER                  PIC S9(04) COMP VALUE +07.
01059
01060      12  FILLER                  PIC  X(03) VALUE '069'.
01061      12  FILLER                  PIC S9(04) COMP VALUE +30.
01062      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01063      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01064      12  FILLER                  PIC S9(04) COMP VALUE +07.
01065
01066 ************** CERTIFICATE VARIABLES - ELCERT *****************
01067 *****CARRIER CODE IN CERT
01068      12  FILLER                  PIC  X(03) VALUE '070'.
01069      12  FILLER                  PIC S9(04) COMP VALUE +1.
01070      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01071      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01072      12  FILLER                  PIC S9(04) COMP VALUE +08.
01073
01074 *****GROUPING CODE IN CERT
01075      12  FILLER                  PIC  X(03) VALUE '071'.
01076      12  FILLER                  PIC S9(04) COMP VALUE +06.
01077      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01078      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01079      12  FILLER                  PIC S9(04) COMP VALUE +08.
01080
01081 *****ACCOUNT NUMBER IN CERT
01082      12  FILLER                  PIC  X(03) VALUE '072'.
01083      12  FILLER                  PIC S9(04) COMP VALUE +10.
01084      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01085      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01086      12  FILLER                  PIC S9(04) COMP VALUE +08.
01087
01088 *****CERTIFICATE NUMBER
01089      12  FILLER                  PIC  X(03) VALUE '073'.
01090      12  FILLER                  PIC S9(04) COMP VALUE +11.
01091      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01092      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01093      12  FILLER                  PIC S9(04) COMP VALUE +08.
01094
01095 *****CERT EFFECTIVE DATE
01096      12  FILLER                  PIC  X(03) VALUE '074'.
01097      12  FILLER                  PIC S9(04) COMP VALUE +08.
01098      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01099      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01100      12  FILLER                  PIC S9(04) COMP VALUE +08.
01101
01102 *****CERT EXPIRATION DATE (LIFE)
01103      12  FILLER                  PIC  X(03) VALUE '075'.
01104      12  FILLER                  PIC S9(04) COMP VALUE +08.
01105      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01106      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01107      12  FILLER                  PIC S9(04) COMP VALUE +08.
01108
01109 *****CERT EXPIRATION DATE (AH)
01110      12  FILLER                  PIC  X(03) VALUE '076'.
01111      12  FILLER                  PIC S9(04) COMP VALUE +08.
01112      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01113      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01114      12  FILLER                  PIC S9(04) COMP VALUE +08.
01115
01116 *****LIFE TERM
01117      12  FILLER                  PIC  X(03) VALUE '077'.
01118      12  FILLER                  PIC S9(04) COMP VALUE +3.
01119      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01120      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01121      12  FILLER                  PIC S9(04) COMP VALUE +08.
01122
01123 *****AH  TERM
01124      12  FILLER                  PIC  X(03) VALUE '078'.
01125      12  FILLER                  PIC S9(04) COMP VALUE +3.
01126      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01127      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01128      12  FILLER                  PIC S9(04) COMP VALUE +08.
01129
01130 *****LIFE COVERAGE AMOUNT
01131      12  FILLER                  PIC  X(03) VALUE '079'.
01132      12  FILLER                  PIC S9(04) COMP VALUE +15.
01133      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01134      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01135      12  FILLER                  PIC S9(04) COMP VALUE +08.
01136
01137 *****AH MONTHLY BENEFIT
01138      12  FILLER                  PIC  X(03) VALUE '080'.
01139      12  FILLER                  PIC S9(04) COMP VALUE +13.
01140      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01141      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01142      12  FILLER                  PIC S9(04) COMP VALUE +08.
01143
01144 *****LIFE CANCEL DATE
01145      12  FILLER                  PIC  X(03) VALUE '081'.
01146      12  FILLER                  PIC S9(04) COMP VALUE +08.
01147      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01148      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01149      12  FILLER                  PIC S9(04) COMP VALUE +08.
01150
01151 *****AH CANCEL DATE
01152      12  FILLER                  PIC  X(03) VALUE '082'.
01153      12  FILLER                  PIC S9(04) COMP VALUE +08.
01154      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01155      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01156      12  FILLER                  PIC S9(04) COMP VALUE +08.
01157
01158 *****LIFE COVERAGE FORM NUMBER
01159      12  FILLER                  PIC  X(03) VALUE '083'.
01160      12  FILLER                  PIC S9(04) COMP VALUE +12.
01161      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01162      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01163      12  FILLER                  PIC S9(04) COMP VALUE +08.
01164
01165 *****UNUSED
01166      12  FILLER                  PIC  X(03) VALUE '084'.
01167      12  FILLER                  PIC S9(04) COMP VALUE +12.
01168      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01169      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01170      12  FILLER                  PIC S9(04) COMP VALUE +08.
01171
01172 *****INSUREDS AGE AT POLICY ISSUE (NOT USED)
01173      12  FILLER                  PIC  X(03) VALUE '085'.
01174      12  FILLER                  PIC S9(04) COMP VALUE +3.
01175      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01176      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01177      12  FILLER                  PIC S9(04) COMP VALUE +08.
01178
01179 *****LOAN NUMBER
01180      12  FILLER                  PIC  X(03) VALUE '086'.
01181      12  FILLER                  PIC S9(04) COMP VALUE +08.
01182      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01183      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01184      12  FILLER                  PIC S9(04) COMP VALUE +08.
01185
01186 *****LOAN BALANCE
01187      12  FILLER                  PIC  X(03) VALUE '087'.
01188      12  FILLER                  PIC S9(04) COMP VALUE +13.
01189      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01190      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01191      12  FILLER                  PIC S9(04) COMP VALUE +08.
01192
01193 *****MEMBER NUMBER
01194      12  FILLER                  PIC  X(03) VALUE '088'.
01195      12  FILLER                  PIC S9(04) COMP VALUE +12.
01196      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01197      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01198      12  FILLER                  PIC S9(04) COMP VALUE +08.
01199
01200 *****INSURED SOC SEC NUMBER
01201      12  FILLER                  PIC  X(03) VALUE '089'.
01202      12  FILLER                  PIC S9(04) COMP VALUE +11.
01203      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01204      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01205      12  FILLER                  PIC S9(04) COMP VALUE +08.
01206
01207 *****INSURED INITIALS & LAST NAME (CERTIFICATE)
01208      12  FILLER                  PIC  X(03) VALUE '090'.
01209      12  FILLER                  PIC S9(04) COMP VALUE +15.
01210      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01211      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01212      12  FILLER                  PIC S9(04) COMP VALUE +08.
01213
01214 *****INSURED FIRST NAME (CERTIFICATE)
01215      12  FILLER                  PIC  X(03) VALUE '091'.
01216      12  FILLER                  PIC S9(04) COMP VALUE +10.
01217      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01218      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01219      12  FILLER                  PIC S9(04) COMP VALUE +08.
01220
01221 *****INSURED MIDDLE INITIAL (CERTIFICATE)
01222      12  FILLER                  PIC  X(03) VALUE '092'.
01223      12  FILLER                  PIC S9(04) COMP VALUE +05.
01224      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01225      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01226      12  FILLER                  PIC S9(04) COMP VALUE +08.
01227
01228 *****ORIG TERM * MON BEN
01229      12  FILLER                  PIC  X(03) VALUE '093'.
01230      12  FILLER                  PIC S9(04) COMP VALUE +15.
01231      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01232      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01233      12  FILLER                  PIC S9(04) COMP VALUE +08.
01234
01235 *****INSURED'S NAME (LAST, FIRST, INIT)
01236      12  FILLER                  PIC  X(03) VALUE '094'.
01237      12  FILLER                  PIC S9(04) COMP VALUE +30.
01238      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01239      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01240      12  FILLER                  PIC S9(04) COMP VALUE +08.
01241
01242 *****INSURED'S NAME (FIRST, INIT, LAST)
01243      12  FILLER                  PIC  X(03) VALUE  '095'.
01244      12  FILLER                  PIC S9(04) COMP VALUE +30.
01245      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01246      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01247      12  FILLER                  PIC S9(04) COMP VALUE +08.
01248
01249 *****TITLE (MR/MS)
01250      12  FILLER                  PIC  X(03) VALUE '096'.
01251      12  FILLER                  PIC S9(04) COMP VALUE +3.
01252      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01253      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01254      12  FILLER                  PIC S9(04) COMP VALUE +08.
01255
01256 *****LIFE PREMIUM (CERTIFICATE)
01257      12  FILLER                  PIC  X(03) VALUE '097'.
01258      12  FILLER                  PIC S9(04) COMP VALUE +15.
01259      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01260      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01261      12  FILLER                  PIC S9(04) COMP VALUE +08.
01262
01263 *****A/H PREMIUM (CERTIFICATE)
01264      12  FILLER                  PIC  X(03) VALUE '098'.
01265      12  FILLER                  PIC S9(04) COMP VALUE +13.
01266      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01267      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01268      12  FILLER                  PIC S9(04) COMP VALUE +08.
01269
01270 *****JOINT'S INITIALS & LAST NAME (CERTIFICATE)
01271      12  FILLER                  PIC  X(03) VALUE '099'.
01272      12  FILLER                  PIC S9(04) COMP VALUE +15.
01273      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01274      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01275      12  FILLER                  PIC S9(04) COMP VALUE +08.
01276
01277 *****JOINT'S FIRST NAME (CERTIFICATE)
01278      12  FILLER                  PIC  X(03) VALUE '100'.
01279      12  FILLER                  PIC S9(04) COMP VALUE +10.
01280      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01281      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01282      12  FILLER                  PIC S9(04) COMP VALUE +08.
01283
01284 *****JOINT'S MIDDLE INITIAL (CERTIFICATE)
01285      12  FILLER                  PIC  X(03) VALUE '101'.
01286      12  FILLER                  PIC S9(04) COMP VALUE +05.
01287      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01288      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01289      12  FILLER                  PIC S9(04) COMP VALUE +08.
01290
01291 *****JOINT'S NAME (LAST, FIRST, INIT)
01292      12  FILLER                  PIC  X(03) VALUE '102'.
01293      12  FILLER                  PIC S9(04) COMP VALUE +30.
01294      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01295      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01296      12  FILLER                  PIC S9(04) COMP VALUE +08.
01297
01298 *****JOINT'S NAME (FIRST, INIT, LAST)
01299      12  FILLER                  PIC  X(03) VALUE '103'.
01300      12  FILLER                  PIC S9(04) COMP VALUE +30.
01301      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01302      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01303      12  FILLER                  PIC S9(04) COMP VALUE +08.
01304
01305 *****INSURED'S FIRST AND LAST NAME
01306      12  FILLER                  PIC  X(03) VALUE '104'.
01307      12  FILLER                  PIC S9(04) COMP VALUE +30.
01308      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01309      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01310      12  FILLER                  PIC S9(04) COMP VALUE +08.
01311
01312 *****JOINT'S FIRST AND LAST NAME
01313      12  FILLER                  PIC  X(03) VALUE '105'.
01314      12  FILLER                  PIC S9(04) COMP VALUE +30.
01315      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01316      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01317      12  FILLER                  PIC S9(04) COMP VALUE +08.
01318
01319 *****ENTERED LIFE REFUND (CERT)
01320      12  FILLER                  PIC  X(03) VALUE '106'.
01321      12  FILLER                  PIC S9(04) COMP VALUE +13.
01322      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01323      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01324      12  FILLER                  PIC S9(04) COMP VALUE +08.
01325
01326 *****ENTERED A/H REFUND (CERT)
01327      12  FILLER                  PIC  X(03) VALUE '107'.
01328      12  FILLER                  PIC S9(04) COMP VALUE +13.
01329      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01330      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01331      12  FILLER                  PIC S9(04) COMP VALUE +08.
01332
01333 *****INSURED'S LAST NAME
01334      12  FILLER                  PIC  X(03) VALUE '108'.
01335      12  FILLER                  PIC S9(04) COMP VALUE +15.
01336      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01337      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01338      12  FILLER                  PIC S9(04) COMP VALUE +08.
01339
01340 *****BENEFICIARY
01341      12  FILLER                  PIC  X(03) VALUE  '109'.
01342      12  FILLER                  PIC S9(04) COMP VALUE +25.
01343      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01344      12  FILLER                  PIC  X(01) VALUE 'Y'.
01345      12  FILLER                  PIC S9(04) COMP VALUE +08.
01346
01347 ************** PENDING VARIABLES - ERPNDB *********************
01348 *****INSURED DATE OF BIRTH
01349      12  FILLER                  PIC  X(03) VALUE '110'.
01350      12  FILLER                  PIC S9(04) COMP VALUE +08.
01351      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01352      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01353      12  FILLER                  PIC S9(04) COMP VALUE +09.
01354
01355 *****ENTERED LIFE PREMIUM (PENDING)
01356      12  FILLER                  PIC  X(03) VALUE '111'.
01357      12  FILLER                  PIC S9(04) COMP VALUE +13.
01358      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01359      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01360      12  FILLER                  PIC S9(04) COMP VALUE +09.
01361
01362 *****ENTERED A/H PREMIUM (PENDING)
01363      12  FILLER                  PIC  X(03) VALUE '112'.
01364      12  FILLER                  PIC S9(04) COMP VALUE +13.
01365      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01366      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01367      12  FILLER                  PIC S9(04) COMP VALUE +09.
01368
01369 *****CALCULATED LIFE PREMIUM (PENDING)
01370      12  FILLER                  PIC  X(03) VALUE '113'.
01371      12  FILLER                  PIC S9(04) COMP VALUE +13.
01372      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01373      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01374      12  FILLER                  PIC S9(04) COMP VALUE +09.
01375
01376 *****CALCULATED A/H PREMIUM (PENDING)
01377      12  FILLER                  PIC  X(03) VALUE '114'.
01378      12  FILLER                  PIC S9(04) COMP VALUE +13.
01379      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01380      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01381      12  FILLER                  PIC S9(04) COMP VALUE +09.
01382
01383 *****DIFFERENCE ENTER/COMPUTED LIFE PREMIUM (PENDING)
01384      12  FILLER                  PIC  X(03) VALUE '115'.
01385      12  FILLER                  PIC S9(04) COMP VALUE +13.
01386      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01387      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01388      12  FILLER                  PIC S9(04) COMP VALUE +09.
01389
01390 *****DIFFERENCE ENTER/COMPUTED A/H PREMIUM (PENDING)
01391      12  FILLER                  PIC  X(03) VALUE '116'.
01392      12  FILLER                  PIC S9(04) COMP VALUE +13.
01393      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01394      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01395      12  FILLER                  PIC S9(04) COMP VALUE +09.
01396
01397 *****PRIOR CANCEL DATE
01398      12  FILLER                  PIC  X(03) VALUE '117'.
01399      12  FILLER                  PIC S9(04) COMP VALUE +08.
01400      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01401      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01402      12  FILLER                  PIC S9(04) COMP VALUE +09.
01403
01404 *****ENTERED LIFE REFUND (PENDING)
01405      12  FILLER                  PIC  X(03) VALUE '118'.
01406      12  FILLER                  PIC S9(04) COMP VALUE +13.
01407      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01408      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01409      12  FILLER                  PIC S9(04) COMP VALUE +09.
01410
01411 *****ENTERED A/H REFUND (PENDING)
01412      12  FILLER                  PIC  X(03) VALUE '119'.
01413      12  FILLER                  PIC S9(04) COMP VALUE +13.
01414      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01415      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01416      12  FILLER                  PIC S9(04) COMP VALUE +09.
01417
01418 *****CALCULATED LIFE REFUND (PENDING)
01419      12  FILLER                  PIC  X(03) VALUE '120'.
01420      12  FILLER                  PIC S9(04) COMP VALUE +13.
01421      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01422      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01423      12  FILLER                  PIC S9(04) COMP VALUE +09.
01424
01425 *****CALCULATED A/H REFUND (PENDING)
01426      12  FILLER                  PIC  X(03) VALUE '121'.
01427      12  FILLER                  PIC S9(04) COMP VALUE +13.
01428      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01429      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01430      12  FILLER                  PIC S9(04) COMP VALUE +09.
01431
01432 *****DIFFERENCE ENTER/COMPUTED LIFE REFUND (PENDING)
01433      12  FILLER                  PIC  X(03) VALUE '122'.
01434      12  FILLER                  PIC S9(04) COMP VALUE +13.
01435      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01436      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01437      12  FILLER                  PIC S9(04) COMP VALUE +09.
01438
01439 *****DIFFERENCE ENTER/COMPUTED A/H REFUND (PENDING)
01440      12  FILLER                  PIC  X(03) VALUE '123'.
01441      12  FILLER                  PIC S9(04) COMP VALUE +13.
01442      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01443      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01444      12  FILLER                  PIC S9(04) COMP VALUE +09.
01445
01446 *****INSUREDS AGE
01447      12  FILLER                  PIC  X(03) VALUE '124'.
01448      12  FILLER                  PIC S9(04) COMP VALUE +3.
01449      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01450      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01451      12  FILLER                  PIC S9(04) COMP VALUE +09.
01452
01453 *****LIFE BENEFIT (PENDING)
01454      12  FILLER                  PIC  X(03) VALUE '125'.
01455      12  FILLER                  PIC S9(04) COMP VALUE +15.
01456      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01457      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01458      12  FILLER                  PIC S9(04) COMP VALUE +09.
01459
01460 *****A/H BENEFIT (PENDING)
01461      12  FILLER                  PIC  X(03) VALUE '126'.
01462      12  FILLER                  PIC S9(04) COMP VALUE +13.
01463      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01464      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01465      12  FILLER                  PIC S9(04) COMP VALUE +09.
01466
01467 *****LIFE RATE
01468      12  FILLER                  PIC  X(03) VALUE '127'.
01469      12  FILLER                  PIC S9(04) COMP VALUE +08.
01470      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01471      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01472      12  FILLER                  PIC S9(04) COMP VALUE +09.
01473
01474 *****A/H RATE
01475      12  FILLER                  PIC  X(03) VALUE '128'.
01476      12  FILLER                  PIC S9(04) COMP VALUE +08.
01477      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01478      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01479      12  FILLER                  PIC S9(04) COMP VALUE +09.
01480
01481 *****TERM (PENDING)
01482      12  FILLER                  PIC  X(03) VALUE '129'.
01483      12  FILLER                  PIC S9(04) COMP VALUE +3.
01484      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01485      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01486      12  FILLER                  PIC S9(04) COMP VALUE +09.
01487
01488 *****BATCH NUMBER
01489      12  FILLER                  PIC  X(03) VALUE '130'.
01490      12  FILLER                  PIC S9(04) COMP VALUE +06.
01491      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01492      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01493      12  FILLER                  PIC S9(04) COMP VALUE +09.
01494
072908*****TOTAL OF LIFE AND A&H REFUND
072908     12  FILLER                  PIC  X(03) VALUE '131'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +13.
072908     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +08.
01501
072908*****NH INTEREST ON REFUNDS
072908     12  FILLER                  PIC  X(03) VALUE '132'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +13.
072908     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +08.
072908*****GREATER OF THE LIFE AND AH CANCEL DATE
072908     12  FILLER                  PIC  X(03) VALUE '133'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +8.
072908     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072908     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
072908     12  FILLER                  PIC S9(04) COMP VALUE +08.
01513
031504*****THE NEXT 6 NOT CURRENTLY USED
01514      12  FILLER                  PIC  X(03) VALUE '134'.
01515      12  FILLER                  PIC S9(04) COMP VALUE +30.
01516      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01517      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01518      12  FILLER                  PIC S9(04) COMP VALUE +09.
01519
01520      12  FILLER                  PIC  X(03) VALUE '135'.
01521      12  FILLER                  PIC S9(04) COMP VALUE +30.
01522      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01523      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01524      12  FILLER                  PIC S9(04) COMP VALUE +09.
01525
01526      12  FILLER                  PIC  X(03) VALUE '136'.
01527      12  FILLER                  PIC S9(04) COMP VALUE +30.
01528      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01529      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01530      12  FILLER                  PIC S9(04) COMP VALUE +09.
01531
01532      12  FILLER                  PIC  X(03) VALUE '137'.
01533      12  FILLER                  PIC S9(04) COMP VALUE +30.
01534      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01535      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01536      12  FILLER                  PIC S9(04) COMP VALUE +09.
01537
01538      12  FILLER                  PIC  X(03) VALUE '138'.
01539      12  FILLER                  PIC S9(04) COMP VALUE +30.
01540      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01541      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01542      12  FILLER                  PIC S9(04) COMP VALUE +09.
01543
01544      12  FILLER                  PIC  X(03) VALUE '139'.
01545      12  FILLER                  PIC S9(04) COMP VALUE +30.
01546      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01547      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01548      12  FILLER                  PIC S9(04) COMP VALUE +09.
01549
01550 ************** COMPENSATION VARIABLES - ERCOMP ****************
01551 *****COMPENSATION ACCT NAME
01552      12  FILLER                  PIC  X(03) VALUE '140'.
01553      12  FILLER                  PIC S9(04) COMP VALUE +30.
01554      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01555      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01556      12  FILLER                  PIC S9(04) COMP VALUE +10.
01557
01558 *****FULL COMPENSATION ADDRESS TYPE 'A'
01559      12  FILLER                  PIC  X(03) VALUE '141'.
01560      12  FILLER                  PIC S9(04) COMP VALUE +30.
01561      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01562      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01563      12  FILLER                  PIC S9(04) COMP VALUE +10.
01564
01565      12  FILLER                  PIC  X(03) VALUE '142'.
01566      12  FILLER                  PIC S9(04) COMP VALUE +30.
01567      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01568      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01569      12  FILLER                  PIC S9(04) COMP VALUE +10.
01570
01571      12  FILLER                  PIC  X(03) VALUE '143'.
01572      12  FILLER                  PIC S9(04) COMP VALUE +30.
01573      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01574      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01575      12  FILLER                  PIC S9(04) COMP VALUE +10.
01576
01577      12  FILLER                  PIC  X(03) VALUE '144'.
01578      12  FILLER                  PIC S9(04) COMP VALUE +30.
01579      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01580      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01581      12  FILLER                  PIC S9(04) COMP VALUE +10.
01582
01583      12  FILLER                  PIC  X(03) VALUE '145'.
01584      12  FILLER                  PIC S9(04) COMP VALUE +30.
01585      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01586      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01587      12  FILLER                  PIC S9(04) COMP VALUE +10.
01588
01589      12  FILLER                  PIC  X(03) VALUE '146'.
01590      12  FILLER                  PIC S9(04) COMP VALUE +12.
01591      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01592      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01593      12  FILLER                  PIC S9(04) COMP VALUE +10.
01594
01595 *****COMPENSATION PHONE NUMBER
01596      12  FILLER                  PIC  X(03) VALUE '147'.
01597      12  FILLER                  PIC S9(04) COMP VALUE +12.
01598      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01599      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01600      12  FILLER                  PIC S9(04) COMP VALUE +10.
01601
01602 *****COMPENSATION CSR NAME
01603      12  FILLER                  PIC  X(03) VALUE '148'.
01604      12  FILLER                  PIC S9(04) COMP VALUE +30.
01605      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01606      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01607      12  FILLER                  PIC S9(04) COMP VALUE +10.
01608
01609 *****COMPENSATION LAST STATEMENT DATE
01610      12  FILLER                  PIC  X(03) VALUE '149'.
01611      12  FILLER                  PIC S9(04) COMP VALUE +18.
01612      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01613      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01614      12  FILLER                  PIC S9(04) COMP VALUE +10.
01615
01616 *****COMPENSATION ENDING BALANCE
01617      12  FILLER                  PIC  X(03) VALUE '150'.
01618      12  FILLER                  PIC S9(04) COMP VALUE +14.
01619      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01620      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01621      12  FILLER                  PIC S9(04) COMP VALUE +10.
01622
01623 ******************  PROCESSOR DATA - ELCNTL (2) ****************
01624 *****EXECUTING PROCESSOR NAME
01625      12  FILLER                  PIC  X(03) VALUE '151'.
01626      12  FILLER                  PIC S9(04) COMP VALUE +30.
01627      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01628      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01629      12  FILLER                  PIC S9(04) COMP VALUE +11.
01630
01631 *****PROCESSOR TITLE
01632      12  FILLER                  PIC  X(03) VALUE '152'.
01633      12  FILLER                  PIC S9(04) COMP VALUE +26.
01634      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01635      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01636      12  FILLER                  PIC S9(04) COMP VALUE +11.
01637
01638 *****PROCESSOR
01639      12  FILLER                  PIC  X(03) VALUE '153'.
01640      12  FILLER                  PIC S9(04) COMP VALUE +04.
01641      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01642      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01643      12  FILLER                  PIC S9(04) COMP VALUE +11.
01644
072308*****CSR TITLE
072308     12  FILLER                  PIC  X(03) VALUE '154'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +30.
072308     12  FILLER                  PIC  X(30) VALUE ALL '*'.
072308     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
072308     12  FILLER                  PIC S9(04) COMP VALUE +11.
01645 *****REMAINING 2 ARE NOT CURRENTLY USED
01652      12  FILLER                  PIC  X(03) VALUE '155'.
01653      12  FILLER                  PIC S9(04) COMP VALUE +30.
01654      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01655      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01656      12  FILLER                  PIC S9(04) COMP VALUE +11.
01657
01658      12  FILLER                  PIC  X(03) VALUE '156'.
01659      12  FILLER                  PIC S9(04) COMP VALUE +30.
01660      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01661      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01662      12  FILLER                  PIC S9(04) COMP VALUE +11.
01663
01664 ******************  CHECK DATA - ERCHEK    *********************
01665 *****CHECK AMOUNT
01666      12  FILLER                  PIC  X(03) VALUE '157'.
01667      12  FILLER                  PIC S9(04) COMP VALUE +13.
01668      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01669      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01670      12  FILLER                  PIC S9(04) COMP VALUE +12.
01671
01672 *****CHECK NUMBER
01673      12  FILLER                  PIC  X(03) VALUE '158'.
01674      12  FILLER                  PIC S9(04) COMP VALUE +7.
01675      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01676      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01677      12  FILLER                  PIC S9(04) COMP VALUE +12.
01678
01679 *****PAYEE 1 NAME
01680      12  FILLER                  PIC  X(03) VALUE '159'.
01681      12  FILLER                  PIC S9(04) COMP VALUE +30.
01682      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01683      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01684      12  FILLER                  PIC S9(04) COMP VALUE +12.
01685
01686 *****PAYEE 2 NAME
01687      12  FILLER                  PIC  X(03) VALUE '160'.
01688      12  FILLER                  PIC S9(04) COMP VALUE +30.
01689      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01690      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01691      12  FILLER                  PIC S9(04) COMP VALUE +12.
01692
01693 *****ADDRSS 1
01694      12  FILLER                  PIC  X(03) VALUE '161'.
01695      12  FILLER                  PIC S9(04) COMP VALUE +30.
01696      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01697      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01698      12  FILLER                  PIC S9(04) COMP VALUE +12.
01699
01700 *****ADDRESS 2
01701      12  FILLER                  PIC  X(03) VALUE '162'.
01702      12  FILLER                  PIC S9(04) COMP VALUE +30.
01703      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01704      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01705      12  FILLER                  PIC S9(04) COMP VALUE +12.
01706
01707 *****PAYEE CITY STATE
01708      12  FILLER                  PIC  X(03) VALUE '163'.
01709      12  FILLER                  PIC S9(04) COMP VALUE +30.
01710      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01711      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01712      12  FILLER                  PIC S9(04) COMP VALUE +12.
01713
01714 *****ZIP CODE
01715      12  FILLER                  PIC  X(03) VALUE '164'.
01716      12  FILLER                  PIC S9(04) COMP VALUE +30.
01717      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01718      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01719      12  FILLER                  PIC S9(04) COMP VALUE +12.
01720
01721 *****CHECK CONTROL
01722      12  FILLER                  PIC  X(03) VALUE '165'.
01723      12  FILLER                  PIC S9(04) COMP VALUE +08.
01724      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01725      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01726      12  FILLER                  PIC S9(04) COMP VALUE +12.
01727
01728 *****REASON FOR CHECK
01729      12  FILLER                  PIC  X(03) VALUE '166'.
01730      12  FILLER                  PIC S9(04) COMP VALUE +25.
01731      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01732      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01733      12  FILLER                  PIC S9(04) COMP VALUE +12.
01734
01735 *****REMAINING 3 ARE NOT CURRENTLY USED
01736      12  FILLER                  PIC  X(03) VALUE '167'.
01737      12  FILLER                  PIC S9(04) COMP VALUE +30.
01738      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01739      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01740      12  FILLER                  PIC S9(04) COMP VALUE +12.
01741
01742      12  FILLER                  PIC  X(03) VALUE '168'.
01743      12  FILLER                  PIC S9(04) COMP VALUE +30.
01744      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01745      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01746      12  FILLER                  PIC S9(04) COMP VALUE +12.
01747
01748      12  FILLER                  PIC  X(03) VALUE '169'.
01749      12  FILLER                  PIC S9(04) COMP VALUE +30.
01750      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01751      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01752      12  FILLER                  PIC S9(04) COMP VALUE +11.
01753
01754
01755 *********** PAYMENT AND ADJUSTMENT DATA - ERPYAJ  **************
01756 *****CHECK AMOUNT - PYAJ
01757      12  FILLER                  PIC  X(03) VALUE '170'.
01758      12  FILLER                  PIC S9(04) COMP VALUE +13.
01759      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01760      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01761      12  FILLER                  PIC S9(04) COMP VALUE +13.
01762
01763 *****CHECK NUMBER - PYAJ
01764      12  FILLER                  PIC  X(03) VALUE '171'.
01765      12  FILLER                  PIC S9(04) COMP VALUE +7.
01766      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01767      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01768      12  FILLER                  PIC S9(04) COMP VALUE +13.
01769
01770 *****CHECK CONTROL - PYAJ
01771      12  FILLER                  PIC  X(03) VALUE '172'.
01772      12  FILLER                  PIC S9(04) COMP VALUE +08.
01773      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01774      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01775      12  FILLER                  PIC S9(04) COMP VALUE +13.
01776
01777 ***** COMMENT - PYAJ
01778      12  FILLER                  PIC  X(03) VALUE '173'.
01779      12  FILLER                  PIC S9(04) COMP VALUE +30.
01780      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01781      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01782      12  FILLER                  PIC S9(04) COMP VALUE +13.
01783
01784 *****NEW FIELDS.
01785 *****COMPENSATION FULL ADDRESS TYPE 'G'
01786      12  FILLER                  PIC  X(03) VALUE '174'.
01787      12  FILLER                  PIC S9(04) COMP VALUE +30.
01788      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01789      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01790      12  FILLER                  PIC S9(04) COMP VALUE +10.
01791
01792      12  FILLER                  PIC  X(03) VALUE '175'.
01793      12  FILLER                  PIC S9(04) COMP VALUE +30.
01794      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01795      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01796      12  FILLER                  PIC S9(04) COMP VALUE +10.
01797
01798      12  FILLER                  PIC  X(03) VALUE '176'.
01799      12  FILLER                  PIC S9(04) COMP VALUE +30.
01800      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01801      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01802      12  FILLER                  PIC S9(04) COMP VALUE +10.
01803
01804      12  FILLER                  PIC  X(03) VALUE '177'.
01805      12  FILLER                  PIC S9(04) COMP VALUE +30.
01806      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01807      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01808      12  FILLER                  PIC S9(04) COMP VALUE +10.
01809
01810      12  FILLER                  PIC  X(03) VALUE '178'.
01811      12  FILLER                  PIC S9(04) COMP VALUE +30.
01812      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01813      12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
01814      12  FILLER                  PIC S9(04) COMP VALUE +10.
01815
01816      12  FILLER                  PIC  X(03) VALUE '179'.
01817      12  FILLER                  PIC S9(04) COMP VALUE +12.
01818      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01819      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01820      12  FILLER                  PIC S9(04) COMP VALUE +10.
01821
01822 *****COMPENSATION FINANCIAL RESPONSIBLE NO.
01823      12  FILLER                  PIC  X(03) VALUE '180'.
01824      12  FILLER                  PIC S9(04) COMP VALUE +10.
01825      12  FILLER                  PIC  X(30) VALUE ALL '*'.
01826      12  FILLER                  PIC  X(01) VALUE ALL 'N'.
01827      12  FILLER                  PIC S9(04) COMP VALUE +10.
100705**** COMPENSATION BILLING SWITCH
100705     12  FILLER                  PIC XXX    VALUE '181'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +7.
100705     12  FILLER                  PIC X(30)  VALUE ALL '*'.
100705     12  FILLER                  PIC X      VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +10.
100705*****COMPENSATION FAX NUMBER
100705     12  FILLER                  PIC  X(03) VALUE '182'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +12.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
100705**** COMPENSATION STATUS
100705     12  FILLER                  PIC XXX    VALUE '183'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +7.
100705     12  FILLER                  PIC X(30)  VALUE ALL '*'.
100705     12  FILLER                  PIC X      VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(4)  COMP VALUE +10.
      ***** THE VARS 184 - 189 WILL TYPICALLY BE USED FOR DCC BANKS
100705*****COMPENSATION FULL ADDRESS TYPE 'B'
100705     12  FILLER                  PIC  X(03) VALUE '184'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +30.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
01791
100705     12  FILLER                  PIC  X(03) VALUE '185'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +30.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
01797
100705     12  FILLER                  PIC  X(03) VALUE '186'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +30.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
01803
100705     12  FILLER                  PIC  X(03) VALUE '187'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +30.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
01809
100705     12  FILLER                  PIC  X(03) VALUE '188'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +30.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'Y'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
01815
100705     12  FILLER                  PIC  X(03) VALUE '189'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +12.
100705     12  FILLER                  PIC  X(30) VALUE ALL '*'.
100705     12  FILLER                  PIC  X(01) VALUE ALL 'N'.
100705     12  FILLER                  PIC S9(04) COMP VALUE +10.
01828
01829
01830  01  FILLER REDEFINES W-SUPPORTED-VARIABLES.
01831 *    12  W-VARIABLE-GRP OCCURS 180 TIMES
100705     12  W-VARIABLE-GRP OCCURS 189 TIMES
01832                         INDEXED BY W-VG-NDX.
01833          16  W-VARIABLE-ID         PIC  X(03).
01834          16  W-VARIABLE-SIZE       PIC S9(04) COMP.
01835          16  W-VG-TEXT.
01836              20  W-VAR-CHAR
01837                         OCCURS 30 TIMES
01838                         INDEXED BY W-VC-NDX
01839                                    PIC  X(01).
01840          16  W-VARIABLE-UPLOW-IND  PIC  X(01).
01841              88  W-USE-UPPER-AND-LOWER-CASE VALUE 'Y'.
01842
01843          16  W-VARIABLE-SOURCE   PIC S9(04) COMP.
01844
01845  01  W-VAR-END                   PIC  X(23)
01846                         VALUE ':VARIABLE WORK AREA END'.
01847
01848  01  W-PROGRAM-TABLE-AREA.
01849      12  FILLER                  PIC  X(15)
01850                                  VALUE 'PROGRAM TABLES:'.
01851
01852      12  W-RECORD-TABLE              VALUE SPACES.
01853          16  W-RC-GRP OCCURS 500 TIMES
01854                        INDEXED BY W-RG-NDX
01855                                   W-RG-NDX1.
01856              20  W-RC-TEXT.
01857                  24  W-RC-CHAR OCCURS 70 TIMES
01858                                 INDEXED BY W-RC-NDX
01859                                            W-RC-NDX1
01860                                  PIC  X(01).
01861              20  W-RC-PC         PIC  9(02).
01862              20  W-RC-SC         PIC  X(01).
01863
01864      12  FILLER REDEFINES W-RECORD-TABLE.
01865          16  W-REC-CHAR OCCURS 36500 TIMES
01866                         INDEXED BY W-RVS-NDX
01867                                    W-RVS-NDX2
01868                                  PIC  X(01).
01869      12  FILLER REDEFINES W-RECORD-TABLE.
01870          16  W-TS-GROUP OCCURS 10 TIMES
01871                         INDEXED BY W-TS-NDX
01872                                  PIC X(3650).
01873
01874      12  FILLER                  PIC  X(11)
01875                                  VALUE 'TEXT TABLE:'.
01876      12  W-TX-TABLE                  VALUE SPACES.
01877          16  W-TX-GRP OCCURS 500 TIMES
01878                        INDEXED BY W-TG-NDX
01879                                   W-TG-NDX2.
01880              20  W-TX-TEXT.
01881                  24  W-TX-CHAR OCCURS 70 TIMES
01882                                     INDEXED BY W-TX-NDX
01883                                                W-TX-NDX1
01884                                                W-TX-NDX2
01885                                  PIC  X(01).
01886              20  W-TX-PC         PIC  9(02).
01887              20  W-TX-SC         PIC  X(01).
01888
01889      12  FILLER                  PIC  X(11)
01890                                  VALUE 'FILE TABLE:'.
01891      12  W-FILE-TABLE                VALUE SPACES.
01892          16  W-FILE-USE-IND OCCURS 20 TIMES
01893                        INDEXED BY W-FILE-NDX
01894                                  PIC  X(01).
01895              88  W-FILE-NOT-USED     VALUE SPACE.
01896              88  W-FILE-USED         VALUE 'Y'.
01897
01898      12  FILLER                  PIC  X(14)
01899                                  VALUE 'END OF TABLES:'.
01900                                  EJECT
01901  01  ERROR-MESSAGES.
01902      12  ER-0000                 PIC  X(04) VALUE '0000'.
01903      12  ER-0004                 PIC  X(04) VALUE '0004'.
01904      12  ER-0006                 PIC  X(04) VALUE '0006'.
01905      12  ER-0008                 PIC  X(04) VALUE '0008'.
01906      12  ER-0013                 PIC  X(04) VALUE '0013'.
01907      12  ER-0023                 PIC  X(04) VALUE '0023'.
01908      12  ER-0029                 PIC  X(04) VALUE '0029'.
01909      12  ER-0033                 PIC  X(04) VALUE '0033'.
01910      12  ER-0042                 PIC  X(04) VALUE '0042'.
01911      12  ER-0047                 PIC  X(04) VALUE '0047'.
01912      12  ER-0051                 PIC  X(04) VALUE '0051'.
01913      12  ER-0066                 PIC  X(04) VALUE '0066'.
01914      12  ER-0067                 PIC  X(04) VALUE '0067'.
01915      12  ER-0070                 PIC  X(04) VALUE '0070'.
01916      12  ER-0168                 PIC  X(04) VALUE '0168'.
01917      12  ER-0169                 PIC  X(04) VALUE '0169'.
01918      12  ER-0174                 PIC  X(04) VALUE '0174'.
01919      12  ER-0175                 PIC  X(04) VALUE '0175'.
01920      12  ER-0176                 PIC  X(04) VALUE '0176'.
01921      12  ER-0177                 PIC  X(04) VALUE '0177'.
01922      12  ER-0179                 PIC  X(04) VALUE '0179'.
01923      12  ER-0180                 PIC  X(04) VALUE '0180'.
01924      12  ER-0181                 PIC  X(04) VALUE '0181'.
01925      12  ER-0182                 PIC  X(04) VALUE '0182'.
01926      12  ER-0184                 PIC  X(04) VALUE '0184'.
01927      12  ER-0185                 PIC  X(04) VALUE '0185'.
01928      12  ER-0187                 PIC  X(04) VALUE '0187'.
01929      12  ER-0188                 PIC  X(04) VALUE '0188'.
01930      12  ER-0189                 PIC  X(04) VALUE '0189'.
01931      12  ER-0190                 PIC  X(04) VALUE '0190'.
01932      12  ER-0191                 PIC  X(04) VALUE '0191'.
01933      12  ER-0215                 PIC  X(04) VALUE '0215'.
01934      12  ER-0279                 PIC  X(04) VALUE '0279'.
01935      12  ER-0280                 PIC  X(04) VALUE '0280'.
01936      12  ER-0412                 PIC  X(04) VALUE '0412'.
01937      12  ER-0413                 PIC  X(04) VALUE '0413'.
01938      12  ER-0454                 PIC  X(04) VALUE '0454'.
01939      12  ER-0533                 PIC  X(04) VALUE '0533'.
01940      12  ER-0537                 PIC  X(04) VALUE '0537'.
122011     12  ER-0715                 PIC  X(04) VALUE '0715'.
122011     12  ER-0894                 PIC  X(04) VALUE '0894'.
061412     12  ER-1560                 PIC  X(04) VALUE '1560'.
101812     12  ER-1565                 PIC  X(04) VALUE '1565'.
122011     12  ER-1778                 PIC  X(04) VALUE '1778'.
           12  ER-1818                 PIC  X(04) VALUE '1818'.
01941      12  ER-2055                 PIC  X(04) VALUE '2055'.
01942      12  ER-2114                 PIC  X(04) VALUE '2114'.
01943      12  ER-2208                 PIC  X(04) VALUE '2208'.
01944      12  ER-2209                 PIC  X(04) VALUE '2209'.
01945      12  ER-2216                 PIC  X(04) VALUE '2216'.
01946      12  ER-2232                 PIC  X(04) VALUE '2232'.
01947      12  ER-2369                 PIC  X(04) VALUE '2369'.
01948      12  ER-2398                 PIC  X(04) VALUE '2398'.
01949      12  ER-2433                 PIC  X(04) VALUE '2433'.
01950      12  ER-2908                 PIC  X(04) VALUE '2908'.
01951      12  ER-2999                 PIC  X(04) VALUE '2999'.
01952      12  ER-3000                 PIC  X(04) VALUE '3000'.
01953      12  ER-3770                 PIC  X(04) VALUE '3770'.
01954      12  ER-3771                 PIC  X(04) VALUE '3771'.
01955      12  ER-3775                 PIC  X(04) VALUE '3775'.
01956      12  ER-3783                 PIC  X(04) VALUE '3783'.
01957      12  ER-7243                 PIC  X(04) VALUE '7243'.
01958      12  ER-7245                 PIC  X(04) VALUE '7245'.
01959      12  ER-7246                 PIC  X(04) VALUE '7246'.
01960      12  ER-7247                 PIC  X(04) VALUE '7247'.
01961      12  ER-7250                 PIC  X(04) VALUE '7250'.
01962      12  ER-7365                 PIC  X(04) VALUE '7365'.
01963      12  ER-7367                 PIC  X(04) VALUE '7367'.
01964      12  ER-7368                 PIC  X(04) VALUE '7368'.
01965      12  ER-7369                 PIC  X(04) VALUE '7369'.
01966      12  ER-7370                 PIC  X(04) VALUE '7370'.
01967      12  ER-7371                 PIC  X(04) VALUE '7371'.
01968      12  ER-7372                 PIC  X(04) VALUE '7372'.
01969      12  ER-7373                 PIC  X(04) VALUE '7373'.
01970      12  ER-7374                 PIC  X(04) VALUE '7374'.
01971      12  ER-7376                 PIC  X(04) VALUE '7376'.
01972      12  ER-7377                 PIC  X(04) VALUE '7377'.
01973      12  ER-7378                 PIC  X(04) VALUE '7378'.
01974      12  ER-7379                 PIC  X(04) VALUE '7379'.
01975      12  ER-7381                 PIC  X(04) VALUE '7381'.
01976      12  ER-7388                 PIC  X(04) VALUE '7388'.
01977      12  ER-7390                 PIC  X(04) VALUE '7390'.
01978      12  ER-7393                 PIC  X(04) VALUE '7393'.
01979      12  ER-7395                 PIC  X(04) VALUE '7395'.
01980      12  ER-7396                 PIC  X(04) VALUE '7396'.
01981      12  ER-7398                 PIC  X(04) VALUE '7398'.
01982      12  ER-8965                 PIC  X(04) VALUE '8965'.
01983      12  ER-9095                 PIC  X(04) VALUE '9095'.
01984      12  ER-9097                 PIC  X(04) VALUE '9097'.
01985      12  ER-9281                 PIC  X(04) VALUE '9281'.
01986      12  ER-9283                 PIC  X(04) VALUE '9283'.
01987      12  ER-9298                 PIC  X(04) VALUE '9298'.
01988      12  ER-9299                 PIC  X(04) VALUE '9299'.
01989      12  ER-9320                 PIC  X(04) VALUE '9320'.
01990      12  ER-9327                 PIC  X(04) VALUE '9327'.
01991      12  ER-9426                 PIC  X(04) VALUE '9426'.
01992      12  ER-9427                 PIC  X(04) VALUE '9427'.
011013     12  ER-9840                 PIC  X(04) VALUE '9840'.
01993                                  EJECT
01994 *    COPY ELCAID.
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
01995  01  FILLER    REDEFINES DFHAID.
01996      12  FILLER                  PIC  X(08).
01997      12  PF-VALUES               PIC  X(01) OCCURS 2.
01998                                  EJECT
01999 *    COPY ELCATTR.
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
02000                                  EJECT
02001 *    COPY ELCDATE.
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
02002                                  EJECT
02003 *    COPY ELCNWA.
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
02004                                  EJECT
02005 *    COPY ELCEMIB.
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
02006  01  EMI-SAVE-AREA               PIC X(400).
02007                                  EJECT
02008 *    COPY ELCLOGOF.
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
02009                                  EJECT
02010 *    COPY ELCSCTM.
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
02011                                  EJECT
02012 *    COPY ELCSCRTY.
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
02013                                  EJECT
02014
02015  01  W-TS-WORK-AREA              PIC X(3650).
02016                                  EJECT
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
02018  01  DFHCOMMAREA                 PIC X(1024).
       01  var  pic x(30).
02019
02020 *    COPY ERCACCT.
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
02021                                  EJECT
02022 *    COPY ERCARCH.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCH.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = LETTERS SENT TO ARCHIVE FILE              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 250  RECFORM = FIXED                           *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCH                        RKP=2,LEN=5     *
00013 *     ALTERNATE PATH1 = ERARCH2 (CERT/RESP)      RKP=07,LEN=35   *
00014 *     ALTERNATE PATH2 = ERARCH3 (FORM NUMBER)    RKP=44,LEN=28   *
00015 *     ALTERNATE PATH3 = ERARCH4 (PROCCESSOR ID)  RKP=73,LEN=28   *
00016 *     ALTERNATE PATH4 = ERARCH5 (ACCOUNT KEY)    RKP=100,LEN=24  *
00017 *     ALTERNATE PATH5 = ERARCH6 (BTCH/CHK KEY)   RKP=124,LEN=11  *
00018 *                                                                *
00019 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00020 ******************************************************************
031011*                   C H A N G E   L O G
031011*
031011* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
031011*-----------------------------------------------------------------
031011*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
031011* EFFECTIVE    NUMBER
031011*-----------------------------------------------------------------
031011* 031011  CR2007070900001  PEMA  ADD FOLLOW-UP LETTER PROCESSING
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
102918* 102918  CR2018080300002  PEMA  ADD ONBASE STUFF
031011******************************************************************
00021  01  LETTER-ARCHIVE.
00022      12  LA-RECORD-ID                PIC  X(02).
00023          88  LA-VALID-ID                VALUE 'LA'.
00024
00025      12  LA-CONTROL-PRIMARY.
00026          16  LA-COMPANY-CD           PIC  X(01).
00027          16  LA-ARCHIVE-NO           PIC S9(08)    COMP.
00028
00029      12  LA-CONTROL-BY-CERT-RESP.
00030          16  LA-COMPANY-CD-A2        PIC  X(01).
00031          16  LA-CERT-NO-A2.
00032              20  LA-CERT-PRIME-A2    PIC  X(10).
00033              20  LA-CERT-SUFFIX-A2   PIC  X(01).
00034          16  LA-RSP-PERSON-A2 REDEFINES LA-CERT-NO-A2.
00035              20  LA-RESP-PERSON-A2   PIC  X(10).
00036              20  LA-TYPE-A2          PIC  X(01).
00037          16  LA-CARRIER-A2           PIC  X(01).
00038          16  LA-GROUPING-A2          PIC  X(06).
00039          16  LA-STATE-A2             PIC  X(02).
00040          16  LA-ACCOUNT-A2           PIC  X(10).
00041          16  LA-EFFECT-DATE-A2       PIC  X(02).
00042          16  LA-ARCHIVE-NO-A2        PIC S9(08)    COMP.
00043
00044      12  LA-CONTROL-BY-FORM.
00045          16  LA-COMPANY-CD-A3        PIC  X(01).
00046          16  LA-FORM-A3              PIC  X(04).
00047          16  LA-CARRIER-A3           PIC  X(01).
00048          16  LA-GROUPING-A3          PIC  X(06).
00049          16  LA-STATE-A3             PIC  X(02).
00050          16  LA-ACCOUNT-A3           PIC  X(10).
00051          16  LA-ARCHIVE-NO-A3        PIC S9(08)    COMP.
00052
00053      12  LA-CONTROL-BY-PROCESSOR.
00054          16  LA-COMPANY-CD-A4        PIC  X(01).
00055          16  LA-PROCESSOR-CD         PIC  X(04).
00056          16  LA-CARRIER-A4           PIC  X(01).
00057          16  LA-GROUPING-A4          PIC  X(06).
00058          16  LA-STATE-A4             PIC  X(02).
00059          16  LA-ACCOUNT-A4           PIC  X(10).
00060          16  LA-ARCHIVE-NO-A4        PIC S9(08)    COMP.
00061
00062      12  LA-CONTROL-BY-KEY-FIELDS.
00063          16  LA-COMPANY-CD-A5        PIC  X(01).
00064          16  LA-CARRIER-A5           PIC  X(01).
00065          16  LA-GROUPING-A5          PIC  X(06).
00066          16  LA-STATE-A5             PIC  X(02).
00067          16  LA-ACCOUNT-A5           PIC  X(10).
00068          16  LA-ARCHIVE-NO-A5        PIC S9(08)    COMP.
00069
00070      12  LA-CONTROL-BY-GROUP-CODE.
00071          16  LA-COMPANY-CD-A6        PIC  X(01).
00072          16  LA-ENTRY-A6.
00073              20  LA-FILLER           PIC  X(02).
00074              20  LA-QUE-CONTROL-A6   PIC S9(08)    COMP.
00075          16  LA-ARCHIVE-NO-A6        PIC S9(08)    COMP.
00076
00077      12  FILLER                      PIC  X(09).
00078
00079      12  LA-HEADER-RECORD.
00080          16  LA-NUMBER-LABEL-LINES   PIC S9(04)    COMP.
00081          16  LA-CREATION-DATE        PIC  X(02).
00082          16  LA-FOLLOW-UP-DATE       PIC  X(02).
070711         16  LA-FINAL-ACT-DATE       REDEFINES
070711               LA-FOLLOW-UP-DATE     PIC  X(02).
00083          16  LA-INITIAL-PRINT-DATE   PIC  X(02).
00084          16  LA-NO-OF-COPIES         PIC S9(01).
00085          16  LA-NO-OF-TEXT-RECORDS   PIC S9(04)    COMP.
00086          16  LA-REPLY-DATE           PIC  X(02).
00087          16  LA-RESEND-DATES.
00090              20  LA-RESEND-DATE      PIC  X(02).
00091              20  LA-SENT-DATE        PIC  X(02).
                   20  FILLER              PIC  X(08).
00099          16  LA-SOURCE-INFORMATION.
00100              20  LA-DATA-SOURCE      PIC  X(01).
00101              20  LA-ADDR-SOURCE      PIC  X(01).
00102          16  LA-STATUS               PIC  X(01).
00103              88  LA-STATUS-ACTIVE         VALUE 'A'.
00104              88  LA-STATUS-COMPLETED      VALUE 'C'.
00105              88  LA-STATUS-ON-HOLD        VALUE 'H'.
00106              88  LA-STATUS-TO-BE-PURGED   VALUE 'X'.
00107              88  LA-STATUS-PURGED         VALUE 'P'.
00108              88  LA-STATUS-VOIDED         VALUE 'V'.
00109          16  LA-LAST-RESENT-PRINT-DATE
00110                                      PIC  X(02).
00111          16  LA-PRINT-RESTRICTION    PIC  X(01).
00112              88  LA-PRINT-ONLY-WHEN-CNTL-GIVEN
00113                                           VALUE 'C'.
00114              88  LA-PRINT-ONLY-WHEN-FORM-GIVEN
00115                                           VALUE 'F'.
00116              88  LA-PRINT-ONLY-WHEN-PROC-GIVEN
00117                                           VALUE 'P'.
00118          16  LA-PURGED-DATE          PIC  X(02).
00119          16  LA-VOIDED-DATE          PIC  X(02).
101705         16  LA-RESEND-LETR          PIC  X(4).
102918         16  LA-VOID-ONBASE-YN       PIC  X.
102918         16  LA-ONBASE-UNIQUE-ID     PIC S9(5) COMP-3.
102918         16  FILLER                  PIC  X(04).
101705*        16  LA-RESEND-LETR-2        PIC  X(4).
101705*        16  LA-RESEND-LETR-3        PIC  X(4).
070711*        16  FILLER                  PIC  X(59).
               16  LA-ARCHIVE-STATUS       PIC  X.
                   88  LA-TEMP                VALUE 'T'.
                   88  LA-QWS                 VALUE 'Q'.
                   88  LA-BATCH               VALUE 'B'.
070711         16  LA-FINAL-ACT-IND        PIC  X(1).
070711         16  LA-VA-DISCLOSURE-IND    PIC  X(1).
110612         16  LA-ENDT-ARCH-NO         PIC S9(8) COMP.
110612         16  LA-ENDT-ARCH-NO-X REDEFINES LA-ENDT-ARCH-NO
110612                                     PIC X(4).
110612         16  FILLER                  PIC  X(42).
00120 *        16  FILLER                  PIC  X(71).
070711         16  LA-LAST-MAINT-DATE      PIC  X(2).
070711         16  LA-LAST-MAINT-TIME      PIC S9(6) COMP-3.
070711         16  LA-LAST-MAINT-TIMEX  REDEFINES LA-LAST-MAINT-TIME
070711                                     PIC  X(4).
070711         16  LA-LAST-UPDATED-BY      PIC  X(4).
00121
00122 ******************************************************************
02023                                  EJECT
02024 *    COPY ERCARCT.
00001 ******************************************************************
00002 *                                                                *
00003 *                            ERCARCT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = TEXT OF ARCHIVED LETTERDS                 *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 1640  RECFORM = FIXED                          *
00011 *                                                                *
00012 *   BASE CLUSTER = ERARCT                        RKP=2,LEN=8     *
00013 *                                                                *
00014 *   LOG = NO                                                     *
00015 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00016 ******************************************************************
070711*                   C H A N G E   L O G
070711*
070711* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
070711*-----------------------------------------------------------------
070711*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
070711* EFFECTIVE    NUMBER
070711*-----------------------------------------------------------------
070711* 070711  CR2011022800001  AJRA  NAPERSOFT CHANGES
070711******************************************************************
00017  01  LETTER-ARCHIVE-TEXT.
00018      12  LT-RECORD-ID                PIC  X(02).
070711         88  LT-VALID-ID                VALUE 'LT'.
00020
00021      12  LT-CONTROL-PRIMARY.
00022          16  LT-COMPANY-CD           PIC  X(01).
00023          16  LT-ARCHIVE-NO           PIC S9(08)    COMP.
00024          16  LT-RECORD-TYPE          PIC  X(01).
00025              88  LT-ADDRESS-DATA        VALUE '1'.
00026              88  LT-TEXT-DATA           VALUE '2'.
070711             88  LT-COMMENT-DATA        VALUE '3'.
00027          16  LT-LINE-SEQ-NO          PIC S9(04)    COMP.
00028
00029      12  FILLER                      PIC  X(28).
00030      12  LT-NUM-LINES-ON-RECORD      PIC S9(04)    COMP.
00031
00032      12  LT-TEXT-RECORD.
00033          16  LT-LETTER-TEXT OCCURS 20 TIMES
00034                             INDEXED BY LT-NDX.
00035              20  LT-TEXT-LINE        PIC  X(70).
00036              20  LT-SKIP-CONTROL     PIC  X(02).
00037                  88  LT-NO-LINES-SKIPPED             VALUE SPACES.
00038                  88  LT-SKIP-TO-NEXT-PAGE            VALUE '99'.
00039              20  FILLER              PIC  X(08).
070711
070711     12  LT-COMMENT-RECORD  REDEFINES LT-TEXT-RECORD.
070711         16  LT-LETTER-COMMENT OCCURS 20 TIMES INDEXED BY LC-NDX.
070711             20  LT-COMMENT-LINE     PIC X(69).
070711             20  LT-COMMENT-CHG-DT   PIC X(02).
070711             20  LT-COMMENT-CHG-BY   PIC X(04).
070711             20  FILLER              PIC X(05).
00040
02025                                  EJECT
02026 *    COPY ELCCERT.
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
02027                                  EJECT
02028 *    COPY ERCCHEK.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCCHEK                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.008                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = CHECK RECORDS                             *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 600    RECFORM = FIXED                         *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERCHEK             RKP=2,LEN=35          *
00013 *       ALTERNATE INDEX = NONE                                   *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
021414*                   C H A N G E   L O G
021414*
021414* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
021414*-----------------------------------------------------------------
021414*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
021414* EFFECTIVE    NUMBER
021414*-----------------------------------------------------------------
021414* 021414    2003053000001  PEMA  changes for auto chk request
021414******************************************************************
00018  01  CHECK-RECORDS.
00019      12  CH-RECORD-ID                      PIC XX.
00020          88  VALID-CH-ID                      VALUE 'CH'.
00021
00022      12  CH-CONTROL-PRIMARY.
00023          16  CH-COMPANY-CD                 PIC X.
00024          16  CH-CARRIER                    PIC X.
00025          16  CH-GROUPING                   PIC X(6).
00026          16  CH-STATE                      PIC XX.
00027          16  CH-ACCOUNT                    PIC X(10).
00028          16  CH-CERT-EFF-DT                PIC XX.
00029          16  CH-CERT-NO.
00030              20  CH-CERT-PRIME             PIC X(10).
00031              20  CH-CERT-SFX               PIC X.
00032          16  CH-SEQUENCE-NO                PIC S9(4)     COMP.
00033
00034      12  CH-RECORDED-DT                    PIC XX.
00035      12  CH-RECORDED-BY                    PIC X(4).
00036      12  CH-LAST-MAINT-HHMMSS              PIC S9(6)     COMP-3.
00037
00038      12  CH-AMOUNT-PAID                    PIC S9(7)V99  COMP-3.
00039      12  CH-CHECK-NO                       PIC X(7).
00040      12  CH-REASON-FOR-CHECK               PIC X(25).
00041      12  CH-CHECK-WRITTEN-DT               PIC XX.
00042      12  FILLER                            PIC X.
00044
00045      12  CH-PAYEE-INFO.
00046          16  CH-PAYEE-NAME-1               PIC X(30).
00047          16  CH-PAYEE-NAME-2               PIC X(30).
00048          16  CH-PAYEE-ADDRESS-1            PIC X(30).
00049          16  CH-PAYEE-ADDRESS-2            PIC X(30).
00050          16  CH-PAYEE-CITY-ST.
021414             20  CH-PAYEE-CITY             PIC X(28).
021414             20  CH-PAYEE-STATE            PIC XX.
00051          16  CH-PAYEE-ZIP-CODE.
00052              20  CH-PAYEE-ZIP.
00053                  24  CH-ZIP-PRI-1ST        PIC X.
00054                      88  CH-CANADIAN-POST-CODE
00055                                            VALUES 'A' THRU 'Z'.
00056                  24  FILLER                PIC X(4).
00057              20  CH-PAYEE-ZIP-EXT          PIC X(4).
00058          16  CH-CANADIAN-POSTAL-CODE REDEFINES CH-PAYEE-ZIP-CODE.
00059              20  CH-CAN-POSTAL-1           PIC XXX.
00060              20  CH-CAN-POSTAL-2           PIC XXX.
00061              20  FILLER                    PIC XXX.
00062
00063      12  CH-CHECK-STUB-TEXT.
00064          16  CH-STUB-LINE-1                PIC X(30).
00065          16  CH-TEXT-LINE-1                PIC X(50).
00066          16  CH-TEXT-LINE-2                PIC X(50).
00067          16  CH-TEXT-LINE-3                PIC X(40).
021414     12  CH-RETURN-TO                      PIC X(30).
00070
00071      12  CH-COMPENSATION-CONTROL.
00072          16  CH-COMP-CARRIER               PIC X.
00073          16  CH-COMP-GROUPING              PIC X(6).
00074          16  CH-COMP-FIN-RESP              PIC X(10).
00075          16  CH-COMP-ACCOUNT               PIC X(10).
00076
00077      12  CH-CREDIT-SELECT-DT               PIC XX.
00078      12  CH-CREDIT-ACCEPT-DT               PIC XX.
00079      12  CH-PAYEE-CODE                     PIC X(6).
00080
00081      12  CH-VOID-DATA.
00082          20  CH-VOID-DT                    PIC XX.
00083          20  CH-VOID-BY                    PIC X(4).
00084          20  CH-VOID-REASON                PIC X(25).
00085
021414     12  CH-APPROVAL-DATA.
021414         20  CH-APPROVAL-DT                PIC XX.
021414         20  CH-APPROVAL-STATUS            PIC X.
021414             88  CH-IN-LIMBO                  VALUE ' '.
021414             88  CH-APPROV-PENDING            VALUE 'P' '2'.
021414             88  CH-APPROVED                  VALUE 'A'.
021414             88  CH-DENIED                    VALUE 'D'.
021414         20  CH-APPROVED-BY                PIC XXXX.
00086      12  CH-CHECK-QUE-CONTROL              PIC S9(8)     COMP.
00087              88  PAYMENT-NOT-QUEUED           VALUE ZERO.
00088      12  CH-CHECK-QUE-SEQUENCE             PIC S9(4)     COMP.
00089
021414     12  ch-released-dt                    pic xx.
021414     12  ch-check-cashed-dt                pic xx.
           12  FILLER                            PIC X.
00090 *    12  CH-CHECK-REFERENCE                PIC X(12).
00091      12  CH-CHECK-ORIGIN-SW                PIC X.
00092              88  CH-REFUND-CHECK              VALUE 'R'.
00093              88  CH-MAINT-CHECK               VALUE 'M'.
00094
00095      12  CH-CANC-DT                        PIC XX.
00096      12  CH-LF-REFUND                      PIC S9(7)V99  COMP-3.
00097      12  CH-AH-REFUND                      PIC S9(7)V99  COMP-3.
00098
00099      12  CH-INSURED-NAME                   PIC X(28).
00100
021414     12  ch-released-by                    pic x(4).
021414     12  ch-csr                            pic x(4).
021414     12  ch-deduct-commission              pic x.
021414         88  ch-deduct-comm                  value 'Y'.
021414         88  ch-do-not-deduct-comm           value 'N'.
00103
           12  FILLER                            PIC X(11).
021414*    12  CH-LETTER-TABLE.
021414*        16  CH-LETTERS OCCURS 3 TIMES
021414*                       INDEXED BY CH-LT-NDX
021414*                                          PIC X(04).
00108
00109      12  FILLER                            PIC X(07).
00110
02029                                  EJECT
02030 *    COPY ELCCNTL.
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
02031                                  EJECT
02032 *    COPY ERCCOMP.
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
02033                                  EJECT
02034 *    COPY ERCMAIL.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCMAIL                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = MAILING DATA CAPTURE RECORDS              *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
CIDMOD*   RECORD SIZE = 374   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ERMAIL                 RKP=2,LEN=33      *
00013 *   ALTERNATE PATH    = NOT USED                                 *
00014 *                                                                *
00015 *   LOG = YES                                                    *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
080406******************************************************************
080406*                   C H A N G E   L O G
080406*
080406* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
080406*-----------------------------------------------------------------
080406*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
080406* EFFECTIVE    NUMBER
080406*-----------------------------------------------------------------
080406* 080406    2006051800002  PEMA  ADD POST CARD MAIL INFO
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
111108* 111108                   PEMA  ADD CRED BENE ADDR2
00017 ******************************************************************
00018
00019  01  MAILING-DATA.
00020      12  MA-RECORD-ID                      PIC XX.
00021          88  VALID-MA-ID                       VALUE 'MA'.
00022
00023      12  MA-CONTROL-PRIMARY.
00024          16  MA-COMPANY-CD                 PIC X.
00025          16  MA-CARRIER                    PIC X.
00026          16  MA-GROUPING.
00027              20  MA-GROUPING-PREFIX        PIC XXX.
00028              20  MA-GROUPING-PRIME         PIC XXX.
00029          16  MA-STATE                      PIC XX.
00030          16  MA-ACCOUNT.
00031              20  MA-ACCOUNT-PREFIX         PIC X(4).
00032              20  MA-ACCOUNT-PRIME          PIC X(6).
00033          16  MA-CERT-EFF-DT                PIC XX.
00034          16  MA-CERT-NO.
00035              20  MA-CERT-PRIME             PIC X(10).
00036              20  MA-CERT-SFX               PIC X.
00037
00038      12  FILLER                            PIC XX.
00039
00040      12  MA-ACCESS-CONTROL.
00041          16  MA-SOURCE-SYSTEM              PIC XX.
00042              88  MA-FROM-CREDIT                VALUE 'CR'.
00043              88  MA-FROM-VSI                   VALUE 'VS'.
00044              88  MA-FROM-WARRANTY              VALUE 'WA'.
00045              88  MA-FROM-OTHER                 VALUE 'OT'.
00046          16  MA-RECORD-ADD-DT              PIC XX.
00047          16  MA-RECORD-ADDED-BY            PIC XXXX.
00048          16  MA-LAST-MAINT-DT              PIC XX.
00049          16  MA-LAST-MAINT-BY              PIC XXXX.
00050          16  MA-LAST-MAINT-HHMMSS          PIC S9(6)       COMP-3.
00051
00052      12  MA-PROFILE-INFO.
00053          16  MA-QUALIFY-CODE-1             PIC XX.
00054          16  MA-QUALIFY-CODE-2             PIC XX.
00055          16  MA-QUALIFY-CODE-3             PIC XX.
00056          16  MA-QUALIFY-CODE-4             PIC XX.
00057          16  MA-QUALIFY-CODE-5             PIC XX.
00058
00059          16  MA-INSURED-LAST-NAME          PIC X(15).
00060          16  MA-INSURED-FIRST-NAME         PIC X(10).
00061          16  MA-INSURED-MIDDLE-INIT        PIC X.
00062          16  MA-INSURED-ISSUE-AGE          PIC 99.
00063          16  MA-INSURED-BIRTH-DT           PIC XX.
00064          16  MA-INSURED-SEX                PIC X.
00065              88  MA-SEX-MALE                   VALUE 'M'.
00066              88  MA-SEX-FEMALE                 VALUE 'F'.
00067          16  MA-INSURED-SOC-SEC-NO         PIC X(11).
00068
080406         16  MA-ADDRESS-CORRECTED          PIC X.
081108         16  MA-JOINT-BIRTH-DT             PIC XX.
00069 *        16  FILLER                        PIC X(12).
00070
00071          16  MA-ADDRESS-LINE-1             PIC X(30).
00072          16  MA-ADDRESS-LINE-2             PIC X(30).
00073          16  MA-CITY-STATE.
                   20  MA-CITY                   PIC X(28).
                   20  MA-ADDR-STATE             PIC XX.
00074          16  MA-ZIP.
00075              20  MA-ZIP-CODE.
00076                  24  MA-ZIP-CODE-1ST       PIC X(1).
00077                      88  MA-CANADIAN-POST-CODE VALUE 'A' THRU 'Z'.
00078                  24  FILLER                PIC X(4).
00079              20  MA-ZIP-PLUS4              PIC X(4).
00080          16  MA-CANADIAN-POSTAL-CODE REDEFINES MA-ZIP.
00081              20  MA-CAN-POSTAL-CODE-1      PIC X(3).
00082              20  MA-CAN-POSTAL-CODE-2      PIC X(3).
00083              20  FILLER                    PIC X(3).
00084
00085          16  MA-PHONE-NO                   PIC 9(11)       COMP-3.
00086
               16  FILLER                        PIC XXX.
00087 *        16  FILLER                        PIC X(10).
00088
           12  MA-CRED-BENE-INFO.
CIDMOD         16  MA-CRED-BENE-NAME                 PIC X(25).
CIDMOD         16  MA-CRED-BENE-ADDR                 PIC X(30).
               16  MA-CRED-BENE-ADDR2                PIC X(30).
CIDMOD         16  MA-CRED-BENE-CTYST.
                   20  MA-CRED-BENE-CITY             PIC X(28).
                   20  MA-CRED-BENE-STATE            PIC XX.
CIDMOD         16  MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-ZIP-CODE.
CIDMOD                 24  MA-CB-ZIP-CODE-1ST        PIC X(1).
CIDMOD                     88  MA-CB-CANADIAN-POST-CODE
                                                 VALUE 'A' THRU 'Z'.
CIDMOD                 24  FILLER                    PIC X(4).
CIDMOD             20  MA-CB-ZIP-PLUS4               PIC X(4).
CIDMOD         16  MA-CB-CANADIAN-POSTAL-CODE
                                  REDEFINES MA-CRED-BENE-ZIP.
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-1       PIC X(3).
CIDMOD             20  MA-CB-CAN-POSTAL-CODE-2       PIC X(3).
CIDMOD             20  FILLER                        PIC X(3).
080406     12  MA-POST-CARD-MAIL-DATA.
080406         16  MA-MAIL-DATA OCCURS 7.
080406             20  MA-MAIL-TYPE              PIC X.
080406                 88  MA-12MO-MAILING           VALUE '1'.
080406                 88  MA-EXP-MAILING            VALUE '2'.
080406             20  MA-MAIL-STATUS            PIC X.
080406                 88  MA-MAIL-ST-MAILED         VALUE '1'.
080406                 88  MA-MAIL-ST-RETURNED       VALUE '2'.
080406                 88  MA-MAIL-ST-NOT-MAILED     VALUE '3'.
080406             20  MA-MAIL-DATE              PIC XX.
080406     12  FILLER                            PIC XX.
           12  FILLER                            PIC XX.
080406*    12  FILLER                            PIC X(30).
00090 ******************************************************************
02035                                  EJECT
02036 *    COPY ERCPNDB.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCPNDB.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.025                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = PENDING NEW BUSINESS (ISSUES AND CANCELS) *
00008 *                                                                *
00009 ******************************************************************
00010 *   NOTE: IF THIS FORMAT IS CHANGED, THE SORT RECORD IN THE      *
00011 *         EL861 A/R SYSTEM MAY NEED TO BE CHANGED.               *
00012 ******************************************************************
00013 *                                                                *
00014 *                                                                *
00015 *   FILE TYPE = VSAM,KSDS                                        *
00016 *   RECORD SIZE = 585  RECFORM = FIXED                           *
00017 *                                                                *
00018 *   BASE CLUSTER = ERPNDB                         RKP=2,LEN=11   *
00019 *       ALTERNATE PATH1 = ERPNDB2  (BY CAR GRP STATE ACCOUNT     *
00020 *                                  EFF-DT CERT CHG-SEQ REC-TYPE) *
00021 *                                                 RKP=13,LEN=36  *
00022 *       ALTERNATE PATH2 = ERPNDB3  (BY CO, ORIGINAL BATCH, SEQ.  *
00023 *                                      AND CHG-SEQ.)             *
00024 *                                                RKP=49,LEN=11   *
00025 *       ALTERNATE PATH3 = ERPNDB4  (BY CO, CSR-ID, BATCH, SEQ.   *
00026 *                                      AND CHG-SEQ.)             *
00027 *                                                RKP=60,LEN=15   *
00028 *                                                                *
00029 *   LOG = NO                                                     *
00030 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00031 ******************************************************************
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100703* 100703    2003080800002  PEMA  ADD SUPERGAP PROCESSING
011904* 011904                   PEMA  ADD TOTAL FEE PROCESSING
040504* 040504    2003080800002  PEMA  ADD DEALER INCENTIVE PROCESSING
020305* 020305    2005020000000  PEMA  ADD CLP STATE TO PNDB RECORD
110105* 110105    2005071200004  PEMA  INCREASE SIZE OF LOAN OFFICER
032306* 032306                   PEMA  ADD BOW LOAN NUMBER
081606* 081606    2006080800002  PEMA  ADD VIN, ISSUES ONLY
073107* 073107  CR2006051600002  PEMA  ADD OVERCHARGE PROCESSING
072308* 072308  CR2007110500003  PEMA  ADD NH REFUND INTEREST PROCESSING
090408* 090408  CR2008040800002  PEMA  ADD JOINT BIRTH DATE PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
072209* 072209  CR2008101500003  AJRA  ADD BORROWER FIRST NAME TO ENDORS
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
071211* 071211  CR2010012700001  PEMA  ADD SPP DEALER DIRECT
073114* 073114  CR2014012300001  PEMA  ADD CU CARRIER 7 PROCESSING
010716* 010716    2015082500001  PEMA CHG POLICY FEE TO CANCEL FEE
010517* 010517  CR2016021600005  PEMA ADD NEW FORCE CODE FOR AGG
062017* 062017  CR2015091000001  PEMA RENAME INTEREST FIELD
012220* 012220  CR2018092700002  TANA ADD LETTER REQUIRED FIELD
122002******************************************************************
00032
00033  01  PENDING-BUSINESS.
00034      12  PB-RECORD-ID                     PIC XX.
00035          88  VALID-PB-ID                        VALUE 'PB'.
00036
00037      12  PB-CONTROL-PRIMARY.
00038          16  PB-COMPANY-CD                PIC X.
00039          16  PB-ENTRY-BATCH               PIC X(6).
00040          16  PB-BATCH-SEQ-NO              PIC S9(4)     COMP.
00041          16  PB-BATCH-CHG-SEQ-NO          PIC S9(4)     COMP.
00042
00043      12  PB-CONTROL-BY-ACCOUNT.
00044          16  PB-COMPANY-CD-A1             PIC X.
00045          16  PB-CARRIER                   PIC X.
00046          16  PB-GROUPING.
00047              20  PB-GROUPING-PREFIX       PIC XXX.
00048              20  PB-GROUPING-PRIME        PIC XXX.
00049          16  PB-STATE                     PIC XX.
00050          16  PB-ACCOUNT.
00051              20  PB-ACCOUNT-PREFIX        PIC X(4).
00052              20  PB-ACCOUNT-PRIME         PIC X(6).
00053          16  PB-CERT-EFF-DT               PIC XX.
00054          16  PB-CERT-NO.
00055              20  PB-CERT-PRIME            PIC X(10).
00056              20  PB-CERT-SFX              PIC X.
00057          16  PB-ALT-CHG-SEQ-NO            PIC S9(4)     COMP.
00058
00059          16  PB-RECORD-TYPE               PIC X.
00060              88  PB-MAILING-DATA                VALUE '0'.
00061              88  PB-ISSUE                       VALUE '1'.
00062              88  PB-CANCELLATION                VALUE '2'.
00063              88  PB-BATCH-TRAILER               VALUE '9'.
00064
00065      12  PB-CONTROL-BY-ORIG-BATCH.
00066          16  PB-ORIGINAL-COMPANY-CD       PIC X.
00067          16  PB-ORIGINAL-ENTRY-BATCH      PIC X(6).
00068          16  PB-ORIGINAL-SEQ-NO           PIC S9(4)     COMP.
00069          16  PB-ORIGINAL-CHG-SEQ-NO       PIC S9(4)     COMP.
00070
00071      12  PB-CONTROL-BY-CSR.
00072          16  PB-CSR-COMPANY-CD            PIC X.
00073          16  PB-CSR-ID                    PIC X(4).
00074          16  PB-CSR-ENTRY-BATCH           PIC X(6).
00075          16  PB-CSR-BATCH-SEQ-NO          PIC S9(4)     COMP.
00076          16  PB-CSR-BATCH-CHG-SEQ-NO      PIC S9(4)     COMP.
00077 ******************************************************************
00078 *    MAILING DATA IS PROCESSED IN ONLY PRGS. EL512 & EL513       *
00079 ******************************************************************
00080
00081      12  PB-LAST-MAINT-DT                 PIC XX.
00082      12  PB-LAST-MAINT-BY                 PIC X(4).
00083      12  PB-LAST-MAINT-HHMMSS             PIC S9(6)     COMP-3.
00084
00085      12  PB-RECORD-BODY                   PIC X(375).
00086
00087      12  PB-ISSUE-RECORD   REDEFINES PB-RECORD-BODY.
00088          16  PB-CERT-ORIGIN               PIC X.
00089              88  CLASIC-CREATED-CERT         VALUE '1'.
00090          16  PB-I-NAME.
00091              20  PB-I-INSURED-LAST-NAME   PIC X(15).
00092              20  PB-I-INSURED-FIRST-NAME.
00093                  24  PB-I-INSURED-1ST-INIT PIC X.
00094                  24  FILLER                PIC X(9).
00095              20  PB-I-INSURED-MIDDLE-INIT PIC X.
00096          16  PB-I-AGE                     PIC S99   COMP-3.
00097          16  PB-I-JOINT-AGE               PIC S99   COMP-3.
00098          16  PB-I-BIRTHDAY                PIC XX.
00099          16  PB-I-INSURED-SEX             PIC X.
00100              88  PB-SEX-MALE     VALUE 'M'.
00101              88  PB-SEX-FEMALE   VALUE 'F'.
00102
00103          16  PB-I-LF-TERM                 PIC S999   COMP-3.
00104          16  PB-I-AH-TERM                 PIC S999   COMP-3.
00105          16  PB-I-LOAN-TERM               PIC S999   COMP-3.
00106          16  PB-I-PAY-FREQUENCY           PIC S99    COMP-3.
00107          16  PB-I-SKIP-CODE               PIC X.
00108              88  PB-NO-MONTHS-SKIPPED      VALUE ' ' '0'.
00109              88  PB-SKIP-JULY              VALUE '1'.
00110              88  PB-SKIP-AUGUST            VALUE '2'.
00111              88  PB-SKIP-SEPTEMBER         VALUE '3'.
00112              88  PB-SKIP-JULY-AUG          VALUE '4'.
00113              88  PB-SKIP-AUG-SEPT          VALUE '5'.
00114              88  PB-SKIP-JULY-AUG-SEPT     VALUE '6'.
00115              88  PB-SKIP-JUNE-JULY-AUG     VALUE '7'.
00116              88  PB-SKIP-JUNE              VALUE '8'.
00117              88  PB-SKIP-JUNE-JULY         VALUE '9'.
00118              88  PB-SKIP-AUG-SEPT-OCT      VALUE 'A'.
00119              88  PB-SKIP-BI-WKLY-3RD-PMT   VALUE 'X'.
00120          16  PB-I-TERM-TYPE               PIC X.
00121              88  PB-PAID-MONTHLY           VALUE ' ' 'M'.
00122              88  PB-PAID-WEEKLY            VALUE 'W'.
00123              88  PB-PAID-SEMI-MONTHLY      VALUE 'S'.
00124              88  PB-PAID-BI-WEEKLY         VALUE 'B'.
00125              88  PB-PAID-13-YEARLY         VALUE 'T'.
00126          16  PB-I-NO-OF-PAYMENTS          PIC S999   COMP-3.
00127          16  PB-I-POLICY-FORM-NO          PIC X(12).
00128          16  PB-I-DATA-ENTRY-SW           PIC X.
00129              88  PB-EFF-DT-PROCESSING      VALUE '1' ' '.
00130              88  PB-EXT-DAYS-PROCESSING    VALUE '2'.
00131              88  PB-EXPIRE-DT-PROCESSING   VALUE '3'.
00132              88  PB-1ST-PMT-DT-PROCESSING  VALUE '4'.
00133          16  PB-I-PAYMENT-AMOUNT          PIC S9(7)V99  COMP-3.
073107         16  PB-I-DCC-OVER-CHG-AMT        PIC S9(5)V99  COMP-3.
011410*        16  PB-I-MICROFILM-NO            PIC S9(9)      COMP-3.
011410         16  PB-I-AH-CLP                  PIC S9(5)V99 COMP-3.
012220         16  PB-I-LETTER-REQD             PIC X.
00136
00137          16  PB-I-LIFE-BENEFIT-CD         PIC XX.
00138              88  PB-VALID-LIFE               VALUE '01' THRU '89'.
00139              88  PB-INVALID-LIFE             VALUE '  ' '00'
00140                                                    '90' THRU '99'.
00141          16  PB-I-LF-BENEFIT-CD   REDEFINES PB-I-LIFE-BENEFIT-CD
00142                                           PIC XX.
00143          16  PB-I-LF-BENEFIT-AMT          PIC S9(9)V99   COMP-3.
100703         16  PB-I-AMOUNT-FINANCED REDEFINES
100703                  PB-I-LF-BENEFIT-AMT     PIC S9(9)V99   COMP-3.
00144          16  PB-I-LF-ALT-BENEFIT-AMT      PIC S9(9)V99   COMP-3.
100703         16  PB-I-UNPAID-CASH-PRICE REDEFINES
100703                  PB-I-LF-ALT-BENEFIT-AMT PIC S9(9)V99   COMP-3.
00145          16  PB-I-LF-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00146          16  PB-I-LF-ALT-PREMIUM-AMT      PIC S9(7)V99   COMP-3.
100703         16  PB-I-CLP-AMOUNT REDEFINES
100703                  PB-I-LF-ALT-PREMIUM-AMT PIC S9(7)V99   COMP-3.
00147          16  PB-I-LF-CALC-FLAG            PIC X.
00148              88 PB-COMP-LF-PREM               VALUE '?'.
00149          16  PB-I-LF-PREM-CALC            PIC S9(7)V99   COMP-3.
00150          16  PB-I-LF-ALT-PREM-CALC        PIC S9(7)V99   COMP-3.
00151          16  PB-I-LF-RATE                 PIC S99V9(5)   COMP-3.
00152          16  PB-I-LF-ALT-RATE             PIC S99V9(5)   COMP-3.
00153          16  PB-I-LF-POLICY-FEE           PIC S9(3)V99   COMP-3.
00154          16  PB-I-LF-REI-RATE             PIC S99V9(5)   COMP-3.
00155          16  PB-I-LF-ALT-REI-RATE         PIC S99V9(5)   COMP-3.
00156          16  PB-I-LF-ABBR                 PIC XXX.
00157          16  PB-I-LF-INPUT-CD             PIC XX.
00158
00159          16  PB-I-AH-BENEFIT-CD           PIC XX.
00160              88  PB-VALID-AH                 VALUE '01' THRU '89'.
00161              88  PB-INVALID-AH               VALUE '  ' '00'
00162                                                    '90' THRU '99'.
00163          16  PB-I-AH-BENEFIT-AMT          PIC S9(7)V99   COMP-3.
00164          16  PB-I-AH-PREMIUM-AMT          PIC S9(7)V99   COMP-3.
00165          16  PB-I-AH-CALC-FLAG            PIC X.
00166              88 PB-COMP-AH-PREM                  VALUE '?'.
00167          16  PB-I-AH-PREM-CALC            PIC S9(7)V99   COMP-3.
00168          16  PB-I-AH-RATE                 PIC S99V9(5)   COMP-3.
010716         16  PB-I-CANCEL-FEE              PIC S9(3)V99   COMP-3.
00170          16  PB-I-AH-REI-RATE             PIC S99V9(5)   COMP-3.
00171          16  PB-I-AH-RATE-TRM             PIC S999       COMP-3.
00172          16  PB-I-AH-ABBR                 PIC XXX.
00173          16  PB-I-AH-INPUT-CD             PIC XXX.
00174
00175          16  PB-I-SPECIAL-REIN-CODE       PIC X.
00176          16  PB-I-REIN-TABLE              PIC XXX.
00177          16  PB-I-BUSINESS-TYPE           PIC 99.
00178          16  PB-I-INDV-GRP-CD             PIC X.
00179          16  PB-I-MORT-CODE.
00180              20  PB-I-TABLE               PIC X.
00181              20  PB-I-INTEREST            PIC XX.
00182              20  PB-I-MORT-TYP            PIC X.
00183          16  PB-I-LF-CRIT-PER             PIC S9(3)      COMP-3.
00184          16  PB-I-AH-CRIT-PER             PIC S9(3)      COMP-3.
011410         16  PB-I-LF-CLP                  PIC S9(5)V99   COMP-3.
00186          16  PB-I-INDV-GRP-OVRD           PIC X.
00187          16  PB-I-RATE-CLASS-OVRD         PIC XX.
00188          16  PB-I-SIG-SW                  PIC X.
00189              88  PB-POLICY-SIGNED             VALUE 'Y'.
00190          16  PB-I-RATE-CLASS              PIC XX.
00191          16  PB-I-RATE-DEVIATION-LF       PIC XXX.
00192          16  PB-I-RATE-DEVIATION-AH       PIC XXX.
00193          16  PB-I-RATE-DEV-PCT-LF         PIC S9V9(6)    COMP-3.
00194          16  PB-I-RATE-DEV-PCT-AH         PIC S9V9(6)    COMP-3.
00195          16  PB-I-LIFE-COMMISSION         PIC SV9(5)     COMP-3.
00196          16  PB-I-JOINT-COMMISSION        PIC SV9(5)     COMP-3.
00197          16  PB-I-AH-COMMISSION           PIC SV9(5)     COMP-3.
00198          16  PB-I-BENEFIT-TYPE            PIC XXX.
00199          16  PB-I-OB-FLAG                 PIC X.
00200              88  PB-I-OB                      VALUE 'B'.
00201              88  PB-I-SUMMARY                 VALUE 'Z'.
00202          16  PB-I-ENTRY-STATUS            PIC X.
00203              88  PB-I-POLICY-IS-ACTIVE        VALUE '1' '3' '4'
122002                                              'M' '5' '9' '2'.
00205              88  PB-I-NORMAL-ENTRY            VALUE '1'.
00206              88  PB-I-POLICY-PENDING          VALUE '2'.
00207              88  PB-I-CONVERSION-ENTRY        VALUE '4'.
00208              88  PB-I-POLICY-IS-REISSUE       VALUE '5'.
                   88  PB-I-POLICY-IS-CASH          VALUE 'C'.
122002             88  PB-I-POLICY-IS-MONTHLY       VALUE 'M'.
00209              88  PB-I-REIN-ONLY               VALUE '9'.
00210              88  PB-I-POLICY-IS-DECLINED      VALUE 'D'.
00211              88  PB-I-POLICY-IS-VOIDED        VALUE 'V'.
00212              88  PB-I-PREM-ACCTNG-ONLY        VALUE 'P'.
00213              88  PB-I-UNDERWRITE-POLICY       VALUE 'U'.
00214          16  PB-I-INT-CODE                PIC X.
00215              88  PB-ADD-ON-INTEREST           VALUE 'A'.
00216              88  PB-SIMPLE-INTEREST           VALUE 'S'.
00217          16  PB-I-LOAN-APR                PIC 9(3)V9(4)   COMP-3.
00218          16  PB-I-SOC-SEC-NO              PIC X(11).
00219          16  PB-I-MEMBER-NO               PIC X(12).
00220          16  PB-I-CURR-SEQ                PIC S9(4)       COMP.
110105*        16  PB-I-LOAN-OFFICER            PIC XXX.
110105         16  PB-I-OLD-LOF                 PIC XXX.
00222          16  PB-I-LF-EXPIRE-DT            PIC XX.
00223          16  PB-I-AH-EXPIRE-DT            PIC XX.
00224          16  PB-I-EXTENTION-DAYS          PIC S999        COMP-3.
00225          16  PB-I-TERM-IN-DAYS            PIC S9(5)       COMP-3.
00226          16  PB-I-LIFE-INDICATOR          PIC X.
00227              88  PB-I-JOINT-COVERAGE         VALUE 'J'.
00228          16  PB-I-LIVES                   PIC S9(7)       COMP-3.
071211         16  PB-I-DDF-IU-RATE-UP REDEFINES PB-I-LIVES
071211                                          PIC S9(5)V99    COMP-3.
00229          16  PB-I-MAIL-ADDRS-SW           PIC X.
00230              88 PB-I-MAIL-ADDRS-NOT-PRESENT  VALUE ' '.
00231              88 PB-I-MAIL-ADDRS-PRESENT      VALUE '1'.
00232          16  PB-I-1ST-PMT-DT              PIC XX.
00233          16  PB-I-JOINT-INSURED.
00234              20 PB-I-JOINT-LAST-NAME      PIC X(15).
00235              20 PB-I-JOINT-FIRST-NAME.
00236                 24  PB-I-JOINT-FIRST-INIT PIC X.
00237                 24  FILLER                PIC X(9).
00238              20 PB-I-JOINT-MIDDLE-INIT    PIC X.
100703*        16  PB-I-BENEFICIARY-NAME        PIC X(25).
100703         16  PB-I-BENEFICIARY-NAME.
100703             20  PB-I-BANK-NUMBER         PIC X(10).
100703             20  FILLER                   PIC X(15).
00240          16  PB-I-LAST-ADD-ON-DT          PIC XX.
011904         16  PB-I-REFERENCE               PIC X(12).
011904         16  FILLER REDEFINES PB-I-REFERENCE.
011904             20  PB-I-TOT-FEES            PIC S9(7)V99 COMP-3.
011904             20  PB-I-TOT-FEES-CALC       PIC S9(7)V99 COMP-3.
020305             20  PB-I-CLP-STATE           PIC XX.
00242          16  PB-I-UNDERWRITING-STATUS     PIC X.
00243              88  PB-I-POLICY-ACCEPTED         VALUE 'A' 'N'.
00244              88  PB-I-POLICY-DECLINED         VALUE 'D'.
00245              88  PB-I-NEEDS-UNDERWRITING      VALUE 'U'.
00246          16  PB-I-STATE-TAX               PIC S9(7)V99 COMP-3.
00247          16  PB-I-MUNI-TAX                PIC S9(7)V99 COMP-3.
00248          16  PB-I-RESIDENT-STATE          PIC XX.
00249          16  PB-I-RATE-CODE               PIC X(4).
00250          16  PB-I-NUM-BILLED              PIC S9(7)    COMP-3.
PEMMOD         16  PB-I-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-I-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
100703         16  PB-I-BANK-FEE                PIC S999V99  COMP-3.
100703         16  PB-I-BANK-NOCHRGB            PIC 99.
040504         16  PB-I-ADDL-CLP                PIC S9(5)V99 COMP-3.
081108         16  PB-I-JOINT-BIRTHDAY          PIC XX.
00252
00253      12  PB-CANCEL-RECORD   REDEFINES PB-RECORD-BODY.
00254          16  PB-C-LF-CANCEL-VOID-SW       PIC X.
00255              88  PB-C-LF-CANCEL-VOIDED        VALUE '1'.
00256          16  PB-C-CANCEL-ORIGIN           PIC X.
00257              88  PB-C-CLAIM-CREATED-CANCEL   VALUE '1'.
00258          16  PB-C-LF-CANCEL-DT            PIC XX.
00259          16  PB-C-LF-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00260          16  PB-C-LF-CALC-REQ             PIC X.
00261              88 PB-COMP-LF-CANCEL            VALUE '?'.
00262          16  PB-C-LF-REF-CALC             PIC S9(7)V99    COMP-3.
00263          16  PB-C-LF-REM-TERM             PIC S9(3)       COMP-3.
00264          16  PB-C-AH-CANCEL-VOID-SW       PIC X.
00265              88  PB-C-AH-CANCEL-VOIDED        VALUE '1'.
00266          16  PB-C-AH-CANCEL-DT            PIC XX.
00267          16  PB-C-AH-CANCEL-AMT           PIC S9(7)V99    COMP-3.
00268          16  PB-C-AH-CALC-REQ             PIC X.
00269              88 PB-COMP-AH-CANCEL            VALUE '?'.
00270          16  PB-C-AH-REF-CALC             PIC S9(7)V99    COMP-3.
00271          16  PB-C-AH-REM-TERM             PIC S9(3)       COMP-3.
00272          16  PB-C-LAST-NAME               PIC X(15).
00273          16  PB-C-REFUND-SW               PIC X.
00274              88  PB-C-REFUND-CREATED          VALUE 'Y'.
00275              88  PB-C-REFUND-REQUESTED        VALUE 'R'.
00276          16  PB-C-LIVES                   PIC S9(3)       COMP-3.
00277          16  PB-C-PAYEE-CODE              PIC X(6).
00278          16  PB-C-LF-REFUND-OVERRIDE      PIC X.
00279          16  PB-C-AH-REFUND-OVERRIDE      PIC X.
00280          16  PB-C-LF-COMM-CHARGEBACK      PIC X.
00281          16  PB-C-AH-COMM-CHARGEBACK      PIC X.
00282          16  PB-C-REFERENCE               PIC X(12).
PEMMOD         16  PB-C-LF-PREM-TAX             PIC S9V9(4)  COMP-3.
PEMMOD         16  PB-C-AH-PREM-TAX             PIC S9V9(4)  COMP-3.
081606         16  PB-C-POST-CARD-IND           PIC X.
081606         16  PB-C-CANCEL-REASON           PIC X.
072308         16  PB-C-REF-INTERFACE-SW        PIC X.
071211         16  PB-C-LF-RFND-CLP             PIC S9(5)V99 COMP-3.
071211         16  PB-C-AH-RFND-CLP             PIC S9(5)V99 COMP-3.
00283          16  FILLER                       PIC X(01).
PEMMOD*        16  FILLER                       PIC X(18).
00284          16  PB-C-POLICY-FORM-NO          PIC X(12).
072308*        16  PB-C-MICROFILM-NO            PIC S9(9)      COMP-3.
062017         16  PB-C-INT-ON-REFS             PIC S9(7)V99   COMP-3.
00286          16  PB-CANCELED-CERT-DATA.
00287              20  PB-CI-INSURED-NAME.
00288                  24  PB-CI-LAST-NAME      PIC X(15).
00289                  24  PB-CI-INITIALS       PIC XX.
00290              20  PB-CI-INSURED-AGE        PIC S99         COMP-3.
00291              20  PB-CI-INSURED-SEX        PIC X.
00292              20  PB-CI-LF-TERM            PIC S999        COMP-3.
00293              20  PB-CI-LF-BENEFIT-CD      PIC XX.
00294              20  PB-CI-LF-BENEFIT-AMT     PIC S9(9)V99    COMP-3.
00295              20  PB-CI-LF-ALT-BENEFIT-AMT PIC S9(9)V99    COMP-3.
00296              20  PB-CI-LF-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00297              20  PB-CI-LF-ALT-PREMIUM-AMT PIC S9(7)V99    COMP-3.
00298              20  PB-CI-AH-TERM            PIC S999        COMP-3.
00299              20  PB-CI-AH-BENEFIT-CD      PIC XX.
00300              20  PB-CI-AH-BENEFIT-AMT     PIC S9(7)V99    COMP-3.
00301              20  PB-CI-AH-PREMIUM-AMT     PIC S9(7)V99    COMP-3.
00302              20  PB-CI-RATE-CLASS         PIC XX.
00303              20  PB-CI-RATE-DEV-LF        PIC XXX.
00304              20  PB-CI-RATE-DEV-AH        PIC XXX.
00305              20  PB-CI-RATE-DEV-PCT-LF    PIC S9V9(6)     COMP-3.
00306              20  PB-CI-RATE-DEV-PCT-AH    PIC S9V9(6)     COMP-3.
00307              20  PB-CI-LIFE-COMMISSION    PIC SV9(5)      COMP-3.
00308              20  PB-CI-AH-COMMISSION      PIC SV9(5)      COMP-3.
00309              20  PB-CI-LF-ABBR            PIC X(3).
00310              20  PB-CI-AH-ABBR            PIC X(3).
00311              20  PB-CI-OB-FLAG            PIC X.
00312                  88  PB-CI-OB                VALUE 'B'.
00313              20  PB-CI-LF-POLICY-STATUS   PIC X.
00314                  88  PB-CI-LF-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00316                  88  PB-CI-LF-NORMAL-ENTRY           VALUE '1'.
00317                  88  PB-CI-LF-POLICY-PENDING         VALUE '2'.
00318                  88  PB-CI-LF-POLICY-IS-RESTORE      VALUE '3'.
00319                  88  PB-CI-LF-CONVERSION-ENTRY       VALUE '4'.
00320                  88  PB-CI-LF-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-LF-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-LF-POLICY-IS-MONTHLY      VALUE 'M'.
00321                  88  PB-CI-LF-LUMP-SUM-DISAB         VALUE '6'.
00322                  88  PB-CI-LF-DEATH-CLAIM-APPLIED    VALUE '7'.
00323                  88  PB-CI-LF-CANCEL-APPLIED         VALUE '8'.
00324                  88  PB-CI-LF-REIN-ONLY              VALUE '9'.
00325                  88  PB-CI-LF-POLICY-IS-DECLINED     VALUE 'D'.
00326                  88  PB-CI-LF-POLICY-IS-VOID         VALUE 'V'.
00327              20  PB-CI-AH-POLICY-STATUS   PIC X.
00328                  88  PB-CI-AH-POLICY-IS-ACTIVE       VALUE '1' '3'
122002                                           'M' '4' '5' '9' '2'.
00330                  88  PB-CI-AH-NORMAL-ENTRY           VALUE '1'.
00331                  88  PB-CI-AH-POLICY-PENDING         VALUE '2'.
00332                  88  PB-CI-AH-POLICY-IS-RESTORE      VALUE '3'.
00333                  88  PB-CI-AH-CONVERSION-ENTRY       VALUE '4'.
00334                  88  PB-CI-AH-POLICY-IS-REISSUE      VALUE '5'.
                       88  PB-CI-AH-POLICY-IS-CASH         VALUE 'C'.
122002                 88  PB-CI-AH-POLICY-IS-MONTHLY      VALUE 'M'.
00335                  88  PB-CI-AH-LUMP-SUM-DISAB         VALUE '6'.
00336                  88  PB-CI-AH-DEATH-CLAIM-APPLIED    VALUE '7'.
00337                  88  PB-CI-AH-CANCEL-APPLIED         VALUE '8'.
00338                  88  PB-CI-AH-REIN-ONLY              VALUE '9'.
00339                  88  PB-CI-AH-POLICY-IS-DECLINED     VALUE 'D'.
00340                  88  PB-CI-AH-POLICY-IS-VOID         VALUE 'V'.
00341              20  PB-CI-PAY-FREQUENCY      PIC 99.
00342              20  PB-CI-LOAN-APR           PIC 9(3)V9(4)   COMP-3.
00343              20  PB-CI-SOC-SEC-NO         PIC X(11).
00344              20  PB-CI-MEMBER-NO          PIC X(12).
00345              20  PB-CI-INT-CODE           PIC X.
00346                  88  PB-CI-ADD-ON                  VALUE 'A'.
00347                  88  PB-CI-SIMPLE                  VALUE 'S'.
00348              20  PB-CI-LOAN-TERM          PIC S999        COMP-3.
00349              20  PB-CI-LOAN-1ST-PMT-DT    PIC X(2).
00350              20  PB-CI-COMP-EXCP-SW       PIC X.
00351                  88  PB-CI-NO-COMP-EXCP            VALUE ' '.
00352                  88  PB-CI-CERT-HAS-ERCOMM-ENTRY   VALUE '1'.
00353              20  PB-CI-ENTRY-STATUS       PIC X.
00354              20  PB-CI-CURR-SEQ           PIC S9(4)       COMP.
00355              20  PB-CI-AH-PAID-THRU-DT    PIC XX.
00356              20  PB-CI-AH-SETTLEMENT-DT   PIC XX.
00357              20  PB-CI-DEATH-DT           PIC XX.
00358              20  PB-CI-LF-PRIOR-CANCEL-DT PIC XX.
00359              20  PB-CI-AH-PRIOR-CANCEL-DT PIC XX.
00360              20  PB-CI-AH-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00361              20  PB-CI-LF-CANCEL-AMT      PIC S9(7)V99    COMP-3.
00362              20  PB-CI-CREDIT-INTERFACE-SW-1 PIC X.
00363              20  PB-CI-CREDIT-INTERFACE-SW-2 PIC X.
00364              20  PB-CI-ENTRY-DT              PIC XX.
00365              20  PB-CI-ENTRY-BATCH           PIC X(6).
00366              20  PB-CI-LF-EXPIRE-DT          PIC XX.
00367              20  PB-CI-AH-EXPIRE-DT          PIC XX.
00368              20  PB-CI-EXTENTION-DAYS        PIC S999     COMP-3.
00369              20  PB-CI-TERM-IN-DAYS          PIC S9(5)    COMP-3.
110105             20  PB-CI-OLD-LOF               PIC XXX.
110105*            20  PB-CI-LOAN-OFFICER          PIC XXX.
00371              20  PB-CI-LIVES                 PIC S9(3)    COMP-3.
00372              20  PB-CI-LF-CRIT-PER           PIC S9(3)    COMP-3.
00373              20  PB-CI-AH-CRIT-PER           PIC S9(3)    COMP-3.
00374              20  PB-CI-INDV-GRP-CD           PIC X.
100703             20  PB-CI-BENEFICIARY-NAME.
100703                 24  PB-CI-BANK-NUMBER       PIC X(10).
100703                 24  FILLER                  PIC X(15).
00376              20  PB-CI-NOTE-SW               PIC X.
00377              20  PB-CI-CANCEL-FEE            PIC S9(3)V99 COMP-3.
00378              20  PB-CI-MUNI-TAX              PIC S9(7)V99 COMP-3.
00379              20  PB-CI-STATE-TAX             PIC S9(7)V99 COMP-3.
040504             20  PB-CI-ADDL-CLP              PIC S9(5)V99 COMP-3.
110105             20  PB-CI-LOAN-OFFICER          PIC X(5).
032306             20  PB-CI-BOW-LOAN-NUMBER       PIC X(14).
072209             20  PB-CI-FIRST-NAME            PIC X(10).
071211             20  PB-CI-DDF-IU-RATE-UP        PIC S9(5)V99 COMP-3.
00380
072209         16  FILLER                       PIC X(13).
072209*032306  16  FILLER                       PIC X(27).
040504*        16  FILLER                       PIC X(46).
00382
00383      12  PB-MAIL-RECORD    REDEFINES PB-RECORD-BODY.
00384          16  FILLER                       PIC X(10).
00385          16  PB-M-INSURED-LAST-NAME       PIC X(15).
00386          16  PB-M-INSURED-FIRST-NAME      PIC X(10).
00387          16  PB-M-INSURED-MID-INIT        PIC X.
00388          16  PB-M-INSURED-AGE             PIC 99.
00389          16  PB-M-INSURED-BIRTHDAY        PIC XX.
00390          16  PB-M-INSURED-SEX             PIC X.
00391          16  PB-M-INSURED-SOC-SEC-NO      PIC X(11).
00392          16  PB-M-INSURED-ADDRESS-1       PIC X(30).
00393          16  PB-M-INSURED-ADDRESS-2       PIC X(30).
00394          16  PB-M-INSURED-CITY-STATE.
051810             20  PB-M-INSURED-CITY        PIC X(28).
051810             20  PB-M-INSURED-STATE       PIC XX.
00395          16  PB-M-INSURED-ZIP-CODE.
00396              20  PB-M-INSURED-ZIP-PRIME.
00397                  24  PB-M-INSURED-ZIP-1   PIC X.
00398                      88  PB-M-CANADIAN-POST-CODE
00399                                              VALUE 'A' THRU 'Z'.
00400                  24  FILLER               PIC X(4).
00401              20  PB-M-INSURED-ZIP-PLUS4   PIC X(4).
00402          16  PB-M-INSURED-CANADIAN-ZIP  REDEFINES
00403                                         PB-M-INSURED-ZIP-CODE.
00404              20  PM-M-INS-CAN-POST1       PIC XXX.
00405              20  PM-M-INS-CAN-POST2       PIC XXX.
00406              20  FILLER                   PIC XXX.
00407          16  PB-M-INSURED-PHONE-NO        PIC 9(10).
081108         16  PB-M-JOINT-BIRTHDAY          PIC XX.
               16  PB-M-CRED-BENE-NAME          PIC X(30).
               16  PB-M-CRED-BENE-ADDR1         PIC X(30).
               16  PB-M-CRED-BENE-ADDR2         PIC X(30).
               16  PB-M-CRED-BENE-CITYST.
                   20  PB-M-CRED-BENE-CITY      PIC X(28).
                   20  PB-M-CRED-BENE-STATE     PIC XX.
081108         16  FILLER                       PIC X(92).
00409
00410      12  PB-BATCH-RECORD   REDEFINES PB-RECORD-BODY.
00411          16  FILLER                       PIC X(10).
00412          16  PB-B-LF-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00413          16  PB-B-LF-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00414          16  PB-B-LF-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00415          16  PB-B-LF-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00416          16  PB-B-LF-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00417          16  PB-B-LF-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00418          16  PB-B-AH-ISS-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00419          16  PB-B-AH-ISS-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00420          16  PB-B-AH-ISS-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00421          16  PB-B-AH-CAN-PRM-REMITTED     PIC S9(9)V99 COMP-3.
00422          16  PB-B-AH-CAN-PRM-ENTERED      PIC S9(9)V99 COMP-3.
00423          16  PB-B-AH-CAN-PRM-COMPUTED     PIC S9(9)V99 COMP-3.
00424          16  PB-B-ISSUE-CNT-REMITTED      PIC S9(5)    COMP-3.
00425          16  PB-B-ISSUE-CNT-ENTERED       PIC S9(5)    COMP-3.
00426          16  PB-B-CANCEL-CNT-REMITTED     PIC S9(5)    COMP-3.
00427          16  PB-B-CANCEL-CNT-ENTERED      PIC S9(5)    COMP-3.
00428          16  PB-B-HIGHEST-SEQ-NO          PIC S9(4)    COMP.
00429          16  PB-ACCOUNT-NAME              PIC X(30).
00430          16  PB-PREM-REF-RPT-FLAG         PIC X.
00431          16  PB-REFERENCE                 PIC X(12).
00432          16  PB-B-RECEIVED-DT             PIC XX.
00433          16  FILLER                       PIC X(234).
00434
00435      12  PB-RECORD-STATUS.
00436          16  PB-CREDIT-SELECT-DT          PIC XX.
00437          16  PB-CREDIT-ACCEPT-DT          PIC XX.
00438          16  PB-BILLED-DT                 PIC XX.
00439          16  PB-BILLING-STATUS            PIC X.
00440              88  PB-ENTRY-REVERSED            VALUE 'R'.
00441              88  PB-EL860-INTERNAL-ERROR      VALUE 'E'.
00442              88  PB-EL860-INTERNAL-PROCESS    VALUE 'P'.
00443          16  PB-RECORD-BILL               PIC X.
00444              88  PB-RECORD-ON-HOLD            VALUE 'H'.
00445              88  PB-RECORD-RETURNED           VALUE 'R'.
00446              88  PB-RECORD-ENDORSED           VALUE 'E'.
00447              88  PB-OVERRIDE-LIFE             VALUE 'L'.
00448              88  PB-OVERRIDE-AH               VALUE 'A'.
00449              88  PB-OVERRIDE-BOTH             VALUE 'B'.
00450          16  PB-BATCH-ENTRY               PIC X.
00451              88  PB-POLICY-IS-DECLINED        VALUE 'D'.
00452              88  PB-REIN-ONLY-CERT            VALUE 'R'.
00453              88  PB-REISSUED-CERT             VALUE 'E'.
                   88  PB-CASH-CERT                 VALUE 'C'.
122002             88  PB-MONTHLY-CERT              VALUE 'M'.
00454              88  PB-PREM-ACCTNG-ONLY          VALUE 'P'.
00455              88  PB-NEEDS-UNDERWRITING        VALUE 'U'.
00456              88  PB-POLICY-IS-VOIDED          VALUE 'V'.
00457          16  PB-FORCE-CODE                PIC X.
00458              88  PB-FORCE-OFF                 VALUE ' ' '0'.
00459              88  PB-ISSUE-FORCE               VALUE 'A' 'O'.
00460              88  PB-CANCEL-FORCE              VALUE '8'.
00461              88  PB-ALL-ISSUE-FORCED          VALUE 'A' 'O'.
00462              88  PB-ALL-CANCEL-FORCED         VALUE '8'.
00463              88  PB-ALL-CANCEL-FORCED-NO-FEE  VALUE '9'.
00464              88  PB-CANCEL-DATE-FORCED        VALUE 'D'.
00465              88  PB-CANCEL-DATE-FORCED-NO-FEE VALUE 'E'.
00466              88  PB-ISSUE-DATE-FORCED         VALUE 'D'.
010517             88  PB-EXCEEDED-LIMIT-FORCED     VALUE 'L'.
073107             88  PB-OVERCHARGE-FORCE          VALUE 'O'.
00467          16  PB-FATAL-FLAG                PIC X.
00468              88  PB-FATAL-ERRORS              VALUE 'X'.
00469          16  PB-FORCE-ER-CD               PIC X.
00470              88  PB-FORCE-ERRORS              VALUE 'F'.
00471              88  PB-UNFORCED-ERRORS           VALUE 'X'.
00472          16  PB-WARN-ER-CD                PIC X.
00473              88  PB-WARNING-ERRORS            VALUE 'W'.
00474          16  FILLER                       PIC X.
00475          16  PB-OUT-BAL-CD                PIC X.
00476              88  PB-OUT-OF-BAL                VALUE 'O'.
00477          16  PB-LIFE-OVERRIDE-L1          PIC X.
00478          16  PB-AH-OVERRIDE-L1            PIC X.
00479          16  PB-INPUT-DT                  PIC XX.
00480          16  PB-INPUT-BY                  PIC X(4).
00481          16  PB-CHG-COUNT                 PIC 9(3)        COMP-3.
00482          16  PB-CALC-TOLERANCE            PIC 9(3)V99     COMP-3.
00483          16  PB-TOLERANCE-REJECT-SW       PIC X.
00484          16  PB-LF-EARNING-METHOD         PIC X.
00485          16  PB-AH-EARNING-METHOD         PIC X.
00486          16  PB-LF-TERM-CALC-METHOD       PIC X.
00487          16  PB-AH-TERM-CALC-METHOD       PIC X.
00488          16  PB-REIN-CD                   PIC XXX.
00489          16  PB-LF-REFUND-TYPE            PIC X.
00490          16  PB-AH-REFUND-TYPE            PIC X.
00491          16  PB-ACCT-EFF-DT               PIC XX.
00492          16  PB-ACCT-EXP-DT               PIC XX.
00493          16  PB-COMPANY-ID                PIC X(3).
00494          16  PB-LF-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00495          16  PB-AH-BILLED-AMTS            PIC S9(7)V99  COMP-3.
00496          16  PB-SV-CARRIER                PIC X.
00497          16  PB-SV-GROUPING               PIC X(6).
00498          16  PB-SV-STATE                  PIC XX.
00499          16  PB-CONFIRMATION-REPT-DT      PIC XX.
00500          16  PB-GA-BILLING-INFO.
00501              20  PB-GA-BILL-DT OCCURS 5 TIMES
00502                                           PIC XX.
00503          16  PB-SV-REMIT-TO  REDEFINES
00504              PB-GA-BILLING-INFO           PIC X(10).
00505          16  PB-NO-OF-ERRORS              PIC S9(3) COMP-3.
110105         16  PB-I-LOAN-OFFICER            PIC X(5).
081606         16  PB-I-VIN                     PIC X(17).
00506
110105         16  FILLER                       PIC X(04).
110105         16  IMNET-BYPASS-SW              PIC X.
00508
00509 ******************************************************************
00510 *                COMMON EDIT ERRORS                              *
00511 ******************************************************************
00512
00513      12  PB-COMMON-ERRORS.
00514          16  PB-COMMON-ERROR    OCCURS 10 TIMES
00515                                            PIC S9(4)     COMP.
00516
00517 ******************************************************************
02037                                  EJECT
02038 *    COPY ERCPYAJ.
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
02039                                  EJECT
02040 *    COPY ELCTEXT.
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
02041                                  EJECT
101812*    COPY ERCENDT.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ERCENDT.                            *
      *                            VMOD=2.001                          *
      *                                                                *
      *   FILE DESCRIPTION = ENDORSEMENT FILE                          *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
PEMMOD*   RECORD SIZE = 579  RECFORM = FIXED                           *
      *                                                                *
      *   BASE CLUSTER = ERENDT                         RKP=02,LEN=36  *
      *       ALTERNATE PATH1 = ERENDT2 (BY ARCH NO)    RKP=38,LEN=05  *
      *                                                                *
      *   LOG = NO                                                     *
      *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
      ******************************************************************
052307*-----------------------------------------------------------------
052307*                   C H A N G E   L O G
052307*
052307* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052307*-----------------------------------------------------------------
052307*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052307* EFFECTIVE    NUMBER
052307*-----------------------------------------------------------------
052307* 052307    2006052600001  AJRA  ADDED FLAG FOR CANCELS ON CERTS
052307*                                WITH OPEN CLAIMS
072312* 072312    2011022800001  AJRA  ADDED BATCH NUMBER
110612* 110612    2012101700002  AJRA  ADD NEW FIELDS
121812* 121812  CR2012101700002  AJRA  ADD DEFAULT AGE FLAG
010616* 010616  CR2015082000001  PEMA  ADD ENDORSEMENT CHECK PROCESSING
052307*-----------------------------------------------------------------
       01  ENDORSEMENT-RECORD.
           12  EN-RECORD-ID                PIC XX.
               88  VALID-EN-ID                VALUE 'EN'.
           12  EN-CONTROL-PRIMARY.
               16  EN-COMPANY-CD           PIC X.
               16  EN-CARRIER              PIC X.
               16  EN-GROUPING             PIC X(6).
               16  EN-STATE                PIC XX.
               16  EN-ACCOUNT              PIC X(10).
               16  EN-CERT-EFF-DT          PIC XX.
               16  EN-CERT-NO.
                   20  EN-CERT-PRIME       PIC X(10).
                   20  EN-CERT-SFX         PIC X.
               16  EN-REC-TYPE             PIC X.
                   88  EN-ISSUE               VALUE 'I'.
                   88  EN-CANCELLATION        VALUE 'C'.
               16  EN-SEQ-NO               PIC 9(04) BINARY.
           12  EN-CONTROL-BY-ARCH-NO.
               16  EN-COMPANY-CD-A1              PIC X.
               16  EN-ARCHIVE-NO                 PIC 9(8) BINARY.
           12  EN-LAST-MAINT-DT            PIC XX.
           12  EN-LAST-MAINT-BY            PIC X(4).
           12  EN-LAST-MAINT-HHMMSS        PIC S9(6)     COMP-3.
           12  EN-ENDORSEMENT-RECORD       PIC X(329).
           12  EN-ISSUE-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
               16  EN-INS-ORIG-LAST-NAME   PIC X(15).
               16  EN-INS-ORIG-FIRST-NAME  PIC X(10).
               16  EN-INS-ORIG-MIDDLE-INIT PIC X.
               16  EN-INS-ORIG-AGE         PIC S999     COMP-3.
               16  EN-JNT-ORIG-LAST-NAME   PIC X(15).
               16  EN-JNT-ORIG-FIRST-NAME  PIC X(10).
               16  EN-JNT-ORIG-MIDDLE-INIT PIC X.
               16  EN-JNT-ORIG-AGE         PIC S999     COMP-3.
               16  EN-LF-ORIG-BENCD        PIC XX.
               16  EN-LF-ORIG-TERM         PIC S999      COMP-3.
               16  EN-LF-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
               16  EN-LF-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
               16  EN-LF-ORIG-ALT-BEN-AMT  PIC S9(9)V99  COMP-3.
               16  EN-LF-ORIG-ALT-PRM-AMT  PIC S9(7)V99  COMP-3.
               16  EN-LF-ORIG-EXP-DT       PIC XX.
      *        16  EN-LF-ORIG-COV-TYPE     PIC X(10).
               16  EN-ORIG-CRED-BENE       PIC X(25).
               16  EN-LF-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
               16  FILLER                  PIC X.
               16  EN-AH-ORIG-BENCD        PIC XX.
               16  EN-AH-ORIG-TERM         PIC S999      COMP-3.
               16  EN-AH-ORIG-BEN-AMT      PIC S9(9)V99  COMP-3.
               16  EN-AH-ORIG-PRM-AMT      PIC S9(7)V99  COMP-3.
               16  EN-AH-ORIG-EXP-DT       PIC XX.
      *        16  EN-AH-ORIG-COV-TYPE     PIC X(10).
      *        16  EN-AH-ORIG-WAIT-PER     PIC 99.
               16  EN-AH-ORIG-COMM-PCT     PIC SV9(5)    COMP-3.
               16  F                       PIC X(09).
               16  EN-AH-ORIG-CP           PIC 99.
               16  EN-INS-NEW-LAST-NAME    PIC X(15).
               16  EN-INS-NEW-FIRST-NAME   PIC X(10).
               16  EN-INS-NEW-MIDDLE-INIT  PIC X.
               16  EN-INS-NEW-AGE          PIC S999     COMP-3.
               16  EN-JNT-NEW-LAST-NAME    PIC X(15).
               16  EN-JNT-NEW-FIRST-NAME   PIC X(10).
               16  EN-JNT-NEW-MIDDLE-INIT  PIC X.
               16  EN-JNT-NEW-AGE          PIC S999     COMP-3.
               16  EN-LF-NEW-BENCD         PIC XX.
               16  EN-LF-NEW-TERM          PIC S999      COMP-3.
               16  EN-LF-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
               16  EN-LF-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
               16  EN-LF-NEW-ALT-BEN-AMT   PIC S9(9)V99  COMP-3.
               16  EN-LF-NEW-ALT-PRM-AMT   PIC S9(7)V99  COMP-3.
               16  EN-LF-NEW-EXP-DT        PIC XX.
               16  EN-NEW-CRED-BENE        PIC X(25).
               16  EN-LF-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
               16  FILLER                  PIC X.
               16  EN-AH-NEW-BENCD         PIC XX.
               16  EN-AH-NEW-TERM          PIC S999      COMP-3.
               16  EN-AH-NEW-BEN-AMT       PIC S9(9)V99  COMP-3.
               16  EN-AH-NEW-PRM-AMT       PIC S9(7)V99  COMP-3.
               16  EN-AH-NEW-EXP-DT        PIC XX.
      *        16  EN-AH-NEW-COV-TYPE      PIC X(10).
      *        16  EN-AH-NEW-WAIT-PER      PIC 99.
      *        16  F                       PIC X(12).
               16  EN-AH-NEW-CP            PIC 99.
               16  EN-SIG-SW               PIC X.
               16  EN-AH-NEW-COMM-PCT      PIC SV9(5)    COMP-3.
121812         16  EN-INS-ORIG-AGE-DEF-FLAG PIC X.
121812         16  EN-JNT-ORIG-AGE-DEF-FLAG PIC X.
121812         16  EN-INS-NEW-AGE-DEF-FLAG PIC X.
121812         16  EN-JNT-NEW-AGE-DEF-FLAG PIC X.
121812         16  FILLER                  PIC X(33).
           12  EN-CANCEL-RECORD REDEFINES EN-ENDORSEMENT-RECORD.
               16  EN-LF-ORIG-REF-DT       PIC XX.
               16  EN-LF-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
               16  EN-AH-ORIG-REF-DT       PIC XX.
               16  EN-AH-ORIG-REF-AMT      PIC S9(7)V99  COMP-3.
               16  EN-LF-NEW-REF-DT        PIC XX.
               16  EN-LF-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
               16  EN-AH-NEW-REF-DT        PIC XX.
               16  EN-AH-NEW-REF-AMT       PIC S9(7)V99  COMP-3.
               16  EN-FLAG-CERT            PIC X.
               16  EN-INS-LAST-NAME        PIC X(15).
               16  EN-INS-FIRST-NAME       PIC X(10).
               16  EN-INS-MIDDLE-INIT      PIC X.
110612         16  EN-LF-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
110612         16  EN-AH-ORIG-REF-COMM-PCT PIC SV9(5)    COMP-3.
110612         16  EN-LF-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
110612         16  EN-AH-NEW-REF-COMM-PCT  PIC SV9(5)    COMP-3.
110612         16  FILLER                  PIC X(262).
           12  EN-MONEY-SW             PIC X.
           12  EN-HEALTH-APP           PIC X.
           12  EN-VOUCHER-SW           PIC X.
           12  EN-PAYEE                PIC X(14).
           12  EN-INPUT-DT             PIC XX.
           12  EN-PROCESS-DT           PIC XX.
           12  EN-LF-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-AH-COMMISSION        PIC SV9(5)    COMP-3.
           12  EN-REASON-CODES.
               16  F OCCURS 12.
                   20  EN-REASON-CODE  PIC X(4).
           12  EN-TEMPLATE-USED        PIC X(8).
           12  EN-DOCU-TYPE            PIC X.
               88  EN-VERI-DOCU          VALUE 'V'.
               88  EN-GCE-DOCU           VALUE 'G'.
               88  EN-CANC-DOCU          VALUE 'C'.
           12  EN-COMMENTS1            PIC X(13).
           12  EN-COMMENTS2            PIC X(70).
           12  EN-COMM-CHGBK           PIC X.
               88  EN-DO-NOT-CHG-ACCT    VALUE 'N'.
               88  EN-CHG-ACCT           VALUE 'Y'.
           12  EN-CSO-PORTION          PIC S9(5)V99  COMP-3.
           12  EN-ACCT-PORTION         PIC S9(5)V99  COMP-3.
072312     12  EN-BATCH-NUMBER         PIC X(6).
072312     12  EN-ACCT-SUMM            PIC X.
072312     12  EN-CSO-SUMM             PIC X.
010616     12  en-check-type           pic x.
072312     12  FILLER                  PIC X(12).
      ******************************************************************
101812                                 EJECT
012413*    COPY ERCNOTE.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ERCNOTE                             *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.003                          *
00006 *                                                                *
00007 *        FILE DESCRIPTION = CERTIFICATE AND BILLING NOTES        *
00008 *                                                                *
00009 *        FILE TYPE= VSAM,KSDS                                    *
00010 *        RECORD SIZE = 825    RECFORM = FIXED                    *
00011 *                                                                *
00012 *        BASE CLUSTER = ERNOTE        RKP=2,LEN=34               *
00013 *                                                                *
00014 *        LOG = YES                                               *
00015 *        SERVREQ = DELETE,UPDATE,NEWREC                          *
00016 *                                                                *
091509******************************************************************
091509*                   C H A N G E   L O G
091509*
091509* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
091509*-----------------------------------------------------------------
091509*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
091509* EFFECTIVE    NUMBER
091509*-----------------------------------------------------------------
091509* 091509  CR2008100900003  AJRA  CERT NOTES MOVED TO NEW FILE. THI
091509*                                FILE WILL CONTAIN BILLING NOTES O
041320* 041320  CR2020030500002  PEMA  Distinguish between iss and canc
00017 ******************************************************************
00018
00019  01  CERTIFICATE-NOTE.
00020      12  CN-RECORD-ID                PIC  XX.
00021          88  VALID-CN-ID                  VALUE 'CN'.
00022
00023      12  CN-CONTROL-PRIMARY.
00024          16  CN-COMPANY-CD           PIC X.
00025          16  CN-CARRIER              PIC X.
00026          16  CN-GROUPING.
00027              20 CN-GROUPING-PREFIX   PIC XXX.
00028              20 CN-GROUPING-PRIME    PIC XXX.
00029          16  CN-STATE                PIC XX.
00030          16  CN-ACCOUNT.
00031              20 CN-ACCOUNT-PREFIX    PIC X(4).
00032              20 CN-ACCOUNT-PRIME     PIC X(6).
00033          16  CN-CERT-EFF-DT          PIC XX.
00034          16  CN-CERT-NO.
00035              20  CN-CERT-PRIME       PIC X(10).
00036              20  CN-CERT-SFX         PIC X.
041320         16  CN-RECORD-TYPE          PIC X.
041320             88  CN-ISSUE-BILLING-NOTE    VALUE '1'.
041320             88  CN-CANCEL-BILLING-NOTE   VALUE '2'.
00038      12  CN-BILLING-START-LINE-NO    PIC 99.
00039      12  CN-BILLING-END-LINE-NO      PIC 99.
00040
00041      12  CN-LINES.
00042          16  CN-LINE OCCURS 10       PIC X(77).
00043
00044      12  CN-LAST-MAINT-DT            PIC XX.
00045      12  CN-LAST-MAINT-HHMMSS        PIC S9(7)   COMP-3.
00046      12  CN-LAST-MAINT-USER          PIC X(4).
041320     12  FILLER                      PIC X(5).
00048 ******************************************************************
012413*    COPY ELCEOBC.
      ******************************************************************
      *                                                                *
      *                                                                *
      *                            ELCEOBC                             *
      *                            VMOD=2.001                          *
      *                                                                *
      *   CLAIM SYSTEM EOB CODE TABLE                                  *
      *                                                                *
      *   THIS COPYBOOK IS USED FOR BOTH THE ONLINE AND BATCH          *
      *   VSAM EOB CODE TABLE                                          *
      *                                                                *
      *   FILE DESCRIPTION = EOB CODE TABLE                            *
      *                                                                *
      *   FILE TYPE = VSAM,KSDS                                        *
      *   RECORD SIZE = 350   RECFORM = FIX                            *
      *                                                                *
      *   BASE CLUSTER NAME = ELEOBC                    RKP=2,LEN=15   *
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
      * 120808    2008100900001  PEMA  NEW COPYBOOK/FILE
      * 081511    2011022800001  PEMA  CHG FOR ADMIN SERV NAPERSOFT
091913* 091913    2013090300001  AJRA  ADDITIONAL RECORD TYPES
      ******************************************************************
       01  EOB-CODES.
           12  EO-RECORD-ID                      PIC XX.
               88  VALID-DN-ID                      VALUE 'EO'.
           12  EO-CONTROL-PRIMARY.
               16  EO-COMPANY-CD                 PIC X.
               16  EO-RECORD-TYPE                PIC X.
                   88  EO-EOB-RECS                  VALUE '1'.
                   88  EO-VERIF-RECS                VALUE '2'.
                   88  EO-GCE-RECS                  VALUE '3'.
091913             88  EO-CANC-RECS                 VALUE '4'.
091913             88  EO-BILL-NOTE-RECS            VALUE '5'.
               16  EO-EOB-CODE                   PIC X(4).
               16  FILLER                        PIC X(9).
           12  EO-MAINT-INFORMATION.
               16  EO-LAST-MAINT-DT              PIC XX.
               16  EO-LAST-MAINT-HHMMSS          PIC S9(7)      COMP-3.
               16  EO-LAST-MAINT-USER            PIC X(4).
               16  FILLER                        PIC XX.
           12  EO-DESCRIPTION                    PIC X(275).
           12  FILLER                            PIC X(46).
      ******************************************************************
012413                                 EJECT
121213*    COPY ELCENCC.
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
121213                                 EJECT
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA VAR
                                ACCOUNT-MASTER LETTER-ARCHIVE
                                LETTER-ARCHIVE-TEXT
                                CERTIFICATE-MASTER CHECK-RECORDS
                                CONTROL-FILE COMPENSATION-MASTER
                                MAILING-DATA PENDING-BUSINESS
                                PENDING-PAY-ADJ TEXT-FILES
                                ENDORSEMENT-RECORD CERTIFICATE-NOTE
                                EOB-CODES ENCLOSURE-CODES.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL689' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
02043      MOVE DFHCOMMAREA TO PROGRAM-INTERFACE-BLOCK.
02044
02045      MOVE EIBDATE                TO DC-JULIAN-YYDDD.
02046      MOVE '5'                    TO DC-OPTION-CODE.
02047      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
02048      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.
02049      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE
02050                                     W-CURRENT-SAVE.
010814     MOVE DC-GREG-DATE-A-EDIT    TO W-SAVE-EDIT-A-DATE
010814     MOVE DC-DAY-OF-WEEK         TO W-SAVE-CYCLE-DAY-OF-WEEK
02051
02052      MOVE 2                      TO EMI-NUMBER-OF-LINES.
02053      MOVE ERROR-MESSAGE-INTERFACE-BLOCK
02054                                  TO EMI-SAVE-AREA.
02055
02056      MOVE EIBTRMID               TO W-TS-TERM-TEXT
02057                                     W-TS-TERM-SCREEN.
02058
02059      IF  EIBCALEN EQUAL 0
02060          MOVE UNACCESS-MSG       TO LOGOFF-MSG
02061          GO TO 8300-SEND-TEXT
031504     END-IF.
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
      *       DISPLAY '  KIXSYS = ' var (1:env-var-len)
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
      *       DISPLAY ' WS KIX SYS ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if
           set P to address of KIXHOST
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixhost not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00'
      *       DISPLAY '  KIXHOST = ' var (1:env-var-len)
              MOVE var(1:env-var-len)  to ws-kixhost
      *       unstring var (1:env-var-len) delimited by '/'
      *          into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
      *             WS-KIX-SYS
      *       end-unstring
              DISPLAY ' WS KIX HOST ' WS-KIXSYS
      *       DISPLAY ' WS KIX MYENV ' WS-KIX-MYENV
           end-if
010814
010814**** This routine will connect to the Logic Database on SQL Server
010814**** and call a stored procedure to determine the next business da
010814
010814     PERFORM 4500-CONNECT-TO-DB  THRU 4500-EXIT
010814     IF SQLCODE = 0
010814        PERFORM 4600-GET-NEXT-BUS-DT  THRU 4600-EXIT
010814        IF SQLCODE = 0
010814            MOVE WS-NEXT-BUS-DT TO W-SAVE-NEXT-BUS-DT-EDIT-A
010814                                   W-EDIT-A-DATE
010814            MOVE W-EDIT-A-YY    TO DC-YMD-YEAR
010814            MOVE W-EDIT-A-MM    TO DC-YMD-MONTH
010814            MOVE W-EDIT-A-DD    TO DC-YMD-DAY
010814            MOVE '3'            TO DC-OPTION-CODE
010814            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
010814            IF NO-CONVERSION-ERROR
010814               MOVE DC-BIN-DATE-1 TO W-SAVE-BIN-NEXT-BUS-DT
010814            ELSE
010814               MOVE W-SAVE-BIN-DATE TO DC-BIN-DATE-1
010814               MOVE '6'         TO DC-OPTION-CODE
010814               MOVE ZEROS       TO DC-ELAPSED-MONTHS
010814               IF W-SAVE-CYCLE-DAY-OF-WEEK = 6
010814                  MOVE 3        TO DC-ELAPSED-DAYS
010814               ELSE
010814                 IF W-SAVE-CYCLE-DAY-OF-WEEK = 7
010814                    MOVE 2     TO DC-ELAPSED-DAYS
010814                 ELSE
010814                    MOVE 1     TO DC-ELAPSED-DAYS
010814                 END-IF
010814               END-IF
010814               PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
010814               IF NO-CONVERSION-ERROR
010814                   MOVE DC-BIN-DATE-2 TO W-SAVE-BIN-NEXT-BUS-DT
010814                   MOVE DC-GREG-DATE-A-EDIT TO
010814                                W-SAVE-NEXT-BUS-DT-EDIT-A
010814               ELSE
010814                   MOVE W-SAVE-BIN-DATE TO W-SAVE-BIN-NEXT-BUS-DT
010814                   MOVE W-SAVE-EDIT-A-DATE TO
010814                                W-SAVE-NEXT-BUS-DT-EDIT-A
010814               END-IF
010814            END-IF
010814        ELSE
010814            MOVE W-SAVE-BIN-DATE TO DC-BIN-DATE-1
010814            MOVE '6'         TO DC-OPTION-CODE
010814            MOVE ZEROS       TO DC-ELAPSED-MONTHS
010814            IF W-SAVE-CYCLE-DAY-OF-WEEK = 6
010814               MOVE 3        TO DC-ELAPSED-DAYS
010814            ELSE
010814              IF W-SAVE-CYCLE-DAY-OF-WEEK = 7
010814                 MOVE 2     TO DC-ELAPSED-DAYS
010814              ELSE
010814                 MOVE 1     TO DC-ELAPSED-DAYS
010814              END-IF
010814            END-IF
010814            PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
010814            IF NO-CONVERSION-ERROR
010814                MOVE DC-BIN-DATE-2 TO W-SAVE-BIN-NEXT-BUS-DT
010814                MOVE DC-GREG-DATE-A-EDIT TO
010814                                W-SAVE-NEXT-BUS-DT-EDIT-A
010814            ELSE
010814                MOVE W-SAVE-BIN-DATE TO W-SAVE-BIN-NEXT-BUS-DT
010814                MOVE W-SAVE-EDIT-A-DATE TO
010814                                W-SAVE-NEXT-BUS-DT-EDIT-A
010814            END-IF
010814        END-IF
010814     ELSE
010814        MOVE W-SAVE-BIN-DATE TO DC-BIN-DATE-1
010814        MOVE '6'         TO DC-OPTION-CODE
010814        MOVE ZEROS       TO DC-ELAPSED-MONTHS
010814        IF W-SAVE-CYCLE-DAY-OF-WEEK = 6
010814           MOVE 3        TO DC-ELAPSED-DAYS
010814        ELSE
010814          IF W-SAVE-CYCLE-DAY-OF-WEEK = 7
010814             MOVE 2     TO DC-ELAPSED-DAYS
010814          ELSE
010814             MOVE 1     TO DC-ELAPSED-DAYS
010814          END-IF
010814        END-IF
010814        PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
010814        IF NO-CONVERSION-ERROR
010814            MOVE DC-BIN-DATE-2 TO W-SAVE-BIN-NEXT-BUS-DT
010814            MOVE DC-GREG-DATE-A-EDIT TO
010814                                W-SAVE-NEXT-BUS-DT-EDIT-A
010814        ELSE
010814            MOVE W-SAVE-BIN-DATE TO W-SAVE-BIN-NEXT-BUS-DT
010814            MOVE W-SAVE-EDIT-A-DATE TO
010814                                W-SAVE-NEXT-BUS-DT-EDIT-A
010814        END-IF
010814     END-IF
021214     PERFORM 4700-DISCONNECT THRU 4700-EXIT
010814
02063      IF  PI-689-CREATE-NO-SCREENS
02064          GO TO 0600-BYPASS-SCREEN-CNTL
031504     END-IF.
02065
02066      IF  PI-CALLING-PROGRAM NOT EQUAL W-THIS-PGM
02067          IF  PI-RETURN-TO-PROGRAM NOT EQUAL W-THIS-PGM
02068              MOVE PI-SAVED-PROGRAM-5
02069                                  TO PI-SAVED-PROGRAM-6
02070              MOVE PI-SAVED-PROGRAM-4
02071                                  TO PI-SAVED-PROGRAM-5
02072              MOVE PI-SAVED-PROGRAM-3
02073                                  TO PI-SAVED-PROGRAM-4
02074              MOVE PI-SAVED-PROGRAM-2
02075                                  TO PI-SAVED-PROGRAM-3
02076              MOVE PI-SAVED-PROGRAM-1
02077                                  TO PI-SAVED-PROGRAM-2
02078              MOVE PI-RETURN-TO-PROGRAM
02079                                  TO PI-SAVED-PROGRAM-1
02080              MOVE PI-CALLING-PROGRAM
02081                                  TO PI-RETURN-TO-PROGRAM
02082              MOVE W-THIS-PGM     TO PI-CALLING-PROGRAM
02083              PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
02084              MOVE LOW-VALUES     TO EL689AO
02085              PERFORM 9905-INITIALIZE-SECURITY THRU 9905-EXIT
PEMUNI             IF PI-689-ARCHIVE-NUMBER NOT NUMERIC
PEMUNI                MOVE ZEROS       TO PI-689-ARCHIVE-NUMBEr
PEMUNI             END-IF
02086              IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6311'
02087                      AND
02088                  PI-689-KEY-DATA-FIELDS GREATER THAN LOW-VALUES
02089                  GO TO 0400-EL6311-INCOMING
02090              ELSE
02091                  MOVE PI-689-ARCHIVE-NUMBER
02092                                  TO W-INCOMING-ARCHIVE
02093                  MOVE LOW-VALUES TO PI-1042-WA
02094                                     PI-689-WORK-AREA
02095                  MOVE ZEROS      TO PI-CURRENT-LINE
02096                                     PI-TEMP-STOR-ITEMS
02097                                     PI-TOTAL-LINES
02098                                     PI-UPDATE-SW
02099                                     PI-689-NUMBER-LABEL-LINES
02100                                     PI-689-NUMBER-TEXT-RECORDS
02101                                     PI-689-ERROR
02102                  MOVE SPACES     TO PI-689-PRINT-SW
02103                                     PI-689-ALT-PRINTER-ID
02104                                     PI-689-LBL-OVERRIDE
02105                                     PI-COMM-CONTROL
02106                                     PI-689-FORM-NUMBER
02107                                     PI-689-TEMP-STOR-ID
02108                                     PI-689-LABEL-SOURCE
02109                                     PI-689-USE-SCREEN-IND
02110                  MOVE '2'        TO PI-ACTION
02111                  MOVE -1         TO MAINTL
02112                  MOVE PI-LOWER-CASE-LETTERS
02113                                  TO W-LC-CASE-IND
02114                  IF  W-INCOMING-ARCHIVE NUMERIC
02115                          AND
02116                      W-INCOMING-ARCH-NO GREATER THAN ZEROS
02117                      MOVE 'S'    TO MAINTI
02118                      MOVE +1     TO MAINTL
02119                      MOVE AL-UANON
02120                                  TO MAINTA
02121                      MOVE W-INCOMING-ARCH-NO
02122                                  TO ARCHNUMI
02123                      MOVE +8     TO ARCHNUML
02124                      MOVE AL-UNNON
02125                                  TO ARCHNUMA
02126                      GO TO 1000-SHOW
02127                  ELSE
02128                      GO TO 0450-SPECIAL-INPUT-PROCESS
031504                 END-IF
031504             END-IF
02129          ELSE
02130              MOVE PI-CALLING-PROGRAM TO W-CALL-PGM
02131              MOVE PI-RETURN-TO-PROGRAM
02132                                      TO PI-CALLING-PROGRAM
02133              MOVE PI-SAVED-PROGRAM-1 TO PI-RETURN-TO-PROGRAM
02134              MOVE PI-SAVED-PROGRAM-2 TO PI-SAVED-PROGRAM-1
02135              MOVE PI-SAVED-PROGRAM-3 TO PI-SAVED-PROGRAM-2
02136              MOVE PI-SAVED-PROGRAM-4 TO PI-SAVED-PROGRAM-3
02137              MOVE PI-SAVED-PROGRAM-5 TO PI-SAVED-PROGRAM-4
02138              MOVE PI-SAVED-PROGRAM-6 TO PI-SAVED-PROGRAM-5
02139              MOVE SPACES             TO PI-SAVED-PROGRAM-6
02140              PERFORM 7700-READ-TEXT-TS THRU 7700-EXIT
02141              MOVE LOW-VALUES         TO EL689AO
02142              PERFORM 7740-RESTORE-SCREEN THRU 7749-EXIT
02143              SET W-RG-NDX            TO PI-CURRENT-LINE
02144              PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
02145                  VARYING
02146                      W-SC-NDX FROM 1 BY 1
02147                  UNTIL
02148                      W-SC-NDX GREATER THAN
02149                      W-NUM-LINES-PER-SCREEN
02150              MOVE PI-689-ALT-PRINTER-ID
02151                                      TO PRINTERO
02152              MOVE AL-UANON           TO PRINTERA
02153              MOVE -1                 TO PRINTERL
02154              GO TO 8100-SEND-INITIAL-MAP
031504         END-IF
031504     END-IF.
02155
02156                                  EJECT
02157  0200-RECEIVE.
02158
081004     MOVE PI-689-FATAL-CTR       TO EMI-FATAL-CTR
081004     MOVE PI-689-FORCABLE-CTR    TO EMI-FORCABLE-CTR
02159      IF  EMI-FATAL-CTR GREATER THAN ZEROS
02160              OR
02161          EMI-FORCABLE-CTR GREATER THAN ZEROS
02162          MOVE 'Y'                TO W-HOLD-IND.
02163
02164      MOVE W-TRANSACTION          TO EIBTRNID.
02165      MOVE EMI-SAVE-AREA          TO ERROR-MESSAGE-INTERFACE-BLOCK.
02166      MOVE ZEROS                  TO W-LAST-ERROR.
02167      MOVE LOW-VALUES             TO EL689AI.
02168      MOVE '104A'                 TO W-TS-ID-TEXT.
02169      MOVE EIBTRMID               TO W-TS-TERM-TEXT
02170                                     W-TS-TERM-SCREEN.
02171
02172      
      * EXEC CICS HANDLE AID
02173 *        CLEAR    (9300-DFHCLEAR)
02174 *        PA1      (9200-PA)
02175 *        PA2      (9200-PA)
02176 *        PA3      (9200-PA)
02177 *    END-EXEC.
      *    MOVE '"&=!"#               V! " #00008631' TO DFHEIV0
           MOVE X'22263D212223202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020562120' TO DFHEIV0(13:12)
           MOVE X'2220233030303038363331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02178
02179      
      * EXEC CICS HANDLE CONDITION
02180 *        PGMIDERR (9700-PGMID-ERROR)
02181 *        ERROR    (9800-ABEND)
02182 *        MAPFAIL  (0325-MAPFAIL)
02183 *    END-EXEC.
      *    MOVE '"$L.?                 ! # #00008638' TO DFHEIV0
           MOVE X'22244C2E3F20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303038363338' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02184
02185      
      * EXEC CICS SYNCPOINT
02186 *    END-EXEC.
      *    MOVE '6"                    !   #00008644' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02187
110404     IF PI-LOWER-CASE-LETTERS = 'Y'
02188 *    IF  W-LC-USE-BOTH-CASES
02189          
      * EXEC CICS RECEIVE
02190 *            MAP      (W-MAP)
02191 *            MAPSET   (W-MAPSET)
02192 *            INTO     (EL689AI)
02193 *            ASIS
02194 *        END-EXEC
           MOVE LENGTH OF
            EL689AI
             TO DFHEIV11
      *    MOVE '8"TAI  L              ''   #00008649' TO DFHEIV0
           MOVE X'382254414920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02195      ELSE
02196          
      * EXEC CICS RECEIVE
02197 *            MAP      (W-MAP)
02198 *            MAPSET   (W-MAPSET)
02199 *            INTO     (EL689AI)
02200 *        END-EXEC.
           MOVE LENGTH OF
            EL689AI
             TO DFHEIV11
      *    MOVE '8"T I  L              ''   #00008656' TO DFHEIV0
           MOVE X'382254204920204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303038363536' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AI, 
                 DFHEIV11, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
02201
02202      IF  NOT DISPLAY-CAP
02203          MOVE 'READ'             TO SM-READ
02204          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
02205          MOVE ER-9097            TO EMI-ERROR
02206          MOVE -1                 TO MAINTL
02207          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02208          GO TO 8100-SEND-INITIAL-MAP.
02209
02210      INSPECT MAINTI CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
02211
02212      IF  ENTERPFL EQUAL 0
02213          GO TO 0300-CHECK-PFKEYS.
02214
02215      IF  EIBAID NOT EQUAL DFHENTER
02216          MOVE ER-0004            TO EMI-ERROR
02217          GO TO 0320-INPUT-ERROR.
02218
02219      IF  ENTERPFI NUMERIC
02220              AND
02221          ENTERPFI GREATER THAN 0
02222              AND
02223          ENTERPFI LESS THAN 25
02224          MOVE PF-VALUES (ENTERPFI)
02225                                  TO EIBAID
02226      ELSE
02227          MOVE ER-0029            TO EMI-ERROR
02228          GO TO 0320-INPUT-ERROR.
02229
02230  0300-CHECK-PFKEYS.
02231
02232      IF  EIBAID EQUAL DFHPF23
02233          PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
02234          MOVE ZEROS              TO PI-TOTAL-LINES
02235                                     PI-CURRENT-LINE
02236          MOVE EIBAID             TO PI-ENTRY-CD-1
02237          MOVE W-XCTL-005         TO W-CALL-PGM
02238          GO TO 9400-XCTL.
02239
02240      IF  EIBAID EQUAL DFHPF24
02241          PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
02242          MOVE ZEROS              TO PI-TOTAL-LINES
02243                                     PI-CURRENT-LINE
02244          MOVE W-XCTL-626         TO W-CALL-PGM
02245          GO TO 9400-XCTL.
02246
02247      MOVE SPACES                 TO W-KEY-FIELDS-CHANGED-IND
02248                                     W-FORM-CHANGED-IND.
02249
02250      IF  PI-689-KEY-DATA-FIELDS EQUAL LOW-VALUES OR SPACES
02251              OR
02252          PI-SHOW-MODE
02253          NEXT SENTENCE
02254      ELSE
02255          IF  PI-689-FORM-NUMBER GREATER THAN SPACES
02256                  AND
02257              FORML NOT EQUAL ZEROS
02258                  AND
02259              PI-689-FORM-NUMBER NOT EQUAL FORMI
02260              MOVE FORMI          TO PI-689-FORM-NUMBER
02261              MOVE LOW-VALUES     TO EL689AI
02262                                     PI-1042-WA
02263                                     PI-689-FOLLOW-UP-DATE
02264                                     PI-689-PRINT-RESTRICTION
02265                                     PI-689-RESEND-DATE-1
02268                                     PI-689-RESEND1-EDIT
02271                                     PI-689-FOLLOW-UP-DATE
02272                                     PI-689-FOLLOW-UP-EDIT
02273                                     PI-689-ERROR-IND
02274              MOVE ZEROS          TO PI-689-ERROR
02275                                     PI-689-NUMBER-COPIES
02276                                     PI-689-NUMBER-LABEL-LINES
02277                                     PI-689-NUMBER-TEXT-RECORDS
02278                                     PI-CURRENT-LINE
02279                                     PI-TEMP-STOR-ITEMS
02280                                     PI-TOTAL-LINES
02281                                     PI-UPDATE-SW
02282              MOVE SPACES         TO PI-689-PRINT-SW
02283                                     PI-689-ALT-PRINTER-ID
02284                                     PI-COMM-CONTROL
02285                                     PI-689-TEMP-STOR-ID
02286                                     PI-689-USE-SCREEN-IND
100705                                    PI-689-RESEND-LETR-1
122011                                    PI-CERT-FORM-ID
011013                                    PI-CERT-REQ-IND
011013                                    PI-REASON-REQ-IND
02287              PERFORM 7740-RESTORE-SCREEN THRU 7749-EXIT
02288              MOVE DFHENTER       TO EIBAID
02289              MOVE 'Y'            TO W-FORM-CHANGED-IND
02290              GO TO 0330-FUNCTION-CHECK
02291          ELSE
02292              IF  PI-689-DATA-SOURCE NOT EQUAL DATASORI
02293                      OR
02294                  (PI-689-LABEL-SOURCE NOT EQUAL ADDRSI
02295                          AND
02296                      (PI-689-LABEL-SOURCE NOT EQUAL SPACES
02297                              OR
02298                          ADDRSI NOT EQUAL LOW-VALUES))
02299                      OR
02300                  PI-689-CARRIER NOT EQUAL CARRIERI
02301                      OR
02302                  PI-689-GROUPING NOT EQUAL GROUPI
02303                      OR
02304                  PI-689-STATE NOT EQUAL STATEI
02305                      OR
02306                  PI-689-ACCOUNT NOT EQUAL ACCTI
02307                      OR
02308                  PI-689-CERT-PRIME NOT EQUAL CERTI
02309                      OR
02310                  PI-689-CERT-SFX NOT EQUAL SFXI
02311                      OR
02312                  PI-689-TYPE NOT EQUAL TYPEI
02313                      OR
02314                  PI-689-DATE-EDIT NOT EQUAL DATEI
02315                      OR
02316                  PI-689-RESP-PERSON NOT EQUAL RPERSONI
02317                      OR
02318                  PI-689-SEQ-EDIT NOT EQUAL SEQI
02319                      OR
02320                  PI-689-ENTRY-BATCH NOT EQUAL BENTRYI
02321                      OR
02322                  PI-689-BCSEQ-EDIT NOT EQUAL BCSEQI
02323                  MOVE 'Y'        TO W-KEY-FIELDS-CHANGED-IND
02324                  MOVE DFHENTER   TO EIBAID
02325                  GO TO 0330-FUNCTION-CHECK.
02326
02327 *    IF  EIBAID EQUAL DFHPF1
02328 *        MOVE +7                 TO W-ROLL-COUNTER
02329 *        GO TO 4000-ROLL-PAGE.
02330
02331 *    IF  EIBAID EQUAL DFHPF2
02332 *        MOVE -7                 TO W-ROLL-COUNTER
02333 *        GO TO 4000-ROLL-PAGE.
02334
02335 *    IF  EIBAID EQUAL DFHPF3
02336 *        IF  W-HOLD-ON
02337 *            MOVE ER-7245        TO EMI-ERROR
02338 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02339 *            MOVE -1             TO MAINTL
02340 *            MOVE AL-UABON       TO MAINTA
02341 *            GO TO 8200-SEND-DATAONLY
02342 *        ELSE
02343 *            GO TO 0330-FUNCTION-CHECK.
041811*    IF (EIBAID = DFHPF8)
041811*       AND (PI-PROCESSOR-ID = 'LMLC' OR 'SPJA' OR 'KAWA' OR
061411*         'CAGB' OR 'DLVA' OR 'ECCA' OR 'KRHA' OR 'ALWA')
041811*       MOVE ER-0029             TO EMI-ERROR
041811*       GO TO 0320-INPUT-ERROR
041811*    END-IF
02345 *    IF EIBAID = DFHPF4 OR DFHPF8
02346 *        IF  W-HOLD-ON
02347 *            MOVE ER-7245        TO EMI-ERROR
02348 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02349 *            MOVE -1             TO MAINTL
02350 *            MOVE AL-UABON       TO MAINTA
02351 *            GO TO 8200-SEND-DATAONLY
02352 *        ELSE
02353 *            GO TO 0330-FUNCTION-CHECK.
02354
02355 *    IF  EIBAID = DFHPF5
02356 *        COMPUTE W-ROLL-COUNTER = ((PI-TOTAL-LINES - 1) * -1)
02357 *        GO TO 4000-ROLL-PAGE.
02358
02359 *    IF  EIBAID = DFHPF6
02360 *        MOVE PI-TOTAL-LINES     TO W-ROLL-COUNTER
02361 *        GO TO 4000-ROLL-PAGE.
02362
122011     IF  EIBAID EQUAL DFHPF7
122011         IF PI-PROMPT-IND EQUAL 'Y'
122011             MOVE ER-0894        TO EMI-ERROR
122011             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122011             MOVE -1             TO MAINTL
122011             MOVE AL-UABON       TO MAINTA
122011             GO TO 8200-SEND-DATAONLY
122011         END-IF
122011     END-IF.
122011
02363      IF  EIBAID EQUAL DFHPF7 or dfhpf8
02364          IF  W-HOLD-ON
02365              MOVE ER-7245        TO EMI-ERROR
02366              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02367              MOVE -1             TO MAINTL
02368              MOVE AL-UABON       TO MAINTA
02369              GO TO 8200-SEND-DATAONLY
02370          ELSE
02371              GO TO 0330-FUNCTION-CHECK.
02372
02373      IF  EIBAID EQUAL DFHENTER
02374          GO TO 0330-FUNCTION-CHECK.
02375
02376      MOVE ER-0029                TO EMI-ERROR.
02377
02378  0320-INPUT-ERROR.
02379
02380      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02381
02382      IF  ENTERPFL EQUAL 0
02383          MOVE -1                 TO MAINTL
02384          MOVE AL-UABON           TO MAINTA
02385      ELSE
02386          MOVE AL-UNBON           TO ENTERPFA
02387          MOVE -1                 TO ENTERPFL.
02388
02389      GO TO 8200-SEND-DATAONLY.
02390
02391  0325-MAPFAIL.
02392 ***********************************************************
02393 *      ROUTINE SHOULD ONLY BE PERFORMED WHEN PRINTING     *
02394 *      LETTERS ON A 3275 PRINTER.                         *
02395 ***********************************************************
02396
02397      PERFORM 7700-READ-TEXT-TS THRU 7700-EXIT
02398
02399      SET W-RG-NDX                TO PI-CURRENT-LINE.
02400      PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
02401              VARYING
02402          W-SC-NDX FROM 1 BY 1
02403              UNTIL
02404          W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
02405
02406      MOVE -1                     TO MAINTL.
02407      GO TO 8100-SEND-INITIAL-MAP.
02408                                  EJECT
02409  0330-FUNCTION-CHECK.
02410
02411      IF  MAINTI EQUAL 'S'
02412 *        IF EIBAID = DFHPF4 OR DFHPF8
02413 *            GO TO 7900-PRINT-LETTER-NOW
02414 *        ELSE
02415              GO TO 1000-SHOW.
02416
02417      IF  NOT MODIFY-CAP
02418          MOVE 'UPDATE'           TO SM-READ
02419          PERFORM 9995-SECURITY-VIOLATION
02420          MOVE ER-0070            TO EMI-ERROR
02421          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02422          MOVE -1                 TO MAINTL
02423          GO TO 8100-SEND-INITIAL-MAP.
02424
02425      IF  W-FORM-CHANGED
02426              OR
02427          W-KEY-FIELDS-CHANGED
02428          GO TO 0330-EDIT.
091712
091712     PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
091712
02429
02430 *    IF  EIBAID EQUAL DFHPF3
02431 *        IF  PI-NO-CARRIER-SECURITY
02432 *                AND
02433 *            PI-NO-ACCOUNT-SECURITY
02434 *            GO TO 5000-EDIT-MODE.
02435
02436      IF  EIBAID EQUAL DFHPF7 or dfhpf8
02437          IF  PI-NO-CARRIER-SECURITY
02438                  AND
02439              PI-NO-ACCOUNT-SECURITY
02440              GO TO 5400-LETTER-RELEASE
02441          ELSE
02442              IF  PI-689-PRINT-PERFORMED
02443                  GO TO 5400-LETTER-RELEASE
02444              ELSE
02445                  MOVE AL-UNBON TO ENTERPFA
02446                  MOVE -1       TO ENTERPFL
02447                  MOVE ER-2398  TO EMI-ERROR
02448                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02449                  GO TO 8200-SEND-DATAONLY.
02450
02451 *    IF  EIBAID = DFHPF4 OR DFHPF8
02452 *        GO TO 7900-PRINT-LETTER-NOW.
02453
02454  0330-EDIT.
02455
02456      IF  W-SC-TEXTL (1) NOT EQUAL ZEROS
02457              OR
02458          W-SC-TEXTL (2) NOT EQUAL ZEROS
02459              OR
02460          W-SC-TEXTL (3) NOT EQUAL ZEROS
02461              OR
02462          W-SC-TEXTL (4) NOT EQUAL ZEROS
02463              OR
02464          W-SC-TEXTL (5) NOT EQUAL ZEROS
02465              OR
02466          W-SC-TEXTL (6) NOT EQUAL ZEROS
02467              OR
02468          W-SC-TEXTL (7) NOT EQUAL ZEROS
02469              OR
02470          W-SC-TEXTL (8) NOT EQUAL ZEROS
02471              OR
02472          W-SC-TEXTL (9) NOT EQUAL ZEROS
02473              OR
02474          W-SC-TEXTL (10) NOT EQUAL ZEROS
02475              OR
02476          W-SC-TEXTL (11) NOT EQUAL ZEROS
02477          IF  W-KEY-FIELDS-CHANGED
02478                  OR
02479              W-FORM-CHANGED
02480              MOVE ER-7250        TO EMI-ERROR
02481              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02482              MOVE -1             TO MAINTL
02483              MOVE AL-UABON       TO MAINTA
02484          ELSE
02485              PERFORM 4100-SET-NDX THRU 4100-EXIT
02486              PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
02487                      VARYING
02488                  W-SC-NDX FROM 1 BY 1
02489                      UNTIL
02490                  W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN
02491              MOVE ER-7398        TO EMI-ERROR
02492              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02493              MOVE -1             TO MAINTL
02494              MOVE AL-UABON       TO MAINTA
02495              GO TO 8200-SEND-DATAONLY.
02496
02497      PERFORM 0350-EDIT-ROUTINE THRU 0350-EXIT.
02498
02499      IF  W-KEY-FIELDS-CHANGED
02500              OR
02501          PI-689-KEY-DATA-FIELDS EQUAL LOW-VALUES OR SPACES
02502          PERFORM 0800-EDIT-KEY-FIELDS THRU 0800-EXIT
02503          IF  MAINTI EQUAL 'C'
02504              GO TO 2000-CREATE
02505          ELSE
02506              NEXT SENTENCE
02507      ELSE
02508          IF  W-FORM-CHANGED
02509              PERFORM 0800-EDIT-KEY-FIELDS THRU 0800-EXIT
02510              IF  MAINTI EQUAL 'C'
02511                  GO TO 2000-CREATE
02512              ELSE
02513                  NEXT SENTENCE
02514          ELSE
02515              MOVE -1             TO MAINTL
02516              GO TO 8200-SEND-DATAONLY.
02517
02518      MOVE ER-0023                TO EMI-ERROR.
02519      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02520      MOVE -1                     TO MAINTL.
02521      MOVE AL-UABON               TO MAINTA.
02522      GO TO 8200-SEND-DATAONLY.
02523                                  EJECT
02524  0350-EDIT-ROUTINE.
02525
02526      IF  MAINTI EQUAL 'C'
02527              AND
02528          FORML EQUAL ZEROS
02529          MOVE -1                 TO FORML
02530          MOVE ER-0177            TO EMI-ERROR
02531          MOVE AL-UABON           TO FORMA
02532          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02533
           IF PRTNOWL > ZEROS
              IF PRTNOWI = 'Y' OR 'N' OR 'P' OR ' '
                 MOVE PRTNOWI          TO PI-PRINT-NOW
                 MOVE AL-UANON         TO PRTNOWA
              END-IF
           END-IF
101812     IF ENDARCHL > ZEROS
101812        IF ENDARCHI NUMERIC
101812            MOVE ENDARCHI       TO PI-ENDT-ARCH-NO
101812            MOVE AL-UNNON       TO ENDARCHA
101812            MOVE ENDARCHI       TO W-ERENDT-ARCHIVE
101812            MOVE PI-COMPANY-CD  TO W-ERENDT-COMPANY-CD-A1
101812            MOVE 'N'            TO W-VALID-ENDT-SW
101812            PERFORM 0355-CHECK-ERENDT THRU 0355-EXIT
101812            IF NOT W-VALID-ENDT
101812                MOVE -1         TO ENDARCHL
101812                MOVE ER-1565    TO EMI-ERROR
101812                MOVE AL-UNBON   TO ENDARCHA
101812                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
101812            END-IF
101812        END-IF
011013     ELSE
011013        MOVE ZEROS              TO PI-ENDT-ARCH-NO
101812     END-IF
           MOVE SPACES                 TO PI-689-PRINT-RESTRICTION
02534 *    IF  PRTRESTL NOT EQUAL ZEROS
02535 *        PERFORM 0360-EDIT-PRINT-RESTRICTIONS THRU 0360-EXIT.
02536
02537      IF  FOLLOWL NOT EQUAL ZEROS
02538          MOVE FOLLOWI            TO W-DEEDIT-FIELD
02539          PERFORM 8600-DEEDIT
02540          MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
02541          MOVE '4'                TO DC-OPTION-CODE
02542          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
02543          IF  DATE-CONVERSION-ERROR
02544              MOVE ER-0182        TO EMI-ERROR
02545              MOVE -1             TO FOLLOWL
02546              MOVE AL-UABON       TO FOLLOWA
02547              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02548          ELSE
02549              MOVE DC-GREG-DATE-1-EDIT
02550                                  TO FOLLOWO
02551                                     PI-689-FOLLOW-UP-EDIT
02552              MOVE AL-UANON       TO FOLLOWA
02553              MOVE DC-BIN-DATE-1  TO PI-689-FOLLOW-UP-DATE
02554      ELSE
02555          MOVE LOW-VALUES         TO PI-689-FOLLOW-UP-DATE.
02556
02557      IF  PI-689-FOLLOW-UP-DATE NOT EQUAL LOW-VALUES
091912         IF  PI-689-FOLLOW-UP-DATE LESS THAN
02559                  W-SAVE-BIN-DATE
02560              MOVE ER-0533        TO EMI-ERROR
02561              MOVE AL-UABON       TO FOLLOWA
02562              MOVE -1             TO FOLLOWL
02563              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02564
02565      IF  RESEND1L NOT EQUAL ZEROS
02566          MOVE RESEND1I           TO W-DEEDIT-FIELD
02567          PERFORM 8600-DEEDIT
02568          MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
02569          MOVE '4'                TO DC-OPTION-CODE
02570          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
02571          IF  DATE-CONVERSION-ERROR
02572              MOVE ER-0185        TO EMI-ERROR
02573              MOVE -1             TO RESEND1L
02574              MOVE AL-UABON       TO RESEND1A
02575              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02576          ELSE
02577              MOVE DC-GREG-DATE-1-EDIT
02578                                  TO RESEND1O
02579                                     PI-689-RESEND1-EDIT
02580              MOVE AL-UANON       TO RESEND1A
02581              MOVE DC-BIN-DATE-1  TO PI-689-RESEND-DATE-1
02582      ELSE
02583          MOVE LOW-VALUES         TO PI-689-RESEND-DATE-1.
02584
02646      IF  PI-689-RESEND-DATE-1 NOT EQUAL LOW-VALUES
02647          IF  PI-689-RESEND-DATE-1 NOT GREATER THAN
02648                  W-SAVE-BIN-DATE
02649              MOVE ER-0537        TO EMI-ERROR
02650              MOVE AL-UABON       TO RESEND1A
02651              MOVE -1             TO RESEND1L
02652              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02653
061412     IF ENCL > ZEROS
061412        MOVE FUNCTION UPPER-CASE(ENCI) TO ENCI
061412                                          PI-ENCLOSURE-CD
121213        MOVE SPACES             TO W-ELENCC-KEY
121213        MOVE PI-COMPANY-CD      TO W-ENCC-COMPANY-CD
121213        MOVE '2'                TO W-ENCC-REC-TYPE
121213        MOVE ENCI               TO W-ENCC-ENC-CODE
121213
121213        
      * EXEC CICS READ
121213*           DATASET    (W-ENCC-FILE-ID)
121213*           SET        (ADDRESS OF ENCLOSURE-CODES)
121213*           RIDFLD     (W-ELENCC-KEY)
121213*           RESP       (W-RESPONSE)
121213*       END-EXEC
      *    MOVE '&"S        E          (  N#00009104' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039313034' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ENCC-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ELENCC-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENCLOSURE-CODES TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
121213
121213        IF RESP-NORMAL
061412           MOVE ENCI             TO PI-ENCLOSURE-CD
061412                                    W-ENCLOSURE-CD
061412           MOVE AL-UANON         TO ENCA
061412        ELSE
061412           MOVE ER-1560          TO EMI-ERROR
061412           MOVE -1               TO ENCL
061412           MOVE AL-UABON         TO ENCA
061412           PERFORM 9900-ERROR-FORMAT
061412                                 THRU 9900-EXIT
061412        END-IF
061412     END-IF
061412
061412
02654      IF NOT PI-CREATE-LABELS
02655          MOVE PI-LABEL-CONTROL   TO ADDRLBLI
02656          MOVE AL-PANON           TO ADDRLBLA
02657          GO TO 0350-BYPASS-OVERRIDE.
02658
02659      IF  ADDRLBLL GREATER THAN ZEROS
02660           INSPECT ADDRLBLI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
02661           IF ADDRLBLI = 'N' OR 'Y'
02662               MOVE ADDRLBLI      TO PI-689-LBL-OVERRIDE
02663               MOVE AL-UANON      TO ADDRLBLA
02664           ELSE
02665              MOVE ER-3783        TO EMI-ERROR
02666              MOVE -1             TO ADDRLBLL
02667              MOVE AL-UABON       TO ADDRLBLA
02668              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02669
02670  0350-BYPASS-OVERRIDE.
02671
02672      IF  PI-BYPASS-LABELS
02673              OR
02674          FORMI EQUAL '9999'
02675              OR
02676          PI-689-LABELS-OVERRIDEN
02677          MOVE SPACES             TO PI-689-LABEL-SOURCE
02678          MOVE LOW-VALUES         TO ADDRSI
02679          MOVE AL-UANOF           TO ADDRSA
02680          MOVE ZEROS              TO ADDRSL
02681      ELSE
02682          IF  ADDRSL GREATER THAN ZEROS
02683              MOVE ADDRSI         TO W-LABEL-SOURCE
02684              IF  W-LABEL-SOURCE-VALID
02685                  MOVE ADDRSI     TO PI-689-LABEL-SOURCE
02686              ELSE
02687                  MOVE -1         TO ADDRSL
02688                  MOVE ER-0176    TO EMI-ERROR
02689                  MOVE AL-UABON   TO ADDRSA
02690                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02691          ELSE
02692              IF  MAINTI EQUAL 'C'
02693                  MOVE -1         TO ADDRSL
02694                  MOVE ER-0176    TO EMI-ERROR
02695                  MOVE AL-UABON   TO ADDRSA
02696                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02697
02698      MOVE SPACES                 TO PI-ALT-DMD-PRT-ID.
02699      IF  PRINTERL GREATER ZERO
02700          INSPECT PRINTERI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
02701          MOVE AL-UANON           TO PRINTERA
02702          MOVE PRINTERI           TO PI-689-ALT-PRINTER-ID
02703                                     PI-ALT-DMD-PRT-ID
02704      ELSE
02705          IF  PI-NO-CARRIER-SECURITY
02706                  AND
02707              PI-NO-ACCOUNT-SECURITY
02708              IF  PI-PROCESSOR-PRINTER GREATER THAN SPACES
02709                  MOVE PI-PROCESSOR-PRINTER
02710                                  TO PI-689-ALT-PRINTER-ID
02711                                     PRINTERI
02712                  MOVE AL-UABON   TO PRINTERA
02713              ELSE
02714                  MOVE PI-COMPANY-ID
02715                                  TO W-CNTL-COMPANY-ID
02716                  MOVE '1'        TO W-CNTL-RECORD-TYPE
02717                  MOVE SPACES     TO W-CNTL-GENL
02718                  MOVE ZEROS      TO W-CNTL-SEQ-NO
02719                  
      * EXEC CICS HANDLE CONDITION
02720 *                     NOTOPEN    (8040-CNTL-NOT-OPEN)
02721 *                     NOTFND     (7945-NOT-FOUND)
02722 *                END-EXEC
      *    MOVE '"$JI                  ! $ #00009190' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303039313930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02723                  
      * EXEC CICS READ
02724 *                     DATASET    (W-CNTL-FILE-ID)
02725 *                     SET        (ADDRESS OF CONTROL-FILE)
02726 *                     RIDFLD     (W-CNTL-KEY)
02727 *                END-EXEC
      *    MOVE '&"S        E          (   #00009194' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303039313934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
02728                  MOVE CF-FORMS-PRINTER-ID
02729                                  TO PI-689-ALT-PRINTER-ID
02730                                     PRINTERI
02731                                     W-TS-TERM-TEXT
02732                  MOVE AL-UABON   TO PRINTERA
02733          ELSE
02734              MOVE AL-UABON       TO PRINTERA
02735              MOVE -1             TO PRINTERL
02736              MOVE ER-9283        TO EMI-ERROR
02737              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02738
02739      IF  COPIESL NOT EQUAL ZEROS
02740          IF  COPIESI NOT NUMERIC
02741                  OR
02742              COPIESI EQUAL '0'
02743              MOVE ER-0184        TO EMI-ERROR
02744              MOVE -1             TO COPIESL
02745              MOVE AL-UABON       TO COPIESA
02746              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02747          ELSE
02748              MOVE COPIESI        TO PI-689-NUMBER-COPIES
02749              MOVE AL-UANON       TO COPIESA
02750      ELSE
02751          IF FORMI EQUAL '9999'
02752              MOVE 1              TO PI-689-NUMBER-COPIES
02753                                     COPIESL
02754                                     COPIESO.
02755
02756      IF  FORML NOT EQUAL ZEROS
02757          INSPECT FORMI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
02758          MOVE FORMI              TO PI-689-FORM-NUMBER.
02759
02760      MOVE SPACES                 TO W-HOLD-IND.
122011
091712     IF PI-CERT-REQ-IND = 'Y'
122011       IF  CERTIDL NOT EQUAL ZEROS
122011         IF CERTIDI NUMERIC
122011             MOVE AL-UANON      TO CERTIDA
122011             MOVE CERTIDI       TO PI-CERT-FORM-ID
122011         ELSE
122011             MOVE ER-1778       TO EMI-ERROR
122011             MOVE -1            TO CERTIDL
122011             MOVE AL-UABON      TO CERTIDA
122011             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122011         END-IF
091712       ELSE
091712          MOVE ER-0715       TO EMI-ERROR
091712          MOVE -1            TO CERTIDL
091712          MOVE AL-UABON      TO CERTIDA
091712          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
091712       END-IF
122011     END-IF.
011013
011013     IF PI-REASON-REQ-IND = 'Y'
011013        IF PI-ENDT-ARCH-NO NOT > ZERO
011013          MOVE ER-9840       TO EMI-ERROR
011013          MOVE -1            TO FORML
011013          MOVE AL-UABON      TO FORMA
011013          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011013        END-IF
011013     END-IF.
02761
02762      IF  NOT EMI-NO-ERRORS
02763          IF  EMI-ERROR EQUAL ER-7250
02764              GO TO 0350-EXIT
02765          ELSE
cidmod            MOVE -1             TO MAINTL
02766             GO TO 8200-SEND-DATAONLY.
02767
02768  0350-EXIT.
02769       EXIT.
02770                                  EJECT
101812 0355-CHECK-ERENDT.
101812
101812*     EXEC CICS GETMAIN
101812*          SET      (ADDRESS OF ENDORSEMENT-RECORD)
101812*          LENGTH   (W-ENDT-LENGTH)
101812*     END-EXEC
101812
101812     
      * EXEC CICS READ
101812*         DATASET    ('ERENDT2')
101812*         SET        (ADDRESS OF ENDORSEMENT-RECORD)
101812*         RIDFLD     (W-ERENDT-KEY-BY-ARCH)
101812*         RESP       (W-RESPONSE)
101812*    END-EXEC
           MOVE 'ERENDT2' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00009278' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303039323738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ERENDT-KEY-BY-ARCH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ENDORSEMENT-RECORD TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
101812
101812     IF (RESP-NORMAL)
101812         IF PI-RETURN-TO-PROGRAM EQUAL 'EL6311'  AND
101812            EN-COMPANY-CD  =  PI-COMPANY-CD      AND
101812            EN-CARRIER     =  PI-689-CARRIER     AND
101812            EN-GROUPING    =  PI-689-GROUPING    AND
101812            EN-STATE       =  PI-689-STATE       AND
101812            EN-ACCOUNT     =  PI-689-ACCOUNT     AND
101812            EN-CERT-EFF-DT =  PI-689-EFF-DATE    AND
101812            EN-CERT-NO     =  PI-689-CERT-NO
101812               SET W-VALID-ENDT TO TRUE
101812         END-IF
101812         IF PI-RETURN-TO-PROGRAM EQUAL 'EL1273'  AND
101812            EN-COMPANY-CD  =  PI-COMPANY-CD      AND
101812            EN-CARRIER     =  PI-CARRIER         AND
101812            EN-GROUPING    =  PI-GROUPING        AND
101812            EN-STATE       =  PI-STATE           AND
101812            EN-ACCOUNT     =  PI-ACCOUNT         AND
101812            EN-CERT-EFF-DT =  PI-CERT-EFF-DT     AND
101812            EN-CERT-NO     =  PI-CERT-NO
101812               SET W-VALID-ENDT TO TRUE
101812         END-IF
101812     END-IF
101812      .
101812
101812 0355-EXIT.
101812      EXIT.
101812                                 EJECT
02771  0360-EDIT-PRINT-RESTRICTIONS.
02772
02773 *    INSPECT PRTRESTI CONVERTING W-LOWER-CASE
02774 *                                TO W-UPPER-CASE.
02775 *
02776 *    IF  (PI-689-PRINT-RESTRICTION EQUAL ' ' OR 'F'
02777 *                                     OR 'P' OR LOW-VALUES)
02778 *            AND
02779 *        PRTRESTI EQUAL 'C'
02780 *            AND
02781 *        PI-689-CONTROL NOT GREATER THAN ZEROS
02782 *        MOVE ER-7243            TO EMI-ERROR
02783 *        MOVE -1                 TO PRTRESTL
02784 *        MOVE AL-UABON           TO PRTRESTA
02785 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
02786 *        GO TO 0360-EXIT.
02787 *
02788 *    IF  PRTRESTL NOT EQUAL ZEROS
02789 *        IF PRTRESTI = 'C' OR 'F' OR 'P'
02790 *            MOVE PRTRESTI       TO PI-689-PRINT-RESTRICTION
02791 *            MOVE AL-UANON       TO PRTRESTA
02792 *        ELSE
02793 *            IF  PRTRESTI EQUAL ' '
02794 *                MOVE SPACES     TO PI-689-PRINT-RESTRICTION
02795 *                MOVE AL-UANON   TO PRTRESTA
02796 *            ELSE
02797 *                MOVE ER-7393    TO EMI-ERROR
02798 *                MOVE -1         TO PRTRESTL
02799 *                MOVE AL-UABON   TO PRTRESTA
02800 *                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
02801 *
02802 *    IF  PI-689-PRINT-RESTRICTION EQUAL 'C'
02803 *            AND
02804 *        W-HOLD-ON
02805 *            AND
02806 *        PI-689-CONTROL NOT GREATER THAN ZEROS
02807 *        MOVE ER-7243            TO EMI-ERROR
02808 *        MOVE -1                 TO PRTRESTL
02809 *        MOVE AL-UABON           TO PRTRESTA
02810 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
           .
02812  0360-EXIT.
02813       EXIT.
02814                                  EJECT
02815  0400-EL6311-INCOMING.
02816
02817      MOVE ZEROS                  TO PI-CURRENT-LINE
02818                                     PI-TEMP-STOR-ITEMS
02819                                     PI-TOTAL-LINES
02820                                     PI-UPDATE-SW
02821                                     PI-689-NUMBER-LABEL-LINES
02822                                     PI-689-ERROR
02823                                     PI-689-NUMBER-TEXT-RECORDS.
02824      MOVE SPACES                 TO PI-689-PRINT-SW
02825                                     PI-689-ALT-PRINTER-ID
02826                                     PI-COMM-CONTROL
02827                                     PI-689-FORM-NUMBER
02828                                     PI-689-TEMP-STOR-ID
02829                                     PI-689-LABEL-SOURCE
02830                                     PI-689-USE-SCREEN-IND.
02831
02832      IF PI-CREATE-LABELS
02833          MOVE  AL-UANON          TO ADDRLBLA
02834          MOVE  +1                TO ADDRLBLL.
02835
02836      MOVE PI-LABEL-CONTROL       TO ADDRLBLI
02837                                     PI-689-LBL-OVERRIDE.
02838
02839      MOVE PI-LOWER-CASE-LETTERS  TO W-LC-CASE-IND.
02840      MOVE '2'                    TO PI-ACTION.
02841      MOVE -1                     TO MAINTL.
02842
122011     MOVE AL-UANON               TO PRTNOWA.
122011     MOVE +1                     TO PRTNOWL.
122011     MOVE 'N'                    TO PRTNOWI
122011                                    PI-PRINT-NOW.
122011
02843      IF  PI-689-CARRIER NOT EQUAL LOW-VALUES
02844          MOVE AL-UANON           TO CARRIERA
02845          MOVE +1                 TO CARRIERL
02846          MOVE PI-689-CARRIER     TO CARRIERI.
02847
02848      IF  PI-689-GROUPING NOT EQUAL LOW-VALUES
02849          MOVE +6                 TO GROUPL
02850          MOVE AL-UANON           TO GROUPA
02851          MOVE PI-689-GROUPING    TO GROUPI.
02852
02853      IF  PI-689-STATE NOT EQUAL LOW-VALUES
02854          MOVE +2                 TO STATEL
02855          MOVE AL-UANON           TO STATEA
02856          MOVE PI-689-STATE       TO STATEI.
02857
02858      IF  PI-689-ACCOUNT NOT EQUAL LOW-VALUES
02859          MOVE +10                TO ACCTL
02860          MOVE AL-UANON           TO ACCTA
02861          MOVE PI-689-ACCOUNT     TO ACCTI.
02862
02863      IF  PI-689-CERT-PRIME NOT EQUAL LOW-VALUES
02864          MOVE +10                TO CERTL
02865          MOVE AL-UANON           TO CERTA
02866          MOVE PI-689-CERT-PRIME  TO CERTI.
02867
02868      IF  PI-689-CERT-SFX NOT EQUAL LOW-VALUES
02869          MOVE +1                 TO SFXL
02870          MOVE AL-UANON           TO SFXA
02871          MOVE PI-689-CERT-SFX    TO SFXI.
02872
02873      IF  PI-689-ENTRY-BATCH NOT EQUAL LOW-VALUES
02874          MOVE +6                 TO BENTRYL
02875          MOVE AL-UANON           TO BENTRYA
02876          MOVE PI-689-ENTRY-BATCH TO BENTRYI.
02877
02878      MOVE +4                     TO BCSEQL.
02879      MOVE AL-UANON               TO BCSEQA.
02880      MOVE PI-689-CHG-SEQ-NO      TO BCSEQI.
02881
02882      IF  PI-689-SEQ-NO NOT EQUAL ZEROS
02883          MOVE +8                 TO SEQL
02884          MOVE AL-UANON           TO SEQA
02885          MOVE PI-689-SEQ-NO      TO SEQI.
02886
02887      IF  PI-689-EFF-DATE NOT EQUAL LOW-VALUES
02888          MOVE PI-689-EFF-DATE    TO DC-BIN-DATE-1
02889          MOVE ' '                TO DC-OPTION-CODE
02890          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
02891          MOVE +8                 TO DATEL
02892          MOVE DC-GREG-DATE-1-EDIT
02893                                  TO DATEI
02894          MOVE AL-UANON           TO DATEA.
02895
02896      MOVE -1                     TO MAINTL.
02897      MOVE '4'                    TO DATASORI
02898                                     PI-689-DATA-SOURCE.
02899      MOVE +1                     TO DATASORL.
02900      MOVE AL-UANON               TO DATASORA.
02901
02902      GO TO 8100-SEND-INITIAL-MAP.
02903
02904  0400-EXIT.
02905       EXIT.
02906                                  EJECT
02907  0450-SPECIAL-INPUT-PROCESS.
02908
02909      IF  PI-RETURN-TO-PROGRAM EQUAL 'EL6501'
02910              AND
02911          PI-CARRIER GREATER THAN LOW-VALUES
02912          MOVE '1'                TO DATASORI
02913                                     PI-689-DATA-SOURCE
02914          MOVE PI-CARRIER         TO CARRIERI
02915          MOVE PI-GROUPING        TO GROUPI
02916          MOVE PI-STATE           TO STATEI
02917          MOVE PI-ACCOUNT         TO ACCTI
02918          MOVE PI-CERT-EFF-DT     TO DC-BIN-DATE-1
02919          MOVE ' '                TO DC-OPTION-CODE
02920          PERFORM 9500-LINK-DATE-CONVERT
02921              THRU 9500-EXIT
02922          MOVE DC-GREG-DATE-1-EDIT
02923                                  TO DATEI
122011         MOVE 'N'                TO PRTNOWI
122011                                    PI-PRINT-NOW
02924          MOVE AL-UANON           TO ACCTA
02925                                     CARRIERA
02926                                     DATEA
02927                                     DATASORA
02928                                     GROUPA
02929                                     STATEA
122011                                    PRTNOWA
02930          MOVE +1                 TO CARRIERL
02931                                     DATASORL
122011                                    PRTNOWL
02932          MOVE -1                 TO MAINTL
02933          MOVE +6                 TO GROUPL
02934          MOVE +2                 TO STATEL
02935          MOVE +8                 TO DATEL
02936          MOVE +10                TO ACCTL
02937      ELSE
02938          IF  PI-RETURN-TO-PROGRAM EQUAL 'EL652'
02939                  AND
02940              PI-CARRIER GREATER THAN LOW-VALUES
02941              MOVE '3'            TO DATASORI
02942                                     PI-689-DATA-SOURCE
02943              MOVE PI-CR-CARRIER  TO CARRIERI
02944              MOVE PI-CR-GROUPING TO GROUPI
02945              MOVE PI-CR-ACCOUNT  TO ACCTI
02946              MOVE PI-CR-FIN-RESP TO RPERSONI
02947              MOVE PI-CR-TYPE     TO TYPEI
122011             MOVE 'N'            TO PRTNOWI
122011                                    PI-PRINT-NOW
02948              MOVE AL-UANON       TO CARRIERA
02949                                     GROUPA
02950                                     ACCTA
02951                                     DATASORA
02952                                     RPERSONA
02953                                     TYPEA
122011                                    PRTNOWA
02954              MOVE +1             TO CARRIERL
02955                                     DATASORL
02956                                     TYPEL
122011                                    PRTNOWL
02957              MOVE -1             TO MAINTL
02958              MOVE +6             TO GROUPL
02959              MOVE +10            TO ACCTL
02960                                     RPERSONL
02961          ELSE
02962              IF  PI-RETURN-TO-PROGRAM EQUAL 'EL1273'
02963                      AND
02964                  PI-CARRIER GREATER THAN LOW-VALUES
02965                  MOVE '2'        TO DATASORI
02966                                     PI-689-DATA-SOURCE
02967                  MOVE PI-CARRIER TO CARRIERI
02968                  MOVE PI-GROUPING
02969                                  TO GROUPI
02970                  MOVE PI-STATE   TO STATEI
02971                  MOVE PI-ACCOUNT TO ACCTI
02972                  MOVE PI-CERT-PRIME
02973                                  TO CERTI
02974                  MOVE PI-CERT-SFX
02975                                  TO SFXI
02976                  MOVE PI-CERT-EFF-DT
02977                                  TO DC-BIN-DATE-1
02978                  MOVE ' '        TO DC-OPTION-CODE
02979                  PERFORM 9500-LINK-DATE-CONVERT
02980                      THRU 9500-EXIT
02981                  MOVE DC-GREG-DATE-1-EDIT
02982                                  TO DATEI
122011                 MOVE 'N'        TO PRTNOWI
122011                                    PI-PRINT-NOW
02983                  MOVE AL-UANON   TO CARRIERA
02984                                     GROUPA
02985                                     STATEA
02986                                     ACCTA
02987                                     CERTA
02988                                     SFXA
02989                                     DATEA
02990                                     ADDRSA
02991                                     DATASORA
122011                                    PRTNOWA
02992                  MOVE +1         TO CARRIERL
02993                                     SFXL
02994                                     DATASORL
02995                                     ADDRSL
122011                                    PRTNOWL
02996                  MOVE -1         TO MAINTL
02997                  MOVE +6         TO GROUPL
02998                  MOVE +2         TO STATEL
02999                  MOVE +8         TO DATEL
03000                  MOVE +10        TO ACCTL
03001                                     CERTL.
03002
03003      IF PI-CREATE-LABELS
03004          MOVE  AL-UANON          TO ADDRLBLA
03005          MOVE  +1                TO ADDRLBLL.
03006
03007      MOVE PI-LABEL-CONTROL       TO ADDRLBLI
03008                                     PI-689-LBL-OVERRIDE.
03009
03010      MOVE LOW-VALUES             TO PI-689-KEY-DATA-FIELDS.
03011      GO TO 8100-SEND-INITIAL-MAP.
03012
03013  0450-EXIT.
03014       EXIT.
03015                                  EJECT
03016  0600-BYPASS-SCREEN-CNTL.
03017 ***************************************************************
03018 *     THIS ROUTINE WILL PROCESS A NEW LETTER REQUIREMENT      *
03019 *     WITH THE INFORMATION FOUND IN THE PI AREA WITHOUT       *
03020 *     PERFORMING ANY SCREEN IO.                               *
03021 ***************************************************************
03022
03023      
      * EXEC CICS HANDLE CONDITION
03024 *        PGMIDERR (9700-PGMID-ERROR)
03025 *        MAPFAIL  (0325-MAPFAIL)
03026 *        ERROR    (9800-ABEND)
03027 *    END-EXEC.
      *    MOVE '"$L?.                 ! % #00009581' TO DFHEIV0
           MOVE X'22244C3F2E20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303039353831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03028
03029      MOVE LOW-VALUES             TO EL689AI.
03030
03031      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM
03032
03033      MOVE PI-689-FORM-NUMBER     TO FORMI.
03034      MOVE +4                     TO FORML.
03035
03036      IF  PI-689-NUMBER-COPIES GREATER THAN ZERO
03037          MOVE PI-689-NUMBER-COPIES
03038                                  TO COPIESI
03039          MOVE +1                 TO COPIESL.
03040
03041      MOVE PI-689-LABEL-SOURCE    TO ADDRSI.
03042      MOVE +1                     TO ADDRSL.
03043      MOVE PI-689-DATA-SOURCE     TO DATASORI.
03044      MOVE +1                     TO DATASORL.
03045
03046 *    IF  PI-689-PRINT-RESTRICTION = 'C' OR 'F' OR 'P'
03047 *        MOVE PI-689-PRINT-RESTRICTION
03048 *                                TO PRTRESTI
03049 *        MOVE +1                 TO PRTRESTL.
03050
03051      MOVE SPACES                 TO PI-689-PRINT-SW
03052                                     PI-689-LBL-OVERRIDE.
03053      MOVE ZEROS                  TO PI-TOTAL-LINES
03054                                     PI-CURRENT-LINE
03055                                     PI-TEMP-STOR-ITEMS
03056                                     PI-UPDATE-SW
03057                                     PI-689-ERROR
03058                                     PI-689-NUMBER-LABEL-LINES
03059                                     PI-689-NUMBER-TEXT-RECORDS.
03060
03061      IF  PI-689-ALT-PRINTER-ID NOT EQUAL LOW-VALUES
03062          MOVE +4                 TO PRINTERL
03063          MOVE PI-689-ALT-PRINTER-ID
03064                                  TO PRINTERI PI-ALT-DMD-PRT-ID.
03065
03066      MOVE SPACES                 TO PI-689-ALT-PRINTER-ID.
03067
03068      IF  PI-689-SOURCE-VARIABLE
03069          PERFORM 0700-GET-VARIABLE-LABEL THRU 0700-EXIT.
03070
03071      MOVE SPACES                 TO W-CNTL-KEY
03072      MOVE LOW-VALUES             TO W-ACCT-KEY
03073                                     W-CERT-KEY
03074                                     W-CHEK-KEY
03075                                     W-COMP-KEY
03076                                     W-MAIL-KEY
03077                                     W-PNDB-KEY
03078                                     W-PNDB2-KEY
03079                                     W-PYAJ-KEY.
03080
03081      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
03082      MOVE PI-COMPANY-CD          TO W-ACCT-COMPANY-CD
03083                                     W-CERT-COMPANY-CD
03084                                     W-CHEK-COMPANY-CD
03085                                     W-COMP-COMPANY-CD
03086                                     W-MAIL-COMPANY-CD
03087                                     W-PNDB-COMPANY-CD
03088                                     W-PNDB2-COMPANY-CD
03089                                     W-PYAJ-COMPANY-CD.
03090
03091      IF  PI-689-CARRIER GREATER THAN LOW-VALUES
03092          MOVE PI-689-CARRIER     TO W-ACCT-CARRIER
03093                                     W-CERT-CARRIER
03094                                     W-MAIL-CARRIER
03095                                     W-CERT-CARRIER
03096                                     W-COMP-CARRIER
03097                                     W-CHEK-CARRIER
03098                                     W-PNDB2-CARRIER
03099                                     W-PYAJ-CARRIER.
03100
03101      IF  PI-689-GROUPING GREATER THAN LOW-VALUES
03102          MOVE PI-689-GROUPING    TO W-ACCT-GROUPING
03103                                     W-CERT-GROUPING
03104                                     W-CHEK-GROUPING
03105                                     W-COMP-GROUPING
03106                                     W-MAIL-GROUPING
03107                                     W-PNDB2-GROUPING
03108                                     W-PYAJ-GROUPING.
03109
03110      IF  PI-689-STATE GREATER THAN LOW-VALUES
03111          MOVE PI-689-STATE       TO W-ACCT-STATE
03112                                     W-CERT-STATE
03113                                     W-CHEK-STATE
03114                                     W-MAIL-STATE
03115                                     W-PNDB2-STATE.
03116
03117      IF  PI-689-ACCOUNT GREATER THAN LOW-VALUES
03118          IF PI-689-DATA-SOURCE = '3' OR '6'
03119              MOVE PI-689-ACCOUNT TO W-COMP-ACCOUNT
03120                                     W-PYAJ-ACCOUNT
03121          ELSE
03122              MOVE PI-689-ACCOUNT TO W-ACCT-ACCOUNT
03123                                     W-CERT-ACCOUNT
03124                                     W-CHEK-ACCOUNT
03125                                     W-MAIL-ACCOUNT
03126                                     W-PNDB2-ACCOUNT.
03127
03128      IF  PI-689-EFF-DATE GREATER THAN LOW-VALUES
03129          MOVE PI-689-EFF-DATE    TO W-CERT-EFF-DT
03130                                     W-CHEK-EFF-DT
03131                                     W-MAIL-EFF-DT
03132                                     W-PNDB2-EFF-DT.
03133
03134      IF  PI-689-CERT-PRIME GREATER THAN LOW-VALUES
03135          MOVE PI-689-CERT-PRIME  TO W-CERT-CERT-PRIME
03136                                     W-CHEK-CERT-PRIME
03137                                     W-MAIL-CERT-PRIME
03138                                     W-PNDB2-CERT-PRIME
03139
03140          IF  PI-689-CERT-SFX GREATER THAN LOW-VALUES
03141              MOVE PI-689-CERT-SFX
03142                                  TO W-CERT-CERT-SFX
03143                                     W-CHEK-CERT-SFX
03144                                     W-MAIL-CERT-SFX
03145                                     W-PNDB2-CERT-SFX
03146          ELSE
03147              MOVE SPACES         TO W-CERT-CERT-SFX
03148                                     W-CHEK-CERT-SFX
03149                                     W-MAIL-CERT-SFX
03150                                     W-PNDB2-CERT-SFX.
03151
03152      IF  PI-689-RESP-PERSON GREATER THAN LOW-VALUES
03153          IF PI-689-DATA-SOURCE = '3' OR '6'
03154              MOVE PI-689-RESP-PERSON
03155                                  TO W-COMP-RESP-PERSON
03156                                     W-PYAJ-FIN-RESP.
03157
03158      IF  PI-689-TYPE GREATER THAN LOW-VALUES
03159          IF PI-689-DATA-SOURCE = '3' OR '6'
03160              MOVE PI-689-TYPE    TO W-COMP-TYPE
03161                                     W-PYAJ-RECORD-TYPE
03162          ELSE
03163              MOVE PI-689-TYPE    TO W-PNDB2-TYPE.
03164
03165      IF  PI-689-SEQ-NO GREATER THAN ZEROS
03166          MOVE PI-689-SEQ-NO      TO W-PNDB-SEQ-NO
03167                                     W-PYAJ-FILE-SEQ-NO
03168                                     W-CHEK-SEQ-NO.
03169
03170      IF  PI-689-CHG-SEQ-NO GREATER THAN ZEROS
03171          MOVE PI-689-CHG-SEQ-NO  TO W-PNDB-CHG-SEQ-NO
03172                                     W-PNDB2-ALT-CHG-SEQ-NO.
03173
03174      IF  PI-689-ENTRY-BATCH GREATER THAN LOW-VALUES
03175          MOVE PI-689-ENTRY-BATCH TO W-PNDB-ENTRY.
03176
03177      IF  PI-689-NO-ERRORS-DETECTED
03178          PERFORM 2000-CREATE THRU 2000-EXIT
03179          IF  NOT PI-689-FATAL-ERROR
03180                  AND
03181              NOT PI-689-ERR-DETECTED-PREV
03182              IF  PI-689-PRINT-ONLY
03183                  PERFORM 7900-PRINT-LETTER-NOW THRU 7900-EXIT
03184              ELSE
03185                  IF  PI-689-PRINT-FIRST
03186                      PERFORM 7900-PRINT-LETTER-NOW THRU 7900-EXIT
03187                      IF  NOT PI-689-FATAL-ERROR
03188                          IF  PI-689-ARCHIVE-LETTER
03189                              PERFORM 5400-LETTER-RELEASE
03190                                  THRU 5400-EXIT
03191                          ELSE
03192                              MOVE ER-7390
03193                                  TO PI-689-ERROR
03194                      ELSE
03195                          NEXT SENTENCE
03196                  ELSE
03197                      IF  NOT PI-689-ARCHIVE-LETTER
03198                          MOVE ER-7390
03199                                  TO PI-689-ERROR
03200                      ELSE
03201                          PERFORM 5400-LETTER-RELEASE
03202                              THRU 5400-EXIT
03203                          IF  NOT PI-689-FATAL-ERROR
03204                              IF  PI-689-PRINT-SECOND
03205                                  PERFORM 7900-PRINT-LETTER-NOW
03206                                      THRU 7900-EXIT.
03207
03208  0600-EXIT.
03209      EXIT.
03210
03211  0620-RETURN-TO-CALLER.
03212
03213      MOVE PROGRAM-INTERFACE-BLOCK
03214                                  TO DFHCOMMAREA.
03215      
      * EXEC CICS RETURN
03216 *    END-EXEC.
      *    MOVE '.(                    ''   #00009773' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303039373733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03217
03218  0700-GET-VARIABLE-LABEL.
03219
03220      IF  PI-689-VARIABLE-DATA-1 NOT GREATER THAN SPACES
03221          MOVE '(VARIABLE NOT PROVIDED)'
03222                                  TO W-RC-TEXT (1)
03223                                     W-VG-TEXT (48)
03224          GO TO 0700-EXIT.
03225
03226      MOVE PI-689-VARIABLE-DATA-1 TO W-RC-TEXT (1)
03227                                     W-VG-TEXT (48).
03228      MOVE PI-689-VARIABLE-DATA-2 TO W-RC-TEXT (2)
03229                                     W-VG-TEXT (49).
03230      MOVE PI-689-VARIABLE-DATA-3 TO W-RC-TEXT (3)
03231                                     W-VG-TEXT (50).
03232      MOVE PI-689-VARIABLE-DATA-4 TO W-RC-TEXT (4)
03233                                     W-VG-TEXT (51).
03234      SET W-RG-NDX                TO +5.
03235
03236  0700-EXIT.
03237      EXIT.
03238                                  EJECT
03239  0800-EDIT-KEY-FIELDS.
03240 ***************************************************************
03241 *     THIS ROUTINE WILL DECIDE WHAT FIELDS ARE NECESSARY      *
03242 *     FROM THE DATA SOURCE FIELD.  IF A NECESSARY FIELD       *
03243 *     IS NOT PROVIDED AN ERROR WILL PREVENT FURTHER           *
03244 *     PROCESSING.                                             *
03245 ***************************************************************
03246      MOVE SPACES                 TO W-CNTL-KEY
03247      MOVE LOW-VALUES             TO W-ACCT-KEY
03248                                     W-CERT-KEY
03249                                     W-CHEK-KEY
03250                                     W-COMP-KEY
03251                                     W-MAIL-KEY
03252                                     W-PNDB-KEY
03253                                     W-PNDB2-KEY
03254                                     W-PYAJ-KEY.
03255
03256      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
03257      MOVE PI-COMPANY-CD          TO W-ACCT-COMPANY-CD
03258                                     W-CERT-COMPANY-CD
03259                                     W-CHEK-COMPANY-CD
03260                                     W-COMP-COMPANY-CD
03261                                     W-MAIL-COMPANY-CD
03262                                     W-PNDB-COMPANY-CD
03263                                     W-PNDB2-COMPANY-CD
03264                                     W-PYAJ-COMPANY-CD.
03265
03266      PERFORM 0900-EDIT-CARRIER THRU 0900-EXIT.
03267      PERFORM 0905-EDIT-GROUPING THRU 0905-EXIT.
03268      PERFORM 0910-EDIT-STATE THRU 0910-EXIT.
03269      PERFORM 0915-EDIT-ACCOUNT THRU 0915-EXIT.
03270      PERFORM 0920-EDIT-EXP-DATE THRU 0920-EXIT.
03271      PERFORM 0925-EDIT-EFF-DATE THRU 0925-EXIT.
03272      PERFORM 0930-EDIT-CERTIFICATE THRU 0930-EXIT.
03273      PERFORM 0935-EDIT-RESP-PERSON THRU 0935-EXIT.
03274      PERFORM 0940-EDIT-ACCOUNT THRU 0940-EXIT.
03275      PERFORM 0945-EDIT-TYPE THRU 0945-EXIT.
03276      PERFORM 0950-EDIT-ENTRY THRU 0950-EXIT.
03277      PERFORM 0955-EDIT-SEQUENCE THRU 0955-EXIT.
03278      PERFORM 0960-EDIT-BATCH-SEQ THRU 0960-EXIT.
03279      PERFORM 0965-EDIT-TYPE THRU 0965-EXIT.
011013     PERFORM 0970-EDIT-CSO THRU 0970-EXIT.
03280
03281      IF  DATASORI NUMERIC
03282          MOVE DATASORI           TO W-DATA-SOURCE
03283                                     PI-689-DATA-SOURCE
03284          GO TO 0800-SOURCE-ACCOUNT
03285                0800-SOURCE-CERTIFICATE
03286                0800-SOURCE-COMPENSATION
03287                0800-SOURCE-PENDING
03288                0800-SOURCE-CHEK
03289                0800-SOURCE-PAYMENTS-ADJS
03290                                  DEPENDING ON W-DATA-SOURCE.
03291
03292      IF  FORMI NOT EQUAL '9999'
03293          MOVE ER-7365            TO EMI-ERROR
03294          MOVE -1                 TO MAINTL
03295          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03296          GO TO 8200-SEND-DATAONLY
03297
03298      ELSE
03299          GO TO 0800-EXIT.
03300
03301  0800-SOURCE-ACCOUNT.
03302
03303      IF  W-ACCT-COMPANY-CD EQUAL LOW-VALUES
03304              OR
03305          W-ACCT-CARRIER EQUAL LOW-VALUES
03306              OR
03307          W-ACCT-GROUPING EQUAL LOW-VALUES
03308              OR
03309          W-ACCT-STATE EQUAL LOW-VALUES
03310              OR
03311          W-ACCT-ACCOUNT EQUAL LOW-VALUES
03312              OR
03313          W-ACCT-EXP-DT EQUAL LOW-VALUES
03314          MOVE ER-7376            TO EMI-ERROR
03315          MOVE -1                 TO MAINTL
03316          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03317
03318      GO TO 0800-ERROR-TEST.
03319
03320  0800-SOURCE-CERTIFICATE.
03321
03322      IF  W-CERT-COMPANY-CD EQUAL LOW-VALUES
03323              OR
03324          W-CERT-CARRIER EQUAL LOW-VALUES
03325              OR
03326          W-CERT-GROUPING EQUAL LOW-VALUES
03327              OR
03328          W-CERT-STATE EQUAL LOW-VALUES
03329              OR
03330          W-CERT-ACCOUNT EQUAL LOW-VALUES
03331              OR
03332          W-CERT-EFF-DT EQUAL LOW-VALUES
03333              OR
03334          W-CERT-CERT-NO EQUAL LOW-VALUES
03335          MOVE ER-7377            TO EMI-ERROR
03336          MOVE -1                 TO MAINTL
03337          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03338
03339      GO TO 0800-ERROR-TEST.
03340
03341  0800-SOURCE-COMPENSATION.
03342
03343      IF  W-COMP-COMPANY-CD EQUAL LOW-VALUES
03344              OR
03345          W-COMP-CARRIER EQUAL LOW-VALUES
03346              OR
03347          W-COMP-GROUPING EQUAL LOW-VALUES
03348              OR
03349          W-COMP-RESP-PERSON EQUAL LOW-VALUES
03350              OR
03351          W-COMP-TYPE EQUAL LOW-VALUES
03352              OR
03353          (W-COMP-TYPE EQUAL 'A'
03354                  AND
03355              W-COMP-ACCOUNT EQUAL LOW-VALUES)
03356          MOVE ER-7378            TO EMI-ERROR
03357          MOVE -1                 TO MAINTL
03358          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03359
03360      GO TO 0800-ERROR-TEST.
03361
03362  0800-SOURCE-PENDING.
03363
03364      IF  BENTRYL EQUAL ZEROS
03365          IF  W-PNDB2-COMPANY-CD EQUAL LOW-VALUES
03366                  OR
03367              W-PNDB2-CARRIER EQUAL LOW-VALUES
03368                  OR
03369              W-PNDB2-GROUPING EQUAL LOW-VALUES
03370                  OR
03371              W-PNDB2-STATE EQUAL LOW-VALUES
03372                  OR
03373              W-PNDB2-ACCOUNT EQUAL LOW-VALUES
03374                  OR
03375              W-PNDB2-EFF-DT EQUAL LOW-VALUES
03376                  OR
03377              W-PNDB2-CERT-NO EQUAL LOW-VALUES
03378                  OR
03379              W-PNDB2-TYPE EQUAL LOW-VALUES
03380                  OR
03381              W-PNDB2-ALT-CHG-SEQ-NO NOT = ZEROS
03382              MOVE ER-7379        TO EMI-ERROR
03383              MOVE -1             TO MAINTL
03384              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03385          ELSE
03386              NEXT SENTENCE
03387      ELSE
03388          IF  W-PNDB-COMPANY-CD EQUAL LOW-VALUES
03389                  OR
03390              W-PNDB-ENTRY EQUAL LOW-VALUES
03391                  OR
03392              W-PNDB-SEQ-NO EQUAL ZEROS
03393              MOVE ER-7379        TO EMI-ERROR
03394              MOVE -1             TO MAINTL
03395              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03396
03397      GO TO 0800-ERROR-TEST.
03398
03399  0800-SOURCE-CHEK.
03400
03401      IF  W-CHEK-COMPANY-CD EQUAL LOW-VALUES
03402              OR
03403          W-CHEK-CARRIER EQUAL LOW-VALUES
03404              OR
03405          W-CHEK-GROUPING EQUAL LOW-VALUES
03406              OR
03407          W-CHEK-STATE EQUAL LOW-VALUES
03408              OR
03409          W-CHEK-ACCOUNT EQUAL LOW-VALUES
03410              OR
03411          W-CHEK-EFF-DT EQUAL LOW-VALUES
03412              OR
03413          W-CHEK-CERT-NO EQUAL LOW-VALUES
03414          MOVE ER-7381            TO EMI-ERROR
03415          MOVE -1                 TO MAINTL
03416          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03417
03418      GO TO 0800-ERROR-TEST.
03419
03420  0800-SOURCE-PAYMENTS-ADJS.
03421
03422      IF  W-PYAJ-COMPANY-CD EQUAL LOW-VALUES
03423              OR
03424          W-PYAJ-CARRIER EQUAL LOW-VALUES
03425              OR
03426          W-PYAJ-GROUPING EQUAL LOW-VALUES
03427              OR
03428          W-PYAJ-FIN-RESP EQUAL LOW-VALUES
03429              OR
03430          W-PYAJ-ACCOUNT EQUAL LOW-VALUES
03431              OR
03432          W-PYAJ-RECORD-TYPE EQUAL LOW-VALUES
03433          MOVE ER-7396            TO EMI-ERROR
03434          MOVE -1                 TO MAINTL
03435          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03436
03437      GO TO 0800-ERROR-TEST.
03438
03439  0800-ERROR-TEST.
03440
03441      IF  NOT EMI-NO-ERRORS
03442          IF  EMI-ERROR EQUAL ER-7250
03443              GO TO 0800-EXIT
03444          ELSE
03445              GO TO 8200-SEND-DATAONLY.
03446
03447  0800-EXIT.
03448      EXIT.
03449                                  EJECT
03450  0900-EDIT-CARRIER.
03451
03452      IF  CARRIERL EQUAL ZEROES
03453          MOVE LOW-VALUES         TO PI-689-CARRIER
03454                                     W-ACCT-CARRIER
03455                                     W-CERT-CARRIER
03456                                     W-COMP-CARRIER
03457                                     W-CHEK-CARRIER
03458                                     W-MAIL-CARRIER
03459                                     W-PNDB2-CARRIER
03460                                     W-PYAJ-CARRIER
03461      ELSE
03462          PERFORM 0990-CARRIER-VALIDITY-CHECK THRU 0990-EXIT.
03463
03464  0900-EXIT.
03465      EXIT.
03466                                  EJECT
03467  0905-EDIT-GROUPING.
03468
03469      IF  GROUPL EQUAL ZEROES
03470          MOVE LOW-VALUES         TO PI-689-GROUPING
03471                                     W-ACCT-GROUPING
03472                                     W-CERT-GROUPING
03473                                     W-CHEK-GROUPING
03474                                     W-COMP-GROUPING
03475                                     W-MAIL-GROUPING
03476                                     W-PNDB2-GROUPING
03477                                     W-PYAJ-GROUPING
03478      ELSE
03479          MOVE AL-UANON           TO GROUPA
03480          MOVE GROUPI             TO PI-689-GROUPING
03481                                     W-ACCT-GROUPING
03482                                     W-CERT-GROUPING
03483                                     W-CHEK-GROUPING
03484                                     W-COMP-GROUPING
03485                                     W-MAIL-GROUPING
03486                                     W-PNDB2-GROUPING
03487                                     W-PYAJ-GROUPING.
03488
03489  0905-EXIT.
03490      EXIT.
03491                                  EJECT
03492  0910-EDIT-STATE.
03493
03494      IF  STATEL EQUAL ZEROES
03495          MOVE LOW-VALUES         TO PI-689-STATE
03496                                     W-ACCT-STATE
03497                                     W-CERT-STATE
03498                                     W-CHEK-STATE
03499                                     W-MAIL-STATE
03500                                     W-PNDB2-STATE
03501      ELSE
03502          PERFORM 0994-STATE-VALIDITY-CHECK THRU 0994-EXIT.
03503
03504  0910-EXIT.
03505      EXIT.
03506                                  EJECT
03507  0915-EDIT-ACCOUNT.
03508
03509      IF  ACCTL EQUAL ZEROES
03510          MOVE LOW-VALUES         TO PI-689-ACCOUNT
03511                                     W-ACCT-ACCOUNT
03512                                     W-CERT-ACCOUNT
03513                                     W-CHEK-ACCOUNT
03514                                     W-COMP-ACCOUNT
03515                                     W-MAIL-ACCOUNT
03516                                     W-PNDB2-ACCOUNT
03517                                     W-PYAJ-ACCOUNT
03518      ELSE
03519          MOVE AL-UANON           TO ACCTA
03520          MOVE ACCTI              TO PI-689-ACCOUNT
03521                                     W-ACCT-ACCOUNT
03522                                     W-CERT-ACCOUNT
03523                                     W-CHEK-ACCOUNT
03524                                     W-COMP-ACCOUNT
03525                                     W-MAIL-ACCOUNT
03526                                     W-PYAJ-ACCOUNT
03527                                     W-PNDB2-ACCOUNT.
03528
03529  0915-EXIT.
03530      EXIT.
03531                                  EJECT
03532  0920-EDIT-EXP-DATE.
03533
03534      IF  DATEL NOT EQUAL ZEROS
03535          IF  DATEI EQUAL '99/99/99' OR '999999'
03536              MOVE '99/99/99'     TO DATEO
03537                                     PI-689-DATE-EDIT
03538              MOVE AL-UANON       TO DATEA
03539              MOVE HIGH-VALUES    TO PI-689-EXP-DATE
03540                                     W-ACCT-EXP-DT
03541          ELSE
03542              MOVE DATEI          TO W-DEEDIT-FIELD
03543              PERFORM 8600-DEEDIT
03544              MOVE W-DEEDIT-FIELD-V0
03545                                  TO DC-GREG-DATE-1-MDY
03546              MOVE '4'            TO DC-OPTION-CODE
03547              PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03548              IF  DATE-CONVERSION-ERROR
03549                  MOVE ER-0454    TO EMI-ERROR
03550                  MOVE -1         TO DATEL
03551                  MOVE AL-UABON   TO DATEA
03552                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03553              ELSE
03554                  MOVE DC-GREG-DATE-1-EDIT
03555                                  TO DATEO
03556                                     PI-689-DATE-EDIT
03557                  MOVE AL-UANON   TO DATEA
03558                  MOVE DC-BIN-DATE-1
03559                                  TO PI-689-EXP-DATE
03560                                     W-ACCT-EXP-DT
03561      ELSE
03562          MOVE LOW-VALUES         TO PI-689-EXP-DATE
03563                                     W-ACCT-EXP-DT
03564                                     PI-689-DATE-EDIT.
03565
03566  0920-EXIT.
03567      EXIT.
03568                                  EJECT
03569  0925-EDIT-EFF-DATE.
03570
03571      IF  CERTL EQUAL ZERO
03572              AND
03573          DATASORI NOT EQUAL '2'
03574          GO TO 0925-EXIT.
03575
03576      IF  DATEL NOT EQUAL ZEROS
03577          MOVE DATEI              TO W-DEEDIT-FIELD
03578          PERFORM 8600-DEEDIT
03579          MOVE W-DEEDIT-FIELD-V0  TO DC-GREG-DATE-1-MDY
03580          MOVE '4'                TO DC-OPTION-CODE
03581          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
03582
03583          IF  DATE-CONVERSION-ERROR
03584              MOVE ER-0215        TO EMI-ERROR
03585              MOVE -1             TO DATEL
03586              MOVE AL-UABON       TO DATEA
03587              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03588          ELSE
03589              MOVE DC-GREG-DATE-1-EDIT
03590                                  TO DATEO
03591                                     PI-689-DATE-EDIT
03592              MOVE AL-UANON       TO DATEA
03593              MOVE DC-BIN-DATE-1  TO PI-689-EFF-DATE
03594                                     W-CERT-EFF-DT
03595                                     W-CHEK-EFF-DT
03596                                     W-MAIL-EFF-DT
03597                                     W-PNDB2-EFF-DT
03598      ELSE
03599          MOVE LOW-VALUES         TO PI-689-EFF-DATE
03600                                     W-PNDB2-EFF-DT
03601                                     W-CHEK-EFF-DT
03602                                     W-MAIL-EFF-DT
03603                                     W-CERT-EFF-DT
03604                                     PI-689-DATE-EDIT.
03605
03606  0925-EXIT.
03607      EXIT.
03608                                  EJECT
03609  0930-EDIT-CERTIFICATE.
03610
03611      IF  CERTL EQUAL ZEROES
03612          MOVE LOW-VALUES         TO PI-689-CERT-PRIME
03613                                     W-CERT-CERT-PRIME
03614                                     W-CHEK-CERT-PRIME
03615                                     W-MAIL-CERT-PRIME
03616                                     W-PNDB2-CERT-PRIME
03617                                     PI-689-CERT-SFX
03618                                     W-CERT-CERT-SFX
03619                                     W-CHEK-CERT-SFX
03620                                     W-MAIL-CERT-SFX
03621                                     W-PNDB2-CERT-SFX
03622      ELSE
03623          MOVE AL-UANON           TO CERTA
03624          MOVE CERTI              TO PI-689-CERT-PRIME
03625                                     W-CERT-CERT-PRIME
03626                                     W-CHEK-CERT-PRIME
03627                                     W-MAIL-CERT-PRIME
03628                                     W-PNDB2-CERT-PRIME
03629          IF  SFXL EQUAL ZEROES
03630                  OR
03631              SFXI EQUAL LOW-VALUES
03632              MOVE SPACES         TO PI-689-CERT-SFX
03633                                     W-CERT-CERT-SFX
03634                                     W-CHEK-CERT-SFX
03635                                     W-MAIL-CERT-SFX
03636                                     W-PNDB2-CERT-SFX
03637          ELSE
03638              MOVE AL-UANON       TO SFXA
03639              MOVE SFXI           TO PI-689-CERT-SFX
03640                                     W-CERT-CERT-SFX
03641                                     W-CHEK-CERT-SFX
03642                                     W-MAIL-CERT-SFX
03643                                     W-PNDB2-CERT-SFX.
03644
03645  0930-EXIT.
03646      EXIT.
03647                                  EJECT
03648  0935-EDIT-RESP-PERSON.
03649
03650      IF  RPERSONL EQUAL ZEROES
03651          MOVE LOW-VALUES         TO PI-689-RESP-PERSON
03652                                     W-COMP-RESP-PERSON
03653                                     W-PYAJ-FIN-RESP
03654      ELSE
03655          MOVE AL-UANON           TO RPERSONA
03656          MOVE RPERSONI           TO PI-689-RESP-PERSON
03657                                     W-PYAJ-FIN-RESP
03658                                     W-COMP-RESP-PERSON.
03659
03660  0935-EXIT.
03661      EXIT.
03662                                  EJECT
03663  0940-EDIT-ACCOUNT.
03664
03665      IF  ACCTL EQUAL ZEROES
03666          MOVE LOW-VALUES         TO PI-689-ACCOUNT
03667                                     W-COMP-ACCOUNT
03668                                     W-PYAJ-ACCOUNT
03669      ELSE
03670          MOVE AL-UANON           TO ACCTA
03671          MOVE ACCTI              TO PI-689-ACCOUNT
03672                                     W-PYAJ-ACCOUNT
03673                                     W-COMP-ACCOUNT.
03674
03675  0940-EXIT.
03676      EXIT.
03677                                  EJECT
03678  0945-EDIT-TYPE.
03679
03680      IF  TYPEL EQUAL ZEROES
03681          MOVE LOW-VALUES         TO PI-689-TYPE
03682                                     W-COMP-TYPE
03683                                     W-PYAJ-RECORD-TYPE
03684      ELSE
03685          INSPECT TYPEI CONVERTING W-LOWER-CASE TO W-UPPER-CASE
03686          IF TYPEI = 'C' OR 'G' OR 'A'
03687              MOVE AL-UANON       TO TYPEA
03688              MOVE TYPEI          TO PI-689-TYPE
03689                                     W-COMP-TYPE
03690                                     W-PYAJ-RECORD-TYPE
03691          ELSE
03692              MOVE ER-7368        TO EMI-ERROR
03693              MOVE -1             TO TYPEL
03694              MOVE AL-UABON       TO TYPEA
03695              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03696
03697  0945-EXIT.
03698      EXIT.
03699                                  EJECT
03700  0950-EDIT-ENTRY.
03701
03702      IF  BENTRYL EQUAL ZEROES
03703          MOVE LOW-VALUES         TO PI-689-ENTRY-BATCH
03704                                     W-PNDB-ENTRY
03705      ELSE
03706          MOVE AL-UANON           TO BENTRYA
03707          MOVE BENTRYI            TO PI-689-ENTRY-BATCH
03708                                     W-PNDB-ENTRY.
03709
03710  0950-EXIT.
03711      EXIT.
03712                                  EJECT
03713  0955-EDIT-SEQUENCE.
03714
03715      IF  SEQL NOT EQUAL ZEROS
03716          IF  SEQI NOT NUMERIC
03717                  OR
03718              SEQI EQUAL '0'
03719              MOVE ER-7367        TO EMI-ERROR
03720              MOVE -1             TO SEQL
03721              MOVE AL-UABON       TO SEQA
03722              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03723          ELSE
03724              MOVE AL-UANON       TO SEQA
03725              MOVE SEQI           TO PI-689-SEQ-NO
03726                                     W-CHEK-SEQ-NO
03727                                     W-PYAJ-FILE-SEQ-NO
03728                                     W-PNDB-SEQ-NO
03729                                     PI-689-SEQ-EDIT
03730      ELSE
03731          MOVE LOW-VALUES         TO PI-689-SEQ-EDIT.
03732
03733  0955-EXIT.
03734      EXIT.
03735                                  EJECT
03736  0960-EDIT-BATCH-SEQ.
03737
03738      IF  BCSEQL NOT EQUAL ZEROS
03739          IF  BCSEQI NOT NUMERIC
03740              MOVE ER-7368        TO EMI-ERROR
03741              MOVE -1             TO BCSEQL
03742              MOVE AL-UABON       TO BCSEQA
03743              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03744          ELSE
03745              MOVE AL-UANON       TO BCSEQA
03746              MOVE BCSEQI         TO PI-689-CHG-SEQ-NO
03747                                     W-PNDB-CHG-SEQ-NO
03748                                     W-PNDB2-ALT-CHG-SEQ-NO
03749                                     PI-689-BCSEQ-EDIT
03750      ELSE
03751          MOVE LOW-VALUES         TO PI-689-BCSEQ-EDIT.
03752
03753  0960-EXIT.
03754      EXIT.
03755                                  EJECT
03756  0965-EDIT-TYPE.
03757
03758      IF  TYPEL EQUAL ZEROES
03759          MOVE LOW-VALUES         TO PI-689-TYPE
03760                                     W-PNDB2-TYPE
03761      ELSE
03762          IF  DATASORI NOT EQUAL '4'
03763              GO TO 0965-EXIT
03764          ELSE
03765              IF TYPEI = '0' OR '1' OR '2' OR '3'
03766                  MOVE AL-UANON   TO TYPEA
03767                  MOVE TYPEI      TO PI-689-TYPE
03768                                     W-PNDB2-TYPE
03769              ELSE
03770                  MOVE ER-7370    TO EMI-ERROR
03771                  MOVE -1         TO TYPEL
03772                  MOVE AL-UABON   TO TYPEA
03773                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03774
03775  0965-EXIT.
03776      EXIT.
011013
011013 0970-EDIT-CSO.
011013
011013     IF ENDARCHL > ZEROS
011013        IF ENDARCHI NUMERIC
011013            MOVE ENDARCHI       TO PI-ENDT-ARCH-NO
011013            MOVE AL-UNNON       TO ENDARCHA
011013            MOVE ENDARCHI       TO W-ERENDT-ARCHIVE
011013            MOVE PI-COMPANY-CD  TO W-ERENDT-COMPANY-CD-A1
011013            MOVE 'N'            TO W-VALID-ENDT-SW
011013            PERFORM 0355-CHECK-ERENDT THRU 0355-EXIT
011013            IF NOT W-VALID-ENDT
011013                MOVE -1         TO ENDARCHL
011013                MOVE ER-1565    TO EMI-ERROR
011013                MOVE AL-UNBON   TO ENDARCHA
011013                PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011013            END-IF
011013        END-IF
011013     ELSE
011013        MOVE ZEROS              TO PI-ENDT-ARCH-NO
011013     END-IF
011013
011013     IF PI-CERT-REQ-IND = 'Y'
011013       IF  CERTIDL NOT EQUAL ZEROS
011013         IF CERTIDI NUMERIC
011013             MOVE AL-UANON      TO CERTIDA
011013             MOVE CERTIDI       TO PI-CERT-FORM-ID
011013         ELSE
011013             MOVE ER-1778       TO EMI-ERROR
011013             MOVE -1            TO CERTIDL
011013             MOVE AL-UABON      TO CERTIDA
011013             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011013         END-IF
011013       ELSE
011013          MOVE ER-0715       TO EMI-ERROR
011013          MOVE -1            TO CERTIDL
011013          MOVE AL-UABON      TO CERTIDA
011013          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011013       END-IF
011013     END-IF.
011013
011013 0970-EXIT.
011013      EXIT.
03777                                  EJECT
03778  0990-CARRIER-VALIDITY-CHECK.
03779
03780      IF  CARRIERI EQUAL SPACES
03781          GO TO 0990-EXIT.
03782
03783      MOVE SPACES                 TO W-CNTL-KEY.
03784      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
03785      MOVE '6'                    TO W-CNTL-RECORD-TYPE.
03786      MOVE CARRIERI               TO W-CNTL-GEN4.
03787      MOVE +0                     TO W-CNTL-SEQ-NO.
03788
03789      
      * EXEC CICS HANDLE CONDITION
03790 *        NOTFND  (0990-CARRIER-NOT-FOUND)
03791 *        NOTOPEN (8040-CNTL-NOT-OPEN)
03792 *    END-EXEC.
      *    MOVE '"$IJ                  ! & #00010391' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303130333931' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03793
03794      
      * EXEC CICS READ
03795 *        DATASET (W-CNTL-FILE-ID)
03796 *        SET     (ADDRESS OF CONTROL-FILE)
03797 *        RIDFLD  (W-CNTL-KEY)
03798 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010396' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130333936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03799
03800      IF  PI-NO-CARRIER-SECURITY
03801              OR
03802          PI-CARRIER-SECURITY EQUAL CARRIERI
03803          MOVE AL-UANON           TO CARRIERA
03804          MOVE CARRIERI           TO PI-689-CARRIER
03805                                     W-ACCT-CARRIER
03806                                     W-CERT-CARRIER
03807                                     W-CHEK-CARRIER
03808                                     W-COMP-CARRIER
03809                                     W-MAIL-CARRIER
03810                                     W-PNDB2-CARRIER
03811                                     W-PYAJ-CARRIER
03812      ELSE
03813          PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT
03814          MOVE ER-9095            TO EMI-ERROR
03815          MOVE -1                 TO MAINTL
03816          MOVE AL-UANON           TO MAINTA
03817          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03818
03819      GO TO 0990-EXIT.
03820
03821  0990-CARRIER-NOT-FOUND.
03822
03823      MOVE -1                     TO CARRIERL.
03824      MOVE AL-UABON               TO CARRIERA.
03825      MOVE ER-2208                TO EMI-ERROR.
03826      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03827
03828  0990-EXIT.
03829      EXIT.
03830                                  EJECT
03831  0994-STATE-VALIDITY-CHECK.
03832
03833      IF  STATEI EQUAL SPACES
03834          GO TO 0994-EXIT.
03835
03836      MOVE SPACES                 TO W-CNTL-KEY.
03837      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
03838      MOVE '3'                    TO W-CNTL-RECORD-TYPE.
03839      MOVE STATEI                 TO W-CNTL-GEN1.
03840      MOVE +0                     TO W-CNTL-SEQ-NO.
03841
03842      
      * EXEC CICS HANDLE CONDITION
03843 *        NOTFND  (0994-STATE-NOT-FOUND)
03844 *        NOTOPEN (8040-CNTL-NOT-OPEN)
03845 *    END-EXEC.
      *    MOVE '"$IJ                  ! '' #00010444' TO DFHEIV0
           MOVE X'2224494A2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2720233030303130343434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03846
03847      
      * EXEC CICS READ
03848 *        DATASET (W-CNTL-FILE-ID)
03849 *        SET     (ADDRESS OF CONTROL-FILE)
03850 *        RIDFLD  (W-CNTL-KEY)
03851 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010449' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130343439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03852
03853      MOVE AL-UANON               TO STATEA.
03854      MOVE STATEI                 TO PI-689-STATE
03855                                     W-ACCT-STATE
03856                                     W-CERT-STATE
03857                                     W-CHEK-STATE
03858                                     W-MAIL-STATE
03859                                     W-PNDB2-STATE.
03860
03861      GO TO 0994-EXIT.
03862
03863  0994-STATE-NOT-FOUND.
03864
03865      MOVE -1                     TO STATEL.
03866      MOVE AL-UABON               TO STATEA.
03867      MOVE ER-2209                TO EMI-ERROR.
03868      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
03869
03870  0994-EXIT.
03871      EXIT.
03872                                  EJECT
03873  1000-SHOW.
03874 ***************************************************************
03875 *     THIS ROUTINE WILL BROWSE THE ARCHIVE FILE WITH THE      *
03876 *     ARCHIVE NUMBER SPECIFIED FROM THE SCREEN.  IF THE       *
03877 *     HEADER RECORD IS FOUND THE TEXT IS THEN READ AND        *
03878 *     WILL BE INSERTED INTO THE W-TS-TABLE AND DISPLAYED.     *
03879 ***************************************************************
03880
03881      MOVE SPACES                 TO W-RECORD-TABLE.
03882
03883      IF  PI-689-LBL-OVERRIDE GREATER THAN SPACES
03884           MOVE PI-689-LBL-OVERRIDE
03885                                  TO ADDRLBLI
03886           MOVE +1                TO ADDRLBLL
03887           MOVE AL-UANON          TO ADDRLBLA
03888      ELSE
03889           MOVE PI-LABEL-CONTROL  TO ADDRLBLI
03890           MOVE +1                TO ADDRLBLL
03891           MOVE AL-UANON          TO ADDRLBLA.
03892
03893      IF  ARCHNUML EQUAL ZEROS
03894          MOVE -1                 TO ARCHNUML
03895          MOVE AL-UNBON           TO ARCHNUMA
03896          MOVE ER-0174            TO EMI-ERROR
03897          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03898          GO TO 8200-SEND-DATAONLY.
03899
03900      IF  ARCHNUMI NOT NUMERIC
03901          MOVE -1                 TO ARCHNUML
03902          MOVE AL-UNBON           TO ARCHNUMA
03903          MOVE ER-0175            TO EMI-ERROR
03904          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
03905          GO TO 8200-SEND-DATAONLY.
03906
03907      IF  PI-SHOW-MODE
03908
03909          IF  PI-689-ARCHIVE-NUMBER EQUAL ARCHNUMI
03910              MOVE -1             TO MAINTL
03911              GO TO 8200-SEND-DATAONLY
03912          ELSE
03913              PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT.
03914
03915      MOVE +0                     TO W-ARCH-SEQ-NO
03916                                     W-ARCT-SEQ-NO.
03917      MOVE PI-COMPANY-CD          TO W-ARCH-COMPANY-CD
03918                                     W-ARCT-COMPANY-CD.
03919      MOVE ARCHNUMI               TO W-ARCH-NUMBER
03920                                     W-ARCT-NUMBER
03921                                     PI-689-ARCHIVE-NUMBER.
03922      MOVE ' '                    TO W-ARCT-REC-TYPE.
03923      MOVE '1'                    TO PI-ACTION.
03924      MOVE SPACES                 TO PI-689-PRINT-SW
03925                                     PI-689-FORM-NUMBER
03926                                     W-DELETE-KEY.
03927      SET W-RG-NDX                TO W-ZEROS.
03928
03929      
      * EXEC CICS HANDLE CONDITION
03930 *         NOTOPEN    (8010-ARCH-NOT-OPEN)
03931 *         NOTFND     (1070-ARCH-NOT-FOUND)
03932 *         ENDFILE    (1070-ARCH-NOT-FOUND)
03933 *    END-EXEC.
      *    MOVE '"$JI''                 ! ( #00010531' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2820233030303130353331' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03934
03935      
      * EXEC CICS READ
03936 *        DATASET (W-ARCH-FILE-ID)
03937 *        SET     (ADDRESS OF LETTER-ARCHIVE)
03938 *        RIDFLD  (W-ARCH-KEY)
03939 *    END-EXEC.
      *    MOVE '&"S        E          (   #00010537' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130353337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
03940
03941      PERFORM 1040-FORMAT-HEADER-DATA THRU 1040-EXIT.
03942
03943 *    EXEC CICS HANDLE CONDITION
03944 *         NOTOPEN    (8015-ARCT-NOT-OPEN)
03945 *         NOTFND     (1020-ARCT-ENDBR)
03946 *         ENDFILE    (1020-ARCT-ENDBR)
03947 *    END-EXEC.
03948 *
03949 *1005-START-BROWSE.
03950 *
03951 *    EXEC CICS STARTBR
03952 *         DATASET    (W-ARCT-FILE-ID)
03953 *         RIDFLD     (W-ARCT-KEY)
03954 *         GTEQ
03955 *    END-EXEC.
03956 *
03957 *    MOVE 'Y'                    TO W-ARCT-BROWSE-STARTED.
03958 *
03959 *1010-READ-NEXT.
03960 *
03961 *    EXEC CICS READNEXT
03962 *         SET       (ADDRESS OF LETTER-ARCHIVE-TEXT)
03963 *         DATASET   (W-ARCT-FILE-ID)
03964 *         RIDFLD    (W-ARCT-KEY)
03965 *    END-EXEC.
03966 *
03967 *    IF  W-ARCH-PARTIAL-KEY EQUAL W-ARCT-PARTIAL-KEY
03968 *        IF  LT-ADDRESS-DATA
03969 *            IF  PI-CREATE-LABELS  AND
03970 *                NOT PI-689-LABELS-OVERRIDEN
03971 *                PERFORM 1050-MOVE-LABELS THRU 1050-EXIT
03972 *                        VARYING
03973 *                    LT-NDX FROM 1 BY 1
03974 *                        UNTIL
03975 *                    LT-NDX GREATER THAN PI-689-NUMBER-LABEL-LINES
03976 *                GO TO 1010-READ-NEXT
03977 *            ELSE
03978 *                MOVE LT-CONTROL-PRIMARY
03979 *                                TO W-DELETE-KEY
03980 *                GO TO 1010-READ-NEXT
03981 *        ELSE
03982 *            PERFORM 1030-TEXT-BUILD THRU 1030-EXIT
03983 *                    VARYING
03984 *                LT-NDX FROM 1 BY 1
03985 *                    UNTIL
03986 *                LT-NDX GREATER THAN LT-NUM-LINES-ON-RECORD
03987 *                    OR
03988 *                LT-NDX GREATER THAN +20
03989 *            GO TO 1010-READ-NEXT.
03990 *
03991 *1020-ARCT-ENDBR.
03992 *
03993 *    IF  W-ARCT-BROWSE-STARTED EQUAL 'Y'
03994 *        EXEC CICS ENDBR
03995 *             DATASET   (W-ARCT-FILE-ID)
03996 *        END-EXEC.
03997 *
03998 *    IF  W-RG-NDX EQUAL ZEROS
03999 *        GO TO 1080-ARCHIVE-TEXT-NOT-FOUND.
04000 *
04001 *    IF  PI-689-NUMBER-LABEL-LINES GREATER THAN +0
04002 *            AND
04003 *        PI-BYPASS-LABELS
04004 *        MOVE W-DELETE-KEY       TO W-ARCT-KEY
04005 *        PERFORM 1060-DELETE-ADDRESS-RECORD THRU 1060-EXIT
04006 *        MOVE +0                 TO PI-689-NUMBER-LABEL-LINES.
04007 *
04008 *    SET PI-TOTAL-LINES          TO W-RG-NDX.
04009 *    MOVE 1                      TO PI-CURRENT-LINE.
04010 *    SET W-RG-NDX                TO +1.
04011 *
04012 *    PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
04013 *            VARYING
04014 *        W-SC-NDX FROM 1 BY 1
04015 *            UNTIL
04016 *        W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
04017
04018      MOVE -1                     TO MAINTL.
04019      GO TO 8100-SEND-INITIAL-MAP.
04020
04021  1020-EXIT.
04022      EXIT.
04023
04024  1030-TEXT-BUILD.
04025
04026      SET W-RG-NDX UP BY 1.
04027      MOVE LT-SKIP-CONTROL (LT-NDX)
04028                                  TO W-PRINT-CONTROL.
04029
04030      IF  W-PRINT-CONTROL EQUAL 99
04031          MOVE W-TOP-FORM         TO W-RC-TEXT (W-RG-NDX)
04032          SET W-RG-NDX UP BY 1
04033      ELSE
04034          SET W-RG-NDX UP BY W-PRINT-CONTROL.
04035
04036      MOVE LT-TEXT-LINE (LT-NDX)  TO W-RC-TEXT (W-RG-NDX).
04037      MOVE LT-SKIP-CONTROL (LT-NDX)
04038                                  TO W-RC-PC (W-RG-NDX).
04039      MOVE SPACES                 TO W-RC-SC (W-RG-NDX).
04040
04041  1030-EXIT.
04042      EXIT.
04043
04044  1040-FORMAT-HEADER-DATA.
04045
04046      MOVE LA-ACCOUNT-A2          TO ACCTO.
04047      MOVE LA-ADDR-SOURCE         TO ADDRSO.
04048      MOVE LA-CARRIER-A2          TO CARRIERO.
04049      MOVE LA-CERT-NO-A2          TO CERTO.
04050      MOVE LA-CERT-SUFFIX-A2      TO SFXO.
04051      MOVE LA-DATA-SOURCE         TO DATASORO.
04052
04053      IF  LA-EFFECT-DATE-A2 EQUAL LOW-VALUES
04054          MOVE ZEROES             TO DATEO
04055
04056      ELSE
04057          MOVE LA-EFFECT-DATE-A2  TO DC-BIN-DATE-1
04058          MOVE ' '                TO DC-OPTION-CODE
04059          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
04060          MOVE DC-GREG-DATE-1-EDIT
04061                                  TO DATEO.
04062
04063      IF  LA-FOLLOW-UP-DATE EQUAL LOW-VALUES
04064          MOVE ZEROES             TO FOLLOWO
04065      ELSE
04066          MOVE LA-FOLLOW-UP-DATE  TO DC-BIN-DATE-1
04067          MOVE ' '                TO DC-OPTION-CODE
04068          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
04069          MOVE DC-GREG-DATE-1-EDIT
04070                                  TO FOLLOWO.
04071
04072      MOVE LA-FORM-A3             TO FORMO.
04073      MOVE LA-GROUPING-A2         TO GROUPO.
04074      MOVE LA-NO-OF-COPIES        TO COPIESO
04075                                     PI-689-NUMBER-COPIES.
04076
04077 *    IF LA-PRINT-RESTRICTION = 'C' OR 'F' OR 'P'
04078 *        MOVE LA-PRINT-RESTRICTION
04079 *                                TO PRTRESTO
04080 *    ELSE
04081 *        MOVE SPACES             TO PRTRESTO.
04082
04083      IF  LA-RESEND-DATE = LOW-VALUES
04084          MOVE ZEROES             TO RESEND1O
04085      ELSE
04086          MOVE LA-RESEND-DATE     TO DC-BIN-DATE-1
04087          MOVE ' '                TO DC-OPTION-CODE
04088          PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
04089          MOVE DC-GREG-DATE-1-EDIT
04090                                  TO RESEND1O.
04109
04110      MOVE LA-STATE-A2            TO STATEO.
04111
04112      MOVE LA-NUMBER-LABEL-LINES  TO PI-689-NUMBER-LABEL-LINES.
04113
04114  1040-EXIT.
04115      EXIT.
04116                                  EJECT
04117  1050-MOVE-LABELS.
04118
04119      SET W-RG-NDX                TO LT-NDX.
04120      MOVE ZEROS                  TO W-RC-PC (W-RG-NDX).
04121      MOVE SPACES                 TO W-RC-SC (W-RG-NDX).
04122      MOVE LT-TEXT-LINE (LT-NDX)  TO W-RC-TEXT (W-RG-NDX).
04123
04124  1050-EXIT.
04125      EXIT.
04126                                  EJECT
04127  1060-DELETE-ADDRESS-RECORD.
04128
04129      
      * EXEC CICS DELETE
04130 *        DATASET (W-ARCT-FILE-ID)
04131 *        RIDFLD  (W-DELETE-KEY)
04132 *    END-EXEC.
      *    MOVE '&(  R                 &   #00010713' TO DFHEIV0
           MOVE X'262820205220202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 W-DELETE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04133
04134      
      * EXEC CICS READ
04135 *        DATASET (W-ARCH-FILE-ID)
04136 *        SET     (ADDRESS OF LETTER-ARCHIVE)
04137 *        RIDFLD  (W-ARCH-KEY)
04138 *        UPDATE
04139 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00010718' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04140
04141      MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES.
04142
04143      
      * EXEC CICS REWRITE
04144 *         FROM      (LETTER-ARCHIVE)
04145 *         DATASET   (W-ARCH-FILE-ID)
04146 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00010727' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303130373237' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04147
04148  1060-EXIT.
04149      EXIT.
04150                                  EJECT
04151  1070-ARCH-NOT-FOUND.
04152
04153      MOVE ER-7371                TO EMI-ERROR.
04154      MOVE -1                     TO ARCHNUML.
04155      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04156      GO TO 8100-SEND-INITIAL-MAP.
04157
04158  1080-ARCHIVE-TEXT-NOT-FOUND.
04159
04160      MOVE ER-7372                TO EMI-ERROR.
04161      MOVE -1                     TO ARCHNUML.
04162      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04163      GO TO 8200-SEND-DATAONLY.
04164                                  EJECT
04165  2000-CREATE.
04166 ***************************************************************
04167 *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *
04168 *    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *
04169 *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *
04170 *    WILL BE DISPLAYED ON THE SCREEN.                         *
04171 *                                                             *
04172 ***************************************************************
04173
04174 ***************************************************************
04175 *    CHECK TO SEE IF IT IS THE SAME REQUEST OR NOT.           *
04176 *    IF IT IS A NEW REQUEST AND A LETTER WAS PRINTED          *
04177 *    THEN FORCE AN ERROR.                                     *
04178 ***************************************************************
04179
04180      IF  PI-689-CREATE-NO-SCREENS
04181          GO TO 2000-GET-TEXT.
04182
04183      IF  PI-CREATE-MODE
04184          IF  PI-689-PRINT-PERFORMED
04185                  AND
04186              FORMI NOT EQUAL 9999
04187              MOVE ER-0279    TO EMI-ERROR
04188              MOVE -1         TO MAINTL
04189              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04190              GO TO 8200-SEND-DATAONLY.
04191
04192      IF  ARCHNUML GREATER THAN ZEROS
04193          MOVE ER-9320            TO EMI-ERROR
04194          MOVE -1                 TO ARCHNUML
04195          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04196          GO TO 8200-SEND-DATAONLY.
04197
04198 ***************************************************************
04199 *    IF  A NEW LETTER IS BEING CREATED FROM SCRATCH, SAVE     *
04200 *    THE EXISTING SCREEN AND PASS CONTROL TO THE TEXT EDITOR  *
04201 ***************************************************************
04202
04203      IF  FORMI EQUAL '9999'
04204          MOVE 16                 TO PI-TOTAL-LINES
04205          MOVE 1                  TO PI-CURRENT-LINE
04206          MOVE SPACES             TO W-FILE-TABLE
04207                                     W-LINE-CONTROL-RECORD
04208                                     W-RECORD-TABLE
04209                                     W-SINGLE-LINE
04210                                     W-SQUEEZED-LINE
04211                                     W-TS-WORK-AREA
04212                                     W-TX-TABLE
04213          PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT
04214          MOVE '3'                TO PI-ACTION
04215          MOVE FORMI              TO PI-689-FORM-NUMBER
04216                                     PI-COMM-CONTROL
04217          MOVE ZEROS              TO PI-UPDATE-SW
04218          MOVE W-PGM-EL1042       TO W-CALL-PGM
04219          GO TO 9400-XCTL.
04220
04221  2000-GET-TEXT.
04222
04223      MOVE SPACES                 TO W-FILE-TABLE
04224                                     W-LINE-CONTROL-RECORD
04225                                     W-RECORD-TABLE
04226                                     W-SINGLE-LINE
04227                                     W-SQUEEZED-LINE
04228                                     W-TS-WORK-AREA
04229                                     W-TX-TABLE.
04230      SET W-TG-NDX                TO +1
04231      MOVE ZEROS                  TO W-INITIAL-COLUMN
04232                                     W-LINE-COUNT
04233                                     W-LINE-INDENT-1
04234                                     W-LINE-INDENT-2
04235                                     W-LINE-INDENT-3
04236                                     W-LINE-INDENT-4
04237                                     W-LINE-INDENT-5
04238                                     W-PAGE
04239                                     W-PARAGRAPH-INDENT
04240                                     W-TOP-MARGIN
04241                                     W-WORK-INDENT.
04242      MOVE PI-LOWER-CASE-LETTERS  TO W-LC-CASE-IND.
04243      MOVE +70                    TO W-LAST-COLUMN.
04244      MOVE +56                    TO W-MAX-LINES-PER-PAGE.
04245      MOVE +1                     TO W-START-COLUMN.
04246      MOVE +71                    TO W-TOO-FAR.
04247
04248      MOVE PI-COMPANY-CD          TO W-TEXT-COMPANY-CD.
04249      MOVE FORMI                  TO W-TEXT-LETTER.
04250      MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.
04251      MOVE 'N'                    TO W-TEXT-BROWSED-SW.
04252
04253      
      * EXEC CICS HANDLE CONDITION
04254 *         NOTFND     (2001-NOT-FOUND)
04255 *         ENDFILE    (2001-NOT-FOUND)
04256 *         NOTOPEN    (8050-TEXT-NOT-OPEN)
04257 *    END-EXEC.
      *    MOVE '"$I''J                 ! ) #00010837' TO DFHEIV0
           MOVE X'222449274A20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2920233030303130383337' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04258
04259      
      * EXEC CICS STARTBR
04260 *         DATASET    (W-TEXT-FILE-ID)
04261 *         RIDFLD     (W-TEXT-KEY)
04262 *         GTEQ
04263 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00010843' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 W-TEXT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04264
04265      
      * EXEC CICS HANDLE CONDITION
04266 *         ENDFILE    (2000-ENDBR)
04267 *    END-EXEC.
      *    MOVE '"$''                   ! * #00010849' TO DFHEIV0
           MOVE X'222427202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2A20233030303130383439' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04268
04269  2000-READ-NEXT.
04270
04271      IF  W-TG-NDX GREATER THAN W-MAX-LINES
04272          MOVE ER-0051            TO EMI-ERROR
04273          MOVE -1                 TO MAINTL
04274          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04275          GO TO 2000-ENDBR.
04276
04277      
      * EXEC CICS READNEXT
04278 *         DATASET    (W-TEXT-FILE-ID)
04279 *         SET        (ADDRESS OF TEXT-FILES)
04280 *         RIDFLD     (W-TEXT-KEY)
04281 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00010861' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
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
           
04282
04283      IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY
04284          GO TO 2000-ENDBR.
04285
04286      MOVE 'Y'                    TO W-TEXT-BROWSED-SW.
04287
04288      MOVE TX-FORM-SQUEEZE-CONTROL
04289                                  TO W-FORM-SQUEEZE-IND.
04290
04291      IF  PI-689-ARCHIVE-SW NOT GREATER THAN SPACES
04292          MOVE TX-ARCHIVE-SW      TO PI-689-ARCHIVE-SW.
04293
04294      IF  TX-FORM-CONTROL-LINE
04295          PERFORM 2100-PROCESS-FORM-CONTROL-LINE THRU 2100-EXIT
04296          GO TO 2000-READ-NEXT.
04297
04298      IF  TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'
04299          PERFORM 2150-PROCESS-Z-CONTROLS THRU 2150-EXIT
04300          GO TO 2000-READ-NEXT.
04301
04302      MOVE TX-TEXT-LINE           TO W-TX-TEXT (W-TG-NDX).
04303      MOVE TX-PROCESS-CONTROL     TO W-TX-PC (W-TG-NDX).
04304      PERFORM 2200-CHECK-FOR-VARIABLE THRU 2200-EXIT.
04305      MOVE TX-LINE-SQUEEZE-CONTROL
04306                                  TO W-TX-SC (W-TG-NDX).
04307
04308      SET W-TG-NDX UP BY 1.
04309      GO TO 2000-READ-NEXT.
04310
04311  2000-ENDBR.
04312
04313      
      * EXEC CICS ENDBR
04314 *        DATASET     (W-TEXT-FILE-ID)
04315 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00010897' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303130383937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TEXT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
04316
04317      IF  W-TEXT-BROWSE-NOT-STARTED
04318          MOVE ER-0006            TO EMI-ERROR
04319          MOVE -1                 TO FORML
04320          MOVE AL-UABON           TO FORMA
04321          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04322          GO TO 8350-SEND-DATAONLY-ERASEAUP.
04323
04324      SET  W-TOTAL-TX-LINES       TO W-TG-NDX.
04325      SUBTRACT +1 FROM W-TOTAL-TX-LINES.
04326      MOVE 1                      TO PI-CURRENT-LINE.
04327
04328      IF  COPIESL NOT GREATER THAN ZEROS
04329          MOVE +1                 TO COPIESI
04330                                     PI-689-NUMBER-COPIES
04331          MOVE +1                 TO COPIESL
04332          MOVE AL-UNNON           TO COPIESA.
04333
04334 ***************************************************************
04335 *    IF IT IS A FORM WITH VARIABLES, THEN RESOLVE ALL OF      *
04336 *    VARIABLE SYMBOLS AND INSERT THEM INTO THE TEXT DATA.     *
04337 ***************************************************************
04338
04339      PERFORM 6000-RESOLVE-VARIABLES THRU 6000-EXIT.
04340
04341 *    IF  PI-689-PRINT-RESTRICTION EQUAL 'C'
04342 *        IF  PI-689-CONTROL NOT GREATER THAN ZEROS
04343 *            MOVE ER-7243        TO EMI-ERROR
04344 *            MOVE -1             TO PRTRESTL
04345 *            MOVE AL-UABON       TO PRTRESTA
04346 *            PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04347
04348      IF  PI-CREATE-LABELS AND
04349          NOT  PI-689-LABELS-OVERRIDEN
04350          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
04351          SET W-RG-NDX UP BY +1
04352      ELSE
04353          SET W-RG-NDX            TO +1.
04354
04355      MOVE W-TOP-FORM             TO W-RC-TEXT (W-RG-NDX).
04356      SET W-RG-NDX UP BY W-TOP-MARGIN.
04357
04358      PERFORM 7400-CREATE-LETTER THRU 7400-EXIT.
04359
04360      SET PI-TOTAL-LINES          TO W-RG-NDX.
04361
04362      IF  W-FIRST-BAD-VARIABLE-FOUND
04363          SET W-RG-NDX            TO W-FIRST-BAD-VARIABLE
04364          MOVE W-FIRST-BAD-VARIABLE
04365                                  TO PI-CURRENT-LINE
04366      ELSE
04367          SET W-RG-NDX            TO 1.
04368
04369      PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
04370              VARYING
04371          W-SC-NDX FROM 1 BY 1
04372              UNTIL
04373          W-SC-NDX GREATER W-NUM-LINES-PER-SCREEN.
04374      IF  PI-689-CREATE-NO-SCREENS
04375          GO TO 2000-EXIT
04376      ELSE
04377          IF  W-FORM-CHANGED
04378              MOVE '3'            TO PI-ACTION
04379              MOVE -1             TO MAINTL
04380              GO TO 8100-SEND-INITIAL-MAP
04381          ELSE
04382              MOVE '3'            TO PI-ACTION
04383              MOVE -1             TO MAINTL
04384              GO TO 8350-SEND-DATAONLY-ERASEAUP.
04385
04386  2000-EXIT.
04387      EXIT.
04388
04389  2001-NOT-FOUND.
04390
04391      MOVE ER-0006                TO EMI-ERROR.
04392      MOVE -1                     TO FORML.
04393      MOVE AL-UABON               TO FORMA.
04394      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
04395      GO TO 8350-SEND-DATAONLY-ERASEAUP.
04396                                  EJECT
04397  2100-PROCESS-FORM-CONTROL-LINE.
04398
04399      MOVE TX-TEXT-LINE           TO W-LINE-CONTROL-RECORD.
04400
04401      IF  W-LC-LINE-WIDTH NUMERIC
04402              AND
04403          W-LC-LINE-WIDTH GREATER THAN ZEROS
04404          COMPUTE W-INITIAL-COLUMN
04405              = (70 - W-LC-LINE-WIDTH) / 2
04406          COMPUTE W-START-COLUMN = W-INITIAL-COLUMN + 1
04407          COMPUTE W-LAST-COLUMN ROUNDED
04408              = W-INITIAL-COLUMN + W-LC-LINE-WIDTH
04409          COMPUTE W-TOO-FAR = W-LAST-COLUMN + 1
04410      ELSE
04411          MOVE 70                 TO W-LC-LINE-WIDTH.
04412
04413      IF  W-LC-PARAGRAPH-INDENT NUMERIC
04414              AND
04415          W-LC-PARAGRAPH-INDENT GREATER THAN ZEROS
04416          MOVE W-LC-PARAGRAPH-INDENT
04417                                  TO W-PARAGRAPH-INDENT.
04418
04419      IF  W-LC-MAX-LINES-PER-PAGE NUMERIC
04420              AND
04421          W-LC-MAX-LINES-PER-PAGE GREATER THAN ZEROS
04422          MOVE W-LC-MAX-LINES-PER-PAGE
04423                                  TO W-MAX-LINES-PER-PAGE.
04424
04425      IF  W-LC-LINE-ADJUST NUMERIC
04426              AND
04427          W-LC-LINE-ADJUST GREATER THAN ZEROS
04428          COMPUTE W-TOTAL-LINE-LENGTH
04429              = W-LC-LINE-WIDTH + (W-LC-LINE-ADJUST * 2)
04430          IF  W-TOTAL-LINE-LENGTH LESS THAN +71
04431              ADD W-LC-LINE-ADJUST
04432                                  TO W-INITIAL-COLUMN
04433                                     W-LAST-COLUMN
04434                                     W-START-COLUMN
04435                                     W-TOO-FAR.
04436
04437      IF  W-LC-LINE-INDENT-1 NUMERIC
04438              AND
04439          W-LC-LINE-INDENT-1 LESS THAN W-LC-LINE-WIDTH
04440          MOVE W-LC-LINE-INDENT-1 TO W-LINE-INDENT-1
04441      ELSE
04442          MOVE +0                 TO W-LINE-INDENT-1.
04443
04444      IF  W-LC-LINE-INDENT-2 NUMERIC
04445              AND
04446          W-LC-LINE-INDENT-2 LESS THAN W-LC-LINE-WIDTH
04447          MOVE W-LC-LINE-INDENT-2 TO W-LINE-INDENT-2
04448      ELSE
04449          MOVE +0                 TO W-LINE-INDENT-2.
04450
04451      IF  W-LC-LINE-INDENT-3 NUMERIC
04452              AND
04453          W-LC-LINE-INDENT-3 LESS THAN W-LC-LINE-WIDTH
04454          MOVE W-LC-LINE-INDENT-3 TO W-LINE-INDENT-3
04455      ELSE
04456          MOVE +0                 TO W-LINE-INDENT-3.
04457
04458      IF  W-LC-LINE-INDENT-4 NUMERIC
04459              AND
04460          W-LC-LINE-INDENT-4 LESS THAN W-LC-LINE-WIDTH
04461          MOVE W-LC-LINE-INDENT-4 TO W-LINE-INDENT-4
04462      ELSE
04463          MOVE +0                 TO W-LINE-INDENT-4.
04464
04465      IF  W-LC-LINE-INDENT-5 NUMERIC
04466              AND
04467          W-LC-LINE-INDENT-5 LESS THAN W-LC-LINE-WIDTH
04468          MOVE W-LC-LINE-INDENT-5 TO W-LINE-INDENT-5
04469
04470      ELSE
04471          MOVE +0                 TO W-LINE-INDENT-5.
04472
04473      IF  W-LC-TOP-MARGIN NUMERIC
04474          MOVE W-LC-TOP-MARGIN    TO W-TOP-MARGIN
04475      ELSE
04476          MOVE +0                 TO W-TOP-MARGIN.
04477
04478      IF W-LC-CASE-IND = 'Y' OR 'N'
04479          NEXT SENTENCE
04480      ELSE
04481          MOVE PI-LOWER-CASE-LETTERS
04482                                  TO W-LC-CASE-IND.
04483
04484  2100-EXIT.
04485      EXIT.
04486                                  EJECT
04487  2150-PROCESS-Z-CONTROLS.
04488
04489      MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.
04490
04491      IF PI-689-RESEND-DATE-1 = LOW-VALUES
04492         IF W-DAYS-TO-RESEND NUMERIC
04493            IF W-DAYS-TO-RESEND > ZEROS
04494               MOVE '6'           TO DC-OPTION-CODE
123113              MOVE W-SAVE-BIN-NEXT-BUS-DT
04496                                  TO DC-BIN-DATE-1
04497               MOVE ZEROS         TO DC-ELAPSED-MONTHS
04498               MOVE W-DAYS-TO-RESEND
04499                                  TO DC-ELAPSED-DAYS
04500               PERFORM 9500-LINK-DATE-CONVERT
                                       THRU 9500-EXIT
04501               IF NO-CONVERSION-ERROR
04502                  MOVE DC-BIN-DATE-2
04503                                  TO PI-689-RESEND-DATE-1
04504                  MOVE DC-GREG-DATE-1-EDIT
04505                                  TO RESEND1O
                                          PI-689-RESEND1-EDIT
04506                  MOVE AL-UANON
04507                                  TO RESEND1A
04508                  MOVE +8         TO RESEND1L
04509               ELSE
04510                  MOVE ER-3770
04511                                  TO EMI-ERROR
04512                  MOVE -1         TO MAINTL
04513                  PERFORM 9900-ERROR-FORMAT
                                       THRU 9900-EXIT
04514                  GO TO 8200-SEND-DATAONLY
100705              END-IF
                 END-IF
              END-IF
           END-IF
100705     IF W-FORM-TO-RESEND NOT = SPACES AND ZEROS
100705        MOVE W-FORM-TO-RESEND    TO PI-689-RESEND-LETR-1
100705     END-IF
04588      IF  PI-689-FOLLOW-UP-DATE EQUAL LOW-VALUES
04589          IF  W-DAYS-TO-FOLLOW-UP NUMERIC
090612            IF  W-DAYS-TO-FOLLOW-UP >= ZEROS
04591                  MOVE '6'        TO DC-OPTION-CODE
04592                  MOVE W-SAVE-BIN-DATE
04593                                  TO DC-BIN-DATE-1
04594                  MOVE ZEROS      TO DC-ELAPSED-MONTHS
04595                  MOVE W-DAYS-TO-FOLLOW-UP
04596                                  TO DC-ELAPSED-DAYS
04597                  PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
04598                  IF  NO-CONVERSION-ERROR
04599                      MOVE DC-BIN-DATE-2
04600                                  TO PI-689-FOLLOW-UP-DATE
04601                      MOVE DC-GREG-DATE-1-EDIT
04602                                  TO FOLLOWO
04603                      MOVE AL-UANON
04604                                  TO FOLLOWA
04605                      MOVE +8     TO FOLLOWL
04606                  ELSE
04607                      MOVE ER-3771
04608                                  TO EMI-ERROR
04609                      MOVE -1     TO MAINTL
04610                      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04611                      GO TO 8200-SEND-DATAONLY.
04612
04613      IF  COPIESI NOT NUMERIC
04614              OR
04615          COPIESI NOT GREATER THAN ZEROS
04616          IF  W-NUMBER-OF-COPIES NUMERIC
04617              IF  W-NUMBER-OF-COPIES GREATER THAN ZEROS
04618                  MOVE W-NUMBER-OF-COPIES
04619                                  TO COPIESI
04620                                     PI-689-NUMBER-COPIES
04621                  MOVE AL-UNNON   TO COPIESA
04622                  MOVE +1         TO COPIESL
04623              ELSE
04624                  MOVE +1         TO COPIESI
04625                                     PI-689-NUMBER-COPIES
04626                  MOVE AL-UNNON   TO COPIESA
04627                  MOVE +1         TO COPIESL
04628          ELSE
04629              MOVE +1             TO COPIESI
04630                                     PI-689-NUMBER-COPIES
04631              MOVE AL-UNNON       TO COPIESA
04632              MOVE +1             TO COPIESL.
122011
122011     IF W-PROMPT-LETTER EQUAL 'Y'
122011         MOVE 'Y'                TO PI-PROMPT-IND
122011         MOVE ER-0894            TO EMI-ERROR
122011         MOVE -1                 TO MAINTL
122011         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122011     END-IF.
061412
061412     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD
061412                                           ENCO
061412     MOVE AL-UANON                      TO ENCA
061412     MOVE +3                            TO ENCL
061412
122011     IF W-PRINT-CERTIFICATE = 'Y'
091712        MOVE 'Y' TO PI-CERT-REQ-IND
122011        IF CERTIDL > ZEROS
122011            IF CERTIDI NUMERIC
122011              MOVE AL-UANON      TO CERTIDA
122011              MOVE CERTIDI       TO PI-CERT-FORM-ID
122011            ELSE
122011              MOVE ER-1778       TO EMI-ERROR
122011              MOVE -1            TO CERTIDL
122011              MOVE AL-UABON      TO CERTIDA
122011              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122011           END-IF
122011        ELSE
122011              MOVE ER-0715       TO EMI-ERROR
122011              MOVE -1            TO CERTIDL
122011              MOVE AL-UABON      TO CERTIDA
122011              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
122011        END-IF
122011     END-IF.
011013
011013     IF W-REASONS-REQUIRED = 'Y'
011013        MOVE 'Y' TO PI-REASON-REQ-IND
011013        IF PI-ENDT-ARCH-NO NOT > ZERO
011013          MOVE ER-9840       TO EMI-ERROR
011013          MOVE -1            TO FORML
011013          MOVE AL-UABON      TO FORMA
011013          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
011013        END-IF
011013     END-IF.
04633
04634 *    IF  PI-689-PRINT-RESTRICTION NOT = 'C' AND 'F' AND 'P' AND
04635 *                                       SPACES
04636 *        IF  W-PRINT-RESTRICTED
04637 *            MOVE W-PRINT-RESTRICTED-IND
04638 *                                TO PRTRESTI
04639 *                                   PI-689-PRINT-RESTRICTION
04640 *            MOVE AL-UANON       TO PRTRESTA
04641 *            MOVE +1             TO PRTRESTL.
           .
04643  2150-EXIT.
04644      EXIT.
04645                                  EJECT
04646  2200-CHECK-FOR-VARIABLE.
04647
04648      IF  W-TX-TEXT (W-TG-NDX) EQUAL SPACES
04649          GO TO 2200-EXIT.
04650
04651      SET W-TX-NDX                TO +1.
04652
04653  2200-CONTINUE.
04654
04655      SEARCH W-TX-CHAR
04656          VARYING
04657              W-TX-NDX
04658          AT END
04659              GO TO 2200-EXIT
04660          WHEN
04661              W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
04662              NEXT SENTENCE.
04663
04664      SET W-TX-NDX UP BY +1.
04665
04666      IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
04667          PERFORM 2220-BUILD-VARIABLE-NUMBER THRU 2220-EXIT
04668          GO TO 2200-CONTINUE.
04669
04670  2200-EXIT.
04671      EXIT.
04672                                  EJECT
04673  2220-BUILD-VARIABLE-NUMBER.
04674
04675      SET W-TX-NDX UP BY +1.
04676      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V1.
04677      SET W-TX-NDX UP BY +1.
04678      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V2.
04679      SET W-TX-NDX UP BY +1.
04680      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX) TO W-V3.
04681
04682      IF  W-VAR-RELATIVE-NUM NUMERIC
04683          SET W-FILE-NDX TO W-VARIABLE-SOURCE (W-VAR-RELATIVE-NUM)
04684          MOVE 'Y' TO W-FILE-USE-IND (W-FILE-NDX).
04685
04686  2220-EXIT.
04687      EXIT.
04688                                  EJECT
04689  4000-ROLL-PAGE.
04690
04691      IF  ENTERPFL NOT EQUAL ZEROS
04692          MOVE -1                 TO ENTERPFL
04693      ELSE
04694          MOVE -1                 TO MAINTL.
04695
04696      IF  PI-TOTAL-LINES EQUAL 0
04697          MOVE ER-0047            TO EMI-ERROR
04698          MOVE -1                 TO MAINTL
04699          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04700          GO TO 8200-SEND-DATAONLY.
04701
04702      COMPUTE W-TEMP-CURRENT-LINE EQUAL
04703          PI-CURRENT-LINE + W-ROLL-COUNTER.
04704
04705      IF  W-TEMP-CURRENT-LINE NEGATIVE
04706              OR
04707          W-TEMP-CURRENT-LINE EQUAL ZEROS
04708          MOVE ER-0067            TO EMI-ERROR
04709          MOVE -1                 TO MAINTL
04710          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04711          MOVE 1                  TO W-TEMP-CURRENT-LINE.
04712
04713 *    IF  W-TEMP-CURRENT-LINE GREATER THAN PI-TOTAL-LINES
04714 *            AND
04715 *        (EIBAID EQUAL DFHPF1 OR DFHPF6)
04716 *        MOVE ER-0066            TO EMI-ERROR
04717 *        MOVE -1                 TO MAINTL
04718 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04719 *        COMPUTE W-TEMP-CURRENT-LINE
04720 *            = PI-TOTAL-LINES + 1 - W-NUM-LINES-PER-SCREEN
04721 *        IF  W-TEMP-CURRENT-LINE NEGATIVE
04722 *                OR
04723 *            W-TEMP-CURRENT-LINE = ZEROS
04724 *            MOVE 1 TO W-TEMP-CURRENT-LINE.
04725
04726      IF  EMI-ERROR NOT EQUAL ER-0191
04727              AND
04728          MAINTI NOT EQUAL 'S'
110404         PERFORM 7700-READ-TEXT-TS THRU 7700-EXIT
04729          PERFORM 4100-SET-NDX THRU 4100-EXIT
04730          PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
04731                  VARYING
04732              W-SC-NDX FROM 1 BY 1
04733                  UNTIL
04734              W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
04735
04736      IF  EMI-ERROR EQUAL ER-0066 OR ER-0067 OR ER-0000 OR
04737                          ER-0191
04738          NEXT SENTENCE
04739      ELSE
04740          GO TO 8200-SEND-DATAONLY.
04741
04742      MOVE W-TEMP-CURRENT-LINE    TO PI-CURRENT-LINE
04743      SET W-RG-NDX                TO PI-CURRENT-LINE
04744      MOVE LOW-VALUES             TO EL689RI
081004*    PERFORM 7700-READ-TEXT-TS   THRU 7700-EXIT
081004     PERFORM 7740-RESTORE-SCREEN THRU 7749-EXIT
081004     SET W-RG-NDX                TO PI-CURRENT-LINE
04745      PERFORM 7000-FORMAT-SCREEN THRU 7000-EXIT
04746              VARYING
04747          W-SC-NDX FROM 1 BY 1
04748              UNTIL
04749          W-SC-NDX GREATER W-NUM-LINES-PER-SCREEN.
04750
04751      GO TO 8200-SEND-DATAONLY.
04752                                  EJECT
04753  4100-SET-NDX.
04754
04755      IF  PI-CURRENT-LINE EQUAL 0
04756          SET W-RG-NDX            TO 1
04757      ELSE
04758          SET W-RG-NDX            TO PI-CURRENT-LINE.
04759
04760  4100-EXIT.
04761       EXIT.
04762                                  EJECT
04763  4200-UPDATE-TABLE-FROM-SCREEN.
04764
04765      IF  W-SC-TEXTL (W-SC-NDX) NOT EQUAL ZEROS
04766          MOVE ZEROS              TO W-SC-TEXTL (W-SC-NDX)
04767          IF  W-RG-NDX GREATER THAN PI-TOTAL-LINES
04768              IF  PI-TOTAL-LINES EQUAL W-MAX-LINES
04769                  MOVE ER-0051    TO EMI-ERROR
04770                  MOVE -1         TO MAINTL
04771                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04772                  GO TO 8200-SEND-DATAONLY
04773              ELSE
04774                  MOVE W-SC-TEXT (W-SC-NDX)
04775                                  TO W-RC-TEXT (W-RG-NDX)
04776                  ADD 1           TO PI-TOTAL-LINES
04777          ELSE
04778              MOVE W-SC-TEXT (W-SC-NDX)
04779                                  TO W-RC-TEXT (W-RG-NDX).
04780
04781      SET W-RG-NDX UP BY 1.
04782
04783  4200-EXIT.
04784       EXIT.
04785                                  EJECT
010814
010814
010814 4500-CONNECT-TO-DB.
010814
063022     MOVE 'TEST_LOGIC'           TO SVR
063022     MOVE 'appuser'              TO USR
063022     MOVE 'appuser@cso'          TO PASS
063022     if ws-kix-myenv = 'cid1p'
063022        MOVE 'PROD_LOGIC'        TO SVR
063022     end-if
010814
010814     STRING
010814         USR DELIMITED SPACE
010814         "." DELIMITED SIZE
010814         PASS DELIMITED SPACE INTO USR-PASS
010814     END-STRING
010814
010814     EXEC SQL
010814        CONNECT TO :SVR USER :USR-PASS
010814     END-EXEC
010814
010814     IF SQLCODE NOT = 0
010814        DISPLAY "ERROR: CANNOT CONNECT "
010814        DISPLAY SQLCODE
010814        DISPLAY SQLERRMC
010814        GO TO 4500-EXIT
010814     END-IF
010814
010814     .
010814 4500-EXIT.
010814     EXIT.
010814
010814
010814
010814 4600-GET-NEXT-BUS-DT.
010814
010814     MOVE W-SAVE-EDIT-A-DATE     TO WS-CYCLE-DATE
010814     MOVE SPACES                 TO WS-NEXT-BUS-DT
010814
010814     IF WS-KIXHOST = 'logictest'
010814        EXEC SQL
010814          CALL NaperTestCalcNextBusDt
010814                        @cycledate = :WS-CYCLE-DATE,
010814                        @nextbusdate = :WS-NEXT-BUS-DT OUT
010814        END-EXEC
010814     ELSE
010814        EXEC SQL
010814          CALL NaperProdCalcNextBusDt
010814                        @cycledate = :WS-CYCLE-DATE,
010814                        @nextbusdate = :WS-NEXT-BUS-DT OUT
010814        END-EXEC
010814     END-IF
010814
010814     IF SQLCODE NOT = 0
010814        DISPLAY "ERROR: DID NOT RETURN NEXT BUS DT "
010814        DISPLAY ' SQL RETURN CODE ' SQLCODE
010814        DISPLAY ' SQL ERR MESS    ' SQLERRMC
010814        GO TO 4600-EXIT
010814     END-IF
010814
010814     .
010814 4600-EXIT.
010814     EXIT.
010814
010814
021214
021214 4700-DISCONNECT.
021214
021214     EXEC SQL
021214        DISCONNECT
021214     END-EXEC
021214     .
021214 4700-EXIT.
021214     EXIT.
021214
04786  5000-EDIT-MODE.
04787 ***************************************************************
04788 *    THIS ROUTINE WILL SAVE A COPY OF THE EXISTING SCREEN     *
04789 *    AND THE W-TS-TABLE OF LETTER TEXT.  IT WILL THEN         *
04790 *    TRANSFER TO THE W-TEXT-EDITOR PROGRAM.                   *
04791 ***************************************************************
04792
04793      IF  PI-SHOW-MODE
04794          MOVE ER-0188            TO EMI-ERROR
04795          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04796          MOVE -1                 TO MAINTL
04797          MOVE AL-UABON           TO MAINTA
04798          GO TO 8200-SEND-DATAONLY.
04799
04800      IF  PI-CURRENT-LINE EQUAL ZEROS
04801          MOVE ER-0187            TO EMI-ERROR
04802          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04803          MOVE -1                 TO MAINTL
04804          GO TO 8200-SEND-DATAONLY.
04805
081004     PERFORM 7700-READ-TEXT-TS   THRU 7700-EXIT
04806      PERFORM 4100-SET-NDX THRU 4100-EXIT.
04807      PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
04808              VARYING
04809          W-SC-NDX FROM 1 BY 1
04810              UNTIL
04811          W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
04812
04813      PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT.
04814
04815      MOVE PI-689-FORM-NUMBER     TO PI-COMM-CONTROL.
04816      MOVE ZEROS                  TO PI-UPDATE-SW.
04817      MOVE '1'                    TO PI-ENTRY-CD-1.
04818      MOVE W-PGM-EL1042           TO W-CALL-PGM.
04819      GO TO 9400-XCTL.
04820                                  EJECT
04821  5400-LETTER-RELEASE.
04822 ***************************************************************
04823 *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *
04824 *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS IN       *
04825 *    THE ARCHIVE FILE.                                        *
04826 *    THE FUNCTIONS BELOW WILL BE PERFORMED.                   *
04827 *        1. CHECK SECURITY.                                   *
04828 *        2. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS.        *
04829 *        3. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *
04830 *        4. WRITE THE NEW ARCHIVE RECORDS FROM W-TS-TABLE.    *
04831 *        5. RESET INSURED'S CONTROL FIELDS AND RETURN THE     *
04832 *           ARCHIVE NUMBER USED  TO FILE THE RECORDS.         *
04833 ***************************************************************
04834
04835      IF  NOT MODIFY-CAP
04836          MOVE ER-0070            TO EMI-ERROR
04837          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04838          MOVE -1                 TO MAINTL
04839          GO TO 8200-SEND-DATAONLY.
04840
04841      IF  PI-SHOW-MODE
04842          MOVE ER-0188            TO EMI-ERROR
04843          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04844          MOVE -1                 TO MAINTL
04845          MOVE AL-UABON           TO MAINTA
04846          GO TO 8200-SEND-DATAONLY.
04847
04848       IF  ARCHNUML GREATER THAN ZEROS
04849           MOVE ER-9320            TO EMI-ERROR
04850           MOVE -1                 TO ARCHNUML
04851           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
04852           GO TO 8200-SEND-DATAONLY.
04853
072312*     IF  PI-CURRENT-LINE EQUAL ZEROS
072312*        MOVE ER-0187            TO EMI-ERROR
072312*        MOVE -1                 TO MAINTL
072312*        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
072312*        GO TO 8200-SEND-DATAONLY.
04859
081004*    PERFORM 7700-READ-TEXT-TS   THRU 7700-EXIT
      *
04860 *    IF  NOT PI-689-CREATE-NO-SCREENS
04861 *        PERFORM 4100-SET-NDX THRU 4100-EXIT
04862 *        PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
04863 *                VARYING
04864 *            W-SC-NDX FROM 1 BY 1
04865 *                UNTIL
04866 *            W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN
04867 *        IF  EMI-FATAL-CTR GREATER THAN ZEROS
04868 *                OR
04869 *            PI-689-FATAL-ERROR
04870 *            GO TO 8200-SEND-DATAONLY.
04871 *
04872 *    MOVE SPACES                 TO W-REMAINING-VAR-SW.
04873 *    PERFORM 7915-SEARCH-REMAINING-VARS THRU 7915-EXIT
04874 *            VARYING
04875 *        W-RVS-NDX FROM +1 BY +1
04876 *            UNTIL
04877 *        W-RVS-NDX > +36500
04878 *            OR
04879 *        W-REMAINING-VAR-FOUND.
04880 *
04881 *    IF  NOT PI-689-CREATE-NO-SCREENS
04882 *        IF  W-REMAINING-VAR-FOUND
04883 *            GO TO 4000-ROLL-PAGE.
04884
04885 *    MOVE SPACES                 TO W-CNTL-KEY.
04886 *    MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
04887 *    MOVE '1'                    TO W-CNTL-RECORD-TYPE.
04888 *    MOVE ZEROS                  TO W-CNTL-SEQ-NO.
04889 *
04890 *    EXEC CICS HANDLE CONDITION
04891 *         NOTOPEN    (8040-CNTL-NOT-OPEN)
04892 *         NOTFND     (5470-CNTL-NOT-FOUND)
04893 *    END-EXEC.
04894 *
04895 *    EXEC CICS READ
04896 *         DATASET    (W-CNTL-FILE-ID)
04897 *         SET        (ADDRESS OF CONTROL-FILE)
04898 *         RIDFLD     (W-CNTL-KEY)
04899 *         UPDATE
04900 *    END-EXEC.
04901 *
04902 *    IF  CF-CREDIT-LAST-ARCH-NUM NOT NUMERIC
04903 *        MOVE ZEROS              TO CF-CREDIT-LAST-ARCH-NUM
04904 *                                   CF-CREDIT-START-ARCH-NUM.
04905 *
04906 *    ADD 1                       TO CF-CREDIT-LAST-ARCH-NUM.
04907 *    MOVE CF-CREDIT-LAST-ARCH-NUM
04908 *                                TO W-ARCH-NUMBER
04909 *                                   W-ARCT-NUMBER.
04910 *
04911 *    EXEC CICS REWRITE
04912 *         FROM      (CONTROL-FILE)
04913 *         DATASET   (W-CNTL-FILE-ID)
04914 *    END-EXEC
04915
04916 *    EXEC CICS HANDLE CONDITION
04917 *         NOTOPEN   (8010-ARCH-NOT-OPEN)
04918 *    END-EXEC.
04919 *
04933 *    EXEC CICS GETMAIN
04934 *       SET      (ADDRESS OF LETTER-ARCHIVE)
04935 *       LENGTH   (W-ARCH-LENGTH)
04936 *    END-EXEC
04937 *
04938 *    MOVE SPACES                 TO LETTER-ARCHIVE
04939 *    MOVE 'LA'                   TO LA-RECORD-ID.
04940 *    MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO
04941 *                                   LA-ARCHIVE-NO-A2
04942 *                                   LA-ARCHIVE-NO-A3
04943 *                                   LA-ARCHIVE-NO-A4
04944 *                                   LA-ARCHIVE-NO-A5
04945 *                                   LA-ARCHIVE-NO-A6
04946 *                                   PI-689-ARCHIVE-NUMBER.
04947 *    MOVE PI-COMPANY-CD          TO LA-COMPANY-CD
04948 *                                   LA-COMPANY-CD-A2
04949 *                                   LA-COMPANY-CD-A3
04950 *                                   LA-COMPANY-CD-A4
04951 *                                   LA-COMPANY-CD-A5
04952 *                                   LA-COMPANY-CD-A6.
04953 *    MOVE PI-689-CARRIER         TO LA-CARRIER-A2
04954 *                                   LA-CARRIER-A3
04955 *                                   LA-CARRIER-A4
04956 *                                   LA-CARRIER-A5.
04957 *    MOVE PI-689-EFF-DATE        TO LA-EFFECT-DATE-A2.
04958 *    MOVE PI-689-GROUPING        TO LA-GROUPING-A2
04959 *                                   LA-GROUPING-A3
04960 *                                   LA-GROUPING-A4
04961 *                                   LA-GROUPING-A5.
04962 *    MOVE PI-689-ACCOUNT         TO LA-ACCOUNT-A2
04963 *                                   LA-ACCOUNT-A3
04964 *                                   LA-ACCOUNT-A4
04965 *                                   LA-ACCOUNT-A5.
04966 *    MOVE PI-689-STATE           TO LA-STATE-A2
04967 *                                   LA-STATE-A3
04968 *                                   LA-STATE-A4
04969 *                                   LA-STATE-A5.
04970 *
04971 *    IF  PI-689-DATA-SOURCE EQUAL '3'
04972 *            AND
04973 *        PI-689-RESP-PERSON GREATER THAN SPACES
04974 *        MOVE PI-689-RESP-PERSON
04975 *                                TO LA-RESP-PERSON-A2
04976 *        MOVE PI-689-TYPE        TO LA-TYPE-A2
04977 *    ELSE
04978 *        IF  PI-689-CERT-PRIME GREATER THAN SPACES
04979 *            MOVE PI-689-CERT-PRIME
04980 *                                TO LA-CERT-PRIME-A2
04981 *            MOVE PI-689-CERT-SFX
04982 *                                TO LA-CERT-SUFFIX-A2
04983 *        ELSE
04984 *            MOVE LOW-VALUE      TO LA-RESP-PERSON-A2
04985 *                                   LA-TYPE-A2.
04986 *
04987 *    IF  PI-689-ENTRY-BATCH GREATER THAN SPACES
04988 *        MOVE PI-689-ENTRY-BATCH TO LA-ENTRY-A6
04989 *        IF  PI-689-CONTROL GREATER THAN ZEROS
04990 *                AND
04991 *            PI-689-PRINT-RESTRICTION EQUAL 'C'
04992 *            MOVE PI-689-CONTROL TO LA-QUE-CONTROL-A6
04993 *            MOVE 'CK'           TO LA-FILLER
04994 *        ELSE
04995 *            NEXT SENTENCE
04996 *    ELSE
04997 *        IF  PI-689-CONTROL GREATER THAN ZEROS
04998 *            MOVE PI-689-CONTROL TO LA-QUE-CONTROL-A6
04999 *            MOVE 'CK'           TO LA-FILLER
05000 *        ELSE
05001 *            MOVE LOW-VALUES     TO LA-ENTRY-A6.
05002 *
05003 *    MOVE ZEROS                  TO W-SEQ-CTR.
05004 *
05005 *    MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.
05006 *
05007 *    MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
05008 *                                   LA-INITIAL-PRINT-DATE
05009 *                                   LA-SENT-DATE
05012 *                                   LA-REPLY-DATE.
05013 *    MOVE 'A'                    TO LA-STATUS.
05014 *
05015 *    IF  PI-689-NUMBER-COPIES NOT NUMERIC
05016 *            OR
05017 *        PI-689-NUMBER-COPIES EQUAL ZEROS
05018 *        MOVE  1                 TO LA-NO-OF-COPIES
05019 *    ELSE
05020 *        MOVE PI-689-NUMBER-COPIES
05021 *                                TO LA-NO-OF-COPIES.
05022 *
05023 *    MOVE PI-689-FORM-NUMBER     TO LA-FORM-A3.
05024 *    MOVE PI-689-FOLLOW-UP-DATE  TO LA-FOLLOW-UP-DATE.
05025 *    MOVE PI-689-PRINT-RESTRICTION
05026 *                                TO LA-PRINT-RESTRICTION
05027 *    MOVE PI-689-LABEL-SOURCE    TO LA-ADDR-SOURCE.
05028 *    MOVE PI-689-DATA-SOURCE     TO LA-DATA-SOURCE.
05029 *    MOVE PI-689-RESEND-DATE-1   TO LA-RESEND-DATE
100705*    MOVE PI-689-RESEND-LETR-1   TO LA-RESEND-LETR
05032 *    MOVE W-CURRENT-SAVE         TO LA-CREATION-DATE.
05033 *
05034 *    IF  PI-689-PRINT-PERFORMED
05035 *        MOVE SPACES             TO PI-689-PRINT-SW
05036 *        MOVE W-CURRENT-SAVE     TO LA-INITIAL-PRINT-DATE
05037 *        IF  LA-RESEND-DATE = LOW-VALUES
05038 *            MOVE 'C'            TO LA-STATUS.
05039 *
05040 *    MOVE PI-689-NUMBER-LABEL-LINES
05041 *                                TO LA-NUMBER-LABEL-LINES.
05042 *    MOVE ZEROS                  TO PI-689-NUMBER-TEXT-RECORDS.
05043 *
05044 *    PERFORM 5410-CREATE-TEXT-RECORDS THRU 5410-EXIT.
05045 *
05046 *    MOVE PI-689-NUMBER-TEXT-RECORDS
05047 *    MOVE ZEROS                  TO LA-NO-OF-TEXT-RECORDS.
05048 *
05049 *    EXEC CICS HANDLE CONDITION
05050 *        DUPREC    (5460-DUPLICATE-ARCHIVE-RCD)
05051 *    END-EXEC.
05052 *
05053 *    PERFORM 5650-WRITE-ARCHIVE THRU 5650-EXIT.
05058 *    MOVE ER-0280                TO EMI-ERROR.
05059 *    MOVE -1                     TO MAINTL.
05060 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05061 *    MOVE W-ARCH-NUMBER          TO W-ARCH-SUPPRESS
05062 *    MOVE W-ARCH-EDIT            TO EMI-TEXT-VARIABLE (1).
           .
05064  5400-END.
           DISPLAY ' MADE IT TO 5400 '
           MOVE SPACES                 TO BL-INPUT
           MOVE ZEROS                  TO BL-ARCHIVE-NO
           MOVE PI-689-DATA-SOURCE     TO BL-DATA-SRCE
           MOVE PI-689-CARRIER         TO BL-CARRIER
           MOVE PI-689-GROUPING        TO BL-GROUP
           MOVE PI-689-STATE           TO BL-STATE
           MOVE PI-689-ACCOUNT         TO BL-ACCOUNT
           IF PI-689-EFF-DATE NOT = LOW-VALUES
              MOVE PI-689-EFF-DATE     TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9500-LINK-DATE-CONVERT
                                       THRU 9500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-1-EDIT
                                       TO BL-EFF-DT
              END-IF
           END-IF
           MOVE PI-689-CERT-NO         TO BL-CERT-NO
           MOVE PI-689-ENTRY-BATCH     TO BL-BATCH-NO
           MOVE PI-689-SEQ-NO          TO BL-BATCH-SEQ
           MOVE PI-689-RESP-PERSON     TO BL-RESP-NO
           MOVE PI-689-FORM-NUMBER     TO BL-LETTER-ID
           MOVE PI-689-NUMBER-COPIES   TO BL-NO-OF-COPIES
101812     IF PI-ENDT-ARCH-NO > ZERO
101812         MOVE PI-ENDT-ARCH-NO    TO BL-ENDT-ARCH-NO
101812     END-IF
101812     MOVE 'LETTER  '             TO BL-SOURCE-SCREEN
           MOVE PI-PROCESSOR-ID        TO BL-PROC-ID
      *    MOVE 'ALWA'                 TO BL-PROC-ID
           MOVE PI-COMPANY-ID          TO BL-COMP-ID
      *    MOVE IFF-PRINT-NOW-SW       TO BL-PRINT-NOW-SW
061412     MOVE PI-ENCLOSURE-CD        TO BL-ENC-CD
           IF PI-689-RESEND-DATE-1 NOT = LOW-VALUES
              MOVE PI-689-RESEND-DATE-1 TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9500-LINK-DATE-CONVERT
                                       THRU 9500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-B-EDIT
                                       TO BL-RESEND-DT
              END-IF
           END-IF
           IF PI-689-FOLLOW-UP-DATE NOT = LOW-VALUES
              MOVE PI-689-FOLLOW-UP-DATE TO DC-BIN-DATE-1
              MOVE ' '                 TO DC-OPTION-CODE
              PERFORM 9500-LINK-DATE-CONVERT
                                       THRU 9500-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-GREG-DATE-B-EDIT
                                       TO BL-FOLLOW-UP-DT
              END-IF
           END-IF
      *    MOVE IFF-COMMENTS           TO BL-COMMENTS
      *    MOVE 'TEST COMMENTS'        TO BL-COMMENTS
122011     MOVE PI-CERT-FORM-ID        TO BL-CERT-FORM-ID
           if eibaid = dfhpf7
              move 'B'                 TO BL-WRITE-ERARCH
           else
              move 'T'                 to bl-write-erarch
           end-if
PEMTST    DISPLAY ' ABOUT TO LINK TO NSRASBL '
      *****************************************
      * Invoke the LETTER business logic
      *****************************************
           
      * exec cics link
      *       program('NSRASBL')
      *       commarea(srch-commarea)
      *    end-exec.
           MOVE LENGTH OF
            srch-commarea
             TO DFHEIV11
           MOVE 'NSRASBL' TO DFHEIV1
      *    MOVE '."C                   (   #00011755' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303131373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 srch-commarea, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           DISPLAY ' MADE IT BACK FROM NSRASBL ' bl-status ' '
              bl-message ' ' bl-archive-no
           move bl-archive-no to w-arch-number
05071
           if eibaid = dfhpf7
012413        PERFORM 5440-ADD-BILLING-NOTE THRU 5440-EXIT
05058         MOVE ER-0280                TO EMI-ERROR
05059         MOVE -1                     TO MAINTL
05060         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05061         MOVE W-ARCH-NUMBER          TO W-ARCH-SUPPRESS
05062         MOVE W-ARCH-EDIT            TO EMI-TEXT-VARIABLE (1)
091712        MOVE '2'                    TO PI-ACTION
091712        MOVE ZEROS                  TO PI-TOTAL-LINES
091712                                       PI-CURRENT-LINE
091712        MOVE SPACES                 TO PI-689-PRINT-SW
091712                                       PI-689-FORM-NUMBER
110512        GO TO 9300-DFHCLEAR
           else
              move w-arch-number          to archnumo
05058         MOVE ER-1818                TO EMI-ERROR
05059         MOVE -1                     TO MAINTL
05060         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           end-if
05076      MOVE -1                     TO MAINTL
05077      GO TO 8200-SEND-DATAONLY
           .
05079  5400-EXIT.
05080      EXIT.
05081                                  EJECT
05082  5410-CREATE-TEXT-RECORDS.
05083
05084 *    EXEC CICS HANDLE CONDITION
05085 *         NOTOPEN   (8015-ARCT-NOT-OPEN)
05086 *         DUPREC    (5465-DUPLICATE-ARCH-TEXT)
05087 *    END-EXEC.
05088 *
05089 *    IF  PI-689-CREATE-NO-SCREENS
05090 *        IF  PI-689-GET-ARCT-MAIN
05091 *            EXEC CICS GETMAIN
05092 *                 SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
05093 *                 LENGTH   (W-ARCT-LENGTH)
05094 *            END-EXEC
05095 *            SET LCP-WS-ADDR-PNTR TO ADDRESS OF
05096 *                                    LETTER-ARCHIVE-TEXT
05097 *            MOVE LCP-WS-ADDR-COMP
05098 *                                TO PI-689-ARCT-POINTER
05099 *        ELSE
05100 *            MOVE PI-689-ARCT-POINTER
05101 *                                TO LCP-WS-ADDR-COMP
05102 *            SET ADDRESS OF LETTER-ARCHIVE-TEXT
05103 *                                TO LCP-WS-ADDR-PNTR
05104 *    ELSE
05105 *        EXEC CICS GETMAIN
05106 *             SET      (ADDRESS OF LETTER-ARCHIVE-TEXT)
05107 *             LENGTH   (W-ARCT-LENGTH)
05108 *        END-EXEC.
05109 *
05110 *    PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT.
05111 *
05112 *    MOVE ZEROS                  TO W-SEQ-CTR
05113 *
05114 *    IF  PI-CREATE-LABELS AND
05115 *        NOT PI-689-LABELS-OVERRIDEN
05116 *        MOVE '1'                TO LT-RECORD-TYPE
05117 *        MOVE +0                 TO LT-LINE-SEQ-NO
05118 *        PERFORM 5600-FORMAT-ADDRESS-LINE THRU 5600-EXIT
05119 *                VARYING
05120 *            W-RG-NDX FROM 1 BY 1
05121 *                UNTIL
05122 *            W-RG-NDX GREATER THAN PI-689-NUMBER-LABEL-LINES
05123 *        PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
05124 *        PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT
05125 *    ELSE
05126 *        SET W-RG-NDX            TO +1.
05127 *
05128 *    SET LT-NDX                  TO W-ZEROS.
05129 *
05130 *    PERFORM 5500-FORMAT-TEXT THRU 5500-EXIT
05131 *            VARYING
05132 *        W-RG-NDX FROM W-RG-NDX BY +1
05133 *            UNTIL
05134 *        W-RG-NDX GREATER THAN PI-TOTAL-LINES.
05135
05136  5410-EXIT.
05137      EXIT.
05138                                  EJECT
05139  5415-INITIALIZE-TEXT.
05140
05141      MOVE SPACES                 TO LETTER-ARCHIVE-TEXT.
05142      MOVE 'LT'                   TO LT-RECORD-ID.
05143
05144      MOVE LA-ARCHIVE-NO          TO LT-ARCHIVE-NO.
05145      MOVE PI-COMPANY-CD          TO LT-COMPANY-CD.
05146
05147      MOVE ZEROS                  TO LT-LINE-SEQ-NO
05148                                     LT-NUM-LINES-ON-RECORD.
05149
05150  5415-EXIT.
05151      EXIT.
012413
012413 5440-ADD-BILLING-NOTE.
012413
012413     MOVE LOW-VALUES             TO W-ELEOBC-KEY
012413     MOVE PI-COMPANY-CD          TO W-EOBC-COMPANY-CD
012413     MOVE '5'                    TO W-EOBC-REC-TYPE
012413
012413     
      * EXEC CICS STARTBR
012413*        DATASET   ('ELEOBC')
012413*        RIDFLD    (W-ELEOBC-KEY)
012413*        GTEQ
012413*        RESP      (W-RESPONSE)
012413*    END-EXEC
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00011865' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383635' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 W-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413
012413     IF NOT RESP-NORMAL
012413        GO TO 5440-EXIT
012413     END-IF
012413      .
012413 5440-READNEXT-ELEOBC.
012413
012413     
      * EXEC CICS READNEXT
012413*       INTO    (EOB-CODES)
012413*       DATASET ('ELEOBC')
012413*       RIDFLD  (W-ELEOBC-KEY)
012413*       RESP    (W-RESPONSE)
012413*    END-EXEC
           MOVE LENGTH OF
            EOB-CODES
             TO DFHEIV12
           MOVE 'ELEOBC' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.IL                  )  N#00011878' TO DFHEIV0
           MOVE X'262E494C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303131383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EOB-CODES, 
                 DFHEIV12, 
                 W-ELEOBC-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413
012413     IF RESP-NORMAL
012413         IF EO-RECORD-TYPE NOT = '5'
012413             GO TO 5440-EXIT
012413         END-IF
012413     ELSE
012413         GO TO 5440-EXIT
012413     END-IF
012413
012413     IF EO-RECORD-TYPE = '5' AND
012413        EO-EOB-CODE = PI-689-FORM-NUMBER
012413           CONTINUE
012413     ELSE
012413         GO TO 5440-READNEXT-ELEOBC
012413     END-IF
012413
012413     MOVE SPACES TO W-BILLING-NOTE
012413     MOVE EO-DESCRIPTION TO W-BN-NOTE
012413     MOVE PI-689-FORM-NUMBER TO W-BN-LTRID
012413     MOVE W-SAVE-DATE TO W-BN-DATE
012413     MOVE PI-PROCESSOR-ID TO W-BN-USERID
012413     MOVE +25 TO W-LEN
012413
012413     PERFORM 5441-UPDATE-BILLING-NOTE THRU 5441-EXIT
012413     .
012413 5440-EXIT.
012413     EXIT.
012413
012413 5441-UPDATE-BILLING-NOTE.
012413
012413     MOVE PI-COMPANY-CD          TO W-SV-COMPANY-CD
012413     MOVE PI-689-CARRIER         TO W-SV-CARRIER
012413     MOVE PI-689-GROUPING        TO W-SV-GROUPING
012413     MOVE PI-689-STATE           TO W-SV-STATE
012413     MOVE PI-689-ACCOUNT         TO W-SV-ACCOUNT
012413     MOVE PI-689-EFF-DATE        TO W-SV-CERT-EFF-DT
012413     MOVE PI-689-CERT-PRIME      TO W-SV-CERT-PRIME
012413     MOVE PI-689-CERT-SFX        TO W-SV-CERT-SFX
012413     MOVE W-SAVE-KEY             TO W-ERNOTE-KEY
041320     if pi-iss-can-pend-rec = '2'
041320        move '2'              to cn-record-type
041320     else
041320        move '1'              to cn-record-type
041320     end-if
041320     move cn-record-type      to w-note-record-type
012413
012413     
      * EXEC CICS READ
012413*       DATASET    (W-NOTE-FILE-ID)
012413*       RIDFLD     (W-ERNOTE-KEY)
012413*       INTO       (CERTIFICATE-NOTE)
012413*       RESP       (W-RESPONSE)
012413*       UPDATE
012413*    END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&"IL       EU         (  N#00011930' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303131393330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 W-ERNOTE-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413
012413     IF RESP-NORMAL
012413       IF CN-BILLING-START-LINE-NO NOT NUMERIC
012413          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
012413       END-IF
012413       IF CN-BILLING-END-LINE-NO NOT NUMERIC
012413          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
012413       END-IF
012413       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
012413           (NOTE-SUB > +10) OR
012413           (CN-LINE (NOTE-SUB) (1:W-LEN) =
012413                             W-BILLING-NOTE (1:W-LEN))
012413       END-PERFORM
012413       IF CN-LINE (NOTE-SUB) (1:W-LEN) =
012413                              W-BILLING-NOTE (1:W-LEN)
012413         
      * EXEC CICS UNLOCK
012413*           DATASET    (W-NOTE-FILE-ID)
012413*        END-EXEC
      *    MOVE '&*                    #   #00011952' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303131393532' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413       ELSE
012413         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
012413           (NOTE-SUB > +10) OR
012413           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES)
012413         END-PERFORM
012413         IF (NOTE-SUB < +11)
012413           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
012413              NOTE-SUB <= CN-BILLING-END-LINE-NO
012413                MOVE W-BILLING-NOTE TO CN-LINE (NOTE-SUB)
012413           ELSE
012413             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
012413              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
012413                MOVE W-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
012413                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
012413             ELSE
012413               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
012413                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
012413                     MOVE W-BILLING-NOTE TO CN-LINE (NOTE-SUB)
012413                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
012413               ELSE
012413                 IF (CN-BILLING-END-LINE-NO = ZEROS)
012413                   MOVE W-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
012413                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
012413                                       CN-BILLING-START-LINE-NO
012413                 ELSE
012413                    PERFORM 5442-SQUEEZE-IT-IN THRU 5442-EXIT
012413                 END-IF
012413               END-IF
012413             END-IF
012413           END-IF
012413           MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
012413           MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
012413           MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
012413           
      * EXEC CICS REWRITE
012413*             DATASET    (W-NOTE-FILE-ID)
012413*             FROM       (CERTIFICATE-NOTE)
012413*             RESP       (W-RESPONSE)
012413*          END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00011988' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303131393838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413           PERFORM 5445-CERTIFICATE-UPDATE THRU 5445-EXIT
012413         END-IF
012413       END-IF
012413     ELSE
012413        MOVE SPACES              TO CERTIFICATE-NOTE
012413        MOVE 'CN'                TO CN-RECORD-ID
012413        MOVE W-SAVE-KEY          TO CN-CONTROL-PRIMARY
012413                                    W-ERNOTE-KEY
041320        if pi-iss-can-pend-rec = '2'
041320           move '2'              to cn-record-type
041320        else
041320           move '1'              to cn-record-type
041320        end-if
041320        move cn-record-type      to w-note-record-type
012413        MOVE 01                  TO CN-BILLING-START-LINE-NO
012413                                    CN-BILLING-END-LINE-NO
012413        MOVE W-BILLING-NOTE      TO CN-LINE (01)
012413        MOVE PI-PROCESSOR-ID     TO CN-LAST-MAINT-USER
012413        MOVE W-SAVE-BIN-DATE     TO CN-LAST-MAINT-DT
012413        MOVE EIBTIME             TO CN-LAST-MAINT-HHMMSS
012413        
      * EXEC CICS WRITE
012413*          DATASET    (W-NOTE-FILE-ID)
012413*          FROM       (CERTIFICATE-NOTE)
041320*          RIDFLD     (cn-control-primary)
012413*          RESP       (W-RESPONSE)
012413*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-NOTE
             TO DFHEIV11
      *    MOVE '&$ L                  ''  N#00012013' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'204E233030303132303133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-NOTE-FILE-ID, 
                 CERTIFICATE-NOTE, 
                 DFHEIV11, 
                 cn-control-primary, 
                 DFHEIV99, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413
012413        PERFORM 5445-CERTIFICATE-UPDATE THRU 5445-EXIT
012413     END-IF
012413
012413     .
012413 5441-EXIT.
012413     EXIT.
012413
012413
012413 5442-SQUEEZE-IT-IN.
012413
012413     IF NOTE-SUB < CN-BILLING-START-LINE-NO
012413        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
012413           NOTE-SUB = +10
012413           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
012413           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
012413             MOVE W-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
012413             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
012413             MOVE +9 TO NOTE-SUB
012413           END-IF
012413        END-PERFORM
012413     ELSE
012413        IF NOTE-SUB > CN-BILLING-END-LINE-NO
012413           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1
012413             UNTIL NOTE-SUB = +1
012413             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
012413             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
012413                MOVE W-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
012413                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
012413                MOVE +2          TO NOTE-SUB
012413             END-IF
012413           END-PERFORM
012413        END-IF
012413     END-IF
012413
012413     .
012413 5442-EXIT.
012413     EXIT.
012413
012413 5445-CERTIFICATE-UPDATE.
012413
012413     MOVE W-SAVE-KEY         TO W-CERT-KEY
012413
012413     
      * EXEC CICS READ
012413*        DATASET  (W-CERT-FILE-ID)
012413*        RIDFLD   (W-CERT-KEY)
012413*        SET      (ADDRESS OF CERTIFICATE-MASTER)
012413*        RESP     (W-RESPONSE)
012413*        UPDATE
012413*    END-EXEC
      *    MOVE '&"S        EU         (  N#00012062' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303132303632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413
012413     IF RESP-NORMAL
012413        EVALUATE CM-NOTE-SW
012413           WHEN '2'
012413           WHEN '3'
012413           WHEN '6'
012413           WHEN '7'
012413              SET NO-CERT-RW     TO TRUE
012413           WHEN ' '
012413              MOVE '2'           TO CM-NOTE-SW
012413           WHEN '1'
012413              MOVE '3'           TO CM-NOTE-SW
012413           WHEN '4'
012413              MOVE '6'           TO CM-NOTE-SW
012413           WHEN '5'
012413              MOVE '7'           TO CM-NOTE-SW
012413        END-EVALUATE
012413     END-IF
012413     IF NOT NO-CERT-RW
012413        
      * EXEC CICS REWRITE
012413*          FROM     (CERTIFICATE-MASTER)
012413*          DATASET  (W-CERT-FILE-ID)
012413*          RESP     (W-RESPONSE)
012413*       END-EXEC
           MOVE LENGTH OF
            CERTIFICATE-MASTER
             TO DFHEIV11
      *    MOVE '&& L                  %  N#00012088' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'204E233030303132303838' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 CERTIFICATE-MASTER, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO W-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413     ELSE
012413        
      * EXEC CICS UNLOCK
012413*          DATASET  (W-CERT-FILE-ID)
012413*       END-EXEC
      *    MOVE '&*                    #   #00012094' TO DFHEIV0
           MOVE X'262A20202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303132303934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
012413     END-IF
012413
012413     .
012413 5445-EXIT.
012413     EXIT.
012413
05152                                  EJECT
05153  5460-DUPLICATE-ARCHIVE-RCD.
05154
05155      IF  NOT PI-689-CREATE-NO-SCREENS
05156          
      * EXEC CICS SYNCPOINT ROLLBACK
05157 *        END-EXEC.
      *    MOVE '6"R                   !   #00012107' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05158
05159      MOVE ER-9426                TO EMI-ERROR.
05160      MOVE -1                     TO MAINTL.
05161      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05162      GO TO 8200-SEND-DATAONLY.
05163
05164  5465-DUPLICATE-ARCH-TEXT.
05165
05166      IF  NOT PI-689-CREATE-NO-SCREENS
05167          
      * EXEC CICS SYNCPOINT ROLLBACK
05168 *        END-EXEC.
      *    MOVE '6"R                   !   #00012118' TO DFHEIV0
           MOVE X'362252202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303132313138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05169
05170      MOVE ER-7369                TO EMI-ERROR.
05171      MOVE -1                     TO MAINTL.
05172      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05173      GO TO 8200-SEND-DATAONLY.
05174
05175  5470-CNTL-NOT-FOUND.
05176
05177      MOVE ER-9299                TO EMI-ERROR.
05178      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05179      MOVE -1                     TO MAINTL.
05180      MOVE AL-UANON               TO MAINTA.
05181      GO TO 8200-SEND-DATAONLY.
05182                                  EJECT
05183  5500-FORMAT-TEXT.
05184
05185      SET LT-NDX UP BY +1.
05186
05187      IF  W-RC-TEXT (W-RG-NDX) EQUAL W-TOP-FORM
05188          SET W-RG-NDX UP BY 1
05189          MOVE 99                 TO LT-SKIP-CONTROL (LT-NDX)
05190          MOVE W-RC-TEXT (W-RG-NDX)
05191                                  TO LT-TEXT-LINE (LT-NDX)
05192      ELSE
05193          IF  W-RC-TEXT (W-RG-NDX) EQUAL SPACES
05194              MOVE ZEROS          TO W-PRINT-CONTROL
05195              PERFORM 5505-FIND-USED-LINE THRU 5505-EXIT
05196                      VARYING
05197                  W-RG-NDX FROM W-RG-NDX BY 1
05198                      UNTIL
05199                  W-RG-NDX GREATER THAN PI-TOTAL-LINES
05200                      OR
05201                  W-RC-TEXT (W-RG-NDX) GREATER THAN SPACES
05202              IF  W-RG-NDX GREATER THAN PI-TOTAL-LINES
05203                  GO TO 5500-WRITE-TEST
05204              ELSE
05205                  MOVE W-PRINT-CONTROL
05206                                  TO LT-SKIP-CONTROL (LT-NDX)
05207                  MOVE W-RC-TEXT (W-RG-NDX)
05208                                  TO LT-TEXT-LINE (LT-NDX)
05209          ELSE
05210              MOVE W-RC-TEXT (W-RG-NDX)
05211                                  TO LT-TEXT-LINE (LT-NDX)
05212              MOVE ZEROS          TO W-PRINT-CONTROL.
05213
05214      ADD +1                      TO LT-NUM-LINES-ON-RECORD.
05215
05216  5500-WRITE-TEST.
05217
05218      IF  W-RG-NDX NOT LESS THAN PI-TOTAL-LINES
05219          IF  LT-TEXT-RECORD NOT EQUAL SPACES
05220              ADD +1              TO PI-689-NUMBER-TEXT-RECORDS
05221              MOVE '2'            TO LT-RECORD-TYPE
05222              ADD +1              TO W-SEQ-CTR
05223              MOVE W-SEQ-CTR      TO LT-LINE-SEQ-NO
05224              PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
05225          ELSE
05226              NEXT SENTENCE
05227      ELSE
05228          IF  LT-NDX EQUAL +20
05229              ADD +1              TO PI-689-NUMBER-TEXT-RECORDS
05230              MOVE '2'            TO LT-RECORD-TYPE
05231              ADD +1              TO W-SEQ-CTR
05232              MOVE W-SEQ-CTR      TO LT-LINE-SEQ-NO
05233              PERFORM 5655-WRITE-ARCHIVE-TEXT THRU 5655-EXIT
05234              SET LT-NDX          TO W-ZEROS
05235              PERFORM 5415-INITIALIZE-TEXT THRU 5415-EXIT.
05236
05237  5500-EXIT.
05238       EXIT.
05239                                  EJECT
05240  5505-FIND-USED-LINE.
05241
05242      ADD +1                      TO W-PRINT-CONTROL.
05243
05244  5505-EXIT.
05245       EXIT.
05246                                  EJECT
05247  5600-FORMAT-ADDRESS-LINE.
05248
05249      SET LT-NDX                  TO W-RG-NDX.
05250      MOVE W-RC-TEXT (W-RG-NDX)   TO LT-TEXT-LINE (LT-NDX).
05251
05252  5600-EXIT.
05253      EXIT.
05254                                  EJECT
05255  5650-WRITE-ARCHIVE.
05256
05257      
      * EXEC CICS WRITE
05258 *         DATASET   (W-ARCH-FILE-ID)
05259 *         FROM      (LETTER-ARCHIVE)
05260 *         RIDFLD    (LA-CONTROL-PRIMARY)
05261 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012208' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 LA-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05262
05263  5650-EXIT.
05264      EXIT.
05265                                  EJECT
05266  5655-WRITE-ARCHIVE-TEXT.
05267
05268      
      * EXEC CICS WRITE
05269 *         DATASET   (W-ARCT-FILE-ID)
05270 *         FROM      (LETTER-ARCHIVE-TEXT)
05271 *         RIDFLD    (LT-CONTROL-PRIMARY)
05272 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE-TEXT
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00012219' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323139' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCT-FILE-ID, 
                 LETTER-ARCHIVE-TEXT, 
                 DFHEIV11, 
                 LT-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05273
05274  5655-EXIT.
05275      EXIT.
05276                                  EJECT
05277  6000-RESOLVE-VARIABLES.
05278 ***************************************************************
05279 *    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *
05280 *    WITH DATA PERTAINING TO THE FORM WITH VARIABLES.         *
05281 *    THIS ROUTINE IS PERFORM THRU 6000-EXIT IN ORDER TO       *
05282 *    RESOLVE ALL OF THE SYMBOLS.                              *
05283 *                                                             *
05284 ***************************************************************
05285
05289      PERFORM 6200-GET-COMPANY-DATA THRU 6200-EXIT.
05290      PERFORM 6250-GET-CNTL2-DATA THRU 6250-EXIT.
05291      PERFORM 6400-GET-PENDING-DATA THRU 6400-EXIT.
05292      PERFORM 6450-GET-CERT-DATA THRU 6450-EXIT.
05293      PERFORM 6500-GET-ACCOUNT-DATA THRU 6500-EXIT
072308*    PERFORM 6350-GET-CSR-DATA   THRU 6350-EXIT
05294      PERFORM 6600-GET-COMPENSATION-DATA THRU 6600-EXIT.
05295      PERFORM 6700-GET-MAIL-DATA THRU 6700-EXIT.
05296      PERFORM 6300-GET-CARRIER-DATA THRU 6300-EXIT.
05297      PERFORM 6750-GET-LIFE-BENEFIT-DATA THRU 6750-EXIT.
05298      PERFORM 6800-GET-A-H-BENEFIT-DATA THRU 6800-EXIT.
05299      PERFORM 6850-GET-CHECK-DATA THRU 6850-EXIT.
05300      PERFORM 6900-GET-PYAJ-DATA THRU 6900-EXIT.
05301      PERFORM 6950-MOVE-SYSTEM-DATA THRU 6950-EXIT.
05302
05303      MOVE SPACES                 TO W-REVERSE-DATE-SW.
05304
05305  6000-EXIT.
05306      EXIT.
05307                                  EJECT
05308  6200-GET-COMPANY-DATA.
05309
05310      IF  ADDRSI EQUAL '3'
05311              AND
05312          PI-CREATE-LABELS
05313              AND
05314          NOT PI-689-LABELS-OVERRIDEN
05315          NEXT SENTENCE
05316      ELSE
05317          IF  W-FILE-NOT-USED (1)
05318              GO TO 6200-EXIT.
05319
05320      MOVE SPACES                 TO W-CNTL-KEY.
05321      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
05322      MOVE +0                     TO W-CNTL-SEQ-NO.
05323      MOVE '1'                    TO W-CNTL-RECORD-TYPE.
05324
05325      
      * EXEC CICS HANDLE CONDITION
05326 *         NOTOPEN   (8040-CNTL-NOT-OPEN)
05327 *         NOTFND    (6200-CNTL1-NOT-FOUND)
05328 *    END-EXEC.
      *    MOVE '"$JI                  ! + #00012274' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2B20233030303132323734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05329
05330      
      * EXEC CICS READ
05331 *         DATASET   (W-CNTL-FILE-ID)
05332 *         SET       (ADDRESS OF CONTROL-FILE)
05333 *         RIDFLD    (W-CNTL-KEY)
05334 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012279' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132323739' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05335
05336      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
05337      MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (1)
05338                                     W-VG-TEXT (25).
05339      MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (2).
05340      MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (3).
05341      MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (4).
05342      MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (5).
05343
05344      IF  CF-CL-CAN-POST-CODE
05345          MOVE CF-CL-CANADIAN-POSTAL-CODE
05346                                  TO W-CANADIAN-POSTAL-CODES
05347          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
05348          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
05349          MOVE SPACES             TO W-LAB-CAN-DASH (6)
05350                                     W-LAB-CAN-FILLER (6)
05351          GO TO 6200-CONTINUE.
05352
05353      IF CF-CL-ZIP-CODE NOT = SPACES AND ZEROS AND LOW-VALUES
05354          MOVE CF-CL-ZIP-CODE     TO W-WORK-ZIP
05355          MOVE SPACES             TO W-LABEL-ZIP (6)
05356          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
05357
05358          IF  W-WORK-ZIP4 GREATER THAN '0000'
05359              MOVE '-'            TO W-LABEL-DASH (6)
05360              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
05361              GO TO 6200-CONTINUE
05362          ELSE
05363              MOVE SPACES         TO W-LABEL-DASH (6)
05364                                     W-LABEL-2ND-ZIP (6)
05365              GO TO 6200-CONTINUE.
05366
05367      IF  CF-CL-ZIP-CODE-NUM NOT NUMERIC
05368          GO TO 6200-CONTINUE.
05369
05370      MOVE CF-CL-ZIP-CODE-NUM     TO W-WORK-ZIP-NUMERIC.
05371
05372      IF  W-WORK-ZIP-NUMERIC LESS THAN +100000
05373          COMPUTE W-WORK-ZIP-NUMERIC
05374              = W-WORK-ZIP-NUMERIC * 10000.
05375
05376      MOVE SPACES                 TO W-LABEL-ZIP (6).
05377      MOVE W-WORK-ZIP5            TO W-LABEL-1ST-ZIP (6).
05378
05379      IF  W-WORK-ZIP4 GREATER THAN '0000'
05380          MOVE '-'                TO W-LABEL-DASH (6)
05381          MOVE W-WORK-ZIP4        TO W-LABEL-2ND-ZIP (6)
05382          GO TO 6200-CONTINUE
05383      ELSE
05384          MOVE SPACES             TO W-LABEL-DASH (6)
05385                                     W-LABEL-2ND-ZIP (6)
05386          GO TO 6200-CONTINUE.
05387
05388  6200-CONTINUE.
05389
05390      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
05391
05392      IF  ADDRSI EQUAL '3'
05393              AND
05394          PI-CREATE-LABELS
05395              AND
05396          NOT PI-689-LABELS-OVERRIDEN
05397          MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1) W-VG-TEXT (1)
05398          MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2) W-VG-TEXT (2)
05399          MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3) W-VG-TEXT (3)
05400          MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4) W-VG-TEXT (04)
05401          MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5) W-VG-TEXT (5)
05402          MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6) W-VG-TEXT (6)
05403          MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
05404          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
05405          SET W-RG-NDX UP BY +1
05406      ELSE
05407          MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (1)
05408          MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (2)
05409          MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (3)
05410          MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (4)
05411          MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (5)
05412          MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (6).
05413
05414      GO TO 6200-EXIT.
05415
05416  6200-CNTL1-NOT-FOUND.
05417
05418      MOVE ER-9299                TO EMI-ERROR.
05419      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05420      MOVE -1                     TO ADDRSL.
05421      MOVE AL-UANON               TO ADDRSA.
05422      GO TO 8200-SEND-DATAONLY.
05423
05424  6200-EXIT.
05425      EXIT.
05426                                  EJECT
05427  6250-GET-CNTL2-DATA.
05428
05429      IF  PI-PROCESSOR-ID EQUAL W-LGXX-ID
05430          GO TO 6250-EXIT.
05431
05432      IF  W-FILE-NOT-USED (11)
05433          GO TO 6250-EXIT.
05434
05435      MOVE SPACES                 TO W-CNTL-KEY.
05436      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
05437      MOVE +0                     TO W-CNTL-SEQ-NO.
05438      MOVE '2'                    TO W-CNTL-RECORD-TYPE.
05439      MOVE PI-PROCESSOR-ID        TO W-CNTL-GENL.
05440
05441      
      * EXEC CICS HANDLE CONDITION
05442 *         NOTOPEN    (8040-CNTL-NOT-OPEN)
05443 *         NOTFND     (6250-CNTL2-NOT-FOUND)
05444 *    END-EXEC.
      *    MOVE '"$JI                  ! , #00012390' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2C20233030303132333930' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05445
05446      
      * EXEC CICS READ
05447 *         DATASET    (W-CNTL-FILE-ID)
05448 *         SET        (ADDRESS OF CONTROL-FILE)
05449 *         RIDFLD     (W-CNTL-KEY)
05450 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012395' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132333935' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05451
05452      MOVE CF-PROCESSOR           TO W-VG-TEXT (153).
05453      MOVE CF-PROCESSOR-NAME      TO W-VG-TEXT (151).
05454      MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (152).
05455      GO TO 6250-EXIT.
05456
05457  6250-CNTL2-NOT-FOUND.
05458
05459      MOVE ER-9299                TO EMI-ERROR.
05460      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05461      MOVE -1                     TO MAINTL.
05462      MOVE AL-UANON               TO MAINTA.
05463      GO TO 8200-SEND-DATAONLY.
05464
05465  6250-EXIT.
05466      EXIT.
05467                                  EJECT
05468  6300-GET-CARRIER-DATA.
05469
05470      IF  ADDRSI EQUAL '2'
05471              AND
05472          PI-CREATE-LABELS
05473              AND
05474          NOT PI-689-LABELS-OVERRIDEN
05475          NEXT SENTENCE
05476      ELSE
05477          IF  W-FILE-NOT-USED (4)
05478              GO TO 6300-EXIT.
05479
05480      IF  PI-689-CARRIER GREATER THAN LOW-VALUES
05481          MOVE SPACES             TO W-CNTL-KEY
05482          MOVE PI-COMPANY-ID      TO W-CNTL-COMPANY-ID
05483          MOVE +0                 TO W-CNTL-SEQ-NO
05484          MOVE '6'                TO W-CNTL-RECORD-TYPE
05485          MOVE PI-689-CARRIER     TO W-CNTL-GEN4
05486      ELSE
05487          MOVE ER-9327            TO EMI-ERROR
05488          MOVE -1                 TO CARRIERL
05489          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05490          GO TO 6300-EXIT.
05491
05492      
      * EXEC CICS HANDLE CONDITION
05493 *         NOTOPEN    (8040-CNTL-NOT-OPEN)
05494 *         NOTFND     (6300-CNTL6-NOT-FOUND)
05495 *    END-EXEC.
      *    MOVE '"$JI                  ! - #00012441' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2D20233030303132343431' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05496
05497      
      * EXEC CICS READ
05498 *         DATASET    (W-CNTL-FILE-ID)
05499 *         SET        (ADDRESS OF CONTROL-FILE)
05500 *         RIDFLD     (W-CNTL-KEY)
05501 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012446' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132343436' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05502
05503      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
05504      MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (1).
05505      MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (2).
05506      MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (3).
05507      MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (4).
05508      MOVE CF-CITY-STATE          TO W-LABEL-LINES (5).
05509
05510      IF  CF-CANADIAN-POST-CODE
05511          MOVE CF-CANADIAN-POSTAL-CODE
05512                                  TO W-CANADIAN-POSTAL-CODES
05513          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
05514          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
05515          MOVE SPACES             TO W-LAB-CAN-DASH (6)
05516                                     W-LAB-CAN-FILLER (6)
05517          GO TO 6300-CONTINUE.
05518
05519      IF CF-ZIP-CODE NOT = SPACES AND ZEROS AND LOW-VALUES
05520          MOVE CF-ZIP-CODE        TO W-WORK-ZIP
05521          MOVE SPACES             TO W-LABEL-ZIP (6)
05522          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
05523          IF  W-WORK-ZIP4 GREATER THAN '0000'
05524              MOVE '-'            TO W-LABEL-DASH (6)
05525              MOVE W-WORK-ZIP4
05526                                  TO W-LABEL-2ND-ZIP (6)
05527              GO TO 6300-CONTINUE
05528          ELSE
05529              MOVE SPACES         TO W-LABEL-DASH (6)
05530                                     W-LABEL-2ND-ZIP (6)
05531              GO TO 6300-CONTINUE.
05532
05533      IF  CF-ZIP-CODE-NUM NOT NUMERIC
05534          GO TO 6300-CONTINUE.
05535
05536      MOVE CF-ZIP-CODE-NUM TO W-WORK-ZIP-NUMERIC.
05537
05538      IF  W-WORK-ZIP-NUMERIC LESS THAN +100000
05539          COMPUTE W-WORK-ZIP-NUMERIC
05540              = W-WORK-ZIP-NUMERIC * 10000.
05541
05542      MOVE SPACES                 TO W-LABEL-ZIP (6).
05543      MOVE W-WORK-ZIP5            TO W-LABEL-1ST-ZIP (6).
05544
05545      IF  W-WORK-ZIP4 GREATER THAN '0000'
05546          MOVE '-'                TO W-LABEL-DASH (6)
05547          MOVE W-WORK-ZIP4        TO W-LABEL-2ND-ZIP (6)
05548          GO TO 6300-CONTINUE
05549      ELSE
05550          MOVE SPACES             TO W-LABEL-DASH (6)
05551                                     W-LABEL-2ND-ZIP (6)
05552          GO TO 6300-CONTINUE.
05553
05554  6300-CONTINUE.
05555
05556      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
05557
05558      IF  ADDRSI EQUAL '2'
05559              AND
05560          PI-CREATE-LABELS
05561              AND
05562          NOT PI-689-LABELS-OVERRIDEN
05563          MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1) W-VG-TEXT (20)
05564          MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2) W-VG-TEXT (21)
05565          MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3) W-VG-TEXT (22)
05566          MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4) W-VG-TEXT (23)
05567          MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5) W-VG-TEXT (24)
05568          MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6) W-VG-TEXT (25)
05569          MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
05570          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
05571          SET W-RG-NDX UP BY +1
05572      ELSE
05573          MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (20)
05574          MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (21)
05575          MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (22)
05576          MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (23)
05577          MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (24)
05578          MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (25).
05579
05580      IF  W-FULL-DATA
05581          MOVE ZEROS              TO W-PHONE-IN
05582          MOVE CF-PHONE-NO        TO W-PHONE-IN
05583          MOVE W-PHI-AREA         TO W-PO-AREA
05584          MOVE W-PHI-PFX          TO W-PO-PFX
05585          MOVE W-PHI-SFX          TO W-PO-SFX
05586          MOVE W-PHONE-OUT        TO W-VG-TEXT (26).
05587
05588      GO TO 6300-EXIT.
05589
05590  6300-CNTL6-NOT-FOUND.
05591
05592      MOVE ER-9298                TO EMI-ERROR.
05593      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
05594      MOVE -1                     TO ADDRSL.
05595      MOVE AL-UANON               TO ADDRSA.
05596      GO TO 8200-SEND-DATAONLY.
05597
05598  6300-EXIT.
05599      EXIT.
05600                                  EJECT
072308 6350-GET-CSR-DATA.
072308     IF W-VG-TEXT (148) (1:4) = '****' OR SPACES
072308        GO TO 6350-EXIT
072308     END-IF
072308*    IF W-FILE-NOT-USED (11)
072308*       GO TO 6350-EXIT
072308*    END-IF
072308     MOVE SPACES                 TO W-CNTL-KEY
072308     MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID
072308     MOVE +0                     TO W-CNTL-SEQ-NO
072308     MOVE '2'                    TO W-CNTL-RECORD-TYPE
092908     MOVE CO-CSR-CODE            TO W-CNTL-GENL
072308     
      * EXEC CICS HANDLE CONDITION
072308*         NOTOPEN    (8040-CNTL-NOT-OPEN)
072308*         NOTFND     (6350-CNTL2-NOT-FOUND)
072308*    END-EXEC
      *    MOVE '"$JI                  ! . #00012562' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2E20233030303132353632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072308     
      * EXEC CICS READ
072308*         DATASET    (W-CNTL-FILE-ID)
072308*         SET        (ADDRESS OF CONTROL-FILE)
072308*         RIDFLD     (W-CNTL-KEY)
072308*    END-EXEC
      *    MOVE '&"S        E          (   #00012566' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132353636' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
072308     MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (154)
072308     GO TO 6350-EXIT
           .
072308 6350-CNTL2-NOT-FOUND.
072308     MOVE ER-9299                TO EMI-ERROR.
072308     PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
072308     MOVE -1                     TO MAINTL.
072308     MOVE AL-UANON               TO MAINTA.
072308     GO TO 8200-SEND-DATAONLY.
072308 6350-EXIT.
072308     EXIT.
05601  6400-GET-PENDING-DATA.
05602
05603      IF (ADDRSI EQUAL '7')
05604         AND (PI-CREATE-LABELS)
05606         AND (NOT PI-689-LABELS-OVERRIDEN)
05608         CONTINUE
05609      ELSE
              IF PI-689-DATA-SOURCE = '4'
                 CONTINUE
              ELSE
05610            IF W-FILE-NOT-USED (9)
05611               GO TO 6400-EXIT
                 END-IF
              END-IF
           END-IF
05613      IF  W-PNDB-ENTRY EQUAL LOW-VALUES
05614          MOVE ER-9327            TO EMI-ERROR
05615          MOVE -1                 TO MAINTL
05616          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05617          GO TO 6400-EXIT.
05618
05619  6400-READ-PNDB-ONLY.
05620
05621      
      * EXEC CICS HANDLE CONDITION
05622 *         NOTOPEN (8060-PNDB-NOT-OPEN)
05623 *         NOTFND  (8070-PNDB-NOT-FOUND)
05624 *    END-EXEC.
      *    MOVE '"$JI                  ! / #00012605' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2F20233030303132363035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05625
05626      
      * EXEC CICS READ
05627 *         DATASET (W-PNDB-FILE-ID)
05628 *         SET     (ADDRESS OF PENDING-BUSINESS)
05629 *         RIDFLD  (W-PNDB-KEY)
05630 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012610' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132363130' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PNDB-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05631
05632      MOVE 'Y'                    TO W-PNDB-FOUND-SW.
041320     move pb-record-type         to pi-iss-can-pend-rec
05633
05634      IF  W-CERT-CERT-PRIME EQUAL LOW-VALUES
05635          MOVE PB-SV-CARRIER      TO PB-CARRIER
05636                                     PI-689-CARRIER
05637          MOVE PB-SV-GROUPING     TO PB-GROUPING
05638                                     PI-689-GROUPING
05639          MOVE PB-SV-STATE        TO PB-STATE
05640                                     PI-689-STATE
05641          MOVE PB-CONTROL-BY-ACCOUNT
05642                                  TO W-CERT-KEY
05643                                     W-MAIL-KEY
05644                                     W-ACCT-KEY
05645          MOVE PB-ACCOUNT         TO PI-689-ACCOUNT
05646          MOVE PB-CERT-EFF-DT     TO PI-689-EFF-DATE
05647          MOVE PB-CERT-NO         TO PI-689-CERT-NO.
05648
05649  6400-ONLY-STOP.
05650
05651      MOVE PB-ENTRY-BATCH         TO W-VG-TEXT (130).
05652
05653      IF  NOT PB-ISSUE
05654          GO TO 6400-CANCEL.
05655
05656      MOVE PB-I-BIRTHDAY          TO DC-BIN-DATE-1.
05657      MOVE SPACES                 TO DC-OPTION-CODE.
05658      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05659
05660      IF  NO-CONVERSION-ERROR
05661          MOVE DC-GREG-DATE-1-EDIT
05662                                  TO W-VG-TEXT (110).
05663
05664      MOVE PB-I-AGE               TO W-EDIT-3-0.
05665      MOVE W-EDIT-3-0             TO W-VG-TEXT (124).
05666      MOVE PB-I-LF-PREMIUM-AMT    TO W-EDIT-7-2.
05667      MOVE W-EDIT-7-2             TO W-VG-TEXT (111).
05668      MOVE PB-I-AH-PREMIUM-AMT    TO W-EDIT-7-2.
05669      MOVE W-EDIT-7-2             TO W-VG-TEXT (112).
05670      MOVE PB-I-LF-PREM-CALC      TO W-EDIT-7-2.
05671      MOVE W-EDIT-7-2             TO W-VG-TEXT (113).
05672      MOVE PB-I-AH-PREM-CALC      TO W-EDIT-7-2.
05673      MOVE W-EDIT-7-2             TO W-VG-TEXT (114).
05674      MOVE PB-I-LF-BENEFIT-AMT    TO W-EDIT-9-2.
05675      MOVE W-EDIT-9-2             TO W-VG-TEXT (125).
05676      MOVE PB-I-AH-BENEFIT-AMT    TO W-EDIT-7-2.
05677      MOVE W-EDIT-7-2             TO W-VG-TEXT (126).
05678      MOVE PB-I-LF-RATE           TO W-EDIT-2-5-S.
05679      MOVE W-EDIT-2-5-S           TO W-VG-TEXT (127).
05680      MOVE PB-I-AH-RATE           TO W-EDIT-2-5-S.
05681      MOVE W-EDIT-2-5-S           TO W-VG-TEXT (128).
05682      MOVE PB-I-LOAN-TERM         TO W-EDIT-3-0.
05683      MOVE W-EDIT-3-0             TO W-VG-TEXT (129).
05684
05685      COMPUTE W-DIFFERENCE
05686          = PB-I-LF-PREM-CALC - PB-I-LF-PREMIUM-AMT.
05687
05688      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05689      MOVE W-EDIT-7-2             TO W-VG-TEXT (115).
05690
05691      COMPUTE W-DIFFERENCE
05692          = PB-I-AH-PREM-CALC - PB-I-AH-PREMIUM-AMT.
05693
05694      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05695      MOVE W-EDIT-7-2             TO W-VG-TEXT (116).
05696
05697  6400-CANCEL.
05698
05699      IF  NOT PB-CANCELLATION
05700          GO TO 6400-EXIT.
05701
05702      MOVE PB-CI-AH-PRIOR-CANCEL-DT
05703                                  TO DC-BIN-DATE-1.
05704      MOVE SPACES                 TO DC-OPTION-CODE.
05705      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05706
05707      IF  NO-CONVERSION-ERROR
05708          MOVE DC-GREG-DATE-1-EDIT
05709                                  TO W-VG-TEXT (117).
05710
05711      MOVE PB-C-LF-CANCEL-AMT     TO W-EDIT-7-2.
05712      MOVE W-EDIT-7-2             TO W-VG-TEXT (118).
05713      MOVE PB-C-AH-CANCEL-AMT     TO W-EDIT-7-2.
05714      MOVE W-EDIT-7-2             TO W-VG-TEXT (119).
05715      MOVE PB-C-LF-REF-CALC       TO W-EDIT-7-2.
05716      MOVE W-EDIT-7-2             TO W-VG-TEXT (120).
05717      MOVE PB-C-AH-REF-CALC       TO W-EDIT-7-2.
05718      MOVE W-EDIT-7-2             TO W-VG-TEXT (121).
072908     COMPUTE W-EDIT-7-2 = PB-C-LF-CANCEL-AMT + PB-C-AH-CANCEL-AMT
072908     MOVE W-EDIT-7-2             TO W-VG-TEXT (131)
05719
062017     IF PB-C-INT-ON-REFS NOT NUMERIC
062017        MOVE ZEROS               TO PB-C-INT-ON-REFS
072908     END-IF
062017     MOVE PB-C-INT-ON-REFS       TO W-EDIT-7-2
072908     MOVE W-EDIT-7-2             TO W-VG-TEXT (132)
072908     MOVE PB-C-LF-CANCEL-DT      TO DC-BIN-DATE-1
072908     IF PB-C-AH-CANCEL-DT > DC-BIN-DATE-1
072908        MOVE PB-C-AH-CANCEL-DT   TO DC-BIN-DATE-1
072908     END-IF
072908     MOVE SPACES                 TO DC-OPTION-CODE
072908     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
05706
072908     IF NO-CONVERSION-ERROR
072908        MOVE DC-GREG-DATE-1-EDIT TO W-VG-TEXT (133)
072908     END-IF
05710
05720      COMPUTE W-DIFFERENCE = PB-C-LF-REF-CALC
05721                            - PB-C-LF-CANCEL-AMT.
05722
05723      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05724      MOVE W-EDIT-7-2             TO W-VG-TEXT (122).
05725
05726      COMPUTE W-DIFFERENCE = PB-C-AH-REF-CALC
05727                            - PB-C-AH-CANCEL-AMT.
05728
05729      MOVE W-DIFFERENCE           TO W-EDIT-7-2.
05730      MOVE W-EDIT-7-2             TO W-VG-TEXT (123).
05731
05732  6400-EXIT.
05733      EXIT.
05734                                  EJECT
05735  6450-GET-CERT-DATA.
05736      IF  W-FILE-NOT-USED (8)
05737          GO TO 6450-EXIT.
05738
05739      IF  W-CERT-CERT-PRIME EQUAL LOW-VALUES
05740          MOVE ER-9327            TO EMI-ERROR
05741          MOVE -1                 TO MAINTL
05742          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05743          GO TO 6450-EXIT.
05744
05745  6450-READ-CERT-ONLY.
05746
05747      
      * EXEC CICS HANDLE CONDITION
05748 *         NOTOPEN (8030-CERT-NOT-OPEN)
05749 *         NOTFND  (8035-CERT-NOT-FOUND)
05750 *    END-EXEC.
      *    MOVE '"$JI                  ! 0 #00012750' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3020233030303132373530' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05751
05752      
      * EXEC CICS READ
05753 *         DATASET (W-CERT-FILE-ID)
05754 *         SET     (ADDRESS OF CERTIFICATE-MASTER)
05755 *         RIDFLD  (W-CERT-KEY)
05756 *    END-EXEC.
      *    MOVE '&"S        E          (   #00012755' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303132373535' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CERT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CERT-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CERTIFICATE-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05757
05758      MOVE 'Y'                    TO W-CERT-FOUND-SW.
05759
05760  6450-ONLY-STOP.
05761
05762      MOVE CM-INSURED-LAST-NAME   TO W-VG-TEXT (108).
05763
05764      MOVE CM-BENEFICIARY         TO W-VG-TEXT (109).
05765
05766      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05767      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05768      MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
05769      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05770      MOVE WS-NAME-WORK           TO W-VG-TEXT (95).
05771
05772      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05773      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05774      MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
05775      PERFORM 7100-FORMAT-LAST-NAME-1ST THRU 7100-EXIT.
05776      MOVE WS-NAME-WORK           TO W-VG-TEXT (94).
05777
05778      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05779      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05780      MOVE '.'                    TO W-NAME-FIRST-REMAIN.
05781      MOVE CM-INSURED-INITIAL2    TO W-NAME-MIDDLE.
05782      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05783      MOVE WS-NAME-WORK           TO W-VG-TEXT (90).
05784
05785      MOVE CM-INSURED-LAST-NAME   TO W-NAME-LAST.
05786      MOVE CM-INSURED-FIRST-NAME  TO W-NAME-FIRST.
05787      MOVE SPACES                 TO W-NAME-MIDDLE.
05788      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05789      MOVE WS-NAME-WORK           TO W-VG-TEXT (104).
05790
05791      MOVE CM-INSURED-FIRST-NAME  TO W-VG-TEXT (91).
05792      MOVE CM-INSURED-INITIAL2    TO W-VG-TEXT (92).
05793
05794      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05795      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05796      MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
05797      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05798      MOVE WS-NAME-WORK           TO W-VG-TEXT (103)
05799                                     W-LABEL-JOINT-NAME.
05800
05801      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05802      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05803      MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
05804      PERFORM 7100-FORMAT-LAST-NAME-1ST THRU 7100-EXIT.
05805      MOVE WS-NAME-WORK           TO W-VG-TEXT (102).
05806
05807      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05808      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05809      MOVE '.'                    TO W-NAME-FIRST-REMAIN.
05810      MOVE CM-JT-INITIAL          TO W-NAME-MIDDLE.
05811      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05812      MOVE WS-NAME-WORK           TO W-VG-TEXT (99).
05813
05814      MOVE CM-JT-LAST-NAME        TO W-NAME-LAST.
05815      MOVE CM-JT-FIRST-NAME       TO W-NAME-FIRST.
05816      MOVE SPACES                 TO W-NAME-MIDDLE.
05817      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
05818      MOVE WS-NAME-WORK           TO W-VG-TEXT (105).
05819
05820      MOVE CM-JT-FIRST-NAME       TO W-VG-TEXT (100).
05821      MOVE CM-JT-INITIAL          TO W-VG-TEXT (101).
05822
05823      COMPUTE W-WORK-AMOUNT
05824          = CM-LF-PREMIUM-AMT + CM-LF-ALT-PREMIUM-AMT.
05825      MOVE W-WORK-AMOUNT          TO W-EDIT-9-2.
05826      MOVE W-EDIT-9-2             TO W-VG-TEXT (97).
05827      MOVE CM-AH-PREMIUM-AMT      TO W-EDIT-7-2.
05828      MOVE W-EDIT-7-2             TO W-VG-TEXT (98).
05829
05830      IF  CM-SEX-FEMAL
05831          MOVE 'MS.'              TO W-VG-TEXT (96)
05832
05833      ELSE
05834          MOVE 'MR.'              TO W-VG-TEXT (96).
05835
05836      MOVE CM-CARRIER OF CERTIFICATE-MASTER
05837                                  TO W-VG-TEXT (70).
05838      MOVE CM-GROUPING            TO W-VG-TEXT (71).
05839      MOVE CM-ACCOUNT             TO W-VG-TEXT (72).
05840      MOVE CM-CERT-NO             TO W-VG-TEXT (73).
05841      MOVE CM-INSURED-ISSUE-AGE   TO W-DISPLAY-3.
05842      MOVE W-DISPLAY-3            TO W-VG-TEXT (83).
05843      MOVE CM-LOAN-NUMBER         TO W-VG-TEXT (86).
05844      MOVE CM-LOAN-BALANCE        TO W-EDIT-7-2.
05845      MOVE W-EDIT-7-2             TO W-VG-TEXT (87).
05846      MOVE CM-MEMBER-NO           TO W-VG-TEXT (88).
05847      MOVE CM-SOC-SEC-NO          TO W-VG-TEXT (89).
05848      MOVE CM-POLICY-FORM-NO      TO W-VG-TEXT (83).
05849      MOVE CM-LF-ORIG-TERM        TO W-EDIT-3-0.
05850      MOVE W-EDIT-3-0             TO W-VG-TEXT (77).
05851      MOVE CM-LF-BENEFIT-AMT      TO W-EDIT-9-2.
05852      MOVE W-EDIT-9-2             TO W-VG-TEXT (79).
05853      MOVE CM-AH-ORIG-TERM        TO W-EDIT-3-0.
05854      MOVE W-EDIT-3-0             TO W-VG-TEXT (78).
05855      MOVE CM-AH-BENEFIT-AMT      TO W-EDIT-7-2.
05856      MOVE W-EDIT-7-2             TO W-VG-TEXT (80).
05857      MOVE CM-AH-ITD-CANCEL-AMT   TO W-EDIT-7-2.
05858      MOVE W-EDIT-7-2             TO W-VG-TEXT (107).
05859      MOVE CM-LF-ITD-CANCEL-AMT   TO W-EDIT-7-2.
05860      MOVE W-EDIT-7-2             TO W-VG-TEXT (106).
           IF PI-689-DATA-SOURCE NOT = '4'
102408     COMPUTE W-EDIT-7-2 = CM-LF-ITD-CANCEL-AMT
102408        + CM-AH-ITD-CANCEL-AMT
102408     MOVE W-EDIT-7-2             TO W-VG-TEXT (131)
062017     IF CM-INT-ON-REFS NOT NUMERIC
062017        MOVE ZEROS               TO CM-INT-ON-REFS
102408     END-IF
062017     MOVE CM-INT-ON-REFS         TO W-EDIT-7-2
102408     MOVE W-EDIT-7-2             TO W-VG-TEXT (132)
102408     MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1
102408     IF CM-AH-CANCEL-DT > DC-BIN-DATE-1
102408        MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-1
102408     END-IF
102408     MOVE SPACES                 TO DC-OPTION-CODE
102408     PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT
102408
102408     IF NO-CONVERSION-ERROR
102408        MOVE DC-GREG-DATE-1-EDIT TO W-VG-TEXT (133)
102408     END-IF
           END-IF
05862      COMPUTE W-WORK-AMOUNT = CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT.
05863      MOVE W-WORK-AMOUNT          TO W-EDIT-9-2.
05864      MOVE W-EDIT-9-2             TO W-VG-TEXT (93).
05865
05866      MOVE CM-LF-CANCEL-DT        TO DC-BIN-DATE-1.
05867      MOVE SPACES                 TO DC-OPTION-CODE.
05868      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05869
05870      IF  NO-CONVERSION-ERROR
05871          MOVE DC-GREG-DATE-1-EDIT
05872                                  TO W-VG-TEXT (81).
05873
05874      MOVE CM-AH-CANCEL-DT        TO DC-BIN-DATE-1.
05875      MOVE SPACES                 TO DC-OPTION-CODE.
05876      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05877
05878      IF  NO-CONVERSION-ERROR
05879          MOVE DC-GREG-DATE-1-EDIT
05880                                  TO W-VG-TEXT (82).
05881
05882      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
05883      MOVE SPACES                 TO DC-OPTION-CODE.
05884      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05885
05886      IF  NO-CONVERSION-ERROR
05887          MOVE DC-GREG-DATE-1-EDIT
05888                                  TO W-VG-TEXT (74).
05889
05890      MOVE CM-LF-ORIG-TERM        TO DC-ELAPSED-MONTHS.
05891
05892      IF  CM-PMT-EXTENSION-DAYS NUMERIC
05893          MOVE CM-PMT-EXTENSION-DAYS
05894                                  TO DC-ELAPSED-DAYS
05895      ELSE
05896          MOVE ZEROS              TO DC-ELAPSED-DAYS.
05897
05898      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
05899      MOVE '6'                    TO DC-OPTION-CODE.
05900      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05901
05902      IF  NO-CONVERSION-ERROR
05903          MOVE DC-GREG-DATE-1-EDIT
05904                                  TO W-VG-TEXT (75).
05905
05906      MOVE CM-AH-ORIG-TERM        TO DC-ELAPSED-MONTHS.
05907      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.
05908      MOVE '6'                    TO DC-OPTION-CODE.
05909      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
05910
05911      IF  NO-CONVERSION-ERROR
05912          MOVE DC-GREG-DATE-1-EDIT
05913                                  TO W-VG-TEXT (76).
05914
05915  6450-EXIT.
05916      EXIT.
05917                                  EJECT
05918  6500-GET-ACCOUNT-DATA.
05919
05920      IF  ADDRSI EQUAL '1'
05921              AND
05922          PI-CREATE-LABELS
05923              AND
05924          NOT PI-689-LABELS-OVERRIDEN
05925          NEXT SENTENCE
05926      ELSE
05927          IF  W-FILE-NOT-USED (6)
05928              GO TO 6500-EXIT
031504         END-IF
031504     END-IF.
05929
05930  6500-READ-ACCT-ONLY.
05931
05932      IF  W-ACCT-ACCOUNT EQUAL LOW-VALUES
05933          MOVE ER-9327            TO EMI-ERROR
05934          MOVE -1                 TO MAINTL
05935          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05936          GO TO 6500-EXIT.
05937
05938      
      * EXEC CICS HANDLE CONDITION
05939 *         NOTOPEN    (8020-ACCT-NOT-OPEN)
05940 *         NOTFND     (6500-ACCT-NOT-FOUND)
022510*         ENDFILE    (6500-ONLY-STOP)
05941 *    END-EXEC.
      *    MOVE '"$JI''                 ! 1 #00012962' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3120233030303132393632' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05942
05943      
      * EXEC CICS STARTBR
05944 *         RIDFLD     (W-ACCT-KEY)
05945 *         DATASET    (W-ACCT-FILE-ID)
05946 *         KEYLENGTH  (20)
05947 *         GENERIC
05948 *    END-EXEC.
           MOVE 20
             TO DFHEIV11
           MOVE 0
             TO DFHEIV12
      *    MOVE '&,   KG    G          &   #00012968' TO DFHEIV0
           MOVE X'262C2020204B472020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303132393638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-FILE-ID, 
                 W-ACCT-KEY, 
                 DFHEIV11, 
                 DFHEIV12, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05949
05950      MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.
05951      MOVE 'Y'                    TO W-ACCT-BROWSE-STARTED-SW.
05952
05953  6500-READNEXT.
05954
05955      
      * EXEC CICS READNEXT
05956 *         DATASET    (W-ACCT-FILE-ID)
05957 *         SET        (ADDRESS OF ACCOUNT-MASTER)
05958 *         RIDFLD     (W-ACCT-KEY)
05959 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00012980' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303132393830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ACCT-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF ACCOUNT-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
05960
05961      IF  W-ACCT-PARTIAL-KEY NOT EQUAL W-ACCT-SAVE-KEY
031504         IF  PRIOR-MATCH-ACCT-PKEY
031504             CONTINUE
031504*            GO TO 6500-ONLY-STOP
031504         ELSE
                   IF  W-FILE-NOT-USED (6)
031504                 PERFORM 6500-ENDBR
031504                 GO TO 6500-EXIT
                   ELSE
                       GO TO 6500-ACCT-NOT-FOUND
031504             END-IF
031504         END-IF
031504     ELSE
031504         SET PRIOR-MATCH-ACCT-PKEY TO TRUE
031504         MOVE AM-CARRIER           TO WS-SAV-AM-CARRIER
031504         MOVE AM-ACCOUNT           TO WS-SAV-AM-ACCOUNT
031504         MOVE AM-DEFN-1            TO WS-SAV-AM-DEFN-1
031504         MOVE AM-REMIT-TO          TO WS-SAV-AM-REMIT-TO
031504         MOVE AM-NAME              TO WS-SAV-AM-NAME
031504         MOVE AM-PERSON            TO WS-SAV-AM-PERSON
031504         MOVE AM-ADDRS             TO WS-SAV-AM-ADDRS
               MOVE SPACES               TO WS-SAV-AM-CITY
031504*        MOVE AM-CITY              TO WS-SAV-AM-CITY
               STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
                  DELIMITED BY '  ' INTO WS-SAV-AM-CITY
               END-STRING
031504         MOVE AM-ZIP               TO WS-SAV-AM-ZIP
031504         MOVE AM-TEL-NO            TO WS-SAV-AM-TEL-NO
031504         MOVE AM-CONTROL-NAME      TO WS-SAV-AM-CONTROL-NAME
072308         MOVE AM-CSR-CODE          TO WS-SAV-AM-CSR-CODE
051109         MOVE AM-ACCOUNT           TO WS-SAV-AM-ERACCT-ACCOUNT
031504         GO TO 6500-READNEXT
031504     END-IF
05976      .
05977  6500-ONLY-STOP.
05978
05979      IF  NOT PI-NO-CARRIER-SECURITY
05980              AND
031504         PI-CARRIER-SECURITY NOT = WS-SAV-AM-CARRIER
05982          MOVE -1                 TO CARRIERL
05983          MOVE AL-UANON           TO CARRIERA
05984          MOVE ER-9095            TO EMI-ERROR
05985          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
05986          PERFORM 6500-ENDBR
05987          GO TO 8200-SEND-DATAONLY.
05988
05989      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
031504     MOVE WS-SAV-AM-NAME         TO W-LABEL-LINES (1).
031504     MOVE WS-SAV-AM-PERSON       TO W-LABEL-LINES (2).
031504     MOVE WS-SAV-AM-ADDRS        TO W-LABEL-LINES (3).
031504     MOVE WS-SAV-AM-CITY         TO W-LABEL-LINES (4).
05994
031504     MOVE WS-SAV-AM-ZIP          TO W-WORK-ZIP.
05996      MOVE SPACES                 TO W-LABEL-ZIP (5).
05997
031504     IF  SAVE-AM-CANADIAN-POST-CODE
031504         MOVE WS-SAV-AM-CANADIAN-POSTAL-CODE
06000                                  TO W-CANADIAN-POSTAL-CODES
06001          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (5)
06002          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (5)
06003          MOVE SPACES             TO W-LAB-CAN-DASH (5)
06004                                     W-LAB-CAN-FILLER (5)
06005      ELSE
06006          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (5)
06007
06008          IF  W-WORK-ZIP4 GREATER THAN '0000'
06009              MOVE '-'            TO W-LABEL-DASH (5)
06010              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (5)
06011          ELSE
06012              MOVE SPACES         TO W-LABEL-DASH (5)
06013                                     W-LABEL-2ND-ZIP (5).
06014
06015      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06016
06017      IF  ADDRSI EQUAL '1'
06018              AND
06019          PI-CREATE-LABELS
06020              AND
06021          NOT PI-689-LABELS-OVERRIDEN
06022          MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
06023                                     W-VG-TEXT (40)
06024          MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
06025                                     W-VG-TEXT (41)
06026          MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
06027                                     W-VG-TEXT (42)
06028          MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
06029                                     W-VG-TEXT (43)
06030          MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
06031                                     W-VG-TEXT (44)
06032          MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
06033                                     W-VG-TEXT (45)
06034          MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
06035          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
06036          SET W-RG-NDX UP BY +1
06037      ELSE
06038          MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (40)
06039          MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (41)
06040          MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (42)
06041          MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (43)
06042          MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (44)
06043          MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (45).
06044
031504     IF  WS-SAV-AM-TEL-NO NOT NUMERIC
06046              OR
031504         WS-SAV-AM-TEL-NO EQUAL ZEROS
06048          MOVE SPACES             TO W-VG-TEXT (46)
06049      ELSE
06050          MOVE ZEROS                  TO W-PHONE-IN
031504         MOVE WS-SAV-AM-AREA-CODE    TO W-PO-AREA
031504         MOVE WS-SAV-AM-TEL-PRE      TO W-PO-PFX
031504         MOVE WS-SAV-AM-TEL-NBR      TO W-PO-SFX
06054          MOVE W-PHONE-OUT            TO W-VG-TEXT (46).
06055
031504     IF WS-SAV-AM-CONTROL-NAME (1:1) NOT = LOW-VALUES
031504        MOVE WS-SAV-AM-CONTROL-NAME  TO W-VG-TEXT (47)
PEMMOD     ELSE
PEMMOD        MOVE SPACES                   TO W-VG-TEXT (47)
PEMMOD     END-IF.
072308     MOVE WS-SAV-AM-CSR-CODE     TO W-VG-TEXT (48)
051109     MOVE WS-SAV-AM-ERACCT-ACCOUNT
051109                                 TO W-VG-TEXT (49)
PEMMOD
031504     MOVE WS-SAV-AM-ACCOUNT      TO W-VG-TEXT (72)
06058      PERFORM 6500-ENDBR.
06059
06060      GO TO 6500-EXIT.
06061
06062  6500-ENDBR.
06063
06064      IF  W-ACCT-BROWSE-STARTED
06065          MOVE 'N'                TO W-ACCT-BROWSE-STARTED-SW
06066          
      * EXEC CICS ENDBR
06067 *            DATASET (W-ACCT-FILE-ID)
06068 *        END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013117' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133313137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ACCT-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031504     END-IF.
06069
06070  6500-ACCT-NOT-FOUND.
06071
06072      PERFORM 6500-ENDBR.
06073
06074      IF  DATASORI NOT EQUAL '1'
06075          MOVE ER-0179            TO EMI-ERROR
06076          GO TO 6500-EXIT.
06077
06078      MOVE ER-0179                TO EMI-ERROR.
06079      MOVE -1                     TO MAINTL.
06080      MOVE AL-UNBON               TO MAINTA.
06081      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06082      GO TO 8200-SEND-DATAONLY.
06083
06084  6500-EXIT.
06085      EXIT.
06086                                  EJECT
06087  6600-GET-COMPENSATION-DATA.
06088
06089      IF  ADDRSI EQUAL '4'
06090              AND
06091          PI-CREATE-LABELS
06092              AND
06093          NOT PI-689-LABELS-OVERRIDEN
06094          NEXT SENTENCE
06095      ELSE
06096          IF  W-FILE-NOT-USED (10)
06097              GO TO 6600-EXIT.
06098
06099      IF  W-COMP-RESP-PERSON GREATER THAN LOW-VALUES
06100          MOVE W-COMP-KEY         TO W-COMP-SAVE-KEY
06101          GO TO 6600-START.
06102
06103      IF  PI-ZERO-CARRIER
06104              OR
06105          PI-ZERO-CAR-GROUP
06106          MOVE ZEROS              TO W-COMP-CARRIER.
06107
06108      IF  PI-ZERO-GROUPING
06109              OR
06110          PI-ZERO-CAR-GROUP
06111          MOVE ZEROS              TO W-COMP-GROUPING.
06112
06113      IF  W-FILE-NOT-USED (6)
06114          PERFORM 6500-READ-ACCT-ONLY THRU 6500-READNEXT
06115          IF  EMI-ERROR EQUAL ER-0179
06116              MOVE ER-9327        TO EMI-ERROR
06117              MOVE -1             TO MAINTL
06118              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06119              GO TO 6600-EXIT.
06120
06122      MOVE  'A'                   TO W-COMP-TYPE.
06125
031504     IF  WS-SAV-AM-REMIT-TO GREATER THAN ZEROS
031504         MOVE WS-SAV-AM-AGT (WS-SAV-AM-REMIT-TO)
06128                                  TO W-COMP-RESP-PERSON
06129      ELSE
06130          MOVE PI-689-ACCOUNT     TO W-COMP-ACCOUNT.
06131
06132      PERFORM VARYING W-NDX FROM +1 BY +1 UNTIL
06136         (W-NDX > +10)
06137             OR
052814         (WS-SAV-AM-COM-TYP (W-NDX) = 'C' OR 'D' OR 'F')
           END-PERFORM
06139
052814     IF  WS-SAV-AM-COM-TYP (W-NDX) = 'C' OR 'D' OR 'F'
031504         MOVE WS-SAV-AM-AGT (W-NDX)  TO W-COMP-ACCOUNT
06142      ELSE
06143          MOVE PI-689-ACCOUNT     TO W-COMP-ACCOUNT.
06144
06145      PERFORM 6500-ENDBR.
06146      MOVE W-COMP-KEY             TO W-COMP-SAVE-KEY.
06147
06148      IF  W-COMP-RESP-PERSON EQUAL LOW-VALUES
06149          MOVE ER-9327            TO EMI-ERROR
06150          MOVE -1                 TO MAINTL
06151          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06152          GO TO 6600-EXIT.
06153
06154  6600-START.
06155
06156      
      * EXEC CICS HANDLE CONDITION
06157 *         NOTOPEN (8080-COMP-NOT-OPEN)
06158 *         NOTFND  (6600-COMP-NOT-FOUND)
06159 *    END-EXEC.
      *    MOVE '"$JI                  ! 2 #00013203' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3220233030303133323033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06160
06161      
      * EXEC CICS STARTBR
06162 *         RIDFLD  (W-COMP-KEY)
06163 *         DATASET (W-COMP-FILE-ID)
06164 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00013208' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133323038' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06165
06166      MOVE 'Y'                    TO W-COMP-BROWSE-SW.
06167
06168  6600-READNEXT.
06169
06170      
      * EXEC CICS READNEXT
06171 *         DATASET (W-COMP-FILE-ID)
06172 *         SET     (ADDRESS OF COMPENSATION-MASTER)
06173 *         RIDFLD  (W-COMP-KEY)
06174 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013217' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133323137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06175
06176      IF CO-CONTROL-PRIMARY
06177            NOT = W-COMP-SAVE-KEY
06178         GO TO 6600-COMP-NOT-FOUND.
06179
06180      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06181      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1)
06182                                     W-VG-TEXT (140).
06183      MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
06184      MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
06185      MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
06186 *    MOVE CO-ADDR-3              TO W-LABEL-LINES (5).
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO W-LABEL-LINES (5)
           END-STRING
06187
06188      MOVE CO-ZIP                 TO W-WORK-ZIP.
06189
06190      IF  CO-CANADIAN-POST-CODE
06191          MOVE CO-CANADIAN-POSTAL-CODE
06192                                  TO W-CANADIAN-POSTAL-CODES
06193          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06194          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06195          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06196                                     W-LAB-CAN-FILLER (6)
06197      ELSE
06198          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06199
06200          IF  W-WORK-ZIP4 GREATER THAN '0000'
06201              MOVE '-'            TO W-LABEL-DASH (6)
06202              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06203          ELSE
06204              MOVE SPACES         TO W-LABEL-DASH (6)
06205                                     W-LABEL-2ND-ZIP (6).
06206
06207      IF  CO-TELEPHONE NOT NUMERIC
06208              OR
06209          CO-TELEPHONE = ZEROS
06210          MOVE SPACES             TO W-VG-TEXT (147)
06211      ELSE
06212          MOVE ZEROS              TO W-PHONE-IN
06213          MOVE CO-AREA-CODE       TO W-PO-AREA
06214          MOVE CO-PREFIX          TO W-PO-PFX
06215          MOVE CO-PHONE           TO W-PO-SFX
06216          MOVE W-PHONE-OUT        TO W-VG-TEXT (147).
06217
100705     IF (CO-FAXNO NOT NUMERIC)
100705        OR (CO-FAXNO = ZEROS)
100705        MOVE SPACES              TO W-VG-TEXT (182)
100705     ELSE
100705        MOVE ZEROS               TO W-PHONE-IN
100705        MOVE CO-FAX-AREA-CODE    TO W-PO-AREA
100705        MOVE CO-FAX-PREFIX       TO W-PO-PFX
100705        MOVE CO-FAX-PHONE        TO W-PO-SFX
100705        MOVE W-PHONE-OUT         TO W-VG-TEXT (182)
100705     END-IF
100705     IF CO-GA-STATUS-CODE NOT = 'A' AND 'I' AND 'P'
100705        MOVE SPACES              TO W-VG-TEXT (183)
100705     ELSE
100705        EVALUATE CO-GA-STATUS-CODE
100705           WHEN 'A'
100705              MOVE 'ACTIVE'      TO W-VG-TEXT (183)
100705           WHEN 'I'
100705              MOVE 'INACTIVE'    TO W-VG-TEXT (183)
100705           WHEN 'P'
100705              MOVE 'PENDING'     TO W-VG-TEXT (183)
100705        END-EVALUATE
100705     END-IF
100705     IF CO-BILL-SW NOT = 'B' AND 'R' AND 'T' AND 'S'
100705        MOVE SPACES              TO W-VG-TEXT (181)
100705     ELSE
100705        EVALUATE CO-BILL-SW
100705           WHEN 'B'
100705              MOVE 'BILLED'      TO W-VG-TEXT (181)
100705           WHEN 'R'
100705              MOVE 'REMIT'       TO W-VG-TEXT (181)
100705           WHEN 'T'
100705              MOVE 'T'           TO W-VG-TEXT (181)
100705           WHEN 'S'
100705              MOVE 'S'           TO W-VG-TEXT (181)
100705        END-EVALUATE
100705     END-IF
06218      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06219
06220      IF  ADDRSI EQUAL '4'
06221              AND
06222          PI-CREATE-LABELS
06223              AND
06224          NOT PI-689-LABELS-OVERRIDEN
06225          MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
06226                                     W-VG-TEXT (141)
06227          MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
06228                                     W-VG-TEXT (142)
06229          MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
06230                                     W-VG-TEXT (143)
06231          MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
06232                                     W-VG-TEXT (144)
06233          MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
06234                                     W-VG-TEXT (145)
06235          MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
06236                                     W-VG-TEXT (146)
06237          MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
06238          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
06239          SET W-RG-NDX UP BY +1
06240      ELSE
06241          MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (141)
06242          MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (142)
06243          MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (143)
06244          MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (144)
06245          MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (145)
06246          MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (146).
06247
06248      PERFORM 6630-GET-CSR-NAME THRU 6630-EXIT.
06249
06250      MOVE CO-RESP-NO             TO W-VG-TEXT (180).
06251      MOVE CO-END-BAL             TO W-EDIT-7-2-NEGATIVE.
06252      MOVE W-EDIT-7-2-NEGATIVE    TO W-VG-TEXT (150).
06253
06254      MOVE CO-LAST-STMT-DT        TO DC-GREG-DATE-1-YMD.
06255      MOVE '3'                    TO DC-OPTION-CODE.
06256      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
06257
06258      IF  NO-CONVERSION-ERROR
06259          MOVE DC-GREG-DATE-1-ALPHA
06260                                  TO W-VG-TEXT (149).
06261
06262      PERFORM 6620-GET-G-DATA THRU 6620-EXIT.
100705     PERFORM 6626-GET-B-DATA     THRU 6626-EXIT
06263
06264      PERFORM 6600-ENDBR.
06265      GO TO 6600-EXIT.
06266
06267  6600-ENDBR.
06268
06269      IF  W-COMP-BROWSE-STARTED
06270          MOVE 'N'                      TO W-COMP-BROWSE-SW
06271          
      * EXEC CICS ENDBR
06272 *             DATASET(W-COMP-FILE-ID)
06273 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00013358' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303133333538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06274
06275  6600-COMP-NOT-FOUND.
06276
06277      PERFORM 6600-ENDBR.
06278
06279      IF ADDRSL GREATER ZERO
06280          IF ADDRSI = '4'
06281             IF PI-689-TYPE EQUAL LOW-VALUES OR SPACES
06282                 MOVE ER-7378                TO EMI-ERROR
06283                 MOVE -1                     TO TYPEL
06284                 MOVE AL-UNBON               TO TYPEA
06285                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06286                 GO TO 8200-SEND-DATAONLY
06287             ELSE
06288                 MOVE ER-2114                TO EMI-ERROR
06289                 MOVE -1                     TO ADDRSL
06290                 MOVE AL-UNBON               TO ADDRSA
06291                 PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06292                 GO TO 8200-SEND-DATAONLY.
06293
06294  6600-EXIT.
06295      EXIT.
06302  6620-GET-G-DATA.
06303
06304      MOVE W-COMP-SAVE-KEY        TO W-COMP-KEY.
06305
06306      IF  CO-RPTCD2 GREATER THAN SPACES
06307          MOVE SPACES             TO W-COMP-RESP-PERSON
06308          MOVE CO-RPTCD2          TO W-COMP-WORK-AREA
06309          MOVE +10                TO W-CWA-NDX
06310          PERFORM 6622-FIND-LAST-NON-SPACE
06311          SET W-COMP-NDX          TO +10
06312          PERFORM 6625-FILL-IN-RESP-PERSON.
06313          INSPECT W-COMP-RESP-PERSON CONVERTING SPACES TO ZEROS.
06314
06315      MOVE LOW-VALUES             TO W-COMP-ACCOUNT.
06316      MOVE 'G'                    TO W-COMP-TYPE.
06317      MOVE W-COMP-KEY             TO W-COMP-SAVE-KEY.
06318
06319      
      * EXEC CICS RESETBR
06320 *         RIDFLD  (W-COMP-KEY)
06321 *         DATASET (W-COMP-FILE-ID)
06322 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00013400' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343030' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06323
06324      
      * EXEC CICS READNEXT
06325 *         DATASET (W-COMP-FILE-ID)
06326 *         SET     (ADDRESS OF COMPENSATION-MASTER)
06327 *         RIDFLD  (W-COMP-KEY)
06328 *    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013405' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343035' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06329
06330      IF  CO-CONTROL-PRIMARY NOT = W-COMP-SAVE-KEY
06331          GO TO 6620-EXIT.
06332
06333      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06334      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1).
06335      MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
06336      MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
06337      MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
06338 *    MOVE CO-ADDR-3              TO W-LABEL-LINES (5).
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO W-LABEL-LINES (5)
           END-STRING
06339
06340      MOVE CO-ZIP                 TO W-WORK-ZIP.
06341
06342      IF  CO-CANADIAN-POST-CODE
06343          MOVE CO-CANADIAN-POSTAL-CODE
06344                                  TO W-CANADIAN-POSTAL-CODES
06345          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06346          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06347          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06348                                     W-LAB-CAN-FILLER (6)
06349      ELSE
06350          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06351          IF  W-WORK-ZIP4 GREATER THAN '0000'
06352              MOVE '-'            TO W-LABEL-DASH (6)
06353              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06354          ELSE
06355              MOVE SPACES         TO W-LABEL-DASH (6)
06356                                     W-LABEL-2ND-ZIP (6).
06357
06358      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06359
06360      MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (174).
06361      MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (175).
06362      MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (176).
06363      MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (177).
06364      MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (178).
06365      MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (179).
06366
06367  6620-EXIT.
06368      EXIT.
06369                                  EJECT
06370  6622-FIND-LAST-NON-SPACE.
06371
06372      IF  W-CWA-CHAR (W-CWA-NDX) EQUAL SPACES
06373          SUBTRACT +1 FROM W-CWA-NDX
06374          GO TO 6622-FIND-LAST-NON-SPACE.
06375
06376  6622-EXIT.
06377      EXIT.
06378                                  EJECT
06379  6625-FILL-IN-RESP-PERSON.
06380
06381      MOVE W-CWA-CHAR (W-CWA-NDX) TO W-COMP-RP-CHAR (W-COMP-NDX)
06382      SET W-COMP-NDX DOWN BY +1.
06383      SUBTRACT +1 FROM W-CWA-NDX.
06384
06385      IF  W-CWA-NDX GREATER THAN ZEROS
06386          GO TO 6625-FILL-IN-RESP-PERSON.
06387
06388  6625-EXIT.
06389      EXIT.
100705 6626-GET-B-DATA.
100705     MOVE W-COMP-SAVE-KEY        TO W-COMP-KEY
100705     IF W-FILE-USED (8)
100705        PERFORM 6450-READ-CERT-ONLY
100705        IF W-CERT-FOUND
100705           MOVE CM-BENEFICIARY (1:10)
100705                                 TO W-COMP-RESP-PERSON
100705        ELSE
100705           MOVE SPACES           TO W-COMP-RESP-PERSON
100705           GO TO 6626-EXIT
100705        END-IF
100705     ELSE
100705        MOVE CM-BENEFICIARY (1:10)
100705                                 TO W-COMP-RESP-PERSON
100705     END-IF
100705     MOVE LOW-VALUES             TO W-COMP-ACCOUNT
100705     MOVE 'B'                    TO W-COMP-TYPE
100705     MOVE W-COMP-KEY             TO W-COMP-SAVE-KEY
06318
100705     
      * EXEC CICS RESETBR
100705*         RIDFLD  (W-COMP-KEY)
100705*         DATASET (W-COMP-FILE-ID)
100705*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&4         G          &   #00013493' TO DFHEIV0
           MOVE X'263420202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343933' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06323
100705     
      * EXEC CICS READNEXT
100705*         DATASET (W-COMP-FILE-ID)
100705*         SET     (ADDRESS OF COMPENSATION-MASTER)
100705*         RIDFLD  (W-COMP-KEY)
100705*    END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00013498' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303133343938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-COMP-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-COMP-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF COMPENSATION-MASTER TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06329
100705     IF  CO-CONTROL-PRIMARY NOT = W-COMP-SAVE-KEY
100705         GO TO 6626-EXIT.
06332
100705     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
100705     MOVE CO-ACCT-NAME           TO W-LABEL-LINES (1).
100705     MOVE CO-MAIL-NAME           TO W-LABEL-LINES (2).
100705     MOVE CO-ADDR-1              TO W-LABEL-LINES (3).
100705     MOVE CO-ADDR-2              TO W-LABEL-LINES (4).
100705*    MOVE CO-ADDR-3              TO W-LABEL-LINES (5).
           STRING CO-ADDR-CITY ' ' CO-ADDR-STATE
              DELIMITED BY '  ' INTO W-LABEL-LINES (5)
           END-STRING
06339
100705     MOVE CO-ZIP                 TO W-WORK-ZIP.
06341
100705     IF  CO-CANADIAN-POST-CODE
100705         MOVE CO-CANADIAN-POSTAL-CODE
100705                                 TO W-CANADIAN-POSTAL-CODES
100705         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
100705         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
100705         MOVE SPACES             TO W-LAB-CAN-DASH (6)
100705                                    W-LAB-CAN-FILLER (6)
100705     ELSE
100705         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
100705         IF  W-WORK-ZIP4 GREATER THAN '0000'
100705             MOVE '-'            TO W-LABEL-DASH (6)
100705             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
100705         ELSE
100705             MOVE SPACES         TO W-LABEL-DASH (6)
100705                                    W-LABEL-2ND-ZIP (6).
06357
100705     PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06359
100705     MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (184).
100705     MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (185).
100705     MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (186).
100705     MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (187).
100705     MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (188).
100705     MOVE W-LABEL-LINES (6)      TO W-VG-TEXT (189).
06366
100705 6626-EXIT.
100705     EXIT.
06391  6630-GET-CSR-NAME.
06392
06393      MOVE SPACES                 TO W-CNTL-KEY.
06394      MOVE PI-COMPANY-ID          TO W-CNTL-COMPANY-ID.
06395      MOVE +0                     TO W-CNTL-SEQ-NO.
06396      MOVE '2'                    TO W-CNTL-RECORD-TYPE.
06397      MOVE CO-CSR-CODE            TO W-CNTL-GENL.
06398
06399      
      * EXEC CICS HANDLE CONDITION
06400 *         NOTOPEN    (8040-CNTL-NOT-OPEN)
06401 *         NOTFND     (6630-CNTL2-NOT-FOUND)
06402 *    END-EXEC.
      *    MOVE '"$JI                  ! 3 #00013554' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3320233030303133353534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06403
06404      
      * EXEC CICS READ
06405 *         DATASET    (W-CNTL-FILE-ID)
06406 *         SET        (ADDRESS OF CONTROL-FILE)
06407 *         RIDFLD     (W-CNTL-KEY)
06408 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013559' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133353539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06409
06410      MOVE CF-PROCESSOR-NAME      TO W-VG-TEXT (148).
092908     MOVE CF-PROCESSOR-TITLE     TO W-VG-TEXT (154)
06411      GO TO 6630-EXIT.
06412
06413  6630-CNTL2-NOT-FOUND.
06414
06415 *    MOVE ER-8965                TO EMI-ERROR.
06416 *    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
06417 *    MOVE -1                     TO MAINTL.
06418 *    MOVE AL-UANON               TO MAINTA.
06419 *    GO TO 8200-SEND-DATAONLY
           .
06421  6630-EXIT.
06422      EXIT.
06423                                  EJECT
06424  6700-GET-MAIL-DATA.
06425
06426      IF  ADDRSI EQUAL '5'
06427              AND
06428          PI-CREATE-LABELS
06429              AND
06430          NOT PI-689-LABELS-OVERRIDEN
06431          NEXT SENTENCE
06432      ELSE
06433          IF  W-FILE-NOT-USED (5)
06434              GO TO 6700-EXIT.
06435
06436      IF  W-MAIL-CERT-NO EQUAL LOW-VALUES
06437          MOVE ER-9327            TO EMI-ERROR
06438          MOVE -1                 TO MAINTL
06439          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06440          GO TO 6700-EXIT.
06441
06442      
      * EXEC CICS HANDLE CONDITION
06443 *         NOTOPEN (8090-MAIL-NOT-OPEN)
06444 *         NOTFND  (6700-MAIL-NOT-FOUND)
06445 *    END-EXEC.
      *    MOVE '"$JI                  ! 4 #00013598' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3420233030303133353938' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06446
06447      
      * EXEC CICS READ
06448 *         DATASET(W-MAIL-FILE-ID)
06449 *         SET    (ADDRESS OF MAILING-DATA)
06450 *         RIDFLD (W-MAIL-KEY)
06451 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013603' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133363033' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAIL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-MAIL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF MAILING-DATA TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06452
06453      MOVE MA-INSURED-LAST-NAME   TO W-NAME-LAST.
06454      MOVE MA-INSURED-FIRST-NAME  TO W-NAME-FIRST.
06455      MOVE MA-INSURED-MIDDLE-INIT TO W-NAME-MIDDLE.
06456      PERFORM 7200-FORMAT-NAME-STRAIGHT THRU 7200-EXIT.
06457
06458      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06459      MOVE WS-NAME-WORK           TO W-LABEL-LINES (1).
06460      MOVE W-LABEL-JOINT-NAME     TO W-LABEL-LINES (2).
06461      MOVE MA-ADDRESS-LINE-1      TO W-LABEL-LINES (3).
06462      MOVE MA-ADDRESS-LINE-2      TO W-LABEL-LINES (4).
06463 *    MOVE MA-CITY-STATE          TO W-LABEL-LINES (5).
           STRING MA-CITY ' ' MA-ADDR-STATE
              DELIMITED BY '  ' INTO W-LABEL-LINES (5)
           END-STRING
06464
06465      MOVE MA-ZIP                 TO W-WORK-ZIP.
06466      MOVE SPACES                 TO W-LABEL-ZIP (6).
06467
06468      IF  MA-CANADIAN-POST-CODE
06469          MOVE MA-CANADIAN-POSTAL-CODE
06470                                  TO W-CANADIAN-POSTAL-CODES
06471          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06472          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06473          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06474                                     W-LAB-CAN-FILLER (6)
06475      ELSE
06476          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06477          IF  W-WORK-ZIP4 GREATER THAN '0000'
06478              MOVE '-'            TO W-LABEL-DASH (6)
06479              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06480          ELSE
06481              MOVE SPACES         TO W-LABEL-DASH (6)
06482                                     W-LABEL-2ND-ZIP (6).
06483
06484      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06485
06486      IF  ADDRSI EQUAL '5'
06487              AND
06488          PI-CREATE-LABELS
06489              AND
06490          NOT PI-689-LABELS-OVERRIDEN
06491          MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
06492                                     W-VG-TEXT (31)
06493          MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
06494                                     W-VG-TEXT (32)
06495          MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
06496                                     W-VG-TEXT (33)
06497          MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
06498                                     W-VG-TEXT (34)
06499          MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
06500                                     W-VG-TEXT (35)
06501          MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
06502                                     W-VG-TEXT (36)
06503          MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
06504          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
06505          SET W-RG-NDX UP BY +1
06506      ELSE
06507          MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (31)
06508          MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (32)
06509          MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (33)
06510          MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (34)
06511          MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (35)
06512          MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (36).
092911     MOVE SPACES                 TO W-LABEL-HOLD-AREA.
           MOVE MA-CRED-BENE-NAME      TO W-LABEL-LINES (1)
           MOVE MA-CRED-BENE-ADDR      TO W-LABEL-LINES (2)
           MOVE MA-CRED-BENE-ADDR2     TO W-LABEL-LINES (3)
042011     STRING MA-CRED-BENE-CITY ' ' MA-CRED-BENE-STATE
042011        DELIMITED BY '  ' INTO W-LABEL-LINES (4)
042011     END-STRING
042011
042011     MOVE MA-CRED-BENE-ZIP       TO W-WORK-ZIP
042011     MOVE SPACES                 TO W-LABEL-ZIP (5)
042011
042011     IF  MA-CB-CANADIAN-POST-CODE
042011         MOVE MA-CB-CANADIAN-POSTAL-CODE
042011                                 TO W-CANADIAN-POSTAL-CODES
042011         MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (5)
042011         MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (5)
042011         MOVE SPACES             TO W-LAB-CAN-DASH (5)
042011                                    W-LAB-CAN-FILLER (5)
042011     ELSE
042011         MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (5)
042011         IF  W-WORK-ZIP4 GREATER THAN '0000'
042011             MOVE '-'            TO W-LABEL-DASH (5)
042011             MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (5)
042011         ELSE
042011             MOVE SPACES         TO W-LABEL-DASH (5)
042011                                    W-LABEL-2ND-ZIP (5)
042011         END-IF
042011     END-IF
           PERFORM 7300-LABEL-MOVE THRU 7300-EXIT
           MOVE W-LABEL-LINES (1)      TO W-VG-TEXT (37)
           MOVE W-LABEL-LINES (2)      TO W-VG-TEXT (50)
           MOVE W-LABEL-LINES (3)      TO W-VG-TEXT (51)
           MOVE W-LABEL-LINES (4)      TO W-VG-TEXT (52)
           MOVE W-LABEL-LINES (5)      TO W-VG-TEXT (53)
06514      GO TO 6700-EXIT.
06515
06516  6700-MAIL-NOT-FOUND.
06517
06518      IF  ADDRSL GREATER ZERO
06519          IF ADDRSI = '5'
06520             MOVE ER-3000         TO EMI-ERROR
06521             MOVE -1              TO ADDRSL
06522             MOVE AL-UNBON        TO ADDRSA
06523             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06524             GO TO 8200-SEND-DATAONLY.
06525
06526  6700-EXIT.
06527      EXIT.
06528                                  EJECT
06529  6750-GET-LIFE-BENEFIT-DATA.
06530
06531      IF  W-FILE-NOT-USED (2)
06532          GO TO 6750-EXIT.
06533
06534      MOVE SPACES                 TO W-CNTL-GEN2.
06535
06536      IF  DATASORI EQUAL '2'
06537          IF  W-FILE-NOT-USED (8)
06538              PERFORM 6450-READ-CERT-ONLY
06539              IF  W-CERT-FOUND
06540                  MOVE CM-LF-BENEFIT-CD
06541                                  TO W-BEN-HOLD
06542                                     W-CNTL-GEN2
06543              ELSE
06544                  NEXT SENTENCE
06545          ELSE
06546              MOVE CM-LF-BENEFIT-CD
06547                                  TO W-BEN-HOLD
06548                                     W-CNTL-GEN2
06549      ELSE
06550          IF  DATASORI EQUAL '4'
06551              IF  W-FILE-NOT-USED (9)
06552                  PERFORM 6400-READ-PNDB-ONLY
06553                  IF  W-PNDB-FOUND
06554                      IF  PI-689-TYPE = '1'
06555                          MOVE PB-I-LF-BENEFIT-CD
06556                                  TO W-BEN-HOLD
06557                                     W-CNTL-GEN2
06558                      ELSE
06559                          MOVE PB-CI-LF-BENEFIT-CD
06560                                  TO W-BEN-HOLD
06561                                     W-CNTL-GEN2
06562              ELSE
06563                  NEXT SENTENCE
06564          ELSE
06565              IF  W-FILE-USED (8)
06566                  MOVE CM-LF-BENEFIT-CD
06567                                  TO W-BEN-HOLD
06568                                     W-CNTL-GEN2
06569              ELSE
06570                  IF  W-FILE-USED (9)
06571                      IF  PI-689-TYPE = '1'
06572                          MOVE PB-I-LF-BENEFIT-CD
06573                                  TO W-BEN-HOLD
06574                                     W-CNTL-GEN2
06575                      ELSE
06576                          MOVE PB-CI-LF-BENEFIT-CD
06577                                  TO W-BEN-HOLD
06578                                     W-CNTL-GEN2.
06579
06580      IF  W-CNTL-GEN2 EQUAL SPACES
06581          MOVE ER-9327            TO EMI-ERROR
06582          MOVE -1                 TO MAINTL
06583          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06584          GO TO 6750-EXIT.
06585
06586      MOVE SPACES                 TO W-CNTL-GEN1.
06587      MOVE '4'                    TO W-CNTL-RECORD-TYPE.
06588      MOVE ZEROS                  TO W-CNTL-SEQ-NO.
06589
06590      
      * EXEC CICS HANDLE CONDITION
06591 *         NOTOPEN (8040-CNTL-NOT-OPEN)
06592 *         NOTFND  (6750-EXIT)
06593 *    END-EXEC.
      *    MOVE '"$JI                  ! 5 #00013782' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3520233030303133373832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06594
06595      
      * EXEC CICS READ
06596 *         DATASET (W-CNTL-FILE-ID)
06597 *         SET     (ADDRESS OF CONTROL-FILE)
06598 *         GTEQ
06599 *         RIDFLD  (W-CNTL-KEY)
06600 *    END-EXEC.
      *    MOVE '&"S        G          (   #00013787' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133373837' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06601
06602      MOVE 1                      TO W-NDX.
06603
06604  6750-LOOP.
06605
06606      IF  W-NDX = 9
06607          GO TO 6750-EXIT.
06608
06609      IF  CF-BENEFIT-CODE (W-NDX) LESS W-BEN-HOLD
06610          ADD 1                   TO W-NDX
06611          GO TO 6750-LOOP.
06612
06613      IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-NDX)
06614          MOVE CF-BENEFIT-DESCRIP (W-NDX)
06615                                  TO W-VG-TEXT (11).
06616
06617  6750-EXIT.
06618      EXIT.
06619                                  EJECT
06620  6800-GET-A-H-BENEFIT-DATA.
06621
06622      IF  W-FILE-NOT-USED (3)
06623          GO TO 6800-EXIT.
06624
06625      MOVE SPACES                 TO W-CNTL-GEN2.
06626
06627      IF  DATASORI EQUAL '2'
06628          IF  W-FILE-NOT-USED (8)
06629              PERFORM 6450-READ-CERT-ONLY
06630              IF  W-CERT-FOUND
06631                  MOVE CM-AH-BENEFIT-CD
06632                                  TO W-BEN-HOLD
06633                                     W-CNTL-GEN2
06634              ELSE
06635                  NEXT SENTENCE
06636          ELSE
06637              MOVE CM-AH-BENEFIT-CD
06638                                  TO W-BEN-HOLD
06639                                     W-CNTL-GEN2
06640      ELSE
06641          IF  DATASORI EQUAL '4'
06642              IF  W-FILE-NOT-USED (9)
06643                  PERFORM 6400-READ-PNDB-ONLY
06644                  IF  W-PNDB-FOUND
06645                      IF  PI-689-TYPE = '1'
06646                          MOVE PB-I-AH-BENEFIT-CD
06647                                  TO W-BEN-HOLD
06648                                     W-CNTL-GEN2
06649                      ELSE
06650                          MOVE PB-CI-AH-BENEFIT-CD
06651                                  TO W-BEN-HOLD
06652                                     W-CNTL-GEN2
06653              ELSE
06654                  NEXT SENTENCE
06655          ELSE
06656              IF  W-FILE-USED (8)
06657                  MOVE CM-AH-BENEFIT-CD
06658                                  TO W-BEN-HOLD
06659                                     W-CNTL-GEN2
06660              ELSE
06661                  IF  W-FILE-USED (9)
06662                      IF  PI-689-TYPE = '1'
06663                          MOVE PB-I-AH-BENEFIT-CD
06664                                  TO W-BEN-HOLD
06665                                     W-CNTL-GEN2
06666                      ELSE
06667                          MOVE PB-CI-AH-BENEFIT-CD
06668                                  TO W-BEN-HOLD
06669                                     W-CNTL-GEN2.
06670
06671      IF  W-CNTL-GEN2 EQUAL SPACES
06672          MOVE ER-9327            TO EMI-ERROR
06673          MOVE -1                 TO MAINTL
06674          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06675          GO TO 6800-EXIT.
06676
06677      MOVE SPACES                 TO W-CNTL-GEN1.
06678      MOVE '5'                    TO W-CNTL-RECORD-TYPE.
06679      MOVE ZEROS                  TO W-CNTL-SEQ-NO.
06680
06681      
      * EXEC CICS HANDLE CONDITION
06682 *         NOTOPEN (8040-CNTL-NOT-OPEN)
06683 *         NOTFND  (6800-EXIT)
06684 *    END-EXEC.
      *    MOVE '"$JI                  ! 6 #00013873' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3620233030303133383733' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06685
06686      
      * EXEC CICS READ
06687 *         DATASET (W-CNTL-FILE-ID)
06688 *         SET     (ADDRESS OF CONTROL-FILE)
06689 *         RIDFLD  (W-CNTL-KEY)
06690 *         GTEQ
06691 *    END-EXEC.
      *    MOVE '&"S        G          (   #00013878' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133383738' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06692
06693      MOVE 1                      TO W-NDX.
06694
06695  6800-LOOP-AH.
06696
06697      IF  W-NDX = 9
06698          GO TO 6800-EXIT.
06699
06700      IF  CF-BENEFIT-CODE (W-NDX) LESS W-BEN-HOLD
06701          ADD 1                   TO W-NDX
06702          GO TO 6800-LOOP-AH.
06703
06704      IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-NDX)
06705          MOVE CF-BENEFIT-DESCRIP (W-NDX)
06706                                  TO W-VG-TEXT (15)
06707          MOVE CF-BENEFIT-ALPHA (W-NDX)
06708                                  TO W-BENEFIT-WORK
06709          MOVE W-ELIM-DAYS        TO W-VG-TEXT (16).
06710
06711  6800-EXIT.
06712      EXIT.
06713                                  EJECT
06714  6850-GET-CHECK-DATA.
06715
06716      IF  ADDRSI EQUAL '6'
06717              AND
06718          PI-CREATE-LABELS
06719              AND
06720          NOT PI-689-LABELS-OVERRIDEN
06721          NEXT SENTENCE
06722      ELSE
06723          IF  W-FILE-NOT-USED (12)
06724              GO TO 6850-EXIT.
06725
06726      IF  W-CHEK-CERT-NO EQUAL LOW-VALUES
06727          MOVE ER-9327            TO EMI-ERROR
06728          MOVE -1                 TO MAINTL
06729          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06730          GO TO 6850-EXIT.
06731
06732      
      * EXEC CICS HANDLE CONDITION
06733 *         NOTOPEN (8045-CHEK-NOT-OPEN)
06734 *         NOTFND  (6850-CHEK-NOT-FOUND)
06735 *    END-EXEC.
      *    MOVE '"$JI                  ! 7 #00013924' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3720233030303133393234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06736
06737      
      * EXEC CICS READ
06738 *         DATASET (W-CHEK-FILE-ID)
06739 *         SET     (ADDRESS OF CHECK-RECORDS)
06740 *         RIDFLD  (W-CHEK-KEY)
06741 *    END-EXEC.
      *    MOVE '&"S        E          (   #00013929' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303133393239' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CHEK-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CHEK-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CHECK-RECORDS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06742
06743      MOVE CH-CHECK-NO            TO W-VG-TEXT (158).
06744      MOVE CH-AMOUNT-PAID         TO W-EDIT-7-2.
06745      MOVE W-EDIT-7-2             TO W-VG-TEXT (157).
06746      MOVE CH-CHECK-QUE-CONTROL   TO W-DISPLAY-8
06747                                     PI-689-CONTROL.
06748      MOVE W-DISPLAY-8            TO W-VG-TEXT (165).
06749      MOVE CH-REASON-FOR-CHECK    TO W-VG-TEXT (166).
06750
06751      MOVE SPACES                 TO W-LABEL-HOLD-AREA.
06752      MOVE CH-PAYEE-NAME-1        TO W-LABEL-LINES (1).
06753      MOVE CH-PAYEE-NAME-2        TO W-LABEL-LINES (2).
06754      MOVE CH-PAYEE-ADDRESS-1     TO W-LABEL-LINES (3).
06755      MOVE CH-PAYEE-ADDRESS-2     TO W-LABEL-LINES (4).
06756      MOVE CH-PAYEE-CITY-ST       TO W-LABEL-LINES (5).
06757
06758      MOVE CH-PAYEE-ZIP           TO W-WORK-ZIP.
06759      MOVE SPACES                 TO W-LABEL-ZIP (6).
06760
06761      IF  CH-CANADIAN-POST-CODE
06762          MOVE CH-CANADIAN-POSTAL-CODE
06763                                  TO W-CANADIAN-POSTAL-CODES
06764          MOVE W-CAN-POSTAL-CD-1  TO W-LAB-CAN-POSTAL-CD-1 (6)
06765          MOVE W-CAN-POSTAL-CD-2  TO W-LAB-CAN-POSTAL-CD-2 (6)
06766          MOVE SPACES             TO W-LAB-CAN-DASH (6)
06767                                     W-LAB-CAN-FILLER (6)
06768      ELSE
06769          MOVE W-WORK-ZIP5        TO W-LABEL-1ST-ZIP (6)
06770          IF  W-WORK-ZIP4 GREATER THAN '0000'
06771              MOVE '-'            TO W-LABEL-DASH (6)
06772              MOVE W-WORK-ZIP4    TO W-LABEL-2ND-ZIP (6)
06773          ELSE
06774              MOVE SPACES         TO W-LABEL-DASH (6)
06775                                     W-LABEL-2ND-ZIP (6).
06776
06777      PERFORM 7300-LABEL-MOVE THRU 7300-EXIT.
06778
06779      IF  ADDRSI EQUAL '6'
06780              AND
06781          PI-CREATE-LABELS
06782              AND
06783          NOT PI-689-LABELS-OVERRIDEN
06784          MOVE W-LABEL-LINES (1)  TO W-RC-TEXT (1)
06785                                     W-VG-TEXT (159)
06786          MOVE W-LABEL-LINES (2)  TO W-RC-TEXT (2)
06787                                     W-VG-TEXT (160)
06788          MOVE W-LABEL-LINES (3)  TO W-RC-TEXT (3)
06789                                     W-VG-TEXT (161)
06790          MOVE W-LABEL-LINES (4)  TO W-RC-TEXT (4)
06791                                     W-VG-TEXT (162)
06792          MOVE W-LABEL-LINES (5)  TO W-RC-TEXT (5)
06793                                     W-VG-TEXT (163)
06794          MOVE W-LABEL-LINES (6)  TO W-RC-TEXT (6)
06795                                     W-VG-TEXT (164)
06796          MOVE W-NUMB-LABEL-LINES TO PI-689-NUMBER-LABEL-LINES
06797          SET W-RG-NDX            TO PI-689-NUMBER-LABEL-LINES
06798          SET W-RG-NDX UP BY +1
06799      ELSE
06800          MOVE W-LABEL-LINES (1)  TO W-VG-TEXT (159)
06801          MOVE W-LABEL-LINES (2)  TO W-VG-TEXT (160)
06802          MOVE W-LABEL-LINES (3)  TO W-VG-TEXT (161)
06803          MOVE W-LABEL-LINES (4)  TO W-VG-TEXT (162)
06804          MOVE W-LABEL-LINES (5)  TO W-VG-TEXT (163)
06805          MOVE W-LABEL-LINES (6)  TO W-VG-TEXT (164).
06806
06807      GO TO 6850-EXIT.
06808
06809  6850-CHEK-NOT-FOUND.
06810
06811      IF  ADDRSL GREATER ZERO
06812          IF ADDRSI = '6'
06813             MOVE ER-2908         TO EMI-ERROR
06814             MOVE -1              TO ADDRSL
06815             MOVE AL-UNBON        TO ADDRSA
06816             PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06817             GO TO 8200-SEND-DATAONLY.
06818
06819  6850-EXIT.
06820      EXIT.
06821                                  EJECT
06822  6900-GET-PYAJ-DATA.
06823
06824      IF  W-FILE-NOT-USED (13)
06825          GO TO 6900-EXIT.
06826
06827      IF  W-PYAJ-FIN-RESP EQUAL LOW-VALUES
06828          MOVE ER-9327            TO EMI-ERROR
06829          MOVE -1                 TO MAINTL
06830          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06831          GO TO 6900-EXIT.
06832
06833      
      * EXEC CICS HANDLE CONDITION
06834 *         NOTOPEN (8085-PYAJ-NOT-OPEN)
06835 *         NOTFND  (6900-PYAJ-NOT-FOUND)
06836 *    END-EXEC.
      *    MOVE '"$JI                  ! 8 #00014025' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3820233030303134303235' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06837
06838  6900-READ-NEXT.
06839
06840      
      * EXEC CICS READ
06841 *         DATASET (W-PYAJ-FILE-ID)
06842 *         SET     (ADDRESS OF PENDING-PAY-ADJ)
06843 *         RIDFLD  (W-PYAJ-KEY)
06844 *         GTEQ
06845 *    END-EXEC.
      *    MOVE '&"S        G          (   #00014032' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303134303332' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-PYAJ-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-PYAJ-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-PAY-ADJ TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
06846
06847      IF  PY-COMPANY-CD NOT EQUAL W-PYAJ-COMPANY-CD
06848              OR
06849          PY-CARRIER NOT EQUAL W-PYAJ-CARRIER
06850              OR
06851          PY-GROUPING NOT EQUAL W-PYAJ-GROUPING
06852              OR
06853          PY-FIN-RESP NOT EQUAL W-PYAJ-FIN-RESP
06854              OR
06855          PY-ACCOUNT NOT EQUAL W-PYAJ-ACCOUNT
06856          GO TO 6900-PYAJ-NOT-FOUND.
06857
06858      IF  PI-689-SEQ-NO GREATER THAN ZEROS
06859              AND
06860          PY-FILE-SEQ-NO NOT EQUAL PI-689-SEQ-NO
06861          GO TO 6900-PYAJ-NOT-FOUND.
06862
06863      IF  PY-RECORD-TYPE NOT EQUAL W-PYAJ-RECORD-TYPE
06864          MOVE PY-CONTROL-PRIMARY TO W-PYAJ-KEY
06865          ADD +1                  TO W-PYAJ-FILE-SEQ-NO
06866          GO TO 6900-READ-NEXT.
06867
06868      MOVE PY-CHECK-NUMBER        TO W-DISPLAY-7.
06869      MOVE W-DISPLAY-7            TO W-VG-TEXT (171).
06870
06871      MOVE PY-ENTRY-AMT           TO W-EDIT-7-2.
06872      MOVE W-EDIT-7-2             TO W-VG-TEXT (170).
06873
06874      MOVE PY-CHECK-QUE-CONTROL   TO W-DISPLAY-8
06875                                     PI-689-CONTROL.
06876      MOVE W-DISPLAY-8            TO W-VG-TEXT (172).
06877      MOVE PY-ENTRY-COMMENT       TO W-VG-TEXT (173).
06878
06879      GO TO 6900-EXIT.
06880
06881  6900-PYAJ-NOT-FOUND.
06882
06883      MOVE ER-7395                TO EMI-ERROR
06884      MOVE -1                     TO DATASORL
06885      MOVE AL-UNBON               TO DATASORA
06886      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
06887      GO TO 8200-SEND-DATAONLY.
06888
06889  6900-EXIT.
06890      EXIT.
06891                                  EJECT
06892  6950-MOVE-SYSTEM-DATA.
06893
06894      IF  W-FILE-NOT-USED (7)
06895          GO TO 6950-EXIT.
06896
06897      MOVE EIBDATE                TO W-DATE-WORK.
06898      MOVE W-DT-WORK              TO DC-JULIAN-YYDDD.
06899      MOVE '5'                    TO DC-OPTION-CODE.
06900      PERFORM 9500-LINK-DATE-CONVERT THRU 9500-EXIT.
06901      MOVE DC-GREG-DATE-1-EDIT    TO W-VG-TEXT (60).
06902      MOVE DC-GREG-DATE-1-ALPHA   TO W-VG-TEXT (61).
06903
06904      MOVE FORMI                  TO W-VG-TEXT (62).
06905
06906      IF  PI-689-VARIABLE-DATA-1 GREATER THAN SPACES
06907          MOVE PI-689-VARIABLE-DATA-1
06908                                  TO W-VG-TEXT (63)
06909          MOVE PI-689-VARIABLE-DATA-2
06910                                  TO W-VG-TEXT (64)
06911          MOVE PI-689-VARIABLE-DATA-3
06912                                  TO W-VG-TEXT (65)
06913          MOVE PI-689-VARIABLE-DATA-4
06914                                  TO W-VG-TEXT (66)
06915      ELSE
06916          MOVE '(VARIABLE NOT PROVIDED)'
06917                                  TO W-VG-TEXT (63).
06918
06919  6950-EXIT.
06920      EXIT.
06921                                  EJECT
06922  7000-FORMAT-SCREEN.
06923
06924      IF  MAINTI EQUAL 'S'
06925              OR
06926          PI-SHOW-MODE
06927          MOVE AL-PANOF           TO W-SC-TEXTA (W-SC-NDX)
06928      ELSE
06929          MOVE AL-UANOF           TO W-SC-TEXTA (W-SC-NDX).
06930
06931      IF  W-RG-NDX NOT GREATER THAN PI-689-NUMBER-LABEL-LINES
06932          SET W-LINE23            TO W-RG-NDX
06933          MOVE 'A'                TO W-LINE1
06934      ELSE
06935          SET W-LIN-NUM           TO W-RG-NDX
06936          COMPUTE W-LIN-NUM
06937              = W-LIN-NUM - PI-689-NUMBER-LABEL-LINES - 1.
06938
06939      MOVE W-LINE-NUM             TO W-SC-LINE (W-SC-NDX).
06940      MOVE W-RC-TEXT (W-RG-NDX)   TO W-SC-TEXT (W-SC-NDX).
06941      SET W-RG-NDX UP BY 1.
06942
06943  7000-EXIT.
06944       EXIT.
06945                                  EJECT
06946  7100-FORMAT-LAST-NAME-1ST.
06947 *****************************************************************
06948 *             M O V E   N A M E   R O U T I N E                 *
06949 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
06950 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
06951 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
06952 *                  FIELD                   VALUE                *
06953 *           W-NAME-LAST    (CL15)      SMITH                    *
06954 *           W-NAME-FIRST   (CL15)      JOHN                     *
06955 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
06956 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
06957 *     CONTAIN                                                   *
06958 *                SMITH, JOHN ALLEN                              *
06959 *     OR                                                        *
06960 *                SMITH, JOHN A.                                 *
06961 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
06962 *     COPYBOOK, ELCNWA.                                         *
06963 *****************************************************************.
06964
06965      MOVE SPACES                 TO WS-NAME-WORK-AREA.
06966      MOVE ZERO                   TO WS-NAME-SW.
06967      SET NWA-INDEX               TO +1.
06968
06969      IF  W-NAME-LAST EQUAL SPACES
06970              AND
06971          W-NAME-MIDDLE EQUAL SPACES
06972          MOVE +1                 TO WS-NAME-SW.
06973
06974      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
06975      PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
06976
06977      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
06978      PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
06979
06980      SET NWA-INDEX UP BY +1.
06981
06982      IF  W-NAME-MIDDLE NOT EQUAL SPACES
06983          IF  W-NAME-MIDDLE-2 EQUAL SPACES
06984              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
06985              SET NWA-INDEX UP BY +1
06986              MOVE '.'            TO WS-NW (NWA-INDEX)
06987              SET NWA-INDEX UP BY +2
06988          ELSE
06989              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
06990              PERFORM 7110-MOVE-NAME THRU 7110-EXIT.
06991
06992  7100-EXIT.
06993      EXIT.
06994                                  EJECT
06995  7110-MOVE-NAME.
06996
06997      IF  WS-NAME-SW GREATER THAN +1
06998          GO TO 7110-EXIT.
06999
07000      IF  WS-NAME-WORK2 = SPACES
07001          GO TO 7110-EXIT.
07002
07003      SET NWA-INDEX2            TO +1.
07004      SET NWA-INDEX3            TO +2.
07005
07006  7110-MOVE-NAME-CYCLE.
07007
07008      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
07009
07010      IF  NWA-INDEX LESS THAN +30
07011          SET NWA-INDEX UP BY +1
07012      ELSE
07013          ADD +2                TO WS-NAME-SW
07014          GO TO 7110-EXIT.
07015
07016      IF  NWA-INDEX2 LESS THAN +20
07017          SET NWA-INDEX3 UP BY +1
07018          SET NWA-INDEX2 UP BY +1.
07019
07020      IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
07021              AND
07022          WS-NW2 (NWA-INDEX3) EQUAL SPACES
07023          IF  WS-NAME-SW EQUAL ZERO
07024              MOVE ','            TO WS-NW (NWA-INDEX)
07025              SET NWA-INDEX UP BY +2
07026              MOVE +1             TO WS-NAME-SW
07027              GO TO 7110-EXIT
07028          ELSE
07029              GO TO 7110-EXIT.
07030
07031      GO TO 7110-MOVE-NAME-CYCLE.
07032
07033  7110-EXIT.
07034      EXIT.
07035                                  EJECT
07036  7200-FORMAT-NAME-STRAIGHT.
07037 *****************************************************************
07038 *           M O V E   N A M E   R O U T I N E                   *
07039 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *
07040 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *
07041 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *
07042 *                  FIELD                   VALUE                *
07043 *           W-NAME-LAST    (CL15)      SMITH                    *
07044 *           W-NAME-FIRST   (CL15)      JOHN                     *
07045 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *
07046 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *
07047 *     CONTAIN                                                   *
07048 *              JOHN A. SMITH                                    *
07049 *     OR                                                        *
07050 *              JOHN ALLEN SMITH                                 *
07051 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *
07052 *     COPYBOOK, ELCNWA.                                         *
07053 *****************************************************************
07054
07055      MOVE SPACES                 TO WS-NAME-WORK-AREA.
07056      MOVE ZERO                   TO WS-NAME-SW.
07057      SET NWA-INDEX               TO +1.
07058
07059      IF  W-NAME-FIRST EQUAL SPACES
07060              AND
07061          W-NAME-MIDDLE EQUAL SPACES
07062          MOVE W-NAME-LAST        TO WS-NAME-WORK
07063          GO TO 7200-EXIT.
07064
07065      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.
07066      PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
07067
07068      IF  W-NAME-MIDDLE NOT EQUAL SPACES
07069          SET NWA-INDEX UP BY +1
07070
07071          IF  W-NAME-MIDDLE-2 EQUAL SPACES
07072              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)
07073              SET NWA-INDEX UP BY +1
07074              MOVE '.'            TO WS-NW (NWA-INDEX)
07075              SET NWA-INDEX UP BY +1
07076
07077          ELSE
07078              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2
07079              PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
07080
07081      SET NWA-INDEX UP BY +1
07082      MOVE W-NAME-LAST            TO WS-NAME-WORK2.
07083      PERFORM 7290-MOVE-NAME THRU 7290-EXIT.
07084
07085  7200-EXIT.
07086      EXIT.
07087                                  EJECT
07088  7290-MOVE-NAME.
07089
07090      IF  WS-NAME-SW GREATER THAN +1
07091          GO TO 7290-EXIT.
07092
07093      IF  WS-NAME-WORK2 EQUAL SPACES
07094          GO TO 7290-EXIT.
07095
07096      SET NWA-INDEX2            TO +1.
07097      SET NWA-INDEX3            TO +2.
07098
07099  7290-MOVE-NAME-CYCLE.
07100
07101      MOVE WS-NW2 (NWA-INDEX2)  TO WS-NW (NWA-INDEX).
07102
07103      IF  NWA-INDEX LESS THAN +30
07104          SET NWA-INDEX UP BY +1
07105      ELSE
07106          ADD +2                TO WS-NAME-SW
07107          GO TO 7290-EXIT.
07108
07109      IF  NWA-INDEX2 LESS THAN +20
07110          SET NWA-INDEX2 UP BY +1
07111          SET NWA-INDEX3 UP BY +1.
07112
07113      IF  WS-NW2 (NWA-INDEX2) EQUAL SPACES
07114              AND
07115          WS-NW2 (NWA-INDEX3) EQUAL SPACES
07116          GO TO 7290-EXIT.
07117
07118      GO TO 7290-MOVE-NAME-CYCLE.
07119
07120  7290-EXIT.
07121      EXIT.
07122                                  EJECT
07123  7300-LABEL-MOVE.
07124
07125      IF  W-LABEL-HOLD-AREA = SPACES
07126          MOVE +0                 TO W-NUMB-LABEL-LINES
07127          GO TO 7300-EXIT.
07128
07129      IF  W-LABEL-LINES (1) = SPACES
07130          MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)
07131          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
07132          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
07133          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07134          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07135          MOVE SPACES             TO W-LABEL-LINES (6)
07136          GO TO 7300-LABEL-MOVE.
07137
07138      IF  W-LABEL-LINES (2) = SPACES
07139              AND
07140          W-LABEL-LINES (3) = SPACES
07141              AND
07142          W-LABEL-LINES (4) = SPACES
07143              AND
07144          W-LABEL-LINES (5) = SPACES
07145              AND
07146          W-LABEL-LINES (6) = SPACES
07147          MOVE 1                  TO W-NDX
07148                                     W-NUMB-LABEL-LINES
07149          GO TO 7300-EXIT.
07150
07151  7300-TRY-2.
07152
07153      IF  W-LABEL-LINES (2) = SPACES
07154          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)
07155          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
07156          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07157          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07158          MOVE SPACES             TO W-LABEL-LINES (6)
07159          GO TO 7300-TRY-2.
07160
07161      IF  W-LABEL-LINES (3) = SPACES
07162              AND
07163          W-LABEL-LINES (4) = SPACES
07164              AND
07165          W-LABEL-LINES (5) = SPACES
07166              AND
07167          W-LABEL-LINES (6) = SPACES
07168          MOVE 2                  TO W-NDX
07169          GO TO 7300-MOVE-ZIP.
07170
07171  7300-TRY-3.
07172
07173      IF  W-LABEL-LINES (3) = SPACES
07174          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)
07175          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07176          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07177          MOVE SPACES             TO W-LABEL-LINES (6)
07178          GO TO 7300-TRY-3.
07179
07180      IF  W-LABEL-LINES (4) = SPACES
07181              AND
07182          W-LABEL-LINES (5) = SPACES
07183              AND
07184          W-LABEL-LINES (6) = SPACES
07185          MOVE 3                   TO W-NDX
07186          GO TO 7300-MOVE-ZIP.
07187
07188  7300-TRY-4.
07189
07190      IF  W-LABEL-LINES (4) = SPACES
07191          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)
07192          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07193          MOVE SPACES             TO W-LABEL-LINES (6)
07194          GO TO 7300-TRY-4.
07195
07196      IF  W-LABEL-LINES (5) = SPACES
07197              AND
07198          W-LABEL-LINES (6) = SPACES
07199          MOVE 4                  TO W-NDX
07200          GO TO 7300-MOVE-ZIP.
07201
07202  7300-TRY-5.
07203
07204      IF  W-LABEL-LINES (5) = SPACES
07205          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)
07206          MOVE SPACES             TO W-LABEL-LINES (6)
07207          GO TO 7300-TRY-5
07208      ELSE
07209          IF  W-LABEL-LINES (6) = SPACES
07210              MOVE 5              TO W-NDX
07211              GO TO 7300-MOVE-ZIP
07212          ELSE
07213              MOVE 6              TO W-NDX.
07214
07215  7300-MOVE-ZIP.
07216
07217      COMPUTE W-NDX2 = W-NDX - 1.
07218
07219      IF  W-LAST-ZIP (W-NDX2) = SPACES
07220 *****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE
07221
07222          IF  W-LABEL-1ST-ZIP (W-NDX) NUMERIC
07225                  MOVE W-LABEL-ZIP (W-NDX)
07226                                  TO W-LAST-ZIP (W-NDX2)
07227                  MOVE SPACES     TO W-LABEL-LINES (W-NDX)
07228                  MOVE W-NDX2     TO W-NUMB-LABEL-LINES
07229                  GO TO 7300-EXIT.
07230
07231      MOVE W-NDX                  TO W-NUMB-LABEL-LINES.
07232
07233  7300-EXIT.
07234      EXIT.
07235                                  EJECT
07236  7400-CREATE-LETTER.
07237 ****************************************************************
07238 *    THIS AREA CONTROLS THE PROCESSING OF INDIVIDUAL INPUT     *
07239 *    RECORDS.                                                  *
07240 ****************************************************************
07241
07242      SET W-SQ-NDX                TO W-START-COLUMN.
07243      MOVE SPACES                 TO W-SQUEEZED-LINE
07244                                     W-WORK-LINE.
07245      MOVE W-START-COLUMN         TO W-LAST-SQUEEZED-SPACE
07246      MOVE +1                     TO W-LAST-WC-SPACE.
07247      MOVE HIGH-VALUES            TO W-LAST-SQ-CHAR.
07248
07249      SET W-TG-NDX                TO +1.
07250
07251      PERFORM 7410-TEXT-LINE-PROCESS THRU 7410-EXIT
07252             VARYING
07253          W-TG-NDX FROM W-TG-NDX BY +1
07254             UNTIL
07255          W-TG-NDX GREATER THAN W-TOTAL-TX-LINES.
07256
07257      IF  W-FORM-SQUEEZE-ON
07258              AND
07259          W-SQUEEZED-LINE GREATER THAN SPACES
07260          PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT.
07261
07262      PERFORM 7640-PAGING THRU 7640-EXIT.
07263
07264  7400-EXIT.
07265      EXIT.
07266                                  EJECT
07267  7410-TEXT-LINE-PROCESS.
07268 ***************************************************************
07269 *    THIS ROUTINE SEARCHES THE TEXT WORK AREA FOR ANY         *
07270 *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *
07271 *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *
07272 *    DATA THAT WAS GENERATED PREVIOUSLY.  WHILE PERFORMING    *
07273 *    THIS FUNCTION THE ROUTINE MOVES EACH CHARACTER OR ITS    *
07274 *    VARIABLE REPLACEMENT TO A WORK LINE.                     *
07275 ***************************************************************
07276
07277
07278      SET W-WC-NDX                TO W-ZEROS.
07279      MOVE HIGH-VALUES            TO W-LAST-CHAR.
07280
07281      MOVE W-TX-SC (W-TG-NDX)     TO W-LINE-SQUEEZE-IND.
07282
07283      PERFORM 7420-TEXT-CHAR-PROCESS THRU 7420-EXIT
07284              VARYING
07285          W-TX-NDX FROM +1 BY +1
07286              UNTIL
07287          W-TX-NDX GREATER THAN +70.
07288
07289 ****************************************************************
07290 *    IF SPACE 'SQUEEZING' IS INDICATED ON A FORM WIDE BASIS    *
07291 *    EACH LINE OF TEXT IS PROCESSED ACCORDING TO THE INDICATED *
07292 *    LINE SQUEEZE CONTROL.                                     *
07293 ****************************************************************
07294      IF  W-FORM-SQUEEZE-ON
07295
07296          IF  W-WORK-LINE GREATER THAN SPACES
07297                  OR
07298              W-AS-IS
07299              PERFORM 7450-SQUEEZING THRU 7450-EXIT
07300
07301          ELSE
07302              NEXT SENTENCE
07303 ****************************************************************
07304 *    IF NO SPACE 'SQUEEZING' IS INDICATED ON A FORM WIDE BASIS *
07305 *    EACH LINE OF TEXT IS PRINT AS IS EXCEPT FOR VARIABLES     *
07306 ****************************************************************
07307      ELSE
07308          IF  W-WORK-LINE GREATER THAN SPACES
07309              MOVE W-TX-PC (W-TG-NDX)
07310                                  TO W-PRINT-CONTROL
07311              MOVE W-WORK-LINE    TO W-SQUEEZED-LINE
07312              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07313              MOVE SPACES         TO W-WORK-LINE.
07314
07315  7410-EXIT.
07316      EXIT.
07317                                  EJECT
07318  7420-TEXT-CHAR-PROCESS.
07319 ****************************************************************
07320 *    LOOKING FOR VARIABLE INDICATORS (**XXX) THIS SECTION      *
07321 *    CONTROLS THE CONVERSION OF THESE INDICATORS INTO THEIR    *
07322 *    ACTUAL VALUES.  IT MOVES THESE REPLACEMENTS AND THE       *
07323 *    REMAINING CHARACTERS TO THE WORK LINE.                    *
07324 ****************************************************************
07325
07326      IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) EQUAL '@'
07327              AND
07328          W-LAST-CHAR EQUAL '@'
07329          PERFORM 7430-PROCESS-VARIABLE THRU 7430-EXIT.
07330
07331      SET W-WC-NDX UP BY +1.
07332
07333      IF  W-WC-NDX GREATER THAN +70
07334          IF  W-FORM-SQUEEZE-ON
07335                  AND
07336              NOT W-AS-IS
07337              PERFORM 7440-BACK-TO-NEAREST-WORD THRU 7440-EXIT
07338                      VARYING
07339                  W-WC-NDX FROM W-LAST-WC-SPACE BY +1
07340                      UNTIL
07341                  W-WC-NDX EQUAL +71
07342              SET W-TX-NDX        TO W-LAST-TX-SPACE
07343
07344              IF  W-WORK-LINE GREATER THAN SPACES
07345                  PERFORM 7450-SQUEEZING THRU 7450-EXIT
07346                  MOVE SPACES     TO W-WORK-LINE
07347                                     W-LINE-SQUEEZE-IND
07348                  SET W-WC-NDX    TO +1
07349              ELSE
07350                  SET W-WC-NDX    TO +1
07351          ELSE
07352              PERFORM 7460-REMAIN-CHAR-CHECK THRU 7460-EXIT.
07353
07354      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07355                                  TO W-CHARACTER-TYPE.
07356
07357      SET W-WC-NDX2               TO W-WC-NDX.
07358      SET W-WC-NDX2 DOWN BY +1.
07359
07360      IF  W-PUNCTUATION
07361              AND
07362          W-WC-NDX2 GREATER THAN W-ZEROS
07363              AND
07364          W-WORK-CHAR (W-WC-NDX2) EQUAL SPACES
07365          SET W-WC-NDX DOWN BY +1.
07366
07367      MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07368                                  TO W-LAST-CHAR
07369                                     W-WORK-CHAR (W-WC-NDX).
07370
07371      IF  W-LAST-CHAR EQUAL SPACES
07372          SET W-LAST-WC-SPACE     TO W-WC-NDX
07373          SET W-LAST-TX-SPACE     TO W-TX-NDX.
07374
07375  7420-EXIT.
07376      EXIT.
07377                                 EJECT
07378  7430-PROCESS-VARIABLE.
07379 ******************************************************************
07380 *    THIS SECTION CHECKS ON ALL '@@XXX' FORMATED CHARACTER       *
07381 *    GROUPS.  IF IN PROPER FORMAT, EACH GROUP IS REPLACED WITH   *
07382 *    ITS CORRESPONDING VALUES.                                   *
07383 ******************************************************************
07384
07385      SET W-TX-NDX UP BY +1.
07386
07387      IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
07388          MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07389                                  TO W-V1
07390          SET W-TX-NDX UP BY +1
07391
07392          IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
07393              MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07394                                  TO W-V2
07395              SET W-TX-NDX UP BY +1
07396              IF  W-TX-CHAR (W-TG-NDX W-TX-NDX) NUMERIC
07397                  MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07398                                  TO W-V3
07399                  SET W-TX-NDX UP BY +1
07400                  MOVE W-TX-CHAR (W-TG-NDX W-TX-NDX)
07401                                  TO W-FIELD-SQUEEZE-IND
07402                  GO TO 7430-CONTINUE
07403              ELSE
07404                  SET W-TX-NDX DOWN BY +3
07405                  MOVE ER-0180    TO EMI-ERROR
07406                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07407                  MOVE -1         TO MAINTL
07408                  MOVE HIGH-VALUES
07409                                  TO W-LAST-CHAR
07410          ELSE
07411              SET W-TX-NDX DOWN BY +2
07412              MOVE ER-0180        TO EMI-ERROR
07413              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07414              MOVE -1             TO MAINTL
07415              MOVE HIGH-VALUES    TO W-LAST-CHAR
07416      ELSE
07417          SET W-TX-NDX DOWN BY +1
07418          MOVE ER-7246            TO EMI-ERROR
07419          MOVE -1                 TO MAINTL
07420          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07421          MOVE HIGH-VALUES        TO W-LAST-CHAR.
07422
07423 ****************************************************************
07424 *    WHEN THE PROPER FORM IS NOT FOUND THE CHARACTERS INVOLVED *
07425 *    ARE TRANSFERRED INTACT AND A NOTE MADE INDICATING THIS    *
07426 *    ACTION.                                                   *
07427 ****************************************************************
07428
07429      IF  W-FIRST-BAD-VARIABLE EQUAL ZEROS
07430          MOVE 'Y'                TO W-FIRST-BAD-VARIABLE-IND
07431          GO TO 7430-EXIT
07432      ELSE
07433          GO TO 7430-EXIT.
07434
07435  7430-CONTINUE.
07436
07437      IF  W-VAR-RELATIVE-NUM LESS THAN 001
07438              OR
07439          W-VAR-RELATIVE-NUM GREATER THAN W-NUM-OF-VARIABLES
07440          SET W-TX-NDX DOWN BY +4
07441          MOVE ER-0180            TO EMI-ERROR
07442          MOVE -1                 TO MAINTL
07443          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07444          MOVE HIGH-VALUES        TO W-LAST-CHAR
07445          IF  W-FIRST-BAD-VARIABLE EQUAL ZEROS
07446              MOVE 'Y'                TO W-FIRST-BAD-VARIABLE-IND
07447              GO TO 7430-EXIT
07448          ELSE
07449              GO TO 7430-EXIT.
07450
07451      SET W-VG-NDX                TO W-VAR-RELATIVE-NUM.
07452
07453      SET W-WC-NDX DOWN BY +1.
07454
07455      IF  (W-FORM-SQUEEZE-OFF
07456                  AND
07457              NOT W-SQUEEZE-FIELD)
07458              OR
07459          (W-FORM-SQUEEZE-ON
07460                  AND
07461              W-AS-IS)
07462          PERFORM 7435-MOVE-VARIABLE-DIRECT THRU 7435-EXIT
07463                  VARYING
07464              W-VC-NDX FROM +1 BY +1
07465                  UNTIL
07466              W-VC-NDX GREATER THAN
07467                  W-VARIABLE-SIZE (W-VG-NDX)
07468
07469          IF  W-VARIABLE-SIZE (W-VG-NDX) GREATER THAN +5
07470              SET W-TX-NDX UP BY W-VARIABLE-SIZE (W-VG-NDX)
07471              SET W-TX-NDX DOWN BY +5
07472              IF  W-TX-NDX GREATER THAN +70
07473                  SET W-TX-NDX    TO +70
07474                  GO TO 7430-EXIT
07475              ELSE
07476                  GO TO 7430-EXIT
07477          ELSE
07478              COMPUTE W-ADJUST-SHORT
07479                  = 5 - W-VARIABLE-SIZE (W-VG-NDX)
07480              SET W-WC-NDX UP BY W-ADJUST-SHORT
07481              GO TO 7430-EXIT.
07482
07483      IF  W-SQUEEZE-FIELD
07484          SET W-TX-NDX UP BY +1.
07485
07486      MOVE SPACES                 TO W-FIRST-CHAR-FOUND-IND.
07487
07488      PERFORM 7470-MOVE-CHAR THRU 7470-EXIT
07489              VARYING
07490          W-VC-NDX FROM +1 BY +1
07491              UNTIL
07492          W-VC-NDX GREATER THAN
07493              W-VARIABLE-SIZE (W-VG-NDX).
07494
07495      MOVE HIGH-VALUES            TO W-LAST-CHAR.
07496
07497      IF  W-FORM-SQUEEZE-ON
07498          GO TO 7430-EXIT.
07499
07500      IF  W-VARIABLE-SIZE (W-VG-NDX) GREATER THAN +6
07501          SET W-TX-NDX UP BY W-VARIABLE-SIZE (W-VG-NDX)
07502          SET W-TX-NDX DOWN BY +6
07503          IF  W-TX-NDX GREATER THAN +70
07504              SET W-TX-NDX        TO +70.
07505
07506      IF  W-WORK-CHAR (W-WC-NDX) EQUAL SPACES
07507          SET W-WC-NDX DOWN BY +1.
07508
07509  7430-EXIT.
07510      EXIT.
07511                                  EJECT
07512  7435-MOVE-VARIABLE-DIRECT.
07513
07514      IF  W-USE-UPPER-AND-LOWER-CASE (W-VAR-RELATIVE-NUM)
07515              AND
07516          W-VAR-CHAR (W-VG-NDX W-VC-NDX) NOT EQUAL SPACES
07517              AND
07518          W-VAR-CHAR (W-VG-NDX W-VC-NDX) ALPHABETIC
07519              AND
07520          (W-VC-NDX GREATER THAN +1
07521                  AND
07522              W-LAST-CHAR NOT EQUAL SPACE
07523                  AND
07524              NOT W-LAST-CHAR-PUNC)
07525          IF  W-LC-USE-BOTH-CASES
07526              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07527                  CONVERTING W-UPPER-CASE TO W-LOWER-CASE
07528          ELSE
07529              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07530                  CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
07531
07532      SET W-WC-NDX UP BY +1.
07533      MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07534                                  TO W-WORK-CHAR (W-WC-NDX)
07535                                     W-LAST-CHAR.
07536
07537  7435-EXIT.
07538      EXIT.
07539                                  EJECT
07540  7437-FIND-NEXT-CHARACTER.
07541 *DELIBERATELY LEFT BLANK.
07542  7437-EXIT.
07543      EXIT.
07544                                  EJECT
07545  7440-BACK-TO-NEAREST-WORD.
07546
07547      MOVE SPACES                 TO W-WORK-CHAR (W-WC-NDX).
07548
07549  7440-EXIT.
07550      EXIT.
07551                                  EJECT
07552  7450-SQUEEZING.
07553
07554      IF  W-NEW-PARAGRAPH
07555          PERFORM 7600-SET-PARA-INDENT THRU 7600-EXIT
07556          MOVE SPACES             TO W-LINE-SQUEEZE-IND
07557          IF  W-SQUEEZED-LINE GREATER THAN SPACES
07558              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07559              MOVE W-TX-PC (W-TG-NDX)
07560                                  TO W-PRINT-CONTROL
07561              MOVE SPACES         TO W-SQUEEZED-LINE
07562              SET W-SQ-NDX        TO W-INITIAL-COLUMN
07563              SET W-SQ-NDX UP BY W-WORK-INDENT
07564          ELSE
07565              MOVE W-TX-PC (W-TG-NDX)
07566                                  TO W-PRINT-CONTROL
07567              SET W-SQ-NDX        TO W-INITIAL-COLUMN
07568              SET W-SQ-NDX UP BY W-WORK-INDENT
07569      ELSE
07570          IF  W-DO-NOT-ADJUST
07571              MOVE SPACES         TO W-LINE-SQUEEZE-IND
07572              IF  W-SQUEEZED-LINE GREATER THAN SPACES
07573                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07574                  MOVE W-WORK-LINE
07575                                  TO W-SQUEEZED-LINE
07576                  MOVE W-TX-PC (W-TG-NDX)
07577                                  TO W-PRINT-CONTROL
07578                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07579                  SET W-SQ-NDX    TO W-INITIAL-COLUMN
07580                  MOVE SPACES     TO W-SQUEEZED-LINE
07581                  GO TO 7450-EXIT
07582              ELSE
07583                  MOVE W-WORK-LINE
07584                                  TO W-SQUEEZED-LINE
07585                  MOVE W-TX-PC (W-TG-NDX)
07586                                  TO W-PRINT-CONTROL
07587                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07588                  MOVE SPACES     TO W-SQUEEZED-LINE
07589                                     W-WORK-LINE
07590                  SET W-SQ-NDX    TO W-INITIAL-COLUMN
07591                  GO TO 7450-EXIT
07592          ELSE
07593              IF  W-ADJUST-TO-LINE-LENGTH
07594                  MOVE SPACES     TO W-LINE-SQUEEZE-IND
07595
07596                  IF  W-SQUEEZED-LINE GREATER THAN SPACES
07597                      PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07598                      SET W-SQ-NDX
07599                                  TO W-INITIAL-COLUMN
07600                      PERFORM 7505-MOVE-STRAIGHT THRU 7505-EXIT
07601                              VARYING
07602                          W-WC-NDX FROM 1 BY 1
07603                              UNTIL
07604                          W-SQ-NDX EQUAL W-TOO-FAR
07605                      MOVE W-TX-PC (W-TG-NDX)
07606                                  TO W-PRINT-CONTROL
07607                      PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07608                      SET W-SQ-NDX
07609                                  TO W-INITIAL-COLUMN
07610                      MOVE SPACES TO W-SQUEEZED-LINE
07611                                     W-WORK-LINE
07612                      GO TO 7450-EXIT
07613                  ELSE
07614                      SET W-SQ-NDX
07615                                  TO W-INITIAL-COLUMN
07616                      PERFORM 7505-MOVE-STRAIGHT THRU 7505-EXIT
07617                              VARYING
07618                          W-WC-NDX FROM 1 BY 1
07619                              UNTIL
07620                          W-SQ-NDX EQUAL W-TOO-FAR
07621                      MOVE W-TX-PC (W-TG-NDX)
07622                                  TO W-PRINT-CONTROL
07623                      PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07624                      MOVE SPACES TO W-SQUEEZED-LINE
07625                                     W-WORK-LINE
07626                      SET W-SQ-NDX
07627                                  TO W-INITIAL-COLUMN
07628                      GO TO 7450-EXIT
07629              ELSE
07630                  IF  W-CONTINUE-PARAGRAPH
07631                      IF  W-SQUEEZED-LINE GREATER THAN SPACES
07632                          PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07633                          PERFORM 7610-SET-CONP-INDENT
07634                              THRU 7610-EXIT
07635                          MOVE SPACES
07636                                  TO W-LINE-SQUEEZE-IND
07637                          MOVE W-TX-PC (W-TG-NDX)
07638                                  TO W-PRINT-CONTROL
07639                          MOVE SPACES
07640                                  TO W-SQUEEZED-LINE
07641                          SET W-SQ-NDX
07642                                  TO W-INITIAL-COLUMN
07643                          SET W-SQ-NDX UP BY W-WORK-INDENT
07644                      ELSE
07645                          PERFORM 7610-SET-CONP-INDENT
07646                              THRU 7610-EXIT
07647                          MOVE SPACES
07648                                  TO W-LINE-SQUEEZE-IND
07649                          MOVE W-TX-PC (W-TG-NDX)
07650                                  TO W-PRINT-CONTROL
07651                          MOVE SPACES
07652                                  TO W-SQUEEZED-LINE
07653                          SET W-SQ-NDX
07654                                  TO W-INITIAL-COLUMN
07655                          SET W-SQ-NDX UP BY W-WORK-INDENT.
07656
07657
07658      PERFORM 7480-UPDATE-SQUEEZED-LINE THRU 7480-EXIT
07659              VARYING
07660          W-WC-NDX FROM +1 BY +1
07661              UNTIL
07662          W-WC-NDX GREATER THAN +70.
07663
07664  7450-EXIT.
07665      EXIT.
07666                                  EJECT
07667  7460-REMAIN-CHAR-CHECK.
07668
07669      IF  W-TX-NDX EQUAL W-WC-NDX
07670          MOVE W-TX-PC (W-TG-NDX)
07671                                  TO W-PRINT-CONTROL
07672          MOVE W-WORK-LINE        TO W-SQUEEZED-LINE
07673          PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07674          MOVE SPACES             TO W-WORK-LINE
07675          SET W-WC-NDX            TO +1
07676      ELSE
07677          SET W-TX-NDX1           TO W-TX-NDX
07678          SEARCH W-TX-CHAR
07679              VARYING W-TX-NDX1
07680              AT END
07681                  MOVE W-TX-PC (W-TG-NDX)
07682                                  TO W-PRINT-CONTROL
07683                  MOVE W-WORK-LINE
07684                                  TO W-SQUEEZED-LINE
07685                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07686                  MOVE SPACES     TO W-WORK-LINE
07687                  SET W-WC-NDX    TO +1
07688                  SET W-TX-NDX    TO +1
07689                  SET W-TG-NDX UP BY +1
07690              WHEN
07691                  W-TX-CHAR (W-TG-NDX W-TX-NDX1) NOT EQUAL SPACES
07692                  MOVE ER-9427    TO EMI-ERROR
07693                  MOVE -1         TO MAINTL
07694                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07695                  MOVE W-TX-PC (W-TG-NDX)
07696                                  TO W-PRINT-CONTROL
07697                  MOVE W-WORK-LINE
07698                                  TO W-SQUEEZED-LINE
07699                  PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07700                  MOVE SPACES     TO W-WORK-LINE
07701                  SET W-WC-NDX    TO +1.
07702
07703  7460-EXIT.
07704      EXIT.
07705                                  EJECT
07706  7470-MOVE-CHAR.
07707
07708      IF  W-FIRST-CHAR-NOT-FOUND
07709              AND
07710          W-VAR-CHAR (W-VG-NDX W-VC-NDX) EQUAL SPACES
07711          MOVE SPACES             TO W-LAST-CHAR
07712          GO TO 7470-EXIT.
07713
07714      MOVE 'Y'                    TO W-FIRST-CHAR-FOUND-IND.
07715
07716      IF  W-SQUEEZE-FIELD
07717              AND
07718          W-LAST-CHAR EQUAL SPACES
07719              AND
07720          W-VAR-CHAR (W-VG-NDX W-VC-NDX) EQUAL SPACES
07721          GO TO 7470-EXIT.
07722
07723      SET W-WC-NDX UP BY +1.
07724
07725      IF  W-WC-NDX GREATER THAN +70
07726          IF  W-FORM-SQUEEZE-ON
07727              GO TO 7470-EXIT
07728          ELSE
07729              MOVE ER-9427        TO EMI-ERROR
07730              MOVE -1             TO MAINTL
07731              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
07732              GO TO 7470-EXIT.
07733
07734      IF  W-USE-UPPER-AND-LOWER-CASE (W-VAR-RELATIVE-NUM)
07735              AND
07736          W-VAR-CHAR (W-VG-NDX W-VC-NDX) NOT EQUAL SPACES
07737              AND
07738          W-VAR-CHAR (W-VG-NDX W-VC-NDX) ALPHABETIC
07739              AND
07740          (W-VC-NDX GREATER THAN +1
07741                  AND
07742              W-LAST-CHAR NOT EQUAL SPACE
07743                  AND
07744              NOT W-LAST-CHAR-PUNC)
07745          IF  W-LC-USE-BOTH-CASES
07746              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07747                  CONVERTING W-UPPER-CASE TO W-LOWER-CASE
07748          ELSE
07749              INSPECT W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07750                  CONVERTING W-LOWER-CASE TO W-UPPER-CASE.
07751
07752      MOVE W-VAR-CHAR (W-VG-NDX W-VC-NDX)
07753                                  TO W-WORK-CHAR (W-WC-NDX)
07754                                     W-LAST-CHAR.
07755
07756      IF  W-LAST-CHAR EQUAL SPACES
07757          SET W-LAST-WC-SPACE     TO W-WC-NDX.
07758
07759  7470-EXIT.
07760      EXIT.
07761                                  EJECT
07762  7480-UPDATE-SQUEEZED-LINE.
07763 ****************************************************************
07764 *    THIS SECTION CONTROLS THE MOVEMENT FROM THE WORK LINE TO *
07765 *    THE SQUEEZED LINE.  DURING THIS PROCESS ALL EXTRA SPACES  *
07766 *    (DEFINED AS TWO OR MORE SPACES NOT FOLLOWING A END OF     *
07767 *    SENTENCE INDICATOR) ARE REMOVED.  IF THE LAST CHARACTER   *
07768 *    IS NOT A SPACE OR PUNCTUATION MARK AN UNFINISHED WORD IS  *
07769 *    ASSUMED.  EVERYTHING TO THE PREVIOUS SPACE IS REMOVED     *
07770 ****************************************************************
07771
07772      SET W-SQ-NDX UP BY +1.
07773
07774      IF  W-SQ-NDX GREATER THAN W-LAST-COLUMN
07775          MOVE W-SQ-CHAR (W-LAST-COLUMN)
07776                                  TO W-CHARACTER-TYPE
07777          IF  W-PUNCTUATION
07778                  OR
07779              W-SPACE
07780              PERFORM 7485-FIND-NEXT-CHARACTER THRU 7485-EXIT
07781              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07782              MOVE SPACES         TO W-SQUEEZED-LINE
07783              SET W-SQ-NDX        TO W-START-COLUMN
07784              SET W-SQ-NDX UP BY W-NEXT-INDENT
07785              SET W-LAST-SQUEEZED-SPACE
07786                                  TO W-SQ-NDX
07787          ELSE
07788              PERFORM 7490-REMOVE-LAST-WORD THRU 7490-EXIT
07789              PERFORM 7510-MOVE-TO-TABLE THRU 7510-EXIT
07790              MOVE SPACES         TO W-SQUEEZED-LINE
07791              SET W-SQ-NDX        TO W-START-COLUMN
07792              SET W-SQ-NDX UP BY W-NEXT-INDENT
07793              SET W-LAST-SQUEEZED-SPACE
07794                                  TO W-SQ-NDX.
07795
07796      IF  W-WC-NDX GREATER THAN +70
07797          GO TO 7480-EXIT.
07798
07799      MOVE W-WORK-CHAR (W-WC-NDX) TO W-CHARACTER-TYPE.
07800
07801      IF  W-END-OF-SENTENCE
07802          MOVE 'Y'                TO W-END-OF-SENTENCE-IND
07803          SET W-WORK-NDX          TO W-SQ-NDX
07804          COMPUTE W-DISPLAY-NDX = W-WORK-NDX - 1
07805          IF  W-DISPLAY-NDX EQUAL W-LAST-SQUEEZED-SPACE
07806              SET W-SQ-NDX        TO W-LAST-SQUEEZED-SPACE
07807          ELSE
07808              NEXT SENTENCE
07809      ELSE
07810          IF  W-PUNCTUATION
07811                  AND
07812              W-LAST-SQ-CHAR EQUAL SPACES
07813              SET W-SQ-NDX DOWN BY +1
07814              MOVE SPACES         TO W-END-OF-SENTENCE-IND
07815          ELSE
07816              IF  W-WORK-CHAR (W-WC-NDX) EQUAL SPACES
07817                  IF  W-LAST-SQ-CHAR EQUAL SPACES
07818                      IF  W-END-OF-SENTENCE-WORKING
07819                          MOVE SPACES
07820                                  TO W-END-OF-SENTENCE-IND
07821                          SET W-LAST-SQUEEZED-SPACE
07822                                  TO W-SQ-NDX
07823                      ELSE
07824                          SET W-SQ-NDX DOWN BY +1
07825                          GO TO 7480-EXIT
07826                  ELSE
07827                      SET W-LAST-SQUEEZED-SPACE
07828                                  TO W-SQ-NDX
07829              ELSE
07830                  MOVE SPACES     TO W-END-OF-SENTENCE-IND.
07831
07832      MOVE W-WORK-CHAR (W-WC-NDX) TO W-SQ-CHAR (W-SQ-NDX)
07833                                     W-LAST-SQ-CHAR.
07834
07835      IF  W-WC-NDX EQUAL +70
07836              AND
07837          W-WORK-CHAR (W-WC-NDX) GREATER THAN SPACES
07838              AND
07839          W-SQ-NDX NOT EQUAL W-LAST-COLUMN
07840          SET W-SQ-NDX UP BY +1
07841          SET W-LAST-SQUEEZED-SPACE
07842                                  TO W-SQ-NDX
07843          MOVE SPACES             TO W-SQ-CHAR (W-SQ-NDX)
07844                                     W-LAST-SQ-CHAR.
07845
07846  7480-EXIT.
07847      EXIT.
07848                                  EJECT
07849  7485-FIND-NEXT-CHARACTER.
07850
07851      MOVE W-WORK-CHAR (W-WC-NDX) TO W-CHARACTER-TYPE.
07852
07853      IF  W-PUNCTUATION
07854          PERFORM 7490-REMOVE-LAST-WORD THRU 7490-EXIT
07855      ELSE
07856          SET W-WC-NDX            TO W-WC-NDX
07857          SEARCH W-WORK-CHAR
07858              VARYING W-WC-NDX
07859              AT END
07860                  GO TO 7485-EXIT
07861              WHEN
07862                  W-WORK-CHAR (W-WC-NDX) NOT EQUAL SPACES
07863                  GO TO 7485-EXIT.
07864
07865  7485-EXIT.
07866      EXIT.
07867                                  EJECT
07868  7490-REMOVE-LAST-WORD.
07869
07870 *********  NOTE: W-WORK-LINE ALWAYS STARTS WITH A FULL WORD*******
07871      IF  W-WC-NDX EQUAL +1
07872          GO TO 7490-EXIT.
07873
07874      ADD +1                      TO W-LAST-SQUEEZED-SPACE.
07875
07876      PERFORM 7500-CLEAR-SQUEEZED-EXCESS THRU 7500-EXIT
07877              VARYING
07878          W-SQ-NDX FROM W-LAST-SQUEEZED-SPACE BY +1
07879              UNTIL
07880          W-SQ-NDX EQUAL W-TOO-FAR.
07881
07882      SET W-WORK-NDX              TO W-WC-NDX.
07883      COMPUTE W-LAST-WC-SPACE
07884          = W-WORK-NDX - W-TOO-FAR + W-LAST-SQUEEZED-SPACE.
07885      SET W-WC-NDX                TO W-LAST-WC-SPACE.
07886
07887  7490-EXIT.
07888      EXIT.
07889                                  EJECT
07890  7500-CLEAR-SQUEEZED-EXCESS.
07891
07892      MOVE SPACES                 TO W-SQ-CHAR (W-SQ-NDX).
07893
07894  7500-EXIT.
07895      EXIT.
07896
07897  7505-MOVE-STRAIGHT.
07898
07899      SET W-SQ-NDX UP BY +1.
07900
07901      MOVE W-WORK-CHAR (W-WC-NDX) TO W-SQ-CHAR (W-SQ-NDX).
07902
07903  7505-EXIT.
07904      EXIT.
07905                                  EJECT
07906  7510-MOVE-TO-TABLE.
07907
07908      IF  W-PRINT-CONTROL NOT NUMERIC
07909          MOVE 00                 TO W-PRINT-CONTROL
07910          ADD +1                  TO W-LINE-COUNT
07911      ELSE
07912          IF  W-PRINT-CONTROL EQUAL 99
07913              PERFORM 7640-PAGING THRU 7640-EXIT
07914              MOVE 00             TO W-PRINT-CONTROL
07915              SET W-RG-NDX UP BY +1
07916              MOVE W-TOP-FORM     TO W-RC-TEXT (W-RG-NDX)
07917              MOVE +1             TO W-LINE-COUNT
07918              SET W-RG-NDX UP BY W-TOP-MARGIN
07919          ELSE
022712           IF W-SQUEEZED-LINE (1:6) NOT EQUAL '&&&&&&'
07920              COMPUTE W-LINE-COUNT
07921                  = W-LINE-COUNT + W-PRINT-CONTROL + 1.
07922
07923      IF  W-LINE-COUNT GREATER THAN W-MAX-LINES-PER-PAGE
07924          PERFORM 7640-PAGING THRU 7640-EXIT
07925          MOVE 00                 TO W-PRINT-CONTROL
07926          SET W-RG-NDX UP BY +1
07927          MOVE W-TOP-FORM         TO W-RC-TEXT (W-RG-NDX)
07928          SET W-RG-NDX UP BY W-TOP-MARGIN
07929          MOVE +1                 TO W-LINE-COUNT.
07930
07931      SET W-RG-NDX UP BY W-PRINT-CONTROL.
07932      SET W-RG-NDX UP BY +1.
07933      MOVE W-SQUEEZED-LINE        TO W-RC-TEXT (W-RG-NDX).
07934      MOVE 00                     TO W-PRINT-CONTROL.
07935
07936      IF  W-FIRST-BAD-VARIABLE-FOUND
07937              AND
07938          W-FIRST-BAD-VARIABLE EQUAL ZEROS
07939          SET W-FIRST-BAD-VARIABLE
07940                                  TO W-RG-NDX.
07941
07942  7510-EXIT.
07943      EXIT.
07944                                  EJECT
07945  7600-SET-PARA-INDENT.
07946
07947      IF  W-LINE-SQUEEZE-IND EQUAL 'P'
07948          MOVE W-PARAGRAPH-INDENT TO W-WORK-INDENT
07949      ELSE
07950      IF  W-LINE-SQUEEZE-IND EQUAL 'Q'
07951          MOVE W-LINE-INDENT-1    TO W-WORK-INDENT
07952          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07953      ELSE
07954      IF  W-LINE-SQUEEZE-IND EQUAL 'R'
07955          MOVE W-LINE-INDENT-2    TO W-WORK-INDENT
07956          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07957      ELSE
07958      IF  W-LINE-SQUEEZE-IND EQUAL 'S'
07959          MOVE W-LINE-INDENT-3    TO W-WORK-INDENT
07960          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07961      ELSE
07962      IF  W-LINE-SQUEEZE-IND EQUAL 'T'
07963          MOVE W-LINE-INDENT-4    TO W-WORK-INDENT
07964          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT
07965      ELSE
07966      IF  W-LINE-SQUEEZE-IND EQUAL 'U'
07967          MOVE W-LINE-INDENT-5    TO W-WORK-INDENT
07968          ADD W-PARAGRAPH-INDENT  TO W-WORK-INDENT.
07969
07970      SET W-TG-NDX2               TO W-TG-NDX.
07971      SET W-TG-NDX2 UP BY +1.
07972
07973      IF  W-TG-NDX EQUAL W-TOTAL-TX-LINES
07974          MOVE W-WORK-INDENT      TO W-NEXT-INDENT
07975      ELSE
07976          PERFORM 7630-NEXT-LINE THRU 7630-EXIT.
07977
07978  7600-EXIT.
07979      EXIT.
07980                                  EJECT
07981  7610-SET-CONP-INDENT.
07982
07983      IF  W-LINE-SQUEEZE-IND EQUAL 'C'
07984          MOVE +0                 TO W-WORK-INDENT
07985      ELSE
07986      IF  W-LINE-SQUEEZE-IND EQUAL 'D'
07987          MOVE W-LINE-INDENT-1    TO W-WORK-INDENT
07988      ELSE
07989      IF  W-LINE-SQUEEZE-IND EQUAL 'E'
07990          MOVE W-LINE-INDENT-2    TO W-WORK-INDENT
07991      ELSE
07992      IF  W-LINE-SQUEEZE-IND EQUAL 'F'
07993          MOVE W-LINE-INDENT-3    TO W-WORK-INDENT
07994      ELSE
07995      IF  W-LINE-SQUEEZE-IND EQUAL 'G'
07996          MOVE W-LINE-INDENT-4    TO W-WORK-INDENT
07997      ELSE
07998      IF  W-LINE-SQUEEZE-IND EQUAL 'H'
07999          MOVE W-LINE-INDENT-5    TO W-WORK-INDENT.
08000
08001      SET W-TG-NDX2               TO W-TG-NDX.
08002      SET W-TG-NDX2 UP BY +1.
08003
08004      IF  W-TG-NDX EQUAL W-TOTAL-TX-LINES
08005          MOVE W-WORK-INDENT      TO W-NEXT-INDENT
08006      ELSE
08007          PERFORM 7630-NEXT-LINE THRU 7630-EXIT.
08008
08009  7610-EXIT.
08010      EXIT.
08011                                  EJECT
08012  7630-NEXT-LINE.
08013
08014      IF  W-TX-SC (W-TG-NDX2) EQUAL ' '
08015          MOVE ZEROS              TO W-NEXT-INDENT
08016      ELSE
08017      IF  W-TX-SC (W-TG-NDX2) EQUAL '1'
08018          MOVE W-LINE-INDENT-1    TO W-NEXT-INDENT
08019      ELSE
08020      IF  W-TX-SC (W-TG-NDX2) EQUAL '2'
08021          MOVE W-LINE-INDENT-2    TO W-NEXT-INDENT
08022      ELSE
08023      IF  W-TX-SC (W-TG-NDX2) EQUAL '3'
08024          MOVE W-LINE-INDENT-3    TO W-NEXT-INDENT
08025      ELSE
08026      IF  W-TX-SC (W-TG-NDX2) EQUAL '4'
08027          MOVE W-LINE-INDENT-4    TO W-NEXT-INDENT
08028      ELSE
08029      IF  W-TX-SC (W-TG-NDX2) EQUAL '5'
08030          MOVE W-LINE-INDENT-5    TO W-NEXT-INDENT
08031      ELSE
08032          MOVE W-WORK-INDENT      TO W-NEXT-INDENT
08033          SUBTRACT W-PARAGRAPH-INDENT
08034                                   FROM W-NEXT-INDENT.
08035
08036  7630-EXIT.
08037      EXIT.
08038                                  EJECT
08039  7640-PAGING.
08040
08041      IF  NOT W-LC-CREATE-PAGES
08042          GO TO 7640-EXIT.
08043
08044      IF  W-LINE-COUNT GREATER ZEROS
08045              AND
08046          W-RG-NDX GREATER THAN +1
08047          ADD +1                  TO W-PAGE
08048          MOVE W-PAGE             TO W-PAGE-NUMBER
08049          COMPUTE W-PAGE-LINE
08050              = W-MAX-LINES-PER-PAGE - W-LINE-COUNT + 3
08051          IF  W-PAGE-LINE GREATER THAN +2
08052              SET W-RG-NDX UP BY W-PAGE-LINE
08053              MOVE W-PAGE-PRT     TO W-RC-TEXT (W-RG-NDX)
08054          ELSE
08055              SET W-RG-NDX UP BY +2
08056              MOVE W-PAGE-PRT     TO W-RC-TEXT (W-RG-NDX).
08057
08058  7640-EXIT.
08059      EXIT.
08060                                  EJECT
08061  7700-READ-TEXT-TS.
08062 **************************************************************
08063 *  THIS PARAGRAPH READS THE TEMPORARY STORAGE RECORDS THAT   *
08064 *  CONTAIN THE TEXT THAT IS PAST BETWEEN EL689, EL6892       *
08065 *  AND EL1042.                                               *
08066 **************************************************************
08067
08068      
      * EXEC CICS HANDLE CONDITION
08069 *         QIDERR     (7700-TS-ERROR)
08070 *         ITEMERR    (7700-TS-ERROR)
08071 *    END-EXEC.
      *    MOVE '"$N<                  ! 9 #00015259' TO DFHEIV0
           MOVE X'22244E3C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3920233030303135323539' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08072
08073      SET W-TS-NDX                TO 1.
08074      MOVE 1                      TO W-TS-ITEM.
08075
08076  7700-LOOP.
08077
08078      IF  W-TS-ITEM GREATER THAN PI-TEMP-STOR-ITEMS
08079          IF  PI-TEMP-STOR-ITEMS GREATER THAN ZEROS
08080              PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
08081              GO TO 7700-EXIT
08082          ELSE
08083              GO TO 7700-EXIT.
08084
08085      
      * EXEC CICS READQ TS
08086 *         INTO     (W-TS-WORK-AREA)
08087 *         QUEUE    (W-TS-NAME-TEXT)
08088 *         LENGTH   (W-TS-LENGTH)
08089 *         ITEM     (W-TS-ITEM)
08090 *    END-EXEC.
      *    MOVE '*$II   L              ''   #00015276' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135323736' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TS-NAME-TEXT, 
                 W-TS-WORK-AREA, 
                 W-TS-LENGTH, 
                 W-TS-ITEM, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08091
08092      MOVE W-TS-WORK-AREA         TO W-TS-GROUP (W-TS-NDX).
08093      SET W-TS-NDX UP BY 1.
08094      ADD 1                       TO W-TS-ITEM.
08095      GO TO 7700-LOOP.
08096
08097  7700-TS-ERROR.
08098
08099      IF  EIBTRNID = W-TRANSACTION OR 'EX14'
08100          MOVE ER-0033            TO EMI-ERROR
08101          MOVE -1                 TO MAINTL
08102          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
08103          GO TO 8200-SEND-DATAONLY.
08104
08105  7700-EXIT.
08106       EXIT.
08107                                  EJECT
08108  7740-RESTORE-SCREEN.
08109
08110      MOVE 'C'                    TO MAINTI.
08111      MOVE +1                     TO MAINTL.
08112      MOVE AL-UANON               TO MAINTA.
08113
08114      MOVE PI-689-FORM-NUMBER     TO FORMI.
08115      MOVE +4                     TO FORML.
08116      MOVE AL-UANON               TO FORMA.
08117
08118 *    IF  PI-689-PRINT-RESTRICTION GREATER THAN SPACES
08119 *        MOVE PI-689-PRINT-RESTRICTION
08120 *                                TO PRTRESTL
08121 *        MOVE +1                 TO PRTRESTL
08122 *        MOVE AL-UANON           TO PRTRESTA.
08123
08124      IF  PI-689-FOLLOW-UP-EDIT GREATER THAN SPACES
08125          MOVE PI-689-FOLLOW-UP-EDIT
08126                                  TO FOLLOWI
08127          MOVE +8                 TO FOLLOWL
08128          MOVE AL-UANON           TO FOLLOWA.
08129
08130      IF  PI-689-RESEND1-EDIT GREATER THAN SPACES
08131          MOVE PI-689-RESEND1-EDIT
08132                                  TO RESEND1I
08133          MOVE +8                 TO RESEND1L
08134          MOVE AL-UANON           TO RESEND1A.
08148      IF  PI-689-ALT-PRINTER-ID GREATER THAN SPACES
08149          MOVE PI-689-ALT-PRINTER-ID
08150                                  TO PRINTERI
08151          MOVE +1                 TO PRINTERL
08152          MOVE AL-UANON           TO PRINTERA.
08153
08154      IF  PI-BYPASS-LABELS
08155           MOVE PI-LABEL-CONTROL  TO ADDRLBLI
08156           MOVE +1                TO ADDRLBLL
08157           MOVE AL-UANON          TO ADDRLBLA
08158      ELSE
08159           IF  PI-689-LBL-OVERRIDE GREATER THAN SPACES
08160                MOVE PI-689-LBL-OVERRIDE
08161                                  TO ADDRLBLI
08162                MOVE +1           TO ADDRLBLL
08163                MOVE AL-UANON     TO ADDRLBLA.
08164
08165      IF  PI-689-NUMBER-COPIES GREATER THAN ZEROS
08166          MOVE PI-689-NUMBER-COPIES   TO COPIESI
08167          MOVE +1                     TO COPIESL
08168          MOVE AL-UNNON               TO COPIESA.
08169
08170      IF  PI-689-LABEL-SOURCE GREATER THAN SPACES
08171          MOVE PI-689-LABEL-SOURCE
08172                                  TO ADDRSI
08173          MOVE +1                 TO ADDRSL
08174          MOVE AL-UANON           TO ADDRSA.
08175
08176      MOVE PI-689-DATA-SOURCE     TO DATASORI.
08177      MOVE +1                     TO DATASORL.
08178      MOVE AL-UANON               TO DATASORA.
08179
08180      IF  PI-689-CARRIER GREATER THAN SPACES
08181          MOVE PI-689-CARRIER     TO CARRIERI
08182          MOVE +1                 TO CARRIERL
08183          MOVE AL-UANON           TO CARRIERA.
08184
08185      IF  PI-689-GROUPING GREATER THAN SPACES
08186          MOVE PI-689-GROUPING    TO GROUPI
08187          MOVE +6                 TO GROUPL
08188          MOVE AL-UANON           TO GROUPA.
08189
08190      IF  PI-689-STATE GREATER THAN SPACES
08191          MOVE PI-689-STATE       TO STATEI
08192          MOVE +2                 TO STATEL
08193          MOVE AL-UANON           TO STATEA.
08194
08195      IF  PI-689-ACCOUNT GREATER THAN SPACES
08196          MOVE PI-689-ACCOUNT     TO ACCTI
08197          MOVE +10                TO ACCTL
08198          MOVE AL-UANON           TO ACCTA.
08199
08200      IF  PI-689-CERT-PRIME GREATER THAN SPACES
08201          MOVE PI-689-CERT-PRIME  TO CERTI
08202          MOVE +10                TO CERTL
08203          MOVE AL-UANON           TO CERTA.
08204
08205      IF  PI-689-CERT-PRIME GREATER THAN SPACES
08206          MOVE PI-689-CERT-SFX    TO SFXI
08207          MOVE +1                 TO SFXL
08208          MOVE AL-UANON           TO SFXA.
08209
08210      IF  PI-689-TYPE GREATER THAN SPACES
08211          MOVE PI-689-TYPE        TO TYPEI
08212          MOVE +1                 TO TYPEL
08213          MOVE AL-UANON           TO TYPEA.
08214
08215      IF  PI-689-RESP-PERSON GREATER THAN SPACES
08216          MOVE PI-689-RESP-PERSON TO RPERSONI
08217          MOVE +10                TO RPERSONL
08218          MOVE AL-UANON           TO RPERSONA.
08219
08220      IF  PI-689-ENTRY-BATCH GREATER THAN SPACES
08221          MOVE PI-689-ENTRY-BATCH TO BENTRYI
08222          MOVE +6                 TO BENTRYL
08223          MOVE AL-UANON           TO BENTRYA.
08224
08225      IF  PI-689-SEQ-EDIT GREATER THAN SPACES
08226          MOVE PI-689-SEQ-EDIT    TO SEQI
08227          MOVE +8                 TO SEQL
08228          MOVE AL-UNNON           TO SEQA.
08229
08230      IF  PI-689-BCSEQ-EDIT GREATER THAN SPACES
08231          MOVE PI-689-BCSEQ-EDIT  TO BCSEQI
08232          MOVE +4                 TO BCSEQL
08233          MOVE AL-UNNON           TO BCSEQA.
08234
08235      IF  PI-689-DATE-EDIT GREATER THAN SPACES
08236          MOVE PI-689-DATE-EDIT   TO DATEI
08237          MOVE +8                 TO DATEL
08238          MOVE AL-UNNON           TO DATEA.
122011
122011     IF PI-CERT-FORM-ID GREATER THAN SPACES
122011        MOVE PI-CERT-FORM-ID     TO CERTIDI
122011        MOVE +5                  TO CERTIDL
122011        MOVE AL-UNNON            TO CERTIDA
122011     END-IF.
122011
122011     IF PI-PRINT-NOW GREATER THAN SPACES
122011        MOVE PI-PRINT-NOW        TO PRTNOWI
122011        MOVE +1                  TO PRTNOWL
122011        MOVE AL-UNNON            TO PRTNOWA
122011     END-IF.
101812     IF PI-ENDT-ARCH-NO GREATER THAN ZERO
101812        MOVE PI-ENDT-ARCH-NO     TO ENDARCHI
101812        MOVE +8                  TO ENDARCHL
101812        MOVE AL-UNNON            TO ENDARCHA
101812     END-IF
061412
061412     MOVE PI-ENCLOSURE-CD        TO ENCI.
061412     MOVE +3                     TO ENCL.
061412     MOVE AL-UANON               TO ENCA.
08239
08240  7749-EXIT.
08241      EXIT.
08242                                  EJECT
08243  7760-PUT-TEMP-STORAGE.
08244 **************************************************************
08245 *  THIS PARAGRAPH WRITES THE TEMPORARY STORAGE RECORDS THAT  *
08246 *  WILL CONTAIN THE CONTENTS OF THE TEXT TABLE FOR USE BY    *
08247 *  EL1042 AND EL6892.                                        *
08248 **************************************************************
08249
08250      SET W-TS-NDX                TO 1.
08251      MOVE 0                      TO PI-TEMP-STOR-ITEMS.
08252
08253      MOVE -1                     TO MAINTL.
08254      PERFORM 7840-WRITE-TS THRU 7840-EXIT
08255              VARYING
08256          W-TS-GROUP-WORK FROM 0 BY W-TS-NUM-REC-IN-GROUP
08257              UNTIL
08258          W-TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.
08259
08260  7760-EXIT.
08261       EXIT.
08262                                  EJECT
08263  7780-DELETE-TEMP-STOR-TEXT.
08264
08265      
      * EXEC CICS HANDLE CONDITION
08266 *         QIDERR      (7780-EXIT)
08267 *    END-EXEC.
      *    MOVE '"$N                   ! : #00015464' TO DFHEIV0
           MOVE X'22244E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3A20233030303135343634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08268
08269      
      * EXEC CICS DELETEQ TS
08270 *         QUEUE       (W-TS-NAME-TEXT)
08271 *    END-EXEC.
      *    MOVE '*&                    #   #00015468' TO DFHEIV0
           MOVE X'2A2620202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TS-NAME-TEXT, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08272
08273  7780-EXIT.
08274      EXIT.
08275                                  EJECT
08276  7840-WRITE-TS.
08277
08278      MOVE W-TS-GROUP (W-TS-NDX)  TO W-TS-WORK-AREA.
08279      SET W-TS-NDX UP BY 1.
08280      ADD 1                       TO PI-TEMP-STOR-ITEMS.
08281
08282      
      * EXEC CICS WRITEQ TS
08283 *         FROM    (W-TS-WORK-AREA)
08284 *         QUEUE   (W-TS-NAME-TEXT)
08285 *         LENGTH  (W-TS-LENGTH)
08286 *         ITEM    (PI-TEMP-STOR-ITEMS)
08287 *    END-EXEC.
      *    MOVE '*" I   L              ''   #00015481' TO DFHEIV0
           MOVE X'2A2220492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135343831' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TS-NAME-TEXT, 
                 W-TS-WORK-AREA, 
                 W-TS-LENGTH, 
                 PI-TEMP-STOR-ITEMS, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08288
08289  7840-EXIT.
08290      EXIT.
08291      EJECT
08292  7900-PRINT-LETTER-NOW.
08293 ***************************************************************
08294 *     THIS ROUTINE WILL CAUSE THE CURRENTLY CREATED LETTER    *
08295 *     TO BE PRINTED ON A HARDCOPY PRINTER.                    *
08296 *     THE TEXT IS EDITED FOR ANY UNRESOLVED SYMBOLS.          *
08297 *     THE PRINTER ID IS OBTAINED FROM THE CONTROL FILE.       *
08298 *     THE LETTER IS SAVED IN TEMP-STORAGE AND A START IS      *
08299 *     ISSUED FOR THE PRINT TRANSACTION.                       *
08300 ***************************************************************
08301
08302 *    IF (EIBAID = DFHPF4 OR DFHPF8)
08303 *            AND
08304 *        PI-SHOW-MODE
08305 *        IF  PRINTERL GREATER ZERO
08306 *            INSPECT PRINTERI CONVERTING W-LOWER-CASE TO
08307 *                                                    W-UPPER-CASE
08308 *            MOVE PRINTERI       TO PI-689-ALT-PRINTER-ID
08309 *                                   PI-ALT-DMD-PRT-ID.
08310 *
08311 *    IF  EIBAID EQUAL DFHPF8
08312 *            AND
08313 *        PI-SHOW-MODE
08314 *            AND
08315 *        LA-INITIAL-PRINT-DATE GREATER THAN LOW-VALUES
08316 *        MOVE ER-7247            TO EMI-ERROR
08317 *        MOVE -1                 TO MAINTL
08318 *        MOVE AL-UABON           TO MAINTA
08319 *        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
08320 *        GO TO 8200-SEND-DATAONLY.
08321
08322      IF  PI-TOTAL-LINES EQUAL 0
08323          MOVE ER-0187            TO EMI-ERROR
08324          MOVE -1                 TO MAINTL
08325          MOVE AL-UABON           TO MAINTA
08326          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
08327          GO TO 8200-SEND-DATAONLY.
08328
08329      IF  EMI-ERROR NOT EQUAL ER-0191
08330          IF  PI-689-CREATE-NO-SCREENS
08331 *                OR
08332 *            (EIBAID = DFHPF4 OR DFHPF8)
08333              SET W-RG-NDX        TO +1
08334          ELSE
08335              PERFORM 4100-SET-NDX THRU 4100-EXIT
08336              PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
08337                      VARYING
08338                  W-SC-NDX FROM 1 BY 1
08339                      UNTIL
08340                  W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN.
08341
08342      MOVE SPACES                 TO W-REMAINING-VAR-SW.
08343
08344      IF EIBAID NOT = DFHPF4 AND DFHPF8
08345          PERFORM 7915-SEARCH-REMAINING-VARS THRU 7915-EXIT
08346                  VARYING
08347              W-RVS-NDX FROM +1 BY +1
08348                  UNTIL
08349              W-RVS-NDX GREATER THAN +36500
08350                  OR
08351              W-REMAINING-VAR-FOUND
08352          IF  W-REMAINING-VAR-FOUND
08353              GO TO 4000-ROLL-PAGE.
08354
081004*    IF EIBAID = DFHPF4 OR DFHPF8
081004*       PERFORM 7700-READ-TEXT-TS
      *                                THRU 7700-EXIT
      *
111104*        PERFORM 4100-SET-NDX THRU 4100-EXIT
111104*        PERFORM 4200-UPDATE-TABLE-FROM-SCREEN THRU 4200-EXIT
111104*                VARYING
111104*            W-SC-NDX FROM 1 BY 1
111104*                UNTIL
111104*            W-SC-NDX GREATER THAN W-NUM-LINES-PER-SCREEN
      *
081004*       PERFORM 7760-PUT-TEMP-STORAGE
      *                                THRU 7760-EXIT
081004*    END-IF
08355      IF PI-689-ALT-PRINTER-ID NOT = SPACES AND LOW-VALUES
08356          MOVE PI-689-ALT-PRINTER-ID
08357                                  TO W-TS-TERM-TEXT
08358      ELSE
08359          IF PI-PROCESSOR-PRINTER NOT = SPACES AND LOW-VALUES
08360              MOVE PI-PROCESSOR-PRINTER
08361                                  TO W-TS-TERM-TEXT
08362          ELSE
08363              MOVE PI-COMPANY-ID  TO W-CNTL-COMPANY-ID
08364              MOVE '1'            TO W-CNTL-RECORD-TYPE
08365              MOVE SPACES         TO W-CNTL-GENL
08366              MOVE ZEROS          TO W-CNTL-SEQ-NO
08367              
      * EXEC CICS HANDLE CONDITION
08368 *                 NOTOPEN    (8040-CNTL-NOT-OPEN)
08369 *                 NOTFND     (7945-NOT-FOUND)
08370 *            END-EXEC
      *    MOVE '"$JI                  ! ; #00015580' TO DFHEIV0
           MOVE X'22244A492020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3B20233030303135353830' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
08371              
      * EXEC CICS READ
08372 *                 DATASET    (W-CNTL-FILE-ID)
08373 *                 SET        (ADDRESS OF CONTROL-FILE)
08374 *                 RIDFLD     (W-CNTL-KEY)
08375 *            END-EXEC
      *    MOVE '&"S        E          (   #00015584' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135353834' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-CNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
08376              MOVE CF-FORMS-PRINTER-ID
08377                                  TO W-TS-TERM-TEXT.
08378
08379 ***********************************************************
08380 *      CHECK TO SEE IF IT IS A PRINT REQUEST FOR PRINTING *
08381 *      LETTERS ON A 3275 PRINTER. IF SO, SAVE THE SCREEN  *
08382 ***********************************************************
08383
08384      SET W-TS-NDX                TO 1.
08385      MOVE EIBTIME                TO W-TS-ID-TIME.
08386      MOVE W-TS-NAME-TEXT         TO PI-689-TEMP-STOR-ID.
08387      MOVE 0                      TO PI-TEMP-STOR-ITEMS.
08388
08389      PERFORM 7840-WRITE-TS THRU 7840-EXIT
08390              VARYING
08391          W-TS-GROUP-WORK FROM 0 BY W-TS-NUM-REC-IN-GROUP
08392              UNTIL
08393          W-TS-GROUP-WORK NOT LESS THAN PI-TOTAL-LINES.
08394
08395 *    IF  EIBAID EQUAL DFHPF8
08396 *        MOVE '1'                TO PI-689-PRINT-SW
08397 *    ELSE
08398          MOVE SPACES             TO PI-689-PRINT-SW.
08399
08400      
      * EXEC CICS HANDLE CONDITION
08401 *         TERMIDERR  (7935-TERM-ERROR)
08402 *         TRANSIDERR (7940-TRAN-ERROR)
08403 *    END-EXEC.
      *    MOVE '"$[\                  ! < #00015613' TO DFHEIV0
           MOVE X'22245B5C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3C20233030303135363133' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08404
08415      
      * EXEC CICS START
08416 *             INTERVAL    (0)
08417 *             TRANSID     (W-PRINT-TRANS)
08418 *             FROM        (PROGRAM-INTERFACE-BLOCK)
08419 *             LENGTH      (PI-COMM-LENGTH)
08420 *             TERMID      (W-TS-TERM-TEXT)
08421 *    END-EXEC.
           MOVE 0 TO DFHEIV10
      *    MOVE '0(ILFT                1   #00015618' TO DFHEIV0
           MOVE X'3028494C4654202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020203120' TO DFHEIV0(13:12)
           MOVE X'2020233030303135363138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV10, 
                 W-PRINT-TRANS, 
                 DFHEIV99, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 W-TS-TERM-TEXT, 
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
           
08422
08423      
      * EXEC CICS SYNCPOINT
08424 *    END-EXEC.
      *    MOVE '6"                    !   #00015626' TO DFHEIV0
           MOVE X'362220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2020233030303135363236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08425
08426      IF  PI-689-CREATE-NO-SCREENS
08427          GO TO 7900-EXIT.
08428
08429 *    IF  EIBAID EQUAL DFHPF8
08430 *            AND
08431 *        PI-SHOW-MODE
08432 *            AND
08433 *        LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES
08434 *        PERFORM 7920-UPDATE-INITIAL-PRINT THRU 7920-EXIT.
08435
08436      MOVE ER-0189                TO EMI-ERROR.
08437      MOVE -1                     TO MAINTL.
08438
08439      PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
081004     MOVE -1                     TO MAINTL
081004     MOVE AL-UABON               TO MAINTA
081004     MOVE '104A'                 TO W-TS-ID-TEXT
081004     MOVE EIBTRMID               TO W-TS-TERM-TEXT
081004                                    W-TS-TERM-SCREEN
08440      GO TO 8200-SEND-DATAONLY.
08441
08442  7900-EXIT.
08443      EXIT.
08444
08445  7915-SEARCH-REMAINING-VARS.
08446
08447      IF  W-REC-CHAR (W-RVS-NDX) EQUAL '@'
08448          SET W-RVS-NDX2          TO W-RVS-NDX
08449          SET W-RVS-NDX2 UP BY +1
08450          IF  W-REC-CHAR (W-RVS-NDX2) EQUAL '@'
08451              SET W-RVS-NDX2 UP BY +1
08452              IF  W-REC-CHAR (W-RVS-NDX2) NUMERIC
08453                  SET PI-CURRENT-LINE  TO W-RVS-NDX
08454                  COMPUTE PI-CURRENT-LINE
08455                      = PI-CURRENT-LINE / 73
08456                  MOVE 'Y'        TO W-REMAINING-VAR-SW
08457                  MOVE +0         TO W-ROLL-COUNTER
08458                  MOVE ER-0191    TO EMI-ERROR
08459                  MOVE -1         TO MAINTL
08460                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08461
08462  7915-EXIT.
08463      EXIT.
08464                                  EJECT
08465  7920-UPDATE-INITIAL-PRINT.
08466
08467      
      * EXEC CICS HANDLE CONDITION
08468 *         NOTOPEN    (8010-ARCH-NOT-OPEN)
08469 *         NOTFND     (1070-ARCH-NOT-FOUND)
08470 *         ENDFILE    (1070-ARCH-NOT-FOUND)
08471 *    END-EXEC.
      *    MOVE '"$JI''                 ! = #00015675' TO DFHEIV0
           MOVE X'22244A492720202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3D20233030303135363735' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
           MOVE PI-COMPANY-CD TO W-ARCH-COMPANY-CD
03919      MOVE ARCHNUMI               TO W-ARCH-NUMBER
08473      
      * EXEC CICS READ
08474 *        DATASET (W-ARCH-FILE-ID)
08475 *        SET     (ADDRESS OF LETTER-ARCHIVE)
08476 *        RIDFLD  (W-ARCH-PARTIAL-KEY)
08477 *        UPDATE
08478 *    END-EXEC.
      *    MOVE '&"S        EU         (   #00015682' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'552020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303135363832' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 W-ARCH-PARTIAL-KEY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF LETTER-ARCHIVE TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08479
08480      MOVE W-SAVE-BIN-DATE        TO LA-INITIAL-PRINT-DATE.
08481
08482      IF  LA-RESEND-DATE = LOW-VALUES
08483          MOVE 'C'                TO LA-STATUS.
08484
08485      
      * EXEC CICS REWRITE
08486 *         FROM      (LETTER-ARCHIVE)
08487 *         DATASET   (W-ARCH-FILE-ID)
08488 *    END-EXEC.
           MOVE LENGTH OF
            LETTER-ARCHIVE
             TO DFHEIV11
      *    MOVE '&& L                  %   #00015694' TO DFHEIV0
           MOVE X'2626204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303135363934' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-ARCH-FILE-ID, 
                 LETTER-ARCHIVE, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08489
08490  7920-EXIT.
08491      EXIT.
08492                                  EJECT
08493  7935-TERM-ERROR.
08494
08495      MOVE ER-0412                TO EMI-ERROR.
08496      MOVE SPACES                 TO PI-689-PRINT-SW.
08497      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08498      MOVE -1                     TO MAINTL.
08499      GO TO 8200-SEND-DATAONLY.
08500
08501  7940-TRAN-ERROR.
08502
08503      MOVE ER-0413                TO EMI-ERROR.
08504      MOVE SPACES                 TO PI-689-PRINT-SW.
08505      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08506      MOVE -1                     TO MAINTL.
08507      GO TO 8200-SEND-DATAONLY.
08508
08509  7945-NOT-FOUND.
08510
08511      MOVE ER-0190                TO EMI-ERROR.
08512      MOVE -1                     TO MAINTL.
08513      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08514      GO TO 8200-SEND-DATAONLY.
08515                                  EJECT
08516  8010-ARCH-NOT-OPEN.
08517
08518      MOVE ER-7388                TO EMI-ERROR.
08519      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08520      MOVE -1                     TO MAINTL.
08521      GO TO 8200-SEND-DATAONLY.
08522
08523  8015-ARCT-NOT-OPEN.
08524
08525      MOVE ER-7373                TO EMI-ERROR.
08526      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08527      MOVE -1                     TO MAINTL.
08528      GO TO 8200-SEND-DATAONLY.
08529
08530  8020-ACCT-NOT-OPEN.
08531
08532      MOVE ER-0168 TO EMI-ERROR
08533      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08534      MOVE -1                     TO MAINTL.
08535      GO TO 8200-SEND-DATAONLY.
08536
08537  8030-CERT-NOT-OPEN.
08538
08539      MOVE ER-0169 TO EMI-ERROR.
08540      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08541      MOVE -1                     TO MAINTL.
08542      GO TO 8200-SEND-DATAONLY.
08543
08544  8035-CERT-NOT-FOUND.
08545
08546      MOVE ER-2369 TO EMI-ERROR.
08547      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08548      MOVE -1                     TO MAINTL.
08549      GO TO 8200-SEND-DATAONLY.
08550
08551  8040-CNTL-NOT-OPEN.
08552
08553      MOVE ER-0042                TO EMI-ERROR.
08554      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08555      MOVE -1                     TO MAINTL.
08556      GO TO 8200-SEND-DATAONLY.
08557
08558  8045-CHEK-NOT-OPEN.
08559
08560      MOVE ER-3775                TO EMI-ERROR.
08561      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08562      MOVE -1                     TO MAINTL.
08563      GO TO 8200-SEND-DATAONLY.
08564
08565  8050-TEXT-NOT-OPEN.
08566
08567      MOVE ER-0013                TO EMI-ERROR.
08568      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08569      MOVE -1                     TO MAINTL.
08570      GO TO 8200-SEND-DATAONLY.
08571
08572  8060-PNDB-NOT-OPEN.
08573
08574      MOVE ER-2216 TO EMI-ERROR.
08575      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08576      MOVE -1                     TO MAINTL.
08577      GO TO 8200-SEND-DATAONLY.
08578
08579  8070-PNDB-NOT-FOUND.
08580
08581      MOVE ER-7374 TO EMI-ERROR.
08582      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08583      MOVE -1                     TO MAINTL.
08584      GO TO 8200-SEND-DATAONLY.
08585
08586  8080-COMP-NOT-OPEN.
08587
08588      MOVE ER-2055 TO EMI-ERROR
08589      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08590      MOVE -1                     TO MAINTL.
08591      GO TO 8200-SEND-DATAONLY.
08592
08593  8085-PYAJ-NOT-OPEN.
08594
08595      MOVE ER-2232 TO EMI-ERROR
08596      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08597      MOVE -1                     TO MAINTL.
08598      GO TO 8200-SEND-DATAONLY.
08599
08600  8090-MAIL-NOT-OPEN.
08601
08602      MOVE ER-2999                TO EMI-ERROR.
08603      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08604      MOVE -1                     TO MAINTL.
08605      GO TO 8200-SEND-DATAONLY.
08606                                  EJECT
08607  8100-SEND-INITIAL-MAP.
08608
081004     PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT
08609      IF  PI-689-CREATE-NO-SCREENS
08610          MOVE '9999'             TO PI-689-ERROR
08611          GO TO 0620-RETURN-TO-CALLER.
08612
08613      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
08614
08615      IF  NOT EMI-NO-ERRORS
08616          MOVE EMI-MESSAGE-AREA (1)
08617                                  TO ERRMSGO
08618      ELSE
08619          MOVE SPACES             TO ERRMSGO.
           MOVE FUNCTION UPPER-CASE(WS-KIX-MYENV)
                                       TO SYSO
           MOVE WS-KIXHOST             TO HOSTO
041811     IF PI-PROCESSOR-ID = 'LMLC' OR 'SPJA' OR 'KAWA' OR 'CAGB'
041811        OR 'DLVA' OR 'ECCA' OR 'KRHA'
041811        MOVE AL-SADOF            TO PF8HDA
041811     END-IF
08621      IF NOT PI-CREATE-LABELS
08622          MOVE AL-PANON           TO ADDRLBLA.
08623
08624      
      * EXEC CICS SEND
08625 *        MAP    (W-MAP)
08626 *        MAPSET (W-MAPSET)
08627 *        FROM   (EL689AO)
08628 *        ERASE
08629 *        CURSOR
08630 *    END-EXEC.
           MOVE LENGTH OF
            EL689AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$     CT  E    H L F ,   #00015840' TO DFHEIV0
           MOVE X'382420202020204354202045' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135383430' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08631
08632      GO TO 9000-RETURN-TRANS.
08633
08634  8100-EXIT.
08635      EXIT.
08636                                  EJECT
08637  8200-SEND-DATAONLY.
08638
081004     PERFORM 7760-PUT-TEMP-STORAGE THRU 7760-EXIT.
08639      IF  PI-689-CREATE-NO-SCREENS
08640          MOVE '9999'             TO PI-689-ERROR
08641          GO TO 0620-RETURN-TO-CALLER.
08642
08643      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
08644
091712     IF EMI-ERROR = ER-0715
091712         DISPLAY 'ER 0715'
091712         MOVE -1          TO CERTIDL
091712         MOVE +0          TO MAINTL
091712     END-IF
091712
08645      IF  NOT EMI-NO-ERRORS
08646          MOVE EMI-MESSAGE-AREA (1)
08647                                  TO ERRMSGO
08648      ELSE
08649          MOVE SPACES             TO ERRMSGO.
08650
08651      
      * EXEC CICS SEND
08652 *        MAP    (W-MAP)
08653 *        MAPSET (W-MAPSET)
08654 *        FROM   (EL689AO)
08655 *        DATAONLY
08656 *        CURSOR
08657 *    END-EXEC.
           MOVE LENGTH OF
            EL689AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT       H L F ,   #00015874' TO DFHEIV0
           MOVE X'382444202020204354202020' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135383734' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08658
08659      GO TO 9000-RETURN-TRANS.
08660
08661  8200-EXIT.
08662      EXIT.
08663                                  EJECT
08664  8300-SEND-TEXT.
08665
08666      IF  PI-689-CREATE-NO-SCREENS
08667          MOVE '9999'             TO PI-689-ERROR
08668          GO TO 0620-RETURN-TO-CALLER.
08669
08670      
      * EXEC CICS SEND TEXT
08671 *        FROM    (LOGOFF-TEXT)
08672 *        LENGTH  (LOGOFF-LENGTH)
08673 *        ERASE
08674 *        FREEKB
08675 *    END-EXEC.
      *    MOVE '8&      T  E F  H   F -   #00015893' TO DFHEIV0
           MOVE X'382620202020202054202045' TO DFHEIV0(1:12)
           MOVE X'204620204820202046202D20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135383933' TO DFHEIV0(25:11)
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
           
08676
08677      GO TO 9000-RETURN-TRANS.
08678
08679  8300-EXIT.
08680      EXIT.
08681                                  EJECT
08682  8350-SEND-DATAONLY-ERASEAUP.
08683
081004     MOVE EIBTRMID               TO W-TS-TERM-TEXT
081004     PERFORM 7760-PUT-TEMP-STORAGE
081004                                 THRU 7760-EXIT
08684      IF  PI-689-CREATE-NO-SCREENS
08685          MOVE '9999'             TO PI-689-ERROR
08686          GO TO 0620-RETURN-TO-CALLER.
08687
08688      PERFORM 9600-FORMAT-DATE-TIME THRU 9600-EXIT.
08689
122011     IF EMI-ERROR = ER-0715
122011         DISPLAY 'ER 0715'
122011         MOVE -1          TO CERTIDL
122011         MOVE +0          TO MAINTL
122011     END-IF
122011
08690      IF  NOT EMI-NO-ERRORS
08691          MOVE EMI-MESSAGE-AREA (1)
08692                                  TO ERRMSGO
08693      ELSE
08694          MOVE SPACES             TO ERRMSGO.
08695
08696      
      * EXEC CICS SEND
08697 *        MAP      (W-MAP)
08698 *        MAPSET   (W-MAPSET)
08699 *        FROM     (EL689AO)
08700 *        DATAONLY
08701 *        ERASEAUP
08702 *        CURSOR
08703 *    END-EXEC.
           MOVE LENGTH OF
            EL689AO
             TO DFHEIV12
           MOVE -1
             TO DFHEIV11
      *    MOVE '8$D    CT  A    H L F ,   #00015928' TO DFHEIV0
           MOVE X'382444202020204354202041' TO DFHEIV0(1:12)
           MOVE X'2020202048204C2046202C20' TO DFHEIV0(13:12)
           MOVE X'2020233030303135393238' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-MAP, 
                 EL689AO, 
                 DFHEIV12, 
                 W-MAPSET, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08704
08705      GO TO 9000-RETURN-TRANS.
08706
08707  8350-EXIT.
08708      EXIT.
08709
08710  8600-DEEDIT.
08711
08712      
      * EXEC CICS BIF DEEDIT
08713 *         FIELD    (W-DEEDIT-FIELD)
08714 *         LENGTH   (15)
08715 *    END-EXEC.
           MOVE 15
             TO DFHEIV11
      *    MOVE '@"L                   #   #00015944' TO DFHEIV0
           MOVE X'40224C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303135393434' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-DEEDIT-FIELD, 
                 DFHEIV11
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08716
08717  8600-EXIT.
08718      EXIT.
08719                                  EJECT
08720  9000-RETURN-TRANS.
08721
081004     MOVE EMI-FATAL-CTR      TO PI-689-FATAL-CTR
081004     MOVE EMI-FORCABLE-CTR   TO PI-689-FORCABLE-CTR
081004     MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO
081004     MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO
081004     
      * EXEC CICS RETURN
081004*        TRANSID    (W-TRANSACTION)
081004*        COMMAREA   (PROGRAM-INTERFACE-BLOCK)
081004*        LENGTH     (PI-COMM-LENGTH)
081004*    END-EXEC.
      *    MOVE '.(CT                  ''   #00015958' TO DFHEIV0
           MOVE X'2E2843542020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303135393538' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-TRANSACTION, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08722 *    MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
08723 *    MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.
08724 *    GO TO 0200-RECEIVE.
08725
08726  9000-EXIT.
08727      EXIT.
08728                                  EJECT
08729  9200-PA.
08730
08731      MOVE ER-0008                TO EMI-ERROR.
08732      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.
08733      MOVE -1                     TO MAINTL.
08734      GO TO 8200-SEND-DATAONLY.
08735
08736  9200-EXIT.
08737      EXIT.
08738                                  EJECT
08739  9300-DFHCLEAR.
08740
08741      IF PI-RETURN-TO-PROGRAM = 'EL689' OR 'EL690'
08742          MOVE '2'                TO PI-ACTION.
08743
08744      IF  PI-CLEAR-MODE
08745          MOVE PI-RETURN-TO-PROGRAM
08746                                  TO W-CALL-PGM
08747          PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
08748          GO TO 9400-XCTL
08749      ELSE
08750          PERFORM 7780-DELETE-TEMP-STOR-TEXT THRU 7780-EXIT
08751          MOVE LOW-VALUES         TO EL689AO
08752                                     PI-1042-WA
08753                                     PI-689-WORK-AREA
08754          MOVE ZEROS              TO PI-CURRENT-LINE
08755                                     PI-TEMP-STOR-ITEMS
08756                                     PI-TOTAL-LINES
08757                                     PI-UPDATE-SW
08758                                     PI-689-NUMBER-LABEL-LINES
08759          MOVE SPACES             TO PI-689-PRINT-SW
08760                                     PI-689-ALT-PRINTER-ID
08761                                     PI-COMM-CONTROL
08762                                     PI-689-FORM-NUMBER
08763                                     PI-689-TEMP-STOR-ID
08764                                     PI-689-LABEL-SOURCE
08765                                     PI-689-USE-SCREEN-IND
08766          MOVE '2'                TO PI-ACTION
08767          MOVE -1                 TO MAINTL
08768          GO TO 8100-SEND-INITIAL-MAP.
08769
08770  9300-EXIT.
08771      EXIT.
08772
08773  9400-XCTL.
08774
08775      
      * EXEC CICS XCTL
08776 *        PROGRAM  (W-CALL-PGM)
08777 *        COMMAREA (PROGRAM-INTERFACE-BLOCK)
08778 *        LENGTH   (PI-COMM-LENGTH)
08779 *    END-EXEC.
      *    MOVE '.$C                   %   #00016016' TO DFHEIV0
           MOVE X'2E2443202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202520' TO DFHEIV0(13:12)
           MOVE X'2020233030303136303136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 PROGRAM-INTERFACE-BLOCK, 
                 PI-COMM-LENGTH, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08780
08781  9400-EXIT.
08782      EXIT.
08783                                  EJECT
08784  9500-LINK-DATE-CONVERT.
08785
08786      IF  DC-BIN-DATE-1 EQUAL HIGH-VALUES
08787              AND
08788          DC-OPTION-CODE EQUAL ' '
08789          MOVE '99/99/99'         TO DC-GREG-DATE-1-EDIT
08790          GO TO 9500-EXIT.
08791
08792      
      * EXEC CICS LINK
08793 *        PROGRAM    ('ELDATCV')
08794 *        COMMAREA   (DATE-CONVERSION-DATA)
08795 *        LENGTH     (DC-COMM-LENGTH)
08796 *    END-EXEC.
           MOVE 'ELDATCV' TO DFHEIV1
      *    MOVE '."C                   (   #00016033' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136303333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08797
08798      IF  NO-CONVERSION-ERROR
08799              AND
08800          PI-COMPANY-ID EQUAL 'AUK'
08801              AND
08802          W-REVERSE-DATE
08803          MOVE DC-GREG-DATE-1-EDIT
08804                                  TO W-EDIT-DATE-1
08805          MOVE W-ED1-MM           TO W-ED2-MM
08806          MOVE W-ED1-DD           TO W-ED2-DD
08807          MOVE W-ED1-YY           TO W-ED2-YY
08808          MOVE W-EDIT-DATE-2      TO DC-GREG-DATE-1-EDIT.
08809
08810  9500-EXIT.
08811      EXIT.
08812
08813  9600-FORMAT-DATE-TIME.
08814
08816      MOVE W-SAVE-DATE            TO RUNDTEO.
08817      MOVE EIBTIME                TO W-TIME-IN.
08818      MOVE W-TIME-OUT             TO RUNTIMEO.
08815      MOVE PI-COMPANY-ID          TO COMPANYO.
101501     MOVE PI-PROCESSOR-ID        TO USERIDO.
08819      MOVE W-MAP-NUM              TO PI-CURRENT-SCREEN-NO.
08820
08821  9600-EXIT.
08822      EXIT.
08823                                  EJECT
08824  9700-PGMID-ERROR.
08825
08826      IF  PI-689-CREATE-NO-SCREENS
08827          MOVE '9999'             TO PI-689-ERROR
08828          GO TO 0620-RETURN-TO-CALLER.
08829
08830      
      * EXEC CICS  HANDLE CONDITION
08831 *        PGMIDERR  (8300-SEND-TEXT)
08832 *    END-EXEC.
      *    MOVE '"$L                   ! > #00016072' TO DFHEIV0
           MOVE X'22244C202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'3E20233030303136303732' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08833
08834      MOVE W-THIS-PGM             TO PI-CALLING-PROGRAM.
08835      MOVE ' '                    TO PI-ENTRY-CD-1.
08836      MOVE W-XCTL-005             TO W-CALL-PGM
08837                                     LOGOFF-PGM.
08838      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.
08839
08840      PERFORM 9400-XCTL THRU 9400-EXIT.
08841
08842  9700-EXIT.
08843      EXIT.
08844
08845  9800-ABEND.
08846
08847      IF  PI-689-CREATE-NO-SCREENS
08848          MOVE '9999'             TO PI-689-ERROR
08849          GO TO 0620-RETURN-TO-CALLER.
08850
08851      MOVE W-LINK-004             TO W-CALL-PGM.
08852      MOVE DFHEIBLK               TO EMI-LINE1
08853
08854      
      * EXEC CICS  LINK
08855 *        PROGRAM   (W-CALL-PGM)
08856 *        COMMAREA  (EMI-LINE1)
08857 *        LENGTH    (72)
08858 *    END-EXEC.
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00016096' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136303936' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08859
08860      GO TO 8200-SEND-DATAONLY.
08861
08862  9800-EXIT.
08863      EXIT.
08864                                  EJECT
08865  9900-ERROR-FORMAT.
08866
08867      IF  PI-689-CREATE-NO-SCREENS
08868          MOVE EMI-ERROR          TO PI-689-ERROR
08869          IF  PI-689-FATAL-ERROR
08870              GO TO 0620-RETURN-TO-CALLER
08871          ELSE
08872              GO TO 9900-EXIT.
08873
08874      IF  EMI-ERROR EQUAL ER-9097
08875          NEXT SENTENCE
08876      ELSE
08877          IF  EMI-ERRORS-COMPLETE
08878                  OR
08879              EMI-ERROR EQUAL W-LAST-ERROR
08880              GO TO 9900-EXIT.
08881
08882      MOVE W-LINK-001             TO W-CALL-PGM.
08883
08884      
      * EXEC CICS LINK
08885 *        PROGRAM    (W-CALL-PGM)
08886 *        COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)
08887 *        LENGTH     (EMI-COMM-LENGTH)
08888 *    END-EXEC.
      *    MOVE '."C                   (   #00016126' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136313236' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 W-CALL-PGM, 
                 ERROR-MESSAGE-INTERFACE-BLOCK, 
                 EMI-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08889
08890      IF  EMI-SEVERITY (2) EQUAL 'X'
08891              AND
08892          EMI-SEVERITY (1) NOT EQUAL 'X'
08893          MOVE EMI-MESSAGE-AREA (2)
08894                                  TO EMI-MESSAGE-AREA (1).
08895
08896      MOVE EMI-ERROR              TO W-LAST-ERROR.
08897
08898  9900-EXIT.
08899      EXIT.
08900
08901  9905-INITIALIZE-SECURITY.
08902 ******************************************************************
08903 *                                                                *
08904 *       THIS LOGIC SETS UP AND READS THE TEMPORARY STORAGE       *
08905 *       USER SECURITY RECORD SET UP BY EL125.  BASED ON THE      *
08906 *       APPLICATION NUMBER FOUND IN WORKING STORAGE UNDER        *
08907 *       W-APPL-SECRTY-NDX (PIC  S9(04) COMP), THIS PROGRAM       *
08908 *       MOVES THE APPROPRIATE APPLICATION AUTHORIZATION INTO     *
08909 *       PI-DISPLAY-CAP AND PI-MODIFY-CAP.  IT THEN CHECKS FOR    *
08910 *       BROWSE AUTHORITY AND IF NOT AUTHORIZED WILL SET AN       *
08911 *       ERROR CONDITION AND EXIT THE PROGRAM.                    *
08912 *                                                                *
08913 *       NOTE:  THE CARRIER/GRP/STATE/ACCOUNT SECURITY DATA       *
08914 *       IS ALSO PROVIDED BY THIS LOGIC.                          *
08915 *                                                                *
08916 ******************************************************************
08917
08918      IF  PI-PROCESSOR-ID EQUAL 'LGXX'
08919          MOVE 'Y'                TO PI-DISPLAY-CAP
08920                                     PI-MODIFY-CAP
08921      ELSE
08922          
      * EXEC CICS READQ TS
08923 *            QUEUE  (PI-SECURITY-TEMP-STORE-ID)
08924 *            INTO   (SECURITY-CONTROL)
08925 *            LENGTH (SC-COMM-LENGTH)
08926 *            ITEM   (1)
08927 *        END-EXEC
           MOVE 1
             TO DFHEIV11
      *    MOVE '*$II   L              ''   #00016164' TO DFHEIV0
           MOVE X'2A2449492020204C20202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303136313634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PI-SECURITY-TEMP-STORE-ID, 
                 SECURITY-CONTROL, 
                 SC-COMM-LENGTH, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
08928          MOVE SC-CREDIT-DISPLAY (W-APPL-SCRTY-NDX)
08929                                  TO PI-DISPLAY-CAP
08930          MOVE SC-CREDIT-UPDATE (W-APPL-SCRTY-NDX)
08931                                  TO PI-MODIFY-CAP.
08932
08933  9905-EXIT.
08934                                  EJECT
08935  9995-SECURITY-VIOLATION.
08936
08937      MOVE EIBDATE                TO SM-JUL-DATE.
08938      MOVE EIBTRMID               TO SM-TERMID.
08939      MOVE W-THIS-PGM             TO SM-PGM.
08940      MOVE EIBTIME                TO W-TIME-IN.
08941      MOVE W-TIME-OUT             TO SM-TIME.
08942      MOVE PI-PROCESSOR-ID        TO SM-PROCESSOR-ID.
08943
08944      
      * EXEC CICS LINK
08945 *         PROGRAM  ('EL003')
08946 *         COMMAREA (SECURITY-MESSAGE)
08947 *         LENGTH   (80)
08948 *    END-EXEC.
           MOVE 'EL003' TO DFHEIV1
           MOVE 80
             TO DFHEIV11
      *    MOVE '."C                   (   #00016186' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303136313836' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 SECURITY-MESSAGE, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
08949
08950  9995-EXIT.
08951      EXIT.
08952
08953  9999-GOBACK.
08954
08955      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL689' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
08956
08957  9999-EXIT.
08958      EXIT.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL689' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 9300-DFHCLEAR,
                     9200-PA,
                     9200-PA,
                     9200-PA
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 9700-PGMID-ERROR,
                     9800-ABEND,
                     0325-MAPFAIL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 8040-CNTL-NOT-OPEN,
                     7945-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 9700-PGMID-ERROR,
                     0325-MAPFAIL,
                     9800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 0990-CARRIER-NOT-FOUND,
                     8040-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 7
               GO TO 0994-STATE-NOT-FOUND,
                     8040-CNTL-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 8
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 9
               GO TO 2001-NOT-FOUND,
                     2001-NOT-FOUND,
                     8050-TEXT-NOT-OPEN
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 10
               GO TO 2000-ENDBR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 11
               GO TO 8040-CNTL-NOT-OPEN,
                     6200-CNTL1-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 12
               GO TO 8040-CNTL-NOT-OPEN,
                     6250-CNTL2-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 13
               GO TO 8040-CNTL-NOT-OPEN,
                     6300-CNTL6-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 14
               GO TO 8040-CNTL-NOT-OPEN,
                     6350-CNTL2-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 15
               GO TO 8060-PNDB-NOT-OPEN,
                     8070-PNDB-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 16
               GO TO 8030-CERT-NOT-OPEN,
                     8035-CERT-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 17
               GO TO 8020-ACCT-NOT-OPEN,
                     6500-ACCT-NOT-FOUND,
                     6500-ONLY-STOP
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 18
               GO TO 8080-COMP-NOT-OPEN,
                     6600-COMP-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 19
               GO TO 8040-CNTL-NOT-OPEN,
                     6630-CNTL2-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 20
               GO TO 8090-MAIL-NOT-OPEN,
                     6700-MAIL-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 21
               GO TO 8040-CNTL-NOT-OPEN,
                     6750-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 22
               GO TO 8040-CNTL-NOT-OPEN,
                     6800-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 23
               GO TO 8045-CHEK-NOT-OPEN,
                     6850-CHEK-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 24
               GO TO 8085-PYAJ-NOT-OPEN,
                     6900-PYAJ-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 25
               GO TO 7700-TS-ERROR,
                     7700-TS-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 26
               GO TO 7780-EXIT
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 27
               GO TO 8040-CNTL-NOT-OPEN,
                     7945-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 28
               GO TO 7935-TERM-ERROR,
                     7940-TRAN-ERROR
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 29
               GO TO 8010-ARCH-NOT-OPEN,
                     1070-ARCH-NOT-FOUND,
                     1070-ARCH-NOT-FOUND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 30
               GO TO 8300-SEND-TEXT
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL689' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
