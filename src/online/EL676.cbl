00001  IDENTIFICATION DIVISION.
00002
00003  PROGRAM-ID.                 EL676 .
00004 *              PROGRAM CONVERTED BY
00005 *              COBOL CONVERSION AID PO 5785-ABJ
00006 *              CONVERSION DATE 02/14/96 07:57:17.
00007 *                            VMOD=2.011
00008 *
00008 *
00009 *AUTHOR.        LOGIC, INC.
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
00023
00024 *REMARKS.
00025 *        TOTAL OF ALL ACCOUNTS WITH OR WITHOUT BUSINESS -
00026 *        REPORTING NET PREMIUMS BY LIFE, A&H, TOTAL.
00027 *
00028 *        ALSO TOTALS NON-PROCESSABLE BUSINESS FOR CLIENT.
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING
100104* 100104                   PEMA  ADD SPP PROCESSING
031113* 031113  CR2012110800003  PEMA  CORRECT UEC FOR SPPDDF
052813* 052813  IR2013052200002  PEMA  CORRECT COMMISSION CALC
020816* 020816  CR2015082500001  PEMA  ADD PROCESSING FOR NEW COMP VPP
122002******************************************************************
00029
00030  ENVIRONMENT DIVISION.
00031  DATA DIVISION.
00032  EJECT
00033  WORKING-STORAGE SECTION.
       01  DFH-START PIC X(04).
00034  01  LCP-TIME-OF-DAY-XX.
00035      05  LCP-TIME-OF-DAY-68        PIC 9(6).
00036      05  FILLER                    PIC 99.
00037  01  LCP-CICS-TIME                 PIC 9(15).
00038  77  FILLER  PIC  X(32)  VALUE '********************************'.
00039  77  FILLER  PIC  X(32)  VALUE '*     EL676  WORKING STORAGE   *'.
00040  77  FILLER  PIC  X(32)  VALUE '******** VMOD=2.011 ************'.
100104 77  SUB                         PIC S999  COMP-3 VALUE +0.
031113 77  s1                          pic s999  comp-3 value +0.
100104 77  CNC-FACT                    PIC S9(3)V9(7) COMP-3 VALUE +0.
052813 77  ws-work-comm                pic s9(7)v99 comp-3 value +0.
100104 77  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
100104     88  RESP-NORMAL                    VALUE +0.
100104     88  RESP-NOTFND                    VALUE +13.
100104     88  RESP-NOTOPEN                   VALUE +19.
100104     88  RESP-ENDFILE                   VALUE +20.
00041
00042  01  FILLER  COMP.
00043      12  CLEN                        PIC S9(4)   VALUE +1024.
00044
00045  01  FILLER          COMP-3.
00046      12  WS-LINE-COUNT               PIC S9(3)   VALUE +99.
00047      12  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.
00048      12  WS-LINE-NUMBER              PIC S9(3)   VALUE +0.
00049      12  WS-PAGE-NUMBER              PIC S9(5)   VALUE +0.
031113 01  ERACCT-key.
031113     16  acct-company-cd         PIC X     VALUE SPACES.
031113     16  acct-carrier            PIC X     VALUE SPACES.
031113     16  acct-grouping           PIC X(6)  VALUE SPACES.
031113     16  acct-state              PIC XX    VALUE SPACES.
031113     16  acct-account            PIC X(10) VALUE SPACES.
031113     16  acct-exp-date           PIC XX    VALUE SPACES.
031113     16  FILLER                  PIC X(4)  VALUE SPACES.
00051  01  ELCNTL-KEY.
00052      12  CNTL-COMP-ID                PIC  X(3).
00053      12  CNTL-REC-TYPE               PIC  X.
00054      12  CNTL-ACCESS                 PIC  X(4).
00055      12  CNTL-SEQ-NO                 PIC S9(4)    COMP.
00056
100104 01  WS-CARRIER-TABLE.
100104     12  FILLER OCCURS 30.
100104         16  WS-CARRIER-CODE         PIC X.
100104         16  WS-SEC-PAY-CARRIER      PIC X.
100104
00057  01  ERPNDB-KEY.
00058      12  PNDB-COMPANY-CD             PIC  X.
00059      12  PNDB-CARRIER                PIC  X.
00060      12  PNDB-GROUPING               PIC  X(6).
00061      12  PNDB-STATE                  PIC  X(2).
00062      12  PNDB-ACCOUNT                PIC  X(10).
00063      12  PNDB-CERT-EFF-DT            PIC  X(2).
00064      12  PNDB-CERT-NO                PIC  X(11).
00065      12  PNDB-ALT-CHG-SEQ-NO         PIC S9(4)   COMP.
00066      12  PNDB-RECORD-TYPE            PIC  X.
00067
00068  01  FILLER.
031113     12  ws-dcc-product-code         pic xxx     value spaces.
031113     12  WS-AH-CATEGORY              PIC X       VALUE ' '.
031113     12  CLAS-LOOK                   PIC XX      VALUE '  '.
00069      12  SAVE-DATE                   PIC  X(8).
00070      12  PEND-SW                     PIC  X      VALUE '0'.
00071          88  GOOD-PEND                           VALUE '0'.
00072      12  REPT-FILE-ID                PIC  X(8)   VALUE 'ELREPT  '.
00073      12  CNTL-FILE-ID                PIC  X(8)   VALUE 'ELCNTL  '.
00074      12  PEND-FILE-ID                PIC  X(8)   VALUE 'ERPNDB2 '.
00075      12  PGM-NAME                    PIC  X(8)   VALUE SPACES.
00076      12  LINK-ELDATCV                PIC  X(8)   VALUE 'ELDATCV '.
00077      12  EMI-LINE1                   PIC  X(72)  VALUE SPACES.
00078      12  GOOD-STARTBR                PIC  X(3)   VALUE 'NO '.
00079      12  WS-TIME-OF-DAY.
00080          16  WS-TIME.
00081              20  WS-HH               PIC  99.
00082              20  WS-MM               PIC  99.
00083              20  WS-SS               PIC  99.
00084          16  WS-HUN-SEC              PIC  99.
00085      12  CARRIER-SWITCH              PIC  X      VALUE 'N'.
00086          88  USE-CARRIER-TOTALS                  VALUE 'Y'.
00087      12  PREVIOUS-CARRIER            PIC  X      VALUE LOW-VALUES.
00088  EJECT
00089  01  CARRIER-XTRACT.
00090      12  CARR-AMOUNT-FIELDS  COMP-3.
00091          16  CARR-XTR-GOOD-LF-AMT    PIC S9(9)V99     VALUE +0.
00092          16  CARR-XTR-GOOD-LF-OB     PIC S9(9)V99     VALUE +0.
00093          16  CARR-XTR-GOOD-LF-CAN    PIC S9(9)V99     VALUE +0.
00094          16  CARR-XTR-GOOD-LF-COM    PIC S9(9)V99     VALUE +0.
00095          16  CARR-XTR-GOOD-AH-AMT    PIC S9(9)V99     VALUE +0.
00096          16  CARR-XTR-GOOD-AH-OB     PIC S9(9)V99     VALUE +0.
00097          16  CARR-XTR-GOOD-AH-CAN    PIC S9(9)V99     VALUE +0.
00098          16  CARR-XTR-GOOD-AH-COM    PIC S9(9)V99     VALUE +0.
00099          16  CARR-XTR-BAD-LF-AMT     PIC S9(9)V99     VALUE +0.
00100          16  CARR-XTR-BAD-LF-OB      PIC S9(9)V99     VALUE +0.
00101          16  CARR-XTR-BAD-LF-CAN     PIC S9(9)V99     VALUE +0.
00102          16  CARR-XTR-BAD-LF-COM     PIC S9(9)V99     VALUE +0.
00103          16  CARR-XTR-BAD-AH-AMT     PIC S9(9)V99     VALUE +0.
00104          16  CARR-XTR-BAD-AH-OB      PIC S9(9)V99     VALUE +0.
00105          16  CARR-XTR-BAD-AH-CAN     PIC S9(9)V99     VALUE +0.
00106          16  CARR-XTR-BAD-AH-COM     PIC S9(9)V99     VALUE +0.
00107          16  CARR-XTR-GOOD-IS-CNT    PIC S9(9)        VALUE +0.
00108          16  CARR-XTR-BAD-IS-CNT     PIC S9(9)        VALUE +0.
00109          16  CARR-XTR-GOOD-CA-CNT    PIC S9(9)        VALUE +0.
00110          16  CARR-XTR-BAD-CA-CNT     PIC S9(9)        VALUE +0.
00111          16  CARR-XTR-GOOD-LF-NET    PIC S9(9)V99     VALUE +0.
00112          16  CARR-XTR-GOOD-AH-NET    PIC S9(9)V99     VALUE +0.
00113          16  CARR-XTR-BAD-LF-NET     PIC S9(9)V99     VALUE +0.
00114          16  CARR-XTR-BAD-AH-NET     PIC S9(9)V99     VALUE +0.
00115          16  CARR-XTR-GOOD-IS-TOT    PIC S9(9)V99     VALUE +0.
00116          16  CARR-XTR-GOOD-CA-TOT    PIC S9(9)V99     VALUE +0.
00117          16  CARR-XTR-GOOD-NET-TOT   PIC S9(9)V99     VALUE +0.
00118          16  CARR-XTR-GOOD-CM-TOT    PIC S9(9)V99     VALUE +0.
00119          16  CARR-XTR-BAD-IS-TOT     PIC S9(9)V99     VALUE +0.
00120          16  CARR-XTR-BAD-CA-TOT     PIC S9(9)V99     VALUE +0.
00121          16  CARR-XTR-BAD-NET-TOT    PIC S9(9)V99     VALUE +0.
00122          16  CARR-XTR-BAD-CM-TOT     PIC S9(9)V99     VALUE +0.
00123  EJECT
00124  01  REPORT-XTRACT.
00125      12  REPT-AMOUNT-FIELDS  COMP-3.
00126          16  REPT-XTR-GOOD-LF-AMT    PIC S9(9)V99     VALUE +0.
00127          16  REPT-XTR-GOOD-LF-OB     PIC S9(9)V99     VALUE +0.
00128          16  REPT-XTR-GOOD-LF-CAN    PIC S9(9)V99     VALUE +0.
00129          16  REPT-XTR-GOOD-LF-COM    PIC S9(9)V99     VALUE +0.
00130          16  REPT-XTR-GOOD-AH-AMT    PIC S9(9)V99     VALUE +0.
00131          16  REPT-XTR-GOOD-AH-OB     PIC S9(9)V99     VALUE +0.
00132          16  REPT-XTR-GOOD-AH-CAN    PIC S9(9)V99     VALUE +0.
00133          16  REPT-XTR-GOOD-AH-COM    PIC S9(9)V99     VALUE +0.
00134          16  REPT-XTR-BAD-LF-AMT     PIC S9(9)V99     VALUE +0.
00135          16  REPT-XTR-BAD-LF-OB      PIC S9(9)V99     VALUE +0.
00136          16  REPT-XTR-BAD-LF-CAN     PIC S9(9)V99     VALUE +0.
00137          16  REPT-XTR-BAD-LF-COM     PIC S9(9)V99     VALUE +0.
00138          16  REPT-XTR-BAD-AH-AMT     PIC S9(9)V99     VALUE +0.
00139          16  REPT-XTR-BAD-AH-OB      PIC S9(9)V99     VALUE +0.
00140          16  REPT-XTR-BAD-AH-CAN     PIC S9(9)V99     VALUE +0.
00141          16  REPT-XTR-BAD-AH-COM     PIC S9(9)V99     VALUE +0.
00142          16  REPT-XTR-GOOD-IS-CNT    PIC S9(9)        VALUE +0.
00143          16  REPT-XTR-BAD-IS-CNT     PIC S9(9)        VALUE +0.
00144          16  REPT-XTR-GOOD-CA-CNT    PIC S9(9)        VALUE +0.
00145          16  REPT-XTR-BAD-CA-CNT     PIC S9(9)        VALUE +0.
00146          16  REPT-XTR-GOOD-LF-NET    PIC S9(9)V99     VALUE +0.
00147          16  REPT-XTR-GOOD-AH-NET    PIC S9(9)V99     VALUE +0.
00148          16  REPT-XTR-BAD-LF-NET     PIC S9(9)V99     VALUE +0.
00149          16  REPT-XTR-BAD-AH-NET     PIC S9(9)V99     VALUE +0.
00150          16  REPT-XTR-GOOD-IS-TOT    PIC S9(9)V99     VALUE +0.
00151          16  REPT-XTR-GOOD-CA-TOT    PIC S9(9)V99     VALUE +0.
00152          16  REPT-XTR-GOOD-NET-TOT   PIC S9(9)V99     VALUE +0.
00153          16  REPT-XTR-GOOD-CM-TOT    PIC S9(9)V99     VALUE +0.
00154          16  REPT-XTR-BAD-IS-TOT     PIC S9(9)V99     VALUE +0.
00155          16  REPT-XTR-BAD-CA-TOT     PIC S9(9)V99     VALUE +0.
00156          16  REPT-XTR-BAD-NET-TOT    PIC S9(9)V99     VALUE +0.
00157          16  REPT-XTR-BAD-CM-TOT     PIC S9(9)V99     VALUE +0.
00158  EJECT
00159  01  WS-HEADING1.
00160      12  FILLER                  PIC  X(24)  VALUE SPACES.
00161      12  WS-H1-TITLE             PIC  X(18)  VALUE
00162              'NET PREMIUM TOTALS'.
00163      12  FILLER                  PIC  X(22)  VALUE SPACES.
00164      12  WS-H1-REPORT-NUMBER     PIC  X(7)   VALUE 'EL -676'.
00165      12  FILLER                  PIC  X(61)  VALUE SPACES.
00166
00167  01  WS-HEADING2.
00168      12  FILLER                  PIC  X(24)  VALUE SPACES.
00169      12  WS-H2-CLIENT-NAME       PIC  X(30)  VALUE SPACES.
00170      12  FILLER                  PIC  X(12)  VALUE SPACES.
00171      12  WS-H2-DATE              PIC  X(8)   VALUE SPACES.
00172      12  FILLER                  PIC  X(58)  VALUE SPACES.
00173
00174  01  WS-HEADING3.
00175      12  FILLER                  PIC  X(24)  VALUE SPACES.
00176      12  WS-H3-DATE              PIC  X(18)  VALUE SPACES.
00177      12  FILLER                  PIC  X(13)  VALUE SPACES.
00178      12  FILLER                  PIC  X(5)   VALUE 'PAGE'.
00179      12  WS-H3-PAGE              PIC ZZZZZ9  VALUE ZERO.
00180      12  FILLER                  PIC  X(66)  VALUE SPACES.
00181
00182  01  WS-HEADING4.
00183      12  FILLER                  PIC  X(5)   VALUE SPACES.
00184      12  FILLER                  PIC  X(10)  VALUE
00185              'CARRIER - '.
00186      12  WS-H4-CARRIER           PIC  X      VALUE SPACES.
00187      12  FILLER                  PIC  X(116) VALUE SPACES.
00188
00189  01  WS-HEADING5.
00190      12  FILLER                  PIC  X(22)  VALUE SPACES.
00191      12  FILLER                  PIC  X(42)  VALUE
00192              'PROCESSABLE    NON PROCESSABLE       TOTAL'.
00193      12  FILLER                  PIC  X(68)  VALUE SPACES.
00194
00195  01  WS-DETAIL1.
00196      12  FILLER                  PIC  X(3)   VALUE SPACES.
00197      12  FILLER                  PIC  X(7)   VALUE 'ISSUES '.
00198      12  WS-DET1-LF-HDG          PIC  X(6)   VALUE SPACES.
00199      12  FILLER                  PIC  X(5)   VALUE ' OB  '.
00200      12  WS-IS-LF-OB-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00201      12  FILLER                  PIC  X(3)   VALUE SPACES.
00202      12  WS-IS-LF-OB-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00203      12  FILLER                  PIC  X(3)   VALUE SPACES.
00204      12  WS-IS-LF-OB-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00205      12  FILLER                  PIC  X(63)  VALUE SPACES.
00206
00207  01  WS-DETAIL2.
00208      12  FILLER                  PIC  X(10)  VALUE SPACES.
00209      12  WS-DET2-LF-HDG          PIC  X(6)   VALUE SPACES.
00210      12  FILLER                  PIC  X(5)   VALUE ' SP  '.
00211      12  WS-IS-LF-SP-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00212      12  FILLER                  PIC  X(3)   VALUE SPACES.
00213      12  WS-IS-LF-SP-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00214      12  FILLER                  PIC  X(3)   VALUE SPACES.
00215      12  WS-IS-LF-SP-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00216      12  FILLER                  PIC  X(63)  VALUE SPACES.
00217
00218  01  WS-DETAIL3.
00219      12  FILLER                  PIC  X(10)  VALUE SPACES.
00220      12  WS-DET3-AH-HDG          PIC  X(6)   VALUE SPACES.
00221      12  FILLER                  PIC  X(5)   VALUE ' OB  '.
00222      12  WS-IS-AH-OB-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00223      12  FILLER                  PIC  X(3)   VALUE SPACES.
00224      12  WS-IS-AH-OB-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00225      12  FILLER                  PIC  X(3)   VALUE SPACES.
00226      12  WS-IS-AH-OB-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00227      12  FILLER                  PIC  X(63)  VALUE SPACES.
00228
00229  01  WS-DETAIL4.
00230      12  FILLER                  PIC  X(10)  VALUE SPACES.
00231      12  WS-DET4-AH-HDG          PIC  X(6)   VALUE SPACES.
00232      12  FILLER                  PIC  X(5)   VALUE ' SP  '.
00233      12  WS-IS-AH-SP-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00234      12  FILLER                  PIC  X(3)   VALUE SPACES.
00235      12  WS-IS-AH-SP-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00236      12  FILLER                  PIC  X(3)    VALUE SPACES.
00237      12  WS-IS-AH-SP-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00238      12  FILLER                  PIC  X(63)  VALUE SPACES.
00239
00240  01  WS-DETAIL5.
00241      12  FILLER                  PIC  X(5)   VALUE SPACES.
00242      12  FILLER                  PIC  X(10)  VALUE 'CANCELS   '.
00243      12  WS-DET5-LF-HDG          PIC  X(6)   VALUE SPACES.
00244      12  WS-CA-LF-GOOD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00245      12  FILLER                  PIC  X(3)   VALUE SPACES.
00246      12  WS-CA-LF-BAD            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00247      12  FILLER                  PIC  X(3)   VALUE SPACES.
00248      12  WS-CA-LF-TOT            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00249      12  FILLER                  PIC  X(63)  VALUE SPACES.
00250
00251  01  WS-DETAIL6.
00252      12  FILLER                  PIC  X(14)  VALUE SPACES.
00253      12  FILLER                  PIC  X      VALUE SPACES.
00254      12  WS-DET6-AH-HDG          PIC  X(6)   VALUE SPACES.
00255      12  WS-CA-AH-GOOD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00256      12  FILLER                  PIC  X(3)   VALUE SPACES.
00257      12  WS-CA-AH-BAD            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00258      12  FILLER                  PIC  X(3)   VALUE SPACES.
00259      12  WS-CA-AH-TOT            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00260      12  FILLER                  PIC  X(63)  VALUE SPACES.
00261
00262  01  WS-DETAIL7.
00263      12  FILLER                  PIC  X(5)   VALUE SPACES.
00264      12  FILLER                  PIC  X(10)  VALUE 'NET       '.
00265      12  WS-DET7-LF-HDG          PIC  X(6)   VALUE SPACES.
00266      12  WS-NET-LF-GOOD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00267      12  FILLER                  PIC  X(3)   VALUE SPACES.
00268      12  WS-NET-LF-BAD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00269      12  FILLER                  PIC  X(3)   VALUE SPACES.
00270      12  WS-NET-LF-TOT           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00271      12  FILLER                  PIC  X(63)  VALUE SPACES.
00272
00273  01  WS-DETAIL8.
00274      12  FILLER                  PIC  X(14)  VALUE SPACES.
00275      12  FILLER                  PIC  X      VALUE SPACES.
00276      12  WS-DET8-AH-HDG          PIC  X(6)   VALUE SPACES.
00277      12  WS-NET-AH-GOOD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00278      12  FILLER                  PIC  X(3)   VALUE SPACES.
00279      12  WS-NET-AH-BAD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00280      12  FILLER                  PIC  X(3)   VALUE SPACES.
00281      12  WS-NET-AH-TOT           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00282      12  FILLER                  PIC  X(63)  VALUE SPACES.
00283
00284  01  WS-DETAIL9.
00285      12  FILLER                  PIC  X(5)   VALUE SPACES.
00286      12  FILLER                  PIC  X(10)  VALUE 'COMM.     '.
00287      12  WS-DET9-LF-HDG          PIC  X(6)   VALUE SPACES.
00288      12  WS-CM-LF-GOOD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00289      12  FILLER                  PIC  X(3)   VALUE SPACES.
00290      12  WS-CM-LF-BAD            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00291      12  FILLER                  PIC  X(3)   VALUE SPACES.
00292      12  WS-CM-LF-TOT            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00293      12  FILLER                  PIC  X(63)  VALUE SPACES.
00294
00295  01  WS-DETAILA.
00296      12  FILLER                  PIC  X(15)  VALUE SPACES.
00297      12  WS-DETA-AH-HDG          PIC  X(6)   VALUE SPACES.
00298      12  WS-CM-AH-GOOD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00299      12  FILLER                  PIC  X(3)   VALUE SPACES.
00300      12  WS-CM-AH-BAD            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00301      12  FILLER                  PIC  X(3)   VALUE SPACES.
00302      12  WS-CM-AH-TOT            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00303      12  FILLER                  PIC  X(63)  VALUE SPACES.
00304
00305  01  WS-TOTAL1.
00306      12  FILLER                  PIC  X(5)   VALUE SPACES.
00307      12  FILLER                  PIC  X(11)  VALUE 'TOTAL   ISS'.
00308      12  FILLER                  PIC  X(5)   VALUE 'UES  '.
00309      12  WS-IS-TOT-GOOD          PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00310      12  FILLER                  PIC  X(3)   VALUE SPACES.
00311      12  WS-IS-TOT-BAD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00312      12  FILLER                  PIC  X(3)   VALUE SPACES.
00313      12  WS-IS-TOT-TOT           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00314      12  FILLER                  PIC  X(63)  VALUE SPACES.
00315
00316  01  WS-TOTAL2.
00317      12  FILLER                  PIC  X(5)   VALUE SPACES.
00318      12  FILLER                  PIC  X(11)  VALUE '       CANC'.
00319      12  FILLER                  PIC  X(5)   VALUE 'ELS  '.
00320      12  WS-CA-TOT-GOOD          PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00321      12  FILLER                  PIC  X(3)   VALUE SPACES.
00322      12  WS-CA-TOT-BAD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00323      12  FILLER                  PIC  X(3)   VALUE SPACES.
00324      12  WS-CA-TOT-TOT           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.
00325      12  FILLER                  PIC  X(63)  VALUE SPACES.
00326
00327  01  WS-TOTAL3.
00328      12  FILLER                  PIC  X(5)   VALUE SPACES.
00329      12  FILLER                  PIC  X(11)  VALUE SPACES.
00330      12  FILLER                  PIC  X(5)   VALUE 'NET  '.
00331      12  WS-NET-TOT-GOOD         PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00332      12  FILLER                  PIC  X(3)   VALUE SPACES.
00333      12  WS-NET-TOT-BAD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00334      12  FILLER                  PIC  X(3)   VALUE SPACES.
00335      12  WS-NET-TOT-TOT          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00336      12  FILLER                  PIC  X(63)  VALUE SPACES.
00337
00338  01  WS-TOTAL4.
00339      12  FILLER                  PIC  X(5)   VALUE SPACES.
00340      12  FILLER                  PIC  X(11)  VALUE '          C'.
00341      12  FILLER                  PIC  X(5)   VALUE 'OMM  '.
00342      12  WS-CM-TOT-GOOD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00343      12  FILLER                  PIC  X(3)   VALUE SPACES.
00344      12  WS-CM-TOT-BAD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00345      12  FILLER                  PIC  X(3)   VALUE SPACES.
00346      12  WS-CM-TOT-TOT           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.
00347      12  FILLER                  PIC  X(63)  VALUE SPACES.
00348
00349  01  WS-TOTAL5.
00350      12  FILLER                  PIC  X(5)   VALUE SPACES.
00351      12  FILLER                  PIC  X(11)  VALUE 'COUNT   ISS'.
00352      12  FILLER                  PIC  X(5)   VALUE 'UES  '.
00353      12  WS-IS-CNT-GOOD          PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.
00354      12  FILLER                  PIC  X(3)   VALUE SPACES.
00355      12  WS-IS-CNT-BAD           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.
00356      12  FILLER                  PIC  X(3)   VALUE SPACES.
00357      12  WS-IS-CNT-TOT           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.
00358      12  FILLER                  PIC  X(67)  VALUE SPACES.
00359
00360  01  WS-TOTAL6.
00361      12  FILLER                  PIC  X(5)   VALUE SPACES.
00362      12  FILLER                  PIC  X(11)  VALUE '       CANC'.
00363      12  FILLER                  PIC  X(5)   VALUE 'ELS  '.
00364      12  WS-CA-CNT-GOOD          PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.
00365      12  FILLER                  PIC  X(3)   VALUE SPACES.
00366      12  WS-CA-CNT-BAD           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.
00367      12  FILLER                  PIC  X(3)   VALUE SPACES.
00368      12  WS-CA-CNT-TOT           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.
00369      12  FILLER                  PIC  X(67)  VALUE SPACES.
00370  EJECT
00371 *                                    COPY ELCREPT.
00001 ******************************************************************
00002 *                                                                *
00002 *                                                                *
00003 *                            ELCREPT.                            *
00004 *           COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE
00005 *                            VMOD=2.002                          *
00006 *                                                                *
00007 *   FILE DESCRIPTION = REPORT STORAGE                            *
00008 *                                                                *
00009 *   FILE TYPE = VSAM,KSDS                                        *
00010 *   RECORD SIZE = 146   RECFORM = FIX                            *
00011 *                                                                *
00012 *   BASE CLUSTER NAME = ELREPT                 RKP=2,LEN=11      *
00013 *       ALTERNATE PATH  = NOT USED                               *
00014 *                                                                *
00015 *   LOG = NO                                                     *
00016 *   SERVREQ = BROWSE, DELETE, UPDATE, NEWREC                     *
00017 ******************************************************************
00018  01  REPORT-SAVE-FILE.
00019      12  RF-RECORD-ID                PIC XX.
00020          88  VALID-RF-ID                VALUE 'RF'.
00021
00022      12  RF-CONTROL-PRIMARY.
00023          16  RF-COMPANY-CD           PIC X.
00024          16  RF-RECORD-TYPE          PIC X.
00025              88  REPORT-DETAIL-RECORD   VALUE '1'.
00026              88  REPORT-TRAILER-RECORD  VALUE '2'.
00027          16  RF-REPORT-ID.
00028              20  RF-SYSTEM-CODE      PIC XX.
00029                  88  CLAS-IC-ONLINE     VALUE 'EL'.
00030                  88  CLAS-SCS-BATCH     VALUE 'EC'.
00031              20  RF-PROGRAM-SEQUENCE PIC 999.
00032          16  RF-LINE-NUMBER          PIC S9(8)       COMP.
00033      12  RF-REPORT-LINE-133.
00034          16  RF-CTL-CHAR-133         PIC X.
00035          16  RF-DATA-133             PIC X(132).
00036          16  RF-DATA-FIRST  REDEFINES RF-DATA-133.
00037              20  RF-DATA-2-81        PIC X(80).
00038              20  FILLER              PIC X(52).
00039          16  RF-DATA-LAST   REDEFINES RF-DATA-133.
00040              20  FILLER              PIC X(53).
00041              20  RF-DATA-55-133      PIC X(79).
00042      12  RF-TRAILER-RECORD  REDEFINES RF-REPORT-LINE-133.
00043          16  FILLER                  PIC X.
00044          16  RF-CURRENT-DATE         PIC X(8).
00045          16  RF-PRINT-HH-MM-SS       PIC X(6).
00046          16  FILLER                  PIC X(115).
00047          16  RF-COMPANY-ID           PIC XXX.
00048 ******************************************************************
00372  EJECT
00373 *                                    COPY ELCDATE.
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
00374  EJECT
00375 *                                    COPY ELCINTF.
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
00376      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.
00377          16  PI-FROM-DT              PIC  XX.
00378          16  PI-THRU-DT              PIC  XX.
00379          16  PI-EL676-STOP-DT        PIC  XX.
00380          16  FILLER                  PIC  X.
00381          16  PI-EL676-ENTER-DT       PIC  XX.
00382          16  FILLER                  PIC  X(631).
00383  EJECT
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
00385  01  DFHCOMMAREA                 PIC  X(1024).
00386
00387 *01 PARMLIST .
00388 *    12  FILLER                  PIC S9(8)    COMP.
00389 *    12  ERPNDB-POINTER          PIC S9(8)    COMP.
00390 *    12  ELCNTL-POINTER          PIC S9(8)    COMP.
00392 *                                COPY ERCPNDB.
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
011410         16  FILLER                       PIC X.
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
072308         16  PB-C-NH-INT-ON-REFS          PIC S9(7)V99   COMP-3.
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
00394 *                                COPY ELCCNTL.
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
012913         16  FILLER                         PIC X(181).
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
031113*                                copy ERCACCT.
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
       PROCEDURE DIVISION USING DFHEIBLK DFHCOMMAREA PENDING-BUSINESS
                                CONTROL-FILE ACCOUNT-MASTER.
       0000-DFHEXIT SECTION.
           MOVE '9#                    $   ' TO DFHEIV0.
           MOVE 'EL676' TO DFHEIV1.
           CALL 'kxdfhei1' USING DFHEIV0 DFH-START DFHEIV DFHEIV1.
00397
pemuni*      exec cics retrieve
pemuni*       into(program-interface-block)
pemuni*       length(pi-comm-length)
pemuni*    end-exec
pemuni
00398      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.
00399      MOVE '5'                    TO  DC-OPTION-CODE.
00400
00401      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00402
00403      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE
00404                                      WS-H2-DATE.
pemuni*    MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.
00406
00407  1000-START-PROGRAM.
00408      
      * EXEC CICS  HANDLE CONDITION
00409 *        ERROR  (8800-ABEND)
00410 *        END-EXEC.
      *    MOVE '"$.                   ! " #00003664' TO DFHEIV0
           MOVE X'22242E202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2220233030303033363634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00411
00412      
      * EXEC CICS  RETRIEVE
00413 *        INTO    (PROGRAM-INTERFACE-BLOCK)
00414 *        LENGTH  (CLEN)
00415 *        END-EXEC.
      *    MOVE '0*I L                 &   #00003668' TO DFHEIV0
           MOVE X'302A49204C20202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363638' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PROGRAM-INTERFACE-BLOCK, 
                 CLEN, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00416
00417      IF PI-EL676-STOP-DT  = LOW-VALUES AND
00418         PI-EL676-ENTER-DT = LOW-VALUES
00419          MOVE PI-CR-MONTH-END-DT  TO  PI-EL676-STOP-DT.
00420
00421      IF PI-EL676-ENTER-DT NOT = LOW-VALUES
00422         MOVE PI-EL676-ENTER-DT       TO  DC-BIN-DATE-1
00423      ELSE
00424         MOVE PI-EL676-STOP-DT        TO  DC-BIN-DATE-1.
00425
00426      MOVE ' '                        TO  DC-OPTION-CODE.
00427
00428      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.
00429
00430      MOVE DC-GREG-DATE-1-ALPHA   TO  WS-H3-DATE.
00431
00432  2000-CHECK-IN-PROGRESS.
00433      
      * EXEC CICS  HANDLE CONDITION
00434 *        NOTFND  (2000-WRITE-INITIAL-TRAILER)
00435 *        END-EXEC.
      *    MOVE '"$I                   ! # #00003689' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2320233030303033363839' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00436
00437      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
00438      MOVE 'RF'                   TO  RF-RECORD-ID.
00439      MOVE '2'                    TO  RF-RECORD-TYPE.
00440      MOVE 'EL676'                TO  RF-REPORT-ID.
00441      MOVE ZEROS                  TO  RF-LINE-NUMBER.
00442
00443      
      * EXEC CICS  READ
00444 *        DATASET  (REPT-FILE-ID)
00445 *        INTO     (REPORT-SAVE-FILE)
00446 *        RIDFLD   (RF-CONTROL-PRIMARY)
00447 *        END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&"IL       E          (   #00003699' TO DFHEIV0
           MOVE X'2622494C2020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033363939' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00448
00449      GO TO 9999-RETURN-CICS.
00450
00451  2000-WRITE-INITIAL-TRAILER.
00452      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
00453      MOVE 'RF'                   TO  RF-RECORD-ID.
00454      MOVE '2'                    TO  RF-RECORD-TYPE.
00455      MOVE 'EL676'                TO  RF-REPORT-ID.
00456      MOVE ZEROS                  TO  RF-LINE-NUMBER.
00457      MOVE SPACES                 TO  RF-TRAILER-RECORD.
00458
00459      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
00460 *    END-EXEC
      *    MOVE '0"A                   "   #00003715' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373135' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00461      
      * EXEC CICS FORMATTIME
00462 *              ABSTIME(LCP-CICS-TIME)
00463 *              TIME(LCP-TIME-OF-DAY-XX)
00464 *    END-EXEC
      *    MOVE 'j$(     (             #   #00003717' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373137' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
00465      MOVE LCP-TIME-OF-DAY-68     TO  RF-PRINT-HH-MM-SS.
00466      MOVE 'STARTED'              TO  RF-CURRENT-DATE.
00467
00468      
      * EXEC CICS  WRITE
00469 *        DATASET  (REPT-FILE-ID)
00470 *        FROM     (REPORT-SAVE-FILE)
00471 *        RIDFLD   (RF-CONTROL-PRIMARY)
00472 *        END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00003724' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373234' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00473
00474  2100-DELETE-REC.
00475      MOVE 1                      TO  RF-LINE-NUMBER.
00476
00477      
      * EXEC CICS  HANDLE CONDITION
00478 *        NOTFND  (2300-DELETE-REC)
00479 *        END-EXEC.
      *    MOVE '"$I                   ! $ #00003733' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2420233030303033373333' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00480
00481  2200-DELETE-1.
00482      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
00483      MOVE 'RF'                   TO  RF-RECORD-ID.
00484      MOVE '1'                    TO  RF-RECORD-TYPE.
00485      MOVE 'EL676'                TO  RF-REPORT-ID.
00486
00487      
      * EXEC CICS  DELETE
00488 *        DATASET    (REPT-FILE-ID)
00489 *        RIDFLD     (RF-CONTROL-PRIMARY)
00490 *        KEYLENGTH  (11)
00491 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00003743' TO DFHEIV0
           MOVE X'26282020524B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00492
00493      ADD 1                       TO  RF-LINE-NUMBER.
00494
00495      GO TO 2200-DELETE-1.
00496
00497  2300-DELETE-REC.
00498      
      * EXEC CICS  HANDLE CONDITION
00499 *        NOTFND  (3000-READ-CONTROL)
00500 *        END-EXEC.
      *    MOVE '"$I                   ! % #00003754' TO DFHEIV0
           MOVE X'222449202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2520233030303033373534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00501
00502  2400-DELETE-2.
00503      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
00504      MOVE 'RF'                   TO  RF-RECORD-ID.
00505      MOVE '2'                    TO  RF-RECORD-TYPE.
00506      MOVE 'EL676'                TO  RF-REPORT-ID.
00507
00508      
      * EXEC CICS  DELETE
00509 *        DATASET    (REPT-FILE-ID)
00510 *        RIDFLD     (RF-CONTROL-PRIMARY)
00511 *        KEYLENGTH  (11)
00512 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00003764' TO DFHEIV0
           MOVE X'26282020524B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373634' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00513
00514      ADD 1                       TO  RF-LINE-NUMBER.
00515
00516      GO TO 2400-DELETE-2.
00517
00518  3000-READ-CONTROL.
00519      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.
00520      MOVE SPACES                 TO  CNTL-ACCESS.
00521      MOVE '1'                    TO  CNTL-REC-TYPE.
00522      MOVE +0                     TO  CNTL-SEQ-NO.
00523
00524      
      * EXEC CICS  READ
00525 *        DATASET  (CNTL-FILE-ID)
00526 *        SET      (ADDRESS OF CONTROL-FILE)
00527 *        RIDFLD   (ELCNTL-KEY)
00528 *        END-EXEC.
      *    MOVE '&"S        E          (   #00003780' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303033373830' TO DFHEIV0(25:11)
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
           
00529
00530      MOVE CF-CL-MAIL-TO-NAME     TO  WS-H2-CLIENT-NAME.
00531
00532      IF CF-CARR-GROUP-ST-ACCNT-CNTL
00533        OR CF-CARR-ST-ACCNT-CNTL
00534        OR CF-CARR-ACCNT-CNTL
00535          MOVE 'Y'                TO  CARRIER-SWITCH.
       3005-START-CARRIER.
100104     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID
100104     MOVE LOW-VALUES             TO  CNTL-ACCESS
100104     MOVE '6'                    TO  CNTL-REC-TYPE
100104     MOVE +0                     TO  CNTL-SEQ-NO
           
      * EXEC CICS  STARTBR
      *        DATASET  (CNTL-FILE-ID)
      *        RIDFLD   (ELCNTL-KEY)
      *        RESP     (WS-RESPONSE)
      *    END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &  N#00003797' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'204E233030303033373937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           IF NOT RESP-NORMAL
              GO TO 3100-START-BROWSE
           END-IF
           PERFORM VARYING SUB FROM +1 BY +1 UNTIL
              (SUB > +30)
              
      * EXEC CICS  READNEXT
      *            DATASET (CNTL-FILE-ID)
      *            SET     (ADDRESS OF CONTROL-FILE)
      *            RIDFLD  (ELCNTL-KEY)
      *            RESP    (WS-RESPONSE)
      *       END-EXEC
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )  N#00003807' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'204E233030303033383037' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 CNTL-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ELCNTL-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF CONTROL-FILE TO
               DFHEIV20
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
              IF (RESP-NORMAL)
                 AND (CF-COMPANY-ID = PI-COMPANY-ID)
                 AND (CF-RECORD-TYPE = '6')
                 MOVE CF-CARRIER-CNTL  TO WS-CARRIER-CODE (SUB)
                 MOVE CF-SECPAY-SWITCH TO WS-SEC-PAY-CARRIER (SUB)
              ELSE
                 MOVE +31              TO SUB
              END-IF
           END-PERFORM
031113     
      * exec cics endbr
031113*       dataset ('ELCNTL')
031113*    end-exec
           MOVE 'ELCNTL' TO DFHEIV1
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00003822' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383232' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
           .
00537  3100-START-BROWSE.
00538      MOVE LOW-VALUES             TO  ERPNDB-KEY
00539      MOVE PI-COMPANY-CD          TO  PNDB-COMPANY-CD
00540
00541      
      * EXEC CICS  HANDLE CONDITION
00542 *        NOTFND   (5000-PRINT-REPORT)
00543 *        ENDFILE  (4900-ENDBR)
00544 *        END-EXEC.
      *    MOVE '"$I''                  ! & #00003830' TO DFHEIV0
           MOVE X'222449272020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202120' TO DFHEIV0(13:12)
           MOVE X'2620233030303033383330' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00545
00546      
      * EXEC CICS  STARTBR
00547 *        DATASET  (PEND-FILE-ID)
00548 *        RIDFLD   (ERPNDB-KEY)
00549 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&,         G          &   #00003835' TO DFHEIV0
           MOVE X'262C20202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383335' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PEND-FILE-ID, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00550
00551      MOVE 'YES'                  TO  GOOD-STARTBR.
00552
00553  4000-READNEXT.
00554      
      * EXEC CICS  READNEXT
00555 *        DATASET  (PEND-FILE-ID)
00556 *        SET      (ADDRESS OF PENDING-BUSINESS)
00557 *        RIDFLD   (ERPNDB-KEY)
00558 *        END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&.S                   )   #00003843' TO DFHEIV0
           MOVE X'262E53202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202920' TO DFHEIV0(13:12)
           MOVE X'2020233030303033383433' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PEND-FILE-ID, 
                 DFHEIV20, 
                 DFHEIV99, 
                 ERPNDB-KEY, 
                 DFHEIV99, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF PENDING-BUSINESS TO
               DFHEIV20
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
00559
00560      IF PB-COMPANY-CD  NOT = PI-COMPANY-CD
00561          GO TO 4900-ENDBR.
00562
00563      IF PB-ISSUE
00564        OR  PB-CANCELLATION
00565          NEXT SENTENCE
00566      ELSE
00567          GO TO 4000-READNEXT.
00568
00569      IF PB-ALT-CHG-SEQ-NO  NOT =  ZEROS
00570          GO TO 4000-READNEXT.
00571
00572      IF PI-EL676-ENTER-DT NOT = LOW-VALUES
00573         IF PI-EL676-ENTER-DT = PB-INPUT-DT
00574            GO TO 4010-CONT
00575         ELSE
00576            GO TO 4000-READNEXT.
00577
00578      IF PB-CREDIT-ACCEPT-DT  =  PI-EL676-STOP-DT
00579        OR  PB-CREDIT-ACCEPT-DT  = LOW-VALUES
00580          NEXT SENTENCE
00581      ELSE
00582          GO TO 4000-READNEXT.
00583
00584      IF PB-CREDIT-SELECT-DT  IS GREATER THAN  PI-EL676-STOP-DT
00585          GO TO 4000-READNEXT.
00586      GO TO 4010-CONT.
00587
00588  4010-CONT.
00589      IF PB-CARRIER  IS NOT EQUAL TO  PREVIOUS-CARRIER
00590          PERFORM 4300-PRINT-CARRIER  THRU  4399-EXIT.
00591
00592      IF PB-RECORD-ON-HOLD
00593        OR  PB-RECORD-RETURNED
00594        OR  PB-FATAL-ERRORS
00595        OR  PB-UNFORCED-ERRORS
00596           MOVE '1'               TO  PEND-SW
00597      ELSE
00598           MOVE '0'               TO  PEND-SW.
00599
00600      IF PB-ISSUE
00601          IF PB-REIN-ONLY-CERT
00602            OR  PB-REISSUED-CERT
122002           OR  PB-MONTHLY-CERT
00603            OR  PB-POLICY-IS-DECLINED
00604            OR  PB-POLICY-IS-VOIDED
00605              IF GOOD-PEND
00606                  ADD +1              TO  CARR-XTR-GOOD-IS-CNT
00607                  GO TO 4000-READNEXT
00608              ELSE
00609                  ADD +1              TO  CARR-XTR-BAD-IS-CNT
00610                  GO TO 4000-READNEXT.
00611
00612      IF PB-CANCELLATION
00613          IF PB-CI-ENTRY-STATUS  = '9'
00614              IF GOOD-PEND
00615                  ADD +1              TO  CARR-XTR-GOOD-CA-CNT
00616                  GO TO 4000-READNEXT
00617              ELSE
00618                  ADD +1              TO  CARR-XTR-BAD-CA-CNT
00619                  GO TO 4000-READNEXT.
00620
00621      IF PB-OVERRIDE-LIFE
00622        OR  PB-OVERRIDE-BOTH
00623          IF PB-ISSUE
00624              MOVE PB-I-LF-PREM-CALC  TO  PB-I-LF-PREMIUM-AMT
00625              MOVE PB-I-LF-ALT-PREM-CALC
00626                                      TO  PB-I-LF-ALT-PREMIUM-AMT
00627          ELSE
00628              MOVE PB-C-LF-REF-CALC   TO  PB-C-LF-CANCEL-AMT.
00629
00630      IF PB-OVERRIDE-AH
00631        OR  PB-OVERRIDE-BOTH
00632          IF PB-ISSUE
00633              MOVE PB-I-AH-PREM-CALC  TO  PB-I-AH-PREMIUM-AMT
00634          ELSE
00635              MOVE PB-C-AH-REF-CALC   TO  PB-C-AH-CANCEL-AMT.
00636
00637      IF PB-ISSUE
00638          IF GOOD-PEND
00639              ADD +1              TO  CARR-XTR-GOOD-IS-CNT
00640              IF PB-I-OB
00641                  COMPUTE CARR-XTR-GOOD-LF-OB =
00642                      (CARR-XTR-GOOD-LF-OB + PB-I-LF-PREMIUM-AMT
00643                      + PB-I-LF-ALT-PREMIUM-AMT)
00644                  COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =
00645                      (CARR-XTR-GOOD-LF-COM + (PB-I-LF-PREMIUM-AMT
00646                      * PB-I-LIFE-COMMISSION))
00647                  COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =
00648                      (CARR-XTR-GOOD-LF-COM +
00649                      (PB-I-LF-ALT-PREMIUM-AMT *
00650                       PB-I-LIFE-COMMISSION))
00651                  COMPUTE CARR-XTR-GOOD-AH-OB =
00652                      (CARR-XTR-GOOD-AH-OB + PB-I-AH-PREMIUM-AMT)
00653                  COMPUTE CARR-XTR-GOOD-AH-COM ROUNDED =
00654                      (CARR-XTR-GOOD-AH-COM + (PB-I-AH-PREMIUM-AMT
00655                      * PB-I-AH-COMMISSION))
00656              ELSE
100104                IF PB-I-LF-BENEFIT-CD NOT = SPACES AND ZEROS
031113                   and 'DD'
00657                    COMPUTE CARR-XTR-GOOD-LF-AMT =
00658                       (CARR-XTR-GOOD-LF-AMT + PB-I-LF-PREMIUM-AMT
00659                       + PB-I-LF-ALT-PREMIUM-AMT)
00660                    COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =
00661                       (CARR-XTR-GOOD-LF-COM + (PB-I-LF-PREMIUM-AMT
00662                       * PB-I-LIFE-COMMISSION))
00663                    COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =
00664                       (CARR-XTR-GOOD-LF-COM +
00665                       (PB-I-LF-ALT-PREMIUM-AMT *
00666                       PB-I-LIFE-COMMISSION))
100104                END-IF
00667                 COMPUTE CARR-XTR-GOOD-AH-AMT =
00668                    (CARR-XTR-GOOD-AH-AMT + PB-I-AH-PREMIUM-AMT)
100104                IF WS-SEC-PAY-CARRIER (SUB) = 'Y'
100104                   COMPUTE CARR-XTR-GOOD-AH-COM =
                            CARR-XTR-GOOD-AH-COM +
                            (PB-I-AH-PREMIUM-AMT
                             - PB-I-LF-ALT-PREMIUM-AMT
                             - PB-I-ADDL-CLP)
                      ELSE
00669                    COMPUTE CARR-XTR-GOOD-AH-COM ROUNDED =
00670                     (CARR-XTR-GOOD-AH-COM + (PB-I-AH-PREMIUM-AMT
00671                     * PB-I-AH-COMMISSION))
                      END-IF
00672          ELSE
00673              PERFORM 4100-CHECK-I-NUMERIC  THRU  4199-XIT.
00674
00675      IF PB-CANCELLATION
020816        IF (PI-COMPANY-ID = 'DCC' or 'VPP')
031113           AND (PB-CI-AH-BENEFIT-CD NOT = SPACES AND ZEROS)
031113           MOVE SPACES           TO WS-AH-CATEGORY
031113                                    ws-dcc-product-code
031113           MOVE SPACES           TO ELCNTL-KEY
031113           MOVE PI-COMPANY-ID    TO CNTL-COMP-ID
031113           MOVE '5'              TO CNTL-REC-TYPE
031113           MOVE PB-CI-AH-BENEFIT-CD
031113                                 TO CNTL-ACCESS (3:2)
031113                                    CLAS-LOOK
031113           MOVE +0               TO CNTL-SEQ-NO
031113           PERFORM 6130-FIND-BENEFIT-CD
031113                                 THRU 6130-EXIT
031113           perform 6200-get-acct thru 6200-exit
031113        END-IF
00676          IF GOOD-PEND
00677              ADD +1              TO  CARR-XTR-GOOD-CA-CNT
00678              COMPUTE CARR-XTR-GOOD-LF-CAN =
00679                  (CARR-XTR-GOOD-LF-CAN + PB-C-LF-CANCEL-AMT)
052813             compute ws-work-comm rounded =
052813                (pb-c-lf-cancel-amt * pb-ci-life-commission) * -1
052813             compute carr-xtr-good-lf-com rounded =
052813                carr-xtr-good-lf-com + ws-work-comm
052813*            COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =
052813*                CARR-XTR-GOOD-LF-COM + ((PB-C-LF-CANCEL-AMT
052813*                * PB-CI-LIFE-COMMISSION) * -1)
00683              COMPUTE CARR-XTR-GOOD-AH-CAN =
00684                  (CARR-XTR-GOOD-AH-CAN + PB-C-AH-CANCEL-AMT)
031113             if ws-ah-category = 'G' or 'L'
031113                if ws-dcc-product-code = 'DDF'
031113                   compute cnc-fact = pb-c-ah-rfnd-clp /
031113                      pb-ci-lf-alt-premium-amt
031113                else
031113                   COMPUTE CNC-FACT = PB-C-AH-CANCEL-AMT /
031113                      PB-CI-AH-PREMIUM-AMT
031113                end-if
                      COMPUTE CARR-XTR-GOOD-AH-COM =
                       CARR-XTR-GOOD-AH-COM +
                       ((CNC-FACT * (PB-CI-AH-PREMIUM-AMT -
                       PB-CI-LF-ALT-PREMIUM-AMT
                       - PB-CI-ADDL-CLP)) * -1)
                   ELSE
052813                compute ws-work-comm rounded =
052813                   (pb-c-ah-cancel-amt * pb-ci-ah-commission) * -1
052813                compute carr-xtr-good-ah-com rounded =
052813                   carr-xtr-good-ah-com + ws-work-comm
052813*               COMPUTE CARR-XTR-GOOD-AH-COM ROUNDED =
052813*                CARR-XTR-GOOD-AH-COM + ((PB-C-AH-CANCEL-AMT
052813*                * PB-CI-AH-COMMISSION) * -1)
                   END-IF
00688          ELSE
00689              PERFORM 4200-CHECK-C-NUMERIC  THRU  4299-XIT.
00690
00691      MOVE '0'                    TO  PEND-SW.
00692
00693      GO TO 4000-READNEXT.
00694
00695  4100-CHECK-I-NUMERIC.
00696      ADD +1                      TO  CARR-XTR-BAD-IS-CNT.
00697
00698      IF PB-I-LF-PREMIUM-AMT  IS NUMERIC
00699        AND  PB-I-LF-ALT-PREMIUM-AMT  IS NUMERIC
00700        AND  PB-I-LIFE-COMMISSION  IS NUMERIC
00701          IF PB-I-OB
00702              COMPUTE CARR-XTR-BAD-LF-OB =
00703                  (CARR-XTR-BAD-LF-OB + PB-I-LF-PREMIUM-AMT
00704                  + PB-I-LF-ALT-PREMIUM-AMT)
00705              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =
00706                  CARR-XTR-BAD-LF-COM + (PB-I-LF-PREMIUM-AMT
00707                  * PB-I-LIFE-COMMISSION)
00708              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =
00709                  CARR-XTR-BAD-LF-COM + (PB-I-LF-ALT-PREMIUM-AMT
00710                  * PB-I-LIFE-COMMISSION)
00711          ELSE
100104            IF PB-I-LF-BENEFIT-CD NOT = SPACES AND ZEROS and 'DD'
00712              COMPUTE CARR-XTR-BAD-LF-AMT =
00713                  (CARR-XTR-BAD-LF-AMT + PB-I-LF-PREMIUM-AMT
00714                  + PB-I-LF-ALT-PREMIUM-AMT)
00715              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =
00716                  CARR-XTR-BAD-LF-COM + (PB-I-LF-PREMIUM-AMT
00717                  * PB-I-LIFE-COMMISSION)
00718              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =
00719                  CARR-XTR-BAD-LF-COM + (PB-I-LF-ALT-PREMIUM-AMT
00720                  * PB-I-LIFE-COMMISSION).
00721
00722      IF PB-I-AH-PREMIUM-AMT  IS NUMERIC
00723        AND  PB-I-AH-COMMISSION  IS NUMERIC
00724          IF PB-I-OB
00725              COMPUTE CARR-XTR-BAD-AH-OB =
00726                  (CARR-XTR-BAD-AH-OB + PB-I-AH-PREMIUM-AMT)
00727              COMPUTE CARR-XTR-BAD-AH-COM ROUNDED =
00728                  CARR-XTR-BAD-AH-COM + (PB-I-AH-PREMIUM-AMT
00729                  * PB-I-AH-COMMISSION)
00730          ELSE
00731              COMPUTE CARR-XTR-BAD-AH-AMT =
00732                  (CARR-XTR-BAD-AH-AMT + PB-I-AH-PREMIUM-AMT)
100104             IF WS-SEC-PAY-CARRIER (SUB) = 'Y'
                      COMPUTE CARR-XTR-BAD-AH-AMT =
                       CARR-XTR-BAD-AH-AMT +
                       (PB-I-AH-PREMIUM-AMT - PB-I-LF-ALT-PREMIUM-AMT
                       - PB-I-ADDL-CLP)
100104             ELSE
00733                 COMPUTE CARR-XTR-BAD-AH-COM ROUNDED =
00734                  CARR-XTR-BAD-AH-COM + (PB-I-AH-PREMIUM-AMT
00735                  * PB-I-AH-COMMISSION).
00736
00737  4199-XIT.
00738      EXIT.
00739
00740  4200-CHECK-C-NUMERIC.
00741      ADD +1                      TO  CARR-XTR-BAD-CA-CNT.
00742
00743      IF PB-C-LF-CANCEL-AMT  IS NUMERIC
00744        AND  PB-CI-LIFE-COMMISSION  IS NUMERIC
00745          COMPUTE CARR-XTR-BAD-LF-CAN =
00746              (CARR-XTR-BAD-LF-CAN + PB-C-LF-CANCEL-AMT)
00747          COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =
00748              CARR-XTR-BAD-LF-COM + ((PB-C-LF-CANCEL-AMT
00749              * PB-CI-LIFE-COMMISSION) * -1).
00750
00751      IF PB-C-AH-CANCEL-AMT  IS NUMERIC
00752         AND PB-CI-AH-COMMISSION  IS NUMERIC
00753         COMPUTE CARR-XTR-BAD-AH-CAN =
00754              (CARR-XTR-BAD-AH-CAN + PB-C-AH-CANCEL-AMT)
031113        if ws-ah-category = 'G' or 'L'
031113           if ws-dcc-product-code = 'DDF'
031113              compute cnc-fact = pb-c-ah-rfnd-clp /
031113                  pb-ci-lf-alt-premium-amt
031113           else
031113              COMPUTE CNC-FACT = PB-C-AH-CANCEL-AMT /
031113                 PB-CI-AH-PREMIUM-AMT
031113           end-if
100104           COMPUTE CARR-XTR-BAD-AH-COM =
100104              CARR-XTR-BAD-AH-COM +
100104              ((CNC-FACT * (PB-CI-AH-PREMIUM-AMT -
100104              PB-CI-LF-ALT-PREMIUM-AMT
100104              - PB-CI-ADDL-CLP)) * -1)
100104        ELSE
00755            COMPUTE CARR-XTR-BAD-AH-COM ROUNDED =
00756               CARR-XTR-BAD-AH-COM + ((PB-C-AH-CANCEL-AMT
00757               * PB-CI-AH-COMMISSION) * -1)
              end-if
           end-if
           .
00759  4299-XIT.
00760      EXIT.
00761  EJECT
00762  4300-PRINT-CARRIER.
100104     PERFORM VARYING SUB FROM +1 BY +1 UNTIL
              (SUB > +31)
              OR (PB-CARRIER = WS-CARRIER-CODE (SUB))
           END-PERFORM
00763      IF USE-CARRIER-TOTALS
00764          NEXT SENTENCE
00765      ELSE
00766          GO TO 4320-ROLL-TOTALS.
00767
00768      IF PREVIOUS-CARRIER  IS EQUAL TO  LOW-VALUES
00769          GO TO 4330-SET-UP-CONTROL.
00770
00771  4310-PRINT-REPORT.
00772      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-DET1-LF-HDG
00773                                      WS-DET2-LF-HDG
00774                                      WS-DET5-LF-HDG
00775                                      WS-DET7-LF-HDG
00776                                      WS-DET9-LF-HDG.
00777      MOVE PI-AH-OVERRIDE-L6      TO  WS-DET3-AH-HDG
00778                                      WS-DET4-AH-HDG
00779                                      WS-DET6-AH-HDG
00780                                      WS-DET8-AH-HDG
00781                                      WS-DETA-AH-HDG.
00782      MOVE WS-HEADING1            TO  RF-DATA-133.
00783      MOVE '1'                    TO  RF-CTL-CHAR-133.
00784
00785      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00786
00787      MOVE WS-HEADING2            TO  RF-DATA-133.
00788      MOVE ' '                    TO  RF-CTL-CHAR-133.
00789
00790      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00791
00792      ADD +1                      TO  WS-PAGE-NUMBER.
00793      MOVE WS-PAGE-NUMBER         TO  WS-H3-PAGE.
00794      MOVE WS-HEADING3            TO  RF-DATA-133.
00795      MOVE ' '                    TO  RF-CTL-CHAR-133.
00796
00797      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00798
00799      MOVE PREVIOUS-CARRIER       TO  WS-H4-CARRIER.
00800      MOVE WS-HEADING4            TO  RF-DATA-133.
00801      MOVE ' '                    TO  RF-CTL-CHAR-133.
00802
00803      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00804
00805      MOVE WS-HEADING5            TO  RF-DATA-133.
00806      MOVE '0'                    TO  RF-CTL-CHAR-133.
00807
00808      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00809
00810      MOVE CARR-XTR-GOOD-LF-AMT   TO  WS-IS-LF-SP-GOOD.
00811      MOVE CARR-XTR-GOOD-LF-OB    TO  WS-IS-LF-OB-GOOD.
00812      MOVE CARR-XTR-GOOD-LF-CAN   TO  WS-CA-LF-GOOD.
           MOVE CARR-XTR-GOOD-LF-COM   TO  WS-CM-LF-GOOD.
00814      MOVE CARR-XTR-GOOD-AH-AMT   TO  WS-IS-AH-SP-GOOD.
00815      MOVE CARR-XTR-GOOD-AH-OB    TO  WS-IS-AH-OB-GOOD.
00816      MOVE CARR-XTR-GOOD-AH-CAN   TO  WS-CA-AH-GOOD.
           MOVE CARR-XTR-GOOD-AH-COM   TO  WS-CM-AH-GOOD.
00818      MOVE CARR-XTR-BAD-LF-AMT    TO  WS-IS-LF-SP-BAD.
00819      MOVE CARR-XTR-BAD-LF-OB     TO  WS-IS-LF-OB-BAD.
00820      MOVE CARR-XTR-BAD-LF-CAN    TO  WS-CA-LF-BAD.
00821      MOVE CARR-XTR-BAD-LF-COM    TO  WS-CM-LF-BAD.
00822      MOVE CARR-XTR-BAD-AH-AMT    TO  WS-IS-AH-SP-BAD.
00823      MOVE CARR-XTR-BAD-AH-OB     TO  WS-IS-AH-OB-BAD.
00824      MOVE CARR-XTR-BAD-AH-CAN    TO  WS-CA-AH-BAD.
00825      MOVE CARR-XTR-BAD-AH-COM    TO  WS-CM-AH-BAD.
00826      MOVE CARR-XTR-GOOD-IS-CNT   TO  WS-IS-CNT-GOOD.
00827      MOVE CARR-XTR-BAD-IS-CNT    TO  WS-IS-CNT-BAD.
00828      MOVE CARR-XTR-GOOD-CA-CNT   TO  WS-CA-CNT-GOOD.
00829      MOVE CARR-XTR-BAD-CA-CNT    TO  WS-CA-CNT-BAD.
00830
00831      ADD CARR-XTR-GOOD-IS-CNT    CARR-XTR-BAD-IS-CNT
00832              GIVING WS-IS-CNT-TOT.
00833      ADD CARR-XTR-GOOD-CA-CNT    CARR-XTR-BAD-CA-CNT
00834              GIVING WS-CA-CNT-TOT.
00835
00836      COMPUTE CARR-XTR-GOOD-LF-NET =
00837          (CARR-XTR-GOOD-LF-AMT + CARR-XTR-GOOD-LF-OB
00838          - CARR-XTR-GOOD-LF-CAN).
00839
00840      COMPUTE CARR-XTR-GOOD-AH-NET =
00841          (CARR-XTR-GOOD-AH-AMT + CARR-XTR-GOOD-AH-OB
00842          - CARR-XTR-GOOD-AH-CAN).
00843
00844      COMPUTE CARR-XTR-BAD-LF-NET =
00845          (CARR-XTR-BAD-LF-AMT + CARR-XTR-BAD-LF-OB
00846          - CARR-XTR-BAD-LF-CAN).
00847
00848      COMPUTE CARR-XTR-BAD-AH-NET =
00849          (CARR-XTR-BAD-AH-AMT + CARR-XTR-BAD-AH-OB
00850          - CARR-XTR-BAD-AH-CAN).
00851
00852      MOVE CARR-XTR-GOOD-LF-NET   TO  WS-NET-LF-GOOD.
00853      MOVE CARR-XTR-GOOD-AH-NET   TO  WS-NET-AH-GOOD.
00854      MOVE CARR-XTR-BAD-LF-NET    TO  WS-NET-LF-BAD.
00855      MOVE CARR-XTR-BAD-AH-NET    TO  WS-NET-AH-BAD.
00856
00857      ADD CARR-XTR-GOOD-LF-NET    CARR-XTR-BAD-LF-NET
00858              GIVING WS-NET-LF-TOT.
00859      ADD CARR-XTR-GOOD-AH-NET    CARR-XTR-BAD-AH-NET
00860              GIVING WS-NET-AH-TOT.
00861
00862      ADD CARR-XTR-GOOD-LF-AMT    CARR-XTR-BAD-LF-AMT
00863              GIVING WS-IS-LF-SP-TOT.
00864      ADD CARR-XTR-GOOD-LF-OB     CARR-XTR-BAD-LF-OB
00865              GIVING WS-IS-LF-OB-TOT.
00866      ADD CARR-XTR-GOOD-LF-CAN    CARR-XTR-BAD-LF-CAN
00867              GIVING WS-CA-LF-TOT.
00868      ADD CARR-XTR-GOOD-LF-COM    CARR-XTR-BAD-LF-COM
00869              GIVING WS-CM-LF-TOT.
00870
00871      ADD CARR-XTR-GOOD-AH-AMT    CARR-XTR-BAD-AH-AMT
00872              GIVING WS-IS-AH-SP-TOT.
00873      ADD CARR-XTR-GOOD-AH-OB     CARR-XTR-BAD-AH-OB
00874              GIVING WS-IS-AH-OB-TOT.
00875      ADD CARR-XTR-GOOD-AH-CAN    CARR-XTR-BAD-AH-CAN
00876              GIVING WS-CA-AH-TOT.
00877      ADD CARR-XTR-GOOD-AH-COM    CARR-XTR-BAD-AH-COM
00878              GIVING WS-CM-AH-TOT.
00879
00880      COMPUTE CARR-XTR-GOOD-IS-TOT =
00881          (CARR-XTR-GOOD-LF-AMT + CARR-XTR-GOOD-LF-OB +
00882           CARR-XTR-GOOD-AH-AMT + CARR-XTR-GOOD-AH-OB).
00883
00884      COMPUTE CARR-XTR-BAD-IS-TOT =
00885          (CARR-XTR-BAD-LF-AMT + CARR-XTR-BAD-LF-OB +
00886           CARR-XTR-BAD-AH-AMT + CARR-XTR-BAD-AH-OB).
00887
00888      COMPUTE CARR-XTR-GOOD-CA-TOT =
00889          (CARR-XTR-GOOD-LF-CAN + CARR-XTR-GOOD-AH-CAN).
00890
00891      COMPUTE CARR-XTR-BAD-CA-TOT =
00892          (CARR-XTR-BAD-LF-CAN + CARR-XTR-BAD-AH-CAN).
00893
00894      COMPUTE CARR-XTR-GOOD-NET-TOT =
00895          (CARR-XTR-GOOD-LF-NET + CARR-XTR-GOOD-AH-NET).
00896
00897      COMPUTE CARR-XTR-BAD-NET-TOT =
00898          (CARR-XTR-BAD-LF-NET + CARR-XTR-BAD-AH-NET).
00899
00900      COMPUTE CARR-XTR-GOOD-CM-TOT =
00901          (CARR-XTR-GOOD-LF-COM + CARR-XTR-GOOD-AH-COM).
00902
00903      COMPUTE CARR-XTR-BAD-CM-TOT =
00904          (CARR-XTR-BAD-LF-COM + CARR-XTR-BAD-AH-COM).
00905
00906      MOVE CARR-XTR-GOOD-IS-TOT   TO  WS-IS-TOT-GOOD.
00907      MOVE CARR-XTR-GOOD-CA-TOT   TO  WS-CA-TOT-GOOD.
00908      MOVE CARR-XTR-GOOD-NET-TOT  TO  WS-NET-TOT-GOOD.
00909      MOVE CARR-XTR-GOOD-CM-TOT   TO  WS-CM-TOT-GOOD.
00910      MOVE CARR-XTR-BAD-IS-TOT    TO  WS-IS-TOT-BAD.
00911      MOVE CARR-XTR-BAD-CA-TOT    TO  WS-CA-TOT-BAD.
00912      MOVE CARR-XTR-BAD-NET-TOT   TO  WS-NET-TOT-BAD.
00913      MOVE CARR-XTR-BAD-CM-TOT    TO  WS-CM-TOT-BAD.
00914
00915      ADD CARR-XTR-GOOD-IS-TOT    CARR-XTR-BAD-IS-TOT
00916              GIVING WS-IS-TOT-TOT.
00917      ADD CARR-XTR-GOOD-CA-TOT    CARR-XTR-BAD-CA-TOT
00918              GIVING WS-CA-TOT-TOT.
00919      ADD CARR-XTR-GOOD-NET-TOT   CARR-XTR-BAD-NET-TOT
00920              GIVING WS-NET-TOT-TOT.
00921      ADD CARR-XTR-GOOD-CM-TOT    CARR-XTR-BAD-CM-TOT
00922              GIVING WS-CM-TOT-TOT.
00923
00924      MOVE WS-DETAIL1             TO  RF-DATA-133.
00925      MOVE '0'                    TO  RF-CTL-CHAR-133.
00926
00927      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00928
00929      MOVE WS-DETAIL2             TO  RF-DATA-133.
00930      MOVE ' '                    TO  RF-CTL-CHAR-133.
00931
00932      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00933
00934      MOVE WS-DETAIL3             TO  RF-DATA-133.
00935      MOVE ' '                    TO  RF-CTL-CHAR-133.
00936
00937      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00938
00939      MOVE WS-DETAIL4             TO  RF-DATA-133.
00940      MOVE ' '                    TO  RF-CTL-CHAR-133.
00941
00942      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00943
00944      MOVE WS-DETAIL5             TO  RF-DATA-133.
00945      MOVE '0'                    TO  RF-CTL-CHAR-133.
00946
00947      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00948
00949      MOVE WS-DETAIL6             TO  RF-DATA-133.
00950      MOVE ' '                    TO  RF-CTL-CHAR-133.
00951
00952      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00953
00954      MOVE WS-DETAIL7             TO  RF-DATA-133.
00955      MOVE '0'                    TO  RF-CTL-CHAR-133.
00956
00957      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00958
00959      MOVE WS-DETAIL8             TO  RF-DATA-133.
00960      MOVE ' '                    TO  RF-CTL-CHAR-133.
00961
00962      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00963
00964      MOVE WS-DETAIL9             TO  RF-DATA-133.
00965      MOVE '0'                    TO  RF-CTL-CHAR-133.
00966
00967      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00968
00969      MOVE WS-DETAILA             TO  RF-DATA-133.
00970      MOVE ' '                    TO  RF-CTL-CHAR-133.
00971
00972      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00973
00974      MOVE WS-TOTAL1              TO  RF-DATA-133.
00975      MOVE '-'                    TO  RF-CTL-CHAR-133.
00976
00977      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00978
00979      MOVE WS-TOTAL2              TO  RF-DATA-133.
00980      MOVE ' '                    TO  RF-CTL-CHAR-133.
00981
00982      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00983
00984      MOVE WS-TOTAL3              TO  RF-DATA-133.
00985      MOVE ' '                    TO  RF-CTL-CHAR-133.
00986
00987      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00988
00989      MOVE WS-TOTAL4              TO  RF-DATA-133.
00990      MOVE ' '                    TO  RF-CTL-CHAR-133.
00991
00992      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00993
00994      MOVE WS-TOTAL5              TO  RF-DATA-133.
00995      MOVE '0'                    TO  RF-CTL-CHAR-133.
00996
00997      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
00998
00999      MOVE WS-TOTAL6              TO  RF-DATA-133.
01000      MOVE ' '                    TO  RF-CTL-CHAR-133.
01001
01002      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01003
01004  4320-ROLL-TOTALS.
01005      COMPUTE REPT-XTR-GOOD-LF-AMT  = REPT-XTR-GOOD-LF-AMT
01006                                    + CARR-XTR-GOOD-LF-AMT.
01007      COMPUTE REPT-XTR-GOOD-LF-OB   = REPT-XTR-GOOD-LF-OB
01008                                    + CARR-XTR-GOOD-LF-OB.
01009      COMPUTE REPT-XTR-GOOD-LF-CAN  = REPT-XTR-GOOD-LF-CAN
01010                                    + CARR-XTR-GOOD-LF-CAN.
01011      COMPUTE REPT-XTR-GOOD-LF-COM  = REPT-XTR-GOOD-LF-COM
01012                                    + CARR-XTR-GOOD-LF-COM.
01013      COMPUTE REPT-XTR-GOOD-AH-AMT  = REPT-XTR-GOOD-AH-AMT
01014                                    + CARR-XTR-GOOD-AH-AMT.
01015      COMPUTE REPT-XTR-GOOD-AH-OB   = REPT-XTR-GOOD-AH-OB
01016                                    + CARR-XTR-GOOD-AH-OB.
01017      COMPUTE REPT-XTR-GOOD-AH-CAN  = REPT-XTR-GOOD-AH-CAN
01018                                    + CARR-XTR-GOOD-AH-CAN.
01019      COMPUTE REPT-XTR-GOOD-AH-COM  = REPT-XTR-GOOD-AH-COM
01020                                    + CARR-XTR-GOOD-AH-COM.
01021      COMPUTE REPT-XTR-BAD-LF-AMT   = REPT-XTR-BAD-LF-AMT
01022                                    + CARR-XTR-BAD-LF-AMT.
01023      COMPUTE REPT-XTR-BAD-LF-OB    = REPT-XTR-BAD-LF-OB
01024                                    + CARR-XTR-BAD-LF-OB.
01025      COMPUTE REPT-XTR-BAD-LF-CAN   = REPT-XTR-BAD-LF-CAN
01026                                    + CARR-XTR-BAD-LF-CAN.
01027      COMPUTE REPT-XTR-BAD-LF-COM   = REPT-XTR-BAD-LF-COM
01028                                    + CARR-XTR-BAD-LF-COM.
01029      COMPUTE REPT-XTR-BAD-AH-AMT   = REPT-XTR-BAD-AH-AMT
01030                                    + CARR-XTR-BAD-AH-AMT.
01031      COMPUTE REPT-XTR-BAD-AH-OB    = REPT-XTR-BAD-AH-OB
01032                                    + CARR-XTR-BAD-AH-OB.
01033      COMPUTE REPT-XTR-BAD-AH-CAN   = REPT-XTR-BAD-AH-CAN
01034                                    + CARR-XTR-BAD-AH-CAN.
01035      COMPUTE REPT-XTR-BAD-AH-COM   = REPT-XTR-BAD-AH-COM
01036                                    + CARR-XTR-BAD-AH-COM.
01037      COMPUTE REPT-XTR-GOOD-IS-CNT  = REPT-XTR-GOOD-IS-CNT
01038                                    + CARR-XTR-GOOD-IS-CNT.
01039      COMPUTE REPT-XTR-BAD-IS-CNT   = REPT-XTR-BAD-IS-CNT
01040                                    + CARR-XTR-BAD-IS-CNT.
01041      COMPUTE REPT-XTR-GOOD-CA-CNT  = REPT-XTR-GOOD-CA-CNT
01042                                    + CARR-XTR-GOOD-CA-CNT.
01043      COMPUTE REPT-XTR-BAD-CA-CNT   = REPT-XTR-BAD-CA-CNT
01044                                    + CARR-XTR-BAD-CA-CNT.
01045
01046      MOVE ZEROS                  TO  CARR-XTR-GOOD-LF-AMT
01047                                      CARR-XTR-GOOD-LF-OB
01048                                      CARR-XTR-GOOD-LF-CAN
01049                                      CARR-XTR-GOOD-LF-COM
01050                                      CARR-XTR-GOOD-AH-AMT
01051                                      CARR-XTR-GOOD-AH-OB
01052                                      CARR-XTR-GOOD-AH-CAN
01053                                      CARR-XTR-GOOD-AH-COM
01054                                      CARR-XTR-BAD-LF-AMT
01055                                      CARR-XTR-BAD-LF-OB
01056                                      CARR-XTR-BAD-LF-CAN
01057                                      CARR-XTR-BAD-LF-COM
01058                                      CARR-XTR-BAD-AH-AMT
01059                                      CARR-XTR-BAD-AH-OB
01060                                      CARR-XTR-BAD-AH-CAN
01061                                      CARR-XTR-BAD-AH-COM
01062                                      CARR-XTR-GOOD-IS-CNT
01063                                      CARR-XTR-BAD-IS-CNT
01064                                      CARR-XTR-GOOD-CA-CNT
01065                                      CARR-XTR-BAD-CA-CNT
01066                                      CARR-XTR-GOOD-LF-NET
01067                                      CARR-XTR-GOOD-AH-NET
01068                                      CARR-XTR-BAD-LF-NET
01069                                      CARR-XTR-BAD-AH-NET
01070                                      CARR-XTR-GOOD-IS-TOT
01071                                      CARR-XTR-GOOD-CA-TOT
01072                                      CARR-XTR-GOOD-NET-TOT
01073                                      CARR-XTR-GOOD-CM-TOT
01074                                      CARR-XTR-BAD-IS-TOT
01075                                      CARR-XTR-BAD-CA-TOT
01076                                      CARR-XTR-BAD-NET-TOT
01077                                      CARR-XTR-BAD-CM-TOT.
01078
01079  4330-SET-UP-CONTROL.
01080      MOVE PB-CARRIER             TO  PREVIOUS-CARRIER.
01081
01082  4399-EXIT.
01083      EXIT.
01084  EJECT
01085  4900-ENDBR.
01086      IF GOOD-STARTBR  = 'YES'
01087          PERFORM 4300-PRINT-CARRIER  THRU  4399-EXIT
01088          
      * EXEC CICS  ENDBR
01089 *            DATASET  (PEND-FILE-ID)
01090 *            END-EXEC.
           MOVE 0
             TO DFHEIV11
      *    MOVE '&2                    $   #00004454' TO DFHEIV0
           MOVE X'263220202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202420' TO DFHEIV0(13:12)
           MOVE X'2020233030303034343534' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PEND-FILE-ID, 
                 DFHEIV11, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01091
01092  5000-PRINT-REPORT.
01093      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-DET1-LF-HDG
01094                                      WS-DET2-LF-HDG
01095                                      WS-DET5-LF-HDG
01096                                      WS-DET7-LF-HDG
01097                                      WS-DET9-LF-HDG.
01098      MOVE PI-AH-OVERRIDE-L6      TO  WS-DET3-AH-HDG
01099                                      WS-DET4-AH-HDG
01100                                      WS-DET6-AH-HDG
01101                                      WS-DET8-AH-HDG
01102                                      WS-DETA-AH-HDG.
01103      MOVE WS-HEADING1            TO  RF-DATA-133.
01104      MOVE '1'                    TO  RF-CTL-CHAR-133.
01105
01106      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01107
01108      MOVE WS-HEADING2            TO  RF-DATA-133.
01109      MOVE ' '                    TO  RF-CTL-CHAR-133.
01110
01111      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01112
01113      ADD +1                      TO  WS-PAGE-NUMBER.
01114      MOVE WS-PAGE-NUMBER         TO  WS-H3-PAGE.
01115      MOVE WS-HEADING3            TO  RF-DATA-133.
01116      MOVE ' '                    TO  RF-CTL-CHAR-133.
01117
01118      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01119
01120      MOVE WS-HEADING5            TO  RF-DATA-133.
01121      MOVE '0'                    TO  RF-CTL-CHAR-133.
01122
01123      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01124
01125      MOVE REPT-XTR-GOOD-LF-AMT   TO  WS-IS-LF-SP-GOOD.
01126      MOVE REPT-XTR-GOOD-LF-OB    TO  WS-IS-LF-OB-GOOD.
01127      MOVE REPT-XTR-GOOD-LF-CAN   TO  WS-CA-LF-GOOD.
           MOVE REPT-XTR-GOOD-LF-COM   TO  WS-CM-LF-GOOD.
01129      MOVE REPT-XTR-GOOD-AH-AMT   TO  WS-IS-AH-SP-GOOD.
01130      MOVE REPT-XTR-GOOD-AH-OB    TO  WS-IS-AH-OB-GOOD.
01131      MOVE REPT-XTR-GOOD-AH-CAN   TO  WS-CA-AH-GOOD.
           MOVE REPT-XTR-GOOD-AH-COM   TO  WS-CM-AH-GOOD.
01133      MOVE REPT-XTR-BAD-LF-AMT    TO  WS-IS-LF-SP-BAD.
01134      MOVE REPT-XTR-BAD-LF-OB     TO  WS-IS-LF-OB-BAD.
01135      MOVE REPT-XTR-BAD-LF-CAN    TO  WS-CA-LF-BAD.
01136      MOVE REPT-XTR-BAD-LF-COM    TO  WS-CM-LF-BAD.
01137      MOVE REPT-XTR-BAD-AH-AMT    TO  WS-IS-AH-SP-BAD.
01138      MOVE REPT-XTR-BAD-AH-OB     TO  WS-IS-AH-OB-BAD.
01139      MOVE REPT-XTR-BAD-AH-CAN    TO  WS-CA-AH-BAD.
01140      MOVE REPT-XTR-BAD-AH-COM    TO  WS-CM-AH-BAD.
01141      MOVE REPT-XTR-GOOD-IS-CNT   TO  WS-IS-CNT-GOOD.
01142      MOVE REPT-XTR-BAD-IS-CNT    TO  WS-IS-CNT-BAD.
01143      MOVE REPT-XTR-GOOD-CA-CNT   TO  WS-CA-CNT-GOOD.
01144      MOVE REPT-XTR-BAD-CA-CNT    TO  WS-CA-CNT-BAD.
01145
01146      ADD REPT-XTR-GOOD-IS-CNT    REPT-XTR-BAD-IS-CNT
01147              GIVING WS-IS-CNT-TOT.
01148      ADD REPT-XTR-GOOD-CA-CNT    REPT-XTR-BAD-CA-CNT
01149              GIVING WS-CA-CNT-TOT.
01150
01151      COMPUTE REPT-XTR-GOOD-LF-NET =
01152          (REPT-XTR-GOOD-LF-AMT + REPT-XTR-GOOD-LF-OB
01153          - REPT-XTR-GOOD-LF-CAN).
01154
01155      COMPUTE REPT-XTR-GOOD-AH-NET =
01156          (REPT-XTR-GOOD-AH-AMT + REPT-XTR-GOOD-AH-OB
01157          - REPT-XTR-GOOD-AH-CAN).
01158
01159      COMPUTE REPT-XTR-BAD-LF-NET =
01160          (REPT-XTR-BAD-LF-AMT + REPT-XTR-BAD-LF-OB
01161          - REPT-XTR-BAD-LF-CAN).
01162
01163      COMPUTE REPT-XTR-BAD-AH-NET =
01164          (REPT-XTR-BAD-AH-AMT + REPT-XTR-BAD-AH-OB
01165          - REPT-XTR-BAD-AH-CAN).
01166
01167      MOVE REPT-XTR-GOOD-LF-NET   TO  WS-NET-LF-GOOD.
01168      MOVE REPT-XTR-GOOD-AH-NET   TO  WS-NET-AH-GOOD.
01169      MOVE REPT-XTR-BAD-LF-NET    TO  WS-NET-LF-BAD.
01170      MOVE REPT-XTR-BAD-AH-NET    TO  WS-NET-AH-BAD.
01171
01172      ADD REPT-XTR-GOOD-LF-NET    REPT-XTR-BAD-LF-NET
01173              GIVING WS-NET-LF-TOT.
01174      ADD REPT-XTR-GOOD-AH-NET    REPT-XTR-BAD-AH-NET
01175              GIVING WS-NET-AH-TOT.
01176
01177      ADD REPT-XTR-GOOD-LF-AMT    REPT-XTR-BAD-LF-AMT
01178              GIVING WS-IS-LF-SP-TOT.
01179      ADD REPT-XTR-GOOD-LF-OB     REPT-XTR-BAD-LF-OB
01180              GIVING WS-IS-LF-OB-TOT.
01181      ADD REPT-XTR-GOOD-LF-CAN    REPT-XTR-BAD-LF-CAN
01182              GIVING WS-CA-LF-TOT.
01183      ADD REPT-XTR-GOOD-LF-COM    REPT-XTR-BAD-LF-COM
01184              GIVING WS-CM-LF-TOT.
01185
01186      ADD REPT-XTR-GOOD-AH-AMT    REPT-XTR-BAD-AH-AMT
01187              GIVING WS-IS-AH-SP-TOT.
01188      ADD REPT-XTR-GOOD-AH-OB     REPT-XTR-BAD-AH-OB
01189              GIVING WS-IS-AH-OB-TOT.
01190      ADD REPT-XTR-GOOD-AH-CAN    REPT-XTR-BAD-AH-CAN
01191              GIVING WS-CA-AH-TOT.
01192      ADD REPT-XTR-GOOD-AH-COM    REPT-XTR-BAD-AH-COM
01193              GIVING WS-CM-AH-TOT.
01194
01195      COMPUTE REPT-XTR-GOOD-IS-TOT =
01196          (REPT-XTR-GOOD-LF-AMT + REPT-XTR-GOOD-LF-OB +
01197           REPT-XTR-GOOD-AH-AMT + REPT-XTR-GOOD-AH-OB).
01198
01199      COMPUTE REPT-XTR-BAD-IS-TOT =
01200          (REPT-XTR-BAD-LF-AMT + REPT-XTR-BAD-LF-OB +
01201           REPT-XTR-BAD-AH-AMT + REPT-XTR-BAD-AH-OB).
01202
01203      COMPUTE REPT-XTR-GOOD-CA-TOT =
01204          (REPT-XTR-GOOD-LF-CAN + REPT-XTR-GOOD-AH-CAN).
01205
01206      COMPUTE REPT-XTR-BAD-CA-TOT =
01207          (REPT-XTR-BAD-LF-CAN + REPT-XTR-BAD-AH-CAN).
01208
01209      COMPUTE REPT-XTR-GOOD-NET-TOT =
01210          (REPT-XTR-GOOD-LF-NET + REPT-XTR-GOOD-AH-NET).
01211
01212      COMPUTE REPT-XTR-BAD-NET-TOT =
01213          (REPT-XTR-BAD-LF-NET + REPT-XTR-BAD-AH-NET).
01214
01215      COMPUTE REPT-XTR-GOOD-CM-TOT =
01216          (REPT-XTR-GOOD-LF-COM + REPT-XTR-GOOD-AH-COM)
01217
01218      COMPUTE REPT-XTR-BAD-CM-TOT =
01219          (REPT-XTR-BAD-LF-COM + REPT-XTR-BAD-AH-COM).
01220
01221      MOVE REPT-XTR-GOOD-IS-TOT   TO  WS-IS-TOT-GOOD.
01222      MOVE REPT-XTR-GOOD-CA-TOT   TO  WS-CA-TOT-GOOD.
01223      MOVE REPT-XTR-GOOD-NET-TOT  TO  WS-NET-TOT-GOOD.
01224      MOVE REPT-XTR-GOOD-CM-TOT   TO  WS-CM-TOT-GOOD.
01225      MOVE REPT-XTR-BAD-IS-TOT    TO  WS-IS-TOT-BAD.
01226      MOVE REPT-XTR-BAD-CA-TOT    TO  WS-CA-TOT-BAD.
01227      MOVE REPT-XTR-BAD-NET-TOT   TO  WS-NET-TOT-BAD.
01228      MOVE REPT-XTR-BAD-CM-TOT    TO  WS-CM-TOT-BAD.
01229
01230      ADD REPT-XTR-GOOD-IS-TOT    REPT-XTR-BAD-IS-TOT
01231              GIVING WS-IS-TOT-TOT.
01232      ADD REPT-XTR-GOOD-CA-TOT    REPT-XTR-BAD-CA-TOT
01233              GIVING WS-CA-TOT-TOT.
01234      ADD REPT-XTR-GOOD-NET-TOT   REPT-XTR-BAD-NET-TOT
01235              GIVING WS-NET-TOT-TOT.
01236      ADD REPT-XTR-GOOD-CM-TOT    REPT-XTR-BAD-CM-TOT
01237              GIVING WS-CM-TOT-TOT.
01238
01239      MOVE WS-DETAIL1             TO  RF-DATA-133.
01240      MOVE '0'                    TO  RF-CTL-CHAR-133.
01241
01242      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01243
01244      MOVE WS-DETAIL2             TO  RF-DATA-133.
01245      MOVE ' '                    TO  RF-CTL-CHAR-133.
01246
01247      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01248
01249      MOVE WS-DETAIL3             TO  RF-DATA-133.
01250      MOVE ' '                    TO  RF-CTL-CHAR-133.
01251
01252      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01253
01254      MOVE WS-DETAIL4             TO  RF-DATA-133.
01255      MOVE ' '                    TO  RF-CTL-CHAR-133.
01256
01257      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01258
01259      MOVE WS-DETAIL5             TO  RF-DATA-133.
01260      MOVE '0'                    TO  RF-CTL-CHAR-133.
01261
01262      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01263
01264      MOVE WS-DETAIL6             TO  RF-DATA-133.
01265      MOVE ' '                    TO  RF-CTL-CHAR-133.
01266
01267      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01268
01269      MOVE WS-DETAIL7             TO  RF-DATA-133.
01270      MOVE '0'                    TO  RF-CTL-CHAR-133.
01271
01272      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01273
01274      MOVE WS-DETAIL8             TO  RF-DATA-133.
01275      MOVE ' '                    TO  RF-CTL-CHAR-133.
01276
01277      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01278
01279      MOVE WS-DETAIL9             TO  RF-DATA-133.
01280      MOVE '0'                    TO  RF-CTL-CHAR-133.
01281
01282      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01283
01284      MOVE WS-DETAILA             TO  RF-DATA-133.
01285      MOVE ' '                    TO  RF-CTL-CHAR-133.
01286
01287      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01288
01289      MOVE WS-TOTAL1              TO  RF-DATA-133.
01290      MOVE '-'                    TO  RF-CTL-CHAR-133.
01291
01292      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01293
01294      MOVE WS-TOTAL2              TO  RF-DATA-133.
01295      MOVE ' '                    TO  RF-CTL-CHAR-133.
01296
01297      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01298
01299      MOVE WS-TOTAL3              TO  RF-DATA-133.
01300      MOVE ' '                    TO  RF-CTL-CHAR-133.
01301
01302      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01303
01304      MOVE WS-TOTAL4              TO  RF-DATA-133.
01305      MOVE ' '                    TO  RF-CTL-CHAR-133.
01306
01307      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01308
01309      MOVE WS-TOTAL5              TO  RF-DATA-133.
01310      MOVE '0'                    TO  RF-CTL-CHAR-133.
01311
01312      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01313
01314      MOVE WS-TOTAL6              TO  RF-DATA-133.
01315      MOVE ' '                    TO  RF-CTL-CHAR-133.
01316
01317      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.
01318
01319  5100-DELETE-INITIAL-2.
01320      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
01321      MOVE 'RF'                   TO  RF-RECORD-ID.
01322      MOVE '2'                    TO  RF-RECORD-TYPE.
01323      MOVE 'EL676'                TO  RF-REPORT-ID.
01324      MOVE ZEROS                  TO  RF-LINE-NUMBER.
01325
01326      
      * EXEC CICS  DELETE
01327 *        DATASET    (REPT-FILE-ID)
01328 *        RIDFLD     (RF-CONTROL-PRIMARY)
01329 *        KEYLENGTH  (11)
01330 *        END-EXEC.
           MOVE 11
             TO DFHEIV11
      *    MOVE '&(  RK                &   #00004692' TO DFHEIV0
           MOVE X'26282020524B202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202620' TO DFHEIV0(13:12)
           MOVE X'2020233030303034363932' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01331
01332  5200-WRITE-TRAILER.
01333      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
01334      MOVE 'RF'                   TO  RF-RECORD-ID.
01335      MOVE 'EL676'                TO  RF-REPORT-ID.
01336      MOVE '2'                    TO  RF-RECORD-TYPE.
01337
01338      ADD +1                      TO  WS-LINE-NUMBER.
01339
01340      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.
01341      MOVE SPACES                 TO  RF-TRAILER-RECORD.
01342
01343      
      * EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)
01344 *    END-EXEC
      *    MOVE '0"A                   "   #00004709' TO DFHEIV0
           MOVE X'302241202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202220' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01345      
      * EXEC CICS FORMATTIME
01346 *              ABSTIME(LCP-CICS-TIME)
01347 *              TIME(LCP-TIME-OF-DAY-XX)
01348 *    END-EXEC
      *    MOVE 'j$(     (             #   #00004711' TO DFHEIV0
           MOVE X'6A2428202020202028202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202320' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373131' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 LCP-CICS-TIME, 
                 LCP-TIME-OF-DAY-XX
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
01349      MOVE LCP-TIME-OF-DAY-68     TO  RF-PRINT-HH-MM-SS.
01350      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.
01351
01352      
      * EXEC CICS  WRITE
01353 *        DATASET  (REPT-FILE-ID)
01354 *        FROM     (REPORT-SAVE-FILE)
01355 *        RIDFLD   (RF-CONTROL-PRIMARY)
01356 *        END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004718' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373138' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01357
01358      GO TO 9999-RETURN-CICS
           .
031113 6130-FIND-BENEFIT-CD.
031113
031113     
      * EXEC CICS READ
031113*       DATASET (CNTL-FILE-ID)
031113*       SET     (ADDRESS OF CONTROL-FILE)
031113*       RIDFLD  (ELCNTL-KEY)
031113*       GTEQ
031113*       RESP    (WS-RESPONSE)
031113*    END-EXEC
      *    MOVE '&"S        G          (  N#00004728' TO DFHEIV0
           MOVE X'262253202020202020202047' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034373238' TO DFHEIV0(25:11)
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
           MOVE EIBRESP  TO WS-RESPONSE
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031113
031113     IF RESP-NORMAL
031113        IF (CF-COMPANY-ID = PI-COMPANY-ID)
031113           AND (CF-RECORD-TYPE = '5')
031113           PERFORM VARYING S1 FROM +1 BY +1 UNTIL
031113              (S1 > +8)
031113              OR (CF-BENEFIT-CODE (S1) = CLAS-LOOK)
031113           END-PERFORM
031113           IF S1 <= +8
031113              MOVE CF-BENEFIT-CATEGORY (S1)
031113                                 TO WS-AH-CATEGORY
031113              GO TO 6130-EXIT
031113           END-IF
031113        END-IF
031113     END-IF
031113
031113     .
031113 6130-EXIT.
031113     EXIT.
031113
031113 6200-get-acct.
031113
031113     move low-values             to eracct-key
031113     move pb-control-by-account (1:20)
031113                                 to eracct-key (1:20)
031113     move pb-acct-exp-dt         to acct-exp-date
031113     
      * exec cics read
031113*       dataset       ('ERACCT')
031113*       ridfld        (eracct-key)
031113*       set           (address of account-master)
031113*       resp          (ws-response)
031113*    end-exec
           MOVE 'ERACCT' TO DFHEIV1
      *    MOVE '&"S        E          (  N#00004761' TO DFHEIV0
           MOVE X'262253202020202020202045' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'204E233030303034373631' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 DFHEIV20, 
                 DFHEIV99, 
                 eracct-key, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           SET ADDRESS OF account-master TO
               DFHEIV20
           MOVE EIBRESP  TO ws-response
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK
031113
031113     if resp-normal
031113        move am-dcc-product-code to ws-dcc-product-code
031113     end-if
031113
031113     .
031113 6200-exit.
031113     exit.
01360  7000-PRT-LINE.
01361      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.
01362      MOVE 'RF'                   TO  RF-RECORD-ID.
01363      MOVE '1'                    TO  RF-RECORD-TYPE.
01364      MOVE 'EL676'                TO  RF-REPORT-ID.
01365
01366      ADD +1                      TO  WS-LINE-NUMBER.
01367
01368      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.
01369
01370      
      * EXEC CICS WRITE
01371 *        DATASET  (REPT-FILE-ID)
01372 *        FROM     (REPORT-SAVE-FILE)
01373 *        RIDFLD   (RF-CONTROL-PRIMARY)
01374 *        END-EXEC.
           MOVE LENGTH OF
            REPORT-SAVE-FILE
             TO DFHEIV11
      *    MOVE '&$ L                  ''   #00004785' TO DFHEIV0
           MOVE X'2624204C2020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373835' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 REPT-FILE-ID, 
                 REPORT-SAVE-FILE, 
                 DFHEIV11, 
                 RF-CONTROL-PRIMARY, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01375
01376  7000-EXIT.
01377      EXIT.
01378
01379  8500-DATE-CONVERT.
01380      MOVE LINK-ELDATCV           TO  PGM-NAME.
01381
01382      
      * EXEC CICS LINK
01383 *        PROGRAM   (PGM-NAME)
01384 *        COMMAREA  (DATE-CONVERSION-DATA)
01385 *        LENGTH    (DC-COMM-LENGTH)
01386 *        END-EXEC.
      *    MOVE '."C                   (   #00004797' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034373937' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 PGM-NAME, 
                 DATE-CONVERSION-DATA, 
                 DC-COMM-LENGTH, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01387
01388  8500-EXIT.
01389      EXIT.
01390
01391  8800-ABEND.
01392      MOVE DFHEIBLK               TO  EMI-LINE1.
01393
01394      
      * EXEC CICS LINK
01395 *        PROGRAM   ('EL004')
01396 *        COMMAREA  (EMI-LINE1)
01397 *        LENGTH    (72)
01398 *        END-EXEC.
           MOVE 'EL004' TO DFHEIV1
           MOVE 72
             TO DFHEIV11
      *    MOVE '."C                   (   #00004809' TO DFHEIV0
           MOVE X'2E2243202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202820' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383039' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV1, 
                 EMI-LINE1, 
                 DFHEIV11, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01399
01400  9999-RETURN-CICS.
01401      
      * EXEC CICS  RETURN
01402 *        END-EXEC.
      *    MOVE '.(                    ''   #00004816' TO DFHEIV0
           MOVE X'2E2820202020202020202020' TO DFHEIV0(1:12)
           MOVE X'202020202020202020202720' TO DFHEIV0(13:12)
           MOVE X'2020233030303034383136' TO DFHEIV0(25:11)
           CALL 'kxdfhei1' USING DFHEIV0, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99, 
                 DFHEIV99
           GO TO 9999-DFHEXIT DEPENDING ON DFHEIGDK.
           
01403
01404      
      * GOBACK.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL676' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.

       9999-DFHBACK SECTION.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL676' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
       9999-DFHEXIT.
           IF DFHEIGDJ EQUAL 0001
               NEXT SENTENCE
           ELSE IF DFHEIGDJ EQUAL 2
               GO TO 8800-ABEND
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 3
               GO TO 2000-WRITE-INITIAL-TRAILER
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 4
               GO TO 2300-DELETE-REC
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 5
               GO TO 3000-READ-CONTROL
               DEPENDING ON DFHEIGDI
           ELSE IF DFHEIGDJ EQUAL 6
               GO TO 5000-PRINT-REPORT,
                     4900-ENDBR
               DEPENDING ON DFHEIGDI.
           MOVE '9%                    "   ' TO DFHEIV0
           MOVE 'EL676' TO DFHEIV1
           CALL 'kxdfhei1' USING DFHEIV0 DFHEIV1
           GOBACK.
