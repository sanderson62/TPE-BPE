00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL676
00003  PROGRAM-ID.                 EL676 .                                 LV011
00004 *              PROGRAM CONVERTED BY                                  CL*11
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*11
00006 *              CONVERSION DATE 02/14/96 07:57:17.                    CL*11
00007 *                            VMOD=2.011                              CL*11
00008 *                                                                 EL676
00008 *                                                                 EL676
00009 *AUTHOR.        LOGIC, INC.                                          CL*11
00010 *               DALLAS, TEXAS.                                       CL*11
00011                                                                   EL676
00012 *DATE-COMPILED.                                                      CL*11
00013                                                                   EL676
00014 *SECURITY.   *****************************************************   CL*11
00015 *            *                                                   *   CL*11
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*11
00017 *            *                                                   *   CL*11
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*11
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*11
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*11
00021 *            *                                                   *   CL*11
00022 *            *****************************************************   CL*11
00023                                                                   EL676
00024 *REMARKS.                                                            CL**2
00025 *        TOTAL OF ALL ACCOUNTS WITH OR WITHOUT BUSINESS -            CL**2
00026 *        REPORTING NET PREMIUMS BY LIFE, A&H, TOTAL.                 CL**2
00027 *                                                                    CL**2
00028 *        ALSO TOTALS NON-PROCESSABLE BUSINESS FOR CLIENT.            CL**2
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
00029                                                                   EL676
00030  ENVIRONMENT DIVISION.                                            EL676
00031  DATA DIVISION.                                                   EL676
00032  EJECT                                                            EL676
00033  WORKING-STORAGE SECTION.                                         EL676
00034  01  LCP-TIME-OF-DAY-XX.                                             CL*11
00035      05  LCP-TIME-OF-DAY-68        PIC 9(6).                         CL*11
00036      05  FILLER                    PIC 99.                           CL*11
00037  01  LCP-CICS-TIME                 PIC 9(15).                        CL*11
00038  77  FILLER  PIC  X(32)  VALUE '********************************'.EL676
00039  77  FILLER  PIC  X(32)  VALUE '*     EL676  WORKING STORAGE   *'.EL676
00040  77  FILLER  PIC  X(32)  VALUE '******** VMOD=2.011 ************'.   CL*11
100104 77  SUB                         PIC S999  COMP-3 VALUE +0.
031113 77  s1                          pic s999  comp-3 value +0.
100104 77  CNC-FACT                    PIC S9(3)V9(7) COMP-3 VALUE +0.
052813 77  ws-work-comm                pic s9(7)v99 comp-3 value +0.
100104 77  WS-RESPONSE                 PIC S9(8) COMP VALUE +0.
100104     88  RESP-NORMAL                    VALUE +0.
100104     88  RESP-NOTFND                    VALUE +13.
100104     88  RESP-NOTOPEN                   VALUE +19.
100104     88  RESP-ENDFILE                   VALUE +20.
00041                                                                   EL676
00042  01  FILLER  COMP.                                                EL676
00043      12  CLEN                        PIC S9(4)   VALUE +1024.     EL676
00044                                                                   EL676
00045  01  FILLER          COMP-3.                                      EL676
00046      12  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL676
00047      12  WS-RECORD-COUNT             PIC S9(9)   VALUE ZERO.      EL676
00048      12  WS-LINE-NUMBER              PIC S9(3)   VALUE +0.        EL676
00049      12  WS-PAGE-NUMBER              PIC S9(5)   VALUE +0.           CL**4

031113 01  ERACCT-key.
031113     16  acct-company-cd         PIC X     VALUE SPACES.
031113     16  acct-carrier            PIC X     VALUE SPACES.
031113     16  acct-grouping           PIC X(6)  VALUE SPACES.
031113     16  acct-state              PIC XX    VALUE SPACES.
031113     16  acct-account            PIC X(10) VALUE SPACES.
031113     16  acct-exp-date           PIC XX    VALUE SPACES.
031113     16  FILLER                  PIC X(4)  VALUE SPACES.

00051  01  ELCNTL-KEY.                                                  EL676
00052      12  CNTL-COMP-ID                PIC  X(3).                   EL676
00053      12  CNTL-REC-TYPE               PIC  X.                      EL676
00054      12  CNTL-ACCESS                 PIC  X(4).                   EL676
00055      12  CNTL-SEQ-NO                 PIC S9(4)    COMP.           EL676
00056                                                                   EL676
100104 01  WS-CARRIER-TABLE.
100104     12  FILLER OCCURS 30.
100104         16  WS-CARRIER-CODE         PIC X.
100104         16  WS-SEC-PAY-CARRIER      PIC X.
100104
00057  01  ERPNDB-KEY.                                                  EL676
00058      12  PNDB-COMPANY-CD             PIC  X.                      EL676
00059      12  PNDB-CARRIER                PIC  X.                         CL**4
00060      12  PNDB-GROUPING               PIC  X(6).                      CL**4
00061      12  PNDB-STATE                  PIC  X(2).                      CL**4
00062      12  PNDB-ACCOUNT                PIC  X(10).                     CL**4
00063      12  PNDB-CERT-EFF-DT            PIC  X(2).                      CL**4
00064      12  PNDB-CERT-NO                PIC  X(11).                     CL**4
00065      12  PNDB-ALT-CHG-SEQ-NO         PIC S9(4)   COMP.               CL**4
00066      12  PNDB-RECORD-TYPE            PIC  X.                         CL**4
00067                                                                   EL676
00068  01  FILLER.                                                      EL676
031113     12  ws-dcc-product-code         pic xxx     value spaces.
031113     12  WS-AH-CATEGORY              PIC X       VALUE ' '.
031113     12  CLAS-LOOK                   PIC XX      VALUE '  '.
00069      12  SAVE-DATE                   PIC  X(8).                   EL676
00070      12  PEND-SW                     PIC  X      VALUE '0'.       EL676
00071          88  GOOD-PEND                           VALUE '0'.       EL676
00072      12  REPT-FILE-ID                PIC  X(8)   VALUE 'ELREPT  '.EL676
00073      12  CNTL-FILE-ID                PIC  X(8)   VALUE 'ELCNTL  '.EL676
00074      12  PEND-FILE-ID                PIC  X(8)   VALUE 'ERPNDB2 '.   CL**4
00075      12  PGM-NAME                    PIC  X(8)   VALUE SPACES.    EL676
00076      12  LINK-ELDATCV                PIC  X(8)   VALUE 'ELDATCV '.EL676
00077      12  EMI-LINE1                   PIC  X(72)  VALUE SPACES.    EL676
00078      12  GOOD-STARTBR                PIC  X(3)   VALUE 'NO '.     EL676
00079      12  WS-TIME-OF-DAY.                                             CL**2
00080          16  WS-TIME.                                                CL**2
00081              20  WS-HH               PIC  99.                        CL**2
00082              20  WS-MM               PIC  99.                        CL**2
00083              20  WS-SS               PIC  99.                        CL**2
00084          16  WS-HUN-SEC              PIC  99.                        CL**2
00085      12  CARRIER-SWITCH              PIC  X      VALUE 'N'.          CL**4
00086          88  USE-CARRIER-TOTALS                  VALUE 'Y'.          CL**4
00087      12  PREVIOUS-CARRIER            PIC  X      VALUE LOW-VALUES.   CL**4
00088  EJECT                                                               CL**4
00089  01  CARRIER-XTRACT.                                                 CL**4
00090      12  CARR-AMOUNT-FIELDS  COMP-3.                                 CL**4
00091          16  CARR-XTR-GOOD-LF-AMT    PIC S9(9)V99     VALUE +0.      CL**4
00092          16  CARR-XTR-GOOD-LF-OB     PIC S9(9)V99     VALUE +0.      CL**4
00093          16  CARR-XTR-GOOD-LF-CAN    PIC S9(9)V99     VALUE +0.      CL**4
00094          16  CARR-XTR-GOOD-LF-COM    PIC S9(9)V99     VALUE +0.      CL**4
00095          16  CARR-XTR-GOOD-AH-AMT    PIC S9(9)V99     VALUE +0.      CL**4
00096          16  CARR-XTR-GOOD-AH-OB     PIC S9(9)V99     VALUE +0.      CL**4
00097          16  CARR-XTR-GOOD-AH-CAN    PIC S9(9)V99     VALUE +0.      CL**4
00098          16  CARR-XTR-GOOD-AH-COM    PIC S9(9)V99     VALUE +0.      CL**4
00099          16  CARR-XTR-BAD-LF-AMT     PIC S9(9)V99     VALUE +0.      CL**4
00100          16  CARR-XTR-BAD-LF-OB      PIC S9(9)V99     VALUE +0.      CL**4
00101          16  CARR-XTR-BAD-LF-CAN     PIC S9(9)V99     VALUE +0.      CL**4
00102          16  CARR-XTR-BAD-LF-COM     PIC S9(9)V99     VALUE +0.      CL**4
00103          16  CARR-XTR-BAD-AH-AMT     PIC S9(9)V99     VALUE +0.      CL**4
00104          16  CARR-XTR-BAD-AH-OB      PIC S9(9)V99     VALUE +0.      CL**4
00105          16  CARR-XTR-BAD-AH-CAN     PIC S9(9)V99     VALUE +0.      CL**4
00106          16  CARR-XTR-BAD-AH-COM     PIC S9(9)V99     VALUE +0.      CL**4
00107          16  CARR-XTR-GOOD-IS-CNT    PIC S9(9)        VALUE +0.      CL**4
00108          16  CARR-XTR-BAD-IS-CNT     PIC S9(9)        VALUE +0.      CL**4
00109          16  CARR-XTR-GOOD-CA-CNT    PIC S9(9)        VALUE +0.      CL**4
00110          16  CARR-XTR-BAD-CA-CNT     PIC S9(9)        VALUE +0.      CL**4
00111          16  CARR-XTR-GOOD-LF-NET    PIC S9(9)V99     VALUE +0.      CL**4
00112          16  CARR-XTR-GOOD-AH-NET    PIC S9(9)V99     VALUE +0.      CL**4
00113          16  CARR-XTR-BAD-LF-NET     PIC S9(9)V99     VALUE +0.      CL**4
00114          16  CARR-XTR-BAD-AH-NET     PIC S9(9)V99     VALUE +0.      CL**4
00115          16  CARR-XTR-GOOD-IS-TOT    PIC S9(9)V99     VALUE +0.      CL**4
00116          16  CARR-XTR-GOOD-CA-TOT    PIC S9(9)V99     VALUE +0.      CL**4
00117          16  CARR-XTR-GOOD-NET-TOT   PIC S9(9)V99     VALUE +0.      CL**4
00118          16  CARR-XTR-GOOD-CM-TOT    PIC S9(9)V99     VALUE +0.      CL**4
00119          16  CARR-XTR-BAD-IS-TOT     PIC S9(9)V99     VALUE +0.      CL**4
00120          16  CARR-XTR-BAD-CA-TOT     PIC S9(9)V99     VALUE +0.      CL**4
00121          16  CARR-XTR-BAD-NET-TOT    PIC S9(9)V99     VALUE +0.      CL**4
00122          16  CARR-XTR-BAD-CM-TOT     PIC S9(9)V99     VALUE +0.      CL**4
00123  EJECT                                                            EL676
00124  01  REPORT-XTRACT.                                               EL676
00125      12  REPT-AMOUNT-FIELDS  COMP-3.                              EL676
00126          16  REPT-XTR-GOOD-LF-AMT    PIC S9(9)V99     VALUE +0.   EL676
00127          16  REPT-XTR-GOOD-LF-OB     PIC S9(9)V99     VALUE +0.   EL676
00128          16  REPT-XTR-GOOD-LF-CAN    PIC S9(9)V99     VALUE +0.   EL676
00129          16  REPT-XTR-GOOD-LF-COM    PIC S9(9)V99     VALUE +0.   EL676
00130          16  REPT-XTR-GOOD-AH-AMT    PIC S9(9)V99     VALUE +0.   EL676
00131          16  REPT-XTR-GOOD-AH-OB     PIC S9(9)V99     VALUE +0.   EL676
00132          16  REPT-XTR-GOOD-AH-CAN    PIC S9(9)V99     VALUE +0.   EL676
00133          16  REPT-XTR-GOOD-AH-COM    PIC S9(9)V99     VALUE +0.   EL676
00134          16  REPT-XTR-BAD-LF-AMT     PIC S9(9)V99     VALUE +0.   EL676
00135          16  REPT-XTR-BAD-LF-OB      PIC S9(9)V99     VALUE +0.   EL676
00136          16  REPT-XTR-BAD-LF-CAN     PIC S9(9)V99     VALUE +0.   EL676
00137          16  REPT-XTR-BAD-LF-COM     PIC S9(9)V99     VALUE +0.   EL676
00138          16  REPT-XTR-BAD-AH-AMT     PIC S9(9)V99     VALUE +0.   EL676
00139          16  REPT-XTR-BAD-AH-OB      PIC S9(9)V99     VALUE +0.   EL676
00140          16  REPT-XTR-BAD-AH-CAN     PIC S9(9)V99     VALUE +0.   EL676
00141          16  REPT-XTR-BAD-AH-COM     PIC S9(9)V99     VALUE +0.   EL676
00142          16  REPT-XTR-GOOD-IS-CNT    PIC S9(9)        VALUE +0.   EL676
00143          16  REPT-XTR-BAD-IS-CNT     PIC S9(9)        VALUE +0.   EL676
00144          16  REPT-XTR-GOOD-CA-CNT    PIC S9(9)        VALUE +0.   EL676
00145          16  REPT-XTR-BAD-CA-CNT     PIC S9(9)        VALUE +0.   EL676
00146          16  REPT-XTR-GOOD-LF-NET    PIC S9(9)V99     VALUE +0.   EL676
00147          16  REPT-XTR-GOOD-AH-NET    PIC S9(9)V99     VALUE +0.   EL676
00148          16  REPT-XTR-BAD-LF-NET     PIC S9(9)V99     VALUE +0.   EL676
00149          16  REPT-XTR-BAD-AH-NET     PIC S9(9)V99     VALUE +0.   EL676
00150          16  REPT-XTR-GOOD-IS-TOT    PIC S9(9)V99     VALUE +0.   EL676
00151          16  REPT-XTR-GOOD-CA-TOT    PIC S9(9)V99     VALUE +0.   EL676
00152          16  REPT-XTR-GOOD-NET-TOT   PIC S9(9)V99     VALUE +0.   EL676
00153          16  REPT-XTR-GOOD-CM-TOT    PIC S9(9)V99     VALUE +0.   EL676
00154          16  REPT-XTR-BAD-IS-TOT     PIC S9(9)V99     VALUE +0.   EL676
00155          16  REPT-XTR-BAD-CA-TOT     PIC S9(9)V99     VALUE +0.   EL676
00156          16  REPT-XTR-BAD-NET-TOT    PIC S9(9)V99     VALUE +0.   EL676
00157          16  REPT-XTR-BAD-CM-TOT     PIC S9(9)V99     VALUE +0.   EL676
00158  EJECT                                                            EL676
00159  01  WS-HEADING1.                                                 EL676
00160      12  FILLER                  PIC  X(24)  VALUE SPACES.        EL676
00161      12  WS-H1-TITLE             PIC  X(18)  VALUE                EL676
00162              'NET PREMIUM TOTALS'.                                EL676
00163      12  FILLER                  PIC  X(22)  VALUE SPACES.        EL676
00164      12  WS-H1-REPORT-NUMBER     PIC  X(7)   VALUE 'EL -676'.     EL676
00165      12  FILLER                  PIC  X(61)  VALUE SPACES.        EL676
00166                                                                   EL676
00167  01  WS-HEADING2.                                                 EL676
00168      12  FILLER                  PIC  X(24)  VALUE SPACES.        EL676
00169      12  WS-H2-CLIENT-NAME       PIC  X(30)  VALUE SPACES.        EL676
00170      12  FILLER                  PIC  X(12)  VALUE SPACES.        EL676
00171      12  WS-H2-DATE              PIC  X(8)   VALUE SPACES.        EL676
00172      12  FILLER                  PIC  X(58)  VALUE SPACES.        EL676
00173                                                                   EL676
00174  01  WS-HEADING3.                                                 EL676
00175      12  FILLER                  PIC  X(24)  VALUE SPACES.        EL676
00176      12  WS-H3-DATE              PIC  X(18)  VALUE SPACES.        EL676
00177      12  FILLER                  PIC  X(13)  VALUE SPACES.        EL676
00178      12  FILLER                  PIC  X(5)   VALUE 'PAGE'.        EL676
00179      12  WS-H3-PAGE              PIC ZZZZZ9  VALUE ZERO.             CL**4
00180      12  FILLER                  PIC  X(66)  VALUE SPACES.        EL676
00181                                                                   EL676
00182  01  WS-HEADING4.                                                 EL676
00183      12  FILLER                  PIC  X(5)   VALUE SPACES.           CL**4
00184      12  FILLER                  PIC  X(10)  VALUE                   CL**4
00185              'CARRIER - '.                                           CL**4
00186      12  WS-H4-CARRIER           PIC  X      VALUE SPACES.           CL**4
00187      12  FILLER                  PIC  X(116) VALUE SPACES.           CL**4
00188                                                                      CL**4
00189  01  WS-HEADING5.                                                    CL**4
00190      12  FILLER                  PIC  X(22)  VALUE SPACES.        EL676
00191      12  FILLER                  PIC  X(42)  VALUE                EL676
00192              'PROCESSABLE    NON PROCESSABLE       TOTAL'.        EL676
00193      12  FILLER                  PIC  X(68)  VALUE SPACES.        EL676
00194                                                                   EL676
00195  01  WS-DETAIL1.                                                  EL676
00196      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00197      12  FILLER                  PIC  X(7)   VALUE 'ISSUES '.     EL676
00198      12  WS-DET1-LF-HDG          PIC  X(6)   VALUE SPACES.        EL676
00199      12  FILLER                  PIC  X(5)   VALUE ' OB  '.       EL676
00200      12  WS-IS-LF-OB-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00201      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00202      12  WS-IS-LF-OB-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00203      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00204      12  WS-IS-LF-OB-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00205      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00206                                                                   EL676
00207  01  WS-DETAIL2.                                                  EL676
00208      12  FILLER                  PIC  X(10)  VALUE SPACES.        EL676
00209      12  WS-DET2-LF-HDG          PIC  X(6)   VALUE SPACES.        EL676
00210      12  FILLER                  PIC  X(5)   VALUE ' SP  '.       EL676
00211      12  WS-IS-LF-SP-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00212      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00213      12  WS-IS-LF-SP-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00214      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00215      12  WS-IS-LF-SP-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00216      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00217                                                                   EL676
00218  01  WS-DETAIL3.                                                  EL676
00219      12  FILLER                  PIC  X(10)  VALUE SPACES.        EL676
00220      12  WS-DET3-AH-HDG          PIC  X(6)   VALUE SPACES.        EL676
00221      12  FILLER                  PIC  X(5)   VALUE ' OB  '.       EL676
00222      12  WS-IS-AH-OB-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00223      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00224      12  WS-IS-AH-OB-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00225      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00226      12  WS-IS-AH-OB-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00227      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00228                                                                   EL676
00229  01  WS-DETAIL4.                                                  EL676
00230      12  FILLER                  PIC  X(10)  VALUE SPACES.        EL676
00231      12  WS-DET4-AH-HDG          PIC  X(6)   VALUE SPACES.        EL676
00232      12  FILLER                  PIC  X(5)   VALUE ' SP  '.       EL676
00233      12  WS-IS-AH-SP-GOOD        PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00234      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00235      12  WS-IS-AH-SP-BAD         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00236      12  FILLER                  PIC  X(3)    VALUE SPACES.       EL676
00237      12  WS-IS-AH-SP-TOT         PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00238      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00239                                                                   EL676
00240  01  WS-DETAIL5.                                                  EL676
00241      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00242      12  FILLER                  PIC  X(10)  VALUE 'CANCELS   '.  EL676
00243      12  WS-DET5-LF-HDG          PIC  X(6)   VALUE SPACES.        EL676
00244      12  WS-CA-LF-GOOD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00245      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00246      12  WS-CA-LF-BAD            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00247      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00248      12  WS-CA-LF-TOT            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00249      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00250                                                                   EL676
00251  01  WS-DETAIL6.                                                  EL676
00252      12  FILLER                  PIC  X(14)  VALUE SPACES.           CL*11
00253      12  FILLER                  PIC  X      VALUE SPACES.        EL676
00254      12  WS-DET6-AH-HDG          PIC  X(6)   VALUE SPACES.        EL676
00255      12  WS-CA-AH-GOOD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00256      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00257      12  WS-CA-AH-BAD            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00258      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00259      12  WS-CA-AH-TOT            PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00260      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00261                                                                   EL676
00262  01  WS-DETAIL7.                                                  EL676
00263      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00264      12  FILLER                  PIC  X(10)  VALUE 'NET       '.  EL676
00265      12  WS-DET7-LF-HDG          PIC  X(6)   VALUE SPACES.        EL676
00266      12  WS-NET-LF-GOOD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00267      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00268      12  WS-NET-LF-BAD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00269      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00270      12  WS-NET-LF-TOT           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00271      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00272                                                                   EL676
00273  01  WS-DETAIL8.                                                  EL676
00274      12  FILLER                  PIC  X(14)  VALUE SPACES.           CL*11
00275      12  FILLER                  PIC  X      VALUE SPACES.        EL676
00276      12  WS-DET8-AH-HDG          PIC  X(6)   VALUE SPACES.        EL676
00277      12  WS-NET-AH-GOOD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00278      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00279      12  WS-NET-AH-BAD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00280      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00281      12  WS-NET-AH-TOT           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00282      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00283                                                                   EL676
00284  01  WS-DETAIL9.                                                  EL676
00285      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00286      12  FILLER                  PIC  X(10)  VALUE 'COMM.     '.  EL676
00287      12  WS-DET9-LF-HDG          PIC  X(6)   VALUE SPACES.        EL676
00288      12  WS-CM-LF-GOOD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00289      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00290      12  WS-CM-LF-BAD            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00291      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00292      12  WS-CM-LF-TOT            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00293      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00294                                                                   EL676
00295  01  WS-DETAILA.                                                  EL676
00296      12  FILLER                  PIC  X(15)  VALUE SPACES.        EL676
00297      12  WS-DETA-AH-HDG          PIC  X(6)   VALUE SPACES.        EL676
00298      12  WS-CM-AH-GOOD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00299      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00300      12  WS-CM-AH-BAD            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00301      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00302      12  WS-CM-AH-TOT            PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00303      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00304                                                                   EL676
00305  01  WS-TOTAL1.                                                   EL676
00306      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00307      12  FILLER                  PIC  X(11)  VALUE 'TOTAL   ISS'. EL676
00308      12  FILLER                  PIC  X(5)   VALUE 'UES  '.       EL676
00309      12  WS-IS-TOT-GOOD          PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.     CL**3
00310      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00311      12  WS-IS-TOT-BAD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.     CL**3
00312      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00313      12  WS-IS-TOT-TOT           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.     CL**3
00314      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00315                                                                   EL676
00316  01  WS-TOTAL2.                                                   EL676
00317      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00318      12  FILLER                  PIC  X(11)  VALUE '       CANC'. EL676
00319      12  FILLER                  PIC  X(5)   VALUE 'ELS  '.       EL676
00320      12  WS-CA-TOT-GOOD          PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00321      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00322      12  WS-CA-TOT-BAD           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00323      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00324      12  WS-CA-TOT-TOT           PIC ZZ,ZZZ,ZZ9.99- VALUE ZEROS.  EL676
00325      12  FILLER                  PIC  X(63)  VALUE SPACES.        EL676
00326                                                                   EL676
00327  01  WS-TOTAL3.                                                   EL676
00328      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00329      12  FILLER                  PIC  X(11)  VALUE SPACES.        EL676
00330      12  FILLER                  PIC  X(5)   VALUE 'NET  '.       EL676
00331      12  WS-NET-TOT-GOOD         PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00332      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00333      12  WS-NET-TOT-BAD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00334      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00335      12  WS-NET-TOT-TOT          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00336      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00337                                                                   EL676
00338  01  WS-TOTAL4.                                                   EL676
00339      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00340      12  FILLER                  PIC  X(11)  VALUE '          C'. EL676
00341      12  FILLER                  PIC  X(5)   VALUE 'OMM  '.       EL676
00342      12  WS-CM-TOT-GOOD          PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00343      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00344      12  WS-CM-TOT-BAD           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00345      12  FILLER                  PIC  X(3)   VALUE SPACES.           CL**3
00346      12  WS-CM-TOT-TOT           PIC ZZ,ZZZ,ZZ9.99-  VALUE ZEROS.    CL**3
00347      12  FILLER                  PIC  X(63)  VALUE SPACES.           CL**3
00348                                                                   EL676
00349  01  WS-TOTAL5.                                                   EL676
00350      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00351      12  FILLER                  PIC  X(11)  VALUE 'COUNT   ISS'. EL676
00352      12  FILLER                  PIC  X(5)   VALUE 'UES  '.       EL676
00353      12  WS-IS-CNT-GOOD          PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.  EL676
00354      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00355      12  WS-IS-CNT-BAD           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.  EL676
00356      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00357      12  WS-IS-CNT-TOT           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.  EL676
00358      12  FILLER                  PIC  X(67)  VALUE SPACES.        EL676
00359                                                                   EL676
00360  01  WS-TOTAL6.                                                   EL676
00361      12  FILLER                  PIC  X(5)   VALUE SPACES.        EL676
00362      12  FILLER                  PIC  X(11)  VALUE '       CANC'. EL676
00363      12  FILLER                  PIC  X(5)   VALUE 'ELS  '.       EL676
00364      12  WS-CA-CNT-GOOD          PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.  EL676
00365      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00366      12  WS-CA-CNT-BAD           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.  EL676
00367      12  FILLER                  PIC  X(3)   VALUE SPACES.        EL676
00368      12  WS-CA-CNT-TOT           PIC Z,ZZZ,ZZZ,Z99- VALUE ZEROS.  EL676
00369      12  FILLER                  PIC  X(67)  VALUE SPACES.        EL676
00370  EJECT                                                            EL676
00371                                      COPY ELCREPT.                   CL**7
00372  EJECT                                                            EL676
00373                                      COPY ELCDATE.                   CL**7
00374  EJECT                                                            EL676
00375                                      COPY ELCINTF.                   CL**7
00376      12  PI-WORK-AREA  REDEFINES  PI-PROGRAM-WORK-AREA.           EL676
00377          16  PI-FROM-DT              PIC  XX.                     EL676
00378          16  PI-THRU-DT              PIC  XX.                     EL676
00379          16  PI-EL676-STOP-DT        PIC  XX.                     EL676
00380          16  FILLER                  PIC  X.                         CL*10
00381          16  PI-EL676-ENTER-DT       PIC  XX.                        CL*10
00382          16  FILLER                  PIC  X(631).                    CL*11
00383  EJECT                                                            EL676
00384  LINKAGE SECTION.                                                 EL676
00385  01  DFHCOMMAREA                 PIC  X(1024).                    EL676
00386                                                                   EL676
00387 *01 PARMLIST .                                                       CL*11
00388 *    12  FILLER                  PIC S9(8)    COMP.                  CL*11
00389 *    12  ERPNDB-POINTER          PIC S9(8)    COMP.                  CL*11
00390 *    12  ELCNTL-POINTER          PIC S9(8)    COMP.                  CL*11

00392                                  COPY ERCPNDB.                       CL**7
00394                                  COPY ELCCNTL.
031113                                 copy ERCACCT.

00396  PROCEDURE DIVISION.                                              EL676
00397                                                                   EL676
pemuni*      exec cics retrieve
pemuni*       into(program-interface-block)
pemuni*       length(pi-comm-length)
pemuni*    end-exec
pemuni
00398      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL676
00399      MOVE '5'                    TO  DC-OPTION-CODE.              EL676
00400                                                                   EL676
00401      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL676
00402                                                                   EL676
00403      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE                    EL676
00404                                      WS-H2-DATE.                  EL676
pemuni*    MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL676
00406                                                                   EL676
00407  1000-START-PROGRAM.                                              EL676
00408      EXEC CICS  HANDLE CONDITION                                  EL676
00409          ERROR  (8800-ABEND)                                      EL676
00410          END-EXEC.                                                EL676
00411                                                                   EL676
00412      EXEC CICS  RETRIEVE                                          EL676
00413          INTO    (PROGRAM-INTERFACE-BLOCK)                        EL676
00414          LENGTH  (CLEN)                                           EL676
00415          END-EXEC.                                                EL676
00416                                                                   EL676
00417      IF PI-EL676-STOP-DT  = LOW-VALUES AND                           CL*10
00418         PI-EL676-ENTER-DT = LOW-VALUES                               CL*10
00419          MOVE PI-CR-MONTH-END-DT  TO  PI-EL676-STOP-DT.           EL676
00420                                                                   EL676
00421      IF PI-EL676-ENTER-DT NOT = LOW-VALUES                           CL*10
00422         MOVE PI-EL676-ENTER-DT       TO  DC-BIN-DATE-1               CL*10
00423      ELSE                                                            CL*10
00424         MOVE PI-EL676-STOP-DT        TO  DC-BIN-DATE-1.              CL*10
00425                                                                      CL*10
00426      MOVE ' '                        TO  DC-OPTION-CODE.             CL*10
00427                                                                   EL676
00428      PERFORM 8500-DATE-CONVERT  THRU  8500-EXIT.                  EL676
00429                                                                   EL676
00430      MOVE DC-GREG-DATE-1-ALPHA   TO  WS-H3-DATE.                  EL676
00431                                                                   EL676
00432  2000-CHECK-IN-PROGRESS.                                          EL676
00433      EXEC CICS  HANDLE CONDITION                                  EL676
00434          NOTFND  (2000-WRITE-INITIAL-TRAILER)                     EL676
00435          END-EXEC.                                                EL676
00436                                                                   EL676
00437      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
00438      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
00439      MOVE '2'                    TO  RF-RECORD-TYPE.              EL676
00440      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
00441      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL676
00442                                                                   EL676
00443      EXEC CICS  READ                                              EL676
00444          DATASET  (REPT-FILE-ID)                                  EL676
00445          INTO     (REPORT-SAVE-FILE)                              EL676
00446          RIDFLD   (RF-CONTROL-PRIMARY)                            EL676
00447          END-EXEC.                                                EL676
00448                                                                   EL676
00449      GO TO 9999-RETURN-CICS.                                      EL676
00450                                                                   EL676
00451  2000-WRITE-INITIAL-TRAILER.                                      EL676
00452      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
00453      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
00454      MOVE '2'                    TO  RF-RECORD-TYPE.              EL676
00455      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
00456      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL676
00457      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL676
00458                                                                      CL**2
00459      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*11
00460      END-EXEC                                                        CL*11
00461      EXEC CICS FORMATTIME                                            CL*11
00462                ABSTIME(LCP-CICS-TIME)                                CL*11
00463                TIME(LCP-TIME-OF-DAY-XX)                              CL*11
00464      END-EXEC                                                        CL*11
00465      MOVE LCP-TIME-OF-DAY-68     TO  RF-PRINT-HH-MM-SS.              CL*11
00466      MOVE 'STARTED'              TO  RF-CURRENT-DATE.             EL676
00467                                                                   EL676
00468      EXEC CICS  WRITE                                             EL676
00469          DATASET  (REPT-FILE-ID)                                  EL676
00470          FROM     (REPORT-SAVE-FILE)                              EL676
00471          RIDFLD   (RF-CONTROL-PRIMARY)                            EL676
00472          END-EXEC.                                                EL676
00473                                                                   EL676
00474  2100-DELETE-REC.                                                 EL676
00475      MOVE 1                      TO  RF-LINE-NUMBER.              EL676
00476                                                                   EL676
00477      EXEC CICS  HANDLE CONDITION                                  EL676
00478          NOTFND  (2300-DELETE-REC)                                EL676
00479          END-EXEC.                                                EL676
00480                                                                   EL676
00481  2200-DELETE-1.                                                   EL676
00482      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
00483      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
00484      MOVE '1'                    TO  RF-RECORD-TYPE.              EL676
00485      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
00486                                                                   EL676
00487      EXEC CICS  DELETE                                            EL676
00488          DATASET    (REPT-FILE-ID)                                EL676
00489          RIDFLD     (RF-CONTROL-PRIMARY)                          EL676
00490          KEYLENGTH  (11)                                          EL676
00491          END-EXEC.                                                EL676
00492                                                                   EL676
00493      ADD 1                       TO  RF-LINE-NUMBER.              EL676
00494                                                                   EL676
00495      GO TO 2200-DELETE-1.                                         EL676
00496                                                                   EL676
00497  2300-DELETE-REC.                                                 EL676
00498      EXEC CICS  HANDLE CONDITION                                  EL676
00499          NOTFND  (3000-READ-CONTROL)                              EL676
00500          END-EXEC.                                                EL676
00501                                                                   EL676
00502  2400-DELETE-2.                                                   EL676
00503      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
00504      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
00505      MOVE '2'                    TO  RF-RECORD-TYPE.              EL676
00506      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
00507                                                                   EL676
00508      EXEC CICS  DELETE                                            EL676
00509          DATASET    (REPT-FILE-ID)                                EL676
00510          RIDFLD     (RF-CONTROL-PRIMARY)                          EL676
00511          KEYLENGTH  (11)                                          EL676
00512          END-EXEC.                                                EL676
00513                                                                   EL676
00514      ADD 1                       TO  RF-LINE-NUMBER.              EL676
00515                                                                   EL676
00516      GO TO 2400-DELETE-2.                                         EL676
00517                                                                   EL676
00518  3000-READ-CONTROL.                                               EL676
00519      MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID.                EL676
00520      MOVE SPACES                 TO  CNTL-ACCESS.                 EL676
00521      MOVE '1'                    TO  CNTL-REC-TYPE.               EL676
00522      MOVE +0                     TO  CNTL-SEQ-NO.                 EL676
00523                                                                   EL676
00524      EXEC CICS  READ                                              EL676
00525          DATASET  (CNTL-FILE-ID)                                  EL676
00526          SET      (ADDRESS OF CONTROL-FILE)                          CL*11
00527          RIDFLD   (ELCNTL-KEY)                                    EL676
00528          END-EXEC.                                                EL676
00529                                                                   EL676
00530      MOVE CF-CL-MAIL-TO-NAME     TO  WS-H2-CLIENT-NAME.           EL676
00531                                                                      CL**4
00532      IF CF-CARR-GROUP-ST-ACCNT-CNTL                                  CL**4
00533        OR CF-CARR-ST-ACCNT-CNTL                                      CL**4
00534        OR CF-CARR-ACCNT-CNTL                                         CL**4
00535          MOVE 'Y'                TO  CARRIER-SWITCH.                 CL**4

       3005-START-CARRIER.
           
100104     MOVE PI-COMPANY-ID          TO  CNTL-COMP-ID
100104     MOVE LOW-VALUES             TO  CNTL-ACCESS
100104     MOVE '6'                    TO  CNTL-REC-TYPE
100104     MOVE +0                     TO  CNTL-SEQ-NO

           EXEC CICS  STARTBR
               DATASET  (CNTL-FILE-ID)
               RIDFLD   (ELCNTL-KEY)
               RESP     (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              GO TO 3100-START-BROWSE
           END-IF

           PERFORM VARYING SUB FROM +1 BY +1 UNTIL
              (SUB > +30)
              EXEC CICS  READNEXT
                   DATASET (CNTL-FILE-ID)
                   SET     (ADDRESS OF CONTROL-FILE)
                   RIDFLD  (ELCNTL-KEY)
                   RESP    (WS-RESPONSE)
              END-EXEC
          
              IF (RESP-NORMAL)
                 AND (CF-COMPANY-ID = PI-COMPANY-ID)
                 AND (CF-RECORD-TYPE = '6')
                 MOVE CF-CARRIER-CNTL  TO WS-CARRIER-CODE (SUB)
                 MOVE CF-SECPAY-SWITCH TO WS-SEC-PAY-CARRIER (SUB)
              ELSE
                 MOVE +31              TO SUB
              END-IF
           END-PERFORM

031113     exec cics endbr
031113        dataset ('ELCNTL')
031113     end-exec

           .
00537  3100-START-BROWSE.                                                  CL**4
00538      MOVE LOW-VALUES             TO  ERPNDB-KEY                   EL676
00539      MOVE PI-COMPANY-CD          TO  PNDB-COMPANY-CD              EL676
00540                                                                   EL676
00541      EXEC CICS  HANDLE CONDITION                                  EL676
00542          NOTFND   (5000-PRINT-REPORT)                             EL676
00543          ENDFILE  (4900-ENDBR)                                       CL**4
00544          END-EXEC.                                                EL676
00545                                                                   EL676
00546      EXEC CICS  STARTBR                                           EL676
00547          DATASET  (PEND-FILE-ID)                                  EL676
00548          RIDFLD   (ERPNDB-KEY)                                    EL676
00549          END-EXEC.                                                EL676
00550                                                                   EL676
00551      MOVE 'YES'                  TO  GOOD-STARTBR.                EL676
00552                                                                   EL676
00553  4000-READNEXT.                                                   EL676
00554      EXEC CICS  READNEXT                                          EL676
00555          DATASET  (PEND-FILE-ID)                                  EL676
00556          SET      (ADDRESS OF PENDING-BUSINESS)                      CL*11
00557          RIDFLD   (ERPNDB-KEY)                                    EL676
00558          END-EXEC.                                                EL676
00559                                                                   EL676
00560      IF PB-COMPANY-CD  NOT = PI-COMPANY-CD                        EL676
00561          GO TO 4900-ENDBR.                                           CL**4
00562                                                                   EL676
00563      IF PB-ISSUE                                                  EL676
00564        OR  PB-CANCELLATION                                        EL676
00565          NEXT SENTENCE                                            EL676
00566      ELSE                                                         EL676
00567          GO TO 4000-READNEXT.                                     EL676
00568                                                                   EL676
00569      IF PB-ALT-CHG-SEQ-NO  NOT =  ZEROS                           EL676
00570          GO TO 4000-READNEXT.                                     EL676
00571                                                                   EL676
00572      IF PI-EL676-ENTER-DT NOT = LOW-VALUES                           CL*10
00573         IF PI-EL676-ENTER-DT = PB-INPUT-DT                           CL*10
00574            GO TO 4010-CONT                                           CL*10
00575         ELSE                                                         CL*10
00576            GO TO 4000-READNEXT.                                      CL*10
00577                                                                      CL*10
00578      IF PB-CREDIT-ACCEPT-DT  =  PI-EL676-STOP-DT                     CL*10
00579        OR  PB-CREDIT-ACCEPT-DT  = LOW-VALUES                         CL*10
00580          NEXT SENTENCE                                               CL*10
00581      ELSE                                                            CL*10
00582          GO TO 4000-READNEXT.                                        CL*10
00583                                                                      CL*10
00584      IF PB-CREDIT-SELECT-DT  IS GREATER THAN  PI-EL676-STOP-DT       CL*10
00585          GO TO 4000-READNEXT.                                        CL*10
00586      GO TO 4010-CONT.                                                CL*10
00587                                                                      CL*10
00588  4010-CONT.                                                          CL*10
00589      IF PB-CARRIER  IS NOT EQUAL TO  PREVIOUS-CARRIER                CL**4
00590          PERFORM 4300-PRINT-CARRIER  THRU  4399-EXIT.                CL**4
00591                                                                      CL**4
00592      IF PB-RECORD-ON-HOLD                                         EL676
00593        OR  PB-RECORD-RETURNED                                     EL676
00594        OR  PB-FATAL-ERRORS                                        EL676
00595        OR  PB-UNFORCED-ERRORS                                     EL676
00596           MOVE '1'               TO  PEND-SW                         CL**8
00597      ELSE                                                            CL**8
00598           MOVE '0'               TO  PEND-SW.                        CL**8
00599                                                                      CL**6
00600      IF PB-ISSUE                                                     CL**6
00601          IF PB-REIN-ONLY-CERT                                        CL**6
00602            OR  PB-REISSUED-CERT                                      CL**6
122002           OR  PB-MONTHLY-CERT
00603            OR  PB-POLICY-IS-DECLINED                                 CL**9
00604            OR  PB-POLICY-IS-VOIDED                                   CL**9
00605              IF GOOD-PEND                                            CL**6
00606                  ADD +1              TO  CARR-XTR-GOOD-IS-CNT        CL**6
00607                  GO TO 4000-READNEXT                                 CL**6
00608              ELSE                                                    CL**6
00609                  ADD +1              TO  CARR-XTR-BAD-IS-CNT         CL**6
00610                  GO TO 4000-READNEXT.                                CL**6
00611                                                                      CL**6
00612      IF PB-CANCELLATION                                              CL**6
00613          IF PB-CI-ENTRY-STATUS  = '9'                                CL**6
00614              IF GOOD-PEND                                            CL**6
00615                  ADD +1              TO  CARR-XTR-GOOD-CA-CNT        CL**6
00616                  GO TO 4000-READNEXT                                 CL**6
00617              ELSE                                                    CL**6
00618                  ADD +1              TO  CARR-XTR-BAD-CA-CNT         CL**6
00619                  GO TO 4000-READNEXT.                                CL**6
00620                                                                   EL676
00621      IF PB-OVERRIDE-LIFE                                          EL676
00622        OR  PB-OVERRIDE-BOTH                                       EL676
00623          IF PB-ISSUE                                              EL676
00624              MOVE PB-I-LF-PREM-CALC  TO  PB-I-LF-PREMIUM-AMT      EL676
00625              MOVE PB-I-LF-ALT-PREM-CALC                              CL**2
00626                                      TO  PB-I-LF-ALT-PREMIUM-AMT     CL**2
00627          ELSE                                                     EL676
00628              MOVE PB-C-LF-REF-CALC   TO  PB-C-LF-CANCEL-AMT.      EL676
00629                                                                   EL676
00630      IF PB-OVERRIDE-AH                                            EL676
00631        OR  PB-OVERRIDE-BOTH                                       EL676
00632          IF PB-ISSUE                                              EL676
00633              MOVE PB-I-AH-PREM-CALC  TO  PB-I-AH-PREMIUM-AMT      EL676
00634          ELSE                                                     EL676
00635              MOVE PB-C-AH-REF-CALC   TO  PB-C-AH-CANCEL-AMT.      EL676
00636                                                                   EL676
00637      IF PB-ISSUE                                                  EL676
00638          IF GOOD-PEND                                             EL676
00639              ADD +1              TO  CARR-XTR-GOOD-IS-CNT            CL**4
00640              IF PB-I-OB                                           EL676
00641                  COMPUTE CARR-XTR-GOOD-LF-OB =                       CL**4
00642                      (CARR-XTR-GOOD-LF-OB + PB-I-LF-PREMIUM-AMT      CL**4
00643                      + PB-I-LF-ALT-PREMIUM-AMT)                      CL**2
00644                  COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =              CL**4
00645                      (CARR-XTR-GOOD-LF-COM + (PB-I-LF-PREMIUM-AMT    CL**4
00646                      * PB-I-LIFE-COMMISSION))                     EL676
00647                  COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =              CL**4
00648                      (CARR-XTR-GOOD-LF-COM +                         CL**4
00649                      (PB-I-LF-ALT-PREMIUM-AMT *                      CL**2
00650                       PB-I-LIFE-COMMISSION))                         CL**2
00651                  COMPUTE CARR-XTR-GOOD-AH-OB =                       CL**4
00652                      (CARR-XTR-GOOD-AH-OB + PB-I-AH-PREMIUM-AMT)     CL**4
00653                  COMPUTE CARR-XTR-GOOD-AH-COM ROUNDED =              CL**4
00654                      (CARR-XTR-GOOD-AH-COM + (PB-I-AH-PREMIUM-AMT    CL**4
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
00674                                                                   EL676
00675      IF PB-CANCELLATION                                           EL676
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
00676          IF GOOD-PEND                                             EL676
00677              ADD +1              TO  CARR-XTR-GOOD-CA-CNT            CL**4
00678              COMPUTE CARR-XTR-GOOD-LF-CAN =                          CL**4
00679                  (CARR-XTR-GOOD-LF-CAN + PB-C-LF-CANCEL-AMT)         CL**4
052813             compute ws-work-comm rounded =
052813                (pb-c-lf-cancel-amt * pb-ci-life-commission) * -1
052813             compute carr-xtr-good-lf-com rounded = 
052813                carr-xtr-good-lf-com + ws-work-comm
052813*            COMPUTE CARR-XTR-GOOD-LF-COM ROUNDED =
052813*                CARR-XTR-GOOD-LF-COM + ((PB-C-LF-CANCEL-AMT
052813*                * PB-CI-LIFE-COMMISSION) * -1)              
00683              COMPUTE CARR-XTR-GOOD-AH-CAN =                          CL**4
00684                  (CARR-XTR-GOOD-AH-CAN + PB-C-AH-CANCEL-AMT)         CL**4
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
00688          ELSE                                                     EL676
00689              PERFORM 4200-CHECK-C-NUMERIC  THRU  4299-XIT.        EL676
00690                                                                   EL676
00691      MOVE '0'                    TO  PEND-SW.                     EL676
00692                                                                   EL676
00693      GO TO 4000-READNEXT.                                         EL676
00694                                                                   EL676
00695  4100-CHECK-I-NUMERIC.                                            EL676
00696      ADD +1                      TO  CARR-XTR-BAD-IS-CNT.            CL**4
00697                                                                   EL676
00698      IF PB-I-LF-PREMIUM-AMT  IS NUMERIC                           EL676
00699        AND  PB-I-LF-ALT-PREMIUM-AMT  IS NUMERIC                      CL**2
00700        AND  PB-I-LIFE-COMMISSION  IS NUMERIC                      EL676
00701          IF PB-I-OB                                               EL676
00702              COMPUTE CARR-XTR-BAD-LF-OB =                            CL**4
00703                  (CARR-XTR-BAD-LF-OB + PB-I-LF-PREMIUM-AMT           CL**4
00704                  + PB-I-LF-ALT-PREMIUM-AMT)                          CL**2
00705              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =                   CL**4
00706                  CARR-XTR-BAD-LF-COM + (PB-I-LF-PREMIUM-AMT          CL**4
00707                  * PB-I-LIFE-COMMISSION)                             CL**2
00708              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =                   CL**4
00709                  CARR-XTR-BAD-LF-COM + (PB-I-LF-ALT-PREMIUM-AMT      CL**4
00710                  * PB-I-LIFE-COMMISSION)                          EL676
00711          ELSE                                                     EL676
100104            IF PB-I-LF-BENEFIT-CD NOT = SPACES AND ZEROS and 'DD'
00712              COMPUTE CARR-XTR-BAD-LF-AMT =                           CL**4
00713                  (CARR-XTR-BAD-LF-AMT + PB-I-LF-PREMIUM-AMT          CL**4
00714                  + PB-I-LF-ALT-PREMIUM-AMT)                          CL**2
00715              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =                   CL**4
00716                  CARR-XTR-BAD-LF-COM + (PB-I-LF-PREMIUM-AMT          CL**4
00717                  * PB-I-LIFE-COMMISSION)                             CL**2
00718              COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =                   CL**4
00719                  CARR-XTR-BAD-LF-COM + (PB-I-LF-ALT-PREMIUM-AMT      CL**4
00720                  * PB-I-LIFE-COMMISSION).                         EL676
00721                                                                   EL676
00722      IF PB-I-AH-PREMIUM-AMT  IS NUMERIC                           EL676
00723        AND  PB-I-AH-COMMISSION  IS NUMERIC                        EL676
00724          IF PB-I-OB                                               EL676
00725              COMPUTE CARR-XTR-BAD-AH-OB =                            CL**4
00726                  (CARR-XTR-BAD-AH-OB + PB-I-AH-PREMIUM-AMT)          CL**4
00727              COMPUTE CARR-XTR-BAD-AH-COM ROUNDED =                   CL**4
00728                  CARR-XTR-BAD-AH-COM + (PB-I-AH-PREMIUM-AMT          CL**4
00729                  * PB-I-AH-COMMISSION)                            EL676
00730          ELSE                                                     EL676
00731              COMPUTE CARR-XTR-BAD-AH-AMT =                           CL**4
00732                  (CARR-XTR-BAD-AH-AMT + PB-I-AH-PREMIUM-AMT)         CL**4
100104             IF WS-SEC-PAY-CARRIER (SUB) = 'Y'
                      COMPUTE CARR-XTR-BAD-AH-AMT =
                       CARR-XTR-BAD-AH-AMT +
                       (PB-I-AH-PREMIUM-AMT - PB-I-LF-ALT-PREMIUM-AMT
                       - PB-I-ADDL-CLP)
100104             ELSE
00733                 COMPUTE CARR-XTR-BAD-AH-COM ROUNDED =
00734                  CARR-XTR-BAD-AH-COM + (PB-I-AH-PREMIUM-AMT          CL**4
00735                  * PB-I-AH-COMMISSION).                           EL676
00736                                                                   EL676
00737  4199-XIT.                                                        EL676
00738      EXIT.                                                        EL676
00739                                                                   EL676
00740  4200-CHECK-C-NUMERIC.                                            EL676
00741      ADD +1                      TO  CARR-XTR-BAD-CA-CNT.            CL**4
00742                                                                   EL676
00743      IF PB-C-LF-CANCEL-AMT  IS NUMERIC                            EL676
00744        AND  PB-CI-LIFE-COMMISSION  IS NUMERIC                     EL676
00745          COMPUTE CARR-XTR-BAD-LF-CAN =                               CL**4
00746              (CARR-XTR-BAD-LF-CAN + PB-C-LF-CANCEL-AMT)              CL**4
00747          COMPUTE CARR-XTR-BAD-LF-COM ROUNDED =                       CL**4
00748              CARR-XTR-BAD-LF-COM + ((PB-C-LF-CANCEL-AMT              CL**4
00749              * PB-CI-LIFE-COMMISSION) * -1).                      EL676
00750                                                                   EL676
00751      IF PB-C-AH-CANCEL-AMT  IS NUMERIC                            EL676
00752         AND PB-CI-AH-COMMISSION  IS NUMERIC                       EL676
00753         COMPUTE CARR-XTR-BAD-AH-CAN =                                CL**4
00754              (CARR-XTR-BAD-AH-CAN + PB-C-AH-CANCEL-AMT)              CL**4
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
00759  4299-XIT.                                                        EL676
00760      EXIT.                                                        EL676
00761  EJECT                                                               CL**4
00762  4300-PRINT-CARRIER.                                                 CL**4

100104     PERFORM VARYING SUB FROM +1 BY +1 UNTIL
              (SUB > +31)
              OR (PB-CARRIER = WS-CARRIER-CODE (SUB))
           END-PERFORM

00763      IF USE-CARRIER-TOTALS                                           CL**4
00764          NEXT SENTENCE                                               CL**4
00765      ELSE                                                            CL**4
00766          GO TO 4320-ROLL-TOTALS.                                     CL**4
00767                                                                   EL676
00768      IF PREVIOUS-CARRIER  IS EQUAL TO  LOW-VALUES                    CL**4
00769          GO TO 4330-SET-UP-CONTROL.                                  CL**4
00770                                                                      CL**4
00771  4310-PRINT-REPORT.                                                  CL**4
00772      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-DET1-LF-HDG                  CL**4
00773                                      WS-DET2-LF-HDG                  CL**4
00774                                      WS-DET5-LF-HDG                  CL**4
00775                                      WS-DET7-LF-HDG                  CL**4
00776                                      WS-DET9-LF-HDG.                 CL**4
00777      MOVE PI-AH-OVERRIDE-L6      TO  WS-DET3-AH-HDG                  CL**4
00778                                      WS-DET4-AH-HDG                  CL**4
00779                                      WS-DET6-AH-HDG                  CL**4
00780                                      WS-DET8-AH-HDG                  CL**4
00781                                      WS-DETA-AH-HDG.                 CL**4
00782      MOVE WS-HEADING1            TO  RF-DATA-133.                    CL**4
00783      MOVE '1'                    TO  RF-CTL-CHAR-133.                CL**4
00784                                                                      CL**4
00785      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00786                                                                      CL**4
00787      MOVE WS-HEADING2            TO  RF-DATA-133.                    CL**4
00788      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00789                                                                      CL**4
00790      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00791                                                                      CL**4
00792      ADD +1                      TO  WS-PAGE-NUMBER.                 CL**4
00793      MOVE WS-PAGE-NUMBER         TO  WS-H3-PAGE.                     CL**4
00794      MOVE WS-HEADING3            TO  RF-DATA-133.                    CL**4
00795      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00796                                                                      CL**4
00797      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00798                                                                      CL**4
00799      MOVE PREVIOUS-CARRIER       TO  WS-H4-CARRIER.                  CL**4
00800      MOVE WS-HEADING4            TO  RF-DATA-133.                    CL**4
00801      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00802                                                                      CL**4
00803      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00804                                                                      CL**4
00805      MOVE WS-HEADING5            TO  RF-DATA-133.                    CL**4
00806      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**4
00807                                                                      CL**4
00808      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00809                                                                      CL**4
00810      MOVE CARR-XTR-GOOD-LF-AMT   TO  WS-IS-LF-SP-GOOD.               CL**4
00811      MOVE CARR-XTR-GOOD-LF-OB    TO  WS-IS-LF-OB-GOOD.               CL**4
00812      MOVE CARR-XTR-GOOD-LF-CAN   TO  WS-CA-LF-GOOD.                  CL**4
           MOVE CARR-XTR-GOOD-LF-COM   TO  WS-CM-LF-GOOD.                  CL**4
00814      MOVE CARR-XTR-GOOD-AH-AMT   TO  WS-IS-AH-SP-GOOD.               CL**4
00815      MOVE CARR-XTR-GOOD-AH-OB    TO  WS-IS-AH-OB-GOOD.               CL**4
00816      MOVE CARR-XTR-GOOD-AH-CAN   TO  WS-CA-AH-GOOD.                  CL**4
           MOVE CARR-XTR-GOOD-AH-COM   TO  WS-CM-AH-GOOD.                  CL**4
00818      MOVE CARR-XTR-BAD-LF-AMT    TO  WS-IS-LF-SP-BAD.                CL**4
00819      MOVE CARR-XTR-BAD-LF-OB     TO  WS-IS-LF-OB-BAD.                CL**4
00820      MOVE CARR-XTR-BAD-LF-CAN    TO  WS-CA-LF-BAD.                   CL**4
00821      MOVE CARR-XTR-BAD-LF-COM    TO  WS-CM-LF-BAD.                   CL**4
00822      MOVE CARR-XTR-BAD-AH-AMT    TO  WS-IS-AH-SP-BAD.                CL**4
00823      MOVE CARR-XTR-BAD-AH-OB     TO  WS-IS-AH-OB-BAD.                CL**4
00824      MOVE CARR-XTR-BAD-AH-CAN    TO  WS-CA-AH-BAD.                   CL**4
00825      MOVE CARR-XTR-BAD-AH-COM    TO  WS-CM-AH-BAD.                   CL**4
00826      MOVE CARR-XTR-GOOD-IS-CNT   TO  WS-IS-CNT-GOOD.                 CL**4
00827      MOVE CARR-XTR-BAD-IS-CNT    TO  WS-IS-CNT-BAD.                  CL**4
00828      MOVE CARR-XTR-GOOD-CA-CNT   TO  WS-CA-CNT-GOOD.                 CL**4
00829      MOVE CARR-XTR-BAD-CA-CNT    TO  WS-CA-CNT-BAD.                  CL**4
00830                                                                      CL**4
00831      ADD CARR-XTR-GOOD-IS-CNT    CARR-XTR-BAD-IS-CNT                 CL**4
00832              GIVING WS-IS-CNT-TOT.                                   CL**4
00833      ADD CARR-XTR-GOOD-CA-CNT    CARR-XTR-BAD-CA-CNT                 CL**4
00834              GIVING WS-CA-CNT-TOT.                                   CL**4
00835                                                                      CL**4
00836      COMPUTE CARR-XTR-GOOD-LF-NET =                                  CL**4
00837          (CARR-XTR-GOOD-LF-AMT + CARR-XTR-GOOD-LF-OB                 CL**4
00838          - CARR-XTR-GOOD-LF-CAN).                                    CL**4
00839                                                                      CL**4
00840      COMPUTE CARR-XTR-GOOD-AH-NET =                                  CL**4
00841          (CARR-XTR-GOOD-AH-AMT + CARR-XTR-GOOD-AH-OB                 CL**4
00842          - CARR-XTR-GOOD-AH-CAN).                                    CL**4
00843                                                                      CL**4
00844      COMPUTE CARR-XTR-BAD-LF-NET =                                   CL**4
00845          (CARR-XTR-BAD-LF-AMT + CARR-XTR-BAD-LF-OB                   CL**4
00846          - CARR-XTR-BAD-LF-CAN).                                     CL**4
00847                                                                      CL**4
00848      COMPUTE CARR-XTR-BAD-AH-NET =                                   CL**4
00849          (CARR-XTR-BAD-AH-AMT + CARR-XTR-BAD-AH-OB                   CL**4
00850          - CARR-XTR-BAD-AH-CAN).                                     CL**4
00851                                                                      CL**4
00852      MOVE CARR-XTR-GOOD-LF-NET   TO  WS-NET-LF-GOOD.                 CL**4
00853      MOVE CARR-XTR-GOOD-AH-NET   TO  WS-NET-AH-GOOD.                 CL**4
00854      MOVE CARR-XTR-BAD-LF-NET    TO  WS-NET-LF-BAD.                  CL**4
00855      MOVE CARR-XTR-BAD-AH-NET    TO  WS-NET-AH-BAD.                  CL**4
00856                                                                      CL**4
00857      ADD CARR-XTR-GOOD-LF-NET    CARR-XTR-BAD-LF-NET                 CL**4
00858              GIVING WS-NET-LF-TOT.                                   CL**4
00859      ADD CARR-XTR-GOOD-AH-NET    CARR-XTR-BAD-AH-NET                 CL**4
00860              GIVING WS-NET-AH-TOT.                                   CL**4
00861                                                                      CL**4
00862      ADD CARR-XTR-GOOD-LF-AMT    CARR-XTR-BAD-LF-AMT                 CL**4
00863              GIVING WS-IS-LF-SP-TOT.                                 CL**4
00864      ADD CARR-XTR-GOOD-LF-OB     CARR-XTR-BAD-LF-OB                  CL**4
00865              GIVING WS-IS-LF-OB-TOT.                                 CL**4
00866      ADD CARR-XTR-GOOD-LF-CAN    CARR-XTR-BAD-LF-CAN                 CL**4
00867              GIVING WS-CA-LF-TOT.                                    CL**4
00868      ADD CARR-XTR-GOOD-LF-COM    CARR-XTR-BAD-LF-COM                 CL**4
00869              GIVING WS-CM-LF-TOT.                                    CL**4
00870                                                                      CL**4
00871      ADD CARR-XTR-GOOD-AH-AMT    CARR-XTR-BAD-AH-AMT                 CL**4
00872              GIVING WS-IS-AH-SP-TOT.                                 CL**4
00873      ADD CARR-XTR-GOOD-AH-OB     CARR-XTR-BAD-AH-OB                  CL**4
00874              GIVING WS-IS-AH-OB-TOT.                                 CL**4
00875      ADD CARR-XTR-GOOD-AH-CAN    CARR-XTR-BAD-AH-CAN                 CL**4
00876              GIVING WS-CA-AH-TOT.                                    CL**4
00877      ADD CARR-XTR-GOOD-AH-COM    CARR-XTR-BAD-AH-COM                 CL**4
00878              GIVING WS-CM-AH-TOT.                                    CL**4
00879                                                                      CL**4
00880      COMPUTE CARR-XTR-GOOD-IS-TOT =                                  CL**4
00881          (CARR-XTR-GOOD-LF-AMT + CARR-XTR-GOOD-LF-OB +               CL**4
00882           CARR-XTR-GOOD-AH-AMT + CARR-XTR-GOOD-AH-OB).               CL**4
00883                                                                      CL**4
00884      COMPUTE CARR-XTR-BAD-IS-TOT =                                   CL**4
00885          (CARR-XTR-BAD-LF-AMT + CARR-XTR-BAD-LF-OB +                 CL**4
00886           CARR-XTR-BAD-AH-AMT + CARR-XTR-BAD-AH-OB).                 CL**4
00887                                                                      CL**4
00888      COMPUTE CARR-XTR-GOOD-CA-TOT =                                  CL**4
00889          (CARR-XTR-GOOD-LF-CAN + CARR-XTR-GOOD-AH-CAN).              CL**4
00890                                                                      CL**4
00891      COMPUTE CARR-XTR-BAD-CA-TOT =                                   CL**4
00892          (CARR-XTR-BAD-LF-CAN + CARR-XTR-BAD-AH-CAN).                CL**4
00893                                                                      CL**4
00894      COMPUTE CARR-XTR-GOOD-NET-TOT =                                 CL**4
00895          (CARR-XTR-GOOD-LF-NET + CARR-XTR-GOOD-AH-NET).              CL**4
00896                                                                      CL**4
00897      COMPUTE CARR-XTR-BAD-NET-TOT =                                  CL**4
00898          (CARR-XTR-BAD-LF-NET + CARR-XTR-BAD-AH-NET).                CL**4
00899                                                                      CL**4
00900      COMPUTE CARR-XTR-GOOD-CM-TOT =                                  CL**4
00901          (CARR-XTR-GOOD-LF-COM + CARR-XTR-GOOD-AH-COM).              CL**4
00902                                                                      CL**4
00903      COMPUTE CARR-XTR-BAD-CM-TOT =                                   CL**4
00904          (CARR-XTR-BAD-LF-COM + CARR-XTR-BAD-AH-COM).                CL**4
00905                                                                      CL**4
00906      MOVE CARR-XTR-GOOD-IS-TOT   TO  WS-IS-TOT-GOOD.                 CL**4
00907      MOVE CARR-XTR-GOOD-CA-TOT   TO  WS-CA-TOT-GOOD.                 CL**4
00908      MOVE CARR-XTR-GOOD-NET-TOT  TO  WS-NET-TOT-GOOD.                CL**4
00909      MOVE CARR-XTR-GOOD-CM-TOT   TO  WS-CM-TOT-GOOD.                 CL**4
00910      MOVE CARR-XTR-BAD-IS-TOT    TO  WS-IS-TOT-BAD.                  CL**4
00911      MOVE CARR-XTR-BAD-CA-TOT    TO  WS-CA-TOT-BAD.                  CL**4
00912      MOVE CARR-XTR-BAD-NET-TOT   TO  WS-NET-TOT-BAD.                 CL**4
00913      MOVE CARR-XTR-BAD-CM-TOT    TO  WS-CM-TOT-BAD.                  CL**4
00914                                                                      CL**4
00915      ADD CARR-XTR-GOOD-IS-TOT    CARR-XTR-BAD-IS-TOT                 CL**4
00916              GIVING WS-IS-TOT-TOT.                                   CL**4
00917      ADD CARR-XTR-GOOD-CA-TOT    CARR-XTR-BAD-CA-TOT                 CL**4
00918              GIVING WS-CA-TOT-TOT.                                   CL**4
00919      ADD CARR-XTR-GOOD-NET-TOT   CARR-XTR-BAD-NET-TOT                CL**4
00920              GIVING WS-NET-TOT-TOT.                                  CL**4
00921      ADD CARR-XTR-GOOD-CM-TOT    CARR-XTR-BAD-CM-TOT                 CL**4
00922              GIVING WS-CM-TOT-TOT.                                   CL**4
00923                                                                      CL**4
00924      MOVE WS-DETAIL1             TO  RF-DATA-133.                    CL**4
00925      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**4
00926                                                                      CL**4
00927      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00928                                                                      CL**4
00929      MOVE WS-DETAIL2             TO  RF-DATA-133.                    CL**4
00930      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00931                                                                      CL**4
00932      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00933                                                                      CL**4
00934      MOVE WS-DETAIL3             TO  RF-DATA-133.                    CL**4
00935      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00936                                                                      CL**4
00937      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00938                                                                      CL**4
00939      MOVE WS-DETAIL4             TO  RF-DATA-133.                    CL**4
00940      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00941                                                                      CL**4
00942      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00943                                                                      CL**4
00944      MOVE WS-DETAIL5             TO  RF-DATA-133.                    CL**4
00945      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**4
00946                                                                      CL**4
00947      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00948                                                                      CL**4
00949      MOVE WS-DETAIL6             TO  RF-DATA-133.                    CL**4
00950      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00951                                                                      CL**4
00952      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00953                                                                      CL**4
00954      MOVE WS-DETAIL7             TO  RF-DATA-133.                    CL**4
00955      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**4
00956                                                                      CL**4
00957      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00958                                                                      CL**4
00959      MOVE WS-DETAIL8             TO  RF-DATA-133.                    CL**4
00960      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00961                                                                      CL**4
00962      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00963                                                                      CL**4
00964      MOVE WS-DETAIL9             TO  RF-DATA-133.                    CL**4
00965      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**4
00966                                                                      CL**4
00967      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00968                                                                      CL**4
00969      MOVE WS-DETAILA             TO  RF-DATA-133.                    CL**4
00970      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00971                                                                      CL**4
00972      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00973                                                                      CL**4
00974      MOVE WS-TOTAL1              TO  RF-DATA-133.                    CL**4
00975      MOVE '-'                    TO  RF-CTL-CHAR-133.                CL**4
00976                                                                      CL**4
00977      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00978                                                                      CL**4
00979      MOVE WS-TOTAL2              TO  RF-DATA-133.                    CL**4
00980      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00981                                                                      CL**4
00982      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00983                                                                      CL**4
00984      MOVE WS-TOTAL3              TO  RF-DATA-133.                    CL**4
00985      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00986                                                                      CL**4
00987      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00988                                                                      CL**4
00989      MOVE WS-TOTAL4              TO  RF-DATA-133.                    CL**4
00990      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
00991                                                                      CL**4
00992      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00993                                                                      CL**4
00994      MOVE WS-TOTAL5              TO  RF-DATA-133.                    CL**4
00995      MOVE '0'                    TO  RF-CTL-CHAR-133.                CL**4
00996                                                                      CL**4
00997      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
00998                                                                      CL**4
00999      MOVE WS-TOTAL6              TO  RF-DATA-133.                    CL**4
01000      MOVE ' '                    TO  RF-CTL-CHAR-133.                CL**4
01001                                                                      CL**4
01002      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                         CL**4
01003                                                                      CL**4
01004  4320-ROLL-TOTALS.                                                   CL**4
01005      COMPUTE REPT-XTR-GOOD-LF-AMT  = REPT-XTR-GOOD-LF-AMT            CL**4
01006                                    + CARR-XTR-GOOD-LF-AMT.           CL**4
01007      COMPUTE REPT-XTR-GOOD-LF-OB   = REPT-XTR-GOOD-LF-OB             CL**4
01008                                    + CARR-XTR-GOOD-LF-OB.            CL**4
01009      COMPUTE REPT-XTR-GOOD-LF-CAN  = REPT-XTR-GOOD-LF-CAN            CL**4
01010                                    + CARR-XTR-GOOD-LF-CAN.           CL**4
01011      COMPUTE REPT-XTR-GOOD-LF-COM  = REPT-XTR-GOOD-LF-COM            CL**4
01012                                    + CARR-XTR-GOOD-LF-COM.           CL**4
01013      COMPUTE REPT-XTR-GOOD-AH-AMT  = REPT-XTR-GOOD-AH-AMT            CL**4
01014                                    + CARR-XTR-GOOD-AH-AMT.           CL**4
01015      COMPUTE REPT-XTR-GOOD-AH-OB   = REPT-XTR-GOOD-AH-OB             CL**4
01016                                    + CARR-XTR-GOOD-AH-OB.            CL**4
01017      COMPUTE REPT-XTR-GOOD-AH-CAN  = REPT-XTR-GOOD-AH-CAN            CL**4
01018                                    + CARR-XTR-GOOD-AH-CAN.           CL**4
01019      COMPUTE REPT-XTR-GOOD-AH-COM  = REPT-XTR-GOOD-AH-COM            CL**4
01020                                    + CARR-XTR-GOOD-AH-COM.           CL**4
01021      COMPUTE REPT-XTR-BAD-LF-AMT   = REPT-XTR-BAD-LF-AMT             CL**4
01022                                    + CARR-XTR-BAD-LF-AMT.            CL**4
01023      COMPUTE REPT-XTR-BAD-LF-OB    = REPT-XTR-BAD-LF-OB              CL**4
01024                                    + CARR-XTR-BAD-LF-OB.             CL**4
01025      COMPUTE REPT-XTR-BAD-LF-CAN   = REPT-XTR-BAD-LF-CAN             CL**4
01026                                    + CARR-XTR-BAD-LF-CAN.            CL**4
01027      COMPUTE REPT-XTR-BAD-LF-COM   = REPT-XTR-BAD-LF-COM             CL**4
01028                                    + CARR-XTR-BAD-LF-COM.            CL**4
01029      COMPUTE REPT-XTR-BAD-AH-AMT   = REPT-XTR-BAD-AH-AMT             CL**4
01030                                    + CARR-XTR-BAD-AH-AMT.            CL**4
01031      COMPUTE REPT-XTR-BAD-AH-OB    = REPT-XTR-BAD-AH-OB              CL**4
01032                                    + CARR-XTR-BAD-AH-OB.             CL**4
01033      COMPUTE REPT-XTR-BAD-AH-CAN   = REPT-XTR-BAD-AH-CAN             CL**4
01034                                    + CARR-XTR-BAD-AH-CAN.            CL**4
01035      COMPUTE REPT-XTR-BAD-AH-COM   = REPT-XTR-BAD-AH-COM             CL**4
01036                                    + CARR-XTR-BAD-AH-COM.            CL**4
01037      COMPUTE REPT-XTR-GOOD-IS-CNT  = REPT-XTR-GOOD-IS-CNT            CL**4
01038                                    + CARR-XTR-GOOD-IS-CNT.           CL**4
01039      COMPUTE REPT-XTR-BAD-IS-CNT   = REPT-XTR-BAD-IS-CNT             CL**4
01040                                    + CARR-XTR-BAD-IS-CNT.            CL**4
01041      COMPUTE REPT-XTR-GOOD-CA-CNT  = REPT-XTR-GOOD-CA-CNT            CL**4
01042                                    + CARR-XTR-GOOD-CA-CNT.           CL**4
01043      COMPUTE REPT-XTR-BAD-CA-CNT   = REPT-XTR-BAD-CA-CNT             CL**4
01044                                    + CARR-XTR-BAD-CA-CNT.            CL**4
01045                                                                      CL**4
01046      MOVE ZEROS                  TO  CARR-XTR-GOOD-LF-AMT            CL**4
01047                                      CARR-XTR-GOOD-LF-OB             CL**4
01048                                      CARR-XTR-GOOD-LF-CAN            CL**4
01049                                      CARR-XTR-GOOD-LF-COM
01050                                      CARR-XTR-GOOD-AH-AMT            CL**4
01051                                      CARR-XTR-GOOD-AH-OB             CL**4
01052                                      CARR-XTR-GOOD-AH-CAN            CL**4
01053                                      CARR-XTR-GOOD-AH-COM
01054                                      CARR-XTR-BAD-LF-AMT             CL**4
01055                                      CARR-XTR-BAD-LF-OB              CL**4
01056                                      CARR-XTR-BAD-LF-CAN             CL**4
01057                                      CARR-XTR-BAD-LF-COM             CL**4
01058                                      CARR-XTR-BAD-AH-AMT             CL**4
01059                                      CARR-XTR-BAD-AH-OB              CL**4
01060                                      CARR-XTR-BAD-AH-CAN             CL**4
01061                                      CARR-XTR-BAD-AH-COM             CL**4
01062                                      CARR-XTR-GOOD-IS-CNT            CL**4
01063                                      CARR-XTR-BAD-IS-CNT             CL**4
01064                                      CARR-XTR-GOOD-CA-CNT            CL**4
01065                                      CARR-XTR-BAD-CA-CNT             CL**4
01066                                      CARR-XTR-GOOD-LF-NET            CL**4
01067                                      CARR-XTR-GOOD-AH-NET            CL**4
01068                                      CARR-XTR-BAD-LF-NET             CL**4
01069                                      CARR-XTR-BAD-AH-NET             CL**4
01070                                      CARR-XTR-GOOD-IS-TOT            CL**4
01071                                      CARR-XTR-GOOD-CA-TOT            CL**4
01072                                      CARR-XTR-GOOD-NET-TOT           CL**4
01073                                      CARR-XTR-GOOD-CM-TOT            CL**4
01074                                      CARR-XTR-BAD-IS-TOT             CL**4
01075                                      CARR-XTR-BAD-CA-TOT             CL**4
01076                                      CARR-XTR-BAD-NET-TOT            CL**4
01077                                      CARR-XTR-BAD-CM-TOT.            CL**4
01078                                                                      CL**4
01079  4330-SET-UP-CONTROL.                                                CL**4
01080      MOVE PB-CARRIER             TO  PREVIOUS-CARRIER.               CL**4
01081                                                                      CL**4
01082  4399-EXIT.                                                          CL**4
01083      EXIT.                                                           CL**4
01084  EJECT                                                               CL**4
01085  4900-ENDBR.                                                         CL**4
01086      IF GOOD-STARTBR  = 'YES'                                     EL676
01087          PERFORM 4300-PRINT-CARRIER  THRU  4399-EXIT                 CL**5
01088          EXEC CICS  ENDBR                                         EL676
01089              DATASET  (PEND-FILE-ID)                              EL676
01090              END-EXEC.                                            EL676
01091                                                                   EL676
01092  5000-PRINT-REPORT.                                               EL676
01093      MOVE PI-LIFE-OVERRIDE-L6    TO  WS-DET1-LF-HDG               EL676
01094                                      WS-DET2-LF-HDG               EL676
01095                                      WS-DET5-LF-HDG               EL676
01096                                      WS-DET7-LF-HDG               EL676
01097                                      WS-DET9-LF-HDG.              EL676
01098      MOVE PI-AH-OVERRIDE-L6      TO  WS-DET3-AH-HDG               EL676
01099                                      WS-DET4-AH-HDG               EL676
01100                                      WS-DET6-AH-HDG               EL676
01101                                      WS-DET8-AH-HDG               EL676
01102                                      WS-DETA-AH-HDG.              EL676
01103      MOVE WS-HEADING1            TO  RF-DATA-133.                 EL676
01104      MOVE '1'                    TO  RF-CTL-CHAR-133.             EL676
01105                                                                   EL676
01106      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01107                                                                   EL676
01108      MOVE WS-HEADING2            TO  RF-DATA-133.                 EL676
01109      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01110                                                                   EL676
01111      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01112                                                                   EL676
01113      ADD +1                      TO  WS-PAGE-NUMBER.                 CL**4
01114      MOVE WS-PAGE-NUMBER         TO  WS-H3-PAGE.                     CL**4
01115      MOVE WS-HEADING3            TO  RF-DATA-133.                 EL676
01116      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01117                                                                   EL676
01118      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01119                                                                   EL676
01120      MOVE WS-HEADING5            TO  RF-DATA-133.                    CL**4
01121      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL676
01122                                                                   EL676
01123      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01124                                                                   EL676
01125      MOVE REPT-XTR-GOOD-LF-AMT   TO  WS-IS-LF-SP-GOOD.            EL676
01126      MOVE REPT-XTR-GOOD-LF-OB    TO  WS-IS-LF-OB-GOOD.            EL676
01127      MOVE REPT-XTR-GOOD-LF-CAN   TO  WS-CA-LF-GOOD.               EL676
           MOVE REPT-XTR-GOOD-LF-COM   TO  WS-CM-LF-GOOD.               EL676

01129      MOVE REPT-XTR-GOOD-AH-AMT   TO  WS-IS-AH-SP-GOOD.            EL676
01130      MOVE REPT-XTR-GOOD-AH-OB    TO  WS-IS-AH-OB-GOOD.            EL676
01131      MOVE REPT-XTR-GOOD-AH-CAN   TO  WS-CA-AH-GOOD.               EL676
           MOVE REPT-XTR-GOOD-AH-COM   TO  WS-CM-AH-GOOD.               EL676
01133      MOVE REPT-XTR-BAD-LF-AMT    TO  WS-IS-LF-SP-BAD.             EL676
01134      MOVE REPT-XTR-BAD-LF-OB     TO  WS-IS-LF-OB-BAD.             EL676
01135      MOVE REPT-XTR-BAD-LF-CAN    TO  WS-CA-LF-BAD.                EL676
01136      MOVE REPT-XTR-BAD-LF-COM    TO  WS-CM-LF-BAD.                EL676
01137      MOVE REPT-XTR-BAD-AH-AMT    TO  WS-IS-AH-SP-BAD.             EL676
01138      MOVE REPT-XTR-BAD-AH-OB     TO  WS-IS-AH-OB-BAD.             EL676
01139      MOVE REPT-XTR-BAD-AH-CAN    TO  WS-CA-AH-BAD.                EL676
01140      MOVE REPT-XTR-BAD-AH-COM    TO  WS-CM-AH-BAD.                EL676
01141      MOVE REPT-XTR-GOOD-IS-CNT   TO  WS-IS-CNT-GOOD.              EL676
01142      MOVE REPT-XTR-BAD-IS-CNT    TO  WS-IS-CNT-BAD.               EL676
01143      MOVE REPT-XTR-GOOD-CA-CNT   TO  WS-CA-CNT-GOOD.              EL676
01144      MOVE REPT-XTR-BAD-CA-CNT    TO  WS-CA-CNT-BAD.               EL676
01145                                                                   EL676
01146      ADD REPT-XTR-GOOD-IS-CNT    REPT-XTR-BAD-IS-CNT              EL676
01147              GIVING WS-IS-CNT-TOT.                                EL676
01148      ADD REPT-XTR-GOOD-CA-CNT    REPT-XTR-BAD-CA-CNT              EL676
01149              GIVING WS-CA-CNT-TOT.                                EL676
01150                                                                   EL676
01151      COMPUTE REPT-XTR-GOOD-LF-NET =                               EL676
01152          (REPT-XTR-GOOD-LF-AMT + REPT-XTR-GOOD-LF-OB              EL676
01153          - REPT-XTR-GOOD-LF-CAN).                                 EL676
01154                                                                   EL676
01155      COMPUTE REPT-XTR-GOOD-AH-NET =                               EL676
01156          (REPT-XTR-GOOD-AH-AMT + REPT-XTR-GOOD-AH-OB              EL676
01157          - REPT-XTR-GOOD-AH-CAN).                                 EL676
01158                                                                   EL676
01159      COMPUTE REPT-XTR-BAD-LF-NET =                                EL676
01160          (REPT-XTR-BAD-LF-AMT + REPT-XTR-BAD-LF-OB                EL676
01161          - REPT-XTR-BAD-LF-CAN).                                  EL676
01162                                                                   EL676
01163      COMPUTE REPT-XTR-BAD-AH-NET =                                EL676
01164          (REPT-XTR-BAD-AH-AMT + REPT-XTR-BAD-AH-OB                EL676
01165          - REPT-XTR-BAD-AH-CAN).                                  EL676
01166                                                                   EL676
01167      MOVE REPT-XTR-GOOD-LF-NET   TO  WS-NET-LF-GOOD.              EL676
01168      MOVE REPT-XTR-GOOD-AH-NET   TO  WS-NET-AH-GOOD.              EL676
01169      MOVE REPT-XTR-BAD-LF-NET    TO  WS-NET-LF-BAD.               EL676
01170      MOVE REPT-XTR-BAD-AH-NET    TO  WS-NET-AH-BAD.               EL676
01171                                                                   EL676
01172      ADD REPT-XTR-GOOD-LF-NET    REPT-XTR-BAD-LF-NET              EL676
01173              GIVING WS-NET-LF-TOT.                                EL676
01174      ADD REPT-XTR-GOOD-AH-NET    REPT-XTR-BAD-AH-NET              EL676
01175              GIVING WS-NET-AH-TOT.                                EL676
01176                                                                   EL676
01177      ADD REPT-XTR-GOOD-LF-AMT    REPT-XTR-BAD-LF-AMT              EL676
01178              GIVING WS-IS-LF-SP-TOT.                              EL676
01179      ADD REPT-XTR-GOOD-LF-OB     REPT-XTR-BAD-LF-OB               EL676
01180              GIVING WS-IS-LF-OB-TOT.                              EL676
01181      ADD REPT-XTR-GOOD-LF-CAN    REPT-XTR-BAD-LF-CAN              EL676
01182              GIVING WS-CA-LF-TOT.                                 EL676
01183      ADD REPT-XTR-GOOD-LF-COM    REPT-XTR-BAD-LF-COM              EL676
01184              GIVING WS-CM-LF-TOT.                                 EL676
01185                                                                   EL676
01186      ADD REPT-XTR-GOOD-AH-AMT    REPT-XTR-BAD-AH-AMT              EL676
01187              GIVING WS-IS-AH-SP-TOT.                              EL676
01188      ADD REPT-XTR-GOOD-AH-OB     REPT-XTR-BAD-AH-OB               EL676
01189              GIVING WS-IS-AH-OB-TOT.                              EL676
01190      ADD REPT-XTR-GOOD-AH-CAN    REPT-XTR-BAD-AH-CAN              EL676
01191              GIVING WS-CA-AH-TOT.                                 EL676
01192      ADD REPT-XTR-GOOD-AH-COM    REPT-XTR-BAD-AH-COM              EL676
01193              GIVING WS-CM-AH-TOT.                                 EL676
01194                                                                   EL676
01195      COMPUTE REPT-XTR-GOOD-IS-TOT =                               EL676
01196          (REPT-XTR-GOOD-LF-AMT + REPT-XTR-GOOD-LF-OB +            EL676
01197           REPT-XTR-GOOD-AH-AMT + REPT-XTR-GOOD-AH-OB).            EL676
01198                                                                   EL676
01199      COMPUTE REPT-XTR-BAD-IS-TOT =                                EL676
01200          (REPT-XTR-BAD-LF-AMT + REPT-XTR-BAD-LF-OB +              EL676
01201           REPT-XTR-BAD-AH-AMT + REPT-XTR-BAD-AH-OB).              EL676
01202                                                                   EL676
01203      COMPUTE REPT-XTR-GOOD-CA-TOT =                               EL676
01204          (REPT-XTR-GOOD-LF-CAN + REPT-XTR-GOOD-AH-CAN).           EL676
01205                                                                   EL676
01206      COMPUTE REPT-XTR-BAD-CA-TOT =                                EL676
01207          (REPT-XTR-BAD-LF-CAN + REPT-XTR-BAD-AH-CAN).             EL676
01208                                                                   EL676
01209      COMPUTE REPT-XTR-GOOD-NET-TOT =                              EL676
01210          (REPT-XTR-GOOD-LF-NET + REPT-XTR-GOOD-AH-NET).           EL676
01211                                                                   EL676
01212      COMPUTE REPT-XTR-BAD-NET-TOT =                               EL676
01213          (REPT-XTR-BAD-LF-NET + REPT-XTR-BAD-AH-NET).             EL676
01214                                                                   EL676
01215      COMPUTE REPT-XTR-GOOD-CM-TOT =                               EL676
01216          (REPT-XTR-GOOD-LF-COM + REPT-XTR-GOOD-AH-COM)
01217                                                                   EL676
01218      COMPUTE REPT-XTR-BAD-CM-TOT =                                EL676
01219          (REPT-XTR-BAD-LF-COM + REPT-XTR-BAD-AH-COM).             EL676
01220                                                                   EL676
01221      MOVE REPT-XTR-GOOD-IS-TOT   TO  WS-IS-TOT-GOOD.              EL676
01222      MOVE REPT-XTR-GOOD-CA-TOT   TO  WS-CA-TOT-GOOD.              EL676
01223      MOVE REPT-XTR-GOOD-NET-TOT  TO  WS-NET-TOT-GOOD.             EL676
01224      MOVE REPT-XTR-GOOD-CM-TOT   TO  WS-CM-TOT-GOOD.              EL676
01225      MOVE REPT-XTR-BAD-IS-TOT    TO  WS-IS-TOT-BAD.               EL676
01226      MOVE REPT-XTR-BAD-CA-TOT    TO  WS-CA-TOT-BAD.               EL676
01227      MOVE REPT-XTR-BAD-NET-TOT   TO  WS-NET-TOT-BAD.              EL676
01228      MOVE REPT-XTR-BAD-CM-TOT    TO  WS-CM-TOT-BAD.               EL676
01229                                                                   EL676
01230      ADD REPT-XTR-GOOD-IS-TOT    REPT-XTR-BAD-IS-TOT              EL676
01231              GIVING WS-IS-TOT-TOT.                                EL676
01232      ADD REPT-XTR-GOOD-CA-TOT    REPT-XTR-BAD-CA-TOT              EL676
01233              GIVING WS-CA-TOT-TOT.                                EL676
01234      ADD REPT-XTR-GOOD-NET-TOT   REPT-XTR-BAD-NET-TOT             EL676
01235              GIVING WS-NET-TOT-TOT.                               EL676
01236      ADD REPT-XTR-GOOD-CM-TOT    REPT-XTR-BAD-CM-TOT              EL676
01237              GIVING WS-CM-TOT-TOT.                                EL676
01238                                                                   EL676
01239      MOVE WS-DETAIL1             TO  RF-DATA-133.                 EL676
01240      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL676
01241                                                                   EL676
01242      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01243                                                                   EL676
01244      MOVE WS-DETAIL2             TO  RF-DATA-133.                 EL676
01245      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01246                                                                   EL676
01247      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01248                                                                   EL676
01249      MOVE WS-DETAIL3             TO  RF-DATA-133.                 EL676
01250      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01251                                                                   EL676
01252      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01253                                                                   EL676
01254      MOVE WS-DETAIL4             TO  RF-DATA-133.                 EL676
01255      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01256                                                                   EL676
01257      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01258                                                                   EL676
01259      MOVE WS-DETAIL5             TO  RF-DATA-133.                 EL676
01260      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL676
01261                                                                   EL676
01262      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01263                                                                   EL676
01264      MOVE WS-DETAIL6             TO  RF-DATA-133.                 EL676
01265      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01266                                                                   EL676
01267      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01268                                                                   EL676
01269      MOVE WS-DETAIL7             TO  RF-DATA-133.                 EL676
01270      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL676
01271                                                                   EL676
01272      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01273                                                                   EL676
01274      MOVE WS-DETAIL8             TO  RF-DATA-133.                 EL676
01275      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01276                                                                   EL676
01277      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01278                                                                   EL676
01279      MOVE WS-DETAIL9             TO  RF-DATA-133.                 EL676
01280      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL676
01281                                                                   EL676
01282      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01283                                                                   EL676
01284      MOVE WS-DETAILA             TO  RF-DATA-133.                 EL676
01285      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01286                                                                   EL676
01287      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01288                                                                   EL676
01289      MOVE WS-TOTAL1              TO  RF-DATA-133.                 EL676
01290      MOVE '-'                    TO  RF-CTL-CHAR-133.             EL676
01291                                                                   EL676
01292      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01293                                                                   EL676
01294      MOVE WS-TOTAL2              TO  RF-DATA-133.                 EL676
01295      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01296                                                                   EL676
01297      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01298                                                                   EL676
01299      MOVE WS-TOTAL3              TO  RF-DATA-133.                 EL676
01300      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01301                                                                   EL676
01302      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01303                                                                   EL676
01304      MOVE WS-TOTAL4              TO  RF-DATA-133.                 EL676
01305      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01306                                                                   EL676
01307      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01308                                                                   EL676
01309      MOVE WS-TOTAL5              TO  RF-DATA-133.                 EL676
01310      MOVE '0'                    TO  RF-CTL-CHAR-133.             EL676
01311                                                                   EL676
01312      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01313                                                                   EL676
01314      MOVE WS-TOTAL6              TO  RF-DATA-133.                 EL676
01315      MOVE ' '                    TO  RF-CTL-CHAR-133.             EL676
01316                                                                   EL676
01317      PERFORM 7000-PRT-LINE  THRU  7000-EXIT.                      EL676
01318                                                                   EL676
01319  5100-DELETE-INITIAL-2.                                           EL676
01320      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
01321      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
01322      MOVE '2'                    TO  RF-RECORD-TYPE.              EL676
01323      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
01324      MOVE ZEROS                  TO  RF-LINE-NUMBER.              EL676
01325                                                                   EL676
01326      EXEC CICS  DELETE                                            EL676
01327          DATASET    (REPT-FILE-ID)                                EL676
01328          RIDFLD     (RF-CONTROL-PRIMARY)                          EL676
01329          KEYLENGTH  (11)                                          EL676
01330          END-EXEC.                                                EL676
01331                                                                   EL676
01332  5200-WRITE-TRAILER.                                              EL676
01333      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
01334      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
01335      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
01336      MOVE '2'                    TO  RF-RECORD-TYPE.              EL676
01337                                                                   EL676
01338      ADD +1                      TO  WS-LINE-NUMBER.              EL676
01339                                                                   EL676
01340      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL676
01341      MOVE SPACES                 TO  RF-TRAILER-RECORD.           EL676
01342                                                                      CL**2
01343      EXEC CICS ASKTIME ABSTIME(LCP-CICS-TIME)                        CL*11
01344      END-EXEC                                                        CL*11
01345      EXEC CICS FORMATTIME                                            CL*11
01346                ABSTIME(LCP-CICS-TIME)                                CL*11
01347                TIME(LCP-TIME-OF-DAY-XX)                              CL*11
01348      END-EXEC                                                        CL*11
01349      MOVE LCP-TIME-OF-DAY-68     TO  RF-PRINT-HH-MM-SS.              CL*11
01350      MOVE SAVE-DATE              TO  RF-CURRENT-DATE.             EL676
01351                                                                   EL676
01352      EXEC CICS  WRITE                                             EL676
01353          DATASET  (REPT-FILE-ID)                                  EL676
01354          FROM     (REPORT-SAVE-FILE)                              EL676
01355          RIDFLD   (RF-CONTROL-PRIMARY)                            EL676
01356          END-EXEC.                                                EL676
01357                                                                   EL676
01358      GO TO 9999-RETURN-CICS

           .
031113 6130-FIND-BENEFIT-CD.
031113
031113     EXEC CICS READ
031113        DATASET (CNTL-FILE-ID)
031113        SET     (ADDRESS OF CONTROL-FILE)
031113        RIDFLD  (ELCNTL-KEY)
031113        GTEQ
031113        RESP    (WS-RESPONSE)
031113     END-EXEC
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
031113     exec cics read
031113        dataset       ('ERACCT')
031113        ridfld        (eracct-key)
031113        set           (address of account-master)
031113        resp          (ws-response)
031113     end-exec
031113
031113     if resp-normal
031113        move am-dcc-product-code to ws-dcc-product-code
031113     end-if
031113
031113     .
031113 6200-exit.
031113     exit.

01360  7000-PRT-LINE.                                                   EL676
01361      MOVE PI-COMPANY-CD          TO  RF-COMPANY-CD.               EL676
01362      MOVE 'RF'                   TO  RF-RECORD-ID.                EL676
01363      MOVE '1'                    TO  RF-RECORD-TYPE.              EL676
01364      MOVE 'EL676'                TO  RF-REPORT-ID.                EL676
01365                                                                   EL676
01366      ADD +1                      TO  WS-LINE-NUMBER.              EL676
01367                                                                   EL676
01368      MOVE WS-LINE-NUMBER         TO  RF-LINE-NUMBER.              EL676
01369                                                                   EL676
01370      EXEC CICS WRITE                                              EL676
01371          DATASET  (REPT-FILE-ID)                                  EL676
01372          FROM     (REPORT-SAVE-FILE)                              EL676
01373          RIDFLD   (RF-CONTROL-PRIMARY)                            EL676
01374          END-EXEC.                                                EL676
01375                                                                   EL676
01376  7000-EXIT.                                                       EL676
01377      EXIT.                                                        EL676
01378                                                                   EL676
01379  8500-DATE-CONVERT.                                               EL676
01380      MOVE LINK-ELDATCV           TO  PGM-NAME.                    EL676
01381                                                                   EL676
01382      EXEC CICS LINK                                               EL676
01383          PROGRAM   (PGM-NAME)                                     EL676
01384          COMMAREA  (DATE-CONVERSION-DATA)                         EL676
01385          LENGTH    (DC-COMM-LENGTH)                               EL676
01386          END-EXEC.                                                EL676
01387                                                                   EL676
01388  8500-EXIT.                                                       EL676
01389      EXIT.                                                        EL676
01390                                                                   EL676
01391  8800-ABEND.                                                      EL676
01392      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL676
01393                                                                   EL676
01394      EXEC CICS LINK                                               EL676
01395          PROGRAM   ('EL004')                                      EL676
01396          COMMAREA  (EMI-LINE1)                                    EL676
01397          LENGTH    (72)                                           EL676
01398          END-EXEC.                                                EL676
01399                                                                   EL676
01400  9999-RETURN-CICS.                                                EL676
01401      EXEC CICS  RETURN                                            EL676
01402          END-EXEC.                                                EL676
01403                                                                   EL676
01404      GOBACK.                                                      EL676
