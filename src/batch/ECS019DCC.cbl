00001  IDENTIFICATION DIVISION.                                         03/19/98
00002                                                                   ECS019A
061004 PROGRAM-ID.                 ECS019DCC.                              LV004
00004 *              PROGRAM CONVERTED BY                               ECS019A
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS019A
00006 *              CONVERSION DATE 11/28/95 10:59:12.                 ECS019A
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS019A
00008 *                            VMOD=2.014.                          ECS019A
00009                                                                   ECS019A
00010 *AUTHOR.     LOGIC, INC.                                          ECS019A
00011 *            DALLAS, TEXAS.                                       ECS019A
00012                                                                   ECS019A
00013 *DATE-COMPILED.                                                   ECS019A
00014                                                                   ECS019A
00015 *SECURITY.   *****************************************************ECS019A
00016 *            *                                                   *ECS019A
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS019A
00018 *            *                                                   *ECS019A
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS019A
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS019A
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS019A
00022 *            *                                                   *ECS019A
00023 *            *****************************************************ECS019A
00024                                                                   ECS019A
00025 *REMARKS.                                                         ECS019A
00026 *          PRINT PREMIUM AND COMMISSION ANALYSIS.                 ECS019A
102501******************************************************************
102501*                   C H A N G E   L O G
102501*
102501* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102501*-----------------------------------------------------------------
102501*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102501* EFFECTIVE    NUMBER
102501*-----------------------------------------------------------------
102501* 102501    2001100100006  SMVA  REPLACE LOW-VALUES WITH SPACES
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                BENEFIT CODES FROM 450 TO 900
010303* 010303                   PEMA  CUSTOMIZE FOR DCC ONLY     
042804* 042804    2004042100005  SMVA  CHG RPT ID FROM ECS019 TO ECS019A
061004* 061004  CR2003080800002  SMVA  ADD DLR INC,LMBA,&CONTR FEES TO RPT
020305* 020305    2005020400002  PEMA  ADD CLP STATE PROCESSING
121506* 121506    2006020100003  PEMA  ADD CARRIER SUMMARY TO HARDCOPY
020107* 020107    2007020100001  PEMA  FIX PRINTING ISSUES WITH REPORT
121208* 121208    2008112500001  AJRA  CHG LMBA FEE TO PGM MGMT FEE
011410* 011410  CR2009050700003  PEMA  ADD SPP DEALER DIRECT
090214* 090214  IR2014090200004  PEMA  ADD CARR 7 CRED UNION PROCESSING
100114* 100114  IR2014100100002  PEMA  CORRECT CARR 7 REFUND CLP CALC
011116* 011116  CR2015082400003  PEMA  ADD CARRIER 9 PROCESSING
092602******************************************************************
00027                                                                   ECS019A
00028  ENVIRONMENT DIVISION.                                            ECS019A
00029  CONFIGURATION SECTION.                                           ECS019A
00030  SPECIAL-NAMES.                                                   ECS019A
00031      C02 IS LCP-CH2                                               ECS019A
00032      C03 IS LCP-CH3                                               ECS019A
00033      C04 IS LCP-CH4                                               ECS019A
00034      C05 IS LCP-CH5                                               ECS019A
00035      C06 IS LCP-CH6                                               ECS019A
00036      C07 IS LCP-CH7                                               ECS019A
00037      C08 IS LCP-CH8                                               ECS019A
00038      C09 IS LCP-CH9                                               ECS019A
00039      C10 IS LCP-CH10                                              ECS019A
00040      C11 IS LCP-CH11                                              ECS019A
00041      C12 IS LCP-CH12                                              ECS019A
00042      S01 IS LCP-P01                                               ECS019A
00043      S02 IS LCP-P02.                                              ECS019A
00044  INPUT-OUTPUT SECTION.                                            ECS019A
00045  FILE-CONTROL.                                                    ECS019A
00046      SELECT HISTORY-IN       ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS019A
00047      SELECT HISTORY-OUT      ASSIGN TO SYS011-UT-2400-S-SYS011.   ECS019A
00048      SELECT EXTRACT          ASSIGN TO SYS016-UT-FBA1-S-SYS016.   ECS019A
00049      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS019A
00050      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS019A
00051      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS019A
00052      SELECT SORTFL           ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  ECS019A
00053                                                                   ECS019A
00054      SELECT ERMEBL                                                ECS019A
00055              ASSIGN SYS024-FBA1-ERMEBL                            ECS019A
00056              ORGANIZATION INDEXED                                 ECS019A
00057              ACCESS DYNAMIC                                       ECS019A
00058              RECORD KEY ME-CONTROL-PRIMARY                        ECS019A
00059              FILE STATUS ERMEBL-FILE-STATUS.                      ECS019A
00060                                                                   ECS019A
00061      SELECT ERACCTT                                               ECS019A
00062              ASSIGN TO SYS025-FBA1-ERACCTT                        ECS019A
00063              ORGANIZATION INDEXED                                 ECS019A
00064              ACCESS IS DYNAMIC                                    ECS019A
00065              RECORD KEY AM-CONTROL-PRIMARY                        ECS019A
00066              FILE STATUS ERACCT-FILE-STATUS.                      ECS019A
00067                                                                   ECS019A
00068  EJECT                                                            ECS019A
00069  DATA DIVISION.                                                   ECS019A
00070  FILE SECTION.                                                    ECS019A
00071                                                                   ECS019A
00072  FD  HISTORY-IN                                                   ECS019A
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORDING MODE IS F.                                         ECS019A
00075                                                                   ECS019A
011410 01  HIST-IN-RECORD                  PIC X(165).
00077                                                                   ECS019A
00078  FD  HISTORY-OUT                                                  ECS019A
00079      BLOCK CONTAINS 0 RECORDS
00080      RECORDING MODE IS F.                                         ECS019A
00081                                                                   ECS019A
011410 01  HISTORY-RECORD                  PIC X(165).
00083                                                                      CL**3
00084      EJECT                                                        ECS019A
00085  FD  EXTRACT                                                      ECS019A
00086      BLOCK CONTAINS 0 RECORDS
00087      RECORDING MODE IS F.                                         ECS019A
00088                                                                   ECS019A
011410 01  EXTR-RECORD                     PIC X(165).
00090                                                                      CL**3
00091      EJECT                                                        ECS019A
00092  FD  ERACCTT.                                                     ECS019A
00093                                                                   ECS019A
00094                                      COPY ERCACCT.                ECS019A
00095      EJECT                                                        ECS019A
00096  FD  DISK-DATE                                                    ECS019A
00097                                      COPY ELCDTEFD.               ECS019A
00098      EJECT                                                        ECS019A
00099  FD  PRNTR                                                        ECS019A
00100                                      COPY ELCPRTFD.               ECS019A
00101                                                                   ECS019A
00102  FD  FICH                                                         ECS019A
00103                                      COPY ELCFCHFD.               ECS019A
00104      EJECT                                                        ECS019A
00105  SD  SORTFL.                                                      ECS019A
00106                                                                   ECS019A
00107  01  SRT-REC.                                                     ECS019A
00108      12  SRT-REPORT-CD-1             PIC X(10).                   ECS019A
00109      12  SRT-WORK-REC.                                            ECS019A
00110          16  S-PARM                  PIC X(20).                   ECS019A
061004         16  FILLER                  PIC X(39).                   ECS019A
020305         16  FILLER                  PIC X(06).
011410         16  FILLER                  PIC X(100).
00113                                                                   ECS019A
00114  FD  ERMEBL.                                                      ECS019A
00115                                                                   ECS019A
00116                                      COPY ERCMEBL.                ECS019A
00117  EJECT                                                            ECS019A
00118  WORKING-STORAGE SECTION.                                         ECS019A
00119  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS019A
00120  01  LCP-ASA                       PIC X.                         ECS019A
00121  01  FILLER  PIC X(32) VALUE '********************************'.  ECS019A
00122  01  FILLER  PIC X(32) VALUE '     ECS019A WORKING STORAGE    '.  ECS019A
00123  01  FILLER  PIC X(32) VALUE '***********VMOD=2.014***********'.  ECS019A
00124                                                                   ECS019A
       01  SAVE-ACCT-KEY                   PIC X(19)  VALUE SPACES.
00125  01  X                               PIC X       VALUE SPACE.     ECS019A
00126  01  SPACE-N                         PIC X       VALUE '1'.       ECS019A
00127  01  SPACE-1                         PIC X       VALUE ' '.       ECS019A
00128  01  SPACE-2                         PIC X       VALUE '0'.       ECS019A
00129  01  SPACE-3                         PIC X       VALUE '-'.       ECS019A
00130  01  X-NET                           PIC S9(9)V99          COMP-3.ECS019A
00131  01  X-TOTAL                         PIC S9(9)V99          COMP-3.ECS019A
00132  01  X1                              PIC S999              COMP-3.ECS019A
00133  01  X2                              PIC S999              COMP-3.ECS019A
00134  01  R1                              PIC S999              COMP-3.ECS019A
00135  01  R2                              PIC S999              COMP-3.ECS019A
00136  01  RECD-CTR                        PIC S9(4) VALUE +0.          ECS019A
00137  01  AH-SW                           PIC S9                COMP-3.ECS019A
00138  01  LIFE-SW                         PIC S9                COMP-3.ECS019A
00139  01  IND-SW                          PIC S9                COMP-3.ECS019A
00140  01  GRP-SW                          PIC S9                COMP-3.ECS019A
00141  01  PRT-SW                          PIC S9                COMP-3.ECS019A
00142  01  FST-SW                          PIC S9                COMP-3.ECS019A
00143  01  PGCTR                           PIC S9(5)             COMP-3.ECS019A
00144  01  LNCTR                           PIC S999              COMP-3.ECS019A
00145  01  SAVE-X2                         PIC S999              COMP-3.ECS019A
00146  01  HAVE-SEQ                        PIC X(20).                   ECS019A
00147  01  NEED-SEQ                        PIC X(19).                   ECS019A
00148  01  SAVE-NAME                       PIC X(30)   VALUE SPACES.    ECS019A
00149  01  SAVE-REPORT-CD-1                PIC X(10)   VALUE SPACES.    ECS019A
00150  01  SAVE-GROUP                      PIC X(06).                   ECS019A
061004 01  WS-TOTAL-FEES                   PIC S9(9)V99 VALUE +0.
011410 01  WS-DIS-AMT1                     PIC Z,ZZZ,ZZ9.99-.
011410 01  WS-DIS-AMT2                     PIC Z,ZZZ,ZZ9.99-.
00151                                                                   ECS019A
00152  01  MONTH-END-DATA.                                              ECS019A
00153      12  ME-START-DATE.                                           ECS019A
00154          16  ME-START-MO             PIC 99.                      ECS019A
00155          16  FILLER                  PIC X.                       ECS019A
00156          16  ME-START-DA             PIC 99.                      ECS019A
00157          16  FILLER                  PIC X.                       ECS019A
00158          16  ME-START-YR             PIC 99.                      ECS019A
00159      12  ME-CNDS-DATE                PIC 9(6).                    ECS019A
00160      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   ECS019A
00161          16  ME-CNDS-MO              PIC 99.                      ECS019A
00162          16  ME-CNDS-DA              PIC 99.                      ECS019A
00163          16  ME-CNDS-YR              PIC 99.                      ECS019A
00164      12  ME-START-TIME               PIC 9(6).                    ECS019A
00165      12  ME-UPDATE-FLAG              PIC X       VALUE 'Y'.       ECS019A
00166          88  ME-DO-UPDATE                        VALUE 'Y'.       ECS019A
00167          88  ME-NO-UPDATE                        VALUE 'N'.       ECS019A
00168      12  ERMEBL-FILE-STATUS          PIC XX.                      ECS019A
00169      12  ERACCT-FILE-STATUS          PIC XX.                      ECS019A
00170      12  MONTH-END-MOYR              PIC S9(5)             COMP-3.ECS019A
00171      12  HLD-019-PREM-L              PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00172      12  HLD-019-PREM-AH             PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00173      12  HLD-019-REF-L               PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00174      12  HLD-019-REF-AH              PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00175      12  HLD-019-COMM-L              PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00176      12  HLD-019-COMM-AH             PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00177      12  HLD-019-OR-L                PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00178      12  HLD-019-OR-AH               PIC S9(9)V99 VALUE +0 COMP-3.ECS019A
00179                                                                   ECS019A
00180  01  WS.                                                          ECS019A
00181      12  REPORT-OPTIONS              PIC X        VALUE '1'.      ECS019A
00182          88  CURRENT-MONTH                        VALUE '1'.      ECS019A
00183          88  QTR-TO-DATE                          VALUE '2'.      ECS019A
00184          88  YEAR-TO-DATE                         VALUE '3'.      ECS019A
00185          88  LAST-TWELVE                          VALUE '4'.      ECS019A
00186      12  BEGIN-DATE.                                                 CL**4
00187          16  BEGIN-CCYY              PIC 9(04)   VALUE 0.         ECS019A
00188          16  BEGIN-CCYR REDEFINES BEGIN-CCYY.                     ECS019A
00189              20  BEGIN-CC            PIC 99.                      ECS019A
00190              20  BEGIN-YR            PIC 99.                      ECS019A
00191          16  BEGIN-MO                PIC 99      VALUE 0.         ECS019A
00192              88  1ST-QTR                         VALUES 01 02 03. ECS019A
00193              88  2ND-QTR                         VALUES 04 05 06. ECS019A
00194              88  3RD-QTR                         VALUES 07 08 09. ECS019A
00195              88  4TH-QTR                         VALUES 10 11 12. ECS019A
00196                                                                      CL**3
00197      12  WS-PROCESS-DATE.                                            CL**3
00198          16  FILLER                  PIC 9.                          CL**3
00199          16  WS-PROCESS-CCYYMM.                                      CL**3
00200              20  W-CC                PIC 99.                         CL**3
00201              20  W-YR                PIC 99.                         CL**3
00202              20  W-MO                PIC 99.                         CL**3
00203      12  WS-PROCESS-NUMERIC  REDEFINES                               CL**3
00204          WS-PROCESS-DATE             PIC 9(7).                       CL**3
00205                                                                      CL**3
00206      12  WS-RETURN-CODE              PIC S9(4)   VALUE ZEROS COMP.ECS019A
00207      12  WS-ABEND-MESSAGE            PIC X(80).                   ECS019A
00208      12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZEROS.     ECS019A
00209      12  WS-ZERO                     PIC S9      VALUE +0  COMP-3.ECS019A
00210      12  PGM-SUB                     PIC S999    VALUE +019  COMP.ECS019A
00211      EJECT                                                        ECS019A
00212                                      COPY ELCDTECX.               ECS019A
00213      EJECT                                                        ECS019A
00214                                      COPY ELCDTEVR.               ECS019A
00215      EJECT                                                        ECS019A
00216  01  HD1.                                                         ECS019A
00217      12  HD1-RPT-CD-1-HDG        PIC X(10)   VALUE SPACES.        ECS019A
00218      12  HD1-FILL                PIC XXX     VALUE ' : '.         ECS019A
00219      12  HD1-RPT-CD-1            PIC X(10)   VALUE SPACES.        ECS019A
00220      12  FILLER                  PIC X(12)   VALUE SPACES.        ECS019A
00221      12  FILLER      PIC X(24)   VALUE 'PREMIUM AND COMMISSION D'.ECS019A
00222      12  FILLER                  PIC X(11)   VALUE 'ISTRIBUTION'. ECS019A
00223      12  HD1-OPTION              PIC X(18)                        ECS019A
00224                         VALUE ' -  CURRENT MONTH '.               ECS019A
00225      12  HD1-OPTION-2            PIC X(31)   VALUE SPACES.        ECS019A
042804     12  FILLER                  PIC X(8)    VALUE 'ECS019A '.    ECS019A
00227                                                                   ECS019A
00228  01  HD2.                                                         ECS019A
00229      12  SUB-HD2                 PIC X(47)   VALUE SPACES.        ECS019A
00230      12  HD-CO                   PIC X(30).                       ECS019A
00231      12  FILLER                  PIC X(42)   VALUE SPACES.        ECS019A
00232      12  HD-RD                   PIC X(8).                        ECS019A
00233                                                                   ECS019A
00234  01  HD3.                                                         ECS019A
00235      12  SUB-HD3                 PIC X(53)   VALUE SPACES.        ECS019A
00236      12  HD-DT                   PIC X(18).                       ECS019A
00237      12  FILLER                  PIC X(48)   VALUE SPACES.        ECS019A
00238      12  FILLER                  PIC X(5)    VALUE 'PAGE '.       ECS019A
00239      12  HD-PAGE                 PIC ZZ,ZZ9.                      ECS019A
00240                                                                   ECS019A
00241  01  HD4.
061004     12 HD-DESC                  PIC X(05).
061004     12 FILLER                   PIC X(123)  VALUE '              
      -    ' * *  P R E M I U M  * *           * * * * * * * *  C O M M 
      -    'I S S I O N  * * * * * * * *               CLAIMS'.

00241  01  HD5.
061004     12 FILLER                   PIC X(56)   VALUE SPACES.
061004     12 FILLER                   PIC X(57)   VALUE 'O/W    PGM MGM
      -    'T FEE    INCENTIVE  CONTRACT FEE  CSO ADMIN'.

00257  01  SUB-HEADINGS.                                                ECS019A
00258      03 HEAD2.                                                    ECS019A
00259         05 FILLER                PIC X(40)   VALUE                ECS019A
00260      'CARR  GROUP  STATE                  ACCT'.                  ECS019A
00261      03 ACCT-HDA REDEFINES HEAD2.                                 ECS019A
00262         05 ACCT-HD2              PIC X(40).                       ECS019A
00263      03 ST-HDA REDEFINES HEAD2.                                   ECS019A
00264         05 ST-HD2                PIC X(28).                       ECS019A
00265         05 FILLER                PIC X(12).                       ECS019A
00266      03 CO-HDA REDEFINES HEAD2.                                   ECS019A
00267         05 CO-HD2                PIC X(11).                       ECS019A
00268         05 FILLER                PIC X(29).                       ECS019A
00269      03 CARR-HDA REDEFINES HEAD2.                                 ECS019A
00270         05 CARR-HD2              PIC X(4).                        ECS019A
00271         05 FILLER                PIC X(36).                       ECS019A
00272      03 RPT-CD-1-HD2.                                             ECS019A
00273         05 FILLER                PIC X(40)   VALUE                ECS019A
00274      'REPORT CODE 1   '.                                          ECS019A
00275                                                                   ECS019A
00276      03 HEAD3.                                                    ECS019A
00277         05 FILLER                PIC XX      VALUE SPACES.        ECS019A
00278         05 HD-CARR               PIC X.                           ECS019A
00279         05 FILLER                PIC XXX     VALUE SPACES.        ECS019A
00280         05 HD-GROUP              PIC X(6).                        ECS019A
00281         05 FILLER                PIC X       VALUE SPACES.        ECS019A
00282         05 HD-ST                 PIC XX.                          ECS019A
00283         05 FILLER                PIC X       VALUE SPACES.        ECS019A
00284         05 HD-ST-NM              PIC X(15).                       ECS019A
00285         05 FILLER                PIC XX      VALUE SPACES.        ECS019A
00286         05 HD-ACCT               PIC X(10).                       ECS019A
00287      03 ACCT-HDB REDEFINES HEAD3.                                 ECS019A
00288         05 ACCT-HD3              PIC X(43).                       ECS019A
00289      03 ST-HDB REDEFINES HEAD3.                                   ECS019A
00290         05 ST-HD3                PIC X(28).                       ECS019A
00291         05 FILLER                PIC X(15).                       ECS019A
00292      03 CO-HDB REDEFINES HEAD3.                                   ECS019A
00293         05 CO-HD3                PIC X(13).                       ECS019A
00294         05 FILLER                PIC X(30).                       ECS019A
00295      03 CARR-HDB REDEFINES HEAD3.                                 ECS019A
00296         05 CARR-HD3              PIC X(4).                        ECS019A
00297         05 FILLER                PIC X(39).                       ECS019A
00298      03 RPT-CD-1-HD3.                                             ECS019A
00299         05 FILLER                PIC XX      VALUE SPACES.        ECS019A
00300         05 HD-RPT-CD-1           PIC X(10).                       ECS019A
00301                                                                   ECS019A
00302      03 SUMM-HD.                                                  ECS019A
00303         05 HD-SUMM-CARR-DESC.                                     ECS019A
00304            07 FILLER             PIC X(8)    VALUE 'CARRIER '.    ECS019A
00305            07 HD-SUMM-CARR       PIC XXX.                         ECS019A
00306            07 HD-SUMM-FINAL-DESC.                                 ECS019A
00307               09 FILLER          PIC X(8)    VALUE '  STATE '.    ECS019A
00308               09 HD-SUMM-ST      PIC XXX.                         ECS019A
00309               09 HD-SUMM-ST-NM   PIC X(15).                       ECS019A
00310  EJECT                                                            ECS019A
00311  01  WORK-REC.                                                    ECS019A
00312      12  W-SEQ.                                                   ECS019A
00313          16  W-ACCT-CNTL.                                         ECS019A
00314              20  W-CARR              PIC X(01).                   ECS019A
00315              20  W-GROUP             PIC X(06).                   ECS019A
00316              20  W-ST                PIC X(02).                   ECS019A
00317              20  W-ACCT              PIC X(10).                   ECS019A
00318          16  W-IG                    PIC 9(01).                   ECS019A
00319          16  W-TYPE                  PIC X(03).                   ECS019A
00320      12  W-CODE                      PIC 9(01).                   ECS019A
00321      12  W-AMTS              COMP-3.      
00322          16  W-AMT                   PIC S9(9)V99.
00323          16  W-BASE                  PIC S9(7)V99.
00324          16  W-OVER                  PIC S9(7)V99.
061004         16  W-DLR-INC               PIC S9(7)V99.
011410         16  W-LF-LMBA-FEE           PIC S9(7)V99.
011410         16  W-AH-LMBA-FEE           PIC S9(7)V99.
061004         16  W-BANK-FEE              PIC S9(7)V99.
               16  W-CSO-ADMIN             PIC S9(7)V99.
00325      12  W-PROCESS-DATE              PIC 9(07)      COMP-3.
032303     12  W-RECALC                    PIC X.
020305     12  W-ACCT-TYPE                 PIC X.
020305     12  W-OVER-TYPE                 PIC X.
020305     12  W-CLP-STATE                 PIC XX.
120305     12  FILLER                      PIC X.
011410     12  W-SPPDD-CLP                 PIC S9(7)V99 COMP-3.
011410     12  FILLER                      PIC X(85).
00327                                                                   ECS019A
020305 01  CUR-ACCT-ST                     PIC XX.
00328  01  CUR-SEQ.                                                     ECS019A
00329      12  CUR-ACCT-CNTL.                                           ECS019A
00330          16  CUR-CARR                PIC X.                       ECS019A
00331          16  CUR-GROUP               PIC X(6).                    ECS019A
00332          16  CUR-ST                  PIC XX.                      ECS019A
00333          16  CUR-ACCT                PIC X(10).                   ECS019A
00334      12  CUR-IG                      PIC 9.                       ECS019A
00335      12  CUR-TYPE.                                                ECS019A
00336          16  CUR-TYP                 PIC XX.                      ECS019A
00337          16  CUR-OB                  PIC X.                       ECS019A
00338                                                                   ECS019A
00339  01  BLD-DESC.                                                    ECS019A
00340      12  BLD-TOTAL                   PIC X(6)    VALUE SPACES.    ECS019A
00341      12  BLD-DESC1                   PIC XX      VALUE SPACES.    ECS019A
00342      12  FILLER                      PIC XX      VALUE SPACES.    ECS019A
00343      12  B-SLASH1                    PIC X       VALUE SPACE.     ECS019A
00344      12  FILLER                      PIC XX      VALUE SPACES.    ECS019A
00345      12  BLD-DESC2                   PIC XX      VALUE SPACES.    ECS019A
00346      12  FILLER                      PIC X       VALUE SPACE.     ECS019A
00347                                                                   ECS019A
00348  01  X-ZERO.                                                      ECS019A
011410     12  FILLER                      PIC X(118800).               ECS019A
00350                                                                   ECS019A
00351  01  X-ZERO-5.                                                    ECS019A
011410     12  FILLER                      PIC X(66).                   ECS019A
00353                                                                   ECS019A
00354  01  WX-POINTER.                                                  ECS019A
00355      12  WX-LAH                      PIC 9.                       ECS019A
00356      12  WX-TYPE.                                                 ECS019A
00357          16  WX-TYP                  PIC XX.                      ECS019A
00358          16  WX-OB                   PIC X.                       ECS019A
00359                                                                   ECS019A
00360  01  WX-DESC.                                                     ECS019A
00361      12  WX-DESC-10                  PIC X(10).                   ECS019A
00362      12  FILLER                      PIC XX.                      ECS019A
00363      12  WX-DESC-OB                  PIC X(4).                    ECS019A
00364                                                                   ECS019A
00365  01  X-POINTERS.                                                  ECS019A
092602     12  X-POINTER OCCURS 900 TIMES.                              ECS019A
00367          16  X-LAH                   PIC 9.                       ECS019A
00368          16  X-TYPE.                                              ECS019A
00369              20  X-TYP               PIC XX.                      ECS019A
00370              20  X-OB                PIC X.                       ECS019A
00371                                                                   ECS019A
00372  01  X-DESCRIPTIONS.                                              ECS019A
092602     12  X-DESC OCCURS 900 TIMES.                                 ECS019A
00374          16  X-DESC-10               PIC X(10).                   ECS019A
00375                                                                   ECS019A
00376  01  X-TOTALS.                                                    ECS019A
00377      12  X-TOTS OCCURS 2 TIMES.                                   ECS019A
092602         16  X-LEVEL OCCURS 900 TIMES.                            ECS019A
00379              20  X-AMTS.                                          ECS019A
011410                 24  FILLER          PIC X(66).                   ECS019A
00381                                                                   ECS019A
00382  01  X-DETL.                                                      ECS019A
00383      12  X-ISSUE                     PIC S9(9)V99 COMP-3.
011410     12  X-SPPDD-CLP                 PIC S9(9)V99 COMP-3.
00384      12  X-CANCL                     PIC S9(9)V99 COMP-3.         ECS019A
00385      12  X-BASE                      PIC S9(9)V99 COMP-3.         ECS019A
00386      12  X-OVER                      PIC S9(9)V99 COMP-3.         ECS019A
00387      12  X-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019A
061004     12  X-DLR-INC                   PIC S9(9)V99 COMP-3.         ECS019A
011410     12  X-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.         ECS019A
011410     12  X-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.         ECS019A
061004     12  X-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  X-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00388                                                                   ECS019A
00389  01  R-TOTALS.                                                    ECS019A
00390      12  R-TOTS OCCURS 2 TIMES.                                   ECS019A
092602         16  R-LEVEL OCCURS 900 TIMES.                            ECS019A
00392              20  R-AMTS.                                          ECS019A
011410                 24  FILLER          PIC X(66).                   ECS019A
00394                                                                   ECS019A
00395  01  R-DETL.                                                      ECS019A
00396      12  R-ISSUE                     PIC S9(9)V99 COMP-3.
011410     12  R-SPPDD-CLP                 PIC S9(9)V99 COMP-3.
00397      12  R-CANCL                     PIC S9(9)V99 COMP-3.         ECS019A
00398      12  R-BASE                      PIC S9(9)V99 COMP-3.         ECS019A
00399      12  R-OVER                      PIC S9(9)V99 COMP-3.         ECS019A
00400      12  R-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019A
061004     12  R-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  R-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  R-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
061004     12  R-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  R-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00401                                                                   ECS019A
00402  01  ST-TOTALS.                                                   ECS019A
011410     12  FILLER                      PIC X(118800).               ECS019A
00404                                                                   ECS019A
00405  01  CO-TOTALS.                                                   ECS019A
011410     12  FILLER                      PIC X(118800).               ECS019A
00407                                                                   ECS019A
00408  01  CARR-TOTALS.                                                 ECS019A
011410     12  FILLER                      PIC X(118800).               ECS019A
00410                                                                   ECS019A
00411  01  RPT-CD-1-TOTALS.                                             ECS019A
011410     12  FILLER                      PIC X(118800).               ECS019A
00413                                                                   ECS019A
00414  01  FINAL-TOTALS.                                                ECS019A
011410     12  FILLER                      PIC X(118800).               ECS019A
00416                                                                   ECS019A
00417  01  SUB-TOTALS.                                                  ECS019A
00418      12  S-ISSUE                     PIC S9(9)V99 COMP-3.
011410     12  S-SPPDD-CLP                 PIC S9(9)V99 COMP-3.
00419      12  S-CANCL                     PIC S9(9)V99 COMP-3.         ECS019A
00420      12  S-BASE                      PIC S9(9)V99 COMP-3.         ECS019A
00421      12  S-OVER                      PIC S9(9)V99 COMP-3.         ECS019A
00422      12  S-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019A
061004     12  S-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  S-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  S-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
061004     12  S-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  S-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00423                                                                   ECS019A
00424  01  TOTALS.                                                      ECS019A
00425      12  T-ISSUE                     PIC S9(9)V99 COMP-3.         ECS019A
011410     12  T-SPPDD-CLP                 PIC S9(9)V99 COMP-3.
00426      12  T-CANCL                     PIC S9(9)V99 COMP-3.         ECS019A
00427      12  T-BASE                      PIC S9(9)V99 COMP-3.         ECS019A
00428      12  T-OVER                      PIC S9(9)V99 COMP-3.         ECS019A
00429      12  T-CLAIM                     PIC S9(9)V99 COMP-3.         ECS019A
061004     12  T-DLR-INC                   PIC S9(9)V99 COMP-3.
011410     12  T-LF-LMBA-FEE               PIC S9(9)V99 COMP-3.
011410     12  T-AH-LMBA-FEE               PIC S9(9)V99 COMP-3.
061004     12  T-BANK-FEE                  PIC S9(9)V99 COMP-3.
           12  T-CSO-ADMIN                 PIC S9(9)V99 COMP-3.
00430                                                                   ECS019A
00431  01  P-REC.                                                       ECS019A
00432      12  P-CCSW                      PIC X.                       ECS019A
00433      12  P-LN.                                                    ECS019A
061004         16  P-TYPE                  PIC X(03).                   ECS019A
061004         16  P-DESC                  PIC X(10).                   ECS019A
061004         16  FILLER                  PIC X(02).                   ECS019A
061004         16  P-ISSUE                 PIC ZZ,ZZZ,ZZZ.99-.          ECS019A
061004*        16  P-CANCL                 PIC Z,ZZZ,ZZZ.ZZ-.           ECS019A
061004*        16  P-NET                   PIC ZZ,ZZZ,ZZZ.ZZ-.          ECS019A
061004         16  FILLER                  PIC X(19).                   ECS019A
061004*        16  P-BASE                  PIC Z,ZZZ,ZZZ.ZZ-.           ECS019A
061004         16  P-OVER                  PIC Z,ZZZ,ZZZ.99-.           ECS019A
061004         16  P-LMBA-FEE              PIC ZZ,ZZZ,ZZZ.99-.
061004         16  P-DLR-INC               PIC Z,ZZZ,ZZZ.99-.
061004         16  P-BANK-FEE              PIC ZZ,ZZZ,ZZZ.99-.
               16  P-CSO-ADMIN             PIC Z,ZZZ,ZZZ.99-.
00443          16  P-CLAIM                 PIC ZZZ,ZZZ,ZZZ.99-.         ECS019A
061004         16  FILLER                  PIC X(03).
00444  EJECT                                                            ECS019A
00445  PROCEDURE DIVISION.                                              ECS019A
00446                                                                   ECS019A
00447  CAPTURE-START.                                                   ECS019A
00448  0000-SET-START.                                                  ECS019A
00449                                  COPY ELCDTERX SUPPRESS.          ECS019A
PEMUNI     OPEN OUTPUT HISTORY-OUT.                                     ECS019A
00451      MOVE WS-TIME                TO  ME-START-TIME.               ECS019A
00452      MOVE WS-CURRENT-DATE        TO  ME-START-DATE.               ECS019A
00453      MOVE ME-START-MO            TO  ME-CNDS-MO.                  ECS019A
00454      MOVE ME-START-DA            TO  ME-CNDS-DA.                  ECS019A
00455      MOVE ME-START-YR            TO  ME-CNDS-YR.                  ECS019A
00456                                                                   ECS019A
00457  0090-INITIALIZATION.                                             ECS019A
00458      MOVE WS-CURRENT-DATE        TO  HD-RD.                       ECS019A
00459      MOVE COMPANY-NAME           TO  HD-CO.                       ECS019A
00460      MOVE ALPH-DATE              TO  HD-DT.                       ECS019A
00461      MOVE RUN-MO                 TO  BEGIN-MO.                    ECS019A
00462      COMPUTE BEGIN-CCYY = RUN-CCYY - 1.                           ECS019A
00463                                                                   ECS019A
00464      MOVE SPACES                 TO CUR-SEQ.                      ECS019A
00465                                                                   ECS019A
00466  0100-SET-REPORT-FORMAT.                                          ECS019A

           MOVE 1                      TO DTE-TOT-OPT
00467                                                                   ECS019A
00468      MOVE DTE-FMT-OPT                TO  REPORT-OPTIONS.          ECS019A
00469                                                                   ECS019A
00470      IF QTR-TO-DATE                                               ECS019A
00471          MOVE ' - QUARTER TO DATE'   TO  HD1-OPTION.              ECS019A
00472                                                                   ECS019A
00473      IF YEAR-TO-DATE                                              ECS019A
00474          MOVE ' -  YEAR TO DATE  '   TO  HD1-OPTION.              ECS019A
00475                                                                   ECS019A
00476      IF LAST-TWELVE                                               ECS019A
00477          MOVE ' - LAST 12 MONTHS '   TO  HD1-OPTION.              ECS019A
00478                                                                   ECS019A
00479  EJECT                                                            ECS019A
00480  0110-SORT-ROUTINE SECTION.                                       ECS019A
00481      SORT SORTFL                                                  ECS019A
00482              ON ASCENDING KEY SRT-REPORT-CD-1 S-PARM              ECS019A
00483          INPUT PROCEDURE 0120-INPUT-RTN  THRU 0179-INPUT-XIT      ECS019A
00484         OUTPUT PROCEDURE 0180-OUTPUT-RTN THRU 0810-OUTPUT-XIT.    ECS019A
00485                                                                   ECS019A
00486      IF SORT-RETURN NOT = ZEROES                                  ECS019A
00487          MOVE +0101                     TO  WS-RETURN-CODE        ECS019A
00488          MOVE 'INTERNAL SORT ABORTED'   TO  WS-ABEND-MESSAGE      ECS019A
00489          GO TO ABEND-PGM.                                         ECS019A
00490                                                                   ECS019A
00491      GO TO 0830-EOJ-RTN.                                          ECS019A
00492      EJECT                                                        ECS019A
00493  0120-INPUT-RTN SECTION.                                          ECS019A
00494      OPEN INPUT EXTRACT  HISTORY-IN                               ECS019A
00495                 ERACCTT.                                          ECS019A
00497                                                                   ECS019A
00498      IF ERACCT-FILE-STATUS  = '00' OR '97'                        ECS019A
00499          NEXT SENTENCE                                            ECS019A
00500        ELSE                                                       ECS019A
00501          MOVE +0302                      TO  WS-RETURN-CODE       ECS019A
00502          MOVE 'ERROR OPENING ACCT MSTR'  TO  WS-ABEND-MESSAGE     ECS019A
00503          GO TO ABEND-PGM.                                         ECS019A
00504                                                                   ECS019A
00505  0130-READ-EXTRACT.                                               ECS019A
00506      READ EXTRACT INTO WORK-REC                                   ECS019A
00507                   AT END GO TO 0140-READ-HISTORY.                 ECS019A
00508                                                                   ECS019A
00509      MOVE W-PROCESS-DATE         TO WS-PROCESS-DATE.                 CL**3
00510                                                                      CL**3
00511      PERFORM 0150-RLS-RTN       THRU 0159-RLS-XIT.                ECS019A
00512      PERFORM 0160-WRITE-HISTORY THRU 0169-WRITE-XIT.              ECS019A
00513                                                                   ECS019A
00514        GO TO 0130-READ-EXTRACT.                                   ECS019A
00515                                                                   ECS019A
00516  0140-READ-HISTORY.                                               ECS019A
00517      READ HISTORY-IN INTO WORK-REC                                ECS019A
00518                   AT END GO TO 0170-CLOSE-FILES.                  ECS019A
00519                                                                   ECS019A
00520      MOVE W-PROCESS-DATE         TO WS-PROCESS-NUMERIC               CL**3
00521                                                                      CL**3
00522      IF W-YR = RUN-YR AND                                         ECS019A
00523         W-MO = RUN-MO                                             ECS019A
00524          GO TO 0140-READ-HISTORY.                                 ECS019A
00525                                                                   ECS019A
00526      IF WS-PROCESS-CCYYMM NOT GREATER THAN BEGIN-DATE                CL**4
00527          GO TO 0140-READ-HISTORY.                                 ECS019A
00528                                                                   ECS019A
00529      PERFORM 0160-WRITE-HISTORY THRU 0169-WRITE-XIT.              ECS019A
00530                                                                   ECS019A
00531      IF CURRENT-MONTH                                             ECS019A
00532          GO TO 0140-READ-HISTORY.                                 ECS019A
00533                                                                   ECS019A
00534      IF LAST-TWELVE                                               ECS019A
00535          PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT                   ECS019A
00536          GO TO 0140-READ-HISTORY.                                 ECS019A
00537                                                                   ECS019A
00538      IF W-YR NOT = RUN-YR                                         ECS019A
00539          GO TO 0140-READ-HISTORY.                                 ECS019A
00540                                                                   ECS019A
00541      IF YEAR-TO-DATE                                              ECS019A
00542          PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT                   ECS019A
00543          GO TO 0140-READ-HISTORY.                                 ECS019A
00544                                                                   ECS019A
00545      IF 1ST-QTR                                                   ECS019A
00546          IF W-MO = 01  OR  02  OR  03                             ECS019A
00547              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT               ECS019A
00548              GO TO 0140-READ-HISTORY.                             ECS019A
00549                                                                   ECS019A
00550      IF 2ND-QTR                                                   ECS019A
00551          IF W-MO = 04  OR  05  OR  06                             ECS019A
00552              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT               ECS019A
00553              GO TO 0140-READ-HISTORY.                             ECS019A
00554                                                                   ECS019A
00555      IF 3RD-QTR                                                   ECS019A
00556          IF W-MO = 07  OR  08  OR  09                             ECS019A
00557              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT               ECS019A
00558              GO TO 0140-READ-HISTORY.                             ECS019A
00559                                                                   ECS019A
00560      IF 4TH-QTR                                                   ECS019A
00561          IF W-MO = 10  OR  11  OR  12                             ECS019A
00562              PERFORM 0150-RLS-RTN THRU 0159-RLS-XIT.              ECS019A
00563                                                                   ECS019A
00564      GO TO 0140-READ-HISTORY.                                     ECS019A
00565                                                                   ECS019A
00566  0150-RLS-RTN.                                                    ECS019A
00567                                                                   ECS019A
102501     MOVE SPACES                 TO SRT-REPORT-CD-1.
00569                                                                   ECS019A
00570      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00571          IF W-ACCT NOT EQUAL HIGH-VALUES                          ECS019A
00572              IF W-ACCT-CNTL NOT = CUR-ACCT-CNTL                   ECS019A
00573                  MOVE W-SEQ             TO CUR-SEQ                ECS019A
00574                  PERFORM 0430-RD-ACCT-RTN THRU 0440-RD-ACCT-XIT.  ECS019A
00575                                                                   ECS019A
00576      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00577          IF W-CARR EQUAL HIGH-VALUES                              ECS019A
00578              MOVE HIGH-VALUES       TO SRT-REPORT-CD-1            ECS019A
00579          ELSE                                                     ECS019A
00580              MOVE SAVE-REPORT-CD-1  TO SRT-REPORT-CD-1.           ECS019A
00581                                                                   ECS019A
00582                                                                   ECS019A
020305     IF W-CLP-STATE = SPACES OR LOW-VALUES OR HIGH-VALUES
020305        MOVE W-ST                TO W-CLP-STATE
020305     END-IF
           
      *  THIS SWAPS THE STATES IF THERE IS A CLP STATE. THE PROGRAM
      *  WILL SORT USING THE CLP STATE BUT WHEN IT NEEDS TO READ THE
      *  ERACCT IT WILL USE THE ACCT STATE THAT IS STORED IN THE CLP 
      *  STATE FIELD.
      
           IF W-ST NOT = W-CLP-STATE
              MOVE W-ST                TO CUR-ACCT-ST
              MOVE W-CLP-STATE         TO W-ST
              MOVE CUR-ACCT-ST         TO W-CLP-STATE
              DISPLAY ' SWAPPING STATE FOR  ' W-ACCT
              ' W-ST ' W-ST ' W-CLP-STATE ' W-CLP-STATE
           END-IF

00583      MOVE WORK-REC               TO SRT-WORK-REC.                 ECS019A
00584      RELEASE SRT-REC.                                             ECS019A
00585                                                                   ECS019A
00586  0159-RLS-XIT.                                                    ECS019A
00587      EXIT.                                                        ECS019A
00588                                                                   ECS019A
00589  0160-WRITE-HISTORY.                                              ECS019A
00590      WRITE HISTORY-RECORD FROM WORK-REC.                          ECS019A
00591  0169-WRITE-XIT.                                                  ECS019A
00592      EXIT.                                                        ECS019A
00593                                                                   ECS019A
00594  0170-CLOSE-FILES.                                                ECS019A
00595      CLOSE EXTRACT  HISTORY-IN  HISTORY-OUT.                      ECS019A
00596  0179-INPUT-XIT.                                                  ECS019A
00597      EXIT.                                                        ECS019A
00598  EJECT                                                            ECS019A
00599  0180-OUTPUT-RTN SECTION.                                         ECS019A
00600      OPEN OUTPUT PRNTR.                                           ECS019A
00601      MOVE SPACES                 TO  P-REC                        ECS019A
00602                                      SAVE-REPORT-CD-1.            ECS019A

102501     MOVE SPACES                 TO  CUR-SEQ
                                           CUR-ACCT-ST
00604                                                                   ECS019A
00605      PERFORM 0690-FORMAT-RTN  THRU 0740-FORMAT-XIT.               ECS019A
00606                                                                   ECS019A
00607      MOVE 0                      TO  FST-SW PGCTR.                ECS019A
00608      MOVE +066                   TO  LNCTR.                       ECS019A
00609                                                                   ECS019A
00610      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00611          MOVE CLAS-REPORT-CD1-CAPTION TO HD1-RPT-CD-1-HDG.        ECS019A
00612                                                                   ECS019A
00614      MOVE SPACES                 TO HD1-OPTION-2.                 ECS019A
00615                                                                   ECS019A
00616  0190-RETURN-LOOP.                                                ECS019A
00617      RETURN SORTFL                                                ECS019A
00618          AT END GO TO 0800-END-OUTPUT.                            ECS019A
00619                                                                   ECS019A
00620      MOVE SRT-WORK-REC           TO  WORK-REC.                    ECS019A
00621      MOVE W-PROCESS-DATE         TO  WS-PROCESS-DATE.                CL**3
00622                                                                   ECS019A
00623      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00624          IF SRT-REPORT-CD-1 NOT EQUAL SAVE-REPORT-CD-1            ECS019A
00625             PERFORM 0230-BREAK-RTN THRU 0410-BREAK-XIT.           ECS019A
00626                                                                   ECS019A
00627      IF W-SEQ NOT = CUR-SEQ                                       ECS019A
00628         PERFORM 0230-BREAK-RTN THRU 0410-BREAK-XIT.               ECS019A
00629                                                                   ECS019A
00630      GO TO 0200-ACCUM-ISSUE                                       ECS019A
00631            0210-ACCUM-CANCL                                       ECS019A
00632            0220-ACCUM-CLAIM                                       ECS019A
00633         DEPENDING ON W-CODE.                                      ECS019A
00634                                                                   ECS019A
00635      DISPLAY 'INVALID CODE IN EXTRACT REC - ' W-CODE.             ECS019A
00636      MOVE +0301                             TO  WS-RETURN-CODE    ECS019A
00637      MOVE 'INVALID CODE IN EXTRACT RECORD'  TO WS-ABEND-MESSAGE.  ECS019A
00638      GO TO ABEND-PGM.                                             ECS019A
00639      EJECT                                                        ECS019A
00640  0200-ACCUM-ISSUE.                                                ECS019A

061004     ADD W-DLR-INC 
011410         W-LF-LMBA-FEE W-AH-LMBA-FEE
061004         W-BANK-FEE W-CSO-ADMIN
061004         GIVING WS-TOTAL-FEES.
061004     SUBTRACT WS-TOTAL-FEES      FROM W-AMT.
011116     if w-carr = '7' or '9'
090214        subtract w-over from w-amt
090214     end-if
           
061004     MOVE +0                     TO  WS-TOTAL-FEES.
011410     IF W-SPPDD-CLP NOT = ZEROS
011410        ADD W-SPPDD-CLP          TO  X-ISSUE
011410     ELSE
00641         ADD W-AMT                TO  X-ISSUE
00642         ADD W-BASE               TO  X-BASE
011410     END-IF
00643      ADD W-OVER                  TO  X-OVER
061004     ADD W-DLR-INC               TO  X-DLR-INC.
011410     ADD W-LF-LMBA-FEE           TO  X-LF-LMBA-FEE.
011410     ADD W-AH-LMBA-FEE           TO  X-AH-LMBA-FEE.
061004     ADD W-BANK-FEE              TO  X-BANK-FEE.
           ADD W-CSO-ADMIN             TO  X-CSO-ADMIN
00644      GO TO 0190-RETURN-LOOP.                                      ECS019A
00645                                                                   ECS019A
00646  0210-ACCUM-CANCL.                                                ECS019A

061004     ADD W-DLR-INC 
011410         W-LF-LMBA-FEE W-AH-LMBA-FEE
061004         W-BANK-FEE W-CSO-ADMIN
061004         GIVING WS-TOTAL-FEES.
 
061004     SUBTRACT WS-TOTAL-FEES      FROM W-AMT.
011116     if w-carr = '7' or '9'
100114        subtract w-over from w-amt
100114     end-if
061004     MOVE +0                     TO  WS-TOTAL-FEES.

011410     IF W-SPPDD-CLP NOT = ZEROS
011410        ADD W-SPPDD-CLP          TO X-CANCL
011410     ELSE
              IF W-AMT < ZEROS
                 COMPUTE W-AMT = W-AMT * -1
              END-IF
00641         ADD W-AMT                TO  X-CANCL
00642         ADD W-BASE               TO  X-BASE
011410     END-IF

00649      ADD W-OVER                  TO  X-OVER.                      ECS019A
061004     ADD W-DLR-INC               TO  X-DLR-INC.
011410     ADD W-LF-LMBA-FEE           TO  X-LF-LMBA-FEE.
011410     ADD W-AH-LMBA-FEE           TO  X-AH-LMBA-FEE.
061004     ADD W-BANK-FEE              TO  X-BANK-FEE
           ADD W-CSO-ADMIN             TO  X-CSO-ADMIN
00650      GO TO 0190-RETURN-LOOP.                                      ECS019A
00651                                                                   ECS019A
00652  0220-ACCUM-CLAIM.                                                ECS019A

00653      ADD W-AMT                   TO  X-CLAIM.                     ECS019A
00654      GO TO 0190-RETURN-LOOP.                                      ECS019A
00655                                                                   ECS019A
00656  0230-BREAK-RTN.                                                  ECS019A
00657      IF FST-SW = 0                                                ECS019A
00658          MOVE 1                  TO  FST-SW                       ECS019A
00659          GO TO 0330-INTL-FINAL.                                   ECS019A
00660                                                                   ECS019A
00661  0240-LINE-BREAK.                                                 ECS019A
00662      MOVE X-DETL                 TO  X-AMTS (X1 X2).              ECS019A
00663      MOVE W-TYPE                 TO  CUR-TYPE.                    ECS019A
00664      MOVE W-IG                   TO  CUR-IG.                      ECS019A
00665                                                                   ECS019A
00666      IF CUR-SEQ = W-SEQ                                           ECS019A
00667          GO TO 0390-INTL-LINE.                                    ECS019A
00668                                                                   ECS019A
00669      IF CUR-CARR = HIGH-VALUE                                     ECS019A
00670          GO TO 0310-FINAL-SUMMARY.                                ECS019A
00671                                                                   ECS019A
00672      IF CUR-ACCT = HIGH-VALUE                                     ECS019A
00673          GO TO 0290-CARR-SUMMARY.                                 ECS019A
00674                                                                   ECS019A
00675  0250-ACCT-BREAK.                                                 ECS019A
00676                                                                   ECS019A
00680      PERFORM 0430-RD-ACCT-RTN THRU 0440-RD-ACCT-XIT.              ECS019A
00681                                                                   ECS019A
00682      IF AM-CONTROL-PRIMARY EQUAL HIGH-VALUES AND                  ECS019A
00683         ERACCT-FILE-STATUS NOT = '10'                             ECS019A
00684          MOVE 'INVALID ACCOUNT'  TO  SAVE-NAME.                   ECS019A
00688                                                                   ECS019A
00689 *  TOTAL LEVEL 1.                                                 ECS019A
00690      MOVE ACCT-HD2               TO  SUB-HD2.                     ECS019A
00691      MOVE ACCT-HD3               TO  SUB-HD3.                     ECS019A
00692      MOVE ST-TOTALS              TO  R-TOTALS.                    ECS019A
00693      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019A
00694      MOVE R-TOTALS               TO  ST-TOTALS.                   ECS019A
00695                                                                   ECS019A
00696      IF DTE-PGM-OPT NOT GREATER THAN '1'                          ECS019A
00697          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00698                                                                   ECS019A
00699      MOVE SPACES                 TO  SAVE-NAME.                   ECS019A
00700      MOVE W-ACCT                 TO  CUR-ACCT.                    ECS019A
011116     MOVE W-CLP-STATE            TO  CUR-ACCT-ST
00701      IF CUR-SEQ = W-SEQ                                           ECS019A
00702          GO TO 0380-INTL-ACCT.                                    ECS019A
00703                                                                   ECS019A
00704  0270-ST-BREAK.                                                   ECS019A
00705 *  TOTAL LEVEL 2.                                                 ECS019A
00706      MOVE ST-HD2                 TO  SUB-HD2.                     ECS019A
00707      MOVE ST-HD3                 TO  SUB-HD3.                     ECS019A
00708      MOVE ST-TOTALS              TO  X-TOTALS.                    ECS019A
00709      MOVE CO-TOTALS              TO  R-TOTALS.                    ECS019A
00710      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019A
00711      MOVE R-TOTALS               TO  CO-TOTALS.                   ECS019A
00712                                                                   ECS019A
00713      IF DTE-PGM-OPT NOT GREATER THAN '2'                          ECS019A
00714          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00715                                                                   ECS019A
00716      MOVE W-ST                   TO  CUR-ST.                      ECS019A
020305     MOVE W-CLP-STATE            TO  CUR-ACCT-ST
00717      IF CUR-SEQ = W-SEQ                                           ECS019A
00718          GO TO 0360-INTL-ST.                                      ECS019A
00719                                                                   ECS019A
00720  0280-CO-BREAK.                                                   ECS019A
00721 *  TOTAL LEVEL 3.                                                 ECS019A
00722      MOVE CO-HD2                 TO  SUB-HD2.                     ECS019A
00723      MOVE CO-HD3                 TO  SUB-HD3.                     ECS019A
00724      MOVE CO-TOTALS              TO  X-TOTALS.                    ECS019A
00725      MOVE CARR-TOTALS            TO  R-TOTALS.                    ECS019A
00726      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019A
00727      MOVE R-TOTALS               TO  CARR-TOTALS.                 ECS019A
00728                                                                   ECS019A
00729      IF DTE-PGM-OPT NOT GREATER THAN '3'                          ECS019A
00730          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00731                                                                   ECS019A
00732      GO TO 0350-INTL-CO.                                          ECS019A
00733                                                                   ECS019A
00734  0290-CARR-SUMMARY.                                               ECS019A
00735 *  TOTAL LEVEL 4.                                                 ECS019A
121506     MOVE 'CARRIER, STATE SUMMARY'
121506                                 TO SUB-HD2
00737      MOVE HD-SUMM-CARR-DESC      TO  SUB-HD3.                     ECS019A
00738                                                                   ECS019A
00739      IF DTE-PGM-OPT NOT GREATER THAN '4'                          ECS019A
00740          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00741                                                                   ECS019A
00742      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00743          IF SRT-REPORT-CD-1 NOT EQUAL SAVE-REPORT-CD-1            ECS019A
00744              GO TO 0300-CARR-BREAK.                               ECS019A
00745                                                                   ECS019A
00746      IF W-CARR = CUR-CARR                                         ECS019A
00747          GO TO 0350-INTL-CO.                                      ECS019A
00748                                                                   ECS019A
00749  0300-CARR-BREAK.                                                 ECS019A
00750 *  TOTAL LEVEL 5.                                                 ECS019A
00751      MOVE CARR-HD2               TO  SUB-HD2.                     ECS019A
121506     MOVE 'CARRIER SUMMARY'      TO SUB-HD2
00752      MOVE CARR-HD3               TO  SUB-HD3.                     ECS019A
00753      MOVE CARR-TOTALS            TO  X-TOTALS.                    ECS019A
00754                                                                   ECS019A
00755      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00756          MOVE RPT-CD-1-TOTALS    TO  R-TOTALS                     ECS019A
00757      ELSE                                                         ECS019A
00758          MOVE FINAL-TOTALS       TO  R-TOTALS.                    ECS019A
00759                                                                   ECS019A
00760      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019A
00761                                                                   ECS019A
00762      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00763          MOVE R-TOTALS           TO  RPT-CD-1-TOTALS              ECS019A
00764      ELSE                                                         ECS019A
00765          MOVE R-TOTALS           TO  FINAL-TOTALS.                ECS019A
00766                                                                   ECS019A
00767      IF DTE-PGM-OPT NOT GREATER THAN '5'                          ECS019A
00768          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00769                                                                   ECS019A
00770      IF DTE-TOT-OPT EQUAL 2                                       ECS019A
00771          IF SRT-REPORT-CD-1 NOT EQUAL SAVE-REPORT-CD-1            ECS019A
00772              GO TO 0305-REPORT-CD-1-BREAK.                        ECS019A
00773                                                                   ECS019A
00774      GO TO 0340-INTL-CARR.                                        ECS019A
00775                                                                   ECS019A
00776  0305-REPORT-CD-1-BREAK.                                          ECS019A
00777 *  TOTAL LEVEL 6.                                                 ECS019A
00778      MOVE SPACES                 TO  SUB-HD2.                     ECS019A
00779      MOVE SPACES                 TO  SUB-HD3.                     ECS019A
00780      MOVE RPT-CD-1-TOTALS        TO  X-TOTALS.                    ECS019A
00781      MOVE FINAL-TOTALS           TO  R-TOTALS.                    ECS019A
00782      PERFORM 0640-ROLL-RTN THRU 0680-ROLL-XIT.                    ECS019A
00783      MOVE R-TOTALS               TO  FINAL-TOTALS.                ECS019A
00784                                                                   ECS019A
00785      PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                      ECS019A
00786                                                                   ECS019A
00787      GO TO 0335-INTL-RPT-CD-1.                                    ECS019A
00788                                                                   ECS019A
00789  0310-FINAL-SUMMARY.                                              ECS019A
00790 *  TOTAL LEVEL 7.                                                 ECS019A
00791      MOVE 'FINAL SUMMARY'        TO  SUB-HD2.                     ECS019A
00792      MOVE HD-SUMM-FINAL-DESC     TO  SUB-HD3.                     ECS019A
00793      MOVE SPACES                 TO HD1-RPT-CD-1-HDG              ECS019A
00794                                     HD1-RPT-CD-1 HD1-FILL.        ECS019A
00795                                                                   ECS019A
00796      IF DTE-PGM-OPT NOT GREATER THAN '6'                          ECS019A
00797          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00798                                                                   ECS019A
00799      IF FST-SW NOT = 2                                            ECS019A
00800          GO TO 0340-INTL-CARR.                                    ECS019A
00801                                                                   ECS019A
00802  0320-FINAL-BREAK.                                                ECS019A
00803 *  TOTAL LEVEL 7.                                                 ECS019A
00804      MOVE 'FINAL TOTALS'         TO  SUB-HD2.                     ECS019A
00805      MOVE SPACES                 TO  SUB-HD3.                     ECS019A
00806      MOVE FINAL-TOTALS           TO  X-TOTALS.                    ECS019A
00807                                                                   ECS019A
00808      IF DTE-FICH = '1'                                            ECS019A
00809          MOVE '2'                TO DTE-FICH.                     ECS019A
00810                                                                   ECS019A
00811      IF DTE-PGM-OPT NOT GREATER THAN '7'                          ECS019A
00812          PERFORM 0460-BLD-RTN THRU 0490-BLD-XIT.                  ECS019A
00813                                                                   ECS019A
00814      GO TO 0410-BREAK-XIT.                                        ECS019A
00815                                                                   ECS019A
00816  0330-INTL-FINAL.                                                 ECS019A
00817      PERFORM 0600-ZERO-RTN THRU 0630-ZERO-XIT.                    ECS019A
00818      MOVE X-ZERO                 TO  FINAL-TOTALS.                ECS019A
00819                                                                   ECS019A
00820  0335-INTL-RPT-CD-1.                                              ECS019A
00821      MOVE SRT-REPORT-CD-1        TO  SAVE-REPORT-CD-1             ECS019A
00822                                      HD-RPT-CD-1                  ECS019A
00823                                      HD1-RPT-CD-1.                ECS019A
00824      MOVE X-ZERO                 TO  RPT-CD-1-TOTALS.             ECS019A
00825                                                                   ECS019A
00826  0340-INTL-CARR.                                                  ECS019A
00827                                                                   ECS019A
00828      MOVE W-CARR        TO CUR-CARR HD-CARR HD-SUMM-CARR.         ECS019A
00829                                                                   ECS019A
00830      MOVE X-ZERO                 TO  CARR-TOTALS.                 ECS019A
00831                                                                   ECS019A
00832  0350-INTL-CO.                                                    ECS019A
00833      MOVE W-GROUP                TO  CUR-GROUP HD-GROUP.          ECS019A
00834      MOVE X-ZERO                 TO  CO-TOTALS.                   ECS019A
00835                                                                   ECS019A
00836  0360-INTL-ST.                                                    ECS019A
00837      MOVE W-ST                   TO  CUR-ST HD-ST                 ECS019A
00838                                      HD-SUMM-ST  STATE-L.         ECS019A
020305     MOVE W-CLP-STATE            TO  CUR-ACCT-ST
00839      MOVE X-ZERO                 TO  ST-TOTALS.                   ECS019A
00840                                                                   ECS019A
00841      IF CLAS-MAXS NOT GREATER ZEROS                               ECS019A
00842          MOVE 'INVALID STATE'    TO  HD-ST-NM HD-SUMM-ST-NM       ECS019A
00843              GO TO 0380-INTL-ACCT.                                ECS019A
00844                                                                   ECS019A
00845      MOVE CLAS-STARTS            TO  CLAS-INDEXS.                 ECS019A
00846      EJECT                                                        ECS019A
00847  0370-FIND-ST-DESC.                                               ECS019A
00848      IF CLAS-INDEXS GREATER CLAS-MAXS                             ECS019A
00849          MOVE 'INVALID STATE'    TO  HD-ST-NM HD-SUMM-ST-NM       ECS019A
00850              GO TO 0380-INTL-ACCT.                                ECS019A
00851                                                                   ECS019A
00852      IF STATE-SUB (CLAS-INDEXS) NOT = STATE-L                     ECS019A
00853          ADD +1                  TO  CLAS-INDEXS                  ECS019A
00854              GO TO 0370-FIND-ST-DESC.                             ECS019A
00855                                                                   ECS019A
00856      MOVE STATE-PIC (CLAS-INDEXS)    TO  HD-ST-NM HD-SUMM-ST-NM.  ECS019A
00857                                                                   ECS019A
00858  0380-INTL-ACCT.                                                  ECS019A
00859      MOVE W-ACCT                 TO  CUR-ACCT HD-ACCT.            ECS019A
00860      MOVE X-ZERO                 TO  X-TOTALS.                    ECS019A
00861                                                                   ECS019A
00862  0390-INTL-LINE.                                                  ECS019A
00863      MOVE W-IG                   TO  CUR-IG WX-LAH.               ECS019A
00864      MOVE W-TYPE                 TO  CUR-TYPE WX-TYPE.            ECS019A
00865                                                                   ECS019A
00866      IF W-IG LESS THAN 3                                          ECS019A
00867          MOVE 1                  TO  X1                           ECS019A
00868         ELSE                                                      ECS019A
00869          MOVE 2                  TO  X1.                          ECS019A
00870                                                                   ECS019A
00871      IF W-IG = 1 OR 3                                             ECS019A
00872          MOVE 1                  TO  WX-LAH                       ECS019A
00873        ELSE                                                       ECS019A
00874          MOVE 2                  TO  WX-LAH.                      ECS019A
00875                                                                   ECS019A
00876      MOVE 1                      TO  X2.                          ECS019A
00877                                                                   ECS019A
00878  0400-LOOP-LINE.                                                  ECS019A
00879      IF WX-POINTER = X-POINTER (X2)                               ECS019A
00880          MOVE X-AMTS (X1 X2)     TO  X-DETL                       ECS019A
00881          GO TO 0410-BREAK-XIT.                                    ECS019A
00882                                                                   ECS019A
00883      ADD 1                       TO  X2.                          ECS019A
00884                                                                   ECS019A
00885      IF X-TYPE (X2) = HIGH-VALUE                                  ECS019A
00886          DISPLAY 'INVALID TYPE - ' WX-POINTER                     ECS019A
00887          MOVE +0301              TO  WS-RETURN-CODE               ECS019A
00888          MOVE 'INVALID TYPE - '  TO  WS-ABEND-MESSAGE             ECS019A
00889          GO TO ABEND-PGM.                                         ECS019A
00890                                                                   ECS019A
00891      GO TO 0400-LOOP-LINE.                                        ECS019A
00892                                                                   ECS019A
00893  0410-BREAK-XIT.                                                  ECS019A
00894      EXIT.                                                        ECS019A
00895  EJECT                                                            ECS019A
00896  0430-RD-ACCT-RTN.                                                ECS019A
00897                                                                   ECS019A
00898      MOVE SPACES                 TO SAVE-REPORT-CD-1              ECS019A
00899                                     SAVE-NAME.

102501     MOVE SPACES                 TO AM-CONTROL-PRIMARY.

00902      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                ECS019A
00903                                                                   ECS019A
00904      MOVE CUR-CARR               TO AM-CARRIER.                   ECS019A
00905                                                                   ECS019A
00911      MOVE CUR-GROUP              TO AM-GROUPING.                  ECS019A
00912                                                                   ECS019A
00913 *    MOVE CUR-ST                 TO AM-STATE.                     ECS019A
           MOVE CUR-ACCT-ST            TO AM-STATE
00914      MOVE CUR-ACCT               TO AM-ACCOUNT.                   ECS019A
020305     MOVE AM-CONTROL-A           TO SAVE-ACCT-KEY
00915                                                                   ECS019A
           IF AM-CARRIER = '2'
              DISPLAY ' WHAT I AM TRYING TO READ '
              AM-STATE '  ' AM-ACCOUNT
           END-IF
00916      START ERACCTT                                                ECS019A
00917          KEY NOT LESS THAN AM-CONTROL-PRIMARY.                    ECS019A
00918                                                                   ECS019A
00919      IF ERACCT-FILE-STATUS NOT = '00'                             ECS019A
00920          MOVE +0300                        TO  WS-RETURN-CODE     ECS019A
00921          MOVE 'START ERROR ACCT MASTER'    TO  WS-ABEND-MESSAGE   ECS019A
00922          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         ECS019A
00923          GO TO ABEND-PGM.                                         ECS019A
00924                                                                   ECS019A
00925  0440-READ-ERACCTT-NEXT.                                          ECS019A
00926                                                                   ECS019A
00927      READ ERACCTT NEXT.                                           ECS019A
00928                                                                   ECS019A
00929      IF ERACCT-FILE-STATUS = '10'                                 ECS019A
00930          MOVE HIGH-VALUES        TO AM-CONTROL-PRIMARY            ECS019A
00931          GO TO 0440-RD-ACCT-XIT.                                  ECS019A
00932                                                                   ECS019A
00933      IF ERACCT-FILE-STATUS NOT = '00'                             ECS019A
00934          MOVE +0301                        TO  WS-RETURN-CODE     ECS019A
00935          MOVE 'ERROR READING ACCT MASTER'  TO  WS-ABEND-MESSAGE   ECS019A
00936          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         ECS019A
00937          GO TO ABEND-PGM.                                         ECS019A
00938                                                                   ECS019A
00939      IF AM-COMPANY-CD GREATER THAN DTE-CLASIC-COMPANY-CD          ECS019A
00940          MOVE HIGH-VALUES        TO AM-CONTROL-PRIMARY            ECS019A
00941          GO TO 0440-RD-ACCT-XIT.                                  ECS019A
00942                                                                   ECS019A
00943 *    IF CUR-ACCT-CNTL = AM-CONTROL-A                              ECS019A
020305     IF SAVE-ACCT-KEY = AM-CONTROL-A
00944          MOVE AM-REPORT-CODE-1   TO SAVE-REPORT-CD-1              ECS019A
00945          MOVE AM-NAME            TO SAVE-NAME                     ECS019A
00946          GO TO 0440-READ-ERACCTT-NEXT.                            ECS019A
00947                                                                   ECS019A
00948  0440-RD-ACCT-XIT.                                                ECS019A
00949      EXIT.                                                        ECS019A
00950  EJECT                                                            ECS019A
00951  0460-BLD-RTN.                                                    ECS019A
00952      PERFORM 0750-HD-RTN THRU 0760-HD-XIT.                        ECS019A
00964                                                                   ECS019A
020107*    IF IND-SW = 0 OR                                             ECS019A
020107*       GRP-SW = 0                                                ECS019A
020107*        GO TO 0490-BLD-XIT.                                      ECS019A
00968                                                                   ECS019A
00969      MOVE 0                      TO  X2.                          ECS019A
00970                                                                   ECS019A
00971  0470-TOTAL-LOOP.                                                 ECS019A
00972      ADD 1                       TO  X2.                          ECS019A
00973      IF X-POINTER (X2) = HIGH-VALUE                               ECS019A
00974          GO TO 0480-TOTAL-XIT.                                    ECS019A
00975                                                                   ECS019A
00976      MOVE X-AMTS (1 X2)          TO  X-DETL.                      ECS019A
00977      MOVE X-AMTS (2 X2)          TO  R-DETL.                      ECS019A
00978      ADD R-ISSUE                 TO  X-ISSUE.
00979      ADD R-CANCL                 TO  X-CANCL.                     ECS019A
00980      ADD R-BASE                  TO  X-BASE.                      ECS019A
00981      ADD R-OVER                  TO  X-OVER.                      ECS019A
061004     ADD R-DLR-INC               TO  X-DLR-INC.
011410     ADD R-LF-LMBA-FEE           TO  X-LF-LMBA-FEE.
011410     ADD R-AH-LMBA-FEE           TO  X-AH-LMBA-FEE.
061004     ADD R-BANK-FEE              TO  X-BANK-FEE
           ADD R-CSO-ADMIN             TO  X-CSO-ADMIN
00982      ADD R-CLAIM                 TO  X-CLAIM.                     ECS019A
00983      MOVE X-DETL                 TO  X-AMTS (1 X2).               ECS019A
00984      GO TO 0470-TOTAL-LOOP.                                       ECS019A
00985                                                                   ECS019A
00986  0480-TOTAL-XIT.                                                  ECS019A
00987      MOVE 'TOTAL'                TO  HD-DESC.                     ECS019A
00988      MOVE 1                      TO  X1.                          ECS019A
00989      MOVE SPACE-3                TO  P-CCSW.                      ECS019A
00990      PERFORM 0510-PRT-EXTRACT THRU 0570-PRT-EXTRACT-XIT.          ECS019A
00991                                                                   ECS019A
00992  0490-BLD-XIT.                                                    ECS019A
00993      EXIT.                                                        ECS019A
00994  EJECT                                                            ECS019A
00995  0510-PRT-EXTRACT.                                                ECS019A
00996      MOVE 0                      TO  PRT-SW LIFE-SW AH-SW X2.     ECS019A
00997      MOVE X-ZERO-5               TO  SUB-TOTALS.                  ECS019A
00998                                                                   ECS019A
00999  0520-LOOP-LIFE.                                                  ECS019A
01000      ADD 1                       TO  X2.                          ECS019A
01001                                                                   ECS019A
01002      IF X-POINTER (X2) = HIGH-VALUE                               ECS019A
01003          GO TO 0530-OUT-LIFE.                                     ECS019A
01004                                                                   ECS019A
01005      IF X-LAH (X2) NOT = 1                                        ECS019A
01006          SUBTRACT 1 FROM X2                                       ECS019A
01007          GO TO 0530-OUT-LIFE.                                     ECS019A
01008                                                                   ECS019A
01009      MOVE X-AMTS (X1 X2)         TO  X-DETL.                      ECS019A
01010                                                                   ECS019A
01011      IF X-DETL = X-ZERO-5                                         ECS019A
01012          GO TO 0520-LOOP-LIFE.                                    ECS019A
01013                                                                   ECS019A
01014      IF PRT-SW = 0                                                ECS019A
01015          MOVE 1                  TO  PRT-SW                       ECS019A
01016          MOVE 1                  TO  LIFE-SW                      ECS019A
01017          MOVE HD4                TO  P-LN                         ECS019A
01018          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
061004         MOVE HD5                TO  P-LN                         ECS019A
061004         PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
01021          MOVE SPACE-2            TO  P-CCSW.                      ECS019A
01022                                                                   ECS019A
01023      MOVE X-POINTER (X2)         TO  WX-POINTER.                  ECS019A
01024      MOVE X-DESC (X2)            TO  WX-DESC.                     ECS019A
01025                                                                   ECS019A
01026      IF WX-OB = '1'                                               ECS019A
01027          MOVE '1YR'              TO  WX-DESC-OB.                  ECS019A
01028      IF WX-OB = '2'                                               ECS019A
01029          MOVE 'REN'              TO  WX-DESC-OB.                  ECS019A
01030                                                                   ECS019A
01031      ADD X-ISSUE                 TO  S-ISSUE
01032      ADD X-CANCL                 TO  S-CANCL.                     ECS019A
01033      ADD X-BASE                  TO  S-BASE.                      ECS019A
01034      ADD X-OVER                  TO  S-OVER.                      ECS019A
061004     ADD X-DLR-INC               TO  S-DLR-INC.
011410     ADD X-LF-LMBA-FEE           TO  S-LF-LMBA-FEE.
011410     ADD X-AH-LMBA-FEE           TO  S-AH-LMBA-FEE.
061004     ADD X-BANK-FEE              TO  S-BANK-FEE
           ADD X-CSO-ADMIN             TO  S-CSO-ADMIN
01035      ADD X-CLAIM                 TO  S-CLAIM.                     ECS019A
010303     PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019A
01037      GO TO 0520-LOOP-LIFE.                                        ECS019A
01038                                                                   ECS019A
01039  0530-OUT-LIFE.                                                   ECS019A
01040      MOVE SUB-TOTALS             TO  TOTALS.                      ECS019A
01041                                                                   ECS019A
01042      IF LIFE-SW = 0                                               ECS019A
01043          GO TO 0540-LOOP-AH.                                      ECS019A
01044                                                                   ECS019A
01045      MOVE SPACES                 TO  BLD-DESC.                    ECS019A
01046      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS019A
01047      MOVE LIFE-OVERRIDE-L2       TO  BLD-DESC1.                   ECS019A
01048      MOVE SPACES                 TO  B-SLASH1                     ECS019A
01049                                      BLD-DESC2.                   ECS019A
01050      MOVE BLD-DESC               TO  WX-DESC.                     ECS019A
01051      MOVE SPACES                 TO  WX-POINTER.                  ECS019A
01052      MOVE SUB-TOTALS             TO  X-DETL.                      ECS019A
010303     PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019A
01054      MOVE SPACE-2                TO  P-CCSW.                      ECS019A
01055      MOVE X-ZERO-5               TO  SUB-TOTALS.                  ECS019A
01056      MOVE SPACES                 TO  BLD-DESC.                    ECS019A
01057                                                                   ECS019A
01058  0540-LOOP-AH.                                                    ECS019A
01059      ADD 1                       TO  X2.                          ECS019A
01060      IF X-POINTER (X2) = HIGH-VALUE                               ECS019A
01061          GO TO 0550-OUT-AH.                                       ECS019A
01062                                                                   ECS019A
01063      MOVE X-AMTS (X1 X2)         TO  X-DETL.                      ECS019A
01064      IF X-ZERO-5 = X-DETL                                         ECS019A
01065          GO TO 0540-LOOP-AH.                                      ECS019A
01066                                                                   ECS019A
01067      IF PRT-SW = 0                                                ECS019A
01068          MOVE 1                  TO  PRT-SW                       ECS019A
01069          MOVE 1                  TO  AH-SW                        ECS019A
01070          MOVE HD4                TO  P-LN                         ECS019A
01071          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
061004         MOVE HD5                TO  P-LN                         ECS019A
061004         PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
01074          MOVE SPACE-2            TO  P-CCSW.                      ECS019A
01075                                                                   ECS019A
01076      IF AH-SW = 0                                                 ECS019A
01077          MOVE 1                  TO  AH-SW.                       ECS019A
01078                                                                   ECS019A
01079      MOVE X-POINTER (X2)         TO  WX-POINTER.                  ECS019A
01080      MOVE X-DESC (X2)            TO  WX-DESC.                     ECS019A
01081                                                                   ECS019A
01082      IF WX-OB = '1'                                               ECS019A
01083          MOVE '1YR'              TO  WX-DESC-OB.                  ECS019A
01084      IF WX-OB = '2'                                               ECS019A
01085          MOVE 'REN'              TO  WX-DESC-OB.                  ECS019A
01086                                                                   ECS019A
01087      ADD X-ISSUE                 TO  S-ISSUE
01088      ADD X-CANCL                 TO  S-CANCL.                     ECS019A
01089      ADD X-BASE                  TO  S-BASE.                      ECS019A
01090      ADD X-OVER                  TO  S-OVER.                      ECS019A
061004     ADD X-DLR-INC               TO  S-DLR-INC.
011410     ADD X-LF-LMBA-FEE           TO  S-LF-LMBA-FEE.
011410     ADD X-AH-LMBA-FEE           TO  S-AH-LMBA-FEE.
061004     ADD X-BANK-FEE              TO  S-BANK-FEE
           ADD X-CSO-ADMIN             TO  S-CSO-ADMIN
01091      ADD X-CLAIM                 TO  S-CLAIM.                     ECS019A
010303     PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019A
01093      GO TO 0540-LOOP-AH.                                          ECS019A
01094                                                                   ECS019A
01095  0550-OUT-AH.                                                     ECS019A
01096      IF AH-SW = 0                                                 ECS019A
01097          GO TO 0570-PRT-EXTRACT-XIT.                              ECS019A
01098                                                                   ECS019A
01099      MOVE SPACES                 TO  BLD-DESC.                    ECS019A
01100      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS019A
01101      MOVE AH-OVERRIDE-L2         TO  BLD-DESC1.                   ECS019A
01102      MOVE SPACES                 TO  B-SLASH1                     ECS019A
01103                                      BLD-DESC2.                   ECS019A
01104      MOVE BLD-DESC               TO  WX-DESC.                     ECS019A
01105      MOVE SPACES                 TO  WX-POINTER.                  ECS019A
01106      MOVE SUB-TOTALS             TO  X-DETL.                      ECS019A
010303     PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019A
01108      MOVE SPACE-2                TO  P-CCSW.                      ECS019A
01109      ADD S-ISSUE                 TO  T-ISSUE
01110      ADD S-CANCL                 TO  T-CANCL.                     ECS019A
01111      ADD S-BASE                  TO  T-BASE.                      ECS019A
01112      ADD S-OVER                  TO  T-OVER.                      ECS019A
061004     ADD S-DLR-INC               TO  T-DLR-INC.
011410     ADD S-LF-LMBA-FEE           TO  T-LF-LMBA-FEE.
011410     ADD S-AH-LMBA-FEE           TO  T-AH-LMBA-FEE.
061004     ADD S-BANK-FEE              TO  T-BANK-FEE
           ADD S-CSO-ADMIN             TO  T-CSO-ADMIN
01113      ADD S-CLAIM                 TO  T-CLAIM.                     ECS019A
01114                                                                   ECS019A
01115  0560-PRT-TOTALS.                                                 ECS019A
01116      IF LIFE-SW = 0                                               ECS019A
01117          GO TO 0570-PRT-EXTRACT-XIT.                              ECS019A
01118                                                                   ECS019A
01119      MOVE SPACES                 TO  BLD-DESC.                    ECS019A
01120      MOVE TOTALS                 TO  X-DETL.                      ECS019A
01121      MOVE 'TOTAL '               TO  BLD-TOTAL.                   ECS019A
01122      MOVE LIFE-OVERRIDE-L2       TO  BLD-DESC1.                   ECS019A
01123      MOVE '/'                    TO  B-SLASH1.                    ECS019A
01124      MOVE AH-OVERRIDE-L2         TO  BLD-DESC2.                   ECS019A
011116     IF DTE-CLIENT = 'DCC' or 'CAP'
010303        MOVE 'TOTAL CLP '        TO  BLD-DESC
010303     END-IF
010303
01125      MOVE BLD-DESC               TO  WX-DESC.                     ECS019A
01126      MOVE SPACES                 TO  WX-POINTER.                  ECS019A
01127      PERFORM 0580-PRT-LINE THRU 0590-PRT-LINE-XIT.                ECS019A
01128                                                                   ECS019A
01129  0570-PRT-EXTRACT-XIT.                                            ECS019A
01130      EXIT.                                                        ECS019A
01131      EJECT                                                        ECS019A
01132  0580-PRT-LINE.                                                   ECS019A
01133      IF LNCTR GREATER THAN +058                                   ECS019A
01134          PERFORM 0750-HD-RTN THRU 0760-HD-XIT                     ECS019A
01135          MOVE HD4                TO  P-LN                         ECS019A
01136          MOVE SPACE-3            TO  P-CCSW                       ECS019A
01137          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
061004         MOVE HD5                TO  P-LN                         ECS019A
061004         PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
01140          MOVE SPACE-2            TO  P-CCSW.                      ECS019A
01141                                                                   ECS019A
01142      MOVE WX-TYP                 TO  P-TYPE.                      ECS019A
01143      MOVE WX-DESC                TO  P-DESC.                      ECS019A

           IF X-CANCL < ZEROS
              COMPUTE X-CANCL = X-CANCL * -1
           END-IF
010303     COMPUTE P-ISSUE = X-ISSUE - X-CANCL - X-BASE

010303
01147      MOVE X-OVER                 TO  P-OVER.                      ECS019A
011410     COMPUTE P-LMBA-FEE = X-LF-LMBA-FEE + X-AH-LMBA-FEE
061004     MOVE X-DLR-INC              TO  P-DLR-INC.
061004     MOVE X-BANK-FEE             TO  P-BANK-FEE
           MOVE X-CSO-ADMIN            TO  P-CSO-ADMIN
01148                                                                   ECS019A
01149      IF SUB-HD2 = 'FINAL TOTALS' AND                              ECS019A
01150          BLD-TOTAL = 'TOTAL ' AND                                 ECS019A
01151          BLD-DESC1 = LIFE-OVERRIDE-L2 AND                         ECS019A
01152          BLD-DESC2 = SPACES                                       ECS019A
01153              MOVE X-ISSUE        TO  HLD-019-PREM-L               ECS019A
01154              MULTIPLY X-CANCL BY -1 GIVING HLD-019-REF-L          ECS019A
01155              MOVE X-BASE         TO  HLD-019-COMM-L               ECS019A
01156              MOVE X-OVER         TO  HLD-019-OR-L.                ECS019A
01157                                                                   ECS019A
01158      IF SUB-HD2 = 'FINAL TOTALS' AND                              ECS019A
01159          BLD-TOTAL = 'TOTAL ' AND                                 ECS019A
01160          BLD-DESC1 = AH-OVERRIDE-L2 AND                           ECS019A
01161          BLD-DESC2 = SPACES                                       ECS019A
01162              MOVE X-ISSUE        TO  HLD-019-PREM-AH              ECS019A
01163              MULTIPLY -1 BY X-CANCL GIVING HLD-019-REF-AH         ECS019A
01164              MOVE X-BASE         TO  HLD-019-COMM-AH              ECS019A
01165              MOVE X-OVER         TO  HLD-019-OR-AH.               ECS019A
01166                                                                   ECS019A
01167      MOVE X-CLAIM                TO  P-CLAIM.                     ECS019A
01168      ADD X-ISSUE X-CANCL GIVING X-NET.                            ECS019A
01169      ADD X-BASE X-OVER GIVING X-TOTAL.                            ECS019A
01172      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019A
01173                                                                   ECS019A
01174  0590-PRT-LINE-XIT.                                               ECS019A
01175      EXIT.                                                        ECS019A
01176      EJECT                                                        ECS019A
01177  0600-ZERO-RTN.                                                   ECS019A
01178      MOVE +0                     TO  X2
                                           X-ISSUE
011410                                     X-SPPDD-CLP 
                                           X-CANCL  
061004                                     X-DLR-INC
011410                                     X-LF-LMBA-FEE
011410                                     X-AH-LMBA-FEE
061004                                     X-BANK-FEE
                                           X-CSO-ADMIN
01179                                      X-BASE
                                           X-OVER
                                           X-CLAIM.

01180      MOVE X-DETL                 TO  X-ZERO-5.                    ECS019A
01181                                                                   ECS019A
01182  0610-ZERO-LOOP.                                                  ECS019A
01183      ADD 1                       TO  X2.                          ECS019A
092602     IF X2 GREATER THAN 900                                       ECS019A
01185          GO TO 0620-ZERO-FILL.                                    ECS019A
01186      MOVE X-ZERO-5               TO  X-AMTS (1 X2)                ECS019A
01187                                      X-AMTS (2 X2).               ECS019A
01188      GO TO 0610-ZERO-LOOP.                                        ECS019A
01189                                                                   ECS019A
01190  0620-ZERO-FILL.                                                  ECS019A
01191      MOVE X-TOTALS               TO  X-ZERO.                      ECS019A
01192                                                                   ECS019A
01193  0630-ZERO-XIT.                                                   ECS019A
01194      EXIT.                                                        ECS019A
01195                                                                   ECS019A
01196  0640-ROLL-RTN.                                                   ECS019A
01197      MOVE 0                      TO  X2.                          ECS019A
01198                                                                   ECS019A
01199  0650-LOOP-X2.                                                    ECS019A
01200      ADD 1                       TO  X2.                          ECS019A
01201      IF X-POINTER (X2) = HIGH-VALUE                               ECS019A
01202          GO TO 0680-ROLL-XIT.                                     ECS019A
01203                                                                   ECS019A
01204  0660-ROLL-INDIVIDUAL.                                            ECS019A
01205      MOVE X-AMTS (1 X2)          TO  X-DETL.                      ECS019A
01206      IF X-DETL = X-ZERO-5                                         ECS019A
01207          GO TO 0670-ROLL-GROUP.                                   ECS019A
01208                                                                   ECS019A
01209      MOVE R-AMTS (1 X2)          TO  R-DETL.                      ECS019A
01210      ADD X-ISSUE                 TO  R-ISSUE
01211      ADD X-CANCL                 TO  R-CANCL.                     ECS019A
01212      ADD X-BASE                  TO  R-BASE.                      ECS019A
01213      ADD X-OVER                  TO  R-OVER.                      ECS019A
061004     ADD X-DLR-INC               TO  R-DLR-INC.
011410     ADD X-LF-LMBA-FEE           TO  R-LF-LMBA-FEE.
011410     ADD X-AH-LMBA-FEE           TO  R-AH-LMBA-FEE.
061004     ADD X-BANK-FEE              TO  R-BANK-FEE
           ADD X-CSO-ADMIN             TO  R-CSO-ADMIN
01214      ADD X-CLAIM                 TO  R-CLAIM.                     ECS019A
01215      MOVE R-DETL                 TO  R-AMTS (1 X2).               ECS019A
01216                                                                   ECS019A
01217  0670-ROLL-GROUP.                                                 ECS019A
01218      MOVE X-AMTS (2 X2)          TO  X-DETL.                      ECS019A
01219      IF X-DETL = X-ZERO-5                                         ECS019A
01220          GO TO 0650-LOOP-X2.                                      ECS019A
01221                                                                   ECS019A
01222      MOVE R-AMTS (2 X2)          TO  R-DETL.                      ECS019A
01223      ADD X-ISSUE                 TO  R-ISSUE
01224      ADD X-CANCL                 TO  R-CANCL.                     ECS019A
01225      ADD X-BASE                  TO  R-BASE.                      ECS019A
01226      ADD X-OVER                  TO  R-OVER.                      ECS019A
061004     ADD X-DLR-INC               TO  R-DLR-INC.
011410     ADD X-LF-LMBA-FEE           TO  R-LF-LMBA-FEE.
011410     ADD X-AH-LMBA-FEE           TO  R-AH-LMBA-FEE.
061004     ADD X-BANK-FEE              TO  R-BANK-FEE
           ADD X-CSO-ADMIN             TO  R-CSO-ADMIN
01227      ADD X-CLAIM                 TO  R-CLAIM.                     ECS019A
01228      MOVE R-DETL                 TO  R-AMTS (2 X2).               ECS019A
01229      GO TO 0650-LOOP-X2.                                          ECS019A
01230                                                                   ECS019A
01231  0680-ROLL-XIT.                                                   ECS019A
01232      EXIT.                                                        ECS019A
01233      EJECT                                                        ECS019A
01234  0690-FORMAT-RTN.                                                 ECS019A
01235      MOVE 0 TO X2.                                                ECS019A
01236      MOVE CLAS-STARTL            TO  CLAS-INDEXL.                 ECS019A
01237      MOVE CLAS-STARTA            TO  CLAS-INDEXA.                 ECS019A
01238      MOVE HIGH-VALUE             TO  X-POINTERS.                  ECS019A
01239      MOVE SPACES                 TO  X-DESCRIPTIONS.              ECS019A
01240      IF CLAS-MAXL = ZEROES                                        ECS019A
01241          GO TO 0710-FORMAT-AH-RTN.                                ECS019A
01242                                                                   ECS019A
01243  0700-FORMAT-LIFE.                                                ECS019A
01244      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        ECS019A
01245          GO TO 0710-FORMAT-AH-RTN.                                ECS019A
01246                                                                   ECS019A
01247      ADD 1                       TO  X2.                          ECS019A
01248      MOVE CLAS-I-BEN (CLAS-INDEXL)   TO  X-TYP (X2).              ECS019A
01249      MOVE 1                      TO  X-LAH (X2).                  ECS019A
01250      MOVE SPACES                 TO  X-OB (X2).                   ECS019A
01251      MOVE CLAS-I-AB10 (CLAS-INDEXL)  TO  X-DESC (X2).             ECS019A
01252                                                                   ECS019A
01253      IF CLAS-I-BAL (CLAS-INDEXL) NOT = 'B'                        ECS019A
01254          ADD 1                   TO  CLAS-INDEXL                  ECS019A
01255          GO TO 0700-FORMAT-LIFE.                                  ECS019A
01256                                                                   ECS019A
01257      MOVE '1'                    TO  X-OB (X2).                   ECS019A
01258      MOVE X2                     TO  SAVE-X2.                     ECS019A
01259      ADD 1                       TO  X2.                          ECS019A
01260      MOVE X-POINTER (SAVE-X2)    TO  X-POINTER (X2).              ECS019A
01261      MOVE X-DESC (SAVE-X2)       TO  X-DESC (X2).                 ECS019A
01262      MOVE '2'                    TO  X-OB (X2).                   ECS019A
01263      ADD 1                       TO  CLAS-INDEXL.                 ECS019A
01264      GO TO 0700-FORMAT-LIFE.                                      ECS019A
01265                                                                   ECS019A
01266  0710-FORMAT-AH-RTN.                                              ECS019A
01267      IF CLAS-MAXA = ZEROES                                        ECS019A
01268          GO TO 0730-FORMAT-SET.                                   ECS019A
01269                                                                   ECS019A
01270  0720-FORMAT-AH.                                                  ECS019A
01271      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        ECS019A
01272          GO TO 0730-FORMAT-SET.                                   ECS019A
01273                                                                   ECS019A
01274      ADD 1                       TO  X2.                          ECS019A
01275      MOVE CLAS-I-BEN (CLAS-INDEXA)   TO  X-TYP (X2).              ECS019A
01276      MOVE 2                      TO  X-LAH (X2).                  ECS019A
01277      MOVE SPACES                 TO  X-OB (X2).                   ECS019A
01278      MOVE CLAS-I-AB10 (CLAS-INDEXA)  TO  X-DESC (X2).             ECS019A
01279                                                                   ECS019A
01280      IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                        ECS019A
01281          ADD 1                   TO  CLAS-INDEXA                  ECS019A
01282          GO TO 0720-FORMAT-AH.                                    ECS019A
01283                                                                   ECS019A
01284      MOVE '1'                    TO  X-OB (X2).                   ECS019A
01285      MOVE X2                     TO  SAVE-X2.                     ECS019A
01286      ADD 1                       TO  X2.                          ECS019A
01287      MOVE X-POINTER (SAVE-X2)    TO  X-POINTER (X2).              ECS019A
01288      MOVE '2'                    TO  X-OB (X2).                   ECS019A
01289      MOVE X-DESC (SAVE-X2)       TO  X-DESC (X2).                 ECS019A
01290      ADD 1                       TO  CLAS-INDEXA.                 ECS019A
01291      GO TO 0720-FORMAT-AH.                                        ECS019A
01292                                                                   ECS019A
01293  0730-FORMAT-SET.                                                 ECS019A
01294                                                                   ECS019A
092602     IF X2 GREATER THAN 900                                       ECS019A
01296          MOVE +0201                     TO  WS-RETURN-CODE        ECS019A
01297          MOVE 'PROGRAM TABLE EXCEEDED'  TO  WS-ABEND-MESSAGE      ECS019A
01298          GO TO ABEND-PGM.                                         ECS019A
01299                                                                   ECS019A
01300  0740-FORMAT-XIT.                                                 ECS019A
01301      EXIT.                                                        ECS019A
01302      EJECT                                                        ECS019A
01303  0750-HD-RTN.                                                     ECS019A
01304      ADD +1                      TO  PGCTR.                       ECS019A
01305      MOVE PGCTR                  TO  HD-PAGE.                     ECS019A
01306      MOVE HD1                    TO  P-LN.                        ECS019A
01307      MOVE SPACE-N                TO  P-CCSW.                      ECS019A
01308      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019A
01309      MOVE HD2                    TO  P-LN.                        ECS019A
01310      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019A
01311      MOVE HD3                    TO  P-LN.                        ECS019A
01312      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019A
01313      MOVE SAVE-NAME              TO  P-LN.                        ECS019A
01314      PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT.                      ECS019A
01315      MOVE +5                     TO  LNCTR.                       ECS019A
01316                                                                   ECS019A
01317  0760-HD-XIT.                                                     ECS019A
01318      EXIT.                                                        ECS019A
01319                                                                   ECS019A
01320  0770-PRT-RTN.                                                    ECS019A
01321      MOVE P-CCSW                 TO  X P-CTL.                     ECS019A
01322      MOVE P-LN                   TO  P-DATA.                      ECS019A
01323      MOVE SPACE-1                TO  P-REC.                       ECS019A
01324                                                                   ECS019A
01325      IF X = SPACE-1                                               ECS019A
01326          ADD 1                   TO  LNCTR                        ECS019A
01327      ELSE                                                         ECS019A
01328          IF X = SPACE-2                                           ECS019A
01329              ADD 2               TO  LNCTR                        ECS019A
01330          ELSE                                                     ECS019A
01331              IF X = SPACE-3                                       ECS019A
01332                  ADD 3           TO  LNCTR.                       ECS019A
01333                                                                   ECS019A
01334  0780-PRT-COPY.                                                   ECS019A
121506                                 COPY PRTN019DCC.
01336  0790-PRT-XIT.                                                    ECS019A
01337      EXIT.                                                        ECS019A
01338      EJECT                                                        ECS019A
01339  0800-END-OUTPUT.                                                 ECS019A
01340      IF FST-SW = +0                                               ECS019A
01341          PERFORM 0750-HD-RTN THRU 0760-HD-XIT                     ECS019A
01342          MOVE 'NO PREMIUM OR COMMISSION TRANSACTIONS' TO  P-LN    ECS019A
01343          MOVE SPACE-2            TO  P-CCSW                       ECS019A
01344          PERFORM 0770-PRT-RTN THRU 0790-PRT-XIT                   ECS019A
01345      ELSE
102501         MOVE SPACES             TO  W-SEQ
01347          MOVE HIGH-VALUES        TO  SRT-REPORT-CD-1              ECS019A
01348          MOVE 2                  TO  FST-SW                       ECS019A
01349          PERFORM 0230-BREAK-RTN THRU 0410-BREAK-XIT.              ECS019A
01350                                                                   ECS019A
01351      CLOSE ERACCTT.                                               ECS019A
01352                                                                   ECS019A
01353  0810-OUTPUT-XIT.                                                 ECS019A
01354      EXIT.                                                        ECS019A
01355                                                                   ECS019A
01356  0820-END-OF-JOB SECTION.                                         ECS019A
01357                                                                   ECS019A
01358   0830-EOJ-RTN.                                                   ECS019A
01359                                  COPY ELCPRTC.                    ECS019A
01360      CLOSE PRNTR.                                                 ECS019A
01361                                                                   ECS019A
01362      OPEN I-O ERMEBL.                                             ECS019A
01363                                                                   ECS019A
01364      IF ERMEBL-FILE-STATUS NOT = ZERO AND '97'                    ECS019A
01365          MOVE 'N'                TO  ME-UPDATE-FLAG.              ECS019A
01366                                                                   ECS019A
01367      MOVE DTE-CLIENT             TO  ME-COMPANY.                  ECS019A
01368      COMPUTE MONTH-END-MOYR = RUN-CCYY * 12 + RUN-MO.             ECS019A
01369      MOVE MONTH-END-MOYR         TO  ME-MOYR.                     ECS019A
01370                                                                   ECS019A
01371      IF ME-DO-UPDATE                                              ECS019A
01372          READ ERMEBL INVALID KEY                                  ECS019A
01373          MOVE 'N'                TO  ME-UPDATE-FLAG               ECS019A
01374          CLOSE ERMEBL.                                            ECS019A
01375                                                                   ECS019A
01376      IF ME-DO-UPDATE                                              ECS019A
01377          MOVE HLD-019-PREM-L     TO  ME-019-PREM-L                ECS019A
01378          MOVE HLD-019-PREM-AH    TO  ME-019-PREM-AH               ECS019A
01379          MOVE HLD-019-REF-L      TO  ME-019-REF-L                 ECS019A
01380          MOVE HLD-019-REF-AH     TO  ME-019-REF-AH                ECS019A
01381          MOVE HLD-019-COMM-L     TO  ME-019-COMM-L                ECS019A
01382          MOVE HLD-019-COMM-AH    TO  ME-019-COMM-AH               ECS019A
01383          MOVE HLD-019-OR-L       TO  ME-019-OR-L                  ECS019A
01384          MOVE HLD-019-OR-AH      TO  ME-019-OR-AH                 ECS019A
011116*        MOVE ME-START-TIME      TO  ME-019-START                 ECS019A
01386          MOVE ME-CNDS-DATE       TO  ME-019-RUN-DT                ECS019A
01387          ACCEPT WS-TIME-OF-DAY   FROM TIME                        ECS019A
011116*        MOVE WS-TIME            TO  ME-019-END                   ECS019A
01389          ADD 1                   TO  ME-019-RUN-CT.               ECS019A
01390                                                                   ECS019A
01391      IF ME-DO-UPDATE                                              ECS019A
01392          REWRITE MONTH-END-BALANCES                               ECS019A
01393          CLOSE ERMEBL.                                            ECS019A
01394                                                                   ECS019A
01395      GOBACK.                                                      ECS019A
01396                                                                   ECS019A
01397                                                                   ECS019A
01398  ABEND-PGM.                                                       ECS019A
01399                                  COPY ELCABEND SUPPRESS.          ECS019A
01400 /                                                                 ECS019A
01401  LCP-WRITE-POS-PRT SECTION.                                       ECS019A
01402      IF LCP-ASA = '+'                                             ECS019A
01403          WRITE PRT AFTER 0 LINE                                   ECS019A
01404      ELSE                                                         ECS019A
01405      IF LCP-ASA = ' '                                             ECS019A
01406          WRITE PRT AFTER ADVANCING 1 LINE                         ECS019A
01407      ELSE                                                         ECS019A
01408      IF LCP-ASA = '0'                                             ECS019A
01409          WRITE PRT AFTER ADVANCING 2 LINE                         ECS019A
01410      ELSE                                                         ECS019A
01411      IF LCP-ASA = '-'                                             ECS019A
01412          WRITE PRT AFTER ADVANCING 3 LINE                         ECS019A
01413      ELSE                                                         ECS019A
01414      IF LCP-ASA = '1'                                             ECS019A
01415          WRITE PRT AFTER ADVANCING PAGE                           ECS019A
01416      ELSE                                                         ECS019A
01417      IF LCP-ASA = '2'                                             ECS019A
01418          WRITE PRT AFTER ADVANCING LCP-CH2                        ECS019A
01419      ELSE                                                         ECS019A
01420      IF LCP-ASA = '3'                                             ECS019A
01421          WRITE PRT AFTER ADVANCING LCP-CH3                        ECS019A
01422      ELSE                                                         ECS019A
01423      IF LCP-ASA = '4'                                             ECS019A
01424          WRITE PRT AFTER ADVANCING LCP-CH4                        ECS019A
01425      ELSE                                                         ECS019A
01426      IF LCP-ASA = '5'                                             ECS019A
01427          WRITE PRT AFTER ADVANCING LCP-CH5                        ECS019A
01428      ELSE                                                         ECS019A
01429      IF LCP-ASA = '6'                                             ECS019A
01430          WRITE PRT AFTER ADVANCING LCP-CH6                        ECS019A
01431      ELSE                                                         ECS019A
01432      IF LCP-ASA = '7'                                             ECS019A
01433          WRITE PRT AFTER ADVANCING LCP-CH7                        ECS019A
01434      ELSE                                                         ECS019A
01435      IF LCP-ASA = '8'                                             ECS019A
01436          WRITE PRT AFTER ADVANCING LCP-CH8                        ECS019A
01437      ELSE                                                         ECS019A
01438      IF LCP-ASA = '9'                                             ECS019A
01439          WRITE PRT AFTER ADVANCING LCP-CH9                        ECS019A
01440      ELSE                                                         ECS019A
01441      IF LCP-ASA = 'A'                                             ECS019A
01442          WRITE PRT AFTER ADVANCING LCP-CH10                       ECS019A
01443      ELSE                                                         ECS019A
01444      IF LCP-ASA = 'B'                                             ECS019A
01445          WRITE PRT AFTER ADVANCING LCP-CH11                       ECS019A
01446      ELSE                                                         ECS019A
01447      IF LCP-ASA = 'C'                                             ECS019A
01448          WRITE PRT AFTER ADVANCING LCP-CH12                       ECS019A
01449      ELSE                                                         ECS019A
01450      IF LCP-ASA = 'V'                                             ECS019A
01451          WRITE PRT AFTER ADVANCING LCP-P01                        ECS019A
01452      ELSE                                                         ECS019A
01453      IF LCP-ASA = 'W'                                             ECS019A
01454          WRITE PRT AFTER ADVANCING LCP-P02                        ECS019A
01455      ELSE                                                         ECS019A
01456      DISPLAY 'ASA CODE ERROR'.                                    ECS019A
01457  LCP-WRITE-END-PRT.                                               ECS019A
01458      EXIT.                                                        ECS019A
