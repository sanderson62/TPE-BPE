00001  IDENTIFICATION DIVISION.                                         04/24/98
00002                                                                   EL344
00003  PROGRAM-ID.                 EL344YTD.                               LV009
00004 *              PROGRAM CONVERTED BY                               EL344
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL344
00006 *              CONVERSION DATE 02/15/96 19:10:51.                 EL344
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL344
00008 *                            VMOD=2.013                           EL344
00009                                                                   EL344
00009                                                                   EL344
00010 *AUTHOR.     LOGIC, INC.                                          EL344
00011 *            DALLAS, TEXAS.                                       EL344
00012                                                                   EL344
00025 *REMARKS.                                                         EL344
00026 *        PRINTS CLAIM STATUS REPORT BY ACCOUNT.                   EL344
122702******************************************************************
122702*                   C H A N G E   L O G
122702*
122702* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122702*-----------------------------------------------------------------
122702*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122702* EFFECTIVE    NUMBER
122702*-----------------------------------------------------------------
122702* 122702    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
122702*                                FOR DCC
121503* 121503    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
011204* 011204                   SMVA  DO NOT ABEND ON FILE STATUS 23   
062905* 062905    2005062800002  PEMA  ADD YTD PROGRAM OPTION
022106* 022106    2004040700004  PEMA  ADD LIFE CLAIM INTEREST
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
022122* 022122  CR2021100800003  PEMA  Add B and H claim types
122702******************************************************************
00027                                                                   EL344
00028  ENVIRONMENT DIVISION.                                            EL344
00029  INPUT-OUTPUT SECTION.                                            EL344
00030  FILE-CONTROL.                                                    EL344
00031                                                                   EL344
00032      SELECT ELMSTR ASSIGN TO SYS024-FBA1-ELMSTR                   EL344
00033              ORGANIZATION IS INDEXED                              EL344
00034              ACCESS IS SEQUENTIAL                                 EL344
00035              RECORD KEY IS CL-CONTROL-PRIMARY                     EL344
00036              FILE STATUS IS CL-STATUS.                            EL344
00037                                                                   EL344
00038      SELECT ERACCT ASSIGN TO SYS021-FBA1-ERACCT                   EL344
00039              ORGANIZATION IS INDEXED                              EL344
00040              ACCESS IS DYNAMIC                                    EL344
00041              RECORD KEY IS AM-CONTROL-PRIMARY                     EL344
00042              FILE STATUS IS ERACCT-STATUS.                        EL344
00043                                                                   EL344
00044      SELECT ELREPT ASSIGN TO SYS018-FBA1-ELREPT                   EL344
00045              ORGANIZATION IS INDEXED                              EL344
00046              ACCESS IS DYNAMIC                                    EL344
00047              RECORD KEY IS RF-CONTROL-PRIMARY                     EL344
00048              FILE STATUS IS DTE-VSAM-FLAGS.                       EL344
00049                                                                   EL344
00050      SELECT ELTRLR ASSIGN TO SYS022-FBA1-ELTRLR                   EL344
00051              ORGANIZATION IS INDEXED                              EL344
00052              ACCESS IS SEQUENTIAL                                 EL344
00053              RECORD KEY IS AT-CONTROL-PRIMARY                     EL344
00054              FILE STATUS IS AT-STATUS.                            EL344
00055                                                                   EL344
00056      SELECT ELCERT ASSIGN TO SYS023-FBA1-ELCERT                   EL344
00057              ORGANIZATION IS INDEXED                              EL344
00058              ACCESS IS DYNAMIC                                    EL344
00059              RECORD KEY IS CM-CONTROL-PRIMARY                     EL344
00060              FILE STATUS IS CM-STATUS.                            EL344
00061                                                                   EL344
00062      SELECT MPPLCY ASSIGN TO SYS025-FBA1-MPPLCY                   EL344
00063              ORGANIZATION IS INDEXED                              EL344
00064              ACCESS IS DYNAMIC                                    EL344
00065              RECORD KEY IS PM-CONTROL-PRIMARY                     EL344
00066              FILE STATUS IS PM-STATUS.                            EL344
00067                                                                   EL344
00068      SELECT MPPROD ASSIGN TO SYS026-FBA1-MPPROD                   EL344
00069              ORGANIZATION IS INDEXED                              EL344
00070              ACCESS IS DYNAMIC                                    EL344
00071              RECORD KEY IS PD-CONTROL-PRIMARY                     EL344
00072              FILE STATUS IS MPPROD-STATUS.                        EL344
00073                                                                   EL344
00074      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   EL344
00075      SELECT SORT-WORK        ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  EL344
00076      SELECT DISK-DATE        ASSIGN TO SYS019-UT-FBA1-S-SYS019.   EL344
00077      SELECT PRINTER          ASSIGN TO SYS008-UR-1403-S-SYS008.   EL344
00078  EJECT                                                            EL344
00079  DATA DIVISION.                                                   EL344
00080  FILE SECTION.                                                    EL344
00081                                                                   EL344
00082  FD  ELMSTR.                                                      EL344
00083                                                                   EL344
00084                               COPY ELCMSTR.                       EL344
00085  EJECT                                                            EL344
00086  FD  ELTRLR.                                                      EL344
00087                                                                   EL344
00088                               COPY ELCTRLR.                       EL344
00089  EJECT                                                            EL344
00090  FD  ERACCT.                                                      EL344
00091                                                                   EL344
00092                               COPY ERCACCT.                       EL344
00093  EJECT                                                            EL344
00094  FD  ELCERT.                                                      EL344
00095                                                                   EL344
00096                               COPY ELCCERT.                       EL344
00097  EJECT                                                            EL344
00098  FD  MPPLCY.                                                      EL344
00099                               COPY MPCPLCY.                       EL344
00100                                                                   EL344
00101  EJECT                                                            EL344
00102  FD  MPPROD.                                                      EL344
00103                               COPY MPCPROD.                       EL344
00104  EJECT                                                            EL344
00105  SD  SORT-WORK.                                                   EL344
00106                                                                   EL344
00107  01  SORT-REC.                                                    EL344
00108     03  ST-ACCT.                                                  EL344
00109        05  ST-CAR     PIC X.                                      EL344
00110        05  ST-COMP    PIC X(6).                                   EL344
00111        05  ST-STATE   PIC XX.                                     EL344
00112        05  ST-ACC     PIC X(10).                                  EL344
00113     03  ST-NAME.                                                  EL344
00114        05  ST-LNAME   PIC X(12).                                  EL344
00115        05  ST-FNAME   PIC X(8).                                   EL344
00116     03  ST-CLMNO      PIC X(7).                                   EL344
00117     03  ST-CERT       PIC X(11).                                  EL344
00118     03  ST-CLMTYP     PIC X.                                      EL344
00119     03  ST-CLMSTA     PIC X.                                      EL344
00120     03  ST-CLOC       PIC X.                                      EL344
00121     03  ST-NOPMTS     PIC S9(3)     COMP-3.                       EL344
00122     03  ST-TOTCLM     PIC S9(7)V99  COMP-3.                       EL344
00123     03  ST-CID        PIC 9(11)     COMP-3.                       EL344
00124     03  ST-CRD        PIC 9(11)     COMP-3.                          CL**2
00125     03  ST-LMD.                                                   EL344
00126       05  ST-LMD-MO    PIC 99.                                    EL344
00127       05  ST-LMD-DA    PIC 99.                                    EL344
00128       05  ST-LMD-YR    PIC 99.                                       CL**8
00129      03  ST-EFF-DT    PIC XX.                                     EL344
00130      03  ST-LAST-CLOSE-REASON PIC X.                              EL344
00131      03  ST-LAST-CLOSE-DT PIC XX.                                 EL344
00132      03  ST-SYSTEM-IDENTIFIER    PIC X(02).                       EL344
00133      03  ST-REFERENCE-NO         PIC X(20).                       EL344
00134                                                                   EL344
00135                                                                   EL344
00136  EJECT                                                            EL344
00137  FD  PRINTER                                                      EL344
00138                               COPY ELCPRTFD.                      EL344
00139  EJECT                                                            EL344
00140  FD  FICH                                                         EL344
00141                               COPY ELCFCHFD.                      EL344
00142  EJECT                                                            EL344
00143  FD  DISK-DATE                                                    EL344
00144                               COPY ELCDTEFD.                      EL344
00145  EJECT                                                            EL344
00146  FD  ELREPT                                                       EL344
00147                               COPY ELCRPTFD.                      EL344
00148                                                                   EL344
00149                               COPY ELCREPT.                       EL344
00150  EJECT                                                            EL344
00151  WORKING-STORAGE SECTION.                                         EL344
00152  77  FILLER  PIC X(32) VALUE '********************************'.  EL344
00153  77  FILLER  PIC X(32) VALUE '*    EL344 WORKING STORAGE     *'.  EL344
00154  77  FILLER  PIC X(32) VALUE '******** VMOD=2.013 ************'.  EL344
00155                                                                   EL344
       77  ws-cl-rec-cnt          pic s9(5) comp-3 value +0.
00156  77  WS-RUN-BIN-DT          PIC XX     VALUE SPACE.               EL344
00157  77  X                      PIC X      VALUE SPACE.               EL344
00158  77  LAST-ACC               PIC X(10)  VALUE SPACE.               EL344
00159  77  LAST-SYS-ID            PIC X(02)  VALUE SPACES.              EL344
00160  77  OLC-REPORT-NAME        PIC X(5)   VALUE 'EL344'.             EL344
00161  77  WS-ZERO                PIC S9     VALUE ZERO  COMP-3.        EL344
00162  77  TOT-OPEN-LIFE          PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00163  77  TOT-OPEN-AH            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  TOT-OPEN-IU            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  TOT-OPEN-GAP           PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  TOT-OPEN-FAM           PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  TOT-OPEN-BRV           PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  TOT-OPEN-HOS           PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  TOT-OPEN-OTH           PIC S9(7)  VALUE ZERO  COMP-3.
00164  77  TOT-CLOSED-LIFE        PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00165  77  TOT-CLOSED-AH          PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  TOT-CLOSED-IU          PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  TOT-CLOSED-GAP         PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  TOT-CLOSED-FAM         PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  TOT-CLOSED-BRV         PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  TOT-CLOSED-HOS         PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  TOT-CLOSED-OTH         PIC S9(7)  VALUE ZERO  COMP-3.
00166  77  EST-TM-LIFE            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00167  77  EST-TM-AH              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  EST-TM-IU              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  EST-TM-GAP             PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  EST-TM-FAM             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  EST-TM-BRV             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  EST-TM-HOS             PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  EST-TM-OTH             PIC S9(7)  VALUE ZERO  COMP-3.
00168  77  EST-LM-LIFE            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00169  77  EST-LM-AH              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  EST-LM-IU              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  EST-LM-GAP             PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  EST-LM-FAM             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  EST-LM-BRV             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  EST-LM-HOS             PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  EST-LM-OTH             PIC S9(7)  VALUE ZERO  COMP-3.
00170  77  EST-TY-LIFE            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00171  77  EST-TY-AH              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  EST-TY-IU              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  EST-TY-GAP             PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  EST-TY-FAM             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  EST-TY-BRV             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  EST-TY-HOS             PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  EST-TY-OTH             PIC S9(7)  VALUE ZERO  COMP-3.
00172  77  DEN-TM-LIFE            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00173  77  DEN-TM-AH              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  DEN-TM-IU              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  DEN-TM-GAP             PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  DEN-TM-FAM             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  DEN-TM-BRV             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  DEN-TM-HOS             PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  DEN-TM-OTH             PIC S9(7)  VALUE ZERO  COMP-3.
00174  77  DEN-LM-LIFE            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00175  77  DEN-LM-AH              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  DEN-LM-IU              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  DEN-LM-GAP             PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  DEN-LM-FAM             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  DEN-LM-BRV             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  DEN-LM-HOS             PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  DEN-LM-OTH             PIC S9(7)  VALUE ZERO  COMP-3.
00176  77  DEN-TY-LIFE            PIC S9(7)  VALUE ZERO  COMP-3.        EL344
00177  77  DEN-TY-AH              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
122702 77  DEN-TY-IU              PIC S9(7)  VALUE ZERO  COMP-3.        EL344
121503 77  DEN-TY-GAP             PIC S9(7)  VALUE ZERO  COMP-3.        EL344
052614 77  DEN-TY-FAM             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  DEN-TY-BRV             PIC S9(7)  VALUE ZERO  COMP-3.
022122 77  DEN-TY-HOS             PIC S9(7)  VALUE ZERO  COMP-3.
100518 77  DEN-TY-OTH             PIC S9(7)  VALUE ZERO  COMP-3.
00178  77  PAY-TM-LIFE            PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
00179  77  PAY-TM-AH              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
122702 77  PAY-TM-IU              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
121503 77  PAY-TM-GAP             PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
052614 77  PAY-TM-FAM             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  PAY-TM-BRV             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  PAY-TM-HOS             PIC S9(7)V99  VALUE ZERO  COMP-3.
100518 77  PAY-TM-OTH             PIC S9(7)V99  VALUE ZERO  COMP-3.
00180  77  PAY-LM-LIFE            PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
00181  77  PAY-LM-AH              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
122702 77  PAY-LM-IU              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
121503 77  PAY-LM-GAP             PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
052614 77  PAY-LM-FAM             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  PAY-LM-BRV             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  PAY-LM-HOS             PIC S9(7)V99  VALUE ZERO  COMP-3.
100518 77  PAY-LM-OTH             PIC S9(7)V99  VALUE ZERO  COMP-3.
00182  77  FIN-TM-LIFE            PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
00183  77  FIN-TM-AH              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
122702 77  FIN-TM-IU              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
121503 77  FIN-TM-GAP             PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
052614 77  FIN-TM-FAM             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  FIN-TM-BRV             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  FIN-TM-HOS             PIC S9(7)V99  VALUE ZERO  COMP-3.
100518 77  FIN-TM-OTH             PIC S9(7)V99  VALUE ZERO  COMP-3.
00184  77  FIN-LM-LIFE            PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
00185  77  FIN-LM-AH              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
122702 77  FIN-LM-IU              PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
121503 77  FIN-LM-GAP             PIC S9(7)V99  VALUE ZERO  COMP-3.     EL344
052614 77  FIN-LM-FAM             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  FIN-LM-BRV             PIC S9(7)V99  VALUE ZERO  COMP-3.
022122 77  FIN-LM-HOS             PIC S9(7)V99  VALUE ZERO  COMP-3.
100518 77  FIN-LM-OTH             PIC S9(7)V99  VALUE ZERO  COMP-3.
00186  77  ED-CNT                 PIC ZZZZ,ZZ9.                         EL344
00187  77  ED-AMT                 PIC Z,ZZZ,ZZZ.99.                     EL344
00188  77  THIS-MONTH             PIC X        VALUE SPACE.             EL344
00189  77  LAST-MONTH             PIC X        VALUE SPACE.             EL344
00190  77  THIS-YEAR              PIC X        VALUE SPACE.             EL344
00191                                                                   EL344
00192  01  AT-STATUS.                                                   EL344
00193      03  AT-STAT-1   PIC X.                                       EL344
00194      03  AT-STAT-2   PIC X.                                       EL344
00195                                                                   EL344
00196  01  ERACCT-STATUS.                                               EL344
00197      03  ERACCT-STAT-1   PIC X.                                   EL344
00198      03  ERACCT-STAT-2   PIC X.                                   EL344
00199                                                                   EL344
00200  01  CL-STATUS.                                                   EL344
00201      03  CL-STAT-1   PIC X.                                       EL344
00202      03  CL-STAT-2   PIC X.                                       EL344
00203                                                                   EL344
00204  01  CM-STATUS.                                                   EL344
00205      03  CM-STAT-1   PIC X.                                       EL344
00206      03  CM-STAT-2   PIC X.                                       EL344
00207                                                                   EL344
00208  01  PM-STATUS.                                                   EL344
00209      03  PM-STAT-1                   PIC X(01).      
00210      03  PM-STAT-2                   PIC X(01).     
00211                                                                   EL344
00212  01  MPPROD-STATUS.                                               EL344
00213      03  MPPROD-STAT-1               PIC X(01).    
00214      03  MPPROD-STAT-2               PIC X(01).   
00215                                                                   EL344
00216  01  FILLER.                                                      EL344
00217      05  ABEND-CODE                  PIC X(4) VALUE SPACES.   
00218      05  ABEND-OPTION                PIC X    VALUE 'Y'.     
00219      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL344
00220      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL344
00221      05  WS-RETURN-CODE       COMP-3 PIC S9(3)   VALUE ZERO.      EL344
00222      05  PGM-SUB                     PIC S999 COMP-3 VALUE +344. 
011204     05  WS-SORT-REC-CNT      COMP-5 PIC S9(08)  VALUE +0.  
00223                                                                      CL**2
00224  01  DATE-BREAKDOWN.                                                 CL**2
00225      05  ST-CID-N        PIC 9(11).                                  CL**6
00226      05  ST-CID-R REDEFINES ST-CID-N.                                CL**6
00227          10  FILLER      PIC 999.                                    CL**2
00228          10  ST-CID-CC   PIC 99.                                     CL**2
00229          10  ST-CID-YR   PIC 99.                                     CL**2
00230          10  ST-CID-MO   PIC 99.                                     CL**2
00231          10  ST-CID-DA   PIC 99.                                     CL**2
00232      05  ST-CRD-N        PIC 9(11).                                  CL**6
00233      05  ST-CRD-R REDEFINES ST-CRD-N.                                CL**6
00234          10  FILLER      PIC 999.                                    CL**2
00235          10  ST-CRD-CC   PIC 99.                                     CL**2
00236          10  ST-CRD-YR   PIC 99.                                     CL**9
00237          10  ST-CRD-MO   PIC 99.                                     CL**2
00238          10  ST-CRD-DA   PIC 99.                                     CL**2
00239  EJECT                                                            EL344
00240  01  HD-1.                                                        EL344
00241      03  FILLER             PIC X(52) VALUE SPACES.               EL344
00242      03  FILLER             PIC X(20) VALUE 'CLAIM STATUS REPORT'.EL344
062905     03  HD1-YTD            PIC X(48) VALUE SPACES.               EL344
00244      03  FILLER             PIC X(8)  VALUE 'EL344'.              EL344
00245                                                                   EL344
00246  01  HD-2.                                                        EL344
00247      03  FILLER             PIC X(47) VALUE SPACES.               EL344
00248      03  HD-COMP            PIC X(30).                            EL344
00249      03  FILLER             PIC X(43) VALUE SPACES.               EL344
00250      03  HD-IPL             PIC X(8).                             EL344
00251                                                                   EL344
00252  01  HD-3.                                                        EL344
00253 *    03  FILLER             PIC X(53) VALUE SPACES.               EL344
           03  filler             pic x(11) value ' RPT CODE1 '.
           03  hd3-rptcd1         pic x(10) value spaces.
           03  filler             pic x(11) value ' RPT CODE2 '.
           03  hd3-rptcd2         pic x(10) value spaces.
           03  filler             pic x(11) value spaces.
00254      03  HD-DATE            PIC X(18).                            EL344
00255      03  FILLER             PIC X(49) VALUE SPACES.               EL344
00256      03  FILLER             PIC X(5)  VALUE 'PAGE '.              EL344
00257      03  HD-PG              PIC ZZ,ZZZ.                           EL344
00258                                                                   EL344
00259  01  SUB-HD1.                                                     EL344
00260      03  FILLER             PIC X(4)    VALUE '0   '.             EL344
00261      03  FILLER             PIC X(14)   VALUE '****ACCOUNT NO'.   EL344
00262      03  FILLER             PIC XX      VALUE SPACES.             EL344
00263      03  SH1-ACNO           PIC X(10).                            EL344
00264      03  FILLER             PIC X(3)    VALUE ' - '.              EL344
00265      03  SH1-ACNA           PIC X(30).                            EL344
00266      03  FILLER             PIC X(76)   VALUE SPACES.             EL344
00267                                                                   EL344
00268  01  SUB-HD2.                                                     EL344
00269      03  FILLER             PIC X     VALUE '0'.                  EL344
00270      03  FILLER             PIC X(5)  VALUE SPACES.               EL344
00271      03  FILLER             PIC X(48) VALUE 'CLAIM  CLAIMANT /'.  EL344
00272      03  FILLER             PIC X(15) VALUE 'RECEIVED'.           EL344
00273      03  FILLER             PIC X(10) VALUE 'INCURRED'.           EL344
00274      03  FILLER             PIC X(16) VALUE 'TOTAL-PAYMENTS  '.   EL344
00275      03  FILLER             PIC X(18) VALUE '******************'. EL344
00276      03  FILLER             PIC X(10)  VALUE ' ACTIVITY'.         EL344
00277      03  FILLER             PIC X(18) VALUE '******************'. EL344
00278                                                                   EL344
00279  01  SUB-HD3.                                                     EL344
00280      03  FILLER             PIC X     VALUE SPACES.               EL344
00281      03  SUBHD3-A           PIC X(4)  VALUE ' CAR'.               EL344
00282      03  FILLER             PIC X(28) VALUE ' NUMBER MEMBER-LOAN'.EL344
00283      03  SUBHD3-B           PIC X(14) VALUE 'CERT. NO.'.          EL344
00284      03  FILLER             PIC X(9)  VALUE 'STATUS'.             EL344
00285      03  FILLER             PIC X(12) VALUE 'DATE    TYP'.        EL344
00286      03  FILLER             PIC X(7)  VALUE '   DATE'.            EL344
00287      03  FILLER             PIC X(20) VALUE '     NO    AMOUNT '. EL344
00288      03  FILLER             PIC X(19) VALUE 'DATE   DESCRIPTION'. EL344
00289      03  FILLER             PIC X(25) VALUE SPACES.               EL344
00290  EJECT                                                            EL344
00291  01  DET-BASIC.                                                   EL344
00292      03  FILLER             PIC XX    VALUE '0'.                  EL344
00293      03  DET-CAR            PIC XX.                               EL344
00294      03  FILLER             PIC X     VALUE SPACES.               EL344
00295      03  DET-CLM            PIC X(7).                             EL344
00296      03  FILLER             PIC X     VALUE SPACES.               EL344
00297      03  DET-NAME           PIC X(20).                            EL344
00298      03  FILLER             PIC X     VALUE SPACES.               EL344
00299      03  DET-CERT           PIC X(11).                            EL344
00300      03  FILLER             PIC XX     VALUE SPACES.              EL344
00301      03  DET-STAT           PIC X(6).                             EL344
00302      03  FILLER             PIC X     VALUE SPACES.               EL344
00303      03  DET-RMO            PIC X(2).                             EL344
00304      03  FILLER             PIC X     VALUE '/'.                  EL344
00305      03  DET-RDA            PIC XX.                               EL344
00306      03  FILLER             PIC X     VALUE '/'.                  EL344
00307      03  DET-RYR            PIC XX.                               EL344
00308      03  FILLER             PIC X VALUE SPACES.                   EL344
00309      03  DET-TYPE           PIC X(6).                             EL344
00310      03  FILLER             PIC X VALUE SPACES.                   EL344
00311      03  DET-INM            PIC XX.                               EL344
00312      03  FILLER             PIC X     VALUE '/'.                  EL344
00313      03  DET-IND            PIC XX.                               EL344
00314      03  FILLER             PIC X     VALUE '/'.                  EL344
00315      03  DET-INY            PIC XX.                               EL344
00316      03  FILLER             PIC X(1)  VALUE SPACES.               EL344
00317      03  DET-NP             PIC ZZZ.                              EL344
00318      03  DET-PAM            PIC ZZZZ,ZZZ.ZZ.                      EL344
00319      03  FILLER             PIC X(44) VALUE SPACES.               EL344
00320                                                                   EL344
00321  01  DET-PRT.                                                     EL344
00322      03  FILLER             PIC X(11) VALUE SPACES.               EL344
00323      03  D-MEMB-LOAN-CR.                                          EL344
00324          05  DMEMB          PIC X(12) VALUE SPACES.               EL344
00325          05  FILLER         PIC X(01) VALUE SPACE.                EL344
00326          05  DLOAN-CR       PIC X(08) VALUE SPACES.               EL344
00327      03  D-LOAN-CV REDEFINES D-MEMB-LOAN-CR.                      EL344
00328          05  DLOAN          PIC X(21).                            EL344
00329 *        05  FILLER         PIC X(01).                            EL344
00330      03  FILLER             PIC X(46) VALUE SPACES.               EL344
00331      02  DDATE.                                                   EL344
00332       03  DMO                PIC XX.                              EL344
00333       03  FILLER             PIC X     VALUE '/'.                 EL344
00334       03  DDA                PIC XX.                              EL344
00335       03  FILLER             PIC X     VALUE '/'.                 EL344
00336       03  DYR                PIC XX.                              EL344
00337      02  FILLER.                                                  EL344
00338      03  FILLER             PIC X     VALUE SPACES.               EL344
00339      03  DMESS              PIC X(33) VALUE SPACES.               EL344
00340  EJECT                                                            EL344
00341  01  PMT-MES.                                                     EL344
00342      03  PM-TYPE            PIC X(7).                             EL344
00343      03  PM-AMT             PIC ZZ,ZZZ.ZZ-.                       EL344
00344      03  PM-BODY.                                                 EL344
00345        05  PM-THRU            PIC X(7).                           EL344
00346        05  FILLER             PIC X     VALUE SPACES.             EL344
00347        05  PDTDATE.                                               EL344
00348          07  PDTMO              PIC XX.                           EL344
00349          07  DSL1               PIC X     VALUE '/'.              EL344
00350          07  PDTDA              PIC XX.                           EL344
00351          07  DSL2               PIC X     VALUE '/'.              EL344
00352          07  PDTYR              PIC XX.                           EL344
00353                                                                   EL344
00354  01  FORM-MES.                                                    EL344
00355      03  FM-TYPE            PIC X(6).                             EL344
00356      03  FILLER             PIC X    VALUE SPACE.                 EL344
00357      03  FM-KIND            PIC X(8).                             EL344
00358      03  FILLER             PIC X(3) VALUE SPACE.                 EL344
00359      03  FM-SENT            PIC X(7).                             EL344
00360      03  FDTDATE.                                                 EL344
00361        05  FDTMO              PIC XX.                             EL344
00362        05  FSL1               PIC X     VALUE '/'.                EL344
00363        05  FDTDA              PIC XX.                             EL344
00364        05  FSL2               PIC X     VALUE '/'.                EL344
00365        05  FDTYR              PIC XX.                             EL344
00366                                                                   EL344
00367  01  TOTAL-HEADING.                                               EL344
00368      05  FILLER               PIC X(37)  VALUE '0'.               EL344
00369      05  TH-AH-OVERRIDE       PIC X(6)   VALUE SPACES.            EL344
00370      05  FILLER               PIC X(6)   VALUE SPACES.            EL344
00371      05  TH-LIFE-OVERRIDE     PIC X(6)   VALUE SPACES.            EL344
121603     05  FILLER               PIC X(7)   VALUE SPACES.            EL344
121603     05  TH-GAP-OVERRIDE      PIC X(6)   VALUE SPACES.            EL344
122702     05  FILLER               PIC X(7)   VALUE SPACES.            EL344
122702     05  TH-IU-OVERRIDE       PIC X(6)   VALUE SPACES.            EL344
052614     05  FILLER               PIC X(5)   VALUE SPACES.
052614     05  TH-FAM-OVERRIDE      PIC X(6)   VALUE SPACES.
100518     05  FILLER               PIC X(6)   VALUE SPACES.
100518     05  TH-OTH-OVERRIDE      PIC X(6)   VALUE SPACES.
100518     05  FILLER               PIC X(6)   VALUE SPACES.
022122     05  TH-BRV-OVERRIDE      PIC X(6)   VALUE SPACES.
022122     05  FILLER               PIC X(6)   VALUE SPACES.
022122     05  TH-HOS-OVERRIDE      PIC X(6)   VALUE SPACES.
00372                                                                   EL344
00373  01  TOTAL-CNT.                                                   EL344
122702     03  TL-MES              PIC X(32).                           EL344
122702     03  FILLER              PIC X(3)  VALUE SPACES.              EL344
122702     03  TL-CNT-AH           PIC ZZZ,ZZ9-.      
00377      03  FILLER              PIC X(5)  VALUE SPACES.              EL344
122702     03  TL-CNT-LIFE         PIC ZZZ,ZZ9-.  
121603     03  FILLER              PIC X(4)  VALUE SPACES.              EL344
121603     03  TL-CNT-GAP          PIC ZZZ,ZZ9-.  
122702     03  FILLER              PIC X(4)  VALUE SPACES.              EL344
122702     03  TL-CNT-IU           PIC ZZZ,ZZ9-.  
052614     03  FILLER              PIC X(4)  VALUE SPACES.
052614     03  TL-CNT-FAM          PIC ZZZ,ZZ9-.
100518     03  FILLER              PIC X(4)  VALUE SPACES.
100518     03  TL-CNT-OTH          PIC ZZZ,ZZ9-.
022122     03  FILLER              PIC X(4)  VALUE SPACES.
022122     03  TL-CNT-BRV          PIC ZZZ,ZZ9-.
022122     03  FILLER              PIC X(4)  VALUE SPACES.
022122     03  TL-CNT-HOS          PIC ZZZ,ZZ9-.
00379                                                                   EL344
00380  01  TOTAL-AMT.                                                   EL344
00381      03  TL-MESA             PIC X(29).                           EL344
00382      03  FILLER              PIC X(2)  VALUE SPACES.              EL344
00383      03  TL-AMT-AH           PIC ZZZZ,ZZZ.99-.
00384      03  FILLER              PIC X     VALUE SPACES.              EL344
00385      03  TL-AMT-LIFE         PIC ZZZZ,ZZZ.99-.
121603     03  FILLER              PIC X     VALUE SPACES.              EL344
121603     03  TL-AMT-GAP          PIC ZZZ,ZZZ.99-.
00384      03  FILLER              PIC X     VALUE SPACES.              EL344
00385      03  TL-AMT-IU           PIC ZZZ,ZZZ.99-.
052614     03  FILLER              PIC X     VALUE SPACES.
052614     03  TL-AMT-FAM          PIC ZZZ,ZZZ.99-.
100518     03  FILLER              PIC X     VALUE SPACES.
100518     03  TL-AMT-OTH          PIC ZZZ,ZZZ.99-.
022122     03  FILLER              PIC X     VALUE SPACES.
022122     03  TL-AMT-BRV          PIC ZZZ,ZZZ.99-.
022122     03  FILLER              PIC X     VALUE SPACES.
022122     03  TL-AMT-HOS          PIC ZZZ,ZZZ.99-.
00386                                                                   EL344
00387  01  COUNTERS-AND-TEMP-STORAGE.                                   EL344
00388      03  WK-DATE.                                                 EL344
00389         05  WK-YR   PIC 99.                                       EL344
00390         05  WK-MO   PIC 99.                                       EL344
00391         05  WK-DA   PIC 99.                                       EL344
00392      03  LN-CT              PIC S9(3) VALUE +58 COMP-3.           EL344
00393      03  PG-NO              PIC S9(5) VALUE +0  COMP-3.           EL344
00394      03  FROM-DATE          PIC XX.                               EL344
00395      03  FROM-DATE2         PIC XX.                               EL344
00396      03  Z-DATE.                                                  EL344
00397          05  Z-YR           PIC 99.                               EL344
00398          05  Z-MO           PIC 99.                               EL344
00399          05  Z-DA           PIC 99.                               EL344
00400      03  SUB-A              PIC S999  VALUE +0 COMP-3.            EL344
00401      03  SUB-B              PIC S999  VALUE +0 COMP-3.            EL344
00402      03  SAVE-F1            PIC X(74) VALUE SPACES.               EL344
00403      03  WS-CHECK-WRITTEN-DATE.                                   EL344
00404          05  WS-CK-MO       PIC 99.                               EL344
00405          05  WS-CK-DA       PIC 99.                               EL344
00406          05  WS-CK-YR       PIC 99.                               EL344
00407  EJECT                                                            EL344
00408                              COPY ELCDTECX.                       EL344
00409  EJECT                                                            EL344
00410                              COPY ELCDTEVR.                       EL344
00411  EJECT                                                            EL344
00412                              COPY ELCDATE.                           CL**7
00413  EJECT                                                            EL344
00414  PROCEDURE DIVISION.                                              EL344
00415                                                                   EL344
00416  0100-SET-DATE.              COPY ELCDTERX.                       EL344
00417  EJECT                                                            EL344
00418  0200-START.                                                      EL344
00419      MOVE    AH-OVERRIDE-L6    TO  TH-AH-OVERRIDE.                EL344
00420      MOVE    LIFE-OVERRIDE-L6  TO  TH-LIFE-OVERRIDE.              EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE 'GAP   '         TO  TH-GAP-OVERRIDE
122702         MOVE 'IU    '         TO  TH-IU-OVERRIDE
052614         MOVE 'FAM   '         TO  TH-FAM-OVERRIDE
022122         MOVE 'BRV   '         TO  TH-BRV-OVERRIDE
022122         MOVE 'HOS   '         TO  TH-HOS-OVERRIDE
122702     END-IF.
100518     MOVE 'OTH      '          TO  TH-OTH-OVERRIDE.
00421      MOVE    COMPANY-NAME      TO  HD-COMP.                       EL344
00422      MOVE    WS-CURRENT-DATE   TO  HD-IPL.                        EL344
00423      MOVE    ALPH-DATE         TO  HD-DATE.                       EL344
pemuni     move +0 to dc-elapsed-days
00424      MOVE    BIN-RUN-DATE      TO  WS-RUN-BIN-DT                  EL344
00425                                    DC-BIN-DATE-1.                 EL344
00426      MOVE    '6'               TO  DC-OPTION-CODE.                EL344
00427      MOVE    -3                TO  DC-ELAPSED-MONTHS.             EL344
00428      PERFORM 1100-DATE-RTN    THRU 1100-DATE-RTN-EXIT.            EL344
pemuni     move +0 to dc-elapsed-days
00429      MOVE    DC-BIN-DATE-2     TO  FROM-DATE.                     EL344
00430      MOVE    '6'               TO  DC-OPTION-CODE.                EL344
00431      MOVE    -1                TO  DC-ELAPSED-MONTHS.             EL344
00432      PERFORM 1100-DATE-RTN    THRU 1100-DATE-RTN-EXIT.            EL344
00433      MOVE    DC-BIN-DATE-2     TO  FROM-DATE2.                    EL344

062905     IF DTE-PGM-OPT = '2'
062905        MOVE WS-RUN-DATE         TO DC-GREG-DATE-CYMD
062905        DISPLAY ' DATE B4 ' DC-GREG-DATE-CYMD
062905        MOVE 01                  TO DC-CYMD-MONTH
062905        MOVE 01                  TO DC-CYMD-DAY
062905        DISPLAY ' DATE AF ' DC-GREG-DATE-CYMD
062905        MOVE 'L'                 TO DC-OPTION-CODE
062905        MOVE +0                  TO DC-ELAPSED-MONTHS
062905                                    DC-ELAPSED-DAYS
062905        PERFORM 1100-DATE-RTN    THRU 1100-DATE-RTN-EXIT
062905        IF NO-CONVERSION-ERROR
062905           MOVE '  YTD FOR ACTUARY '
062905                                 TO HD1-YTD
062905           MOVE DC-BIN-DATE-1    TO FROM-DATE
062905                                    FROM-DATE2
062905        ELSE
062905           DISPLAY ' PROBLEM WITH CONVERTING OPTION 2   '
062905                   '  ' ' USING DEFAULT PGM OPTION '
062905        END-IF
062905     END-IF

00435      OPEN INPUT ELMSTR                                            EL344
00436                 ERACCT                                            EL344
00437                 ELCERT                                            EL344
00438                 ELTRLR                                            EL344
00439                 MPPLCY                                            EL344
00440                 MPPROD                                            EL344
00441         OUTPUT PRINTER.                                           EL344
00442                                                                   EL344
00443      IF CL-STATUS  NOT = '00' AND '97'                            EL344
00444          MOVE CL-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00445          MOVE 'ERROR OCCURED OPEN - ELMSTR'  TO  WS-ABEND-MESSAGE EL344
00446          GO TO ABEND-PGM.                                         EL344
00447                                                                   EL344
00448      IF ERACCT-STATUS  NOT = '00' AND '97'                        EL344
00449          MOVE  ERACCT-STATUS             TO  WS-ABEND-FILE-STATUS EL344
00450          MOVE 'ERROR OCCURED OPEN - ERACCT'  TO  WS-ABEND-MESSAGE EL344
00451          GO TO ABEND-PGM.                                         EL344
00452                                                                   EL344
00453      IF CM-STATUS NOT = '00' AND '97'                             EL344
00454          MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00455          MOVE 'ERROR OCCURED OPEN - ELCERT'  TO  WS-ABEND-MESSAGE EL344
00456          GO TO ABEND-PGM.                                         EL344
00457                                                                   EL344
00458      IF AT-STATUS NOT = '00' AND '97'                             EL344
00459          MOVE AT-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00460          MOVE 'ERROR OCCURED OPEN - ELTRLR'  TO  WS-ABEND-MESSAGE EL344
00461          GO TO ABEND-PGM.                                         EL344
00462                                                                   EL344
00463      IF (PM-STATUS NOT = '00' AND '97' AND '9%' AND '9+')         EL344
00464          MOVE MPPROD-STATUS      TO  WS-ABEND-FILE-STATUS         EL344
00465          MOVE 'ERROR OCCURED OPEN - MPPLCY'  TO  WS-ABEND-MESSAGE EL344
00466          GO TO ABEND-PGM.                                         EL344
00467                                                                   EL344
00468      IF (MPPROD-STATUS NOT = '00' AND '97' AND '9%' AND '9+')     EL344
00469          MOVE MPPROD-STATUS      TO  WS-ABEND-FILE-STATUS         EL344
00470          MOVE 'ERROR OCCURED OPEN - MPPROD'  TO  WS-ABEND-MESSAGE EL344
00471          GO TO ABEND-PGM.                                         EL344
00472                                                                   EL344
00473      MOVE LOW-VALUES            TO CL-CONTROL-PRIMARY.            EL344
00474      MOVE DTE-CLASIC-COMPANY-CD TO CL-COMPANY-CD.                 EL344
00475                                                                   EL344
00476      START ELMSTR   KEY NOT LESS THAN CL-COMPANY-CD.              EL344
00477                                                                   EL344
011204     EVALUATE CL-STATUS
011204     WHEN ZERO 
011204         CONTINUE
011204
011204     WHEN '23'
011204         DISPLAY 'NO CLAIM RECS FOR THIS COMPANY'
011204         PERFORM 9999-EOJ
011204
011204     WHEN OTHER 
00479          MOVE CL-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00480          MOVE 'ERROR OCCURED START - ELMSTR'                      EL344
00481                                  TO  WS-ABEND-MESSAGE             EL344
00482          GO TO ABEND-PGM
011204     END-EVALUATE.
00483                                                                   EL344
00484      SORT SORT-WORK                                               EL344
00485        ON ASCENDING KEY   ST-ACC                                  EL344
00486                           ST-SYSTEM-IDENTIFIER                    EL344
00487                           ST-NAME                                 EL344
00488          INPUT PROCEDURE IS 0300-BUILD-SORT-REC                   EL344
00489          OUTPUT PROCEDURE IS 0400-PROCESS-CLAIMS.                 EL344
00490                                                                   EL344
00491      PERFORM 9999-EOJ.                                            EL344
00492                                                                   EL344
00493  0300-SORT-END.                                                   EL344
00494  EJECT                                                            EL344
00495  0300-BUILD-SORT-REC SECTION.                                     EL344
00496                                                                   EL344
00497  0300-READ-LOOP.                                                  EL344
00498      READ ELMSTR  NEXT RECORD.                                    EL344
00499                                                                   EL344
00500      IF   CL-STAT-1  = '1'                                        EL344
00501          GO TO 0300-E-MAST-SORT.                                  EL344
00502                                                                   EL344
00503      IF CL-STAT-1 NOT = ZERO                                      EL344
00504          MOVE CL-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00505          MOVE 'ERROR OCCURED START - ELMSTR'                      EL344
00506                                  TO  WS-ABEND-MESSAGE             EL344
00507          GO TO ABEND-PGM.                                         EL344
00508                                                                   EL344
00509      IF CL-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL344
00510            GO TO 0300-E-MAST-SORT.                                EL344
      *    add +1 to ws-cl-rec-cnt.
      *    if ws-cl-rec-cnt > +1000
      *       go to 0300-e-mast-sort.

00511                                                                   EL344
00512      MOVE    CL-CERT-ACCOUNT      TO  ST-ACC.                     EL344
00513      MOVE    CL-CERT-NO           TO  ST-CERT.                    EL344
00514      MOVE    CL-CERT-CARRIER      TO  ST-CAR.                     EL344
00515      MOVE    CL-CERT-GROUPING     TO  ST-COMP.                    EL344
00516      MOVE    CL-CERT-STATE        TO  ST-STATE.                   EL344
00517      MOVE    CL-CLAIM-NO          TO  ST-CLMNO.                   EL344
00518      MOVE    CL-CLAIM-PREM-TYPE   TO  ST-CLMSTA.                  EL344
00519      MOVE    CL-CLAIM-TYPE        TO  ST-CLMTYP.                  EL344
00520      MOVE    CL-INSURED-LAST-NAME TO  ST-LNAME.                   EL344
00521      MOVE    CL-INSURED-1ST-NAME  TO  ST-FNAME.                   EL344
00522      MOVE    CL-NO-OF-PMTS-MADE   TO  ST-NOPMTS.                  EL344
00523      MOVE    CL-CLAIM-STATUS      TO  ST-CLOC.                    EL344
00524      MOVE    CL-TOTAL-PAID-AMT    TO  ST-TOTCLM.                  EL344
00525      MOVE    CL-CERT-EFF-DT       TO  ST-EFF-DT.                  EL344
00526      MOVE    CL-FILE-ESTABLISH-DT TO  DC-BIN-DATE-1.              EL344
00527      MOVE    SPACES               TO  DC-OPTION-CODE.             EL344
00528      PERFORM 1100-DATE-RTN       THRU 1100-DATE-RTN-EXIT.         EL344
00529      MOVE    DC-GREG-DATE-1-YMD   TO  WK-DATE.                    EL344
00530      PERFORM 1500-CK-DATE.                                        EL344
00531                                                                   EL344
00532      IF  THIS-MONTH = 'Y'                                         EL344
122702         EVALUATE TRUE
122702         WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1    
00534              ADD +1  TO  EST-TM-LIFE                              EL344

122702         WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1
00536              ADD +1  TO  EST-TM-AH

121603         WHEN CL-CLAIM-TYPE = 'G'
121603             ADD +1  TO  EST-TM-GAP

122702         WHEN CL-CLAIM-TYPE = 'I'
122702             ADD +1  TO  EST-TM-IU
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             ADD +1  TO  EST-TM-FAM
100518
022122         WHEN CL-CLAIM-TYPE = 'B'
022122             ADD +1  TO  EST-TM-BRV
022122
022122         WHEN CL-CLAIM-TYPE = 'H'
022122             ADD +1  TO  EST-TM-HOS
100518
100518         WHEN CL-CLAIM-TYPE = 'O'
100518             ADD +1  TO  EST-TM-OTH
122702         END-EVALUATE
122702     END-IF.
00537                                                                   EL344
00538      IF  LAST-MONTH = 'Y'                                         EL344
122702         EVALUATE TRUE
122702         WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1    
00534              ADD +1  TO  EST-LM-LIFE                              EL344

122702         WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1
00536              ADD +1  TO  EST-LM-AH

121603         WHEN CL-CLAIM-TYPE = 'G'
121603             ADD +1  TO  EST-LM-GAP

122702         WHEN CL-CLAIM-TYPE = 'I'
122702             ADD +1  TO  EST-LM-IU
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             ADD +1  TO  EST-LM-FAM
100518
022122         WHEN CL-CLAIM-TYPE = 'B'
022122             ADD +1  TO  EST-LM-BRV
022122
022122         WHEN CL-CLAIM-TYPE = 'H'
022122             ADD +1  TO  EST-LM-HOS
100518
100518         WHEN CL-CLAIM-TYPE = 'O'
100518             ADD +1  TO  EST-LM-OTH
122702         END-EVALUATE
122702     END-IF.
00543                                                                   EL344
00544      IF  THIS-YEAR  = 'Y'                                         EL344
122702         EVALUATE TRUE
122702         WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1    
00534              ADD +1  TO  EST-TY-LIFE                              EL344

122702         WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1
00536              ADD +1  TO  EST-TY-AH

121603         WHEN CL-CLAIM-TYPE = 'G'
121603             ADD +1  TO  EST-TY-GAP

122702         WHEN CL-CLAIM-TYPE = 'I'
122702             ADD +1  TO  EST-TY-IU
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             ADD +1  TO  EST-TY-FAM
100518
022122         WHEN CL-CLAIM-TYPE = 'B'
022122             ADD +1  TO  EST-TY-BRV
022122
022122         WHEN CL-CLAIM-TYPE = 'H'
022122             ADD +1  TO  EST-TY-HOS
100518
100518         WHEN CL-CLAIM-TYPE = 'O'
100518             ADD +1  TO  EST-TY-OTH
122702         END-EVALUATE
122702     END-IF.
00549                                                                   EL344
122702     EVALUATE TRUE
122702     WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1 AND 
00551           CLAIM-IS-OPEN      
00552          ADD +1  TO  TOT-OPEN-LIFE      

122702     WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1 AND 
122702          CLAIM-IS-CLOSED    
00554          ADD +1  TO  TOT-CLOSED-LIFE     

122702     WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1 AND 
122702          CLAIM-IS-OPEN      
00557          ADD +1  TO  TOT-OPEN-AH         

122702     WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1 AND 
122702          CLAIM-IS-CLOSED    
00559          ADD +1  TO  TOT-CLOSED-AH
00560                                                                   EL344
121603     WHEN CL-CLAIM-TYPE = 'G' AND               
121603          CLAIM-IS-OPEN      
121603         ADD +1  TO  TOT-OPEN-GAP       

121603     WHEN CL-CLAIM-TYPE = 'G' AND 
121603          CLAIM-IS-CLOSED    
121603         ADD +1  TO  TOT-CLOSED-GAP

122702     WHEN CL-CLAIM-TYPE = 'I' AND               
122702          CLAIM-IS-OPEN      
122702         ADD +1  TO  TOT-OPEN-IU        

122702     WHEN CL-CLAIM-TYPE = 'I' AND 
122702          CLAIM-IS-CLOSED    
122702         ADD +1  TO  TOT-CLOSED-IU
052614
052614     WHEN CL-CLAIM-TYPE = 'F' AND               
052614          CLAIM-IS-OPEN      
052614         ADD +1  TO  TOT-OPEN-FAM       
052614
052614     WHEN CL-CLAIM-TYPE = 'F' AND 
052614          CLAIM-IS-CLOSED    
052614         ADD +1  TO  TOT-CLOSED-FAM
100518
022122     WHEN CL-CLAIM-TYPE = 'B' AND               
022122          CLAIM-IS-OPEN      
022122         ADD +1  TO  TOT-OPEN-BRV       
022122
022122     WHEN CL-CLAIM-TYPE = 'B' AND 
022122          CLAIM-IS-CLOSED    
022122         ADD +1  TO  TOT-CLOSED-BRV
022122
022122     WHEN CL-CLAIM-TYPE = 'H' AND               
022122          CLAIM-IS-OPEN      
022122         ADD +1  TO  TOT-OPEN-HOS       
022122
022122     WHEN CL-CLAIM-TYPE = 'H' AND 
022122          CLAIM-IS-CLOSED    
022122         ADD +1  TO  TOT-CLOSED-HOS
100518
100518     WHEN CL-CLAIM-TYPE = 'O' AND
100518          CLAIM-IS-OPEN
100518         ADD +1  TO  TOT-OPEN-OTH
100518
100518     WHEN CL-CLAIM-TYPE = 'O' AND
100518          CLAIM-IS-CLOSED
100518         ADD +1  TO  TOT-CLOSED-OTH
122702     END-EVALUATE.
00560                                                                   EL344
00561      MOVE    CL-INCURRED-DT     TO  DC-BIN-DATE-1.                EL344
00562      MOVE    SPACES             TO  DC-OPTION-CODE.               EL344
00563      PERFORM 1100-DATE-RTN     THRU 1100-DATE-RTN-EXIT.           EL344
00564      MOVE    DC-GREG-DATE-CYMD  TO  ST-CID.                          CL**6
00565      MOVE    CL-REPORTED-DT     TO  DC-BIN-DATE-1.                EL344
00566      MOVE    SPACES             TO  DC-OPTION-CODE.               EL344
00567      PERFORM 1100-DATE-RTN     THRU 1100-DATE-RTN-EXIT.           EL344
00568      MOVE    DC-GREG-DATE-CYMD  TO  ST-CRD.                          CL**6
00569      MOVE    DC-GREG-DATE-1-YMD TO  WK-DATE.                      EL344
00570      PERFORM 1500-CK-DATE.                                        EL344
00571                                                                   EL344
00572      IF CLAIM-DENIED                                              EL344
00573          NEXT SENTENCE                                            EL344
00574      ELSE                                                         EL344
00575          GO TO 0300-CONTINUE.                                     EL344
00576                                                                   EL344
00577      IF THIS-MONTH  =  'Y'                                        EL344
122702         EVALUATE TRUE
122702         WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1   
00579              ADD +1              TO  DEN-TM-LIFE                  EL344

122702         WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1
00581              ADD +1              TO  DEN-TM-AH

121603         WHEN CL-CLAIM-TYPE = 'G'
121603             ADD +1              TO  DEN-TM-GAP

122702         WHEN CL-CLAIM-TYPE = 'I'
122702             ADD +1              TO  DEN-TM-IU
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             ADD +1              TO  DEN-TM-FAM
100518
022122         WHEN CL-CLAIM-TYPE = 'B'
022122             ADD +1              TO  DEN-TM-BRV
022122
022122         WHEN CL-CLAIM-TYPE = 'H'
022122             ADD +1              TO  DEN-TM-HOS
100518
100518         WHEN CL-CLAIM-TYPE = 'O'
100518             ADD +1              TO  DEN-TM-OTH
122702         END-EVALUATE
122702     END-IF.
00582                                                                   EL344
00583      IF LAST-MONTH  =  'Y'                                        EL344
122702         EVALUATE TRUE
122702         WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1   
00579              ADD +1              TO  DEN-LM-LIFE                  EL344

122702         WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1
00581              ADD +1              TO  DEN-LM-AH

121603         WHEN CL-CLAIM-TYPE = 'G'
121603             ADD +1              TO  DEN-LM-GAP

122702         WHEN CL-CLAIM-TYPE = 'I'
122702             ADD +1              TO  DEN-LM-IU
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             ADD +1              TO  DEN-LM-FAM
100518
022122         WHEN CL-CLAIM-TYPE = 'B'
022122             ADD +1              TO  DEN-LM-BRV
022122
022122         WHEN CL-CLAIM-TYPE = 'H'
022122             ADD +1              TO  DEN-LM-HOS
100518
100518         WHEN CL-CLAIM-TYPE = 'O'
100518             ADD +1              TO  DEN-LM-OTH
122702         END-EVALUATE
122702     END-IF.
00588                                                                   EL344
00589      IF THIS-YEAR  =  'Y'                                         EL344
122702         EVALUATE TRUE
122702         WHEN CL-CLAIM-TYPE = LIFE-OVERRIDE-L1   
00579              ADD +1              TO  DEN-TY-LIFE                  EL344

122702         WHEN CL-CLAIM-TYPE = AH-OVERRIDE-L1
00581              ADD +1              TO  DEN-TY-AH

121603         WHEN CL-CLAIM-TYPE = 'G'
121603             ADD +1              TO  DEN-TY-GAP

122702         WHEN CL-CLAIM-TYPE = 'I'
122702             ADD +1              TO  DEN-TY-IU
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             ADD +1              TO  DEN-TY-FAM
100518
022122         WHEN CL-CLAIM-TYPE = 'B'
022122             ADD +1              TO  DEN-TY-BRV
022122
022122         WHEN CL-CLAIM-TYPE = 'H'
022122             ADD +1              TO  DEN-TY-HOS
100518
100518         WHEN CL-CLAIM-TYPE = 'O'
100518             ADD +1              TO  DEN-TY-OTH
122702         END-EVALUATE
122702     END-IF.
00594                                                                   EL344
00595  0300-CONTINUE.                                                   EL344
00596      MOVE    CL-LAST-PMT-DT        TO  DC-BIN-DATE-1.             EL344
00597      MOVE    SPACES                TO  DC-OPTION-CODE.            EL344
00598      PERFORM 1100-DATE-RTN        THRU 1100-DATE-RTN-EXIT.        EL344
00599      MOVE    DC-GREG-DATE-1-YMD    TO  ST-LMD.                       CL**6
00600      MOVE    CL-LAST-CLOSE-REASON  TO  ST-LAST-CLOSE-REASON.      EL344
00601      MOVE    CL-LAST-CLOSE-DT      TO  ST-LAST-CLOSE-DT.          EL344
00602                                                                   EL344
00603      IF  CLAIM-IS-CLOSED                                          EL344
00604        AND                                                        EL344
00605          ((CL-LAST-CLOSE-REASON EQUAL TO '3' AND                  EL344
00606            CL-LAST-MAINT-DT LESS THAN FROM-DATE2)                 EL344
00607         OR CL-LAST-MAINT-DT LESS THAN FROM-DATE)                  EL344
00608              GO TO 0300-READ-LOOP.                                EL344
00609                                                                   EL344
00610      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     EL344
00611          MOVE 'CV'               TO  ST-SYSTEM-IDENTIFIER         EL344
00612          MOVE CL-CV-REFERENCE-NO TO  ST-REFERENCE-NO              EL344
00613      ELSE                                                         EL344
00614          MOVE 'CR'               TO  ST-SYSTEM-IDENTIFIER         EL344
00615          MOVE SPACES             TO  ST-REFERENCE-NO.             EL344
00616                                                                   EL344
00617      RELEASE SORT-REC.                                            EL344
011204     ADD +1 TO WS-SORT-REC-CNT.
00618      GO TO 0300-READ-LOOP.                                        EL344
00619                                                                   EL344
00620  0300-E-MAST-SORT.                                                EL344
00621      EXIT.                                                        EL344
00622  EJECT                                                            EL344
00623  0400-PROCESS-CLAIMS     SECTION.                                 EL344
00624      CLOSE ELMSTR.                                                EL344
00625                                                                   EL344
00626      IF  CL-STAT-1  NOT = ZERO                                    EL344
00627          MOVE CL-STATUS    TO  WS-ABEND-FILE-STATUS               EL344
00628          MOVE 'ERROR OCCURED CLOSE - ELMSTR'  TO  WS-ABEND-MESSAGEEL344
00629          GO TO ABEND-PGM.                                         EL344
00630                                                                   EL344
00631  0400-READ-CLAIMS.                                                EL344
00632      RETURN SORT-WORK                                             EL344
00633          AT END                                                   EL344
00634              GO TO 1000-EXIT.                                     EL344
00635                                                                   EL344
00636      MOVE DTE-CLASIC-COMPANY-CD TO AT-COMPANY-CD.                 EL344
00637      MOVE ST-CAR                TO AT-CARRIER.                    EL344
00638      MOVE ST-CLMNO              TO AT-CLAIM-NO.                   EL344
00639      MOVE ST-CERT               TO AT-CERT-NO.                    EL344
00640      MOVE ZEROS                 TO AT-SEQUENCE-NO.                EL344
00641                                                                   EL344
00642      START ELTRLR   KEY NOT LESS THAN AT-CONTROL-PRIMARY.         EL344
00643                                                                   EL344
00644      IF  AT-STAT-1  NOT = ZERO                                    EL344
00645          MOVE AT-STATUS   TO  WS-ABEND-FILE-STATUS                EL344
00646          MOVE 'ERROR OCCURED START - ELTRLR'  TO  WS-ABEND-MESSAGEEL344
00647          GO TO ABEND-PGM.                                         EL344
00648  EJECT                                                            EL344
00649  0500-BLD-BASIC.                                                  EL344
00650                                                                   EL344
00651      PERFORM 1200-MOVE-ACCT THRU 1200-SUB-HEADING-EXIT.           EL344
00652                                                                   EL344
00653      MOVE    ZEROS           TO  DET-NP DET-PAM.                  EL344
00654      MOVE    ST-CAR          TO  DET-CAR.                         EL344
00655      MOVE    ST-CLMNO        TO  DET-CLM.                         EL344
00656      MOVE    ST-NAME         TO  DET-NAME.                        EL344
00657      MOVE    ST-CERT         TO  DET-CERT.                        EL344
00658                                                                   EL344
00659      IF ST-CLOC = 'C'                                             EL344
00660            MOVE 'CLOSED' TO DET-STAT                              EL344
00661      ELSE                                                         EL344
00662            MOVE ' OPEN ' TO DET-STAT.                             EL344
00663                                                                   EL344
00664      MOVE ST-CRD    TO ST-CRD-N.                                     CL**6
00665      MOVE ST-CRD-MO TO DET-RMO.                                   EL344
00666      MOVE ST-CRD-DA TO DET-RDA.                                   EL344
00667      MOVE ST-CRD-YR TO DET-RYR.                                   EL344
00668                                                                      CL**6
00669      MOVE ST-CID    TO ST-CID-N.                                     CL**6
00670      MOVE ST-CID-MO TO DET-INM.                                   EL344
00671      MOVE ST-CID-DA TO DET-IND.                                   EL344
00672      MOVE ST-CID-YR TO DET-INY.                                   EL344
00673                                                                      CL**6
00674      MOVE SPACES    TO DET-TYPE.                                  EL344
00675                                                                   EL344
122702     EVALUATE TRUE
122702     WHEN ST-CLMTYP = AH-OVERRIDE-L1   
00677          MOVE AH-OVERRIDE-L6   TO DET-TYPE
00678                                                                   EL344
122702     WHEN ST-CLMTYP = LIFE-OVERRIDE-L1       
00680          MOVE LIFE-OVERRIDE-L6 TO DET-TYPE
00678                                    
121603     WHEN ST-CLMTYP = 'G'         
121603         MOVE 'GAP   '         TO DET-TYPE

122702     WHEN ST-CLMTYP = 'I'         
122702         MOVE 'IU    '         TO DET-TYPE
052614
052614     WHEN ST-CLMTYP = 'F'
052614         MOVE 'FAM   '         TO DET-TYPE
100518
022122     WHEN ST-CLMTYP = 'B'
022122         MOVE 'BRV   '         TO DET-TYPE
022122
022122     WHEN ST-CLMTYP = 'H'
022122         MOVE 'HOS   '         TO DET-TYPE
100518
100518     WHEN ST-CLMTYP = 'O'
100518         MOVE 'OTH   '         TO DET-TYPE
122702     END-EVALUATE
00681                                                                   EL344
00682      MOVE ST-NOPMTS TO DET-NP.                                    EL344
00683      MOVE ST-TOTCLM TO DET-PAM.                                   EL344
00684                                                                   EL344
00685      IF  ST-LAST-CLOSE-REASON EQUAL TO '3'                        EL344
00686          MOVE    'AUTOMATICALLY CLOSED'  TO  DMESS                EL344
00687          MOVE    ST-LAST-CLOSE-DT        TO  DC-BIN-DATE-1        EL344
00688          MOVE    SPACES                  TO  DC-OPTION-CODE       EL344
00689          PERFORM 1100-DATE-RTN                                    EL344
00690          MOVE    DC-GREG-DATE-1-EDIT     TO  DDATE                EL344
00691      ELSE                                                         EL344
00692          MOVE    SPACES                  TO  DMESS.               EL344
00693                                                                   EL344
00694      ADD +2 TO LN-CT.                                             EL344
00695                                                                   EL344
00696      IF LN-CT GREATER +56                                         EL344
00697          PERFORM 1200-SUB-HEADING THRU 1200-SUB-HEADING-EXIT.     EL344
00698                                                                   EL344
00699      MOVE    DET-BASIC TO PRT.                                    EL344
00700      PERFORM 1400-PRINT-RTN        THRU 1400-PRINT-RTN-EXIT.      EL344
00701                                                                   EL344
00702      IF ST-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     EL344
00703          GO TO 0525-GET-POLICY-DATA.                              EL344
00704                                                                   EL344
00705      MOVE    DTE-CLASIC-COMPANY-CD  TO  CM-COMPANY-CD.            EL344
00706      MOVE    ST-CAR                 TO  CM-CARRIER.               EL344
00707      MOVE    ST-COMP                TO  CM-GROUPING.              EL344
00708      MOVE    ST-STATE               TO  CM-STATE.                 EL344
00709      MOVE    ST-ACC                 TO  CM-ACCOUNT.               EL344
00710      MOVE    ST-EFF-DT              TO  CM-CERT-EFF-DT.           EL344
00711      MOVE    ST-CERT                TO  CM-CERT-NO.               EL344
00712                                                                   EL344
00713      READ ELCERT.                                                 EL344
00714                                                                   EL344
00715      IF   CM-STATUS = '23'                                        EL344
00716           GO TO 0600-TRAILER-LOOP.                                EL344
00717                                                                   EL344
00718      IF CM-STAT-1 NOT = ZERO                                      EL344
00719          MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00720          MOVE 'ERROR OCCURED START - ELCERT'                      EL344
00721                                  TO  WS-ABEND-MESSAGE             EL344
00722          GO TO ABEND-PGM.                                         EL344
00723                                                                   EL344
00724      MOVE SPACES           TO D-MEMB-LOAN-CR.                     EL344
00725      MOVE CM-MEMBER-NO     TO DMEMB.                              EL344
00726      MOVE CM-LOAN-NUMBER   TO DLOAN-CR.                           EL344
00727                                                                   EL344
00728      GO TO 0600-TRAILER-LOOP.                                     EL344
00729                                                                   EL344
00730  0525-GET-POLICY-DATA.                                            EL344
00731                                                                   EL344
00732      MOVE DTE-CLASIC-COMPANY-CD      TO  PM-COMPANY-CD.           EL344
00733      MOVE ST-CAR                     TO  PM-CARRIER.              EL344
00734      MOVE ST-COMP                    TO  PM-GROUPING.             EL344
00735      MOVE ST-STATE                   TO  PM-STATE.                EL344
00736      MOVE ST-ACC                     TO  PM-PRODUCER.             EL344
00737      MOVE ST-EFF-DT                  TO  PM-POLICY-EFF-DT.        EL344
00738      MOVE ST-REFERENCE-NO            TO  PM-REFERENCE-NUMBER.     EL344
00739                                                                   EL344
00740      READ MPPLCY.                                                 EL344
00741                                                                   EL344
00742      IF PM-STATUS IS EQUAL TO '23'                                EL344
00743          GO TO 0600-TRAILER-LOOP.                                 EL344
00744                                                                   EL344
00745      MOVE SPACES                     TO  D-MEMB-LOAN-CR.          EL344
00746      MOVE PM-LOAN-NUMBER             TO  DLOAN.                   EL344
00747      GO TO 0600-TRAILER-LOOP.                                     EL344
00748                                                                   EL344
00749  EJECT                                                            EL344
00750  0550-PRT-IT.                                                     EL344
00751      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
00752      ADD     +1 TO LN-CT.                                         EL344
00753                                                                   EL344
00754      IF LN-CT GREATER +56                                         EL344
00755          PERFORM 1200-SUB-HEADING THRU 1200-SUB-HEADING-EXIT.     EL344
00756                                                                   EL344
00757  0600-TRAILER-LOOP.                                               EL344
00758      READ ELTRLR  NEXT RECORD.                                    EL344
00759                                                                   EL344
00760      IF  AT-STAT-1  = '1'                                         EL344
00761         GO TO 0400-READ-CLAIMS.                                   EL344
00762                                                                   EL344
00763      IF AT-STAT-1  NOT =  ZERO                                    EL344
00764          MOVE AT-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
00765          MOVE 'ERROR OCCURED READNEXT - ELTRLR'                   EL344
00766                                  TO  WS-ABEND-MESSAGE             EL344
00767          GO TO ABEND-PGM.                                         EL344
00768                                                                   EL344
00769      IF  (AT-CLAIM-NO NOT = ST-CLMNO) OR                          EL344
00770          (AT-CERT-NO NOT = ST-CERT)                               EL344
00771          GO TO 0400-READ-CLAIMS.                                  EL344
00772                                                                   EL344
00773      IF PAYMENT-TR
00774         GO TO 0700-BLD-PAYMENT
022106     END-IF
00775                                                                   EL344
00776      IF CORRESPONDENCE-TR                                         EL344
00777         GO TO 0800-BLD-CORRESPONDENCE.                            EL344
00778                                                                   EL344
00779      IF DENIAL-TR                                                 EL344
00780         GO TO 0900-BLD-DENIAL.                                    EL344
00781                                                                   EL344
00782      IF FORM-CONTROL-TR                                           EL344
00783         GO TO 0900-BLD-FORM-CONTROL.                              EL344
00784                                                                   EL344
00785                                                                   EL344
00786      GO TO 0600-TRAILER-LOOP.                                     EL344
00787  EJECT                                                            EL344
00788  0700-BLD-PAYMENT.                                                EL344
00789 ******************************************************************EL344
00790 *     THIS PARAGRAPH BUILD THE PAYMENT PRINT LINE AS WELL AS     *EL344
00791 *     TOTALS CURRENT MONTH AND PREVIOUS MONTHS PAYMENTS.  FOR    *EL344
00792 *     A PAYMENT TO QUALIFY AS A CURRENT MONTH PAYMENT THE        *EL344
00793 *     FOLLOWING CONDITIONS MUST BE MET:                          *EL344
00794 *         1.  THE PAYMENT SELECT DATE MUST EQUAL THE RUN DATE    *EL344
00795 *         2.  THE CHECK WRITTEN MONTH AND YEAR MUST EQUAL THE    *EL344
00796 *             RUN MONTH AND YEAR                                 *EL344
00797 *     FOR A PAYMENT TO QUALIFY AS A PREVIOUS MONTH PAYMENT THE   *EL344
00798 *     FOLLOWING CONDITIONS MUST BE MET:                          *EL344
00799 *         1.  THE PAYMENT SELECT MONTH AND YEAR MUST EQUAL THE   *EL344
00800 *             RUN MONTH - 1 AND, IF NECESSARY, THE RUN YEAR - 1  *EL344
00801 *         2.  THE CHECK WRITTEN MONTH AND YEAR MUST EQUAL THE    *EL344
00802 *             RUN MONTH - 1 AND, IF NECESSARY, THE RUN YEAR - 1  *EL344
00803 ******************************************************************EL344
00804                                                                   EL344
00805      IF DTE-CLIENT IS EQUAL TO 'FIA'                              EL344
00806          IF AT-CHECK-WRITTEN-DT IS EQUAL TO LOW-VALUES OR SPACES  EL344
00807              GO TO 0600-TRAILER-LOOP.                             EL344
00808                                                                   EL344
00809      MOVE    AT-CHECK-WRITTEN-DT TO  DC-BIN-DATE-1.               EL344
00810      MOVE    SPACES              TO  DC-OPTION-CODE.              EL344
00811      PERFORM 1100-DATE-RTN      THRU 1100-DATE-RTN-EXIT.          EL344
00812      MOVE    DC-GREG-DATE-1-MDY  TO  WS-CHECK-WRITTEN-DATE.       EL344
00813      MOVE    DC-GREG-DATE-1-YMD  TO  WK-DATE.                     EL344
00814      MOVE    WK-MO               TO  DMO.                         EL344
00815      MOVE    WK-DA               TO  DDA.                         EL344
00816      MOVE    WK-YR               TO  DYR.                         EL344
00817      MOVE    AT-AMOUNT-PAID      TO  PM-AMT.                      EL344
00818                                                                   EL344
00819      MOVE RUN-MO  TO Z-MO.                                        EL344
00820      MOVE RUN-DA  TO Z-DA.                                        EL344
00821      MOVE RUN-YR  TO Z-YR.                                        EL344
00822      MOVE SPACES TO THIS-MONTH                                    EL344
00823                     THIS-YEAR                                     EL344
00824                     LAST-MONTH.                                   EL344
00825                                                                   EL344
00826      IF AT-PMT-SELECT-DT EQUAL LOW-VALUES                         EL344
00827         MOVE '**FUTURE**'        TO PM-BODY                       EL344
00828         GO TO 0700-PMT-REL.                                       EL344
00829                                                                   EL344
00830      IF AT-VOID-DT IS NOT GREATER THAN WS-RUN-BIN-DT              EL344
00831          IF  VOID-NOT-SELECTED                                    EL344
00832              NEXT SENTENCE                                        EL344
00833          ELSE                                                     EL344
00834              MOVE '** VOID **' TO PM-BODY                         EL344
00835              GO TO 0700-PMT-REL.                                  EL344
00836                                                                   EL344
00837      IF Z-YR = WK-YR                                              EL344
00838         MOVE 'Y' TO THIS-YEAR.                                    EL344
00839                                                                   EL344
00840      IF    (Z-YR = WK-YR AND                                      EL344
00841             Z-MO = WK-MO)                                         EL344
00842           AND                                                     EL344
00843            (Z-YR = WS-CK-YR AND                                   EL344
00844             Z-MO = WS-CK-MO)                                      EL344
00845            MOVE 'Y' TO THIS-MONTH.                                EL344
00846                                                                   EL344
00847      COMPUTE Z-MO = Z-MO - 1.                                     EL344
00848                                                                   EL344
00849      IF Z-MO = 0                                                  EL344
00850          MOVE 12   TO Z-MO                                        EL344
00851          IF Z-YR = 00                                                CL**6
00852              MOVE 99   TO Z-YR                                       CL**6
00853          ELSE                                                        CL**6
00854              SUBTRACT +1 FROM Z-YR.                                  CL**6
00855                                                                   EL344
00856      IF     (Z-YR = WK-YR AND                                     EL344
00857              Z-MO = WK-MO)                                        EL344
00858           AND                                                     EL344
00859             (Z-YR = WS-CK-YR AND                                  EL344
00860              Z-MO = WS-CK-MO)                                     EL344
00861              MOVE 'Y' TO LAST-MONTH.                              EL344
00862                                                                   EL344
00863 *    PERFORM 1500-CK-DATE.                                        EL344
00864                                                                   EL344
00865      IF THIS-MONTH = 'Y'   
122702         EVALUATE TRUE 
122702         WHEN ST-CLMTYP = LIFE-OVERRIDE-L1   
00867              ADD AT-AMOUNT-PAID TO PAY-TM-LIFE                    EL344

122702         WHEN ST-CLMTYP = AH-OVERRIDE-L1
00869              ADD AT-AMOUNT-PAID TO PAY-TM-AH

121603         WHEN ST-CLMTYP = 'G'
121603             ADD AT-AMOUNT-PAID TO PAY-TM-GAP

122702         WHEN ST-CLMTYP = 'I'
122702             ADD AT-AMOUNT-PAID TO PAY-TM-IU
052614
052614         WHEN ST-CLMTYP = 'F'
052614             ADD AT-AMOUNT-PAID TO PAY-TM-FAM
100518
022122         WHEN ST-CLMTYP = 'B'
022122             ADD AT-AMOUNT-PAID TO PAY-TM-BRV
022122
022122         WHEN ST-CLMTYP = 'H'
022122             ADD AT-AMOUNT-PAID TO PAY-TM-HOS
100518
100518         WHEN ST-CLMTYP = 'O'
100518             ADD AT-AMOUNT-PAID TO PAY-TM-OTH
122702         END-EVALUATE 
122702     END-IF.
00870                                                                   EL344
00871      IF  LAST-MONTH = 'Y'                                         EL344
122702         EVALUATE TRUE 
122702         WHEN ST-CLMTYP = LIFE-OVERRIDE-L1   
00867              ADD AT-AMOUNT-PAID TO PAY-LM-LIFE                    EL344

122702         WHEN ST-CLMTYP = AH-OVERRIDE-L1
00869              ADD AT-AMOUNT-PAID TO PAY-LM-AH

121603         WHEN ST-CLMTYP = 'G'
121603             ADD AT-AMOUNT-PAID TO PAY-LM-GAP

122702         WHEN ST-CLMTYP = 'I'
122702             ADD AT-AMOUNT-PAID TO PAY-LM-IU
052614
052614         WHEN ST-CLMTYP = 'F'
052614             ADD AT-AMOUNT-PAID TO PAY-LM-FAM
100518
022122         WHEN ST-CLMTYP = 'B'
022122             ADD AT-AMOUNT-PAID TO PAY-LM-BRV
022122
022122         WHEN ST-CLMTYP = 'H'
022122             ADD AT-AMOUNT-PAID TO PAY-LM-HOS
100518
100518         WHEN ST-CLMTYP = 'O'
100518             ADD AT-AMOUNT-PAID TO PAY-LM-OTH
122702         END-EVALUATE 
122702     END-IF.
00876                                                                   EL344
00877      MOVE 'PAYMT'        TO PM-TYPE.                              EL344
00878                                                                   EL344
00879      IF DTE-CLAIM-PAID-THRU-TO EQUAL ' '                          EL344
00880         MOVE 'PD THRU'      TO PM-THRU                            EL344
00881      ELSE                                                         EL344
00882         MOVE 'PD  TO '      TO PM-THRU.                           EL344
00883                                                                   EL344
00884      IF FINAL-PAYMENT          
00885          MOVE '*FINAL*' TO PM-THRU                                EL344
00886          IF  THIS-MONTH = 'Y'                                     EL344
122702             EVALUATE TRUE 
122702             WHEN ST-CLMTYP = LIFE-OVERRIDE-L1   
00867                  ADD AT-AMOUNT-PAID TO FIN-TM-LIFE  

122702             WHEN ST-CLMTYP = AH-OVERRIDE-L1
00869                  ADD AT-AMOUNT-PAID TO FIN-TM-AH

121603             WHEN ST-CLMTYP = 'G'
121603                 ADD AT-AMOUNT-PAID TO FIN-TM-GAP

122702             WHEN ST-CLMTYP = 'I'
122702                 ADD AT-AMOUNT-PAID TO FIN-TM-IU
052614
052614             WHEN ST-CLMTYP = 'F'
052614                 ADD AT-AMOUNT-PAID TO FIN-TM-FAM
100518
022122             WHEN ST-CLMTYP = 'B'
022122                 ADD AT-AMOUNT-PAID TO FIN-TM-BRV
022122
022122             WHEN ST-CLMTYP = 'H'
022122                 ADD AT-AMOUNT-PAID TO FIN-TM-HOS
100518
100518             WHEN ST-CLMTYP = 'O'
100518                 ADD AT-AMOUNT-PAID TO FIN-TM-OTH
122702             END-EVALUATE 

00891          ELSE                                                     EL344
00892              IF LAST-MONTH = 'Y'      
122702                 EVALUATE TRUE 
122702                 WHEN ST-CLMTYP = LIFE-OVERRIDE-L1   
00867                      ADD AT-AMOUNT-PAID TO FIN-LM-LIFE         

122702                 WHEN ST-CLMTYP = AH-OVERRIDE-L1
00869                      ADD AT-AMOUNT-PAID TO FIN-LM-AH

121603                 WHEN ST-CLMTYP = 'G'
121603                     ADD AT-AMOUNT-PAID TO FIN-LM-GAP

122702                 WHEN ST-CLMTYP = 'I'
122702                     ADD AT-AMOUNT-PAID TO FIN-LM-IU
052614
052614                 WHEN ST-CLMTYP = 'F'
052614                     ADD AT-AMOUNT-PAID TO FIN-LM-FAM
100518
022122                 WHEN ST-CLMTYP = 'B'
022122                     ADD AT-AMOUNT-PAID TO FIN-LM-BRV
022122
022122                 WHEN ST-CLMTYP = 'H'
022122                     ADD AT-AMOUNT-PAID TO FIN-LM-HOS
100518
100518                 WHEN ST-CLMTYP = 'O'
100518                     ADD AT-AMOUNT-PAID TO FIN-LM-OTH
122702                 END-EVALUATE 
122702             END-IF
122702         END-IF
122702     END-IF.
00897                                                                   EL344
00898      IF  PARTIAL-PAYMENT                                          EL344
00899          MOVE 'PARTIAL' TO PM-THRU.                               EL344
00900                                                                   EL344
00901      IF  LUMP-SUM-PAYMENT                                         EL344
00902          MOVE 'LUMP-S*' TO PM-THRU.                               EL344
00903                                                                   EL344
00904      IF    ONLINE-AUTO-PMT                                        EL344
00905        AND PARTIAL-PAYMENT                                        EL344
00906            MOVE 'AUTO-P' TO PM-THRU.                              EL344
00907                                                                   EL344
00908      IF    ONLINE-AUTO-PMT                                        EL344
00909        AND FINAL-PAYMENT                                          EL344
00910            MOVE 'AUTO-F' TO PM-THRU.                              EL344
00911                                                                   EL344
00912      IF  ADDITIONAL-PAYMENT                                       EL344
00913          MOVE '**ADDITIONAL**' TO PM-BODY                         EL344
00914          GO TO 0700-PMT-REL.                                      EL344
00915                                                                   EL344
00916      IF  CHARGEABLE-EXPENSE                                       EL344
00917          MOVE 'EXPENSE' TO PM-THRU                                EL344
00918          MOVE SPACES    TO PM-TYPE.                               EL344
00919                                                                   EL344
00920      IF DTE-CLAIM-PAID-THRU-TO EQUAL ' '                          EL344
00921         MOVE    AT-PAID-THRU-DT     TO  DC-BIN-DATE-1             EL344
00922         MOVE    SPACES              TO  DC-OPTION-CODE            EL344
00923         PERFORM 1100-DATE-RTN      THRU 1100-DATE-RTN-EXIT        EL344
00924         MOVE    DC-GREG-DATE-1-YMD  TO  WK-DATE                   EL344
00925      ELSE                                                         EL344
00926         MOVE    AT-PAID-THRU-DT     TO  DC-BIN-DATE-1             EL344
00927         MOVE    '6'                 TO  DC-OPTION-CODE            EL344
00928         MOVE    +1                  TO  DC-ELAPSED-DAYS           EL344
00929         MOVE    +0                  TO  DC-ELAPSED-MONTHS         EL344
00930         PERFORM 1100-DATE-RTN      THRU 1100-DATE-RTN-EXIT        EL344
00931         MOVE    DC-GREG-DATE-1-YMD  TO  WK-DATE.                  EL344
00932                                                                   EL344
00933      MOVE    WK-MO               TO  PDTMO.                       EL344
00934      MOVE    WK-DA               TO  PDTDA.                       EL344
00935      MOVE    WK-YR               TO  PDTYR.                       EL344
00936      MOVE    '/'                 TO  DSL1 DSL2.                   EL344
00937                                                                   EL344
00938  0700-PMT-REL.                                                    EL344
00939      MOVE PMT-MES  TO DMESS.                                      EL344
00940      MOVE DET-PRT  TO PRT.                                        EL344
00941      GO TO 0550-PRT-IT.                                           EL344
00942  EJECT                                                            EL344
00943  0800-BLD-CORRESPONDENCE.                                         EL344
00944      MOVE    AT-RECORDED-DT             TO  DC-BIN-DATE-1.        EL344
00945      MOVE    SPACES                     TO  DC-OPTION-CODE.       EL344
00946      PERFORM 1100-DATE-RTN             THRU 1100-DATE-RTN-EXIT.   EL344
00947      MOVE    DC-GREG-DATE-1-YMD         TO  WK-DATE.              EL344
00948      MOVE    WK-MO                      TO  DMO.                  EL344
00949      MOVE    WK-DA                      TO  DDA.                  EL344
00950      MOVE    WK-YR                      TO  DYR.                  EL344
00951      MOVE    'LETTER'                   TO  FM-TYPE.              EL344
00952      MOVE    AT-STD-LETTER-FORM         TO  FM-KIND.              EL344
00953                                                                   EL344
00954  0800-CK-LETTER-SENT.                                             EL344
00955      IF AT-LETTER-SENT-DT = SPACES OR LOW-VALUES                  EL344
00956         GO TO 0800-CK-LETTER-RESEND.                              EL344
00957                                                                   EL344
00958      MOVE    AT-LETTER-SENT-DT   TO  DC-BIN-DATE-1.               EL344
00959      MOVE    SPACES              TO  DC-OPTION-CODE.              EL344
00960      PERFORM 1100-DATE-RTN      THRU 1100-DATE-RTN-EXIT.          EL344
00961      MOVE    DC-GREG-DATE-1-YMD  TO  WK-DATE.                     EL344
00962      MOVE    WK-MO               TO  FDTMO.                       EL344
00963      MOVE    WK-DA               TO  FDTDA.                       EL344
00964      MOVE    WK-YR               TO  FDTYR.                       EL344
00965      MOVE    'SENT'              TO  FM-SENT.                     EL344
00966      MOVE    '/'                 TO  FSL1                         EL344
00967                                      FSL2.                        EL344
00968      MOVE    FORM-MES            TO  DMESS.                       EL344
00969      MOVE    DET-PRT             TO  PRT.                         EL344
00970      PERFORM 1400-PRINT-RTN     THRU 1400-PRINT-RTN-EXIT.         EL344
00971      ADD     +1                  TO  LN-CT.                       EL344
00972                                                                   EL344
00973      IF LN-CT GREATER +56                                         EL344
00974          PERFORM 1200-SUB-HEADING THRU 1200-SUB-HEADING-EXIT.     EL344
00975                                                                   EL344
00976  0800-CK-LETTER-RESEND.                                           EL344
00977                                                                   EL344
00978      IF AT-RESEND-PRINT-DATE = SPACES OR LOW-VALUES               EL344
00979         GO TO 0800-CK-LETTER-RECEIPT.                             EL344
00980                                                                   EL344
00981      MOVE AT-RESEND-PRINT-DATE TO DC-BIN-DATE-1.                  EL344
00982      MOVE SPACES TO DC-OPTION-CODE.                               EL344
00983      PERFORM 1100-DATE-RTN THRU 1100-DATE-RTN-EXIT.               EL344
00984      MOVE DC-GREG-DATE-1-YMD TO WK-DATE.                          EL344
00985                                                                   EL344
00986      MOVE WK-MO TO FDTMO.                                         EL344
00987      MOVE WK-DA TO FDTDA.                                         EL344
00988      MOVE WK-YR TO FDTYR.                                         EL344
00989                                                                   EL344
00990      MOVE 'RESENT' TO FM-SENT.                                    EL344
00991      MOVE '/' TO FSL1 FSL2.                                       EL344
00992      MOVE FORM-MES TO DMESS.                                      EL344
00993      MOVE DET-PRT TO PRT.                                         EL344
00994                                                                   EL344
00995      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
00996      ADD +1 TO LN-CT.                                             EL344
00997      IF LN-CT GREATER +56                                         EL344
00998          PERFORM 1200-SUB-HEADING THRU 1200-SUB-HEADING-EXIT.     EL344
00999                                                                   EL344
01000  0800-CK-LETTER-RECEIPT.                                          EL344
01001      IF   AT-LETTER-ANSWERED-DT = SPACES                          EL344
01002        OR LOW-VALUES                                              EL344
01003           GO TO 0600-TRAILER-LOOP.                                EL344
01004                                                                   EL344
01005      MOVE    AT-LETTER-ANSWERED-DT TO  DC-BIN-DATE-1.             EL344
01006      MOVE    SPACES               TO  DC-OPTION-CODE.             EL344
01007      PERFORM 1100-DATE-RTN       THRU 1100-DATE-RTN-EXIT.         EL344
01008      MOVE    DC-GREG-DATE-1-YMD   TO  WK-DATE.                    EL344
01009      MOVE    WK-MO                TO  FDTMO.                      EL344
01010      MOVE    WK-DA                TO  FDTDA.                      EL344
01011      MOVE    WK-YR                TO  FDTYR.                      EL344
01012      MOVE    'RECVD'              TO  FM-SENT.                    EL344
01013      MOVE    '/'                  TO  FSL1 FSL2.                  EL344
01014      MOVE    FORM-MES             TO  DMESS.                      EL344
01015      MOVE    DET-PRT              TO  PRT.                        EL344
01016      GO TO 0550-PRT-IT.                                           EL344
01017  EJECT                                                            EL344
01018  0900-BLD-DENIAL.                                                 EL344
01019      MOVE    AT-RECORDED-DT      TO  DC-BIN-DATE-1.               EL344
01020      MOVE    SPACES              TO  DC-OPTION-CODE.              EL344
01021      PERFORM 1100-DATE-RTN      THRU 1100-DATE-RTN-EXIT.          EL344
01022      MOVE    DC-GREG-DATE-1-YMD  TO  WK-DATE.                     EL344
01023      MOVE    WK-MO               TO  DMO.                         EL344
01024      MOVE    WK-DA               TO  DDA.                         EL344
01025      MOVE    WK-YR               TO  DYR.                         EL344
01026      MOVE    'DENIAL'            TO  PM-TYPE.                     EL344
01027      MOVE    PMT-MES             TO  DMESS.                       EL344
01028      MOVE    DET-PRT             TO  PRT.                         EL344
01029      GO TO 0550-PRT-IT.                                           EL344
01030  EJECT                                                            EL344
01031  0900-BLD-FORM-CONTROL.                                           EL344
01032      MOVE    AT-RECORDED-DT     TO  DC-BIN-DATE-1.                EL344
01033      MOVE    SPACES             TO  DC-OPTION-CODE.               EL344
01034      PERFORM 1100-DATE-RTN     THRU 1100-DATE-RTN-EXIT.           EL344
01035      MOVE    DC-GREG-DATE-1-YMD TO  WK-DATE.                      EL344
01036      MOVE    WK-MO              TO  DMO.                          EL344
01037      MOVE    WK-DA              TO  DDA.                          EL344
01038      MOVE    WK-YR              TO  DYR.                          EL344
01039      MOVE    'FORMS'            TO  FM-TYPE.                      EL344
01040                                                                   EL344
01041      IF INITIAL-FORM                                              EL344
01042        MOVE 'INITIAL'  TO FM-KIND.                                EL344
01043                                                                   EL344
01044      IF PROGRESS-FORM                                             EL344
01045        MOVE 'PROGRESS' TO FM-KIND.                                EL344
01046                                                                   EL344
01047  0900-CK-FORM-SENT.                                               EL344
01048                                                                   EL344
01049      IF AT-FORM-SEND-ON-DT = SPACES OR LOW-VALUES                 EL344
01050         GO TO 0900-CK-FORM-RECEIPT.                               EL344
01051                                                                   EL344
01052      MOVE AT-FORM-SEND-ON-DT TO DC-BIN-DATE-1.                    EL344
01053      MOVE SPACES TO DC-OPTION-CODE.                               EL344
01054      PERFORM 1100-DATE-RTN THRU 1100-DATE-RTN-EXIT.               EL344
01055      MOVE DC-GREG-DATE-1-YMD TO WK-DATE.                          EL344
01056                                                                   EL344
01057      MOVE WK-MO TO FDTMO.                                         EL344
01058      MOVE WK-DA TO FDTDA.                                         EL344
01059      MOVE WK-YR TO FDTYR.                                         EL344
01060                                                                   EL344
01061      MOVE 'SENT' TO FM-SENT.                                      EL344
01062      MOVE '/' TO FSL1 FSL2.                                       EL344
01063      MOVE FORM-MES TO DMESS.                                      EL344
01064      MOVE DET-PRT TO PRT.                                         EL344
01065                                                                   EL344
01066      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01067      ADD +1 TO LN-CT.                                             EL344
01068      IF LN-CT GREATER +56                                         EL344
01069          PERFORM 1200-SUB-HEADING THRU 1200-SUB-HEADING-EXIT.     EL344
01070                                                                   EL344
01071  0900-CK-FORM-RECEIPT.                                            EL344
01072      IF   AT-FORM-ANSWERED-DT = SPACES                            EL344
01073        OR LOW-VALUES                                              EL344
01074           GO TO 0900-CK-RE-SENT.                                  EL344
01075                                                                   EL344
01076      MOVE AT-FORM-ANSWERED-DT TO DC-BIN-DATE-1.                   EL344
01077      MOVE SPACES TO DC-OPTION-CODE.                               EL344
01078      PERFORM 1100-DATE-RTN THRU 1100-DATE-RTN-EXIT.               EL344
01079      MOVE DC-GREG-DATE-1-YMD TO WK-DATE.                          EL344
01080                                                                   EL344
01081      MOVE WK-MO TO FDTMO.                                         EL344
01082      MOVE WK-DA TO FDTDA.                                         EL344
01083      MOVE WK-YR TO FDTYR.                                         EL344
01084                                                                   EL344
01085      MOVE 'RECVD' TO FM-SENT.                                     EL344
01086      MOVE '/' TO FSL1 FSL2.                                       EL344
01087      MOVE FORM-MES TO DMESS.                                      EL344
01088      MOVE DET-PRT TO PRT.                                         EL344
01089                                                                   EL344
01090      GO TO 0550-PRT-IT.                                           EL344
01091                                                                   EL344
01092  0900-CK-RE-SENT.                                                 EL344
01093      IF   AT-FORM-REPRINT-DT = SPACES                             EL344
01094        OR LOW-VALUES                                              EL344
01095           GO TO 0600-TRAILER-LOOP.                                EL344
01096                                                                   EL344
01097      MOVE AT-FORM-REPRINT-DT TO DC-BIN-DATE-1.                    EL344
01098      MOVE SPACES TO DC-OPTION-CODE.                               EL344
01099      PERFORM 1100-DATE-RTN THRU 1100-DATE-RTN-EXIT.               EL344
01100      MOVE DC-GREG-DATE-1-YMD TO WK-DATE.                          EL344
01101                                                                   EL344
01102      MOVE WK-MO TO FDTMO.                                         EL344
01103      MOVE WK-DA TO FDTDA.                                         EL344
01104      MOVE WK-YR TO FDTYR.                                         EL344
01105                                                                   EL344
01106      MOVE 'RESENT' TO FM-SENT.                                    EL344
01107      MOVE '/' TO FSL1 FSL2.                                       EL344
01108      MOVE FORM-MES TO DMESS.                                      EL344
01109      MOVE DET-PRT TO PRT.                                         EL344
01110                                                                   EL344
01111      GO TO 0550-PRT-IT.                                           EL344
01112                                                                   EL344
01113  1000-EXIT.                                                       EL344
01114      EXIT.                                                        EL344
01115                                                                   EL344
01116  1100-DATE-RTN SECTION.                                           EL344
01117                                                                   EL344
01118      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL344
01119                                                                   EL344
01120      IF DC-ERROR-CODE NOT = SPACE                                 EL344
01121         MOVE ZEROS TO DC-CONVERSION-DATES.                        EL344
01122                                                                   EL344
01123  1100-DATE-RTN-EXIT.                                              EL344
01124      EXIT.                                                        EL344
01125                                                                   EL344
01126  1200-HEADING SECTION.                                            EL344
01127                                                                   EL344
01128  1200-REPORT-HEADING.                                             EL344
01129      ADD +1 TO PG-NO.                                             EL344
01130      MOVE PG-NO TO HD-PG.                                         EL344
01131      MOVE HD-1 TO PRT.                                            EL344
01132      MOVE '1' TO P-CTL.                                           EL344
01133      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01134      MOVE HD-2 TO PRT.                                            EL344
01135      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01136      MOVE HD-3 TO PRT.                                            EL344
01137      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01138      MOVE +3 TO LN-CT.                                            EL344
01139                                                                   EL344
01140  1200-MOVE-ACCT.                                                  EL344
01141                                                                   EL344
01142      IF ST-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     EL344
01143          GO TO 1200-MOVE-PRODUCER.                                EL344
01144                                                                   EL344
01145      IF  ST-ACC = LAST-ACC AND                                    EL344
01146          ST-SYSTEM-IDENTIFIER IS EQUAL TO LAST-SYS-ID             EL344
01147          GO TO 1200-SUB-HEADING-EXIT.                             EL344
01148                                                                   EL344
01149      PERFORM 1300-FIND-ACCT THRU 1300-FIND-ACCT-EXIT.             EL344
01150      MOVE ST-ACC TO SH1-ACNO.                                     EL344
01151                                                                   EL344
01152      IF  ERACCT-STATUS = '23'                                     EL344
01153          MOVE 'NO ACCOUNT NAME' TO SH1-ACNA                       EL344
01154      ELSE                                                         EL344
01155          MOVE AM-NAME           TO SH1-ACNA                       EL344
pemmod         if am-report-code-1 (1:1) = low-values
pemmod            move spaces to am-report-code-1
pemmod         end-if
pemmod         if am-report-code-2 (1:1) = low-values
pemmod            move spaces to am-report-code-2
pemmod         end-if
pemmod         move am-report-code-1 to hd3-rptcd1
pemmod         move am-report-code-2 to hd3-rptcd2
pemmod     end-if
01156                                                                   EL344
01157      GO TO 1200-SUB-HEADING.                                      EL344
01158                                                                   EL344
01159  1200-MOVE-PRODUCER.                                              EL344
01160                                                                   EL344
01161      IF ST-ACC = LAST-ACC AND                                     EL344
01162         ST-SYSTEM-IDENTIFIER IS EQUAL TO LAST-SYS-ID              EL344
01163          GO TO 1200-SUB-HEADING-EXIT.                             EL344
01164                                                                   EL344
01165      PERFORM 1350-FIND-PRODUCER THRU 1350-EXIT.                   EL344
01166      MOVE ST-ACC                     TO  SH1-ACNO.                EL344
01167                                                                   EL344
01168      IF MPPROD-STATUS IS EQUAL TO '23'                            EL344
01169          MOVE 'NO PRODUCER NAME'     TO  SH1-ACNA                 EL344
01170      ELSE                                                         EL344
01171          MOVE PD-NAME                TO  SH1-ACNA.                EL344
01172                                                                   EL344
01173  1200-SUB-HEADING.                                                EL344
01174      PERFORM 1200-REPORT-HEADING.                                 EL344
01175      MOVE SUB-HD1 TO PRT.                                         EL344
01176      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01177      MOVE SUB-HD2 TO PRT.                                         EL344
01178      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01179      MOVE SUB-HD3 TO PRT.                                         EL344
01180      PERFORM 1400-PRINT-RTN THRU 1400-PRINT-RTN-EXIT.             EL344
01181      ADD +4 TO LN-CT.                                             EL344
01182                                                                   EL344
01183  1200-SUB-HEADING-EXIT.                                           EL344
01184      EXIT.                                                        EL344
01185  EJECT                                                            EL344
01186  1300-FIND-ACCT SECTION.                                          EL344
01187                                                                   EL344
01188      MOVE DTE-CLASIC-COMPANY-CD TO AM-COMPANY-CD.                 EL344
01189      MOVE ST-CAR     TO AM-CARRIER.                               EL344
01190      MOVE ST-COMP    TO AM-GROUPING.                              EL344
01191      MOVE ST-STATE   TO AM-STATE.                                 EL344
01192      MOVE ST-ACC     TO AM-ACCOUNT.                               EL344
01193      MOVE ST-EFF-DT  TO AM-EXPIRATION-DT.                         EL344
01194                                                                   EL344
01195      START ERACCT   KEY NOT LESS THAN AM-CONTROL-PRIMARY.         EL344
01196                                                                   EL344
01197      IF ERACCT-STAT-1 NOT = ZERO                                  EL344
01198          MOVE ERACCT-STATUS          TO  WS-ABEND-FILE-STATUS     EL344
01199          MOVE 'ERROR OCCURED START - ERACCT'                      EL344
01200                                  TO  WS-ABEND-MESSAGE             EL344
01201          GO TO ABEND-PGM.                                         EL344
01202                                                                   EL344
01203      READ ERACCT NEXT.                                            EL344
01204                                                                   EL344
01205      IF   ERACCT-STATUS = '23'                                    EL344
01206          GO TO 1300-NOFOUND.                                      EL344
01207                                                                   EL344
01208      IF ERACCT-STAT-1 NOT = ZERO                                  EL344
01209          MOVE ERACCT-STATUS          TO  WS-ABEND-FILE-STATUS     EL344
01210          MOVE 'ERROR OCCURED READNEXT - ERACCT'                   EL344
01211                                  TO  WS-ABEND-MESSAGE             EL344
01212          GO TO ABEND-PGM.                                         EL344
01213                                                                   EL344
01214      IF ST-CAR    NOT = AM-CARRIER  AND                           EL344
01215         ST-COMP   NOT = AM-GROUPING AND                           EL344
01216         ST-STATE  NOT = AM-STATE    AND                           EL344
01217         ST-ACC    NOT = AM-ACCOUNT                                EL344
01218         MOVE '23'                TO ERACCT-STATUS.                EL344
01219                                                                   EL344
01220  1300-NOFOUND.                                                    EL344
01221      MOVE ST-ACC TO LAST-ACC.                                     EL344
01222      MOVE ST-SYSTEM-IDENTIFIER       TO  LAST-SYS-ID.             EL344
01223                                                                   EL344
01224  1300-FIND-ACCT-EXIT.                                             EL344
01225      EXIT.                                                        EL344
01226  EJECT                                                            EL344
01227  1350-FIND-PRODUCER SECTION.                                      EL344
01228                                                                   EL344
01229      MOVE DTE-CLASIC-COMPANY-CD      TO  PD-COMPANY-CD.           EL344
01230      MOVE ST-CAR                     TO  PD-CARRIER.              EL344
01231      MOVE ST-COMP                    TO  PD-GROUPING.             EL344
01232      MOVE ST-STATE                   TO  PD-STATE.                EL344
01233      MOVE ST-ACC                     TO  PD-PRODUCER              EL344
01234      MOVE ST-EFF-DT                  TO  PD-EXPIRE-DATE.          EL344
01235                                                                   EL344
01236      START MPPROD   KEY NOT LESS THAN PD-CONTROL-PRIMARY.         EL344
01237                                                                   EL344
01238      IF MPPROD-STAT-1 NOT = ZERO                                  EL344
01239          MOVE MPPROD-STATUS          TO  WS-ABEND-FILE-STATUS     EL344
01240          MOVE 'ERROR OCCURED START - MPPROD'                      EL344
01241                                  TO  WS-ABEND-MESSAGE             EL344
01242          GO TO ABEND-PGM.                                         EL344
01243                                                                   EL344
01244      READ MPPROD NEXT.                                            EL344
01245                                                                   EL344
01246      IF MPPROD-STATUS = '23'                                      EL344
01247          GO TO 1350-NOFOUND.                                      EL344
01248                                                                   EL344
01249      IF MPPROD-STAT-1 NOT = ZERO                                  EL344
01250          MOVE MPPROD-STATUS          TO  WS-ABEND-FILE-STATUS     EL344
01251          MOVE 'ERROR OCCURED READNEXT - MPPROD'                   EL344
01252                                      TO  WS-ABEND-MESSAGE         EL344
01253          GO TO ABEND-PGM.                                         EL344
01254                                                                   EL344
01255      IF ST-CAR    NOT = PD-CARRIER  AND                           EL344
01256         ST-COMP   NOT = PD-GROUPING AND                           EL344
01257         ST-STATE  NOT = PD-STATE    AND                           EL344
01258         ST-ACC    NOT = PD-PRODUCER                               EL344
01259         MOVE '23'                    TO  MPPROD-STATUS.           EL344
01260                                                                   EL344
01261  1350-NOFOUND.                                                    EL344
01262      MOVE ST-ACC                     TO  LAST-ACC.                EL344
01263      MOVE ST-SYSTEM-IDENTIFIER       TO  LAST-SYS-ID.             EL344
01264                                                                   EL344
01265  1350-EXIT.                                                       EL344
01266      EXIT.                                                        EL344
01267  EJECT                                                            EL344
01268  1400-PRINT-RTN SECTION.                                          EL344
01269                                                                   EL344
01270      MOVE P-CTL TO X.                                             EL344
01271      PERFORM 1400-PRINT-LINE THRU 1400-PRINT-LINE-EXIT.           EL344
01272      MOVE SPACES TO PMT-MES                                       EL344
01273                     DMESS                                         EL344
01274                     DMEMB                                         EL344
01275                     DLOAN                                         EL344
01276                     FORM-MES.                                     EL344
01277                                                                   EL344
01278  1400-PRINT-RTN-EXIT.                                             EL344
01279      EXIT.                                                        EL344
01280                                                                   EL344
01281  1400-PRINT-LINE SECTION.    COPY ELCPRT2X.                       EL344
01282                                                                   EL344
01283  1400-PRINT-LINE-EXIT.                                            EL344
01284      EXIT.                                                        EL344
01285                                                                   EL344
01286  1500-CK-DATE SECTION.                                            EL344
01287                                                                   EL344
01288      MOVE RUN-MO  TO Z-MO.                                        EL344
01289      MOVE RUN-DA  TO Z-DA.                                        EL344
01290      MOVE RUN-YR  TO Z-YR.                                        EL344
01291      MOVE SPACES TO THIS-MONTH                                    EL344
01292                     THIS-YEAR                                     EL344
01293                     LAST-MONTH.                                   EL344
01294                                                                   EL344
01295      IF Z-YR = WK-YR                                              EL344
01296         MOVE 'Y' TO THIS-YEAR.                                    EL344
01297                                                                   EL344
01298      IF    Z-YR = WK-YR                                           EL344
01299        AND Z-MO = WK-MO                                           EL344
01300            MOVE 'Y' TO THIS-MONTH.                                EL344
01301                                                                   EL344
01302      COMPUTE Z-MO = Z-MO - 1.                                     EL344
01303                                                                   EL344
01304      IF Z-MO = 0                                                  EL344
01305          MOVE 12   TO Z-MO                                        EL344
01306          IF Z-YR = 00                                                CL**6
01307              MOVE 99   TO Z-YR                                       CL**6
01308          ELSE                                                        CL**6
01309              SUBTRACT +1 FROM Z-YR.                                  CL**6
01310                                                                   EL344
01311      IF      Z-YR = WK-YR                                         EL344
01312          AND Z-MO = WK-MO                                         EL344
01313              MOVE 'Y' TO LAST-MONTH.                              EL344
01314                                                                   EL344
01315  9999-EOJ SECTION.                                                EL344
01316                                                                   EL344
011204     IF WS-SORT-REC-CNT > +0
011204         CONTINUE
011204     ELSE
011204         GO TO 9999-CLOSE
011204     END-IF.

01317      PERFORM 1200-REPORT-HEADING.
01318      MOVE TOTAL-HEADING          TO  PRT.
01319      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.
01320      MOVE    EST-TM-AH           TO  TL-CNT-AH.
01321      MOVE    EST-TM-LIFE         TO  TL-CNT-LIFE.
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE EST-TM-GAP          TO  TL-CNT-GAP
122702         MOVE EST-TM-IU          TO  TL-CNT-IU
052614         MOVE EST-TM-FAM         TO  TL-CNT-FAM
022122         MOVE EST-TM-BRV         TO  TL-CNT-BRV
022122         MOVE EST-TM-HOS         TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE EST-TM-OTH             TO  TL-CNT-OTH

01322      MOVE ' CLAIMS ESTABLISHED - THIS MONTH' TO TL-MES.
01323      MOVE TOTAL-CNT              TO  PRT.
01324      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.

01325      MOVE    EST-LM-AH            TO  TL-CNT-AH.                  EL344
01326      MOVE    EST-LM-LIFE          TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE EST-LM-GAP          TO  TL-CNT-GAP
122702         MOVE EST-LM-IU           TO  TL-CNT-IU
052614         MOVE EST-LM-FAM          TO  TL-CNT-FAM
022122         MOVE EST-LM-BRV          TO  TL-CNT-BRV
022122         MOVE EST-LM-HOS          TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE EST-LM-OTH              TO  TL-CNT-OTH

01327      MOVE    '                    - LAST MONTH' TO TL-MES.        EL344
01328      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01329      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01330      MOVE    EST-TY-AH            TO  TL-CNT-AH.                  EL344
01331      MOVE    EST-TY-LIFE          TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE EST-TY-GAP          TO  TL-CNT-GAP
122702         MOVE EST-TY-IU           TO  TL-CNT-IU
052614         MOVE EST-TY-FAM          TO  TL-CNT-FAM
022122         MOVE EST-TY-BRV          TO  TL-CNT-BRV
022122         MOVE EST-TY-HOS          TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE EST-TY-OTH              TO  TL-CNT-OTH
01332      MOVE    '                    - THIS YEAR' TO TL-MES.         EL344
01333      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01334      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01335      MOVE    TOT-OPEN-AH          TO  TL-CNT-AH.                  EL344
01336      MOVE    TOT-OPEN-LIFE        TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE TOT-OPEN-GAP        TO  TL-CNT-GAP
122702         MOVE TOT-OPEN-IU         TO  TL-CNT-IU
052614         MOVE TOT-OPEN-FAM        TO  TL-CNT-FAM
022122         MOVE TOT-OPEN-BRV        TO  TL-CNT-BRV
022122         MOVE TOT-OPEN-HOS        TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE TOT-OPEN-OTH            TO  TL-CNT-OTH
01337      MOVE    ' TOTAL OPEN CLAIMS' TO  TL-MES.                     EL344
01338      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01339      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01340      MOVE    TOT-CLOSED-AH        TO  TL-CNT-AH.                  EL344
01341      MOVE    TOT-CLOSED-LIFE      TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE TOT-CLOSED-GAP      TO  TL-CNT-GAP
122702         MOVE TOT-CLOSED-IU       TO  TL-CNT-IU
052614         MOVE TOT-CLOSED-FAM      TO  TL-CNT-FAM
022122         MOVE TOT-CLOSED-BRV      TO  TL-CNT-BRV
022122         MOVE TOT-CLOSED-HOS      TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE TOT-CLOSED-OTH          TO  TL-CNT-OTH
01342      MOVE    ' TOTAL CLOSED CLAIMS' TO TL-MES.                    EL344
01343      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01344      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01345      MOVE    DEN-TM-AH            TO  TL-CNT-AH.                  EL344
01346      MOVE    DEN-TM-LIFE          TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE DEN-TM-GAP          TO  TL-CNT-GAP
122702         MOVE DEN-TM-IU           TO  TL-CNT-IU
052614         MOVE DEN-TM-FAM          TO  TL-CNT-FAM
022122         MOVE DEN-TM-BRV          TO  TL-CNT-BRV
022122         MOVE DEN-TM-HOS          TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE DEN-TM-OTH              TO  TL-CNT-OTH
01347      MOVE    ' CLAIMS DENIED  - THIS MONTH' TO TL-MES.            EL344
01348      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01349      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01350      MOVE    DEN-LM-AH            TO  TL-CNT-AH.                  EL344
01351      MOVE    DEN-LM-LIFE          TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE DEN-LM-GAP          TO  TL-CNT-GAP
122702         MOVE DEN-LM-IU           TO  TL-CNT-IU
052614         MOVE DEN-LM-FAM          TO  TL-CNT-FAM
022122         MOVE DEN-LM-BRV          TO  TL-CNT-BRV
022122         MOVE DEN-LM-HOS          TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE DEN-LM-OTH              TO  TL-CNT-OTH
01352      MOVE    '                - LAST MONTH' TO TL-MES.            EL344
01353      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01354      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01355      MOVE    DEN-TY-AH            TO  TL-CNT-AH.                  EL344
01356      MOVE    DEN-TY-LIFE          TO  TL-CNT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE DEN-TY-GAP          TO  TL-CNT-GAP
122702         MOVE DEN-TY-IU           TO  TL-CNT-IU
052614         MOVE DEN-TY-FAM          TO  TL-CNT-FAM
022122         MOVE DEN-TY-BRV          TO  TL-CNT-BRV
022122         MOVE DEN-TY-HOS          TO  TL-CNT-HOS
122702     END-IF.
100518     MOVE DEN-TY-OTH              TO  TL-CNT-OTH
01357      MOVE    '                - THIS YEAR' TO TL-MES.             EL344
01358      MOVE    TOTAL-CNT            TO  PRT.                        EL344
01359      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01360      MOVE    PAY-TM-AH            TO  TL-AMT-AH.                  EL344
01361      MOVE    PAY-TM-LIFE          TO  TL-AMT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE PAY-TM-GAP          TO  TL-AMT-GAP
122702         MOVE PAY-TM-IU           TO  TL-AMT-IU
052614         MOVE PAY-TM-FAM          TO  TL-AMT-FAM
022122         MOVE PAY-TM-BRV          TO  TL-AMT-BRV
022122         MOVE PAY-TM-HOS          TO  TL-AMT-HOS
122702     END-IF.
100518     MOVE PAY-TM-OTH              TO  TL-AMT-OTH
01362      MOVE    ' PAYMENTS MADE  - THIS MONTH' TO TL-MESA.           EL344
01363      MOVE    TOTAL-AMT            TO  PRT.                        EL344
01364      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01365      MOVE    PAY-LM-AH            TO  TL-AMT-AH.                  EL344
01366      MOVE    PAY-LM-LIFE          TO  TL-AMT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE PAY-LM-GAP          TO  TL-AMT-GAP
122702         MOVE PAY-LM-IU           TO  TL-AMT-IU
052614         MOVE PAY-LM-FAM          TO  TL-AMT-FAM
022122         MOVE PAY-LM-BRV          TO  TL-AMT-BRV
022122         MOVE PAY-LM-HOS          TO  TL-AMT-HOS
122702     END-IF.
100518     MOVE PAY-LM-OTH              TO  TL-AMT-OTH
01367      MOVE    '                - LAST MONTH' TO TL-MESA.           EL344
01368      MOVE    TOTAL-AMT            TO  PRT.                        EL344
01369      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01370      MOVE    FIN-TM-AH            TO  TL-AMT-AH.                  EL344
01371      MOVE    FIN-TM-LIFE          TO  TL-AMT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE FIN-TM-GAP          TO  TL-AMT-GAP
122702         MOVE FIN-TM-IU           TO  TL-AMT-IU
052614         MOVE FIN-TM-FAM          TO  TL-AMT-FAM
022122         MOVE FIN-TM-BRV          TO  TL-AMT-BRV
022122         MOVE FIN-TM-HOS          TO  TL-AMT-HOS
122702     END-IF.
100518     MOVE FIN-TM-OTH              TO  TL-AMT-OTH
01372      MOVE    ' FINAL PAYMENTS - THIS MONTH' TO TL-MESA.           EL344
01373      MOVE    TOTAL-AMT            TO  PRT.                        EL344
01374      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344

01375      MOVE    FIN-LM-AH            TO  TL-AMT-AH.                  EL344
01376      MOVE    FIN-LM-LIFE          TO  TL-AMT-LIFE.                EL344
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
121603         MOVE FIN-LM-GAP          TO  TL-AMT-GAP
122702         MOVE FIN-LM-IU           TO  TL-AMT-IU
052614         MOVE FIN-LM-FAM          TO  TL-AMT-FAM
022122         MOVE FIN-LM-BRV          TO  TL-AMT-BRV
022122         MOVE FIN-LM-HOS          TO  TL-AMT-HOS
122702     END-IF.
100518     MOVE FIN-LM-OTH              TO  TL-AMT-OTH
01377      MOVE    '               - LAST MONTH' TO TL-MESA.            EL344
01378      MOVE    TOTAL-AMT            TO  PRT.                        EL344
01379      PERFORM 1400-PRINT-RTN      THRU 1400-PRINT-RTN-EXIT.        EL344
01380                                                                   EL344
01381  9999-CLOSE.              COPY ELCPRTCX.                          EL344
01382                                                                   EL344
01383      CLOSE ERACCT ELCERT ELTRLR MPPLCY MPPROD PRINTER.            EL344
01384                                                                   EL344
01385      IF ERACCT-STAT-1 NOT = ZERO                                  EL344
01386          MOVE ERACCT-STATUS      TO  WS-ABEND-FILE-STATUS         EL344
01387          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      EL344
01388                                  TO  WS-ABEND-MESSAGE             EL344
01389          GO TO ABEND-PGM.                                         EL344
01390                                                                   EL344
01391      IF CM-STAT-1 NOT = ZERO                                      EL344
01392          MOVE CM-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
01393          MOVE 'ERROR OCCURED CLOSE - ELCERT'                      EL344
01394                                  TO  WS-ABEND-MESSAGE             EL344
01395          GO TO ABEND-PGM.                                         EL344
01396                                                                   EL344
01397      IF AT-STAT-1 NOT = ZERO                                      EL344
01398          MOVE AT-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
01399          MOVE 'ERROR OCCURED CLOSE - ELTRLR'                      EL344
01400                                  TO  WS-ABEND-MESSAGE             EL344
01401          GO TO ABEND-PGM.                                         EL344
01402                                                                   EL344
unix  *    IF PM-STAT-1 NOT = ZERO                                      EL344
unix  *        MOVE PD-STATUS          TO  WS-ABEND-FILE-STATUS         EL344
unix  *        MOVE 'ERROR OCCURED CLOSE - MPPLCY'                      EL344
unix  *                                TO  WS-ABEND-MESSAGE             EL344
unix  *        GO TO ABEND-PGM.                                         EL344
unix  *                                                                 EL344
unix  *    IF MPPROD-STAT-1 NOT = ZERO                                  EL344
unix  *        MOVE MPPROD-STATUS      TO  WS-ABEND-FILE-STATUS         EL344
unix  *        MOVE 'ERROR OCCURED CLOSE - MPPROD'                      EL344
unix  *                                TO  WS-ABEND-MESSAGE             EL344
unix  *        GO TO ABEND-PGM.                                         EL344
01414                                                                   EL344
01415      GOBACK.                                                      EL344
01416                                                                   EL344
01417  ABEND-PGM SECTION.         COPY ELCABEND.                        EL344
