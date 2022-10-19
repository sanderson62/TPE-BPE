00001  IDENTIFICATION DIVISION.                                         03/11/98
00002                                                                   ECS021
00003  PROGRAM-ID.                 ECS021.                                 LV011
00004 *              PROGRAM CONVERTED BY                               ECS021
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS021
00006 *              CONVERSION DATE 06/24/94 08:40:14.                 ECS021
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS021
00008 *                            VMOD=2.029.                          ECS021
00009                                                                   ECS021
00010 *AUTHOR.        LOGIC, INC.                                       ECS021
00011 *               DALLAS, TEXAS.                                    ECS021
00012                                                                   ECS021
00013 *DATE-COMPILED.                                                   ECS021
00014                                                                   ECS021
00015 *SECURITY.   *****************************************************ECS021
00016 *            *                                                   *ECS021
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC INC.      *ECS021
00018 *            *                                                   *ECS021
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS021
00020 *            *   OF LOGIC INC. IS EXPRESSLY PROHIBITED WITHOUT   *ECS021
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS021
00022 *            *                                                   *ECS021
00023 *            *****************************************************ECS021
00024                                                                   ECS021
00025 *REMARKS.                                                         ECS021
00026 *        THIS PROGRAM WILL READ THE EPEC FILE AND PRODUCE         ECS021
00027 *        A PROFIT ANALYSIS REPORT BY BENEFIT TYPE.                ECS021
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES FROM 300 TO 900
022804* 022804                   SMVA  HANDLE FILE STATUS 23 ON ELCNTL
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
101205* 101205    2005021800002  PEMA  ADD LITTLE GUYS TO ECS021
042407* 042407  IR2007042400001  AJRA  EXPAND PAGE DISPLAY TO 5 DIGITS
053107* 053107  IR2007050700001  PEMA  CORRECT ACCT NAME FOR DCC
061907* 061907  IR2007061400001  PEMA  FIX LR FIELD AND ACCT NAME
022808* 022808  CR2007083100002  PEMA  ADD 'F' ACCT STATUS
102908* 102908    2008073000002  AJRA  ADD RETRO EXTRACT
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
092602******************************************************************
00028                                                                   ECS021
00029  ENVIRONMENT DIVISION.                                            ECS021
00030  INPUT-OUTPUT SECTION.                                            ECS021
00031  FILE-CONTROL.                                                    ECS021
00032                                                                   ECS021
00033      SELECT  SORT-FILE       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS021
00034                                                                   ECS021
00035      SELECT  PRINTER         ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS021
00036                                                                   ECS021
00037      SELECT  EXCEP-PRINT     ASSIGN TO SYS009-UR-1403-S-SYS009.   ECS021
00038                                                                   ECS021
00039      SELECT  EPEC-FILE       ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS021
00040                                                                   ECS021
PEMUNI     SELECT  ACCT-MSTR       ASSIGN TO ERACCTT                    ECS021
00042                              ACCESS        SEQUENTIAL             ECS021
00043                              ORGANIZATION  INDEXED                ECS021
00044                              FILE STATUS   ERACCT-FILE-STATUS     ECS021
00045                              RECORD KEY    AM-CONTROL-PRIMARY.    ECS021
00046                                                                   ECS021
PEMUNI     SELECT  ERACCT          ASSIGN TO ERACCT                     ECS021
00042                              ACCESS        SEQUENTIAL             ECS021
00043                              ORGANIZATION  INDEXED                ECS021
00044                              FILE STATUS   ACCT-FILE-STATUS       ECS021
00045                              RECORD KEY    ERACCT-KEY.            ECS021
00046                                                                   ECS021
00047      SELECT  ELCNTL          ASSIGN TO ELCNTL                     ECS021
00048                              ACCESS       DYNAMIC                 ECS021
00049                              ORGANIZATION INDEXED                 ECS021
00050                              FILE STATUS  ELCNTL-FILE-STATUS      ECS021
00051                              RECORD KEY   CF-CONTROL-PRIMARY.     ECS021
00052                                                                   ECS021
00053      SELECT  COMP-MSTR       ASSIGN TO ERCOMP                     ECS021
00054                              ACCESS       DYNAMIC                 ECS021
00055                              ORGANIZATION INDEXED                 ECS021
00056                              FILE STATUS  ERCOMP-FILE-STATUS      ECS021
00057                              RECORD KEY   CO-CONTROL-PRIMARY.     ECS021
00058                                                                   ECS021
00059      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS021
00060                                                                   ECS021
00061      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS021
00062                                                                   ECS021
102908     SELECT  DATA-OUT        ASSIGN TO SYS022-UT-2400-S-SYS022
102908                             ORGANIZATION IS LINE SEQUENTIAL.
00058                                                                   ECS021
CIDMOD     SELECT  MIDWEST         ASSIGN TO SYS050-UR-1403-S-SYS050.      CL*23
CIDMOD                                                                  ECS021
CIDMOD     SELECT  DODDS           ASSIGN TO SYS051-UR-1403-S-SYS051.      CL*23
CIDMOD                                                                  ECS021
CIDMOD     SELECT  MWAUTO          ASSIGN TO SYS052-UR-1403-S-SYS052.      CL*23
CIDMOD                                                                  ECS021
00063  EJECT                                                            ECS021
00064  DATA DIVISION.                                                   ECS021
00065  FILE SECTION.                                                    ECS021
00066                                                                   ECS021
00067  SD  SORT-FILE.                                                   ECS021
00068                                                                   ECS021
00069  01  SW-RECORD.                                                   ECS021
00070      12  SW-REPORT-CONTROL-KEY.                                   ECS021
00071          16  SW-BREAK-FIELD-1             PIC X(10).              ECS021
00072          16  SW-BREAK-FIELD-2             PIC X(10).              ECS021
00073          16  SW-BREAK-FIELD-3             PIC X(10).              ECS021
00074          16  SW-BREAK-FIELD-4             PIC X(10).              ECS021
00075          16  SW-BREAK-FIELD-5             PIC X(10).              ECS021
00076          16  SW-BREAK-FIELD-6             PIC X(10).              ECS021
00077      12  SW-CONTROL-KEY-REDEF REDEFINES SW-REPORT-CONTROL-KEY     ECS021
00078              OCCURS 6 TIMES.                                      ECS021
00079          16  SW-BREAK-FIELD               PIC X(10).              ECS021
102908     12  SW-CARRIER                       PIC X.
00080      12  SW-ACCT-NAME                     PIC X(30).              ECS021
121707     12  SW-ACCT-STATUS                   PIC X.
00081      12  SW-GA-NAME                       PIC X(30).              ECS021
00082      12  SW-STATE-NAME                    PIC X(25).              ECS021
102908     12  SW-STATE-ABBREVIATION            PIC X(2).
00083      12  SW-PROD-DATE                     PIC X(6).               ECS021
00084      12  SW-RECORD-DATA            OCCURS 23.                     ECS021
00085          16  SW-BENEFIT-TYPE              PIC X.                  ECS021
00086          16  SW-BENEFIT-CODE              PIC 99.                 ECS021
00087          16  SW-BEN-TBL-POS               PIC S999 COMP.          ECS021
00088                                                                   ECS021
00089          16  SW-PERIOD    COMP-3   OCCURS 15.                     ECS021
00090              24  SW-ISS-CNT               PIC S9(7).              ECS021
00091              24  SW-CNC-CNT               PIC S9(7).              ECS021
00092              24  SW-ISS-PREM              PIC S9(11)V99.          ECS021
00093              24  SW-CNC-PREM              PIC S9(11)V99.          ECS021
00094              24  SW-NET-COMPEN            PIC S9(11)V99.          ECS021
00095              24  SW-CLM-CNT               PIC S9(7).              ECS021
00096              24  SW-CLM-AMT               PIC S9(11)V99.          ECS021
00097              24  SW-LOSS-RESV             PIC S9(11)V99.          ECS021
00098              24  SW-PRM-EARND             PIC S9(9)V99.           ECS021
00099              24  SW-PRM-INFRC             PIC S9(9)V99.           ECS021
00100              24  SW-INFRC-CNT             PIC S9(9).              ECS021
00101              24  SW-AVG-AGE               PIC S9(9).              ECS021
00102              24  SW-AVG-ORG-TRM           PIC S9(9).              ECS021
00103              24  SW-WGHT-AGE              PIC S9(9).              ECS021
00104              24  SW-WGHT-ORG-TRM          PIC S9(9).              ECS021
00105              24  SW-EXP-PCT               PIC S999V9(4).          ECS021
00106              24  SW-ADDED-TO-CNT          PIC S9(5).              ECS021
102908             24  SW-ACCT-COMM             PIC S9(11)V99.
102908             24  SW-OW-COMM               PIC S9(11)V99.
00107                                                                   ECS021
00108  EJECT                                                            ECS021
00109  FD  PRINTER                                                      ECS021
00110                              COPY ELCPRTFD.                       ECS021
00111                                                                   ECS021
00112  FD  EXCEP-PRINT                                                  ECS021
00113      BLOCK CONTAINS 0 RECORDS
00114      RECORDING MODE F.                                            ECS021
00115                                                                   ECS021
00116  01  PRT-EXCEP.                                                   ECS021
00117      12  PE-CTL              PIC X.                               ECS021
00118      12  PE-DATA             PIC X(132).                          ECS021
00119                                                                   ECS021
00120  FD  EPEC-FILE                                                    ECS021
00121                              COPY ECSEPCFD.                       ECS021
00122                                                                   ECS021
00123  01  EP-REC                  PIC X(325).                          ECS021
00124  EJECT                                                            ECS021
00125  FD  ACCT-MSTR.                                                   ECS021
00126                                                                   ECS021
00127                              COPY ERCACCT.                        ECS021
00128  EJECT                                                            ECS021
00125  FD  ERACCT.                                                      ECS021
00126                                                                   ECS021
00127  01  ERACCT-RECORD.
           05  FILLER                  PIC XX.
           05  ERACCT-KEY.
               10  ACCT-COMPANY-CD     PIC X.
               10  ACCT-CONTROL-A      PIC X(19).
               10  ACCT-CONTROL-B      PIC X(6).
           05  FILLER                  PIC X(88).
           05  ACCT-NAME               PIC X(30).
           05  FILLER                  PIC X(1854).
00128  EJECT                                                            ECS021
00129  FD  ELCNTL.                                                      ECS021
00130                              COPY ELCCNTL.                        ECS021
00131                                                                   ECS021
00132  EJECT                                                            ECS021
00133  FD  COMP-MSTR.                                                   ECS021
00134                              COPY ERCCOMP.                        ECS021
00135                                                                   ECS021
00136  EJECT                                                            ECS021
00137  FD  DISK-DATE                                                    ECS021
00138                              COPY ELCDTEFD.                       ECS021
00139                                                                   ECS021
00140  FD  FICH                    COPY ELCFCHFD.                       ECS021
00141                                                                   ECS021
102908 FD  DATA-OUT
102908     BLOCK CONTAINS 0 RECORDS.
102908 01  DATA-OUT-REC             PIC X(1100). 
102908
CIDMOD FD  MIDWEST                                                         CL*19
CIDMOD     BLOCK CONTAINS 0 RECORDS.
CIDMOD 01  TEST-MIDWEST.                                                   CL*19
CIDMOD     12  TEST-CC             PIC X.                                  CL*19
CIDMOD     12  FILLER              PIC X(04).                              CL*19
CIDMOD     12  TEST-FLD-1          PIC X(11).                              CL*19
CIDMOD     12  TEST-FLD-2          PIC X(07).                              CL*19
CIDMOD     12  TEST-REC            PIC X(110).                             CL*19
CIDMOD                                                                     CL*19
CIDMOD                                                                     CL*11
CIDMOD FD  DODDS                                                           CL*19
CIDMOD     BLOCK CONTAINS 0 RECORDS.
CIDMOD 01  TEST-DODDS.                                                     CL*19
CIDMOD     12  DODDS-CC            PIC X.                                  CL*19
CIDMOD     12  FILLER              PIC X(04).                              CL*19
CIDMOD     12  DODDS-FLD-1         PIC X(11).                              CL*19
CIDMOD     12  DODDS-FLD-2         PIC X(07).                              CL*19
CIDMOD     12  DODDS-REC           PIC X(110).                             CL*19
CIDMOD                                                                     CL*19
CIDMOD FD  MWAUTO                                                          CL*19
CIDMOD     BLOCK CONTAINS 0 RECORDS.
CIDMOD 01  TEST-MWAUTO.                                                    CL*19
CIDMOD     12  MWAUTO-CC           PIC X.                                  CL*19
CIDMOD     12  FILLER              PIC X(04).                              CL*19
CIDMOD     12  MWAUTO-FLD-1        PIC X(11).                              CL*19
CIDMOD     12  MWAUTO-FLD-2        PIC X(07).                              CL*19
CIDMOD     12  MWAUTO-REC          PIC X(110).                             CL*19
CIDMOD                                                                     CL*19
00107                                                                      CL*11
00142  EJECT                                                            ECS021
00143  WORKING-STORAGE SECTION.                                         ECS021
00144 *01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS021
00145                                                                   ECS021
00146  77  FILLER  PIC X(32) VALUE '********************************'.  ECS021
00147  77  FILLER  PIC X(32) VALUE '     ECS021 WORKING STORAGE     '.  ECS021
00148  77  FILLER  PIC X(32) VALUE '******** VMOD=2.029*************'.  ECS021
00149                                                                   ECS021
CIDMOD 77  PRINT-MIDWEST-SW        PIC XXX              VALUE 'YES'.    ECS021
CIDMOD 77  PRINT-DODDS-SW          PIC XXX              VALUE 'YES'.    ECS021
CIDMOD 77  PRINT-MWAUTO-SW         PIC XXX              VALUE 'YES'.    ECS021
CIDMOD 77  SKIP-PRINT-SW           PIC XXX              VALUE 'YES'.    ECS021
00150  77  PGM-SUB                 PIC S999      COMP-3 VALUE +21.      ECS021
00151  77  SP-ISS-CNT              PIC S9(7)     COMP-3 VALUE +0.       ECS021
00152  77  SP-CNC-CNT              PIC S9(7)     COMP-3 VALUE +0.       ECS021
00153  77  EPECS-READ-COUNT        PIC S9(7)     COMP-3 VALUE +0.       ECS021
00154  77  EPECS-SELECTED          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00155  77  EPECS-SORTED            PIC S9(7)     COMP-3 VALUE +0.       ECS021
00156  77  EPECS-RETURNED          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00157  77  GA-LOOP-COUNT           PIC S9(7)     COMP-3 VALUE +0.       ECS021
00158  77  NON-EP-EC-DROP-CNT      PIC S9(7)     COMP-3 VALUE +0.       ECS021
00159  77  ACCT-STATUS-DROP        PIC S9(7)     COMP-3 VALUE +0.       ECS021
00160  77  ACCT-TERM-DROP          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00161  77  BAD-ACCT-STATUS         PIC S9(7)     COMP-3 VALUE +0.       ECS021
00162  77  REIN-EPEC-DROP-CNT      PIC S9(7)     COMP-3 VALUE +0.       ECS021
00163  77  EPEC-DATE-DROP-CNT1     PIC S9(7)     COMP-3 VALUE +0.       ECS021
00164  77  EPEC-DATE-DROP-CNT2     PIC S9(7)     COMP-3 VALUE +0.       ECS021
00165  77  BAD-DATE-LOGIC-CNT      PIC S9(7)     COMP-3 VALUE +0.       ECS021
00166  77  CARRIER-DROP-CNT        PIC S9(7)     COMP-3 VALUE +0.       ECS021
00167  77  GROUP-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00168  77  STATE-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00169  77  ACCOUNT-DROP-CNT        PIC S9(7)     COMP-3 VALUE +0.       ECS021
00170  77  AGENT-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00171  77  BUS-TYP-DROP-CNT        PIC S9(7)     COMP-3 VALUE +0.       ECS021
00172  77  LIFE-BEN-DROP-CNT       PIC S9(7)     COMP-3 VALUE +0.       ECS021
00173  77  AH-BEN-DROP-CNT         PIC S9(7)     COMP-3 VALUE +0.       ECS021
00174  77  RPT-CODE1-DROP-CNT      PIC S9(7)     COMP-3 VALUE +0.       ECS021
00175  77  RPT-CODE2-DROP-CNT      PIC S9(7)     COMP-3 VALUE +0.       ECS021
00176  77  USER1-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00177  77  USER2-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00178  77  USER3-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00179  77  USER4-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00180  77  USER5-DROP-CNT          PIC S9(7)     COMP-3 VALUE +0.       ECS021
00181  77  LINE-CNT                PIC S999      COMP-3 VALUE +99.      ECS021
00182  77  PAGE-CNT                PIC S9(5)     COMP-3 VALUE +0.       ECS021
00183  77  LINE-E-MAX              PIC S999      COMP-3 VALUE +57.      ECS021
00184  77  LINE-E-CNT              PIC S999      COMP-3 VALUE +99.      ECS021
00185  77  PAGE-E-CNT              PIC S9(5)     COMP-3 VALUE +0.       ECS021
00186  77  ST-IDX                  PIC S999      COMP   VALUE +0.       ECS021
00187  77  AGT-IDX                 PIC S999      COMP   VALUE +0.       ECS021
00188  77  GNRL-IDX                PIC S999      COMP   VALUE +0.       ECS021
00189  77  DTE-IDX                 PIC S999      COMP   VALUE +0.       ECS021
00190  77  TOT-IDX                 PIC S999      COMP   VALUE +0.       ECS021
00191  77  BEN-IDX                 PIC S999      COMP   VALUE +0.       ECS021
00192  77  BRK-IDX                 PIC S999      COMP   VALUE +0.       ECS021
00193  77  PREV-IDX                PIC S999      COMP   VALUE +0.       ECS021
00194  77  DTE-NDX                 PIC S999      COMP   VALUE +0.       ECS021
00195  77  TOT-NDX                 PIC S999      COMP   VALUE +0.       ECS021
00196  77  BEN-NDX                 PIC S999      COMP   VALUE +0.       ECS021
00197  77  BRK-NDX                 PIC S999      COMP   VALUE +0.       ECS021
00198  77  PREV-NDX                PIC S999      COMP   VALUE +0.       ECS021
00199  77  SAVE-BEN-INDEX          PIC S999      COMP   VALUE +0.       ECS021
00200  77  ABEND-COUNTER           PIC S999      COMP-3.                ECS021
00201  77  X                       PIC  X               VALUE SPACE.    ECS021
00202  77  XE                      PIC  X               VALUE SPACE.    ECS021
00203  77  WS-EP-CODE              PIC  X               VALUE SPACE.    ECS021
00204  77  WS-PRM-EARND            PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00205  77  WS-COMM-EARND           PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00206  77  WS-COMM-NWN             PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00207  77  WS-COMPEN-NWN           PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00208  77  WS-NET-PCT              PIC S9V9(14)         VALUE +0.       ECS021
00209  77  SAVE-ISS-PRM            PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00210  77  SAVE-CNC-PRM            PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00211  77  SAVE-COMM-EARND         PIC S9(9)V99  COMP-3 VALUE +0.       ECS021
00212  77  W-DISPLAY-TBL-POS       PIC 9999             VALUE ZEROS.    ECS021
00213  77  HOLD-EXCEPTION-PRT      PIC X(133)           VALUE SPACES.   ECS021
00214                                                                   ECS021
00215  01  WS-SUB-PROGRAM-ACTIONS.                                      ECS021
00216      12  W-BUILD-ZERO-TABLE      PIC X(02) VALUE 'BZ'.            ECS021
00217      12  W-ZERO-TABLE            PIC X(02) VALUE 'ZT'.            ECS021
00218      12  W-APPLY-SORT-RCD        PIC X(02) VALUE 'AS'.            ECS021
00219      12  W-ADD-LINK-TABLES       PIC X(02) VALUE 'AT'.            ECS021
00220      12  W-MOVE-TO-LINK          PIC X(02) VALUE 'MT'.            ECS021
00221                                                                   ECS021
00222  01  WS-EXCEPTION-DATE.                                           ECS021
00223      12  WS-EXCEP-CCYY           PIC 9(04).                       ECS021
00224      12  WS-EXCEP-CCYR  REDEFINES  WS-EXCEP-CCYY.                 ECS021
00225          16  WS-EXCEP-CC         PIC 99.                          ECS021
00226          16  WS-EXCEP-YR         PIC 99.                          ECS021
00227      12  WS-EXCEP-MO             PIC 99.                          ECS021
00228      12  WS-EXCEP-DA             PIC 99.                          ECS021
00229                                                                   ECS021
00230  01  REQUEST-TABLE.                                               ECS021
00231      12  PROCESSING-REQUEST  OCCURS 10  PIC X(2).                 ECS021
00232      12  NUMBER-OF-REQUESTS             PIC S9(04) COMP.          ECS021
00233                                                                   ECS021
00234  EJECT                                                            ECS021
00235                              COPY ECSEPC01.                       ECS021
00236  EJECT                                                            ECS021
00237                                                                   ECS021
00238  01  WS-ABEND-STORAGE.                                            ECS021
00239      12  WS-RETURN-CODE          PIC S9(4)  VALUE +0 COMP.        ECS021
00240      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         ECS021
00241      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          ECS021
00242      12  WS-ZERO                 PIC S9     VALUE +0 COMP-3.      ECS021
00243      12  WS-ABEND-CODE           PIC S9(4).                       ECS021
00244      12  WORK-ABEND-CODE  REDEFINES  WS-ABEND-CODE.               ECS021
00245          16  WAC-1               PIC X.                           ECS021
00246          16  WAC-2               PIC X.                           ECS021
00247          16  WAC-3-4             PIC 99.                          ECS021
00248                                                                   ECS021
00249  01  WS-FILE-STATUS.                                              ECS021
00250      12  ELCNTL-FILE-STATUS      PIC XX.                          ECS021
00251      12  ERACCT-FILE-STATUS      PIC XX.                          ECS021
00251      12  ACCT-FILE-STATUS        PIC XX.                          ECS021
00252      12  ERCOMP-FILE-STATUS      PIC XX.                          ECS021
00253                                                                   ECS021
00254  01  WS-SWITCHES.                                                 ECS021
PEMMOD     12  WS-AM-NAME              PIC X(30) VALUE SPACES.
PEMMOD     12  WS-HOLD-CNTL            PIC X(19) VALUE SPACES.
PEMTST     12  WS-REPORT-NO            PIC XXX  VALUE SPACES.
PEMTST     12  WS-REPORT-SEQ REDEFINES WS-REPORT-NO
PEMTST                                 PIC 999.
00255      12  WS-ACCT-BREAK-POSITION  PIC 99   VALUE ZERO.             ECS021
00256      12  WS-GA-BREAK-POSITION    PIC 99   VALUE ZEROS.            ECS021
00257      12  WS-STATE-BREAK-POSITION PIC 99   VALUE ZEROS.            ECS021
00258      12  WS-SAVE-GA-NAME         PIC X(30)  VALUE SPACES.         ECS021
00259      12  WS-SAVE-STATE-NAME      PIC X(25)  VALUE SPACES.         ECS021
102908     12  WS-SAVE-STATE-ABBREVIATION PIC X(2).
00260      12  WS-SAVE-GA-CARR         PIC X(01).                       ECS021
00261      12  WS-SAVE-GA-GROUP        PIC X(06).                       ECS021
102908     12  WS-SAVE-CARRIER         PIC X(01).
00262      12  WS-SAVE-ACCT-NAME       PIC X(30).                       ECS021
121707     12  WS-SAVE-ACCT-STATUS     PIC X.
00263      12  WS-SAVE-PROD-DATE.                                       ECS021
00264          16  WS-SAVE-PROD-YR     PIC XX.                          ECS021
00265          16  WS-SAVE-PROD-MO     PIC XX.                          ECS021
00266          16  WS-SAVE-PROD-DA     PIC XX.                          ECS021
00267      12  WS-SAVE-COMPEN-STRUCTURE.                                ECS021
00268          16  WS-GA-COUNT         PIC S9(4)  VALUE +0 COMP.        ECS021
00269          16  WS-1ST-AGENT-NUMBER PIC X(10)  VALUE SPACES.         ECS021
00270          16  WS-AGENT-OCCURS  OCCURS  10 TIMES.                   ECS021
00271              20  WS-AGENT-NUMBER PIC X(10).                       ECS021
00272                                                                   ECS021
00273      12  WS-ACCT-BREAK-SW        PIC X(01)  VALUE 'N'.            ECS021
00274      12  WS-PREV-EP-ACCOUNT      PIC X(10)  VALUE SPACES.         ECS021
00275      12  WS-PREV-EP-EXP-DTE      PIC 9(11)  VALUE 0.                 CL**2
00276                                                                   ECS021
00277      12  CURRENT-ACTIVITY-SW     PIC X.                           ECS021
00278          88  CURRENT-ACTIVITY-FOUND         VALUE 'Y'.            ECS021
00279          88  NO-CURRENT-ACTIVITY            VALUE 'N'.            ECS021
00280      12  LIFE-BENEFIT-COUNT      PIC S999 VALUE +0 COMP-3.        ECS021
00281          88  NO-LIFE-BENEFITS               VALUE +0.             ECS021
00282      12  AH-BENEFIT-COUNT        PIC S999 VALUE +0 COMP-3.        ECS021
00283          88  NO-AH-BENEFITS                 VALUE +0.             ECS021
00284      12  SORT-BENEFITS-EXCEEDED  PIC X      VALUE 'N'.            ECS021
00285          88  SORT-RECORD-FULL               VALUE 'Y'.            ECS021
00286      12  SORT-PRIME-SWITCH       PIC X      VALUE 'Y'.            ECS021
00287          88  SORT-KEY-NEEDS-PRIMING         VALUE 'Y'.            ECS021
00288      12  BAD-DATE-SWITCH         PIC X      VALUE 'N'.            ECS021
00289          88  BAD-EPEC-DATE                  VALUE 'Y'.            ECS021
00290      12  TOTAL-TYPE-SWITCH       PIC XX     VALUE SPACES.         ECS021
00291          88  LIFE-INDIVIDUAL                VALUE 'LI'.           ECS021
00292          88  LIFE-TOTAL                     VALUE 'LT'.           ECS021
00293          88  AH-INDIVIDUAL                  VALUE 'AI'.           ECS021
00294          88  AH-TOTAL                       VALUE 'AT'.           ECS021
00295          88  BOTH-TOTAL                     VALUE 'BT'.           ECS021
00296      12  BREAK-SWITCH            PIC X      VALUE SPACES.         ECS021
00297          88  ROLLING-BREAK                  VALUE ' '.            ECS021
00298          88  LAST-12-BREAK                  VALUE '1'.            ECS021
00299          88  PREV-12-BREAK                  VALUE '2'.            ECS021
00300          88  YTD-BREAK                      VALUE '3'.            ECS021
00301          88  PREV-YTD-BREAK                 VALUE '4'.            ECS021
00302      12  HEADING-SWITCH          PIC 99     VALUE ZEROS.          ECS021
00303      12  WS-1ST-EXCEPTION-PRT-SW     PIC X      VALUE 'Y'.        ECS021
00304          88  FIRST-EXCEPTION-PRINT          VALUE 'Y'.            ECS021
00305      12  CHECK-AGT-TYPE          PIC X      VALUE SPACES.         ECS021
00306          88  PROPER-AGT-TYPE     VALUE 'C' 'D' 'O' 'P'.           ECS021
00307      12  WS-NUMBER-OF-BREAKS     PIC 9      VALUE ZEROS.          ECS021
00308          88  REPORT-HAS-SIX-BREAKS          VALUE 6.              ECS021
00309          88  REPORT-HAS-FIVE-BREAKS         VALUE 5.              ECS021
00310          88  REPORT-HAS-FOUR-BREAKS         VALUE 4.              ECS021
00311          88  REPORT-HAS-THREE-BREAKS        VALUE 3.              ECS021
00312          88  REPORT-HAS-TWO-BREAKS          VALUE 2.              ECS021
00313          88  REPORT-HAS-ONE-BREAK           VALUE 1.              ECS021
00314      12  WS-SPECIAL-PERIOD-DESCRIPTIONS.                          ECS021
00315          16  LAST-12-DESC            PIC X(4)   VALUE 'L12'.      ECS021
00316          16  PREV-12-DESC            PIC X(4)   VALUE 'P12'.      ECS021
00317          16  YTD-DESC                PIC X(4)   VALUE 'YTD'.      ECS021
00318          16  PREV-YTD-DESC           PIC X(4)   VALUE 'PYTD'.     ECS021
00319          16  ITD-DESC                PIC X(4)   VALUE 'ITD'.      ECS021
00320          16  AVERAGE-DESC            PIC X(6)   VALUE 'MO AVG'.   ECS021
00321                                                                   ECS021
00322      EJECT                                                        ECS021
00323  01  WS-EXC-COMPUTED-NUMBERS COMP-3.                              ECS021
00324      12  EXC-ISS-CNT             PIC S9(7)       VALUE +0.        ECS021
00325      12  EXC-CNC-CNT             PIC S9(7)       VALUE +0.        ECS021
00326      12  EXC-ISS-PREM            PIC S9(11)V99   VALUE +0.        ECS021
00327      12  EXC-CNC-PREM            PIC S9(11)V99   VALUE +0.        ECS021
00328      12  EXC-CVRGS               PIC S9(7)       VALUE +0.        ECS021
00329      12  EXC-CVRGS-DIFF          PIC S9(5)       VALUE +0.        ECS021
00330      12  EXC-WRITTEN-PREM-PCT    PIC S999        VALUE +0.        ECS021
00331      12  EXC-EARND-PREM          PIC S9(11)V99   VALUE +0.        ECS021
00332      12  EXC-CLAIM-AMT           PIC S9(11)V99   VALUE +0.        ECS021
00333      12  EXC-PMO-CVRGS           PIC S9(7)       VALUE +0.        ECS021
00334      12  EXC-PMO-ISS-CNT         PIC S9(7)       VALUE +0.        ECS021
00335      12  EXC-PMO-EARND-PREM      PIC S9(11)V99   VALUE +0.        ECS021
00336      12  EXC-PMO-WRITTEN-PREM    PIC S9(11)V99   VALUE +0.        ECS021
00337      12  EXC-CMO-CVRGS           PIC S9(7)       VALUE +0.        ECS021
00338      12  EXC-CMO-ISS-CNT         PIC S9(7)       VALUE +0.        ECS021
00339      12  EXC-CMO-EARND-PREM      PIC S9(11)V99   VALUE +0.        ECS021
00340      12  EXC-CMO-WRITTEN-PREM    PIC S9(11)V99   VALUE +0.        ECS021
00341      12  EXC-WRITTEN-PREM        PIC S9(11)V99   VALUE +0.        ECS021
00342      12  EXC-CANCEL-RATIO        PIC S999V9      VALUE +0.        ECS021
00343      12  EXC-L12-EARND-PREM      PIC S9(11)V99   VALUE +0.        ECS021
00344      12  EXC-P12-EARND-PREM      PIC S9(11)V99   VALUE +0.        ECS021
00345      12  EXC-EARND-PREM-DECR-PCT PIC S99V999     VALUE +0.        ECS021
00346      12  EXC-L12-LOSS-RATIO      PIC S99V999     VALUE +0.        ECS021
00347      12  EXC-P12-LOSS-RATIO      PIC S99V999     VALUE +0.        ECS021
00348      12  EXC-LOSS-RATIO-DIFF     PIC S99V999     VALUE +0.        ECS021
00349      12  EXC-L12-PROFIT-PERCENT  PIC S99V999     VALUE +0.        ECS021
00350      12  EXC-P12-PROFIT-PERCENT  PIC S99V999     VALUE +0.        ECS021
00351      12  EXC-PROFIT-PCT-DIFF     PIC S99V999     VALUE +0.        ECS021
00352      12  EXC-ACC-AVG-INFRC       PIC S9(9)       VALUE +0.        ECS021
00353      12  EXC-ACC-AVG-INFRC-CNT   PIC S9(9)       VALUE +0.        ECS021
00354      12  EXC-ACC-AVG-ORG-TRM     PIC S9(7)V9     VALUE +0.        ECS021
00355      12  EXC-ACC-AVG-ORG-TRM-CNT PIC S999        VALUE +0.        ECS021
00356      12  EXC-ACC-AVG-WGHT-TRM    PIC S9(7)V9     VALUE +0.        ECS021
00357      12  EXC-ACC-AVG-WGHT-TRM-CNT PIC S999       VALUE +0.        ECS021
00358      12  EXC-ACC-AVG-ISS-AGE     PIC S9(7)V9     VALUE +0.        ECS021
00359      12  EXC-ACC-AVG-ISS-CNT     PIC S999        VALUE +0.        ECS021
00360      12  EXC-ACC-AVG-WGHT-AGE    PIC S9(7)V9     VALUE +0.        ECS021
00361      12  EXC-ACC-AVG-WGHT-CNT    PIC S999        VALUE +0.        ECS021
00362      12  EXC-INFRC-CVRG          PIC S9(9)       VALUE +0.        ECS021
00363      12  EXC-CMO-INFRC-CVRG      PIC S9(9)       VALUE +0.        ECS021
00364      12  EXC-AVRG-ORIG-TERM      PIC S9(3)V9     VALUE +0.        ECS021
00365      12  EXC-WGHT-ORIG-TERM      PIC S9(3)V9     VALUE +0.        ECS021
00366      12  EXC-AVRG-ISS-AGE        PIC S9(2)V9     VALUE +0.        ECS021
00367      12  EXC-WGHT-ISS-AGE        PIC S9(2)V9     VALUE +0.        ECS021
00368      12  EXC-INFRC-PCT           PIC S9(3)V9     VALUE +0.        ECS021
00369      12  EXC-ORIG-TERM-PCT       PIC S9(3)V9     VALUE +0.        ECS021
00370      12  EXC-CMO-ORG-TRM         PIC S9(3)V9     VALUE +0.        ECS021
00371      12  EXC-ISS-AGE-PCT         PIC S9(3)V9     VALUE +0.        ECS021
00372      12  EXC-CMO-AVG-AGE         PIC S9(2)V9     VALUE +0.        ECS021
00373      12  EXC-WGHT-TRM-PCT        PIC S9(3)V9     VALUE +0.        ECS021
00374      12  EXC-CMO-WGHT-ORG-TRM    PIC S9(3)V9     VALUE +0.        ECS021
00375      12  EXC-WGHT-AGE-PCT        PIC S9(3)V9     VALUE +0.        ECS021
00376      12  EXC-CMO-WGHT-AGE        PIC S9(2)V9     VALUE +0.        ECS021
00377      12  EXC-L12-CLAIM-AMT       PIC S9(11)V99   VALUE +0.        ECS021
00378      12  EXC-L12-NET-COMPEN      PIC S9(11)V99   VALUE +0.        ECS021
00379      12  EXC-RETENTION-CALC      PIC S9(11)V99   VALUE +0.        ECS021
00380                                                                   ECS021
00381  01  WS-COMPUTED-NUMBERS COMP-3.                                  ECS021
00382      12  LIFE-ISS-CNT            PIC S9(7)       VALUE +0.        ECS021
00383      12  ISS-CNT                 PIC S9(7)       VALUE +0.        ECS021
00384      12  CNC-CNT                 PIC S9(7)       VALUE +0.        ECS021
00385      12  CLM-CNT                 PIC S9(7)       VALUE +0.        ECS021
00386      12  LF-CLM-CNT              PIC S9(7)       VALUE +0.        ECS021
00387      12  AH-CLM-CNT              PIC S9(7)       VALUE +0.        ECS021
00388      12  ISS-PREM                PIC S9(11)V99   VALUE +0.        ECS021
00389      12  CNC-PREM                PIC S9(11)V99   VALUE +0.        ECS021
00390      12  NET-EARND-PREM          PIC S9(9)V99    VALUE +0.        ECS021
00391      12  NET-CLAIM-AMT           PIC S9(11)V99   VALUE +0.        ECS021
00392      12  NET-COMPEN              PIC S9(11)V99   VALUE +0.        ECS021
00393      12  NET-CVRGS               PIC S9(7)       VALUE +0.        ECS021
00394      12  NET-WRITTEN-PREM        PIC S9(11)V99   VALUE +0.        ECS021
00395      12  TOT-ISS-CNT             PIC S9(7)       VALUE +0.        ECS021
00396      12  TOT-CNC-CNT             PIC S9(7)       VALUE +0.        ECS021
00397      12  TOT-SINGLE-ELEM         PIC S9(7)       VALUE +0.        ECS021
00398      12  TOT-JOINT-RETRO         PIC S9(7)       VALUE +0.        ECS021
00399      12  TOT-LIFE-LEVEL          PIC S9(7)       VALUE +0.        ECS021
00400      12  TOT-LIFE-DECR           PIC S9(7)       VALUE +0.        ECS021
00401      12  TOT-ISS-PREM            PIC S9(11)V99   VALUE +0.        ECS021
00402      12  TOT-CNC-PREM            PIC S9(11)V99   VALUE +0.        ECS021
00403      12  TOT-EARND-PREM          PIC S9(9)V99    VALUE +0.        ECS021
00404      12  TOT-CLAIM-AMT           PIC S9(11)V99   VALUE +0.        ECS021
00405      12  TOT-LF-CLAIM-AMT        PIC S9(11)V99   VALUE +0.        ECS021
00406      12  TOT-AH-CLAIM-AMT        PIC S9(11)V99   VALUE +0.        ECS021
00407      12  TOT-COMPEN              PIC S9(11)V99   VALUE +0.        ECS021
00408      12  TOT-CVRGS               PIC S9(7)       VALUE +0.        ECS021
00409      12  TOT-WRITTEN-PREM        PIC S9(11)V99   VALUE +0.        ECS021
00410      12  CANCEL-RATIO            PIC S99V999     VALUE +0.        ECS021
00411      12  LOSS-RESERVE            PIC S9(11)V99   VALUE +0.        ECS021
061907     12  LOSS-RATIO              PIC S9(5)V999   VALUE +0.
00413      12  CLAIMS-PAID             PIC S9(11)V99   VALUE +0.        ECS021
00414      12  LF-CLAIMS-PAID          PIC S9(11)V99   VALUE +0.        ECS021
00415      12  AH-CLAIMS-PAID          PIC S9(11)V99   VALUE +0.        ECS021
00416      12  COMPEN-PERCENT          PIC S99V999     VALUE +0.        ECS021
00417      12  SV-COMP-PCT             PIC S99V9(7)    VALUE +0.        ECS021
00418      12  EXPENSE-AMT             PIC S9(9)V99    VALUE +0.        ECS021
00419      12  EXPENSE-PERCENT         PIC S999V9(4)   VALUE +0.        ECS021
00420      12  EXP-EARN-RATIO          PIC S99V999     VALUE +0.        ECS021
00421      12  PERIOD-PROFIT           PIC S9(9)V99    VALUE +0.        ECS021
00422      12  PROFIT-PERCENT          PIC S99V9(5)    VALUE +0.        ECS021
00423      12  PENETRATION-PERCENT     PIC S99V999     VALUE +0.        ECS021
00424      12  AVG-CANCEL-PREMIUM      PIC S9(11)V99   VALUE +0.        ECS021
00425      12  AVG-PREMIUM             PIC S9(11)V99   VALUE +0.        ECS021
00426      12  AVG-CLAIM               PIC S9(11)V99   VALUE +0.        ECS021
00427      12  LF-AVG-CLAIM            PIC S9(11)V99   VALUE +0.        ECS021
00428      12  AH-AVG-CLAIM            PIC S9(11)V99   VALUE +0.        ECS021
00429      12  AVG-ORG-TRM             PIC S9(9)V9     VALUE +0.        ECS021
00430      12  AVG-AGE                 PIC S9(9)V9     VALUE +0.        ECS021
00431      12  WGHT-AGE                PIC S9(9)V9     VALUE +0.        ECS021
00432      12  WGHT-ORG-TRM            PIC S9(9)V9     VALUE +0.        ECS021
102908     12  WRK-AMOUNT              PIC S9(11)V99   VALUE +0.
00433                                                                   ECS021
00434      EJECT                                                        ECS021
00435  01  ACCUM-AVG-CNTRS     COMP-3.                                  ECS021
00436      12  ACCUM-AVG-INFRC         PIC S9(9)       VALUE +0.        ECS021
00437      12  ACCUM-AVG-INFRC-CNT     PIC S999        VALUE +0.        ECS021
00438      12  ACCUM-AVG-PREM          PIC S9(7)V99    VALUE +0.        ECS021
00439      12  ACCUM-AVG-PREM-CNT      PIC S999        VALUE +0.        ECS021
00440      12  ACCUM-AVG-ORG-TRM       PIC S9(9)V9     VALUE +0.        ECS021
00441      12  ACCUM-AVG-ORG-TRM-CNT   PIC S999        VALUE +0.        ECS021
00442      12  ACCUM-AVG-WGHT-TRM      PIC S9(9)V9     VALUE +0.        ECS021
00443      12  ACCUM-AVG-WGHT-TRM-CNT  PIC S999        VALUE +0.        ECS021
00444      12  ACCUM-AVG-ISS-AGE       PIC S9(9)V9     VALUE +0.        ECS021
00445      12  ACCUM-AVG-ISS-CNT       PIC S999        VALUE +0.        ECS021
00446      12  ACCUM-AVG-WGHT-AGE      PIC S9(9)V9     VALUE +0.        ECS021
00447      12  ACCUM-AVG-WGHT-CNT      PIC S999        VALUE +0.        ECS021
00448      12  ACCUM-AVG-LIFE-CLAIM    PIC S9(7)V99    VALUE +0.        ECS021
00449      12  ACCUM-AVG-LIFE-CNT      PIC S999        VALUE +0.        ECS021
00450      12  ACCUM-AVG-AH-CLAIM      PIC S9(7)V99    VALUE +0.        ECS021
00451      12  ACCUM-AVG-AH-CNT        PIC S999        VALUE +0.        ECS021
00452      12  ACCUM-AVG-REFUND        PIC S9(7)V99    VALUE +0.        ECS021
00453      12  ACCUM-AVG-REFUND-CNT    PIC S999        VALUE +0.        ECS021
00454                                                                   ECS021
00455  01  ZERO-ACCUM-AVG-CNTRS     COMP-3.                             ECS021
00456      12  FILLER                  PIC S9(9)       VALUE +0.        ECS021
00457      12  FILLER                  PIC S999        VALUE +0.        ECS021
00458      12  FILLER                  PIC S9(7)V99    VALUE +0.        ECS021
00459      12  FILLER                  PIC S999        VALUE +0.        ECS021
00460      12  FILLER                  PIC S9(9)V9     VALUE +0.        ECS021
00461      12  FILLER                  PIC S999        VALUE +0.        ECS021
00462      12  FILLER                  PIC S9(9)V9     VALUE +0.        ECS021
00463      12  FILLER                  PIC S999        VALUE +0.        ECS021
00464      12  FILLER                  PIC S9(9)V9     VALUE +0.        ECS021
00465      12  FILLER                  PIC S999        VALUE +0.        ECS021
00466      12  FILLER                  PIC S9(9)V9     VALUE +0.        ECS021
00467      12  FILLER                  PIC S999        VALUE +0.        ECS021
00468      12  FILLER                  PIC S9(7)V99    VALUE +0.        ECS021
00469      12  FILLER                  PIC S999        VALUE +0.        ECS021
00470      12  FILLER                  PIC S9(7)V99    VALUE +0.        ECS021
00471      12  FILLER                  PIC S999        VALUE +0.        ECS021
00472      12  FILLER                  PIC S9(7)V99    VALUE +0.        ECS021
00473      12  FILLER                  PIC S999        VALUE +0.        ECS021
00474                                                                   ECS021
00475      EJECT                                                        ECS021
00476  01  HD-1ST-PROD-DATE.                                            ECS021
00477      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00478      12  FILLER          PIC X(17)  VALUE ' 1ST PROD DATE - '.    ECS021
00479      12  HD-PROD-DATE.                                            ECS021
00480          16  HD-PROD-MO    PIC XX.                                ECS021
00481          16  FILLER        PIC X      VALUE '/'.                  ECS021
00482          16  HD-PROD-DA    PIC XX.                                ECS021
00483          16  FILLER        PIC X      VALUE '/'.                  ECS021
00484          16  HD-PROD-YR    PIC XX.                                ECS021
           12  FILLER          PIC X(39)  VALUE SPACES.
           12  FILLER          PIC X(10)  VALUE ' STATUS : '.
           12  HD-ACCT-STATUS  PIC X(10)  VALUE SPACES.
00485 *    12  FILLER          PIC X(59)  VALUE SPACES.                 ECS021
00486      12  FILLER          PIC X(17)  VALUE '  ACCOUNT NAME - '.    ECS021
00487      12  HD-ACCT-NAME    PIC X(30)  VALUE SPACES.                 ECS021
00488                                                                   ECS021
PEMTST 01  WS-VARIABLE-HEADERS.
PEMTST     12  VAR-HEAD-1.
PEMTST         16  FILLER      PIC X(15)  VALUE SPACES.
PEMTST         16  FILLER      PIC X(46)  VALUE
PEMTST         'PROFIT ANALYSIS, REPORT CODE 1, STATE, ACCOUNT'.
PEMTST         16  FILLER      PIC X(35)  VALUE SPACES.
PEMTST     12  VAR-HEAD-2.
PEMTST         16  FILLER      PIC X(07)  VALUE SPACES.
PEMTST         16  FILLER      PIC X(32)  VALUE
PEMTST         'PROFIT ANALYSIS, REPORT CODE 1, '.
PEMTST         16  FILLER      PIC X(29)  VALUE
PEMTST         'REPORT CODE 2, STATE, ACCOUNT'.
PEMTST         16  FILLER      PIC X(27)  VALUE SPACES.
PEMTST     12  VAR-HEAD-3.
PEMTST         16  FILLER      PIC X(27)  VALUE SPACES.
PEMTST         16  FILLER      PIC X(22)  VALUE
PEMTST         'PROFIT ANALYSIS, STATE'.
PEMTST         16  FILLER      PIC X(47)  VALUE SPACES.
PEMTST     12  VAR-HEAD-4.
PEMTST         16  FILLER      PIC X(15)  VALUE SPACES.
PEMTST         16  FILLER      PIC X(46)  VALUE
PEMTST         'PROFIT ANALYSIS, REPORT CODE 2, STATE, ACCOUNT'.
PEMTST         16  FILLER      PIC X(35)  VALUE SPACES.
PEMTST     12  VAR-HEAD-5.
PEMTST         16  FILLER      PIC X(30)  VALUE SPACES.
PEMTST         16  FILLER      PIC X(16)  VALUE
PEMTST         'PROFIT ANALYSIS '.
PEMTST         16  FILLER      PIC X(50)  VALUE SPACES.
00489  01  GA-HEADER.                                                   ECS021
00490      12  FILLER          PIC X(85)  VALUE SPACES.                 ECS021
00491      12  FILLER          PIC X(17)  VALUE '  GEN AGT NAME - '.    ECS021
00492      12  GA-NAME         PIC X(30)  VALUE SPACES.                 ECS021
00493                                                                   ECS021
00494  01  STATE-HEADER.                                                ECS021
00495      12  FILLER          PIC X(17)  VALUE '    STATE NAME - '.    ECS021
00496      12  ST-NAME         PIC X(25)  VALUE SPACES.                 ECS021
00497                                                                   ECS021
00498  01  HEADER-ONE.                                                  ECS021
00499      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00500      12  BRK-1-DESC      PIC X(15)  VALUE 'LEVEL 1 CNTL :'.       ECS021
00501      12  BRK-1-CNTL      PIC X(10)  VALUE SPACES.                 ECS021
PEMTST     12  HDR1-DESC       PIC X(96)  VALUE SPACES.
00502 *    12  FILLER          PIC X(29)  VALUE SPACES.                 ECS021
PEMTST*    12  FILLER          PIC X(15)  VALUE 'PROFIT ANALYSIS'.      ECS021
PEMTST*    12  HDR1-DESC       PIC X(51)  VALUE ' '.                    ECS021
PEMTST     12  FILLER          PIC X(6)   VALUE 'ECS021'.               ECS021
PEMTST     12  HDR1-ID         PIC XXX    VALUE '   '.
00505                                                                   ECS021
00506  01  HEADER-TWO.                                                  ECS021
00507      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00508      12  BRK-2-DESC      PIC X(15)  VALUE 'LEVEL 2 CNTL :'.       ECS021
00509      12  BRK-2-CNTL      PIC X(10)  VALUE SPACES.                 ECS021
00510      12  FILLER          PIC X(23)  VALUE SPACES.                 ECS021
00511      12  HDG-2-COMPANY.                                           ECS021
00512          16  FILLER      PIC X(5)   VALUE SPACES.                 ECS021
00513          16  FILLER      PIC X(20)  VALUE '--  COMPANY  NAME --'. ECS021
00514          16  FILLER      PIC X(5)   VALUE SPACES.                 ECS021
00515      12  FILLER          PIC X(42)  VALUE SPACES.                 ECS021
00516      12  HDG-2-DATE      PIC X(8)   VALUE 'XX/XX/XX'.             ECS021
00517                                                                   ECS021
00518  01  HEADER-THREE.                                                ECS021
00519      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00520      12  BRK-3-DESC      PIC X(15)  VALUE 'LEVEL 3 CNTL :'.       ECS021
00521      12  BRK-3-CNTL      PIC X(10)  VALUE SPACES.                 ECS021
00522      12  FILLER          PIC X(29)  VALUE SPACES.                 ECS021
00523      12  HDG-3-DATE      PIC X(18)  VALUE '--  ALPHA DATE  --'.   ECS021
042407     12  FILLER          PIC X(47)  VALUE SPACES.                 ECS021
00525      12  FILLER          PIC X(6)   VALUE 'PAGE'.                 ECS021
042407     12  HDG-3-PAGE      PIC ZZZZ9.                               ECS021
00527                                                                   ECS021
00528  01  HEADER-FOUR.                                                 ECS021
00529      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00530      12  BRK-4-DESC      PIC X(15)  VALUE 'LEVEL 4 CNTL :'.       ECS021
00531      12  BRK-4-CNTL      PIC X(10)  VALUE SPACES.                 ECS021
00532      12  FILLER          PIC X(23)  VALUE SPACES.                 ECS021
00533      12  HDG-4-OPTION    PIC X(30)  VALUE SPACES.                 ECS021
00534      12  FILLER          PIC X(44)  VALUE SPACES.                 ECS021
00535                                                                   ECS021
00536  01  HEADER-FIVE.                                                 ECS021
00537      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00538      12  BRK-5-DESC      PIC X(15)  VALUE 'LEVEL 5 CNTL :'.       ECS021
00539      12  BRK-5-CNTL      PIC X(10)  VALUE SPACES.                 ECS021
00540      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00541                                                                   ECS021
00542  01  HEADER-SIX.                                                  ECS021
00543      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00544      12  BRK-6-DESC      PIC X(15)  VALUE 'LEVEL 6 CNTL :'.       ECS021
00545      12  BRK-6-CNTL      PIC X(10)  VALUE SPACES.                 ECS021
00546      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00547                                                                   ECS021
00548      EJECT                                                        ECS021
00549  01  HEADER-SEVEN.                                                ECS021
00550      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00551      12  HDG-7-TYPE      PIC X(6)   VALUE SPACES.                 ECS021
00552      12  FILLER          PIC X(10)  VALUE ' COVERAGE'.            ECS021
00553      12  HDG-7-BENEFIT   PIC XX     VALUE SPACES.                 ECS021
00554      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00555      12  HDG-7-DESC      PIC X(10)  VALUE SPACES.                 ECS021
00556      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00557      12  HDG-7-COMMENT   PIC X(10)  VALUE SPACES.                 ECS021
101205     12  FILLER          PIC X(24)  VALUE SPACES.                 ECS021
101205     12  HDG-7-PGM-OPT   PIC X(66)  VALUE SPACES.                 ECS021
00560                                                                   ECS021
00561  01  HEADER-EIGHT.                                                ECS021
00562      12  HDG-8-CTL        PIC X.                                  ECS021
00563      12  FILLER           PIC X(10)  VALUE SPACES.                ECS021
00564      12  HDG-8-NET        PIC XXX    VALUE 'NET'.                 ECS021
00565      12  FILLER           PIC X(29)  VALUE SPACES.                ECS021
00566      12  HDG-8-NET-WRT    PIC X(11)  VALUE 'NET WRITTEN'.         ECS021
00567      12  FILLER           PIC X(6)   VALUE SPACES.                ECS021
00568      12  HDG-8-EARNED     PIC X(6)   VALUE 'EARNED'.              ECS021
00569      12  FILLER           PIC XXX    VALUE SPACES.                ECS021
00570      12  HDG-8-CANCEL     PIC X(6)   VALUE 'CANCEL'.              ECS021
00571      12  FILLER           PIC X(7)   VALUE SPACES.                ECS021
00572      12  HDG-8-CLAIMS     PIC X(6)   VALUE 'CLAIMS'.              ECS021
00573      12  FILLER           PIC XXX    VALUE SPACES.                ECS021
00574      12  HDG-8-INCR-DECR  PIC X(11)  VALUE 'INCR / DECR'.         ECS021
00575      12  FILLER           PIC X(5)   VALUE SPACES.                ECS021
00576      12  HDG-8-LOSS       PIC X(4)   VALUE 'LOSS'.                ECS021
00577      12  FILLER           PIC X      VALUE SPACES.                ECS021
00578      12  HDG-8-TOTAL      PIC X(5)   VALUE 'TOTAL'.               ECS021
00579      12  FILLER           PIC X      VALUE SPACES.                ECS021
00580      12  HDG-8-EXP-EARN   PIC X(8)   VALUE 'EXP/EARN'.            ECS021
00581      12  FILLER           PIC X      VALUE SPACES.                ECS021
00582      12  HDG-8-PERIOD     PIC X(6)   VALUE 'PERIOD'.              ECS021
00583                                                                   ECS021
00584  01  HEADER-NINE.                                                 ECS021
00585      12  HDG-9-CTL        PIC X.                                  ECS021
00586      12  HDG-9-PERIOD     PIC X(6)    VALUE 'PERIOD'.             ECS021
00587      12  FILLER           PIC XXX     VALUE SPACES.               ECS021
00588      12  HDG-9-CVRGS      PIC X(5)    VALUE 'COUNT'.              ECS021
00589      12  FILLER           PIC XX      VALUE SPACES.               ECS021
00590      12  HDG-9-BEN-TYP1   PIC X(6)    VALUE 'SINGLE'.             ECS021
00591      12  FILLER           PIC XXX     VALUE SPACES.               ECS021
00592      12  HDG-9-BEN-TYP2   PIC X(5)    VALUE 'JOINT'.              ECS021
00593      12  FILLER           PIC XXX     VALUE SPACES.               ECS021
00594      12  HDG-9-BEN-TYP3   PIC X(6)    VALUE 'LEVEL '.             ECS021
00595      12  FILLER           PIC X(5)    VALUE SPACES.               ECS021
00596      12  HDG-9-PREMIUM-1  PIC X(7)    VALUE 'PREMIUM'.            ECS021
00597      12  FILLER           PIC X(8)    VALUE SPACES.               ECS021
00598      12  HDG-9-PREMIUM-2  PIC X(7)    VALUE 'PREMIUM'.            ECS021
00599      12  FILLER           PIC XXX     VALUE SPACES.               ECS021
00600      12  HDG-9-RATIO-1    PIC X(5)    VALUE 'RATIO'.              ECS021
00601      12  FILLER           PIC XXX     VALUE SPACES.               ECS021
00602      12  HDG-9-CLAIMS     PIC X(11)   VALUE '     PAID'.          ECS021
00603      12  FILLER           PIC XX      VALUE SPACES.               ECS021
00604      12  HDG-9-LOSS-RSVS  PIC X(12)   VALUE 'LOSS RESERVE'.       ECS021
00605      12  FILLER           PIC XXX     VALUE SPACES.               ECS021
00606      12  HDG-9-RATIO-2    PIC X(5)    VALUE 'RATIO'.              ECS021
00607      12  FILLER           PIC X       VALUE SPACES.               ECS021
00608      12  HDG-9-COMP       PIC X(6)    VALUE 'COMP %'.             ECS021
00609      12  FILLER           PIC XX      VALUE SPACES.               ECS021
00610      12  HDG-9-RATIO-3    PIC X(5)    VALUE 'RATIO'.              ECS021
00611      12  FILLER           PIC X       VALUE SPACES.               ECS021
00612      12  HDG-9-PROFIT     PIC X(7)    VALUE 'PROFIT%'.            ECS021
00613                                                                   ECS021
00614  01  DASH-LINE-1.                                                 ECS021
00615      12  FILLER          PIC X(133) VALUE ALL '-'.                ECS021
00616                                                                   ECS021
00617      EJECT                                                        ECS021
00618  01  BREAK-HEADINGS.                                              ECS021
00619      12  BREAK-DESC       OCCURS  6  TIMES  PIC X(15).            ECS021
00620                                                                   ECS021
00621  01  HEADINGS-DESCRIPTIONS.                                       ECS021
00622      12  CARRIER-DESC       PIC X(15) VALUE '     CARRIER :'.     ECS021
00623      12  GROUPING-DESC      PIC X(15) VALUE '    GROUPING :'.     ECS021
00624      12  STATE-DESC         PIC X(15) VALUE '       STATE :'.     ECS021
00625      12  ACCOUNT-DESC       PIC X(15) VALUE '     ACCOUNT :'.     ECS021
00626      12  AGENT-DESC         PIC X(15) VALUE 'GENERAL AGENT:'.     ECS021
00627      12  BUSINESS-DESC      PIC X(15) VALUE 'BUSINESS TYPE:'.     ECS021
00628      12  GA-NAME-DESC       PIC X(15) VALUE 'GEN AGT NAME :'.     ECS021
00629      12  RPT-CDE1-DESCRIPT.                                       ECS021
00630          16  FILLER         PIC X(4)  VALUE SPACES.               ECS021
00631          16  RPT-CDE1-DESC  PIC X(10) VALUE 'RPT CODE 1'.         ECS021
00632          16  FILLER         PIC X(1)  VALUE SPACES.               ECS021
00633      12  RPT-CDE2-DESCRIPT.                                       ECS021
00634          16  FILLER         PIC X(5)  VALUE SPACES.               ECS021
00635          16  RPT-CDE2-DESC  PIC X(10) VALUE 'RPT CODE 2'.         ECS021
043007     12  RPT-CDE3-DESCRIPT.                                       ECS021
043007         16  FILLER         PIC X(4)  VALUE SPACES.               ECS021
043007         16  RPT-CDE3-DESC  PIC X(10) VALUE 'RPT CODE 3'.         ECS021
00636      12  SELECT1-DESC       PIC X(15) VALUE ' USER CODE 1 :'.     ECS021
00637      12  SELECT2-DESC       PIC X(15) VALUE ' USER CODE 2 :'.     ECS021
00638      12  SELECT3-DESC       PIC X(15) VALUE ' USER CODE 3 :'.     ECS021
00639      12  SELECT4-DESC       PIC X(15) VALUE ' USER CODE 4 :'.     ECS021
00640      12  SELECT5-DESC       PIC X(15) VALUE ' USER CODE 5 :'.     ECS021
00641      12  SELECT6-DESC       PIC X(15) VALUE ' USER CODE 6 :'.     ECS021
00642      12  NET-WRT-DESC.                                            ECS021
00643          16  FILLER         PIC XXX VALUE '**'.                   ECS021
00644          16  FILLER         PIC X(34) VALUE                       ECS021
00645          'RATIOS AND PERCENTS DERIVED USING '.
101205         16  NET-WRT-DESC-SPE    PIC X(25) VALUE
101205         'NET WRITTEN PREMIUM'.
00646          16  FILLER         PIC XXX VALUE ' **'.                  ECS021
00647      12  EARNED-DESC.                                             ECS021
00648          16  FILLER         PIC XXX VALUE '** '.                  ECS021
00649          16  FILLER         PIC X(34) VALUE
00650          'RATIOS AND PERCENTS DERIVED USING '.
101205         16  EARNED-DESC-SPE     PIC X(25) VALUE
101205         'EARNED PREMIUM '.
00651          16  FILLER         PIC XXX VALUE ' **'.                  ECS021
00652      12  SINGLE-DESC        PIC X(6) VALUE 'SINGLE'.              ECS021
00653      12  ELIM-DESC          PIC X(6) VALUE ' ELIM '.              ECS021
00654      12  JOINT-DESC         PIC X(5) VALUE 'JOINT'.               ECS021
00655      12  RETRO-DESC         PIC X(5) VALUE 'RETRO'.               ECS021
00656      12  LEVEL-DESC         PIC X(6) VALUE 'LEVEL '.              ECS021
00657      12  PENE-DESC          PIC X(6) VALUE 'PENE %'.              ECS021
00658                                                                   ECS021
00659      EJECT                                                        ECS021
00660  01  HEADER-EXCEP-ONE.                                            ECS021
00661      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00662      12  FILLER          PIC X(54)  VALUE SPACES.                 ECS021
00663      12  FILLER          PIC X(70)  VALUE 'EXCEPTION REPORT'.     ECS021
00664      12  FILLER          PIC X(8)   VALUE 'ECS-021E'.             ECS021
00665                                                                   ECS021
00666  01  HEADER-EXCEP-TWO.                                            ECS021
00667      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00668      12  FILLER          PIC X(48)  VALUE SPACES.                 ECS021
00669      12  HDGE-2-COMPANY.                                          ECS021
00670          16  FILLER      PIC X(5)   VALUE SPACES.                 ECS021
00671          16  FILLER      PIC X(20)  VALUE '--  COMPANY  NAME --'. ECS021
00672          16  FILLER      PIC X(5)   VALUE SPACES.                 ECS021
00673      12  FILLER          PIC X(44)  VALUE SPACES.                 ECS021
00674      12  HDGE-2-DATE      PIC X(8)   VALUE 'XX/XX/XX'.            ECS021
00675                                                                   ECS021
00676  01  HEADER-EXCEP-THREE.                                          ECS021
00677      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00678      12  FILLER          PIC X(54)  VALUE SPACES.                 ECS021
00679      12  HDGE-3-DATE     PIC X(18)  VALUE '--  ALPHA DATE  --'.   ECS021
042407     12  FILLER          PIC X(47)  VALUE SPACES.                 ECS021
00681      12  FILLER          PIC X(6)   VALUE 'PAGE'.                 ECS021
042407     12  HDGE-3-PAGE     PIC ZZZZ9.                               ECS021
00683                                                                   ECS021
00684  01  HEADER-EXCEP-FOUR.                                           ECS021
00685      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00686      12  HDGE-1-DESC     PIC X(15)  VALUE 'LEVEL 1 CNTL :'.       ECS021
00687      12  HDGE-1-CNTL     PIC X(10)  VALUE SPACES.                 ECS021
00688      12  FILLER          PIC X(2)   VALUE SPACES.                 ECS021
00689      12  HDGE-2-DESC     PIC X(15)  VALUE 'LEVEL 2 CNTL :'.       ECS021
00690      12  HDGE-2-CNTL     PIC X(10)  VALUE SPACES.                 ECS021
00691      12  FILLER          PIC X(2)   VALUE SPACES.                 ECS021
00692      12  HDGE-3-DESC     PIC X(15)  VALUE 'LEVEL 3 CNTL :'.       ECS021
00693      12  HDGE-3-CNTL     PIC X(10)  VALUE SPACES.                 ECS021
00694      12  FILLER          PIC X(2)   VALUE SPACES.                 ECS021
00695      12  HDGE-4-DESC     PIC X(15)  VALUE 'LEVEL 4 CNTL :'.       ECS021
00696      12  HDGE-4-CNTL     PIC X(10)  VALUE SPACES.                 ECS021
00697                                                                   ECS021
00698  01  HEADER-EXCEP-FIVE.                                           ECS021
00699      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00700      12  HDGE-5-DESC     PIC X(15)  VALUE 'LEVEL 5 CNTL :'.       ECS021
00701      12  HDGE-5-CNTL     PIC X(10)  VALUE SPACES.                 ECS021
00702      12  FILLER          PIC X(2)   VALUE SPACES.                 ECS021
00703      12  HDGE-6-DESC     PIC X(15)  VALUE 'LEVEL 6 CNTL :'.       ECS021
00704      12  HDGE-6-CNTL     PIC X(10)  VALUE SPACES.                 ECS021
00705      12  FILLER          PIC X(2)   VALUE SPACES.                 ECS021
00706      12  HDGE-7-DESC     PIC X(15)  VALUE 'GEN AGT NAME :'.       ECS021
00707      12  HDGE-7-CNTL     PIC X(30)  VALUE SPACES.                 ECS021
00708                                                                   ECS021
00709                                                                   ECS021
00710  01  WS-EXCEPTION-PRINT-MSGS.                                     ECS021
00711      12  EXP-ISS-CNT-MSG.                                         ECS021
00712          14  FILLER               PIC X      VALUE SPACES.        ECS021
00713          14  FILLER               PIC X(26)  VALUE                ECS021
00714                  'ISSUE COUNT DIFFERENCE OF '.                    ECS021
00715          14  EXP-ISS-CNT-LIMIT    PIC ZZZZ9.                      ECS021
00716          14  FILLER               PIC X(11)  VALUE                ECS021
00717                  ' EXCEEDED  '.                                   ECS021
00718          14  FILLER               PIC X(16)  VALUE                ECS021
00719                  ' CURRENT MONTH: '.                              ECS021
00720          14  EXP-CMO-ISS-CNT      PIC Z,ZZZ,ZZ9.                  ECS021
00721          14  FILLER               PIC X(02)  VALUE SPACES.        ECS021
00722          14  FILLER               PIC X(13)  VALUE                ECS021
00723                  'PRIOR MONTH: '.                                 ECS021
00724          14  EXP-PMO-ISS-CNT      PIC Z,ZZZ,ZZ9.                  ECS021
00725      12  EXP-PRM-CHG-MSG.                                         ECS021
00726          14  FILLER               PIC X      VALUE SPACES.        ECS021
00727          14  FILLER               PIC X(42)  VALUE                ECS021
00728                  'WRITTEN PREMIUM EXCEPTION % EXCEEDED -    '.    ECS021
00729          14  FILLER               PIC X(15)  VALUE                ECS021
00730                  'CURRENT MONTH: '.                               ECS021
00731          14  EXP-CMO-WRITTEN-PREM PIC Z,ZZZ,ZZ9-.                 ECS021
00732          14  FILLER               PIC X(02)  VALUE SPACES.        ECS021
00733          14  FILLER               PIC X(13)  VALUE                ECS021
00734                  'PRIOR MONTH: '.                                 ECS021
00735          14  EXP-PMO-WRITTEN-PREM PIC Z,ZZZ,ZZ9-.                 ECS021
00736          14  FILLER               PIC X(09)  VALUE ' CHANGE: '.   ECS021
00737          14  EXP-WRITTEN-PREM-PCT PIC ZZ9.                        ECS021
00738          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00739      12  EXP-PRM-DECR-MSG.                                        ECS021
00740          14  FILLER               PIC X      VALUE SPACES.        ECS021
00741          14  FILLER               PIC X(37)  VALUE                ECS021
00742                  'EARNED PREMIUM DECLINE EXCEEDED -    '.         ECS021
00743          14  FILLER               PIC X(21)  VALUE                ECS021
00744                  ' LAST TWELVE MONTHS: '.                         ECS021
00745          14  EXP-L12-EARND-PRM    PIC Z,ZZZ,ZZ9-.                 ECS021
00746          14  FILLER               PIC X(02)  VALUE SPACES.        ECS021
00747          14  FILLER               PIC X(21)  VALUE                ECS021
00748                  'PRIOR TWELVE MONTHS: '.                         ECS021
00749          14  EXP-P12-EARND-PRM    PIC Z,ZZZ,ZZ9-.                 ECS021
00750          14  FILLER               PIC X(02)  VALUE SPACES.        ECS021
00751          14  FILLER               PIC X(10)  VALUE                ECS021
00752                  'DECREASE: '.                                    ECS021
00753          14  EXP-PRM-DECR-PCT     PIC ZZ9.9.                      ECS021
00754          14  FILLER               PIC X(02)  VALUE     ' %'.      ECS021
00755      12  EXP-CANCEL-RATIO-MSG.                                    ECS021
00756          14  FILLER               PIC X      VALUE SPACES.        ECS021
00757          14  FILLER               PIC X(25)  VALUE                ECS021
00758                  'CANCEL RATIO EXCEEDED -  '.                     ECS021
00759          14  FILLER               PIC X(35)  VALUE                ECS021
00760                  'CURRENT MONTH PREMIUMS  -  ISSUES: '.           ECS021
00761          14  EXP-ISS-PREM         PIC ZZ,ZZZ,ZZZ,ZZZ.99-.         ECS021
00762          14  FILLER               PIC X(10)  VALUE                ECS021
00763                  ' CANCELS: '.                                    ECS021
00764          14  EXP-CNC-PREM         PIC ZZ,ZZZ,ZZZ,ZZZ.99-.         ECS021
00765          14  FILLER               PIC X(08)  VALUE                ECS021
00766                  ' RATIO: '.                                      ECS021
00767          14  EXP-CANCEL-RATIO     PIC ZZ9.9.                      ECS021
00768          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00769      12  EXP-RETENTION-MSG.                                       ECS021
00770          14  FILLER               PIC X      VALUE SPACES.        ECS021
00771          14  FILLER               PIC X(27)  VALUE                ECS021
00772                  'RETENTION LIMIT EXCEEDED - '.                   ECS021
00773          14  FILLER               PIC X(7)   VALUE                ECS021
00774                  'LIMIT: '.                                       ECS021
00775          14  EXP-RETENTION-LIMIT  PIC Z,ZZZ,ZZZ-.                 ECS021
00776          14  FILLER               PIC X(13)  VALUE                ECS021
00777                  ' CALCULATED: '.                                 ECS021
00778          14  EXP-RETENTION-CALC   PIC ZZ,ZZZ,ZZZ,ZZZ.99-.         ECS021
00779      12  EXP-LOSS-RATIO-MSG.                                      ECS021
00780          14  FILLER               PIC X      VALUE SPACES.        ECS021
00781          14  EXP-COVG-DESC-1      PIC X(06).                      ECS021
00782          14  FILLER               PIC X(20)  VALUE                ECS021
00783                  'LOSS RATIO LIMIT OF '.                          ECS021
00784          14  EXP-LOSS-RATIO-LIMIT PIC ZZ9.                        ECS021
00785          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00786          14  FILLER               PIC X(12)  VALUE                ECS021
00787                  ' EXCEEDED   '.                                  ECS021
00788          14  FILLER               PIC X(21)  VALUE                ECS021
00789                  'CURRENT MONTH RATIO: '.                         ECS021
00790          14  EXP-LOSS-RATIO       PIC Z9.999.                     ECS021
00791          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00792      12  EXP-PROFIT-PERCENT-MSG.                                  ECS021
00793          14  FILLER               PIC X      VALUE SPACES.        ECS021
00794          14  EXP-COVG-DESC-2      PIC X(06).                      ECS021
00795          14  FILLER               PIC X(27)  VALUE                ECS021
00796                  'PERIOD PROFIT EXCEEDED -   '.                   ECS021
00797          14  FILLER               PIC X(22)  VALUE                ECS021
00798                  'CURRENT MONTH PROFIT: '.                        ECS021
00799          14  EXP-PROFIT-PERCENT   PIC Z9.999.                     ECS021
00800          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00801      12  EXP-LTM-LOSS-RATIO-MSG.                                  ECS021
00802          14  FILLER               PIC X      VALUE SPACES.        ECS021
00803          14  EXP-COVG-DESC-3      PIC X(06).                      ECS021
00804          14  FILLER               PIC X(38)  VALUE                ECS021
00805                  'LAST 12 MONTHS LOSS RATIO EXCEEDED -  '.        ECS021
00806          14  FILLER               PIC X(20)  VALUE                ECS021
00807                  'LAST TWELVE MONTHS: '.                          ECS021
00808          14  EXP-L12-LOSS-RATIO   PIC Z9.999.                     ECS021
00809          14  FILLER               PIC X(03)  VALUE ' % '.         ECS021
00810          14  FILLER               PIC X(21)  VALUE                ECS021
00811                  'PRIOR TWELVE MONTHS: '.                         ECS021
00812          14  EXP-P12-LOSS-RATIO   PIC Z9.999.                     ECS021
00813          14  FILLER               PIC X(03)  VALUE ' % '.         ECS021
00814          14  FILLER               PIC X(21)  VALUE                ECS021
00815                  'CHANGE: '.                                      ECS021
00816          14  EXP-LOSS-RATIO-DIFF  PIC Z9.999.                     ECS021
00817          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00818      12  EXP-LTM-PERIOD-PROFIT-MSG.                               ECS021
00819          14  FILLER               PIC X      VALUE SPACES.        ECS021
00820          14  EXP-COVG-DESC-4      PIC X(06).                      ECS021
00821          14  FILLER               PIC X(40)  VALUE                ECS021
00822                  'LAST 12 MONTHS PERIOD PROFIT EXCEEDED - '.      ECS021
00823          14  FILLER               PIC X(20)  VALUE                ECS021
00824                  'LAST TWELVE MONTHS: '.                          ECS021
00825          14  EXP-L12-PROFIT-PCT   PIC Z9.999.                     ECS021
00826          14  FILLER               PIC X(03)  VALUE ' % '.         ECS021
00827          14  FILLER               PIC X(21)  VALUE                ECS021
00828                  'PRIOR TWELVE MONTHS: '.                         ECS021
00829          14  EXP-P12-PROFIT-PCT   PIC Z9.999.                     ECS021
00830          14  FILLER               PIC X(03)  VALUE ' % '.         ECS021
00831          14  FILLER               PIC X(21)  VALUE                ECS021
00832                  'CHANGE: '.                                      ECS021
00833          14  EXP-PROFIT-PCT-DIFF  PIC Z9.999.                     ECS021
00834          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00835      12  EXP-LTM-INFORCE-CNT-MSG.                                 ECS021
00836          14  FILLER               PIC X      VALUE SPACES.        ECS021
00837          14  EXP-COVG-DESC-5      PIC X(06).                      ECS021
00838          14  FILLER               PIC X(39)  VALUE                ECS021
00839                  'LAST 12 MONTH INFORCE EXCEEDED - '.             ECS021
00840          14  FILLER               PIC X(17)  VALUE                ECS021
00841                  'LAST 12 AVERAGE: '.                             ECS021
00842          14  EXP-LTM-INFORCE-AVG  PIC ZZZ,ZZZ,ZZ9.                ECS021
00843          14  FILLER               PIC X(16)  VALUE                ECS021
00844                  ' CURRENT MONTH: '.                              ECS021
00845          14  EXP-CMO-INFORCE-CNT  PIC ZZZ,ZZZ,ZZ9.                ECS021
00846          14  FILLER               PIC X(10)  VALUE                ECS021
00847                  '  CHANGE: '.                                    ECS021
00848          14  EXP-AVG-INFORCE-DIFF PIC ZZ9.9.                      ECS021
00849          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00850      12  EXP-LTM-TERM-MSG.                                        ECS021
00851          14  FILLER               PIC X      VALUE SPACES.        ECS021
00852          14  EXP-COVG-DESC-6      PIC X(06).                      ECS021
00853          14  FILLER               PIC X(31)  VALUE                ECS021
00854                  'LAST 12 MONTHS TERM EXCEEDED - '.               ECS021
00855          14  FILLER               PIC X(20)  VALUE                ECS021
00856                  'LAST TWELVE MONTHS: '.                          ECS021
00857          14  EXP-LTM-ORIG-TRM-AVG PIC ZZ9.9.                      ECS021
00858          14  FILLER               PIC X(03)  VALUE ' % '.         ECS021
00859          14  FILLER               PIC X(15)  VALUE                ECS021
00860                  'CURRENT MONTH: '.                               ECS021
00861          14  EXP-CMO-ORIG-TRM-CNT PIC ZZ9.9.                      ECS021
00862          14  FILLER               PIC X(03)  VALUE ' % '.         ECS021
00863          14  FILLER               PIC X(08)  VALUE                ECS021
00864                  'CHANGE: '.                                      ECS021
00865          14  EXP-AVG-TERM-DIFF    PIC ZZ9.9.                      ECS021
00866          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00867      12  EXP-LTM-AGE-MSG.                                         ECS021
00868          14  FILLER               PIC X      VALUE SPACES.        ECS021
00869          14  EXP-COVG-DESC-7      PIC X(06).                      ECS021
00870          14  FILLER               PIC X(36)  VALUE                ECS021
00871                  'LAST 12 MONTHS AGE LIMIT EXCEEDED - '.          ECS021
00872          14  FILLER               PIC X(25)  VALUE                ECS021
00873                  'LAST TWELVE MONTHS AVRG: '.                     ECS021
00874          14  EXP-LTM-ISS-AGE-AVG  PIC Z9.9.                       ECS021
00875          14  FILLER               PIC X(16)  VALUE                ECS021
00876                  ' CURRENT MONTH: '.                              ECS021
00877          14  EXP-CMO-ISS-AGE      PIC Z9.9.                       ECS021
00878          14  FILLER               PIC X(09)  VALUE                ECS021
00879                  ' CHANGE: '.                                     ECS021
00880          14  EXP-ISS-AGE-DIFF     PIC ZZ9.9.                      ECS021
00881          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00882      12  EXP-LTM-WGHT-TERM-MSG.                                   ECS021
00883          14  FILLER               PIC X      VALUE SPACES.        ECS021
00884          14  EXP-COVG-DESC-8      PIC X(06).                      ECS021
00885          14  FILLER               PIC X(41)  VALUE                ECS021
00886                  ' LAST 12 MONTHS WEIGHTED TERM EXCEEDED - '.     ECS021
00887          14  FILLER               PIC X(25)  VALUE                ECS021
00888                  'LAST TWELVE MONTHS AVRG: '.                     ECS021
00889          14  EXP-LTM-WGHT-TRM-AVG PIC ZZ9.9.                      ECS021
00890          14  FILLER               PIC X(16)  VALUE                ECS021
00891                  ' CURRENT MONTH: '.                              ECS021
00892          14  EXP-CMO-WGHT-TRM     PIC ZZ9.9.                      ECS021
00893          14  FILLER               PIC X(09)  VALUE                ECS021
00894                  ' CHANGE: '.                                     ECS021
00895          14  EXP-WGHT-TERM-DIFF   PIC ZZ9.9.                      ECS021
00896          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00897      12  EXP-LTM-WGHT-AGE-MSG.                                    ECS021
00898          14  FILLER               PIC X      VALUE SPACES.        ECS021
00899          14  EXP-COVG-DESC-9      PIC X(06).                      ECS021
00900          14  FILLER               PIC X(45)  VALUE                ECS021
00901                 ' LAST 12 MONTHS WEIGHTED AVRG AGE EXCEEDED - '.  ECS021
00902          14  FILLER               PIC X(25)  VALUE                ECS021
00903                  'LAST TWELVE MONTHS AVRG: '.                     ECS021
00904          14  EXP-LTM-WGHT-AGE-AVG PIC ZZ9.9.                      ECS021
00905          14  FILLER               PIC X(16)  VALUE                ECS021
00906                  ' CURRENT MONTH: '.                              ECS021
00907          14  EXP-CMO-WGHT-AGE     PIC ZZ9.9.                      ECS021
00908          14  FILLER               PIC X(09)  VALUE                ECS021
00909                  ' CHANGE: '.                                     ECS021
00910          14  EXP-WGHT-AGE-DIFF   PIC ZZ9.9.                       ECS021
00911          14  FILLER               PIC X(02)  VALUE ' %'.          ECS021
00912      12  EXP-MAX-ISS-AGE-MSG.                                     ECS021
00913          14  FILLER               PIC X      VALUE SPACES.        ECS021
00914          14  EXP-COVG-DESC-10     PIC X(06).                      ECS021
00915          14  FILLER               PIC X(26)  VALUE                ECS021
00916                  'MAXIMUM AVERAGE ISSUE AGE '.                    ECS021
00917          14  EXP-MAX-ISS-AGE      PIC Z9.                         ECS021
00918          14  FILLER               PIC X(43)  VALUE                ECS021
00919                  ' HAS BEEN EXCEEDED - CALCULATED AVERAGE IS '.   ECS021
00920          14  EXP-AVG-ISS-AGE      PIC Z9.9.                       ECS021
00921  01  DETAIL-LINE-1.                                               ECS021
00922      12  FILLER                   PIC X      VALUE SPACES.        ECS021
00923      12  DTL-MONTH-1              PIC X(4)   VALUE SPACES.        ECS021
00924      12  DTL-YEAR-1               PIC XX     VALUE SPACES.        ECS021
00925      12  FILLER                   PIC X      VALUE SPACES.        ECS021
00926      12  DTL-NET-CVRG             PIC ZZZZZZ9-.                   ECS021
00927      12  DTL-SNGL-ELIM            PIC ZZZZZZ9-.                   ECS021
00928      12  DTL-JNT-RETRO            PIC ZZZZZZ9-.                   ECS021
00929      12  DTL-LEVEL-LIFE           PIC ZZZZZZ9-.                   ECS021
00930      12  DTL-PENETRATION  REDEFINES DTL-LEVEL-LIFE.               ECS021
00931          16  FILLER               PIC XX.                         ECS021
00932          16  DTL-PENE-PERCENT-OV.                                 ECS021
00933              20  DTL-PENE-PERCENT PIC ZZ9.9-.                     ECS021
00934      12  DTL-NET-PREM             PIC ZZ,ZZZ,ZZZ,ZZ9-.            ECS021
00935      12  DTL-EARND-PREM           PIC ZZ,ZZZ,ZZZ,ZZ9-.            ECS021
00936      12  DTL-CNCL-RATIO-OV.                                       ECS021
00937          16  DTL-CNCL-RATIO       PIC ZZ9.9-.                     ECS021
00938      12  DTL-CLAIMS-PAID          PIC ZZ,ZZZ,ZZZ,ZZ9-.            ECS021
00939      12  DTL-LOSS-RSVS            PIC ZZ,ZZZ,ZZZ,ZZ9-.            ECS021
00940      12  DTL-LOSS-RATIO-OV.                                       ECS021
00941          16  DTL-LOSS-RATIO       PIC ZZ9.9-.                     ECS021
00942      12  FILLER                   PIC X      VALUE SPACES.        ECS021
00943      12  DTL-COMP-PRCNT-OV.                                       ECS021
00944          16  DTL-COMP-PRCNT       PIC ZZ9.9-.                     ECS021
00945      12  FILLER                   PIC X      VALUE SPACES.        ECS021
00946      12  DTL-EXP-EARN-RATIO-OV.                                   ECS021
00947          16  DTL-EXP-EARN-RATIO   PIC ZZ9.9-.                     ECS021
00948      12  DTL-PROFIT-PRCNT-OV.                                     ECS021
00949          16  DTL-PROFIT-PRCNT     PIC ZZ9.99-.                    ECS021
00950                                                                   ECS021
00951      EJECT                                                        ECS021
00952  01  HEADER-TEN.                                                  ECS021
00953      12  FILLER          PIC X(80)  VALUE SPACES.                 ECS021
00954      12  FILLER          PIC X(13)  VALUE 'AVERAGE'.              ECS021
00955      12  FILLER          PIC X(7)   VALUE 'AVERAGE'.              ECS021
00956                                                                   ECS021
00957  01  HEADER-ELEVEN.                                               ECS021
00958      12  FILLER          PIC X(40)  VALUE                         ECS021
00959          '         INFORCE      AVERAGE     AVERAG'.              ECS021
00960      12  FILLER          PIC X(40)  VALUE                         ECS021
00961          'E    WEIGHTED    AVERAGE    WEIGHTED'.                  ECS021
00962      12  FILLER          PIC X      VALUE SPACES.                 ECS021
00963      12  HDG-11-TYPE-LF  PIC X(6)   VALUE ' LIFE'.                ECS021
00964      12  FILLER          PIC X(6)   VALUE SPACES.                 ECS021
00965      12  HDG-11-TYPE-AH  PIC X(6)   VALUE ' A&H'.                 ECS021
00966      12  FILLER          PIC X(21)  VALUE '       AVERAGE'.       ECS021
00967                                                                   ECS021
00968  01  HEADER-TWELVE.                                               ECS021
00969      12  FILLER          PIC X(40)  VALUE                         ECS021
00970          '          COUNT       PREMIUMS   ORIG TE'.              ECS021
00971      12  FILLER          PIC X(40)  VALUE                         ECS021
00972          'RM   ORIG TERM  ISSUE AGE  ISSUE AGE'.                  ECS021
00973      12  FILLER          PIC X(40)  VALUE                         ECS021
00974          ' CLAIM        CLAIM       REFUNDS'.                     ECS021
00975                                                                   ECS021
00976  01  DASH-LINE-2.                                                 ECS021
00977      12  FILLER          PIC X(127) VALUE  ALL '-'.               ECS021
00978                                                                   ECS021
00979  01  DETAIL-LINE-2.                                               ECS021
00980      12  FILLER             PIC X      VALUE SPACES.              ECS021
00981      12  DTL-LINE-2-DESC.                                         ECS021
00982          16  DTL-MONTH-2    PIC X(4)   VALUE SPACES.              ECS021
00983          16  DTL-YEAR-2     PIC XX     VALUE SPACES.              ECS021
00984      12  DTL-INFRC-CVRG     PIC ZZZZZZZZ9-.                       ECS021
00985      12  FILLER             PIC XXX    VALUE SPACES.              ECS021
00986      12  DTL-AVRG-PREM      PIC Z,ZZZ,ZZ9.99.                     ECS021
00987      12  FILLER             PIC XX     VALUE SPACES.              ECS021
00988      12  DTL-AVRG-ORIG-TERM PIC ZZ9.9.                            ECS021
00989      12  FILLER             PIC X(7)   VALUE SPACES.              ECS021
00990      12  DTL-WGHT-ORIG-TERM PIC ZZ9.9.                            ECS021
00991      12  FILLER             PIC X(8)   VALUE SPACES.              ECS021
00992      12  DTL-AVRG-ISS-AGE   PIC Z9.9.                             ECS021
00993      12  FILLER             PIC X(7)   VALUE SPACES.              ECS021
00994      12  DTL-WGHT-ISS-AGE   PIC Z9.9.                             ECS021
00995      12  FILLER             PIC X      VALUE SPACES.              ECS021
00996      12  DTL-AVRG-LF-CLAIM  PIC Z,ZZZ,ZZ9.99-.                    ECS021
00997      12  DTL-AVRG-AH-CLAIM  PIC Z,ZZZ,ZZ9.99-.                    ECS021
00998      12  DTL-AVRG-REFUND    PIC Z,ZZZ,ZZ9.99-.                    ECS021
00999      12  FILLER             PIC X(9)   VALUE SPACES.              ECS021
01000                                                                   ECS021
01001      EJECT                                                        ECS021
01002  01  COMMON-IDENTIFIER.                                           ECS021
01003      12  FILLER                       PIC X(28)                   ECS021
01004                        VALUE '**** START OF COMMON TBL****'.      ECS021
01005  01  COMMON-TABLE.                                                ECS021
01006      12  COMMON-ACCUMULATORS   OCCURS  900 TIMES.                 ECS021
01007          16  CMMN-BENEFIT-TYPE        PIC X.                      ECS021
01008          16  CMMN-BENEFIT-CODE        PIC 99.                     ECS021
01009          16  CMMN-PERIOD    OCCURS   15  TIMES  COMP-3.           ECS021
01010              20  CMMN-ISS-CNT         PIC S9(7).                  ECS021
01011              20  CMMN-CNC-CNT         PIC S9(7).                  ECS021
01012              20  CMMN-ISS-PREM        PIC S9(11)V99.              ECS021
01013              20  CMMN-CNC-PREM        PIC S9(11)V99.              ECS021
01014              20  CMMN-NET-COMPEN      PIC S9(11)V99.              ECS021
01015              20  CMMN-CLM-CNT         PIC S9(7).                  ECS021
01016              20  CMMN-CLM-AMT         PIC S9(11)V99.              ECS021
01017              20  CMMN-LOSS-RESV       PIC S9(11)V99.              ECS021
01018              20  CMMN-EARND-PREM      PIC S9(9)V99.               ECS021
01019              20  CMMN-PRM-INFRC       PIC S9(9)V99.               ECS021
01020              20  CMMN-INFRC-CNT       PIC S9(9).                  ECS021
01021              20  CMMN-AVG-AGE         PIC S9(9).                  ECS021
01022              20  CMMN-AVG-ORG-TRM     PIC S9(9).                  ECS021
01023              20  CMMN-WGHT-AGE        PIC S9(9).                  ECS021
01024              20  CMMN-WGHT-ORG-TRM    PIC S9(9).                  ECS021
01025              20  CMMN-EXP-PCT         PIC S999V9(4).              ECS021
01026              20  CMMN-ADDED-TO-CNT    PIC S9(5).                  ECS021
102908             20  CMMN-ACCT-COMM       PIC S9(11)V99.
102908             20  CMMN-OW-COMM         PIC S9(11)V99.
01027                                                                   ECS021
092602*01  COMMON-EXTENSION-1.                                          ECS021
092602*    12  TABLE-EXTENSION-1     OCCURS   75 TIMES.                 ECS021
01030 *        16  FILLER                   PIC X.                      ECS021
01031 *        16  FILLER                   PIC 99.                     ECS021
01032 *        16  TBL-EXTENSION-1  OCCURS 15  TIMES  COMP-3.           ECS021
01033 *            20  FILLER               PIC S9(7).                  ECS021
01034 *            20  FILLER               PIC S9(7).                  ECS021
01035 *            20  FILLER               PIC S9(11)V99.              ECS021
01036 *            20  FILLER               PIC S9(11)V99.              ECS021
01037 *            20  FILLER               PIC S9(11)V99.              ECS021
01038 *            20  FILLER               PIC S9(7).                  ECS021
01039 *            20  FILLER               PIC S9(11)V99.              ECS021
01040 *            20  FILLER               PIC S9(11)V99.              ECS021
01041 *            20  FILLER               PIC S9(9)V99.               ECS021
01042 *            20  FILLER               PIC S9(9)V99.               ECS021
01043 *            20  FILLER               PIC S9(9).                  ECS021
01044 *            20  FILLER               PIC S9(9).                  ECS021
01045 *            20  FILLER               PIC S9(9).                  ECS021
01046 *            20  FILLER               PIC S9(9).                  ECS021
01047 *            20  FILLER               PIC S9(9).                  ECS021
01048 *            20  FILLER               PIC S999V9(4).              ECS021
01049 *            20  FILLER               PIC S9(5).                  ECS021
01050 *                                                                 ECS021
01051 *01  COMMON-EXTENSION-2.                                          ECS021
01052 *    12  TABLE-EXTENSION-2     OCCURS   75 TIMES.                 ECS021
01053 *        16  FILLER                   PIC X.                      ECS021
01054 *        16  FILLER                   PIC 99.                     ECS021
01055 *        16  TBL-EXTENSION-2  OCCURS 15  TIMES  COMP-3.           ECS021
01056 *            20  FILLER               PIC S9(7).                  ECS021
01057 *            20  FILLER               PIC S9(7).                  ECS021
01058 *            20  FILLER               PIC S9(11)V99.              ECS021
01059 *            20  FILLER               PIC S9(11)V99.              ECS021
01060 *            20  FILLER               PIC S9(11)V99.              ECS021
01061 *            20  FILLER               PIC S9(7).                  ECS021
01062 *            20  FILLER               PIC S9(11)V99.              ECS021
01063 *            20  FILLER               PIC S9(11)V99.              ECS021
01064 *            20  FILLER               PIC S9(9)V99.               ECS021
01065 *            20  FILLER               PIC S9(9)V99.               ECS021
01066 *            20  FILLER               PIC S9(9).                  ECS021
01067 *            20  FILLER               PIC S9(9).                  ECS021
01068 *            20  FILLER               PIC S9(9).                  ECS021
01069 *            20  FILLER               PIC S9(9).                  ECS021
01070 *            20  FILLER               PIC S9(9).                  ECS021
01071 *            20  FILLER               PIC S999V9(4).              ECS021
01072 *            20  FILLER               PIC S9(5).                  ECS021
01073 *                                                                 ECS021
01074 *01  COMMON-EXTENSION-3.                                          ECS021
01075 *    12  TABLE-EXTENSION-3     OCCURS   75 TIMES.                 ECS021
01076 *        16  FILLER                   PIC X.                      ECS021
01077 *        16  FILLER                   PIC 99.                     ECS021
01078 *        16  TBL-EXTENSION-3  OCCURS 15  TIMES  COMP-3.           ECS021
01079 *            20  FILLER               PIC S9(7).                  ECS021
01080 *            20  FILLER               PIC S9(7).                  ECS021
01081 *            20  FILLER               PIC S9(11)V99.              ECS021
01082 *            20  FILLER               PIC S9(11)V99.              ECS021
01083 *            20  FILLER               PIC S9(11)V99.              ECS021
01084 *            20  FILLER               PIC S9(7).                  ECS021
01085 *            20  FILLER               PIC S9(11)V99.              ECS021
01086 *            20  FILLER               PIC S9(11)V99.              ECS021
01087 *            20  FILLER               PIC S9(9)V99.               ECS021
01088 *            20  FILLER               PIC S9(9)V99.               ECS021
01089 *            20  FILLER               PIC S9(9).                  ECS021
01090 *            20  FILLER               PIC S9(9).                  ECS021
01091 *            20  FILLER               PIC S9(9).                  ECS021
01092 *            20  FILLER               PIC S9(9).                  ECS021
01093 *            20  FILLER               PIC S9(9).                  ECS021
01094 *            20  FILLER               PIC S999V9(4).              ECS021
092602*            20  FILLER               PIC S9(5).                  ECS021
01096                                                                   ECS021
01097      EJECT                                                        ECS021
01098  01  COMMON-TOTAL-IDENTIFIER.                                     ECS021
01099      12  FILLER                   PIC X(28)                       ECS021
01100                        VALUE '***START OF COMMON TOTAL ***'.      ECS021
01101  01  COMMON-TOTAL-TABLE.                                          ECS021
01102      12  COMMON-TOTALS          OCCURS  3 TIMES.                  ECS021
01103          16  CMNT-PERIOD    OCCURS   15  TIMES  COMP-3.           ECS021
01104              20  CMNT-ISS-CNT         PIC S9(7).                  ECS021
01105              20  CMNT-CNC-CNT         PIC S9(7).                  ECS021
01106              20  CMNT-SINGLE-ELEM     PIC S9(7).                  ECS021
01107              20  CMNT-JOINT-RETRO     PIC S9(7).                  ECS021
01108              20  CMNT-LIFE-LEVEL      PIC S9(7).                  ECS021
01109              20  CMNT-ISS-PREM        PIC S9(11)V99.              ECS021
01110              20  CMNT-CNC-PREM        PIC S9(11)V99.              ECS021
01111              20  CMNT-NET-COMPEN      PIC S9(11)V99.              ECS021
01112              20  CMNT-LF-CLM-CNT      PIC S9(7).                  ECS021
01113              20  CMNT-LF-CLM-AMT      PIC S9(11)V99.              ECS021
01114              20  CMNT-AH-CLM-CNT      PIC S9(7).                  ECS021
01115              20  CMNT-AH-CLM-AMT      PIC S9(11)V99.              ECS021
01116              20  CMNT-LOSS-RESV       PIC S9(11)V99.              ECS021
01117              20  CMNT-EARND-PREM      PIC S9(9)V99.               ECS021
01118              20  CMNT-PRM-INFRC       PIC S9(9)V99.               ECS021
01119              20  CMNT-INFRC-CNT       PIC S9(9).                  ECS021
01120              20  CMNT-AVG-AGE         PIC S9(9).                  ECS021
01121              20  CMNT-AVG-ORG-TRM     PIC S9(9).                  ECS021
01122              20  CMNT-WGHT-AGE        PIC S9(9).                  ECS021
01123              20  CMNT-WGHT-ORG-TRM    PIC S9(9).                  ECS021
01124              20  CMNT-EXP-PCT         PIC S999V9(4).              ECS021
01125              20  CMNT-ADDED-TO-CNT    PIC S9(5).                  ECS021
102908             20  CMNT-ACCT-COMM       PIC S9(11)V99.
102908             20  CMNT-OW-COMM         PIC S9(11)V99.
01126                                                                   ECS021
01127      EJECT                                                        ECS021
01128  01  ZERO-ENTRY-IDENTIFIER.                                       ECS021
01129      12  FILLER                   PIC X(28)                       ECS021
01130                            VALUE '****START OF ZERO ENTRY ****'.  ECS021
01131  01  ZERO-TABLE-ENTRIES.                                          ECS021
01132      12  ZERO-ACCUMS              COMP-3.                         ECS021
01133          16  FILLER               PIC S9(7)       VALUE +0.       ECS021
01134          16  FILLER               PIC S9(7)       VALUE +0.       ECS021
01135          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021
01136          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021
01137          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021
01138          16  FILLER               PIC S9(7)       VALUE +0.       ECS021
01139          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021
01140          16  FILLER               PIC S9(11)V99   VALUE +0.       ECS021
01141          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021
01142          16  FILLER               PIC S9(9)V99    VALUE +0.       ECS021
01143          16  FILLER               PIC S9(9)       VALUE +0.       ECS021
01144          16  FILLER               PIC S9(9)       VALUE +0.       ECS021
01145          16  FILLER               PIC S9(9)       VALUE +0.       ECS021
01146          16  FILLER               PIC S9(9)       VALUE +0.       ECS021
01147          16  FILLER               PIC S9(9)       VALUE +0.       ECS021
01148          16  FILLER               PIC S999V9(4)   VALUE +0.       ECS021
01149          16  FILLER               PIC S9(5)       VALUE +0.       ECS021
102908         16  FILLER               PIC S9(11)V99   VALUE +0.
102908         16  FILLER               PIC S9(11)V99   VALUE +0.
01150                                                                   ECS021
01151      EJECT                                                        ECS021
01152 ***************************************************************   ECS021
01153 *  THE FOLLOWING TABLE VALUES WILL BE DETERMINED BY THE DATE  *   ECS021
01154 *  READ IN FROM EL300 DISK DATE.  THIS TABLE DETERMINES THE   *   ECS021
01155 *  PERIOD ASSOCIATED WITH THE 15 OCCURRENCES UNDER THE 300    *   ECS021
01156 *  OCCURRENCES IN EACH BREAK TABLE. EXAMPLE: RUN DATE 06/30/86*   ECS021
01157 *                                                             *   ECS021
01158 *  LEVEL 1 REPRESENTS 06/84     LEVEL 9  REPRESENTS 12/85     *   ECS021
01159 *  LEVEL 2 REPRESENTS 12/84     LEVEL 10 REPRESENTS 01/86     *   ECS021
01160 *  LEVEL 3 REPRESENTS 06/85     LEVEL 11 REPRESENTS 02/86     *   ECS021
01161 *  LEVEL 4 REPRESENTS 07/85     LEVEL 12 REPRESENTS 03/86     *   ECS021
01162 *  LEVEL 5 REPRESENTS 08/85     LEVEL 13 REPRESENTS 04/86     *   ECS021
01163 *  LEVEL 6 REPRESENTS 09/85     LEVEL 14 REPRESENTS 05/86     *   ECS021
01164 *  LEVEL 7 REPRESENTS 10/85     LEVEL 15 REPRESENTS 06/86     *   ECS021
01165 *  LEVEL 8 REPRESENTS 11/85                                   *   ECS021
01166 *                                                             *   ECS021
01167 ***************************************************************   ECS021
01168                                                                   ECS021
01169  01  DATE-ASSIGNMENT-TABLE.                                       ECS021
01170      12  DATE-LVL1.                                               ECS021
01171          16  LVL1-CC              PIC 99          VALUE ZEROS.    ECS021
01172          16  LVL1-YY              PIC 99          VALUE ZEROS.    ECS021
01173          16  LVL1-MM              PIC 99          VALUE ZEROS.    ECS021
01174      12  DATE-LVL2.                                               ECS021
01175          16  LVL2-CC              PIC 99          VALUE ZEROS.    ECS021
01176          16  LVL2-YY              PIC 99          VALUE ZEROS.    ECS021
01177          16  LVL2-MM              PIC 99          VALUE ZEROS.    ECS021
01178      12  DATE-LVL3.                                               ECS021
01179          16  LVL3-CC              PIC 99          VALUE ZEROS.    ECS021
01180          16  LVL3-YY              PIC 99          VALUE ZEROS.    ECS021
01181          16  LVL3-MM              PIC 99          VALUE ZEROS.    ECS021
01182      12  DATE-LVL4.                                               ECS021
01183          16  LVL4-CC              PIC 99          VALUE ZEROS.    ECS021
01184          16  LVL4-YY              PIC 99          VALUE ZEROS.    ECS021
01185          16  LVL4-MM              PIC 99          VALUE ZEROS.    ECS021
01186      12  DATE-LVL5.                                               ECS021
01187          16  LVL5-CC              PIC 99          VALUE ZEROS.    ECS021
01188          16  LVL5-YY              PIC 99          VALUE ZEROS.    ECS021
01189          16  LVL5-MM              PIC 99          VALUE ZEROS.    ECS021
01190      12  DATE-LVL6.                                               ECS021
01191          16  LVL6-CC              PIC 99          VALUE ZEROS.    ECS021
01192          16  LVL6-YY              PIC 99          VALUE ZEROS.    ECS021
01193          16  LVL6-MM              PIC 99          VALUE ZEROS.    ECS021
01194      12  DATE-LVL7.                                               ECS021
01195          16  LVL7-CC              PIC 99          VALUE ZEROS.    ECS021
01196          16  LVL7-YY              PIC 99          VALUE ZEROS.    ECS021
01197          16  LVL7-MM              PIC 99          VALUE ZEROS.    ECS021
01198      12  DATE-LVL8.                                               ECS021
01199          16  LVL8-CC              PIC 99          VALUE ZEROS.    ECS021
01200          16  LVL8-YY              PIC 99          VALUE ZEROS.    ECS021
01201          16  LVL8-MM              PIC 99          VALUE ZEROS.    ECS021
01202      12  DATE-LVL9.                                               ECS021
01203          16  LVL9-CC              PIC 99          VALUE ZEROS.    ECS021
01204          16  LVL9-YY              PIC 99          VALUE ZEROS.    ECS021
01205          16  LVL9-MM              PIC 99          VALUE ZEROS.    ECS021
01206      12  DATE-LVL10.                                              ECS021
01207          16  LVL10-CC             PIC 99          VALUE ZEROS.    ECS021
01208          16  LVL10-YY             PIC 99          VALUE ZEROS.    ECS021
01209          16  LVL10-MM             PIC 99          VALUE ZEROS.    ECS021
01210      12  DATE-LVL11.                                              ECS021
01211          16  LVL11-YY             PIC 99          VALUE ZEROS.    ECS021
01212          16  LVL11-YY             PIC 99          VALUE ZEROS.    ECS021
01213          16  LVL11-MM             PIC 99          VALUE ZEROS.    ECS021
01214      12  DATE-LVL12.                                              ECS021
01215          16  LVL12-YY             PIC 99          VALUE ZEROS.    ECS021
01216          16  LVL12-YY             PIC 99          VALUE ZEROS.    ECS021
01217          16  LVL12-MM             PIC 99          VALUE ZEROS.    ECS021
01218      12  DATE-LVL13.                                              ECS021
01219          16  LVL13-YY             PIC 99          VALUE ZEROS.    ECS021
01220          16  LVL13-YY             PIC 99          VALUE ZEROS.    ECS021
01221          16  LVL13-MM             PIC 99          VALUE ZEROS.    ECS021
01222      12  DATE-LVL14.                                              ECS021
01223          16  LVL14-CC             PIC 99          VALUE ZEROS.       CL**7
01224          16  LVL14-YY             PIC 99          VALUE ZEROS.    ECS021
01225          16  LVL14-MM             PIC 99          VALUE ZEROS.    ECS021
01226      12  DATE-LVL15.                                              ECS021
01227          16  LVL15-CC             PIC 99          VALUE ZEROS.       CL**7
01228          16  LVL15-YY             PIC 99          VALUE ZEROS.    ECS021
01229          16  LVL15-MM             PIC 99          VALUE ZEROS.    ECS021
01230                                                                   ECS021
01231  01  DATE-BREAK-TABLE    REDEFINES   DATE-ASSIGNMENT-TABLE.       ECS021
01232      12  BRK-YY-MM     OCCURS      15 TIMES.                      ECS021
01233          16  BRK-CCYY             PIC 9(04).                      ECS021
01234          16  BRK-CCYR  REDEFINES BRK-CCYY.                        ECS021
01235              20  BRK-CC           PIC 99.                         ECS021
01236              20  BRK-YY           PIC 99.                         ECS021
01237          16  BRK-MM               PIC 99.                         ECS021
01238                                                                   ECS021
01239      EJECT                                                        ECS021
01240 *******************************************************           ECS021
01241 *  THE FOLLOWING STATE EXPENSE TABLE WILL BE USED IN  *           ECS021
01242 *  COMPUTING FIGURES WHICH REQUIRE A EXPENSE  PERCENT.*           ECS021
01243 *        14% IS STORED AS FOLLOWS: (014.0000)         *           ECS021
01244 *******************************************************           ECS021
01245                                                                   ECS021
01246  01  STATE-EXPENSE-PERCENT-TABLE.                                 ECS021
01247      05  ST-MAX-IDX               PIC S999 COMP VALUE +0.         ECS021
01248      05  STATE-OCCURRENCES OCCURS  99 TIMES.                      ECS021
01249          10  STATE-CODE           PIC XX.                         ECS021
01250          10  STATE-ABBREVIATION   PIC XX.                         ECS021
01251          10  STATE-NAME           PIC X(25).                      ECS021
01252          10  STATE-LF-EXP-PCT     PIC S999V9(4) COMP-3.           ECS021
01253          10  STATE-AH-EXP-PCT     PIC S999V9(4) COMP-3.           ECS021
01254                                                                   ECS021
01255  01  BEG-DATE.                                                    ECS021
01256      12  BD-YR           PIC 99.                                  ECS021
01257      12  BD-MO           PIC 99.                                  ECS021
01258      12  BD-DA           PIC 99.                                  ECS021
01259                                                                   ECS021
01260  01  PRV-MONTH.                                                   ECS021
01261      12  PM-YR           PIC 99.                                  ECS021
01262      12  PM-MO           PIC 99.                                  ECS021
01263      12  PM-DA           PIC 99.                                  ECS021
01264                                                                   ECS021
01265  01  PRV-QTR.                                                     ECS021
01266      12  PQ-YR           PIC 99.                                  ECS021
01267      12  PQ-MO           PIC 99.                                  ECS021
01268      12  PQ-DA           PIC 99.                                  ECS021
01269                                                                   ECS021
01270  01  RUN-DT.                                                      ECS021
01271      12  RD-YR           PIC 99.                                  ECS021
01272      12  RD-MO           PIC 99.                                  ECS021
01273      12  RD-DA           PIC 99.                                  ECS021
01274                                                                   ECS021
01275  01  EPEC-YY-MM.                                                  ECS021
01276      12  EPEC-CC         PIC 99.                                  ECS021
01277      12  EPEC-YY         PIC 99.                                  ECS021
01278      12  EPEC-MM         PIC 99.                                  ECS021
01279                                                                   ECS021
01280 ***************************************************************** ECS021
01281 *  RR-ACCTMSTR-CNTRL IS THE KEY USED TO READ THE ACCOUNT MASTER * ECS021
01282 *  RECORD ASSOCIATED WITH THE RR-REC TO BE RELEASED TO SORT.    * ECS021
01283 *  RR-REC IS UPDATED WITH VARIOUS FIELDS PROVIDED.              * ECS021
01284 ***************************************************************** ECS021
01285                                                                   ECS021
01286  01  RR-ACCTMSTR-CNTRL.                                           ECS021
01287      12  RR-ACCTMSTR-CARR            PIC X.                       ECS021
01288      12  RR-ACCTMSTR-GROUP           PIC X(6).                    ECS021
01289      12  RR-ACCTMSTR-STATE           PIC XX.                      ECS021
01290      12  RR-ACCTMSTR-ACCOUNT         PIC X(10).                   ECS021
01291      12  RR-ACCTMSTR-EXP-DT          PIC X(6).                    ECS021
01292      EJECT                                                        ECS021
010809
010809 01  RR-ACCTMSTR-CNTRL-EFFDT.
010809     12  RR-ACCTMSTR-CNTRL-1-EFFDT   PIC X(19).
010809     12  RR-ACCTMSTR-EFF-DATE-EFFDT  PIC 9(11) COMP-3.
010809 01  WS-AM-MSTR-CNTRL-EFFDT.
010809     12  WS-AM-MSTR-CNTRL-1-EFFDT    PIC X(19).
010809     12  WS-AM-MSTR-EFF-DATE-EFFDT   PIC 9(11) COMP-3.
01293 ******************************************************************ECS021
01294 *  CONTROL-BREAKS IS USED IN THE INPUT PROCEDURE TO BUILD THE    *ECS021
01295 *  SORT KEY.                                                     *ECS021
01296 *  SW-REPORT-CONTROL-KEY IS THE CURRENT CONTROL BREAK RETURNED   *ECS021
01297 *  FROM  SORT.  IT IS COMPARED WITH SAVE-CONTROL-BREAK TO        *ECS021
01298 *  TRIGGER BREAKS IN THE REPORT PRODUCED IN THE OUTPUT PROCEDURE *ECS021
01299 ******************************************************************ECS021
01300                                                                   ECS021
01301  01  SAVE-CONTROL-BREAK.                                          ECS021
01302      12  SAVE-BREAK-1                    PIC X(10).               ECS021
01303      12  SAVE-BREAK-2                    PIC X(10).               ECS021
01304      12  SAVE-BREAK-3                    PIC X(10).               ECS021
01305      12  SAVE-BREAK-4                    PIC X(10).               ECS021
01306      12  SAVE-BREAK-5                    PIC X(10).               ECS021
01307      12  SAVE-BREAK-6                    PIC X(10).               ECS021
01308                                                                   ECS021
01309  01  CONTROL-BREAKS.                                              ECS021
01310      12  CONTROL-BREAK OCCURS  6  TIMES  PIC X(10).               ECS021
01311                                                                   ECS021
01312  01  HOLD-CONTROL-FILE                   PIC X(750).              ECS021
01313                                                                   ECS021
01314  01  HOLD-EXCEPTION-REC.                                          ECS021
01315      12  FILLER                             PIC X(022).           ECS021
01316                                                                   ECS021
01317      12  WS-EXCEPTION-RECORD-BODY.                                ECS021
01318          16  WS-ACCOUNTS-LT-ONE-YEAR        PIC X.                ECS021
01319              88  WS-EXCEPTION-ACCTS-WITHIN-ONE  VALUE 'Y'.        ECS021
01320                                                                   ECS021
01321          16  WS-COMBINED-LIFE-AH-OPT.                             ECS021
01322              20  WS-ISS-COUNT-DIFF          PIC S9(05)     COMP-3.ECS021
01323              20  WS-SINGLE-MO-PREM-PCT      PIC S9(02).           ECS021
01324              20  WS-EARN-PREM-DECR-PCT      PIC S9(02).           ECS021
01325              20  WS-CANCELLATION-RATIO      PIC S9(02).           ECS021
01326                                                                   ECS021
01327          16  WS-LIFE-OPT.                                         ECS021
01328              20  WS-LF-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.ECS021
01329              20  WS-LF-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.ECS021
01330              20  WS-LF-PERIOD-PROFIT        PIC S9(03)     COMP-3.ECS021
01331              20  WS-LF-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.ECS021
01332              20  WS-LF-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.ECS021
01333              20  WS-LF-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.ECS021
01334              20  WS-LF-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.ECS021
01335              20  WS-LF-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.ECS021
01336              20  WS-LF-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.ECS021
01337              20  WS-LF-AVG-AGE-MAX          PIC S9(02).           ECS021
01338                                                                   ECS021
01339          16  WS-AH-OPT.                                           ECS021
01340              20  WS-AH-LOSS-RATIO-PCT       PIC S9(03)     COMP-3.ECS021
01341              20  WS-AH-LTM-LOSS-RATIO       PIC S9(03)     COMP-3.ECS021
01342              20  WS-AH-PERIOD-PROFIT        PIC S9(03)     COMP-3.ECS021
01343              20  WS-AH-LTM-PROFIT-PCT       PIC S9(02)V9   COMP-3.ECS021
01344              20  WS-AH-LTM-INFORCE-DECR     PIC S9(02)V9   COMP-3.ECS021
01345              20  WS-AH-LTM-TERM-CHG         PIC S9(02)V9   COMP-3.ECS021
01346              20  WS-AH-TERM-AVG-WEIGHTED    PIC S9(02)V9   COMP-3.ECS021
01347              20  WS-AH-LTM-AGE-PCT          PIC S9(02)V9   COMP-3.ECS021
01348              20  WS-AH-AGE-AVG-WEIGHTED     PIC S9(02)V9   COMP-3.ECS021
01349              20  WS-AH-AVG-AGE-MAX          PIC S9(02).           ECS021
01350                                                                   ECS021
01351          16  WS-ACCT-ZERO-MONTH-PRODUCTION  PIC X.                ECS021
01352              88  WS-ACCT-CURRENT-MONTH-ACT   VALUE 'A'.           ECS021
01353              88  WS-ACCT-WITH-NO-PRODUCTION  VALUE 'B'.           ECS021
01354              88  WS-ACCT-WITH-ISSUE-ACTIVITY VALUE 'C'.           ECS021
01355                                                                   ECS021
01356          16  WS-RETENTION-LIMIT             PIC S9(7)      COMP-3.ECS021
01357                                                                   ECS021
01358          16  FILLER                         PIC X(673).           ECS021
01359                                                                   ECS021
102908  01  DATA-OUT-RECORD.
082908     05  DO-ME-DATE.
082908         10  DO-ME-MO         PIC 9(2).
082908         10  FILLER           PIC X(1)  VALUE '/'.
082908         10  DO-ME-DD         PIC 9(2).
082908         10  FILLER           PIC X(1)  VALUE '/'.
082908         10  DO-ME-YR         PIC 9(4).
082908     05  FILLER               PIC X(1)  VALUE ';'.
082908     05  DO-CARRIER           PIC X(1).
082908     05  FILLER               PIC X(1)  VALUE ';'.
082908     05  DO-STATE             PIC X(2).
082908     05  FILLER               PIC X(1)  VALUE ';'.
082908     05  DO-ACCOUNT-NUM       PIC X(10).
082908     05  FILLER               PIC X(1)  VALUE ';'.
082908     05  DO-ACCOUNT-NAME      PIC X(30).
082908     05  DO-ACCUMULATORS OCCURS 7 TIMES.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-DESCR         PIC X(28).
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-CURLF         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-YTDLF         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-L12LF         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-ITDLF         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-CURAH         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-YTDAH         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-L12AH         PIC -(11).99.
082908         10  FILLER           PIC X(1)  VALUE ';'.
082908         10  DO-ITDAH         PIC -(11).99.
01360                                                                   ECS021
092602                             COPY ELCDTECX.                       ECS021
01362                                                                   ECS021
01363                              COPY ELCDTEVR.                       ECS021
01364                                                                   ECS021
01365                              COPY ELCEPCVR.                       ECS021
01366                                                                   ECS021
01367                              COPY ELCACCTV.                       ECS021
01368                                                                   ECS021
101205 LINKAGE SECTION.

101205 01  PARM.
101205     05  PARM-LENGTH BINARY  PICTURE IS S9(4).
101205     05  PARM-VALUE  DISPLAY PICTURE IS X(100).
101205                                                                  
pemuni*LINKAGE SECTION.                                                 ECS021
pemuni*01  PARM-DATA.                                                   ECS021
pemuni*    05  PARM-LENGTH         PIC S9(4) COMP.                      ECS021
pemuni*    05  PARM-SWITCH         PIC X.                               ECS021
01373                                                                   ECS021
pemuni*PROCEDURE DIVISION USING PARM-DATA.
101205 PROCEDURE DIVISION USING PARM.
01376                                                                   ECS021
01377  0000-READ-DATE-CARD.                                             ECS021
01378                              COPY ELCDTERX.                       ECS021
01379                                                                   ECS021
01380      IF CLAS-MAXA EQUAL +0                                        ECS021
01381          MOVE CLAS-MAXL       TO CLAS-MAXA                        ECS021
01382                                  CLAS-STARTA.                     ECS021
01383                                                                   ECS021
01384 ******************************************************************ECS021
01385 *  LOGIC FOUND BETWEEN HEADINGS 0100-INITIALIZE-DATE-AREAS AND   *ECS021
01386 *  0230-INITIALIZE-TABLES IS EXECUTED ONE TIME AND ACCOMPLISHES  *   CL**8
01387 *  THE FOLLOWING:                                                *ECS021
01388 *                                                                *ECS021
01389 *    1) 0100-INITIALIZE-DATE-AREAS THRU 0115-OPEN-CONTROL-FILE   *ECS021
01390 *       BUILDS (DATE-ASSIGNMENT-TABLE) WHICH CONTROLS WHAT       *ECS021
01391 *       PERIODS WILL BE REPORTED UPON. THIS LOGIC USES RUN-DATE  *ECS021
01392 *       SUPPLIED BY THE STANDARD DATE CARD ROUTINE AS A BASE TO  *ECS021
01393 *       DERIVE ALL OTHER DATES IN TABLE. THIS IS ALSO WHERE THE  *ECS021
01394 *       CONTROL FILE IS OPENED.                                  *ECS021
01395 *                                                                *ECS021
01396 *       DATE TABLE EXAMPLE WITH A RUN-DATE OF 06/30/86           *ECS021
01397 *                                                                *ECS021
01398 *       LEVEL 1 REPRESENTS 06/84     LEVEL 9  REPRESENTS 12/85   *ECS021
01399 *       LEVEL 2 REPRESENTS 12/84     LEVEL 10 REPRESENTS 01/86   *ECS021
01400 *       LEVEL 3 REPRESENTS 06/85     LEVEL 11 REPRESENTS 02/86   *ECS021
01401 *       LEVEL 4 REPRESENTS 07/85     LEVEL 12 REPRESENTS 03/86   *ECS021
01402 *       LEVEL 5 REPRESENTS 08/85     LEVEL 13 REPRESENTS 04/86   *ECS021
01403 *       LEVEL 6 REPRESENTS 09/85     LEVEL 14 REPRESENTS 05/86   *ECS021
01404 *       LEVEL 7 REPRESENTS 10/85     LEVEL 15 REPRESENTS 06/86   *ECS021
01405 *       LEVEL 8 REPRESENTS 11/85                                 *ECS021
01406 *                                                                *ECS021
01407 *    2) 0120-READ-STATE-MASTER THRU 0125-READ-STATE-MASTER-LOOP  *ECS021
01408 *       READS EACH STATE RECORD FOUND ON THE CONTROL FILE AND    *ECS021
01409 *       TABLES A EXPENSE PERCENTAGE FOR LIFE AND A&H.  THIS      *ECS021
01410 *       PERCENTAGE IS APPLIED AGAINST A PERIODS PROFITS TO       *ECS021
01411 *       REDUCE NET INCOME FOR OVERHEAD CONSIDERATIONS.           *ECS021
01412 *                                                                *ECS021
01413 *    3) 0130-READ-REPORT-RECORD THRU 0185-CHECK-USER-CODE5       *ECS021
01414 *       READS THE REPORT CUSTOMIZATION RECORD GENERATED BY       *ECS021
01415 *       EL604A ONLINE APPLICATION.  THIS RECORD DETERMINES:      *ECS021
01416 *                                                                *ECS021
01417 *               1) NUMBER OF CONTROL BREAKS IN REPORT (6 MAX)    *ECS021
01418 *               2) SEQUENCE OF CONTROL BREAKS                    *ECS021
01419 *               3) DATA EXCLUSION CRITERIA FOR REPORT            *ECS021
01420 *                  I.E. ONLY ACCOUNT 1234567890                  *ECS021
01421 *                                                                *ECS021
01422 *    4) 0200-INITIALIZE-TABLES THRU 0230-INITIALIZE-TABLES       *ECS021
01423 *       ZEROS ALL OCCURRENCES OF ACCUMULATION TABLES USED        *ECS021
01424 *       IN PROGRAM.                                              *ECS021
01425 ******************************************************************ECS021
01426      EJECT                                                        ECS021
01427  0100-INITIALIZE-DATE-AREAS.                                      ECS021
01428      MOVE WS-CURRENT-DATE        TO HDG-2-DATE                    ECS021
01429                                     HDGE-2-DATE.                  ECS021
01430                                                                   ECS021
01431      MOVE ALPH-DATE              TO HDG-3-DATE                    ECS021
01432                                     HDGE-3-DATE.                  ECS021
01433                                                                   ECS021
01434      MOVE COMPANY-NAME           TO HDG-2-COMPANY                 ECS021
01435                                     HDGE-2-COMPANY.               ECS021
01436                                                                   ECS021
01437      IF DTE-FMT-OPT = '1' OR '2' OR '3' OR '4' OR '5'             ECS021
01438          NEXT SENTENCE                                            ECS021
01439      ELSE                                                         ECS021
01440          MOVE '1'                TO DTE-FMT-OPT.                  ECS021
01441                                                                   ECS021
101205     IF PARM-LENGTH > +0
              IF PARM-VALUE = 'SPECIAL'
                 MOVE 'SINGLE FEE EQUIVALENT'  TO
                      NET-WRT-DESC-SPE
                      EARNED-DESC-SPE
              END-IF
           END-IF

01442      IF DTE-PGM-OPT = 2                                           ECS021
01443          MOVE NET-WRT-DESC       TO HDG-7-PGM-OPT                 ECS021
01444      ELSE                                                         ECS021
01445          MOVE EARNED-DESC        TO HDG-7-PGM-OPT.                ECS021
01446                                                                   ECS021
01447      MOVE LIFE-OVERRIDE-L6       TO HDG-11-TYPE-LF.               ECS021
01448      MOVE AH-OVERRIDE-L6         TO HDG-11-TYPE-AH.               ECS021
01449                                                                   ECS021
01450      MOVE CLAS-REPORT-CD1-CAPTION                                 ECS021
01451                                  TO RPT-CDE1-DESC                 ECS021
01452      MOVE CLAS-REPORT-CD2-CAPTION                                 ECS021
01453                                  TO RPT-CDE2-DESC                 ECS021
01455      MOVE RUN-CC                 TO LVL15-CC.                        CL**5
01456      MOVE RUN-YR                 TO LVL15-YY.                        CL**5
01457      MOVE RUN-MO                 TO LVL15-MM.                     ECS021
102908
102908     MOVE RUN-CCYY               TO DO-ME-YR.
102908     MOVE RUN-MO                 TO DO-ME-MO.
102908     MOVE RUN-DA                 TO DO-ME-DD.
01458                                                                   ECS021
01459      PERFORM 0110-BUILD-DATE-TABLE THRU 0110-EXIT                 ECS021
01460          VARYING DTE-IDX FROM +15 BY -1                           ECS021
01461            UNTIL DTE-IDX LESS +4.                                 ECS021
01462                                                                   ECS021
01463      COMPUTE BRK-CCYY (1) =  BRK-CCYY (15) - 2.                   ECS021
01464                                                                   ECS021
01465      MOVE BRK-MM (15)            TO BRK-MM (1).                   ECS021
01466      MOVE BRK-CCYY (1)           TO BRK-CCYY (2).                 ECS021
01467      MOVE 12                     TO BRK-MM (2).                   ECS021
01468                                                                   ECS021
pemtst*    IF PARM-LENGTH GREATER THAN ZERO                             ECS021
01470 *        DISPLAY '*** LEVEL 1  DATE YYMM = ' DATE-LVL1            ECS021
01471 *        DISPLAY '*** LEVEL 2  DATE YYMM = ' DATE-LVL2            ECS021
01472 *        DISPLAY '*** LEVEL 3  DATE YYMM = ' DATE-LVL3            ECS021
01473 *        DISPLAY '*** LEVEL 4  DATE YYMM = ' DATE-LVL4            ECS021
01474 *        DISPLAY '*** LEVEL 5  DATE YYMM = ' DATE-LVL5            ECS021
01475 *        DISPLAY '*** LEVEL 6  DATE YYMM = ' DATE-LVL6            ECS021
01476 *        DISPLAY '*** LEVEL 7  DATE YYMM = ' DATE-LVL7            ECS021
01477 *        DISPLAY '*** LEVEL 8  DATE YYMM = ' DATE-LVL8            ECS021
01478 *        DISPLAY '*** LEVEL 9  DATE YYMM = ' DATE-LVL9            ECS021
01479 *        DISPLAY '*** LEVEL 10 DATE YYMM = ' DATE-LVL10           ECS021
01480 *        DISPLAY '*** LEVEL 11 DATE YYMM = ' DATE-LVL11           ECS021
01481 *        DISPLAY '*** LEVEL 12 DATE YYMM = ' DATE-LVL12           ECS021
01482 *        DISPLAY '*** LEVEL 13 DATE YYMM = ' DATE-LVL13           ECS021
01483 *        DISPLAY '*** LEVEL 14 DATE YYMM = ' DATE-LVL14           ECS021
01484 *        DISPLAY '*** LEVEL 15 DATE YYMM = ' DATE-LVL15.          ECS021
01485                                                                   ECS021
01486      GO TO 0115-OPEN-CONTROL-FILE.                                ECS021
01487                                                                   ECS021
01488  0110-BUILD-DATE-TABLE.                                           ECS021
01489      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
01490                                                                   ECS021
01491      MOVE BRK-CCYY (DTE-IDX)     TO BRK-CCYY (PREV-IDX).             CL**3
01492      MOVE BRK-MM (DTE-IDX)       TO BRK-MM (PREV-IDX).            ECS021
01493                                                                   ECS021
01494      COMPUTE BRK-MM (PREV-IDX) =                                  ECS021
01495              BRK-MM (PREV-IDX) - 1.                               ECS021
01496                                                                   ECS021
01497      IF BRK-MM (PREV-IDX) = ZEROS                                 ECS021
01498          MOVE 12                 TO BRK-MM (PREV-IDX)             ECS021
01499          COMPUTE BRK-CCYY (PREV-IDX) =                            ECS021
01500                  BRK-CCYY (PREV-IDX) - 1.                         ECS021
01501                                                                   ECS021
01502  0110-EXIT.                                                       ECS021
01503      EXIT.                                                        ECS021
01504                                                                   ECS021
01505      EJECT                                                        ECS021
01506 *****************************************************             ECS021
01507 *  READ CONTROL FILE (REPORT CUSTOMIZATION RECORD)  *             ECS021
01508 *  AND ESTABLISH BREAK CRITERIA, BREAK SEQUENCES,   *             ECS021
01509 *  AND DATA EXCLUSION CRITERIA.                     *             ECS021
01510 *****************************************************             ECS021
01511                                                                   ECS021
01512  0115-OPEN-CONTROL-FILE.                                          ECS021
01513      OPEN  INPUT ELCNTL.                                          ECS021
01514                                                                   ECS021
01515      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        ECS021
01516          NEXT SENTENCE                                            ECS021
01517        ELSE                                                       ECS021
01518          MOVE '**** BAD OPEN ON CNTL FILE ****'                   ECS021
01519                                  TO WS-ABEND-MESSAGE              ECS021
01520          MOVE '2'                TO WAC-1                         ECS021
01521          MOVE '1'                TO WAC-2                         ECS021
01522          MOVE ELCNTL-FILE-STATUS TO WAC-3-4                       ECS021
01523                                     WS-ABEND-FILE-STATUS          ECS021
01524          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01525          GO TO ABEND-PGM.                                         ECS021
01526                                                                   ECS021
01527      EJECT                                                        ECS021
01528 ************************************************                  ECS021
01529 *  READ CONTROL FILE STATE MASTER RECORDS AND  *                  ECS021
01530 *  TABALIZE EXPENSE PERCENTAGES TO BE APPLIED  *                  ECS021
01531 *  TO PERIOD PROFITS.                          *                  ECS021
01532 ************************************************                  ECS021
01533                                                                   ECS021
01534  0120-READ-STATE-MASTER.                                          ECS021
01535      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           ECS021
01536      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                ECS021
01537      MOVE '3'                    TO CF-RECORD-TYPE.               ECS021
01538      MOVE LOW-VALUES             TO CF-ACCESS-OF-STATE.           ECS021
01539      MOVE +0                     TO CF-SEQUENCE-NO.               ECS021
01540      MOVE +1                     TO ST-IDX.                       ECS021
01541                                                                   ECS021
01542      START ELCNTL KEY NOT LESS CF-CONTROL-PRIMARY.                ECS021
01543                                                                   ECS021
01544      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS021
01545          MOVE '**** STATE MASTER RECORDS NOT FOUND ****'          ECS021
01546                                  TO WS-ABEND-MESSAGE              ECS021
01547          MOVE '0'                TO WAC-1                         ECS021
01548          MOVE '1'                TO WAC-2                         ECS021
01549          MOVE ELCNTL-FILE-STATUS TO WAC-3-4                       ECS021
01550                                     WS-ABEND-FILE-STATUS          ECS021
01551          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01552          GO TO ABEND-PGM.                                         ECS021
01553                                                                   ECS021
01554  0125-READ-STATE-MASTER-LOOP.                                     ECS021
01555      READ ELCNTL NEXT RECORD                                      ECS021
01556           AT END                                                  ECS021
01557           GO TO 0130-READ-REPORT-RECORD.                          ECS021
01558                                                                   ECS021
01559      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS021
01560          MOVE '**** ERROR OCCURRED READING STATE MASTER ****'     ECS021
01561                                  TO WS-ABEND-MESSAGE              ECS021
01562          MOVE '0'                TO WAC-1                         ECS021
01563          MOVE '2'                TO WAC-2                         ECS021
01564          MOVE ELCNTL-FILE-STATUS TO WAC-3-4                       ECS021
01565                                     WS-ABEND-FILE-STATUS          ECS021
01566          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01567          GO TO ABEND-PGM.                                         ECS021
01568                                                                   ECS021
01569      IF CF-COMPANY-ID NOT = DTE-CLIENT                            ECS021
01570          GO TO 0130-READ-REPORT-RECORD.                           ECS021
01571                                                                   ECS021
01572      IF NOT CF-STATE-MASTER                                       ECS021
01573          GO TO 0130-READ-REPORT-RECORD.                           ECS021
01574                                                                   ECS021
01575      MOVE CF-STATE-CODE          TO STATE-CODE         (ST-IDX).  ECS021
01576      MOVE CF-STATE-ABBREVIATION  TO STATE-ABBREVIATION (ST-IDX).  ECS021
01577      MOVE CF-STATE-NAME          TO STATE-NAME         (ST-IDX).  ECS021
01578                                                                   ECS021
01579      IF CF-ST-LF-EXP-PCT NOT NUMERIC                              ECS021
01580          MOVE +0 TO CF-ST-LF-EXP-PCT.                             ECS021
01581                                                                   ECS021
01582      IF CF-ST-AH-EXP-PCT NOT NUMERIC                              ECS021
01583          MOVE +0 TO CF-ST-AH-EXP-PCT.                             ECS021
01584                                                                   ECS021
01585      COMPUTE STATE-LF-EXP-PCT (ST-IDX) ROUNDED =                  ECS021
01586              CF-ST-LF-EXP-PCT / +100.                             ECS021
01587      COMPUTE STATE-AH-EXP-PCT (ST-IDX) ROUNDED =                  ECS021
01588              CF-ST-AH-EXP-PCT / +100.                             ECS021
01589                                                                   ECS021
01590      MOVE ST-IDX                 TO ST-MAX-IDX.                   ECS021
01591      ADD +1 TO ST-IDX.                                            ECS021
01592                                                                   ECS021
01593      GO TO 0125-READ-STATE-MASTER-LOOP.                           ECS021
01594                                                                   ECS021
01595      EJECT                                                        ECS021
01596 ************************************************                  ECS021
01597 *  READ CONTROL FILE STATE MASTER RECORDS AND  *                  ECS021
01598 *  TABALIZE EXPENSE PERCENTAGE.                *                  ECS021
01599 ************************************************                  ECS021
01600                                                                   ECS021
01601  0130-READ-REPORT-RECORD.                                         ECS021
PEMTST
PEMTST     ACCEPT WS-REPORT-NO
PEMTST
PEMTST     IF WS-REPORT-NO NOT NUMERIC
PEMTST        MOVE '021'               TO WS-REPORT-NO
PEMTST     END-IF
PEMTST     EVALUATE WS-REPORT-NO
PEMTST        WHEN '001'
PEMTST          MOVE VAR-HEAD-1        TO HDR1-DESC
PEMTST          MOVE '-01'             TO HDR1-ID
PEMTST        WHEN '002'
PEMTST          MOVE VAR-HEAD-2        TO HDR1-DESC
PEMTST          MOVE '-02'             TO HDR1-ID
PEMTST        WHEN '003'
PEMTST          MOVE VAR-HEAD-3        TO HDR1-DESC
PEMTST          MOVE '-03'             TO HDR1-ID
PEMTST        WHEN '004'
PEMTST          MOVE VAR-HEAD-4        TO HDR1-DESC
PEMTST          MOVE '-04'             TO HDR1-ID
PEMTST        WHEN OTHER
PEMTST          MOVE VAR-HEAD-5        TO HDR1-DESC
PEMTST          MOVE '   '             TO HDR1-ID
PEMTST     END-EVALUATE
102908     IF WS-REPORT-NO = '307' AND DTE-FMT-OPT = 3
102908         MOVE 'RET'              TO HDR1-ID
102908     END-IF.
01602      MOVE SPACES                 TO CF-CONTROL-PRIMARY            ECS021
01603                                     BREAK-HEADINGS.               ECS021
01604      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                ECS021
01605      MOVE 'C'                    TO CF-RECORD-TYPE.               ECS021
PEMTST*    MOVE PGM-SUB                TO CF-CUSTOM-REPORT-NO.          ECS021
PEMTST     MOVE WS-REPORT-SEQ          TO CF-CUSTOM-REPORT-NO.          ECS021
01607      MOVE +0                     TO CF-SEQUENCE-NO.               ECS021
01608                                                                   ECS021
01609      READ ELCNTL.                                                 ECS021
01610                                                                   ECS021
022804     EVALUATE TRUE
022804     WHEN ELCNTL-FILE-STATUS = '00'
022804         CONTINUE

022804     WHEN ELCNTL-FILE-STATUS = '23'
022804         GO TO 9999-END-OF-JOB

022804     WHEN OTHER
01612          MOVE '**** REPORTS CUSTOMIZATION RCD NOT FOUND ****'     ECS021
01613                                  TO WS-ABEND-MESSAGE              ECS021
01614          MOVE '0'                TO WAC-1                         ECS021
01615          MOVE '0'                TO WAC-2                         ECS021
01616          MOVE ELCNTL-FILE-STATUS TO WAC-3-4                       ECS021
01617                                     WS-ABEND-FILE-STATUS          ECS021
01618          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01619          GO TO ABEND-PGM
022804     END-EVALUATE.
01620                                                                   ECS021
01621 ************************************************                  ECS021
01622 *  READ CONTROL FILE FOR THE EXCEPTION REPORT  *                  ECS021
01623 *  RECORD                                      *                  ECS021
01624 ************************************************                  ECS021
01625                                                                   ECS021
01626      MOVE SPACES                 TO HOLD-EXCEPTION-REC.           ECS021
01627                                                                   ECS021
01628      IF CF-EXCEPTION-LIST-IND EQUAL 'Y'                           ECS021
01629           NEXT SENTENCE                                           ECS021
01630      ELSE                                                         ECS021
01631           GO TO 0132-CONTINUE.                                    ECS021
01632                                                                   ECS021
01633      MOVE CONTROL-FILE           TO HOLD-CONTROL-FILE.            ECS021
01634                                                                   ECS021
01635      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           ECS021
01636      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                ECS021
01637      MOVE 'C'                    TO CF-RECORD-TYPE.               ECS021
01638      MOVE PGM-SUB                TO CF-CUSTOM-REPORT-NO.          ECS021
01639      MOVE +0002                  TO CF-SEQUENCE-NO.               ECS021
01640                                                                   ECS021
01641      READ ELCNTL.                                                 ECS021
01642                                                                   ECS021
01643      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS021
01644          MOVE '**** REPORTS EXCEPTION RCD NOT FOUND ****'         ECS021
01645                                  TO WS-ABEND-MESSAGE              ECS021
01646          MOVE '0'                TO WAC-1                         ECS021
01647          MOVE '0'                TO WAC-2                         ECS021
01648          MOVE ELCNTL-FILE-STATUS TO WAC-3-4                       ECS021
01649                                     WS-ABEND-FILE-STATUS          ECS021
01650          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01651          GO TO ABEND-PGM.                                         ECS021
01652                                                                   ECS021
01653                                                                   ECS021
01654      MOVE SPACES                 TO HDG-4-OPTION.                 ECS021
01655                                                                   ECS021
01656      IF CF-ACCT-CURRENT-MONTH-ACT                                 ECS021
01657          MOVE '    CURRENT MONTH ACTIVITY    '                    ECS021
01658                                  TO HDG-4-OPTION                  ECS021
01659      ELSE                                                         ECS021
01660         IF CF-ACCT-WITH-NO-PRODUCTION                             ECS021
01661              MOVE '  NO CURRENT MONTH ACTIVITY   '                ECS021
01662                                      TO HDG-4-OPTION              ECS021
01663         ELSE                                                      ECS021
01664             IF CF-ACCT-WITH-ISSUE-ACTIVITY                        ECS021
01665                  MOVE ' CURRENT MONTH - NEW BUSINESS '            ECS021
01666                                          TO HDG-4-OPTION.         ECS021
01667                                                                   ECS021
01668      MOVE CONTROL-FILE           TO HOLD-EXCEPTION-REC.           ECS021
01669                                                                   ECS021
01670      MOVE HOLD-CONTROL-FILE      TO CONTROL-FILE.                 ECS021
01671                                                                   ECS021
01672  0132-CONTINUE.                                                   ECS021
01673                                                                   ECS021
01674      IF CF-CARRIER-OPT-SEQ IS NOT NUMERIC                         ECS021
01675          MOVE 0                    TO  CF-CARRIER-OPT-SEQ.        ECS021
01676                                                                   ECS021
01677      IF CF-GROUP-OPT-SEQ IS NOT NUMERIC                           ECS021
01678          MOVE 0                    TO  CF-GROUP-OPT-SEQ.          ECS021
01679                                                                   ECS021
01680      IF CF-STATE-OPT-SEQ IS NOT NUMERIC                           ECS021
01681          MOVE 0                    TO  CF-STATE-OPT-SEQ.          ECS021
01682                                                                   ECS021
01683      IF CF-ACCOUNT-OPT-SEQ IS NOT NUMERIC                         ECS021
01684          MOVE 0                    TO  CF-ACCOUNT-OPT-SEQ.        ECS021
01685                                                                   ECS021
01686      IF CF-BUS-TYP-OPT-SEQ IS NOT NUMERIC                         ECS021
01687          MOVE 0                    TO  CF-BUS-TYP-OPT-SEQ.        ECS021
01688                                                                   ECS021
01689      IF CF-LF-TYP-OPT-SEQ IS NOT NUMERIC                          ECS021
01690          MOVE 0                    TO  CF-LF-TYP-OPT-SEQ.         ECS021
01691                                                                   ECS021
01692      IF CF-AH-TYP-OPT-SEQ IS NOT NUMERIC                          ECS021
01693          MOVE 0                    TO  CF-AH-TYP-OPT-SEQ.         ECS021
01694                                                                   ECS021
01695      IF CF-REPTCD1-OPT-SEQ IS NOT NUMERIC                         ECS021
01696          MOVE 0                    TO  CF-REPTCD1-OPT-SEQ.        ECS021
01697                                                                   ECS021
01698      IF CF-REPTCD2-OPT-SEQ IS NOT NUMERIC                         ECS021
01699          MOVE 0                    TO  CF-REPTCD2-OPT-SEQ.        ECS021
01700                                                                   ECS021
01701      IF CF-USER1-OPT-SEQ IS NOT NUMERIC                           ECS021
01702          MOVE 0                    TO  CF-USER1-OPT-SEQ.          ECS021
01703                                                                   ECS021
01704      IF CF-USER2-OPT-SEQ IS NOT NUMERIC                           ECS021
01705          MOVE 0                    TO  CF-USER2-OPT-SEQ.          ECS021
01706                                                                   ECS021
01707      IF CF-USER3-OPT-SEQ IS NOT NUMERIC                           ECS021
01708          MOVE 0                    TO  CF-USER3-OPT-SEQ.          ECS021
01709                                                                   ECS021
01710      IF CF-USER4-OPT-SEQ IS NOT NUMERIC                           ECS021
01711          MOVE 0                    TO  CF-USER4-OPT-SEQ.          ECS021
01712                                                                   ECS021
01713      IF CF-USER5-OPT-SEQ IS NOT NUMERIC                           ECS021
01714          MOVE 0                    TO  CF-USER5-OPT-SEQ.          ECS021
01715                                                                   ECS021
01716      IF CF-REINS-OPT-SEQ IS NOT NUMERIC                           ECS021
01717          MOVE 0                    TO  CF-REINS-OPT-SEQ.          ECS021
01718                                                                   ECS021
01719      IF CF-AGENT-OPT-SEQ IS NOT NUMERIC                           ECS021
01720          MOVE 0                    TO  CF-AGENT-OPT-SEQ.          ECS021
01721                                                                   ECS021
01722      IF CF-CARRIER-OPT-NOT-USED                                   ECS021
01723          GO TO 0135-CHECK-GROUP-CONTROL.                          ECS021
01724                                                                   ECS021
01725      MOVE CF-CARRIER-OPT-SEQ       TO BRK-IDX.                    ECS021
01726                                                                   ECS021
01727      IF CF-CARRIER-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS            ECS021
01728          MOVE CF-CARRIER-OPT-SEQ   TO WS-NUMBER-OF-BREAKS.        ECS021
01729                                                                   ECS021
01730      MOVE CARRIER-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01731                                                                   ECS021
01732  0135-CHECK-GROUP-CONTROL.                                        ECS021
01733      IF CF-GROUP-OPT-NOT-USED                                     ECS021
01734          GO TO 0140-CHECK-STATE-CONTROL.                          ECS021
01735                                                                   ECS021
01736      MOVE CF-GROUP-OPT-SEQ         TO BRK-IDX.                    ECS021
01737                                                                   ECS021
01738      IF CF-GROUP-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01739          MOVE CF-GROUP-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01740                                                                   ECS021
01741      MOVE GROUPING-DESC            TO BREAK-DESC (BRK-IDX).       ECS021
01742                                                                   ECS021
01743  0140-CHECK-STATE-CONTROL.                                        ECS021
01744      IF CF-STATE-OPT-NOT-USED                                     ECS021
01745          GO TO 0145-CHECK-ACCOUNT-CONTROL.                        ECS021
01746                                                                   ECS021
01747      MOVE CF-STATE-OPT-SEQ         TO BRK-IDX                     ECS021
01748                                       WS-STATE-BREAK-POSITION.    ECS021
01749                                                                   ECS021
01750      IF CF-STATE-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01751          MOVE CF-STATE-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01752                                                                   ECS021
01753      MOVE STATE-DESC               TO BREAK-DESC (BRK-IDX).       ECS021
01754                                                                   ECS021
01755  0145-CHECK-ACCOUNT-CONTROL.                                      ECS021
01756      IF CF-ACCOUNT-OPT-NOT-USED                                   ECS021
01757          GO TO 0147-CHECK-AGENT-CONTROL.                          ECS021
01758                                                                   ECS021
01759      MOVE CF-ACCOUNT-OPT-SEQ       TO BRK-IDX                     ECS021
01760                                       WS-ACCT-BREAK-POSITION.     ECS021
01761                                                                   ECS021
01762      IF CF-ACCOUNT-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS            ECS021
01763          MOVE CF-ACCOUNT-OPT-SEQ   TO WS-NUMBER-OF-BREAKS.        ECS021
01764                                                                   ECS021
01765      MOVE ACCOUNT-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01766                                                                   ECS021
01767  0147-CHECK-AGENT-CONTROL.                                        ECS021
01768      IF CF-AGENT-OPT-NOT-USED                                     ECS021
01769          GO TO 0150-CHECK-BUS-TYP-CONTROL.                        ECS021
01770                                                                   ECS021
01771      MOVE CF-AGENT-OPT-SEQ         TO BRK-IDX                     ECS021
01772                                       WS-GA-BREAK-POSITION.       ECS021
01773                                                                   ECS021
01774      IF CF-AGENT-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01775          MOVE CF-AGENT-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01776                                                                   ECS021
01777      MOVE AGENT-DESC               TO BREAK-DESC (BRK-IDX).       ECS021
01778                                                                   ECS021
01779  0150-CHECK-BUS-TYP-CONTROL.                                      ECS021
01780      IF CF-BUS-TYP-OPT-NOT-USED                                   ECS021
01781          GO TO 0155-CHECK-REPORT-CODE1.                           ECS021
01782                                                                   ECS021
01783      MOVE CF-BUS-TYP-OPT-SEQ       TO BRK-IDX.                    ECS021
01784                                                                   ECS021
01785      IF CF-BUS-TYP-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS            ECS021
01786          MOVE CF-BUS-TYP-OPT-SEQ   TO WS-NUMBER-OF-BREAKS.        ECS021
01787                                                                   ECS021
01788      MOVE BUSINESS-DESC            TO BREAK-DESC (BRK-IDX).       ECS021
01789                                                                   ECS021
01790  0155-CHECK-REPORT-CODE1.                                         ECS021
01791      IF CF-REPTCD1-OPT-NOT-USED                                   ECS021
01792          GO TO 0160-CHECK-REPORT-CODE2.                           ECS021
01793                                                                   ECS021
01794      MOVE CF-REPTCD1-OPT-SEQ       TO BRK-IDX.                    ECS021
01795                                                                   ECS021
01796      IF CF-REPTCD1-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS            ECS021
01797          MOVE CF-REPTCD1-OPT-SEQ   TO WS-NUMBER-OF-BREAKS.        ECS021
01798                                                                   ECS021
01799      MOVE RPT-CDE1-DESCRIPT        TO BREAK-DESC (BRK-IDX).       ECS021
01800                                                                   ECS021
01801  0160-CHECK-REPORT-CODE2.                                         ECS021
01802      IF CF-REPTCD2-OPT-NOT-USED                                   ECS021
01803          GO TO 0165-CHECK-USER-CODE1.                             ECS021
01804                                                                   ECS021
01805      MOVE CF-REPTCD2-OPT-SEQ       TO BRK-IDX.                    ECS021
01806                                                                   ECS021
01807      IF CF-REPTCD2-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS            ECS021
01808          MOVE CF-REPTCD2-OPT-SEQ   TO WS-NUMBER-OF-BREAKS.        ECS021
01809                                                                   ECS021
043007     IF (WS-REPORT-SEQ > 199)
043007        AND (WS-REPORT-SEQ < 300)
043007        MOVE RPT-CDE3-DESCRIPT   TO BREAK-DESC (BRK-IDX)
043007     ELSE
043007        MOVE RPT-CDE2-DESCRIPT   TO BREAK-DESC (BRK-IDX)
043007     END-IF

           .
01812  0165-CHECK-USER-CODE1.                                           ECS021
01813      IF CF-USER1-OPT-NOT-USED                                     ECS021
01814          GO TO 0170-CHECK-USER-CODE2.                             ECS021
01815                                                                   ECS021
01816      MOVE CF-USER1-OPT-SEQ         TO BRK-IDX.                    ECS021
01817                                                                   ECS021
01818      IF CF-USER1-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01819          MOVE CF-USER1-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01820                                                                   ECS021
01821      MOVE SELECT1-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01822                                                                   ECS021
01823  0170-CHECK-USER-CODE2.                                           ECS021
01824      IF CF-USER2-OPT-NOT-USED                                     ECS021
01825          GO TO 0175-CHECK-USER-CODE3.                             ECS021
01826                                                                   ECS021
01827      MOVE CF-USER2-OPT-SEQ         TO BRK-IDX.                    ECS021
01828                                                                   ECS021
01829      IF CF-USER2-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01830          MOVE CF-USER2-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01831                                                                   ECS021
01832      MOVE SELECT2-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01833                                                                   ECS021
01834  0175-CHECK-USER-CODE3.                                           ECS021
01835      IF CF-USER3-OPT-NOT-USED                                     ECS021
01836          GO TO 0180-CHECK-USER-CODE4.                             ECS021
01837                                                                   ECS021
01838      MOVE CF-USER3-OPT-SEQ         TO BRK-IDX.                    ECS021
01839                                                                   ECS021
01840      IF CF-USER3-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01841          MOVE CF-USER3-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01842                                                                   ECS021
01843      MOVE SELECT3-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01844                                                                   ECS021
01845  0180-CHECK-USER-CODE4.                                           ECS021
01846      IF CF-USER4-OPT-NOT-USED                                     ECS021
01847          GO TO 0185-CHECK-USER-CODE5.                             ECS021
01848                                                                   ECS021
01849      MOVE CF-USER4-OPT-SEQ         TO BRK-IDX.                    ECS021
01850                                                                   ECS021
01851      IF CF-USER4-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01852          MOVE CF-USER4-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01853                                                                   ECS021
01854      MOVE SELECT4-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01855                                                                   ECS021
01856  0185-CHECK-USER-CODE5.                                           ECS021
01857      IF CF-USER5-OPT-NOT-USED                                     ECS021
01858          GO TO 0200-INITIALIZE-TABLES.                            ECS021
01859                                                                   ECS021
01860      MOVE CF-USER5-OPT-SEQ         TO BRK-IDX.                    ECS021
01861                                                                   ECS021
01862      IF CF-USER5-OPT-SEQ GREATER WS-NUMBER-OF-BREAKS              ECS021
01863          MOVE CF-USER5-OPT-SEQ     TO WS-NUMBER-OF-BREAKS.        ECS021
01864                                                                   ECS021
01865      MOVE SELECT5-DESC             TO BREAK-DESC (BRK-IDX).       ECS021
01866                                                                   ECS021
01867      EJECT                                                        ECS021
01868 ******************************************************            ECS021
01869 *  INITIALIZE ALL OCCURRENCES OF BENEFIT TABLES USED  *           ECS021
01870 ******************************************************            ECS021
01871                                                                   ECS021
01872  0200-INITIALIZE-TABLES.                                          ECS021
01873                                                                   ECS021
01874      MOVE 1                      TO NUMBER-OF-REQUESTS.              CL**8
01875      MOVE W-BUILD-ZERO-TABLE     TO PROCESSING-REQUEST (1).          CL**9
01876                                                                   ECS021
01877      PERFORM 7100-CALL-SUB-MODULE-ONE THRU 7100-EXIT.             ECS021
102908*01878      PERFORM 7200-CALL-SUB-MODULE-TWO THRU 7200-EXIT.             ECS021
102908*01879      PERFORM 7300-CALL-SUB-MODULE-THREE THRU 7300-EXIT.           ECS021
102908*01880      PERFORM 7400-CALL-SUB-MODULE-FOUR THRU 7400-EXIT.            ECS021
102908*01881      PERFORM 7500-CALL-SUB-MODULE-FIVE THRU 7500-EXIT.            ECS021
102908*01882      PERFORM 7600-CALL-SUB-MODULE-SIX THRU 7600-EXIT.             ECS021
01883                                                                   ECS021
01884      EJECT                                                        ECS021
01885  0500-SORT-CONTROL-STATEMENTS.                                    ECS021
01886      OPEN INPUT EPEC-FILE                                         ECS021
01887                 ACCT-MSTR ERACCT                                  ECS021
01888                 COMP-MSTR                                         ECS021
01889          OUTPUT PRINTER                                           ECS021
CIDMOD                DODDS                                             ECS021
CIDMOD                MWAUTO                                            ECS021
CIDMOD                MIDWEST                                           ECS021
01890                 EXCEP-PRINT.                                      ECS021
102908
102908     IF WS-REPORT-NO = '307' AND DTE-FMT-OPT = 3
102908         OPEN OUTPUT DATA-OUT
102908     END-IF.
01891                                                                   ECS021
01892      IF ERACCT-FILE-STATUS  = '00' OR '97'                        ECS021
01893          NEXT SENTENCE                                            ECS021
01894        ELSE                                                       ECS021
01895          MOVE '****OPEN ERROR ON ACCTMSTR FILE ****'              ECS021
01896                                  TO WS-ABEND-MESSAGE              ECS021
01897          MOVE '1'                TO WAC-1                         ECS021
01898          MOVE '1'                TO WAC-2                         ECS021
01899          MOVE ERACCT-FILE-STATUS TO WAC-3-4                       ECS021
01900                                     WS-ABEND-FILE-STATUS          ECS021
01901          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01902          GO TO ABEND-PGM.                                         ECS021
01903                                                                   ECS021
01892      IF ACCT-FILE-STATUS  = '00' OR '97'                          ECS021
01893          NEXT SENTENCE                                            ECS021
01894        ELSE                                                       ECS021
01895          MOVE '****OPEN ERROR ON ERACCT   FILE ****'              ECS021
01896                                  TO WS-ABEND-MESSAGE              ECS021
01897          MOVE '1'                TO WAC-1                         ECS021
01898          MOVE '1'                TO WAC-2                         ECS021
01899          MOVE ACCT-FILE-STATUS TO WAC-3-4                         ECS021
01900                                     WS-ABEND-FILE-STATUS          ECS021
01901          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01902          GO TO ABEND-PGM.                                         ECS021
01903                                                                   ECS021
01904      IF ERCOMP-FILE-STATUS  = '00' OR '97'                        ECS021
01905          NEXT SENTENCE                                            ECS021
01906        ELSE                                                       ECS021
01907          MOVE '****OPEN ERROR ON COMPMSTR FILE ****'              ECS021
01908                                  TO WS-ABEND-MESSAGE              ECS021
01909          MOVE '1'                TO WAC-1                         ECS021
01910          MOVE '1'                TO WAC-2                         ECS021
01911          MOVE ERCOMP-FILE-STATUS TO WAC-3-4                       ECS021
01912                                     WS-ABEND-FILE-STATUS          ECS021
01913          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01914          GO TO ABEND-PGM.                                         ECS021
01915                                                                   ECS021
01916  0510-SORT-DEFINITION.                                            ECS021
01917      SORT SORT-FILE                                               ECS021
01918              ON ASCENDING SW-REPORT-CONTROL-KEY                   ECS021
01919          INPUT PROCEDURE                                          ECS021
01920              1000-SELECT-EPEC-RECS THRU 1999-EXIT                 ECS021
01921          OUTPUT PROCEDURE                                         ECS021
01922              5000-PRINT-STATEMENT  THRU 5099-EXIT.                ECS021
01923                                                                   ECS021
01924      IF SORT-RETURN NOT = ZERO                                    ECS021
01925          MOVE '**** UNSUCCESSFUL SORT BAD RETURN CODE ****'       ECS021
01926                                  TO WS-ABEND-MESSAGE              ECS021
01927          MOVE SORT-RETURN        TO WS-ABEND-CODE                 ECS021
01928          MOVE '0'                TO WAC-1                         ECS021
01929          MOVE '1'                TO WAC-2                         ECS021
01930          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01931          GO TO ABEND-PGM.                                         ECS021
01932                                                                   ECS021
01933      GO TO 9999-END-OF-JOB.                                       ECS021
01934                                                                   ECS021
01935  EJECT                                                            ECS021
01936  1000-SELECT-EPEC-RECS SECTION.                                   ECS021
01937                                                                   ECS021
01938 ********************************************                      ECS021
01939 *  ONE TIME ROUTINE PERFORM PRIMING READS  *                      ECS021
01940 ********************************************                      ECS021
01941                                                                   ECS021
01942  1000-ONE-TIME-PRIMING-READ.                                      ECS021
01943                                                                   ECS021
01944      PERFORM 4000-READ-EPEC THRU 4000-EXIT.                       ECS021
01945                                                                   ECS021
01946      IF EP-CONTROL EQUAL HIGH-VALUES                              ECS021
01947          DISPLAY '**** EPECS READ      **** ' EPECS-READ-COUNT    ECS021
01948          DISPLAY '**** EPECS NON EP EC **** ' NON-EP-EC-DROP-CNT  ECS021
01949          DISPLAY '**** EPECS REIN DROP **** ' REIN-EPEC-DROP-CNT  ECS021
01950          DISPLAY '**** EPECS DATE DROP1**** ' EPEC-DATE-DROP-CNT1 ECS021
01951          DISPLAY '**** EPECS DATE DROP2**** ' EPEC-DATE-DROP-CNT2 ECS021
01952          DISPLAY '*** NO EPEC RECORDS SELECTED FOR PROCESSING ***'ECS021
01953          MOVE '*** NO EPEC RECORDS SELECTED FOR PROCESSING ***'   ECS021
01954                                  TO WS-ABEND-MESSAGE              ECS021
01955          MOVE +1000              TO WS-ABEND-CODE                 ECS021
01956          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
01957          GO TO ABEND-PGM.                                         ECS021
01958                                                                   ECS021
01959      MOVE EP-ACCOUNT             TO WS-PREV-EP-ACCOUNT.           ECS021
01960      MOVE EP-EXP-DTE             TO WS-PREV-EP-EXP-DTE.           ECS021
01961                                                                   ECS021
01962      PERFORM 4400-READ-ACCT THRU 4449-EXIT.                       ECS021
03143      MOVE EP-CONTROL             TO RR-ACCTMSTR-CNTRL
                                          WS-HOLD-CNTL.
010809     MOVE EP-CNTRL-1             TO RR-ACCTMSTR-CNTRL-EFFDT.
010809     MOVE EP-EFF-DTE             TO RR-ACCTMSTR-EFF-DATE-EFFDT.                                          
053107     MOVE EP-COMPANY-CD          TO ACCT-COMPANY-CD
PEMMOD     PERFORM 4450-GET-AM-NAME    THRU 4499-EXIT.
01963                                                                   ECS021
01964 ****************************************                          ECS021
01965 *  START OF SORT INPUT PROCEDURE LOOP  *                          ECS021
01966 ****************************************                          ECS021
01967                                                                   ECS021
01968  1000-CHECK-SELECTION-CRITERIA.                                   ECS021
01969                                                                   ECS021
01970      PERFORM 4300-MATCH-FILES THRU 4399-EXIT.                     ECS021

022808     IF AM-STATUS = '0' OR '1' OR '2' OR '3' OR '4' OR '5'
021916                        OR '6' OR '7' OR '8' OR '9'
01973         CONTINUE
01974      ELSE
01975         MOVE '0'                 TO AM-STATUS
01976         ADD +1                   TO BAD-ACCT-STATUS
           END-IF

01978      IF AM-STATUS EQUAL '0'                                       ECS021
01979          IF CF-ACTIVE-ACCOUNTS OR CF-ALL-ACCOUNTS                 ECS021
01980              GO TO 1001-CHECK-ACCOUNT-TERM.                       ECS021
01981                                                                   ECS021
022808     IF AM-STATUS EQUAL '1' OR '4' OR '5'
021916                        OR '6' OR '7' OR '8' OR '9'
01983          IF CF-INACTIVE-ACCOUNTS OR CF-ALL-ACCOUNTS               ECS021
01984              GO TO 1001-CHECK-ACCOUNT-TERM.                       ECS021
01985                                                                   ECS021
01986      IF AM-STATUS EQUAL '2'                                       ECS021
01987          IF CF-INACTIVE-ACCOUNTS OR CF-ALL-ACCOUNTS               ECS021
01988              GO TO 1001-CHECK-ACCOUNT-TERM.                       ECS021
01989                                                                   ECS021
102004     IF AM-STATUS = '3'
121307        IF CF-ALL-ACCOUNTS OR CF-CANCELLED-ACCOUNTS
102004           GO TO 1001-CHECK-ACCOUNT-TERM
102004        END-IF
102004     END-IF
01989                                                                   ECS021
01990      ADD +1                      TO ACCT-STATUS-DROP.             ECS021
01991                                                                   ECS021
01992 ***************************************************               ECS021
01993 *  DROP EPEC RECORD DUE ACCOUNT STATUS SELECTION  *               ECS021
01994 *  CRITERIA IN REPORT CUSTOMIZATION RECORD        *               ECS021
01995 ***************************************************               ECS021
01996                                                                   ECS021
01997      GO TO 1510-RETRIEVE-NEW-EPEC.                                ECS021
01998                                                                   ECS021
01999  1001-CHECK-ACCOUNT-TERM.                                         ECS021
02000                                                                   ECS021
02001      IF NOT CF-EXCEPTION-LIST-REQUESTED                           ECS021
02002          GO TO 1001-CHECK-CARRIER-CONTROL.                        ECS021
02003                                                                   ECS021
02004      IF NOT WS-EXCEPTION-ACCTS-WITHIN-ONE                         ECS021
02005          GO TO 1001-CHECK-CARRIER-CONTROL.                        ECS021
02006                                                                   ECS021
02007      MOVE RUN-MO TO WS-EXCEP-MO.                                  ECS021
02008      MOVE RUN-DA TO WS-EXCEP-DA.                                  ECS021
02009      COMPUTE WS-EXCEP-CCYY = RUN-CCYY - 1.                        ECS021
02010                                                                   ECS021
02011      IF WS-AM-EFFECT-DT GREATER THAN WS-EXCEPTION-DATE            ECS021
02012          GO TO 1001-CHECK-CARRIER-CONTROL.                        ECS021
02013                                                                   ECS021
02014      ADD +1                      TO ACCT-TERM-DROP.               ECS021
02015                                                                   ECS021
02016 ***************************************************               ECS021
02017 *  DROP EPEC RECORD DUE ACCOUNT TERM SELECTION    *               ECS021
02018 *  CRITERIA IN REPORT EXCEPTION RECORD            *               ECS021
02019 ***************************************************               ECS021
02020                                                                   ECS021
02021      GO TO 1510-RETRIEVE-NEW-EPEC.                                ECS021
02022                                                                   ECS021
02023 *********************************************                     ECS021
02024 *  INTERROGATE CURRENT EPEC RECORD AGAINST  *                     ECS021
02025 *  SELECTION CRITERIA PROVIDED BY CONTROL   *                     ECS021
02026 *  FILE (REPORT CUSTOMIZATION RECORD).      *                     ECS021
02027 *********************************************                     ECS021
02028                                                                   ECS021
02029  1001-CHECK-CARRIER-CONTROL.                                      ECS021
02030      IF CF-CARRIER-SELECT (1) = SPACES AND                        ECS021
02031         CF-CARRIER-SELECT (2) = SPACES AND                        ECS021
02032         CF-CARRIER-SELECT (3) = SPACES                            ECS021
02033             GO TO 1002-NO-CARRIER-SELECTION.                      ECS021
02034                                                                   ECS021
02035      IF EP-CARRIER   = CF-CARRIER-SELECT (1) OR                   ECS021
02036                        CF-CARRIER-SELECT (2) OR                   ECS021
02037                        CF-CARRIER-SELECT (3)                      ECS021
02038         GO TO 1002-NO-CARRIER-SELECTION                           ECS021
02039      ELSE                                                         ECS021
02040         ADD +1 TO CARRIER-DROP-CNT                                ECS021
02041         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02042                                                                   ECS021
02043  1002-NO-CARRIER-SELECTION.                                       ECS021
02044      IF CF-CARRIER-OPT-NOT-USED                                   ECS021
02045          GO TO 1003-CHECK-GROUP-CONTROL.                          ECS021
02046                                                                   ECS021
02047      MOVE CF-CARRIER-OPT-SEQ       TO BRK-IDX.                    ECS021
02048      MOVE EP-CARRIER               TO CONTROL-BREAK (BRK-IDX).    ECS021
02049                                                                   ECS021
02050  1003-CHECK-GROUP-CONTROL.                                        ECS021
02051      IF CF-GROUP-SELECT (1) = SPACES AND                          ECS021
02052         CF-GROUP-SELECT (2) = SPACES AND                          ECS021
02053         CF-GROUP-SELECT (3) = SPACES                              ECS021
02054             GO TO 1004-NO-GROUP-SELECTION.                        ECS021
02055                                                                   ECS021
02056      IF EP-GROUPING  = CF-GROUP-SELECT (1) OR                     ECS021
02057                        CF-GROUP-SELECT (2) OR                     ECS021
02058                        CF-GROUP-SELECT (3)                        ECS021
02059         GO TO 1004-NO-GROUP-SELECTION                             ECS021
02060      ELSE                                                         ECS021
02061         ADD +1 TO GROUP-DROP-CNT                                  ECS021
02062         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02063                                                                   ECS021
02064  1004-NO-GROUP-SELECTION.                                         ECS021
02065      IF CF-GROUP-OPT-NOT-USED                                     ECS021
02066          GO TO 1005-CHECK-STATE-CONTROL.                          ECS021
02067                                                                   ECS021
02068      MOVE CF-GROUP-OPT-SEQ         TO BRK-IDX.                    ECS021
02069      MOVE EP-GROUPING              TO CONTROL-BREAK (BRK-IDX).    ECS021
02070                                                                   ECS021
02071  1005-CHECK-STATE-CONTROL.                                        ECS021
02072      IF CF-STATE-SELECT (1)  = SPACES AND                         ECS021
02073         CF-STATE-SELECT (2)  = SPACES AND                         ECS021
02074         CF-STATE-SELECT (3)  = SPACES                             ECS021
02075             GO TO 1006-NO-STATE-SELECTION.                        ECS021
02076                                                                   ECS021
02077      IF EP-STATE = CF-STATE-SELECT (1) OR                         ECS021
02078                    CF-STATE-SELECT (2) OR                         ECS021
02079                    CF-STATE-SELECT (3)                            ECS021
02080         GO TO 1006-NO-STATE-SELECTION.                            ECS021
02081                                                                   ECS021
02082      IF STATE-ABBREVIATION (ST-IDX) = CF-STATE-SELECT (1) OR      ECS021
02083                                       CF-STATE-SELECT (2) OR      ECS021
02084                                       CF-STATE-SELECT (3)         ECS021
02085         GO TO 1006-NO-STATE-SELECTION.                            ECS021
02086                                                                   ECS021
02087      ADD +1 TO STATE-DROP-CNT.                                    ECS021
02088      GO TO 1510-RETRIEVE-NEW-EPEC.                                ECS021
02089                                                                   ECS021
02090  1006-NO-STATE-SELECTION.                                         ECS021
02091      IF CF-STATE-OPT-NOT-USED                                     ECS021
02092          GO TO 1007-CHECK-ACCOUNT-CONTROL.                        ECS021
02093                                                                   ECS021
02094      MOVE CF-STATE-OPT-SEQ         TO BRK-IDX.                    ECS021
02095      MOVE EP-STATE                 TO CONTROL-BREAK (BRK-IDX).    ECS021
02096                                                                   ECS021
02097  1007-CHECK-ACCOUNT-CONTROL.                                      ECS021
02098      IF CF-ACCOUNT-SELECT (1)  = SPACES AND                       ECS021
02099         CF-ACCOUNT-SELECT (2)  = SPACES AND                       ECS021
02100         CF-ACCOUNT-SELECT (3)  = SPACES                           ECS021
02101             GO TO 1008-NO-ACCOUNT-SELECTION.                      ECS021
02102                                                                   ECS021
02103      IF EP-ACCOUNT = CF-ACCOUNT-SELECT (1) OR                     ECS021
02104                      CF-ACCOUNT-SELECT (2) OR                     ECS021
02105                      CF-ACCOUNT-SELECT (3)                        ECS021
02106         GO TO 1008-NO-ACCOUNT-SELECTION                           ECS021
02107      ELSE                                                         ECS021
02108         ADD +1 TO ACCOUNT-DROP-CNT                                ECS021
02109         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02110                                                                   ECS021
02111  1008-NO-ACCOUNT-SELECTION.                                       ECS021
02112      IF CF-ACCOUNT-OPT-NOT-USED                                   ECS021
02113          GO TO 1009-CHECK-AGENT-CONTROL.                          ECS021
02114                                                                   ECS021
02115      MOVE CF-ACCOUNT-OPT-SEQ       TO BRK-IDX.                    ECS021
02116      MOVE EP-ACCOUNT               TO CONTROL-BREAK (BRK-IDX).    ECS021
02117                                                                   ECS021
02118  1009-CHECK-AGENT-CONTROL.                                        ECS021
02119      IF CF-AGENT-OPT-USED                                         ECS021
02120          PERFORM 4500-SEARCH-GENERAL-AGENT THRU 4599-EXIT         ECS021
02121      ELSE                                                         ECS021
02122          GO TO 1011-CHECK-BUS-TYP-CONTROL.                        ECS021
02123                                                                   ECS021
02124      MOVE CF-AGENT-OPT-SEQ         TO BRK-IDX.                    ECS021
02125                                                                   ECS021
02126      IF WS-ACCT-BREAK-SW IS EQUAL TO 'Y'                          ECS021
02127          MOVE SPACES               TO CONTROL-BREAK (BRK-IDX)     ECS021
02128      ELSE                                                         ECS021
02129          MOVE WS-1ST-AGENT-NUMBER  TO CONTROL-BREAK (BRK-IDX).    ECS021
02130                                                                   ECS021
02131  1011-CHECK-BUS-TYP-CONTROL.                                      ECS021
02132      IF (CF-BUS-TYP-SELECT (1)  = SPACES OR ZEROS) AND            ECS021
02133         (CF-BUS-TYP-SELECT (2)  = SPACES OR ZEROS) AND            ECS021
02134         (CF-BUS-TYP-SELECT (3)  = SPACES OR ZEROS)                ECS021
02135             GO TO 1012-NO-BUS-TYP-SELECTION.                      ECS021
02136                                                                   ECS021
02137      IF AM-GPCD = CF-BUS-TYP-SELECT (1) OR                        ECS021
02138                   CF-BUS-TYP-SELECT (2) OR                        ECS021
02139                   CF-BUS-TYP-SELECT (3)                           ECS021
02140         GO TO 1012-NO-BUS-TYP-SELECTION                           ECS021
02141      ELSE                                                         ECS021
02142         ADD +1 TO BUS-TYP-DROP-CNT                                ECS021
02143         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02144                                                                   ECS021
02145  1012-NO-BUS-TYP-SELECTION.                                       ECS021
02146      IF CF-BUS-TYP-OPT-NOT-USED                                   ECS021
02147          GO TO 1013-CHECK-LIFE-BENEFIT.                           ECS021
02148                                                                   ECS021
02149      MOVE CF-BUS-TYP-OPT-SEQ       TO BRK-IDX.                    ECS021
02150      MOVE AM-GPCD                  TO CONTROL-BREAK (BRK-IDX).    ECS021
02151                                                                   ECS021
02152  1013-CHECK-LIFE-BENEFIT.                                         ECS021
02153      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS021
02154          GO TO 1014-CHECK-AH-BENEFIT.                             ECS021
02155                                                                   ECS021
02156      IF CF-BUS-LF-SELECT (1) = SPACES AND                         ECS021
02157         CF-BUS-LF-SELECT (2) = SPACES AND                         ECS021
02158         CF-BUS-LF-SELECT (3) = SPACES                             ECS021
02159             GO TO 1015-CHECK-REPORT-CODE1.                        ECS021
02160                                                                   ECS021
02161      IF EP-BEN-CODE  = CF-BUS-LF-SELECT (1) OR                    ECS021
02162                        CF-BUS-LF-SELECT (2) OR                    ECS021
02163                        CF-BUS-LF-SELECT (3)                       ECS021
02164         GO TO 1015-CHECK-REPORT-CODE1                             ECS021
02165      ELSE                                                         ECS021
02166         ADD +1 TO LIFE-BEN-DROP-CNT                               ECS021
02167         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02168                                                                   ECS021
02169      GO TO 1015-CHECK-REPORT-CODE1.                               ECS021
02170                                                                   ECS021
02171  1014-CHECK-AH-BENEFIT.                                           ECS021
02172      IF CF-BUS-AH-SELECT (1)  = SPACES AND                        ECS021
02173         CF-BUS-AH-SELECT (2)  = SPACES AND                        ECS021
02174         CF-BUS-AH-SELECT (3)  = SPACES                            ECS021
02175           GO TO 1015-CHECK-REPORT-CODE1.                          ECS021
02176                                                                   ECS021
02177      IF EP-BEN-CODE  =  CF-BUS-AH-SELECT (1)  OR                  ECS021
02178                         CF-BUS-AH-SELECT (2)  OR                  ECS021
02179                         CF-BUS-AH-SELECT (3)                      ECS021
02180         GO TO 1015-CHECK-REPORT-CODE1                             ECS021
02181      ELSE                                                         ECS021
02182         ADD +1 TO AH-BEN-DROP-CNT                                 ECS021
02183         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02184                                                                   ECS021
02185  1015-CHECK-REPORT-CODE1.                                         ECS021
02186      IF CF-REPTCD1-SELECT (1) = SPACES AND                        ECS021
02187         CF-REPTCD1-SELECT (2) = SPACES AND                        ECS021
02188         CF-REPTCD1-SELECT (3) = SPACES                            ECS021
02189           GO TO 1016-NO-REPORT-CODE1-SELECTION.                   ECS021
02190                                                                   ECS021
02191      IF AM-REPORT-CODE-1 = SPACES                                 ECS021
02192          MOVE LOW-VALUES  TO  AM-REPORT-CODE-1.                   ECS021
02193                                                                   ECS021
02194      IF AM-REPORT-CODE-1 = CF-REPTCD1-SELECT (1) OR               ECS021
02195                            CF-REPTCD1-SELECT (2) OR               ECS021
02196                            CF-REPTCD1-SELECT (3)                  ECS021
02197         GO TO 1016-NO-REPORT-CODE1-SELECTION                      ECS021
02198      ELSE                                                         ECS021
02199         ADD +1 TO RPT-CODE1-DROP-CNT                              ECS021
02200         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02201                                                                   ECS021
02202  1016-NO-REPORT-CODE1-SELECTION.                                  ECS021
02203      IF CF-REPTCD1-OPT-NOT-USED                                   ECS021
02204          GO TO 1017-CHECK-REPORT-CODE2.                           ECS021
02205                                                                   ECS021
02206      MOVE CF-REPTCD1-OPT-SEQ       TO BRK-IDX.                    ECS021
02207      MOVE AM-REPORT-CODE-1         TO CONTROL-BREAK (BRK-IDX).    ECS021
02208                                                                   ECS021
02209  1017-CHECK-REPORT-CODE2.                                         ECS021
02210      IF CF-REPTCD2-SELECT (1)  = SPACES AND                       ECS021
02211         CF-REPTCD2-SELECT (2)  = SPACES AND                       ECS021
02212         CF-REPTCD2-SELECT (3)  = SPACES                           ECS021
02213             GO TO 1018-NO-REPORT-CODE2-SELECTION.                 ECS021
02214                                                                   ECS021
043007     IF (WS-REPORT-SEQ > 199)
043007        AND (WS-REPORT-SEQ < 300)
043007        MOVE AM-REPORT-CODE-3      TO AM-REPORT-CODE-2
           END-IF

02215      IF AM-REPORT-CODE-2 = SPACES                                 ECS021
02216          MOVE LOW-VALUES  TO  AM-REPORT-CODE-2.                   ECS021
02217                                                                   ECS021
02218      IF AM-REPORT-CODE-2 = CF-REPTCD2-SELECT (1)  OR              ECS021
02219                            CF-REPTCD2-SELECT (2)  OR              ECS021
02220                            CF-REPTCD2-SELECT (3)                  ECS021
02221         GO TO 1018-NO-REPORT-CODE2-SELECTION                      ECS021
02222      ELSE                                                         ECS021
02223         ADD +1 TO RPT-CODE2-DROP-CNT                              ECS021
02224         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02225                                                                   ECS021
02226  1018-NO-REPORT-CODE2-SELECTION.                                  ECS021
02227      IF CF-REPTCD2-OPT-NOT-USED                                   ECS021
02228          GO TO 1019-CHECK-USER-CODE1.                             ECS021
02229                                                                   ECS021
02230      MOVE CF-REPTCD2-OPT-SEQ       TO BRK-IDX.                    ECS021

043007     IF (WS-REPORT-SEQ > 199)
043007        AND (WS-REPORT-SEQ < 300)
043007        MOVE AM-REPORT-CODE-3      TO CONTROL-BREAK (BRK-IDX)
043007     ELSE
043007        MOVE AM-REPORT-CODE-2      TO CONTROL-BREAK (BRK-IDX)
043007     END-IF

           .
02233  1019-CHECK-USER-CODE1.                                           ECS021
02234      IF CF-USER1-SELECT (1)  = SPACES AND                         ECS021
02235         CF-USER1-SELECT (2)  = SPACES AND                         ECS021
02236         CF-USER1-SELECT (3)  = SPACES                             ECS021
02237             GO TO 1020-NO-USER-CODE1-SELECTION.                   ECS021
02238                                                                   ECS021
02239      IF AM-USER-SELECT-1 = SPACES                                 ECS021
02240          MOVE LOW-VALUES  TO  AM-USER-SELECT-1.                   ECS021
02241                                                                   ECS021
02242      IF AM-USER-SELECT-1 = CF-USER1-SELECT (1)  OR                ECS021
02243                            CF-USER1-SELECT (2)  OR                ECS021
02244                            CF-USER1-SELECT (3)                    ECS021
02245         GO TO 1020-NO-USER-CODE1-SELECTION                        ECS021
02246      ELSE                                                         ECS021
02247         ADD +1 TO USER1-DROP-CNT                                  ECS021
02248         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02249                                                                   ECS021
02250  1020-NO-USER-CODE1-SELECTION.                                    ECS021
02251      IF CF-USER1-OPT-NOT-USED                                     ECS021
02252          GO TO 1021-CHECK-USER-CODE2.                             ECS021
02253                                                                   ECS021
02254      MOVE CF-USER1-OPT-SEQ         TO BRK-IDX.                    ECS021
02255      MOVE AM-USER-SELECT-1         TO CONTROL-BREAK (BRK-IDX).    ECS021
02256                                                                   ECS021
02257  1021-CHECK-USER-CODE2.                                           ECS021
02258      IF CF-USER2-SELECT (1)  = SPACES AND                         ECS021
02259         CF-USER2-SELECT (2)  = SPACES AND                         ECS021
02260         CF-USER2-SELECT (3)  = SPACES                             ECS021
02261           GO TO 1022-NO-USER-CODE2-SELECTION.                     ECS021
02262                                                                   ECS021
02263      IF AM-USER-SELECT-2 = SPACES                                 ECS021
02264          MOVE LOW-VALUES  TO  AM-USER-SELECT-2.                   ECS021
02265                                                                   ECS021
02266      IF AM-USER-SELECT-2 = CF-USER2-SELECT (1)  OR                ECS021
02267                            CF-USER2-SELECT (2)  OR                ECS021
02268                            CF-USER2-SELECT (3)                    ECS021
02269         GO TO 1022-NO-USER-CODE2-SELECTION                        ECS021
02270      ELSE                                                         ECS021
02271         ADD +1 TO USER2-DROP-CNT                                  ECS021
02272         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02273                                                                   ECS021
02274  1022-NO-USER-CODE2-SELECTION.                                    ECS021
02275      IF CF-USER2-OPT-NOT-USED                                     ECS021
02276          GO TO 1023-CHECK-USER-CODE3.                             ECS021
02277                                                                   ECS021
02278      MOVE CF-USER2-OPT-SEQ         TO BRK-IDX.                    ECS021
02279      MOVE AM-USER-SELECT-2         TO CONTROL-BREAK (BRK-IDX).    ECS021
02280                                                                   ECS021
02281  1023-CHECK-USER-CODE3.                                           ECS021
02282      IF CF-USER3-SELECT (1) = SPACES AND                          ECS021
02283         CF-USER3-SELECT (2) = SPACES AND                          ECS021
02284         CF-USER3-SELECT (3) = SPACES                              ECS021
02285             GO TO 1024-NO-USER-CODE3-SELECTION.                   ECS021
02286                                                                   ECS021
02287      IF AM-USER-SELECT-3 = SPACES                                 ECS021
02288          MOVE LOW-VALUES  TO  AM-USER-SELECT-3.                   ECS021
02289                                                                   ECS021
02290      IF AM-USER-SELECT-3 = CF-USER3-SELECT (1)  OR                ECS021
02291                            CF-USER3-SELECT (2)  OR                ECS021
02292                            CF-USER3-SELECT (3)                    ECS021
02293         GO TO 1024-NO-USER-CODE3-SELECTION                        ECS021
02294      ELSE                                                         ECS021
02295         ADD +1 TO USER3-DROP-CNT                                  ECS021
02296         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02297                                                                   ECS021
02298  1024-NO-USER-CODE3-SELECTION.                                    ECS021
02299      IF CF-USER3-OPT-NOT-USED                                     ECS021
02300          GO TO 1025-CHECK-USER-CODE4.                             ECS021
02301                                                                   ECS021
02302      MOVE CF-USER3-OPT-SEQ         TO BRK-IDX.                    ECS021
02303      MOVE AM-USER-SELECT-3         TO CONTROL-BREAK (BRK-IDX).    ECS021
02304                                                                   ECS021
02305  1025-CHECK-USER-CODE4.                                           ECS021
02306      IF CF-USER4-SELECT (1)  = SPACES AND                         ECS021
02307         CF-USER4-SELECT (2)  = SPACES AND                         ECS021
02308         CF-USER4-SELECT (3)  = SPACES                             ECS021
02309             GO TO 1026-NO-USER-CODE4-SELECTION.                   ECS021
02310                                                                   ECS021
02311      IF AM-USER-SELECT-4 = SPACES                                 ECS021
02312          MOVE LOW-VALUES  TO  AM-USER-SELECT-4.                   ECS021
02313                                                                   ECS021
02314      IF AM-USER-SELECT-4 = CF-USER4-SELECT (1)  OR                ECS021
02315                            CF-USER4-SELECT (2)  OR                ECS021
02316                            CF-USER4-SELECT (3)                    ECS021
02317         GO TO 1026-NO-USER-CODE4-SELECTION                        ECS021
02318      ELSE                                                         ECS021
02319         ADD +1 TO USER4-DROP-CNT                                  ECS021
02320         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02321                                                                   ECS021
02322  1026-NO-USER-CODE4-SELECTION.                                    ECS021
02323      IF CF-USER4-OPT-NOT-USED                                     ECS021
02324          GO TO 1027-CHECK-USER-CODE5.                             ECS021
02325                                                                   ECS021
02326      MOVE CF-USER4-OPT-SEQ         TO BRK-IDX.                    ECS021
02327      MOVE AM-USER-SELECT-4         TO CONTROL-BREAK (BRK-IDX).    ECS021
02328                                                                   ECS021
02329  1027-CHECK-USER-CODE5.                                           ECS021
02330      IF CF-USER5-SELECT (1) = SPACES AND                          ECS021
02331         CF-USER5-SELECT (2) = SPACES AND                          ECS021
02332         CF-USER5-SELECT (3) = SPACES                              ECS021
02333             GO TO 1028-NO-USER-CODE5-SELECTION.                   ECS021
02334                                                                   ECS021
02335      IF AM-USER-SELECT-5 = SPACES                                 ECS021
02336          MOVE LOW-VALUES  TO  AM-USER-SELECT-5.                   ECS021
02337                                                                   ECS021
02338      IF AM-USER-SELECT-5 = CF-USER5-SELECT (1)  OR                ECS021
02339                            CF-USER5-SELECT (2)  OR                ECS021
02340                            CF-USER5-SELECT (3)                    ECS021
02341         GO TO 1028-NO-USER-CODE5-SELECTION                        ECS021
02342      ELSE                                                         ECS021
02343         ADD +1 TO USER5-DROP-CNT                                  ECS021
02344         GO TO 1510-RETRIEVE-NEW-EPEC.                             ECS021
02345                                                                   ECS021
02346  1028-NO-USER-CODE5-SELECTION.                                    ECS021
02347      IF CF-USER5-OPT-NOT-USED                                     ECS021
02348          GO TO 1045-EPEC-TEST-COMPLETE.                           ECS021
02349                                                                   ECS021
02350      MOVE CF-USER5-OPT-SEQ         TO BRK-IDX.                    ECS021
02351      MOVE AM-USER-SELECT-5         TO CONTROL-BREAK (BRK-IDX).    ECS021
02352                                                                   ECS021
02353      EJECT                                                        ECS021
02354 ***********************************************                   ECS021
02355 *  EPEC RECORD HAS PASSED EXCLUSION CRITERIA  *                   ECS021
02356 *  ESTABLISHED BY REPORT CUSTOMIZATION RECORD *                   ECS021
02357 ***********************************************                   ECS021
02358                                                                   ECS021
02359  1045-EPEC-TEST-COMPLETE.                                         ECS021
02360                                                                   ECS021
02361      IF CONTROL-BREAKS NOT = SW-REPORT-CONTROL-KEY                ECS021
02362          PERFORM 3000-RELEASE-SORT-REC THRU 3999-EXIT.            ECS021
02363                                                                   ECS021
02364      PERFORM 2200-MATCH-EPEC-RUN-DATE THRU 2299-EXIT.             ECS021
02365                                                                   ECS021
02366      IF BAD-EPEC-DATE                                             ECS021
02367          MOVE 'N'                  TO BAD-DATE-SWITCH             ECS021
02368          DISPLAY 'EPEC PRIMARY CONTROL      = ' EP-CONTROL        ECS021
02369          DISPLAY 'BAD EPEC DATE (EPEC YEAR) = ' EP-RUN-YR         ECS021
02370          DISPLAY '              (EPEC MNTH) = ' EP-RUN-MO         ECS021
02371          ADD +1 TO BAD-DATE-LOGIC-CNT                             ECS021
02372          GO TO 1510-RETRIEVE-NEW-EPEC.                            ECS021
02373                                                                   ECS021
02374 ****  CHECK TO SEE IF EPEC RECORD FALLS BETWEEN PERIODS  ****     ECS021
02375 ****  REQUIRED FOR REPORT. IF SO AND IT'S NOT A PURGE    ****     ECS021
02376 ****  RECORD, DROP IT.                                   ****     ECS021
02377                                                                   ECS021
02378      IF BRK-YY-MM (DTE-IDX) NOT = EPEC-YY-MM AND                  ECS021
02379          EP-PURGE NOT = 'P'                                       ECS021
02380              ADD +1 TO EPEC-DATE-DROP-CNT2                        ECS021
02381              GO TO 1510-RETRIEVE-NEW-EPEC.                        ECS021
02382                                                                   ECS021
02383  1500-APPLY-EPEC-TO-SORT.                                         ECS021
02384      PERFORM 2500-MATCH-SORT-BENEFIT THRU 2599-EXIT.              ECS021
02385                                                                   ECS021
02386      IF SORT-RECORD-FULL                                          ECS021
02387          PERFORM 3000-RELEASE-SORT-REC THRU 3999-EXIT             ECS021
02388          MOVE 'N'               TO SORT-BENEFITS-EXCEEDED         ECS021
02389          GO TO 1045-EPEC-TEST-COMPLETE.                           ECS021
02390                                                                   ECS021
02391 **************************************************                ECS021
02392 *  SPECIAL CODE REQUIRED TO CREDIT ADDITIONAL    *                ECS021
02393 *  COUNTERS WHEN RUNNING WITH A DECEMBER MONTH-  *                ECS021
02394 *  END DATE. REQUIRED TO DEVELOPE PYTD CORRECT.  *                ECS021
02395 **************************************************                ECS021
02396                                                                   ECS021
02397      IF DTE-IDX EQUAL +1                                          ECS021
02398          IF BRK-YY-MM(1) EQUAL BRK-YY-MM(2)                       ECS021
02399              ADD +1              TO DTE-IDX                       ECS021
02400              PERFORM 2520-BENEFIT-MATCHED THRU 2599-EXIT.         ECS021
02401                                                                   ECS021
02402 **************************************************                ECS021
02403 *  IF THE EPEC RECORD IS A PURGE EPEC RECORD IT  *                ECS021
02404 *  MUST BE ADDED TO ALL DATE LEVELS THAT CONTAIN *                ECS021
02405 *  A DATE GREATER THAN EP-RUN-DATE.              *                ECS021
02406 **************************************************                ECS021
02407                                                                   ECS021
02408      IF EP-DATA-FROM-PURGED-CERTS                                 ECS021
02409          ADD +1 TO DTE-IDX                                        ECS021
02410          PERFORM 2520-BENEFIT-MATCHED THRU 2599-EXIT              ECS021
02411              VARYING DTE-IDX FROM DTE-IDX BY +1                   ECS021
02412                UNTIL DTE-IDX GREATER +15.                         ECS021
02413                                                                   ECS021
02414  1510-RETRIEVE-NEW-EPEC.                                          ECS021
02415      PERFORM 4000-READ-EPEC THRU 4000-EXIT.                       ECS021
02416 *    IF EP-ACCOUNT = '0000072350'
      *       DISPLAY ' ACCOUNT FOUND '
      *    END-IF
02417      IF EP-CONTROL = HIGH-VALUES                                  ECS021
02418          PERFORM 3000-RELEASE-SORT-REC     THRU 3999-EXIT         ECS021
02419          PERFORM 4999-END-SELECT-EPEC-RECS THRU 4999-EXIT         ECS021
02420          GO TO 1999-EXIT.                                         ECS021
02421                                                                   ECS021
02422      IF EP-EXP-DTE IS EQUAL TO WS-PREV-EP-EXP-DTE                    CL**2
02423          NEXT SENTENCE                                            ECS021
02424      ELSE                                                         ECS021
02425          MOVE 'Y'                TO  WS-ACCT-BREAK-SW             ECS021
02426          MOVE EP-EXP-DTE         TO  WS-PREV-EP-EXP-DTE           ECS021
02427          MOVE EP-ACCOUNT         TO  WS-PREV-EP-ACCOUNT.          ECS021
02428                                                                   ECS021
02429      IF EP-ACCOUNT IS EQUAL TO WS-PREV-EP-ACCOUNT                 ECS021
02430          NEXT SENTENCE                                            ECS021
02431      ELSE                                                         ECS021
02432          MOVE 'Y'                TO  WS-ACCT-BREAK-SW             ECS021
02433          MOVE EP-ACCOUNT         TO  WS-PREV-EP-ACCOUNT           ECS021
02434          MOVE EP-EXP-DTE         TO  WS-PREV-EP-EXP-DTE.          ECS021
02435                                                                   ECS021
02436      GO TO 1000-CHECK-SELECTION-CRITERIA.                         ECS021
02437                                                                   ECS021
02438  1999-EXIT.                                                       ECS021
02439      EXIT.                                                        ECS021
02440                                                                   ECS021
02441 ***************************************************************   ECS021
02442 *  MATCH DATES IN EPEC TO PROPER DATE OCCURRENCE UNDER BENEFIT *  ECS021
02443 ***************************************************************   ECS021
02444                                                                   ECS021
02445  2200-MATCH-EPEC-RUN-DATE.                                        ECS021
02446      MOVE +1                     TO DTE-IDX.                      ECS021
02447      MOVE EP-RUN-CC              TO EPEC-CC.                      ECS021
02448      MOVE EP-RUN-YR              TO EPEC-YY.                      ECS021
02449      MOVE EP-RUN-MO              TO EPEC-MM.                      ECS021
02450                                                                   ECS021
02451  2210-MATCH-RUN-DATE-LOOP.                                        ECS021
02452      IF BRK-YY-MM (DTE-IDX) NOT LESS EPEC-YY-MM                   ECS021
02453          GO TO 2220-DATES-MATCHED.                                ECS021
02454                                                                   ECS021
02455  2215-NO-MATCH.                                                   ECS021
02456      ADD +1                      TO DTE-IDX.                      ECS021
02457                                                                   ECS021
02458      IF DTE-IDX GREATER +15                                       ECS021
02459          MOVE 'Y'                TO BAD-DATE-SWITCH               ECS021
02460          GO TO 2220-DATES-MATCHED.                                ECS021
02461                                                                   ECS021
02462      GO TO 2210-MATCH-RUN-DATE-LOOP.                              ECS021
02463                                                                   ECS021
02464  2220-DATES-MATCHED.                                              ECS021
02465                                                                   ECS021
02466  2299-EXIT.                                                       ECS021
02467      EXIT.                                                        ECS021
02468                                                                   ECS021
02469      EJECT                                                        ECS021
02470 ******************************************************            ECS021
02471 *  APPLY EPEC RECORD NUMBERS TO SORT RECORD NUMBERS  *            ECS021
02472 ******************************************************            ECS021
02473                                                                   ECS021
02474  2500-MATCH-SORT-BENEFIT.                                         ECS021
02475      MOVE +1                     TO BEN-IDX.                      ECS021
02476                                                                   ECS021
02477  2510-MATCH-BENEFIT-LOOP.                                         ECS021
02478      IF EP-BEN-CODE = SW-BENEFIT-CODE (BEN-IDX) AND               ECS021
02479         EP-RCD-TYPE = SW-BENEFIT-TYPE (BEN-IDX)                   ECS021
02480             GO TO 2520-BENEFIT-MATCHED.                           ECS021
02481                                                                   ECS021
02482      IF SW-BENEFIT-CODE (BEN-IDX) = ZEROS                         ECS021
02483          MOVE EP-BEN-CODE        TO SW-BENEFIT-CODE (BEN-IDX)     ECS021
02484          MOVE EP-RCD-TYPE        TO SW-BENEFIT-TYPE (BEN-IDX)     ECS021
02485          MOVE SAVE-BEN-INDEX     TO SW-BEN-TBL-POS  (BEN-IDX)     ECS021
02486          GO TO 2520-BENEFIT-MATCHED.                              ECS021
02487                                                                   ECS021
02488      ADD +1 TO BEN-IDX.                                           ECS021
02489                                                                   ECS021
02490      IF BEN-IDX GREATER +23                                       ECS021
02491          MOVE 'Y'                TO SORT-BENEFITS-EXCEEDED        ECS021
02492          GO TO 2599-EXIT                                          ECS021
02493        ELSE                                                       ECS021
02494          GO TO 2510-MATCH-BENEFIT-LOOP.                           ECS021
02495                                                                   ECS021
02496  2520-BENEFIT-MATCHED.                                            ECS021
02497      IF VALID-EC-ID                                               ECS021
02498          GO TO 2530-EC-RECORD-ADDS.                               ECS021
02499                                                                   ECS021
02500      ADD EP-ISS-CNT    TO SW-ISS-CNT   (BEN-IDX DTE-IDX).         ECS021
02501      ADD EP-CNC-CNT    TO SW-CNC-CNT   (BEN-IDX DTE-IDX).         ECS021
02502      ADD EP-ISS-PRM    TO SW-ISS-PREM  (BEN-IDX DTE-IDX).         ECS021
02503      ADD EP-CNC-PRM    TO SW-CNC-PREM  (BEN-IDX DTE-IDX).         ECS021
02504      ADD EP-CLM-AMT    TO SW-CLM-AMT   (BEN-IDX DTE-IDX).         ECS021
02505      ADD EP-CLM-CNT    TO SW-CLM-CNT   (BEN-IDX DTE-IDX).         ECS021
02506      ADD EP-CLAIM-ADJ  TO SW-CLM-AMT   (BEN-IDX DTE-IDX).         ECS021
02507      ADD EP-CLM-DU     TO SW-LOSS-RESV (BEN-IDX DTE-IDX).         ECS021
02508      ADD EP-CLM-IBNR   TO SW-LOSS-RESV (BEN-IDX DTE-IDX).         ECS021
02509      ADD EP-LOSS-RESV  TO SW-LOSS-RESV (BEN-IDX DTE-IDX).         ECS021
02510                                                                   ECS021
02511      MOVE EP-ISS-PRM          TO SAVE-ISS-PRM.                    ECS021
02512      MOVE EP-CNC-PRM          TO SAVE-CNC-PRM.                    ECS021
02513      MOVE ZEROS               TO WS-PRM-EARND.                    ECS021
02514                                                                   ECS021
02515      IF WS-EP-CODE = 'R' OR 'N' OR 'U' OR 'A' OR 'T'              ECS021
02516          MOVE EP-PRM-78       TO WS-PRM-EARND.                    ECS021
02517                                                                   ECS021
02518      IF WS-EP-CODE = 'P'                                          ECS021
02519          MOVE EP-PRM-PR       TO WS-PRM-EARND.                    ECS021
02520                                                                   ECS021
02521      IF (WS-EP-CODE = 'B' OR 'K' OR 'L')                          ECS021
02522          MOVE EP-PRM-ST       TO WS-PRM-EARND.                    ECS021
02523                                                                   ECS021
02524      IF WS-EP-CODE = 'M'                                          ECS021
02525          COMPUTE WS-PRM-EARND ROUNDED =                           ECS021
02526                 (EP-PRM-PR + EP-PRM-78)  *  +.5.                  ECS021
02527                                                                   ECS021
02528      IF WS-EP-CODE = '1'                                          ECS021
02529          COMPUTE WS-PRM-EARND ROUNDED =                           ECS021
02530                 (EP-PRM-PR * +.6667 ) + (EP-PRM-78 * +.3333).     ECS021
02531                                                                   ECS021
02532      IF WS-EP-CODE = '2'                                          ECS021
02533          COMPUTE WS-PRM-EARND ROUNDED =                           ECS021
02534                 (EP-PRM-PR * +.80 ) + (EP-PRM-78 * +.20).         ECS021
02535                                                                   ECS021
02536      COMPUTE SW-PRM-EARND (BEN-IDX DTE-IDX) =                     ECS021
02537              SW-PRM-EARND (BEN-IDX DTE-IDX) + WS-PRM-EARND.       ECS021
02538                                                                   ECS021
02539      IF EP-IN-FORCE NOT NUMERIC                                   ECS021
02540          MOVE ZEROS           TO EP-IN-FORCE.                     ECS021
02541                                                                   ECS021
02542      ADD EP-IN-FORCE          TO SW-PRM-INFRC(BEN-IDX DTE-IDX).   ECS021
02543                                                                   ECS021
02544      IF EP-INFORCE-CNT NOT NUMERIC                                ECS021
02545          MOVE ZEROS           TO EP-INFORCE-CNT.                  ECS021
02546                                                                   ECS021
02547      ADD EP-INFORCE-CNT TO SW-INFRC-CNT(BEN-IDX DTE-IDX).         ECS021
02548                                                                   ECS021
02549      IF EP-AVG-AGE NOT NUMERIC                                    ECS021
02550          MOVE ZEROS           TO EP-AVG-AGE.                      ECS021
02551                                                                   ECS021
02552      IF EP-ISS-CNT GREATER ZEROS                                  ECS021
02553          COMPUTE SW-AVG-AGE (BEN-IDX DTE-IDX) =                   ECS021
02554                  SW-AVG-AGE (BEN-IDX DTE-IDX) +                   ECS021
02555                  (EP-AVG-AGE * EP-ISS-CNT)                        ECS021
02556      ELSE                                                         ECS021
02557          ADD EP-AVG-AGE  TO SW-AVG-AGE  (BEN-IDX DTE-IDX).        ECS021
02558                                                                   ECS021
02559      IF EP-AVG-ORIG-TERM NOT NUMERIC                              ECS021
02560          MOVE ZEROS           TO EP-AVG-ORIG-TERM.                ECS021
02561                                                                   ECS021
02562      IF EP-ISS-CNT GREATER THAN ZEROS                             ECS021
02563          COMPUTE SW-AVG-ORG-TRM (BEN-IDX DTE-IDX) =               ECS021
02564                  SW-AVG-ORG-TRM (BEN-IDX DTE-IDX) +               ECS021
02565                  (EP-AVG-ORIG-TERM * EP-ISS-CNT)                  ECS021
02566      ELSE                                                         ECS021
02567          ADD EP-AVG-ORIG-TERM TO                                  ECS021
02568                          SW-AVG-ORG-TRM (BEN-IDX DTE-IDX).        ECS021
02569                                                                   ECS021
02570      IF EP-WEIGHTED-AGE NOT NUMERIC                               ECS021
02571          MOVE ZEROS           TO EP-WEIGHTED-AGE.                 ECS021
02572                                                                   ECS021
02573      IF EP-ISS-CNT GREATER THAN ZEROS                             ECS021
02574          COMPUTE SW-WGHT-AGE (BEN-IDX DTE-IDX) =                  ECS021
02575                  SW-WGHT-AGE (BEN-IDX DTE-IDX) +                  ECS021
02576                  (EP-WEIGHTED-AGE * EP-ISS-CNT)                   ECS021
02577      ELSE                                                         ECS021
02578          ADD EP-WEIGHTED-AGE TO SW-WGHT-AGE (BEN-IDX DTE-IDX).    ECS021
02579                                                                   ECS021
02580      IF EP-WEIGHTED-ORIG-TERM NOT NUMERIC                         ECS021
02581          MOVE ZEROS           TO EP-WEIGHTED-ORIG-TERM.           ECS021
02582                                                                   ECS021
02583      IF EP-ISS-CNT GREATER THAN ZEROS                             ECS021
02584          COMPUTE SW-WGHT-ORG-TRM(BEN-IDX DTE-IDX) =               ECS021
02585                  SW-WGHT-ORG-TRM(BEN-IDX DTE-IDX) +               ECS021
02586                  (EP-WEIGHTED-ORIG-TERM * EP-ISS-CNT)             ECS021
02587      ELSE                                                         ECS021
02588          ADD EP-WEIGHTED-ORIG-TERM TO                             ECS021
02589                               SW-WGHT-ORG-TRM(BEN-IDX DTE-IDX).   ECS021
02590                                                                   ECS021
02591      IF EP-AVG-REM-TERM NOT NUMERIC                               ECS021
02592          MOVE ZEROS           TO EP-AVG-REM-TERM.                 ECS021
02593                                                                   ECS021
02594      ADD 1 TO  SW-ADDED-TO-CNT (BEN-IDX DTE-IDX).                 ECS021
02595                                                                   ECS021
121707     MOVE AM-STATUS              TO SW-ACCT-STATUS

02596      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            ECS021
02597          IF AM-LF-RPT021-EXP-PCT NUMERIC                          ECS021
02598              IF AM-LF-RPT021-EXP-PCT NOT = ZEROS                  ECS021
02599                  COMPUTE SW-EXP-PCT (BEN-IDX DTE-IDX) =           ECS021
02600                          SW-EXP-PCT (BEN-IDX DTE-IDX) +           ECS021
02601                          (AM-LF-RPT021-EXP-PCT / +100)            ECS021
02602              ELSE                                                 ECS021
02603                  COMPUTE SW-EXP-PCT (BEN-IDX DTE-IDX) =           ECS021
02604                          SW-EXP-PCT (BEN-IDX DTE-IDX) +           ECS021
02605                          STATE-LF-EXP-PCT (ST-IDX)                ECS021
02606          ELSE                                                     ECS021
02607              COMPUTE SW-EXP-PCT (BEN-IDX DTE-IDX) =               ECS021
02608                      SW-EXP-PCT (BEN-IDX DTE-IDX) +               ECS021
02609                      STATE-LF-EXP-PCT (ST-IDX).                   ECS021
02610                                                                   ECS021
02611      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS021
02612          IF AM-AH-RPT021-EXP-PCT NUMERIC                          ECS021
02613              IF AM-AH-RPT021-EXP-PCT NOT = ZEROS                  ECS021
02614                  COMPUTE SW-EXP-PCT (BEN-IDX DTE-IDX) =           ECS021
02615                          SW-EXP-PCT (BEN-IDX DTE-IDX) +           ECS021
02616                         (AM-AH-RPT021-EXP-PCT / +100)             ECS021
02617              ELSE                                                 ECS021
02618                  COMPUTE SW-EXP-PCT (BEN-IDX DTE-IDX) =           ECS021
02619                          SW-EXP-PCT (BEN-IDX DTE-IDX) +           ECS021
02620                          STATE-AH-EXP-PCT (ST-IDX)                ECS021
02621          ELSE                                                     ECS021
02622              COMPUTE SW-EXP-PCT (BEN-IDX DTE-IDX) =               ECS021
02623                      SW-EXP-PCT (BEN-IDX DTE-IDX) +               ECS021
02624                      STATE-AH-EXP-PCT (ST-IDX).                   ECS021
02625                                                                   ECS021
02626      GO TO 2599-EXIT.                                             ECS021
02627                                                                   ECS021
02628  2530-EC-RECORD-ADDS.                                             ECS021
02629      MOVE +1                  TO AGT-IDX.                         ECS021
02630                                                                   ECS021
02631  2535-EC-COMM-LOOP.                                               ECS021
02632      MOVE EC-AGT-TYPE (AGT-IDX)                                   ECS021
02633                               TO CHECK-AGT-TYPE.                  ECS021
02634                                                                   ECS021
02635      IF NOT PROPER-AGT-TYPE                                       ECS021
02636          GO TO 2540-BYPASS-COMMISSION-ADD.                        ECS021
102908
102908     IF EC-AGT-TYPE (AGT-IDX) = 'C' OR 'D'
102908        COMPUTE SW-ACCT-COMM (BEN-IDX DTE-IDX) = 
102908                SW-ACCT-COMM (BEN-IDX DTE-IDX) + 
102908                (EC-ISS-COMM (AGT-IDX) - EC-CNC-COMM (AGT-IDX))
102908     ELSE
102908        COMPUTE SW-OW-COMM (BEN-IDX DTE-IDX) = 
102908                SW-OW-COMM (BEN-IDX DTE-IDX) + 
102908                (EC-ISS-COMM (AGT-IDX) - EC-CNC-COMM (AGT-IDX))
102908     END-IF.
02637                                                                   ECS021
02638      MOVE ZEROS               TO WS-COMM-EARND.                   ECS021
02639                                                                   ECS021
02640      IF WS-EP-CODE = 'R' OR 'N' OR 'U' OR 'A' OR 'T'              ECS021
02641          MOVE EC-COMM-78 (AGT-IDX)                                ECS021
02642                               TO WS-COMM-EARND.                   ECS021
02643                                                                   ECS021
02644      IF WS-EP-CODE = 'P'                                          ECS021
02645          MOVE EC-COMM-PR (AGT-IDX)                                ECS021
02646                               TO WS-COMM-EARND.                   ECS021
02647                                                                   ECS021
02648      IF (WS-EP-CODE = 'B' OR 'K' OR 'L')                          ECS021
02649         IF EC-COMM-ST (AGT-IDX) NOT NUMERIC                       ECS021
02650             MOVE ZEROS        TO  EC-COMM-ST (AGT-IDX)            ECS021
02651                                   WS-COMM-EARND                   ECS021
02652         ELSE                                                      ECS021
02653             MOVE EC-COMM-ST (AGT-IDX)                             ECS021
02654                               TO WS-COMM-EARND.                   ECS021
02655                                                                   ECS021
02656      IF WS-EP-CODE = 'M'                                          ECS021
02657          COMPUTE WS-COMM-EARND ROUNDED =                          ECS021
02658                 (EC-COMM-PR (AGT-IDX) +                           ECS021
02659                  EC-COMM-78 (AGT-IDX)) *  +.5.                    ECS021
02660                                                                   ECS021
02661      IF WS-EP-CODE = '1'                                          ECS021
02662          COMPUTE WS-COMM-EARND ROUNDED =                          ECS021
02663                 (EC-COMM-PR (AGT-IDX) * +.6667) +                 ECS021
02664                 (EC-COMM-78 (AGT-IDX) * +.3333).                  ECS021
02665                                                                   ECS021
02666      IF WS-EP-CODE = '2'                                          ECS021
02667          COMPUTE WS-COMM-EARND ROUNDED =                          ECS021
02668                 (EC-COMM-PR (AGT-IDX) * +.80) +                   ECS021
02669                 (EC-COMM-78 (AGT-IDX) * +.20).                    ECS021
02670                                                                   ECS021
02671          COMPUTE SW-NET-COMPEN (BEN-IDX DTE-IDX) =                ECS021
02672                  SW-NET-COMPEN (BEN-IDX DTE-IDX) +                ECS021
02673                  WS-COMM-EARND.                                   ECS021
02674                                                                   ECS021
02675  2540-BYPASS-COMMISSION-ADD.                                      ECS021
02676      ADD +1 TO AGT-IDX.                                           ECS021
02677                                                                   ECS021
02678      IF AGT-IDX LESS +6                                           ECS021
02679          GO TO 2535-EC-COMM-LOOP.                                 ECS021
02680                                                                   ECS021
02681  2599-EXIT.                                                       ECS021
02682      EXIT.                                                        ECS021
02683                                                                   ECS021
02684      EJECT                                                        ECS021
02685 ******************************************                        ECS021
02686 *  RELEASE SORT RECORD TO SORT, CLEAR    *                        ECS021
02687 *  COUNTERS AND INSERT NEW KEY           *                        ECS021
02688 ******************************************                        ECS021
02689                                                                   ECS021
02690  3000-RELEASE-SORT-REC.                                           ECS021
02691      IF SORT-KEY-NEEDS-PRIMING                                    ECS021
02692          MOVE 'N'                TO SORT-PRIME-SWITCH             ECS021
02693          GO TO 3100-SKIP-RECORD-RELEASE.                          ECS021
02694                                                                   ECS021
02695      IF DTE-PGM-OPT NOT = '2'                                     ECS021
02696          GO TO 3050-SKIP-NET-CALC.                                ECS021
02697                                                                   ECS021
02698      MOVE +1                     TO BEN-IDX                       ECS021
02699                                     DTE-IDX.                      ECS021
02700  3050-NET-CALC-LOOP.                                              ECS021
02701      IF SW-BENEFIT-CODE (BEN-IDX) = ZEROS                         ECS021
02702          GO TO 3050-SKIP-NET-CALC.                                ECS021
02703                                                                   ECS021
02704      IF SW-PRM-EARND (BEN-IDX DTE-IDX) = ZEROS                    ECS021
02705          GO TO 3050-ADD-IDX.                                      ECS021
02706                                                                   ECS021
02707      COMPUTE WS-NET-PCT  ROUNDED =                                ECS021
02708              SW-NET-COMPEN (BEN-IDX DTE-IDX) /                    ECS021
02709              SW-PRM-EARND  (BEN-IDX DTE-IDX).                     ECS021
02710                                                                   ECS021
02711      COMPUTE WS-COMPEN-NWN ROUNDED =                              ECS021
02712             (SW-ISS-PREM (BEN-IDX DTE-IDX) -                      ECS021
02713              SW-CNC-PREM (BEN-IDX DTE-IDX)) *                     ECS021
02714              WS-NET-PCT.                                          ECS021
02715                                                                   ECS021
02716      MOVE WS-COMPEN-NWN   TO   SW-NET-COMPEN (BEN-IDX DTE-IDX).   ECS021
02717                                                                   ECS021
02718  3050-ADD-IDX.                                                    ECS021
02719      ADD +1 TO DTE-IDX.                                           ECS021
02720                                                                   ECS021
02721      IF DTE-IDX LESS +16                                          ECS021
02722          GO TO 3050-NET-CALC-LOOP.                                ECS021
02723                                                                   ECS021
02724      ADD +1 TO BEN-IDX.                                           ECS021
02725                                                                   ECS021
02726      IF BEN-IDX LESS +24                                          ECS021
02727          MOVE +1                 TO DTE-IDX                       ECS021
02728          GO TO 3050-NET-CALC-LOOP.                                ECS021
02729                                                                   ECS021
02730  3050-SKIP-NET-CALC.                                              ECS021
02731                                                                   ECS021
02732      MOVE +1                     TO GNRL-IDX.                     ECS021
02733                                                                   ECS021
02734  3050-RELEASE-AGENT-LOOP.                                         ECS021
02735                                                                   ECS021
02736      IF CF-AGENT-SELECT (1)  = SPACES AND                         ECS021
02737         CF-AGENT-SELECT (2)  = SPACES AND                         ECS021
02738         CF-AGENT-SELECT (3)  = SPACES                             ECS021
02739             GO TO 3050-NO-AGENT-SELECTION.                        ECS021
02740                                                                   ECS021
02741      IF SW-BREAK-FIELD(CF-AGENT-OPT-SEQ) = CF-AGENT-SELECT (1) OR ECS021
02742                                            CF-AGENT-SELECT (2) OR ECS021
02743                                            CF-AGENT-SELECT (3)    ECS021
02744         NEXT SENTENCE                                             ECS021
02745      ELSE                                                         ECS021
02746         ADD +1                   TO AGENT-DROP-CNT                ECS021
02747         GO TO 3050-CHECK-AGENT-LOOP.                              ECS021
02748                                                                   ECS021
02749  3050-NO-AGENT-SELECTION.                                         ECS021
02750                                                                   ECS021
02751      IF CF-AGENT-OPT-USED                                         ECS021
02752          PERFORM 3500-READ-COMP-MSTR THRU 3500-EXIT.              ECS021
02753                                                                   ECS021
02754      MOVE +0                    TO SP-ISS-CNT                     ECS021
02755                                    SP-CNC-CNT.                    ECS021
02756                                                                   ECS021
02757      IF WS-ACCT-WITH-ISSUE-ACTIVITY                               ECS021
02758          PERFORM 3700-SEARCH-FOR-ACTIVITY THRU 3700-EXIT          ECS021
02759          IF NO-CURRENT-ACTIVITY                                   ECS021
02760              GO TO 3100-SKIP-RECORD-RELEASE.                      ECS021
02761                                                                   ECS021
02762      IF WS-ACCT-WITH-NO-PRODUCTION                                ECS021
02763          PERFORM 3600-SEARCH-FOR-ACTIVITY THRU 3600-EXIT          ECS021
02764          IF CURRENT-ACTIVITY-FOUND                                ECS021
02765              GO TO 3100-SKIP-RECORD-RELEASE.                      ECS021
02766                                                                   ECS021
02767      IF WS-ACCT-CURRENT-MONTH-ACT                                 ECS021
02768          PERFORM 3600-SEARCH-FOR-ACTIVITY THRU 3600-EXIT          ECS021
02769          IF NO-CURRENT-ACTIVITY                                   ECS021
02770              GO TO 3100-SKIP-RECORD-RELEASE.                      ECS021
02771                                                                   ECS021
02772      RELEASE SW-RECORD.                                           ECS021
02773      ADD +1                      TO EPECS-SORTED.                 ECS021
02774                                                                   ECS021
02775  3050-CHECK-AGENT-LOOP.                                           ECS021
02776                                                                   ECS021
02777      IF CF-AGENT-OPT-NOT-USED                                     ECS021
02778          GO TO 3050-NO-AGENT-BREAK.                               ECS021
02779                                                                   ECS021
02780 ****  LOGIC NEEDED TO SEARCH THE REMAINING COMPENSATION  ****     ECS021
02781 ****  STRUCTURE AND CREATE SORT RECORDS TO REPORT THE    ****     ECS021
02782 ****  ADDITIONAL GENERAL AGENTS FOUND. ONLY USED WHEN    ****     ECS021
02783 ****  GENERAL AGENT SELECTED AS A REPORT BREAK.          ****     ECS021
02784                                                                   ECS021
02785      IF GNRL-IDX GREATER THAN WS-GA-COUNT                         ECS021
02786          GO TO 3050-NO-AGENT-BREAK.                               ECS021
02787                                                                   ECS021
02788      MOVE WS-AGENT-NUMBER (GNRL-IDX)                              ECS021
02789                                  TO                               ECS021
02790                                  SW-BREAK-FIELD(CF-AGENT-OPT-SEQ).ECS021
02791                                                                   ECS021
02792      ADD +1                      TO GNRL-IDX                      ECS021
02793                                     GA-LOOP-COUNT.                ECS021
02794                                                                   ECS021
02795      GO TO 3050-RELEASE-AGENT-LOOP.                               ECS021
02796                                                                   ECS021
02797  3050-NO-AGENT-BREAK.                                             ECS021
02798                                                                   ECS021
02799      MOVE 'N'                    TO SORT-BENEFITS-EXCEEDED.       ECS021
02800                                                                   ECS021
pemuni*    IF PARM-LENGTH = ZERO                                        ECS021
pemuni         GO TO 3100-SKIP-RECORD-RELEASE.                          ECS021
02803                                                                   ECS021
02804      MOVE +1                     TO BEN-IDX                       ECS021
02805                                     DTE-IDX.                      ECS021
02806                                                                   ECS021
02807  3050-BENEFIT-ANALYSIS-LOOP.                                      ECS021
02808      IF SW-BENEFIT-CODE (BEN-IDX) = ZEROS                         ECS021
02809          GO TO 3100-SKIP-RECORD-RELEASE.                          ECS021
02810                                                                   ECS021
02811      DISPLAY '**              ** '.                               ECS021
02812      DISPLAY '** BENEFIT CODE ** ' SW-BENEFIT-CODE (BEN-IDX).     ECS021
02813      DISPLAY '** BENEFIT TYPE ** ' SW-BENEFIT-TYPE (BEN-IDX).     ECS021
02814      MOVE SW-BEN-TBL-POS (BEN-IDX)                                ECS021
02815                                  TO W-DISPLAY-TBL-POS.            ECS021
02816      DISPLAY '** BENE POSITION** ' W-DISPLAY-TBL-POS.             ECS021
02817                                                                   ECS021
02818  3075-DATE-ANALYSIS-LOOP.                                         ECS021
02819      DISPLAY ' '.                                                 ECS021
02820      DISPLAY '** CNTS / AMTS (YY/MM) ** ' BRK-YY-MM   (DTE-IDX).  ECS021
02821      DISPLAY ' '.                                                 ECS021
02822      DISPLAY '*ISSUE COUNT * ' SW-ISS-CNT     (BEN-IDX DTE-IDX).  ECS021
02823      DISPLAY '*CANCEL COUNT* ' SW-CNC-CNT     (BEN-IDX DTE-IDX).  ECS021
02824      DISPLAY '* ISSUE PREM * ' SW-ISS-PREM    (BEN-IDX DTE-IDX).  ECS021
02825      DISPLAY '*CANCEL PREM * ' SW-CNC-PREM    (BEN-IDX DTE-IDX).  ECS021
02826      DISPLAY '* NET COMPEN * ' SW-NET-COMPEN  (BEN-IDX DTE-IDX).  ECS021
02827      DISPLAY '*CLAIM  COUNT* ' SW-CLM-CNT     (BEN-IDX DTE-IDX).  ECS021
02828      DISPLAY '*CLAIM AMOUNT* ' SW-CLM-AMT     (BEN-IDX DTE-IDX).  ECS021
02829      DISPLAY '* LOSS RESV  * ' SW-LOSS-RESV   (BEN-IDX DTE-IDX).  ECS021
02830      DISPLAY '*EARNED PREM * ' SW-PRM-EARND   (BEN-IDX DTE-IDX).  ECS021
02831      DISPLAY '*INFORCE PREM* ' SW-PRM-INFRC   (BEN-IDX DTE-IDX).  ECS021
02832      DISPLAY '*INFORCE CNT * ' SW-INFRC-CNT   (BEN-IDX DTE-IDX).  ECS021
02833      DISPLAY '*  AVRG AGE  * ' SW-AVG-AGE     (BEN-IDX DTE-IDX).  ECS021
02834      DISPLAY '*AVG ORG TRM * ' SW-AVG-ORG-TRM (BEN-IDX DTE-IDX).  ECS021
02835      DISPLAY '*WGHT    AGE * ' SW-WGHT-AGE    (BEN-IDX DTE-IDX).  ECS021
02836      DISPLAY '*WGHT ORG TRM* ' SW-WGHT-ORG-TRM(BEN-IDX DTE-IDX).  ECS021
02837      DISPLAY '*EXPENSE PCT * ' SW-EXP-PCT     (BEN-IDX DTE-IDX).  ECS021
02838      DISPLAY '*ADDED TO CNT* ' SW-ADDED-TO-CNT(BEN-IDX DTE-IDX).  ECS021
02839      DISPLAY ' '.                                                 ECS021
02840      DISPLAY ' '.                                                 ECS021
02841                                                                   ECS021
02842      ADD +1 TO DTE-IDX.                                           ECS021
02843                                                                   ECS021
02844      IF DTE-IDX LESS +16                                          ECS021
02845          GO TO 3075-DATE-ANALYSIS-LOOP.                           ECS021
02846                                                                   ECS021
02847      ADD +1 TO BEN-IDX.                                           ECS021
02848                                                                   ECS021
02849      IF BEN-IDX LESS +24                                          ECS021
02850          MOVE +1                 TO DTE-IDX                       ECS021
02851          GO TO 3050-BENEFIT-ANALYSIS-LOOP.                        ECS021
02852                                                                   ECS021
02853  3100-SKIP-RECORD-RELEASE.                                        ECS021
02854                                                                   ECS021
02855      IF WS-ACCT-BREAK-SW IS EQUAL TO 'Y'                          ECS021
02856          MOVE CF-AGENT-OPT-SEQ      TO  BRK-IDX                   ECS021
02857          MOVE WS-1ST-AGENT-NUMBER   TO  CONTROL-BREAK (BRK-IDX)   ECS021
02858          MOVE 'N'                   TO  WS-ACCT-BREAK-SW.         ECS021
02859                                                                   ECS021
02860      MOVE SPACES                 TO SW-RECORD.                    ECS021
02861      MOVE CONTROL-BREAKS         TO SW-REPORT-CONTROL-KEY.        ECS021
102908     MOVE AM-CARRIER             TO SW-CARRIER.
PEMMOD     MOVE WS-AM-NAME             TO SW-ACCT-NAME.                 ECS021
PEMMOD     MOVE AM-1ST-PROD-DATE    TO SW-PROD-DATE.                    ECS021
02864      MOVE STATE-NAME (ST-IDX)    TO SW-STATE-NAME.                ECS021
102908     MOVE STATE-ABBREVIATION (ST-IDX) TO SW-STATE-ABBREVIATION.
02865                                                                   ECS021
02866      IF CF-AGENT-OPT-USED                                         ECS021
02867          PERFORM 3400-SCAN-FOR-GNRL-AGENTS THRU 3400-EXIT.        ECS021
02868                                                                   ECS021
02869      MOVE +1                     TO BEN-IDX                       ECS021
02870                                     DTE-IDX.                      ECS021
02871                                                                   ECS021
02872  3200-CLEAR-SORT-BENEFIT-LOOP.                                    ECS021
02873      MOVE ZEROS                  TO SW-BENEFIT-CODE (BEN-IDX)     ECS021
02874                                     SW-BEN-TBL-POS  (BEN-IDX).    ECS021
02875                                                                   ECS021
02876  3300-CLEAR-SORT-DATE-LOOP.                                       ECS021
02877      MOVE ZERO-ACCUMS            TO SW-PERIOD (BEN-IDX DTE-IDX).  ECS021
02878      ADD +1 TO DTE-IDX.                                           ECS021
02879                                                                   ECS021
02880      IF DTE-IDX LESS +16                                          ECS021
02881          GO TO 3300-CLEAR-SORT-DATE-LOOP.                         ECS021
02882                                                                   ECS021
02883      ADD +1 TO BEN-IDX.                                           ECS021
02884                                                                   ECS021
02885      IF BEN-IDX LESS +24                                          ECS021
02886          MOVE +1                 TO DTE-IDX                       ECS021
02887          GO TO 3200-CLEAR-SORT-BENEFIT-LOOP.                      ECS021
02888                                                                   ECS021
02889  3999-EXIT.                                                       ECS021
02890      EXIT.                                                        ECS021
02891                                                                   ECS021
02892  3400-SCAN-FOR-GNRL-AGENTS.                                       ECS021
02893                                                                   ECS021
02894      MOVE +1                     TO GNRL-IDX                      ECS021
02895      MOVE ZEROS                  TO WS-GA-COUNT.                  ECS021
02896                                                                   ECS021
02897  3400-GENERAL-AGENT-LOOP.                                         ECS021
02898                                                                   ECS021
02899      IF AM-COM-TYP (GNRL-IDX) EQUAL 'O' OR 'P' OR 'G' OR 'B'      ECS021
02900          IF AM-AGT (GNRL-IDX) NOT EQUAL WS-1ST-AGENT-NUMBER       ECS021
02901              ADD +1              TO WS-GA-COUNT                   ECS021
02902              MOVE AM-AGT (GNRL-IDX)                               ECS021
02903                                  TO WS-AGENT-NUMBER (WS-GA-COUNT) ECS021
02904              MOVE AM-CARRIER     TO WS-SAVE-GA-CARR               ECS021
02905              MOVE AM-GROUPING    TO WS-SAVE-GA-GROUP.             ECS021
02906                                                                   ECS021
02907      ADD +1                      TO GNRL-IDX.                     ECS021
02908                                                                   ECS021
02909      IF GNRL-IDX LESS THAN +11                                    ECS021
02910          GO TO 3400-GENERAL-AGENT-LOOP.                           ECS021
02911                                                                   ECS021
02912  3400-EXIT.                                                       ECS021
02913      EXIT.                                                        ECS021
02914                                                                   ECS021
02915  3500-READ-COMP-MSTR.                                             ECS021
02916                                                                   ECS021
02917      MOVE DTE-CLASIC-COMPANY-CD        TO CO-COMPANY-CD.          ECS021
02918      MOVE WS-SAVE-GA-CARR              TO CO-CARRIER.             ECS021
02919      MOVE WS-SAVE-GA-GROUP             TO CO-GROUPING.            ECS021
02920      MOVE SW-BREAK-FIELD (CF-AGENT-OPT-SEQ)                       ECS021
02921                                        TO CO-RESP-NO.             ECS021
02922      MOVE 'G'                          TO CO-TYPE.                ECS021
02923      MOVE LOW-VALUES                   TO CO-ACCOUNT.             ECS021
02924                                                                   ECS021
02925      READ COMP-MSTR.                                              ECS021
02926                                                                   ECS021
02927      IF ERCOMP-FILE-STATUS IS NOT EQUAL TO '00'                   ECS021
02928          DISPLAY 'RETURN CODE = ' ERCOMP-FILE-STATUS              ECS021
02929          MOVE '****** NO AGENT ******' TO SW-GA-NAME              ECS021
02930          GO TO 3500-EXIT.                                         ECS021
02931                                                                   ECS021
02932      IF CO-ACCT-NAME IS EQUAL TO SPACES OR LOW-VALUES             ECS021
02933          MOVE CO-MAIL-NAME                 TO SW-GA-NAME          ECS021
02934      ELSE                                                         ECS021
02935          MOVE CO-ACCT-NAME                 TO SW-GA-NAME.         ECS021
02936                                                                   ECS021
02937  3500-EXIT.                                                       ECS021
02938      EXIT.                                                        ECS021
02939                                                                   ECS021
02940      EJECT                                                        ECS021
02941 ***********************************************************       ECS021
02942 * THE SELECTION FOR THOSE ACCOUNTS WITH NO ACTIVITY FOR   *       ECS021
02943 * THE CURRENT MONTH HAS BEEN CHOSEN.  THIS WILL SEARCH    *       ECS021
02944 * THE 15 OCCURRENCE OF ALL BENEFITS FOR ISSUE OR CANCEL   *       ECS021
02945 * ACTIVITY.                                               *       ECS021
02946 ***********************************************************       ECS021
02947                                                                   ECS021
02948  3600-SEARCH-FOR-ACTIVITY.                                        ECS021
02949      MOVE +15                      TO DTE-IDX.                    ECS021
02950      MOVE +1                       TO BEN-IDX.                    ECS021
02951      MOVE 'N'                      TO CURRENT-ACTIVITY-SW.        ECS021
02952      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
02953                                                                   ECS021
02954  3600-ACTIVITY-LOOP.                                              ECS021
02955      COMPUTE SP-ISS-CNT =                                         ECS021
02956              SW-ISS-CNT (BEN-IDX DTE-IDX) -                       ECS021
02957              SW-ISS-CNT (BEN-IDX PREV-IDX).                       ECS021
02958                                                                   ECS021
02959      IF WS-ACCT-WITH-ISSUE-ACTIVITY                               ECS021
02960          NEXT SENTENCE                                            ECS021
02961      ELSE                                                         ECS021
02962          COMPUTE SP-CNC-CNT =                                     ECS021
02963              SW-CNC-CNT (BEN-IDX DTE-IDX) -                       ECS021
02964              SW-CNC-CNT (BEN-IDX PREV-IDX).                       ECS021
02965                                                                   ECS021
02966      IF (SP-ISS-CNT NOT EQUAL ZERO OR                             ECS021
02967           SP-CNC-CNT NOT EQUAL ZERO)                              ECS021
02968              MOVE 'Y'               TO CURRENT-ACTIVITY-SW        ECS021
02969              GO TO 3600-EXIT.                                     ECS021
02970                                                                   ECS021
02971      ADD +1                        TO BEN-IDX.                    ECS021
02972                                                                   ECS021
02973      IF BEN-IDX GREATER THAN 23                                   ECS021
02974           GO TO 3600-EXIT                                         ECS021
02975      ELSE                                                         ECS021
02976           GO TO 3600-ACTIVITY-LOOP.                               ECS021
02977                                                                   ECS021
02978  3600-EXIT.                                                       ECS021
02979      EXIT.                                                        ECS021
02980                                                                   ECS021
02981      EJECT                                                        ECS021
02982  3700-SEARCH-FOR-ACTIVITY.                                        ECS021
02983      MOVE +15                      TO DTE-IDX.                    ECS021
02984      MOVE +1                       TO BEN-IDX.                    ECS021
02985      MOVE 'N'                      TO CURRENT-ACTIVITY-SW.        ECS021
02986      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
02987                                                                   ECS021
02988  3700-ACTIVITY-LOOP.                                              ECS021
02989      COMPUTE SP-ISS-CNT =                                         ECS021
02990              SW-ISS-CNT (BEN-IDX DTE-IDX) -                       ECS021
02991              SW-ISS-CNT (BEN-IDX PREV-IDX).                       ECS021
02992                                                                   ECS021
02993      IF SP-ISS-CNT GREATER THAN ZERO                              ECS021
02994          MOVE 'Y'               TO CURRENT-ACTIVITY-SW            ECS021
02995          GO TO 3700-EXIT.                                         ECS021
02996                                                                   ECS021
02997      ADD +1                        TO BEN-IDX.                    ECS021
02998                                                                   ECS021
02999      IF BEN-IDX GREATER THAN 23                                   ECS021
03000           GO TO 3700-EXIT                                         ECS021
03001      ELSE                                                         ECS021
03002           GO TO 3700-ACTIVITY-LOOP.                               ECS021
03003                                                                   ECS021
03004  3700-EXIT.                                                       ECS021
03005      EXIT.                                                        ECS021
03006                                                                   ECS021
03007      EJECT                                                        ECS021
03008 ****************************                                      ECS021
03009 *  READ ROUTINE EPEC FILE  *                                      ECS021
03010 ****************************                                      ECS021
03011                                                                   ECS021
03012  4000-READ-EPEC.                                                  ECS021
03013      READ EPEC-FILE INTO EP-RECORD AT END                         ECS021
03014          MOVE HIGH-VALUES        TO EP-RECORD                     ECS021
03015          GO TO 4000-EXIT.                                         ECS021
03016                                                                   ECS021
03017      ADD +1 TO EPECS-READ-COUNT.                                  ECS021
03018                                                                   ECS021
03019      IF EP-RECORD-ID NOT = 'EP' AND 'EC'                          ECS021
03020          ADD +1 TO NON-EP-EC-DROP-CNT                             ECS021
03021          GO TO 4000-READ-EPEC.                                    ECS021
03022                                                                   ECS021
03023      IF EP-REIN = 'R'                                             ECS021
03024          ADD +1 TO REIN-EPEC-DROP-CNT                             ECS021
03025          GO TO 4000-READ-EPEC.                                    ECS021
03026                                                                   ECS021
03027 **************************************************                ECS021
03028 *  DROP EPEC RECORDS READ THAT ARE NOT RELEVANT  *                ECS021
03029 *  TO THE PERIODS BEING REPORTED ON.             *                ECS021
03030 **************************************************                ECS021
03031                                                                   ECS021
03032      COPY ELCEPCM1.                                               ECS021
03033                                                                   ECS021
03034 **   IF EP-RUN-YR GREATER LVL15-YY                                ECS021
TSTMOD     IF EP-RUN-CCYY GREATER BRK-CCYY (15)
03035          ADD +1 TO EPEC-DATE-DROP-CNT1                            ECS021
03036          GO TO 4000-READ-EPEC.                                    ECS021
03037                                                                   ECS021
03038      IF EP-RUN-YR = LVL15-YY AND                                  ECS021
03039         EP-RUN-MO GREATER LVL15-MM                                ECS021
03040            ADD +1 TO EPEC-DATE-DROP-CNT1                          ECS021
03041            GO TO 4000-READ-EPEC.                                  ECS021
03042                                                                   ECS021
03043 **   IF EP-RUN-YR LESS LVL1-YY AND                                ECS021
TSTMOD     IF EP-RUN-CCYY LESS BRK-CCYY (1) AND
03044         EP-PURGE NOT = 'P'                                        ECS021
03045            ADD +1 TO EPEC-DATE-DROP-CNT1                          ECS021
03046            GO TO 4000-READ-EPEC.                                  ECS021
03047                                                                   ECS021
03048      IF EP-RUN-YR = LVL1-YY    AND                                ECS021
03049         EP-RUN-MO LESS LVL1-MM AND                                ECS021
03050         EP-PURGE NOT = 'P'                                        ECS021
03051            ADD +1 TO EPEC-DATE-DROP-CNT1                          ECS021
03052            GO TO 4000-READ-EPEC.                                  ECS021
03053                                                                   ECS021
03054      ADD +1 TO EPECS-SELECTED.                                    ECS021
03055                                                                   ECS021
03056      PERFORM 4100-SEARCH-STATE-TABLE   THRU 4199-EXIT.            ECS021
03057      PERFORM 4200-SEARCH-BENEFIT-TABLE THRU 4299-EXIT.            ECS021
03058                                                                   ECS021
03059  4000-EXIT.                                                       ECS021
03060      EXIT.                                                        ECS021
03061                                                                   ECS021
03062      EJECT                                                        ECS021
03063  4100-SEARCH-STATE-TABLE.                                         ECS021
03064      MOVE +1                     TO ST-IDX.                       ECS021
03065                                                                   ECS021
03066  4100-STATE-LOOP.                                                 ECS021
03067      IF ST-IDX GREATER ST-MAX-IDX                                 ECS021
03068          DISPLAY 'INVALID STATE CODE FOUND - ' EP-STATE           ECS021
03069          MOVE '**** INVALID STATE CODE FOUND ****'                ECS021
03070                                  TO WS-ABEND-MESSAGE              ECS021
03071          MOVE '0'                TO WAC-1                         ECS021
03072          MOVE '4'                TO WAC-2                         ECS021
03073          MOVE '06'               TO WAC-3-4                       ECS021
03074          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
03075          GO TO ABEND-PGM.                                         ECS021
03076                                                                   ECS021
03077      IF EP-STATE NOT = STATE-CODE (ST-IDX)                        ECS021
03078          ADD +1 TO ST-IDX                                         ECS021
03079          GO TO 4100-STATE-LOOP.                                   ECS021
03080                                                                   ECS021
03081  4199-EXIT.                                                       ECS021
03082      EXIT.                                                        ECS021
03083                                                                   ECS021
03084      EJECT                                                        ECS021
03085  4200-SEARCH-BENEFIT-TABLE.                                       ECS021
03086      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              ECS021
03087          MOVE CLAS-STARTA        TO CLAS-INDEXA                   ECS021
03088          GO TO 4200-AH-LOOP.                                      ECS021
03089                                                                   ECS021
03090      MOVE CLAS-STARTL            TO CLAS-INDEXL.                  ECS021
03091                                                                   ECS021
03092  4200-LIFE-LOOP.                                                  ECS021
03093      IF CLAS-INDEXL GREATER CLAS-MAXL OR                          ECS021
03094         CLAS-INDEXL = +0                                          ECS021
03095          DISPLAY 'INVALID LIFE BENEFIT TYPE - ' EP-BEN-CODE       ECS021
03096          MOVE '**** INVALID LIFE BENEFIT TYPE ****'               ECS021
03097                                  TO WS-ABEND-MESSAGE              ECS021
03098          MOVE '0'                TO WAC-1                         ECS021
03099          MOVE '4'                TO WAC-2                         ECS021
03100          MOVE '01'               TO WAC-3-4                       ECS021
03101          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
03102          GO TO ABEND-PGM.                                         ECS021
03103                                                                   ECS021
03104      IF EP-BEN-CODE = CLAS-I-BEN (CLAS-INDEXL)                    ECS021
03105          MOVE CLAS-I-EP (CLAS-INDEXL)                             ECS021
03106                                  TO WS-EP-CODE                    ECS021
03107          MOVE CLAS-INDEXL        TO SAVE-BEN-INDEX                ECS021
03108          GO TO 4250-SET-EP-CODE                                   ECS021
03109      ELSE                                                         ECS021
03110          ADD +1 TO CLAS-INDEXL                                    ECS021
03111          GO TO 4200-LIFE-LOOP.                                    ECS021
03112                                                                   ECS021
03113  4200-AH-LOOP.                                                    ECS021
03114      IF CLAS-INDEXA GREATER CLAS-MAXA OR                          ECS021
03115         CLAS-INDEXA = +0                                          ECS021
03116          DISPLAY 'INVALID AH BENEFIT TYPE - ' EP-BEN-CODE         ECS021
03117          MOVE '**** INVALID AH BENEFIT TYPE ****'                 ECS021
03118                                  TO WS-ABEND-MESSAGE              ECS021
03119          MOVE '0'                TO WAC-1                         ECS021
03120          MOVE '4'                TO WAC-2                         ECS021
03121          MOVE '01'               TO WAC-3-4                       ECS021
03122          MOVE WS-ABEND-CODE      TO WS-RETURN-CODE                ECS021
03123          GO TO ABEND-PGM.                                         ECS021
03124                                                                   ECS021
03125      IF EP-BEN-CODE = CLAS-I-BEN (CLAS-INDEXA)                    ECS021
03126          MOVE CLAS-I-EP (CLAS-INDEXA)                             ECS021
03127                                  TO WS-EP-CODE                    ECS021
03128          MOVE CLAS-INDEXA        TO SAVE-BEN-INDEX                ECS021
03129          GO TO 4250-SET-EP-CODE                                   ECS021
03130      ELSE                                                         ECS021
03131          ADD +1 TO CLAS-INDEXA                                    ECS021
03132          GO TO 4200-AH-LOOP.                                      ECS021
03133                                                                   ECS021
03134  4250-SET-EP-CODE.                                                ECS021
03135      IF STATE-ABBREVIATION (ST-IDX) = 'WY'                        ECS021
03136          MOVE 'P'                TO WS-EP-CODE.                   ECS021
03137                                                                   ECS021
03138  4299-EXIT.                                                       ECS021
03139      EXIT.                                                        ECS021
03140      EJECT                                                        ECS021
03141                                                                   ECS021
03142  4300-MATCH-FILES.                                                ECS021
03143      MOVE EP-CONTROL             TO RR-ACCTMSTR-CNTRL
                                          WS-HOLD-CNTL.
010809     MOVE EP-CNTRL-1             TO RR-ACCTMSTR-CNTRL-EFFDT.
010809     MOVE EP-EFF-DTE             TO RR-ACCTMSTR-EFF-DATE-EFFDT.                                          
03144                                                                   ECS021
010809     MOVE AM-CNTRL-1             TO WS-AM-MSTR-CNTRL-1-EFFDT.
010809     MOVE AM-EFFECT-DT           TO WS-AM-MSTR-EFF-DATE-EFFDT.
010809     IF WS-AM-MSTR-CNTRL-EFFDT < RR-ACCTMSTR-CNTRL-EFFDT
010809*PEMMOD     IF AM-MSTR-CNTRL < RR-ACCTMSTR-CNTRL                         ECS021
03146          PERFORM 4400-READ-ACCT THRU 4449-EXIT                    ECS021
03147          GO TO 4300-MATCH-FILES.

           IF ACCT-CONTROL-A = WS-HOLD-CNTL
              MOVE ACCT-NAME           TO WS-AM-NAME
           END-IF
           
03148      IF ACCT-CONTROL-A NOT > WS-HOLD-CNTL
              PERFORM 4450-GET-AM-NAME THRU 4499-EXIT
           END-IF
           
           .   
03149  4399-EXIT.                                                       ECS021
03150      EXIT.                                                        ECS021
03151                                                                   ECS021
03152  4400-READ-ACCT.                                                  ECS021
03153      READ ACCT-MSTR AT END                                        ECS021
03154                     MOVE HIGH-VALUES TO ACCOUNT-MASTER.           ECS021
03155                                                                   ECS021
03156      COPY ELCACCTI.                                               ECS021
03157                                                                   ECS021
03158  4449-EXIT.                                                       ECS021
03159      EXIT.                                                        ECS021
03160                                                                   ECS021
03152  4450-GET-AM-NAME.                                                ECS021
03153      READ ERACCT AT END                                           ECS021
03154                     MOVE HIGH-VALUES TO ERACCT-RECORD.            ECS021
03155                                                                   ECS021
061907     IF ACCT-COMPANY-CD > DTE-CLASIC-COMPANY-CD
061907        MOVE HIGH-VALUES         TO ERACCT-RECORD
061907     END-IF

PEMMOD     IF ACCT-CONTROL-A NOT = HIGH-VALUES
053107        IF ACCT-COMPANY-CD < EP-COMPANY-CD
053107           GO TO 4450-GET-AM-NAME
053107        END-IF
PEMMOD        IF ACCT-CONTROL-A <= WS-HOLD-CNTL
PEMMOD           MOVE ACCT-NAME         TO WS-AM-NAME
PEMMOD           GO TO 4450-GET-AM-NAME
PEMMOD        END-IF
PEMMOD     END-IF
03155                                                                   ECS021
03157      .                                                            ECS021
03158  4499-EXIT.                                                       ECS021
03159      EXIT.                                                        ECS021

03161  4500-SEARCH-GENERAL-AGENT.                                       ECS021
03162                                                                   ECS021
03163      MOVE +1                     TO GNRL-IDX                      ECS021
03164      MOVE '*NO AGENT*'           TO WS-1ST-AGENT-NUMBER.          ECS021
03165                                                                   ECS021
03166  4500-GENERAL-AGENT-LOOP.                                         ECS021
03167                                                                   ECS021
03168      IF AM-COM-TYP (GNRL-IDX) EQUAL 'O' OR 'P' OR 'G' OR 'B'      ECS021
03169          MOVE AM-AGT (GNRL-IDX)  TO WS-1ST-AGENT-NUMBER           ECS021
03170          GO TO 4599-EXIT.                                         ECS021
03171                                                                   ECS021
03172      ADD +1                      TO GNRL-IDX.                     ECS021
03173                                                                   ECS021
03174      IF GNRL-IDX LESS THAN +11                                    ECS021
03175          GO TO 4500-GENERAL-AGENT-LOOP.                           ECS021
03176                                                                   ECS021
03177  4599-EXIT.                                                       ECS021
03178      EXIT.                                                        ECS021
03179                                                                   ECS021
03180      EJECT                                                        ECS021
03181  4999-END-SELECT-EPEC-RECS.                                       ECS021
03182      CLOSE EPEC-FILE                                              ECS021
03183            ACCT-MSTR ERACCT.                                      ECS021
03184                                                                   ECS021
03185      CLOSE ELCNTL.                                                ECS021
03186                                                                   ECS021
03187      IF ELCNTL-FILE-STATUS NOT = '00'                             ECS021
03188          MOVE '**** BAD CLOSE CONTROL FILE   ****'                ECS021
03189                              TO WS-ABEND-MESSAGE                  ECS021
03190          MOVE '2'            TO WAC-1                             ECS021
03191          MOVE '2'            TO WAC-2                             ECS021
03192          MOVE ELCNTL-FILE-STATUS TO WAC-3-4                       ECS021
03193          MOVE WS-ABEND-CODE  TO WS-RETURN-CODE                    ECS021
03194          GO TO ABEND-PGM.                                         ECS021
03195                                                                   ECS021
03196  4999-EXIT.                                                       ECS021
03197      EXIT.                                                        ECS021
03198                                                                   ECS021
03199      EJECT                                                        ECS021
03200 ******************************************************            ECS021
03201 *  (5000) ROUTINES ARE THE BEGINING OF THE SORT      *            ECS021
03202 *  OUTPUT PROCEDURE WHICH WILL PRODUCE THE REPORT    *            ECS021
03203 ******************************************************            ECS021
03204                                                                   ECS021
03205  5000-PRINT-STATEMENT SECTION.                                    ECS021
03206                                                                   ECS021
03207      PERFORM 9040-RETURN-SW-FILE THRU 9040-EXIT.                  ECS021
03208      MOVE SW-REPORT-CONTROL-KEY  TO SAVE-CONTROL-BREAK.           ECS021
03209                                                                   ECS021
03210  5010-REPORT-LOOP-ONE.                                            ECS021
03211      PERFORM 6000-CHECK-CONTROL-BREAK THRU 6999-EXIT.             ECS021
03212                                                                   ECS021
03213      IF SW-REPORT-CONTROL-KEY = HIGH-VALUES                       ECS021
03214          GO TO 5099-EXIT.                                         ECS021
03215                                                                   ECS021
102908     MOVE SW-CARRIER             TO WS-SAVE-CARRIER.
03216      MOVE SW-ACCT-NAME           TO WS-SAVE-ACCT-NAME.            ECS021
121707     MOVE SW-ACCT-STATUS         TO WS-SAVE-ACCT-STATUS           
03217      MOVE SW-PROD-DATE           TO WS-SAVE-PROD-DATE.            ECS021
03218      MOVE SW-GA-NAME             TO WS-SAVE-GA-NAME.              ECS021
03219      MOVE SW-STATE-NAME          TO WS-SAVE-STATE-NAME.           ECS021
102908     MOVE SW-STATE-ABBREVIATION  TO WS-SAVE-STATE-ABBREVIATION.
03220      MOVE SW-REPORT-CONTROL-KEY  TO SAVE-CONTROL-BREAK.           ECS021
03221                                                                   ECS021
03222      IF REPORT-HAS-SIX-BREAKS                                     ECS021
03223          MOVE 1                   TO NUMBER-OF-REQUESTS              CL**8
03224          MOVE W-APPLY-SORT-RCD    TO PROCESSING-REQUEST (1)          CL**9
03225          PERFORM 7600-CALL-SUB-MODULE-SIX THRU 7600-EXIT          ECS021
03226          GO TO 5020-RETURN-SORT-RECORD.                           ECS021
03227                                                                   ECS021
03228      IF REPORT-HAS-FIVE-BREAKS                                    ECS021
03229          MOVE 1                   TO NUMBER-OF-REQUESTS              CL**8
03230          MOVE W-APPLY-SORT-RCD    TO PROCESSING-REQUEST (1)          CL**9
03231          PERFORM 7500-CALL-SUB-MODULE-FIVE THRU 7500-EXIT         ECS021
03232          GO TO 5020-RETURN-SORT-RECORD.                           ECS021
03233                                                                   ECS021
03234      IF REPORT-HAS-FOUR-BREAKS                                    ECS021
03235          MOVE 1                   TO NUMBER-OF-REQUESTS              CL**8
03236          MOVE W-APPLY-SORT-RCD    TO PROCESSING-REQUEST (1)          CL**9
03237          PERFORM 7400-CALL-SUB-MODULE-FOUR THRU 7400-EXIT         ECS021
03238          GO TO 5020-RETURN-SORT-RECORD.                           ECS021
03239                                                                   ECS021
03240      IF REPORT-HAS-THREE-BREAKS                                   ECS021
03241          MOVE 1                   TO NUMBER-OF-REQUESTS              CL**8
03242          MOVE W-APPLY-SORT-RCD    TO PROCESSING-REQUEST (1)          CL**9
03243          PERFORM 7300-CALL-SUB-MODULE-THREE THRU 7300-EXIT        ECS021
03244          GO TO 5020-RETURN-SORT-RECORD.                           ECS021
03245                                                                   ECS021
03246      IF REPORT-HAS-TWO-BREAKS                                     ECS021
03247          MOVE 1                   TO NUMBER-OF-REQUESTS              CL**8
03248          MOVE W-APPLY-SORT-RCD    TO PROCESSING-REQUEST (1)          CL**9
03249          PERFORM 7200-CALL-SUB-MODULE-TWO THRU 7200-EXIT          ECS021
03250          GO TO 5020-RETURN-SORT-RECORD.                           ECS021
03251                                                                   ECS021
03252      IF REPORT-HAS-ONE-BREAK                                      ECS021
03253          MOVE 1                   TO NUMBER-OF-REQUESTS              CL**8
03254          MOVE W-APPLY-SORT-RCD    TO PROCESSING-REQUEST (1)          CL**9
03255          PERFORM 7100-CALL-SUB-MODULE-ONE THRU 7100-EXIT.         ECS021
03256                                                                   ECS021
03257  5020-RETURN-SORT-RECORD.                                         ECS021
03258                                                                   ECS021
03259      PERFORM 9040-RETURN-SW-FILE THRU 9040-EXIT.                  ECS021
03260                                                                   ECS021
03261      GO TO 5010-REPORT-LOOP-ONE.                                  ECS021
03262                                                                   ECS021
03263 ****************************************                          ECS021
03264 *  END OUTPUT PROCEDURE MAINLINE LOOP  *                          ECS021
03265 ****************************************                          ECS021
03266                                                                   ECS021
03267  5099-EXIT.                                                       ECS021
03268      EXIT.                                                        ECS021
03269                                                                   ECS021
03270      EJECT                                                        ECS021
03271 ********************************************************          ECS021
03272 *  6000 ROUTINE DETERMINES IF A BREAK HAS OCCURRED IN  *          ECS021
03273 *  THE REPORT CONTROL BREAKS.                          *          ECS021
03274 ********************************************************          ECS021
03275                                                                   ECS021
03276  6000-CHECK-CONTROL-BREAK.                                        ECS021
03277 ****  CHECK FOR CONTROL LEVEL ONE BREAK  ****                     ECS021
03278      IF SW-BREAK-FIELD-1 NOT = SAVE-BREAK-1                       ECS021
03279          PERFORM 6600-PRINT-BREAK-SIX   THRU 6699-EXIT            ECS021
03280          PERFORM 6500-PRINT-BREAK-FIVE  THRU 6599-EXIT            ECS021
03281          PERFORM 6400-PRINT-BREAK-FOUR  THRU 6499-EXIT            ECS021
03282          PERFORM 6300-PRINT-BREAK-THREE THRU 6399-EXIT            ECS021
03283          PERFORM 6200-PRINT-BREAK-TWO   THRU 6299-EXIT            ECS021
03284          PERFORM 6100-PRINT-BREAK-ONE   THRU 6199-EXIT            ECS021
03285          GO TO 6999-EXIT.                                         ECS021
03286                                                                   ECS021
03287 ****  CHECK FOR CONTROL LEVEL TWO BREAK  ****                        CL*10
03288      IF SW-BREAK-FIELD-2 NOT = SAVE-BREAK-2                       ECS021
03289          PERFORM 6600-PRINT-BREAK-SIX   THRU 6699-EXIT            ECS021
03290          PERFORM 6500-PRINT-BREAK-FIVE  THRU 6599-EXIT            ECS021
03291          PERFORM 6400-PRINT-BREAK-FOUR  THRU 6499-EXIT            ECS021
03292          PERFORM 6300-PRINT-BREAK-THREE THRU 6399-EXIT            ECS021
03293          PERFORM 6200-PRINT-BREAK-TWO   THRU 6299-EXIT            ECS021
03294          GO TO 6999-EXIT.                                         ECS021
03295                                                                   ECS021
03296 ****  CHECK FOR CONTROL LEVEL THREE BREAK  ****                   ECS021
03297      IF SW-BREAK-FIELD-3 NOT = SAVE-BREAK-3                       ECS021
03298          PERFORM 6600-PRINT-BREAK-SIX   THRU 6699-EXIT            ECS021
03299          PERFORM 6500-PRINT-BREAK-FIVE  THRU 6599-EXIT            ECS021
03300          PERFORM 6400-PRINT-BREAK-FOUR  THRU 6499-EXIT            ECS021
03301          PERFORM 6300-PRINT-BREAK-THREE THRU 6399-EXIT            ECS021
03302          GO TO 6999-EXIT.                                         ECS021
03303                                                                   ECS021
03304 ****  CHECK FOR CONTROL LEVEL FOUR BREAK  ****                       CL**8
03305      IF SW-BREAK-FIELD-4 NOT = SAVE-BREAK-4                       ECS021
03306          PERFORM 6600-PRINT-BREAK-SIX  THRU 6699-EXIT             ECS021
03307          PERFORM 6500-PRINT-BREAK-FIVE THRU 6599-EXIT             ECS021
03308          PERFORM 6400-PRINT-BREAK-FOUR THRU 6499-EXIT             ECS021
03309          GO TO 6999-EXIT.                                         ECS021
03310                                                                   ECS021
03311 ****  CHECK FOR CONTROL LEVEL FIVE BREAK  ****                       CL**8
03312      IF SW-BREAK-FIELD-5 NOT = SAVE-BREAK-5                       ECS021
03313          PERFORM 6600-PRINT-BREAK-SIX  THRU 6699-EXIT             ECS021
03314          PERFORM 6500-PRINT-BREAK-FIVE THRU 6599-EXIT             ECS021
03315          GO TO 6999-EXIT.                                         ECS021
03316                                                                   ECS021
03317 ****  CHECK FOR CONTROL LEVEL SIX BREAK  ****                     ECS021
03318      IF SW-BREAK-FIELD-6 NOT = SAVE-BREAK-6                       ECS021
03319          PERFORM 6600-PRINT-BREAK-SIX THRU 6699-EXIT.             ECS021
03320                                                                   ECS021
03321  6999-EXIT.                                                       ECS021
03322      EXIT.                                                        ECS021
03323                                                                   ECS021
03324      EJECT                                                        ECS021
03325  6100-PRINT-BREAK-ONE.                                            ECS021
03326                                                                   ECS021
03327      MOVE 1                       TO HEADING-SWITCH.                 CL**8
03328                                                                   ECS021
03329      MOVE 2                       TO NUMBER-OF-REQUESTS.             CL**8
03330      MOVE W-MOVE-TO-LINK          TO PROCESSING-REQUEST (1).         CL**9
03331      MOVE W-ZERO-TABLE            TO PROCESSING-REQUEST (2).         CL**9
03332      PERFORM 7100-CALL-SUB-MODULE-ONE THRU 7100-EXIT.             ECS021
03333      PERFORM 6700-COMPUTE-REPORT-FIGURES THRU 6700-EXIT.          ECS021
102908
102908     IF WS-REPORT-NO = '307' AND DTE-FMT-OPT = 3
102908         PERFORM 7900-WRITE-DATA-OUT THRU 7900-EXIT
102908     END-IF.
03334                                                                   ECS021
03335      IF CF-ACCOUNT-OPT-SEQ EQUAL 1                                ECS021
03336         IF CF-EXCEPTION-LIST-REQUESTED                            ECS021
03337             PERFORM 6900-CALC-TOTAL-EXCEPTIONS THRU 6900-EXIT.    ECS021
03338                                                                   ECS021
03339  6199-EXIT.                                                       ECS021
03340      EXIT.                                                        ECS021
03341                                                                   ECS021
03342  6200-PRINT-BREAK-TWO.                                            ECS021
03343                                                                   ECS021
03344      IF WS-NUMBER-OF-BREAKS LESS THAN 2                              CL**8
03345          GO TO 6299-EXIT.                                         ECS021
03346                                                                   ECS021
03347      MOVE 2                       TO HEADING-SWITCH.                 CL**8
03348                                                                   ECS021
03349      MOVE 2                       TO NUMBER-OF-REQUESTS.             CL**8
03350      MOVE W-MOVE-TO-LINK          TO PROCESSING-REQUEST (1).         CL**9
03351      MOVE W-ZERO-TABLE            TO PROCESSING-REQUEST (2).         CL**9
03352      PERFORM 7200-CALL-SUB-MODULE-TWO THRU 7200-EXIT.             ECS021
03353      PERFORM 6700-COMPUTE-REPORT-FIGURES THRU 6700-EXIT.          ECS021
03354                                                                   ECS021
03355      IF CF-ACCOUNT-OPT-SEQ EQUAL 2                                ECS021
03356         IF CF-EXCEPTION-LIST-REQUESTED                            ECS021
03357             PERFORM 6900-CALC-TOTAL-EXCEPTIONS THRU 6900-EXIT.    ECS021
03358                                                                   ECS021
03359      MOVE 1                       TO NUMBER-OF-REQUESTS.             CL**8
03360      MOVE W-ADD-LINK-TABLES       TO PROCESSING-REQUEST (1).         CL**9
03361      PERFORM 7100-CALL-SUB-MODULE-ONE THRU 7100-EXIT.             ECS021
03362                                                                   ECS021
03363  6299-EXIT.                                                       ECS021
03364      EXIT.                                                        ECS021
03365                                                                   ECS021
03366  6300-PRINT-BREAK-THREE.                                          ECS021
03367                                                                   ECS021
03368      IF WS-NUMBER-OF-BREAKS LESS THAN 3                              CL**8
03369          GO TO 6399-EXIT.                                         ECS021
03370                                                                   ECS021
03371      MOVE 3                       TO HEADING-SWITCH.                 CL**8
03372                                                                   ECS021
03373      MOVE 2                       TO NUMBER-OF-REQUESTS.             CL**8
03374      MOVE W-MOVE-TO-LINK          TO PROCESSING-REQUEST (1).         CL**9
03375      MOVE W-ZERO-TABLE            TO PROCESSING-REQUEST (2).         CL**9
03376      PERFORM 7300-CALL-SUB-MODULE-THREE THRU 7300-EXIT.           ECS021
03377      PERFORM 6700-COMPUTE-REPORT-FIGURES THRU 6700-EXIT.          ECS021
03378                                                                   ECS021
03379      IF CF-ACCOUNT-OPT-SEQ EQUAL 3                                ECS021
03380         IF CF-EXCEPTION-LIST-REQUESTED                            ECS021
03381             PERFORM 6900-CALC-TOTAL-EXCEPTIONS THRU 6900-EXIT.    ECS021
03382                                                                   ECS021
03383      MOVE 1                       TO NUMBER-OF-REQUESTS.             CL**8
03384      MOVE W-ADD-LINK-TABLES       TO PROCESSING-REQUEST (1).         CL**9
03385      PERFORM 7200-CALL-SUB-MODULE-TWO THRU 7200-EXIT.             ECS021
03386                                                                   ECS021
03387  6399-EXIT.                                                       ECS021
03388      EXIT.                                                        ECS021
03389      EJECT                                                        ECS021
03390  6400-PRINT-BREAK-FOUR.                                           ECS021
03391                                                                   ECS021
03392      IF WS-NUMBER-OF-BREAKS LESS THAN 4                              CL**8
03393          GO TO 6499-EXIT.                                         ECS021
03394                                                                   ECS021
03395      MOVE 4                       TO HEADING-SWITCH.                 CL**8
03396                                                                   ECS021
03397      MOVE 2                       TO NUMBER-OF-REQUESTS.             CL**8
03398      MOVE W-MOVE-TO-LINK          TO PROCESSING-REQUEST (1).         CL**9
03399      MOVE W-ZERO-TABLE            TO PROCESSING-REQUEST (2).         CL**9
03400      PERFORM 7400-CALL-SUB-MODULE-FOUR THRU 7400-EXIT.            ECS021
03401      PERFORM 6700-COMPUTE-REPORT-FIGURES THRU 6700-EXIT.          ECS021
03402                                                                   ECS021
03403      IF CF-ACCOUNT-OPT-SEQ EQUAL 4                                ECS021
03404         IF CF-EXCEPTION-LIST-REQUESTED                            ECS021
03405             PERFORM 6900-CALC-TOTAL-EXCEPTIONS THRU 6900-EXIT.    ECS021
03406                                                                   ECS021
03407      MOVE 1                       TO NUMBER-OF-REQUESTS.             CL**8
03408      MOVE W-ADD-LINK-TABLES       TO PROCESSING-REQUEST (1).         CL**9
03409      PERFORM 7300-CALL-SUB-MODULE-THREE THRU 7300-EXIT.           ECS021
03410                                                                   ECS021
03411  6499-EXIT.                                                       ECS021
03412      EXIT.                                                        ECS021
03413                                                                   ECS021
03414  6500-PRINT-BREAK-FIVE.                                           ECS021
03415                                                                   ECS021
03416      IF WS-NUMBER-OF-BREAKS LESS THAN 5                              CL**8
03417          GO TO 6599-EXIT.                                         ECS021
03418                                                                   ECS021
03419      MOVE 5                       TO HEADING-SWITCH.                 CL**8
03420                                                                   ECS021
03421      MOVE 2                       TO NUMBER-OF-REQUESTS.             CL**8
03422      MOVE W-MOVE-TO-LINK          TO PROCESSING-REQUEST (1).         CL**9
03423      MOVE W-ZERO-TABLE            TO PROCESSING-REQUEST (2).         CL**9
03424      PERFORM 7500-CALL-SUB-MODULE-FIVE THRU 7500-EXIT.            ECS021
03425      PERFORM 6700-COMPUTE-REPORT-FIGURES THRU 6700-EXIT.          ECS021
03426                                                                   ECS021
03427      IF CF-ACCOUNT-OPT-SEQ EQUAL 5                                ECS021
03428         IF CF-EXCEPTION-LIST-REQUESTED                            ECS021
03429             PERFORM 6900-CALC-TOTAL-EXCEPTIONS THRU 6900-EXIT.    ECS021
03430                                                                   ECS021
03431      MOVE 1                       TO NUMBER-OF-REQUESTS.             CL**8
03432      MOVE W-ADD-LINK-TABLES       TO PROCESSING-REQUEST (1).         CL**9
03433      PERFORM 7400-CALL-SUB-MODULE-FOUR THRU 7400-EXIT.            ECS021
03434                                                                   ECS021
03435  6599-EXIT.                                                       ECS021
03436      EXIT.                                                        ECS021
03437                                                                   ECS021
03438  6600-PRINT-BREAK-SIX.                                            ECS021
03439                                                                   ECS021
03440      IF WS-NUMBER-OF-BREAKS LESS THAN 6                              CL**8
03441          GO TO 6699-EXIT.                                         ECS021
03442                                                                   ECS021
03443      MOVE 6                       TO HEADING-SWITCH.                 CL**8
03444                                                                   ECS021
03445      MOVE 2                       TO NUMBER-OF-REQUESTS.             CL**8
03446      MOVE W-MOVE-TO-LINK          TO PROCESSING-REQUEST (1).         CL**9
03447      MOVE W-ZERO-TABLE            TO PROCESSING-REQUEST (2).         CL**9
03448      PERFORM 7600-CALL-SUB-MODULE-SIX THRU 7600-EXIT.             ECS021
03449      PERFORM 6700-COMPUTE-REPORT-FIGURES THRU 6700-EXIT.          ECS021
03450                                                                   ECS021
03451      IF CF-ACCOUNT-OPT-SEQ EQUAL 6                                ECS021
03452         IF CF-EXCEPTION-LIST-REQUESTED                            ECS021
03453             PERFORM 6900-CALC-TOTAL-EXCEPTIONS THRU 6900-EXIT.    ECS021
03454                                                                   ECS021
03455      MOVE 1                       TO NUMBER-OF-REQUESTS.             CL**8
03456      MOVE W-ADD-LINK-TABLES       TO PROCESSING-REQUEST (1).         CL**9
03457      PERFORM 7500-CALL-SUB-MODULE-FIVE THRU 7500-EXIT.            ECS021
03458                                                                   ECS021
03459  6699-EXIT.                                                       ECS021
03460      EXIT.                                                        ECS021
03461                                                                   ECS021
03462      EJECT                                                        ECS021
03463 ********************************************************          ECS021
03464 *  COMMON ROUTINE USED TO COMPUTE REPORT FIGURES FROM  *          ECS021
03465 *  ACCUMULATED EPEC NUMBERS.                           *          ECS021
03466 *                                                      *          ECS021
03467 *  THIS SECTION PRODUCES LIFE INDIVIDUAL NUMBERS       *          ECS021
03468 ********************************************************          ECS021
03469                                                                   ECS021
03470  6700-COMPUTE-REPORT-FIGURES.                                     ECS021
03471      MOVE +0                     TO LIFE-BENEFIT-COUNT.           ECS021
03472      MOVE +1                     TO BEN-IDX.                      ECS021
03473                                                                   ECS021
03474  6700-LIFE-BENEFIT-LOOP.                                          ECS021
03475      IF BEN-IDX GREATER CLAS-MAXL                                 ECS021
03476          MOVE +1                 TO BEN-IDX                       ECS021
03477          GO TO 6700-TOTAL-LIFE-BENEFIT.                           ECS021
03478                                                                   ECS021
03479      IF CMMN-BENEFIT-CODE (BEN-IDX) = ZEROS                       ECS021
03480          ADD +1 TO BEN-IDX                                        ECS021
03481          GO TO 6700-LIFE-BENEFIT-LOOP.                            ECS021
03482                                                                   ECS021
03483      ADD +1 TO LIFE-BENEFIT-COUNT.                                ECS021
03484                                                                   ECS021
03485      MOVE 'LI'                   TO TOTAL-TYPE-SWITCH.            ECS021
03486                                                                   ECS021
102908     IF WS-REPORT-NO = '307' AND DTE-FMT-OPT = 3
102908         GO TO 6700-SKIP-LF-AVERAGE-INFO
102908     END-IF.
102908
03487      IF DTE-FMT-OPT = 1 OR 5                                      ECS021
03488          GO TO 6700-SKIP-LF-AVERAGE-INFO.                         ECS021
03489                                                                   ECS021
03490      IF DTE-FMT-OPT = 2                                           ECS021
03491          PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT              ECS021
03492          GO TO 6700-SKIP-LF-DETAIL-BENEFIT.                       ECS021
03493                                                                   ECS021
03494      PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT.                 ECS021
03495                                                                   ECS021
03496      MOVE SPACES                 TO X.                            ECS021
03497                                                                   ECS021
03498      PERFORM 6750-COMPUTE-MONTH-FIGURES THRU 6750-EXIT            ECS021
03499          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03500            UNTIL DTE-IDX GREATER +15.                             ECS021
03501                                                                   ECS021
03502  6700-LF-LAST-12-MONTH.                                           ECS021
03503      MOVE '0'                    TO X.                            ECS021
03504      MOVE +15                    TO DTE-IDX.                      ECS021
03505      MOVE +3                     TO PREV-IDX.                     ECS021
03506      MOVE '1'                    TO BREAK-SWITCH.                 ECS021
03507      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03508                                                                   ECS021
03509  6700-LF-PREV-12-MONTH.                                           ECS021
03510      MOVE SPACES                 TO X.                            ECS021
03511      MOVE +3                     TO DTE-IDX.                      ECS021
03512      MOVE +1                     TO PREV-IDX.                     ECS021
03513      MOVE '2'                    TO BREAK-SWITCH.                 ECS021
03514      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03515                                                                   ECS021
03516  6700-LF-YEAR-TO-DATE.                                            ECS021
03517      MOVE '0'                    TO X.                            ECS021
03518      MOVE +15                    TO DTE-IDX.                      ECS021
03519                                                                   ECS021
03520      COMPUTE PREV-IDX = DTE-IDX - BRK-MM (DTE-IDX).               ECS021
03521                                                                   ECS021
03522      MOVE '3'                    TO BREAK-SWITCH.                 ECS021
03523      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03524                                                                   ECS021
03525  6700-LF-PREV-YTD.                                                ECS021
03526      MOVE SPACES                 TO X.                            ECS021
03527      MOVE +3                     TO DTE-IDX.                      ECS021
03528      MOVE +2                     TO PREV-IDX.                     ECS021
03529      MOVE '4'                    TO BREAK-SWITCH.                 ECS021
03530      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03531                                                                   ECS021
03532  6700-LF-ITD.                                                     ECS021
03533      MOVE '0'                    TO X.                            ECS021
03534      PERFORM 6755-PRODUCE-ITD-FIGURES THRU 6755-EXIT.             ECS021
03535                                                                   ECS021
03536  6700-SKIP-LF-DETAIL-BENEFIT.                                     ECS021
03537      IF DTE-FMT-OPT = 3                                           ECS021
03538          GO TO 6700-SKIP-LF-AVERAGE-INFO.                         ECS021
03539                                                                   ECS021
03540      PERFORM 9000-AVERAGE-INFO-HEADINGS THRU 9010-EXIT.           ECS021
03541                                                                   ECS021
03542      MOVE ZERO-ACCUM-AVG-CNTRS   TO ACCUM-AVG-CNTRS.              ECS021
03543                                                                   ECS021
03544      PERFORM 6770-COMPUTE-AVERAGE-FIGURES THRU 6770-EXIT          ECS021
03545          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03546            UNTIL DTE-IDX GREATER +15.                             ECS021
03547                                                                   ECS021
03548  6700-SKIP-LF-AVERAGE-INFO.                                       ECS021
03549      ADD +1                      TO BEN-IDX.                      ECS021
03550      GO TO 6700-LIFE-BENEFIT-LOOP.                                ECS021
03551                                                                   ECS021
03552      EJECT                                                        ECS021
03553 **********************************************                    ECS021
03554 *  THIS SECTION PRODUCES LIFE TOTAL NUMBERS  *                    ECS021
03555 **********************************************                    ECS021
03556                                                                   ECS021
03557  6700-TOTAL-LIFE-BENEFIT.                                         ECS021
03558                                                                   ECS021
03559      IF DTE-FMT-OPT = 5                                           ECS021
03560          MOVE +0                 TO AH-BENEFIT-COUNT              ECS021
03561          MOVE CLAS-STARTA        TO BEN-IDX                       ECS021
03562          GO TO 6700-AH-BENEFIT-LOOP.                              ECS021
03563                                                                   ECS021
03564      IF NO-LIFE-BENEFITS                                          ECS021
03565          MOVE +0                 TO AH-BENEFIT-COUNT              ECS021
03566          MOVE CLAS-STARTA        TO BEN-IDX                       ECS021
03567          GO TO 6700-AH-BENEFIT-LOOP.                              ECS021
03568                                                                   ECS021
03569      MOVE 'LT'                   TO TOTAL-TYPE-SWITCH.            ECS021
03570                                                                   ECS021
03571      IF DTE-FMT-OPT = 2                                           ECS021
03572          PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT              ECS021
03573          GO TO 6700-SKIP-LF-DETAIL-TOTAL.                         ECS021
03574                                                                   ECS021
03575      PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT.                 ECS021
03576                                                                   ECS021
03577      MOVE SPACES                 TO X.                            ECS021
03578                                                                   ECS021
03579      PERFORM 6760-COMPUTE-TOTAL-FIGURES THRU 6760-EXIT            ECS021
03580          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03581            UNTIL DTE-IDX GREATER +15.                             ECS021
03582                                                                   ECS021
03583  6700-LF-LAST-12-TOTAL.                                           ECS021
03584      MOVE '0'                    TO X.                            ECS021
03585      MOVE +15                    TO DTE-IDX.                      ECS021
03586      MOVE +3                     TO PREV-IDX.                     ECS021
03587      MOVE '1'                    TO BREAK-SWITCH.                 ECS021
03588      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03589                                                                   ECS021
03590  6700-LF-PREV-12-TOTAL.                                           ECS021
03591      MOVE SPACES                 TO X.                            ECS021
03592      MOVE +3                     TO DTE-IDX.                      ECS021
03593      MOVE +1                     TO PREV-IDX.                     ECS021
03594      MOVE '2'                    TO BREAK-SWITCH.                 ECS021
03595      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03596                                                                   ECS021
03597  6700-LF-YEAR-TO-DATE-TOTAL.                                      ECS021
03598      MOVE '0'                    TO X.                            ECS021
03599      MOVE +15                    TO DTE-IDX.                      ECS021
03600                                                                   ECS021
03601      COMPUTE PREV-IDX = DTE-IDX - BRK-MM (DTE-IDX).               ECS021
03602                                                                   ECS021
03603      MOVE '3'                    TO BREAK-SWITCH.                 ECS021
03604      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03605                                                                   ECS021
03606  6700-LF-PREV-YTD-TOTAL.                                          ECS021
03607      MOVE SPACES                 TO X.                            ECS021
03608      MOVE +3                     TO DTE-IDX.                      ECS021
03609      MOVE +2                     TO PREV-IDX.                     ECS021
03610      MOVE '4'                    TO BREAK-SWITCH.                 ECS021
03611      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03612                                                                   ECS021
03613  6700-LF-ITD-TOTAL.                                               ECS021
03614      MOVE '0'                    TO X.                            ECS021
03615      PERFORM 6765-TOTAL-ITD-FIGURES THRU 6765-EXIT.               ECS021
03616                                                                   ECS021
03617  6700-SKIP-LF-DETAIL-TOTAL.                                       ECS021
03618      IF DTE-FMT-OPT = '3'                                         ECS021
03619          GO TO 6700-SKIP-LF-AVE-TOTAL.                            ECS021
03620                                                                   ECS021
03621      PERFORM 9000-AVERAGE-INFO-HEADINGS THRU 9010-EXIT.           ECS021
03622                                                                   ECS021
03623      MOVE ZERO-ACCUM-AVG-CNTRS   TO ACCUM-AVG-CNTRS.              ECS021
03624                                                                   ECS021
03625      MOVE SPACES                 TO X.                            ECS021
03626                                                                   ECS021
03627      PERFORM 6780-COMPUTE-TOTAL-AVERAGES THRU 6780-EXIT           ECS021
03628          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03629            UNTIL DTE-IDX GREATER +15.                             ECS021
03630                                                                   ECS021
03631  6700-SKIP-LF-AVE-TOTAL.                                          ECS021
03632      EJECT                                                        ECS021
03633 *************************************************                 ECS021
03634 *  THIS SECTION PRODUCES AH INDIVIDUAL NUMBERS  *                 ECS021
03635 *************************************************                 ECS021
03636                                                                   ECS021
03637      MOVE +0                     TO AH-BENEFIT-COUNT.             ECS021
03638      MOVE CLAS-STARTA            TO BEN-IDX.                      ECS021
03639                                                                   ECS021
03640  6700-AH-BENEFIT-LOOP.                                            ECS021
03641      IF BEN-IDX GREATER CLAS-MAXA                                 ECS021
03642          MOVE +2                 TO BEN-IDX                       ECS021
03643          GO TO 6700-TOTAL-AH-BENEFIT.                             ECS021
03644                                                                   ECS021
03645      IF CMMN-BENEFIT-CODE (BEN-IDX) = ZEROS                       ECS021
03646          ADD +1 TO BEN-IDX                                        ECS021
03647          GO TO 6700-AH-BENEFIT-LOOP.                              ECS021
03648                                                                   ECS021
03649      ADD +1 TO AH-BENEFIT-COUNT.                                  ECS021
03650                                                                   ECS021
03651      MOVE 'AI'                   TO TOTAL-TYPE-SWITCH.            ECS021
03652                                                                   ECS021
102908     IF WS-REPORT-NO = '307' AND DTE-FMT-OPT = 3
102908         GO TO 6700-SKIP-AH-AVERAGE-INFO
102908     END-IF.
102908
03653      IF DTE-FMT-OPT = 1 OR 5                                      ECS021
03654          GO TO 6700-SKIP-AH-AVERAGE-INFO.                         ECS021
03655                                                                   ECS021
03656      IF DTE-FMT-OPT = 2                                           ECS021
03657          PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT              ECS021
03658          GO TO 6700-SKIP-DETAIL-AH-BENEFIT.                       ECS021
03659                                                                   ECS021
03660      PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT.                 ECS021
03661                                                                   ECS021
03662      MOVE SPACES                 TO X.                            ECS021
03663                                                                   ECS021
03664      PERFORM 6750-COMPUTE-MONTH-FIGURES THRU 6750-EXIT            ECS021
03665          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03666            UNTIL DTE-IDX GREATER +15.                             ECS021
03667                                                                   ECS021
03668  6700-AH-LAST-12-MONTH.                                           ECS021
03669      MOVE '0'                    TO X.                            ECS021
03670      MOVE +15                    TO DTE-IDX.                      ECS021
03671      MOVE +3                     TO PREV-IDX.                     ECS021
03672      MOVE '1'                    TO BREAK-SWITCH.                 ECS021
03673      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03674                                                                   ECS021
03675  6700-AH-PREV-12-MONTH.                                           ECS021
03676      MOVE SPACES                 TO X.                            ECS021
03677      MOVE +3                     TO DTE-IDX.                      ECS021
03678      MOVE +1                     TO PREV-IDX.                     ECS021
03679      MOVE '2'                    TO BREAK-SWITCH.                 ECS021
03680      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03681                                                                   ECS021
03682  6700-AH-YEAR-TO-DATE.                                            ECS021
03683      MOVE '0'                    TO X.                            ECS021
03684      MOVE +15                    TO DTE-IDX.                      ECS021
03685                                                                   ECS021
03686      COMPUTE PREV-IDX = DTE-IDX - BRK-MM (DTE-IDX).               ECS021
03687                                                                   ECS021
03688      MOVE '3'                    TO BREAK-SWITCH.                 ECS021
03689      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03690                                                                   ECS021
03691  6700-AH-PREV-YTD.                                                ECS021
03692      MOVE SPACES                 TO X.                            ECS021
03693      MOVE +3                     TO DTE-IDX.                      ECS021
03694      MOVE +2                     TO PREV-IDX.                     ECS021
03695      MOVE '4'                    TO BREAK-SWITCH.                 ECS021
03696      PERFORM 6750-SPECIAL-INTERVAL-FIGURES THRU 6750-EXIT.        ECS021
03697                                                                   ECS021
03698  6700-AH-ITD.                                                     ECS021
03699      MOVE '0'                    TO X.                            ECS021
03700      PERFORM 6755-PRODUCE-ITD-FIGURES THRU 6755-EXIT.             ECS021
03701                                                                   ECS021
03702  6700-SKIP-DETAIL-AH-BENEFIT.                                     ECS021
03703      IF DTE-FMT-OPT = 3                                           ECS021
03704          GO TO 6700-SKIP-AH-AVERAGE-INFO.                         ECS021
03705                                                                   ECS021
03706      PERFORM 9000-AVERAGE-INFO-HEADINGS THRU 9010-EXIT.           ECS021
03707                                                                   ECS021
03708      MOVE ZERO-ACCUM-AVG-CNTRS   TO ACCUM-AVG-CNTRS.              ECS021
03709                                                                   ECS021
03710      PERFORM 6770-COMPUTE-AVERAGE-FIGURES THRU 6770-EXIT          ECS021
03711          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03712            UNTIL DTE-IDX GREATER +15.                             ECS021
03713                                                                   ECS021
03714  6700-SKIP-AH-AVERAGE-INFO.                                       ECS021
03715      ADD +1                      TO BEN-IDX.                      ECS021
03716      GO TO 6700-AH-BENEFIT-LOOP.                                  ECS021
03717                                                                   ECS021
03718      EJECT                                                        ECS021
03719 ********************************************                      ECS021
03720 *  THIS SECTION PRODUCES AH TOTAL NUMBERS  *                      ECS021
03721 ********************************************                      ECS021
03722                                                                   ECS021
03723  6700-TOTAL-AH-BENEFIT.                                           ECS021
03724                                                                   ECS021
03725      IF DTE-FMT-OPT = 5                                           ECS021
03726          GO TO 6700-TOTAL-BOTH-BENEFITS.                          ECS021
03727                                                                   ECS021
03728      IF NO-AH-BENEFITS                                            ECS021
03729          GO TO 6700-TOTAL-BOTH-BENEFITS.                          ECS021
03730                                                                   ECS021
03731      MOVE 'AT'                   TO TOTAL-TYPE-SWITCH.            ECS021
03732                                                                   ECS021
03733      IF DTE-FMT-OPT = 2                                           ECS021
03734          PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT              ECS021
03735          GO TO 6700-SKIP-AH-DETAIL-TOTAL.                         ECS021
03736                                                                   ECS021
03737      PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT.                 ECS021
03738                                                                   ECS021
03739      MOVE SPACE                  TO X.                            ECS021
03740                                                                   ECS021
03741      PERFORM 6760-COMPUTE-TOTAL-FIGURES THRU 6760-EXIT            ECS021
03742          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03743            UNTIL DTE-IDX GREATER +15.                             ECS021
03744                                                                   ECS021
03745  6700-AH-LAST-12-TOTAL.                                           ECS021
03746      MOVE '0'                    TO X.                            ECS021
03747      MOVE +15                    TO DTE-IDX.                      ECS021
03748      MOVE +3                     TO PREV-IDX.                     ECS021
03749      MOVE '1'                    TO BREAK-SWITCH.                 ECS021
03750      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03751                                                                   ECS021
03752  6700-AH-PREV-12-TOTAL.                                           ECS021
03753      MOVE SPACES                 TO X.                            ECS021
03754      MOVE +3                     TO DTE-IDX.                      ECS021
03755      MOVE +1                     TO PREV-IDX.                     ECS021
03756      MOVE '2'                    TO BREAK-SWITCH.                 ECS021
03757      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03758                                                                   ECS021
03759  6700-AH-YEAR-TO-DATE-TOTAL.                                      ECS021
03760      MOVE '0'                    TO X.                            ECS021
03761      MOVE +15                    TO DTE-IDX.                      ECS021
03762                                                                   ECS021
03763      COMPUTE PREV-IDX = DTE-IDX - BRK-MM (DTE-IDX).               ECS021
03764                                                                   ECS021
03765      MOVE '3'                    TO BREAK-SWITCH.                 ECS021
03766      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03767                                                                   ECS021
03768  6700-AH-PREV-YTD-TOTAL.                                          ECS021
03769      MOVE SPACES                 TO X.                            ECS021
03770      MOVE +3                     TO DTE-IDX.                      ECS021
03771      MOVE +2                     TO PREV-IDX.                     ECS021
03772      MOVE '4'                    TO BREAK-SWITCH.                 ECS021
03773      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03774                                                                   ECS021
03775  6700-AH-ITD-TOTAL.                                               ECS021
03776      MOVE '0'                    TO X.                            ECS021
03777      PERFORM 6765-TOTAL-ITD-FIGURES THRU 6765-EXIT.               ECS021
03778                                                                   ECS021
03779  6700-SKIP-AH-DETAIL-TOTAL.                                       ECS021
03780      IF DTE-FMT-OPT = '3'                                         ECS021
03781          GO TO 6700-SKIP-AH-AVE-TOTAL.                            ECS021
03782                                                                   ECS021
03783      PERFORM 9000-AVERAGE-INFO-HEADINGS THRU 9010-EXIT.           ECS021
03784                                                                   ECS021
03785      MOVE ZERO-ACCUM-AVG-CNTRS   TO ACCUM-AVG-CNTRS.              ECS021
03786                                                                   ECS021
03787      PERFORM 6780-COMPUTE-TOTAL-AVERAGES THRU 6780-EXIT           ECS021
03788          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03789            UNTIL DTE-IDX GREATER +15.                             ECS021
03790                                                                   ECS021
03791  6700-SKIP-AH-AVE-TOTAL.                                          ECS021
03792      EJECT                                                        ECS021
03793                                                                   ECS021
03794 **************************************************                ECS021
03795 *  THIS SECTION PRODUCES COMBINED TOTAL NUMBERS  *                ECS021
03796 **************************************************                ECS021
03797                                                                   ECS021
03798  6700-TOTAL-BOTH-BENEFITS.                                        ECS021
03799      IF NO-LIFE-BENEFITS AND NO-AH-BENEFITS                       ECS021
03800          GO TO 6700-EXIT.                                         ECS021
03801                                                                   ECS021
03802      MOVE +3                     TO BEN-IDX.                      ECS021
03803      MOVE 'BT'                   TO TOTAL-TYPE-SWITCH.            ECS021
03804                                                                   ECS021
03805      IF DTE-FMT-OPT = 2                                           ECS021
03806          PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT              ECS021
03807          GO TO 6700-SKIP-BT-DETAIL-TOTAL.                         ECS021
03808                                                                   ECS021
03809      PERFORM 9000-HEADING-ROUTINE THRU 9010-EXIT.                 ECS021
03810                                                                   ECS021
03811      MOVE SPACE                  TO X.                            ECS021
03812                                                                   ECS021
03813      PERFORM 6760-COMPUTE-TOTAL-FIGURES THRU 6760-EXIT            ECS021
03814          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03815            UNTIL DTE-IDX GREATER +15.                             ECS021
03816                                                                   ECS021
03817  6700-BOTH-LAST-12-TOTAL.                                         ECS021
03818      MOVE '0'                    TO X.                            ECS021
03819      MOVE +15                    TO DTE-IDX.                      ECS021
03820      MOVE +3                     TO PREV-IDX.                     ECS021
03821      MOVE '1'                    TO BREAK-SWITCH.                 ECS021
03822      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03823                                                                   ECS021
03824  6700-BOTH-PREV-12-TOTAL.                                         ECS021
03825      MOVE SPACES                 TO X.                            ECS021
03826      MOVE +3                     TO DTE-IDX.                      ECS021
03827      MOVE +1                     TO PREV-IDX.                     ECS021
03828      MOVE '2'                    TO BREAK-SWITCH.                 ECS021
03829      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03830                                                                   ECS021
03831  6700-BOTH-YEAR-TO-DATE-TOTAL.                                    ECS021
03832      MOVE '0'                    TO X.                            ECS021
03833      MOVE +15                    TO DTE-IDX.                      ECS021
03834                                                                   ECS021
03835      COMPUTE PREV-IDX = DTE-IDX - BRK-MM (DTE-IDX).               ECS021
03836                                                                   ECS021
03837      MOVE '3'                    TO BREAK-SWITCH.                 ECS021
03838      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03839                                                                   ECS021
03840  6700-BOTH-PREV-YTD-TOTAL.                                        ECS021
03841      MOVE SPACES                 TO X.                            ECS021
03842      MOVE +3                     TO DTE-IDX.                      ECS021
03843      MOVE +2                     TO PREV-IDX.                     ECS021
03844      MOVE '4'                    TO BREAK-SWITCH.                 ECS021
03845      PERFORM 6760-SPECIAL-INTERVAL-FIGURES THRU 6760-EXIT.        ECS021
03846                                                                   ECS021
03847  6700-BOTH-ITD-TOTAL.                                             ECS021
03848      MOVE '0'                    TO X.                            ECS021
03849      PERFORM 6765-TOTAL-ITD-FIGURES THRU 6765-EXIT.               ECS021
03850                                                                   ECS021
03851  6700-SKIP-BT-DETAIL-TOTAL.                                       ECS021
03852      IF DTE-FMT-OPT = '3'                                         ECS021
03853          GO TO 6700-EXIT.                                         ECS021
03854                                                                   ECS021
03855      PERFORM 9000-AVERAGE-INFO-HEADINGS THRU 9010-EXIT.           ECS021
03856                                                                   ECS021
03857      MOVE ZERO-ACCUM-AVG-CNTRS   TO ACCUM-AVG-CNTRS.              ECS021
03858                                                                   ECS021
03859      PERFORM 6780-COMPUTE-TOTAL-AVERAGES THRU 6780-EXIT           ECS021
03860          VARYING DTE-IDX FROM +4 BY +1                            ECS021
03861            UNTIL DTE-IDX GREATER +15.                             ECS021
03862                                                                   ECS021
03863  6700-EXIT.                                                       ECS021
03864      EXIT.                                                        ECS021
03865                                                                   ECS021
03866      EJECT                                                        ECS021
03867 ******************************************************************ECS021
03868 *         COMPUTATIONS AND FORMULA'S FOUND IN:                   *ECS021
03869 *                                                                *ECS021
03870 *         1) 6750-COMPUTE-MONTH-FIGURES                          *ECS021
03871 *         2) 6755-PRODUCE-ITD-FIGURES                            *ECS021
03872 *         3) 6760-COMPUTE-TOTAL-FIGURES                          *ECS021
03873 *         4) 6765-TOTAL-ITD-FIGURES                              *ECS021
03874 *         5) 6770-COMPUTE-AVERAGE-FIGURES                        *ECS021
03875 *         6) 6780-COMPUTE-TOTAL-AVERAGES                         *ECS021
03876 *                                                                *ECS021
03877 *     SPECIAL NOTES:                                             *ECS021
03878 *     PERIODS REPORTED ON ARE ALWAYS DERIVED BY SUBTRACTING      *ECS021
03879 *     INCEPTION-TO-DATE FIGURES ACCUMULATED FROM THE EPEC FILE.  *ECS021
03880 *     FOR EXAMPLE TO DERIVE JUNE 1986 NUMBERS YOU SUBTRACT MAY   *ECS021
03881 *     1986 INCEPTION TO DATE FROM JUNE 1986 INCEPTION TO DATE.   *ECS021
03882 *     THE ITD LINE GENERATED IN THE REPORT STANDS ON IT'S OWN.   *ECS021
03883 *                                                                *ECS021
03884 *     1) NET COVERAGES       ISSUE COUNT                         *ECS021
03885 *                         -  CANCEL COUNT                        *ECS021
03886 *                           --------------                       *ECS021
03887 *                            NET COVERAGES                       *ECS021
03888 *     2) SINGLE / ELIM                                           *ECS021
03889 *        ADD ISSUE COUNT TO THIS COUNTER IF THE BENEFIT TYPE     *ECS021
03890 *        IS SINGLE REDUCING LIFE OR A&H ELIMINATION COVERAGE.    *ECS021
03891 *                                                                *ECS021
03892 *     3) JOINT / RETRO                                           *ECS021
03893 *        ADD ISSUE COUNT TO THIS COUNTER IF THE BENEFIT TYPE     *ECS021
03894 *        IS JOINT REDUCING LIFE OR A&H RETRO COVERAGE.           *ECS021
03895 *                                                                *ECS021
03896 *     4) LEVEL / PENE %                                          *ECS021
03897 *        ADD ISSUE COUNT TO THIS COUNTER IF THE BENEFIT TYPE     *ECS021
03898 *        IS LEVEL LIFE.  IF PRODUCING AN A&H TOTAL PAGE THIS     *ECS021
03899 *        COLUMN WILL BE A&H PENETRATION PERCENT.                 *ECS021
03900 *                                                                *ECS021
03901 *     AH ISSUE CNT / LIFE REDUCING CNT = PENETRATION PERCENT     *ECS021
03902 *                                                                *ECS021
03903 *     5) NET WRITTEN PREMIUM       ISSUE PREMIUM                 *ECS021
03904 *                             -    CANCEL PREMIUM                *ECS021
03905 *                               --------------------             *ECS021
03906 *                                NET WRITTEN PREMIUM             *ECS021
03907 *     6) CANCEL RATIO                                            *ECS021
03908 *           CANCEL PREMIUM / ISSUE PREMIUM = CANCEL RATIO        *ECS021
03909 *                                                                *ECS021
03910 *     7) CLAIMS PAID               CLAIMS PAID                   *ECS021
03911 *                              +   CLAIM ADJUSTMENT              *ECS021
03912 *                                -------------------             *ECS021
03913 *                                  CLAIMS PAID                   *ECS021
03914 *                                                                *ECS021
03915 *     8) LOSS RESERVE              P.T.C. / DUE UNPAID RSVS      *ECS021
03916 *                              +   I.B.N.R. RSVS                 *ECS021
03917 *                              +   CDT / FUTURE RSVS             *ECS021
03918 *                                --------------------------      *ECS021
03919 *                                     LOSS RESERVE               *ECS021
03920 *     9) LOSS RATIO                                              *ECS021
03921 *   (CLAIMS PAID + LOSS RESERVE) / EARNED PREMIUM = LOSS RATIO   *ECS021
03922 *                                                                *ECS021
03923 *    10) COMPENSATION PERCENTAGE                                 *ECS021
03924 * EARNED COMMISSION  /  EARNED PREMIUM = COMPENSATION PERCENTAGE *ECS021
03925 *                                                                *ECS021
03926 *    11) EXPENSE TO EARNINGS RATIO                               *ECS021
03927 * (WRITTEN OR EARNED PREMIUM) * EXPENSE PERCENT = EXPENSE AMOUNT *ECS021
03928 *                                                                *ECS021
03929 *  EXPENSE AMOUNT / (WRITTEN OR EARNED PREMIUM) = EXPENSE RATIO  *ECS021
03930 *                                                                *ECS021
03931 *    12) PERIOD PROFIT PERCENT                                   *ECS021
03932 *                         EARNED OR WRITTEN PREMIUM              *ECS021
03933 *                      -  CLAIM AMOUNT                           *ECS021
03934 *                      -  LOSS RESERVE                           *ECS021
03935 *                      -  COMMISSION AMOUNT                      *ECS021
03936 *                      -  EXPENSE AMOUNT                         *ECS021
03937 *                        ---------------------------             *ECS021
03938 *                         PERIOD PROFIT                          *ECS021
03939 *                                                                *ECS021
03940 *  PERIOD PROFIT / (WRITTEN OR EARNED PREMIUM) = PERIOD PROFIT % *ECS021
03941 *                                                                *ECS021
03942 *    13) AVERAGE PREMIUM                                         *ECS021
03943 *        ISSUE PREMIUM / ISSUE COUNT = AVERAGE PREMIUM           *ECS021
03944 *                                                                *ECS021
03945 *    14) AVERAGE REFUND                                          *ECS021
03946 *        CANCEL PREMIUM / CANCEL COUNT = AVERAGE REFUND          *ECS021
03947 ******************************************************************ECS021
03948                                                                   ECS021
03949      EJECT                                                        ECS021
03950                                                                   ECS021
03951  6750-COMPUTE-MONTH-FIGURES.                                      ECS021
03952      MOVE SPACES                 TO BREAK-SWITCH.                 ECS021
03953      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
03954                                                                   ECS021
03955  6750-SPECIAL-INTERVAL-FIGURES.                                   ECS021
03956      COMPUTE ISS-CNT =                                            ECS021
03957              CMMN-ISS-CNT (BEN-IDX DTE-IDX) -                     ECS021
03958              CMMN-ISS-CNT (BEN-IDX PREV-IDX).                     ECS021
03959                                                                   ECS021
03960      COMPUTE CNC-CNT =                                            ECS021
03961              CMMN-CNC-CNT (BEN-IDX DTE-IDX) -                     ECS021
03962              CMMN-CNC-CNT (BEN-IDX PREV-IDX).                     ECS021
03963                                                                   ECS021
03964      COMPUTE NET-CVRGS = ISS-CNT - CNC-CNT.                       ECS021
03965                                                                   ECS021
03966      COMPUTE ISS-PREM =                                           ECS021
03967              CMMN-ISS-PREM (BEN-IDX DTE-IDX) -                    ECS021
03968              CMMN-ISS-PREM (BEN-IDX PREV-IDX).                    ECS021
03969                                                                   ECS021
03970      COMPUTE CNC-PREM =                                           ECS021
03971              CMMN-CNC-PREM (BEN-IDX DTE-IDX) -                    ECS021
03972              CMMN-CNC-PREM (BEN-IDX PREV-IDX).                    ECS021
03973                                                                   ECS021
03974      COMPUTE NET-WRITTEN-PREM = ISS-PREM - CNC-PREM.              ECS021
03975                                                                   ECS021
03976      MOVE ZEROS                  TO CANCEL-RATIO.                 ECS021
03977                                                                   ECS021
03978      IF ISS-PREM = ZEROS                                          ECS021
03979        IF CNC-PREM = ZEROS                                        ECS021
03980            GO TO 6750-BYPASS-CANCEL-RATIO                         ECS021
03981          ELSE                                                     ECS021
03982            MOVE 99.999 TO CANCEL-RATIO                            ECS021
03983            GO TO 6750-BYPASS-CANCEL-RATIO.                        ECS021
03984                                                                   ECS021
03985      COMPUTE CANCEL-RATIO ROUNDED = CNC-PREM / ISS-PREM.          ECS021
03986                                                                   ECS021
03987  6750-BYPASS-CANCEL-RATIO.                                        ECS021
03988      COMPUTE LOSS-RESERVE =                                       ECS021
03989              CMMN-LOSS-RESV (BEN-IDX DTE-IDX) -                   ECS021
03990              CMMN-LOSS-RESV (BEN-IDX PREV-IDX).                   ECS021
03991                                                                   ECS021
03992      MOVE ZEROS                  TO LOSS-RATIO.                   ECS021
03993                                                                   ECS021
03994      COMPUTE NET-EARND-PREM =                                     ECS021
03995              CMMN-EARND-PREM (BEN-IDX DTE-IDX) -                  ECS021
03996              CMMN-EARND-PREM (BEN-IDX PREV-IDX).                  ECS021
03997                                                                   ECS021
03998      COMPUTE NET-CLAIM-AMT =                                      ECS021
03999              CMMN-CLM-AMT (BEN-IDX DTE-IDX) -                     ECS021
04000              CMMN-CLM-AMT (BEN-IDX PREV-IDX).                     ECS021
04001                                                                   ECS021
04002      IF NET-EARND-PREM = ZEROS                                    ECS021
04003          GO TO 6750-BYPASS-LOSS-RATIO.                            ECS021
04004                                                                   ECS021
04005      COMPUTE LOSS-RATIO ROUNDED =                                 ECS021
04006              (NET-CLAIM-AMT + LOSS-RESERVE) / NET-EARND-PREM.     ECS021
04007                                                                   ECS021
04008  6750-BYPASS-LOSS-RATIO.                                          ECS021
04009      MOVE ZEROS                  TO COMPEN-PERCENT                ECS021
04010                                     SV-COMP-PCT.                  ECS021
04011                                                                   ECS021
04012      COMPUTE NET-COMPEN =                                         ECS021
04013              CMMN-NET-COMPEN (BEN-IDX DTE-IDX) -                  ECS021
04014              CMMN-NET-COMPEN (BEN-IDX PREV-IDX).                  ECS021
04015                                                                   ECS021
04016      IF DTE-PGM-OPT = 1                                           ECS021
04017          IF NET-EARND-PREM = ZEROS                                ECS021
04018              GO TO 6750-BYPASS-COMPEN-PERCENT                     ECS021
04019           ELSE                                                    ECS021
04020              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04021                      NET-COMPEN / NET-EARND-PREM                  ECS021
04022      ELSE                                                         ECS021
04023          IF NET-WRITTEN-PREM = ZEROS                              ECS021
04024              GO TO 6750-BYPASS-COMPEN-PERCENT                     ECS021
04025           ELSE                                                    ECS021
04026              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04027                      NET-COMPEN / NET-WRITTEN-PREM.               ECS021
04028                                                                   ECS021
04029  6750-BYPASS-COMPEN-PERCENT.                                      ECS021
04030      IF CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX) = ZEROS               ECS021
04031          MOVE ZEROS                 TO EXP-EARN-RATIO             ECS021
04032          GO TO 6750-BYPASS-EXPENSE-PCT.                           ECS021
04033                                                                   ECS021
04034      IF DTE-PGM-OPT = 2                                           ECS021
04035          IF NET-WRITTEN-PREM = ZEROS                              ECS021
04036              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04037              GO TO 6750-BYPASS-EXPENSE-PCT.                       ECS021
04038                                                                   ECS021
04039      IF DTE-PGM-OPT = 1                                           ECS021
04040          IF NET-EARND-PREM = ZEROS                                ECS021
04041              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04042              GO TO 6750-BYPASS-EXPENSE-PCT.                       ECS021
04043                                                                   ECS021
04044      COMPUTE EXPENSE-PERCENT =                                    ECS021
04045              CMMN-EXP-PCT      (BEN-IDX DTE-IDX) /                ECS021
04046              CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX).                 ECS021
04047                                                                   ECS021
04048      IF DTE-PGM-OPT = 2                                           ECS021
04049          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04050                  NET-WRITTEN-PREM * EXPENSE-PERCENT               ECS021
04051          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04052                  EXPENSE-AMT / NET-WRITTEN-PREM                   ECS021
04053       ELSE                                                        ECS021
04054          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04055                  NET-EARND-PREM * EXPENSE-PERCENT                 ECS021
04056          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04057                  EXPENSE-AMT / NET-EARND-PREM.                    ECS021
04058                                                                   ECS021
04059  6750-BYPASS-EXPENSE-PCT.                                         ECS021
04060      IF DTE-PGM-OPT = 2                                           ECS021
04061          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04062                  NET-WRITTEN-PREM -                               ECS021
04063                  (NET-CLAIM-AMT + LOSS-RESERVE + NET-COMPEN +     ECS021
04064                  EXPENSE-AMT)                                     ECS021
04065      ELSE                                                         ECS021
04066          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04067                  NET-EARND-PREM -                                 ECS021
04068                  (NET-CLAIM-AMT + LOSS-RESERVE + NET-COMPEN +     ECS021
04069                  EXPENSE-AMT).                                    ECS021
04070                                                                   ECS021
04071      IF DTE-PGM-OPT = 2                                           ECS021
04072          IF NET-WRITTEN-PREM = ZEROS                              ECS021
04073              MOVE ZEROS             TO PROFIT-PERCENT             ECS021
04074              GO TO 6750-BUILD-DETAIL-1.                           ECS021
04075                                                                   ECS021
04076      IF DTE-PGM-OPT = 1                                           ECS021
04077          IF NET-EARND-PREM = ZEROS                                ECS021
04078              MOVE ZEROS             TO PROFIT-PERCENT             ECS021
04079              GO TO 6750-BUILD-DETAIL-1.                           ECS021
04080                                                                   ECS021
04081      IF DTE-PGM-OPT = 2                                           ECS021
04082          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04083                  PERIOD-PROFIT / NET-WRITTEN-PREM                 ECS021
04084      ELSE                                                         ECS021
04085          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04086                  PERIOD-PROFIT / NET-EARND-PREM.                  ECS021
04087                                                                   ECS021
04088  6750-BUILD-DETAIL-1.                                             ECS021
04089      IF ROLLING-BREAK                                             ECS021
04090          MOVE BRK-MM (DTE-IDX)   TO DTL-MONTH-1                   ECS021
04091          MOVE BRK-YY (DTE-IDX)   TO DTL-YEAR-1.                   ECS021
04092                                                                   ECS021
04093      IF LAST-12-BREAK                                             ECS021
04094          MOVE LAST-12-DESC       TO DTL-MONTH-1.                  ECS021
04095                                                                   ECS021
04096      IF PREV-12-BREAK                                             ECS021
04097          MOVE PREV-12-DESC                                        ECS021
04098                                  TO DTL-MONTH-1.                  ECS021
04099                                                                   ECS021
04100      IF YTD-BREAK                                                 ECS021
04101          MOVE YTD-DESC           TO DTL-MONTH-1.                  ECS021
04102                                                                   ECS021
04103      IF PREV-YTD-BREAK                                            ECS021
04104          MOVE PREV-YTD-DESC                                       ECS021
04105                                  TO DTL-MONTH-1.                  ECS021
04106                                                                   ECS021
04107      MOVE NET-CVRGS              TO DTL-NET-CVRG.                 ECS021
04108                                                                   ECS021
04109      IF CMMN-BENEFIT-TYPE (BEN-IDX) = LIFE-OVERRIDE-L1            ECS021
04110          IF CLAS-I-RL-AH (BEN-IDX) = 'R'                          ECS021
04111              IF CLAS-I-JOINT (BEN-IDX) = 'J'                      ECS021
04112                  MOVE NET-CVRGS  TO DTL-JNT-RETRO                 ECS021
04113              ELSE                                                 ECS021
04114                  MOVE NET-CVRGS  TO DTL-SNGL-ELIM                 ECS021
04115          ELSE                                                     ECS021
04116              MOVE NET-CVRGS      TO DTL-LEVEL-LIFE                ECS021
04117      ELSE                                                         ECS021
04118          IF CLAS-I-AB1 (BEN-IDX) = 'E'                            ECS021
04119              MOVE NET-CVRGS      TO DTL-SNGL-ELIM                 ECS021
04120          ELSE                                                     ECS021
04121              IF CLAS-I-AB1 (BEN-IDX) = 'R'                        ECS021
04122                   MOVE NET-CVRGS TO DTL-JNT-RETRO.                ECS021
04123                                                                   ECS021
04124      MOVE NET-WRITTEN-PREM       TO DTL-NET-PREM.                 ECS021
04125      MOVE NET-EARND-PREM         TO DTL-EARND-PREM.               ECS021
04126                                                                   ECS021
04127      MOVE NET-CLAIM-AMT          TO DTL-CLAIMS-PAID.              ECS021
04128                                                                   ECS021
04129      MOVE LOSS-RESERVE           TO DTL-LOSS-RSVS.                ECS021
04130                                                                   ECS021
04131      PERFORM 6800-COMPUTE-DTL-PERCENTS THRU 6800-EXIT.            ECS021
04132                                                                   ECS021
04133      PERFORM 8000-PRINT-DETAIL THRU 8099-EXIT.                    ECS021
04134                                                                   ECS021
04135  6750-EXIT.                                                       ECS021
04136      EXIT.                                                        ECS021
04137                                                                   ECS021
04138      EJECT                                                        ECS021
04139                                                                   ECS021
04140  6755-PRODUCE-ITD-FIGURES.                                        ECS021
04141      COMPUTE NET-CVRGS =                                          ECS021
04142              CMMN-ISS-CNT (BEN-IDX 15) -                          ECS021
04143              CMMN-CNC-CNT (BEN-IDX 15).                           ECS021
04144                                                                   ECS021
04145      COMPUTE NET-WRITTEN-PREM =                                   ECS021
04146              CMMN-ISS-PREM (BEN-IDX 15) -                         ECS021
04147              CMMN-CNC-PREM (BEN-IDX 15).                          ECS021
04148                                                                   ECS021
04149      MOVE ZEROS                  TO CANCEL-RATIO.                 ECS021
04150                                                                   ECS021
04151      IF CMMN-ISS-PREM (BEN-IDX 15) = ZEROS                        ECS021
04152          GO TO 6755-BYPASS-CANCEL-RATIO.                          ECS021
04153                                                                   ECS021
04154      COMPUTE CANCEL-RATIO ROUNDED =                               ECS021
04155              CMMN-CNC-PREM (BEN-IDX 15) /                         ECS021
04156              CMMN-ISS-PREM (BEN-IDX 15).                          ECS021
04157                                                                   ECS021
04158  6755-BYPASS-CANCEL-RATIO.                                        ECS021
04159      MOVE ZEROS                  TO LOSS-RATIO.                   ECS021
04160                                                                   ECS021
04161      IF CMMN-EARND-PREM (BEN-IDX 15) = ZEROS                      ECS021
04162          GO TO 6755-BYPASS-LOSS-RATIO.                            ECS021
04163                                                                   ECS021
04164      COMPUTE LOSS-RATIO ROUNDED =                                 ECS021
04165             (CMMN-CLM-AMT    (BEN-IDX 15) +                       ECS021
04166              CMMN-LOSS-RESV  (BEN-IDX 15)) /                      ECS021
04167              CMMN-EARND-PREM (BEN-IDX 15).                        ECS021
04168                                                                   ECS021
04169  6755-BYPASS-LOSS-RATIO.                                          ECS021
04170      MOVE ZEROS                  TO COMPEN-PERCENT                ECS021
04171                                     SV-COMP-PCT.                  ECS021
04172                                                                   ECS021
04173      IF CMMN-EARND-PREM (BEN-IDX 15) = ZEROS                      ECS021
04174          GO TO 6755-BYPASS-COMPEN-PERCENT.                        ECS021
04175                                                                   ECS021
04176      IF DTE-PGM-OPT = '1'                                         ECS021
04177          IF CMMN-EARND-PREM (BEN-IDX 15) = ZEROS                  ECS021
04178              GO TO 6755-BYPASS-COMPEN-PERCENT                     ECS021
04179          ELSE                                                     ECS021
04180              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04181                      CMMN-NET-COMPEN (BEN-IDX 15) /               ECS021
04182                      CMMN-EARND-PREM (BEN-IDX 15)                 ECS021
04183      ELSE                                                         ECS021
04184          IF NET-WRITTEN-PREM = ZEROS                              ECS021
04185              GO TO 6755-BYPASS-COMPEN-PERCENT                     ECS021
04186          ELSE                                                     ECS021
04187              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04188                      CMMN-NET-COMPEN (BEN-IDX 15) /               ECS021
04189                      NET-WRITTEN-PREM.                            ECS021
04190                                                                   ECS021
04191      COMPUTE SV-COMP-PCT =                                        ECS021
04192              CMMN-NET-COMPEN (BEN-IDX 15) /                       ECS021
04193              CMMN-EARND-PREM (BEN-IDX 15).                        ECS021
04194                                                                   ECS021
04195  6755-BYPASS-COMPEN-PERCENT.                                      ECS021
04196      IF CMMN-ADDED-TO-CNT (BEN-IDX 15) = ZEROS                    ECS021
04197          MOVE ZEROS                 TO EXP-EARN-RATIO             ECS021
04198          GO TO 6755-BYPASS-EXPENSE-PCT.                           ECS021
04199                                                                   ECS021
04200      IF DTE-PGM-OPT = 2                                           ECS021
04201          IF NET-WRITTEN-PREM = ZEROS                              ECS021
04202              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04203              GO TO 6755-BYPASS-EXPENSE-PCT.                       ECS021
04204                                                                   ECS021
04205      IF DTE-PGM-OPT = 1                                           ECS021
04206          IF CMMN-EARND-PREM (BEN-IDX 15) = ZEROS                  ECS021
04207              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04208              GO TO 6755-BYPASS-EXPENSE-PCT.                       ECS021
04209                                                                   ECS021
04210      COMPUTE EXPENSE-PERCENT =                                    ECS021
04211              CMMN-EXP-PCT      (BEN-IDX 15) /                     ECS021
04212              CMMN-ADDED-TO-CNT (BEN-IDX 15).                      ECS021
04213                                                                   ECS021
04214      IF DTE-PGM-OPT = 2                                           ECS021
04215          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04216                  NET-WRITTEN-PREM * EXPENSE-PERCENT               ECS021
04217          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04218                  EXPENSE-AMT / NET-WRITTEN-PREM                   ECS021
04219      ELSE                                                         ECS021
04220          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04221                  CMMN-EARND-PREM (BEN-IDX 15) * EXPENSE-PERCENT   ECS021
04222          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04223                  EXPENSE-AMT / CMMN-EARND-PREM (BEN-IDX 15).      ECS021
04224                                                                   ECS021
04225  6755-BYPASS-EXPENSE-PCT.                                         ECS021
04226      IF DTE-PGM-OPT = 2                                           ECS021
04227          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04228                  NET-WRITTEN-PREM -                               ECS021
04229                 (CMMN-CLM-AMT    (BEN-IDX 15)    +                ECS021
04230                  CMMN-LOSS-RESV  (BEN-IDX 15)    +                ECS021
04231                  CMMN-NET-COMPEN (BEN-IDX 15)    +                ECS021
04232                  EXPENSE-AMT)                                     ECS021
04233      ELSE                                                         ECS021
04234          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04235                  CMMN-EARND-PREM (BEN-IDX 15) -                   ECS021
04236                 (CMMN-CLM-AMT    (BEN-IDX 15) +                   ECS021
04237                  CMMN-LOSS-RESV  (BEN-IDX 15) +                   ECS021
04238                  CMMN-NET-COMPEN (BEN-IDX 15) +                   ECS021
04239                  EXPENSE-AMT).                                    ECS021
04240                                                                   ECS021
04241                                                                   ECS021
04242      IF DTE-PGM-OPT = 2                                           ECS021
04243          IF NET-WRITTEN-PREM = ZEROS                              ECS021
04244              MOVE ZEROS          TO PROFIT-PERCENT                ECS021
04245              GO TO 6755-BUILD-DETAIL-1.                           ECS021
04246                                                                   ECS021
04247      IF DTE-PGM-OPT = 1                                           ECS021
04248          IF CMMN-EARND-PREM (BEN-IDX 15) = ZEROS                  ECS021
04249              MOVE ZEROS          TO PROFIT-PERCENT                ECS021
04250              GO TO 6755-BUILD-DETAIL-1.                           ECS021
04251                                                                   ECS021
04252      IF DTE-PGM-OPT = 2                                           ECS021
04253          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04254                  PERIOD-PROFIT / NET-WRITTEN-PREM                 ECS021
04255      ELSE                                                         ECS021
04256          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04257                  PERIOD-PROFIT / CMMN-EARND-PREM (BEN-IDX 15).    ECS021
04258                                                                   ECS021
04259  6755-BUILD-DETAIL-1.                                             ECS021
04260      MOVE ITD-DESC               TO DTL-MONTH-1.                  ECS021
04261                                                                   ECS021
04262      MOVE NET-CVRGS              TO DTL-NET-CVRG.                 ECS021
04263                                                                   ECS021
04264      IF CMMN-BENEFIT-TYPE (BEN-IDX) = LIFE-OVERRIDE-L1            ECS021
04265          IF CLAS-I-RL-AH (BEN-IDX) = 'R'                          ECS021
04266              IF CLAS-I-JOINT (BEN-IDX) = 'J'                      ECS021
04267                  MOVE NET-CVRGS  TO DTL-JNT-RETRO                 ECS021
04268              ELSE                                                 ECS021
04269                  MOVE NET-CVRGS  TO DTL-SNGL-ELIM                 ECS021
04270          ELSE                                                     ECS021
04271              MOVE NET-CVRGS      TO DTL-LEVEL-LIFE                ECS021
04272      ELSE                                                         ECS021
04273          IF CLAS-I-AB1 (BEN-IDX) = 'E'                            ECS021
04274              MOVE NET-CVRGS  TO DTL-SNGL-ELIM                     ECS021
04275          ELSE                                                     ECS021
04276              IF CLAS-I-AB1 (BEN-IDX) = 'R'                        ECS021
04277                  MOVE NET-CVRGS  TO DTL-JNT-RETRO.                ECS021
04278                                                                   ECS021
04279      MOVE NET-WRITTEN-PREM       TO DTL-NET-PREM.                 ECS021
04280      MOVE CMMN-EARND-PREM (BEN-IDX 15)                            ECS021
04281                                  TO DTL-EARND-PREM.               ECS021
04282                                                                   ECS021
04283      MOVE CMMN-CLM-AMT (BEN-IDX 15)                               ECS021
04284                                  TO DTL-CLAIMS-PAID.              ECS021
04285      MOVE CMMN-LOSS-RESV (BEN-IDX 15)                             ECS021
04286                                  TO DTL-LOSS-RSVS.                ECS021
04287                                                                   ECS021
04288      PERFORM 6800-COMPUTE-DTL-PERCENTS THRU 6800-EXIT.            ECS021
04289                                                                   ECS021
04290      PERFORM 8000-PRINT-DETAIL THRU 8099-EXIT.                    ECS021
04291                                                                   ECS021
04292  6755-EXIT.                                                       ECS021
04293      EXIT.                                                        ECS021
04294                                                                   ECS021
04295      EJECT                                                        ECS021
04296  6760-COMPUTE-TOTAL-FIGURES.                                      ECS021
04297      MOVE SPACES                 TO BREAK-SWITCH.                 ECS021
04298                                                                   ECS021
04299      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
04300                                                                   ECS021
04301  6760-SPECIAL-INTERVAL-FIGURES.                                   ECS021
04302      COMPUTE TOT-ISS-CNT =                                        ECS021
04303              CMNT-ISS-CNT (BEN-IDX DTE-IDX) -                     ECS021
04304              CMNT-ISS-CNT (BEN-IDX PREV-IDX).                     ECS021
04305                                                                   ECS021
04306      COMPUTE TOT-CNC-CNT =                                        ECS021
04307              CMNT-CNC-CNT (BEN-IDX DTE-IDX) -                     ECS021
04308              CMNT-CNC-CNT (BEN-IDX PREV-IDX).                     ECS021
04309                                                                   ECS021
04310      COMPUTE TOT-CVRGS =                                          ECS021
04311              TOT-ISS-CNT - TOT-CNC-CNT.                           ECS021
04312                                                                   ECS021
04313      IF BOTH-TOTAL                                                ECS021
04314          GO TO 6760-SKIP-COVERAGE-TYPES.                          ECS021
04315                                                                   ECS021
04316      COMPUTE TOT-SINGLE-ELEM =                                    ECS021
04317              CMNT-SINGLE-ELEM (BEN-IDX DTE-IDX) -                 ECS021
04318              CMNT-SINGLE-ELEM (BEN-IDX PREV-IDX).                 ECS021
04319                                                                   ECS021
04320      COMPUTE TOT-JOINT-RETRO =                                    ECS021
04321              CMNT-JOINT-RETRO (BEN-IDX DTE-IDX) -                 ECS021
04322              CMNT-JOINT-RETRO (BEN-IDX PREV-IDX).                 ECS021
04323                                                                   ECS021
04324      COMPUTE TOT-LIFE-LEVEL =                                     ECS021
04325              CMNT-LIFE-LEVEL (BEN-IDX DTE-IDX) -                  ECS021
04326              CMNT-LIFE-LEVEL (BEN-IDX PREV-IDX).                  ECS021
04327                                                                   ECS021
04328  6760-SKIP-COVERAGE-TYPES.                                        ECS021
04329      COMPUTE TOT-ISS-PREM =                                       ECS021
04330              CMNT-ISS-PREM (BEN-IDX DTE-IDX) -                    ECS021
04331              CMNT-ISS-PREM (BEN-IDX PREV-IDX).                    ECS021
04332                                                                   ECS021
04333      COMPUTE TOT-CNC-PREM =                                       ECS021
04334              CMNT-CNC-PREM (BEN-IDX DTE-IDX) -                    ECS021
04335              CMNT-CNC-PREM (BEN-IDX PREV-IDX).                    ECS021
04336                                                                   ECS021
04337      COMPUTE TOT-WRITTEN-PREM =                                   ECS021
04338              TOT-ISS-PREM - TOT-CNC-PREM.                         ECS021
04339                                                                   ECS021
04340      MOVE ZEROS                  TO CANCEL-RATIO.                 ECS021
04341                                                                   ECS021
04342      IF TOT-ISS-PREM = ZEROS                                      ECS021
04343          GO TO 6760-BYPASS-CANCEL-RATIO.                          ECS021
04344                                                                   ECS021
04345      COMPUTE CANCEL-RATIO ROUNDED =                               ECS021
04346              TOT-CNC-PREM  /  TOT-ISS-PREM.                       ECS021
04347                                                                   ECS021
04348  6760-BYPASS-CANCEL-RATIO.                                        ECS021
04349      COMPUTE LOSS-RESERVE =                                       ECS021
04350              CMNT-LOSS-RESV (BEN-IDX DTE-IDX) -                   ECS021
04351              CMNT-LOSS-RESV (BEN-IDX PREV-IDX).                   ECS021
04352                                                                   ECS021
04353      MOVE ZEROS                  TO LOSS-RATIO.                   ECS021
04354                                                                   ECS021
04355      COMPUTE TOT-EARND-PREM =                                     ECS021
04356              CMNT-EARND-PREM (BEN-IDX DTE-IDX) -                  ECS021
04357              CMNT-EARND-PREM (BEN-IDX PREV-IDX).                  ECS021
04358                                                                   ECS021
04359      IF LIFE-TOTAL                                                ECS021
04360          COMPUTE TOT-CLAIM-AMT =                                  ECS021
04361                  CMNT-LF-CLM-AMT (BEN-IDX DTE-IDX) -              ECS021
04362                  CMNT-LF-CLM-AMT (BEN-IDX PREV-IDX).              ECS021
04363                                                                   ECS021
04364      IF AH-TOTAL                                                  ECS021
04365          COMPUTE TOT-CLAIM-AMT =                                  ECS021
04366                  CMNT-AH-CLM-AMT (BEN-IDX DTE-IDX) -              ECS021
04367                  CMNT-AH-CLM-AMT (BEN-IDX PREV-IDX).              ECS021
04368                                                                   ECS021
04369      IF BOTH-TOTAL                                                ECS021
04370          COMPUTE TOT-CLAIM-AMT =                                  ECS021
04371                  CMNT-LF-CLM-AMT (BEN-IDX DTE-IDX) -              ECS021
04372                  CMNT-LF-CLM-AMT (BEN-IDX PREV-IDX) +             ECS021
04373                  CMNT-AH-CLM-AMT (BEN-IDX DTE-IDX) -              ECS021
04374                  CMNT-AH-CLM-AMT (BEN-IDX PREV-IDX).              ECS021
04375                                                                   ECS021
04376      IF TOT-EARND-PREM = ZEROS                                    ECS021
04377          GO TO 6760-BYPASS-LOSS-RATIO.                            ECS021
04378                                                                   ECS021
04379      COMPUTE LOSS-RATIO ROUNDED =                                 ECS021
04380             (TOT-CLAIM-AMT + LOSS-RESERVE ) / TOT-EARND-PREM.     ECS021
04381                                                                   ECS021
04382  6760-BYPASS-LOSS-RATIO.                                          ECS021
04383      MOVE ZEROS                  TO COMPEN-PERCENT                ECS021
04384                                     SV-COMP-PCT.                  ECS021
04385                                                                   ECS021
04386      COMPUTE TOT-COMPEN =                                         ECS021
04387              CMNT-NET-COMPEN (BEN-IDX DTE-IDX) -                  ECS021
04388              CMNT-NET-COMPEN (BEN-IDX PREV-IDX).                  ECS021
04389                                                                   ECS021
04390      IF DTE-PGM-OPT = '1'                                         ECS021
04391          IF TOT-EARND-PREM = ZEROS                                ECS021
04392              GO TO 6760-BYPASS-COMPEN-PERCENT                     ECS021
04393          ELSE                                                     ECS021
04394              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04395                      TOT-COMPEN / TOT-EARND-PREM                  ECS021
04396      ELSE                                                         ECS021
04397          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
04398              GO TO 6760-BYPASS-COMPEN-PERCENT                     ECS021
04399          ELSE                                                     ECS021
04400              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04401                      TOT-COMPEN / TOT-WRITTEN-PREM.               ECS021
04402                                                                   ECS021
04403  6760-BYPASS-COMPEN-PERCENT.                                      ECS021
04404      IF CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX) = ZEROS               ECS021
04405          MOVE ZEROS                 TO EXP-EARN-RATIO             ECS021
04406          GO TO 6760-BYPASS-EXPENSE-PCT.                           ECS021
04407                                                                   ECS021
04408      IF DTE-PGM-OPT = 2                                           ECS021
04409          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
04410              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04411              GO TO 6760-BYPASS-EXPENSE-PCT.                       ECS021
04412                                                                   ECS021
04413      IF DTE-PGM-OPT = 1                                           ECS021
04414          IF TOT-EARND-PREM = ZEROS                                ECS021
04415              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04416              GO TO 6760-BYPASS-EXPENSE-PCT.                       ECS021
04417                                                                   ECS021
04418      COMPUTE EXPENSE-PERCENT =                                    ECS021
04419              CMNT-EXP-PCT      (BEN-IDX DTE-IDX) /                ECS021
04420              CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX).                 ECS021
04421                                                                   ECS021
04422      IF DTE-PGM-OPT = 2                                           ECS021
04423          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04424                  TOT-WRITTEN-PREM * EXPENSE-PERCENT               ECS021
04425          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04426                  EXPENSE-AMT / TOT-WRITTEN-PREM                   ECS021
04427      ELSE                                                         ECS021
04428          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04429                  TOT-EARND-PREM * EXPENSE-PERCENT                 ECS021
04430          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04431                  EXPENSE-AMT / TOT-EARND-PREM.                    ECS021
04432                                                                   ECS021
04433  6760-BYPASS-EXPENSE-PCT.                                         ECS021
04434      IF DTE-PGM-OPT = 2                                           ECS021
04435          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04436                  TOT-WRITTEN-PREM -                               ECS021
04437                  (TOT-CLAIM-AMT + LOSS-RESERVE + TOT-COMPEN +     ECS021
04438                   EXPENSE-AMT)                                    ECS021
04439      ELSE                                                         ECS021
04440          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04441              TOT-EARND-PREM -                                     ECS021
04442                  (TOT-CLAIM-AMT + LOSS-RESERVE + TOT-COMPEN +     ECS021
04443                   EXPENSE-AMT).                                   ECS021
04444                                                                   ECS021
04445      IF DTE-PGM-OPT = 2                                           ECS021
04446          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
04447              MOVE ZEROS             TO PROFIT-PERCENT             ECS021
04448              GO TO 6760-BUILD-DETAIL-1.                           ECS021
04449                                                                   ECS021
04450      IF DTE-PGM-OPT = 1                                           ECS021
04451          IF TOT-EARND-PREM = ZEROS                                ECS021
04452              MOVE ZEROS             TO PROFIT-PERCENT             ECS021
04453              GO TO 6760-BUILD-DETAIL-1.                           ECS021
04454                                                                   ECS021
04455      IF DTE-PGM-OPT = 2                                           ECS021
04456          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04457                  PERIOD-PROFIT / TOT-WRITTEN-PREM                 ECS021
04458      ELSE                                                         ECS021
04459          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04460                  PERIOD-PROFIT / TOT-EARND-PREM.                  ECS021
04461                                                                   ECS021
04462  6760-BUILD-DETAIL-1.                                             ECS021
04463      IF ROLLING-BREAK                                             ECS021
04464          MOVE BRK-MM (DTE-IDX)   TO DTL-MONTH-1                   ECS021
04465          MOVE BRK-YY (DTE-IDX)   TO DTL-YEAR-1.                   ECS021
04466                                                                   ECS021
04467      IF LAST-12-BREAK                                             ECS021
04468          MOVE LAST-12-DESC       TO DTL-MONTH-1.                  ECS021
04469                                                                   ECS021
04470      IF PREV-12-BREAK                                             ECS021
04471          MOVE PREV-12-DESC                                        ECS021
04472                                  TO DTL-MONTH-1.                  ECS021
04473                                                                   ECS021
04474      IF YTD-BREAK                                                 ECS021
04475          MOVE YTD-DESC           TO DTL-MONTH-1.                  ECS021
04476                                                                   ECS021
04477      IF PREV-YTD-BREAK                                            ECS021
04478          MOVE PREV-YTD-DESC                                       ECS021
04479                                  TO DTL-MONTH-1.                  ECS021
04480                                                                   ECS021
04481      MOVE TOT-CVRGS              TO DTL-NET-CVRG.                 ECS021
04482                                                                   ECS021
04483      IF BOTH-TOTAL                                                ECS021
04484          GO TO 6760-SKIP-PENETRATION-CALC.                        ECS021
04485                                                                   ECS021
04486      MOVE TOT-SINGLE-ELEM        TO DTL-SNGL-ELIM.                ECS021
04487      MOVE TOT-JOINT-RETRO        TO DTL-JNT-RETRO.                ECS021
04488                                                                   ECS021
04489 ****************************************************************  ECS021
04490 *  BEN-IDX EQUAL 1 (LIFE TOTALS) JUST MOVE LIFE LEVEL COUNTER  *  ECS021
04491 *  BEN-IDX EQUAL 2 (AH   TOTALS) COMPUTE PENETRATION PERCENT   *  ECS021
04492 *  OF AH TO LIFE POLICIES                                      *  ECS021
04493 ****************************************************************  ECS021
04494                                                                   ECS021
04495      IF BEN-IDX = +1                                              ECS021
04496          MOVE TOT-LIFE-LEVEL     TO DTL-LEVEL-LIFE                ECS021
04497          GO TO 6760-SKIP-PENETRATION-CALC.                        ECS021
04498                                                                   ECS021
04499      MOVE ZEROS                  TO PENETRATION-PERCENT.          ECS021
04500                                                                   ECS021
04501      COMPUTE TOT-LIFE-DECR =                                      ECS021
04502              CMNT-SINGLE-ELEM (1 DTE-IDX) -                       ECS021
04503              CMNT-SINGLE-ELEM (1 PREV-IDX).                       ECS021
04504                                                                   ECS021
04505      COMPUTE TOT-LIFE-DECR = TOT-LIFE-DECR +                      ECS021
04506              (CMNT-JOINT-RETRO (1 DTE-IDX) -                      ECS021
04507               CMNT-JOINT-RETRO (1 PREV-IDX)).                     ECS021
04508                                                                   ECS021
04509      IF TOT-LIFE-DECR = ZEROS                                     ECS021
04510          MOVE ZEROS              TO DTL-PENE-PERCENT              ECS021
04511          GO TO 6760-SKIP-PENETRATION-CALC.                        ECS021
04512                                                                   ECS021
04513      COMPUTE PENETRATION-PERCENT ROUNDED =                        ECS021
04514              TOT-ISS-CNT / TOT-LIFE-DECR.                         ECS021
04515                                                                   ECS021
04516      MULTIPLY PENETRATION-PERCENT BY +100 GIVING DTL-PENE-PERCENT.ECS021
04517                                                                   ECS021
04518  6760-SKIP-PENETRATION-CALC.                                      ECS021
04519      MOVE TOT-WRITTEN-PREM       TO DTL-NET-PREM.                 ECS021
04520      MOVE TOT-EARND-PREM         TO DTL-EARND-PREM.               ECS021
04521                                                                   ECS021
04522      MOVE TOT-CLAIM-AMT          TO DTL-CLAIMS-PAID.              ECS021
04523                                                                   ECS021
04524      MOVE LOSS-RESERVE           TO DTL-LOSS-RSVS.                ECS021
04525                                                                   ECS021
04526      PERFORM 6800-COMPUTE-DTL-PERCENTS THRU 6800-EXIT.            ECS021
04527                                                                   ECS021
04528      PERFORM 8000-PRINT-DETAIL THRU 8099-EXIT.                    ECS021
04529                                                                   ECS021
04530  6760-EXIT.                                                       ECS021
04531      EXIT.                                                        ECS021
04532                                                                   ECS021
04533      EJECT                                                        ECS021
04534                                                                   ECS021
04535  6765-TOTAL-ITD-FIGURES.                                          ECS021
04536      COMPUTE TOT-CVRGS =                                          ECS021
04537              CMNT-ISS-CNT (BEN-IDX 15) -                          ECS021
04538              CMNT-CNC-CNT (BEN-IDX 15).                           ECS021
04539                                                                   ECS021
04540      COMPUTE TOT-WRITTEN-PREM =                                   ECS021
04541              CMNT-ISS-PREM (BEN-IDX 15) -                         ECS021
04542              CMNT-CNC-PREM (BEN-IDX 15).                          ECS021
04543                                                                   ECS021
04544      MOVE ZEROS                  TO CANCEL-RATIO.                 ECS021
04545                                                                   ECS021
04546      IF CMNT-ISS-PREM (BEN-IDX 15) = ZEROS                        ECS021
04547          GO TO 6765-BYPASS-CANCEL-RATIO.                          ECS021
04548                                                                   ECS021
04549      COMPUTE CANCEL-RATIO ROUNDED =                               ECS021
04550              CMNT-CNC-PREM (BEN-IDX 15) /                         ECS021
04551              CMNT-ISS-PREM (BEN-IDX 15).                          ECS021
04552                                                                   ECS021
04553  6765-BYPASS-CANCEL-RATIO.                                        ECS021
04554      MOVE ZEROS                  TO LOSS-RATIO.                   ECS021
04555                                                                   ECS021
04556      IF CMNT-EARND-PREM (BEN-IDX 15) = ZEROS                      ECS021
04557          GO TO 6765-BYPASS-LOSS-RATIO.                            ECS021
04558                                                                   ECS021
04559      IF LIFE-TOTAL                                                ECS021
04560          COMPUTE TOT-CLAIM-AMT = CMNT-LF-CLM-AMT (BEN-IDX 15).    ECS021
04561                                                                   ECS021
04562      IF AH-TOTAL                                                  ECS021
04563          COMPUTE TOT-CLAIM-AMT = CMNT-AH-CLM-AMT (BEN-IDX 15).    ECS021
04564                                                                   ECS021
04565      IF BOTH-TOTAL                                                ECS021
04566          COMPUTE TOT-CLAIM-AMT =                                  ECS021
04567                  CMNT-LF-CLM-AMT (BEN-IDX 15) +                   ECS021
04568                  CMNT-AH-CLM-AMT (BEN-IDX 15).                    ECS021
04569                                                                   ECS021
04570      COMPUTE LOSS-RATIO ROUNDED =                                 ECS021
04571              (TOT-CLAIM-AMT + CMNT-LOSS-RESV (BEN-IDX 15)) /      ECS021
04572              CMNT-EARND-PREM (BEN-IDX 15).                        ECS021
04573                                                                   ECS021
04574  6765-BYPASS-LOSS-RATIO.                                          ECS021
04575      MOVE ZEROS                  TO COMPEN-PERCENT                ECS021
04576                                     SV-COMP-PCT.                  ECS021
04577                                                                   ECS021
04578      IF DTE-PGM-OPT = '1'                                         ECS021
04579          IF CMNT-EARND-PREM (BEN-IDX 15) = ZEROS                  ECS021
04580              GO TO 6765-BYPASS-COMPEN-PERCENT                     ECS021
04581          ELSE                                                     ECS021
04582              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04583                      CMNT-NET-COMPEN (BEN-IDX 15) /               ECS021
04584                      CMNT-EARND-PREM (BEN-IDX 15)                 ECS021
04585      ELSE                                                         ECS021
04586          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
04587              GO TO 6765-BYPASS-COMPEN-PERCENT                     ECS021
04588          ELSE                                                     ECS021
04589              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
04590                      CMNT-NET-COMPEN (BEN-IDX 15) /               ECS021
04591                      TOT-WRITTEN-PREM.                            ECS021
04592                                                                   ECS021
04593  6765-BYPASS-COMPEN-PERCENT.                                      ECS021
04594      IF CMNT-ADDED-TO-CNT (BEN-IDX 15) = ZEROS                    ECS021
04595          MOVE ZEROS                 TO EXP-EARN-RATIO             ECS021
04596          GO TO 6765-BYPASS-EXPENSE-PCT.                           ECS021
04597                                                                   ECS021
04598      IF DTE-PGM-OPT = 2                                           ECS021
04599          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
04600              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04601              GO TO 6765-BYPASS-EXPENSE-PCT.                       ECS021
04602                                                                   ECS021
04603      IF DTE-PGM-OPT = 1                                           ECS021
04604          IF CMNT-EARND-PREM (BEN-IDX 15) = ZEROS                  ECS021
04605              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
04606              GO TO 6765-BYPASS-EXPENSE-PCT.                       ECS021
04607                                                                   ECS021
04608      COMPUTE EXPENSE-PERCENT =                                    ECS021
04609              CMNT-EXP-PCT      (BEN-IDX 15) /                     ECS021
04610              CMNT-ADDED-TO-CNT (BEN-IDX 15).                      ECS021
04611                                                                   ECS021
04612      IF DTE-PGM-OPT = 2                                           ECS021
04613          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04614                  TOT-WRITTEN-PREM * EXPENSE-PERCENT               ECS021
04615          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04616                  EXPENSE-AMT / TOT-WRITTEN-PREM                   ECS021
04617      ELSE                                                         ECS021
04618          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
04619                  CMNT-EARND-PREM (BEN-IDX 15) * EXPENSE-PERCENT   ECS021
04620          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
04621                  EXPENSE-AMT / CMNT-EARND-PREM (BEN-IDX 15).      ECS021
04622                                                                   ECS021
04623  6765-BYPASS-EXPENSE-PCT.                                         ECS021
04624      IF DTE-PGM-OPT = 2                                           ECS021
04625          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04626                  TOT-WRITTEN-PREM -                               ECS021
04627                 (TOT-CLAIM-AMT + CMNT-LOSS-RESV (BEN-IDX 15) +    ECS021
04628                  CMNT-NET-COMPEN (BEN-IDX 15) +                   ECS021
04629                  EXPENSE-AMT)                                     ECS021
04630      ELSE                                                         ECS021
04631          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
04632                  CMNT-EARND-PREM (BEN-IDX 15) -                   ECS021
04633                 (TOT-CLAIM-AMT + CMNT-LOSS-RESV (BEN-IDX 15) +    ECS021
04634                  CMNT-NET-COMPEN (BEN-IDX 15) +                   ECS021
04635                  EXPENSE-AMT).                                    ECS021
04636                                                                   ECS021
04637      IF DTE-PGM-OPT = 2                                           ECS021
04638          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
04639              MOVE ZEROS          TO PROFIT-PERCENT                ECS021
04640              GO TO 6765-BUILD-DETAIL-1.                           ECS021
04641                                                                   ECS021
04642      IF DTE-PGM-OPT = 1                                           ECS021
04643          IF CMNT-EARND-PREM (BEN-IDX 15) = ZEROS                  ECS021
04644              MOVE ZEROS          TO PROFIT-PERCENT                ECS021
04645              GO TO 6765-BUILD-DETAIL-1.                           ECS021
04646                                                                   ECS021
04647      IF DTE-PGM-OPT = 2                                           ECS021
04648          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04649                  PERIOD-PROFIT / TOT-WRITTEN-PREM                 ECS021
04650      ELSE                                                         ECS021
04651          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
04652                  PERIOD-PROFIT / CMNT-EARND-PREM (BEN-IDX 15).    ECS021
04653                                                                   ECS021
04654  6765-BUILD-DETAIL-1.                                             ECS021
04655      MOVE ITD-DESC               TO DTL-MONTH-1.                  ECS021
04656                                                                   ECS021
04657      MOVE TOT-CVRGS              TO DTL-NET-CVRG.                 ECS021
04658                                                                   ECS021
04659      IF BOTH-TOTAL                                                ECS021
04660          GO TO 6765-SKIP-PENETRATION-CALC.                        ECS021
04661                                                                   ECS021
04662      MOVE CMNT-SINGLE-ELEM (BEN-IDX 15)                           ECS021
04663                                  TO DTL-SNGL-ELIM.                ECS021
04664      MOVE CMNT-JOINT-RETRO (BEN-IDX 15)                           ECS021
04665                                  TO DTL-JNT-RETRO.                ECS021
04666                                                                   ECS021
04667 ****************************************************************  ECS021
04668 *  BEN-IDX EQUAL 1 (LIFE TOTALS) JUST MOVE LIFE LEVEL COUNTER  *  ECS021
04669 *  BEN-IDX EQUAL 2 (AH   TOTALS) COMPUTE PENETRATION PERCENT   *  ECS021
04670 *  OF AH TO LIFE POLICIES                                      *  ECS021
04671 ****************************************************************  ECS021
04672                                                                   ECS021
04673      IF BEN-IDX = +1                                              ECS021
04674          MOVE CMNT-LIFE-LEVEL (BEN-IDX 15)                        ECS021
04675                                  TO DTL-LEVEL-LIFE                ECS021
04676          GO TO 6765-SKIP-PENETRATION-CALC.                        ECS021
04677                                                                   ECS021
04678      MOVE ZEROS                  TO PENETRATION-PERCENT.          ECS021
04679                                                                   ECS021
04680      COMPUTE TOT-LIFE-DECR =                                      ECS021
04681              CMNT-SINGLE-ELEM (1 15) + CMNT-JOINT-RETRO (1 15).   ECS021
04682                                                                   ECS021
04683      IF TOT-LIFE-DECR = ZEROS                                     ECS021
04684          MOVE ZEROS              TO DTL-PENE-PERCENT              ECS021
04685          GO TO 6765-SKIP-PENETRATION-CALC.                        ECS021
04686                                                                   ECS021
04687      COMPUTE PENETRATION-PERCENT ROUNDED =                        ECS021
04688              CMNT-ISS-CNT (BEN-IDX 15) / TOT-LIFE-DECR.           ECS021
04689                                                                   ECS021
04690      MULTIPLY PENETRATION-PERCENT BY +100 GIVING DTL-PENE-PERCENT.ECS021
04691                                                                   ECS021
04692  6765-SKIP-PENETRATION-CALC.                                      ECS021
04693      MOVE TOT-WRITTEN-PREM       TO DTL-NET-PREM.                 ECS021
04694      MOVE CMNT-EARND-PREM (BEN-IDX 15)                            ECS021
04695                                  TO DTL-EARND-PREM.               ECS021
04696                                                                   ECS021
04697      MOVE TOT-CLAIM-AMT          TO DTL-CLAIMS-PAID.              ECS021
04698                                                                   ECS021
04699      MOVE CMNT-LOSS-RESV (BEN-IDX 15)                             ECS021
04700                                  TO DTL-LOSS-RSVS.                ECS021
04701                                                                   ECS021
04702      PERFORM 6800-COMPUTE-DTL-PERCENTS THRU 6800-EXIT.            ECS021
04703                                                                   ECS021
04704      PERFORM 8000-PRINT-DETAIL THRU 8099-EXIT.                    ECS021
04705                                                                   ECS021
04706  6765-EXIT.                                                       ECS021
04707      EXIT.                                                        ECS021
04708                                                                   ECS021
04709      EJECT                                                        ECS021
04710                                                                   ECS021
04711  6770-COMPUTE-AVERAGE-FIGURES.                                    ECS021
04712                                                                   ECS021
04713      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
04714                                                                   ECS021
04715      COMPUTE ISS-CNT =                                            ECS021
04716              CMMN-ISS-CNT (BEN-IDX DTE-IDX) -                     ECS021
04717              CMMN-ISS-CNT (BEN-IDX PREV-IDX).                     ECS021
04718                                                                   ECS021
04719      IF ISS-CNT = ZEROS                                           ECS021
04720          MOVE ZERO               TO AVG-PREMIUM                   ECS021
04721          GO TO 6770-ISSUE-COUNT-ZERO.                             ECS021
04722                                                                   ECS021
04723      COMPUTE ISS-PREM =                                           ECS021
04724              CMMN-ISS-PREM (BEN-IDX DTE-IDX) -                    ECS021
04725              CMMN-ISS-PREM (BEN-IDX PREV-IDX).                    ECS021
04726                                                                   ECS021
04727      COMPUTE AVG-PREMIUM = ISS-PREM / ISS-CNT.                    ECS021
04728                                                                   ECS021
04729  6770-ISSUE-COUNT-ZERO.                                           ECS021
04730      COMPUTE CNC-CNT =                                            ECS021
04731              CMMN-CNC-CNT (BEN-IDX DTE-IDX) -                     ECS021
04732              CMMN-CNC-CNT (BEN-IDX PREV-IDX).                     ECS021
04733                                                                   ECS021
04734      IF CNC-CNT = ZEROS                                           ECS021
04735          MOVE ZERO               TO AVG-CANCEL-PREMIUM            ECS021
04736          GO TO 6770-CANCEL-COUNT-ZERO.                            ECS021
04737                                                                   ECS021
04738      COMPUTE CNC-PREM =                                           ECS021
04739              CMMN-CNC-PREM (BEN-IDX DTE-IDX) -                    ECS021
04740              CMMN-CNC-PREM (BEN-IDX PREV-IDX).                    ECS021
04741                                                                   ECS021
04742      COMPUTE AVG-CANCEL-PREMIUM = CNC-PREM / CNC-CNT.             ECS021
04743                                                                   ECS021
04744  6770-CANCEL-COUNT-ZERO.                                          ECS021
04745      COMPUTE CLM-CNT =                                            ECS021
04746              CMMN-CLM-CNT (BEN-IDX DTE-IDX) -                     ECS021
04747              CMMN-CLM-CNT (BEN-IDX PREV-IDX).                     ECS021
04748                                                                   ECS021
04749      COMPUTE CLAIMS-PAID =                                        ECS021
04750              CMMN-CLM-AMT (BEN-IDX DTE-IDX) -                     ECS021
04751              CMMN-CLM-AMT (BEN-IDX PREV-IDX).                     ECS021
04752                                                                   ECS021
04753      IF CLM-CNT NOT = ZEROS                                       ECS021
04754          COMPUTE AVG-CLAIM = CLAIMS-PAID / CLM-CNT                ECS021
04755      ELSE                                                         ECS021
04756          MOVE ZEROS              TO AVG-CLAIM.                    ECS021
04757                                                                   ECS021
04758      MOVE BRK-MM (DTE-IDX)       TO DTL-MONTH-2.                  ECS021
04759      MOVE BRK-YY (DTE-IDX)       TO DTL-YEAR-2.                   ECS021
04760                                                                   ECS021
04761      IF CMMN-ISS-CNT (BEN-IDX DTE-IDX) NOT = ZEROS                ECS021
04762          COMPUTE AVG-ORG-TRM ROUNDED =                            ECS021
04763                  CMMN-AVG-ORG-TRM  (BEN-IDX DTE-IDX) /            ECS021
04764                  CMMN-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04765          COMPUTE WGHT-ORG-TRM ROUNDED =                           ECS021
04766                  CMMN-WGHT-ORG-TRM (BEN-IDX DTE-IDX) /            ECS021
04767                  CMMN-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04768          COMPUTE AVG-AGE ROUNDED =                                ECS021
04769                  CMMN-AVG-AGE      (BEN-IDX DTE-IDX) /            ECS021
04770                  CMMN-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04771          COMPUTE WGHT-AGE ROUNDED =                               ECS021
04772                  CMMN-WGHT-AGE     (BEN-IDX DTE-IDX) /            ECS021
04773                  CMMN-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04774      ELSE                                                         ECS021
04775      IF CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX) NOT = ZEROS           ECS021
04776          COMPUTE AVG-ORG-TRM ROUNDED =                            ECS021
04777                  CMMN-AVG-ORG-TRM  (BEN-IDX DTE-IDX) /            ECS021
04778                  CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
04779          COMPUTE WGHT-ORG-TRM ROUNDED =                           ECS021
04780                  CMMN-WGHT-ORG-TRM (BEN-IDX DTE-IDX) /            ECS021
04781                  CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
04782          COMPUTE AVG-AGE ROUNDED =                                ECS021
04783                  CMMN-AVG-AGE      (BEN-IDX DTE-IDX) /            ECS021
04784                  CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
04785          COMPUTE WGHT-AGE ROUNDED =                               ECS021
04786                  CMMN-WGHT-AGE     (BEN-IDX DTE-IDX) /            ECS021
04787                  CMMN-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
04788      ELSE                                                         ECS021
04789          MOVE ZEROS              TO AVG-ORG-TRM                   ECS021
04790                                     WGHT-ORG-TRM                  ECS021
04791                                     AVG-AGE                       ECS021
04792                                     WGHT-AGE.                     ECS021
04793                                                                   ECS021
04794      MOVE CMMN-INFRC-CNT (BEN-IDX DTE-IDX)                        ECS021
04795                                  TO DTL-INFRC-CVRG.               ECS021
04796      MOVE AVG-ORG-TRM            TO DTL-AVRG-ORIG-TERM.           ECS021
04797      MOVE WGHT-ORG-TRM           TO DTL-WGHT-ORIG-TERM.           ECS021
04798      MOVE AVG-AGE                TO DTL-AVRG-ISS-AGE.             ECS021
04799      MOVE WGHT-AGE               TO DTL-WGHT-ISS-AGE.             ECS021
04800                                                                   ECS021
04801      IF LIFE-INDIVIDUAL                                           ECS021
04802          MOVE AVG-CLAIM          TO DTL-AVRG-LF-CLAIM             ECS021
04803      ELSE                                                         ECS021
04804          MOVE AVG-CLAIM          TO DTL-AVRG-AH-CLAIM.            ECS021
04805                                                                   ECS021
04806      MOVE AVG-PREMIUM            TO DTL-AVRG-PREM.                ECS021
04807      MOVE AVG-CANCEL-PREMIUM     TO DTL-AVRG-REFUND.              ECS021
04808                                                                   ECS021
04809      PERFORM 8500-PRINT-DETAIL THRU 8599-EXIT.                    ECS021
04810                                                                   ECS021
04811  6770-ACCUM-ALL-PERIODS.                                          ECS021
04812      IF AVG-PREMIUM GREATER ZERO                                  ECS021
04813          ADD AVG-PREMIUM         TO ACCUM-AVG-PREM                ECS021
04814          ADD 1                   TO ACCUM-AVG-PREM-CNT.           ECS021
04815                                                                   ECS021
04816      IF AVG-CANCEL-PREMIUM GREATER ZERO                           ECS021
04817          ADD AVG-CANCEL-PREMIUM  TO ACCUM-AVG-REFUND              ECS021
04818          ADD 1                   TO ACCUM-AVG-REFUND-CNT.         ECS021
04819                                                                   ECS021
04820      IF CMMN-INFRC-CNT (BEN-IDX DTE-IDX) GREATER ZERO             ECS021
04821          ADD CMMN-INFRC-CNT (BEN-IDX DTE-IDX)                     ECS021
04822                                  TO ACCUM-AVG-INFRC               ECS021
04823          ADD 1                   TO ACCUM-AVG-INFRC-CNT.          ECS021
04824                                                                   ECS021
04825      IF AVG-ORG-TRM GREATER ZERO                                  ECS021
04826          ADD AVG-ORG-TRM         TO ACCUM-AVG-ORG-TRM             ECS021
04827          ADD 1                   TO ACCUM-AVG-ORG-TRM-CNT.        ECS021
04828                                                                   ECS021
04829      IF WGHT-ORG-TRM GREATER ZERO                                 ECS021
04830          ADD WGHT-ORG-TRM        TO ACCUM-AVG-WGHT-TRM            ECS021
04831          ADD 1                   TO ACCUM-AVG-WGHT-TRM-CNT.       ECS021
04832                                                                   ECS021
04833      IF AVG-AGE GREATER ZERO                                      ECS021
04834          ADD AVG-AGE             TO ACCUM-AVG-ISS-AGE             ECS021
04835          ADD 1                   TO ACCUM-AVG-ISS-CNT.            ECS021
04836                                                                   ECS021
04837      IF WGHT-AGE GREATER ZERO                                     ECS021
04838          ADD WGHT-AGE            TO ACCUM-AVG-WGHT-AGE            ECS021
04839          ADD 1                   TO ACCUM-AVG-WGHT-CNT.           ECS021
04840                                                                   ECS021
04841      IF AVG-CLAIM GREATER ZERO                                    ECS021
04842          IF LIFE-INDIVIDUAL                                       ECS021
04843              ADD AVG-CLAIM       TO ACCUM-AVG-LIFE-CLAIM          ECS021
04844              ADD 1               TO ACCUM-AVG-LIFE-CNT            ECS021
04845          ELSE                                                     ECS021
04846              ADD AVG-CLAIM       TO ACCUM-AVG-AH-CLAIM            ECS021
04847              ADD 1               TO ACCUM-AVG-AH-CNT.             ECS021
04848                                                                   ECS021
04849      IF DTE-IDX LESS +15                                          ECS021
04850          GO TO 6770-EXIT.                                         ECS021
04851                                                                   ECS021
04852      MOVE AVERAGE-DESC           TO DTL-LINE-2-DESC.              ECS021
04853                                                                   ECS021
04854      IF ACCUM-AVG-INFRC-CNT GREATER ZERO                          ECS021
04855          COMPUTE DTL-INFRC-CVRG =                                 ECS021
04856                  ACCUM-AVG-INFRC / ACCUM-AVG-INFRC-CNT            ECS021
04857      ELSE                                                         ECS021
04858          MOVE ZERO               TO DTL-INFRC-CVRG.               ECS021
04859                                                                   ECS021
04860      IF ACCUM-AVG-PREM-CNT GREATER ZERO                           ECS021
04861          COMPUTE DTL-AVRG-PREM =                                  ECS021
04862                  ACCUM-AVG-PREM / ACCUM-AVG-PREM-CNT              ECS021
04863      ELSE                                                         ECS021
04864          MOVE ZERO               TO DTL-AVRG-PREM.                ECS021
04865                                                                   ECS021
04866      IF ACCUM-AVG-ORG-TRM-CNT GREATER ZERO                        ECS021
04867          COMPUTE DTL-AVRG-ORIG-TERM =                             ECS021
04868                  ACCUM-AVG-ORG-TRM / ACCUM-AVG-ORG-TRM-CNT        ECS021
04869      ELSE                                                         ECS021
04870          MOVE ZERO               TO DTL-AVRG-ORIG-TERM.           ECS021
04871                                                                   ECS021
04872      IF ACCUM-AVG-WGHT-TRM-CNT GREATER ZERO                       ECS021
04873          COMPUTE DTL-WGHT-ORIG-TERM =                             ECS021
04874                  ACCUM-AVG-WGHT-TRM / ACCUM-AVG-WGHT-TRM-CNT      ECS021
04875      ELSE                                                         ECS021
04876          MOVE ZERO               TO DTL-WGHT-ORIG-TERM.           ECS021
04877                                                                   ECS021
04878      IF ACCUM-AVG-ISS-CNT GREATER ZERO                            ECS021
04879          COMPUTE DTL-AVRG-ISS-AGE =                               ECS021
04880                  ACCUM-AVG-ISS-AGE / ACCUM-AVG-ISS-CNT            ECS021
04881      ELSE                                                         ECS021
04882          MOVE ZERO               TO DTL-AVRG-ISS-AGE.             ECS021
04883                                                                   ECS021
04884      IF ACCUM-AVG-WGHT-CNT GREATER ZERO                           ECS021
04885          COMPUTE DTL-WGHT-ISS-AGE =                               ECS021
04886                  ACCUM-AVG-WGHT-AGE / ACCUM-AVG-WGHT-CNT          ECS021
04887      ELSE                                                         ECS021
04888          MOVE ZERO               TO DTL-WGHT-ISS-AGE.             ECS021
04889                                                                   ECS021
04890      IF LIFE-INDIVIDUAL                                           ECS021
04891          IF ACCUM-AVG-LIFE-CNT GREATER ZERO                       ECS021
04892              COMPUTE DTL-AVRG-LF-CLAIM =                          ECS021
04893                      ACCUM-AVG-LIFE-CLAIM / ACCUM-AVG-LIFE-CNT    ECS021
04894      ELSE                                                         ECS021
04895          MOVE ZERO               TO DTL-AVRG-LF-CLAIM.            ECS021
04896                                                                   ECS021
04897      IF AH-INDIVIDUAL                                             ECS021
04898          IF ACCUM-AVG-AH-CNT GREATER ZERO                         ECS021
04899              COMPUTE DTL-AVRG-AH-CLAIM =                          ECS021
04900                      ACCUM-AVG-AH-CLAIM / ACCUM-AVG-AH-CNT        ECS021
04901      ELSE                                                         ECS021
04902          MOVE ZERO               TO DTL-AVRG-AH-CLAIM.            ECS021
04903                                                                   ECS021
04904      IF ACCUM-AVG-REFUND-CNT GREATER ZERO                         ECS021
04905          COMPUTE DTL-AVRG-REFUND =                                ECS021
04906                  ACCUM-AVG-REFUND / ACCUM-AVG-REFUND-CNT          ECS021
04907      ELSE                                                         ECS021
04908          MOVE ZERO               TO DTL-AVRG-REFUND.              ECS021
04909                                                                   ECS021
04910      PERFORM 9000-PRINT-DASH-LINE THRU 9010-EXIT.                 ECS021
04911      PERFORM 8500-PRINT-DETAIL    THRU 8599-EXIT.                 ECS021
04912                                                                   ECS021
04913  6770-EXIT.                                                       ECS021
04914      EXIT.                                                        ECS021
04915                                                                   ECS021
04916      EJECT                                                        ECS021
04917                                                                   ECS021
04918  6780-COMPUTE-TOTAL-AVERAGES.                                     ECS021
04919                                                                   ECS021
04920      COMPUTE PREV-IDX = DTE-IDX - 1.                              ECS021
04921                                                                   ECS021
04922      COMPUTE ISS-CNT =                                            ECS021
04923              CMNT-ISS-CNT (BEN-IDX DTE-IDX) -                     ECS021
04924              CMNT-ISS-CNT (BEN-IDX PREV-IDX).                     ECS021
04925                                                                   ECS021
04926      IF ISS-CNT = ZEROS                                           ECS021
04927          MOVE ZERO               TO AVG-PREMIUM                   ECS021
04928          GO TO 6780-ISSUE-COUNT-ZERO.                             ECS021
04929                                                                   ECS021
04930      COMPUTE ISS-PREM =                                           ECS021
04931              CMNT-ISS-PREM (BEN-IDX DTE-IDX) -                    ECS021
04932              CMNT-ISS-PREM (BEN-IDX PREV-IDX).                    ECS021
04933                                                                   ECS021
04934      COMPUTE AVG-PREMIUM = ISS-PREM  /  ISS-CNT.                  ECS021
04935                                                                   ECS021
04936  6780-ISSUE-COUNT-ZERO.                                           ECS021
04937      COMPUTE CNC-CNT =                                            ECS021
04938              CMNT-CNC-CNT (BEN-IDX DTE-IDX) -                     ECS021
04939              CMNT-CNC-CNT (BEN-IDX PREV-IDX).                     ECS021
04940                                                                   ECS021
04941      IF CNC-CNT = ZEROS                                           ECS021
04942          MOVE ZERO               TO AVG-CANCEL-PREMIUM            ECS021
04943          GO TO 6780-CANCEL-COUNT-ZERO.                            ECS021
04944                                                                   ECS021
04945      COMPUTE CNC-PREM =                                           ECS021
04946              CMNT-CNC-PREM (BEN-IDX DTE-IDX) -                    ECS021
04947              CMNT-CNC-PREM (BEN-IDX PREV-IDX).                    ECS021
04948                                                                   ECS021
04949      COMPUTE AVG-CANCEL-PREMIUM = CNC-PREM  /  CNC-CNT.           ECS021
04950                                                                   ECS021
04951  6780-CANCEL-COUNT-ZERO.                                          ECS021
04952      IF LIFE-TOTAL OR BOTH-TOTAL                                  ECS021
04953          COMPUTE LF-CLM-CNT =                                     ECS021
04954                  CMNT-LF-CLM-CNT (BEN-IDX DTE-IDX) -              ECS021
04955                  CMNT-LF-CLM-CNT (BEN-IDX PREV-IDX)               ECS021
04956          COMPUTE LF-CLAIMS-PAID =                                 ECS021
04957                  CMNT-LF-CLM-AMT (BEN-IDX DTE-IDX) -              ECS021
04958                  CMNT-LF-CLM-AMT (BEN-IDX PREV-IDX)               ECS021
04959          IF LF-CLM-CNT NOT = ZEROS                                ECS021
04960              COMPUTE LF-AVG-CLAIM =                               ECS021
04961                      LF-CLAIMS-PAID / LF-CLM-CNT                  ECS021
04962          ELSE                                                     ECS021
04963              MOVE ZEROS          TO LF-AVG-CLAIM                  ECS021
04964      ELSE                                                         ECS021
04965          MOVE ZEROS              TO LF-AVG-CLAIM.                 ECS021
04966                                                                   ECS021
04967      IF AH-TOTAL OR BOTH-TOTAL                                    ECS021
04968          COMPUTE AH-CLM-CNT =                                     ECS021
04969                  CMNT-AH-CLM-CNT (BEN-IDX DTE-IDX) -              ECS021
04970                  CMNT-AH-CLM-CNT (BEN-IDX PREV-IDX)               ECS021
04971          COMPUTE AH-CLAIMS-PAID =                                 ECS021
04972                  CMNT-AH-CLM-AMT (BEN-IDX DTE-IDX) -              ECS021
04973                  CMNT-AH-CLM-AMT (BEN-IDX PREV-IDX)               ECS021
04974          IF AH-CLM-CNT NOT = ZEROS                                ECS021
04975              COMPUTE AH-AVG-CLAIM =                               ECS021
04976                      AH-CLAIMS-PAID / AH-CLM-CNT                  ECS021
04977          ELSE                                                     ECS021
04978              MOVE ZEROS          TO AH-AVG-CLAIM                  ECS021
04979      ELSE                                                         ECS021
04980          MOVE ZEROS              TO AH-AVG-CLAIM.                 ECS021
04981                                                                   ECS021
04982      MOVE BRK-MM (DTE-IDX)       TO DTL-MONTH-2.                  ECS021
04983      MOVE BRK-YY (DTE-IDX)       TO DTL-YEAR-2.                   ECS021
04984                                                                   ECS021
04985      IF CMNT-ISS-CNT (BEN-IDX DTE-IDX) NOT = ZEROS                ECS021
04986          COMPUTE AVG-ORG-TRM ROUNDED =                            ECS021
04987                  CMNT-AVG-ORG-TRM  (BEN-IDX DTE-IDX) /            ECS021
04988                  CMNT-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04989          COMPUTE WGHT-ORG-TRM ROUNDED =                           ECS021
04990                  CMNT-WGHT-ORG-TRM (BEN-IDX DTE-IDX) /            ECS021
04991                  CMNT-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04992          COMPUTE AVG-AGE ROUNDED =                                ECS021
04993                  CMNT-AVG-AGE      (BEN-IDX DTE-IDX) /            ECS021
04994                  CMNT-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04995          COMPUTE WGHT-AGE ROUNDED =                               ECS021
04996                  CMNT-WGHT-AGE     (BEN-IDX DTE-IDX) /            ECS021
04997                  CMNT-ISS-CNT      (BEN-IDX DTE-IDX)              ECS021
04998      ELSE                                                         ECS021
04999      IF CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX) NOT = ZEROS           ECS021
05000          COMPUTE AVG-ORG-TRM ROUNDED =                            ECS021
05001                  CMNT-AVG-ORG-TRM  (BEN-IDX DTE-IDX) /            ECS021
05002                  CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
05003          COMPUTE WGHT-ORG-TRM ROUNDED =                           ECS021
05004                  CMNT-WGHT-ORG-TRM (BEN-IDX DTE-IDX) /            ECS021
05005                  CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
05006          COMPUTE AVG-AGE ROUNDED =                                ECS021
05007                  CMNT-AVG-AGE      (BEN-IDX DTE-IDX) /            ECS021
05008                  CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
05009          COMPUTE WGHT-AGE ROUNDED =                               ECS021
05010                  CMNT-WGHT-AGE     (BEN-IDX DTE-IDX) /            ECS021
05011                  CMNT-ADDED-TO-CNT (BEN-IDX DTE-IDX)              ECS021
05012      ELSE                                                         ECS021
05013          MOVE ZEROS              TO AVG-ORG-TRM                   ECS021
05014                                     WGHT-ORG-TRM                  ECS021
05015                                     AVG-AGE                       ECS021
05016                                     WGHT-AGE.                     ECS021
05017                                                                   ECS021
05018      MOVE CMNT-INFRC-CNT (BEN-IDX DTE-IDX)                        ECS021
05019                                  TO DTL-INFRC-CVRG.               ECS021
05020                                                                   ECS021
05021      MOVE AVG-ORG-TRM            TO DTL-AVRG-ORIG-TERM.           ECS021
05022      MOVE WGHT-ORG-TRM           TO DTL-WGHT-ORIG-TERM.           ECS021
05023      MOVE AVG-AGE                TO DTL-AVRG-ISS-AGE.             ECS021
05024      MOVE WGHT-AGE               TO DTL-WGHT-ISS-AGE.             ECS021
05025                                                                   ECS021
05026      IF LIFE-TOTAL OR BOTH-TOTAL                                  ECS021
05027          MOVE LF-AVG-CLAIM       TO DTL-AVRG-LF-CLAIM.            ECS021
05028                                                                   ECS021
05029      IF AH-TOTAL OR BOTH-TOTAL                                    ECS021
05030          MOVE AH-AVG-CLAIM       TO DTL-AVRG-AH-CLAIM.            ECS021
05031                                                                   ECS021
05032      MOVE AVG-PREMIUM            TO DTL-AVRG-PREM.                ECS021
05033      MOVE AVG-CANCEL-PREMIUM     TO DTL-AVRG-REFUND.              ECS021
05034                                                                   ECS021
05035      PERFORM 8500-PRINT-DETAIL THRU 8599-EXIT.                    ECS021
05036                                                                   ECS021
05037  6780-ACCUM-ALL-PERIODS.                                          ECS021
05038      IF AVG-PREMIUM GREATER ZERO                                  ECS021
05039          ADD AVG-PREMIUM         TO ACCUM-AVG-PREM                ECS021
05040          ADD 1                   TO ACCUM-AVG-PREM-CNT.           ECS021
05041                                                                   ECS021
05042      IF AVG-CANCEL-PREMIUM GREATER ZERO                           ECS021
05043          ADD AVG-CANCEL-PREMIUM  TO ACCUM-AVG-REFUND              ECS021
05044          ADD 1                   TO ACCUM-AVG-REFUND-CNT.         ECS021
05045                                                                   ECS021
05046      IF CMNT-INFRC-CNT (BEN-IDX DTE-IDX) GREATER ZERO             ECS021
05047          ADD CMNT-INFRC-CNT (BEN-IDX DTE-IDX)                     ECS021
05048                                  TO ACCUM-AVG-INFRC               ECS021
05049          ADD 1                   TO ACCUM-AVG-INFRC-CNT.          ECS021
05050                                                                   ECS021
05051      IF AVG-ORG-TRM GREATER ZERO                                  ECS021
05052          ADD AVG-ORG-TRM         TO ACCUM-AVG-ORG-TRM             ECS021
05053          ADD 1                   TO ACCUM-AVG-ORG-TRM-CNT.        ECS021
05054                                                                   ECS021
05055      IF WGHT-ORG-TRM GREATER ZERO                                 ECS021
05056          ADD WGHT-ORG-TRM        TO ACCUM-AVG-WGHT-TRM            ECS021
05057          ADD 1                   TO ACCUM-AVG-WGHT-TRM-CNT.       ECS021
05058                                                                   ECS021
05059      IF AVG-AGE GREATER ZERO                                      ECS021
05060          ADD AVG-AGE             TO ACCUM-AVG-ISS-AGE             ECS021
05061          ADD 1                   TO ACCUM-AVG-ISS-CNT.            ECS021
05062                                                                   ECS021
05063      IF WGHT-AGE GREATER ZERO                                     ECS021
05064          ADD WGHT-AGE            TO ACCUM-AVG-WGHT-AGE            ECS021
05065          ADD 1                   TO ACCUM-AVG-WGHT-CNT.           ECS021
05066                                                                   ECS021
05067      IF LIFE-TOTAL OR BOTH-TOTAL                                  ECS021
05068          IF LF-AVG-CLAIM GREATER ZERO                             ECS021
05069              ADD LF-AVG-CLAIM    TO ACCUM-AVG-LIFE-CLAIM          ECS021
05070              ADD 1               TO ACCUM-AVG-LIFE-CNT.           ECS021
05071                                                                   ECS021
05072      IF AH-TOTAL OR BOTH-TOTAL                                    ECS021
05073          IF AH-AVG-CLAIM GREATER ZERO                             ECS021
05074              ADD AH-AVG-CLAIM    TO ACCUM-AVG-AH-CLAIM            ECS021
05075              ADD 1               TO ACCUM-AVG-AH-CNT.             ECS021
05076                                                                   ECS021
05077      IF DTE-IDX LESS +15                                          ECS021
05078          GO TO 6780-EXIT.                                         ECS021
05079                                                                   ECS021
05080      MOVE AVERAGE-DESC           TO DTL-LINE-2-DESC.              ECS021
05081                                                                   ECS021
05082      IF ACCUM-AVG-INFRC-CNT GREATER ZERO                          ECS021
05083          COMPUTE DTL-INFRC-CVRG =                                 ECS021
05084                  ACCUM-AVG-INFRC / ACCUM-AVG-INFRC-CNT            ECS021
05085      ELSE                                                         ECS021
05086          MOVE ZERO               TO DTL-INFRC-CVRG.               ECS021
05087                                                                   ECS021
05088      IF ACCUM-AVG-PREM-CNT GREATER ZERO                           ECS021
05089          COMPUTE DTL-AVRG-PREM =                                  ECS021
05090                  ACCUM-AVG-PREM / ACCUM-AVG-PREM-CNT              ECS021
05091      ELSE                                                         ECS021
05092          MOVE ZERO               TO DTL-AVRG-PREM.                ECS021
05093                                                                   ECS021
05094      IF ACCUM-AVG-ORG-TRM-CNT GREATER ZERO                        ECS021
05095          COMPUTE DTL-AVRG-ORIG-TERM ROUNDED =                     ECS021
05096                  ACCUM-AVG-ORG-TRM / ACCUM-AVG-ORG-TRM-CNT        ECS021
05097      ELSE                                                         ECS021
05098          MOVE ZERO               TO DTL-AVRG-ORIG-TERM.           ECS021
05099                                                                   ECS021
05100      IF ACCUM-AVG-WGHT-TRM-CNT GREATER ZERO                       ECS021
05101          COMPUTE DTL-WGHT-ORIG-TERM ROUNDED =                     ECS021
05102                  ACCUM-AVG-WGHT-TRM / ACCUM-AVG-WGHT-TRM-CNT      ECS021
05103      ELSE                                                         ECS021
05104          MOVE ZERO               TO DTL-WGHT-ORIG-TERM.           ECS021
05105                                                                   ECS021
05106      IF ACCUM-AVG-ISS-CNT GREATER ZERO                            ECS021
05107          COMPUTE DTL-AVRG-ISS-AGE ROUNDED =                       ECS021
05108                  ACCUM-AVG-ISS-AGE / ACCUM-AVG-ISS-CNT            ECS021
05109      ELSE                                                         ECS021
05110          MOVE ZERO               TO DTL-AVRG-ISS-AGE.             ECS021
05111                                                                   ECS021
05112      IF ACCUM-AVG-WGHT-CNT GREATER ZERO                           ECS021
05113          COMPUTE DTL-WGHT-ISS-AGE ROUNDED =                       ECS021
05114                  ACCUM-AVG-WGHT-AGE / ACCUM-AVG-WGHT-CNT          ECS021
05115      ELSE                                                         ECS021
05116          MOVE ZERO               TO DTL-WGHT-ISS-AGE.             ECS021
05117                                                                   ECS021
05118      IF LIFE-TOTAL OR BOTH-TOTAL                                  ECS021
05119          IF ACCUM-AVG-LIFE-CNT GREATER ZERO                       ECS021
05120              COMPUTE DTL-AVRG-LF-CLAIM =                          ECS021
05121                      ACCUM-AVG-LIFE-CLAIM / ACCUM-AVG-LIFE-CNT    ECS021
05122      ELSE                                                         ECS021
05123          MOVE ZERO               TO DTL-AVRG-LF-CLAIM.            ECS021
05124                                                                   ECS021
05125      IF AH-TOTAL OR BOTH-TOTAL                                    ECS021
05126          IF ACCUM-AVG-AH-CNT GREATER ZERO                         ECS021
05127              COMPUTE DTL-AVRG-AH-CLAIM =                          ECS021
05128                      ACCUM-AVG-AH-CLAIM / ACCUM-AVG-AH-CNT        ECS021
05129      ELSE                                                         ECS021
05130          MOVE ZERO               TO DTL-AVRG-AH-CLAIM.            ECS021
05131                                                                   ECS021
05132      IF ACCUM-AVG-REFUND-CNT GREATER ZERO                         ECS021
05133          COMPUTE DTL-AVRG-REFUND =                                ECS021
05134                  ACCUM-AVG-REFUND / ACCUM-AVG-REFUND-CNT          ECS021
05135      ELSE                                                         ECS021
05136          MOVE ZERO               TO DTL-AVRG-REFUND.              ECS021
05137                                                                   ECS021
05138      PERFORM 9000-PRINT-DASH-LINE THRU 9010-EXIT.                 ECS021
05139      PERFORM 8500-PRINT-DETAIL    THRU 8599-EXIT.                 ECS021
05140                                                                   ECS021
05141  6780-EXIT.                                                       ECS021
05142      EXIT.                                                        ECS021
05143                                                                   ECS021
05144      EJECT                                                        ECS021
05145  6800-COMPUTE-DTL-PERCENTS.                                       ECS021
05146      IF CANCEL-RATIO GREATER +9.999                               ECS021
05147          MOVE +9.999             TO CANCEL-RATIO.                 ECS021
05148                                                                   ECS021
05149      IF CANCEL-RATIO LESS -9.999                                  ECS021
05150          MOVE -9.999             TO CANCEL-RATIO.                 ECS021
05151                                                                   ECS021
05152      MULTIPLY CANCEL-RATIO BY +100 GIVING DTL-CNCL-RATIO.         ECS021
05153                                                                   ECS021
05154      IF LOSS-RATIO GREATER +9.999                                 ECS021
05155          MOVE +9.999             TO LOSS-RATIO.                   ECS021
05156                                                                   ECS021
05157      IF LOSS-RATIO LESS -9.999                                    ECS021
05158          MOVE -9.999             TO LOSS-RATIO.                   ECS021
05159                                                                   ECS021
05160      MULTIPLY LOSS-RATIO   BY +100 GIVING DTL-LOSS-RATIO.         ECS021
05161                                                                   ECS021
05162      IF COMPEN-PERCENT GREATER +9.999                             ECS021
05163          MOVE +9.999             TO COMPEN-PERCENT.               ECS021
05164                                                                   ECS021
05165      IF COMPEN-PERCENT LESS -9.999                                ECS021
05166          MOVE -9.999             TO COMPEN-PERCENT.               ECS021
05167                                                                   ECS021
05168      MULTIPLY COMPEN-PERCENT BY +100 GIVING DTL-COMP-PRCNT.       ECS021
05169                                                                   ECS021
05170      IF EXP-EARN-RATIO GREATER +9.999                             ECS021
05171          MOVE +9.999             TO EXP-EARN-RATIO.               ECS021
05172                                                                   ECS021
05173      IF EXP-EARN-RATIO LESS -9.999                                ECS021
05174          MOVE -9.999             TO EXP-EARN-RATIO.               ECS021
05175                                                                   ECS021
05176      MULTIPLY EXP-EARN-RATIO BY +100 GIVING DTL-EXP-EARN-RATIO.   ECS021
05177                                                                   ECS021
05178      IF PROFIT-PERCENT GREATER +9.9999                            ECS021
05179          MOVE +9.9999            TO PROFIT-PERCENT.               ECS021
05180                                                                   ECS021
05181      IF PROFIT-PERCENT LESS -9.9999                               ECS021
05182          MOVE -9.9999            TO PROFIT-PERCENT.               ECS021
05183                                                                   ECS021
05184      MULTIPLY PROFIT-PERCENT BY +100 GIVING DTL-PROFIT-PRCNT.     ECS021
05185                                                                   ECS021
05186  6800-EXIT.                                                       ECS021
05187      EXIT.                                                        ECS021
05188                                                                   ECS021
05189      EJECT                                                        ECS021
05190  6900-CALC-TOTAL-EXCEPTIONS.                                      ECS021
05191                                                                   ECS021
05192      MOVE 'Y' TO WS-1ST-EXCEPTION-PRT-SW.                         ECS021
05193      MOVE +3     TO   BEN-NDX.                                    ECS021
05194      MOVE +14    TO   DTE-NDX.                                    ECS021
05195                                                                   ECS021
05196  6905-NEXT.                                                       ECS021
05197                                                                   ECS021
05198      COMPUTE PREV-NDX = DTE-NDX - 1.                              ECS021
05199                                                                   ECS021
05200      COMPUTE EXC-ISS-CNT =                                        ECS021
05201              CMNT-ISS-CNT (BEN-NDX DTE-NDX) -                     ECS021
05202              CMNT-ISS-CNT (BEN-NDX PREV-NDX).                     ECS021
05203                                                                   ECS021
05204      COMPUTE EXC-CNC-CNT =                                        ECS021
05205              CMNT-CNC-CNT (BEN-NDX DTE-NDX) -                     ECS021
05206              CMNT-CNC-CNT (BEN-NDX PREV-NDX).                     ECS021
05207                                                                   ECS021
05208      COMPUTE EXC-CVRGS =                                          ECS021
05209              (EXC-ISS-CNT - EXC-CNC-CNT).                         ECS021
05210                                                                   ECS021
05211      COMPUTE EXC-EARND-PREM =                                     ECS021
05212              CMNT-EARND-PREM (BEN-NDX DTE-NDX) -                  ECS021
05213              CMNT-EARND-PREM (BEN-NDX PREV-NDX).                  ECS021
05214                                                                   ECS021
05215      COMPUTE EXC-ISS-PREM =                                       ECS021
05216              CMNT-ISS-PREM (BEN-NDX DTE-NDX) -                    ECS021
05217              CMNT-ISS-PREM (BEN-NDX PREV-NDX).                    ECS021
05218                                                                   ECS021
05219      COMPUTE EXC-CNC-PREM =                                       ECS021
05220              CMNT-CNC-PREM (BEN-NDX DTE-NDX) -                    ECS021
05221              CMNT-CNC-PREM (BEN-NDX PREV-NDX).                    ECS021
05222                                                                   ECS021
05223      COMPUTE EXC-WRITTEN-PREM =                                   ECS021
05224              EXC-ISS-PREM - EXC-CNC-PREM.                         ECS021
05225                                                                   ECS021
05226      IF DTE-NDX LESS THAN +15                                     ECS021
05227          MOVE EXC-ISS-CNT      TO  EXC-PMO-ISS-CNT                ECS021
05228          MOVE EXC-CVRGS        TO  EXC-PMO-CVRGS                  ECS021
05229          MOVE EXC-EARND-PREM TO    EXC-PMO-EARND-PREM             ECS021
05230          MOVE EXC-WRITTEN-PREM TO  EXC-PMO-WRITTEN-PREM           ECS021
05231          ADD +1 TO DTE-NDX                                        ECS021
05232          GO TO 6905-NEXT.                                         ECS021
05233                                                                   ECS021
05234      MOVE EXC-ISS-CNT          TO  EXC-CMO-ISS-CNT.               ECS021
05235      MOVE EXC-CVRGS            TO  EXC-CMO-CVRGS                  ECS021
05236      MOVE EXC-EARND-PREM       TO  EXC-CMO-EARND-PREM.            ECS021
05237      MOVE EXC-WRITTEN-PREM     TO  EXC-CMO-WRITTEN-PREM.          ECS021
05238                                                                   ECS021
05239      MOVE ZEROS TO EXC-CVRGS-DIFF.                                ECS021
05240      IF EXC-PMO-ISS-CNT GREATER THAN EXC-CMO-ISS-CNT              ECS021
05241          COMPUTE EXC-CVRGS-DIFF = EXC-PMO-ISS-CNT -               ECS021
05242                                   EXC-CMO-ISS-CNT.                ECS021
05243                                                                   ECS021
05244      MOVE ZEROS TO EXC-WRITTEN-PREM-PCT.                          ECS021
05245      IF EXC-PMO-WRITTEN-PREM NOT EQUAL ZEROS                      ECS021
05246          IF EXC-PMO-WRITTEN-PREM GREATER THAN EXC-CMO-WRITTEN-PREMECS021
05247              COMPUTE EXC-WRITTEN-PREM-PCT ROUNDED =               ECS021
05248                  ((EXC-PMO-WRITTEN-PREM - EXC-CMO-WRITTEN-PREM) / ECS021
05249                             EXC-PMO-WRITTEN-PREM) * 100           ECS021
05250          ELSE                                                     ECS021
05251              COMPUTE EXC-WRITTEN-PREM-PCT ROUNDED =               ECS021
05252                  ((EXC-CMO-WRITTEN-PREM - EXC-PMO-WRITTEN-PREM) / ECS021
05253                             EXC-PMO-WRITTEN-PREM) * 100.          ECS021
05254                                                                   ECS021
05255      MOVE ZEROS                  TO EXC-CANCEL-RATIO.             ECS021
05256      IF EXC-ISS-PREM NOT EQUAL ZEROS                              ECS021
05257          COMPUTE EXC-CANCEL-RATIO ROUNDED =                       ECS021
05258              (EXC-CNC-PREM    /  EXC-ISS-PREM) * +100             ECS021
05259      ELSE                                                         ECS021
05260          MOVE -100.00          TO EXC-CANCEL-RATIO.               ECS021
05261                                                                   ECS021
05262      MOVE +15    TO   DTE-NDX.                                    ECS021
05263      MOVE +3     TO   PREV-NDX.                                   ECS021
05264                                                                   ECS021
05265      COMPUTE EXC-L12-EARND-PREM =                                 ECS021
05266              CMNT-EARND-PREM (BEN-NDX DTE-NDX) -                  ECS021
05267              CMNT-EARND-PREM (BEN-NDX PREV-NDX).                  ECS021
05268                                                                   ECS021
05269      COMPUTE EXC-L12-NET-COMPEN =                                 ECS021
05270              CMNT-NET-COMPEN (BEN-NDX DTE-NDX) -                  ECS021
05271              CMNT-NET-COMPEN (BEN-NDX PREV-NDX).                  ECS021
05272                                                                   ECS021
05273      COMPUTE EXC-L12-CLAIM-AMT =                                  ECS021
05274                  CMNT-LF-CLM-AMT (BEN-NDX DTE-NDX) -              ECS021
05275                  CMNT-LF-CLM-AMT (BEN-NDX PREV-NDX) +             ECS021
05276                  CMNT-AH-CLM-AMT (BEN-NDX DTE-NDX) -              ECS021
05277                  CMNT-AH-CLM-AMT (BEN-NDX PREV-NDX).              ECS021
05278                                                                   ECS021
05279      COMPUTE EXC-RETENTION-CALC =                                 ECS021
05280                                   EXC-L12-EARND-PREM -            ECS021
05281                                   EXC-L12-NET-COMPEN -            ECS021
05282                                   EXC-L12-CLAIM-AMT.              ECS021
05283                                                                   ECS021
05284      MOVE +3     TO   DTE-NDX.                                    ECS021
05285      MOVE +1     TO   PREV-NDX.                                   ECS021
05286                                                                   ECS021
05287      COMPUTE EXC-P12-EARND-PREM =                                 ECS021
05288              CMNT-EARND-PREM (BEN-NDX DTE-NDX) -                  ECS021
05289              CMNT-EARND-PREM (BEN-NDX PREV-NDX).                  ECS021
05290                                                                   ECS021
05291      MOVE ZEROS TO EXC-EARND-PREM-DECR-PCT.                       ECS021
05292      IF EXC-P12-EARND-PREM NOT EQUAL ZEROS                        ECS021
05293          IF EXC-P12-EARND-PREM GREATER THAN EXC-L12-EARND-PREM    ECS021
05294              COMPUTE EXC-EARND-PREM-DECR-PCT ROUNDED =            ECS021
05295                  ((EXC-P12-EARND-PREM - EXC-L12-EARND-PREM) /     ECS021
05296                             EXC-P12-EARND-PREM) * 100.            ECS021
05297                                                                   ECS021
05298      IF  WS-RETENTION-LIMIT NUMERIC                               ECS021
05299           NEXT SENTENCE                                           ECS021
05300      ELSE                                                         ECS021
05301           MOVE ZEROS TO WS-RETENTION-LIMIT.                       ECS021
05302                                                                   ECS021
05303      IF  (WS-RETENTION-LIMIT GREATER THAN ZEROS) AND              ECS021
05304          (EXC-RETENTION-CALC LESS THAN WS-RETENTION-LIMIT)        ECS021
05305            MOVE EXC-RETENTION-CALC  TO EXP-RETENTION-CALC         ECS021
05306            MOVE WS-RETENTION-LIMIT TO EXP-RETENTION-LIMIT         ECS021
05307            MOVE EXP-RETENTION-MSG  TO HOLD-EXCEPTION-PRT          ECS021
05308            PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.       ECS021
05309                                                                   ECS021
05310      IF  (WS-ISS-COUNT-DIFF GREATER THAN ZEROS)  AND              ECS021
05311          (EXC-CVRGS-DIFF GREATER THAN WS-ISS-COUNT-DIFF)          ECS021
05312            MOVE EXC-CMO-ISS-CNT TO  EXP-CMO-ISS-CNT               ECS021
05313            MOVE EXC-PMO-ISS-CNT TO  EXP-PMO-ISS-CNT               ECS021
05314            MOVE WS-ISS-COUNT-DIFF  TO EXP-ISS-CNT-LIMIT           ECS021
05315            MOVE EXP-ISS-CNT-MSG TO HOLD-EXCEPTION-PRT             ECS021
05316            PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.       ECS021
05317                                                                   ECS021
05318      IF  (WS-SINGLE-MO-PREM-PCT GREATER THAN ZEROS)  AND          ECS021
05319          (EXC-WRITTEN-PREM-PCT > WS-SINGLE-MO-PREM-PCT)           ECS021
05320            MOVE EXC-CMO-WRITTEN-PREM TO EXP-CMO-WRITTEN-PREM      ECS021
05321            MOVE EXC-PMO-WRITTEN-PREM TO EXP-PMO-WRITTEN-PREM      ECS021
05322            MOVE EXC-WRITTEN-PREM-PCT TO EXP-WRITTEN-PREM-PCT      ECS021
05323            MOVE EXP-PRM-CHG-MSG TO   HOLD-EXCEPTION-PRT           ECS021
05324            PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.       ECS021
05325                                                                   ECS021
05326      IF  (WS-EARN-PREM-DECR-PCT GREATER THAN ZEROS)  AND          ECS021
05327          (EXC-EARND-PREM-DECR-PCT > WS-EARN-PREM-DECR-PCT)        ECS021
05328            MOVE EXC-L12-EARND-PREM TO EXP-L12-EARND-PRM           ECS021
05329            MOVE EXC-P12-EARND-PREM TO EXP-P12-EARND-PRM           ECS021
05330            MOVE EXC-EARND-PREM-DECR-PCT TO EXP-PRM-DECR-PCT       ECS021
05331            MOVE EXP-PRM-DECR-MSG TO   HOLD-EXCEPTION-PRT          ECS021
05332            PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.       ECS021
05333                                                                   ECS021
05334      IF  (WS-CANCELLATION-RATIO GREATER THAN ZEROS)  AND          ECS021
05335          (EXC-CANCEL-RATIO > WS-CANCELLATION-RATIO)               ECS021
05336            MOVE EXC-CNC-PREM TO EXP-CNC-PREM                      ECS021
05337            MOVE EXC-ISS-PREM TO EXP-ISS-PREM                      ECS021
05338            MOVE EXC-CANCEL-RATIO TO EXP-CANCEL-RATIO              ECS021
05339            MOVE EXP-CANCEL-RATIO-MSG TO   HOLD-EXCEPTION-PRT      ECS021
05340            PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.       ECS021
05341                                                                   ECS021
05342      IF CF-EXCEPTION-LIST-REQUESTED                               ECS021
05343          MOVE 'LT'                   TO TOTAL-TYPE-SWITCH         ECS021
05344          PERFORM 7000-COMPUTE-COVG-EXCEPTIONS THRU 7000-EXIT.     ECS021
05345                                                                   ECS021
05346      IF CF-EXCEPTION-LIST-REQUESTED                               ECS021
05347          MOVE 'AT'                   TO TOTAL-TYPE-SWITCH         ECS021
05348          PERFORM 7000-COMPUTE-COVG-EXCEPTIONS THRU 7000-EXIT.     ECS021
05349                                                                   ECS021
05350  6900-EXIT.                                                       ECS021
05351      EXIT.                                                        ECS021
05352                                                                   ECS021
05353      EJECT                                                        ECS021
05354  7000-COMPUTE-COVG-EXCEPTIONS.                                    ECS021
05355                                                                   ECS021
05356      IF LIFE-TOTAL                                                ECS021
05357          MOVE +1        TO  BEN-NDX                               ECS021
05358          MOVE ' LIFE '  TO  EXP-COVG-DESC-1, EXP-COVG-DESC-2      ECS021
05359                             EXP-COVG-DESC-3, EXP-COVG-DESC-4      ECS021
05360                             EXP-COVG-DESC-5, EXP-COVG-DESC-6      ECS021
05361                             EXP-COVG-DESC-7, EXP-COVG-DESC-8      ECS021
05362                             EXP-COVG-DESC-9, EXP-COVG-DESC-10     ECS021
05363      ELSE                                                         ECS021
05364          MOVE +2        TO  BEN-NDX                               ECS021
05365          MOVE 'A / H '  TO  EXP-COVG-DESC-1, EXP-COVG-DESC-2      ECS021
05366                             EXP-COVG-DESC-3, EXP-COVG-DESC-4      ECS021
05367                             EXP-COVG-DESC-5, EXP-COVG-DESC-6      ECS021
05368                             EXP-COVG-DESC-7, EXP-COVG-DESC-8      ECS021
05369                             EXP-COVG-DESC-9, EXP-COVG-DESC-10.    ECS021
05370                                                                   ECS021
05371      MOVE +15    TO   DTE-NDX.                                    ECS021
05372                                                                   ECS021
05373      PERFORM 7005-CALC-COVG-EXCEPTIONS THRU 7005-EXIT.            ECS021
05374                                                                   ECS021
05375      IF  LIFE-TOTAL                                               ECS021
05376          IF (WS-LF-LOSS-RATIO-PCT GREATER THAN ZEROS)  AND        ECS021
05377             (LOSS-RATIO > WS-LF-LOSS-RATIO-PCT)                   ECS021
05378              MOVE WS-LF-LOSS-RATIO-PCT TO EXP-LOSS-RATIO-LIMIT    ECS021
05379              MOVE LOSS-RATIO TO EXP-LOSS-RATIO                    ECS021
05380              MOVE EXP-LOSS-RATIO-MSG TO   HOLD-EXCEPTION-PRT      ECS021
05381              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05382                                                                   ECS021
05383      IF  LIFE-TOTAL                                               ECS021
05384          IF (WS-LF-PERIOD-PROFIT GREATER THAN ZEROS)  AND         ECS021
05385             (PROFIT-PERCENT > WS-LF-PERIOD-PROFIT)                ECS021
05386              MOVE PROFIT-PERCENT TO EXP-PROFIT-PERCENT            ECS021
05387              MOVE EXP-PROFIT-PERCENT-MSG TO  HOLD-EXCEPTION-PRT   ECS021
05388              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05389                                                                   ECS021
05390      IF AH-TOTAL                                                  ECS021
05391          IF (WS-AH-LOSS-RATIO-PCT GREATER THAN ZEROS)  AND        ECS021
05392             (LOSS-RATIO > WS-AH-LOSS-RATIO-PCT)                   ECS021
05393              MOVE WS-AH-LOSS-RATIO-PCT TO EXP-LOSS-RATIO-LIMIT    ECS021
05394              MOVE LOSS-RATIO TO EXP-LOSS-RATIO                    ECS021
05395              MOVE EXP-LOSS-RATIO-MSG TO   HOLD-EXCEPTION-PRT      ECS021
05396              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05397                                                                   ECS021
05398      IF  AH-TOTAL                                                 ECS021
05399          IF (WS-AH-PERIOD-PROFIT GREATER THAN ZEROS)  AND         ECS021
05400             (PROFIT-PERCENT > WS-AH-PERIOD-PROFIT)                ECS021
05401              MOVE PROFIT-PERCENT TO EXP-PROFIT-PERCENT            ECS021
05402              MOVE EXP-PROFIT-PERCENT-MSG TO  HOLD-EXCEPTION-PRT   ECS021
05403              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05404                                                                   ECS021
05405      MOVE +15    TO   DTE-NDX.                                    ECS021
05406      MOVE +3     TO   PREV-NDX.                                   ECS021
05407                                                                   ECS021
05408      PERFORM 7005-CALC-COVG-PERIOD THRU 7005-EXIT.                ECS021
05409                                                                   ECS021
05410      MOVE LOSS-RATIO TO EXC-L12-LOSS-RATIO.                       ECS021
05411      MOVE PROFIT-PERCENT TO EXC-L12-PROFIT-PERCENT.               ECS021
05412                                                                   ECS021
05413      MOVE +3     TO   DTE-NDX.                                    ECS021
05414      MOVE +1     TO   PREV-NDX.                                   ECS021
05415                                                                   ECS021
05416      PERFORM 7005-CALC-COVG-PERIOD THRU 7005-EXIT.                ECS021
05417                                                                   ECS021
05418      MOVE LOSS-RATIO TO EXC-P12-LOSS-RATIO.                       ECS021
05419      MOVE PROFIT-PERCENT TO EXC-P12-PROFIT-PERCENT.               ECS021
05420                                                                   ECS021
05421      COMPUTE EXC-LOSS-RATIO-DIFF =                                ECS021
05422         EXC-L12-LOSS-RATIO - EXC-P12-LOSS-RATIO.                  ECS021
05423                                                                   ECS021
05424      IF EXC-LOSS-RATIO-DIFF NEGATIVE                              ECS021
05425         COMPUTE EXC-LOSS-RATIO-DIFF = EXC-LOSS-RATIO-DIFF * -1.   ECS021
05426                                                                   ECS021
05427      COMPUTE EXC-PROFIT-PCT-DIFF =                                ECS021
05428         EXC-L12-PROFIT-PERCENT - EXC-P12-PROFIT-PERCENT.          ECS021
05429                                                                   ECS021
05430      IF EXC-PROFIT-PCT-DIFF NEGATIVE                              ECS021
05431         COMPUTE EXC-PROFIT-PCT-DIFF = EXC-PROFIT-PCT-DIFF * -1.   ECS021
05432                                                                   ECS021
05433      IF LIFE-TOTAL                                                ECS021
05434          IF (WS-LF-LTM-LOSS-RATIO GREATER THAN ZEROS)  AND        ECS021
05435             (EXC-LOSS-RATIO-DIFF > WS-LF-LTM-LOSS-RATIO)          ECS021
05436              MOVE EXC-L12-LOSS-RATIO TO EXP-L12-LOSS-RATIO        ECS021
05437              MOVE EXC-P12-LOSS-RATIO TO EXP-P12-LOSS-RATIO        ECS021
05438              MOVE EXC-LOSS-RATIO-DIFF TO EXP-LOSS-RATIO-DIFF      ECS021
05439              MOVE EXP-LTM-LOSS-RATIO-MSG TO   HOLD-EXCEPTION-PRT  ECS021
05440              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05441                                                                   ECS021
05442      IF LIFE-TOTAL                                                ECS021
05443          IF (WS-LF-LTM-PROFIT-PCT GREATER THAN ZEROS)  AND        ECS021
05444             (EXC-PROFIT-PCT-DIFF > WS-LF-LTM-PROFIT-PCT)          ECS021
05445              MOVE EXC-PROFIT-PCT-DIFF TO EXP-PROFIT-PCT-DIFF      ECS021
05446              MOVE EXC-L12-PROFIT-PERCENT TO EXP-L12-PROFIT-PCT    ECS021
05447              MOVE EXC-P12-PROFIT-PERCENT TO EXP-P12-PROFIT-PCT    ECS021
05448              MOVE EXP-LTM-PERIOD-PROFIT-MSG TO HOLD-EXCEPTION-PRT ECS021
05449              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05450                                                                   ECS021
05451      IF AH-TOTAL                                                  ECS021
05452          IF (WS-AH-LTM-LOSS-RATIO GREATER THAN ZEROS)  AND        ECS021
05453             (EXC-LOSS-RATIO-DIFF > WS-AH-LTM-LOSS-RATIO)          ECS021
05454              MOVE EXC-L12-LOSS-RATIO TO EXP-L12-LOSS-RATIO        ECS021
05455              MOVE EXC-P12-LOSS-RATIO TO EXP-P12-LOSS-RATIO        ECS021
05456              MOVE EXC-LOSS-RATIO-DIFF TO EXP-LOSS-RATIO-DIFF      ECS021
05457              MOVE EXP-LTM-LOSS-RATIO-MSG TO   HOLD-EXCEPTION-PRT  ECS021
05458              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05459                                                                   ECS021
05460      IF AH-TOTAL                                                  ECS021
05461          IF (WS-AH-LTM-PROFIT-PCT GREATER THAN ZEROS)  AND        ECS021
05462             (EXC-PROFIT-PCT-DIFF > WS-AH-LTM-PROFIT-PCT)          ECS021
05463              MOVE EXC-PROFIT-PCT-DIFF TO EXP-PROFIT-PCT-DIFF      ECS021
05464              MOVE EXC-L12-PROFIT-PERCENT TO EXP-L12-PROFIT-PCT    ECS021
05465              MOVE EXC-P12-PROFIT-PERCENT TO EXP-P12-PROFIT-PCT    ECS021
05466              MOVE EXP-LTM-PERIOD-PROFIT-MSG TO HOLD-EXCEPTION-PRT ECS021
05467              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05468                                                                   ECS021
05469      MOVE ZEROS TO  EXC-ACC-AVG-INFRC                             ECS021
05470                     EXC-ACC-AVG-INFRC-CNT                         ECS021
05471                     EXC-ACC-AVG-ORG-TRM                           ECS021
05472                     EXC-ACC-AVG-ORG-TRM-CNT                       ECS021
05473                     EXC-ACC-AVG-WGHT-TRM                          ECS021
05474                     EXC-ACC-AVG-WGHT-TRM-CNT                      ECS021
05475                     EXC-ACC-AVG-ISS-AGE                           ECS021
05476                     EXC-ACC-AVG-ISS-CNT                           ECS021
05477                     EXC-ACC-AVG-WGHT-AGE                          ECS021
05478                     EXC-ACC-AVG-WGHT-CNT.                         ECS021
05479                                                                   ECS021
05480      PERFORM 7050-CALC-AVG-EXCEPTIONS THRU 7050-EXIT              ECS021
05481          VARYING DTE-NDX FROM +4 BY +1                            ECS021
05482            UNTIL DTE-NDX GREATER +15.                             ECS021
05483                                                                   ECS021
05484      IF LIFE-TOTAL                                                ECS021
05485          IF (WS-LF-AVG-AGE-MAX GREATER THAN ZEROS)  AND           ECS021
05486             (EXC-AVRG-ISS-AGE > WS-LF-AVG-AGE-MAX)                ECS021
05487              MOVE WS-LF-AVG-AGE-MAX TO EXP-MAX-ISS-AGE            ECS021
05488              MOVE EXC-AVRG-ISS-AGE TO EXP-AVG-ISS-AGE             ECS021
05489              MOVE EXP-MAX-ISS-AGE-MSG TO HOLD-EXCEPTION-PRT       ECS021
05490              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05491                                                                   ECS021
05492      IF AH-TOTAL                                                  ECS021
05493          IF (WS-AH-AVG-AGE-MAX GREATER THAN ZEROS)  AND           ECS021
05494             (EXC-AVRG-ISS-AGE > WS-AH-AVG-AGE-MAX)                ECS021
05495              MOVE WS-AH-AVG-AGE-MAX TO EXP-MAX-ISS-AGE            ECS021
05496              MOVE EXC-AVRG-ISS-AGE TO EXP-AVG-ISS-AGE             ECS021
05497              MOVE EXP-MAX-ISS-AGE-MSG TO HOLD-EXCEPTION-PRT       ECS021
05498              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05499                                                                   ECS021
05500      MOVE ZEROS TO EXC-INFRC-PCT.                                 ECS021
05501                                                                   ECS021
05502      IF EXC-INFRC-CVRG GREATER THAN ZEROS                         ECS021
05503         IF EXC-INFRC-CVRG GREATER THAN EXC-CMO-INFRC-CVRG         ECS021
05504              COMPUTE EXC-INFRC-PCT ROUNDED =                      ECS021
05505                    ((EXC-INFRC-CVRG - EXC-CMO-INFRC-CVRG) /       ECS021
05506                        EXC-INFRC-CVRG) * 100.                     ECS021
05507                                                                   ECS021
05508      MOVE ZEROS TO EXC-ORIG-TERM-PCT.                             ECS021
05509                                                                   ECS021
05510      IF EXC-AVRG-ORIG-TERM GREATER THAN ZEROS                     ECS021
05511         IF EXC-CMO-ORG-TRM GREATER THAN EXC-AVRG-ORIG-TERM        ECS021
05512              COMPUTE EXC-ORIG-TERM-PCT ROUNDED =                  ECS021
05513                    ((EXC-CMO-ORG-TRM - EXC-AVRG-ORIG-TERM) /      ECS021
05514                            EXC-CMO-ORG-TRM) * 100.                ECS021
05515                                                                   ECS021
05516      MOVE ZEROS TO EXC-ISS-AGE-PCT.                               ECS021
05517                                                                   ECS021
05518      IF EXC-AVRG-ISS-AGE GREATER THAN ZEROS                       ECS021
05519         IF EXC-CMO-AVG-AGE GREATER THAN EXC-AVRG-ISS-AGE          ECS021
05520              COMPUTE EXC-ISS-AGE-PCT ROUNDED =                    ECS021
05521                    ((EXC-CMO-AVG-AGE - EXC-AVRG-ISS-AGE) /        ECS021
05522                            EXC-CMO-AVG-AGE) * 100.                ECS021
05523                                                                   ECS021
05524      MOVE ZEROS TO EXC-WGHT-TRM-PCT.                              ECS021
05525                                                                   ECS021
05526      IF EXC-WGHT-ORIG-TERM GREATER THAN ZEROS                     ECS021
05527         IF EXC-CMO-WGHT-ORG-TRM GREATER THAN EXC-WGHT-ORIG-TERM   ECS021
05528              COMPUTE EXC-WGHT-TRM-PCT ROUNDED =                   ECS021
05529                 ((EXC-CMO-WGHT-ORG-TRM - EXC-WGHT-ORIG-TERM) /    ECS021
05530                         EXC-CMO-WGHT-ORG-TRM) * 100.              ECS021
05531                                                                   ECS021
05532      MOVE ZEROS TO EXC-WGHT-AGE-PCT.                              ECS021
05533                                                                   ECS021
05534      IF EXC-WGHT-ISS-AGE GREATER THAN ZEROS                       ECS021
05535         IF EXC-CMO-WGHT-AGE GREATER THAN EXC-WGHT-ISS-AGE         ECS021
05536              COMPUTE EXC-WGHT-AGE-PCT ROUNDED =                   ECS021
05537                 ((EXC-CMO-WGHT-AGE - EXC-WGHT-ISS-AGE) /          ECS021
05538                         EXC-CMO-WGHT-AGE) * 100.                  ECS021
05539                                                                   ECS021
05540      IF LIFE-TOTAL                                                ECS021
05541         IF (WS-LF-LTM-AGE-PCT GREATER THAN ZEROS)  AND            ECS021
05542            (EXC-ISS-AGE-PCT > WS-LF-LTM-AGE-PCT)                  ECS021
05543             MOVE EXC-AVRG-ISS-AGE TO EXP-LTM-ISS-AGE-AVG          ECS021
05544             MOVE EXC-CMO-AVG-AGE TO EXP-CMO-ISS-AGE               ECS021
05545             MOVE EXC-ISS-AGE-PCT TO EXP-ISS-AGE-DIFF              ECS021
05546             MOVE EXP-LTM-AGE-MSG TO HOLD-EXCEPTION-PRT            ECS021
05547             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05548                                                                   ECS021
05549      IF AH-TOTAL                                                  ECS021
05550         IF (WS-AH-LTM-AGE-PCT GREATER THAN ZEROS)  AND            ECS021
05551            (EXC-ISS-AGE-PCT > WS-AH-LTM-AGE-PCT)                  ECS021
05552             MOVE EXC-AVRG-ISS-AGE TO EXP-LTM-ISS-AGE-AVG          ECS021
05553             MOVE EXC-CMO-AVG-AGE TO EXP-CMO-ISS-AGE               ECS021
05554             MOVE EXC-ISS-AGE-PCT TO EXP-ISS-AGE-DIFF              ECS021
05555             MOVE EXP-LTM-AGE-MSG TO HOLD-EXCEPTION-PRT            ECS021
05556             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05557                                                                   ECS021
05558      IF LIFE-TOTAL                                                ECS021
05559         IF (WS-LF-AGE-AVG-WEIGHTED GREATER THAN ZEROS)  AND       ECS021
05560            (EXC-WGHT-AGE-PCT > WS-LF-AGE-AVG-WEIGHTED)            ECS021
05561             MOVE EXC-WGHT-ISS-AGE TO EXP-LTM-WGHT-AGE-AVG         ECS021
05562             MOVE EXC-CMO-WGHT-AGE TO EXP-CMO-WGHT-AGE             ECS021
05563             MOVE EXC-WGHT-AGE-PCT TO EXP-WGHT-AGE-DIFF            ECS021
05564             MOVE EXP-LTM-WGHT-AGE-MSG TO HOLD-EXCEPTION-PRT       ECS021
05565             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05566                                                                   ECS021
05567      IF AH-TOTAL                                                  ECS021
05568         IF (WS-AH-AGE-AVG-WEIGHTED GREATER THAN ZEROS)  AND       ECS021
05569            (EXC-WGHT-AGE-PCT > WS-AH-AGE-AVG-WEIGHTED)            ECS021
05570             MOVE EXC-WGHT-ISS-AGE TO EXP-LTM-WGHT-AGE-AVG         ECS021
05571             MOVE EXC-CMO-WGHT-AGE TO EXP-CMO-WGHT-AGE             ECS021
05572             MOVE EXC-WGHT-AGE-PCT TO EXP-WGHT-AGE-DIFF            ECS021
05573             MOVE EXP-LTM-WGHT-AGE-MSG TO HOLD-EXCEPTION-PRT       ECS021
05574             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05575                                                                   ECS021
05576      IF LIFE-TOTAL                                                ECS021
05577         IF (WS-LF-TERM-AVG-WEIGHTED GREATER THAN ZEROS) AND       ECS021
05578            (EXC-WGHT-TRM-PCT > WS-LF-TERM-AVG-WEIGHTED)           ECS021
05579             MOVE EXC-WGHT-ORIG-TERM TO EXP-LTM-WGHT-TRM-AVG       ECS021
05580             MOVE EXC-CMO-WGHT-ORG-TRM TO EXP-CMO-WGHT-TRM         ECS021
05581             MOVE EXC-WGHT-TRM-PCT TO EXP-WGHT-TERM-DIFF           ECS021
05582             MOVE EXP-LTM-WGHT-TERM-MSG TO HOLD-EXCEPTION-PRT      ECS021
05583             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05584                                                                   ECS021
05585      IF AH-TOTAL                                                  ECS021
05586         IF (WS-AH-TERM-AVG-WEIGHTED GREATER THAN ZEROS) AND       ECS021
05587            (EXC-WGHT-TRM-PCT > WS-AH-TERM-AVG-WEIGHTED)           ECS021
05588             MOVE EXC-WGHT-ORIG-TERM TO EXP-LTM-WGHT-TRM-AVG       ECS021
05589             MOVE EXC-CMO-WGHT-ORG-TRM TO EXP-CMO-WGHT-TRM         ECS021
05590             MOVE EXC-WGHT-TRM-PCT TO EXP-WGHT-TERM-DIFF           ECS021
05591             MOVE EXP-LTM-WGHT-TERM-MSG TO HOLD-EXCEPTION-PRT      ECS021
05592             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05593                                                                   ECS021
05594      IF LIFE-TOTAL                                                ECS021
05595         IF (WS-LF-LTM-TERM-CHG GREATER THAN ZEROS)  AND           ECS021
05596            (EXC-CMO-ORG-TRM > EXC-AVRG-ORIG-TERM)  AND            ECS021
05597            (EXC-ORIG-TERM-PCT > WS-LF-LTM-TERM-CHG)               ECS021
05598             MOVE EXC-AVRG-ORIG-TERM TO EXP-LTM-ORIG-TRM-AVG       ECS021
05599             MOVE EXC-CMO-ORG-TRM TO EXP-CMO-ORIG-TRM-CNT          ECS021
05600             MOVE EXC-ORIG-TERM-PCT TO EXP-AVG-TERM-DIFF           ECS021
05601             MOVE EXP-LTM-TERM-MSG TO HOLD-EXCEPTION-PRT           ECS021
05602             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05603                                                                   ECS021
05604      IF AH-TOTAL                                                  ECS021
05605         IF (WS-AH-LTM-TERM-CHG GREATER THAN ZEROS)  AND           ECS021
05606            (EXC-ORIG-TERM-PCT > WS-AH-LTM-TERM-CHG)               ECS021
05607             MOVE EXC-AVRG-ORIG-TERM TO EXP-LTM-ORIG-TRM-AVG       ECS021
05608             MOVE EXC-CMO-ORG-TRM TO EXP-CMO-ORIG-TRM-CNT          ECS021
05609             MOVE EXC-ORIG-TERM-PCT TO EXP-AVG-TERM-DIFF           ECS021
05610             MOVE EXP-LTM-TERM-MSG TO HOLD-EXCEPTION-PRT           ECS021
05611             PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.      ECS021
05612                                                                   ECS021
05613      IF LIFE-TOTAL                                                ECS021
05614          IF (WS-LF-LTM-INFORCE-DECR GREATER THAN ZEROS)  AND      ECS021
05615             (EXC-INFRC-PCT > WS-LF-LTM-INFORCE-DECR)              ECS021
05616              MOVE EXC-INFRC-CVRG TO EXP-LTM-INFORCE-AVG           ECS021
05617              MOVE EXC-CMO-INFRC-CVRG TO EXP-CMO-INFORCE-CNT       ECS021
05618              MOVE EXC-INFRC-PCT TO EXP-AVG-INFORCE-DIFF           ECS021
05619              MOVE EXP-LTM-INFORCE-CNT-MSG TO HOLD-EXCEPTION-PRT   ECS021
05620              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05621                                                                   ECS021
05622      IF AH-TOTAL                                                  ECS021
05623          IF (WS-AH-LTM-INFORCE-DECR GREATER THAN ZEROS)  AND      ECS021
05624             (EXC-INFRC-PCT > WS-AH-LTM-INFORCE-DECR)              ECS021
05625              MOVE EXC-INFRC-CVRG TO EXP-LTM-INFORCE-AVG           ECS021
05626              MOVE EXC-CMO-INFRC-CVRG TO EXP-CMO-INFORCE-CNT       ECS021
05627              MOVE EXC-INFRC-PCT TO EXP-AVG-INFORCE-DIFF           ECS021
05628              MOVE EXP-LTM-INFORCE-CNT-MSG TO HOLD-EXCEPTION-PRT   ECS021
05629              PERFORM 9060-PRINT-EXCEPTION-RTN THRU 9069-EXIT.     ECS021
05630                                                                   ECS021
05631  7000-EXIT.                                                       ECS021
05632      EXIT.                                                        ECS021
05633                                                                   ECS021
05634      EJECT                                                        ECS021
05635  7005-CALC-COVG-EXCEPTIONS.                                       ECS021
05636                                                                   ECS021
05637      COMPUTE PREV-NDX = DTE-NDX - 1.                              ECS021
05638                                                                   ECS021
05639  7005-CALC-COVG-PERIOD.                                           ECS021
05640                                                                   ECS021
05641      COMPUTE TOT-ISS-PREM =                                       ECS021
05642              CMNT-ISS-PREM (BEN-NDX DTE-NDX) -                    ECS021
05643              CMNT-ISS-PREM (BEN-NDX PREV-NDX).                    ECS021
05644                                                                   ECS021
05645      COMPUTE TOT-CNC-PREM =                                       ECS021
05646              CMNT-CNC-PREM (BEN-NDX DTE-NDX) -                    ECS021
05647              CMNT-CNC-PREM (BEN-NDX PREV-NDX).                    ECS021
05648                                                                   ECS021
05649      COMPUTE TOT-WRITTEN-PREM =                                   ECS021
05650              TOT-ISS-PREM - TOT-CNC-PREM.                         ECS021
05651                                                                   ECS021
05652      COMPUTE LOSS-RESERVE =                                       ECS021
05653              CMNT-LOSS-RESV (BEN-NDX DTE-NDX) -                   ECS021
05654              CMNT-LOSS-RESV (BEN-NDX PREV-NDX).                   ECS021
05655                                                                   ECS021
05656      MOVE ZEROS                  TO LOSS-RATIO.                   ECS021
05657                                                                   ECS021
05658      COMPUTE EXC-EARND-PREM =                                     ECS021
05659              CMNT-EARND-PREM (BEN-NDX DTE-NDX) -                  ECS021
05660              CMNT-EARND-PREM (BEN-NDX PREV-NDX).                  ECS021
05661                                                                   ECS021
05662      IF LIFE-TOTAL                                                ECS021
05663          COMPUTE TOT-CLAIM-AMT =                                  ECS021
05664                  CMNT-LF-CLM-AMT (BEN-NDX DTE-NDX) -              ECS021
05665                  CMNT-LF-CLM-AMT (BEN-NDX PREV-NDX).              ECS021
05666                                                                   ECS021
05667      IF AH-TOTAL                                                  ECS021
05668          COMPUTE TOT-CLAIM-AMT =                                  ECS021
05669                  CMNT-AH-CLM-AMT (BEN-NDX DTE-NDX) -              ECS021
05670                  CMNT-AH-CLM-AMT (BEN-NDX PREV-NDX).              ECS021
05671                                                                   ECS021
05672      IF BOTH-TOTAL                                                ECS021
05673          COMPUTE TOT-CLAIM-AMT =                                  ECS021
05674                  CMNT-LF-CLM-AMT (BEN-NDX DTE-NDX) -              ECS021
05675                  CMNT-LF-CLM-AMT (BEN-NDX PREV-NDX) +             ECS021
05676                  CMNT-AH-CLM-AMT (BEN-NDX DTE-NDX) -              ECS021
05677                  CMNT-AH-CLM-AMT (BEN-NDX PREV-NDX).              ECS021
05678                                                                   ECS021
05679      IF EXC-EARND-PREM NOT EQUAL ZEROS                            ECS021
05680          COMPUTE LOSS-RATIO ROUNDED =                             ECS021
05681             (TOT-CLAIM-AMT + LOSS-RESERVE ) / EXC-EARND-PREM.     ECS021
05682                                                                   ECS021
05683      MOVE ZEROS                  TO COMPEN-PERCENT                ECS021
05684                                     SV-COMP-PCT.                  ECS021
05685                                                                   ECS021
05686      COMPUTE TOT-COMPEN =                                         ECS021
05687              CMNT-NET-COMPEN (BEN-NDX DTE-NDX) -                  ECS021
05688              CMNT-NET-COMPEN (BEN-NDX PREV-NDX).                  ECS021
05689                                                                   ECS021
05690      IF DTE-PGM-OPT = '1'                                         ECS021
05691          IF EXC-EARND-PREM = ZEROS                                ECS021
05692              GO TO 7005-BYPASS-COMPEN-PERCENT                     ECS021
05693          ELSE                                                     ECS021
05694              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
05695                      TOT-COMPEN / EXC-EARND-PREM                  ECS021
05696      ELSE                                                         ECS021
05697          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
05698              GO TO 7005-BYPASS-COMPEN-PERCENT                     ECS021
05699          ELSE                                                     ECS021
05700              COMPUTE COMPEN-PERCENT ROUNDED =                     ECS021
05701                      TOT-COMPEN / TOT-WRITTEN-PREM.               ECS021
05702                                                                   ECS021
05703  7005-BYPASS-COMPEN-PERCENT.                                      ECS021
05704      IF CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX) = ZEROS               ECS021
05705          MOVE ZEROS                 TO EXP-EARN-RATIO             ECS021
05706          GO TO 7005-BYPASS-EXPENSE-PCT.                           ECS021
05707                                                                   ECS021
05708      IF DTE-PGM-OPT = 2                                           ECS021
05709          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
05710              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
05711              GO TO 7005-BYPASS-EXPENSE-PCT.                       ECS021
05712                                                                   ECS021
05713      IF DTE-PGM-OPT = 1                                           ECS021
05714          IF EXC-EARND-PREM = ZEROS                                ECS021
05715              MOVE ZEROS             TO EXP-EARN-RATIO             ECS021
05716              GO TO 7005-BYPASS-EXPENSE-PCT.                       ECS021
05717                                                                   ECS021
05718      COMPUTE EXPENSE-PERCENT =                                    ECS021
05719              CMNT-EXP-PCT      (BEN-NDX DTE-NDX) /                ECS021
05720              CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX).                 ECS021
05721                                                                   ECS021
05722      IF DTE-PGM-OPT = 2                                           ECS021
05723          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
05724                  TOT-WRITTEN-PREM * EXPENSE-PERCENT               ECS021
05725          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
05726                  EXPENSE-AMT / TOT-WRITTEN-PREM                   ECS021
05727      ELSE                                                         ECS021
05728          COMPUTE EXPENSE-AMT ROUNDED =                            ECS021
05729                  EXC-EARND-PREM * EXPENSE-PERCENT                 ECS021
05730          COMPUTE EXP-EARN-RATIO ROUNDED =                         ECS021
05731                  EXPENSE-AMT / EXC-EARND-PREM.                    ECS021
05732                                                                   ECS021
05733  7005-BYPASS-EXPENSE-PCT.                                         ECS021
05734      IF DTE-PGM-OPT = 2                                           ECS021
05735          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
05736                  TOT-WRITTEN-PREM -                               ECS021
05737                  (TOT-CLAIM-AMT + LOSS-RESERVE + TOT-COMPEN +     ECS021
05738                   EXPENSE-AMT)                                    ECS021
05739      ELSE                                                         ECS021
05740          COMPUTE PERIOD-PROFIT ROUNDED =                          ECS021
05741              EXC-EARND-PREM -                                     ECS021
05742                  (TOT-CLAIM-AMT + LOSS-RESERVE + TOT-COMPEN +     ECS021
05743                   EXPENSE-AMT).                                   ECS021
05744                                                                   ECS021
05745      IF DTE-PGM-OPT = 2                                           ECS021
05746          IF TOT-WRITTEN-PREM = ZEROS                              ECS021
05747              MOVE ZEROS             TO PROFIT-PERCENT             ECS021
05748              GO TO 7005-EXIT.                                     ECS021
05749                                                                   ECS021
05750      IF DTE-PGM-OPT = 1                                           ECS021
05751          IF EXC-EARND-PREM = ZEROS                                ECS021
05752              MOVE ZEROS             TO PROFIT-PERCENT             ECS021
05753              GO TO 7005-EXIT.                                     ECS021
05754                                                                   ECS021
05755      IF DTE-PGM-OPT = 2                                           ECS021
05756          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
05757                  PERIOD-PROFIT / TOT-WRITTEN-PREM                 ECS021
05758      ELSE                                                         ECS021
05759          COMPUTE PROFIT-PERCENT ROUNDED =                         ECS021
05760                  PERIOD-PROFIT / EXC-EARND-PREM.                  ECS021
05761                                                                   ECS021
05762  7005-EXIT.                                                       ECS021
05763      EXIT.                                                        ECS021
05764                                                                   ECS021
05765      EJECT                                                        ECS021
05766  7050-CALC-AVG-EXCEPTIONS.                                        ECS021
05767                                                                   ECS021
05768      COMPUTE PREV-NDX = DTE-NDX - 1.                              ECS021
05769                                                                   ECS021
05770      IF CMNT-ISS-CNT (BEN-NDX DTE-NDX) NOT = ZEROS                ECS021
05771          COMPUTE AVG-ORG-TRM ROUNDED =                            ECS021
05772                  CMNT-AVG-ORG-TRM  (BEN-NDX DTE-NDX) /            ECS021
05773                  CMNT-ISS-CNT      (BEN-NDX DTE-NDX)              ECS021
05774          COMPUTE WGHT-ORG-TRM ROUNDED =                           ECS021
05775                  CMNT-WGHT-ORG-TRM (BEN-NDX DTE-NDX) /            ECS021
05776                  CMNT-ISS-CNT      (BEN-NDX DTE-NDX)              ECS021
05777          COMPUTE AVG-AGE ROUNDED =                                ECS021
05778                  CMNT-AVG-AGE      (BEN-NDX DTE-NDX) /            ECS021
05779                  CMNT-ISS-CNT      (BEN-NDX DTE-NDX)              ECS021
05780          COMPUTE WGHT-AGE ROUNDED =                               ECS021
05781                  CMNT-WGHT-AGE     (BEN-NDX DTE-NDX) /            ECS021
05782                  CMNT-ISS-CNT      (BEN-NDX DTE-NDX)              ECS021
05783      ELSE                                                         ECS021
05784      IF CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX) NOT = ZEROS           ECS021
05785          COMPUTE AVG-ORG-TRM ROUNDED =                            ECS021
05786                  CMNT-AVG-ORG-TRM  (BEN-NDX DTE-NDX) /            ECS021
05787                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05788          COMPUTE WGHT-ORG-TRM ROUNDED =                           ECS021
05789                  CMNT-WGHT-ORG-TRM (BEN-NDX DTE-NDX) /            ECS021
05790                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05791          COMPUTE AVG-AGE ROUNDED =                                ECS021
05792                  CMNT-AVG-AGE      (BEN-NDX DTE-NDX) /            ECS021
05793                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05794          COMPUTE WGHT-AGE ROUNDED =                               ECS021
05795                  CMNT-WGHT-AGE     (BEN-NDX DTE-NDX) /            ECS021
05796                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05797      ELSE                                                         ECS021
05798          MOVE ZEROS              TO AVG-ORG-TRM                   ECS021
05799                                     WGHT-ORG-TRM                  ECS021
05800                                     AVG-AGE                       ECS021
05801                                     WGHT-AGE.                     ECS021
05802                                                                   ECS021
05803      MOVE CMNT-INFRC-CNT (BEN-NDX DTE-NDX)                        ECS021
05804                                  TO EXC-INFRC-CVRG.               ECS021
05805                                                                   ECS021
05806  7050-ACCUM-ALL-PERIODS.                                          ECS021
05807      IF CMNT-INFRC-CNT (BEN-NDX DTE-NDX) GREATER ZERO             ECS021
05808          ADD CMNT-INFRC-CNT (BEN-NDX DTE-NDX)                     ECS021
05809                                  TO EXC-ACC-AVG-INFRC             ECS021
05810          ADD 1                   TO EXC-ACC-AVG-INFRC-CNT.        ECS021
05811                                                                   ECS021
05812      IF AVG-ORG-TRM GREATER ZERO                                  ECS021
05813          ADD AVG-ORG-TRM         TO EXC-ACC-AVG-ORG-TRM           ECS021
05814          ADD 1                   TO EXC-ACC-AVG-ORG-TRM-CNT.      ECS021
05815                                                                   ECS021
05816      IF WGHT-ORG-TRM GREATER ZERO                                 ECS021
05817          ADD WGHT-ORG-TRM        TO EXC-ACC-AVG-WGHT-TRM          ECS021
05818          ADD 1                   TO EXC-ACC-AVG-WGHT-TRM-CNT.     ECS021
05819                                                                   ECS021
05820      IF AVG-AGE GREATER ZERO                                      ECS021
05821          ADD AVG-AGE             TO EXC-ACC-AVG-ISS-AGE           ECS021
05822          ADD 1                   TO EXC-ACC-AVG-ISS-CNT.          ECS021
05823                                                                   ECS021
05824      IF WGHT-AGE GREATER ZERO                                     ECS021
05825          ADD WGHT-AGE            TO EXC-ACC-AVG-WGHT-AGE          ECS021
05826          ADD 1                   TO EXC-ACC-AVG-WGHT-CNT.         ECS021
05827                                                                   ECS021
05828      IF DTE-NDX LESS +15                                          ECS021
05829          GO TO 7050-EXIT.                                         ECS021
05830                                                                   ECS021
05831      IF CMNT-ISS-CNT (BEN-NDX DTE-NDX) NOT = ZEROS                ECS021
05832          COMPUTE EXC-CMO-ORG-TRM ROUNDED =                        ECS021
05833                  CMNT-AVG-ORG-TRM  (BEN-NDX DTE-NDX) /            ECS021
05834                  CMNT-ISS-CNT (BEN-NDX DTE-NDX)                   ECS021
05835          COMPUTE EXC-CMO-WGHT-ORG-TRM ROUNDED =                   ECS021
05836                  CMNT-WGHT-ORG-TRM (BEN-NDX DTE-NDX) /            ECS021
05837                  CMNT-ISS-CNT (BEN-NDX DTE-NDX)                   ECS021
05838          COMPUTE EXC-CMO-AVG-AGE ROUNDED =                        ECS021
05839                  CMNT-AVG-AGE      (BEN-NDX DTE-NDX) /            ECS021
05840                  CMNT-ISS-CNT (BEN-NDX DTE-NDX)                   ECS021
05841          COMPUTE EXC-CMO-WGHT-AGE ROUNDED =                       ECS021
05842                  CMNT-WGHT-AGE     (BEN-NDX DTE-NDX) /            ECS021
05843                  CMNT-ISS-CNT (BEN-NDX DTE-NDX)                   ECS021
05844      ELSE                                                         ECS021
05845      IF CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX) NOT = ZEROS           ECS021
05846          COMPUTE EXC-CMO-ORG-TRM ROUNDED =                        ECS021
05847                  CMNT-AVG-ORG-TRM  (BEN-NDX DTE-NDX) /            ECS021
05848                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05849          COMPUTE EXC-CMO-WGHT-ORG-TRM ROUNDED =                   ECS021
05850                  CMNT-WGHT-ORG-TRM (BEN-NDX DTE-NDX) /            ECS021
05851                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05852          COMPUTE EXC-CMO-AVG-AGE ROUNDED =                        ECS021
05853                  CMNT-AVG-AGE      (BEN-NDX DTE-NDX) /            ECS021
05854                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05855          COMPUTE EXC-CMO-WGHT-AGE ROUNDED =                       ECS021
05856                  CMNT-WGHT-AGE     (BEN-NDX DTE-NDX) /            ECS021
05857                  CMNT-ADDED-TO-CNT (BEN-NDX DTE-NDX)              ECS021
05858      ELSE                                                         ECS021
05859          MOVE ZEROS              TO EXC-CMO-ORG-TRM               ECS021
05860                                     EXC-CMO-WGHT-ORG-TRM          ECS021
05861                                     EXC-CMO-AVG-AGE               ECS021
05862                                     EXC-CMO-WGHT-AGE.             ECS021
05863                                                                   ECS021
05864      MOVE CMNT-INFRC-CNT (BEN-NDX DTE-NDX)                        ECS021
05865                                  TO EXC-CMO-INFRC-CVRG.           ECS021
05866                                                                   ECS021
05867      MOVE ZEROS TO  EXC-INFRC-CVRG                                ECS021
05868                     EXC-AVRG-ORIG-TERM                            ECS021
05869                     EXC-WGHT-ORIG-TERM                            ECS021
05870                     EXC-AVRG-ISS-AGE                              ECS021
05871                     EXC-WGHT-ISS-AGE.                             ECS021
05872                                                                   ECS021
05873      IF EXC-ACC-AVG-INFRC-CNT GREATER ZERO                        ECS021
05874          COMPUTE EXC-INFRC-CVRG =                                 ECS021
05875                  EXC-ACC-AVG-INFRC / EXC-ACC-AVG-INFRC-CNT.       ECS021
05876                                                                   ECS021
05877      IF EXC-ACC-AVG-ORG-TRM-CNT GREATER ZERO                      ECS021
05878          COMPUTE EXC-AVRG-ORIG-TERM ROUNDED =                     ECS021
05879                  EXC-ACC-AVG-ORG-TRM / EXC-ACC-AVG-ORG-TRM-CNT.   ECS021
05880                                                                   ECS021
05881      IF EXC-ACC-AVG-WGHT-TRM-CNT GREATER ZERO                     ECS021
05882          COMPUTE EXC-WGHT-ORIG-TERM ROUNDED =                     ECS021
05883                  EXC-ACC-AVG-WGHT-TRM / EXC-ACC-AVG-WGHT-TRM-CNT. ECS021
05884                                                                   ECS021
05885      IF EXC-ACC-AVG-ISS-CNT GREATER ZERO                          ECS021
05886          COMPUTE EXC-AVRG-ISS-AGE ROUNDED =                       ECS021
05887                  EXC-ACC-AVG-ISS-AGE / EXC-ACC-AVG-ISS-CNT.       ECS021
05888                                                                   ECS021
05889      IF EXC-ACC-AVG-WGHT-CNT GREATER ZERO                         ECS021
05890          COMPUTE EXC-WGHT-ISS-AGE ROUNDED =                       ECS021
05891                  EXC-ACC-AVG-WGHT-AGE / EXC-ACC-AVG-WGHT-CNT.     ECS021
05892                                                                   ECS021
05893  7050-EXIT.                                                       ECS021
05894      EXIT.                                                        ECS021
05895                                                                   ECS021
05896      EJECT                                                        ECS021
05897  7100-CALL-SUB-MODULE-ONE.                                        ECS021
05898                                                                   ECS021
102908*05899      CALL   'ECS021T1'   USING       REQUEST-TABLE                ECS021
102908     CALL   'ECS021T1RET'   USING    REQUEST-TABLE
05900                                      SW-RECORD                    ECS021
05901                                      COMMON-TABLE                 ECS021
092602*                                    COMMON-EXTENSION-1           ECS021
092602*                                    COMMON-EXTENSION-2           ECS021
092608*                                    COMMON-EXTENSION-3           ECS021
05905                                      COMMON-TOTAL-TABLE           ECS021
05906                                      CLASIC-SYSTEM-CODES          ECS021
05907                                      CLAS-INS-TYPES               ECS021
05908                                      CLAS-INDEX-TBL.              ECS021
05909                                                                   ECS021
05910  7100-EXIT.                                                       ECS021
05911      EXIT.                                                        ECS021
05912                                                                   ECS021
05913  7200-CALL-SUB-MODULE-TWO.                                        ECS021
05914                                                                   ECS021
05915      CALL   'ECS021T2'   USING       REQUEST-TABLE                ECS021
05916                                      SW-RECORD                    ECS021
05917                                      COMMON-TABLE                 ECS021
092602*                                    COMMON-EXTENSION-1           ECS021
092602*                                    COMMON-EXTENSION-2           ECS021
092602*                                    COMMON-EXTENSION-3           ECS021
05921                                      COMMON-TOTAL-TABLE           ECS021
05922                                      CLASIC-SYSTEM-CODES          ECS021
05923                                      CLAS-INS-TYPES               ECS021
05924                                      CLAS-INDEX-TBL.              ECS021
05925                                                                   ECS021
05926  7200-EXIT.                                                       ECS021
05927      EXIT.                                                        ECS021
05928                                                                   ECS021
05929  7300-CALL-SUB-MODULE-THREE.                                      ECS021
05930                                                                   ECS021
05931      CALL   'ECS021T3'   USING       REQUEST-TABLE                ECS021
05932                                      SW-RECORD                    ECS021
05933                                      COMMON-TABLE                 ECS021
092602*                                    COMMON-EXTENSION-1           ECS021
092602*                                    COMMON-EXTENSION-2           ECS021
092602*                                    COMMON-EXTENSION-3           ECS021
05937                                      COMMON-TOTAL-TABLE           ECS021
05938                                      CLASIC-SYSTEM-CODES          ECS021
05939                                      CLAS-INS-TYPES               ECS021
05940                                      CLAS-INDEX-TBL.              ECS021
05941                                                                   ECS021
05942  7300-EXIT.                                                       ECS021
05943      EXIT.                                                        ECS021
05944                                                                   ECS021
05945  7400-CALL-SUB-MODULE-FOUR.                                       ECS021
05946                                                                   ECS021
05947      CALL   'ECS021T4'   USING       REQUEST-TABLE                ECS021
05948                                      SW-RECORD                    ECS021
05949                                      COMMON-TABLE                 ECS021
092602*                                    COMMON-EXTENSION-1           ECS021
092602*                                    COMMON-EXTENSION-2           ECS021
092602*                                    COMMON-EXTENSION-3           ECS021
05953                                      COMMON-TOTAL-TABLE           ECS021
05954                                      CLASIC-SYSTEM-CODES          ECS021
05955                                      CLAS-INS-TYPES               ECS021
05956                                      CLAS-INDEX-TBL.              ECS021
05957                                                                   ECS021
05958  7400-EXIT.                                                       ECS021
05959      EXIT.                                                        ECS021
05960                                                                   ECS021
05961  7500-CALL-SUB-MODULE-FIVE.                                       ECS021
05962                                                                   ECS021
05963      CALL   'ECS021T5'   USING       REQUEST-TABLE                ECS021
05964                                      SW-RECORD                    ECS021
05965                                      COMMON-TABLE                 ECS021
092602*                                    COMMON-EXTENSION-1           ECS021
092602*                                    COMMON-EXTENSION-2           ECS021
092602*                                    COMMON-EXTENSION-3           ECS021
05969                                      COMMON-TOTAL-TABLE           ECS021
05970                                      CLASIC-SYSTEM-CODES          ECS021
05971                                      CLAS-INS-TYPES               ECS021
05972                                      CLAS-INDEX-TBL.              ECS021
05973                                                                   ECS021
05974  7500-EXIT.                                                       ECS021
05975      EXIT.                                                        ECS021
05976                                                                   ECS021
05977  7600-CALL-SUB-MODULE-SIX.                                        ECS021
05978                                                                   ECS021
05979      CALL   'ECS021T6'   USING       REQUEST-TABLE                ECS021
05980                                      SW-RECORD                    ECS021
05981                                      COMMON-TABLE                 ECS021
092602*                                    COMMON-EXTENSION-1           ECS021
092602*                                    COMMON-EXTENSION-2           ECS021
092602*                                    COMMON-EXTENSION-3           ECS021
05985                                      COMMON-TOTAL-TABLE           ECS021
05986                                      CLASIC-SYSTEM-CODES          ECS021
05987                                      CLAS-INS-TYPES               ECS021
05988                                      CLAS-INDEX-TBL.              ECS021
05989                                                                   ECS021
05990  7600-EXIT.                                                       ECS021
05991      EXIT.                                                        ECS021
05992                                                                   ECS021
102908
102908 7900-WRITE-DATA-OUT.
102908     MOVE WS-SAVE-CARRIER TO DO-CARRIER.
102908     MOVE WS-SAVE-STATE-ABBREVIATION TO DO-STATE.
102908     MOVE SAVE-BREAK-1    TO DO-ACCOUNT-NUM.
102908     MOVE WS-SAVE-ACCT-NAME TO DO-ACCOUNT-NAME.
102908     COMPUTE PREV-NDX = 15 - BRK-MM (15).
102908     MOVE 'GROSS WRITTEN PREM' TO DO-DESCR (1).
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (1 15) - 
102908                          CMNT-ISS-PREM (1 14).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (1).
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (1 15) -
102908                          CMNT-ISS-PREM (1 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (1).
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (1 15) -
102908                          CMNT-ISS-PREM (1 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (1). 
102908     MOVE CMNT-ISS-PREM (1 15) TO DO-ITDLF (1).
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (2 15) - 
102908                          CMNT-ISS-PREM (2 14).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (1).
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (2 15) -
102908                          CMNT-ISS-PREM (2 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (1).
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (2 15) -
102908                          CMNT-ISS-PREM (2 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (1). 
102908     MOVE CMNT-ISS-PREM (2 15) TO DO-ITDAH (1).
102908
102908     MOVE 'CANCEL PREM' TO DO-DESCR (2).
102908     COMPUTE WRK-AMOUNT = CMNT-CNC-PREM (1 15) - 
102908                          CMNT-CNC-PREM (1 14).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (2).
102908     COMPUTE WRK-AMOUNT = CMNT-CNC-PREM (1 15) -
102908                          CMNT-CNC-PREM (1 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (2).
102908     COMPUTE WRK-AMOUNT = CMNT-CNC-PREM (1 15) -
102908                          CMNT-CNC-PREM (1 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (2). 
102908     MOVE CMNT-CNC-PREM (1 15) TO DO-ITDLF (2).
102908     COMPUTE WRK-AMOUNT = CMNT-CNC-PREM (2 15) - 
102908                          CMNT-CNC-PREM (2 14).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (2).
102908     COMPUTE WRK-AMOUNT = CMNT-CNC-PREM (2 15) -
102908                          CMNT-CNC-PREM (2 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (2).
102908     COMPUTE WRK-AMOUNT = CMNT-CNC-PREM (2 15) -
102908                          CMNT-CNC-PREM (2 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (2). 
102908     MOVE CMNT-CNC-PREM (2 15) TO DO-ITDAH (2).
102908
102908     MOVE 'NET WRITTEN PREM' TO DO-DESCR (3).
102908     COMPUTE WRK-AMOUNT = (CMNT-ISS-PREM (1 15) -
102908                          CMNT-ISS-PREM (1 14)) -
102908                          (CMNT-CNC-PREM (1 15) - 
102908                          CMNT-CNC-PREM (1 14)).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (3).
102908     COMPUTE WRK-AMOUNT = (CMNT-ISS-PREM (1 15) -
102908                          CMNT-ISS-PREM (1 PREV-NDX)) -
102908                          (CMNT-CNC-PREM (1 15) -
102908                          CMNT-CNC-PREM (1 PREV-NDX)).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (3).
102908     COMPUTE WRK-AMOUNT = (CMNT-ISS-PREM (1 15) -
102908                          CMNT-ISS-PREM (1 3)) -
102908                          (CMNT-CNC-PREM (1 15) -
102908                          CMNT-CNC-PREM (1 3)). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (3). 
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (1 15) -
102908                          CMNT-CNC-PREM (1 15)
102908     MOVE WRK-AMOUNT      TO DO-ITDLF (3).
102908     COMPUTE WRK-AMOUNT = (CMNT-ISS-PREM (2 15) -
102908                          CMNT-ISS-PREM (2 14)) -
102908                          (CMNT-CNC-PREM (2 15) - 
102908                          CMNT-CNC-PREM (2 14)).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (3).
102908     COMPUTE WRK-AMOUNT = (CMNT-ISS-PREM (2 15) -
102908                          CMNT-ISS-PREM (2 PREV-NDX)) -
102908                          (CMNT-CNC-PREM (2 15) -
102908                          CMNT-CNC-PREM (2 PREV-NDX)).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (3).
102908     COMPUTE WRK-AMOUNT = (CMNT-ISS-PREM (2 15) -
102908                          CMNT-ISS-PREM (2 3)) -
102908                          (CMNT-CNC-PREM (2 15) -
102908                          CMNT-CNC-PREM (2 3)). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (3). 
102908     COMPUTE WRK-AMOUNT = CMNT-ISS-PREM (2 15) -
102908                          CMNT-CNC-PREM (2 15)
102908     MOVE WRK-AMOUNT      TO DO-ITDAH (3).
102908
102908     MOVE 'EARNED PREM' TO DO-DESCR (4).
102908     COMPUTE WRK-AMOUNT = CMNT-EARND-PREM (1 15) - 
102908                          CMNT-EARND-PREM (1 14).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (4).
102908     COMPUTE WRK-AMOUNT = CMNT-EARND-PREM (1 15) -
102908                          CMNT-EARND-PREM (1 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (4).
102908     COMPUTE WRK-AMOUNT = CMNT-EARND-PREM (1 15) -
102908                          CMNT-EARND-PREM (1 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (4). 
102908     MOVE CMNT-EARND-PREM (1 15) TO DO-ITDLF (4).
102908     COMPUTE WRK-AMOUNT = CMNT-EARND-PREM (2 15) - 
102908                          CMNT-EARND-PREM (2 14).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (4).
102908     COMPUTE WRK-AMOUNT = CMNT-EARND-PREM (2 15) -
102908                          CMNT-EARND-PREM (2 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (4).
102908     COMPUTE WRK-AMOUNT = CMNT-EARND-PREM (2 15) -
102908                          CMNT-EARND-PREM (2 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (4). 
102908     MOVE CMNT-EARND-PREM (2 15) TO DO-ITDAH (4).
102908
102908     MOVE 'ACCOUNT COMM' TO DO-DESCR (5).
102908     COMPUTE WRK-AMOUNT = CMNT-ACCT-COMM (1 15) - 
102908                          CMNT-ACCT-COMM (1 14).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (5).
102908     COMPUTE WRK-AMOUNT = CMNT-ACCT-COMM (1 15) -
102908                          CMNT-ACCT-COMM (1 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (5).
102908     COMPUTE WRK-AMOUNT = CMNT-ACCT-COMM (1 15) -
102908                          CMNT-ACCT-COMM (1 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (5). 
102908     MOVE CMNT-ACCT-COMM (1 15) TO DO-ITDLF (5).
102908     COMPUTE WRK-AMOUNT = CMNT-ACCT-COMM (2 15) - 
102908                          CMNT-ACCT-COMM (2 14).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (5).
102908     COMPUTE WRK-AMOUNT = CMNT-ACCT-COMM (2 15) -
102908                          CMNT-ACCT-COMM (2 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (5).
102908     COMPUTE WRK-AMOUNT = CMNT-ACCT-COMM (2 15) -
102908                          CMNT-ACCT-COMM (2 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (5). 
102908     MOVE CMNT-ACCT-COMM (2 15) TO DO-ITDAH (5).
102908
102908     MOVE 'OVERWRITE COMM' TO DO-DESCR (6).
102908     COMPUTE WRK-AMOUNT = CMNT-OW-COMM (1 15) - 
102908                          CMNT-OW-COMM (1 14).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (6).
102908     COMPUTE WRK-AMOUNT = CMNT-OW-COMM (1 15) -
102908                          CMNT-OW-COMM (1 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (6).
102908     COMPUTE WRK-AMOUNT = CMNT-OW-COMM (1 15) -
102908                          CMNT-OW-COMM (1 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (6). 
102908     MOVE CMNT-OW-COMM (1 15) TO DO-ITDLF (6).
102908     COMPUTE WRK-AMOUNT = CMNT-OW-COMM (2 15) - 
102908                          CMNT-OW-COMM (2 14).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (6).
102908     COMPUTE WRK-AMOUNT = CMNT-OW-COMM (2 15) -
102908                          CMNT-OW-COMM (2 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (6).
102908     COMPUTE WRK-AMOUNT = CMNT-OW-COMM (2 15) -
102908                          CMNT-OW-COMM (2 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (6). 
102908     MOVE CMNT-OW-COMM (2 15) TO DO-ITDAH (6).
102908
102908     MOVE 'PAID CLAIMS' TO DO-DESCR (7).
102908     COMPUTE WRK-AMOUNT = CMNT-LF-CLM-AMT (1 15) - 
102908                          CMNT-LF-CLM-AMT (1 14).
102908     MOVE WRK-AMOUNT      TO DO-CURLF (7).
102908     COMPUTE WRK-AMOUNT = CMNT-LF-CLM-AMT (1 15) -
102908                          CMNT-LF-CLM-AMT (1 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDLF (7).
102908     COMPUTE WRK-AMOUNT = CMNT-LF-CLM-AMT (1 15) -
102908                          CMNT-LF-CLM-AMT (1 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12LF (7). 
102908     MOVE CMNT-LF-CLM-AMT (1 15) TO DO-ITDLF (7).
102908     COMPUTE WRK-AMOUNT = CMNT-AH-CLM-AMT (2 15) - 
102908                          CMNT-AH-CLM-AMT (2 14).
102908     MOVE WRK-AMOUNT      TO DO-CURAH (7).
102908     COMPUTE WRK-AMOUNT = CMNT-AH-CLM-AMT (2 15) -
102908                          CMNT-AH-CLM-AMT (2 PREV-NDX).
102908     MOVE WRK-AMOUNT      TO DO-YTDAH (7).
102908     COMPUTE WRK-AMOUNT = CMNT-AH-CLM-AMT (2 15) -
102908                          CMNT-AH-CLM-AMT (2 3). 
102908     MOVE WRK-AMOUNT      TO DO-L12AH (7). 
102908     MOVE CMNT-AH-CLM-AMT (2 15) TO DO-ITDAH (7).
102908
102908     WRITE DATA-OUT-REC FROM DATA-OUT-RECORD.
102908
102908 7900-EXIT.
102908     EXIT.
102908
05993      EJECT                                                        ECS021
05994  8000-PRINT-DETAIL.                                               ECS021
05995      MOVE DETAIL-LINE-1          TO PRT.                          ECS021
05996      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
05997                                                                   ECS021
05998      MOVE SPACES                 TO DETAIL-LINE-1.                ECS021
05999                                                                   ECS021
06000  8099-EXIT.                                                       ECS021
06001      EXIT.                                                        ECS021
06002                                                                   ECS021
06003  8500-PRINT-DETAIL.                                               ECS021
06004      MOVE DETAIL-LINE-2          TO PRT.                          ECS021
06005      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06006                                                                   ECS021
06007      MOVE SPACES                 TO DETAIL-LINE-2.                ECS021
06008                                                                   ECS021
06009  8599-EXIT.                                                       ECS021
06010      EXIT.                                                        ECS021
06011                                                                   ECS021
06012      EJECT                                                        ECS021
06013  9000-HEADING-ROUTINE.                                            ECS021
06014                                                                   ECS021
06015      MOVE SPACES                 TO BRK-1-DESC BRK-1-CNTL         ECS021
06016                                     BRK-2-DESC BRK-2-CNTL         ECS021
06017                                     BRK-3-DESC BRK-3-CNTL         ECS021
06018                                     BRK-4-DESC BRK-4-CNTL         ECS021
06019                                     BRK-5-DESC BRK-5-CNTL         ECS021
06020                                     BRK-6-DESC BRK-6-CNTL.        ECS021
06021                                                                   ECS021
06022      MOVE BREAK-DESC (1)         TO BRK-1-DESC.                   ECS021
06023      MOVE SAVE-BREAK-1           TO BRK-1-CNTL.                   ECS021
06024      MOVE HEADER-ONE             TO PRT.                          ECS021
06025      MOVE '1'                    TO X.                            ECS021
06026      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06027                                                                   ECS021
06028      IF HEADING-SWITCH GREATER THAN 1                                CL**8
06029          MOVE BREAK-DESC (2)     TO BRK-2-DESC                    ECS021
06030          MOVE SAVE-BREAK-2       TO BRK-2-CNTL.                   ECS021
06031                                                                   ECS021
06032      MOVE HEADER-TWO             TO PRT.                          ECS021
06033      MOVE ' '                    TO X.                            ECS021
06034      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06035                                                                   ECS021
06036      ADD +1 TO PAGE-CNT.                                          ECS021
06037      MOVE PAGE-CNT               TO HDG-3-PAGE.                   ECS021
06038                                                                   ECS021
06039      IF HEADING-SWITCH GREATER THAN 2                                CL**8
06040          MOVE BREAK-DESC (3)     TO BRK-3-DESC                    ECS021
06041          MOVE SAVE-BREAK-3       TO BRK-3-CNTL.                   ECS021
06042                                                                   ECS021
06043      MOVE HEADER-THREE           TO PRT.                          ECS021
06044      MOVE ' '                    TO X.                            ECS021
06045      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06046                                                                   ECS021
06047      IF HEADING-SWITCH GREATER THAN 3                                CL**8
06048          MOVE BREAK-DESC (4)     TO BRK-4-DESC                    ECS021
06049          MOVE SAVE-BREAK-4       TO BRK-4-CNTL.                   ECS021
06050                                                                   ECS021
06051      MOVE HEADER-FOUR            TO PRT.                          ECS021
06052      MOVE ' '                    TO X.                            ECS021
06053      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06054                                                                   ECS021
06055      IF HEADING-SWITCH GREATER THAN 4                                CL**8
06056          MOVE BREAK-DESC (5)     TO BRK-5-DESC                    ECS021
06057          MOVE SAVE-BREAK-5       TO BRK-5-CNTL.                   ECS021
06058                                                                   ECS021
06059      MOVE HEADER-FIVE            TO PRT.                          ECS021
06060      MOVE ' '                    TO X.                            ECS021
06061      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06062                                                                   ECS021
06063      IF HEADING-SWITCH GREATER THAN 5                                CL**8
06064          MOVE BREAK-DESC (6)     TO BRK-6-DESC                    ECS021
06065          MOVE SAVE-BREAK-6       TO BRK-6-CNTL.                   ECS021
06066                                                                   ECS021
06067      MOVE HEADER-SIX             TO PRT.                          ECS021
06068      MOVE ' '                    TO X.                            ECS021
06069      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06070                                                                   ECS021
06071      IF WS-ACCT-BREAK-POSITION NOT EQUAL ZEROS                    ECS021
06072          IF WS-ACCT-BREAK-POSITION NOT GREATER THAN HEADING-SWITCHECS021
06073              MOVE WS-SAVE-PROD-MO TO HD-PROD-MO                   ECS021
06074              MOVE WS-SAVE-PROD-DA TO HD-PROD-DA                   ECS021
06075              MOVE WS-SAVE-PROD-YR TO HD-PROD-YR                   ECS021
                   EVALUATE WS-SAVE-ACCT-STATUS
                     WHEN '0'
                       MOVE 'ACTIVE'   TO HD-ACCT-STATUS
                     WHEN '1'
                       MOVE 'INACTIVE' TO HD-ACCT-STATUS
                     WHEN '2'
                       MOVE 'TRANSFER' TO HD-ACCT-STATUS
                     WHEN '3'
                       MOVE 'CANCELLED'
                                       TO HD-ACCT-STATUS
                     WHEN '4'
                       MOVE 'INACTIVE' TO HD-ACCT-STATUS
                     WHEN '5'
                       MOVE 'SUSPENDED' TO HD-ACCT-STATUS
021916               WHEN '6'
021916                  MOVE 'DROPPED'  TO HD-ACCT-STATUS
021916               WHEN '7'
021916                  MOVE 'LAPSED'   TO HD-ACCT-STATUS
021916               WHEN '8'
021916                  MOVE 'RUN-OFF'  TO HD-ACCT-STATUS
021916               WHEN '9'
021916                  MOVE 'PENDING'  TO HD-ACCT-STATUS
                     WHEN OTHER
                       MOVE 'UNKNOWN'  TO HD-ACCT-STATUS
                   END-EVALUATE
06076              MOVE WS-SAVE-ACCT-NAME                               ECS021
06077                                  TO HD-ACCT-NAME                  ECS021
06078              MOVE HD-1ST-PROD-DATE                                ECS021
06079                                  TO PRT                           ECS021
06080              MOVE '0'            TO X                             ECS021
06081              PERFORM 9020-PRT-RTN THRU 9030-EXIT.                 ECS021
06082                                                                   ECS021
06083      IF WS-GA-BREAK-POSITION NOT EQUAL ZEROS                      ECS021
06084          IF WS-GA-BREAK-POSITION NOT GREATER THAN HEADING-SWITCH  ECS021
06085              MOVE WS-SAVE-GA-NAME   TO GA-NAME                    ECS021
06086              MOVE GA-HEADER         TO PRT                        ECS021
06087              MOVE '0'               TO X                          ECS021
06088              PERFORM 9020-PRT-RTN THRU 9030-EXIT.                 ECS021
06089                                                                   ECS021
06090      IF WS-STATE-BREAK-POSITION NOT EQUAL ZEROS                   ECS021
06091          IF WS-STATE-BREAK-POSITION NOT GREATER HEADING-SWITCH    ECS021
06092              MOVE WS-SAVE-STATE-NAME TO  ST-NAME                  ECS021
06093              MOVE STATE-HEADER       TO  PRT                      ECS021
06094              MOVE '0'                TO  X                        ECS021
06095              PERFORM 9020-PRT-RTN THRU 9030-EXIT.                 ECS021
06096                                                                   ECS021
06097      MOVE CLAS-I-BEN  (BEN-IDX)  TO HDG-7-BENEFIT.                ECS021
06098      MOVE CLAS-I-AB10 (BEN-IDX)  TO HDG-7-DESC.                   ECS021
06099      MOVE CLAS-I-COMMENT (BEN-IDX)                                ECS021
06100                                  TO HDG-7-COMMENT.                ECS021
06101      MOVE SPACES                 TO HDG-7-TYPE.                   ECS021
06102                                                                   ECS021
06103      IF LIFE-INDIVIDUAL                                           ECS021
06104          MOVE LIFE-OVERRIDE-L6   TO HDG-7-TYPE.                   ECS021
06105                                                                   ECS021
06106      IF AH-INDIVIDUAL                                             ECS021
06107          MOVE AH-OVERRIDE-L6     TO HDG-7-TYPE.                   ECS021
06108                                                                   ECS021
06109      IF LIFE-TOTAL                                                ECS021
06110          MOVE SPACES             TO HDG-7-BENEFIT                 ECS021
06111          MOVE LIFE-OVERRIDE-L12  TO HDG-7-DESC                    ECS021
06112          MOVE 'TOTAL'            TO HDG-7-COMMENT.                ECS021
06113                                                                   ECS021
06114      IF AH-TOTAL                                                  ECS021
06115          MOVE SPACES             TO HDG-7-BENEFIT                 ECS021
06116          MOVE AH-OVERRIDE-L12    TO HDG-7-DESC                    ECS021
06117          MOVE 'TOTAL'            TO HDG-7-COMMENT.                ECS021
06118                                                                   ECS021
06119      IF BOTH-TOTAL                                                ECS021
06120          MOVE SPACES             TO HDG-7-BENEFIT                 ECS021
06121          MOVE '  COMBINED'       TO HDG-7-DESC                    ECS021
06122          MOVE 'TOTAL'            TO HDG-7-COMMENT.                ECS021
06123                                                                   ECS021
06124      MOVE HEADER-SEVEN           TO PRT.                          ECS021
06125      MOVE '0'                    TO X.                            ECS021
06126      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06127                                                                   ECS021
06128      IF DTE-FMT-OPT = '2'                                         ECS021
06129          GO TO 9010-EXIT.                                         ECS021
06130                                                                   ECS021
06131      MOVE HEADER-EIGHT           TO PRT.                          ECS021
06132      MOVE '-'                    TO X.                            ECS021
06133      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06134                                                                   ECS021
06135      MOVE SPACES                 TO HDG-9-BEN-TYP1                ECS021
06136                                     HDG-9-BEN-TYP2                ECS021
06137                                     HDG-9-BEN-TYP3.               ECS021
06138                                                                   ECS021
06139      IF LIFE-INDIVIDUAL OR LIFE-TOTAL                             ECS021
06140          MOVE SINGLE-DESC        TO HDG-9-BEN-TYP1                ECS021
06141          MOVE JOINT-DESC         TO HDG-9-BEN-TYP2                ECS021
06142          MOVE LEVEL-DESC         TO HDG-9-BEN-TYP3.               ECS021
06143                                                                   ECS021
06144      IF AH-INDIVIDUAL OR AH-TOTAL                                 ECS021
06145          MOVE ELIM-DESC          TO HDG-9-BEN-TYP1                ECS021
06146          MOVE RETRO-DESC         TO HDG-9-BEN-TYP2.               ECS021
06147                                                                   ECS021
06148      IF AH-TOTAL                                                  ECS021
06149          MOVE PENE-DESC          TO HDG-9-BEN-TYP3.               ECS021
06150                                                                   ECS021
06151      IF BOTH-TOTAL                                                ECS021
06152          MOVE SPACES             TO HDG-9-BEN-TYP1                ECS021
06153                                     HDG-9-BEN-TYP2                ECS021
06154                                     HDG-9-BEN-TYP3.               ECS021
06155                                                                   ECS021
06156      MOVE HEADER-NINE            TO PRT.                          ECS021
06157      MOVE ' '                    TO X.                            ECS021
06158      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06159                                                                   ECS021
06160      MOVE DASH-LINE-1            TO PRT.                          ECS021
06161      MOVE ' '                    TO X.                            ECS021
06162      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06163                                                                   ECS021
06164      GO TO 9010-EXIT.                                             ECS021
06165                                                                   ECS021
06166  9000-AVERAGE-INFO-HEADINGS.                                      ECS021
06167      MOVE HEADER-TEN             TO PRT.                          ECS021
06168      MOVE '-'                    TO X.                            ECS021
06169      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06170                                                                   ECS021
06171      MOVE HEADER-ELEVEN          TO PRT.                          ECS021
06172      MOVE ' '                    TO X.                            ECS021
06173      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06174                                                                   ECS021
06175      MOVE HEADER-TWELVE          TO PRT.                          ECS021
06176      MOVE ' '                    TO X.                            ECS021
06177      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06178                                                                   ECS021
06179  9000-PRINT-DASH-LINE.                                            ECS021
06180      MOVE DASH-LINE-2            TO PRT.                          ECS021
06181      MOVE ' '                    TO X.                            ECS021
06182      PERFORM 9020-PRT-RTN THRU 9030-EXIT.                         ECS021
06183                                                                   ECS021
06184  9010-EXIT.                                                       ECS021
06185      EXIT.                                                        ECS021
06186                                                                   ECS021
06187      EJECT                                                        ECS021
06188  9020-PRT-RTN.                                                    ECS021
CIDMOD                             COPY PRTN021.
CIDMOD*                            COPY ELCPRT2.                        ECS021
06190                                                                   ECS021
06191  9030-EXIT.                                                       ECS021
06192      EXIT.                                                        ECS021
06193                                                                   ECS021
06194  9040-RETURN-SW-FILE.                                             ECS021
06195      RETURN SORT-FILE                                             ECS021
06196          AT END MOVE HIGH-VALUES TO SW-REPORT-CONTROL-KEY         ECS021
06197                 GO TO 9040-EXIT.                                  ECS021
06198                                                                   ECS021
06199      ADD +1 TO EPECS-RETURNED.                                    ECS021
06200                                                                   ECS021
06201  9040-EXIT.                                                       ECS021
06202      EXIT.                                                        ECS021
06203                                                                   ECS021
06204      EJECT                                                        ECS021
06205  9060-PRINT-EXCEPTION-RTN.                                        ECS021
06206                                                                   ECS021
06207      IF LINE-E-CNT GREATER THAN LINE-E-MAX                        ECS021
06208          NEXT SENTENCE                                            ECS021
06209      ELSE                                                         ECS021
06210          GO TO 9065-PRINT-EXCEPTION-BREAK.                        ECS021
06211                                                                   ECS021
06212      MOVE HEADER-EXCEP-ONE       TO PRT-EXCEP.                    ECS021
06213      MOVE '1'                    TO XE.                           ECS021
06214      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06215                                                                   ECS021
06216      MOVE HEADER-EXCEP-TWO       TO PRT-EXCEP.                    ECS021
06217      MOVE ' '                    TO XE.                           ECS021
06218      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06219                                                                   ECS021
06220      ADD +1 TO PAGE-E-CNT                                         ECS021
06221      MOVE PAGE-E-CNT             TO HDGE-3-PAGE.                  ECS021
06222                                                                   ECS021
06223      MOVE HEADER-EXCEP-THREE     TO PRT-EXCEP.                    ECS021
06224      MOVE ' '                    TO XE.                           ECS021
06225      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06226                                                                   ECS021
06227      MOVE +4 TO LINE-E-CNT.                                       ECS021
06228      MOVE 'Y' TO WS-1ST-EXCEPTION-PRT-SW.                         ECS021
06229                                                                   ECS021
06230  9065-PRINT-EXCEPTION-BREAK.                                      ECS021
06231                                                                   ECS021
06232      IF  FIRST-EXCEPTION-PRINT                                    ECS021
06233           MOVE 'N' TO WS-1ST-EXCEPTION-PRT-SW                     ECS021
06234      ELSE                                                         ECS021
06235           GO TO 9069-PRT-EXCEPTION-DETAIL.                        ECS021
06236                                                                   ECS021
06237      MOVE SPACES                 TO HDGE-1-DESC HDGE-1-CNTL       ECS021
06238                                     HDGE-2-DESC HDGE-2-CNTL       ECS021
06239                                     HDGE-3-DESC HDGE-3-CNTL       ECS021
06240                                     HDGE-4-DESC HDGE-4-CNTL       ECS021
06241                                     HDGE-5-DESC HDGE-5-CNTL       ECS021
06242                                     HDGE-6-DESC HDGE-6-CNTL       ECS021
06243                                     HDGE-7-DESC HDGE-7-CNTL.      ECS021
06244                                                                   ECS021
06245      MOVE BREAK-DESC (1)         TO HDGE-1-DESC.                  ECS021
06246      MOVE SAVE-BREAK-1           TO HDGE-1-CNTL.                  ECS021
06247                                                                   ECS021
06248      IF HEADING-SWITCH GREATER THAN 1                                CL**8
06249          MOVE BREAK-DESC (2)     TO HDGE-2-DESC                   ECS021
06250          MOVE SAVE-BREAK-2       TO HDGE-2-CNTL.                  ECS021
06251                                                                   ECS021
06252      IF HEADING-SWITCH GREATER THAN 2                                CL**8
06253          MOVE BREAK-DESC (3)     TO HDGE-3-DESC                   ECS021
06254          MOVE SAVE-BREAK-3       TO HDGE-3-CNTL.                  ECS021
06255                                                                   ECS021
06256      IF HEADING-SWITCH GREATER THAN 3                                CL**8
06257          MOVE BREAK-DESC (4)     TO HDGE-4-DESC                   ECS021
06258          MOVE SAVE-BREAK-4       TO HDGE-4-CNTL.                  ECS021
06259                                                                   ECS021
06260      MOVE SPACES                 TO PRT-EXCEP.                    ECS021
06261      MOVE '0'                    TO XE.                           ECS021
06262      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06263                                                                   ECS021
06264      MOVE HEADER-EXCEP-FOUR      TO PRT-EXCEP.                    ECS021
06265      MOVE '0'                    TO XE.                           ECS021
06266      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06267                                                                   ECS021
06268      IF HEADING-SWITCH GREATER THAN 4                                CL**8
06269          MOVE BREAK-DESC (5)     TO HDGE-5-DESC                   ECS021
06270          MOVE SAVE-BREAK-5       TO HDGE-5-CNTL.                  ECS021
06271                                                                   ECS021
06272      IF HEADING-SWITCH GREATER THAN 5                                CL**8
06273          MOVE BREAK-DESC (6)     TO HDGE-6-DESC                   ECS021
06274          MOVE SAVE-BREAK-6       TO HDGE-6-CNTL.                  ECS021
06275                                                                   ECS021
06276      IF WS-GA-BREAK-POSITION NOT EQUAL ZEROS                      ECS021
06277          IF WS-GA-BREAK-POSITION NOT GREATER THAN HEADING-SWITCH  ECS021
06278              MOVE GA-NAME-DESC      TO HDGE-7-DESC                ECS021
06279              MOVE WS-SAVE-GA-NAME   TO HDGE-7-CNTL.               ECS021
06280                                                                   ECS021
06281      IF (HDGE-5-DESC EQUAL SPACES)  AND                           ECS021
06282         (HDGE-5-CNTL EQUAL SPACES)  AND                           ECS021
06283         (HDGE-6-DESC EQUAL SPACES)  AND                           ECS021
06284         (HDGE-6-CNTL EQUAL SPACES) AND                            ECS021
06285         (HDGE-7-DESC EQUAL SPACES) AND                            ECS021
06286         (HDGE-7-CNTL EQUAL SPACES)                                ECS021
06287          NEXT SENTENCE                                            ECS021
06288      ELSE                                                         ECS021
06289          MOVE HEADER-EXCEP-FIVE      TO PRT-EXCEP                 ECS021
06290          MOVE ' '                    TO XE                        ECS021
06291          PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                ECS021
06292                                                                   ECS021
06293      MOVE SPACES                 TO PRT-EXCEP.                    ECS021
06294      MOVE ' '                    TO XE.                           ECS021
06295      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06296                                                                   ECS021
06297  9069-PRT-EXCEPTION-DETAIL.                                       ECS021
06298                                                                   ECS021
06299      MOVE HOLD-EXCEPTION-PRT TO PRT-EXCEP.                        ECS021
06300                                                                   ECS021
06301      PERFORM 9070-WRITE-A-LINE THRU 9079-EXIT.                    ECS021
06302                                                                   ECS021
06303      MOVE SPACES TO  HOLD-EXCEPTION-PRT.                          ECS021
06304                                                                   ECS021
06305  9069-EXIT.                                                       ECS021
06306      EXIT.                                                        ECS021
06307                                                                   ECS021
06308  9070-WRITE-A-LINE.                                               ECS021
06309                                                                   ECS021
06310      IF XE EQUAL SPACES                                           ECS021
06311          ADD +1 TO LINE-E-CNT                                     ECS021
06312      ELSE                                                         ECS021
06313         IF XE EQUAL '0'                                           ECS021
06314             ADD +2 TO LINE-E-CNT.                                 ECS021
06315                                                                   ECS021
06316      MOVE XE TO PE-CTL.                                           ECS021
06317                                                                   ECS021
06318      IF PE-CTL IS EQUAL TO ' '                                    ECS021
06319        WRITE PRT-EXCEP AFTER ADVANCING 1 LINE                     ECS021
06320      ELSE                                                         ECS021
06321        IF PE-CTL IS EQUAL TO '0'                                  ECS021
06322          WRITE PRT-EXCEP AFTER ADVANCING 2 LINES                  ECS021
06323        ELSE                                                       ECS021
06324          IF PE-CTL IS EQUAL TO '3'                                ECS021
06325            WRITE PRT-EXCEP AFTER ADVANCING 3 LINES                ECS021
06326          ELSE                                                     ECS021
06327            WRITE PRT-EXCEP AFTER ADVANCING PAGE.                  ECS021
06328                                                                   ECS021
06329      MOVE SPACES TO PRT-EXCEP.                                    ECS021
06330                                                                   ECS021
06331  9079-EXIT.                                                       ECS021
06332      EXIT.                                                        ECS021
06333                                                                   ECS021
06334  EJECT                                                            ECS021
06335  END-OF-JOB SECTION.                                              ECS021
06336                                                                   ECS021
06337  9999-END-OF-JOB.                                                 ECS021
06338      DISPLAY '**** EPECS READ      **** ' EPECS-READ-COUNT.       ECS021
06339      DISPLAY '**** EPECS NON EP EC **** ' NON-EP-EC-DROP-CNT.     ECS021
06340      DISPLAY '**** EPECS REIN DROP **** ' REIN-EPEC-DROP-CNT.     ECS021
06341      DISPLAY '**** EPECS DATE DROP1**** ' EPEC-DATE-DROP-CNT1.    ECS021
06342      DISPLAY '**** EPECS DATE DROP2**** ' EPEC-DATE-DROP-CNT2.    ECS021
06343      DISPLAY '**** EPECS SELECTED  **** ' EPECS-SELECTED.         ECS021
06344      DISPLAY '**** EPECS SORTED    **** ' EPECS-SORTED.           ECS021
06345      DISPLAY '**** EPECS RETURNED  **** ' EPECS-RETURNED.         ECS021
06346      DISPLAY '**** ACCT STATUS DROP**** ' ACCT-STATUS-DROP.       ECS021
06347      DISPLAY '**** ACCT TERM DROP  **** ' ACCT-TERM-DROP.         ECS021
06348      DISPLAY '**** BAD ACCT STATUS **** ' BAD-ACCT-STATUS.        ECS021
06349      DISPLAY '**** BAD DATE DROP   **** ' BAD-DATE-LOGIC-CNT.     ECS021
06350      DISPLAY '**** CARRIER DROP    **** ' CARRIER-DROP-CNT.       ECS021
06351      DISPLAY '**** GROUP DROP      **** ' GROUP-DROP-CNT.         ECS021
06352      DISPLAY '**** STATE DROP      **** ' STATE-DROP-CNT.         ECS021
06353      DISPLAY '**** ACCOUNT DROP    **** ' ACCOUNT-DROP-CNT.       ECS021
06354      DISPLAY '**** AGENT DROP      **** ' AGENT-DROP-CNT.         ECS021
06355      DISPLAY '**** BUS TYPE DROP   **** ' BUS-TYP-DROP-CNT.       ECS021
06356      DISPLAY '**** LIFE BEN DROP   **** ' LIFE-BEN-DROP-CNT.      ECS021
06357      DISPLAY '**** AH BEN DROP     **** ' AH-BEN-DROP-CNT.        ECS021
06358      DISPLAY '**** RPT CD1 DROP    **** ' RPT-CODE1-DROP-CNT.     ECS021
06359      DISPLAY '**** RPT CD2 DROP    **** ' RPT-CODE2-DROP-CNT.     ECS021
06360      DISPLAY '**** USER 1 DROP     **** ' USER1-DROP-CNT.         ECS021
06361      DISPLAY '**** USER 2 DROP     **** ' USER2-DROP-CNT.         ECS021
06362      DISPLAY '**** USER 3 DROP     **** ' USER3-DROP-CNT.         ECS021
06363      DISPLAY '**** USER 4 DROP     **** ' USER4-DROP-CNT.         ECS021
06364      DISPLAY '**** USER 5 DROP     **** ' USER5-DROP-CNT.         ECS021
06365                                                                   ECS021
06366                              COPY ELCPRTC.                        ECS021

022804     IF ELCNTL-FILE-STATUS NOT = '23'
06367          CLOSE PRINTER
CIDMOD         CLOSE DODDS
CIDMOD         CLOSE MWAUTO
CIDMOD         CLOSE MIDWEST
06368          CLOSE EXCEP-PRINT
022804     END-IF.
102908     IF WS-REPORT-NO = '307' AND DTE-FMT-OPT = 3
102908         CLOSE DATA-OUT
102908     END-IF.
06369                                                                   ECS021
06370      GOBACK.                                                      ECS021
06371                                                                   ECS021
06372  ABEND-PGM SECTION.                                               ECS021
06373            COPY ELCABEND.                                         ECS021
