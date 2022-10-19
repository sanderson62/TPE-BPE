       IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS031
00003  PROGRAM-ID.                ECS031.                                  LV017
00004 *              PROGRAM CONVERTED BY                               ECS031
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS031
00006 *              CONVERSION DATE 11/28/95 11:07:32.                 ECS031
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            ECS031
00008 *                            VMOD=2.014.                          ECS031
00009                                                                   ECS031
00010 *AUTHOR.     LOGIC, INC.                                          ECS031
00011 *            DALLAS, TEXAS.                                       ECS031
00012                                                                   ECS031
00013 *DATE-COMPILED.                                                   ECS031
00014                                                                   ECS031
00015 *SECURITY.   *****************************************************ECS031
00016 *            *                                                   *ECS031
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS031
00018 *            *                                                   *ECS031
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS031
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS031
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS031
00022 *            *                                                   *ECS031
00023 *            *****************************************************ECS031
00024                                                                   ECS031
00025 *REMARKS.                                                         ECS031
00026                                                                   ECS031
00027 *    PRINT CLAIM REGISTER.                                        ECS031
00028 *       DTE-PGM-PIC (031) MUST BE SET                             ECS031
00029 *            1 = INCEPTION TO DATE PAID CLAIMS                    ECS031
00030 *            2 = YTD PAID CLAIM REGISTER - PAID YR = RUN YR       ECS031
00031 *            3 = LAST 12 MONTHS                                   ECS031
00032                                                                   ECS031
122321******************************************************************
122321*                   C H A N G E   L O G
122321*
122321* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122321*-----------------------------------------------------------------
122321*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122321* EFFECTIVE    NUMBER
122321*-----------------------------------------------------------------
122321* 010716  CR2021121700002  PEMA  Correct clm pmt counts
122321******************************************************************
00033  ENVIRONMENT DIVISION.                                            ECS031
00034  CONFIGURATION SECTION.                                           ECS031
00035                                                                      CL*14
00036  INPUT-OUTPUT SECTION.                                            ECS031
00037  FILE-CONTROL.                                                    ECS031
00038                                                                   ECS031
00039      SELECT  EXTRACT     ASSIGN TO SYS018-UT-2400-S-SYS018.       ECS031
00040      SELECT  PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS031
00041      SELECT  ACC-MSTR    ASSIGN TO SYS021-FBA1-ERACCTT            ECS031
00042                          ORGANIZATION IS INDEXED                  ECS031
00043                          ACCESS IS SEQUENTIAL                     ECS031
00044                          RECORD KEY IS AM-CONTROL-PRIMARY         ECS031
00045                          FILE STATUS IS ERACCT-FILE-STATUS.       ECS031
00046      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-3380-S-SYS019.       ECS031
00047      SELECT  CLM-CARR    ASSIGN TO SYS015-UT-3380-S-SYS015.       ECS031
00048      SELECT  CLM-SRT     ASSIGN TO SYS001-UT-3380-S-SORTWK1.      ECS031
00049      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS031
00050                                                                   ECS031
00051      EJECT                                                        ECS031
00052  DATA DIVISION.                                                   ECS031
00053  FILE SECTION.                                                    ECS031
00054                                                                   ECS031
00055      EJECT                                                        ECS031
00056  FD  ACC-MSTR.                                                    ECS031
00057                                                                   ECS031
00058                              COPY ERCACCT.                        ECS031
00059                                                                   ECS031
00060  FD  EXTRACT                                                      ECS031
00061                              COPY ECSEXTFD.                       ECS031
00062                                     COPY ECSEXT01.                ECS031
00063                                                                   ECS031
00064      EJECT                                                        ECS031
00065  FD  DISK-DATE                                                    ECS031
00066                              COPY ELCDTEFD.                       ECS031
00067                                                                   ECS031
00068      EJECT                                                        ECS031
00069  FD  PRNTR                                                        ECS031
00070                              COPY ELCPRTFD.                       ECS031
00071  01  DTL.                                                         ECS031
00072      12  FILLER      PIC X(2).                                    ECS031
00073      12  P-ST        PIC XX.                                      ECS031
00074      12  FILLER      PIC X(3).                                    ECS031
00075      12  P-ACCT      PIC X(10).                                   ECS031
00076      12  FILLER      PIC X.                                       ECS031
00077      12  P-CERT      PIC X(11).                                   ECS031
00078      12  FILLER      PIC X.                                       ECS031
00079      12  P-EMO       PIC 99.                                      ECS031
00080      12  P-EMOD      PIC X.                                       ECS031
00081      12  P-EDA       PIC 99.                                      ECS031
00082      12  P-EDAD      PIC X.                                       ECS031
00083      12  P-EYR       PIC 99.                                      ECS031
00084      12  FILLER      PIC X.                                       ECS031
00085      12  P-NAME      PIC X(13).                                   ECS031
00086      12  FILLER      PIC X.                                       ECS031
00087      12  P-INL1      PIC X.                                       ECS031
00088      12  P-INL2      PIC X.                                       ECS031
00089      12  FILLER      PIC X.                                       ECS031
00090      12  P-TYPE      PIC X(6).                                    ECS031
00091      12  FILLER      PIC X.                                       ECS031
00092      12  P-IMO       PIC 99.                                      ECS031
00093      12  P-IMOD      PIC X.                                       ECS031
00094      12  P-IDA       PIC 99.                                      ECS031
00095      12  P-IDAD      PIC X.                                       ECS031
00096      12  P-IYR       PIC 99.                                      ECS031
00097      12  FILLER      PIC X.                                       ECS031
00098      12  P-PMO       PIC 99.                                      ECS031
00099      12  P-PMOD      PIC X.                                       ECS031
00100      12  P-PDA       PIC 99.                                      ECS031
00101      12  P-PDAD      PIC X.                                       ECS031
00102      12  P-PYR       PIC 99.                                      ECS031
00103      12  FILLER      PIC XX.                                      ECS031
00104      12  P-CLMNO     PIC X(7).                                    ECS031
00105      12  P-DTH       PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.          ECS031
00106      12  P-DIS       PIC Z,ZZZ,ZZZ.99-  BLANK WHEN ZERO.          ECS031
00107      12  FILLER      PIC X.                                       ECS031
00108      12  P-CAUSE     PIC X(6).                                    ECS031
00109      12  FILLER      PIC X.                                       ECS031
00110      12  P-AGE       PIC XX.                                      ECS031
00111      12  FILLER      PIC X.                                       ECS031
00112      12  P-CHK-NO    PIC X(7).                                    ECS031
00113                                                                   ECS031
00114  01  DTL-C.                                                       ECS031
00115      12  FILL        PIC X.                                       ECS031
00116      12  DC-GROUP    PIC X(6).                                    ECS031
00117      12  FILL        PIC X.                                       ECS031
00118      12  C-ST        PIC XX.                                      ECS031
00119      12  FILL        PIC X.                                       ECS031
00120      12  FILL        PIC X(122).                                  ECS031
00121                                                                   ECS031
00122      EJECT                                                        ECS031
00123  FD  CLM-CARR                                                     ECS031
00124      BLOCK CONTAINS 0 RECORDS
00125      RECORDING MODE F.                                            ECS031
00126                                                                   ECS031
00127  01  CAR-REC.                                                     ECS031
00128      12  FILLER          PIC X(504).                              ECS031
00129      12  C-GROUP         PIC X(6).                                ECS031
00130                                                                   ECS031
00131      EJECT                                                        ECS031
00132  SD  CLM-SRT.                                                     ECS031
00133                                                                   ECS031
00134  01  SRT-REC.                                                     ECS031
00135      12  FILLER          PIC X(4).                                ECS031
00136      12  S-CAR           PIC X.                                   ECS031
00137      12  S-CO            PIC X(6).                                ECS031
00138      12  S-ST            PIC X(2).                                ECS031
00139      12  S-ACCT          PIC X(10).                               ECS031
00140      12  FILLER          PIC X(18).                               ECS031
00141      12  S-RPT-CD1       PIC X(10).                               ECS031
00142      12  S-RPT-CD2       PIC X(10).                               ECS031
00143      12  FILLER          PIC X(310).                              ECS031
00144      12  S-CLM           PIC X(7).                                ECS031
00145      12  S-CHK           PIC X(7).                                ECS031
00146      12  FILLER          PIC X(119).                              ECS031
00147      12  S-GROUP         PIC X(6).                                ECS031
00148                                                                   ECS031
00149  FD  FICH                                                         ECS031
00150                              COPY ELCFCHFD.                       ECS031
00151                                                                   ECS031
00152      EJECT                                                        ECS031
00153  WORKING-STORAGE SECTION.                                         ECS031
00154  77  FILLER  PIC X(32) VALUE '********************************'.  ECS031
00155  77  FILLER  PIC X(32) VALUE '     ECS031 WORKING STORAGE     '.  ECS031
00156  77  FILLER  PIC X(32) VALUE '**** VMOD=2.014  ***************'.  ECS031
00157                                                                   ECS031
00158  77  CLM-CTR             PIC S9(6)    VALUE +0.                   ECS031
00159  77  DET-END             PIC XXX     VALUE SPACE.                 ECS031
00160  77  ERACCT-FILE-STATUS  PIC X(2)    VALUE SPACE.                 ECS031
00161                                                                   ECS031
00162  01  WS-DE-COMP          PIC X(7) VALUE SPACES.                   ECS031
00163  01  WS.                                                          ECS031
00164      05  SAVE-AREA.                                               ECS031
00165          10  SAV-RPT-CD-1         PIC X(10)   VALUE SPACE.        ECS031
00166          10  SAV-RPT-CD-2         PIC X(10) VALUE SPACE.          ECS031
00167          10  SAV-CARRIER          PIC X     VALUE SPACE.          ECS031
00168          10  SAV-GROUP            PIC X(6)  VALUE SPACE.          ECS031
00169          10  SAV-STATE            PIC XX    VALUE SPACE.          ECS031
00170          10  SAV-ACCOUNT          PIC X(10) VALUE SPACE.          ECS031
00171          10  SAV-DE-CNTRL         PIC X(19) VALUE SPACE.          ECS031
00172                                                                   ECS031
00173      05  WS-START-DATE        PIC 9(11).                             CL*14
00174      05  WS-END-DATE          PIC 9(11).                             CL*14
00175                                                                      CL*14
00176      05  WS-ABEND-MESSAGE     PIC X(80) VALUE SPACES.             ECS031
00177      05  WS-ABEND-FILE-STATUS PIC X(4)  VALUE ZEROS.              ECS031
00178      05  WS-ZERO              PIC S9    VALUE ZERO.               ECS031
00179      05  WS-RETURN-CODE       PIC X(4)  VALUE ZEROS.              ECS031
00180      05  ABEND-OPTION         PIC X     VALUE 'Y'.                ECS031
00181      05  PGM-SUB              PIC S999    COMP    VALUE +031.     ECS031
00182                                                                   ECS031
00183      05  L12-DATE.                                                ECS031
00184          10  FILLER      PIC 999.                                    CL*14
00185          10  L12-CCYY    PIC 9(4).                                   CL*14
00186          10  L12-MO      PIC 99.                                     CL*14
00187          10  L12-DA      PIC 99.                                     CL*14
00188      05  L12-DATE-N  REDEFINES                                       CL*14
00189             L12-DATE     PIC 9(11).                                  CL*14
00190                                                                   ECS031
00191  01  BUILD-TYPE.                                                  ECS031
00192      05  FILLER          PIC X.                                   ECS031
00193      05  TYPE-LA         PIC XX.                                  ECS031
00194      05  TYPE-DASH       PIC X               VALUE '-'.           ECS031
00195      05  TYPE-CD         PIC X.                                   ECS031
00196      05  FILLER          PIC X.                                   ECS031
00197                                                                   ECS031
00198      EJECT                                                        ECS031
00199                              COPY ELCDTECX.                       ECS031
00200      EJECT                                                        ECS031
00201                              COPY ELCDTEVR.                       ECS031
00202      EJECT                                                        ECS031
00203                              COPY ELCEXTVR.                       ECS031
00204                                                                   ECS031
00205  01  HD1A.                                                        ECS031
00206      12  FILLER      PIC X(5)    VALUE SPACES.                    ECS031
00207      12  HD1A-CAPT   PIC X(10)   VALUE SPACES.                    ECS031
00208      12  HD1A-FILL   PIC XXX     VALUE SPACES.                    ECS031
00209      12  HD1A-RPT-CD PIC X(10)   VALUE SPACES.                    ECS031
00210      12  FILLER      PIC X(12)   VALUE SPACES.                    ECS031
00211      12  HD1A-KIND   PIC X(6)    VALUE SPACES.                    ECS031
00212      12  FILLER      PIC XX      VALUE SPACES.                    ECS031
00213      12  FILLER      PIC X(29)                                    ECS031
00214                            VALUE 'CLAIM REGISTER FOR CARRIER -'.  ECS031
00215      12  HD1A-CARR   PIC X.                                       ECS031
00216      12  HD1A-COMP   PIC X(9) VALUE ' GROUP -'.                   ECS031
00217      12  HD1A-GROUP  PIC X(6) VALUE SPACES.                       ECS031
00218      12  FILLER      PIC X(26) VALUE SPACES.                      ECS031
00219      12  FILLER      PIC X(6) VALUE 'ECS031'.                     ECS031
00220      12  HD1A-RPT    PIC X    VALUE 'A'.                          ECS031
00221      12  FILLER      PIC X(6) VALUE SPACES.                       ECS031
00222                                                                   ECS031
00223  01  HD1B.                                                        ECS031
00224      12  FILLER      PIC X(40) VALUE SPACES.                      ECS031
00225      12  HD1B-KIND2 PIC X(6)     VALUE SPACES.                    ECS031
00226      12  FILLER      PIC XX      VALUE SPACES.                    ECS031
00227      12  FILLER      PIC X(29) VALUE                              ECS031
00228                          'CLAIM REGISTER FOR CARRIER - '.         ECS031
00229      12  HD1B-CARR   PIC X.                                       ECS031
00230      12  FILLER      PIC X(41) VALUE SPACES.                      ECS031
00231      12  FILLER      PIC X(6) VALUE 'ECS031'.                     ECS031
00232      12  HD1B-RPT    PIC X    VALUE 'B'.                          ECS031
00233      12  FILLER      PIC X(06) VALUE SPACES.                      ECS031
00234                                                                   ECS031
00235  01  HD2.                                                         ECS031
00236      12  FILLER      PIC X(47)   VALUE SPACES.                    ECS031
00237      12  HD2-CO      PIC X(30).                                   ECS031
00238      12  FILLER      PIC X(42)   VALUE SPACES.                    ECS031
00239      12  HD2-RD      PIC X(08).                                   ECS031
00240      12  FILLER      PIC X(05)   VALUE SPACES.                    ECS031
00241                                                                   ECS031
00242  01  HD3.                                                         ECS031
00243      12  FILLER      PIC X(53)   VALUE SPACES.                    ECS031
00244      12  HD3-DT      PIC X(18).                                   ECS031
00245      12  FILLER      PIC X(48)   VALUE SPACES.                    ECS031
00246      12  FILLER      PIC X(5)    VALUE 'PAGE'.                    ECS031
00247      12  HD3-PG      PIC ZZ,ZZ9.                                  ECS031
00248      12  FILLER      PIC X(02)   VALUE SPACES.                    ECS031
00249                                                                   ECS031
00250  01  HD4.                                                         ECS031
00251      12  HD4-1   PIC X(28) VALUE ' ST     ACCOUNT     CERT.   '.  ECS031
00252      12  FILLER  PIC X(9)  VALUE 'EFFECTIVE'.                     ECS031
00253      12  FILLER  PIC X(23)  VALUE ' INSURED           TYPE'.      ECS031
00254      12  FILLER  PIC X(23)  VALUE '  INCURRED   DATE    CL'.      ECS031
00255      12  FILLER  PIC X(25)  VALUE 'AIM    ----AMOUNT OF CLAI'.    ECS031
00256      12  FILLER  PIC X(25)  VALUE 'M---- CAUSE   AGE CHECK  '.    ECS031
00257                                                                   ECS031
00258  01  HD5.                                                         ECS031
00259      12  FILLER  PIC X(12)  VALUE SPACES.                         ECS031
00260      12  FILLER  PIC X(49)  VALUE '                  DATE     '.  ECS031
00261      12  FILLER  PIC X(23)  VALUE '   DATE     PAID    NUM'.      ECS031
00262      12  FILLER  PIC X(25)  VALUE 'BER         LIFE         '.    ECS031
00263      12  FILLER  PIC X(25)  VALUE ' A&H   CD        NUMBER  '.    ECS031
00264                                                                   ECS031
00265  01  HD5G.                                                        ECS031
00266      12  FILLER  PIC X(12)  VALUE SPACES.                         ECS031
00267      12  FILLER  PIC X(49)  VALUE '                  DATE     '.  ECS031
00268      12  FILLER  PIC X(23)  VALUE '   DATE     PAID    NUM'.      ECS031
00269      12  FILLER  PIC X(25)  VALUE 'BER         LIFE         '.    ECS031
00270      12  FILLER  PIC X(25)  VALUE ' A&H   CD        NUMBER  '.    ECS031
00271                                                                   ECS031
00272  01  HD6.                                                         ECS031
00273      12  FILL    PIC X(28) VALUE ' GROUP ST    ACCOUNT    CERT'.  ECS031
00274                                                                   ECS031
00275      EJECT                                                        ECS031
00276  01  COMP-3-AREA     COMP-3.                                      ECS031
00277      12  PG-NO       PIC S9(5)        VALUE +0.                   ECS031
00278      12  LN-CT       PIC S9(5)        VALUE +0.                   ECS031
00279      12  CNT         PIC S9(6)        VALUE +0.                   ECS031
00280      12  T-DTH       PIC S9(10)V99    VALUE +0.                   ECS031
00281      12  T-DIS       PIC S9(10)V99    VALUE +0.                   ECS031
00282                                                                   ECS031
00283  01  G-TOTCNT        PIC S9(6)       VALUE +0.                    ECS031
00284  01  G-TOTDTH        PIC S9(10)V99   VALUE +0.                    ECS031
00285  01  G-TOTDIS        PIC S9(10)V99   VALUE +0.                    ECS031
00286                                                                   ECS031
00287  01  MISC-WORK-AREA.                                              ECS031
00288      12  WK-DATE.                                                    CL*12
00289          16  FILLER  PIC 999.                                     ECS031
00290          16  W-CC    PIC 99.                                      ECS031
00291          16  W-YR    PIC 99.                                      ECS031
00292          16  W-MO    PIC 99.                                      ECS031
00293          16  W-DA    PIC 99.                                      ECS031
00294      12  WK-DATE-N  REDEFINES                                        CL*12
00295            WK-DATE   PIC 9(11).                                      CL*12
00296                                                                      CL*12
00297      12  X           PIC X       VALUE SPACE.                     ECS031
00298      12  CAR-SW      PIC X       VALUE SPACE.                     ECS031
00299      12  SAV-COMP    PIC X(7)    VALUE SPACES.                    ECS031
00300      12  PREV-ACT    PIC X(10)   VALUE SPACES.                    ECS031
00301      12  PREV-STATE  PIC XX      VALUE SPACES.                    ECS031
00302      12  SAV-PND-SW  PIC X.                                       ECS031
00303                                                                   ECS031
00304  01  TL-LN.                                                       ECS031
00305      12  FILLER      PIC X(8)   VALUE SPACES.                     ECS031
00306      12  TLC         PIC X(24)  VALUE 'TOTAL FOR CARRIER/GROUP'.  ECS031
00307      12  TL-CO       PIC X(10)  VALUE SPACES.                     ECS031
00308      12  FILLER      PIC X(25)  VALUE SPACES.                     ECS031
122321     12  TL-CNT      PIC ZZZ,ZZ9-.                                ECS031
00310      12  FILLER      PIC X(12)  VALUE ' CLAIMS FOR '.             ECS031
00311      12  TL-DTH      PIC $(6),$$$.99-.                            ECS031
00312      12  TL-DIS      PIC $(6),$$$.99-.                            ECS031
00313                                                                   ECS031
00314  01  G-T-LN.                                                      ECS031
00315      12  FILL        PIC X(8)    VALUE SPACE.                     ECS031
00316      12  FILL        PIC X(18)   VALUE 'GRAND TOTALS'.            ECS031
00317      12  FILL        PIC X(38)   VALUE SPACE.                     ECS031
00318      12  GT-CNT      PIC ZZZ,ZZ9.                                 ECS031
00319      12  FILLNT      PIC X(11)   VALUE ' CLAIMS FOR'.             ECS031
00320      12  FILLER      PIC X(3)    VALUE SPACES.                    ECS031
00321      12  GT-DTH      PIC $$$$$,$$$,$$$.99-.                       ECS031
00322      12  FILLER      PIC X(2)    VALUE SPACES.                    ECS031
00323      12  GT-DIS      PIC $$$$,$$$,$$$.99-.                        ECS031
00324                                                                   ECS031
00325  01  TL-CONSTANT.                                                 ECS031
00326      12  TL-CON  PIC X(12)              VALUE SPACE.              ECS031
00327      12  TL-CAP  PIC X(10)              VALUE SPACE.              ECS031
00328      12  TL-FILL PIC XX                 VALUE SPACE.              ECS031
00329                                                                   ECS031
00330  01  TOTALS              COMP-3.                                  ECS031
00331      12  A-T-CNT         PIC S9(6)      VALUE +0.                 ECS031
00332      12  A-T-DTH         PIC S9(9)V99   VALUE +0.                 ECS031
00333      12  A-T-DIS         PIC S9(9)V99   VALUE +0.                 ECS031
00334      12  S-T-CNT         PIC S9(6)      VALUE +0.                 ECS031
00335      12  S-T-DTH         PIC S9(9)V99   VALUE +0.                 ECS031
00336      12  S-T-DIS         PIC S9(9)V99   VALUE +0.                 ECS031
00337      12  RPT-CD2-CNT     PIC S9(6)      VALUE +0.                 ECS031
00338      12  RPT-CD2-DTH     PIC S9(8)V99   VALUE +0.                 ECS031
00339      12  RPT-CD2-DIS     PIC S9(8)V99   VALUE +0.                 ECS031
00340      12  GRP-CNT         PIC S9(6)      VALUE +0.                 ECS031
00341      12  GRP-DTH         PIC S9(8)V99   VALUE +0.                 ECS031
00342      12  GRP-DIS         PIC S9(8)V99   VALUE +0.                 ECS031
00343      12  CAR-CNT         PIC S9(6)      VALUE +0.                 ECS031
00344      12  CAR-DTH         PIC S9(8)V99   VALUE +0.                 ECS031
00345      12  CAR-DIS         PIC S9(8)V99   VALUE +0.                 ECS031
00346      12  RPT-CD1-CNT     PIC S9(6)      VALUE +0.                 ECS031
00347      12  RPT-CD1-DTH     PIC S9(8)V99   VALUE +0.                 ECS031
00348      12  RPT-CD1-DIS     PIC S9(8)V99   VALUE +0.                 ECS031
00349      12  FIN-CNT         PIC S9(6)      VALUE +0.                 ECS031
00350      12  FIN-DTH         PIC S9(8)V99   VALUE +0.                 ECS031
00351      12  FIN-DIS         PIC S9(8)V99   VALUE +0.                 ECS031
00352                                                                   ECS031
00353      EJECT                                                        ECS031
00354                                COPY ELCDATE.                         CL*16
00355                                                                   ECS031
00356      EJECT                                                        ECS031
00357  PROCEDURE DIVISION.                                              ECS031
00358  0100-SET-START.                                                  ECS031
00359                              COPY ELCDTERX.                       ECS031
00360                                                                   ECS031
00361      OPEN INPUT EXTRACT                                           ECS031
00362                 ACC-MSTR                                          ECS031
00363           OUTPUT PRNTR.                                           ECS031
00364                                                                   ECS031
00365      MOVE SPACES                 TO AM-CONTROL-A.                 ECS031
00366                                                                   ECS031
00367      IF ERACCT-FILE-STATUS = ZERO OR '97'                         ECS031
00368          NEXT SENTENCE                                            ECS031
00369      ELSE                                                         ECS031
00370          MOVE 'ERROR OCCURED OPEN - ERACCTT' TO WS-ABEND-MESSAGE  ECS031
00371          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS031
00372          PERFORM ABEND-PGM.                                       ECS031
00373                                                                   ECS031
00374      IF EP-DT NOT = RUN-DATE                                         CL*14
00375          MOVE EP-DT              TO DC-GREG-DATE-CYMD                CL*14
00376          MOVE 'L'                TO DC-OPTION-CODE                   CL*14
00377          CALL 'ELDATCX' USING DATE-CONVERSION-DATA                   CL*14
00378          MOVE DC-GREG-DATE-1-ALPHA TO ALPH-DATE.                     CL*14
00379                                                                   ECS031
00380      MOVE COMPANY-NAME           TO HD2-CO.                       ECS031
00381      MOVE ALPH-DATE              TO HD3-DT.                       ECS031
00382      MOVE WS-CURRENT-DATE        TO HD2-RD.                       ECS031
00383                                                                   ECS031
00384      IF DTE-PGM-OPT = '1'                                         ECS031
00385          MOVE 'ITD'              TO HD1A-KIND HD1B-KIND2          ECS031
00386        ELSE                                                       ECS031
00387      IF DTE-PGM-OPT = '2'                                         ECS031
00388          MOVE 'YTD'              TO HD1A-KIND HD1B-KIND2          ECS031
00389        ELSE                                                       ECS031
00390      IF DTE-PGM-OPT = '3'                                         ECS031
00391          MOVE RUN-DATE           TO L12-DATE-N                       CL*14
00392          SUBTRACT 1 FROM L12-CCYY                                    CL*14
00393          MOVE 'L12'              TO HD1A-KIND HD1B-KIND2          ECS031
00394      ELSE                                                         ECS031
00395      IF DTE-PGM-OPT = 4                                           ECS031
00396         MOVE RUN-DATE            TO WS-START-DATE                    CL*14
00397         MOVE EP-DT               TO WS-END-DATE                      CL*14
00398         MOVE 'PERIOD'            TO HD1A-KIND HD1B-KIND2.         ECS031
00399                                                                   ECS031
00400  0110-SORT-IN SECTION.                                            ECS031
00401      IF DTE-FMT-OPT = 2                                           ECS031
00402          NEXT SENTENCE                                            ECS031
00403      ELSE                                                         ECS031
00404          SORT CLM-SRT                                             ECS031
00405              ASCENDING KEY S-CAR                                  ECS031
00406                            S-CLM                                  ECS031
00407                            S-CHK                                  ECS031
00408              INPUT PROCEDURE 0120-GET-CLAIM THRU 0380-STDE-ONE-X  ECS031
00409              GIVING CLM-CARR                                      ECS031
00410          PERFORM 0115-CK-SRT-RETURN                               ECS031
00411          GO TO 0390-STDE-TWO.                                     ECS031
00412                                                                   ECS031
00413      SORT CLM-SRT                                                 ECS031
00414          ASCENDING KEY S-RPT-CD1                                  ECS031
00415                        S-CAR                                      ECS031
00416                        S-CO                                       ECS031
00417                        S-RPT-CD2                                  ECS031
00418                        S-ST                                       ECS031
00419                        S-ACCT                                     ECS031
00420          INPUT PROCEDURE 0120-GET-CLAIM THRU 0380-STDE-ONE-X      ECS031
00421              GIVING CLM-CARR.                                     ECS031
00422                                                                      CL*11
00423      PERFORM 0115-CK-SRT-RETURN.                                     CL*11
00424                                                                      CL*11
00425      GO TO 0400-PRODUCE-REPORT.                                      CL*11
00426                                                                   ECS031
00427  0115-CK-SRT-RETURN.                                              ECS031
00428      IF SORT-RETURN NOT = ZEROS                                   ECS031
00429          MOVE '0101'             TO WS-RETURN-CODE                ECS031
00430          GO TO ABEND-PGM.                                         ECS031
00431                                                                   ECS031
00432  0120-GET-CLAIM  SECTION.                                         ECS031
00433                                                                   ECS031
00434      READ EXTRACT                                                 ECS031
00435          AT END  GO TO 0370-END-STEP.                             ECS031
00436                                                                   ECS031
00437      IF DE-RECORD-ID NOT = 'DE'                                   ECS031
00438          GO TO 0120-GET-CLAIM.                                    ECS031
00439                                                                   ECS031
00440      IF DE-REIN NOT = SPACE                                       ECS031
00441          GO TO 0120-GET-CLAIM.                                    ECS031
00442                                                                   ECS031
00443      IF NOT DE-CLAIM                                              ECS031
00444          GO TO 0120-GET-CLAIM.                                    ECS031
00445                                                                   ECS031
00446      COPY ELCEXTM1.                                                  CL*14
00447                                                                      CL*14
00448      IF DTE-FMT-OPT = 2                                           ECS031
00449          IF DE-CNTRL1 NOT = SAV-DE-CNTRL                          ECS031
00450              MOVE SPACES         TO SAV-RPT-CD-1                  ECS031
00451                                     SAV-RPT-CD-2                  ECS031
00452              PERFORM 0720-RD-ACCT-MSTR THRU 0720-EXIT             ECS031
00453              MOVE SAV-RPT-CD-1   TO DE-REPORT-CODE-1              ECS031
00454              MOVE SAV-RPT-CD-2   TO DE-REPORT-CODE-2              ECS031
00455          ELSE                                                     ECS031
00456              MOVE SAV-RPT-CD-1   TO DE-REPORT-CODE-1              ECS031
00457              MOVE SAV-RPT-CD-2   TO DE-REPORT-CODE-2.             ECS031
00458                                                                   ECS031
00459      IF DE-CLM-PROC-DT NOT NUMERIC                                ECS031
00460          MOVE DE-PAY             TO DE-CLM-PROC-DT.                  CL*13
00461                                                                   ECS031
00462      IF DTE-PGM-OPT = 1                                           ECS031
00463         IF DE-CLM-PROC-DT GREATER RUN-DATE                           CL*14
00464            GO TO 0120-GET-CLAIM                                   ECS031
00465         ELSE                                                         CL*15
00466            GO TO 0130-ADD-CLM-CTR.                                ECS031
00467                                                                   ECS031
00468      IF DTE-PGM-OPT = 2                                           ECS031
00469         IF DE-CP-CCYY NOT = RUN-CCYY                                 CL*14
00470            GO TO 0120-GET-CLAIM.                                  ECS031
00471                                                                   ECS031
00472      IF DTE-PGM-OPT = 2                                           ECS031
00473         IF DE-CP-CCYY = RUN-CCYY  AND                                CL*14
00474            DE-CP-MO GREATER THAN RUN-MO                              CL*14
00475            GO TO 0120-GET-CLAIM.                                  ECS031
00476                                                                   ECS031
00477      IF DTE-PGM-OPT = 3                                           ECS031
00478         IF DE-CLM-PROC-DT NOT GREATER L12-DATE-N  OR                 CL*15
00479                               GREATER RUN-DATE                       CL*14
00480            GO TO 0120-GET-CLAIM.                                  ECS031
00481                                                                   ECS031
00482      IF DTE-PGM-OPT = 4                                           ECS031
00483         IF DE-CLM-PROC-DT NOT GREATER WS-START-DATE  OR              CL*15
00484                               GREATER WS-END-DATE                 ECS031
00485            GO TO 0120-GET-CLAIM.                                  ECS031
00486                                                                   ECS031
00487  0130-ADD-CLM-CTR.                                                ECS031
00488      IF DE-VOIDED-PAYMENT                                         ECS031
00489          SUBTRACT +1 FROM CLM-CTR                                 ECS031
00490      ELSE                                                         ECS031
00491         IF DE-CLAIM-AMT NOT EQUAL ZEROS                           ECS031
00492             ADD +1 TO CLM-CTR.                                    ECS031
00493                                                                   ECS031
00494      IF PREV-ACT = SPACE                                          ECS031
00495          MOVE DE-ACCOUNT TO PREV-ACT.                             ECS031
00496                                                                   ECS031
00497      IF PREV-STATE = SPACE                                        ECS031
00498          MOVE DE-STATE   TO PREV-STATE.                           ECS031
00499                                                                   ECS031
00500      STRING DE-CARRIER  DELIMITED BY SIZE                         ECS031
00501             DE-GROUPING DELIMITED BY SIZE                         ECS031
00502      INTO WS-DE-COMP.                                             ECS031
00503                                                                   ECS031
00504  0140-MAIN-LOOP.                                                  ECS031
00505      IF SAV-COMP = SPACES                                         ECS031
00506          GO TO 0200-SET-NEW.                                      ECS031
00507                                                                   ECS031
00508      IF WS-DE-COMP NOT = SAV-COMP                                 ECS031
00509          GO TO 0190-PRT-TOTALS.                                   ECS031
00510                                                                   ECS031
00511      IF CAR-SW NOT = SPACE                                        ECS031
00512          GO TO 0150-CONT-RPT.                                     ECS031
00513                                                                   ECS031
00514      IF DE-ACCOUNT NOT = PREV-ACT                                 ECS031
00515          PERFORM 0350-ACCT-TOT THRU 0350-EXIT.                    ECS031
00516                                                                   ECS031
00517      IF DE-STATE   NOT = PREV-STATE                               ECS031
00518          PERFORM 0360-STATE-TOT THRU 0360-EXIT.                   ECS031
00519                                                                   ECS031
00520  0150-CONT-RPT.                                                   ECS031
00521      MOVE SPACES                 TO DTL.                          ECS031
00522      MOVE DE-STATE               TO P-ST.                         ECS031
00523      MOVE DE-ACCOUNT             TO P-ACCT.                       ECS031
00524      MOVE DE-CERT                TO P-CERT.                       ECS031
00525      MOVE DE-EFF                 TO WK-DATE-N.                       CL*12
00526      MOVE W-YR                   TO P-EYR.                           CL*17
00527      MOVE W-MO                   TO P-EMO.                           CL*17
00528      MOVE W-DA                   TO P-EDA.                           CL*17
00529      MOVE '-'                    TO P-EMOD  P-EDAD.               ECS031
00530      MOVE DE-LNAME               TO P-NAME.                       ECS031
00531      MOVE DE-1ST-INIT-FNAME      TO P-INL1.                       ECS031
00532      MOVE DE-INIT                TO P-INL2.                       ECS031
00533      MOVE DE-CNUM                TO P-CLMNO.                      ECS031
00534                                                                   ECS031
00535      IF DTE-CLIENT EQUAL 'HAN'                                    ECS031
00536         NEXT SENTENCE                                             ECS031
00537      ELSE                                                         ECS031
00538         IF DE-OB-DTH OR DE-OB-AH                                  ECS031
00539             MOVE 'O.B. CERT.'    TO P-NAME                        ECS031
00540             MOVE SPACES          TO P-INL1 P-INL2.                ECS031
00541                                                                   ECS031
00542      MOVE ZEROS                  TO P-DTH  P-DIS.                 ECS031
00543                                                                   ECS031
00544      IF DE-DTH  OR  DE-OB-DTH                                     ECS031
00545          GO TO 0160-PT-DEATH.                                     ECS031
00546                                                                   ECS031
00547      IF DE-AH  OR  DE-OB-AH                                       ECS031
00548          GO TO 0170-PT-DISAB.                                     ECS031
00549                                                                   ECS031
00550      MOVE 'UNKNO'                TO  P-TYPE.                      ECS031
00551                                                                   ECS031
00552      GO TO 0180-PT-LST.                                           ECS031
00553                                                                   ECS031
00554  0160-PT-DEATH.                                                   ECS031
00555      MOVE LIFE-OVERRIDE-L6       TO P-TYPE.                       ECS031
00556                                                                   ECS031
00557      IF DE-PAY-CODE = 'E'                                         ECS031
00558         MOVE LIFE-OVERRIDE-L2    TO TYPE-LA                       ECS031
00559         MOVE 'E'                 TO TYPE-CD                       ECS031
00560         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00561                                                                   ECS031
00562      IF DE-PAY-CODE = 'F'                                         ECS031
00563         MOVE LIFE-OVERRIDE-L2    TO TYPE-LA                       ECS031
00564         MOVE 'F'                 TO TYPE-CD                       ECS031
00565         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00566                                                                   ECS031
00567      IF DE-PAY-CODE = 'P'                                         ECS031
00568         MOVE LIFE-OVERRIDE-L2    TO TYPE-LA                       ECS031
00569         MOVE 'P'                 TO TYPE-CD                       ECS031
00570         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00571                                                                   ECS031
00572      IF DE-PAY-CODE = 'A'                                         ECS031
00573         MOVE LIFE-OVERRIDE-L2    TO TYPE-LA                       ECS031
00574         MOVE 'A'                 TO TYPE-CD                       ECS031
00575         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00576                                                                   ECS031
00577      IF DE-PAY-CODE = 'V'                                         ECS031
00578         MOVE LIFE-OVERRIDE-L2    TO TYPE-LA                       ECS031
00579         MOVE 'V'                 TO TYPE-CD                       ECS031
00580         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00581                                                                   ECS031
00582      ADD DE-CLAIM-AMT TO T-DTH                                    ECS031
00583                          A-T-DTH                                  ECS031
00584                          S-T-DTH.                                 ECS031
00585      MOVE DE-CLAIM-AMT           TO  P-DTH.                       ECS031
00586                                                                   ECS031
00587      GO TO 0180-PT-LST.                                           ECS031
00588                                                                   ECS031
00589  0170-PT-DISAB.                                                   ECS031
00590      MOVE AH-OVERRIDE-L6         TO P-TYPE.                       ECS031
00591                                                                   ECS031
00592      IF DE-PAY-CODE = 'F'                                         ECS031
00593         MOVE AH-OVERRIDE-L2      TO TYPE-LA                       ECS031
00594         MOVE 'F'                 TO TYPE-CD                       ECS031
00595         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00596                                                                   ECS031
00597      IF DE-PAY-CODE = 'P'                                         ECS031
00598         MOVE AH-OVERRIDE-L2      TO TYPE-LA                       ECS031
00599         MOVE 'P'                 TO TYPE-CD                       ECS031
00600         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00601                                                                   ECS031
00602      IF DE-PAY-CODE = 'A'                                         ECS031
00603         MOVE AH-OVERRIDE-L2      TO TYPE-LA                       ECS031
00604         MOVE 'A'                 TO TYPE-CD                       ECS031
00605         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00606                                                                   ECS031
00607      IF DE-PAY-CODE = 'V'                                         ECS031
00608         MOVE AH-OVERRIDE-L2      TO TYPE-LA                       ECS031
00609         MOVE 'V'                 TO TYPE-CD                       ECS031
00610         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00611                                                                   ECS031
00612      IF DE-PAY-CODE = 'E'                                         ECS031
00613         MOVE AH-OVERRIDE-L2      TO TYPE-LA                       ECS031
00614         MOVE 'E'                 TO TYPE-CD                       ECS031
00615         MOVE BUILD-TYPE          TO P-TYPE.                       ECS031
00616                                                                   ECS031
00617      ADD DE-CLAIM-AMT TO T-DIS                                    ECS031
00618                          A-T-DIS                                  ECS031
00619                          S-T-DIS.                                 ECS031
00620      MOVE  DE-CLAIM-AMT          TO  P-DIS.                       ECS031
00621                                                                   ECS031
00622  0180-PT-LST.                                                     ECS031
00623      IF CAR-SW = '9'                                              ECS031
00624          MOVE SPACES             TO P-ST                          ECS031
00625          MOVE C-GROUP            TO DC-GROUP                      ECS031
00626          MOVE DE-STATE           TO C-ST.                         ECS031
00627                                                                   ECS031
00628      MOVE DE-ACCOUNT             TO PREV-ACT.                     ECS031
00629      MOVE DE-INCUR               TO WK-DATE-N.                       CL*12
00630      MOVE W-YR                   TO P-IYR.                           CL*17
00631      MOVE W-MO                   TO P-IMO.                           CL*17
00632      MOVE W-DA                   TO P-IDA.                           CL*17
00633      MOVE '-'                    TO P-IMOD  P-IDAD.               ECS031
00634      MOVE DE-CLM-AGE             TO P-AGE.                        ECS031
00635      MOVE DE-PAY                 TO WK-DATE-N.                       CL*12
00636      MOVE W-YR                   TO P-PYR.                           CL*17
00637      MOVE W-MO                   TO P-PMO.                           CL*17
00638      MOVE W-DA                   TO P-PDA.                           CL*17
00639      MOVE '-'                    TO P-PMOD  P-PDAD.               ECS031
00640      MOVE DE-CLM-CAUSE           TO P-CAUSE.                      ECS031
00641      MOVE DE-CHECK               TO P-CHK-NO.                     ECS031
00642                                                                   ECS031
00643      MOVE ' '                    TO X.                            ECS031
00644      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00645                                                                   ECS031
00646      ADD 1 TO LN-CT.                                              ECS031
00647                                                                   ECS031
122321     IF (DE-VOIDED-PAYMENT)
122321        or (de-claim-amt < +0)
00649          SUBTRACT +1 FROM CNT                                     ECS031
00650          SUBTRACT +1 FROM A-T-CNT                                 ECS031
00651          SUBTRACT +1 FROM S-T-CNT                                 ECS031
00652      ELSE                                                         ECS031
00653         IF DE-CLAIM-AMT NOT EQUAL ZEROS                           ECS031
00654             ADD 1 TO CNT                                          ECS031
00655                      A-T-CNT                                      ECS031
00656                      S-T-CNT.                                     ECS031
00657                                                                   ECS031
00658  0180-CONT.                                                       ECS031
00659      IF LN-CT GREATER 52                                          ECS031
00660          PERFORM 0210-PT-HDNG.                                    ECS031
00661                                                                   ECS031
00662      IF CAR-SW = SPACE                                            ECS031
00663          PERFORM 0230-WRITE-CAR THRU 0240-EXIT.                   ECS031
00664                                                                   ECS031
00665      IF CAR-SW = '9'                                              ECS031
00666          GO TO 0260-GET-CAR.                                      ECS031
00667                                                                   ECS031
00668      GO TO 0120-GET-CLAIM.                                        ECS031
00669                                                                   ECS031
00670  0190-PRT-TOTALS.                                                 ECS031
00671      IF CAR-SW NOT = '9'                                             CL*11
00672          PERFORM 0350-ACCT-TOT  THRU 0350-EXIT                       CL*11
00673          PERFORM 0360-STATE-TOT THRU 0360-EXIT.                   ECS031
00674                                                                   ECS031
00675      MOVE 'TOTAL FOR CARRIER/GROUP' TO TLC.                       ECS031
00676      MOVE CNT                  TO TL-CNT.                         ECS031
00677      MOVE T-DTH                TO TL-DTH.                         ECS031
00678      MOVE T-DIS                TO TL-DIS.                         ECS031
00679      ADD T-DTH                 TO G-TOTDTH.                       ECS031
00680      ADD T-DIS                 TO G-TOTDIS.                       ECS031
00681      ADD CNT                   TO G-TOTCNT.                       ECS031
00682      MOVE TL-LN                TO P-DATA.                         ECS031
00683                                                                   ECS031
00684      MOVE '0' TO X.                                               ECS031
00685      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00686                                                                   ECS031
00687      MOVE SPACES               TO SAV-COMP.                       ECS031
00688                                                                   ECS031
00689  0200-SET-NEW.                                                    ECS031
00690      IF CAR-SW = '9'                                              ECS031
00691          MOVE WS-DE-COMP       TO HD1B-CARR TL-CO SAV-COMP        ECS031
00692          MOVE HD1B             TO HD1A                            ECS031
00693       ELSE                                                        ECS031
00694          MOVE DE-CARRIER       TO  HD1A-CARR                      ECS031
00695          MOVE DE-GROUPING      TO  HD1A-GROUP                     ECS031
00696          MOVE WS-DE-COMP       TO  TL-CO SAV-COMP.                ECS031
00697                                                                   ECS031
00698      MOVE DE-ACCOUNT           TO PREV-ACT.                       ECS031
00699      MOVE DE-STATE             TO PREV-STATE.                     ECS031
00700      MOVE ZEROS                TO CNT                             ECS031
00701                                   T-DTH                           ECS031
00702                                   T-DIS.                          ECS031
00703      EJECT                                                        ECS031
00704  0210-PT-HDNG.                                                    ECS031
00705      ADD 1     TO PG-NO.                                          ECS031
00706      MOVE PG-NO                TO HD3-PG                          ECS031
00707      MOVE HD1A                 TO P-DATA.                         ECS031
00708      MOVE '1'                  TO X.                              ECS031
00709      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00710                                                                   ECS031
00711      MOVE HD2                  TO P-DATA.                         ECS031
00712      MOVE ' '                  TO X.                              ECS031
00713      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00714                                                                   ECS031
00715      MOVE HD3                  TO P-DATA.                         ECS031
00716      MOVE ' '                  TO X.                              ECS031
00717      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00718                                                                   ECS031
00719      MOVE HD4                  TO P-DATA.                         ECS031
00720      MOVE '0'                  TO X.                              ECS031
00721      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00722                                                                   ECS031
00723      MOVE HD5                  TO P-DATA.                         ECS031
00724      MOVE ' '                  TO X.                              ECS031
00725      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00726                                                                   ECS031
00727      MOVE SPACES               TO P-DATA.                         ECS031
00728      MOVE ' '                  TO X.                              ECS031
00729      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00730                                                                   ECS031
00731      MOVE SPACES               TO P-DATA.                         ECS031
00732      MOVE ZERO                 TO LN-CT.                          ECS031
00733                                                                   ECS031
00734  0220-P-H-X.                                                      ECS031
00735      GO TO 0140-MAIN-LOOP.                                        ECS031
00736                                                                   ECS031
00737      EJECT                                                        ECS031
00738  0230-WRITE-CAR.                                                  ECS031
00739                                                                      CL*14
00740      MOVE DETAIL-EXTRACT       TO SRT-REC.                        ECS031
00741      MOVE DE-GROUPING          TO S-GROUP.                        ECS031
00742      IF DTE-FMT-OPT = 1                                           ECS031
00743          MOVE SPACES           TO S-CO.                           ECS031
00744                                                                   ECS031
00745      RELEASE SRT-REC.                                             ECS031
00746                                                                   ECS031
00747  0240-EXIT.                                                       ECS031
00748      EXIT.                                                        ECS031
00749                                                                   ECS031
00750  0250-PRNT-CARR.                                                  ECS031
00751      MOVE ZEROS                TO LN-CT                           ECS031
00752                                   T-DIS  T-DTH                    ECS031
00753                                   G-TOTDTH  G-TOTDIS  G-TOTCNT.   ECS031
00754      MOVE 'TOTAL FOR CARRIER'  TO TLC.                            ECS031
00755      MOVE HD1B                 TO HD1A.                           ECS031
00756      MOVE HD6                  TO HD4-1.                          ECS031
00757      MOVE HD5G                 TO HD5.                            ECS031
00758      MOVE SPACES               TO SAV-COMP WS-DE-COMP.            ECS031
00759                                                                   ECS031
00760  0260-GET-CAR.                                                    ECS031
00761      READ CLM-CARR INTO DETAIL-EXTRACT                            ECS031
00762          AT END  GO TO 0270-EXIT.                                 ECS031
00763                                                                   ECS031
00764      IF NOT DE-CLAIM                                              ECS031
00765          GO TO 0260-GET-CAR.                                      ECS031
00766                                                                   ECS031
00767      MOVE DE-CARRIER           TO WS-DE-COMP.                     ECS031
00768      GO TO 0140-MAIN-LOOP.                                        ECS031
00769                                                                   ECS031
00770  0270-EXIT.                                                       ECS031
00771      EXIT.                                                        ECS031
00772                                                                   ECS031
00773  0280-NO-INPUT.                                                   ECS031
00774      MOVE ' NO CLAIMS INPUT RECEIVED FOR ECS031' TO P-DATA.       ECS031
00775      MOVE ' '                    TO X.                            ECS031
00776      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00777                                                                   ECS031
00778  0290-EXIT.                                                       ECS031
00779      EXIT.                                                        ECS031
00780                                                                   ECS031
00781      EJECT                                                        ECS031
00782  0300-PRT-RTN.                                                    ECS031
00783                              COPY ELCPRT2.                        ECS031
00784  0310-EXIT.                                                       ECS031
00785       EXIT.                                                       ECS031
00786                                                                   ECS031
00787      EJECT                                                        ECS031
00788                                                                   ECS031
00789  0330-G-TOTALS.                                                   ECS031
00790      MOVE SPACES                 TO HD1A-COMP                     ECS031
00791                                     HD1B-CARR                     ECS031
00792                                     HD1A-CARR                     ECS031
00793                                     HD1A-GROUP.                   ECS031
00794      PERFORM 0210-PT-HDNG.                                        ECS031
00795                                                                   ECS031
00796      MOVE G-TOTCNT               TO GT-CNT.                       ECS031
00797      MOVE G-TOTDTH               TO GT-DTH.                       ECS031
00798      MOVE G-TOTDIS               TO GT-DIS.                       ECS031
00799      MOVE G-T-LN                 TO P-DATA.                       ECS031
00800                                                                   ECS031
00801      MOVE '0' TO X.                                               ECS031
00802      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00803                                                                   ECS031
00804  0340-EXIT.                                                       ECS031
00805      EXIT.                                                        ECS031
00806                                                                   ECS031
00807  0350-ACCT-TOT.                                                   ECS031
00808      MOVE 'TOTAL FOR ACCOUNT'    TO TLC.                          ECS031
00809      MOVE SPACE                  TO TL-CO                         ECS031
00810                                     P-DATA.                       ECS031
00811      MOVE A-T-CNT                TO TL-CNT.                       ECS031
00812      MOVE A-T-DTH                TO TL-DTH.                       ECS031
00813      MOVE A-T-DIS                TO TL-DIS.                       ECS031
00814      MOVE TL-LN                  TO P-DATA.                       ECS031
00815                                                                   ECS031
00816      MOVE ' '                    TO X.                            ECS031
00817      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00818                                                                   ECS031
00819      MOVE SPACE                  TO P-DATA.                       ECS031
00820      MOVE ' '                    TO X.                            ECS031
00821      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00822                                                                   ECS031
00823      ADD 3 TO LN-CT.                                              ECS031
00824      MOVE ZEROS                  TO A-T-CNT  A-T-DTH  A-T-DIS.    ECS031
00825                                                                   ECS031
00826  0350-EXIT.                                                       ECS031
00827      EXIT.                                                        ECS031
00828                                                                   ECS031
00829  0360-STATE-TOT.                                                  ECS031
00830      MOVE 'TOTAL FOR STATE'      TO TLC.                          ECS031
00831      MOVE SPACE                  TO TL-CO                         ECS031
00832                                     P-DATA.                       ECS031
00833      MOVE S-T-CNT                TO TL-CNT.                       ECS031
00834      MOVE S-T-DTH                TO TL-DTH.                       ECS031
00835      MOVE S-T-DIS                TO TL-DIS.                       ECS031
00836      MOVE TL-LN                  TO P-DATA.                       ECS031
00837                                                                   ECS031
00838      MOVE ' '                    TO X.                            ECS031
00839      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00840                                                                   ECS031
00841      MOVE SPACE                  TO P-DATA.                       ECS031
00842      MOVE ' '                    TO X.                            ECS031
00843      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00844                                                                   ECS031
00845      ADD 3                       TO LN-CT.                        ECS031
00846      MOVE ZERO                   TO S-T-CNT  S-T-DTH  S-T-DIS.    ECS031
00847      MOVE DE-STATE               TO PREV-STATE.                   ECS031
00848                                                                   ECS031
00849  0360-EXIT.                                                       ECS031
00850      EXIT.                                                        ECS031
00851                                                                   ECS031
00852  0370-END-STEP.                                                   ECS031
00853      IF CLM-CTR = ZERO                                            ECS031
00854          PERFORM 0280-NO-INPUT                                    ECS031
00855          MOVE ALL '9'            TO DETAIL-EXTRACT                ECS031
00856          PERFORM 0230-WRITE-CAR THRU 0240-EXIT                    ECS031
00857          GO TO 0380-STDE-ONE-X.                                   ECS031
00858                                                                   ECS031
00859      PERFORM 0190-PRT-TOTALS.                                     ECS031
00860      PERFORM 0330-G-TOTALS.                                       ECS031
00861                                                                   ECS031
00862      MOVE ZERO                   TO PG-NO.                        ECS031
00863                                                                   ECS031
00864  0380-STDE-ONE-X.                                                 ECS031
00865      EXIT.                                                        ECS031
00866                                                                   ECS031
00867      EJECT                                                        ECS031
00868  0390-STDE-TWO  SECTION.                                          ECS031
00869      IF CLM-CTR = ZERO                                            ECS031
00870          GO TO 0800-CLOSE-ALL.                                    ECS031
00871                                                                   ECS031
00872      OPEN INPUT CLM-CARR.                                         ECS031
00873      MOVE '9'                    TO CAR-SW.                       ECS031
00874      PERFORM 0250-PRNT-CARR THRU 0270-EXIT.                       ECS031
00875      PERFORM 0190-PRT-TOTALS.                                     ECS031
00876      PERFORM 0330-G-TOTALS.                                       ECS031
00877      GO TO 0800-CLOSE-ALL.                                        ECS031
00878      EJECT                                                        ECS031
00879  0400-PRODUCE-REPORT SECTION.                                     ECS031
00880      IF CLM-CTR = ZERO                                            ECS031
00881          GO TO 0800-CLOSE-ALL.                                    ECS031
00882      OPEN INPUT CLM-CARR.                                         ECS031
00883      PERFORM 0710-RD-DETAIL.                                      ECS031
00884      PERFORM 0405-SET-HEADING.                                    ECS031
00885      GO TO 0420-CHECK-BREAKS.                                     ECS031
00886                                                                   ECS031
00887  0405-SET-HEADING.                                                ECS031
00888      MOVE 'B'                     TO HD1A-RPT.                    ECS031
00889      MOVE ' : '                   TO HD1A-FILL.                   ECS031
00890      MOVE ' GROUP -'              TO HD1A-COMP.                   ECS031
00891      MOVE CLAS-REPORT-CD1-CAPTION TO HD1A-CAPT.                   ECS031
00892      MOVE DE-CARRIER              TO HD1A-CARR.                   ECS031
00893      MOVE DE-GROUPING             TO HD1A-GROUP.                  ECS031
00894      MOVE COMPANY-NAME            TO HD2-CO.                      ECS031
00895      MOVE DE-REPORT-CODE-1        TO SAV-RPT-CD-1                 ECS031
00896                                      HD1A-RPT-CD.                 ECS031
00897      PERFORM 0210-PT-HDNG.                                        ECS031
00898                                                                   ECS031
00899  0410-GET-DETAIL.                                                 ECS031
00900      PERFORM 0710-RD-DETAIL THRU 0710-EXIT.                       ECS031
00901      IF DET-END = 'END'                                           ECS031
00902          PERFORM 0500-TOTALS THRU 0560-EXIT                       ECS031
00903          PERFORM 0570-FINAL-TOTAL THRU 0599-EXIT                  ECS031
00904          GO TO 0800-CLOSE-ALL.                                    ECS031
00905                                                                   ECS031
00906  0420-CHECK-BREAKS.                                               ECS031
00907      IF DE-REPORT-CODE-1 =  SAV-RPT-CD-1                          ECS031
00908          NEXT SENTENCE                                            ECS031
00909      ELSE                                                         ECS031
00910          PERFORM 0500-TOTALS THRU 0560-EXIT.                      ECS031
00911                                                                   ECS031
00912      IF SAV-CARRIER = SPACES                                      ECS031
00913         MOVE DE-CARRIER          TO SAV-CARRIER                   ECS031
00914         MOVE DE-GROUPING         TO SAV-GROUP                     ECS031
00915         MOVE DE-STATE            TO SAV-STATE                     ECS031
00916         MOVE DE-REPORT-CODE-2    TO SAV-RPT-CD-2                  ECS031
00917         MOVE DE-ACCOUNT          TO SAV-ACCOUNT.                  ECS031
00918                                                                   ECS031
00919      IF DE-REPORT-CODE-1 NOT = SAV-RPT-CD-1                       ECS031
00920          PERFORM 0510-ACCT-BREAK THRU 0560-EXIT.                  ECS031
00921                                                                   ECS031
00922      IF DE-CARRIER NOT = SAV-CARRIER                              ECS031
00923          PERFORM 0510-ACCT-BREAK THRU 0550-EXIT.                  ECS031
00924                                                                   ECS031
00925      IF DE-GROUPING NOT = SAV-GROUP                               ECS031
00926          PERFORM 0510-ACCT-BREAK THRU 0540-EXIT.                  ECS031
00927                                                                   ECS031
00928      IF DE-REPORT-CODE-2 NOT = SAV-RPT-CD-2                       ECS031
00929          PERFORM 0510-ACCT-BREAK THRU 0530-EXIT.                  ECS031
00930                                                                   ECS031
00931      IF DE-STATE NOT = SAV-STATE                                  ECS031
00932          PERFORM 0510-ACCT-BREAK THRU 0520-EXIT.                  ECS031
00933                                                                   ECS031
00934      IF DE-ACCOUNT NOT = SAV-ACCOUNT                              ECS031
00935          PERFORM 0510-ACCT-BREAK THRU 0510-EXIT.                  ECS031
00936                                                                   ECS031
00937  0430-PRT-DETAIL.                                                 ECS031
00938      PERFORM 0150-CONT-RPT THRU 0180-PT-LST                       ECS031
00939      IF LN-CT GREATER 52                                          ECS031
00940          PERFORM 0210-PT-HDNG.                                    ECS031
00941                                                                   ECS031
00942      GO TO 0410-GET-DETAIL.                                       ECS031
00943                                                                   ECS031
00944  0499-EXIT.                                                       ECS031
00945      EXIT.                                                        ECS031
00946                                                                   ECS031
00947  0500-TOTALS SECTION.                                             ECS031
00948                                                                   ECS031
00949  0510-ACCT-BREAK.                                                 ECS031
00950      PERFORM 0350-ACCT-TOT THRU 0350-EXIT.                        ECS031
00951      MOVE DE-ACCOUNT             TO SAV-ACCOUNT.                  ECS031
00952  0510-EXIT.                                                       ECS031
00953      EXIT.                                                        ECS031
00954                                                                   ECS031
00955  0520-STATE-BREAK.                                                ECS031
00956      COMPUTE RPT-CD2-CNT = RPT-CD2-CNT + S-T-CNT.                 ECS031
00957      COMPUTE RPT-CD2-DTH = RPT-CD2-DTH + S-T-DTH.                 ECS031
00958      COMPUTE RPT-CD2-DIS = RPT-CD2-DIS + S-T-DIS.                 ECS031
00959      PERFORM 0360-STATE-TOT THRU 0360-EXIT.                       ECS031
00960      MOVE DE-STATE               TO SAV-STATE.                    ECS031
00961  0520-EXIT.                                                       ECS031
00962      EXIT.                                                        ECS031
00963                                                                   ECS031
00964  0530-RPT-CD2-BREAK.                                              ECS031
00965      COMPUTE GRP-CNT = GRP-CNT + RPT-CD2-CNT.                     ECS031
00966      COMPUTE GRP-DTH = GRP-DTH + RPT-CD2-DTH.                     ECS031
00967      COMPUTE GRP-DIS = GRP-DIS + RPT-CD2-DIS.                     ECS031
00968      MOVE 'TOTAL FOR  '          TO TL-CON.                       ECS031
00969      MOVE CLAS-REPORT-CD2-CAPTION TO TL-CAP.                      ECS031
00970      MOVE SPACE                  TO TL-FILL.                      ECS031
00971      MOVE TL-CONSTANT            TO TLC.                          ECS031
00972      MOVE SAV-RPT-CD-2           TO TL-CO.                        ECS031
00973      MOVE SPACE                  TO P-DATA.                       ECS031
00974      MOVE RPT-CD2-CNT            TO TL-CNT.                       ECS031
00975      MOVE RPT-CD2-DTH            TO TL-DTH.                       ECS031
00976      MOVE RPT-CD2-DIS            TO TL-DIS.                       ECS031
00977      MOVE TL-LN                  TO P-DATA.                       ECS031
00978      MOVE ' '                    TO X.                            ECS031
00979      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00980      MOVE SPACE                  TO P-DATA.                       ECS031
00981      MOVE ' '                    TO X.                            ECS031
00982      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
00983      ADD 3 TO LN-CT.                                              ECS031
00984      MOVE ZERO                   TO RPT-CD2-CNT RPT-CD2-DTH       ECS031
00985                                     RPT-CD2-DIS.                  ECS031
00986      MOVE DE-REPORT-CODE-2 TO SAV-RPT-CD-2.                       ECS031
00987  0530-EXIT.                                                       ECS031
00988      EXIT.                                                        ECS031
00989                                                                   ECS031
00990  0540-GROUP-BREAK.                                                ECS031
00991      COMPUTE CAR-CNT = CAR-CNT + GRP-CNT.                         ECS031
00992      COMPUTE CAR-DTH = CAR-DTH + GRP-DTH.                         ECS031
00993      COMPUTE CAR-DIS = CAR-DIS + GRP-DIS.                         ECS031
00994      MOVE 'TOTAL FOR GROUP'      TO TLC.                          ECS031
00995      MOVE SPACE                  TO TL-CO                         ECS031
00996                                     P-DATA.                       ECS031
00997      MOVE GRP-CNT                TO TL-CNT.                       ECS031
00998      MOVE GRP-DTH                TO TL-DTH.                       ECS031
00999      MOVE GRP-DIS                TO TL-DIS.                       ECS031
01000      MOVE TL-LN                  TO P-DATA.                       ECS031
01001      MOVE ' '                    TO X.                            ECS031
01002      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01003      MOVE SPACE                  TO P-DATA.                       ECS031
01004      MOVE ' '                    TO X.                            ECS031
01005      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01006      ADD 3 TO LN-CT.                                              ECS031
01007      MOVE ZERO                   TO GRP-CNT GRP-DTH               ECS031
01008                                     GRP-DIS.                      ECS031
01009      MOVE DE-GROUPING            TO SAV-GROUP                     ECS031
01010                                     HD1A-GROUP.                   ECS031
01011  0540-EXIT.                                                       ECS031
01012      EXIT.                                                        ECS031
01013                                                                   ECS031
01014  0550-CARRIER-BREAK.                                              ECS031
01015      COMPUTE RPT-CD1-CNT = RPT-CD1-CNT + CAR-CNT.                 ECS031
01016      COMPUTE RPT-CD1-DTH = RPT-CD1-DTH + CAR-DTH.                 ECS031
01017      COMPUTE RPT-CD1-DIS = RPT-CD1-DIS + CAR-DIS.                 ECS031
01018      MOVE 'TOTAL FOR CARRIER'     TO TLC.                         ECS031
01019      MOVE SPACE                   TO P-DATA                       ECS031
01020                                      TL-CO.                       ECS031
01021      MOVE CAR-CNT                 TO TL-CNT.                      ECS031
01022      MOVE CAR-DTH                 TO TL-DTH.                      ECS031
01023      MOVE CAR-DIS                 TO TL-DIS.                      ECS031
01024      MOVE TL-LN                   TO P-DATA.                      ECS031
01025      MOVE ' '                     TO X.                           ECS031
01026      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01027      MOVE SPACE                   TO P-DATA.                      ECS031
01028      MOVE ' '                     TO X.                           ECS031
01029      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01030      ADD 3 TO LN-CT.                                              ECS031
01031      MOVE ZERO                    TO CAR-CNT CAR-DTH              ECS031
01032                                     CAR-DIS.                      ECS031
01033      IF DE-REPORT-CODE-1 = SAV-RPT-CD-1                           ECS031
01034          MOVE DE-CARRIER          TO HD1A-CARR                    ECS031
01035          PERFORM 0210-PT-HDNG                                     ECS031
01036      ELSE                                                         ECS031
01037          MOVE SPACE               TO HD1A-CARR                    ECS031
01038                                      HD1A-COMP                    ECS031
01039                                      HD1A-GROUP                   ECS031
01040          PERFORM 0210-PT-HDNG.                                    ECS031
01041      MOVE DE-CARRIER              TO HD1A-CARR                    ECS031
01042                                      SAV-CARRIER                  ECS031
01043      MOVE ' GROUP -'              TO HD1A-COMP                    ECS031
01044      MOVE SAV-GROUP               TO HD1A-GROUP.                  ECS031
01045  0550-EXIT.                                                       ECS031
01046      EXIT.                                                        ECS031
01047                                                                   ECS031
01048  0560-REPORT-CODE1-BREAK.                                         ECS031
01049      COMPUTE FIN-CNT = FIN-CNT + RPT-CD1-CNT.                     ECS031
01050      COMPUTE FIN-DTH = FIN-DTH + RPT-CD1-DTH.                     ECS031
01051      COMPUTE FIN-DIS = FIN-DIS + RPT-CD1-DIS.                     ECS031
01052      MOVE 'TOTALS FOR  '          TO TL-CON.                      ECS031
01053      MOVE CLAS-REPORT-CD1-CAPTION TO TL-CAP.                      ECS031
01054      MOVE SPACE                   TO TL-CO                        ECS031
01055                                      TL-FILL.                     ECS031
01056      MOVE TL-CONSTANT             TO TLC.                         ECS031
01057      MOVE SPACE                   TO P-DATA.                      ECS031
01058      MOVE RPT-CD1-CNT             TO TL-CNT.                      ECS031
01059      MOVE RPT-CD1-DTH             TO TL-DTH.                      ECS031
01060      MOVE RPT-CD1-DIS             TO TL-DIS.                      ECS031
01061      MOVE TL-LN                   TO P-DATA.                      ECS031
01062      MOVE ' '                     TO X.                           ECS031
01063      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01064      MOVE SPACE                   TO P-DATA.                      ECS031
01065      MOVE ' '                     TO X.                           ECS031
01066      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01067      ADD 3 TO LN-CT.                                              ECS031
01068      MOVE ZERO                    TO RPT-CD1-CNT RPT-CD1-DTH      ECS031
01069                                      RPT-CD1-DIS.                 ECS031
01070      MOVE DE-REPORT-CODE-1        TO SAV-RPT-CD-1                 ECS031
01071                                      HD1A-RPT-CD.                 ECS031
01072      MOVE CLAS-REPORT-CD1-CAPTION TO HD1A-CAPT.                   ECS031
01073      IF DET-END = 'END'                                           ECS031
01074          NEXT SENTENCE                                            ECS031
01075      ELSE                                                         ECS031
01076          PERFORM 0210-PT-HDNG.                                    ECS031
01077  0560-EXIT.                                                       ECS031
01078      EXIT.                                                        ECS031
01079                                                                   ECS031
01080  0570-FINAL-TOTAL.                                                ECS031
01081      MOVE SPACE                   TO HD1B-CARR                    ECS031
01082      MOVE HD1B                    TO HD1A.                        ECS031
01083      PERFORM 0210-PT-HDNG.                                        ECS031
01084      MOVE 'OVERALL TOTALS   '     TO TLC.                         ECS031
01085      MOVE SPACE                   TO TL-CO                        ECS031
01086                                      P-DATA.                      ECS031
01087      MOVE FIN-CNT                 TO TL-CNT.                      ECS031
01088      MOVE FIN-DTH                 TO TL-DTH.                      ECS031
01089      MOVE FIN-DIS                 TO TL-DIS.                      ECS031
01090      MOVE TL-LN                   TO P-DATA.                      ECS031
01091      MOVE ' '                     TO X.                           ECS031
01092      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01093      MOVE SPACE                   TO P-DATA.                      ECS031
01094      MOVE ' '                     TO X.                           ECS031
01095      PERFORM 0300-PRT-RTN THRU 0310-EXIT.                         ECS031
01096      ADD 3 TO LN-CT.                                              ECS031
01097  0570-EXIT.                                                       ECS031
01098      EXIT.                                                        ECS031
01099                                                                   ECS031
01100  0599-EXIT.                                                       ECS031
01101      EXIT.                                                        ECS031
01102                                                                   ECS031
01103  0700-READ-SECTION.                                               ECS031
01104                                                                   ECS031
01105  0710-RD-DETAIL.                                                  ECS031
01106                                                                   ECS031
01107      IF DET-END = 'END'                                           ECS031
01108          GO TO 0710-EXIT.                                         ECS031
01109                                                                   ECS031
01110      READ CLM-CARR INTO DETAIL-EXTRACT                            ECS031
01111          AT END  MOVE 'END' TO DET-END.                           ECS031
01112                                                                   ECS031
01113  0710-EXIT.                                                       ECS031
01114      EXIT.                                                        ECS031
01115                                                                   ECS031
01116  0720-RD-ACCT-MSTR.                                               ECS031
01117      IF ERACCT-FILE-STATUS = '10'                                 ECS031
01118         GO TO 0720-EXIT.                                          ECS031
01119                                                                   ECS031
01120      IF DE-CNTRL1 = AM-CONTROL-A                                  ECS031
01121           MOVE AM-REPORT-CODE-1 TO SAV-RPT-CD-1                   ECS031
01122           MOVE AM-REPORT-CODE-2 TO SAV-RPT-CD-2                   ECS031
01123           MOVE DE-CNTRL1        TO SAV-DE-CNTRL.                  ECS031
01124                                                                   ECS031
01125      READ ACC-MSTR.                                               ECS031
01126                                                                   ECS031
01127      IF ERACCT-FILE-STATUS = '10'                                 ECS031
01128         GO TO 0720-EXIT.                                          ECS031
01129                                                                   ECS031
01130      IF ERACCT-FILE-STATUS NOT EQUAL ZERO                         ECS031
01131          MOVE 'ERROR OCCURED READ - ERACCTT' TO WS-ABEND-MESSAGE  ECS031
01132          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS031
01133          PERFORM ABEND-PGM.                                       ECS031
01134                                                                   ECS031
01135       IF DE-CNTRL1 < AM-CONTROL-A                                 ECS031
01136           GO TO 0720-EXIT.                                        ECS031
01137                                                                   ECS031
01138       GO TO 0720-RD-ACCT-MSTR.                                    ECS031
01139                                                                   ECS031
01140  0720-EXIT.                                                       ECS031
01141      EXIT.                                                        ECS031
01142                                                                   ECS031
01143  0800-CLOSE-ALL.                                                  ECS031
01144                              COPY ELCPRTC.                        ECS031
01145                                                                   ECS031
01146      CLOSE PRNTR  EXTRACT  ACC-MSTR.                              ECS031
01147                                                                   ECS031
01148      IF ERACCT-FILE-STATUS = ZERO                                 ECS031
01149          NEXT SENTENCE                                            ECS031
01150      ELSE                                                         ECS031
01151          MOVE 'ERROR OCCURED CLOSE- ERACCTT' TO WS-ABEND-MESSAGE  ECS031
01152          MOVE ERACCT-FILE-STATUS TO WS-ABEND-FILE-STATUS          ECS031
01153          PERFORM ABEND-PGM.                                       ECS031
01154                                                                   ECS031
01155      GOBACK.                                                      ECS031
01156                                                                   ECS031
01157  ABEND-PGM SECTION.                                               ECS031
01158                              COPY ELCABEND.                       ECS031
01159                                                                      CL*14
