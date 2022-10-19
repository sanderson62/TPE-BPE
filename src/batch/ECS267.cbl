00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   ECS267
00003  PROGRAM-ID.                 ECS267.                                 LV006
00004 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             ECS267
00005 *                            VMOD=2.005.                             CL**2
00006 *                                                                 ECS267
00007 *AUTHOR.        LOGIC, INC.                                       ECS267
00008 *               DALLAS, TEXAS.                                    ECS267
00009 *                                                                 ECS267
00010 *DATE-COMPILED.                                                   ECS267
00011 *                                                                 ECS267
00012 *SECURITY.   *****************************************************ECS267
00013 *            *                                                   *ECS267
00014 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS267
00015 *            *                                                   *ECS267
00016 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS267
00017 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS267
00018 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *ECS267
00019 *            *                                                   *ECS267
00020 *            *****************************************************ECS267
00021 *                                                                 ECS267
00022 *REMARKS.                                                         ECS267
00023 *        GENERATES YTD COMPENSATION FOR ACCOUNTS.                 ECS267
00024                                                                   ECS267
052814******************************************************************
052814*                   C H A N G E   L O G
052814*
052814* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
052814*-----------------------------------------------------------------
052814*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
052814* EFFECTIVE    NUMBER
052814*-----------------------------------------------------------------
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
052814******************************************************************
00025  ENVIRONMENT DIVISION.                                            ECS267
00026  INPUT-OUTPUT SECTION.                                            ECS267
00027  FILE-CONTROL.                                                    ECS267
00028                                                                   ECS267
00029      SELECT  SORT-FILE   ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.      ECS267
00030      SELECT  PRNTR       ASSIGN TO SYS008-UR-1403-S-SYS008.       ECS267
00031      SELECT  CERT-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.       ECS267
00032      SELECT  ACCT-IN     ASSIGN TO SYS011-UT-2400-S-SYS011.       ECS267
00033      SELECT  COMM-OUT    ASSIGN TO SYS012-UT-2400-S-SYS012.       ECS267
00034      SELECT  COMM-IN     ASSIGN TO SYS013-UT-2400-S-SYS013.       ECS267
00035      SELECT  DISK-DATE   ASSIGN TO SYS019-UT-FBA1-S-SYS019.       ECS267
00036      SELECT  FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.       ECS267
00037  EJECT                                                            ECS267
00038  DATA DIVISION.                                                   ECS267
00039  FILE SECTION.                                                    ECS267
00040                                                                   ECS267
00041  SD  SORT-FILE                                                    ECS267
00042      RECORDING MODE F                                             ECS267
00043      RECORD CONTAINS 700 CHARACTERS.                              ECS267
00044                                                                   ECS267
00045  01  SORT-REC.                                                    ECS267
00046      12  FILLER              PIC  XX.                             ECS267
00047      12  SORT-PARM.                                               ECS267
00048          16  S-SEQ.                                               ECS267
00049              20  S-COMP-CD   PIC  X.                              ECS267
00050              20  S-CARR      PIC  X.                              ECS267
00051              20  S-COMP      PIC  X(6).                           ECS267
00052              20  S-RESP      PIC  X(10).                          ECS267
00053              20  S-ACCT      PIC  X(10).                          ECS267
00054              20  S-TYPE      PIC  X.                              ECS267
00055      12  FILLER              PIC  X(419).                         ECS267
00056      12  FILLER              PIC  X(250).                         ECS267
00057  EJECT                                                            ECS267
00058  FD  PRNTR                                                        ECS267
00059                              COPY ELCPRTFD.                       ECS267
00060  EJECT                                                            ECS267
00061  FD  CERT-IN                                                      ECS267
00062                              COPY ECSCRIFD.                       ECS267
00063  EJECT                                                            ECS267
00064  FD  ACCT-IN                                                      ECS267
00065      RECORDING MODE F                                             ECS267
00066      LABEL RECORDS STANDARD                                       ECS267
00067      BLOCK CONTAINS 0 RECORDS
00068      RECORD CONTAINS 2000 CHARACTERS.                             ECS267
00069                                                                   ECS267
00070  01  ACCT-REC-IN             PIC  X(2000).                        ECS267
00071  EJECT                                                            ECS267
00072  FD  COMM-OUT                                                     ECS267
00073                              COPY ECSCOOFD.                       ECS267
00074  EJECT                                                            ECS267
00075  FD  COMM-IN                                                      ECS267
00076                              COPY ECSCOIFD.                       ECS267
00077  EJECT                                                            ECS267
00078  FD  DISK-DATE                                                    ECS267
00079                              COPY ELCDTEFD.                       ECS267
00080  EJECT                                                            ECS267
00081  FD  FICH                                                         ECS267
00082                              COPY ELCFCHFD.                       ECS267
00083  EJECT                                                            ECS267
00084  WORKING-STORAGE SECTION.                                         ECS267
00085  77  FILLER PIC  X(32) VALUE '********************************'.  ECS267
00086  77  FILLER PIC  X(32) VALUE '*           ECS267             *'.  ECS267
00087  77  FILLER PIC  X(32) VALUE '*********** VMOD=2.005 *********'.     CL**2
00088                                                                   ECS267
00089  77  SUB                     PIC S9(3)   COMP    VALUE +0.        ECS267
00090  77  AGT                     PIC S9(3)   COMP    VALUE +0.        ECS267
00091  77  PGM-SUB                 PIC S9(3)   COMP    VALUE +082.      ECS267
00092  77  LNCTR                   PIC S9(3)   COMP-3  VALUE +066.      ECS267
00093  77  PGCTR                   PIC S9(5)   COMP-3  VALUE +0.        ECS267
00094  77  X                       PIC  X              VALUE ' '.          CL**5
00095  77  SPACE-NP                PIC  X              VALUE '1'.       ECS267
00096  77  SPACE-1                 PIC  X              VALUE ' '.       ECS267
00097  77  SPACE-2                 PIC  X              VALUE '0'.       ECS267
00098  77  SPACE-3                 PIC  X              VALUE '-'.       ECS267
00099  77  REMITX                  PIC  99             VALUE ZERO.      ECS267
00100                                                                   ECS267
00101  01  AGENTS-TABLE.                                                ECS267
00102      12  AGENT-DATA      OCCURS 300 TIMES.                        ECS267
00103          16  AGT-CARR        PIC  X.                              ECS267
00104          16  AGT-COMP        PIC  X(6).                           ECS267
00105          16  AGT-NO          PIC  X(10).                          ECS267
00106          16  AGT-AMT         PIC S9(7)V99 COMP-3.                 ECS267
00107                                                                   ECS267
00108  01  MISC-WS.                                                     ECS267
00109      12  DATE-AREA.                                               ECS267
00110          16  BEG-DATE.                                            ECS267
00111              20  BEG-CC      PIC  99.                             ECS267
00112              20  BEG-YR      PIC  99.                             ECS267
00113              20  BEG-MO      PIC  99.                             ECS267
00114          16  END-DATE.                                            ECS267
00115              20  END-CC      PIC  99.                             ECS267
00116              20  END-YR      PIC  99.                             ECS267
00117              20  END-MO      PIC  99.                             ECS267
00118          16  TEST-DATE.                                           ECS267
00119              20  TEST-CC     PIC  99.                             ECS267
00120              20  TEST-YR     PIC  99.                             ECS267
00121              20  TEST-MO     PIC  99.                             ECS267
00122      12  WS-RETURN-CODE      PIC S9(4)       COMP.                ECS267
00123      12  WS-ABEND-MESSAGE    PIC  X(80).                          ECS267
00124      12  WS-ABEND-FILE-STATUS                                     ECS267
00125                              PIC  XX                VALUE ZEROS.  ECS267
00126      12  WS-ZERO             PIC S9          COMP-3 VALUE +0.     ECS267
00127      12  FINAL-TOTALS.                                            ECS267
00128          16  TOTAL-COMM      PIC S9(9)V99 COMP-3 VALUE +0.        ECS267
00129          16  SAVE-COMM       PIC S9(9)V99 COMP-3 VALUE +0.        ECS267
00130      12  SAVE-SOC-SEC        PIC  X(13)          VALUE SPACES.    ECS267
00131  EJECT                                                            ECS267
00132  01  SAVE-RECORD.                                                 ECS267
00133      12  SAV-RECORD-ID                         PIC  XX.           ECS267
00134          88  SAV-VALID-CO-ID                      VALUE 'CO'.     ECS267
00135      12  SAV-CONTROL-PRIMARY.                                     ECS267
00136          16  SAV-COMPANY-CD                    PIC  X.            ECS267
00137          16  SAV-CONTROL.                                         ECS267
00138              20  SAV-CTL-1.                                       ECS267
00139                  24  SAV-CARR-GROUP.                              ECS267
00140                      28  SAV-CARRIER           PIC  X.            ECS267
00141                      28  SAV-GROUPING.                            ECS267
00142                          32  SAV-GROUP-PREFIX  PIC  X(3).         ECS267
00143                          32  SAV-GROUP-PRIME   PIC  X(3).         ECS267
00144                  24  SAV-RESP-NO.                                 ECS267
00145                      28  SAV-RESP-PREFIX       PIC  X(4).         ECS267
00146                      28  SAV-RESP-PRIME        PIC  X(6).         ECS267
00147              20  SAV-CTL-2.                                       ECS267
00148                  24  SAV-ACCOUNT.                                 ECS267
00149                      28  SAV-ACCT-PREFIX       PIC  X(4).         ECS267
00150                      28  SAV-ACCT-PRIME        PIC  X(6).         ECS267
00151          16  SAV-TYPE                          PIC  X.            ECS267
00152              88  SAV-COMPANY-TYPE                 VALUE 'C'.      ECS267
00153              88  SAV-GEN-AGENT-TYPE               VALUE 'G'.      ECS267
00154              88  SAV-ACCOUNT-TYPE                 VALUE 'A'.      ECS267
00155      12  SAV-MAINT-INFORMATION.                                   ECS267
00156          16  SAV-LAST-MAINT-DT                 PIC  XX.           ECS267
00157          16  SAV-LAST-MAINT-HHMMSS             PIC S9(7) COMP-3.  ECS267
00158          16  SAV-LAST-MAINT-USER               PIC  X(4).         ECS267
00159          16  FILLER                            PIC  X(10).        ECS267
00160      12  SAV-BALANCE-CONTROL                   PIC  X.            ECS267
00161          88  SAV-CARRY-BALANCE                    VALUE 'Y'.      ECS267
00162          88  SAV-NO-BALANCE                       VALUE 'N'.      ECS267
00163      12  SAV-INTERNAL-CONTROL-1                PIC  X.            ECS267
00164          88  SAV-AUTO-GENERATED-THIS-RUN          VALUE 'X'.      ECS267
00165          88  SAV-AUTO-GENERATED                   VALUE 'Y'.      ECS267
00166          88  SAV-NOT-AUTO-GENERATED               VALUE 'N'.      ECS267
00167      12  SAV-INTERNAL-CONTROL-2                PIC  X.            ECS267
00168          88  SAV-STATEMENT-THIS-RUN               VALUE 'Y'.      ECS267
00169          88  SAV-NO-STATEMENT-THIS-RUN            VALUE 'N'.      ECS267
00170      12  SAV-FUTURE-SPACE                      PIC  X(5).         ECS267
00171          88  SAV-FUTURE-NOT-USED                  VALUE '     '.  ECS267
00172      12  SAV-ACCT-NAME                         PIC  X(30).        ECS267
00173      12  SAV-MAIL-NAME                         PIC  X(30).        ECS267
00174      12  SAV-ADDR-1                            PIC  X(30).        ECS267
00175      12  SAV-ADDR-2                            PIC  X(30).        ECS267
00176      12  SAV-ADDR-3                            PIC  X(30).        ECS267
00177      12  SAV-ZIP.                                                 ECS267
00178          16  SAV-ZIP-PRIME                     PIC  X(5).         ECS267
00179          16  SAV-ZIP-PLUS4                     PIC  X(4).         ECS267
00180      12  SAV-SOC-SEC                           PIC  X(13).        ECS267
00181      12  SAV-TELEPHONE.                                           ECS267
00182          16  SAV-AREA-CODE                     PIC  X(3).         ECS267
00183          16  SAV-PREFIX                        PIC  X(3).         ECS267
00184          16  SAV-PHONE                         PIC  X(4).         ECS267
00185      12  FILLER                                PIC  X(16).        ECS267
00186      12  SAV-USER-CODE                         PIC  X.            ECS267
00187      12  SAV-USER-FUTURE                       PIC  X(12).        ECS267
00188      12  SAV-LAST-ACTIVITY-DATE.                                  ECS267
00189          16  SAV-ACT-YEAR                      PIC  99.           ECS267
00190          16  SAV-ACT-MONTH                     PIC  99.           ECS267
00191          16  SAV-ACT-DAY                       PIC  99.           ECS267
00192      12  SAV-LAST-STMT-DT.                                        ECS267
00193          16  SAV-LAST-STMT-YEAR                PIC  99.           ECS267
00194          16  SAV-LAST-STMT-MONTH               PIC  99.           ECS267
00195          16  SAV-LAST-STMT-DAY                 PIC  99.           ECS267
00196      12  SAV-MO-END-TOTALS.                                       ECS267
00197          16  SAV-MONTHLY-TOTALS.                                  ECS267
00198              20  SAV-BAL-FWD             PIC S9(7)V99 COMP-3.     ECS267
00199              20  SAV-CUR-COM             PIC S9(7)V99 COMP-3.     ECS267
00200              20  SAV-CUR-CHG             PIC S9(7)V99 COMP-3.     ECS267
00201              20  SAV-CUR-PMT             PIC S9(7)V99 COMP-3.     ECS267
00202              20  SAV-END-BAL             PIC S9(7)V99 COMP-3.     ECS267
00203          16  SAV-AGING-TOTALS.                                    ECS267
00204              20  SAV-CUR                 PIC S9(7)V99 COMP-3.     ECS267
00205              20  SAV-OV30                PIC S9(7)V99 COMP-3.     ECS267
00206              20  SAV-OV60                PIC S9(7)V99 COMP-3.     ECS267
00207              20  SAV-OV90                PIC S9(7)V99 COMP-3.     ECS267
00208          16  SAV-YTD-TOTALS.                                      ECS267
00209              20  SAV-YTD-COM             PIC S9(7)V99 COMP-3.     ECS267
00210              20  SAV-YTD-OV              PIC S9(7)V99 COMP-3.     ECS267
00211          16  SAV-OVER-UNDER-TOTALS.                               ECS267
00212              20  SAV-CUR-OVR-UNDR        PIC S9(7)V99 COMP-3.     ECS267
00213              20  SAV-YTD-OVR-UNDR        PIC S9(7)V99 COMP-3.     ECS267
00214      12  SAV-MISCELLANEOUS-TOTALS.                                ECS267
00215          16  SAV-FICA-TOTALS.                                     ECS267
00216              20  SAV-CUR-FICA            PIC S9(7)V99 COMP-3.     ECS267
00217              20  SAV-YTD-FICA            PIC S9(7)V99 COMP-3.     ECS267
00218          16  SAV-CLAIM-TOTALS.                                    ECS267
00219              20  SAV-LF-CLM-AMT          PIC S9(9)V99 COMP-3.     ECS267
00220              20  SAV-AH-CLM-AMT          PIC S9(9)V99 COMP-3.     ECS267
00221      12  SAV-CURRENT-TOTALS.                                      ECS267
00222          16  SAV-CURRENT-LAST-STMT-DT.                            ECS267
00223              20  SAV-CURRENT-LAST-STMT-YEAR    PIC  99.           ECS267
00224              20  SAV-CURRENT-LAST-STMT-MONTH   PIC  99.           ECS267
00225              20  SAV-CURRENT-LAST-STMT-DAY     PIC  99.           ECS267
00226          16  SAV-CURRENT-MONTHLY-TOTALS.                          ECS267
00227              20  SAV-CURRENT-BAL-FWD     PIC S9(7)V99 COMP-3.     ECS267
00228              20  SAV-CURRENT-CUR-COM     PIC S9(7)V99 COMP-3.     ECS267
00229              20  SAV-CURRENT-CUR-CHG     PIC S9(7)V99 COMP-3.     ECS267
00230              20  SAV-CURRENT-CUR-PMT     PIC S9(7)V99 COMP-3.     ECS267
00231              20  SAV-CURRENT-END-BAL     PIC S9(7)V99 COMP-3.     ECS267
00232          16  SAV-CURRENT-AGING-TOTALS.                            ECS267
00233              20  SAV-CURRENT-CUR         PIC S9(7)V99 COMP-3.     ECS267
00234              20  SAV-CURRENT-OV30        PIC S9(7)V99 COMP-3.     ECS267
00235              20  SAV-CURRENT-OV60        PIC S9(7)V99 COMP-3.     ECS267
00236              20  SAV-CURRENT-OV90        PIC S9(7)V99 COMP-3.     ECS267
00237          16  SAV-CURRENT-YTD-TOTALS.                              ECS267
00238              20  SAV-CURRENT-YTD-COM     PIC S9(7)V99 COMP-3.     ECS267
00239              20  SAV-CURRENT-YTD-OV      PIC S9(7)V99 COMP-3.     ECS267
00240      12  FILLER                            PIC  X(20).            ECS267
00241  EJECT                                                            ECS267
00242  01  HD1.                                                         ECS267
00243      12  FILLER              PIC  X(40)      VALUE SPACES.        ECS267
00244      12  FILLER              PIC  X(48)      VALUE                ECS267
00245              'NET CREDIT COMPENSATION FOR PREMIUM WRITTEN - 19'.  ECS267
00246      12  HD1-YR              PIC  XX         VALUE '84'.          ECS267
00247      12  FILLER              PIC  X(35)      VALUE SPACES.        ECS267
00248      12  FILLER              PIC  X(7)       VALUE 'ECS-267'.     ECS267
00249                                                                   ECS267
00250  01  HD2.                                                         ECS267
00251      12  HD-TAX-ID           PIC  X(52)      VALUE SPACES.        ECS267
00252      12  H2-COMP             PIC  X(30).                          ECS267
00253      12  FILLER              PIC  X(43)      VALUE SPACES.        ECS267
00254      12  H2-DATE             PIC  X(8).                           ECS267
00255                                                                   ECS267
00256  01  HD3.                                                         ECS267
00257      12  FILLER              PIC  X(58)      VALUE SPACES.        ECS267
00258      12  H3-DATE             PIC  X(18).                          ECS267
00259      12  FILLER              PIC  X(46)      VALUE SPACES.        ECS267
00260      12  FILLER              PIC  X(4)       VALUE 'PAGE'.        ECS267
00261      12  H3-PAGE             PIC ZZ,ZZ9.                          ECS267
00262                                                                   ECS267
00263  01  HD4.                                                         ECS267
00264      12  FILLER              PIC  X(44)      VALUE                ECS267
00265              '---------- CONTROL ----------- ------------ '.      ECS267
00266      12  FILLER              PIC  X(44)      VALUE                ECS267
00267              'NAME ------------ -- TAX ID ---  COMPENSATIO'.      ECS267
00268      12  FILLER              PIC  X(44)      VALUE                ECS267
00269              'N      OVERWRITE        BALANCE             '.      ECS267
00270                                                                   ECS267
00271  01  HD5.                                                         ECS267
00272      12  FILLER              PIC  X(44)      VALUE SPACES.        ECS267
00273                                                                   ECS267
00274  01  HD-CAT.                                                      ECS267
00275      12  HD-BEG-MO           PIC  99.                             ECS267
00276      12  HD-BEG-YR           PIC  99.                             ECS267
00277      12  FILLER              PIC  X(3)     VALUE ' - '.           ECS267
00278      12  HD-END-MO           PIC  99.                             ECS267
00279      12  HD-END-YR           PIC  99.                             ECS267
00280                                                                   ECS267
00281  01  P-REC.                                                       ECS267
00282      12  P-CCSW              PIC  X.                              ECS267
00283      12  P-LINE.                                                  ECS267
00284          16  P-CARR          PIC  X(2).                           ECS267
00285          16  P-COMP          PIC  X(7).                           ECS267
00286          16  P-RESP          PIC  X(11).                          ECS267
00287          16  P-ACCT          PIC  X(11).                          ECS267
00288          16  P-NAME          PIC  X(31).                          ECS267
00289          16  P-TXID          PIC  X(14).                          ECS267
00290          16  P-COMM          PIC ZZZZ,ZZZ,ZZZ.ZZ-.                ECS267
00291          16  P-OV            PIC ZZZZ,ZZZ,ZZZ.ZZ-.                ECS267
00292          16  P-BAL           PIC ZZZZ,ZZZ,ZZZ.ZZ-.                ECS267
00293          16  FILLER          PIC  X(9).                           ECS267
00294  EJECT                                                            ECS267
00295                              COPY ECSCRT01.                       ECS267
00296                              COPY ELCCRTVR.                          CL**3
00297  EJECT                                                            ECS267
00298                              COPY ERCACCT.                        ECS267
00299  EJECT                                                            ECS267
00300                              COPY ERCCOMP.                        ECS267
00301  EJECT                                                            ECS267
00302                              COPY ELCDATE.                           CL**6
00303  EJECT                                                            ECS267
00304                                                                   ECS267
00305                              COPY ELCDTECX.                       ECS267
00306                                                                   ECS267
00307                              COPY ELCDTEVR.                       ECS267
00308  EJECT                                                            ECS267
00309  PROCEDURE DIVISION.                                              ECS267
00310                                                                   ECS267
00311  0000-STANDARD-RTNS  SECTION.                                     ECS267
00312                              COPY ELCDTERX.                       ECS267
00313                                                                   ECS267
00314      OPEN OUTPUT PRNTR.                                           ECS267
00315                                                                   ECS267
00316      MOVE SPACES                 TO  P-REC.                       ECS267
00317      MOVE ALPH-DATE              TO  H3-DATE.                     ECS267
00318      MOVE COMPANY-NAME           TO  H2-COMP.                     ECS267
00319      MOVE WS-CURRENT-DATE           TO  H2-DATE.                  ECS267
00320      MOVE CNT-NAME (6)           TO  HD-TAX-ID.                   ECS267
00321      MOVE 01                     TO  BEG-MO.                      ECS267
00322      MOVE RUN-MO                 TO  END-MO.                      ECS267
00323      MOVE RUN-YR                 TO  BEG-YR  END-YR  HD1-YR.      ECS267
00324      MOVE RUN-CC                 TO  BEG-CC  END-CC               ECS267
00325      MOVE BEG-MO                 TO  HD-BEG-MO.                   ECS267
00326      MOVE BEG-YR                 TO  HD-BEG-YR.                   ECS267
00327      MOVE END-MO                 TO  HD-END-MO.                   ECS267
00328      MOVE END-YR                 TO  HD-END-YR.                   ECS267
00329                                                                   ECS267
00330  0100-SORT-ROUTINE  SECTION.                                      ECS267
00331                                                                   ECS267
00332  0110-SORT-RTN.                                                   ECS267
00333      SORT SORT-FILE  ON ASCENDING  SORT-PARM                      ECS267
00334         INPUT PROCEDURE 0200-INPUT-RTN  THRU  0799-EXIT           ECS267
00335         OUTPUT PROCEDURE 0800-OUTPUT-RTN  THRU  1099-EXIT.        ECS267
00336                                                                   ECS267
00337      GO TO 1510-E-O-J.                                            ECS267
00338  EJECT                                                            ECS267
00339  0200-INPUT-RTN  SECTION.                                         ECS267
00340                                                                   ECS267
00341  0210-OPEN-ROUTINE.                                               ECS267
00342      PERFORM 0600-LOAD-OLD-RTN  THRU  0699-EXIT.                  ECS267
00343                                                                   ECS267
00344      MOVE +0                     TO  AGT.                         ECS267
00345                                                                   ECS267
00346  0220-ZERO-AGT.                                                   ECS267
00347      ADD +1                      TO  AGT.                         ECS267
00348                                                                   ECS267
00349      MOVE HIGH-VALUES            TO  AGT-NO   (AGT)               ECS267
00350                                      AGT-CARR (AGT)               ECS267
00351                                      AGT-COMP (AGT).              ECS267
00352      MOVE +0                     TO  AGT-AMT  (AGT).              ECS267
00353                                                                   ECS267
00354      IF AGT LESS +300                                             ECS267
00355          GO TO 0220-ZERO-AGT.                                     ECS267
00356                                                                   ECS267
00357      OPEN INPUT  CERT-IN  ACCT-IN.                                ECS267
00358                                                                   ECS267
00359      MOVE LOW-VALUE              TO  ACCOUNT-MASTER               ECS267
00360                                      COMPENSATION-MASTER.         ECS267
00361                                                                   ECS267
00362  0230-READ-CERT.                                                  ECS267
00363      READ CERT-IN  AT END                                         ECS267
00364          GO TO 0700-END-INPUT.                                    ECS267
00365                                                                   ECS267
00366      MOVE CERT-IN-RECORD         TO  CERTIFICATE-RECORD.          ECS267
00367      COPY ELCCRTM1.                                               ECS267
00368      MOVE CR-ENTRY-MO            TO  TEST-MO.                     ECS267
00369      MOVE CR-ENTRY-YR            TO  TEST-YR                      ECS267
00370      MOVE CR-ENTRY-CC            TO  TEST-CC                      ECS267
00371                                                                   ECS267
00372      IF TEST-DATE LESS BEG-DATE                                   ECS267
00373        AND  CR-LF-CANCEL-EXIT-DATE = ZEROS                        ECS267
00374        AND  CR-AH-CANCEL-EXIT-DATE = ZEROS                        ECS267
00375          GO TO 0230-READ-CERT.                                    ECS267
00376                                                                   ECS267
00377      IF TEST-DATE GREATER END-DATE                                ECS267
00378          GO TO 0230-READ-CERT.                                    ECS267
00379                                                                   ECS267
00380      IF CR-POLICY-IS-DECLINED OR                                  ECS267
00381         CR-POLICY-IS-VOID                                         ECS267
00382          GO TO 0230-READ-CERT.                                    ECS267
00383                                                                   ECS267
00384      IF CR-ENTRY-STATUS = '5' OR  '3'                             ECS267
00385        AND  CR-LF-CANCEL-EXIT-DATE = ZEROS                        ECS267
00386        AND  CR-AH-CANCEL-EXIT-DATE = ZEROS                        ECS267
00387          GO TO 0230-READ-CERT.                                    ECS267
00388                                                                   ECS267
00389  0240-COMPARE-RTN.                                                ECS267
00390      IF CR-CONTROL-1 LESS AM-MSTR-CNTRL                           ECS267
00391          GO TO 0250-HAVE-ACCOUNT-MASTER.                          ECS267
00392                                                                   ECS267
00393      PERFORM 0400-RD-ACCT-RTN  THRU  0499-EXIT.                   ECS267
00394                                                                   ECS267
00395      GO TO 0240-COMPARE-RTN.                                      ECS267
00396  EJECT                                                            ECS267
00397  0250-HAVE-ACCOUNT-MASTER.                                        ECS267
00398      IF (TEST-DATE LESS BEG-DATE OR GREATER END-DATE) OR          ECS267
00399         (CR-ENTRY-STATUS = '5' OR '3')                            ECS267
00400          MOVE +0                 TO CR-AHPRM                      ECS267
00401                                     CR-LFPRM                      ECS267
00402                                     CR-LFPRM-ALT.                 ECS267
00403                                                                   ECS267
00404      MOVE CR-LF-CEX-MO           TO  TEST-MO.                     ECS267
00405      MOVE CR-LF-CEX-YR           TO  TEST-YR                      ECS267
00406      MOVE CR-LF-CEX-CC           TO  TEST-CC                      ECS267
00407                                                                   ECS267
00408      IF TEST-DATE LESS BEG-DATE OR GREATER END-DATE               ECS267
00409          MOVE +0                 TO  CR-LFRFND.                   ECS267
00410                                                                   ECS267
00411      ADD CR-LFPRM-ALT            TO  CR-LFPRM.                    ECS267
00412                                                                   ECS267
00413      SUBTRACT CR-LFRFND          FROM  CR-LFPRM.                  ECS267
00414                                                                   ECS267
00415      MOVE CR-AH-CEX-MO           TO  TEST-MO.                     ECS267
00416      MOVE CR-AH-CEX-YR           TO  TEST-YR                      ECS267
00417      MOVE CR-AH-CEX-CC           TO  TEST-CC                      ECS267
00418                                                                   ECS267
00419      IF TEST-DATE LESS BEG-DATE OR GREATER END-DATE               ECS267
00420          MOVE +0                 TO  CR-AHRFND.                   ECS267
00421                                                                   ECS267
00422      SUBTRACT CR-AHRFND FROM CR-AHPRM.                            ECS267
00423                                                                   ECS267
00424      MOVE +0                     TO  SUB.                         ECS267
00425                                                                   ECS267
00426  0260-CALC-COMM.                                                  ECS267
00427      ADD +1 TO  SUB.                                              ECS267
00428                                                                   ECS267
00429      IF SUB GREATER +010                                          ECS267
00430          GO TO 0230-READ-CERT.                                    ECS267
00431                                                                   ECS267
052814     IF CR-AGT-TYPE (SUB) = 'O' OR 'P' or 'S'
00433           GO TO 0270-LOAD-TABLE.                                  ECS267
00434                                                                   ECS267
00435      IF CR-AGT-TYPE (SUB) = 'D' OR 'C'                            ECS267
00436          NEXT SENTENCE                                            ECS267
00437      ELSE                                                         ECS267
00438          GO TO 0260-CALC-COMM.                                    ECS267
00439                                                                   ECS267
00440      COMPUTE CR-LFPRM-CALC ROUNDED = CR-LCOM-L (SUB) * CR-LFPRM.  ECS267
00441      COMPUTE CR-AHPRM-CALC ROUNDED = CR-LCOM-AH (SUB)             ECS267
00442                                    * CR-AHPRM.                    ECS267
00443                                                                   ECS267
00444      ADD CR-AHPRM-CALC           TO  CO-YTD-COM.                  ECS267
00445      ADD CR-LFPRM-CALC           TO  CO-YTD-COM.                  ECS267
00446                                                                   ECS267
00447      GO TO 0260-CALC-COMM.                                        ECS267
00448  EJECT                                                            ECS267
00449  0270-LOAD-TABLE.                                                 ECS267
00450      MOVE +0                     TO  AGT.                         ECS267
00451                                                                   ECS267
00452  0280-LOOP-TABLE.                                                 ECS267
00453      ADD +1                      TO  AGT.                         ECS267
00454                                                                   ECS267
00455      IF AGT GREATER +300                                          ECS267
00456          PERFORM 0300-DUMP-TABLE-RTN  THRU  0399-EXIT             ECS267
00457          GO TO 0280-LOOP-TABLE.                                   ECS267
00458                                                                   ECS267
00459      IF (AGT-NO   (AGT) = CR-COM-AGT (SUB)  AND                   ECS267
00460          AGT-CARR (AGT) = CR-CARRIER        AND                   ECS267
00461          AGT-COMP (AGT) = CR-GROUPING)                            ECS267
00462                    OR                                             ECS267
00463         (AGT-NO   (AGT) = HIGH-VALUES  AND                        ECS267
00464          AGT-CARR (AGT) = HIGH-VALUES  AND                        ECS267
00465          AGT-COMP (AGT) = HIGH-VALUES)                            ECS267
00466            MOVE CR-COM-AGT (SUB)   TO  AGT-NO   (AGT)             ECS267
00467            MOVE CR-CARRIER         TO  AGT-CARR (AGT)             ECS267
00468            MOVE CR-GROUPING        TO  AGT-COMP (AGT)             ECS267
00469            GO TO 0290-HAVE-AGT.                                   ECS267
00470                                                                   ECS267
00471      GO TO 0280-LOOP-TABLE.                                       ECS267
00472                                                                   ECS267
00473  0290-HAVE-AGT.                                                   ECS267
00474      COMPUTE AGT-AMT (AGT) ROUNDED = AGT-AMT (AGT)                ECS267
00475                                   + ((CR-LFPRM * CR-LCOM-L (SUB)) ECS267
00476                                   + (CR-AHPRM * CR-LCOM-AH (SUB)))ECS267
00477                                                                   ECS267
00478      GO TO 0260-CALC-COMM.                                        ECS267
00479  EJECT                                                            ECS267
00480  0300-DUMP-TABLE-RTN.                                             ECS267
00481      MOVE +0                     TO  AGT.                         ECS267
00482      MOVE COMPENSATION-MASTER    TO  SAVE-RECORD.                 ECS267
00483      MOVE SPACES                 TO  COMPENSATION-MASTER.         ECS267
00484      MOVE 'CO'                   TO  CO-RECORD-ID.                ECS267
00485      MOVE DTE-CLASIC-COMPANY-CD  TO  CO-COMPANY-CD.               ECS267
00486      MOVE 'G'                    TO  CO-TYPE.                     ECS267
00487      MOVE 'Y'                    TO  CO-BALANCE-CONTROL.          ECS267
00488      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1        ECS267
00489                                      CO-INTERNAL-CONTROL-2.       ECS267
00490      MOVE LOW-VALUES             TO  CO-ACCOUNT.                  ECS267
00491      MOVE BIN-RUN-DATE           TO  CO-LAST-MAINT-DT.            ECS267
00492                                                                   ECS267
00493      MOVE WS-TIME-OF-DAY         TO  CO-LAST-MAINT-HHMMSS.        ECS267
00494      MOVE RUN-MO                 TO  CO-ACT-MONTH                 ECS267
00495                                      CO-LAST-STMT-YEAR.           ECS267
00496      MOVE RUN-YR                 TO  CO-ACT-YEAR                  ECS267
00497                                      CO-LAST-STMT-MONTH.          ECS267
00498      MOVE 30                     TO  CO-LAST-STMT-DAY             ECS267
00499                                      CO-ACT-DAY.                  ECS267
00500      MOVE +0                     TO  CO-BAL-FWD                   ECS267
00501                                      CO-CUR-COM                   ECS267
00502                                      CO-CUR-CHG                   ECS267
00503                                      CO-CUR-PMT                   ECS267
00504                                      CO-END-BAL                   ECS267
00505                                      CO-CUR                       ECS267
00506                                      CO-OV30                      ECS267
00507                                      CO-OV60                      ECS267
00508                                      CO-OV90                      ECS267
00509                                      CO-YTD-COM                   ECS267
00510                                      CO-YTD-OV                    ECS267
00511                                      CO-LF-CLM-AMT                ECS267
00512                                      CO-AH-CLM-AMT                ECS267
00513                                      CO-CUR-FICA                  ECS267
00514                                      CO-YTD-FICA                  ECS267
00515                                      CO-CUR-OVR-UNDR              ECS267
00516                                      CO-YTD-OVR-UNDR.             ECS267
00517  EJECT                                                            ECS267
00518  0310-DUMP-TABLE.                                                 ECS267
00519      ADD +1                      TO  AGT.                         ECS267
00520                                                                   ECS267
00521      IF AGT GREATER +300                                          ECS267
00522          GO TO 0320-END-DUMP.                                     ECS267
00523                                                                   ECS267
00524      IF AGT-NO (AGT) = HIGH-VALUES                                ECS267
00525          GO TO 0320-END-DUMP.                                     ECS267
00526                                                                   ECS267
00527      MOVE AGT-CARR (AGT)         TO  CO-CARRIER.                  ECS267
00528      MOVE AGT-COMP (AGT)         TO  CO-GROUPING.                 ECS267
00529      MOVE AGT-NO   (AGT)         TO  CO-RESP-NO.                  ECS267
00530      MOVE AGT-AMT  (AGT)        TO  CO-YTD-OV.                    ECS267
00531                                                                   ECS267
00532      PERFORM 0500-RELEASE-ROUTINE  THRU  0599-EXIT.               ECS267
00533                                                                   ECS267
00534      MOVE HIGH-VALUES            TO  AGT-NO   (AGT)               ECS267
00535                                      AGT-CARR (AGT)               ECS267
00536                                      AGT-COMP (AGT).              ECS267
00537      MOVE +0                     TO  AGT-AMT  (AGT).              ECS267
00538                                                                   ECS267
00539      GO TO 0310-DUMP-TABLE.                                       ECS267
00540                                                                   ECS267
00541  0320-END-DUMP.                                                   ECS267
00542      MOVE SAVE-RECORD            TO  COMPENSATION-MASTER.         ECS267
00543      MOVE +0                     TO  AGT.                         ECS267
00544                                                                   ECS267
00545  0399-EXIT.                                                       ECS267
00546      EXIT.                                                        ECS267
00547  EJECT                                                            ECS267
00548  0400-RD-ACCT-RTN.                                                ECS267
00549      READ ACCT-IN  AT END                                         ECS267
00550          MOVE +1111              TO  WS-RETURN-CODE               ECS267
00551          MOVE 'ACCOUNT MASTER - END OF FILE'                      ECS267
00552                                  TO  WS-ABEND-MESSAGE             ECS267
00553          GO TO ABEND-PGM.                                         ECS267
00554                                                                   ECS267
00555      MOVE ACCT-REC-IN            TO  ACCOUNT-MASTER.              ECS267
00556                                                                   ECS267
00557      PERFORM 0500-RELEASE-ROUTINE  THRU  0599-EXIT.               ECS267
00558                                                                   ECS267
00559      MOVE SPACES                 TO  COMPENSATION-MASTER.         ECS267
00560      MOVE DTE-CLASIC-COMPANY-CD  TO  CO-COMPANY-CD.               ECS267
00561      MOVE 'CO'                   TO  CO-RECORD-ID.                ECS267
00562      MOVE 'A'                    TO  CO-TYPE.                     ECS267
00563      MOVE 'N'                    TO  CO-INTERNAL-CONTROL-1        ECS267
00564                                      CO-INTERNAL-CONTROL-2.       ECS267
00565      MOVE AM-CONTROL-A           TO  CO-CARR-GROUP.               ECS267
00566      MOVE AM-AGT (1)             TO  CO-ACCOUNT.                  ECS267
00567                                                                   ECS267
00568      IF AM-REMIT-TO GREATER '01' AND LESS '11'                    ECS267
00569          MOVE AM-REMIT-TO        TO  REMITX                       ECS267
00570      ELSE                                                         ECS267
00571          MOVE 01                 TO  REMITX  AM-REMIT-TO.         ECS267
00572                                                                   ECS267
00573      IF AM-REMIT-TO = '01'                                        ECS267
00574          MOVE AM-AGT (1)         TO  CO-RESP-NO                   ECS267
00575          MOVE 'Y'                TO  CO-BALANCE-CONTROL           ECS267
00576      ELSE                                                         ECS267
00577          MOVE AM-AGT (REMITX)    TO  CO-RESP-NO                   ECS267
00578          MOVE 'N'                TO  CO-BALANCE-CONTROL.          ECS267
00579                                                                   ECS267
00580      MOVE BIN-RUN-DATE           TO  CO-LAST-MAINT-DT.            ECS267
00581      MOVE WS-TIME-OF-DAY         TO  CO-LAST-MAINT-HHMMSS.        ECS267
00582      MOVE RUN-MO                 TO  CO-ACT-MONTH                 ECS267
00583                                      CO-LAST-STMT-MONTH.          ECS267
00584      MOVE RUN-YR                 TO  CO-ACT-YEAR                  ECS267
00585                                      CO-LAST-STMT-YEAR.           ECS267
00586      MOVE 30                     TO  CO-LAST-STMT-DAY             ECS267
00587                                      CO-ACT-DAY.                  ECS267
00588  EJECT                                                            ECS267
00589      MOVE +0                     TO  CO-BAL-FWD                   ECS267
00590                                      CO-CUR-COM                   ECS267
00591                                      CO-CUR-CHG                   ECS267
00592                                      CO-CUR-PMT                   ECS267
00593                                      CO-END-BAL                   ECS267
00594                                      CO-OV30                      ECS267
00595                                      CO-OV60                      ECS267
00596                                      CO-OV90                      ECS267
00597                                      CO-YTD-COM                   ECS267
00598                                      CO-YTD-OV                    ECS267
00599                                      CO-LF-CLM-AMT                ECS267
00600                                      CO-AH-CLM-AMT                ECS267
00601                                      CO-CUR-FICA                  ECS267
00602                                      CO-YTD-FICA                  ECS267
00603                                      CO-CUR-OVR-UNDR              ECS267
00604                                      CO-YTD-OVR-UNDR.             ECS267
00605                                                                   ECS267
00606  0499-EXIT.                                                       ECS267
00607      EXIT.                                                        ECS267
00608  EJECT                                                            ECS267
00609  0500-RELEASE-ROUTINE.                                            ECS267
00610      IF NOT VALID-CO-ID                                           ECS267
00611          GO TO 0599-EXIT.                                         ECS267
00612                                                                   ECS267
00613  0510-RELEASE.                                                    ECS267
00614      IF CO-LAST-STMT-DT NOT NUMERIC                               ECS267
00615          MOVE ZERO               TO  CO-LAST-STMT-DT.             ECS267
00616                                                                   ECS267
00617      IF CO-LF-CLM-AMT NOT NUMERIC                                 ECS267
00618          MOVE +0                 TO  CO-LF-CLM-AMT.               ECS267
00619                                                                   ECS267
00620      IF CO-AH-CLM-AMT NOT NUMERIC                                 ECS267
00621          MOVE +0                 TO  CO-AH-CLM-AMT.               ECS267
00622                                                                   ECS267
00623      IF CO-CUR-FICA NOT NUMERIC                                   ECS267
00624          MOVE +0                 TO  CO-CUR-FICA.                 ECS267
00625                                                                   ECS267
00626      IF CO-YTD-FICA NOT NUMERIC                                   ECS267
00627          MOVE +0                 TO  CO-YTD-FICA.                 ECS267
00628                                                                   ECS267
00629      IF CO-CUR-OVR-UNDR NOT NUMERIC                               ECS267
00630          MOVE +0                 TO  CO-CUR-OVR-UNDR.             ECS267
00631                                                                   ECS267
00632      IF CO-YTD-OVR-UNDR NOT NUMERIC                               ECS267
00633          MOVE +0                 TO  CO-YTD-OVR-UNDR.             ECS267
00634                                                                   ECS267
00635      IF CO-BAL-FWD NOT NUMERIC                                    ECS267
00636          MOVE +0                 TO  CO-BAL-FWD.                  ECS267
00637                                                                   ECS267
00638      IF CO-CUR-CHG NOT NUMERIC                                    ECS267
00639          MOVE +0                 TO  CO-CUR-CHG.                  ECS267
00640                                                                   ECS267
00641      IF CO-CUR-COM NOT NUMERIC                                    ECS267
00642          MOVE +0                 TO  CO-CUR-COM.                  ECS267
00643                                                                   ECS267
00644      IF CO-CUR-PMT NOT NUMERIC                                    ECS267
00645          MOVE +0                 TO  CO-CUR-PMT.                  ECS267
00646                                                                   ECS267
00647      IF CO-END-BAL NOT NUMERIC                                    ECS267
00648          MOVE +0                 TO  CO-END-BAL.                  ECS267
00649  EJECT                                                            ECS267
00650      IF CO-CUR NOT NUMERIC                                        ECS267
00651          MOVE +0                 TO  CO-CUR.                      ECS267
00652                                                                   ECS267
00653      IF CO-OV30 NOT NUMERIC                                       ECS267
00654          MOVE +0                 TO  CO-OV30.                     ECS267
00655                                                                   ECS267
00656      IF CO-OV60 NOT NUMERIC                                       ECS267
00657          MOVE +0                 TO  CO-OV60.                     ECS267
00658                                                                   ECS267
00659      IF CO-OV90 NOT NUMERIC                                       ECS267
00660          MOVE +0                 TO  CO-OV90.                     ECS267
00661                                                                   ECS267
00662      IF CO-YTD-COM NOT NUMERIC                                    ECS267
00663          MOVE +0                 TO  CO-YTD-COM.                  ECS267
00664                                                                   ECS267
00665      IF CO-YTD-OV NOT NUMERIC                                     ECS267
00666          MOVE +0                 TO  CO-YTD-OV.                   ECS267
00667                                                                   ECS267
00668      MOVE COMPENSATION-MASTER    TO  SORT-REC.                    ECS267
00669                                                                   ECS267
00670      RELEASE SORT-REC.                                            ECS267
00671                                                                   ECS267
00672  0599-EXIT.                                                       ECS267
00673      EXIT.                                                        ECS267
00674  EJECT                                                            ECS267
00675  0600-LOAD-OLD-RTN.                                               ECS267
00676      OPEN INPUT  COMM-IN.                                         ECS267
00677                                                                   ECS267
00678  0610-READ-OLD.                                                   ECS267
00679      READ COMM-IN  AT END                                         ECS267
00680          GO TO 0620-END-OLD.                                      ECS267
00681                                                                   ECS267
00682      MOVE COMP-IN-RECORD         TO  COMPENSATION-MASTER.         ECS267
00683      MOVE +0                     TO  CO-YTD-COM   CO-YTD-OV.      ECS267
00684                                                                   ECS267
00685      PERFORM 0500-RELEASE-ROUTINE  THRU  0599-EXIT.               ECS267
00686                                                                   ECS267
00687      GO TO 0610-READ-OLD.                                         ECS267
00688                                                                   ECS267
00689  0620-END-OLD.                                                    ECS267
00690      MOVE LOW-VALUE              TO  COMPENSATION-MASTER.         ECS267
00691                                                                   ECS267
00692      CLOSE COMM-IN.                                               ECS267
00693                                                                   ECS267
00694  0699-EXIT.                                                       ECS267
00695      EXIT.                                                        ECS267
00696  EJECT                                                            ECS267
00697  0700-END-INPUT.                                                  ECS267
00698      PERFORM 0500-RELEASE-ROUTINE  THRU  0599-EXIT.               ECS267
00699                                                                   ECS267
00700      PERFORM 0300-DUMP-TABLE-RTN  THRU  0399-EXIT.                ECS267
00701                                                                   ECS267
00702      CLOSE CERT-IN  ACCT-IN.                                      ECS267
00703                                                                   ECS267
00704  0799-EXIT.                                                       ECS267
00705      EXIT.                                                        ECS267
00706  EJECT                                                            ECS267
00707  0800-OUTPUT-RTN  SECTION.                                        ECS267
00708                                                                   ECS267
00709  0810-OPEN-OUTPUT.                                                ECS267
00710      OPEN OUTPUT  COMM-OUT.                                       ECS267
00711                                                                   ECS267
00712      MOVE LOW-VALUE              TO  COMPENSATION-MASTER.         ECS267
00713      MOVE +066                   TO  LNCTR.                       ECS267
00714                                                                   ECS267
00715  0820-RETURN-RTN.                                                 ECS267
00716      RETURN SORT-FILE  AT END                                     ECS267
00717          PERFORM 0900-WRT-COMM-RTN  THRU  0999-EXIT               ECS267
00718          GO TO 1000-END-OUTPUT.                                   ECS267
00719                                                                   ECS267
00720      IF SORT-PARM NOT = CO-CONTROL-PRIMARY                        ECS267
00721          GO TO 0830-NEW-CONTROL.                                  ECS267
00722                                                                   ECS267
00723      MOVE COMPENSATION-MASTER    TO  SAVE-RECORD.                 ECS267
00724      MOVE SORT-REC               TO  COMPENSATION-MASTER.         ECS267
00725                                                                   ECS267
00726      ADD SAV-YTD-COM             TO  CO-YTD-COM.                  ECS267
00727      ADD SAV-YTD-OV              TO  CO-YTD-OV.                   ECS267
00728      ADD SAV-END-BAL             TO  CO-END-BAL.                  ECS267
00729      ADD SAV-BAL-FWD             TO  CO-BAL-FWD.                  ECS267
00730      ADD SAV-CUR-COM             TO  CO-CUR-COM.                  ECS267
00731      ADD SAV-CUR-CHG             TO  CO-CUR-CHG.                  ECS267
00732      ADD SAV-CUR-PMT             TO  CO-CUR-PMT.                  ECS267
00733      ADD SAV-CUR                 TO  CO-CUR.                      ECS267
00734      ADD SAV-OV30                TO  CO-OV30.                     ECS267
00735      ADD SAV-OV60                TO  CO-OV60.                     ECS267
00736      ADD SAV-OV90                TO  CO-OV90.                     ECS267
00737      ADD SAV-LF-CLM-AMT          TO  CO-LF-CLM-AMT.               ECS267
00738      ADD SAV-AH-CLM-AMT          TO  CO-AH-CLM-AMT.               ECS267
00739      ADD SAV-CUR-FICA            TO  CO-CUR-FICA.                 ECS267
00740      ADD SAV-YTD-FICA            TO  CO-YTD-FICA.                 ECS267
00741      ADD SAV-CUR-OVR-UNDR        TO  CO-CUR-OVR-UNDR.             ECS267
00742      ADD SAV-YTD-OVR-UNDR        TO  CO-YTD-OVR-UNDR.             ECS267
00743                                                                   ECS267
00744      IF SAV-SOC-SEC NOT = SPACES                                  ECS267
00745          MOVE SAV-SOC-SEC        TO  CO-SOC-SEC.                  ECS267
00746                                                                   ECS267
00747      IF SAV-TELEPHONE NOT = SPACES                                ECS267
00748          MOVE SAV-TELEPHONE      TO  CO-TELEPHONE.                ECS267
00749                                                                   ECS267
00750      IF SAV-ACCT-NAME NOT = SPACES                                ECS267
00751          MOVE SAV-ACCT-NAME      TO  CO-ACCT-NAME.                ECS267
00752                                                                   ECS267
00753      IF SAV-MAIL-NAME NOT = SPACES                                ECS267
00754          MOVE SAV-MAIL-NAME      TO  CO-MAIL-NAME.                ECS267
00755                                                                   ECS267
00756      IF SAV-ADDR-1 NOT = SPACES                                   ECS267
00757          MOVE SAV-ADDR-1         TO  CO-ADDR-1.                   ECS267
00758                                                                   ECS267
00759      IF SAV-ADDR-2 NOT = SPACES                                   ECS267
00760          MOVE SAV-ADDR-2         TO  CO-ADDR-2.                   ECS267
00761  EJECT                                                            ECS267
00762      IF SAV-ADDR-3 NOT = SPACES                                   ECS267
00763          MOVE SAV-ADDR-3         TO  CO-ADDR-3.                   ECS267
00764                                                                   ECS267
00765      IF SAV-ZIP    NOT = SPACES                                   ECS267
00766          MOVE SAV-ZIP            TO  CO-ZIP.                      ECS267
00767                                                                   ECS267
00768      GO TO 0820-RETURN-RTN.                                       ECS267
00769                                                                   ECS267
00770  0830-NEW-CONTROL.                                                ECS267
00771      PERFORM 0900-WRT-COMM-RTN  THRU  0999-EXIT.                  ECS267
00772                                                                   ECS267
00773      MOVE SORT-REC               TO  COMPENSATION-MASTER.         ECS267
00774                                                                   ECS267
00775      GO TO 0820-RETURN-RTN.                                       ECS267
00776  EJECT                                                            ECS267
00777  0900-WRT-COMM-RTN.                                               ECS267
00778      IF NOT VALID-CO-ID                                           ECS267
00779          GO TO 0999-EXIT.                                         ECS267
00780                                                                   ECS267
00781      IF CO-ADDR-1 = CO-ADDR-2                                     ECS267
00782          MOVE SPACES             TO  CO-ADDR-2.                   ECS267
00783                                                                   ECS267
00784      MOVE COMPENSATION-MASTER    TO  COMP-OUT-RECORD.             ECS267
00785                                                                   ECS267
00786      WRITE COMP-OUT-RECORD.                                       ECS267
00787                                                                   ECS267
00788      IF LNCTR GREATER +056                                        ECS267
00789          PERFORM 1300-HEADING-ROUTINE  THRU  1399-EXIT.           ECS267
00790                                                                   ECS267
00791      MOVE CO-CARRIER             TO  P-CARR.                      ECS267
00792      MOVE CO-GROUPING            TO  P-COMP.                      ECS267
00793                                                                   ECS267
00794      IF CO-RESP-NO NOT = LOW-VALUE                                ECS267
00795          MOVE CO-RESP-NO         TO  P-RESP.                      ECS267
00796                                                                   ECS267
00797      IF CO-ACCOUNT NOT = LOW-VALUE                                ECS267
00798          MOVE CO-ACCOUNT         TO  P-ACCT.                      ECS267
00799                                                                   ECS267
00800      MOVE CO-ACCT-NAME           TO  P-NAME.                      ECS267
00801      MOVE CO-SOC-SEC             TO  P-TXID.                      ECS267
00802      MOVE CO-YTD-COM             TO  P-COMM.                      ECS267
00803      MOVE CO-YTD-OV              TO  P-OV.                        ECS267
00804      MOVE CO-END-BAL             TO  P-BAL.                       ECS267
00805                                                                   ECS267
00806      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00807                                                                   ECS267
00808      ADD CO-YTD-COM              TO  TOTAL-COMM.                  ECS267
00809      ADD CO-YTD-OV               TO  TOTAL-COMM.                  ECS267
00810                                                                   ECS267
00811  0999-EXIT.                                                       ECS267
00812      EXIT.                                                        ECS267
00813  EJECT                                                            ECS267
00814  1000-END-OUTPUT.                                                 ECS267
00815      MOVE 'FINAL TOTALS'         TO  P-NAME.                      ECS267
00816      MOVE TOTAL-COMM             TO  P-COMM.                      ECS267
00817                                                                   ECS267
00818      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00819                                                                   ECS267
00820      CLOSE COMM-OUT.                                              ECS267
00821                                                                   ECS267
00822  1099-EXIT.                                                       ECS267
00823      EXIT.                                                        ECS267
00824  EJECT                                                            ECS267
00825  1100-COMMON-ROUTINES  SECTION.                                   ECS267
00826                                                                   ECS267
00827  1200-DATE-CONVERSION.                                            ECS267
00828      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   ECS267
00829                                                                   ECS267
00830  1299-EXIT.                                                       ECS267
00831      EXIT.                                                        ECS267
00832                                                                   ECS267
00833  1300-HEADING-ROUTINE.                                            ECS267
00834      MOVE SPACE-NP               TO  P-REC.                       ECS267
00835      MOVE HD1                    TO  P-LINE.                      ECS267
00836                                                                   ECS267
00837      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00838                                                                   ECS267
00839      MOVE HD2                    TO  P-LINE.                      ECS267
00840                                                                   ECS267
00841      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00842                                                                   ECS267
00843      ADD +1                      TO  PGCTR.                       ECS267
00844                                                                   ECS267
00845      MOVE PGCTR                  TO  H3-PAGE.                     ECS267
00846      MOVE HD3                    TO  P-LINE.                      ECS267
00847                                                                   ECS267
00848      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00849                                                                   ECS267
00850      MOVE SPACE-2                TO  P-CCSW.                      ECS267
00851      MOVE HD4                    TO  P-LINE.                      ECS267
00852                                                                   ECS267
00853      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00854                                                                   ECS267
00855      MOVE HD5                    TO  P-LINE.                      ECS267
00856                                                                   ECS267
00857      PERFORM 1400-PRINT-ROUTINE  THRU  1499-EXIT.                 ECS267
00858                                                                   ECS267
00859      MOVE 7                      TO  LNCTR.                       ECS267
00860      MOVE SPACE-2                TO  P-CCSW.                      ECS267
00861                                                                   ECS267
00862  1399-EXIT.                                                       ECS267
00863      EXIT.                                                        ECS267
00864  EJECT                                                            ECS267
00865  1400-PRINT-ROUTINE.                                              ECS267
00866      MOVE P-REC                  TO  PRT.                         ECS267
00867      MOVE P-CCSW                 TO  X.                           ECS267
00868                                                                   ECS267
00869      IF P-CCSW = SPACE-1                                          ECS267
00870          ADD 1                   TO  LNCTR                        ECS267
00871      ELSE                                                         ECS267
00872          IF P-CCSW = SPACE-2                                      ECS267
00873              ADD 2               TO  LNCTR                        ECS267
00874          ELSE                                                     ECS267
00875              IF P-CCSW = SPACE-3                                  ECS267
00876                  ADD 3           TO  LNCTR                        ECS267
00877              ELSE                                                 ECS267
00878                  MOVE 1          TO  LNCTR.                       ECS267
00879                                                                   ECS267
00880      MOVE SPACE-1                TO  P-REC.                       ECS267
00881                                                                   ECS267
00882  1410-COPY-PRINT.                                                 ECS267
00883                              COPY ELCPRT2.                           CL**5
00884                                                                   ECS267
00885  1499-EXIT.                                                       ECS267
00886      EXIT.                                                        ECS267
00887  EJECT                                                            ECS267
00888  1500-END-OF-JOB  SECTION.                                        ECS267
00889                                                                   ECS267
00890  1510-E-O-J.                                                      ECS267
00891      CLOSE PRNTR.                                                 ECS267
00892                                                                   ECS267
00893  1520-CLOSE-FICH.                                                 ECS267
00894                              COPY ELCPRTC.                        ECS267
00895                                                                   ECS267
00896      GOBACK.                                                      ECS267
00897                                                                   ECS267
00898  ABEND-PGM  SECTION.                                              ECS267
00899                              COPY ELCABEND.                       ECS267
