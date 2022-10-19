00001  IDENTIFICATION DIVISION.                                         03/09/98
00002                                                                   ECS083
00003  PROGRAM-ID.                ECS083.                                  LV002
00004 *              PROGRAM CONVERTED BY                               ECS083
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS083
00006 *              CONVERSION DATE 02/08/96 18:42:05.                 ECS083
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          ECS083
00008 *                           VMOD=2.008.                           ECS083
00009 *AUTHOR.     LOGIC, INC.                                          ECS083
00010 *            DALLAS, TEXAS.                                       ECS083
00011                                                                   ECS083
00012 *DATE-COMPILED.                                                   ECS083
00013                                                                   ECS083
00014 *SECURITY.   *****************************************************ECS083
00015 *            *                                                   *ECS083
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *ECS083
00017 *            *                                                   *ECS083
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *ECS083
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *ECS083
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *ECS083
00021 *            *                                                   *ECS083
00022 *            *****************************************************ECS083
00023                                                                   ECS083
00024 *REMARKS.                                                         ECS083
00025 *        EXTRACT UNEARNED PREMIUM AND COMMISSION REPORT RECORDS.  ECS083
00026 *        EXTRACT BY PROGRAM SWITCHES                              ECS083
00027 *            1 - STATE                                            ECS083
00028 *                COMPANY                                          ECS083
00029 *                STATE WITHIN CARRIER                             ECS083
00030 *                CARRIER                                          ECS083
00031 *                STATE OVERALL                                    ECS083
00032 *                FINAL TOTALS                                     ECS083
00033 *            2 - COMPANY                                          ECS083
00034 *                STATE WITHIN CARRIER                             ECS083
00035 *                CARRIER                                          ECS083
00036 *                STATE OVERALL                                    ECS083
00037 *                FINAL TOTALS                                     ECS083
00038 *            3 - STATE WITHIN CARRIER                             ECS083
00039 *                CARRIER                                          ECS083
00040 *                STATE OVERALL                                    ECS083
00041 *                FINAL TOTALS                                     ECS083
00042 *            4 - CARRIER                                          ECS083
00043 *                STATE OVERALL                                    ECS083
00044 *                FINAL TOTALS                                     ECS083
00045 *            5 - STATE OVERALL                                    ECS083
00046 *                FINAL TOTALS                                     ECS083
00047 *            6 - FINAL TOTALS                                     ECS083
121610******************************************************************
121610*                   C H A N G E   L O G
121610*
121610* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121610*-----------------------------------------------------------------
121610*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121610* EFFECTIVE    NUMBER
121610*-----------------------------------------------------------------
121610* 121610    2010120900001  PEMA  CORRECT ISS YR SORT
020113* 020113  IR2013020100001  PEMA  INCREASE # OF MORT TABLES
040114* 040114  CR2011122200002  AJRA  MODIFY DOMICILE PREM, COMM, AND TAX
010716* 010716  CR2015082500001  PEMA  VPP CHANGES
120816* 120816  CR2016111600001  PEMA  Add stat load to ahstat uep
012519* 012519* IR2019012500001  PEMA  Fix bug when > 99 mort tables
121610******************************************************************
00048                                                                   ECS083
00049  ENVIRONMENT DIVISION.                                            ECS083
00050  INPUT-OUTPUT SECTION.                                            ECS083
00051  FILE-CONTROL.                                                    ECS083
00052                                                                   ECS083
00053      SELECT REPTFL    ASSIGN TO SYS004-UT-FBA1-S-SYS004.          ECS083
00054      SELECT PRNTR     ASSIGN TO SYS008-UR-1403-S-SYS008.          ECS083
00055      SELECT GAAP-EXTR ASSIGN TO SYS011-UT-2400-S-SYS011.          ECS083
00056      SELECT DISK-DATE ASSIGN TO SYS019-UT-FBA1-S-SYS019.          ECS083
00057      SELECT FICH      ASSIGN TO SYS020-UT-2400-S-SYS020.          ECS083
00058  EJECT                                                            ECS083
00059  DATA DIVISION.                                                   ECS083
00060  FILE SECTION.                                                    ECS083
00061                                                                   ECS083
00062  FD  REPTFL                                                       ECS083
00063      BLOCK CONTAINS 0 RECORDS
00064      RECORDING MODE F.                                            ECS083
00065                                                                   ECS083
00066  01  RPT-REC.                                                     ECS083
012519     12  R-PARM              PIC X(27).
040114     12  FILLER              PIC X(130).                          ECS083
00069  EJECT                                                            ECS083
00070  FD  PRNTR                                                        ECS083
00071                              COPY ELCPRTFD.                       ECS083
00072  EJECT                                                            ECS083
00073  FD  GAAP-EXTR                                                    ECS083
00074                              COPY ECSGAPFD.                       ECS083
00075                                                                   ECS083
00076                              COPY ECSGAP01.                       ECS083
00077  EJECT                                                            ECS083
00078  FD  DISK-DATE                                                    ECS083
00079                              COPY ELCDTEFD.                       ECS083
00080  EJECT                                                            ECS083
00081  FD  FICH                                                         ECS083
00082                              COPY ECSFICH.                        ECS083
00083  EJECT                                                            ECS083
00084  WORKING-STORAGE SECTION.                                         ECS083
00085  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      ECS083
00086  77  FILLER  PIC X(32) VALUE '********************************'.  ECS083
00087  77  FILLER  PIC X(32) VALUE '     ECS083 WORKING STORAGE     '.  ECS083
00088  77  FILLER  PIC X(32) VALUE '***** VMOD=2.008 ***************'.  ECS083
00089                                                                   ECS083
00090  77  PGM-SUB                 PIC S999    COMP    VALUE +083.      ECS083
00091  77  X1                      PIC S9(4)   COMP.                    ECS083
00092  77  X2                      PIC S9(4)   COMP.                    ECS083
00093  77  X3                      PIC S9(4)   COMP.                    ECS083
00094  77  MAX-BEN                 PIC S9(4)   COMP    VALUE +100.      ECS083
00095  77  X                       PIC X               VALUE SPACE.     ECS083
121610 77  WORK-YR                 PIC S9999           VALUE ZERO.      ECS083
040114 77  WRK-DOMI                PIC S9(9)V99 COMP-3 VALUE +0.
00096                                                                      CL**2
00097  01  WORK-REC.                                                    ECS083
00098      12  W-REIN              PIC X(6).                            ECS083
00099      12  W-SEQ.                                                   ECS083
00100          16  W-SEQ1.                                              ECS083
00101              20  W-CARR      PIC X.                               ECS083
00102              20  W-CO        PIC X(6).                            ECS083
00103              20  W-ST        PIC XX.                              ECS083
00104          16  W-SEQ2.                                              ECS083
121610             20  W-YR        PIC 9999.
00106              20  W-CODE      PIC 9.                               ECS083
00107          16  W-SEQ3.                                              ECS083
00108              20  W-LAH       PIC X.                               ECS083
00109              20  W-BEN       PIC XX.                              ECS083
012519             20  W-MORT      PIC 999.
00111      12  W-FL                PIC X.                               ECS083
00112      12  W-AMTS.                                                  ECS083
00113          16  W-DUP           PIC S9(7)     COMP-3.                ECS083
00114          16  W-COUNT         PIC S9(7)     COMP-3.                ECS083
00115          16  W-WRITTEN       PIC S9(9)V99  COMP-3.                ECS083
00116          16  W-P78           PIC S9(9)V99  COMP-3.                ECS083
00117          16  W-PRATA         PIC S9(9)V99  COMP-3.                ECS083
00118          16  W-DOMICILE      PIC S9(9)V99  COMP-3.                ECS083
00119          16  W-STATE         PIC S9(9)V99  COMP-3.                ECS083
00120          16  W-RESERV        PIC S9(9)V99  COMP-3.                ECS083
00121          16  W-ALTRSV        PIC S9(9)V99  COMP-3.                ECS083
00122          16  W-REMAIN        PIC S9(13)V99 COMP-3.                ECS083
00123          16  W-PAID          PIC S9(9)V99  COMP-3.                ECS083
00124          16  W-C78           PIC S9(9)V99  COMP-3.                ECS083
00125          16  W-CRATA         PIC S9(9)V99  COMP-3.                ECS083
040114         16  W-CDOMI         PIC S9(9)V99  COMP-3.
00126          16  W-TAX           PIC S9(9)V99  COMP-3.                ECS083
00127          16  W-T78           PIC S9(9)V99  COMP-3.                ECS083
00128          16  W-TRATA         PIC S9(9)V99  COMP-3.                ECS083
040114         16  W-TDOMI         PIC S9(9)V99  COMP-3.
00129      12  W-M-AMTS.                                                ECS083
00130          16  W-IUNDR         PIC S9(9)V99  COMP-3.                ECS083
00131          16  W-IOVER         PIC S9(9)V99  COMP-3.                ECS083
00132          16  W-GUNDR         PIC S9(9)V99  COMP-3.                ECS083
00133          16  W-GOVER         PIC S9(9)V99  COMP-3.                ECS083
00134  EJECT                                                            ECS083
00135  01  MISC-WS.                                                     ECS083
00136      12  WS-RETURN-CODE      PIC S9(4)   COMP.                    ECS083
00137      12  WS-ABEND-MESSAGE    PIC X(80).                           ECS083
00138      12  WS-ABEND-FILE-STATUS PIC XX     VALUE ZEROS.             ECS083
00139      12  WS-ZERO             PIC S9       VALUE +0 COMP-3.        ECS083
00140      12  CUR-SEQ.                                                 ECS083
00141          16  CUR-CARR        PIC X.                               ECS083
00142          16  CUR-CO          PIC X(6).                            ECS083
00143          16  CUR-ST          PIC XX.                              ECS083
00144      12  PRE-SEQ.                                                 ECS083
00145          16  PRE-CARR        PIC X          VALUE LOW-VALUE.      ECS083
00146          16  PRE-CO          PIC X(6)       VALUE LOW-VALUE.      ECS083
00147          16  PRE-ST          PIC XX         VALUE LOW-VALUE.      ECS083
00148      12  PRE-REIN            PIC X(6)       VALUE LOW-VALUE.      ECS083
00149      12  X-YR                PIC 9(04)      VALUE ZEROS.          ECS083
00150                                                                   ECS083
00151  01  PASS-ONE-WORK.                                               ECS083
00152      12  XT-TABLE.                                                ECS083
121610         16  XT-YEARS        OCCURS 15 TIMES.                     ECS083
00154              20  XT-SEQ      OCCURS 100 TIMES.                    ECS083
00155                  24  XT-SEQ1     PIC XXX.                         ECS083
012519                 24  XT-SEQ2     PIC XXX.
00157                  24  XT-SEQ3     PIC X(6).                        ECS083
00158      12  YT-TABLE.                                                ECS083
121610         16  YT-YEARS        OCCURS 15 TIMES.                     ECS083
00160              20  YT-SEQ      OCCURS 100 TIMES.                    ECS083
00161                  24  YT-SEQ1     PIC XXX.                         ECS083
012519                 24  YT-SEQ2     PIC XXX.
00163                  24  YT-SEQ3     PIC X(6).                        ECS083
00164      12  X-SEQ.                                                   ECS083
00165          16  X-SEQ1.                                              ECS083
00166              20  X-LAH           PIC X.                           ECS083
00167              20  X-BEN           PIC XX.                          ECS083
00168          16  X-SEQ2.                                              ECS083
012519             20  X-MORT          PIC 999.
00170          16  X-SEQ3.                                              ECS083
00171              20  X-REIN          PIC X(6).                        ECS083
00172                                                                   ECS083
00173  01  COMMON-TOTALS.                                               ECS083
00174      12  X-DETL.                                                  ECS083
00175          16  X-DUP               PIC S9(7)     COMP-3.            ECS083
00176          16  X-COUNT             PIC S9(7)     COMP-3.            ECS083
00177          16  X-WRITTEN           PIC S9(9)V99  COMP-3.            ECS083
00178          16  X-P78               PIC S9(9)V99  COMP-3.            ECS083
00179          16  X-PRATA             PIC S9(9)V99  COMP-3.            ECS083
00180          16  X-DOMICILE          PIC S9(9)V99  COMP-3.            ECS083
00181          16  X-STATE             PIC S9(9)V99  COMP-3.            ECS083
00182          16  X-RESERV            PIC S9(9)V99  COMP-3.            ECS083
00183          16  X-ALTRSV            PIC S9(9)V99  COMP-3.            ECS083
00184          16  X-REMAIN            PIC S9(13)V99 COMP-3.            ECS083
00185          16  X-PAID              PIC S9(9)V99  COMP-3.            ECS083
00186          16  X-C78               PIC S9(9)V99  COMP-3.            ECS083
00187          16  X-CRATA             PIC S9(9)V99  COMP-3.            ECS083
040114         16  X-CDOMI             PIC S9(9)V99  COMP-3.
00188          16  X-TAX               PIC S9(9)V99  COMP-3.            ECS083
00189          16  X-T78               PIC S9(9)V99  COMP-3.            ECS083
00190          16  X-TRATA             PIC S9(9)V99  COMP-3.            ECS083
040114         16  X-TDOMI             PIC S9(9)V99  COMP-3.
00191      12  X-M-DETL.                                                ECS083
00192          16  X-IUNDR             PIC S9(9)V99  COMP-3.            ECS083
00193          16  X-IOVER             PIC S9(9)V99  COMP-3.            ECS083
00194          16  X-GUNDR             PIC S9(9)V99  COMP-3.            ECS083
00195          16  X-GOVER             PIC S9(9)V99  COMP-3.            ECS083
00196                                                                   ECS083
00197  01  COMMON-TOTALS-1.                                             ECS083
00198      12  X-TOTALS.                                                ECS083
121610         16  X-YEARS         OCCURS 15 TIMES.                     ECS083
00200              20  X-TYPES     OCCURS 100 TIMES.                    ECS083
040114                 24  X-AMTS      PIC X(106).                      ECS083
00202                                                                   ECS083
00203  01  COMMON-TOTALS-2.                                             ECS083
00204      12  Y-TOTALS.                                                ECS083
121610         16  Y-YEARS         OCCURS 15 TIMES.                     ECS083
00206              20  Y-TYPES     OCCURS 100 TIMES.                    ECS083
040114                 24  Y-AMTS      PIC X(106).                      ECS083
00208                                                                   ECS083
00209  01  COMMON-TOTALS-3.                                             ECS083
00210      12  X-FILLER1.                                               ECS083
121610         16  X-FILLER3       OCCURS 15  TIMES.                    ECS083
00212              20  X-FILLER4   OCCURS 100 TIMES.                    ECS083
00213                  24  X-M-AMTS    PIC X(24).                       ECS083
00214      12  Y-FILLER1.                                               ECS083
121610         16  Y-FILLER3       OCCURS 15  TIMES.                    ECS083
00216              20  Y-FILLER4   OCCURS 100 TIMES.                    ECS083
00217                  24  Y-M-AMTS    PIC X(24).                       ECS083
00218                                                                   ECS083
00219                                                                   ECS083
00220  01  HEAD-1.                                                      ECS083
00221      12  FILLER              PIC X(42)           VALUE SPACES.    ECS083
00222      12  FILLER              PIC X(39)           VALUE            ECS083
00223              'UNEARNED PREMIUM AND COMMISSION EXTRACT'.           ECS083
00224      12  FILLER              PIC X(38)           VALUE SPACES.    ECS083
00225      12  FILLER              PIC X(7)            VALUE 'ECS083'.  ECS083
00226                                                                   ECS083
00227  01  HEAD-2.                                                      ECS083
00228      12  FILLER              PIC X(47)           VALUE SPACES.    ECS083
00229      12  HD-CLIENT           PIC X(30)           VALUE SPACES.    ECS083
00230      12  FILLER              PIC X(42)           VALUE SPACES.    ECS083
00231      12  HD-DATE             PIC X(8)            VALUE SPACES.    ECS083
00232                                                                   ECS083
00233  01  HEAD-3.                                                      ECS083
00234      12  FILLER              PIC X(53)           VALUE SPACES.    ECS083
00235      12  HD-ALF-DTE          PIC X(18)           VALUE SPACES.    ECS083
00236      12  FILLER              PIC X(48)           VALUE SPACES.    ECS083
00237      12  FILLER              PIC X(5)            VALUE 'PAGE'.    ECS083
00238      12  HD-PAGE             PIC ZZ,ZZZ.                          ECS083
00239                                                                   ECS083
00240      COPY ELCDTECX.                                               ECS083
00241                                                                   ECS083
00242      COPY ELCDTEVR.                                               ECS083
00243                                                                   ECS083
00244      COPY ELCGAPVR.                                               ECS083
00245  EJECT                                                            ECS083
00246  PROCEDURE DIVISION.                                              ECS083
00247                                                                   ECS083
00248  0100-SET-START.                                                  ECS083
00249                              COPY ELCDTERX.                       ECS083
00250                                                                   ECS083
00251  0110-OPEN-RTN.                                                   ECS083
00252      OPEN INPUT  GAAP-EXTR                                        ECS083
00253           OUTPUT REPTFL PRNTR.                                    ECS083
00254                                                                   ECS083
00255      PERFORM 0500-PRINT-HEADINGS THRU 0510-HEADING-EXIT.          ECS083
00256                                                                   ECS083
00257  0120-INTL-RTN.                                                   ECS083
00258                                                                   ECS083
00259      IF CLAS-MAXM = ZEROS                                         ECS083
00260          MOVE +1 TO CLAS-STARTM.                                  ECS083
00261                                                                   ECS083
00262      ADD +1 TO CLAS-MAXM.                                         ECS083
00263                                                                   ECS083
121610     IF CLAS-MAXM GREATER THAN +150                               ECS083
121610         MOVE +150 TO CLAS-MAXM.
CIDMOD                                                                  ECS083
CIDMOD*    IF CLAS-MAXM GREATER THAN +40                                ECS083
CIDMOD*        MOVE +40 TO CLAS-MAXM.                                   ECS083
00266                                                                   ECS083
00267      MOVE CLAS-MAXM TO X1.                                        ECS083
00268      MOVE SPACES    TO CLAS-MORT-CODE (X1).                       ECS083
00269      MOVE 'OTHERS'  TO CLAS-MORT-DESC (X1).                       ECS083
00270                                                                   ECS083
00271      MOVE HIGH-VALUES      TO XT-TABLE  YT-TABLE.                 ECS083
00272      PERFORM 0330-PRESS-RTN.                                      ECS083
00273                                                                   ECS083
00274      MOVE +0 TO X2.                                               ECS083
00275  0125-INTL-X2.                                                    ECS083
00276      ADD +1 TO X2.                                                ECS083
00277                                                                   ECS083
121610     IF X2 GREATER THAN +15                                       ECS083
00279          GO TO 0130-READ-RTN.                                     ECS083
00280                                                                   ECS083
00281      MOVE +0 TO X3.                                               ECS083
00282                                                                   ECS083
00283  0127-INTL-X3.                                                    ECS083
00284      ADD +1 TO X3.                                                ECS083
00285                                                                   ECS083
00286      IF X3 GREATER THAN MAX-BEN                                   ECS083
00287          GO TO 0125-INTL-X2.                                      ECS083
00288                                                                   ECS083
00289      MOVE X-DETL   TO X-AMTS (X2 X3)                              ECS083
00290                       Y-AMTS (X2 X3).                             ECS083
00291      MOVE X-M-DETL TO X-M-AMTS (X2 X3)                            ECS083
00292                       Y-M-AMTS (X2 X3).                           ECS083
00293                                                                   ECS083
00294      GO TO 0127-INTL-X3.                                          ECS083
00295  EJECT                                                            ECS083
00296  0130-READ-RTN.                                                   ECS083
00297      READ GAAP-EXTR AT END                                        ECS083
00298          GO TO 0520-END-INPUT.                                    ECS083
00299                                                                   ECS083
00300      IF GR-REIN = 'P' OR 'R'                                      ECS083
00301          NEXT SENTENCE                                            ECS083
00302      ELSE                                                         ECS083
00303         MOVE 0301 TO WS-RETURN-CODE                               ECS083
00304         MOVE ' INVALID GAAP RECORD ' TO WS-ABEND-MESSAGE          ECS083
00305         GO TO ABEND-PGM.                                          ECS083
00306                                                                   ECS083
00307      COPY ELCGAPM1.                                               ECS083
00308                                                                   ECS083
00309      MOVE GR-CARRIER   TO CUR-CARR.                               ECS083
00310      MOVE GR-GROUPING  TO CUR-CO.                                 ECS083
00311      MOVE GR-STATE     TO CUR-ST.                                 ECS083
00312                                                                   ECS083
00313      IF CUR-SEQ NOT = PRE-SEQ                                     ECS083
00314          PERFORM 0330-PRESS-RTN THRU 0380-PRESS-XIT.              ECS083
00315                                                                   ECS083
00316  0140-SET-X1.                                                     ECS083
00317      IF GR-REIN = 'P'                                             ECS083
00318          MOVE +1 TO X1                                            ECS083
00319      ELSE                                                         ECS083
00320          MOVE +2 TO X1.                                           ECS083
00321                                                                   ECS083
00322  0150-SET-X2.                                                     ECS083
121610     MOVE GR-CCYY TO X-YR
121610*    MOVE GR-CC TO X-YR(1:2).                                     ECS083
121610*    MOVE GR-YR TO X-YR(3:2).                                     ECS083
00325                                                                   ECS083
00326      COMPUTE X2 = (RUN-CCYY - X-YR) + +1.                         ECS083
00327                                                                   ECS083
121610     IF X2 GREATER THAN +15                                       ECS083
121610         MOVE +15 TO X2.                                          ECS083
00330                                                                   ECS083
00331      IF X2 LESS THAN +1                                           ECS083
00332          MOVE +1 TO X2.                                           ECS083
00333                                                                   ECS083
00334  0160-FIND-LIFE.                                                  ECS083
00335      IF GR-LFTYP = ZEROS                                          ECS083
00336          GO TO 0210-FIND-AH.                                      ECS083
00337                                                                   ECS083
00338      MOVE '1'         TO X-LAH.                                   ECS083
00339      MOVE GR-LFTYP    TO X-BEN.                                   ECS083
00340      MOVE CLAS-STARTM TO CLAS-INDEXM.                             ECS083
00341                                                                   ECS083
00342  0170-LOOP-CLAS-INDEXM.                                           ECS083
00343      IF CLAS-MORT-CODE (CLAS-INDEXM) = GR-MORT-CODE OR            ECS083
00344         CLAS-MORT-CODE (CLAS-INDEXM) = SPACES                     ECS083
00345          MOVE CLAS-INDEXM TO X-MORT                               ECS083
00346      ELSE                                                         ECS083
00347          ADD +1           TO CLAS-INDEXM                          ECS083
00348          GO TO 0170-LOOP-CLAS-INDEXM.                             ECS083
00349                                                                   ECS083
00350      IF GR-REIN = 'R'                                             ECS083
00351          MOVE GR-REIN-COMP TO X-REIN                              ECS083
00352      ELSE                                                         ECS083
00353          MOVE LOW-VALUE    TO X-REIN.                             ECS083
00354                                                                   ECS083
00355      PERFORM 0300-LOCATE-X3 THRU 0320-LOCATE-X3-XIT.              ECS083
00356                                                                   ECS083
00357      IF X1 = +1                                                   ECS083
00358          MOVE X-AMTS (X2 X3)   TO X-DETL                          ECS083
00359          MOVE X-M-AMTS (X2 X3) TO X-M-DETL                        ECS083
00360      ELSE                                                         ECS083
00361          MOVE Y-AMTS (X2 X3)   TO X-DETL                          ECS083
00362          MOVE Y-M-AMTS (X2 X3) TO X-M-DETL.                       ECS083
00363                                                                   ECS083
00364      IF GR-SUMMARY-REC                                            ECS083
00365         ADD GR-CNT-LF TO X-COUNT                                  ECS083
00366       ELSE                                                        ECS083
00367         ADD +1        TO X-COUNT.                                 ECS083
00368                                                                   ECS083
00369      ADD GR-LFPRM     TO X-WRITTEN.                               ECS083
00370      ADD GRR-LFPRM    TO X-P78.                                   ECS083
00371      ADD GRP-LFPRM    TO X-PRATA.                                 ECS083
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
040114        ADD GRD-LFPRM    TO X-DOMICILE
040114     ELSE
040114        ADD GRS-LFPRM    TO X-DOMICILE
040114     END-IF.
00373      ADD GRS-LFPRM    TO X-STATE.                                 ECS083
00374      ADD GR-LFCOM     TO X-PAID.                                  ECS083
00375      ADD GRR-LFCOM    TO X-C78.                                   ECS083
00376      ADD GRP-LFCOM    TO X-CRATA.                                 ECS083
040114     ADD GRS-LFCOM    TO X-CDOMI.
00377      ADD GR-LFTAX     TO X-TAX.                                   ECS083
00378      ADD GRR-LFTAX    TO X-T78.                                   ECS083
00379      ADD GRP-LFTAX    TO X-TRATA.                                 ECS083
040114     COMPUTE WRK-DOMI ROUNDED = 
040114                (GRS-LFPRM / GR-LFPRM) * GR-LFTAX.
040114     ADD WRK-DOMI     TO X-TDOMI.
00380                                                                   ECS083
00381      IF DTE-CLIENT = 'HER'                                        ECS083
00382          ADD GR-LFBEN     TO X-REMAIN                             ECS083
00383      ELSE                                                         ECS083
00384          ADD GR-REM-AMT   TO X-REMAIN.                            ECS083
00385                                                                   ECS083
00386      ADD GR-RESV      TO X-RESERV.                                ECS083
00387      ADD GR-ALT-RESV  TO X-ALTRSV.                                ECS083
00388                                                                   ECS083
00389      IF GR-IG = '1'                                               ECS083
00390          GO TO 0190-LIFE-IND.                                     ECS083
00391                                                                   ECS083
00392  0180-LIFE-GRP.                                                   ECS083
00393      IF GR-LF-TERM LESS THAN +121                                 ECS083
00394         ADD GR-RESV TO X-GUNDR                                    ECS083
00395      ELSE                                                         ECS083
00396         ADD GR-RESV TO X-GOVER.                                   ECS083
00397                                                                   ECS083
00398      GO TO 0200-END-LIFE.                                         ECS083
00399                                                                   ECS083
00400  0190-LIFE-IND.                                                   ECS083
00401      IF GR-LF-TERM LESS THAN +121                                 ECS083
00402         ADD GR-RESV TO X-IUNDR                                    ECS083
00403      ELSE                                                         ECS083
00404         ADD GR-RESV TO X-IOVER.                                   ECS083
00405                                                                   ECS083
00406  0200-END-LIFE.                                                   ECS083
00407      IF GR-AHTYP NOT = ZEROS                                      ECS083
00408          IF GR-SUMMARY-REC                                        ECS083
00409              NEXT SENTENCE                                        ECS083
00410           ELSE                                                    ECS083
00411              ADD +1 TO X-DUP.                                     ECS083
00412                                                                   ECS083
00413      IF X1 = +1                                                   ECS083
00414          MOVE X-DETL   TO X-AMTS (X2 X3)                          ECS083
00415          MOVE X-M-DETL TO X-M-AMTS (X2 X3)                        ECS083
00416      ELSE                                                         ECS083
00417          MOVE X-DETL   TO Y-AMTS (X2 X3)                          ECS083
00418          MOVE X-M-DETL TO Y-M-AMTS (X2 X3).                       ECS083
00419                                                                   ECS083
00420  0210-FIND-AH.                                                    ECS083
00421      IF GR-AHTYP = ZEROS                                          ECS083
00422          GO TO 0130-READ-RTN.                                     ECS083
00423                                                                   ECS083
00424      MOVE '2'      TO X-LAH.                                      ECS083
00425      MOVE GR-AHTYP TO X-BEN.                                      ECS083
00426      MOVE ZEROS    TO X-SEQ2.                                     ECS083
00427                                                                   ECS083
00428      IF GR-REIN = 'R'                                             ECS083
00429          MOVE GR-REIN-COMP TO X-REIN                              ECS083
00430      ELSE                                                         ECS083
00431          MOVE LOW-VALUE    TO X-REIN.                             ECS083
00432                                                                   ECS083
00433      PERFORM 0300-LOCATE-X3 THRU 0320-LOCATE-X3-XIT.              ECS083
00434                                                                   ECS083
00435      IF X1 = +1                                                   ECS083
00436          MOVE X-AMTS (X2 X3)   TO X-DETL                          ECS083
00437          MOVE X-M-AMTS (X2 X3) TO X-M-DETL                        ECS083
00438      ELSE                                                         ECS083
00439          MOVE Y-AMTS (X2 X3)   TO X-DETL                          ECS083
00440          MOVE Y-M-AMTS (X2 X3) TO X-M-DETL.                       ECS083
00441                                                                   ECS083
00442      IF GR-SUMMARY-REC                                            ECS083
00443         ADD GR-CNT-AH TO X-COUNT                                  ECS083
00444       ELSE                                                        ECS083
00445         ADD +1        TO X-COUNT.                                 ECS083
00446                                                                   ECS083
00447      ADD GR-AHPRM     TO X-WRITTEN.                               ECS083
00448      ADD GRR-AHPRM    TO X-P78.                                   ECS083
00449      ADD GRP-AHPRM    TO X-PRATA.                                 ECS083
010716     IF DTE-CLIENT = 'DCC' or 'VPP'
040114        ADD GRD-AHPRM    TO X-DOMICILE
040114     ELSE
040114        COMPUTE WRK-DOMI ROUNDED = (GRR-AHPRM + GRP-AHPRM) / 2
040114        ADD WRK-DOMI     TO X-DOMICILE
040114     END-IF.
120816     compute x-state = x-state +
120816        grs-ahprm + gr-loaded-stat-uep
00452      ADD GR-AHCOM     TO X-PAID.                                  ECS083
00453      ADD GRR-AHCOM    TO X-C78.                                   ECS083
00454      ADD GRP-AHCOM    TO X-CRATA.                                 ECS083
040114     COMPUTE WRK-DOMI ROUNDED = (GRR-AHCOM + GRP-AHCOM) / 2.
040114     ADD WRK-DOMI     TO X-CDOMI.
00455      ADD GR-AHTAX     TO X-TAX.                                   ECS083
00456      ADD GRR-AHTAX    TO X-T78.                                   ECS083
00457      ADD GRP-AHTAX    TO X-TRATA.                                 ECS083
040114     COMPUTE WRK-DOMI ROUNDED = (GRR-AHTAX + GRP-AHTAX) / 2.
040114     ADD WRK-DOMI     TO X-TDOMI.
00458                                                                   ECS083
00459      IF DTE-CLIENT = 'HER'                                        ECS083
00460          COMPUTE X-REMAIN ROUNDED EQUAL                           ECS083
00461                  (GR-AHBEN * GR-AH-TERM) + X-REMAIN               ECS083
00462      ELSE                                                         ECS083
00463          COMPUTE X-REMAIN ROUNDED EQUAL                           ECS083
00464                  (GR-AHBEN * GR-AH-REMTERM) + X-REMAIN.           ECS083
00465                                                                   ECS083
00466      IF X1 = +1                                                   ECS083
00467          MOVE X-DETL   TO X-AMTS (X2 X3)                          ECS083
00468          MOVE X-M-DETL TO X-M-AMTS (X2 X3)                        ECS083
00469      ELSE                                                         ECS083
00470          MOVE X-DETL   TO Y-AMTS (X2 X3)                          ECS083
00471          MOVE X-M-DETL TO Y-M-AMTS (X2 X3).                       ECS083
00472                                                                   ECS083
00473      GO TO 0130-READ-RTN.                                         ECS083
00474                                                                   ECS083
00475  EJECT                                                            ECS083
00476  0300-LOCATE-X3.                                                  ECS083
00477      MOVE +0 TO X3.                                               ECS083
00478                                                                   ECS083
00479  0310-LOOP-LX3.                                                   ECS083
00480      ADD +1 TO X3.                                                ECS083
00481                                                                   ECS083
00482      IF X3 GREATER THAN MAX-BEN                                   ECS083
00483          PERFORM 0390-PRESS-ONE-RTN THRU 0420-PRESS-ONE-XIT.      ECS083
00484                                                                   ECS083
00485      IF X1 = +1                                                   ECS083
00486        IF XT-SEQ (X2 X3) = HIGH-VALUE  OR                         ECS083
00487           XT-SEQ (X2 X3) = X-SEQ                                  ECS083
00488            MOVE X-SEQ TO XT-SEQ (X2 X3)                           ECS083
00489            GO TO 0320-LOCATE-X3-XIT.                              ECS083
00490                                                                   ECS083
00491      IF X1 = +2                                                   ECS083
00492        IF YT-SEQ (X2 X3) = HIGH-VALUE  OR                         ECS083
00493           YT-SEQ (X2 X3) = X-SEQ                                  ECS083
00494            MOVE X-SEQ TO YT-SEQ (X2 X3)                           ECS083
00495            GO TO 0320-LOCATE-X3-XIT.                              ECS083
00496                                                                   ECS083
00497      GO TO 0310-LOOP-LX3.                                         ECS083
00498                                                                   ECS083
00499  0320-LOCATE-X3-XIT.                                              ECS083
00500      EXIT.                                                        ECS083
00501                                                                   ECS083
00502  0330-PRESS-RTN.                                                  ECS083
00503      MOVE +0 TO X1                                                ECS083
00504                 X-DUP                                             ECS083
00505                 X-COUNT                                           ECS083
00506                 X-WRITTEN                                         ECS083
00507                 X-P78                                             ECS083
00508                 X-PRATA                                           ECS083
00509                 X-DOMICILE                                        ECS083
00510                 X-STATE                                           ECS083
00511                 X-RESERV                                          ECS083
00512                 X-ALTRSV                                          ECS083
00513                 X-REMAIN                                          ECS083
00514                 X-PAID                                            ECS083
00515                 X-C78                                             ECS083
00516                 X-CRATA                                           ECS083
040114                X-CDOMI
00517                 X-TAX                                             ECS083
00518                 X-T78                                             ECS083
00519                 X-TRATA                                           ECS083
040114                X-TDOMI
00520                 X-IUNDR                                           ECS083
00521                 X-IOVER                                           ECS083
00522                 X-GUNDR                                           ECS083
00523                 X-GOVER.                                          ECS083
00524                                                                   ECS083
00525  0340-PRESS-LOOP-X1.                                              ECS083
00526      ADD +1 TO X1.                                                ECS083
00527                                                                   ECS083
00528      IF X1 GREATER THAN +2                                        ECS083
00529          GO TO 0370-INTL-PRESS.                                   ECS083
00530                                                                   ECS083
00531      MOVE +0 TO X2.                                               ECS083
00532                                                                   ECS083
00533  0350-PRESS-LOOP-X2.                                              ECS083
00534      ADD +1 TO X2.                                                ECS083
00535                                                                   ECS083
121610     IF X2 GREATER THAN +15                                       ECS083
00537          GO TO 0340-PRESS-LOOP-X1.                                ECS083
00538                                                                   ECS083
00539      MOVE +0 TO X3.                                               ECS083
00540                                                                   ECS083
00541  0360-PRESS-LOOP-X3.                                              ECS083
00542      ADD +1 TO X3.                                                ECS083
00543                                                                   ECS083
00544      IF X3 GREATER THAN MAX-BEN                                   ECS083
00545          GO TO 0350-PRESS-LOOP-X2.                                ECS083
00546                                                                   ECS083
00547      IF X1 = +1                                                   ECS083
00548        IF XT-SEQ (X2 X3) = HIGH-VALUE                             ECS083
00549            GO TO 0350-PRESS-LOOP-X2                               ECS083
00550        ELSE                                                       ECS083
00551            MOVE X-AMTS (X2 X3)       TO W-AMTS                    ECS083
00552            IF W-COUNT NOT = +0                                    ECS083
00553                MOVE XT-SEQ (X2 X3)   TO W-SEQ3                    ECS083
00554                MOVE X-M-AMTS (X2 X3) TO W-M-AMTS                  ECS083
00555                MOVE XT-SEQ3 (X2 X3)  TO PRE-REIN                  ECS083
00556            ELSE                                                   ECS083
00557                GO TO 0365-ZERO-PRESS.                             ECS083
00558                                                                   ECS083
00559      IF X1 = +2                                                   ECS083
00560        IF YT-SEQ (X2 X3) = HIGH-VALUE                             ECS083
00561            GO TO 0350-PRESS-LOOP-X2                               ECS083
00562        ELSE                                                       ECS083
00563            MOVE Y-AMTS (X2 X3)       TO W-AMTS                    ECS083
00564            IF W-COUNT NOT = +0                                    ECS083
00565                MOVE YT-SEQ (X2 X3)   TO W-SEQ3                    ECS083
00566                MOVE Y-M-AMTS (X2 X3) TO W-M-AMTS                  ECS083
00567                MOVE YT-SEQ3 (X2 X3)  TO PRE-REIN                  ECS083
00568            ELSE                                                   ECS083
00569                GO TO 0365-ZERO-PRESS.                             ECS083
00570                                                                   ECS083
00571      MOVE LOW-VALUE TO W-REIN.                                    ECS083
00572      MOVE PRE-SEQ   TO W-SEQ1.                                    ECS083
00573      MOVE X1        TO W-CODE.                                    ECS083
00574      MOVE X2        TO W-YR.                                      ECS083
00575                                                                   ECS083
00576      PERFORM 0430-GEN-RTN THRU 0450-GEN-XIT.                      ECS083
00577                                                                   ECS083
00578  0365-ZERO-PRESS.                                                 ECS083
00579                                                                   ECS083
00580      IF X1 = +1                                                   ECS083
00581          MOVE HIGH-VALUE TO XT-SEQ (X2 X3)                        ECS083
00582          MOVE X-DETL     TO X-AMTS (X2 X3)                        ECS083
00583          MOVE X-M-DETL   TO X-M-AMTS (X2 X3)                      ECS083
00584      ELSE                                                         ECS083
00585          MOVE HIGH-VALUE TO YT-SEQ (X2 X3)                        ECS083
00586          MOVE X-DETL     TO Y-AMTS (X2 X3)                        ECS083
00587          MOVE X-M-DETL   TO Y-M-AMTS (X2 X3).                     ECS083
00588                                                                   ECS083
00589      GO TO 0360-PRESS-LOOP-X3.                                    ECS083
00590                                                                   ECS083
00591  0370-INTL-PRESS.                                                 ECS083
00592      MOVE CUR-SEQ TO PRE-SEQ.                                     ECS083
00593                                                                   ECS083
00594  0380-PRESS-XIT.                                                  ECS083
00595      EXIT.                                                        ECS083
00596                                                                   ECS083
00597                                                                   ECS083
00598  0390-PRESS-ONE-RTN.                                              ECS083
00599      MOVE +0 TO X3                                                ECS083
00600                 X-DUP                                             ECS083
00601                 X-COUNT                                           ECS083
00602                 X-WRITTEN                                         ECS083
00603                 X-P78                                             ECS083
00604                 X-PRATA                                           ECS083
00605                 X-DOMICILE                                        ECS083
00606                 X-STATE                                           ECS083
00607                 X-RESERV                                          ECS083
00608                 X-ALTRSV                                          ECS083
00609                 X-REMAIN                                          ECS083
00610                 X-PAID                                            ECS083
00611                 X-C78                                             ECS083
00612                 X-CRATA                                           ECS083
040114                X-CDOMI
00613                 X-TAX                                             ECS083
00614                 X-T78                                             ECS083
00615                 X-TRATA                                           ECS083
040114                X-TDOMI
00616                 X-IUNDR                                           ECS083
00617                 X-IOVER                                           ECS083
00618                 X-GUNDR                                           ECS083
00619                 X-GOVER.                                          ECS083
00620                                                                   ECS083
00621  0400-PRESS-ONE-LOOP.                                             ECS083
00622      ADD +1 TO X3.                                                ECS083
00623                                                                   ECS083
00624      IF X3 GREATER THAN MAX-BEN                                   ECS083
00625          GO TO 0410-PRESS-ONE-END.                                ECS083
00626                                                                   ECS083
00627      MOVE LOW-VALUE TO W-REIN.                                    ECS083
00628      MOVE PRE-SEQ   TO W-SEQ1.                                    ECS083
00629      MOVE X1        TO W-CODE.                                    ECS083
00630      MOVE X2        TO W-YR.                                      ECS083
00631                                                                   ECS083
00632      IF X1 = +1                                                   ECS083
00633          MOVE XT-SEQ (X2 X3)   TO W-SEQ3                          ECS083
00634          MOVE X-AMTS (X2 X3)   TO W-AMTS                          ECS083
00635          MOVE X-M-AMTS (X2 X3) TO W-M-AMTS                        ECS083
00636          MOVE XT-SEQ3 (X2 X3)  TO PRE-REIN                        ECS083
00637      ELSE                                                         ECS083
00638          MOVE YT-SEQ (X2 X3)   TO W-SEQ3                          ECS083
00639          MOVE Y-AMTS (X2 X3)   TO W-AMTS                          ECS083
00640          MOVE Y-M-AMTS (X2 X3) TO W-M-AMTS                        ECS083
00641          MOVE YT-SEQ3 (X2 X3)  TO PRE-REIN.                       ECS083
00642                                                                   ECS083
00643      PERFORM 0430-GEN-RTN THRU 0450-GEN-XIT.                      ECS083
00644                                                                   ECS083
00645      IF X1 = +1                                                   ECS083
00646          MOVE HIGH-VALUE TO XT-SEQ (X2 X3)                        ECS083
00647          MOVE X-DETL     TO X-AMTS (X2 X3)                        ECS083
00648          MOVE X-M-DETL   TO X-M-AMTS (X2 X3)                      ECS083
00649      ELSE                                                         ECS083
00650          MOVE HIGH-VALUE TO YT-SEQ (X2 X3)                        ECS083
00651          MOVE X-DETL     TO Y-AMTS (X2 X3)                        ECS083
00652          MOVE X-M-DETL   TO Y-M-AMTS (X2 X3).                     ECS083
00653                                                                   ECS083
00654      GO TO 0400-PRESS-ONE-LOOP.                                   ECS083
00655                                                                   ECS083
00656  0410-PRESS-ONE-END.                                              ECS083
00657      MOVE +1 TO X3.                                               ECS083
00658                                                                   ECS083
00659  0420-PRESS-ONE-XIT.                                              ECS083
00660      EXIT.                                                        ECS083
00661                                                                   ECS083
00662                                                                   ECS083
00663  0430-GEN-RTN.                                                    ECS083
00664 **   COMPUTE W-YR = (RUN-CC - W-YR) + 1.                          ECS083
121610     COMPUTE WORK-YR = (RUN-CCYY - W-YR) + 1.
00665                                                                      CL**2
00666 **   IF W-YR < 0                                                  ECS083
00667 **      ADD 100 TO W-YR.                                             CL**2
CIDMOD     IF WORK-YR < 0                                               ECS083
CIDMOD        ADD 100 TO WORK-YR.                                          CL**2
CIDMOD     MOVE WORK-YR TO W-YR.                                        ECS083
00668                                                                   ECS083
00669  0440-GEN-LOOP.                                                   ECS083
00670      IF DTE-PGM-OPT NOT GREATER THAN 1                            ECS083
00671          PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  ECS083
00672                                                                   ECS083
00673      MOVE HIGH-VALUE TO W-ST.                                     ECS083
00674                                                                   ECS083
00675      IF DTE-PGM-OPT NOT GREATER THAN 2                            ECS083
00676          PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  ECS083
00677                                                                   ECS083
00678      MOVE HIGH-VALUE TO W-CO.                                     ECS083
00679                                                                   ECS083
00680      IF DTE-PGM-OPT NOT GREATER THAN 4                            ECS083
00681          PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  ECS083
00682                                                                   ECS083
00683      MOVE HIGH-VALUE TO W-CARR.                                   ECS083
00684                                                                   ECS083
00685      IF DTE-PGM-OPT NOT GREATER THAN 6                            ECS083
00686          PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  ECS083
00687                                                                   ECS083
00688      MOVE PRE-SEQ    TO W-SEQ1.                                   ECS083
00689      MOVE HIGH-VALUE TO W-CO.                                     ECS083
00690                                                                   ECS083
00691      IF DTE-PGM-OPT NOT GREATER THAN 3                            ECS083
00692          PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  ECS083
00693                                                                   ECS083
00694      MOVE HIGH-VALUE TO W-CARR.                                   ECS083
00695                                                                   ECS083
00696      IF DTE-PGM-OPT NOT GREATER THAN 5                            ECS083
00697          PERFORM 0460-RLS-RTN THRU 0470-RLS-XIT.                  ECS083
00698                                                                   ECS083
00699      IF W-REIN NOT = PRE-REIN                                     ECS083
00700          MOVE PRE-SEQ  TO W-SEQ1                                  ECS083
00701          MOVE PRE-REIN TO W-REIN                                  ECS083
00702          GO TO 0440-GEN-LOOP.                                     ECS083
00703                                                                   ECS083
00704  0450-GEN-XIT.                                                    ECS083
00705      EXIT.                                                        ECS083
00706                                                                   ECS083
00707  0460-RLS-RTN.                                                    ECS083
00708      MOVE WORK-REC TO RPT-REC.                                    ECS083
00709                                                                   ECS083
00710      WRITE RPT-REC.                                               ECS083
00711                                                                   ECS083
00712  0470-RLS-XIT.                                                    ECS083
00713      EXIT.                                                        ECS083
00714  EJECT                                                            ECS083
00715  0480-PRT-RTN.                                                    ECS083
00716                              COPY ELCPRT2.                        ECS083
00717                                                                   ECS083
00718  0490-E-PRT-RTN.                                                  ECS083
00719      EXIT.                                                        ECS083
00720                                                                   ECS083
00721  0500-PRINT-HEADINGS.                                             ECS083
00722      MOVE '1'    TO X.                                            ECS083
00723      MOVE HEAD-1 TO P-DATA.                                       ECS083
00724      PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    ECS083
00725                                                                   ECS083
00726      MOVE ' '             TO X.                                   ECS083
00727      MOVE WS-CURRENT-DATE TO HD-DATE.                             ECS083
00728      MOVE COMPANY-NAME    TO HD-CLIENT.                           ECS083
00729      MOVE HEAD-2          TO P-DATA.                              ECS083
00730      PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    ECS083
00731                                                                   ECS083
00732      MOVE 1         TO HD-PAGE.                                   ECS083
00733      MOVE ALPH-DATE TO HD-ALF-DTE.                                ECS083
00734      MOVE HEAD-3    TO P-DATA.                                    ECS083
00735      PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    ECS083
00736                                                                   ECS083
00737  0510-HEADING-EXIT.                                               ECS083
00738      EXIT.                                                        ECS083
00739                                                                   ECS083
00740  0520-END-INPUT.                                                  ECS083
00741      PERFORM 0330-PRESS-RTN THRU 0380-PRESS-XIT.                  ECS083
00742                                                                   ECS083
00743      CLOSE GAAP-EXTR REPTFL.                                      ECS083
00744                                                                   ECS083
00745      MOVE '0' TO X.                                               ECS083
00746      MOVE 'UNEARNED PREMIUM AND COMMMISON EXTRACT COMPLETED'      ECS083
00747               TO P-DATA.                                          ECS083
00748      PERFORM 0480-PRT-RTN THRU 0490-E-PRT-RTN.                    ECS083
00749                                                                   ECS083
00750  0530-CLOSE-FICH.                                                 ECS083
00751                              COPY ELCPRTC.                        ECS083
00752                                                                   ECS083
00753  0540-CLOSE-PRINTER.                                              ECS083
00754      CLOSE PRNTR.                                                 ECS083
00755      GOBACK.                                                      ECS083
00756                                                                   ECS083
00757  ABEND-PGM.                                                       ECS083
00758                      COPY ELCABEND.                               ECS083
00759      EJECT                                                        ECS083
