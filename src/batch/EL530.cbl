00001  IDENTIFICATION DIVISION.                                         04/18/98
00002                                                                   EL530
00003  PROGRAM-ID.               EL530 .                                   LV004
00004 *              PROGRAM CONVERTED BY                               EL530
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL530
00006 *              CONVERSION DATE 04/10/96 10:13:13.                 EL530
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL530
00008 *                         VMOD=2.013.                                CL**3
00009                                                                   EL530
00010 *AUTHOR.     LOGIC INC.                                           EL530
00011 *            DALLAS, TEXAS.                                       EL530
00012                                                                   EL530
00013 *DATE-COMPILED.                                                   EL530
00014                                                                   EL530
00015 *SECURITY.   *****************************************************EL530
00016 *            *                                                   *EL530
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL530
00018 *            *                                                   *EL530
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL530
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL530
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL530
00022 *            *                                                   *EL530
00023 *            *****************************************************EL530
00024                                                                   EL530
00025 *REMARKS.                                                         EL530
00026 *       GENERAL FUNCTION IS TO PURGE OLD RECORDS FROM THE         EL530
00027 *       CERT CHANGE, PENDING BUSINESS, PENDING CLAIMS, AND        EL530
00028 *       PAYMENTS/ADJUSTMENTS FILE, A/R REQUEST. A RECORD IS       EL530
00029 *       PURGED IF THE CREDIT ACCEPT DATE IS NOT LOW-VALUES.       EL530
00030                                                                   EL530
00031 *       INPUT FILES-    PENDING BUSINESS (ISSUES AND CANCELS)     EL530
00032 *                       CERT CHANGES                              EL530
00033 *                       PENDING CLAIMS                            EL530
00034 *                       PAYMENTS AND ADJUSTMENTS                  EL530
00035 *                       A/R REQUEST                               EL530
00036 *                       BILLING STATEMENTS                        EL530
00037 *                       PENDING RETRO/REIN ADJUSTMENTS            EL530
00038 *                       CHEK FILE                                 EL530
00039 *                       CHKQ FILE                                 EL530
00040 *                       CMKQ FILE                                 EL530
00041 *                       CONTROL FILE                              EL530
00042                                                                   EL530
00043 *       OUTPUT- PURGE         COUNT REPORT                        EL530
00044 *                       PENDING BUSINESS                          EL530
00045 *                       CERT CHANGES                              EL530
00046 *                       PENDING CLAIMS                            EL530
00047 *                       PAYMENTS AND ADJUSTMENTS                  EL530
00048 *                       PENDING RETRO/REIN ADJUSTMENTS            EL530
00049 *                       PENDING MAIL DATA                         EL530
00050 *                       A/R REQUEST                               EL530
00051                                                                   EL530
022614******************************************************************
022614*                   C H A N G E   L O G
022614*
022614* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
022614*-----------------------------------------------------------------
022614*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
022614* EFFECTIVE    NUMBER
022614*-----------------------------------------------------------------
022614* 022614  IR2014022600001  PEMA  BYPASS ERCHEK&ERCHKQ DELETES
022614******************************************************************
00053  ENVIRONMENT DIVISION.                                            EL530
00054  CONFIGURATION SECTION.                                           EL530
00055  SPECIAL-NAMES.                                                   EL530
00056      C02 IS LCP-CH2                                               EL530
00057      C03 IS LCP-CH3                                               EL530
00058      C04 IS LCP-CH4                                               EL530
00059      C05 IS LCP-CH5                                               EL530
00060      C06 IS LCP-CH6                                               EL530
00061      C07 IS LCP-CH7                                               EL530
00062      C08 IS LCP-CH8                                               EL530
00063      C09 IS LCP-CH9                                               EL530
00064      C10 IS LCP-CH10                                              EL530
00065      C11 IS LCP-CH11                                              EL530
00066      C12 IS LCP-CH12                                              EL530
00067      S01 IS LCP-P01                                               EL530
00068      S02 IS LCP-P02.                                              EL530
00069                                                                   EL530
00070  INPUT-OUTPUT SECTION.                                            EL530
00071                                                                   EL530
00072  FILE-CONTROL.                                                    EL530
00073                                                                   EL530
00074      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL530
00075                                                                   EL530
00076      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL530
00077                                                                   EL530
00078      SELECT ELCNTL           ASSIGN TO SYS021-FBA1-ELCNTL         EL530
00079                              ORGANIZATION IS INDEXED              EL530
00080                              ACCESS IS DYNAMIC                    EL530
00081                              RECORD KEY IS CF-CONTROL-PRIMARY     EL530
00082                              FILE STATUS IS ELCNTL-FILE-STATUS.   EL530
00083                                                                   EL530
00084      SELECT ERPNDB           ASSIGN TO SYS021-FBA1-ERPNDB         EL530
00085                              ORGANIZATION IS INDEXED              EL530
00086                              ACCESS IS DYNAMIC                    EL530
00087                              RECORD KEY IS PB-CONTROL-PRIMARY     EL530
00088                              FILE STATUS IS ERPNDB-FILE-STATUS.   EL530
00089                                                                   EL530
00090      SELECT ERCRTC           ASSIGN TO SYS022-FBA1-ERCRTC         EL530
00091                              ORGANIZATION IS INDEXED              EL530
00092                              ACCESS IS DYNAMIC                    EL530
00093                              RECORD KEY IS CC-CONTROL-PRIMARY     EL530
00094                              FILE STATUS IS ERCRTC-FILE-STATUS.   EL530
00095                                                                   EL530
083102*    SELECT ERNOTE           ASSIGN TO SYS022-FBA1-ERNOTE         EL530
083102*                            ORGANIZATION IS INDEXED              EL530
083102*                            ACCESS IS DYNAMIC                    EL530
083102*                            RECORD KEY IS CN-CONTROL-PRIMARY     EL530
083102*                            FILE STATUS IS ERNOTE-FILE-STATUS.   EL530
00101                                                                   EL530
00102      SELECT ERPNDC           ASSIGN TO SYS023-FBA1-ERPNDC         EL530
00103                              ORGANIZATION IS INDEXED              EL530
00104                              ACCESS IS DYNAMIC                    EL530
00105                              RECORD KEY IS PC-CONTROL-PRIMARY     EL530
00106                              FILE STATUS IS ERPNDC-FILE-STATUS.   EL530
00107                                                                   EL530
00108      SELECT ERPYAJ           ASSIGN TO SYS024-FBA1-ERPYAJ         EL530
00109                              ORGANIZATION IS INDEXED              EL530
00110                              ACCESS IS DYNAMIC                    EL530
00111                              RECORD KEY IS PY-CONTROL-PRIMARY     EL530
00112                              FILE STATUS IS ERPYAJ-FILE-STATUS.   EL530
00113                                                                   EL530
00114      SELECT ERBILL           ASSIGN TO SYS026-FBA1-ERBILL         EL530
00115                              ORGANIZATION IS INDEXED              EL530
00116                              ACCESS IS DYNAMIC                    EL530
00117                              RECORD KEY IS BI-CONTROL-PRIMARY     EL530
00118                              FILE STATUS IS ERBILL-FILE-STATUS.   EL530
00119                                                                   EL530
00120      SELECT ERREPY           ASSIGN TO SYS027-FBA1-ERREPY         EL530
00121                              ORGANIZATION IS INDEXED              EL530
00122                              ACCESS IS DYNAMIC                    EL530
00123                              RECORD KEY IS RP-CONTROL-PRIMARY     EL530
00124                              FILE STATUS IS ERREPY-FILE-STATUS.   EL530
00125                                                                   EL530
00126      SELECT ERCHEK           ASSIGN TO SYS028-FBA1-ERCHEK         EL530
00127                              ORGANIZATION IS INDEXED              EL530
00128                              ACCESS IS DYNAMIC                    EL530
00129                              RECORD KEY IS CH-CONTROL-PRIMARY     EL530
00130                              FILE STATUS IS ERCHEK-FILE-STATUS.   EL530
00131                                                                   EL530
00132      SELECT ERCHKQ           ASSIGN TO SYS029-FBA1-ERCHKQ         EL530
00133                              ORGANIZATION IS INDEXED              EL530
00134                              ACCESS IS DYNAMIC                    EL530
00135                              RECORD KEY IS CQ-CONTROL-PRIMARY     EL530
00136                              FILE STATUS IS ERCHKQ-FILE-STATUS.   EL530
00137                                                                   EL530
00138      SELECT ERCMKQ           ASSIGN TO SYS030-FBA1-ERCMKQ         EL530
00139                              ORGANIZATION IS INDEXED              EL530
00140                              ACCESS IS DYNAMIC                    EL530
00141                              RECORD KEY IS MQ-CONTROL-PRIMARY     EL530
00142                              FILE STATUS IS ERCMKQ-FILE-STATUS.   EL530
00143                                                                   EL530
00144      SELECT ERRQST           ASSIGN TO SYS031-FBA1-ERRQST         EL530
00145                              ORGANIZATION IS INDEXED              EL530
00146                              ACCESS IS DYNAMIC                    EL530
00147                              RECORD KEY IS RQ-CONTROL-PRIMARY     EL530
00148                              FILE STATUS IS ERRQST-FILE-STATUS.   EL530
00149                                                                   EL530
00150      SELECT ERPNDM           ASSIGN TO SYS032-FBA1-ERPNDM         EL530
00151                              ORGANIZATION IS INDEXED              EL530
00152                              ACCESS IS RANDOM                     EL530
00153                              RECORD KEY IS PM-CONTROL-PRIMARY     EL530
00154                              FILE STATUS IS ERPNDM-FILE-STATUS.   EL530
00155                                                                   EL530
00156  DATA DIVISION.                                                   EL530
00157  FILE SECTION.                                                    EL530
00158                                                                   EL530
00159  FD  PRNTR                           COPY ELCPRTFD.               EL530
00160                                                                   EL530
00161  FD  DISK-DATE                       COPY ELCDTEFD.               EL530
00162                                                                   EL530
00163  FD  ELCNTL.                                                      EL530
00164                                                                   EL530
00165      COPY ELCCNTL.                                                EL530
00166      EJECT                                                        EL530
00167                                                                   EL530
00168  FD  ERPNDB.                                                      EL530
00169                                                                   EL530
00170      COPY ERCPNDB.                                                EL530
00171      EJECT                                                        EL530
00172                                                                   EL530
00173  FD  ERCRTC.                                                      EL530
00174                                                                   EL530
00175      COPY ERCCRTC.                                                EL530
00176      EJECT                                                        EL530
00177                                                                   EL530
083102*FD  ERNOTE.                                                      EL530
083102*                                                                 EL530
083102*    COPY ERCNOTE.                                                EL530
083102*    EJECT                                                        EL530
00182                                                                   EL530
00183  FD  ERPNDC.                                                      EL530
00184                                                                   EL530
00185      COPY ERCPNDC.                                                EL530
00186      EJECT                                                        EL530
00187                                                                   EL530
00188  FD  ERPYAJ.                                                      EL530
00189                                                                   EL530
00190      COPY ERCPYAJ.                                                EL530
00191      EJECT                                                        EL530
00192                                                                   EL530
00193  FD  ERBILL.                                                      EL530
00194                                                                   EL530
00195      COPY ERCBILL.                                                EL530
00196      EJECT                                                        EL530
00197                                                                   EL530
00198  FD  ERREPY.                                                      EL530
00199                                                                   EL530
00200      COPY ERCREPY.                                                EL530
00201                                                                   EL530
00202      EJECT                                                        EL530
00203                                                                   EL530
00204  FD  ERCHEK.                                                      EL530
00205                                                                   EL530
00206      COPY ERCCHEK.                                                EL530
00207                                                                   EL530
00208      EJECT                                                        EL530
00209                                                                   EL530
00210  FD  ERCHKQ.                                                      EL530
00211                                                                   EL530
00212      COPY ERCCHKQ.                                                EL530
00213                                                                   EL530
00214      EJECT                                                        EL530
00215                                                                   EL530
00216  FD  ERCMKQ.                                                      EL530
00217                                                                   EL530
00218      COPY ERCCMKQ.                                                EL530
00219                                                                   EL530
00220      EJECT                                                        EL530
00221                                                                   EL530
00222  FD  ERPNDM.                                                      EL530
00223                                                                   EL530
00224      COPY ERCPNDM.                                                EL530
00225                                                                   EL530
00226      EJECT                                                        EL530
00227  FD  ERRQST.                                                      EL530
00228                                                                   EL530
00229      COPY ERCRQST.                                                EL530
00230                                                                   EL530
00231      EJECT                                                        EL530
00232  WORKING-STORAGE SECTION.                                         EL530
00233  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL530
00234  77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.   EL530
00235  77  LCP-ASA                       PIC X.                         EL530
00236                                                                   EL530
00237  77  FILLER  PIC X(32) VALUE '********************************'.  EL530
00238  77  FILLER  PIC X(32) VALUE '      EL530 WORKING-STORAGE     '.  EL530
00239  77  FILLER  PIC X(32) VALUE '******** VMOD=2.013 ************'.     CL**3
00240                                                                   EL530
00241  01  FILLER                COMP-3.                                EL530
00242      05  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL530
00243      05  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL530
00244      05  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +58.       EL530
00245      05  WS-ZERO                     PIC S9      VALUE ZERO.      EL530
00246      05  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL530
00247      05  BATCH-COUNT                 PIC S9(5)   VALUE ZEROS.     EL530
00248      05  ERPNDB-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00249      05  ERPNDM-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00250      05  ERCRTC-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
083102*    05  ERNOTE-RECORDS-REWRITES     PIC S9(7)   VALUE ZEROS.     EL530
00252      05  ERPNDC-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00253      05  ERPYAJ-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00254      05  ERBILL-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00255      05  ERREPY-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00256      05  ERCHEK-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00257      05  ERCHKQ-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00258      05  ERCMKQ-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00259      05  ERRQST-RECORDS-PURGED       PIC S9(7)   VALUE ZEROS.     EL530
00260                                                                   EL530
00261      EJECT                                                        EL530
00262  01  FILLER.                                                      EL530
00263      05  FINAL-TOTALS-SW             PIC X       VALUE SPACE.     EL530
00264          88  FINAL-TOTALS                        VALUE 'Y'.       EL530
00265      05  SAVE-BATCH-NO               PIC X(6)    VALUE SPACES.    EL530
00266      05  ERBILL-COMP-KEY             PIC X(17)   VALUE SPACES.    EL530
00267      05  ERBILL-SAVE-KEY             PIC X(17)   VALUE SPACES.    EL530
00268      05  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL530
00269      05  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL530
00270      05  ELCNTL-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00271      05  ERPNDB-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00272      05  ERCRTC-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
083102*    05  ERNOTE-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00274      05  ERPNDC-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00275      05  ERPYAJ-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00276      05  ERBILL-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00277      05  ERREPY-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00278      05  ERCHEK-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00279      05  ERCHKQ-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00280      05  ERCMKQ-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00281      05  ERPNDM-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00282      05  ERRQST-FILE-STATUS          PIC XX      VALUE ZERO.      EL530
00283      05  WS-SAVE-PRINT-RECORD        PIC X(133) VALUE SPACES.     EL530
00284                                                                   EL530
00285      05  WS-SAVE-KEY.                                             EL530
00286          10  WS-SAVE-COMPANY-CD      PIC X      VALUE SPACES.     EL530
00287          10  WS-SAVE-CARRIER         PIC X      VALUE SPACES.     EL530
00288          10  WS-SAVE-GROUPING        PIC X(6)   VALUE SPACES.     EL530
00289          10  WS-SAVE-STATE           PIC XX     VALUE SPACES.     EL530
00290          10  WS-SAVE-ACCOUNT         PIC X(10)  VALUE SPACES.     EL530
00291          10  WS-SAVE-BATCH           PIC X(6)   VALUE SPACES.     EL530
00292      05  WS-PURGE-DT                 PIC XX     VALUE SPACES.     EL530
00293      05  WS-CHKQ-PURGE-DT            PIC XX     VALUE SPACES.     EL530
00294      05  X                           PIC X      VALUE SPACES.     EL530
00295      05  WS-SAVE-DATE                PIC XX.                      EL530
00296      05  WS-SAVE-TIME                PIC 9(6).                    EL530
00297      05  WS-DASH                     PIC XX     VALUE ' -'.       EL530
00298      05  PGM-SUB                     PIC S9(4)  COMP  VALUE +530. EL530
00299      05  ABEND-CODE                  PIC X(4)   VALUE SPACES.     EL530
00300      05  ABEND-OPTION                PIC X      VALUE SPACES.     EL530
00301      05  OLC-REPORT-NAME             PIC X(5)   VALUE 'EL530'.    EL530
00302                                                                   EL530
00303      EJECT                                                        EL530
00304                                                                   EL530
00305      COPY ELCDATE.                                                   CL**4
00306                                                                   EL530
00307      EJECT                                                        EL530
00308  01  WS-HEADING1.                                                 EL530
00309      05  FILLER                      PIC X(50) VALUE '1'.         EL530
00310      05  FILLER                      PIC X(25) VALUE              EL530
00311          'CREDIT FILE PURGE REPORT '.                             EL530
00312      05  FILLER                      PIC X(50) VALUE SPACES.      EL530
00313      05  FILLER                      PIC X(8)  VALUE 'EL530'.     EL530
00314                                                                   EL530
00315  01  WS-HEADING2.                                                 EL530
00316      05  FILLER                      PIC X(46) VALUE SPACES.      EL530
00317      05  WS-H2-CLIENT-NAME           PIC X(30) VALUE SPACES.      EL530
00318      05  FILLER                      PIC X(49) VALUE SPACES.      EL530
00319      05  WS-H2-DATE                  PIC X(8)  VALUE SPACES.      EL530
00320                                                                   EL530
00321  01  WS-HEADING3.                                                 EL530
00322      05  FILLER                      PIC X(53) VALUE SPACES.      EL530
00323      05  WS-H3-DATE                  PIC X(58) VALUE SPACES.      EL530
00324      05  FILLER                      PIC X(5)  VALUE 'PAGE'.      EL530
00325      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL530
00326                                                                   EL530
00327  01  WS-HEADING4                     PIC X(133) VALUE SPACES.     EL530
00328                                                                   EL530
00329  01  WS-HEADING5.                                                 EL530
00330      05  FILLER                      PIC X(49)  VALUE SPACES.     EL530
00331      05  WS-H4-COMPANY-ID            PIC XXX    VALUE SPACES.     EL530
00332      05  FILLER                      PIC X(24)  VALUE             EL530
00333                              ' PENDING BUSINESS DETAIL'.          EL530
00334                                                                   EL530
00335  01  WS-HEADING6.                                                 EL530
00336      05  FILLER                      PIC X(37)  VALUE '0'.        EL530
00337      05  FILLER                      PIC X(17)  VALUE             EL530
00338                              'BATCH NUMBER'.                      EL530
00339      05  FILLER                      PIC X(19)  VALUE             EL530
00340                              'RECORDS PURGED'.                    EL530
00341      05  FILLER                      PIC X(21)  VALUE             EL530
00342                              'COMPLETE BATCH PURGED'.             EL530
00343      EJECT                                                        EL530
00344  01  PNDB-DETAIL.                                                 EL530
00345      05  FILLER                      PIC X(39) VALUE SPACES.      EL530
00346      05  P-BATCH                     PIC X(6)  VALUE SPACES.      EL530
00347      05  FILLER                      PIC X(12) VALUE SPACES.      EL530
00348      05  P-COUNT                     PIC ZZ,Z99.                  EL530
00349      05  FILLER                      PIC X(18) VALUE SPACES.      EL530
00350      05  P-YES-NO                    PIC X(3)  VALUE SPACES.      EL530
00351                                                                   EL530
00352  01  WS-PNDB-COUNT.                                               EL530
00353      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00354      05  FILLER                      PIC X(38)                    EL530
00355                   VALUE 'PENDING BUSINESS RECORDS PURGED.......'. EL530
00356      05  PNDB-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00357                                                                   EL530
00358  01  WS-PNDM-COUNT.                                               EL530
00359      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00360      05  FILLER                      PIC X(38)                    EL530
00361                   VALUE 'PENDING MAILING RECORDS PURGED.......'.  EL530
00362      05  PNDM-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00363                                                                   EL530
00364  01  WS-CRTC-COUNT.                                               EL530
00365      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00366      05  FILLER                      PIC X(38)                    EL530
00367                   VALUE 'CERT CHANGE RECORDS PURGED............'. EL530
00368      05  ERTC-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00369                                                                   EL530
00370  01  WS-NOTE-COUNT.                                               EL530
00371      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00372      05  FILLER                      PIC X(38)                    EL530
00373                   VALUE 'CERT NOTE RECORDS PURGED..............'. EL530
00374      05  NOTE-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00375                                                                   EL530
00376  01  WS-PNDC-COUNT.                                               EL530
00377      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00378      05  FILLER                      PIC X(38)                    EL530
00379                   VALUE 'PENDING CLAIMS RECORDS PURGED.........'. EL530
00380      05  PNDC-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00381                                                                   EL530
00382  01  WS-PYAJ-COUNT.                                               EL530
00383      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00384      05  FILLER                      PIC X(38)                    EL530
00385                   VALUE 'PAYMENTS/ADJUSTMENTS RECORDS PURGED...'. EL530
00386      05  PYAJ-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00387                                                                   EL530
00388  01  WS-BILL-COUNT.                                               EL530
00389      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00390      05  FILLER                      PIC X(38)                    EL530
00391                   VALUE 'BILLING STATEMENT RECORDS PURGED......'. EL530
00392      05  BILL-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00393                                                                   EL530
00394  01  WS-REPY-COUNT.                                               EL530
00395      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00396      05  FILLER                      PIC X(38)                    EL530
00397                   VALUE 'RETRO REIN-ADJUSTMENT RECORDS PURGED..'. EL530
00398      05  REPY-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00399                                                                   EL530
00400  01  WS-CHEK-COUNT.                                               EL530
00401      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00402      05  FILLER                      PIC X(38)                    EL530
00403                   VALUE 'CHEK - MAINTENANCE RECORDS PURGED.....'. EL530
00404      05  CHEK-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00405                                                                   EL530
00406  01  WS-CHKQ-COUNT.                                               EL530
00407      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00408      05  FILLER                      PIC X(38)                    EL530
00409                   VALUE 'CHKQ - CHEK QUE RECORDS PURGED........'. EL530
00410      05  CHKQ-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00411                                                                   EL530
00412  01  WS-CMKQ-COUNT.                                               EL530
00413      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00414      05  FILLER                      PIC X(38)                    EL530
00415                   VALUE 'CMKQ - COMM CHEK QUE PURGED...........'. EL530
00416      05  CMKQ-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00417                                                                   EL530
00418  01  WS-RQST-COUNT.                                               EL530
00419      05  FILLER                      PIC X(5)  VALUE SPACES.      EL530
00420      05  FILLER                      PIC X(38)                    EL530
00421                   VALUE 'RQST - REQUEST  RECORDS PURGED........'. EL530
00422      05  RQST-CNT                    PIC Z,ZZZ,ZZ9.               EL530
00423                                                                   EL530
00424      COPY ELCDTECX SUPPRESS.                                      EL530
00425                                                                   EL530
00426      COPY ELCDTEVR SUPPRESS.                                      EL530
00427                                                                   EL530
00428      EJECT                                                        EL530
00429  PROCEDURE DIVISION.                                              EL530
00430                                                                   EL530
00431  0000-LOAD-DATE-CARD.                                             EL530
00432                                 COPY ELCDTERX SUPPRESS.           EL530
00433                                                                   EL530
00434  0000-CONVERT-CURRENT-DATE.                                       EL530
00435      ACCEPT WS-ACCEPT-DATE FROM DATE.                             EL530
00436      MOVE WS-AD-YY               TO WS-CD-YY.                     EL530
00437      MOVE WS-AD-MM               TO WS-CD-MM.                     EL530
00438      MOVE WS-AD-DD               TO WS-CD-DD.                     EL530
00439                                                                   EL530
00440      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL530
00441      MOVE '2'                    TO DC-OPTION-CODE.               EL530
00442      PERFORM 8500-DATE-CONVERSION.                                EL530
00443                                                                   EL530
00444      IF DATE-CONVERSION-ERROR                                     EL530
00445          DISPLAY '*** EL530  DATE CONVERSION ERROR JOB WILL ABEND'EL530
00446              UPON CONSOLE                                         EL530
00447          MOVE DC-ERROR-CODE      TO WS-ABEND-FILE-STATUS          EL530
00448          GO TO ABEND-PGM.                                         EL530
00449                                                                   EL530
00450      MOVE DC-BIN-DATE-1          TO WS-PURGE-DT.                  EL530
00451      MOVE DC-GREG-DATE-1-ALPHA   TO ALPH-DATE.                    EL530
00452                                                                   EL530
00453      MOVE '6'                    TO DC-OPTION-CODE.               EL530
00454      MOVE -4                     TO DC-ELAPSED-MONTHS.            EL530
00455      PERFORM 8500-DATE-CONVERSION.                                EL530
00456                                                                   EL530
00457      IF DATE-CONVERSION-ERROR                                     EL530
00458          DISPLAY '*** EL530  DATE CONVERSION ERROR JOB WILL ABEND'EL530
00459              UPON CONSOLE                                         EL530
00460          MOVE DC-ERROR-CODE      TO WS-ABEND-FILE-STATUS          EL530
00461          GO TO ABEND-PGM.                                         EL530
00462                                                                   EL530
00463      MOVE DC-BIN-DATE-2          TO WS-CHKQ-PURGE-DT.             EL530
00464                                                                   EL530
00465      OPEN INPUT ELCNTL                                               CL**3
00466          OUTPUT PRNTR.                                               CL**3
00467                                                                   EL530
00468      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        EL530
00469          NEXT SENTENCE                                            EL530
00470        ELSE                                                       EL530
00471          DISPLAY '*** EL530  ELCNTL OPEN ERROR - JOB WILL ABEND'  EL530
00472              UPON CONSOLE                                         EL530
00473          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00474          GO TO ABEND-PGM.                                         EL530
00475                                                                   EL530
00476      MOVE LOW-VALUES             TO CF-CONTROL-PRIMARY.           EL530
00477      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL530
00478      MOVE '1'                    TO CF-RECORD-TYPE.               EL530
00479      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL530
00480      MOVE +0                     TO CF-SEQUENCE-NO.               EL530
00481                                                                   EL530
00482      READ ELCNTL.                                                 EL530
00483                                                                   EL530
00484      IF ELCNTL-FILE-STATUS = ZERO                                 EL530
00485          NEXT SENTENCE                                            EL530
00486        ELSE                                                       EL530
00487          DISPLAY '*** EL530  ELCNTL READ ERROR - JOB WILL ABEND'  EL530
00488              UPON CONSOLE                                         EL530
00489          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00490          GO TO ABEND-PGM.                                         EL530
00491      EJECT                                                        EL530
00492                                                                   EL530
00493  1000-PROCESS-PENDING-BUSINESS.                                   EL530
00494      OPEN I-O  ERPNDB                                             EL530
083102*              ERNOTE                                             EL530
00496                ERPNDM.                                            EL530
00497                                                                   EL530
00498      IF ERPNDB-FILE-STATUS  = '00' OR '97'                        EL530
00499          NEXT SENTENCE                                            EL530
00500        ELSE                                                       EL530
00501          DISPLAY '*** EL530  ERPNDB OPEN ERROR - JOB WILL ABEND'  EL530
00502              UPON CONSOLE                                         EL530
00503          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00504          GO TO ABEND-PGM.                                         EL530
00505                                                                   EL530
083102*    IF ERNOTE-FILE-STATUS  = '00' OR '97' OR '9%' OR '9+'        EL530
083102*        NEXT SENTENCE                                            EL530
083102*      ELSE                                                       EL530
083102*        DISPLAY '*** EL530  ERNOTE OPEN ERROR - JOB WILL ABEND'  EL530
083102*            UPON CONSOLE                                         EL530
083102*        MOVE ERNOTE-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
083102*        GO TO ABEND-PGM.                                         EL530
00513                                                                   EL530
00514      IF ERPNDM-FILE-STATUS  = '00' OR '97'                        EL530
00515          NEXT SENTENCE                                            EL530
00516        ELSE                                                       EL530
00517          DISPLAY '*** EL530 ERPNDM OPEN ERROR - JOB WILL ABEND'   EL530
00518              UPON CONSOLE                                         EL530
00519          MOVE ERPNDM-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL530
00520          GO TO ABEND-PGM.                                         EL530
00521                                                                   EL530
00522      MOVE LOW-VALUES             TO  PB-CONTROL-PRIMARY.          EL530
00523      MOVE DTE-CLASIC-COMPANY-CD  TO  PB-COMPANY-CD.               EL530
00524                                                                   EL530
00525      START ERPNDB  KEY NOT LESS THAN  PB-CONTROL-PRIMARY.         EL530
00526                                                                   EL530
00527      IF ERPNDB-FILE-STATUS  = '23'                                EL530
00528          GO TO 1300-CLOSE-ERPNDB-FILES.                           EL530
00529                                                                   EL530
00530      IF ERPNDB-FILE-STATUS  = '10'                                EL530
00531          GO TO 1300-CLOSE-ERPNDB-FILES.                           EL530
00532                                                                   EL530
00533      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL530
00534          DISPLAY '*** EL530 ERPNDB START ERROR - JOB WILL ABEND'  EL530
00535              UPON CONSOLE                                         EL530
00536          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00537          GO TO ABEND-PGM.                                         EL530
00538                                                                   EL530
00539  1200-ERPNDB-LOOP.                                                EL530
00540      READ ERPNDB NEXT RECORD.                                     EL530
00541                                                                   EL530
00542      IF ERPNDB-FILE-STATUS = '10'                                 EL530
00543          GO TO 1300-CLOSE-ERPNDB-FILES.                           EL530
00544                                                                   EL530
00545      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL530
00546          DISPLAY '*** EL530  ERPNDB READ ERROR - JOB WILL ABEND'  EL530
00547              UPON CONSOLE                                         EL530
00548          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00549          GO TO ABEND-PGM.                                         EL530
00550                                                                   EL530
00551      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00552          GO TO 1300-CLOSE-ERPNDB-FILES.                           EL530
00553                                                                   EL530
00554      IF PB-CREDIT-ACCEPT-DT = LOW-VALUES                          EL530
00555          GO TO 1200-ERPNDB-LOOP.                                  EL530
00556                                                                   EL530
00557      IF LCP-ONCTR-01 =  0                                         EL530
00558          ADD 1 TO LCP-ONCTR-01                                    EL530
00559          MOVE PB-ENTRY-BATCH     TO SAVE-BATCH-NO                 EL530
00560          MOVE PB-COMPANY-ID      TO WS-H4-COMPANY-ID.             EL530
00561                                                                   EL530
00562      IF PB-ENTRY-BATCH NOT = SAVE-BATCH-NO                        EL530
00563          PERFORM 7200-PRINT-PNDB-DETAIL.                          EL530
00564                                                                   EL530
00565      IF PB-BATCH-TRAILER                                          EL530
00566          MOVE 'YES'  TO P-YES-NO.                                 EL530
00567                                                                   EL530
00568      ADD +1  TO BATCH-COUNT.                                      EL530
00569                                                                   EL530
00570      IF PB-ISSUE                                                  EL530
00571         IF PB-I-MAIL-ADDRS-PRESENT                                EL530
00572            PERFORM 1400-DELETE-PEND-MAIL THRU 1400-EXIT.          EL530
00573                                                                   EL530
PEMTST*    IF PB-CI-NOTE-SW NOT = SPACE                                 EL530
PEMTST*        PERFORM 1350-REWRITE-NOTE THRU 1350-EXIT.                EL530
083102*    PERFORM 1350-REWRITE-NOTE   THRU 1350-EXIT                   EL530
00576                                                                   EL530
00577      ADD +1  TO ERPNDB-RECORDS-PURGED.                            EL530
00578      DELETE ERPNDB RECORD.                                        EL530
00579                                                                   EL530
00580      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL530
00581          DISPLAY '*** EL530  ERPNDB DELETE ERROR - JOB WILL ABEND'EL530
00582              UPON CONSOLE                                         EL530
00583          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00584          GO TO ABEND-PGM.                                         EL530
00585                                                                   EL530
00586      GO TO 1200-ERPNDB-LOOP.                                      EL530
00587                                                                   EL530
00588  1300-CLOSE-ERPNDB-FILES.                                         EL530
00589      PERFORM 7200-PRINT-PNDB-DETAIL.                              EL530
00590                                                                   EL530
00591      CLOSE ERPNDB.                                                EL530
00592                                                                   EL530
00593      IF ERPNDB-FILE-STATUS NOT = ZEROS                            EL530
00594          DISPLAY '*** EL530  ERPNDB CLOSE ERROR - JOB WILL ABEND' EL530
00595              UPON CONSOLE                                         EL530
00596          MOVE ERPNDB-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00597          GO TO ABEND-PGM.                                         EL530
00598                                                                   EL530
083102*    CLOSE ERNOTE.                                                EL530
00600                                                                   EL530
083102*    IF ERNOTE-FILE-STATUS NOT = ZEROS                            EL530
083102*        DISPLAY '*** EL530  ERNOTE CLOSE ERROR - JOB WILL ABEND' EL530
083102*            UPON CONSOLE                                         EL530
083102*        MOVE ERNOTE-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
083102*        GO TO ABEND-PGM.                                         EL530
00606                                                                   EL530
00607      CLOSE ERPNDM.                                                EL530
00608                                                                   EL530
00609      IF ERPNDM-FILE-STATUS IS NOT = ZEROS                         EL530
00610          DISPLAY '*** EL530  ERPNDM CLOSE ERROR - JOB WILL ABEND' EL530
00611              UPON CONSOLE                                         EL530
00612          MOVE ERPNDM-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL530
00613          GO TO ABEND-PGM.                                         EL530
00614                                                                   EL530
00615      GO TO 2000-PROCESS-CERT-CHANGES.                             EL530
00616                                                                   EL530
00617      EJECT                                                        EL530
083102*1350-REWRITE-NOTE.                                               EL530
083102*    MOVE PB-CONTROL-BY-ACCOUNT  TO  CN-CONTROL-PRIMARY.          EL530
00620                                                                   EL530
083102*    READ ERNOTE.                                                 EL530
00622                                                                   EL530
083102*    IF ERNOTE-FILE-STATUS  = '23' OR '10'                        EL530
083102*        GO TO 1350-EXIT.                                         EL530
00625                                                                   EL530
083102*    IF ERNOTE-FILE-STATUS NOT = ZEROS                            EL530
083102*        DISPLAY '*** EL530 ERNOTE START ERROR - JOB WILL ABEND'  EL530
083102*            UPON CONSOLE                                         EL530
083102*        MOVE ERNOTE-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
083102*        ADD +1 TO ERNOTE-RECORDS-REWRITES                        EL530
083102*        GO TO ABEND-PGM.                                         EL530
00631                                                                   EL530
083102*    IF CN-LAST-MAINT-DT NOT > BIN-RUN-DATE
083102*       MOVE ZEROS               TO CN-BILLING-START-LINE-NO      EL530
083102*                                   CN-BILLING-END-LINE-NO        EL530
083102*       REWRITE CERTIFICATE-NOTE                                  EL530
083102*    END-IF
00636                                                                   EL530
083102*    IF ERNOTE-FILE-STATUS NOT = ZEROS                            EL530
083102*        DISPLAY '*** EL530  ERNOTE REWRITE ERROR- JOB WILL ABEND'EL530
083102*            UPON CONSOLE                                         EL530
083102*        MOVE ERNOTE-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
083102*        GO TO ABEND-PGM.                                         EL530
00642                                                                   EL530
083102*1350-EXIT.                                                       EL530
083102*     EXIT.                                                       EL530
00645                                                                   EL530
00646      EJECT                                                        EL530
00647  1400-DELETE-PEND-MAIL.                                           EL530
00648      MOVE PB-CONTROL-PRIMARY     TO  PM-CONTROL-PRIMARY.          EL530
00649      READ ERPNDM.                                                 EL530
00650                                                                   EL530
00651      IF ERPNDM-FILE-STATUS = '23'                                 EL530
00652          GO TO 1400-EXIT.                                         EL530
00653                                                                   EL530
00654      IF ERPNDM-FILE-STATUS NOT = ZEROS                            EL530
00655          DISPLAY '*** EL530  ERPNDM READ ERROR - JOB WILL ABEND'  EL530
00656              UPON CONSOLE                                         EL530
00657          MOVE ERPNDM-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL530
00658          GO TO ABEND-PGM.                                         EL530
00659                                                                   EL530
00660      ADD +1  TO ERPNDM-RECORDS-PURGED.                            EL530
00661                                                                   EL530
00662      DELETE ERPNDM RECORD.                                        EL530
00663                                                                   EL530
00664      IF ERPNDM-FILE-STATUS NOT = ZEROS                            EL530
00665          DISPLAY '*** EL530  ERPNDM DELETE ERROR - JOB WILL ABEND'EL530
00666              UPON CONSOLE                                         EL530
00667          MOVE ERPNDM-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL530
00668          GO TO ABEND-PGM.                                         EL530
00669                                                                   EL530
00670  1400-EXIT.                                                       EL530
00671      EXIT.                                                        EL530
00672                                                                   EL530
00673      EJECT                                                        EL530
00674                                                                   EL530
00675  2000-PROCESS-CERT-CHANGES.                                       EL530
00676      OPEN I-O  ERCRTC.                                            EL530
00677                                                                   EL530
00678      IF ERCRTC-FILE-STATUS  = '00' OR '97'                        EL530
00679          NEXT SENTENCE                                            EL530
00680        ELSE                                                       EL530
00681          DISPLAY '*** EL530  ERCRTC OPEN ERROR - JOB WILL ABEND'  EL530
00682              UPON CONSOLE                                         EL530
00683          MOVE ERCRTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00684          GO TO ABEND-PGM.                                         EL530
00685                                                                   EL530
00686      MOVE LOW-VALUES             TO  CC-CONTROL-PRIMARY.          EL530
00687      MOVE DTE-CLASIC-COMPANY-CD  TO  CC-COMPANY-CD.               EL530
00688                                                                   EL530
00689      START ERCRTC  KEY NOT LESS THAN  CC-CONTROL-PRIMARY.         EL530
00690                                                                   EL530
00691      IF ERCRTC-FILE-STATUS  = '23'                                EL530
00692          GO TO 2300-CLOSE-ERCRTC-FILES.                           EL530
00693                                                                   EL530
00694      IF ERCRTC-FILE-STATUS  = '00'                                EL530
00695          NEXT SENTENCE                                            EL530
00696        ELSE                                                       EL530
00697          DISPLAY '*** EL530 ERCRTC START ERROR - JOB WILL ABEND'  EL530
00698              UPON CONSOLE                                         EL530
00699          MOVE ERCRTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00700          GO TO ABEND-PGM.                                         EL530
00701                                                                   EL530
00702  2200-ERCRTC-LOOP.                                                EL530
00703      READ ERCRTC NEXT RECORD.                                     EL530
00704                                                                   EL530
00705      IF CC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00706          GO TO 2300-CLOSE-ERCRTC-FILES.                           EL530
00707                                                                   EL530
00708      IF ERCRTC-FILE-STATUS = '10'                                 EL530
00709          GO TO 2300-CLOSE-ERCRTC-FILES.                           EL530
00710                                                                   EL530
00711      IF ERCRTC-FILE-STATUS NOT = ZEROS                            EL530
00712          DISPLAY '*** EL530  ERCRTC READ ERROR - JOB WILL ABEND'  EL530
00713              UPON CONSOLE                                         EL530
00714          MOVE ERCRTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00715          GO TO ABEND-PGM.                                         EL530
00716                                                                   EL530
00717      IF CC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00718          GO TO 2300-CLOSE-ERCRTC-FILES.                           EL530
00719                                                                   EL530
00720      IF CC-CREDIT-ACCEPT-DT = LOW-VALUES                          EL530
00721          GO TO 2200-ERCRTC-LOOP.                                  EL530
00722                                                                   EL530
00723      ADD +1  TO ERCRTC-RECORDS-PURGED.                            EL530
00724                                                                   EL530
00725      DELETE ERCRTC RECORD.                                        EL530
00726                                                                   EL530
00727      IF ERCRTC-FILE-STATUS NOT = ZEROS                            EL530
00728          DISPLAY '*** EL530  ERCRTC DELETE ERROR - JOB WILL ABEND'EL530
00729              UPON CONSOLE                                         EL530
00730          MOVE ERCRTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00731          GO TO ABEND-PGM.                                         EL530
00732                                                                   EL530
00733      GO TO 2200-ERCRTC-LOOP.                                      EL530
00734                                                                   EL530
00735  2300-CLOSE-ERCRTC-FILES.                                         EL530
00736      CLOSE ERCRTC.                                                EL530
00737                                                                   EL530
00738      IF ERCRTC-FILE-STATUS NOT = ZEROS                            EL530
00739          DISPLAY '*** EL530  ERCRTC CLOSE ERROR - JOB WILL ABEND' EL530
00740              UPON CONSOLE                                         EL530
00741          MOVE ERCRTC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00742          GO TO ABEND-PGM.                                         EL530
00743      EJECT                                                        EL530
00744                                                                   EL530
00745  3000-PROCESS-PENDING-CLAIMS.                                     EL530
00746      OPEN I-O  ERPNDC.                                            EL530
00747                                                                   EL530
00748      IF ERPNDC-FILE-STATUS  = '00' OR '97'                        EL530
00749          NEXT SENTENCE                                            EL530
00750        ELSE                                                       EL530
00751          DISPLAY '*** EL530  ERPNDC OPEN ERROR - JOB WILL ABEND'  EL530
00752              UPON CONSOLE                                         EL530
00753          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00754          GO TO ABEND-PGM.                                         EL530
00755                                                                   EL530
00756      MOVE LOW-VALUES             TO  PC-CONTROL-PRIMARY.          EL530
00757      MOVE DTE-CLASIC-COMPANY-CD  TO  PC-COMPANY-CD.               EL530
00758                                                                   EL530
00759      START ERPNDC  KEY NOT LESS THAN  PC-CONTROL-PRIMARY.         EL530
00760                                                                   EL530
00761      IF ERPNDC-FILE-STATUS  = '23'                                EL530
00762          GO TO 3300-CLOSE-ERPNDC-FILES.                           EL530
00763                                                                   EL530
00764      IF ERPNDC-FILE-STATUS  = '00'                                EL530
00765          NEXT SENTENCE                                            EL530
00766        ELSE                                                       EL530
00767          DISPLAY '*** EL530 ERPNDC START ERROR - JOB WILL ABEND'  EL530
00768              UPON CONSOLE                                         EL530
00769          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00770          GO TO ABEND-PGM.                                         EL530
00771                                                                   EL530
00772  3200-ERPNDC-LOOP.                                                EL530
00773      READ ERPNDC NEXT RECORD.                                     EL530
00774                                                                   EL530
00775       IF PC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                EL530
00776          GO TO 3300-CLOSE-ERPNDC-FILES.                           EL530
00777                                                                   EL530
00778      IF ERPNDC-FILE-STATUS = '10'                                 EL530
00779          GO TO 3300-CLOSE-ERPNDC-FILES.                           EL530
00780                                                                   EL530
00781      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL530
00782          DISPLAY '*** EL530  ERPNDC READ ERROR - JOB WILL ABEND'  EL530
00783              UPON CONSOLE                                         EL530
00784          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00785          GO TO ABEND-PGM.                                         EL530
00786                                                                   EL530
00787      IF PC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00788          GO TO 3300-CLOSE-ERPNDC-FILES.                           EL530
00789                                                                   EL530
00790      IF PC-CREDIT-ACCEPT-DT = LOW-VALUES                          EL530
00791          GO TO 3200-ERPNDC-LOOP.                                  EL530
00792                                                                   EL530
00793      ADD +1  TO ERPNDC-RECORDS-PURGED.                            EL530
00794                                                                   EL530
00795      DELETE ERPNDC RECORD.                                        EL530
00796                                                                   EL530
00797      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL530
00798          DISPLAY '*** EL530  ERPNDC DELETE ERROR - JOB WILL ABEND'EL530
00799              UPON CONSOLE                                         EL530
00800          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00801          GO TO ABEND-PGM.                                         EL530
00802                                                                   EL530
00803      GO TO 3200-ERPNDC-LOOP.                                      EL530
00804                                                                   EL530
00805  3300-CLOSE-ERPNDC-FILES.                                         EL530
00806      CLOSE ERPNDC.                                                EL530
00807                                                                   EL530
00808      IF ERPNDC-FILE-STATUS NOT = ZEROS                            EL530
00809          DISPLAY '*** EL530  ERPNDC CLOSE ERROR - JOB WILL ABEND' EL530
00810              UPON CONSOLE                                         EL530
00811          MOVE ERPNDC-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00812          GO TO ABEND-PGM.                                         EL530
00813      EJECT                                                        EL530
00814                                                                   EL530
00815  4000-PROCESS-PENDING-PAY-ADJ.                                    EL530
00816      OPEN I-O  ERPYAJ.                                            EL530
00817                                                                   EL530
00818      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL530
00819          NEXT SENTENCE                                            EL530
00820        ELSE                                                       EL530
00821          DISPLAY '*** EL530  ERPNDB OPEN ERROR - JOB WILL ABEND'  EL530
00822              UPON CONSOLE                                         EL530
00823          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00824          GO TO ABEND-PGM.                                         EL530
00825                                                                   EL530
00826      MOVE LOW-VALUES             TO  PY-CONTROL-PRIMARY.          EL530
00827      MOVE DTE-CLASIC-COMPANY-CD  TO  PY-COMPANY-CD.               EL530
00828                                                                   EL530
00829      START ERPYAJ  KEY NOT LESS THAN  PY-CONTROL-PRIMARY.         EL530
00830                                                                   EL530
00831      IF ERPYAJ-FILE-STATUS  = '23'                                EL530
00832          GO TO 4300-CLOSE-ERPYAJ-FILES.                           EL530
00833                                                                   EL530
00834      IF ERPYAJ-FILE-STATUS  = '00' OR '97'                        EL530
00835          NEXT SENTENCE                                            EL530
00836        ELSE                                                       EL530
00837          DISPLAY '*** EL530 ERPYAJ START ERROR - JOB WILL ABEND'  EL530
00838              UPON CONSOLE                                         EL530
00839          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00840          GO TO ABEND-PGM.                                         EL530
00841                                                                   EL530
00842  4200-ERPYAJ-LOOP.                                                EL530
00843      READ ERPYAJ NEXT RECORD.                                     EL530
00844                                                                   EL530
00845      IF PY-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00846          GO TO 4300-CLOSE-ERPYAJ-FILES.                           EL530
00847                                                                   EL530
00848      IF ERPYAJ-FILE-STATUS = '10'                                 EL530
00849          GO TO 4300-CLOSE-ERPYAJ-FILES.                           EL530
00850                                                                   EL530
00851      IF ERPYAJ-FILE-STATUS NOT = ZEROS                            EL530
00852          DISPLAY '*** EL530  ERPYAJ READ ERROR - JOB WILL ABEND'  EL530
00853              UPON CONSOLE                                         EL530
00854          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00855          GO TO ABEND-PGM.                                         EL530
00856                                                                   EL530
00857      IF PY-CREDIT-ACCEPT-DT = LOW-VALUES                          EL530
00858          GO TO 4200-ERPYAJ-LOOP.                                  EL530
00859                                                                   EL530
00860      ADD +1  TO ERPYAJ-RECORDS-PURGED.                            EL530
00861                                                                   EL530
00862      DELETE ERPYAJ RECORD.                                        EL530
00863                                                                   EL530
00864      IF ERPYAJ-FILE-STATUS NOT = ZEROS                            EL530
00865          DISPLAY '*** EL530  ERPYAJ DELETE ERROR - JOB WILL ABEND'EL530
00866              UPON CONSOLE                                         EL530
00867          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00868          GO TO ABEND-PGM.                                         EL530
00869                                                                   EL530
00870      GO TO 4200-ERPYAJ-LOOP.                                      EL530
00871                                                                   EL530
00872  4300-CLOSE-ERPYAJ-FILES.                                         EL530
00873      CLOSE ERPYAJ.                                                EL530
00874                                                                   EL530
00875      IF ERPYAJ-FILE-STATUS NOT = ZEROS                            EL530
00876          DISPLAY '*** EL530  ERPYAJ CLOSE ERROR - JOB WILL ABEND' EL530
00877              UPON CONSOLE                                         EL530
00878          MOVE ERPYAJ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00879          GO TO ABEND-PGM.                                         EL530
00880      EJECT                                                        EL530
00881                                                                   EL530
00882  5000-PROCESS-BILLING-STATEMENT.                                  EL530
00883                                                                   EL530
00884      OPEN I-O  ERBILL.                                            EL530
00885                                                                   EL530
00886      IF ERBILL-FILE-STATUS  = '00' OR '97'                        EL530
00887          NEXT SENTENCE                                            EL530
00888        ELSE                                                       EL530
00889          DISPLAY '*** EL530  ERBILL OPEN ERROR - JOB WILL ABEND'  EL530
00890              UPON CONSOLE                                         EL530
00891          MOVE ERBILL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00892          GO TO ABEND-PGM.                                         EL530
00893                                                                   EL530
00894      MOVE LOW-VALUES             TO  BI-CONTROL-PRIMARY.          EL530
00895      MOVE DTE-CLASIC-COMPANY-CD  TO  BI-COMPANY-CD.               EL530
00896                                                                   EL530
00897      START ERBILL  KEY NOT LESS THAN  BI-CONTROL-PRIMARY.         EL530
00898                                                                   EL530
00899      IF ERBILL-FILE-STATUS  = '23'                                EL530
00900          GO TO 5300-CLOSE-ERBILL-FILES.                           EL530
00901                                                                   EL530
00902  5200-ERBILL-LOOP.                                                EL530
00903      READ ERBILL NEXT RECORD.                                     EL530
00904                                                                   EL530
00905      IF BI-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00906          GO TO 5300-CLOSE-ERBILL-FILES.                           EL530
00907                                                                   EL530
00908      IF ERBILL-FILE-STATUS = '10'                                 EL530
00909          GO TO 5300-CLOSE-ERBILL-FILES.                           EL530
00910                                                                   EL530
00911      IF ERBILL-FILE-STATUS NOT = ZEROS                            EL530
00912          DISPLAY '*** EL530  ERBILL READ ERROR - JOB WILL ABEND'  EL530
00913              UPON CONSOLE                                         EL530
00914          MOVE ERBILL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00915          GO TO ABEND-PGM.                                         EL530
00916                                                                   EL530
00917      IF BI-HEADER-DATA                                            EL530
00918          IF BI-INITIAL-PRINT-DATE = LOW-VALUES                    EL530
00919              MOVE '3'            TO BI-RECORD-TYPE                EL530
00920              MOVE 9999           TO BI-LINE-SEQ-NO                EL530
00921              MOVE SPACES         TO ERBILL-SAVE-KEY               EL530
00922              GO TO 5200-ERBILL-LOOP                               EL530
00923          ELSE                                                     EL530
00924              MOVE BI-CONTROL-PRIMARY TO ERBILL-SAVE-KEY.          EL530
00925                                                                   EL530
00926      MOVE BI-CONTROL-PRIMARY     TO ERBILL-COMP-KEY.              EL530
00927                                                                   EL530
00928      IF ERBILL-COMP-KEY NOT = ERBILL-SAVE-KEY                     EL530
00929          GO TO 5200-ERBILL-LOOP.                                  EL530
00930                                                                   EL530
00931      ADD +1  TO ERBILL-RECORDS-PURGED.                            EL530
00932                                                                   EL530
00933      DELETE ERBILL RECORD.                                        EL530
00934                                                                   EL530
00935      IF ERBILL-FILE-STATUS NOT = ZEROS                            EL530
00936          DISPLAY '*** EL530  ERBILL DELETE ERROR - JOB WILL ABEND'EL530
00937              UPON CONSOLE                                         EL530
00938          MOVE ERBILL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00939          GO TO ABEND-PGM.                                         EL530
00940                                                                   EL530
00941      GO TO 5200-ERBILL-LOOP.                                      EL530
00942                                                                   EL530
00943  5300-CLOSE-ERBILL-FILES.                                         EL530
00944      CLOSE ERBILL.                                                EL530
00945                                                                   EL530
00946      IF ERBILL-FILE-STATUS NOT = ZEROS                            EL530
00947          DISPLAY '*** EL530  ERBILL CLOSE ERROR - JOB WILL ABEND' EL530
00948              UPON CONSOLE                                         EL530
00949          MOVE ERBILL-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00950          GO TO ABEND-PGM.                                         EL530
00951      EJECT                                                        EL530
00952  5500-PROCESS-RETRO-REIN-ADJ.                                     EL530
00953      OPEN I-O  ERREPY.                                            EL530
00954                                                                   EL530
00955      IF ERREPY-FILE-STATUS  = '00' OR '97'                        EL530
00956          NEXT SENTENCE                                            EL530
00957        ELSE                                                       EL530
00958          DISPLAY '*** EL530  ERREPY OPEN ERROR - JOB WILL ABEND'  EL530
00959              UPON CONSOLE                                         EL530
00960          MOVE ERREPY-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00961          GO TO ABEND-PGM.                                         EL530
00962                                                                   EL530
00963      MOVE LOW-VALUES             TO  RP-CONTROL-PRIMARY.          EL530
00964      MOVE DTE-CLASIC-COMPANY-CD  TO  RP-COMPANY-CD.               EL530
00965                                                                   EL530
00966      START ERREPY  KEY NOT LESS THAN  RP-CONTROL-PRIMARY.         EL530
00967                                                                   EL530
00968      IF ERREPY-FILE-STATUS  = '23'                                EL530
00969          GO TO 5800-CLOSE-ERREPY-FILES.                           EL530
00970                                                                   EL530
00971  5700-ERREPY-LOOP.                                                EL530
00972      READ ERREPY NEXT RECORD.                                     EL530
00973                                                                   EL530
00974      IF RP-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
00975          GO TO 5800-CLOSE-ERREPY-FILES.                           EL530
00976                                                                   EL530
00977      IF ERREPY-FILE-STATUS = '10'                                 EL530
00978          GO TO 5800-CLOSE-ERREPY-FILES.                           EL530
00979                                                                   EL530
00980      IF ERREPY-FILE-STATUS NOT = ZEROS                            EL530
00981          DISPLAY '*** EL530  ERREPY READ ERROR - JOB WILL ABEND'  EL530
00982              UPON CONSOLE                                         EL530
00983          MOVE ERREPY-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00984          GO TO ABEND-PGM.                                         EL530
00985                                                                   EL530
00986      IF RP-CREDIT-ACCEPT-DT = LOW-VALUES                          EL530
00987          GO TO 5700-ERREPY-LOOP.                                  EL530
00988                                                                   EL530
00989      ADD +1 TO ERREPY-RECORDS-PURGED.                             EL530
00990                                                                   EL530
00991      DELETE ERREPY RECORD.                                        EL530
00992                                                                   EL530
00993      IF ERREPY-FILE-STATUS NOT = ZEROS                            EL530
00994          DISPLAY '*** EL530  ERREPY DELETE ERROR - JOB WILL ABEND'EL530
00995              UPON CONSOLE                                         EL530
00996          MOVE ERREPY-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
00997          GO TO ABEND-PGM.                                         EL530
00998                                                                   EL530
00999      GO TO 5700-ERREPY-LOOP.                                      EL530
01000                                                                   EL530
01001  5800-CLOSE-ERREPY-FILES.                                         EL530
01002      CLOSE ERREPY.                                                EL530
01003                                                                   EL530
01004      IF ERREPY-FILE-STATUS NOT = ZEROS                            EL530
01005          DISPLAY '*** EL530  ERREPY CLOSE ERROR - JOB WILL ABEND' EL530
01006              UPON CONSOLE                                         EL530
01007          MOVE ERREPY-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01008          GO TO ABEND-PGM.                                         EL530

022614     go to 6200-PROCESS-CMKQ-RECS

           .
01012  5900-PROCESS-CHEK-MAINT-RECS.                                    EL530
01013                                                                   EL530
01014      OPEN I-O  ERCHEK.                                            EL530
01015                                                                   EL530
01016      IF ERCHEK-FILE-STATUS  = '96'                                EL530
01017          GO TO 6000-PROCESS-CHKQ-RECS.                            EL530
01018                                                                   EL530
01019      IF ERCHEK-FILE-STATUS  = '00' OR '97'                        EL530
01020          NEXT SENTENCE                                            EL530
01021        ELSE                                                       EL530
01022          DISPLAY '*** EL530  ERCHEK OPEN ERROR - JOB WILL ABEND'  EL530
01023              UPON CONSOLE                                         EL530
01024          MOVE ERCHEK-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01025          GO TO ABEND-PGM.                                         EL530
01026                                                                   EL530
01027      MOVE LOW-VALUES             TO  CH-CONTROL-PRIMARY.          EL530
01028      MOVE DTE-CLASIC-COMPANY-CD  TO  CH-COMPANY-CD.               EL530
01029                                                                   EL530
01030      START ERCHEK KEY NOT LESS THAN  CH-CONTROL-PRIMARY.          EL530
01031                                                                   EL530
01032      IF ERCHEK-FILE-STATUS  = '23'                                EL530
01033          GO TO 5990-CLOSE-ERCHEK-FILE.                            EL530
01034                                                                   EL530
01035  5910-ERCHEK-LOOP.                                                EL530
01036      READ ERCHEK NEXT RECORD.                                     EL530
01037                                                                   EL530
01038      IF CH-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
01039          GO TO 5990-CLOSE-ERCHEK-FILE.                            EL530
01040                                                                   EL530
01041      IF ERCHEK-FILE-STATUS = '10'                                 EL530
01042          GO TO 5990-CLOSE-ERCHEK-FILE.                            EL530
01043                                                                   EL530
01044      IF ERCHEK-FILE-STATUS NOT = ZEROS                            EL530
01045          DISPLAY '*** EL530  ERCHEK READ ERROR - JOB WILL ABEND'  EL530
01046              UPON CONSOLE                                         EL530
01047          MOVE ERCHEK-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01048          GO TO ABEND-PGM.                                         EL530
01049                                                                   EL530
01050      IF CH-VOID-DT GREATER THAN LOW-VALUES  AND                   EL530
01051         CH-CHECK-WRITTEN-DT = LOW-VALUES                          EL530
01052          GO TO 5920-PURGE-ERCHEK.                                 EL530
01053                                                                   EL530
01054      IF DTE-CLIENT = 'LAP'  OR  'RMC'                             EL530
01055          GO TO 5910-ERCHEK-LOOP.                                  EL530
01056                                                                   EL530
01057      IF CH-CHECK-WRITTEN-DT = LOW-VALUES                          EL530
01058          GO TO 5910-ERCHEK-LOOP.                                  EL530
01059                                                                   EL530
01060      IF CH-CHECK-WRITTEN-DT GREATER THAN WS-CHKQ-PURGE-DT         EL530
01061          GO TO 5910-ERCHEK-LOOP.                                  EL530
01062                                                                   EL530
01063  5920-PURGE-ERCHEK.                                               EL530
01064                                                                   EL530
01065      ADD +1 TO ERCHEK-RECORDS-PURGED.                             EL530
01066                                                                   EL530
01067      DELETE ERCHEK RECORD.                                        EL530
01068                                                                   EL530
01069      IF ERCHEK-FILE-STATUS NOT = ZEROS                            EL530
01070          DISPLAY '*** EL530  ERCHEK DELETE ERROR - JOB WILL ABEND'EL530
01071              UPON CONSOLE                                         EL530
01072          MOVE ERCHEK-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01073          GO TO ABEND-PGM.                                         EL530
01074                                                                   EL530
01075      GO TO 5910-ERCHEK-LOOP.                                      EL530
01076                                                                   EL530
01077  5990-CLOSE-ERCHEK-FILE.                                          EL530
01078      CLOSE ERCHEK.                                                EL530
01079                                                                   EL530
01080      IF ERCHEK-FILE-STATUS NOT = ZEROS                            EL530
01081          DISPLAY '*** EL530  ERCHEK CLOSE ERROR - JOB WILL ABEND' EL530
01082              UPON CONSOLE                                         EL530
01083          MOVE ERCHEK-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01084          GO TO ABEND-PGM.                                         EL530
01085                                                                   EL530
01086      EJECT                                                        EL530
01087  6000-PROCESS-CHKQ-RECS.                                          EL530
01088                                                                   EL530
01089      OPEN I-O  ERCHKQ.                                            EL530
01090                                                                   EL530
01091      IF ERCHKQ-FILE-STATUS  = '96'                                EL530
01092          GO TO 6500-PROCESS-RQST-RECS.                            EL530
01093                                                                   EL530
01094      IF ERCHKQ-FILE-STATUS  = '00' OR '97'                        EL530
01095          NEXT SENTENCE                                            EL530
01096        ELSE                                                       EL530
01097          DISPLAY '*** EL530  ERCHKQ OPEN ERROR - JOB WILL ABEND'  EL530
01098              UPON CONSOLE                                         EL530
01099          MOVE ERCHKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01100          GO TO ABEND-PGM.                                         EL530
01101                                                                   EL530
01102      MOVE LOW-VALUES             TO  CQ-CONTROL-PRIMARY.          EL530
01103      MOVE DTE-CLASIC-COMPANY-CD  TO  CQ-COMPANY-CD.               EL530
01104                                                                   EL530
01105      START ERCHKQ  KEY NOT LESS THAN  CQ-CONTROL-PRIMARY.         EL530
01106                                                                   EL530
01107      IF ERCHKQ-FILE-STATUS  = '23'                                EL530
01108          GO TO 6090-CLOSE-ERCHKQ-FILE.                            EL530
01109                                                                   EL530
01110  6010-ERCHKQ-LOOP.                                                EL530
01111      READ ERCHKQ NEXT RECORD.                                     EL530
01112                                                                   EL530
01113      IF CQ-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
01114          GO TO 6090-CLOSE-ERCHKQ-FILE.                            EL530
01115                                                                   EL530
01116      IF ERCHKQ-FILE-STATUS = '10'                                 EL530
01117          GO TO 6090-CLOSE-ERCHKQ-FILE.                            EL530
01118                                                                   EL530
01119      IF ERCHKQ-FILE-STATUS NOT = ZEROS                            EL530
01120          DISPLAY '*** EL530  ERCHKQ READ ERROR - JOB WILL ABEND'  EL530
01121              UPON CONSOLE                                         EL530
01122          MOVE ERCHKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01123          GO TO ABEND-PGM.                                         EL530
01124                                                                   EL530
01125      IF CHECK-IS-VOID  AND                                        EL530
01126         CQ-TIMES-PRINTED = ZEROS                                  EL530
01127          GO TO 6020-PURGE-ERCHKQ.                                 EL530
01128                                                                   EL530
01129      IF DTE-CLIENT = 'LAP'  OR  'RMC'                             EL530
01130          GO TO 6010-ERCHKQ-LOOP.                                  EL530
01131                                                                   EL530
01132      IF CQ-CHECK-WRITTEN-DT = LOW-VALUES                          EL530
01133          GO TO 6010-ERCHKQ-LOOP.                                  EL530
01134                                                                   EL530
01135      IF CQ-CHECK-WRITTEN-DT GREATER THAN WS-CHKQ-PURGE-DT         EL530
01136          GO TO 6010-ERCHKQ-LOOP.                                  EL530
01137                                                                   EL530
01138  6020-PURGE-ERCHKQ.                                               EL530
01139                                                                   EL530
01140      ADD +1 TO ERCHKQ-RECORDS-PURGED.                             EL530
01141                                                                   EL530
01142      DELETE ERCHKQ RECORD.                                        EL530
01143                                                                   EL530
01144      IF ERCHKQ-FILE-STATUS NOT = ZEROS                            EL530
01145          DISPLAY '*** EL530  ERCHKQ DELETE ERROR - JOB WILL ABEND'EL530
01146              UPON CONSOLE                                         EL530
01147          MOVE ERCHKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01148          GO TO ABEND-PGM.                                         EL530
01149                                                                   EL530
01150      GO TO 6010-ERCHKQ-LOOP.                                      EL530
01151                                                                   EL530
01152  6090-CLOSE-ERCHKQ-FILE.                                          EL530
01153      CLOSE ERCHKQ.                                                EL530
01154                                                                   EL530
01155      IF ERCHKQ-FILE-STATUS NOT = ZEROS                            EL530
01156          DISPLAY '*** EL530  ERCHKQ CLOSE ERROR - JOB WILL ABEND' EL530
01157              UPON CONSOLE                                         EL530
01158          MOVE ERCHKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01159          GO TO ABEND-PGM.                                         EL530
01160                                                                   EL530
01161      EJECT                                                        EL530
01162                                                                   EL530
01163  6200-PROCESS-CMKQ-RECS.                                          EL530
01164      IF CF-AR-SYSTEM-USED                                         EL530
01165          NEXT SENTENCE                                            EL530
01166      ELSE                                                         EL530
01167          GO TO 6500-PROCESS-RQST-RECS.                            EL530
01168                                                                   EL530
01169      OPEN I-O  ERCMKQ.                                            EL530
01170                                                                   EL530
01171      IF ERCMKQ-FILE-STATUS  EQUAL '00' OR '97' OR '9%' OR '9+'    EL530
01172          NEXT SENTENCE                                            EL530
01173      ELSE                                                         EL530
01174          DISPLAY '*** EL530  ERCMKQ OPEN ERROR - JOB WILL ABEND'  EL530
01175              UPON CONSOLE                                         EL530
01176          MOVE ERCHKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01177          GO TO ABEND-PGM.                                         EL530
01178                                                                   EL530
01179      MOVE LOW-VALUES             TO  MQ-CONTROL-PRIMARY.          EL530
01180      MOVE DTE-CLASIC-COMPANY-CD  TO  MQ-COMPANY-CD.               EL530
01181                                                                   EL530
01182      START ERCMKQ  KEY NOT LESS THAN  MQ-CONTROL-PRIMARY.         EL530
01183                                                                   EL530
01184      IF ERCMKQ-FILE-STATUS  = '23'                                EL530
01185          GO TO 6290-CLOSE-ERCMKQ-FILE.                            EL530
01186                                                                   EL530
01187  6210-ERCMKQ-LOOP.                                                EL530
01188      READ ERCMKQ NEXT RECORD.                                     EL530
01189                                                                   EL530
01190      IF MQ-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
01191          GO TO 6290-CLOSE-ERCMKQ-FILE.                            EL530
01192                                                                   EL530
01193      IF ERCMKQ-FILE-STATUS = '10'                                 EL530
01194          GO TO 6290-CLOSE-ERCMKQ-FILE.                            EL530
01195                                                                   EL530
01196      IF ERCMKQ-FILE-STATUS NOT = ZEROS                            EL530
01197          DISPLAY '*** EL530  ERCMKQ READ ERROR - JOB WILL ABEND'  EL530
01198              UPON CONSOLE                                         EL530
01199          MOVE ERCMKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01200          GO TO ABEND-PGM.                                         EL530
01201                                                                   EL530
01202      IF MQ-CHECK-WRITTEN-DT = LOW-VALUES                          EL530
01203          GO TO 6210-ERCMKQ-LOOP.                                  EL530
01204                                                                   EL530
01205      IF MQ-CHECK-WRITTEN-DT GREATER THAN WS-CHKQ-PURGE-DT         EL530
01206          GO TO 6210-ERCMKQ-LOOP.                                  EL530
01207                                                                   EL530
01208      ADD +1 TO ERCMKQ-RECORDS-PURGED.                             EL530
01209                                                                   EL530
01210      DELETE ERCMKQ RECORD.                                        EL530
01211                                                                   EL530
01212      IF ERCMKQ-FILE-STATUS NOT = ZEROS                            EL530
01213          DISPLAY '*** EL530  ERCMKQ DELETE ERROR - JOB WILL ABEND'EL530
01214              UPON CONSOLE                                         EL530
01215          MOVE ERCMKQ-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01216          GO TO ABEND-PGM.                                         EL530
01217                                                                   EL530
01218      GO TO 6210-ERCMKQ-LOOP.                                      EL530
01219                                                                   EL530
01220  6290-CLOSE-ERCMKQ-FILE.                                          EL530
01221      CLOSE ERCMKQ.                                                EL530
01222                                                                   EL530
01223      EJECT                                                        EL530
01224                                                                   EL530
01225  6500-PROCESS-RQST-RECS.                                          EL530
01226      IF CF-AR-SYSTEM-USED                                         EL530
01227          NEXT SENTENCE                                            EL530
01228      ELSE                                                         EL530
01229          GO TO 7000-PRINT-REPORT.                                 EL530
01230                                                                   EL530
01231      OPEN I-O  ERRQST.                                            EL530
01232                                                                   EL530
01233      IF ERRQST-FILE-STATUS  = '00' OR '97' OR '9%' OR '9+'        EL530
01234          NEXT SENTENCE                                            EL530
01235        ELSE                                                       EL530
01236          DISPLAY '*** EL530  ERRQST OPEN ERROR - JOB WILL ABEND'  EL530
01237              UPON CONSOLE                                         EL530
01238          MOVE ERRQST-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01239          GO TO ABEND-PGM.                                         EL530
01240                                                                   EL530
01241      MOVE LOW-VALUES             TO  RQ-CONTROL-PRIMARY.          EL530
01242      MOVE DTE-CLASIC-COMPANY-CD  TO  RQ-COMPANY-CD.               EL530
01243                                                                   EL530
01244      START ERRQST  KEY NOT LESS THAN  RQ-CONTROL-PRIMARY.         EL530
01245                                                                   EL530
01246      IF ERRQST-FILE-STATUS  = '23'                                EL530
01247          GO TO 6590-CLOSE-ERRQST-FILE.                            EL530
01248                                                                   EL530
01249  6510-ERRQST-LOOP.                                                EL530
01250      READ ERRQST NEXT RECORD.                                     EL530
01251                                                                   EL530
01252      IF RQ-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 EL530
01253          GO TO 6590-CLOSE-ERRQST-FILE.                            EL530
01254                                                                   EL530
01255      IF ERRQST-FILE-STATUS = '10'                                 EL530
01256          GO TO 6590-CLOSE-ERRQST-FILE.                            EL530
01257                                                                   EL530
01258      IF ERRQST-FILE-STATUS NOT = ZEROS                            EL530
01259          DISPLAY '*** EL530  ERRQST READ ERROR - JOB WILL ABEND'  EL530
01260              UPON CONSOLE                                         EL530
01261          MOVE ERRQST-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01262          GO TO ABEND-PGM.                                         EL530
01263                                                                   EL530
01264      IF RQ-CREDIT-ACCEPT-DT = LOW-VALUES                          EL530
01265          GO TO 6510-ERRQST-LOOP.                                  EL530
01266                                                                   EL530
01267      ADD +1 TO ERRQST-RECORDS-PURGED.                             EL530
01268                                                                   EL530
01269      DELETE ERRQST RECORD.                                        EL530
01270                                                                   EL530
01271      IF ERRQST-FILE-STATUS NOT = ZEROS                            EL530
01272          DISPLAY '*** EL530  ERRQST DELETE ERROR - JOB WILL ABEND'EL530
01273              UPON CONSOLE                                         EL530
01274          MOVE ERRQST-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01275          GO TO ABEND-PGM.                                         EL530
01276                                                                   EL530
01277      GO TO 6510-ERRQST-LOOP.                                      EL530
01278                                                                   EL530
01279  6590-CLOSE-ERRQST-FILE.                                          EL530
01280      CLOSE ERRQST.                                                EL530
01281                                                                   EL530
01282      IF ERRQST-FILE-STATUS NOT = ZEROS                            EL530
01283          DISPLAY '*** EL530  ERRQST CLOSE ERROR - JOB WILL ABEND' EL530
01284              UPON CONSOLE                                         EL530
01285          MOVE ERRQST-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL530
01286          GO TO ABEND-PGM.                                         EL530
01287                                                                   EL530
01288      EJECT                                                        EL530
01289                                                                   EL530
01290  7000-PRINT-REPORT.                                               EL530
01291      MOVE +99                    TO WS-LINE-COUNT.                EL530
01292      MOVE 'Y'                    TO FINAL-TOTALS-SW.              EL530
01293                                                                   EL530
01294      MOVE '-'                    TO P-CTL.                           CL**2
01295      MOVE ERPNDB-RECORDS-PURGED  TO PNDB-CNT.                     EL530
01296      MOVE WS-PNDB-COUNT          TO P-DATA.                       EL530
01297      PERFORM WRITE-A-LINE.                                        EL530
01298                                                                   EL530
01299      MOVE '0'                    TO P-CTL.                           CL**2
01300      MOVE ERPNDM-RECORDS-PURGED  TO PNDM-CNT.                     EL530
01301      MOVE WS-PNDM-COUNT          TO P-DATA.                       EL530
01302      PERFORM WRITE-A-LINE.                                        EL530
01303                                                                   EL530
01304      MOVE 0                      TO P-CTL.                        EL530
01305      MOVE ERCRTC-RECORDS-PURGED  TO ERTC-CNT.                     EL530
01306      MOVE WS-CRTC-COUNT          TO P-DATA.                       EL530
01307      PERFORM WRITE-A-LINE.                                        EL530
01308                                                                   EL530
083102*    MOVE 0                      TO P-CTL.                        EL530
083102*    MOVE ERNOTE-RECORDS-REWRITES TO NOTE-CNT.                    EL530
083102*    MOVE WS-NOTE-COUNT          TO P-DATA.                       EL530
083102*    PERFORM WRITE-A-LINE.                                        EL530
01313                                                                   EL530
01314      MOVE 0                      TO P-CTL.                        EL530
01315      MOVE ERPNDC-RECORDS-PURGED  TO PNDC-CNT.                     EL530
01316      MOVE WS-PNDC-COUNT          TO P-DATA.                       EL530
01317      PERFORM WRITE-A-LINE.                                        EL530
01318                                                                   EL530
01319      MOVE 0                      TO P-CTL.                        EL530
01320      MOVE ERPYAJ-RECORDS-PURGED  TO PYAJ-CNT.                     EL530
01321      MOVE WS-PYAJ-COUNT          TO P-DATA.                       EL530
01322      PERFORM WRITE-A-LINE.                                        EL530
01323                                                                   EL530
01324      MOVE 0                      TO P-CTL.                        EL530
01325      MOVE ERBILL-RECORDS-PURGED  TO BILL-CNT.                     EL530
01326      MOVE WS-BILL-COUNT          TO P-DATA.                       EL530
01327      PERFORM WRITE-A-LINE.                                        EL530
01328                                                                   EL530
01329      MOVE 0                      TO P-CTL.                        EL530
01330      MOVE ERREPY-RECORDS-PURGED  TO REPY-CNT.                     EL530
01331      MOVE WS-REPY-COUNT          TO P-DATA.                       EL530
01332      PERFORM WRITE-A-LINE.                                        EL530
01333                                                                   EL530
01334      MOVE 0                      TO P-CTL.                        EL530
01335      MOVE ERCHEK-RECORDS-PURGED  TO CHEK-CNT.                     EL530
01336      MOVE WS-CHEK-COUNT          TO P-DATA.                       EL530
01337      PERFORM WRITE-A-LINE.                                        EL530
01338                                                                   EL530
01339      MOVE 0                      TO P-CTL.                        EL530
01340      MOVE ERCHKQ-RECORDS-PURGED  TO CHKQ-CNT.                     EL530
01341      MOVE WS-CHKQ-COUNT          TO P-DATA.                       EL530
01342      PERFORM WRITE-A-LINE.                                        EL530
01343                                                                   EL530
01344      MOVE 0                      TO P-CTL.                        EL530
01345      MOVE ERCMKQ-RECORDS-PURGED  TO CMKQ-CNT.                     EL530
01346      MOVE WS-CMKQ-COUNT          TO P-DATA.                       EL530
01347      PERFORM WRITE-A-LINE.                                        EL530
01348                                                                   EL530
01349      MOVE 0                      TO P-CTL.                        EL530
01350      MOVE ERRQST-RECORDS-PURGED  TO RQST-CNT.                     EL530
01351      MOVE WS-RQST-COUNT          TO P-DATA.                       EL530
01352      PERFORM WRITE-A-LINE.                                        EL530
01353                                                                   EL530
01354      CLOSE PRNTR.                                                 EL530
01355      GOBACK.                                                      EL530
01356      EJECT                                                        EL530
01357  7200-PRINT-PNDB-DETAIL SECTION.                                  EL530
01358      MOVE SAVE-BATCH-NO          TO P-BATCH.                      EL530
01359      MOVE BATCH-COUNT            TO P-COUNT.                      EL530
01360                                                                   EL530
01361      IF P-YES-NO NOT = 'YES'                                      EL530
01362          MOVE 'NO'               TO P-YES-NO.                     EL530
01363                                                                   EL530
01364      MOVE SPACE                  TO P-CTL.                           CL**2
01365      MOVE PNDB-DETAIL            TO P-DATA.                       EL530
01366      PERFORM WRITE-A-LINE.                                        EL530
01367      MOVE SPACES                 TO P-YES-NO.                     EL530
01368      MOVE PB-ENTRY-BATCH         TO SAVE-BATCH-NO.                EL530
01369      MOVE ZEROS                  TO BATCH-COUNT.                  EL530
01370                                                                   EL530
01371      IF PB-COMPANY-ID NOT = WS-H4-COMPANY-ID                      EL530
01372          MOVE PB-COMPANY-ID      TO WS-H4-COMPANY-ID              EL530
01373          MOVE +99                TO WS-LINE-COUNT.                EL530
01374                                                                   EL530
01375  7200-EXIT.                                                       EL530
01376      EXIT.                                                        EL530
01377      EJECT                                                        EL530
01378                                                                   EL530
01379  8500-DATE-CONVERSION SECTION.                                    EL530
01380      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   EL530
01381                                                                   EL530
01382  8500-EXIT.                                                       EL530
01383       EXIT.                                                       EL530
01384                                                                   EL530
01385  WRITE-A-LINE SECTION.           COPY ELCWAL.                     EL530
01386                                                                   EL530
01387  WRITE-HEADINGS SECTION.                                          EL530
01388 ***************************************************************** EL530
01389 *                            ELCWHS1.                           * EL530
01390 *                            VMOD=2.001                         * EL530
01391 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL530
01392 *****************************************************************.EL530
01393  WHS-010.                                                         EL530
01394      IF  WS-H2-DATE EQUAL SPACES                                  EL530
01395          MOVE WS-CURRENT-DATE    TO  WS-H2-DATE                   EL530
01396          MOVE COMPANY-NAME       TO  WS-H2-CLIENT-NAME            EL530
01397          MOVE ALPH-DATE          TO  WS-H3-DATE.                  EL530
01398                                                                   EL530
01399      ADD +1  TO  WS-PAGE.                                         EL530
01400      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL530
01401      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL530
01402      MOVE ZERO                   TO  WS-LINE-COUNT.               EL530
01403                                                                   EL530
01404      MOVE WS-HEADING1            TO  PRT.                         EL530
01405      MOVE '1'                    TO  X.                           EL530
01406      PERFORM WRITE-PRINTER.                                       EL530
01407                                                                   EL530
01408      MOVE WS-HEADING2            TO  PRT.                         EL530
01409      MOVE ' '                    TO  X.                           EL530
01410      PERFORM WRITE-PRINTER.                                       EL530
01411                                                                   EL530
01412      MOVE WS-HEADING3            TO  PRT.                         EL530
01413      MOVE ' '                    TO  X.                           EL530
01414      PERFORM WRITE-PRINTER.                                       EL530
01415                                                                   EL530
01416      MOVE WS-HEADING4            TO  PRT.                         EL530
01417      MOVE ' '                    TO  X.                           EL530
01418      PERFORM WRITE-PRINTER.                                       EL530
01419                                                                   EL530
01420                                                                   EL530
01421      IF NOT FINAL-TOTALS                                          EL530
01422          MOVE WS-HEADING5        TO PRT                           EL530
01423          PERFORM WRITE-PRINTER                                    EL530
01424          MOVE WS-HEADING6        TO PRT                           EL530
01425          PERFORM WRITE-PRINTER                                    EL530
01426          MOVE SPACES             TO PRT                           EL530
01427          PERFORM WRITE-PRINTER.                                   EL530
01428                                                                   EL530
01429      MOVE WS-SAVE-PRINT-RECORD   TO PRT.                          EL530
01430                                                                   EL530
01431      MOVE +11                    TO  WS-LINE-COUNT.               EL530
01432                                                                   EL530
01433  WHS-EXIT.                                                        EL530
01434      EXIT.                                                        EL530
01435      EJECT                                                        EL530
01436  WRITE-PRINTER SECTION.          COPY ELCWPS.                     EL530
01437                                                                   EL530
01438      MOVE X TO LCP-ASA                                            EL530
01439      PERFORM LCP-WRITE-POS-PRT                                    EL530
01440          THRU LCP-WRITE-END-PRT.                                  EL530
01441                                                                   EL530
01442                                                                   EL530
01443  ABEND-PGM SECTION.              COPY ELCABEND SUPPRESS.          EL530
01444 /                                                                 EL530
01445  LCP-WRITE-POS-PRT SECTION.                                       EL530
01446      IF LCP-ASA = '+'                                             EL530
01447          WRITE PRT AFTER 0 LINE                                   EL530
01448      ELSE                                                         EL530
01449      IF LCP-ASA = ' '                                             EL530
01450          WRITE PRT AFTER ADVANCING 1 LINE                         EL530
01451      ELSE                                                         EL530
01452      IF LCP-ASA = '0'                                             EL530
01453          WRITE PRT AFTER ADVANCING 2 LINE                         EL530
01454      ELSE                                                         EL530
01455      IF LCP-ASA = '-'                                             EL530
01456          WRITE PRT AFTER ADVANCING 3 LINE                         EL530
01457      ELSE                                                         EL530
01458      IF LCP-ASA = '1'                                             EL530
01459          WRITE PRT AFTER ADVANCING PAGE                           EL530
01460      ELSE                                                         EL530
01461      IF LCP-ASA = '2'                                             EL530
01462          WRITE PRT AFTER ADVANCING LCP-CH2                        EL530
01463      ELSE                                                         EL530
01464      IF LCP-ASA = '3'                                             EL530
01465          WRITE PRT AFTER ADVANCING LCP-CH3                        EL530
01466      ELSE                                                         EL530
01467      IF LCP-ASA = '4'                                             EL530
01468          WRITE PRT AFTER ADVANCING LCP-CH4                        EL530
01469      ELSE                                                         EL530
01470      IF LCP-ASA = '5'                                             EL530
01471          WRITE PRT AFTER ADVANCING LCP-CH5                        EL530
01472      ELSE                                                         EL530
01473      IF LCP-ASA = '6'                                             EL530
01474          WRITE PRT AFTER ADVANCING LCP-CH6                        EL530
01475      ELSE                                                         EL530
01476      IF LCP-ASA = '7'                                             EL530
01477          WRITE PRT AFTER ADVANCING LCP-CH7                        EL530
01478      ELSE                                                         EL530
01479      IF LCP-ASA = '8'                                             EL530
01480          WRITE PRT AFTER ADVANCING LCP-CH8                        EL530
01481      ELSE                                                         EL530
01482      IF LCP-ASA = '9'                                             EL530
01483          WRITE PRT AFTER ADVANCING LCP-CH9                        EL530
01484      ELSE                                                         EL530
01485      IF LCP-ASA = 'A'                                             EL530
01486          WRITE PRT AFTER ADVANCING LCP-CH10                       EL530
01487      ELSE                                                         EL530
01488      IF LCP-ASA = 'B'                                             EL530
01489          WRITE PRT AFTER ADVANCING LCP-CH11                       EL530
01490      ELSE                                                         EL530
01491      IF LCP-ASA = 'C'                                             EL530
01492          WRITE PRT AFTER ADVANCING LCP-CH12                       EL530
01493      ELSE                                                         EL530
01494      IF LCP-ASA = 'V'                                             EL530
01495          WRITE PRT AFTER ADVANCING LCP-P01                        EL530
01496      ELSE                                                         EL530
01497      IF LCP-ASA = 'W'                                             EL530
01498          WRITE PRT AFTER ADVANCING LCP-P02                        EL530
01499      ELSE                                                         EL530
01500      DISPLAY 'ASA CODE ERROR'.                                    EL530
01501  LCP-WRITE-END-PRT.                                               EL530
01502      EXIT.                                                        EL530

