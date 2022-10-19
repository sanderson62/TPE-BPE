00001  IDENTIFICATION DIVISION.                                         03/06/98
00002                                                                   EL560
00003  PROGRAM-ID.                 EL560 .                                 LV002
00004 *              PROGRAM CONVERTED BY                               EL560
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL560
00006 *              CONVERSION DATE 04/14/95 13:45:50.                 EL560
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            EL560
00008 *                            VMOD=2.011.                          EL560
00009                                                                   EL560
00010 *AUTHOR.        LOGIC, INC.                                       EL560
00011 *               DALLAS, TEXAS.                                    EL560
00012                                                                   EL560
00013 *DATE-COMPILED.                                                   EL560
00014                                                                   EL560
00015 *SECURITY.   *****************************************************EL560
00016 *            *                                                   *EL560
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL560
00018 *            *                                                   *EL560
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL560
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL560
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL560
00022 *            *                                                   *EL560
00023 *            *****************************************************EL560
00024                                                                   EL560
00025 *REMARKS.                                                         EL560
00026 *        READS EXTR TAPE FROM EL521 - BUILDS A TEMPORARY VSAM     EL560
00027 *        FILE, LOADING ONLY THE BUSINESS RECORDS THAT ARE ON      EL560
00028 *        HOLD, RETURNED, OR IN ERROR (FATAL AND UNFORCED).        EL560
00029 *        - THIS VSAM FILE IS INPUT TO EL562.                      EL560
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
122002******************************************************************
00030                                                                   EL560
00031  EJECT                                                            EL560
00032  ENVIRONMENT DIVISION.                                            EL560
00033  INPUT-OUTPUT SECTION.                                            EL560
00034  FILE-CONTROL.                                                    EL560
00035                                                                   EL560
00036      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   EL560
00037      SELECT EXTRACT-INTERFACE-FILE                                EL560
00038                              ASSIGN TO SYS010-UT-2400-S-SYS010.   EL560
00039      SELECT ERACCT           ASSIGN TO SYS015-FBA1-ERACCT         EL560
00040                              ORGANIZATION IS INDEXED              EL560
00041                              ACCESS IS DYNAMIC                    EL560
00042                              RECORD KEY IS AM-CONTROL-PRIMARY     EL560
00043                              FILE STATUS IS ERACCT-FILE-STATUS.   EL560
00044      SELECT DISK-DATE        ASSIGN TO SYS019-FBA1-S-SYS019.      EL560
00045      SELECT ERPNDE           ASSIGN TO SYS022-FBA1-ERPNDE         EL560
00046                              ORGANIZATION IS INDEXED              EL560
00047                              ACCESS IS DYNAMIC                    EL560
00048                              RECORD KEY IS PB-CONTROL-PRIMARY     EL560
00049                              FILE STATUS IS ERPNDE-FILE-STATUS.   EL560
00050  EJECT                                                            EL560
00051  DATA DIVISION.                                                   EL560
00052  FILE SECTION.                                                    EL560
00053                                                                   EL560
00054  FD  PRNTR                                                        EL560
00055                             COPY ELCPRTFD.                        EL560
00056                                                                   EL560
00057  EJECT                                                            EL560
00058  FD  EXTRACT-INTERFACE-FILE                                       EL560
00059      COPY ERCEXTFD.                                               EL560
00060                                                                   EL560
00061      COPY ERCEXTR.                                                EL560
00062  EJECT                                                            EL560
00063  FD  ERACCT.                                                         CL**2
00064                                                                      CL**2
00065      COPY ERCACCT.                                                EL560
00066  EJECT                                                            EL560
00067  FD  DISK-DATE                                                    EL560
00068                                  COPY ELCDTEFD.                   EL560
00069                                                                   EL560
00070  FD  ERPNDE.                                                         CL**2
00071                                                                      CL**2
00072      COPY ERCPNDB.                                                EL560
00073  EJECT                                                            EL560
00074  WORKING-STORAGE SECTION.                                         EL560
00075  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      EL560
00076  77  FILLER  PIC X(32)   VALUE '********************************'.EL560
00077  77  FILLER  PIC X(32)   VALUE '*     EL560  WORKING STORAGE   *'.EL560
00078  77  FILLER  PIC X(32)   VALUE '********* VMOD=2.011 ***********'.EL560
00079                                                                   EL560
00080  01  FILLER              COMP-3.                                  EL560
00081      12  WS-RECORD-COUNT             PIC S9(7)   VALUE ZERO.      EL560
00082      12  WS-EXTRACT-COUNT            PIC S9(7)   VALUE ZERO.      EL560
00083      12  WS-DELETE-COUNT             PIC S9(7)   VALUE ZERO.      EL560
00084      12  WS-ISSUE-COUNT              PIC S9(7)   VALUE ZERO.      EL560
00085      12  WS-CANCEL-COUNT             PIC S9(7)   VALUE ZERO.      EL560
00086      12  WS-DUP-COUNT                PIC S9(7)   VALUE ZERO.      EL560
00087      12  MAX-DUP-ENTRIES             PIC S9(5)   VALUE +50.       EL560
00088      12  WS-AGENT-OVERRIDE-COUNT     PIC S9(7)   VALUE ZERO.      EL560
00089      12  WS-NOT-FOUND-COUNT          PIC S9(7)   VALUE ZERO.      EL560
00090      12  WS-LINE-COUNT               PIC S9(3)   VALUE +99.       EL560
00091      12  WS-LINE-COUNT-MAX           PIC S9(3)   VALUE +60.       EL560
00092      12  WS-PAGE                     PIC S9(5)   VALUE ZERO.      EL560
00093      12  WS-RETURN-CODE              PIC S9(3)   VALUE ZERO.      EL560
00094      12  WS-ZERO                     PIC S9      VALUE ZERO.      EL560
00095      12  WS-2619-COUNT               PIC S9(7)   VALUE ZERO.      EL560
00096      12  WS-2601-COUNT               PIC S9(7)   VALUE ZERO.      EL560
00097      12  PRT-SUB                     PIC S9(5)   VALUE ZERO.      EL560
00098                                                                   EL560
00099  01  ACCTINDX                        PIC S9(4)   COMP VALUE ZERO. EL560
00100                                                                   EL560
00101  01  WS-TOTALS.                                                   EL560
00102      12  WS-TOTAL-1.                                              EL560
00103          16  FILLER                  PIC X(40)   VALUE            EL560
00104              ' PENDING BUSINESS EXTRACTS IN - '.                  EL560
00105          16  WS-TOTAL-EXTRACTS       PIC Z(7)   VALUE ZERO.       EL560
00106      12  WS-TOTAL-2.                                              EL560
00107          16  FILLER                  PIC X(40)   VALUE            EL560
00108              ' PENDING BUSINESS ISSUES  OUT - '.                  EL560
00109          16  WS-TOTAL-ISSUES         PIC Z(7)   VALUE ZERO.       EL560
00110      12  WS-TOTAL-3.                                              EL560
00111          16  FILLER                  PIC X(40)   VALUE            EL560
00112              ' PENDING BUSINESS CANCELS  OUT - '.                 EL560
00113          16  WS-TOTAL-CANCELS        PIC Z(7)   VALUE ZERO.       EL560
00114      12  WS-TOTAL-4.                                              EL560
00115          16  FILLER                  PIC X(40)   VALUE            EL560
00116              ' AGENT NUMBERS CHANGED         - '.                 EL560
00117          16  WS-TOTAL-OVERRIDE       PIC Z(7)   VALUE ZERO.       EL560
00118      12  WS-TOTAL-5.                                              EL560
00119          16  FILLER                  PIC X(40)   VALUE            EL560
00120              ' RECORDS DROPPED - NO ACCOUNT  - '.                 EL560
00121          16  WS-TOTAL-NOT-FOUND      PIC Z(7)   VALUE ZERO.       EL560
00122      12  WS-TOTAL-6.                                              EL560
00123          16  FILLER                  PIC X(40)   VALUE            EL560
00124              ' RECORDS WITH 2619 ERRORS      - '.                 EL560
00125          16  WS-2619-ERRORS          PIC Z(7)   VALUE ZERO.       EL560
00126      12  WS-TOTAL-7.                                              EL560
00127          16  FILLER                  PIC X(40)   VALUE            EL560
00128              ' RECORDS WITH 2601 ERRORS      - '.                 EL560
00129          16  WS-2601-ERRORS          PIC Z(7)   VALUE ZERO.       EL560
00130                                                                   EL560
00131  01  FILLER              COMP  SYNC.                              EL560
00132      12  PGM-SUB                     PIC S9(4)   VALUE +560.      EL560
00133      12  WS-INDEX                    PIC S9(4)   VALUE ZERO.      EL560
00134      12  WS-LENGTH  REDEFINES                                     EL560
00135          WS-INDEX                    PIC S9(4).                   EL560
00136                                                                   EL560
00137  01  FILLER.                                                      EL560
00138      12  WS-ERROR-2619               PIC S9(4) COMP VALUE +2619.  EL560
00139      12  WS-ERROR-2601               PIC S9(4) COMP VALUE +2601.  EL560
00140      12  WS-DISPLAY-TIME             PIC 99B99B99.                EL560
00141      12  X                           PIC X.                       EL560
00142      12  ABEND-CODE                  PIC X(4).                    EL560
00143      12  ABEND-OPTION                PIC X.                       EL560
00144      12  WS-SAVE-PRINT-RECORD        PIC X(133)  VALUE SPACES.    EL560
00145      12  WS-LAST-CARRIER             PIC X VALUE LOW-VALUES.      EL560
00146      12  WS-ABEND-MESSAGE            PIC X(80)   VALUE SPACES.    EL560
00147      12  WS-ABEND-FILE-STATUS        PIC XX      VALUE ZERO.      EL560
00148      12  ERPNDE-FILE-STATUS.                                      EL560
00149          16  ERPNDE-STAT-1           PIC X       VALUE ZERO.      EL560
00150          16  ERPNDE-STAT-2           PIC X       VALUE ZERO.      EL560
00151      12  ERACCT-FILE-STATUS.                                      EL560
00152          16  ERACCT-STAT-1           PIC X       VALUE ZERO.      EL560
00153          16  ERACCT-STAT-2           PIC X       VALUE ZERO.      EL560
00154                                                                   EL560
00155  01  WS-CONTROL-PRIMARY.                                          EL560
00156      12  WS-COMPANY-CD      PIC X.                                EL560
00157      12  FILLER             PIC X(25).                            EL560
00158                                                                   EL560
00159  01  WS-AM-CONTROL-PRIMARY.                                       EL560
00160      12  WS-AM-CONTROLS.                                          EL560
00161          16  WS-AM-COMPANY-CD        PIC X.                       EL560
00162          16  WS-AM-CARRIER           PIC X.                       EL560
00163          16  WS-AM-GROUPING          PIC X(6).                    EL560
00164          16  WS-AM-STATE             PIC XX.                      EL560
00165          16  WS-AM-ACCOUNT           PIC X(10).                   EL560
00166      12  WS-AM-EXPIRE-DATE.                                       EL560
00167          16  WS-AM-EXPIRATION-DT     PIC XX.                      EL560
00168          16  FILLER                  PIC X(4).                    EL560
00169  01  WS-LAST-AM-CONTROLS             PIC X(20).                   EL560
00170  EJECT                                                            EL560
00171  01  WS-HEADING1.                                                 EL560
00172      12  FILLER                      PIC X(52)   VALUE '1'.       EL560
00173      12  FILLER                      PIC X(68)   VALUE            EL560
00174          'EXTRACT INTERFACE FILE TOTALS'.                         EL560
00175      12  FILLER                      PIC X(9) VALUE 'EL560  '.    EL560
00176                                                                   EL560
00177  01  WS-HEADING2.                                                 EL560
00178      12  FILLER                      PIC X       VALUE SPACES.    EL560
00179      12  WS-H2-COMPANY-ID            PIC XXX     VALUE 'XXX'.     EL560
00180      12  FILLER                      PIC X(47)   VALUE SPACES.    EL560
00181      12  WS-H2-CLIENT-NAME           PIC X(30)   VALUE SPACES.    EL560
00182      12  FILLER                      PIC X(39)   VALUE SPACES.    EL560
00183      12  WS-H2-DATE                  PIC X(08)   VALUE SPACES.    EL560
00184                                                                   EL560
00185  01  WS-HEADING3.                                                 EL560
00186      12  FILLER                      PIC X(56)   VALUE SPACES.    EL560
00187      12  WS-H3-DATE                  PIC X(64)   VALUE SPACES.    EL560
00188      12  FILLER                      PIC X(5)    VALUE 'PAGE'.    EL560
00189      12  WS-H3-PAGE                  PIC ZZ,ZZ9  VALUE ZERO.      EL560
00190      12  FILLER                      PIC X(02)   VALUE SPACES.    EL560
00191                                                                   EL560
00192  01  WS-HEADING4                     PIC X(132)  VALUE SPACES.    EL560
00193                                                                   EL560
00194  01  WS-DETAIL1.                                                  EL560
00195      12  FILLER             PIC X.                                EL560
00196      12  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD'.    EL560
00197      12  DTL-MSG2           PIC X(18) VALUE SPACE.                EL560
00198      12  DTL-MSG3           PIC X(18) VALUE SPACE.                EL560
00199      12  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL560
00200      12  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL560
00201      12  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL560
00202      12  FILLER             PIC X(11) VALUE ' DELETED)'.          EL560
00203  EJECT                                                            EL560
00204  01  DUP-HEADER.                                                  EL560
00205      12  FILLER             PIC X(58) VALUE SPACES.               EL560
00206      12  FILLER             PIC X(17) VALUE                       EL560
00207          'DUPLICATE RECORDS'.                                     EL560
00208      12  FILLER             PIC X(57) VALUE SPACES.               EL560
00209                                                                   EL560
00210  01  DUP-COL-HEADERS.                                             EL560
00211      12  FILLER             PIC X(04) VALUE SPACES.               EL560
00212      12  FILLER             PIC X(11) VALUE 'ENTRY BATCH'.        EL560
00213      12  FILLER             PIC X(03) VALUE SPACES.               EL560
00214      12  FILLER             PIC X(07) VALUE 'CARRIER'.            EL560
00215      12  FILLER             PIC X(03) VALUE SPACES.               EL560
00216      12  FILLER             PIC X(08) VALUE 'GROUPING'.           EL560
00217      12  FILLER             PIC X(03) VALUE SPACES.               EL560
00218      12  FILLER             PIC X(05) VALUE 'STATE'.              EL560
00219      12  FILLER             PIC X(03) VALUE SPACES.               EL560
00220      12  FILLER             PIC X(14) VALUE 'ACCOUNT NUMBER'.     EL560
00221      12  FILLER             PIC X(02) VALUE SPACES.               EL560
00222      12  FILLER             PIC X(14) VALUE 'FIN. RESP. NO.'.     EL560
00223      12  FILLER             PIC X(03) VALUE SPACES.               EL560
00224      12  FILLER             PIC X(13) VALUE 'CERT EFF DATE'.      EL560
00225      12  FILLER             PIC X(03) VALUE SPACES.               EL560
00226      12  FILLER             PIC X(11) VALUE 'CERT NUMBER'.        EL560
00227      12  FILLER             PIC X(25) VALUE SPACES.               EL560
00228                                                                   EL560
00229  01  PRINT-DUP-DETAIL.                                            EL560
00230      12  DETAIL-DUP-LINE   OCCURS   50 TIMES.                     EL560
00231          16  FILLER            PIC X(07).                         EL560
00232          16  DTL-BATCH-NO      PIC X(06).                         EL560
00233          16  FILLER            PIC X(08).                         EL560
00234          16  DTL-CARRIER       PIC X(01).                         EL560
00235          16  FILLER            PIC X(07).                         EL560
00236          16  DTL-GROUPING      PIC X(06).                         EL560
00237          16  FILLER            PIC X(06).                         EL560
00238          16  DTL-STATE         PIC X(02).                         EL560
00239          16  FILLER            PIC X(06).                         EL560
00240          16  DTL-ACCT-NO       PIC X(10).                         EL560
00241          16  FILLER            PIC X(06).                         EL560
00242          16  DTL-GA-NO         PIC X(10).                         EL560
00243          16  FILLER            PIC X(07).                         EL560
00244          16  DTL-CERT-EFF-DT   PIC X(08).                         EL560
00245          16  FILLER            PIC X(07).                         EL560
00246          16  DTL-CERT-NUMBER   PIC X(11).                         EL560
00247  EJECT                                                            EL560
00248      COPY ELCDATE.                                                EL560
00249  EJECT                                                            EL560
00250      COPY ELCDTECX.                                               EL560
00251  EJECT                                                            EL560
00252      COPY ELCDTEVR.                                               EL560
00253  EJECT                                                            EL560
00254  PROCEDURE DIVISION.                                              EL560
00255                                                                   EL560
00256  0000-LOAD-DATE-CARD.                COPY ELCDTERX.               EL560
00257                                                                   EL560
00258  0000-MAIN-LOGIC.                                                 EL560
00259      PERFORM 1000-INITIALIZE    THRU 1999-EXIT.                   EL560
00260                                                                   EL560
00261      PERFORM 2000-DELETE-OLD    THRU 2999-EXIT.                   EL560
00262                                                                   EL560
00263      PERFORM 3000-READ-EXTRACTS THRU 3999-EXIT.                   EL560
00264                                                                   EL560
00265      PERFORM 4000-FINALIZE      THRU 4999-EXIT.                   EL560
00266                                                                   EL560
00267      GOBACK.                                                      EL560
00268  EJECT                                                            EL560
00269  1000-INITIALIZE.                                                 EL560
00270      OPEN INPUT  EXTRACT-INTERFACE-FILE                           EL560
00271                  ERACCT                                           EL560
00272           I-O    ERPNDE                                           EL560
00273           OUTPUT PRNTR.                                           EL560
00274                                                                   EL560
00275      IF ERPNDE-FILE-STATUS  = '00' OR '97'                        EL560
00276          NEXT SENTENCE                                            EL560
00277        ELSE                                                       EL560
00278          MOVE 'ERROR OCCURED OPEN  - ERPNDE'                      EL560
00279                                  TO  WS-ABEND-MESSAGE             EL560
00280          MOVE ERPNDE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL560
00281          GO TO ABEND-PGM.                                         EL560
00282                                                                   EL560
00283      IF ERACCT-FILE-STATUS  = '00' OR '97'                        EL560
00284          NEXT SENTENCE                                            EL560
00285        ELSE                                                       EL560
00286          MOVE 'ERROR OCCURED OPEN  - ERACCT'                      EL560
00287                                  TO  WS-ABEND-MESSAGE             EL560
00288          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL560
00289          GO TO ABEND-PGM.                                         EL560
00290                                                                   EL560
00291      MOVE LOW-VALUES             TO  WS-CONTROL-PRIMARY.          EL560
00292      MOVE DTE-CLASIC-COMPANY-CD  TO  WS-COMPANY-CD.               EL560
00293      MOVE DTE-CLIENT             TO  WS-H2-COMPANY-ID.            EL560
00294      MOVE ALPH-DATE              TO  WS-H3-DATE.                  EL560
00295      MOVE COMPANY-NAME           TO  WS-H2-CLIENT-NAME.           EL560
00296      MOVE WS-CURRENT-DATE        TO  WS-H2-DATE.                  EL560
00297      MOVE SPACES                 TO  PRINT-DUP-DETAIL.            EL560
00298                                                                   EL560
00299  1999-EXIT.                                                       EL560
00300       EXIT.                                                       EL560
00301  EJECT                                                            EL560
00302  2000-DELETE-OLD.                                                 EL560
00303      MOVE WS-CONTROL-PRIMARY  TO PB-CONTROL-PRIMARY.              EL560
00304                                                                   EL560
00305      START ERPNDE KEY NOT LESS PB-CONTROL-PRIMARY.                EL560
00306                                                                   EL560
00307      IF ERPNDE-FILE-STATUS  = '10' OR '23'                        EL560
00308          GO TO 2999-EXIT.                                         EL560
00309                                                                   EL560
00310      IF ERPNDE-FILE-STATUS NOT = ZERO                             EL560
00311          MOVE ERPNDE-FILE-STATUS      TO WS-ABEND-FILE-STATUS     EL560
00312          MOVE ' ERPNDE START ERROR- ' TO WS-ABEND-MESSAGE         EL560
00313          GO TO ABEND-PGM.                                         EL560
00314                                                                   EL560
00315  2100-READNEXT.                                                   EL560
00316      READ ERPNDE  NEXT RECORD.                                    EL560
00317                                                                   EL560
00318      IF ERPNDE-STAT-1 = '1'                                       EL560
00319          GO TO 2999-EXIT.                                         EL560
00320                                                                   EL560
00321      IF ERPNDE-FILE-STATUS NOT = ZEROS                            EL560
00322          MOVE ' -ERROR ON READ- ' TO WS-ABEND-MESSAGE             EL560
00323          MOVE ERPNDE-FILE-STATUS TO WS-ABEND-FILE-STATUS          EL560
00324          GO TO ABEND-PGM.                                         EL560
00325                                                                   EL560
00326      IF DTE-CLASIC-COMPANY-CD NOT = PB-COMPANY-CD-A1              EL560
00327          GO TO 2999-EXIT.                                         EL560
00328                                                                   EL560
00329      DELETE  ERPNDE  RECORD.                                      EL560
00330                                                                   EL560
00331      IF ERPNDE-STAT-1 NOT = '0'                                   EL560
00332          MOVE ERPNDE-FILE-STATUS       TO WS-ABEND-FILE-STATUS    EL560
00333          MOVE ' ERPNDE DELETE ERROR- ' TO WS-ABEND-MESSAGE        EL560
00334          GO TO ABEND-PGM.                                         EL560
00335                                                                   EL560
00336      ADD 1  TO WS-DELETE-COUNT.                                   EL560
00337                                                                   EL560
00338      GO TO 2100-READNEXT.                                         EL560
00339                                                                   EL560
00340  2999-EXIT.                                                       EL560
00341       EXIT.                                                       EL560
00342  EJECT                                                            EL560
00343  3000-READ-EXTRACTS.                                              EL560
00344      READ EXTRACT-INTERFACE-FILE                                  EL560
00345          AT END  GO TO 3999-EXIT.                                 EL560
00346                                                                   EL560
00347      ADD +1    TO  WS-EXTRACT-COUNT.                              EL560
00348                                                                   EL560
00349      IF EX-RECORD-TYPE NOT = 'A'                                  EL560
00350          GO TO 3000-READ-EXTRACTS.                                EL560
00351                                                                   EL560
00352      MOVE EX-DATA-AREAS TO PENDING-BUSINESS.                      EL560
00353                                                                   EL560
00354      IF PB-ISSUE OR PB-CANCELLATION                               EL560
00355          NEXT SENTENCE                                            EL560
00356      ELSE                                                         EL560
00357          GO TO 3000-READ-EXTRACTS.                                EL560
00358                                                                   EL560
00359      IF PB-ALT-CHG-SEQ-NO NOT = ZERO                              EL560
00360          GO TO 3000-READ-EXTRACTS.                                EL560
00361                                                                   EL560
00362      IF PB-ISSUE                                                  EL560
00363          IF PB-REIN-ONLY-CERT OR PB-REISSUED-CERT
122002*           OR PB-MONTHLY-CERT
00364              GO TO 3000-READ-EXTRACTS                             EL560
00365          ELSE                                                     EL560
00366              IF PB-POLICY-IS-VOIDED                               EL560
00367                  IF DTE-CLIENT = 'GTL'                            EL560
00368                      NEXT SENTENCE                                EL560
00369                  ELSE                                             EL560
00370                      GO TO 3000-READ-EXTRACTS.                    EL560
00371                                                                   EL560
00372      IF PB-CANCELLATION                                           EL560
00373          IF PB-REIN-ONLY-CERT                                     EL560
00374            OR PB-POLICY-IS-VOIDED                                 EL560
00375              GO TO 3000-READ-EXTRACTS.                            EL560
00376                                                                   EL560
00377      IF WS-ERROR-2619 = PB-COMMON-ERROR (1) OR                    EL560
00378                         PB-COMMON-ERROR (2) OR                    EL560
00379                         PB-COMMON-ERROR (3) OR                    EL560
00380                         PB-COMMON-ERROR (4) OR                    EL560
00381                         PB-COMMON-ERROR (5) OR                    EL560
00382                         PB-COMMON-ERROR (6) OR                    EL560
00383                         PB-COMMON-ERROR (7) OR                    EL560
00384                         PB-COMMON-ERROR (8) OR                    EL560
00385                         PB-COMMON-ERROR (9) OR                    EL560
00386                         PB-COMMON-ERROR (10)                      EL560
00387          ADD +1 TO WS-2619-COUNT                                  EL560
00388          GO TO 3000-READ-EXTRACTS.                                EL560
00389                                                                   EL560
00390      IF WS-ERROR-2601 = PB-COMMON-ERROR (1) OR                    EL560
00391                         PB-COMMON-ERROR (2) OR                    EL560
00392                         PB-COMMON-ERROR (3) OR                    EL560
00393                         PB-COMMON-ERROR (4) OR                    EL560
00394                         PB-COMMON-ERROR (5) OR                    EL560
00395                         PB-COMMON-ERROR (6) OR                    EL560
00396                         PB-COMMON-ERROR (7) OR                    EL560
00397                         PB-COMMON-ERROR (8) OR                    EL560
00398                         PB-COMMON-ERROR (9) OR                    EL560
00399                         PB-COMMON-ERROR (10)                      EL560
00400          ADD +1 TO WS-2601-COUNT                                  EL560
00401          GO TO 3000-READ-EXTRACTS.                                EL560
00402                                                                   EL560
00403      IF PB-CANCELLATION                                           EL560
00404          IF PB-C-REFUND-CREATED                                   EL560
00405              GO TO 3100-WRITE-IT.                                 EL560
00406                                                                   EL560
00407      IF PB-POLICY-IS-DECLINED                                     EL560
00408          GO TO 3100-WRITE-IT.                                     EL560
00409                                                                   EL560
00410      IF PB-POLICY-IS-VOIDED                                       EL560
00411          GO TO 3100-WRITE-IT.                                     EL560
00412                                                                   EL560
00413      IF PB-RECORD-BILL NOT = SPACE                                EL560
00414          GO TO 3100-WRITE-IT.                                     EL560
00415                                                                   EL560
00416      IF PB-FATAL-ERRORS   OR                                      EL560
00417         PB-UNFORCED-ERRORS                                        EL560
00418          GO TO 3100-WRITE-IT.                                     EL560
00419                                                                   EL560
00420      GO TO 3000-READ-EXTRACTS.                                    EL560
00421                                                                   EL560
00422  3100-WRITE-IT.                                                   EL560
00423      IF PB-CARRIER = SPACE                                        EL560
00424          MOVE PB-SV-CARRIER   TO PB-CARRIER.                      EL560
00425                                                                   EL560
00426      IF PB-GROUPING = SPACES                                      EL560
00427          MOVE PB-SV-GROUPING  TO PB-GROUPING.                     EL560
00428                                                                   EL560
00429      IF PB-STATE = SPACES                                         EL560
00430          MOVE PB-SV-STATE     TO PB-STATE.                        EL560
00431                                                                   EL560
00432      MOVE LOW-VALUES          TO WS-AM-CONTROL-PRIMARY.           EL560
00433      MOVE PB-COMPANY-CD       TO WS-AM-COMPANY-CD.                EL560
00434      MOVE PB-CARRIER          TO WS-AM-CARRIER.                   EL560
00435      MOVE PB-GROUPING         TO WS-AM-GROUPING.                  EL560
00436      MOVE PB-STATE            TO WS-AM-STATE.                     EL560
00437      MOVE PB-ACCOUNT          TO WS-AM-ACCOUNT.                   EL560
00438      MOVE PB-CERT-EFF-DT      TO WS-AM-EXPIRATION-DT.             EL560
00439                                                                   EL560
00440      IF (WS-AM-CONTROLS = WS-LAST-AM-CONTROLS) AND                EL560
00441          (PB-CERT-EFF-DT GREATER THAN AM-EFFECTIVE-DT AND         EL560
00442           PB-CERT-EFF-DT NOT GREATER THAN AM-EXPIRATION-DT)       EL560
00443          GO TO 3200-GET-AGENT-NO.                                 EL560
00444                                                                   EL560
00445      MOVE WS-AM-CONTROL-PRIMARY TO AM-CONTROL-PRIMARY.            EL560
00446                                                                   EL560
00447      START ERACCT KEY NOT LESS THAN AM-CONTROL-PRIMARY.           EL560
00448                                                                   EL560
00449      IF ERACCT-FILE-STATUS = '10' OR '23'                         EL560
00450          MOVE SPACES                     TO AM-CONTROL-PRIMARY    EL560
00451          ADD +1 TO WS-NOT-FOUND-COUNT                             EL560
00452          GO TO 3000-READ-EXTRACTS                                 EL560
00453      ELSE                                                         EL560
00454          IF ERACCT-FILE-STATUS NOT = '00'                         EL560
00455              MOVE ERACCT-FILE-STATUS       TO WS-ABEND-FILE-STATUSEL560
00456              MOVE ' ERACCT READ ERROR- ' TO WS-ABEND-MESSAGE      EL560
00457              GO TO ABEND-PGM.                                     EL560
00458                                                                   EL560
00459  3200-READ-NEXT.                                                  EL560
00460                                                                   EL560
00461      READ ERACCT NEXT RECORD.                                     EL560
00462                                                                   EL560
00463      IF ERACCT-FILE-STATUS = '10'                                 EL560
00464          MOVE SPACES             TO AM-CONTROL-PRIMARY            EL560
00465          ADD +1 TO WS-NOT-FOUND-COUNT                             EL560
00466          GO TO 3000-READ-EXTRACTS.                                EL560
00467                                                                   EL560
00468      IF PB-COMPANY-CD EQUAL AM-COMPANY-CD AND                     EL560
00469         PB-CARRIER    EQUAL AM-CARRIER    AND                     EL560
00470         PB-GROUPING   EQUAL AM-GROUPING   AND                     EL560
00471         PB-STATE      EQUAL AM-STATE      AND                     EL560
00472         PB-ACCOUNT    EQUAL AM-ACCOUNT                            EL560
00473          NEXT SENTENCE                                            EL560
00474      ELSE                                                         EL560
00475          GO TO 3200-READ-NEXT.                                    EL560
00476                                                                   EL560
00477      IF PB-CERT-EFF-DT NOT LESS THAN AM-EFFECTIVE-DT AND          EL560
00478         PB-CERT-EFF-DT NOT GREATER THAN AM-EXPIRATION-DT          EL560
00479          NEXT SENTENCE                                            EL560
00480      ELSE                                                         EL560
00481          GO TO 3200-READ-NEXT.                                    EL560
00482                                                                   EL560
00483  3200-GET-AGENT-NO.                                               EL560
00484      MOVE +1                     TO ACCTINDX.                     EL560
00485      MOVE WS-AM-CONTROLS         TO WS-LAST-AM-CONTROLS.          EL560
00486                                                                   EL560
00487  3500-AGENT-LOOK-LOOP.                                            EL560
00488                                                                   EL560
052814     IF AM-COM-TYP (ACCTINDX) = 'C' OR 'D' OR 'F'
00490          IF AM-AGT (ACCTINDX) NOT = PB-ACCOUNT                    EL560
00491              ADD +1 TO WS-AGENT-OVERRIDE-COUNT                    EL560
00492              MOVE AM-AGT (ACCTINDX)  TO PB-ACCOUNT                EL560
00493          ELSE                                                     EL560
00494              NEXT SENTENCE                                        EL560
00495      ELSE                                                         EL560
00496          ADD +1 TO ACCTINDX                                       EL560
00497          GO TO 3500-AGENT-LOOK-LOOP.                              EL560
00498                                                                   EL560
00499      ADD +1     TO  WS-RECORD-COUNT.                              EL560
00500                                                                   EL560
00501      IF PB-ISSUE                                                  EL560
00502          ADD +1    TO  WS-ISSUE-COUNT.                            EL560
00503                                                                   EL560
00504      IF PB-CANCELLATION                                           EL560
00505          ADD +1    TO  WS-CANCEL-COUNT.                           EL560
00506                                                                   EL560
00507      MOVE AM-REMIT-TO            TO  ACCTINDX.                    EL560
00508      MOVE AM-AGT (ACCTINDX)      TO  PB-SV-REMIT-TO.              EL560
00509                                                                   EL560
00510      WRITE PENDING-BUSINESS.                                      EL560
00511                                                                   EL560
00512      IF ERPNDE-FILE-STATUS  =  '22'                               EL560
00513        IF WS-DUP-COUNT LESS THAN MAX-DUP-ENTRIES                  EL560
00514          ADD +1                    TO  WS-DUP-COUNT               EL560
00515          ADD +1                    TO  PRT-SUB                    EL560
00516          MOVE PB-ENTRY-BATCH       TO  DTL-BATCH-NO (PRT-SUB)     EL560
00517          MOVE PB-CARRIER           TO  DTL-CARRIER  (PRT-SUB)     EL560
00518          MOVE PB-GROUPING          TO  DTL-GROUPING (PRT-SUB)     EL560
00519          MOVE PB-STATE             TO  DTL-STATE    (PRT-SUB)     EL560
00520          MOVE AM-ACCOUNT           TO  DTL-ACCT-NO  (PRT-SUB)     EL560
00521          MOVE PB-ACCOUNT           TO  DTL-GA-NO    (PRT-SUB)     EL560
00522          MOVE PB-CERT-EFF-DT       TO  DC-BIN-DATE-1              EL560
00523          MOVE ' '                  TO  DC-OPTION-CODE             EL560
00524          PERFORM 8500-DATE-CONVERSION THRU 8590-EXIT              EL560
00525          MOVE DC-GREG-DATE-1-EDIT  TO  DTL-CERT-EFF-DT (PRT-SUB)  EL560
00526          MOVE PB-CERT-NO           TO  DTL-CERT-NUMBER (PRT-SUB)  EL560
00527          GO TO 3000-READ-EXTRACTS                                 EL560
00528        ELSE                                                       EL560
00529          GO TO 3000-READ-EXTRACTS.                                EL560
00530                                                                   EL560
00531      IF ERPNDE-FILE-STATUS NOT = '00'                             EL560
00532          MOVE 'ERROR OCCURED WRITE - ERPNDE'                      EL560
00533                                  TO  WS-ABEND-MESSAGE             EL560
00534          MOVE ERPNDE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL560
00535          GO TO ABEND-PGM.                                         EL560
00536                                                                   EL560
00537      GO TO 3000-READ-EXTRACTS.                                    EL560
00538                                                                   EL560
00539  3999-EXIT.                                                       EL560
00540       EXIT.                                                       EL560
00541  EJECT                                                            EL560
00542                                                                   EL560
00543  4000-FINALIZE.                                                   EL560
00544      CLOSE EXTRACT-INTERFACE-FILE                                 EL560
00545            ERPNDE                                                 EL560
00546            ERACCT.                                                EL560
00547                                                                   EL560
00548      IF ERPNDE-FILE-STATUS NOT = ZEROS                            EL560
00549          MOVE 'ERROR OCCURED CLOSE - ERPNDE'                      EL560
00550                                  TO  WS-ABEND-MESSAGE             EL560
00551          MOVE ERPNDE-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL560
00552          GO TO ABEND-PGM.                                         EL560
00553                                                                   EL560
00554      IF ERACCT-FILE-STATUS NOT = ZEROS                            EL560
00555          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      EL560
00556                                  TO  WS-ABEND-MESSAGE             EL560
00557          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         EL560
00558          GO TO ABEND-PGM.                                         EL560
00559                                                                   EL560
00560      MOVE WS-RECORD-COUNT      TO  D-RECORD-COUNT.                EL560
00561      MOVE WS-DELETE-COUNT      TO  D-DELETE-COUNT.                EL560
00562      MOVE WS-DETAIL1           TO  PRT.                           EL560
CIDMOD     MOVE '0'                  TO  P-CTL.                         EL560
00563                                                                   EL560
00564      PERFORM WRITE-A-LINE.                                        EL560
00565                                                                   EL560
00566      MOVE WS-EXTRACT-COUNT         TO  WS-TOTAL-EXTRACTS.         EL560
00567      MOVE WS-ISSUE-COUNT           TO  WS-TOTAL-ISSUES.           EL560
00568      MOVE WS-CANCEL-COUNT          TO  WS-TOTAL-CANCELS.          EL560
00569      MOVE WS-AGENT-OVERRIDE-COUNT  TO  WS-TOTAL-OVERRIDE.         EL560
00570      MOVE WS-NOT-FOUND-COUNT       TO  WS-TOTAL-NOT-FOUND.        EL560
00571      MOVE WS-2619-COUNT            TO  WS-2619-ERRORS.            EL560
00572      MOVE WS-2601-COUNT            TO  WS-2601-ERRORS.            EL560
00573      MOVE WS-TOTAL-1           TO  PRT.                           EL560
00574                                                                   EL560
00575      PERFORM WRITE-A-LINE.                                        EL560
00576                                                                   EL560
00577      MOVE WS-TOTAL-2           TO  PRT.                           EL560
00578      PERFORM WRITE-A-LINE.                                        EL560
00579                                                                   EL560
00580      MOVE WS-TOTAL-3           TO  PRT.                           EL560
00581      PERFORM WRITE-A-LINE.                                        EL560
00582                                                                   EL560
00583      MOVE WS-TOTAL-4           TO  PRT.                           EL560
00584      PERFORM WRITE-A-LINE.                                        EL560
00585                                                                   EL560
00586      MOVE WS-TOTAL-5           TO  PRT.                           EL560
00587      PERFORM WRITE-A-LINE.                                        EL560
00588                                                                   EL560
00589      MOVE WS-TOTAL-6           TO  PRT.                           EL560
00590      PERFORM WRITE-A-LINE.                                        EL560
00591                                                                   EL560
00592      MOVE WS-TOTAL-7           TO  PRT.                           EL560
00593      PERFORM WRITE-A-LINE.                                        EL560
00594                                                                   EL560
00595      IF WS-DUP-COUNT  GREATER THAN  ZEROS                         EL560
00596          MOVE DUP-HEADER        TO  PRT                           EL560
00597 **       MOVE  1                TO  P-CTL                         EL560
CIDMOD         MOVE '-'               TO  P-CTL                         EL560
00598          PERFORM WRITE-A-LINE                                     EL560
00599          MOVE DUP-COL-HEADERS   TO  PRT                           EL560
00600          MOVE '0'               TO  P-CTL                         EL560
00601          PERFORM WRITE-A-LINE                                     EL560
00602          PERFORM 5000-PRINT-DUP-RECORDS  THRU  5000-EXIT          EL560
00603             VARYING PRT-SUB  FROM +1 BY +1                        EL560
00604                UNTIL PRT-SUB  GREATER THAN  WS-DUP-COUNT          EL560
00605      ELSE                                                         EL560
00606          MOVE DUP-HEADER        TO  PRT                           EL560
00607 **       MOVE  1                TO  P-CTL                         EL560
CIDMOD         MOVE '-'               TO  P-CTL                         EL560
00608          PERFORM WRITE-A-LINE                                     EL560
00609          MOVE DUP-COL-HEADERS   TO  PRT                           EL560
00610          MOVE '0'               TO  P-CTL                         EL560
00611          PERFORM WRITE-A-LINE                                     EL560
CIDMOD**       MOVE  0                TO  P-CTL                         EL560
00613          MOVE ' NO DUPLICATE RECORDS THIS RUN'  TO  PRT           EL560
00614          MOVE '0'               TO  P-CTL                         EL560
00615          PERFORM WRITE-A-LINE.                                    EL560
00616                                                                   EL560
00617      CLOSE PRNTR.                                                 EL560
00618                                                                   EL560
00619  4999-EXIT.                                                       EL560
00620       EXIT.                                                       EL560
00621  EJECT                                                            EL560
00622  5000-PRINT-DUP-RECORDS.                                          EL560
00623                                                                   EL560
00624      MOVE DETAIL-DUP-LINE (PRT-SUB)   TO  PRT.                    EL560
00625 **   MOVE  1                          TO  P-CTL.                  EL560
CIDMOD     MOVE ' '                         TO  P-CTL.                  EL560
00626      PERFORM WRITE-A-LINE.                                        EL560
00627                                                                   EL560
00628  5000-EXIT.                                                       EL560
00629       EXIT.                                                       EL560
00630       EJECT                                                       EL560
00631  8500-DATE-CONVERSION SECTION. COPY ELCDCS.                       EL560
00632  EJECT                                                            EL560
00633  WRITE-A-LINE SECTION.         COPY ELCWAL.                       EL560
00634                                                                   EL560
00635  WRITE-HEADINGS SECTION.                                          EL560
00636 *****************************************************************.EL560
00637 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL560
00638 *****************************************************************.EL560
00639  WHS-010.                                                         EL560
00640                                                                   EL560
00641      ADD +1                      TO  WS-PAGE.                     EL560
00642      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL560
00643      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL560
00644 **   MOVE ZERO                   TO  WS-LINE-COUNT.               EL560
CIDMOD     MOVE  +4                    TO  WS-LINE-COUNT.               EL560
00645                                                                   EL560
00646      MOVE WS-HEADING1            TO  PRT.                         EL560
CIDMOD*    MOVE '1'                    TO  X.                           EL560
00648      PERFORM WRITE-PRINTER.                                       EL560
00649                                                                   EL560
00650      MOVE WS-HEADING2            TO  PRT.                         EL560
CIDMOD*    MOVE ' '                    TO  X.                           EL560
00652      PERFORM WRITE-PRINTER.                                       EL560
00653                                                                   EL560
00654      MOVE WS-HEADING3            TO  PRT.                         EL560
CIDMOD*    MOVE ' '                    TO  X.                           EL560
00656      PERFORM WRITE-PRINTER.                                       EL560
00657                                                                   EL560
00658      MOVE WS-HEADING4            TO  PRT.                         EL560
CIDMOC*    MOVE ' '                    TO  X.                           EL560
00660      PERFORM WRITE-PRINTER.                                       EL560
00661                                                                   EL560
00662  WHS-020.                                                         EL560
00663      MOVE WS-SAVE-PRINT-RECORD   TO  PRT.                         EL560
CIDMOD*    MOVE  1                     TO  P-CTL.                       EL560
00665                                                                   EL560
00666  WHS-EXIT.                                                        EL560
00667      EXIT.                                                        EL560
00668                                                                   EL560
00669  WRITE-PRINTER SECTION.                                           EL560
00670                                                                   EL560
00671      MOVE P-CTL                TO X.                              EL560
CIDMOD     MOVE  ' '                 TO P-CTL.                          EL560
00672                                                                   EL560
00673 **   IF P-CTL   EQUAL 1                                           EL560
CIDMOD     IF  X      EQUAL ' '                                         EL560
00674          WRITE PRT   AFTER ADVANCING 1 LINE                       EL560
00675      ELSE                                                         EL560
00676 **       IF P-CTL   EQUAL '0'                                     EL560
CIDMOD         IF  X      EQUAL '0'                                     EL560
00677              WRITE PRT   AFTER ADVANCING 2 LINE                   EL560
00678          ELSE                                                     EL560
00679 **           IF P-CTL   EQUAL  3                                  EL560
CIDMOD             IF  X      EQUAL '-'                                 EL560
00680                  WRITE PRT   AFTER ADVANCING 3 LINE               EL560
00681              ELSE                                                 EL560
00682                  WRITE PRT   AFTER ADVANCING PAGE.                EL560
00683                                                                   EL560
00684  WPS-EXIT.                                                        EL560
00685      EXIT.                                                        EL560
00686                                                                   EL560
00687  ABEND-PGM SECTION. COPY ELCABEND.                                EL560
