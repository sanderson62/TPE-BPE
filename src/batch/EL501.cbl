00001  IDENTIFICATION DIVISION.                                         06/05/98
00002                                                                   EL501
00003  PROGRAM-ID.                 EL501 .                                 LV014
00004 *              PROGRAM CONVERTED BY                               EL501
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   EL501
00006 *              CONVERSION DATE 02/19/96 16:24:14.                 EL501
00007 *           PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE             EL501
00008 *                          VMOD=2.010                             EL501
00009                                                                   EL501
00009                                                                   EL501
00010 *AUTHOR.     LOGIC INC.                                           EL501
00011 *            DALLAS, TEXAS.                                       EL501
00012                                                                   EL501
00013 *DATE-COMPILED.                                                   EL501
00014                                                                   EL501
00015 *SECURITY.   *****************************************************EL501
00016 *            *                                                   *EL501
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *EL501
00018 *            *                                                   *EL501
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *EL501
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *EL501
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *EL501
00022 *            *                                                   *EL501
00023 *            *****************************************************EL501
00024                                                                   EL501
00025 *REMARKS.                                                         EL501
00026 *         THIS PROGRAM IS USED TO CREATE THE ONLINE ACCOUNT MASTER   CL**8
00027 *       (ERACCT).  A CONTROL PAGE WILL BE PRINTED AT EOJ.            CL**8
00028 *                                                                    CL**8
00029 *         THE NON VSAM INPUT IS USED BY THIS MODULE.              EL501
00030                                                                   EL501
102004******************************************************************
102004*                   C H A N G E   L O G
102004*
102004* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
102004*-----------------------------------------------------------------
102004*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
102004* EFFECTIVE    NUMBER
102004*-----------------------------------------------------------------
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
061907* 061907                   PEMA  ADD PGM OPTION 2 PROCESSING
022808* 022808  CR2007083100002  PEMA  ADD 'F' ACCT STATUS
031811* 031811  CR2011012700001  PEMA  ADD ACCT STATUS S - SUSPENDED
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
102004******************************************************************
00031  ENVIRONMENT DIVISION.                                            EL501
00032                                                                   EL501
00033  INPUT-OUTPUT SECTION.                                            EL501
00034                                                                   EL501
00035  FILE-CONTROL.                                                    EL501
00036                                                                   EL501
00037      SELECT PRNTR          ASSIGN TO SYS008-UR-1403-S-SYS008.     EL501
00038                                                                   EL501
00039      SELECT FICH           ASSIGN TO SYS020-UT-2400-S-SYS020.     EL501
00040                                                                   EL501
00041      SELECT DISK-DATE      ASSIGN TO SYS019-UT-FBA1-S-SYS019.     EL501
00042                                                                   EL501
00043      SELECT AM-MSTR       ASSIGN TO SYS010-UT-2400-S-SYS010.      EL501
00044                                                                   EL501
00045      SELECT ERACCT        ASSIGN TO SYS011-FBA1-ERACCT               CL**9
00046              ORGANIZATION IS INDEXED                              EL501
00047              ACCESS IS DYNAMIC                                    EL501
00048              RECORD KEY IS AM-CONTROL-PRIMARY                     EL501
00049              FILE STATUS IS ACCOUNT-FILE-STATUS.                  EL501
00050                                                                   EL501
00051      SELECT ELCNTL         ASSIGN TO SYS012-FBA1-ELCNTL           EL501
00052              ORGANIZATION IS INDEXED                              EL501
00053              ACCESS IS DYNAMIC                                    EL501
00054              RECORD KEY IS CF-CONTROL-PRIMARY                     EL501
00055              FILE STATUS IS CONTROL-FILE-STATUS.                  EL501
00056                                                                   EL501
00057      SELECT ELREPT         ASSIGN TO SYS018-FBA1-ELREPT           EL501
00058              ORGANIZATION IS INDEXED                              EL501
00059              ACCESS IS DYNAMIC                                    EL501
00060              RECORD KEY IS RF-CONTROL-PRIMARY                     EL501
00061              FILE STATUS IS DTE-VSAM-FLAGS.                       EL501
00062                                                                   EL501
00063      SELECT ERMEBL                                                EL501
00064              ASSIGN SYS024-FBA1-ERMEBL                            EL501
00065              ORGANIZATION INDEXED                                 EL501
00066              ACCESS DYNAMIC                                       EL501
00067              RECORD KEY ME-CONTROL-PRIMARY                        EL501
00068              FILE STATUS ERMEBL-FILE-STATUS.                      EL501
00069                                                                   EL501
00070      EJECT                                                        EL501
00071  DATA DIVISION.                                                   EL501
00072                                                                   EL501
00073  FILE SECTION.                                                    EL501
00074                                                                   EL501
00075  FD  PRNTR                   COPY ELCPRTFD.                       EL501
00076                                                                   EL501
00077  FD  FICH                    COPY ELCFCHFD.                       EL501
00078                                                                   EL501
00079  FD  DISK-DATE               COPY ELCDTEFD.                       EL501
00080                                                                   EL501
00081      EJECT                                                        EL501
00082  FD  AM-MSTR                                                      EL501
00083      BLOCK CONTAINS 0 RECORDS
00084      RECORDING MODE F.                                            EL501
00085  01  AM-MSTR-REC         PIC X(2000).                             EL501
00086                                                                   EL501
00087  FD  ERACCT.                                                         CL**9
00088                            COPY ERCACCT.                          EL501
00089                                                                   EL501
00090      EJECT                                                        EL501
00091  FD  ELCNTL.                                                      EL501
00092                            COPY ELCCNTL.                          EL501
00093                                                                   EL501
00094      EJECT                                                        EL501
00095  FD  ELREPT.                                                      EL501
00096                            COPY ELCREPT.                          EL501
00097                                                                   EL501
00098      EJECT                                                        EL501
00099  FD  ERMEBL.                                                      EL501
00100                         COPY ERCMEBL.                             EL501
00101      EJECT                                                        EL501
00102  WORKING-STORAGE SECTION.                                         EL501
00103  77  FILLER  PIC X(32) VALUE '********************************'.  EL501
00104  77  FILLER  PIC X(32) VALUE '      EL501 WORKING-STORAGE     '.  EL501
00105  77  FILLER  PIC X(32) VALUE '*********VMOD=2.010 ************'.  EL501
00106                                                                   EL501
00107  01  MONTH-END-DATA.                                              EL501
00108      12  ME-START-DATE.                                           EL501
00109          16  ME-START-MO         PIC 99.                          EL501
00110          16  FILLER              PIC X.                           EL501
00111          16  ME-START-DA         PIC 99.                          EL501
00112          16  FILLER              PIC X.                           EL501
00113          16  ME-START-YR         PIC 99.                          EL501
00114      12  ME-CNDS-DATE            PIC 9(6).                        EL501
00115      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   EL501
00116          16  ME-CNDS-MO          PIC 99.                          EL501
00117          16  ME-CNDS-DA          PIC 99.                          EL501
00118          16  ME-CNDS-YR          PIC 99.                          EL501
00119      12  ME-START-TIME           PIC 9(6).                        EL501
00120      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 EL501
00121          88  ME-DO-UPDATE        VALUE 'Y'.                       EL501
00122          88  ME-NO-UPDATE        VALUE 'N'.                       EL501
00123      12  ERMEBL-FILE-STATUS      PIC XX.                          EL501
00124      12  MONTH-END-MOYR          PIC S9(5)  COMP-3.                  CL**6
00125                                                                   EL501
00126  01  WORK-AREAS.                                                  EL501
00127      12  WS-LINE-COUNT          PIC S9(3)  VALUE +99 COMP-3.      EL501
00128      12  WS-LINE-COUNT-MAX      PIC S9(3)  VALUE +60 COMP-3.      EL501
00129      12  WS-PAGE                PIC S9(3)  VALUE +0  COMP-3.      EL501
00130      12  WS-ZERO                PIC S9     VALUE +0  COMP-3.      EL501
00131      12  WS-RETURN-CODE         PIC S9(4)            COMP.        EL501
00132                                                                   EL501
00133      12  WS-CURRENT-BIN-DT      PIC XX.                           EL501
00134      12  WS-ABEND-MESSAGE       PIC X(80).                        EL501
00135      12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              EL501
00136      12  ACCOUNT-FILE-STATUS    PIC XX  VALUE ZEROS.              EL501
00137      12  AM REDEFINES ACCOUNT-FILE-STATUS.                        EL501
00138          16  AM1               PIC X.                             EL501
00139          16  AM2               PIC X.                             EL501
00140      12  CONTROL-FILE-STATUS    PIC XX  VALUE ZEROS.              EL501
00141                                                                   EL501
00142      12  EOF-SW                 PIC X    VALUE SPACE.             EL501
00143          88  END-OF-FILE                 VALUE 'E'.               EL501
00144                                                                   EL501
00145      12  WS-CLCNTL-STATUS-SW    PIC S9   VALUE +0.                EL501
00146          88  ELCNTL-NOT-OPEN             VALUE +0.                EL501
00147          88  ELCNTL-OPEN                 VALUE +1.                EL501
00148                                                                   EL501
00149      12  ERROR-SW               PIC X    VALUE SPACE.             EL501
00150          88  ERROR-OCCURRED              VALUE 'E'.               EL501
00151          88  NO-ERRORS                   VALUE ' '.               EL501
00152                                                                   EL501
00153      12  WS-DUMMY-VARIABLE      PIC X    VALUE ZEROS.             EL501
00154          88  VAR-AMT                    VALUE '3'.                EL501
00155          88  VAR-AMT-STAT               VALUE ' '.                EL501
00156          88  VAR-AMT-CARR               VALUE '4'.                EL501
00157          88  VAR-AMT-CARR-STAT          VALUE '2'.                EL501
00158          88  VAR-ALL                     VALUE '1'.               EL501
00159                                                                   EL501
00160  01  DTE-INTERFACE-CODES.                                         EL501
00161      12  X                  PIC X           VALUE SPACE.          EL501
00162      12  PGM-SUB            PIC S9(4)  COMP VALUE +501.           EL501
00163      12  ABEND-CODE         PIC 9999        VALUE ZERO.           EL501
00164      12  ABEND-OPTION       PIC X           VALUE SPACE.          EL501
00165      12  OLC-REPORT-NAME    PIC X(6)        VALUE 'EL501'.        EL501
00166      12  WS-PROCESSOR       PIC X(4)        VALUE SPACES.         EL501
00167                                                                   EL501
00168                                                                   EL501
00169  01  WS-CONTROL-PRIMARY.                                          EL501
00170      05  WS-COMPANY-CD      PIC X.                                EL501
00171      05  FILLER             PIC X(25).                               CL**6
00172                                                                   EL501
00173  01  COMP-3-WORK-AREA.                                            EL501
00174      05  K1                 PIC S9(7)  VALUE +1.                  EL501
00175      05  K2                 PIC S9(7)  VALUE +2.                  EL501
00176      05  RECORD-COUNT       PIC S9(7)  VALUE +0.                  EL501
00177      05  DELETE-COUNT       PIC S9(7)  VALUE +0.                  EL501
00178      05  DATE-ERROR-COUNT   PIC S9(7)  VALUE +0.                  EL501
00179                                                                   EL501
00180  01  WS-SAVE-PRINT-RECORD   PIC X(133) VALUE SPACES.              EL501
00181                                                                   EL501
00182                                                                   EL501
00183      EJECT                                                        EL501
00184                             COPY ELCDATE.                            CL*11
00185                                                                   EL501
00186      EJECT                                                        EL501
00187  01  WS-HEADING1.                                                 EL501
00188                                                                   EL501
00189      05  FILLER                      PIC X(51)       VALUE '1'.   EL501
00190      05  WS-H1-TITLE                 PIC X(73)       VALUE        EL501
00191          'ACCOUNT FILE LOAD  '.                                   EL501
00192      05  WS-H1-REPORT-NUMBER         PIC X(9) VALUE 'EL501  '.    EL501
00193                                                                   EL501
00194  01  WS-HEADING2.                                                 EL501
00195      05  FILLER                      PIC X(46)       VALUE SPACES.EL501
00196      05  WS-H2-CLIENT-NAME           PIC X(78)       VALUE SPACES.EL501
00197      05  WS-H2-DATE                  PIC X(8)        VALUE SPACES.EL501
00198      05  FILLER                      PIC X           VALUE SPACES.EL501
00199                                                                   EL501
00200  01  WS-HEADING3.                                                 EL501
00201      05  FILLER                      PIC X(51)       VALUE SPACES.EL501
00202      05  WS-H3-DATE                  PIC X(60)       VALUE SPACES.EL501
00203      05  FILLER                      PIC X(5)        VALUE 'PAGE'.EL501
00204      05  WS-H3-PAGE                  PIC ZZ,ZZ9.                  EL501
00205      05  FILLER                      PIC X(11)       VALUE SPACES.EL501
00206                                                                   EL501
00207  01  WS-HEADING4                     PIC X(132)      VALUE SPACES.EL501
00208                                                                   EL501
00209  01  WS-DETAIL1.                                                  EL501
00210      05  FILLER             PIC X.                                EL501
00211      05  DTL-MSG1           PIC X(18) VALUE 'SUCCESSFUL LOAD   '. EL501
00212      05  DTL-MSG2           PIC X(18) VALUE SPACE.                EL501
00213      05  DTL-MSG3           PIC X(18) VALUE SPACE.                EL501
00214      05  D-RECORD-COUNT     PIC Z,ZZZ,ZZ9.                        EL501
00215      05  FILLER             PIC X(10) VALUE ' RECORDS ('.         EL501
00216      05  D-DELETE-COUNT     PIC Z,ZZZ,ZZ9.                        EL501
00217      05  FILLER             PIC X(11) VALUE ' DELETED)'.          EL501
00218      05  FILLER             PIC X(15) VALUE '   DATE ERRORS '.    EL501
00219      05  D-DATE-ERROR-COUNT PIC Z,ZZZ,ZZ9.                        EL501
00220                                                                   EL501
00221      EJECT                                                        EL501
00222                             COPY ELCDTECX.                        EL501
00223      EJECT                                                        EL501
00224                             COPY ELCDTEVR.                        EL501
00225                                                                   EL501
00226      EJECT                                                        EL501
00227  PROCEDURE DIVISION.                                              EL501
00228                                                                   EL501
00229  0000-CAPTURE-START.                                              EL501
00230      OPEN I-O ERMEBL.                                             EL501
00231      IF ERMEBL-FILE-STATUS = ZERO OR '97'                         EL501
00232          NEXT SENTENCE                                            EL501
00233      ELSE                                                         EL501
00234          MOVE 'N'                TO ME-UPDATE-FLAG.               EL501
00235                                                                   EL501
00236  0000-LOAD-DATE-WS. COPY ELCDTERX.                                EL501
00237                                                                   EL501
00238      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                EL501
00239      MOVE WS-TIME                TO ME-START-TIME.                EL501
00240      MOVE ME-START-MO            TO ME-CNDS-MO.                   EL501
00241      MOVE ME-START-DA            TO ME-CNDS-DA.                   EL501
00242      MOVE ME-START-YR            TO ME-CNDS-YR.                   EL501
00243                                                                   EL501
00244      EJECT                                                        EL501
00245      MOVE DTE-CLIENT             TO ME-COMPANY.                   EL501
00246      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.              CL**6
00247      MOVE MONTH-END-MOYR         TO ME-MOYR.                      EL501
00248      IF ME-DO-UPDATE                                              EL501
00249          READ ERMEBL INVALID KEY                                  EL501
00250          MOVE 'N'                TO ME-UPDATE-FLAG                EL501
00251          CLOSE ERMEBL.                                            EL501
00252                                                                   EL501
00253  0100-MAINLINE.                                                   EL501
00254      PERFORM 0200-INITIALIZE THRU 0299-EXIT.                      EL501
00255                                                                   EL501
00256      PERFORM 0300-READ-CNTL  THRU 0399-EXIT.                      EL501
00257                                                                   EL501
00258      IF NO-ERRORS                                                 EL501
00259          PERFORM 0500-DELETE-OLD THRU 0599-EXIT.                  EL501
00260                                                                   EL501
00261      IF NO-ERRORS                                                 EL501
00262          PERFORM 0700-LOAD THRU 0799-EXIT                         EL501
00263                  UNTIL END-OF-FILE                                EL501
00264                  OR    ERROR-OCCURRED.                            EL501
00265                                                                   EL501
00266      IF NO-ERRORS                                                 EL501
00267          PERFORM 0400-UPDATE-CNTL THRU 0499-EXIT.                 EL501
00268                                                                   EL501
00269      MOVE RECORD-COUNT           TO D-RECORD-COUNT.               EL501
00270      MOVE DELETE-COUNT           TO D-DELETE-COUNT.               EL501
00271      MOVE DATE-ERROR-COUNT       TO D-DATE-ERROR-COUNT.           EL501
00272      MOVE WS-DETAIL1             TO PRT.                          EL501
00273      PERFORM WRITE-A-LINE.                                        EL501
00274                                                                   EL501
00275      PERFORM 0900-FINALIZE THRU 0999-EXIT.                        EL501
00276                                                                   EL501
00277      IF ME-DO-UPDATE                                              EL501
00278          MOVE 1                  TO ME-501-FLAG                   EL501
00279          MOVE ME-START-TIME      TO ME-501-START                  EL501
00280          MOVE ME-CNDS-DATE       TO ME-501-RUN-DT                 EL501
00281          ACCEPT WS-TIME-OF-DAY   FROM TIME                        EL501
00282          MOVE WS-TIME            TO ME-501-END                    EL501
00283          ADD 1                   TO ME-501-RUN-CT                 EL501
00284          REWRITE MONTH-END-BALANCES                               EL501
00285          CLOSE ERMEBL.                                            EL501
00286                                                                   EL501
00287  0199-MAINLINE-EXIT.                                              EL501
00288      GOBACK.                                                      EL501
00289                                                                   EL501
00290  0200-INITIALIZE.                                                 EL501
00291      MOVE ZEROS                  TO WS-RETURN-CODE.               EL501
00292      MOVE COMPANY-NAME           TO WS-H2-CLIENT-NAME.            EL501
00293      MOVE WS-CURRENT-DATE        TO WS-H2-DATE.                   EL501
00294      MOVE ALPH-DATE              TO WS-H3-DATE.                   EL501
00295                                                                   EL501
00296      OPEN INPUT  AM-MSTR                                          EL501
00297           I-O    ERACCT                                              CL**9
00298           OUTPUT PRNTR.                                           EL501
00299                                                                   EL501
00300      IF ACCOUNT-FILE-STATUS = ZERO OR '97'                        EL501
00301          NEXT SENTENCE                                            EL501
00302      ELSE                                                         EL501
00303          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00304          MOVE ' ACT OPEN ERROR- '  TO WS-ABEND-MESSAGE            EL501
00305          GO TO ABEND-PGM.                                         EL501
00306                                                                   EL501
00307      MOVE LOW-VALUES TO WS-CONTROL-PRIMARY.                       EL501
00308      MOVE DTE-CLASIC-COMPANY-CD  TO WS-COMPANY-CD.                EL501
00309      MOVE DTE-CLIENT             TO WS-PROCESSOR.                 EL501
00310                                                                   EL501
00311      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          EL501
00312      MOVE '2'                    TO DC-OPTION-CODE.               EL501
00313      PERFORM 0800-DATE-RTN THRU 0899-EXIT.                        EL501
00314      MOVE DC-BIN-DATE-1          TO WS-CURRENT-BIN-DT.            EL501

061907     DISPLAY ' PGM OPTION ' DTE-PGM-OPT
           .
00316  0299-EXIT.                                                       EL501
00317      EXIT.                                                        EL501
00318      EJECT                                                        EL501
00319  0300-READ-CNTL.                                                  EL501
00320      OPEN INPUT ELCNTL.                                           EL501
00321                                                                   EL501
00322      IF CONTROL-FILE-STATUS = ZERO OR '97'                        EL501
00323          NEXT SENTENCE                                            EL501
00324      ELSE                                                         EL501
00325          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00326          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501
00327          GO TO ABEND-PGM.                                         EL501
00328                                                                   EL501
00329      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL501
00330      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL501
00331      MOVE DTE-CLIENT             TO CF-COMPANY-ID                 EL501
00332      MOVE '1'                    TO CF-RECORD-TYPE                EL501
00333      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL501
00334                                                                   EL501
00335      READ ELCNTL                                                  EL501
00336                                                                   EL501
00337      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501
00338          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00339          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL501
00340          GO TO ABEND-PGM.                                         EL501
00341                                                                   EL501
00342      IF DTE-CLIENT = 'MON'                                        EL501
00343          GO TO 0310-DO-MON.                                       EL501
00344                                                                   EL501
061907     IF DTE-PGM-OPT = 2
061907        DISPLAY ' BYPASS MAINT APPLIED EDIT ' DTE-PGM-OPT
061907        GO TO 0320-CONTINUE
061907     END-IF
061907
00345      IF EP-SW EQUAL '1'
00346         IF CF-ACCOUNT-MSTR-CREATE-DT <
00347            CF-ACCOUNT-MSTR-MAINT-DT
00348            MOVE 'E'              TO ERROR-SW
00349            MOVE '- ABORT LOAD     -' TO DTL-MSG1
00350            MOVE '- MAINT APPLIED  -' TO DTL-MSG3
00351            DISPLAY '*EL501*  LOAD ABORTED FOR - ' DTE-CLIENT
00353            DISPLAY '*EL501*  MAINT APPLIED '
00354            MOVE SPACES           TO WS-ABEND-FILE-STATUS
00355            MOVE '*EL501* MAINT APPLIED'
                                       TO WS-ABEND-MESSAGE
00356            GO TO ABEND-PGM
              END-IF
00359      ELSE
00360         MOVE 'E'                 TO ERROR-SW
00361         MOVE '- ABORT LOAD     -'
                                       TO DTL-MSG1
00362         MOVE '- UPDATE NOT ON  -'
                                       TO DTL-MSG3
00363         DISPLAY '*EL501*  LOAD ABORTED FOR - ' DTE-CLIENT
00364            UPON CONSOLE
00365         DISPLAY '*EL501*  NEED UPDATE SWITCH ON ' UPON CONSOLE
00366         MOVE SPACES              TO WS-ABEND-FILE-STATUS
00367         MOVE '*EL501* UPDATE NOT ON'
                                       TO WS-ABEND-MESSAGE
00368         GO TO ABEND-PGM
           END-IF

00370      GO TO 0320-CONTINUE.                                         EL501
00371                                                                   EL501
00372  0310-DO-MON.                                                     EL501
00373      IF EP-SW EQUAL '1'                                           EL501
00374          NEXT SENTENCE                                            EL501
00375        ELSE                                                       EL501
00376          MOVE 'E'                TO ERROR-SW                      EL501
00377          MOVE '- ABORT LOAD     -' TO DTL-MSG1                    EL501
00378          MOVE '- UPDATE NOT ON  -' TO DTL-MSG3                    EL501
00379          DISPLAY '*EL501*  LOAD ABORTED FOR - ' DTE-CLIENT        EL501
00380              UPON CONSOLE                                         EL501
00381          DISPLAY '*EL501*  NEED UPDATE SWITCH ON ' UPON CONSOLE   EL501
00382          MOVE SPACES             TO WS-ABEND-FILE-STATUS          EL501
00383          MOVE '*EL501* UPDATE NOT ON' TO WS-ABEND-MESSAGE         EL501
00384          GO TO ABEND-PGM.                                         EL501
00385                                                                   EL501
00386  0320-CONTINUE.                                                   EL501
00387      MOVE CF-CERT-ACCESS-CONTROL   TO WS-DUMMY-VARIABLE.          EL501
00388                                                                   EL501
00389      CLOSE ELCNTL.                                                EL501
00390                                                                   EL501
00391      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501
00392          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00393          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501
00394          GO TO ABEND-PGM.                                         EL501
00395                                                                   EL501
00396      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL501
00397                                                                   EL501
00398  0399-EXIT.                                                       EL501
00399      EXIT.                                                        EL501
00400                                                                   EL501
00401  0400-UPDATE-CNTL.                                                EL501
00402      OPEN I-O ELCNTL.                                             EL501
00403                                                                   EL501
00404      IF CONTROL-FILE-STATUS = ZERO OR '97'                        EL501
00405          NEXT SENTENCE                                            EL501
00406      ELSE                                                         EL501
00407          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00408          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501
00409          GO TO ABEND-PGM.                                         EL501
00410                                                                   EL501
00411      MOVE +1                     TO WS-CLCNTL-STATUS-SW.          EL501
00412      MOVE SPACES                 TO CF-ACCESS-CD-GENL.            EL501
00413      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                EL501
00414      MOVE '1'                    TO CF-RECORD-TYPE.               EL501
00415      MOVE ZEROS                  TO CF-SEQUENCE-NO.               EL501
00416                                                                   EL501
00417      READ ELCNTL                                                  EL501
00418                                                                   EL501
00419      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501
00420          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00421          MOVE ' CNTL READ ERROR- ' TO WS-ABEND-MESSAGE            EL501
00422          GO TO ABEND-PGM.                                         EL501
00423                                                                   EL501
00424      MOVE WS-CURRENT-BIN-DT      TO CF-ACCOUNT-MSTR-MAINT-DT      EL501
00425                                     CF-ACCOUNT-MSTR-CREATE-DT.    EL501
00426                                                                   EL501
00427      REWRITE CONTROL-FILE.                                        EL501
00428                                                                   EL501
00429      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501
00430          MOVE CONTROL-FILE-STATUS     TO WS-ABEND-FILE-STATUS     EL501
00431          MOVE ' CNTL REWRITE ERROR- ' TO WS-ABEND-MESSAGE         EL501
00432          GO TO ABEND-PGM.                                         EL501
00433                                                                   EL501
00434      CLOSE ELCNTL.                                                EL501
00435                                                                   EL501
00436      IF CONTROL-FILE-STATUS NOT = ZERO                            EL501
00437          MOVE CONTROL-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00438          MOVE ' CNTL OPEN ERROR- ' TO WS-ABEND-MESSAGE            EL501
00439          GO TO ABEND-PGM.                                         EL501
00440                                                                   EL501
00441      MOVE +0                     TO WS-CLCNTL-STATUS-SW.          EL501
00442                                                                   EL501
00443  0499-EXIT.                                                       EL501
00444      EXIT.                                                        EL501
00445                                                                   EL501
00446  0500-DELETE-OLD.                                                 EL501
00447      MOVE WS-CONTROL-PRIMARY     TO AM-CONTROL-PRIMARY.           EL501
00448                                                                   EL501
00449  0510-START.                                                      EL501
00450      START ERACCT KEY NOT LESS AM-CONTROL-PRIMARY.                   CL**9
00451                                                                   EL501
00452      IF ACCOUNT-FILE-STATUS  = '23' OR '10'                       EL501
00453           GO TO 0599-EXIT.                                        EL501
00454                                                                   EL501
00455      IF ACCOUNT-FILE-STATUS NOT = ZERO                            EL501
00456          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00457          MOVE ' ACT START ERROR- ' TO WS-ABEND-MESSAGE            EL501
00458          GO TO ABEND-PGM.                                         EL501
00459                                                                   EL501
00460  0520-READNEXT.                                                   EL501
00461      READ ERACCT NEXT RECORD.                                        CL**9
00462                                                                   EL501
00463      IF AM1 = '1'                                                 EL501
00464          GO TO 0599-EXIT.                                         EL501
00465                                                                   EL501
00466      IF ACCOUNT-FILE-STATUS NOT = ZEROS                           EL501
00467          MOVE ' -ERROR ON READ- ' TO WS-ABEND-MESSAGE             EL501
00468          MOVE ACCOUNT-FILE-STATUS TO WS-ABEND-FILE-STATUS         EL501
00469          GO TO ABEND-PGM.                                         EL501
00470                                                                   EL501
00471      IF DTE-CLASIC-COMPANY-CD NOT = AM-COMPANY-CD                 EL501
00472          GO TO 0599-EXIT.                                         EL501
00473                                                                   EL501
00474      DELETE  ERACCT RECORD.                                          CL**9
00475                                                                   EL501
00476      PERFORM 0600-ERROR-CHECK THRU 0699-EXIT.                     EL501
00477                                                                   EL501
00478      IF ERROR-OCCURRED                                            EL501
00479          MOVE ' -ERR ON DELETE- ' TO DTL-MSG3                     EL501
00480          GO TO 0599-EXIT.                                         EL501
00481                                                                   EL501
00482      ADD 1  TO DELETE-COUNT.                                      EL501
00483                                                                   EL501
00484      GO TO 0520-READNEXT.                                         EL501
00485                                                                   EL501
00486  0599-EXIT.                                                       EL501
00487      EXIT.                                                        EL501
00488                                                                   EL501
00489  0600-ERROR-CHECK.                                                EL501
00490      IF AM1 NOT = '0'                                             EL501
00491          MOVE   AM               TO DTL-MSG2                      EL501
00492          MOVE   'E'              TO ERROR-SW.                     EL501
00493                                                                   EL501
00494  0699-EXIT.                                                       EL501
00495      EXIT.                                                        EL501
00496                                                                   EL501
00497      EJECT                                                        EL501
00498  0700-LOAD.                                                       EL501
00499      READ AM-MSTR                                                 EL501
00500             AT END MOVE 'E' TO EOF-SW                             EL501
00501             GO TO 0799-EXIT.                                      EL501
00502 ***                                                               EL501
00503      MOVE AM-MSTR-REC          TO ACCOUNT-MASTER.                 EL501
00504                                                                   EL501
00505 *    INSPECT AM-STATUS CONVERTING 'TIA' TO '210'.                 EL501
031811*    INSPECT AM-STATUS CONVERTING 'SFCTIA' TO '543210'
021916     INSPECT AM-STATUS CONVERTING 'PRLDSFCTIA' TO '9876543210'

00506                                                                   EL501
00507      IF AM-EXPIRE-DT = 99999999999                                   CL*12
00508         MOVE LOW-VALUES          TO AM-CNTRL-B                       CL**7
00509         MOVE HIGH-VALUES         TO AM-EXPIRATION-DT              EL501
00510      ELSE                                                         EL501
00511         MOVE AM-EXPIRE-DT        TO DC-GREG-DATE-CYMD                CL**2
00512         MOVE 'L'                 TO DC-OPTION-CODE                   CL**2
00513         PERFORM 0800-DATE-RTN THRU 0899-EXIT                      EL501
00514         IF NO-CONVERSION-ERROR                                    EL501
00515            MOVE LOW-VALUES       TO AM-CNTRL-B                       CL**7
00516            MOVE DC-BIN-DATE-1    TO AM-EXPIRATION-DT                 CL**6
00517         ELSE                                                      EL501
00518            DISPLAY '*EL501*  INVALID EXPIRE DATE - ' AM           EL501
00519            DISPLAY '*EL501* ACCOUNT - ' AM-CARRIER ' ' AM-GROUPINGEL501
00520                ' ' AM-STATE ' ' AM-ACCOUNT.                       EL501
00521                                                                   EL501
00522      IF AM-EFFECT-DT = 99999999999                                   CL*12
00523         MOVE HIGH-VALUES         TO AM-EFFECTIVE-DT                  CL**6
00524      ELSE                                                         EL501
00525         MOVE AM-EFFECT-DT        TO DC-GREG-DATE-CYMD                CL**2
00526         MOVE 'L'                 TO DC-OPTION-CODE                   CL**2
00527         PERFORM 0800-DATE-RTN THRU 0899-EXIT                      EL501
00528         IF NO-CONVERSION-ERROR                                    EL501
00529            MOVE DC-BIN-DATE-1    TO AM-EFFECTIVE-DT               EL501
00530         ELSE                                                      EL501
00531            DISPLAY '*EL501*  INVALID EFFECT DATE - ' AM-EFFECT-DT EL501
00532            DISPLAY '*EL501* ACCOUNT - ' AM-CARRIER ' ' AM-GROUPINGEL501
00533                ' ' AM-STATE ' ' AM-ACCOUNT.                       EL501
00534                                                                   EL501
00535      MOVE AM-CONTROL-PRIMARY    TO AM-CONTROL-BY-VAR-GRP          EL501
00536                                                                   EL501
00537      IF WS-DUMMY-VARIABLE EQUAL ' '                               EL501
00538         MOVE SPACES              TO AM-VG-CARRIER                 EL501
00539                                     AM-VG-GROUPING                EL501
00540      ELSE                                                         EL501
00541      IF WS-DUMMY-VARIABLE EQUAL '2'                               EL501
00542         MOVE SPACES              TO AM-VG-GROUPING                EL501
00543      ELSE                                                         EL501
00544      IF WS-DUMMY-VARIABLE EQUAL '3'                               EL501
00545         MOVE SPACES              TO AM-VG-CARRIER                 EL501
00546                                     AM-VG-GROUPING                EL501
00547                                     AM-VG-STATE                   EL501
00548      ELSE                                                         EL501
00549      IF WS-DUMMY-VARIABLE EQUAL '4'                               EL501
00550         MOVE SPACES              TO AM-VG-STATE                   EL501
00551                                     AM-VG-GROUPING.               EL501
00552                                                                   EL501
00553      WRITE ACCOUNT-MASTER.                                        EL501
00554                                                                   EL501
00555      IF AM1 NOT = '0'                                             EL501
00556          MOVE ACCOUNT-FILE-STATUS TO DTL-MSG2                     EL501
00557          MOVE ' -ERR ON WRITE- ' TO DTL-MSG3                      EL501
00558          DISPLAY '*EL501* WRITE ERROR  ACCOUNT - ' AM-CARRIER ' ' EL501
00559              AM-GROUPING ' ' AM-STATE ' ' AM-ACCOUNT ' '          EL501
00560              AM-EXPIRATION-DT                                     EL501
00561          DISPLAY '*EL501* VARIABLE GROUP KEY   - '                EL501
00562              AM-CONTROL-BY-VAR-GRP                                EL501
00563          MOVE 'E'                TO ERROR-SW                      EL501
00564          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00565          MOVE ' ACCOUNT WRITE ERROR ' TO WS-ABEND-MESSAGE         EL501
00566          GO TO ABEND-PGM.                                         EL501
00567                                                                   EL501
00568      ADD  1 TO RECORD-COUNT.                                      EL501
00569                                                                   EL501
00570                                                                   EL501
00571  0799-EXIT.                                                       EL501
00572      EXIT.                                                        EL501
00573                                                                   EL501
00574  0800-DATE-RTN.                                                   EL501
00575      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  EL501
00576                                                                   EL501
00577      IF DC-ERROR-CODE NOT = SPACE                                 EL501
00578          MOVE  ZEROS             TO DC-BIN-DATE-1                 EL501
00579          ADD 1  TO DATE-ERROR-COUNT.                              EL501
00580                                                                   EL501
00581  0899-EXIT.                                                       EL501
00582      EXIT.                                                        EL501
00583                                                                   EL501
00584      EJECT                                                        EL501
00585                                                                   EL501
00586  0900-FINALIZE.                                                   EL501
00587      CLOSE AM-MSTR                                                EL501
00588            PRNTR                                                  EL501
00589            ERACCT.                                                   CL**9
00590                                                                   EL501
00591      IF ACCOUNT-FILE-STATUS NOT = ZEROS                           EL501
00592          MOVE ' -ERROR ON CLOSE- ' TO WS-ABEND-MESSAGE            EL501
00593          MOVE ACCOUNT-FILE-STATUS  TO WS-ABEND-FILE-STATUS        EL501
00594          GO TO ABEND-PGM.                                         EL501
00595                                                                   EL501
00596      IF ELCNTL-OPEN                                               EL501
00597          CLOSE ELCNTL                                             EL501
00598          IF CONTROL-FILE-STATUS NOT = ZEROS                       EL501
00599              MOVE ' -ERROR ON CLOSE- ' TO WS-ABEND-MESSAGE        EL501
00600              MOVE CONTROL-FILE-STATUS TO WS-ABEND-FILE-STATUS     EL501
00601              GO TO ABEND-PGM.                                     EL501
00602                                                                   EL501
00603  0910-CLOSE-OTHER. COPY ELCPRTCX.                                 EL501
00604                                                                   EL501
00605                                                                   EL501
00606  0999-EXIT.                                                       EL501
00607      EXIT.                                                        EL501
00608      EJECT                                                        EL501
00609  WRITE-A-LINE SECTION. COPY ELCWAL.                               EL501
00610      EJECT                                                        EL501
00611  WRITE-HEADINGS SECTION.                                          EL501
00612 ***************************************************************** EL501
00613 *                                                               * EL501
00614 *                            ELCWHS1.                           * EL501
00615 *                            VMOD=2.001                         * EL501
00616 *                                                               * EL501
00617 *    THIS SECTION CONTROLS THE WRITING OF THE HEADINGS          * EL501
00618 *****************************************************************.EL501
00619  WHS-010.                                                         EL501
00620      ADD +1  TO  WS-PAGE.                                         EL501
00621      MOVE WS-PAGE                TO  WS-H3-PAGE.                  EL501
00622      MOVE PRT                    TO  WS-SAVE-PRINT-RECORD.        EL501
00623      MOVE ZERO                   TO  WS-LINE-COUNT.               EL501
00624                                                                   EL501
00625      MOVE WS-HEADING1            TO  PRT.                         EL501
00626      MOVE '1'                    TO  X.                           EL501
00627      PERFORM WRITE-PRINTER.                                       EL501
00628                                                                   EL501
00629      MOVE WS-HEADING2            TO  PRT.                         EL501
00630      MOVE ' '                    TO  X.                           EL501
00631      PERFORM WRITE-PRINTER.                                       EL501
00632                                                                   EL501
00633      MOVE WS-HEADING3            TO  PRT.                         EL501
00634      MOVE ' '                    TO  X.                           EL501
00635      PERFORM WRITE-PRINTER.                                       EL501
00636                                                                   EL501
00637      MOVE WS-HEADING4            TO  PRT.                         EL501
00638      MOVE ' '                    TO  X.                           EL501
00639      PERFORM WRITE-PRINTER.                                       EL501
00640                                                                   EL501
00641      MOVE +4 TO WS-LINE-COUNT.                                    EL501
00642                                                                   EL501
00643  WHS-020. COPY ELCWHS2.                                           EL501
00644      EJECT                                                        EL501
00645  WRITE-PRINTER SECTION. COPY ELCWPS.                              EL501
00646  WPS-020. COPY ELCPRT2X.                                          EL501
00647                                                                   EL501
00648  ABEND-PGM  SECTION.  COPY ELCABEND.                              EL501
00649                                                                   EL501
