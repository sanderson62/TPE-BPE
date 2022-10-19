00001  IDENTIFICATION DIVISION.                                         11/09/95
00002                                                                   EL001
00003  PROGRAM-ID.                 EL001 .                                 LV017
00004 *                            VMOD=2.017.                             CL*17
00005                                                                   EL001
00006 *AUTHOR.           LOGIC,INC.                                     EL001
00007 *                  DALLAS,TEXAS.                                  EL001
00008 *                                                                 EL001
00008 *                                                                 EL001
00009  DATE-COMPILED.                                                   EL001
00010  SECURITY.   *****************************************************EL001
00011              *                                                   *EL001
00012              *    THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.    *EL001
00013              *                                                   *EL001
00014              *  USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES  *EL001
00015              *  OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT   *EL001
00016              *  THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.      *EL001
00017              *                                                   *EL001
00018              *****************************************************EL001
00019                                                                   EL001
00020 *REMARKS. ERROR TEXT PROCESSOR.                                      CL**6
00021 *         THIS IS A LINKED TO SUBROUTINE FOR HANDLING THE ERROR      CL**6
00022 *         LOOKUP AND TEXT FORMATTING OF THE ERROR LINES.             CL**6
00023 *                                                                    CL**7
00024 *                                                                    CL**7
00025 *         ANY PROGRAM CHANGES TO THIS MODULE SHOULD ALSO BE          CL**7
00026 *         APPLIED TO LGX001 WHEN CHANGES EFFECT LOGIC PROCESSING     CL**7
00027 *         CLIENTS.                                                   CL**7
00028                                                                   EL001
00029      EJECT                                                        EL001
00030  ENVIRONMENT DIVISION.                                            EL001
00031  DATA DIVISION.                                                   EL001
00032  WORKING-STORAGE SECTION.                                         EL001
00033  77  FILLER  PIC X(32)  VALUE '********************************'. EL001
00034  77  FILLER  PIC X(32)  VALUE '*    EL001 WORKING STORAGE     *'. EL001
00035  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.017 ************'.    CL*17
00036                                                                   EL001
00037  01  WS-DATE-AREA.                                                EL001
00038      05  TRANS-EXB1              PIC X(4)    VALUE 'EXB1'.           CL**2
00039      05  TRANS-MXB1              PIC X(4)    VALUE 'MXB1'.           CL**4
00040      05  TRANS-MXA5              PIC X(4)    VALUE 'MXA5'.           CL**4
00041      05  TRANS-MXA6              PIC X(4)    VALUE 'MXA6'.           CL**4
00042      05  TRANS-MXB2              PIC X(4)    VALUE 'MXB2'.           CL**4
00043      05  TRANS-MXB3              PIC X(4)    VALUE 'MXB3'.           CL**4
061013     05  trans-ex19              pic x(4)    value 'EX19'.
00044      05  SAVE-DATE               PIC X(8)    VALUE SPACES.        EL001
00045                                                                   EL001
00046  01  WORK-AREA.                                                   EL001
00047      12  MAP-LENGTH              PIC S9(4)  COMP.                 EL001
00048      12  WCC-CNTL                PIC X      VALUE ' '.            EL001
00049      12  EM-KEY                  PIC 9(4).                        EL001
00050      12  EM-FILE-ID              PIC X(8)    VALUE 'ELERRS'.      EL001
00051      12  EM-FILE-ID-FR           PIC X(8)    VALUE 'MPFERR'.         CL*16
00052      12  FOUND-SWITCH            PIC 9       VALUE 0.             EL001
00053          88  REC-NOT-FOUND           VALUE 1.                     EL001
00054      12  TIME-IN                 PIC S9(7).                       EL001
00055      12  TIME-OUT REDEFINES TIME-IN.                              EL001
00056          16  FILLER              PIC X.                           EL001
00057          16  TIME-O              PIC 99V99.                       EL001
00058          16  FILLER              PIC XX.                          EL001
00059      12  ER-NUM                  PIC 99      VALUE 1.             EL001
00060      12  ER-SUB                  PIC 99      VALUE 0.             EL001
00061      12  WCC-CTL                 PIC X       VALUE ' '.           EL001
00062      12  WS-ERROR-LINE.                                           EL001
00063          16  WS-ERROR-NUMBER     PIC 9999.                        EL001
00064          16  WS-FILL             PIC X.                           EL001
00065          16  WS-ERROR-SEVERITY   PIC X.                           EL001
00066          16  FILLER              PIC X.                           EL001
00067          16  WS-ERROR-TEXT.                                       EL001
00068              20  WS-PREFIX       PIC X(06).                       EL001
00069              20  FILLER          PIC X(59).                       EL001
00070      12  TEMP-ID.                                                 EL001
00071          16  TEMP-TERM           PIC X(4).                        EL001
00072          16  FILLER              PIC X(4)    VALUE '001A'.        EL001
00073                                                                   EL001
00074              COPY EL001S.                                            CL**7
00075  01  MAP-AREA REDEFINES EL001AI.                                  EL001
00076      12  FILLER                  PIC X(31).                       EL001
00077      12  ERROR-LINES-FOR-MAP OCCURS 12 TIMES.                     EL001
00078          16  FILLER              PIC X(3).                        EL001
00079          16  ER-LINE             PIC X(72).                       EL001
00080                                                                   EL001
00081      EJECT                                                        EL001
00082              COPY ELCEMIB.                                           CL**7
00083      EJECT                                                        EL001
00084              COPY ELCDATE.                                           CL**7
00085      EJECT                                                           CL*15
00086              COPY ELCERRWS.                                          CL*15
00087      EJECT                                                           CL*16
00088              COPY ELCERRS.                                           CL*16
00089      EJECT                                                        EL001
00090  LINKAGE SECTION.                                                 EL001
00091                                                                   EL001
00092  01  DFHCOMMAREA                 PIC X(400).                      EL001
00093                                                                   EL001
00094  01  PARMLIST.                                                    EL001
00095      12  FILLER                  PIC S9(8) COMP.                  EL001
00096      12  ERROR-POINTER           PIC S9(8) COMP.                     CL*16
00097      12  ERROR-POINTER-FR        PIC S9(8) COMP.                     CL*16
00098      12  MAP-POINTER             PIC S9(8) COMP.                  EL001
00099                                                                   EL001
00100  01  ERROR-MESSAGE-FILE-REST     PIC X(72).                          CL*16
00101  01  ERROR-MESSAGE-FILE-FR       PIC X(72).                          CL*16
00102                                                                   EL001
00103  01  MAP-BUFFER                  PIC X.                           EL001
00104      EJECT                                                        EL001
00105  PROCEDURE DIVISION.                                              EL001
00106      SERVICE RELOAD PARMLIST.                                     EL001
00107                                                                   EL001
00108      MOVE DFHCOMMAREA  TO  ERROR-MESSAGE-INTERFACE-BLOCK.         EL001
00109                                                                   EL001
00110      IF EMI-ROLL-SWITCH = 'Y'                                     EL001
00111         GO TO 3000-ROLL-CODES.                                    EL001
00112                                                                   EL001
00113      MOVE EMI-ERROR              TO EM-KEY.                       EL001
00114                                                                   EL001
00115  0100-READ-ERRORS.                                                   CL*16
00116                                                                      CL*16
00117      IF  EMI-LANGUAGE-IS-FR                                          CL*16
00118          EXEC CICS HANDLE CONDITION                                  CL*16
00119               NOTFND   (2150-NOT-FOUND-FR)                           CL*16
00120               NOTOPEN  (2250-NOT-OPEN-FR)                            CL*16
00121          END-EXEC                                                    CL*16
00122                                                                   EL001
00123          EXEC CICS READ                                              CL*16
00124               DATASET  (EM-FILE-ID-FR)                               CL*16
00125               RIDFLD   (EM-KEY)                                      CL*16
00126               INTO     (ERROR-MESSAGE-FILE)                          CL*16
00127          END-EXEC                                                    CL*16
00128                                                                   EL001
00129 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR                        CL*16
00130                                                                      CL*16
00131      ELSE                                                            CL*16
00132          EXEC CICS HANDLE CONDITION                                  CL*16
00133               NOTFND   (2100-NOT-FOUND)                              CL*16
00134               NOTOPEN  (2200-NOT-OPEN)                               CL*16
00135          END-EXEC                                                    CL*16
00136                                                                      CL*16
00137          EXEC CICS READ                                              CL*16
00138               DATASET  (EM-FILE-ID)                                  CL*16
00139               RIDFLD   (EM-KEY)                                      CL*16
00140               INTO     (ERROR-MESSAGE-FILE)                          CL*16
00141          END-EXEC.                                                   CL*16
00142                                                                      CL*16
00143 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.                     CL*16
00144                                                                      CL*10
00145      MOVE EMI-CLIENT-ID TO E-CLIENT-ID.                              CL*15
00146                                                                      CL*14
00147                                  COPY ELCERRPD.                      CL*15
00148                                                                      CL*12
00149      IF EM-ERROR-TEXT-PREFIX = 'LLLLLL'                           EL001
00150         MOVE EMI-LIFE-OVERRIDE-L6  TO EM-ERROR-TEXT-PREFIX.       EL001
00151                                                                   EL001
00152      IF EM-ERROR-TEXT-PREFIX = 'AAAAAA'                           EL001
00153         MOVE EMI-AH-OVERRIDE-L6    TO EM-ERROR-TEXT-PREFIX.       EL001
00154                                                                      CL**5
00155      IF EM-ERROR-TEXT-PREFIX = 'DDDDDD'                              CL**5
00156         MOVE EMI-DATE-FIELD        TO EM-ERROR-TEXT-PREFIX.

061013     if em-error-text-prefix = 'BBBBBB'
061013        move emi-claim-type        to em-error-text-prefix
061013     end-if
061013     if ws-error-text (1:10) = 'XXXXXXXXXX'
061013        and emi-claim-no not = spaces
061013        move emi-claim-no        to ws-error-text (1:10)
061013     end-if
00157                                                                   EL001
           .
00159  2000-PROCESS-ERRORS.                                             EL001
00160 **********************************************************        EL001
00161 *       THIS ROUTINE WILL CHECK THE SEVERITY OF THE      *        EL001
00162 *       ERROR AND DETERMINE IF IT IS TO BE PROCESSED     *        EL001
00163 *       OR NOT                                           *        EL001
00164 **********************************************************        EL001
00165      IF REC-NOT-FOUND                                                CL*12
00166 *        MOVE 'N'                TO  EMI-MESSAGE-FLAG                CL*17
00167          GO TO 2000-CONT-PROCESS-ERRORS.                             CL*12
00168                                                                      CL*12
00169      MOVE EM-ERROR-SEVERITY      TO EMI-SEVERITY-SAVE.            EL001
00170      MOVE 'N'                    TO EMI-MESSAGE-FLAG.             EL001
00171                                                                   EL001
00172      IF EM-ERROR-SEVERITY = 'N'                                   EL001
00173         ADD 1 TO EMI-NOTE-CTR                                     EL001
00174         IF NOT EMI-PROCESS-ALL-ERRORS                             EL001
00175            GO TO 2300-RETURN.                                     EL001
00176                                                                   EL001
00177      IF EM-ERROR-SEVERITY = 'W'                                   EL001
00178         ADD 1 TO EMI-WARNING-CTR                                  EL001
00179         IF EMI-BYPASS-WARNINGS  OR                                EL001
00180            EMI-BYPASS-FORCABLES OR                                EL001
00181            EMI-BYPASS-FATALS                                      EL001
00182              GO TO 2300-RETURN.                                   EL001
00183                                                                   EL001
00184      IF EM-ERROR-SEVERITY = 'F'                                   EL001
00185         ADD 1 TO EMI-FORCABLE-CTR                                 EL001
00186         IF EMI-BYPASS-FORCABLES OR                                EL001
00187            EMI-BYPASS-FATALS                                      EL001
00188              GO TO 2300-RETURN.                                   EL001
00189                                                                   EL001
00190      IF EM-ERROR-SEVERITY = 'X'                                   EL001
00191         ADD 1 TO EMI-FATAL-CTR                                    EL001
00192         IF EMI-BYPASS-FATALS                                      EL001
00193            GO TO 2300-RETURN.                                     EL001
00194                                                                      CL*12
00195  2000-CONT-PROCESS-ERRORS.                                           CL*12
00196                                                                   EL001
00197      MOVE 'Y'                    TO EMI-MESSAGE-FLAG.             EL001
00198                                                                   EL001
00199      IF EMI-ERRORS-COMPLETE                                       EL001
00200         GO TO 2300-RETURN.                                        EL001
00201                                                                   EL001
00202      EJECT                                                        EL001
00203 **********************************************************        EL001
00204 *       THIS ROUTINE WILL CHECK TO SEE IF THE ERROR      *        EL001
00205 *       HAS ALREADY BEEN PROCESSED. IF IT HAS, THEN      *        EL001
00206 *       PROCESSING WILL BE TERMINATED FOR THIS ERROR.    *        EL001
00207 **********************************************************        EL001
00208      IF EMI-FORMAT-CODES-ONLY                                     EL001
00209         IF EMI-ERROR  =  EMI-ERR-NUM (1) OR                       EL001
00210                          EMI-ERR-NUM (2) OR                       EL001
00211                          EMI-ERR-NUM (3) OR                       EL001
00212                          EMI-ERR-NUM (4) OR                       EL001
00213                          EMI-ERR-NUM (5) OR                       EL001
00214                          EMI-ERR-NUM (6) OR                       EL001
00215                          EMI-ERR-NUM (7) OR                       EL001
00216                          EMI-ERR-NUM (8) OR                       EL001
00217                          EMI-ERR-NUM (9) OR                       EL001
00218                          EMI-ERR-NUM (10)                         EL001
00219            GO TO 2300-RETURN.                                     EL001
00220                                                                   EL001
00221                                                                   EL001
00222      IF EMI-NUMBER-OF-LINES = 2                                   EL001
00223         IF EMI-ERROR  =  EMI-ERROR-NUMBER (1)  OR                 EL001
00224                          EMI-ERROR-NUMBER (2)                     EL001
00225            GO TO 2300-RETURN                                      EL001
00226         ELSE                                                      EL001
00227            GO TO 2050-CHECK-LINE.                                 EL001
00228                                                                   EL001
00229      IF EMI-NUMBER-OF-LINES = 3                                   EL001
00230         IF EMI-ERROR  =  EMI-ERROR-NUMBER (1)  OR                 EL001
00231                          EMI-ERROR-NUMBER (2)  OR                 EL001
00232                          EMI-ERROR-NUMBER (3)                     EL001
00233            GO TO 2300-RETURN.                                     EL001
00234                                                                   EL001
00235      EJECT                                                        EL001
00236  2050-CHECK-LINE.                                                 EL001
00237 **********************************************************        EL001
00238 *       THIS ROUTINE WILL DETERMINE WHICH ERROR LINE     *        EL001
00239 *       THE MESSAGE WILL FORMATTED INTO AND SET THE      *        EL001
00240 *       NECESSARY SWITCHES AND SUBSCRIPTS.               *        EL001
00241 **********************************************************        EL001
00242      MOVE '2' TO EMI-SWITCH1.                                     EL001
00243                                                                   EL001
00244      IF EMI-NUMBER-OF-LINES = 1                                   EL001
00245         SET EMI-INDX TO 1                                         EL001
00246         PERFORM 2490-FORMAT-LINE                                  EL001
00247         MOVE '2'                 TO EMI-SWITCH-AREA-1             EL001
00248         MOVE '3'                 TO EMI-SWITCH1                   EL001
00249         GO TO 2300-RETURN.                                        EL001
00250                                                                   EL001
00251      IF EMI-NUMBER-OF-LINES = 2                                   EL001
00252         IF EMI-AREA1-EMPTY                                        EL001
00253            SET EMI-INDX TO 1                                      EL001
00254            MOVE '2'              TO EMI-SWITCH-AREA-1             EL001
00255            PERFORM 2490-FORMAT-LINE                               EL001
00256            GO TO 2300-RETURN                                      EL001
00257         ELSE                                                      EL001
00258            SET EMI-INDX TO 2                                      EL001
00259            IF EMI-FORMAT-CODES-ONLY                               EL001
00260               PERFORM 2500-FORMAT-CODES-ONLY THRU 2500-EXIT       EL001
00261               GO TO 2300-RETURN                                   EL001
00262            ELSE                                                   EL001
00263               MOVE '3'           TO EMI-SWITCH1                   EL001
00264               MOVE '2'           TO EMI-SWITCH-AREA-2             EL001
00265               PERFORM 2490-FORMAT-LINE                            EL001
00266               GO TO 2300-RETURN.                                  EL001
00267                                                                   EL001
00268         IF EMI-AREA1-EMPTY                                        EL001
00269            SET EMI-INDX TO 1                                      EL001
00270            MOVE '2'              TO EMI-SWITCH-AREA-1             EL001
00271            PERFORM 2490-FORMAT-LINE                               EL001
00272            GO TO 2300-RETURN.                                     EL001
00273                                                                   EL001
00274         IF EMI-AREA2-EMPTY                                        EL001
00275            SET EMI-INDX TO 2                                      EL001
00276            MOVE '2'              TO EMI-SWITCH-AREA-2             EL001
00277            PERFORM 2490-FORMAT-LINE                               EL001
00278            GO TO 2300-RETURN.                                     EL001
00279                                                                   EL001
00280         SET EMI-INDX TO 3.                                        EL001
00281                                                                   EL001
00282         IF EMI-FORMAT-CODES-ONLY                                  EL001
00283            PERFORM 2500-FORMAT-CODES-ONLY THRU 2500-EXIT          EL001
00284            GO TO 2300-RETURN                                      EL001
00285         ELSE                                                      EL001
00286            MOVE '3'              TO EMI-SWITCH1                   EL001
00287            PERFORM 2490-FORMAT-LINE                               EL001
00288            GO TO 2300-RETURN.                                     EL001
00289                                                                   EL001
00290  2100-NOT-FOUND.                                                  EL001
00291      MOVE 1                      TO FOUND-SWITCH.                 EL001
00292      GO TO 2000-PROCESS-ERRORS.                                   EL001
00293                                                                   EL001
00294  2150-NOT-FOUND-FR.                                                  CL*16
00295      MOVE 'E'                    TO EMI-LANGUAGE-IND.                CL*16
00296      GO TO 0100-READ-ERRORS.                                         CL*16
00297                                                                      CL*16
00298  2200-NOT-OPEN.                                                   EL001
00299      SET EMI-INDX TO 1.                                           EL001
00300      MOVE 'ELERRS FILE NOT OPEN' TO EMI-ERROR-TEXT (EMI-INDX).    EL001
00301      MOVE '3'                    TO EMI-SWITCH1.                     CL*16
00302                                                                      CL*16
00303  2250-NOT-OPEN-FR.                                                   CL*16
00304      SET EMI-INDX TO 1.                                              CL*16
00305      MOVE 'MPERRS FILE NOT OPEN' TO EMI-ERROR-TEXT (EMI-INDX).       CL*16
00306      MOVE '3'                    TO EMI-SWITCH1.                  EL001
00307                                                                   EL001
00308  2300-RETURN.                                                     EL001
00309      MOVE ERROR-MESSAGE-INTERFACE-BLOCK  TO  DFHCOMMAREA.         EL001
00310                                                                   EL001
00311      EXEC CICS RETURN                                             EL001
00312      END-EXEC.                                                    EL001
00313                                                                   EL001
00314      GOBACK.                                                      EL001
00315      EJECT                                                        EL001
00316  2490-FORMAT-LINE.                                                EL001
00317      IF REC-NOT-FOUND                                             EL001
00318         MOVE 'NO TEXT RECORD FOUND FOR THIS ERROR' TO             EL001
00319               EMI-ERROR-TEXT (EMI-INDX)                           EL001
00320         MOVE 'X'                 TO EMI-SEVERITY (EMI-INDX)          CL*17
00321      ELSE                                                         EL001
00322         MOVE EM-ERROR-SEVERITY   TO EMI-SEVERITY (EMI-INDX)       EL001
00323         MOVE '-'                 TO EMI-FILL (EMI-INDX)           EL001
00324         MOVE EM-ERROR-TEXT       TO EMI-ERROR-TEXT (EMI-INDX).    EL001
00325                                                                   EL001
00326      MOVE EMI-ERROR              TO EMI-ERROR-NUMBER (EMI-INDX).  EL001
00327                                                                   EL001
00328  2500-FORMAT-CODES-ONLY.                                          EL001
00329      IF EMI-SUB = 2                                               EL001
00330         MOVE SPACES              TO EMI-ERROR-TEXT (3).           EL001
00331                                                                   EL001
00332      MOVE EMI-ERROR              TO EMI-ERR-NUM (EMI-SUB).        EL001
00333      MOVE '-'                    TO EMI-FILLER (EMI-SUB).         EL001
00334                                                                   EL001
00335      IF REC-NOT-FOUND                                             EL001
00336         MOVE 'X'                 TO EMI-SEV (EMI-SUB)                CL*17
00337      ELSE                                                         EL001
00338         MOVE EM-ERROR-SEVERITY   TO EMI-SEV (EMI-SUB).            EL001
00339                                                                   EL001
00340      IF EMI-SUB = 1                                               EL001
00341         ADD 1 TO EMI-SUB                                          EL001
00342         PERFORM 2490-FORMAT-LINE                                  EL001
00343         GO TO 2500-EXIT.                                          EL001
00344                                                                   EL001
00345      IF EMI-INDX = 2                                              EL001
00346         MOVE EMI-MESSAGE-AREA (3)                                 EL001
00347                                  TO EMI-MESSAGE-AREA (2).         EL001
00348                                                                   EL001
00349      IF EMI-SUB = 10                                              EL001
00350         MOVE '3'                 TO EMI-SWITCH1                   EL001
00351      ELSE                                                         EL001
00352         ADD 1 TO EMI-SUB.                                         EL001
00353                                                                   EL001
00354  2500-EXIT.                                                       EL001
00355       EXIT.                                                       EL001
00356                                                                   EL001
00357      EJECT                                                        EL001
00358  3000-ROLL-CODES.                                                 EL001
00359      MOVE LOW-VALUES             TO EL001AI.                      EL001
00360 *                                                                    CL*16
00361 *    EXEC CICS HANDLE CONDITION                                      CL*16
00362 *         NOTFND    (4080-NOT-FOUND)                                 CL*16
00363 *         NOTOPEN   (2200-NOT-OPEN)                                  CL*16
00364 *    END-EXEC.                                                       CL*16
00365                                                                   EL001
00366      IF EMI-NUMBER-OF-LINES = 1                                   EL001
00367         IF EMI-FORMAT-CODES-ONLY                                  EL001
00368            MOVE EMI-LINE1        TO EMI-LINE3                     EL001
00369            MOVE 1                TO ER-SUB                        EL001
00370            PERFORM 4000-READ-ERRORS THRU 4099-EXIT                EL001
00371                    UNTIL ER-NUM GREATER THAN EMI-SUB              EL001
00372            GO TO 3500-SEND-ERROR-SCREEN.                          EL001
00373                                                                   EL001
00374      IF EMI-NUMBER-OF-LINES = 2                                   EL001
00375         IF EMI-FORMAT-CODES-ONLY                                  EL001
00376            MOVE EMI-LINE1        TO ER-LINE (1)                   EL001
00377            MOVE EMI-LINE2        TO EMI-LINE3                     EL001
00378            MOVE 2                TO ER-SUB                        EL001
00379            PERFORM 4000-READ-ERRORS THRU 4099-EXIT                EL001
00380                    UNTIL ER-NUM GREATER THAN EMI-SUB              EL001
00381            GO TO 3500-SEND-ERROR-SCREEN.                          EL001
00382                                                                   EL001
00383      IF EMI-NUMBER-OF-LINES = 3                                   EL001
00384         IF EMI-FORMAT-CODES-ONLY                                  EL001
00385            MOVE EMI-LINE1        TO ER-LINE (1)                   EL001
00386            MOVE EMI-LINE2        TO ER-LINE (2)                   EL001
00387            MOVE 3                TO ER-SUB                        EL001
00388            PERFORM 4000-READ-ERRORS THRU 4099-EXIT                EL001
00389                    UNTIL ER-NUM GREATER THAN EMI-SUB              EL001
00390            GO TO 3500-SEND-ERROR-SCREEN.                          EL001
00391                                                                   EL001
00392      GO TO 2300-RETURN.                                           EL001
00393      EJECT                                                        EL001
00394  3500-SEND-ERROR-SCREEN.                                          EL001
00395                                                                      CL**2
00396      IF EIBTRNID = TRANS-EXB1 OR                                     CL**4
00397                    TRANS-MXB1 OR                                     CL**4
00398                    TRANS-MXA6 OR                                     CL**4
00399                    TRANS-MXB2 OR                                     CL**4
00400                    TRANS-MXB3 OR                                     CL**4
00401                    TRANS-MXA5                                        CL**4
061013                or trans-ex19
00402         GO TO 3515-SEND-ERRORS.                                      CL**2
00403                                                                      CL**2
00404      EXEC CICS RECEIVE                                            EL001
00405           SET    (MAP-POINTER)                                    EL001
00406           LENGTH (MAP-LENGTH)                                     EL001
00407           BUFFER                                                  EL001
00408      END-EXEC.                                                    EL001
00409                                                                   EL001
00410      SERVICE RELOAD MAP-BUFFER.                                   EL001
00411                                                                   EL001
00412      MOVE EIBTRMID               TO TEMP-TERM.                    EL001
00413                                                                   EL001
00414      EXEC CICS HANDLE CONDITION                                   EL001
00415           QIDERR  (3510-WRITE-TS)                                 EL001
00416      END-EXEC.                                                    EL001
00417                                                                   EL001
00418      EXEC CICS DELETEQ TS                                         EL001
00419           QUEUE  (TEMP-ID)                                        EL001
00420      END-EXEC.                                                    EL001
00421                                                                   EL001
00422  3510-WRITE-TS.                                                   EL001
00423                                                                      CL**2
00424      EXEC CICS WRITEQ TS                                          EL001
00425           QUEUE  (TEMP-ID)                                        EL001
00426           LENGTH (MAP-LENGTH)                                     EL001
00427           FROM   (MAP-BUFFER)                                     EL001
00428      END-EXEC.                                                    EL001
00429                                                                      CL**2
00430  3515-SEND-ERRORS.                                                   CL**2
00431                                                                   EL001
00432      MOVE EIBDATE                TO DC-JULIAN-YYDDD.                 CL**3
00433      MOVE '5'                    TO DC-OPTION-CODE.                  CL**3
00434      PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.                  CL**3
00435      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                      CL**3
00436                                                                      CL**3
00437      MOVE SAVE-DATE              TO RUNDTEI.                      EL001
00438      MOVE EIBTIME                TO TIME-IN.                      EL001
00439      MOVE TIME-O                 TO RUNTIMO.                      EL001
00440                                                                   EL001
00441      EXEC CICS SEND                                               EL001
00442           MAPSET  ('EL001S')                                      EL001
00443           MAP     ('EL001A')                                      EL001
00444           FROM    (EL001AI)                                       EL001
00445           ERASE   WAIT                                            EL001
00446      END-EXEC.                                                    EL001
00447                                                                   EL001
00448      EXEC CICS HANDLE AID                                         EL001
00449           ANYKEY(3520-SEND-MAP)                                   EL001
00450      END-EXEC.                                                    EL001
00451                                                                   EL001
00452      EXEC CICS RECEIVE                                            EL001
00453           MAPSET  ('EL001S')                                      EL001
00454           MAP     ('EL001A')                                      EL001
00455           INTO    (EL001AI)                                       EL001
00456      END-EXEC.                                                    EL001
00457                                                                   EL001
00458  3520-SEND-MAP.                                                   EL001
00459                                                                      CL**2
00460      IF EIBTRNID = TRANS-EXB1 OR                                     CL**4
00461                    TRANS-MXB1 OR                                     CL**4
00462                    TRANS-MXA6 OR                                     CL**4
00463                    TRANS-MXB2 OR                                     CL**4
00464                    TRANS-MXB3 OR                                     CL**4
00465                    TRANS-MXA5                                        CL**4
061013                or trans-ex19
00466         GO TO 2300-RETURN.                                           CL**2
00467                                                                      CL**2
00468      EXEC CICS READQ TS                                           EL001
00469           QUEUE  (TEMP-ID)                                        EL001
00470           LENGTH (MAP-LENGTH)                                     EL001
00471           SET    (MAP-POINTER)                                    EL001
00472      END-EXEC                                                     EL001
00473                                                                   EL001
00474      SERVICE RELOAD MAP-BUFFER.                                   EL001
00475                                                                   EL001
00476      EXEC CICS SEND                                               EL001
00477           FROM    (MAP-BUFFER)                                    EL001
00478           LENGTH  (MAP-LENGTH)                                    EL001
00479           CTLCHAR (WCC-CTL)                                       EL001
00480      END-EXEC.                                                    EL001
00481                                                                   EL001
00482      GO TO 2300-RETURN.                                           EL001
00483                                                                   EL001
00484  4000-READ-ERRORS.                                                EL001
00485      IF EMI-ERR-NUM (ER-NUM) NOT NUMERIC                             CL*17
00486         MOVE 99                  TO ER-NUM                        EL001
00487         GO TO 4099-EXIT.                                          EL001
00488                                                                   EL001
00489      MOVE EMI-ERR-NUM (ER-NUM)   TO EM-KEY.                       EL001
00490      MOVE SPACES                 TO WS-ERROR-LINE.                EL001
00491                                                                   EL001
00492      IF  EMI-LANGUAGE-IS-FR                                          CL*16
00493          EXEC CICS HANDLE CONDITION                                  CL*16
00494               NOTFND   (4090-NOT-FOUND-FR)                           CL*16
00495               NOTOPEN  (2250-NOT-OPEN-FR)                            CL*16
00496          END-EXEC                                                    CL*16
00497                                                                      CL*12
00498          EXEC CICS READ                                              CL*16
00499               DATASET (EM-FILE-ID)                                   CL*16
00500               RIDFLD  (EM-KEY)                                       CL*16
00501               INTO    (ERROR-MESSAGE-FILE)                           CL*16
00502          END-EXEC                                                    CL*16
00503                                                                   EL001
00504 *        SERVICE RELOAD ERROR-MESSAGE-FILE-FR                        CL*16
00505                                                                      CL*16
00506      ELSE                                                            CL*16
00507          EXEC CICS HANDLE CONDITION                                  CL*16
00508               NOTFND   (4080-NOT-FOUND)                              CL*16
00509               NOTOPEN  (2200-NOT-OPEN)                               CL*16
00510          END-EXEC                                                    CL*16
00511                                                                      CL*16
00512          EXEC CICS READ                                              CL*16
00513               DATASET (EM-FILE-ID)                                   CL*16
00514               RIDFLD  (EM-KEY)                                       CL*16
00515               INTO    (ERROR-MESSAGE-FILE)                           CL*16
00516          END-EXEC.                                                   CL*16
00517                                                                      CL*16
00518 *        SERVICE RELOAD ERROR-MESSAGE-FILE-REST.                     CL*16
00519                                                                      CL**5
00520      MOVE EMI-CLIENT-ID TO E-CLIENT-ID.                              CL*15
00521                                                                      CL**8
00522                                  COPY ELCERRPD.                      CL*15
00523                                                                      CL*12
00524      MOVE '-'                    TO WS-FILL.                      EL001
00525      MOVE EM-ERROR-SEVERITY      TO WS-ERROR-SEVERITY.            EL001
00526      MOVE EM-ERROR-TEXT          TO WS-ERROR-TEXT.                EL001
00527                                                                   EL001
00528  4050-MOVE-NUMBER.                                                EL001
00529      MOVE EM-MESSAGE-NUMBER      TO WS-ERROR-NUMBER.              EL001
00530                                                                   EL001
00531      IF WS-PREFIX = 'LLLLLL'                                      EL001
00532         MOVE EMI-LIFE-OVERRIDE-L6  TO WS-PREFIX.                  EL001
00533                                                                   EL001
00534      IF WS-PREFIX = 'AAAAAA'                                      EL001
00535         MOVE EMI-AH-OVERRIDE-L6    TO WS-PREFIX.                  EL001
00536                                                                   EL001
061013     if ws-error-text (1:10) = 'XXXXXXXXXX'
061013        and emi-claim-no not = spaces
061013        move emi-claim-no        to ws-error-text (1:10)
061013     end-if

00537      MOVE WS-ERROR-LINE          TO ER-LINE (ER-SUB).             EL001
00538                                                                   EL001
00539      ADD 1   TO ER-SUB                                            EL001
00540                 ER-NUM.                                           EL001
00541                                                                   EL001
00542      GO TO 4099-EXIT.                                             EL001
00543                                                                   EL001
00544  4080-NOT-FOUND.                                                  EL001
00545      MOVE SPACES                 TO ER-LINE (ER-SUB).             EL001
00546      MOVE 'NO TEXT RECORD FOUND FOR THIS ERROR'  TO WS-ERROR-TEXT.EL001
00547      MOVE WS-ERROR-LINE          TO ER-LINE (ER-SUB).                CL*12
00548      ADD 1                       TO ER-SUB                           CL*12
00549                                     ER-NUM.                          CL*12
00550      GO TO 4099-EXIT.                                                CL*16
00551                                                                      CL*16
00552  4090-NOT-FOUND-FR.                                                  CL*16
00553                                                                      CL*16
00554      MOVE 'E'                    TO EMI-LANGUAGE-IND.                CL*16
00555      GO TO 4000-READ-ERRORS.                                         CL*16
00556                                                                   EL001
00557  4099-EXIT.                                                       EL001
00558       EXIT.                                                       EL001
00559                                                                   EL001
00560  9700-LINK-DATE-CONVERT.                                          EL001
00561      EXEC CICS LINK                                               EL001
00562          PROGRAM    ('ELDATCV')                                   EL001
00563          COMMAREA   (DATE-CONVERSION-DATA)                        EL001
00564          LENGTH     (DC-COMM-LENGTH)                              EL001
00565      END-EXEC.                                                    EL001
00566                                                                   EL001
00567  9700-EXIT.                                                       EL001
00568      EXIT.                                                        EL001
00569                                                                   EL001
