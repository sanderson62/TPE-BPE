00001  IDENTIFICATION DIVISION.                                         03/06/96
00002                                                                   EL659
00003  PROGRAM-ID.                 EL659 .                                 LV007
00004 *              PROGRAM CONVERTED BY                                  CL**7
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL**7
00006 *              CONVERSION DATE 02/14/96 12:01:56.                    CL**7
00007 *                            VMOD=2.007                              CL**7
00008 *                                                                    CL**7
00008 *AUTHOR.                                                             CL**7
00009 *               LOGIC, INC.                                          CL**7
00010 *               DALLAS, TEXAS.                                       CL**7
00011                                                                   EL659
00012 *DATE-COMPILED.                                                      CL**7
00013                                                                      CL**4
00014 *SECURITY.   *****************************************************   CL**7
00015 *            *                                                   *   CL**7
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL**7
00017 *            *                                                   *   CL**7
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL**7
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL**7
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL**7
00021 *            *                                                   *   CL**7
00022 *            *****************************************************   CL**7
00023                                                                   EL659
00024 *REMARKS.                                                         EL659
00025 *        THIS PROGRAM PROVIDES THE QUALIFICATION NECESSARY FOR    EL659
00026 *    THE ACCOUNT MASTER (ACCOUNT NAME AND/OR MAIL TO NAME),       EL659
00027 *    COMPENSATION MASTER (ACCOUNT NAME AND/OR MAIL TO NAME),      EL659
00028 *    AND REINSURANCE COMPANY NAME (WITH LEVEL NUMBERS) LOOK-UP.   EL659
00029                                                                   EL659
00030 *    SCREENS     - EL659A - ACCOUNT/COMPANY NAME REFERENCE        EL659
00031                                                                   EL659
00032 *    ENTERED BY  - EL601 - SYSTEM ADMINISTRATION MENU             EL659
00033 *                  EL650 - ACCOUNT MAINTENANCE                    EL659
00034 *                  EL652 - COMPENSATION MAINTENANCE               EL659
00035 *                  EL651 - REINSURANCE TABLE MAINTENANCE          EL659
00036                                                                   EL659
00037 *    EXIT TO     - CALLING PROGRAM                                EL659
00038 *                  EL6592 - ACCOUNT/COMPANY NAME LOOK-UP          EL659
00039                                                                   EL659
00040 *    INPUT FILE  - ERNAME - ACCOUNT/COMPANY NAME XREF FILE        EL659
00041                                                                   EL659
00042 *    OUTPUT FILE - NONE                                           EL659
00043                                                                   EL659
00044 *    COMMAREA    - PASSED.  IF AN ACCOUNT/COMPANY IS SELECTED,    EL659
00045 *                  THE CONTROL OF THAT ACCOUNT/COMPANY IS PLACED  EL659
00046 *                  IN THE APPROPRIATE FIELDS OF THE COMMAREA FOR  EL659
00047 *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM EL659
00048 *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE  EL659
00049 *                  RECORD KEY INFORMATION NEEDED BY EL6592.       EL659
00050                                                                   EL659
00051 *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL601.  ON     EL659
00052 *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE EL659
00053 *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVEEL659
00054 *                  ENTRIES (XCTL FROM CICS VIA EX65) THE SCREEN   EL659
00055 *                  WILL BE READ AND ACTION WILL BE BASED ON THE   EL659
00056 *                  OPTION INDICATED.                              EL659
00057  EJECT                                                            EL659
081905******************************************************************
081905*                   C H A N G E   L O G
081905*
081905* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
081905*-----------------------------------------------------------------
081905*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
081905* EFFECTIVE    NUMBER
081905*-----------------------------------------------------------------
081905* 081905  IR2005041400004  PEMA  ADD DELETE OF TEMP STORAGE ID'S
081905******************************************************************

00058  ENVIRONMENT DIVISION.                                            EL659
00059  DATA DIVISION.                                                   EL659
00060  WORKING-STORAGE SECTION.                                         EL659
00061                                                                   EL659
00062  77  FILLER  PIC  X(32) VALUE '********************************'. EL659
00063  77  FILLER  PIC  X(32) VALUE '*    EL659 WORKING STORAGE     *'. EL659
00064  77  FILLER  PIC  X(32) VALUE '*********** VMOD=2.007 *********'.    CL**7
00065                                                                   EL659
00066      COPY ELCSCTM.                                                   CL**4
00067                                                                      CL**4
00068      COPY ELCSCRTY.                                                  CL**4
00069                                                                   EL659
00070  01  WS-DATE-AREA.                                                EL659
00071      12  SAVE-DATE               PIC  X(08)      VALUE SPACES.    EL659
00072      12  SAVE-BIN-DATE           PIC  X(02)      VALUE SPACES.    EL659
00073                                                                   EL659
00074  01  FILLER                  COMP-3.                              EL659
00075      12  TIME-IN                     PIC S9(07)  VALUE ZERO.      EL659
00076      12  TIME-OUT  REDEFINES                                      EL659
00077          TIME-IN                     PIC S9(03)V9(4).             EL659
00078                                                                   EL659
00079  01  FILLER                  COMP SYNC.                           EL659
00080      12  SC-ITEM                 PIC S9(04)      VALUE +0001.     EL659
00081                                                                   EL659
00082  01  FILLER.                                                      EL659
081905     12  WS-RESPONSE             PIC S9(8)   COMP.
081905         88  RESP-NORMAL                  VALUE +00.
081905         88  RESP-NOTFND                  VALUE +13.
081905         88  RESP-NOTOPEN                 VALUE +19.
081905         88  RESP-ENDFILE                 VALUE +20.
00083      12  WS-MAPSET-NAME          PIC  X(08)      VALUE 'EL659S'.  EL659
00084      12  WS-MAP-NAME             PIC  X(08)      VALUE 'EL659A'.  EL659
00085      12  FILLER  REDEFINES  WS-MAP-NAME.                          EL659
00086          16  FILLER              PIC  X(02).                      EL659
00087          16  WS-MAP-NUMBER       PIC  X(04).                      EL659
00088          16  FILLER              PIC  X(02).                         CL**7
00089      12  THIS-PGM                PIC  X(08)      VALUE 'EL659'.   EL659
00090      12  WS-ERNAME-DSID          PIC  X(08)      VALUE 'ERNAME'.     CL**6
00091      12  WS-TRANS-ID             PIC  X(04)      VALUE 'EX65'.    EL659
00092      12  WS-INPUT-FIELD          PIC  X(50)      VALUE SPACES.    EL659
00093      12  WS-INPUT-CHAR  REDEFINES                                 EL659
00094          WS-INPUT-FIELD          PIC  X(01)                       EL659
00095              OCCURS  50  TIMES       INDEXED  BY  INPUT-INDEX.    EL659
00096  EJECT                                                            EL659
081905     12  WS-TEMP-STORAGE-KEY.                                     EL6592
081905         16  WS-TSK-TERM-ID          PIC  X(04)  VALUE 'XXXX'.    EL6592
081905         16  FILLER                  PIC  X(04)  VALUE '6592'.    EL6592
081905     12  WS-TEMP-STORAGE-KEY1.                                    EL6592
081905         16  WS-TSK-TERM-ID1         PIC  X(04)  VALUE 'XXXX'.    EL6592
081905         16  FILLER                  PIC  X(04)  VALUE '659P'.    EL6592
00097      12  ERROR-MESSAGES.                                          EL659
00098          16  ER-0004             PIC  X(04)      VALUE '0004'.    EL659
00099          16  ER-0008             PIC  X(04)      VALUE '0008'.    EL659
00100          16  ER-0029             PIC  X(04)      VALUE '0029'.    EL659
00101          16  ER-0070             PIC  X(04)      VALUE '0070'.    EL659
00102          16  ER-0647             PIC  X(04)      VALUE '0647'.       CL**5
00103          16  ER-0671             PIC  X(04)      VALUE '0671'.    EL659
00104          16  ER-0673             PIC  X(04)      VALUE '0673'.    EL659
00105  EJECT                                                            EL659
00106      COPY ELCINTF.                                                   CL**4
00107                                                                   EL659
00108      COPY ELC659PI.                                                  CL**4
00109          16  PI-EL659-TO-EL130-CNTRL.                                CL**4
00110              20  PI-EL659-CARRIER    PIC X.                          CL**4
00111              20  PI-EL659-GROUPING   PIC X(6).                       CL**4
00112              20  PI-EL659-STATE      PIC XX.                         CL**4
00113              20  PI-EL659-ACCOUNT    PIC X(10).                      CL**4
00114          16  FILLER                  PIC X(90).                      CL**7
00115  EJECT                                                            EL659
00116      COPY ELCEMIB.                                                   CL**4
00117  EJECT                                                            EL659
00118      COPY ELCDATE.                                                   CL**4
00119  EJECT                                                            EL659
00120      COPY ELCLOGOF.                                                  CL**4
00121  EJECT                                                            EL659
00122      COPY EL659S.                                                    CL**4
00123  EJECT                                                            EL659
00124      COPY ELCATTR.                                                   CL**4
00125  EJECT                                                            EL659
00126      COPY ELCAID.                                                    CL**4
00127                                                                   EL659
00128  01  FILLER  REDEFINES  DFHAID.                                   EL659
00129      12  FILLER                      PIC  X(08).                  EL659
00130      12  PF-VALUES                   PIC  X(01)                   EL659
00131              OCCURS  24  TIMES.                                   EL659
00132  EJECT                                                            EL659
00133  LINKAGE SECTION.                                                 EL659
00134                                                                   EL659
00135  01  DFHCOMMAREA                     PIC  X(1024).                EL659
00136                                                                   EL659
00137 *01 PARMLIST             COMP  SYNC.                                 CL**7
00138 *    12  FILLER                      PIC S9(09).                     CL**7
00139 *    12  ERNAME-POINTER              PIC S9(09).                     CL**7
00140  EJECT                                                            EL659
00141      COPY ERCNAME.                                                   CL**4
00142  EJECT                                                            EL659
00143  PROCEDURE DIVISION.                                              EL659
00144                                                                   EL659
00145      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL659
00146      MOVE '5'                    TO  DC-OPTION-CODE.              EL659
00147                                                                   EL659
00148      PERFORM 7000-DATE-CONVERSION.                                EL659
00149                                                                   EL659
00150      MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                   EL659
00151      MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.               EL659
00152      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL659
00153      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL659
00154                                      EMI-SWITCH2.                 EL659
00155                                                                   EL659
00156 *    NOTE ******************************************************* EL659
00157 *         *                                                     * EL659
00158 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL659
00159 *         *  FROM ANOTHER MODULE.                               * EL659
00160 *         *                                                     * EL659
00161 *         *******************************************************.EL659
00162                                                                   EL659
00163      IF EIBCALEN  IS NOT GREATER THAN  ZERO                       EL659
00164          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL659
00165          GO TO 6000-SEND-TEXT.                                    EL659
00166                                                                   EL659
00167      EXEC CICS HANDLE CONDITION                                   EL659
00168          PGMIDERR  (9400-PGMIDERR)                                EL659
00169          NOTFND    (1020-NOTFND)                                  EL659
00170          ENDFILE   (1020-NOTFND)                                  EL659
00171          ERROR     (9600-ERROR)                                   EL659
00172      END-EXEC.                                                    EL659
00173  EJECT                                                            EL659
00174  0100-MAIN-LOGIC.                                                 EL659
00175      IF PI-CALLING-PROGRAM  IS NOT EQUAL TO  THIS-PGM             EL659
00176          IF PI-RETURN-TO-PROGRAM  IS NOT EQUAL TO  THIS-PGM       EL659
00177              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-6    EL659
00178              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-5    EL659
00179              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-4    EL659
00180              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-3    EL659
00181              MOVE PI-SAVED-PROGRAM-1    TO  PI-SAVED-PROGRAM-2    EL659
00182              MOVE PI-RETURN-TO-PROGRAM  TO  PI-SAVED-PROGRAM-1    EL659
00183              MOVE PI-CALLING-PROGRAM    TO  PI-RETURN-TO-PROGRAM  EL659
00184              MOVE THIS-PGM              TO  PI-CALLING-PROGRAM    EL659
00185          ELSE                                                     EL659
00186              MOVE PI-RETURN-TO-PROGRAM  TO  PI-CALLING-PROGRAM    EL659
00187              MOVE PI-SAVED-PROGRAM-1    TO  PI-RETURN-TO-PROGRAM  EL659
00188              MOVE PI-SAVED-PROGRAM-2    TO  PI-SAVED-PROGRAM-1    EL659
00189              MOVE PI-SAVED-PROGRAM-3    TO  PI-SAVED-PROGRAM-2    EL659
00190              MOVE PI-SAVED-PROGRAM-4    TO  PI-SAVED-PROGRAM-3    EL659
00191              MOVE PI-SAVED-PROGRAM-5    TO  PI-SAVED-PROGRAM-4    EL659
00192              MOVE PI-SAVED-PROGRAM-6    TO  PI-SAVED-PROGRAM-5    EL659
00193              MOVE SPACES                TO  PI-SAVED-PROGRAM-6    EL659
00194              PERFORM 3000-BUILD-SCREEN                            EL659
00195      ELSE                                                         EL659
00196          GO TO 0120-MAIN-LOGIC.                                   EL659
00197                                                                   EL659
00198  0110-MAIN-LOGIC.                                                 EL659
00199 *    NOTE ******************************************************* EL659
00200 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL659
00201 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL659
00202 *         *******************************************************.EL659
00203                                                                   EL659
00204      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL659
00205                                      PI-CONTROL-IN-PROGRESS.      EL659
00206      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL659
00207                                      PI-LINE-COUNT                EL659
00208                                      PI-BROWSE-SW                 EL659
00209                                      PI-KEY-LENGTH                EL659
00210                                      PI-TS-ITEM                   EL659
00211                                      PI-END-OF-FILE               EL659
00212                                      PI-START-SW.                    CL**6
00213                                                                   EL659



081905*    NOTE *******************************************************
081905*         *      I ADDED THE DELETES TO SEE IF THIS WILL FIX    *
081905*         *  THE IR 2005041400004. IT APPEARS THAT THE ONLY     *
081905*         *  TIME IT HAPPENS IS IN THIS PROGRAM AND EL6592      *
081905*         *  IT SEEMS LIKE EL6592 CLEANS UP AFTER ITSELF OKAY   *
081905*         *******************************************************

081905         MOVE EIBTRMID           TO  WS-TSK-TERM-ID
081905                                     WS-TSK-TERM-ID1
081905         EXEC CICS DELETEQ TS
081905             QUEUE  (WS-TEMP-STORAGE-KEY)
081905             RESP   (WS-RESPONSE)
081905         END-EXEC

081905         EXEC CICS DELETEQ TS
081905             QUEUE  (WS-TEMP-STORAGE-KEY1)
081905             RESP   (WS-RESPONSE)
081905         END-EXEC




00214 *    NOTE ******************************************************* EL659
00215 *         *      SEND THE INITIAL MAP OUT TO BEGIN PROCESSING   * EL659
00216 *         *  FOR EL659.                                         * EL659
00217 *         *******************************************************.EL659
00218                                                                   EL659
00219      MOVE LOW-VALUES             TO  EL659AO.                     EL659
00220                                                                   EL659
00221      PERFORM 4000-SEND-INITIAL-MAP.                               EL659
00222                                                                   EL659
00223      GO TO 9100-RETURN-TRAN.                                      EL659
00224  EJECT                                                            EL659
00225  0120-MAIN-LOGIC.                                                 EL659
00226      IF PI-1ST-TIME-SW  IS NOT EQUAL TO  ZERO                     EL659
00227          GO TO 0110-MAIN-LOGIC.                                   EL659
00228                                                                   EL659
00229 *    NOTE ******************************************************* EL659
00230 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL659
00231 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL659
00232 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL659
00233 *         *******************************************************.EL659
00234                                                                   EL659
00235      IF EIBAID  IS EQUAL TO  DFHCLEAR                             EL659
00236          GO TO 9300-CLEAR.                                        EL659
00237                                                                   EL659
00238      IF EIBAID  IS EQUAL TO  DFHPA1  OR  DFHPA2  OR  DFHPA3       EL659
00239          MOVE LOW-VALUES         TO  EL659AO                      EL659
00240          MOVE -1                 TO  APFKL                        EL659
00241          MOVE ER-0008            TO  EMI-ERROR                    EL659
00242          PERFORM 5000-SEND-DATA-ONLY                              EL659
00243          GO TO 9100-RETURN-TRAN.                                  EL659
00244                                                                   EL659
00245      EXEC CICS RECEIVE                                            EL659
00246          INTO    (EL659AI)                                        EL659
00247          MAPSET  (WS-MAPSET-NAME)                                 EL659
00248          MAP     (WS-MAP-NAME)                                    EL659
00249      END-EXEC.                                                    EL659
00250                                                                   EL659
00251      IF APFKL  IS GREATER THAN  ZERO                              EL659
00252          IF EIBAID  IS NOT EQUAL TO  DFHENTER                     EL659
00253              MOVE ER-0004        TO  EMI-ERROR                    EL659
00254              MOVE AL-UNBOF       TO  APFKA                        EL659
00255              MOVE -1             TO  APFKL                        EL659
00256              PERFORM 5000-SEND-DATA-ONLY                          EL659
00257              GO TO 9100-RETURN-TRAN                               EL659
00258          ELSE                                                     EL659
00259              IF APFKO  IS NUMERIC                                 EL659
00260                  IF APFKO  IS GREATER THAN  0                     EL659
00261                    AND APFKO  IS LESS THAN  25                    EL659
00262                      MOVE PF-VALUES (APFKI)                       EL659
00263                                  TO  EIBAID                       EL659
00264                  ELSE                                             EL659
00265                      MOVE ER-0029                                 EL659
00266                                  TO  EMI-ERROR                    EL659
00267                      MOVE AL-UNBOF                                EL659
00268                                  TO  APFKA                        EL659
00269                      MOVE -1     TO  APFKL                        EL659
00270                      PERFORM 5000-SEND-DATA-ONLY                  EL659
00271                      GO TO 9100-RETURN-TRAN.                      EL659
00272                                                                   EL659
00273      IF EIBAID  IS EQUAL TO  DFHPF12                              EL659
00274          MOVE 'EL010'            TO  THIS-PGM                     EL659
00275          GO TO 9200-XCTL.                                         EL659
00276                                                                   EL659
00277      IF EIBAID  IS EQUAL TO  DFHPF23                              EL659
00278          GO TO 9000-RETURN-CICS.                                  EL659
00279                                                                   EL659
00280      IF EIBAID  IS EQUAL TO  DFHPF24                              EL659
00281          MOVE 'EL126'            TO  THIS-PGM                     EL659
00282          GO TO 9200-XCTL.                                         EL659
00283                                                                   EL659
00284      IF EIBAID  IS EQUAL TO  DFHENTER                             EL659
00285          NEXT SENTENCE                                            EL659
00286      ELSE                                                         EL659
00287          MOVE ER-0008            TO  EMI-ERROR                    EL659
00288          MOVE -1                 TO  APFKL                        EL659
00289          PERFORM 5000-SEND-DATA-ONLY                              EL659
00290          GO TO 9100-RETURN-TRAN.                                  EL659
00291  EJECT                                                            EL659
00292  0130-MAIN-LOGIC.                                                 EL659
00293      MOVE SPACES                 TO  PI-SELECTION-CRITERIA        EL659
00294                                      PI-NAME-LOOKUP-KEY              CL**6
00295                                      PI-CITY                         CL**6
00296                                      PI-ST.                          CL**6
00297      MOVE PI-COMPANY-CD          TO  PI-SC-COMPANY-CD             EL659
00298                                      PI-NLK-COMPANY-CD.           EL659
00299                                                                   EL659
00300      IF PI-PROCESSOR-ID  IS EQUAL TO  'LGXX'                      EL659
00301          NEXT SENTENCE                                            EL659
00302      ELSE                                                         EL659
00303          EXEC CICS READQ TS                                       EL659
00304              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL659
00305              INTO    (SECURITY-CONTROL)                           EL659
00306              LENGTH  (SC-COMM-LENGTH)                             EL659
00307              ITEM    (SC-ITEM)                                    EL659
00308          END-EXEC                                                 EL659
00309          MOVE SC-CREDIT-DISPLAY (31)                              EL659
00310                                  TO  PI-DISPLAY-CAP               EL659
00311          MOVE SC-CREDIT-UPDATE  (31)                              EL659
00312                                  TO  PI-MODIFY-CAP                EL659
00313          IF NOT DISPLAY-CAP                                       EL659
00314              MOVE 'READ'         TO  SM-READ                      EL659
00315              PERFORM 9700-SECURITY-VIOLATION                      EL659
00316              MOVE ER-0070        TO  EMI-ERROR                    EL659
00317              PERFORM 4000-SEND-INITIAL-MAP                        EL659
00318              GO TO 9100-RETURN-TRAN.                              EL659
00319  EJECT                                                            EL659
00320  0200-DETERMINE-OPTION.                                              CL**6
00321      MOVE ZERO                   TO  PI-OPTION.                      CL**6
00322      MOVE WS-ERNAME-DSID         TO  PI-DSID.                        CL**6
00323      MOVE +1                     TO  PI-KEY-LENGTH.               EL659
00324      MOVE +0                     TO  PI-CITY-LENGTH.                 CL**6
00325                                                                      CL**6
00326      IF ANAMEL GREATER ZERO                                          CL**6
00327          MOVE ANAMEI             TO  PI-SC-NAME                      CL**6
00328                                      WS-INPUT-FIELD                  CL**6
00329          PERFORM 0220-GET-KEY-LENGTH THRU 0229-EXIT                  CL**6
00330              VARYING INPUT-INDEX FROM ANAMEL BY -1                   CL**6
00331                  UNTIL WS-INPUT-CHAR (INPUT-INDEX)                   CL**6
00332                   NOT = SPACE                                        CL**6
00333          ADD ANAMEL              TO  PI-KEY-LENGTH                   CL**6
00334          MOVE -1                 TO  ANAMEL                          CL**6
00335          MOVE '1'                TO  PI-OPTION.                      CL**6
00336                                                                      CL**6
00337      IF ARTYPL GREATER ZERO                                          CL**6
00338        IF ARTYPI = 'A' OR 'C' OR 'R'                                 CL**6
00339            MOVE ARTYPI           TO  PI-SC-RECORD-TYPE               CL**6
00340            IF PI-SC-NAME NOT = SPACES                                CL**6
00341                MOVE '2'          TO  PI-OPTION                       CL**6
00342              ELSE                                                    CL**6
00343                MOVE '3'          TO  PI-OPTION                       CL**6
00344          ELSE                                                        CL**6
00345            MOVE ER-0647          TO  EMI-ERROR                       CL**6
00346            MOVE ZERO             TO  ANAMEL                          CL**6
00347            MOVE -1               TO  ARTYPL                          CL**6
00348            PERFORM 5000-SEND-DATA-ONLY                               CL**6
00349            GO TO 9100-RETURN-TRAN.                                   CL**6
00350                                                                      CL**6
00351      IF CITYL GREATER ZERO                                           CL**6
00352          MOVE CITYI              TO  PI-CITY                         CL**6
00353                                      WS-INPUT-FIELD                  CL**6
00354          PERFORM 0230-GET-CITY-LENGTH THRU 0239-EXIT                 CL**6
00355              VARYING INPUT-INDEX FROM CITYL BY -1                    CL**6
00356                  UNTIL WS-INPUT-CHAR (INPUT-INDEX)                   CL**6
00357                   NOT = SPACE                                        CL**6
00358          ADD CITYL               TO  PI-CITY-LENGTH.                 CL**6
00359                                                                   EL659
00360      IF STL GREATER ZERO                                             CL**6
00361          MOVE STI                TO  PI-ST.                          CL**6
00362                                                                   EL659
00363      MOVE PI-SELECTION-CRITERIA  TO  PI-NAME-LOOKUP-KEY.          EL659
00364                                                                   EL659
00365      GO TO 1000-READ-NAME-LOOKUP.                                    CL**6
00366                                                                   EL659
00367  0220-GET-KEY-LENGTH.                                             EL659
00368      SUBTRACT +1                 FROM  ANAMEL.                       CL**6
00369                                                                   EL659
00370  0229-EXIT.                                                       EL659
00371       EXIT.                                                          CL**6
00372                                                                      CL**6
00373  0230-GET-CITY-LENGTH.                                               CL**6
00374      SUBTRACT +1                 FROM  CITYL.                        CL**6
00375                                                                   EL659
00376  0239-EXIT.                                                          CL**6
00377       EXIT.                                                          CL**6
00378                                                                   EL659
00379  EJECT                                                            EL659
00380  1000-READ-NAME-LOOKUP  SECTION.                                  EL659
00381      EXEC CICS HANDLE CONDITION                                   EL659
00382          DUPKEY   (9200-XCTL)                                     EL659
00383          NOTFND   (1020-NOTFND)                                   EL659
00384          DSIDERR  (1010-DSIDERR)                                  EL659
00385      END-EXEC.                                                    EL659
00386                                                                   EL659
00387      MOVE +1                     TO  PI-START-SW.                 EL659
00388                                                                   EL659
00389      EXEC CICS READ                                               EL659
00390          DATASET    (PI-DSID)                                     EL659
00391          RIDFLD     (PI-NAME-LOOKUP-KEY)                          EL659
00392          SET        (ADDRESS OF NAME-LOOKUP-MASTER)                  CL**7
00393          GENERIC                                                  EL659
00394          EQUAL                                                    EL659
00395          KEYLENGTH  (PI-KEY-LENGTH)                               EL659
00396      END-EXEC.                                                    EL659
00397                                                                      CL**2
00398      IF NL-COMPANY-CD  IS EQUAL TO  PI-COMPANY-CD                    CL**2
00399          NEXT SENTENCE                                               CL**2
00400      ELSE                                                            CL**2
00401          GO TO 1020-NOTFND.                                          CL**2
00402                                                                   EL659
00403      MOVE 'EL6592'               TO  THIS-PGM.                    EL659
00404                                                                   EL659
00405      GO TO 9200-XCTL.                                             EL659
00406                                                                   EL659
00407  1010-DSIDERR.                                                    EL659
00408      MOVE ER-0671                TO  EMI-ERROR.                   EL659
00409                                                                   EL659
00410      PERFORM 5000-SEND-DATA-ONLY.                                 EL659
00411                                                                   EL659
00412      GO TO 9100-RETURN-TRAN.                                      EL659
00413                                                                   EL659
00414  1020-NOTFND.                                                     EL659
00415      MOVE ER-0673                TO  EMI-ERROR.                   EL659
00416                                                                   EL659
00417      PERFORM 5000-SEND-DATA-ONLY.                                 EL659
00418                                                                   EL659
00419      GO TO 9100-RETURN-TRAN.                                      EL659
00420                                                                   EL659
00421  EJECT                                                            EL659
00422  3000-BUILD-SCREEN  SECTION.                                      EL659
00423 ******************************************************************EL659
00424 *          REBUILD ORIGNAL SCREEN AND ERROR MESSAGE IF EL6592    *EL659
00425 *          DID NOT FIND ANY CERTIFICATES DURING BROWSE OF FILE.  *EL659
00426 ******************************************************************EL659
00427                                                                   EL659
00428      IF EIBTRNID  IS EQUAL TO  WS-TRANS-ID                        EL659
00429          NEXT SENTENCE                                            EL659
00430      ELSE                                                         EL659
00431          GO TO 3099-EXIT.                                         EL659
00432                                                                   EL659
00433      IF PI-BROWSE-SW  IS EQUAL TO  +9                             EL659
00434          NEXT SENTENCE                                            EL659
00435      ELSE                                                         EL659
00436          GO TO 3099-EXIT.                                         EL659
00437                                                                   EL659
00438      MOVE LOW-VALUES             TO  EL659AO.                     EL659
00439      MOVE ER-0673                TO  EMI-ERROR.                   EL659
00440                                                                   EL659
00441      IF PI-ST GREATER SPACES                                         CL**6
00442          MOVE PI-ST              TO  STO                             CL**6
00443          MOVE AL-UABON           TO  STA                             CL**6
00444          MOVE -1                 TO  STL.                            CL**6
00445                                                                   EL659
00446      IF PI-CITY GREATER SPACES                                       CL**6
00447          MOVE PI-CITY            TO  CITYO                           CL**6
00448          MOVE AL-UABON           TO  CITYA                           CL**6
00449          MOVE -1                 TO  CITYL.                          CL**6
00450                                                                   EL659
00451      IF PI-SC-RECORD-TYPE GREATER SPACES                             CL**6
00452          MOVE PI-SC-RECORD-TYPE  TO  ARTYPO                          CL**6
00453          MOVE AL-UABON           TO  ARTYPA                          CL**6
00454          MOVE -1                 TO  ARTYPL.                         CL**6
00455                                                                   EL659
00456      IF PI-SC-NAME GREATER SPACES                                    CL**6
00457          MOVE PI-SC-NAME         TO  ANAMEO                          CL**6
00458          MOVE AL-UABON           TO  ANAMEA                          CL**6
00459          MOVE -1                 TO  ANAMEL.                         CL**6
00460                                                                   EL659
00461  3040-INITIALIZE-WORK-AREAS.                                      EL659
00462      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA         EL659
00463                                      PI-CONTROL-IN-PROGRESS.      EL659
00464      MOVE ZERO                   TO  PI-1ST-TIME-SW               EL659
00465                                      PI-LINE-COUNT                EL659
00466                                      PI-BROWSE-SW                 EL659
00467                                      PI-KEY-LENGTH                EL659
00468                                      PI-TS-ITEM                   EL659
00469                                      PI-END-OF-FILE               EL659
00470                                      PI-START-SW.                    CL**6
00471                                                                   EL659
00472      PERFORM 5000-SEND-DATA-ONLY.                                 EL659
00473                                                                   EL659
00474      GO TO 9100-RETURN-TRAN.                                      EL659
00475                                                                   EL659
00476  3099-EXIT.                                                       EL659
00477      EXIT.                                                        EL659
00478  EJECT                                                            EL659
00479  4000-SEND-INITIAL-MAP  SECTION.                                  EL659
00480      MOVE -1                     TO  ANAMEL.                         CL**6
00481      MOVE SAVE-DATE              TO  ADATEO.                      EL659
00482      MOVE EIBTIME                TO  TIME-IN.                     EL659
00483      MOVE TIME-OUT               TO  ATIMEO.                      EL659
00484                                                                   EL659
00485      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL659
00486          PERFORM 9500-ERROR-FORMAT.                                  CL**6
00487                                                                   EL659
00488      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     EL659
00489      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                     EL659
00490                                                                   EL659
00491      EXEC CICS SEND                                               EL659
00492          FROM    (EL659AO)                                        EL659
00493          MAPSET  (WS-MAPSET-NAME)                                 EL659
00494          MAP     (WS-MAP-NAME)                                    EL659
00495          CURSOR                                                   EL659
00496          ERASE                                                    EL659
00497      END-EXEC.                                                    EL659
00498                                                                   EL659
00499  4099-EXIT.                                                       EL659
00500      EXIT.                                                        EL659
00501  EJECT                                                            EL659
00502  5000-SEND-DATA-ONLY SECTION.                                     EL659
00503      MOVE SAVE-DATE              TO  ADATEO.                      EL659
00504      MOVE EIBTIME                TO  TIME-IN.                     EL659
00505      MOVE TIME-OUT               TO  ATIMEO.                      EL659
00506                                                                   EL659
00507      IF EMI-ERROR  IS NOT EQUAL TO  ZERO                          EL659
00508          PERFORM 9500-ERROR-FORMAT.                               EL659
00509                                                                   EL659
00510      MOVE EMI-MESSAGE-AREA (1)   TO  AEMSG1O.                     EL659
00511      MOVE EMI-MESSAGE-AREA (2)   TO  AEMSG2O.                     EL659
00512                                                                   EL659
00513      EXEC CICS SEND DATAONLY                                      EL659
00514          FROM    (EL659AO)                                        EL659
00515          MAPSET  (WS-MAPSET-NAME)                                 EL659
00516          MAP     (WS-MAP-NAME)                                    EL659
00517          CURSOR                                                   EL659
00518      END-EXEC.                                                    EL659
00519                                                                   EL659
00520  5099-EXIT.                                                       EL659
00521      EXIT.                                                        EL659
00522  EJECT                                                            EL659
00523  6000-SEND-TEXT  SECTION.                                         EL659
00524      EXEC CICS SEND TEXT                                          EL659
00525          FROM    (LOGOFF-TEXT)                                    EL659
00526          LENGTH  (LOGOFF-LENGTH)                                  EL659
00527          ERASE                                                    EL659
00528          FREEKB                                                   EL659
00529      END-EXEC.                                                    EL659
00530                                                                   EL659
00531      EXEC CICS RETURN                                             EL659
00532          END-EXEC.                                                EL659
00533                                                                   EL659
00534  6099-EXIT.                                                       EL659
00535      EXIT.                                                        EL659
00536  EJECT                                                            EL659
00537  7000-DATE-CONVERSION  SECTION.                                   EL659
00538      EXEC CICS LINK                                               EL659
00539          PROGRAM   ('ELDATCV')                                    EL659
00540          COMMAREA  (DATE-CONVERSION-DATA)                         EL659
00541          LENGTH    (DC-COMM-LENGTH)                               EL659
00542      END-EXEC.                                                    EL659
00543                                                                   EL659
00544  7099-EXIT.                                                       EL659
00545      EXIT.                                                        EL659
00546                                                                   EL659
00547  EJECT                                                            EL659
00548  9000-RETURN-CICS  SECTION.                                       EL659
00549      MOVE 'EL005'                TO  THIS-PGM.                    EL659
00550      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL659
00551                                                                   EL659
00552      GO TO 9200-XCTL.                                             EL659
00553                                                                   EL659
00554  9099-EXIT.                                                       EL659
00555      EXIT.                                                        EL659
00556                                                                   EL659
00557  9100-RETURN-TRAN  SECTION.                                       EL659
00558      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL659
00559      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL659
00560                                                                   EL659
00561      EXEC CICS RETURN                                             EL659
00562          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL659
00563          LENGTH    (PI-COMM-LENGTH)                               EL659
00564          TRANSID   (WS-TRANS-ID)                                  EL659
00565      END-EXEC.                                                    EL659
00566                                                                   EL659
00567  9199-EXIT.                                                       EL659
00568      EXIT.                                                        EL659
00569                                                                   EL659
00570  9200-XCTL  SECTION.                                              EL659
00571      MOVE DFHENTER               TO  EIBAID.                      EL659
00572                                                                   EL659
00573      EXEC CICS XCTL                                               EL659
00574          PROGRAM   (THIS-PGM)                                     EL659
00575          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL659
00576          LENGTH    (PI-COMM-LENGTH)                               EL659
00577      END-EXEC.                                                    EL659
00578                                                                   EL659
00579  9299-EXIT.                                                       EL659
00580      EXIT.                                                        EL659
00581  EJECT                                                            EL659
00582  9300-CLEAR  SECTION.                                             EL659
00583      MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                    EL659
00584                                                                   EL659
00585      GO TO 9200-XCTL.                                             EL659
00586                                                                   EL659
00587  9399-EXIT.                                                       EL659
00588      EXIT.                                                        EL659
00589                                                                   EL659
00590  9400-PGMIDERR  SECTION.                                          EL659
00591      EXEC CICS HANDLE CONDITION                                   EL659
00592          PGMIDERR  (6000-SEND-TEXT)                               EL659
00593      END-EXEC.                                                    EL659
00594                                                                   EL659
00595      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM           EL659
00596                                      LOGOFF-PGM.                  EL659
00597      MOVE 'EL005'                TO  THIS-PGM.                    EL659
00598      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL659
00599      MOVE SPACES                 TO  PI-ENTRY-CD-1.               EL659
00600                                                                   EL659
00601      GO TO 9200-XCTL.                                             EL659
00602                                                                   EL659
00603  9499-EXIT.                                                       EL659
00604      EXIT.                                                        EL659
00605  EJECT                                                            EL659
00606  9500-ERROR-FORMAT  SECTION.                                      EL659
00607      EXEC CICS LINK                                               EL659
00608          PROGRAM   ('EL001')                                      EL659
00609          COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)                EL659
00610          LENGTH    (EMI-COMM-LENGTH)                              EL659
00611      END-EXEC.                                                    EL659
00612                                                                   EL659
00613  9599-EXIT.                                                       EL659
00614      EXIT.                                                        EL659
00615                                                                   EL659
00616  9600-ERROR  SECTION.                                             EL659
00617      MOVE DFHEIBLK               TO  EMI-LINE1.                   EL659
00618                                                                   EL659
00619      EXEC CICS LINK                                               EL659
00620          PROGRAM   ('EL004')                                      EL659
00621          COMMAREA  (EMI-LINE1)                                    EL659
00622          LENGTH    (72)                                           EL659
00623      END-EXEC.                                                    EL659
00624                                                                   EL659
00625      PERFORM 5000-SEND-DATA-ONLY.                                 EL659
00626                                                                   EL659
00627      GO TO 9100-RETURN-TRAN.                                      EL659
00628                                                                   EL659
00629  9699-EXIT.                                                       EL659
00630      EXIT.                                                        EL659
00631                                                                   EL659
00632  9700-SECURITY-VIOLATION.                                         EL659
00633                                  COPY ELCSCTP.                    EL659
00634                                                                   EL659
00635  9799-EXIT.                                                       EL659
00636      EXIT.                                                        EL659
00637                                                                   EL659
00638  9999-LAST-PARAGRAPH  SECTION.                                    EL659
00639      GOBACK.                                                      EL659
