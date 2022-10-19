00001  IDENTIFICATION DIVISION.                                         04/16/98
00002                                                                   ECS016
00003  PROGRAM-ID.                 ECS016.                                 LV019
00004 *              PROGRAM CONVERTED BY                               ECS016
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   ECS016
00006 *              CONVERSION DATE 11/28/95 10:57:04.                 ECS016
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            ECS016
00008 *                            VMOD=2.014                              CL*10
00009                                                                   ECS016
00010 *AUTHOR.     LOGIC, INC.                                          ECS016
00011 *            DALLAS, TEXAS.                                       ECS016
00012                                                                   ECS016
00013 *DATE-COMPILED.                                                   ECS016
00025 *REMARKS.                                                         ECS016
00026 *        THIS PROGRAM WILL MERGE ITD EP-EC EXTRACTS               ECS016
00027 *        AND CURRENT MONTH EP-EC EXTRACTS. IN THE EVENT           ECS016
00028 *        OF AN EXPIRY OR EFFECTIVE DATE CHANGE, ALL ITD           ECS016
00029 *        EXTRACTS WILL BE CHANGED TO MATCH.                       ECS016
00030                                                                   ECS016
00031 *        BOTH FILES WILL BE SEQUENCE CHECKED. IN THE CASE OF      ECS016
00032 *        AN OUT OF SEQUENCE CONDITION BOTH CONTROLS WILL          ECS016
00033 *        BE DISPLAYED AND PROCESSING TERMINATED.                  ECS016
00034                                                                   ECS016
00035 *        THE RUN DATE IN THE CURRENT EP-EC EXTRACTS WILL BE       ECS016
00036 *        COMPARED TO THE DATE CARD RUN DATE. ANY MISMATCH WILL    ECS016
00037 *        CAUSE A PROGRAM TERMINATION.                             ECS016
00038                                                                   ECS016
00039 *        A NEW ACCOUNT MASTER WILL BE CREATED WITH ALL THE        ECS016
00040 *        APPROPRIATE FIELDS INITIALIZED.                          ECS016
071003******************************************************************
071003*                   C H A N G E   L O G
071003*
071003* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
071003*-----------------------------------------------------------------
071003*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
071003* EFFECTIVE    NUMBER
071003*-----------------------------------------------------------------
071003* 071003                   SMVA  FIX DISPLAY-PRT OUTPUT SO FIRST 
071003*                                RPT REC HAS CC OF '1' FOR CIB009L
040512* 040512  IR2012020200003  AJRA  DO NOT PURGE RECS WITH CLAIM ADJ
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
121514* 121514  CR201412150000?  PEMA  Keep year-end recs for 9 years
040317* 040317  CR2017040300001  PEMA  Abort when mismatch w/eracct
071003******************************************************************
00041                                                                   ECS016
00042  ENVIRONMENT DIVISION.                                            ECS016
00043  CONFIGURATION SECTION.                                           ECS016
00044                                                                      CL*17
00045  INPUT-OUTPUT SECTION.                                            ECS016
00046  FILE-CONTROL.                                                    ECS016
00047                                                                   ECS016
00048      SELECT  SORT-WORK       ASSIGN TO SYS001-DA-FBA1-S-SORTWK1.  ECS016
00049      SELECT  PRNTR           ASSIGN TO SYS008-UR-1403-S-SYS008.   ECS016
00050      SELECT  EPEC-IN         ASSIGN TO SYS010-UT-2400-S-SYS010.   ECS016
00051      SELECT  EXTR-IN         ASSIGN TO SYS018-UT-2400-S-SYS018.   ECS016
00052      SELECT  EPEC-OUT        ASSIGN TO SYS012-UT-2400-S-SYS012.   ECS016
00053      SELECT  ACCT-OUT        ASSIGN TO SYS014-UT-2400-S-SYS014.   ECS016
00054      SELECT ERACCTT          ASSIGN TO SYS010-FBA1-ERACCTT        ECS016
00055              ORGANIZATION IS INDEXED                              ECS016
00056              ACCESS IS DYNAMIC                                    ECS016
00057              RECORD KEY IS AM-CONTROL-PRIMARY                     ECS016
00058              FILE STATUS IS ERACCTT-FILE-STATUS.                  ECS016
00059      SELECT  DISK-DATE       ASSIGN TO SYS019-UT-FBA1-S-SYS019.   ECS016
00060      SELECT  FICH            ASSIGN TO SYS020-UT-2400-S-SYS020.   ECS016
CIDMOD     SELECT  DISPLAY-PRT     ASSIGN TO SYS022-UR-1403-S-SYS022.
070714     SELECT  ERMEBL          ASSIGN SYS024-FBA1-ERMEBL      
070714                             ORGANIZATION INDEXED           
070714                             ACCESS DYNAMIC                 
070714                             RECORD KEY ME-CONTROL-PRIMARY  
070714                             FILE STATUS ERMEBL-FILE-STATUS.

00062  DATA DIVISION.                                                   ECS016
00063  FILE SECTION.                                                    ECS016
00064                                                                   ECS016
00065  SD  SORT-WORK.                                                   ECS016
00066                                                                   ECS016
00067  01  SORT-WORK-REC.                                               ECS016
00068      12  SW-REC-TYPE             PIC XX.                          ECS016
00069      12  FILLER                  PIC XX.                          ECS016
00070      12  SW-CCSA.                                                 ECS016
00071          16  SW-CARR             PIC X.                           ECS016
00072          16  SW-GROUPING         PIC X(6).                        ECS016
00073          16  SW-STATE            PIC XX.                          ECS016
00074          16  SW-ACCOUNT          PIC X(10).                       ECS016
00075      12  SW-EXPIRATION           PIC 9(11)  COMP-3.                  CL**4
00076      12  SW-EFFECTIVE            PIC 9(11)  COMP-3.                  CL**4
00077      12  SW-REIN-COMP            PIC X(6).                        ECS016
00078      12  SW-RCD-TYPE             PIC X.                           ECS016
00079      12  SW-BEN-TYPE             PIC XX.                          ECS016
00080      12  SW-COMM-SEQ-NO          PIC X.                           ECS016
00081      12  FILLER                  PIC X(273).                      ECS016
00082      12  SW-PURGE-CODE           PIC X.                           ECS016
00083      12  SW-RUN-DATE             PIC 9(11)  COMP-3.                  CL**4
00084                                                                   ECS016
00085  FD  PRNTR                                                        ECS016
00086                              COPY ELCPRTFD.                       ECS016
00087  EJECT                                                            ECS016
00088  FD  EPEC-IN                                                      ECS016
00089      BLOCK CONTAINS 0 RECORDS
00090      RECORDING MODE F.                                            ECS016
00091                                                                   ECS016
00092  01  EPEC-IN-REC.                                                 ECS016
00093      12  EIR-REC-TYPE            PIC XX.                          ECS016
00094      12  FILLER                  PIC XX.                          ECS016
00095      12  EIR-CCSA.                                                ECS016
00096          16  EIR-CARRIER         PIC X.                           ECS016
00097          16  EIR-GROUPING        PIC X(6).                        ECS016
00098          16  EIR-STATE           PIC XX.                          ECS016
00099          16  EIR-ACCOUNT         PIC X(10).                       ECS016
00100      12  EIR-EXPIRATION          PIC 9(11)  COMP-3.                  CL**4
00101      12  EIR-EFFECTIVE           PIC 9(11)  COMP-3.                  CL**4
00102      12  EIR-REIN-COMP           PIC X(6).                        ECS016
00103      12  EIR-LIFE-AH             PIC X.                           ECS016
00104      12  EIR-BEN-TYPE            PIC XX.                          ECS016
00105      12  EIR-COMM-SEQ-NO         PIC X.                           ECS016
00106      12  FILLER                  PIC X(273).                      ECS016
00107      12  EIR-PURGE-CODE          PIC X.                           ECS016
00108      12  EIR-RUN-DATE            PIC 9(11)  COMP-3.                  CL**4
00109                                                                   ECS016
00110  FD  EXTR-IN                                                      ECS016
00111      BLOCK CONTAINS 0 RECORDS
00112      RECORDING MODE F.                                            ECS016
00113                                                                   ECS016
00114  01  EXTR-IN-REC.                                                 ECS016
00115      12  EXTR-REC-TYPE           PIC XX.                          ECS016
00116      12  FILLER                  PIC X(317).                      ECS016
00117      12  EXTR-REC-RUN-DATE       PIC 9(11)  COMP-3.                  CL**4
00118                                                                   ECS016
00119  FD  EPEC-OUT                                                     ECS016
00120      BLOCK CONTAINS 0 RECORDS
00121      RECORDING MODE F.                                            ECS016
00122                                                                   ECS016
00123  01  EPEC-OUT-REC                PIC X(325).                      ECS016
00124  EJECT                                                            ECS016
00125  FD  ERACCTT.                                                     ECS016
00126                              COPY ERCACCT.                        ECS016
00127  EJECT                                                            ECS016
00128  FD  ACCT-OUT                                                     ECS016
00129      BLOCK CONTAINS 0 RECORDS
00130      RECORDING MODE F.                                            ECS016
00131                                                                   ECS016
00132  01  ACCT-OUT-REC                PIC X(2000).                     ECS016
00133                                                                   ECS016
00134  FD  DISK-DATE                                                    ECS016
00135                              COPY ELCDTEFD.                       ECS016
00136  FD  FICH                                                         ECS016
00137                              COPY ELCFCHFD.                       ECS016
CIDMOD                                                                  00000626
CIDMOD FD  DISPLAY-PRT                                                  00000627
CIDMOD     RECORDING MODE F                                             00000628
CIDMOD     LABEL RECORDS ARE STANDARD                                   00000629
CIDMOD     RECORD CONTAINS 133 CHARACTERS                               00000630
CIDMOD     BLOCK CONTAINS 0 RECORDS                                     00000631
CIDMOD     DATA RECORD IS DISPLAY-REC.                                  00000632
CIDMOD                                                                  00000633
CIDMOD 01  DISPLAY-REC.                                                 00000634
CIDMOD     12  DISPLAY-CC              PIC X.                           00000635
CIDMOD     12  DISPLAY-INFO            PIC X(132).                      00000636
CIDMOD                                                                  00000637
070714 FD  ERMEBL.
070714                                 COPY ERCMEBL.
00138  EJECT                                                            ECS016
00139  WORKING-STORAGE SECTION.                                         ECS016
00140  77  FILLER  PIC X(32) VALUE '********************************'.  ECS016
00141  77  FILLER  PIC X(32) VALUE '     ECS016 WORKING STORAGE     '.  ECS016
00142  77  FILLER  PIC X(32) VALUE '******* VMOD=2.014 *************'.     CL*10
00143                                                                   ECS016
CIDMOD 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               00000644
CIDMOD 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             00000645
CIDMOD 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             00000646
CIDMOD                                                                  00000648
00144  01  MISC-SWITCHS.                                                ECS016
00145      12  EXTR-NEEDED-SW          PIC X   VALUE 'Y'.               ECS016
00146          88  EXTR-NEEDED                     VALUE 'Y'.           ECS016
00147      12  EPEC-NEEDED-SW          PIC X   VALUE 'Y'.               ECS016
00148          88  EPEC-NEEDED                     VALUE 'Y'.           ECS016
00149      12  ACCT-NEEDED-SW          PIC X   VALUE 'Y'.               ECS016
00150          88  ACCT-NEEDED                     VALUE 'Y'.           ECS016
00151      12  EXTR-ACCT-SW            PIC X   VALUE 'N'.               ECS016
00152          88  EXTR-ACCT-EQUAL                 VALUE 'Y'.           ECS016
00153      12  EPEC-ACCT-SW            PIC X   VALUE 'N'.               ECS016
00154          88  EPEC-ACCT-EQUAL                 VALUE 'Y'.           ECS016
00155      12  PURGE-SW                PIC X   VALUE 'N'.               ECS016
00156          88  PURGE-THIS-RECORD               VALUE 'Y'.           ECS016
00157      12  END-OF-FILE-SWITCHS.                                     ECS016
00158          16  EXTR-END-SW         PIC X   VALUE 'N'.               ECS016
00159              88  END-OF-EXTR                 VALUE 'Y'.           ECS016
00160          16  EPEC-END-SW         PIC X   VALUE 'N'.               ECS016
00161              88  END-OF-EPEC                 VALUE 'Y'.           ECS016
00162          16  ACCT-END-SW         PIC X   VALUE 'N'.               ECS016
00163              88  END-OF-ACCT                 VALUE 'Y'.           ECS016
00164      12  END-OF-FILE-SWITCHS   REDEFINES END-OF-FILE-SWITCHS.     ECS016
00165          16  FILLER              PIC X(3).                        ECS016
00166              88  END-OF-ALL-FILES            VALUE 'YYY'.         ECS016
00167      12  EPEC-RECORD-SW          PIC X   VALUE 'N'.               ECS016
00168          88  FROM-EPEC-FILE                  VALUE 'Y'.           ECS016
00169      12  NEW-ACCT-NEEDED         PIC X   VALUE 'N'.               ECS016
00170          88  WRITE-SW                        VALUE 'Y'.           ECS016
00171      12  ACCT-DATE-CHG           PIC X   VALUE 'N'.               ECS016
00172                                                                   ECS016
CIDMOD 01  SAVE-EPEC-CON               PIC X(9) VALUE 'SAVE EPEC'.      00000647
CIDMOD 01  SAVE-EPEC.                                                   00000647
CIDMOD     12  SAVE-EP-REC-TYPE        PIC XX.                          00000648
CIDMOD     12  FILLER                  PIC XX.                          00000648
CIDMOD     12  SAVE-EP-CCSA            PIC X(19).                       00000648
CIDMOD     12  SAVE-EP-EXPIRATION      PIC 9(11) COMP-3.                00000648
CIDMOD     12  SAVE-EP-EFFECTIVE       PIC 9(11) COMP-3.                00000648
CIDMOD     12  SAVE-EP-REIN-COMP       PIC X(6).                        00000648
CIDMOD     12  SAVE-EP-LIFE-AH         PIC X.                           00000648
CIDMOD     12  SAVE-EP-BEN-TYPE        PIC XX.                          00000648
CIDMOD     12  SAVE-EP-COMM-SEQ-NO     PIC X.                           00000648
CIDMOD     12  FILLER                  PIC X(280).                      00000648
CIDMOD                                                                  00000648
00173  01  MISC-WORK-AREAS.                                             ECS016
00174      12  X                       PIC X       VALUE SPACE.         ECS016
00175      12  WS-RETURN-CODE          PIC S9(4) COMP.                  ECS016
00176      12  WS-ABEND-CODE           PIC S9(4) COMP.                  ECS016
00177      12  WS-ABEND-MESSAGE        PIC X(80).                       ECS016
00178      12  WS-ABEND-FILE-STATUS    PIC XX  VALUE ZEROS.             ECS016
00179      12  ERACCTT-FILE-STATUS     PIC XX  VALUE ZEROS.             ECS016
00180      12  WS-ZERO                 PIC S9  VALUE ZERO COMP-3.       ECS016
00181      12  PGM-SUB                 PIC 9(4)    VALUE 016       COMP.ECS016
00182      12  SAVE-HIGH-CERT          PIC 9(11) COMP-3 VALUE ZERO.        CL*17
00183      12  SAVE-LOW-CERT           PIC 9(11) COMP-3                    CL*17
00184                                           VALUE 99999999999.         CL*17
00185      12  AINDEX                  PIC 9999   COMP.                 ECS016
00186      12  WORK-RUN-DATE           PIC 9(11).                          CL**3
00187      12  WORK-RUN-DATE-R REDEFINES WORK-RUN-DATE.                    CL**2
00188          16  FILLER              PIC 999.                         ECS016
00189          16  WRD-CC              PIC 99.                          ECS016
00190          16  WRD-YR              PIC 99.                          ECS016
00191          16  WRD-MO              PIC 99.                          ECS016
00192          16  WRD-DA              PIC 99.                          ECS016
00193      12  PURGE-DEC               PIC 9(04).                       ECS016
00194      12  PURGE-DATE              PIC 9(11).                       ECS016
00195      12  PURGE-DATE-R  REDEFINES  PURGE-DATE.                     ECS016
00196          16  FILLER              PIC 999.                         ECS016
00197          16  PURGE-CCYY          PIC 9(04).                       ECS016
00198          16  PURGE-CCYR  REDEFINES PURGE-CCYY.                    ECS016
00199              20  PURGE-CC        PIC 99.                          ECS016
00200              20  PURGE-YR        PIC 99.                          ECS016
00201          16  PURGE-MO            PIC 99.                          ECS016
00202          16  PURGE-DA            PIC 99.                          ECS016
00203      12  DISPLAY1-DATE           PIC 9(11).                          CL*11
00204      12  DISPLAY1-DATE-R REDEFINES DISPLAY1-DATE.                    CL*11
00205          16  FILLER              PIC 999.                            CL*11
00206          16  DIS1-CC             PIC 99.                             CL*11
00207          16  DIS1-YR             PIC 99.                             CL*11
00208          16  DIS1-MO             PIC 99.                             CL*11
00209          16  DIS1-DA             PIC 99.                             CL*11
00210      12  DISPLAY2-DATE           PIC 9(11).                          CL*11
00211      12  DISPLAY2-DATE-R REDEFINES DISPLAY2-DATE.                    CL*11
00212          16  FILLER              PIC 999.                            CL*11
00213          16  DIS2-CC             PIC 99.                             CL*11
00214          16  DIS2-YR             PIC 99.                             CL*11
00215          16  DIS2-MO             PIC 99.                             CL*11
00216          16  DIS2-DA             PIC 99.                             CL*11
00217                                                                   ECS016
00218                                                                   ECS016
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Month end balancing work area                           ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714 01  MONTH-END-DATA.                                          
070714     12  ME-START-DATE.                                       
070714         16  ME-START-MO     PIC  99.                         
070714         16  FILLER          PIC  X.                          
070714         16  ME-START-DA     PIC  99.                         
070714         16  FILLER          PIC  X.                          
070714         16  ME-START-YR     PIC  99.                         
070714     12  ME-CNDS-DATE        PIC  9(6).                       
070714     12  ME-CNDS-DATE-R  REDEFINES  ME-CNDS-DATE.             
070714         16  ME-CNDS-MO      PIC  99.                         
070714         16  ME-CNDS-DA      PIC  99.                         
070714         16  ME-CNDS-YR      PIC  99.                         
070714     12  ME-START-TIME       PIC  9(6).                       
070714     12  ME-UPDATE-FLAG      PIC  X          VALUE 'Y'.       
070714         88  ME-DO-UPDATE                    VALUE 'Y'.       
070714         88  ME-NO-UPDATE                    VALUE 'N'.       
070714     12  ERMEBL-FILE-STATUS  PIC  XX.                         
070714     12  MONTH-END-MOYR      PIC  9(4)                 COMP.  
070714     12  hld-016-RECS-IN         PIC S9(9)  COMP-3 value +0.
070714     12  hld-016-RECS-OUT        PIC S9(9)  COMP-3 value +0.

00219  01  MISC-COMP-3-AREAS       COMP-3.                              ECS016
00220      12  EPEC-IN-COUNT           PIC 9(9)    VALUE ZERO.          ECS016
00221      12  EPEC-OUT-COUNT          PIC 9(9)    VALUE ZERO.          ECS016
00222      12  EPEC-PURGED-COUNT       PIC 9(9)    VALUE ZERO.          ECS016
00223      12  EXTR-ADDED-COUNT        PIC 9(9)    VALUE ZERO.          ECS016
00224      12  ACCT-WRITE-COUNT        PIC 9(9)    VALUE ZERO.          ECS016
00225      12  ACCT-READ-COUNT         PIC 9(9)    VALUE ZERO.          ECS016
00226  EJECT                                                            ECS016
00227  01  MISC-CONTROL-WORK-AREAS.                                     ECS016
00228      12  WORK-ACCOUNT-CONTROL-1.                                  ECS016
00229          16  WORK-ACCOUNT-CONTROL-1-DISPLAY.                         CL*11
00230              20  WAC1-CARR       PIC X       VALUE SPACES.           CL*11
00231              20  WAC1-COMP       PIC X(6)    VALUE SPACES.           CL*11
00232              20  WAC1-STATE      PIC XX      VALUE SPACES.           CL*11
00233              20  WAC1-ACCOUNT    PIC X(10)   VALUE SPACES.           CL*11
00234          16  WAC1-EXPIRATION     PIC 9(11)   VALUE 0 COMP-3.         CL**4
00235          16  WAC1-EFFECTIVE      PIC 9(11)   VALUE 0 COMP-3.         CL**4
00236                                                                   ECS016
00237      12  WORK-ACCOUNT-CONTROL-2.                                  ECS016
00238          16  WORK-ACCOUNT-CONTROL-2-DISPLAY.                         CL*11
00239              20  WAC2-CARR       PIC X       VALUE SPACES.           CL*11
00240              20  WAC2-COMP       PIC X(6)    VALUE SPACES.           CL*11
00241              20  WAC2-STATE      PIC XX      VALUE SPACES.           CL*11
00242              20  WAC2-ACCOUNT    PIC X(10)   VALUE SPACES.           CL*11
00243          16  WAC2-EXPIRATION     PIC 9(11)   VALUE 0 COMP-3.         CL**4
00244          16  WAC2-EFFECTIVE      PIC 9(11)   VALUE 0 COMP-3.         CL**4
00245                                                                   ECS016
00246      12  WORK-EXTR-CONTROL.                                       ECS016
00247          16  WEX-CONTROL-A.                                       ECS016
00248              20  WEX-CONTROL-DISPLAY.                                CL*11
00249                  24  WEX-CARR    PIC X       VALUE SPACES.           CL*11
00250                  24  WEX-COMP    PIC X(6)    VALUE SPACES.           CL*11
00251                  24  WEX-STATE   PIC XX      VALUE SPACES.           CL*11
00252                  24  WEX-ACCOUNT PIC X(10)   VALUE SPACES.           CL*11
00253              20  WEX-EXPIRATION  PIC 9(11)   VALUE 0 COMP-3.         CL**4
00254              20  WEX-EFFECTIVE   PIC 9(11)   VALUE 0 COMP-3.         CL**4
00255          16  WEX-CONTROL-B.                                       ECS016
00256              20  WEX-REIN-COMP   PIC X(6)    VALUE SPACES.           CL**5
00257              20  WEX-RUN-DATE    PIC 9(11)   VALUE 0 COMP-3.         CL**4
00258              20  WEX-LIFE-AH     PIC X       VALUE SPACES.        ECS016
00259              20  WEX-BEN-TYPE    PIC XX      VALUE SPACES.        ECS016
00260              20  WEX-PURGE-CODE  PIC X       VALUE SPACES.        ECS016
00261              20  WEX-REC-TYPE    PIC XX      VALUE SPACES.        ECS016
00262              20  WEX-COMM-SEQ-NO PIC X       VALUE SPACES.        ECS016
00263                                                                   ECS016
00264      12  WORK-EPEC-CONTROL.                                       ECS016
00265          16  WEP-CONTROL-A.                                       ECS016
00266              20  WEP-CONTROL-DISPLAY.                                CL*15
00267                  24  WEP-CARR    PIC X       VALUE SPACES.           CL*11
00268                  24  WEP-COMP    PIC X(6)    VALUE SPACES.           CL*11
00269                  24  WEP-STATE   PIC XX      VALUE SPACES.           CL*11
00270                  24  WEP-ACCOUNT PIC X(10)   VALUE SPACES.           CL*11
00271              20  WEP-EXPIRATION  PIC 9(11)   VALUE 0 COMP-3.         CL*11
00272              20  WEP-EFFECTIVE   PIC 9(11)   VALUE 0 COMP-3.         CL*11
00273          16  WEP-CONTROL-B.                                       ECS016
00274              20  WEP-REIN-COMP   PIC X(6)    VALUE SPACES.        ECS016
00275              20  WEP-RUN-DATE    PIC 9(11)   VALUE 0 COMP-3.         CL*11
00276              20  WEP-LIFE-AH     PIC X       VALUE SPACES.        ECS016
00277              20  WEP-BEN-TYPE    PIC XX      VALUE SPACES.        ECS016
00278              20  WEP-PURGE-CODE  PIC X       VALUE SPACES.        ECS016
00279              20  WEP-REC-TYPE    PIC XX      VALUE SPACES.        ECS016
00280              20  WEP-COMM-SEQ-NO PIC X       VALUE SPACES.        ECS016
00281                                                                   ECS016
00282      12  WORK-EPEC-CONTROL-2.                                     ECS016
00283          16  FILLER              PIC X(19)   VALUE SPACES.        ECS016
00284          16  WEP2-EXPIRATION     PIC 9(11)   VALUE 0 COMP-3.         CL**4
00285          16  WEP2-EFFECTIVE      PIC 9(11)   VALUE 0 COMP-3.         CL**4
00286          16  FILLER              PIC X(19)   VALUE SPACES.        ECS016
00287                                                                   ECS016
00288      12  PREV-EPEC-IN            PIC X(50)   VALUE SPACES.        ECS016
00289      12  PREV-EPEC-OUT           PIC X(50)   VALUE SPACES.        ECS016
00290      12  CURR-EPEC-OUT           PIC X(50)   VALUE SPACES.        ECS016
00291                                                                   ECS016
00292      EJECT                                                        ECS016
00293  01  ERROR-MESSAGES.                                              ECS016
00294      12  ER-0101                 PIC X(4)    VALUE '0101'.        ECS016
00295      12  ER-0302                 PIC X(4)    VALUE '0302'.        ECS016
00296      12  ER-0610                 PIC X(4)    VALUE '0610'.        ECS016
00297      12  ER-0612                 PIC X(4)    VALUE '0612'.        ECS016
00298      12  ER-0998                 PIC X(4)    VALUE '0998'.        ECS016
00299                                                                   ECS016
00300  01  HEADING-1.                                                   ECS016
00301      12  FILLER              PIC X(55)   VALUE SPACES.            ECS016
00302      12  FILLER              PIC X(15)   VALUE 'EPEC FILE MERGE'. ECS016
00303      12  FILLER              PIC X(50)   VALUE SPACES.            ECS016
00304      12  FILLER              PIC X(7)    VALUE 'ECS016'.          ECS016
00305                                                                   ECS016
00306  01  HEADING-2.                                                   ECS016
00307      12  FILLER              PIC X(47)   VALUE SPACES.            ECS016
00308      12  H2-CLIENT           PIC X(30)   VALUE SPACES.            ECS016
00309      12  FILLER              PIC X(43)   VALUE SPACES.            ECS016
00310      12  H2-RUN-DATE         PIC X(8)    VALUE SPACES.            ECS016
00311                                                                   ECS016
00312  01  HEADING-3.                                                   ECS016
00313      12  FILLER              PIC X(53)   VALUE SPACES.            ECS016
00314      12  H3-ALPH-DATE        PIC X(18)   VALUE SPACES.            ECS016
00315      12  FILLER              PIC X(41)   VALUE SPACES.            ECS016
00316      12  FILLER              PIC X(5)    VALUE 'PAGE'.            ECS016
00317      12  H3-PAGE-NO          PIC ZZ,ZZZ.                          ECS016
00318                                                                   ECS016
00319  01  DETAIL-LINE.                                                 ECS016
00320      12  DL-MSG              PIC X(22).                           ECS016
00321      12  DL-COUNT            PIC ZZZ,ZZZ,ZZ9.                     ECS016
CIDMOD                                                                  00003074
CIDMOD 01  DISPLAY-HD-1.                                                00003075
071003     12  FILLER      PIC X(01) VALUE '1'.  
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      00003077
CIDMOD     12  FILLER      PIC X(23) VALUE 'PROCESSING ERROR REPORT'.   00003078
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      00003079
CIDMOD     12  FILLER      PIC X(07) VALUE 'ECS-016'.                   00003080
CIDMOD                                                                  00003270
CIDMOD 01  DISPLAY-HD-2.                                                00003271
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      00003272
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      00003273
CIDMOD     12  FILLER      PIC X(56) VALUE SPACES.                      00003274
CIDMOD     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      00003275
CIDMOD     12  FILLER      PIC X(67) VALUE SPACES.                      00003276
CIDMOD                                                                  00003277
CIDMOD 01  DISPLAY-LINE.                                                00003278
CIDMOD     05  DISPLAY-LINE-05.                                         00003279
CIDMOD        10  DIS-CC                 PIC X.                         00003280
CIDMOD        10  DIS-LINE-REASON        PIC X(32).                     00003281
CIDMOD        10  DIS-LINE-REC.                                         00003281
CIDMOD           12  DIS-LINE-CONTROL       PIC X(19).                  00000648
CIDMOD           12  DIS-LINE-EXPIRATION    PIC 9(8).                   00000648
CIDMOD           12  DIS-LINE-EFFECTIVE     PIC 9(8).                   00000648
CIDMOD           12  DIS-LINE-REIN-COMP     PIC X(6).                   00000648
CIDMOD           12  DIS-LINE-RUN-DATE      PIC 9(8).                   00000648
CIDMOD           12  DIS-LINE-LIFE-AH       PIC X.                      00000648
CIDMOD           12  DIS-LINE-BEN-TYPE      PIC XX.                     00000648
CIDMOD           12  DIS-LINE-PURGE-CODE    PIC X.                      00000648
CIDMOD           12  DIS-LINE-REC-TYPE      PIC XX.                     00000648
CIDMOD           12  DIS-LINE-COMM-SEQ-NO   PIC X.                      00000648
CIDMOD           12  FILLER                 PIC X(44).                  00000648
CIDMOD                                                                  00003400
CIDMOD     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            00003401
CIDMOD        10  DIS-CC-ALT          PIC X.                            00003402
CIDMOD        10  DIS-FLD-1           PIC X(52).                        00003403
CIDMOD        10  DIS-FLD-2.                                            00003404
CIDMOD          12  DIS-FLD2-REC-TYPE       PIC XX.                     00000648
CIDMOD          12  FILLER                  PIC XX.                     00000648
CIDMOD          12  DIS-FLD2-CCSA           PIC X(19).                  00000648
CIDMOD          12  DIS-FLD2-EXPIRATION     PIC 9(8).                   00000648
CIDMOD          12  DIS-FLD2-EFFECTIVE      PIC 9(8).                   00000648
CIDMOD          12  DIS-FLD2-REIN-COMP      PIC X(6).                   00000648
CIDMOD          12  DIS-FLD2-LIFE-AH        PIC X.                      00000648
CIDMOD          12  DIS-FLD2-BEN-TYPE       PIC XX.                     00000648
CIDMOD          12  DIS-FLD2-COMM-SEQ-NO    PIC X.                      00000648
CIDMOD          12  FILLER                  PIC X(31).                  00000648
CIDMOD                                                                  00003405
00322  EJECT                                                            ECS016
00323                              COPY ECSEPC01.                       ECS016
00324  EJECT                                                            ECS016
00325                              COPY ELCEPCVR.                       ECS016
00326  EJECT                                                            ECS016
00327                              COPY ELCACCTV.                       ECS016
00328                                                                   ECS016
00329                              COPY ELCDTECX.                       ECS016
00330                                                                   ECS016
00331                              COPY ELCDTEVR.                       ECS016
00332  EJECT                                                            ECS016
00333  PROCEDURE DIVISION.                                              ECS016
00334                                                                   ECS016
00335  0000-READ-DATE-CARD.                                             ECS016
00336                              COPY ELCDTERX.                       ECS016
00337                                                                   ECS016
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***
070714****                                                           ***
070714****   Set up the month-end auto balancing.                    ***
070714****                                                           ***
070714****=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=_=***

070714     MOVE WS-TIME                TO ME-START-TIME
070714     MOVE WS-CURRENT-DATE        TO ME-START-DATE
070714     MOVE ME-START-MO            TO ME-CNDS-MO
070714     MOVE ME-START-DA            TO ME-CNDS-DA
070714     MOVE ME-START-YR            TO ME-CNDS-YR

           .
00338  0100-MAIN-LINE.                                                  ECS016
CIDMOD     MOVE WS-CURRENT-DATE        TO DIS-DATE.
00339      MOVE WS-CURRENT-DATE        TO H2-RUN-DATE.                  ECS016
00340      MOVE COMPANY-NAME           TO H2-CLIENT.                    ECS016
00341      MOVE ALPH-DATE              TO H3-ALPH-DATE.                 ECS016
00342      MOVE RUN-DATE               TO WORK-RUN-DATE.                ECS016
00343                                                                   ECS016
00344      IF DTE-CLIENT = 'TLA' OR 'RBL' OR 'ELS' OR 'SNI' OR 'VSL'    ECS016
00345          MOVE ZEROS              TO PURGE-DEC                     ECS016
00346                                     PURGE-DATE                    ECS016
00347      ELSE                                                         ECS016
00348          IF DTE-CLIENT = 'MON'                                    ECS016
00349              COMPUTE PURGE-DEC = RUN-CCYY - 6                     ECS016
00350              MOVE WORK-RUN-DATE  TO PURGE-DATE                       CL**9
00351              SUBTRACT 1 FROM PURGE-CCYY                           ECS016
00352              MOVE 31             TO PURGE-DA                      ECS016
00353              IF PURGE-MO GREATER 01                               ECS016
00354                  SUBTRACT 1 FROM PURGE-MO                         ECS016
00355              ELSE                                                 ECS016
00356                  SUBTRACT 1 FROM PURGE-CCYY                       ECS016
00357                  ADD 11          TO PURGE-MO                      ECS016
00358          ELSE                                                     ECS016
121514             COMPUTE PURGE-DEC = RUN-CCYY - 9
00360              MOVE WORK-RUN-DATE  TO PURGE-DATE                    ECS016
00361              SUBTRACT 2 FROM PURGE-CCYY                           ECS016
00362              IF PURGE-MO GREATER 02                               ECS016
00363                  SUBTRACT 2 FROM PURGE-MO                         ECS016
00364              ELSE                                                 ECS016
00365                  SUBTRACT 1 FROM PURGE-CCYY                       ECS016
00366                  ADD 10 TO PURGE-MO.                              ECS016

121514     display ' work run date ' ws-run-date
121514     display ' purge date    ' purge-date
121514     display ' purge dec     ' purge-dec

00367                                                                   ECS016
00368      OPEN OUTPUT PRNTR.                                           ECS016
00369                                                                   ECS016
CIDMOD     OPEN OUTPUT DISPLAY-PRT.                                     00003604
CIDMOD                                                                  00003605
CIDMOD     PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT.           00003606
CIDMOD                                                                  00003607
00370      PERFORM 0200-MAIN-PROCESSING      THRU 0299-EXIT.            ECS016
00371                                                                   ECS016
00372      PERFORM 8000-PRINT-HEADINGS       THRU 8099-EXIT.            ECS016
00373                                                                   ECS016
00374      PERFORM 8300-PRINT-CONTROL-TOTALS THRU 8399-EXIT.            ECS016
00375                                                                   ECS016
CIDMOD         DISPLAY ' '.                                             00004021
CIDMOD         DISPLAY '* * * * * * * * * * * * * * * * * *'.           00004022
CIDMOD         DISPLAY ' DISPLAY ERROR COUNT = '  ERROR-COUNT.          00004023
CIDMOD         DISPLAY '* * * * * * * * * * * * * * * * * *'.           00004024
CIDMOD         DISPLAY ' '.                                             00004025
CIDMOD                                                                  00004026
071003*        PERFORM 8600-DISPLAY-HD  THRU                            00004027
071003*              8600-HD-EXIT.                                      00004028
CIDMOD                                                                  00004029
CIDMOD         MOVE ' DISPLAY ERROR COUNT = '    TO DIS-LINE-REASON     00004030
CIDMOD         MOVE ERROR-COUNT                TO  DIS-LINE-REC         00004031
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00004032
CIDMOD               8600-DISPLAY-EXIT.                                 00004033
CIDMOD                                                                  00004160
00376      CLOSE PRNTR.                                                 ECS016
00377                                                                   ECS016
CIDMOD     CLOSE DISPLAY-PRT.                                           00004163
CIDMOD                                                                  00004164
00378  0120-CLOSE-FICH.                                                 ECS016
00379                              COPY ELCPRTC.                        ECS016
070714     OPEN I-O ERMEBL.                                  
070714                                                       
070714     IF ERMEBL-FILE-STATUS <> ZERO and '97'
070714        MOVE 'N'                 TO ME-UPDATE-FLAG
070714        display ' me open ' ermebl-file-status
070714     end-if
070714     MOVE DTE-CLIENT             TO ME-COMPANY
070714                                                           
070714     COMPUTE MONTH-END-MOYR  =
070714        RUN-CCYY  *  12  +  RUN-MO
070714                                                           
070714     MOVE MONTH-END-MOYR         TO ME-MOYR
070714                                                           
070714     IF ME-DO-UPDATE                                       
070714        READ ERMEBL
070714        if ermebl-file-status <> '00'
070714           display ' me read ' ermebl-file-status
070714           MOVE 'N'              TO ME-UPDATE-FLAG    
070714           CLOSE ERMEBL
070714        end-if
070714     end-if

070714     IF ME-DO-UPDATE
070714        move hld-016-recs-in     to me-016-recs-in
070714        move hld-016-recs-out    to me-016-recs-out
070714        REWRITE MONTH-END-BALANCES
070714        CLOSE ERMEBL
070714     end-if

00381      GOBACK.                                                      ECS016
00382  EJECT                                                            ECS016
00383  0200-MAIN-PROCESSING.                                            ECS016
00384      SORT SORT-WORK ON ASCENDING                                  ECS016
00385              SW-CCSA                                              ECS016
00386              SW-EXPIRATION                                        ECS016
00387              SW-EFFECTIVE                                         ECS016
00388              SW-REIN-COMP                                         ECS016
00389              SW-RUN-DATE                                          ECS016
00390              SW-RCD-TYPE                                          ECS016
00391              SW-BEN-TYPE                                          ECS016
00392              SW-PURGE-CODE                                        ECS016
00393              SW-REC-TYPE                                          ECS016
00394              SW-COMM-SEQ-NO                                       ECS016
00395          INPUT  PROCEDURE 0300-SORT-EXTR         THRU 0399-EXIT   ECS016
00396          OUTPUT PROCEDURE 0600-PROCESS-ALL-FILES THRU 0699-EXIT.  ECS016
00397                                                                   ECS016
00398      IF SORT-RETURN NOT = ZEROS                                   ECS016
00399          MOVE ER-0101            TO WS-ABEND-CODE                 ECS016
00400          GO TO ABEND-PGM.                                         ECS016
00401                                                                   ECS016
00402  0299-EXIT.                                                       ECS016
00403      EXIT.                                                        ECS016
00404  EJECT                                                            ECS016
00405  0300-SORT-EXTR SECTION.                                          ECS016
00406      OPEN INPUT EXTR-IN.                                          ECS016
00407      PERFORM 0310-EXTR-LOOP THRU 0319-EXIT UNTIL END-OF-EXTR.     ECS016
00408      CLOSE EXTR-IN.                                               ECS016
00409      MOVE 'N'                    TO EXTR-END-SW.                  ECS016
00410      GO TO 0399-EXIT.                                             ECS016
00411                                                                   ECS016
00412  0310-EXTR-LOOP.                                                  ECS016
00413      PERFORM 9000-READ-EXTR THRU 9099-EXIT.                       ECS016
00414                                                                   ECS016
00415      IF END-OF-EXTR                                               ECS016
00416          GO TO 0319-EXIT.                                         ECS016
00417                                                                   ECS016
00418      ADD 1 TO EXTR-ADDED-COUNT.                                   ECS016
00419      MOVE EXTR-IN-REC            TO SORT-WORK-REC.                ECS016
00420      RELEASE SORT-WORK-REC.                                       ECS016
00421                                                                   ECS016
00422  0319-EXIT.                                                       ECS016
00423      EXIT.                                                        ECS016
00424                                                                   ECS016
00425  0399-EXIT.                                                       ECS016
00426      EXIT.                                                        ECS016
00427  EJECT                                                            ECS016
00428  0600-PROCESS-ALL-FILES SECTION.                                  ECS016
00429      OPEN INPUT  EPEC-IN                                          ECS016
00430                  ERACCTT                                          ECS016
00431           OUTPUT EPEC-OUT                                         ECS016
00432                  ACCT-OUT.                                        ECS016
00433                                                                   ECS016
00434      IF ERACCTT-FILE-STATUS  = '00' OR '97'                       ECS016
00435          NEXT SENTENCE                                            ECS016
00436        ELSE                                                       ECS016
00437          MOVE ERACCTT-FILE-STATUS                                 ECS016
00438                                  TO WS-ABEND-FILE-STATUS          ECS016
00439          MOVE ' ERACCTT OPEN ERROR- '                             ECS016
00440                                  TO WS-ABEND-MESSAGE              ECS016
00441          GO TO ABEND-PGM.                                         ECS016
00442                                                                   ECS016
00443      MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY.           ECS016
00444      MOVE DTE-CLASIC-COMPANY-CD  TO AM-CONTROL-PRIMARY.           ECS016
00445      START ERACCTT KEY NOT LESS AM-CONTROL-PRIMARY.               ECS016
00446                                                                   ECS016
00447      IF ERACCTT-FILE-STATUS NOT = ZERO                            ECS016
00448          MOVE ERACCTT-FILE-STATUS                                 ECS016
00449                                  TO WS-ABEND-FILE-STATUS          ECS016
00450          MOVE ' ERACCTT START ERROR- '                            ECS016
00451                                  TO WS-ABEND-MESSAGE              ECS016
00452          GO TO ABEND-PGM.                                         ECS016
00453                                                                   ECS016
00454      PERFORM 1000-MERGE-INPUTS THRU 1099-EXIT                     ECS016
00455          UNTIL END-OF-ALL-FILES.                                  ECS016
00456                                                                   ECS016
00457  END-RUN.                                                         ECS016
00458      CLOSE EPEC-IN                                                ECS016
00459            ERACCTT                                                ECS016
00460            EPEC-OUT                                               ECS016
00461            ACCT-OUT.                                              ECS016
00462                                                                   ECS016
00463  0699-EXIT.                                                       ECS016
00464      EXIT.                                                        ECS016
00465  EJECT                                                            ECS016
00466                                                                   ECS016
00467  1000-MERGE-INPUTS.                                               ECS016
00468      IF EXTR-NEEDED                                               ECS016
00469          PERFORM 9200-RETURN-SORTED-EXTRACT THRU 9299-EXIT        ECS016
00470          MOVE 'N'                TO EXTR-NEEDED-SW.               ECS016
00471                                                                   ECS016
00472      IF EPEC-NEEDED                                               ECS016
00473          PERFORM 9300-READ-EPEC-IN THRU 9399-EXIT                 ECS016
00474          MOVE 'N'                TO EPEC-NEEDED-SW.               ECS016
00475                                                                   ECS016
00476      IF ACCT-NEEDED                                               ECS016
00477          PERFORM 9400-READ-ACCT-IN THRU 9499-EXIT                 ECS016
00478          MOVE 'N'                TO ACCT-NEEDED-SW.               ECS016
00479                                                                   ECS016
00480      IF END-OF-ALL-FILES                                          ECS016
00481          GO TO 1099-EXIT.                                         ECS016
00482                                                                   ECS016
00483      PERFORM 2000-MATCH-EXTR-ACCT THRU 2099-EXIT.                 ECS016
00484      PERFORM 2500-MATCH-EPEC-ACCT THRU 2599-EXIT.                 ECS016
00485                                                                   ECS016
00486      IF (EXTR-ACCT-EQUAL) AND (EPEC-ACCT-EQUAL)                   ECS016
00487          IF WORK-EXTR-CONTROL LESS WORK-EPEC-CONTROL-2            ECS016
00488              MOVE 'N'            TO EPEC-ACCT-SW                  ECS016
00489          ELSE                                                     ECS016
00490              MOVE 'N'            TO EXTR-ACCT-SW.                 ECS016
00491                                                                   ECS016
00492      IF EXTR-ACCT-EQUAL                                           ECS016
00493          MOVE SORT-WORK-REC      TO EP-RECORD                     ECS016
00494          MOVE WORK-EXTR-CONTROL  TO CURR-EPEC-OUT                 ECS016
00495          MOVE 'Y'                TO EXTR-NEEDED-SW                ECS016
00496          PERFORM 3000-SAVE-HI-LOW-CERT-DATES THRU 3099-EXIT       ECS016
00497          PERFORM 9600-WRITE-NEW-EPEC         THRU 9699-EXIT.      ECS016
00498                                                                   ECS016
00499      IF EPEC-ACCT-EQUAL                                           ECS016
00500          MOVE EPEC-IN-REC         TO EP-RECORD                    ECS016
00501          MOVE WORK-EPEC-CONTROL-2 TO CURR-EPEC-OUT                ECS016
00502          MOVE 'Y'                TO EPEC-NEEDED-SW                ECS016
00503                                     EPEC-RECORD-SW                ECS016
00504          PERFORM 3000-SAVE-HI-LOW-CERT-DATES THRU 3099-EXIT       ECS016
00505          MOVE 'N'                TO EPEC-RECORD-SW                ECS016
00506          PERFORM 4000-TEST-FOR-PURGE     THRU 4099-EXIT           ECS016
00507          IF NOT PURGE-THIS-RECORD                                 ECS016
00508              PERFORM 9600-WRITE-NEW-EPEC THRU 9699-EXIT           ECS016
00509          ELSE                                                     ECS016
00510              ADD 1 TO EPEC-PURGED-COUNT.                          ECS016
00511                                                                   ECS016
00512      IF (NOT EXTR-ACCT-EQUAL) AND (NOT EPEC-ACCT-EQUAL)           ECS016
00513          PERFORM 9800-WRITE-NEW-ACCT THRU 9899-EXIT               ECS016
00514          MOVE 'Y'                TO ACCT-NEEDED-SW.               ECS016
00515                                                                   ECS016
00516      MOVE 'N'                    TO EXTR-ACCT-SW                  ECS016
00517                                     EPEC-ACCT-SW  PURGE-SW.       ECS016
00518                                                                   ECS016
00519  1099-EXIT.                                                       ECS016
00520      EXIT.                                                        ECS016
00521  EJECT                                                            ECS016
00522  2000-MATCH-EXTR-ACCT.                                            ECS016
00523      IF WEX-CONTROL-A = WORK-ACCOUNT-CONTROL-1                    ECS016
00524          MOVE 'Y'                TO EXTR-ACCT-SW                  ECS016
00525          GO TO 2099-EXIT.                                         ECS016
00526                                                                   ECS016
00527      IF WEX-CONTROL-A LESS WORK-ACCOUNT-CONTROL-1                 ECS016
00528          MOVE WEX-EXPIRATION     TO DISPLAY1-DATE                    CL*11
00529          MOVE WEX-EFFECTIVE      TO DISPLAY2-DATE                    CL*11
00530          DISPLAY 'NO ACCT MSTR FOR EXTR = '                          CL*12
00531                    WEX-CONTROL-DISPLAY                               CL*14
00532          ' ' DIS1-CC DIS1-YR ' ' DIS1-MO ' ' DIS1-DA                 CL*11
00533          ' ' DIS2-CC DIS2-YR ' ' DIS2-MO ' ' DIS2-DA                 CL*11
00534          MOVE WAC1-EXPIRATION    TO DISPLAY1-DATE                    CL*11
00535          MOVE WAC1-EFFECTIVE     TO DISPLAY2-DATE                    CL*11
00536          DISPLAY 'WORK ACCOUNT CONTROL 1 = '                         CL*11
00537              WORK-ACCOUNT-CONTROL-1-DISPLAY                          CL*11
00538          ' ' DIS1-CC DIS1-YR ' ' DIS1-MO ' ' DIS1-DA                 CL*11
00539          ' ' DIS2-CC DIS2-YR ' ' DIS2-MO ' ' DIS2-DA                 CL*11
00540          MOVE 'NO ACCT MSTR FOR EXTR'                             ECS016
00541                                  TO WS-ABEND-MESSAGE              ECS016
00542          MOVE ER-0302            TO WS-ABEND-CODE                 ECS016
CIDMOD                                                                  00005599
CIDMOD         ADD  +1  TO  ERROR-COUNT                                 00005600
CIDMOD         MOVE 'NO ACCT MSTR FOR EXTR = ' TO DIS-LINE-REASON       00005601
CIDMOD         MOVE WEX-CONTROL-DISPLAY    TO DIS-LINE-CONTROL          00005641
CIDMOD         MOVE WEX-EXPIRATION         TO DIS-LINE-EXPIRATION       00005641
CIDMOD         MOVE WEX-EFFECTIVE          TO DIS-LINE-EFFECTIVE        00005641
CIDMOD         MOVE WEX-REIN-COMP          TO DIS-LINE-REIN-COMP        00005641
CIDMOD         MOVE WEX-RUN-DATE           TO DIS-LINE-RUN-DATE         00005641
CIDMOD         MOVE WEX-LIFE-AH            TO DIS-LINE-LIFE-AH          00005641
CIDMOD         MOVE WEX-BEN-TYPE           TO DIS-LINE-BEN-TYPE         00005641
CIDMOD         MOVE WEX-PURGE-CODE         TO DIS-LINE-PURGE-CODE       00005641
CIDMOD         MOVE WEX-REC-TYPE           TO DIS-LINE-REC-TYPE         00005641
CIDMOD         MOVE WEX-COMM-SEQ-NO        TO DIS-LINE-COMM-SEQ-NO      00005641
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005603
CIDMOD               8600-DISPLAY-EXIT                                  00005604
CIDMOD         MOVE 'ACCOUNT MASTER KEY IS = ' TO DIS-LINE-REASON       00005605
CIDMOD         MOVE WORK-ACCOUNT-CONTROL-1-DISPLAY                      00005645
CIDMOD                                     TO DIS-LINE-CONTROL          00005645
CIDMOD         MOVE WAC1-EXPIRATION        TO DIS-LINE-EXPIRATION       00005641
CIDMOD         MOVE WAC1-EFFECTIVE         TO DIS-LINE-EFFECTIVE        00005641
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005607
CIDMOD               8600-DISPLAY-EXIT                                  00005608
CIDMOD         MOVE SPACES TO DISPLAY-LINE                              00005609
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005610
CIDMOD               8600-DISPLAY-EXIT                                  00005611
CIDMOD                                                                  00005612
CIDMOD         PERFORM 9200-RETURN-SORTED-EXTRACT THRU 9299-EXIT        00005613
CIDMOD         MOVE 'N' TO EXTR-NEEDED-SW                               00005614
040317*        GO TO 2000-MATCH-EXTR-ACCT.                              00005615
040317         GO TO ABEND-PGM.                                         ECS016
00544                                                                   ECS016
00545  2099-EXIT.                                                       ECS016
00546      EXIT.                                                        ECS016
00547      EJECT                                                        ECS016
00548  2500-MATCH-EPEC-ACCT.                                            ECS016
00549      IF WEP-CONTROL-A = WORK-ACCOUNT-CONTROL-2                    ECS016
00550          MOVE 'Y'                TO EPEC-ACCT-SW                  ECS016
00551          MOVE WAC1-EXPIRATION    TO EIR-EXPIRATION                ECS016
00552                                     WEP2-EXPIRATION               ECS016
00553          MOVE WAC1-EFFECTIVE     TO EIR-EFFECTIVE                 ECS016
00554                                     WEP2-EFFECTIVE                ECS016
00555          GO TO 2599-EXIT.                                         ECS016
00556                                                                   ECS016
00557      IF WEP-CONTROL-A LESS WORK-ACCOUNT-CONTROL-2                 ECS016
00558          MOVE WEP-EXPIRATION     TO DISPLAY1-DATE                    CL*11
00559          MOVE WEP-EFFECTIVE      TO DISPLAY2-DATE                    CL*11
00560          DISPLAY 'NO ACCT MSTR FOR EPEC  = '                         CL*11
00561                    WEP-CONTROL-DISPLAY                               CL*15
00562          ' ' DIS1-CC DIS1-YR ' ' DIS1-MO ' ' DIS1-DA                 CL*11
00563          ' ' DIS2-CC DIS2-YR ' ' DIS2-MO ' ' DIS2-DA                 CL*11
00564          MOVE WAC2-EXPIRATION    TO DISPLAY1-DATE                    CL*11
00565          MOVE WAC2-EFFECTIVE     TO DISPLAY2-DATE                    CL*11
00566          DISPLAY 'WORK ACCOUNT CONTROL 2 = '                      ECS016
00567              WORK-ACCOUNT-CONTROL-2-DISPLAY                          CL*16
00568          ' ' DIS1-CC DIS1-YR ' ' DIS1-MO ' ' DIS1-DA                 CL*11
00569          ' ' DIS2-CC DIS2-YR ' ' DIS2-MO ' ' DIS2-DA                 CL*11
00570          MOVE 'NO ACCT MSTR FOR EPEC'                             ECS016
00571                                  TO WS-ABEND-MESSAGE              ECS016
00572          MOVE ER-0302            TO WS-ABEND-CODE                 ECS016
CIDMOD                                                                  00005637
CIDMOD         WRITE  EPEC-OUT-REC FROM SAVE-EPEC                       00005638
CIDMOD         ADD  +1  TO  ERROR-COUNT                                 00005639
CIDMOD         MOVE 'NO ACCT MSTR FOR EPEC = ' TO DIS-LINE-REASON       00005640
CIDMOD         MOVE WEP-CONTROL-DISPLAY    TO DIS-LINE-CONTROL          00005641
CIDMOD         MOVE WEP-EXPIRATION         TO DIS-LINE-EXPIRATION       00005641
CIDMOD         MOVE WEP-EFFECTIVE          TO DIS-LINE-EFFECTIVE        00005641
CIDMOD         MOVE WEP-REIN-COMP          TO DIS-LINE-REIN-COMP        00005641
CIDMOD         MOVE WEP-RUN-DATE           TO DIS-LINE-RUN-DATE         00005641
CIDMOD         MOVE WEP-LIFE-AH            TO DIS-LINE-LIFE-AH          00005641
CIDMOD         MOVE WEP-BEN-TYPE           TO DIS-LINE-BEN-TYPE         00005641
CIDMOD         MOVE WEP-PURGE-CODE         TO DIS-LINE-PURGE-CODE       00005641
CIDMOD         MOVE WEP-REC-TYPE           TO DIS-LINE-REC-TYPE         00005641
CIDMOD         MOVE WEP-COMM-SEQ-NO        TO DIS-LINE-COMM-SEQ-NO      00005641
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005642
CIDMOD               8600-DISPLAY-EXIT                                  00005643
CIDMOD         MOVE 'ACCOUNT MASTER KEY IS = ' TO DIS-LINE-REASON       00005644
CIDMOD         MOVE WORK-ACCOUNT-CONTROL-2-DISPLAY                      00005645
CIDMOD                                     TO DIS-LINE-CONTROL          00005645
CIDMOD         MOVE WAC2-EXPIRATION        TO DIS-LINE-EXPIRATION       00005641
CIDMOD         MOVE WAC2-EFFECTIVE         TO DIS-LINE-EFFECTIVE        00005641
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005646
CIDMOD               8600-DISPLAY-EXIT                                  00005647
CIDMOD                                                                  00005648
CIDMOD         MOVE                                                     00005649
CIDMOD          ' KEY OF UN-MATCHED EPEC JUST RE-WRITTEN IS '           00005650
CIDMOD             TO DIS-FLD-1                                         00005651
CIDMOD         MOVE SPACES     TO  DIS-FLD-2                            00005652
CIDMOD         MOVE SAVE-EP-REC-TYPE    TO DIS-FLD2-REC-TYPE            00005652
CIDMOD         MOVE SAVE-EP-CCSA        TO DIS-FLD2-CCSA                00005652
CIDMOD         MOVE SAVE-EP-EXPIRATION  TO DIS-FLD2-EXPIRATION          00005652
CIDMOD         MOVE SAVE-EP-EFFECTIVE   TO DIS-FLD2-EFFECTIVE           00005652
CIDMOD         MOVE SAVE-EP-REIN-COMP   TO DIS-FLD2-REIN-COMP           00005652
CIDMOD         MOVE SAVE-EP-LIFE-AH     TO DIS-FLD2-LIFE-AH             00005652
CIDMOD         MOVE SAVE-EP-BEN-TYPE    TO DIS-FLD2-BEN-TYPE            00005652
CIDMOD         MOVE SAVE-EP-COMM-SEQ-NO TO DIS-FLD2-COMM-SEQ-NO         00005652
CIDMOD         DISPLAY                                                  00005658
CIDMOD          ' KEY OF UN-MATCHED EPEC JUST RE-WRITTEN IS '           00005659
CIDMOD                DIS-FLD-2                                         00005660
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005653
CIDMOD               8600-DISPLAY-EXIT                                  00005654
CIDMOD         MOVE  SPACES TO DISPLAY-LINE                             00005655
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            00005656
CIDMOD               8600-DISPLAY-EXIT                                  00005657
CIDMOD                                                                  00005661
CIDMOD         PERFORM 9300-READ-EPEC-IN THRU 9399-EXIT                 00005662
CIDMOD         MOVE 'N' TO EPEC-NEEDED-SW                               00005663
040317*        GO TO 2500-MATCH-EPEC-ACCT.                              00005664
040317         GO TO ABEND-PGM.                                         ECS016
00574                                                                   ECS016
00575  2599-EXIT.                                                       ECS016
00576      EXIT.                                                        ECS016
00577  EJECT                                                            ECS016
00578  3000-SAVE-HI-LOW-CERT-DATES.                                     ECS016
00579      IF EP-RECORD-ID NOT = 'EP'                                   ECS016
00580          GO TO 3099-EXIT.                                         ECS016
00581 *                                                                 ECS016
00582 *    IF EP-REIN = 'R'                                             ECS016
00583 *        GO TO 3099-EXIT.                                         ECS016
00584                                                                   ECS016
00585      IF FROM-EPEC-FILE                                            ECS016
00586          IF EP-PURGE NOT = 'P'                                    ECS016
00587              GO TO 3099-EXIT.                                     ECS016
00588                                                                   ECS016
00589                                                                   ECS016
00590      IF EP-LO-CERT NOT NUMERIC OR EP-LO-CERT = ZERO                  CL*18
00591          GO TO 3099-EXIT.                                         ECS016
00592                                                                   ECS016
00593      IF EP-HI-CERT NOT NUMERIC OR EP-HI-CERT = ZERO                  CL*18
00594          GO TO 3099-EXIT.                                         ECS016
00595                                                                   ECS016
00596      IF EP-LO-CERT LESS THAN SAVE-LOW-CERT                           CL**6
00597          MOVE EP-LO-CERT         TO SAVE-LOW-CERT.                ECS016
00598                                                                   ECS016
00599      IF EP-HI-CERT GREATER THAN SAVE-HIGH-CERT                       CL**6
00600          MOVE EP-HI-CERT         TO SAVE-HIGH-CERT.               ECS016
00601                                                                   ECS016
00602  3099-EXIT.                                                       ECS016
00603      EXIT.                                                        ECS016
00604  EJECT                                                            ECS016
00605  4000-TEST-FOR-PURGE.                                             ECS016
00606      IF EP-PURGE = 'P'                                            ECS016
00607          GO TO 4099-EXIT.                                         ECS016
00608                                                                   ECS016
00609      IF VALID-EP-ID                                               ECS016
00610          IF +0 NOT = EP-RETRO-EXPENSES OR EP-RETRO-PAYMENTS       ECS016
00611                   OR EP-RETRO-OTH-COMM OR EP-ADJUST               ECS016
040512                  OR EP-CLAIM-ADJ
00612                      GO TO 4099-EXIT.                             ECS016
00613                                                                   ECS016
00614      MOVE EP-RUN-DTE             TO WS-EP-RUN-DTE-N.                 CL*18

00616      IF EP-RUN-MO = 12    AND                                     ECS016
00617         EP-RUN-CCYY GREATER PURGE-DEC                             ECS016
00618          GO TO 4099-EXIT.                                         ECS016
00619                                                                   ECS016
00620      IF EP-RUN-DTE GREATER PURGE-DATE                             ECS016
00621          GO TO 4099-EXIT.                                         ECS016
00622                                                                   ECS016
00623      MOVE 'Y'                    TO PURGE-SW.                     ECS016
00624                                                                   ECS016
00625  4099-EXIT.                                                       ECS016
00626      EXIT.                                                        ECS016
00627  EJECT                                                            ECS016
00628  8000-PRINT-HEADINGS.                                             ECS016
00629      MOVE '1'                    TO X.                            ECS016
00630      MOVE HEADING-1              TO P-DATA.                       ECS016
00631      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00632      MOVE ' '                    TO X.                            ECS016
00633      MOVE HEADING-2              TO P-DATA.                       ECS016
00634      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00635      MOVE 1                      TO H3-PAGE-NO.                   ECS016
00636      MOVE HEADING-3              TO P-DATA.                       ECS016
00637      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00638      MOVE SPACES                 TO P-DATA.                       ECS016
00639      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00640                                                                   ECS016
00641  8099-EXIT.                                                       ECS016
00642      EXIT.                                                        ECS016
00643                                                                   ECS016
00644  8300-PRINT-CONTROL-TOTALS.                                       ECS016

070714     if me-do-update
070714        move epec-in-count       to hld-016-recs-in
070714        move epec-out-count      to hld-016-recs-out
070714     end-if

00645      MOVE SPACES                 TO DETAIL-LINE.                  ECS016
00646      MOVE ' '                    TO X.                            ECS016
00647      MOVE 'EPEC RECORDS READ '   TO DL-MSG.                       ECS016
00648      MOVE EPEC-IN-COUNT          TO DL-COUNT.                     ECS016
00649      MOVE DETAIL-LINE            TO P-DATA.                       ECS016
00650      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00651      MOVE 'EXTR RECORDS ADDED '  TO DL-MSG.                       ECS016
00652      MOVE EXTR-ADDED-COUNT       TO DL-COUNT.                     ECS016
00653      MOVE DETAIL-LINE            TO P-DATA.                       ECS016
00654      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00655      MOVE 'EPEC RECORDS PURGED ' TO DL-MSG.                       ECS016
00656      MOVE EPEC-PURGED-COUNT      TO DL-COUNT.                     ECS016
00657      MOVE DETAIL-LINE            TO P-DATA.                       ECS016
00658      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00659      MOVE 'EPEC RECORDS WRITTEN ' TO DL-MSG.                      ECS016
00660      MOVE EPEC-OUT-COUNT         TO DL-COUNT.                     ECS016
00661      MOVE DETAIL-LINE            TO P-DATA.                       ECS016
00662      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00663      MOVE 'ACCT ITEMS READ      ' TO DL-MSG.                      ECS016
00664      MOVE ACCT-READ-COUNT        TO DL-COUNT.                     ECS016
00665      MOVE DETAIL-LINE            TO P-DATA.                       ECS016
00666      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00667      MOVE 'ACCT ITEMS WRITTEN   ' TO DL-MSG.                      ECS016
00668      MOVE ACCT-WRITE-COUNT       TO DL-COUNT.                     ECS016
00669      MOVE DETAIL-LINE            TO P-DATA.                       ECS016
00670      PERFORM 8500-PRINT-ROUTINE THRU 8599-EXIT.                   ECS016
00671                                                                   ECS016
00672  8399-EXIT.                                                       ECS016
00673      EXIT.                                                        ECS016
00674                                                                   ECS016
00675  8500-PRINT-ROUTINE.                                              ECS016
00676                              COPY ELCPRT2.                        ECS016
00677                                                                   ECS016
00678  8599-EXIT.                                                       ECS016
00679      EXIT.                                                        ECS016
CIDMOD                                                                  00007511
CIDMOD 8600-DISPLAY-PRT.                                                00007512
CIDMOD                                                                  00007513
CIDMOD     IF  DIS-HEAD-SW =  'Y'                                       00007514
CIDMOD       MOVE 'N' TO  DIS-HEAD-SW                                   00007515
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             00007516
CIDMOD             8600-HD-EXIT                                         00007517
CIDMOD           GO TO 8600-DISPLAY-EXIT.                               00007518
CIDMOD                                                                  00007519
CIDMOD     IF  DIS-LINE-CNT GREATER THAN 59                             00007520
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             00007521
CIDMOD             8600-HD-EXIT.                                        00007522
CIDMOD                                                                  00007523
CIDMOD     MOVE   SPACES TO DIS-CC.                                     00007524
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      00007525
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-LINE.                        00007526
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                00007527
CIDMOD     MOVE   SPACES TO DISPLAY-LINE.                               00007528
CIDMOD                                                                  00007529
CIDMOD                                                                  00007530
CIDMOD 8600-DISPLAY-EXIT.                                               00007531
CIDMOD     EXIT.                                                        00007532
CIDMOD                                                                  00007740
CIDMOD 8600-DISPLAY-HD.                                                 00007741
CIDMOD                                                                  00007742
071003*    MOVE '1' TO  DISPLAY-CC.                                     00007743
CIDMOD     MOVE ZEROS TO DIS-LINE-CNT.                                  00007744
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-HD-1.                         00007745
CIDMOD     ADD  +1  TO DIS-LINE-CNT.                                    00007746
CIDMOD     MOVE ' ' TO  DISPLAY-CC.                                     00007747
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                00007748
CIDMOD*    WRITE  DISPLAY-REC.                                          00007749
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      00007750
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-HD-2.                        00007751
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      00007752
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                00007753
CIDMOD     WRITE  DISPLAY-REC.                                          00007754
CIDMOD                                                                  00007755
CIDMOD 8600-HD-EXIT.                                                    00007902
CIDMOD     EXIT.                                                        00007903
CIDMOD                                                                  00007900
00680  EJECT                                                            ECS016
00681  9000-READ-EXTR.                                                  ECS016
00682      READ EXTR-IN                                                 ECS016
00683          AT END  MOVE 'Y' TO EXTR-END-SW                          ECS016
00684          GO TO 9099-EXIT.                                         ECS016
00685                                                                   ECS016
CIDMOD     MOVE WORK-RUN-DATE                                           00007991
CIDMOD          TO EXTR-REC-RUN-DATE.                                   00007992
CIDMOD                                                                  00007993
00686      IF EXTR-REC-RUN-DATE NOT = WORK-RUN-DATE                     ECS016
00687          DISPLAY 'EXTR DATE NOT = RUN DATE '                      ECS016
00688          DISPLAY 'RUN DATE = ' RUN-DATE                              CL*17
00689          DISPLAY 'EXTR DATE = ' EXTR-REC-RUN-DATE                 ECS016
00690          MOVE 'EXTR DATE NOT = RUN DATE'                          ECS016
00691                                  TO WS-ABEND-MESSAGE              ECS016
00692          MOVE ER-0998            TO WS-ABEND-CODE                 ECS016
00693          GO TO ABEND-PGM.                                         ECS016
00694                                                                   ECS016
00695  9099-EXIT.                                                       ECS016
00696      EXIT.                                                        ECS016
00697                                                                   ECS016
00698  EJECT                                                            ECS016
00699  9200-RETURN-SORTED-EXTRACT.                                      ECS016
00700      RETURN SORT-WORK                                             ECS016
00701          AT END  MOVE 'Y' TO EXTR-END-SW                          ECS016
00702          MOVE HIGH-VALUES TO WORK-EXTR-CONTROL                    ECS016
00703          GO TO 9299-EXIT.                                         ECS016
00704                                                                   ECS016
00705      MOVE SW-CARR                TO  WEX-CARR.                    ECS016
00706      MOVE SW-GROUPING            TO  WEX-COMP.                    ECS016
00707      MOVE SW-STATE               TO  WEX-STATE.                   ECS016
00708      MOVE SW-ACCOUNT             TO  WEX-ACCOUNT.                 ECS016
00709      MOVE SW-EXPIRATION          TO  WEX-EXPIRATION.              ECS016
00710      MOVE SW-EFFECTIVE           TO  WEX-EFFECTIVE.               ECS016
00711      MOVE SW-REIN-COMP           TO  WEX-REIN-COMP.               ECS016
00712      MOVE SW-RUN-DATE            TO  WEX-RUN-DATE.                ECS016
00713      MOVE SW-RCD-TYPE            TO  WEX-LIFE-AH.                 ECS016
00714      MOVE SW-BEN-TYPE            TO  WEX-BEN-TYPE.                ECS016
00715      MOVE SW-PURGE-CODE          TO  WEX-PURGE-CODE.              ECS016
00716      MOVE SW-REC-TYPE            TO  WEX-REC-TYPE.                ECS016
00717      MOVE SW-COMM-SEQ-NO         TO  WEX-COMM-SEQ-NO.             ECS016
00718                                                                   ECS016
CIDMOD     MOVE EPEC-IN-REC            TO SAVE-EPEC.                    00008065
CIDMOD                                                                  00008066
00719  9299-EXIT.                                                       ECS016
00720      EXIT.                                                        ECS016
00721                                                                   ECS016
00722  EJECT                                                            ECS016
00723  9300-READ-EPEC-IN.                                               ECS016
00724      READ EPEC-IN                                                 ECS016
00725          AT END  MOVE 'Y' TO EPEC-END-SW                          ECS016
00726                  MOVE HIGH-VALUES  TO WORK-EPEC-CONTROL           ECS016
00727                                       WORK-EPEC-CONTROL-2         ECS016
00728                  GO TO 9399-EXIT.                                 ECS016
00729                                                                   ECS016
00730      IF EIR-EXPIRATION = 00099999999                                 CL*19
00731          MOVE 99999999999        TO EIR-EXPIRATION.                  CL*19
00732                                                                      CL*19
00733      ADD 1 TO EPEC-IN-COUNT.                                      ECS016
00734      MOVE EIR-CARRIER            TO  WEP-CARR.                    ECS016
00735      MOVE EIR-GROUPING           TO  WEP-COMP.                    ECS016
uktdel*    MOVE EIR-STATE              TO  WEP STATE.                   ECS016
uktins     MOVE EIR-STATE              TO  WEP-STATE.                   ECS016
00737      MOVE EIR-ACCOUNT            TO  WEP-ACCOUNT.                 ECS016
00738      MOVE EIR-EXPIRATION         TO  WEP-EXPIRATION.              ECS016
00739      MOVE EIR-EFFECTIVE          TO  WEP-EFFECTIVE.               ECS016
00740      MOVE EIR-REIN-COMP          TO  WEP-REIN-COMP.               ECS016
00741      MOVE EIR-RUN-DATE           TO  WEP-RUN-DATE.                ECS016
00742      MOVE EIR-LIFE-AH            TO  WEP-LIFE-AH.                 ECS016
00743      MOVE EIR-BEN-TYPE           TO  WEP-BEN-TYPE.                ECS016
00744      MOVE EIR-PURGE-CODE         TO  WEP-PURGE-CODE.              ECS016
00745      MOVE EIR-REC-TYPE           TO  WEP-REC-TYPE.                ECS016
00746      MOVE EIR-COMM-SEQ-NO        TO  WEP-COMM-SEQ-NO.             ECS016
00747                                                                   ECS016
00748      IF WORK-EPEC-CONTROL LESS PREV-EPEC-IN                       ECS016
00749          DISPLAY 'OUT OF SEQUENCE ON INPUT EPEC '                 ECS016
00750          DISPLAY 'PREV CONTROL ' PREV-EPEC-IN                     ECS016
00751          DISPLAY 'CURR CONTROL ' WORK-EPEC-CONTROL                ECS016
00752          MOVE 'OUT OF SEQUENCE ON INPUT EPEC'                     ECS016
00753                                  TO WS-ABEND-MESSAGE              ECS016
00754          MOVE ER-0610            TO WS-ABEND-CODE                 ECS016
00755          GO TO ABEND-PGM.                                         ECS016
00756                                                                   ECS016
00757      MOVE WORK-EPEC-CONTROL      TO PREV-EPEC-IN.                 ECS016
00758      MOVE WORK-EPEC-CONTROL      TO WORK-EPEC-CONTROL-2.          ECS016
00759                                                                   ECS016
00760  9399-EXIT.                                                       ECS016
00761      EXIT.                                                        ECS016
00762                                                                   ECS016
00763  EJECT                                                            ECS016
00764  9400-READ-ACCT-IN.                                               ECS016
00765      READ ERACCTT NEXT RECORD.                                    ECS016
00766                                                                   ECS016
00767      IF ERACCTT-FILE-STATUS =  '10'                               ECS016
00768          MOVE HIGH-VALUES        TO WORK-ACCOUNT-CONTROL-1        ECS016
00769                                     WORK-ACCOUNT-CONTROL-2        ECS016
00770          MOVE 'Y'                TO ACCT-END-SW                   ECS016
00771          GO TO 9499-EXIT.                                         ECS016
00772                                                                   ECS016
00773      IF ERACCTT-FILE-STATUS NOT = ZERO                            ECS016
00774          MOVE ERACCTT-FILE-STATUS                                 ECS016
00775                                  TO WS-ABEND-FILE-STATUS          ECS016
00776          MOVE ' ERACCTT READ ERROR- '                             ECS016
00777                                  TO WS-ABEND-MESSAGE              ECS016
00778          GO TO ABEND-PGM.                                         ECS016
00779                                                                   ECS016
00780      IF AM-1ST-PROD-DATE NOT NUMERIC                              ECS016
00781          MOVE ZEROS              TO AM-1ST-PROD-DATE.             ECS016
00782      IF AM-LO-CERT-DATE NOT NUMERIC                               ECS016
00783          MOVE ZEROS              TO AM-LO-CERT-DATE.              ECS016
00784      IF AM-HI-CERT-DATE NOT NUMERIC                               ECS016
00785          MOVE ZEROS              TO AM-HI-CERT-DATE.              ECS016
00786                                                                   ECS016
00787 *****COPY ELCACCTI.                                                  CL*17
00788      MOVE AM-CARRIER             TO  WAC1-CARR.                   ECS016
00789      MOVE AM-GROUPING            TO  WAC1-COMP.                   ECS016
00790      MOVE AM-STATE               TO  WAC1-STATE.                  ECS016
00791      MOVE AM-ACCOUNT             TO  WAC1-ACCOUNT.                ECS016
00792      MOVE AM-EXPIRE-DT           TO  WAC1-EXPIRATION.             ECS016
00793      MOVE AM-EFFECT-DT           TO  WAC1-EFFECTIVE.              ECS016
00794      MOVE WORK-ACCOUNT-CONTROL-1 TO WORK-ACCOUNT-CONTROL-2.       ECS016
00795                                                                   ECS016
00796      IF AM-PREV-DATES = SPACES  OR  ZEROS                            CL*17
00797          GO TO 9499-ADD.                                          ECS016
00798                                                                   ECS016
00799      IF AM-PREV-EXP-DT = AM-EXPIRE-DT  AND                        ECS016
00800         AM-PREV-EFF-DT = AM-EFFECT-DT                             ECS016
00801            GO TO 9499-ADD.                                        ECS016
00802                                                                   ECS016
00803       MOVE AM-PREV-EXP-DT     TO WAC2-EXPIRATION.                 ECS016
00804       MOVE AM-PREV-EFF-DT     TO WAC2-EFFECTIVE.                  ECS016
00805       MOVE 'Y'                TO ACCT-DATE-CHG.                   ECS016
00806                                                                   ECS016
00807  9499-ADD.                                                        ECS016
00808      ADD 1 TO ACCT-READ-COUNT.                                    ECS016
00809                                                                   ECS016
00810  9499-EXIT.                                                       ECS016
00811      EXIT.                                                        ECS016
00812                                                                   ECS016
00813  EJECT                                                            ECS016
00814  9600-WRITE-NEW-EPEC.                                             ECS016
00815      IF CURR-EPEC-OUT LESS PREV-EPEC-OUT                          ECS016
00816          DISPLAY 'OUT OF SEQUENCE ON OUTPUT EPEC '                ECS016
00817          DISPLAY 'PREV CONTROL ' PREV-EPEC-OUT                    ECS016
00818          DISPLAY 'CURR CONTROL ' CURR-EPEC-OUT                    ECS016
00819          MOVE 'OUT OF SEQUENCE ON OUTPUT EPEC'                    ECS016
00820                                  TO WS-ABEND-MESSAGE              ECS016
00821          MOVE ER-0612            TO WS-ABEND-CODE                 ECS016
00822          GO TO ABEND-PGM.                                         ECS016
00823                                                                   ECS016
00824      MOVE CURR-EPEC-OUT          TO PREV-EPEC-OUT.                ECS016
00825 *****COPY ELCEPCM2.                                                  CL*17
00826      WRITE EPEC-OUT-REC FROM EP-RECORD                            ECS016
00827      ADD 1 TO EPEC-OUT-COUNT.                                     ECS016
00828                                                                   ECS016
00829  9699-EXIT.                                                       ECS016
00830      EXIT.                                                        ECS016
00831                                                                   ECS016
00832  EJECT                                                            ECS016
00833  9800-WRITE-NEW-ACCT.                                             ECS016
00834                                                                   ECS016
00835      MOVE AM-EXPIRE-DT           TO AM-PREV-EXP-DT.                  CL*17
00836      MOVE AM-EFFECT-DT           TO AM-PREV-EFF-DT.                  CL*17
00837                                                                   ECS016
00838      PERFORM 9830-ZERO VARYING AINDEX FROM 1 BY 1 UNTIL           ECS016
00839            AINDEX = 11.                                           ECS016
00840                                                                   ECS016
00841      MOVE AM-COMM-STRUCTURE      TO AM-COMM-STRUCTURE-SAVED.      ECS016
00842      MOVE AM-REMIT-TO            TO AM-SAVED-REMIT-TO.            ECS016
00843                                                                   ECS016
00844      MOVE ' '                    TO AM-COMM-CHANGE-STATUS         ECS016
00845                                     AM-BILLING-STATUS             ECS016
00846                                     AM-RECALC-COMM                ECS016
00847                                     AM-RECALC-REIN.               ECS016
00848                                                                   ECS016
00849      IF SAVE-LOW-CERT NOT =  99999999999                             CL*17
00850          MOVE SAVE-LOW-CERT      TO AM-LO-CERT-DATE.                 CL*17
00851                                                                   ECS016
00852      IF SAVE-HIGH-CERT NOT = ZERO                                 ECS016
00853          MOVE SAVE-HIGH-CERT     TO AM-HI-CERT-DATE.                 CL*17
00854                                                                   ECS016
00855      IF AM-1ST-PROD-DATE = ZEROS                                  ECS016
00856          IF AM-LO-CERT-DATE NOT = ZEROS                           ECS016
00857              MOVE WRD-YR         TO AM-1ST-PROD-YR                   CL*17
00858              MOVE WRD-MO         TO AM-1ST-PROD-MO                   CL*17
00859              MOVE WRD-DA         TO AM-1ST-PROD-DA.                  CL*17
00860                                                                   ECS016
00861      MOVE LOW-VALUES             TO AM-AR-HI-CERT-DATE.           ECS016
00862                                                                   ECS016
00863 *****COPY ELCACCTO.                                                  CL*17
00864      WRITE ACCT-OUT-REC FROM ACCOUNT-MASTER.                      ECS016
00865                                                                   ECS016
00866      ADD 1 TO ACCT-WRITE-COUNT.                                   ECS016
00867                                                                   ECS016
00868      GO TO 9890-SET-NEXT.                                         ECS016
00869                                                                   ECS016
00870  9830-ZERO.                                                       ECS016
00871                                                                   ECS016
00872      MOVE SPACES                 TO AM-RECALC-LV-INDIC (AINDEX).  ECS016
00873                                                                   ECS016
00874  9890-SET-NEXT.                                                   ECS016
00875                                                                   ECS016
00876      MOVE 99999999999            TO SAVE-LOW-CERT.                   CL*17
00877      MOVE ZEROS                  TO SAVE-HIGH-CERT.               ECS016
00878                                                                   ECS016
00879  9899-EXIT.                                                       ECS016
00880      EXIT.                                                        ECS016
00881      EJECT                                                        ECS016
00882                                                                   ECS016
00883  ABEND-PGM SECTION.                                               ECS016
00884                              COPY ELCABEND.                       ECS016
00885                                                                      CL*17
