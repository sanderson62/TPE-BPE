00001  IDENTIFICATION DIVISION.                                         04/29/97
00002                                                                   EL175
00003  PROGRAM-ID.                 EL175 .                                 LV028
00004 *              PROGRAM CONVERTED BY                                  CL*17
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*17
00006 *              CONVERSION DATE 05/09/95 10:08:20.                    CL*17
00007 *                            VMOD=2.028                              CL*28
00008 *                                                                 EL175
00008 *                                                                 EL175
00009 *AUTHOR.    LOGIC, INC.                                              CL*17
00010 *           DALLAS, TEXAS.                                           CL*17

00024 *REMARKS.                                                            CL**2
00025 *        THIS PROGRAM IS USED TO INDICATE THE CHECKS BEING           CL**2
00026 *    RELEASED TO PRINT.  EACH RELEASE CONSTITUTES A CONTROL GROUP    CL**2
00027 *    THAT IS REFERENCED BY THE CHECK WRITER (EL177) AND THE CHECK    CL**2
00028 *    PRINT STARTER (EL176).                                          CL**2
00029                                                                   EL175
00030 *    SCREENS     - EL175A - CHECK RELEASE                            CL**2
00031                                                                   EL175
00032 *    ENTERED BY  - EL171  - REPORT MENU                              CL**2
00033                                                                   EL175
00034 *    EXIT TO     - EL171  - RESULT OF CLEAR                          CL**2
00035                                                                   EL175
00036 *    INPUT FILES - ELTRLR - PAYMENT TRAILERS                         CL**2
00037 *                  ELACTQ - ACTIVITY QUEUE                           CL**2
00038 *                  ELCNTL - CONTROL FILE                             CL**2
00039                                                                   EL175
00040 *    OUTPUT FILES - ELTRLR - PAYMENT TRAILERS                        CL**2
00041 *                   ELACTQ - ACTIVITY QUEUE                          CL**2
00042 *                   ELCNTL - CONTROL FILE                            CL**2
00043 *                   ELCHKQ - CHECK QUEUE                             CL**2
00044                                                                   EL175
00045 *    COMMAREA    - PASSED.                                           CL**2
00046                                                                   EL175
00048 *    NARRATIVE   - ON FIRST ENTRY, A SKELETON SCREEN IS SENT SO      CL**2
00049 *                  THE OPERATOR MAY ENTER THE QUALIFICATION DATA     CL**2
00050 *                  FOR THE BATCH.  WHEN THE OPERATOR COMPLETES THE   CL**2
00051 *                  DATA, THE PROGRAM IS RE-ENTERED TO SELECT THE     CL*12
00052 *                  PAYMENTS AND CREATE THE CONTROL BATCH.            CL**2
00053                                                                   EL175
00055 *                  THE PROCESSING STEPS INVOLVED IN CREATING         CL*12
00056 *                  THE BATCH IS DETAILED AS FOLLOWS:                 CL**2
00057                                                                   EL175
00058 *                  1.  THE COMPANY CONTROL RECORD IS READ FOR        CL**2
00059 *                      UPDATE.  A CHECK QUEUE SEQUENCE NUMBER IS     CL**2
00060 *                      ASSIGNED USING CF-CO-CHECK-QUE-COUNTER + 1.   CL**2
00061 *                      THE RECORD IS UPDATED WITH THE NEW NUMBER     CL**2
00062 *                      AND FILED.                                    CL**2
00063                                                                   EL175
00064 *                  2.  THE ACTIVITY QUE FILE IS READ SEQUENTIALLY    CL**2
00065 *                      TO FIND THE CLAIMS HAVING PENDING PAYMENTS.   CL**2
00066                                                                   EL175
00067 *                  3.  USING THE CONTROL FROM THE ACTIVITY QUE       CL**2
00068 *                      RECORD, THE PAYMENT ACTIVITY TRAILER IS       CL**2
00069 *                      READ SEQUENTIALLY LOOKING FOR PAYMENTS THAT   CL**2
00070 *                      MEET THE QUALIFICATIONS INPUT.                CL*12
00071                                                                   EL175
00072 *                      THE SEARCH OF THE TRAILER CHAIN WILL QUIT     CL**2
00073 *                      UPON ENCOUNTERING PAYMENT TRAILERS CREATED    CL**2
00074 *                      OVER 60 DAYS PRIOR TO THE CURRENT DATE.       CL**2
00075 *                      THIS IS DONE TO SHORTEN THE SEARCH TIME.      CL**2
00076 *                      THE CONSEQUENCE OF THIS IS THAT CHECKS MUST   CL**2
00077 *                      BE WRITTEN WITHIN 60 DAYS OF ISSUE.           CL**2
00078                                                                   EL175
00079 *                  4.  AS QUALIFYING PAYMENTS ARE FOUND, A CHECK     CL*12
00080 *                      RECORD  IS CREATED AND FILED.  THE SEQUENCE   CL**2
00081 *                      IN THIS RECORD (CQ-SEQUENCE-NO) IS A          CL**2
00082 *                      COUNTER KEPT BY THE PROGRAM OF ENTRIES        CL**2
00083 *                      SELECTED FOR PROCESSING.  THE SELECTED        CL**2
00084 *                      TRAILER IS UPDATED WITH THE QUE POINTER AND   CL**2
00085 *                      FILED BACK.                                   CL**2
00086                                                                   EL175
00087 *                  5.  THE ACTIVITY QUEUE RECORD IS UPDATED TO       CL**2
00088 *                      REFLECT THE PAYMENTS RELEASED.  IF ALL        CL**2
00089 *                      PAYMENTS ARE RELEASED FOR A CONTROL AND NO    CL**2
00090 *                      OTHER ACTIVITY IS PENDING, THE RECORD IS      CL**2
00091 *                      DELETED.  IF OTHER ACTIVITY IS QUED OR THE    CL*12
00092 *                      PAYMENT COUNT IS NOT ZERO, THE UPDATED        CL**2
00093 *                      ACTIVITY QUEUE RECORD IS FILED.               CL**2
00094                                                                   EL175
00095 *                  6.  WHEN THE RELEASE IS COMPLETE, A SKELETON IS   CL**2
00096 *                      RETURNED ALONG WITH AN END-OF-JOB MESSAGE.    CL**2
00097 *                      THE EOJ MESSAGE GIVES THE CONTROL NUMBER      CL**2
00098 *                      ASSIGNED AND THE NUMBER OF PAYMENTS IN THE    CL*12
00099 *                      BATCH.                                        CL*12
00100                                                                   EL175
00101 *                  AT COMPLETION, THE OPERATOR MAY ENTER A NEW       CL**2
00102 *                  QUALIFICATION OR PRESS CLEAR TO RETURN TO THE     CL**2
00103 *                  REPORT MENU (EL171).                              CL**2
111502******************************************************************
111502*                   C H A N G E   L O G
111502*
111502* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
111502*-----------------------------------------------------------------
111502*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
111502* EFFECTIVE    NUMBER
111502*-----------------------------------------------------------------
111502* 111502    2002111800003  SMVA  STOP EXECUTION OF EX64
121902* 121902    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYP I
121203* 121203    2003080800002  SMVA  ADD PROCESSING FOR NEW CLM TYP G
031808* 031808    2006032200004  AJRA  ADD MESSAGE GIVING COUNT AND TOTAL 
031808*                                AMOUNT FOR UNAPPROVED PAYMENTS
090309* 090309    2008070200001  AJRA  DO NOT ALLOW USERS TO RELEASE ALL
090309*                                CHECKS. RELEASE DONE DURING CYCLE.
092809* 092809    2009092500001  AJRA  FIX RELEASE WHEN FIRST CLAIM BLANK
080712* 080712  IR2012080300001  PEMA  CORRECT HANDLE CONDITION
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
111502******************************************************************
00104                                                                   EL175
00106  ENVIRONMENT DIVISION.                                            EL175
00107                                                                   EL175
00108  DATA DIVISION.                                                   EL175
00109                                                                   EL175
00110  WORKING-STORAGE SECTION.                                         EL175
00111                                                                   EL175
00112  77  FILLER  PIC X(32)  VALUE '********************************'. EL175
00113  77  FILLER  PIC X(32)  VALUE '*    EL175 WORKING STORAGE     *'. EL175
00114  77  FILLER  PIC X(32)  VALUE '********** VMOD=2.028 **********'.    CL*28
00115                                                                   EL175
00116                              COPY ELCSCTM.                           CL**7
00117                                                                   EL175
00118                              COPY ELCSCRTY.                          CL**7
00119                                                                      CL*17
00120                              COPY ELCNWA.                            CL*17
00121                                                                   EL175
00122      EJECT                                                           CL*14
00123  01  WS-DATE-AREA.                                                EL175
00124      12  SAVE-DATE           PIC X(8)    VALUE SPACES.            EL175
00125      12  SAVE-BIN-DATE       PIC X(2)    VALUE SPACES.            EL175
00126                                                                      CL*17
00127  01  DMD-DATE-YYYYMMDD.                                              CL*17
00128      12  DMD-DECADE          PIC XX      VALUE SPACES.               CL*17
00129      12  DMD-YYMMDD.                                                 CL*17
00130          16  DMD-YY          PIC XX      VALUE SPACES.               CL*17
00131          16  DMD-MM          PIC XX      VALUE SPACES.               CL*17
00132          16  DMD-DD          PIC XX      VALUE SPACES.               CL*17
00133                                                                      CL*17
00134  01  DMD-DATE-MMDDYYYY.                                              CL*17
00135      12  DMD-MDY-MM          PIC XX      VALUE SPACES.               CL*17
00136      12  DMD-MDY-SLASH1      PIC X       VALUE '/'.                  CL*17
00137      12  DMD-MDY-DD          PIC XX      VALUE SPACES.               CL*17
00138      12  DMD-MDY-SLASH2      PIC X       VALUE '/'.                  CL*17
00139      12  DMD-MDY-DECADE      PIC XX      VALUE SPACES.               CL*17
00140      12  DMD-MDY-YY          PIC XX      VALUE SPACES.               CL*17
00141                                                                      CL*17
00142  01  WS-MAIL-CODE.                                                   CL*17
00143      12  FILLER              PIC X.                                  CL*17
00144      12  DMD-MAIL-CODE.                                              CL*17
00145          16  WS-MAIL-4       PIC X(4).                               CL*17
00146          16  WS-MAIL-5       PIC X.                                  CL*17
00147      12  FILLER              PIC X(4).                               CL*17
00148                                                                      CL*17
00149      12  W-NAME-LAST             PIC  X(15).                         CL*17
00150      12  W-NAME-FIRST            PIC  X(15).                         CL*17
00151      12  W-NAME-MIDDLE.                                              CL*17
00152          16  FILLER              PIC  X.                             CL*25
00153          16  W-NAME-MIDDLE-2     PIC  X.                             CL*25
00154          16  FILLER              PIC  X(13).                         CL*17
00155                                                                      CL*17
00156  01  EN-EOB-LENGTH                   PIC S9(04) COMP VALUE +310.     CL*17
00157  01  EB-EOB-LENGTH                   PIC S9(04) COMP VALUE +410.     CL*17
00158                                                                      CL*18
00159  01  WS-PREV-CLAIM.                                                  CL*17
00160      12  WS-PREV-COMPANY             PIC X.                          CL*17
00161      12  WS-PREV-CARRIER             PIC X.                          CL*17
00162      12  WS-PREV-CLAIM-NO            PIC X(7)   VALUE LOW-VALUES.    CL*17
00163      12  WS-PREV-CERT-NO             PIC X(11).                      CL*17
00164      12  WS-PREV-SEQ-NO              PIC S9(4)  COMP VALUE ZEROS.    CL*19
00165      12  WS-PREV-RECORD-TYPE         PIC X.                          CL*17
00166                                                                      CL*19
00167  01  WS-HAVE-NOTE-HEADER             PIC X.                          CL*17
00168      88  HAVE-NOTE-HEADER                        VALUE 'Y'.          CL*17
00169  01  WS-DMD-TAPE-PYMT-SW             PIC X.                          CL*19
00170      88  WS-DMD-TAPE-PYMT                        VALUE 'Y'.          CL*19
090309 01  WS-ALL-CHECK-RELEASE            PIC X.
090309     88  WS-STOP-ALL-CHECKS                      VALUE 'Y'.
00171                                                                   EL175
00172  01  FILLER                          COMP-3.                      EL175
00173      12  WS-LAST-ERROR-COUNT         PIC S9(3)   VALUE ZERO.      EL175
00174      12  WS-UPDATE-SW                PIC S9      VALUE ZERO.      EL175
00175      12  WS-PAYMENT-COUNTER          PIC S9(3)   VALUE ZERO.      EL175
031808     12  WS-PMT-UNAPPROVED-COUNT     PIC S9(3)   VALUE ZERO.
00176                                                                   EL175
00177      12  TIME-IN                     PIC S9(7)   VALUE ZERO.      EL175
00178      12  TIME-OUT REDEFINES TIME-IN  PIC S999V9(4).                  CL*27
00179      12  WS-HHMM  REDEFINES TIME-IN  PIC S9(5)V99.                   CL*27
00180                                                                   EL175
00181      12  WS-ELACTQ-BROWSE-SW         PIC S9      VALUE ZERO.      EL175
00182      12  WS-ELTRLR-BROWSE-SW         PIC S9      VALUE ZERO.      EL175
00183      12  WS-ELNOTE-BROWSE-SW         PIC S9      VALUE ZERO.         CL*19
00184                                                                   EL175
00185      12  WS-RELEASED-COUNT           PIC S9(5)   VALUE ZERO.      EL175
00186      12  WS-RELEASED-COUNT-2         PIC S9(5)   VALUE ZERO.         CL*14
00187      12  WS-RELEASED-COUNT-3         PIC S9(5)   VALUE ZERO.         CL*14
031808     12  WS-UNAPPROVED-COUNT         PIC S9(5)   VALUE ZERO.
00188                                                                      CL*14
00189      12  WS-RELEASED-AMOUNT          PIC S9(9)V99 VALUE ZERO.     EL175
00190      12  WS-RELEASED-AMOUNT-2        PIC S9(9)V99 VALUE ZERO.        CL*14
00191      12  WS-RELEASED-AMOUNT-3        PIC S9(9)V99 VALUE ZERO.        CL*14
031808     12  WS-UNAPPROVED-AMOUNT        PIC S9(9)V99 VALUE ZERO.
00192                                                                      CL*14
00193      12  WS-NON-CASH-REL-CNT         PIC S9(05)   VALUE ZERO.        CL*14
00194      12  WS-NON-CASH-REL-AMT         PIC S9(9)V99 VALUE ZERO.        CL*14
00195                                                                      CL*19
00196      12  WS-DMD-PYMT-COUNT           PIC S9(5)   VALUE ZERO.         CL*19
00197      12  WS-DMD-PYMT-AMOUNT          PIC S9(9)V99 VALUE ZERO.        CL*19
00198      12  WS-DMD-DMO-COUNT            PIC S9(5)   VALUE ZERO.         CL*19
00199      12  WS-DMD-DMO-AMOUNT           PIC S9(9)V99 VALUE ZERO.        CL*19
00200                                                                   EL175
00201  01  FILLER     COMP  SYNC.                                       EL175
00202      12  SC-ITEM                     PIC S9(4)   VALUE +0001.     EL175
00203                                                                   EL175
00204      12  WS-KEY-LENGTH               PIC S9(4)   VALUE ZERO.      EL175
00205                                                                   EL175
00206      12  WS-CHECK-QUE-COUNTER        PIC S9(8)   VALUE ZERO.      EL175
00207      12  WS-CHECK-QUE-COUNTER-2      PIC S9(8)   VALUE ZERO.         CL*14
00208      12  WS-CHECK-QUE-COUNTER-3      PIC S9(8)   VALUE ZERO.         CL*14
00209                                                                      CL*14
00210      12  WS-CHECK-COUNTER            PIC S9(4)   VALUE +10.       EL175
00211      12  WS-CHECK-COUNTER-2          PIC S9(4)   VALUE +10.          CL*14
00212      12  WS-CHECK-COUNTER-3          PIC S9(4)   VALUE +10.          CL*14
00213                                                                   EL175
00214      12  WS-DMO-LENGTH               PIC S9(4)   VALUE +108 COMP.    CL*18
00215      12  WS-DCT-LENGTH               PIC S9(4)   VALUE +53  COMP.    CL*18
00216                                                                      CL*18
00217      EJECT                                                           CL*14
00218  01  FILLER.                                                      EL175
00219      12  WS-CONTROL-FILE-KEY.                                     EL175
00220          16  WS-CFK-COMPANY-ID       PIC X(3)    VALUE SPACES.    EL175
00221          16  WS-CFK-RECORD-TYPE      PIC X       VALUE SPACES.    EL175
00222          16  FILLER                  PIC XX      VALUE SPACES.    EL175
00223          16  WS-CFK-BENEFIT-NO       PIC XX      VALUE SPACES.    EL175
00224          16  WS-CFK-SEQUENCE-NO      PIC S9(4)   VALUE ZERO       EL175
00225                                      COMP.                        EL175
00226                                                                   EL175
00227      12  WS-ELMSTR-KEY               PIC X(20).                      CL**2
00228      12  WS-ACTIVITY-TRAILERS-KEY.                                EL175
00229          16  WS-ATK-COMPANY-CD       PIC X.                       EL175
00230          16  WS-ATK-CARRIER          PIC X.                       EL175
00231          16  WS-ATK-CLAIM-NO         PIC X(7).                    EL175
00232          16  WS-ATK-CERT-NO          PIC X(11).                   EL175
00233          16  WS-ATK-SEQUENCE-NO      PIC S9(4) COMP.                 CL*25
00234                                                                   EL175
00235      12  WS-LAST-ACTIVITY-TRAILERS-KEY PIC X(22) VALUE LOW-VALUES.EL175
00236                                                                   EL175
00237      12  WS-ACTIVITY-QUE-KEY.                                     EL175
00238          16  WS-AQK-COMPANY-CD       PIC X.                       EL175
00239          16  WS-AQK-CARRIER          PIC X.                       EL175
00240          16  WS-AQK-CLAIM-NO         PIC X(7).                    EL175
00241          16  WS-AQK-CERT-NO          PIC X(11).                   EL175
00242                                                                   EL175
00243      12  WS-LAST-ACTIVITY-QUE-KEY    PIC X(20) VALUE LOW-VALUES.  EL175
00244                                                                   EL175
00245      12  WS-CLAIM-MASTER-KEY.                                     EL175
00246          16  WS-CK-COMPANY-CD       PIC X.                        EL175
00247          16  WS-CK-CARRIER          PIC X.                        EL175
00248          16  WS-CK-CLAIM-NO         PIC X(7).                     EL175
00249          16  WS-CK-CERT-NO          PIC X(11).                    EL175
00250                                                                   EL175
00251      12  WS-NOTE-KEY.                                                CL*17
00252          16  WS-EN-COMPANY-CD       PIC X.                           CL*17
00253          16  WS-EN-CARRIER          PIC X.                           CL*17
00254          16  WS-EN-CLAIM-NO         PIC X(7).                        CL*17
00255          16  WS-EN-CERT-NO          PIC X(11).                       CL*17
00256          16  WS-EN-PAYMENT-SEQ-NO   PIC S9(4)  COMP.                 CL*17
00257          16  WS-EN-RECORD-TYPE      PIC X.                           CL*17
00258                                                                      CL*18
00259      12  W-NOTE-KEY.                                                 CL*18
00260          16  W-NOTE-COMP-CD         PIC X.                           CL*18
00261          16  W-NOTE-CERT-KEY.                                        CL*18
00262              20  W-NOTE-CARRIER     PIC X.                           CL*18
00263              20  W-NOTE-GROUPING    PIC X(6).                        CL*18
00264              20  W-NOTE-STATE       PIC XX.                          CL*18
00265              20  W-NOTE-ACCOUNTG    PIC X(10).                       CL*18
00266              20  W-NOTE-EFF-DT      PIC XX.                          CL*18
00267              20  W-NOTE-CERT-NO     PIC X(11).                       CL*18
00268                                                                      CL*17
00269      12  WS-MAPSET-NAME              PIC X(8)  VALUE 'EL175S'.    EL175
00270                                                                   EL175
00271      12  WS-MAP-NAME                 PIC X(8)  VALUE 'EL175A'.       CL*25
00272      12  FILLER REDEFINES WS-MAP-NAME.                               CL*25
00273          16  FILLER                  PIC XX.                      EL175
00274          16  WS-MAP-NUMBER           PIC X(6).                    EL175
00275                                                                   EL175
00276      12  THIS-PGM                    PIC X(8) VALUE 'EL175'.      EL175
00277                                                                   EL175
00278      12  EL001                       PIC X(8) VALUE 'EL001'.      EL175
00279      12  EL004                       PIC X(8) VALUE 'EL004'.      EL175
00280      12  EL005                       PIC X(8) VALUE 'EL005'.      EL175
00281      12  EL010                       PIC X(8) VALUE 'EL010'.      EL175
00282      12  EL126                       PIC X(8) VALUE 'EL126'.      EL175
00283      12  ELDATCV                     PIC X(8) VALUE 'ELDATCV'.    EL175
00284                                                                   EL175
00285      12  WS-FORMS-PRINTER            PIC X(4) VALUE SPACES.          CL*12
00286      12  WS-CHECK-QUE-DSID           PIC X(8) VALUE 'ELCHKQ'.     EL175
00287      12  WS-ENQ-COMPANY-ID           PIC X(3) VALUE SPACES.          CL*12
00288      12  WS-ACTIVITY-TRAILERS-DSID   PIC X(8) VALUE 'ELTRLR'.     EL175
00289      12  WS-CONTROL-FILE-DSID        PIC X(8) VALUE 'ELCNTL'.     EL175
00290      12  WS-ACTIVITY-QUE-DSID        PIC X(8) VALUE 'ELACTQ'.     EL175
00291      12  WS-NOTE-DSID                PIC X(8) VALUE 'ELNOTE'.        CL*17
00292      12  WS-EXTRACT-DSID             PIC X(8) VALUE 'ELNOTX'.        CL*17
00293                                                                   EL175
00294      12  WS-SPACES                   PIC X       VALUE SPACES.    EL175
00295                                                                   EL175
00296      12  WS-CLAIM-NO                 PIC X(7)    VALUE SPACES.    EL175
00297      12  WS-CARRIER                  PIC X       VALUE SPACES.    EL175
00298                                                                      CL*14
00299      12  WS-CERT-NO.                                                 CL*14
00300          16  WS-CERT-PRIME.                                          CL*14
00301              20  WS-CERT-PRIME-1-3   PIC X(3)    VALUE SPACES.       CL*25
00302              20  WS-CERT-PRIME-4-10  PIC X(7)    VALUE SPACES.       CL*25
00303          16  WS-CERT-SFX             PIC X       VALUE SPACES.       CL*25
00304                                                                   EL175
00305      12  WS-CURRENT-DATE             PIC XX VALUE LOW-VALUES.     EL175
00306                                                                   EL175
00307      12  WS-TRANS-ID                 PIC X(4)    VALUE 'EX45'.    EL175
00308                                                                   EL175
00309      12  WS-TOTAL-LINE1.                                          EL175
00310          16  FILLER                  PIC X(14)   VALUE            EL175
00311              'CONTROL GROUP'.                                     EL175
00312          16  WS-TL1-CONTROL-GROUP    PIC 9(7)-   VALUE ZEROS.        CL*19
00313          16  WS-TL1-RELEASE          PIC X(20)   VALUE               CL*12
00314              ' RELEASED'.                                         EL175
00315                                                                   EL175
00316      12  WS-TOTAL-LINE2.                                          EL175
00317          16  WS-TL1-COUNT            PIC ZZ,ZZ9      VALUE ZEROS.    CL*19
00318          16  FILLER                  PIC X(6)        VALUE           CL*12
00319              ' CHECK'.                                               CL*14
00320          16  WS-TL1-PLURAL           PIC X           VALUE           CL*12
00321              'S'.                                                    CL*12
00322          16  FILLER                  PIC X(18)       VALUE           CL*12
00323              ' IN THE AMOUNT OF'.                                    CL*12
00324          16  WS-TL1-AMOUNT           PIC Z,ZZZ,ZZ9.99.            EL175
00325                                                                   EL175
031808     12  WS-TOTAL-LINE3.
031808         16  FILLER                  PIC X(20)   VALUE
031808             'AWAITING APPROVAL - '.
031808         16  WS-TL3-COUNT            PIC ZZ,ZZ9      VALUE ZEROS.
031808         16  FILLER                  PIC X(6)        VALUE       
031808             ' CHECK'.                                           
031808         16  WS-TL3-PLURAL           PIC X           VALUE       
031808             'S'.                                                
031808         16  FILLER                  PIC X(18)       VALUE       
031808             ' IN THE AMOUNT OF'.                                
031808         16  WS-TL3-AMOUNT           PIC Z,ZZZ,ZZ9.99.           
031808
00326      12  WS-TOTAL-LINE-1-3.                                          CL*14
00327          16  FILLER                  PIC X(15)   VALUE               CL*14
00328              'CONTROL GROUPS'.                                       CL*14
00329          16  WS-TL1-3-CONTROL-GROUP  PIC 9(7)-.                      CL*14
00330          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00331          16  WS-TL2-3-CONTROL-GROUP  PIC 9(7)-.                      CL*14
00332          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00333          16  WS-TL3-3-CONTROL-GROUP  PIC 9(7)-.                      CL*14
00334          16  WS-TL-1-3-RELEASE       PIC X(20)   VALUE               CL*14
00335              ' RELEASED'.                                            CL*14
00336                                                                      CL*14
00337      12  WS-TOTAL-LINE-2-3.                                          CL*14
00338          16  WS-TL1-3-COUNT          PIC ZZZZ9.                      CL*14
00339          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00340          16  WS-TL2-3-COUNT          PIC ZZZZ9.                      CL*14
00341          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00342          16  WS-TL3-3-COUNT          PIC ZZZZ9.                      CL*14
00343          16  FILLER                  PIC X(18)   VALUE               CL*14
00344              ' CKS IN THE AMT OF'.                                   CL*14
00345          16  WS-TL1-3-AMOUNT         PIC ZZZZ,ZZ9.99.                CL*14
00346          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00347          16  WS-TL2-3-AMOUNT         PIC ZZZZ,ZZ9.99.                CL*14
00348          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00349          16  WS-TL3-3-AMOUNT         PIC ZZZZ,ZZ9.99.                CL*14
00350                                                                      CL*14
00351      12  WS-NON-CASH-TOTAL-LINE.                                     CL*15
00352          16  WS-NC-TL1-COUNT         PIC ZZZZ9.                      CL*15
00353          16  WS-NC-TL1-LIT           PIC X(32)   VALUE SPACES.       CL*15
00354                                                                      CL*19
00355      12  WS-DMD-TOTAL-LINE1.                                         CL*19
00356          16  FILLER                  PIC X(14)   VALUE               CL*19
00357              'CONTROL GROUP'.                                        CL*19
00358          16  WS-DMD-TL1-CONTROL-GROUP                                CL*19
00359                                      PIC 9(7)-.                      CL*19
00360          16  FILLER                  PIC X       VALUE SPACE.        CL*25
00361          16  WS-DMD-TL1-COUNT        PIC ZZ,ZZ9.                     CL*19
00362          16  WS-DMD-TL1-RELEASE      PIC X(20)   VALUE               CL*19
00363              ' RELEASED'.                                            CL*19
00364          16  WS-DMD-TL1-AMOUNT       PIC Z,ZZZ,ZZ9.99.               CL*19
00365                                                                      CL*19
00366      12  WS-DMD-TOTAL-LINE2.                                         CL*19
00367          16  WS-DMD-TL2-COUNT1       PIC ZZ,ZZ9.                     CL*19
00368          16  FILLER                  PIC X(6)        VALUE           CL*19
00369              ' CHECK'.                                               CL*19
00370          16  WS-DMD-TL2-PLURAL1      PIC X           VALUE           CL*19
00371              ' '.                                                    CL*19
00372          16  FILLER                  PIC X(6)        VALUE           CL*25
00373              ' FOR '.                                                CL*19
00374          16  WS-DMD-TL2-AMOUNT1      PIC Z,ZZZ,ZZ9.99.               CL*19
00375          16  FILLER                  PIC X(6)        VALUE SPACES.   CL*25
00376          16  WS-DMD-TL2-COUNT2       PIC ZZ,ZZ9.                     CL*19
00377          16  FILLER                  PIC X           VALUE SPACE.    CL*19
00378          16  FILLER                  PIC X(3)        VALUE           CL*19
00379              'DMO'.                                                  CL*19
00380          16  WS-DMD-TL2-PLURAL2      PIC X           VALUE           CL*19
00381              ' '.                                                    CL*19
00382          16  FILLER                  PIC X(7)        VALUE           CL*25
00383              ' FOR '.                                                CL*19
00384          16  WS-DMD-TL2-AMOUNT2      PIC Z,ZZZ,ZZ9.99.               CL*19
00385                                                                      CL*14
00386      12  WS-PMT-APPROVAL             PIC X.                          CL**7
00387           88 WS-PMT-APPROVAL-USED            VALUE 'Y' 'G'.          CL**7
00388                                                                   EL175
00389      12  WS-SAVE-NOTE-RECORD         PIC X(310).                     CL*17
00390                                                                      CL*17
00391      EJECT                                                           CL*14
00392  01  ERROR-MESSAGES.                                              EL175
00393      12  ER-0004                 PIC X(4)  VALUE '0004'.          EL175
00394      12  ER-0008                 PIC X(4)  VALUE '0008'.          EL175
00395      12  ER-0029                 PIC X(4)  VALUE '0029'.          EL175
00396      12  ER-0070                 PIC X(4)  VALUE '0070'.          EL175
00397      12  ER-0078                 PIC X(4)  VALUE '0078'.             CL*13
00398      12  ER-0330                 PIC X(4)  VALUE '0330'.          EL175
00399      12  ER-0331                 PIC X(4)  VALUE '0331'.          EL175
00400      12  ER-0395                 PIC X(4)  VALUE '0395'.          EL175
00401      12  ER-0412                 PIC X(4)  VALUE '0412'.          EL175
00402      12  ER-0413                 PIC X(4)  VALUE '0413'.          EL175
00403      12  ER-0431                 PIC X(4)  VALUE '0431'.          EL175
00404      12  ER-0432                 PIC X(4)  VALUE '0432'.          EL175
00405      12  ER-0568                 PIC X(4)  VALUE '0568'.          EL175
00406      12  ER-0849                 PIC X(4)  VALUE '0849'.             CL*18
00407      12  ER-0910                 PIC X(4)  VALUE '0910'.             CL*25
00408      12  ER-0913                 PIC X(4)  VALUE '0913'.             CL*25
00409      12  ER-0919                 PIC X(4)  VALUE '0919'.             CL*17
00410      12  ER-0921                 PIC X(4)  VALUE '0921'.             CL*17
00411      12  ER-0946                 PIC X(4)  VALUE '0946'.             CL*18
00412      12  ER-0947                 PIC X(4)  VALUE '0947'.             CL*18
00413      12  ER-0948                 PIC X(4)  VALUE '0948'.             CL*18
00414      12  ER-0949                 PIC X(4)  VALUE '0949'.             CL*18
00415      12  ER-0950                 PIC X(4)  VALUE '0950'.             CL*22
00416      12  ER-0951                 PIC X(4)  VALUE '0951'.             CL*22
00417      12  ER-0954                 PIC X(4)  VALUE '0954'.             CL*18
00418      12  ER-0974                 PIC X(4)  VALUE '0974'.             CL*21
00419      12  ER-0975                 PIC X(4)  VALUE '0975'.             CL*21
00420      12  ER-1882                 PIC X(4)  VALUE '1882'.             CL**2
00421      12  ER-2370                 PIC X(4)  VALUE '2370'.          EL175
00422      12  ER-3048                 PIC X(4)  VALUE '3048'.             CL*12
00423      12  ER-8051                 PIC X(4)  VALUE '8051'.             CL*18
00424      12  ER-8052                 PIC X(4)  VALUE '8052'.             CL*18
00425      12  ER-8053                 PIC X(4)  VALUE '8053'.             CL*18
00426      12  ER-8054                 PIC X(4)  VALUE '8054'.             CL*18
00427      12  ER-8055                 PIC X(4)  VALUE '8055'.             CL*18
00428      12  ER-8056                 PIC X(4)  VALUE '8056'.             CL*18
00429      12  ER-8057                 PIC X(4)  VALUE '8057'.             CL*18
00430      12  ER-8058                 PIC X(4)  VALUE '8058'.             CL*18
00431      12  ER-8059                 PIC X(4)  VALUE '8059'.             CL*18
00432      12  ER-8060                 PIC X(4)  VALUE '8060'.             CL*18
00433      12  ER-8061                 PIC X(4)  VALUE '8061'.             CL*18
00434      12  ER-8062                 PIC X(4)  VALUE '8062'.             CL*18
00435      12  ER-8063                 PIC X(4)  VALUE '8063'.             CL*18
00436      12  ER-8064                 PIC X(4)  VALUE '8064'.             CL*18
00437      12  ER-8065                 PIC X(4)  VALUE '8065'.             CL*18
00438      12  ER-8066                 PIC X(4)  VALUE '8066'.             CL*18
00439      12  ER-8135                 PIC X(4)  VALUE '8135'.             CL*23
00440      12  ER-8137                 PIC X(4)  VALUE '8137'.             CL*23
00441      12  ER-8152                 PIC X(4)  VALUE '8152'.             CL*20
00442      12  ER-8153                 PIC X(4)  VALUE '8153'.             CL*20
00443      12  ER-8154                 PIC X(4)  VALUE '8154'.             CL*23
00444      12  ER-8155                 PIC X(4)  VALUE '8155'.             CL*23
00445                                                                   EL175
00446      EJECT                                                        EL175
00447                              COPY ELCINTF SUPPRESS.                  CL**7
00448                                                                   EL175
00449      12  FILLER REDEFINES PI-PROGRAM-WORK-AREA.                      CL*25
00450          16  FILLER                  PIC X.                       EL175
00451          16  PI-CK-CONTROL-NO        PIC S9(8)      COMP.         EL175
00452          16  PI-PROC-SW              PIC S9.                         CL*12
00453              88  PI-SCREEN-PROCESSED        VALUE +1.                CL*12
090309             88  PI-ALL-CHKS-NOT-ALLOWED    VALUE +2.
00454          16  PI-CHECK-AMOUNT         PIC S9(7)      COMP-3.          CL*13
00455          16  FILLER                  PIC X(103).                     CL*15
00456          16  PI-END-CONTROL-NO       PIC S9(8)      COMP.            CL*14
00457          16  PI-MONTH-END-SAVE       PIC XX.                         CL*25
00458          16  PI-NON-CASH-REL-CNT     PIC S9(5)      COMP-3.          CL*25
00459          16  PI-NON-CASH-REL-AMT     PIC S9(9)V99   COMP-3.          CL*14
00460          16  FILLER                  PIC X(512).                     CL*17
00461                                                                   EL175
00462      EJECT                                                           CL*14
00463      COPY EL175S.                                                    CL*13
00464                                                                   EL175
00465  01  FILLER  REDEFINES  EL175AI.                                     CL*25
00466      12  FILLER                        PIC X(170).                   CL*25
00467      12  FILLER      OCCURS 5  INDEXED BY EL175A-INDEX1.             CL*25
00468          16  FILLER  OCCURS 3  INDEXED BY EL175A-INDEX2.             CL*25
00469              20  EL175A-CLAIM-LENGTH   PIC S9(4) COMP.               CL*25
00470              20  EL175A-CLAIM-ATTRB    PIC X.                        CL*25
00471              20  EL175A-CLAIM          PIC X(7).                     CL*25
00472              20  EL175A-CARRIER-LENGTH PIC S9(4) COMP.               CL*25
00473              20  EL175A-CARRIER-ATTRB  PIC X.                        CL*25
00474              20  EL175A-CARRIER        PIC X.                        CL*25
00475                                                                   EL175
00476      EJECT                                                        EL175
00477                                  COPY ELCDCTB.                       CL*18
00478      EJECT                                                           CL*18
00479                                  COPY ELCEMIB.                       CL**7
00480      EJECT                                                           CL*14
00481                                  COPY ELCDATE.                       CL**7
00482      EJECT                                                           CL*14
00483                                  COPY ELCLOGOF.                      CL**7
00484      EJECT                                                           CL*14
00485                                  COPY ELCATTR.                       CL**7
00486      EJECT                                                           CL*18
00487                                  COPY ELCDMO.                        CL*18
CIDMOD     EJECT                                                           CL*18
CIDMOD                                 COPY ELCDAR.                        CL*18
00488      EJECT                                                           CL*14
00489                                  COPY ELCAID.                        CL**7
00490      EJECT                                                           CL*17
00491  01  FILLER REDEFINES DFHAID.                                        CL*25
00492      12  FILLER                      PIC X(8).                    EL175
00493      12  PF-VALUES                   PIC X                        EL175
00494          OCCURS 24 TIMES.                                         EL175
00495                                                                   EL175
00496      EJECT                                                           CL*14
00497  LINKAGE SECTION.                                                 EL175
00498                                                                   EL175
00499  01  DFHCOMMAREA                     PIC X(1024).                 EL175
00500                                                                   EL175
00501      EJECT                                                        EL175
00502                                  COPY ELCCHKQ.                       CL**7
00503      EJECT                                                        EL175
00504                                  COPY ELCACTQ.                       CL**7
00505      EJECT                                                        EL175
00506                                  COPY ELCCNTL.                       CL**7
00507      EJECT                                                        EL175
00508                                  COPY ELCTRLR.                       CL**7
00509      EJECT                                                           CL**2
00510                                  COPY ELCMSTR.                       CL**7
00511      EJECT                                                           CL*17
00512                                  COPY ELCNOTE.                       CL*17
00513      EJECT                                                           CL*17
00514                                  COPY ELCNOTX.                       CL*17
00515      EJECT                                                           CL*18
00516                                  COPY ERCDMDNT.                      CL*18
00517      EJECT                                                        EL175
00518  PROCEDURE DIVISION.                                              EL175
00519                                                                   EL175
00520      MOVE EIBDATE               TO DC-JULIAN-YYDDD.               EL175
00521      MOVE '5'                   TO DC-OPTION-CODE.                EL175
00522      PERFORM 8500-DATE-CONVERSION.                                EL175
00523      MOVE DC-GREG-DATE-1-EDIT   TO  SAVE-DATE.                    EL175
00524      MOVE DC-BIN-DATE-1         TO  SAVE-BIN-DATE.                EL175
00525                                                                   EL175
00526      MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.     EL175
00527                                                                   EL175
00528 *    NOTE ******************************************************* EL175
00529 *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL * EL175
00530 *         *  FROM ANOTHER MODULE.                               * EL175
00531 *         *******************************************************.EL175
00532                                                                   EL175
00533      IF EIBCALEN NOT GREATER THAN ZERO                            EL175
00534          MOVE UNACCESS-MSG       TO  LOGOFF-MSG                   EL175
00535          GO TO 8300-SEND-TEXT.                                    EL175
00536                                                                   EL175
00537      EXEC CICS HANDLE CONDITION                                   EL175
00538          PGMIDERR   (9600-PGMIDERR)                               EL175
00539          NOTFND     (0140-MAIN-LOGIC)                             EL175
00540          ENDFILE    (0140-MAIN-LOGIC)                             EL175
00541          ENQBUSY    (0910-ENQ-BUSY)                               EL175
00542          TERMIDERR  (7010-TERMID-ERROR)                           EL175
00543          TRANSIDERR (7020-TRANS-ERROR)                            EL175
00544          ERROR      (9990-ERROR)                                     CL*25
00545      END-EXEC.                                                       CL*25
00546                                                                   EL175
00547      MOVE +2                     TO  EMI-NUMBER-OF-LINES          EL175
00548                                      EMI-SWITCH2.                 EL175
00549                                                                   EL175
00550      EJECT                                                        EL175
00551  0010-MAIN-LOGIC.                                                 EL175
00552      IF PI-CALLING-PROGRAM NOT = THIS-PGM                         EL175
00553          IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                   EL175
00554              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6     EL175
00555              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5     EL175
00556              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4     EL175
00557              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3     EL175
00558              MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2     EL175
00559              MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1     EL175
00560              MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM   EL175
00561              MOVE THIS-PGM             TO  PI-CALLING-PROGRAM     EL175
00562            ELSE                                                   EL175
00563              MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM     EL175
00564              MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM   EL175
00565              MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1     EL175
00566              MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2     EL175
00567              MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3     EL175
00568              MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4     EL175
00569              MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5     EL175
00570              MOVE SPACES               TO  PI-SAVED-PROGRAM-6     EL175
00571        ELSE                                                       EL175
00572          GO TO 0040-PROCESS-INPUT.                                   CL*12
00573                                                                   EL175
00574  0015-INITIALIZE.                                                    CL*12
00575 *    NOTE ******************************************************* EL175
00576 *         *      INITALIZE THE WORK FIELDS FOR THE PROGRAM      * EL175
00577 *         *  INTERFACE BLOCK FOR THIS MODULE.                   * EL175
00578 *         *******************************************************.EL175
00579                                                                   EL175
00580      MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.        EL175
00581      MOVE ZERO                   TO  PI-PROC-SW.                     CL*12
00582      MOVE LOW-VALUES             TO  PI-MONTH-END-SAVE.              CL*14
00583      MOVE +0                     TO  PI-CK-CONTROL-NO                CL*14
00584                                      PI-END-CONTROL-NO               CL*14
00585                                      PI-NON-CASH-REL-CNT             CL*14
00586                                      PI-NON-CASH-REL-AMT.            CL*14
090309     MOVE 'N'                    TO  WS-ALL-CHECK-RELEASE.
00587                                                                      CL*14
00588      MOVE LOW-VALUES             TO  EL175AI.                     EL175
00589      MOVE -1                     TO  AOPTIONL.                    EL175
00590      PERFORM 8100-SEND-INITIAL-MAP.                               EL175
00591                                                                   EL175
00592      EJECT                                                        EL175
00593  0040-PROCESS-INPUT.                                                 CL*12
00594 *    NOTE ******************************************************* EL175
00595 *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- * EL175
00596 *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    * EL175
00597 *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         * EL175
00598 *         *******************************************************.EL175
00599                                                                   EL175
00600      IF EIBAID = DFHCLEAR                                         EL175
00601          GO TO 9400-CLEAR.                                        EL175
00602                                                                   EL175
00603      IF PI-PROCESSOR-ID = 'LGXX'                                  EL175
00604          NEXT SENTENCE                                            EL175
00605      ELSE                                                         EL175
00606          EXEC CICS READQ TS                                       EL175
00607              QUEUE   (PI-SECURITY-TEMP-STORE-ID)                  EL175
00608              INTO    (SECURITY-CONTROL)                           EL175
00609              LENGTH  (SC-COMM-LENGTH)                             EL175
00610              ITEM    (SC-ITEM)                                    EL175
00611          END-EXEC                                                 EL175
00612          MOVE SC-CLAIMS-DISPLAY (12)   TO  PI-DISPLAY-CAP         EL175
00613          MOVE SC-CLAIMS-UPDATE  (12)   TO  PI-MODIFY-CAP          EL175
00614          IF NOT MODIFY-CAP                                        EL175
00615              MOVE 'UPDATE'             TO  SM-READ                EL175
00616              PERFORM 9995-SECURITY-VIOLATION                      EL175
00617              MOVE ER-0070              TO  EMI-ERROR              EL175
00618              GO TO 8100-SEND-INITIAL-MAP.                         EL175
00619                                                                   EL175
00620      IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                       EL175
00621          MOVE ER-0008               TO  EMI-ERROR                    CL*14
00622          MOVE -1                 TO  APFKL                        EL175
00623          GO TO 8200-SEND-DATAONLY.                                   CL*13
00624                                                                   EL175
00625      EXEC CICS RECEIVE                                            EL175
00626          INTO   (EL175AI)                                         EL175
00627          MAPSET (WS-MAPSET-NAME)                                  EL175
00628          MAP    (WS-MAP-NAME)                                        CL*25
00629      END-EXEC.                                                       CL*25
00630                                                                   EL175
00631      IF APFKL > ZERO                                                 CL*27
00632          IF EIBAID NOT = DFHENTER                                 EL175
00633              MOVE ER-0004           TO  EMI-ERROR                    CL*14
00634              MOVE AL-UNBOF       TO  APFKA                        EL175
00635              MOVE -1             TO  APFKL                        EL175
00636              GO TO 8200-SEND-DATAONLY                                CL*13
00637            ELSE                                                   EL175
00638              IF APFKO IS NUMERIC                                  EL175
00639                AND APFKO > ZERO                                      CL*27
00640                AND APFKO < '25'                                      CL*27
00641                  MOVE PF-VALUES (APFKI)  TO  EIBAID               EL175
00642                ELSE                                               EL175
00643                  MOVE ER-0029           TO  EMI-ERROR                CL*14
00644                  MOVE AL-UNBOF       TO  APFKA                    EL175
00645                  MOVE -1             TO  APFKL                    EL175
00646                  GO TO 8200-SEND-DATAONLY.                           CL*13
00647                                                                   EL175
00648      IF EIBAID = DFHPF12                                          EL175
00649          MOVE EL010              TO  THIS-PGM                     EL175
00650          GO TO 9300-XCTL.                                         EL175
00651                                                                   EL175
00652      IF EIBAID = DFHPF23                                          EL175
00653          GO TO 9000-RETURN-CICS.                                  EL175
00654                                                                   EL175
00655      IF EIBAID = DFHPF24                                          EL175
00656          MOVE EL126              TO  THIS-PGM                     EL175
00657          GO TO 9300-XCTL.                                         EL175
092809
092809     IF EIBAID = DFHPF1 AND PI-SCREEN-PROCESSED
092809        IF AOPTIONI = '1' OR '2' OR '3' OR '4'
092809            IF ABYL = +0 AND
092809               AAMTL = +0 AND
092809               ACARRL = +0 AND
092809               AGROUPL = +0 AND
092809               AACCTL = +0 AND
092809               ACLNO07L = +0 AND
092809               ACLNO08L = +0 AND
092809               ACLNO09L = +0 AND
092809               ACLNO10L = +0 AND
092809               ACLNO11L = +0 AND
092809               ACLNO12L = +0 AND
092809               ACLNO13L = +0 AND
092809               ACLNO14L = +0 AND
092809               ACLNO15L = +0 AND
092809               ACLNO16L = +0 AND
092809               ACLNO17L = +0 AND
092809               ACLNO18L = +0 AND
092809               ACLNO19L = +0 AND
092809               ACLNO20L = +0 AND
092809               ACLNO21L = +0 
092809                  MOVE +2       TO PI-PROC-SW
092809            END-IF
092809        END-IF
092809     END-IF.
00658                                                                   EL175
00659      IF EIBAID = DFHPF1 AND                                          CL*12
00660         PI-SCREEN-PROCESSED                                          CL*12
00661           GO TO 0200-PROCESS-CHECK-RELEASE.                          CL*12
00662                                                                      CL*12
00663      IF EIBAID NOT = DFHENTER                                     EL175
00664          MOVE ER-0008            TO  EMI-ERROR                    EL175
00665          MOVE -1                 TO  APFKL                        EL175
00666          GO TO 8200-SEND-DATAONLY.                                   CL*13
00667                                                                   EL175
00668      EJECT                                                        EL175
00669  0100-MAIN-LOGIC.                                                 EL175
00670                                                                   EL175
00671 *    NOTE ******************************************************* EL175
00672 *         *      OPTION                                         *    CL*25
00673 *         *        1   -     ALL PAYMENTS                       *    CL*25
00674 *         *        2   -     LIFE PAYMENTS  (COVERAGE 1)        *    CL*25
00675 *         *        3   -     A&H PAYMENTS   (COVERAGE 2)        *    CL*25
00676 *         *        4   -     AUTO PAYMENTS                      *    CL*25
00677 *         *        5   -     COMBINED PAYMENTS                  *    CL*25
00678 *         *******************************************************.EL175
00679                                                                   EL175
00680      IF AOPTIONL GREATER ZERO AND                                    CL*27
00681         (AOPTIONI GREATER ZERO AND LESS '6')                         CL*12
00682          MOVE AL-UNNON           TO  AOPTIONA                     EL175
00683        ELSE                                                       EL175
00684          MOVE -1                 TO  AOPTIONL                     EL175
00685          MOVE AL-UNBON           TO  AOPTIONA                     EL175
00686          MOVE ER-0330               TO  EMI-ERROR                    CL*14
00687          PERFORM 9900-ERROR-FORMAT.                               EL175
00688                                                                      CL*13
00689      IF AAMTL GREATER ZERO                                           CL*13
00690         EXEC CICS BIF DEEDIT                                         CL*13
00691              FIELD (AAMTI)                                           CL*13
00692              LENGTH (7)                                              CL*13
00693         END-EXEC                                                     CL*13
00694         IF AAMTI NOT NUMERIC                                         CL*13
00695             MOVE -1             TO  AAMTL                            CL*13
00696             MOVE AL-UABON       TO  AAMTA                            CL*13
00697             MOVE ER-0078        TO  EMI-ERROR                        CL*13
00698             PERFORM 9900-ERROR-FORMAT                                CL*13
00699          ELSE                                                        CL*13
00700             MOVE AAMTI          TO  PI-CHECK-AMOUNT                  CL*13
00701                                     AAMTO                            CL*13
00702             MOVE AL-UANON       TO  AAMTA.                           CL*13
00703                                                                      CL*15
00704      IF PI-COMPANY-ID = 'CSL' AND                                    CL*25
00705         AOPTIONI = '1'                                               CL*25
00706          MOVE -1                TO  AOPTIONL                         CL*15
00707          MOVE AL-UNBON          TO  AOPTIONA                         CL*15
00708          MOVE ER-0330           TO  EMI-ERROR                        CL*15
00709          PERFORM 9900-ERROR-FORMAT.                                  CL*15
00710                                                                   EL175
00711      IF NOT PI-NO-CARRIER-SECURITY                                EL175
00712          IF ACARRL GREATER THAN +0                                EL175
00713             IF ACARRI NOT = PI-CARRIER-SECURITY                   EL175
00714                MOVE -1      TO ACARRL                                CL*25
00715                MOVE ER-2370 TO EMI-ERROR                          EL175
00716                PERFORM 9900-ERROR-FORMAT.                         EL175
00717                                                                   EL175
00718      IF NOT PI-NO-CARRIER-SECURITY                                   CL**2
00719         IF GCARRL GREATER THAN +0                                    CL**2
00720            IF GCARRI NOT = PI-CARRIER-SECURITY                       CL**2
00721               MOVE -1            TO GCARRL                           CL*25
00722               MOVE AL-UABOF      TO  GCARRA                          CL**2
00723               MOVE ER-2370       TO EMI-ERROR                        CL*25
00724               PERFORM 9900-ERROR-FORMAT.                             CL**2
00725                                                                      CL**2
00726      IF AOPTIONI NOT = '5'                                           CL*27
00727         GO TO 0105-MAIN-LOGIC.                                       CL**2
00728                                                                      CL**2
00729      IF GCARRL  > +0 OR                                              CL*27
00730         GGROUPL > +0 OR                                              CL*27
00731         GSTATEL > +0 OR                                              CL*27
00732         GACCTL  > +0 OR                                              CL*27
00733         GBENEL  > +0                                                 CL*27
00734         NEXT SENTENCE                                                CL**2
00735      ELSE                                                            CL**2
00736         GO TO 0105-MAIN-LOGIC.                                       CL**2
00737                                                                      CL**2
00738      IF (GCARRL > +0 AND                                             CL*27
00739         GGROUPL > +0 AND                                             CL*27
00740         GSTATEL > +0 AND                                             CL*27
00741         GACCTL  > +0 AND                                             CL*27
00742         GBENEL  = +0)                                                CL*27
00743        OR                                                            CL**2
00744         (GCARRL = +0 AND                                             CL*27
00745         GGROUPL = +0 AND                                             CL*27
00746         GSTATEL = +0 AND                                             CL*27
00747         GACCTL  = +0 AND                                             CL*27
00748         GBENEL  > +0)                                                CL*27
00749         NEXT SENTENCE                                                CL**2
00750      ELSE                                                            CL**2
00751         MOVE ER-1882 TO EMI-ERROR                                    CL**2
00752         MOVE -1      TO GCARRL                                       CL*25
00753         PERFORM 9900-ERROR-FORMAT.                                   CL**2
00754                                                                      CL**2
00755  0105-MAIN-LOGIC.                                                    CL**2
00756                                                                      CL**2
00757      IF NOT EMI-NO-ERRORS                                            CL**2
00758         GO TO 8200-SEND-DATAONLY.                                    CL**2
00759                                                                      CL**2
090309     IF AOPTIONI = '1' OR '2' OR '3' OR '4'
090309         IF ABYL = +0 AND
090309            AAMTL = +0 AND
090309            ACARRL = +0 AND
090309            AGROUPL = +0 AND
090309            AACCTL = +0 AND
090309            ACLNO07L = +0 AND
090309            ACLNO08L = +0 AND
090309            ACLNO09L = +0 AND
090309            ACLNO10L = +0 AND
090309            ACLNO11L = +0 AND
090309            ACLNO12L = +0 AND
090309            ACLNO13L = +0 AND
090309            ACLNO14L = +0 AND
090309            ACLNO15L = +0 AND
090309            ACLNO16L = +0 AND
090309            ACLNO17L = +0 AND
090309            ACLNO18L = +0 AND
090309            ACLNO19L = +0 AND
090309            ACLNO20L = +0 AND
090309            ACLNO21L = +0 
090309               MOVE 'Y'      TO WS-ALL-CHECK-RELEASE
090309         END-IF
090309     ELSE
090309         MOVE 'N'            TO WS-ALL-CHECK-RELEASE
090309     END-IF.
090309
00760      MOVE +9                     TO  WS-KEY-LENGTH.               EL175
00761                                                                   EL175
00762      SET EL175A-INDEX1                                            EL175
00763          EL175A-INDEX2  TO  +1.                                   EL175
00764                                                                   EL175
00765  0110-MAIN-LOGIC.                                                 EL175
00766                                                                      CL**2
00767      IF EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0     CL*27
00768          IF NOT PI-NO-CARRIER-SECURITY                            EL175
00769             IF PI-CARRIER-SECURITY NOT = EL175A-CARRIER           EL175
00770                                  (EL175A-INDEX1 EL175A-INDEX2)    EL175
00771                MOVE ER-2370 TO EMI-ERROR                          EL175
00772                MOVE -1      TO EL175A-CARRIER-ATTRB                  CL*25
00773                               (EL175A-INDEX1 EL175A-INDEX2)          CL*25
00774                PERFORM 9900-ERROR-FORMAT                          EL175
00775                GO TO 0150-MAIN-LOGIC.                             EL175
00776                                                                      CL**2
00777      IF EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0       CL*27
00778         IF NOT PI-NO-CARRIER-SECURITY                             EL175
00779            MOVE PI-CARRIER-SECURITY TO EL175A-CARRIER             EL175
00780                               (EL175A-INDEX1 EL175A-INDEX2)       EL175
00781            MOVE +1 TO EL175A-CARRIER-LENGTH                       EL175
00782                               (EL175A-INDEX1 EL175A-INDEX2).      EL175
00783                                                                   EL175
00784      MOVE EMI-FATAL-CTR          TO  WS-LAST-ERROR-COUNT.            CL*25
00785                                                                   EL175
00786      IF EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0       CL*27
00787                       OR                                             CL*25
00788         EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0     CL*27
00789          NEXT SENTENCE                                            EL175
00790        ELSE                                                       EL175
00791          GO TO 0150-MAIN-LOGIC.                                   EL175
00792                                                                      CL*13
00793      IF (EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2)           CL*27
00794                                           NOT > ZERO                 CL*27
00795                         OR                                           CL*25
00796          EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2)         CL*27
00797                                           NOT > ZERO)                CL*27
00798             MOVE ER-0431               TO  EMI-ERROR                 CL*25
00799             PERFORM 9900-ERROR-FORMAT                                CL*25
00800             GO TO 0150-MAIN-LOGIC.                                   CL*25
00801                                                                   EL175
00802      MOVE SPACES                 TO  WS-ACTIVITY-QUE-KEY.            CL*25
00803      MOVE PI-COMPANY-CD          TO  WS-AQK-COMPANY-CD.              CL*25
00804                                                                   EL175
00805      MOVE EL175A-CLAIM   (EL175A-INDEX1 EL175A-INDEX2)               CL*25
00806                                  TO  WS-AQK-CLAIM-NO              EL175
00807                                      WS-CLAIM-NO.                    CL*25
00808                                                                   EL175
00809      MOVE EL175A-CARRIER (EL175A-INDEX1 EL175A-INDEX2)               CL*25
00810                                  TO  WS-AQK-CARRIER               EL175
00811                                      WS-CARRIER.                  EL175
00812                                                                      CL**5
00813      IF WS-ELACTQ-BROWSE-SW = ZERO                                   CL*12
00814          EXEC CICS STARTBR                                        EL175
00815              DATASET   (WS-ACTIVITY-QUE-DSID)                     EL175
00816              RIDFLD    (WS-ACTIVITY-QUE-KEY)                      EL175
00817              GENERIC   EQUAL                                      EL175
00818              KEYLENGTH (WS-KEY-LENGTH)                               CL*25
00819          END-EXEC                                                    CL*25
00820          MOVE +1                 TO  WS-ELACTQ-BROWSE-SW             CL*12
00821        ELSE                                                       EL175
00822          EXEC CICS RESETBR                                        EL175
00823              DATASET   (WS-ACTIVITY-QUE-DSID)                     EL175
00824              RIDFLD    (WS-ACTIVITY-QUE-KEY)                      EL175
00825              GENERIC   EQUAL                                      EL175
00826              KEYLENGTH (WS-KEY-LENGTH)                               CL*25
00827          END-EXEC.                                                   CL*25
00828                                                                   EL175
00829  0120-MAIN-LOGIC.                                                 EL175
00830                                                                      CL**2
00831      EXEC CICS READNEXT                                           EL175
00832          DATASET (WS-ACTIVITY-QUE-DSID)                           EL175
00833          RIDFLD  (WS-ACTIVITY-QUE-KEY)                            EL175
00834          SET     (ADDRESS OF ACTIVITY-QUE)                           CL*25
00835      END-EXEC.                                                       CL*25
00836                                                                   EL175
00837      IF AQ-PAYMENT-COUNTER NOT NUMERIC                               CL*10
00838          MOVE ZEROS              TO AQ-PAYMENT-COUNTER.              CL*10
00839                                                                      CL*18
00840      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                          CL*10
00841          MOVE ZEROS              TO AQ-PMT-UNAPPROVED-COUNT.         CL*10
00842                                                                      CL*10
00843      IF WS-AQK-COMPANY-CD NOT = PI-COMPANY-CD  OR                 EL175
00844         WS-AQK-CARRIER    NOT = WS-CARRIER     OR                    CL*27
00845         WS-AQK-CLAIM-NO   NOT = WS-CLAIM-NO                       EL175
00846          MOVE ER-0432               TO  EMI-ERROR                 EL175
00847          PERFORM 9900-ERROR-FORMAT                                EL175
00848          GO TO 0150-MAIN-LOGIC.                                   EL175
00849                                                                   EL175
00850      IF NOT PENDING-PAYMENTS                                      EL175
00851          GO TO 0120-MAIN-LOGIC.                                   EL175
00852                                                                   EL175
00853      MOVE AL-UANON               TO EL175A-CLAIM-ATTRB            EL175
00854                                    (EL175A-INDEX1  EL175A-INDEX2).   CL*25
00855                                                                   EL175
00856      IF EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0     CL*27
00857          MOVE AL-UANON         TO  EL175A-CARRIER-ATTRB           EL175
00858                                   (EL175A-INDEX1 EL175A-INDEX2).     CL*25
00859                                                                   EL175
00860      MOVE +1                     TO  ACLNO07L.                       CL**2
00861                                                                   EL175
00862      GO TO 0150-MAIN-LOGIC.                                       EL175
00863                                                                   EL175
00864  0140-MAIN-LOGIC.                                                 EL175
00865      MOVE ER-0331                   TO  EMI-ERROR.                EL175
00866      PERFORM 9900-ERROR-FORMAT.                                   EL175
00867                                                                   EL175
00868  0150-MAIN-LOGIC.                                                 EL175
00869                                                                      CL**2
00870      IF EMI-FATAL-CTR NOT GREATER THAN WS-LAST-ERROR-COUNT        EL175
00871          GO TO 0160-MAIN-LOGIC.                                   EL175
00872                                                                   EL175
00873      IF EL175A-CLAIM-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0       CL*27
00874          MOVE AL-UABON           TO  EL175A-CLAIM-ATTRB           EL175
00875                                     (EL175A-INDEX1  EL175A-INDEX2)EL175
00876        ELSE                                                       EL175
00877          MOVE AL-UABOF           TO  EL175A-CLAIM-ATTRB           EL175
00878                                    (EL175A-INDEX1  EL175A-INDEX2).EL175
00879                                                                   EL175
00880      MOVE -1                     TO  EL175A-CLAIM-LENGTH          EL175
00881                                     (EL175A-INDEX1  EL175A-INDEX2)EL175
00882                                                                   EL175
00883      IF EL175A-CARRIER-LENGTH (EL175A-INDEX1 EL175A-INDEX2) > +0     CL*27
00884          MOVE AL-UABON           TO  EL175A-CARRIER-ATTRB         EL175
00885                                    (EL175A-INDEX1  EL175A-INDEX2) EL175
00886        ELSE                                                       EL175
00887          MOVE AL-UABOF           TO  EL175A-CARRIER-ATTRB         EL175
00888                                    (EL175A-INDEX1  EL175A-INDEX2).EL175
00889                                                                   EL175
00890  0160-MAIN-LOGIC.                                                 EL175
00891      IF EL175A-INDEX1 LESS THAN +5                                   CL**2
00892          SET EL175A-INDEX1 UP BY +1                               EL175
00893          GO TO 0110-MAIN-LOGIC.                                   EL175
00894                                                                   EL175
00895      IF EL175A-INDEX2 LESS THAN +3                                EL175
00896          SET EL175A-INDEX1 TO +1                                  EL175
00897          SET EL175A-INDEX2 UP BY +1                               EL175
00898          GO TO 0110-MAIN-LOGIC.                                   EL175
00899                                                                   EL175
00900      IF WS-ELACTQ-BROWSE-SW NOT = ZERO                               CL*12
00901          MOVE ZERO               TO  WS-ELACTQ-BROWSE-SW             CL*12
00902          EXEC CICS ENDBR                                          EL175
00903              DATASET (WS-ACTIVITY-QUE-DSID)                          CL*25
00904          END-EXEC.                                                   CL*25
00905                                                                   EL175
00906      IF EMI-FATAL-CTR GREATER THAN ZERO                           EL175
00907          MOVE ZERO               TO PI-PROC-SW                       CL*12
00908          GO TO 8200-SEND-DATAONLY.                                   CL*13
00909                                                                   EL175
00910      EJECT                                                        EL175
00911  0200-PROCESS-CHECK-RELEASE.                                         CL*12
00912 *    NOTE ******************************************************* EL175
00913 *         *      OBTAIN EXCLUSIVE CONTROL OF THE CHECK QUEUE    * EL175
00914 *         *  DATASET DURING THE GENERATION OF THE CHECK QUEUE   * EL175
00915 *         *  RECORDS.                                           * EL175
00916 *         *******************************************************.EL175
00917                                                                   EL175
00918      MOVE PI-COMPANY-ID          TO  WS-ENQ-COMPANY-ID.           EL175
00919                                                                   EL175
00920      IF EIBAID = DFHPF1                                              CL*12
00921          EXEC CICS ENQ                                               CL*12
00922              RESOURCE (WS-CHECK-QUE-DSID)                            CL*12
00923              LENGTH   (11)                                           CL*12
00924          END-EXEC.                                                   CL*12
00925                                                                   EL175
00926 *    NOTE ******************************************************* EL175
00927 *         *      GET THE CONTROL GROUP NUMBER FROM THE COMPANY  * EL175
00928 *         *  CONTROL RECORD OF THE CONTROL FILE.                * EL175
00929 *         *******************************************************.EL175
00930                                                                   EL175
00931      MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.         EL175
00932                                                                   EL175
00933      MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.           EL175
00934      MOVE '1'                    TO  WS-CFK-RECORD-TYPE.          EL175
00935      MOVE ZERO                   TO  WS-CFK-SEQUENCE-NO.          EL175
00936                                                                   EL175
00937      IF EIBAID = DFHPF1                                              CL*12
00938          EXEC CICS READ UPDATE                                       CL*12
00939              DATASET (WS-CONTROL-FILE-DSID)                          CL*12
00940              RIDFLD  (WS-CONTROL-FILE-KEY)                           CL*12
00941              SET     (ADDRESS OF CONTROL-FILE)                       CL*17
00942          END-EXEC                                                    CL*12
00943        ELSE                                                          CL*12
00944          EXEC CICS READ                                              CL*12
00945              DATASET (WS-CONTROL-FILE-DSID)                          CL*12
00946              RIDFLD  (WS-CONTROL-FILE-KEY)                           CL*12
00947              SET     (ADDRESS OF CONTROL-FILE)                       CL*17
00948          END-EXEC.                                                   CL*12
00949                                                                   EL175
00950      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.             EL175
00951      MOVE '5'                    TO  DC-OPTION-CODE.              EL175
00952      PERFORM 8500-DATE-CONVERSION.                                EL175
00953      MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.             EL175
00954      MOVE EIBTIME                TO  TIME-IN.                     EL175
00955                                                                   EL175
00956      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
00957          IF AOPTIONI = '1'                                           CL*25
00958              GO TO 0200-SET-CONTROL-GROUPS.                          CL*14
00959                                                                      CL*14
00960      ADD +1                      TO  CF-CO-CHECK-QUE-COUNTER.     EL175
00961                                                                   EL175
00962      IF CO-QUE-COUNT-RESET                                        EL175
00963          MOVE +1                 TO  CF-CO-CHECK-QUE-COUNTER.     EL175
00964                                                                   EL175
00965      MOVE CF-FORMS-PRINTER-ID      TO  WS-FORMS-PRINTER              CL**2
00966      MOVE CF-CO-CHECK-QUE-COUNTER  TO  WS-CHECK-QUE-COUNTER.      EL175
00967      MOVE CF-PAYMENT-APPROVAL-SW   TO  WS-PMT-APPROVAL.           EL175
00968      MOVE CF-CURRENT-MONTH-END     TO  PI-MONTH-END-SAVE.            CL*14
00969                                                                      CL*14
00970      GO TO 0200-REWRITE-CONTROL-FILE.                                CL*16
00971                                                                      CL*14
00972  0200-SET-CONTROL-GROUPS.                                            CL*14
00973                                                                      CL*14
00974      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
00975          ADD +1                    TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00976          IF CO-QUE-COUNT-RESET                                       CL*14
00977              MOVE +1               TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00978                                        WS-CHECK-QUE-COUNTER          CL*14
00979              ADD +1                TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00980              MOVE CF-CO-CHECK-QUE-COUNTER                            CL*14
00981                                    TO  WS-CHECK-QUE-COUNTER-2        CL*14
00982              ADD +1                TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00983              MOVE CF-CO-CHECK-QUE-COUNTER                            CL*14
00984                                    TO  WS-CHECK-QUE-COUNTER-3        CL*14
00985          ELSE                                                        CL*14
00986              MOVE CF-CO-CHECK-QUE-COUNTER                            CL*14
00987                                    TO  WS-CHECK-QUE-COUNTER          CL*14
00988              ADD +1                TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00989              IF CO-QUE-COUNT-RESET                                   CL*14
00990                  MOVE +1           TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00991                                        WS-CHECK-QUE-COUNTER-2        CL*14
00992                  ADD +1            TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00993                  MOVE CF-CO-CHECK-QUE-COUNTER                        CL*14
00994                                    TO  WS-CHECK-QUE-COUNTER-3        CL*14
00995             ELSE                                                     CL*14
00996                 MOVE CF-CO-CHECK-QUE-COUNTER                         CL*14
00997                                    TO  WS-CHECK-QUE-COUNTER-2        CL*14
00998                 ADD +1             TO  CF-CO-CHECK-QUE-COUNTER       CL*14
00999                 IF CO-QUE-COUNT-RESET                                CL*14
01000                     MOVE +1        TO  CF-CO-CHECK-QUE-COUNTER       CL*14
01001                                        WS-CHECK-QUE-COUNTER-3        CL*14
01002                 ELSE                                                 CL*14
01003                     MOVE CF-CO-CHECK-QUE-COUNTER                     CL*14
01004                                    TO  WS-CHECK-QUE-COUNTER-3.       CL*14
01005                                                                      CL*14
01006      MOVE CF-FORMS-PRINTER-ID      TO  WS-FORMS-PRINTER              CL*14
01007      MOVE CF-PAYMENT-APPROVAL-SW   TO  WS-PMT-APPROVAL.              CL*14
01008      MOVE CF-CURRENT-MONTH-END     TO  PI-MONTH-END-SAVE.            CL*14
01009                                                                      CL*16
01010  0200-REWRITE-CONTROL-FILE.                                          CL*16
01011                                                                      CL*16
01012      IF EIBAID = DFHPF1                                              CL*25
01013          EXEC CICS REWRITE                                           CL*16
01014              DATASET   (WS-CONTROL-FILE-DSID)                        CL*16
01015              FROM      (CONTROL-FILE)                                CL*16
01016          END-EXEC.                                                   CL*16
01017                                                                      CL*14
01018      EJECT                                                           CL*14
01019  0200-CONTINUE.                                                      CL*14
01020 *    NOTE ******************************************************* EL175
01021 *         *      GET STORAGE FOR WRITING CHECK QUEUE RECORDS    *    CL*12
01022 *         *******************************************************.EL175
01023                                                                   EL175
01024      EXEC CICS GETMAIN                                            EL175
01025          SET     (ADDRESS OF CHECK-QUE)                              CL*17
01026          LENGTH  (100)                                               CL**2
01027          INITIMG (WS-SPACES)                                      EL175
01028      END-EXEC.                                                    EL175
01029                                                                   EL175
01030 *    NOTE ******************************************************* EL175
01031 *         *      BROWSE THE ACTIVITY QUEUE TO FIND ANY CLAIMS   *    CL*12
01032 *         *  THAT HAVE PAYMENTS PENDING.                        *    CL*12
01033 *         *                                                     * EL175
01034 *         *      WHEN A PAYMENT IS PENDING, BROWSE THE ACTIVITY * EL175
01035 *         *  TRAILERS TO LOCATE THE PAYMENT TRAILER RECORDS. IF * EL175
01036 *         *  THE PAYMENT TRAILER MEETS THE QUALIFICATION FOR    * EL175
01037 *         *  THIS CHECK RELEASE RUN, GENERATE A CHECK QUEUE     *    CL*12
01038 *         *  RECORD.                                            * EL175
01039 *         *******************************************************.EL175
01040                                                                   EL175
01041      MOVE LOW-VALUES             TO  WS-ACTIVITY-QUE-KEY.         EL175
01042      MOVE PI-COMPANY-CD          TO  WS-AQK-COMPANY-CD.           EL175
01043                                                                   EL175
01044      IF ACARRL GREATER THAN ZERO                                  EL175
01045          MOVE ACARRI             TO  WS-AQK-CARRIER.              EL175
01046                                                                   EL175
01047      EJECT                                                        EL175
01048  0205-STARTBR-ELACTQ.                                                CL*12
01049      EXEC CICS HANDLE CONDITION                                   EL175
01050          NOTFND  (0300-END-OF-SEARCH)                                CL*12
01051          ENDFILE (0300-END-OF-SEARCH)                                CL*12
01052      END-EXEC.                                                       CL*12
01053                                                                   EL175
01054      IF WS-ELACTQ-BROWSE-SW = ZERO                                EL175
01055          EXEC CICS STARTBR                                        EL175
01056              DATASET (WS-ACTIVITY-QUE-DSID)                       EL175
01057              RIDFLD  (WS-ACTIVITY-QUE-KEY)                        EL175
01058              GTEQ                                                    CL*25
01059          END-EXEC                                                    CL*25
01060          MOVE +1                 TO  WS-ELACTQ-BROWSE-SW.         EL175
01061                                                                   EL175
01062  0220-READNEXT-ELACTQ.                                               CL*12
01063      EXEC CICS READNEXT                                           EL175
01064          DATASET (WS-ACTIVITY-QUE-DSID)                           EL175
01065          RIDFLD  (WS-ACTIVITY-QUE-KEY)                            EL175
01066          SET     (ADDRESS OF ACTIVITY-QUE)                           CL*25
01067      END-EXEC.                                                       CL*25
01068                                                                      CL*10
01069      IF AQ-PAYMENT-COUNTER NOT NUMERIC                               CL*10
01070          MOVE ZEROS              TO AQ-PAYMENT-COUNTER.              CL*10
01071                                                                      CL*18
01072      IF AQ-PMT-UNAPPROVED-COUNT NOT NUMERIC                          CL*10
01073          MOVE ZEROS              TO AQ-PMT-UNAPPROVED-COUNT.         CL*10
01074                                                                   EL175
01075      IF WS-ACTIVITY-QUE-KEY = WS-LAST-ACTIVITY-QUE-KEY            EL175
01076          GO TO 0220-READNEXT-ELACTQ.                                 CL*12
01077                                                                   EL175
01078      MOVE WS-ACTIVITY-QUE-KEY    TO  WS-LAST-ACTIVITY-QUE-KEY.    EL175
01079                                                                   EL175
01080      IF WS-AQK-COMPANY-CD NOT = PI-COMPANY-CD                     EL175
01081          GO TO 0300-END-OF-SEARCH.                                   CL*12
01082                                                                   EL175
01083      IF NOT PENDING-PAYMENTS                                      EL175
01084          GO TO 0220-READNEXT-ELACTQ.                                 CL*12
01085                                                                   EL175
01086      IF WS-PMT-APPROVAL-USED                                         CL**7
01087         IF AQ-PMT-UNAPPROVED-COUNT NOT EQUAL +0                   EL175
031808*01088            GO TO 0220-READNEXT-ELACTQ.                               CL*12
031808           PERFORM 3000-TOTAL-UNAPPROVED THRU 3999-EXIT.

080712     EXEC CICS HANDLE CONDITION
080712         NOTFND  (0300-END-OF-SEARCH)
080712         ENDFILE (0300-END-OF-SEARCH)
080712     END-EXEC

01090      IF ACARRL > ZERO   AND                                          CL*27
01091         AQ-CARRIER > ACARRI                                          CL*27
01092              GO TO 0300-END-OF-SEARCH.                               CL*12
01093                                                                   EL175
01094      IF ACLNO07L NOT GREATER THAN ZERO                               CL**2
092809       AND ACLNO08L NOT GREATER THAN ZERO
092809       AND ACLNO09L NOT GREATER THAN ZERO
092809       AND ACLNO10L NOT GREATER THAN ZERO
092809       AND ACLNO11L NOT GREATER THAN ZERO
092809       AND ACLNO12L NOT GREATER THAN ZERO
092809       AND ACLNO13L NOT GREATER THAN ZERO
092809       AND ACLNO14L NOT GREATER THAN ZERO
092809       AND ACLNO15L NOT GREATER THAN ZERO
092809       AND ACLNO16L NOT GREATER THAN ZERO
092809       AND ACLNO17L NOT GREATER THAN ZERO
092809       AND ACLNO18L NOT GREATER THAN ZERO
092809       AND ACLNO19L NOT GREATER THAN ZERO
092809       AND ACLNO20L NOT GREATER THAN ZERO
092809       AND ACLNO21L NOT GREATER THAN ZERO
01095          GO TO 0240-MAIN-LOGIC.                                   EL175
01096                                                                   EL175
01097      SET EL175A-INDEX1                                            EL175
01098          EL175A-INDEX2 TO +1.                                     EL175
01099                                                                   EL175
01100      EJECT                                                        EL175
01101  0230-MAIN-LOGIC.                                                 EL175
01102      IF AQ-CLAIM-NO = EL175A-CLAIM (EL175A-INDEX1 EL175A-INDEX2)     CL*25
01103          GO TO 0240-MAIN-LOGIC.                                   EL175
01104                                                                   EL175
01105      IF EL175A-INDEX1 LESS THAN +5                                   CL**2
01106          SET EL175A-INDEX1 UP BY +1                               EL175
01107          GO TO 0230-MAIN-LOGIC.                                   EL175
01108                                                                   EL175
01109      IF EL175A-INDEX2 LESS THAN +3                                EL175
01110          SET EL175A-INDEX1 TO +1                                  EL175
01111          SET EL175A-INDEX2 UP BY +1                               EL175
01112          GO TO 0230-MAIN-LOGIC.                                   EL175
01113                                                                   EL175
01114      GO TO 0220-READNEXT-ELACTQ.                                     CL*12
01115                                                                   EL175
01116  0240-MAIN-LOGIC.                                                 EL175
01117      EXEC CICS ENDBR                                              EL175
01118          DATASET (WS-ACTIVITY-QUE-DSID)                              CL*25
01119      END-EXEC.                                                       CL*25
01120                                                                   EL175
01121      MOVE ZERO                   TO  WS-ELACTQ-BROWSE-SW.         EL175
01122                                                                   EL175
01123      IF EIBAID = DFHPF1                                              CL*12
01124          EXEC CICS READ UPDATE                                       CL*12
01125              DATASET (WS-ACTIVITY-QUE-DSID)                          CL*12
01126              RIDFLD  (WS-ACTIVITY-QUE-KEY)                           CL*12
01127              SET     (ADDRESS OF ACTIVITY-QUE)                       CL*17
01128          END-EXEC                                                    CL*12
01129       ELSE                                                           CL*12
01130          EXEC CICS READ                                              CL*12
01131              DATASET (WS-ACTIVITY-QUE-DSID)                          CL*12
01132              RIDFLD  (WS-ACTIVITY-QUE-KEY)                           CL*12
01133              SET     (ADDRESS OF ACTIVITY-QUE)                       CL*17
01134          END-EXEC.                                                   CL*12
01135                                                                   EL175
01136      MOVE ZERO                   TO  WS-UPDATE-SW.                EL175
01137                                                                   EL175
01138      MOVE AQ-PAYMENT-COUNTER     TO  WS-PAYMENT-COUNTER.          EL175
01139                                                                   EL175
01140      MOVE WS-ACTIVITY-QUE-KEY    TO  WS-ACTIVITY-TRAILERS-KEY.       CL*17
01141      MOVE ZERO                   TO  WS-ATK-SEQUENCE-NO.          EL175
01142                                                                   EL175
01143      MOVE LOW-VALUES             TO  WS-LAST-ACTIVITY-TRAILERS-KEY   CL*17
01144                                                                   EL175
01145      EXEC CICS HANDLE CONDITION                                   EL175
01146          NOTFND  (0260-MAIN-LOGIC)                                EL175
01147          ENDFILE (0260-MAIN-LOGIC)                                EL175
01148      END-EXEC.                                                    EL175
01149                                                                   EL175
01150      EJECT                                                        EL175
01151  0250-MAIN-LOGIC.                                                 EL175
01152      IF AQ-PAYMENT-COUNTER NOT GREATER THAN ZERO                  EL175
01153          GO TO 0260-MAIN-LOGIC.                                   EL175
01154                                                                   EL175
01155      IF WS-ELTRLR-BROWSE-SW = ZERO                                EL175
01156          EXEC CICS STARTBR                                        EL175
01157              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  EL175
01158              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                   EL175
01159              EQUAL                                                   CL*25
01160          END-EXEC                                                    CL*25
01161          MOVE +1               TO  WS-ELTRLR-BROWSE-SW            EL175
01162        ELSE                                                       EL175
01163          EXEC CICS RESETBR                                        EL175
01164              DATASET (WS-ACTIVITY-TRAILERS-DSID)                  EL175
01165              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                   EL175
01166              EQUAL                                                   CL*25
01167          END-EXEC.                                                   CL*25
01168                                                                   EL175
01169      EJECT                                                        EL175
01170  0255-READNEXT-ELTRLR.                                               CL*12
01171      EXEC CICS READNEXT                                           EL175
01172          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      EL175
01173          RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                       EL175
01174          SET     (ADDRESS OF ACTIVITY-TRAILERS)                      CL*25
01175      END-EXEC.                                                       CL*25
01176                                                                   EL175
01177      IF WS-ACTIVITY-TRAILERS-KEY = WS-LAST-ACTIVITY-TRAILERS-KEY     CL*12
01178          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01179                                                                   EL175
01180      MOVE WS-ACTIVITY-TRAILERS-KEY  TO                            EL175
01181                                   WS-LAST-ACTIVITY-TRAILERS-KEY   EL175
01182                                                                   EL175
01183      IF WS-AQK-COMPANY-CD NOT = WS-ATK-COMPANY-CD  OR             EL175
01184         WS-AQK-CARRIER    NOT = WS-ATK-CARRIER     OR                CL*27
01185         WS-AQK-CLAIM-NO   NOT = WS-ATK-CLAIM-NO    OR                CL*27
01186         WS-AQK-CERT-NO    NOT = WS-ATK-CERT-NO                    EL175
01187          GO TO 0260-MAIN-LOGIC.                                   EL175
01188                                                                   EL175
01189      IF AT-TRAILER-TYPE NOT = '2'                                 EL175
01190          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01191                                                                   EL175
01192 *    NOTE ******************************************************* EL175
01193 *         *      OPTION                                         *    CL*25
01194 *         *        1    -    ALL PAYMENTS                       *    CL*25
01195 *         *        2    -    LIFE PAYMENTS (COVERAGE 1)         *    CL*25
01196 *         *        3    -    A&H PAYMENTS  (COVERAGE 2)         *    CL*25
01197 *         *        4    -    AUTO PAYMENTS                      *    CL*25
01198 *         *        5    -    COMBINED PAYMENTS                  *    CL*25
01199 *         *******************************************************.EL175
01200                                                                   EL175
01201      IF (AOPTIONI = '2' AND                                       EL175
100518         AT-CLAIM-TYPE NOT = PI-LIFE-OVERRIDE-L1 AND 'O')         EL175
01203        OR                                                         EL175
01204         (AOPTIONI = '3' AND                                       EL175
121203         (AT-CLAIM-TYPE NOT = PI-AH-OVERRIDE-L1 AND 'I' AND
052614          'G' AND 'F')) 
01206        OR                                                         EL175
01207         (AOPTIONI = '4' AND                                       EL175
01208          AT-PAYMENT-ORIGIN NOT = '2')                             EL175
01209        OR                                                            CL**2
01210         (AOPTIONI = '5' AND                                          CL**2
01211          AT-GROUPED-PAYMENT NOT = 'Y')                               CL*27
01212              GO TO 0255-READNEXT-ELTRLR.                             CL*12
01213                                                                      CL**2
01214      IF AOPTIONI NOT = '5'                                           CL*27
01215         IF AT-GROUPED-PAYMENT = 'Y'                                  CL*27
01216            GO TO 0255-READNEXT-ELTRLR.                               CL*12
01217                                                                   EL175
01218      IF AT-AMOUNT-PAID   NEGATIVE                                 EL175
01219          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01220                                                                   EL175
01221      IF AAMTL GREATER ZERO                                           CL*13
01222          IF AT-AMOUNT-PAID LESS PI-CHECK-AMOUNT                      CL*13
01223              NEXT SENTENCE                                           CL*13
01224            ELSE                                                      CL*13
01225              GO TO 0255-READNEXT-ELTRLR.                             CL*13
01226                                                                      CL*13
01227      IF OFFLINE-PMT                                               EL175
01228          SUBTRACT +1 FROM WS-PAYMENT-COUNTER                      EL175
01229          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01230                                                                   EL175
01231      IF AT-VOID-DT NOT = LOW-VALUES                               EL175
01232          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01233                                                                   EL175
01234      IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES                         CL*25
01235          GO TO 0255-READNEXT-ELTRLR.                                 CL*14
01236                                                                      CL*14
01237      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*27
01238        IF AT-CASH-PAYMENT = 'N'                                      CL*27
01239          GO TO 0257-CONTINUE-PAYMENTS.                               CL*14
01240                                                                      CL*14
01241      IF ABYL GREATER THAN ZERO                                    EL175
01242        AND ABYI NOT = AT-RECORDED-BY                              EL175
01243          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01244                                                                   EL175
01245      IF AT-TO-BE-WRITTEN-DT GREATER THAN WS-CURRENT-DATE          EL175
01246          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01247                                                                   EL175
01248      IF AT-CHECK-QUE-CONTROL NOT = ZERO                           EL175
01249          GO TO 0255-READNEXT-ELTRLR.                                 CL*12
01250                                                                   EL175
01251      IF WS-PMT-APPROVAL-USED                                         CL**7
01252         IF AT-PAYMENT-APPROVAL-SW NOT = 'A'                          CL*27
01253            GO TO 0255-READNEXT-ELTRLR.                               CL*12
01254                                                                   EL175
01255      IF AGROUPL > +0 OR                                              CL*27
01256         AACCTL > +0 OR                                               CL*27
01257         AOPTIONI = '5'                                               CL*25
01258          NEXT SENTENCE                                               CL**8
01259      ELSE                                                            CL**8
01260          GO TO 0257-CONTINUE-PAYMENTS.                               CL**8
01261                                                                      CL**2
01262      MOVE AT-CONTROL-PRIMARY TO WS-ELMSTR-KEY.                       CL**2
01263                                                                      CL**2
01264      EXEC CICS READ                                                  CL**2
01265           DATASET   ('ELMSTR')                                       CL**2
01266           RIDFLD    (WS-ELMSTR-KEY)                                  CL**2
01267           SET       (ADDRESS OF CLAIM-MASTER)                        CL*17
01268      END-EXEC.                                                       CL**2
01269                                                                      CL**8
01270      IF AGROUPL > +0                                                 CL*27
01271          IF AGROUPI = CL-CERT-GROUPING                               CL*25
01272              NEXT SENTENCE                                           CL**9
01273          ELSE                                                        CL**9
01274              GO TO 0255-READNEXT-ELTRLR.                             CL*12
01275                                                                      CL**9
01276      IF AACCTL > +0                                                  CL*27
01277          IF AACCTI = CL-CERT-ACCOUNT                                 CL*25
01278              NEXT SENTENCE                                           CL**8
01279          ELSE                                                        CL**8
01280              GO TO 0255-READNEXT-ELTRLR.                             CL*12
01281                                                                      CL**2
01282      IF GBENEL > +0                                                  CL*27
01283         IF AT-PAYEE-TYPE = 'B' AND                                   CL*27
01284            GBENEI = CL-BENEFICIARY                                   CL*27
01285            NEXT SENTENCE                                             CL**2
01286         ELSE                                                         CL**2
01287            GO TO 0255-READNEXT-ELTRLR.                               CL*12
01288                                                                      CL**2
01289      IF GACCTL > +0                                                  CL*27
01290         IF AT-PAYEE-TYPE = 'A'        AND                            CL*27
01291            GCARRI  = CL-CERT-CARRIER  AND                            CL*27
01292            GGROUPI = CL-CERT-GROUPING AND                            CL*27
01293            GSTATEI = CL-CERT-STATE    AND                            CL*27
01294            GACCTI  = CL-CERT-ACCOUNT                                 CL*27
01295              NEXT SENTENCE                                           CL*27
01296           ELSE                                                       CL*27
01297              GO TO 0255-READNEXT-ELTRLR.                             CL*27
01298                                                                      CL**2
01299      EJECT                                                        EL175
01300  0257-CONTINUE-PAYMENTS.                                             CL**2
01301                                                                      CL**2
01302 *    NOTE ******************************************************* EL175
01303 *         *      THE PAYMENT TRAILER HAS MET ALL OF THE         * EL175
01304 *         *  QUALIFICATIONS FOR THIS CHECK RELEASE, NOW         * EL175
01305 *         *  GENERATE A CHECK QUEUE RECORD.                     * EL175
01306 *         *******************************************************.EL175
01307                                                                   EL175
01308      SUBTRACT +1 FROM AQ-PAYMENT-COUNTER                          EL175
01309                       WS-PAYMENT-COUNTER.                         EL175
01310                                                                      CL*27
01311      MOVE +1                         TO  WS-UPDATE-SW.               CL*14
01312                                                                      CL*12
01313      IF PI-COMPANY-ID NOT = 'DMD'                                    CL*27
01314        IF AT-CASH-PAYMENT = 'N'                                      CL*27
01315          ADD +1                      TO  WS-NON-CASH-REL-CNT         CL*14
01316          ADD AT-AMOUNT-PAID          TO  WS-NON-CASH-REL-AMT         CL*14
01317          GO TO 0257-END-BROWSE-ELACTQ.                               CL*14
01318                                                                      CL*14
01319      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01320          IF AOPTIONI = '1'                                           CL*25
01321              NEXT SENTENCE                                           CL*14
01322          ELSE                                                        CL*14
01323              ADD +1                  TO  WS-RELEASED-COUNT           CL*14
01324              ADD AT-AMOUNT-PAID      TO  WS-RELEASED-AMOUNT          CL*14
01325              GO TO 0257-END-BROWSE-ELACTQ                            CL*14
01326      ELSE                                                            CL*14
01327          ADD +1                      TO  WS-RELEASED-COUNT           CL*14
01328          ADD AT-AMOUNT-PAID          TO  WS-RELEASED-AMOUNT          CL*14
01329          GO TO 0257-END-BROWSE-ELACTQ.                               CL*14
01330                                                                      CL*14
01331      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01332          IF AT-PAYMENT-TYPE = '5' OR '6'                             CL*25
01333              ADD +1                  TO  WS-RELEASED-COUNT           CL*14
01334              ADD AT-AMOUNT-PAID      TO  WS-RELEASED-AMOUNT          CL*14
01335          ELSE                                                        CL*14
01336              MOVE AT-CERT-NO         TO  WS-CERT-NO                  CL*14
01337              IF WS-CERT-PRIME-1-3 = '888'                            CL*25
01338                  ADD +1              TO  WS-RELEASED-COUNT-2         CL*14
01339                  ADD AT-AMOUNT-PAID  TO  WS-RELEASED-AMOUNT-2        CL*14
01340              ELSE                                                    CL*14
01341                  ADD +1              TO  WS-RELEASED-COUNT-3         CL*14
01342                  ADD AT-AMOUNT-PAID  TO  WS-RELEASED-AMOUNT-3.       CL*14
01343                                                                      CL*14
01344  0257-END-BROWSE-ELACTQ.                                             CL*14
01345                                                                   EL175
01346      EXEC CICS ENDBR                                              EL175
01347          DATASET (WS-ACTIVITY-TRAILERS-DSID)                      EL175
01348      END-EXEC.                                                    EL175
01349                                                                   EL175
01350      MOVE ZERO                   TO  WS-ELTRLR-BROWSE-SW.         EL175
01351                                                                   EL175
01352      IF EIBAID = DFHPF1                                              CL*13
01353          EXEC CICS READ UPDATE                                       CL*13
01354              DATASET (WS-ACTIVITY-TRAILERS-DSID)                     CL*13
01355              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                      CL*13
01356              SET     (ADDRESS OF ACTIVITY-TRAILERS)                  CL*17
01357          END-EXEC                                                    CL*13
01358        ELSE                                                          CL*13
01359          EXEC CICS READ                                              CL*13
01360              DATASET (WS-ACTIVITY-TRAILERS-DSID)                     CL*13
01361              RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                      CL*13
01362              SET     (ADDRESS OF ACTIVITY-TRAILERS)                  CL*17
01363          END-EXEC.                                                   CL*13
01364                                                                   EL175
01365      MOVE SPACES                 TO  CHECK-QUE.                   EL175
01366                                                                   EL175
01367      IF AT-CASH-PAYMENT = 'N'                                        CL*25
01368         GO TO 0259-REWRITE-PMT-TRLR.                                 CL*14
01369                                                                      CL*14
01370      IF AOPTIONI NOT = '5'                                           CL*27
01371         GO TO 0258-CONTINUE-ELCHKQ.                                  CL**2
01372                                                                      CL**2
01373      IF AT-PAYEE-TYPE = 'B'                                          CL*27
01374         MOVE CL-BENEFICIARY      TO  CQ-PAYEE-BENE-ACCT              CL**2
01375      ELSE                                                            CL**2
01376      IF AT-PAYEE-TYPE = 'A'                                          CL*27
01377         MOVE AT-CARRIER          TO  CQ-PAYEE-CARRIER                CL**2
01378         MOVE CL-CERT-GROUPING    TO  CQ-PAYEE-GROUPING               CL**2
01379         MOVE CL-CERT-STATE       TO  CQ-PAYEE-STATE                  CL**2
01380         MOVE CL-CERT-ACCOUNT     TO  CQ-PAYEE-BENE-ACCT.             CL**2
01381                                                                      CL**2
01382  0258-CONTINUE-ELCHKQ.                                               CL**2
01383                                                                      CL**2
01384      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01385          IF AOPTIONI = '1'                                           CL*25
01386              NEXT SENTENCE                                           CL*18
01387          ELSE                                                        CL*18
01388              MOVE WS-CHECK-QUE-COUNTER                               CL*18
01389                                  TO  CQ-CONTROL-NUMBER               CL*18
01390                                      CQ-CONTROL-NUMBER-A1            CL*18
01391                                      AT-CHECK-QUE-CONTROL            CL*18
01392              MOVE WS-CHECK-COUNTER                                   CL*18
01393                                  TO  CQ-SEQUENCE-NUMBER              CL*18
01394                                      CQ-SEQUENCE-NUMBER-A1           CL*18
01395                                      AT-CHECK-QUE-SEQUENCE           CL*18
01396              ADD +1              TO  WS-CHECK-COUNTER                CL*18
01397              GO TO 0259-CONTINUE-ELCHKQ                              CL*18
01398      ELSE                                                            CL*14
01399          MOVE WS-CHECK-QUE-COUNTER                                   CL*18
01400                                  TO  CQ-CONTROL-NUMBER               CL*18
01401                                      CQ-CONTROL-NUMBER-A1            CL*18
01402                                      AT-CHECK-QUE-CONTROL            CL*18
01403          MOVE WS-CHECK-COUNTER   TO  CQ-SEQUENCE-NUMBER              CL*18
01404                                      AT-CHECK-QUE-SEQUENCE           CL*18
01405                                      CQ-SEQUENCE-NUMBER-A1           CL*18
01406          ADD +1  TO  WS-CHECK-COUNTER                                CL*19
01407          GO TO 0259-CONTINUE-ELCHKQ.                                 CL*19
01408                                                                      CL*14
01409      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01410        IF AT-PAYMENT-TYPE = '5' OR '6'                               CL*25
01411          MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER           CL*14
01412                                          CQ-CONTROL-NUMBER-A1        CL*14
01413                                          AT-CHECK-QUE-CONTROL        CL*14
01414          MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER          CL*14
01415                                          CQ-SEQUENCE-NUMBER-A1       CL*14
01416                                          AT-CHECK-QUE-SEQUENCE       CL*14
01417          ADD +1                      TO  WS-CHECK-COUNTER            CL*14
01418          GO TO 0259-CONTINUE-ELCHKQ.                                 CL*14
01419                                                                      CL*14
01420      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01421        MOVE AT-CERT-NO               TO  WS-CERT-NO                  CL*25
01422        IF WS-CERT-PRIME-1-3 = '888'                                  CL*25
01423          MOVE WS-CHECK-QUE-COUNTER-2 TO  CQ-CONTROL-NUMBER           CL*14
01424                                          CQ-CONTROL-NUMBER-A1        CL*14
01425                                          AT-CHECK-QUE-CONTROL        CL*14
01426          MOVE WS-CHECK-COUNTER-2     TO  CQ-SEQUENCE-NUMBER          CL*14
01427                                          CQ-SEQUENCE-NUMBER-A1       CL*14
01428                                          AT-CHECK-QUE-SEQUENCE       CL*14
01429          ADD +1                      TO  WS-CHECK-COUNTER-2          CL*14
01430          GO TO 0259-CONTINUE-ELCHKQ                                  CL*14
01431        ELSE                                                          CL*14
01432          MOVE WS-CHECK-QUE-COUNTER-3 TO  CQ-CONTROL-NUMBER           CL*14
01433                                          CQ-CONTROL-NUMBER-A1        CL*14
01434                                          AT-CHECK-QUE-CONTROL        CL*14
01435          MOVE WS-CHECK-COUNTER-3     TO  CQ-SEQUENCE-NUMBER          CL*14
01436                                          CQ-SEQUENCE-NUMBER-A1       CL*14
01437                                          AT-CHECK-QUE-SEQUENCE       CL*14
01438          ADD +1                      TO  WS-CHECK-COUNTER-3          CL*14
01439          GO TO 0259-CONTINUE-ELCHKQ.                                 CL*14
01440                                                                      CL*14
01441  0259-CONTINUE-ELCHKQ.                                               CL*14
01442                                                                      CL*14
01443      MOVE 'CQ'                   TO  CQ-RECORD-ID.                   CL*14
01444      MOVE AT-COMPANY-CD          TO  CQ-COMPANY-CD                   CL*14
01445                                      CQ-COMPANY-CD-A1.               CL*14
01446                                                                      CL*14
01447      MOVE 'Q'                    TO  CQ-ENTRY-TYPE.               EL175
01448      MOVE AT-CARRIER             TO  CQ-CARRIER                      CL**2
01449      MOVE AT-CLAIM-NO            TO  CQ-CLAIM-NO.                 EL175
01450                                                                      CL*14
CIDMOD     MOVE 'CQ'                   TO  CQ-RECORD-ID.                     000
CIDMOD     MOVE AT-COMPANY-CD          TO  CQ-COMPANY-CD                     000
CIDMOD                                     CQ-COMPANY-CD-A1.                 000
CIDMOD     MOVE WS-CHECK-QUE-COUNTER   TO  CQ-CONTROL-NUMBER                 000
CIDMOD                                     CQ-CONTROL-NUMBER-A1              000
CIDMOD                                     AT-CHECK-QUE-CONTROL.             000
CIDMOD     MOVE WS-CHECK-COUNTER       TO  CQ-SEQUENCE-NUMBER                000
CIDMOD                                     AT-CHECK-QUE-SEQUENCE             000
CIDMOD                                     CQ-SEQUENCE-NUMBER-A1.            000
CIDMOD     ADD +1  TO  WS-CHECK-COUNTER.                                     000
CIDMOD                                                                       000
CIDMOD*    IF (AT-PAYMENT-ORIGIN = '2')  OR                                  000
CIDMOD*       (AT-TO-BE-WRITTEN-DT = WS-CURRENT-DATE)                        000
CIDMOD*          PERFORM 0920-WRITE-DLYACTV THRU 0920-EXIT                   000
CIDMOD*    END-IF                                                            000
CIDMOD                                                                       000
PEMMOD     IF EIBAID = DFHPF1                                              CL*13
PEMMOD        PERFORM 0920-WRITE-DLYACTV                                     000
PEMMOD                                 THRU 0920-EXIT                        000
PEMMOD     END-IF                                                            000
PEMMOD                                                                       000
01451      IF PI-COMPANY-ID = 'CVL' OR 'MNL'                               CL*25
01452          MOVE AT-CLAIM-NO        TO  CQ-PAYEE-BENE-ACCT              CL*18
01453      ELSE                                                            CL*18
01454          IF PI-COMPANY-ID = 'DMD'                                    CL*18
01455              PERFORM 0900-GET-MASTER THRU 0900-EXIT                  CL*25
01456              MOVE AT-COMPANY-CD  TO  CQ-DMD-COMPANY-CD-A2            CL*18
01457              MOVE AT-PAYEE-TYPE  TO  CQ-DMD-PAYEE-TYPE-A2            CL*18
01458              MOVE CL-BENEFICIARY TO  CQ-DMD-BENE-CODE-A2             CL*18
01459              MOVE AT-CLAIM-NO    TO  CQ-DMD-CLAIM-NO-A2              CL*18
01460              MOVE EIBTIME        TO  CQ-DMD-TIME-SEQ-A2.             CL*18
01461                                                                      CL*14
01462      MOVE AT-CERT-NO             TO  CQ-CERT-NO.                  EL175
01463      MOVE AT-CLAIM-TYPE          TO  CQ-CLAIM-TYPE.               EL175
01464      MOVE AT-CLAIM-PREM-TYPE     TO  CQ-CLAIM-SUB-TYPE.           EL175
01465      MOVE AT-SEQUENCE-NO         TO  CQ-PMT-TRLR-SEQUENCE.        EL175
01466      MOVE AT-AMOUNT-PAID         TO  CQ-CHECK-AMOUNT.             EL175
01467      MOVE AT-CHECK-NO            TO  CQ-CHECK-NUMBER.             EL175
01468      MOVE AT-PAYMENT-TYPE        TO  CQ-PAYMENT-TYPE.             EL175
01469      MOVE ZERO                   TO  CQ-TIMES-PRINTED                CL*25
01470                                      CQ-PRINT-AT-HHMM.               CL*25
01471      MOVE AT-RECORDED-BY         TO  CQ-CHECK-BY-USER.            EL175
01472                                                                   EL175
01473      MOVE LOW-VALUES             TO  CQ-CHECK-WRITTEN-DT.         EL175
01474                                                                   EL175
01475      MOVE +1750                  TO  CQ-LAST-UPDATED-BY.          EL175
01476      MOVE SAVE-BIN-DATE          TO  CQ-LAST-UPDATED-DT.             CL*19
01477      MOVE EIBTIME                TO  CQ-LAST-UPDATED-HHMMSS.         CL*19
01478                                                                   EL175
01479      IF EIBAID = DFHPF1                                              CL*13
01480          EXEC CICS WRITE                                             CL*13
01481              DATASET (WS-CHECK-QUE-DSID)                             CL*13
01482              FROM    (CHECK-QUE)                                     CL*13
01483              RIDFLD  (CQ-CONTROL-PRIMARY)                            CL*13
01484          END-EXEC.                                                   CL*13
01485                                                                   EL175
01486  0259-REWRITE-PMT-TRLR.                                              CL*25
01487                                                                   EL175
01488      MOVE +1750                  TO  AT-PAYMENT-LAST-UPDATED-BY.  EL175
01489      MOVE SAVE-BIN-DATE          TO  AT-PAYMENT-LAST-MAINT-DT.       CL*14
01490                                                                   EL175
01491      IF AT-CASH-PAYMENT = 'N'                                        CL*25
01492          MOVE SAVE-BIN-DATE      TO  AT-CHECK-WRITTEN-DT             CL*14
01493          MOVE PI-MONTH-END-SAVE  TO  AT-PMT-SELECT-DT.               CL*14
01494                                                                   EL175
01495      IF PI-COMPANY-ID = 'DMD' AND                                    CL*25
01496         EIBAID = DFHPF1                                              CL*17
01497          PERFORM 0900-GET-MASTER  THRU  0900-EXIT.                   CL*17
01498                                                                      CL*17
01499      IF PI-COMPANY-ID = 'DMD'                                        CL*25
01500          IF CQ-CHECK-NUMBER NOT NUMERIC                              CL*19
01501              IF EIBAID = DFHPF1                                      CL*19
01502                  PERFORM 1000-CREATE-DMD-DMO  THRU  1000-EXIT        CL*19
01503              ELSE                                                    CL*19
01504                  ADD +1 TO WS-DMD-DMO-COUNT                          CL*19
01505                  ADD AT-AMOUNT-PAID TO WS-DMD-DMO-AMOUNT             CL*19
01506          ELSE                                                        CL*19
01507              ADD +1 TO WS-DMD-PYMT-COUNT                             CL*19
01508              ADD AT-AMOUNT-PAID TO WS-DMD-PYMT-AMOUNT.               CL*19
01509                                                                      CL*17
01510      IF PI-COMPANY-ID = 'DMD' AND                                    CL*25
01511         EIBAID = DFHPF1                                              CL*17
01512          PERFORM 2000-CREATE-DMD-NOTE  THRU  2099-EXIT               CL*17
01513          IF AT-CLAIM-NO NOT = WS-PREV-CLAIM-NO                       CL*17
01514              PERFORM 2500-DELETE-DMD-NOTE  THRU  2599-EXIT.          CL*25
01515                                                                      CL*17
01516      IF EIBAID = DFHPF1                                              CL*13
01517          EXEC CICS REWRITE                                           CL*13
01518              DATASET (WS-ACTIVITY-TRAILERS-DSID)                     CL*13
01519              FROM    (ACTIVITY-TRAILERS)                             CL*13
01520          END-EXEC.                                                   CL*13
01521                                                                   EL175
01522      GO TO 0250-MAIN-LOGIC.                                       EL175
01523                                                                   EL175
01524      EJECT                                                        EL175
01525  0260-MAIN-LOGIC.                                                 EL175
01526      IF AQ-PAYMENT-COUNTER NOT GREATER THAN ZERO                  EL175
01527          MOVE SPACES             TO  AQ-PENDING-PAYMENT-FLAG.     EL175
01528                                                                   EL175
01529      MOVE +1750                  TO  AQ-LAST-UPDATED-BY.          EL175
01530                                                                   EL175
01531      IF WS-UPDATE-SW = ZERO                                       EL175
01532         EXEC CICS UNLOCK                                          EL175
01533              DATASET  (WS-ACTIVITY-QUE-DSID)                      EL175
01534         END-EXEC                                                     CL*25
01535         GO TO 0205-STARTBR-ELACTQ.                                   CL*12
01536                                                                      CL*12
01537      IF EIBAID = DFHPF1                                              CL*13
01538          IF AQ-PENDING-ACTIVITY-FLAGS = SPACES                       CL*13
01539              EXEC CICS DELETE                                        CL*13
01540                  DATASET (WS-ACTIVITY-QUE-DSID)                      CL*13
01541              END-EXEC                                                CL*13
01542           ELSE                                                       CL*19
01543              EXEC CICS REWRITE                                       CL*13
01544                  DATASET (WS-ACTIVITY-QUE-DSID)                      CL*13
01545                  FROM    (ACTIVITY-QUE)                              CL*13
01546              END-EXEC.                                               CL*13
01547                                                                   EL175
01548      GO TO 0205-STARTBR-ELACTQ.                                      CL*12
01549                                                                   EL175
01550      EJECT                                                        EL175
01551  0300-END-OF-SEARCH.                                                 CL*12
01552      IF WS-ELTRLR-BROWSE-SW = +1                                  EL175
01553          EXEC CICS ENDBR                                          EL175
01554              DATASET (WS-ACTIVITY-TRAILERS-DSID)                     CL*25
01555          END-EXEC.                                                   CL*25
01556                                                                   EL175
01557      IF WS-ELACTQ-BROWSE-SW = +1                                  EL175
01558          EXEC CICS ENDBR                                          EL175
01559              DATASET (WS-ACTIVITY-QUE-DSID)                          CL*25
01560          END-EXEC.                                                   CL*25
01561                                                                      CL*19
01562      IF PI-COMPANY-ID = 'DMD'                                        CL*25
01563          MOVE WS-CHECK-QUE-COUNTER                                   CL*19
01564                                  TO  WS-DMD-TL1-CONTROL-GROUP        CL*19
01565                                      PI-CK-CONTROL-NO                CL*19
01566                                      PI-END-CONTROL-NO               CL*19
01567          MOVE WS-RELEASED-COUNT  TO  WS-DMD-TL1-COUNT                CL*19
01568          MOVE WS-RELEASED-AMOUNT TO  WS-DMD-TL1-AMOUNT               CL*19
01569          MOVE WS-DMD-PYMT-COUNT  TO  WS-DMD-TL2-COUNT1               CL*19
01570          MOVE WS-DMD-PYMT-AMOUNT TO  WS-DMD-TL2-AMOUNT1              CL*19
01571          MOVE WS-DMD-DMO-COUNT   TO  WS-DMD-TL2-COUNT2               CL*19
01572          MOVE WS-DMD-DMO-AMOUNT  TO  WS-DMD-TL2-AMOUNT2              CL*19
01573          IF WS-DMD-PYMT-COUNT GREATER +1                             CL*25
01574              MOVE 'S'            TO  WS-DMD-TL2-PLURAL1              CL*19
01575          END-IF                                                      CL*27
01576          IF WS-DMD-DMO-COUNT NOT = +1                                CL*19
01577              MOVE 'S'            TO  WS-DMD-TL2-PLURAL2              CL*19
01578          END-IF                                                      CL*27
01579          GO TO 0301-MAIN-LOGIC.                                      CL*19
01580                                                                   EL175
01581      IF (PI-COMPANY-ID = 'AIG' OR 'AUK')                             CL*25
01582                    AND                                               CL*14
01583          AOPTIONI = '1'                                              CL*25
01584              NEXT SENTENCE                                           CL*14
01585          ELSE                                                        CL*14
01586              MOVE WS-CHECK-QUE-COUNTER   TO  WS-TL1-CONTROL-GROUP    CL*14
01587                                              PI-CK-CONTROL-NO        CL*14
01588                                              PI-END-CONTROL-NO       CL*14
01589              MOVE WS-RELEASED-COUNT      TO  WS-TL1-COUNT            CL*14
01590              MOVE WS-RELEASED-AMOUNT     TO  WS-TL1-AMOUNT           CL*14
031808             MOVE WS-UNAPPROVED-COUNT    TO  WS-TL3-COUNT
031808             MOVE WS-UNAPPROVED-AMOUNT   TO  WS-TL3-AMOUNT
01591              GO TO 0301-MAIN-LOGIC.                                  CL*14
01592                                                                      CL*14
01593      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01594          MOVE WS-CHECK-QUE-COUNTER   TO  WS-TL1-3-CONTROL-GROUP      CL*14
01595                                          PI-CK-CONTROL-NO            CL*14
01596          MOVE WS-RELEASED-COUNT      TO  WS-TL1-3-COUNT              CL*14
01597          MOVE WS-RELEASED-AMOUNT     TO  WS-TL1-3-AMOUNT             CL*14
01598          MOVE WS-CHECK-QUE-COUNTER-2 TO  WS-TL2-3-CONTROL-GROUP      CL*14
01599          MOVE WS-RELEASED-COUNT-2    TO  WS-TL2-3-COUNT              CL*14
01600          MOVE WS-RELEASED-AMOUNT-2   TO  WS-TL2-3-AMOUNT             CL*14
01601          MOVE WS-CHECK-QUE-COUNTER-3 TO  WS-TL3-3-CONTROL-GROUP      CL*14
01602                                          PI-END-CONTROL-NO           CL*14
01603          MOVE WS-RELEASED-COUNT-3    TO  WS-TL3-3-COUNT              CL*14
01604          MOVE WS-RELEASED-AMOUNT-3   TO  WS-TL3-3-AMOUNT.            CL*14
01605                                                                      CL*14
01606  0301-MAIN-LOGIC.                                                    CL*14
01607                                                                      CL*14
01608      MOVE WS-NON-CASH-REL-CNT        TO  PI-NON-CASH-REL-CNT.        CL*14
01609      MOVE WS-NON-CASH-REL-AMT        TO  PI-NON-CASH-REL-AMT.        CL*14
01610                                                                      CL*17
01611      IF PI-COMPANY-ID = 'DMD' AND                                    CL*25
01612         EIBAID = DFHPF1                                              CL*17
01613          PERFORM 2500-DELETE-DMD-NOTE  THRU  2599-EXIT.              CL*17
01614                                                                      CL*14
01615      IF PI-COMPANY-ID = 'AIG' OR 'AUK'                               CL*25
01616          GO TO 0302-MAIN-LOGIC.                                      CL*14
01617                                                                      CL*12
01618      IF WS-RELEASED-COUNT GREATER 1                                  CL*12
01619          MOVE 'S'                TO  WS-TL1-PLURAL                   CL*12
01620       ELSE                                                           CL*12
01621          MOVE ' '                TO  WS-TL1-PLURAL.                  CL*12
031808
031808     IF WS-UNAPPROVED-COUNT NOT EQUAL 1
031808         MOVE 'S'                TO  WS-TL3-PLURAL
031808     ELSE
031808         MOVE ' '                TO  WS-TL3-PLURAL
031808     END-IF.
01622                                                                      CL*12
01623      IF EIBAID = DFHPF1                                              CL*12
01624          MOVE LOW-VALUES         TO  EL175AI                         CL*19
01625              IF WS-RELEASED-COUNT > +0                                CL*27
031808               OR WS-UNAPPROVED-COUNT > +0
01626                  IF PI-COMPANY-ID = 'DMD'                            CL*19
01627                      MOVE ' RELEASED FOR '                           CL*19
01628                                  TO  WS-DMD-TL1-RELEASE              CL*19
01629                      MOVE WS-DMD-TOTAL-LINE1                         CL*19
01630                                  TO  EMI-MESSAGE-AREA (1)            CL*19
01631                      MOVE WS-DMD-TOTAL-LINE2                         CL*19
01632                                  TO  EMI-MESSAGE-AREA (2)            CL*19
01633                      GO TO 0301-RELEASE-CHECKS                       CL*19
01634                  ELSE                                                CL*19
01635                      MOVE 'RELEASED'                                 CL*19
01636                                  TO  WS-TL1-RELEASE                  CL*19
01637                      MOVE WS-TOTAL-LINE1                             CL*19
01638                                  TO  EMI-MESSAGE-AREA (1)            CL*19
01639                      MOVE WS-TOTAL-LINE2                             CL*19
01640                                  TO  EMI-MESSAGE-AREA (2)            CL*19
031808                     MOVE WS-TOTAL-LINE3
031808                                 TO  EMI-MESSAGE-AREA (3)
01641                      GO TO 0301-RELEASE-CHECKS                       CL*19
01642              ELSE                                                    CL*19
01643                  IF WS-NON-CASH-REL-CNT > +0                         CL*27
01644                      MOVE WS-NON-CASH-REL-CNT                        CL*19
01645                                  TO  WS-NC-TL1-COUNT                 CL*19
01646                      MOVE ' NON CASH CHECKS RELEASED'                CL*19
01647                                  TO  WS-NC-TL1-LIT                   CL*19
01648                      MOVE WS-NON-CASH-TOTAL-LINE                     CL*19
01649                                  TO  EMI-MESSAGE-AREA (1)            CL*19
01650                      GO TO 0301-RELEASE-CHECKS.                      CL*19
01651                                                                      CL*15
01652      IF WS-RELEASED-COUNT > +0                                       CL*27
031808       OR WS-UNAPPROVED-COUNT > +0
01653          IF PI-COMPANY-ID = 'DMD'                                    CL*19
01654              MOVE ' TO BE RELEASED FOR'                              CL*19
01655                                  TO  WS-DMD-TL1-RELEASE              CL*19
01656              MOVE WS-DMD-TOTAL-LINE1                                 CL*19
01657                                  TO  EMI-MESSAGE-AREA (1)            CL*19
01658              MOVE WS-DMD-TOTAL-LINE2                                 CL*19
01659                                  TO  EMI-MESSAGE-AREA (2)            CL*19
01660              GO TO 0301-RELEASE-CHECKS                               CL*19
01661          ELSE                                                        CL*19
01662              MOVE 'TO BE RELEASED'                                   CL*19
01663                                  TO  WS-TL1-RELEASE                  CL*19
01664              MOVE WS-TOTAL-LINE1 TO  EMI-MESSAGE-AREA (1)            CL*27
01665              MOVE WS-TOTAL-LINE2 TO  EMI-MESSAGE-AREA (2)            CL*27
031808             MOVE WS-TOTAL-LINE3 TO  EMI-MESSAGE-AREA (3)
01666              GO TO 0301-RELEASE-CHECKS.                              CL*19
01667                                                                      CL*12
01668      IF WS-NON-CASH-REL-CNT > ZERO                                   CL*27
01669          MOVE WS-NON-CASH-REL-CNT        TO  WS-NC-TL1-COUNT         CL*15
01670          MOVE ' NON CASH CHECKS TO BE RELEASED'                      CL*15
01671                                          TO  WS-NC-TL1-LIT           CL*15
01672          MOVE WS-NON-CASH-TOTAL-LINE     TO  EMI-MESSAGE-AREA (1)    CL*15
01673          GO TO 0301-RELEASE-CHECKS.                                  CL*15
01674                                                                      CL*15
01675      MOVE +0                             TO  PI-PROC-SW.             CL*15
01676      MOVE -1                             TO  AOPTIONL.               CL*15
01677      MOVE ER-3048                        TO  EMI-ERROR.              CL*15
01678      GO TO 8200-SEND-DATAONLY.                                       CL*15
01679                                                                      CL*15
01680  0301-RELEASE-CHECKS.                                                CL*15
01681                                                                   EL175
01682      IF EIBAID = DFHPF1                                              CL*12
111502*        PERFORM 7000-PRINT-CHECKS-WAITING                           CL*12
01684          GO TO 0015-INITIALIZE                                       CL*12
01685        ELSE                                                          CL*12
031808        IF WS-RELEASED-COUNT > +0
01686          MOVE +1                 TO PI-PROC-SW                       CL*12
031808        ELSE
031808         MOVE +0                 TO PI-PROC-SW
031808        END-IF
090309        IF WS-STOP-ALL-CHECKS
090309         MOVE +2                 TO PI-PROC-SW
090309        END-IF
01687          MOVE -1                 TO AOPTIONL                         CL*12
01688          GO TO 8200-SEND-DATAONLY.                                   CL*13
01689                                                                   EL175
01690  0302-MAIN-LOGIC.                                                    CL*14
01691                                                                      CL*14
01692      IF EIBAID = DFHPF1                                              CL*14
01693        MOVE LOW-VALUES                   TO  EL175AI                 CL*15
01694        IF WS-RELEASED-COUNT   > +0 OR                                CL*27
01695           WS-RELEASED-COUNT-2 > +0 OR                                CL*27
01696           WS-RELEASED-COUNT-3 > +0                                   CL*27
01697            MOVE 'RELEASED'               TO  WS-TL-1-3-RELEASE       CL*15
01698            MOVE WS-TOTAL-LINE-1-3        TO  EMI-MESSAGE-AREA (1)    CL*15
01699            MOVE WS-TOTAL-LINE-2-3        TO  EMI-MESSAGE-AREA (2)    CL*15
01700            GO TO 0302-RELEASE-CHECKS                                 CL*15
01701        ELSE                                                          CL*15
01702          IF WS-NON-CASH-REL-CNT > +0                                 CL*27
01703            MOVE WS-NON-CASH-REL-CNT      TO  WS-NC-TL1-COUNT         CL*15
01704            MOVE ' NON CASH CHECKS RELEASED'  TO  WS-NC-TL1-LIT       CL*15
01705            MOVE WS-NON-CASH-TOTAL-LINE   TO  EMI-MESSAGE-AREA (1)    CL*15
01706            GO TO 0302-RELEASE-CHECKS.                                CL*15
01707                                                                      CL*14
01708      IF WS-RELEASED-COUNT   > +0 OR                                  CL*27
01709         WS-RELEASED-COUNT-2 > +0 OR                                  CL*27
01710         WS-RELEASED-COUNT-3 > +0                                     CL*27
01711          MOVE 'TO BE RELEASED'           TO  WS-TL-1-3-RELEASE       CL*15
01712          MOVE WS-TOTAL-LINE-1-3          TO  EMI-MESSAGE-AREA (1)    CL*15
01713          MOVE WS-TOTAL-LINE-2-3          TO  EMI-MESSAGE-AREA (2)    CL*15
01714          GO TO 0302-RELEASE-CHECKS.                                  CL*15
01715                                                                      CL*15
01716      IF WS-NON-CASH-REL-CNT > +0                                     CL*27
01717          MOVE WS-NON-CASH-REL-CNT        TO  WS-NC-TL1-COUNT         CL*15
01718          MOVE ' NON CASH CHECKS TO BE RELEASED '                     CL*15
01719                                          TO  WS-NC-TL1-LIT           CL*15
01720          MOVE WS-NON-CASH-TOTAL-LINE     TO  EMI-MESSAGE-AREA (1)    CL*15
01721          GO TO 0302-RELEASE-CHECKS.                                  CL*15
01722                                                                      CL*15
01723      MOVE +0                             TO  PI-PROC-SW.             CL*15
01724      MOVE -1                             TO  AOPTIONL.               CL*15
01725      MOVE ER-3048                        TO  EMI-ERROR.              CL*15
01726      GO TO 8200-SEND-DATAONLY.                                       CL*15
01727                                                                      CL*15
01728  0302-RELEASE-CHECKS.                                                CL*15
01729                                                                      CL*14
01730      IF EIBAID = DFHPF1                                              CL*14
111502*        PERFORM 7000-PRINT-CHECKS-WAITING                           CL*14
01732          GO TO 0015-INITIALIZE                                       CL*14
01733      ELSE                                                            CL*14
01734          MOVE +1                     TO  PI-PROC-SW                  CL*14
01735          MOVE -1                     TO  AOPTIONL                    CL*14
01736          GO TO 8200-SEND-DATAONLY.                                   CL*14
01737                                                                      CL*14
01738      EJECT                                                        EL175
01739  0910-ENQ-BUSY.                                                   EL175
01740 *    NOTE ******************************************************* EL175
01741 *         *      IF ONE OF THE OTHER PROGRAMS (EL176 OR EL177)  * EL175
01742 *         *  HAS EXCLUSIVE CONTROL OF THE CHECK QUEUE DSID,     *    CL*12
01743 *         *  SEND A MESSAGE TO THE OPERATOR TO WAIT A FEW       *    CL*12
01744 *         *  MOMENTS AND TRY AGAIN.                             * EL175
01745 *         *******************************************************.EL175
01746                                                                   EL175
01747      MOVE ER-0395                TO  EMI-ERROR.                   EL175
01748      MOVE -1                     TO  AOPTIONL.                    EL175
01749      GO TO 8200-SEND-DATAONLY.                                       CL*13
01750                                                                   EL175
CIDMOD     EJECT                                                        EL175
CIDMOD 0920-WRITE-DLYACTV.                                              EL1CL*17
CIDMOD     MOVE SPACES                 TO DAILY-ACTIVITY-RECORD.        00007125
CIDMOD     MOVE AT-CONTROL-PRIMARY     TO DA-KEY                             000
CIDMOD     MOVE AT-SEQUENCE-NO         TO DA-TRAILER-SEQ-NO                  000
PEMMOD     MOVE 'P'                    TO DA-RECORD-TYPE
CIDMOD
CIDMOD     EXEC CICS HANDLE CONDITION                                   00007136
CIDMOD          NOTOPEN(0920-NOTOPEN)                                   00007137
CIDMOD          DUPREC(0920-EXIT)                                       00007138
CIDMOD     END-EXEC.                                                    00007139
CIDMOD
CIDMOD     EXEC CICS WRITE                                              00007140
CIDMOD          DATASET('DLYACTV')                                      00007141
CIDMOD          RIDFLD(DA-KEY)                                          00007142
CIDMOD          FROM(DAILY-ACTIVITY-RECORD)                             00007143
CIDMOD          LENGTH(25)                                              00007144
CIDMOD     END-EXEC.                                                    00007145
CIDMOD     GO TO 0920-EXIT.                                             00007148
CIDMOD                                                                  00007149
CIDMOD 0920-NOTOPEN.                                                    00007150
CIDMOD     MOVE '2955' TO  EMI-ERROR                                       CL*19
CIDMOD     GO TO 8200-SEND-DATAONLY.                                       CL*19
CIDMOD                                                                  EL1CL*17
CIDMOD 0920-EXIT.                                                       EL1CL*17
CIDMOD     EXIT.                                                        EL1CL*17
CIDMOD                                                                  EL1CL*17
01751      EJECT                                                        EL175
01752 ******************************************************************   CL*17
01753 *    DMD ONLY  -  READ ELMSTR IF NOT AVAILABLE                       CL*27
01754 ******************************************************************   CL*17
01755  0900-GET-MASTER.                                                    CL*17
01756                                                                      CL*17
01757 *    IF AT-CARRIER  = CL-CARRIER  AND                                CL*28
01758 *       AT-CLAIM-NO = CL-CLAIM-NO                                    CL*28
01759 *        GO TO 0900-EXIT.                                            CL*28
01760                                                                      CL*17
01761      MOVE AT-CONTROL-PRIMARY     TO WS-ELMSTR-KEY.                   CL*17
01762                                                                      CL*17
01763      EXEC CICS READ                                                  CL*17
01764           DATASET   ('ELMSTR')                                       CL*17
01765           RIDFLD    (WS-ELMSTR-KEY)                                  CL*17
01766           SET       (ADDRESS OF CLAIM-MASTER)                        CL*17
01767      END-EXEC.                                                       CL*17
01768                                                                      CL*17
01769  0900-EXIT.                                                          CL*17
01770      EXIT.                                                           CL*17
01771                                  EJECT                               CL*17
01772 ******************************************************************   CL*17
01773 *    DMD ONLY  -  CREATE THE DIRECT MARKETING OUTPUT FILE            CL*17
01774 ******************************************************************   CL*17
01775  1000-CREATE-DMD-DMO.                                                CL*17
01776                                                                      CL*17
01777      MOVE CL-CERT-KEY-DATA       TO W-NOTE-CERT-KEY.                 CL*18
01778      MOVE PI-COMPANY-CD          TO W-NOTE-COMP-CD.                  CL*18
01779      MOVE CL-CERT-NO             TO W-NOTE-CERT-NO.                  CL*18
01780                                                                      CL*18
01781      EXEC CICS HANDLE CONDITION                                      CL*18
01782           NOTFND   (1000-NOTE-NOT-FOUND)                             CL*18
01783           NOTOPEN  (1000-NOTE-NOT-OPEN)                              CL*19
01784      END-EXEC.                                                       CL*25
01785                                                                      CL*18
01786      EXEC CICS READ                                                  CL*18
01787           DATASET('ERNOTE')                                          CL*18
01788           SET    (ADDRESS OF CERTIFICATE-NOTE)                       CL*18
01789           RIDFLD (W-NOTE-KEY)                                        CL*18
01790      END-EXEC.                                                       CL*25
01791                                                                      CL*18
01792      MOVE SPACES                 TO DCT-COMMUNICATION-AREA.          CL*18
01793      MOVE CL-BENEFICIARY         TO DCT-LOGIC-BENEFICIARY-ID.        CL*18
01794      MOVE CL-CCN                 TO DCT-CREDIT-CARD-NUMBER.          CL*18
01795                                                                      CL*18
01796      IF CL-CERT-GROUPING (5:2) = ZEROS OR SPACES                     CL*20
01797          MOVE 'CC'               TO DCT-PRODUCT-CODE                 CL*18
01798      ELSE                                                            CL*18
01799          MOVE CL-CERT-GROUPING (5:2) TO DCT-PRODUCT-CODE.            CL*20
01800                                                                      CL*18
01801      MOVE CN-CSI-CC-BILL-BANK-ID TO DCT-BILLING-BANK-ID.             CL*18
01802      MOVE '01'                   TO DCT-COLUMN-ID-REQUESTED.         CL*23
01803                                                                      CL*18
01804      EXEC CICS LINK                                                  CL*18
01805          PROGRAM    ('DLO006')                                       CL*18
01806          COMMAREA   (DCT-COMMUNICATION-AREA)                         CL*18
01807          LENGTH     (WS-DCT-LENGTH)                                  CL*18
01808      END-EXEC.                                                       CL*18
01809                                                                      CL*17
01810      IF DCT-RETURN-CODE = 'OK'                                       CL*25
01811          GO TO 1000-CONT.                                            CL*18
01812                                                                      CL*18
01813      IF DCT-RETURN-CODE = '01'                                       CL*25
01814          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*25
01815          MOVE ER-0910            TO EMI-ERROR                        CL*25
01816          MOVE -1                 TO AOPTIONL                         CL*25
01817          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*25
01818          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01819          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01820          GO TO 8200-SEND-DATAONLY.                                   CL*25
01821                                                                      CL*17
01822      IF DCT-RETURN-CODE = '02'                                       CL*25
01823          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*25
01824          MOVE ER-0913            TO EMI-ERROR                        CL*25
01825          MOVE -1                 TO AOPTIONL                         CL*25
01826          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*25
01827          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01828          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01829          GO TO 8200-SEND-DATAONLY.                                   CL*25
01830                                                                      CL*25
01831      IF DCT-RETURN-CODE = '03'                                       CL*25
01832          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01833          MOVE ER-0951            TO EMI-ERROR                        CL*18
01834          MOVE -1                 TO AOPTIONL                         CL*18
01835          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01836          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01837          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01838          GO TO 8200-SEND-DATAONLY.                                   CL*18
01839                                                                      CL*18
01840      IF DCT-RETURN-CODE = '04'                                       CL*25
01841          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01842          MOVE ER-0946            TO EMI-ERROR                        CL*18
01843          MOVE -1                 TO AOPTIONL                         CL*17
01844          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
01845          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01846          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01847          GO TO 8200-SEND-DATAONLY.                                   CL*17
01848                                                                      CL*17
01849      IF DCT-RETURN-CODE = '05'                                       CL*25
01850          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01851          MOVE ER-0947            TO EMI-ERROR                        CL*18
01852          MOVE -1                 TO AOPTIONL                         CL*18
01853          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01854          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01855          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01856          GO TO 8200-SEND-DATAONLY.                                   CL*18
01857                                                                      CL*17
01858      IF DCT-RETURN-CODE = '06'                                       CL*25
01859          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01860          MOVE ER-0921            TO EMI-ERROR                        CL*23
01861          MOVE -1                 TO AOPTIONL                         CL*23
01862          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
01863          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01864          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01865          GO TO 8200-SEND-DATAONLY.                                   CL*23
01866                                                                      CL*23
01867      IF DCT-RETURN-CODE = '07'                                       CL*25
01868          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01869          MOVE ER-0919            TO EMI-ERROR                        CL*23
01870          MOVE -1                 TO AOPTIONL                         CL*23
01871          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*23
01872          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01873          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01874          GO TO 8200-SEND-DATAONLY.                                   CL*23
01875                                                                      CL*23
01876      IF DCT-RETURN-CODE = '08'                                       CL*25
01877          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01878          MOVE ER-0948            TO EMI-ERROR                        CL*18
01879          MOVE -1                 TO AOPTIONL                         CL*18
01880          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01881          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01882          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01883          GO TO 8200-SEND-DATAONLY.                                   CL*18
01884                                                                      CL*18
01885      IF DCT-RETURN-CODE = 'N1'                                       CL*25
01886          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01887          MOVE ER-0950            TO EMI-ERROR                        CL*18
01888          MOVE -1                 TO AOPTIONL                         CL*18
01889          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01890          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01891          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01892          GO TO 8200-SEND-DATAONLY.                                   CL*18
01893                                                                      CL*18
01894      IF DCT-RETURN-CODE = 'E1'                                       CL*21
01895          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01896          MOVE ER-0974            TO EMI-ERROR                        CL*21
01897          MOVE -1                 TO AOPTIONL                         CL*21
01898          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*21
01899          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01900          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01901          GO TO 8200-SEND-DATAONLY.                                   CL*21
01902                                                                      CL*21
01903      IF DCT-RETURN-CODE = 'E2'                                       CL*21
01904          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01905          MOVE ER-0975            TO EMI-ERROR                        CL*21
01906          MOVE -1                 TO AOPTIONL                         CL*21
01907          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*21
01908          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01909          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01910          GO TO 8200-SEND-DATAONLY.                                   CL*21
01911                                                                      CL*21
01912      IF DCT-RETURN-CODE NOT = 'OK'                                   CL*21
01913          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01914          MOVE ER-0949            TO EMI-ERROR                        CL*18
01915          MOVE -1                 TO AOPTIONL                         CL*18
01916          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01917          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01918          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01919          GO TO 8200-SEND-DATAONLY.                                   CL*18
01920                                                                      CL*18
01921  1000-CONT.                                                          CL*18
01922                                                                      CL*18
01923      MOVE 'Y'                    TO WS-DMD-TAPE-PYMT-SW.             CL*19
01924                                                                      CL*19
01925      ADD +1 TO WS-DMD-DMO-COUNT.                                     CL*19
01926      ADD AT-AMOUNT-PAID TO WS-DMD-DMO-AMOUNT.                        CL*19
01927                                                                      CL*19
01928      MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                CL*19
01929      MOVE '5'                    TO  DC-OPTION-CODE.                 CL*19
01930      PERFORM 8500-DATE-CONVERSION.                                   CL*17
01931      MOVE DC-GREG-DATE-1-YMD     TO DMD-YYMMDD.                      CL*17
01932      MOVE DC-BIN-DATE-1          TO AT-CHECK-WRITTEN-DT.             CL*19
01933                                                                      CL*17
01934      IF DMD-YY GREATER THAN 70                                       CL*17
01935          MOVE '19'               TO DMD-DECADE                       CL*17
01936      ELSE                                                            CL*17
01937          MOVE '20'               TO DMD-DECADE.                      CL*17
01938                                                                      CL*19
01939      MOVE +88888888              TO AT-CHECK-QUE-CONTROL.            CL*19
01940      MOVE +8888                  TO AT-CHECK-QUE-SEQUENCE.           CL*19
01941                                                                      CL*17
01942      MOVE SPACES                 TO DMO-COMMUNICATION-AREA.          CL*18
01943      MOVE 'DR'                   TO DM-RECORD-TYPE.                  CL*17
01944      MOVE DCT-DISTRIBUTION-CODE  TO DM-DIST-CODE.                    CL*17
01945      MOVE DCT-MAIL-CODE          TO DM-MAIL-CODE.                    CL*18
01946      MOVE CL-CLAIM-NO            TO DM-CLAIM-NO.                     CL*18
01947      MOVE CL-CERT-NO (4:1)       TO DM-CLAIM-TYPE.                   CL*23
01948      MOVE CL-CARRIER             TO DM-CARRIER.                      CL*23
01949      MOVE CL-CCN                 TO DM-CREDIT-CARD-NUMBER.           CL*18
01950      MOVE DMD-DATE-YYYYMMDD      TO DM-PAYMENT-DATE.                 CL*18
01951                                                                      CL*18
01952      MOVE CL-INSURED-LAST-NAME   TO W-NAME-LAST.                     CL*17
01953      MOVE CL-INSURED-1ST-NAME    TO W-NAME-FIRST.                    CL*17
01954      MOVE CL-INSURED-MID-INIT    TO W-NAME-MIDDLE.                   CL*17
01955      PERFORM 1100-FORMAT-LAST-NAME-1ST THRU 1100-EXIT.               CL*17
01956      MOVE WS-NAME-WORK           TO DM-INSURED-NAME.                 CL*18
01957                                                                      CL*18
01958      MOVE AT-PAYMENT-TYPE        TO DM-PAYMENT-TYPE.                 CL*17
01959      MOVE AT-AMOUNT-PAID         TO DM-PAYMENT-AMT.                  CL*18
01960      MOVE AT-CERT-NO             TO DM-CERT-NO.                      CL*17
01961      MOVE AT-SEQUENCE-NO         TO DM-TRLR-SEQ-NO.                  CL*17
01962                                                                      CL*17
01963      EXEC CICS LINK                                                  CL*18
01964          PROGRAM    ('DLO025')                                       CL*18
01965          COMMAREA   (DMO-COMMUNICATION-AREA)                         CL*18
01966          LENGTH     (WS-DMO-LENGTH)                                  CL*18
01967      END-EXEC.                                                       CL*18
01968                                                                      CL*17
01969      IF DM-RETURN-CODE = 'OK'                                        CL*25
01970          GO TO 1000-EXIT.                                            CL*18
01971                                                                      CL*17
01972      IF DM-RETURN-CODE = '01'                                        CL*25
01973          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01974          MOVE ER-8051            TO EMI-ERROR                        CL*18
01975          MOVE -1                 TO AOPTIONL                         CL*17
01976          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*17
01977          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01978          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01979          GO TO 8200-SEND-DATAONLY.                                   CL*17
01980                                                                      CL*18
01981      IF DM-RETURN-CODE = '02'                                        CL*25
01982          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01983          MOVE ER-8052            TO EMI-ERROR                        CL*18
01984          MOVE -1                 TO AOPTIONL                         CL*18
01985          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01986          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01987          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01988          GO TO 8200-SEND-DATAONLY.                                   CL*18
01989                                                                      CL*18
01990      IF DM-RETURN-CODE = '03'                                        CL*25
01991          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
01992          MOVE ER-8053            TO EMI-ERROR                        CL*18
01993          MOVE -1                 TO AOPTIONL                         CL*18
01994          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
01995          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
01996          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
01997          GO TO 8200-SEND-DATAONLY.                                   CL*18
01998                                                                      CL*18
01999      IF DM-RETURN-CODE = '04'                                        CL*25
02000          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02001          MOVE ER-8054            TO EMI-ERROR                        CL*18
02002          MOVE -1                 TO AOPTIONL                         CL*18
02003          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02004          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02005          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02006          GO TO 8200-SEND-DATAONLY.                                   CL*18
02007                                                                      CL*18
02008      IF DM-RETURN-CODE = '05'                                        CL*25
02009          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02010          MOVE ER-8055            TO EMI-ERROR                        CL*18
02011          MOVE -1                 TO AOPTIONL                         CL*18
02012          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02013          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02014          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02015          GO TO 8200-SEND-DATAONLY.                                   CL*18
02016                                                                      CL*18
02017      IF DM-RETURN-CODE = '06'                                        CL*25
02018          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02019          MOVE ER-8056            TO EMI-ERROR                        CL*18
02020          MOVE -1                 TO AOPTIONL                         CL*18
02021          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02022          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02023          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02024          GO TO 8200-SEND-DATAONLY.                                   CL*18
02025                                                                      CL*18
02026      IF DM-RETURN-CODE = '07'                                        CL*25
02027          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02028          MOVE ER-8057            TO EMI-ERROR                        CL*18
02029          MOVE -1                 TO AOPTIONL                         CL*18
02030          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02031          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02032          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02033          GO TO 8200-SEND-DATAONLY.                                   CL*18
02034                                                                      CL*18
02035      IF DM-RETURN-CODE = '08'                                        CL*25
02036          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02037          MOVE ER-8058            TO EMI-ERROR                        CL*18
02038          MOVE -1                 TO AOPTIONL                         CL*18
02039          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02040          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02041          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02042          GO TO 8200-SEND-DATAONLY.                                   CL*18
02043                                                                      CL*18
02044      IF DM-RETURN-CODE = '09'                                        CL*25
02045          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02046          MOVE ER-8059            TO EMI-ERROR                        CL*18
02047          MOVE -1                 TO AOPTIONL                         CL*18
02048          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02049          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02050          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02051          GO TO 8200-SEND-DATAONLY.                                   CL*18
02052                                                                      CL*18
02053      IF DM-RETURN-CODE = '10'                                        CL*25
02054          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02055          MOVE ER-8060            TO EMI-ERROR                        CL*18
02056          MOVE -1                 TO AOPTIONL                         CL*18
02057          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02058          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02059          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02060          GO TO 8200-SEND-DATAONLY.                                   CL*18
02061                                                                      CL*18
02062      IF DM-RETURN-CODE = '11'                                        CL*25
02063          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02064          MOVE ER-8061            TO EMI-ERROR                        CL*18
02065          MOVE -1                 TO AOPTIONL                         CL*18
02066          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02067          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02068          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02069          GO TO 8200-SEND-DATAONLY.                                   CL*18
02070                                                                      CL*18
02071      IF DM-RETURN-CODE = '12'                                        CL*25
02072          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02073          MOVE ER-8062            TO EMI-ERROR                        CL*18
02074          MOVE -1                 TO AOPTIONL                         CL*18
02075          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02076          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02077          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02078          GO TO 8200-SEND-DATAONLY.                                   CL*18
02079                                                                      CL*18
02080      IF DM-RETURN-CODE = '13'                                        CL*25
02081          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02082          MOVE ER-8063            TO EMI-ERROR                        CL*18
02083          MOVE -1                 TO AOPTIONL                         CL*18
02084          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02085          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02086          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02087          GO TO 8200-SEND-DATAONLY.                                   CL*18
02088                                                                      CL*18
02089      IF DM-RETURN-CODE = '14'                                        CL*25
02090          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02091          MOVE ER-8064            TO EMI-ERROR                        CL*18
02092          MOVE -1                 TO AOPTIONL                         CL*18
02093          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02094          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02095          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02096          GO TO 8200-SEND-DATAONLY.                                   CL*18
02097                                                                      CL*18
02098      IF DM-RETURN-CODE = '15'                                        CL*25
02099          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02100          MOVE ER-8065            TO EMI-ERROR                        CL*18
02101          MOVE -1                 TO AOPTIONL                         CL*18
02102          PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                    CL*18
02103          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*25
02104          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*25
02105          GO TO 8200-SEND-DATAONLY.                                   CL*20
02106                                                                      CL*20
02107      IF DM-RETURN-CODE = '16'                                        CL*23
02108          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02109          MOVE ER-8154            TO EMI-ERROR                        CL*23
02110          MOVE -1                 TO AOPTIONL                         CL*23
02111          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT               CL*23
02112          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*23
02113          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*23
02114          GO TO 8200-SEND-DATAONLY.                                   CL*23
02115                                                                      CL*23
02116      IF DM-RETURN-CODE = '17'                                        CL*23
02117          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02118          MOVE ER-8155            TO EMI-ERROR                        CL*23
02119          MOVE -1                 TO AOPTIONL                         CL*23
02120          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT               CL*23
02121          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*23
02122          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*23
02123          GO TO 8200-SEND-DATAONLY.                                   CL*23
02124                                                                      CL*23
02125      IF DM-RETURN-CODE = 'N1'                                        CL*20
02126          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02127          MOVE ER-8152            TO EMI-ERROR                        CL*20
02128          MOVE -1                 TO AOPTIONL                         CL*20
02129          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT               CL*20
02130          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*20
02131          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*20
02132          GO TO 8200-SEND-DATAONLY.                                   CL*20
02133                                                                      CL*20
02134      IF DM-RETURN-CODE = 'E1'                                        CL*20
02135          EXEC CICS SYNCPOINT ROLLBACK END-EXEC                       CL*23
02136          MOVE ER-8153            TO EMI-ERROR                        CL*20
02137          MOVE -1                 TO AOPTIONL                         CL*20
02138          PERFORM 9900-ERROR-FORMAT      THRU 9900-EXIT               CL*20
02139          PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT               CL*20
02140          PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT               CL*20
02141          GO TO 8200-SEND-DATAONLY.                                   CL*18
02142                                                                      CL*18
02143      EXEC CICS SYNCPOINT ROLLBACK END-EXEC.                          CL*23
02144      MOVE ER-8066                TO EMI-ERROR.                       CL*18
02145      MOVE -1                     TO AOPTIONL.                        CL*18
02146      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
02147      PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT.                  CL*25
02148      PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT.                  CL*25
02149      GO TO 8200-SEND-DATAONLY.                                       CL*18
02150                                                                      CL*18
02151  1000-NOTE-NOT-FOUND.                                                CL*18
02152                                                                      CL*18
02153      MOVE ER-0954                TO EMI-ERROR.                       CL*18
02154      MOVE -1                     TO AOPTIONL.                        CL*18
02155      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                       CL*18
02156      PERFORM 1200-UNLOCK-CLAIM-MSTR THRU 1200-EXIT.                  CL*25
02157      PERFORM 1250-UNLOCK-TRLR       THRU 1250-EXIT.                  CL*25
02158      GO TO 8200-SEND-DATAONLY.                                       CL*18
02159                                                                      CL*17
02160  1000-NOTE-NOT-OPEN.                                                 CL*19
02161                                                                      CL*19
02162      MOVE ER-8135                TO  EMI-ERROR.                      CL*19
02163      MOVE -1                     TO  APFKL.                          CL*19
02164      GO TO 8200-SEND-DATAONLY.                                       CL*19
02165                                                                      CL*19
02166  1000-EXIT.                                                          CL*17
02167      EXIT.                                                           CL*17
02168                                                                      CL*18
02169  1100-FORMAT-LAST-NAME-1ST.                                          CL*17
02170 *****************************************************************    CL*17
02171 *             M O V E   N A M E   R O U T I N E                 *    CL*17
02172 *     THE FOLLOWING ROUTINE REARRANGES A GIVEN NAME SO          *    CL*17
02173 *     THAT IT READS LAST, FIRST, MIDDLE.  PLACE NAME            *    CL*17
02174 *     FIELDS IN THE FOLLOWING WORKING STORAGE FIELDS.           *    CL*17
02175 *                  FIELD                   VALUE                *    CL*17
02176 *           W-NAME-LAST    (CL15)      SMITH                    *    CL*17
02177 *           W-NAME-FIRST   (CL15)      JOHN                     *    CL*17
02178 *           W-NAME-MIDDLE  (CL15)      ALLEN/A                  *    CL*17
02179 *     AFTER NAME HAS BEEN MOVED WS-NAME-WORK (CL30) WILL        *    CL*17
02180 *     CONTAIN                                                   *    CL*17
02181 *                SMITH, JOHN ALLEN                              *    CL*17
02182 *     OR                                                        *    CL*17
02183 *                SMITH, JOHN A.                                 *    CL*17
02184 *     TO USE THIS ROUTINE YOU NEED THE WORKING STORAGE          *    CL*17
02185 *     COPYBOOK, ELCNWA.                                         *    CL*17
02186 *****************************************************************.   CL*17
02187                                                                      CL*17
02188      MOVE SPACES                 TO  WS-NAME-WORK-AREA.              CL*17
02189      MOVE ZERO                   TO  WS-NAME-SW.                     CL*17
02190      SET NWA-INDEX               TO +1.                              CL*17
02191                                                                      CL*17
02192      IF W-NAME-LAST = SPACES                                         CL*27
02193              AND                                                     CL*17
02194         W-NAME-MIDDLE = SPACES                                       CL*27
02195          MOVE +1                 TO WS-NAME-SW.                      CL*17
02196                                                                      CL*17
02197      MOVE W-NAME-LAST            TO WS-NAME-WORK2.                   CL*17
02198      PERFORM 1150-MOVE-NAME THRU 1150-EXIT.                          CL*17
02199                                                                      CL*17
02200      MOVE W-NAME-FIRST           TO WS-NAME-WORK2.                   CL*17
02201      PERFORM 1150-MOVE-NAME THRU 1150-EXIT.                          CL*17
02202                                                                      CL*17
02203      SET NWA-INDEX UP BY +1.                                         CL*17
02204                                                                      CL*17
02205      IF W-NAME-MIDDLE NOT = SPACES                                   CL*27
02206          IF W-NAME-MIDDLE-2 = SPACES                                 CL*27
02207              MOVE W-NAME-MIDDLE  TO WS-NW (NWA-INDEX)                CL*17
02208              SET NWA-INDEX UP BY +1                                  CL*17
02209              MOVE '.'            TO WS-NW (NWA-INDEX)                CL*17
02210              SET NWA-INDEX UP BY +2                                  CL*17
02211          ELSE                                                        CL*17
02212              MOVE W-NAME-MIDDLE  TO WS-NAME-WORK2                    CL*17
02213              PERFORM 1150-MOVE-NAME THRU 1150-EXIT.                  CL*17
02214                                                                      CL*17
02215  1100-EXIT.                                                          CL*17
02216      EXIT.                                                           CL*17
02217                                  EJECT                               CL*17
02218  1150-MOVE-NAME.                                                     CL*17
02219                                                                      CL*17
02220      IF WS-NAME-SW > +1                                              CL*27
02221          GO TO 1150-EXIT.                                            CL*17
02222                                                                      CL*17
02223      IF WS-NAME-WORK2 = SPACES                                       CL*25
02224          GO TO 1150-EXIT.                                            CL*17
02225                                                                      CL*17
02226      SET NWA-INDEX2            TO +1.                                CL*17
02227      SET NWA-INDEX3            TO +2.                                CL*17
02228                                                                      CL*17
02229  1150-MOVE-NAME-CYCLE.                                               CL*17
02230                                                                      CL*17
02231      MOVE WS-NW2 (NWA-INDEX2)  TO  WS-NW (NWA-INDEX).                CL*17
02232                                                                      CL*17
02233      IF NWA-INDEX < +30                                              CL*27
02234          SET NWA-INDEX UP BY +1                                      CL*17
02235      ELSE                                                            CL*17
02236          ADD +2                TO  WS-NAME-SW                        CL*17
02237          GO TO 1150-EXIT.                                            CL*17
02238                                                                      CL*17
02239      IF NWA-INDEX2 < +20                                             CL*27
02240          SET NWA-INDEX3 UP BY +1                                     CL*17
02241          SET NWA-INDEX2 UP BY +1.                                    CL*17
02242                                                                      CL*17
02243      IF WS-NW2 (NWA-INDEX2) = SPACES                                 CL*27
02244              AND                                                     CL*17
02245         WS-NW2 (NWA-INDEX3) = SPACES                                 CL*27
02246          IF WS-NAME-SW = ZERO                                        CL*27
02247              MOVE ','            TO  WS-NW (NWA-INDEX)               CL*17
02248              SET NWA-INDEX UP BY +2                                  CL*17
02249              MOVE +1             TO  WS-NAME-SW                      CL*17
02250              GO TO 1150-EXIT                                         CL*17
02251          ELSE                                                        CL*17
02252              GO TO 1150-EXIT.                                        CL*17
02253                                                                      CL*17
02254      GO TO 1150-MOVE-NAME-CYCLE.                                     CL*17
02255                                                                      CL*17
02256  1150-EXIT.                                                          CL*17
02257      EXIT.                                                           CL*17
02258                                  EJECT                               CL*17
02259  1200-UNLOCK-CLAIM-MSTR.                                             CL*17
02260       EXEC CICS UNLOCK                                               CL*17
02261            DATASET  (WS-ACTIVITY-QUE-DSID)                           CL*17
02262       END-EXEC.                                                      CL*23
02263                                                                      CL*23
02264  1200-EXIT.                                                          CL*17
02265      EXIT.                                                           CL*17
02266                                                                      CL*23
02267  1250-UNLOCK-TRLR.                                                   CL*17
02268       EXEC CICS UNLOCK                                               CL*17
02269            DATASET (WS-ACTIVITY-TRAILERS-DSID)                       CL*17
02270       END-EXEC.                                                      CL*23
02271                                                                      CL*23
02272  1250-EXIT.                                                          CL*17
02273      EXIT.                                                           CL*17
02274                                  EJECT                               CL*17
02275 ******************************************************************   CL*17
02276 *    DMD ONLY  -  UPDATE THE CLAIM EOB NOTES FILE - ELNOTE           CL*19
02277 ******************************************************************   CL*17
02278  2000-CREATE-DMD-NOTE.                                               CL*17
02279                                                                      CL*17
02280      MOVE 'N'                    TO  WS-HAVE-NOTE-HEADER.            CL*17
02281      MOVE +0                     TO  WS-ELNOTE-BROWSE-SW.            CL*19
02282                                                                      CL*17
02283      MOVE LOW-VALUES             TO  WS-NOTE-KEY.                    CL*19
02284      MOVE AT-COMPANY-CD          TO  WS-EN-COMPANY-CD.               CL*17
02285      MOVE AT-CARRIER             TO  WS-EN-CARRIER                   CL*17
02286      MOVE AT-CLAIM-NO            TO  WS-EN-CLAIM-NO.                 CL*17
02287      MOVE ZEROS                  TO  WS-EN-PAYMENT-SEQ-NO.           CL*17
02288      MOVE '1'                    TO  WS-EN-RECORD-TYPE.              CL*17
02289                                                                      CL*17
02290      EXEC CICS HANDLE CONDITION                                      CL*17
02291          NOTFND  (2025-EOB-EXTRACT)                                  CL*17
02292          ENDFILE (2025-EOB-EXTRACT)                                  CL*17
02293          NOTOPEN (2050-NOT-OPEN)                                     CL*17
02294      END-EXEC.                                                       CL*17
02295                                                                      CL*17
02296      EXEC CICS STARTBR                                               CL*17
02297          DATASET (WS-NOTE-DSID)                                      CL*17
02298          RIDFLD  (WS-NOTE-KEY)                                       CL*17
02299          GTEQ                                                        CL*17
02300      END-EXEC.                                                       CL*17
02301                                                                      CL*19
02302      MOVE +1                     TO  WS-ELNOTE-BROWSE-SW.            CL*19
02303                                                                      CL*17
02304      EXEC CICS READNEXT                                              CL*17
02305          DATASET (WS-NOTE-DSID)                                      CL*17
02306          RIDFLD  (WS-NOTE-KEY)                                       CL*17
02307          SET     (ADDRESS OF CLAIM-EOB-NOTES)                        CL*17
02308      END-EXEC.                                                       CL*17
02309                                                                      CL*17
02310      IF EN-COMPANY-CD NOT = AT-COMPANY-CD                            CL*17
02311          GO TO 2025-EOB-EXTRACT.                                     CL*17
02312                                                                      CL*17
02313      IF (AT-CARRIER NOT = EN-CARRIER) OR                             CL*17
02314         (AT-CLAIM-NO NOT = EN-CLAIM-NO)                              CL*17
02315          GO TO 2025-EOB-EXTRACT.                                     CL*17
02316                                                                      CL*17
02317      IF EN-RECORD-TYPE NOT = '1'                                     CL*17
02318          GO TO 2025-EOB-EXTRACT.                                     CL*17
02319                                                                      CL*17
02320      MOVE 'Y'                    TO  WS-HAVE-NOTE-HEADER.            CL*17
02321      MOVE CLAIM-EOB-NOTES        TO  WS-SAVE-NOTE-RECORD.            CL*17
02322                                                                      CL*17
02323      EXEC CICS ENDBR                                                 CL*17
02324          DATASET (WS-NOTE-DSID)                                      CL*17
02325      END-EXEC.                                                       CL*17
02326                                                                      CL*17
02327      MOVE +0                     TO  WS-ELNOTE-BROWSE-SW.            CL*19
02328                                                                      CL*19
02329      EXEC CICS GETMAIN                                               CL*17
02330           SET      (ADDRESS OF CLAIM-EOB-NOTES)                      CL*17
02331           LENGTH   (EN-EOB-LENGTH)                                   CL*17
02332      END-EXEC.                                                       CL*17
02333                                                                      CL*17
02334      MOVE WS-SAVE-NOTE-RECORD    TO  CLAIM-EOB-NOTES.                CL*17
02335                                                                      CL*17
02336      MOVE AT-CERT-NO             TO  EN-CERT-NO.                     CL*17
02337      MOVE AT-SEQUENCE-NO         TO  EN-PAYMENT-SEQ-NO.              CL*17
02338      MOVE '2'                    TO  EN-RECORD-TYPE.                 CL*17
02339      MOVE +1750                  TO  EN-LAST-MAINT-BY.               CL*17
02340      MOVE SAVE-BIN-DATE          TO  EN-LAST-MAINT-DT.               CL*17
02341                                                                      CL*17
02342      EXEC CICS WRITE                                                 CL*17
02343          DATASET (WS-NOTE-DSID)                                      CL*17
02344          FROM    (CLAIM-EOB-NOTES)                                   CL*17
02345          RIDFLD  (EN-CONTROL-PRIMARY)                                CL*17
02346      END-EXEC.                                                       CL*17
02347                                                                      CL*17
02348  2025-EOB-EXTRACT.                                                   CL*17
02349                                                                      CL*17
02350      IF WS-ELNOTE-BROWSE-SW = +1                                     CL*19
02351          EXEC CICS ENDBR                                             CL*17
02352              DATASET (WS-NOTE-DSID)                                  CL*17
02353          END-EXEC.                                                   CL*17
02354                                                                      CL*17
02355      EXEC CICS GETMAIN                                               CL*17
02356           SET      (ADDRESS OF CLAIM-EOB-EXTRACT)                    CL*17
02357           LENGTH   (EB-EOB-LENGTH)                                   CL*17
02358      END-EXEC.                                                       CL*17
02359                                                                      CL*17
02360      MOVE SPACES                 TO  CLAIM-EOB-EXTRACT.              CL*19
02361      MOVE 'EB'                   TO  EB-RECORD-ID.                   CL*17
02362      MOVE AT-COMPANY-CD          TO  EB-COMPANY-CD.                  CL*17
02363      MOVE AT-CARRIER             TO  EB-CARRIER.                     CL*17
02364      MOVE AT-CLAIM-NO            TO  EB-CLAIM-NO.                    CL*17
02365      MOVE AT-CERT-NO             TO  EB-CERT-NUMBER.                 CL*17
02366      MOVE AT-SEQUENCE-NO         TO  EB-PMT-TRLR-SEQ-NO.             CL*17
02367                                                                      CL*17
02368      MOVE CL-INCURRED-DT         TO  DC-BIN-DATE-1.                  CL*17
02369      MOVE ' '                    TO  DC-OPTION-CODE.                 CL*17
02370      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.                  CL*17
02371                                                                      CL*17
02372      IF DATE-CONVERSION-ERROR                                        CL*17
02373          MOVE ZEROS              TO  EB-INCURRED-DT                  CL*17
02374      ELSE                                                            CL*17
02375          MOVE DC-GREG-DATE-1-MDY (1:2) TO DMD-MDY-MM                 CL*27
02376          MOVE DC-GREG-DATE-1-MDY (3:2) TO DMD-MDY-DD                 CL*27
02377          MOVE DC-GREG-DATE-1-MDY (5:2) TO DMD-MDY-YY                 CL*27
02378          IF DMD-MDY-YY < '70'                                        CL*27
02379              MOVE '20'           TO  DMD-MDY-DECADE                  CL*17
02380          ELSE                                                        CL*17
02381              MOVE '19'           TO  DMD-MDY-DECADE                  CL*17
02382          END-IF                                                      CL*17
02383          MOVE DMD-DATE-MMDDYYYY  TO  EB-INCURRED-DT.                 CL*17
02384                                                                      CL*17
02385      MOVE AT-PAID-FROM-DT        TO  DC-BIN-DATE-1.                  CL*17
02386      MOVE ' '                    TO  DC-OPTION-CODE.                 CL*17
02387      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.                  CL*17
02388                                                                      CL*17
02389      IF DATE-CONVERSION-ERROR                                        CL*17
02390          MOVE ZEROS              TO  EB-PAID-FROM                    CL*17
02391      ELSE                                                            CL*17
02392          MOVE DC-GREG-DATE-1-MDY (1:2) TO DMD-MDY-MM                 CL*27
02393          MOVE DC-GREG-DATE-1-MDY (3:2) TO DMD-MDY-DD                 CL*27
02394          MOVE DC-GREG-DATE-1-MDY (5:2) TO DMD-MDY-YY                 CL*27
02395          IF DMD-MDY-YY < '70'                                        CL*27
02396              MOVE '20'           TO  DMD-MDY-DECADE                  CL*17
02397          ELSE                                                        CL*17
02398              MOVE '19'           TO  DMD-MDY-DECADE                  CL*17
02399          END-IF                                                      CL*17
02400          MOVE DMD-DATE-MMDDYYYY  TO  EB-PAID-FROM.                   CL*17
02401                                                                      CL*17
02402      MOVE AT-PAID-THRU-DT        TO  DC-BIN-DATE-1.                  CL*17
02403      MOVE ' '                    TO  DC-OPTION-CODE.                 CL*17
02404      PERFORM 8500-DATE-CONVERSION  THRU  8500-EXIT.                  CL*17
02405                                                                      CL*17
02406      IF DATE-CONVERSION-ERROR                                        CL*17
02407          MOVE ZEROS              TO  EB-PAID-THRU                    CL*17
02408      ELSE                                                            CL*17
02409          MOVE DC-GREG-DATE-1-MDY (1:2) TO DMD-MDY-MM                 CL*27
02410          MOVE DC-GREG-DATE-1-MDY (3:2) TO DMD-MDY-DD                 CL*27
02411          MOVE DC-GREG-DATE-1-MDY (5:2) TO DMD-MDY-YY                 CL*27
02412          IF DMD-MDY-YY < '70'                                        CL*27
02413              MOVE '20'           TO  DMD-MDY-DECADE                  CL*17
02414          ELSE                                                        CL*17
02415              MOVE '19'           TO  DMD-MDY-DECADE                  CL*17
02416          END-IF                                                      CL*17
02417          MOVE DMD-DATE-MMDDYYYY  TO  EB-PAID-THRU.                   CL*17
02418                                                                      CL*17
02419      MOVE CL-INSURED-ADDR-CNT    TO  EB-PAYMENT-ADDR-SEQ.            CL*20
02420      MOVE AT-PAYEES-NAME         TO  EB-PAYEE-NAME.                  CL*17
02421      MOVE AT-AMOUNT-PAID         TO  EB-PAID-AMOUNT.                 CL*17
02422      MOVE AT-PAYMENT-TYPE        TO  EB-PAYMENT-TYPE.                CL*17
02423      MOVE AT-EOB-CODE1           TO  EB-EOB-CODE1.                   CL*17
02424      MOVE AT-EOB-CODE2           TO  EB-EOB-CODE2.                   CL*17
02425      MOVE AT-EOB-CODE3           TO  EB-EOB-CODE3.                   CL*17
02426      MOVE AT-EOB-CODE4           TO  EB-EOB-CODE4.                   CL*17
02427      MOVE AT-EOB-CODE5           TO  EB-EOB-CODE5.                   CL*17
02428                                                                      CL*17
02429      IF HAVE-NOTE-HEADER                                             CL*17
02430          MOVE EN-EOB-NOTE1       TO  EB-EOB-NOTE1                    CL*17
02431          MOVE EN-EOB-NOTE2       TO  EB-EOB-NOTE2                    CL*17
02432          MOVE EN-EOB-NOTE3       TO  EB-EOB-NOTE3                    CL*17
02433          MOVE EN-EOB-NOTE4       TO  EB-EOB-NOTE4.                   CL*17
02434                                                                      CL*17
02435      EXEC CICS WRITE                                                 CL*17
02436          DATASET (WS-EXTRACT-DSID)                                   CL*17
02437          FROM    (CLAIM-EOB-EXTRACT)                                 CL*17
02438          RIDFLD  (EB-CONTROL-PRIMARY)                                CL*17
02439      END-EXEC.                                                       CL*17
02440                                                                      CL*17
02441      GO TO 2099-EXIT.                                                CL*17
02442                                                                      CL*17
02443  2050-NOT-OPEN.                                                      CL*17
02444                                                                      CL*17
02445      MOVE ER-8137                TO  EMI-ERROR.                      CL*19
02446      MOVE -1                     TO  APFKL.                          CL*17
02447      GO TO 8200-SEND-DATAONLY.                                       CL*17
02448                                                                      CL*17
02449  2099-EXIT.                                                          CL*17
02450      EXIT.                                                           CL*17
02451                                  EJECT                               CL*17
02452 ******************************************************************   CL*17
02453 *    DMD ONLY  -  DELETE THE CLAIM EOB NOTES HEADER                  CL*17
02454 ******************************************************************   CL*17
02455  2500-DELETE-DMD-NOTE.                                               CL*17
02456                                                                      CL*17
02457      IF WS-PREV-CLAIM-NO = LOW-VALUES                                CL*17
02458          GO TO 2550-SETUP-NEXT.                                      CL*17
02459                                                                      CL*17
02460      EXEC CICS HANDLE CONDITION                                      CL*17
02461          NOTFND  (2550-SETUP-NEXT)                                   CL*17
02462          ENDFILE (2550-SETUP-NEXT)                                   CL*17
02463          NOTOPEN (2050-NOT-OPEN)                                     CL*17
02464      END-EXEC.                                                       CL*17
02465                                                                      CL*17
02466      MOVE WS-PREV-CLAIM          TO  WS-NOTE-KEY.                    CL*17
02467                                                                      CL*17
02468      EXEC CICS READ                                                  CL*17
02469          DATASET (WS-NOTE-DSID)                                      CL*17
02470          RIDFLD  (WS-NOTE-KEY)                                       CL*17
02471          SET     (ADDRESS OF CLAIM-EOB-NOTES)                        CL*17
02472          UPDATE                                                      CL*17
02473      END-EXEC.                                                       CL*17
02474                                                                      CL*17
02475      EXEC CICS DELETE                                                CL*17
02476          DATASET (WS-NOTE-DSID)                                      CL*17
02477      END-EXEC.                                                       CL*17
02478                                                                      CL*17
02479  2550-SETUP-NEXT.                                                    CL*17
02480                                                                      CL*17
02481      MOVE AT-COMPANY-CD          TO  WS-PREV-COMPANY.                CL*17
02482      MOVE AT-CARRIER             TO  WS-PREV-CARRIER.                CL*17
02483      MOVE AT-CLAIM-NO            TO  WS-PREV-CLAIM-NO.               CL*17
02484      MOVE LOW-VALUES             TO  WS-PREV-CERT-NO.                CL*19
02485      MOVE ZEROS                  TO  WS-PREV-SEQ-NO.                 CL*17
02486      MOVE '1'                    TO  WS-PREV-RECORD-TYPE.            CL*17
02487                                                                      CL*17
02488  2599-EXIT.                                                          CL*17
02489      EXIT.                                                           CL*17
02490                                  EJECT                               CL*17
031808
031808****************************************************************** 
031808*    TOTAL THE UNAPPROVED PAYMENTS         
031808****************************************************************** 
031808 3000-TOTAL-UNAPPROVED.
031808     MOVE AQ-PMT-UNAPPROVED-COUNT TO WS-PMT-UNAPPROVED-COUNT.
031808                                                               
031808     MOVE WS-ACTIVITY-QUE-KEY TO  WS-ACTIVITY-TRAILERS-KEY. 
031808     MOVE ZERO                TO  WS-ATK-SEQUENCE-NO.
031808                                                                  
031808     MOVE LOW-VALUES          TO  WS-LAST-ACTIVITY-TRAILERS-KEY.
031808                                     
031808     EXEC CICS HANDLE CONDITION      
031808         NOTFND  (3999-EXIT)
031808         ENDFILE (3999-EXIT)
031808     END-EXEC.                       
031808                  
031808 3100-MAIN-LOGIC. 
031808     IF WS-PMT-UNAPPROVED-COUNT NOT GREATER THAN ZERO       
031808         GO TO 3300-END-BROWSE-ELTRLR.
031808                                                                
031808     IF WS-ELTRLR-BROWSE-SW = ZERO                              
031808         EXEC CICS STARTBR                                      
031808             DATASET (WS-ACTIVITY-TRAILERS-DSID)                
031808             RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                 
031808             EQUAL                                              
031808         END-EXEC                                               
031808         MOVE +1               TO  WS-ELTRLR-BROWSE-SW          
031808       ELSE                                                     
031808         EXEC CICS RESETBR                                      
031808             DATASET (WS-ACTIVITY-TRAILERS-DSID)                
031808             RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                 
031808             EQUAL                                              
031808         END-EXEC.                                              
031808                                                                
031808     EJECT                                                      
031808 3200-READNEXT-ELTRLR.                                          
031808
031808     IF WS-PMT-UNAPPROVED-COUNT NOT GREATER THAN ZERO       
031808         GO TO 3300-END-BROWSE-ELTRLR.
031808                                                                
031808     EXEC CICS READNEXT                                         
031808         DATASET (WS-ACTIVITY-TRAILERS-DSID)                    
031808         RIDFLD  (WS-ACTIVITY-TRAILERS-KEY)                     
031808         SET     (ADDRESS OF ACTIVITY-TRAILERS)                 
031808     END-EXEC.                                                  
031808                                                                
031808     IF WS-ACTIVITY-TRAILERS-KEY = WS-LAST-ACTIVITY-TRAILERS-KEY
031808         GO TO 3200-READNEXT-ELTRLR.                            
031808                                                                
031808     MOVE WS-ACTIVITY-TRAILERS-KEY  TO                          
031808                                  WS-LAST-ACTIVITY-TRAILERS-KEY 
031808                                                                
031808     IF WS-AQK-COMPANY-CD NOT = WS-ATK-COMPANY-CD  OR           
031808        WS-AQK-CARRIER    NOT = WS-ATK-CARRIER     OR           
031808        WS-AQK-CLAIM-NO   NOT = WS-ATK-CLAIM-NO    OR           
031808        WS-AQK-CERT-NO    NOT = WS-ATK-CERT-NO                  
031808         GO TO 3300-END-BROWSE-ELTRLR
031808     END-IF.
031808                                                                
031808     IF AT-TRAILER-TYPE NOT = '2'                               
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     IF AT-AMOUNT-PAID   NEGATIVE                               
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808                                                                
031808     IF AT-VOID-DT NOT = LOW-VALUES                             
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808                                                                
031808     IF AT-CHECK-WRITTEN-DT NOT = LOW-VALUES                    
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808                                                                
031808     IF AT-TO-BE-WRITTEN-DT GREATER THAN WS-CURRENT-DATE        
031808         GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808                                                                
031808     IF AT-PAYMENT-APPROVAL-SW NOT = 'U'                        
031808           GO TO 3200-READNEXT-ELTRLR
031808     END-IF.
031808
031808     SUBTRACT +1 FROM WS-PMT-UNAPPROVED-COUNT.
031808     ADD +1                      TO  WS-UNAPPROVED-COUNT. 
031808     ADD AT-AMOUNT-PAID          TO  WS-UNAPPROVED-AMOUNT.
031808    
031808     GO TO 3200-READNEXT-ELTRLR.
031808
031808 3300-END-BROWSE-ELTRLR.                                        
031808                                                                
031808     IF WS-ELTRLR-BROWSE-SW > ZERO                              
031808         EXEC CICS ENDBR                                        
031808             DATASET (WS-ACTIVITY-TRAILERS-DSID)                
031808         END-EXEC                                               
031808     END-IF.                                                    
031808                                                                
031808     MOVE ZERO                   TO  WS-ELTRLR-BROWSE-SW.       
031808                                                                
031808                                                                
031808 3999-EXIT.
031808     EXIT.
031808                                EJECT
031808                                             
111502*7000-PRINT-CHECKS-WAITING SECTION.                               EL175
111502*                                                                 EL175
111502*    IF PI-COMPANY-ID = 'TIH' OR 'OFL' OR 'CGL' OR 'FGL' OR 'TII'    CL**3
111502*       GO TO 7090-EXIT.                                          EL175
111502*                                                                 EL175
111502*    IF PI-COMPANY-ID = 'DMD' OR 'CID' OR 'DCC'                      CL*24
02498 *        MOVE EIBTRMID       TO WS-FORMS-PRINTER                     CL*24
111502*        EXEC CICS START                                             CL*24
111502*             TRANSID    ('EX64')                                    CL*24
111502*             FROM       (PROGRAM-INTERFACE-BLOCK)                   CL*24
111502*             LENGTH     (PI-COMM-LENGTH)                            CL*24
02503 *             TERMID     (WS-FORMS-PRINTER)                          CL*24
111502*        END-EXEC                                                    CL*24
111502*    ELSE                                                            CL*24
111502*        EXEC CICS START                                             CL*24
111502*             TRANSID    ('EX64')                                    CL*24
111502*             FROM       (PROGRAM-INTERFACE-BLOCK)                   CL*24
111502*             LENGTH     (PI-COMM-LENGTH)                            CL*24
111502*             TERMID     (WS-FORMS-PRINTER)                          CL*24
111502*        END-EXEC.                                                   CL*24
02512                                                                   EL175
111502*    GO TO 7090-EXIT.                                             EL175
02514                                                                   EL175
02515  7010-TERMID-ERROR.                                               EL175
02516      MOVE ER-0412                TO EMI-ERROR.                    EL175
02517      MOVE -1                     TO APFKL.                        EL175
02518      GO TO 8200-SEND-DATAONLY.                                       CL*13
02519                                                                   EL175
02520  7020-TRANS-ERROR.                                                EL175
02521      MOVE ER-0413                TO EMI-ERROR.                    EL175
02522      MOVE -1                     TO APFKL.                        EL175
02523      GO TO 8200-SEND-DATAONLY.                                       CL*13
02524                                                                   EL175
111502*7090-EXIT.                                                       EL175
111502*    EXIT.                                                        EL175
02527                                                                   EL175
02528      EJECT                                                        EL175
02529  8100-SEND-INITIAL-MAP SECTION.                                   EL175
02530                                                                   EL175
02531      MOVE EIBTIME                TO  TIME-IN.                     EL175
02532                                                                   EL175
02533      MOVE SAVE-DATE              TO ADATEO.                       EL175
02534      MOVE TIME-OUT               TO ATIMEO.                       EL175
02535      MOVE PI-LIFE-OVERRIDE-L6    TO COVG1O.                       EL175
02536      MOVE PI-AH-OVERRIDE-L6      TO COVG2O.                       EL175
02537      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.                         CL*12
02538      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.                         CL*12
031808     MOVE EMI-MESSAGE-AREA (3)   TO AEMSG3O.
02539                                                                   EL175
02540      EXEC CICS SEND                                               EL175
02541          FROM   (EL175AI)                                         EL175
02542          MAPSET (WS-MAPSET-NAME)                                  EL175
02543          MAP    (WS-MAP-NAME)                                     EL175
02544          CURSOR ERASE                                                CL*25
02545      END-EXEC.                                                       CL*25
02546                                                                   EL175
02547      PERFORM 9100-RETURN-TRAN.                                    EL175
02548                                                                   EL175
02549  8200-EXIT.                                                       EL175
02550      EXIT.                                                        EL175
02551                                                                   EL175
02552      EJECT                                                        EL175
02553  8200-SEND-DATAONLY SECTION.                                      EL175
02554      IF EMI-ERROR = ZERO                                             CL*12
090309         MOVE 'PRESS PF1 TO RELEASE CHECKS' TO APF1O
02555          MOVE AL-PABOF           TO APF1A                            CL*12
02556          MOVE AL-PADOF           TO ACOMPA                           CL*12
02557       ELSE                                                           CL*12
02558          PERFORM 9900-ERROR-FORMAT                                EL175
02559          MOVE AL-PADOF           TO APF1A                            CL*12
02560          MOVE AL-PABOF           TO ACOMPA.                          CL*12
031808
031808     IF WS-RELEASED-COUNT = +0
031808         MOVE AL-PADOF           TO APF1A                         
031808         MOVE AL-PABOF           TO ACOMPA
031808     END-IF.
090309
090309     IF PI-ALL-CHKS-NOT-ALLOWED
090309         MOVE 'RELEASE ALL CHKS NOT ALLOWED' TO APF1O
090309         MOVE AL-PABOF           TO APF1A                         
090309         MOVE AL-PADOF           TO ACOMPA
090309     END-IF.
02561                                                                   EL175
02562      MOVE EIBTIME                TO  TIME-IN.                     EL175
02563                                                                   EL175
02564      MOVE SAVE-DATE              TO  ADATEO.                      EL175
02565      MOVE TIME-OUT               TO  ATIMEO.                      EL175
02566      MOVE EMI-MESSAGE-AREA (1)   TO AEMSG1O.                      EL175
02567      MOVE EMI-MESSAGE-AREA (2)   TO AEMSG2O.                      EL175
031808     MOVE EMI-MESSAGE-AREA (3)   TO AEMSG3O.
02568                                                                   EL175
02569      EXEC CICS SEND DATAONLY                                      EL175
02570          FROM   (EL175AI)                                         EL175
02571          MAPSET (WS-MAPSET-NAME)                                  EL175
02572          MAP    (WS-MAP-NAME)                                     EL175
02573          CURSOR                                                      CL*25
02574      END-EXEC.                                                       CL*25
02575                                                                   EL175
02576      PERFORM 9100-RETURN-TRAN.                                    EL175
02577                                                                   EL175
02578  8100-EXIT.                                                       EL175
02579      EXIT.                                                        EL175
02580                                                                   EL175
02581      EJECT                                                        EL175
02582  8300-SEND-TEXT SECTION.                                          EL175
02583      EXEC CICS SEND TEXT                                          EL175
02584          FROM   (LOGOFF-TEXT)                                     EL175
02585          LENGTH (LOGOFF-LENGTH)                                   EL175
02586          ERASE  FREEKB END-EXEC.                                  EL175
02587                                                                   EL175
02588      EXEC CICS RETURN                                             EL175
02589      END-EXEC.                                                       CL*25
02590                                                                   EL175
02591  8300-EXIT.                                                       EL175
02592      EXIT.                                                        EL175
02593                                                                   EL175
02594      EJECT                                                        EL175
02595  8500-DATE-CONVERSION SECTION.                                    EL175
02596      EXEC CICS LINK                                               EL175
02597          PROGRAM  (ELDATCV)                                       EL175
02598          COMMAREA (DATE-CONVERSION-DATA)                          EL175
02599          LENGTH   (DC-COMM-LENGTH)                                   CL*25
02600      END-EXEC.                                                       CL*25
02601                                                                   EL175
02602  8500-EXIT.                                                       EL175
02603      EXIT.                                                        EL175
02604                                                                   EL175
02605      EJECT                                                        EL175
02606  9000-RETURN-CICS SECTION.                                        EL175
02607      MOVE EL005                  TO  THIS-PGM.                    EL175
02608      MOVE EIBAID                 TO  PI-ENTRY-CD-1.               EL175
02609      PERFORM 9300-XCTL.                                           EL175
02610                                                                   EL175
02611  9000-EXIT.                                                       EL175
02612      EXIT.                                                        EL175
02613                                                                   EL175
02614  9100-RETURN-TRAN SECTION.                                        EL175
02615      MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.            EL175
02616      MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.        EL175
02617                                                                   EL175
02618      EXEC CICS RETURN                                             EL175
02619          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL175
02620          LENGTH   (PI-COMM-LENGTH)                                EL175
02621          TRANSID  (WS-TRANS-ID)                                      CL*25
02622      END-EXEC.                                                       CL*25
02623                                                                   EL175
02624  9100-EXIT.                                                       EL175
02625      EXIT.                                                        EL175
02626                                                                   EL175
02627  9300-XCTL SECTION.                                               EL175
02628      MOVE DFHENTER               TO  EIBAID.                      EL175
02629                                                                   EL175
02630      EXEC CICS XCTL                                               EL175
02631          PROGRAM  (THIS-PGM)                                      EL175
02632          COMMAREA (PROGRAM-INTERFACE-BLOCK)                       EL175
02633          LENGTH   (PI-COMM-LENGTH)                                   CL*25
02634      END-EXEC.                                                       CL*25
02635                                                                   EL175
02636  9300-EXIT.                                                       EL175
02637      EXIT.                                                        EL175
02638                                                                   EL175
02639      EJECT                                                        EL175
02640  9400-CLEAR SECTION.                                              EL175
02641      MOVE PI-RETURN-TO-PROGRAM  TO  THIS-PGM.                     EL175
02642      PERFORM 9300-XCTL.                                           EL175
02643                                                                   EL175
02644  9400-EXIT.                                                       EL175
02645      EXIT.                                                        EL175
02646                                                                   EL175
02647  9600-PGMIDERR SECTION.                                           EL175
02648      EXEC CICS HANDLE CONDITION                                   EL175
02649          PGMIDERR (8300-SEND-TEXT)                                   CL*25
02650      END-EXEC.                                                       CL*25
02651                                                                   EL175
02652      MOVE THIS-PGM               TO  PI-CALLING-PROGRAM.          EL175
02653                                                                   EL175
02654      MOVE EL005                  TO  THIS-PGM                     EL175
02655                                      LOGOFF-PGM.                  EL175
02656      MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                 EL175
02657                                                                   EL175
02658      MOVE SPACES                 TO  PI-ENTRY-CD-1                EL175
02659      PERFORM 9300-XCTL.                                           EL175
02660                                                                   EL175
02661  9600-EXIT.                                                       EL175
02662      EXIT.                                                        EL175
02663                                                                   EL175
02664      EJECT                                                        EL175
02665  9900-ERROR-FORMAT SECTION.                                       EL175
02666      EXEC CICS LINK                                               EL175
02667          PROGRAM  (EL001)                                         EL175
02668          COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                 EL175
02669          LENGTH   (EMI-COMM-LENGTH)                                  CL*25
02670      END-EXEC.                                                       CL*25
02671                                                                   EL175
02672  9900-EXIT.                                                       EL175
02673      EXIT.                                                        EL175
02674                                                                   EL175
02675      EJECT                                                        EL175
02676  9990-ERROR SECTION.                                              EL175
02677      MOVE DFHEIBLK TO EMI-LINE1.                                  EL175
02678      EXEC CICS LINK                                               EL175
02679          PROGRAM  (EL004)                                         EL175
02680          COMMAREA (EMI-LINE1)                                     EL175
02681          LENGTH   (72)                                               CL*25
02682      END-EXEC.                                                       CL*25
02683                                                                   EL175
02684      GO TO 8200-SEND-DATAONLY.                                       CL*13
02685                                                                   EL175
02686  9990-EXIT.                                                       EL175
02687      EXIT.                                                        EL175
02688                                                                   EL175
02689  9995-SECURITY-VIOLATION.                                         EL175
02690                              COPY ELCSCTP.                        EL175
02691                                                                   EL175
02692  9995-EXIT.                                                       EL175
02693      EXIT.                                                        EL175
02694                                                                   EL175

