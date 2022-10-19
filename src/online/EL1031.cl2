00001  IDENTIFICATION DIVISION.                                         10/18/96
00002                                                                   EL1031
00003  PROGRAM-ID.                 EL1031.                                 LV022
00004 *              PROGRAM CONVERTED BY                                  CL*19
00005 *              COBOL CONVERSION AID PO 5785-ABJ                      CL*19
00006 *              CONVERSION DATE 09/21/94 08:23:00.                    CL*19
00007 *                            VMOD=2.022.                             CL*22
00008 *                                                                 EL1031
00008 *                                                                 EL1031
00009 *AUTHOR.        LOGIC,INC.                                           CL*19
00010 *               DALLAS, TEXAS.                                       CL*19
00011                                                                   EL1031
00012 *DATE-COMPILED.                                                      CL*19
00013                                                                   EL1031
00014 *SECURITY.   *****************************************************   CL*19
00015 *            *                                                   *   CL*19
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *   CL*19
00017 *            *                                                   *   CL*19
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *   CL*19
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *   CL*19
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *   CL*19
00021 *            *                                                   *   CL*19
00022 *            *****************************************************   CL*19
00023                                                                   EL1031
00024 *                                                                    CL**6
00025 *REMARKS.                                                         EL1031
00026 *        TRANSACTION - EXH8 - USER RECORD APPLICATION DESCRIPT    EL1031
092908******************************************************************
092908*                   C H A N G E   L O G
092908*
092908* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092908*-----------------------------------------------------------------
092908*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092908* EFFECTIVE    NUMBER
092908*-----------------------------------------------------------------
092908* 092908    2008091100003  PEMA ADD NEW APPS 29 AND 30
012009* 012009    2008100900001  PEMA ADD NEW CLAIM APP
092908******************************************************************
00027                                                                   EL1031
00028      EJECT                                                        EL1031
00029  ENVIRONMENT DIVISION.                                            EL1031
00030  DATA DIVISION.                                                   EL1031
00031  WORKING-STORAGE SECTION.                                         EL1031
00032  77  FILLER  PIC  X(32) VALUE '********************************'. EL1031
00033  77  FILLER  PIC  X(32) VALUE '*    EL1031 WORKING STORAGE    *'. EL1031
00034  77  FILLER  PIC  X(32) VALUE '********* VMOD=2.022 ***********'.    CL*22
00035                                                                   EL1031
00036  01  ERROR-MESSAGES.                                              EL1031
00037      12  ER-0000             PIC  X(4)       VALUE '0000'.        EL1031
00038      12  ER-0004             PIC  X(4)       VALUE '0004'.        EL1031
00039      12  ER-0007             PIC  X(4)       VALUE '0007'.        EL1031
00040      12  ER-0029             PIC  X(4)       VALUE '0029'.        EL1031
00041      12  ER-0042             PIC  X(4)       VALUE '0042'.        EL1031
00042      12  ER-0068             PIC  X(4)       VALUE '0068'.        EL1031
00043      12  ER-0073             PIC  X(4)       VALUE '0073'.        EL1031
00044      12  ER-0086             PIC  X(4)       VALUE '0086'.        EL1031
00045      12  ER-7008             PIC  X(4)       VALUE '7008'.        EL1031
00046      12  ER-7299             PIC  X(4)       VALUE '7299'.        EL1031
00047      12  ER-7320             PIC  X(4)       VALUE '7320'.        EL1031
00048      12  ER-7322             PIC  X(4)       VALUE '7322'.        EL1031
00049                                                                   EL1031
00050  01  WS-DATE-AREA.                                                EL1031
00051      12  SAVE-DATE           PIC  X(8)       VALUE SPACES.        EL1031
00052      12  SAVE-BIN-DATE       PIC  XX         VALUE SPACES.        EL1031
00053                                                                   EL1031
00054      EJECT                                                        EL1031
00055  01  STANDARD-AREAS.                                              EL1031
00056      12  MAP-NAME            PIC  X(8)       VALUE 'EL1031A'.     EL1031
00057      12  MAPSET-NAME         PIC  X(8)       VALUE 'EL1031S'.     EL1031
00058      12  TRANS-ID            PIC  X(4)       VALUE 'EXH8'.        EL1031
00059      12  THIS-PGM            PIC  X(8)       VALUE 'EL1031'.      EL1031
00060      12  PGM-NAME            PIC  X(8).                           EL1031
00061      12  TIME-IN             PIC S9(7).                           EL1031
00062      12  TIME-OUT-R  REDEFINES  TIME-IN.                          EL1031
00063          16  FILLER          PIC  X.                              EL1031
00064          16  TIME-OUT        PIC  99V99.                          EL1031
00065          16  FILLER          PIC  XX.                             EL1031
00066      12  XCTL-005            PIC  X(8)       VALUE 'EL005'.       EL1031
00067      12  XCTL-010            PIC  X(8)       VALUE 'EL010'.       EL1031
00068      12  XCTL-126            PIC  X(8)       VALUE 'EL126'.       EL1031
00069      12  XCTL-626            PIC  X(8)       VALUE 'EL626'.       EL1031
00070      12  XCTL-400            PIC  X(8)       VALUE 'LF400'.       EL1031
00071      12  XCTL-800            PIC  X(8)       VALUE 'GL800'.       EL1031
00072      12  LINK-001            PIC  X(8)       VALUE 'EL001'.       EL1031
00073      12  LINK-004            PIC  X(8)       VALUE 'EL004'.       EL1031
00074      12  LINK-ELDATCV        PIC  X(8)       VALUE 'ELDATCV'.     EL1031
00075      12  FILE-ID-ELCNTL      PIC  X(8)       VALUE 'ELCNTL'.         CL*19
00076                                                                   EL1031
00077      EJECT                                                        EL1031
00078  01  SYSTEM-APP-TABLES.                                           EL1031
00079                                                                   EL1031
00080 **************************************                            EL1031
00081 *  CREDIT APPLICATIONS DESCRIPTIONS  *                            EL1031
00082 **************************************                            EL1031
00083                                                                   EL1031
00084      12  CREDIT-DESCRIPT-TABLE.                                   EL1031
00085          16  CR-001 PIC X(25) VALUE 'END USER REPORTING       '.     CL*19
00086          16  CR-002 PIC X(25) VALUE 'PROGRAM OPTIONS          '.     CL*19
00087          16  CR-003 PIC X(25) VALUE 'TEXT FILE (LETTERS FORMS)'.     CL*19
00088          16  CR-004 PIC X(25) VALUE 'ACCOUNT MASTERS          '.     CL*19
00089          16  CR-005 PIC X(25) VALUE 'COMPENSATION MASTERS     '.     CL*19
00090          16  CR-006 PIC X(25) VALUE 'RATE MASTERS             '.     CL*19
00091          16  CR-007 PIC X(25) VALUE 'REINSURANCE MASTERS      '.     CL*19
00092          16  CR-008 PIC X(25) VALUE 'COMMISSION TABLES        '.     CL*19
00093          16  CR-009 PIC X(25) VALUE 'MORTALITY TABLE CONTROLS '.     CL*19
00094          16  CR-010 PIC X(25) VALUE 'LOAN OFFICERS            '.     CL*19
00095          16  CR-011 PIC X(25) VALUE 'DATA ENTRY               '.     CL*19
00096          16  CR-012 PIC X(25) VALUE 'REVIEW AND CORRECTION    '.     CL*19
00097          16  CR-013 PIC X(25) VALUE 'FULL FILE EDIT           '.     CL*19
00098          16  CR-014 PIC X(25) VALUE 'CLAIMS AND RESERVES      '.     CL*19
00099          16  CR-015 PIC X(25) VALUE 'COMPENSATIONS (PYMT/ADJ) '.     CL*19
00100          16  CR-016 PIC X(25) VALUE 'RETRO/REINS   (PYMT/ADJ) '.     CL*19
00101          16  CR-017 PIC X(25) VALUE 'CHECK MAINTENANCE(CREDIT)'.     CL*19
00102          16  CR-018 PIC X(25) VALUE 'ACCOUNT STATEMENTS       '.     CL*19
00103          16  CR-019 PIC X(25) VALUE 'GENERAL AGENT STATEMENTS '.     CL*19
00104          16  CR-020 PIC X(25) VALUE 'LOSS RATIO SELECTION     '.     CL*19
00105          16  CR-021 PIC X(25) VALUE 'ONLINE STATEMENT PRINTING'.  EL1031
00106          16  CR-022 PIC X(25) VALUE 'CHECKS TO PRINT  (CREDIT)'.  EL1031
00107          16  CR-023 PIC X(25) VALUE 'CHECK RELEASE    (CREDIT)'.  EL1031
00108          16  CR-024 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.  EL1031
00109          16  CR-025 PIC X(25) VALUE 'ACCOUNT NOTEPAD          '.     CL*18
00110          16  CR-026 PIC X(25) VALUE 'RETRO MASTER      (EL606)'.     CL*19
00111          16  CR-027 PIC X(25) VALUE 'RETRO HISTORY     (EL607)'.     CL*19
00112          16  CR-028 PIC X(25) VALUE 'BANK MASTER MAINTENANCE  '.     CL*22
092908         16  CR-029 PIC X(25) VALUE 'STATE CONTROLS (EL106A)  '.  EL1031
092908         16  CR-030 PIC X(25) VALUE 'REPORT CUSTOMIZATION 604A'.  EL1031
00115          16  CR-031 PIC X(25) VALUE 'CERTIFICATE LOOK-UP      '.  EL1031
00116          16  CR-032 PIC X(25) VALUE 'CERTIFICATE NOTES        '.  EL1031
00117          16  CR-033 PIC X(25) VALUE 'CERTIFICATE CHANGES      '.  EL1031
00118          16  CR-034 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00119          16  CR-035 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00120          16  CR-036 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00121          16  CR-037 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00122          16  CR-038 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00123          16  CR-039 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00124          16  CR-040 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00125          16  CR-041 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00126          16  CR-042 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00127          16  CR-043 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00128          16  CR-044 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00129                                                                   EL1031
00130      EJECT                                                        EL1031
00131 **************************************                            EL1031
00132 *   CLAIM APPLICATIONS DESCRIPTIONS  *                            EL1031
00133 **************************************                            EL1031
00134                                                                   EL1031
00135      12  CLAIM-DESCRIPT-TABLE.                                    EL1031
00136          16  CL-001 PIC X(25) VALUE 'NEW CLAIM SETUP          '.  EL1031
00137          16  CL-002 PIC X(25) VALUE 'RECORD MAIL RECEIVED     '.  EL1031
00138          16  CL-003 PIC X(25) VALUE 'CLAIM AUDIT              '.  EL1031
00139          16  CL-004 PIC X(25) VALUE 'BENEFICIARY MASTER       '.  EL1031
00140          16  CL-005 PIC X(25) VALUE 'CLAIM MAINTENANCE        '.  EL1031
00141          16  CL-006 PIC X(25) VALUE 'DENIAL PROCESSING        '.  EL1031
00142          16  CL-007 PIC X(25) VALUE 'CLAIMS LETTER WRITER     '.  EL1031
00143          16  CL-008 PIC X(25) VALUE 'NOTE / REMINDER RECORDING'.  EL1031
00144          16  CL-009 PIC X(25) VALUE 'SETUP AUTOMATIC PAYMENT  '.  EL1031
00145          16  CL-010 PIC X(25) VALUE 'PAYMENT PROCESSING       '.  EL1031
00146          16  CL-011 PIC X(25) VALUE 'CHECKS TO PRINT  (CLAIMS)'.  EL1031
00147          16  CL-012 PIC X(25) VALUE 'CHECK RELEASE    (CLAIMS)'.  EL1031
00148          16  CL-013 PIC X(25) VALUE 'PRINT RELEASED CHECKS    '.  EL1031
00149          16  CL-014 PIC X(25) VALUE 'CLAIM ADDRESS MAINTENANCE'.  EL1031
00150          16  CL-015 PIC X(25) VALUE 'CLAIM TRAILER MAINTENANCE'.  EL1031
00151          16  CL-016 PIC X(25) VALUE 'CLAIM STATUS/DISPOSITION '.  EL1031
00152          16  CL-017 PIC X(25) VALUE 'SUPERVISOR REQUEST REPORT'.  EL1031
00153          16  CL-018 PIC X(25) VALUE 'FILE FOLDER LABEL PRINT  '.  EL1031
00154          16  CL-019 PIC X(25) VALUE 'CLAIMS STATUS PRINT      '.  EL1031
00155          16  CL-020 PIC X(25) VALUE 'LETTER/ADDR LABEL PRINT  '.  EL1031
00156          16  CL-021 PIC X(25) VALUE 'CLAIM LOOK-UP            '.  EL1031
00157          16  CL-022 PIC X(25) VALUE 'REVIEW PENDING ACTIVITY  '.     CL**6
00158          16  CL-023 PIC X(25) VALUE 'CHECK RECON      (EL146) '.     CL*14
00159          16  CL-024 PIC X(25) VALUE 'POLICY FORM MSTR (EL1582)'.     CL*14
00160          16  CL-025 PIC X(25) VALUE 'AUTO ACT MAINT   (EL145) '.     CL*14
00161          16  CL-026 PIC X(25) VALUE 'INITIAL/PROG FORMS PRINT '.     CL*16
00162          16  CL-027 PIC X(25) VALUE 'ONLINE REPORTS           '.     CL*16
00163          16  CL-028 PIC X(25) VALUE 'PAYMENT APPROVAL         '.     CL*21
012009         16  CL-029 PIC X(25) VALUE 'DENIAL MAINTENANCE       '.  EL1031
00165          16  CL-030 PIC X(25) VALUE 'REJECT MAINTENANCE       '.     CL*14
00166          16  CL-031 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00167          16  CL-032 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00168          16  CL-033 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00169          16  CL-034 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00170          16  CL-035 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00171          16  CL-036 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00172          16  CL-037 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00173          16  CL-038 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00174          16  CL-039 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00175          16  CL-040 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00176          16  CL-041 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00177          16  CL-042 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00178          16  CL-043 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00179          16  CL-044 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00180                                                                   EL1031
00181      EJECT                                                        EL1031
00182 *******************************************                       EL1031
00183 *  CREDIT CARD APPLICATIONS DESCRIPTIONS  *                       EL1031
00184 *******************************************                       EL1031
00185                                                                   EL1031
00186      12  CRDT-CRD-DESCRIPT-TABLE.                                 EL1031
00187          16  CC-001 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00188          16  CC-002 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00189          16  CC-003 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00190          16  CC-004 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00191          16  CC-005 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00192          16  CC-006 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00193          16  CC-007 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00194          16  CC-008 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00195          16  CC-009 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00196          16  CC-010 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00197          16  CC-011 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00198          16  CC-012 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00199          16  CC-013 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00200          16  CC-014 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00201          16  CC-015 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00202          16  CC-016 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00203          16  CC-017 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00204          16  CC-018 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00205          16  CC-019 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00206          16  CC-020 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00207          16  CC-021 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00208          16  CC-022 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00209          16  CC-023 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00210          16  CC-024 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00211          16  CC-025 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00212          16  CC-026 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00213          16  CC-027 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00214          16  CC-028 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00215          16  CC-029 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00216          16  CC-030 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00217          16  CC-031 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00218          16  CC-032 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00219          16  CC-033 PIC X(25) VALUE '****** FURURE USE  ******'.  EL1031
00220          16  CC-034 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00221          16  CC-035 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00222          16  CC-036 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00223          16  CC-037 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00224          16  CC-038 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00225          16  CC-039 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00226          16  CC-040 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00227          16  CC-041 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00228          16  CC-042 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00229          16  CC-043 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00230          16  CC-044 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00231                                                                   EL1031
00232      EJECT                                                        EL1031
00233 ************************************                              EL1031
00234 *  A/R  APPLICATIONS DESCRIPTIONS  *                              EL1031
00235 ************************************                              EL1031
00236                                                                   EL1031
00237      12  ACCT-RCV-DESCRIPT-TABLE.                                 EL1031
00238          16  AR-001 PIC X(25) VALUE ' ACCOUNTS RECEIVABLE MENU'.     CL**3
00239          16  AR-002 PIC X(25) VALUE ' REQUEST FILE PROCESSING '.     CL**3
00240          16  AR-003 PIC X(25) VALUE ' REQUEST FILE DISPLAY    '.     CL**3
00241          16  AR-004 PIC X(25) VALUE ' SUMMARY CROSS REFERENCE '.     CL**3
00242          16  AR-005 PIC X(25) VALUE ' PAYMENT AND ADJUSTMENTS '.     CL**3
00243          16  AR-006 PIC X(25) VALUE ' CHECK FILE MAINTENANCE  '.     CL*12
00244          16  AR-007 PIC X(25) VALUE ' CHECK RELEASE           '.     CL*12
00245          16  AR-008 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00246          16  AR-009 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00247          16  AR-010 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00248          16  AR-011 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00249          16  AR-012 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00250          16  AR-013 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00251          16  AR-014 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00252          16  AR-015 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00253          16  AR-016 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00254          16  AR-017 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00255          16  AR-018 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00256          16  AR-019 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00257          16  AR-020 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00258          16  AR-021 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00259          16  AR-022 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00260          16  AR-023 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00261          16  AR-024 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00262          16  AR-025 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00263          16  AR-026 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00264          16  AR-027 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00265          16  AR-028 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00266          16  AR-029 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00267          16  AR-030 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00268          16  AR-031 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00269          16  AR-032 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00270          16  AR-033 PIC X(25) VALUE '****** FURURE USE  ******'.  EL1031
00271          16  AR-034 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00272          16  AR-035 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00273          16  AR-036 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00274          16  AR-037 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00275          16  AR-038 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00276          16  AR-039 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00277          16  AR-040 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00278          16  AR-041 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00279          16  AR-042 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00280          16  AR-043 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00281          16  AR-044 PIC X(25) VALUE '****** FUTURE USE  ******'.  EL1031
00282                                                                   EL1031
00283      EJECT                                                           CL**6
00284 ****************************************                             CL**6
00285 *  MORTGAGE APPLICATIONS DESCRIPTIONS  *                             CL**6
00286 ****************************************                             CL**6
00287                                                                      CL**6
00288      12  MORTGAGE-DESCRIPT-TABLE.                                    CL**6
00289          16  MP-001 PIC X(25) VALUE 'CONVENIENCE CODES (EM114)'.     CL*15
00290          16  MP-002 PIC X(25) VALUE 'PRODUCER MASTER   (EM650)'.     CL**7
00291          16  MP-003 PIC X(25) VALUE 'LETTER ARCHIVE(EM152/153)'.     CL**7
00292          16  MP-004 PIC X(25) VALUE 'MORTALITY CODES   (EL602)'.     CL*17
00293          16  MP-005 PIC X(25) VALUE 'POLICY MSTR LOOKUP(EM127)'.     CL**7
00294          16  MP-006 PIC X(25) VALUE 'LETTER PRINTER    (EM178)'.     CL**7
00295          16  MP-007 PIC X(25) VALUE 'TEXT FILE UPDATE  (EL104)'.     CL**7
00296          16  MP-008 PIC X(25) VALUE 'PRODUCER PLANS    (EM659)'.     CL**7
00297          16  MP-009 PIC X(25) VALUE 'DATA ENTRY        (EM630)'.     CL**7
00298          16  MP-010 PIC X(25) VALUE 'REVIEW AND CORR   (EM631)'.     CL**7
00299          16  MP-011 PIC X(25) VALUE 'GRP INV MATCHLIST(EM6361)'.     CL**7
00300          16  MP-012 PIC X(25) VALUE 'RATE UPDATE       (EM656)'.     CL**7
00301          16  MP-013 PIC X(25) VALUE 'SOLICITATION ENTRY(EM660)'.     CL**6
00302          16  MP-014 PIC X(25) VALUE 'UNDERWRITERS      (EM160)'.     CL**6
00303          16  MP-015 PIC X(25) VALUE 'INDIVIDUAL PAYMENT(EM637)'.     CL**7
00304          16  MP-016 PIC X(25) VALUE 'POL INV MATCHLIST(EM6362)'.     CL**7
00305          16  MP-017 PIC X(25) VALUE 'PAYMENT REVERSAL  (EM639)'.     CL**7
00306          16  MP-018 PIC X(25) VALUE 'PREMIUM QOUTE     (EM680)'.     CL**7
00307          16  MP-019 PIC X(25) VALUE 'CLAIMS AND RES    (EM632)'.     CL**7
00308          16  MP-020 PIC X(25) VALUE 'MIB SEARCH       (EM1602)'.     CL**7
00309          16  MP-021 PIC X(25) VALUE 'PLN SUMMARY RPT   (EM690)'.     CL**7
00310          16  MP-022 PIC X(25) VALUE 'PROD SUMMARY RPT  (EM673)'.     CL**7
00311          16  MP-023 PIC X(25) VALUE 'COMP MSTR SUMMARY (EM675)'.     CL**7
00312          16  MP-024 PIC X(25) VALUE 'PYMNT AND ADJ RPT (EM682)'.     CL**7
00313          16  MP-025 PIC X(25) VALUE 'UNDWRTR STATUS    (EM692)'.     CL**7
00314          16  MP-026 PIC X(25) VALUE 'UNDWRTR WORK SHT  (EM693)'.     CL**7
00315          16  MP-027 PIC X(25) VALUE 'APP FILE SUMMARY  (EM691)'.     CL**7
00316          16  MP-028 PIC X(25) VALUE 'POL HLDR - GEN    (EM694)'.     CL**7
00317          16  MP-029 PIC X(25) VALUE 'POL HLDR - CLAIMS (EM695)'.     CL**7
00318          16  MP-030 PIC X(25) VALUE 'POL HLDR - BILL   (EM696)'.     CL**7
00319          16  MP-031 PIC X(25) VALUE 'ISSUE POLICY      (EM697)'.     CL**7
00320          16  MP-032 PIC X(25) VALUE 'POL STATUS RPT    (EM698)'.     CL**7
00321          16  MP-033 PIC X(25) VALUE 'RETRO PYMNTS/ADJ  (EM634)'.     CL**7
00322          16  MP-034 PIC X(25) VALUE 'GRP PRM PYMNT POST(EM638)'.     CL**7
00323          16  MP-035 PIC X(25) VALUE 'COMP MSTR UPDATE  (EM652)'.     CL*20
00324          16  MP-036 PIC X(25) VALUE 'ONLINE REPORTS    (EL179)'.     CL**7
00325          16  MP-037 PIC X(25) VALUE 'ACCOUNT RECEIVABLE(EM250)'.     CL*19
00326          16  MP-038 PIC X(25) VALUE 'COMP PYMNTS/ADJS  (EM235)'.     CL*20
00327          16  MP-039 PIC X(25) VALUE 'BANK MSTR UPDATE  (EM611)'.     CL**8
00328          16  MP-040 PIC X(25) VALUE 'LOAN OFFICER UPDT (EM610)'.     CL**8
00329          16  MP-041 PIC X(25) VALUE 'REINSTATE CANCELS (EM128)'.     CL*14
00330          16  MP-042 PIC X(25) VALUE 'CHECKS(EM677,83,85,86,87)'.     CL*14
00331          16  MP-043 PIC X(25) VALUE 'GENERAL LEDGER    (EM605)'.     CL*19
00332          16  MP-044 PIC X(25) VALUE '****** FUTURE USE  ******'.     CL**6
00333                                                                      CL**6
00334  01  INDIVIDUAL-TABLES    REDEFINES    SYSTEM-APP-TABLES.         EL1031
00335      12  SYSTEM    OCCURS  5 TIMES.                                  CL**6
00336          16  APP-DESCRIPT  OCCURS  44 TIMES  PIC X(25).           EL1031
00337                                                                   EL1031
00338      EJECT                                                        EL1031
00339  01  MISC-WORK-AREAS.                                             EL1031
00340      12  SYS                 PIC S9(4)      VALUE +0    COMP.     EL1031
00341      12  MAXSYS              PIC S9(4)      VALUE +4    COMP.     EL1031
00342      12  SLOT                PIC S9(4)      VALUE +1    COMP.     EL1031
00343      12  MAXSLOT             PIC S9(4)      VALUE +20   COMP.     EL1031
00344      12  APP-SLOT            PIC S9(4)      VALUE +1    COMP.     EL1031
00345      12  MAXAPP              PIC S9(4)      VALUE +44   COMP.     EL1031
00346      12  DSCR-IDX            PIC S9(4)      VALUE +0    COMP.        CL**6
00347      12  ONE                 PIC S9(4)      VALUE +1    COMP.     EL1031
00348      12  SWITCH-MODS-CNT     PIC S9(3)      VALUE +0    COMP-3.   EL1031
00349          88  NO-SWITCH-MODS                 VALUE +0.             EL1031
00350      12  SWITCH-ERROR-CNT    PIC S9(7)      VALUE +0    COMP-3.   EL1031
00351          88  NO-SWITCH-ERRORS               VALUE +0.             EL1031
00352      12  GETMAIN-SPACE       PIC  X         VALUE SPACE.             CL*19
00353                                                                   EL1031
00354  01  SYSTEM-DESCRIPTIONS.                                         EL1031
00355      12  CREDIT      PIC X(26) VALUE 'CREDIT APPLICATIONS -     '.   CL**6
00356      12  CLAIMS      PIC X(26) VALUE 'CLAIMS APPLICATIONS -     '.   CL**6
00357      12  CREDIT-CARD PIC X(26) VALUE 'CREDIT CARD APPLICATIONS -'.   CL**6
00358      12  ACCT-RECV   PIC X(26) VALUE 'ACCT RECV APPLICATIONS -  '.   CL**6
00359      12  LIFE        PIC X(26) VALUE 'CONVENIENCE APPLICATIONS -'.   CL*15
00360      12  GNRL-LDGR   PIC X(26) VALUE 'GENERAL LDGR APPLICATIONS-'.   CL**6
00361                                                                   EL1031
00362  01  DESCRIPTION-TABLE  REDEFINES  SYSTEM-DESCRIPTIONS.           EL1031
00363      12  DESCRIPT  OCCURS  6 TIMES  PIC X(26).                    EL1031
00364                                                                   EL1031
00365  01  ACCESS-KEYS.                                                 EL1031
00366      12  ELCNTL-KEY.                                              EL1031
00367          16  CK-COMP-ID      PIC  X(3).                           EL1031
00368          16  CK-REC-TYPE     PIC  X          VALUE '2'.           EL1031
00369          16  CK-USER-CD      PIC  X(4)       VALUE SPACES.        EL1031
00370          16  CK-SEQ          PIC S9(4)       VALUE +0      COMP.  EL1031
00371                                                                   EL1031
00372      EJECT                                                        EL1031
00373                              COPY ELCDATE.                           CL*10
00374      EJECT                                                        EL1031
00375                              COPY ELCLOGOF.                          CL*10
00376      EJECT                                                        EL1031
00377                              COPY ELCATTR.                           CL*10
00378      EJECT                                                        EL1031
00379                              COPY ELCEMIB.                           CL*10
00380      EJECT                                                        EL1031
00381                              COPY ELCINTF.                           CL*10
00382      12  FILLER  REDEFINES  PI-PROGRAM-WORK-AREA.                 EL1031
00383          16  PI-PREV-USER    PIC  X(4).                           EL1031
00384          16  PI-READ-SW      PIC S9(1).                              CL**6
00385              88  READ-PROCESSOR-ZERO    VALUE +0.                    CL**6
00386              88  READ-PROCESSOR-ONE     VALUE +1.                    CL**6
00387          16  PI-APPLICATION  PIC S9(1).                           EL1031
00388              88  PI-VALID-APP           VALUES ARE +1 +2 +3 +4.      CL**6
00389              88  CREDIT-APP             VALUE +1.                    CL**6
00390              88  CLAIMS-APP             VALUE +2.                    CL**6
00391              88  CREDIT-CARD-APP        VALUE +3.                    CL**6
00392              88  ACCT-RECV-APP          VALUE +4.                    CL**6
00393              88  MORTGAGE-APP           VALUE +1.                    CL**6
00394          16  PI-HIGH-IDX     PIC S9(4)  COMP.                     EL1031
00395          16  PI-LOW-IDX      PIC S9(4)  COMP.                     EL1031
00396          16  FILLER          PIC X(630).                             CL*20
00397                                                                   EL1031
00398      EJECT                                                        EL1031
00399                              COPY ELCJPFX.                           CL*10
00400                              PIC  X(750).                            CL*11
00401      EJECT                                                        EL1031
00402                              COPY ELCAID.                            CL*10
00403                                                                   EL1031
00404  01  FILLER  REDEFINES  DFHAID.                                   EL1031
00405      12  FILLER              PIC  X(8).                           EL1031
00406      12  PF-VALUES           PIC  X          OCCURS 2.            EL1031
00407      EJECT                                                        EL1031
00408                              COPY EL1031S.                           CL*10
00409                                                                   EL1031
00410  01  FILLER  REDEFINES  EL1031AI.                                 EL1031
00411      12  FILLER                  PIC  X(60).                      EL1031
00412      12  APP-INFO                OCCURS 20 TIMES.                 EL1031
00413          16  SLOT-NUM-LENGTH     PIC S9(4)   COMP.                EL1031
00414          16  SLOT-NUM-ATTRB      PIC  X.                          EL1031
00415          16  SLOT-NUM            PIC  99.                         EL1031
00416          16  BLANK-SLOT-NUM  REDEFINES  SLOT-NUM                  EL1031
00417                                  PIC  XX.                         EL1031
00418          16  BRWS-SWITCH-LENGTH  PIC S9(4)   COMP.                EL1031
00419          16  BRWS-SWITCH-ATTRB   PIC  X.                          EL1031
00420          16  BRWS-SWITCH         PIC  X.                          EL1031
00421          16  UPDT-SWITCH-LENGTH  PIC S9(4)   COMP.                EL1031
00422          16  UPDT-SWITCH-ATTRB   PIC  X.                          EL1031
00423          16  UPDT-SWITCH         PIC  X.                          EL1031
00424          16  DSCR-LENGTH         PIC S9(4)   COMP.                EL1031
00425          16  DSCR-ATTRB          PIC  X.                          EL1031
00426          16  DSCR                PIC  X(25).                      EL1031
00427                                                                   EL1031
00428      EJECT                                                        EL1031
00429  LINKAGE SECTION.                                                 EL1031
00430  01  DFHCOMMAREA             PIC  X(1024).                        EL1031
00431                                                                   EL1031
00432 *01 PARMLIST .                                                       CL*19
00433 *    12  FILLER              PIC S9(8)   COMP.                       CL*19
00434 *    12  ELCNTL-POINTER      PIC S9(8)   COMP.                       CL*19
00435      EJECT                                                        EL1031
00436                              COPY ELCCNTL.                           CL*10
00437                                                                   EL1031
00438      EJECT                                                        EL1031
00439  PROCEDURE DIVISION.                                              EL1031
00440                                                                   EL1031
00441      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1031
00442      MOVE '5'                    TO DC-OPTION-CODE.               EL1031
00443                                                                   EL1031
00444      PERFORM 9700-LINK-DATE-CONVERT  THRU  9799-EXIT.             EL1031
00445                                                                   EL1031
00446      MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE.                    EL1031
00447      MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE.                EL1031
00448      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      EL1031
00449      MOVE PI-APPLICATION         TO SYS.                          EL1031
00450                                                                   EL1031
00451 ************************************************************      EL1031
00452 *    IF ATTEMPTING TO EXECUTE PROGRAM WITHOUT SIGNING ON   *      EL1031
00453 *    (COMM LENGTH = ZERO), SEND ERROR MESSAGE.             *      EL1031
00454 ************************************************************      EL1031
00455                                                                   EL1031
00456      IF EIBCALEN  = 0                                             EL1031
00457          GO TO 8800-UNAUTHORIZED-ACCESS.                          EL1031
00458                                                                   EL1031
00459      IF PI-CALLING-PROGRAM  NOT = THIS-PGM                        EL1031
00460          IF PI-RETURN-TO-PROGRAM  NOT = THIS-PGM                  EL1031
00461              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-6     EL1031
00462              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-5     EL1031
00463              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-4     EL1031
00464              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-3     EL1031
00465              MOVE PI-SAVED-PROGRAM-1    TO PI-SAVED-PROGRAM-2     EL1031
00466              MOVE PI-RETURN-TO-PROGRAM  TO PI-SAVED-PROGRAM-1     EL1031
00467              MOVE PI-CALLING-PROGRAM    TO PI-RETURN-TO-PROGRAM   EL1031
00468              MOVE THIS-PGM              TO PI-CALLING-PROGRAM     EL1031
00469          ELSE                                                     EL1031
00470              MOVE PI-RETURN-TO-PROGRAM  TO PI-CALLING-PROGRAM     EL1031
00471              MOVE PI-SAVED-PROGRAM-1    TO PI-RETURN-TO-PROGRAM   EL1031
00472              MOVE PI-SAVED-PROGRAM-2    TO PI-SAVED-PROGRAM-1     EL1031
00473              MOVE PI-SAVED-PROGRAM-3    TO PI-SAVED-PROGRAM-2     EL1031
00474              MOVE PI-SAVED-PROGRAM-4    TO PI-SAVED-PROGRAM-3     EL1031
00475              MOVE PI-SAVED-PROGRAM-5    TO PI-SAVED-PROGRAM-4     EL1031
00476              MOVE PI-SAVED-PROGRAM-6    TO PI-SAVED-PROGRAM-5     EL1031
00477              MOVE SPACES                TO PI-SAVED-PROGRAM-6.    EL1031
00478                                                                   EL1031
00479      EXEC CICS  HANDLE CONDITION                                  EL1031
00480          NOTOPEN   (8870-NOTOPEN)                                 EL1031
00481          NOTFND    (8880-NOT-FOUND)                               EL1031
00482          PGMIDERR  (9600-PGMID-ERROR)                             EL1031
00483          ERROR     (9999-ABEND)                                   EL1031
00484          END-EXEC.                                                EL1031
00485                                                                   EL1031
00486      IF EIBTRNID  NOT = TRANS-ID                                  EL1031
00487          MOVE LOW-VALUES         TO EL1031AO                      EL1031
00488          MOVE PI-APPLICATION     TO SYS                           EL1031
00489          GO TO 7000-BUILD-OUTPUT-MAP.                             EL1031
00490                                                                   EL1031
00491      IF EIBAID  = DFHCLEAR                                        EL1031
00492          GO TO 9400-CLEAR.                                        EL1031
00493                                                                   EL1031
00494      EJECT                                                        EL1031
00495  0200-RECEIVE.                                                    EL1031
00496                                                                   EL1031
00497      MOVE LOW-VALUES             TO EL1031AI.                     EL1031
00498                                                                   EL1031
00499      IF EIBAID  = DFHPA1  OR  DFHPA2  OR  DFHPA3                  EL1031
00500          MOVE ER-7008            TO EMI-ERROR                     EL1031
00501          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL1031
00502          MOVE -1                 TO BRWS1L                        EL1031
00503          GO TO 8200-SEND-DATAONLY.                                EL1031
00504                                                                   EL1031
00505      EXEC CICS  RECEIVE                                           EL1031
00506          MAP     (MAP-NAME)                                       EL1031
00507          MAPSET  (MAPSET-NAME)                                    EL1031
00508          INTO    (EL1031AI)                                       EL1031
00509          END-EXEC.                                                EL1031
00510                                                                   EL1031
00511      IF ENTERPFL  = ZERO                                          EL1031
00512          GO TO 0300-CHECK-PFKEYS.                                 EL1031
00513                                                                   EL1031
00514      IF EIBAID  NOT = DFHENTER                                    EL1031
00515          MOVE ER-0004            TO EMI-ERROR                     EL1031
00516          GO TO 0310-INPUT-ERROR.                                  EL1031
00517                                                                   EL1031
00518      IF ENTERPFI GREATER 0 AND LESS 25                            EL1031
00519          MOVE PF-VALUES (ENTERPFI)  TO EIBAID                     EL1031
00520      ELSE                                                         EL1031
00521          MOVE ER-0029               TO EMI-ERROR                  EL1031
00522          GO TO 0310-INPUT-ERROR.                                  EL1031
00523                                                                   EL1031
00524      EJECT                                                        EL1031
00525  0300-CHECK-PFKEYS.                                               EL1031
00526                                                                   EL1031
00527      IF EIBAID  = DFHPF23                                         EL1031
00528          GO TO 8820-PF23.                                         EL1031
00529                                                                   EL1031
00530      IF EIBAID  = DFHPF24                                         EL1031
00531          GO TO 9200-RETURN-MAIN-MENU.                             EL1031
00532                                                                   EL1031
00533      IF EIBAID  =  DFHPF12                                        EL1031
00534          GO TO 9500-PF12.                                         EL1031
00535                                                                   EL1031
00536      IF EIBAID  = DFHPF1                                          EL1031
00537          GO TO 5000-ROLL-APP-FORWARD.                             EL1031
00538                                                                   EL1031
00539      IF EIBAID  = DFHPF2                                          EL1031
00540          GO TO 5100-ROLL-APP-BACKWARD.                            EL1031
00541                                                                   EL1031
00542      IF EIBAID  = DFHENTER                                        EL1031
00543          GO TO 1000-EDIT-DATA.                                    EL1031
00544                                                                   EL1031
00545      MOVE ER-0029                TO EMI-ERROR.                    EL1031
00546                                                                   EL1031
00547  0310-INPUT-ERROR.                                                EL1031
00548                                                                   EL1031
00549      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL1031
00550                                                                   EL1031
00551      MOVE AL-UNBON               TO ENTERPFA.                     EL1031
00552                                                                   EL1031
00553      IF ENTERPFL = ZERO                                           EL1031
00554          MOVE -1                 TO BRWS1L                        EL1031
00555      ELSE                                                         EL1031
00556          MOVE -1                 TO ENTERPFL.                     EL1031
00557                                                                   EL1031
00558      GO TO 8200-SEND-DATAONLY.                                    EL1031
00559                                                                   EL1031
00560      EJECT                                                        EL1031
00561  1000-EDIT-DATA.                                                  EL1031
00562                                                                   EL1031
00563      IF BRWS-SWITCH-LENGTH(SLOT) EQUAL ZERO AND                   EL1031
00564         UPDT-SWITCH-LENGTH(SLOT) EQUAL ZERO                       EL1031
00565         GO TO 1100-SWITCH-LENGTH-ZERO.                            EL1031
00566                                                                   EL1031
00567      ADD +1                      TO SWITCH-MODS-CNT.              EL1031
00568                                                                      CL*20
00569      IF (BRWS-SWITCH-LENGTH(SLOT) GREATER THAN ZERO)  OR             CL*20
00570         (UPDT-SWITCH-LENGTH(SLOT) GREATER THAN ZERO)                 CL*20
00571          IF PI-USER-ALMIGHTY-YES                                     CL*20
00572              NEXT SENTENCE                                           CL*20
00573          ELSE                                                        CL*20
00574              MOVE ER-0007            TO EMI-ERROR                    CL*20
00575              PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT              CL*20
00576              MOVE LOW-VALUES         TO EL1031AO                     CL*20
00577              GO TO 8200-SEND-DATAONLY.                               CL*20
00578                                                                   EL1031
00579      IF BRWS-SWITCH-LENGTH(SLOT) GREATER THAN ZERO                EL1031
00580          IF BRWS-SWITCH(SLOT) NOT EQUAL 'Y' AND 'N'                  CL*19
00581              ADD +1              TO SWITCH-ERROR-CNT              EL1031
00582              MOVE -1             TO BRWS-SWITCH-LENGTH(SLOT)      EL1031
00583              MOVE AL-UABON       TO BRWS-SWITCH-ATTRB(SLOT).      EL1031
00584                                                                   EL1031
00585      IF UPDT-SWITCH-LENGTH(SLOT) GREATER THAN ZERO                EL1031
00586          IF UPDT-SWITCH(SLOT) NOT EQUAL 'Y' AND 'N'                  CL*19
00587              ADD +1              TO SWITCH-ERROR-CNT              EL1031
00588              MOVE -1             TO UPDT-SWITCH-LENGTH(SLOT)      EL1031
00589              MOVE AL-UABON       TO UPDT-SWITCH-ATTRB(SLOT).      EL1031
00590                                                                   EL1031
00591  1100-SWITCH-LENGTH-ZERO.                                         EL1031
00592                                                                   EL1031
00593      IF SLOT LESS THAN MAXSLOT                                    EL1031
00594          ADD +1                  TO SLOT                          EL1031
00595          GO TO 1000-EDIT-DATA.                                    EL1031
00596                                                                   EL1031
00597      IF NO-SWITCH-MODS                                            EL1031
00598          MOVE -1                 TO BRWS-SWITCH-LENGTH(1)         EL1031
00599          MOVE ER-7322            TO EMI-ERROR                     EL1031
00600          PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                 EL1031
00601          GO TO 8200-SEND-DATAONLY.                                EL1031
00602                                                                   EL1031
00603      IF NOT NO-SWITCH-ERRORS                                      EL1031
00604          MOVE ER-7320            TO EMI-ERROR                     EL1031
00605          PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                 EL1031
00606          GO TO 8200-SEND-DATAONLY.                                EL1031
00607                                                                   EL1031
00608      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                   EL1031
00609      MOVE PI-PREV-USER           TO CK-USER-CD.                      CL**6
00610      MOVE '2'                    TO CK-REC-TYPE.                  EL1031
00611                                                                      CL**6
00612      IF READ-PROCESSOR-ONE                                           CL**6
00613          MOVE PI-READ-SW         TO CK-SEQ.                          CL**6
00614                                                                   EL1031
00615      EXEC CICS  READ                                              EL1031
00616          SET      (ADDRESS OF CONTROL-FILE)                          CL*19
00617          DATASET  (FILE-ID-ELCNTL)                                   CL*19
00618          RIDFLD   (ELCNTL-KEY)                                    EL1031
00619          UPDATE                                                      CL*19
00620          END-EXEC.                                                EL1031
00621                                                                   EL1031
00622      IF CF-LAST-MAINT-BY     NOT = PI-UPDATE-BY  OR               EL1031
00623         CF-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS               EL1031
00624          EXEC CICS  UNLOCK                                        EL1031
00625              DATASET  ('ELCNTL')                                  EL1031
00626              END-EXEC                                             EL1031
00627          MOVE ER-0068            TO EMI-ERROR                     EL1031
00628          PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT               EL1031
00629          GO TO 7000-BUILD-OUTPUT-MAP.                             EL1031
00630                                                                   EL1031
00631      MOVE 'B'                    TO JP-RECORD-TYPE.               EL1031
00632      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL1031
00633                                                                   EL1031
00634      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL1031
00635                                                                   EL1031
00636  1200-UPDATE-LOOP.                                                EL1031
00637                                                                   EL1031
00638      MOVE PI-APPLICATION         TO SYS.                          EL1031
00639                                                                   EL1031
00640      MOVE PI-LOW-IDX             TO APP-SLOT.                     EL1031
00641                                                                   EL1031
00642      PERFORM 2000-UPDATE-RECORD-SWITCHES THRU 2000-EXIT           EL1031
00643          VARYING SLOT  FROM  +1  BY  +1                           EL1031
00644              UNTIL SLOT GREATER THAN MAXSLOT.                     EL1031
00645                                                                   EL1031
00646      MOVE PI-PROCESSOR-ID        TO CF-LAST-MAINT-BY              EL1031
00647                                     PI-UPDATE-BY.                 EL1031
00648      MOVE EIBTIME                TO CF-LAST-MAINT-HHMMSS          EL1031
00649                                     PI-UPDATE-HHMMSS.             EL1031
00650      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              EL1031
00651      MOVE '5'                    TO DC-OPTION-CODE.               EL1031
00652      MOVE LINK-ELDATCV           TO PGM-NAME.                     EL1031
00653                                                                   EL1031
00654      EXEC CICS  LINK                                              EL1031
00655          PROGRAM   (PGM-NAME)                                     EL1031
00656          COMMAREA  (DATE-CONVERSION-DATA)                         EL1031
00657          LENGTH    (DC-COMM-LENGTH)                               EL1031
00658          END-EXEC.                                                EL1031
00659                                                                   EL1031
00660      MOVE DC-BIN-DATE-1          TO CF-LAST-MAINT-DT.             EL1031
00661      MOVE 'C'                    TO JP-RECORD-TYPE.               EL1031
00662      MOVE CONTROL-FILE           TO JP-RECORD-AREA.               EL1031
00663                                                                   EL1031
00664      EXEC CICS  REWRITE                                           EL1031
00665          DATASET  ('ELCNTL')                                      EL1031
00666          FROM     (CONTROL-FILE)                                  EL1031
00667          END-EXEC.                                                EL1031
00668                                                                   EL1031
00669      PERFORM 8400-LOG-JOURNAL-RECORD  THRU  8499-EXIT.            EL1031
00670      MOVE ER-0000                TO EMI-ERROR.                    EL1031
00671      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL1031
00672                                                                   EL1031
00673      MOVE PI-LOW-IDX             TO APP-SLOT.                     EL1031
00674      MOVE LOW-VALUES             TO EL1031AO.                     EL1031
00675      MOVE -1                     TO BRWS1L.                       EL1031
00676      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL1031
00677                                                                   EL1031
00678  2000-UPDATE-RECORD-SWITCHES.                                     EL1031
00679                                                                   EL1031
00680      IF BRWS-SWITCH-LENGTH(SLOT) GREATER THAN ZERO                EL1031
00681          MOVE BRWS-SWITCH(SLOT)  TO CF-BROWSE-APP(SYS APP-SLOT).  EL1031
00682                                                                   EL1031
00683      IF UPDT-SWITCH-LENGTH(SLOT) GREATER THAN ZERO                EL1031
00684          MOVE UPDT-SWITCH(SLOT)  TO CF-UPDATE-APP(SYS APP-SLOT).  EL1031
00685                                                                   EL1031
00686      ADD +1                      TO APP-SLOT.                     EL1031
00687                                                                   EL1031
00688  2000-EXIT.                                                       EL1031
00689      EXIT.                                                        EL1031
00690                                                                   EL1031
00691      EJECT                                                        EL1031
00692  5000-ROLL-APP-FORWARD.                                           EL1031
00693                                                                   EL1031
00694      IF PI-HIGH-IDX GREATER THAN +44                              EL1031
00695          MOVE +01                TO PI-HIGH-IDX                   EL1031
00696          MOVE ER-7299            TO EMI-ERROR                     EL1031
00697          PERFORM 9000-ERROR-FORMAT THRU 9099-EXIT                 EL1031
00698          GO TO 7000-BUILD-OUTPUT-MAP.                             EL1031
00699                                                                   EL1031
00700      COMPUTE APP-SLOT EQUAL PI-HIGH-IDX - +1.                     EL1031
00701                                                                   EL1031
00702      GO TO 7000-BUILD-OUTPUT-MAP.                                 EL1031
00703                                                                   EL1031
00704      EJECT                                                        EL1031
00705  5100-ROLL-APP-BACKWARD.                                          EL1031
00706                                                                   EL1031
00707      IF PI-LOW-IDX LESS THAN MAXSLOT                              EL1031
00708          MOVE +1                 TO PI-LOW-IDX                    EL1031
00709      ELSE                                                         EL1031
00710          COMPUTE PI-LOW-IDX EQUAL PI-LOW-IDX - +19.               EL1031
00711                                                                   EL1031
00712      MOVE PI-LOW-IDX             TO APP-SLOT.                     EL1031
00713                                                                   EL1031
00714      EJECT                                                        EL1031
00715  7000-BUILD-OUTPUT-MAP.                                           EL1031
00716                                                                   EL1031
00717      PERFORM 7500-READ-PROCESSOR THRU 7500-EXIT.                  EL1031
00718                                                                      CL**6
00719      MOVE CF-LAST-MAINT-BY       TO PI-UPDATE-BY.                    CL**6
00720      MOVE CF-LAST-MAINT-HHMMSS   TO PI-UPDATE-HHMMSS.                CL**6
00721                                                                      CL**6
00722      MOVE SYS                    TO DSCR-IDX.                        CL**6
00723                                                                      CL**6
00724      IF READ-PROCESSOR-ONE                                           CL**6
00725          COMPUTE DSCR-IDX EQUAL DSCR-IDX + +4.                       CL**6
00726                                                                      CL**6
00727      PERFORM 7100-FILL-APPLICATION-DISPLAY THRU 7100-EXIT         EL1031
00728          VARYING SLOT FROM ONE BY ONE                             EL1031
00729              UNTIL SLOT GREATER THAN MAXSLOT OR                   EL1031
00730                  APP-SLOT GREATER THAN MAXAPP.                    EL1031
00731                                                                   EL1031
00732      MOVE APP-SLOT               TO PI-HIGH-IDX.                  EL1031
00733                                                                   EL1031
00734      PERFORM 7200-BLANK-DISPLAY THRU 7200-EXIT                    EL1031
00735          VARYING SLOT FROM SLOT BY ONE                            EL1031
00736              UNTIL SLOT GREATER THAN MAXSLOT.                     EL1031
00737                                                                   EL1031
00738      GO TO 8100-SEND-INITIAL-MAP.                                 EL1031
00739                                                                   EL1031
00740  7100-FILL-APPLICATION-DISPLAY.                                   EL1031
00741                                                                   EL1031
00742      IF SLOT EQUAL ONE                                            EL1031
00743          MOVE APP-SLOT           TO PI-LOW-IDX.                   EL1031
00744                                                                   EL1031
00745      MOVE APP-DESCRIPT(DSCR-IDX APP-SLOT)                            CL**6
00746                                  TO DSCR(SLOT).                   EL1031
00747      MOVE CF-BROWSE-APP(SYS APP-SLOT)                             EL1031
00748                                  TO BRWS-SWITCH(SLOT).            EL1031
00749      MOVE CF-UPDATE-APP(SYS APP-SLOT)                             EL1031
00750                                  TO UPDT-SWITCH(SLOT).            EL1031
00751      MOVE APP-SLOT               TO SLOT-NUM(SLOT).               EL1031
00752      ADD +1                      TO APP-SLOT.                     EL1031
00753                                                                   EL1031
00754  7100-EXIT.                                                       EL1031
00755      EXIT.                                                        EL1031
00756                                                                   EL1031
00757  7200-BLANK-DISPLAY.                                              EL1031
00758                                                                   EL1031
00759      MOVE SPACES                 TO DSCR(SLOT)                    EL1031
00760                                     BRWS-SWITCH(SLOT)             EL1031
00761                                     UPDT-SWITCH(SLOT)             EL1031
00762                                     BLANK-SLOT-NUM(SLOT).         EL1031
00763      ADD +1                      TO APP-SLOT.                     EL1031
00764                                                                   EL1031
00765  7200-EXIT.                                                       EL1031
00766      EXIT.                                                        EL1031
00767                                                                   EL1031
00768      EJECT                                                        EL1031
00769  7500-READ-PROCESSOR.                                             EL1031
00770                                                                   EL1031
00771      MOVE PI-COMPANY-ID          TO CK-COMP-ID.                   EL1031
00772      MOVE PI-PREV-USER           TO CK-USER-CD.                      CL**6
00773      MOVE '2'                    TO CK-REC-TYPE.                  EL1031
00774      MOVE PI-READ-SW             TO CK-SEQ.                          CL**6
00775                                                                   EL1031
00776      EXEC CICS  READ                                              EL1031
00777          SET      (ADDRESS OF CONTROL-FILE)                          CL*19
00778          DATASET  (FILE-ID-ELCNTL)                                   CL*19
00779          RIDFLD   (ELCNTL-KEY)                                    EL1031
00780          END-EXEC.                                                EL1031
00781                                                                   EL1031
00782  7500-EXIT.                                                       EL1031
00783      EXIT.                                                        EL1031
00784                                                                   EL1031
00785      EJECT                                                        EL1031
00786  8100-SEND-INITIAL-MAP.                                           EL1031
00787                                                                   EL1031
00788      MOVE SAVE-DATE              TO RUNDTEO.                      EL1031
00789      MOVE EIBTIME                TO TIME-IN.                      EL1031
00790      MOVE TIME-OUT               TO RUNTIMEO.                     EL1031
00791                                                                      CL**6
00792      IF READ-PROCESSOR-ONE                                           CL**6
00793          COMPUTE SYS EQUAL SYS + +4.                                 CL**6
00794                                                                      CL**6
00795      MOVE DESCRIPT(SYS)          TO SCRDSCRO.                        CL**6
00796      MOVE -1                     TO BRWS1L.                       EL1031
00797      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO.                      EL1031
00798                                                                   EL1031
00799      EXEC CICS  SEND                                              EL1031
00800          MAP     (MAP-NAME)                                       EL1031
00801          MAPSET  (MAPSET-NAME)                                    EL1031
00802          FROM    (EL1031AO)                                       EL1031
00803          ERASE                                                    EL1031
00804          CURSOR                                                   EL1031
00805          END-EXEC.                                                EL1031
00806                                                                   EL1031
00807      GO TO 9100-RETURN-TRAN.                                      EL1031
00808                                                                   EL1031
00809  8200-SEND-DATAONLY.                                              EL1031
00810                                                                   EL1031
00811      MOVE SAVE-DATE              TO RUNDTEO.                      EL1031
00812      MOVE EIBTIME                TO TIME-IN.                      EL1031
00813      MOVE TIME-OUT               TO RUNTIMEO.                     EL1031
00814                                                                      CL**6
00815      IF READ-PROCESSOR-ONE                                           CL**6
00816          COMPUTE SYS EQUAL SYS + +4.                                 CL**6
00817                                                                      CL**6
00818      MOVE DESCRIPT(SYS)          TO SCRDSCRO.                        CL**6
00819      MOVE -1                     TO BRWS1L.                       EL1031
00820      MOVE EMI-MESSAGE-AREA (1)   TO ERRMSGO                       EL1031
00821                                                                   EL1031
00822      EXEC CICS  SEND                                              EL1031
00823          MAP     (MAP-NAME)                                       EL1031
00824          MAPSET  (MAPSET-NAME)                                    EL1031
00825          FROM    (EL1031AO)                                       EL1031
00826          DATAONLY                                                 EL1031
00827          CURSOR                                                   EL1031
00828          END-EXEC.                                                EL1031
00829                                                                   EL1031
00830      GO TO 9100-RETURN-TRAN.                                      EL1031
00831                                                                   EL1031
00832  8300-SEND-TEXT.                                                  EL1031
00833                                                                   EL1031
00834      EXEC CICS  SEND TEXT                                         EL1031
00835          FROM    (LOGOFF-TEXT)                                    EL1031
00836          LENGTH  (LOGOFF-LENGTH)                                  EL1031
00837          ERASE                                                    EL1031
00838          FREEKB                                                   EL1031
00839          END-EXEC.                                                EL1031
00840                                                                   EL1031
00841      EXEC CICS  RETURN                                            EL1031
00842          END-EXEC.                                                EL1031
00843                                                                   EL1031
00844  8400-LOG-JOURNAL-RECORD.                                         EL1031
00845                                                                   EL1031
00846      IF PI-JOURNAL-FILE-ID  = ZERO                                EL1031
00847          GO TO 8499-EXIT.                                         EL1031
00848                                                                   EL1031
00849      MOVE PI-PROCESSOR-ID        TO JP-USER-ID.                   EL1031
00850      MOVE FILE-ID-ELCNTL         TO JP-FILE-ID.                      CL*19
00851      MOVE THIS-PGM               TO JP-PROGRAM-ID.                EL1031
00852                                                                   EL1031
012009*    EXEC CICS  JOURNAL                                           EL1031
012009*        JFILEID  (PI-JOURNAL-FILE-ID)                            EL1031
012009*        JTYPEID  ('EL')                                          EL1031
012009*        FROM     (JOURNAL-RECORD)                                EL1031
012009*        LENGTH   (773)                                              CL*11
012009*        END-EXEC.                                                EL1031
           .                                                            EL1031
00860  8499-EXIT.                                                       EL1031
00861      EXIT.                                                        EL1031
00862                                                                   EL1031
00863  8800-UNAUTHORIZED-ACCESS.                                        EL1031
00864                                                                   EL1031
00865      MOVE UNACCESS-MSG           TO LOGOFF-MSG.                   EL1031
00866      GO TO 8300-SEND-TEXT.                                        EL1031
00867                                                                   EL1031
00868  8820-PF23.                                                       EL1031
00869                                                                   EL1031
00870      MOVE EIBAID                 TO PI-ENTRY-CD-1.                EL1031
00871      MOVE XCTL-005               TO PGM-NAME.                     EL1031
00872      GO TO 9300-XCTL.                                             EL1031
00873                                                                   EL1031
00874  8870-NOTOPEN.                                                    EL1031
00875                                                                   EL1031
00876      MOVE ER-0042                TO EMI-ERROR.                    EL1031
00877      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL1031
00878      MOVE -1                     TO BRWS1L.                       EL1031
00879      GO TO 8200-SEND-DATAONLY.                                    EL1031
00880                                                                   EL1031
00881  8880-NOT-FOUND.                                                  EL1031
00882                                                                   EL1031
00883      MOVE ER-0073                TO EMI-ERROR.                    EL1031
00884      PERFORM 9000-ERROR-FORMAT  THRU  9099-EXIT.                  EL1031
00885      MOVE -1                     TO BRWS1L.                       EL1031
00886      MOVE AL-UABON               TO BRWS1A.                       EL1031
00887      GO TO 8200-SEND-DATAONLY.                                    EL1031
00888                                                                   EL1031
00889  9100-RETURN-TRAN.                                                EL1031
00890                                                                   EL1031
00891      MOVE EMI-ERROR-NUMBER (1)   TO PI-LAST-ERROR-NO.             EL1031
00892      MOVE '103B'                 TO PI-CURRENT-SCREEN-NO.         EL1031
00893                                                                   EL1031
00894      EXEC CICS  RETURN                                            EL1031
00895          TRANSID   (TRANS-ID)                                     EL1031
00896          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL1031
00897          LENGTH    (PI-COMM-LENGTH)                               EL1031
00898          END-EXEC.                                                EL1031
00899                                                                   EL1031
00900  9200-RETURN-MAIN-MENU.                                           EL1031
00901                                                                   EL1031
00902      IF PI-SESSION-IN-PROGRESS  = '1'                             EL1031
00903          MOVE XCTL-126           TO PGM-NAME.                     EL1031
00904                                                                   EL1031
00905      IF PI-SESSION-IN-PROGRESS  = '2'                             EL1031
00906          MOVE XCTL-626           TO PGM-NAME.                     EL1031
00907                                                                   EL1031
00908      IF PI-SESSION-IN-PROGRESS  = '4'                             EL1031
00909          MOVE XCTL-400           TO PGM-NAME.                     EL1031
00910                                                                   EL1031
00911      IF PI-SESSION-IN-PROGRESS  = '5'                             EL1031
00912          MOVE XCTL-800           TO PGM-NAME.                     EL1031
00913                                                                   EL1031
00914      GO TO 9300-XCTL.                                             EL1031
00915                                                                   EL1031
00916  9300-XCTL.                                                       EL1031
00917      EXEC CICS  XCTL                                              EL1031
00918          PROGRAM   (PGM-NAME)                                     EL1031
00919          COMMAREA  (PROGRAM-INTERFACE-BLOCK)                      EL1031
00920          LENGTH    (PI-COMM-LENGTH)                               EL1031
00921          END-EXEC.                                                EL1031
00922                                                                   EL1031
00923  9400-CLEAR.                                                      EL1031
00924      MOVE PI-RETURN-TO-PROGRAM   TO PGM-NAME.                     EL1031
00925                                                                   EL1031
00926      GO TO 9300-XCTL.                                             EL1031
00927                                                                   EL1031
00928  9500-PF12.                                                       EL1031
00929      MOVE XCTL-010               TO PGM-NAME.                     EL1031
00930                                                                   EL1031
00931      GO TO 9300-XCTL.                                             EL1031
00932                                                                   EL1031
00933  9600-PGMID-ERROR.                                                EL1031
00934      EXEC CICS  HANDLE CONDITION                                  EL1031
00935          PGMIDERR  (8300-SEND-TEXT)                               EL1031
00936          END-EXEC.                                                EL1031
00937                                                                   EL1031
00938      MOVE PGM-NAME               TO PI-CALLING-PROGRAM.           EL1031
00939      MOVE ' '                    TO PI-ENTRY-CD-1.                EL1031
00940      MOVE XCTL-005               TO PGM-NAME.                     EL1031
00941      MOVE PGM-NAME               TO LOGOFF-PGM.                   EL1031
00942      MOVE PGMIDERR-MSG           TO LOGOFF-FILL.                  EL1031
00943                                                                   EL1031
00944      GO TO 9300-XCTL.                                             EL1031
00945                                                                   EL1031
00946  9700-LINK-DATE-CONVERT.                                          EL1031
00947      EXEC CICS  LINK                                              EL1031
00948          PROGRAM   ('ELDATCV')                                    EL1031
00949          COMMAREA  (DATE-CONVERSION-DATA)                         EL1031
00950          LENGTH    (DC-COMM-LENGTH)                               EL1031
00951          END-EXEC.                                                EL1031
00952                                                                   EL1031
00953  9799-EXIT.                                                       EL1031
00954      EXIT.                                                        EL1031
00955                                                                   EL1031
00956  9000-ERROR-FORMAT.                                               EL1031
00957      IF NOT EMI-ERRORS-COMPLETE                                   EL1031
00958          MOVE LINK-001           TO PGM-NAME                      EL1031
00959          EXEC CICS  LINK                                          EL1031
00960              PROGRAM   (PGM-NAME)                                 EL1031
00961              COMMAREA  (ERROR-MESSAGE-INTERFACE-BLOCK)            EL1031
00962              LENGTH    (EMI-COMM-LENGTH)                          EL1031
00963              END-EXEC.                                            EL1031
00964                                                                   EL1031
00965  9099-EXIT.                                                       EL1031
00966      EXIT.                                                        EL1031
00967                                                                   EL1031
00968  9999-ABEND.                                                      EL1031
00969      MOVE LINK-004               TO PGM-NAME.                     EL1031
00970      MOVE DFHEIBLK               TO EMI-LINE1                     EL1031
00971                                                                   EL1031
00972      EXEC CICS  LINK                                              EL1031
00973          PROGRAM   (PGM-NAME)                                     EL1031
00974          COMMAREA  (EMI-LINE1)                                    EL1031
00975          LENGTH    (72)                                           EL1031
00976          END-EXEC.                                                EL1031
00977                                                                   EL1031
00978      GO TO 8200-SEND-DATAONLY.                                    EL1031
00979                                                                   EL1031
