00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL1523.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 07/18/94 11:29:57.                 
00007 *                            VMOD=2.006.                          
00008 *                                                                 
00008 *                                                                 
00009 *AUTHOR.     LOGIC,INC.                                           
00010 *            DALLAS, TEXAS.                                       
00011                                                                   
00012 *DATE-COMPILED.                                                   
00013                                                                   
00014 *SECURITY.   *****************************************************
00015 *            *                                                   *
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.     *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.    TRANSACTION - EXL1 - LETTER GENERATOR.               
00025 *            THIS PROGRAM GENERATES LETTERS WHEN PROPERLY CALLED  
00026 *            FROM ANOTHER ONLINE PROGRAM.  THIS PROGRAM DOES NOT  
00027 *            CAUSE LETTERS TO BE PRINTED.  INSTEAD IT AUTOMATICALL
00028 *            ENTERS THEM INTO THE ARCHIVE FILE.                   
00029 *                                                                 
00030 *                                                                 
00031 *                                                                 
121802******************************************************************
121802*                   C H A N G E   L O G
121802*
121802* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
121802*-----------------------------------------------------------------
121802*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
121802* EFFECTIVE    NUMBER
121802*-----------------------------------------------------------------
121802* 121802    2001061800003  SMVA  ADD PROCESSING FOR NEW CLM TYPE I
031307* 031307                   PEMA  ADD 3.1 VARIABLE
090108* 090108    2007041300006  AJRA  ADD VARIABLE @@AUTOPYDT FOR AUTO 
033110* 033110  CR2009122800001  AJRA  NAPERSOFT
040110* 040110  CR2009070600002  AJRA  ADD RESEND LETTER ID TO LETTER
100518* 100518  CR2017061500001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
121802******************************************************************
00032                                                                   
00033  ENVIRONMENT DIVISION.                                            
00034                                                                   
00035                                  EJECT                            
00036  DATA DIVISION.                                                   
00037  WORKING-STORAGE SECTION.                                         
pemuni*77  LCP-WS-ADDR-COMP              PIC S9(8) COMP.                
pemuni*77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP     
pemuni*                                  USAGE POINTER.                 
pemuni 77  LCP-WS-ADDR-COMP              PIC x(4) comp-5 value 0.       
pemuni 77  LCP-WS-ADDR-PNTR              REDEFINES LCP-WS-ADDR-COMP     
00040                                    USAGE POINTER.                 
00041  77  FILLER  PIC X(32) VALUE '********************************'.  
00042  77  FILLER  PIC X(32) VALUE '*    EL1523 WORKING STORAGE    *'.  
00043  77  FILLER  PIC X(32) VALUE '********** VMOD=2.006 **********'.  
031307 77  B1                          PIC S999 COMP-3 VALUE +0.
00044                                                                   
00045  01  W-PROGRAM-WORK-AREAS.                                        
00046      12  FILLER                  PIC  X(17)                       
00047                                  VALUE 'PROGRAM WORK AREA'.       
00048      12  W-ASKTIME-CTR           PIC S9(08) COMP.                 
00049      12  W-CORR-TRLR-SEQ         PIC S9(04) COMP.                 
00050      12  W-PI-ADDR-SEQ           PIC S9(04) COMP.                 
00051      12  W-POSITION2             PIC S9(04) COMP.                 
00052      12  W-POSITION21            PIC S9(04) COMP.                 
00053      12  W-SEQ-COUNTER           PIC S9(04) COMP.                 
00054                                                                   
00055      12  W-CURRENT-LINE          PIC S9(03) COMP-3 VALUE +0.      
00056      12  W-ROLL-COUNTER          PIC S9(03) VALUE +0     COMP-3.  
00057      12  W-TOTAL-LINES           PIC S9(03) COMP-3 VALUE +0.      
00058                                                                   
00059      12  W-ARCH-SUPPRESS         PIC ZZZZZZZZ99.                  
00060      12  W-ARCH-EDIT REDEFINES W-ARCH-SUPPRESS                    
00061                                  PIC  X(10).                      
00062                                                                   
00063      12  W-CURRENT-ERROR         PIC  9(04).                      
00064                                                                   
00065      12  W-DATE-WORK             PIC  9(07).                      
00066      12  W-DT-REDEF REDEFINES W-DATE-WORK.                        
00067          16  FILLER              PIC  X(02).                      
00068          16  W-DT-WORK           PIC  9(05).                      
00069                                                                   
00070      12  W-NDX-WORK              PIC  9(02).                      
00071      12  W-SUB                   PIC  9(02).                      
00072      12  W-WORK-AMOUNT           PIC S9(09)V99 VALUE +0.          
00073      12  W-ZIP-NUMERIC           PIC  9(09).                      
00074      12  W-ZIP-NONNUM   REDEFINES  W-ZIP-NUMERIC                  
00075                                  PIC  X(09).                      
00076                                                                   
00077      12  W-ADDR-TYPE-CD.                                          
00078          16  W-ADDR-TYPE         PIC  X(01).                      
00079          16  W-ADDR-SEQ          PIC  X(01).                      
00080          16  W-ADDR-SEQ-NUM REDEFINES                             
00081              W-ADDR-SEQ          PIC  9(01).                      
00082                                                                   
00083      12  W-BEN-HOLD              PIC  X(02).                      
00084      12  W-BENEFIT-WORK          PIC  X(03).                      
00085      12  W-BEN-R REDEFINES W-BENEFIT-WORK.                        
00086          16  W-ELIM-DAYS         PIC  X(02).                      
00087          16  FILLER              PIC  X(01).                      
00088                                                                   
00089      12  W-Z-CONTROL-DATA.                                        
00090          16  W-NUMBER-OF-COPIES  PIC  9(01).                      
00091          16  FILLER              PIC  X(01).                      
00092          16  W-DAYS-TO-FOLLOW-UP PIC  9(03).                      
00093          16  FILLER              PIC  X(01).                      
00094          16  W-DAYS-TO-RESEND-1  PIC  9(03).                      
00095          16  FILLER              PIC  X(01).                      
040110         16  W-FORM-TO-RESEND    PIC  X(4).
040110         16  FILLER              PIC  X(1).
040110         16  W-PROMPT-LETTER     PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-ENCLOSURE-CD      PIC  X(3).
040110         16  FILLER              PIC  X(1).
040110         16  W-AUTO-CLOSE-IND    PIC  X(1).
040110         16  FILLER              PIC  X(1).
040110         16  W-LETTER-TO-BENE    PIC  X(1).                       
00099                                                                   
00100      12  W-GROUPING.                                              
00101          16  W-GROUP-3           PIC  X(03).                      
00102          16  FILLER              PIC  X(03).                      
00103                                                                   
00104      12  W-NAME.                                                  
00105          16  W-FIRST-NAME        PIC  X(12).                      
00106          16  W-MIDDLE-NAME       PIC  X(12).                      
00107          16  W-LAST-NAME         PIC  X(15).                      
00108                                                                   
00109      12  W-CREDIT-CARD-LOAN-NO.                                   
00110          16  W-LOAN-NUMBER       PIC  X(08).                      
00111          16  W-CURRENT-LOAN-NO   PIC  X(12).                      
00112                                                                   
00113      12  W-DEEDIT-FIELD          PIC  X(15).                      
00114      12  W-DEEDIT-FIELD-V0 REDEFINES W-DEEDIT-FIELD               
00115                                  PIC S9(15).                      
00116                                                                   
00117      12  W-CURRENT-SAVE          PIC  X(02).                      
00118      12  W-EDIT-DATE-1.                                           
00119          16  W-ED1-MM            PIC X(02).                       
00120          16  FILLER              PIC X(01)    VALUE '/'.          
00121          16  W-ED1-DD            PIC X(02).                       
00122          16  FILLER              PIC X(01)    VALUE '/'.          
00123          16  W-ED1-YY            PIC X(02).                       
00124      12  W-EDIT-DATE-2.                                           
00125          16  W-ED2-DD            PIC X(02).                       
00126          16  FILLER              PIC X(01)    VALUE '/'.          
00127          16  W-ED2-MM            PIC X(02).                       
00128          16  FILLER              PIC X(01)    VALUE '/'.          
00129          16  W-ED2-YY            PIC X(02).                       
00130                                                                   
00131      12  W-INSURED-LAST-NAME     PIC  X(15).                      
00132      12  W-INSURED-MID-INIT      PIC  X(01).                      
00133      12  W-INSURED-1ST-NAME      PIC  X(12).                      
00134                                                                   
00135      12  W-NAME-WORK.                                             
00136          16  W-NW                PIC  X(01)                       
00137              OCCURS 30 TIMES INDEXED BY W-NWA-NDX.                
00138                                                                   
00139      12  W-NAME-WORK2.                                            
00140          16  W-NW2               PIC  X(01)                       
00141              OCCURS 20 TIMES INDEXED BY W-NWA-NDX2 W-NWA-NDX3.    
00142                                                                   
00143                                                                   
00144      12  W-PGM-NAME              PIC  X(08).                      
00145      12  W-PHONE-IN              PIC  9(11) VALUE ZEROS.          
00146      12  W-PHONE-IN-R   REDEFINES W-PHONE-IN.                     
00147          16  FILLER              PIC  9(01).                      
00148          16  W-PI-AREA           PIC  9(03).                      
00149          16  W-PI-PFX            PIC  9(03).                      
00150          16  W-PI-SFX            PIC  9(04).                      
00151      12  W-PHONE-OUT.                                             
00152          16  W-PO-AREA           PIC  X(03).                      
00153          16  FILLER              PIC  X(01) VALUE '-'.            
00154          16  W-PO-PFX            PIC  X(03).                      
00155          16  FILLER              PIC  X(01) VALUE '-'.            
00156          16  W-PO-SFX            PIC  X(04).                      
00157                                                                   
00158      12  W-SAVE-PROD-RECORD      PIC  X(2000) VALUE SPACES.       
00159      12  W-SAVE-ACCT-RECORD      PIC  X(2000) VALUE SPACES.       
00160      12  W-SAVE-BIN-DATE         PIC  X(02) VALUE SPACES.         
00161      12  W-SAVE-DATE             PIC  X(08) VALUE SPACES.         
00162                                                                   
00163      12  W-SINGLE-LINE           PIC  X(70).                      
00164      12  W-SINGLE-LINE-BY-1 REDEFINES W-SINGLE-LINE.              
00165          16  W-ONE-CHAR OCCURS 70 TIMES INDEXED BY NDX1 NDX2      
00166                                  PIC  X(01).                      
00167      12  W-STATE-LINE            PIC  X(01) VALUE 'N'.            
00168      12  W-TEMP-AREA1.                                            
00169          16  W-TEMP-1 OCCURS 29 TIMES INDEXED BY W-TA1            
00170                                  PIC  X(01).                      
00171      12  W-TEMP-AREA2.                                            
00172          16  W-TEMP-2 OCCURS 30 TIMES INDEXED BY W-TA2 W-TA21     
00173                                                  W-MOVE-NDX       
00174                                  PIC  X(01).                      
00175                                                                   
00176      12  W-TEMP-CURR-LINE        PIC S9(03)   COMP-3.             
00177      12  W-TIME-IN               PIC S9(07).                      
00178      12  W-TIME-OUT-R REDEFINES W-TIME-IN.                        
00179          16  FILLER              PIC  X(01).                      
00180          16  W-TIME-OUT          PIC  9(02)V9(02).                
00181          16  FILLER              PIC  X(02).                      
00182      12  W-VAR-HOLD.                                              
00183          16  W-V1                PIC  X(01).                      
00184          16  W-V2                PIC  X(01).                      
00185          16  W-V3                PIC  X(01).                      
00186          16  W-V4                PIC  X(01).                      
00187      12  W-V-HOLD REDEFINES W-VAR-HOLD.                           
00188          16  W-V-NUM             PIC  9(02).                      
00189          16  W-V-PERIOD          PIC  X(01).                      
00190          16  W-V-DECIMAL         PIC  9(01).                      
00191                                                                   
00192      12  W-WORD-LENGTH           PIC S9(04)  COMP-3.              
00193                                                                   
00194      12  W-ZIP-CODE.                                              
00195          16  W-AM-ZIP-CODE       PIC  X(05).                      
00196          16  W-AM-ZIP-DASH       PIC  X(01).                      
00197          16  W-AM-ZIP-PLUS4      PIC  X(04).                      
00198      12  W-ZIP-CODE-CANADIAN   REDEFINES  W-ZIP-CODE.             
00199          16  W-CAN-POSTAL-1      PIC  X(03).                      
00200          16  FILLER              PIC  X(01).                      
00201          16  W-CAN-POSTAL-2      PIC  X(03).                      
00202          16  FILLER              PIC  X(03).                      
00203                                                                   
00204  01  W-PROGRAM-SWITCHES.                                          
00205      12  FILLER                  PIC  X(16)                       
00206                                       VALUE 'PROGRAM SWITCHES'.   
00207                                                                   
00208      12  W-ACCT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.            
00209      12  W-ACCT-READ-SW          PIC  X(01) VALUE ' '.            
00210      12  W-PROD-BROWSE-STARTED   PIC  X(01) VALUE 'N'.            
00211      12  W-PROD-READ-SW          PIC  X(01) VALUE ' '.            
00212      12  W-ACTV-BROWSE-STARTED   PIC  X(01) VALUE 'N'.            
00213      12  W-COMP-READ-SW          PIC  X(01) VALUE ' '.            
00214      12  W-DATA-FOUND-SW         PIC  X(01).                      
00215          88  NO-CHARACTERS-FOUND            VALUE 'N'.            
00216      12  W-GETMAIN-SW            PIC  9(01) VALUE 0.              
00217          88  W-NO-GETMAIN-DONE-YET          VALUE 0.              
00218      12  W-LABELS-SW             PIC  X(01) VALUE SPACE.          
00219      12  W-NAME-SW               PIC S9(01) COMP-3 VALUE ZERO.    
00220      12  W-REVERSE-DATE-SW       PIC X(01)  VALUE SPACES.         
00221          88  W-REVERSE-DATE                 VALUE 'Y'.            
00222      12  W-TEXT-BROWSE-STARTED   PIC  X(01) VALUE 'N'.            
00223                                                                   
00224                                  EJECT                            
00225  01  W-ACCESS-KEYS.                                               
00226      12  FILLER                  PIC  X(11)                       
00227                                  VALUE 'ACCESS KEYS'.             
00228                                                                   
00229      12  W-ACCT-KEY.                                              
00230          16  W-ACCT-PARTIAL-KEY.                                  
00231              20  W-ACCT-CO       PIC  X(01).                      
00232              20  W-ACCT-CARRIER  PIC  X(01).                      
00233              20  W-ACCT-GROUPING PIC  X(6).                       
00234              20  W-ACCT-STATE    PIC  X(02).                      
00235              20  W-ACCT-ACCOUNT  PIC  X(10).                      
00236          16  W-ACCT-EXP-DATE     PIC  X(02).                      
00237                                                                   
00238      12  W-PROD-KEY.                                              
00239          16  W-PROD-PARTIAL-KEY.                                  
00240              20  W-PROD-CO       PIC  X(01).                      
00241              20  W-PROD-CARRIER  PIC  X(01).                      
00242              20  W-PROD-GROUPING PIC  X(6).                       
00243              20  W-PROD-STATE    PIC  X(02).                      
00244              20  W-PROD-PRODUCER PIC  X(10).                      
00245          16  W-PROD-EXP-DATE     PIC  X(02).                      
00246                                                                   
00247      12  W-ACTV-KEY.                                              
00248          16  W-ACTV-PARTIAL-KEY.                                  
00249              20  W-ACTV-CO       PIC  X(01).                      
00250              20  W-ACTV-CARRIER  PIC  X(01).                      
00251              20  W-ACTV-CLAIM    PIC  X(07).                      
00252              20  W-ACTV-CERT-NUM PIC  X(11).                      
00253          16  W-ACTV-SEQ          PIC S9(04)   VALUE +0    COMP.   
00254                                                                   
00255      12  W-ARCH-KEY.                                              
00256          16  W-ARCH-PARTIAL-KEY.                                  
00257              20  W-ARCH-CO       PIC  X(01).                      
00258              20  W-ARCH-NUMBER   PIC S9(08)      COMP.            
00259          16  W-ARCH-REC-TYPE     PIC  X(01).                      
00260          16  W-ARCH-SEQ          PIC S9(04)      COMP VALUE +0.   
00261                                                                   
00262      12  W-BENE-KEY.                                              
00263          16  W-BENE-COMP-CD      PIC  X(01).                      
00264          16  W-BENE-REC-TYPE     PIC  X(01).                      
00265          16  W-BENE-NUMBER.                                       
00266              20  W-BENE-CREDITOR PIC  X(03).                      
00267              20  W-BENE-FILLER   PIC  X(07).                      
00268                                                                   
00269      12  W-CERT-KEY.                                              
00270          16  W-CERT-CO           PIC  X(01).                      
00271          16  W-CERT-CARRIER      PIC  X(01).                      
00272          16  W-CERT-GROUPING     PIC  X(6).                       
00273          16  W-CERT-STATE        PIC  X(02).                      
00274          16  W-CERT-ACCOUNT      PIC  X(10).                      
00275          16  W-CERT-EFF-DT       PIC  X(02).                      
00276          16  W-CERT-CERT-NUM     PIC  X(11).                      
00277                                                                   
00278      12  W-PLCY-KEY.                                              
00279          16  W-PLCY-CO           PIC  X(01).                      
00280          16  W-PLCY-CARRIER      PIC  X(01).                      
00281          16  W-PLCY-GROUPING     PIC  X(6).                       
00282          16  W-PLCY-STATE        PIC  X(02).                      
00283          16  W-PLCY-PRODUCER     PIC  X(10).                      
00284          16  W-PLCY-EFF-DT       PIC  X(02).                      
00285          16  W-PLCY-REFERENCE-NO PIC  X(20).                      
00286                                                                   
00287      12  W-CNTL-KEY.                                              
00288          16  W-CNTL-CO           PIC  X(03).                      
00289          16  W-CNTL-RECORD-TYPE  PIC  X(01) VALUE '1'.            
00290          16  W-CNTL-GENL.                                         
00291              20  W-CNTL-GEN1     PIC  X(02) VALUE SPACES.         
00292              20  W-CNTL-GEN2.                                     
00293                 24  W-CNTL-GEN3  PIC  X(01) VALUE SPACES.         
00294                 24  W-CNTL-GEN4  PIC  X(01) VALUE SPACES.         
00295          16  W-CNTL-SEQ          PIC S9(04) VALUE +0     COMP.    
00296                                                                   
00297      12  W-CLM-KEY.                                               
00298          16  W-CLM-CO            PIC  X(01).                      
00299          16  W-CLM-CARRIER       PIC  X(01).                      
00300          16  W-CLM-CLAIM         PIC  X(07).                      
00301          16  W-CLM-CERT-NUM      PIC  X(11).                      
00302                                                                   
00303       12  W-COMP-KEY.                                             
00304           16  W-COMP-COMPANY-CD  PIC  X(01).                      
00305           16  W-COMP-CARRIER     PIC  X(01).                      
00306           16  W-COMP-GROUPING    PIC  X(06).                      
00307           16  W-COMP-RESP-NO     PIC  X(10).                      
00308           16  W-COMP-ACCOUNT     PIC  X(10).                      
00309           16  W-COMP-TYPE        PIC  X(01).                      
00310                                                                   
00311      12  W-TEXT-KEY.                                              
00312          16  W-TEXT-PARTIAL-KEY.                                  
00313              20  W-TEXT-CO       PIC  X(01).                      
00314              20  W-TEXT-LETTER   PIC  X(04).                      
00315          16  W-TEXT-FILLER       PIC  X(08)   VALUE SPACES.       
00316          16  W-TEXT-SEQ          PIC S9(04)   VALUE +0    COMP.   
00317                                                                   
00318      12  W-PLAN-KEY.                                              
00319          16  W-PLAN-CO           PIC  X(01).                      
00320          16  W-PLAN-CARRIER      PIC  X(01).                      
00321          16  W-PLAN-GROUPING     PIC  X(06).                      
00322          16  W-PLAN-STATE        PIC  X(02).                      
00323          16  W-PLAN-PRODUCER     PIC  X(10).                      
00324          16  W-PLAN-CODE         PIC  X(02).                      
00325          16  W-PLAN-REV-NO       PIC  9(03).                      
00326                                                                   
00327      12  W-ARCH-SAVE-KEY         PIC  X(05).                      
00328      12  W-ACTV-SAVE-KEY         PIC  X(20).                      
00329      12  W-ACCT-SAVE-KEY         PIC  X(20).                      
00330      12  W-PROD-SAVE-KEY         PIC  X(20).                      
00331      12  W-TEXT-SAVE-KEY         PIC  X(05).                      
00332                                  EJECT                            
00333  01  W-PROGRAM-INTERFACE.                                         
00334      12  FILLER                  PIC  X(14)                       
00335                                  VALUE 'INTERFACE AREA'.          
00336                                  COPY ELCINTF.                    
00337      12  PI-WA REDEFINES PI-PROGRAM-WORK-AREA.                    
00338          16  PI-EL1523-WA.                                        
00339              20  PI-FORM-NUMBER  PIC  X(04).                      
00340              20  PI-NUMBER-COPIES                                 
00341                                  PIC  9(01).                      
00342              20  PI-ADDR-TYPE    PIC  X(02).                      
00343              20  PI-FOLLOW-UP-DATE                                
00344                                  PIC  X(02).                      
00345              20  PI-RESEND-DATE  PIC  X(02).                      
00346              20  PI-ERROR-CODE   PIC  9(04).                      
00347                  88  PI-NO-ERRORS-DETECTED VALUE 0000.            
00348                  88  PI-FATAL-ERROR VALUES  0006 0013 0042        
00349                                             0154 0168 0169        
00350                                             0172 0179 0186        
00351                                             0281 0332 2055        
00352                                             3697 3698 3699        
00353                                             3770 3771 3772        
00354                                             7675 9106 9808        
00355                                             9883 9887.            
00356              20  PI-REASON       PIC  X(70).                      
00357              20  PI-ARCHIVE-NUMBER                                
00358                                  PIC  9(08).                      
00359              20  PI-ACCT-POINTER PIC S9(08) COMP.                 
00360              20  PI-ACTV-POINTER PIC S9(08) COMP.                 
00361              20  PI-ARCH-POINTER PIC S9(08) COMP.                 
00362              20  PI-VAR-POINTER  PIC S9(08) COMP.                 
00363              20  PI-PROD-POINTER PIC S9(08) COMP.                 
090108*00364          16  FILLER              PIC X(527).               
090108         16  PI-AUTO-LETTER-DATE  PIC X(10).
040110         16  PI-RESEND-FORM-NUMBER PIC X(4).
040110         16  PI-PROMPT-LETTER     PIC X.
040110         16  PI-ENCLOSURE-CD      PIC X(3).
040110         16  PI-AUTO-CLOSE-IND    PIC X(1).
040110         16  PI-LETTER-TO-BENE    PIC X(1).
040110         16  FILLER               PIC X(507).
00365                                  EJECT                            
00366  01  W-PROGRAM-TABLE             PIC  X(14)                       
00367                                      VALUE 'PROGRAM TABLES'.      
00368  01  W-LABEL-HOLD-AREA.                                           
00369      12  W-LABEL-LINES OCCURS 6 TIMES                             
00370                         INDEXED BY W-NDX   W-NDX2.                
00371          16  W-LABEL-ZIP.                                         
00372              20  W-LABEL-1ST-ZIP                                  
00373                                  PIC  X(05).                      
00374              20  FILLER          PIC  X(01).                      
00375              20  W-LABEL-2ND-ZIP                                  
00376                                  PIC  X(04).                      
00377          16  FILLER              PIC  X(09).                      
00378          16  W-LAST-DIGIT        PIC  X(01).                      
00379          16  W-LAST-ZIP.                                          
00380              20  W-LAST-1ST-ZIP  PIC  X(05).                      
00381              20  FILLER          PIC  X(01).                      
00382              20  W-LAST-2ND-ZIP PIC   X(04).                      
00383                                                                   
00384                                                                   
00385  01  W-RECORD-TABLE              PIC  X(21900) VALUE SPACES.      
00386  01  W-REC-TABLE REDEFINES W-RECORD-TABLE.                        
00387      12  TS-GROUP OCCURS 6 TIMES INDEXED BY TS-NDX                
00388                                  PIC  X(3650).                    
00389                                                                   
00390  01  W-REC-ENTRIES REDEFINES W-RECORD-TABLE.                      
00391      12  W-REC-ENT OCCURS 300 TIMES INDEXED BY W-TB-NDX W-TB-NDX1.
00392          16  W-REC-TEXT          PIC  X(70).                      
00393          16  W-REC-PC            PIC  9(02).                      
00394          16  FILLER              PIC  X(01).                      
00395                                                                   
00396                                  EJECT                            
00397  01  W-PROGRAM-VARIABLE-AREA.                                     
00398      12  FILLER                  PIC  X(13)                       
00399                                       VALUE 'VARIABLE AREA'.      
00400 ****************************************************              
00401 *       WHEN ADDING OR DELETING ENTRIES TO         *              
00402 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *              
00403 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *              
00404 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *              
00405 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *              
00406 *       ALSO YOU NEED TO INCREASE THE LENGTH OF    *              
00407 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *              
00408 ****************************************************              
00409                                                                   
00410 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF    
00411 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA     
00412                                                                   
00413      12  SS-NUM-ENTRIES          PIC  9(03) VALUE 116      COMP-3.
00414      12  SS-COUNTER              PIC  9(03)                COMP-3.
00415      12  SS-WORK-AREA-LENGTH     PIC S9(04) VALUE +3340    COMP.  
00416                                                                   
00417  01  W-VARIABLE-WORK-AREA.                                        
00418      12  W-VAR-CODE              PIC  X(04).                      
00419      12  W-VAR-LEN               PIC  9(02).                      
00420      12  W-VAR-DATA          PIC  X(100).                         
00421      12  W-VAR-DATA-R REDEFINES W-VAR-DATA.                       
00422        16  W-VAR-W-ONE-CHAR OCCURS 100 TIMES                      
00423                         INDEXED BY W-NDXV                         
00424                                  PIC  X(01).                      
00425  01  SYSTEM-SUPPORTED-VARIABLES.                                  
00426 *****COMPANY NAME                                                 
00427      12  SS01                    PIC  X(04) VALUE    '01.0'.      
00428      12  SS01L                   PIC  9(02) VALUE 36.             
00429      12  SS01D                   PIC  X(30) VALUE ALL '*'.        
00430 *****FULL COMPANY ADDRESS                                         
00431      12  SS02-1                  PIC  X(04) VALUE    '02.1'.      
00432      12  SS02-1L                 PIC  9(02) VALUE 36.             
00433      12  SS02-1D                 PIC  X(30) VALUE ALL '*'.        
00434      12  SS02-2                  PIC  X(04) VALUE    '02.2'.      
00435      12  SS02-2L                 PIC  9(02) VALUE 36.             
00436      12  SS02-2D                 PIC  X(30) VALUE ALL '*'.        
00437      12  SS02-3                  PIC  X(04) VALUE    '02.3'.      
00438      12  SS02-3L                 PIC  9(02) VALUE 36.             
00439      12  SS02-3D                 PIC  X(30) VALUE ALL '*'.        
00440      12  SS02-4                  PIC  X(04) VALUE    '02.4'.      
00441      12  SS02-4L                 PIC  9(02) VALUE 36.             
00442      12  SS02-4D                 PIC  X(30) VALUE ALL '*'.        
00443      12  SS02-5                  PIC  X(04) VALUE    '02.5'.      
00444      12  SS02-5L                 PIC  9(02) VALUE 36.             
00445      12  SS02-5D                 PIC  X(30) VALUE ALL '*'.        
00446 *****CARRIER NAME                                                 
00447      12  SS03                    PIC  X(04) VALUE    '03.0'.      
00448      12  SS03L                   PIC  9(02) VALUE 36.             
00449      12  SS03D                   PIC  X(30) VALUE ALL '*'.        
031307*****INVESTORS HERITAGE
031307* as Administrator for Investors Heritage Life Insurance Company
031307     12  SS03-1                  PIC X(4)  VALUE     '03.1'.      
031307     12  SS03-1L                 PIC 99    VALUE 68.              
031307     12  SS03-1D                 PIC X(62) VALUE ALL '*'.         
00450 *****FULL CARRIER ADDRESS                                         
00451      12  SS04-1                  PIC  X(04) VALUE    '04.1'.      
00452      12  SS04-1L                 PIC  9(02) VALUE 36.             
00453      12  SS04-1D                 PIC  X(30) VALUE ALL '*'.        
00454      12  SS04-2                  PIC  X(04) VALUE    '04.2'.      
00455      12  SS04-2L                 PIC  9(02) VALUE 36.             
00456      12  SS04-2D                 PIC  X(30) VALUE ALL '*'.        
00457      12  SS04-3                  PIC  X(04) VALUE    '04.3'.      
00458      12  SS04-3L                 PIC  9(02) VALUE 36.             
00459      12  SS04-3D                 PIC  X(30) VALUE ALL '*'.        
00460      12  SS04-4                  PIC  X(04) VALUE    '04.4'.      
00461      12  SS04-4L                 PIC  9(02) VALUE 36.             
00462      12  SS04-4D                 PIC  X(30) VALUE ALL '*'.        
00463      12  SS04-5                  PIC  X(04) VALUE    '04.5'.      
00464      12  SS04-5L                 PIC  9(02) VALUE 36.             
00465      12  SS04-5D                 PIC  X(30) VALUE ALL '*'.        
00466 *****CARRIER PHONE NUMBER                                         
00467      12  SS04-6                  PIC  X(04) VALUE    '04.6'.      
00468      12  SS04-6L                 PIC  9(02) VALUE 18.             
00469      12  SS04-6D                 PIC  X(12) VALUE ALL '*'.        
00470 *****FULL ADDRESEE LABEL                                          
00471      12  SS05-1                  PIC  X(04) VALUE    '05.1'.      
00472      12  SS05-1L                 PIC  9(02) VALUE 36.             
00473      12  SS05-1D                 PIC  X(30) VALUE ALL '*'.        
00474      12  SS05-2                  PIC  X(04) VALUE    '05.2'.      
00475      12  SS05-2L                 PIC  9(02) VALUE 36.             
00476      12  SS05-2D                 PIC  X(30) VALUE ALL '*'.        
00477      12  SS05-3                  PIC  X(04) VALUE    '05.3'.      
00478      12  SS05-3L                 PIC  9(02) VALUE 36.             
00479      12  SS05-3D                 PIC  X(30) VALUE ALL '*'.        
00480      12  SS05-4                  PIC  X(04) VALUE    '05.4'.      
00481      12  SS05-4L                 PIC  9(02) VALUE 36.             
00482      12  SS05-4D                 PIC  X(30) VALUE ALL '*'.        
00483      12  SS05-5                  PIC  X(04) VALUE    '05.5'.      
00484      12  SS05-5L                 PIC  9(02) VALUE 36.             
00485      12  SS05-5D                 PIC  X(30) VALUE ALL '*'.        
00486      12  SS05-6                  PIC  X(04) VALUE    '05.6'.      
00487      12  SS05-6L                 PIC  9(02) VALUE 36.             
00488      12  SS05-6D                 PIC  X(30) VALUE ALL '*'.        
00489 *****ACCOUNT NAME                                                 
00490      12  SS06                    PIC  X(04) VALUE    '06.0'.      
00491      12  SS06L                   PIC  9(02) VALUE 36.             
00492      12  SS06D                   PIC  X(30) VALUE ALL '*'.        
00493 *****FULL ACCOUNT ADDRESS                                         
00494      12  SS07-1                  PIC  X(04) VALUE    '07.1'.      
00495      12  SS07-1L                 PIC  9(02) VALUE 36.             
00496      12  SS07-1D                 PIC  X(30) VALUE ALL '*'.        
00497      12  SS07-2                  PIC  X(04) VALUE    '07.2'.      
00498      12  SS07-2L                 PIC  9(02) VALUE 36.             
00499      12  SS07-2D                 PIC  X(30) VALUE ALL '*'.        
00500      12  SS07-3                  PIC  X(04) VALUE    '07.3'.      
00501      12  SS07-3L                 PIC  9(02) VALUE 36.             
00502      12  SS07-3D                 PIC  X(30) VALUE ALL '*'.        
00503      12  SS07-4                  PIC  X(04) VALUE    '07.4'.      
00504      12  SS07-4L                 PIC  9(02) VALUE 36.             
00505      12  SS07-4D                 PIC  X(30) VALUE ALL '*'.        
00506      12  SS07-5                  PIC  X(04) VALUE    '07.5'.      
00507      12  SS07-5L                 PIC  9(02) VALUE 36.             
00508      12  SS07-5D                 PIC  X(30) VALUE ALL '*'.        
00509 *****ACCOUNT PHONE NUMBER                                         
00510      12  SS07-6                  PIC  X(04) VALUE    '07.6'.      
00511      12  SS07-6L                 PIC  9(02) VALUE 18.             
00512      12  SS07-6D                 PIC  X(12) VALUE ALL '*'.        
00513 *****EXECUTING PROCESSOR NAME                                     
00514      12  SS08                    PIC  X(04) VALUE    '08.0'.      
00515      12  SS08L                   PIC  9(02) VALUE 36.             
00516      12  SS08D                   PIC  X(30) VALUE ALL '*'.        
00517 *****PROCESSOR TITLE                                              
00518      12  SS09                    PIC  X(04) VALUE    '09.0'.      
00519      12  SS09L                   PIC  9(02) VALUE 32.             
00520      12  SS09D                   PIC  X(26) VALUE ALL '*'.        
00521 *****INSUREDS NAME                                                
00522      12  SS10                    PIC  X(04) VALUE    '10.0'.      
00523      12  SS10L                   PIC  9(02) VALUE 36.             
00524      12  SS10D                   PIC  X(30) VALUE ALL '*'.        
00525 *****INSUREDS ADDRESS                                             
00526      12  SS11-1                  PIC  X(04) VALUE    '11.1'.      
00527      12  SS11-1L                 PIC  9(02) VALUE 36.             
00528      12  SS11-1D                 PIC  X(30) VALUE ALL '*'.        
00529      12  SS11-2                  PIC  X(04) VALUE    '11.2'.      
00530      12  SS11-2L                 PIC  9(02) VALUE 36.             
00531      12  SS11-2D                 PIC  X(30) VALUE ALL '*'.        
00532      12  SS11-3                  PIC  X(04) VALUE    '11.3'.      
00533      12  SS11-3L                 PIC  9(02) VALUE 36.             
00534      12  SS11-3D                 PIC  X(30) VALUE ALL '*'.        
00535      12  SS11-4                  PIC  X(04) VALUE    '11.4'.      
00536      12  SS11-4L                 PIC  9(02) VALUE 36.             
00537      12  SS11-4D                 PIC  X(30) VALUE ALL '*'.        
00538 *****INSUREDS NAME FROM ADDR TRAILER                              
00539      12  SS11-5                  PIC  X(04) VALUE    '11.5'.      
00540      12  SS11-5L                 PIC  9(02) VALUE 36.             
00541      12  SS11-5D                 PIC  X(30) VALUE ALL '*'.        
00542 *****INSUREDS PHONE NUMBER FROM ADDR TRAILER                      
00543      12  SS11-6                  PIC  X(04) VALUE    '11.6'.      
00544      12  SS11-6L                 PIC  9(02) VALUE 18.             
00545      12  SS11-6D                 PIC  X(12) VALUE ALL '*'.        
00546 *****CLAIM TYPE NAME                                              
00547      12  SS12                    PIC  X(04) VALUE    '12.0'.      
00548      12  SS12L                   PIC  9(02) VALUE 12.             
00549      12  SS12D                   PIC  X(6) VALUE ALL '*'.         
00550 *****CLAIM INCURRED DATE                                          
00551      12  SS13                    PIC  X(04) VALUE    '13.0'.      
00552      12  SS13L                   PIC  9(02) VALUE 14.             
00553      12  SS13D                   PIC  X(08) VALUE ALL '*'.        
00554 *****CLAIM REPORTED DATE                                          
00555      12  SS14                    PIC  X(04) VALUE    '14.0'.      
00556      12  SS14L                   PIC  9(02) VALUE 14.             
00557      12  SS14D                   PIC  X(08) VALUE ALL '*'.        
00558 *****LAST PAYMENT DATE                                            
00559      12  SS15                    PIC  X(04) VALUE    '15.0'.      
00560      12  SS15L                   PIC  9(02) VALUE 14.             
00561      12  SS15D                   PIC  X(08) VALUE ALL '*'.        
00562 *****LAST PAYMENT AMOUNT                                          
00563      12  SS16                    PIC  X(04) VALUE    '16.0'.      
00564      12  SS16L                   PIC  9(02) VALUE 17.             
00565      12  SS16D                   PIC $$$$,$$$.99 VALUE ZEROS.     
00566 *****CLAIM PAID THRU/TO DATE                                      
00567      12  SS17                    PIC  X(04) VALUE    '17.0'.      
00568      12  SS17L                   PIC  9(02) VALUE 14.             
00569      12  SS17D                   PIC  X(08) VALUE ALL '*'.        
00570 *****TOTAL PAID TO DATE                                           
00571      12  SS18                    PIC  X(04) VALUE    '18.0'.      
00572      12  SS18L                   PIC  9(02) VALUE 17.             
00573      12  SS18D                   PIC $$$$,$$$.99 VALUE ZEROS.     
00574 *****DIAGNOSIS OR CAUSE                                           
00575      12  SS19                    PIC  X(04) VALUE    '19.0'.      
00576      12  SS19L                   PIC  9(02) VALUE 32.             
00577      12  SS19D                   PIC  X(26) VALUE ALL '*'.        
00578 *****CAUSE CODE                                                   
00579      12  SS19-1                  PIC  X(04) VALUE    '19.1'.      
00580      12  SS19-1L                 PIC  9(02) VALUE 12.             
00581      12  SS19-1D                 PIC  X(6) VALUE ALL '*'.         
00582 *****CURRENT DATE                                                 
00583      12  SS20                    PIC  X(04) VALUE    '20.0'.      
00584      12  SS20L                   PIC  9(02) VALUE 14.             
00585      12  SS20D                   PIC  X(08) VALUE ALL '*'.        
00586 *****FULL CURRENT DATE                                            
00587      12  SS21                    PIC  X(04) VALUE    '21.0'.      
00588      12  SS21L                   PIC  9(02) VALUE 24.             
00589      12  SS21D                   PIC  X(18) VALUE ALL '*'.        
00590 *****BENEFIT DESCRIPTION                                          
00591      12  SS22                    PIC  X(04) VALUE    '22.0'.      
00592      12  SS22L                   PIC  9(02) VALUE 16.             
00593      12  SS22D                   PIC  X(10) VALUE ALL '*'.        
00594 *****CARRIER CODE IN CERT                                         
00595      12  SS23                    PIC  X(04) VALUE    '23.0'.      
00596      12  SS23L                   PIC  9(02) VALUE 9.              
00597      12  SS23D                   PIC  X(03) VALUE ALL '*'.        
00598 *****GROUPING CODE IN CERT                                        
00599      12  SS24                    PIC  X(04) VALUE    '24.0'.      
00600      12  SS24L                   PIC  9(02) VALUE 12.             
00601      12  SS24D                   PIC  X(6) VALUE ALL '*'.         
00602 *****ACCOUNT NUMBER IN CERT                                       
00603      12  SS25                    PIC  X(04) VALUE    '25.0'.      
00604      12  SS25L                   PIC  9(02) VALUE 16.             
00605      12  SS25D                   PIC  X(10) VALUE ALL '*'.        
00606 *****CERTIFICATE NUMBER                                           
00607      12  SS26                    PIC  X(04) VALUE    '26.0'.      
00608      12  SS26L                   PIC  9(02) VALUE 17.             
00609      12  SS26D                   PIC  X(11) VALUE ALL '*'.        
00610 *****CERT EFFECTIVE DATE                                          
00611      12  SS27                    PIC  X(04) VALUE    '27.0'.      
00612      12  SS27L                   PIC  9(02) VALUE 14.             
00613      12  SS27D                   PIC  X(08) VALUE ALL '*'.        
00614 *****CERT EXPIRATION DATE                                         
00615      12  SS28                    PIC  X(04) VALUE    '28.0'.      
00616      12  SS28L                   PIC  9(02) VALUE 14.             
00617      12  SS28D                   PIC  X(08) VALUE ALL '*'.        
00618 *****APPLICABLE COVERAGE TERM                                     
00619      12  SS29                    PIC  X(04) VALUE    '29.0'.      
00620      12  SS29L                   PIC  9(02) VALUE 9.              
00621      12  SS29D                   PIC  X(03) VALUE ALL '*'.        
00622 *****APPLICABLE COVERAGE AMOUNT                                   
00623      12  SS30                    PIC  X(04) VALUE    '30.0'.      
00624      12  SS30L                   PIC  9(02) VALUE 18.             
00625      12  SS30D                   PIC $$$$$,$$$.99 VALUE ZEROS.    
00626 *****APPLICABLE COVERAGE CANCEL DATE                              
00627      12  SS31                    PIC  X(04) VALUE    '31.0'.      
00628      12  SS31L                   PIC  9(02) VALUE 14.             
00629      12  SS31D                   PIC  X(08) VALUE ALL '*'.        
00630 *****APPLICABLE COVERAGE FORM NUMBER                              
00631      12  SS32                    PIC  X(04) VALUE    '32.0'.      
00632      12  SS32L                   PIC  9(02) VALUE 18.             
00633      12  SS32D                   PIC  X(12) VALUE ALL '*'.        
00634 *****INSURES AGE AT POLICY ISSUE                                  
00635      12  SS33                    PIC  X(04) VALUE    '33.0'.      
00636      12  SS33L                   PIC  9(02) VALUE 9.              
00637      12  SS33D                   PIC  X(03) VALUE ALL '*'.        
00638 *****CLAIM NUMBER                                                 
00639      12  SS34                    PIC  X(04) VALUE    '34.0'.      
00640      12  SS34L                   PIC  9(02) VALUE 13.             
00641      12  SS34D                   PIC  X(07) VALUE ALL '*'.        
00642 *****LAST DENIAL TEXT                                             
00643      12  SS35-1                  PIC  X(04) VALUE    '35.1'.      
00644      12  SS35-1L                 PIC  9(02) VALUE 66.             
00645      12  SS35-1D                 PIC  X(60) VALUE ALL '*'.        
00646      12  SS35-2                  PIC  X(04) VALUE    '35.2'.      
00647      12  SS35-2L                 PIC  9(02) VALUE 66.             
00648      12  SS35-2D                 PIC  X(60) VALUE ALL '*'.        
00649 *****LOAN NUMBER                                                  
00650      12  SS36                    PIC  X(04) VALUE    '36.0'.      
00651      12  SS36L                   PIC  9(02) VALUE 14.             
00652      12  SS36D                   PIC  X(08) VALUE ALL '*'.        
00653 *****CREDIT CARD LOAN NUMBER                                      
00654      12  SS36-1                  PIC  X(04) VALUE    '36.1'.      
00655      12  SS36-1L                 PIC  9(02) VALUE 26.             
00656      12  SS36-1D                 PIC  X(20) VALUE ALL '*'.        
00657 *****LOAN BALANCE                                                 
00658      12  SS37                    PIC  X(04) VALUE    '37.0'.      
00659      12  SS37L                   PIC  9(02) VALUE 18.             
00660      12  SS37D                   PIC $$$$$,$$$.99 VALUE ZEROS.    
00661 *****MEMBER NUMBER                                                
00662      12  SS38                    PIC  X(04) VALUE    '38.0'.      
00663      12  SS38L                   PIC  9(02) VALUE 18.             
00664      12  SS38D                   PIC  X(12) VALUE ALL '*'.        
00665 *****INSURED NAME (FIRST M LAST)                                  
00666      12  SS39                    PIC  X(04) VALUE    '39.0'.      
00667      12  SS39L                   PIC  9(02) VALUE 36.             
00668      12  SS39D                   PIC  X(30) VALUE ALL '*'.        
00669 *****INSURED LAST NAME ONLY                                       
00670      12  SS40                    PIC  X(04) VALUE    '40.0'.      
00671      12  SS40L                   PIC  9(02) VALUE 21.             
00672      12  SS40D                   PIC  X(15) VALUE ALL '*'.        
00673 *****TITLE (MR/MS)                                                
00674      12  SS41                    PIC  X(04) VALUE    '41.0'.      
00675      12  SS41L                   PIC  9(02) VALUE 9.              
00676      12  SS41D                   PIC  X(03) VALUE ALL '*'.        
00677 *****ELIMINATION PERIOD                                           
00678      12  SS42                    PIC  X(04) VALUE    '42.0'.      
00679      12  SS42L                   PIC  9(02) VALUE 9.              
00680      12  SS42D                   PIC  X(03) VALUE ALL '*'.        
00681 *****BENEFICIARY NAME                                             
00682      12  SS43                    PIC  X(04) VALUE    '43.0'.      
00683      12  SS43L                   PIC  9(02) VALUE 36.             
00684      12  SS43D                   PIC  X(30) VALUE ALL '*'.        
00685 *****BENEFICIARY ADDRESS                                          
00686      12  SS44-1                  PIC  X(04) VALUE    '44.1'.      
00687      12  SS44-1L                 PIC  9(02) VALUE 36.             
00688      12  SS44-1D                 PIC  X(30) VALUE ALL '*'.        
00689      12  SS44-2                  PIC  X(04) VALUE    '44.2'.      
00690      12  SS44-2L                 PIC  9(02) VALUE 36.             
00691      12  SS44-2D                 PIC  X(30) VALUE ALL '*'.        
00692      12  SS44-3                  PIC  X(04) VALUE    '44.3'.      
00693      12  SS44-3L                 PIC  9(02) VALUE 36.             
00694      12  SS44-3D                 PIC  X(30) VALUE ALL '*'.        
00695      12  SS44-4                  PIC  X(04) VALUE    '44.4'.      
00696      12  SS44-4L                 PIC  9(02) VALUE 36.             
00697      12  SS44-4D                 PIC  X(30) VALUE ALL '*'.        
00698      12  SS44-5                  PIC  X(04) VALUE    '44.5'.      
00699      12  SS44-5L                 PIC  9(02) VALUE 18.             
00700      12  SS44-5D                 PIC  X(12) VALUE ALL '*'.        
00701 *****INSUREDS DATE OF BIRTH                                       
00702      12  SS45                    PIC  X(04) VALUE    '45.0'.      
00703      12  SS45L                   PIC  9(02) VALUE 14.             
00704      12  SS45D                   PIC  X(08) VALUE ALL '*'.        
00705 *****INSUREDS SOC SEC NUMBER                                      
00706      12  SS46                    PIC  X(04) VALUE    '46.0'.      
00707      12  SS46L                   PIC  9(02) VALUE 17.             
00708      12  SS46D                   PIC  X(11) VALUE ALL '*'.        
00709 *****PHYSICIANS  NAME                                             
00710      12  SS47                    PIC  X(04) VALUE    '47.0'.      
00711      12  SS47L                   PIC  9(02) VALUE 36.             
00712      12  SS47D                   PIC  X(30) VALUE ALL '*'.        
00713 *****PHYSICIANS  ADDRESS                                          
00714      12  SS47-1                  PIC  X(04) VALUE    '47.1'.      
00715      12  SS47-1L                 PIC  9(02) VALUE 36.             
00716      12  SS47-1D                 PIC  X(30) VALUE ALL '*'.        
00717      12  SS47-2                  PIC  X(04) VALUE    '47.2'.      
00718      12  SS47-2L                 PIC  9(02) VALUE 36.             
00719      12  SS47-2D                 PIC  X(30) VALUE ALL '*'.        
00720      12  SS47-3                  PIC  X(04) VALUE    '47.3'.      
00721      12  SS47-3L                 PIC  9(02) VALUE 36.             
00722      12  SS47-3D                 PIC  X(30) VALUE ALL '*'.        
00723      12  SS47-4                  PIC  X(04) VALUE    '47.4'.      
00724      12  SS47-4L                 PIC  9(02) VALUE 36.             
00725      12  SS47-4D                 PIC  X(30) VALUE ALL '*'.        
00726      12  SS47-5                  PIC  X(04) VALUE    '47.5'.      
00727      12  SS47-5L                 PIC  9(02) VALUE 18.             
00728      12  SS47-5D                 PIC  X(12) VALUE ALL '*'.        
00729 *****EMPLOYERS   NAME                                             
00730      12  SS48                    PIC  X(04) VALUE    '48.0'.      
00731      12  SS48L                   PIC  9(02) VALUE 36.             
00732      12  SS48D                   PIC  X(30) VALUE ALL '*'.        
00733 *****EMPLOYERS   ADDRESS                                          
00734      12  SS48-1                  PIC  X(04) VALUE    '48.1'.      
00735      12  SS48-1L                 PIC  9(02) VALUE 36.             
00736      12  SS48-1D                 PIC  X(30) VALUE ALL '*'.        
00737      12  SS48-2                  PIC  X(04) VALUE    '48.2'.      
00738      12  SS48-2L                 PIC  9(02) VALUE 36.             
00739      12  SS48-2D                 PIC  X(30) VALUE ALL '*'.        
00740      12  SS48-3                  PIC  X(04) VALUE    '48.3'.      
00741      12  SS48-3L                 PIC  9(02) VALUE 36.             
00742      12  SS48-3D                 PIC  X(30) VALUE ALL '*'.        
00743      12  SS48-4                  PIC  X(04) VALUE    '48.4'.      
00744      12  SS48-4L                 PIC  9(02) VALUE 36.             
00745      12  SS48-4D                 PIC  X(30) VALUE ALL '*'.        
00746      12  SS48-5                  PIC  X(04) VALUE    '48.5'.      
00747      12  SS48-5L                 PIC  9(02) VALUE 18.             
00748      12  SS48-5D                 PIC  X(12) VALUE ALL '*'.        
00749 *****OTHER1      NAME                                             
00750      12  SS49                    PIC  X(04) VALUE    '49.0'.      
00751      12  SS49L                   PIC  9(02) VALUE 36.             
00752      12  SS49D                   PIC  X(30) VALUE ALL '*'.        
00753 *****OTHER1      ADDRESS                                          
00754      12  SS49-1                  PIC  X(04) VALUE    '49.1'.      
00755      12  SS49-1L                 PIC  9(02) VALUE 36.             
00756      12  SS49-1D                 PIC  X(30) VALUE ALL '*'.        
00757      12  SS49-2                  PIC  X(04) VALUE    '49.2'.      
00758      12  SS49-2L                 PIC  9(02) VALUE 36.             
00759      12  SS49-2D                 PIC  X(30) VALUE ALL '*'.        
00760      12  SS49-3                  PIC  X(04) VALUE    '49.3'.      
00761      12  SS49-3L                 PIC  9(02) VALUE 36.             
00762      12  SS49-3D                 PIC  X(30) VALUE ALL '*'.        
00763      12  SS49-4                  PIC  X(04) VALUE    '49.4'.      
00764      12  SS49-4L                 PIC  9(02) VALUE 36.             
00765      12  SS49-4D                 PIC  X(30) VALUE ALL '*'.        
00766      12  SS49-5                  PIC  X(04) VALUE    '49.5'.      
00767      12  SS49-5L                 PIC  9(02) VALUE 18.             
00768      12  SS49-5D                 PIC  X(12) VALUE ALL '*'.        
00769 *****OTHER2      NAME                                             
00770      12  SS50                    PIC  X(04) VALUE    '50.0'.      
00771      12  SS50L                   PIC  9(02) VALUE 36.             
00772      12  SS50D                   PIC  X(30) VALUE ALL '*'.        
00773 *****OTHER2      ADDRESS                                          
00774      12  SS50-1                  PIC  X(04) VALUE    '50.1'.      
00775      12  SS50-1L                 PIC  9(02) VALUE 36.             
00776      12  SS50-1D                 PIC  X(30) VALUE ALL '*'.        
00777      12  SS50-2                  PIC  X(04) VALUE    '50.2'.      
00778      12  SS50-2L                 PIC  9(02) VALUE 36.             
00779      12  SS50-2D                 PIC  X(30) VALUE ALL '*'.        
00780      12  SS50-3                  PIC  X(04) VALUE    '50.3'.      
00781      12  SS50-3L                 PIC  9(02) VALUE 36.             
00782      12  SS50-3D                 PIC  X(30) VALUE ALL '*'.        
00783      12  SS50-4                  PIC  X(04) VALUE    '50.4'.      
00784      12  SS50-4L                 PIC  9(02) VALUE 36.             
00785      12  SS50-4D                 PIC  X(30) VALUE ALL '*'.        
00786      12  SS50-5                  PIC  X(04) VALUE    '50.5'.      
00787      12  SS50-5L                 PIC  9(02) VALUE 18.             
00788      12  SS50-5D                 PIC  X(12) VALUE ALL '*'.        
00789 *****A&H TERM TIMES MON. BEN.                                     
00790      12  SS51                    PIC  X(04) VALUE    '51.0'.      
00791      12  SS51L                   PIC  9(02) VALUE 17.             
00792      12  SS51D                   PIC $$$$,$$$.99  VALUE ZEROS.    
00793 *****THIRD PARTY NAME                                             
00794      12  SS52                    PIC  X(04) VALUE    '52.0'.      
00795      12  SS52L                   PIC  9(02) VALUE 36.             
00796      12  SS52D                   PIC  X(30) VALUE ALL '*'.        
00797 *****THIRD PARTY ADDRESS                                          
00798      12  SS53-1                  PIC  X(04) VALUE    '53.1'.      
00799      12  SS53-1L                 PIC  9(02) VALUE 36.             
00800      12  SS53-1D                 PIC  X(30) VALUE ALL '*'.        
00801      12  SS53-2                  PIC  X(04) VALUE    '53.2'.      
00802      12  SS53-2L                 PIC  9(02) VALUE 36.             
00803      12  SS53-2D                 PIC  X(30) VALUE ALL '*'.        
00804      12  SS53-3                  PIC  X(04) VALUE    '53.3'.      
00805      12  SS53-3L                 PIC  9(02) VALUE 36.             
00806      12  SS53-3D                 PIC  X(30) VALUE ALL '*'.        
00807      12  SS53-4                  PIC  X(04) VALUE    '53.4'.      
00808      12  SS53-4L                 PIC  9(02) VALUE 36.             
00809      12  SS53-4D                 PIC  X(30) VALUE ALL '*'.        
00810      12  SS53-5                  PIC  X(04) VALUE    '53.5'.      
00811      12  SS53-5L                 PIC  9(02) VALUE 36.             
00812      12  SS53-5D                 PIC  X(30) VALUE ALL '*'.        
00813 *****THIRD PARTY PHONE NUMBER                                     
00814      12  SS53-6                  PIC  X(04) VALUE    '53.6'.      
00815      12  SS53-6L                 PIC  9(02) VALUE 18.             
00816      12  SS53-6D                 PIC  X(12) VALUE ALL '*'.        
00817 *****CERTIFICATE SEQUENCE                                         
00818      12  SS54                    PIC  X(04) VALUE    '54.0'.      
00819      12  SS54L                   PIC  9(02) VALUE 09.             
00820      12  SS54D                   PIC  X(03) VALUE ALL '*'.        
00821 *****CERTIFICATE TOTAL  E                                         
00822      12  SS55                    PIC  X(04) VALUE    '55.0'.      
00823      12  SS55L                   PIC  9(02) VALUE 09.             
00824      12  SS55D                   PIC  X(03) VALUE ALL '*'.        
00825 *****CREDITOR ID                                                  
00826      12  SS56                    PIC  X(04) VALUE    '56.0'.      
00827      12  SS56L                   PIC  9(02) VALUE 36.             
00828      12  SS56D                   PIC  X(30) VALUE ALL '*'.        
00829 *****INSUREDS NAME (CERTIFICATE)                                  
00830      12  SS57                    PIC X(4)  VALUE     '57.0'.      
00831      12  SS57L                   PIC 99    VALUE 36.              
00832      12  SS57D                   PIC X(30) VALUE ALL '*'.         
00833 *****JOINTS NAME (CERTIFICATE)                                    
00834      12  SS58                    PIC X(4)  VALUE     '58.0'.      
00835      12  SS58L                   PIC 99    VALUE 36.              
00836      12  SS58D                   PIC X(30) VALUE ALL '*'.         
00837 *****POLICY REFERENCE NUMBER                                      
00838      12  SS59                    PIC X(4)  VALUE     '59.0'.      
00839      12  SS59L                   PIC 99    VALUE 26.              
00840      12  SS59D                   PIC X(20) VALUE ALL '*'.         
00841                                                                   
00842 ****************************************************              
00843 *       WHEN ADDING OR DELETING ENTRIES TO         *              
00844 *       THE SYSTEM-SUPPORTED-VARIABLES THE         *              
00845 *       SS-NUM-ENTRIES FIELD MUST BE ALTERED       *              
00846 *       TO MATCH THE NUMBER OF ENTRIES IN THE      *              
00847 *       SYSTEM-SUPPORTED-VARIABLE TABLE.           *              
00848 *       ALSO YOU NEED           TO INCREASE THE LENGTH OF *       
00849 *       SS-WORK-AREA-LENGTH AND SYSTEM-VARIABLES   *              
00850 ****************************************************              
00851                                  EJECT                            
00852                                  COPY ELCDATE.                    
00853                                  EJECT                            
00854  01  W-PROGRAM-CONSTANTS.                                         
00855      12  FILLER                  PIC  X(17)                       
00856                                  VALUE 'PROGRAM CONSTANTS'.       
00857      12  W-ACCT-LENGTH           PIC S9(04) COMP  VALUE +2000.    
00858      12  W-PROD-LENGTH           PIC S9(04) COMP  VALUE +2000.    
00859      12  W-ACTV-LENGTH           PIC S9(04) COMP  VALUE +200.     
00860      12  W-ARCH-LENGTH           PIC S9(04) COMP  VALUE +90.      
033110     12  W-NAPS-LENGTH           PIC S9(04) COMP  VALUE +150.
00861                                                                   
00862      12  W-ACCT-ID               PIC  X(08)   VALUE 'ERACCT'.     
00863      12  W-ACTV-ID               PIC  X(08)   VALUE 'ELTRLR'.     
00864      12  W-ARCH-ID               PIC  X(08)   VALUE 'ELARCH'.     
00865      12  W-BENE-ID               PIC  X(08)   VALUE 'ELBENE'.     
00866      12  W-CLM-ID                PIC  X(08)   VALUE 'ELMSTR'.     
00867      12  W-CNTL-ID               PIC  X(08)   VALUE 'ELCNTL'.     
00868      12  W-CERT-ID               PIC  X(08)   VALUE 'ELCERT'.     
00869      12  W-PROD-ID               PIC  X(08)   VALUE 'MPPROD'.     
00870      12  W-PLCY-ID               PIC  X(08)   VALUE 'MPPLCY'.     
00871      12  W-PLAN-ID               PIC  X(08)   VALUE 'MPPLAN'.     
033110     12  W-NAPS-ID               PIC  X(08)   VALUE 'ELNAPS'.
00872      12  W-GETMAINSPACE          PIC  X(01) VALUE SPACE.          
00873      12  W-LINK-ELDATCV          PIC  X(07) VALUE 'ELDATCV'.      
00874      12  W-LOWER-CASE                                             
00875                     PIC  X(26) VALUE 'abcdefghijklmnopqrstuvwxyz'.
00876      12  W-MAX-LINES             PIC  9(03) VALUE 300.            
00877      12  W-TEXT-ID               PIC  X(08) VALUE 'ELLETR'.       
00878      12  W-TOP-FORM              PIC  X(70)                       
00879          VALUE '*****TOP OF FORM *****'.                          
00880      12  W-TRANSACTION           PIC  X(04) VALUE 'EXL1'.         
00881      12  W-UPPER-CASE                                             
00882                     PIC  X(26) VALUE 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'.
00883                                  EJECT                            
00884  01  ERROR-MESSAGES.                                              
00885      12  ER-0006                 PIC  X(04) VALUE '0006'.         
00886      12  ER-0013                 PIC  X(04) VALUE '0013'.         
00887      12  ER-0042                 PIC  X(04) VALUE '0042'.         
00888      12  ER-0051                 PIC  X(04) VALUE '0051'.         
00889      12  ER-0154                 PIC  X(04) VALUE '0154'.         
00890      12  ER-0168                 PIC  X(04) VALUE '0168'.         
00891      12  ER-0169                 PIC  X(04) VALUE '0169'.         
00892      12  ER-0172                 PIC  X(04) VALUE '0172'.         
00893      12  ER-0176                 PIC  X(04) VALUE '0176'.         
00894      12  ER-0178                 PIC  X(04) VALUE '0178'.         
00895      12  ER-0179                 PIC  X(04) VALUE '0179'.         
00896      12  ER-0180                 PIC  X(04) VALUE '0180'.         
00897      12  ER-0181                 PIC  X(04) VALUE '0181'.         
00898      12  ER-0186                 PIC  X(04) VALUE '0186'.         
00899      12  ER-0191                 PIC  X(04) VALUE '0191'.         
00900      12  ER-0206                 PIC  X(04) VALUE '0206'.         
00901      12  ER-0281                 PIC  X(04) VALUE '0281'.         
00902      12  ER-0332                 PIC  X(04) VALUE '0332'.         
00903      12  ER-0413                 PIC  X(04) VALUE '0413'.         
00904      12  ER-2055                 PIC  X(04) VALUE '2055'.         
00905      12  ER-3697                 PIC  X(04) VALUE '3697'.         
00906      12  ER-3698                 PIC  X(04) VALUE '3698'.         
00907      12  ER-3699                 PIC  X(04) VALUE '3699'.         
00908      12  ER-3766                 PIC  X(04) VALUE '3766'.         
00909      12  ER-3770                 PIC  X(04) VALUE '3770'.         
00910      12  ER-3771                 PIC  X(04) VALUE '3771'.         
00911      12  ER-3772                 PIC  X(04) VALUE '3772'.         
00912      12  ER-7675                 PIC  X(04) VALUE '7675'.         
00913      12  ER-9106                 PIC  X(04) VALUE '9106'.         
00914      12  ER-9483                 PIC  X(04) VALUE '9483'.         
00915      12  ER-9808                 PIC  X(04) VALUE '9808'.         
00916      12  ER-9883                 PIC  X(04) VALUE '9883'.         
00917      12  ER-9886                 PIC  X(04) VALUE '9886'.         
00918      12  ER-9887                 PIC  X(04) VALUE '9887'.         
00919                                                                   
00920                                  EJECT                            
00921  LINKAGE SECTION.                                                 
00922  01  DFHCOMMAREA                 PIC  X(1024).                    
00923                                                                   
00924 *01 PARMLIST .                                                    
00925 *    02  FILLER                  PIC S9(08)  COMP.                
00926 *    02  L-ACCT-POINTER          PIC S9(08)  COMP.                
00927 *    02  L-ACTV-POINTER          PIC S9(08)  COMP.                
00928 *    02  L-ARCH-POINTER          PIC S9(08)  COMP.                
00929 *    02  L-BENE-POINTER          PIC S9(08)  COMP.                
00930 *    02  L-CERT-POINTER          PIC S9(08)  COMP.                
00931 *    02  L-CLM-POINTER           PIC S9(08)  COMP.                
00932 *    02  L-CNTL-POINTER          PIC S9(08)  COMP.                
00933 *    02  L-COMP-POINTER          PIC S9(08)  COMP.                
00934 *    02  L-TEXT-POINTER          PIC S9(08)  COMP.                
00935 *    02  L-PROD-POINTER          PIC S9(08)  COMP.                
00936 *    02  L-PLCY-POINTER          PIC S9(08)  COMP.                
00937 *    02  L-PLAN-POINTER          PIC S9(08)  COMP.                
00938 *    02  L-VARIABLE-POINTER      PIC S9(08)  COMP.                
00939                                                                   
00940                                  EJECT                            
00941                                  COPY ERCACCT.                    
00942                                  EJECT                            
00943                                  COPY ELCTRLR.                    
00944                                  EJECT                            
00945                                  COPY ELCARCH.                    
00946                                  EJECT                            
00947                                  COPY ELCBENE.                    
00948                                  EJECT                            
00949                                  COPY ELCCERT.                    
00950                                  EJECT                            
00951                                  COPY ELCMSTR.                    
00952                                  EJECT                            
00953                                  COPY ELCCNTL.                    
00954                                  EJECT                            
00955                                  COPY ERCCOMP.                    
00956                                  EJECT                            
00957                                  COPY ELCTEXT.                    
00958                                  EJECT                            
00959                                  COPY MPCPROD.                    
00960                                  EJECT                            
00961                                  COPY MPCPLCY.                    
00962                                  EJECT                            
00963                                  COPY MPCPLAN.                    
00964                                  EJECT                            
033110                                 COPY ELCNAPS.
033110                                 EJECT
00965 *  THE SYSTEM-VARIABLES  FIELD LENGTH MUST MATCH THE LENGTH OF    
00966 *  THE SS-WORK-AREA-LENGTH FIELD FOR THE W-VARIABLE-WORK-AREA     
00967                                                                   
00968  01  SYSTEM-VARIABLES            PIC  X(3340).                    
00969  01  SYS-VAR-ENTRY REDEFINES SYSTEM-VARIABLES.                    
00970      12  SYS-VAR-CODE            PIC  X(04).                      
00971      12  SYS-VAR-LEN             PIC  9(02).                      
00972      12  SYS-VAR-DATA            PIC  X(100).                     
00973                                                                   
00974                                  EJECT                            
00975  PROCEDURE DIVISION.                                              
00976                                                                   
00977 *    SERVICE RELOAD PARMLIST.                                     
00978                                                                   
00979      MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK.      
00980                                                                   
00981      MOVE EIBDATE                TO DC-JULIAN-YYDDD.              
00982      MOVE '5'                    TO DC-OPTION-CODE.               
00983      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
00984      MOVE DC-GREG-DATE-1-EDIT    TO W-SAVE-DATE.                  
00985      MOVE DC-BIN-DATE-1          TO W-SAVE-BIN-DATE.              
00986                                                                   
00987      MOVE W-SAVE-BIN-DATE        TO W-CURRENT-SAVE.               
00988                                                                   
00989      EXEC CICS HANDLE CONDITION                                   
00990          PGMIDERR (9600-PGMID-ERROR)                              
00991          ERROR    (9990-ABEND)                                    
00992      END-EXEC.                                                    
00993                                                                   
00994                                                                   
00995      MOVE ZEROS                  TO PI-ERROR-CODE                 
00996                                     W-PI-ADDR-SEQ                 
00997                                     W-ADDR-SEQ                    
00998                                     W-CURRENT-LINE                
00999                                     W-TOTAL-LINES.                
01000                                                                   
01001      EXEC CICS SYNCPOINT                                          
01002      END-EXEC.                                                    
01003                                  EJECT                            
01004  0100-PROCESS-REQUEST.                                            
01005                                                                   
01006      PERFORM 1000-EDIT-ROUTINE THRU 1000-EXIT.                    
01007                                                                   
01008      IF  PI-FATAL-ERROR                                           
01009          GO TO 0200-RETURN-TO-CALLING-PGM.                        
01010                                                                   
01011      PERFORM 2000-CREATE-LETTER THRU 2999-EXIT.                   
01012                                                                   
01013      IF  PI-FATAL-ERROR                                           
01014          GO TO 0200-RETURN-TO-CALLING-PGM.                        
01015                                                                   
01016      PERFORM 6000-ARCHIVE-LETTER THRU 6000-EXIT.                  
01017                                                                   
01018  0100-EXIT.                                                       
01019      EXIT.                                                        
01020                                  EJECT                            
01021  0200-RETURN-TO-CALLING-PGM.                                      
01022                                                                   
01023      IF  PI-FATAL-ERROR                                           
01024          EXEC CICS SYNCPOINT ROLLBACK                             
01025          END-EXEC.                                                
01026                                                                   
01027      MOVE PROGRAM-INTERFACE-BLOCK                                 
01028                                  TO DFHCOMMAREA.                  
01029                                                                   
01030      EXEC CICS RETURN                                             
01031      END-EXEC.                                                    
01032                                                                   
01033  0200-EXIT.                                                       
01034      EXIT.                                                        
01035                                  EJECT                            
01036  1000-EDIT-ROUTINE.                                               
01037                                                                   
01038      PERFORM 1100-SET-CODES THRU 1100-EXIT.                       
01039                                                                   
01040      IF  PI-RESEND-DATE EQUAL SPACES                              
01041          MOVE LOW-VALUES         TO PI-RESEND-DATE.               
01042                                                                   
01043      IF  PI-FOLLOW-UP-DATE   EQUAL SPACES                         
01044          MOVE LOW-VALUES         TO PI-FOLLOW-UP-DATE.            
01045                                                                   
01046      IF  PI-ADDR-TYPE GREATER THAN LOW-VALUES                     
01047          MOVE PI-ADDR-TYPE       TO W-ADDR-TYPE-CD                
01048                                                                   
01049          IF  PI-ADDR-TYPE EQUAL 'I' OR 'B' OR 'A' OR 'P'          
01050                                OR 'O' OR 'Q' OR 'E'               
01051              NEXT SENTENCE                                        
01052                                                                   
01053          ELSE                                                     
01054              MOVE ER-0176        TO PI-ERROR-CODE                 
01055              PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.           
01056                                                                   
01057  1000-EXIT.                                                       
01058       EXIT.                                                       
01059                                  EJECT                            
01060  1100-SET-CODES.                                                  
01061                                                                   
01062      MOVE PI-COMPANY-ID          TO W-CNTL-CO.                    
01063                                                                   
01064      MOVE PI-COMPANY-CD          TO W-CLM-CO                      
01065                                     W-TEXT-CO                     
01066                                     W-ACTV-CO                     
01067                                     W-CERT-CO                     
01068                                     W-BENE-COMP-CD                
01069                                     W-COMP-COMPANY-CD             
01070                                     W-ACCT-CO                     
01071                                     W-ARCH-CO                     
01072                                     W-PROD-CO                     
01073                                     W-PLCY-CO                     
01074                                     W-PLAN-CO.                    
01075                                                                   
01076      MOVE PI-CARRIER             TO W-CLM-CARRIER                 
01077                                     W-ACTV-CARRIER                
01078                                     W-CERT-CARRIER                
01079                                     W-ACCT-CARRIER                
01080                                     W-PROD-CARRIER                
01081                                     W-PLCY-CARRIER                
01082                                     W-PLAN-CARRIER.               
01083                                                                   
01084      MOVE PI-CERT-NO             TO W-CLM-CERT-NUM                
01085                                     W-ACTV-CERT-NUM               
01086                                     W-CERT-CERT-NUM.              
01087                                                                   
01088      MOVE PI-CLAIM-NO            TO W-CLM-CLAIM                   
01089                                     W-ACTV-CLAIM.                 
01090                                                                   
01091      MOVE W-ACTV-KEY             TO W-ACTV-SAVE-KEY.              
01092                                                                   
01093  1100-EXIT.                                                       
01094       EXIT.                                                       
01095                                  EJECT                            
01096  2000-CREATE-LETTER.                                              
01097 ***************************************************************   
01098 *    THIS ROUTINE WILL CREATE A NEW LETTER BY READING THE     *   
01099 *    TEXT FILE WITH THE FORM CODE SPECIFIED FROM THE SCREEN.  *   
01100 *    ALL VARIABLE SYMBOLS WILL BE RESOLVED AND THE LETTER     *   
01101 *    WILL BE DISPLAYED ONTO THE SCREEN.                       *   
01102 *                                                             *   
01103 ***************************************************************   
01104                                                                   
01105      MOVE SPACES                 TO W-RECORD-TABLE.               
01106                                                                   
01107      PERFORM 3000-READ-ADDR THRU 3999-EXIT.                       
01108                                                                   
01109      IF  PI-FATAL-ERROR                                           
01110          GO TO 2999-EXIT.                                         
01111                                                                   
01112      SET W-TB-NDX                TO 7.                            
01113      MOVE W-TOP-FORM             TO W-REC-TEXT (W-TB-NDX).        
01114      SET W-TB-NDX UP BY 1.                                        
01115                                                                   
01116      MOVE PI-FORM-NUMBER         TO W-TEXT-LETTER.                
01117      MOVE W-TEXT-PARTIAL-KEY     TO W-TEXT-SAVE-KEY.              
01118                                                                   
01119      EXEC CICS HANDLE CONDITION                                   
01120           NOTFND     (2120-ENDBR)                                 
01121           ENDFILE    (2120-ENDBR)                                 
01122           NOTOPEN    (2900-TEXT-NOT-OPEN)                         
01123      END-EXEC                                                     
01124                                                                   
01125      EXEC CICS STARTBR                                            
01126           DATASET    (W-TEXT-ID)                                  
01127           RIDFLD     (W-TEXT-KEY)                                 
01128           GTEQ                                                    
01129      END-EXEC.                                                    
01130                                                                   
01131      MOVE 'Y'                    TO W-TEXT-BROWSE-STARTED.        
01132                                                                   
01133  2110-READ-NEXT.                                                  
01134                                                                   
01135      IF  W-TB-NDX GREATER THAN W-MAX-LINES                        
01136          MOVE ER-0051            TO W-CURRENT-ERROR               
01137          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT                
01138          GO TO 2120-ENDBR.                                        
01139                                                                   
01140      EXEC CICS READNEXT                                           
01141           DATASET    (W-TEXT-ID)                                  
01142           SET        (ADDRESS OF TEXT-FILES)                      
01143           RIDFLD     (W-TEXT-KEY)                                 
01144      END-EXEC.                                                    
01145                                                                   
01146 *    SERVICE RELOAD TEXT-FILES.                                   
01147                                                                   
01148      IF  W-TEXT-PARTIAL-KEY NOT EQUAL W-TEXT-SAVE-KEY             
01149          GO TO 2120-ENDBR.                                        
01150                                                                   
01151      IF  TX-LINE-SQUEEZE-CONTROL EQUAL 'Z'                        
01152          PERFORM 2800-PROCESS-Z-CONTROLS THRU 2800-EXIT           
01153                                                                   
01154          IF  PI-FATAL-ERROR                                       
01155              GO TO 2999-EXIT                                      
01156                                                                   
01157          ELSE                                                     
01158              GO TO 2110-READ-NEXT.                                
01159                                                                   

031307      IF (PI-COMPANY-ID = 'CID')
031307         AND (PI-CARRIER NOT = '8')
031307         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
031307            B1 > +20
031307            IF TX-TEXT-LINE (B1:5) = '@03.1'
031307               GO TO 2110-READ-NEXT
031307            END-IF
031307         END-PERFORM
031307      END-IF
090108      IF PI-AUTO-LETTER-DATE > SPACES
090108         PERFORM VARYING B1 FROM +1 BY +1 UNTIL
090108           B1 > 61
090108           IF TX-TEXT-LINE (B1:10) = '@@AUTOPYDT' or '@@autopydt'
090108               MOVE PI-AUTO-LETTER-DATE TO TX-TEXT-LINE(B1:10)
090108           END-IF
090108         END-PERFORM
090108      END-IF.

01160      MOVE TX-TEXT-LINE           TO W-REC-TEXT (W-TB-NDX).        
01161      MOVE TX-PROCESS-CONTROL     TO W-REC-PC (W-TB-NDX)           
01162                                     W-NDX-WORK.                   
01163      SET W-TB-NDX UP BY 1.                                        
01164                                                                   
01165      IF  W-NDX-WORK = 99                                          
01166          MOVE W-TOP-FORM         TO W-REC-TEXT (W-TB-NDX)         
01167          SET W-TB-NDX UP BY 1                                     
01168          GO TO 2110-READ-NEXT                                     
01169                                                                   
01170      ELSE                                                         
01171          SET W-TB-NDX UP BY W-NDX-WORK                            
01172          GO TO 2110-READ-NEXT.                                    
01173                                                                   
01174  2120-ENDBR.                                                      
01175                                                                   
01176      IF  W-TEXT-BROWSE-STARTED = 'Y'                              
01177          MOVE 'N'                TO W-TEXT-BROWSE-STARTED         
01178                                                                   
01179          EXEC CICS ENDBR                                          
01180               DATASET (W-TEXT-ID)                                 
01181          END-EXEC.                                                
01182                                                                   
01183      IF  W-TB-NDX = 8                                             
01184          MOVE ER-0006            TO W-CURRENT-ERROR               
01185          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT                
01186          GO TO 2999-EXIT.                                         
01187                                                                   
01188      SET W-TB-NDX DOWN BY 1.                                      
01189      SET W-TOTAL-LINES           TO W-TB-NDX.                     
01190      MOVE 1                      TO W-CURRENT-LINE.               
01191                                                                   
01192      IF  PI-COMPANY-ID EQUAL 'AUK'                                
01193          MOVE 'Y'                TO W-REVERSE-DATE-SW.            
01194                                                                   
01195      PERFORM 7000-RESOLVE-VARIABLES THRU 7399-EXIT.               
01196                                                                   
01197      IF  PI-COMPANY-ID EQUAL 'AUK'                                
01198          MOVE SPACES             TO W-REVERSE-DATE-SW.            
01199                                                                   
01200      PERFORM 7800-VARIABLE-SEARCH THRU 7899-EXIT                  
01201              VARYING                                              
01202          W-TB-NDX FROM 7 BY 1                                     
01203              UNTIL                                                
01204          W-TB-NDX > W-TOTAL-LINES.                                
01205      GO TO 2999-EXIT.                                             
01206                                  EJECT                            
01207  2800-PROCESS-Z-CONTROLS.                                         
01208                                                                   
01209      MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA.             
01210                                                                   
01211      IF  PI-RESEND-DATE EQUAL LOW-VALUES                          
01212                                                                   
01213          IF  W-DAYS-TO-RESEND-1 NUMERIC                           
01214                  AND                                              
01215              W-DAYS-TO-RESEND-1 GREATER THAN ZEROS                
01216              MOVE '6'            TO DC-OPTION-CODE                
01217              MOVE W-SAVE-BIN-DATE                                 
01218                                  TO DC-BIN-DATE-1                 
01219              MOVE ZEROS          TO DC-ELAPSED-MONTHS             
01220              MOVE W-DAYS-TO-RESEND-1                              
01221                                  TO DC-ELAPSED-DAYS               
01222              PERFORM 9700-DATE-LINK THRU 9700-EXIT                
01223                                                                   
01224              IF  NO-CONVERSION-ERROR                              
01225                  MOVE DC-BIN-DATE-2                               
01226                                  TO PI-RESEND-DATE                
01227                                                                   
01228              ELSE                                                 
01229                  MOVE ER-3770    TO W-CURRENT-ERROR               
01230                  PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.       
01231                                                                   
01232      IF  PI-FOLLOW-UP-DATE EQUAL LOW-VALUES                       
01233                                                                   
01234          IF  W-DAYS-TO-FOLLOW-UP NUMERIC                          
01235                  AND                                              
01236              W-DAYS-TO-FOLLOW-UP GREATER THAN ZEROS               
01237              MOVE '6'            TO DC-OPTION-CODE                
01238              MOVE W-SAVE-BIN-DATE                                 
01239                                  TO DC-BIN-DATE-1                 
01240              MOVE ZEROS          TO DC-ELAPSED-MONTHS             
01241              MOVE W-DAYS-TO-FOLLOW-UP                             
01242                                  TO DC-ELAPSED-DAYS               
01243              PERFORM 9700-DATE-LINK THRU 9700-EXIT                
01244                                                                   
01245              IF  NO-CONVERSION-ERROR                              
01246                  MOVE DC-BIN-DATE-2                               
01247                                  TO PI-FOLLOW-UP-DATE             
01248                                                                   
01249              ELSE                                                 
01250                  MOVE ER-3771    TO W-CURRENT-ERROR               
01251                  PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.       
01252                                                                   
01253      IF  PI-NUMBER-COPIES NOT NUMERIC                             
01254              OR                                                   
01255          PI-NUMBER-COPIES EQUAL ZEROS                             
01256                                                                   
01257          IF  W-NUMBER-OF-COPIES NUMERIC                           
01258                  AND                                              
01259              W-NUMBER-OF-COPIES NOT EQUAL ZEROS                   
01260              MOVE W-NUMBER-OF-COPIES                              
01261                                  TO PI-NUMBER-COPIES              
01262                                                                   
01263          ELSE                                                     
01264              MOVE +1             TO PI-NUMBER-COPIES.             
01265                                                                   
040110     IF W-FORM-TO-RESEND > SPACES
040110         MOVE W-FORM-TO-RESEND          TO PI-RESEND-FORM-NUMBER
040110     ELSE
040110         MOVE LOW-VALUES                TO PI-RESEND-FORM-NUMBER
040110     END-IF.
040110
040110     MOVE W-PROMPT-LETTER               TO PI-PROMPT-LETTER.
040110     MOVE W-ENCLOSURE-CD                TO PI-ENCLOSURE-CD.
040110     MOVE W-AUTO-CLOSE-IND              TO PI-AUTO-CLOSE-IND.
040110     MOVE W-LETTER-TO-BENE              TO PI-LETTER-TO-BENE.
040110
01266  2800-EXIT.                                                       
01267      EXIT.                                                        
01268                                                                   
01269  2900-TEXT-NOT-OPEN.                                              
01270                                                                   
01271      MOVE ER-0013                TO W-CURRENT-ERROR.              
01272      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01273                                                                   
01274  2999-EXIT.                                                       
01275      EXIT.                                                        
01276                                  EJECT                            
01277  3000-READ-ADDR.                                                  
01278                                                                   
01279      EXEC CICS HANDLE CONDITION                                   
01280           NOTOPEN    (3920-CLM-NOT-OPEN)                          
01281           NOTFND     (3900-CLAIM-NOT-FOUND)                       
01282      END-EXEC.                                                    
01283                                                                   
01284      EXEC CICS READ                                               
01285           DATASET    (W-CLM-ID)                                   
01286           SET        (ADDRESS OF CLAIM-MASTER)                    
01287           RIDFLD     (W-CLM-KEY)                                  
01288      END-EXEC.                                                    
01289                                                                   
01290 *    SERVICE RELOAD CLAIM-MASTER.                                 
01291                                                                   
01292      IF  PI-ADDR-TYPE NOT GREATER THAN SPACES                     
01293          GO TO 3999-EXIT.                                         
01294                                                                   
01295      PERFORM 6600-SET-ADDR-SEQ THRU 6699-EXIT.                    
01296                                                                   
01297      MOVE PI-ADDR-TYPE           TO W-ADDR-TYPE-CD.               
01298                                                                   
01299      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     
01300          GO TO 3200-READ-PRODUCER.                                
01301                                                                   
01302  3100-READ-ACCT.                                                  
01303                                                                   
01304      MOVE CL-CERT-GROUPING       TO W-ACCT-GROUPING.              
01305      MOVE CL-CERT-STATE          TO W-ACCT-STATE.                 
01306      MOVE CL-CERT-ACCOUNT        TO W-ACCT-ACCOUNT.               
01307      MOVE CL-CERT-EFF-DT         TO W-ACCT-EXP-DATE.              
01308                                                                   
01309      EXEC CICS HANDLE CONDITION                                   
01310           NOTOPEN    (3930-ACCT-NOT-OPEN)                         
01311           NOTFND     (3910-ACCT-NOT-FOUND)                        
01312           END-EXEC.                                               
01313                                                                   
01314      PERFORM 8000-STARTBR-ERACCT THRU 8000-EXIT.                  
01315                                                                   
01316      MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.              
01317                                                                   
01318  3110-READNEXT.                                                   
01319                                                                   
01320      PERFORM 8010-READNEXT-ERACCT THRU 8010-EXIT.                 
01321                                                                   
01322      IF  W-ACCT-PARTIAL-KEY NOT = W-ACCT-SAVE-KEY                 
01323                                                                   
01324          IF  W-SAVE-ACCT-RECORD EQUAL SPACES                      
01325              GO TO 3910-ACCT-NOT-FOUND                            
01326                                                                   
01327          ELSE                                                     
01328              MOVE 'Y'            TO W-ACCT-READ-SW                
01329              MOVE AM-CONTROL-PRIMARY                              
01330                                  TO W-ACCT-KEY                    
01331              MOVE W-SAVE-ACCT-RECORD                              
01332                                  TO ACCOUNT-MASTER                
01333              GO TO 3120-CONTINUE-BUILD-ADDR.                      
01334                                                                   
01335      IF  AM-EXPIRATION-DT EQUAL HIGH-VALUES                       
01336          NEXT SENTENCE                                            
01337                                                                   
01338      ELSE                                                         
01339          MOVE ACCOUNT-MASTER     TO W-SAVE-ACCT-RECORD            
01340          GO TO 3110-READNEXT.                                     
01341                                                                   
01342      MOVE AM-CONTROL-PRIMARY     TO W-ACCT-KEY.                   
01343      MOVE 'Y'                    TO W-ACCT-READ-SW.               
01344                                                                   
01345                                                                   
01346  3120-CONTINUE-BUILD-ADDR.                                        
01347                                                                   
01348      MOVE SPACES                 TO W-SAVE-ACCT-RECORD.           
01349                                                                   
01350      IF  W-ADDR-TYPE-CD EQUAL 'A0'                                
01351          MOVE ZEROS              TO W-PI-ADDR-SEQ                 
01352                                                                   
01353      ELSE                                                         
01354          GO TO 3350-CHECK-BENE-ADDR.                              
01355                                                                   
01356      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
01357      MOVE AM-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).        
01358      MOVE AM-PERSON TO W-REC-TEXT (02) W-LABEL-LINES (02).        
01359      MOVE AM-ADDRS  TO W-REC-TEXT (03) W-LABEL-LINES (03).        
01360      MOVE AM-CITY   TO W-REC-TEXT (04) W-LABEL-LINES (04).        
01361      MOVE SPACES    TO W-REC-TEXT (05) W-LABEL-LINES (05).        
01362                                                                   
01363      MOVE SPACES                 TO W-ZIP-CODE.                   
01364                                                                   
01365      IF  AM-CANADIAN-POST-CODE                                    
01366          MOVE AM-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
01367          MOVE AM-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
01368                                                                   
01369      ELSE                                                         
01370          MOVE AM-ZIP-PRIME       TO W-AM-ZIP-CODE                 
01371                                                                   
01372          IF  AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
01373              MOVE '-'            TO W-AM-ZIP-DASH                 
01374              MOVE AM-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
01375                                                                   
01376      MOVE W-ZIP-CODE             TO W-REC-TEXT (6)                
01377                                     W-LABEL-LINES (06).           
01378                                                                   
01379      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
01380                                                                   
01381      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.      
01382      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.      
01383      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.      
01384      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.      
01385      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.      
01386      MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (06) SS05-6D.      
01387                                                                   
01388      GO TO 3999-EXIT.                                             
01389                                                                   
01390      EJECT                                                        
01391  3200-READ-PRODUCER.                                              
01392                                                                   
01393      MOVE CL-CERT-GROUPING       TO W-PROD-GROUPING.              
01394      MOVE CL-CERT-STATE          TO W-PROD-STATE.                 
01395      MOVE CL-CERT-ACCOUNT        TO W-PROD-PRODUCER.              
01396      MOVE CL-CERT-EFF-DT         TO W-PROD-EXP-DATE.              
01397                                                                   
01398      EXEC CICS HANDLE CONDITION                                   
01399           NOTOPEN    (3960-PROD-NOT-OPEN)                         
01400           NOTFND     (3950-PROD-NOT-FOUND)                        
01401           END-EXEC.                                               
01402                                                                   
01403      PERFORM 8050-STARTBR-EMPROD THRU 8050-EXIT.                  
01404                                                                   
01405      MOVE W-PROD-PARTIAL-KEY     TO W-PROD-SAVE-KEY.              
01406                                                                   
01407  3210-READNEXT.                                                   
01408                                                                   
01409      PERFORM 8060-READNEXT-EMPROD THRU 8060-EXIT.                 
01410                                                                   
01411      IF  W-PROD-PARTIAL-KEY NOT = W-PROD-SAVE-KEY                 
01412                                                                   
01413          IF  W-SAVE-PROD-RECORD EQUAL SPACES                      
01414              GO TO 3950-PROD-NOT-FOUND                            
01415                                                                   
01416          ELSE                                                     
01417              MOVE 'Y'            TO W-PROD-READ-SW                
01418              MOVE PD-CONTROL-PRIMARY                              
01419                                  TO W-PROD-KEY                    
01420              MOVE W-SAVE-PROD-RECORD                              
01421                                  TO PRODUCER-MASTER               
01422              GO TO 3220-CONTINUE-BUILD-ADDR.                      
01423                                                                   
01424      IF  PD-EXPIRE-DATE EQUAL HIGH-VALUES                         
01425          NEXT SENTENCE                                            
01426                                                                   
01427      ELSE                                                         
01428          MOVE PRODUCER-MASTER    TO W-SAVE-PROD-RECORD            
01429          GO TO 3210-READNEXT.                                     
01430                                                                   
01431      MOVE PD-CONTROL-PRIMARY     TO W-PROD-KEY.                   
01432      MOVE 'Y'                    TO W-PROD-READ-SW.               
01433                                                                   
01434  3220-CONTINUE-BUILD-ADDR.                                        
01435                                                                   
01436      MOVE SPACES                 TO W-SAVE-PROD-RECORD.           
01437                                                                   
01438      IF  W-ADDR-TYPE-CD EQUAL 'A0'                                
01439          MOVE ZEROS              TO W-PI-ADDR-SEQ                 
01440                                                                   
01441      ELSE                                                         
01442          GO TO 3350-CHECK-BENE-ADDR.                              
01443                                                                   
01444      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
01445      MOVE PD-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).        
01446      MOVE PD-PERSON TO W-REC-TEXT (02) W-LABEL-LINES (02).        
01447      MOVE PD-ADDRS  TO W-REC-TEXT (03) W-LABEL-LINES (03).        
01448      MOVE PD-CITY   TO W-REC-TEXT (04) W-LABEL-LINES (04).        
01449      MOVE SPACES    TO W-REC-TEXT (05) W-LABEL-LINES (05).        
01450                                                                   
01451      MOVE SPACES                 TO W-ZIP-CODE.                   
01452                                                                   
01453      MOVE PD-ZIP-PRIME           TO W-AM-ZIP-CODE.                
01454                                                                   
01455      IF  PD-ZIP-PLUS4 NOT = SPACES AND  ZEROS                     
01456          MOVE '-'                TO W-AM-ZIP-DASH                 
01457          MOVE PD-ZIP-PLUS4       TO W-AM-ZIP-PLUS4.               
01458                                                                   
01459      MOVE W-ZIP-CODE             TO W-REC-TEXT (6)                
01460                                     W-LABEL-LINES (06).           
01461                                                                   
01462      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
01463                                                                   
01464      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.      
01465      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.      
01466      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.      
01467      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.      
01468      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.      
01469      MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (06) SS05-6D.      
01470                                                                   
01471      GO TO 3999-EXIT.                                             
01472                                                                   
01473      EJECT                                                        
01474  3350-CHECK-BENE-ADDR.                                            
01475                                                                   
01476      IF  W-ADDR-TYPE-CD NOT = 'B0'                                
01477          GO TO 3400-CONTINUE-BUILD-ADDR.                          
01478                                                                   
01479      IF  CL-BENEFICIARY = SPACES                                  
01480          GO TO 3450-ACTV-NOT-FOUND.                               
01481                                                                   
01482      EXEC CICS HANDLE CONDITION                                   
01483           NOTFND (3450-ACTV-NOT-FOUND)                            
01484      END-EXEC.                                                    
01485                                                                   
01486      MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.               
01487      MOVE 'B'                    TO W-BENE-REC-TYPE.              
01488      MOVE CL-BENEFICIARY         TO W-BENE-NUMBER.                
01489                                                                   
01490      EXEC CICS READ                                               
01491           DATASET    (W-BENE-ID)                                  
01492           SET        (ADDRESS OF BENEFICIARY-MASTER)              
01493           RIDFLD     (W-BENE-KEY)                                 
01494           END-EXEC.                                               
01495                                                                   
01496 *    SERVICE RELOAD BENEFICIARY-MASTER.                           
01497                                                                   
01498      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
01499      MOVE BE-MAIL-TO-NAME        TO W-REC-TEXT (1)                
01500                                     W-LABEL-LINES (01).           
01501      MOVE BE-ADDRESS-LINE-1      TO W-REC-TEXT (02)               
01502                                     W-LABEL-LINES (02).           
01503      MOVE BE-ADDRESS-LINE-2      TO W-REC-TEXT (03)               
01504                                     W-LABEL-LINES (03).           
01505      MOVE BE-CITY-STATE          TO W-REC-TEXT (04)               
01506                                     W-LABEL-LINES (04).           
01507                                                                   
01508      MOVE SPACES                 TO W-ZIP-CODE.                   
01509                                                                   
01510      IF  BE-CANADIAN-POST-CODE                                    
01511          MOVE BE-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
01512          MOVE BE-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
01513                                                                   
01514      ELSE                                                         
01515          MOVE BE-ZIP-PRIME       TO W-AM-ZIP-CODE                 
01516                                                                   
01517          IF  BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
01518              MOVE '-'            TO W-AM-ZIP-DASH                 
01519              MOVE BE-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
01520                                                                   
01521      MOVE W-ZIP-CODE             TO W-REC-TEXT (05)               
01522                                     W-LABEL-LINES (05).           
01523                                                                   
01524      MOVE SPACES                 TO W-REC-TEXT (6).               
01525                                                                   
01526      GO TO 3420-SET-ADDR.                                         
01527                                                                   
01528  3400-CONTINUE-BUILD-ADDR.                                        
01529                                                                   
01530      IF  W-ACTV-SEQ = ZEROS                                       
01531          GO TO 3450-ACTV-NOT-FOUND.                               
01532                                                                   
01533      MOVE W-ACTV-SEQ             TO W-PI-ADDR-SEQ.                
01534                                                                   
01535      EXEC CICS HANDLE CONDITION                                   
01536           NOTOPEN    (3940-ACTV-NOT-OPEN)                         
01537           NOTFND     (3450-ACTV-NOT-FOUND)                        
01538      END-EXEC.                                                    
01539                                                                   
01540      EXEC CICS READ                                               
01541           DATASET    (W-ACTV-ID)                                  
01542           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
01543           RIDFLD     (W-ACTV-KEY)                                 
01544      END-EXEC.                                                    
01545                                                                   
01546 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
01547                                                                   
01548      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
01549      MOVE AT-MAIL-TO-NAME   TO W-REC-TEXT (01) W-LABEL-LINES (01).
01550      MOVE AT-ADDRESS-LINE-1 TO W-REC-TEXT (02) W-LABEL-LINES (02).
01551      MOVE AT-ADDRESS-LINE-2 TO W-REC-TEXT (03) W-LABEL-LINES (03).
01552      MOVE AT-CITY-STATE     TO W-REC-TEXT (04) W-LABEL-LINES (04).
01553                                                                   
01554      MOVE SPACES                 TO W-ZIP-CODE.                   
01555                                                                   
01556      IF  AT-CANADIAN-POST-CODE                                    
01557          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
01558          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
01559                                                                   
01560      ELSE                                                         
01561          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
01562          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
01563              MOVE '-'            TO W-AM-ZIP-DASH                 
01564              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
01565                                                                   
01566      MOVE W-ZIP-CODE             TO W-REC-TEXT (05)               
01567                                     W-LABEL-LINES (05).           
01568                                                                   
01569      MOVE SPACES                 TO W-REC-TEXT (6).               
01570                                                                   
01571  3420-SET-ADDR.                                                   
01572                                                                   
01573      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
01574                                                                   
01575      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (01) SS05-1D.      
01576      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS05-2D.      
01577      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS05-3D.      
01578      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS05-4D.      
01579      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS05-5D.      
01580      MOVE SPACES                 TO W-REC-TEXT (06) SS05-6D.      
01581      GO TO 3999-EXIT.                                             
01582                                                                   
01583  3450-ACTV-NOT-FOUND.                                             
01584                                                                   
01585      IF  W-ACTV-SEQ EQUAL +29                                     
01586          NEXT SENTENCE                                            
01587                                                                   
01588      ELSE                                                         
01589          GO TO 3480-CONTINUE-ACTV-ERROR.                          
01590                                                                   
01591      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     
01592          GO TO 3999-EXIT.                                         
01593                                                                   
01594      IF  AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                     
01595          MOVE ZEROS              TO AM-3RD-PARTY-NOTIF-LEVEL      
01596          GO TO 3480-CONTINUE-ACTV-ERROR.                          
01597                                                                   
01598      IF  AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS  
01599          GO TO 3480-CONTINUE-ACTV-ERROR.                          
01600                                                                   
01601      MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD.            
01602      MOVE AM-CARRIER             TO W-COMP-CARRIER.               
01603      MOVE AM-GROUPING            TO W-COMP-GROUPING.              
01604      MOVE 'A'                    TO W-COMP-TYPE.                  
01605      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                       
01606                                  TO W-COMP-RESP-NO.               
01607                                                                   
01608      IF  AM-3RD-PARTY-NOTIF-LEVEL EQUAL AM-REMIT-TO               
01609          IF AM-COM-TYP (AM-REMIT-TO) EQUAL 'O' OR 'P' OR          
052814                                           'G' OR 'B' or 'S'      
01611              MOVE 'G'            TO W-COMP-TYPE                   
01612              MOVE LOW-VALUES     TO W-COMP-ACCOUNT                
01613          ELSE                                                     
01614              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)               
01615                                  TO W-COMP-ACCOUNT                
01616      ELSE                                                         
01617          MOVE 'G'                TO W-COMP-TYPE                   
01618          MOVE LOW-VALUES         TO W-COMP-ACCOUNT.               
01619                                                                   
01620      IF  PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP                     
01621          MOVE ZEROS              TO W-COMP-CARRIER.               
01622                                                                   
01623      IF  PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP                    
01624          MOVE ZEROS              TO W-COMP-GROUPING.              
01625                                                                   
01626      EXEC CICS HANDLE CONDITION                                   
01627           NOTFND    (3480-CONTINUE-ACTV-ERROR)                    
01628           NOTOPEN   (3479-COMP-NOT-OPEN)                          
01629      END-EXEC.                                                    
01630                                                                   
01631      EXEC CICS  READ                                              
01632           SET      (ADDRESS OF COMPENSATION-MASTER)               
01633           DATASET  ('ERCOMP')                                     
01634           RIDFLD   (W-COMP-KEY)                                   
01635      END-EXEC.                                                    
01636                                                                   
01637 *    SERVICE RELOAD COMPENSATION-MASTER.                          
01638                                                                   
01639      MOVE 'Y'                    TO W-COMP-READ-SW.               
01640      MOVE SPACES                 TO W-LABEL-HOLD-AREA             
01641      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)            
01642                                                                   
01643      IF  CO-ACCT-NAME EQUAL SPACES                                
01644          MOVE CO-MAIL-NAME       TO W-LABEL-LINES (01).           
01645                                                                   
01646      MOVE CO-ADDR-1              TO W-LABEL-LINES (02).           
01647      MOVE CO-ADDR-2              TO W-LABEL-LINES (03).           
01648      MOVE CO-ADDR-3              TO W-LABEL-LINES (04).           
01649                                                                   
01650      MOVE SPACES                 TO W-ZIP-CODE.                   
01651                                                                   
01652      IF  CO-CANADIAN-POST-CODE                                    
01653          MOVE CO-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
01654          MOVE CO-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
01655                                                                   
01656      ELSE                                                         
01657          MOVE CO-ZIP-PRIME       TO W-AM-ZIP-CODE                 
01658                                                                   
01659          IF  CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
01660              MOVE '-'            TO W-AM-ZIP-DASH                 
01661              MOVE CO-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
01662                                                                   
01663      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
01664                                                                   
01665      MOVE ZEROS                  TO W-PHONE-IN.                   
01666      MOVE CO-AREA-CODE           TO W-PO-AREA.                    
01667      MOVE CO-PREFIX              TO W-PO-PFX.                     
01668      MOVE CO-PHONE               TO W-PO-SFX.                     
01669      MOVE W-PHONE-OUT            TO SS53-6D.                      
01670                                                                   
01671      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
01672                                                                   
01673      MOVE W-LABEL-LINES (01)     TO W-REC-TEXT (1) SS52D.         
01674      MOVE W-LABEL-LINES (02)     TO W-REC-TEXT (02) SS53-1D.      
01675      MOVE W-LABEL-LINES (03)     TO W-REC-TEXT (03) SS53-2D.      
01676      MOVE W-LABEL-LINES (04)     TO W-REC-TEXT (04) SS53-3D.      
01677      MOVE W-LABEL-LINES (05)     TO W-REC-TEXT (05) SS53-4D.      
01678      MOVE W-LABEL-LINES (06)     TO W-REC-TEXT (6) SS53-5D.       
01679                                                                   
01680      GO TO 3999-EXIT.                                             
01681                                                                   
01682  3479-COMP-NOT-OPEN.                                              
01683                                                                   
01684      MOVE ER-2055                TO W-CURRENT-ERROR.              
01685      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01686      GO TO 3999-EXIT.                                             
01687                                  EJECT                            
01688  3480-CONTINUE-ACTV-ERROR.                                        
01689                                                                   
01690      MOVE ER-0178                TO W-CURRENT-ERROR.              
01691      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01692 *    MOVE SPACES                 TO PI-ADDR-TYPE.                 
01693      MOVE SPACES                 TO W-REC-TEXT (01)               
01694                                     W-REC-TEXT (02)               
01695                                     W-REC-TEXT (03)               
01696                                     W-REC-TEXT (04)               
01697                                     W-REC-TEXT (05)               
01698                                     W-REC-TEXT (06).              
01699      GO TO 3999-EXIT.                                             
01700                                                                   
01701  3900-CLAIM-NOT-FOUND.                                            
01702                                                                   
01703      MOVE ER-0186                TO W-CURRENT-ERROR.              
01704      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01705      GO TO 3999-EXIT.                                             
01706                                                                   
01707  3910-ACCT-NOT-FOUND.                                             
01708                                                                   
01709      MOVE ER-0179                TO W-CURRENT-ERROR.              
01710      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01711      GO TO 3999-EXIT.                                             
01712                                                                   
01713  3920-CLM-NOT-OPEN.                                               
01714                                                                   
01715      MOVE ER-0154                TO W-CURRENT-ERROR.              
01716      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01717      GO TO 3999-EXIT.                                             
01718                                                                   
01719  3930-ACCT-NOT-OPEN.                                              
01720                                                                   
01721      MOVE ER-0168                TO W-CURRENT-ERROR.              
01722      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01723      GO TO 3999-EXIT.                                             
01724                                                                   
01725  3940-ACTV-NOT-OPEN.                                              
01726                                                                   
01727      MOVE ER-0172                TO W-CURRENT-ERROR.              
01728      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01729      GO TO 3999-EXIT.                                             
01730                                                                   
01731  3950-PROD-NOT-FOUND.                                             
01732      MOVE ER-9887                TO W-CURRENT-ERROR.              
01733      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01734      GO TO 3999-EXIT.                                             
01735                                                                   
01736  3960-PROD-NOT-OPEN.                                              
01737      MOVE ER-9886                TO W-CURRENT-ERROR.              
01738      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01739      GO TO 3999-EXIT.                                             
01740                                                                   
01741  3999-EXIT.                                                       
01742       EXIT.                                                       
01743                                  EJECT                            
01744  6000-ARCHIVE-LETTER.                                             
01745 ***************************************************************   
01746 *    THIS ROUTINE WILL BE USED WHEN THE LETTER HAS BEEN       *   
01747 *    COMPLETED AND IS TO BE PUT AS PERMANENT RECORDS ONTO     *   
01748 *    THE ARCHIVE FILE.                                        *   
01749 *    THE FOLLOWING FUNCTIONS WILL BE PERFORMED                *   
01750 *        1. MAKE SURE THERE ARE NO UNRESOLVED SYMBOLS         *   
01751 *        2. GET THE ARCHIVE NUMBER FROM THE CONTROL FILE.     *   
01752 *        3. WRITE THE NEW ARCHIVE RECORDS FROM TS-TABLE.      *   
01753 *        4. BUILD A CORRESPONDENCE TRAILER.                   *   
01754 *        5. BUILD OR UPDATE THE ACTIVITY QUE FILE WITH THE    *   
01755 *           ARCHIVE NUMBER IF IT IS TO BE PRINTED LATER.      *   
01756 *        6. RESET ALL CONTROL FIELDS AND RETURN THE           *   
01757 *           ARCHIVE NUMBER USED TO FILE THE RECORDS.          *   
01758 ***************************************************************   
01759                                                                   
01760      MOVE +0                     TO TALLY.                        
01761      INSPECT W-RECORD-TABLE TALLYING TALLY                        
01762                                  FOR CHARACTERS BEFORE '@'.       
01763                                                                   
01764      IF  TALLY LESS THAN +21900                                   
01765          COMPUTE W-CURRENT-LINE = TALLY / 73                      
01766          MOVE ER-0191            TO W-CURRENT-ERROR               
01767          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.               
01768                                                                   
01769      MOVE '1'                    TO W-CNTL-RECORD-TYPE.           
01770      MOVE ZEROS                  TO W-CNTL-SEQ.                   
01771      MOVE SPACES                 TO W-CNTL-GENL.                  
01772                                                                   
01773      EXEC CICS HANDLE CONDITION                                   
01774           NOTFND  (6100-NOT-FOUND)                                
01775           NOTOPEN (6110-CNTL-NOT-OPEN)                            
01776      END-EXEC.                                                    
01777                                                                   
01778      EXEC CICS READ                                               
01779           DATASET  (W-CNTL-ID)                                    
01780           SET      (ADDRESS OF CONTROL-FILE)                      
01781           RIDFLD   (W-CNTL-KEY)                                   
01782           UPDATE                                                  
01783      END-EXEC.                                                    
01784                                                                   
01785 *    SERVICE RELOAD CONTROL-FILE.                                 
01786      ADD 1                       TO CF-CO-ARCHIVE-COUNTER.        
01787      MOVE CF-CO-ARCHIVE-COUNTER  TO W-ARCH-NUMBER                 
01788                                     PI-ARCHIVE-NUMBER.            
01789      MOVE CF-PRINT-ADDRESS-LABELS                                 
01790                                  TO W-LABELS-SW.                  
01791                                                                   
01792      EXEC CICS REWRITE                                            
01793           FROM      (CONTROL-FILE)                                
01794           DATASET   (W-CNTL-ID)                                   
01795      END-EXEC                                                     
01796                                                                   
01797      PERFORM 6500-BUILD-CORRESPOND THRU 6599-EXIT.                
01798                                                                   
01799      EXEC CICS HANDLE CONDITION                                   
01800           NOTOPEN (6120-ARCH-NOT-OPEN)                            
01801      END-EXEC.                                                    
01802                                                                   
01803      IF  PI-ARCH-POINTER GREATER THAN ZEROS                       
01804          MOVE PI-ARCH-POINTER    TO LCP-WS-ADDR-COMP              
01805          SET ADDRESS OF LETTER-ARCHIVE TO LCP-WS-ADDR-PNTR        
01806 *        SERVICE RELOAD LETTER-ARCHIVE                            
01807                                                                   
01808      ELSE                                                         
01809          EXEC CICS GETMAIN                                        
01810               SET      (ADDRESS OF LETTER-ARCHIVE)                
01811               LENGTH   (W-ARCH-LENGTH)                            
01812          END-EXEC                                                 
01813          SET LCP-WS-ADDR-PNTR TO ADDRESS OF LETTER-ARCHIVE        
01814                                                                   
01815 *        SERVICE RELOAD LETTER-ARCHIVE                            
01816          MOVE LCP-WS-ADDR-COMP TO PI-ARCH-POINTER.                
01817                                                                   
01818      MOVE SPACES                 TO LETTER-ARCHIVE.               
01819      MOVE 'LA'                   TO LA-RECORD-ID.                 
01820      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO                 
01821                                     LA-ARCHIVE-NO-A1.             
01822      MOVE '1'                    TO LA-RECORD-TYPE                
01823                                     LA-RECORD-TYPE-A1.            
01824      MOVE ZEROS                  TO LA-LINE-SEQ-NO                
01825                                     LA-LINE-SEQ-NO-A1.            
01826      MOVE W-ARCH-CO              TO LA-COMPANY-CD                 
01827                                     LA-COMPANY-CD-A1.             
01828      MOVE W-CLM-CARRIER          TO LA-CARRIER.                   
01829      MOVE PI-CLAIM-NO            TO LA-CLAIM-NO.                  
01830      MOVE PI-CERT-NO             TO LA-CERT-NO.                   
01831                                                                   
01832      IF  PI-NUMBER-COPIES NUMERIC                                 
01833              AND                                                  
01834          PI-NUMBER-COPIES GREATER THAN ZEROS                      
01835          MOVE PI-NUMBER-COPIES                                    
01836                                  TO LA-NO-OF-COPIES               
01837                                                                   
01838      ELSE                                                         
01839          MOVE  1                 TO LA-NO-OF-COPIES.              
01840                                                                   
01841      MOVE PI-RESEND-DATE         TO LA-RESEND-DATE.               
01842                                                                   
01843      MOVE PI-PROCESSOR-ID        TO LA-PROCESSOR-CD.              
01844      MOVE W-CURRENT-SAVE         TO LA-CREATION-DT.               
01845      MOVE LOW-VALUES             TO LA-INITIAL-PRINT-DATE         
01846                                     LA-RESEND-PRINT-DATE.         
01847      MOVE W-CORR-TRLR-SEQ        TO LA-CORR-TRLR-SEQ.             
01848      PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.                   
033110
040110     IF PI-PROMPT-LETTER NOT EQUAL 'Y'
033110         PERFORM 6700-BUILD-NAPERSOFT THRU 6799-EXIT
040110     END-IF.
01849                                                                   
01850      IF  W-LABELS-SW EQUAL       TO 'N'                           
01851          NEXT SENTENCE                                            
01852                                                                   
01853      ELSE                                                         
01854          SET W-TB-NDX            TO 1                             
01855          MOVE ZEROS              TO W-SEQ-COUNTER                 
01856          PERFORM 6300-FORMAT-ADDRESS THRU 6300-EXIT               
01857                  VARYING                                          
01858              W-TB-NDX FROM 1 BY 1                                 
01859                  UNTIL                                            
01860              W-TB-NDX GREATER THAN 6.                             
01861                                                                   
01862      MOVE ZEROS                  TO W-SEQ-COUNTER.                
01863      PERFORM 6200-FORMAT-TEXT THRU 6200-EXIT                      
01864              VARYING                                              
01865          W-TB-NDX FROM 8 BY 1                                     
01866              UNTIL                                                
01867          W-TB-NDX GREATER THAN W-TOTAL-LINES.                     
01868                                                                   
01869  6000-EXIT.                                                       
01870      EXIT.                                                        
01871                                                                   
01872  6100-NOT-FOUND.                                                  
01873                                                                   
01874      MOVE ER-0281                TO W-CURRENT-ERROR.              
01875      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01876      GO TO 6000-EXIT.                                             
01877                                                                   
01878  6110-CNTL-NOT-OPEN.                                              
01879                                                                   
01880      MOVE ER-0042                TO W-CURRENT-ERROR.              
01881      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01882      GO TO 6000-EXIT.                                             
01883                                                                   
01884  6120-ARCH-NOT-OPEN.                                              
01885                                                                   
01886      MOVE ER-0332                TO W-CURRENT-ERROR.              
01887      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01888      GO TO 6000-EXIT.                                             
01889                                                                   
01890                                  EJECT                            
01891  6200-FORMAT-TEXT.                                                
01892                                                                   
01893      MOVE SPACES                 TO LETTER-ARCHIVE.               
01894      MOVE '3'                    TO LA-RECORD-TYPE                
01895                                     LA-RECORD-TYPE-A1.            
01896      MOVE 'LA'                   TO LA-RECORD-ID.                 
01897      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO                 
01898                                     LA-ARCHIVE-NO-A1.             
01899      MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO                
01900                                     LA-LINE-SEQ-NO-A1.            
01901      MOVE W-ARCH-CO              TO LA-COMPANY-CD                 
01902                                     LA-COMPANY-CD-A1.             
01903      MOVE W-REC-TEXT (W-TB-NDX)  TO LA-TEXT-LINE.                 
01904      SET W-TB-NDX1               TO W-TB-NDX.                     
01905      SET W-TB-NDX1 UP BY 1.                                       
01906      MOVE ZEROS                  TO W-NDX-WORK.                   
01907                                                                   
01908  6200-LOOP.                                                       
01909                                                                   
01910      IF  W-TB-NDX1 LESS THAN W-TOTAL-LINES                        
01911              AND                                                  
01912          W-REC-TEXT (W-TB-NDX1) = SPACES                          
01913          SET W-TB-NDX1 UP BY 1                                    
01914          ADD 1                   TO W-NDX-WORK                    
01915          GO TO 6200-LOOP.                                         
01916                                                                   
01917      IF  W-REC-TEXT (W-TB-NDX1) = W-TOP-FORM                      
01918          MOVE '99'               TO LA-SKIP-CONTROL               
01919          SET W-TB-NDX1 UP BY 1                                    
01920                                                                   
01921      ELSE                                                         
01922          MOVE W-NDX-WORK         TO LA-SKIP-CONTROL.              
01923                                                                   
01924      SET W-TB-NDX                TO W-TB-NDX1.                    
01925      SET W-TB-NDX DOWN BY 1.                                      
01926      PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.                   
01927      ADD 1 TO W-SEQ-COUNTER.                                      
01928                                                                   
01929  6200-EXIT.                                                       
01930       EXIT.                                                       
01931                                  EJECT                            
01932  6300-FORMAT-ADDRESS.                                             
01933                                                                   
01934      MOVE SPACES                 TO LETTER-ARCHIVE.               
01935      MOVE '2'                    TO LA-RECORD-TYPE                
01936                                     LA-RECORD-TYPE-A1.            
01937      MOVE 'LA'                   TO LA-RECORD-ID.                 
01938      MOVE W-ARCH-NUMBER          TO LA-ARCHIVE-NO                 
01939                                     LA-ARCHIVE-NO-A1.             
01940      MOVE W-SEQ-COUNTER          TO LA-LINE-SEQ-NO                
01941                                     LA-LINE-SEQ-NO-A1.            
01942      MOVE W-ARCH-CO              TO LA-COMPANY-CD                 
01943                                     LA-COMPANY-CD-A1.             
01944      MOVE W-REC-TEXT (W-TB-NDX)  TO LA-ADDRESS-LINE.              
01945      PERFORM 6400-WRITE-ARCHIVE THRU 6400-EXIT.                   
01946      ADD 1 TO W-SEQ-COUNTER.                                      
01947                                                                   
01948  6300-EXIT.                                                       
01949       EXIT.                                                       
01950                                  EJECT                            
01951  6400-WRITE-ARCHIVE.                                              
01952                                                                   
01953      EXEC CICS HANDLE CONDITION                                   
01954          DUPREC    (6420-ARCH-DUPREC)                             
01955          NOTOPEN   (6410-ARCH-NOT-OPEN)                           
01956          NOSPACE   (6425-ARCH-NOSPACE)                            
01957      END-EXEC.                                                    
01958                                                                   
01959      EXEC CICS WRITE                                              
01960           DATASET   (W-ARCH-ID)                                   
01961           FROM      (LETTER-ARCHIVE)                              
01962           RIDFLD    (LA-CONTROL-PRIMARY)                          
01963      END-EXEC.                                                    
01964                                                                   
01965  6400-EXIT.                                                       
01966      EXIT.                                                        
01967                                                                   
01968  6410-ARCH-NOT-OPEN.                                              
01969                                                                   
01970      MOVE ER-0332                TO W-CURRENT-ERROR.              
01971      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01972      GO TO 6400-EXIT.                                             
01973                                                                   
01974  6420-ARCH-DUPREC.                                                
01975                                                                   
01976      MOVE ER-3766                TO W-CURRENT-ERROR.              
01977      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01978      GO TO 6400-EXIT.                                             
01979                                                                   
01980  6425-ARCH-NOSPACE.                                               
01981                                                                   
01982      MOVE ER-3699                TO W-CURRENT-ERROR.              
01983      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
01984      GO TO 6400-EXIT.                                             
01985                                  EJECT                            
01986  6500-BUILD-CORRESPOND.                                           
01987 ***************************************************************   
01988 *    THIS ROUTINE WILL GET THE TRAILER SEQUENCE NUMBER FROM   *   
01989 *    THE CLAIM MASTER AND BUILD A CORRESPONDENCE TRAILER      *   
01990 *    USING THE NEW SEQUENCE NUMBER.                           *   
01991 *    INPUT DATA FROM THE SCREEN IS USED TO CREATE THE NEW     *   
01992 *    TRAILER RECORD.                                          *   
01993 ***************************************************************   
01994                                                                   
01995      MOVE PI-CLAIM-NO            TO W-CLM-CLAIM.                  
01996                                                                   
01997      EXEC CICS READ                                               
01998           DATASET    (W-CLM-ID)                                   
01999           SET        (ADDRESS OF CLAIM-MASTER)                    
02000           RIDFLD     (W-CLM-KEY)                                  
02001           UPDATE                                                  
02002           END-EXEC.                                               
02003                                                                   
02004 *    SERVICE RELOAD CLAIM-MASTER.                                 
02005                                                                   
02006      SUBTRACT 1 FROM CL-TRAILER-SEQ-CNT.                          
02007                                                                   
02008      IF  PI-FOLLOW-UP-DATE GREATER THAN CL-NEXT-FOLLOWUP-DT       
02009          MOVE PI-FOLLOW-UP-DATE  TO CL-NEXT-FOLLOWUP-DT.          
02010                                                                   
02011      IF  PI-RESEND-DATE GREATER THAN CL-NEXT-FOLLOWUP-DT          
02012          MOVE PI-RESEND-DATE     TO CL-NEXT-FOLLOWUP-DT.          
02013                                                                   
02014      MOVE '2'                    TO CL-LAST-MAINT-TYPE.           
02015                                                                   
02016      IF  PI-ACTV-POINTER GREATER THAN ZEROS                       
02017          MOVE PI-ACTV-POINTER    TO LCP-WS-ADDR-COMP              
02018          SET ADDRESS OF ACTIVITY-TRAILERS TO LCP-WS-ADDR-PNTR     
02019 *        SERVICE RELOAD ACTIVITY-TRAILERS                         
02020                                                                   
02021      ELSE                                                         
02022          EXEC CICS GETMAIN                                        
02023               SET      (ADDRESS OF ACTIVITY-TRAILERS)             
02024               INITIMG  (W-GETMAINSPACE)                           
02025               LENGTH   (W-ACTV-LENGTH)                            
02026          END-EXEC                                                 
02027          SET LCP-WS-ADDR-PNTR TO ADDRESS OF ACTIVITY-TRAILERS     
02028                                                                   
02029 *        SERVICE RELOAD ACTIVITY-TRAILERS                         
02030          MOVE LCP-WS-ADDR-COMP TO PI-ACTV-POINTER.                
02031                                                                   
02032      MOVE 'AT'                   TO AT-RECORD-ID.                 
02033      MOVE  4                     TO AT-TRAILER-TYPE.              
02034      MOVE W-CURRENT-SAVE         TO AT-RECORDED-DT                
02035                                     CL-LAST-MAINT-DT              
02036                                     AT-CORR-LAST-MAINT-DT         
02037      MOVE PI-PROCESSOR-ID        TO AT-RECORDED-BY                
02038                                     CL-LAST-MAINT-USER            
02039                                     AT-CORR-LAST-UPDATED-BY       
02040      MOVE EIBTIME                TO AT-LAST-MAINT-HHMMSS          
02041                                     CL-LAST-MAINT-HHMMSS.         
02042      MOVE CL-CONTROL-PRIMARY     TO AT-CONTROL-PRIMARY.           
02043 *    MOVE W-ACTV-KEY             TO AT-CONTROL-PRIMARY.           
02044 *    MOVE W-ACTV-SAVE-KEY        TO AT-CONTROL-PRIMARY.           
02045      MOVE PI-CLAIM-NO            TO AT-CLAIM-NO.                  
02046      MOVE CL-TRAILER-SEQ-CNT     TO AT-SEQUENCE-NO                
02047                                     W-CORR-TRLR-SEQ.              
02048      MOVE W-CURRENT-SAVE         TO AT-LETTER-SENT-DT.            
02049      MOVE PI-FOLLOW-UP-DATE      TO AT-RECEIPT-FOLLOW-UP.         
02050      MOVE PI-RESEND-DATE         TO AT-AUTO-RE-SEND-DT.           
02051      MOVE LOW-VALUES             TO AT-LETTER-ANSWERED-DT         
02052                                     AT-LETTER-PURGED-DT.          
02053      MOVE W-ARCH-NUMBER          TO AT-LETTER-ARCHIVE-NO.         
02054      MOVE '1'                    TO AT-LETTER-ORIGIN.             
02055                                                                   
02056      MOVE PI-FORM-NUMBER         TO AT-STD-LETTER-FORM            
040110     MOVE PI-RESEND-FORM-NUMBER  TO AT-RESEND-LETTER-FORM.
040110     MOVE PI-AUTO-CLOSE-IND      TO AT-AUTO-CLOSE-IND.
040110     MOVE PI-LETTER-TO-BENE      TO AT-LETTER-TO-BENE.
02057                                                                   
02058      IF  PI-REASON GREATER THAN LOW-VALUES                        
02059          MOVE PI-REASON          TO AT-REASON-TEXT                
02060                                                                   
02061      ELSE                                                         
02062          MOVE SPACES             TO AT-REASON-TEXT.               
02063                                                                   
02064      MOVE W-PI-ADDR-SEQ          TO AT-ADDRESS-REC-SEQ-NO.        
02065                                                                   
02066      IF  PI-ADDR-TYPE = LOW-VALUES                                
02067          MOVE SPACES             TO AT-ADDRESEE-TYPE              
02068                                                                   
02069      ELSE                                                         
02070          MOVE PI-ADDR-TYPE       TO AT-ADDRESEE-TYPE.             
02071                                                                   
02072      MOVE W-REC-TEXT (1)         TO AT-ADDRESSEE-NAME.            
02073                                                                   
02074      MOVE LOW-VALUES             TO AT-INITIAL-PRINT-DATE         
02075                                     AT-RESEND-PRINT-DATE.         
02076                                                                   
02077      EXEC CICS HANDLE CONDITION                                   
02078          DUPREC    (6596-ACTV-DUPREC)                             
02079          NOSPACE   (6597-ACTV-NOSPACE)                            
02080      END-EXEC.                                                    
02081                                                                   
02082      EXEC CICS WRITE                                              
02083           DATASET    (W-ACTV-ID)                                  
02084           FROM       (ACTIVITY-TRAILERS)                          
02085           RIDFLD     (AT-CONTROL-PRIMARY)                         
02086      END-EXEC.                                                    
02087                                                                   
02088      EXEC CICS HANDLE CONDITION                                   
02089          DUPREC      (6598-REWRITE-CLAIM)                         
02090      END-EXEC.                                                    
02091                                                                   
02092      EXEC CICS REWRITE                                            
02093           DATASET    (W-CLM-ID)                                   
02094           FROM       (CLAIM-MASTER)                               
02095      END-EXEC.                                                    
02096                                                                   
02097      GO TO 6599-EXIT.                                             
02098                                                                   
02099  6596-ACTV-DUPREC.                                                
02100                                                                   
02101      MOVE ER-3697                TO W-CURRENT-ERROR.              
02102      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02103      GO TO 6599-EXIT.                                             
02104                                                                   
02105  6597-ACTV-NOSPACE.                                               
02106                                                                   
02107      MOVE ER-3698                TO W-CURRENT-ERROR.              
02108      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02109      GO TO 6599-EXIT.                                             
02110                                                                   
02111  6598-REWRITE-CLAIM.                                              
02112                                                                   
02113  6599-EXIT.                                                       
02114       EXIT.                                                       
02115                                  EJECT                            
02116  6600-SET-ADDR-SEQ.                                               
02117                                                                   
02118      MOVE PI-ADDR-TYPE           TO W-ADDR-TYPE-CD.               
02119                                                                   
02120      IF  W-ADDR-SEQ NOT NUMERIC                                   
02121          GO TO 6699-EXIT.                                         
02122                                                                   
02123      MOVE ZEROS                  TO W-PI-ADDR-SEQ.                
02124                                                                   
02125      IF  PI-ADDR-TYPE EQUAL 'I'                                   
02126          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02127                                                                   
02128      ELSE                                                         
02129      IF  PI-ADDR-TYPE EQUAL 'A'                                   
02130          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02131          ADD +20                 TO W-ACTV-SEQ                    
02132                                                                   
02133      ELSE                                                         
02134      IF  PI-ADDR-TYPE EQUAL 'B'                                   
02135          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02136          ADD +10                 TO W-ACTV-SEQ                    
02137                                                                   
02138      ELSE                                                         
02139      IF  PI-ADDR-TYPE EQUAL 'P'                                   
02140          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02141          ADD +30                 TO W-ACTV-SEQ                    
02142                                                                   
02143      ELSE                                                         
02144      IF  PI-ADDR-TYPE EQUAL 'E'                                   
02145          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02146          ADD +40                 TO W-ACTV-SEQ                    
02147                                                                   
02148      ELSE                                                         
02149      IF  PI-ADDR-TYPE EQUAL 'O'                                   
02150          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02151          ADD +50                 TO W-ACTV-SEQ                    
02152                                                                   
02153      ELSE                                                         
02154      IF  PI-ADDR-TYPE EQUAL 'Q'                                   
02155          MOVE W-ADDR-SEQ-NUM     TO W-ACTV-SEQ                    
02156          ADD +60                 TO W-ACTV-SEQ.                   
02157                                                                   
02158  6699-EXIT.                                                       
02159       EXIT.                                                       
033110
033110 6700-BUILD-NAPERSOFT.
033110
033110     EXEC CICS GETMAIN                                            
033110          SET      (ADDRESS OF NAPERSOFT-FILE)                    
033110          LENGTH   (W-NAPS-LENGTH)                                
033110     END-EXEC.                                                    
033110
033110     MOVE LOW-VALUES            TO  NAPERSOFT-FILE.
033110     MOVE 'NA'                  TO  NA-RECORD-ID.
033110     MOVE PI-COMPANY-CD         TO  NA-COMPANY-CD.
033110     MOVE PI-CARRIER            TO  NA-CARRIER.
033110     MOVE PI-CLAIM-NO           TO  NA-CLAIM-NO.
033110     MOVE PI-CERT-NO            TO  NA-CERT-NO.
033110     MOVE W-ARCH-NUMBER         TO  NA-ARCHIVE-NO.
033110     MOVE PI-FORM-NUMBER        TO  NA-LETTER-ID.
033110     MOVE PI-PROCESSOR-ID       TO  NA-PROCESSOR-ID.
033110     MOVE W-CURRENT-SAVE        TO  NA-CREATION-DT.
033110     MOVE LOW-VALUES            TO  NA-INITIAL-PRINT-DT
033110     MOVE PI-FOLLOW-UP-DATE     TO  NA-FOLLOW-UP-DT.
033110     MOVE PI-RESEND-DATE        TO  NA-RESEND-DT
040110     MOVE PI-RESEND-FORM-NUMBER TO  NA-RESEND-LETTER-ID.
033110     IF PI-NUMBER-COPIES NOT = ZEROS
033110         MOVE PI-NUMBER-COPIES  TO  NA-NO-OF-COPIES
033110     ELSE
033110         MOVE 1                 TO  NA-NO-OF-COPIES
033110     END-IF.
033110     IF PI-ADDR-TYPE = ' 0'
033110         MOVE LOW-VALUES        TO  NA-ADDRESS-TYPE
033110     ELSE
033110         MOVE PI-ADDR-TYPE      TO  NA-ADDRESS-TYPE
033110     END-IF.
033110     MOVE W-CORR-TRLR-SEQ       TO  NA-CORR-TRLR-SEQ.
040110     MOVE PI-ENCLOSURE-CD       TO  NA-ENCLOSURE-CD.
033110     IF PI-AUTO-LETTER-DATE > SPACES
033110        MOVE PI-AUTO-LETTER-DATE TO DC-GREG-DATE-A-EDIT 
033110        MOVE DC-EDITA-MONTH     TO DC-EDIT1-MONTH
033110        MOVE SLASHA-1           TO SLASH1-1            
033110        MOVE DC-EDITA-DAY       TO DC-EDIT1-DAY
033110        MOVE SLASHA-2           TO SLASH1-2
033110        MOVE DC-EDITA-YEAR      TO DC-EDIT1-YEAR
033110        MOVE '2'                TO  DC-OPTION-CODE                
033110        PERFORM 9700-DATE-LINK  THRU  9700-EXIT                   
033110        IF DATE-CONVERSION-ERROR                                  
033110            MOVE LOW-VALUES     TO  NA-AUTOPYDT            
033110        ELSE                                                      
033110            MOVE DC-BIN-DATE-1  TO  NA-AUTOPYDT         
033110     ELSE                                                         
033110        MOVE LOW-VALUES         TO  NA-AUTOPYDT
033110     END-IF.
033110
033110     EXEC CICS WRITE                                              
033110          DATASET    (W-NAPS-ID)                                  
033110          FROM       (NAPERSOFT-FILE)                          
033110          RIDFLD     (NA-CONTROL-PRIMARY)                         
033110     END-EXEC.                                                    
033110
033110 6799-EXIT.
033110      EXIT.
02160                                  EJECT                            
02161  7000-RESOLVE-VARIABLES.                                          
02162 ***************************************************************   
02163 *    THIS ROUTINE WILL FORMAT THE SYSTEM DEFINED SYMBOLS      *   
02164 *    WITH DATA PERTAINING        TO THE DESIGNATED CLAIM.     *   
02165 *    THIS ROUTINE IS PERFORM THRU 7399-EXIT IN ORDER TO       *   
02166 *    RESOLVE ALL OF THE SYMBOLS.                              *   
02167 ***************************************************************   
02168                                                                   
02169      MOVE PI-CLAIM-NO            TO SS34D.                        
02170                                                                   

121802     EVALUATE TRUE
121802
121802     WHEN CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1
121802        MOVE PI-AH-OVERRIDE-L6   TO SS12D
121802
121802     WHEN CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1
121802        MOVE PI-LIFE-OVERRIDE-L6 TO SS12D
121802
121802     WHEN CL-CLAIM-TYPE = 'I'
121802        MOVE '  IU  '            TO SS12D
121802
121802     WHEN CL-CLAIM-TYPE = 'G'
121802        MOVE ' GAP  '            TO SS12D
080322
080322     WHEN CL-CLAIM-TYPE = 'B'
080322        MOVE ' BRV  '            TO SS12D
080322
080322     WHEN CL-CLAIM-TYPE = 'H'
080322        MOVE ' HOSP '            TO SS12D
100518
100518     WHEN CL-CLAIM-TYPE = 'O'
100518        MOVE ' OTH  '            TO SS12D
121802
121802     END-EVALUATE

02178      MOVE CL-LAST-PMT-AMT        TO SS16D.                        
02179      MOVE CL-TOTAL-PAID-AMT      TO SS18D.                        
02180      MOVE CL-CAUSE-CD            TO SS19-1D.                      
02181      MOVE CL-ASSOC-CERT-SEQU     TO SS54D.                        
02182      MOVE CL-ASSOC-CERT-TOTAL    TO SS55D.                        
02183                                                                   
02184      MOVE CL-INCURRED-DT         TO DC-BIN-DATE-1.                
02185      MOVE SPACES                 TO DC-OPTION-CODE.               
02186      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02187                                                                   
02188      IF  NO-CONVERSION-ERROR                                      
02189          MOVE DC-GREG-DATE-1-EDIT                                 
02190                                  TO SS13D.                        
02191                                                                   
02192      MOVE CL-REPORTED-DT         TO DC-BIN-DATE-1.                
02193      MOVE SPACES                 TO DC-OPTION-CODE.               
02194      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02195                                                                   
02196      IF  NO-CONVERSION-ERROR                                      
02197          MOVE DC-GREG-DATE-1-EDIT                                 
02198                                  TO SS14D.                        
02199                                                                   
02200      MOVE CL-LAST-PMT-DT         TO DC-BIN-DATE-1.                
02201      MOVE SPACES                 TO DC-OPTION-CODE.               
02202      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02203                                                                   
02204      IF  NO-CONVERSION-ERROR                                      
02205          MOVE DC-GREG-DATE-1-EDIT                                 
02206                                  TO SS15D.                        
02207                                                                   
02208      IF  NOT PI-USES-PAID-TO                                      
02209          MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1                 
02210          MOVE SPACES             TO DC-OPTION-CODE                
02211          PERFORM 9700-DATE-LINK THRU 9700-EXIT                    
02212                                                                   
02213          IF  NO-CONVERSION-ERROR                                  
02214              MOVE DC-GREG-DATE-1-EDIT                             
02215                                  TO SS17D                         
02216                                                                   
02217          ELSE                                                     
02218              MOVE SPACES         TO SS17D                         
02219                                                                   
02220      ELSE                                                         
02221          MOVE CL-PAID-THRU-DT    TO DC-BIN-DATE-1                 
02222          MOVE '6'                TO DC-OPTION-CODE                
02223          MOVE +1                 TO DC-ELAPSED-DAYS               
02224          MOVE +0                 TO DC-ELAPSED-MONTHS             
02225          PERFORM 9700-DATE-LINK THRU 9700-EXIT                    
02226                                                                   
02227          IF  NO-CONVERSION-ERROR                                  
02228              MOVE DC-GREG-DATE-1-EDIT                             
02229                                  TO SS17D.                        
02230                                                                   
02231      MOVE CL-INSURED-BIRTH-DT    TO DC-BIN-DATE-1.                
02232      MOVE SPACES                 TO DC-OPTION-CODE.               
02233      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02234                                                                   
02235      IF  NO-CONVERSION-ERROR                                      
02236          MOVE DC-GREG-DATE-1-EDIT                                 
02237                                  TO SS45D                         
02238                                                                   
02239      ELSE                                                         
02240          MOVE '@@DOB'            TO SS45D.                        
02241                                                                   
02242      IF  CL-SSN-STATE = CL-CERT-STATE                             
02243                                                                   
02244          IF  CL-SSN-ACCOUNT EQUAL CL-CERT-ACCOUNT-PRIME           
02245              NEXT SENTENCE                                        
02246                                                                   
02247          ELSE                                                     
02248              MOVE CL-SOC-SEC-NO  TO SS46D                         
02249                                                                   
02250      ELSE                                                         
02251          MOVE CL-SOC-SEC-NO      TO SS46D.                        
02252                                                                   
02253      MOVE CL-CERT-GROUPING       TO W-CERT-GROUPING               
02254                                     W-ACCT-GROUPING               
02255                                     W-PROD-GROUPING               
02256                                     W-PLCY-GROUPING               
02257                                     W-PLAN-GROUPING.              
02258      MOVE CL-CERT-STATE          TO W-CERT-STATE                  
02259                                     W-ACCT-STATE                  
02260                                     W-PROD-STATE                  
02261                                     W-PLCY-STATE                  
02262                                     W-PLAN-STATE.                 
02263      MOVE CL-CERT-ACCOUNT        TO W-CERT-ACCOUNT                
02264                                     W-ACCT-ACCOUNT                
02265                                     W-PROD-PRODUCER               
02266                                     W-PLCY-PRODUCER               
02267                                     W-PLAN-PRODUCER.              
02268      MOVE CL-CERT-EFF-DT         TO W-CERT-EFF-DT                 
02269                                     W-PLCY-EFF-DT.                
02270      MOVE CL-CV-REFERENCE-NO     TO W-PLCY-REFERENCE-NO.          
02271                                                                   
02272      PERFORM 7400-MOVE-NAME THRU 7400-EXIT.                       
02273                                                                   
02274      IF  LOWER-CASE-LETTERS-USED                                  
02275          MOVE W-NAME-WORK        TO W-TEMP-AREA2                  
02276          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
02277          MOVE W-TEMP-AREA2       TO SS10D                         
02278                                                                   
02279      ELSE                                                         
02280          MOVE W-NAME-WORK        TO SS10D.                        
02281                                                                   
02282      MOVE CL-INSURED-1ST-NAME    TO W-FIRST-NAME.                 
02283      MOVE CL-INSURED-LAST-NAME   TO W-LAST-NAME.                  
02284      MOVE CL-INSURED-MID-INIT    TO W-MIDDLE-NAME.                
02285      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.                       
02286                                                                   
02287      IF  LOWER-CASE-LETTERS-USED                                  
02288          MOVE W-NAME-WORK        TO W-TEMP-AREA2                  
02289          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
02290          MOVE W-TEMP-AREA2       TO SS39D                         
02291                                                                   
02292      ELSE                                                         
02293          MOVE W-NAME-WORK        TO SS39D.                        
02294                                                                   
02295      MOVE CL-INSURED-LAST-NAME   TO SS40D.                        
02296                                                                   
02297      IF  INSURED-IS-FEMALE                                        
02298          MOVE 'MS.'              TO SS41D                         
02299                                                                   
02300      ELSE                                                         
02301          MOVE 'MR.'              TO SS41D.                        
02302                                                                   
02303      IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'                       
02304          PERFORM 7370-RESOLVE-CREDITOR THRU 7370-EXIT.            
02305                                                                   
02306      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     
02307          GO TO 7010-READ-POLICY-RECORD.                           
02308                                  EJECT                            
02309      EXEC CICS HANDLE CONDITION                                   
02310           NOTOPEN   (7022-CERT-NOT-OPEN)                          
02311           NOTFND    (7024-CERT-NOT-FOUND)                         
02312      END-EXEC.                                                    
02313                                                                   
02314      EXEC CICS READ                                               
02315           DATASET   (W-CERT-ID)                                   
02316           SET       (ADDRESS OF CERTIFICATE-MASTER)               
02317           RIDFLD    (W-CERT-KEY)                                  
02318      END-EXEC.                                                    
02319                                                                   
02320 *    SERVICE RELOAD CERTIFICATE-MASTER.                           
02321                                                                   
02322      MOVE CM-CERT-NO             TO SS26D.                        
02323      MOVE CM-INSURED-ISSUE-AGE   TO SS33D.                        
02324      MOVE CM-LOAN-BALANCE        TO SS37D.                        
02325      MOVE CM-MEMBER-NO           TO SS38D.                        
02326                                                                   
02327      IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'                       
02328          MOVE CL-CURRENT-CARRIER TO SS23D                         
02329          MOVE CL-CURRENT-GROUPING                                 
02330                                  TO SS24D                         
02331          MOVE CL-CURRENT-ACCOUNT TO SS25D                         
02332          MOVE CM-MEMBER-NO       TO SS36D                         
02333          MOVE CM-GROUPING        TO W-GROUPING                    
02334                                                                   
02335          IF  W-GROUP-3 EQUAL 'C01' OR 'C02'                       
02336              MOVE CM-MEMBER-NO   TO W-CURRENT-LOAN-NO             
02337              MOVE CM-LOAN-NUMBER TO W-LOAN-NUMBER                 
02338              MOVE W-CREDIT-CARD-LOAN-NO                           
02339                                  TO SS36-1D                       
02340                                                                   
02341          ELSE                                                     
02342              MOVE CM-MEMBER-NO   TO SS36-1D                       
02343                                                                   
02344      ELSE                                                         
02345          MOVE CM-CARRIER         TO SS23D                         
02346          MOVE CM-GROUPING        TO SS24D                         
02347          MOVE CM-ACCOUNT         TO SS25D                         
02348          MOVE CM-LOAN-NUMBER     TO SS36D.                        
02349                                                                   
02350      MOVE CM-CERT-EFF-DT         TO DC-BIN-DATE-1.                
02351      MOVE SPACES                 TO DC-OPTION-CODE.               
02352      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02353                                                                   
02354      IF  NO-CONVERSION-ERROR                                      
02355          MOVE DC-GREG-DATE-1-EDIT TO SS27D.                       
02356                                                                   
02357      MOVE CM-INSURED-FIRST-NAME  TO W-FIRST-NAME.                 
02358      MOVE CM-INSURED-LAST-NAME   TO W-LAST-NAME.                  
02359      MOVE CM-INSURED-INITIAL2    TO W-MIDDLE-NAME.                
02360      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.                       
02361                                                                   
02362      IF  LOWER-CASE-LETTERS-USED                                  
02363          MOVE W-NAME-WORK        TO W-TEMP-AREA2                  
02364          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
02365          MOVE W-TEMP-AREA2       TO SS57D                         
02366                                                                   
02367      ELSE                                                         
02368          MOVE W-NAME-WORK        TO SS57D.                        
02369                                                                   
02370      MOVE CM-JT-FIRST-NAME       TO W-FIRST-NAME.                 
02371      MOVE CM-JT-LAST-NAME        TO W-LAST-NAME.                  
02372      MOVE CM-JT-INITIAL          TO W-MIDDLE-NAME.                
02373      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.                       
02374                                                                   
02375      IF  LOWER-CASE-LETTERS-USED                                  
02376          MOVE W-NAME-WORK        TO W-TEMP-AREA2                  
02377          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
02378          MOVE W-TEMP-AREA2       TO SS58D                         
02379                                                                   
02380      ELSE                                                         
02381          MOVE W-NAME-WORK        TO SS58D.                        
02382                                                                   
100518     IF  CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
02384          MOVE CM-LF-BENEFIT-CD   TO W-BEN-HOLD                    
02385          MOVE CM-LF-ORIG-TERM    TO SS29D                         
02386          MOVE CM-LF-BENEFIT-AMT  TO SS30D                         
02387          MOVE CM-POLICY-FORM-NO  TO SS32D                         
02388          MOVE CM-LF-CANCEL-DT    TO DC-BIN-DATE-1                 
02389                                                                   
02390      ELSE                                                         
02391          MOVE CM-AH-BENEFIT-CD   TO W-BEN-HOLD                    
02392          MOVE CM-AH-ORIG-TERM    TO SS29D                         
02393          MOVE CM-AH-BENEFIT-AMT  TO SS30D                         
02394          MOVE CM-POLICY-FORM-NO  TO SS32D                         
02395          MOVE CM-AH-CANCEL-DT    TO DC-BIN-DATE-1.                
02396                                                                   
02397      MOVE ZEROS                  TO W-WORK-AMOUNT.                
02398      COMPUTE W-WORK-AMOUNT =                                      
02399         (CM-AH-ORIG-TERM * CM-AH-BENEFIT-AMT).                    
02400      MOVE W-WORK-AMOUNT          TO SS51D.                        
02401                                                                   
02402      MOVE SPACES                 TO DC-OPTION-CODE.               
02403      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02404      IF  NO-CONVERSION-ERROR                                      
02405          MOVE DC-GREG-DATE-1-EDIT TO SS31D.                       
02406                                                                   
02407      MOVE ' '                    TO DC-OPTION-CODE.               
02408      MOVE +0                     TO DC-ELAPSED-MONTHS             
02409                                           DC-ELAPSED-DAYS.        
02410                                                                   
100518     IF  CL-CLAIM-TYPE EQUAL     TO PI-LIFE-OVERRIDE-L1 OR 'O'
02412          MOVE CM-LF-LOAN-EXPIRE-DT                                
02413                                  TO DC-BIN-DATE-1                 
02414          PERFORM 9700-DATE-LINK THRU 9700-EXIT                    
02415                                                                   
02416          IF  NO-CONVERSION-ERROR                                  
02417              MOVE DC-GREG-DATE-1-EDIT TO SS28D                    
02418                                                                   
02419          ELSE                                                     
02420              NEXT SENTENCE                                        
02421                                                                   
02422      ELSE                                                         
02423          MOVE CM-AH-LOAN-EXPIRE-DT                                
02424                                  TO DC-BIN-DATE-1                 
02425          PERFORM 9700-DATE-LINK THRU 9700-EXIT                    
02426                                                                   
02427          IF  NO-CONVERSION-ERROR                                  
02428              MOVE DC-GREG-DATE-1-EDIT                             
02429                                  TO SS28D.                        
02430                                                                   
02431      GO TO 7015-READ-DIAGNOSIS-TRAILER.                           
02432                                                                   
02433      EJECT                                                        
02434  7010-READ-POLICY-RECORD.                                         
02435                                                                   
02436      EXEC CICS HANDLE CONDITION                                   
02437           NOTOPEN    (7026-PLCY-NOT-OPEN)                         
02438           NOTFND     (7028-PLCY-NOT-FOUND)                        
02439      END-EXEC.                                                    
02440                                                                   
02441      EXEC CICS READ                                               
02442           DATASET  (W-PLCY-ID)                                    
02443           SET      (ADDRESS OF POLICY-MASTER)                     
02444           RIDFLD   (W-PLCY-KEY)                                   
02445      END-EXEC.                                                    
02446                                                                   
02447 *    SERVICE RELOAD POLICY-MASTER.                                
02448                                                                   
02449      MOVE PM-REFERENCE-NUMBER    TO SS59D.                        
02450      MOVE PM-INSURED-ISSUE-AGE   TO SS33D.                        
02451      MOVE PM-LOAN-BALC           TO SS37D.                        
02452      MOVE PM-CARRIER             TO SS23D.                        
02453      MOVE PM-GROUPING            TO SS24D.                        
02454      MOVE PM-PRODUCER            TO SS25D.                        
02455      MOVE PM-LOAN-NUMBER         TO SS36-1D.                      
02456                                                                   
02457      MOVE PM-POLICY-EFF-DT       TO DC-BIN-DATE-1.                
02458      MOVE SPACES                 TO DC-OPTION-CODE.               
02459      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02460                                                                   
02461      IF NO-CONVERSION-ERROR                                       
02462          MOVE DC-GREG-DATE-1-EDIT TO SS27D.                       
02463                                                                   
02464      MOVE PM-INSURED-FIRST-NAME  TO W-FIRST-NAME.                 
02465      MOVE PM-INSURED-LAST-NAME   TO W-LAST-NAME.                  
02466      MOVE PM-INSURED-MIDDLE-INIT TO W-MIDDLE-NAME.                
02467      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.                       
02468                                                                   
02469      IF LOWER-CASE-LETTERS-USED                                   
02470          MOVE W-NAME-WORK        TO W-TEMP-AREA2                  
02471          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
02472          MOVE W-TEMP-AREA2       TO SS57D                         
02473      ELSE                                                         
02474          MOVE W-NAME-WORK        TO SS57D.                        
02475                                                                   
02476      MOVE PM-JOINT-FIRST-NAME    TO W-FIRST-NAME.                 
02477      MOVE PM-JOINT-LAST-NAME     TO W-LAST-NAME.                  
02478      MOVE PM-JOINT-MIDDLE-INIT   TO W-MIDDLE-NAME.                
02479      PERFORM 7500-MOVE-NAME THRU 7500-EXIT.                       
02480                                                                   
02481      IF LOWER-CASE-LETTERS-USED                                   
02482          MOVE W-NAME-WORK        TO W-TEMP-AREA2                  
02483          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
02484          MOVE W-TEMP-AREA2       TO SS58D                         
02485      ELSE                                                         
02486          MOVE W-NAME-WORK        TO SS58D.                        
02487                                                                   
02488      MOVE PM-INS-PLAN-CD         TO W-BEN-HOLD.                   
02489      MOVE PM-LOAN-TERM           TO SS29D.                        
02490      MOVE PM-INS-POLICY-FORM     TO SS32D.                        
02491                                                                   
02492      IF PM-AH-MORT-PLAN                                           
02493          MOVE PM-INS-MONTH-BENEFIT TO SS30D                       
02494          MOVE ZEROS                TO W-WORK-AMOUNT               
02495          COMPUTE W-WORK-AMOUNT =                                  
02496              (PM-INS-MONTH-BENEFIT * PM-LOAN-TERM)                
02497          MOVE W-WORK-AMOUNT        TO SS51D                       
02498      ELSE                                                         
02499          MOVE PM-INS-TOTAL-BENEFIT TO SS30D.                      
02500                                                                   
02501      MOVE PM-CANCEL-DT           TO DC-BIN-DATE-1.                
02502      MOVE SPACES                 TO DC-OPTION-CODE.               
02503      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02504                                                                   
02505      IF NO-CONVERSION-ERROR                                       
02506          MOVE DC-GREG-DATE-1-EDIT TO SS31D.                       
02507                                                                   
02508      MOVE PM-INS-TERMINATION-DT  TO DC-BIN-DATE-1.                
02509      MOVE SPACES                 TO DC-OPTION-CODE.               
02510      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
02511                                                                   
02512      IF NO-CONVERSION-ERROR                                       
02513          MOVE DC-GREG-DATE-1-EDIT TO SS28D.                       
02514                                                                   
02515      EJECT                                                        
02516  7015-READ-DIAGNOSIS-TRAILER.                                     
02517      EXEC CICS HANDLE CONDITION                                   
02518           NOTOPEN    (7020-ACTV-NOT-OPEN)                         
02519           NOTFND     (7040-READ-BENEFICIARY)                      
02520      END-EXEC.                                                    
02521                                                                   
02522      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-KEY.                   
02523      MOVE +90                    TO W-ACTV-SEQ.                   
02524                                                                   
02525      EXEC CICS READ                                               
02526           DATASET  (W-ACTV-ID)                                    
02527           SET      (ADDRESS OF ACTIVITY-TRAILERS)                 
02528           RIDFLD   (W-ACTV-KEY)                                   
02529      END-EXEC.                                                    
02530                                                                   
02531 *    SERVICE RELOAD ACTIVITY-TRAILERS                             
02532                                                                   
02533      IF  AT-TRAILER-TYPE EQUAL '6'                                
02534          MOVE AT-INFO-LINE-1     TO SS19D.                        
02535                                                                   
02536      GO TO 7040-READ-BENEFICIARY.                                 
02537                                  EJECT                            
02538  7020-ACTV-NOT-OPEN.                                              
02539                                                                   
02540      MOVE ER-0172                TO W-CURRENT-ERROR.              
02541      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02542      GO TO 7399-EXIT.                                             
02543                                                                   
02544  7022-CERT-NOT-OPEN.                                              
02545                                                                   
02546      MOVE ER-0169                TO W-CURRENT-ERROR.              
02547      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02548      GO TO 7399-EXIT.                                             
02549                                                                   
02550  7024-CERT-NOT-FOUND.                                             
02551                                                                   
02552      MOVE ER-0206                TO W-CURRENT-ERROR.              
02553      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02554      GO TO 7399-EXIT.                                             
02555                                                                   
02556  7026-PLCY-NOT-OPEN.                                              
02557                                                                   
02558      MOVE ER-9883                TO W-CURRENT-ERROR.              
02559      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02560      GO TO 7399-EXIT.                                             
02561                                                                   
02562  7028-PLCY-NOT-FOUND.                                             
02563                                                                   
02564      MOVE ER-9483                TO W-CURRENT-ERROR.              
02565      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
02566      GO TO 7399-EXIT.                                             
02567                                  EJECT                            
02568                                                                   
02569  7040-READ-BENEFICIARY.                                           
02570                                                                   
02571      IF  CL-BENIF-ADDR-CNT EQUAL +0                               
02572              AND                                                  
02573          CL-BENEFICIARY = SPACES                                  
02574          GO TO 7100-READ-PHYSICIAN-ADDR.                          
02575                                                                   
02576      MOVE CL-BENIF-ADDR-CNT      TO W-ACTV-SEQ.                   
02577      ADD +10                     TO W-ACTV-SEQ.                   
02578                                                                   
02579      EXEC CICS HANDLE CONDITION                                   
02580           NOTOPEN    (7380-BENE-NOT-OPEN)                         
02581           NOTFND     (7100-READ-PHYSICIAN-ADDR)                   
02582      END-EXEC.                                                    
02583                                                                   
02584      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
02585                                                                   
02586      IF  W-ACTV-SEQ NOT EQUAL +10                                 
02587          GO TO 7060-GET-FROM-ACTIVITY.                            
02588                                                                   
02589      MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.               
02590      MOVE 'B'                    TO W-BENE-REC-TYPE.              
02591      MOVE CL-BENEFICIARY         TO W-BENE-NUMBER.                
02592                                                                   
02593      EXEC CICS READ                                               
02594           DATASET    (W-BENE-ID)                                  
02595           SET        (ADDRESS OF BENEFICIARY-MASTER)              
02596           RIDFLD     (W-BENE-KEY)                                 
02597      END-EXEC.                                                    
02598                                                                   
02599 *    SERVICE RELOAD BENEFICIARY-MASTER.                           
02600                                                                   
02601      MOVE BE-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
02602      MOVE BE-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02603      MOVE BE-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02604      MOVE BE-CITY-STATE          TO W-LABEL-LINES (04).           
02605                                                                   
02606      MOVE SPACES                 TO W-ZIP-CODE.                   
02607                                                                   
02608      IF  BE-CANADIAN-POST-CODE                                    
02609          MOVE BE-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02610          MOVE BE-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02611                                                                   
02612      ELSE                                                         
02613          MOVE BE-ZIP-PRIME       TO W-AM-ZIP-CODE                 
02614                                                                   
02615          IF  BE-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02616              MOVE '-'            TO W-AM-ZIP-DASH                 
02617              MOVE BE-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02618                                                                   
02619      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02620                                                                   
02621      MOVE BE-PHONE-NO            TO W-PHONE-IN.                   
02622                                                                   
02623      GO TO 7080-SET-PHONE.                                        
02624                                                                   
02625  7060-GET-FROM-ACTIVITY.                                          
02626                                                                   
02627      EXEC CICS HANDLE CONDITION                                   
02628           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
02629           NOTFND     (7100-READ-PHYSICIAN-ADDR)                   
02630      END-EXEC.                                                    
02631                                                                   
02632      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02633                                                                   
02634      EXEC CICS READ                                               
02635           DATASET  (W-ACTV-ID)                                    
02636           SET      (ADDRESS OF ACTIVITY-TRAILERS)                 
02637           RIDFLD   (W-ACTV-KEY)                                   
02638      END-EXEC.                                                    
02639                                                                   
02640 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
02641                                                                   
02642      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
02643      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02644      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02645      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
02646                                                                   
02647      MOVE SPACES                 TO W-ZIP-CODE.                   
02648                                                                   
02649      IF  AT-CANADIAN-POST-CODE                                    
02650          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02651          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02652                                                                   
02653      ELSE                                                         
02654          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
02655                                                                   
02656          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02657              MOVE '-'            TO W-AM-ZIP-DASH                 
02658              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02659                                                                   
02660      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02661                                                                   
02662      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
02663                                                                   
02664  7080-SET-PHONE.                                                  
02665                                                                   
02666      MOVE W-PI-AREA              TO W-PO-AREA.                    
02667      MOVE W-PI-PFX               TO W-PO-PFX.                     
02668      MOVE W-PI-SFX               TO W-PO-SFX.                     
02669      MOVE W-PHONE-OUT            TO SS44-5D.                      
02670                                                                   
02671      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
02672                                                                   
02673      MOVE W-LABEL-LINES (01)     TO SS43D.                        
02674      MOVE W-LABEL-LINES (02)     TO SS44-1D.                      
02675      MOVE W-LABEL-LINES (03)     TO SS44-2D.                      
02676      MOVE W-LABEL-LINES (04)     TO SS44-3D.                      
02677      MOVE W-LABEL-LINES (05)     TO SS44-4D.                      
02678                                  EJECT                            
02679  7100-READ-PHYSICIAN-ADDR.                                        
02680                                                                   
02681      MOVE CL-DOCTOR-ADDR-CNT     TO W-ACTV-SEQ.                   
02682      ADD +30                     TO W-ACTV-SEQ.                   
02683                                                                   
02684      IF  W-ACTV-SEQ EQUAL +30                                     
02685          GO TO 7120-READ-EMPLOYER-ADDR.                           
02686                                                                   
02687      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02688                                                                   
02689      EXEC CICS HANDLE CONDITION                                   
02690           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
02691           NOTFND     (7120-READ-EMPLOYER-ADDR)                    
02692      END-EXEC.                                                    
02693                                                                   
02694      EXEC CICS READ                                               
02695           DATASET    (W-ACTV-ID)                                  
02696           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
02697           RIDFLD     (W-ACTV-KEY)                                 
02698      END-EXEC.                                                    
02699                                                                   
02700 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
02701                                                                   
02702      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
02703      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
02704      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02705      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02706      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
02707                                                                   
02708      MOVE SPACES                 TO W-ZIP-CODE.                   
02709                                                                   
02710      IF  AT-CANADIAN-POST-CODE                                    
02711          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02712          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02713                                                                   
02714      ELSE                                                         
02715          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
02716                                                                   
02717          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02718              MOVE '-'            TO W-AM-ZIP-DASH                 
02719              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02720                                                                   
02721      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02722                                                                   
02723      MOVE ZEROS                  TO W-PHONE-IN.                   
02724      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
02725      MOVE W-PI-AREA              TO W-PO-AREA.                    
02726      MOVE W-PI-PFX               TO W-PO-PFX.                     
02727      MOVE W-PI-SFX               TO W-PO-SFX.                     
02728      MOVE W-PHONE-OUT            TO SS47-5D.                      
02729                                                                   
02730      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
02731                                                                   
02732      MOVE W-LABEL-LINES (01)     TO SS47D.                        
02733      MOVE W-LABEL-LINES (02)     TO SS47-1D.                      
02734      MOVE W-LABEL-LINES (03)     TO SS47-2D.                      
02735      MOVE W-LABEL-LINES (04)     TO SS47-3D.                      
02736      MOVE W-LABEL-LINES (05)     TO SS47-4D.                      
02737      GO TO 7120-READ-EMPLOYER-ADDR.                               
02738                                  EJECT                            
02739                                                                   
02740  7120-READ-EMPLOYER-ADDR.                                         
02741                                                                   
02742      MOVE CL-EMPLOYER-ADDR-CNT   TO W-ACTV-SEQ.                   
02743      ADD +40                     TO W-ACTV-SEQ.                   
02744                                                                   
02745      IF  W-ACTV-SEQ EQUAL +40                                     
02746          GO TO 7140-READ-INSURED-ADDR.                            
02747                                                                   
02748      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02749                                                                   
02750      EXEC CICS HANDLE CONDITION                                   
02751           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
02752           NOTFND     (7140-READ-INSURED-ADDR)                     
02753      END-EXEC.                                                    
02754                                                                   
02755      EXEC CICS READ                                               
02756           DATASET    (W-ACTV-ID)                                  
02757           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
02758           RIDFLD     (W-ACTV-KEY)                                 
02759      END-EXEC.                                                    
02760                                                                   
02761 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
02762                                                                   
02763      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
02764      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
02765      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02766      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02767      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
02768                                                                   
02769      MOVE SPACES                 TO W-ZIP-CODE.                   
02770                                                                   
02771      IF  AT-CANADIAN-POST-CODE                                    
02772          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02773          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02774                                                                   
02775      ELSE                                                         
02776          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
02777                                                                   
02778          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02779              MOVE '-'            TO W-AM-ZIP-DASH                 
02780              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02781                                                                   
02782      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02783                                                                   
02784      MOVE ZEROS                  TO W-PHONE-IN.                   
02785      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
02786      MOVE W-PI-AREA              TO W-PO-AREA.                    
02787      MOVE W-PI-PFX               TO W-PO-PFX.                     
02788      MOVE W-PI-SFX               TO W-PO-SFX.                     
02789      MOVE W-PHONE-OUT            TO SS48-5D.                      
02790                                                                   
02791      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
02792                                                                   
02793      MOVE W-LABEL-LINES (01)     TO SS48D.                        
02794      MOVE W-LABEL-LINES (02)     TO SS48-1D.                      
02795      MOVE W-LABEL-LINES (03)     TO SS48-2D.                      
02796      MOVE W-LABEL-LINES (04)     TO SS48-3D.                      
02797      MOVE W-LABEL-LINES (05)     TO SS48-4D.                      
02798                                                                   
02799                                  EJECT                            
02800  7140-READ-INSURED-ADDR.                                          
02801                                                                   
02802      MOVE CL-INSURED-ADDR-CNT    TO W-ACTV-SEQ.                   
02803                                                                   
02804      IF  W-ACTV-SEQ EQUAL +0                                      
02805          GO TO 7160-READ-OTHER1-ADDR.                             
02806                                                                   
02807      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02808                                                                   
02809      EXEC CICS HANDLE CONDITION                                   
02810           NOTOPEN   (7390-ACTV-NOT-OPEN)                          
02811           NOTFND    (7160-READ-OTHER1-ADDR)                       
02812           END-EXEC.                                               
02813                                                                   
02814      EXEC CICS READ                                               
02815           DATASET   (W-ACTV-ID)                                   
02816           SET       (ADDRESS OF ACTIVITY-TRAILERS)                
02817           RIDFLD    (W-ACTV-KEY)                                  
02818           END-EXEC.                                               
02819                                                                   
02820 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
02821                                                                   
02822      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
02823      MOVE SS10D                  TO W-LABEL-LINES (01).           
02824      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02825      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02826      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
02827                                                                   
02828      MOVE SPACES                 TO W-ZIP-CODE.                   
02829                                                                   
02830      IF  AT-CANADIAN-POST-CODE                                    
02831          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02832          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02833                                                                   
02834      ELSE                                                         
02835          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
02836                                                                   
02837          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02838              MOVE '-'            TO W-AM-ZIP-DASH                 
02839              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02840                                                                   
02841      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02842                                                                   
02843      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
02844                                                                   
02845      MOVE W-LABEL-LINES (01)     TO SS10D.                        
02846      MOVE W-LABEL-LINES (02)     TO SS11-1D.                      
02847      MOVE W-LABEL-LINES (03)     TO SS11-2D.                      
02848      MOVE W-LABEL-LINES (04)     TO SS11-3D.                      
02849      MOVE W-LABEL-LINES (05)     TO SS11-4D.                      
02850      MOVE AT-MAIL-TO-NAME        TO SS11-5D.                      
02851                                                                   
02852      MOVE ZEROS                  TO W-PHONE-IN.                   
02853      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
02854      MOVE W-PI-AREA              TO W-PO-AREA.                    
02855      MOVE W-PI-PFX               TO W-PO-PFX.                     
02856      MOVE W-PI-SFX               TO W-PO-SFX.                     
02857      MOVE W-PHONE-OUT            TO SS11-6D.                      
02858                                                                   
02859                                  EJECT                            
02860  7160-READ-OTHER1-ADDR.                                           
02861                                                                   
02862      MOVE CL-OTHER-1-ADDR-CNT    TO W-ACTV-SEQ.                   
02863      ADD +50                     TO W-ACTV-SEQ.                   
02864                                                                   
02865      IF  W-ACTV-SEQ EQUAL +50                                     
02866          GO TO 7180-READ-OTHER2-ADDR.                             
02867                                                                   
02868      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02869                                                                   
02870      EXEC CICS HANDLE CONDITION                                   
02871           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
02872           NOTFND     (7180-READ-OTHER2-ADDR)                      
02873           END-EXEC.                                               
02874                                                                   
02875      EXEC CICS READ                                               
02876           DATASET    (W-ACTV-ID)                                  
02877           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
02878           RIDFLD     (W-ACTV-KEY)                                 
02879           END-EXEC.                                               
02880                                                                   
02881 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
02882                                                                   
02883      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
02884      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
02885      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02886      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02887      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
02888                                                                   
02889      MOVE SPACES                 TO W-ZIP-CODE.                   
02890                                                                   
02891      IF  AT-CANADIAN-POST-CODE                                    
02892          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02893          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02894                                                                   
02895      ELSE                                                         
02896          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
02897                                                                   
02898          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02899              MOVE '-'            TO W-AM-ZIP-DASH                 
02900              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02901                                                                   
02902      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02903                                                                   
02904      MOVE ZEROS                  TO W-PHONE-IN.                   
02905      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
02906      MOVE W-PI-AREA              TO W-PO-AREA.                    
02907      MOVE W-PI-PFX               TO W-PO-PFX.                     
02908      MOVE W-PI-SFX               TO W-PO-SFX.                     
02909      MOVE W-PHONE-OUT            TO SS49-5D.                      
02910                                                                   
02911      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
02912                                                                   
02913      MOVE W-LABEL-LINES (01)     TO SS49D.                        
02914      MOVE W-LABEL-LINES (02)     TO SS49-1D.                      
02915      MOVE W-LABEL-LINES (03)     TO SS49-2D.                      
02916      MOVE W-LABEL-LINES (04)     TO SS49-3D.                      
02917      MOVE W-LABEL-LINES (05)     TO SS49-4D.                      
02918                                  EJECT                            
02919  7180-READ-OTHER2-ADDR.                                           
02920                                                                   
02921      MOVE CL-OTHER-2-ADDR-CNT    TO W-ACTV-SEQ.                   
02922      ADD +60                     TO W-ACTV-SEQ.                   
02923                                                                   
02924      IF  W-ACTV-SEQ EQUAL +60                                     
02925          GO TO 7200-NOT-FOUND.                                    
02926                                                                   
02927      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02928                                                                   
02929      EXEC CICS HANDLE CONDITION                                   
02930           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
02931           NOTFND     (7200-NOT-FOUND)                             
02932           END-EXEC.                                               
02933                                                                   
02934      EXEC CICS READ                                               
02935           DATASET    (W-ACTV-ID)                                  
02936           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
02937           RIDFLD     (W-ACTV-KEY)                                 
02938           END-EXEC.                                               
02939                                                                   
02940 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
02941                                                                   
02942      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
02943      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
02944      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
02945      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
02946      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
02947                                                                   
02948      MOVE SPACES                 TO W-ZIP-CODE.                   
02949                                                                   
02950      IF  AT-CANADIAN-POST-CODE                                    
02951          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
02952          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
02953                                                                   
02954      ELSE                                                         
02955          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
02956                                                                   
02957          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
02958              MOVE '-'            TO W-AM-ZIP-DASH                 
02959              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
02960                                                                   
02961      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
02962                                                                   
02963      MOVE ZEROS                  TO W-PHONE-IN.                   
02964      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
02965      MOVE W-PI-AREA              TO W-PO-AREA.                    
02966      MOVE W-PI-PFX               TO W-PO-PFX.                     
02967      MOVE W-PI-SFX               TO W-PO-SFX.                     
02968      MOVE W-PHONE-OUT            TO SS50-5D.                      
02969                                                                   
02970      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
02971                                                                   
02972      MOVE W-LABEL-LINES (01)     TO SS50D.                        
02973      MOVE W-LABEL-LINES (02)     TO SS50-1D.                      
02974      MOVE W-LABEL-LINES (03)     TO SS50-2D.                      
02975      MOVE W-LABEL-LINES (04)     TO SS50-3D.                      
02976      MOVE W-LABEL-LINES (05)     TO SS50-4D.                      
02977                                  EJECT                            
02978  7200-NOT-FOUND.                                                  
02979                                                                   
02980      IF  ACCOUNT-IS-ONLINE                                        
02981          GO TO 7220-READ-ACCOUNT.                                 
02982                                                                   
02983      MOVE CL-ACCOUNT-ADDR-CNT    TO W-ACTV-SEQ.                   
02984      ADD +20                     TO W-ACTV-SEQ.                   
02985                                                                   
02986      IF  W-ACTV-SEQ EQUAL +20                                     
02987          GO TO 7220-READ-ACCOUNT.                                 
02988                                                                   
02989      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
02990                                                                   
02991      EXEC CICS HANDLE CONDITION                                   
02992           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
02993           NOTFND     (7220-READ-ACCOUNT)                          
02994           END-EXEC.                                               
02995                                                                   
02996      EXEC CICS READ                                               
02997           DATASET    (W-ACTV-ID)                                  
02998           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
02999           RIDFLD     (W-ACTV-KEY)                                 
03000           END-EXEC.                                               
03001                                                                   
03002 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
03003                                                                   
03004      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03005      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
03006      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
03007      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
03008      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
03009                                                                   
03010      MOVE SPACES                 TO W-ZIP-CODE.                   
03011                                                                   
03012      IF  AT-CANADIAN-POST-CODE                                    
03013          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
03014          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
03015                                                                   
03016      ELSE                                                         
03017          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
03018                                                                   
03019          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
03020              MOVE '-'            TO W-AM-ZIP-DASH                 
03021              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
03022                                                                   
03023      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
03024                                                                   
03025      MOVE ZEROS                  TO W-PHONE-IN.                   
03026      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
03027      MOVE W-PI-AREA              TO W-PO-AREA.                    
03028      MOVE W-PI-PFX               TO W-PO-PFX.                     
03029      MOVE W-PI-SFX               TO W-PO-SFX.                     
03030      MOVE W-PHONE-OUT            TO SS07-6D.                      
03031                                                                   
03032      IF  PI-COMPANY-ID EQUAL     TO 'FLA'                         
03033          NEXT SENTENCE                                            
03034                                                                   
03035      ELSE                                                         
03036          PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                  
03037                                                                   
03038      MOVE W-LABEL-LINES (01)     TO SS06D.                        
03039      MOVE W-LABEL-LINES (02)     TO SS07-1D.                      
03040      MOVE W-LABEL-LINES (03)     TO SS07-2D.                      
03041      MOVE W-LABEL-LINES (04)     TO SS07-3D.                      
03042      MOVE W-LABEL-LINES (05)     TO SS07-4D.                      
03043      MOVE W-LABEL-LINES (06)     TO SS07-5D.                      
03044                                  EJECT                            
03045  7220-READ-ACCOUNT.                                               
03046                                                                   
03047      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     
03048          GO TO 7230-READ-PRODUCER.                                
03049                                                                   
03050      IF  W-ACCT-READ-SW EQUAL 'Y'                                 
03051          GO TO 7228-BUILD-ACCT-ADDR.                              
03052                                                                   
03053      MOVE CM-CERT-EFF-DT         TO W-ACCT-EXP-DATE.              
03054                                                                   
03055      EXEC CICS HANDLE CONDITION                                   
03056           NOTOPEN   (7375-ACCT-NOT-OPEN)                          
03057           NOTFND    (7240-READ-3RD-PARTY)                         
03058           END-EXEC.                                               
03059                                                                   
03060  7222-STARTBR-ACCOUNT.                                            
03061                                                                   
03062      MOVE W-ACCT-PARTIAL-KEY     TO W-ACCT-SAVE-KEY.              
03063      PERFORM 8000-STARTBR-ERACCT THRU 8000-EXIT.                  
03064                                                                   
03065  7226-READNEXT-ACCOUNT.                                           
03066                                                                   
03067      PERFORM 8010-READNEXT-ERACCT THRU 8010-EXIT.                 
03068                                                                   
03069      IF  W-ACCT-PARTIAL-KEY IS NOT EQUAL W-ACCT-SAVE-KEY          
03070                                                                   
03071          IF  W-SAVE-ACCT-RECORD EQUAL SPACES                      
03072              GO TO 7240-READ-3RD-PARTY                            
03073                                                                   
03074          ELSE                                                     
03075              MOVE AM-CONTROL-PRIMARY  TO W-ACCT-KEY               
03076              MOVE W-SAVE-ACCT-RECORD TO ACCOUNT-MASTER            
03077              GO TO 7228-BUILD-ACCT-ADDR.                          
03078                                                                   
03079      IF  AM-EXPIRATION-DT EQUAL HIGH-VALUES                       
03080          MOVE AM-CONTROL-PRIMARY TO W-ACCT-KEY                    
03081                                                                   
03082      ELSE                                                         
03083          MOVE ACCOUNT-MASTER     TO W-SAVE-ACCT-RECORD            
03084          GO TO 7226-READNEXT-ACCOUNT.                             
03085                                                                   
03086  7228-BUILD-ACCT-ADDR.                                            
03087                                                                   
03088      MOVE SPACES                 TO W-SAVE-ACCT-RECORD.           
03089                                                                   
03090      IF  NOT ACCOUNT-IS-ONLINE                                    
03091          GO TO 7240-READ-3RD-PARTY.                               
03092                                                                   
03093      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03094      MOVE AM-NAME                TO W-LABEL-LINES (01).           
03095      MOVE AM-PERSON              TO W-LABEL-LINES (02).           
03096      MOVE AM-ADDRS               TO W-LABEL-LINES (03).           
03097      MOVE AM-CITY                TO W-LABEL-LINES (04).           
03098                                                                   
03099      MOVE SPACES                 TO W-ZIP-CODE.                   
03100                                                                   
03101      IF  AM-CANADIAN-POST-CODE                                    
03102          MOVE AM-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
03103          MOVE AM-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
03104                                                                   
03105      ELSE                                                         
03106          MOVE AM-ZIP-PRIME       TO W-AM-ZIP-CODE                 
03107                                                                   
03108          IF  AM-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
03109              MOVE '-'            TO W-AM-ZIP-DASH                 
03110              MOVE AM-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
03111                                                                   
03112      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
03113                                                                   
03114      MOVE ZEROS                  TO W-PHONE-IN.                   
03115      MOVE AM-AREA-CODE           TO W-PO-AREA.                    
03116      MOVE AM-TEL-PRE             TO W-PO-PFX.                     
03117      MOVE AM-TEL-NBR             TO W-PO-SFX.                     
03118      MOVE W-PHONE-OUT            TO SS07-6D.                      
03119                                                                   
03120      IF  PI-COMPANY-ID EQUAL     TO 'FLA'                         
03121          NEXT SENTENCE                                            
03122                                                                   
03123      ELSE                                                         
03124          PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                  
03125                                                                   
03126      MOVE W-LABEL-LINES (01)     TO SS06D.                        
03127      MOVE W-LABEL-LINES (02)     TO SS07-1D.                      
03128      MOVE W-LABEL-LINES (03)     TO SS07-2D.                      
03129      MOVE W-LABEL-LINES (04)     TO SS07-3D.                      
03130      MOVE W-LABEL-LINES (05)     TO SS07-4D.                      
03131      MOVE W-LABEL-LINES (06)     TO SS07-5D.                      
03132                                                                   
03133      GO TO 7240-READ-3RD-PARTY.                                   
03134                                  EJECT                            
03135  7230-READ-PRODUCER.                                              
03136                                                                   
03137      IF W-PROD-READ-SW IS EQUAL TO 'Y'                            
03138          GO TO 7238-BUILD-PROD-ADDR.                              
03139                                                                   
03140      MOVE PM-POLICY-EFF-DT       TO W-PROD-EXP-DATE.              
03141                                                                   
03142      EXEC CICS HANDLE CONDITION                                   
03143           NOTOPEN   (7395-PROD-NOT-OPEN)                          
03144           NOTFND    (7300-READ-DENIAL)                            
03145      END-EXEC.                                                    
03146                                                                   
03147  7232-STARTBR-PRODUCER.                                           
03148                                                                   
03149      MOVE W-PROD-PARTIAL-KEY     TO W-PROD-SAVE-KEY.              
03150      PERFORM 8050-STARTBR-EMPROD THRU 8050-EXIT.                  
03151                                                                   
03152  7236-READNEXT-PRODUCER.                                          
03153                                                                   
03154      PERFORM 8060-READNEXT-EMPROD THRU 8060-EXIT.                 
03155                                                                   
03156      IF W-PROD-PARTIAL-KEY IS NOT EQUAL W-PROD-SAVE-KEY           
03157          IF W-SAVE-PROD-RECORD IS EQUAL TO SPACES                 
03158              GO TO 7300-READ-DENIAL                               
03159          ELSE                                                     
03160              MOVE PD-CONTROL-PRIMARY TO W-PROD-KEY                
03161              MOVE W-SAVE-PROD-RECORD TO PRODUCER-MASTER           
03162              GO TO 7238-BUILD-PROD-ADDR.                          
03163                                                                   
03164      IF PD-EXPIRE-DATE IS EQUAL TO HIGH-VALUES                    
03165          MOVE PD-CONTROL-PRIMARY TO W-PROD-KEY                    
03166      ELSE                                                         
03167          MOVE PRODUCER-MASTER    TO W-SAVE-PROD-RECORD            
03168          GO TO 7236-READNEXT-PRODUCER.                            
03169                                                                   
03170  7238-BUILD-PROD-ADDR.                                            
03171                                                                   
03172      MOVE SPACES                 TO W-SAVE-PROD-RECORD.           
03173                                                                   
03174      IF NOT ACCOUNT-IS-ONLINE                                     
03175          GO TO 7300-READ-DENIAL.                                  
03176                                                                   
03177      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03178      MOVE PD-NAME                TO W-LABEL-LINES (01).           
03179      MOVE PD-PERSON              TO W-LABEL-LINES (02).           
03180      MOVE PD-ADDRS               TO W-LABEL-LINES (03).           
03181      MOVE PD-CITY                TO W-LABEL-LINES (04).           
03182                                                                   
03183      MOVE SPACES                 TO W-ZIP-CODE.                   
03184                                                                   
03185      MOVE PD-ZIP-PRIME           TO W-AM-ZIP-CODE.                
03186      IF (PD-ZIP-PLUS4 IS NOT EQUAL TO SPACES AND ZEROS)           
03187          MOVE '-'                TO W-AM-ZIP-DASH                 
03188          MOVE PD-ZIP-PLUS4       TO W-AM-ZIP-PLUS4.               
03189                                                                   
03190      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
03191                                                                   
03192      MOVE ZEROS                  TO W-PHONE-IN.                   
03193      MOVE PD-AREA-CODE           TO W-PO-AREA.                    
03194      MOVE PD-TEL-PRE             TO W-PO-PFX.                     
03195      MOVE PD-TEL-NBR             TO W-PO-SFX.                     
03196      MOVE W-PHONE-OUT            TO SS07-6D.                      
03197                                                                   
03198      IF PI-COMPANY-ID IS EQUAL TO 'FLA'                           
03199          NEXT SENTENCE                                            
03200      ELSE                                                         
03201          PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                  
03202                                                                   
03203      MOVE W-LABEL-LINES (01)     TO SS06D.                        
03204      MOVE W-LABEL-LINES (02)     TO SS07-1D.                      
03205      MOVE W-LABEL-LINES (03)     TO SS07-2D.                      
03206      MOVE W-LABEL-LINES (04)     TO SS07-3D.                      
03207      MOVE W-LABEL-LINES (05)     TO SS07-4D.                      
03208      MOVE W-LABEL-LINES (06)     TO SS07-5D.                      
03209                                                                   
03210      GO TO 7300-READ-DENIAL.                                      
03211                                  EJECT                            
03212  7240-READ-3RD-PARTY.                                             
03213                                                                   
03214      MOVE +29                    TO W-ACTV-SEQ.                   
03215                                                                   
03216      EXEC CICS HANDLE CONDITION                                   
03217           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
03218           NOTFND     (7260-READ-COMP)                             
03219      END-EXEC.                                                    
03220                                                                   
03221      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
03222                                                                   
03223      EXEC CICS READ                                               
03224           DATASET    (W-ACTV-ID)                                  
03225           SET        (ADDRESS OF ACTIVITY-TRAILERS)               
03226           RIDFLD     (W-ACTV-KEY)                                 
03227      END-EXEC.                                                    
03228                                                                   
03229 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
03230                                                                   
03231      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03232      MOVE AT-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
03233      MOVE AT-ADDRESS-LINE-1      TO W-LABEL-LINES (02).           
03234      MOVE AT-ADDRESS-LINE-2      TO W-LABEL-LINES (03).           
03235      MOVE AT-CITY-STATE          TO W-LABEL-LINES (04).           
03236                                                                   
03237      MOVE SPACES                 TO W-ZIP-CODE.                   
03238                                                                   
03239      IF  AT-CANADIAN-POST-CODE                                    
03240          MOVE AT-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
03241          MOVE AT-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
03242                                                                   
03243      ELSE                                                         
03244          MOVE AT-ZIP-CODE        TO W-AM-ZIP-CODE                 
03245                                                                   
03246          IF  AT-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
03247              MOVE '-'            TO W-AM-ZIP-DASH                 
03248              MOVE AT-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
03249                                                                   
03250      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
03251                                                                   
03252      MOVE ZEROS                  TO W-PHONE-IN.                   
03253      MOVE AT-PHONE-NO            TO W-PHONE-IN.                   
03254      MOVE W-PI-AREA              TO W-PO-AREA.                    
03255      MOVE W-PI-PFX               TO W-PO-PFX.                     
03256      MOVE W-PI-SFX               TO W-PO-SFX.                     
03257      MOVE W-PHONE-OUT            TO SS53-6D.                      
03258                                                                   
03259      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
03260                                                                   
03261      MOVE W-LABEL-LINES (01)     TO SS52D.                        
03262      MOVE W-LABEL-LINES (02)     TO SS53-1D.                      
03263      MOVE W-LABEL-LINES (03)     TO SS53-2D.                      
03264      MOVE W-LABEL-LINES (04)     TO SS53-3D.                      
03265      MOVE W-LABEL-LINES (05)     TO SS53-4D.                      
03266      MOVE W-LABEL-LINES (06)     TO SS53-5D.                      
03267                                                                   
03268      GO TO 7300-READ-DENIAL.                                      
03269                                  EJECT                            
03270  7260-READ-COMP.                                                  
03271                                                                   
03272      IF  W-COMP-READ-SW EQUAL 'Y'                                 
03273          GO TO 7280-BUILD-COMP-ADDR.                              
03274                                                                   
03275      IF  AM-3RD-PARTY-NOTIF-LEVEL NOT NUMERIC                     
03276          MOVE ZEROS              TO AM-3RD-PARTY-NOTIF-LEVEL      
03277          GO TO 7300-READ-DENIAL.                                  
03278                                                                   
03279      IF  AM-3RD-PARTY-NOTIF-LEVEL GREATER THAN 00 AND             
03280              LESS THAN 11                                         
03281          NEXT SENTENCE                                            
03282                                                                   
03283      ELSE                                                         
03284          GO TO 7300-READ-DENIAL.                                  
03285                                                                   
03286      IF  AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL) EQUAL SPACES OR ZEROS  
03287          GO TO 7300-READ-DENIAL.                                  
03288                                                                   
03289      MOVE PI-COMPANY-CD          TO W-COMP-COMPANY-CD             
03290      MOVE AM-CARRIER             TO W-COMP-CARRIER                
03291      MOVE AM-GROUPING            TO W-COMP-GROUPING               
03292      MOVE 'A'                    TO W-COMP-TYPE                   
03293      MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)                       
03294                                  TO W-COMP-RESP-NO.               
03295                                                                   
03296      IF  AM-3RD-PARTY-NOTIF-LEVEL EQUAL AM-REMIT-TO               
03297          IF AM-COM-TYP (AM-REMIT-TO) EQUAL 'O' OR 'P' OR          
052814                                           'G' OR 'B' or 'S'      
03299              MOVE 'G'            TO W-COMP-TYPE                   
03300              MOVE LOW-VALUES     TO W-COMP-ACCOUNT                
03301          ELSE                                                     
03302              MOVE AM-AGT (AM-3RD-PARTY-NOTIF-LEVEL)               
03303                                  TO W-COMP-ACCOUNT                
03304      ELSE                                                         
03305          MOVE 'G'                TO W-COMP-TYPE                   
03306          MOVE LOW-VALUES         TO W-COMP-ACCOUNT.               
03307                                                                   
03308      IF  PI-ZERO-CARRIER OR PI-ZERO-CAR-GROUP                     
03309          MOVE ZEROS              TO W-COMP-CARRIER.               
03310                                                                   
03311      IF  PI-ZERO-GROUPING OR PI-ZERO-CAR-GROUP                    
03312          MOVE ZEROS              TO W-COMP-GROUPING.              
03313                                                                   
03314      EXEC CICS HANDLE CONDITION                                   
03315           NOTFND    (7300-READ-DENIAL)                            
03316      END-EXEC.                                                    
03317                                                                   
03318      EXEC CICS  READ                                              
03319           SET      (ADDRESS OF COMPENSATION-MASTER)               
03320           DATASET  ('ERCOMP')                                     
03321           RIDFLD   (W-COMP-KEY)                                   
03322      END-EXEC.                                                    
03323                                                                   
03324 *    SERVICE RELOAD COMPENSATION-MASTER.                          
03325                                                                   
03326  7280-BUILD-COMP-ADDR.                                            
03327                                                                   
03328      MOVE SPACES                 TO W-LABEL-HOLD-AREA             
03329      MOVE CO-ACCT-NAME           TO W-LABEL-LINES (01)            
03330                                                                   
03331      IF  CO-ACCT-NAME EQUAL SPACES                                
03332          MOVE CO-MAIL-NAME       TO W-LABEL-LINES (01).           
03333                                                                   
03334      MOVE CO-ADDR-1              TO W-LABEL-LINES (02).           
03335      MOVE CO-ADDR-2              TO W-LABEL-LINES (03).           
03336      MOVE CO-ADDR-3              TO W-LABEL-LINES (04).           
03337                                                                   
03338      MOVE SPACES                 TO W-ZIP-CODE.                   
03339                                                                   
03340      IF  CO-CANADIAN-POST-CODE                                    
03341          MOVE CO-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
03342          MOVE CO-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
03343                                                                   
03344      ELSE                                                         
03345          MOVE CO-ZIP-PRIME       TO W-AM-ZIP-CODE                 
03346                                                                   
03347          IF  CO-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
03348              MOVE '-'            TO W-AM-ZIP-DASH                 
03349              MOVE CO-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
03350                                                                   
03351      MOVE W-ZIP-CODE             TO W-LABEL-LINES (05).           
03352                                                                   
03353      MOVE ZEROS                  TO W-PHONE-IN.                   
03354      MOVE CO-AREA-CODE           TO W-PO-AREA.                    
03355      MOVE CO-PREFIX              TO W-PO-PFX.                     
03356      MOVE CO-PHONE               TO W-PO-SFX.                     
03357      MOVE W-PHONE-OUT            TO SS53-6D.                      
03358                                                                   
03359      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
03360                                                                   
03361      MOVE W-LABEL-LINES (01)     TO SS52D.                        
03362      MOVE W-LABEL-LINES (02)     TO SS53-1D.                      
03363      MOVE W-LABEL-LINES (03)     TO SS53-2D.                      
03364      MOVE W-LABEL-LINES (04)     TO SS53-3D.                      
03365      MOVE W-LABEL-LINES (05)     TO SS53-4D.                      
03366      MOVE W-LABEL-LINES (06)     TO SS53-5D.                      
03367                                  EJECT                            
03368  7300-READ-DENIAL.                                                
03369                                                                   
03370      MOVE +90                    TO W-ACTV-SEQ.                   
03371                                                                   
03372      MOVE CL-CONTROL-PRIMARY     TO W-ACTV-PARTIAL-KEY.           
03373                                                                   
03374      EXEC CICS HANDLE CONDITION                                   
03375           NOTOPEN    (7390-ACTV-NOT-OPEN)                         
03376           NOTFND     (7340-READ-CNTL1)                            
03377           ENDFILE    (7340-READ-CNTL1)                            
03378           END-EXEC.                                               
03379                                                                   
03380      EXEC CICS STARTBR                                            
03381           DATASET    (W-ACTV-ID)                                  
03382           RIDFLD     (W-ACTV-KEY)                                 
03383           GTEQ                                                    
03384           END-EXEC.                                               
03385                                                                   
03386      MOVE 'Y'                    TO W-ACTV-BROWSE-STARTED.        
03387      MOVE W-ACTV-PARTIAL-KEY     TO W-ACTV-SAVE-KEY.              
03388                                                                   
03389  7304-READ-NEXT.                                                  
03390                                                                   
03391      EXEC CICS READNEXT                                           
03392           SET     (ADDRESS OF ACTIVITY-TRAILERS)                  
03393           RIDFLD  (W-ACTV-KEY)                                    
03394           DATASET (W-ACTV-ID)                                     
03395           END-EXEC.                                               
03396                                                                   
03397 *    SERVICE RELOAD ACTIVITY-TRAILERS.                            
03398                                                                   
03399      IF  W-ACTV-PARTIAL-KEY NOT = W-ACTV-SAVE-KEY                 
03400          GO TO 7340-READ-CNTL1.                                   
03401                                                                   
03402      IF  AT-TRAILER-TYPE NOT = '8'                                
03403          GO TO 7304-READ-NEXT.                                    
03404                                                                   
03405      MOVE AT-DENIAL-INFO-1       TO SS35-1D.                      
03406      MOVE AT-DENIAL-INFO-2       TO SS35-2D.                      
03407                                  EJECT                            
03408  7340-READ-CNTL1.                                                 
03409                                                                   
03410      IF  W-ACTV-BROWSE-STARTED = 'Y'                              
03411          MOVE 'N'                TO W-ACTV-BROWSE-STARTED         
03412          EXEC CICS ENDBR                                          
03413               DATASET    (W-ACTV-ID)                              
03414          END-EXEC.                                                
03415                                                                   
03416      IF  SS35-1D = ALL '*'                                        
03417          MOVE '@@DENIAL1'        TO SS35-1D.                      
03418                                                                   
03419      IF  SS35-2D = ALL '*'                                        
03420          MOVE '@@DENIAL2'        TO SS35-2D.                      
03421                                                                   
03422      MOVE '1'                    TO W-CNTL-RECORD-TYPE.           
03423      MOVE ZEROS                  TO W-CNTL-SEQ.                   
03424                                                                   
03425      EXEC CICS HANDLE CONDITION                                   
03426           NOTOPEN   (7385-CNTL-NOT-OPEN)                          
03427           NOTFND    (7340-READ-CNTL2)                             
03428      END-EXEC.                                                    
03429                                                                   
03430      EXEC CICS READ                                               
03431           DATASET   (W-CNTL-ID)                                   
03432           SET       (ADDRESS OF CONTROL-FILE)                     
03433           RIDFLD    (W-CNTL-KEY)                                  
03434      END-EXEC.                                                    
03435                                                                   
03436 *    SERVICE RELOAD CONTROL-FILE.                                 
03437                                                                   
03438      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03439      MOVE CF-CL-MAIL-TO-NAME     TO W-LABEL-LINES (01).           
03440      MOVE CF-CL-IN-CARE-OF       TO W-LABEL-LINES (02).           
03441      MOVE CF-CL-ADDR-LINE-1      TO W-LABEL-LINES (03).           
03442      MOVE CF-CL-ADDR-LINE-2      TO W-LABEL-LINES (04).           
03443      MOVE CF-CL-CITY-STATE       TO W-LABEL-LINES (05).           
03444                                                                   
03445      IF  CF-CL-ZIP-CODE-NUM NOT NUMERIC                           
03446          MOVE ZEROS              TO CF-CL-ZIP-CODE-NUM.           
03447                                                                   
03448      IF  CF-CL-ZIP-CODE-NUM NOT = ZEROS                           
03449          MOVE CF-CL-ZIP-CODE-NUM TO W-ZIP-NUMERIC                 
03450          MOVE W-ZIP-NONNUM       TO CF-CL-ZIP-CODE.               
03451                                                                   
03452      MOVE SPACES                 TO W-ZIP-CODE.                   
03453                                                                   
03454      IF  CF-CL-CAN-POST-CODE                                      
03455          MOVE CF-CL-CAN-POSTAL-1 TO W-CAN-POSTAL-1                
03456          MOVE CF-CL-CAN-POSTAL-2 TO W-CAN-POSTAL-2                
03457                                                                   
03458      ELSE                                                         
03459          MOVE CF-CL-ZIP-PRIME    TO W-AM-ZIP-CODE                 
03460                                                                   
03461          IF  CF-CL-ZIP-PLUS4 NOT = SPACES AND  ZEROS              
03462              MOVE '-'            TO W-AM-ZIP-DASH                 
03463              MOVE CF-CL-ZIP-PLUS4                                 
03464                                  TO W-AM-ZIP-PLUS4.               
03465                                                                   
03466      MOVE W-ZIP-CODE             TO W-LABEL-LINES (06).           
03467                                                                   
03468      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
03469                                                                   
03470      MOVE W-LABEL-LINES (01)     TO SS01D.                        
03471      MOVE W-LABEL-LINES (02)     TO SS02-1D.                      
03472      MOVE W-LABEL-LINES (03)     TO SS02-2D.                      
03473      MOVE W-LABEL-LINES (04)     TO SS02-3D.                      
03474      MOVE W-LABEL-LINES (05)     TO SS02-4D.                      
03475      MOVE W-LABEL-LINES (06)     TO SS02-5D.                      
03476                                                                   
03477  7340-READ-CNTL2.                                                 
03478                                                                   
03479      IF  PI-PROCESSOR-ID =  'LGXX'                                
03480          GO TO 7350-READ-CNTL4.                                   
03481                                                                   
03482      MOVE '2'                    TO W-CNTL-RECORD-TYPE.           
03483      MOVE PI-PROCESSOR-ID        TO W-CNTL-GENL.                  
03484      MOVE ZEROS                  TO W-CNTL-SEQ.                   
03485                                                                   
03486      EXEC CICS HANDLE CONDITION                                   
03487           NOTOPEN    (7385-CNTL-NOT-OPEN)                         
03488           NOTFND     (7350-READ-CNTL4)                            
03489      END-EXEC.                                                    
03490                                                                   
03491      EXEC CICS READ                                               
03492           DATASET    (W-CNTL-ID)                                  
03493           SET        (ADDRESS OF CONTROL-FILE)                    
03494           RIDFLD     (W-CNTL-KEY)                                 
03495       END-EXEC.                                                   
03496                                                                   
03497 *    SERVICE RELOAD CONTROL-FILE.                                 
03498                                                                   
03499      MOVE CF-PROCESSOR-NAME      TO SS08D.                        
03500      MOVE CF-PROCESSOR-TITLE     TO SS09D.                        
03501                                  EJECT                            
03502  7350-READ-CNTL4.                                                 
03503                                                                   
03504      IF CL-SYSTEM-IDENTIFIER IS EQUAL TO 'CV'                     
03505          GO TO 7350-READ-EMPLAN.                                  
03506                                                                   
03507      IF  W-BEN-HOLD = ZEROS                                       
03508          GO TO 7350-READ-CNTL6.                                   
03509                                                                   
03510      MOVE W-BEN-HOLD             TO W-CNTL-GEN2.                  
03511      MOVE SPACES                 TO W-CNTL-GEN1.                  
03512                                                                   
100518     IF  CL-CLAIM-TYPE = PI-LIFE-OVERRIDE-L1 OR 'O'
03514          MOVE '4'                TO W-CNTL-RECORD-TYPE            
03515                                                                   
03516      ELSE                                                         
03517          MOVE '5'                TO W-CNTL-RECORD-TYPE.           
03518                                                                   
03519      MOVE ZEROS                  TO W-CNTL-SEQ.                   
03520                                                                   
03521      EXEC CICS HANDLE CONDITION                                   
03522           NOTOPEN    (7385-CNTL-NOT-OPEN)                         
03523           NOTFND     (7350-READ-CNTL6)                            
03524      END-EXEC.                                                    
03525                                                                   
03526      EXEC CICS READ                                               
03527           DATASET    (W-CNTL-ID)                                  
03528           SET        (ADDRESS OF CONTROL-FILE)                    
03529           RIDFLD     (W-CNTL-KEY)                                 
03530           GTEQ                                                    
03531       END-EXEC.                                                   
03532                                                                   
03533 *    SERVICE RELOAD CONTROL-FILE.                                 
03534                                                                   
03535      MOVE 1                      TO W-SUB.                        
03536                                                                   
03537  7350-LOOP.                                                       
03538                                                                   
03539      IF  W-SUB = 9                                                
03540          GO TO 7350-READ-CNTL6.                                   
03541                                                                   
03542      IF  CF-BENEFIT-CODE (W-SUB) LESS THAN W-BEN-HOLD             
03543          ADD 1                   TO W-SUB                         
03544          GO TO 7350-LOOP.                                         
03545                                                                   
03546      IF  W-BEN-HOLD = CF-BENEFIT-CODE (W-SUB)                     
03547          MOVE CF-BENEFIT-DESCRIP (W-SUB) TO SS22D                 
03548                                                                   
121802         IF  CL-CLAIM-TYPE = PI-AH-OVERRIDE-L1 OR 'I' OR 'G'
080322                                               OR 'B' OR 'H'
03550              MOVE CF-BENEFIT-ALPHA (W-SUB)                        
03551                                  TO W-BENEFIT-WORK                
03552              MOVE W-ELIM-DAYS    TO SS42D.                        
03553                                                                   
03554      GO TO 7350-READ-CNTL6.                                       
03555                                  EJECT                            
03556  7350-READ-EMPLAN.                                                
03557                                                                   
03558      IF W-BEN-HOLD IS EQUAL TO ZEROS                              
03559          GO TO 7350-READ-CNTL6.                                   
03560                                                                   
03561      MOVE PM-GROUPING            TO W-PLAN-GROUPING.              
03562      MOVE PM-STATE               TO W-PLAN-STATE.                 
03563      MOVE PM-PRODUCER            TO W-PLAN-PRODUCER.              
03564      MOVE PM-INS-PLAN-CD         TO W-PLAN-CODE.                  
03565      MOVE PM-INS-PLAN-REVISION   TO W-PLAN-REV-NO.                
03566                                                                   
03567      EXEC CICS HANDLE CONDITION                                   
03568           NOTOPEN  (7397-PLAN-NOT-OPEN)                           
03569           NOTFND   (7350-READ-CNTL6)                              
03570      END-EXEC.                                                    
03571                                                                   
03572      EXEC CICS READ                                               
03573           DATASET   (W-PLAN-ID)                                   
03574           SET       (ADDRESS OF PRODUCER-PLANS)                   
03575           RIDFLD    (W-PLAN-KEY)                                  
03576      END-EXEC.                                                    
03577                                                                   
03578 *    SERVICE RELOAD PRODUCER-PLANS.                               
03579                                                                   
03580      MOVE PP-PLAN-DESCRIPTION    TO SS22D.                        
03581                                  EJECT                            
03582  7350-READ-CNTL6.                                                 
03583                                                                   
03584      MOVE '6'                    TO W-CNTL-RECORD-TYPE.           
03585      MOVE SPACES                 TO W-CNTL-GENL.                  
03586                                                                   
03587      IF  PI-COMPANY-ID EQUAL 'AIG' OR 'AUK'                       
03588          MOVE CL-CURRENT-CARRIER TO W-CNTL-GEN4                   
03589                                                                   
03590      ELSE                                                         
03591          MOVE PI-CARRIER         TO W-CNTL-GEN4.                  
03592                                                                   
03593      MOVE ZEROS                  TO W-CNTL-SEQ.                   
03594                                                                   
03595      EXEC CICS HANDLE CONDITION                                   
03596           NOTOPEN    (7385-CNTL-NOT-OPEN)                         
03597           NOTFND     (7360-SET-DATE)                              
03598      END-EXEC.                                                    
03599                                                                   
03600      EXEC CICS READ                                               
03601           DATASET    (W-CNTL-ID)                                  
03602           SET        (ADDRESS OF CONTROL-FILE)                    
03603           RIDFLD     (W-CNTL-KEY)                                 
03604      END-EXEC.                                                    
03605                                                                   
03606 *    SERVICE RELOAD CONTROL-FILE.                                 
03607                                                                   
03608      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03609      MOVE CF-MAIL-TO-NAME        TO W-LABEL-LINES (01).           
03610      MOVE CF-IN-CARE-OF          TO W-LABEL-LINES (02).           
03611      MOVE CF-ADDRESS-LINE-1      TO W-LABEL-LINES (03).           
03612      MOVE CF-ADDRESS-LINE-2      TO W-LABEL-LINES (04).           
03613      MOVE CF-CITY-STATE          TO W-LABEL-LINES (05).           
03614                                                                   
03615      IF  CF-ZIP-CODE-NUM NOT NUMERIC                              
03616          MOVE ZEROS              TO CF-ZIP-CODE-NUM.              
03617                                                                   
03618      IF  CF-ZIP-CODE-NUM NOT = ZEROS                              
03619          MOVE CF-ZIP-CODE-NUM    TO W-ZIP-NUMERIC                 
03620          MOVE W-ZIP-NONNUM       TO CF-ZIP-CODE.                  
03621                                                                   
03622      MOVE SPACES                 TO W-ZIP-CODE.                   
03623                                                                   
03624      IF  CF-CANADIAN-POST-CODE                                    
03625          MOVE CF-CAN-POSTAL-1    TO W-CAN-POSTAL-1                
03626          MOVE CF-CAN-POSTAL-2    TO W-CAN-POSTAL-2                
03627                                                                   
03628      ELSE                                                         
03629          MOVE CF-ZIP-PRIME       TO W-AM-ZIP-CODE                 
03630                                                                   
03631          IF  CF-ZIP-PLUS4 NOT = SPACES AND  ZEROS                 
03632              MOVE '-'            TO W-AM-ZIP-DASH                 
03633              MOVE CF-ZIP-PLUS4   TO W-AM-ZIP-PLUS4.               
03634                                                                   
03635      MOVE W-ZIP-CODE             TO W-LABEL-LINES (06).           
03636                                                                   
03637      MOVE ZEROS                  TO W-PHONE-IN.                   
03638      MOVE CF-PHONE-NO            TO W-PHONE-IN.                   
03639      MOVE W-PI-AREA              TO W-PO-AREA.                    
03640      MOVE W-PI-PFX               TO W-PO-PFX.                     
03641      MOVE W-PI-SFX               TO W-PO-SFX.                     
03642      MOVE W-PHONE-OUT            TO SS04-6D.                      
03643                                                                   
03644      PERFORM 7600-LABEL-MOVE THRU 7600-EXIT.                      
03645                                                                   
03646      MOVE W-LABEL-LINES (01)     TO SS03D.                        
031307     IF PI-CARRIER = '8'
031307        MOVE 'as Administrator for Investors Heritage Life Insuran
031307-     'ce Company'               TO SS03-1D
031307     END-IF
03647      MOVE W-LABEL-LINES (02)     TO SS04-1D.                      
03648      MOVE W-LABEL-LINES (03)     TO SS04-2D.                      
03649      MOVE W-LABEL-LINES (04)     TO SS04-3D.                      
03650      MOVE W-LABEL-LINES (05)     TO SS04-4D.                      
03651      MOVE W-LABEL-LINES (06)     TO SS04-5D.                      
03652                                  EJECT                            
03653  7360-SET-DATE.                                                   
03654                                                                   
03655      MOVE EIBDATE                TO W-DATE-WORK.                  
03656      MOVE W-DT-WORK              TO DC-JULIAN-YYDDD.              
03657      MOVE '5'                    TO DC-OPTION-CODE.               
03658      PERFORM 9700-DATE-LINK THRU 9700-EXIT.                       
03659      MOVE DC-GREG-DATE-1-EDIT    TO SS20D.                        
03660      MOVE DC-GREG-DATE-1-ALPHA   TO SS21D.                        
03661                                                                   
03662      IF  LOWER-CASE-LETTERS-USED                                  
03663          NEXT SENTENCE                                            
03664                                                                   
03665      ELSE                                                         
03666          GO TO 7399-EXIT.                                         
03667                                                                   
03668      INSPECT SS19D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE.   
03669      INSPECT SS35-1D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE.   
03670      INSPECT SS35-2D   CONVERTING W-UPPER-CASE TO W-LOWER-CASE.   
03671      INSPECT SS22D     CONVERTING W-UPPER-CASE TO W-LOWER-CASE.   
03672                                                                   
03673      MOVE SS08D                  TO W-TEMP-AREA2                  
03674      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT             
03675      MOVE W-TEMP-AREA2           TO SS08D                         
03676                                                                   
03677      MOVE SS09D                  TO W-TEMP-AREA2                  
03678      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT             
03679      MOVE W-TEMP-AREA2           TO SS09D                         
03680                                                                   
03681      MOVE SS11-5D                TO W-TEMP-AREA2                  
03682      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT             
03683      MOVE W-TEMP-AREA2           TO SS11-5D                       
03684                                                                   
03685      MOVE SS21D                  TO W-TEMP-AREA2                  
03686      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT             
03687      MOVE W-TEMP-AREA2           TO SS21D                         
03688                                                                   
03689      MOVE SS40D                  TO W-TEMP-AREA2                  
03690      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT             
03691      MOVE W-TEMP-AREA2           TO SS40D                         
03692                                                                   
03693      MOVE SS41D                  TO W-TEMP-AREA2                  
03694      PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT             
03695      MOVE W-TEMP-AREA2           TO SS41D.                        
03696      GO TO 7399-EXIT.                                             
03697                                  EJECT                            
03698  7370-RESOLVE-CREDITOR.                                           
03699                                                                   
03700      EXEC CICS HANDLE CONDITION                                   
03701           NOTOPEN    (7380-BENE-NOT-OPEN)                         
03702           NOTFND     (7370-EXIT)                                  
03703      END-EXEC.                                                    
03704                                                                   
03705      MOVE SPACES                 TO W-LABEL-HOLD-AREA.            
03706                                                                   
03707      MOVE PI-COMPANY-CD          TO W-BENE-COMP-CD.               
03708      MOVE 'B'                    TO W-BENE-REC-TYPE.              
03709      MOVE SPACES                 TO W-BENE-NUMBER.                
03710      MOVE CL-CURRENT-GROUPING    TO W-BENE-CREDITOR.              
03711                                                                   
03712      EXEC CICS READ                                               
03713           DATASET    (W-BENE-ID)                                  
03714           SET        (ADDRESS OF BENEFICIARY-MASTER)              
03715           RIDFLD     (W-BENE-KEY)                                 
03716      END-EXEC.                                                    
03717                                                                   
03718 *    SERVICE RELOAD BENEFICIARY-MASTER.                           
03719                                                                   
03720      IF  LOWER-CASE-LETTERS-USED                                  
03721          MOVE BE-MAIL-TO-NAME    TO W-TEMP-AREA2                  
03722          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
03723          MOVE W-TEMP-AREA2       TO SS56D                         
03724                                                                   
03725      ELSE                                                         
03726          MOVE BE-MAIL-TO-NAME    TO SS56D.                        
03727                                                                   
03728  7370-EXIT.                                                       
03729      EXIT.                                                        
03730                                                                   
03731  7375-ACCT-NOT-OPEN.                                              
03732                                                                   
03733      MOVE ER-0168                TO W-CURRENT-ERROR.              
03734      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
03735      GO TO 7399-EXIT.                                             
03736                                                                   
03737  7380-BENE-NOT-OPEN.                                              
03738                                                                   
03739      MOVE ER-7675                TO W-CURRENT-ERROR.              
03740      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
03741      GO TO 7399-EXIT.                                             
03742                                                                   
03743                                                                   
03744  7385-CNTL-NOT-OPEN.                                              
03745                                                                   
03746      MOVE ER-0042                TO W-CURRENT-ERROR.              
03747      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
03748      GO TO 7399-EXIT.                                             
03749                                                                   
03750  7390-ACTV-NOT-OPEN.                                              
03751                                                                   
03752      MOVE ER-0172                TO W-CURRENT-ERROR.              
03753      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
03754      GO TO 7399-EXIT.                                             
03755                                                                   
03756  7395-PROD-NOT-OPEN.                                              
03757                                                                   
03758      MOVE ER-9106                TO W-CURRENT-ERROR.              
03759      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
03760      GO TO 7399-EXIT.                                             
03761                                                                   
03762  7397-PLAN-NOT-OPEN.                                              
03763                                                                   
03764      MOVE ER-9808                TO W-CURRENT-ERROR.              
03765      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
03766      GO TO 7399-EXIT.                                             
03767                                                                   
03768  7399-EXIT.                                                       
03769      EXIT.                                                        
03770                                  EJECT                            
03771  7400-MOVE-NAME.                                                  
03772                                                                   
03773      MOVE SPACES                 TO W-NAME-WORK                   
03774                                     W-NAME-WORK2.                 
03775      MOVE ZERO                   TO W-NAME-SW.                    
03776      SET W-NWA-NDX               TO +1.                           
03777                                                                   
03778      IF  CL-INSURED-1ST-NAME = SPACES                             
03779              AND                                                  
03780          CL-INSURED-MID-INIT = SPACES                             
03781          MOVE +1                 TO W-NAME-SW.                    
03782                                                                   
03783      MOVE CL-INSURED-LAST-NAME   TO W-NAME-WORK2.                 
03784      PERFORM 7420-MOVE-NAME THRU 7429-EXIT.                       
03785                                                                   
03786      MOVE CL-INSURED-1ST-NAME    TO W-NAME-WORK2.                 
03787      PERFORM 7420-MOVE-NAME THRU 7429-EXIT.                       
03788                                                                   
03789      SET W-NWA-NDX UP BY +1.                                      
03790      MOVE CL-INSURED-MID-INIT    TO W-NAME-WORK2.                 
03791      PERFORM 7420-MOVE-NAME THRU 7429-EXIT.                       
03792                                                                   
03793  7400-EXIT.                                                       
03794      EXIT.                                                        
03795                                  EJECT                            
03796  7420-MOVE-NAME SECTION.                                          
03797                                                                   
03798      IF  W-NAME-SW GREATER THAN +1                                
03799          GO TO 7429-EXIT.                                         
03800                                                                   
03801      IF  W-NAME-WORK2 = SPACES                                    
03802          GO TO 7429-EXIT.                                         
03803                                                                   
03804      SET W-NWA-NDX2              TO +1.                           
03805      SET W-NWA-NDX3              TO +2.                           
03806                                                                   
03807  7422-MOVE-NAME.                                                  
03808                                                                   
03809      MOVE W-NW2 (W-NWA-NDX2)   TO W-NW (W-NWA-NDX).               
03810                                                                   
03811      IF  W-NWA-NDX LESS THAN +30                                  
03812          SET W-NWA-NDX UP BY +1                                   
03813                                                                   
03814      ELSE                                                         
03815          ADD +2                  TO W-NAME-SW                     
03816          GO TO 7429-EXIT.                                         
03817                                                                   
03818      IF  W-NWA-NDX2 LESS THAN +20                                 
03819          SET W-NWA-NDX3 UP BY +1                                  
03820          SET W-NWA-NDX2 UP BY +1.                                 
03821                                                                   
03822      IF  W-NW2 (W-NWA-NDX2) = SPACES                              
03823              AND                                                  
03824          W-NW2 (W-NWA-NDX3) = SPACES                              
03825                                                                   
03826          IF  W-NAME-SW = ZERO                                     
03827              MOVE ','            TO W-NW (W-NWA-NDX)              
03828              SET W-NWA-NDX UP BY +2                               
03829              MOVE +1             TO W-NAME-SW                     
03830              GO TO 7429-EXIT                                      
03831                                                                   
03832          ELSE                                                     
03833              GO TO 7429-EXIT.                                     
03834                                                                   
03835      GO TO 7422-MOVE-NAME.                                        
03836                                                                   
03837  7429-EXIT.                                                       
03838      EXIT.                                                        
03839                                  EJECT                            
03840  7500-MOVE-NAME.                                                  
03841                                                                   
03842      MOVE SPACES                 TO W-NAME-WORK                   
03843                                     W-NAME-WORK2.                 
03844      MOVE ZERO                   TO W-NAME-SW.                    
03845      SET W-NWA-NDX               TO +1.                           
03846                                                                   
03847      IF  W-FIRST-NAME = SPACES                                    
03848              AND                                                  
03849          W-MIDDLE-NAME = SPACES                                   
03850          MOVE W-LAST-NAME                                         
03851                                  TO W-NAME-WORK                   
03852          GO TO 7500-EXIT.                                         
03853                                                                   
03854      MOVE W-FIRST-NAME           TO W-NAME-WORK2.                 
03855      PERFORM 7520-MOVE-NAME THRU 7529-EXIT.                       
03856                                                                   
03857      SET W-NWA-NDX UP BY +1.                                      
03858                                                                   
03859      IF  W-MIDDLE-NAME NOT = SPACES                               
03860          MOVE W-MIDDLE-NAME      TO W-NW (W-NWA-NDX)              
03861          SET W-NWA-NDX UP BY +1                                   
03862          MOVE '.'                TO W-NW (W-NWA-NDX)              
03863          SET W-NWA-NDX UP BY +2.                                  
03864                                                                   
03865      MOVE W-LAST-NAME            TO W-NAME-WORK2.                 
03866      PERFORM 7520-MOVE-NAME THRU 7529-EXIT.                       
03867                                                                   
03868  7500-EXIT.                                                       
03869      EXIT.                                                        
03870                                  EJECT                            
03871  7520-MOVE-NAME SECTION.                                          
03872                                                                   
03873      IF  W-NAME-SW GREATER THAN +1                                
03874          GO TO 7529-EXIT.                                         
03875                                                                   
03876      IF  W-NAME-WORK2 = SPACES                                    
03877          GO TO 7529-EXIT.                                         
03878                                                                   
03879      SET W-NWA-NDX2              TO +1.                           
03880      SET W-NWA-NDX3              TO +2.                           
03881                                                                   
03882  7522-MOVE-NAME.                                                  
03883                                                                   
03884      MOVE W-NW2 (W-NWA-NDX2)     TO W-NW (W-NWA-NDX).             
03885                                                                   
03886      IF  W-NWA-NDX LESS THAN +30                                  
03887          SET W-NWA-NDX UP BY +1                                   
03888                                                                   
03889      ELSE                                                         
03890          ADD +2                 TO W-NAME-SW                      
03891          GO TO 7529-EXIT.                                         
03892                                                                   
03893      IF  W-NWA-NDX2 LESS THAN +20                                 
03894          SET W-NWA-NDX2 UP BY +1                                  
03895          SET W-NWA-NDX3 UP BY +1.                                 
03896                                                                   
03897      IF  W-NW2 (W-NWA-NDX2) = SPACES                              
03898              AND                                                  
03899          W-NW2 (W-NWA-NDX3) = SPACES                              
03900          GO TO 7529-EXIT.                                         
03901                                                                   
03902      GO TO 7522-MOVE-NAME.                                        
03903                                                                   
03904  7529-EXIT.                                                       
03905      EXIT.                                                        
03906                                  EJECT                            
03907  7600-LABEL-MOVE.                                                 
03908                                                                   
03909      IF  W-LABEL-HOLD-AREA = SPACES                               
03910          GO TO 7600-EXIT.                                         
03911                                                                   
03912      PERFORM 7700-TRANSLATE-LOWER THRU 7700-EXIT.                 
03913                                                                   
03914      IF  W-LABEL-LINES (1) = SPACES                               
03915          MOVE W-LABEL-LINES (2)  TO W-LABEL-LINES (1)             
03916          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)             
03917          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)             
03918          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)             
03919          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)             
03920          MOVE SPACES             TO W-LABEL-LINES (6)             
03921          GO TO 7600-LABEL-MOVE.                                   
03922                                                                   
03923      IF  W-LABEL-LINES (2) = SPACES AND                           
03924          W-LABEL-LINES (3) = SPACES AND                           
03925          W-LABEL-LINES (4) = SPACES AND                           
03926          W-LABEL-LINES (5) = SPACES AND                           
03927          W-LABEL-LINES (6) = SPACES                               
03928          SET W-NDX               TO 1                             
03929          GO TO 7600-MOVE-ZIP.                                     
03930                                                                   
03931      IF  W-LABEL-LINES (2) = SPACES                               
03932          MOVE W-LABEL-LINES (3)  TO W-LABEL-LINES (2)             
03933          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)             
03934          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)             
03935          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)             
03936          MOVE SPACES             TO W-LABEL-LINES (6)             
03937          GO TO 7600-LABEL-MOVE.                                   
03938                                                                   
03939      IF  W-LABEL-LINES (3) = SPACES AND                           
03940          W-LABEL-LINES (4) = SPACES AND                           
03941          W-LABEL-LINES (5) = SPACES AND                           
03942          W-LABEL-LINES (6) = SPACES                               
03943          SET W-NDX               TO 2                             
03944          GO TO 7600-MOVE-ZIP.                                     
03945                                                                   
03946      IF  W-LABEL-LINES (3) = SPACES                               
03947          MOVE W-LABEL-LINES (4)  TO W-LABEL-LINES (3)             
03948          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)             
03949          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)             
03950          MOVE SPACES             TO W-LABEL-LINES (6)             
03951          GO TO 7600-LABEL-MOVE.                                   
03952                                                                   
03953      IF  W-LABEL-LINES (4) = SPACES AND                           
03954          W-LABEL-LINES (5) = SPACES AND                           
03955          W-LABEL-LINES (6) = SPACES                               
03956          SET W-NDX               TO 3                             
03957          GO TO 7600-MOVE-ZIP.                                     
03958                                                                   
03959      IF  W-LABEL-LINES (4) = SPACES                               
03960          MOVE W-LABEL-LINES (5)  TO W-LABEL-LINES (4)             
03961          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)             
03962          MOVE SPACES             TO W-LABEL-LINES (6)             
03963          GO TO 7600-LABEL-MOVE.                                   
03964                                                                   
03965      IF  W-LABEL-LINES (5) = SPACES AND                           
03966          W-LABEL-LINES (6) = SPACES                               
03967          SET W-NDX               TO 4                             
03968          GO TO 7600-MOVE-ZIP.                                     
03969                                                                   
03970      IF  W-LABEL-LINES (5) = SPACES                               
03971          MOVE W-LABEL-LINES (6)  TO W-LABEL-LINES (5)             
03972          MOVE SPACES             TO W-LABEL-LINES (6)             
03973          SET W-NDX               TO 5                             
03974          GO TO 7600-MOVE-ZIP                                      
03975                                                                   
03976      ELSE                                                         
03977          IF  W-LABEL-LINES (6) = SPACES                           
03978              SET W-NDX           TO 5                             
03979              GO TO 7600-MOVE-ZIP                                  
03980                                                                   
03981          ELSE                                                     
03982              SET W-NDX           TO 6.                            
03983                                                                   
03984  7600-MOVE-ZIP.                                                   
03985                                                                   
03986      SET W-NDX2                  TO W-NDX.                        
03987      SET W-NDX2 DOWN BY +1.                                       
03988                                                                   
03989      IF  W-LAST-ZIP (W-NDX2) = SPACES                             
03990              AND                                                  
03991          W-LAST-DIGIT (W-NDX2) EQUAL SPACES                       
03992 *****CANADIAN ZIP CODES (NON NUMERIC) STAY ON THE LAST LINE       
03993                                                                   
03994          IF  W-LABEL-1ST-ZIP (W-NDX) NUMERIC                      
03995                                                                   
03996              IF  PI-COMPANY-ID NOT = 'FLA'                        
03997                  MOVE W-LABEL-ZIP (W-NDX)                         
03998                                  TO W-LAST-ZIP (W-NDX2)           
03999                  MOVE SPACES     TO W-LABEL-LINES (W-NDX).        
04000                                                                   
04001  7600-EXIT.                                                       
04002      EXIT.                                                        
04003                                  EJECT                            
04004  7700-TRANSLATE-LOWER.                                            
04005                                                                   
04006      IF  LOWER-CASE-LETTERS-USED                                  
04007          NEXT SENTENCE                                            
04008                                                                   
04009      ELSE                                                         
04010          GO TO 7700-EXIT.                                         
04011                                                                   
04012      IF  W-LABEL-LINES (1) NOT = SPACES                           
04013          MOVE W-LABEL-LINES (1)  TO W-TEMP-AREA2                  
04014          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
04015          MOVE W-TEMP-AREA2       TO W-LABEL-LINES (1).            
04016                                                                   
04017      IF  W-LABEL-LINES (2) NOT = SPACES                           
04018          MOVE W-LABEL-LINES (2)  TO W-TEMP-AREA2                  
04019          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
04020          MOVE W-TEMP-AREA2       TO W-LABEL-LINES (2).            
04021                                                                   
04022      IF  W-LABEL-LINES (3) NOT = SPACES                           
04023          MOVE W-LABEL-LINES (3)  TO W-TEMP-AREA2                  
04024          PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT         
04025          MOVE W-TEMP-AREA2       TO W-LABEL-LINES (3).            
04026                                                                   
04027 *****THE CITY STATE WILL BE ON LINE FOUR OR FIVE DEPENDING        
04028 *****ON THE FORMAT USED.  THE SIXTH LINE BEING BLANK WILL TELL.   
04029                                                                   
04030      MOVE 'N'                    TO W-STATE-LINE.                 
04031                                                                   
04032      IF  W-LABEL-LINES (4) NOT = SPACES                           
04033         IF  W-LABEL-LINES (6) = SPACES                            
04034            MOVE 'Y'              TO W-STATE-LINE                  
04035            MOVE W-LABEL-LINES (4)                                 
04036                                  TO W-TEMP-AREA2                  
04037            PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT       
04038            MOVE W-TEMP-AREA2     TO W-LABEL-LINES (4).            
04039                                                                   
04040      IF  W-LABEL-LINES (5) NOT = SPACES                           
04041         IF  W-LABEL-LINES (6) NOT = SPACES                        
04042            MOVE 'Y'              TO W-STATE-LINE                  
04043            MOVE W-LABEL-LINES (5)                                 
04044                                  TO W-TEMP-AREA2                  
04045            PERFORM 7750-SEARCH-AND-TRANSLATE THRU 7750-EXIT       
04046            MOVE W-TEMP-AREA2     TO W-LABEL-LINES (5).            
04047                                                                   
04048  7700-EXIT.                                                       
04049      EXIT.                                                        
04050                                  EJECT                            
04051                                                                   
04052  7750-SEARCH-AND-TRANSLATE.                                       
04053                                                                   
04054      SET W-TA1                   TO +1.                           
04055                                                                   
04056  7750-FIND-FIRST-NON-BLANK.                                       
04057                                                                   
04058      IF  W-TEMP-2 (W-TA1) = SPACES                                
04059          SET W-TA1 UP BY 1                                        
04060          GO TO 7750-FIND-FIRST-NON-BLANK.                         
04061                                                                   
04062 *****SET INDEX TO THE NEXT CHAR TO START THE TRANSLATE.           
04063      SET W-TA2                   TO W-TA1                         
04064      SET W-TA2 UP BY 1                                            
04065      SET W-TA21                  TO W-TA2                         
04066      SET W-TA21 UP BY 1.                                          
04067                                                                   
04068      MOVE 'N'                    TO W-DATA-FOUND-SW.              
04069      PERFORM 7282-FIND-NEXT-BLANK THRU 7282-EXIT.                 
04070                                                                   
04071  7750-EXIT.                                                       
04072      EXIT.                                                        
04073      EJECT                                                        
04074                                                                   
04075  7282-FIND-NEXT-BLANK.                                            
04076                                                                   
04077      IF  W-TA21 GREATER THAN 31                                   
04078          GO TO 7282-EXIT.                                         
04079                                                                   
04080      IF  W-TA21 EQUAL 31                                          
04081                                                                   
04082          IF  NO-CHARACTERS-FOUND                                  
04083              GO TO 7282-EXIT                                      
04084                                                                   
04085          ELSE                                                     
04086              SET W-TA1           TO +1                            
04087              MOVE SPACES         TO W-TEMP-AREA1                  
04088                                                                   
04089              PERFORM 7283-MOVE                                    
04090                      VARYING                                      
04091                  W-MOVE-NDX FROM W-TA2 BY 1                       
04092                      UNTIL                                        
04093                  W-MOVE-NDX EQUAL W-TA21                          
04094                                                                   
04095              PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT       
04096              SET W-TA1           TO +1                            
04097                                                                   
04098              PERFORM 7284-MOVE-BACK                               
04099                      VARYING                                      
04100                  W-MOVE-NDX FROM W-TA2 BY 1                       
04101                      UNTIL                                        
04102                  W-MOVE-NDX EQUAL W-TA21                          
04103                                                                   
04104              GO TO 7282-EXIT.                                     
04105                                                                   
04106      IF  W-TEMP-2 (W-TA2) = SPACE OR ',' OR '/'                   
04107          GO TO 7282-NEXT-GROUP-SEARCH.                            
04108                                                                   
04109      IF  W-TEMP-2 (W-TA21) = SPACES OR ',' OR '/'                 
04110          NEXT SENTENCE                                            
04111                                                                   
04112      ELSE                                                         
04113          MOVE 'Y'                TO W-DATA-FOUND-SW               
04114          SET W-TA21 UP BY +1                                      
04115          GO TO 7282-FIND-NEXT-BLANK.                              
04116                                                                   
04117      SET W-TA1                   TO +1.                           
04118      MOVE SPACES                 TO W-TEMP-AREA1.                 
04119                                                                   
04120      PERFORM 7283-MOVE                                            
04121              VARYING                                              
04122          W-MOVE-NDX FROM W-TA2 BY 1                               
04123              UNTIL                                                
04124          W-MOVE-NDX EQUAL W-TA21.                                 
04125                                                                   
04126      PERFORM 7285-TEST-AND-TRANSLATE THRU 7285-EXIT.              
04127      SET W-TA1                   TO +1.                           
04128                                                                   
04129      PERFORM 7284-MOVE-BACK                                       
04130              VARYING                                              
04131          W-MOVE-NDX FROM W-TA2 BY 1                               
04132              UNTIL                                                
04133          W-MOVE-NDX EQUAL W-TA21.                                 
04134                                                                   
04135      SET W-TA2                  TO W-TA21.                        
04136      SET W-TA21 UP BY 1.                                          
04137      MOVE 'N'                   TO W-DATA-FOUND-SW.               
04138                                                                   
04139  7282-NEXT-GROUP-SEARCH.                                          
04140                                                                   
04141      IF  W-TEMP-2 (W-TA2) EQUAL SPACES OR ',' OR '/'              
04142          SET W-TA2 UP BY 1                                        
04143                                                                   
04144          IF  W-TA2 = 30                                           
04145              GO TO 7282-EXIT                                      
04146                                                                   
04147          ELSE                                                     
04148              GO TO 7282-NEXT-GROUP-SEARCH.                        
04149                                                                   
04150      SET W-TA2  UP BY 1.                                          
04151      SET W-TA21                  TO W-TA2.                        
04152      SET W-TA21 UP BY 1.                                          
04153      GO TO 7282-FIND-NEXT-BLANK.                                  
04154                                                                   
04155  7282-EXIT.                                                       
04156      EXIT.                                                        
04157                                                                   
04158  7283-MOVE.                                                       
04159                                                                   
04160      MOVE W-TEMP-2 (W-MOVE-NDX)  TO W-TEMP-1 (W-TA1).             
04161      SET W-TA1 UP BY +1.                                          
04162                                                                   
04163  7284-MOVE-BACK.                                                  
04164                                                                   
04165      MOVE W-TEMP-1 (W-TA1)       TO W-TEMP-2 (W-MOVE-NDX).        
04166      SET W-TA1 UP BY +1.                                          
04167                                                                   
04168  7285-TEST-AND-TRANSLATE.                                         
04169 ***BYPASS IF  THE AREA MAY BE A PO BOX, OR RR NUMBER              
04170                                                                   
04171      IF  W-TEMP-AREA1 = '.O.' OR 'RR'                             
04172          GO TO 7285-EXIT.                                         
04173                                                                   
04174 ***BYPASS IF IT IS A CITY/STATE LINE AND BEYOND CHARACTER 07      
04175 ***AND IT APPEARS THAT IT MAY BE A ABREVIATION.                   
04176                                                                   
04177      SET W-POSITION2             TO W-TA2.                        
04178      SET W-POSITION21            TO W-TA21.                       
04179      COMPUTE W-WORD-LENGTH = W-POSITION21 - W-POSITION2.          
04180                                                                   
04181      IF  W-WORD-LENGTH LESS THAN 3                                
04182              AND                                                  
04183          W-STATE-LINE = 'Y'                                       
04184              AND                                                  
04185          W-TA2 GREATER THAN 07                                    
04186          GO TO 7285-EXIT.                                         
04187                                                                   
04188      INSPECT W-TEMP-AREA1 CONVERTING W-UPPER-CASE TO W-LOWER-CASE.
04189                                                                   
04190  7285-EXIT.                                                       
04191      EXIT.                                                        
04192                                                                   
04193  7800-VARIABLE-SEARCH.                                            
04194 ***************************************************************   
04195 *    THIS ROUTINE SEARCHES THE NEWLY CREATED LETTER FOR ANY   *   
04196 *    VARIABLE SYMBOL AND WILL REPLACE THE VARIABLE SYMBOL     *   
04197 *    WITH THE CORRESPONDING DATA FROM THE SYSTEM DEFINED      *   
04198 *    DATA THAT WAS GENERATED IN PARAGRAPHS 7000-7299.         *   
04199 *                                                             *   
04200 *    THE ADDRESSING OF THE VARIABLE TABLE IS ACCOMPLISHED     *   
04201 *    BY DOING A GETMAIN, MOVING THE TABLE TO THE NEW STORAGE  *   
04202 *    AND BY ADJUSTING THE BLL POINTER TO POINT AT THE DATA.   *   
04203 ***************************************************************   
04204                                                                   
04205      MOVE W-REC-TEXT (W-TB-NDX)  TO W-SINGLE-LINE.                
04206      SET NDX1                    TO 1.                            
04207                                                                   
04208  7801-LOOP.                                                       
04209                                                                   
04210      IF  NDX1 GREATER 70                                          
04211          MOVE W-SINGLE-LINE      TO W-REC-TEXT (W-TB-NDX)         
04212          GO TO 7899-EXIT.                                         
04213                                                                   
04214      IF  W-ONE-CHAR (NDX1) NOT = '@'                              
04215          SET NDX1 UP BY 1                                         
04216          GO TO 7801-LOOP.                                         
04217                                                                   
04218      SET NDX2                    TO NDX1.                         
04219      SET NDX2 UP BY 1.                                            
04220                                                                   
04221      IF  W-ONE-CHAR (NDX2) = '@'                                  
04222          SET NDX1 UP BY 2                                         
04223          GO TO 7801-LOOP.                                         
04224                                                                   
04225      MOVE W-ONE-CHAR (NDX2)      TO W-V1.                         
04226      SET NDX2 UP BY 1.                                            
04227      MOVE W-ONE-CHAR (NDX2)      TO W-V2.                         
04228      SET NDX2 UP BY 1.                                            
04229      MOVE W-ONE-CHAR (NDX2)      TO W-V3.                         
04230      SET NDX2 UP BY 1.                                            
04231      MOVE W-ONE-CHAR (NDX2)      TO W-V4.                         
04232                                                                   
04233      IF  W-V-NUM NOT NUMERIC                                      
04234          GO TO 7830-VAR-ERROR.                                    
04235                                                                   
04236      IF  W-V-PERIOD NOT = '.'                                     
04237          MOVE '.'                TO W-V-PERIOD                    
04238          MOVE ZERO               TO W-V-DECIMAL                   
04239          GO TO 7840-TABLE-SEARCH.                                 
04240                                                                   
04241      IF  W-V-DECIMAL NUMERIC                                      
04242          GO TO 7840-TABLE-SEARCH.                                 
04243                                                                   
04244  7830-VAR-ERROR.                                                  
04245                                                                   
04246      MOVE ER-0180                TO W-CURRENT-ERROR.              
04247      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
04248      SET NDX1 UP BY 1.                                            
04249      GO TO 7801-LOOP.                                             
04250                                                                   
04251  7840-TABLE-SEARCH.                                               
04252                                                                   
04253      IF  W-NO-GETMAIN-DONE-YET                                    
04254                                                                   
04255          IF  PI-VAR-POINTER GREATER THAN ZEROS                    
04256              MOVE PI-VAR-POINTER TO LCP-WS-ADDR-COMP              
04257              SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR  
04258 *            SERVICE RELOAD SYSTEM-VARIABLES                      
04259              MOVE SYSTEM-SUPPORTED-VARIABLES                      
04260                                  TO SYSTEM-VARIABLES              
04261              MOVE 1              TO W-GETMAIN-SW                  
04262              MOVE 1              TO SS-COUNTER                    
04263                                                                   
04264          ELSE                                                     
04265              EXEC CICS GETMAIN                                    
04266                   SET     (ADDRESS OF SYSTEM-VARIABLES)           
04267                   LENGTH  (SS-WORK-AREA-LENGTH)                   
04268              END-EXEC                                             
04269                                                                   
04270 *            SERVICE RELOAD SYSTEM-VARIABLES                      
04271                                                                   
04272              MOVE 1              TO W-GETMAIN-SW                  
04273              SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES  
04274              MOVE LCP-WS-ADDR-COMP                                
04275                                  TO PI-VAR-POINTER                
04276              MOVE SYSTEM-SUPPORTED-VARIABLES                      
04277                                  TO SYSTEM-VARIABLES              
04278              MOVE 1              TO SS-COUNTER                    
04279                                                                   
04280      ELSE                                                         
04281          MOVE 1                  TO SS-COUNTER                    
04282          MOVE PI-VAR-POINTER     TO LCP-WS-ADDR-COMP              
04283          SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR.     
04284                                                                   
04285  7850-TABLE-LOOP.                                                 
04286                                                                   
04287 *    SERVICE RELOAD SYSTEM-VARIABLES.                             
04288                                                                   
04289      IF  SS-COUNTER GREATER THAN  SS-NUM-ENTRIES                  
04290          GO TO 7830-VAR-ERROR.                                    
04291                                                                   
04292      IF  SYS-VAR-CODE NOT = W-VAR-HOLD                            
04293          SET LCP-WS-ADDR-PNTR TO ADDRESS OF SYSTEM-VARIABLES      
04294          ADD SYS-VAR-LEN         TO LCP-WS-ADDR-COMP              
04295          SET ADDRESS OF SYSTEM-VARIABLES TO LCP-WS-ADDR-PNTR      
04296          ADD 1                   TO SS-COUNTER                    
04297          GO TO 7850-TABLE-LOOP.                                   
04298                                                                   
04299      MOVE SYS-VAR-ENTRY          TO W-VARIABLE-WORK-AREA.         
04300      SET W-NDXV                  TO 1.                            
04301      SUBTRACT 6                  FROM W-VAR-LEN.                  
04302      PERFORM 7900-MOVE-VAR-DATA THRU 7900-EXIT                    
04303          W-VAR-LEN TIMES.                                         
04304      GO TO 7801-LOOP.                                             
04305                                                                   
04306  7899-EXIT.                                                       
04307       EXIT.                                                       
04308                                                                   
04309  7900-MOVE-VAR-DATA.                                              
04310                                                                   
04311      IF  NDX1 GREATER 70                                          
04312          MOVE ER-0181            TO W-CURRENT-ERROR               
04313          PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT                
04314          GO TO 7801-LOOP.                                         
04315                                                                   
04316      MOVE W-VAR-W-ONE-CHAR (W-NDXV) TO W-ONE-CHAR (NDX1).         
04317      SET W-NDXV UP BY 1.                                          
04318      SET NDX1 UP BY 1.                                            
04319                                                                   
04320  7900-EXIT.                                                       
04321       EXIT.                                                       
04322                                  EJECT                            
04323  8000-STARTBR-ERACCT.                                             
04324                                                                   
04325      IF  W-ACCT-READ-SW EQUAL    TO 'Y'                           
04326          NEXT SENTENCE                                            
04327                                                                   
04328      ELSE                                                         
04329          IF  PI-ACCT-POINTER GREATER THAN ZEROS                   
04330              MOVE PI-ACCT-POINTER                                 
04331                                  TO LCP-WS-ADDR-COMP              
04332              SET ADDRESS OF ACCOUNT-MASTER TO LCP-WS-ADDR-PNTR    
04333 *            SERVICE RELOAD ACCOUNT-MASTER                        
04334                                                                   
04335          ELSE                                                     
04336              EXEC CICS GETMAIN                                    
04337                   SET      (ADDRESS OF ACCOUNT-MASTER)            
04338                   LENGTH   (W-ACCT-LENGTH)                        
04339              END-EXEC                                             
04340              SET LCP-WS-ADDR-PNTR TO ADDRESS OF ACCOUNT-MASTER    
04341                                                                   
04342 *            SERVICE RELOAD ACCOUNT-MASTER                        
04343              MOVE LCP-WS-ADDR-COMP TO PI-ACCT-POINTER.            
04344                                                                   
04345      EXEC CICS STARTBR                                            
04346          RIDFLD      (W-ACCT-KEY)                                 
04347          DATASET     (W-ACCT-ID)                                  
04348          KEYLENGTH   (20)                                         
04349          GENERIC                                                  
04350      END-EXEC.                                                    
04351                                                                   
04352      MOVE 'Y'                   TO W-ACCT-BROWSE-STARTED.         
04353                                                                   
04354  8000-EXIT.                                                       
04355      EXIT.                                                        
04356                                                                   
04357  8010-READNEXT-ERACCT.                                            
04358                                                                   
04359      EXEC CICS READNEXT                                           
04360          DATASET   (W-ACCT-ID)                                    
04361          INTO      (ACCOUNT-MASTER)                               
04362          RIDFLD    (W-ACCT-KEY)                                   
04363      END-EXEC.                                                    
04364                                                                   
04365  8010-EXIT.                                                       
04366      EXIT.                                                        
04367                                                                   
04368                                  EJECT                            
04369  8050-STARTBR-EMPROD.                                             
04370                                                                   
04371      IF  W-PROD-READ-SW EQUAL    TO 'Y'                           
04372          NEXT SENTENCE                                            
04373                                                                   
04374      ELSE                                                         
04375          IF  PI-PROD-POINTER GREATER THAN ZEROS                   
04376              MOVE PI-PROD-POINTER                                 
04377                                  TO LCP-WS-ADDR-COMP              
04378              SET ADDRESS OF PRODUCER-MASTER TO LCP-WS-ADDR-PNTR   
04379 *            SERVICE RELOAD PRODUCER-MASTER                       
04380                                                                   
04381          ELSE                                                     
04382              EXEC CICS GETMAIN                                    
04383                   SET      (ADDRESS OF PRODUCER-MASTER)           
04384                   LENGTH   (W-PROD-LENGTH)                        
04385              END-EXEC                                             
04386              SET LCP-WS-ADDR-PNTR TO ADDRESS OF PRODUCER-MASTER   
04387                                                                   
04388 *            SERVICE RELOAD PRODUCER-MASTER                       
04389              MOVE LCP-WS-ADDR-COMP TO PI-PROD-POINTER.            
04390                                                                   
04391      EXEC CICS STARTBR                                            
04392          RIDFLD      (W-PROD-KEY)                                 
04393          DATASET     (W-PROD-ID)                                  
04394          KEYLENGTH   (20)                                         
04395          GENERIC                                                  
04396      END-EXEC.                                                    
04397                                                                   
04398      MOVE 'Y'                   TO W-PROD-BROWSE-STARTED.         
04399                                                                   
04400  8050-EXIT.                                                       
04401      EXIT.                                                        
04402                                                                   
04403  8060-READNEXT-EMPROD.                                            
04404                                                                   
04405      EXEC CICS READNEXT                                           
04406          DATASET   (W-PROD-ID)                                    
04407          INTO      (PRODUCER-MASTER)                              
04408          RIDFLD    (W-PROD-KEY)                                   
04409      END-EXEC.                                                    
04410                                                                   
04411  8060-EXIT.                                                       
04412      EXIT.                                                        
04413                                                                   
04414                                 EJECT                             
04415  9600-PGMID-ERROR.                                                
04416                                                                   
04417      MOVE 9999                   TO W-CURRENT-ERROR.              
04418      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
04419      GO TO 0200-RETURN-TO-CALLING-PGM.                            
04420                                                                   
04421  9700-DATE-LINK.                                                  
04422                                                                   
04423      MOVE W-LINK-ELDATCV         TO W-PGM-NAME.                   
04424                                                                   
04425      EXEC CICS LINK                                               
04426          PROGRAM   (W-PGM-NAME)                                   
04427          COMMAREA  (DATE-CONVERSION-DATA)                         
04428          LENGTH    (DC-COMM-LENGTH)                               
04429      END-EXEC.                                                    
04430                                                                   
04431      IF  NO-CONVERSION-ERROR                                      
04432              AND                                                  
04433          W-REVERSE-DATE                                           
04434              AND                                                  
04435          PI-COMPANY-ID EQUAL 'AUK'                                
04436          MOVE DC-GREG-DATE-1-EDIT                                 
04437                                  TO W-EDIT-DATE-1                 
04438          MOVE W-ED1-MM           TO W-ED2-MM                      
04439          MOVE W-ED1-DD           TO W-ED2-DD                      
04440          MOVE W-ED1-YY           TO W-ED2-YY                      
04441          MOVE W-EDIT-DATE-2      TO DC-GREG-DATE-1-EDIT.          
04442                                                                   
04443  9700-EXIT.                                                       
04444       EXIT.                                                       
04445                                                                   
04446  9900-ERROR-PROCESS.                                              
04447                                                                   
04448      MOVE W-CURRENT-ERROR        TO PI-ERROR-CODE.                
04449                                                                   
04450      IF  PI-FATAL-ERROR                                           
04451          GO TO 0200-RETURN-TO-CALLING-PGM.                        
04452                                                                   
04453  9900-EXIT.                                                       
04454      EXIT.                                                        
04455                                                                   
04456  9990-ABEND.                                                      
04457                                                                   
04458      MOVE 9999                   TO W-CURRENT-ERROR.              
04459      PERFORM 9900-ERROR-PROCESS THRU 9900-EXIT.                   
04460      GO TO 0200-RETURN-TO-CALLING-PGM.                            
04461                                                                   
04462  9990-EXIT.                                                       
04463      EXIT.                                                        
