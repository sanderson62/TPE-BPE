00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ECS020.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 11/28/95 10:59:57.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.038.                          
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *                                                   *
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00018 *            *                                                   *
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00022 *            *                                                   *
00023 *            *****************************************************
00024                                                                   
00025 *REMARKS.                                                         
00026 *        THIS PROGRAM WILL PRINT THE EARNED PREMIUM               
00027 *        AND LOSS REPORT, AND CREATE A 'LOSS RATIO'               
00028 *        EXTRACT FILE TO BE LOADED ON-LINE.                       
061102******************************************************************
061102*                   C H A N G E   L O G
061102*
061102* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
061102*-----------------------------------------------------------------
061102*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
061102* EFFECTIVE    NUMBER
061102*-----------------------------------------------------------------
061102* 061102    2002060300022  SMVA  CHANGE SYS008 USER NAME TO
061102*                               PRINTER-OUTPUT TO RID COMPILE ERRS
061102******************************************************************
061402* 061402    2002010700009  PEMA  add the subtotal even if there
061402*                               is only one ah or lf detail line
070102* 070102    IR             SMVA ELIMINATE STACK OF BLANK PAGES AT
070102*                              BEGIN & END OF SYS008 OUTPUT (B&C)
061402******************************************************************
00029                                                                   
00030  ENVIRONMENT DIVISION.                                            
00031  CONFIGURATION SECTION.                                           
00032  SPECIAL-NAMES.                                                   
00033      C02 IS LCP-CH2                                               
00034      C03 IS LCP-CH3                                               
00035      C04 IS LCP-CH4                                               
00036      C05 IS LCP-CH5                                               
00037      C06 IS LCP-CH6                                               
00038      C07 IS LCP-CH7                                               
00039      C08 IS LCP-CH8                                               
00040      C09 IS LCP-CH9                                               
00041      C10 IS LCP-CH10                                              
00042      C11 IS LCP-CH11                                              
00043      C12 IS LCP-CH12                                              
00044      S01 IS LCP-P01                                               
00045      S02 IS LCP-P02.                                              
00046  INPUT-OUTPUT SECTION.                                            
00047  FILE-CONTROL.                                                    
00048                                                                   
00049      SELECT SORT-WORK        ASSIGN TO SYS001-UT-3380-S-SORTWK1.  
061102     SELECT PRINTER-OUTPUT   ASSIGN TO SYS008-UR-1403-S-SYS008.
00051      SELECT EXTRACTS         ASSIGN TO SYS011-UT-2400-S-SYS011.   
00052      SELECT LOSS-RATIOS      ASSIGN TO SYS013-UT-2400-S-SYS013.   
00053      SELECT RTBL-FILE        ASSIGN TO ERRTBLT
00054                              ACCESS IS SEQUENTIAL                 
00055                              ORGANIZATION IS INDEXED              
00056                              FILE STATUS IS REIN-FILE-STATUS      
00057                              RECORD KEY IS RE-CONTROL-PRIMARY.    
00058      SELECT ACCT-MASTER      ASSIGN TO ERACCTT
00059                              ACCESS IS DYNAMIC                    
00060                              ORGANIZATION IS INDEXED              
00061                              FILE STATUS IS AM-FILE-STATUS        
00062                              RECORD KEY IS AM-CONTROL-PRIMARY.    
00063                                                                   
00064      SELECT ELCNTL           ASSIGN TO SYS009-FBA1-ELCNTL         
00065                              ACCESS       DYNAMIC                 
00066                              ORGANIZATION INDEXED                 
00067                              FILE STATUS  ELCNTL-FILE-STATUS      
00068                              RECORD KEY   CF-CONTROL-PRIMARY.     
00069                                                                   
00070      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   
00071      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00072  EJECT                                                            
00072  EJECT                                                            
00073  DATA DIVISION.                                                   
00074  FILE SECTION.                                                    
00075                                                                   
00076  SD  SORT-WORK.                                                   
00077                                                                   
00078  01  SORT-RECORD.                                                 
00079      12  SW-CONTROL.                                              
00080          16  SW-RPT-CD-1         PIC X(10).                       
00081          16  SW-CARRIER          PIC X.                           
00082          16  SW-GROUPING         PIC X(6).                        
00083          16  SW-RPT-CD-2         PIC X(10).                       
00084          16  SW-STATE            PIC XX.                          
00085          16  SW-ACCOUNT          PIC X(10).                       
00086          16  SW-EXP-DATE         PIC 9(11)  COMP-3.               
00087          16  SW-EFF-DATE         PIC 9(11)  COMP-3.               
00088      12  SW-EP-RECORD.                                            
00089          16  FILLER              PIC XXX.                         
00090          16  SW-REIN             PIC X.                           
00091          16  FILLER              PIC X(31).                       
00092          16  SW-REI-CO.                                           
00093              20  SW-REINCO       PIC XXX.                         
00094              20  SW-REINCO-SUB   PIC XXX.                         
00095          16  SW-RCD-TYPE         PIC X.                           
00096          16  SW-BEN-CODE         PIC XX.                          
00097          16  FILLER              PIC X(274).                      
00098          16  SW-PURGE            PIC X.                           
00099          16  SW-RUN-DTE          PIC 9(11)  COMP-3.               
00100      12  SW-ACCT-FLAG            PIC X.                           
JJPMOD     12  SW-REIN-NAME            PIC X(30).
00101                                                                   
061102 FD  PRINTER-OUTPUT
00103                                 COPY ELCPRTFD.                    
00104  EJECT                                                            
00105  FD  EXTRACTS                                                     
00106      BLOCK CONTAINS 0 RECORDS
00107      RECORDING MODE F.                                            
00108                                                                   
00109  01  EPR-RECORD.                                                  
00110      12  EPR-REC-ID                PIC XX.                        
00111      12  EPR-COMPANY-CD            PIC X.                         
00112      12  EPR-REIN                  PIC X.                         
00113      12  EPR-CONTROL.                                             
00114          16  EPR-CARRIER           PIC X.                         
00115          16  EPR-GROUPING.                                        
00116              20  EPR-GROUP-PREFIX  PIC XXX.                       
00117              20  EPR-GROUP-PRIME   PIC XXX.                       
00118          16  EPR-STATE             PIC XX.                        
00119          16  EPR-ACCOUNT.                                         
00120              20  EPR-ACCT-PREFIX   PIC X(4).                      
00121              20  EPR-ACCT-PRIME    PIC X(6).                      
00122          16  EPR-DATES.                                           
00123              20  EPR-EXP-DTE       PIC 9(11)  COMP-3.             
00124              20  EPR-EFF-DTE       PIC 9(11)  COMP-3.             
00125      12  EPR-REI-CO                PIC X(6).                      
00126      12  EPR-RCD-TYPE              PIC X.                         
00127      12  EPR-BEN-CODE              PIC XX.                        
00128      12  EPR-SPEC-REIN             PIC X.                         
00129      12  FILLER                    PIC X(273).                    
00130      12  EPR-PURGE                 PIC X.                         
00131      12  EPR-RUN-DATE              PIC 9(11)  COMP-3.             
00132                                                                   
00133  FD  LOSS-RATIOS                                                  
00134      BLOCK CONTAINS 0 RECORDS
00135      RECORDING MODE F.                                            
00136                                                                   
00137  01  LOSS-RATIO-EXTRACT            PIC X(525).                    
00138                                                                   
00139  EJECT                                                            
00140  FD  RTBL-FILE.                                                   
00141                                                                   
00142                                    COPY ERCREIN.                  
00143  EJECT                                                            
00144  FD  ACCT-MASTER.                                                 
00145                                                                   
00146                                    COPY ERCACCT.                  
00147                                                                   
00148  EJECT                                                            
00149  FD  ELCNTL.                                                      
00150                                                                   
00151                                    COPY ELCCNTL.                  
00152                                                                   
00153  FD  DISK-DATE                                                    
00154                                    COPY ELCDTEFD.                 
00155  EJECT                                                            
00156  FD  FICH                                                         
00157                                    COPY ELCFCHFD.                 
00158  EJECT                                                            
00159  WORKING-STORAGE SECTION.                                         
00160  77  LCP-ASA                       PIC X.                         
00161  77  FILLER  PIC X(32) VALUE '********************************'.  
00162  77  FILLER  PIC X(32) VALUE '     ECS020 WORKING STORAGE     '.  
00163  77  FILLER  PIC X(32) VALUE '******** VMOD=2.038 ************'.  
00164                                                                   
00165  77  W-ZEROS                 PIC S9(4) COMP VALUE +0.             
00166  77  VALID-RTBL              PIC 9     VALUE 0.                   
00167  77  VALID-EXT               PIC 9     VALUE 0.                   
00168  77  EXT-EOF                 PIC 9     VALUE 0.                   
00169  77  FIRST-SW                PIC 9     VALUE 0.                   
00170  77  SECND-PASS-SW           PIC 9     VALUE 0.                   
00171  77  L1                      PIC X     VALUE SPACES.              
00172  77  EP-COMP                 PIC X(7)  VALUE SPACES.              
00173  77  EP-KEY                  PIC X(26) VALUE SPACES.              
CIDMOD                                                                  
070102*01  SPACE-LINE              PIC X(132)     VALUE SPACES.
00174                                                                   
00175  01  AM-FILE-STATUS          PIC XX.                              
00176                                                                   
00177  01  ELCNTL-FILE-STATUS.                                          
00178      12  CNTL-FILE-STATUS-1  PIC X.                               
00179      12  CNTL-FILE-STATUS-2  PIC X.                               
00180                                                                   
00181  01  REIN-FILE-STATUS.                                            
00182      12  REIN-FILE-STATUS-1  PIC X.                               
00183      12  REIN-FILE-STATUS-2  PIC X.                               
00184                                                                   
00185  01  WORK-ABEND-CODE.                                             
00186      12  WAC-1               PIC X.                               
00187      12  WAC-2               PIC X.                               
00188      12  WAC-3-4.                                                 
00189          16  WAC-3           PIC X.                               
00190          16  WAC-4           PIC X.                               
UNIX   01  WS-HOLD-SORT-RECORD     PIC X(407) VALUE SPACES.
00192  01  MISC.                                                        
UNIX       12  WS-RELEASE-CNT      PIC 9(9)  VALUE ZEROS.
UNIX       12  WS-RETURN-CNT       PIC 9(9)  VALUE ZEROS.
00193      12  WS-CUSTOMIZATION-FLAG PIC X   VALUE SPACE.               
00194          88  CUSTOMIZATION-FOUND       VALUE SPACE.               
unix       12  X1                  PIC S999    COMP-3.                    
00196      12  X2                  PIC S999    COMP-3.                    
00197      12  X3                  PIC S999    COMP-3.                    
00198      12  X4                  PIC S999    COMP-3.                    
00199      12  X5                  PIC S999    COMP-3.                    
00200      12  X                   PIC X.                               
00201      12  ZERO-FLAG           PIC X               VALUE SPACE.     
00202          88  ALL-AMOUNTS-ZERO   VALUE ' '.                        
00203      12  W-TOTAL-SW          PIC X(01)           VALUE SPACE.     
00204          88  W-TOTALS-BEING-PROCESSED VALUE 'Y'.                  
00205      12  WS-RETURN-CODE      PIC X(4)            VALUE ZEROS.     
00206      12  ABEND-OPTION        PIC X               VALUE 'Y'.       
00207      12  WS-ABEND-MESSAGE    PIC X(80)           VALUE SPACES.    
00208      12  WS-ABEND-FILE-STATUS PIC XX             VALUE SPACES.    
00209      12  WS-ZERO             PIC S9              VALUE ZERO.      
00210      12  PAGER               PIC S9(5)   COMP-3  VALUE ZERO.      
00211      12  LINER               PIC S999    COMP-3  VALUE +99.       
00212      12  PGM-SUB             PIC S999    COMP-3  VALUE +020.      
00213      12  DATE-PRT-CTR        PIC S9(5)   COMP-3  VALUE ZERO.      
00214      12  WS-Y-LOSS           PIC S99V999 COMP-3.                  
00215      12  WS-I-LOSS           PIC S99V999 COMP-3.                  
00216      12  WS-FAC              PIC S999V9(5)       COMP-3.          
00217      12  WS-PRO              PIC S9V9999         COMP-3.          
00218      12  WS-R78              PIC S9V9999         COMP-3.          
00219      12  LF-PRO              PIC S9V9999         COMP-3.          
00220      12  LF-R78              PIC S9V9999         COMP-3.          
00221      12  AH-PRO              PIC S9V9999         COMP-3.          
00222      12  AH-R78              PIC S9V9999         COMP-3.          
00223      12  TT-PRM-78           PIC S9(9)V99        COMP-3.          
00224      12  TT-PRM-PR           PIC S9(9)V99        COMP-3.          
00225      12  TT-PRM-ST           PIC S9(9)V99        COMP-3.          
00226      12  LOSS-RATIO-SW       PIC X               VALUE SPACE.     
00227          88  LOSS-RATIO-TOO-LARGE      VALUE 'Y'.                 
00228      12  BENEFIT-PRT-SW      PIC X               VALUE SPACE.     
00229          88  PRINTING-1ST-BENEFIT      VALUE 'X'.                 
00230      12  G-A-PRT-SW          PIC X               VALUE SPACE.     
00231          88  PRINTING-G-A-HDR          VALUE SPACE.               
00232      12  REIN-PRT-SW         PIC X               VALUE SPACE.     
00233          88  PRINTING-REIN-TOTALS      VALUE 'X'.                 
00234      12  WS-PRT-FLAGS.                                            
00235          16  WS-REIN-PRT-FLG PIC X.                               
00236          16  WS-GROS-PRT-FLG PIC X.                               
00237      12  WS-RUN-DTE.                                              
00238          16  WS-RUN-CC       PIC 99.                              
00239          16  WS-RUN-YR       PIC 99.                              
00240          16  WS-RUN-MO       PIC 99.                              
00241          16  WS-RUN-DA       PIC 99.                              
00242      12  WS-RUN-DTE-N REDEFINES                                   
00243             WS-RUN-DTE       PIC 9(8).                            
00244      12  WS-DATE.                                                 
00245          16  WS-MO           PIC 99.                              
00246          16  FILLER          PIC X               VALUE '-'.       
00247          16  WS-DA           PIC 99.                              
00248          16  FILLER          PIC X               VALUE '-'.       
00249          16  WS-YR           PIC 99.                              
00250      12  WS-EPR-RUN-DATE     PIC 9(11).                           
00251      12  WS-EPR-RUN-DATE-N REDEFINES WS-EPR-RUN-DATE.             
00252          16  FILLER                PIC 999.                       
00253          16  EPR-RUN-YR-MO.                                       
00254              20  EP-RUN-CC         PIC 99.                        
00255              20  EPR-RUN-YR        PIC 99.                        
00256              20  FILLER            PIC 99.                        
00257          16  FILLER                PIC 99.                        
00258                                                                   
00259      12  WS-REIN-COMP-TBL.                                        
00260          16  WS-REIN-COMP-TBL-X OCCURS 1500 TIMES.                
00261              20  WS-RCT-COMP PIC X(6).                            
00262              20  WS-RCT-NAME PIC X(30).                           
00263              20  WS-RCT-CEDE PIC X(30).                           
00264              20  WS-LF-IBNR  PIC SV999   COMP-3.                  
00265              20  WS-AH-IBNR  PIC SV999   COMP-3.                  
00266              20  WS-LF-PRO   PIC S9V9999 COMP-3.                  
00267              20  WS-LF-R78   PIC S9V9999 COMP-3.                  
00268              20  WS-AH-PRO   PIC S9V9999 COMP-3.                  
00269              20  WS-AH-R78   PIC S9V9999 COMP-3.                  
00270                                                                   
00271      12  WS-RCT-X            PIC S9(4)   COMP    VALUE +0.        
00272      12  WS-RCT-MAX          PIC S9(4)   COMP    VALUE +1500.     
00273      12  WS-LAST-REIN-CO     PIC X(6)            VALUE SPACES.    
00274      12  WS-ACCT-OV-SW       PIC X               VALUE SPACE.     
00275      12  W-TOTAL-LINE.                                            
00276          16  W-TOTAL-WORK-AREA.                                   
00277              20  W-TW-1-TITLE                                     
00278                              PIC X(09).                           
00279              20  W-TW-1-CODE PIC X(11).                           
00280              20  W-TW-1-LP   PIC X(01).                           
00281              20  W-TW-1-NAME PIC X(30).                           
00282              20  W-TW-1-RP   PIC X(01).                           
00283              20  FILLER      PIC X(01).                           
00284              20  W-TW-2-TITLE                                     
00285                              PIC X(14).                           
00286              20  W-TW-2-CODE PIC X(11).                           
00287              20  W-TW-2-LP   PIC X(01).                           
00288              20  W-TW-2-NAME PIC X(30).                           
00289              20  W-TW-2-RP   PIC X(01).                           
00290              20  FILLER      PIC X(01).                           
00291              20  W-TW-3-TITLE                                     
00292                              PIC X(14).                           
00293              20  W-TW-3-CODE PIC X(11).                           
00294              20  W-TW-3-LP   PIC X(01).                           
00295              20  W-TW-3-NAME PIC X(30).                           
00296              20  W-TW-3-RP   PIC X(01).                           
00297              20  FILLER      PIC X(01).                           
00298              20  W-TW-4-TITLE                                     
00299                              PIC X(14).                           
00300              20  W-TW-4-CODE PIC X(11).                           
00301              20  W-TW-4-LP   PIC X(01).                           
00302              20  W-TW-4-NAME PIC X(30).                           
00303              20  W-TW-4-RP   PIC X(01).                           
00304              20  FILLER      PIC X(01).                           
00305              20  W-TW-5-TITLE                                     
00306                              PIC X(14).                           
00307              20  W-TW-5-CODE PIC X(11).                           
00308              20  W-TW-5-LP   PIC X(01).                           
00309              20  W-TW-5-NAME PIC X(30).                           
00310              20  W-TW-5-RP   PIC X(01).                           
00311              20  FILLER      PIC X(01).                           
00312          16  FILLER REDEFINES W-TOTAL-WORK-AREA.                  
00313              20  W-TW-CHAR   OCCURS 285 TIMES                     
00314                              INDEXED BY W-TW-NDX                  
00315                              PIC X(01).                           
CIDMOD                                                                  
CIDMOD 01  WORK-SV-RPT-CD.                                              
CIDMOD     12  FILLER              PIC X  VALUE SPACE.                  
CIDMOD     12  WK-SV-RPT-CD        PIC X(10).                           
CIDMOD                                                                  
00316  EJECT                                                            
00317  01  FILLER PIC X(32) VALUE '******* ACCUMULATORS ***********'.   
00318  01  DATE-TOTS               PIC X(22500).
00319  01  ACCT-TOTS               PIC X(22500).
00320  01  ST-TOTS                 PIC X(22500).
00321  01  RPT2-TOTS               PIC X(22500).
00322  01  COMP-TOTS               PIC X(22500).
00323  01  CARR-TOTS               PIC X(22500).
00324  01  REIN-TOTS               PIC X(22500).
00325  01  RPT1-TOTS               PIC X(22500).
00326  01  FINL-TOTS               PIC X(22500).
00327  01  WORK-TOT-0              PIC X(22500).
00328                                                                   
00329  01  TOTAL-WORK-AREAS.                                            
00330      12  WORK-TOT-1.                                              
00331          16  WT1             OCCURS 300.                          
00332              20  WT1-KEY.                                         
00333                  24  WT1-REIN    PIC X.                           
00334                  24  WT1-LF-AH   PIC X.                           
00335                  24  WT1-BEN-TYP PIC XX.                          
UNIX               20  WT1-ACCUMS  PIC X(71).
00337                                                                   
00338      12  WORK-TOT-2.                                              
00339          16  WT2             OCCURS 300.                          
00340              20  WT2-KEY.                                         
00341                  24  WT2-REIN    PIC X.                           
00342                  24  WT2-LF-AH   PIC X.                           
00343                  24  WT2-BEN-TYP PIC XX.                          
UNIX               20  WT2-ACCUMS  PIC X(71).
00345                                                                   
UNIX       12  WORK-ACCUMS-0       PIC X(71).
00347                                                                   
00348      12  WORK-ACCUMS-1       COMP-3.                              
00349          16  WA1-Y-CERT      PIC S9(7)       VALUE ZERO.          
00350          16  WA1-Y-NET       PIC S9(11)V99   VALUE ZERO.          
00351          16  WA1-Y-EARN      PIC S9(11)V99   VALUE ZERO.          
00352          16  WA1-Y-PAID      PIC S9(11)V99   VALUE ZERO.          
00353          16  WA1-Y-RESV      PIC S9(11)V99   VALUE ZERO.          
00354          16  WA1-I-CERT      PIC S9(7)       VALUE ZERO.          
00355          16  WA1-I-NET       PIC S9(11)V99   VALUE ZERO.          
00356          16  WA1-I-EARN      PIC S9(11)V99   VALUE ZERO.          
00357          16  WA1-I-PAID      PIC S9(11)V99   VALUE ZERO.          
00358          16  WA1-I-RESV      PIC S9(11)V99   VALUE ZERO.          
00359          16  WRK-RESV        PIC S9(11)V99   VALUE ZERO.          
00360                                                                   
00361      12  WORK-ACCUMS-2       COMP-3.                              
00362          16  WA2-Y-CERT      PIC S9(7).                           
00363          16  WA2-Y-NET       PIC S9(11)V99.                       
00364          16  WA2-Y-EARN      PIC S9(11)V99.                       
00365          16  WA2-Y-PAID      PIC S9(11)V99.                       
00366          16  WA2-Y-RESV      PIC S9(11)V99.                       
00367          16  WA2-I-CERT      PIC S9(7).                           
00368          16  WA2-I-NET       PIC S9(11)V99.                       
00369          16  WA2-I-EARN      PIC S9(11)V99.                       
00370          16  WA2-I-PAID      PIC S9(11)V99.                       
00371          16  WA2-I-RESV      PIC S9(11)V99.                       
00359          16  FILLER          PIC S9(11)V99   VALUE ZERO.
00372                                                                   
00373      12  TABLE-LIMIT         PIC S999    COMP-3  VALUE +300.      
00374      12  BEN-TOTALS          PIC X(71).
00375      12  TYP-TOTALS          PIC X(71).
00376      12  SAVE-PR-REIN        PIC X.                               
00377      12  SAVE-PR-LF-AH       PIC X.                               
00378                                                                   
00379      12  KEY-LOOK.                                                
00380          16  KL-REIN         PIC X.                               
00381          16  KL-LF-AH        PIC X.                               
00382          16  KL-BEN-TYP      PIC XX.                              
00383                                                                   
00384      12  PR-BEN-CTR          PIC S999    COMP-3  VALUE ZERO.      
00385      12  PR-TYP-CTR          PIC S999    COMP-3  VALUE ZERO.      
00386                                                                   
00387  01  SAVE-CONTROL.                                                
00388      12  SV-REIN             PIC X.                               
00389      12  SV-CONTROL.                                              
00390          16  SV-CONTROL-A.                                        
00391              20  SV-RPT-CD-1     PIC X(10).                       
00392              20  SV-CARRIER      PIC X.                           
00393              20  SV-GROUPING     PIC X(6).                        
00394              20  SV-RPT-CD-2     PIC X(10).                       
00395              20  SV-STATE        PIC XX.                          
00396              20  SV-ACCOUNT      PIC X(10).                       
00397          16  SV-EXP-DTE          PIC 9(11) COMP-3.                
00398          16  SV-EFF-DTE          PIC 9(11) COMP-3.                
00399      12  SV-REI-CO.                                               
00400          16  SV-REINCO       PIC XXX.                             
00401          16  SV-REINCO-SUB   PIC XXX.                             
00402      12  SV-ACCT-FLAG        PIC X.                               
00403                                                                   
00404  01  SV-NAME                 PIC X(30).                           
00405  01  SV-CITY                 PIC X(30).                           
00406  01  SV-REIN-NAME            PIC X(30).                           
JJPMOD 01  SV-X-REIN-NAME          PIC X(30).                           
JJPMOD 01  SV-X-REIN-COMP          PIC X(03).                           
00407                                                                   
00408  EJECT                                                            
00409                              COPY ERCLOSS.                        
00410                                                                   
00411  01  LOSS-RATIO-INITIALIZED  PIC X(525).                          
00412                                                                   
00413  EJECT                                                            
00414                              COPY ECSEPC01.                       
00415  EJECT                                                            
00416                              COPY ELCEPCVR.                       
00417  EJECT                                                            
00418                              COPY ELCDTECX.                       
00419  EJECT                                                            
00420                              COPY ELCDTEVR.                       
00421                                                                   
00422  01  DATE-EDIT.                                                   
00423      12  DE-MO               PIC 99.                              
00424      12  FILLER              PIC X           VALUE '/'.           
00425      12  DE-DA               PIC 99.                              
00426      12  FILLER              PIC X           VALUE '/'.           
00427      12  DE-YR               PIC 99.                              
00428  01  PRV-DEC.                                                     
00429      12  PRV-CCYY            PIC 9(04)       VALUE 1999.          
00430      12  PRV-CCYR REDEFINES PRV-CCYY.                             
00431          16  PRV-CC          PIC 99.                              
00432          16  PRV-YR          PIC 99.                              
00433      12  PRV-MO              PIC 99          VALUE 12.            
00434  01  WS-SV-EFF-DTE           PIC 9(11).                           
00435  01  WS-SV-EFF-DTE-R REDEFINES WS-SV-EFF-DTE.                     
00436      12  FILLER          PIC 999.                                 
00437      12  SV-EFF-CC       PIC 99.                                  
00438      12  SV-EFF-YR       PIC 99.                                  
00439      12  SV-EFF-MO       PIC 99.                                  
00440      12  SV-EFF-DA       PIC 99.                                  
00441  EJECT                                                            
00442  01  PRINT-LINES.                                                 
00443                                                                   
00444      12  LOSS-RATIO-MSG.                                          
00445          16  FILLER              PIC X(01)       VALUE ' '.
00446          16  FILLER              PIC X(49)       VALUE
00447          '****** THIS LOSS RATIO FIELD CONTAINS A VALUE TOO'.
00448          16  FILLER              PIC X(54)       VALUE            
00449          ' LARGE OR TOO SMALL TO BE SHOWN.'.
00450      12  HDR-1.                                                   
00451          16  FILLER          PIC X(48)       VALUE SPACES.        
00452          16  FILLER          PIC X(72)       VALUE                
00453             'EARNED PREMIUM AND LOSS REPORT'.                     
00454          16  FILLER          PIC X(6)        VALUE 'ECS020'.      
00455          16  H1-SUFFIX       PIC X           VALUE 'A'.           
00456      12  HDR-2.                                                   
00457          16  FILLER          PIC XX          VALUE SPACES.        
00458          16  H2-CARRIER      PIC X(10)       VALUE SPACES.        
00459          16  FILLER          PIC X           VALUE SPACES.        
00460          16  H2-CARR         PIC X           VALUE SPACES.        
00461          16  FILLER          PIC X(33)       VALUE SPACES.        
00462          16  H2-COMP         PIC X(30).                           
00463          16  FILLER          PIC X(43)       VALUE SPACES.        
00464          16  H2-DATE         PIC X(8).                            
00465      12  HDR-3.                                                   
00466          16  FILLER          PIC XX          VALUE SPACES.        
00467          16  H3-REIN         PIC X(13)       VALUE SPACES.        
00468          16  FILLER          PIC X           VALUE SPACES.        
00469          16  H3-REIN-COMP    PIC XXX         VALUE SPACES.        
00470          16  FILLER          PIC X           VALUE SPACES.        
00471          16  H3-REIN-NAME    PIC X(30)       VALUE SPACES.        
00472          16  FILLER          PIC X(4)        VALUE SPACES.        
00473          16  H3-DATE         PIC X(18).                           
00474          16  FILLER          PIC X(48)       VALUE SPACES.        
00475          16  FILLER          PIC X(5)        VALUE 'PAGE '.       
00476          16  H3-PAGE         PIC ZZ,ZZ9.                          
00477      12  HDR-3B.                                                  
00478          16  FILLER          PIC XX          VALUE SPACES.        
00479          16  H3B-RPT1-DESC   PIC X(13)     VALUE 'REPORT CODE 1'. 
00480          16  FILLER          PIC XXX         VALUE ' - '.         
00481          16  H3B-RPT-CODE-1  PIC X(10)       VALUE SPACES.        
00482      12  HDR-3C.                                                  
00483          16  FILLER          PIC XX          VALUE SPACES.        
00484          16  H3C-RPT2-DESC   PIC X(13)     VALUE 'REPORT CODE 2'. 
00485          16  FILLER          PIC XXX         VALUE ' - '.         
00486          16  H3C-RPT-CODE-2  PIC X(10)       VALUE SPACES.        
00487      12  HDR-3G.                                                  
00488          16  FILLER          PIC XX          VALUE SPACES.        
00489          16  H3G-G-A         PIC X(13)     VALUE 'GENERAL AGENT'. 
00490          16  FILLER          PIC X           VALUE SPACES.        
00491          16  H3G-G-A-NUMBER  PIC X(10)       VALUE SPACES.        
00492          16  FILLER          PIC XX          VALUE SPACES.        
00493          16  H3G-G-A-NAME    PIC X(30)       VALUE SPACES.        
00494      12  HDR-3R.                                                  
00495          16  FILLER          PIC XX          VALUE SPACES.        
00496          16  H3R-REIN        PIC X(13)       VALUE SPACES.        
00497          16  FILLER          PIC XX          VALUE '( '.          
00498          16  H3R-REIN-COMP   PIC XXX         VALUE SPACES.        
00499          16  FILLER          PIC XXX         VALUE ' ) '.         
00500          16  H3R-REIN-NAME   PIC X(30)       VALUE SPACES.        
00501      12  HDR-4A.                                                  
00502          16  H4A-SUMMARY     PIC X(20)       VALUE SPACES.        
00503          16  H4A-CARRIER     PIC X(7)        VALUE SPACES.        
00504          16  H4A-CARR        PIC X           VALUE SPACES.        
00505          16  FILLER          PIC X           VALUE SPACES.        
00506          16  H4A-GROUP       PIC X(5)        VALUE SPACES.        
00507          16  H4A-GRP         PIC X(6)        VALUE SPACES.        
00508          16  H4A-STATE       PIC X(5)        VALUE SPACES.        
00509          16  H4A-ST          PIC XX          VALUE SPACES.        
00510          16  H4A-ACCOUNT     PIC X(10)       VALUE SPACES.        
00511          16  H4A-ACCT        PIC X(10)       VALUE SPACES.        
00512          16  FILLER          PIC X           VALUE SPACES.        
00513          16  H4A-AM-NAME     PIC X(31)       VALUE SPACES.        
00514          16  FILLER          PIC X           VALUE SPACES.        
00515          16  H4A-CTY-ST      PIC X(15)       VALUE SPACES.        
00516          16  FILLER          PIC X           VALUE SPACES.        
00517          16  H4A-EFFECTIVE   PIC X(8)        VALUE SPACES.        
00518          16  H4A-EFF-DTE     PIC X(8)        VALUE SPACES.        
00519      12  HDR-4A-SUB-DESC.                                         
00520          16  FILLER          PIC X(11)    VALUE '  REIN SUB '.    
00521          16  H4A-SUB         PIC XXX.                             
00522          16  FILLER          PIC X           VALUE SPACES.        
00523      12  HDR-4-TOTAL.                                             
00524          16  FILLER          PIC X(12)                            
00525              VALUE ' TOTALS FOR '.                                
00526          16  H4T-LINE.                                            
00527              20  H4T-CHAR OCCURS 121 TIMES                        
00528                           INDEXED BY H4T-NDX                      
00529                              PIC X(01).                           
00530      12  HDR-5A.                                                  
00531          16  FILLER          PIC X(45)       VALUE                
00532             '            ** - - - - - - - - - - YEAR-TO-DA'.      
00533          16  FILLER          PIC X(44)       VALUE                
00534             'TE - - - - - - - - - - ** ** - - - - - - - -'.       
00535          16  FILLER          PIC X(44)       VALUE                
00536             ' -  INCEPTION-TO-DATE - - - - - - - - - - **'.       
00537      12  HDR-5B.                                                  
00538          16  FILLER          PIC X(45)       VALUE                
00539             '            ** - - - - - - - - - - YEAR-TO-DA'.      
00540          16  FILLER          PIC X(44)       VALUE                
00541             'TE - - - - - - - - - - ** ** - - - - - - - -'.       
00542          16  FILLER          PIC X(44)       VALUE                
00543             ' -  INCEPTION-TO-DATE - - - - - - - - - - **'.       
00544      12  HDR-5C.                                                  
00545          16  FILLER          PIC X(45)       VALUE                
00546             '            ** - - - - - - - - - -  YEAR-TO-D'.      
00547          16  FILLER          PIC X(44)       VALUE                
00548             'ATE  - - - - - - - - - - ** ** - - - - - - -'.       
00549          16  FILLER          PIC X(44)       VALUE                
00550             ' - -  INCEPTION-TO-DATE - - - - - - - - - **'.       
00551      12  HDR-5D.                                                  
00552          16  FILLER          PIC X(45)       VALUE                
00553             '            ** - - - - - - - - YEAR-TO-DATE -'.      
00554          16  FILLER          PIC X(44)       VALUE                
00555             ' - - - - - - -  ** ** - - - - - - - - INCEPT'.       
00556          16  FILLER          PIC X(44)       VALUE                
00557             'ION-TO-DATE - - - - - - - - **              '.       
00558      12  HDR-6A.                                                  
00559          16  FILLER          PIC X(45)       VALUE                
00560             '            NUMBER    WRITTEN     EARNED     '.      
00561          16  FILLER          PIC X(44)       VALUE                
00562             'CLAIMS     CLAIM    LOSS  NUMBER     WRITTEN'.       
00563          16  FILLER          PIC X(44)       VALUE                
00564             '      EARNED      CLAIMS      CLAIM    LOSS '.       
00565      12  HDR-6B.                                                  
00566          16  FILLER          PIC X(45)       VALUE                
00567             '               WRITTEN        EARNED       CL'.      
00568          16  FILLER          PIC X(44)       VALUE                
00569             'AIMS      CLAIM     LOSS      WRITTEN       '.       
00570          16  FILLER          PIC X(44)       VALUE                
00571             '  EARNED        CLAIMS       CLAIM    LOSS  '.       
00572      12  HDR-6C.                                                  
00573          16  FILLER          PIC X(45)       VALUE                
00574             '             NUMBER       WRITTEN         EAR'.      
00575          16  FILLER          PIC X(44)       VALUE                
00576             'NED         CLAIMS    LOSS   NUMBER       WR'.       
00577          16  FILLER          PIC X(44)       VALUE                
00578             'ITTEN         EARNED         CLAIMS    LOSS '.       
00579      12  HDR-6D.                                                  
00580          16  FILLER          PIC X(45)       VALUE                
00581             '                WRITTEN      EARNED      CLAI'.      
00582          16  FILLER          PIC X(44)       VALUE                
00583             'MS      CLAIM  LOSS     WRITTEN       EARNED'.       
00584          16  FILLER          PIC X(44)       VALUE                
00585             '       CLAIMS       CLAIM  LOSS    UNEARNED '.       
00586      12  HDR-7A.                                                  
00587          16  FILLER          PIC X(45)       VALUE                
00588             '             CERTS    PREMIUM    PREMIUM     '.      
00589          16  FILLER          PIC X(44)       VALUE                
00590             ' PAID     RESERVE  RATIO             PREMIUM'.       
00591          16  FILLER          PIC X(44)       VALUE                
00592             '     PREMIUM       PAID      RESERVE  RATIO '.       
00593      12  HDR-7B.                                                  
00594          16  FILLER          PIC X(45)       VALUE                
00595             '               PREMIUM       PREMIUM        P'.      
00596          16  FILLER          PIC X(44)       VALUE                
00597             'AID      RESERVE   RATIO      PREMIUM       '.       
00598          16  FILLER          PIC X(44)       VALUE                
00599             ' PREMIUM         PAID       RESERVE   RATIO '.       
00600      12  HDR-7C.                                                  
00601          16  FILLER          PIC X(45)       VALUE                
00602             '              CERTS       PREMIUM        PREM'.      
00603          16  FILLER          PIC X(44)       VALUE                
00604             'IUM          PAID    RATIO    CERTS       PR'.       
00605          16  FILLER          PIC X(44)       VALUE                
00606             'EMIUM        PREMIUM          PAID    RATIO '.       
00607      12  HDR-7D.                                                  
00608          16  FILLER          PIC X(45)       VALUE                
00609             '                PREMIUM     PREMIUM       PAI'.      
00610          16  FILLER          PIC X(44)       VALUE                
00611             'D      RESERVE RATIO    PREMIUM      PREMIUM'.       
00612          16  FILLER          PIC X(44)       VALUE                
00613             '        PAID       RESERVE RATIO    PREMIUM '.       
00614      12  HDR-8.                                                   
00615          16  H8-SW           PIC X           VALUE SPACES.        
00616          16  H8-DESC         PIC X(13).                           
00617                                                                   
00618      12  DTL-1A.                                                  
00619          16  FILLER          PIC X           VALUE SPACES.        
00620          16  D1-DESC.                                             
00621              20  D1-DESC1    PIC X(6).                            
00622              20  D1-DESC2    PIC X(4).                            
00623          16  D1A-Y-CERTS     PIC ZZZZ,ZZ9-.                       
00624          16  D1A-Y-NET       PIC ZZ,ZZZ,ZZ9-.                     
00625          16  D1A-Y-EARNED    PIC ZZ,ZZZ,ZZ9-.                     
00626          16  D1A-Y-PAID      PIC ZZ,ZZZ,ZZ9-.                     
00627          16  D1A-Y-RESERVE   PIC ZZ,ZZZ,ZZ9-.                     
00628          16  D1A-Y-LOSS-OV.                                       
00629              20  D1A-Y-LOSS  PIC ZZ9.9-.                          
00630          16  D1A-I-CERTS     PIC ZZZZ,ZZ9-.                       
00631          16  D1A-I-NET       PIC ZZZ,ZZZ,ZZ9-.                    
00632          16  D1A-I-EARNED    PIC ZZZ,ZZZ,ZZ9-.                    
00633          16  D1A-I-PAID      PIC ZZZ,ZZZ,ZZ9-.                    
00634          16  D1A-I-RESERVE   PIC ZZZ,ZZZ,ZZ9-.                    
00635          16  D1A-I-LOSS-OV.                                       
00636              20  D1A-I-LOSS  PIC ZZ9.9-.                          
00637      12  DTL-1B              REDEFINES DTL-1A.                    
00638          16  FILLER          PIC X(11).                           
00639          16  D1B-Y-NET       PIC ZZ,ZZZ,ZZZ.99-.                  
00640          16  D1B-Y-EARNED    PIC ZZ,ZZZ,ZZZ.99-.                  
00641          16  D1B-Y-PAID      PIC Z(5),ZZZ.99-.                    
00642          16  D1B-Y-RESERVE   PIC Z(4),ZZZ.99-.                    
00643          16  D1B-Y-LOSS-OV.                                       
00644              20  D1B-Y-LOSS  PIC ZZ9.9-.                          
00645          16  D1B-I-NET       PIC ZZZ,ZZZ,ZZZ.99-.                 
00646          16  D1B-I-EARNED    PIC ZZZ,ZZZ,ZZZ.99-.                 
00647          16  D1B-I-PAID      PIC Z(6),ZZZ.99-.                    
00648          16  D1B-I-RESERVE   PIC Z(5),ZZZ.99-.                    
00649          16  D1B-I-LOSS-OV.                                       
00650              20  D1B-I-LOSS  PIC ZZ9.9-.                          
00651      12  DTL-1C              REDEFINES DTL-1B.                    
00652          16  FILLER          PIC X(11).                           
00653          16  D1C-Y-CERTS     PIC Z,ZZZ,ZZ9-.                      
00654          16  D1C-Y-NET       PIC ZZZ,ZZZ,ZZZ.99-.                 
00655          16  D1C-Y-EARNED    PIC ZZZ,ZZZ,ZZZ.99-.                 
00656          16  D1C-Y-PAID      PIC ZZZ,ZZZ,ZZZ.99-.                 
00657          16  D1C-Y-LOSS-OV.                                       
00658              20  D1C-Y-LOSS  PIC ZZ9.9-.                          
00659          16  D1C-I-CERTS     PIC Z,ZZZ,ZZ9-.                      
00660          16  D1C-I-NET       PIC ZZZ,ZZZ,ZZZ.99-.                 
00661          16  D1C-I-EARNED    PIC ZZZ,ZZZ,ZZZ.99-.                 
00662          16  D1C-I-PAID      PIC ZZZ,ZZZ,ZZZ.99-.                 
00663          16  D1C-I-LOSS-OV.                                       
00664              20  D1C-I-LOSS  PIC ZZ9.9-.                          
00665      12  DTL-1D              REDEFINES DTL-1C.                    
00666          16  FILLER          PIC X(12).                           
00667          16  D1D-Y-NET       PIC ZZZ,ZZZ,ZZ9-.                    
00668          16  D1D-Y-EARNED    PIC ZZZ,ZZZ,ZZ9-.                    
00669          16  D1D-Y-PAID      PIC ZZZ,ZZZ,ZZ9-.                    
00670          16  D1D-Y-RESERVE   PIC ZZZ,ZZZ,ZZ9-.                    
00671          16  D1D-Y-LOSS-OV.                                       
00672              20  D1D-Y-LOSS  PIC ZZ9-.                            
00673          16  D1D-I-NET       PIC ZZZZ,ZZZ,ZZ9-.                   
00674          16  D1D-I-EARNED    PIC ZZZZ,ZZZ,ZZ9-.                   
00675          16  D1D-I-PAID      PIC ZZZZ,ZZZ,ZZ9-.                   
00676          16  D1D-I-RESERVE   PIC ZZZZ,ZZZ,ZZ9-.                   
00677          16  D1D-I-LOSS-OV.                                       
00678              20  D1D-I-LOSS  PIC ZZ9-.                            
00679          16  D1D-UNEARNED    PIC ZZZZ,ZZZ,ZZ9-.                   
00680  EJECT                                                            
00681  PROCEDURE DIVISION.                                              
00682                                                                   
unix       DISPLAY 'CURRENT ECS020T BEFORE DATE LOAD '.
00683  0000-READ-DATE.                                                  
00684                              COPY ELCDTERX.                       
00685                                                                   
00686  0010-INITIALIZATION.                                             
unix       DISPLAY 'CURRENT ECS020T '.
00687      MOVE SPACES          TO DTL-1A.                              
00688      MOVE WS-CURRENT-DATE TO H2-DATE.                             
00689      MOVE COMPANY-NAME    TO H2-COMP.                             
00690      MOVE ALPH-DATE       TO H3-DATE.                             
00691                                                                   
00692      IF CLAS-REPORT-CD1-CAPTION NOT = SPACES                      
00693          MOVE CLAS-REPORT-CD1-CAPTION TO H3B-RPT1-DESC.           
00694      IF CLAS-REPORT-CD2-CAPTION NOT = SPACES                      
00695          MOVE CLAS-REPORT-CD2-CAPTION TO H3C-RPT2-DESC.           
00696                                                                   
00697      IF DTE-PGM-OPT = 6                                           
00698          MOVE 2   TO DTE-PGM-OPT                                  
00699          MOVE 'X' TO WS-ACCT-OV-SW.                               
00700                                                                   
00701      IF DTE-FMT-OPT = '2'                                         
00702          MOVE HDR-5B TO HDR-5A                                    
00703          MOVE HDR-6B TO HDR-6A                                    
00704          MOVE HDR-7B TO HDR-7A.                                   
00705                                                                   
00706      IF DTE-FMT-OPT = '3'                                         
00707          MOVE HDR-5C TO HDR-5A                                    
00708          MOVE HDR-6C TO HDR-6A                                    
00709          MOVE HDR-7C TO HDR-7A.                                   
00710                                                                   
00711      IF DTE-FMT-OPT = '4'                                         
00712          MOVE HDR-5D TO HDR-5A                                    
00713          MOVE HDR-6D TO HDR-6A                                    
00714          MOVE HDR-7D TO HDR-7A.                                   
00715                                                                   
00716      MOVE LIFE-OVERRIDE-L1            TO L1.                      
00717                                                                   
00718      SUBTRACT 1 FROM RUN-CCYY GIVING PRV-CCYY.                    
00719                                                                   
00720      MOVE DTE-CONV-DT           TO WS-RUN-DTE-N.                  
00721                                                                   
00722      IF WS-RUN-YR = RUN-YR                                        
00723          MOVE WS-RUN-CC TO PRV-CC                                 
00724          MOVE WS-RUN-YR TO PRV-YR                                 
00725          MOVE WS-RUN-MO TO PRV-MO.                                
00726                                                                   
00727      MOVE RUN-DATE              TO WS-RUN-DTE-N.                  
00728                                                                   
00729      MOVE SPACES                TO LOSS-RATIO-MASTER.             
00730      MOVE 'LR'                  TO LR-RECORD-ID.                  
00731      MOVE LOW-VALUES            TO LR-CONTROL.                    
00732      MOVE DTE-CLASIC-COMPANY-CD TO LR-COMPANY-CD.                 
00733      MOVE WS-RUN-DTE-N          TO LR-RUN-DATE-N.                 
00734                                                                   
00735      MOVE ZEROS           TO  LR-AGT-NO (1 1)   LR-SNG-PCT (1 1)  
00736                               LR-AGT-NO (1 2)   LR-SNG-PCT (1 2)  
00737                               LR-AGT-NO (1 3)   LR-SNG-PCT (1 3)  
00738                               LR-JNT-PCT (1 1)  LR-A-H-PCT (1 1)  
00739                               LR-JNT-PCT (1 2)  LR-A-H-PCT (1 2)  
00740                               LR-JNT-PCT (1 3)  LR-A-H-PCT (1 3). 
00741      MOVE LR-ACCT-RANGES (1)    TO LR-ACCT-RANGES (2).            
00742                                                                   
00743      MOVE ZEROS             TO LR-YTD-NET (1)    LR-ITD-NET (1)   
00744                                LR-YTD-EARN (1)   LR-ITD-EARN (1)  
00745                                LR-YTD-PAID (1)   LR-ITD-PAID (1)  
00746                                LR-YTD-RESV (1)   LR-ITD-RESV (1)  
00747                                LR-YTD-INCUR (1)  LR-ITD-INCUR (1) 
00748                                LR-YTD-RATIO (1)  LR-ITD-RATIO (1).
00749      MOVE LR-TOTALS (1)         TO LR-TOTALS (2)   LR-TOTALS (3). 
00750                                                                   
00751      MOVE LOSS-RATIO-MASTER     TO LOSS-RATIO-INITIALIZED.        
00752                                                                   
00753      IF DTE-CLIENT = 'FFL'                                        
00754          MOVE +.005 TO FAC-1                                      
00755          MOVE ZEROS TO FAC-2.                                     
00756                                                                   
00757      IF DTE-CLIENT = 'FIM'                                        
00758          MOVE +.015 TO FAC-1                                      
00759          MOVE +.025 TO FAC-2.                                     
00760                                                                   
00761      MOVE WORK-ACCUMS-1 TO WORK-ACCUMS-0.                         
00762      MOVE WORK-ACCUMS-0 TO WT1-ACCUMS (1).                        
00763      MOVE SPACES        TO WT1-KEY (1).                           
00764      MOVE +2            TO X1.                                    
00765                                                                   
00766      EJECT                                                        
00767  0050-OPEN-CONTROL-FILE.                                          
00768      OPEN INPUT ELCNTL.                                           
00769                                                                   
00770      IF ELCNTL-FILE-STATUS  = '00' OR '97'                        
00771          NEXT SENTENCE                                            
00772        ELSE                                                       
00773          MOVE '**** ELCNTL OPEN ERROR ****'                       
00774                                  TO WS-ABEND-MESSAGE              
00775          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
00776          GO TO ABEND-PGM.                                         
00777                                                                   
00778  0055-READ-REPORT-RECORD.                                         
00779      MOVE SPACES                 TO CF-CONTROL-PRIMARY.           
00780      MOVE DTE-CLIENT             TO CF-COMPANY-ID.                
00781      MOVE 'C'                    TO CF-RECORD-TYPE.               
00782      MOVE PGM-SUB                TO CF-CUSTOM-REPORT-NO.          
00783      MOVE +0                     TO CF-SEQUENCE-NO.               
00784                                                                   
00785      READ ELCNTL.                                                 
00786                                                                   
00787      IF ELCNTL-FILE-STATUS NOT = '00'                             
00788          MOVE 'X'                TO WS-CUSTOMIZATION-FLAG.        
00789                                                                   
00790      EJECT                                                        
00791  0085-INIT-WORK-ZERO.                                             
00792      MOVE WT1 (1) TO WT1 (X1).                                    
00793      ADD +1 TO X1.                                                
00794                                                                   
00795      IF X1 NOT GREATER THAN TABLE-LIMIT                           
00796          GO TO 0085-INIT-WORK-ZERO.                               
00797                                                                   
00798      MOVE WORK-TOT-1                TO WORK-TOT-0.                
00799                                                                   
00800      OPEN INPUT RTBL-FILE.                                        
00801                                                                   
00802      IF REIN-FILE-STATUS = '35'                                   
00803          GO TO 0100-SET-SORT-SEQUENCE.                            
00804                                                                   
00805      IF REIN-FILE-STATUS  = '00' OR '97'                          
00806          NEXT SENTENCE                                            
00807        ELSE                                                       
00808          MOVE '2'                  TO WAC-1                       
00809          MOVE '1'                  TO WAC-2                       
00810          MOVE REIN-FILE-STATUS     TO WAC-3-4                     
00811          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              
00812          GO TO ABEND-PGM.                                         
00813                                                                   
00814      MOVE HIGH-VALUES TO WS-REIN-COMP-TBL.                        
00815                                                                   
00816  EJECT                                                            
00817  0090-READ-RTBL-FILE.                                             
00818      READ RTBL-FILE.                                              
00819                                                                   
00820      IF REIN-FILE-STATUS = '10'                                   
00821          CLOSE RTBL-FILE                                          
00822          IF REIN-FILE-STATUS = '00'                               
00823              GO TO 0100-SET-SORT-SEQUENCE                         
00824          ELSE                                                     
00825              MOVE '2'              TO WAC-1                       
00826              MOVE '2'              TO WAC-2                       
00827              MOVE REIN-FILE-STATUS TO WAC-3-4                     
00828              MOVE WORK-ABEND-CODE  TO WS-RETURN-CODE              
00829              GO TO ABEND-PGM.                                     
00830                                                                   
00831      IF REIN-FILE-STATUS NOT = '00'                               
00832          MOVE '2'               TO WAC-1                          
00833          MOVE '4'               TO WAC-2                          
00834          MOVE REIN-FILE-STATUS  TO WAC-3-4                        
00835          MOVE WORK-ABEND-CODE   TO WS-RETURN-CODE                 
00836          GO TO ABEND-PGM.                                         
00837                                                                   
00838      IF NOT RE-COMPANY-RECORD                                     
00839          GO TO 0090-READ-RTBL-FILE.                               
00840                                                                   
00841      ADD +1 TO WS-RCT-X.                                          
00842                                                                   
00843      IF WS-RCT-X GREATER THAN WS-RCT-MAX                          
00844          MOVE '0201'            TO WS-RETURN-CODE                 
00845          GO TO ABEND-PGM.                                         
00846                                                                   
00847      MOVE RE-COMPANY            TO WS-RCT-COMP (WS-RCT-X).        
00848      MOVE RE-NAME               TO WS-RCT-NAME (WS-RCT-X).        
00849      MOVE RE-CEDE-NAME          TO WS-RCT-CEDE (WS-RCT-X).        
00850                                                                   
00851      IF RE-LF-IBNR-PCT NOT NUMERIC  MOVE ZEROS TO RE-LF-IBNR-PCT. 
00852      IF RE-AH-IBNR-PCT NOT NUMERIC  MOVE ZEROS TO RE-AH-IBNR-PCT. 
00853      IF RE-LF-PR-PCT   NOT NUMERIC  MOVE ZEROS TO RE-LF-PR-PCT.   
00854      IF RE-LF-78-PCT   NOT NUMERIC  MOVE ZEROS TO RE-LF-78-PCT.   
00855      IF RE-AH-PR-PCT   NOT NUMERIC  MOVE ZEROS TO RE-AH-PR-PCT.   
00856      IF RE-AH-78-PCT   NOT NUMERIC  MOVE ZEROS TO RE-AH-78-PCT.   
00857                                                                   
00858      MOVE RE-LF-IBNR-PCT        TO WS-LF-IBNR (WS-RCT-X).         
00859      MOVE RE-AH-IBNR-PCT        TO WS-AH-IBNR (WS-RCT-X).         
00860                                                                   
00861      MOVE RE-LF-PR-PCT          TO WS-LF-PRO (WS-RCT-X).          
00862      MOVE RE-LF-78-PCT          TO WS-LF-R78 (WS-RCT-X).          
00863      MOVE RE-AH-PR-PCT          TO WS-AH-PRO (WS-RCT-X).          
00864      MOVE RE-AH-78-PCT          TO WS-AH-R78 (WS-RCT-X).          
00865                                                                   
00866      GO TO 0090-READ-RTBL-FILE.                                   
00867  EJECT                                                            
00868  0100-SET-SORT-SEQUENCE.                                          
00869                                                                   
00870          SORT SORT-WORK ON ASCENDING  SW-REIN                     
JJPMOD                                      SW-REIN-NAME                
00871                                       SW-REINCO                   
00872                                       SW-CONTROL                  
00873                                       SW-REINCO-SUB               
00874                                       SW-RCD-TYPE                 
00875                                       SW-BEN-CODE                 
00876                                       SW-RUN-DTE                  
00877                            DESCENDING SW-PURGE                    
00878          INPUT PROCEDURE  0200-SORT-INPUT THRU 0299-EXIT          
00879          OUTPUT PROCEDURE 0300-PRINT-EPL  THRU 2999-EXIT.         
00880                                                                   
00881      IF SORT-RETURN NOT = ZEROS                                   
00882          MOVE '0101' TO WS-RETURN-CODE                            
00883          GO TO ABEND-PGM.                                         
00884                                                                   
00885      GO TO 9999-END-OF-JOB.                                       
00886  EJECT                                                            
00887  0200-SORT-INPUT SECTION.                                         
00888                                                                   
00889      OPEN INPUT EXTRACTS  ACCT-MASTER.                            
00890                                                                   
00891      IF AM-FILE-STATUS  = '00' OR '97'                            
00892          NEXT SENTENCE                                            
00893        ELSE                                                       
00894          MOVE '1'                  TO WAC-1                       
00895          MOVE '1'                  TO WAC-2                       
00896          MOVE AM-FILE-STATUS       TO WAC-3-4                     
00897          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              
00898          GO TO ABEND-PGM.                                         
00899                                                                   
00900  0210-READ-EXTRACTS.                                              
00901      READ EXTRACTS                                                
00902                AT END CLOSE EXTRACTS                              
unix                    DISPLAY ' EXTRACTS AT END '
00903                       GO TO 0299-EXIT.                            
00904                                                                   
00905                                                                   
00906  0230-SELECT-EXTRACTS.                                            
00907                                                                   
00908      IF EPR-REC-ID NOT = 'EP'                                     
00909          GO TO 0210-READ-EXTRACTS.                                
00910                                                                   
00911      MOVE EPR-RUN-DATE TO WS-EPR-RUN-DATE.                        
00912                                                                   
00913      IF EPR-PURGE     NOT = 'P'          AND                      
00914         EPR-RUN-DATE  NOT = WS-RUN-DTE-N AND                      
00915         EPR-RUN-YR-MO NOT = PRV-DEC                               
00916           GO TO 0210-READ-EXTRACTS.                               
00917                                                                   
00918      IF EPR-PURGE = 'P'                                           
00919          AND EPR-RUN-DATE GREATER THAN WS-RUN-DTE-N               
00920              GO TO 0210-READ-EXTRACTS.                            
00921                                                                   
00922      MOVE SPACE                            TO SW-ACCT-FLAG.       
00923      IF CUSTOMIZATION-FOUND                                       
00924          IF CF-ACTIVE-ACCOUNTS                                    
00925              MOVE DTE-CLASIC-COMPANY-CD    TO AM-COMPANY-CD       
00926              MOVE EPR-CONTROL              TO AM-MSTR-CNTRL       
00927              PERFORM 2700-READ-AM-MSTR THRU 2700-EXIT             
00928              IF NOT AM-ACCOUNT-ACTIVE                             
00929                  MOVE 'X'                  TO SW-ACCT-FLAG.       
00930                                                                   
00931  0250-RELEASE-EXTRACTS.                                           
00932      IF EPR-PURGE NOT = 'P'                                       
00933          IF EPR-RUN-DATE = WS-RUN-DTE-N                           
00934              MOVE 'A' TO EPR-PURGE                                
00935          ELSE                                                     
00936              IF EPR-RUN-YR-MO = PRV-DEC                           
00937                  MOVE 'S' TO EPR-PURGE.                           
00938                                                                   
00939      IF EPR-PURGE = 'P'                                           
00940          AND EPR-RUN-YR-MO GREATER THAN PRV-DEC                   
00941              MOVE 'Q' TO EPR-PURGE.                               
00942                                                                   
00943      IF EPR-RCD-TYPE = AH-OVERRIDE-L1                             
00944          MOVE 'Z'                  TO EPR-RCD-TYPE.               
00945                                                                   
00946      MOVE LOW-VALUES               TO SW-CONTROL.                 
JJPMOD     MOVE LOW-VALUES               TO SW-REIN-NAME.               
JJPMOD     MOVE LOW-VALUES               TO SV-X-REIN-COMP.             
00947      MOVE EPR-CARRIER              TO SW-CARRIER.                 
00948      MOVE EPR-GROUPING             TO SW-GROUPING.                
00949      MOVE EPR-STATE                TO SW-STATE.                   
00950      MOVE EPR-ACCOUNT              TO SW-ACCOUNT.                 
00951      MOVE EPR-EXP-DTE              TO SW-EXP-DATE.                
00952      MOVE EPR-EFF-DTE              TO SW-EFF-DATE.                
00953                                                                   
00954      MOVE EPR-RECORD               TO SW-EP-RECORD.               
00955                                                                   
00956      IF SW-REIN NOT = SPACE                                       
00957          MOVE 'R'                  TO SW-REIN                     
UNIX           PERFORM 0300-RELEASE-REC  THRU 0300-REXIT
00958 *        RELEASE SORT-RECORD
JJPMOD         MOVE 'X'                  TO SW-REIN                     
JJPMOD         PERFORM 2250-FIND-REIN-NAME THRU 2270-EXIT               
UNIX           PERFORM 0300-RELEASE-REC  THRU 0300-REXIT
00958 *        RELEASE SORT-RECORD
00959          GO TO 0210-READ-EXTRACTS.                                
00960                                                                   
00961      MOVE 'A'                      TO SW-REIN.                    
00962      MOVE LOW-VALUES               TO SW-REI-CO.                  
JJPMOD     MOVE LOW-VALUES               TO SW-REIN-NAME.               
UNIX       PERFORM 0300-RELEASE-REC  THRU 0300-REXIT
00958 *    RELEASE SORT-RECORD
00964                                                                   
00965      MOVE DTE-CLASIC-COMPANY-CD    TO AM-COMPANY-CD.              
00966      MOVE EPR-CONTROL              TO AM-MSTR-CNTRL.              
00967      PERFORM 2700-READ-AM-MSTR THRU 2700-EXIT.                    
00968                                                                   
00969      IF DTE-TOT-OPT = '5' OR '6' OR '7' OR '8'                    
00970          GO TO 0280-RELEASE-RPTCD-EXTRACTS.                       
00971                                                                   
00972      MOVE 'G'                      TO SW-REIN.                    
00973      MOVE +1 TO X1.                                               
00974                                                                   
00975  0270-RELEASE-GA-EXTRACTS.                                        
00976                                                                   
00977      IF AM-COM-TYP (X1) = 'O'  OR  'P'                            
00978          MOVE AM-AGT (X1)          TO SW-RPT-CD-2                 
UNIX           PERFORM 0300-RELEASE-REC  THRU 0300-REXIT
00958 *        RELEASE SORT-RECORD
UNIX       END-IF
00980
00981      ADD +1 TO X1.                                                
00982      IF X1 LESS THAN +11                                          
00983          GO TO 0270-RELEASE-GA-EXTRACTS.                          
00984                                                                   
00985  0280-RELEASE-RPTCD-EXTRACTS.                                     
00986                                                                   
00987      IF DTE-TOT-OPT = '2' OR '4' OR '6' OR '8'                    
00988          IF AM-REPORT-CODE-1 NOT = SPACES                         
00989              MOVE 'B'              TO SW-REIN                     
00990              MOVE AM-REPORT-CODE-1 TO SW-RPT-CD-1                 
00991              MOVE LOW-VALUES       TO SW-RPT-CD-2                 
UNIX               PERFORM 0300-RELEASE-REC  THRU 0300-REXIT
00958 *            RELEASE SORT-RECORD
UNIX           END-IF
           END-IF
00993
00994                                                                   
00995      IF DTE-TOT-OPT = '3' OR '4' OR '7' OR '8'                    
00996          IF AM-REPORT-CODE-2 NOT = SPACES                         
00997              MOVE 'C'              TO SW-REIN                     
00998              MOVE LOW-VALUES       TO SW-RPT-CD-1                 
00999              MOVE AM-REPORT-CODE-2 TO SW-RPT-CD-2                 
UNIX               PERFORM 0300-RELEASE-REC  THRU 0300-REXIT
00958 *            RELEASE SORT-RECORD
UNIX           END-IF
           END-IF
01001                                                                   
01002                                                                   
01003      GO TO 0210-READ-EXTRACTS.                                    
01004                                                                   
01005  0299-EXIT.                                                       
01006      EXIT.                                                        
01007                                                                   
01008  EJECT                                                            
UNIX   0300-RELEASE-REC SECTION.

           MOVE SORT-RECORD            TO WS-HOLD-SORT-RECORD
           IF SW-BEN-CODE = SPACES
              DISPLAY ' BEN CODE SPACES ' SW-REIN '  ' SW-CONTROL
           END-IF
           IF SW-BEN-CODE = LOW-VALUES
              DISPLAY ' BEN CODE LV ' SW-REIN '  ' SW-CONTROL
           END-IF
           IF SW-BEN-CODE = HIGH-VALUES
              DISPLAY ' BEN CODE HV ' SW-REIN '  ' SW-CONTROL
           END-IF
           IF SW-BEN-CODE = ZEROS
              DISPLAY ' BEN CODE 00 ' SW-REIN '  ' SW-CONTROL
           END-IF
           RELEASE SORT-RECORD
           ADD 1    TO WS-RELEASE-CNT
           MOVE WS-HOLD-SORT-RECORD    TO SORT-RECORD
           .
       0300-REXIT.
UNIX       EXIT.

01009  0300-PRINT-EPL SECTION.
01010                                                                   
unix       DISPLAY ' 0300-PRINT '.
UNIX       DISPLAY ' RECORDS RELEASED ' WS-RELEASE-CNT
061102     OPEN OUTPUT PRINTER-OUTPUT
01012                  LOSS-RATIOS.                                     
01013                                                                   
01014      PERFORM 0390-INITIALIZE-ACCUMS THRU 0390-EXIT.               
01015                                                                   
01016      PERFORM 0383-RETURN-EXTRACTS.                                
01017                                                                   
01018      IF SW-CONTROL = HIGH-VALUES                                  
01019          DISPLAY ' NO EXTRACT RECORDS'                            
01020          MOVE '0701' TO WS-RETURN-CODE                            
01021          GO TO ABEND-PGM.                                         
01022                                                                   
01023      MOVE SW-REIN                   TO SV-REIN                    
01024                                        H1-SUFFIX.                 
01025      MOVE SW-CONTROL                TO SV-CONTROL.                
01026      MOVE SW-REI-CO                 TO SV-REI-CO.                 
01027      MOVE SW-ACCT-FLAG              TO SV-ACCT-FLAG.              
JJPMOD     MOVE SW-REIN-NAME              TO SV-X-REIN-NAME.            
01028                                                                   
01029      GO TO 0330-CHECK-EOF.                                        
01030                                                                   
01031  0320-CHECK-CONTROL.                                              
01032      IF SW-REIN NOT = SV-REIN                                     
01033          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01034          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01035          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01036          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    
01037          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    
01038          PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    
01039          PERFORM 1750-REIN-PRT  THRU 1759-EXIT                    
01040          PERFORM 1770-RPT1-PRT  THRU 1799-EXIT                    
01041          PERFORM 1800-FINAL-PRT THRU 1899-EXIT                    
01042          PERFORM 0390-INITIALIZE-ACCUMS THRU 0390-EXIT            
01043          MOVE SW-REIN TO H1-SUFFIX                                
01044          MOVE +0  TO PAGER                                        
01045          MOVE +99 TO LINER
070102         MOVE SPACE TO LOSS-RATIO-SW
CIDMOD                                                                  
070102**    SKIP 1 ADDITIONAL PAGE BETWEEN THE HARDCOPY REPORTS
070102**    CURRENTLY ONLY ECS020B AND ECS020C
070102*        IF SW-REIN = 'C'
070102*           WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE
070102*        END-IF
CIDMOD         GO TO 0330-CHECK-EOF
CIDMOD     END-IF.                                                      
01047                                                                   
01048      IF SW-RPT-CD-1 NOT = SV-RPT-CD-1                             
01049          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01050          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01051          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01052          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    
01053          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    
01054          PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    
01055          PERFORM 1750-REIN-PRT  THRU 1759-EXIT                    
01056          PERFORM 1770-RPT1-PRT  THRU 1799-EXIT                    
01057          GO TO 0330-CHECK-EOF.                                    
01058                                                                   
JJPMOD     IF SW-REIN  =  'X'                                           
JJPMOD         IF SW-REIN-NAME NOT = SV-X-REIN-NAME                     
JJPMOD             PERFORM 1300-DATE-PRT  THRU 1399-EXIT                
JJPMOD             PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                
JJPMOD             PERFORM 1500-STATE-PRT THRU 1549-EXIT                
JJPMOD             PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                
JJPMOD             PERFORM 1600-COMP-PRT  THRU 1699-EXIT                
JJPMOD             PERFORM 1700-CARR-PRT  THRU 1749-EXIT                
JJPMOD             PERFORM 1750-REIN-PRT  THRU 1759-EXIT                
JJPMOD             GO TO 0330-CHECK-EOF                                 
JJPMOD         END-IF                                                   
JJPMOD     ELSE                                                         
JJPMOD         IF SW-REINCO NOT = SV-REINCO                             
JJPMOD             PERFORM 1300-DATE-PRT  THRU 1399-EXIT                
JJPMOD             PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                
JJPMOD             PERFORM 1500-STATE-PRT THRU 1549-EXIT                
JJPMOD             PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                
JJPMOD             PERFORM 1600-COMP-PRT  THRU 1699-EXIT                
JJPMOD             PERFORM 1700-CARR-PRT  THRU 1749-EXIT                
JJPMOD             PERFORM 1750-REIN-PRT  THRU 1759-EXIT                
JJPMOD             GO TO 0330-CHECK-EOF                                 
JJPMOD         END-IF                                                   
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
01059 **   IF SW-REINCO NOT = SV-REINCO                                 
01060 **       PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01061 **       PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01062 **       PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01063 **       PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    
01064 **       PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    
01065 **       PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    
01066 **       PERFORM 1750-REIN-PRT  THRU 1759-EXIT                    
01067 **       GO TO 0330-CHECK-EOF.                                    
01068                                                                   
01069      IF SW-CARRIER NOT = SV-CARRIER                               
01070          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01071          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01072          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01073          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    
01074          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    
01075          PERFORM 1700-CARR-PRT  THRU 1749-EXIT                    
01076          GO TO 0330-CHECK-EOF.                                    
01077                                                                   
01078      IF SW-GROUPING NOT = SV-GROUPING                             
01079          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01080          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01081          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01082          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    
01083          PERFORM 1600-COMP-PRT  THRU 1699-EXIT                    
01084          GO TO 0330-CHECK-EOF.                                    
01085                                                                   
01086      IF SW-RPT-CD-2 NOT = SV-RPT-CD-2                             
01087          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01088          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01089          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01090          PERFORM 1550-RPT2-PRT  THRU 1599-EXIT                    
01091          GO TO 0330-CHECK-EOF.                                    
01092                                                                   
01093      IF SW-STATE NOT = SV-STATE                                   
01094          PERFORM 1300-DATE-PRT  THRU 1399-EXIT                    
01095          PERFORM 1400-ACCT-PRT  THRU 1499-EXIT                    
01096          PERFORM 1500-STATE-PRT THRU 1549-EXIT                    
01097          GO TO 0330-CHECK-EOF.                                    
01098                                                                   
01099      IF SW-ACCOUNT NOT = SV-ACCOUNT                               
01100          PERFORM 1300-DATE-PRT THRU 1399-EXIT                     
01101          PERFORM 1400-ACCT-PRT THRU 1499-EXIT                     
01102          GO TO 0330-CHECK-EOF.                                    
01103                                                                   
01104      IF SW-EXP-DATE   NOT = SV-EXP-DTE  OR                        
01105         SW-EFF-DATE   NOT = SV-EFF-DTE  OR                        
01106         SW-REINCO-SUB NOT = SV-REINCO-SUB                         
01107            PERFORM 1300-DATE-PRT THRU 1399-EXIT.                  
01108                                                                   
01109  0330-CHECK-EOF.                                                  
01110      IF SW-CONTROL = HIGH-VALUES                                  
01111          GO TO 2999-EXIT.                                         
01112                                                                   
01113      MOVE SW-EP-RECORD                TO EP-RECORD.               
01114                                                                   
01115      COPY ELCEPCM1.                                               
01116                                                                   
01117      IF EP-REIN NOT = 'R'                                         
01118          MOVE SPACES TO H3-REIN                                   
01119                         H3-REIN-COMP                              
01120                         H3-REIN-NAME.                             
01121                                                                   
01122      MOVE DATE-TOTS   TO WORK-TOT-1.                              
01123      MOVE EP-REIN     TO KL-REIN.                                 
01124      MOVE EP-RCD-TYPE TO KL-LF-AH.                                
01125                                                                   
01126      IF EP-BEN-CODE NOT = ZERO                                    
01127          MOVE EP-BEN-CODE TO KL-BEN-TYP                           
01128      ELSE                                                         
01129          DISPLAY 'ZERO BENEFIT TYPE INVALID ON EPEC FILE '        
01130                        EP-CONTROL                                 
01131          MOVE '0301' TO WS-RETURN-CODE                            
01132          GO TO ABEND-PGM.                                         
01133                                                                   
01134      MOVE +1 TO X1.                                               
01135                                                                   
01136  0340-FIND-WT1-KEY.                                               
01137      IF WT1-KEY (X1) = SPACES   OR                                
01138         KEY-LOOK     = WT1-KEY (X1)                               
01139              MOVE WT1-ACCUMS (X1) TO WORK-ACCUMS-1                
01140              MOVE KEY-LOOK        TO WT1-KEY (X1)                 
01141      ELSE                                                         
01142          ADD +1 TO X1                                             
01143          IF X1 NOT GREATER THAN TABLE-LIMIT                       
01144              GO TO 0340-FIND-WT1-KEY                              
01145          ELSE                                                     
01146              DISPLAY 'DETAIL ACCUMULATOR TABLE FULL '             
01147                        EP-CONTROL                                 
01148              MOVE '0202' TO WS-RETURN-CODE                        
01149              GO TO ABEND-PGM.                                     
01150                                                                   
01151      PERFORM 1200-FIND-BEN-CODE THRU 1299-EXIT.                   
01152                                                                   
01153      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
01154                                                                   
01155  0350-STATE-LOOKUP.                                               
01156      IF EP-STATE NOT = STATE-SUB (CLAS-INDEXS)                    
01157          IF CLAS-INDEXS NOT = CLAS-MAXS                           
01158              ADD +1 TO CLAS-INDEXS                                
01159              GO TO 0350-STATE-LOOKUP.                             
01160                                                                   
01161      IF DTE-CLIENT = 'FIM'  AND                                   
01162         EP-REIN    = 'R'    AND                                   
01163         EP-REI-CO NOT = WS-LAST-REIN-CO                           
01164          MOVE +1 TO WS-RCT-X                                      
01165      ELSE                                                         
01166          GO TO 0380-ACCUMULATE-TOTALS.                            
01167                                                                   
01168  0360-FIND-REIN-IBNR-PCT.                                         
01169      IF WS-RCT-X GREATER THAN WS-RCT-MAX OR                       
01170         WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      
01171          MOVE ZEROS TO FAC-3  FAC-4                               
01172          GO TO 0380-ACCUMULATE-TOTALS.                            
01173                                                                   
01174      IF EP-REI-CO NOT = WS-RCT-COMP (WS-RCT-X)                    
01175          ADD +1 TO WS-RCT-X                                       
01176          GO TO 0360-FIND-REIN-IBNR-PCT.                           
01177                                                                   
01178      MOVE WS-LF-IBNR (WS-RCT-X) TO FAC-3.                         
01179      MOVE WS-AH-IBNR (WS-RCT-X) TO FAC-4.                         
01180                                                                   
01181      MOVE WS-LF-PRO (WS-RCT-X)  TO LF-PRO.                        
01182      MOVE WS-LF-R78 (WS-RCT-X)  TO LF-R78.                        
01183      MOVE WS-AH-PRO (WS-RCT-X)  TO AH-PRO.                        
01184      MOVE WS-AH-R78 (WS-RCT-X)  TO AH-R78.                        
01185                                                                   
01186      MOVE EP-REI-CO TO WS-LAST-REIN-CO.                           
01187                                                                   
01188  0380-ACCUMULATE-TOTALS.                                          
01189                                                                   
01190      IF EP-PRM-78-ADJ NOT NUMERIC                                 
01191          MOVE ZEROS             TO EP-PRM-78-ADJ.                 
01192      IF EP-PRM-PR-ADJ NOT NUMERIC                                 
01193          MOVE ZEROS             TO EP-PRM-PR-ADJ.                 
01194      IF EP-PRM-ST-ADJ NOT NUMERIC                                 
01195          MOVE ZEROS             TO EP-PRM-ST-ADJ.                 
01196                                                                   
01197      COMPUTE TT-PRM-78 = EP-PRM-78 + EP-PRM-78-ADJ.               
01198      COMPUTE TT-PRM-PR = EP-PRM-PR + EP-PRM-PR-ADJ.               
01199      COMPUTE TT-PRM-ST = EP-PRM-ST + EP-PRM-ST-ADJ.               
01200                                                                   
01201      IF EP-PURGE = 'P'                                            
01202          PERFORM 1900-ITD-ADD THRU 1999-EXIT.                     
01203                                                                   
01204      IF EP-PURGE = ('A' OR 'Q')                                   
01205          PERFORM 1900-ITD-ADD THRU 1999-EXIT                      
01206          PERFORM 2000-YTD-ADD THRU 2099-EXIT.                     
01207                                                                   
01208      IF EP-PURGE = 'S'                                            
01209          PERFORM 2100-YTD-SUB THRU 2199-EXIT.                     
01210                                                                   
01211      MOVE WORK-ACCUMS-1 TO WT1-ACCUMS (X1).                       
01212      MOVE WORK-TOT-1    TO DATE-TOTS.                             
01213                                                                   
01214      MOVE SW-REIN                      TO SV-REIN.                
01215      MOVE SW-CONTROL                   TO SV-CONTROL.             
01216      MOVE SW-REI-CO                    TO SV-REI-CO.              
01217      MOVE SW-ACCT-FLAG                 TO SV-ACCT-FLAG.           
JJPMOD     MOVE SW-REIN-NAME                 TO SV-X-REIN-NAME.         
01218                                                                   
01219  0383-RETURN-EXTRACTS.                                            
01220      RETURN SORT-WORK AT END
UNIX         DISPLAY ' RETURN AT END '
01221          MOVE HIGH-VALUES TO SORT-RECORD
           END-RETURN
UNIX       IF SORT-RECORD NOT = HIGH-VALUES
              ADD 1 TO WS-RETURN-CNT
           END-IF
           IF WS-RETURN-CNT > 838850
              DISPLAY ' RETURNED SO FAR ' WS-RETURN-CNT
              DISPLAY ' STUFF ' SW-REIN '  ' SW-CONTROL 
           END-IF
           IF WS-RETURN-CNT = WS-RELEASE-CNT
              DISPLAY ' RETURNS = RELEASES '
           END-IF
unix       IF SW-BEN-CODE = SPACES
              DISPLAY ' RET BEN CODE SPACES ' SW-REIN '  ' SW-CONTROL
           END-IF
           IF SW-BEN-CODE = LOW-VALUES
              DISPLAY ' RET BEN CODE LV ' SW-REIN '  ' SW-CONTROL
              DISPLAY ' RETURN COUNT  ' WS-RETURN-CNT
              DISPLAY ' PREV REC = ' WS-HOLD-SORT-RECORD
           END-IF
           IF SW-BEN-CODE = HIGH-VALUES
              DISPLAY ' RET BEN CODE HV ' SW-REIN '  ' SW-CONTROL
           END-IF
           IF SW-BEN-CODE = ZEROS
              DISPLAY ' RET BEN CODE 00 ' SW-REIN '  ' SW-CONTROL
           END-IF
           MOVE SORT-RECORD    TO WS-HOLD-SORT-RECORD
01222      .
01223  0389-RETURN-TO-CHECK.                                            
01224      GO TO 0320-CHECK-CONTROL.                                    
01225                                                                   
01226  0390-INITIALIZE-ACCUMS.                                          
01227      MOVE WORK-TOT-0                TO DATE-TOTS                  
01228                                        ACCT-TOTS                  
01229                                        ST-TOTS                    
01230                                        RPT2-TOTS                  
01231                                        COMP-TOTS                  
01232                                        CARR-TOTS                  
01233                                        REIN-TOTS                  
01234                                        RPT1-TOTS                  
01235                                        FINL-TOTS                  
01236                                        WORK-TOT-1                 
01237                                        WORK-TOT-2.                
01238                                                                   
01239      MOVE WORK-ACCUMS-0             TO WORK-ACCUMS-1              
01240                                        WORK-ACCUMS-2              
01241                                        BEN-TOTALS                 
01242                                        TYP-TOTALS.                
01243      MOVE ZEROS                     TO WRK-RESV.                  
01244                                                                   
01245  0390-EXIT.                                                       
01246      EXIT.                                                        
01247  EJECT                                                            
01248  0400-PRINT-RTN.                                                  
01249      MOVE LOSS-RATIO-INITIALIZED   TO LOSS-RATIO-MASTER.          
01250                                                                   
01251      MOVE +1            TO X1.                                    
01252      MOVE 'X'           TO BENEFIT-PRT-SW.                        
01253      MOVE WT1-LF-AH (1) TO SAVE-PR-LF-AH.                         
01254                                                                   
01255  0410-PR-LOOP.                                                    
01256      IF WT1-KEY (X1) = SPACES                                     
01257          PERFORM 1000-PR-BEN-TOT THRU 1099-EXIT                   
01258          PERFORM 1100-PR-TYP-TOT THRU 1199-EXIT                   
01259          GO TO 0499-EXIT.                                         
01260                                                                   
01261      IF WT1-LF-AH (X1) NOT = SAVE-PR-LF-AH                        
01262          PERFORM 1000-PR-BEN-TOT THRU 1099-EXIT.                  
01263                                                                   
01264      MOVE WT1-LF-AH  (X1) TO SAVE-PR-LF-AH.                       
01265      MOVE WT1-ACCUMS (X1) TO WORK-ACCUMS-1.                       
01266                                                                   
01267      PERFORM 0900-CHECK-ZEROS THRU 0999-EXIT.                     
01268                                                                   
01269      IF SV-ACCT-FLAG = 'X'                                        
01270          IF SV-REIN = 'R'                                         
01271 **           PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT           
JJPMOD             PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT           
01272              GO TO 0420-SKIP-PRINT                                
01273            ELSE                                                   
01274              GO TO 0420-SKIP-PRINT.                               
01275                                                                   
01276      IF ALL-AMOUNTS-ZERO                                          
01277          GO TO 0420-SKIP-PRINT.                                   
01278                                                                   
           IF  LINER GREATER THAN +50
01280          MOVE 'X'                TO BENEFIT-PRT-SW                
01281                                                                   
01282          IF  W-TOTALS-BEING-PROCESSED                             
01283              PERFORM 2350-HDR-RTN-T THRU 2350-EXIT                
01284              PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT      
01285                                                                   
01286          ELSE                                                     
01287              PERFORM 2300-HDR-RTN-A THRU 2300-EXIT.               
01288                                                                   
01289      IF  PRINTING-1ST-BENEFIT                                     
01290          MOVE SPACE              TO BENEFIT-PRT-SW                
01291                                                                   
01292          IF  W-TOTALS-BEING-PROCESSED                             
01293              PERFORM 2420-HDR-RTN-T THRU 2420-EXIT                
01294                                                                   
01295          ELSE                                                     
01296              PERFORM 2400-HDR-RTN-B THRU 2400-EXIT.               
01297                                                                   
01298      ADD +1 TO PR-BEN-CTR                                         
01299                PR-TYP-CTR.                                        
01300                                                                   
01301      PERFORM 1200-FIND-BEN-CODE THRU 1299-EXIT.                   
01302                                                                   
01303      IF WT1-LF-AH (X1) = L1                                       
01304          MOVE CLAS-I-AB10 (CLAS-INDEXL) TO D1-DESC                
01305      ELSE                                                         
01306          MOVE CLAS-I-AB10 (CLAS-INDEXA) TO D1-DESC.               
01307                                                                   
01308      PERFORM 0600-SET-UP-DTL-PRT THRU 0699-EXIT.                  
01309                                                                   
01310      MOVE ' ' TO X.                                               
           ADD +1 TO LINER.
01312                                                                   
01313      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
01314                                                                   
01315  0420-SKIP-PRINT.                                                 
01316      MOVE BEN-TOTALS TO WORK-ACCUMS-2.                            
01317                                                                   
01318      PERFORM 0500-ADD-RTN THRU 0599-EXIT.                         
01319                                                                   
01320      MOVE WORK-ACCUMS-2 TO BEN-TOTALS.                            
01321      MOVE TYP-TOTALS TO WORK-ACCUMS-2.                            
01322                                                                   
01323      PERFORM 0500-ADD-RTN THRU 0599-EXIT.                         
01324                                                                   
01325      MOVE WORK-ACCUMS-2 TO TYP-TOTALS.                            
01326      ADD +1 TO X1.                                                
01327                                                                   
01328      GO TO 0410-PR-LOOP.                                          
01329                                                                   
01330  0499-EXIT.                                                       
01331      EXIT.                                                        
01332                                                                   
01333  0500-ADD-RTN.                                                    
01334      ADD WA1-Y-CERT TO WA2-Y-CERT.                                
01335      ADD WA1-Y-NET  TO WA2-Y-NET.                                 
01336      ADD WA1-Y-EARN TO WA2-Y-EARN.                                
01337      ADD WA1-Y-PAID TO WA2-Y-PAID.                                
01338      ADD WA1-Y-RESV TO WA2-Y-RESV.                                
01339      ADD WA1-I-CERT TO WA2-I-CERT.                                
01340      ADD WA1-I-NET  TO WA2-I-NET.                                 
01341      ADD WA1-I-EARN TO WA2-I-EARN.                                
01342      ADD WA1-I-PAID TO WA2-I-PAID.                                
01343      ADD WA1-I-RESV TO WA2-I-RESV.                                
01344                                                                   
01345  0599-EXIT.                                                       
01346      EXIT.                                                        
01347  EJECT                                                            
01348  0600-SET-UP-DTL-PRT.                                             
01349      IF WA1-Y-EARN NOT = ZERO                                     
01350          IF DTE-CLIENT = 'FMK' OR 'FLI' OR 'FLU'                  
01351              COMPUTE WS-Y-LOSS ROUNDED = WA1-Y-PAID / WA1-Y-EARN  
01352          ELSE                                                     
01353              COMPUTE WS-Y-LOSS ROUNDED =                          
01354                       (WA1-Y-PAID + WA1-Y-RESV) / WA1-Y-EARN      
01355      ELSE                                                         
01356          MOVE ZEROS TO WS-Y-LOSS.                                 
01357                                                                   
01358      IF WA1-I-EARN NOT = ZERO                                     
01359          IF DTE-CLIENT = 'FMK' OR 'FLI' OR 'FLU'                  
01360              COMPUTE WS-I-LOSS ROUNDED = WA1-I-PAID / WA1-I-EARN  
01361          ELSE                                                     
01362              COMPUTE WS-I-LOSS ROUNDED =                          
01363                       (WA1-I-PAID + WA1-I-RESV) / WA1-I-EARN      
01364      ELSE                                                         
01365          MOVE ZEROS TO WS-I-LOSS.                                 
01366                                                                   
01367      IF DTE-FMT-OPT = '2'                                         
01368          GO TO 0620-PRT-OPTION-2.                                 
01369                                                                   
01370      IF DTE-FMT-OPT = '3'                                         
01371          GO TO 0630-PRT-OPTION-3.                                 
01372                                                                   
01373      IF DTE-FMT-OPT = '4'                                         
01374          GO TO 0640-PRT-OPTION-4.                                 
01375                                                                   
01376  0610-PRT-OPTION-1.                                               
01377      MOVE WA1-Y-CERT TO D1A-Y-CERTS.                              
01378      MOVE WA1-Y-NET  TO D1A-Y-NET.                                
01379      MOVE WA1-Y-EARN TO D1A-Y-EARNED.                             
01380      MOVE WA1-Y-PAID TO D1A-Y-PAID.                               
01381      MOVE WA1-Y-RESV TO D1A-Y-RESERVE.                            
01382                                                                   
01383      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   
01384          MOVE '******' TO D1A-Y-LOSS-OV                           
01385          MOVE 'Y'      TO LOSS-RATIO-SW                           
01386      ELSE                                                         
01387          MULTIPLY WS-Y-LOSS BY +100 GIVING D1A-Y-LOSS.            
01388                                                                   
01389      MOVE WA1-I-CERT TO D1A-I-CERTS.                              
01390      MOVE WA1-I-NET  TO D1A-I-NET.                                
01391      MOVE WA1-I-EARN TO D1A-I-EARNED.                             
01392      MOVE WA1-I-PAID TO D1A-I-PAID.                               
01393      MOVE WA1-I-RESV TO D1A-I-RESERVE.                            
01394                                                                   
01395      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   
01396          MOVE '******' TO D1A-I-LOSS-OV                           
01397          MOVE 'Y'      TO LOSS-RATIO-SW                           
01398      ELSE                                                         
01399          MULTIPLY WS-I-LOSS BY +100 GIVING D1A-I-LOSS.            
01400                                                                   
01401      MOVE DTL-1A TO PRT.                                          
01402                                                                   
01403      GO TO 0699-EXIT.                                             
01404                                                                   
01405  0620-PRT-OPTION-2.                                               
01406      MOVE WA1-Y-NET  TO D1B-Y-NET.                                
01407      MOVE WA1-Y-EARN TO D1B-Y-EARNED.                             
01408      MOVE WA1-Y-PAID TO D1B-Y-PAID.                               
01409      MOVE WA1-Y-RESV TO D1B-Y-RESERVE.                            
01410                                                                   
01411      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   
01412          MOVE '******' TO D1B-Y-LOSS-OV                           
01413          MOVE 'Y'      TO LOSS-RATIO-SW                           
01414      ELSE                                                         
01415          MULTIPLY WS-Y-LOSS BY +100 GIVING D1B-Y-LOSS.            
01416                                                                   
01417      MOVE WA1-I-NET  TO D1B-I-NET.                                
01418      MOVE WA1-I-EARN TO D1B-I-EARNED.                             
01419      MOVE WA1-I-PAID TO D1B-I-PAID.                               
01420      MOVE WA1-I-RESV TO D1B-I-RESERVE.                            
01421                                                                   
01422      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   
01423          MOVE '******' TO D1B-I-LOSS-OV                           
01424          MOVE 'Y'      TO LOSS-RATIO-SW                           
01425      ELSE                                                         
01426          MULTIPLY WS-I-LOSS BY +100 GIVING D1B-I-LOSS.            
01427                                                                   
01428      MOVE DTL-1A TO PRT.                                          
01429                                                                   
01430      GO TO 0699-EXIT.                                             
01431                                                                   
01432  0630-PRT-OPTION-3.                                               
01433      MOVE WA1-Y-CERT TO D1C-Y-CERTS.                              
01434      MOVE WA1-Y-NET  TO D1C-Y-NET.                                
01435      MOVE WA1-Y-EARN TO D1C-Y-EARNED.                             
01436      MOVE WA1-Y-PAID TO D1C-Y-PAID.                               
01437                                                                   
01438      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   
01439          MOVE '******' TO D1C-Y-LOSS-OV                           
01440          MOVE 'Y'      TO LOSS-RATIO-SW                           
01441      ELSE                                                         
01442          MULTIPLY WS-Y-LOSS BY +100 GIVING D1C-Y-LOSS.            
01443                                                                   
01444      MOVE WA1-I-CERT TO D1C-I-CERTS.                              
01445      MOVE WA1-I-NET  TO D1C-I-NET.                                
01446      MOVE WA1-I-EARN TO D1C-I-EARNED.                             
01447      MOVE WA1-I-PAID TO D1C-I-PAID.                               
01448                                                                   
01449      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   
01450          MOVE '******' TO D1C-I-LOSS-OV                           
01451          MOVE 'Y'      TO LOSS-RATIO-SW                           
01452      ELSE                                                         
01453          MULTIPLY WS-I-LOSS BY +100 GIVING D1C-I-LOSS.            
01454                                                                   
01455      MOVE DTL-1A TO PRT.                                          
01456                                                                   
01457      GO TO 0699-EXIT.                                             
01458                                                                   
01459  0640-PRT-OPTION-4.                                               
01460      MOVE WA1-Y-NET  TO D1D-Y-NET.                                
01461      MOVE WA1-Y-EARN TO D1D-Y-EARNED.                             
01462      MOVE WA1-Y-PAID TO D1D-Y-PAID.                               
01463      MOVE WA1-Y-RESV TO D1D-Y-RESERVE.                            
01464                                                                   
01465      IF WS-Y-LOSS GREATER +9.999 OR LESS -9.999                   
01466          MOVE '****' TO D1D-Y-LOSS-OV                             
01467      ELSE                                                         
01468          MULTIPLY WS-Y-LOSS BY +100 GIVING D1D-Y-LOSS.            
01469                                                                   
01470      MOVE WA1-I-NET  TO D1D-I-NET.                                
01471      MOVE WA1-I-EARN TO D1D-I-EARNED.                             
01472      MOVE WA1-I-PAID TO D1D-I-PAID.                               
01473      MOVE WA1-I-RESV TO D1D-I-RESERVE.                            
01474                                                                   
01475      IF WS-I-LOSS GREATER +9.999 OR LESS -9.999                   
01476          MOVE '****' TO D1D-I-LOSS-OV                             
01477      ELSE                                                         
01478          MULTIPLY WS-I-LOSS BY +100 GIVING D1D-I-LOSS.            
01479                                                                   
01480      SUBTRACT WA1-I-EARN FROM WA1-I-NET GIVING D1D-UNEARNED.      
01481                                                                   
01482      MOVE DTL-1A TO PRT.                                          
01483                                                                   
01484  0699-EXIT.                                                       
01485      EXIT.                                                        
01486  EJECT                                                            
01487  0700-ACCUM-RTN.                                                  
01488      MOVE +1     TO X1.                                           
01489                                                                   
01490  0710-ACCUM-RTN-LOOP-1.                                           
01491      IF WT1-KEY (X1) = SPACES                                     
01492          GO TO 0799-EXIT.                                         
01493                                                                   
01494      MOVE +1 TO X2.                                               
01495                                                                   
01496  0720-ACCUM-RTN-LOOP-2.                                           
01497      IF WT1-KEY (X1) = WT2-KEY (X2)  OR                           
01498         WT2-KEY (X2) = SPACES                                     
01499              MOVE WT1-ACCUMS (X1) TO WORK-ACCUMS-1                
01500              MOVE WT2-ACCUMS (X2) TO WORK-ACCUMS-2                
01501              PERFORM 0500-ADD-RTN THRU 0599-EXIT                  
01502              MOVE WORK-ACCUMS-2   TO WT2-ACCUMS (X2)              
01503              MOVE WT1-KEY (X1)    TO WT2-KEY (X2)                 
01504              ADD +1 TO X1                                         
01505              GO TO 0710-ACCUM-RTN-LOOP-1.                         
01506                                                                   
01507      IF WT1-KEY (X1) LESS THAN WT2-KEY (X2)                       
01508          PERFORM 0800-SLIDE-ACCUMS THRU 0899-EXIT                 
01509          GO TO 0720-ACCUM-RTN-LOOP-2.                             
01510                                                                   
01511      ADD +1 TO X2.                                                
01512                                                                   
01513      IF X2 NOT GREATER THAN TABLE-LIMIT                           
01514          GO TO 0720-ACCUM-RTN-LOOP-2                              
01515      ELSE                                                         
01516          DISPLAY 'TOTAL ACCUMULATOR TABLE FULL '                  
01517                        SAVE-CONTROL                               
01518          MOVE '0203' TO WS-RETURN-CODE                            
01519          GO TO ABEND-PGM.                                         
01520                                                                   
01521  0799-EXIT.                                                       
01522      EXIT.                                                        
01523  EJECT                                                            
01524  0800-SLIDE-ACCUMS.                                               
01525      IF WT2-KEY (TABLE-LIMIT) NOT = SPACES                        
01526          DISPLAY 'TOTAL ACCUMULATOR TABLE FULL '                  
01527                        SAVE-CONTROL                               
01528          MOVE '0203' TO WS-RETURN-CODE                            
01529          GO TO ABEND-PGM.                                         
01530                                                                   
01531      MOVE TABLE-LIMIT TO X3.                                      
01532                                                                   
01533  0810-SLIDE-ACCUMS-LOOP-1.                                        
01534      IF WT2-KEY (X3) = SPACES                                     
01535          SUBTRACT +1 FROM X3                                      
01536          GO TO 0810-SLIDE-ACCUMS-LOOP-1.                          
01537                                                                   
01538      ADD X3 +1 GIVING X4.                                         
01539                                                                   
01540  0820-SLIDE-ACCUMS-LOOP-2.                                        
01541      MOVE WT2 (X3) TO WT2 (X4).                                   
01542                                                                   
01543      IF X3 GREATER THAN X2                                        
01544          SUBTRACT +1 FROM X3                                      
01545          SUBTRACT +1 FROM X4                                      
01546          GO TO 0820-SLIDE-ACCUMS-LOOP-2.                          
01547                                                                   
01548      MOVE SPACES        TO WT2-KEY (X2).                          
01549      MOVE WORK-ACCUMS-0 TO WT2-ACCUMS (X2).                       
01550                                                                   
01551  0899-EXIT.                                                       
01552      EXIT.                                                        
01553                                                                   
01554  0900-CHECK-ZEROS.                                                
01555      MOVE ' ' TO ZERO-FLAG.                                       
01556                                                                   
01557      IF WA1-Y-NET  NOT = ZERO  OR                                 
01558         WA1-Y-EARN NOT = ZERO  OR                                 
01559         WA1-Y-PAID NOT = ZERO  OR                                 
01560         WA1-I-NET  NOT = ZERO  OR                                 
01561         WA1-I-EARN NOT = ZERO  OR                                 
01562         WA1-I-PAID NOT = ZERO                                     
01563              MOVE 'X' TO ZERO-FLAG                                
01564              GO TO 0999-EXIT.                                     
01565                                                                   
01566      IF DTE-FMT-OPT = '1' OR '3'                                  
01567          IF WA1-Y-CERT NOT = ZERO  OR                             
01568             WA1-I-CERT NOT = ZERO                                 
01569                  MOVE 'X' TO ZERO-FLAG                            
01570                  GO TO 0999-EXIT.                                 
01571                                                                   
01572      IF DTE-FMT-OPT = '1' OR '2' OR '4'                           
01573          IF WA1-Y-RESV NOT = ZERO  OR                             
01574             WA1-I-RESV NOT = ZERO                                 
01575                  MOVE 'X' TO ZERO-FLAG                            
01576                  GO TO 0999-EXIT.                                 
01577                                                                   
01578  0999-EXIT.                                                       
01579      EXIT.                                                        
01580  EJECT                                                            
01581  1000-PR-BEN-TOT.                                                 
01582      MOVE '  TOT'              TO D1-DESC1.                       
01583                                                                   
01584      IF SAVE-PR-LF-AH = L1                                        
01585          MOVE +2               TO X5                              
01586          MOVE LIFE-OVERRIDE-L2 TO D1-DESC2                        
01587      ELSE                                                         
01588          MOVE +3               TO X5                              
01589          MOVE AH-OVERRIDE-L2   TO D1-DESC2.                       
01590                                                                   
01591      MOVE BEN-TOTALS           TO WORK-ACCUMS-1.                  
01592      PERFORM 0600-SET-UP-DTL-PRT THRU 0699-EXIT.                  
01593                                                                   
061402*    IF PR-BEN-CTR GREATER THAN +1                                
01595          MOVE ' ' TO X                                            
01596          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     
01597                                                                   
01598      MOVE WA1-Y-NET     TO LR-YTD-NET (X5).                       
01599      MOVE WA1-Y-EARN    TO LR-YTD-EARN (X5).                      
01600      MOVE WA1-Y-PAID    TO LR-YTD-PAID (X5).                      
01601      MOVE WA1-Y-RESV    TO LR-YTD-RESV (X5).                      
01602      COMPUTE LR-YTD-INCUR (X5) = WA1-Y-PAID + WA1-Y-RESV.         
01603                                                                   
01604      MOVE WA1-I-NET     TO LR-ITD-NET (X5).                       
01605      MOVE WA1-I-EARN    TO LR-ITD-EARN (X5).                      
01606      MOVE WA1-I-PAID    TO LR-ITD-PAID (X5).                      
01607      MOVE WA1-I-RESV    TO LR-ITD-RESV (X5).                      
01608      COMPUTE LR-ITD-INCUR (X5) = WA1-I-PAID + WA1-I-RESV.         
01609                                                                   
01610      MOVE ZERO          TO PR-BEN-CTR.                            
01611      MOVE WORK-ACCUMS-0 TO BEN-TOTALS.                            
01612      ADD +1 TO LINER.                                             
01613                                                                   
01614  1099-EXIT.                                                       
01615      EXIT.                                                        
01616                                                                   
01617  1100-PR-TYP-TOT.                                                 
01618      MOVE '    TOTAL' TO D1-DESC.                                 
01619                                                                   
01620      MOVE +1                   TO X5.                             
01621                                                                   
01622      MOVE TYP-TOTALS           TO WORK-ACCUMS-1.                  
01623      PERFORM 0600-SET-UP-DTL-PRT THRU 0699-EXIT.                  
01624                                                                   
061402*    IF PR-TYP-CTR GREATER THAN +1
01626          MOVE ' ' TO X                                            
01627          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     
01628                                                                   
01629      MOVE WA1-Y-NET     TO LR-YTD-NET (X5).                       
01630      MOVE WA1-Y-EARN    TO LR-YTD-EARN (X5).                      
01631      MOVE WA1-Y-PAID    TO LR-YTD-PAID (X5).                      
01632      MOVE WA1-Y-RESV    TO LR-YTD-RESV (X5).                      
01633      COMPUTE LR-YTD-INCUR (X5) = WA1-Y-PAID + WA1-Y-RESV.         
01634                                                                   
01635      MOVE WA1-I-NET     TO LR-ITD-NET (X5).                       
01636      MOVE WA1-I-EARN    TO LR-ITD-EARN (X5).                      
01637      MOVE WA1-I-PAID    TO LR-ITD-PAID (X5).                      
01638      MOVE WA1-I-RESV    TO LR-ITD-RESV (X5).                      
01639      COMPUTE LR-ITD-INCUR (X5) = WA1-I-PAID + WA1-I-RESV.         
01640                                                                   
01641      MOVE ZERO          TO PR-TYP-CTR.                            
01642      MOVE WORK-ACCUMS-0 TO TYP-TOTALS.                            
01643      ADD +1 TO LINER.                                             
01644                                                                   
01645  1199-EXIT.                                                       
01646      EXIT.                                                        
01647                                                                   
01648  1200-FIND-BEN-CODE.                                              
01649      MOVE CLAS-STARTL TO CLAS-INDEXL.                             
01650                                                                   
01651      IF WT1-LF-AH (X1) = 'Z' OR 'A'                               
01652          GO TO 1230-SET-FIND-AH-TYPE.                             
01653                                                                   
01654      IF CLAS-STARTL = ZEROS                                       
01655          GO TO 1220-LIFE-NOT-TABLED.                              
01656                                                                   
01657  1210-FIND-LF-TYPE.                                               
01658      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        
01659          GO TO 1220-LIFE-NOT-TABLED.                              
01660                                                                   
01661      IF WT1-BEN-TYP (X1) = CLAS-I-BEN (CLAS-INDEXL)               
01662          GO TO 1299-EXIT.                                         
01663                                                                   
01664      ADD +1 TO CLAS-INDEXL.                                       
01665                                                                   
01666      GO TO 1210-FIND-LF-TYPE.                                     
01667                                                                   
01668  1220-LIFE-NOT-TABLED.                                            
01669      DISPLAY 'LIFE/AH REC CODE'                                   
01670                        WT1-LF-AH (X1).                            
01671      DISPLAY 'LIFE BENEFIT TYPE NOT IN TABLE - ('                 
01672                        WT1-BEN-TYP (X1) ')'.                      
01673                                                                   
01674      MOVE '0401' TO WS-RETURN-CODE.                               
01675                                                                   
01676      GO TO ABEND-PGM.                                             
01677                                                                   
01678  1230-SET-FIND-AH-TYPE.                                           
01679      MOVE CLAS-STARTA TO CLAS-INDEXA.                             
01680                                                                   
01681      IF CLAS-STARTA = ZEROS                                       
01682          GO TO 1250-AH-NOT-TABLED.                                
01683                                                                   
01684  1240-FIND-AH-TYPE.                                               
01685      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        
01686          GO TO 1250-AH-NOT-TABLED.                                
01687                                                                   
01688      IF WT1-BEN-TYP (X1) = CLAS-I-BEN (CLAS-INDEXA)               
01689          GO TO 1299-EXIT.                                         
01690                                                                   
01691      ADD +1 TO CLAS-INDEXA.                                       
01692                                                                   
01693      GO TO 1240-FIND-AH-TYPE.                                     
01694                                                                   
01695  1250-AH-NOT-TABLED.                                              
01696      DISPLAY 'LIFE/AH REC CODE'                                   
01697                        WT1-LF-AH (X1).                            
01698      DISPLAY 'A&H BENEFIT TYPE NOT IN TABLE - ('                  
01699                        WT1-BEN-TYP (X1) ')'.                      
01700                                                                   
01701      MOVE '0402' TO WS-RETURN-CODE.                               
01702                                                                   
01703      GO TO ABEND-PGM.                                             
01704                                                                   
01705  1299-EXIT.                                                       
01706      EXIT.                                                        
01707  EJECT                                                            
01708  1300-DATE-PRT.                                                   
01709      MOVE SPACES TO HDR-4A.                                       
01710                                                                   
01711      MOVE DATE-TOTS TO WORK-TOT-1.                                
01712      MOVE ACCT-TOTS TO WORK-TOT-2.                                
01713                                                                   
01714      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
01715                                                                   
01716      MOVE WORK-TOT-0 TO DATE-TOTS.                                
01717      MOVE WORK-TOT-2 TO ACCT-TOTS.                                
01718                                                                   
01719      IF DTE-PGM-OPT = '1'  OR  '2'                                
CIDMOD*        IF EP-CNTRL-1 NOT = AM-CONTROL-A                         
CIDMOD         IF EP-CONTROL NOT = AM-MSTR-CNTRL                        
01721              MOVE DTE-CLASIC-COMPANY-CD    TO AM-COMPANY-CD       
01722              MOVE EP-CONTROL               TO AM-MSTR-CNTRL       
01723              PERFORM 2700-READ-AM-MSTR THRU 2700-EXIT             
01724              MOVE AM-NAME                  TO SV-NAME             
01725              MOVE AM-CITY                  TO SV-CITY.            
01726                                                                   
01727      IF (SV-REIN = 'B'  OR  'C'  OR  'G')  OR                     
01728         DTE-PGM-OPT NOT = 1                                       
01729          GO TO 1399-EXIT.                                         
01730                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1399-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
01731      MOVE ' CARR'                     TO H4A-CARRIER.             
01732      MOVE ' GRP '                     TO H4A-GROUP.               
01733      MOVE '  ST'                      TO H4A-STATE.               
01734      MOVE '    ACCT  '                TO H4A-ACCOUNT.             
01735      MOVE 'EFF DT'                    TO H4A-EFFECTIVE.           
01736                                                                   
01737      MOVE EP-CARRIER                  TO H4A-CARR.                
01738      MOVE EP-GROUPING                 TO H4A-GRP.                 
01739      MOVE EP-STATE                    TO H4A-ST.                  
01740      MOVE EP-ACCOUNT                  TO H4A-ACCT.                
01741      MOVE SV-NAME                     TO H4A-AM-NAME.             
01742      MOVE SV-CITY                     TO H4A-CTY-ST.              
01743      MOVE EP-EFF-MO                   TO WS-MO.                   
01744      MOVE EP-EFF-DA                   TO WS-DA.                   
01745      MOVE EP-EFF-YR                   TO WS-YR.                   
01746      MOVE WS-DATE                     TO H4A-EFF-DTE.             
01747                                                                   
01748      IF SV-REIN = 'R'                                             
01749          MOVE SV-REINCO-SUB      TO H4A-SUB                       
01750          MOVE HDR-4A-SUB-DESC    TO H4A-CTY-ST.                   
01751                                                                   
01752      IF DATE-PRT-CTR = ZERO                                       
01753          IF EP-CARRIER  NOT = SW-CARRIER  OR                      
01754             EP-GROUPING NOT = SW-GROUPING OR                      
01755             EP-STATE    NOT = SW-STATE    OR                      
01756             EP-ACCOUNT  NOT = SW-ACCOUNT                          
01757              MOVE '  ------ SUMMARY FOR' TO H4A-SUMMARY
01758              MOVE SV-CITY        TO H4A-CTY-ST                    
01759              MOVE SPACES         TO H4A-EFFECTIVE                 
01760                                     H4A-EFF-DTE.                  
01761                                                                   
01762      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
01763                                                                   
01764      ADD +1 TO DATE-PRT-CTR.                                      
01765                                                                   
01766  1399-EXIT.                                                       
01767      EXIT.                                                        
01768                                                                   
01769  1400-ACCT-PRT.                                                   
01770      MOVE ACCT-TOTS TO WORK-TOT-1.                                
01771      MOVE ST-TOTS   TO WORK-TOT-2.                                
01772                                                                   
01773      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
01774                                                                   
01775      MOVE WORK-TOT-0 TO ACCT-TOTS.                                
01776      MOVE WORK-TOT-2 TO ST-TOTS.                                  
01777                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1499-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
01778      IF DTE-PGM-OPT = 2  OR                                       
01779         (SV-REIN  = 'B'  OR  'C'  OR  'G')                        
01780          MOVE +2 TO DATE-PRT-CTR                                  
01781      ELSE                                                         
01782          IF DTE-PGM-OPT NOT = 1                                   
01783              GO TO 1499-EXIT.                                     
01784                                                                   
01785      IF WS-ACCT-OV-SW = 'X'                                       
01786          MOVE +99  TO LINER.                                      
01787                                                                   
01788      IF DATE-PRT-CTR GREATER THAN +1                              
               IF  LINER GREATER THAN +50
CIDMOD             MOVE 'X'                TO BENEFIT-PRT-SW            
CIDMOD             PERFORM 2300-HDR-RTN-A THRU 2300-EXIT                
CIDMOD         END-IF
01789          MOVE SPACES TO HDR-4A                                    
01790          MOVE '  ------ SUMMARY FOR' TO H4A-SUMMARY
01791          MOVE ' CARR'                TO H4A-CARRIER               
01792          MOVE ' GRP '                TO H4A-GROUP                 
01793          MOVE '  ST'                 TO H4A-STATE                 
01794          MOVE '    ACCT  '           TO H4A-ACCOUNT               
01795          MOVE SV-CARRIER             TO H4A-CARR                  
01796          MOVE SV-GROUPING            TO H4A-GRP                   
01797          MOVE SV-STATE               TO H4A-ST                    
01798          MOVE SV-ACCOUNT             TO H4A-ACCT                  
01799          MOVE SV-NAME                TO H4A-AM-NAME               
01800          MOVE SV-CITY                TO H4A-CTY-ST                
01801          PERFORM 0400-PRINT-RTN THRU 0499-EXIT                    
CIDMOD     END-IF.                                                      
01802                                                                   
01803      MOVE +0 TO DATE-PRT-CTR.                                     
01804                                                                   
01805      MOVE SV-REIN              TO LR-RCD-TYPE.                    
01806      MOVE SV-REINCO            TO LR-REIN-CO                      
01807      MOVE SV-RPT-CD-1          TO LR-RPT-CD-1.                    
01808      MOVE SV-CARRIER           TO LR-CARRIER.                     
01809      MOVE SV-GROUPING          TO LR-GROUPING.                    
01810      MOVE SV-RPT-CD-2          TO LR-GA-RPT-CD-2.                 
01811      MOVE SV-STATE             TO LR-STATE.                       
01812      MOVE SV-ACCOUNT           TO LR-ACCOUNT.                     
01813 *    MOVE SV-REINCO-SUB        TO LR-REIN-SUB.                    
01814                                                                   
01815      MOVE SV-NAME              TO LR-ACCT-NAME.                   
01816      MOVE SV-REIN-NAME         TO LR-REIN-NAME.                   
01817                                                                   
01818      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         
01819                                                                   
01820  1499-EXIT.                                                       
01821      EXIT.                                                        
01822                                                                   
01823  1500-STATE-PRT.                                                  
01824                                                                   
01825      MOVE ST-TOTS   TO WORK-TOT-1.                                
01826      MOVE RPT2-TOTS TO WORK-TOT-2.                                
01827                                                                   
01828      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
01829                                                                   
01830      MOVE WORK-TOT-0 TO ST-TOTS.                                  
01831      MOVE WORK-TOT-2 TO RPT2-TOTS.                                
01832                                                                   
01833      IF SV-REIN = 'B'  OR  'C'  OR  'G'                           
01834          GO TO 1549-EXIT                                          
01835      ELSE                                                         
01836          IF DTE-PGM-OPT NOT = 1 AND 2 AND 3                       
01837              GO TO 1549-EXIT.                                     
01838                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1549-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
01839      IF CLAS-STARTS = ZERO                                        
01840          MOVE SPACES TO HDR-4A                                    
01841          GO TO 1520-GET-ST-DESC-1.                                
01842                                                                   
01843      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
01844                                                                   
01845  1510-GET-ST-DESC.                                                
01846      IF SV-STATE NOT = STATE-SUB (CLAS-INDEXS)                    
01847          IF CLAS-INDEXS NOT = CLAS-MAXS                           
01848              ADD +1 TO CLAS-INDEXS                                
01849              GO TO 1510-GET-ST-DESC.                              
01850                                                                   
01851      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            
01852                                                                   
01853      MOVE STATE-PIC (CLAS-INDEXS) TO W-TW-1-NAME.                 
01854                                                                   
01855  1520-GET-ST-DESC-1.                                              
01856      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
01857                                                                   
01858      MOVE 'IN CARRIER'           TO W-TW-3-TITLE.                 
01859      MOVE 'IN GROUP'             TO W-TW-2-TITLE.                 
01860      MOVE 'STATE'                TO W-TW-1-TITLE.                 
01861      MOVE SV-CARRIER             TO W-TW-3-CODE.                  
01862      MOVE SV-GROUPING            TO W-TW-2-CODE.                  
01863      MOVE SV-STATE               TO W-TW-1-CODE.                  
01864      MOVE ')'                    TO W-TW-1-RP.                    
01865      MOVE '('                    TO W-TW-1-LP.                    
01866                                                                   
01867      IF  SV-REIN = 'R'                                            
01868 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               
01869          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  
01870          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   
01871          MOVE SV-REINCO          TO W-TW-4-CODE                   
01872          MOVE ')'                TO W-TW-4-RP                     
01873          MOVE '('                TO W-TW-4-LP.                    
01874 *                                                                 
01875 *    ELSE                                                         
01876 *        IF  SV-REIN = 'C'                                        
01877 *            MOVE 'IN RPT CODE2' TO W-TW-4-TITLE                  
01878 *            MOVE SV-RPT-CD-2    TO W-TW-4-CODE                   
01879 *                                                                 
01880 *        ELSE                                                     
01881 *            IF  SV-REIN = 'G'                                    
01882 *                MOVE 'FOR GEN AGENT'                             
01883 *                                TO W-TW-4-TITLE                  
01884 *                MOVE SV-RPT-CD-2                                 
01885 *                                TO W-TW-4-CODE                   
01886 *                                                                 
01887 *            ELSE                                                 
01888 *                IF  SV-REIN = 'B'                                
01889 *                    MOVE 'IN RPT CODE1'                          
01890 *                                TO W-TW-4-TITLE                  
01891 *                    MOVE SV-RPT-CD-1                             
01892 *                                TO W-TW-4-CODE.                  
01893                                                                   
01894      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
01895                                                                   
01896      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
01897                                                                   
01898      MOVE 'Y'                    TO W-TOTAL-SW.                   
01899      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
01900      MOVE +99                    TO LINER.                        
01901      MOVE SPACE                  TO W-TOTAL-SW.                   
01902                                                                   
01903      IF SV-REIN = 'A'                                             
01904          MOVE 'S'                TO LR-RCD-TYPE                   
01905          MOVE SV-STATE           TO LR-STATE                      
01906          MOVE STATE-PIC (CLAS-INDEXS)                             
01907                                  TO LR-ACCT-NAME                  
01908          PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.
070102
070102*    IF SV-REIN = 'A' OR 'G' OR 'R' OR 'X'
070102*        GO TO 1549-EXIT.
CIDMOD* SKIP 1 ADDITIONAL PAGE TO GET NEW STATE TO PRINT ON SEP. PAGE.  
CIDMOD* PRINT IS SENT TO AGENTS. ONLY PAPER RPT NEEDS EXTRA SKIP.       
070102*
070102*    WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE.
01909                                                                   
01910  1549-EXIT.                                                       
01911      EXIT.                                                        
01912                                                                   
01913  1550-RPT2-PRT.                                                   
01914      MOVE RPT2-TOTS TO WORK-TOT-1.                                
01915      MOVE COMP-TOTS TO WORK-TOT-2.                                
01916                                                                   
01917      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
01918                                                                   
01919      MOVE WORK-TOT-0 TO RPT2-TOTS.                                
01920      MOVE WORK-TOT-2 TO COMP-TOTS.                                
01921                                                                   
01922      IF SV-REIN = 'C'  OR  'G'                                    
01923          NEXT SENTENCE                                            
01924      ELSE                                                         
01925          GO TO 1599-EXIT.                                         
01926                                                                   
01927      MOVE SPACES TO W-TOTAL-WORK-AREA.                            
01928      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
01929                                                                   
01930      MOVE 'IN CARRIER'           TO W-TW-3-TITLE.                 
01931 *    MOVE 'IN CARRIER'           TO W-TW-2-TITLE.                 
01932      MOVE 'IN GROUP'             TO W-TW-2-TITLE.                 
01933 *    MOVE 'GROUP'                TO W-TW-1-TITLE.                 
01934      MOVE 'AGENT'                TO W-TW-1-TITLE.                 
01935 *    MOVE SV-CARRIER             TO W-TW-2-CODE.                  
01936 *    MOVE SV-GROUPING            TO W-TW-1-CODE.                  
01937      MOVE SV-CARRIER             TO W-TW-3-CODE.                  
01938      MOVE SV-GROUPING            TO W-TW-2-CODE.                  
01939      MOVE SV-RPT-CD-2            TO W-TW-1-CODE.                  
01940                                                                   
01941      IF  SV-REIN = 'C'                                            
01942          MOVE 'IN RPT CODE2'     TO W-TW-4-TITLE                  
01943          MOVE SV-RPT-CD-2        TO W-TW-4-CODE                   
01944                                                                   
01945      ELSE                                                         
01946          IF  SV-REIN = 'G'                                        
01947              MOVE 'FOR GEN AGENT'                                 
01948                                  TO W-TW-4-TITLE                  
01949              MOVE SV-RPT-CD-2    TO W-TW-4-CODE.                  
01950                                                                   
01951      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
01952                                                                   
01953      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
01954                                                                   
01955      MOVE 'Y'                    TO W-TOTAL-SW.                   
01956      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
01957      MOVE SPACE                  TO W-TOTAL-SW.                   
01958                                                                   
01959      IF DTE-CLIENT = 'AN1' OR 'ANT' OR 'DDB' OR 'BWS' OR          
01960                      'GSL' OR 'NIS' OR 'TFS' OR 'UW1'             
01961          MOVE ' '              TO PRT                             
01962          MOVE '1'              TO X                               
01963          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     
01964                                                                   
01965      MOVE +99                  TO LINER.                          
01966                                                                   
01967      MOVE SV-REIN              TO LR-RCD-TYPE.                    
01968      MOVE SV-CARRIER           TO LR-CARRIER.                     
01969      MOVE SV-GROUPING          TO LR-GROUPING.                    
01970      MOVE SV-RPT-CD-2          TO LR-GA-RPT-CD-2.                 
01971                                                                   
01972      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         
01973                                                                   
070102*    IF H1-SUFFIX = 'G' OR 'R'
070102*    IF H1-SUFFIX = 'A' OR 'G' OR 'R' OR 'X'
070102*        GO TO 1599-EXIT.
CIDMOD* SKIP 1 ADDITIONAL PAGE TO GET NEW ACCT TO PRINT ON SEP. PAGE.   
CIDMOD* PRINT IS SENT TO AGENTS. ONLY PAPER RPT NEEDS EXTRA SKIP.       
070102*    WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE.
CIDMOD                                                                  
01974  1599-EXIT.                                                       
01975      EXIT.                                                        
01976                                                                   
01977  1600-COMP-PRT.                                                   
01978      MOVE COMP-TOTS TO WORK-TOT-1.                                
01979      MOVE CARR-TOTS TO WORK-TOT-2.                                
01980                                                                   
01981      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
01982                                                                   
01983      MOVE WORK-TOT-0 TO COMP-TOTS.                                
01984      MOVE WORK-TOT-2 TO CARR-TOTS.                                
01985                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1699-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
01986      IF SV-REIN = 'C'  OR  'G'                                    
01987          NEXT SENTENCE                                            
01988      ELSE                                                         
01989          IF SV-REIN = 'B'  OR                                     
01990            (DTE-PGM-OPT NOT = 1 AND 2 AND 3 AND 4                 
01991                           AND 5 AND 6 AND 7 AND 8)                
01992              GO TO 1699-EXIT.                                     
01993                                                                   
01994      MOVE 'X'                    TO G-A-PRT-SW.                   
01995      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            
01996      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
01997                                                                   
01998      MOVE 'IN CARRIER'           TO W-TW-2-TITLE.                 
01999      MOVE 'GROUP'                TO W-TW-1-TITLE.                 
02000      MOVE SV-CARRIER             TO W-TW-2-CODE.                  
02001      MOVE SV-GROUPING            TO W-TW-1-CODE.                  
02002                                                                   
02003      IF  SV-REIN = 'R'                                            
02004 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               
02005          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  
02006          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   
02007          MOVE SV-REINCO          TO W-TW-4-CODE                   
02008          MOVE ')'                TO W-TW-4-RP                     
02009          MOVE '('                TO W-TW-4-LP                     
02010                                                                   
02011      ELSE                                                         
02012          IF  SV-REIN = 'C'                                        
02013              MOVE 'IN RPT CODE2' TO W-TW-4-TITLE                  
02014              MOVE SV-RPT-CD-2    TO W-TW-4-CODE                   
02015                                                                   
02016          ELSE                                                     
02017              IF  SV-REIN = 'G'                                    
02018                  MOVE 'FOR GEN AGENT'                             
02019                                  TO W-TW-4-TITLE                  
02020                  MOVE SV-RPT-CD-2                                 
02021                                  TO W-TW-4-CODE                   
02022                                                                   
02023              ELSE                                                 
02024                  IF  SV-REIN = 'B'                                
02025                      MOVE 'IN RPT CODE1'                          
02026                                  TO W-TW-4-TITLE                  
02027                      MOVE SV-RPT-CD-1                             
02028                                  TO W-TW-4-CODE.                  
02029                                                                   
02030      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
02031                                                                   
02032      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
02033                                                                   
02034      MOVE 'Y'                    TO W-TOTAL-SW.                   
02035      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
02036      MOVE +99                    TO LINER.                        
02037      MOVE SPACE                  TO W-TOTAL-SW.                   
02038                                                                   
02039      MOVE SPACE                  TO G-A-PRT-SW.                   
02040                                                                   
02041  1699-EXIT.                                                       
02042      EXIT.                                                        
02043                                                                   
02044  1700-CARR-PRT.                                                   
02045      MOVE CARR-TOTS TO WORK-TOT-1.                                
02046      MOVE REIN-TOTS TO WORK-TOT-2.                                
02047                                                                   
02048      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
02049                                                                   
02050      MOVE WORK-TOT-0 TO CARR-TOTS.                                
02051      MOVE WORK-TOT-2 TO REIN-TOTS.                                
02052                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1749-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
02053      IF SV-REIN = 'C'  OR  'G'                                    
02054          NEXT SENTENCE                                            
02055      ELSE                                                         
02056          IF SV-REIN = 'B'  OR                                     
02057            (DTE-PGM-OPT NOT = 1 AND 2 AND 3 AND 4 AND 5)          
02058              GO TO 1749-EXIT.                                     
02059                                                                   
02060      MOVE 'X'                    TO G-A-PRT-SW.                   
02061      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            
02062      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
02063                                                                   
02064      MOVE 'CARRIER'              TO W-TW-1-TITLE.                 
02065      MOVE SV-CARRIER             TO W-TW-1-CODE.                  
02066                                                                   
02067      IF  SV-REIN = 'R'                                            
02068 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               
02069          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  
02070          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   
02071          MOVE SV-REINCO          TO W-TW-4-CODE                   
02072          MOVE ')'                TO W-TW-4-RP                     
02073          MOVE '('                TO W-TW-4-LP                     
02074                                                                   
02075      ELSE                                                         
02076          IF  SV-REIN = 'C'                                        
02077              MOVE 'IN RPT CODE2' TO W-TW-4-TITLE                  
02078              MOVE SV-RPT-CD-2    TO W-TW-4-CODE                   
02079                                                                   
02080          ELSE                                                     
02081              IF  SV-REIN = 'G'                                    
02082                  MOVE 'FOR GEN AGENT'                             
02083                                  TO W-TW-4-TITLE                  
02084                  MOVE SV-RPT-CD-2                                 
02085                                  TO W-TW-4-CODE                   
02086                                                                   
02087              ELSE                                                 
02088                  IF  SV-REIN = 'B'                                
02089                      MOVE 'IN RPT CODE1'                          
02090                                  TO W-TW-4-TITLE                  
02091                      MOVE SV-RPT-CD-1                             
02092                                  TO W-TW-4-CODE.                  
02093                                                                   
02094      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
02095                                                                   
02096      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
02097                                                                   
02098      MOVE 'Y'                    TO W-TOTAL-SW.                   
02099      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
02100      MOVE +99                    TO LINER.                        
02101      MOVE SPACE                  TO W-TOTAL-SW.                   
02102                                                                   
02103      MOVE SPACE                  TO G-A-PRT-SW.                   
02104                                                                   
02105  1749-EXIT.                                                       
02106      EXIT.                                                        
02107                                                                   
02108  1750-REIN-PRT.                                                   
02109      MOVE REIN-TOTS TO WORK-TOT-1.                                
02110      MOVE RPT1-TOTS TO WORK-TOT-2.                                
02111                                                                   
02112      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
02113                                                                   
02114      MOVE WORK-TOT-0 TO REIN-TOTS.                                
02115      MOVE WORK-TOT-2 TO RPT1-TOTS.                                
02116                                                                   
02117      IF SV-REIN = 'A'  OR  'B'  OR  'C'  OR  'G'                  
02118          GO TO 1759-EXIT                                          
02119      ELSE                                                         
02120          IF DTE-PGM-OPT NOT = 1 AND 2 AND 3 AND 4 AND 5           
02121              GO TO 1759-EXIT.                                     
02122                                                                   
02123      MOVE 'X'                TO REIN-PRT-SW.                      
02124      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            
02125      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
02126 *                                                                 
02127 *    MOVE 'REINSURANCE'          TO W-TW-1-NAME.                  
02128 *                                                                 
02129      IF  SV-REIN = 'R'                                            
02130 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               
02131          MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  
02132          MOVE SV-REIN-NAME       TO W-TW-4-NAME                   
02133          MOVE SV-REINCO          TO W-TW-4-CODE                   
02134          MOVE ')'                TO W-TW-4-RP                     
02135          MOVE '('                TO W-TW-4-LP.                    
02136                                                                   
JJPMOD     IF  SV-REIN = 'X'                                            
JJPMOD         MOVE 'IN REIN COMP'     TO W-TW-4-TITLE                  
JJPMOD         MOVE SV-X-REIN-NAME     TO W-TW-4-NAME                   
JJPMOD         MOVE SPACES             TO W-TW-4-CODE                   
JJPMOD         MOVE ')'                TO W-TW-4-RP                     
JJPMOD         MOVE '('                TO W-TW-4-LP.                    
JJPMOD                                                                  
02137      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
02138                                                                   
02139      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
02140                                                                   
02141      MOVE 'Y'                    TO W-TOTAL-SW.                   
02142      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
02143      MOVE SPACE                  TO W-TOTAL-SW.                   
02144      MOVE +99                TO LINER.                            
02145                                                                   
02146      MOVE SPACE              TO REIN-PRT-SW.                      
02147                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1759-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
02148      MOVE SV-REIN            TO LR-RCD-TYPE.                      
02149      MOVE SV-REINCO          TO LR-REIN-CO.                       
02150      MOVE SV-REIN-NAME       TO LR-REIN-NAME.                     
02151                                                                   
02152      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         
02153                                                                   
02154  1759-EXIT.                                                       
02155      EXIT.                                                        
02156                                                                   
02157  1770-RPT1-PRT.                                                   
02158      MOVE RPT1-TOTS TO WORK-TOT-1.                                
02159      MOVE FINL-TOTS TO WORK-TOT-2.                                
02160                                                                   
02161      PERFORM 0700-ACCUM-RTN THRU 0799-EXIT.                       
02162                                                                   
02163      MOVE WORK-TOT-0 TO RPT1-TOTS.                                
02164      MOVE WORK-TOT-2 TO FINL-TOTS.                                
02165                                                                   
02166      IF SV-REIN NOT = 'B'                                         
02167          GO TO 1799-EXIT.                                         
02168                                                                   
02169      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            
02170      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
02171                                                                   
02172      MOVE 'RPT CODE1'            TO W-TW-1-TITLE                  
02173 **   MOVE SV-RPT-CD-1            TO W-TW-1-CODE.                  
CIDMOD     MOVE SV-RPT-CD-1            TO WK-SV-RPT-CD.                 
CIDMOD     MOVE WORK-SV-RPT-CD         TO W-TW-1-CODE.                  
02174                                                                   
02175      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
02176                                                                   
02177      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
02178                                                                   
02179      MOVE SPACES                   TO HDR-4A.                     
02180      MOVE '  ------ SUMMARY    '   TO H4A-SUMMARY.
02181                                                                   
02182      MOVE 'Y'                    TO W-TOTAL-SW.                   
02183      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.

070102     IF  LOSS-RATIO-TOO-LARGE
070102         MOVE LOSS-RATIO-MSG   TO PRT
070102         MOVE '0'              TO X
070102         MOVE SPACES           TO LOSS-RATIO-SW
070102         PERFORM 2500-PRT-RTN  THRU 2599-EXIT.

02184      MOVE +99                  TO LINER.
02185      MOVE SPACE                TO W-TOTAL-SW.
02186                                                                   
02187      MOVE SV-REIN              TO LR-RCD-TYPE.                    
02188      MOVE SV-RPT-CD-1          TO LR-RPT-CD-1.                    
02189                                                                   
02190      PERFORM 2800-WRITE-LOSS-RATIO-MASTER THRU 2899-EXIT.         
CIDMOD                                                                  
CIDMOD* SKIP 1 ADDITIONAL PAGE TO GET NEW ACCT TO PRINT ON SEP. PAGE.   
CIDMOD* PRINT IS SENT TO AGENTS. ONLY PAPER RPT NEEDS EXTRA SKIP.       
070102*    WRITE PRT FROM SPACE-LINE AFTER ADVANCING PAGE.
CIDMOD                                                                  
02191                                                                   
02192  1799-EXIT.                                                       
02193      EXIT.                                                        
02194                                                                   
02195  1800-FINAL-PRT.                                                  
02196      MOVE FINL-TOTS  TO WORK-TOT-1.                               
02197      MOVE WORK-TOT-0 TO FINL-TOTS.                                
02198                                                                   
JJPMOD     IF SV-REIN = 'X'                                             
JJPMOD         GO TO 1899-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
02199      MOVE SPACE TO SV-REIN                                        
02200                    SV-ACCT-FLAG.                                  
02201                                                                   
02202      MOVE SPACES                 TO W-TOTAL-WORK-AREA.            
02203      PERFORM 2350-HDR-RTN-T THRU 2350-EXIT.                       
02204                                                                   
02205      MOVE 'REPORT'               TO W-TW-1-NAME                   
02206                                                                   
02207      PERFORM 2600-REMOVE-SPACES THRU 2600-EXIT.                   
02208                                                                   
02209      PERFORM 2370-WRITE-DETAIL-HEADER THRU 2370-EXIT.             
02210      MOVE '0'             TO X.                                   
02211                                                                   
02212      MOVE 'Y'                    TO W-TOTAL-SW.                   
02213      PERFORM 0400-PRINT-RTN THRU 0499-EXIT.                       
02214      MOVE SPACE                  TO W-TOTAL-SW.                   
02215                                                                   
02216  1899-EXIT.                                                       
02217      EXIT.                                                        
02218  EJECT                                                            
02219  1900-ITD-ADD.                                                    
02220      COMPUTE WA1-I-CERT =                                         
02221          WA1-I-CERT + EP-ISS-CNT - EP-CNC-CNT.                    
02222                                                                   
02223      COMPUTE WA1-I-NET =                                          
02224          WA1-I-NET + EP-ISS-PRM - EP-CNC-PRM.                     
02225                                                                   
02226      ADD EP-CLM-AMT TO WA1-I-PAID.                                
02227                                                                   
02228      ADD EP-CLM-DU TO WA1-I-RESV.                                 
02229      IF EP-CLM-PV NUMERIC                                         
02230          ADD EP-CLM-PV TO WA1-I-RESV.                             
02231      ADD EP-LOSS-RESV TO WA1-I-RESV.                              
02232                                                                   
02233      IF DTE-CLIENT = 'FIM' OR                                     
02234        (DTE-CLIENT = 'FFL' AND EP-RCD-TYPE = L1)                  
02235          NEXT SENTENCE                                            
02236      ELSE                                                         
02237          ADD EP-CLM-IBNR TO WA1-I-RESV.                           
02238                                                                   
02239      IF EP-RCD-TYPE = L1                                          
02240          MOVE FAC-1 TO WS-FAC                                     
02241      ELSE                                                         
02242          MOVE FAC-2 TO WS-FAC.                                    
02243                                                                   
02244      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02245          IF EP-RCD-TYPE = L1                                      
02246              MOVE FAC-3          TO WS-FAC                        
02247              MOVE LF-PRO         TO WS-PRO                        
02248              MOVE LF-R78         TO WS-R78                        
02249          ELSE                                                     
02250              MOVE FAC-4          TO WS-FAC                        
02251              MOVE AH-PRO         TO WS-PRO                        
02252              MOVE AH-R78         TO WS-R78.                       
02253                                                                   
02254      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02255          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     
02256              IF (EP-RCD-TYPE = L1 AND                             
02257                  CLAS-I-EP (CLAS-INDEXL) = 'P') OR                
02258                 (EP-RCD-TYPE = 'Z' AND                            
02259                  CLAS-I-EP (CLAS-INDEXA) = 'P') OR                
02260                 (STATE-ABBR (CLAS-INDEXS) = 'WY')                 
02261                  MOVE +1.00      TO WS-PRO                        
02262              ELSE                                                 
02263                  MOVE +1.00      TO WS-R78.                       
02264                                                                   
02265      IF EP-RCD-TYPE =  L1  AND                                    
02266         CLAS-I-EP (CLAS-INDEXL) = 'U'                             
02267          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     
02268                                                                   
02269      IF (DTE-CLIENT = 'FIA'  AND                                  
02270          EP-CARRIER = ('A' OR 'B' OR 'C' OR 'O')  AND             
02271          EP-RCD-TYPE = 'Z')                                       
02272                    OR                                             
02273         (DTE-CLIENT = ('AN1' OR 'DDB' OR 'UW1' OR 'TFS' OR        
02274                       'NIS' OR 'BWS')  AND                        
02275          EP-RCD-TYPE = 'Z')                                       
02276              COMPUTE WA1-I-NET = WA1-I-NET -                      
02277                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02278              COMPUTE WA1-I-EARN = WA1-I-EARN +                    
02279                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02280              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02281                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02282                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02283                  * WS-FAC)                                        
02284               GO TO 1999-EXIT.                                    
02285                                                                   
02286      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02287          COMPUTE WA1-I-NET = WA1-I-NET -                          
02288               (EP-PRM-PR-ADJ * WS-PRO) - (EP-PRM-78-ADJ * WS-R78) 
02289          COMPUTE WA1-I-EARN = WA1-I-EARN +                        
02290               (EP-PRM-PR * WS-PRO) + (EP-PRM-78 * WS-R78)         
02291          COMPUTE WA1-I-RESV = WA1-I-RESV +                        
02292              (((EP-ISS-PRM - EP-CNC-PRM) -                        
02293              (TT-PRM-PR * WS-PRO) - (TT-PRM-78 * WS-R78))         
02294              * WS-FAC)                                            
02295            GO TO 1999-EXIT.                                       
02296                                                                   
02297      IF DTE-CLIENT = 'GIC'                                        
02298          IF EP-RCD-TYPE = 'Z' AND                                 
02299             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    
02300              COMPUTE WA1-I-NET = WA1-I-NET -                      
02301                   (EP-PRM-PR-ADJ * +.80) - (EP-PRM-78-ADJ * +.20) 
02302              COMPUTE WA1-I-EARN = WA1-I-EARN +                    
02303                   (EP-PRM-PR * +.80) + (EP-PRM-78 * +.20)         
02304              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02305                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02306                  (TT-PRM-PR * +.80) - (TT-PRM-78 * +.20))         
02307                  * WS-FAC)                                        
02308                GO TO 1999-EXIT.                                   
02309                                                                   
02310      IF DTE-CLIENT = 'FIA'                                        
02311          IF EP-RCD-TYPE = L1                                      
02312            AND EP-REIN NOT = 'R'                                  
02313              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-78-ADJ        
02314              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-78          
02315              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02316                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02317                  * WS-FAC)                                        
02318              GO TO 1999-EXIT.                                     
02319                                                                   
02320      IF (EP-RCD-TYPE = L1  AND CLAS-I-EP (CLAS-INDEXL) = '1') OR  
02321         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = '1') OR  
02322         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 
02323                              AND (EP-CARRIER = '5' OR '7'))       
02324                  COMPUTE WA1-I-NET = WA1-I-NET -                  
02325                       (EP-PRM-PR-ADJ * +.6667) -                  
02326                       (EP-PRM-78-ADJ * +.3333)                    
02327                  COMPUTE WA1-I-EARN = WA1-I-EARN +                
02328                       (EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333) 
02329                  COMPUTE WA1-I-RESV = WA1-I-RESV +                
02330                      (((EP-ISS-PRM - EP-CNC-PRM) -                
02331                      (TT-PRM-PR * +.6667) - (TT-PRM-78 * +.3333)) 
02332                      * WS-FAC)                                    
02333                  GO TO 1999-EXIT.                                 
02334                                                                   
02335      IF (EP-RCD-TYPE = L1  AND CLAS-I-EP (CLAS-INDEXL) = 'P') OR  
02336         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = 'P') OR  
02337         (STATE-ABBR (CLAS-INDEXS) = 'WY') OR                      
02338         (DTE-CLIENT = 'SLC')  OR                                  
02339         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 
02340                            AND EP-CARRIER = '2')                  
02341              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-PR-ADJ        
02342              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-PR          
02343              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02344                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-PR)         
02345                  * WS-FAC)                                        
02346              GO TO 1999-EXIT.                                     
02347                                                                   
02348      IF EP-RCD-TYPE = L1                                          
02349          AND CLAS-I-EP (CLAS-INDEXL) = 'M'                        
02350               COMPUTE WA1-I-NET = WA1-I-NET -                     
02351                   ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)          
02352               COMPUTE WA1-I-EARN = WA1-I-EARN +                   
02353                   ((EP-PRM-PR + EP-PRM-78) / +2)                  
02354               COMPUTE WA1-I-RESV = WA1-I-RESV +                   
02355                   (((EP-ISS-PRM - EP-CNC-PRM) -                   
02356                   ((TT-PRM-PR + TT-PRM-78) / +2))                 
02357                   * WS-FAC).                                      
02358                                                                   
02359      IF EP-RCD-TYPE = L1                                          
02360          AND (CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A') 
02361              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-78-ADJ        
02362              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-78          
02363              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02364                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02365                  * WS-FAC).                                       
02366                                                                   
02367      IF EP-RCD-TYPE = L1                                          
02368          AND CLAS-I-EP (CLAS-INDEXL) = 'B'                        
02369              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-ST-ADJ        
02370              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-ST          
02371              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02372                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-ST)         
02373                  * WS-FAC).                                       
02374                                                                   
02375      IF EP-RCD-TYPE = 'Z'                                         
02376          AND CLAS-I-EP (CLAS-INDEXA) = 'M'                        
02377               COMPUTE WA1-I-NET = WA1-I-NET -                     
02378                   ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)          
02379               COMPUTE WA1-I-EARN = WA1-I-EARN +                   
02380                   ((EP-PRM-PR + EP-PRM-78) / +2)                  
02381               COMPUTE WA1-I-RESV = WA1-I-RESV +                   
02382                   (((EP-ISS-PRM - EP-CNC-PRM) -                   
02383                   ((TT-PRM-PR + TT-PRM-78) / +2))                 
02384                   * WS-FAC).                                      
02385                                                                   
02386      IF (EP-RCD-TYPE = 'Z'                                        
02387                  AND (CLAS-I-EP (CLAS-INDEXA) = 'R' OR 'A'))      
02388           IF (DTE-CLIENT = 'POS' AND EP-CARRIER = '2')  OR        
02389              (DTE-CLIENT = 'FIM')                                 
02390               COMPUTE WA1-I-NET = WA1-I-NET -                     
02391                   ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)          
02392               COMPUTE WA1-I-EARN = WA1-I-EARN +                   
02393                   ((EP-PRM-PR + EP-PRM-78) / +2)                  
02394               COMPUTE WA1-I-RESV = WA1-I-RESV +                   
02395                   (((EP-ISS-PRM - EP-CNC-PRM) -                   
02396                   ((TT-PRM-PR + TT-PRM-78) / +2))                 
02397                   * WS-FAC)                                       
02398           ELSE                                                    
02399              COMPUTE WA1-I-NET = WA1-I-NET - EP-PRM-78-ADJ        
02400              COMPUTE WA1-I-EARN = WA1-I-EARN + EP-PRM-78          
02401              COMPUTE WA1-I-RESV = WA1-I-RESV +                    
02402                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02403                  * WS-FAC).                                       
02404                                                                   
02405  1999-EXIT.                                                       
02406      EXIT.                                                        
02407  EJECT                                                            
02408  2000-YTD-ADD.                                                    
02409      COMPUTE WA1-Y-CERT =                                         
02410          WA1-Y-CERT + EP-ISS-CNT - EP-CNC-CNT.                    
02411                                                                   
02412      COMPUTE WA1-Y-NET =                                          
02413          WA1-Y-NET + EP-ISS-PRM - EP-CNC-PRM.                     
02414                                                                   
02415      ADD EP-CLM-AMT TO WA1-Y-PAID.                                
02416                                                                   
02417      ADD EP-CLM-DU TO WA1-Y-RESV.                                 
02418      IF EP-CLM-PV NUMERIC                                         
02419          ADD EP-CLM-PV TO WA1-Y-RESV.                             
02420      ADD EP-LOSS-RESV TO WA1-Y-RESV.                              
02421                                                                   
02422      IF DTE-CLIENT = 'FIM'  OR                                    
02423         (DTE-CLIENT = 'FFL'  AND  EP-RCD-TYPE = L1)               
02424          NEXT SENTENCE                                            
02425      ELSE                                                         
02426          ADD EP-CLM-IBNR TO WA1-Y-RESV.                           
02427                                                                   
02428      IF EP-RCD-TYPE = L1                                          
02429          MOVE FAC-1 TO WS-FAC                                     
02430      ELSE                                                         
02431          MOVE FAC-2 TO WS-FAC.                                    
02432                                                                   
02433      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02434          IF EP-RCD-TYPE = L1                                      
02435              MOVE FAC-3          TO WS-FAC                        
02436              MOVE LF-PRO         TO WS-PRO                        
02437              MOVE LF-R78         TO WS-R78                        
02438          ELSE                                                     
02439              MOVE FAC-4          TO WS-FAC                        
02440              MOVE AH-PRO         TO WS-PRO                        
02441              MOVE AH-R78         TO WS-R78.                       
02442                                                                   
02443      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02444          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     
02445              IF (EP-RCD-TYPE = L1 AND                             
02446                  CLAS-I-EP (CLAS-INDEXL) = 'P') OR                
02447                 (EP-RCD-TYPE = 'Z' AND                            
02448                  CLAS-I-EP (CLAS-INDEXA) = 'P') OR                
02449                 (STATE-ABBR (CLAS-INDEXS) = 'WY')                 
02450                  MOVE +1.00      TO WS-PRO                        
02451              ELSE                                                 
02452                  MOVE +1.00      TO WS-R78.                       
02453                                                                   
02454      IF EP-RCD-TYPE = L1  AND                                     
02455         CLAS-I-EP (CLAS-INDEXL) = 'U'                             
02456          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     
02457                                                                   
02458      IF (DTE-CLIENT = 'FIA'  AND                                  
02459          EP-CARRIER = ('A' OR 'B' OR 'C' OR 'O')  AND             
02460          EP-RCD-TYPE = 'Z')                                       
02461                    OR                                             
02462         (DTE-CLIENT = ('AN1' OR 'DDB' OR 'UW1' OR 'TFS' OR        
02463                       'NIS' OR 'BWS')  AND                        
02464          EP-RCD-TYPE = 'Z')                                       
02465              COMPUTE WA1-Y-NET = WA1-Y-NET -                      
02466                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02467              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    
02468                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02469              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02470                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02471                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02472                  * WS-FAC)                                        
02473                GO TO 2099-EXIT.                                   
02474                                                                   
02475      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02476          COMPUTE WA1-Y-NET = WA1-Y-NET -                          
02477               (EP-PRM-PR-ADJ * WS-PRO) - (EP-PRM-78-ADJ * WS-R78) 
02478          COMPUTE WA1-Y-EARN = WA1-Y-EARN +                        
02479               (EP-PRM-PR * WS-PRO) + (EP-PRM-78 * WS-R78)         
02480          COMPUTE WA1-Y-RESV = WA1-Y-RESV +                        
02481              (((EP-ISS-PRM - EP-CNC-PRM) -                        
02482              (TT-PRM-PR * WS-PRO) - (TT-PRM-78 * WS-R78))         
02483              * WS-FAC)                                            
02484            GO TO 2099-EXIT.                                       
02485                                                                   
02486      IF DTE-CLIENT = 'GIC'                                        
02487          IF EP-RCD-TYPE = 'Z' AND                                 
02488             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    
02489              COMPUTE WA1-Y-NET = WA1-Y-NET -                      
02490                   (EP-PRM-PR-ADJ * +.80) - (EP-PRM-78-ADJ * +.20) 
02491              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    
02492                   (EP-PRM-PR * +.80) + (EP-PRM-78 * +.20)         
02493              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02494                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02495                  (TT-PRM-PR * +.80) - (TT-PRM-78 * +.20))         
02496                  * WS-FAC)                                        
02497                GO TO 2099-EXIT.                                   
02498                                                                   
02499      IF DTE-CLIENT = 'FIA'                                        
02500          IF EP-RCD-TYPE = L1                                      
02501            AND EP-REIN NOT = 'R'                                  
02502              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-78-ADJ        
02503              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-78          
02504              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02505                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02506                  * WS-FAC)                                        
02507              GO TO 2099-EXIT.                                     
02508                                                                   
02509      IF (EP-RCD-TYPE =  L1 AND CLAS-I-EP (CLAS-INDEXL) = '1') OR  
02510         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = '1') OR  
02511         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 
02512                              AND (EP-CARRIER = '5' OR '7'))       
02513                  COMPUTE WA1-Y-NET = WA1-Y-NET -                  
02514                       (EP-PRM-PR-ADJ * +.6667) -                  
02515                       (EP-PRM-78-ADJ * +.3333)                    
02516                  COMPUTE WA1-Y-EARN = WA1-Y-EARN +                
02517                       (EP-PRM-PR * +.6667) + (EP-PRM-78 * +.3333) 
02518                  COMPUTE WA1-Y-RESV = WA1-Y-RESV +                
02519                      (((EP-ISS-PRM - EP-CNC-PRM) -                
02520                      (TT-PRM-PR * +.6667) - (TT-PRM-78 * +.3333)) 
02521                      * WS-FAC)                                    
02522                  GO TO 2099-EXIT.                                 
02523                                                                   
02524      IF (EP-RCD-TYPE =  L1 AND CLAS-I-EP (CLAS-INDEXL) = 'P') OR  
02525         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = 'P') OR  
02526         (STATE-ABBR (CLAS-INDEXS) = 'WY') OR                      
02527         (DTE-CLIENT = 'SLC')  OR                                  
02528         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 
02529                              AND EP-CARRIER = '2')                
02530              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-PR-ADJ        
02531              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-PR          
02532              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02533                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-PR)         
02534                  * WS-FAC)                                        
02535             GO TO 2099-EXIT.                                      
02536                                                                   
02537      IF EP-RCD-TYPE = L1                                          
02538          AND CLAS-I-EP (CLAS-INDEXL) = 'M'                        
02539              COMPUTE WA1-Y-NET = WA1-Y-NET -                      
02540                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02541              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    
02542                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02543              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02544                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02545                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02546                  * WS-FAC).                                       
02547                                                                   
02548      IF EP-RCD-TYPE = L1                                          
02549          AND (CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A') 
02550              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-78-ADJ        
02551              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-78          
02552              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02553                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02554                  * WS-FAC).                                       
02555                                                                   
02556      IF EP-RCD-TYPE = L1                                          
02557          AND CLAS-I-EP (CLAS-INDEXL) = 'B'                        
02558              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-ST-ADJ        
02559              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-ST          
02560              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02561                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-ST)         
02562                  * WS-FAC).                                       
02563                                                                   
02564      IF EP-RCD-TYPE = 'Z'                                         
02565          AND CLAS-I-EP (CLAS-INDEXA) = 'M'                        
02566              COMPUTE WA1-Y-NET = WA1-Y-NET -                      
02567                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02568              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    
02569                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02570              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02571                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02572                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02573                  * WS-FAC).                                       
02574                                                                   
02575      IF (EP-RCD-TYPE = 'Z'                                        
02576                  AND (CLAS-I-EP (CLAS-INDEXA) = 'R' OR 'A'))      
02577          IF (DTE-CLIENT = 'POS' AND EP-CARRIER = '2')  OR         
02578             (DTE-CLIENT = 'FIM')                                  
02579              COMPUTE WA1-Y-NET = WA1-Y-NET -                      
02580                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02581              COMPUTE WA1-Y-EARN = WA1-Y-EARN +                    
02582                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02583              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02584                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02585                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02586                  * WS-FAC)                                        
02587          ELSE                                                     
02588              COMPUTE WA1-Y-NET = WA1-Y-NET - EP-PRM-78-ADJ        
02589              COMPUTE WA1-Y-EARN = WA1-Y-EARN + EP-PRM-78          
02590              COMPUTE WA1-Y-RESV = WA1-Y-RESV +                    
02591                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02592                  * WS-FAC).                                       
02593                                                                   
02594  2099-EXIT.                                                       
02595      EXIT.                                                        
02596  EJECT                                                            
02597  2100-YTD-SUB.                                                    
02598      COMPUTE WA1-Y-CERT =                                         
02599          WA1-Y-CERT - EP-ISS-CNT + EP-CNC-CNT.                    
02600                                                                   
02601      COMPUTE WA1-Y-NET =                                          
02602          WA1-Y-NET - EP-ISS-PRM + EP-CNC-PRM.                     
02603                                                                   
02604      SUBTRACT EP-CLM-AMT FROM WA1-Y-PAID.                         
02605                                                                   
02606      SUBTRACT EP-CLM-DU FROM WA1-Y-RESV.                          
02607      IF EP-CLM-PV NUMERIC                                         
02608          SUBTRACT EP-CLM-PV FROM WA1-Y-RESV.                      
02609      SUBTRACT EP-LOSS-RESV FROM WA1-Y-RESV.                       
02610                                                                   
02611      IF DTE-CLIENT = 'FIM'  OR                                    
02612         (DTE-CLIENT = 'FFL'  AND  EP-RCD-TYPE = L1)               
02613          NEXT SENTENCE                                            
02614      ELSE                                                         
02615          SUBTRACT EP-CLM-IBNR FROM WA1-Y-RESV.                    
02616                                                                   
02617      IF EP-RCD-TYPE = L1                                          
02618          MOVE FAC-1 TO WS-FAC                                     
02619      ELSE                                                         
02620          MOVE FAC-2 TO WS-FAC.                                    
02621                                                                   
02622      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02623          IF EP-RCD-TYPE = L1                                      
02624              MOVE FAC-3          TO WS-FAC                        
02625              MOVE LF-PRO         TO WS-PRO                        
02626              MOVE LF-R78         TO WS-R78                        
02627          ELSE                                                     
02628              MOVE FAC-4          TO WS-FAC                        
02629              MOVE AH-PRO         TO WS-PRO                        
02630              MOVE AH-R78         TO WS-R78.                       
02631                                                                   
02632      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02633          IF WS-PRO = ZERO  AND  WS-R78 = ZERO                     
02634              IF (EP-RCD-TYPE = L1 AND                             
02635                  CLAS-I-EP (CLAS-INDEXL) = 'P') OR                
02636                 (EP-RCD-TYPE = 'Z' AND                            
02637                  CLAS-I-EP (CLAS-INDEXA) = 'P') OR                
02638                 (STATE-ABBR (CLAS-INDEXS) = 'WY')                 
02639                  MOVE +1.00      TO WS-PRO                        
02640              ELSE                                                 
02641                  MOVE +1.00      TO WS-R78.                       
02642                                                                   
02643      IF EP-RCD-TYPE = L1  AND                                     
02644         CLAS-I-EP (CLAS-INDEXL) = 'U'                             
02645          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     
02646                                                                   
02647      IF (DTE-CLIENT = 'FIA'  AND                                  
02648          EP-CARRIER = ('A' OR 'B' OR 'C' OR 'O')  AND             
02649          EP-RCD-TYPE = 'Z')                                       
02650                    OR                                             
02651         (DTE-CLIENT = ('AN1' OR 'DDB' OR 'UW1' OR 'TFS' OR        
02652                       'NIS' OR 'BWS')  AND                        
02653          EP-RCD-TYPE = 'Z')                                       
02654              COMPUTE WA1-Y-NET = WA1-Y-NET +                      
02655                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02656              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    
02657                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02658              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02659                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02660                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02661                  * WS-FAC)                                        
02662                GO TO 2199-EXIT.                                   
02663                                                                   
02664      IF DTE-CLIENT = 'FIM'  AND  EP-REIN = 'R'                    
02665          COMPUTE WA1-Y-NET = WA1-Y-NET +                          
02666               (EP-PRM-PR-ADJ * WS-PRO) + (EP-PRM-78-ADJ * WS-R78) 
02667          COMPUTE WA1-Y-EARN = WA1-Y-EARN -                        
02668               (EP-PRM-PR * WS-PRO) - (EP-PRM-78 * WS-R78)         
02669          COMPUTE WA1-Y-RESV = WA1-Y-RESV -                        
02670              (((EP-ISS-PRM - EP-CNC-PRM) -                        
02671              (TT-PRM-PR * WS-PRO) - (TT-PRM-78 * WS-R78))         
02672              * WS-FAC)                                            
02673         GO TO 2199-EXIT.                                          
02674                                                                   
02675      IF DTE-CLIENT = 'GIC'                                        
02676          IF EP-RCD-TYPE = 'Z' AND                                 
02677             CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                    
02678              COMPUTE WA1-Y-NET = WA1-Y-NET +                      
02679                   (EP-PRM-PR-ADJ * +.80) + (EP-PRM-78-ADJ * +.20) 
02680              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    
02681                   (EP-PRM-PR * +.80) - (EP-PRM-78 * +.20)         
02682              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02683                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02684                  (TT-PRM-PR * +.80) - (TT-PRM-78 * +.20))         
02685                  * WS-FAC)                                        
02686             GO TO 2199-EXIT.                                      
02687                                                                   
02688      IF DTE-CLIENT = 'FIA'                                        
02689          IF EP-RCD-TYPE = L1                                      
02690            AND EP-REIN NOT = 'R'                                  
02691              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-78-ADJ        
02692              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-78          
02693              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02694                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02695                  * WS-FAC)                                        
02696              GO TO 2199-EXIT.                                     
02697                                                                   
02698      IF (EP-RCD-TYPE = L1 AND CLAS-I-EP (CLAS-INDEXL) = '1') OR   
02699         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = '1') OR  
02700         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 
02701                              AND (EP-CARRIER = '5' OR '7'))       
02702                  COMPUTE WA1-Y-NET = WA1-Y-NET +                  
02703                       (EP-PRM-PR-ADJ * +.6667) +                  
02704                       (EP-PRM-78-ADJ * +.3333)                    
02705                  COMPUTE WA1-Y-EARN = WA1-Y-EARN -                
02706                       (EP-PRM-PR * +.6667) - (EP-PRM-78 * +.3333) 
02707                  COMPUTE WA1-Y-RESV = WA1-Y-RESV -                
02708                      (((EP-ISS-PRM - EP-CNC-PRM) -                
02709                      (TT-PRM-PR * +.6667) - (TT-PRM-78 * +.3333)) 
02710                      * WS-FAC)                                    
02711                 GO TO 2199-EXIT.                                  
02712                                                                   
02713      IF (EP-RCD-TYPE = L1 AND CLAS-I-EP (CLAS-INDEXL) = 'P') OR   
02714         (EP-RCD-TYPE = 'Z' AND CLAS-I-EP (CLAS-INDEXA) = 'P') OR  
02715         (STATE-ABBR (CLAS-INDEXS) = 'WY') OR                      
02716         (DTE-CLIENT = 'SLC')  OR                                  
02717         (EP-RCD-TYPE = 'Z' AND DTE-CLIENT = 'ITY'                 
02718                              AND EP-CARRIER = '2')                
02719              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-PR-ADJ        
02720              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-PR          
02721              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02722                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-PR)         
02723                  * WS-FAC)                                        
02724                   GO TO 2199-EXIT.                                
02725                                                                   
02726      IF EP-RCD-TYPE = L1                                          
02727         AND CLAS-I-EP (CLAS-INDEXL) = 'M'                         
02728              COMPUTE WA1-Y-NET = WA1-Y-NET +                      
02729                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02730              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    
02731                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02732              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02733                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02734                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02735                  * WS-FAC).                                       
02736                                                                   
02737      IF EP-RCD-TYPE = L1                                          
02738         AND (CLAS-I-EP (CLAS-INDEXL) = 'R' OR 'T' OR 'N' OR 'A')  
02739              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-78-ADJ        
02740              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-78          
02741              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02742                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02743                  * WS-FAC).                                       
02744                                                                   
02745      IF EP-RCD-TYPE = L1                                          
02746         AND CLAS-I-EP (CLAS-INDEXL) = 'B'                         
02747              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-ST-ADJ        
02748              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-ST          
02749              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02750                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-ST)         
02751                  * WS-FAC).                                       
02752                                                                   
02753      IF EP-RCD-TYPE = 'Z'                                         
02754         AND CLAS-I-EP (CLAS-INDEXA) = 'M'                         
02755              COMPUTE WA1-Y-NET = WA1-Y-NET +                      
02756                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02757              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    
02758                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02759              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02760                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02761                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02762                  * WS-FAC).                                       
02763                                                                   
02764      IF (EP-RCD-TYPE = 'Z'                                        
02765                  AND (CLAS-I-EP (CLAS-INDEXA) = 'R' OR 'A'))      
02766          IF (DTE-CLIENT = 'POS' AND EP-CARRIER = '2')  OR         
02767             (DTE-CLIENT = 'FIM')                                  
02768              COMPUTE WA1-Y-NET = WA1-Y-NET +                      
02769                  ((EP-PRM-PR-ADJ + EP-PRM-78-ADJ) / +2)           
02770              COMPUTE WA1-Y-EARN = WA1-Y-EARN -                    
02771                  ((EP-PRM-PR + EP-PRM-78) / +2)                   
02772              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02773                  (((EP-ISS-PRM - EP-CNC-PRM) -                    
02774                  ((TT-PRM-PR + TT-PRM-78) / +2))                  
02775                  * WS-FAC)                                        
02776          ELSE                                                     
02777              COMPUTE WA1-Y-NET = WA1-Y-NET + EP-PRM-78-ADJ        
02778              COMPUTE WA1-Y-EARN = WA1-Y-EARN - EP-PRM-78          
02779              COMPUTE WA1-Y-RESV = WA1-Y-RESV -                    
02780                  (((EP-ISS-PRM - EP-CNC-PRM) - TT-PRM-78)         
02781                  * WS-FAC).                                       
02782                                                                   
02783  2199-EXIT.                                                       
02784      EXIT.                                                        
02785  EJECT                                                            
02786  2200-FIND-REIN-NAME.                                             
02787 *    IF PRINTING-REIN-TOTALS                                      
02788 *        MOVE SPACES              TO H2-CARRIER  H2-CARR          
02789 *    ELSE                                                         
02790 *        MOVE 'CARRIER   '        TO H2-CARRIER.                  
02791 *        MOVE SV-CARRIER          TO H2-CARR.                     
02792                                                                   
02793      MOVE 'REINSURED BY' TO H3R-REIN.                             
02794 *    MOVE 'CEDED FROM'   TO H3R-REIN.                             
02795                                                                   
02796      IF SV-REINCO = H3R-REIN-COMP                                 
02797 **       GO TO 2299-EXIT.                                         
JJPMOD         GO TO 2230-EXIT.                                         
02798                                                                   
02799      MOVE +0        TO WS-RCT-X.                                  
02800      MOVE SV-REINCO TO H3R-REIN-COMP.                             
02801                                                                   
02802  2210-FIND-REIN-NAME-LOOP.                                        
02803      ADD +1 TO WS-RCT-X.                                          
02804                                                                   
02805      IF WS-RCT-X GREATER THAN WS-RCT-MAX OR                       
02806         WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      
02807              MOVE 'NAME UNKNOWN' TO H3R-REIN-NAME                 
02808                                     SV-REIN-NAME                  
02809 **           GO TO 2299-EXIT.                                     
JJPMOD             GO TO 2230-EXIT.                                     
02810                                                                   
02811      IF SV-REI-CO = WS-RCT-COMP (WS-RCT-X)                        
02812          MOVE WS-RCT-NAME (WS-RCT-X) TO H3R-REIN-NAME             
02813                                         SV-REIN-NAME              
02814 *        MOVE WS-RCT-CEDE (WS-RCT-X) TO H3R-REIN-NAME             
02815 **       GO TO 2299-EXIT.                                         
JJPMOD         GO TO 2230-EXIT.                                         
02816                                                                   
02817      GO TO 2210-FIND-REIN-NAME-LOOP.                              
02818                                                                   
02819 *2299-EXIT.                                                       
JJPMOD 2230-EXIT.                                                       
02820      EXIT.                                                        
JJPMOD                                                                  
JJPMOD 2250-FIND-REIN-NAME.                                             
JJPMOD                                                                  
JJPMOD     IF SW-REINCO = SV-X-REIN-COMP                                
JJPMOD         MOVE SV-X-REIN-NAME TO SW-REIN-NAME                      
JJPMOD         GO TO 2270-EXIT.                                         
JJPMOD                                                                  
JJPMOD     MOVE +0        TO WS-RCT-X.                                  
JJPMOD     MOVE SW-REINCO TO SV-X-REIN-COMP.                            
JJPMOD                                                                  
JJPMOD 2260-FIND-REIN-NAME-LOOP.                                        
JJPMOD     ADD +1 TO WS-RCT-X.                                          
JJPMOD                                                                  
JJPMOD     IF WS-RCT-X GREATER THAN WS-RCT-MAX OR                       
JJPMOD        WS-RCT-COMP (WS-RCT-X) = HIGH-VALUES                      
JJPMOD             MOVE 'NAME UNKNOWN' TO SW-REIN-NAME                  
JJPMOD                                    SV-X-REIN-NAME                
JJPMOD             GO TO 2270-EXIT.                                     
JJPMOD                                                                  
JJPMOD     IF SW-REI-CO = WS-RCT-COMP (WS-RCT-X)                        
JJPMOD         MOVE WS-RCT-NAME (WS-RCT-X) TO SW-REIN-NAME              
JJPMOD                                        SV-X-REIN-NAME            
JJPMOD         IF SW-REIN-NAME = SPACES OR LOW-VALUES
JJPMOD             MOVE 'NAME UNKNOWN' TO SW-REIN-NAME                  
JJPMOD                                    SV-X-REIN-NAME                
JJPMOD         END-IF
JJPMOD         GO TO 2270-EXIT                                          
JJPMOD     END-IF.                                                      
JJPMOD                                                                  
JJPMOD     GO TO 2260-FIND-REIN-NAME-LOOP.                              
JJPMOD                                                                  
JJPMOD 2270-EXIT.                                                       
JJPMOD     EXIT.                                                        
02821  EJECT                                                            
02822  2300-HDR-RTN-A.                                                  
02823                                                                   
           IF  LOSS-RATIO-TOO-LARGE
               MOVE LOSS-RATIO-MSG     TO PRT
070102         MOVE '0'                TO X
               MOVE SPACES             TO LOSS-RATIO-SW
               MOVE +3                 TO LINER
070102         PERFORM 2500-PRT-RTN THRU 2599-EXIT.
02830                                                                   
02831      ADD +1 TO PAGER.                                             
02832      MOVE PAGER TO H3-PAGE.                                       
02833      MOVE HDR-1 TO PRT.                                           
02834      MOVE '1'   TO X.                                             
02835                                                                   
02836      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02837                                                                   
02838      MOVE HDR-2 TO PRT.                                           
02839      MOVE ' '   TO X.                                             
02840                                                                   
02841      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02842                                                                   
02843      MOVE HDR-3 TO PRT.                                           
02844      MOVE ' '   TO X.                                             
02845                                                                   
02846      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02847                                                                   
02848      MOVE +3 TO LINER.                                            
02849                                                                   
           IF SV-REIN = 'B'
               MOVE SV-RPT-CD-1      TO H3B-RPT-CODE-1
               MOVE HDR-3B TO PRT
               MOVE ' '    TO X
               PERFORM 2500-PRT-RTN THRU 2599-EXIT
               ADD +1 TO LINER.
02856                                                                   
02857      IF SV-REIN = 'C'                                             
02858          MOVE SV-RPT-CD-2      TO H3C-RPT-CODE-2                  
02859          MOVE HDR-3C TO PRT                                       
02860          MOVE ' '    TO X                                         
02861          PERFORM 2500-PRT-RTN THRU 2599-EXIT                      
02862          ADD +1 TO LINER.                                         
02863                                                                   
02864      IF SV-REIN = 'G'                                             
02865          IF PRINTING-G-A-HDR                                      
02866              MOVE SV-RPT-CD-2      TO H3G-G-A-NUMBER              
02867              MOVE HDR-3G TO PRT                                   
02868              MOVE ' '    TO X                                     
02869              PERFORM 2500-PRT-RTN THRU 2599-EXIT                  
02870              ADD +1 TO LINER.                                     
02871                                                                   
02872      IF SV-REIN = 'R'                                             
02873 **       PERFORM 2200-FIND-REIN-NAME THRU 2299-EXIT               
JJPMOD         PERFORM 2200-FIND-REIN-NAME THRU 2230-EXIT               
02874          MOVE HDR-3R TO PRT                                       
02875          MOVE ' '    TO X                                         
02876          PERFORM 2500-PRT-RTN THRU 2599-EXIT                      
02877          ADD +1 TO LINER.                                         
02878                                                                   
02879      MOVE HDR-5A TO PRT.                                          
02880      MOVE '0'    TO X.                                            
02881      ADD +2 TO LINER.                                             
02882                                                                   
02883      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02884                                                                   
02885      MOVE HDR-6A TO PRT.                                          
02886      MOVE ' '    TO X.                                            
02887      ADD +1 TO LINER.                                             
02888                                                                   
02889      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02890                                                                   
02891      MOVE HDR-7A TO PRT.                                          
02892      MOVE ' '    TO X.                                            
02893      ADD +1 TO LINER.                                             
02894                                                                   
02895      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02896                                                                   
02897  2300-EXIT.                                                       
02898      EXIT.                                                        
02899  EJECT                                                            
02900  2350-HDR-RTN-T.                                                  
02901                                                                   
02902      IF  LOSS-RATIO-TOO-LARGE                                     
02903          MOVE LOSS-RATIO-MSG TO PRT                               
070102         MOVE '0'            TO X
02905          MOVE SPACES         TO LOSS-RATIO-SW                     
02906          PERFORM 2500-PRT-RTN THRU 2599-EXIT.                     
02907                                                                   
02908      ADD +1 TO PAGER.                                             
02909      MOVE PAGER TO H3-PAGE.                                       
02910      MOVE HDR-1 TO PRT.                                           
02911      MOVE '1'   TO X.                                             
02912                                                                   
02913      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02914                                                                   
02915      MOVE HDR-2 TO PRT.                                           
02916      MOVE ' '   TO X.                                             
02917                                                                   
02918      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02919                                                                   
02920      MOVE HDR-3 TO PRT.                                           
02921      MOVE ' '   TO X.                                             
02922                                                                   
02923      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02924                                                                   
02925      MOVE +3 TO LINER.                                            
02926                                                                   
02927  2350-EXIT.                                                       
02928      EXIT.                                                        
02929                                                                   
02930  2370-WRITE-DETAIL-HEADER.                                        
02931                                                                   
02932      MOVE HDR-4-TOTAL            TO PRT.                          
02933      MOVE '0'                    TO X.                            
02934      ADD +2                      TO LINER.                        
02935                                                                   
02936      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02937                                                                   
02938      MOVE HDR-5A TO PRT.                                          
02939      MOVE '0'    TO X.                                            
02940      ADD +2 TO LINER.                                             
02941                                                                   
02942      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02943                                                                   
02944      MOVE HDR-6A TO PRT.                                          
02945      MOVE ' '    TO X.                                            
02946      ADD +1 TO LINER.                                             
02947                                                                   
02948      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02949                                                                   
02950      MOVE HDR-7A TO PRT.                                          
02951      MOVE ' '    TO X.                                            
02952      ADD +1 TO LINER.                                             
02953                                                                   
02954      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02955                                                                   
02956  2370-EXIT.                                                       
02957      EXIT.                                                        
02958                                                                   
02959  2400-HDR-RTN-B.                                                  
02960                                                                   
02961      MOVE HDR-4A TO PRT.                                          
02962      MOVE '-'    TO X.                                            
02963      ADD +3      TO LINER.                                        
02964                                                                   
02965      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02966                                                                   
02967 **   IF H1-SUFFIX = 'R'                                           
JJPMOD     IF H1-SUFFIX = 'R' OR 'X'                                    
02968          MOVE ' REINSURANCE' TO H8-DESC                           
02969      ELSE                                                         
02970          MOVE ' NEW BUSINESS' TO H8-DESC.                         
02971                                                                   
02972      MOVE ' '    TO X.                                            
02973      MOVE HDR-8  TO PRT.                                          
02974                                                                   
02975      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02976                                                                   
02977      ADD +1 TO LINER.                                             
02978                                                                   
02979  2400-EXIT.                                                       
02980      EXIT.                                                        
02981                                                                   
02982  2420-HDR-RTN-T.
070102******************************************************************
070102** THIS ROUTINE IS FOR THE "TOTALS" PAGE FOLLOWING ACCOUNT
070102** SUMMARIES FOR EACH COMPANY
070102******************************************************************
JJPMOD     IF H1-SUFFIX = 'R' OR 'X'                                    
02985          MOVE ' REINSURANCE' TO H8-DESC                           
02986      ELSE                                                         
02987          MOVE ' NEW BUSINESS' TO H8-DESC.                         
02988                                                                   
02989      MOVE ' '    TO X.                                            
02990      MOVE HDR-8  TO PRT.                                          
02991                                                                   
02992      PERFORM 2500-PRT-RTN THRU 2599-EXIT.                         
02993                                                                   
02994      ADD +1 TO LINER.                                             
02995                                                                   
02996  2420-EXIT.                                                       
02997      EXIT.                                                        
02998                                                                   
02999  2500-PRT-RTN.                                                    
CIDMOD                             COPY PRTN020.                        
CIDMOD*                            COPY ELCPRT2.                        
03001                                                                   
03002  2599-EXIT.            EXIT.                                      
03003                                  EJECT                            
03004  2600-REMOVE-SPACES.                                              
03005                                                                   
03006      MOVE SPACES                 TO H4T-LINE.                     
03007      SET H4T-NDX                 TO W-ZEROS.                      
03008                                                                   
03009      PERFORM 2620-TRANSFER-DATA THRU 2620-EXIT                    
03010              VARYING                                              
03011          W-TW-NDX FROM 1 BY 1                                     
03012              UNTIL                                                
03013          W-TW-NDX GREATER THAN +284.                              
03014                                                                   
03015  2600-EXIT.                                                       
03016      EXIT.                                                        
03017                                                                   
03018  2620-TRANSFER-DATA.                                              
03019                                                                   
03020      IF  W-TW-CHAR (W-TW-NDX) EQUAL SPACES                        
03021              AND                                                  
03022          H4T-CHAR (H4T-NDX) EQUAL SPACES                          
03023          NEXT SENTENCE                                            
03024                                                                   
03025      ELSE                                                         
03026          IF  W-TW-CHAR (W-TW-NDX) EQUAL ')'                       
03027                  AND                                              
03028              H4T-CHAR (H4T-NDX) EQUAL SPACES                      
03029              MOVE W-TW-CHAR (W-TW-NDX)                            
03030                                  TO H4T-CHAR (H4T-NDX)            
03031                                                                   
03032          ELSE                                                     
03033              SET H4T-NDX UP BY +1                                 
03034              MOVE W-TW-CHAR (W-TW-NDX)                            
03035                                  TO H4T-CHAR (H4T-NDX).           
03036                                                                   
03037  2620-EXIT.                                                       
03038      EXIT.                                                        
03039                                  EJECT                            
03040  2700-READ-AM-MSTR.                                               
03041      READ ACCT-MASTER.                                            
03042                                                                   
03043      IF AM-FILE-STATUS NOT = '00'                                 
03044          MOVE '1'                  TO WAC-1                       
03045          MOVE '4'                  TO WAC-2                       
03046          MOVE AM-FILE-STATUS       TO WAC-3-4                     
03047          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              
03048 *        DISPLAY 'AM-KEY - ' AM-CONTROL-PRIMARY                   
CIDMOD*        DISPLAY 'RET-CD - ' WS-RETURN-CODE                       
CIDMOD         GO TO 2700-EXIT.                                         
CIDMOD*        GO TO ABEND-PGM.                                         
03050                                                                   
03051  2700-EXIT.                                                       
03052      EXIT.                                                        
03053                                                                   
03054  2800-WRITE-LOSS-RATIO-MASTER.                                    
03055                                                                   
03056      WRITE LOSS-RATIO-EXTRACT  FROM  LOSS-RATIO-MASTER.           
03057                                                                   
03058  2899-EXIT.                                                       
03059      EXIT.                                                        
03060                                                                   
03061  2999-EXIT.                                                       
03062      EXIT.                                                        
03063                                                                   
03064  ABEND-PGM SECTION.                                               
03065                           COPY ELCABEND.                          
03066                                                                   
03067  9999-END-OF-JOB.                                                 
03068      CLOSE ACCT-MASTER                                            
03069            ELCNTL                                                 
03070            LOSS-RATIOS                                            
061102           PRINTER-OUTPUT.
03072                                                                   
03073      IF AM-FILE-STATUS NOT = '00'                                 
03074          MOVE '1'                  TO WAC-1                       
03075          MOVE '2'                  TO WAC-2                       
03076          MOVE AM-FILE-STATUS       TO WAC-3-4                     
03077          MOVE WORK-ABEND-CODE      TO WS-RETURN-CODE              
03078          GO TO ABEND-PGM.                                         
03079                                                                   
03080      IF ELCNTL-FILE-STATUS NOT = '00'                             
03081          MOVE '**** ELCNTL CLOSE ERROR ****'                      
03082                                  TO WS-ABEND-MESSAGE              
03083          MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS          
03084          GO TO ABEND-PGM.                                         
03085                                                                   
03086                                COPY ELCPRTC.                      
03087      GOBACK.                                                      
03088 /                                                                 
03089  LCP-WRITE-POS-PRT SECTION.                                       
03090      IF LCP-ASA = '+'                                             
03091          WRITE PRT AFTER 0 LINE                                   
03092      ELSE                                                         
03093      IF LCP-ASA = ' '                                             
03094          WRITE PRT AFTER ADVANCING 1 LINE                         
03095      ELSE                                                         
03096      IF LCP-ASA = '0'                                             
03097          WRITE PRT AFTER ADVANCING 2 LINE                         
03098      ELSE                                                         
03099      IF LCP-ASA = '-'                                             
03100          WRITE PRT AFTER ADVANCING 3 LINE                         
03101      ELSE                                                         
03102      IF LCP-ASA = '1'                                             
03103          WRITE PRT AFTER ADVANCING PAGE                           
03104      ELSE                                                         
03105      IF LCP-ASA = '2'                                             
03106          WRITE PRT AFTER ADVANCING LCP-CH2                        
03107      ELSE                                                         
03108      IF LCP-ASA = '3'                                             
03109          WRITE PRT AFTER ADVANCING LCP-CH3                        
03110      ELSE                                                         
03111      IF LCP-ASA = '4'                                             
03112          WRITE PRT AFTER ADVANCING LCP-CH4                        
03113      ELSE                                                         
03114      IF LCP-ASA = '5'                                             
03115          WRITE PRT AFTER ADVANCING LCP-CH5                        
03116      ELSE                                                         
03117      IF LCP-ASA = '6'                                             
03118          WRITE PRT AFTER ADVANCING LCP-CH6                        
03119      ELSE                                                         
03120      IF LCP-ASA = '7'                                             
03121          WRITE PRT AFTER ADVANCING LCP-CH7                        
03122      ELSE                                                         
03123      IF LCP-ASA = '8'                                             
03124          WRITE PRT AFTER ADVANCING LCP-CH8                        
03125      ELSE                                                         
03126      IF LCP-ASA = '9'                                             
03127          WRITE PRT AFTER ADVANCING LCP-CH9                        
03128      ELSE                                                         
03129      IF LCP-ASA = 'A'                                             
03130          WRITE PRT AFTER ADVANCING LCP-CH10                       
03131      ELSE                                                         
03132      IF LCP-ASA = 'B'                                             
03133          WRITE PRT AFTER ADVANCING LCP-CH11                       
03134      ELSE                                                         
03135      IF LCP-ASA = 'C'                                             
03136          WRITE PRT AFTER ADVANCING LCP-CH12                       
03137      ELSE                                                         
03138      IF LCP-ASA = 'V'                                             
03139          WRITE PRT AFTER ADVANCING LCP-P01                        
03140      ELSE                                                         
03141      IF LCP-ASA = 'W'                                             
03142          WRITE PRT AFTER ADVANCING LCP-P02                        
03143      ELSE                                                         
03144      DISPLAY 'ASA CODE ERROR'.                                    
03145  LCP-WRITE-END-PRT.                                               
03146      EXIT.                                                        
