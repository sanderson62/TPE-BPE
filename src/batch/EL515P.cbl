00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                  EL515 .                             
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 10/30/95 13:55:19.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.027.                          
00009                                                                   
00010 *AUTHOR.        LOGIC INC.                                        
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
00025 *REMARKS.     CLASIC-CREDIT  EDIT LIST.                           
00026 *          PROGRAM PROCESS OPTION :                               
00027 *                                                                 
00028 *        (EL515-A) SPACE  =  SELECT ALL RECORDS ON FILE           
00029 *        (EL515-B)     1  =  SELECT ERRORS ONLY                   
00030 *        (EL515-C)     2  =  SELECT MONTH-ENDING DATE (DATE-CARD) 
00031 *        (EL515-D)     3  =  SELECT MONTH-ENDING-DATE ERRORS ONLY 
00032 *        (EL515-E)     4  =  SELECT ERRORS (UNFORCED AND FATAL)   
00033 *        (EL515-F)     5  =  SELECT MONTH-ENDING-DATE  -          
00034 *                                          (UNFORCED AND FATAL ERR
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
091604* 091604    2003040800003  PEMA  ADD BILLING NOTE PRINT
102706* 102706  CR2006052600003  PEMA  ADD SIG SW PROCESSING FOR CID
060408* 060408  CR2008021200003  PEMA  ADD CSR CODE TO REPORT
122002******************************************************************
00035   EJECT                                                           
00036  ENVIRONMENT DIVISION.                                            
00037  CONFIGURATION SECTION.                                           
00038  SPECIAL-NAMES.                                                   
00039      C02 IS LCP-CH2                                               
00040      C03 IS LCP-CH3                                               
00041      C04 IS LCP-CH4                                               
00042      C05 IS LCP-CH5                                               
00043      C06 IS LCP-CH6                                               
00044      C07 IS LCP-CH7                                               
00045      C08 IS LCP-CH8                                               
00046      C09 IS LCP-CH9                                               
00047      C10 IS LCP-CH10                                              
00048      C11 IS LCP-CH11                                              
00049      C12 IS LCP-CH12                                              
00050      S01 IS LCP-P01                                               
00051      S02 IS LCP-P02.                                              
00052  INPUT-OUTPUT SECTION.                                            
00053  FILE-CONTROL.                                                    
00054                                                                   
00055      SELECT ERPNDB ASSIGN TO SYS024-FBA1-ERPNDB                   
00056                    ORGANIZATION     IS INDEXED                    
00057                    ACCESS           IS DYNAMIC                    
00058                    RECORD KEY       IS PB-CONTROL-PRIMARY         
00059                    FILE STATUS      IS PB-STATUS-CODE.            
00060                                                                   
00061      SELECT ERPNDC ASSIGN TO SYS021-FBA1-ERPNDC                   
00062                    ORGANIZATION     IS INDEXED                    
00063                    ACCESS           IS DYNAMIC                    
00064                    RECORD KEY       IS PC-CONTROL-PRIMARY         
00065                    FILE STATUS      IS PC-STATUS-CODE.            
00066                                                                   
00067      SELECT ERCRTC ASSIGN TO SYS022-FBA1-ERCRTC                   
00068                    ORGANIZATION     IS INDEXED                    
00069                    ACCESS           IS DYNAMIC                    
00070                    RECORD KEY       IS CC-CONTROL-PRIMARY         
00071                    FILE STATUS      IS CC-STATUS-CODE.            
00072                                                                   
00073      SELECT ELCNTL ASSIGN TO SYS023-FBA1-ELCNTL                   
00074                    ORGANIZATION     IS INDEXED                    
00075                    ACCESS           IS DYNAMIC                    
00076                    RECORD KEY       IS CF-CONTROL-PRIMARY         
00077                    FILE STATUS      IS CN-STATUS-CODE.            
00078                                                                   
00079      SELECT ERACCT ASSIGN TO SYS011-FBA1-ERACCT2                  
00080                    ORGANIZATION     IS INDEXED                    
00081                    ACCESS           IS DYNAMIC                    
00082                    RECORD KEY       IS AM-CONTROL-BY-VAR-GRP      
00083                    FILE STATUS      IS AM-STATUS-CODE.            
00084                                                                   
00085      SELECT ELERRS ASSIGN TO SYS027-FBA1-ELERRS                   
00086                    ORGANIZATION     IS INDEXED                    
00087                    ACCESS           IS DYNAMIC                    
00088                    RECORD KEY       IS EM-CONTROL-PRIMARY         
00089                    FILE STATUS      IS EM-STATUS-CODE.            
00090                                                                   
00091      SELECT ELREPT ASSIGN TO SYS010-FBA1-ELREPT                   
00092                    ORGANIZATION     IS INDEXED                    
00093                    ACCESS           IS DYNAMIC                    
00094                    RECORD KEY       IS RF-CONTROL-PRIMARY         
00095                    FILE STATUS      IS DTE-VSAM-FLAGS.            
00096                                                                   
091604     SELECT ERNOTE ASSIGN TO SYS025-FBA1-ERNOTE         
091604                   ORGANIZATION     IS INDEXED              
091604                   ACCESS           IS DYNAMIC                    
091604                   RECORD KEY       IS CN-CONTROL-PRIMARY     
091604                   FILE STATUS      IS ERNOTE-FILE-STATUS.   
091604
00097      SELECT SORT-WORK   ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.       
00098      SELECT PRINT-FILE  ASSIGN TO SYS008-UR-1403-S-SYS008.        
00099      SELECT DISK-DATE   ASSIGN TO SYS019-UT-FBA1-S-SYS019.        
00100      SELECT FICH        ASSIGN TO SYS020-UT-2400-S-SYS020.        
00101  EJECT                                                            
00102  DATA DIVISION.                                                   
00103  FILE SECTION.                                                    
00104                                                                   
00105  FD  ERPNDB.                                                      
00106                                                                   
00107      COPY ERCPNDB.                                                
00108  EJECT                                                            
00109                                                                   
00110  FD  ERPNDC.                                                      
00111                                                                   
00112      COPY ERCPNDC.                                                
00113  EJECT                                                            
00114                                                                   
00115  FD  ERCRTC.                                                      
00116                                                                   
00117      COPY ERCCRTC.                                                
00118  EJECT                                                            
00119                                                                   
00120  FD  ELCNTL.                                                      
00121                                                                   
00122      COPY ELCCNTL.                                                
00123  EJECT                                                            
00124  FD  ERACCT.                                                      
00125                                                                   
00126      COPY ERCACCT.                                                
00127      EJECT                                                        
00128  FD  ELERRS.                                                      
00129                                                                   
00130      COPY ELCERRS.                                                
00131                                                                   
00132      EJECT                                                        
00133  FD  ELREPT       COPY ELCRPTFD.                                  
00134      COPY ELCREPT.                                                
00135      EJECT                                                        
091604 FD  ERNOTE.                                                      
091604                                                                  
091604                                 COPY ERCNOTE.
091604 EJECT                                                            
00136  SD  SORT-WORK.                                                   
00137                                                                   
00138  01  SORT-RECORD.                                                 
00139      12  FILLER                      PIC XX.                      
00140      12  SORT-PEND-DATA.                                          
00141          16  SORT-P-COMPANY-CD       PIC X.                       
00142          16  SORT-P-ENTRY-BATCH      PIC X(6).                    
00143          16  FILLER                  PIC S9(4) COMP.              
00144          16  FILLER                  PIC S9(4) COMP.              
00145          16  FILLER                  PIC X.                       
00146          16  SORT-P-CARRIER          PIC X.                       
00147          16  SORT-P-GROUPING         PIC X(6).                    
00148          16  SORT-P-STATE            PIC XX.                      
00149          16  SORT-P-ACCOUNT          PIC X(10).                   
00150          16  SORT-P-CERT-EFF-DT      PIC XX.                      
00151          16  SORT-P-CERT-NO          PIC X(11).                   
00152          16  SORT-P-CHG-SEQ-NO       PIC S9(4) COMP.              
00153          16  SORT-P-RECORD-TYPE      PIC X.                       
00154      12  FILLER                      PIC X(536).                  
00155      EJECT                                                        
00156  FD  PRINT-FILE                                                   
00157                              COPY ELCPRTFD.                       
00158      EJECT                                                        
00159  FD  DISK-DATE                                                    
00160                              COPY ELCDTEFD.                       
00161      EJECT                                                        
00162  FD  FICH                                                         
00163                              COPY ELCFCHFD.                       
00164  EJECT                                                            
00165  WORKING-STORAGE SECTION.                                         
00166  01  LCP-ABND-CODE                 PIC S999 COMP VALUE +519.      
00167  77  LCP-ASA                       PIC X.                         
00168  77  FILLER  PIC X(32) VALUE '********************************'.  
00169  77  FILLER  PIC X(32) VALUE '     EL515  WORKING-STORAGE     '.  
00170  77  FILLER  PIC X(32) VALUE '******** VMOD=2.027 ************'.  
091604 77  STRT-LN                     PIC S99 COMP VALUE +0.        
091604 77  END-LN                      PIC S99 COMP VALUE +0.        
00171                                                                   
00172  01  WS-COMP-AREA            COMP-3.                              
00173     03  AVAILABLE-ISSUE-CNT      PIC S9(7)           VALUE +0.    
00174     03  AVAILABLE-CANCEL-CNT     PIC S9(7)           VALUE +0.    
00175     03  BATCH-CNT                PIC S9(7)           VALUE +0.    
00176     03  BATCHES-OUT-OF-BALANCE   PIC S9(7)           VALUE +0.    
00177     03  C-ACCEPT                 PIC S9(5)           VALUE +0.    
00178     03  CANCEL-CNT               PIC S9(7)           VALUE +0.    
00179     03  CLAIM-PAYMENTS           PIC S9(7)           VALUE +0.    
00180     03  FATAL-ISSUE-CNT          PIC S9(7)           VALUE +0.    
00181     03  FATAL-CANCEL-CNT         PIC S9(7)           VALUE +0.    
00182     03  FATAL-CLM-PMT-CNT        PIC S9(7)           VALUE +0.    
00183     03  FATAL-CLM-RES-CNT        PIC S9(7)           VALUE +0.    
00184     03  HOLD-ISSUE-CNT           PIC S9(7)           VALUE +0.    
00185     03  HOLD-CANCEL-CNT          PIC S9(7)           VALUE +0.    
00186     03  ISSUE-CNT                PIC S9(7)           VALUE +0.    
00187     03  LINE-CT                  PIC S9(3)           VALUE +99.   
00188     03  LOSS-RESERVES            PIC S9(7)           VALUE +0.    
00189     03  MAX-LINES                PIC S9(3)           VALUE +50.   
00190     03  PGE-CT                   PIC S9(5)           VALUE +0.    
00191     03  PND-RECS                 PIC S9(7)           VALUE +0.    
00192     03  RESERVE-TOTAL            PIC S9(7)V99        VALUE +0.    
00193     03  RETURNED-ISSUE-CNT       PIC S9(7)           VALUE +0.    
00194     03  RETURNED-CANCEL-CNT      PIC S9(7)           VALUE +0.    
00195     03  SUB                      PIC S999            VALUE +0.    
00196     03  T-SEQ                    PIC S999            VALUE +0.    
00197     03  TEST-CNT                 PIC S999            VALUE +0.    
00198     03  UNFORCE-ISSUE-CNT        PIC S9(7)           VALUE +0.    
00199     03  UNFORCE-CANCEL-CNT       PIC S9(7)           VALUE +0.    
00200     03  UNFORCE-CLM-PMT-CNT      PIC S9(7)           VALUE +0.    
00201     03  UNFORCE-CLM-RES-CNT      PIC S9(7)           VALUE +0.    
00202                                                                   
00203     03  TPA-LPREM                PIC S9(9)V99        VALUE +0.    
00204     03  TPA-LREF                 PIC S9(9)V99        VALUE +0.    
00205     03  TPA-APREM                PIC S9(9)V99        VALUE +0.    
00206     03  TPA-AREF                 PIC S9(9)V99        VALUE +0.    
00207     03  TPA-CLM                  PIC S9(9)V99        VALUE +0.    
00208     03  TPA-LRES                 PIC S9(9)V99        VALUE +0.    
00209                                                                   
00210     03  TNA-LPREM                PIC S9(9)V99        VALUE +0.    
00211     03  TNA-LREF                 PIC S9(9)V99        VALUE +0.    
00212     03  TNA-APREM                PIC S9(9)V99        VALUE +0.    
00213     03  TNA-AREF                 PIC S9(9)V99        VALUE +0.    
00214     03  TNA-CLM                  PIC S9(9)V99        VALUE +0.    
00215     03  TNA-LRES                 PIC S9(9)V99        VALUE +0.    
00216                                                                   
00217     03  FTA-LPREM                PIC S9(9)V99        VALUE +0.    
00218     03  FTA-LREF                 PIC S9(9)V99        VALUE +0.    
00219     03  FTA-APREM                PIC S9(9)V99        VALUE +0.    
00220     03  FTA-AREF                 PIC S9(9)V99        VALUE +0.    
00221     03  FTA-CLM                  PIC S9(9)V99        VALUE +0.    
00222     03  FTA-LRES                 PIC S9(9)V99        VALUE +0.    
00223                                                                   
00224     03  TOT-ENT-LF-ISSUE         PIC S9(9)V99        VALUE +0.    
00225     03  TOT-ENT-AH-ISSUE         PIC S9(9)V99        VALUE +0.    
00226     03  TOT-ENT-ISSUE-CNT        PIC S9(9)V99        VALUE +0.    
00227     03  TOT-REM-LF-ISSUE         PIC S9(9)V99        VALUE +0.    
00228     03  TOT-REM-AH-ISSUE         PIC S9(9)V99        VALUE +0.    
00229     03  TOT-REM-ISSUE-CNT        PIC S9(9)V99        VALUE +0.    
00230     03  TOT-ENT-LF-CANCEL        PIC S9(9)V99        VALUE +0.    
00231     03  TOT-ENT-AH-CANCEL        PIC S9(9)V99        VALUE +0.    
00232     03  TOT-ENT-CANCEL-CNT       PIC S9(9)V99        VALUE +0.    
00233     03  TOT-REM-LF-CANCEL        PIC S9(9)V99        VALUE +0.    
00234     03  TOT-REM-AH-CANCEL        PIC S9(9)V99        VALUE +0.    
00235     03  TOT-REM-CANCEL-CNT       PIC S9(9)V99        VALUE +0.    
00236     03  TMP-LF-OUT               PIC S9(9)V99        VALUE +0.    
00237     03  TMP-AH-OUT               PIC S9(9)V99        VALUE +0.    
00238     03  TMP-CNT-OUT              PIC S9(9)V99        VALUE +0.    
00239     03  TOT-LF-OUT               PIC S9(9)V99        VALUE +0.    
00240     03  TOT-AH-OUT               PIC S9(9)V99        VALUE +0.    
00241     03  TOT-CNT-OUT              PIC S9(9)V99        VALUE +0.    
00242                                                                   
00243  01  WS-WORK-AREAS.                                               
00244     03  WS-ERROR-2736            PIC S9(4)  COMP   VALUE +2736.   
00245     03  WS-DISPLAY-ERR           PIC 9(4).                        
00246     03  ABEND-CODE               PIC X(4)          VALUE ZERO.    
00247     03  ABEND-OPTION             PIC X             VALUE 'Y'.     
00248     03  EOF-SW                   PIC X             VALUE SPACE.   
00249     03  NEW-ACCT                 PIC X             VALUE SPACE.   
00250     03  OLC-REPORT-NAME          PIC X(6)          VALUE 'EL515'. 
00251     03  WS-ABEND-FILE-STATUS     PIC XX            VALUE ZERO.    
00252     03  WS-ABEND-MESSAGE         PIC X(80)         VALUE SPACES.  
00253     03  WS-ABEND-PROGRAM         PIC X(8)          VALUE SPACES.  
00254     03  WS-RETURN-CODE           PIC S9(4)         VALUE +0.      
00255     03  WS-ZERO                  PIC S9     COMP-3 VALUE +0.      
00256     03  X                        PIC X.                           
00257     03  X-MAX                    PIC S9(5)  COMP-3 VALUE +1000.   
00258     03  X-SEQ                    PIC S9(5)  COMP-3 VALUE +0.      
00259     03  PGM-SUB                  PIC S999   COMP   VALUE +515.    
00260     03  CLAIM-SAVE               PIC X(3).                        
00261     03  PGM-NAME                 PIC X(8)          VALUE SPACE.   
00262     03  WS-TEST-INPUT-DT.                                         
00263        05  WS-TID-MM             PIC 99.                          
00264        05  WS-TID-DD             PIC 99.                          
00265        05  WS-TID-YY             PIC 99.                          
00266     03  WS-TEST-MAINT-DT.                                         
00267        05  WS-TMD-MM             PIC 99.                          
00268        05  WS-TMD-DD             PIC 99.                          
00269        05  WS-TMD-YY             PIC 99.                          
00270     03  WS-ERR-CODE-X.                                            
00271        05  FILLER                PIC 99.                          
00272        05  WS-ERROR-SUB          PIC 99.                          
00273     03  WS-ERR-CODE REDEFINES WS-ERR-CODE-X                       
00274                                  PIC 9(4).                        
00275                                                                   
00276     03  ER-2600                  PIC X(4)          VALUE '2600'.  
00277     03  ER-2625                  PIC X(4)          VALUE '2625'.  
00278     03  ER-2725                  PIC X(4)          VALUE '2725'.  
00279     03  ER-2695                  PIC X(4)          VALUE '2695'.  
00280     03  ER-2800                  PIC X(4)          VALUE '2800'.  
00281     03  ER-2825                  PIC X(4)          VALUE '2825'.  
00282     03  ER-2925                  PIC X(4)          VALUE '2925'.  
00283                                                                   
00284     03  HIGHEST-BATCH            PIC X(6)          VALUE SPACE.   
00285     03  WS-CONTD.                                                 
00286        05  FILLER                PIC X(5)          VALUE '(CONT'. 
00287        05  FILLER                PIC X             VALUE QUOTE.   
00288        05  FILLER                PIC X(3)          VALUE 'D.)'.   
00289                                                                   
091604    03  ERNOTE-FILE-STATUS       PIC XX            VALUE '00'.
00290     03  PB-STATUS-CODE.                                           
00291        05  PB-STATUS-1            PIC X.                          
00292        05  PB-STATUS-2            PIC X.                          
00293     03  PC-STATUS-CODE.                                           
00294        05  PC-STATUS-1            PIC X.                          
00295        05  PC-STATUS-2            PIC X.                          
00296     03  CC-STATUS-CODE.                                           
00297        05  CC-STATUS-1            PIC X.                          
00298        05  CC-STATUS-2            PIC X.                          
00299     03  AM-STATUS-CODE.                                           
00300        05  AM-STATUS-1            PIC X.                          
00301        05  AM-STATUS-2            PIC X.                          
00302     03  EM-STATUS-CODE.                                           
00303        05  EM-STATUS-1            PIC X.                          
00304        05  EM-STATUS-2            PIC X.                          
00305     03  CN-STATUS-CODE.                                           
00306        05  CN-STATUS-1            PIC X.                          
00307        05  CN-STATUS-2            PIC X.                          
00308  EJECT                                                            
00309  01  SAVE-BATCH-RECORDS.                                          
00310     03  SAVE-BATCH     OCCURS 1000.                               
00311       05  SBR-SEQ                 PIC S9(5)     COMP-3.           
00312       05  SAVE-BATCH-NO           PIC X(6).                       
00313       05  SAVE-BATCH-DT           PIC XX.                         
00314       05  BATCH-TRAIL.                                            
00315         07  FILLER                PIC X(10).                      
00316         07  REM-LF-ISSUE          PIC S9(9)V99  COMP-3.           
00317         07  ENT-LF-ISSUE          PIC S9(9)V99  COMP-3.           
00318         07  COM-LF-ISSUE          PIC S9(9)V99  COMP-3.           
00319         07  REM-LF-CANCEL         PIC S9(9)V99  COMP-3.           
00320         07  ENT-LF-CANCEL         PIC S9(9)V99  COMP-3.           
00321         07  COM-LF-CANCEL         PIC S9(9)V99  COMP-3.           
00322         07  REM-AH-ISSUE          PIC S9(9)V99  COMP-3.           
00323         07  ENT-AH-ISSUE          PIC S9(9)V99  COMP-3.           
00324         07  COM-AH-ISSUE          PIC S9(9)V99  COMP-3.           
00325         07  REM-AH-CANCEL         PIC S9(9)V99  COMP-3.           
00326         07  ENT-AH-CANCEL         PIC S9(9)V99  COMP-3.           
00327         07  COM-AH-CANCEL         PIC S9(9)V99  COMP-3.           
00328         07  REM-ISSUE-CNT         PIC S9(5)     COMP-3.           
00329         07  ENT-ISSUE-CNT         PIC S9(5)     COMP-3.           
00330         07  REM-CANCEL-CNT        PIC S9(5)     COMP-3.           
00331         07  ENT-CANCEL-CNT        PIC S9(5)     COMP-3.           
00332                                                                   
00333  01  CL-TOTALS         COMP-3.                                    
00334      12  CL-TOT-LPAY         PIC S9(9)V99           VALUE +0.     
00335      12  CL-TOT-APAY         PIC S9(9)V99           VALUE +0.     
00336      12  CL-TOT-64CDT        PIC S9(9)V99           VALUE +0.     
00337      12  CL-TOT-PTC          PIC S9(9)V99           VALUE +0.     
00338      12  CL-TOT-IBNR         PIC S9(9)V99           VALUE +0.     
00339      12  CL-TOT-EXP          PIC S9(9)V99           VALUE +0.     
00340                                                                   
00341    EJECT                                                          
00342  01  USER-SUMMARY-TOTALS.                                         
00343      12  USER-SUM-SRCH           PIC XXXX.                        
00344      12  USER-SUM-TBL   OCCURS 101 TIMES                          
00345                         INDEXED BY USTINDX                        
00346                                    USTINDXHI1                     
00347                                    USTINDXHI2.                    
00348          16  UST-USER            PIC XXXX.                        
00349          16  UST-BAT-INP         PIC S9(5)           COMP-3.      
00350          16  UST-DET-INP         PIC S9(5)           COMP-3.      
00351          16  UST-BAT-MNTC        PIC S9(5)           COMP-3.      
00352          16  UST-DET-MNTC        PIC S9(5)           COMP-3.      
00353          16  UST-UPFRC-PROC      PIC S9(5)           COMP-3.
102706         16  UST-SIG-SW          PIC S9(5)           COMP-3.
00354                                                                   
00355  01  SUMMARY-WORK-AREAS.                                          
00356      12  WKDATE               PIC 9(08).                          
00357      12  WKDATE-R REDEFINES WKDATE.                               
00358          16  WKCC             PIC 99.                             
00359          16  WKYR             PIC 99.                             
00360          16  WKMO             PIC 99.                             
00361          16  WKDA             PIC 99.                             
00362      12  EDIT-DATE.                                               
00363          16  EDMO             PIC XX.                             
00364          16  FILLER           PIC X VALUE '/'.                    
00365          16  EDDA             PIC XX.                             
00366          16  FILLER           PIC X VALUE '/'.                    
00367          16  EDYR             PIC XX.                             
00368      12  RUNDT.                                                   
00369          16  RNCC             PIC 99.                             
00370          16  RNYR             PIC 99.                             
00371          16  RNMO             PIC 99.                             
00372          16  RNDA             PIC 99.                             
00373      12  SELDT                PIC 9(08).                          
00374      12  SELDT-R  REDEFINES SELDT.                                
00375          16  SLCC             PIC 99.                             
00376          16  SLYR             PIC 99.                             
00377          16  SLMO             PIC 99.                             
00378          16  SLDA             PIC 99.                             
00379      12  EARLIEST-DATE.                                           
00380          16  EACC             PIC 99 VALUE 99.                    
00381          16  EAYR             PIC 99 VALUE 99.                    
00382          16  EAMO             PIC 99 VALUE 99.                    
00383          16  EADA             PIC 99 VALUE 99.                    
00384      12  LATEST-DATE.                                             
00385          16  LACC             PIC 99  VALUE 0.                    
00386          16  LAYR             PIC 99  VALUE 0.                    
00387          16  LAMO             PIC 99  VALUE 0.                    
00388          16  LADA             PIC 99  VALUE 0.                    
00389                                                                   
00390      12  ISSUE-CANCEL         PIC X.                              
00391          88  ISSUES              VALUE '1'.                       
00392          88  CANCELS             VALUE '2'.                       
00393                                                                   
00394      12  OVER30               PIC S9(7)      COMP-3  VALUE +0.    
00395      12  OVER60               PIC S9(7)      COMP-3  VALUE +0.    
00396      12  OVER90               PIC S9(7)      COMP-3  VALUE +0.    
00397                                                                   
00398  01  CONTROL-KEYS.                                                
00399      12  CURR-KEY.                                                
00400          16  CUR-CARR        PIC X.                               
00401          16  CUR-GRP         PIC X(6).                            
00402          16  CUR-ST          PIC XX.                              
00403          16  CUR-ACCT        PIC X(10).                           
00404      12  PREV-KEY.                                                
00405          16  PRV-CARR        PIC X.                               
00406          16  PRV-GRP         PIC X(6).                            
00407          16  PRV-ST          PIC XX.                              
00408          16  PRV-ACCT        PIC X(10).                           
00409                                                                   
00410       EJECT                                                       
00411  01  HDG1.                                                        
           12  FILLER              PIC X(12) VALUE SPACES.
           12  HDG1-CSR            PIC X(4)  VALUE SPACES.
           12  FILLER              PIC X(28) VALUE SPACES.
00412 *    12  FILLER              PIC X(44) VALUE SPACES.              
00413      12  FILLER              PIC X(27) VALUE                      
00414                    'LISTING OF PENDING BUSINESS'.                 
00415      12  ER-ONLY             PIC X(48) VALUE SPACES.              
00416      12  CL-515              PIC X(7)  VALUE 'EL515A '.           
00417                                                                   
00418  01  HDG2.                                                        
00419      12  FILLER              PIC X(43) VALUE SPACES.              
00420      12  H2-COMP             PIC X(30).                           
00421      12  FILLER              PIC X(46) VALUE SPACES.              
00422      12  H2-DATE             PIC X(8).                            
00423                                                                   
00424  01  HDG3.                                                        
00425      12  FILLER              PIC X(49) VALUE SPACES.              
00426      12  H3-DATE             PIC X(18).                           
00427      12  FILLER              PIC X(49) VALUE SPACES.              
00428      12  FILLER              PIC X(5)  VALUE 'PAGE '.             
00429      12  H3-PAGE             PIC ZZ,ZZZ.                          
00430                                                                   
00431  01  HDG3A.                                                       
00432      12  FILLER              PIC X(32) VALUE
00433                    ' CARR   GRP  ST ACCOUNT      CSR'.

00435  01  HDG4.                                                        
00436      12  FILLER              PIC XX   VALUE SPACES.               
00437      12  H4-CARR             PIC X.                               
00438      12  FILLER              PIC XXX  VALUE SPACES.               
00439      12  H4-GRP              PIC X(6).                            
00440      12  FILLER              PIC X    VALUE SPACES.               
00441      12  H4-ST               PIC XX.                              
00442      12  FILLER              PIC X    VALUE SPACES.               
00443      12  H4-ACCT             PIC X(10).                           
00444      12  FILLER              PIC XXX  VALUE SPACES.
           12  H4-CSR              PIC X(4) VALUE SPACE.
           12  FILLER              PIC XXX  VALUES SPACES.
00445      12  H4-NAME             PIC X(20).                           
00446      12  FILLER              PIC XXX  VALUE SPACES.               
00447      12  H4-TOTAL            PIC X(15) VALUE SPACES.              
00448      12  FILLER              PIC X     VALUE SPACES.              
00449      12  H4-CONTD            PIC X(9)  VALUE SPACES.

00451                                                                   
00452  01  IC-TOT-HD1.                                                  
00453      12  FILLER              PIC X(15) VALUE SPACES.              
00454      12  FILLER              PIC X(53) VALUE                      
00455             'BATCH  DATED      ---------- REMITTED ----------'.   
00456      12  FILLER              PIC X(35) VALUE                      
00457             '---------- ENTERED ---------'.                       
00458      12  FILLER              PIC X(25) VALUE                      
00459             '----- OUT OF BALANCE ----'.                          
00460                                                                   
00461  01  IC-TOT-HD2.                                                  
00462      12  FILLER              PIC X(34) VALUE SPACES.              
00463      12  IC2-LF              PIC XX    VALUE 'LF'.                
00464      12  FILLER              PIC X(9)  VALUE ' PREMIUM'.          
00465      12  IC2-AH              PIC XX    VALUE 'AH'.                
00466      12  FILLER              PIC X(20) VALUE ' PREMIUM    CNT'.   
00467      12  IC2-LF2             PIC XX    VALUE 'LF'.                
00468      12  FILLER              PIC X(9)  VALUE ' PREMIUM'.          
00469      12  IC2-AH2             PIC XX    VALUE 'AH'.                
00470      12  FILLER              PIC X(20) VALUE ' PREMIUM    CNT'.   
00471      12  IC2-LF3             PIC XX    VALUE 'LF'.                
00472      12  FILLER              PIC X(9)  VALUE ' PREMIUM'.          
00473      12  IC2-AH3             PIC XX    VALUE 'AH'.                
00474      12  FILLER              PIC X(20) VALUE ' PREMIUM    CNT'.   
00475                                                                   
00476  01  IC-TOT-HD2-1.                                                
00477      12  FILLER              PIC X(34) VALUE SPACES.              
00478      12  IC2-LF-1            PIC XX    VALUE 'LF'.                
00479      12  FILLER              PIC X(9)  VALUE ' REFUND'.           
00480      12  IC2-AH-1            PIC XX    VALUE 'AH'.                
00481      12  FILLER              PIC X(20) VALUE ' REFUND     CNT'.   
00482      12  IC2-LF2-1           PIC XX    VALUE 'LF'.                
00483      12  FILLER              PIC X(9)  VALUE ' REFUND'.           
00484      12  IC2-AH2-1           PIC XX    VALUE 'AH'.                
00485      12  FILLER              PIC X(20) VALUE ' REFUND     CNT'.   
00486      12  IC2-LF3-1           PIC XX    VALUE 'LF'.                
00487      12  FILLER              PIC X(9)  VALUE ' REFUND'.           
00488      12  IC2-AH3-1           PIC XX    VALUE 'AH'.                
00489      12  FILLER              PIC X(20) VALUE ' REFUND     CNT'.   
00490                                                                   
00491  01  CL-TOT-HD1.                                                  
00492      12  FILLER              PIC X(20) VALUE                      
00493             ' ** CLAIM TOTALS **'.                                
00494                                                                   
00495  01  CL-TOT-HD2.                                                  
00496      12  FILLER              PIC XX    VALUE  SPACES.             
00497      12  CL2-LF              PIC XXX   VALUE  'LF'.               
00498      12  FILLER              PIC X(15) VALUE  ' PAYMENTS'.        
00499      12  CL2-AH              PIC XXX   VALUE  'AH'.               
00500      12  FILLER              PIC X(12) VALUE  ' PAYMENTS'.        
00501      12  FILLER              PIC X(16) VALUE  ' 64CDT RESERVE'.   
00502      12  FILLER              PIC X(49) VALUE                      
00503             'P.T.C. RESERVE   IBNR RESERVE    EXPENSE PAYMENTS'.  
00504      EJECT                                                        
00505                                                                   
00506  01  ISSUE-HD1.                                                   
00507      12  FILLER              PIC X(100) VALUE                     
00508            ' *** ISSUE TRANSACTIONS ***'.                         
00509  01  ISSUE-TOT-HD1.                                               
00510      12  FILLER              PIC X(100) VALUE                     
00511            ' *** ACCOUNT ISSUE TRANSACTION TOTALS ***'.           
00512  01  ISSUE-HD2.                                                   
00513      12  FILLER              PIC X(39) VALUE                      
00514            ' CERT       EFFECTIVE     NAME    SEX'.               
00515      12  FILLER              PIC X(12) VALUE 'TERM-------'.       
00516      12  IH2-LF              PIC XXX   VALUE 'LF'.                
00517      12  FILLER              PIC X(16) VALUE ' DATA ----------'.  
00518      12  FILLER              PIC X(11) VALUE '  TERM----'.        
00519      12  IH2-AH              PIC XXX   VALUE 'AH'.                
00520      12  FILLER              PIC X(16) VALUE ' DATA ----------'.  
00521                                                                   
00522  01  ISSUE-HD3.                                                   
00523      12  FILLER              PIC X(52) VALUE                      
00524            ' NUMBER       DATE     SOC-SEC-NO AGE   CP   TYP  '.  
00525      12  FILLER              PIC X(50) VALUE                      
00526            'BENEFIT    PREMIUM   CP  TYP MO BENEFIT  PREMIUM'.    
00527      12  FILLER              PIC X(30) VALUE                      
00528            ' I A.P.R  REI F E  INPUT BATCH'.                      
00529                                                                   
00530  01  ISSUE-HD4.                                                   
00531      12  FILLER              PIC X(21)      VALUE SPACES.         
00532      12  FILLER              PIC X(23)      VALUE                 
00533            '   MEMBER NO.     FREQ '.                             
00534      12  FILLER              PIC X(59)      VALUE SPACES.         
00535      12  FILLER              PIC X(29)      VALUE                 
00536            'G CL/DEV SIG C C   STAT  USER'.                       
00537                                                                   
00538  01  ISSUE-HD5.                                                   
00539      12  FILLER              PIC X(105)     VALUE SPACES.         
00540      12  FILLER              PIC X(22)      VALUE                 
00541            'LIVES  SKP       BILL'.                               
00542      EJECT                                                        
00543                                                                   
00544  01  CANCEL-HD1.                                                  
00545      12  FILLER              PIC X(100) VALUE                     
00546            ' *** CANCEL TRANSACTIONS ***'.                        
00547                                                                   
00548  01  CANCEL-TOT-HD1.                                              
00549      12  FILLER              PIC X(100) VALUE                     
00550            ' *** ACCOUNT CANCEL TRANSACTION TOTALS ***'.          
00551                                                                   
00552  01  CANCEL-HD2.                                                  
00553      12  FILLER              PIC X(44)           VALUE            
00554              '  CERT       EFFECTIVE   NAME     LF CANCEL '.      
00555      12  FILLER              PIC X(08)           VALUE            
00556              '------- '.                                          
00557      12  CH2-LF              PIC X(02)           VALUE 'LF'.      
00558      12  FILLER              PIC X(17)           VALUE            
00559              ' ------- ------- '.                                 
00560      12  CH2-AH              PIC X(02)           VALUE 'AH'.      
00561      12  FILLER              PIC X(15)           VALUE            
00562              ' -------   PD T'.                                   
00563      12  FILLER              PIC X(13)           VALUE            
00564              'HRU   PRIOR  '.                                     
00565      12  CH2-LF2             PIC X(02)           VALUE 'LF'.      
00566      12  FILLER              PIC X(29)           VALUE            
00567              '-METH   APR  F  INPUT  BATCH '.                     
00568                                                                   
00569  01  CANCEL-HD3.                                                  
00570      12  FILLER              PIC X(44)           VALUE            
00571              ' NUMBER        DATE    SOC-SEC-NO AH CANCEL '.      
00572      12  FILLER              PIC X(44)           VALUE            
00573              'TERM TYP    REFUND TERM TYP    REFUND    DEA'.      
00574      12  FILLER              PIC X(13)           VALUE            
00575              'TH   CANCEL  '.                                     
00576      12  CH3-AH              PIC X(02)           VALUE 'AH'.      
00577      12  FILLER              PIC X(29)           VALUE            
00578              '-METH  LIVES C STATUS   USER '.                     
00579                                                                   
00580  01  CANCEL-HD4.                                                  
00581      12  FILLER              PIC X(44)           VALUE            
00582              '                       MEMBER NO            '.      
00583      12  FILLER              PIC X(44)           VALUE            
00584              ' REM                REM                     '.      
00585      12  FILLER              PIC X(44)           VALUE            
00586              '                               BILL         '.      
00587                                                                   
00588      EJECT                                                        
00589                                                                   
00590  01  CLAIM-HD1.                                                   
00591      12  FILLER              PIC X(100) VALUE                     
00592              '*** CLAIM PAYMENTS AND RESERVES ***'.               
00593                                                                   
00594  01  CLAIM-HD2.                                                   
00595      12  FILLER              PIC X(52) VALUE                      
00596            ' CAR GRP  ST ACCOUNT    CERT          EFF.   CLAIM'.  
00597      12  FILLER              PIC X(46) VALUE                      
00598            'INSURED CLM-TYP CHECK   PMT/RESV   DATE PD'.          
00599      12  FILLER              PIC X(33) VALUE                      
00600            'INCURRED  TERM REM BEN    F INPUT'.                   
00601                                                                   
00602  01  CLAIM-HD3.                                                   
00603      12  FILLER              PIC X(24) VALUE SPACES.              
00604      12  FILLER              PIC X(45) VALUE                      
00605            'NUMBER        DATE    NO.'.                           
00606      12  FILLER              PIC X(18) VALUE                      
00607            'NO.      TYPE     '.                                  
00608      12  HD3-HDG             PIC X(07) VALUE 'PD THRU'.           
00609      12  FILLER              PIC X(38) VALUE                      
00610            '    REPORTED  REM  PREV PD    C STATUS'.              
00611                                                                   
00612  01  CHANGE-HD1.                                                  
00613      12  FILLER              PIC X(100) VALUE                     
00614              ' *** CHANGES TO MASTER FILE ***'.                   
00615  01  CHANGE-HD2.                                                  
00616      12  FILLER              PIC X(47) VALUE                      
00617            ' CAR GRP    ST ACCOUNT     CERT NO.   EFF DT'.        
00618      12  FILLER              PIC X(55) VALUE                      
00619        'NAME        AGE   APR   JT.NAME   SOC-SEC-NO ATRM LTRM'.  
00620                                                                   
00621      12  FILLER              PIC X(30) VALUE                      
00622            'I/G   PREM-TYPE   BY   ENTRY'.                        
00623                                                                   
00624  01  CHANGE-HD3.                                                  
00625      12  FILLER              PIC X(47) VALUE SPACES.              
00626      12  FILLER              PIC X(55) VALUE                      
00627        '            SEX   FORM            MEMBER NO   LIVES'.     
00628                                                                   
00629      12  FILLER              PIC X(30) VALUE                      
00630            'LOAN    BAL       OFF'.                               
00631                                                                   
00632  01  USER-TOT-HD1.                                                
00633      12  FILLER                  PIC X(31)                        
00634              VALUE ' *** SUMMARY TOTALS BY USER ***'.             
00635                                                                   
00636  01  USER-TOT-HD2.                                                
00637      12  FILLER                  PIC X(26)                        
00638              VALUE ' USER  -- BATCHES --      '.                  
00639      12  FILLER                  PIC X(36)
00640              VALUE '---------- ISSUES/CANCELS ----------'.                 
00641                                                                   
00642  01  USER-TOT-HD3.                                                
00643      12  FILLER                  PIC X(26)                        
00644              VALUE '       INPUT  UPDATE'.                        
00645      12  FILLER                  PIC X(38)                        
00646              VALUE 'INPUT  UPDATE  UPDT & FORCE PART B SIG'.
00647                                                                   
00648      EJECT                                                        
00649  01  ISSUE-PRINT-LINE.                                            
00650      12  ISPL-CERT           PIC X(11).                           
00651      12  FILLER              PIC X.                               
00652      12  ISPL-EFF-DT         PIC X(8).                            
00653      12  FILLER              PIC X.                               
00654      12  ISPL-NM.                                                 
00655          16  ISPL-INIT       PIC XX.                              
00656          16  FILLER          PIC X.                               
00657          16  ISPL-NAME       PIC X(9).                            
00658      12  FILLER              PIC XX.                              
00659      12  ISPL-SEX            PIC X.                               
00660      12  FILLER              PIC X(3).                            
00661      12  ISPL-LFTRM          PIC ZZZ.                             
00662      12  ISPL-ALT            PIC XXX.                             
00663      12  ISPL-LCOD           PIC XX.                              
00664      12  ISPL-LBEN           PIC ZZZZZZ,ZZZ.ZZ-.                  
00665      12  ISPL-LBEN-2 REDEFINES ISPL-LBEN                          
00666                              PIC ZZZZZ,ZZZ,ZZZ-.                  
00667      12  ISPL-LPRM           PIC ZZZZZZZ.ZZ-.                     
00668      12  FILLER              PIC X.                               
00669      12  ISPL-AHTRM          PIC ZZZ.                             
00670      12  FILLER              PIC X.                               
00671      12  ISPL-ACOD           PIC XXX.                             
00672      12  ISPL-ABEN           PIC ZZZZ,ZZZ.ZZ-.                    
00673      12  ISPL-ABEN-2 REDEFINES ISPL-ABEN                          
00674                              PIC ZZZ,ZZZ,ZZZ-.                    
00675      12  ISPL-APRM           PIC ZZ,ZZZ.ZZ-.                      
00676      12  FILLER              PIC X.                               
00677      12  ISPL-IG             PIC X.                               
00678      12  FILLER              PIC X.                               
00679      12  ISPL-APR            PIC ZZ.ZZZZ.                         
00680      12  FILLER              PIC X.                               
00681      12  ISPL-REI            PIC X.                               
00682      12  FILLER              PIC XX.                              
00683      12  ISPL-FC             PIC X.                               
00684      12  FILLER              PIC X.                               
00685      12  ISPL-EC             PIC X.                               
00686      12  ISPL-INPUT          PIC X(6).                            
00687      12  FILLER              PIC X.                               
00688      12  ISPL-BATCH          PIC X(6).                            
00689                                                                   
00690  01  SECOND-ISSUE-LINE REDEFINES ISSUE-PRINT-LINE.                
00691      12  FILLER              PIC X(21).                           
00692      12  ISPL-SOC-SEC.                                            
00693        14  ISPL-SOC-SEC-2    PIC XX.                              
00694        14  FILLER            PIC X(9).                            
00695      12  FILLER              PIC XXX.                             
00696      12  ISPL-AGE            PIC ZZ.                              
00697      12  FILLER              PIC XX.                              
00698      12  ISPL-LFCP           PIC ZZZ.                             
00699      12  FILLER              PIC XXX.                             
00700      12  ISPL-LTYP           PIC XX.                              
00701      12  FILLER              PIC XX.                              
00702      12  ISPL-LOBR           PIC ZZ.ZZZZ.                         
00703      12  FILLER              PIC X.                               
00704      12  ISPL-LPRM1          PIC ZZZZZZZ,ZZ9.99-.                 
00705      12  FILLER              PIC X.                               
00706      12  ISPL-AHCP           PIC ZZZ.                             
00707      12  FILLER              PIC X.                               
00708      12  ISPL-ATYP           PIC XX.                              
00709      12  FILLER              PIC XX.                              
00710      12  ISPL-AOBR           PIC ZZ.ZZZZ.                         
00711      12  FILLER              PIC X.                               
00712      12  ISPL-APRM1          PIC ZZZZZ,ZZ9.99-.                   
00713      12  FILLER              PIC XXX.                             
00714      12  ISPL-BC             PIC XX.                              
00715      12  FILLER              PIC X.                               
00716      12  ISPL-DEV            PIC XXX.                             
00717      12  FILLER              PIC XX.                              
00718      12  ISPL-SIG            PIC X.                               
00719      12  FILLER              PIC X(7).                            
00720      12  ISPL-STAT.                                               
00721          16  ISPL-CL         PIC XXX.                             
00722          16  ISPL-LEVEL      PIC 999.                             
00723      12  FILLER              PIC X.                               
00724      12  ISPL-WHO            PIC X(4).                            
00725                                                                   
00726  01  THIRD-ISSUE-LINE REDEFINES ISSUE-PRINT-LINE.                 
00727      12  FILLER              PIC X(17).                           
00728      12  ISPL-MEMBER.                                             
00729        14  ISPL-MEMBER-2     PIC XX.                              
00730        14  FILLER            PIC X(14).                           
00731      12  FILLER              PIC X(6).                            
00732      12  ISPL-FREQ           PIC Z(4).                            
00733      12  FILLER              PIC XX.                              
00734      12  ISPL-LTYP1          PIC XXX.                             
00735      12  FILLER              PIC X(10).                           
00736      12  ISPL-LPRM2          PIC ZZZZZZ,ZZZ.ZZ-.                  
00737      12  FILLER              PIC X(5).                            
00738      12  ISPL-ATYP1          PIC XXX.                             
00739      12  FILLER              PIC X(7).                            
00740      12  ISPL-APRM2          PIC ZZZZZZZ,ZZZ.ZZ-.                 
00741      12  FILLER              PIC X.                               
00742      12  ISPL-LIVES          PIC Z(7).                            
00743      12  FILLER              PIC XXX.                             
00744      12  ISPL-SKP            PIC X.                               
00745      12  FILLER              PIC X(5).                            
00746      12  ISPL-BILL           PIC X(6).                            
00747                                                                   
00748      EJECT                                                        
00749  01  CANCEL-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.                
00750      12  FILLER              PIC X.                               
00751      12  CNPL-CERT           PIC X(11).                           
00752      12  FILLER              PIC X.                               
00753      12  CNPL-EFF-DT         PIC X(8).                            
00754      12  FILLER              PIC X.                               
00755      12  CNPL-NM.                                                 
00756          16  CNPL-INIT       PIC XX.                              
00757          16  FILLER          PIC X.                               
00758          16  CNPL-NAME       PIC X(10).                           
00759      12  FILLER              PIC X.                               
00760      12  CNPL-LFCANC         PIC X(6).                            
00761      12  FILLER              PIC XXX.                             
00762      12  CNPL-LFTRM          PIC ZZZ.                             
00763      12  FILLER              PIC X.                               
00764      12  CNPL-LCOD           PIC XX.                              
00765      12  CNPL-LREF           PIC ZZZZ,ZZZ.ZZ-.                    
00766      12  FILLER              PIC X.                               
00767      12  CNPL-AHTRM          PIC ZZZ.                             
00768      12  FILLER              PIC X.                               
00769      12  CNPL-ACOD           PIC XX.                              
00770      12  CNPL-AREF           PIC ZZZZ,ZZZ.ZZ-.                    
00771      12  FILLER              PIC X.                               
00772      12  CNPL-PDTHRU         PIC X(8).                            
00773      12  FILLER              PIC X.                               
00774      12  CNPL-LF-CANCEL      PIC X(8).                            
00775      12  FILLER              PIC X.                               
00776      12  CNPL-LFMETH         PIC X(7).                            
00777      12  FILLER              PIC X.                               
00778      12  CNPL-APR            PIC ZZZ.ZZ.                          
00779      12  FILLER              PIC X.                               
00780      12  CNPL-FC             PIC X.                               
00781      12  FILLER              PIC XX.                              
00782      12  CNPL-INPUT          PIC X(6).                            
00783      12  FILLER              PIC X.                               
00784      12  CNPL-BATCH          PIC X(6).                            
00785                                                                   
00786  01  SECOND-CANCEL-LINE REDEFINES ISSUE-PRINT-LINE.               
00787      12  FILLER              PIC X(22).                           
00788      12  CNPL-SOC-SEC.                                            
00789          16  CNPL-SOC-SEC-2  PIC XX.                              
00790          16  FILLER          PIC X(9).                            
00791      12  FILLER              PIC XXX.                             
00792      12  CNPL-AHCANC         PIC X(6).                            
00793      12  FILLER              PIC XXX.                             
00794      12  CNPL-LFREM          PIC ZZZ.                             
00795      12  FILLER              PIC X.                               
00796      12  CNPL-LTYP           PIC XX.                              
00797      12  CNPL-LREF1          PIC ZZZZ,ZZZ.ZZ-.                    
00798      12  FILLER              PIC X.                               
00799      12  CNPL-AHREM          PIC ZZZ.                             
00800      12  FILLER              PIC X.                               
00801      12  CNPL-ATYP           PIC XX.                              
00802      12  CNPL-AREF1          PIC ZZZZ,ZZZ.ZZ-.                    
00803      12  FILLER              PIC X.                               
00804      12  CNPL-DEATH          PIC X(8).                            
00805      12  FILLER              PIC X.                               
00806      12  CNPL-AH-CANCEL      PIC X(8).                            
00807      12  FILLER              PIC X.                               
00808      12  CNPL-AHMETH         PIC X(7).                            
00809      12  FILLER              PIC XX.                              
00810      12  CNPL-LIVES          PIC ZZZZ.                            
00811      12  FILLER              PIC X(5).                            
00812      12  CNPL-STAT.                                               
00813          16  CNPL-CL         PIC XXX.                             
00814          16  CNPL-LEVEL      PIC 999.                             
00815      12  FILLER              PIC XX.                              
00816      12  CNPL-WHO            PIC X(4).                            
00817      12  FILLER              PIC X.                               
00818                                                                   
00819  01  THIRD-CANCEL-LINE REDEFINES ISSUE-PRINT-LINE.                
00820      12  FILLER              PIC X(22).                           
00821      12  CNPL-MEMBER.                                             
00822          16  CNPL-MEMBER-2   PIC XX.                              
00823          16  FILLER          PIC X(10).                           
00824      12  FILLER              PIC X(15).                           
00825      12  CNPL-LTYP1          PIC XXX.                             
00826      12  CNPL-LREF2          PIC ZZZ,ZZZ.ZZ-.                     
00827      12  FILLER              PIC X(5).                            
00828      12  CNPL-ATYP1          PIC XXX.                             
00829      12  CNPL-AREF2          PIC ZZZ,ZZZ.ZZ-.                     
00830      12  FILLER              PIC X(37).                           
00831      12  CNPL-BILL           PIC X(6).                            
00832      12  FILLER              PIC X(7).                            
00833      EJECT                                                        
00834                                                                   
00835  01  CLAIM-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.                 
00836      12  FILLER              PIC X.                               
00837      12  CLPL-CARR           PIC X.                               
00838      12  FILLER              PIC X.                               
00839      12  CLPL-GRP            PIC X(6).                            
00840      12  FILLER              PIC X.                               
00841      12  CLPL-ST             PIC XX.                              
00842      12  FILLER              PIC X.                               
00843      12  CLPL-ACCT           PIC X(10).                           
00844      12  FILLER              PIC X.                               
00845      12  CLPL-CERT           PIC X(11).                           
00846      12  FILLER              PIC X.                               
00847      12  CLPL-EFF-DT         PIC X(8).                            
00848      12  FILLER              PIC X.                               
00849      12  CLPL-CLAIMNO        PIC X(7).                            
00850      12  FILLER              PIC X.                               
00851      12  CLPL-INSURED        PIC X(7).                            
00852      12  FILLER              PIC X.                               
00853      12  CLPL-CLM            PIC XX.                              
00854      12  FILLER              PIC X.                               
00855      12  CLPL-TYP            PIC XX.                              
00856      12  FILLER              PIC X.                               
00857      12  CLPL-CHECKNO        PIC X(7).                            
00858      12  CLPL-CLMAMT         PIC ZZZZZZ.ZZ-.                      
00859      12  FILLER              PIC XXX.                             
00860      12  CLPL-PAID           PIC X(8).                            
00861      12  FILLER              PIC XXX.                             
00862      12  CLPL-INCURRED       PIC X(8).                            
00863      12  FILLER              PIC X.                               
00864      12  CLPL-TRM            PIC ZZZ.                             
00865      12  FILLER              PIC X.                               
00866      12  CLPL-BEN            PIC ZZZZZ.ZZ-.                       
00867      12  FILLER              PIC X.                               
00868      12  CLPL-FC             PIC X.                               
00869      12  FILLER              PIC X(4).                            
00870      12  CLPL-INPUT          PIC X(6).                            
00871                                                                   
00872  01  SECOND-CLAIM-LINE REDEFINES ISSUE-PRINT-LINE.                
00873      12  FILLER              PIC X(77).                           
00874      12  CLPL-TYPE           PIC X(7).                            
00875      12  FILLER              PIC XXX.                             
00876      12  CLPL-PDTHRU         PIC X(8).                            
00877      12  FILLER              PIC XXX.                             
00878      12  CLPL-REPORTED       PIC X(8).                            
00879      12  FILLER              PIC XX.                              
00880      12  CLPL-REM            PIC ZZZ.                             
00881      12  FILLER              PIC X.                               
00882      12  CLPL-PREV           PIC ZZZZZ.ZZ-.                       
00883      12  FILLER              PIC X(4).                            
00884      12  CLPL-STAT           PIC X(6).                            
00885                                                                   
00886  01  CHANGE-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.                
00887      12  FILLER              PIC X.                               
00888      12  CHPL-CARR           PIC X.                               
00889      12  FILLER              PIC XXX.                             
00890      12  CHPL-GRP            PIC X(6).                            
00891      12  FILLER              PIC X.                               
00892      12  CHPL-ST             PIC XX.                              
00893      12  FILLER              PIC X.                               
00894      12  CHPL-ACCT           PIC X(10).                           
00895      12  FILLER              PIC X.                               
00896      12  CHPL-CERT           PIC X(11).                           
00897      12  FILLER              PIC X.                               
00898      12  CHPL-EFF-DT         PIC X(8).                            
00899      12  FILLER              PIC X.                               
00900      12  CHPL-LNAME          PIC X(12).                           
00901      12  FILLER              PIC X.                               
00902      12  CHPL-AGE            PIC ZZ.                              
00903      12  FILLER              PIC X.                               
00904      12  CHPL-APR            PIC ZZ.ZZZZ.                         
00905      12  FILLER              PIC X.                               
00906      12  CHPL-JNAME          PIC X(10).                           
00907      12  FILLER              PIC X.                               
00908      12  CHPL-SOC-SEC        PIC X(11).                           
00909      12  FILLER              PIC X.                               
00910      12  CHPL-ATRM           PIC ZZZ.                             
00911      12  FILLER              PIC X.                               
00912      12  CHPL-LTRM           PIC ZZZ.                             
00913      12  FILLER              PIC X.                               
00914      12  CHPL-IG             PIC X.                               
00915      12  FILLER              PIC X(8).                            
00916      12  CHPL-PTYPE          PIC XXX.                             
00917      12  FILLER              PIC X(6).                            
00918      12  CHPL-BY             PIC X(4).                            
00919      12  FILLER              PIC X.                               
00920      12  CHPL-ENTRY          PIC X(6).                            
00921                                                                   
00922  01  SECOND-CHANGE-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.         
00923      12  FILLER              PIC X(47).                           
00924      12  CHPL-FIRST          PIC X(8).                            
00925      12  FILLER              PIC X(5).                            
00926      12  CHPL-SEX            PIC X.                               
00927      12  FILLER              PIC X.                               
00928      12  CHPL-JFIRST         PIC X(8).                            
00929      12  FILLER              PIC X.                               
00930      12  CHPL-FORM           PIC X(8).                            
00931      12  FILLER              PIC X.                               
00932      12  CHPL-MEMBER         PIC X(12).                           
00933      12  FILLER              PIC XX.                              
00934      12  CHPL-LIVES          PIC ZZZ.                             
00935      12  FILLER              PIC X.                               
00936      12  CHPL-LOAN           PIC X(6).                            
00937      12  FILLER              PIC X.                               
00938      12  CHPL-BAL            PIC ZZZZZZ.ZZ.                       
00939      12  FILLER              PIC X(4).                            
00940      12  CHPL-OFF            PIC X(6).                            
00941                                                                   
00942      EJECT                                                        
00943  01  IC-TOTAL-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.              
00944      12  FILLER              PIC X(14).                           
00945      12  ICPL-BATCH          PIC X(6).                            
00946      12  FILLER              PIC X.                               
00947      12  ICPL-DATE           PIC X(8).                            
00948      12  FILLER              PIC X.                               
00949      12  ICPL-LF-REM         PIC Z,ZZZ,ZZZ.99-.                   
00950      12  ICPL-AH-REM         PIC Z,ZZZ,ZZZ.99-.                   
00951      12  ICPL-CNT-REM        PIC ZZZZZ9-.                         
00952      12  ICPL-LF-ENT         PIC Z,ZZZ,ZZZ.99-.                   
00953      12  ICPL-AH-ENT         PIC Z,ZZZ,ZZZ.99-.                   
00954      12  ICPL-CNT-ENT        PIC ZZZZZ9-.                         
00955      12  ICPL-LF-OUT         PIC Z,ZZZ,ZZZ.99-.                   
00956      12  ICPL-AH-OUT         PIC Z,ZZZ,ZZZ.99-.                   
00957      12  ICPL-CNT-OUT        PIC ZZZZZ9-.                         
00958                                                                   
00959  01  CL-TOTAL-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.              
00960      12  FILLER              PIC XX.                              
00961      12  CTPL-LF-PAY         PIC Z,ZZZ,ZZZ.99-.                   
00962      12  FILLER              PIC X.                               
00963      12  CTPL-AH-PAY         PIC Z,ZZZ,ZZZ.99-.                   
00964      12  FILLER              PIC XXX.                             
00965      12  CTPL-64CDT          PIC Z,ZZZ,ZZZ.99-.                   
00966      12  FILLER              PIC X(4).                            
00967      12  CTPL-PTC            PIC Z,ZZZ,ZZZ.99-.                   
00968      12  FILLER              PIC XXX.                             
00969      12  CTPL-IBNR           PIC Z,ZZZ,ZZZ.99-.                   
00970      12  FILLER              PIC X(6).                            
00971      12  CTPL-EXP            PIC Z,ZZZ,ZZZ.99-.                   
00972                                                                   
00973      EJECT                                                        
00974  01  ERROR-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.                 
00975      12  FILLER              PIC X(59).                           
00976      12  EPL-MESSAGE.                                             
00977          16  EPL-MESS        PIC X(4).                            
00978          16  FILLER          PIC X.                               
00979          16  EPL-STAT        PIC X.                               
00980          16  FILLER          PIC X.                               
00981          16  EPL-MSG.                                             
00982             20  EPL-COV      PIC X(6).                            
00983             20  FILLER       PIC X(59).                           
00984                                                                   
00985  01  PREMIUM-PRINT-LINE REDEFINES ISSUE-PRINT-LINE.               
00986      12  FILLER              PIC X(64).                           
00987      12  PPL-LF-DESC.                                             
00988          16  PPL-LF          PIC XX.                              
00989          16  PPL-DES-1       PIC X(6).                            
00990      12  PPL-LF-PREM         PIC ZZZ,ZZZ.ZZ-.                     
00991      12  PPL-AH-DESC.                                             
00992          16  FILLER          PIC X.                               
00993          16  PPL-AH          PIC XX.                              
00994          16  PPL-DES-2       PIC X(6).                            
00995      12  PPL-AH-PREM         PIC ZZZ,ZZZ.ZZ-.                     
00996      12  FILLER              PIC X(26).                           
00997                                                                   
00998  01  USER-SUM-LINE   REDEFINES ISSUE-PRINT-LINE.                  
00999      12  FILLER                  PIC X.                           
01000      12  USL-USER                PIC X(4).                        
01001      12  FILLER                  PIC X.                           
01002      12  USL-BAT-INP             PIC ZZZZ9.                       
01003      12  FILLER                  PIC XX.                          
01004      12  USL-BAT-MNTC            PIC ZZZZ9.                       
01005      12  FILLER                  PIC X(7).                        
01006      12  USL-DET-INP             PIC ZZZZ9.                       
01007      12  FILLER                  PIC XX.                          
01008      12  USL-DET-MNTC            PIC ZZZZ9.                       
01009      12  FILLER                  PIC X(6).                        
01010      12  USL-UPFRC-PROC          PIC ZZZZ9.                       
102706     12  FILLER                  PIC X(6).                        
102706     12  USL-SIG-SW              PIC ZZZZ9.                       
01011                                                                   
01012      EJECT                                                        
01013  01  SUMMARY-LINE-1.                                              
01014      12  FILLER              PIC X(100) VALUE                     
01015              '*** SUMMARY TOTALS FOR FILE ***'.                   
01016                                                                   
01017  01  SUMMARY-LINE-2.                                              
01018      12  FILLER              PIC X(23) VALUE                      
01019              ' EARLIEST ENTRY DATE -'.                            
01020      12  SL2-EARLY           PIC X(8) VALUE 'XX-XX-XX'.           
01021                                                                   
01022  01  SUMMARY-LINE-3.                                              
01023      12  FILLER              PIC X(23) VALUE                      
01024              ' LATEST ENTRY DATE   -'.                            
01025      12  SL3-LATE            PIC X(8) VALUE 'XX-XX-XX'.           
01026                                                                   
01027  01  SUMMARY-LINE-4.                                              
01028      12  FILLER              PIC X(24) VALUE                      
01029              ' ISS/CAN OVER 30 DAYS -'.                           
01030      12  SL4-30              PIC Z(3)Z9.                          
01031                                                                   
01032  01  SUMMARY-LINE-5.                                              
01033      12  FILLER              PIC X(24) VALUE                      
01034              ' ISS/CAN OVER 60 DAYS -'.                           
01035      12  SL5-60              PIC Z(3)Z9.                          
01036                                                                   
01037  01  SUMMARY-LINE-6.                                              
01038      12  FILLER              PIC X(24) VALUE                      
01039              ' ISS/CAN OVER 90 DAYS -'.                           
01040      12  SL6-90              PIC Z(3)Z9.                          
01041                                                                   
01042  01  SUMMARY-LINE-7.                                              
01043      12  FILLER              PIC X(24) VALUE                      
01044              ' ISSUES PROCESSED     -'.                           
01045      12  SL7-ISSUES          PIC Z(3)Z9.                          
01046                                                                   
01047  01  SUMMARY-LINE-8.                                              
01048      12  FILLER              PIC X(24) VALUE                      
01049              ' CANCELS PROCESSED    -'.                           
01050      12  SL8-CANCELS         PIC Z(3)Z9.                          
01051                                                                   
01052  01  SUMMARY-LINE-9.                                              
01053      12  FILLER              PIC X(24) VALUE                      
01054              ' TOTAL ISSUES/CANCELS - '.                          
01055      12  SL9-BOTH            PIC Z(3)Z9.                          
01056                                                                   
01057  01  SUMMARY-LINE-10.                                             
01058      12  FILLER              PIC X(24) VALUE                      
01059              ' TOTAL BATCHES        -'.                           
01060      12  SL10-BATCHES        PIC Z(3)Z9.                          
01061                                                                   
01062  01  SUMMARY-LINE-11.                                             
01063      12  FILLER              PIC X(24) VALUE                      
01064              ' BATCHES OUT OF BALANCE-'.                          
01065      12  SL11-OUTOFBAL       PIC Z(3)Z9.                          
01066                                                                   
01067  01  SUMMARY-LINE-12.                                             
01068      12  FILLER              PIC X(24) VALUE                      
01069              ' HIGHEST BATCH NUMBER -'.                           
01070      12  SL12-HIGHBATCH      PIC X(6).                            
01071                                                                   
01072  01  SUMMARY-HDG.                                                 
01073      12  FILLER              PIC X(27) VALUE SPACES.              
01074      12  SH-LF1              PIC XXX   VALUE 'LF'.                
01075      12  FILLER              PIC X(14) VALUE ' PREMIUM'.          
01076      12  SH-LF2              PIC XXX   VALUE 'LF'.                
01077      12  FILLER              PIC X(13) VALUE ' REFUNDS'.          
01078      12  SH-AH1              PIC XXX   VALUE 'AH'.                
01079      12  FILLER              PIC X(13) VALUE ' PREMIUM'.          
01080      12  SH-AH2              PIC XXX   VALUE 'AH'.                
01081      12  FILLER              PIC X(12) VALUE ' REFUNDS'.          
01082      12  FILLER              PIC X(30) VALUE                      
01083              ' CLAIM PAYMENTS  LOSS RESERVES'.                    
01084                                                                   
01085  01  SUMMARY-HDG-2.                                               
01086      12  FILLER              PIC X(15) VALUE SPACES.              
01087      12  FILLER              PIC X(56) VALUE                      
01088         'TOTAL COUNT   FATAL    FORCIBLE    ON HOLD    RETURNED'. 
01089      12  FILLER              PIC X(39) VALUE                      
01090              'AVAILABLE FOR PROCESSING'.                          
01091                                                                   
01092  01  SUMMARY-HDG-3.                                               
01093      12  FILLER              PIC X(15) VALUE SPACES.              
01094      12  FILLER              PIC X(56) VALUE                      
01095         'TOTAL COUNT   FATAL    FORCIBLE'.                        
01096      12  FILLER              PIC X(39) VALUE                      
01097              'AVAILABLE FOR PROCESSING'.                          
01098                                                                   
01099  01  SUMMARY-PRINT-LINE.                                          
01100      12  FILLER              PIC X.                               
01101      12  SMPL-DESC           PIC X(22).                           
01102      12  SMPL-LF-PREM        PIC ZZZ,ZZZ,ZZZ.99-.                 
01103      12  FILLER              PIC X.                               
01104      12  SMPL-LF-REF         PIC ZZZ,ZZZ,ZZZ.99-.                 
01105      12  FILLER              PIC X.                               
01106      12  SMPL-AH-PREM        PIC ZZZ,ZZZ,ZZZ.99-.                 
01107      12  FILLER              PIC X.                               
01108      12  SMPL-AH-REF         PIC ZZZ,ZZZ,ZZZ.99-.                 
01109      12  FILLER              PIC XXX.                             
01110      12  SMPL-CLM            PIC ZZZ,ZZZ,ZZZ.99-.                 
01111      12  FILLER              PIC X.                               
01112      12  SMPL-LRES           PIC ZZZ,ZZZ,ZZZ.99-.                 
01113                                                                   
01114  01  SUMMARY-PRINT-LINE-2   REDEFINES SUMMARY-PRINT-LINE.         
01115      12  FILLER              PIC X.                               
01116      12  SMPL2-DESC          PIC X(17).                           
01117      12  SMPL2-TOTAL         PIC Z(4)9-.                          
01118      12  FILLER              PIC X(4).                            
01119      12  SMPL2-FATAL         PIC Z(4)9-.                          
01120      12  FILLER              PIC X(6).                            
01121      12  SMPL2-FORCIBLE      PIC Z(4)9-.                          
01122      12  FILLER              PIC X(6).                            
01123      12  SMPL2-HOLD          PIC Z(4)9-.                          
01124      12  FILLER              PIC X(6).                            
01125      12  SMPL2-RETURNED      PIC Z(4)9-.                          
01126      12  FILLER              PIC X(12).                           
01127      12  SMPL2-AVAILABLE     PIC Z(4)9-.                          
01128  EJECT                                                            
01129      COPY ELCDATE.                                                
01130  EJECT                                                            
01131      COPY ELCEMIB.                                                
01132  EJECT                                                            
01133      COPY ELCDTECX.                                               
01134  EJECT                                                            
01135      COPY ELCDTEVR.                                               
01136  EJECT                                                            
01137      COPY ELCACCTV.                                               
01138  EJECT                                                            
01139      COPY ELCERRWS.                                               
01140  EJECT                                                            
01141  PROCEDURE DIVISION.                                              
01142                                                                   
01143  0000-GET-DATE.                                                   
01144      COPY ELCDTERX.                                               
01145                                                                   
01146      MOVE DTE-CLIENT             TO  E-CLIENT-ID.                 
01147                                                                   
           move '11/01/12'             to ws-current-date
01148 ** SET UP HEADINGS                                                
01149      MOVE LIFE-OVERRIDE-L2 TO IC2-LF CL2-LF IC2-LF2 IC2-LF3       
01150                               IC2-LF-1 IC2-LF2-1 IC2-LF3-1        
01151                               SH-LF1 SH-LF2 IH2-LF CH2-LF CH2-LF2.
01152                                                                   
01153      MOVE AH-OVERRIDE-L2 TO IC2-AH CL2-AH IC2-AH2 IC2-AH3         
01154                             IC2-AH-1 IC2-AH2-1 IC2-AH3-1          
01155                             SH-AH1 SH-AH2 IH2-AH CH3-AH CH2-AH.   
01156                                                                   
01157      IF DTE-CLAIM-PAID-THRU-TO = '1'                              
01158         MOVE 'PD  TO '    TO HD3-HDG.                             
01159                                                                   
01160  0200-OPEN-EM.                                                    
01161      OPEN INPUT ERPNDB                                            
01162                   ERPNDC                                          
01163                     ERCRTC                                        
01164                       ERACCT                                      
01165                         ELCNTL                                    
01166                           ELERRS                                  
091604                            ERNOTE
01167          OUTPUT PRINT-FILE.                                       
01168                                                                   
01169      IF PB-STATUS-CODE  = '00' OR '97'                            
01170          NEXT SENTENCE                                            
01171        ELSE                                                       
01172          MOVE PB-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01173          MOVE 'ERROR OCCURED OPEN - ERPNDB'                       
01174                                  TO  WS-ABEND-MESSAGE             
01175          GO TO ABEND-PGM.                                         
01176                                                                   
01177      IF PC-STATUS-CODE  = '00' OR '97'                            
01178          NEXT SENTENCE                                            
01179        ELSE                                                       
01180          MOVE PC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01181          MOVE 'ERROR OCCURED OPEN - ERPNDC'                       
01182                                  TO  WS-ABEND-MESSAGE             
01183          GO TO ABEND-PGM.                                         
01184                                                                   
01185      IF CC-STATUS-CODE  = '00' OR '97'                            
01186          NEXT SENTENCE                                            
01187        ELSE                                                       
01188          MOVE CC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01189          MOVE 'ERROR OCCURED OPEN - ERCRTC'                       
01190                                  TO  WS-ABEND-MESSAGE             
01191          GO TO ABEND-PGM.                                         
01192                                                                   
01193      IF AM-STATUS-CODE  = '00' OR '97'                            
01194          NEXT SENTENCE                                            
01195        ELSE                                                       
01196          MOVE AM-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01197          MOVE 'ERROR OCCURED OPEN - ERACCT'                       
01198                                  TO  WS-ABEND-MESSAGE             
01199          GO TO ABEND-PGM.                                         
01200                                                                   
01201      IF CN-STATUS-CODE  = '00' OR '97'                            
01202          NEXT SENTENCE                                            
01203        ELSE                                                       
01204          MOVE CN-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01205          MOVE 'ERROR OCCURED OPEN - ELCNTL'                       
01206                                  TO  WS-ABEND-MESSAGE             
01207          GO TO ABEND-PGM.                                         
01208                                                                   
01209      IF EM-STATUS-CODE  = '00' OR '97'                            
01210          NEXT SENTENCE                                            
01211        ELSE                                                       
01212          MOVE EM-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01213          MOVE 'ERROR OCCURED OPEN - ELERRS'                       
01214                                  TO  WS-ABEND-MESSAGE             
01215          GO TO ABEND-PGM.                                         
091604     IF ERNOTE-FILE-STATUS = '00' OR '97'
091604        CONTINUE
091604     ELSE
091604        MOVE ERNOTE-FILE-STATUS  TO WS-ABEND-FILE-STATUS         
091604        MOVE 'ERROR OCCURED OPEN - ERNOTE'                       
091604                                 TO WS-ABEND-MESSAGE             
091604        PERFORM ABEND-PGM
091604     END-IF

           .
01216       EJECT                                                       
01217  0300-MAIN-LINE.                                                  
01218      MOVE LOW-VALUES             TO  CF-CONTROL-PRIMARY.          
01219      MOVE DTE-CLIENT             TO  CF-COMPANY-ID.               
01220      MOVE '1'                    TO  CF-RECORD-TYPE.              
01221      MOVE SPACES                 TO  CF-ACCESS-CD-GENL.           
01222      MOVE +0                     TO  CF-SEQUENCE-NO.              
01223                                                                   
01224      READ ELCNTL.                                                 
01225                                                                   
01226      IF CN-STATUS-CODE NOT = ZERO                                 
01227          MOVE 'ERROR OCCURED READ INITIAL - ELCNTL'               
01228                                  TO  WS-ABEND-MESSAGE             
01229          MOVE CN-STATUS-CODE TO WS-ABEND-FILE-STATUS              
01230          GO TO ABEND-PGM.                                         
01231                                                                   
01232      IF DTE-PGM-OPT = '1'                                         
01233         MOVE '(* ALL ERRORS *)'      TO  ER-ONLY                  
01234         MOVE 'EL515B '               TO  CL-515.                  
01235                                                                   
01236      IF DTE-PGM-OPT = '2'                                         
01237         MOVE '(* MONTH-END - FULL DETAIL *)' TO  ER-ONLY          
01238         MOVE 'EL515C '               TO  CL-515.                  
01239                                                                   
01240      IF DTE-PGM-OPT = '3'                                         
01241         MOVE '(* MONTH-END - ALL ERRORS *)' TO  ER-ONLY           
01242         MOVE 'EL515D '               TO  CL-515.                  
01243                                                                   
01244      IF DTE-PGM-OPT = '4'                                         
01245         MOVE '(* FATAL AND UNFORCED ERRORS ONLY *)' TO  ER-ONLY   
01246         MOVE 'EL515E '               TO  CL-515.                  
01247                                                                   
01248      IF DTE-PGM-OPT = '5'                                         
01249         MOVE '(* MONTH-END - FATAL AND UNFORCED *)' TO  ER-ONLY   
01250         MOVE 'EL515F '               TO  CL-515.                  
01251                                                                   
01252      IF DTE-PGM-OPT = '6'                                         
01253         MOVE '(* FORCED ERRORS ONLY *)' TO  ER-ONLY               
01254         MOVE 'EL515G '               TO  CL-515.                  
01255                                                                   
01256      MOVE COMPANY-NAME           TO H2-COMP.                      
01257      MOVE WS-CURRENT-DATE        TO H2-DATE.                      
01258      MOVE ALPH-DATE              TO H3-DATE.                      
01259                                                                   
01260      MOVE RUN-MO                 TO RNMO.                         
01261      MOVE RUN-DA                 TO RNDA.                         
01262      MOVE RUN-YR                 TO RNYR.                         
01263      MOVE RUN-CC                 TO RNCC.                         
01264                                                                   
01265      MOVE SPACES TO USER-SUMMARY-TOTALS.                          
01266                                                                   
01267      MOVE ZEROS                  TO UST-BAT-INP    (101)          
01268                                     UST-DET-INP    (101)          
01269                                     UST-BAT-MNTC   (101)          
01270                                     UST-DET-MNTC   (101)          
01271                                     UST-UPFRC-PROC (101)
102706                                    UST-SIG-SW     (101)
           .
01273  0500-SORT-ISSUES.                                                
01274         SORT SORT-WORK   ASCENDING KEY  SORT-P-CARRIER            
01275                                         SORT-P-GROUPING           
01276                                         SORT-P-STATE              
01277                                         SORT-P-ACCOUNT            
01278                                         SORT-P-RECORD-TYPE        
01279                                         SORT-P-CERT-EFF-DT        
01280                                         SORT-P-CERT-NO            
01281                                         SORT-P-CHG-SEQ-NO         
01282                                         SORT-P-ENTRY-BATCH        
01283                                                                   
01284                INPUT PROCEDURE  0510-READ-ISSUES                  
01285                OUTPUT PROCEDURE 0600-READ-TRANS.                  
01286                                                                   
01287      CLOSE ERPNDB                                                 
01288              ERPNDC                                               
01289                ERCRTC                                             
01290                  ERACCT                                           
01291                    ELERRS                                         
01292                      ELCNTL                                       
091604                      ERNOTE
01293                        PRINT-FILE.                                
01294                                                                   
01295      IF PB-STATUS-CODE NOT = ZERO                                 
01296          MOVE PB-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01297          MOVE 'ERROR OCCURED CLOSE - ERPNDB'                      
01298                                  TO  WS-ABEND-MESSAGE             
01299          GO TO ABEND-PGM.                                         
01300                                                                   
01301      IF PC-STATUS-CODE NOT = ZERO                                 
01302          MOVE PC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01303          MOVE 'ERROR OCCURED CLOSE - ERPNDC'                      
01304                                  TO  WS-ABEND-MESSAGE             
01305          GO TO ABEND-PGM.                                         
01306                                                                   
01307      IF CC-STATUS-CODE NOT = ZERO                                 
01308          MOVE CC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01309          MOVE 'ERROR OCCURED CLOSE - ERCRTC'                      
01310                                  TO  WS-ABEND-MESSAGE             
01311          GO TO ABEND-PGM.                                         
01312                                                                   
01313      IF AM-STATUS-CODE NOT = ZERO                                 
01314          MOVE AM-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01315          MOVE 'ERROR OCCURED CLOSE - ERACCT'                      
01316                                  TO  WS-ABEND-MESSAGE             
01317          GO TO ABEND-PGM.                                         
01318                                                                   
01319      IF CN-STATUS-CODE NOT = ZERO                                 
01320          MOVE CN-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01321          MOVE 'ERROR OCCURED CLOSE - ELCNTL'                      
01322                                  TO  WS-ABEND-MESSAGE             
01323          GO TO ABEND-PGM.                                         
01324                                                                   
01325      IF EM-STATUS-CODE NOT = ZERO                                 
01326          MOVE EM-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01327          MOVE 'ERROR OCCURED CLOSE - ELERRS'                      
01328                                  TO  WS-ABEND-MESSAGE             
01329          GO TO ABEND-PGM.                                         
01330                                                                   
091604     IF ERNOTE-FILE-STATUS NOT = '00'                             
091604        MOVE ERNOTE-FILE-STATUS  TO WS-ABEND-FILE-STATUS         
091604        MOVE 'ERROR OCCURED CLOSE - ERNOTE'                      
091604                                 TO  WS-ABEND-MESSAGE             
091604        PERFORM ABEND-PGM
091604     END-IF
01330                                                                   
01331               COPY ELCPRTCX.                                      
01332                                                                   
01333      GOBACK.                                                      
01334    EJECT                                                          
01335  0510-READ-ISSUES         SECTION.                                
01336      MOVE LOW-VALUES            TO PB-CONTROL-PRIMARY.            
01337      MOVE DTE-CLASIC-COMPANY-CD TO PB-COMPANY-CD.                 
01338                                                                   
01339      START ERPNDB   KEY NOT LESS THAN PB-CONTROL-PRIMARY.         
01340                                                                   
01341      IF PB-STATUS-CODE = '23'                                     
01342          GO TO 0530-E-INPUT-SECT.                                 
01343                                                                   
01344      IF PB-STATUS-CODE NOT = ZERO                                 
01345          MOVE PB-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01346          MOVE 'ERROR OCCURED START - ERPNDB'                      
01347                                  TO  WS-ABEND-MESSAGE             
01348          GO TO ABEND-PGM.                                         
01349      EJECT                                                        
01350  0520-READ-ISSUES.                                                
01351      READ ERPNDB  NEXT RECORD.                                    
01352                                                                   
01353      IF PB-STATUS-1 = '1'                                         
01354          GO TO 0530-E-INPUT-SECT.                                 
01355                                                                   
01356      IF PB-STATUS-CODE NOT = ZERO                                 
01357          MOVE PB-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
01358          MOVE 'ERROR OCCURED READ - ERPNDB'                       
01359                                  TO  WS-ABEND-MESSAGE             
01360          GO TO ABEND-PGM.                                         
01361                                                                   
01362      IF PB-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
01363          GO TO 0530-E-INPUT-SECT.                                 
01364                                                                   
01365      MOVE PB-ENTRY-BATCH TO CLAIM-SAVE.                           
01366                                                                   
01367      IF CLAIM-SAVE  = '#CL'                                       
01368          GO TO 0520-READ-ISSUES.                                  
01369                                                                   
01370      IF PB-ALT-CHG-SEQ-NO  NOT =  ZEROS                           
01371          GO TO 0520-READ-ISSUES.                                  
01372                                                                   
01373      IF PB-CREDIT-ACCEPT-DT = LOW-VALUES OR BIN-RUN-DATE          
01374          NEXT SENTENCE                                            
01375      ELSE                                                         
01376          ADD 1 TO C-ACCEPT                                        
01377          GO TO 0520-READ-ISSUES.                                  
01378                                                                   
01379      MOVE PB-CREDIT-SELECT-DT  TO DC-BIN-DATE-1.                  
01380      MOVE SPACES               TO DC-OPTION-CODE.                 
01381      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
01382      MOVE DC-GREG-DATE-1-YMD   TO SELDT.                          
01383      MOVE DC-ALPHA-CEN-N       TO SLCC.                           
01384                                                                   
01385 ********* CREDIT-SELECT DATE                                      
01386      IF DTE-PGM-OPT = '2' OR '3' OR '5'                           
01387         IF SELDT GREATER RUNDT                                    
01388            GO TO 0520-READ-ISSUES.                                
01389                                                                   
01390      MOVE PB-ENTRY-BATCH TO HIGHEST-BATCH.                        
01391                                                                   
01392      IF PB-BATCH-TRAILER                                          
01393          MOVE CUR-CARR            TO PB-SV-CARRIER                
01394          MOVE CUR-GRP             TO PB-SV-GROUPING               
01395          MOVE CUR-ST              TO PB-SV-STATE                  
01396          MOVE ' '                 TO PB-RECORD-TYPE               
01397          ADD 1 TO BATCH-CNT                                       
01398          PERFORM 2800-ADD-USER-BATCH THRU 2899-EXIT               
01399          IF PB-B-LF-ISS-PRM-ENTERED                               
01400                      NOT =  PB-B-LF-ISS-PRM-REMITTED              
01401            OR PB-B-LF-CAN-PRM-ENTERED                             
01402                      NOT =  PB-B-LF-CAN-PRM-REMITTED              
01403            OR PB-B-AH-ISS-PRM-ENTERED                             
01404                      NOT =  PB-B-AH-ISS-PRM-REMITTED              
01405            OR PB-B-LF-ISS-PRM-ENTERED                             
01406                      NOT =  PB-B-LF-ISS-PRM-REMITTED              
01407              ADD 1               TO  BATCHES-OUT-OF-BALANCE       
01408              GO TO 0525-RELEASE                                   
01409          ELSE                                                     
01410              GO TO 0525-RELEASE.                                  
01411                                                                   
01412      MOVE PB-SV-CARRIER             TO CUR-CARR.                  
01413      MOVE PB-SV-GROUPING            TO CUR-GRP.                   
01414      MOVE PB-SV-STATE               TO CUR-ST.                    
01415                                                                   
01416      IF PB-ISSUE                                                  
01417          PERFORM 1000-ADD-ISSUES THRU 1000-ADD-ISSUES-EXIT.       
01418                                                                   
01419      IF PB-CANCELLATION                                           
01420          PERFORM 2000-ADD-CANCELS THRU 2000-ADD-CANCELS-EXIT.     
01421                                                                   
01422      IF PB-RECORD-ON-HOLD                                         
01423          GO TO 0525-RELEASE.                                      
01424                                                                   
01425 ********* (ERRORS ONLY)                                           
01426      IF DTE-PGM-OPT = '1' OR '3' OR '4' OR '5' OR '6'             
01427          GO TO 0521-ERRORS.                                       
01428                                                                   
01429      GO TO 0525-RELEASE.                                          
01430                                                                   
01431  0521-ERRORS.                                                     
01432      IF DTE-PGM-OPT = '4' OR '5'                                  
01433          IF PB-UNFORCED-ERRORS OR PB-FATAL-ERRORS                 
01434              GO TO 0525-RELEASE                                   
01435          ELSE                                                     
01436              GO TO 0520-READ-ISSUES                               
01437      ELSE                                                         
01438          IF DTE-PGM-OPT = '6'                                     
01439              IF PB-FORCE-ERRORS                                   
01440                  GO TO 0525-RELEASE                               
01441              ELSE                                                 
01442                  GO TO 0520-READ-ISSUES                           
01443          ELSE                                                     
01444              IF PB-NO-OF-ERRORS NOT = ZERO                        
01445                  GO TO 0525-RELEASE                               
01446              ELSE                                                 
01447                  GO TO 0520-READ-ISSUES.                          
01448                                                                   
01449  0525-RELEASE.                                                    
CIDMOD
CIDMOD     IF DTE-FMT-OPT = '2'
CIDMOD        MOVE 1        TO PND-RECS
CIDMOD        GO TO 0520-READ-ISSUES
CIDMOD     END-IF
CIDMOD
01450      MOVE PENDING-BUSINESS TO SORT-RECORD
01451      RELEASE SORT-RECORD.                                         
01452      ADD 1 TO PND-RECS.                                           
01453      GO TO 0520-READ-ISSUES.                                      
01454                                                                   
01455  0530-E-INPUT-SECT.                                               
01456      EXIT.                                                        
01457      EJECT                                                        
01458  0600-READ-TRANS      SECTION.                                    
01459      PERFORM 0800-PRINT-PB THRU 0800-EXIT.                        
01460                                                                   
CIDMOD     IF DTE-FMT-OPT NOT = '2'
CIDMOD        PERFORM 3000-PRINT-CLAIM THRU 3000-PRINT-CLAIM-EXIT       
CIDMOD     END-IF
01462                                                                   
CIDMOD     IF (DTE-PGM-OPT NOT = '1' AND '3' AND '4' AND '5' AND '6')   
CIDMOD                          AND
CIDMOD        (DTE-FMT-OPT NOT = '2')
CIDMOD        PERFORM 4000-PRINT-CHANGE                                 
CIDMOD                                 THRU 4000-PRINT-CHANGE-EXIT      
CIDMOD     END-IF
01465                                                                   
01466      PERFORM 4900-PRINT-USER-TOTS THRU 4900-EXIT.                 
01467                                                                   
CIDMOD     IF DTE-FMT-OPT NOT = '2'
CIDMOD        PERFORM 5000-PRINT-SUMMARY                                
CIDMOD                                 THRU 5000-EXIT                   
CIDMOD     END-IF
01469      .                                                            
01470  0700-CLOSE-EM.                                                   
01471      EXIT.                                                        
01472      EJECT                                                        
01473  0800-PRINT-PB   SECTION.                                         
01474      IF ((PB-STATUS-CODE = '23') OR                               
01475         (PND-RECS  = 0))                                          
01476          PERFORM 7000-PRT-HDG THRU 7000-EXIT                      
01477          MOVE '**** NO PENDING BUSINESS AT THIS TIME ****'        
01478                                  TO P-DATA                        
01479          MOVE '-' TO X                                            
01480          PERFORM 7000-PRINT-LINE                                  
01481          DISPLAY '****INVALID ACCEPT DATES ' C-ACCEPT             
01482          GO TO 0800-EXIT                                          
CIDMOD     ELSE
CIDMOD        IF DTE-FMT-OPT = '2'
CIDMOD           GO TO 0800-EXIT
CIDMOD        END-IF
CIDMOD     END-IF
01483                                                                   
01484      MOVE SPACE TO ISSUE-CANCEL.                                  
01485                                                                   
01486  0800-READ-PB.                                                    
01487      RETURN SORT-WORK                                             
01488           AT END GO TO 0800-EOF.                                  
01489                                                                   
01490      MOVE SORT-RECORD            TO PENDING-BUSINESS
01491                                                                   
01492      IF PB-RECORD-TYPE NOT = ' '                                  
01493          GO TO 0800-CK-IS-CN.                                     
01494                                                                   
01495 ********* BATCH HEADER RECORD                                     
01496      IF ISSUES                                                    
01497          PERFORM 1000-PRT-TOTS THRU 1000-PRT-TOTS-EXIT.           
01498                                                                   
01499      IF CANCELS                                                   
01500          PERFORM 2000-PRT-TOTS THRU 2000-PRT-TOTS-EXIT.           
01501                                                                   
01502      IF PB-CARRIER  NOT = CUR-CARR OR                             
01503         PB-GROUPING NOT = CUR-GRP  OR                             
01504         PB-STATE    NOT = CUR-ST   OR                             
01505         PB-ACCOUNT  NOT = CUR-ACCT                                
01506           PERFORM 1500-NEW-ACCT THRU 1500-NEW-ACCT-EXIT.          
01507                                                                   
01508      MOVE PB-BATCH-RECORD     TO BATCH-TRAIL   (X-SEQ).           
01509      MOVE PB-ENTRY-BATCH      TO SAVE-BATCH-NO (X-SEQ).           
01510      MOVE PB-CREDIT-SELECT-DT TO SAVE-BATCH-DT (X-SEQ).           
01511      MOVE X-SEQ               TO SBR-SEQ       (X-SEQ).           
01512      ADD 1 TO X-SEQ.                                              
01513      IF X-SEQ GREATER THAN X-MAX                                  
01514          MOVE 'ACCT TABLE SIZE EXCEEDED - 1001'                   
01515                                TO WS-ABEND-MESSAGE                
01516          GO TO ABEND-PGM.                                         
01517                                                                   
01518      GO TO 0800-READ-PB.                                          
01519       EJECT                                                       
01520  0800-CK-IS-CN.                                                   
01521      IF PB-CANCELLATION                                           
01522         PERFORM 2000-FORMAT-CANCEL                                
01523        ELSE                                                       
01524         PERFORM 1000-FORMAT-ISSUE.                                
01525                                                                   
01526      GO TO 0800-READ-PB.                                          
01527                                                                   
01528  0800-EOF.                                                        
01529      IF ISSUES                                                    
01530          PERFORM 1000-PRT-TOTS THRU 1000-PRT-TOTS-EXIT.           
01531                                                                   
01532      IF CANCELS                                                   
01533          PERFORM 2000-PRT-TOTS THRU 2000-PRT-TOTS-EXIT.           
01534                                                                   
01535  0800-EXIT.                                                       
01536      EXIT.                                                        
01537     EJECT                                                         
01538  1000-FORMAT-ISSUE SECTION.                                       
01539      ADD 4 TO LINE-CT.                                            
01540      IF LINE-CT GREATER MAX-LINES                                 
01541          PERFORM 7000-PRT-HDG THRU 7000-EXIT                      
01542          MOVE HDG3A          TO P-DATA                            
01543          MOVE ' '            TO X                                 
01544          PERFORM 7000-PRINT-LINE                                  
01545          MOVE SPACES         TO H4-TOTAL  H4-CONTD                
01546          MOVE HDG4           TO P-DATA                            
01547          MOVE ' '            TO X                                 
01548          PERFORM 7000-PRINT-LINE                                  
01549          PERFORM 7100-PRT-IS-HDG THRU 7100-EXIT.                  
01550                                                                   
01551      MOVE SPACES TO ISSUE-PRINT-LINE.                             
01552                                                                   
01553      MOVE PB-CERT-NO           TO ISPL-CERT.                      
01554                                                                   
01555      MOVE PB-CERT-EFF-DT      TO DC-BIN-DATE-1.                   
01556      MOVE SPACES              TO DC-OPTION-CODE.                  
01557      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
01558      MOVE DC-GREG-DATE-1-EDIT TO ISPL-EFF-DT.                     
01559                                                                   
01560      MOVE PB-I-LF-TERM           TO ISPL-LFTRM.                   
01561      MOVE PB-I-AH-TERM           TO ISPL-AHTRM.                   
01562                                                                   
01563      MOVE PB-I-INSURED-1ST-INIT  TO ISPL-INIT.                    
01564      MOVE PB-I-INSURED-LAST-NAME TO ISPL-NAME.                    
01565      MOVE PB-I-INSURED-SEX       TO ISPL-SEX.                     
01566      MOVE PB-I-LF-INPUT-CD       TO ISPL-LCOD.                    
01567      MOVE PB-I-LF-BENEFIT-AMT    TO ISPL-LBEN.                    
01568                                                                   
01569      IF PB-OVERRIDE-LIFE    OR                                    
01570         PB-OVERRIDE-BOTH                                          
01571            MOVE PB-I-LF-PREM-CALC TO PB-I-LF-PREMIUM-AMT.         
01572                                                                   
01573      MOVE PB-I-LF-PREMIUM-AMT    TO ISPL-LPRM.                    
01574                                                                   
01575      IF PB-I-AH-INPUT-CD NOT = ZEROS                              
01576          MOVE PB-I-AH-INPUT-CD   TO ISPL-ACOD.                    
01577                                                                   
01578      MOVE PB-I-AH-BENEFIT-AMT    TO ISPL-ABEN.                    
01579                                                                   
01580      IF PB-OVERRIDE-AH      OR                                    
01581         PB-OVERRIDE-BOTH                                          
01582            MOVE PB-I-AH-PREM-CALC TO PB-I-AH-PREMIUM-AMT.         
01583                                                                   
01584      MOVE PB-I-AH-PREMIUM-AMT    TO ISPL-APRM.                    
01585      MOVE PB-I-INDV-GRP-CD       TO ISPL-IG.                      
01586      MOVE PB-I-LOAN-APR          TO ISPL-APR.                     
01587      MOVE PB-I-SPECIAL-REIN-CODE TO ISPL-REI.                     
01588      MOVE PB-FORCE-CODE          TO ISPL-FC.                      
01589      MOVE PB-BATCH-ENTRY         TO ISPL-EC.                      
01590                                                                   
01591      IF PB-INPUT-DT NOT = LOW-VALUES AND SPACES                   
01592          MOVE PB-INPUT-DT         TO DC-BIN-DATE-1                
01593          MOVE SPACES              TO DC-OPTION-CODE               
01594          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
01595          MOVE DC-GREG-DATE-1-MDY  TO ISPL-INPUT.                  
01596                                                                   
01597      MOVE PB-ENTRY-BATCH          TO ISPL-BATCH.                  
01598                                                                   
01599      IF PB-BATCH-CHG-SEQ-NO  NOT = ZERO                           
01600         MOVE SPACES             TO ISPL-CERT                      
01601                                    ISPL-EFF-DT                    
01602         MOVE 'CHANGE REC'       TO ISPL-NM.                       
01603                                                                   
01604      MOVE ISSUE-PRINT-LINE TO P-DATA.                             
01605      MOVE '0' TO X.                                               
01606                                                                   
01607      PERFORM 7000-PRINT-LINE.                                     
01608  EJECT                                                            
01609  1000-FORMAT-2ND-LINE.                                            
01610      MOVE SPACES TO ISSUE-PRINT-LINE.                             
01611                                                                   
01612      MOVE PB-I-SOC-SEC-NO        TO ISPL-SOC-SEC.                 
01613                                                                   
01614      IF ISPL-SOC-SEC-2  = PB-SV-STATE                             
01615          MOVE SPACES             TO ISPL-SOC-SEC.                 
01616                                                                   
01617      IF PB-I-LF-CRIT-PER NUMERIC                                  
01618          MOVE PB-I-LF-CRIT-PER   TO ISPL-LFCP.                    
01619                                                                   
01620      IF PB-I-AH-CRIT-PER NUMERIC                                  
01621          MOVE PB-I-AH-CRIT-PER   TO ISPL-AHCP.                    
01622                                                                   
01623      MOVE PB-I-AGE               TO ISPL-AGE.                     
01624                                                                   
01625      IF PB-I-LIFE-BENEFIT-CD NOT = ZEROS                          
01626          MOVE PB-I-LIFE-BENEFIT-CD TO ISPL-LTYP.                  
01627                                                                   
01628      IF PB-I-AH-BENEFIT-CD   NOT = ZEROS                          
01629          MOVE PB-I-AH-BENEFIT-CD   TO ISPL-ATYP.                  
01630                                                                   
01631      IF PB-I-LF-PREMIUM-AMT NOT = PB-I-LF-PREM-CALC               
01632          MOVE PB-I-LF-PREM-CALC   TO ISPL-LPRM1.                  
01633                                                                   
01634      IF PB-I-AH-PREMIUM-AMT NOT = PB-I-AH-PREM-CALC               
01635          MOVE PB-I-AH-PREM-CALC   TO ISPL-APRM1.                  
01636                                                                   
01637      IF PB-I-RATE-CLASS NOT = ZEROS                               
01638          MOVE PB-I-RATE-CLASS     TO ISPL-BC.                     
01639                                                                   
01640      MOVE PB-I-SIG-SW            TO ISPL-SIG.                     
01641                                                                   
01642      IF PB-FORCE-ERRORS                                           
01643          MOVE 'FORCE'            TO ISPL-STAT                     
01644          MOVE PB-LAST-MAINT-BY   TO ISPL-WHO.                     
01645      IF PB-WARNING-ERRORS                                         
01646          MOVE 'WARN'             TO ISPL-STAT.                    
01647      IF PB-UNFORCED-ERRORS                                        
01648          MOVE 'UNFORC'           TO ISPL-STAT.                    
01649      IF PB-FATAL-ERRORS                                           
01650          MOVE 'FATAL'            TO ISPL-STAT.                    
01651      IF PB-RECORD-ON-HOLD                                         
01652          MOVE 'HOLD'             TO ISPL-STAT.                    
01653      IF PB-RECORD-RETURNED                                        
01654          MOVE 'RETRND'           TO ISPL-STAT.                    
01655                                                                   
01656      IF PB-OVERRIDE-LIFE    OR                                    
01657         PB-OVERRIDE-AH      OR                                    
01658         PB-OVERRIDE-BOTH                                          
01659            MOVE 'OVRIDE'         TO ISPL-STAT.                    
01660                                                                   
01661      IF PB-BATCH-CHG-SEQ-NO  NOT = ZERO                           
01662          COMPUTE ISPL-LEVEL = +1000 - PB-BATCH-CHG-SEQ-NO         
01663          MOVE 'EL-'              TO ISPL-CL.                      
01664                                                                   
01665      MOVE SECOND-ISSUE-LINE TO P-DATA.                            
01666      MOVE ' '               TO X.                                 
01667      PERFORM 7000-PRINT-LINE.                                     
01668                                                                   
01669  1000-FORMAT-3RD-LINE.                                            
01670      MOVE SPACES TO ISSUE-PRINT-LINE.                             
01671                                                                   
01672      MOVE PB-I-MEMBER-NO         TO ISPL-MEMBER.                  
01673                                                                   
01674      IF ISPL-MEMBER-2  = PB-SV-STATE                              
01675          MOVE SPACES             TO ISPL-MEMBER.                  
01676      IF PB-I-SUMMARY                                              
01677          MOVE SPACES             TO ISPL-MEMBER.                  
01678                                                                   
01679      IF PB-I-PAY-FREQUENCY NUMERIC                                
01680          MOVE PB-I-PAY-FREQUENCY TO ISPL-FREQ.                    
01681                                                                   
01682      IF PB-I-LF-ABBR  NOT = ZEROS                                 
01683         MOVE PB-I-LF-ABBR        TO ISPL-LTYP1.                   
01684                                                                   
01685      IF PB-I-AH-ABBR  NOT = ZEROS                                 
01686         MOVE PB-I-AH-ABBR        TO ISPL-ATYP1.                   
01687                                                                   
01688      SUBTRACT PB-I-LF-PREMIUM-AMT  FROM PB-I-LF-PREM-CALC         
01689                        GIVING       ISPL-LPRM2.                   
01690      SUBTRACT PB-I-AH-PREMIUM-AMT  FROM PB-I-AH-PREM-CALC         
01691                        GIVING       ISPL-APRM2.                   
01692                                                                   
01693      IF PB-I-LIVES NUMERIC                                        
01694          MOVE PB-I-LIVES         TO ISPL-LIVES                    
01695        ELSE                                                       
01696          MOVE ZEROS              TO ISPL-LIVES.                   
01697                                                                   
01698      MOVE PB-I-SKIP-CODE         TO ISPL-SKP.                     
01699                                                                   
01700      IF PB-BILLED-DT NOT = ZEROS AND LOW-VALUES AND SPACES        
01701          MOVE PB-BILLED-DT        TO DC-BIN-DATE-1                
01702          MOVE SPACES              TO DC-OPTION-CODE               
01703          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
01704          MOVE DC-GREG-DATE-1-MDY  TO ISPL-BILL.                   
01705                                                                   
01706      MOVE THIRD-ISSUE-LINE TO P-DATA.                             
01707      MOVE ' '              TO X.                                  
01708      PERFORM 7000-PRINT-LINE.                                     
01709                                                                   
01710      IF (PB-I-LF-ALT-BENEFIT-AMT = ZERO)
100703        OR (PB-I-LF-BENEFIT-CD = '  ' OR '00')
01711         GO TO 1000-FORMAT-ERRORS
100703     END-IF
01712                                                                   
01713      MOVE SPACES       TO ISSUE-PRINT-LINE.                       
01714      MOVE '**'         TO ISPL-LCOD.                              
01715      MOVE 'ALT'        TO ISPL-ALT.                               
01716                                                                   
01717      MOVE PB-I-LF-ALT-BENEFIT-AMT    TO ISPL-LBEN.                
01718                                                                   
01719      IF PB-OVERRIDE-LIFE    OR                                    
01720         PB-OVERRIDE-BOTH                                          
01721            MOVE PB-I-LF-ALT-PREM-CALC TO PB-I-LF-ALT-PREMIUM-AMT. 
01722                                                                   
01723      MOVE PB-I-LF-ALT-PREMIUM-AMT    TO ISPL-LPRM.                
01724                                                                   
01725      MOVE ISSUE-PRINT-LINE TO P-DATA.                             
01726      MOVE '0' TO X.                                               
01727      PERFORM 7000-PRINT-LINE.                                     
01728                                                                   
01729      MOVE SPACES       TO ISSUE-PRINT-LINE.                       
01730                                                                   
01731      IF PB-I-LF-ALT-PREMIUM-AMT NOT = PB-I-LF-ALT-PREM-CALC       
01732          MOVE PB-I-LF-ALT-PREM-CALC    TO ISPL-LPRM1.             
01733                                                                   
01734      MOVE SECOND-ISSUE-LINE TO P-DATA.                            
01735      MOVE ' ' TO X.                                               
01736      PERFORM 7000-PRINT-LINE.                                     
01737                                                                   
01738      MOVE SPACES       TO ISSUE-PRINT-LINE.                       
01739                                                                   
01740      SUBTRACT PB-I-LF-ALT-PREMIUM-AMT  FROM PB-I-LF-ALT-PREM-CALC 
01741                                        GIVING ISPL-LPRM2.         
01742                                                                   
01743      MOVE THIRD-ISSUE-LINE TO P-DATA.                             
01744      MOVE ' ' TO X.                                               
01745                                                                   
01746      PERFORM 7000-PRINT-LINE.                                     
01747                                                                   
01748  1000-FORMAT-ERRORS.                                              
01749      PERFORM 4500-FORMAT-ERRORS THRU 4599-EXIT.                   
091604     PERFORM 4700-FORMAT-BILL-NOTES
091604                                 THRU 4700-EXIT
01750                                                                   
01751      MOVE '1' TO ISSUE-CANCEL.                                    
01752                                                                   
01753  1000-FORMAT-ISSUE-EXIT.                                          
01754      EXIT.                                                        
01755      EJECT                                                        
01756  1000-ADD-ISSUES SECTION.                                         
01757      IF PB-BATCH-CHG-SEQ-NO NOT = ZERO                            
01758          GO TO 1000-ADD-ISSUES-EXIT.                              
01759                                                                   
01760      PERFORM 2500-SUMMARIZE THRU 2500-EXIT.                       
01761                                                                   
01762      ADD 1 TO ISSUE-CNT.                                          
01763                                                                   
01764      PERFORM 2600-ADD-USER-TABLE THRU 2699-EXIT.                  
01765                                                                   
CIDMOD     IF DTE-FMT-OPT = '2'
CIDMOD        GO TO 1000-ADD-ISSUES-EXIT                                
CIDMOD     END-IF
CIDMOD
01766      IF  PB-FATAL-ERRORS    OR                                    
01767          PB-UNFORCED-ERRORS OR                                    
01768          PB-RECORD-ON-HOLD  OR                                    
01769          PB-RECORD-RETURNED                                       
01770            NEXT SENTENCE                                          
01771          ELSE                                                     
01772            GO TO 1000-PROCESSABLE.                                
01773                                                                   
01774      IF PB-RECORD-ON-HOLD                                         
01775          ADD 1 TO HOLD-ISSUE-CNT                                  
01776          GO TO 1000-NON-PROCESSABLE.                              
01777                                                                   
01778      IF PB-RECORD-RETURNED                                        
01779          ADD 1 TO RETURNED-ISSUE-CNT                              
01780          GO TO 1000-NON-PROCESSABLE.                              
01781                                                                   
01782      IF PB-FATAL-ERRORS                                           
01783          ADD 1 TO FATAL-ISSUE-CNT                                 
01784          GO TO 1000-NON-PROCESSABLE.                              
01785                                                                   
01786      IF PB-UNFORCED-ERRORS                                        
01787          ADD 1 TO UNFORCE-ISSUE-CNT.                              
01788                                                                   
01789      GO TO 1000-NON-PROCESSABLE.                                  
01790                                                                   
01791  1000-PROCESSABLE.                                                
01792      ADD 1 TO AVAILABLE-ISSUE-CNT.                                
01793                                                                   
01794      IF PB-REIN-ONLY-CERT                                         
01795        OR  PB-REISSUED-CERT                                       
122002       OR  PB-MONTHLY-CERT
01796        OR CLASIC-CREATED-CERT                                     
01797        OR  PB-POLICY-IS-DECLINED                                  
01798        OR  PB-POLICY-IS-VOIDED                                    
01799          GO TO 1000-ADD-ISSUES-EXIT.                              
01800                                                                   
01801      IF PB-OVERRIDE-LIFE                                          
01802        OR PB-OVERRIDE-BOTH                                        
01803          IF PB-I-LF-PREM-CALC = ZERO                              
01804              NEXT SENTENCE                                        
01805          ELSE                                                     
01806              ADD PB-I-LF-PREM-CALC                                
01807                                  TO  TPA-LPREM                    
01808      ELSE                                                         
01809          IF PB-I-LF-PREMIUM-AMT NOT = ZERO                        
01810              ADD PB-I-LF-PREMIUM-AMT                              
01811                                  TO  TPA-LPREM.                   
01812                                                                   
01813      IF PB-OVERRIDE-AH                                            
01814        OR PB-OVERRIDE-BOTH                                        
01815          IF PB-I-AH-PREM-CALC = ZERO                              
01816              NEXT SENTENCE                                        
01817          ELSE                                                     
01818              ADD PB-I-AH-PREM-CALC                                
01819                                  TO  TPA-APREM                    
01820      ELSE                                                         
01821          IF PB-I-AH-PREMIUM-AMT NOT = ZERO                        
01822              ADD PB-I-AH-PREMIUM-AMT                              
01823                                  TO  TPA-APREM.                   
01824                                                                   
01825      IF PB-OVERRIDE-LIFE                                          
01826         OR PB-OVERRIDE-BOTH                                        
01827         IF PB-I-LF-ALT-PREM-CALC = ZERO                          
01828            CONTINUE
01829         ELSE                                                     
01830            ADD PB-I-LF-ALT-PREM-CALC                            
01831                                  TO TPA-LPREM                    
100703        END-IF
01832      ELSE                                                         
01833         IF PB-I-LF-ALT-PREMIUM-AMT NOT = ZERO                    
100703           IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00'
01834               ADD PB-I-LF-ALT-PREMIUM-AMT
01835                                  TO TPA-LPREM
100703           END-IF
100703        END-IF
100703     END-IF
01836                                                                   
01837      GO TO 1000-ADD-ISSUES-EXIT.                                  
01838                                                                   
01839  1000-NON-PROCESSABLE.                                            
01840      IF PB-REIN-ONLY-CERT                                         
01841        OR PB-REISSUED-CERT                                        
122002       OR PB-MONTHLY-CERT
01842        OR CLASIC-CREATED-CERT                                     
01843        OR  PB-POLICY-IS-DECLINED                                  
01844        OR  PB-POLICY-IS-VOIDED                                    
01845          GO TO 1000-ADD-ISSUES-EXIT.                              
01846                                                                   
01847      IF PB-OVERRIDE-LIFE                                          
01848        OR PB-OVERRIDE-BOTH                                        
01849          IF PB-I-LF-PREM-CALC = ZERO                              
01850              NEXT SENTENCE                                        
01851          ELSE                                                     
01852              ADD PB-I-LF-PREM-CALC                                
01853                                  TO  TNA-LPREM                    
01854      ELSE                                                         
01855          IF PB-I-LF-PREMIUM-AMT NOT = ZERO                        
01856              ADD PB-I-LF-PREMIUM-AMT                              
01857                                  TO  TNA-LPREM.                   
01858                                                                   
01859      IF PB-OVERRIDE-AH                                            
01860        OR PB-OVERRIDE-BOTH                                        
01861          IF PB-I-AH-PREM-CALC = ZERO                              
01862              NEXT SENTENCE                                        
01863          ELSE                                                     
01864              ADD PB-I-AH-PREM-CALC                                
01865                                  TO  TNA-APREM                    
01866      ELSE                                                         
01867          IF PB-I-AH-PREMIUM-AMT NOT = ZERO                        
01868              ADD PB-I-AH-PREMIUM-AMT                              
01869                                  TO  TNA-APREM.                   
01870                                                                   
01871      IF PB-OVERRIDE-LIFE                                          
01872         OR PB-OVERRIDE-BOTH                                        
01873         IF PB-I-LF-ALT-PREM-CALC = ZERO                          
01874            CONTINUE
01875         ELSE                                                     
01876            ADD PB-I-LF-ALT-PREM-CALC                            
01877                                  TO TNA-LPREM                    
100703        END-IF
01878      ELSE                                                         
01879         IF PB-I-LF-ALT-PREMIUM-AMT NOT = ZERO                    
100703           IF PB-I-LF-BENEFIT-CD NOT = '  ' AND '00'
01880               ADD PB-I-LF-ALT-PREMIUM-AMT                          
01881                                  TO TNA-LPREM
100703           END-IF
100703        END-IF
100703     END-IF
01882      .                                                             
01883  1000-ADD-ISSUES-EXIT.                                            
01884      EXIT.                                                        
01885    EJECT                                                          
01886  1000-PRT-TOTS SECTION.                                           
01887      MOVE SPACES TO IC-TOTAL-PRINT-LINE.                          
01888                                                                   
01889      IF LINE-CT = 99                                              
01890          GO TO 1000-PRT-TOTS-EXIT.                                
01891                                                                   
01892      MOVE 1  TO X-SEQ T-SEQ.                                      
01893                                                                   
01894  1000-PRT-TOTS-LOOP.                                              
01895      IF SBR-SEQ (X-SEQ) = ZERO                                    
01896          IF T-SEQ LESS 3                                          
01897              GO TO 1000-END-TOTS.                                 
01898                                                                   
01899      IF SBR-SEQ (X-SEQ) NOT = ZERO                                
01900          IF ENT-ISSUE-CNT (X-SEQ) = 0   AND                       
01901             REM-ISSUE-CNT (X-SEQ) = 0                             
01902                ADD 1 TO X-SEQ                                     
01903                GO TO 1000-PRT-TOTS-LOOP.                          
01904                                                                   
01905      IF LINE-CT GREATER (MAX-LINES - 8)                           
01906          PERFORM 7000-PRT-HDG THRU 7000-EXIT                      
01907          MOVE HDG3A          TO P-DATA                            
01908          MOVE ' '            TO X                                 
01909          PERFORM 7000-PRINT-LINE                                  
01910          MOVE HDG4           TO P-DATA                            
01911          MOVE ' '            TO X                                 
01912          PERFORM 7000-PRINT-LINE                                  
01913          MOVE ISSUE-HD1      TO P-DATA                            
01914          MOVE '0'            TO X                                 
01915          PERFORM 7000-PRINT-LINE                                  
01916          MOVE 7              TO LINE-CT                           
01917          PERFORM 1000-PRT-BAT-TOT-HDR                             
01918             THRU 1000-BAT-TOT-HDR-EXIT                            
01919        ELSE                                                       
01920          IF T-SEQ = 1                                             
01921              PERFORM 1000-PRT-BAT-TOT-HDR                         
01922                 THRU 1000-BAT-TOT-HDR-EXIT.                       
01923                                                                   
01924      MOVE WS-CONTD TO H4-CONTD.                                   
01925                                                                   
01926      IF SBR-SEQ (X-SEQ) = 0                                       
01927        IF T-SEQ GREATER 2                                         
01928           MOVE TOT-ENT-LF-ISSUE    TO ICPL-LF-ENT                 
01929           MOVE TOT-ENT-AH-ISSUE    TO ICPL-AH-ENT                 
01930           MOVE TOT-ENT-ISSUE-CNT   TO ICPL-CNT-ENT                
01931           MOVE TOT-REM-LF-ISSUE    TO ICPL-LF-REM                 
01932           MOVE TOT-REM-AH-ISSUE    TO ICPL-AH-REM                 
01933           MOVE TOT-REM-ISSUE-CNT   TO ICPL-CNT-REM                
01934           MOVE TOT-LF-OUT          TO ICPL-LF-OUT                 
01935           MOVE TOT-AH-OUT          TO ICPL-AH-OUT                 
01936           MOVE TOT-CNT-OUT         TO ICPL-CNT-OUT                
01937           MOVE 'TOTALS'            TO ICPL-BATCH                  
01938           MOVE SPACES              TO ICPL-DATE                   
01939           MOVE IC-TOTAL-PRINT-LINE TO P-DATA                      
01940           MOVE '0'                 TO X                           
01941           PERFORM 7000-PRINT-LINE                                 
01942           ADD 2                    TO LINE-CT                     
01943           GO TO 1000-END-TOTS                                     
01944        ELSE                                                       
01945           GO TO 1000-END-TOTS.                                    
01946                                                                   
01947      MOVE SAVE-BATCH-NO (X-SEQ)  TO ICPL-BATCH.                   
01948                                                                   
01949      MOVE SAVE-BATCH-DT (X-SEQ)  TO DC-BIN-DATE-1.                
01950      MOVE SPACES                 TO DC-OPTION-CODE.               
01951      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
01952      MOVE DC-GREG-DATE-1-EDIT    TO ICPL-DATE.                    
01953                                                                   
01954      MOVE ENT-LF-ISSUE  (X-SEQ)  TO ICPL-LF-ENT.                  
01955      MOVE ENT-AH-ISSUE  (X-SEQ)  TO ICPL-AH-ENT.                  
01956      MOVE ENT-ISSUE-CNT (X-SEQ)  TO ICPL-CNT-ENT.                 
01957      MOVE REM-LF-ISSUE  (X-SEQ)  TO ICPL-LF-REM.                  
01958      MOVE REM-AH-ISSUE  (X-SEQ)  TO ICPL-AH-REM.                  
01959      MOVE REM-ISSUE-CNT (X-SEQ)  TO ICPL-CNT-REM.                 
01960      ADD ENT-LF-ISSUE   (X-SEQ)  TO TOT-ENT-LF-ISSUE.             
01961      ADD ENT-AH-ISSUE   (X-SEQ)  TO TOT-ENT-AH-ISSUE.             
01962      ADD ENT-ISSUE-CNT  (X-SEQ)  TO TOT-ENT-ISSUE-CNT.            
01963      ADD REM-LF-ISSUE   (X-SEQ)  TO TOT-REM-LF-ISSUE.             
01964      ADD REM-AH-ISSUE   (X-SEQ)  TO TOT-REM-AH-ISSUE.             
01965      ADD REM-ISSUE-CNT  (X-SEQ)  TO TOT-REM-ISSUE-CNT.            
01966                                                                   
01967 ****  OUT OF BALANCE                                              
01968      SUBTRACT ENT-LF-ISSUE (X-SEQ) FROM REM-LF-ISSUE (X-SEQ)      
01969                         GIVING     ICPL-LF-OUT                    
01970                                    TMP-LF-OUT.                    
01971                                                                   
01972      SUBTRACT ENT-AH-ISSUE (X-SEQ) FROM REM-AH-ISSUE (X-SEQ)      
01973                         GIVING     ICPL-AH-OUT                    
01974                                    TMP-AH-OUT.                    
01975                                                                   
01976      SUBTRACT ENT-ISSUE-CNT (X-SEQ) FROM REM-ISSUE-CNT (X-SEQ)    
01977                         GIVING     ICPL-CNT-OUT                   
01978                                    TMP-CNT-OUT.                   
01979                                                                   
01980      ADD TMP-LF-OUT      TO TOT-LF-OUT.                           
01981      ADD TMP-AH-OUT      TO TOT-AH-OUT.                           
01982      ADD TMP-CNT-OUT     TO TOT-CNT-OUT.                          
01983                                                                   
01984      MOVE IC-TOTAL-PRINT-LINE TO P-DATA.                          
01985      MOVE '0'                 TO X.                               
01986      PERFORM 7000-PRINT-LINE.                                     
01987      ADD 2                    TO LINE-CT.                         
01988      ADD 1                    TO X-SEQ T-SEQ.                     
01989                                                                   
01990      GO TO 1000-PRT-TOTS-LOOP.                                    
01991                                                                   
01992  1000-PRT-BAT-TOT-HDR.                                            
01993      MOVE '***TOTALS***' TO H4-TOTAL.                             
01994      MOVE SPACES         TO H4-CONTD.                             
01995      MOVE HDG4           TO P-DATA.                               
01996      MOVE '0'            TO X.                                    
01997      PERFORM 7000-PRINT-LINE.                                     
01998      MOVE IC-TOT-HD1     TO P-DATA.                               
01999      MOVE '0'            TO X.                                    
02000      PERFORM 7000-PRINT-LINE.                                     
02001      MOVE IC-TOT-HD2     TO P-DATA.                               
02002      MOVE  ' '           TO X.                                    
02003      PERFORM 7000-PRINT-LINE.                                     
02004      ADD 7               TO LINE-CT.                              
02005                                                                   
02006  1000-BAT-TOT-HDR-EXIT.                                           
02007      EXIT.                                                        
02008                                                                   
02009  1000-END-TOTS.                                                   
02010      MOVE ZEROS TO      TOT-ENT-LF-ISSUE                          
02011                         TOT-ENT-AH-ISSUE                          
02012                         TOT-ENT-ISSUE-CNT                         
02013                         TOT-REM-LF-ISSUE                          
02014                         TOT-REM-AH-ISSUE                          
02015                         TOT-REM-ISSUE-CNT                         
02016                         TOT-LF-OUT                                
02017                         TOT-AH-OUT                                
02018                         TOT-CNT-OUT.                              
02019                                                                   
02020      MOVE SPACES TO ISSUE-CANCEL                                  
02021                     H4-TOTAL                                      
02022                     H4-CONTD.                                     
02023                                                                   
02024      MOVE 99    TO LINE-CT.                                       
02025                                                                   
02026  1000-PRT-TOTS-EXIT.                                              
02027      EXIT.                                                        
02028      EJECT                                                        
02029  1500-NEW-ACCT SECTION.                                           
02030      MOVE 1 TO X-SEQ.                                             
02031                                                                   
02032  1500-NEW-ACCT-LOOP.                                              
02033      MOVE ZEROS TO SBR-SEQ (X-SEQ).                               
02034                                                                   
02035      ADD 1 TO X-SEQ.                                              
02036                                                                   
02037      IF X-SEQ LESS X-MAX                                          
02038          GO TO 1500-NEW-ACCT-LOOP.                                
02039                                                                   
02040      MOVE PB-ACCOUNT      TO H4-ACCT.                             
02041                                                                   
02042      IF DTE-CLIENT = 'TCL' OR 'DMD'                               
02043          MOVE PB-CARRIER     TO H4-CARR                           
02044          MOVE PB-GROUPING    TO H4-GRP                            
02045          MOVE PB-STATE       TO H4-ST                             
02046      ELSE                                                         
02047          MOVE PB-SV-CARRIER  TO H4-CARR                           
02048          MOVE PB-SV-GROUPING TO H4-GRP                            
02049          MOVE PB-SV-STATE    TO H4-ST
               MOVE PB-CSR-ID      TO H4-CSR
           END-IF
02050                                                                   
02051      PERFORM 9000-GET-ACCT  THRU  9400-EXIT.                      
02052                                                                   
02053      MOVE PB-CARRIER  TO CUR-CARR.                                
02054      MOVE PB-GROUPING TO CUR-GRP.                                 
02055      MOVE PB-STATE    TO CUR-ST.                                  
02056      MOVE PB-ACCOUNT  TO CUR-ACCT.                                
02057      MOVE 1           TO X-SEQ.                                   
02058      MOVE 99          TO LINE-CT.                                 
02059                                                                   
02060  1500-NEW-ACCT-EXIT.                                              
02061      EXIT.                                                        
02062      EJECT                                                        
02063  2000-FORMAT-CANCEL SECTION.                                      
02064      IF ISSUES                                                    
02065         PERFORM 1000-PRT-TOTS THRU 1000-PRT-TOTS-EXIT.            
02066                                                                   
02067      ADD 4 TO LINE-CT.                                            
02068                                                                   
02069      IF LINE-CT GREATER MAX-LINES                                 
02070          PERFORM 7000-PRT-HDG THRU 7000-EXIT                      
02071          MOVE HDG3A   TO P-DATA                                   
02072          MOVE ' '     TO X                                        
02073          PERFORM 7000-PRINT-LINE                                  
02074          MOVE SPACES  TO H4-TOTAL H4-CONTD                        
02075          MOVE HDG4    TO P-DATA                                   
02076          MOVE ' '     TO X                                        
02077          PERFORM 7000-PRINT-LINE                                  
02078          PERFORM 7200-PRT-CN-HDG THRU 7200-EXIT.                  
02079                                                                   
02080      MOVE SPACES               TO CANCEL-PRINT-LINE.              
02081                                                                   
02082      MOVE PB-CERT-NO           TO CNPL-CERT.                      
02083                                                                   
02084      MOVE PB-CI-LF-TERM         TO CNPL-LFTRM.                    
02085      MOVE PB-CI-AH-TERM         TO CNPL-AHTRM.                    
02086                                                                   
02087      MOVE PB-CERT-EFF-DT       TO DC-BIN-DATE-1.                  
02088      MOVE SPACES               TO DC-OPTION-CODE.                 
02089      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
02090      MOVE DC-GREG-DATE-1-EDIT  TO CNPL-EFF-DT.                    
02091                                                                   
02092      MOVE PB-CI-INITIALS       TO CNPL-INIT.                      
02093      MOVE PB-CI-LAST-NAME      TO CNPL-NAME.                      
02094                                                                   
02095      IF PB-CI-LAST-NAME = SPACES                                  
02096          MOVE PB-C-LAST-NAME    TO CNPL-NAME.                     
02097                                                                   
02098      IF PB-C-LF-CANCEL-DT NOT = SPACES AND ZEROS AND LOW-VALUES   
02099          MOVE PB-C-LF-CANCEL-DT  TO DC-BIN-DATE-1                 
02100          MOVE SPACES             TO DC-OPTION-CODE                
02101          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02102          MOVE DC-GREG-DATE-1-MDY TO CNPL-LFCANC.                  
02103                                                                   
02104      IF PB-CI-LF-BENEFIT-CD NOT = ZERO                            
02105          MOVE PB-CI-LF-BENEFIT-CD   TO CNPL-LCOD.                 
02106                                                                   
02107      IF PB-OVERRIDE-LIFE    OR                                    
02108         PB-OVERRIDE-BOTH                                          
02109            MOVE PB-C-LF-REF-CALC TO PB-C-LF-CANCEL-AMT.           
02110                                                                   
02111      MOVE PB-C-LF-CANCEL-AMT    TO CNPL-LREF.                     
02112                                                                   
02113      IF PB-CI-AH-BENEFIT-CD NOT = ZEROS                           
02114          MOVE PB-CI-AH-BENEFIT-CD TO CNPL-ACOD.                   
02115                                                                   
02116      IF PB-OVERRIDE-AH      OR                                    
02117         PB-OVERRIDE-BOTH                                          
02118            MOVE PB-C-AH-REF-CALC TO PB-C-AH-CANCEL-AMT.           
02119                                                                   
02120      MOVE PB-C-AH-CANCEL-AMT    TO CNPL-AREF.                     
02121                                                                   
02122      IF PB-CI-AH-PAID-THRU-DT  NOT = SPACES AND ZEROS             
02123                                   AND LOW-VALUES                  
02124         MOVE PB-CI-AH-PAID-THRU-DT TO DC-BIN-DATE-1               
02125         MOVE SPACES                TO DC-OPTION-CODE              
02126         PERFORM 6000-DATE-RTN  THRU 6000-EXIT                     
02127         MOVE DC-GREG-DATE-1-EDIT   TO CNPL-PDTHRU.                
02128                                                                   
02129      IF PB-CI-LF-PRIOR-CANCEL-DT = SPACES OR ZEROS OR LOW-VALUES  
02130          NEXT SENTENCE                                            
02131      ELSE                                                         
02132          MOVE PB-CI-LF-PRIOR-CANCEL-DT                            
02133                                  TO  DC-BIN-DATE-1                
02134          MOVE ' '                TO  DC-OPTION-CODE               
02135          PERFORM 6000-DATE-RTN  THRU  6000-EXIT                   
02136          MOVE DC-GREG-DATE-1-EDIT                                 
02137                                  TO  CNPL-LF-CANCEL.              
02138                                                                   
02139      EVALUATE PB-LF-REFUND-TYPE                                   
02140         WHEN '1'                                                  
02141            MOVE 'RULE78'            TO CNPL-LFMETH                
02142         WHEN '2'                                                  
02143            MOVE 'PRORAT'            TO CNPL-LFMETH                
02144         WHEN '3'                                                  
02145            MOVE 'CALIF'             TO CNPL-LFMETH                
02146         WHEN '4'                                                  
02147            MOVE 'TEXAS'             TO CNPL-LFMETH                
02148         WHEN '5'                                                  
02149            MOVE 'NETPAY'            TO CNPL-LFMETH                
02150         WHEN '6'                                                  
02151            MOVE 'ATCP'              TO CNPL-LFMETH                
02152         WHEN '7'                                                  
02153            MOVE 'UTAH'              TO CNPL-LFMETH                
02154         WHEN '9'                                                  
02155            MOVE 'SUM   '            TO CNPL-LFMETH                
02156      END-EVALUATE.                                                
02157                                                                   
02158      MOVE PB-CI-LOAN-APR        TO CNPL-APR.                      
02159      MOVE PB-FORCE-CODE         TO CNPL-FC.                       
02160                                                                   
02161      MOVE PB-INPUT-DT          TO DC-BIN-DATE-1.                  
02162      MOVE SPACES               TO DC-OPTION-CODE.                 
02163      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
02164      MOVE DC-GREG-DATE-1-MDY   TO CNPL-INPUT.                     
02165                                                                   
02166      MOVE PB-ENTRY-BATCH       TO CNPL-BATCH.                     
02167                                                                   
02168      IF PB-BATCH-CHG-SEQ-NO  NOT = ZERO                           
02169          MOVE SPACES             TO CNPL-CERT                     
02170                                     CNPL-EFF-DT                   
02171          MOVE 'CHANGE REC'       TO CNPL-NM.                      
02172                                                                   
02173      MOVE CANCEL-PRINT-LINE TO P-DATA.                            
02174      MOVE '0'               TO X.                                 
02175      PERFORM 7000-PRINT-LINE.                                     
02176  EJECT                                                            
02177  2000-FORMAT-2ND-LINE.                                            
02178      MOVE SPACES                TO CANCEL-PRINT-LINE.             
02179                                                                   
02180      MOVE PB-CI-SOC-SEC-NO      TO CNPL-SOC-SEC.                  
02181                                                                   
02182      IF CNPL-SOC-SEC-2  = PB-SV-STATE                             
02183          MOVE SPACES            TO CNPL-SOC-SEC.                  
02184                                                                   
02185      IF PB-C-AH-CANCEL-DT NOT = ZEROS AND LOW-VALUES AND SPACES   
02186          MOVE PB-C-AH-CANCEL-DT    TO DC-BIN-DATE-1               
02187          MOVE SPACES               TO DC-OPTION-CODE              
02188          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02189          MOVE DC-GREG-DATE-1-MDY   TO CNPL-AHCANC.                
02190                                                                   
02191      MOVE PB-C-LF-REM-TERM      TO CNPL-LFREM.                    
02192      MOVE PB-C-AH-REM-TERM      TO CNPL-AHREM.                    
02193                                                                   
02194      IF PB-CI-LF-BENEFIT-CD  NOT = ZEROS                          
02195          MOVE PB-CI-LF-BENEFIT-CD TO CNPL-LTYP.                   
02196                                                                   
02197      IF PB-CI-AH-BENEFIT-CD  NOT = ZEROS                          
02198          MOVE PB-CI-AH-BENEFIT-CD TO CNPL-ATYP.                   
02199                                                                   
02200      MOVE PB-C-LF-REF-CALC      TO CNPL-LREF1.                    
02201      MOVE PB-C-AH-REF-CALC      TO CNPL-AREF1.                    
02202                                                                   
02203      IF PB-CI-DEATH-DT  NOT = SPACES AND ZEROS AND LOW-VALUES     
02204          MOVE PB-CI-DEATH-DT       TO DC-BIN-DATE-1               
02205          MOVE SPACES               TO DC-OPTION-CODE              
02206          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02207          MOVE DC-GREG-DATE-1-EDIT  TO CNPL-DEATH.                 
02208                                                                   
02209      IF PB-CI-AH-PRIOR-CANCEL-DT = SPACES OR ZEROS OR LOW-VALUES  
02210          NEXT SENTENCE                                            
02211      ELSE                                                         
02212          MOVE PB-CI-AH-PRIOR-CANCEL-DT                            
02213                                  TO  DC-BIN-DATE-1                
02214          MOVE ' '                TO  DC-OPTION-CODE               
02215          PERFORM 6000-DATE-RTN  THRU  6000-EXIT                   
02216          MOVE DC-GREG-DATE-1-EDIT                                 
02217                                  TO  CNPL-AH-CANCEL.              
02218                                                                   
02219      IF PB-AH-REFUND-TYPE = '1'                                   
02220        MOVE 'RULE78'            TO CNPL-AHMETH                    
02221       ELSE                                                        
02222        IF PB-AH-REFUND-TYPE = '2'                                 
02223          MOVE 'PRORAT'            TO CNPL-AHMETH                  
02224         ELSE                                                      
02225          IF PB-AH-REFUND-TYPE = '3'                               
02226            MOVE 'CALIF'             TO CNPL-AHMETH                
02227           ELSE                                                    
02228            IF PB-AH-REFUND-TYPE = '4'                             
02229              MOVE 'TEXAS'             TO CNPL-AHMETH              
02230             ELSE                                                  
02231              IF PB-AH-REFUND-TYPE = '5'                           
02232                MOVE 'NETPAY'            TO CNPL-AHMETH            
02233               ELSE                                                
02234                IF PB-AH-REFUND-TYPE = '6'                         
02235                  MOVE 'ATCP'              TO CNPL-AHMETH          
02236                 ELSE                                              
02237                  IF PB-AH-REFUND-TYPE = '7'                       
02238                    MOVE 'UTAH'              TO CNPL-AHMETH.       
02239                                                                   
02240      IF PB-C-LIVES NUMERIC                                        
02241          MOVE PB-C-LIVES         TO CNPL-LIVES                    
02242        ELSE                                                       
02243          MOVE ZEROS              TO CNPL-LIVES.                   
02244                                                                   
02245      IF PB-FORCE-ERRORS                                           
02246          MOVE 'FORCE'            TO CNPL-STAT                     
02247          MOVE PB-LAST-MAINT-BY   TO CNPL-WHO.                     
02248      IF PB-WARNING-ERRORS                                         
02249          MOVE 'WARN'             TO CNPL-STAT.                    
02250      IF PB-UNFORCED-ERRORS                                        
02251          MOVE 'UNFORC'           TO CNPL-STAT.                    
02252      IF PB-FATAL-ERRORS                                           
02253          MOVE 'FATAL'            TO CNPL-STAT.                    
02254      IF PB-RECORD-ON-HOLD                                         
02255          MOVE 'HOLD'             TO CNPL-STAT.                    
02256      IF PB-RECORD-RETURNED                                        
02257          MOVE 'RETRND'           TO CNPL-STAT.                    
02258                                                                   
02259      IF PB-OVERRIDE-LIFE    OR                                    
02260         PB-OVERRIDE-AH      OR                                    
02261         PB-OVERRIDE-BOTH                                          
02262            MOVE 'OVRIDE'         TO CNPL-STAT.                    
02263                                                                   
02264 ***  CHANGE LEVEL                                                 
02265      IF PB-BATCH-CHG-SEQ-NO  NOT = ZERO                           
02266          COMPUTE CNPL-LEVEL = +1000 - PB-BATCH-CHG-SEQ-NO         
02267          MOVE 'EL-'             TO CNPL-CL.                       
02268                                                                   
02269      MOVE SECOND-CANCEL-LINE TO P-DATA.                           
02270      MOVE ' '                TO X.                                
02271      PERFORM 7000-PRINT-LINE.                                     
02272    EJECT                                                          
02273  2000-FORMAT-3RD-LINE.                                            
02274      MOVE SPACES                TO CANCEL-PRINT-LINE.             
02275                                                                   
02276      MOVE PB-CI-MEMBER-NO       TO CNPL-MEMBER.                   
02277                                                                   
02278      IF CNPL-MEMBER-2  = PB-SV-STATE                              
02279          MOVE SPACES            TO CNPL-MEMBER.                   
02280                                                                   
02281      IF PB-CI-LF-ABBR  NOT = ZEROS                                
02282          MOVE PB-CI-LF-ABBR     TO CNPL-LTYP1.                    
02283                                                                   
02284      IF PB-CI-AH-ABBR  NOT = ZEROS                                
02285          MOVE PB-CI-AH-ABBR     TO CNPL-ATYP1.                    
02286                                                                   
02287      SUBTRACT PB-C-LF-CANCEL-AMT FROM PB-C-LF-REF-CALC            
02288                        GIVING      CNPL-LREF2.                    
02289                                                                   
02290      SUBTRACT PB-C-AH-CANCEL-AMT FROM PB-C-AH-REF-CALC            
02291                        GIVING      CNPL-AREF2.                    
02292                                                                   
02293      IF PB-BILLED-DT NOT = ZEROS AND LOW-VALUES AND SPACES        
02294          MOVE PB-BILLED-DT         TO DC-BIN-DATE-1               
02295          MOVE SPACES               TO DC-OPTION-CODE              
02296          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02297          MOVE DC-GREG-DATE-1-MDY   TO CNPL-BILL.                  
02298                                                                   
02299      MOVE THIRD-CANCEL-LINE TO P-DATA.                            
02300      MOVE ' '               TO X.                                 
02301      PERFORM 7000-PRINT-LINE.                                     
02302                                                                   
02303      IF WS-ERROR-2736 = PB-COMMON-ERROR (1) OR                    
02304                         PB-COMMON-ERROR (2) OR                    
02305                         PB-COMMON-ERROR (3) OR                    
02306                         PB-COMMON-ERROR (4) OR                    
02307                         PB-COMMON-ERROR (5) OR                    
02308                         PB-COMMON-ERROR (6) OR                    
02309                         PB-COMMON-ERROR (7) OR                    
02310                         PB-COMMON-ERROR (8) OR                    
02311                         PB-COMMON-ERROR (9) OR                    
02312                         PB-COMMON-ERROR (10)                      
02313          MOVE SPACES             TO PREMIUM-PRINT-LINE            
02314          MOVE LIFE-OVERRIDE-L2   TO PPL-LF                        
02315          MOVE '-PREM'            TO PPL-DES-1 PPL-DES-2           
02316          MOVE AH-OVERRIDE-L2     TO PPL-AH                        
02317          MOVE PB-CI-LF-PREMIUM-AMT                                
02318                                  TO PPL-LF-PREM                   
02319          MOVE PB-CI-AH-PREMIUM-AMT                                
02320                                  TO PPL-AH-PREM                   
02321          MOVE PREMIUM-PRINT-LINE TO P-DATA                        
02322          MOVE ' '                TO X                             
02323          ADD 1                   TO LINE-CT                       
02324          PERFORM 7000-PRINT-LINE.                                 
02325                                                                   
02326      PERFORM 4500-FORMAT-ERRORS THRU 4599-EXIT.                   
091604     PERFORM 4700-FORMAT-BILL-NOTES
091604                                 THRU 4700-EXIT
02327                                                                   
02328      MOVE '2' TO ISSUE-CANCEL.                                    
02329                                                                   
02330  2000-FORMAT-CANCEL-EXIT.                                         
02331      EXIT.                                                        
02332     EJECT                                                         
02333  2000-ADD-CANCELS SECTION.                                        
02334      IF PB-BATCH-CHG-SEQ-NO NOT = ZERO                            
02335          GO TO 2000-ADD-CANCELS-EXIT.                             
02336                                                                   
02337      PERFORM 2500-SUMMARIZE THRU 2500-EXIT.                       
02338                                                                   
02339      ADD 1 TO CANCEL-CNT.                                         
02340                                                                   
02341      PERFORM 2600-ADD-USER-TABLE THRU 2699-EXIT.                  
02342                                                                   
CIDMOD     IF DTE-FMT-OPT = '2'
CIDMOD        GO TO 2000-ADD-CANCELS-EXIT                               
CIDMOD     END-IF
CIDMOD
02343      IF  PB-FATAL-ERRORS    OR                                    
02344          PB-UNFORCED-ERRORS OR                                    
02345          PB-RECORD-ON-HOLD  OR                                    
02346          PB-RECORD-RETURNED                                       
02347            NEXT SENTENCE                                          
02348         ELSE                                                      
02349            GO TO 2000-PROCESSABLE.                                
02350                                                                   
02351      IF PB-RECORD-ON-HOLD                                         
02352          ADD 1 TO HOLD-CANCEL-CNT                                 
02353          GO TO 2000-NON-PROCESSABLE.                              
02354                                                                   
02355      IF PB-RECORD-RETURNED                                        
02356          ADD 1 TO RETURNED-CANCEL-CNT                             
02357          GO TO 2000-NON-PROCESSABLE.                              
02358                                                                   
02359      IF PB-FATAL-ERRORS                                           
02360          ADD 1 TO FATAL-CANCEL-CNT                                
02361          GO TO 2000-NON-PROCESSABLE.                              
02362                                                                   
02363      IF PB-UNFORCED-ERRORS                                        
02364          ADD 1 TO UNFORCE-CANCEL-CNT.                             
02365                                                                   
02366      GO TO 2000-NON-PROCESSABLE.                                  
02367                                                                   
02368  2000-PROCESSABLE.                                                
02369      ADD 1 TO AVAILABLE-CANCEL-CNT.                               
02370                                                                   
02371      IF PB-CI-ENTRY-STATUS = '9'                                  
02372          GO TO 2000-ADD-CANCELS-EXIT.                             
02373                                                                   
02374      IF PB-OVERRIDE-LIFE                                          
02375        OR PB-OVERRIDE-BOTH                                        
02376          IF PB-C-LF-REF-CALC = ZERO                               
02377              NEXT SENTENCE                                        
02378          ELSE                                                     
02379              ADD PB-C-LF-REF-CALC                                 
02380                                  TO  TPA-LREF                     
02381      ELSE                                                         
02382          IF PB-C-LF-CANCEL-AMT NOT = ZERO                         
02383              ADD PB-C-LF-CANCEL-AMT                               
02384                                  TO  TPA-LREF.                    
02385                                                                   
02386      IF PB-OVERRIDE-AH                                            
02387        OR PB-OVERRIDE-BOTH                                        
02388          IF PB-C-AH-REF-CALC = ZERO                               
02389              NEXT SENTENCE                                        
02390          ELSE                                                     
02391              ADD PB-C-AH-REF-CALC                                 
02392                                  TO  TPA-AREF                     
02393      ELSE                                                         
02394          IF PB-C-AH-CANCEL-AMT NOT = ZERO                         
02395              ADD PB-C-AH-CANCEL-AMT                               
02396                                  TO  TPA-AREF.                    
02397                                                                   
02398      GO TO 2000-ADD-CANCELS-EXIT.                                 
02399                                                                   
02400  2000-NON-PROCESSABLE.                                            
02401      IF PB-CI-ENTRY-STATUS = '9'                                  
02402          GO TO 2000-ADD-CANCELS-EXIT.                             
02403                                                                   
02404      IF PB-OVERRIDE-LIFE                                          
02405        OR PB-OVERRIDE-BOTH                                        
02406          IF PB-C-LF-REF-CALC = ZERO                               
02407              NEXT SENTENCE                                        
02408          ELSE                                                     
02409              ADD PB-C-LF-REF-CALC                                 
02410                                  TO  TNA-LREF                     
02411      ELSE                                                         
02412          IF PB-C-LF-CANCEL-AMT NOT = ZERO                         
02413              ADD PB-C-LF-CANCEL-AMT                               
02414                                  TO  TNA-LREF.                    
02415                                                                   
02416      IF PB-OVERRIDE-AH                                            
02417        OR PB-OVERRIDE-BOTH                                        
02418          IF PB-C-AH-REF-CALC = ZERO                               
02419              NEXT SENTENCE                                        
02420          ELSE                                                     
02421              ADD PB-C-AH-REF-CALC                                 
02422                                  TO  TNA-AREF                     
02423      ELSE                                                         
02424          IF PB-C-AH-CANCEL-AMT NOT = ZERO                         
02425              ADD PB-C-AH-CANCEL-AMT                               
02426                                  TO  TNA-AREF.                    
02427                                                                   
02428  2000-ADD-CANCELS-EXIT.                                           
02429      EXIT.                                                        
02430    EJECT                                                          
02431  2000-PRT-TOTS SECTION.                                           
02432      MOVE SPACES TO IC-TOTAL-PRINT-LINE.                          
02433                                                                   
02434      IF LINE-CT = 99                                              
02435          GO TO 2000-PRT-TOTS-EXIT.                                
02436                                                                   
02437      MOVE 1  TO X-SEQ T-SEQ.                                      
02438                                                                   
02439  2000-PRT-TOTS-LOOP.                                              
02440      IF SBR-SEQ (X-SEQ) = ZERO                                    
02441          IF T-SEQ LESS 3                                          
02442              GO TO 2000-END-TOTS.                                 
02443                                                                   
02444      IF SBR-SEQ (X-SEQ) NOT = ZERO                                
02445          IF ENT-CANCEL-CNT (X-SEQ) = 0   AND                      
02446             REM-CANCEL-CNT (X-SEQ) = 0                            
02447                ADD 1 TO X-SEQ                                     
02448                GO TO 2000-PRT-TOTS-LOOP.                          
02449                                                                   
02450      IF LINE-CT GREATER (MAX-LINES - 8)                           
02451          PERFORM 7000-PRT-HDG THRU 7000-EXIT                      
02452          MOVE HDG3A      TO P-DATA                                
02453          MOVE ' '        TO X                                     
02454          PERFORM 7000-PRINT-LINE                                  
02455          MOVE HDG4       TO P-DATA                                
02456          MOVE ' '        TO X                                     
02457          PERFORM 7000-PRINT-LINE                                  
02458          MOVE CANCEL-HD1 TO P-DATA                                
02459          MOVE '0'        TO X                                     
02460          PERFORM 7000-PRINT-LINE                                  
02461          MOVE 7          TO LINE-CT                               
02462          PERFORM 2000-PRT-BAT-TOT-HDR                             
02463             THRU 2000-BAT-TOT-HDR-EXIT                            
02464        ELSE                                                       
02465          IF T-SEQ = 1                                             
02466              PERFORM 2000-PRT-BAT-TOT-HDR                         
02467                 THRU 2000-BAT-TOT-HDR-EXIT.                       
02468                                                                   
02469      MOVE WS-CONTD TO H4-CONTD.                                   
02470                                                                   
02471      IF SBR-SEQ (X-SEQ) = 0                                       
02472        IF T-SEQ GREATER 2                                         
02473           MOVE TOT-ENT-LF-CANCEL   TO ICPL-LF-ENT                 
02474           MOVE TOT-ENT-AH-CANCEL   TO ICPL-AH-ENT                 
02475           MOVE TOT-ENT-CANCEL-CNT  TO ICPL-CNT-ENT                
02476           MOVE TOT-REM-LF-CANCEL   TO ICPL-LF-REM                 
02477           MOVE TOT-REM-AH-CANCEL   TO ICPL-AH-REM                 
02478           MOVE TOT-REM-CANCEL-CNT  TO ICPL-CNT-REM                
02479           MOVE TOT-LF-OUT          TO ICPL-LF-OUT                 
02480           MOVE TOT-AH-OUT          TO ICPL-AH-OUT                 
02481           MOVE TOT-CNT-OUT         TO ICPL-CNT-OUT                
02482           MOVE 'TOTALS'            TO ICPL-BATCH                  
02483           MOVE SPACES              TO ICPL-DATE                   
02484           MOVE IC-TOTAL-PRINT-LINE TO P-DATA                      
02485           MOVE '0'                 TO X                           
02486           PERFORM 7000-PRINT-LINE                                 
02487           ADD 2                    TO LINE-CT                     
02488           GO TO 2000-END-TOTS                                     
02489        ELSE                                                       
02490           GO TO 2000-END-TOTS.                                    
02491                                                                   
02492      MOVE SAVE-BATCH-NO (X-SEQ)  TO ICPL-BATCH.                   
02493                                                                   
02494      MOVE SAVE-BATCH-DT (X-SEQ)  TO DC-BIN-DATE-1.                
02495      MOVE SPACES                 TO DC-OPTION-CODE.               
02496      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
02497      MOVE DC-GREG-DATE-1-EDIT    TO ICPL-DATE.                    
02498                                                                   
02499      MOVE ENT-LF-CANCEL  (X-SEQ) TO ICPL-LF-ENT.                  
02500      MOVE ENT-AH-CANCEL  (X-SEQ) TO ICPL-AH-ENT.                  
02501      MOVE ENT-CANCEL-CNT (X-SEQ) TO ICPL-CNT-ENT.                 
02502      MOVE REM-LF-CANCEL  (X-SEQ) TO ICPL-LF-REM.                  
02503      MOVE REM-AH-CANCEL  (X-SEQ) TO ICPL-AH-REM.                  
02504      MOVE REM-CANCEL-CNT (X-SEQ) TO ICPL-CNT-REM.                 
02505      ADD ENT-LF-CANCEL   (X-SEQ) TO TOT-ENT-LF-CANCEL.            
02506      ADD ENT-AH-CANCEL   (X-SEQ) TO TOT-ENT-AH-CANCEL.            
02507      ADD ENT-CANCEL-CNT  (X-SEQ) TO TOT-ENT-CANCEL-CNT.           
02508      ADD REM-LF-CANCEL   (X-SEQ) TO TOT-REM-LF-CANCEL.            
02509      ADD REM-AH-CANCEL   (X-SEQ) TO TOT-REM-AH-CANCEL.            
02510      ADD REM-CANCEL-CNT  (X-SEQ) TO TOT-REM-CANCEL-CNT.           
02511                                                                   
02512 ****  OUT OF BALANCE                                              
02513      SUBTRACT ENT-LF-CANCEL (X-SEQ) FROM REM-LF-CANCEL (X-SEQ)    
02514                         GIVING     ICPL-LF-OUT                    
02515                                    TMP-LF-OUT.                    
02516      SUBTRACT ENT-AH-CANCEL (X-SEQ) FROM REM-AH-CANCEL (X-SEQ)    
02517                         GIVING     ICPL-AH-OUT                    
02518                                    TMP-AH-OUT.                    
02519      SUBTRACT ENT-CANCEL-CNT (X-SEQ) FROM REM-CANCEL-CNT (X-SEQ)  
02520                         GIVING     ICPL-CNT-OUT                   
02521                                    TMP-CNT-OUT.                   
02522                                                                   
02523      ADD TMP-LF-OUT      TO TOT-LF-OUT.                           
02524      ADD TMP-AH-OUT      TO TOT-AH-OUT.                           
02525      ADD TMP-CNT-OUT     TO TOT-CNT-OUT.                          
02526                                                                   
02527      MOVE IC-TOTAL-PRINT-LINE TO P-DATA.                          
02528      MOVE '0' TO X.                                               
02529      PERFORM 7000-PRINT-LINE.                                     
02530      ADD 2   TO LINE-CT.                                          
02531      ADD  1  TO X-SEQ T-SEQ.                                      
02532      GO TO 2000-PRT-TOTS-LOOP.                                    
02533                                                                   
02534  2000-PRT-BAT-TOT-HDR.                                            
02535      MOVE '***TOTALS***' TO H4-TOTAL.                             
02536      MOVE SPACES         TO H4-CONTD.                             
02537      MOVE HDG4           TO P-DATA.                               
02538      MOVE '0'            TO X.                                    
02539      PERFORM 7000-PRINT-LINE.                                     
02540      MOVE IC-TOT-HD1     TO P-DATA.                               
02541      MOVE '0'            TO X.                                    
02542      PERFORM 7000-PRINT-LINE.                                     
02543      MOVE IC-TOT-HD2-1   TO P-DATA.                               
02544      MOVE  ' '           TO X.                                    
02545      PERFORM 7000-PRINT-LINE.                                     
02546      ADD 7               TO LINE-CT.                              
02547                                                                   
02548  2000-BAT-TOT-HDR-EXIT.                                           
02549      EXIT.                                                        
02550                                                                   
02551  2000-END-TOTS.                                                   
02552      MOVE ZEROS TO      TOT-ENT-LF-CANCEL                         
02553                         TOT-ENT-AH-CANCEL                         
02554                         TOT-ENT-CANCEL-CNT                        
02555                         TOT-REM-LF-CANCEL                         
02556                         TOT-REM-AH-CANCEL                         
02557                         TOT-REM-CANCEL-CNT                        
02558                         TOT-LF-OUT                                
02559                         TOT-AH-OUT                                
02560                         TOT-CNT-OUT.                              
02561                                                                   
02562      MOVE SPACE      TO ISSUE-CANCEL.                             
02563      MOVE 99         TO LINE-CT.                                  
02564                                                                   
02565  2000-PRT-TOTS-EXIT.                                              
02566      EXIT.                                                        
02567    EJECT                                                          
02568  2500-SUMMARIZE SECTION.                                          
02569      MOVE PB-INPUT-DT         TO DC-BIN-DATE-1.                   
02570      MOVE SPACES              TO DC-OPTION-CODE.                  
02571      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
02572      MOVE DC-GREG-DATE-1-YMD  TO WKDATE.                          
02573      MOVE DC-ALPHA-CEN-N      TO WKCC.                            
02574                                                                   
02575      IF WKDATE LESS EARLIEST-DATE                                 
02576          MOVE WKDATE TO EARLIEST-DATE.                            
02577                                                                   
02578      IF WKDATE GREATER LATEST-DATE                                
02579          MOVE WKDATE TO LATEST-DATE.                              
02580                                                                   
02581      MOVE WS-CURRENT-DATE    TO DC-GREG-DATE-1-EDIT.              
02582      MOVE '2'                TO DC-OPTION-CODE.                   
02583      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
02584      MOVE DC-BIN-DATE-1      TO DC-BIN-DATE-2                     
02585      MOVE PB-INPUT-DT        TO DC-BIN-DATE-1.                    
02586      MOVE '1'                TO DC-OPTION-CODE.                   
02587      PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                       
02588                                                                   
02589      IF DC-ELAPSED-DAYS LESS 60 AND GREATER 30                    
02590          ADD 1 TO OVER30.                                         
02591      IF DC-ELAPSED-DAYS LESS 90 AND GREATER 60                    
02592          ADD 1 TO OVER60.                                         
02593      IF DC-ELAPSED-DAYS GREATER 90                                
02594          ADD 1 TO OVER90.                                         
02595                                                                   
02596  2500-EXIT.                                                       
02597      EXIT.                                                        
02598      EJECT                                                        
02599  2600-DUMMY-SECTION    SECTION.                                   
02600                                                                   
02601  2600-ADD-USER-TABLE.                                             
02602      IF PB-INPUT-BY = USER-SUM-SRCH                               
02603          NEXT SENTENCE                                            
02604        ELSE                                                       
02605          MOVE PB-INPUT-BY TO USER-SUM-SRCH                        
02606          PERFORM 2700-SRCH-USR-TABLE THRU 2799-EXIT.              
02607
02608      MOVE ZEROS                  TO  WS-TEST-INPUT-DT.            
02609      MOVE PB-INPUT-DT            TO  DC-BIN-DATE-1.               
02610      MOVE ' '                    TO  DC-OPTION-CODE.              
02611                                                                   
02612      PERFORM 6000-DATE-RTN  THRU  6000-EXIT.                      
02613                                                                   
02614      MOVE DC-GREG-DATE-1-MDY     TO  WS-TEST-INPUT-DT.            
02615                                                                   
02616      IF WS-TID-MM  =  RUN-MO                                      
02617        AND WS-TID-YY  =  RUN-YR                                   
02618          NEXT SENTENCE                                            
02619      ELSE                                                         
02620          GO TO 2610-CONTINUE.                                     
02621                                                                   
02622      ADD 1 TO UST-DET-INP (USTINDX)                               
02623               UST-DET-INP (101).                                  
02624                                                                   
102706     .
02625  2610-CONTINUE.                                                   
02626      IF PB-LAST-MAINT-BY = 'LGXX'                                 
02627          GO TO 2699-EXIT.                                         
02628                                                                   
02629      IF PB-LAST-MAINT-BY = PB-INPUT-BY                            
02630          IF PB-LAST-MAINT-DT = PB-INPUT-DT                        
02631              GO TO 2699-EXIT                                      
02632            ELSE                                                   
02633              NEXT SENTENCE                                        
02634        ELSE                                                       
02635          MOVE PB-LAST-MAINT-BY TO USER-SUM-SRCH                   
02636          PERFORM 2700-SRCH-USR-TABLE THRU 2799-EXIT.              
02637                                                                   
02638      MOVE ZEROS                  TO  WS-TEST-MAINT-DT.            
02639      MOVE PB-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               
02640      MOVE ' '                    TO  DC-OPTION-CODE.              
02641                                                                   
02642      PERFORM 6000-DATE-RTN  THRU  6000-EXIT.                      
02643                                                                   
02644      MOVE DC-GREG-DATE-1-MDY     TO  WS-TEST-MAINT-DT.            
02645                                                                   
02646      IF WS-TMD-MM  =  RUN-MO                                      
02647        AND WS-TMD-YY  =  RUN-YR                                   
02648          NEXT SENTENCE                                            
02649      ELSE                                                         
02650          GO TO 2699-EXIT.                                         
02651                                                                   
02652      ADD 1 TO UST-DET-MNTC (USTINDX)                              
02653               UST-DET-MNTC (101).                                 

102706     IF PB-I-SIG-SW = 'Y'
102706        IF (PB-FORCE-CODE = ' ' OR '0')
102706           AND (PB-FATAL-FLAG NOT = 'X')
102706           AND (PB-FORCE-ER-CD NOT = 'F' AND 'X')
102706           ADD 1                 TO UST-SIG-SW (USTINDX)
102706                                    UST-SIG-SW (101)
102706        END-IF
102706     END-IF
102706
02655      IF PB-FORCE-ERRORS                                           
02656          ADD 1 TO UST-UPFRC-PROC (USTINDX)                        
02657                   UST-UPFRC-PROC (101).                           
02658                                                                   
02659  2699-EXIT.                                                       
02660      EXIT.                                                        
02661                                                                   
02662  2700-SRCH-USR-TABLE.                                             
02663      SET USTINDX TO 1.                                            
02664      GO TO 2720-CK-SPACES.                                        
02665                                                                   
02666  2710-SET-USTINDX-UP.                                             
02667      SET USTINDX UP BY 1.                                         
02668      IF USTINDX GREATER 100                                       
02669          MOVE 'USER TABLE SIZE EXCEEDED - 100'                    
02670                                TO WS-ABEND-MESSAGE                
02671          GO TO ABEND-PGM.                                         
02672                                                                   
02673  2720-CK-SPACES.                                                  
02674      IF UST-USER (USTINDX) = SPACES                               
02675          SET USTINDXHI1 TO USTINDX                                
02676          GO TO 2790-ADD-ENTRY.                                    
02677                                                                   
02678      IF UST-USER (USTINDX) = USER-SUM-SRCH                        
02679          GO TO 2799-EXIT.                                         
02680                                                                   
02681      IF UST-USER (USTINDX) LESS USER-SUM-SRCH                     
02682          GO TO 2710-SET-USTINDX-UP.                               
02683                                                                   
02684      SET USTINDXHI1 UP BY 1.                                      
02685      IF USTINDXHI1 GREATER 100                                    
02686          MOVE 'USER TABLE SIZE EXCEEDED - 101'                    
02687                         TO WS-ABEND-MESSAGE                       
02688          GO TO ABEND-PGM.                                         
02689                                                                   
02690      SET USTINDXHI2 TO USTINDXHI1.                                
02691                                                                   
02692  2740-SET-INDX2-DOWN.                                             
02693      SET USTINDXHI2 DOWN BY 1.                                    
02694                                                                   
02695      MOVE USER-SUM-TBL (USTINDXHI2)                               
02696                      TO USER-SUM-TBL (USTINDXHI2 + 1).            
02697                                                                   
02698      IF USTINDXHI2 GREATER USTINDX                                
02699          GO TO 2740-SET-INDX2-DOWN.                               
02700                                                                   
02701  2790-ADD-ENTRY.                                                  
02702      MOVE USER-SUM-SRCH TO UST-USER (USTINDX).                    
02703                                                                   
02704      MOVE ZERO TO UST-BAT-INP (USTINDX)                           
02705                   UST-DET-INP (USTINDX)                           
02706                   UST-BAT-MNTC (USTINDX)                          
02707                   UST-DET-MNTC (USTINDX)                          
02708                   UST-UPFRC-PROC (USTINDX)
102706                  UST-SIG-SW     (USTINDX)
           .
02710  2799-EXIT.                                                       
02711      EXIT.                                                        
02712                                                                   
02713  2800-ADD-USER-BATCH.                                             
02714      IF PB-INPUT-BY = USER-SUM-SRCH                               
02715          NEXT SENTENCE                                            
02716        ELSE                                                       
02717          MOVE PB-INPUT-BY TO USER-SUM-SRCH                        
02718          PERFORM 2700-SRCH-USR-TABLE THRU 2799-EXIT.              
02719                                                                   
02720      MOVE ZEROS                  TO  WS-TEST-INPUT-DT.            
02721      MOVE PB-INPUT-DT            TO  DC-BIN-DATE-1.               
02722      MOVE ' '                    TO  DC-OPTION-CODE.              
02723                                                                   
02724      PERFORM 6000-DATE-RTN  THRU  6000-EXIT.                      
02725                                                                   
02726      MOVE DC-GREG-DATE-1-MDY     TO  WS-TEST-INPUT-DT.            
02727                                                                   
02728      IF WS-TID-MM  =  RUN-MO                                      
02729        AND WS-TID-YY  =  RUN-YR                                   
02730          NEXT SENTENCE                                            
02731      ELSE                                                         
02732          GO TO 2810-CONTINUE.                                     
02733                                                                   
02734      ADD 1 TO UST-BAT-INP (USTINDX)                               
02735               UST-BAT-INP (101).                                  
02736                                                                   
02737  2810-CONTINUE.                                                   
02738      IF PB-LAST-MAINT-BY = 'LGXX'                                 
02739          GO TO 2899-EXIT.                                         
02740                                                                   
02741      IF PB-LAST-MAINT-BY = PB-INPUT-BY                            
02742          IF PB-LAST-MAINT-DT = PB-INPUT-DT                        
02743            GO TO 2899-EXIT                                        
02744           ELSE                                                    
02745            NEXT SENTENCE                                          
02746        ELSE                                                       
02747          MOVE PB-LAST-MAINT-BY TO USER-SUM-SRCH                   
02748          PERFORM 2700-SRCH-USR-TABLE THRU 2799-EXIT.              
02749                                                                   
02750      MOVE ZEROS                  TO  WS-TEST-MAINT-DT.            
02751      MOVE PB-LAST-MAINT-DT       TO  DC-BIN-DATE-1.               
02752      MOVE ' '                    TO  DC-OPTION-CODE.              
02753                                                                   
02754      PERFORM 6000-DATE-RTN  THRU  6000-EXIT.                      
02755                                                                   
02756      MOVE DC-GREG-DATE-1-MDY     TO  WS-TEST-MAINT-DT.            
02757                                                                   
02758      IF WS-TMD-MM  =  RUN-MO                                      
02759        AND WS-TMD-YY  =  RUN-YR                                   
02760          NEXT SENTENCE                                            
02761      ELSE                                                         
02762          GO TO 2899-EXIT.                                         
02763                                                                   
02764      ADD 1 TO UST-BAT-MNTC (USTINDX)                              
02765               UST-BAT-MNTC (101).                                 
02766                                                                   
02767  2899-EXIT.                                                       
02768      EXIT.                                                        
02769      EJECT                                                        
02770  3000-PRINT-CLAIM SECTION.                                        
02771      MOVE LOW-VALUES            TO PC-CONTROL-PRIMARY.            
02772      MOVE DTE-CLASIC-COMPANY-CD TO PC-COMPANY-CD.                 
02773                                                                   
02774      START ERPNDC   KEY NOT LESS THAN PC-COMPANY-CD.              
02775                                                                   
02776  3000-CK-STATUS-CODE.                                             
02777      IF PC-STATUS-CODE = '23'                                     
02778          PERFORM 7000-PRT-HDG    THRU 7000-EXIT                   
02779          PERFORM 7300-PRT-CL-HDG THRU 7300-EXIT                   
02780          MOVE '**** NO CLAIMS AT THIS TIME ****' TO P-DATA        
02781          MOVE '-'    TO X                                         
02782          PERFORM 7000-PRINT-LINE                                  
02783          PERFORM 3000-PRT-TOTS   THRU 3000-PRT-TOTS-EXIT          
02784          GO TO 3000-PRINT-CLAIM-EXIT.                             
02785                                                                   
02786      IF PC-STATUS-1 NOT = ZERO                                    
02787          MOVE PC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
02788          MOVE 'ERROR OCCURED START - ERPNDC'                      
02789                                  TO  WS-ABEND-MESSAGE             
02790          GO TO ABEND-PGM.                                         
02791                                                                   
02792      MOVE 99 TO LINE-CT.                                          
02793                                                                   
02794  3000-READ-LOOP.                                                  
02795      READ ERPNDC  NEXT RECORD.                                    
02796                                                                   
02797      IF PC-STATUS-1 = '1'                                         
02798          GO TO 3000-EOF.                                          
02799                                                                   
02800      IF PC-STATUS-1 NOT = ZERO                                    
02801          MOVE PC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
02802          MOVE 'ERROR OCCURED READ - ERPNDC'                       
02803                                  TO  WS-ABEND-MESSAGE             
02804          GO TO ABEND-PGM.                                         
02805                                                                   
02806      IF PC-CREDIT-ACCEPT-DT NOT = LOW-VALUES                      
02807            GO TO 3000-READ-LOOP.                                  
02808                                                                   
02809       MOVE PC-CREDIT-SELECT-DT    TO DC-BIN-DATE-1.               
02810       MOVE SPACES                 TO DC-OPTION-CODE.              
02811       PERFORM 6000-DATE-RTN  THRU 6000-EXIT.                      
02812       MOVE DC-GREG-DATE-1-YMD     TO SELDT.                       
02813                                                                   
02814      IF PC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
02815         IF LINE-CT = 99                                           
02816            MOVE '23' TO PC-STATUS-CODE                            
02817            GO TO 3000-CK-STATUS-CODE                              
02818        ELSE                                                       
02819            GO TO 3000-EOF.                                        
02820                                                                   
02821      IF PC-CLAIMS                                                 
02822          ADD 1 TO CLAIM-PAYMENTS                                  
02823          IF PC-FATAL-ERRORS                                       
02824              ADD 1 TO FATAL-CLM-PMT-CNT                           
02825          ELSE                                                     
02826              IF PC-UNFORCED-ERRORS                                
02827                  ADD 1 TO UNFORCE-CLM-PMT-CNT.                    
02828                                                                   
02829      IF PC-RESERVES                                               
02830          ADD 1 TO LOSS-RESERVES                                   
02831          IF PC-FATAL-ERRORS                                       
02832              ADD 1 TO FATAL-CLM-RES-CNT                           
02833          ELSE                                                     
02834              IF PC-UNFORCED-ERRORS                                
02835                  ADD 1 TO UNFORCE-CLM-RES-CNT.                    
02836                                                                   
02837 ********* CREDIT-SELECT DATE                                      
02838      IF DTE-PGM-OPT = '2' OR '3' OR '5'                           
02839         IF SELDT GREATER RUNDT                                    
02840            GO TO 3000-READ-LOOP.                                  
02841                                                                   
02842 ********* (ERRORS ONLY)                                           
02843      IF DTE-PGM-OPT = '1' OR '3' OR '4' OR '5' OR '6'             
02844           GO TO 3000-ERRORS.                                      
02845                                                                   
02846      GO TO 3000-FORMAT-CLAIM.                                     
02847                                                                   
02848  3000-ERRORS.                                                     
02849                                                                   
02850      IF (DTE-CLIENT = 'HER')                                      
02851               AND (DTE-PGM-OPT = '6')                             
02852         IF PC-ERROR-FLAGS NOT = SPACES                            
02853             GO TO 3000-FORMAT-CLAIM                               
02854         ELSE                                                      
02855             PERFORM 3000-ADD-CLAIMS THRU 3000-ADD-CL-EXIT         
02856             GO TO 3000-READ-LOOP.                                 
02857                                                                   
02858      IF DTE-PGM-OPT = '4' OR '5'                                  
02859          IF PC-UNFORCED-ERRORS OR PC-FATAL-ERRORS                 
02860              GO TO 3000-FORMAT-CLAIM                              
02861          ELSE                                                     
02862              PERFORM 3000-ADD-CLAIMS THRU 3000-ADD-CL-EXIT        
02863              GO TO 3000-READ-LOOP                                 
02864      ELSE                                                         
02865          IF DTE-PGM-OPT = '6'                                     
02866              IF PC-FORCE-ERRORS                                   
02867                  GO TO 3000-FORMAT-CLAIM                          
02868              ELSE                                                 
02869                  PERFORM 3000-ADD-CLAIMS THRU 3000-ADD-CL-EXIT    
02870                  GO TO 3000-READ-LOOP                             
02871          ELSE                                                     
02872              IF PC-ERROR-FLAGS NOT = SPACES                       
02873                  GO TO 3000-FORMAT-CLAIM                          
02874              ELSE                                                 
02875                  PERFORM 3000-ADD-CLAIMS THRU 3000-ADD-CL-EXIT    
02876                  GO TO 3000-READ-LOOP.                            
02877  EJECT                                                            
02878  3000-FORMAT-CLAIM.                                               
02879      IF DTE-CLIENT = 'TCL' OR 'DMD'                               
02880          MOVE PC-CARRIER     TO H4-CARR                           
02881          MOVE PC-GROUPING    TO H4-GRP                            
02882          MOVE PC-STATE       TO H4-ST                             
02883      ELSE                                                         
02884          MOVE PC-SV-CARRIER  TO H4-CARR                           
02885          MOVE PC-SV-GROUPING TO H4-GRP                            
02886          MOVE PC-SV-STATE    TO H4-ST
           END-IF
02887                                                                   
02888      MOVE PC-SV-CARRIER      TO CUR-CARR PB-CARRIER.              
02889      MOVE PC-SV-GROUPING     TO CUR-GRP  PB-GROUPING.             
02890      MOVE PC-SV-STATE        TO CUR-ST   PB-STATE.                
02891      MOVE PC-ACCOUNT         TO CUR-ACCT PB-ACCOUNT H4-ACCT.      
02892                                                                   
02893      IF CURR-KEY NOT = PREV-KEY                                   
02894          PERFORM 3000-PRT-TOTS   THRU 3000-PRT-TOTS-EXIT          
02895          PERFORM 9000-GET-ACCT   THRU 9400-EXIT                   
02896          PERFORM 7000-PRT-HDG    THRU 7000-EXIT                   
02897          PERFORM 7300-PRT-CL-HDG THRU 7300-EXIT.                  
02898                                                                   
02899      MOVE PC-SV-CARRIER     TO PRV-CARR.                          
02900      MOVE PC-SV-GROUPING    TO PRV-GRP.                           
02901      MOVE PC-SV-STATE       TO PRV-ST.                            
02902      MOVE PC-ACCOUNT        TO PRV-ACCT.                          
02903                                                                   
02904      MOVE LINE-CT TO TEST-CNT.                                    
02905                                                                   
02906      IF PC-FUTURE-RESERVE-AMT NOT = ZERO                          
02907          ADD 2 TO TEST-CNT.                                       
02908                                                                   
02909      IF PC-IBNR-RESERVE-AMT NOT = ZERO                            
02910          ADD 2 TO TEST-CNT.                                       
02911                                                                   
02912      IF PC-PTC-RESERVE-AMT NOT = ZERO                             
02913          ADD 2 TO TEST-CNT.                                       
02914                                                                   
02915      IF PC-MANUAL-RESERVE-AMT NOT = ZERO                          
02916          ADD 2 TO TEST-CNT.                                       
02917                                                                   
02918      IF TEST-CNT GREATER MAX-LINES                                
02919          PERFORM 7000-PRT-HDG    THRU 7000-EXIT                   
02920          PERFORM 7300-PRT-CL-HDG THRU 7300-EXIT.                  
02921                                                                   
02922      ADD 1 TO LINE-CT.                                            
02923      MOVE SPACES TO CLAIM-PRINT-LINE.                             
02924                                                                   
02925      MOVE PC-SV-CARRIER        TO CLPL-CARR.                      
02926      MOVE PC-SV-GROUPING       TO CLPL-GRP.                       
02927      MOVE PC-SV-STATE          TO CLPL-ST.                        
02928      MOVE PC-ACCOUNT           TO CLPL-ACCT.                      
02929      MOVE PC-CERT-NO           TO CLPL-CERT.                      
02930                                                                   
02931      IF PC-CERT-EFF-DT NOT = LOW-VALUES AND SPACES                
02932          MOVE PC-CERT-EFF-DT       TO DC-BIN-DATE-1               
02933          MOVE SPACES               TO DC-OPTION-CODE              
02934          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02935          MOVE DC-GREG-DATE-1-EDIT  TO CLPL-EFF-DT.                
02936                                                                   
02937      MOVE PC-CLAIM-NO          TO CLPL-CLAIMNO.                   
02938      MOVE PC-CC-INSURED-NAME   TO CLPL-INSURED.                   
02939                                                                   
02940      IF PC-LF-CLAIM                                               
02941          MOVE 'SP'             TO CLPL-CLM                        
02942          MOVE 'LF'             TO CLPL-TYP.                       
02943                                                                   
02944      IF PC-AH-CLAIM                                               
02945          MOVE 'SP'             TO CLPL-CLM                        
02946          MOVE 'AH'             TO CLPL-TYP.                       
02947                                                                   
02948      IF PC-OB-LF-CLAIM                                            
02949          MOVE 'OB'             TO CLPL-CLM                        
02950          MOVE 'LF'             TO CLPL-TYP.                       
02951                                                                   
02952      IF PC-OB-AH-CLAIM                                            
02953          MOVE 'OB'             TO CLPL-CLM                        
02954          MOVE 'AH'             TO CLPL-TYP.                       
02955                                                                   
02956      MOVE PC-CHECK-NO          TO CLPL-CHECKNO.                   
02957      MOVE PC-CLAIM-PAYMENT     TO CLPL-CLMAMT.                    
02958                                                                   
02959      IF PC-PAYMENT-DT NOT = LOW-VALUES AND SPACES                 
02960          MOVE PC-PAYMENT-DT        TO DC-BIN-DATE-1               
02961          MOVE SPACES               TO DC-OPTION-CODE              
02962          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02963          MOVE DC-GREG-DATE-1-EDIT  TO CLPL-PAID.                  
02964                                                                   
02965      IF PC-INCURRED-DT NOT = LOW-VALUES AND SPACES                
02966          MOVE PC-INCURRED-DT       TO DC-BIN-DATE-1               
02967          MOVE SPACES               TO DC-OPTION-CODE              
02968          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02969          MOVE DC-GREG-DATE-1-EDIT  TO CLPL-INCURRED.              
02970                                                                   
02971      MOVE PC-CC-ORIG-TERM      TO CLPL-TRM.                       
02972      MOVE PC-REMAINING-BENEFIT TO CLPL-BEN.                       
02973      MOVE PC-FORCE-CODE        TO CLPL-FC.                        
02974                                                                   
02975      IF PC-INPUT-DT NOT = LOW-VALUES AND SPACES                   
02976          MOVE PC-INPUT-DT          TO DC-BIN-DATE-1               
02977          MOVE SPACES               TO DC-OPTION-CODE              
02978          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
02979          MOVE DC-GREG-DATE-1-MDY   TO CLPL-INPUT.                 
02980                                                                   
02981      IF NOT PC-RESERVES                                           
02982         GO TO 3000-PRT-LINE1.                                     
02983                                                                   
02984      MOVE ZERO TO X.                                              
02985                                                                   
02986      IF PC-FUTURE-RESERVE-AMT NOT = ZERO                          
02987         MOVE PC-FUTURE-RESERVE-AMT TO CLPL-CLMAMT                 
02988         MOVE CLAIM-PRINT-LINE      TO P-DATA                      
02989         PERFORM 7000-PRINT-LINE                                   
02990         MOVE SPACES                TO CLAIM-PRINT-LINE            
02991         MOVE '64CDT'               TO CLPL-TYPE                   
02992         MOVE SECOND-CLAIM-LINE     TO P-DATA                      
02993         MOVE ' '                   TO X                           
02994         PERFORM 7000-PRINT-LINE                                   
02995         ADD 2 TO LINE-CT.                                         
02996                                                                   
02997      IF PC-IBNR-RESERVE-AMT NOT = ZERO                            
02998         MOVE PC-IBNR-RESERVE-AMT   TO CLPL-CLMAMT                 
02999         MOVE CLAIM-PRINT-LINE      TO P-DATA                      
03000         PERFORM 7000-PRINT-LINE                                   
03001         MOVE SPACES                TO CLAIM-PRINT-LINE            
03002         MOVE 'IBNR'                TO CLPL-TYPE                   
03003         MOVE SECOND-CLAIM-LINE     TO P-DATA                      
03004         MOVE ' '                   TO X                           
03005         PERFORM 7000-PRINT-LINE                                   
03006         ADD 2 TO LINE-CT.                                         
03007                                                                   
03008      IF PC-PTC-RESERVE-AMT NOT = ZERO                             
03009         MOVE PC-PTC-RESERVE-AMT    TO CLPL-CLMAMT                 
03010         MOVE CLAIM-PRINT-LINE      TO P-DATA                      
03011         PERFORM 7000-PRINT-LINE                                   
03012         MOVE SPACES TO CLAIM-PRINT-LINE                           
03013         MOVE 'PTC'                 TO CLPL-TYPE                   
03014         MOVE SECOND-CLAIM-LINE     TO P-DATA                      
03015         MOVE ' '                   TO X                           
03016         PERFORM 7000-PRINT-LINE                                   
03017         ADD 2 TO LINE-CT.                                         
03018                                                                   
03019      IF PC-MANUAL-RESERVE-AMT NOT = ZERO                          
03020         MOVE PC-MANUAL-RESERVE-AMT TO CLPL-CLMAMT                 
03021         MOVE CLAIM-PRINT-LINE      TO P-DATA                      
03022         PERFORM 7000-PRINT-LINE                                   
03023         MOVE SPACES                TO CLAIM-PRINT-LINE            
03024         MOVE 'MANUAL'              TO CLPL-TYPE                   
03025         MOVE SECOND-CLAIM-LINE     TO P-DATA                      
03026         MOVE ' '                   TO X                           
03027         PERFORM 7000-PRINT-LINE                                   
03028         ADD 2 TO LINE-CT.                                         
03029                                                                   
03030      PERFORM 3000-ADD-CLAIMS THRU 3000-ADD-CL-EXIT.               
03031                                                                   
03032      PERFORM 4800-FORMAT-ERRORS THRU 4899-EXIT.                   
03033                                                                   
03034      GO TO 3000-READ-LOOP.                                        
03035                                                                   
03036  3000-PRT-LINE1.                                                  
03037      MOVE CLAIM-PRINT-LINE TO P-DATA.                             
03038      MOVE '0'              TO X.                                  
03039      PERFORM 7000-PRINT-LINE.                                     
03040      ADD 1 TO LINE-CT.                                            
03041  EJECT                                                            
03042  3000-FORMAT-2ND-LINE.                                            
03043      MOVE SPACES TO CLAIM-PRINT-LINE.                             
03044                                                                   
03045      IF PC-PARTIAL-PAYMENT                                        
03046          MOVE 'PARTIAL'        TO CLPL-TYPE.                      
03047      IF PC-FINAL-PAYMENT                                          
03048          MOVE 'FINAL'          TO CLPL-TYPE.                      
03049      IF PC-LUMP-SUM-PAYMENT                                       
03050          MOVE 'LUMP SUM'       TO CLPL-TYPE.                      
03051      IF PC-ADDITIONAL-PAYMENT                                     
03052          MOVE 'ADDITNL'        TO CLPL-TYPE.                      
03053      IF PC-VOIDED-PAYMENT                                         
03054          MOVE 'VOIDED'         TO CLPL-TYPE.                      
03055                                                                   
03056      IF PC-CHARGEBLE-EXPENSE       OR                             
03057         PC-NON-CHARGEBLE-EXPENSE                                  
03058          MOVE '   EXP'         TO CLPL-TYPE.                      
03059                                                                   
03060      IF PC-PAID-THRU-DT NOT = LOW-VALUES AND SPACES               
03061         IF DTE-CLAIM-PAID-THRU-TO = ' '                           
03062            MOVE PC-PAID-THRU-DT      TO DC-BIN-DATE-1             
03063            MOVE SPACES               TO DC-OPTION-CODE            
03064            PERFORM 6000-DATE-RTN  THRU 6000-EXIT                  
03065            MOVE DC-GREG-DATE-1-EDIT  TO CLPL-PDTHRU               
03066         ELSE                                                      
03067            MOVE PC-PAID-THRU-DT      TO DC-BIN-DATE-1             
03068            MOVE '6'                  TO DC-OPTION-CODE            
03069            MOVE +1                   TO DC-ELAPSED-DAYS           
03070            MOVE +0                   TO DC-ELAPSED-MONTHS         
03071            PERFORM 6000-DATE-RTN  THRU 6000-EXIT                  
03072            MOVE DC-GREG-DATE-1-EDIT  TO CLPL-PDTHRU.              
03073                                                                   
03074      IF PC-REPORTED-DT NOT = LOW-VALUES AND SPACES                
03075          MOVE PC-REPORTED-DT       TO DC-BIN-DATE-1               
03076          MOVE SPACES               TO DC-OPTION-CODE              
03077          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
03078          MOVE DC-GREG-DATE-1-EDIT  TO CLPL-REPORTED.              
03079                                                                   
03080      MOVE PC-REMAINING-TERM     TO CLPL-REM.                      
03081      MOVE PC-CC-PRIOR-DEATH-AMT TO CLPL-PREV.                     
03082                                                                   
03083      IF PC-AH-CLAIM OR PC-OB-AH-CLAIM                             
03084          MOVE PC-CC-PRIOR-LUMP-PMT  TO CLPL-PREV.                 
03085                                                                   
03086      IF PC-FORCE-ERRORS                                           
03087          MOVE 'FORCE'           TO CLPL-STAT.                     
03088      IF PC-WARNING-ERRORS                                         
03089          MOVE 'WARN'            TO CLPL-STAT.                     
03090      IF PC-UNFORCED-ERRORS                                        
03091          MOVE 'UNFORC'          TO CLPL-STAT.                     
03092      IF PC-FATAL-ERRORS                                           
03093          MOVE 'FATAL'           TO CLPL-STAT.                     
03094                                                                   
03095      MOVE SECOND-CLAIM-LINE TO P-DATA.                            
03096      MOVE ' '               TO X.                                 
03097      PERFORM 7000-PRINT-LINE.                                     
03098      ADD 1 TO LINE-CT.                                            
03099                                                                   
03100      PERFORM 3000-ADD-CLAIMS THRU 3000-ADD-CL-EXIT.               
03101                                                                   
03102      PERFORM 4800-FORMAT-ERRORS THRU 4899-EXIT.                   
03103                                                                   
03104      GO TO 3000-READ-LOOP.                                        
03105  EJECT                                                            
03106  3000-ADD-CLAIMS.                                                 
03107      IF PC-UNFORCED-ERRORS OR                                     
03108         PC-FATAL-ERRORS                                           
03109            GO TO 3000-NON-PROCESSABLE.                            
03110                                                                   
03111  3000-PROCESSABLE.                                                
03112      ADD PC-CLAIM-PAYMENT TO TPA-CLM.                             
03113      ADD PC-FUTURE-RESERVE-AMT PC-IBNR-RESERVE-AMT                
03114          PC-PTC-RESERVE-AMT    PC-MANUAL-RESERVE-AMT              
03115                           TO TPA-LRES.                            
03116      GO TO 3000-ALL.                                              
03117                                                                   
03118  3000-NON-PROCESSABLE.                                            
03119      ADD PC-CLAIM-PAYMENT TO TNA-CLM.                             
03120      ADD PC-FUTURE-RESERVE-AMT PC-IBNR-RESERVE-AMT                
03121          PC-PTC-RESERVE-AMT    PC-MANUAL-RESERVE-AMT              
03122                           TO TNA-LRES.                            
03123                                                                   
03124  3000-ALL.                                                        
03125      IF PC-LF-CLAIM  OR  PC-OB-LF-CLAIM                           
03126         ADD PC-CLAIM-PAYMENT TO CL-TOT-LPAY.                      
03127                                                                   
03128      IF PC-AH-CLAIM  OR  PC-OB-AH-CLAIM                           
03129         ADD PC-CLAIM-PAYMENT TO CL-TOT-APAY.                      
03130                                                                   
03131      IF PC-CHARGEBLE-EXPENSE OR                                   
03132         PC-NON-CHARGEBLE-EXPENSE                                  
03133           ADD PC-CLAIM-PAYMENT    TO CL-TOT-EXP.                  
03134                                                                   
03135      ADD PC-FUTURE-RESERVE-AMT TO CL-TOT-64CDT.                   
03136      ADD PC-IBNR-RESERVE-AMT   TO CL-TOT-IBNR.                    
03137      ADD PC-PTC-RESERVE-AMT    TO CL-TOT-PTC.                     
03138                                                                   
03139  3000-ADD-CL-EXIT.                                                
03140      EXIT.                                                        
03141                                                                   
03142  3000-EOF.                                                        
03143       PERFORM 3000-PRT-TOTS THRU 3000-PRT-TOTS-EXIT.              
03144       GO TO 3000-PRINT-CLAIM-EXIT.                                
03145                                                                   
03146  3000-PRT-TOTS.                                                   
03147      IF LINE-CT = 99                                              
03148          GO TO 3000-PRT-TOTS-EXIT.                                
03149                                                                   
03150      MOVE SPACES TO CL-TOTAL-PRINT-LINE.                          
03151      MOVE CL-TOT-HD1 TO P-DATA.                                   
03152      MOVE '-'        TO X.                                        
03153      PERFORM 7000-PRINT-LINE.                                     
03154      MOVE CL-TOT-HD2 TO P-DATA.                                   
03155      MOVE ' '        TO X.                                        
03156      PERFORM 7000-PRINT-LINE.                                     
03157                                                                   
03158      MOVE CL-TOT-LPAY            TO CTPL-LF-PAY.                  
03159      MOVE CL-TOT-APAY            TO CTPL-AH-PAY.                  
03160      MOVE CL-TOT-64CDT           TO CTPL-64CDT.                   
03161      MOVE CL-TOT-PTC             TO CTPL-PTC.                     
03162      MOVE CL-TOT-IBNR            TO CTPL-IBNR.                    
03163      MOVE CL-TOT-EXP             TO CTPL-EXP.                     
03164      MOVE CL-TOTAL-PRINT-LINE    TO P-DATA.                       
03165      MOVE '0'                    TO X.                            
03166      PERFORM 7000-PRINT-LINE.                                     
03167                                                                   
03168      INITIALIZE CL-TOTALS.                                        
03169                                                                   
03170  3000-PRT-TOTS-EXIT.                                              
03171      EXIT.                                                        
03172                                                                   
03173  3000-PRINT-CLAIM-EXIT.                                           
03174      EXIT.                                                        
03175      EJECT                                                        
03176  4000-PRINT-CHANGE SECTION.                                       
03177      MOVE LOW-VALUES            TO CC-CONTROL-PRIMARY.            
03178      MOVE DTE-CLASIC-COMPANY-CD TO CC-COMPANY-CD.                 
03179                                                                   
03180      START ERCRTC   KEY NOT LESS THAN CC-COMPANY-CD.              
03181                                                                   
03182      IF CC-STATUS-CODE = '23'                                     
03183          PERFORM 7000-PRT-HDG    THRU 7000-EXIT                   
03184          PERFORM 7400-PRT-CC-HDG THRU 7400-EXIT                   
03185          MOVE '**** NO CHANGES AT THIS TIME ****' TO P-DATA       
03186          MOVE '-' TO X                                            
03187          PERFORM 7000-PRINT-LINE                                  
03188          GO TO 4000-PRINT-CHANGE-EXIT.                            
03189                                                                   
03190      IF CC-STATUS-1 NOT = ZERO                                    
03191          MOVE CC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
03192          MOVE 'ERROR OCCURED START - CHANGE'                      
03193                                  TO  WS-ABEND-MESSAGE             
03194          GO TO ABEND-PGM.                                         
03195                                                                   
03196      MOVE 99 TO LINE-CT.                                          
03197                                                                   
03198  4000-READ-LOOP.                                                  
03199      READ ERCRTC  NEXT RECORD.                                    
03200                                                                   
03201      IF CC-STATUS-1 = '1'                                         
03202          GO TO 4000-PRINT-CHANGE-EXIT.                            
03203                                                                   
03204      IF CC-STATUS-1 NOT = ZERO                                    
03205          MOVE CC-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
03206          MOVE 'ERROR OCCURED READ - CHANGE'                       
03207                                  TO  WS-ABEND-MESSAGE             
03208          GO TO ABEND-PGM.                                         
03209                                                                   
03210      IF CC-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
03211            GO TO 4000-PRINT-CHANGE-EXIT.                          
03212  EJECT                                                            
03213  4000-FORMAT-CHANGE.                                              
03214      MOVE SPACES TO CHANGE-PRINT-LINE.                            
03215      ADD 3 TO LINE-CT.                                            
03216      IF LINE-CT GREATER THAN MAX-LINES                            
03217          PERFORM 7000-PRT-HDG    THRU 7000-EXIT                   
03218          PERFORM 7400-PRT-CC-HDG THRU 7400-EXIT.                  
03219                                                                   
03220      MOVE CC-CARRIER           TO CHPL-CARR.                      
03221      MOVE CC-GROUPING          TO CHPL-GRP.                       
03222      MOVE CC-STATE             TO CHPL-ST.                        
03223      MOVE CC-ACCOUNT           TO CHPL-ACCT.                      
03224      MOVE CC-CERT-NO           TO CHPL-CERT.                      
03225                                                                   
03226      IF CC-CERT-EFF-DT NOT = LOW-VALUES AND SPACES                
03227          MOVE CC-CERT-EFF-DT       TO DC-BIN-DATE-1               
03228          MOVE SPACES               TO DC-OPTION-CODE              
03229          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
03230          MOVE DC-GREG-DATE-1-EDIT  TO CHPL-EFF-DT.                
03231                                                                   
03232      MOVE CC-INSURED-LAST-NAME TO CHPL-LNAME.                     
03233      MOVE CC-AGE               TO CHPL-AGE.                       
03234      MOVE CC-JT-LAST-NAME      TO CHPL-JNAME.                     
03235      MOVE CC-LOAN-APR          TO CHPL-APR.                       
03236      MOVE CC-SOC-SEC-NO        TO CHPL-SOC-SEC.                   
03237      MOVE CC-AH-ORIG-TERM      TO CHPL-ATRM.                      
03238      MOVE CC-LF-ORIG-TERM      TO CHPL-LTRM.                      
03239      MOVE CC-IND-GRP-TYPE      TO CHPL-IG.                        
03240      MOVE CC-PREMIUM-TYPE      TO CHPL-PTYPE.                     
03241      MOVE CC-LAST-MAINT-BY     TO CHPL-BY.                        
03242                                                                   
03243      IF CC-LAST-MAINT-DT NOT = LOW-VALUES AND SPACES              
03244          MOVE CC-LAST-MAINT-DT     TO DC-BIN-DATE-1               
03245          MOVE SPACES               TO DC-OPTION-CODE              
03246          PERFORM 6000-DATE-RTN  THRU 6000-EXIT                    
03247          MOVE DC-GREG-DATE-1-MDY   TO CHPL-ENTRY.                 
03248                                                                   
03249      MOVE CHANGE-PRINT-LINE TO P-DATA.                            
03250      MOVE '0'               TO X.                                 
03251      PERFORM 7000-PRINT-LINE.                                     
03252      EJECT                                                        
03253  4000-FORMAT-2ND-LINE.                                            
03254      MOVE SPACES TO CHANGE-PRINT-LINE.                            
03255      MOVE CC-INSURED-1ST-INIT  TO CHPL-FIRST.                     
03256      MOVE CC-INSURED-SEX       TO CHPL-SEX.                       
03257      MOVE CC-MEMBER-NO         TO CHPL-MEMBER.                    
03258      MOVE CC-POLICY-FORM-NO    TO CHPL-FORM.                      
03259                                                                   
03260      IF CC-LIVES NUMERIC                                          
03261         MOVE CC-LIVES          TO CHPL-LIVES                      
03262       ELSE                                                        
03263         MOVE ZEROS             TO CHPL-LIVES.                     
03264                                                                   
03265      MOVE CC-LOAN-NUMBER       TO CHPL-LOAN.                      
03266      MOVE CC-LOAN-BALANCE      TO CHPL-BAL.                       
03267      MOVE CC-LOAN-OFFICER      TO CHPL-OFF.                       
03268                                                                   
03269      MOVE SECOND-CHANGE-PRINT-LINE TO P-DATA.                     
03270      MOVE ' '               TO X.                                 
03271      PERFORM 7000-PRINT-LINE.                                     
03272                                                                   
03273      GO TO 4000-READ-LOOP.                                        
03274                                                                   
03275  4000-PRINT-CHANGE-EXIT.                                          
03276      EXIT.                                                        
03277      EJECT                                                        
03278  4500-FORMAT-ERRORS.                                              
03279      IF PB-NO-OF-ERRORS = ZERO                                    
03280        IF PB-FATAL-ERRORS                                         
03281           GO TO 4600-UNEDITED-TRANS                               
03282         ELSE                                                      
03283           GO TO 4599-EXIT.                                        
03284                                                                   
03285      MOVE 1                   TO SUB.                             
03286                                                                   
03287  4510-ERR-LOOP.                                                   
03288      IF PB-NO-OF-ERRORS LESS SUB                                  
03289         GO TO 4599-EXIT.                                          
03290                                                                   
03291      MOVE PB-COMMON-ERROR (SUB)  TO WS-ERR-CODE.                  
03292      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03293      MOVE SPACES              TO ERROR-PRINT-LINE.                
03294      MOVE WS-ERR-CODE-X       TO EPL-MESS.                        
03295      MOVE EM-ERROR-SEVERITY   TO EPL-STAT.                        
03296      MOVE EM-ERROR-TEXT       TO EPL-MSG.                         
03297      IF EPL-COV = 'LLLLLL'                                        
03298         MOVE LIFE-OVERRIDE-L6 TO EPL-COV.                         
03299      IF EPL-COV = 'AAAAAA'                                        
03300         MOVE AH-OVERRIDE-L6   TO EPL-COV.                         
03301                                                                   
03302      MOVE ERROR-PRINT-LINE    TO P-DATA.                          
03303      MOVE ' '                 TO X.                               
03304      ADD 1 TO LINE-CT.                                            
03305      PERFORM  7000-PRINT-LINE.                                    
03306                                                                   
03307      ADD 1 TO SUB.                                                
03308      IF SUB LESS 11                                               
03309         GO TO 4510-ERR-LOOP.                                      
03310                                                                   
03311  4599-EXIT.                                                       
03312       EXIT.                                                       
03313                                                                   
03314  4600-UNEDITED-TRANS.                                             
03315      MOVE ER-2695           TO WS-ERR-CODE-X.                     
03316      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03317      MOVE SPACES TO ERROR-PRINT-LINE.                             
03318      MOVE EM-ERROR-SEVERITY TO EPL-STAT.                          
03319      MOVE EM-ERROR-TEXT     TO EPL-MSG.                           
03320      MOVE ERROR-PRINT-LINE  TO P-DATA.                            
03321      MOVE ' '               TO X.                                 
03322      ADD 1 TO LINE-CT.                                            
03323      PERFORM 7000-PRINT-LINE.                                     
03324      GO TO 4599-EXIT.                                             
03325       EJECT                                                       
091604 4700-FORMAT-BILL-NOTES.

091604     MOVE PB-COMPANY-CD          TO CN-COMPANY-CD
091604     MOVE PB-CARRIER             TO CN-CARRIER
091604     MOVE PB-GROUPING            TO CN-GROUPING
091604     MOVE PB-STATE               TO CN-STATE
091604     MOVE PB-ACCOUNT             TO CN-ACCOUNT
091604     MOVE PB-CERT-EFF-DT         TO CN-CERT-EFF-DT
091604     MOVE PB-CERT-NO             TO CN-CERT-NO

091604     READ ERNOTE
091604                                                                  
091604     IF ERNOTE-FILE-STATUS NOT = '00' AND '10' AND '23'
091604        MOVE 'ERROR OCCURED READ  - ERNOTE'                      
091604                                 TO WS-ABEND-MESSAGE            
091604        MOVE ERNOTE-FILE-STATUS  TO WS-ABEND-FILE-STATUS        
091604        PERFORM ABEND-PGM
           END-IF
091604                                                                  
091604     IF ERNOTE-FILE-STATUS = '00'
091604        IF CN-BILLING-START-LINE-NO NUMERIC                          
091604           MOVE CN-BILLING-START-LINE-NO TO STRT-LN
091604        ELSE                                                         
091604           MOVE ZERO             TO STRT-LN
              END-IF
091604        IF CN-BILLING-END-LINE-NO NUMERIC                            
091604           MOVE CN-BILLING-END-LINE-NO TO END-LN                  
091604        ELSE                                                         
091604           MOVE ZERO             TO END-LN
091604        END-IF
              IF ((STRT-LN = ZEROS)
                 AND (END-LN = ZEROS))
                          OR
                 (STRT-LN > END-LN)
                 CONTINUE
              ELSE
                 PERFORM VARYING STRT-LN FROM STRT-LN BY +1 UNTIL
                    (STRT-LN > END-LN)
                    MOVE SPACES        TO ERROR-PRINT-LINE
                    MOVE CN-LINE (STRT-LN) TO EPL-MESSAGE
                    MOVE ERROR-PRINT-LINE TO P-DATA
                    MOVE ' '           TO X
                    ADD 1 TO LINE-CT
                    PERFORM 7000-PRINT-LINE
                 END-PERFORM
              END-IF
           END-IF
091604        

           .
091604 4700-EXIT.
091604     EXIT.

03326  4800-FORMAT-ERRORS.                                              
03327      IF PC-ERROR-FLAGS = SPACES                                   
03328         GO TO 4899-EXIT.                                          
03329                                                                   
03330      MOVE ER-2800                TO WS-ERR-CODE-X.                
03331      MOVE 1                      TO SUB.                          
03332                                                                   
03333  4810-STD-LOOP.                                                   
03334      IF PC-ERR-FLAG (SUB) = SPACES                                
03335         GO TO 4810-ADD-SUB.                                       
03336                                                                   
03337      MOVE SUB                 TO WS-ERROR-SUB.                    
03338      PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
03339                                                                   
03340      MOVE SPACES              TO ERROR-PRINT-LINE.                
03341      MOVE EM-ERROR-SEVERITY   TO EPL-STAT.                        
03342      MOVE EM-ERROR-TEXT       TO EPL-MSG.                         
03343      IF EPL-COV = 'LLLLLL'                                        
03344         MOVE LIFE-OVERRIDE-L6 TO EPL-COV.                         
03345      IF EPL-COV = 'AAAAAA'                                        
03346         MOVE AH-OVERRIDE-L6   TO EPL-COV.                         
03347                                                                   
03348      MOVE ERROR-PRINT-LINE    TO P-DATA.                          
03349      MOVE ' '                 TO X.                               
03350      ADD 1 TO LINE-CT.                                            
03351      PERFORM  7000-PRINT-LINE.                                    
03352                                                                   
03353  4810-ADD-SUB.                                                    
03354      ADD 1 TO SUB.                                                
03355      IF SUB LESS 100                                              
03356         GO TO 4810-STD-LOOP.                                      
03357                                                                   
03358  4899-EXIT.                                                       
03359       EXIT.                                                       
03360  EJECT                                                            
03361  4900-PRINT-USER-TOTS.                                            
03362      PERFORM 7000-PRT-HDG       THRU 7000-EXIT.                   
03363      PERFORM 7500-PRT-SUMRY-HDG THRU 7500-EXIT.                   
03364      MOVE SPACES TO ISSUE-PRINT-LINE.                             
03365      SET USTINDX TO 1.                                            
03366      GO TO 4900-CK-LST-SPACES.                                    
03367                                                                   
03368  4900-SET-USTINDX-UP.                                             
03369      SET USTINDX UP BY 1.                                         
03370      IF USTINDX GREATER 100                                       
03371          GO TO 4900-PRT-USR-TOT-LN.                               
03372                                                                   
03373  4900-CK-LST-SPACES.                                              
03374      IF UST-USER (USTINDX) = SPACES                               
03375          GO TO 4900-PRT-USR-TOT-LN.                               
03376                                                                   
03377  4900-ADD.                                                        
03378      ADD 1 TO LINE-CT.                                            
03379      IF LINE-CT GREATER MAX-LINES                                 
03380          PERFORM 7000-PRT-HDG       THRU 7000-EXIT                
03381          PERFORM 7500-PRT-SUMRY-HDG THRU 7500-EXIT.               
03382                                                                   
03383      MOVE UST-USER (USTINDX)       TO USL-USER.                   
03384      MOVE UST-BAT-INP (USTINDX)    TO USL-BAT-INP.                
03385      MOVE UST-BAT-MNTC (USTINDX)   TO USL-BAT-MNTC.               
03386      MOVE UST-DET-INP (USTINDX)    TO USL-DET-INP.                
03387      MOVE UST-DET-MNTC (USTINDX)   TO USL-DET-MNTC.               
03388      MOVE UST-UPFRC-PROC (USTINDX) TO USL-UPFRC-PROC
102706     MOVE UST-SIG-SW    (USTINDX)  TO USL-SIG-SW
03389      MOVE USER-SUM-LINE TO P-DATA.                                
03390      MOVE ' ' TO X.                                               
03391      PERFORM 7000-PRINT-LINE.                                     
03392                                                                   
03393  4900-ADD-EXIT.                                                   
03394      EXIT.                                                        
03395                                                                   
03396  4900-CONTD.                                                      
03397      GO TO 4900-SET-USTINDX-UP.                                   
03398                                                                   
03399  4900-PRT-USR-TOT-LN.                                             
03400      MOVE SPACES TO P-DATA.                                       
03401      MOVE ' '    TO X.                                            
03402      PERFORM 7000-PRINT-LINE.                                     
03403      SET USTINDX TO 101.                                          
03404      MOVE 'TOT.' TO UST-USER (USTINDX).                           
03405      PERFORM 4900-ADD THRU 4900-ADD-EXIT.                         
03406                                                                   
03407  4900-EXIT.                                                       
03408      EXIT.                                                        
03409       EJECT                                                       
03410  5000-PRINT-SUMMARY SECTION.                                      
03411      MOVE SPACES TO SUMMARY-PRINT-LINE HDG4.                      
03412      MOVE SPACES TO ER-ONLY.                                      
03413      PERFORM 7000-PRT-HDG THRU 7000-EXIT.                         
03414                                                                   
03415      MOVE SUMMARY-LINE-1 TO P-DATA.                               
03416      MOVE '-'            TO X.                                    
03417      PERFORM 7000-PRINT-LINE.                                     
03418                                                                   
03419      MOVE EAYR           TO EDYR.                                 
03420      MOVE EAMO           TO EDMO.                                 
03421      MOVE EADA           TO EDDA.                                 
03422                                                                   
03423      MOVE EDIT-DATE      TO SL2-EARLY.                            
03424      MOVE SUMMARY-LINE-2 TO P-DATA.                               
03425      MOVE ' '            TO X.                                    
03426      PERFORM 7000-PRINT-LINE.                                     
03427                                                                   
03428      MOVE LAYR           TO EDYR.                                 
03429      MOVE LAMO           TO EDMO.                                 
03430      MOVE LADA           TO EDDA.                                 
03431                                                                   
03432      MOVE EDIT-DATE      TO SL3-LATE.                             
03433      MOVE SUMMARY-LINE-3 TO P-DATA.                               
03434      MOVE ' '            TO X.                                    
03435      PERFORM 7000-PRINT-LINE.                                     
03436                                                                   
03437      MOVE OVER30         TO SL4-30.                               
03438      MOVE SUMMARY-LINE-4 TO P-DATA.                               
03439      MOVE '0'            TO X.                                    
03440      PERFORM 7000-PRINT-LINE.                                     
03441                                                                   
03442      MOVE OVER60         TO SL5-60.                               
03443      MOVE SUMMARY-LINE-5 TO P-DATA.                               
03444      MOVE ' '            TO X.                                    
03445      PERFORM 7000-PRINT-LINE.                                     
03446                                                                   
03447      MOVE OVER90         TO SL6-90.                               
03448      MOVE SUMMARY-LINE-6 TO P-DATA.                               
03449      MOVE ' '            TO X.                                    
03450      PERFORM 7000-PRINT-LINE.                                     
03451                                                                   
03452      MOVE ISSUE-CNT      TO SL7-ISSUES.                           
03453      MOVE SUMMARY-LINE-7 TO P-DATA.                               
03454      MOVE '0'            TO X.                                    
03455      PERFORM 7000-PRINT-LINE.                                     
03456                                                                   
03457      MOVE CANCEL-CNT     TO SL8-CANCELS.                          
03458      MOVE SUMMARY-LINE-8 TO P-DATA.                               
03459      MOVE ' '            TO X.                                    
03460      PERFORM 7000-PRINT-LINE.                                     
03461                                                                   
03462      ADD CANCEL-CNT ISSUE-CNT GIVING SL9-BOTH.                    
03463      MOVE SUMMARY-LINE-9 TO P-DATA.                               
03464      MOVE ' '            TO X.                                    
03465      PERFORM 7000-PRINT-LINE.                                     
03466                                                                   
03467      MOVE BATCH-CNT       TO SL10-BATCHES.                        
03468      MOVE SUMMARY-LINE-10 TO P-DATA.                              
03469      MOVE '0'             TO X.                                   
03470      PERFORM 7000-PRINT-LINE.                                     
03471                                                                   
03472      MOVE BATCHES-OUT-OF-BALANCE TO SL11-OUTOFBAL.                
03473      MOVE SUMMARY-LINE-11 TO P-DATA.                              
03474      MOVE ' '             TO X.                                   
03475      PERFORM 7000-PRINT-LINE.                                     
03476                                                                   
03477      MOVE HIGHEST-BATCH TO SL12-HIGHBATCH.                        
03478      MOVE SUMMARY-LINE-12 TO P-DATA.                              
03479      MOVE ' '             TO X.                                   
03480      PERFORM 7000-PRINT-LINE.                                     
03481                                                                   
03482      MOVE SPACES          TO P-DATA.                              
03483      MOVE ' '             TO X.                                   
03484      PERFORM 7000-PRINT-LINE.                                     
03485                                                                   
03486      MOVE SUMMARY-HDG    TO P-DATA.                               
03487      MOVE '0'            TO X.                                    
03488      PERFORM 7000-PRINT-LINE.                                     
03489                                                                   
03490      MOVE 'TOTAL PROCESSABLE'  TO SMPL-DESC.                      
03491      MOVE TPA-LPREM            TO SMPL-LF-PREM.                   
03492      MOVE TPA-LREF             TO SMPL-LF-REF.                    
03493      MOVE TPA-APREM            TO SMPL-AH-PREM.                   
03494      MOVE TPA-AREF             TO SMPL-AH-REF.                    
03495      MOVE TPA-CLM              TO SMPL-CLM.                       
03496      MOVE TPA-LRES             TO SMPL-LRES.                      
03497      MOVE SUMMARY-PRINT-LINE   TO P-DATA.                         
03498      MOVE ' '                  TO X.                              
03499      PERFORM 7000-PRINT-LINE.                                     
03500      MOVE SPACES TO SUMMARY-PRINT-LINE.                           
03501                                                                   
03502      MOVE 'TOTAL NON-PROCESSABLE'  TO SMPL-DESC.                  
03503      MOVE TNA-LPREM                TO SMPL-LF-PREM.               
03504      MOVE TNA-LREF                 TO SMPL-LF-REF.                
03505      MOVE TNA-APREM                TO SMPL-AH-PREM.               
03506      MOVE TNA-AREF                 TO SMPL-AH-REF.                
03507      MOVE TNA-CLM                  TO SMPL-CLM.                   
03508      MOVE TNA-LRES                 TO SMPL-LRES.                  
03509      MOVE SUMMARY-PRINT-LINE       TO P-DATA.                     
03510      MOVE ' '                      TO X.                          
03511      PERFORM 7000-PRINT-LINE.                                     
03512      MOVE SPACES TO SUMMARY-PRINT-LINE.                           
03513                                                                   
03514      MOVE 'FILE TOTALS'         TO SMPL-DESC.                     
03515      ADD TPA-LPREM  TNA-LPREM GIVING FTA-LPREM.                   
03516      ADD TPA-LREF   TNA-LREF  GIVING FTA-LREF.                    
03517      ADD TPA-APREM  TNA-APREM GIVING FTA-APREM.                   
03518      ADD TPA-AREF   TNA-AREF  GIVING FTA-AREF.                    
03519      ADD TPA-CLM    TNA-CLM   GIVING FTA-CLM.                     
03520      ADD TPA-LRES   TNA-LRES  GIVING FTA-LRES.                    
03521      MOVE FTA-LPREM             TO SMPL-LF-PREM.                  
03522      MOVE FTA-LREF              TO SMPL-LF-REF.                   
03523      MOVE FTA-APREM             TO SMPL-AH-PREM.                  
03524      MOVE FTA-AREF              TO SMPL-AH-REF.                   
03525      MOVE FTA-CLM               TO SMPL-CLM.                      
03526      MOVE FTA-LRES              TO SMPL-LRES.                     
03527      MOVE SUMMARY-PRINT-LINE    TO P-DATA.                        
03528      MOVE ' '                   TO X.                             
03529      PERFORM 7000-PRINT-LINE.                                     
03530      MOVE SPACES               TO SUMMARY-PRINT-LINE.             
03531                                                                   
03532      MOVE SUMMARY-HDG-2        TO P-DATA.                         
03533      MOVE '0'                  TO X.                              
03534      PERFORM 7000-PRINT-LINE.                                     
03535                                                                   
03536      MOVE 'ISSUES'             TO SMPL2-DESC.                     
03537      MOVE ISSUE-CNT            TO SMPL2-TOTAL.                    
03538      MOVE FATAL-ISSUE-CNT      TO SMPL2-FATAL.                    
03539      MOVE UNFORCE-ISSUE-CNT    TO SMPL2-FORCIBLE.                 
03540      MOVE HOLD-ISSUE-CNT       TO SMPL2-HOLD.                     
03541      MOVE RETURNED-ISSUE-CNT   TO SMPL2-RETURNED.                 
03542      MOVE AVAILABLE-ISSUE-CNT  TO SMPL2-AVAILABLE.                
03543      MOVE SUMMARY-PRINT-LINE   TO P-DATA.                         
03544      MOVE ' '                  TO X.                              
03545      PERFORM 7000-PRINT-LINE.                                     
03546      MOVE SPACES TO SUMMARY-PRINT-LINE.                           
03547                                                                   
03548      MOVE 'CANCELS'            TO SMPL2-DESC.                     
03549      MOVE CANCEL-CNT           TO SMPL2-TOTAL.                    
03550      MOVE FATAL-CANCEL-CNT     TO SMPL2-FATAL.                    
03551      MOVE UNFORCE-CANCEL-CNT   TO SMPL2-FORCIBLE.                 
03552      MOVE HOLD-CANCEL-CNT      TO SMPL2-HOLD.                     
03553      MOVE RETURNED-CANCEL-CNT  TO SMPL2-RETURNED.                 
03554      MOVE AVAILABLE-CANCEL-CNT TO SMPL2-AVAILABLE.                
03555      MOVE SUMMARY-PRINT-LINE   TO P-DATA.                         
03556      MOVE ' '                  TO X.                              
03557      PERFORM 7000-PRINT-LINE.                                     
03558      MOVE SPACES TO SUMMARY-PRINT-LINE.                           
03559                                                                   
03560      MOVE 'TOTALS '            TO SMPL2-DESC.                     
03561                                                                   
03562      ADD ISSUE-CNT CANCEL-CNT                                     
03563                      GIVING SMPL2-TOTAL.                          
03564                                                                   
03565      ADD FATAL-ISSUE-CNT FATAL-CANCEL-CNT                         
03566                      GIVING SMPL2-FATAL.                          
03567                                                                   
03568      ADD UNFORCE-ISSUE-CNT UNFORCE-CANCEL-CNT                     
03569                      GIVING SMPL2-FORCIBLE.                       
03570                                                                   
03571      ADD HOLD-ISSUE-CNT HOLD-CANCEL-CNT                           
03572                      GIVING SMPL2-HOLD.                           
03573                                                                   
03574      ADD RETURNED-ISSUE-CNT RETURNED-CANCEL-CNT                   
03575                      GIVING SMPL2-RETURNED.                       
03576                                                                   
03577      ADD AVAILABLE-ISSUE-CNT AVAILABLE-CANCEL-CNT                 
03578                      GIVING SMPL2-AVAILABLE.                      
03579                                                                   
03580      MOVE SUMMARY-PRINT-LINE TO P-DATA.                           
03581      MOVE ' '                TO X.                                
03582      PERFORM 7000-PRINT-LINE.                                     
03583                                                                   
03584      MOVE SUMMARY-HDG-3        TO P-DATA.                         
03585      MOVE '0'                  TO X.                              
03586      PERFORM 7000-PRINT-LINE.                                     
03587                                                                   
03588      MOVE 'CLAIM PAYMENTS'     TO SMPL2-DESC.                     
03589      MOVE CLAIM-PAYMENTS       TO SMPL2-TOTAL.                    
03590      MOVE FATAL-CLM-PMT-CNT    TO SMPL2-FATAL.                    
03591      MOVE UNFORCE-CLM-PMT-CNT  TO SMPL2-FORCIBLE.                 
03592      MOVE ZEROS                TO SMPL2-HOLD.                     
03593      MOVE ZEROS                TO SMPL2-RETURNED.                 
03594                                                                   
03595      COMPUTE SMPL2-AVAILABLE = CLAIM-PAYMENTS - FATAL-CLM-PMT-CNT 
03596                              - UNFORCE-CLM-PMT-CNT.               
03597                                                                   
03598      MOVE SUMMARY-PRINT-LINE   TO P-DATA.                         
03599      MOVE ' '                  TO X.                              
03600      PERFORM 7000-PRINT-LINE.                                     
03601      MOVE SPACES TO SUMMARY-PRINT-LINE.                           
03602                                                                   
03603      MOVE 'CLAIM RESERVES'     TO SMPL2-DESC.                     
03604      MOVE LOSS-RESERVES        TO SMPL2-TOTAL.                    
03605      MOVE FATAL-CLM-RES-CNT    TO SMPL2-FATAL.                    
03606      MOVE UNFORCE-CLM-RES-CNT  TO SMPL2-FORCIBLE.                 
03607      MOVE ZEROS                TO SMPL2-HOLD.                     
03608      MOVE ZEROS                TO SMPL2-RETURNED.                 
03609                                                                   
03610      COMPUTE SMPL2-AVAILABLE = LOSS-RESERVES - FATAL-CLM-RES-CNT  
03611                              - UNFORCE-CLM-RES-CNT.               
03612                                                                   
03613      MOVE SUMMARY-PRINT-LINE   TO P-DATA.                         
03614      MOVE ' '                  TO X.                              
03615      PERFORM 7000-PRINT-LINE.                                     
03616      MOVE SPACES TO SUMMARY-PRINT-LINE.                           
03617                                                                   
03618      MOVE 'TOTALS '            TO SMPL2-DESC.                     
03619                                                                   
03620      ADD CLAIM-PAYMENTS  LOSS-RESERVES                            
03621                      GIVING SMPL2-TOTAL.                          
03622                                                                   
03623      ADD FATAL-CLM-PMT-CNT  FATAL-CLM-RES-CNT                     
03624                      GIVING SMPL2-FATAL.                          
03625                                                                   
03626      ADD UNFORCE-CLM-PMT-CNT  UNFORCE-CLM-RES-CNT                 
03627                      GIVING SMPL2-FORCIBLE.                       
03628                                                                   
03629      MOVE ZEROS                TO SMPL2-HOLD.                     
03630      MOVE ZEROS                TO SMPL2-RETURNED.                 
03631                                                                   
03632      COMPUTE SMPL2-AVAILABLE = (CLAIM-PAYMENTS                    
03633          - FATAL-CLM-PMT-CNT - UNFORCE-CLM-PMT-CNT)               
03634          + (LOSS-RESERVES - FATAL-CLM-RES-CNT                     
03635          - UNFORCE-CLM-RES-CNT).                                  
03636                                                                   
03637      MOVE SUMMARY-PRINT-LINE TO P-DATA.                           
03638      MOVE ' '                TO X.                                
03639      PERFORM 7000-PRINT-LINE.                                     
03640                                                                   
03641  5000-EXIT.                                                       
03642      EXIT.                                                        
03643      EJECT                                                        
03644  6000-DATE-RTN SECTION.                                           
03645      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  
03646                                                                   
03647      IF DC-ERROR-CODE NOT = SPACE                                 
03648         MOVE ZEROS TO DC-CONVERSION-DATES.                        
03649                                                                   
03650  6000-EXIT.                                                       
03651      EXIT.                                                        
03652                                                                   
03653  7000-PRT-HDG SECTION.                                            
03654      ADD 1               TO PGE-CT.                               
03655      MOVE PGE-CT         TO H3-PAGE.                              
03656      MOVE HDG1           TO P-DATA.                               
03657      MOVE '1'            TO X.                                    
03658      PERFORM 7000-PRINT-LINE.                                     
03659      MOVE HDG2           TO P-DATA.                               
03660      MOVE ' '            TO X.                                    
03661      PERFORM 7000-PRINT-LINE.                                     
03662      MOVE HDG3           TO P-DATA.                               
03663      MOVE ' '            TO X.                                    
03664      PERFORM 7000-PRINT-LINE.                                     
03665      MOVE 3              TO LINE-CT.                              
03666                                                                   
03667  7000-EXIT.                                                       
03668      EXIT.                                                        
03669                                                                   
03670  7000-PRINT-LINE   SECTION.                                       
03671      IF EPL-COV = 'LLLLLL'                                        
03672          MOVE LIFE-OVERRIDE-L6 TO EPL-COV.                        
03673      IF EPL-COV = 'AAAAAA'                                        
03674          MOVE AH-OVERRIDE-L6   TO EPL-COV.                        
03675                                                                   
03676                      COPY ELCPRT2X.                               
03677                                                                   
03678  7000-PRINT-LINE-EXIT.                                            
03679       EXIT.                                                       
03680  EJECT                                                            
03681  7100-PRT-IS-HDG SECTION.                                         
03682      MOVE ISSUE-HD1      TO P-DATA.                               
03683      MOVE '0'            TO X.                                    
03684      PERFORM 7000-PRINT-LINE.                                     
03685      MOVE ISSUE-HD2      TO P-DATA.                               
03686      MOVE '0'            TO X.                                    
03687      PERFORM 7000-PRINT-LINE.                                     
03688      MOVE ISSUE-HD3      TO P-DATA.                               
03689      MOVE ' '            TO X.                                    
03690      PERFORM 7000-PRINT-LINE.                                     
03691      MOVE ISSUE-HD4      TO P-DATA.                               
03692      MOVE ' '            TO X.                                    
03693      PERFORM 7000-PRINT-LINE.                                     
03694      MOVE ISSUE-HD5      TO P-DATA.                               
03695      MOVE ' '            TO X.                                    
03696      PERFORM 7000-PRINT-LINE.                                     
03697      ADD 7               TO LINE-CT.                              
03698                                                                   
03699  7100-EXIT.                                                       
03700      EXIT.                                                        
03701                                                                   
03702  7200-PRT-CN-HDG.                                                 
03703      MOVE CANCEL-HD1     TO P-DATA.                               
03704      MOVE '0'            TO X.                                    
03705      PERFORM 7000-PRINT-LINE.                                     
03706      MOVE CANCEL-HD2     TO P-DATA.                               
03707      MOVE '0'            TO X.                                    
03708      PERFORM 7000-PRINT-LINE.                                     
03709      MOVE CANCEL-HD3     TO P-DATA.                               
03710      MOVE ' '            TO X.                                    
03711      PERFORM 7000-PRINT-LINE.                                     
03712      MOVE CANCEL-HD4     TO P-DATA.                               
03713      MOVE ' '            TO X.                                    
03714      PERFORM 7000-PRINT-LINE.                                     
03715      ADD 6               TO LINE-CT.                              
03716                                                                   
03717  7200-EXIT.                                                       
03718      EXIT.                                                        
03719                                                                   
03720  7300-PRT-CL-HDG.                                                 
03721      MOVE CLAIM-HD1      TO P-DATA.                               
03722      MOVE '0'            TO X.                                    
03723      PERFORM 7000-PRINT-LINE.                                     
03724      MOVE CLAIM-HD2      TO P-DATA.                               
03725      MOVE '0'            TO X.                                    
03726      PERFORM 7000-PRINT-LINE.                                     
03727      MOVE CLAIM-HD3      TO P-DATA.                               
03728      MOVE ' '            TO X.                                    
03729      PERFORM 7000-PRINT-LINE.                                     
03730      ADD 5               TO LINE-CT.                              
03731                                                                   
03732  7300-EXIT.                                                       
03733      EXIT.                                                        
03734                                                                   
03735  7400-PRT-CC-HDG.                                                 
03736      MOVE CHANGE-HD1     TO P-DATA.                               
03737      MOVE '0'            TO X.                                    
03738      PERFORM 7000-PRINT-LINE.                                     
03739      MOVE CHANGE-HD2     TO P-DATA.                               
03740      MOVE '0'            TO X.                                    
03741      PERFORM 7000-PRINT-LINE.                                     
03742      MOVE CHANGE-HD3     TO P-DATA.                               
03743      MOVE ' '            TO X.                                    
03744      PERFORM 7000-PRINT-LINE.                                     
03745      ADD 4               TO LINE-CT.                              
03746                                                                   
03747  7400-EXIT.                                                       
03748      EXIT.                                                        
03749                                                                   
03750  7500-PRT-SUMRY-HDG.                                              
03751      MOVE USER-TOT-HD1 TO P-DATA.                                 
03752      MOVE '0'          TO X.                                      
03753      PERFORM 7000-PRINT-LINE.                                     
03754      MOVE USER-TOT-HD2 TO P-DATA.                                 
03755      MOVE ' '          TO X.                                      
03756      PERFORM 7000-PRINT-LINE.                                     
03757      MOVE USER-TOT-HD3 TO P-DATA.                                 
03758      MOVE ' '          TO X.                                      
03759      PERFORM 7000-PRINT-LINE.                                     
03760      MOVE SPACES       TO P-DATA.                                 
03761      MOVE ' '          TO X.                                      
03762      PERFORM 7000-PRINT-LINE.                                     
03763      ADD 5             TO LINE-CT.                                
03764                                                                   
03765  7500-EXIT.                                                       
03766      EXIT.                                                        
03767  EJECT                                                            
03768  9000-GET-ACCT.                                                   
03769      MOVE SPACES                 TO  AM-CONTROL-BY-VAR-GRP.       
03770      MOVE CF-COMPANY-CD          TO  AM-COMPANY-CD-A1.            
03771      MOVE PB-ACCOUNT             TO  AM-VG-ACCOUNT.               
03772                                                                   
03773      IF CF-ST-ACCNT-CNTL                                          
03774          MOVE PB-STATE           TO  AM-VG-STATE.                 
03775                                                                   
03776      IF CF-CARR-ACCNT-CNTL                                        
03777          MOVE PB-CARRIER         TO  AM-VG-CARRIER.               
03778                                                                   
03779      IF CF-CARR-ST-ACCNT-CNTL                                     
03780          MOVE PB-CARRIER         TO  AM-VG-CARRIER                
03781          MOVE PB-STATE           TO  AM-VG-STATE.                 
03782                                                                   
03783      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               
03784          MOVE PB-CARRIER         TO  AM-VG-CARRIER                
03785          MOVE PB-GROUPING        TO  AM-VG-GROUPING               
03786          MOVE PB-STATE           TO  AM-VG-STATE.                 
03787                                                                   
03788      START ERACCT                                                 
03789          KEY IS GREATER THAN AM-CONTROL-BY-VAR-GRP.               
03790                                                                   
03791      IF AM-STATUS-CODE = '23'                                     
03792          GO TO 9310-INVALID-ACCOUNT.                              
03793                                                                   
03794      IF AM-STATUS-CODE NOT = ZERO                                 
03795          MOVE AM-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
03796          MOVE 'ERROR OCCURED START - ERACCT'                      
03797                                  TO  WS-ABEND-MESSAGE             
03798          GO TO ABEND-PGM.                                         
03799                                                                   
03800      READ ERACCT NEXT RECORD.                                     
03801                                                                   
03802      IF AM-STATUS-1 = '1'                                         
03803          GO TO 9310-INVALID-ACCOUNT.                              
03804                                                                   
03805      IF AM-STATUS-1 NOT = ZERO                                    
03806          MOVE AM-STATUS-CODE     TO  WS-ABEND-FILE-STATUS         
03807          MOVE 'ERROR OCCURED START - ERACCT'                      
03808                                  TO  WS-ABEND-MESSAGE             
03809          GO TO ABEND-PGM.                                         
03810                                                                   
03811      COPY ELCACCTI.                                               
03812                                                                   
03813      IF CF-ACCNT-CNTL                                             
03814          IF PB-ACCOUNT  = AM-VG-ACCOUNT                           
03815              GO TO 9320-VALID-ACCOUNT                             
03816          ELSE                                                     
03817              GO TO 9310-INVALID-ACCOUNT.                          
03818                                                                   
03819      IF CF-ST-ACCNT-CNTL                                          
03820          IF PB-ACCOUNT   = AM-VG-ACCOUNT AND                      
03821             PB-STATE     = AM-VG-STATE                            
03822              GO TO 9320-VALID-ACCOUNT                             
03823          ELSE                                                     
03824              GO TO 9310-INVALID-ACCOUNT.                          
03825                                                                   
03826      IF CF-CARR-ACCNT-CNTL                                        
03827          IF PB-ACCOUNT   = AM-VG-ACCOUNT AND                      
03828             PB-CARRIER   = AM-VG-CARRIER                          
03829              GO TO 9320-VALID-ACCOUNT                             
03830          ELSE                                                     
03831              GO TO 9310-INVALID-ACCOUNT.                          
03832                                                                   
03833      IF CF-CARR-ST-ACCNT-CNTL                                     
03834          IF PB-ACCOUNT    = AM-VG-ACCOUNT AND                     
03835             PB-STATE      = AM-VG-STATE   AND                     
03836             PB-CARRIER    = AM-VG-CARRIER                         
03837              GO TO 9320-VALID-ACCOUNT                             
03838          ELSE                                                     
03839              GO TO 9310-INVALID-ACCOUNT.                          
03840                                                                   
03841      IF CF-CARR-GROUP-ST-ACCNT-CNTL                               
03842          IF PB-ACCOUNT     = AM-VG-ACCOUNT AND                    
03843             PB-STATE       = AM-VG-STATE   AND                    
03844             PB-CARRIER     = AM-VG-CARRIER AND                    
03845             PB-GROUPING    = AM-VG-GROUPING                       
03846              GO TO 9320-VALID-ACCOUNT.                            
03847                                                                   
03848  9310-INVALID-ACCOUNT.                                            
03849      MOVE '**INVALID ACCOUNT**'  TO H4-NAME.                      
03850      GO TO 9380-CLR-H4.                                           
03851                                                                   
03852  9320-VALID-ACCOUNT.                                              
03853      MOVE AM-NAME                TO  H4-NAME

           IF H4-CSR = SPACES
060208        MOVE AM-CSR-CODE         TO H4-CSR
           END-IF
03854                                                                   
           .
03855  9380-CLR-H4.                                                     
03856      MOVE SPACES TO H4-TOTAL                                      
03857                     H4-CONTD.                                     
03858                                                                   
03859  9400-EXIT.                                                       
03860      EXIT.                                                        
03861      EJECT                                                        
03862  9900-ERROR-FORMAT.                                               
03863      MOVE WS-ERR-CODE-X TO EM-MESSAGE-NUMBER.                     
03864                                                                   
03865      READ ELERRS                                                  
03866          KEY IS EM-CONTROL-PRIMARY.                               
03867                                                                   
03868      IF EM-STATUS-CODE = '23'                                     
03869          MOVE '******ERROR NUMBER NOT FOUND' TO EM-ERROR-TEXT.    
03870                                                                   
03871                                  COPY ELCERRPD.                   
03872                                                                   
03873  9900-EXIT.                                                       
03874       EXIT.                                                       
03875                                                                   
03876  ABEND-PGM SECTION.                                               
03877                                  COPY ELCABEND.                   
03878 /                                                                 
03879  LCP-WRITE-POS-PRT SECTION.                                       
03880      IF LCP-ASA = '+'                                             
03881          WRITE PRT AFTER 0 LINE                                   
03882      ELSE                                                         
03883      IF LCP-ASA = ' '                                             
03884          WRITE PRT AFTER ADVANCING 1 LINE                         
03885      ELSE                                                         
03886      IF LCP-ASA = '0'                                             
03887          WRITE PRT AFTER ADVANCING 2 LINE                         
03888      ELSE                                                         
03889      IF LCP-ASA = '-'                                             
03890          WRITE PRT AFTER ADVANCING 3 LINE                         
03891      ELSE                                                         
03892      IF LCP-ASA = '1'                                             
03893          WRITE PRT AFTER ADVANCING PAGE                           
03894      ELSE                                                         
03895      IF LCP-ASA = '2'                                             
03896          WRITE PRT AFTER ADVANCING LCP-CH2                        
03897      ELSE                                                         
03898      IF LCP-ASA = '3'                                             
03899          WRITE PRT AFTER ADVANCING LCP-CH3                        
03900      ELSE                                                         
03901      IF LCP-ASA = '4'                                             
03902          WRITE PRT AFTER ADVANCING LCP-CH4                        
03903      ELSE                                                         
03904      IF LCP-ASA = '5'                                             
03905          WRITE PRT AFTER ADVANCING LCP-CH5                        
03906      ELSE                                                         
03907      IF LCP-ASA = '6'                                             
03908          WRITE PRT AFTER ADVANCING LCP-CH6                        
03909      ELSE                                                         
03910      IF LCP-ASA = '7'                                             
03911          WRITE PRT AFTER ADVANCING LCP-CH7                        
03912      ELSE                                                         
03913      IF LCP-ASA = '8'                                             
03914          WRITE PRT AFTER ADVANCING LCP-CH8                        
03915      ELSE                                                         
03916      IF LCP-ASA = '9'                                             
03917          WRITE PRT AFTER ADVANCING LCP-CH9                        
03918      ELSE                                                         
03919      IF LCP-ASA = 'A'                                             
03920          WRITE PRT AFTER ADVANCING LCP-CH10                       
03921      ELSE                                                         
03922      IF LCP-ASA = 'B'                                             
03923          WRITE PRT AFTER ADVANCING LCP-CH11                       
03924      ELSE                                                         
03925      IF LCP-ASA = 'C'                                             
03926          WRITE PRT AFTER ADVANCING LCP-CH12                       
03927      ELSE                                                         
03928      IF LCP-ASA = 'V'                                             
03929          WRITE PRT AFTER ADVANCING LCP-P01                        
03930      ELSE                                                         
03931      IF LCP-ASA = 'W'                                             
03932          WRITE PRT AFTER ADVANCING LCP-P02                        
03933      ELSE                                                         
03934      DISPLAY 'ASA CODE ERROR'.                                    
03935  LCP-WRITE-END-PRT.                                               
03936      EXIT.                                                        
