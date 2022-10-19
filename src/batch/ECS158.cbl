00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ECS158.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 05/16/94 16:45:02.                 
00007 *             PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE           
00008 *                            VMOD=2.041.                          
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
00013 *DATE-COMPILED. 10/13/86.                                         
00014                                                                   
00015 *SECURITY.   ***************************************************  
00016 *            *                                                    
00017 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.      
00018 *            *                                                    
00019 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES  
00020 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT   
00021 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC, INC.      
00022 *            *                                                    
00023 *            ***************************************************  
00024                                                                   
00025 *REMARKS.                                                         
00026 *        PROGRAM PULLS AN EXTRACT ON THE CLAS SYSTEM CERT FILE    
00027 *        AND THE EXTRACTS FROM ECS157 IN ORDER TO PROCESS         
00028 *        EXTRACTS FOR INSURANCE COMMISSION CALL.                  
00029 *                                                                 
00030 *        RUN DATE IN DATE CARD IS CONSIDERED TO BE CALL-DATE.     
00031 *                                                                 
00032 *        BEGINNING OF 'CALL' PERIOD IS RUN DATE OR 12 MOS PRIOR   
00033 *        TO EP-DT                                                 
00034 *                                                                 
00035 *                                                                 
00036 ****************************************************************  
00037 *                                                                 
00038 *    RR   =  NUMBER OF MONTHS CERT WAS INFORCE DURING PERIOD      
00039 *                (USED TO CALCULATE MEAN INFORCE)                 
00040 *                                                                 
00041 *    M1   =  NUMBER OF MONTHS REMAINING UNTIL EXPIRATION, DEATH   
00042 *            CANCELLATION FROM START OF PERIOD OR ISSUE DATE      
00043 *            WHICHEVER WAS LATER.                                 
00044 *                (USED TO CALCULATE MEAN INFORCE)                 
00045 *                                                                 
00046 *    M2   =  NUMBER OF MONTHS REMAINING UNTIL EXPIRATION AT END   
00047 *            OF PERIOD IF INFORCE AT END OF PERIOD.               
00048 *                (USED TO CALCULATE UNEARNED PREMIUM AT END)      
00049 *                                                                 
00050 *    M3   =  NUMBER OF MONTHS REMAINING UNTIL EXPIRATION AT STAR  
00051 *            OF PERIOD.                                           
00052 *                (USED TO CALCULATE UNEARNED PREMIUM AT START)    
00053 *                                                                 
00054 *    M5   =  NUMBER OF FULL MONTHS REMAINING UNTIL EXPIRATION     
00055 *          AT END  OF PERIOD IF INFORCE AT END OF PERIOD.         
00056 *                (USED TO CALCULATE UNEARNED PREMIUM AT END)      
00057 *                                                                 
00058 *    M6   =  NUMBER OF FULL MONTHS REMAINING UNTIL EXPIRATION     
00059 *        AT START  OF PERIOD.                                     
00060 *                (USED TO CALCULATE UNEARNED PREMIUM AT START)    
00061 *                                                                 
00062 ****************************************************************  
122002******************************************************************
122002*                   C H A N G E   L O G
122002*
122002* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
122002*-----------------------------------------------------------------
122002*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
122002* EFFECTIVE    NUMBER
122002*-----------------------------------------------------------------
122002* 122002                   PEMA  ADD MONTHLY PRODUCT PROCESSING  
120803* 120803                   PEMA  REMOVE SUPPLEMENTAL IBNR CALC
032609* 032609  CR2008030600001  PEMA  CHANGE TX AND VA TO ROA
120710* 120710  CR2010101500002  PEMA  MN ROA AND MEAN
062712* 062712    2012062600003  PEMA  ADD PROCESSING FOR AHL
052814* 052814  CR2014012300001  PEMA  DCC CREDIT UNION CHANGES
122002******************************************************************
00063  EJECT                                                            
00064  ENVIRONMENT DIVISION.                                            
00065  INPUT-OUTPUT SECTION.                                            
00066  FILE-CONTROL.                                                    
00067                                                                   
00068      SELECT CERTS        ASSIGN TO SYS010-UT-2400-S-SYS010.       
00069      SELECT XTRS-IN      ASSIGN TO SYS012-UT-2400-S-SYS012.       
00070      SELECT ERACCTT      ASSIGN TO SYS015-UT-FBA1-ERACCTT         
00071                          ACCESS IS SEQUENTIAL                     
00072                          ORGANIZATION IS INDEXED                  
00073                          FILE STATUS IS ERACCTT-FILE-STATUS       
00074                          RECORD KEY IS AM-CONTROL-PRIMARY.        
00075      SELECT XTRACT       ASSIGN TO SYS011-UT-2400-S-SYS011.       
00076      SELECT CLMS-HIST    ASSIGN TO SYS013-UT-2400-S-SYS013.       
00077      SELECT SORTED-CLAIMS ASSIGN TO SYS016-UT-FBA1-S-SYS016.      
00078      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       
00079      SELECT PRINTX       ASSIGN TO SYS008-UR-1403-S-SYS008.       
00080      SELECT PRINTC       ASSIGN TO SYS009-UR-1403-S-SYS009.       
00081      SELECT SORT-CLMS    ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.      
00082      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       
00083      SELECT ERRTBL-IN    ASSIGN TO SYS014-ERRTBLT
00084                          ACCESS IS DYNAMIC                        
00085                          ORGANIZATION IS INDEXED                  
00086                          FILE STATUS IS ERRTBL-FILE-STATUS        
00087                          RECORD KEY IS RE-CONTROL-PRIMARY.        
00088  EJECT                                                            
00089  DATA DIVISION.                                                   
00090  FILE SECTION.                                                    
00091                                                                   
00092  FD  CLMS-HIST                                                    
00093                             COPY ECSEXTFD.                        
00094                                                                   
00095  01  IN-CLMS                 PIC X(510).                          
00096                                                                   
00097  EJECT                                                            
00098  FD  ERRTBL-IN.
00099                                  COPY ERCREIN.                    
00100                                                                   
00101      EJECT                                                        
00102  FD  CERTS                                                        
00103                                  COPY ECSCRIFD.                   
00104                                  COPY ECSCRT01.                   
00105                                                                   
00106      EJECT                                                        
00107  FD  ERACCTT.                                                     
00108                                  COPY ERCACCT.                    
00109      EJECT                                                        
00110  SD  SORT-CLMS.                                                   
00111                                                                   
00112  01  SORT-CLMS-REC.                                               
00113      12  SORT-CLAIM-KEY.                                          
00114          16  S-CARRIER       PIC X(01).                           
00115          16  S-GROUPING      PIC X(06).                           
00116          16  S-STATE         PIC X(02).                           
00117          16  S-ACCOUNT       PIC X(10).                           
00118          16  S-EFF-DATE      PIC X(06).                           
00119          16  S-CERT-NO       PIC X(11).                           
00120      12  SORT-EP-EXTR        PIC X(510).                          
00121                                                                   
00122      EJECT                                                        
00123  FD  SORTED-CLAIMS                                                
00124      BLOCK CONTAINS 0 RECORDS
00125      RECORDING MODE IS F.                                         
00126                                                                   
00127  01  SORTED-CLAIMS-REC.                                           
00128      12  FILLER              PIC X(36).                           
00129      12  SORTED-EXTRACT      PIC X(510).                          
00130      EJECT                                                        
00131  FD  XTRACT                                                       
00132      BLOCK CONTAINS 0 RECORDS
00133      RECORDING MODE IS F.                                         
00134                                                                   
00135                                                                   
00136  01  CALL-X                      PIC X(666).                      
00137                                                                   
00138  FD  XTRS-IN                                                      
00139      BLOCK CONTAINS 0 RECORDS
00140      RECORDING MODE IS F.                                         
00141                                                                   
00142  01  XTR-RECORD                  PIC X(666).                      
00143      EJECT                                                        
00144  FD  DISK-DATE                                                    
00145                                  COPY ELCDTEFD.                   
00146      EJECT                                                        
00147  FD  PRINTX                                                       
00148                                  COPY ELCPRTFD.                   
00149  FD  PRINTC                                                       
00150      RECORDING MODE F.                                            
00151                                                                   
00152  01  PRTC.                                                        
00153      12  PRTC-CTL                PIC X(01).                       
00154      12  PRTC-DATA               PIC X(132).                      
00155                                                                   
00156  FD  FICH                                                         
00157                                  COPY ECSFICH.                    
00158      EJECT                                                        
00159  WORKING-STORAGE SECTION.                                         
00160  77  FILLER  PIC X(32) VALUE '********************************'.  
00161  77  FILLER  PIC X(32) VALUE '     ECS158 WORKING STORAGE     '.  
00162  77  FILLER  PIC X(32) VALUE '********* VMOD=2.041 ***********'.  
00163                                                                   
       77  WS-CLMS-DB-CNT          PIC 9(9)  VALUE ZEROS.
       77  WS-CERT-DB-CNT          PIC 9(9)  VALUE ZEROS.
       77  WS-ACCT-DB-CNT          PIC 9(9)  VALUE ZEROS.
       77  WS-RTBL-DB-CNT          PIC 9(9)  VALUE ZEROS.
       77  WS-SCLM-DB-CNT          PIC 9(9)  VALUE ZEROS.
       77  WS-RATE-DB-CNT          PIC 9(9)  VALUE ZEROS.
       77  WS-TOT-CLMS-DB-CNT      PIC 9(9)  VALUE ZEROS.
       77  WS-TOT-CERT-DB-CNT      PIC 9(9)  VALUE ZEROS.
       77  WS-TOT-ACCT-DB-CNT      PIC 9(9)  VALUE ZEROS.
       77  WS-TOT-RTBL-DB-CNT      PIC 9(9)  VALUE ZEROS.
       77  WS-TOT-SCLM-DB-CNT      PIC 9(9)  VALUE ZEROS.
       77  WS-TOT-RATE-DB-CNT      PIC 9(9)  VALUE ZEROS.
00164  77  FROM-LIFE-SW            PIC X(01) VALUE SPACES.              
00165  77  PAGE-CTR                PIC S9(07)    VALUE +0  COMP-3.      
00166  77  LF-BAL-REMTERM          PIC S9(03)V99 VALUE +0 COMP-3.       
00167  77  LINE-CTR                PIC S9(03)    VALUE +99 COMP-3.      
00168  77  WRK-A                   PIC S9(7)V99  VALUE +0  COMP-3.      
00169  77  WRK-B                   PIC S9(7)V99  VALUE +0  COMP-3.      
00170  77  WRK-ALT                 PIC S9(7)V99  VALUE +0  COMP-3.      
00171  77  WRK-C                   PIC S9(07)V99 VALUE +0  COMP-3.      
00172  77  WRK-D                   PIC S9(4)V9(11) VALUE +0  COMP-3.    
00173  77  WRK-E                   PIC S9(4)V9(11) VALUE +0  COMP-3.    
00174  77  WRK-F                   PIC S9(07)V99 VALUE +0  COMP-3.      
00175  77  WRK-G                   PIC S9(07)V99 VALUE +0  COMP-3.      
00176  77  WRK-H                   PIC S9(07)V99 VALUE +0  COMP-3.      
CIDMOD 77  WS-DIFF                 PIC S9(7)V9(5) VALUE +0 COMP-3.
00177  77  WRK-REMTRM              PIC S9(03)    VALUE +0  COMP-3.      
00178  77  WS-MEAN-CALC            PIC S9(09)V99 VALUE +0  COMP-3.      
00179  77  WS-ACCT-MATCH-SW        PIC X(01)  VALUE SPACES.             
00180      88  ACCOUNT-MATCHED         VALUE '*'.                       
00181  77  M4                      PIC S9(03)V99 VALUE +0  COMP-3.      
00181  77  M9                      PIC S9(03)V99 VALUE +0  COMP-3.      
00182  77  WORK-M4                 PIC S9(03)    VALUE +0  COMP-3.      
00183  77  ACCT-MATCH-CONTROL      PIC X(19)     VALUE LOW-VALUES.      
00184  77  ACCT-MATCH-EFF          PIC 9(11)     VALUE 0.               
00185  77  LIFE-AH-SUB             PIC S9(04)    VALUE +0  COMP.        
00186  77  CERT-CONTROL            PIC X(36)     VALUE LOW-VALUES.      
00187  77  CLAIMS-CONTROL          PIC X(36)     VALUE LOW-VALUES.      
00188  77  HOLD-CLAIMS-CONTROL     PIC X(36)     VALUE LOW-VALUES.      
00189  77  WS-LF-RATE-ATTEMPT      PIC 9(07)     VALUE ZEROS.           
00190  77  WS-AH-RATE-ATTEMPT      PIC 9(07)     VALUE ZEROS.           
00191  77  WS-LF-RATE-ERROR        PIC 9(07)     VALUE ZEROS.           
00192  77  WS-AH-RATE-ERROR        PIC 9(07)     VALUE ZEROS.           
00193  77  WS-HOLD-LF-PRIM-FAC     PIC S9(9)V99  VALUE +0  COMP-3.      
00194  77  WS-HOLD-AH-PRIM-FAC     PIC S9(9)V99  VALUE +0  COMP-3.      
00195  77  LF-DEV-TO-PRIM-FACTOR   PIC S99V9(5)  VALUE +0  COMP-3.      
00196  77  AH-DEV-TO-PRIM-FACTOR   PIC S99V9(5)  VALUE +0  COMP-3.      
00197  77  YR                      PIC S999      VALUE +3  COMP.        
00198  77  SA                      PIC S999      VALUE +0  COMP.        
00199  77  SB                      PIC S999      VALUE +0  COMP.        
00200  77  REM-TRM                 PIC S999      VALUE +0  COMP-3.      
00201  77  SV-FACTOR               PIC S9        VALUE +0  COMP-3.      
00202  77  COMM-PREMM              PIC S9(7)V99  VALUE +0  COMP-3.      
00203  77  AGT-COMM                PIC S9(4)V99  VALUE +0  COMP-3.      
00204  77  REINS                   PIC S9(6)     VALUE +0  COMP-3.      
00205  77  FLD                     PIC S999      VALUE +0  COMP-3.      
00206  77  X                       PIC X         VALUE ' '.             
00207  77  ACC-SW                  PIC X         VALUE SPACES.          
00208  77  EFF-COUNT               PIC X         VALUE SPACES.          
00209  77  FLA-CLM-REIN-BASE       PIC S9(5)V99  VALUE +0  COMP-3.      
00210  77  ADJ-FLA-REIN-BASE       PIC S9(5)V9(4) VALUE +0 COMP-3.      
00211  77  REIN-RT-SW              PIC X         VALUE SPACE.           
00212  77  DTO-CC                  PIC 9(2)      VALUE 0.               
00213  77  DTO-YR                  PIC 9(2)      VALUE 0.               
00214  77  DTO-MO                  PIC 9(2)      VALUE 0.               
00215  77  WORK-MO                 PIC 9(3)      VALUE 0.               
00216  77  WORK-MO1                PIC 9(3)      VALUE 0.               
00217  77  CALL-CNT                PIC S9(7)     VALUE +0  COMP-3.      
00218  77  TEXAS-CNT               PIC S9(7)     VALUE +0  COMP-3.      
00219  77  ERRTBL-FILE-STATUS      PIC XX        VALUE '00'.            
00220  77  ERACCTT-FILE-STATUS     PIC XX        VALUE '00'.            
00221  77  LF-EARN-CODE            PIC X(01)     VALUE ' '.             
00222  77  AH-EARN-METHOD          PIC X(01)     VALUE ' '.             
00223  77  TEMP-3                  PIC S9(2)V9(9) VALUE +0 COMP-3.      
00224  77  WS-MATCH-SWITCH         PIC X(01)     VALUE ' '.             
00225      88  CLAIM-NO-CERT           VALUE '1'.                       
00226      88  CERT-NO-CLAIM           VALUE '2'.                       
00227      88  CERT-AND-CLAIM          VALUE '3'.                       
00228  77  MEAN-CAL                PIC S9(11)V99 VALUE +0  COMP-3.      
00229  77  WS-ACCT-MATCH           PIC X(01)     VALUE SPACES.          
00230      88  FROM-CLAIM              VALUE '1'.                       
00231      88  FROM-CERT               VALUE '2'.                       
00232  77  SAVE-STATE              PIC X(02)     VALUE SPACES.          
00233  77  TEMP-STATE              PIC X(02)     VALUE SPACES.          
00234  77  SAVE-BUSC-TYPE          PIC 9(02)     VALUE ZEROS.           
00235  77  WS-REPORT-SW            PIC X(01)     VALUE SPACES.          
00236      88  STATE-IS-EXCLUDED       VALUE 'X'.                       
00237      88  REPORT-OVER-UNDER-60    VALUES '4' '5'.                  
00238  77  WS-EXCL-BUSC-SW         PIC X(01)     VALUE SPACES.          
00239      88  BUSC-TYPE-IS-EXCLUDED   VALUE 'X'.                       
00240  77  EXCL-SUB                PIC S9(04)    VALUE +0  COMP.        
00241  77  STATE-TX                PIC X(02)     VALUE SPACES.          
00242  77  STATE-IL                PIC X(02)     VALUE SPACES.          
00243  77  STATE-GA                PIC X(02)     VALUE SPACES.          
00244  77  STATE-NC                PIC X(02)     VALUE SPACES.          
00245  77  STATE-UT                PIC X(02)     VALUE SPACES.          
00246  77  STATE-IA                PIC X(02)     VALUE SPACES.          
00247  77  STATE-NE                PIC X(02)     VALUE SPACES.          
00248  77  STATE-NV                PIC X(02)     VALUE SPACES.          
00249  77  STATE-OH                PIC X(02)     VALUE SPACES.          
00250  77  STATE-OR                PIC X(02)     VALUE SPACES.          
00251  77  STATE-WI                PIC X(02)     VALUE SPACES.          
00252  77  WS-PROCESS-SW           PIC X(01)     VALUE SPACES.          
00253      88  SINGLE-PERIOD-PROCESS   VALUE 'Y'.                       
00254  77  NUMBER-OF-PERIODS       PIC S999      VALUE +3  COMP.        
00255  77  WK-DEV-FACTOR           PIC S9(7)     VALUE +1000000.        
00256                                                                   
00257  01  WS-DEBUG-AREA.                                               
00258      12  WS-DEBUG-SW             PIC X(01) VALUE ' '.             
00259          88  DEBUG-IS-ON                VALUES '1' '2' '3' '4'.   
00260          88  DEBUG-LF-INFORCE-CNT       VALUE '1'.                
00261          88  DEBUG-AH-INFORCE-CNT       VALUE '2'.                
00262          88  DEBUG-LF-STATUTORY         VALUE '3'.                
00263          88  DEBUG-AH-STATUTORY         VALUE '4'.                
00264      12  WS-S-CNT                PIC S9(11) COMP-3 VALUE +0.      
00265      12  WS-S-INFORCE-CNT        PIC S9(11) COMP-3 VALUE +0.      
00266      12  WS-S-STATUTORY          PIC S9(11)V99 COMP-3 VALUE +0.   
00267      12  FORMAT-DATE.                                             
00268          16  FORMAT-MONTH        PIC XX.                          
00269          16  FILLER              PIC X  VALUE '/'.                
00270          16  FORMAT-DAY          PIC XX.                          
00271          16  FILLER              PIC X  VALUE '/'.                
00272          16  FORMAT-YEAR         PIC XX.                          
00273      12  BIN-DE-CLM-PROC-DT      PIC XX.                          
00274      12  BIN-DE-INCUR            PIC XX.                          
00275                                                                   
00276  01  WORK-DEVIATION-CODES.                                        
00277      12  WK-LF-DEV-CODE.                                          
00278          16  WK-LF-DEV-CODE-1    PIC X.                           
00279          16  WK-LF-DEV-CODE-2    PIC X.                           
00280          16  WK-LF-DEV-CODE-3    PIC X.                           
00281      12  WK-AH-DEV-CODE.                                          
00282          16  WK-AH-DEV-CODE-1    PIC X.                           
00283          16  WK-AH-DEV-CODE-2    PIC X.                           
00284          16  WK-AH-DEV-CODE-3    PIC X.                           
00285      12  WK-TEMP-DEV-PCT         PIC 9(7).                        
00286      12  WK-TEMP-DEV-PCT-RDF     REDEFINES WK-TEMP-DEV-PCT.       
00287          16  WK-TEMP-DEV         PIC X(7).                        
00288      EJECT                                                        
00289                              COPY ECSEXTCL.                       
00290      EJECT                                                        
00291                                                                   
00292                              COPY ECSEXT01.                       
00293      EJECT                                                        
00294  01  TEXAS-REGS-WORK-AREAS.                                       
00295      12  TEX-FACT-1          PIC S9(7)V9(2)    COMP-3.            
00296      12  TEX-FACT-2          PIC S9(3)   COMP-3.                  
00297      12  TEX-FACT-3          PIC S9(3)   COMP-3.                  
00298      12  TEX-FACT-4          PIC S9(7)   COMP-3.                  
00299      12  TEX-FACT-5          PIC S9(3)   COMP-3.                  
00300      12  TEX-FACT-6          PIC S9(3)   COMP-3.                  
00301      12  TEX-FACT-7          PIC S9(7)   COMP-3.                  
00302      12  TEX-FACT-8          PIC S9V9(6) COMP-3.                  
00303      12  TEX-FACT-9          PIC S9(4)V9(11)   COMP-3.            
00304                                                                   
00305  01  NET-PAY-INTERFACE.                                           
00306      05  NP-APR                  PIC S9(3)V9(4)    COMP-3.        
00307      05  NP-ORIG                 PIC S9(3)         COMP-3.        
00308      05  NP-REM                  PIC S9(3)         COMP-3.        
00309      05  NP-OPT                  PIC X(01).                       
00310      05  NP-CAP                  PIC S9(3)         COMP-3.        
00311      05  NP-FACTOR               PIC S9(4)V9(9)    COMP-3.        
00312      05  NP-WORK1                PIC S9(6)V9(9)    COMP-3.        
00313      05  NP-WORK2                PIC S9(6)V9(9)    COMP-3.        
00314                                                                   
00315  01  METHOD-TABLE.                                                
00316      05  M-TABLE.                                                 
00317          10  FILLER              PIC X(03) VALUE 'AKP'.           
00318          10  FILLER              PIC X(03) VALUE 'ALP'.           
00319          10  FILLER              PIC X(03) VALUE 'ARR'.           
00320          10  FILLER              PIC X(03) VALUE 'AZP'.           
00321          10  FILLER              PIC X(03) VALUE 'CAP'.           
00322          10  FILLER              PIC X(03) VALUE 'DEP'.           
00323          10  FILLER              PIC X(03) VALUE 'GAP'.           
00324          10  FILLER              PIC X(03) VALUE 'IDP'.           
00325          10  FILLER              PIC X(03) VALUE 'ILP'.           
00326          10  FILLER              PIC X(03) VALUE 'KSP'.           
00327          10  FILLER              PIC X(03) VALUE 'KYP'.           
00328          10  FILLER              PIC X(03) VALUE 'LAP'.           
00329          10  FILLER              PIC X(03) VALUE 'MDP'.           
00330          10  FILLER              PIC X(03) VALUE 'MIP'.           
00331          10  FILLER              PIC X(03) VALUE 'MTP'.           
00332          10  FILLER              PIC X(03) VALUE 'NMR'.           
00333          10  FILLER              PIC X(03) VALUE 'NVP'.           
00334          10  FILLER              PIC X(03) VALUE 'OHO'.           
00335          10  FILLER              PIC X(03) VALUE 'OKK'.           
00336          10  FILLER              PIC X(03) VALUE 'ORP'.           
00337          10  FILLER              PIC X(03) VALUE 'PAR'.           
00338          10  FILLER              PIC X(03) VALUE 'SCP'.           
00339          10  FILLER              PIC X(03) VALUE 'SDP'.           
00340          10  FILLER              PIC X(03) VALUE 'TNR'.           
00341          10  FILLER              PIC X(03) VALUE 'TXT'.           
00342          10  FILLER              PIC X(03) VALUE 'UTR'.           
00343          10  FILLER              PIC X(03) VALUE 'VAV'.           
00344          10  FILLER              PIC X(03) VALUE 'WAR'.           
00345          10  FILLER              PIC X(03) VALUE 'WIP'.           
00346          10  FILLER              PIC X(03) VALUE 'WVR'.           
00347          10  FILLER              PIC X(03) VALUE 'WYP'.           
00348      05  MR-TABLE REDEFINES M-TABLE  OCCURS 31 TIMES              
00349                             INDEXED BY TABLE-INDEX.               
00350          10  T-STATE             PIC X(02).                       
00351          10  METHOD-CODE         PIC X(01).                       
00352                                                                   
00353  01  TYPE-METHOD                 PIC X(01)     VALUE SPACES.      
00354      88  R78                           VALUE 'R'.                 
00355      88  PRATA                         VALUE 'P'.                 
00356      88  TEXAS-SPECIAL                 VALUE 'T'.                 
00357      88  OK-SPECIAL                    VALUE 'K'.                 
00358      88  MEAN                          VALUE 'M'.                 
00359      88  ANTICIPATION                  VALUE 'A'.                 
00360      88  OHIO-SPECIAL                  VALUE 'O'.                 
00361      88  VIRGINIA-SPECIAL              VALUE 'V'.                 
00362      88  1-3RD-2-3RDS                  VALUE '1'.                 
00363      88  AH-EARN-NET-PAY               VALUE 'N'.                 
00364                                                                   
00365  01  DEFAULT-AH-METHOD           PIC X(01)     VALUE 'R'.         
00366                                                                   
00367  01  WS-ABEND-AREA.                                               
00368      05  WS-ABEND-FILE-STATUS    PIC X(02).                       
00369      05  WS-ABEND-MESSAGE        PIC X(80) VALUE SPACES.          
00370      05  WS-RETURN-CODE          PIC S9(04)  COMP VALUE +0.       
00371      05  WS-ZERO                 PIC S9(01) VALUE +0 COMP-3.      
00372                                                                   
00373  01  WORK-ABEND-CODE.                                             
00374      12  WAC-1                   PIC X.                           
00375      12  WAC-2                   PIC X.                           
00376      12  WAC-3-4.                                                 
00377          16  WAC-3               PIC X.                           
00378          16  WAC-4               PIC X.                           
00379                                                                   
00380      EJECT                                                        
00381                                COPY ELCCALC.                      
00382      EJECT                                                        
00383  01  INITIALIZED-FRONT.                                           
00384      12  FILLER              PIC X(35)      VALUE SPACES.         
00385      12  FILLER              PIC X(37)      VALUE SPACES.         
00386      12  FILLER              PIC X(02)      VALUE ZEROS.          
00387      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00388      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00389      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00390      12  FILLER              PIC X(26)      VALUE SPACES.         
00391      12  FILLER              PIC 9(02)      VALUE ZEROS.          
00392      12  FILLER              PIC X(12)      VALUE SPACES.         
00393      12  FILLER              PIC 9(02)      VALUE ZEROS.          
00394      12  FILLER              PIC S9(3)V9(4) VALUE +0 COMP-3.      
00395      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00396      12  FILLER              PIC X(01)      VALUE SPACES.         
00397      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00398      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00399      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00400      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00401  01  INITIALIZED-BACK.                                            
00402      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00403      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00404      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00405      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00406      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00407      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00408      12  FILLER              PIC S9(03)V9   VALUE +0 COMP-3.      
00409      12  FILLER              PIC S9(03)V9   VALUE +0 COMP-3.      
00410      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00411      12  FILLER              PIC S9(03)     VALUE +0 COMP-3.      
00412      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00413      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00414      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00415      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00416      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00417      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00418      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00419      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00420      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00421      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00422      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00423      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00424      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00425      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00426      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00427      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00428      12  FILLER              PIC S9(11)V99  VALUE +0 COMP-3.      
00429      12  FILLER              PIC S9(5)V99   VALUE +0 COMP-3.      
00430      12  FILLER              PIC S9(5)V99   VALUE +0 COMP-3.      
00431      12  FILLER              PIC S9(7)V99   VALUE +0 COMP-3.      
00432      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00433      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00434      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00435      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00436      12  FILLER              PIC S9(9)V99   VALUE +0 COMP-3.      
00437      12  FILLER              PIC X(12)      VALUE SPACES.         
00438                                                                   
00439 ******************************************************************
00440      EJECT                                                        
00441  01  FILLER.                                                      
00442      12  WS-RESERVE-WORK OCCURS 3 TIMES.                          
00443          16  WS-AH-END-R78-RES      PIC S9(09)V99 COMP-3.         
00444          16  WS-AH-BEG-R78-RES      PIC S9(09)V99 COMP-3.         
00445          16  WS-AH-END-PRO-RES      PIC S9(09)V99 COMP-3.         
00446          16  WS-AH-BEG-PRO-RES      PIC S9(09)V99 COMP-3.         
00447          16  WS-AH-END-NET-RES      PIC S9(09)V99 COMP-3.         
00448          16  WS-AH-BEG-NET-RES      PIC S9(09)V99 COMP-3.         
00449          16  WS-AH-END-MEAN-RES     PIC S9(09)V99 COMP-3.         
00450          16  WS-AH-BEG-MEAN-RES     PIC S9(09)V99 COMP-3.         
00451          16  WS-AH-END-STATE-RES    PIC S9(09)V99 COMP-3.         
00452          16  WS-AH-BEG-STATE-RES    PIC S9(09)V99 COMP-3.         
00453          16  WS-LEV-LF-BEGIN-RES    PIC S9(09)V99 COMP-3.         
00454          16  WS-LEV-LF-BEGIN-ALT    PIC S9(09)V99 COMP-3.         
00455          16  WS-LEV-LF-END-RES      PIC S9(09)V99 COMP-3.         
00456          16  WS-LEV-LF-END-ALT      PIC S9(09)V99 COMP-3.         
00457          16  WS-RED-LF-BEGIN-RES    PIC S9(09)V99 COMP-3.         
00458          16  WS-RED-LF-BEGIN-ALT    PIC S9(09)V99 COMP-3.         
00459          16  WS-RED-LF-END-RES      PIC S9(09)V99 COMP-3.         
00460          16  WS-RED-LF-END-ALT      PIC S9(09)V99 COMP-3.         
00461                                                                   
00462      12  WS-RESERVE-INIT.                                         
00463          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00464          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00465          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00466          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00467          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00468          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00469          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00470          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00471          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00472          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00473          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00474          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00475          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00476          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00477          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00478          16  FILLER            PIC S9(09)V99 VALUE +0 COMP-3.     
00479                                                                   
00480      12  WS-REM-TERM-WORK OCCURS 3 TIMES.                         
00481          16  WS-LF-BEG-REM-TRM1     PIC S9(03)V99 COMP-3.         
00482          16  WS-LF-END-REM-TRM1     PIC S9(03)V99 COMP-3.         
00483          16  WS-AH-BEG-REM-TRM1     PIC S9(03)V99 COMP-3.         
00484          16  WS-AH-END-REM-TRM1     PIC S9(03)V99 COMP-3.         
00485          16  WS-LF-LEV-MEAN-INFORCE PIC S9(11)V99 COMP-3.         
00486          16  WS-LF-RED-MEAN-INFORCE PIC S9(11)V99 COMP-3.         
00487                                                                   
00488      12  WS-REM-TERM-INIT.                                        
00489          16  FILLER                 PIC S9(03)V99 COMP-3 VALUE +0.
00490          16  FILLER                 PIC S9(03)V99 COMP-3 VALUE +0.
00491          16  FILLER                 PIC S9(03)V99 COMP-3 VALUE +0.
00492          16  FILLER                 PIC S9(03)V99 COMP-3 VALUE +0.
00493          16  FILLER                 PIC S9(11)V99 COMP-3 VALUE +0.
00494          16  FILLER                 PIC S9(11)V99 COMP-3 VALUE +0.
00495                                                                   
00496  01  FILLER.                                                      
CIDMOD     12  INTERMED                PIC S9(9)V9(6)  COMP-3.
CIDMOD     12  WS-REM-AMT              PIC S9(11)V99 COMP-3 VALUE +0.
00497      12  FILLER OCCURS 2 TIMES.                                   
00498          16  WS-DEV           PIC X(03).                          
00499          16  WS-BEN-TYPE      PIC X(02).                          
00500          16  WS-TERM          PIC S9(03)  COMP-3.                 
00501          16  FILLER OCCURS 2 TIMES.                               
00502              20  FILLER OCCURS 3 TIMES.                           
00503                  24  WS-CLAIM-AMT PIC S9(09)V99 COMP-3.           
00504                  24  WS-INC-CNT   PIC S9(03).                     
00505                  24  WS-PD-CNT    PIC S9(03).                     
00506                                                                   
00507      12  HOLD-LIFE-RECORD        PIC X(666)  VALUE SPACES.        
00508      12  HOLD-AH-RECORD          PIC X(666)  VALUE SPACES.        
00509      12  FROM-REIN               PIC X(01) VALUE SPACES.          
00510      12  WS-AGT-NO.                                               
00511          16  WS-AGT-SIX       PIC X(06)    VALUE SPACES.          
00512          16  FILLER           PIC X(04)    VALUE SPACES.          
00513                                                                   
00514  01  DATE-AREAS.                                                  
00515      05  FIRST-PAY-DT              PIC X(02).                     
00516      05  ENTRY-DT                  PIC X(02).                     
00517      05  EXIT-DT                   PIC X(02).                     
00518      05  EXPIRE-DT                 PIC X(02).                     
00519      05  EFFECT-DT                 PIC X(02).                     
00520      05  BEGIN-DT   OCCURS 3 TIMES PIC X(02).                     
00521      05  END-DT     OCCURS 3 TIMES PIC X(02).                     
00522      05  DT-START                  PIC X(02).                     
00523      05  DT-END                    PIC X(02).                     
00524      05  AH-CLM                    PIC X(02).                     
00525      05  AH-CAN                    PIC X(02).                     
00526      05  LF-CLM                    PIC X(02).                     
00527      05  LF-CAN                    PIC X(02).                     
00528      05  WS-CR-BIN-DATE            PIC XX.                        
00529                                                                   
00530      05  CLM-INCUR-REIN-DATE.                                     
00531          10  CIRD-CCYY.                                           
00532              15  CIRD-CC         PIC 99.                          
00533              15  CIRD-YR         PIC 99.                          
00534          10  CIRD-MO             PIC 99.                          
00535                                                                   
00536      05  BEGIN-DATE OCCURS 3 TIMES.                               
00537          10  BEGIN-CCYY          PIC 9(04).                       
00538          10  BEGIN-CCYR  REDEFINES  BEGIN-CCYY.                   
00539              15  BEGIN-CC        PIC 99.                          
00540              15  BEGIN-YR        PIC 99.                          
00541          10  BEGIN-MO            PIC 99.                          
00542          10  BEGIN-DA            PIC 99.                          
00543                                                                   
00544                                                                   
00545      05  ENDING-DATE OCCURS 3 TIMES.                              
00546          10  ENDING-CCYY         PIC 9(04).                       
00547          10  ENDING-CCYR  REDEFINES  ENDING-CCYY.                 
00548              15  ENDING-CC       PIC 99.                          
00549              15  ENDING-YR       PIC 99.                          
00550          10  ENDING-MO           PIC 99.                          
00551          10  ENDING-DA           PIC 99.                          
00552                                                                   
00553      05  ENTRY-DATE.                                              
00554          10  ENTRY-YR            PIC 99.                          
00555          10  ENTRY-MO            PIC 99.                          
00556          10  ENTRY-DA            PIC 99.                          
00557                                                                   
00558      05  EXIT-DATE.                                               
00559          10  EXIT-YR             PIC 99.                          
00560          10  EXIT-MO             PIC 99.                          
00561          10  EXIT-DA             PIC 99.                          
00562                                                                   
00563      05  EXPIRE-DATE.                                             
00564          10  EXPIRE-YR           PIC 99.                          
00565          10  EXPIRE-MO           PIC 99.                          
00566          10  EXPIRE-DA           PIC 99.                          
00567                                                                   
00568  01  REM-TRM-MISC-FIELDS.                                         
00569      12  BGN-DT.                                                  
00570          16  BGN-YR              PIC 99.                          
00571          16  BGN-MO              PIC 99.                          
00572          16  BGN-DA              PIC 99.                          
00573      12  END-DTE.                                                 
00574          16  END-YR              PIC 99.                          
00575          16  END-MO              PIC 99.                          
00576          16  END-DA              PIC 99.                          
00577      12  REM-TRM1                PIC S9(3)V99    COMP-3.          
00578      12  REM-TRM2                PIC S9(3)V99    COMP-3.          
00579                                                                   
00580                                  COPY ECSRITAB.                   
00581      EJECT                                                        
00582                                                                   
00583  01  ABEND-FIELDS.                                                
00584      12  PGM-SUB         PIC S999 COMP  VALUE +158.               
00585      12  FIRST-TIME-SW   PIC X  VALUE 'Y'.                        
00586          88  FIRST-TIME         VALUE 'Y'.                        
00587                                                                   
00588      EJECT                                                        
00589  01  HEAD-1.                                                      
00590      05  FILLER      PIC X(54) VALUE SPACES.                      
00591      05  FILLER      PIC X(18) VALUE 'STATE CALL EXTRACT'.        
00592      05  FILLER      PIC X(47) VALUE SPACES.                      
00593      05  FILLER      PIC X(06) VALUE 'ECS158'.                    
00594      05  FILLER      PIC X(06) VALUE SPACES.                      
00595                                                                   
00596  01  HEAD-2.                                                      
00597      05  FILLER      PIC X(47) VALUE SPACES.                      
00598      05  HD-CLIENT   PIC X(30) VALUE SPACES.                      
00599      05  FILLER      PIC X(42) VALUE SPACES.                      
00600      05  HD-DATE     PIC X(08) VALUE SPACES.                      
00601      05  FILLER      PIC X(05) VALUE SPACES.                      
00602                                                                   
00603  01  HEAD-3.                                                      
00604      05  FILLER      PIC X(53) VALUE SPACES.                      
00605      05  HD-ALF-DTE  PIC X(18) VALUE SPACES.                      
00606      05  FILLER      PIC X(41) VALUE SPACES.                      
00607      05  FILLER      PIC X(05) VALUE 'PAGE'.                      
00608      05  HD-PAGE     PIC ZZ,ZZZ.                                  
00609                                                                   
00610  01  HEAD-4.                                                      
00611      12  FILLER      PIC X(50)   VALUE                            
00612         ' CARR GROUP STATE ACCOUNT    CERTIFICATE EFF-DT   '.     
00613      12  FILLER      PIC X(55)   VALUE                            
00614         '   BENEFIT     PREMIUM  TERM  REM TERM    UNEARN    L/A'.
00615      12  FILLER      PIC X(25)   VALUE                            
00616         '   BENEFIT DES  PRIM PREM'.                              
00617      EJECT                                                        
00618  01  DETAIL-1.                                                    
00619      12  FILLER      PIC X(02)   VALUE SPACES.                    
00620      12  D-CARR      PIC X(01)   VALUE SPACES.                    
00621      12  FILLER      PIC X(03)   VALUE SPACES.                    
00622      12  D-GROUP     PIC X(06)   VALUE SPACES.                    
00623      12  FILLER      PIC X(02)   VALUE SPACES.                    
00624      12  D-STATE     PIC X(02)   VALUE SPACES.                    
00625      12  FILLER      PIC X(02)   VALUE SPACES.                    
00626      12  D-ACCOUNT   PIC X(10)   VALUE SPACES.                    
00627      12  FILLER      PIC X(01)   VALUE SPACES.                    
00628      12  D-CERT-NO   PIC X(11)   VALUE SPACES.                    
00629      12  FILLER      PIC X(01)   VALUE SPACES.                    
00630      12  D-EFF-DATE  PIC X(08)   VALUE SPACES.                    
00631      12  FILLER      PIC X(01)   VALUE SPACES.                    
00632      12  D-BENEFIT   PIC Z(9).99 VALUE ZEROS.                     
00633      12  FILLER      PIC X(01)   VALUE SPACES.                    
00634      12  D-PREM      PIC Z(7).99 VALUE ZEROS.                     
00635      12  FILLER      PIC X(02)   VALUE SPACES.                    
00636      12  D-TERM      PIC ZZ9     VALUE ZEROS.                     
00637      12  FILLER      PIC X(04)   VALUE SPACES.                    
00638      12  D-REM-TERM  PIC ZZ9     VALUE ZEROS.                     
00639      12  FILLER      PIC X(05)   VALUE SPACES.                    
00640      12  D-UNEARN    PIC Z(7).99 VALUE ZEROS.                     
00641      12  FILLER      PIC X(03)   VALUE SPACES.                    
00642      12  D-L-A       PIC X(01)   VALUE SPACES.                    
00643      12  FILLER      PIC X(05)   VALUE SPACES.                    
00644      12  D-BEN-DESC  PIC X(10)   VALUE SPACES.                    
00645      12  FILLER      PIC X(01)   VALUE SPACES.                    
00646      12  D-PRI-PREM  PIC Z(7).99 VALUE ZEROS.                     
00647                                                                   
00648  01  HEAD-1B.                                                     
00649      05  FILLER      PIC X(54) VALUE SPACES.                      
00650      05  FILLER      PIC X(18) VALUE 'STATE CALL EXTRACT'.        
00651      05  FILLER      PIC X(47) VALUE SPACES.                      
00652      05  FILLER      PIC X(07) VALUE 'ECS158B'.                   
00653      05  FILLER      PIC X(06) VALUE SPACES.                      
00654                                                                   
00655  01  HEAD-2B.                                                     
00656      05  FILLER      PIC X(47) VALUE SPACES.                      
00657      05  HD2B-CLIENT PIC X(30) VALUE SPACES.                      
00658      05  FILLER      PIC X(42) VALUE SPACES.                      
00659      05  HD2B-DATE   PIC X(08) VALUE SPACES.                      
00660      05  FILLER      PIC X(05) VALUE SPACES.                      
00661                                                                   
00662  01  HEAD-3B.                                                     
00663      05  FILLER      PIC X(53) VALUE SPACES.                      
00664      05  HD3B-ALF-DTE PIC X(18) VALUE SPACES.                     
00665      05  FILLER      PIC X(41) VALUE SPACES.                      
00666      05  FILLER      PIC X(05) VALUE 'PAGE'.                      
00667      05  HD3B-PAGE   PIC ZZ,ZZZ.                                  
00668                                                                   
00669  01  HEAD-4B.                                                     
00670      12  FILLER      PIC X(50)   VALUE                            
00671         ' CARR GROUP STATE ACCOUNT    CERTIFICATE EFF-DT   '.     
00672      12  FILLER      PIC X(55)   VALUE                            
00673         ' LIFE AMT       AH AMT                              L/A'.
00674      12  FILLER      PIC X(25)   VALUE                            
00675         '                         '.                              
00676      EJECT                                                        
00677  01  DETAIL-1B.                                                   
00678      12  FILLER      PIC X(02)   VALUE SPACES.                    
00679      12  DB-CARR     PIC X(01)   VALUE SPACES.                    
00680      12  FILLER      PIC X(03)   VALUE SPACES.                    
00681      12  DB-GROUP    PIC X(06)   VALUE SPACES.                    
00682      12  FILLER      PIC X(02)   VALUE SPACES.                    
00683      12  DB-STATE    PIC X(02)   VALUE SPACES.                    
00684      12  FILLER      PIC X(02)   VALUE SPACES.                    
00685      12  DB-ACCOUNT  PIC X(10)   VALUE SPACES.                    
00686      12  FILLER      PIC X(01)   VALUE SPACES.                    
00687      12  DB-CERT-NO  PIC X(11)   VALUE SPACES.                    
00688      12  FILLER      PIC X(01)   VALUE SPACES.                    
00689      12  DB-EFF-DATE PIC X(08)   VALUE SPACES.                    
00690      12  FILLER      PIC X(01)   VALUE SPACES.                    
00691      12  DB-CLAIML   PIC Z(9).99 VALUE ZEROS.                     
00692      12  FILLER      PIC X(03)   VALUE SPACES.                    
00693      12  DB-CLAIMA   PIC Z(6).99 VALUE ZEROS.                     
00694      12  FILLER      PIC X(02)   VALUE SPACES.                    
00695      12  FILLER      PIC X(03)   VALUE SPACES.                    
00696      12  FILLER      PIC X(04)   VALUE SPACES.                    
00697      12  FILLER      PIC X(03)   VALUE SPACES.                    
00698      12  FILLER      PIC X(05)   VALUE SPACES.                    
00699      12  FILLER      PIC X(10)   VALUE SPACES.                    
00700      12  FILLER      PIC X(03)   VALUE SPACES.                    
00701      12  DB-L-A      PIC X(01)   VALUE SPACES.                    
00702      12  FILLER      PIC X(05)   VALUE SPACES.                    
00703      12  FILLER      PIC X(10)   VALUE SPACES.                    
00704      12  FILLER      PIC X(01)   VALUE SPACES.                    
00705      12  FILLER      PIC X(10)   VALUE SPACES.                    
00706                                                                   
00707  01  DISPLAY-LINE.                                                
00708      12  FILLER             PIC X(05) VALUE SPACES.               
00709      12  DISPLAY-MESS       PIC X(30) VALUE SPACES.               
00710      12  FILLER             PIC X(03) VALUE SPACES.               
00711      12  DISPLAY-COUNT      PIC ZZ,ZZZ,ZZZ VALUE ZEROS.           
00712                                                                   
00713      EJECT                                                        
00714                                  COPY ELCDATE.                    
00715      EJECT                                                        
00716                                                                   
00717                                  COPY ELCDTECX.                   
00718                                                                   
00719                                  COPY ELCDTEVR.                   
00720                                                                   
00721                                  COPY ELCCRTVR.                   
00722                                                                   
00723                                  COPY ELCEXTVR.                   
00724                                                                   
00725                                  COPY ELCACCTV.                   
00726                                                                   
00727                                  COPY ELCPSEVR.                   
00728      EJECT                                                        
00729  PROCEDURE DIVISION.                                              
00730                                                                   
00731  0001-DT-CRD-READ SECTION.                                        
00732                                  COPY ELCDTERX.                   
00733                                                                   
00734      IF DEBUG-IS-ON                                               
00735          MOVE '3'                TO DTE-PGM-OPT.                  
00736                                                                   
00737      IF EP-DT = RUN-DATE                                          
00738         MOVE ALPH-DATE           TO HD-ALF-DTE                    
00739      ELSE                                                         
00740         MOVE EP-DT               TO DC-GREG-DATE-CYMD             
00741         MOVE 'L'                 TO DC-OPTION-CODE                
00742         PERFORM 6150-DATE-CONVERSION-ROUTINE                      
00743         IF NO-CONVERSION-ERROR                                    
00744            MOVE DC-GREG-DATE-1-ALPHA TO HD-ALF-DTE                
00745         ELSE                                                      
00746            MOVE 'ALPHA-DATE ERROR'   TO  WS-ABEND-MESSAGE         
00747            MOVE DC-ERROR-CODE        TO  WS-ABEND-FILE-STATUS     
00748            GO TO ABEND-PGM.                                       
00749                                                                   
00750      IF EP-DT = RUN-DATE                                          
00751         SUBTRACT 1 FROM RUN-CCYY.                                 
00752                                                                   
00753      MOVE EP-DT                      TO DC-GREG-DATE-CYMD.        
00754      MOVE 'L'                        TO DC-OPTION-CODE.           
00755      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
00756      IF NO-CONVERSION-ERROR                                       
00757         MOVE DC-BIN-DATE-1           TO END-DT (1)                
00758      ELSE                                                         
00759         MOVE SPACE TO DC-ERROR-CODE                               
00760         MOVE LOW-VALUES              TO END-DT (1).               
00761                                                                   
00762      MOVE END-DT (1)                 TO DC-BIN-DATE-1.            
00763      MOVE '6'                        TO DC-OPTION-CODE.           
00764      MOVE -12                        TO DC-ELAPSED-MONTHS.        
pemuni     move +0                         to dc-elapsed-days
00765      PERFORM 6150-DATE-CONVERSION-ROUTINE.
00766      IF NO-CONVERSION-ERROR                                       
00767         MOVE DC-BIN-DATE-2           TO END-DT (2)                
00768      ELSE                                                         
00769         MOVE SPACE TO DC-ERROR-CODE                               
00770         MOVE LOW-VALUES              TO END-DT (2).               
00771                                                                   
00772      MOVE END-DT (2)                 TO DC-BIN-DATE-1.            
00773      MOVE '6'                        TO DC-OPTION-CODE.           
00774      MOVE -12                        TO DC-ELAPSED-MONTHS.        
pemuni     move +0                         to dc-elapsed-days
00775      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
00776      IF NO-CONVERSION-ERROR                                       
00777         MOVE DC-BIN-DATE-2           TO END-DT (3)                
00778      ELSE                                                         
00779         MOVE SPACE TO DC-ERROR-CODE                               
00780         MOVE LOW-VALUES              TO END-DT (3).               
00781                                                                   
00782      MOVE +0                         TO DC-ELAPSED-MONTHS.        
00783                                                                   
CIDMOD*    MOVE BIN-RUN-DATE               TO DC-BIN-DATE-1.            
CIDMOD*    MOVE '6'                        TO DC-OPTION-CODE.           
CIDMOD*    MOVE -12                        TO DC-ELAPSED-MONTHS.        
CIDMOD*    PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
CIDMOD*    IF NO-CONVERSION-ERROR                                       
CIDMOD*       MOVE DC-BIN-DATE-2           TO BEGIN-DT (1)              
CIDMOD*    ELSE                                                         
CIDMOD*       MOVE SPACE TO DC-ERROR-CODE                               
CIDMOD*       MOVE LOW-VALUES              TO BEGIN-DT (1).             
00793                                                                   
CIDMOD     IF EP-DT = RUN-DATE                                          
CIDMOD        MOVE BIN-RUN-DATE               TO DC-BIN-DATE-1          
CIDMOD        MOVE '6'                        TO DC-OPTION-CODE         
CIDMOD        MOVE -12                        TO DC-ELAPSED-MONTHS      
pemuni        move +0                         to dc-elapsed-days
CIDMOD        PERFORM 6150-DATE-CONVERSION-ROUTINE                      
CIDMOD        IF NO-CONVERSION-ERROR                                    
CIDMOD          MOVE DC-BIN-DATE-2           TO BEGIN-DT (1)            
CIDMOD        ELSE                                                      
CIDMOD           MOVE SPACE TO DC-ERROR-CODE                            
CIDMOD           MOVE LOW-VALUES              TO BEGIN-DT (1)           
CIDMOD        END-IF
CIDMOD     ELSE
CIDMOD        MOVE BIN-RUN-DATE               TO BEGIN-DT (1)
CIDMOD     END-IF
CIDMOD
00794      MOVE BEGIN-DT (1)               TO DC-BIN-DATE-1.            
00795      MOVE '6'                        TO DC-OPTION-CODE.           
00796      MOVE -12                        TO DC-ELAPSED-MONTHS.        
pemuni     move +0                         to dc-elapsed-days
00797      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
00798      IF NO-CONVERSION-ERROR                                       
00799         MOVE DC-BIN-DATE-2           TO BEGIN-DT (2)              
00800      ELSE                                                         
00801         MOVE SPACE TO DC-ERROR-CODE                               
00802         MOVE LOW-VALUES              TO BEGIN-DT (2).             
00803                                                                   
00804      MOVE BEGIN-DT (2)               TO DC-BIN-DATE-1.            
00805      MOVE '6'                        TO DC-OPTION-CODE.           
00806      MOVE -12                        TO DC-ELAPSED-MONTHS.        
pemuni     move +0                         to dc-elapsed-days
00807      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
00808      IF NO-CONVERSION-ERROR                                       
00809         MOVE DC-BIN-DATE-2           TO BEGIN-DT (3)              
00810      ELSE                                                         
00811         MOVE SPACE TO DC-ERROR-CODE                               
00812         MOVE LOW-VALUES              TO BEGIN-DT (3).             
00813                                                                   
00814      MOVE +0                         TO DC-ELAPSED-MONTHS.        
00815                                                                   
00816      MOVE EP-MO TO ENDING-MO (1)                                  
00817                    ENDING-MO (2)                                  
00818                    ENDING-MO (3).                                 
00819      MOVE EP-DA TO ENDING-DA (1)                                  
00820                    ENDING-DA (2)                                  
00821                    ENDING-DA (3).                                 
00822                                                                   
00823      MOVE EP-CCYY      TO ENDING-CCYY (1).                        
00824      COMPUTE ENDING-CCYY (2) = (EP-CCYY - 1).                     
00825      COMPUTE ENDING-CCYY (3) = (EP-CCYY - 2).                     
00826                                                                   
00827      MOVE RUN-CCYY     TO BEGIN-CCYY (1).                         
00828      COMPUTE BEGIN-CCYY (2) = (RUN-CCYY - 1).                     
00829      COMPUTE BEGIN-CCYY (3) = (RUN-CCYY - 2).                     
00830                                                                   
00831      MOVE RUN-MO TO BEGIN-MO (1)                                  
00832                     BEGIN-MO (2)                                  
00833                     BEGIN-MO (3).                                 
00834      MOVE RUN-DA TO BEGIN-DA (1)                                  
00835                     BEGIN-DA (2)                                  
00836                     BEGIN-DA (3).                                 
00837                                                                   
00838      MOVE +3 TO NUMBER-OF-PERIODS.                                
00839                                                                   
00840      IF EP-SW = '1'                                               
00841          MOVE ZEROS TO BEGIN-DATE (3)                             
00842                        ENDING-DATE (3)                            
00843          MOVE 'Y'   TO WS-PROCESS-SW                              
00844          MOVE +2    TO NUMBER-OF-PERIODS.                         
00845                                                                   
00846      MOVE HIGH-VALUES     TO REIN-HOLD-AREAS.                     
00847      MOVE SPACES          TO REIN-LEVELS-END.                     
PEMTST     DISPLAY 'BEGIN 1 ' BEGIN-DATE (1)
PEMTST     DISPLAY 'BEGIN 2 ' BEGIN-DATE (2)
PEMTST     DISPLAY 'BEGIN 3 ' BEGIN-DATE (3)
PEMTST     DISPLAY ' END  1 ' ENDING-DATE (1)
PEMTST     DISPLAY ' END  2 ' ENDING-DATE (2)
PEMTST     DISPLAY ' END 3  ' ENDING-DATE (3)
00848                                                                   
00849      INITIALIZE  CALL-EXTRACT.                                    
00850                                                                   
00851      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
00852                                                                   
00853      MOVE +0 TO CLAS-INDEXS.                                      
00854                                                                   
00855      MOVE SPACES TO STATE-IL                                      
00856                     STATE-TX                                      
00857                     STATE-NC                                      
00858                     STATE-GA                                      
00859                     STATE-UT                                      
00860                     STATE-IA                                      
00861                     STATE-NE                                      
00862                     STATE-NV                                      
00863                     STATE-OH                                      
00864                     STATE-OR                                      
00865                     STATE-WI.                                     
00866                                                                   
00867  0070-FIND-TX.                                                    
00868                                                                   
00869      ADD +1 TO CLAS-INDEXS.                                       
00870                                                                   
00871      IF CLAS-INDEXS GREATER CLAS-MAXS                             
00872         GO TO 0080-SORT-CLAIMS.                                   
00873                                                                   
00874      IF STATE-ABBR (CLAS-INDEXS) = 'TX'                           
00875         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-TX                  
00876      ELSE                                                         
00877      IF STATE-ABBR (CLAS-INDEXS) = 'IL'                           
00878         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-IL                  
00879      ELSE                                                         
00880      IF STATE-ABBR (CLAS-INDEXS) = 'NC'                           
00881         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-NC                  
00882      ELSE                                                         
00883      IF STATE-ABBR (CLAS-INDEXS) = 'GA'                           
00884         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-GA                  
00885      ELSE                                                         
00886      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
00887         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-UT                  
00888      ELSE                                                         
00889      IF STATE-ABBR (CLAS-INDEXS) = 'IA'                           
00890         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-IA                  
00891      ELSE                                                         
00892      IF STATE-ABBR (CLAS-INDEXS) = 'NE'                           
00893         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-NE                  
00894      ELSE                                                         
00895      IF STATE-ABBR (CLAS-INDEXS) = 'NV'                           
00896         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-NV                  
00897      ELSE                                                         
00898      IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
00899         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-OH                  
00900      ELSE                                                         
00901      IF STATE-ABBR (CLAS-INDEXS) = 'OR'                           
00902         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-OR                  
00903      ELSE                                                         
00904      IF STATE-ABBR (CLAS-INDEXS) = 'WI'                           
00905         MOVE STATE-SUB (CLAS-INDEXS) TO STATE-WI.                 
00906                                                                   
00907      GO TO 0070-FIND-TX.                                          
00908                                                                   
00909  0080-SORT-CLAIMS.                                                
00910                                                                   
00911      SORT SORT-CLMS ON ASCENDING KEY SORT-CLAIM-KEY               
00912           INPUT PROCEDURE 0090-GET-ONLY-CLAIMS THRU               
00913                           0110-READ-CLAIMS-EXIT                   
00914           GIVING SORTED-CLAIMS.                                   
00915                                                                   
00916      IF SORT-RETURN NOT = ZEROES                                  
00917         MOVE +0101 TO WS-RETURN-CODE                              
00918         MOVE ' ERROR ON SORT ' TO WS-ABEND-MESSAGE                
00919         MOVE SPACES            TO WS-ABEND-FILE-STATUS            
00920         GO TO ABEND-PGM.                                          
00921                                                                   
00922      GO TO 0120-CERT-INPUTS-PROC.                                 
00923                                                                   
00924  0090-GET-ONLY-CLAIMS     SECTION.                                
00925                                                                   
00926      OPEN  INPUT CLMS-HIST.                                       
00927                                                                   
00928  0100-READ-CLAIMS.                                                
00929                                                                   
00930      READ CLMS-HIST INTO DETAIL-EXTRACT AT END                    
00931          CLOSE CLMS-HIST                                          
00932          GO TO 0110-READ-CLAIMS-EXIT.                             
00933                                                                   
           ADD  1 TO WS-CLMS-DB-CNT
                     WS-TOT-CLMS-DB-CNT
           IF WS-CLMS-DB-CNT > 10000
              MOVE ZEROS TO WS-CLMS-DB-CNT
              DISPLAY ' CLAIMS READ ' WS-TOT-CLMS-DB-CNT
           END-IF
00934      IF DE-REIN NOT = SPACE                                       
00935         GO TO 0100-READ-CLAIMS.                                   
00936                                                                   
00937      IF NOT DE-CLAIM                                              
00938         GO TO 0100-READ-CLAIMS.                                   
00939                                                                   
00940      COPY ELCEXTM1.                                               
00941                                                                   
00942      IF DE-CLM-PROC-DT NOT NUMERIC                                
00943         MOVE DE-PAY-MO    TO DE-CP-MO                             
00944         MOVE DE-PAY-DA    TO DE-CP-DA                             
00945         MOVE DE-PAY-YR    TO DE-CP-YR.                            
00946                                                                   
00947      MOVE DE-INCUR        TO  DC-GREG-DATE-CYMD.                  
00948      MOVE 'L'             TO  DC-OPTION-CODE.                     
00949      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
00950      IF NO-CONVERSION-ERROR                                       
00951         MOVE DC-BIN-DATE-1  TO  BIN-DE-INCUR                      
00952      ELSE                                                         
00953         MOVE 'DE-INCUR CONVERSION ERROR' TO                       
00954                                          WS-ABEND-MESSAGE         
00955         MOVE DC-ERROR-CODE  TO  WS-ABEND-FILE-STATUS              
00956         DISPLAY 'DE CONTROL 1 ' DE-CONTROL                        
00957         MOVE LOW-VALUES     TO  BIN-DE-INCUR.                     
00958 *       GO TO ABEND-PGM.                                          
00959                                                                   
00960      MOVE DE-CLM-PROC-DT  TO  DC-GREG-DATE-CYMD.                  
00961      MOVE 'L'             TO  DC-OPTION-CODE.                     
00962      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
00963      IF NO-CONVERSION-ERROR                                       
00964         MOVE DC-BIN-DATE-1  TO  BIN-DE-CLM-PROC-DT                
00965      ELSE                                                         
00966         MOVE 'DE-CLM-PROC-DT CONVERSION ERROR' TO                 
00967                                          WS-ABEND-MESSAGE         
00968         MOVE DC-ERROR-CODE  TO  WS-ABEND-FILE-STATUS              
00969         GO TO ABEND-PGM.                                          
00970                                                                   
00971      IF BIN-DE-CLM-PROC-DT GREATER THAN END-DT (1)                
00972         GO TO 0100-READ-CLAIMS.                                   
00973                                                                   
00974      IF SINGLE-PERIOD-PROCESS AND                                 
00975             BIN-DE-CLM-PROC-DT NOT GREATER THAN BEGIN-DT (2)      
00976              GO TO 0100-READ-CLAIMS                               
00977      ELSE                                                         
00978          IF BIN-DE-CLM-PROC-DT NOT GREATER THAN BEGIN-DT (3)      
00979              GO TO 0100-READ-CLAIMS.                              
00980                                                                   
00981      MOVE DE-CONTROL            TO SORT-CLAIM-KEY.                
00982                                                                   
00983      MOVE DETAIL-EXTRACT        TO SORT-EP-EXTR.                  
00984                                                                   
00985      RELEASE SORT-CLMS-REC.                                       
00986                                                                   
00987      GO TO 0100-READ-CLAIMS.                                      
00988                                                                   
00989  0110-READ-CLAIMS-EXIT.                                           
00990      EXIT.                                                        
00991      EJECT                                                        
00992                                                                   
00993  0120-CERT-INPUTS-PROC SECTION.                                   
00994                                                                   
00995      OPEN OUTPUT XTRACT.                                          
00996                                                                   
00997  0140-OPEN-REST.                                                  
00998                                                                   
00999      MOVE 'R' TO CP-RATE-FILE.                                    
01000      MOVE 'O' TO CP-IO-FUNCTION.                                  
01001      PERFORM 6130-CALL-RATING-ROUTINE THRU 6140-RATE-EXIT.        
01002      IF IO-ERROR                                                  
01003         MOVE +0302   TO WS-RETURN-CODE                            
01004         MOVE ' ERROR ON ERRATE OPEN ' TO WS-ABEND-MESSAGE         
01005         GO TO ABEND-PGM.                                          
01006                                                                   
01007      OPEN INPUT CERTS  ERACCTT SORTED-CLAIMS                      
01008           OUTPUT PRINTX.                                          
01009                                                                   
01010      IF DTE-PGM-OPT = '3' OR '4'                                  
01011         OPEN OUTPUT PRINTC.                                       
01012                                                                   
01013      IF ERACCTT-FILE-STATUS NOT = '00' AND '97'                   
01014         MOVE ' ERROR ON OPEN ERACCT ' TO WS-ABEND-MESSAGE         
01015         MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01016         GO TO ABEND-PGM.                                          
01017                                                                   
01018  0150-READ-ACCOUNT.                                               
01019                                                                   
01020      READ ERACCTT  NEXT RECORD.                                   
01021                                                                   
01022      IF ERACCTT-FILE-STATUS = '10'                                
01023         MOVE HIGH-VALUES TO AM-CONTROL-PRIMARY.                   
01024                                                                   
01025      IF ERACCTT-FILE-STATUS NOT = '00' AND '10'                   
01026         MOVE ' ERROR ON READ ERACCT ' TO WS-ABEND-MESSAGE         
01027         MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
01028         GO TO ABEND-PGM.                                          
01029                                                                   
           ADD  1 TO WS-ACCT-DB-CNT
                     WS-TOT-ACCT-DB-CNT
           IF WS-ACCT-DB-CNT > 10000
              MOVE ZEROS TO WS-ACCT-DB-CNT
              DISPLAY ' ACCTS  READ ' WS-TOT-ACCT-DB-CNT
           END-IF
01030      COPY ELCACCTI.                                               
01031                                                                   
01032  0160-READ-CERT-MASTER.                                           
01033                                                                   
01034      READ CERTS AT END                                            
01035           MOVE HIGH-VALUES TO CERT-IN-RECORD.                     
01036                                                                   
           ADD  1 TO WS-CERT-DB-CNT
                     WS-TOT-CERT-DB-CNT
           IF WS-CERT-DB-CNT > 10000
              MOVE ZEROS TO WS-CERT-DB-CNT
              DISPLAY ' CERT   READ ' WS-TOT-CERT-DB-CNT
           END-IF
01037       COPY ELCCRTM1.                                              
01038                                                                   
01039      IF CR-STATE NOT = HIGH-VALUES                                
01040          IF CR-STATE NOT = SAVE-STATE                             
01041            MOVE CR-STATE TO TEMP-STATE                            
01042            PERFORM 6200-LOCATE-STATE THRU 6200-EXIT.              
01043                                                                   
01044      IF CR-STATE NOT = HIGH-VALUES                                
01045         IF STATE-IS-EXCLUDED                                      
01046             GO TO 0160-READ-CERT-MASTER.                          
01047                                                                   
PEMTST     IF (CR-CARRIER = '9')
              AND (CR-STATE = 'NE')
              AND (CR-ACCOUNT = '0000015860')
              AND (CR-CERT-NO = '1098173790 ')
              DISPLAY ' BIG CERT BYPASSED ' CR-CERT-NO
              GO TO 0160-READ-CERT-MASTER
           END-IF

01048      IF CR-POLICY-IS-DECLINED OR
01049         CR-POLICY-IS-VOID                                         
01050             GO TO 0160-READ-CERT-MASTER.                          
01051                                                                   
01052      MOVE CR-FULL-CONTROL TO CERT-CONTROL.                        
01053                                                                   
01054  0170-READ-SORTED-CLAIMS.                                         
01055                                                                   
01056      READ SORTED-CLAIMS AT END                                    
01057        MOVE HIGH-VALUES TO SORTED-EXTRACT.                        
01058                                                                   
           ADD  1 TO WS-SCLM-DB-CNT
                     WS-TOT-SCLM-DB-CNT
           IF WS-SCLM-DB-CNT > 10000
              MOVE ZEROS TO WS-SCLM-DB-CNT
              DISPLAY ' SCLMS  READ ' WS-TOT-SCLM-DB-CNT
           END-IF
01059      MOVE SORTED-EXTRACT TO DETAIL-EXTRACT.                       
01060                                                                   
01061      MOVE DE-CONTROL TO CLAIMS-CONTROL.                           
01062                                                                   
01063      IF SORTED-EXTRACT NOT = HIGH-VALUES                          
01064         IF DE-STATE NOT = SAVE-STATE                              
01065            MOVE DE-STATE TO TEMP-STATE                            
01066            PERFORM 6200-LOCATE-STATE THRU 6200-EXIT.              
01067                                                                   
01068      IF SORTED-EXTRACT NOT = HIGH-VALUES                          
01069         IF STATE-IS-EXCLUDED                                      
01070             GO TO 0170-READ-SORTED-CLAIMS.                        
01071                                                                   
01072      IF SORTED-EXTRACT = HIGH-VALUES                              
01073          GO TO 0230-PROCESS-CLAIMS-EXIT.                          
01074                                                                   
01075      COPY   ELCEXTM1.                                             
01076                                                                   
01077      IF DE-CLM-PROC-DT NOT NUMERIC                                
01078         MOVE DE-PAY-MO    TO DE-CP-MO                             
01079         MOVE DE-PAY-DA    TO DE-CP-DA                             
01080         MOVE DE-PAY-YR    TO DE-CP-YR.                            
01081                                                                   
01082      MOVE DE-INCUR        TO  DC-GREG-DATE-CYMD.                  
01083      MOVE 'L'             TO  DC-OPTION-CODE.                     
01084      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01085      IF NO-CONVERSION-ERROR                                       
01086         MOVE DC-BIN-DATE-1  TO  BIN-DE-INCUR                      
01087      ELSE                                                         
01088         MOVE 'DE-INCUR CONVERSION ERROR' TO                       
01089                                          WS-ABEND-MESSAGE         
01090         MOVE DC-ERROR-CODE  TO  WS-ABEND-FILE-STATUS              
01091         DISPLAY 'CLAIMS CONTROL 2 ' CLAIMS-CONTROL                
01092         MOVE LOW-VALUES     TO  BIN-DE-INCUR.                     
01093 *       GO TO ABEND-PGM.                                          
01094                                                                   
01095      MOVE DE-CLM-PROC-DT  TO  DC-GREG-DATE-CYMD.                  
01096      MOVE 'L'             TO  DC-OPTION-CODE.                     
01097      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01098      IF NO-CONVERSION-ERROR                                       
01099         MOVE DC-BIN-DATE-1  TO  BIN-DE-CLM-PROC-DT                
01100      ELSE                                                         
01101         MOVE 'DE-CLM-PROC-DT CONVERSION ERROR' TO                 
01102                                          WS-ABEND-MESSAGE         
01103         MOVE DC-ERROR-CODE  TO  WS-ABEND-FILE-STATUS              
01104         GO TO ABEND-PGM.                                          
01105                                                                   
01106      EJECT                                                        
01107  0180-MATCH-ROUTINE.                                              
01108                                                                   
01109      IF CERT-CONTROL = HIGH-VALUES                                
01110         IF CLAIMS-CONTROL = HIGH-VALUES                           
01111            GO TO 6100-E-O-J-RTN.                                  
01112                                                                   
01113      IF CLAIMS-CONTROL LESS THAN CERT-CONTROL                     
01114         MOVE '1' TO WS-MATCH-SWITCH                               
01115         PERFORM 0190-PROCESS-CLAIMS THRU 0230-PROCESS-CLAIMS-EXIT 
01116         PERFORM 1500-RELEASE THRU 1540-RELEASE-EXIT               
01117         GO TO 0180-MATCH-ROUTINE.                                 
01118                                                                   
01119      IF CLAIMS-CONTROL GREATER THAN CERT-CONTROL                  
01120         MOVE '2' TO WS-MATCH-SWITCH                               
01121         PERFORM 0240-PROCESS-CERTS THRU 1410-PROCESS-CERTS-EXIT   
01122         PERFORM 0160-READ-CERT-MASTER                             
01123         GO TO 0180-MATCH-ROUTINE.                                 
01124                                                                   
01125      MOVE '3' TO WS-MATCH-SWITCH.                                 
01126      PERFORM 0190-PROCESS-CLAIMS THRU 0230-PROCESS-CLAIMS-EXIT.   
01127      PERFORM 0240-PROCESS-CERTS THRU 1410-PROCESS-CERTS-EXIT.     
01128      PERFORM 0160-READ-CERT-MASTER.                               
01129                                                                   
01130      GO TO 0180-MATCH-ROUTINE.                                    
01131                                                                   
01132      EJECT                                                        
01133  0190-PROCESS-CLAIMS.                                             
01134                                                                   
01135      INITIALIZE  CALL-EXTRACT.                                    
01136                                                                   
01137      MOVE LOW-VALUES          TO HOLD-CLAIMS-CONTROL.             
01138                                                                   
01139      MOVE ZEROS TO WS-DEV (1)      WS-DEV (2)                     
01140                    WS-BEN-TYPE (1) WS-BEN-TYPE (2)                
01141                    WS-TERM (1)     WS-TERM (2).                   
01142                                                                   
01143      PERFORM 0200-ZERO-CLAIM-TABLE VARYING                        
01144              SB FROM +1 BY +1 UNTIL SB GREATER THAN +2 AFTER      
01145              YR FROM +1 BY +1 UNTIL YR GREATER THAN +3.           
01146                                                                   
01147      GO TO 0210-CONTINUE-CLAIMS.                                  
01148                                                                   
01149  0200-ZERO-CLAIM-TABLE.                                           
01150                                                                   
01151      MOVE +0         TO WS-CLAIM-AMT (1 SB YR)                    
01152                         WS-CLAIM-AMT (2 SB YR)                    
01153                         WS-INC-CNT (1 SB YR)                      
01154                         WS-PD-CNT  (1 SB YR)                      
01155                         WS-INC-CNT (2 SB YR)                      
01156                         WS-PD-CNT  (2 SB YR).                     
01157                                                                   
01158  0210-CONTINUE-CLAIMS.                                            
01159                                                                   
01160      MOVE +1         TO LIFE-AH-SUB.                              
01161                                                                   
01162      IF CERT-AND-CLAIM                                            
01163         GO TO 0220-CLAIMS-LOOP.                                   
01164                                                                   
01165      MOVE CLAIMS-CONTROL      TO ACCT-MATCH-CONTROL.              
01166      MOVE DE-EFF              TO ACCT-MATCH-EFF.                  
01167                                                                   
01168      MOVE '1'                 TO WS-ACCT-MATCH.                   
01169      PERFORM 0250-MATCH-TO-ACCOUNT THRU                           
01170              0260-MATCH-ACCT-EXIT.                                
01171                                                                   
01172      MOVE DE-CARRIER          TO EX-CARRIER.                      
01173      MOVE DE-GROUPING         TO EX-GROUPING.                     
01174      MOVE DE-STATE            TO EX-STATE.                        
01175      MOVE DE-ACCOUNT          TO EX-ACCOUNT.                      
01176      MOVE '2'                 TO EX-RECORD-TYPE.                  
01177      MOVE '0'                 TO EX-TERM-CD.                      
01178      MOVE '000'               TO EX-STATE-DEV.                    
01179                                                                   
01180      IF DE-DTH OR DE-OB-DTH                                       
01181         MOVE 'L' TO EX-LIFE-AH                                    
01182      ELSE                                                         
01183         MOVE 'A' TO EX-LIFE-AH.                                   
01184                                                                   
01185      IF DE-LF-TERM NOT NUMERIC                                    
01186         MOVE ZEROS            TO DE-LF-TERM.                      
01187                                                                   
01188      IF DE-AH-TERM NOT NUMERIC                                    
01189         MOVE ZEROS            TO DE-AH-TERM.                      
01190                                                                   
01191      IF DE-DTH OR DE-OB-DTH                                       
01192         IF DE-LF-TERM LESS THAN +60                               
01193            MOVE '1' TO EX-TERM-CD                                 
01194         ELSE                                                      
01195            MOVE '2' TO EX-TERM-CD                                 
01196      ELSE                                                         
01197         IF DE-AH-TERM LESS THAN +60                               
01198            MOVE '1' TO EX-TERM-CD                                 
01199         ELSE                                                      
01200            MOVE '2' TO EX-TERM-CD.                                
01201                                                                   
01202      IF DE-IG = '1' OR 'I'                                        
01203         MOVE 'I'              TO EX-I-G                           
01204      ELSE                                                         
01205         MOVE 'G'              TO EX-I-G.                          
01206                                                                   
01207      MOVE '00'                TO EX-CAL-TABLE.                    
01208      MOVE '01'                TO EX-ACCT-TYPE.                    
01209                                                                   
01210      MOVE DE-ACC-NAME         TO EX-NAME.                         
01211                                                                   
01212      IF ACCOUNT-MATCHED                                           
01213         MOVE AM-NAME          TO EX-NAME                          
01214         MOVE AM-GPCD          TO EX-ACCT-TYPE.                    
01215                                                                   
01216      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
01217         MOVE CR-RATING-CLASS  TO EX-CAL-TABLE                     
01218      ELSE                                                         
01219         MOVE AM-CAL-TABLE     TO EX-CAL-TABLE.                    
01220                                                                   
01221  0220-CLAIMS-LOOP.                                                
01222                                                                   
01223      IF CLAIMS-CONTROL NOT = HOLD-CLAIMS-CONTROL AND              
01224         HOLD-CLAIMS-CONTROL NOT = LOW-VALUES                      
01225            MOVE CLAIMS-CONTROL      TO HOLD-CLAIMS-CONTROL        
01226            GO TO 0230-PROCESS-CLAIMS-EXIT.                        
01227                                                                   
01228      MOVE CLAIMS-CONTROL      TO HOLD-CLAIMS-CONTROL.             
01229                                                                   
01230      IF DE-DTH OR DE-OB-DTH                                       
01231         MOVE +1   TO LIFE-AH-SUB                                  
01232      ELSE                                                         
01233         MOVE +2   TO LIFE-AH-SUB.                                 
01234                                                                   
01235      IF DE-LF-TERM NOT NUMERIC                                    
01236         MOVE ZEROS            TO DE-LF-TERM.                      
01237                                                                   
01238      IF DE-AH-TERM NOT NUMERIC                                    
01239         MOVE ZEROS            TO DE-AH-TERM.                      
01240                                                                   
01241      IF DE-DTH OR DE-OB-DTH                                       
01242         MOVE DE-LF-TYPE       TO WS-BEN-TYPE (LIFE-AH-SUB)        
01243         MOVE DE-LF-TERM       TO WS-TERM     (LIFE-AH-SUB)        
01244      ELSE                                                         
01245         MOVE DE-AH-TYPE       TO WS-BEN-TYPE (LIFE-AH-SUB)        
01246         MOVE DE-AH-TERM       TO WS-TERM     (LIFE-AH-SUB).       
01247                                                                   
01248      IF AM-GPCD NOT NUMERIC                                       
01249          MOVE ZEROS TO AM-GPCD.                                   
01250                                                                   
01251      IF ACCOUNT-MATCHED                                           
01252          IF AM-GPCD NOT = SAVE-BUSC-TYPE                          
01253              PERFORM 6300-LOCATE-BUSC-TYPE THRU 6399-EXIT.        
01254                                                                   
01255      IF ACCOUNT-MATCHED                                           
01256          IF BUSC-TYPE-IS-EXCLUDED                                 
01257              GO TO 0225-CONTINUE-CLAIMS.                          
01258                                                                   
01259      IF ACCOUNT-MATCHED                                           
01260         IF DE-AH OR DE-OB-AH                                      
01261            MOVE AM-AH-DEVIATION TO WS-DEV (LIFE-AH-SUB)           
01262         ELSE                                                      
01263            MOVE AM-LF-DEVIATION TO WS-DEV (LIFE-AH-SUB).          
01264                                                                   
01265      IF NOT SINGLE-PERIOD-PROCESS                                 
01266          GO TO 0222-CONTINUE.                                     
01267                                                                   
01268      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (1)            
01269        AND                                                        
01270         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (1)              
01271          ADD DE-CLAIM-AMT      TO WS-CLAIM-AMT (LIFE-AH-SUB 1 1)  
01272      ELSE                                                         
01273      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (2)            
01274        AND                                                        
01275         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (2)              
01276         ADD DE-CLAIM-AMT       TO WS-CLAIM-AMT (LIFE-AH-SUB 1 2). 
01277                                                                   
01278      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (1)            
01279        AND                                                        
01280         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (1)              
01281         IF DE-VOIDED-PAYMENT                                      
01282             SUBTRACT +1 FROM WS-PD-CNT (LIFE-AH-SUB 1 1)          
01283         ELSE                                                      
01284            IF DE-CLAIM-AMT NOT = ZEROS                            
01285                ADD +1 TO WS-PD-CNT (LIFE-AH-SUB 1 1)              
01286            ELSE                                                   
01287                NEXT SENTENCE                                      
01288      ELSE                                                         
01289      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (2)            
01290        AND                                                        
01291         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (2)              
01292         IF DE-VOIDED-PAYMENT                                      
01293             SUBTRACT +1 FROM WS-PD-CNT (LIFE-AH-SUB 1 2)          
01294         ELSE                                                      
01295            IF DE-CLAIM-AMT NOT = ZEROS                            
01296                ADD +1 TO WS-PD-CNT (LIFE-AH-SUB 1 2).             
01297                                                                   
01298      IF BIN-DE-INCUR NOT GREATER THAN END-DT (1)                  
01299        AND                                                        
01300         BIN-DE-INCUR GREATER THAN BEGIN-DT (1)                    
01301         MOVE +1                TO WS-INC-CNT (LIFE-AH-SUB 1 1)    
01302      ELSE                                                         
01303      IF BIN-DE-INCUR NOT GREATER THAN END-DT (2)                  
01304        AND                                                        
01305         BIN-DE-INCUR GREATER THAN BEGIN-DT (2)                    
01306         MOVE +1             TO WS-INC-CNT (LIFE-AH-SUB 1 2).      
01307                                                                   
01308      GO TO 0225-CONTINUE-CLAIMS.                                  
01309                                                                   
01310  0222-CONTINUE.                                                   
01311      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (1)            
01312        AND                                                        
01313         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (1)              
01314         ADD DE-CLAIM-AMT       TO WS-CLAIM-AMT (LIFE-AH-SUB 1 1)  
01315      ELSE                                                         
01316      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (2)            
01317        AND                                                        
01318         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (2)              
01319         ADD DE-CLAIM-AMT       TO WS-CLAIM-AMT (LIFE-AH-SUB 1 2)  
01320      ELSE                                                         
01321      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (3)            
01322        AND                                                        
01323         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (3)              
01324         ADD DE-CLAIM-AMT       TO WS-CLAIM-AMT (LIFE-AH-SUB 1 3). 
01325                                                                   
01326      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (1)            
01327        AND                                                        
01328         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (1)              
01329         IF DE-VOIDED-PAYMENT                                      
01330             SUBTRACT +1 FROM WS-PD-CNT (LIFE-AH-SUB 1 1)          
01331         ELSE                                                      
01332            IF DE-CLAIM-AMT NOT = ZEROS                            
01333                ADD +1 TO WS-PD-CNT (LIFE-AH-SUB 1 1)              
01334            ELSE                                                   
01335                NEXT SENTENCE                                      
01336      ELSE                                                         
01337      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (2)            
01338        AND                                                        
01339         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (2)              
01340         IF DE-VOIDED-PAYMENT                                      
01341             SUBTRACT +1 FROM WS-PD-CNT (LIFE-AH-SUB 1 2)          
01342         ELSE                                                      
01343            IF DE-CLAIM-AMT NOT = ZEROS                            
01344                ADD +1 TO WS-PD-CNT (LIFE-AH-SUB 1 2)              
01345            ELSE                                                   
01346                NEXT SENTENCE                                      
01347      ELSE                                                         
01348      IF BIN-DE-CLM-PROC-DT NOT GREATER THAN END-DT (3)            
01349        AND                                                        
01350         BIN-DE-CLM-PROC-DT GREATER THAN BEGIN-DT (3)              
01351         IF DE-VOIDED-PAYMENT                                      
01352             SUBTRACT +1 FROM WS-PD-CNT (LIFE-AH-SUB 1 3)          
01353         ELSE                                                      
01354            IF DE-CLAIM-AMT NOT = ZEROS                            
01355                ADD +1 TO WS-PD-CNT (LIFE-AH-SUB 1 3).             
01356                                                                   
01357      IF BIN-DE-INCUR NOT GREATER THAN END-DT (1)                  
01358        AND                                                        
01359         BIN-DE-INCUR GREATER THAN BEGIN-DT (1)                    
01360         MOVE +1                TO WS-INC-CNT (LIFE-AH-SUB 1 1)    
01361      ELSE                                                         
01362      IF BIN-DE-INCUR NOT GREATER THAN END-DT (2)                  
01363        AND                                                        
01364         BIN-DE-INCUR GREATER THAN BEGIN-DT (2)                    
01365         MOVE +1             TO WS-INC-CNT (LIFE-AH-SUB 1 2)       
01366      ELSE                                                         
01367      IF BIN-DE-INCUR NOT GREATER THAN END-DT (3)                  
01368        AND                                                        
01369         BIN-DE-INCUR GREATER THAN BEGIN-DT (3)                    
01370         MOVE +1          TO WS-INC-CNT (LIFE-AH-SUB 1 3).         
01371                                                                   
01372  0225-CONTINUE-CLAIMS.                                            
01373                                                                   
01374      PERFORM 0170-READ-SORTED-CLAIMS.                             
01375                                                                   
01376      GO TO 0220-CLAIMS-LOOP.                                      
01377                                                                   
01378  0230-PROCESS-CLAIMS-EXIT.                                        
01379      EXIT.                                                        
01380                                                                   
01381      EJECT                                                        
01382  0240-PROCESS-CERTS.                                              
01383                                                                   
01384      IF CR-LF-CANC-DT NOT NUMERIC                                 
01385         MOVE ZEROS      TO CR-LF-CANC-DT.                         
01386                                                                   
01387      IF CR-LF-CANCEL-EXIT-DATE NOT NUMERIC                        
01388         MOVE ZEROS      TO CR-LF-CANCEL-EXIT-DATE.                
01389                                                                   
01390      IF CR-AH-CANC-DT NOT NUMERIC                                 
01391         MOVE ZEROS      TO CR-AH-CANC-DT.                         
01392                                                                   
01393      IF CR-AH-CANCEL-EXIT-DATE NOT NUMERIC                        
01394         MOVE ZEROS      TO CR-AH-CANCEL-EXIT-DATE.                
01395                                                                   
01396      IF CR-DTH-DT NOT NUMERIC                                     
01397         MOVE ZEROS      TO CR-DTH-DT.                             
01398                                                                   
01399      IF CR-LF-CLAIM-EXIT-DATE  NOT NUMERIC                        
01400         MOVE ZEROS      TO CR-LF-CLAIM-EXIT-DATE.                 
01401                                                                   
01402      IF CR-DTH-DT NOT = ZEROS                                     
01403          IF CR-LF-CLAIM-EXIT-DATE = ZEROS                         
01404              IF CR-LF-CURRENT-STATUS = '7'                        
01405                  MOVE CR-DTH-DT  TO CR-LF-CLAIM-EXIT-DATE.        
01406                                                                   
01407      IF CR-LF-CANC-DT NOT = ZEROS                                 
01408          IF CR-LF-CANCEL-EXIT-DATE = ZEROS                        
01409              MOVE CR-LF-CANC-DT TO CR-LF-CANCEL-EXIT-DATE.        
01410                                                                   
01411      IF CR-AH-CANC-DT NOT = ZEROS                                 
01412          IF CR-AH-CANCEL-EXIT-DATE = ZEROS                        
01413              MOVE CR-AH-CANC-DT TO CR-AH-CANCEL-EXIT-DATE.        
01414                                                                   
01415      IF CERT-NO-CLAIM                                             
01416         MOVE ZEROS TO WS-DEV (1)      WS-DEV (2)                  
01417                       WS-BEN-TYPE (1) WS-BEN-TYPE (2)             
01418                       WS-TERM (1)     WS-TERM (2)                 
01419                                                                   
01420         PERFORM 0200-ZERO-CLAIM-TABLE VARYING                     
01421              SB FROM +1 BY +1 UNTIL SB GREATER THAN +2 AFTER      
01422              YR FROM +1 BY +1 UNTIL YR GREATER THAN +3.           
01423                                                                   
01424      MOVE +1                  TO  LIFE-AH-SUB    YR.              
01425                                                                   
01426      INITIALIZE   CALL-EXTRACT.                                   
01427                                                                   
01428      MOVE CALL-EXTRACT        TO HOLD-LIFE-RECORD                 
01429                                  HOLD-AH-RECORD.                  
01430                                                                   
01431      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01432      MOVE 'L'                        TO DC-OPTION-CODE.           
01433      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01434      IF NO-CONVERSION-ERROR                                       
01435         MOVE DC-BIN-DATE-1            TO EFFECT-DT                
01436      ELSE                                                         
01437         MOVE LOW-VALUES               TO EFFECT-DT.               
01438                                                                   
01439      MOVE CR-ENTRY-DATE              TO DC-GREG-DATE-CYMD.        
01440      MOVE 'L'                        TO DC-OPTION-CODE.           
01441      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01442      IF NO-CONVERSION-ERROR                                       
01443         MOVE DC-BIN-DATE-1            TO ENTRY-DT                 
01444      ELSE                                                         
01445         MOVE EFFECT-DT                TO ENTRY-DT.                
01446                                                                   
01447      IF ENTRY-DT GREATER THAN END-DT (1)                          
01448         GO TO 1410-PROCESS-CERTS-EXIT.                            
01449                                                                   
01450      MOVE CERT-CONTROL TO ACCT-MATCH-CONTROL.                     
01451      MOVE CR-DT        TO ACCT-MATCH-EFF.                         
01452      MOVE '2'          TO WS-ACCT-MATCH.                          
01453                                                                   
01454      PERFORM 0250-MATCH-TO-ACCOUNT THRU                           
01455              0260-MATCH-ACCT-EXIT.                                
01456                                                                   
01457      GO TO 0265-CONTINUE-CERT.                                    
01458                                                                   
01459  0250-MATCH-TO-ACCOUNT.                                           
01460                                                                   
01461      MOVE ' ' TO WS-ACCT-MATCH-SW.                                
01462                                                                   
01463      IF ACCT-MATCH-CONTROL GREATER THAN AM-CONTROL-A              
01464         PERFORM 0150-READ-ACCOUNT                                 
01465         GO TO 0250-MATCH-TO-ACCOUNT.                              
01466                                                                   
01467      IF ACCT-MATCH-CONTROL LESS THAN AM-CONTROL-A                 
01468         MOVE +0302 TO WS-RETURN-CODE                              
01469         IF FROM-CLAIM                                             
01470            MOVE 'NO ACCOUNT MASTER FOR CLAIM ' TO                 
01471                  WS-ABEND-MESSAGE                                 
01472            DISPLAY 'CLAIM CONTROL ' ACCT-MATCH-CONTROL            
01473            DISPLAY 'CLAIM EFF DTE ' ACCT-MATCH-EFF                
01474            GO TO 0260-MATCH-ACCT-EXIT                             
01475         ELSE                                                      
01476            MOVE 'NO ACCOUNT MASTER FOR CERT  ' TO                 
01477                  WS-ABEND-MESSAGE                                 
01478            DISPLAY 'CERT  CONTROL ' ACCT-MATCH-CONTROL            
01479            DISPLAY 'CERT  EFF DTE ' ACCT-MATCH-EFF                
01480            GO TO 0260-MATCH-ACCT-EXIT.                            
01481                                                                   
01482      IF ACCT-MATCH-EFF GREATER THAN WS-AM-EXPIRE-DT               
01483         PERFORM 0150-READ-ACCOUNT                                 
01484         GO TO 0250-MATCH-TO-ACCOUNT.                              
01485                                                                   
01486      MOVE '*' TO WS-ACCT-MATCH-SW.                                
01487                                                                   
01488  0260-MATCH-ACCT-EXIT.                                            
01489      EXIT.                                                        
01490                                                                   
01491      EJECT                                                        
01492  0265-CONTINUE-CERT.                                              
01493                                                                   
01494      IF CR-LFPRM-CALC NOT NUMERIC                                 
01495         MOVE +0    TO CR-LFPRM-CALC.                              
01496                                                                   
01497      IF CR-AHPRM-CALC NOT NUMERIC                                 
01498         MOVE +0    TO CR-AHPRM-CALC.                              
01499                                                                   
01500      IF CR-LFRFND-CALC NOT NUMERIC                                
01501         MOVE +0    TO CR-LFRFND-CALC.                             
01502                                                                   
01503      IF CR-AHRFND-CALC NOT NUMERIC                                
01504         MOVE +0    TO CR-AHRFND-CALC.                             
01505                                                                   
01506      IF CR-APR NOT NUMERIC                                        
01507         MOVE +0    TO CR-APR.                                     
01508                                                                   
01509      IF CR-LF-DEV-CODE = SPACES OR LOW-VALUES                     
01510         MOVE ZEROS TO CR-LF-DEV-CODE.                             
01511                                                                   
01512      IF CR-AH-DEV-CODE = SPACES OR LOW-VALUES                     
01513         MOVE ZEROS TO CR-AH-DEV-CODE.                             
01514                                                                   
01515      IF DTE-CLIENT = 'CRI'                                        
01516          MOVE CR-LF-DEV-CODE     TO  WK-LF-DEV-CODE               
01517          MOVE 'P'                TO  WK-LF-DEV-CODE-1             
01518          MOVE '0'                TO  WK-LF-DEV-CODE-3             
01519          MOVE CR-AH-DEV-CODE     TO  WK-AH-DEV-CODE               
01520          MOVE 'P'                TO  WK-AH-DEV-CODE-1             
01521          MOVE '0'                TO  WK-AH-DEV-CODE-3.            
01522                                                                   
01523      MOVE CR-CARRIER        TO EX-CARRIER.                        
01524      MOVE CR-GROUPING       TO EX-GROUPING.                       
01525      MOVE CR-STATE          TO EX-STATE.                          
01526      MOVE CR-ACCOUNT        TO EX-ACCOUNT                         
01527                                                                   
01528      IF CR-IND-GRP = '1' OR 'I'                                   
01529         MOVE 'I'            TO EX-I-G                             
01530      ELSE                                                         
01531         MOVE 'G'            TO EX-I-G.                            
01532                                                                   
01533      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
01534         MOVE CR-RATING-CLASS  TO EX-CAL-TABLE                     
01535      ELSE                                                         
01536         MOVE AM-CAL-TABLE     TO EX-CAL-TABLE.                    
01537                                                                   
01538      IF ACCOUNT-MATCHED                                           
01539         MOVE AM-GPCD           TO EX-ACCT-TYPE                    
01540         MOVE AM-NAME           TO EX-NAME.                        
01541                                                                   
01542      IF ACCOUNT-MATCHED                                           
01543          IF AM-GPCD NOT = SAVE-BUSC-TYPE                          
01544              PERFORM 6300-LOCATE-BUSC-TYPE THRU 6399-EXIT.        
01545                                                                   
01546      IF ACCOUNT-MATCHED                                           
01547          IF BUSC-TYPE-IS-EXCLUDED                                 
01548              GO TO 1410-PROCESS-CERTS-EXIT.                       
01549                                                                   
01550      MOVE CR-DT             TO EX-EFF-DATE.                       
01551      MOVE CR-AGE            TO EX-AGE.                            
01552      MOVE CR-ENTRY-DATE     TO EX-ENTRY-DATE.                     
01553      MOVE CR-PMT-FREQ       TO EX-PMT-FREQ.                       
01554      MOVE CR-APR            TO EX-APR.                            
01555      MOVE CR-LOAN-TERM      TO EX-CAP-TERM.                       
01556      MOVE CR-LFAMT          TO EX-INITIAL-AMT.                    
01557      MOVE CR-AHAMT          TO EX-BENEFIT.                        
01558      MOVE '3'               TO EX-RECORD-TYPE.                    
01559                                                                   
01560      MOVE CLAS-STARTL TO CLAS-INDEXL                              
01561      MOVE CLAS-STARTA TO CLAS-INDEXA.                             
01562      MOVE CLAS-STARTS TO CLAS-INDEXS.                             
01563                                                                   
01564      MOVE WS-RESERVE-INIT   TO WS-RESERVE-WORK (1)                
01565                                WS-RESERVE-WORK (2)                
01566                                WS-RESERVE-WORK (3).               
01567                                                                   
01568      MOVE WS-REM-TERM-INIT  TO WS-REM-TERM-WORK (1)               
01569                                WS-REM-TERM-WORK (2)               
01570                                WS-REM-TERM-WORK (3).              
01571                                                                   
01572      MOVE LOW-VALUES TO LF-CAN LF-CLM                             
01573                         AH-CAN AH-CLM.                            
01574                                                                   
01575  0270-FIND-STATE.                                                 
01576                                                                   
01577      IF CLAS-INDEXS GREATER THAN CLAS-MAXS                        
01578         MOVE ' INVALID STATE ' TO WS-ABEND-MESSAGE                
01579         MOVE CR-STATE          TO WS-ABEND-FILE-STATUS            
01580         MOVE +0                TO WS-RETURN-CODE                  
01581         GO TO ABEND-PGM.                                          
01582                                                                   
01583      IF STATE-PRIM-FAC-DEV (CLAS-INDEXS) = SPACES OR LOW-VALUES   
01584         MOVE ZEROS TO STATE-PRIM-FAC-DEV (CLAS-INDEXS).           
01585                                                                   
01586      IF STATE-SUB (CLAS-INDEXS) NOT = CR-STATE                    
01587         ADD +1 TO CLAS-INDEXS                                     
01588         GO TO 0270-FIND-STATE.                                    
01589                                                                   
01590  0280-PROCESS-LIFE.                                               
01591                                                                   
01592      IF CR-LFTYP = ZEROS OR SPACES                                
01593          GO TO 1000-PROCESS-AH.                                   
01594                                                                   
01595      IF CR-LF-DEV-PCT NOT NUMERIC                                 
01596          MOVE ZEROS TO CR-LF-DEV-PCT.                             
01597                                                                   
01598      IF DTE-CLIENT  =  'CRI'                                      
01599          MOVE WK-LF-DEV-CODE      TO  EX-STATE-DEV                
01600      ELSE                                                         
01601          IF CR-LF-DEV-PCT NOT = +0 AND +1.0                       
01602              COMPUTE WK-TEMP-DEV-PCT =                            
01603                              CR-LF-DEV-PCT * WK-DEV-FACTOR        
01604              MOVE WK-TEMP-DEV     TO  EX-STATE-DEV                
01605          ELSE                                                     
01606              MOVE CR-LF-DEV-CODE  TO  EX-STATE-DEV.               
01607                                                                   
01608      MOVE 'L'             TO EX-LIFE-AH.                          
01609                                                                   
01610  0290-CHECK-LF-STATUS.                                            

032609     MOVE ZEROS                  TO EX-EXIT-DATE
032609     MOVE LOW-VALUES             TO EXIT-DT
032609                                    EXPIRE-DT

01612      IF CR-LF-CURRENT-STATUS NOT = '6' AND '7' AND '8'            
01613         GO TO 0300-FIND-LIFE-LOOP.                                
01614                                                                   
01615      MOVE CR-LF-CANCEL-EXIT-DATE     TO DC-GREG-DATE-CYMD.        
01616      MOVE 'L'                        TO DC-OPTION-CODE.           
01617      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01618      IF NO-CONVERSION-ERROR                                       
01619         MOVE DC-BIN-DATE-1            TO LF-CAN                   
01620      ELSE                                                         
01621         MOVE LOW-VALUES               TO LF-CAN.                  
01622                                                                   
01623      MOVE CR-LF-CLAIM-EXIT-DATE      TO DC-GREG-DATE-CYMD.        
01624      MOVE 'L'                        TO DC-OPTION-CODE.           
01625      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01626      IF NO-CONVERSION-ERROR                                       
01627         MOVE DC-BIN-DATE-1            TO LF-CLM                   
01628      ELSE                                                         
01629         MOVE LOW-VALUES               TO LF-CLM.                  
01630                                                                   
01631      IF LF-CAN = LOW-VALUES AND                                   
01632         LF-CLM = LOW-VALUES                                       
01633         GO TO 0300-FIND-LIFE-LOOP.                                
01634                                                                   
01635      IF LF-CAN = LOW-VALUES AND                                   
01636         LF-CLM NOT = LOW-VALUES                                   
01637         MOVE CR-LF-CLAIM-EXIT-DATE TO EX-EXIT-DATE                
01638         GO TO 0300-FIND-LIFE-LOOP.                                
01639                                                                   
01640      IF LF-CLM = LOW-VALUES                                       
01641         MOVE CR-LF-CANCEL-EXIT-DATE TO EX-EXIT-DATE               
01642         GO TO 0300-FIND-LIFE-LOOP.                                
01643                                                                   
01644      IF LF-CAN LESS THAN LF-CLM                                   
01645         MOVE CR-LF-CANCEL-EXIT-DATE TO EX-EXIT-DATE               
01646         GO TO 0300-FIND-LIFE-LOOP.                                
01647                                                                   
01648      MOVE CR-LF-CLAIM-EXIT-DATE TO EX-EXIT-DATE.                  
01649                                                                   
01650  0300-FIND-LIFE-LOOP.                                             
01651                                                                   
01652      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        
01653         MOVE ' INVALID LIFE TYPE ' TO WS-ABEND-MESSAGE            
01654         MOVE CR-LFTYP TO WS-ABEND-FILE-STATUS                     
01655         GO TO ABEND-PGM.                                          
01656                                                                   
01657      IF CR-LFTYP NOT = CLAS-I-BEN (CLAS-INDEXL)                   
01658         ADD +1 TO CLAS-INDEXL                                     
01659         GO TO 0300-FIND-LIFE-LOOP.                                
01660                                                                   
01661      MOVE CLAS-I-EP (CLAS-INDEXL) TO LF-EARN-CODE.                
01662                                                                   
PEMMOD*    IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                          
PEMMOD*       IF AM-EARN-METHOD-R NOT = SPACES AND ZEROS                
PEMMOD*          MOVE AM-EARN-METHOD-R TO LF-EARN-CODE                  
PEMMOD*       ELSE                                                      
PEMMOD*          NEXT SENTENCE                                          
PEMMOD*    ELSE                                                         
PEMMOD*       IF AM-EARN-METHOD-L NOT = SPACES AND ZEROS                
PEMMOD*          MOVE AM-EARN-METHOD-L TO LF-EARN-CODE.                 
01671                                                                   
01672  0310-WAS-LIFE-INFORCE.                                           
01673                                                                   
032609     IF EX-EXIT-DATE NOT = ZEROS
01674         MOVE EX-EXIT-DATE        TO DC-GREG-DATE-CYMD
01675         MOVE 'L'                 TO DC-OPTION-CODE
01676         PERFORM 6150-DATE-CONVERSION-ROUTINE
01677         IF NO-CONVERSION-ERROR
01678            MOVE DC-BIN-DATE-1    TO EXIT-DT
01679         ELSE
01680            MOVE LOW-VALUES       TO EXIT-DT
032609        END-IF
032609     END-IF
01681                                                                   
01682      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01683      MOVE 'L'                        TO DC-OPTION-CODE.           
01684      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01685      IF NO-CONVERSION-ERROR                                       
01686         MOVE DC-BIN-DATE-1            TO EFFECT-DT                
01687      ELSE                                                         
01688         MOVE LOW-VALUES               TO EFFECT-DT.               
01689                                                                   
01690      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
01691          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
01692                                                                   
01693      MOVE LOW-VALUES             TO     FIRST-PAY-DT.             
01694                                                                   
01695      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
01696          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
01697          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
01698                                         DC-ELAPSED-DAYS           
01699          MOVE '3'                    TO DC-OPTION-CODE            
01700          PERFORM 6150-DATE-CONVERSION-ROUTINE                     
01701          IF NO-CONVERSION-ERROR                                   
01702              MOVE DC-BIN-DATE-1      TO FIRST-PAY-DT              
01703          ELSE                                                     
01704              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
01705                                                                   
01706      IF FIRST-PAY-DT LESS THAN  EFFECT-DT                         
01707          MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT.                      
01708                                                                   
01709      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
01710          MOVE EFFECT-DT              TO DC-BIN-DATE-1             
01711          MOVE +1                     TO DC-ELAPSED-MONTHS         
01712          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
01713          MOVE '6'                    TO DC-OPTION-CODE            
01714          PERFORM 6150-DATE-CONVERSION-ROUTINE                     
01715          MOVE DC-BIN-DATE-2          TO FIRST-PAY-DT              
01716          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
01717                                                                   
01718      MOVE FIRST-PAY-DT            TO DC-BIN-DATE-1.               
01719                                                                   
01720      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND         
01721         (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')                
01722         MOVE CR-LF-TERM TO DC-ELAPSED-MONTHS                      
01723      ELSE                                                         
01724         COMPUTE DC-ELAPSED-MONTHS = CR-LF-TERM - +1.              
01725                                                                   
01726      MOVE '6'                     TO DC-OPTION-CODE.              
pemuni     move +0                         to dc-elapsed-days
01727      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01728      IF NO-CONVERSION-ERROR                                       
01729          MOVE DC-BIN-DATE-2        TO EXPIRE-DT                   
01730      ELSE                                                         
01731          MOVE LOW-VALUES           TO EXPIRE-DT.                  
01732                                                                   
01733      IF (EXPIRE-DT = LOW-VALUES)      OR                          
01734          (DTE-REM-TRM-CALC-OPTION = '1' OR '2')                   
01735         MOVE EFFECT-DT               TO DC-BIN-DATE-1             
01736         MOVE CR-LF-TERM              TO DC-ELAPSED-MONTHS         
01737         MOVE ZEROS                   TO DC-ELAPSED-DAYS           
01738         MOVE '6'                     TO DC-OPTION-CODE            
01739         PERFORM 6150-DATE-CONVERSION-ROUTINE                      
01740         IF NO-CONVERSION-ERROR                                    
01741            MOVE DC-BIN-DATE-2        TO EXPIRE-DT                 
01742         ELSE                                                      
01743            MOVE LOW-VALUES           TO EXPIRE-DT.                

032609     MOVE CR-LF-EXPIRE-DATE      TO DC-GREG-DATE-CYMD
032609     MOVE 'L'                    TO DC-OPTION-CODE
032609     PERFORM 6150-DATE-CONVERSION-ROUTINE
032609     IF NO-CONVERSION-ERROR
032609        MOVE DC-BIN-DATE-1       TO EXPIRE-DT
032609     ELSE
032609        MOVE LOW-VALUES          TO EXPIRE-DT
032609     END-IF

01745      MOVE +0                         TO DC-ELAPSED-MONTHS         
01746                                                                   
01747      MOVE CR-LF-TERM                 TO EX-TERM.                  
01748                                                                   
01749      IF CR-LF-TERM LESS THAN +60                                  
01750         MOVE '1'                     TO EX-TERM-CD                
01751      ELSE                                                         
01752         MOVE '2'                     TO EX-TERM-CD.               
01753                                                                   
01754      MOVE CR-LFTYP                   TO EX-BEN-TYPE.              
01755                                                                   
01756      MOVE CR-LFPRM                   TO EX-PREM                   
01757                                         EX-PRIM-FAC-PREM          
01758                                         WS-HOLD-LF-PRIM-FAC.      
01759                                                                   
01760      IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'               
01761         MOVE CR-LFPRM-ALT      TO EX-PREM-ALT                     
01762                                   EX-PRIM-FAC-PREM-ALT.           
01763                                                                   
01764  0315-LIFE-SUMMARY-ADJUSTMENTS.                                   
01765                                                                   
01766      IF CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'Z'                  
01767          GO TO 0320-CHECK-LF-PERIOD-INFORCE.                      
01768                                                                   
01769      IF CR-LFPRM NOT GREATER CR-LFRFND                            
01770          MOVE ZERO                   TO EX-INITIAL-AMT            
01771                                         EX-PREM                   
01772      ELSE                                                         
01773          IF CR-LFRFND = ZERO                                      
01774              MOVE CR-LFAMT           TO EX-INITIAL-AMT            
01775              MOVE CR-LFPRM           TO EX-PREM                   
01776          ELSE                                                     
01777              COMPUTE TEMP-3 ROUNDED = CR-LFRFND / CR-LFPRM        
01778              COMPUTE EX-INITIAL-AMT ROUNDED =                     
01779                               CR-LFAMT - (CR-LFAMT * TEMP-3)      
01780              COMPUTE EX-PREM ROUNDED =                            
01781                               CR-LFPRM - CR-LFRFND.               
01782                                                                   
01783      MOVE EX-PREM                    TO CR-LFPRM.                 
01784                                                                   
01785  0320-CHECK-LF-PERIOD-INFORCE.                                    
01786                                                                   
01787      IF YR GREATER THAN NUMBER-OF-PERIODS                         
01788          GO TO 0390-END-PROCESS-LIFE.                             
01789                                                                   
01790      IF ENTRY-DT GREATER END-DT (YR)                              
01791         GO TO 0390-END-PROCESS-LIFE.                              
01792                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'
01794         MOVE +0             TO EX-PREM                            
01795                                EX-PREM-ALT                        
01796                                EX-PRIM-FAC-PREM                   
01797                                EX-PRIM-FAC-PREM-ALT               
01798         GO TO 0360-CALC-LF-REFUND.                                
01799                                                                   
01800      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
01801         GO TO 0340-END-REM-TERM-LF.                               
01802                                                                   
01803      MOVE EFFECT-DT         TO DT-START.                          
01804      IF BEGIN-DT (YR) GREATER THAN DT-START                       
01805         MOVE BEGIN-DT (YR) TO DT-START.                           
01806                                                                   
01807 *    IF EXPIRE-DT NOT GREATER THAN END-DT (YR)                    
01808 *       MOVE EXPIRE-DT TO DT-END                                  
01809 *    ELSE                                                         
01810 *       MOVE END-DT (YR) TO DT-END.                               

032609     IF EXPIRE-DT < END-DT (YR)                    
032609        MOVE EXPIRE-DT           TO DT-END
032609     ELSE
032609        MOVE END-DT (YR)         TO DT-END
032609     END-IF

01812      IF EXIT-DT NOT = LOW-VALUES                                  
01813         IF EXIT-DT LESS THAN DT-END                               
01814            MOVE EXIT-DT TO DT-END.                                
01815                                                                   
01816      MOVE DT-START                   TO DC-BIN-DATE-1.            
01817      MOVE DT-END                     TO DC-BIN-DATE-2.            
01818      MOVE '1'                        TO DC-OPTION-CODE.           
01819      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01820      IF NO-CONVERSION-ERROR                                       
01821         MOVE DC-ELAPSED-MONTHS       TO RR (YR)                   
01822      ELSE                                                         
01823         MOVE +0                      TO RR (YR).                  
01824                                                                   
01825      IF RR (YR) LESS THAN +0                                      
01826         MOVE +0 TO RR (YR).                                       
01827                                                                   
PEMTST     MOVE EXPIRE-DT TO DT-END.                                    
01829                                                                   
01830      IF EXIT-DT NOT = LOW-VALUES                                  
01831         IF EXIT-DT LESS THAN DT-END                               
01832            IF EXIT-DT NOT GREATER THAN END-DT (YR)                
01833               MOVE EXIT-DT TO DT-END.                             
01834                                                                   
01835      MOVE DT-START                   TO DC-BIN-DATE-1.            
01836      MOVE DT-END                     TO DC-BIN-DATE-2.            
01837      MOVE '1'                        TO DC-OPTION-CODE.           
01838      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01839      IF NO-CONVERSION-ERROR                                       
01840         MOVE DC-ELAPSED-MONTHS       TO M1 (YR)                   
01841      ELSE                                                         
01842         MOVE +0                      TO M1 (YR).                  
01843                                                                   
01844      IF M1 (YR) LESS THAN +0                                      
01845         MOVE +0 TO M1 (YR).                                       
01846                                                                   

032609     IF EXPIRE-DT < END-DT (YR)
032609        MOVE +0                  TO M2 (YR) M5 (YR)
032609        GO TO 0330-REM-TERM-LF-BEGIN
032609     END-IF
032609
032609     IF EXIT-DT NOT = LOW-VALUES
032609        IF EXIT-DT <= END-DT (YR)                       
032609           MOVE +0               TO M2 (YR) M5 (YR)
032609           GO TO 0330-REM-TERM-LF-BEGIN
032609        END-IF
032609     END-IF

01851      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01852      MOVE 'L'                        TO DC-OPTION-CODE.           
01853      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01854      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
01855                                                                   
01856      MOVE ENDING-DATE (YR)           TO DC-GREG-DATE-CYMD.        
01857      MOVE 'L'                        TO DC-OPTION-CODE.           
01858      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01859      MOVE DC-BIN-DATE-1              TO CP-VALUATION-DT.          
01860                                                                   
01861      IF CP-TERM-IS-DAYS                                           
01862         MOVE CR-LF-TERM-IN-DAYS      TO CP-TERM-OR-EXT-DAYS       
01863      ELSE                                                         
01864         MOVE ZEROS                   TO CP-TERM-OR-EXT-DAYS.      
01865                                                                   
01866      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM          
01867                                         CP-LOAN-TERM.             
01868      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
01869      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
01870                                                                   
01871      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND         
01872        CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                   
01873        ADD +1 TO CP-ORIGINAL-TERM CP-LOAN-TERM.                   
01874                                                                   
01875      MOVE 'Y' TO FROM-LIFE-SW.                                    
01876      PERFORM 6000-CALC-REM-TERM THRU 6010-REM-TERM-EXIT.          
01877                                                                   
01878      IF REM-TRM2 GREATER CR-LF-TERM                               
01879         MOVE CR-LF-TERM TO M2 (YR) M5 (YR)                        
01880                            WS-LF-END-REM-TRM1 (YR)                
01881      ELSE                                                         
01882      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND         
01883        CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                   
01884         MOVE REM-TRM1       TO M2 (YR)                            
01885                                WS-LF-END-REM-TRM1 (YR)            
01886         MOVE LF-BAL-REMTERM TO M5 (YR)                            
01887      ELSE                                                         
01888         MOVE REM-TRM1       TO M2 (YR)                            
01889                                WS-LF-END-REM-TRM1 (YR)            
01890         MOVE REM-TRM2       TO M5 (YR).                           
01891                                                                   
01892  0330-REM-TERM-LF-BEGIN.                                          
01893                                                                   
01894      IF (ENTRY-DT GREATER BEGIN-DT (YR)) OR                       
01895         (DT-END NOT GREATER BEGIN-DT (YR))                        
01896         MOVE +0 TO M3 (YR) M6 (YR)                                
01897         GO TO 0340-END-REM-TERM-LF.                               
01898                                                                   
01899      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
01900      MOVE 'L'                        TO DC-OPTION-CODE.           
01901      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01902      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
01903                                                                   
01904      MOVE BEGIN-DATE (YR)            TO DC-GREG-DATE-CYMD.        
01905      MOVE 'L'                        TO DC-OPTION-CODE.           
01906      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
01907      MOVE DC-BIN-DATE-1              TO CP-VALUATION-DT.          
01908                                                                   
01909      IF CP-TERM-IS-DAYS                                           
01910         MOVE CR-LF-TERM-IN-DAYS    TO CP-TERM-OR-EXT-DAYS         
01911      ELSE                                                         
01912         MOVE ZEROS                 TO CP-TERM-OR-EXT-DAYS.        
01913                                                                   
01914      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM          
01915                                         CP-LOAN-TERM.             
01916                                                                   
01917      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
01918      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
01919                                                                   
01920      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND         
01921        CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                   
01922        ADD +1 TO CP-ORIGINAL-TERM CP-LOAN-TERM.                   
01923                                                                   
01924      MOVE 'Y' TO FROM-LIFE-SW.                                    
01925      PERFORM 6000-CALC-REM-TERM THRU 6010-REM-TERM-EXIT.          
01926                                                                   
01927      IF REM-TRM2 GREATER CR-LF-TERM                               
01928         MOVE CR-LF-TERM TO M3 (YR) M6 (YR)                        
01929                            WS-LF-BEG-REM-TRM1 (YR)                
01930      ELSE                                                         
01931      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND         
01932        CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L'                   
01933         MOVE REM-TRM1       TO M3 (YR)                            
01934                                WS-LF-BEG-REM-TRM1 (YR)            
01935         MOVE LF-BAL-REMTERM TO M6 (YR)                            
01936      ELSE                                                         
01937         MOVE REM-TRM1       TO M3 (YR)                            
01938                                WS-LF-BEG-REM-TRM1 (YR)            
01939         MOVE REM-TRM2       TO M6 (YR).                           
01940                                                                   
01941  0340-END-REM-TERM-LF.                                            
01942                                                                   
01943      IF M5 (YR) GREATER THAN +0                                   
01944          IF CLAS-I-CALC-TYPE (CLAS-INDEXL) = 'Z'                  
01945              COMPUTE EX-C-CNT (YR) = CR-LIVES -                   
01946                                      CR-SUM-CAN-CNT-ITD           
01947          ELSE                                                     
01948              MOVE +1 TO EX-C-CNT (YR).                            
01949                                                                   
01950      IF DEBUG-LF-INFORCE-CNT                                      
01951          IF (YR = +1) AND                                         
01952             (EX-C-CNT (YR) GREATER THAN ZEROS)                    
01953              ADD EX-C-CNT (YR) TO WS-S-INFORCE-CNT                
01954              DISPLAY ' CERT ', CR-FULL-CONTROL                    
01955                      ' CNT ', EX-C-CNT (YR)                       
01956                      ' TOT ', WS-S-INFORCE-CNT.                   
01957                                                                   
01958      MOVE +0 TO SA.                                               
01959                                                                   
01960  0350-CALC-LIFE-COMMISSIONS.                                      
01961                                                                   
01962      IF (ENTRY-DT GREATER THAN END-DT (YR))                       
01963        OR                                                         
01964         (ENTRY-DT NOT GREATER THAN BEGIN-DT (YR))                 
01965         GO TO 0360-CALC-LF-REFUND.                                
01966                                                                   
01967      ADD +1 TO SA.                                                
01968                                                                   
01969      IF SA GREATER THAN +10                                       
01970         GO TO 0360-CALC-LF-REFUND.                                
01971                                                                   
01972      IF CR-COM-AGT (SA) = ZEROS OR SPACES                         
01973          GO TO 0350-CALC-LIFE-COMMISSIONS.                        
01974                                                                   
052814     IF CR-AGT-TYPE (SA) = 'C' OR 'D' OR 'F'
01976         COMPUTE AGT-COMM =                                        
01977          (CR-LFPRM + CR-LFPRM-ALT) * CR-LCOM-L (SA)               
01978         ADD AGT-COMM TO EX-AGT-COMM (YR)                          
01979      ELSE                                                         
052814        IF CR-AGT-TYPE (SA) = 'O' OR 'P' OR 'G' OR 'B' OR 'S'
01981            COMPUTE AGT-COMM =                                     
01982             (CR-LFPRM + CR-LFPRM-ALT) * CR-LCOM-L (SA)            
01983            ADD AGT-COMM TO EX-OVR-COMM (YR).                      
01984                                                                   
01985      GO TO 0350-CALC-LIFE-COMMISSIONS.                            
01986                                                                   
01987  0360-CALC-LF-REFUND.                                             
01988                                                                   
01989      IF LF-CAN GREATER THAN BEGIN-DT (YR) AND                     
01990         LF-CAN NOT GREATER THAN END-DT (YR)                       
01991         MOVE CR-LFRFND     TO EX-CNC-AMT (YR)                     
01992      ELSE                                                         
01993         GO TO 0380-FINISH-LIFE-YEAR.                              
01994                                                                   
01995      MOVE +0 TO SA.                                               
01996                                                                   
01997  0370-CALC-LIFE-REF-COMM.                                         
01998                                                                   
01999      ADD +1 TO SA.                                                
02000                                                                   
02001      IF SA GREATER THAN +10                                       
02002         GO TO 0380-FINISH-LIFE-YEAR.                              
02003                                                                   
02004      IF CR-COM-AGT (SA) = ZEROS OR SPACES                         
02005          GO TO 0370-CALC-LIFE-REF-COMM.                           
02006                                                                   
052814     IF CR-AGT-TYPE (SA) = 'C' OR 'D' OR 'F'
02008         COMPUTE AGT-COMM =                                        
02009          -1 * CR-LFRFND * CR-LCOM-L (SA)                          
02010         ADD AGT-COMM TO EX-AGT-COMM (YR)                          
02011      ELSE                                                         
052814        IF CR-AGT-TYPE (SA) = 'O' OR 'P' OR 'G' OR 'B' OR 'S'
02013            COMPUTE AGT-COMM =                                     
02014             -1 * CR-LFRFND * CR-LCOM-L (SA)                       
02015            ADD AGT-COMM TO EX-OVR-COMM (YR).                      
02016                                                                   
02017      GO TO 0370-CALC-LIFE-REF-COMM.                               
02018                                                                   
02019  0380-FINISH-LIFE-YEAR.                                           
02020                                                                   
02021      ADD +1 TO YR.                                                
02022      GO TO 0320-CHECK-LF-PERIOD-INFORCE.                          
02023                                                                   
02024  0390-END-PROCESS-LIFE.                                           
02025                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'                              
02027         GO TO 0400-RELEASE-LIFE-RECORD.                           
02028                                                                   
02029      IF CR-LF-DEV-PCT NOT NUMERIC                                 
02030         MOVE +0       TO CR-LF-DEV-PCT.                           
02031                                                                   
062712     IF DTE-CLIENT = 'CID' OR 'AHL'
CIDMOD        GO TO 0400-RELEASE-LIFE-RECORD
CIDMOD     END-IF
CIDMOD
02032      IF (CR-STATE = STATE-IL OR STATE-GA OR                       
02033                     STATE-NC OR STATE-UT OR STATE-TX OR           
02034                     STATE-IA OR STATE-NE OR STATE-NV OR           
02035                     STATE-OH OR STATE-OR OR STATE-WI)             
02036                 AND                                               
02037          (RR (1) GREATER THAN +0 OR                               
02038           RR (2) GREATER THAN +0 OR                               
02039           RR (3) GREATER THAN +0)                                 
02040         NEXT SENTENCE                                             
02041      ELSE                                                         
CIDMOD     IF DTE-CLIENT  =  'CRI' OR 'CVL' OR 'CID'
02043          NEXT SENTENCE                                            
02044      ELSE                                                         
02045          IF (RR (1) GREATER THAN +0 OR                            
02046              RR (2) GREATER THAN +0 OR                            
02047              RR (3) GREATER THAN +0)                              
02048                    AND                                            
02049             ((CR-LF-DEV-CODE NOT =                                
02050                    STATE-PRIM-FAC-DEV (CLAS-INDEXS))              
02051                    OR                                             
02052             (CR-LF-DEV-PCT NOT = +0 AND +1.0))                    
02053              NEXT SENTENCE                                        
02054          ELSE                                                     
02055              GO TO 0400-RELEASE-LIFE-RECORD.                      
02056                                                                   
CIDMOD*    MOVE EP-DT                      TO DC-GREG-DATE-CYMD.        
CIDMOD*    MOVE 'L'                        TO DC-OPTION-CODE.           
CIDMOD*    PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
CIDMOD*    MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
CIDMOD     MOVE END-DT (1)                 TO CP-CERT-EFF-DT
02061      MOVE CLAS-I-RL-AH (CLAS-INDEXL) TO CP-BENEFIT-TYPE.          
02062      MOVE SPACES                     TO CP-ACCT-FLD-5.            
02063      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
02064      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
02065      MOVE CR-LFPRM                   TO CP-ORIGINAL-PREMIUM.      
02066      MOVE CR-LF-CRIT-PERIOD          TO CP-CRITICAL-MONTHS.       
02067      MOVE CR-LF-TERM                 TO CP-ORIGINAL-TERM          
02068                                         CP-LOAN-TERM.             
02069      MOVE CR-LFAMT                   TO CP-ORIGINAL-BENEFIT       
02070                                         CP-RATING-BENEFIT-AMT.    
02071      MOVE CR-APR                     TO CP-LOAN-APR.              
02072      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
02073      MOVE LF-EARN-CODE               TO CP-EARNING-METHOD.        
02074      MOVE '3'                        TO CP-PROCESS-TYPE.          
02075      MOVE CLAS-I-BAL (CLAS-INDEXL)   TO CP-SPECIAL-CALC-CD.       
02076      IF CR-RATING-CLASS NOT = SPACES AND ZEROS                    
02077         MOVE CR-RATING-CLASS         TO CP-CLASS-CODE             
02078      ELSE                                                         
02079         MOVE AM-CAL-TABLE            TO CP-CLASS-CODE.            
02080                                                                   
02081      IF DTE-CLIENT  =  'CRI'                                      
02082          MOVE WK-LF-DEV-CODE         TO  CP-DEVIATION-CODE        
02083      ELSE                                                         
02084          MOVE STATE-PRIM-FAC-DEV (CLAS-INDEXS)                    
02085                                      TO  CP-DEVIATION-CODE.       
02086                                                                   
02087      MOVE +0                         TO CP-RATE-DEV-PCT.          
02088      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
02089      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
02090      MOVE CR-LFTYP                   TO CP-BENEFIT-CD.            
02091      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
02092      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
02093      MOVE LIFE-OVERRIDE-L1           TO CP-LIFE-OVERRIDE-CODE.    
02094                                                                   
02095      IF CP-STATE-STD-ABBRV = 'OR'                                 
02096          COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT + CR-LFAMT-ALT. 
02097                                                                   
02098      IF (CP-RATE-AS-STANDARD) AND (CP-EARN-AS-NET-PAY)            
02099         COMPUTE CP-ORIGINAL-BENEFIT =                             
02100          (CR-LFAMT - CR-LFPRM).                                   
02101                                                                   
02102      IF CP-TRUNCATED-LIFE                                         
02103         MOVE CR-LOAN-TERM            TO CP-LOAN-TERM.             
02104                                                                   
02105      IF CP-TERM-IS-DAYS                                           
02106         MOVE CR-LF-TERM-IN-DAYS    TO CP-TERM-OR-EXT-DAYS         
02107      ELSE                                                         
02108         MOVE ZEROS                 TO CP-TERM-OR-EXT-DAYS.        
02109                                                                   
02110      PERFORM 6130-CALL-RATING-ROUTINE THRU 6140-RATE-EXIT.        
02111      ADD 1 TO WS-LF-RATE-ATTEMPT.                                 
02112                                                                   
062712     IF DTE-CLIENT = 'CID' OR 'AHL'
CIDMOD        IF CR-LFPRM-CALC NOT NUMERIC
CIDMOD           MOVE +0               TO CR-LFPRM-CALC
CIDMOD        END-IF
CIDMOD        IF CR-LFPRM-CALC = ZEROS
CIDMOD           MOVE CR-LFPRM         TO CR-LFPRM-CALC
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
02113      IF NOT CP-ERROR-OCCURED                                      
CIDMOD        IF (CP-CALC-PREMIUM NOT = +0) AND                         
CIDMOD           (CR-LFPRM-CALC NOT = +0)
CIDMOD           IF DTE-CLIENT = 'CID'
CIDMOD              COMPUTE WS-DIFF = (CP-CALC-PREMIUM -
CIDMOD                 CR-LFPRM-CALC) / CR-LFPRM-CALC
CIDMOD              IF WS-DIFF < 0
CIDMOD                 COMPUTE WS-DIFF = WS-DIFF * -1
CIDMOD              END-IF
CIDMOD              IF WS-DIFF > 0.02
CIDMOD                 MOVE CP-CALC-PREMIUM TO EX-PRIM-FAC-PREM         
CIDMOD                                    WS-HOLD-LF-PRIM-FAC           
CIDMOD              END-IF
CIDMOD           ELSE
02115               MOVE CP-CALC-PREMIUM TO EX-PRIM-FAC-PREM            
02116                                     WS-HOLD-LF-PRIM-FAC           
CIDMOD           END-IF
CIDMOD        END-IF                                                    
02119      ELSE                                                         
02120         ADD 1 TO WS-LF-RATE-ERROR                                 
CIDMOD     END-IF
02121                                                                   
02122      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L'         
02123         GO TO 0400-RELEASE-LIFE-RECORD.                           
02124                                                                   
02125      IF CR-LF-DEV-PCT = +0                                        
02126         GO TO 0400-RELEASE-LIFE-RECORD.                           
02127                                                                   
02128      MOVE 'L'          TO CP-BENEFIT-TYPE.                        
02129      MOVE '2'          TO CP-EARNING-METHOD.                      
02130      MOVE SPACES       TO CP-SPECIAL-CALC-CD.                     
02131      MOVE 'LEV'        TO CP-DEVIATION-CODE.                      
02132      MOVE CR-LFTYP     TO CP-BENEFIT-CD.                          
02133      MOVE CR-LFAMT-ALT TO CP-ORIGINAL-BENEFIT                     
02134                           CP-RATING-BENEFIT-AMT.                  
02135                                                                   
02136      IF CP-STATE-STD-ABBRV = 'OR'                                 
02137          COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT + CR-LFAMT-ALT. 
02138                                                                   
02139      MOVE CR-LFPRM-ALT TO CP-ORIGINAL-PREMIUM.                    
02140      MOVE +0           TO CP-CRITICAL-MONTHS                      
02141                           CP-RATE-DEV-PCT.                        
02142                                                                   
02143      MOVE CR-LF-TERM   TO CP-ORIGINAL-TERM                        
02144                           CP-REMAINING-TERM                       
02145                           CP-LOAN-TERM.                           
02146                                                                   
02147      IF CLAS-I-BAL (CLAS-INDEXL) = 'L'                            
02148         ADD +1 TO CP-ORIGINAL-TERM                                
02149                   CP-REMAINING-TERM                               
02150                   CP-LOAN-TERM.                                   
02151                                                                   
02152      PERFORM 6130-CALL-RATING-ROUTINE THRU 6140-RATE-EXIT.        
02153      ADD 1 TO WS-LF-RATE-ATTEMPT.                                 
02154                                                                   
02155      IF NOT CP-ERROR-OCCURED                                      
02156         IF CP-CALC-PREMIUM NOT = +0                               
02157            MOVE CP-CALC-PREMIUM TO EX-PRIM-FAC-PREM-ALT           
02158         ELSE                                                      
02159            NEXT SENTENCE                                          
02160      ELSE                                                         
02161         ADD 1 TO WS-LF-RATE-ERROR.                                
02162                                                                   
02163  0400-RELEASE-LIFE-RECORD.                                        
02164                                                                   
02165      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
02166      MOVE 'L'                        TO DC-OPTION-CODE.           
02167      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
02168      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
02169                                                                   
02170      PERFORM 0410-BUILD-LIFE-CLAIMS                               
02171          VARYING YR FROM +1 BY +1                                 
02172          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
02173                                                                   
02174      GO TO 0420-END-LIFE-CLAIMS.                                  
02175                                                                   
02176  0410-BUILD-LIFE-CLAIMS.                                          
02177                                                                   
02178      IF CERT-AND-CLAIM                                            
02179         MOVE WS-CLAIM-AMT (1 1 YR) TO EX-CLAIM-AMT (YR)           
02180         MOVE WS-INC-CNT   (1 1 YR) TO EX-INC-CNT   (YR)           
02181         MOVE WS-PD-CNT    (1 1 YR) TO EX-PD-CNT    (YR).          
02182                                                                   
02183  0420-END-LIFE-CLAIMS.                                            
02184                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'
02186         GO TO 0460-END-LF-RESERVES.                               
02187                                                                   
02188      MOVE EX-PREM       TO WRK-C.                                 
02189      MOVE EX-PREM-ALT   TO WRK-ALT.                               
02190                                                                   
02191      PERFORM 0480-CALC-LIFE-UNEARN THRU 0540-LIFE-UNEARN-EXIT     
02192          VARYING YR FROM +1 BY +1                                 
02193          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
02194                                                                   
02195      PERFORM 0430-BUILD-LF-DEV-RESERVES                           
02196          VARYING YR FROM +1 BY +1                                 
02197          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
02198                                                                   
02199      GO TO 0440-CALC-LF-PRIM-FAC.                                 
02200                                                                   
02201  0430-BUILD-LF-DEV-RESERVES.                                      
02202                                                                   
02203      IF (DTE-CLIENT = 'CID') AND                                  
02204         (STATE-ABBR (CLAS-INDEXS) = 'WY') AND                     
02205         (CLAS-I-EP (CLAS-INDEXL) = 'T')                           
02206            COMPUTE EX-BEG-ST-RES (YR) =                           
02207              WS-RED-LF-BEGIN-RES (YR) +                           
02208              WS-RED-LF-BEGIN-ALT (YR)                             
02209            COMPUTE EX-END-ST-RES (YR) =                           
02210              WS-RED-LF-END-RES (YR) +                             
02211              WS-RED-LF-END-ALT (YR)                               
02212            MOVE EX-BEG-ST-RES (YR) TO                             
02213                 EX-PRI-BEG-ST-RES (YR)                            
02214            MOVE EX-END-ST-RES (YR) TO                             
02215                 EX-PRI-END-ST-RES (YR)                            
02216      ELSE                                                         
02217      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
02218         COMPUTE EX-BEG-ST-RES (YR) =                              
02219           WS-LEV-LF-BEGIN-RES (YR) +                              
02220           WS-LEV-LF-BEGIN-ALT (YR)                                
02221         COMPUTE EX-END-ST-RES (YR) =                              
02222           WS-LEV-LF-END-RES (YR) +                                
02223           WS-LEV-LF-END-ALT (YR)                                  
02224           MOVE EX-BEG-ST-RES (YR) TO                              
02225                EX-PRI-BEG-ST-RES (YR)                             
02226           MOVE EX-END-ST-RES (YR) TO                              
02227                EX-PRI-END-ST-RES (YR)                             
02228      ELSE                                                         
02229      IF STATE-ABBR (CLAS-INDEXS) = 'AL'                           
02230         COMPUTE EX-BEG-ST-RES (YR) =                              
02231           WS-RED-LF-BEGIN-RES (YR) +                              
02232           WS-RED-LF-BEGIN-ALT (YR)                                
02233         COMPUTE EX-END-ST-RES (YR) =                              
02234           WS-RED-LF-END-RES (YR) +                                
02235           WS-RED-LF-END-ALT (YR)                                  
02236           MOVE EX-BEG-ST-RES (YR) TO                              
02237                EX-PRI-BEG-ST-RES (YR)                             
02238           MOVE EX-END-ST-RES (YR) TO                              
02239                EX-PRI-END-ST-RES (YR)                             
02240      ELSE                                                         
02241      IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L')             
02242         COMPUTE EX-BEG-ST-RES (YR) =                              
02243           WS-RED-LF-BEGIN-RES (YR) +                              
02244           WS-LEV-LF-BEGIN-ALT (YR)                                
02245         COMPUTE EX-END-ST-RES (YR) =                              
02246           WS-RED-LF-END-RES (YR) +                                
02247           WS-LEV-LF-END-ALT (YR)                                  
02248           MOVE EX-BEG-ST-RES (YR) TO                              
02249                EX-PRI-BEG-ST-RES (YR)                             
02250           MOVE EX-END-ST-RES (YR) TO                              
02251                EX-PRI-END-ST-RES (YR)                             
02252      ELSE                                                         
02253      IF (STATE-ABBR (CLAS-INDEXS) = 'OH')  AND                    
CIDMOD        (EX-CAL-TABLE NOT = 'L ') AND
02254         (EX-TERM GREATER THAN +60)  AND                           
02255         (EX-APR GREATER THAN +0)  AND                             
CIDMOD        (EX-EFF-DATE  GREATER THAN 19831031 )                     
02256 **      (EX-EFF-DATE  GREATER THAN 199831031 )                    
02257            COMPUTE EX-BEG-ST-RES (YR) =                           
02258              WS-RED-LF-BEGIN-RES (YR) +                           
02259              WS-RED-LF-BEGIN-ALT (YR)                             
02260            COMPUTE EX-END-ST-RES (YR) =                           
02261              WS-RED-LF-END-RES (YR) +                             
02262              WS-RED-LF-END-ALT (YR)                               
02263            MOVE EX-BEG-ST-RES (YR) TO                             
02264                 EX-PRI-BEG-ST-RES (YR)                            
02265            MOVE EX-END-ST-RES (YR) TO                             
02266                 EX-PRI-END-ST-RES (YR)                            
02267      ELSE                                                         
02268      IF (CLAS-I-EP (CLAS-INDEXL) = 'N' OR 'T') AND                
02269          (DTE-CLIENT NOT = 'MON')                                 
02270            COMPUTE EX-BEG-ST-RES (YR) =                           
02271              WS-RED-LF-BEGIN-RES (YR) +                           
02272              WS-RED-LF-BEGIN-ALT (YR)                             
02273            COMPUTE EX-END-ST-RES (YR) =                           
02274              WS-RED-LF-END-RES (YR) +                             
02275              WS-RED-LF-END-ALT (YR)                               
02276            MOVE EX-BEG-ST-RES (YR) TO                             
02277                 EX-PRI-BEG-ST-RES (YR)                            
02278            MOVE EX-END-ST-RES (YR) TO                             
02279                 EX-PRI-END-ST-RES (YR)                            
02280      ELSE                                                         
02281      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
02282         COMPUTE EX-BEG-ST-RES (YR) =                              
02283           WS-LEV-LF-BEGIN-RES (YR) +                              
02284           WS-LEV-LF-BEGIN-ALT (YR)                                
02285         COMPUTE EX-END-ST-RES (YR) =                              
02286           WS-LEV-LF-END-RES (YR) +                                
02287           WS-LEV-LF-END-ALT (YR)                                  
02288         MOVE EX-BEG-ST-RES (YR) TO                                
02289              EX-PRI-BEG-ST-RES (YR)                               
02290         MOVE EX-END-ST-RES (YR) TO                                
02291              EX-PRI-END-ST-RES (YR)                               
02292      ELSE                                                         
02293      IF (DTE-CLIENT = 'MON') AND                                  
02294         (CLAS-I-EP (CLAS-INDEXL) = '2')                           
02295           COMPUTE EX-BEG-ST-RES (YR) =                            
02296             WS-LEV-LF-BEGIN-RES (YR) +                            
02297             WS-LEV-LF-BEGIN-ALT (YR)                              
02298           COMPUTE EX-END-ST-RES (YR) =                            
02299             WS-LEV-LF-END-RES (YR) +                              
02300             WS-LEV-LF-END-ALT (YR)                                
02301           MOVE EX-BEG-ST-RES (YR) TO                              
02302                EX-PRI-BEG-ST-RES (YR)                             
02303           MOVE EX-END-ST-RES (YR) TO                              
02304                EX-PRI-END-ST-RES (YR)                             
02305      ELSE                                                         
02306         COMPUTE EX-BEG-ST-RES (YR) =                              
02307           WS-RED-LF-BEGIN-RES (YR) +                              
02308           WS-RED-LF-BEGIN-ALT (YR)                                
02309         COMPUTE EX-END-ST-RES (YR) =                              
02310           WS-RED-LF-END-RES (YR) +                                
02311           WS-RED-LF-END-ALT (YR)                                  
02312           MOVE EX-BEG-ST-RES (YR) TO                              
02313                EX-PRI-BEG-ST-RES (YR)                             
02314           MOVE EX-END-ST-RES (YR) TO                              
02315                EX-PRI-END-ST-RES (YR).                            
02316                                                                   
02317      IF CLAS-I-EP (CLAS-INDEXL) = 'N' OR 'T'                      
02318         MOVE WS-LF-RED-MEAN-INFORCE (YR) TO                       
02319              EX-MEAN-INFORCE (YR)                                 
02320      ELSE                                                         
02321      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
02322         MOVE WS-LF-LEV-MEAN-INFORCE (YR) TO                       
02323              EX-MEAN-INFORCE (YR)                                 
02324      ELSE                                                         
02325         MOVE WS-LF-RED-MEAN-INFORCE (YR) TO                       
02326              EX-MEAN-INFORCE (YR).                                
02327                                                                   
02328  0440-CALC-LF-PRIM-FAC.                                           
02329                                                                   
02330      IF EX-PREM NOT = EX-PRIM-FAC-PREM                            
02331         MOVE EX-PRIM-FAC-PREM     TO WRK-C                        
02332         MOVE EX-PRIM-FAC-PREM-ALT TO WRK-ALT                      
02333         PERFORM 0480-CALC-LIFE-UNEARN THRU                        
02334                                       0530-CALC-UNEARN-FINISH     
02335             VARYING YR FROM +1 BY +1                              
02336             UNTIL YR GREATER THAN NUMBER-OF-PERIODS               
02337         PERFORM 0450-BUILD-LF-PRI-RESERVES                        
02338             VARYING YR FROM +1 BY +1                              
02339             UNTIL YR GREATER THAN NUMBER-OF-PERIODS.              
02340                                                                   
02341      GO TO 0460-END-LF-RESERVES.                                  
02342                                                                   
02343  0450-BUILD-LF-PRI-RESERVES.                                      
02344                                                                   
02345      IF (DTE-CLIENT = 'CID') AND                                  
02346         (STATE-ABBR (CLAS-INDEXS) = 'WY') AND                     
02347         (CLAS-I-EP (CLAS-INDEXL) = 'T')                           
02348           COMPUTE EX-PRI-BEG-ST-RES (YR) =                        
02349             WS-RED-LF-BEGIN-RES (YR) +                            
02350             WS-RED-LF-BEGIN-ALT (YR)                              
02351           COMPUTE EX-PRI-END-ST-RES (YR) =                        
02352             WS-RED-LF-END-RES (YR) +                              
02353             WS-RED-LF-END-ALT (YR)                                
02354      ELSE                                                         
02355      IF STATE-ABBR (CLAS-INDEXS) = 'WY'                           
02356         COMPUTE EX-PRI-BEG-ST-RES (YR) =                          
02357           WS-LEV-LF-BEGIN-RES (YR) +                              
02358           WS-LEV-LF-BEGIN-ALT (YR)                                
02359         COMPUTE EX-PRI-END-ST-RES (YR) =                          
02360           WS-LEV-LF-END-RES (YR) +                                
02361           WS-LEV-LF-END-ALT (YR)                                  
02362      ELSE                                                         
02363      IF STATE-ABBR (CLAS-INDEXS) = 'AL'                           
02364         COMPUTE EX-PRI-BEG-ST-RES (YR) =                          
02365           WS-RED-LF-BEGIN-RES (YR) +                              
02366           WS-RED-LF-BEGIN-ALT (YR)                                
02367         COMPUTE EX-PRI-END-ST-RES (YR) =                          
02368           WS-RED-LF-END-RES (YR) +                                
02369           WS-RED-LF-END-ALT (YR)                                  
02370      ELSE                                                         
02371      IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'               
02372         COMPUTE EX-PRI-BEG-ST-RES (YR) =                          
02373           WS-RED-LF-BEGIN-RES (YR) +                              
02374           WS-LEV-LF-BEGIN-ALT (YR)                                
02375         COMPUTE EX-PRI-END-ST-RES (YR) =                          
02376           WS-RED-LF-END-RES (YR) +                                
02377           WS-LEV-LF-END-ALT (YR)                                  
02378      ELSE                                                         
02379      IF (STATE-ABBR (CLAS-INDEXS) = 'OH')  AND                    
CIDMOD        (EX-CAL-TABLE NOT = 'L ') AND
02380         (EX-TERM GREATER THAN +60)  AND                           
02381         (EX-APR GREATER THAN +0)  AND                             
02382         (EX-EFF-DATE  GREATER THAN  19831031 )                    
02383           COMPUTE EX-PRI-BEG-ST-RES (YR) =                        
02384             WS-RED-LF-BEGIN-RES (YR) +                            
02385             WS-RED-LF-BEGIN-ALT (YR)                              
02386           COMPUTE EX-PRI-END-ST-RES (YR) =                        
02387             WS-RED-LF-END-RES (YR) +                              
02388             WS-RED-LF-END-ALT (YR)                                
02389      ELSE                                                         
02390      IF (CLAS-I-EP (CLAS-INDEXL) = 'N' OR 'T') AND                
02391         (DTE-CLIENT NOT = 'MON')                                  
02392           COMPUTE EX-PRI-BEG-ST-RES (YR) =                        
02393             WS-RED-LF-BEGIN-RES (YR) +                            
02394             WS-RED-LF-BEGIN-ALT (YR)                              
02395           COMPUTE EX-PRI-END-ST-RES (YR) =                        
02396             WS-RED-LF-END-RES (YR) +                              
02397             WS-RED-LF-END-ALT (YR)                                
02398      ELSE                                                         
02399      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
02400         COMPUTE EX-PRI-BEG-ST-RES (YR) =                          
02401           WS-LEV-LF-BEGIN-RES (YR) +                              
02402           WS-LEV-LF-BEGIN-ALT (YR)                                
02403         COMPUTE EX-PRI-END-ST-RES (YR) =                          
02404           WS-LEV-LF-END-RES (YR) +                                
02405           WS-LEV-LF-END-ALT (YR)                                  
02406      ELSE                                                         
02407      IF (CLAS-I-EP (CLAS-INDEXL) = '2') AND                       
02408         (DTE-CLIENT = 'MON')                                      
02409           COMPUTE EX-PRI-BEG-ST-RES (YR) =                        
02410             WS-LEV-LF-BEGIN-RES (YR) +                            
02411             WS-LEV-LF-BEGIN-ALT (YR)                              
02412           COMPUTE EX-PRI-END-ST-RES (YR) =                        
02413             WS-LEV-LF-END-RES (YR) +                              
02414             WS-LEV-LF-END-ALT (YR)                                
02415      ELSE                                                         
02416         COMPUTE EX-PRI-BEG-ST-RES (YR) =                          
02417           WS-RED-LF-BEGIN-RES (YR) +                              
02418           WS-RED-LF-BEGIN-ALT (YR)                                
02419         COMPUTE EX-PRI-END-ST-RES (YR) =                          
02420           WS-RED-LF-END-RES (YR) +                                
02421           WS-RED-LF-END-ALT (YR).                                 
02422                                                                   
02423  0460-END-LF-RESERVES.                                            
02424                                                                   
02425      IF DEBUG-LF-STATUTORY                                        
02426          GO TO 0460-CONTINUE.                                     
02427                                                                   
02428      IF DTE-PGM-OPT NOT = '3' AND '4'                             
02429         GO TO 0470-BYPASS-LF-DETAIL.                              
02430                                                                   
PEMTST     IF M5 (1) = +0                                               
PEMTST        GO TO 0470-BYPASS-LF-DETAIL.                              
02433                                                                   
02434  0460-CONTINUE.                                                   
02435                                                                   
02436      MOVE EX-CARRIER           TO D-CARR.                         
02437      MOVE EX-GROUPING          TO D-GROUP.                        
02438      MOVE EX-STATE             TO D-STATE.                        
02439      MOVE EX-ACCOUNT           TO D-ACCOUNT.                      
02440      MOVE CR-CERT-NO           TO D-CERT-NO.                      
02441      MOVE 'L'                  TO DC-OPTION-CODE.                 
02442      MOVE EX-EFF-DATE          TO DC-GREG-DATE-CYMD.              
02443      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
02444      MOVE DC-GREG-DATE-1-EDIT  TO D-EFF-DATE.                     
02445      MOVE EX-PREM              TO D-PREM.                         
02446      MOVE EX-PRIM-FAC-PREM     TO D-PRI-PREM                      
02447      MOVE EX-TERM              TO D-TERM.                         
02448      MOVE M2 (1)               TO D-REM-TERM.                     
02449      MOVE EX-END-ST-RES (1)    TO D-UNEARN.                       
02450      MOVE EX-INITIAL-AMT       TO D-BENEFIT.                      
02451      MOVE 'L'                  TO D-L-A.                          
02452      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO D-BEN-DESC.                
02453                                                                   
02454      PERFORM 1600-PRINT-A-LINE.                                   
02455                                                                   
02456      IF DEBUG-LF-STATUTORY                                        
02457          IF EX-END-ST-RES (1) NOT = ZEROS                         
02458              ADD +1                TO WS-S-CNT                    
02459              ADD EX-END-ST-RES (1) TO WS-S-STATUTORY              
02460              DISPLAY ' CNT ', WS-S-CNT                            
02461                      ' TOT ', WS-S-STATUTORY                      
02462                      ' RESV ', EX-END-ST-RES (1)                  
02463                      ' CERT ', CR-FULL-CONTROL.                   
02464                                                                   
02465  0470-BYPASS-LF-DETAIL.                                           
02466                                                                   
02467      MOVE CALL-EXTRACT TO HOLD-LIFE-RECORD.                       
02468                                                                   
02469      IF CR-ENTRY-STATUS NOT = '9'                                 
02470         PERFORM 1500-RELEASE THRU 1540-RELEASE-EXIT.              
02471                                                                   
02472      GO TO 1000-PROCESS-AH.                                       
02473                                                                   
02474  0480-CALC-LIFE-UNEARN.                                           
02475                                                                   
02476      MOVE +0     TO WS-LEV-LF-BEGIN-RES (YR)                      
02477                     WS-LEV-LF-BEGIN-ALT (YR)                      
02478                     WS-LEV-LF-END-RES (YR)                        
02479                     WS-LEV-LF-END-ALT (YR)                        
02480                     WS-RED-LF-BEGIN-RES (YR)                      
02481                     WS-RED-LF-BEGIN-ALT (YR)                      
02482                     WS-RED-LF-END-RES (YR)                        
02483                     WS-RED-LF-END-ALT (YR)                        
02484                     WRK-B  MEAN-CAL.                              
02485                                                                   
02486  0490-CALC-LEVEL-BEG.                                             
02487                                                                   
02488      MOVE +0        TO WRK-B   WRK-H.                             
02489                                                                   
02490      MOVE WS-LF-BEG-REM-TRM1 (YR) TO M4.                          
02491                                                                   
02492      IF M4 NOT POSITIVE                                           
02493          IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND     
02494             (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L') AND        
02495             (M4 = ZEROS) AND (M6 (YR) GREATER THAN +0)            
02496               NEXT SENTENCE                                       
02497          ELSE                                                     
02498               GO TO 0500-CALC-LEVEL-END.                          
02499                                                                   
02500      PERFORM 0550-CALC-LEVEL-UNEARN THRU 0590-LEVEL-EXIT.         
02501                                                                   
02502      MOVE WRK-B  TO WS-LEV-LF-BEGIN-RES (YR).                     
02503      MOVE WRK-H  TO WS-LEV-LF-BEGIN-ALT (YR).                     
02504                                                                   
02505  0500-CALC-LEVEL-END.                                             
02506                                                                   
02507      MOVE +0        TO WRK-B   WRK-H.                             
02508                                                                   
02509      MOVE WS-LF-END-REM-TRM1 (YR) TO M4.                          
02510                                                                   
02511      IF M4 NOT POSITIVE                                           
02512          IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND     
02513             (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L') AND        
02514             (M4 = ZEROS) AND (M5 (YR) GREATER THAN +0)            
02515               NEXT SENTENCE                                       
02516          ELSE                                                     
02517               GO TO 0510-CALC-REDUCE-BEG.                         
02518                                                                   
02519      PERFORM 0550-CALC-LEVEL-UNEARN THRU 0590-LEVEL-EXIT.         
02520                                                                   
02521      MOVE WRK-B  TO WS-LEV-LF-END-RES (YR).                       
02522      MOVE WRK-H  TO WS-LEV-LF-END-ALT (YR).                       
02523                                                                   
02524  0510-CALC-REDUCE-BEG.                                            
02525                                                                   
02526      MOVE +0   TO WRK-B   WRK-H.                                  
02527                                                                   
02528      IF ENTRY-DT GREATER  BEGIN-DT (YR)                           
02529         GO TO 0520-CALC-REDUCE-END.                               
02530                                                                   
02531      MOVE WS-LF-BEG-REM-TRM1 (YR) TO M4.                          
CIDMOD     MOVE M6 (YR)                TO M9
02532                                                                   
02533      IF M4 NOT POSITIVE                                           
02534          IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND     
02535             (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L') AND        
02536             (M4 = ZEROS) AND (M6 (YR) GREATER THAN +0)            
02537               NEXT SENTENCE                                       
02538          ELSE                                                     
02539               GO TO 0520-CALC-REDUCE-END.                         
02540                                                                   
02541      PERFORM 0600-CALC-REDUCE-UNEARN THRU 0640-REDUCE-EXIT.       
02542                                                                   
02543      MOVE WRK-B  TO WS-RED-LF-BEGIN-RES (YR).                     
02544      MOVE WRK-H  TO WS-RED-LF-BEGIN-ALT (YR).                     
02545                                                                   
02546  0520-CALC-REDUCE-END.                                            
02547                                                                   
02548      MOVE +0   TO WRK-B   WRK-H.                                  
02549                                                                   
02550      IF ENTRY-DT GREATER THAN END-DT (YR)                         
02551         GO TO 0530-CALC-UNEARN-FINISH.                            
02552                                                                   
02553      MOVE WS-LF-END-REM-TRM1 (YR) TO M4.                          
CIDMOD     MOVE M5 (YR)                TO M9
02554                                                                   
02555      IF M4 NOT POSITIVE                                           
02556          IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND     
02557             (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L') AND        
02558             (M4 = ZEROS) AND (M5 (YR) GREATER THAN +0)            
02559               NEXT SENTENCE                                       
02560          ELSE                                                     
02561               GO TO 0530-CALC-UNEARN-FINISH.                      
02562                                                                   
02563      PERFORM 0600-CALC-REDUCE-UNEARN THRU 0640-REDUCE-EXIT.       
02564                                                                   
02565      MOVE WRK-B  TO WS-RED-LF-END-RES (YR).
02566      MOVE WRK-H  TO WS-RED-LF-END-ALT (YR).                       
02567                                                                   
02568  0530-CALC-UNEARN-FINISH.                                         
02569                                                                   
02570  0535-DUMMY-PARA.                                                 
02571                                                                   
02572      IF (M5 (YR) NOT = +0) OR                                     
02573         (M6 (YR) NOT = +0)                                        
02574         PERFORM 0710-LEVEL-MEAN THRU 0730-L-EXIT                  
02575         PERFORM 0740-REDUCING-MEAN THRU  0770-R-EXIT.             
02576                                                                   
02577  0540-LIFE-UNEARN-EXIT.                                           
02578      EXIT.                                                        
02579                                                                   
02580      EJECT                                                        
02581  0550-CALC-LEVEL-UNEARN.                                          
02582                                                                   
02583  0560-CALC-PRO-FACTOR.                                            
02584                                                                   
02585      IF DTE-CLIENT = 'WSL' OR 'FLI' OR 'FLU' OR                   
02586                      'MIC' OR 'MCC'                               
02587         COMPUTE WRK-D ROUNDED = (M4 - .5) / EX-TERM               
02588      ELSE                                                         
02589         COMPUTE WRK-D ROUNDED = M4 / EX-TERM.                     
02590                                                                   
02591      IF WRK-D GREATER +1.000                                      
02592         MOVE +1.000 TO WRK-D.                                     
02593                                                                   
02594  0570-END-CALC-PRO-FACTOR.                                        
02595                                                                   
02596      COMPUTE WRK-B ROUNDED = WRK-D * WRK-C.                       
02597                                                                   
02598      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
02599         GO TO 0590-LEVEL-EXIT.                                    
02600                                                                   
02601      IF CLAS-I-EP (CLAS-INDEXL) NOT = 'B' AND 'K' AND 'L'         
02602         GO TO 0580-COMPUTE-LEV-UNEARN.                            
02603                                                                   
02604      IF CLAS-I-BAL (CLAS-INDEXL) = 'L'                            
02605         GO TO 0580-COMPUTE-LEV-UNEARN.                            
02606                                                                   
02607      IF DTE-CLIENT = 'WSL' OR 'FLI' OR 'FLU' OR                   
02608                      'MIC' OR 'MCC'                               
02609         COMPUTE WRK-D ROUNDED = (M4 + .5) / (EX-TERM + 1)         
02610      ELSE                                                         
02611         COMPUTE WRK-D ROUNDED = (M4 + 1) / (EX-TERM + 1).         
02612                                                                   
02613  0580-COMPUTE-LEV-UNEARN.                                         
02614                                                                   
02615      COMPUTE WRK-H ROUNDED = WRK-D * WRK-ALT.                     
02616                                                                   
02617  0590-LEVEL-EXIT.                                                 
02618      EXIT.                                                        
02619                                                                   
02620      EJECT                                                        
02621  0600-CALC-REDUCE-UNEARN.                                         
02622                                                                   
02623  0610-CALC-R78-FACTOR.                                            
02624                                                                   
02625      COMPUTE WRK-F = M4 * (M4 + +1).                              
02626                                                                   
02627      IF DTE-R78 = '1' OR DTE-CLIENT = 'FLI'                       
02628          COMPUTE WRK-F = M4 * M4.                                 
02629                                                                   
02630      IF DTE-CLIENT = 'MPP'                                        
02631         COMPUTE WRK-F = (EX-TERM - M4) *                          
02632                        ((EX-TERM - M4) + +1).                     
02633                                                                   
02634      COMPUTE WRK-G = EX-TERM * (EX-TERM + +1).                    
02635      COMPUTE WRK-E ROUNDED = WRK-F / WRK-G.                       
02636                                                                   
02637      IF WRK-E GREATER +1.000                                      
02638          MOVE +1.000 TO WRK-E.                                    
02639                                                                   
02640  0620-CALC-R78-FACTOR-END.                                        
02641                                                                   
02642      COMPUTE WRK-B ROUNDED = WRK-E * WRK-C.                       
02643                                                                   
02644      IF CLAS-I-BAL (CLAS-INDEXL) = 'B'                            
02645         GO TO 0640-REDUCE-EXIT.                                   
02646                                                                   
02647      IF CLAS-I-EP (CLAS-INDEXL) = 'U'                             
02648          MOVE 'R' TO CLAS-I-EP (CLAS-INDEXL).                     

032609     IF CLAS-I-EP (CLAS-INDEXL) = 'T'
032609        IF (DTE-CLIENT = 'CID')
032609           AND (STATE-ABBR (CLAS-INDEXS) = 'TX' OR 'VA')
032609           CONTINUE
032609        ELSE
032609           PERFORM 0650-TEXAS-REG-RED
032609                                 THRU 0660-TEXAS-REG-RED-X
032609           GO TO 0630-COMPUTE-RED-UNEARN
032609        END-IF
032609     END-IF

02654      IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'               
02655         PERFORM 0690-BALLOON-RED THRU 0700-BALLOON-RED-EXIT       
02656         COMPUTE WRK-H ROUNDED = WRK-E * WRK-ALT                   
02657         GO TO 0640-REDUCE-EXIT.                                   
02658                                                                   
062712     if dte-client = 'AHL'
062712        go to 0640-reduce-exit
062712     end-if

02659      IF STATE-ABBR (CLAS-INDEXS) = 'AL'                           
02660          GO TO 0640-REDUCE-EXIT.                                  
02661                                                                   
02662      IF (DTE-CLIENT = 'NCL') AND                                  
02663         (CLAS-I-EP (CLAS-INDEXL) = 'N') AND                       
02664         (EX-EFF-DATE  LESS THAN  19910101)                        
02665          GO TO 0630-COMPUTE-RED-UNEARN.                           
02666                                                                   
CIDMOD     IF (DTE-CLIENT = 'CID') AND
032609        ((STATE-ABBR (CLAS-INDEXS) = 'TX' OR 'VA')
032609*       (EX-EFF-DATE > 19911231)
                    OR
120710        ((STATE-ABBR (CLAS-INDEXS) = 'MN')
120710         AND (EX-EFF-DATE > 20091231)))
CIDMOD        PERFORM 0800-RULE-ANTIC  THRU 0800-EXIT
CIDMOD        GO TO 0640-REDUCE-EXIT                                    
CIDMOD     END-IF
CIDMOD
02667      IF (CLAS-I-EP (CLAS-INDEXL) = 'N') AND                       
02668         (DTE-CLIENT NOT = 'MON') AND                              
02669         (CR-APR NOT = +0)                                         
02670          PERFORM 0670-NET-PAY-RED THRU 0680-NET-PAY-RED-X         
02671          GO TO 0630-COMPUTE-RED-UNEARN.                           
02672                                                                   
CIDMOD*    IF STATE-ABBR (CLAS-INDEXS) = 'OH'                           
02673      IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND                     
CIDMOD        (EX-CAL-TABLE NOT = 'L ')
02674          IF (EX-TERM GREATER THAN +60)  AND                       
02675             (EX-APR GREATER THAN +0)  AND                         
02676             (EX-EFF-DATE  GREATER THAN  19831031)                 
02677              PERFORM 0670-NET-PAY-RED THRU 0680-NET-PAY-RED-X     
02678              GO TO 0630-COMPUTE-RED-UNEARN.                       
02679                                                                   
02680      IF STATE-ABBR (CLAS-INDEXS) = 'MT'                           
02681          IF (EX-TERM GREATER THAN +61)  AND                       
02682             (EX-APR GREATER THAN ZERO)  AND                       
02683             (EX-EFF-DATE  GREATER THAN  19830318)                 
02684              PERFORM 0670-NET-PAY-RED THRU 0680-NET-PAY-RED-X     
02685              GO TO 0630-COMPUTE-RED-UNEARN.                       
02686                                                                   
02687      IF STATE-ABBR (CLAS-INDEXS) = 'UT'                           
02688          IF (EX-TERM GREATER THAN +62)  AND                       
02689             (EX-APR GREATER THAN ZERO)  AND                       
02690             (EX-EFF-DATE  GREATER THAN  19810831) AND             
02691             (EX-EFF-DATE  LESS THAN  19830901)                    
02692              PERFORM 0670-NET-PAY-RED THRU 0680-NET-PAY-RED-X     
02693              GO TO 0630-COMPUTE-RED-UNEARN.                       
02694                                                                   
02695      IF STATE-ABBR (CLAS-INDEXS) = 'RI'                           
02696          IF (EX-TERM GREATER THAN +60)  AND                       
02697             (EX-APR GREATER THAN ZERO)  AND                       
02698             (EX-EFF-DATE  GREATER THAN  19831231)                 
02699              PERFORM 0670-NET-PAY-RED THRU 0680-NET-PAY-RED-X     
02700              GO TO 0630-COMPUTE-RED-UNEARN.                       
02701                                                                   
02702  0630-COMPUTE-RED-UNEARN.                                         
02703      COMPUTE WRK-B ROUNDED = WRK-E * WRK-C.                       
02704                                                                   
02705  0640-REDUCE-EXIT.                                                
02706      EXIT.                                                        
02707                                                                   
02708      EJECT                                                        
02709  0650-TEXAS-REG-RED.                                              
02710                                                                   
02711      MOVE +0            TO WRK-B.                                 
02712      IF M4 NOT POSITIVE                                           
02713          GO TO 0660-TEXAS-REG-RED-X.                              
02714                                                                   
02715      IF M4 NOT LESS EX-TERM                                       
02716          MOVE WRK-C     TO WRK-B                                  
02717          MOVE +1        TO WRK-E                                  
02718          GO TO 0660-TEXAS-REG-RED-X.                              
02719                                                                   
02720      COMPUTE TEX-FACT-4 =                                         
02721          (EX-TERM * EX-TERM) + (EX-PMT-FREQ * EX-TERM).           
02722      DIVIDE M4 BY EX-PMT-FREQ                                     
02723          GIVING TEX-FACT-5                                        
02724          REMAINDER TEX-FACT-6.                                    
02725      COMPUTE TEX-FACT-5 = TEX-FACT-5 * EX-PMT-FREQ.               
02726      COMPUTE TEX-FACT-7 = (TEX-FACT-5 * TEX-FACT-5) +             
02727          (TEX-FACT-5 * EX-PMT-FREQ) +                             
02728          (2 * (TEX-FACT-6 * (TEX-FACT-5 + EX-PMT-FREQ))).         
02729      COMPUTE TEX-FACT-8 ROUNDED = TEX-FACT-7 / TEX-FACT-4.        
02730      MOVE TEX-FACT-8   TO WRK-E.                                  
02731                                                                   
02732  0660-TEXAS-REG-RED-X.                                            
02733      EXIT.                                                        
02734                                                                   
02735  0670-NET-PAY-RED.                                                
02736                                                                   
02737      MOVE ZERO          TO WRK-B.                                 
02738      IF M4 NOT POSITIVE                                           
02739          GO TO 0680-NET-PAY-RED-X.                                
02740                                                                   
02741      IF M4 NOT LESS EX-TERM                                       
02742          MOVE WRK-C     TO WRK-B                                  
02743          MOVE +1        TO WRK-E                                  
02744          GO TO 0680-NET-PAY-RED-X.                                
02745                                                                   
02746      MOVE EX-TERM       TO NP-ORIG, NP-CAP.                       
02747      MOVE EX-APR        TO NP-APR.                                
02748      MOVE M4            TO NP-REM.                                
02749                                                                   
02750      MOVE CLAS-I-BAL (CLAS-INDEXL) TO NP-OPT.                     
02751      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
02752         MOVE EX-CAP-TERM TO NP-ORIG.                              
02753                                                                   
02754      MOVE 'R' TO NP-OPT.                                          
02755      CALL 'ECSNETRM' USING                                        
02756          NP-APR, NP-ORIG, NP-REM, NP-OPT, NP-CAP, NP-FACTOR.      
02757                                                                   
02758      MOVE NP-FACTOR  TO WRK-E.                                    
02759                                                                   
02760  0680-NET-PAY-RED-X.                                              
02761      EXIT.                                                        
02762                                                                   
02763  0690-BALLOON-RED.                                                
02764                                                                   
02765      IF CLAS-I-BAL (CLAS-INDEXL) = 'L'                            
02766         GO TO 0700-BALLOON-RED-EXIT.                              
02767                                                                   
02768      IF DTE-R78 = '1' OR DTE-CLIENT = 'FLI'                       
02769         COMPUTE WRK-F = (M4 + 1) * (M4 + 1)                       
02770      ELSE                                                         
02771         COMPUTE WRK-F = (M4 + 1) * (M4 + 2).                      
02772                                                                   
02773      IF DTE-CLIENT = 'MPP'                                        
02774         COMPUTE WRK-F = ((EX-TERM + 1) - (M4 + 1)) *              
02775                 (((EX-TERM + 1) - (M4 + 1)) + 1).                 
02776                                                                   
02777      COMPUTE WRK-G = (EX-TERM + 1) * (EX-TERM + 2).               
02778      COMPUTE WRK-E ROUNDED = WRK-F / WRK-G.                       
02779                                                                   
02780  0700-BALLOON-RED-EXIT.                                           
02781      EXIT.                                                        
02782      EJECT                                                        
02783                                                                   
02784  0710-LEVEL-MEAN.                                                 
02785                                                                   
02786      MOVE +0 TO WS-LF-LEV-MEAN-INFORCE (YR).                      
02787                                                                   
02788      IF RR (YR) NOT = +0                                          
02789         MOVE EX-INITIAL-AMT TO WS-LF-LEV-MEAN-INFORCE (YR)        
02790         ADD CR-LFAMT-ALT    TO WS-LF-LEV-MEAN-INFORCE (YR).       
02791                                                                   
02792  0720-IBNR-CALC.                                                  
02793                                                                   
062712     IF DTE-CLIENT = 'MON' OR 'DCC' OR 'CID' OR 'AHL'
02795          GO TO 0730-L-EXIT.                                       
02796                                                                   
02797      IF M6 (YR) POSITIVE                                          
02798         COMPUTE EX-B-IBNR (YR) =                                  
02799                 EX-B-IBNR (YR) +                                  
02800                 (MEAN-CAL * .0005).                               
02801                                                                   
02802      IF M5 (YR) POSITIVE                                          
02803         COMPUTE EX-E-IBNR (YR) =                                  
02804                 EX-E-IBNR (YR) +                                  
02805                 (MEAN-CAL * .0005).                               
02806                                                                   
02807  0730-L-EXIT.                                                     
02808      EXIT.                                                        
02809                                                                   
02810  0740-REDUCING-MEAN.                                              
02811                                                                   
02812      MOVE +0             TO MEAN-CAL                              
02813                             WS-LF-RED-MEAN-INFORCE (YR).          
02814                                                                   
02815      DIVIDE EX-TERM INTO EX-INITIAL-AMT GIVING WRK-A.             
02816      MOVE RR (YR)        TO  WORK-M4.                             
02817                                                                   
062712     IF DTE-CLIENT NOT = 'MON' AND 'CID' AND 'DCC' AND 'AHL'
02819          IF M6 (YR) POSITIVE                                      
02820              COMPUTE EX-B-IBNR (YR) =                             
02821              EX-B-IBNR (YR) + ((M6 (YR) * WRK-A) * .0005).        
02822                                                                   
02823  0750-R-MEAN.                                                     
02824                                                                   
02825      COMPUTE  MEAN-CAL  =  MEAN-CAL  +  (M1 (YR) * WRK-A) +       
02826                            CR-LFAMT-ALT.                          
02827      COMPUTE  M1 (YR) =  M1 (YR) - +1.                            
02828      COMPUTE  WORK-M4  =  WORK-M4  -  +1.                         
02829                                                                   
02830      IF WORK-M4  GREATER  ZERO                                    
02831          GO  TO  0750-R-MEAN.                                     
02832                                                                   
02833      IF RR (YR) NOT = +0                                          
02834         DIVIDE RR (YR) INTO MEAN-CAL GIVING MEAN-CAL.             
02835                                                                   
02836      MOVE MEAN-CAL TO WS-LF-RED-MEAN-INFORCE (YR).                
02837                                                                   
062712     IF DTE-CLIENT NOT = 'MON' AND 'CID' AND 'DCC' AND 'AHL'
02839          IF M5 (YR) POSITIVE                                      
02840              COMPUTE EX-E-IBNR (YR) =                             
02841              EX-E-IBNR (YR) + ((M5 (YR) * WRK-A) * .0005).        
02842                                                                   
02843      GO TO 0770-R-EXIT.                                           
02844                                                                   
02845  0760-R-NET-PAY-MEAN.                                             
02846                                                                   
02847      MOVE M3 (YR)                  TO NP-ORIG NP-REM NP-CAP.      
02848      MOVE EX-APR                   TO NP-APR.                     
02849      MOVE CLAS-I-BAL (CLAS-INDEXL) TO NP-OPT.                     
02850                                                                   
02851      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
02852         MOVE EX-CAP-TERM TO NP-ORIG.                              
02853                                                                   
02854      CALL 'ECSNETRM' USING                                        
02855         NP-APR, NP-ORIG, NP-REM, NP-OPT, NP-CAP,  NP-FACTOR.      
02856                                                                   
02857      COMPUTE NP-WORK1 = NP-FACTOR * 6.5                           
02858      MOVE M2 (YR)                  TO NP-ORIG NP-REM NP-CAP.      
02859      MOVE EX-APR                   TO NP-APR.                     
02860      MOVE CLAS-I-BAL (CLAS-INDEXL) TO NP-OPT.                     
02861                                                                   
02862      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
02863         MOVE EX-CAP-TERM TO NP-ORIG.                              
02864                                                                   
02865      CALL 'ECSNETRM' USING                                        
02866         NP-APR, NP-ORIG, NP-REM, NP-OPT, NP-CAP, NP-FACTOR.       
02867                                                                   
02868      COMPUTE  NP-WORK2 = NP-FACTOR * 6.5                          
02869      SUBTRACT NP-WORK2 FROM NP-WORK1.                             
02870                                                                   
02871      COMPUTE MEAN-CAL = NP-WORK1 * (EX-INITIAL-AMT / +1000)       
02872      IF RR (YR) NOT = +0                                          
02873         DIVIDE RR (YR) INTO MEAN-CAL GIVING MEAN-CAL.             
02874                                                                   
02875      MOVE MEAN-CAL TO WS-LF-RED-MEAN-INFORCE (YR).                
02876                                                                   
02877  0770-R-EXIT.                                                     
02878      EXIT.                                                        
02879      EJECT                                                        
02880                                                                   
CIDMOD 0800-RULE-ANTIC.

           PERFORM 1700-CALC-REM-AMT   THRU 1799-EXIT

           MOVE EFFECT-DT              TO  CP-CERT-EFF-DT

           IF CR-LOAN-1ST-PMT-DT IS NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD
              MOVE '3'                 TO  DC-OPTION-CODE
              PERFORM 6150-DATE-CONVERSION-ROUTINE
              MOVE DC-BIN-DATE-1       TO  CP-FIRST-PAY-DATE
           ELSE
              MOVE CP-CERT-EFF-DT      TO  DC-BIN-DATE-1
              MOVE +1                  TO  DC-ELAPSED-MONTHS
              MOVE +0                  TO  DC-ELAPSED-DAYS
              MOVE '6'                 TO  DC-OPTION-CODE
              PERFORM 6150-DATE-CONVERSION-ROUTINE
              MOVE DC-BIN-DATE-2       TO  CP-FIRST-PAY-DATE
           END-IF
           MOVE M4                     TO  CP-REMAINING-TERM
           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO CP-BENEFIT-TYPE
           MOVE SPACES                 TO CP-ACCT-FLD-5
           MOVE DTE-CLIENT             TO CP-COMPANY-ID
           MOVE CR-AGE                 TO CP-ISSUE-AGE
           MOVE CR-LFPRM               TO CP-ORIGINAL-PREMIUM
           MOVE CR-LF-CRIT-PERIOD      TO CP-CRITICAL-MONTHS
           MOVE CR-LF-TERM             TO CP-ORIGINAL-TERM
                                          CP-LOAN-TERM
           MOVE CR-LFAMT               TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
           MOVE WS-REM-AMT             TO CP-REMAINING-BENEFIT
           MOVE CR-APR                 TO CP-LOAN-APR
           MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY
           MOVE LF-EARN-CODE           TO CP-RATING-METHOD
           MOVE '6'                    TO CP-EARNING-METHOD
           MOVE '3'                    TO CP-PROCESS-TYPE
           MOVE ' '                    TO CP-SPECIAL-CALC-CD
           IF CR-RATING-CLASS NOT = SPACES AND ZEROS
              MOVE CR-RATING-CLASS     TO CP-CLASS-CODE
           ELSE
              MOVE AM-CAL-TABLE        TO CP-CLASS-CODE
           END-IF

           MOVE CR-LF-DEV-CODE         TO  CP-DEVIATION-CODE

           MOVE CR-LF-DEV-PCT              TO CP-RATE-DEV-PCT
           MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE
           MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV
           MOVE CR-LFTYP                   TO CP-BENEFIT-CD
           MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD
           MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS
           MOVE LIFE-OVERRIDE-L1           TO CP-LIFE-OVERRIDE-CODE

           IF CP-STATE-STD-ABBRV = 'OR'
              COMPUTE CP-RATING-BENEFIT-AMT = CR-LFAMT + CR-LFAMT-ALT
           END-IF

           IF (CP-RATE-AS-STANDARD) AND (CP-EARN-AS-NET-PAY)
              COMPUTE CP-ORIGINAL-BENEFIT =
               (CR-LFAMT - CR-LFPRM)
           END-IF

           IF CP-TRUNCATED-LIFE
              MOVE CR-LOAN-TERM            TO CP-LOAN-TERM
           END-IF

           IF CP-TERM-IS-DAYS
              MOVE CR-LF-TERM-IN-DAYS    TO CP-TERM-OR-EXT-DAYS
           ELSE
              MOVE ZEROS                 TO CP-TERM-OR-EXT-DAYS
           END-IF

           CALL 'ELRFNDX' USING CALCULATION-PASS-AREA
           MOVE CP-CALC-REFUND         TO WRK-B

           .
CIDMOD 0800-EXIT.
           EXIT.

02881  1000-PROCESS-AH.                                                 
02882                                                                   
02883      MOVE ZEROS               TO  EX-EXIT-DATE.                   
02884      MOVE INITIALIZED-BACK    TO  EX-DATA-BY-YEAR (1)             
02885                                   EX-DATA-BY-YEAR (2)             
02886                                   EX-DATA-BY-YEAR (3).            
02887                                                                   
02888      IF CR-AHTYP = ZEROS OR SPACES                                
02889         GO TO 1400-CHECK-IF-REINSURANCE.                          
02890                                                                   
02891      IF CR-AH-DEV-PCT NOT NUMERIC                                 
02892          MOVE ZEROS TO CR-AH-DEV-PCT.                             
02893                                                                   
02894      IF DTE-CLIENT  = 'CRI'                                       
02895          MOVE WK-AH-DEV-CODE     TO  EX-STATE-DEV                 
02896      ELSE                                                         
02897          IF CR-AH-DEV-PCT NOT = +0 AND +1.0                       
02898              COMPUTE WK-TEMP-DEV-PCT =                            
02899                              CR-AH-DEV-PCT * WK-DEV-FACTOR        
02900              MOVE WK-TEMP-DEV    TO  EX-STATE-DEV                 
02901          ELSE                                                     
02902              MOVE CR-AH-DEV-CODE TO  EX-STATE-DEV.                
02903                                                                   
02904      MOVE 'A'             TO EX-LIFE-AH.                          
02905                                                                   
02906  1010-CHECK-AH-STATUS.                                            
02907                                                                   
02908      MOVE +2 TO LIFE-AH-SUB.                                      
02909      MOVE +1 TO YR.                                               
02910                                                                   
02911      IF CR-AH-CURRENT-STATUS NOT = '6' AND '7' AND '8'            
02912         GO TO 1020-FIND-AH-LOOP.                                  
02913                                                                   
02914      MOVE CR-AH-CANCEL-EXIT-DATE     TO DC-GREG-DATE-CYMD.        
02915      MOVE 'L'                        TO DC-OPTION-CODE.           
02916      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
02917      IF NO-CONVERSION-ERROR                                       
02918         MOVE DC-BIN-DATE-1            TO AH-CAN                   
02919      ELSE                                                         
02920         MOVE LOW-VALUES               TO AH-CAN.                  
02921                                                                   
02922      MOVE CR-AH-SETTLEMENT-EXIT-DATE TO DC-GREG-DATE-CYMD.        
02923      MOVE 'L'                        TO DC-OPTION-CODE.           
02924      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
02925      IF NO-CONVERSION-ERROR                                       
02926         MOVE DC-BIN-DATE-1            TO AH-CLM                   
02927      ELSE                                                         
02928         MOVE LOW-VALUES               TO AH-CLM.                  
02929                                                                   
02930      IF AH-CAN = LOW-VALUES AND                                   
02931         AH-CLM = LOW-VALUES                                       
02932         GO TO 1020-FIND-AH-LOOP.                                  
02933                                                                   
02934      IF AH-CAN = LOW-VALUES AND                                   
02935         AH-CLM NOT = LOW-VALUES                                   
02936         MOVE CR-AH-SETTLEMENT-EXIT-DATE TO EX-EXIT-DATE           
02937         GO TO 1020-FIND-AH-LOOP.                                  
02938                                                                   
02939      IF AH-CLM = LOW-VALUES                                       
02940          MOVE CR-AH-CANCEL-EXIT-DATE TO EX-EXIT-DATE              
02941          GO TO 1020-FIND-AH-LOOP.                                 
02942                                                                   
02943      IF AH-CAN LESS THAN AH-CLM                                   
02944          MOVE CR-AH-CANCEL-EXIT-DATE TO EX-EXIT-DATE              
02945          GO TO 1020-FIND-AH-LOOP.                                 
02946                                                                   
02947      MOVE CR-AH-SETTLEMENT-EXIT-DATE TO EX-EXIT-DATE.             
02948                                                                   
02949  1020-FIND-AH-LOOP.                                               
02950                                                                   
02951                                                                   
02952      IF CLAS-INDEXA GREATER THAN CLAS-MAXA                        
02953         MOVE ' INVALID AH   TYPE ' TO WS-ABEND-MESSAGE            
02954         MOVE CR-AHTYP TO WS-ABEND-FILE-STATUS                     
02955         GO TO ABEND-PGM.                                          
02956                                                                   
02957      IF CR-AHTYP NOT = CLAS-I-BEN (CLAS-INDEXA)                   
02958         ADD +1 TO CLAS-INDEXA                                     
02959         GO TO 1020-FIND-AH-LOOP.                                  
02960                                                                   
02961      MOVE CLAS-I-EP (CLAS-INDEXA) TO AH-EARN-METHOD.              
02962                                                                   
PEMMOD*    IF CLAS-I-RL-AH (CLAS-INDEXA) = 'A'                          
PEMMOD*       IF AM-EARN-METHOD-A NOT = SPACES AND ZEROS                
PEMMOD*          MOVE AM-EARN-METHOD-A TO AH-EARN-METHOD.               
02966                                                                   
062712     if dte-client = 'AHL'
062712        move 'R'                 to type-method
062712     else
02967         PERFORM 1300-FIND-METHOD THRU 1310-METHOD-EXIT
062712     end-if
02968                                                                   
02969                                                                   
02970                                                                   
02971      MOVE EX-EXIT-DATE               TO DC-GREG-DATE-CYMD.        
02972      MOVE 'L'                        TO DC-OPTION-CODE.           
02973      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
02974      IF NO-CONVERSION-ERROR                                       
02975         MOVE DC-BIN-DATE-1            TO EXIT-DT                  
02976      ELSE                                                         
02977         MOVE LOW-VALUES               TO EXIT-DT.                 
02978                                                                   
02979      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
02980      MOVE 'L'                        TO DC-OPTION-CODE.           
02981      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
02982      IF NO-CONVERSION-ERROR                                       
02983         MOVE DC-BIN-DATE-1            TO EFFECT-DT                
02984      ELSE                                                         
02985         MOVE LOW-VALUES               TO EFFECT-DT.               
02986                                                                   
02987      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
02988          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
02989                                                                   
02990      MOVE LOW-VALUES             TO     FIRST-PAY-DT.             
02991                                                                   
02992      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
02993          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
02994          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
02995                                         DC-ELAPSED-DAYS           
02996          MOVE '3'                    TO DC-OPTION-CODE            
02997          PERFORM 6150-DATE-CONVERSION-ROUTINE                     
02998          IF NO-CONVERSION-ERROR                                   
02999              MOVE DC-BIN-DATE-1      TO FIRST-PAY-DT              
03000          ELSE                                                     
03001              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
03002                                                                   
03003      IF FIRST-PAY-DT  LESS THAN  EFFECT-DT                        
03004          MOVE ZEROS  TO  CR-LOAN-1ST-PMT-DT.                      
03005                                                                   
03006      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
03007          MOVE EFFECT-DT              TO DC-BIN-DATE-1             
03008          MOVE +1                     TO DC-ELAPSED-MONTHS         
03009          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
03010          MOVE '6'                    TO DC-OPTION-CODE            
03011          PERFORM 6150-DATE-CONVERSION-ROUTINE                     
03012          MOVE DC-BIN-DATE-2          TO FIRST-PAY-DT              
03013          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
03014                                                                   
03015      MOVE FIRST-PAY-DT            TO DC-BIN-DATE-1.               
03016      COMPUTE DC-ELAPSED-MONTHS = CR-AH-TERM - +1                  
03017      MOVE ZEROS                   TO DC-ELAPSED-DAYS.             
03018      MOVE '6'                     TO DC-OPTION-CODE.              
03019      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03020      IF NO-CONVERSION-ERROR                                       
03021          MOVE DC-BIN-DATE-2        TO EXPIRE-DT                   
03022      ELSE                                                         
03023          MOVE LOW-VALUES           TO EXPIRE-DT.                  
03024                                                                   
03025      IF (EXPIRE-DT = LOW-VALUES)      OR                          
03026          (DTE-REM-TRM-CALC-OPTION = '1' OR '2')                   
03027         MOVE EFFECT-DT               TO DC-BIN-DATE-1             
03028         MOVE CR-AH-TERM              TO DC-ELAPSED-MONTHS         
03029         MOVE ZEROS                   TO DC-ELAPSED-DAYS           
03030         MOVE '6'                     TO DC-OPTION-CODE            
03031         PERFORM 6150-DATE-CONVERSION-ROUTINE                      
03032         IF NO-CONVERSION-ERROR                                    
03033            MOVE DC-BIN-DATE-2        TO EXPIRE-DT                 
03034         ELSE                                                      
03035            MOVE LOW-VALUES           TO EXPIRE-DT.                

032609     MOVE CR-AH-EXPIRE-DATE      TO DC-GREG-DATE-CYMD
032609     MOVE 'L'                    TO DC-OPTION-CODE
032609     PERFORM 6150-DATE-CONVERSION-ROUTINE
032609     IF NO-CONVERSION-ERROR
032609        MOVE DC-BIN-DATE-1       TO EXPIRE-DT
032609     ELSE
032609        MOVE LOW-VALUES          TO EXPIRE-DT
032609     END-IF

03037      MOVE +0                         TO DC-ELAPSED-MONTHS         
03038                                                                   
03039      MOVE CR-AH-TERM         TO EX-TERM                           
03040                                                                   
03041      IF CR-AH-TERM LESS THAN +60                                  
03042         MOVE '1' TO EX-TERM-CD                                    
03043      ELSE                                                         
03044         MOVE '2' TO EX-TERM-CD.                                   
03045                                                                   
03046      MOVE CR-AHTYP           TO EX-BEN-TYPE.                      
03047                                                                   
03048      MOVE CR-AHPRM          TO EX-PREM                            
03049                                EX-PRIM-FAC-PREM                   
03050                                WS-HOLD-AH-PRIM-FAC.               
03051                                                                   
03052  1035-A-H-SUMMARY-ADJUSTMENTS.                                    
03053                                                                   
03054      IF CLAS-I-CALC-TYPE (CLAS-INDEXA) NOT = 'Z'                  
03055          GO TO 1040-CHECK-AH-PERIOD-INFORCE.                      
03056                                                                   
03057      IF CR-AHPRM NOT GREATER CR-AHRFND                            
03058          MOVE ZERO                   TO EX-BENEFIT                
03059                                         EX-PREM                   
03060      ELSE                                                         
03061          IF CR-AHRFND = ZERO                                      
03062              MOVE CR-AHAMT           TO EX-BENEFIT                
03063              MOVE CR-AHPRM           TO EX-PREM                   
03064          ELSE                                                     
03065              COMPUTE TEMP-3 ROUNDED = CR-AHRFND / CR-AHPRM        
03066              COMPUTE EX-BENEFIT ROUNDED =                         
03067                               CR-AHAMT - (CR-AHAMT * TEMP-3)      
03068              COMPUTE EX-PREM ROUNDED =                            
03069                               CR-AHPRM - CR-AHRFND.               
03070                                                                   
03071      MOVE EX-PREM                    TO CR-AHPRM.                 
03072                                                                   
03073  1040-CHECK-AH-PERIOD-INFORCE.                                    
03074                                                                   
03075      IF YR GREATER THAN NUMBER-OF-PERIODS                         
03076          GO TO 1110-END-PROCESS-AH.                               
03077                                                                   
03078      IF ENTRY-DT GREATER END-DT (YR)                              
03079         GO TO 1110-END-PROCESS-AH.                                
03080                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'                              
03082         MOVE +0                TO EX-PREM                         
03083                                   EX-PRIM-FAC-PREM                
03084                                   WS-HOLD-AH-PRIM-FAC             
03085         GO TO 1080-CALC-AH-REFUND.                                
03086                                                                   
03087      IF CLAS-I-BAL (CLAS-INDEXA) = 'B'                            
03088         GO TO 1060-END-REM-TERM-AH.                               
03089                                                                   
03090      MOVE EFFECT-DT         TO DT-START.                          
03091      IF BEGIN-DT (YR) GREATER THAN DT-START                       
03092         MOVE BEGIN-DT (YR) TO DT-START.                           

032609*    IF EXPIRE-DT NOT GREATER THAN END-DT (YR)                    
032609*       MOVE EXPIRE-DT TO DT-END                                  
032609*    ELSE                                                         
032609*       MOVE END-DT (YR) TO DT-END.                               
032609
032609     IF EXPIRE-DT < END-DT (YR)
032609        MOVE EXPIRE-DT           TO DT-END
032609     ELSE
032609        MOVE END-DT (YR)         TO DT-END
032609     END-IF

03099      IF EXIT-DT NOT = LOW-VALUES                                  
03100         IF EXIT-DT LESS THAN DT-END                               
03101            MOVE EXIT-DT TO DT-END.                                
03102                                                                   
03103      MOVE DT-START                   TO DC-BIN-DATE-1.            
03104      MOVE DT-END                     TO DC-BIN-DATE-2.            
03105      MOVE '1'                        TO DC-OPTION-CODE.           
03106      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03107      IF NO-CONVERSION-ERROR                                       
03108         MOVE DC-ELAPSED-MONTHS       TO RR (YR)                   
03109      ELSE                                                         
03110         MOVE +0                      TO RR (YR).                  
03111                                                                   
03112      IF RR (YR) LESS THAN +0                                      
03113         MOVE +0 TO RR (YR).                                       
03114                                                                   
03115      MOVE EXPIRE-DT TO DT-END.                                    
03116                                                                   
03117      IF EXIT-DT NOT = LOW-VALUES                                  
03118         IF EXIT-DT LESS THAN DT-END                               
03119             IF EXIT-DT NOT GREATER THAN END-DT (YR)               
03120                 MOVE EXIT-DT TO DT-END.                           
03121                                                                   
03122      MOVE DT-START                   TO DC-BIN-DATE-1.            
03123      MOVE DT-END                     TO DC-BIN-DATE-2.            
03124      MOVE '1'                        TO DC-OPTION-CODE.           
03125      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03126      IF NO-CONVERSION-ERROR                                       
03127         MOVE DC-ELAPSED-MONTHS       TO M1 (YR)                   
03128      ELSE                                                         
03129         MOVE +0                      TO M1 (YR).                  
03130                                                                   
03131      IF M1 (YR) LESS THAN +0                                      
03132         MOVE +0 TO M1 (YR).                                       

032609     IF EXPIRE-DT < END-DT (YR)
032609        MOVE +0                  TO M2 (YR) M5 (YR)
032609        GO TO 1050-REM-TERM-AH-BEGIN
032609     END-IF
032609
032609     IF EXIT-DT NOT = LOW-VALUES
032609        IF EXIT-DT <= END-DT (YR)                       
032609           MOVE +0               TO M2 (YR) M5 (YR)
032609        GO TO 1050-REM-TERM-AH-BEGIN
032609        END-IF
032609     END-IF
032609
032609*    IF DT-END <= END-DT (YR)                       
032609*        MOVE +0 TO M2 (YR) M5 (YR)                               
032609*        GO TO 1050-REM-TERM-AH-BEGIN.                            

03138      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
03139      MOVE 'L'                        TO DC-OPTION-CODE.           
03140      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03141      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
03142                                                                   
03143      MOVE ENDING-DATE(YR)            TO DC-GREG-DATE-CYMD.        
03144      MOVE 'L'                        TO DC-OPTION-CODE.           
03145      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03146      MOVE DC-BIN-DATE-1              TO CP-VALUATION-DT.          
03147                                                                   
03148      MOVE ZEROS                      TO CP-TERM-OR-EXT-DAYS.      
03149                                                                   
03150      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM          
03151                                         CP-LOAN-TERM.             
03152      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
03153      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
03154      MOVE 'N' TO FROM-LIFE-SW.                                    
03155      PERFORM 6000-CALC-REM-TERM THRU 6010-REM-TERM-EXIT.          
03156                                                                   
03157      IF REM-TRM2 GREATER CR-AH-TERM                               
03158         MOVE CR-AH-TERM TO M2 (YR) M5 (YR)                        
03159                            WS-AH-END-REM-TRM1 (YR)                
03160      ELSE                                                         
03161         MOVE REM-TRM1 TO M2 (YR)                                  
03162                          WS-AH-END-REM-TRM1 (YR)                  
03163         MOVE REM-TRM2 TO M5 (YR).                                 
03164                                                                   
03165  1050-REM-TERM-AH-BEGIN.                                          
03166                                                                   
03167      IF (ENTRY-DT GREATER BEGIN-DT (YR)) OR                       
03168         (DT-END NOT GREATER BEGIN-DT (YR))                        
03169         MOVE +0 TO M3 (YR) M6 (YR)                                
03170         GO TO 1060-END-REM-TERM-AH.                               
03171                                                                   
03172      MOVE CR-DT                      TO DC-GREG-DATE-CYMD.        
03173      MOVE 'L'                        TO DC-OPTION-CODE.           
03174      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03175      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
03176                                                                   
03177      MOVE BEGIN-DATE(YR)             TO DC-GREG-DATE-CYMD.        
03178      MOVE 'L'                        TO DC-OPTION-CODE.           
03179      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03180      MOVE DC-BIN-DATE-1              TO CP-VALUATION-DT.          
03181                                                                   
03182      MOVE ZEROS                      TO CP-TERM-OR-EXT-DAYS.      
03183                                                                   
03184      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM          
03185                                         CP-LOAN-TERM.             
03186                                                                   
03187      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
03188      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
03189      MOVE 'N' TO FROM-LIFE-SW.                                    
03190      PERFORM 6000-CALC-REM-TERM THRU 6010-REM-TERM-EXIT.          
03191                                                                   
03192      IF REM-TRM2 GREATER CR-AH-TERM                               
03193         MOVE CR-AH-TERM TO M3 (YR) M6 (YR)                        
03194                            WS-AH-BEG-REM-TRM1 (YR)                
03195      ELSE                                                         
03196         MOVE REM-TRM1 TO M3 (YR)                                  
03197                          WS-AH-BEG-REM-TRM1 (YR)                  
03198         MOVE REM-TRM2 TO M6 (YR).                                 
03199                                                                   
03200  1060-END-REM-TERM-AH.                                            
03201                                                                   
03202      IF M5 (YR) GREATER THAN +0                                   
03203          IF CLAS-I-CALC-TYPE (CLAS-INDEXA) = 'Z'                  
03204              COMPUTE EX-C-CNT (YR) = CR-LIVES -                   
03205                                      CR-SUM-CAN-CNT-ITD           
03206          ELSE                                                     
03207              MOVE +1 TO EX-C-CNT (YR).                            
03208                                                                   
03209      IF DEBUG-AH-INFORCE-CNT                                      
03210          IF (YR = +1) AND                                         
03211             (EX-C-CNT (YR) GREATER THAN ZEROS)                    
03212              ADD EX-C-CNT (YR) TO WS-S-INFORCE-CNT                
03213              DISPLAY ' CERT ', CR-FULL-CONTROL                    
03214                      ' CNT ', EX-C-CNT (YR)                       
03215                      ' TOT ', WS-S-INFORCE-CNT.                   
03216                                                                   
03217      MOVE +0 TO SA.                                               
03218                                                                   
03219  1070-CALC-AH-COMMISSIONS.                                        
03220                                                                   
03221      IF (ENTRY-DT GREATER THAN END-DT (YR))                       
03222        OR                                                         
03223         (ENTRY-DT NOT GREATER THAN BEGIN-DT (YR))                 
03224         GO TO 1080-CALC-AH-REFUND.                                
03225                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'
03227         GO TO 1080-CALC-AH-REFUND.                                
03228                                                                   
03229      ADD +1 TO SA.                                                
03230                                                                   
03231      IF SA GREATER THAN +10                                       
03232         GO TO 1080-CALC-AH-REFUND.                                
03233                                                                   
03234      IF CR-COM-AGT (SA) = ZEROS OR SPACES                         
03235          GO TO 1070-CALC-AH-COMMISSIONS.                          
03236                                                                   
052814     IF CR-AGT-TYPE (SA) = 'C' OR 'D' OR 'F'
03238         COMPUTE AGT-COMM =                                        
03239          CR-AHPRM * CR-LCOM-AH (SA)                               
03240          ADD AGT-COMM TO EX-AGT-COMM (YR)                         
03241      ELSE                                                         
052814        IF CR-AGT-TYPE (SA) = 'O' OR 'P' OR 'G' OR 'B' OR 'S'     
03243            COMPUTE AGT-COMM =                                     
03244             CR-AHPRM * CR-LCOM-AH (SA)                            
03245             ADD AGT-COMM TO EX-OVR-COMM (YR).                     
03246                                                                   
03247      GO TO 1070-CALC-AH-COMMISSIONS.                              
03248                                                                   
03249  1080-CALC-AH-REFUND.                                             
03250                                                                   
03251      IF AH-CAN GREATER THAN BEGIN-DT (YR) AND                     
03252         AH-CAN NOT GREATER THAN END-DT (YR)                       
03253         MOVE CR-AHRFND     TO EX-CNC-AMT (YR)                     
03254      ELSE                                                         
03255         GO TO 1100-FINISH-AH-YEAR.                                
03256                                                                   
03257      MOVE +0 TO SA.                                               
03258                                                                   
03259  1090-CALC-AH-REF-COMM.                                           
03260                                                                   
03261      ADD +1 TO SA.                                                
03262                                                                   
03263      IF SA GREATER THAN +10                                       
03264         GO TO 1100-FINISH-AH-YEAR.                                
03265                                                                   
03266      IF CR-COM-AGT (SA) = ZEROS OR SPACES                         
03267          GO TO 1090-CALC-AH-REF-COMM.                             
03268                                                                   
052814     IF CR-AGT-TYPE (SA) = 'C' OR 'D' OR 'F'
03270         COMPUTE AGT-COMM = -1 * CR-AHRFND * CR-LCOM-AH (SA)       
03271         ADD AGT-COMM TO EX-AGT-COMM (YR)                          
03272      ELSE                                                         
052814        IF CR-AGT-TYPE (SA) = 'O' OR 'P' OR 'G' OR 'B' OR 'S'     
03274            COMPUTE AGT-COMM = -1 * CR-AHRFND * CR-LCOM-AH (SA)    
03275            ADD AGT-COMM TO EX-OVR-COMM (YR).                      
03276                                                                   
03277      GO TO 1090-CALC-AH-REF-COMM.                                 
03278                                                                   
03279  1100-FINISH-AH-YEAR.                                             
03280                                                                   
03281      ADD +1 TO YR.                                                
03282      GO TO 1040-CHECK-AH-PERIOD-INFORCE.                          
03283                                                                   
03284  1110-END-PROCESS-AH.                                             
03285                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'
03287         GO TO 1120-RELEASE-AH-RECORD.                             
03288                                                                   
03289      IF CR-AH-DEV-PCT NOT NUMERIC                                 
03290         MOVE +0       TO CR-AH-DEV-PCT.                           
03291                                                                   
062712     IF DTE-CLIENT = 'CID' OR 'AHL'
CIDMOD        GO TO 1120-RELEASE-AH-RECORD                              
CIDMOD     END-IF
CIDMOD
03292      IF (CR-STATE =  STATE-IL OR STATE-GA OR                      
03293          STATE-NC OR STATE-UT OR STATE-TX OR                      
03294          STATE-IA OR STATE-NE OR STATE-NV OR                      
03295          STATE-OH OR STATE-OR OR STATE-WI)                        
03296                      AND                                          
03297          (RR (1) GREATER THAN +0 OR                               
03298           RR (2) GREATER THAN +0 OR                               
03299           RR (3) GREATER THAN +0)                                 
03300         NEXT SENTENCE                                             
03301      ELSE                                                         
CIDMOD     IF DTE-CLIENT = 'CRI' OR 'CVL' OR 'CID'
03303          NEXT SENTENCE                                            
03304      ELSE                                                         
03305          IF (RR (1) GREATER THAN +0 OR                            
03306              RR (2) GREATER THAN +0 OR                            
03307              RR (3) GREATER THAN +0)                              
03308            AND ((CR-AH-DEV-CODE NOT =                             
03309                    STATE-PRIM-FAC-DEV (CLAS-INDEXS))              
03310            OR (CR-AH-DEV-PCT NOT = +0 AND +1.0))                  
03311              NEXT SENTENCE                                        
03312          ELSE                                                     
03313              GO TO 1120-RELEASE-AH-RECORD.                        
03314                                                                   
03315      MOVE EP-DT                      TO DC-GREG-DATE-CYMD.        
03316      MOVE 'L'                        TO DC-OPTION-CODE.           
03317      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03318      MOVE DC-BIN-DATE-1              TO CP-CERT-EFF-DT.           
03319      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
03320      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
03321      MOVE AH-EARN-METHOD             TO CP-EARNING-METHOD.        
03322      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
03323      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
03324      IF CR-RATING-CLASS NOT = SPACES AND ZEROS                    
03325         MOVE CR-RATING-CLASS         TO CP-CLASS-CODE             
03326      ELSE                                                         
03327         MOVE AM-CAL-TABLE            TO CP-CLASS-CODE.            
03328                                                                   
03329      IF DTE-CLIENT = 'CRI'                                        
03330          MOVE WK-AH-DEV-CODE     TO  CP-DEVIATION-CODE            
03331      ELSE                                                         
03332          MOVE STATE-PRIM-FAC-DEV (CLAS-INDEXS)                    
03333                                  TO  CP-DEVIATION-CODE.           
03334                                                                   
03335      MOVE +0                         TO CP-RATE-DEV-PCT.          
03336      MOVE 'A'                        TO CP-BENEFIT-TYPE.          
03337      MOVE '3'                        TO CP-PROCESS-TYPE.          
03338      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
03339      MOVE CR-AH-TERM                 TO CP-ORIGINAL-TERM          
03340                                         CP-LOAN-TERM.             
03341      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
03342                                         CP-RATING-BENEFIT-AMT.    
03343      IF CP-STATE-STD-ABBRV = 'OR'                                 
03344          COMPUTE CP-RATING-BENEFIT-AMT =                          
03345                                  CR-AHAMT * CR-AH-TERM.           
03346      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY          
03347      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
03348      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
03349      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
03350      MOVE CR-APR                     TO CP-LOAN-APR.              
03351      MOVE CR-AH-CRIT-PERIOD          TO CP-CRITICAL-MONTHS.       
03352      MOVE SPACES                     TO CP-ACCT-FLD-5.            
03353      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
03354                                                                   
03355      PERFORM 6130-CALL-RATING-ROUTINE THRU 6140-RATE-EXIT.        
03356                                                                   
03357      ADD 1 TO WS-AH-RATE-ATTEMPT.                                 
03358                                                                   
062712     IF DTE-CLIENT = 'CID' OR 'AHL'
CIDMOD        IF CR-AHPRM-CALC NOT NUMERIC
CIDMOD           MOVE +0               TO CR-AHPRM-CALC
CIDMOD        END-IF
CIDMOD        IF CR-AHPRM-CALC = ZEROS
CIDMOD           MOVE CR-AHPRM         TO CR-AHPRM-CALC
CIDMOD        END-IF
CIDMOD     END-IF
CIDMOD
03359      IF NOT CP-ERROR-OCCURED                                      
CIDMOD        IF (CP-CALC-PREMIUM NOT = +0) AND                         
CIDMOD           (CR-AHPRM-CALC NOT = +0)
CIDMOD           IF DTE-CLIENT = 'CID'
CIDMOD              COMPUTE WS-DIFF = (CP-CALC-PREMIUM -
CIDMOD                 CR-AHPRM-CALC) / CR-AHPRM-CALC
CIDMOD              IF WS-DIFF < 0
CIDMOD                 COMPUTE WS-DIFF = WS-DIFF * -1
CIDMOD              END-IF
CIDMOD              IF WS-DIFF > 0.02
CIDMOD                 MOVE CP-CALC-PREMIUM TO EX-PRIM-FAC-PREM         
CIDMOD                                    WS-HOLD-AH-PRIM-FAC           
CIDMOD              END-IF
CIDMOD           ELSE
03361               MOVE CP-CALC-PREMIUM TO EX-PRIM-FAC-PREM            
03362                                    WS-HOLD-AH-PRIM-FAC            
03363            END-IF                                                 
03363         END-IF                                                    
03365      ELSE                                                         
03366         ADD 1 TO WS-AH-RATE-ERROR                                 
CIDMOD     END-IF
03367                                                                   
CIDMOD     .
03368  1120-RELEASE-AH-RECORD.                                          
03369                                                                   
03370      PERFORM 1130-BUILD-AH-CLAIMS                                 
03371          VARYING YR FROM +1 BY +1                                 
03372          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
03373                                                                   
03374      GO TO 1140-END-AH-CLAIMS.                                    
03375                                                                   
03376  1130-BUILD-AH-CLAIMS.                                            
03377                                                                   
03378      IF CERT-AND-CLAIM                                            
03379         MOVE WS-CLAIM-AMT (2 1 YR) TO EX-CLAIM-AMT (YR)           
03380         MOVE WS-INC-CNT   (2 1 YR) TO EX-INC-CNT   (YR)           
03381         MOVE WS-PD-CNT    (2 1 YR) TO EX-PD-CNT    (YR).          
03382                                                                   
03383  1140-END-AH-CLAIMS.                                              
03384                                                                   
122002     IF CR-ENTRY-STATUS = '5' OR 'M'
03386         GO TO 1180-END-AH-RESERVES.                               
03387                                                                   
03388      MOVE EX-PREM TO WRK-C.                                       
03389                                                                   
03390 *    PERFORM 1200-CALC-AH-UNEARN THRU 1299-AH-UNEARN-EXIT         
011405     PERFORM 1200-CALC-AH-UNEARN THRU 1200-EXIT
03391          VARYING YR FROM +1 BY +1                                 
03392          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
03393                                                                   
03394      PERFORM 1150-BUILD-AH-DEV-RESERVES                           
03395          VARYING YR FROM +1 BY +1                                 
03396          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
03397                                                                   
03398      GO TO 1160-CALC-AH-PRIM-FAC.                                 
03399                                                                   
03400  1150-BUILD-AH-DEV-RESERVES.                                      
03401                                                                   
03402      MOVE WS-AH-BEG-MEAN-RES (YR)  TO EX-BEG-MEAN-RES (YR)        
03403                                       EX-PRI-BEG-MEAN-RES (YR).   
03404                                                                   
03405      MOVE WS-AH-BEG-STATE-RES (YR) TO EX-BEG-ST-RES (YR)          
03406                                       EX-PRI-BEG-ST-RES (YR).     
03407                                                                   
03408      MOVE WS-AH-BEG-R78-RES (YR)   TO EX-BEG-R78-RES (YR)         
03409                                       EX-PRI-BEG-R78-RES (YR).    
03410                                                                   
03411      MOVE WS-AH-BEG-PRO-RES (YR)   TO EX-BEG-PRO-RES (YR)         
03412                                       EX-PRI-BEG-PRO-RES (YR).    
03413                                                                   
03414      MOVE WS-AH-END-MEAN-RES (YR)  TO EX-END-MEAN-RES (YR)        
03415                                       EX-PRI-END-MEAN-RES (YR).   
03416                                                                   
03417      MOVE WS-AH-END-STATE-RES (YR) TO EX-END-ST-RES (YR)          
03418                                       EX-PRI-END-ST-RES (YR).     
03419                                                                   
03420      MOVE WS-AH-END-R78-RES (YR)   TO EX-END-R78-RES (YR)         
03421                                       EX-PRI-END-R78-RES (YR).    
03422                                                                   
03423      MOVE WS-AH-END-PRO-RES (YR)   TO EX-END-PRO-RES (YR)         
03424                                       EX-PRI-END-PRO-RES (YR).    
03425                                                                   
03426  1160-CALC-AH-PRIM-FAC.                                           
03427                                                                   
03428      IF EX-PREM NOT = EX-PRIM-FAC-PREM                            
03429         MOVE EX-PRIM-FAC-PREM TO WRK-C                            
03430 *       PERFORM 1200-CALC-AH-UNEARN THRU 1299-AH-UNEARN-EXIT      
011405        PERFORM 1200-CALC-AH-UNEARN THRU 1200-EXIT
03431             VARYING YR FROM +1 BY +1                              
03432             UNTIL YR GREATER THAN NUMBER-OF-PERIODS               
03433         PERFORM 1170-BUILD-AH-PRI-RESERVES                        
03434             VARYING YR FROM +1 BY +1                              
03435             UNTIL YR GREATER THAN NUMBER-OF-PERIODS.              
03436                                                                   
03437      GO TO 1180-END-AH-RESERVES.                                  
03438                                                                   
03439  1170-BUILD-AH-PRI-RESERVES.                                      
03440                                                                   
03441      MOVE WS-AH-BEG-MEAN-RES  (YR) TO EX-PRI-BEG-MEAN-RES (YR).   
03442      MOVE WS-AH-BEG-STATE-RES (YR) TO EX-PRI-BEG-ST-RES   (YR).   
03443      MOVE WS-AH-BEG-R78-RES   (YR) TO EX-PRI-BEG-R78-RES  (YR).   
03444      MOVE WS-AH-BEG-PRO-RES   (YR) TO EX-PRI-BEG-PRO-RES  (YR).   
03445                                                                   
03446      MOVE WS-AH-END-MEAN-RES  (YR) TO EX-PRI-END-MEAN-RES (YR).   
03447      MOVE WS-AH-END-STATE-RES (YR) TO EX-PRI-END-ST-RES   (YR).   
03448      MOVE WS-AH-END-R78-RES   (YR) TO EX-PRI-END-R78-RES  (YR).   
03449      MOVE WS-AH-END-PRO-RES   (YR) TO EX-PRI-END-PRO-RES  (YR).   
03450                                                                   
03451  1180-END-AH-RESERVES.                                            
03452                                                                   
03453      IF DEBUG-AH-STATUTORY                                        
03454          GO TO 1180-CONTINUE.                                     
03455                                                                   
03456      IF DTE-PGM-OPT NOT = '3' AND '4'                             
03457         GO TO 1190-BYPASS-AH-DETAIL.                              
03458                                                                   
03459      IF M5 (1) = +0                                               
03460         GO TO 1190-BYPASS-AH-DETAIL.                              
03461                                                                   
03462  1180-CONTINUE.                                                   
03463                                                                   
03464      MOVE EX-CARRIER           TO D-CARR.                         
03465      MOVE EX-GROUPING          TO D-GROUP.                        
03466      MOVE EX-STATE             TO D-STATE.                        
03467      MOVE EX-ACCOUNT           TO D-ACCOUNT.                      
03468      MOVE CR-CERT-NO           TO D-CERT-NO.                      
03469                                                                   
03470      MOVE 'L'                  TO DC-OPTION-CODE.                 
03471      MOVE EX-EFF-DATE          TO DC-GREG-DATE-CYMD.              
03472      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
03473      MOVE DC-GREG-DATE-1-EDIT  TO D-EFF-DATE.                     
03474                                                                   
03475      MOVE EX-PREM              TO D-PREM.                         
03476      MOVE EX-PRIM-FAC-PREM     TO D-PRI-PREM.                     
03477      MOVE EX-TERM              TO D-TERM.                         
03478      MOVE M2 (1)               TO D-REM-TERM.                     
03479      MOVE EX-END-ST-RES (1)    TO D-UNEARN.                       
03480      MOVE EX-BENEFIT           TO D-BENEFIT.                      
03481      MOVE 'A'                  TO D-L-A.                          
03482      MOVE CLAS-I-AB10 (CLAS-INDEXA) TO D-BEN-DESC.                
03483                                                                   
03484      PERFORM 1600-PRINT-A-LINE.                                   
03485                                                                   
03486      IF DEBUG-AH-STATUTORY                                        
03487          IF EX-END-ST-RES (1) NOT = ZEROS                         
03488              ADD +1                TO WS-S-CNT                    
03489              ADD EX-END-ST-RES (1) TO WS-S-STATUTORY              
03490              DISPLAY ' CNT ', WS-S-CNT                            
03491                      ' TOT ', WS-S-STATUTORY                      
03492                      ' RESV ', EX-END-ST-RES (1)                  
03493                      ' CERT ', CR-FULL-CONTROL.                   
03494                                                                   
03495  1190-BYPASS-AH-DETAIL.                                           
03496                                                                   
03497      MOVE CALL-EXTRACT          TO HOLD-AH-RECORD.                
03498                                                                   
03499      IF CR-ENTRY-STATUS NOT = '9'                                 
03500         PERFORM 1500-RELEASE THRU 1540-RELEASE-EXIT.              
03501                                                                   
03502      GO TO 1400-CHECK-IF-REINSURANCE.                             
03503                                                                   
03504  1200-CALC-AH-UNEARN.                                             
03505                                                                   
03506      MOVE +0 TO         WS-AH-END-R78-RES (YR)                    
03507                         WS-AH-BEG-R78-RES (YR)                    
03508                         WS-AH-END-PRO-RES (YR)                    
03509                         WS-AH-BEG-PRO-RES (YR)                    
03510                         WS-AH-END-NET-RES (YR)                    
03511                         WS-AH-BEG-NET-RES (YR)                    
03512                         WS-AH-END-MEAN-RES (YR)                   
03513                         WS-AH-BEG-MEAN-RES (YR)                   
03514                         WS-AH-END-STATE-RES (YR)                  
03515                         WS-AH-BEG-STATE-RES (YR)                  
03516                         WRK-B.                                    
03517                                                                   
03520      IF ENTRY-DT GREATER  BEGIN-DT (YR)
03521         GO TO  1200-CALC-AH-END
           END-IF
03522                                                                   
03523      MOVE WS-AH-BEG-REM-TRM1 (YR)
                                       TO M4
                                          CP-REMAINING-TERM
03524                                                                   
03525      IF M4 NOT POSITIVE
03526         GO TO 1200-CALC-AH-END
           END-IF

011405     MOVE BEGIN-DATE (YR)        TO DC-GREG-DATE-CYMD
011405     MOVE 'L'                    TO DC-OPTION-CODE
011405     PERFORM 6150-DATE-CONVERSION-ROUTINE
011405     MOVE DC-BIN-DATE-1          TO CP-VALUATION-DT
011405                                                                  
           MOVE CLAS-I-RL-AH (CLAS-INDEXA)
                                       TO CP-BENEFIT-TYPE
032609     MOVE M4                     TO WRK-REMTRM
011405     PERFORM 1250-CALC-UNEARNED-PREMIUM
                                       THRU 1250-EXIT

           MOVE CP-R78-U-PRM           TO WS-AH-BEG-R78-RES (YR)
           MOVE CP-PRORATA-U-PRM       TO WS-AH-BEG-PRO-RES (YR)
           MOVE CP-STATE-U-PRM         TO WS-AH-BEG-STATE-RES (YR)

011405     COMPUTE WS-AH-BEG-MEAN-RES (YR) =
011405        (WS-AH-BEG-PRO-RES (YR) + WS-AH-BEG-R78-RES (YR)) * .5
011405                                                                  
03528 *    PERFORM 0560-CALC-PRO-FACTOR
03529 *                                                                 
03530 *    COMPUTE WRK-B ROUNDED = WRK-D * WRK-C
03531 *    MOVE WRK-B TO WS-AH-BEG-PRO-RES (YR)
           .                                                            
03533  1200-CALC-AH-END.                                          
03534                                                                   
03535      IF ENTRY-DT GREATER THAN END-DT (YR)
03536         GO TO  1200-EXIT
           END-IF
03537                                                                   
03538      MOVE WS-AH-END-REM-TRM1 (YR)
                                       TO M4
                                          CP-REMAINING-TERM
03539                                                                   
03540      IF M4 NOT POSITIVE
03541         GO TO 1200-EXIT
           END-IF
03542                                                                   
011405     MOVE ENDING-DATE (YR)       TO DC-GREG-DATE-CYMD
011405     MOVE 'L'                    TO DC-OPTION-CODE
011405     PERFORM 6150-DATE-CONVERSION-ROUTINE
011405     MOVE DC-BIN-DATE-1          TO CP-VALUATION-DT
011405                                                                  
           MOVE CLAS-I-RL-AH (CLAS-INDEXA)
                                       TO CP-BENEFIT-TYPE
032609     MOVE M4                     TO WRK-REMTRM
011405     PERFORM 1250-CALC-UNEARNED-PREMIUM
                                       THRU 1250-EXIT

           MOVE CP-R78-U-PRM           TO WS-AH-END-R78-RES (YR)
           MOVE CP-PRORATA-U-PRM       TO WS-AH-END-PRO-RES (YR)
           MOVE CP-STATE-U-PRM         TO WS-AH-END-STATE-RES (YR)

011405     COMPUTE WS-AH-END-MEAN-RES (YR) =
011405        (WS-AH-END-PRO-RES (YR) + WS-AH-END-R78-RES (YR)) * .5
011405                                                                  
303543*    PERFORM 0560-CALC-PRO-FACTOR
03544 *                                                                 
03545 *    COMPUTE WRK-B ROUNDED = WRK-D * WRK-C
03546 *    MOVE WRK-B TO WS-AH-END-PRO-RES (YR)
           .
       1200-EXIT.
           EXIT.
03547                                                                   
03548  1230-CALC-AH-R78-BEG.                                            
03549                                                                   
03550      IF ENTRY-DT GREATER THAN BEGIN-DT (YR)                       
03551         GO TO 1240-CALC-AH-R78-END.                               
03552                                                                   
03553      MOVE WS-AH-BEG-REM-TRM1 (YR) TO M4.                          
03554                                                                   
03555      IF M4 NOT POSITIVE                                           
03556         GO TO 1240-CALC-AH-R78-END.                               
03557                                                                   
03558      PERFORM 0610-CALC-R78-FACTOR.                                
03559                                                                   
03560      COMPUTE WRK-B ROUNDED = WRK-E * WRK-C.                       
03561      MOVE WRK-B TO WS-AH-BEG-R78-RES (YR).                        
03562                                                                   
03563  1240-CALC-AH-R78-END.                                            
03564                                                                   
03565      IF ENTRY-DT GREATER THAN END-DT (YR)                         
03566         GO TO 1250-CALC-AH-MEAN-RES.                              
03567                                                                   
03568      MOVE WS-AH-END-REM-TRM1 (YR) TO M4.                          
03569                                                                   
03570      IF M4 NOT POSITIVE                                           
03571         GO TO 1250-CALC-AH-MEAN-RES.                              
03572                                                                   
03573      PERFORM 0610-CALC-R78-FACTOR.                                
03574                                                                   
03575      COMPUTE WRK-B ROUNDED = WRK-E * WRK-C.                       
03576      MOVE WRK-B TO WS-AH-END-R78-RES (YR).                        
03577                                                                   
03578  1250-CALC-AH-MEAN-RES.                                           
03579                                                                   
03580      COMPUTE WS-AH-BEG-MEAN-RES (YR) =                            
03581         (WS-AH-BEG-PRO-RES (YR) + WS-AH-BEG-R78-RES (YR)) * .5.   
03582                                                                   
03583      COMPUTE WS-AH-END-MEAN-RES (YR) =                            
03584         (WS-AH-END-PRO-RES (YR) + WS-AH-END-R78-RES (YR)) * .5.   
03585                                                                   
03586  1260-CALC-AH-NET-BEG.                                            
03587                                                                   
03588      IF AH-EARN-NET-PAY                                           
03589          NEXT SENTENCE                                            
03590      ELSE                                                         
03591          GO TO 1280-CALC-STATE-REGS.                              
03592                                                                   
03593      IF ENTRY-DT GREATER THAN BEGIN-DT (YR)                       
03594         GO TO 1270-CALC-AH-NET-END.                               
03595                                                                   
03596      MOVE WS-AH-BEG-REM-TRM1 (YR) TO M4.                          
03597                                                                   
03598      IF M4 NOT POSITIVE                                           
03599         GO TO 1270-CALC-AH-NET-END.                               
03600                                                                   
03601      PERFORM 1275-NET-PAY-AH THRU 1275-NET-PAY-AH-X.              
03602                                                                   
03603      COMPUTE WRK-B ROUNDED = WRK-E * WRK-C.                       
03604      MOVE WRK-B TO WS-AH-BEG-NET-RES (YR).                        
03605                                                                   
03606  1270-CALC-AH-NET-END.                                            
03607                                                                   
03608      IF ENTRY-DT GREATER THAN END-DT (YR)                         
03609         GO TO 1280-CALC-STATE-REGS.                               
03610                                                                   
03611      MOVE WS-AH-END-REM-TRM1 (YR) TO M4.                          
03612                                                                   
03613      IF M4 NOT POSITIVE                                           
03614         GO TO 1280-CALC-STATE-REGS.                               
03615                                                                   
03616      PERFORM 1275-NET-PAY-AH THRU 1275-NET-PAY-AH-X.              
03617                                                                   
03618      COMPUTE WRK-B ROUNDED = WRK-E * WRK-C.                       
03619      MOVE WRK-B TO WS-AH-END-NET-RES (YR).                        
03620                                                                   
03621      GO TO 1280-CALC-STATE-REGS.                                  
03622                                                                   
03623  1275-NET-PAY-AH.                                                 
03624                                                                   
03625      MOVE ZERO                     TO WRK-B.                      
03626                                                                   
03627      IF M4 NOT POSITIVE                                           
03628          GO TO 1275-NET-PAY-AH-X.                                 
03629                                                                   
03630      IF M4 NOT LESS EX-TERM                                       
03631          MOVE WRK-C                TO WRK-B                       
03632          MOVE +1                   TO WRK-E                       
03633          GO TO 1275-NET-PAY-AH-X.                                 
03634                                                                   
03635      MOVE EX-TERM                  TO NP-ORIG                     
03636                                       NP-CAP.                     
03637      MOVE EX-APR                   TO NP-APR.                     
03638      MOVE M4                       TO NP-REM.                     
03639                                                                   
03640      MOVE CLAS-I-BAL (CLAS-INDEXA) TO NP-OPT.                     
03641      IF NP-OPT = 'T' OR 'U' OR 'V' OR 'W' OR 'X'                  
03642         MOVE EX-CAP-TERM           TO NP-ORIG.                    
03643                                                                   
03644      MOVE 'R'                      TO NP-OPT.                     
03645                                                                   
03646      CALL 'ECSNETRM' USING                                        
03647          NP-APR, NP-ORIG, NP-REM, NP-OPT, NP-CAP, NP-FACTOR.      
03648                                                                   
03649      MOVE NP-FACTOR                TO WRK-E.                      
03650                                                                   
03651  1275-NET-PAY-AH-X.                                               
03652      EXIT.                                                        
03653                                                                   
03654  1280-CALC-STATE-REGS.                                            
03655                                                                   
03656      IF ANTICIPATION                                              
03657         IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                     
03658               IF EX-BEN-TYPE NOT = ZERO                           
03659                  MOVE M6 (YR) TO WORK-M4                          
03660                  MOVE M3 (YR) TO WRK-REMTRM                       
03661                  PERFORM 1320-CHECK-AH-RATES THRU 1330-CHECK-EXIT 
03662                  IF CP-ERROR-RATE-NOT-FOUND OR                    
03663                     CP-ERROR-RATE-IS-ZERO   OR                    
03664                     CP-ERROR-IN-DATES                             
03665                   NEXT SENTENCE                                   
03666                  ELSE                                             
03667                   COMPUTE WRK-B ROUNDED =                         
03668                    ((M3 (YR) * EX-BENEFIT) / +100)                
03669                                * CP-PREMIUM-RATE                  
03670                   MOVE WRK-B TO WS-AH-BEG-STATE-RES (YR).         
03671                                                                   
03672      IF ANTICIPATION                                              
03673         IF CLAS-I-BAL (CLAS-INDEXA) NOT = 'B'                     
03674               IF EX-BEN-TYPE NOT = ZERO                           
03675                  MOVE M5 (YR) TO WORK-M4                          
03676                  MOVE M2 (YR) TO WRK-REMTRM                       
03677                  PERFORM 1320-CHECK-AH-RATES THRU 1330-CHECK-EXIT 
03678                  IF CP-ERROR-RATE-NOT-FOUND OR                    
03679                     CP-ERROR-RATE-IS-ZERO   OR                    
03680                     CP-ERROR-IN-DATES                             
03681                   NEXT SENTENCE                                   
03682                  ELSE                                             
03683                   COMPUTE WRK-B ROUNDED =                         
03684                    ((M2 (YR) * EX-BENEFIT) / +100)                
03685                                * CP-PREMIUM-RATE                  
03686                   MOVE WRK-B TO WS-AH-END-STATE-RES (YR).         
03687                                                                   
03688      IF R78                                                       
03689         MOVE WS-AH-BEG-R78-RES (YR) TO                            
03690                        WS-AH-BEG-STATE-RES (YR)                   
03691         MOVE WS-AH-END-R78-RES (YR) TO                            
03692                        WS-AH-END-STATE-RES (YR).                  
03693                                                                   
03694      IF AH-EARN-NET-PAY                                           
03695         MOVE WS-AH-BEG-NET-RES (YR) TO                            
03696                        WS-AH-BEG-STATE-RES (YR)                   
03697         MOVE WS-AH-END-NET-RES (YR) TO                            
03698                        WS-AH-END-STATE-RES (YR).                  
03699                                                                   
03700      IF PRATA                                                     
03701         MOVE WS-AH-BEG-PRO-RES (YR) TO                            
03702                        WS-AH-BEG-STATE-RES (YR)                   
03703         MOVE WS-AH-END-PRO-RES (YR) TO                            
03704                        WS-AH-END-STATE-RES (YR).                  
03705                                                                   
03706      IF MEAN                                                      
03707         COMPUTE WS-AH-BEG-STATE-RES (YR) =                        
03708         (WS-AH-BEG-PRO-RES (YR) + WS-AH-BEG-R78-RES (YR)) * .5    
03709         COMPUTE WS-AH-END-STATE-RES (YR) =                        
03710         (WS-AH-END-PRO-RES (YR) + WS-AH-END-R78-RES (YR)) * .5.   
03711                                                                   
03712      IF 1-3RD-2-3RDS                                              
03713         COMPUTE WS-AH-BEG-STATE-RES (YR) =                        
03714         (WS-AH-BEG-PRO-RES (YR) * .6667) +                        
03715         (WS-AH-BEG-R78-RES (YR) * .3333)                          
03716         COMPUTE WS-AH-END-STATE-RES (YR) =                        
03717         (WS-AH-END-PRO-RES (YR) * .6667) +                        
03718         (WS-AH-END-R78-RES (YR) * .3333).                         
03719                                                                   
03720  1299-AH-UNEARN-EXIT.
03721      EXIT.

011405 1250-CALC-UNEARNED-PREMIUM.
011405                                                                  
011405*    MOVE CR-DT                  TO DC-GREG-DATE-CYMD
011405*    MOVE 'L'                    TO DC-OPTION-CODE
011405*    PERFORM 0400-DATE-CONVERSION-ROUTINE THRU 0409-EXIT          
011405*    MOVE DC-BIN-DATE-1          TO CP-CERT-EFF-DT
           MOVE EFFECT-DT              TO CP-CERT-EFF-DT
011405                                                                  
011405*    MOVE BIN-RUN-DATE           TO CP-VALUATION-DT
011405                                                                  
           MOVE CR-AH-TERM             TO CP-ORIGINAL-TERM
           MOVE CR-AHPRM               TO CP-ORIGINAL-PREMIUM
           MOVE CR-AHAMT               TO CP-ORIGINAL-BENEFIT
                                          CP-RATING-BENEFIT-AMT
011405     MOVE CLAS-I-BAL (CLAS-INDEXA)
                                       TO CP-SPECIAL-CALC-CD
011405     MOVE DTE-R78                TO CP-R78-OPTION
011405     MOVE SPACES                 TO CP-ACCT-FLD-5
011405     MOVE DTE-CLIENT             TO CP-COMPANY-ID
011405     MOVE CR-AGE                 TO CP-ISSUE-AGE
011405     MOVE CR-APR                 TO CP-LOAN-APR
011405     MOVE CR-PMT-FREQ            TO CP-PAY-FREQUENCY
011405     MOVE CR-LOAN-TERM           TO CP-LOAN-TERM
011405     MOVE SPACES                 TO CP-DOMICILE-STATE
011405     MOVE CR-CARRIER             TO CP-CARRIER
011405                                                                  
011405     IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
011405         MOVE CR-RATING-CLASS    TO CP-CLASS-CODE             
011405     ELSE                                                         
011405         MOVE AM-CAL-TABLE       TO CP-CLASS-CODE
           END-IF

011405     MOVE STATE-SUB (CLAS-INDEXS)
                                       TO CP-STATE
011405     MOVE STATE-ABBR (CLAS-INDEXS)
                                       TO CP-STATE-STD-ABBRV
011405     MOVE DTE-CLASIC-COMPANY-CD  TO CP-COMPANY-CD
011405     MOVE CR-PMT-EXTENSION-DAYS  TO CP-TERM-OR-EXT-DAYS

           MOVE ' ' TO CP-EARNING-METHOD


021904     IF DTE-CLIENT = 'CID'
              IF CP-STATE-STD-ABBRV = 'OH'
                 IF CR-LFTYP NOT = ZEROS AND SPACES
                    IF (CLAS-I-EP (CLAS-INDEXL) = 'N' OR '5')
                                OR
                       ((CR-LF-TERM > +60)
                        AND (CR-APR > ZEROS))
                       MOVE '5'          TO CP-EARNING-METHOD
                    END-IF
                 END-IF
              END-IF
           END-IF


011405                                                                  
011405     CALL 'ELUPRMX' USING CALCULATION-PASS-AREA

062712     if dte-client not = 'AHL'
032609        IF CP-STATE-STD-ABBRV = 'VA'
032609           PERFORM 1320-CHECK-AH-RATES
032609                                 THRU 1330-CHECK-EXIT
032609           IF CP-ERROR-RATE-NOT-FOUND  OR
032609              CP-ERROR-RATE-IS-ZERO  OR
032609              CP-ERROR-IN-DATES
032609              CONTINUE
032609           ELSE
032609              COMPUTE CP-STATE-U-PRM ROUNDED =
032609                 ((WRK-REMTRM * CR-AHAMT)
032609                 / +100) * CP-PREMIUM-RATE
032609           END-IF
032609        END-IF
062712     END-IF

           .
011405 1250-EXIT.                                                       
011405     EXIT.                                                        

03724  1300-FIND-METHOD.                                                

032609     IF STATE-ABBR (CLAS-INDEXS) = 'VA'
032609        MOVE '1'                 TO STATE-CALL-EARN (CLAS-INDEXS)
032609     END-IF

03726      IF STATE-CALL-EARN (CLAS-INDEXS) = '2'                       
03727         MOVE 'R' TO TYPE-METHOD                                   
03728         GO TO 1310-METHOD-EXIT                                    
03729      ELSE                                                         
03730         IF STATE-CALL-EARN (CLAS-INDEXS) = '3'                    
03731            MOVE 'P' TO TYPE-METHOD                                
03732            GO TO 1310-METHOD-EXIT                                 
03733         ELSE                                                      
03734            IF STATE-CALL-EARN (CLAS-INDEXS) = '4'                 
03735               MOVE 'M' TO TYPE-METHOD                             
03736               GO TO 1310-METHOD-EXIT                              
03737            ELSE                                                   
03738               IF STATE-CALL-EARN (CLAS-INDEXS) = '5'              
03739                  MOVE CLAS-I-EP (CLAS-INDEXA) TO TYPE-METHOD      
03740                  GO TO 1310-METHOD-EXIT.                          
03741                                                                   
03742      SET TABLE-INDEX TO +1                                        
03743                                                                   
03744      SEARCH MR-TABLE VARYING TABLE-INDEX                          
03745               AT END                                              
03746          MOVE DEFAULT-AH-METHOD             TO TYPE-METHOD        
03747               WHEN                                                
03748          T-STATE (TABLE-INDEX) = STATE-ABBR (CLAS-INDEXS)         
03749              MOVE METHOD-CODE (TABLE-INDEX) TO TYPE-METHOD.       
03750                                                                   
03751      IF AH-EARN-METHOD = 'A' OR 'C'                               
03752         MOVE 'A' TO TYPE-METHOD                                   
03753         GO TO 1310-METHOD-EXIT.                                   
03754                                                                   
03755      IF DTE-CLIENT = 'CSO' OR 'CID'                               
03756          IF STATE-ABBR (CLAS-INDEXS) = 'CO'                       
03757              IF EX-EFF-DATE LESS THAN 19970101                    
03758                  MOVE 'R'        TO  TYPE-METHOD                  
CIDMOD                 GO TO 1310-METHOD-EXIT                           
03759              ELSE                                                 
03760                  MOVE 'M'        TO  TYPE-METHOD                  
03761                  GO TO 1310-METHOD-EXIT.                          
03762                                                                   
03763      IF TEXAS-SPECIAL                                             
03764         IF EX-EFF-DATE  GREATER THAN  19830831                    
03765            MOVE 'M' TO TYPE-METHOD                                
03766            GO TO 1310-METHOD-EXIT                                 
03767         ELSE                                                      
03768            MOVE 'R' TO TYPE-METHOD                                
03769            GO TO 1310-METHOD-EXIT.                                
03770                                                                   
03771      IF OK-SPECIAL                                                
03772         IF EX-EFF-DATE  GREATER THAN  19820629                    
03773            MOVE 'M' TO TYPE-METHOD                                
03774            GO TO 1310-METHOD-EXIT                                 
03775         ELSE                                                      
03776            MOVE 'R' TO TYPE-METHOD                                
03777            GO TO 1310-METHOD-EXIT.                                
03778                                                                   
03779      IF OHIO-SPECIAL                                              
03780         IF EX-EFF-DATE  GREATER THAN  19831031                    
03781            MOVE 'M' TO TYPE-METHOD                                
03782            GO TO 1310-METHOD-EXIT                                 
03783         ELSE                                                      
03784            MOVE 'P' TO TYPE-METHOD                                
03785            GO TO 1310-METHOD-EXIT.                                

032609     IF VIRGINIA-SPECIAL                                          
032609        MOVE 'A'                 TO TYPE-METHOD
032609                                    AH-EARN-METHOD
032609        IF CLAS-I-BAL (CLAS-INDEXA) = 'C'
032609           MOVE 'P'              TO TYPE-METHOD
032609                                    AH-EARN-METHOD
032609        END-IF
032609     END-IF

03796      IF (DTE-CLIENT = 'NCL') AND                                  
03797         (EX-EFF-DATE  LESS THAN  19910101 )                       
03798           NEXT SENTENCE                                           
03799      ELSE                                                         
03800          IF (CR-APR GREATER THAN +0)                              
03801             IF  (AH-EARN-METHOD = 'N') OR                         
03802                 (CLAS-I-EP (CLAS-INDEXA) = 'N')                   
03803                  MOVE 'N' TO TYPE-METHOD.                         
03804                                                                   
03805  1310-METHOD-EXIT.                                                
03806      EXIT.                                                        
03807                                                                   
03808      EJECT                                                        
03809  1320-CHECK-AH-RATES.                                             

032609     MOVE EFFECT-DT              TO CP-CERT-EFF-DT

03820      MOVE CLAS-I-RL-AH (CLAS-INDEXA) TO CP-BENEFIT-TYPE.          
03821      MOVE SPACES                     TO CP-ACCT-FLD-5.            
03822      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
03823      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
03824      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
03825      MOVE CR-AGE                     TO CP-ISSUE-AGE.             
03826      MOVE WRK-REMTRM                 TO CP-ORIGINAL-TERM.         
03827      MOVE CR-AHAMT                   TO CP-ORIGINAL-BENEFIT       
03828                                         CP-RATING-BENEFIT-AMT.    
03829      IF CP-STATE-STD-ABBRV = 'OR'                                 
03830          COMPUTE CP-RATING-BENEFIT-AMT =                          
03831                                  CR-AHAMT * WRK-REMTRM.           
03832      MOVE CR-APR                     TO CP-LOAN-APR.              
03833      MOVE CR-PMT-FREQ                TO CP-PAY-FREQUENCY.         
03834      MOVE AH-EARN-METHOD             TO CP-EARNING-METHOD.        
03835      MOVE '3'                        TO CP-PROCESS-TYPE.          
03836      MOVE CLAS-I-BAL (CLAS-INDEXA)   TO CP-SPECIAL-CALC-CD.       
03837      MOVE CR-LOAN-TERM               TO CP-LOAN-TERM.             
03838      IF CR-RATING-CLASS NOT = SPACE AND ZERO                      
03839          MOVE CR-RATING-CLASS        TO CP-CLASS-CODE             
03840      ELSE                                                         
03841          MOVE AM-CAL-TABLE           TO CP-CLASS-CODE.            
03842      MOVE AM-AH-DEVIATION            TO CP-DEVIATION-CODE.        
03843      MOVE CR-AHTYP                   TO CP-BENEFIT-CD.            
03844      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
03845      MOVE CR-PMT-EXTENSION-DAYS      TO CP-TERM-OR-EXT-DAYS.      
03846      MOVE AH-OVERRIDE-L1             TO CP-AH-OVERRIDE-CODE.      
03847      MOVE AM-AH-DEVIATION-PCT        TO CP-RATE-DEV-PCT.          
03848                                                                   
03849      PERFORM 6130-CALL-RATING-ROUTINE THRU 6140-RATE-EXIT.        
03850                                                                   
03851  1330-CHECK-EXIT.                                                 
03852      EXIT.                                                        
03853                                                                   
03854  1400-CHECK-IF-REINSURANCE.                                       
03855                                                                   
03856      IF DTE-PGM-OPT = '2' OR '4'                                  
03857         PERFORM 2000-REINSURANCE-ROUTINE THRU                     
03858                 2030-REINSURANCE-ROUTINE-X.                       
03859                                                                   
03860  1410-PROCESS-CERTS-EXIT.                                         
03861      EXIT.                                                        
03862                                                                   
03863      EJECT                                                        
03864  1500-RELEASE.                                                    
03865                                                                   
03866      IF CERT-NO-CLAIM OR                                          
03867         CERT-AND-CLAIM                                            
03868         GO TO 1520-RELEASE-SORT-RECORD.                           
03869                                                                   
03870      MOVE +1 TO LIFE-AH-SUB                                       
03871                 YR.                                               
03872                                                                   
03873  1510-ADD-CLAIMS-TO-EXTRACT.                                      
03874                                                                   
03875      IF LIFE-AH-SUB GREATER THAN +2                               
03876         GO TO 1540-RELEASE-EXIT.                                  
03877                                                                   
03878      IF YR GREATER THAN NUMBER-OF-PERIODS                         
03879          PERFORM 1520-RELEASE-SORT-RECORD                         
03880          ADD +1 TO LIFE-AH-SUB                                    
03881          MOVE +1 TO YR                                            
03882          GO TO 1510-ADD-CLAIMS-TO-EXTRACT.                        
03883                                                                   
03884      MOVE WS-CLAIM-AMT (LIFE-AH-SUB 1 YR) TO EX-CLAIM-AMT (YR).   
03885      MOVE WS-INC-CNT   (LIFE-AH-SUB 1 YR) TO EX-INC-CNT   (YR).   
03886      MOVE WS-PD-CNT    (LIFE-AH-SUB 1 YR) TO EX-PD-CNT    (YR).   
03887      MOVE WS-DEV       (LIFE-AH-SUB)      TO EX-STATE-DEV.        
03888      MOVE WS-BEN-TYPE  (LIFE-AH-SUB)      TO EX-BEN-TYPE.         
03889                                                                   
03890      IF LIFE-AH-SUB = +1                                          
03891          MOVE 'L'                       TO EX-LIFE-AH             
03892      ELSE                                                         
03893          MOVE 'A'                       TO EX-LIFE-AH.            
03894                                                                   
03895      ADD +1 TO YR.                                                
03896      GO TO 1510-ADD-CLAIMS-TO-EXTRACT.                            
03897                                                                   
03898  1520-RELEASE-SORT-RECORD.                                        
03899                                                                   
03900      IF SINGLE-PERIOD-PROCESS                                     
03901          MOVE ZEROS              TO  EX-INC-CNT          (2)      
03902                                      EX-PD-CNT           (2)      
03903                                      EX-C-CNT            (2)      
03904                                      RR                  (2)      
03905                                      M1                  (2)      
03906                                      M2                  (2)      
03907                                      M3                  (2)      
03908                                      M5                  (2)      
03909                                      M6                  (2)      
03910                                      EX-BEG-ST-RES       (2)      
03911                                      EX-END-ST-RES       (2)      
03912                                      EX-BEG-R78-RES      (2)      
03913                                      EX-END-R78-RES      (2)      
03914                                      EX-BEG-PRO-RES      (2)      
03915                                      EX-END-PRO-RES      (2)      
03916                                      EX-BEG-MEAN-RES     (2)      
03917                                      EX-END-MEAN-RES     (2)      
03918                                      EX-PRI-BEG-ST-RES   (2)      
03919                                      EX-PRI-END-ST-RES   (2)      
03920                                      EX-PRI-BEG-R78-RES  (2)      
03921                                      EX-PRI-END-R78-RES  (2)      
03922                                      EX-PRI-BEG-PRO-RES  (2)      
03923                                      EX-PRI-END-PRO-RES  (2)      
03924                                      EX-PRI-BEG-MEAN-RES (2)      
03925                                      EX-PRI-END-MEAN-RES (2)      
03926                                      EX-MEAN-INFORCE     (2)      
03927                                      EX-OVR-COMM         (2)      
03928                                      EX-AGT-COMM         (2)      
03929                                      EX-CNC-AMT          (2)      
03930                                      EX-RETRO-PMTS       (2)      
03931          MOVE INITIALIZED-BACK   TO  EX-DATA-BY-YEAR   (3).       
03932                                                                   
03933      MOVE LOW-VALUES             TO SORT-KEY.                     
03934                                                                   
CIDMOD     IF EX-CLAIM-AMT (1) NOT NUMERIC                              
CIDMOD        MOVE ZEROS TO EX-CLAIM-AMT (1).                           
CIDMOD                                                                  
CIDMOD     IF EX-CLAIM-AMT (2) NOT NUMERIC                              
CIDMOD        MOVE ZEROS TO EX-CLAIM-AMT (2).                           
CIDMOD                                                                  
CIDMOD     IF EX-CLAIM-AMT (3) NOT NUMERIC                              
CIDMOD        MOVE ZEROS TO EX-CLAIM-AMT (3).                           
CIDMOD                                                                  
03935      IF EX-BEN-TYPE = '00'                                        
03936         NEXT SENTENCE                                             
03937      ELSE                                                         
03938         IF EX-CLAIM-AMT (1) GREATER THAN +0                       
03939            PERFORM XXXX-PRINT-CLAIMS THRU XXXX-EXIT               
03940            WRITE CALL-X FROM CALL-EXTRACT                         
03941            ADD +1 TO CALL-CNT                                     
03942         ELSE                                                      
03943            WRITE CALL-X FROM CALL-EXTRACT                         
03944            ADD +1 TO CALL-CNT.                                    
03945                                                                   
03946  1530-CHECK-FOR-TEXAS.                                            
03947                                                                   
03948      IF EX-BEN-TYPE = '00'                                        
03949         NEXT SENTENCE                                             
03950      ELSE                                                         
03951         IF REPORT-OVER-UNDER-60  OR                               
03952            EX-STATE = STATE-TX                                    
03953            IF (EX-CLAIM-AMT (1) GREATER THAN +0 OR                
03954                EX-CLAIM-AMT (2) GREATER THAN +0 OR                
03955                EX-CLAIM-AMT (3) GREATER THAN +0)                  
03956              IF EX-TERM-CD = '1'                                  
03957                 MOVE '2'    TO EX-TERM-CD                         
03958                 MOVE 'X'    TO EX-TX-RESV-ADJ-REC                 
03959                 WRITE CALL-X FROM CALL-EXTRACT                    
03960                 MOVE SPACES TO EX-TX-RESV-ADJ-REC                 
03961                 ADD +1 TO TEXAS-CNT                               
03962              ELSE                                                 
03963              IF EX-TERM-CD = '2'                                  
03964                 MOVE '1'    TO EX-TERM-CD                         
03965                 MOVE 'X'    TO EX-TX-RESV-ADJ-REC                 
03966                 WRITE CALL-X FROM CALL-EXTRACT                    
03967                 MOVE SPACES TO EX-TX-RESV-ADJ-REC                 
03968                 ADD +1 TO TEXAS-CNT.                              
03969                                                                   
03970  1540-RELEASE-EXIT.                                               
03971      EXIT.                                                        
03972                                                                   
03973      EJECT                                                        
03974  XXXX-PRINT-CLAIMS.                                               
03975                                                                   
03976      IF DTE-PGM-OPT = '3' OR '4'                                  
03977         NEXT SENTENCE                                             
03978      ELSE                                                         
03979         GO TO XXXX-EXIT.                                          
03980                                                                   
03981      MOVE SPACES              TO DETAIL-1B.                       
03982                                                                   
03983      IF EX-LIFE-AH = 'L'                                          
03984         MOVE EX-CLAIM-AMT (1) TO DB-CLAIML                        
03985      ELSE                                                         
03986      IF EX-LIFE-AH = 'A'                                          
03987         MOVE EX-CLAIM-AMT (1) TO DB-CLAIMA.                       
03988                                                                   
03989      MOVE EX-CARRIER       TO DB-CARR.                            
03990      MOVE EX-GROUPING      TO DB-GROUP.                           
03991      MOVE EX-STATE         TO DB-STATE.                           
03992      MOVE EX-ACCOUNT       TO DB-ACCOUNT.                         
03993      MOVE EX-EFF-DATE-YR   TO FORMAT-YEAR.                        
03994      MOVE EX-EFF-DATE-MO   TO FORMAT-MONTH.                       
03995      MOVE EX-EFF-DATE-DA   TO FORMAT-DAY.                         
03996      MOVE FORMAT-DATE      TO DB-EFF-DATE.                        
03997      MOVE EX-LIFE-AH       TO DB-L-A.                             
03998      MOVE DETAIL-1B        TO PRTC-DATA.                          
03999      MOVE SPACES           TO PRTC-CTL.                           
04000      WRITE PRTC.                                                  
04001                                                                   
04002  XXXX-EXIT.                                                       
04003      EXIT.                                                        
04004                                                                   
04005      EJECT                                                        
04006                                                                   
04007  1600-PRINT-A-LINE.                                               
04008                                                                   
04009      IF LINE-CTR GREATER THAN +60                                 
04010         MOVE +8 TO LINE-CTR                                       
04011         ADD +1 TO PAGE-CTR                                        
04012         PERFORM 1630-PRINT-HEADINGS THRU                          
04013                 1640-PRINT-HEADINGS-EXIT.                         
04014                                                                   
04015      MOVE DETAIL-1 TO P-DATA.                                     
04016      MOVE ' '      TO X.                                          
04017      PERFORM 1610-PRT-RTN THRU 1620-E-PRT-RTN.                    
04018      ADD +1 TO LINE-CTR.                                          
04019                                                                   
04020  1610-PRT-RTN.                                                    
04021                                  COPY ELCPRT2.                    
04022  1620-E-PRT-RTN.                                                  
04023      EXIT.                                                        
04024                                                                   
04025  1630-PRINT-HEADINGS.                                             
04026                                                                   
04027      MOVE '1'             TO X.                                   
04028      MOVE HEAD-1          TO P-DATA.                              
04029      PERFORM 1610-PRT-RTN THRU 1620-E-PRT-RTN.                    
04030                                                                   
04031      MOVE ' '             TO X.                                   
04032      MOVE WS-CURRENT-DATE TO HD-DATE.                             
04033      MOVE COMPANY-NAME    TO HD-CLIENT.                           
04034      MOVE HEAD-2          TO P-DATA.                              
04035      PERFORM 1610-PRT-RTN THRU 1620-E-PRT-RTN.                    
04036                                                                   
04037      MOVE PAGE-CTR        TO HD-PAGE.                             
04038      MOVE ALPH-DATE       TO HD-ALF-DTE.                          
04039      MOVE HEAD-3          TO P-DATA.                              
04040      PERFORM 1610-PRT-RTN THRU 1620-E-PRT-RTN.                    
04041                                                                   
04042      MOVE '0'             TO X.                                   
04043      MOVE HEAD-4          TO P-DATA.                              
04044      PERFORM 1610-PRT-RTN THRU 1620-E-PRT-RTN.                    
04045                                                                   
04046  1640-PRINT-HEADINGS-EXIT.                                        
04047      EXIT.                                                        
04048      EJECT                                                        
04049                                                                   
       1700-CALC-REM-AMT.

           IF CR-LFTYP = 'QD'
              MOVE +15.0               TO CR-APR
           END-IF

062712     if dte-client = 'AHL'
062712        if cr-loan-term = zeros
062712           move cr-lf-term       to cr-loan-term
062712        end-if
062712     end-if

           MOVE +0                     TO WS-REM-AMT
           IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'
              IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
                 COMPUTE WS-REM-AMT =
                   CR-LFAMT + CR-LFAMT-ALT
              ELSE
                 MOVE CR-LFAMT         TO WS-REM-AMT
              END-IF
              GO TO 1750-DISPLAY-AMTS
           END-IF

062712     if dte-client = 'AHL'
062712        compute intermed rounded =
062712           cr-lfamt / cr-loan-term
062712     else
              COMPUTE INTERMED ROUNDED = CR-LFAMT / CR-LF-TERM
062712     end-if

           IF M4 = CR-LF-TERM
              IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
                 COMPUTE WS-REM-AMT =
                   CR-LFAMT + CR-LFAMT-ALT
              ELSE
                 MOVE CR-LFAMT         TO WS-REM-AMT
              END-IF
              GO TO 1750-DISPLAY-AMTS
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
              GO TO 1710-ORDINARY-REM
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'T'
              GO TO 1720-CALC-TEXAS-REM
           END-IF

           IF CR-LFTYP = 'QD'
              MOVE +15.0               TO CR-APR
              MOVE 'N'                 TO
                             CLAS-I-CALC-TYPE (CLAS-INDEXL)
              GO TO 1730-CALC-NET-PAY-REM
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'N'
              GO TO 1730-CALC-NET-PAY-REM
           END-IF

           IF (STATE-ABBR (CLAS-INDEXS) = 'OH') AND
              (CR-RATING-CLASS NOT = 'L ')
               IF (CR-LF-TERM GREATER THAN +60) AND
                  (CR-DT GREATER THAN 19831031) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 1730-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF STATE-ABBR (CLAS-INDEXS) = 'MT'
               IF (CR-LF-TERM GREATER THAN +61) AND
                  (CR-DT  GREATER THAN 19830318) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 1730-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF STATE-ABBR (CLAS-INDEXS) = 'UT'
               IF (CR-LF-TERM GREATER THAN +62) AND
                  (CR-DT  GREATER THAN 19810831) AND
                  (CR-DT  LESS THAN 19830901) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 1730-CALC-NET-PAY-REM
              END-IF
           END-IF

           IF STATE-ABBR (CLAS-INDEXS) = 'RI'
               IF (CR-LF-TERM GREATER THAN +60) AND
                  (CR-DT  GREATER THAN 19831231) AND
                  (CR-APR GREATER THAN ZERO)
                   GO TO 1730-CALC-NET-PAY-REM
              END-IF
           END-IF

           .
       1710-ORDINARY-REM.

           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
               COMPUTE WS-REM-AMT ROUNDED =
                        (INTERMED * M9) + CR-LFAMT-ALT
           else
062712        if dte-client = 'AHL'
062712           compute ws-rem-amt rounded = intermed *
062712              (m9 + (cr-loan-term - cr-lf-term))
              ELSE
                 COMPUTE WS-REM-AMT ROUNDED = INTERMED * M9
062712        end-if
           END-IF

           GO TO 1750-DISPLAY-AMTS

           .
       1720-CALC-TEXAS-REM.

           DIVIDE CR-LFAMT BY CR-LF-TERM
               GIVING TEX-FACT-1.
           DIVIDE M9 BY CR-PMT-FREQ
               GIVING TEX-FACT-2
               REMAINDER TEX-FACT-3.

           IF TEX-FACT-3 NOT = ZERO
               ADD +1                  TO TEX-FACT-2
           END-IF

           IF (TEX-FACT-2 * CR-PMT-FREQ) = CR-LF-TERM
               MOVE CR-LFAMT           TO WS-REM-AMT
           ELSE
               COMPUTE WS-REM-AMT ROUNDED =
                   (TEX-FACT-1 * (TEX-FACT-2 * CR-PMT-FREQ))
           END-IF

           GO TO 1750-DISPLAY-AMTS

           .
       1730-CALC-NET-PAY-REM.

           MOVE EFFECT-DT              TO  CP-CERT-EFF-DT

           IF CR-LOAN-1ST-PMT-DT IS NOT = ZEROS
              MOVE CR-LOAN-1ST-PMT-DT  TO  DC-GREG-DATE-1-YMD
              MOVE '3'                 TO  DC-OPTION-CODE
              PERFORM 6150-DATE-CONVERSION-ROUTINE
              MOVE DC-BIN-DATE-1       TO  CP-FIRST-PAY-DATE
           ELSE
              MOVE CP-CERT-EFF-DT      TO  DC-BIN-DATE-1
              MOVE +1                  TO  DC-ELAPSED-MONTHS
              MOVE +0                  TO  DC-ELAPSED-DAYS
              MOVE '6'                 TO  DC-OPTION-CODE
              PERFORM 6150-DATE-CONVERSION-ROUTINE
              MOVE DC-BIN-DATE-2       TO  CP-FIRST-PAY-DATE
           END-IF

           IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'
              COMPUTE CP-ORIGINAL-BENEFIT = CR-LFAMT + CR-LFAMT-ALT
           ELSE
              MOVE CR-LFAMT            TO  CP-ORIGINAL-BENEFIT
           END-IF
           MOVE CR-APR                 TO  CP-LOAN-APR
           MOVE CR-LF-TERM             TO  CP-ORIGINAL-TERM
           MOVE CR-LOAN-TERM           TO  CP-LOAN-TERM
           MOVE M9                     TO  CP-REMAINING-TERM
           MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)
                                       TO CP-SPECIAL-CALC-CD
           MOVE CLAS-I-RL-AH (CLAS-INDEXL)
                                       TO  CP-BENEFIT-TYPE
           MOVE CLAS-I-EP (CLAS-INDEXL)
                                       TO  CP-EARNING-METHOD
062712     if dte-client = 'ahl'
062712        move +14                 to cp-loan-apr
062712     end-if

           CALL 'ELRAMTX' USING CALCULATION-PASS-AREA

           MOVE CP-REMAINING-AMT       TO  WS-REM-AMT

           .
       1750-DISPLAY-AMTS.

      *    IF (CR-DT > 19921231) AND
      *       (CR-DT < 19940101) AND
      *    IF (WS-BIN-VAL-DATES (VYR) = X'95FF')
      *       MOVE CR-DT TO WS-DISPLAY-DATE
      *       MOVE CR-LF-TERM TO WS-DISPLAY-TERM
      *       MOVE LF-REM-TRM2 TO WS-DISPLAY-RTERM
      *       MOVE CR-LFPRM TO WS-DISPLAY-PRM
      *       IF CLAS-I-EP (CLAS-INDEXL) EQUAL 'B' OR 'K' OR 'L'
      *          ADD CR-LFAMT CR-LFAMT-ALT GIVING WS-DISPLAY-AMT
      *       ELSE
      *          MOVE CR-LFAMT     TO WS-DISPLAY-AMT
      *       END-IF
      *       MOVE WS-REM-AMT   TO WS-DISPLAY-RAMT
      *       DISPLAY '  ' CR-CERT-NO '  ' WS-DISPLAY-DATE '   '
      *        WS-DISPLAY-TERM '  ' WS-DISPLAY-RTERM '   '
      *        WS-DISPLAY-AMT '   ' WS-DISPLAY-RAMT
      *    END-IF

           .
       1799-EXIT.
           EXIT.

04050  2000-REINSURANCE-ROUTINE.                                        
04051                                                                   
04052      PERFORM CLEAR-REIN-HOLD THRU CLEAR-REIN-HOLD-X.              
04053                                                                   
04054      IF CR-REIN-TABLE = SPACES OR ZEROS                           
04055         GO TO 2030-REINSURANCE-ROUTINE-X.                         
04056                                                                   
04057      MOVE CR-REIN-TABLE          TO REIN-SRCH.                    
04058      PERFORM 2040-RR-READ-REIN THRU 2050-RR-READ-REIN-X.          
04059                                                                   
04060  2010-REINSURE-ROUTINE-GET-CALC.                                  
04061                                  COPY ECSRTPFM.                   
04062                                                                   
04063  2020-REINSURE-ROUTINE-SKIP-1.                                    
04064      PERFORM 2060-REINSURE-CANCEL THRU 2070-REINSURE-CANCEL-X.    
04065                                                                   
04066      PERFORM 2080-REINSURE-CLAIM THRU 2090-REINSURE-CLAIM-X       
04067          VARYING YR FROM +1 BY +1                                 
04068          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
04069                                                                   
04070      PERFORM 2100-REINSURANCE-EXTRACTS THRU                       
04071              3050-REINSURANCE-EXTRACTS-X                          
04072              VARYING SUB1 FROM +1 BY +1 UNTIL                     
04073              REIN-COMP (SUB1) = SPACES.                           
04074                                                                   
04075  2030-REINSURANCE-ROUTINE-X.                                      
04076      EXIT.                                                        
04077                                                                   
04078      EJECT                                                        
04079  2040-RR-READ-REIN.                                               
04080      IF REIN-OPEN-SW = ' '                                        
04081         MOVE DTE-CLASIC-COMPANY-CD TO REIN-SRCH-COMP-CD           
04082         MOVE 'X'                   TO REIN-OPEN-SW                
04083         MOVE 'A'                   TO REIN-SRCH-CODE              
04084         OPEN INPUT ERRTBL-IN
04085         IF ERRTBL-FILE-STATUS NOT = '00' AND '97'                 
04086            MOVE ' ERROR ON OPEN ERRTBL ' TO WS-ABEND-MESSAGE      
04087            MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS        
04088            GO TO ABEND-PGM                                        
04089         ELSE                                                      
04090            PERFORM REIN-CO-TABLE-BUILD THRU REIN-BUILD-EXIT.      
04091                                                                   
04092      IF REIN-SRCH NOT = SAVE-REIN-SRCH                            
04093         MOVE REIN-SRCH   TO SAVE-REIN-SRCH                        
04094         MOVE REIN-SEARCH TO RE-CONTROL-PRIMARY                    
04095         READ ERRTBL-IN
04096         IF ERRTBL-FILE-STATUS NOT = '00'                          
04097            MOVE ' ERROR ON READ ERRTBL ' TO WS-ABEND-MESSAGE      
04098            MOVE ERRTBL-FILE-STATUS TO WS-ABEND-FILE-STATUS        
04099            GO TO ABEND-PGM.                                       
04100                                                                   
      *    ADD  1 TO WS-RTBL-DB-CNT
      *              WS-TOT-RTBL-DB-CNT
      *    IF WS-RTBL-DB-CNT > 10000
      *       MOVE ZEROS TO WS-RTBL-DB-CNT
      *       DISPLAY ' RTBLS  READ ' WS-TOT-RTBL-DB-CNT
      *    END-IF
           .
04101  2050-RR-READ-REIN-X.                                             
04102      EXIT.                                                        
04103                                                                   
04104      EJECT                                                        
04105  2060-REINSURE-CANCEL.                                            
04106                                                                   
04107      IF REIN-COMP (1) = SPACES                                    
04108         GO TO 2070-REINSURE-CANCEL-X.                             
04109                                                                   
04110      IF CR-LFRFND = +0 AND                                        
04111         CR-AHRFND = +0                                            
04112         GO TO 2070-REINSURE-CANCEL-X.                             
04113                                                                   
04114      PERFORM REINSURE-CALC-CANCEL THRU REINSURE-CALC-CANCEL-X     
04115              VARYING SUB1 FROM +1 BY +1 UNTIL                     
04116                        REIN-COMP (SUB1) = SPACES.                 
04117                                                                   
04118  2070-REINSURE-CANCEL-X.                                          
04119      EXIT.                                                        
04120                                                                   
04121  2080-REINSURE-CLAIM.                                             
04122                                                                   
04123      IF REIN-COMP (1) = SPACES                                    
04124         GO TO 2090-REINSURE-CLAIM-X.                              
04125                                                                   
04126      IF CERT-NO-CLAIM                                             
04127         GO TO 2090-REINSURE-CLAIM-X.                              
04128                                                                   
04129      MOVE WS-CLAIM-AMT (1 1 YR) TO RW-LFCLMWK.                    
04130      MOVE WS-CLAIM-AMT (2 1 YR) TO RW-AHCLMWK.                    
04131      MOVE +0                    TO RW-LFCLM                       
04132                                    RW-AHCLM.                      
04133                                                                   
04134      IF RW-LFCLMWK = +0                                           
04135          AND RW-AHCLMWK = +0                                      
04136              GO TO 2090-REINSURE-CLAIM-X.                         
04137                                                                   
04138      PERFORM REINSURE-CALC-CLAIM THRU REINSURE-CALC-CLAIM-X       
04139                   VARYING SUB1 FROM +1 BY +1 UNTIL                
04140                             REIN-COMP (SUB1) = SPACES.            
04141                                                                   
04142      MOVE RWF-LFCLML TO WS-CLAIM-AMT (1 2 YR)                     
04143      MOVE RWF-AHCLML TO WS-CLAIM-AMT (2 2 YR).                    
04144                                                                   
04145  2090-REINSURE-CLAIM-X.                                           
04146      EXIT.                                                        
04147                                                                   
04148                                                                   
04149      EJECT                                                        
04150  2100-REINSURANCE-EXTRACTS.                                       
04151                                                                   
04152      IF REIN-LF-AH-FLGS (SUB1) = SPACES                           
04153         GO TO 3050-REINSURANCE-EXTRACTS-X.                        
04154                                                                   
04155      IF REIN-REM-SW (SUB1) = 'Z'                                  
04156         GO TO 3050-REINSURANCE-EXTRACTS-X.                        
04157                                                                   
04158      MOVE HOLD-LIFE-RECORD      TO CALL-EXTRACT.                  
04159                                                                   
04160      MOVE REIN-COMP (SUB1)      TO EX-REIN.                       
04161      MOVE REIN-WORK-FLDS (SUB1) TO RWF-FIELDS.                    
04162                                                                   
04163      MOVE ZEROS                 TO  EX-PREM                       
04164                         EX-PRIM-FAC-PREM                          
04165                            EX-OVR-COMM (1)                        
04166                           EX-OVR-COMM (2)                         
04167                           EX-OVR-COMM (3)                         
04168                             EX-AGT-COMM (1)                       
04169                             EX-AGT-COMM (2)                       
04170                             EX-AGT-COMM (3)                       
04171                             EX-INITIAL-AMT                        
04172                             EX-BENEFIT                            
04173                             EX-CNC-AMT (1)                        
04174                             EX-CNC-AMT (2)                        
04175                             EX-CNC-AMT (3)                        
04176                             EX-CLAIM-AMT (1)                      
04177                             EX-CLAIM-AMT (2)                      
04178                             EX-CLAIM-AMT (3).                     
04179                                                                   
04180                                                                   
04181      IF REIN-LF-FLG (SUB1) = 'X'                                  
04182          MOVE RWF-LFAMT          TO EX-INITIAL-AMT                
04183          MOVE RWF-LFPRM          TO EX-PREM                       
04184                                     EX-PRIM-FAC-PREM              
04185          MOVE CR-LFTYP           TO EX-BEN-TYPE                   
04186      ELSE                                                         
04187          MOVE ZEROS              TO EX-BEN-TYPE.                  
04188                                                                   
04189      IF REIN-LF-FLG (SUB1) = 'X'                                  
04190         IF SINGLE-PERIOD-PROCESS                                  
04191             IF LF-CAN GREATER THAN BEGIN-DT (1) AND               
04192                LF-CAN NOT GREATER THAN END-DT (1)                 
04193                MOVE RWF-LFRFND   TO EX-CNC-AMT (1)                
04194             ELSE                                                  
04195                NEXT SENTENCE                                      
04196         ELSE                                                      
04197         IF LF-CAN GREATER THAN BEGIN-DT (1) AND                   
04198            LF-CAN NOT GREATER THAN END-DT (1)                     
04199            MOVE RWF-LFRFND       TO EX-CNC-AMT (1)                
04200         ELSE                                                      
04201         IF LF-CAN GREATER THAN BEGIN-DT (2) AND                   
04202            LF-CAN NOT GREATER THAN END-DT (2)                     
04203            MOVE RWF-LFRFND       TO EX-CNC-AMT (2)                
04204         ELSE                                                      
04205         IF LF-CAN GREATER THAN BEGIN-DT (3) AND                   
04206            LF-CAN NOT GREATER THAN END-DT (3)                     
04207            MOVE RWF-LFRFND       TO EX-CNC-AMT (3).               
04208                                                                   
04209      IF (REIN-LF-FLG (SUB1) = 'X') AND                            
04210         (CR-LFPRM NOT = +0)                                       
04211         COMPUTE LF-DEV-TO-PRIM-FACTOR =                           
04212               (WS-HOLD-LF-PRIM-FAC / CR-LFPRM)                    
04213         COMPUTE EX-PRIM-FAC-PREM =                                
04214               (LF-DEV-TO-PRIM-FACTOR * EX-PREM).                  
04215                                                                   
04216  2110-DO-REIN-LF-CLM.                                             
04217                                                                   
04218      IF SINGLE-PERIOD-PROCESS                                     
04219          MOVE WS-CLAIM-AMT (1 2 1) TO EX-CLAIM-AMT (1)            
04220          MOVE WS-CLAIM-AMT (1 2 2) TO EX-CLAIM-AMT (2)            
04221      ELSE                                                         
04222          MOVE WS-CLAIM-AMT (1 2 1) TO EX-CLAIM-AMT (1)            
04223          MOVE WS-CLAIM-AMT (1 2 2) TO EX-CLAIM-AMT (2)            
04224          MOVE WS-CLAIM-AMT (1 2 3) TO EX-CLAIM-AMT (3).           
04225                                                                   
04226      MOVE +0 TO SB.                                               
04227                                                                   
04228  2120-DO-REIN-LF-ISS-COMM.                                        
04229                                                                   
04230      ADD +1                      TO SB.                           
04231      IF SB GREATER THAN NUMBER-OF-PERIODS                         
04232          MOVE +0      TO SB                                       
04233          GO TO 2130-DO-REIN-LF-CNC-COMM.                          
04234                                                                   
04235      IF EX-PREM = +0                                              
04236         GO TO 2120-DO-REIN-LF-ISS-COMM.                           
04237                                                                   
04238      IF ENTRY-DT GREATER THAN BEGIN-DT (SB) AND                   
04239         NOT GREATER THAN END-DT (SB)                              
04240         MOVE +1                     TO SV-FACTOR                  
04241         MOVE EX-PREM                TO COMM-PREMM                 
04242         MOVE 'X'                    TO FROM-REIN                  
04243         PERFORM 5000-COMMISSIONS-LIFE THRU 5020-L-COMM-EXIT.      
04244                                                                   
04245      GO TO 2120-DO-REIN-LF-ISS-COMM.                              
04246                                                                   
04247  2130-DO-REIN-LF-CNC-COMM.                                        
04248                                                                   
04249      ADD +1                      TO SB.                           
04250      IF SB GREATER THAN NUMBER-OF-PERIODS                         
04251          GO TO 2140-END-LIFE-REIN.                                
04252                                                                   
04253      IF EX-CNC-AMT (SB) = +0                                      
04254         GO TO 2130-DO-REIN-LF-CNC-COMM.                           
04255                                                                   
04256      MOVE -1                     TO SV-FACTOR.                    
04257      MOVE EX-CNC-AMT (SB)        TO COMM-PREMM.                   
04258      MOVE 'X'                    TO FROM-REIN.                    
04259      PERFORM 5000-COMMISSIONS-LIFE THRU 5020-L-COMM-EXIT.         
04260                                                                   
04261      GO TO 2130-DO-REIN-LF-CNC-COMM.                              
04262                                                                   
04263  2140-END-LIFE-REIN.                                              
04264                                                                   
04265      MOVE EX-PREM TO WRK-C.                                       
04266      MOVE +0          TO WRK-ALT.                                 
04267                                                                   
04268      PERFORM 0480-CALC-LIFE-UNEARN THRU 0530-CALC-UNEARN-FINISH   
04269          VARYING YR FROM +1 BY +1                                 
04270          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
04271                                                                   
04272      PERFORM 0430-BUILD-LF-DEV-RESERVES                           
04273          VARYING YR FROM +1 BY +1                                 
04274          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
04275                                                                   
04276      IF EX-PREM NOT = EX-PRIM-FAC-PREM                            
04277         MOVE EX-PRIM-FAC-PREM TO WRK-C                            
04278         MOVE +0               TO WRK-ALT                          
04279         PERFORM 0480-CALC-LIFE-UNEARN THRU                        
04280                              0540-LIFE-UNEARN-EXIT                
04281             VARYING YR FROM +1 BY +1                              
04282             UNTIL YR GREATER THAN NUMBER-OF-PERIODS               
04283         PERFORM 0450-BUILD-LF-PRI-RESERVES                        
04284             VARYING YR FROM +1 BY +1                              
04285             UNTIL YR GREATER THAN NUMBER-OF-PERIODS.              
04286                                                                   
04287      IF DTE-PGM-OPT NOT = '3' AND '4'                             
04288         GO TO 2150-BYPASS-LF-DETAIL-REIN.                         
04289                                                                   
04290      MOVE EX-CARRIER           TO D-CARR.                         
04291      MOVE EX-GROUPING          TO D-GROUP.                        
04292      MOVE EX-STATE             TO D-STATE.                        
04293      MOVE EX-ACCOUNT           TO D-ACCOUNT.                      
04294      MOVE CR-CERT-NO           TO D-CERT-NO.                      
04295                                                                   
04296      MOVE 'L'                  TO DC-OPTION-CODE.                 
04297      MOVE EX-EFF-DATE          TO DC-GREG-DATE-CYMD.              
04298      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
04299      MOVE DC-GREG-DATE-1-EDIT  TO D-EFF-DATE.                     
04300                                                                   
04301      MOVE EX-PREM              TO D-PREM.                         
04302      MOVE EX-PRIM-FAC-PREM     TO D-PRI-PREM.                     
04303      MOVE EX-TERM              TO D-TERM.                         
04304      MOVE M2 (1)               TO D-REM-TERM.                     
04305      MOVE EX-END-ST-RES (1)    TO D-UNEARN.                       
04306      MOVE EX-INITIAL-AMT       TO D-BENEFIT.                      
04307      MOVE 'L'                  TO D-L-A.                          
04308      MOVE 'LIFE REIN '         TO D-BEN-DESC.                     
04309                                                                   
04310      PERFORM 1600-PRINT-A-LINE.                                   
04311                                                                   
04312  2150-BYPASS-LF-DETAIL-REIN.                                      
04313                                                                   
04314      PERFORM 1520-RELEASE-SORT-RECORD THRU 1540-RELEASE-EXIT.     
04315                                                                   
04316  3000-DO-AH-REIN.                                                 
04317                                                                   
04318      MOVE HOLD-AH-RECORD        TO CALL-EXTRACT.                  
04319                                                                   
04320      MOVE REIN-COMP (SUB1)      TO EX-REIN.                       
04321                                                                   
04322      MOVE ZEROS                 TO EX-PREM                        
04323                                    EX-PRIM-FAC-PREM               
04324                                    EX-OVR-COMM (1)                
04325                                    EX-OVR-COMM (2)                
04326                                    EX-OVR-COMM (3)                
04327                                    EX-AGT-COMM (1)                
04328                                    EX-AGT-COMM (2)                
04329                                    EX-AGT-COMM (3)                
04330                                    EX-INITIAL-AMT                 
04331                                    EX-BENEFIT                     
04332                                    EX-CNC-AMT (1)                 
04333                                    EX-CNC-AMT (2)                 
04334                                    EX-CNC-AMT (3)                 
04335                                    EX-CLAIM-AMT (1)               
04336                                    EX-CLAIM-AMT (2)               
04337                                    EX-CLAIM-AMT (3).              
04338                                                                   
04339      IF REIN-AH-FLG (SUB1) = 'X'                                  
04340          MOVE RWF-AHAMT          TO EX-BENEFIT                    
04341          MOVE RWF-AHPRM          TO EX-PREM                       
04342                                     EX-PRIM-FAC-PREM              
04343          MOVE CR-AHTYP           TO EX-BEN-TYPE                   
04344      ELSE                                                         
04345          MOVE ZEROS              TO EX-BEN-TYPE.                  
04346                                                                   
04347      IF REIN-AH-FLG (SUB1) = 'X'                                  
04348         IF SINGLE-PERIOD-PROCESS                                  
04349             IF AH-CAN GREATER THAN BEGIN-DT (1) AND               
04350                AH-CAN NOT GREATER THAN END-DT (1)                 
04351                MOVE RWF-AHRFND   TO EX-CNC-AMT (1)                
04352             ELSE                                                  
04353                NEXT SENTENCE                                      
04354         ELSE                                                      
04355         IF AH-CAN GREATER THAN BEGIN-DT (1) AND                   
04356            AH-CAN NOT GREATER THAN END-DT (1)                     
04357            MOVE RWF-AHRFND       TO EX-CNC-AMT (1)                
04358         ELSE                                                      
04359         IF AH-CAN GREATER THAN BEGIN-DT (2) AND                   
04360            AH-CAN NOT GREATER THAN END-DT (2)                     
04361            MOVE RWF-AHRFND       TO EX-CNC-AMT (2)                
04362         ELSE                                                      
04363         IF AH-CAN GREATER THAN BEGIN-DT (3) AND                   
04364            AH-CAN NOT GREATER THAN END-DT (3)                     
04365            MOVE RWF-AHRFND       TO EX-CNC-AMT (3).               
04366                                                                   
04367      IF (REIN-AH-FLG (SUB1) = 'X') AND                            
04368         (CR-AHPRM NOT = +0)                                       
04369         COMPUTE AH-DEV-TO-PRIM-FACTOR =                           
04370               (WS-HOLD-AH-PRIM-FAC / CR-AHPRM)                    
04371         COMPUTE EX-PRIM-FAC-PREM =                                
04372               (AH-DEV-TO-PRIM-FACTOR * EX-PREM).                  
04373                                                                   
04374  3010-DO-REIN-AH-CLM.                                             
04375                                                                   
04376      IF SINGLE-PERIOD-PROCESS                                     
04377          MOVE WS-CLAIM-AMT (2 2 1) TO EX-CLAIM-AMT (1)            
04378          MOVE WS-CLAIM-AMT (2 2 2) TO EX-CLAIM-AMT (2)            
04379      ELSE                                                         
04380          MOVE WS-CLAIM-AMT (2 2 1) TO EX-CLAIM-AMT (1)            
04381          MOVE WS-CLAIM-AMT (2 2 2) TO EX-CLAIM-AMT (2)            
04382          MOVE WS-CLAIM-AMT (2 2 3) TO EX-CLAIM-AMT (3).           
04383                                                                   
04384      MOVE +0                     TO SB.                           
04385                                                                   
04386  3020-DO-REIN-AH-ISS-COMM.                                        
04387                                                                   
04388      ADD +1 TO SB.                                                
04389      IF SB GREATER THAN NUMBER-OF-PERIODS                         
04390          MOVE +0      TO SB                                       
04391          GO TO 3030-DO-REIN-AH-CNC-COMM.                          
04392                                                                   
04393      IF EX-PREM = +0                                              
04394         GO TO 3020-DO-REIN-AH-ISS-COMM.                           
04395                                                                   
04396      IF ENTRY-DT GREATER THAN BEGIN-DT (SB) AND                   
04397         NOT GREATER THAN END-DT (SB)                              
04398         MOVE +1                     TO SV-FACTOR                  
04399         MOVE EX-PREM                TO COMM-PREMM                 
04400         MOVE 'X'                    TO FROM-REIN                  
04401         PERFORM 5030-COMMISSIONS-AH THRU 5050-A-COMM-EXIT.        
04402                                                                   
04403      GO TO 3020-DO-REIN-AH-ISS-COMM.                              
04404                                                                   
04405  3030-DO-REIN-AH-CNC-COMM.                                        
04406                                                                   
04407      ADD +1 TO SB.                                                
04408      IF SB GREATER THAN NUMBER-OF-PERIODS                         
04409          GO TO 3040-END-AH-REIN.                                  
04410                                                                   
04411      IF EX-CNC-AMT (SB) = +0                                      
04412         GO TO 3030-DO-REIN-AH-CNC-COMM.                           
04413                                                                   
04414      MOVE -1                     TO SV-FACTOR.                    
04415      MOVE EX-CNC-AMT (SB)        TO COMM-PREMM.                   
04416      MOVE 'X'                    TO FROM-REIN.                    
04417      PERFORM 5030-COMMISSIONS-AH THRU 5050-A-COMM-EXIT.           
04418                                                                   
04419      GO TO 3030-DO-REIN-AH-CNC-COMM.                              
04420                                                                   
04421  3040-END-AH-REIN.                                                
04422                                                                   
04423      MOVE EX-PREM TO WRK-C.                                       
04424                                                                   
04425 *    PERFORM 1200-CALC-AH-UNEARN THRU 1299-AH-UNEARN-EXIT         
011405     PERFORM 1200-CALC-AH-UNEARN THRU 1200-EXIT
04426          VARYING YR FROM +1 BY +1                                 
04427          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
04428                                                                   
04429      PERFORM 1150-BUILD-AH-DEV-RESERVES                           
04430          VARYING YR FROM +1 BY +1                                 
04431          UNTIL YR GREATER THAN NUMBER-OF-PERIODS.                 
04432                                                                   
04433      IF EX-PREM NOT = EX-PRIM-FAC-PREM                            
04434         MOVE EX-PRIM-FAC-PREM TO WRK-C                            
04435 *       PERFORM 1200-CALC-AH-UNEARN THRU 1299-AH-UNEARN-EXIT      
011405        PERFORM 1200-CALC-AH-UNEARN THRU 1200-EXIT
04436             VARYING YR FROM +1 BY +1                              
04437             UNTIL YR GREATER THAN NUMBER-OF-PERIODS               
04438             PERFORM 1170-BUILD-AH-PRI-RESERVES                    
04439                 VARYING YR FROM +1 BY +1                          
04440                 UNTIL YR GREATER THAN NUMBER-OF-PERIODS.          
04441                                                                   
04442      IF DTE-PGM-OPT NOT = '3' AND '4'                             
04443         GO TO 3050-BYPASS-AH-DETAIL-REIN.                         
04444                                                                   
04445      MOVE EX-CARRIER           TO D-CARR.                         
04446      MOVE EX-GROUPING          TO D-GROUP.                        
04447      MOVE EX-STATE             TO D-STATE.                        
04448      MOVE EX-ACCOUNT           TO D-ACCOUNT.                      
04449      MOVE CR-CERT-NO           TO D-CERT-NO.                      
04450                                                                   
04451      MOVE 'L'                  TO DC-OPTION-CODE.                 
04452      MOVE EX-EFF-DATE          TO DC-GREG-DATE-CYMD.              
04453      PERFORM 6150-DATE-CONVERSION-ROUTINE.                        
04454      MOVE DC-GREG-DATE-1-EDIT  TO D-EFF-DATE.                     
04455                                                                   
04456      MOVE EX-PREM              TO D-PREM.                         
04457      MOVE EX-PRIM-FAC-PREM     TO D-PRI-PREM.                     
04458      MOVE EX-TERM              TO D-TERM.                         
04459      MOVE M2 (1)               TO D-REM-TERM.                     
04460      MOVE EX-END-ST-RES (1)    TO D-UNEARN.                       
04461      MOVE EX-BENEFIT           TO D-BENEFIT.                      
04462      MOVE 'A'                  TO D-L-A.                          
04463      MOVE ' AH REIN'           TO D-BEN-DESC.                     
04464                                                                   
04465      PERFORM 1600-PRINT-A-LINE.                                   
04466                                                                   
04467  3050-BYPASS-AH-DETAIL-REIN.                                      
04468                                                                   
04469      PERFORM 1520-RELEASE-SORT-RECORD THRU 1540-RELEASE-EXIT.     
04470                                                                   
04471  3050-REINSURANCE-EXTRACTS-X.                                     
04472      EXIT.                                                        
04473                                                                   
04474      EJECT                                                        
04475  4000-REINSURANCE-ROUTINES.                                       
04476                              COPY ECSRIRTN.                       
04477                                                                   
04478  5000-COMMISSIONS-LIFE.                                           
04479      MOVE +0 TO SA.                                               
04480                                                                   
04481  5010-COMM-LIFE-LOOP.                                             
04482                                                                   
04483      ADD +1 TO SA.                                                
04484      IF SA GREATER +10                                            
04485          GO TO 5020-L-COMM-EXIT.                                  
04486                                                                   
04487      IF CR-COM-AGT (SA) = ZEROS OR SPACES                         
04488         GO TO 5010-COMM-LIFE-LOOP.                                
04489                                                                   
04490      IF FROM-REIN = 'X'                                           
04491         IF CR-AGT-TYPE (SA) = 'W'                                 
04492            MOVE CR-COM-AGT (SA) TO WS-AGT-NO                      
04493            IF WS-AGT-SIX = REIN-COMP (SUB1)                       
04494               COMPUTE AGT-COMM =                                  
04495                  SV-FACTOR * (COMM-PREMM * CR-LCOM-L (SA))        
04496                  ADD AGT-COMM TO EX-AGT-COMM (SB)                 
04497            ELSE                                                   
04498               NEXT SENTENCE                                       
04499         ELSE                                                      
052814           IF CR-AGT-TYPE (SA) = 'R' OR 'D' OR 'U'
04501               COMPUTE AGT-COMM =                                  
04502                  SV-FACTOR * (COMM-PREMM * CR-LCOM-L (SA))        
04503                  ADD AGT-COMM TO EX-AGT-COMM (SB)                 
04504            ELSE                                                   
04505               IF CR-AGT-TYPE (SA) = 'T' OR 'P' OR 'V' OR 'B'      
04506                  COMPUTE AGT-COMM =                               
04507                    SV-FACTOR * (COMM-PREMM * CR-LCOM-L (SA))      
04508                    ADD AGT-COMM TO EX-OVR-COMM (SB).              
04509                                                                   
04510      GO TO 5010-COMM-LIFE-LOOP.                                   
04511                                                                   
04512  5020-L-COMM-EXIT.                                                
04513      EXIT.                                                        
04514                                                                   
04515  5030-COMMISSIONS-AH.                                             
04516      MOVE +0 TO SA.                                               
04517                                                                   
04518  5040-COMM-AH-LOOP.                                               
04519      ADD +1 TO SA.                                                
04520                                                                   
04521      IF SA GREATER +10                                            
04522          GO TO 5050-A-COMM-EXIT.                                  
04523                                                                   
04524      IF CR-COM-AGT (SA) = ZEROS OR SPACES                         
04525          GO TO 5040-COMM-AH-LOOP.                                 
04526                                                                   
04527      IF FROM-REIN = 'X'                                           
04528         IF CR-AGT-TYPE (SA) = 'W'                                 
04529            MOVE CR-COM-AGT (SA) TO WS-AGT-NO                      
04530            IF WS-AGT-SIX = REIN-COMP (SUB1)                       
04531               COMPUTE AGT-COMM =                                  
04532                 SV-FACTOR * (COMM-PREMM * CR-LCOM-AH (SA))        
04533                 ADD AGT-COMM TO EX-AGT-COMM (SB)                  
04534            ELSE                                                   
04535               NEXT SENTENCE                                       
04536         ELSE                                                      
052814           IF CR-AGT-TYPE (SA) = 'R' OR 'D' OR 'U'
04538               COMPUTE AGT-COMM =                                  
04539                 SV-FACTOR * (COMM-PREMM * CR-LCOM-AH (SA))        
04540                  ADD AGT-COMM TO EX-AGT-COMM (SB)                 
04541            ELSE                                                   
04542               IF CR-AGT-TYPE (SA) = 'T' OR 'P' OR 'V' OR 'B'      
04543                  COMPUTE AGT-COMM =                               
04544                    SV-FACTOR * (COMM-PREMM * CR-LCOM-AH (SA))     
04545                    ADD AGT-COMM TO EX-OVR-COMM (SB).              
04546                                                                   
04547      GO TO 5040-COMM-AH-LOOP.                                     
04548                                                                   
04549  5050-A-COMM-EXIT.                                                
04550      EXIT.                                                        
04551      EJECT                                                        
04552                                                                   
04553  6000-CALC-REM-TERM.                                              
04554                                                                   
04555      IF CR-LOAN-1ST-PMT-DT NOT NUMERIC                            
04556          MOVE ZEROS                  TO CR-LOAN-1ST-PMT-DT.       
04557                                                                   
04558      IF CR-LOAN-1ST-PMT-DT NOT = ZEROS                            
04559          MOVE CR-LOAN-1ST-PMT-DT     TO DC-GREG-DATE-1-YMD        
04560          MOVE ZEROS                  TO DC-ELAPSED-MONTHS         
04561                                         DC-ELAPSED-DAYS           
04562          MOVE '3'                    TO DC-OPTION-CODE            
04563          PERFORM 6150-DATE-CONVERSION-ROUTINE                     
04564          IF NO-CONVERSION-ERROR                                   
04565              MOVE DC-BIN-DATE-1      TO CP-FIRST-PAY-DATE         
04566          ELSE                                                     
04567              MOVE ZEROS              TO CR-LOAN-1ST-PMT-DT.       
04568                                                                   
04569      IF CR-LOAN-1ST-PMT-DT = ZEROS                                
04570          MOVE CP-CERT-EFF-DT         TO DC-BIN-DATE-1             
04571          MOVE +1                     TO DC-ELAPSED-MONTHS         
04572          MOVE ZEROS                  TO DC-ELAPSED-DAYS           
04573          MOVE '6'                    TO DC-OPTION-CODE            
04574          PERFORM 6150-DATE-CONVERSION-ROUTINE                     
04575          MOVE DC-BIN-DATE-2          TO CP-FIRST-PAY-DATE         
04576          MOVE DC-GREG-DATE-1-YMD     TO CR-LOAN-1ST-PMT-DT.       
04577                                                                   
04578      MOVE STATE-SUB (CLAS-INDEXS)    TO CP-STATE.                 
04579      MOVE STATE-ABBR (CLAS-INDEXS)   TO CP-STATE-STD-ABBRV.       
04580      MOVE '3'                        TO CP-PROCESS-TYPE.          
04581      MOVE DTE-CLASIC-COMPANY-CD      TO CP-COMPANY-CD.            
04582      MOVE DTE-CLIENT                 TO CP-COMPANY-ID.            
04583      MOVE SPACES                     TO CP-ACCT-FLD-5.            
04584      MOVE DTE-REM-TRM                TO CP-REM-TERM-METHOD.       
04585      MOVE DTE-REM-TRM-CALC-OPTION    TO CP-REM-TRM-CALC-OPTION.   
04586                                                                   
04587      IF DTE-CLIENT = 'FIM'                                        
04588          MOVE '5'                    TO CP-REM-TERM-METHOD.       
04589                                                                   
04590      IF DTE-CLIENT = 'POS'                                        
04591         IF CR-CARRIER = '1'                                       
04592            MOVE '1'                TO CP-REM-TERM-METHOD          
04593         ELSE                                                      
04594            MOVE '2'                TO CP-REM-TERM-METHOD.         
04595                                                                   
04596      CALL 'ELRTRMX' USING CALCULATION-PASS-AREA.                  
04597                                                                   
04598      IF DTE-CLIENT = 'NCL'                                        
04599          MOVE CP-REMAINING-TERM-1 TO CP-REMAINING-TERM-2.         
04600                                                                   
04601      IF FROM-LIFE-SW = 'Y'                                        
04602         IF (CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L') AND      
04603           (CLAS-I-CALC-TYPE (CLAS-INDEXL) NOT = 'L')              
04604           MOVE CP-REMAINING-TERM-2 TO LF-BAL-REMTERM              
04605           COMPUTE CP-REMAINING-TERM-1 =                           
04606                   CP-REMAINING-TERM-1 - +1                        
04607           COMPUTE CP-REMAINING-TERM-2 =                           
04608                   CP-REMAINING-TERM-2 - +1.                       
04609                                                                   
04610      IF CP-REMAINING-TERM-1 NEGATIVE                              
04611          MOVE ZEROS TO CP-REMAINING-TERM-1.                       
04612                                                                   
04613      IF CP-REMAINING-TERM-2 NEGATIVE                              
04614          MOVE ZEROS TO CP-REMAINING-TERM-2.                       
04615                                                                   
04616      IF NO-CP-ERROR                                               
04617         MOVE CP-REMAINING-TERM-1        TO REM-TRM1               
04618         MOVE CP-REMAINING-TERM-2        TO REM-TRM2               
04619      ELSE                                                         
04620         MOVE +0                         TO REM-TRM1               
04621                                            REM-TRM2.              
04622                                                                   
04623  6010-REM-TERM-EXIT.                                              
04624      EXIT.                                                        
04625                                                                   
04626      EJECT                                                        
04627  6100-E-O-J-RTN.                                                  
04628                                                                   
04629      OPEN INPUT XTRS-IN.                                          
04630                                                                   
04631  6103-READ-LOSS-XTRS.                                             
04632                                                                   
04633      READ XTRS-IN INTO CALL-EXTRACT                               
04634              AT END                                               
04635                 CLOSE XTRS-IN                                     
04636                 GO TO 6107-CLOSE-FILES.                           
04637                                                                   
04638      PERFORM 1520-RELEASE-SORT-RECORD THRU 1540-RELEASE-EXIT.     
04639                                                                   
04640      GO TO 6103-READ-LOSS-XTRS.                                   
04641                                                                   
04642  6107-CLOSE-FILES.                                                
04643                                                                   
04644      MOVE '0'                 TO X.                               
04645      MOVE '   ECS158  EXTRACT COMPLETE  ' TO P-DATA.              
04646      PERFORM 1610-PRT-RTN THRU 1620-E-PRT-RTN.                    
04647      DISPLAY '  CONTROL TOTALS  '.                                
04648      DISPLAY ' LIFE RATE ATTEMPTS....... ' WS-LF-RATE-ATTEMPT.    
04649      DISPLAY ' LIFE RATE ERRORS  ....... ' WS-LF-RATE-ERROR.      
04650      DISPLAY ' AH   RATE ATTEMPTS....... ' WS-AH-RATE-ATTEMPT.    
04651      DISPLAY ' AH   RATE ERRORS  ....... ' WS-AH-RATE-ERROR.      
04652      DISPLAY ' TEXAS ADJ RECS    ....... ' TEXAS-CNT.             
04653      DISPLAY ' '.                                                 
04654                                                                   
04655      CLOSE CERTS  PRINTX XTRACT.                                  
04656      CLOSE ERACCTT.                                               
04657                                                                   
04658      IF REIN-OPEN-SW = 'X'                                        
04659         CLOSE ERRTBL-IN.
04660                                                                   
04661      CLOSE SORTED-CLAIMS.                                         
04662                                                                   
04663      IF DTE-PGM-OPT = '3' OR '4'                                  
04664         CLOSE PRINTC.                                             
04665                                                                   
04666      MOVE 'R' TO CP-RATE-FILE.                                    
04667      MOVE 'C' TO CP-IO-FUNCTION.                                  
04668      PERFORM 6130-CALL-RATING-ROUTINE THRU 6140-RATE-EXIT.        
04669                                                                   
04670      IF IO-ERROR                                                  
04671         MOVE +0303   TO WS-RETURN-CODE                            
04672         MOVE ' ERROR ON ERRATE CLOSE' TO WS-ABEND-MESSAGE         
04673         GO TO ABEND-PGM.                                          
04674                                                                   
04675      IF ERACCTT-FILE-STATUS NOT = '00'                            
04676         MOVE ' ERROR ON CLOSE  ERACCT ' TO WS-ABEND-MESSAGE       
04677         MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
04678         GO TO ABEND-PGM.                                          
04679                                                                   
04680  6110-CLOSE-FICH.                                                 
04681                              COPY ELCPRTC.                        
04682                                                                   
04683      MOVE ZEROS  TO RETURN-CODE.
04683      GOBACK.                                                      
04684                                                                   
04685  6120-E-O-J-XIT.                                                  
04686      EXIT.                                                        
04687                                                                   
04688  6130-CALL-RATING-ROUTINE.                                        
04689                                                                   
           ADD 1 TO WS-RATE-DB-CNT
                    WS-TOT-RATE-DB-CNT
           IF WS-RATE-DB-CNT > 10000
              DISPLAY ' RATES ' WS-TOT-RATE-DB-CNT
              MOVE ZEROS TO WS-RATE-DB-CNT
           END-IF
04690      CALL 'ELRATEX' USING CALCULATION-PASS-AREA.                  
04691                                                                   
04692  6140-RATE-EXIT.                                                  
04693      EXIT.                                                        
04694                                                                   
04695  6150-DATE-CONVERSION-ROUTINE.                                    
04696      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
04697                                                                   
04698  6200-LOCATE-STATE.                                               
04699      MOVE SPACES TO SAVE-STATE                                    
04700                     WS-REPORT-SW.                                 
04701      MOVE +0     TO EXCL-SUB.                                     
04702                                                                   
04703  6200-LOCATE-STATE-LOOP.                                          
04704      ADD +1 TO EXCL-SUB.                                          
04705                                                                   
04706      IF EXCL-SUB GREATER THAN CLAS-MAXS                           
04707          MOVE TEMP-STATE TO SAVE-STATE                            
04708          GO TO 6200-EXIT.                                         
04709                                                                   
04710      IF STATE-SUB (EXCL-SUB) = TEMP-STATE                         
04711          MOVE TEMP-STATE TO SAVE-STATE                            
04712          MOVE STATE-CALL-BREAK (EXCL-SUB) TO WS-REPORT-SW         
04713          GO TO 6200-EXIT.                                         
04714                                                                   
04715      GO TO 6200-LOCATE-STATE-LOOP.                                
04716                                                                   
04717  6200-EXIT.                                                       
04718       EXIT.                                                       
04719                                                                   
04720      EJECT                                                        
04721  6300-LOCATE-BUSC-TYPE.                                           
04722                                                                   
04723      MOVE ZEROS                  TO SAVE-BUSC-TYPE.               
04724      MOVE SPACES                 TO WS-EXCL-BUSC-SW.              
04725                                                                   
04726      MOVE +1                     TO CLAS-INDEXB.                  
04727                                                                   
04728      IF AM-GPCD NOT NUMERIC                                       
04729          GO TO 6399-EXIT.                                         
04730                                                                   
04731      MOVE AM-GPCD            TO SAVE-BUSC-TYPE.                   
04732                                                                   
04733  6300-LOCATE-BUSC-LOOP.                                           
04734                                                                   
04735      IF CLAS-INDEXB GREATER THAN CLAS-MAXB                        
04736          GO TO 6399-EXIT.                                         
04737                                                                   
04738      IF CLAS-BUSC-CODE (CLAS-INDEXB) NOT = SAVE-BUSC-TYPE         
04739          ADD +1 TO CLAS-INDEXB                                    
04740          GO TO 6300-LOCATE-BUSC-LOOP.                             
04741                                                                   
04742      MOVE CLAS-BUSC-EXCL (CLAS-INDEXB) TO WS-EXCL-BUSC-SW.        
04743                                                                   
04744  6399-EXIT.                                                       
04745      EXIT.                                                        
04746                                                                   
04747  9999-DUM-END.                                                    
04748      MOVE ZEROS  TO RETURN-CODE.
04748      GOBACK.                                                      
04749                                                                   
04750  ABEND-PGM.                                                       
04751                           COPY ELCABEND.                          
04752                                                                   
04753      EJECT                                                        
