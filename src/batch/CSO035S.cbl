00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 CSO035.                              
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 11/28/95 11:10:06.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.018.                          
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
00013 *DATE-COMPILED.                                                   
00014                                                                   
00015 *SECURITY.   *****************************************************
00016 *            *   THIS PROGRAM IS THE PROPERTY OF LOGIC, INC.     *
00017 *            *                                                   *
00018 *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
00019 *            *   OF LOGIC, INC. IS EXPRESSLY PROHIBITED WITHOUT  *
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.                                                         
00025 *    1.  PRINTS MONTHLY PRODUCTION REPORTS FOR LAST 12 MONTHS BY  
00026 *          ACCOUNT, STATE, GROUPING, CARRIER, WITH GRAND TOTALS.  
00027 *                                                                 
00028 *    2.  CREATES EXTRACT RECORDS FOR PRINTING BY ECS036 FOR       
00029 *            A.  OVERALL STATE REPORT                             
00030 *            B.  CARRIER AND GROUPING WITHIN BUSINESS TYPE        
00031 *            C.  CARRIER AND GROUPING WITHIN AGENCY (LEVEL 2-10)  
00032 *                (GA FOR NMC)                                     
00033 *            D.  OVERALL SPECIAL REPORT-CODE-1 REPORT             
00034 *            E.  SPECIAL REPORT-CODE-2 WITHIN CARRIER & GROUP     
00035 *                                                                 
CIDMOD EJECT                                                            
CIDMOD****************************************************************  
101504****************************************************************  
101504******************************************************************
101504*                   C H A N G E   L O G
101504*
101504* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
101504*-----------------------------------------------------------------
CIDMOD** CID MODS:                                                      
CIDMOD**                                                                
CIDMOD** 1. CR# 2000042100002 - DANA                                    
CIDMOD**    - USE LOGIC VERSION OF ELCPRT2 TO PRINT ALL PAGES           
CIDMOD**    - CHANGE DET-TMS-TCOM                                       
CIDMOD**                                                                
101504*-----------------------------------------------------------------
101504*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
101504* EFFECTIVE    NUMBER
101504*-----------------------------------------------------------------
101504* 101504    2003102100005  PEMA  ADD NEW FILE FOR REPORT CODE 1 RPT
102004* 102004    2003031400002  PEMA  ADD NEW STATUS CODE
122804* 122804    2004101500005  PEMA  ADD ITD AND PYTD AND PL12 TO 036
010306* 010306    2006010300002  PEMA  CORRECT PROBLEM WITH PYTD TOTALS
051810* 051810 CR2010042900001   PEMA  SPEARATE CITY AND STATE
031811* 031811 CR2011012700001   PEMA  ADD ACCT STATUS S - SUSPENDED
022312* 022312 CR2012020100003   AJRA  ADD AHL REPORT
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
021916* 021916  CR2014010900001  TANA  ADD ACCT STATUS D,L,R,P
083018* 083018  CR2018090400001  PEMA  ADD LDS REPORT
101504******************************************************************
00036  EJECT                                                            
00037  ENVIRONMENT DIVISION.                                            
00038  CONFIGURATION SECTION.                                           
00039                                                                   
00040  INPUT-OUTPUT SECTION.                                            
00041  FILE-CONTROL.                                                    
00042                                                                   
00043      SELECT AM-MAST-IN   ASSIGN TO SYS021-FBA1-ERACCTT            
00044                          ORGANIZATION  INDEXED                    
CIDMOD                         ACCESS        DYNAMIC                    
00046                          RECORD KEY    AM-CONTROL-PRIMARY         
00047                          FILE STATUS   ERACCT-FILE-STATUS.        
00048      SELECT EARNED-PREM  ASSIGN TO SYS011-UT-2400-S-SYS011.       
00049      SELECT EXTRACT-OT   ASSIGN TO SYS012-UT-FBA1-S-SYS012.       
00050      SELECT PRNT         ASSIGN TO SYS008-UR-1403-S-SYS008.       
00051      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       
00052      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       
101504     SELECT RPTCDE1      ASSIGN TO SYS021-UT-2400-S-SYS021.       
00053      SELECT ERMEBL       ASSIGN TO SYS024-FBA1-ERMEBL             
00054                          ORGANIZATION  INDEXED                    
00055                          ACCESS        DYNAMIC                    
00056                          RECORD KEY    ME-CONTROL-PRIMARY         
00057                          FILE STATUS   ERMEBL-FILE-STATUS.        
CIDMOD     SELECT DISPLAY-PRT  ASSIGN TO SYS022-UR-1403-S-SYS022.       
00058  EJECT                                                            
00059  DATA DIVISION.                                                   
00060  FILE SECTION.                                                    
00061                                                                   
00062  FD  AM-MAST-IN.                                                  
00063                                                                   
00064                                  COPY ERCACCT.                    
00065  EJECT                                                            
00066  FD  EARNED-PREM                                                  
00067                                  COPY ECSEPCFD.                   
00068                                                                   
00069                                  COPY ECSEPC01.                   
00070  EJECT                                                            
00071  FD  EXTRACT-OT                                                   
00072      BLOCK CONTAINS 0 RECORDS
00073      RECORDING MODE F.                                            
00074                                                                   
CIDMOD 01  EXTRACT-OT-REC              PIC X(196).                      
CIDMOD*01  EXTRACT-OT-REC              PIC X(166).                      
00076  EJECT                                                            
00077  FD  PRNT                                                         
00078                                  COPY ELCPRTFD.                   
00079  EJECT                                                            
00080  FD  DISK-DATE                                                    
00081                                  COPY ELCDTEFD.                   
00082  EJECT                                                            
101504 FD  RPTCDE1
101504     BLOCK CONTAINS 0 RECORDS
101504     RECORDING MODE F.

101504 01  RPTCDE1-RECORD              PIC X(171).

00083  FD  FICH                                                         
00084                                  COPY ELCFCHFD.                   
00085  EJECT                                                            
00086  FD  ERMEBL.                                                      
00087                                                                   
00088                                  COPY ERCMEBL.                    
CIDMOD FD  DISPLAY-PRT                                                  
CIDMOD     RECORDING MODE F                                             
CIDMOD     BLOCK CONTAINS 0 RECORDS.
CIDMOD                                                                  
CIDMOD 01  DISPLAY-REC.                                                 
CIDMOD     12  DISPLAY-CC              PIC X.                           
CIDMOD     12  DISPLAY-INFO            PIC X(131).                      
CIDMOD                                                                  
00089  EJECT                                                            
00090  WORKING-STORAGE SECTION.                                         
00091  77  FILLER  PIC X(32) VALUE '********************************'.  
00092  77  FILLER  PIC X(32) VALUE '     ECS035 WORKING STORAGE     '.  
00093  77  FILLER  PIC X(32) VALUE '********** VMOD=2.018 **********'.  
       77  WS-DISP-SW                  PIC X VALUE SPACES.
           88  DISP-REC                      VALUE 'Y'.
           88  NO-DISP-REC                   VALUE 'N'.
00094                                                                   
       77  WS-DISP-AMT                 PIC ZZZ,ZZZ,ZZZ.99 VALUE ZEROS.
       77  WS-DISP-DTE                 PIC 9(6).
00095  77  Z                           PIC 9(4)  COMP   VALUE 0.        
00096  77  Z1                          PIC 9(4)  COMP   VALUE 0.        
00097  77  CNT-IN                      PIC S9(9) COMP-3 VALUE ZEROS.    
00098  77  CNT-OT                      PIC S9(9) COMP-3 VALUE ZEROS.    
00099  77  AM-CNT-IN                   PIC  9(9)        VALUE ZEROS.    
00100  77  WS-ISS-FLAG                 PIC X            VALUE SPACE.    
CIDMOD 77  PRNT-TOT-SW                 PIC X            VALUE 'N'.      
00101  77  WS-ISS-CNT                  PIC S9(9)        VALUE ZEROS.    
00102  77  LINE-CNT                    PIC S999  COMP-3 VALUE ZEROS.    
00103  77  PAGE-CNT                    PIC S9(5) COMP-3 VALUE ZEROS.    
00104  77  PGM-SUB                     PIC S999  COMP   VALUE +035.     
00105  77  X                           PIC X.                           
00106  77  A1                          PIC S999  COMP   VALUE +000.     
00107  77  X1                          PIC S999  COMP   VALUE +000.     
       77  DISP-X1                     PIC 999          VALUE ZEROS.
00108  77  Y1                          PIC S999  COMP   VALUE +000.     
00109  77  SAVE-X1                     PIC S999  COMP   VALUE +000.     
00110  77  SET-CTR                     PIC S999  COMP-3 VALUE ZERO.     
00111 *77  DATE-RANGE-MAX              PIC 99           VALUE 13.       
122804 77  DATE-RANGE-MAX              PIC 99           VALUE 15.
00112  77  DATE-RANGE-MAX-1            PIC 99           VALUE 12.       
00113  77  CARRIER-L                   PIC X            VALUE LOW-VALUE.
00114  77  ERACCT-FILE-STATUS          PIC XX           VALUE ZERO.     
CIDMOD 77  SAVE-PRIMARY                PIC X(25)        VALUE SPACES.   
CIDMOD                                                                  
CIDMOD 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               
CIDMOD 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             
CIDMOD 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             
101504 77  SAVE-REPORT-CODE-1      PIC X(10)   VALUE SPACES.
101504 77  PRT-RPTCDE1-SW          PIC X       VALUE ' '.
           88  PRT-RPTCDE1                  VALUE 'Y'.
CIDMOD                                                                  
CIDMOD 01  DISPLAY-HD-1.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(23) VALUE 'PROCESSING ERROR REPORT'.   
CIDMOD     12  FILLER      PIC X(50) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(07) VALUE 'ECS-035'.                   
CIDMOD                                                                  
CIDMOD 01  DISPLAY-HD-2.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(56) VALUE SPACES.                      
CIDMOD     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(66) VALUE SPACES.                      
CIDMOD                                                                  
CIDMOD 01  DISPLAY-LINE.                                                
CIDMOD     05  DISPLAY-LINE-05.                                         
CIDMOD        10  DIS-CC              PIC X.                            
CIDMOD        10  DIS-LINE-REASON     PIC X(32).                        
CIDMOD        10  DIS-LINE-REC        PIC X(99).                        
CIDMOD                                                                  
CIDMOD     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            
CIDMOD        10  DIS-CC-ALT          PIC X.                            
CIDMOD        10  DIS-FLD-1           PIC X(52).                        
CIDMOD        10  DIS-FLD-2           PIC X(79).                        
CIDMOD                                                                  
101504 01  WS-RPTCDE1-RECORD.
           05  WS-RPTCDE1-RPTCDE1      PIC X(10).
           05  WS-RPTCDE1-ACT-KEY      PIC X(19).
           05  WS-RPTCDE1-LINE-NO      PIC 9(9)     VALUE ZEROS.
           05  WS-RPTCDE1-PRT.
               10  WS-RPTCDE1-CTL      PIC X.
               10  WS-RPTCDE1-DATA     PIC X(132).
101504     
00115  EJECT                                                            
00116  01  WS.                                                          
00117      12  WS-RETURN-CODE          PIC S9(4)       VALUE ZERO.      
00118      12  WS-ZERO                 PIC S9          VALUE ZERO.      
00119      12  WS-ABEND-MESSAGE        PIC X(80)       VALUE SPACES.    
00120      12  WS-ABEND-FILE-STATUS    PIC XX          VALUE ZERO.      
00121      12  WS-EE-DTE               PIC 9(07).                       
00122      12  WS-EE-DTE-R REDEFINES WS-EE-DTE.                         
00123          16  FILLER              PIC 9.                           
00124          16  EE-CCYY             PIC 9(4).                        
00125          16  EE-MO               PIC 99.                          
00126                                                                   
00127  01  MONTH-END-DATA.                                              
00128      12  ME-START-DATE.                                           
00129          16  ME-START-MO         PIC 99.                          
00130          16  FILLER              PIC X.                           
00131          16  ME-START-DA         PIC 99.                          
00132          16  FILLER              PIC X.                           
00133          16  ME-START-YR         PIC 99.                          
00134      12  ME-CNDS-DATE            PIC 9(6).                        
00135      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   
00136          16  ME-CNDS-MO          PIC 99.                          
00137          16  ME-CNDS-DA          PIC 99.                          
00138          16  ME-CNDS-YR          PIC 99.                          
00139      12  ME-START-TIME           PIC 9(6).                        
00140      12  ME-UPDATE-FLAG          PIC X          VALUE 'Y'.        
00141          88  ME-DO-UPDATE                       VALUE 'Y'.        
00142          88  ME-NO-UPDATE                       VALUE 'N'.        
00143      12  ERMEBL-FILE-STATUS      PIC XX.                          
00144      12  MONTH-END-MOYR          PIC S9(5) COMP-3.                
00145                                                                   
00146  01  W-HD-WORK-AREA.                                              
00147      12  W-ZEROS                 PIC S9(04) COMP VALUE +0.        
00148      12  W-WL-NDX                PIC S9(04) COMP.                 
00149      12  W-WA2-NDX               PIC S9(04) COMP.                 
00150      12  W-WORK-LINE.                                             
00151          16  FILLER              PIC X(10).                       
00152          16  W-WORK-AREA-2.                                       
00153              20  W-WA2-CHAR OCCURS 86 TIMES                       
00154                                  PIC X(01).                       
00155      12  W-WORKING-LINE.                                          
00156          16  W-WL-CHAR OCCURS 86 TIMES                            
00157                                  PIC X(01).                       
00158                                                                   
00159  01  QTR-SW                      PIC X      VALUE SPACE.          
00160      88  QTR-END                            VALUE '1'.            
00161                                                                   
00162  01  QTR-COMP                    PIC X      VALUE SPACE.          
00163      88  QTR-CO                             VALUE '1'.            
00164                                                                   
00165  01  B-E-AGT-TABLE.                                               
00166      05  B-E-AGT                 PIC X(10)  OCCURS 9 TIMES.       
00167                                                                   
00168  01  WS-EXTR-REC.                                                 
00169      05  EE-PASS-NO              PIC X.                           
00170      05  EE-CNTL.                                                 
00171          10  EE-CNTL-1.                                           
00172              15  EE-CNTL-GA      PIC X(10).                       
00173              15  EE-CNTL-ACCT    PIC X(10).                       
00174          10  EE-CNTL-2.                                           
00175              15  EE-CARR         PIC X.                           
00176              15  EE-GROUP        PIC X(6).                        
00177          10  FILLER              PIC X(12).                       
00178      05  EE-ALT-RPT-CNTL  REDEFINES  EE-CNTL.                     
00179          10  EE-A-RPT-CD-1       PIC X(10).                       
00180          10  EE-A-CARR           PIC X.                           
00181          10  EE-A-GROUP          PIC X(6).                        
00182          10  EE-A-RPT-CD-2       PIC X(10).                       
00183          10  EE-A-STATE          PIC XX.                          
00184          10  EE-A-ACCT           PIC X(10).                       
00185      05  EE-DTE                  PIC 9(07)   COMP-3.              
00186      05  EE-CNTRS    COMP-3.                                      
00187          10  EE-CERT             PIC S9(11)V99.                   
00188          10  EE-LBEN             PIC S9(11)V99.                   
00189          10  EE-LPRM             PIC S9(11)V99.                   
00190          10  EE-LCLM             PIC S9(11)V99.                   
00191          10  EE-ABEN             PIC S9(11)V99.                   
00192          10  EE-APRM             PIC S9(11)V99.                   
00193          10  EE-ACLM             PIC S9(11)V99.                   
00194          10  EE-TPRM             PIC S9(11)V99.                   
00195          10  EE-TCOM             PIC S9(11)V99.                   
00196      05  FILLER                  PIC X(3).                        
00197      05  EE-ISS-FLAG             PIC X.                           
00198      05  EE-ACCT-STATUS          PIC X.                           
00199      05  EE-ACCT-NAME            PIC X(30).                       
00200      05  EE-AM-EXPIRES           PIC 9(11) COMP-3.                
00201      05  EE-AM-HI-CERT           PIC 9(11) COMP-3.                
00202      05  EE-MTH-HI-CERT          PIC 9(11) COMP-3.                
00203      05  EE-ISS-CNT              PIC S9(9) COMP-3.                
00204      05  FILLER                  PIC X.                           
00199      05  EE-ACCT-CITY            PIC X(30).                       
00205                                                                   
00206  01  PRINT-SWITCHES.                                              
00207      05  P-ACC-SW                PIC X VALUE ' '.                 
00208      05  P-ST-SW                 PIC X VALUE ' '.                 
00209      05  P-GRP-SW                PIC X VALUE ' '.                 
00210      05  P-CA-SW                 PIC X VALUE ' '.                 
00211                                                                   
00212  01  RUN-DT.                                                      
00213      05  RUN-DT-CCYY             PIC 9(4).                        
00214      05  RUN-DT-MO               PIC 99.                          
00215                                                                   
00216  01  RUN9DT REDEFINES RUN-DT     PIC 9(6).                        
00217                                                                   
00218  01  WORK-ACCUM  COMP-3.                                          
00219      05  CERT            PIC S9(11)V99   VALUE ZEROS.             
00220      05  LBEN            PIC S9(12)V99   VALUE ZEROS.             
00221      05  LPRM            PIC S9(11)V99   VALUE ZEROS.             
00222      05  LCLM            PIC S9(12)V99   VALUE ZEROS.             
00223      05  ABEN            PIC S9(12)V99   VALUE ZEROS.             
00224      05  APRM            PIC S9(11)V99   VALUE ZEROS.             
00225      05  ACLM            PIC S9(12)V99   VALUE ZEROS.             
00226      05  TPRM            PIC S9(11)V99   VALUE ZEROS.             
00227      05  TCOM            PIC S9(11)V99   VALUE ZEROS.             
00228  EJECT                                                            
00229  01  HD-1-ACCOUNT.                                                
102004     05  FILLER          PIC X(10)   VALUE SPACES.
102004     05  HD-1-RPT-CD1    PIC X(10)   VALUE SPACES.
102004     05  FILLER          PIC X(25)   VALUE SPACES.
00230 *    05  FILLER          PIC X(45)   VALUE SPACES.                
00231      05  ACCT-PER-HD     PIC X(10)   VALUE '  MONTHLY '.          
00232      05  FILLER          PIC X(12)   VALUE 'ACCOUNT PROD'.        
00233      05  FILLER          PIC X(13)   VALUE 'UCTION REPORT'.       
00234      05  FILLER          PIC X(39)   VALUE SPACES.                
00235      05  FILLER          PIC X(6)    VALUE 'ECS035'.              
00236                                                                   
00237  01  HD-1-STATE.                                                  
00238      05  FILLER          PIC X(46)   VALUE SPACES.                
00239      05  STATE-PER-HD    PIC X(10)   VALUE '  MONTHLY '.          
00240      05  FILLER          PIC X(12)   VALUE 'STATE PRODUC'.        
00241      05  FILLER          PIC X(11)   VALUE 'TION REPORT'.         
00242      05  FILLER          PIC X(40)   VALUE SPACES.                
00243      05  FILLER          PIC X(6)    VALUE 'ECS035'.              
00244                                                                   
00245  01  HD-1-GROUPING.                                               
00246      05  FILLER          PIC X(45)   VALUE SPACES.                
00247      05  GRP-PER-HD      PIC X(10)   VALUE '  MONTHLY '.          
00248      05  FILLER          PIC X(13)   VALUE 'GROUPING PROD'.       
00249      05  FILLER          PIC X(13)   VALUE 'UCTION REPORT'.       
00250      05  FILLER          PIC X(38)   VALUE SPACES.                
00251      05  FILLER          PIC X(6)    VALUE 'ECS035'.              
00252                                                                   
00253  01  HD-1-CARRIER.                                                
00254      05  FILLER          PIC X(45)   VALUE SPACES.                
00255      05  CARR-PER-HD     PIC X(10)   VALUE '  MONTHLY '.          
00256      05  FILLER          PIC X(12)   VALUE 'CARRIER PROD'.        
00257      05  FILLER          PIC X(13)   VALUE 'UCTION REPORT'.       
00258 *    05  FILLER          PIC X(39)   VALUE SPACES.                
           05  FILLER          PIC X(5)    VALUE SPACES.
           05  FILLER          PIC X(34)   VALUE SPACES.
00259      05  FILLER          PIC X(6)    VALUE 'ECS035'.              
00260                                                                   
00261  01  HD-1-GRAND-TOTALS.                                           
00262      05  FILLER          PIC X(47)   VALUE SPACES.                
00263      05  FILLER          PIC X(20)   VALUE 'PRODUCTION REPORT GR'.
00264      05  FILLER          PIC X(10)   VALUE 'AND TOTALS'.          
00265      05  FILLER          PIC X(42)   VALUE SPACES.                
00266      05  FILLER          PIC X(6)    VALUE 'ECS035'.              
00267                                                                   
00268  01  HD-2.                                                        
00269      05  FILLER          PIC X(47)   VALUE SPACES.                
00270      05  HD-COMPANY-NAME PIC X(30)   VALUE SPACES.                
00271      05  FILLER          PIC X(08)   VALUE SPACES.                
           05  HD2-PARM        PIC X(34)   VALUE SPACES.
00272      05  HD-IPL-DATE     PIC X(8)    VALUE SPACES.                
00273                                                                   
00274  01  HD-3.                                                        
00275      05  FILLER          PIC X(53)   VALUE SPACES.                
00276      05  HD-ALPHA-DATE   PIC X(18)   VALUE SPACES.                
00277      05  FILLER          PIC X(48)   VALUE SPACES.                
00278      05  FILLER          PIC X(5)    VALUE 'PAGE '.               
00279      05  HD-PAGE         PIC ZZ,ZZ9.                              
00280                                                                   
00281  01  HD-4.                                                        
00282      05  HD4-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          
00283      05  FILLER          PIC X(06)   VALUE 'GROUP '.              
00284      05  HD-GROUPING     PIC X(6)    VALUE SPACES.                
00285                                                                   
00286  01  HD-5.                                                        
00287      05  HD5-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          
00288      05  FILLER          PIC X(08)   VALUE 'ACCOUNT '.            
00289      05  HD-ACCOUNT      PIC X(10)   VALUE SPACES.                
00290      05  FILLER          PIC X(2)    VALUE ' ('.                  
00291      05  HD-ACCT-NAME    PIC X(30)   VALUE SPACES.                
00292      05  FILLER          PIC X(4)    VALUE ' AT '.                
00293      05  HD-ACCT-ADDRESS PIC X(30)   VALUE SPACES.                
00294      05  FILLER          PIC X(1)    VALUE ')'.                   
00295                                                                   
00296  01  HD-6.                                                        
00297      05  HD6-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          
00298      05  FILLER          PIC X(06)   VALUE 'STATE '.              
00299      05  HD-STATE-ABBR   PIC X(02)   VALUE SPACES.                
00300      05  FILLER          PIC X(02)   VALUE ' ('.                  
00301      05  HD-STATE        PIC X(30)   VALUE SPACES.                
00302      05  FILLER          PIC X(1)    VALUE ')'.                   
00303                                                                   
00304  01  HD-7.                                                        
00305      05  HD7-TOTAL-FOR   PIC X(10)   VALUE 'TOTAL FOR '.          
00306      05  FILLER          PIC X(08)   VALUE 'CARRIER '.            
00307      05  HD-CARRIER      PIC X       VALUE SPACES.                
00308      05  FILLER          PIC X(2)    VALUE ' ('.                  
00309      05  HD-CARR-NAME    PIC X(30)   VALUE SPACES.                
00310      05  FILLER          PIC X(1)    VALUE ')'.                   
00311                                                                   
00312  01  HD-8.                                                        
00313      05  FILLER          PIC X(20)   VALUE 'GRAND TOTALS '.       
00314                                                                   
00315  01  HD-9.                                                        
00316      05  HD-9-1          PIC X(20)   VALUE '               NET  '.
00317      05  FILLER          PIC X(5)    VALUE SPACES.                
00318      05  HD-9-LF-OVRD-1  PIC X(6).                                
00319      05  FILLER          PIC X(8)    VALUE SPACES.                
00320      05  HD-9-LF-OVRD-2  PIC X(6).                                
00321      05  FILLER          PIC X(9)    VALUE SPACES.                
00322      05  HD-9-LF-OVRD-3  PIC X(6).                                
00323      05  FILLER          PIC X(8)    VALUE SPACES.                
00324      05  HD-9-AH-OVRD-1  PIC X(6).                                
00325      05  FILLER          PIC X(8)    VALUE SPACES.                
00326      05  HD-9-AH-OVRD-2  PIC X(6).                                
00327      05  FILLER          PIC X(9)    VALUE SPACES.                
00328      05  HD-9-AH-OVRD-3  PIC X(6).                                
00329      05  FILLER          PIC X(9)    VALUE SPACES.                
00330      05  FILLER          PIC X(20)   VALUE 'TOTAL        TOTAL  '.
00331                                                                   
00332  01  HD-TMS-9.                                                    
CIDMOD*    05  FILLER          PIC X(13)   VALUE SPACES.                
CIDMOD*    05  FILLER          PIC X(5)    VALUE 'NO OF'.               
CIDMOD     05  FILLER          PIC X(7)    VALUE SPACES.                
CIDMOD     05  FILLER          PIC X(12)   VALUE 'TOTAL ISSUED'.        
00335      05  FILLER          PIC X(4)    VALUE SPACES.                
00336      05  FILLER          PIC X(5)    VALUE 'NO OF'.               
00337      05  FILLER          PIC X(7)    VALUE SPACES.                
00338      05  FILLER          PIC X(6)    VALUE 'NO OF '.              
00339      05  FILLER          PIC X(9)    VALUE SPACES.                
00340      05  FILLER          PIC X(5)    VALUE 'GROSS'.               
00341      05  FILLER          PIC X(10)   VALUE SPACES.                
00342      05  FILLER          PIC X(5)    VALUE 'TOTAL'.               
00343      05  FILLER          PIC X(12)   VALUE SPACES.                
00344      05  FILLER          PIC X(3)    VALUE 'NET'.                 
00345      05  FILLER          PIC X(9)    VALUE SPACES.                
00346      05  FILLER          PIC X(5)    VALUE 'TOTAL'.               
00347      05  FILLER          PIC X(6)    VALUE SPACES.                
00348      05  FILLER          PIC X(12)   VALUE 'PREMIUM LESS'.        
00349      05  FILLER          PIC X(9)    VALUE SPACES.                
00350      05  FILLER          PIC X(5)    VALUE 'TOTAL'.               
00351                                                                   
00352  01  HD-TMS-10.                                                   
LGC136*    05  FILLER          PIC X(13)   VALUE SPACES.                
LGC136*    05  FILLER          PIC X(5)    VALUE 'CERTS'.               
LGC136     05  FILLER          PIC X(09)   VALUE SPACES.                
LGC136     05  FILLER          PIC X(09)   VALUE 'COVERAGES'.           
00353      05  FILLER          PIC X(4)    VALUE SPACES.                
00356      05  FILLER          PIC X(7)    VALUE 'CANCELS'.             
00357      05  FILLER          PIC X(4)    VALUE SPACES.                
00358      05  FILLER          PIC X(9)    VALUE 'COVERAGES'.           
00359      05  FILLER          PIC X(7)    VALUE SPACES.                
00360      05  FILLER          PIC X(7)    VALUE 'PREMIUM'.             
00361      05  FILLER          PIC X(8)    VALUE SPACES.                
00362      05  FILLER          PIC X(7)    VALUE 'REFUNDS'.             
00363      05  FILLER          PIC X(9)    VALUE SPACES.                
00364      05  FILLER          PIC X(7)    VALUE 'PREMIUM'.             
00365      05  FILLER          PIC X(4)    VALUE SPACES.                
00366      05  FILLER          PIC X(10)   VALUE 'COMMISSION'.          
00367      05  FILLER          PIC X(5)    VALUE SPACES.                
00368      05  FILLER          PIC X(10)   VALUE 'COMMISSION'.          
00369      05  FILLER          PIC X(10)   VALUE SPACES.                
00370      05  FILLER          PIC X(6)    VALUE 'CLAIMS'.              
00371                                                                   
00372  01  HD-10.                                                       
CIDMOD*    05  HD-10-1         PIC X(20)   VALUE '            ACTIVITY'.
CIDMOD     05  HD-10-1         PIC X(20)   VALUE '           COVERAGES'.
00374      05  FILLER          PIC X(20)   VALUE '    BENEFITS       P'.
00375      05  FILLER          PIC X(20)   VALUE 'REMIUM        CLAIMS'.
00376      05  FILLER          PIC X(20)   VALUE '       BENEFITS     '.
00377      05  FILLER          PIC X(20)   VALUE '  PREMIUM        CLA'.
00378      05  FILLER          PIC X(20)   VALUE 'IMS        PREMIUM  '.
00379      05  FILLER          PIC X(12)   VALUE '  COMMISSION'.        
00380                                                                   
00381  01  DETAIL-LINE.                                                 
00382      05  DET-TITLE.                                               
00383          10  DET-DATE.                                            
00384              15  DET-MO          PIC XXX     VALUE SPACES.        
00385              15  FILLER          PIC X       VALUE SPACES.        
00386              15  DET-YR          PIC XX      VALUE SPACES.        
00387              15  FILLER          PIC X       VALUE SPACES.        
00388          10  DET-CERTS           PIC ZZZ,ZZZ,Z99-.                
00389      05  DET-LBEN                PIC ZZZZZ,ZZZ,Z99-.              
00390      05  DET-LPRM                PIC ZZ,ZZZ,ZZ9.99-.              
00391      05  DET-LCLM                PIC ZZ,ZZZ,ZZ9.99-.              
00392      05  DET-ABEN                PIC ZZZ,ZZZ,ZZ9.99-.             
00393      05  DET-APRM                PIC ZZ,ZZZ,ZZ9.99-.              
00394      05  DET-ACLM                PIC ZZ,ZZZ,ZZ9.99-.              
00395      05  DET-TPRM                PIC ZZZ,ZZZ,ZZ9.99-.             
00396      05  DET-TCOM                PIC ZZZZZ,ZZ9.99-.               
00397                                                                   
00398  01  DETAIL-TMS-LINE-1.                                           
00399      05  DET-TMS-TITLE.                                           
00400          10  DET-TMS-DATE.                                        
00401              15  DET-TMS-MO      PIC XXX     VALUE SPACES.        
00402              15  FILLER          PIC X       VALUE SPACES.        
00403              15  DET-TMS-YR      PIC XX      VALUE SPACES.        
00404              15  FILLER          PIC X       VALUE SPACES.        
00405          10  DET-TMS-CERTS       PIC ZZ,ZZZ,Z99-.                 
00406          10  DET-TMS-CANCELS     PIC Z,ZZZ,Z99-.                  
00407          10  FILLER              PIC X.                           
00408          10  DET-TMS-LDESC       PIC XX.                          
00409          10  FILLER              PIC X.                           
00410      05  DET-TMS-LCOVERAG        PIC Z,ZZZ,Z99-.                  
00411      05  DET-TMS-LPRM            PIC ZZ,ZZZ,ZZ9.99-.              
00412      05  FILLER                  PIC X.                           
00413      05  DET-TMS-LCAN            PIC ZZ,ZZZ,ZZ9.99-.              
00414      05  FILLER                  PIC X.                           
00415      05  DET-TMS-TPRM            PIC ZZZ,ZZZ,ZZ9.99-.             
CIDMOD**   05  FILLER                  PIC X.                           
CIDMOD**   05  DET-TMS-TCOM            PIC ZZZZZ,ZZ9.99-.               
CIDMOD     05  DET-TMS-TCOM            PIC ZZ,ZZZ,ZZ9.99-.              
00418      05  FILLER                  PIC X.                           
00419      05  DET-TMS-NPRM            PIC ZZZ,ZZZ,ZZ9.99-.             
00420      05  FILLER                  PIC X.                           
00421      05  DET-TMS-LCLM            PIC ZZ,ZZZ,ZZ9.99-.              
00422                                                                   
00423  01  DETAIL-TMS-LINE-2.                                           
00424      05  FILLER                  PIC X(29).                       
00425      05  DET-TMS-ADESC           PIC XX.                          
00426      05  FILLER                  PIC X.                           
00427      05  DET-TMS-ACOVERAG        PIC Z,ZZZ,Z99-.                  
00428      05  DET-TMS-APRM            PIC ZZ,ZZZ,ZZ9.99-.              
00429      05  FILLER                  PIC X.                           
00430      05  DET-TMS-ACAN            PIC ZZ,ZZZ,ZZ9.99-.              
00431      05  FILLER                  PIC X(47).                       
00432      05  DET-TMS-ACLM            PIC ZZ,ZZZ,ZZ9.99-.              
00433                                                                   
00434  01  DETAIL-TMS-LINE-3.                                           
00435      05  FILLER                  PIC X(131)      VALUE ALL '-'.   
00436                                                                   
00437  01  DET-DATE-LINE.                                               
00438      05  DET-DATE-DESC           PIC X(17)       VALUE SPACES.    
00439      05  DET-DATE-MO             PIC XX.                          
00440      05  DET-DATE-SLASH-1        PIC X           VALUE '/'.       
00441      05  DET-DATE-DA             PIC XX.                          
00442      05  DET-DATE-SLASH-2        PIC X           VALUE '/'.       
00443      05  DET-DATE-YR             PIC XX.                          
00444      05  FILLER                  PIC X(107)      VALUE SPACES.    
00445  EJECT                                                            
00446  01  ACCOUNT-ACCUMULATORS.                                        
00447      05  ACCOUNT-ACCUM           OCCURS 15 TIMES.                 
00448          10  AC-CERT             PIC S9(9)        COMP-3.         
00449          10  AC-LBEN             PIC S9(12)V99    COMP-3.         
00450          10  AC-LPRM             PIC S9(11)V99    COMP-3.         
00451          10  AC-LCLM             PIC S9(12)V99    COMP-3.         
00452          10  AC-ABEN             PIC S9(11)V99    COMP-3.         
00453          10  AC-APRM             PIC S9(11)V99    COMP-3.         
00454          10  AC-ACLM             PIC S9(11)V99    COMP-3.         
00455          10  AC-TPRM             PIC S9(11)V99    COMP-3.         
00456          10  AC-TCOM             PIC S9(11)V99    COMP-3.         
00457          10  AC-DATE             PIC  9(11)       COMP-3.         
00458      05  AC-DATE-R               OCCURS 15 TIMES.                 
00459          10  FILLER          PIC 999.                             
00460          10  AC-DATE-CC      PIC 99.                              
00461          10  AC-DATE-YR      PIC 99.                              
00462          10  AC-DATE-MO      PIC 99.                              
00463          10  AC-DATE-DA      PIC 99.                              
00464      05  ZERO-ACCOUNT-ACCUM.                                      
00465          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00466          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. 
00467          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00468          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. 
00469          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00470          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00471          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00472          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00473          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00474          10  FILLER              PIC  9(11)     COMP-3  VALUE  0. 
00475                                                                   
00476  01  DATE-RANGE-ACCUMULATORS.                                     
00477      05  DATE-RANGE-ACCUM        OCCURS 15 TIMES.                 
00478          10  DR-CERT             PIC S9(9)        COMP-3.         
00479          10  DR-ISS              PIC S9(9)        COMP-3.         
00480          10  DR-LBEN             PIC S9(12)V99    COMP-3.         
00481          10  DR-LPRM             PIC S9(11)V99    COMP-3.         
00482          10  DR-LCLM             PIC S9(12)V99    COMP-3.         
00483          10  DR-ABEN             PIC S9(11)V99    COMP-3.         
00484          10  DR-APRM             PIC S9(11)V99    COMP-3.         
00485          10  DR-ACLM             PIC S9(11)V99    COMP-3.         
00486          10  DR-TPRM             PIC S9(11)V99    COMP-3.         
00487          10  DR-TCOM             PIC S9(11)V99    COMP-3.         
00488          10  DR-DATE             PIC  9(11)       COMP-3.         
00489      05  ZERO-DATE-RANGE-ACCUM.                                   
00490          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00491          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00492          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. 
00493          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00494          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. 
00495          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00496          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00497          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00498          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00499          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00500          10  FILLER              PIC 9(11)      COMP-3  VALUE  0. 
00501                                                                   
00502  01  ACCUMULATORS COMP-3.                                         
00503      05  STATE-ACCUM             OCCURS 15 TIMES.                 
00504          10  ST-CERT             PIC S9(9).                       
00505          10  ST-LBEN             PIC S9(12)V99.                   
00506          10  ST-LPRM             PIC S9(11)V99.                   
00507          10  ST-LCLM             PIC S9(12)V99.                   
00508          10  ST-ABEN             PIC S9(11)V99.                   
00509          10  ST-APRM             PIC S9(11)V99.                   
00510          10  ST-ACLM             PIC S9(11)V99.                   
00511          10  ST-TPRM             PIC S9(11)V99.                   
00512          10  ST-TCOM             PIC S9(11)V99.                   
00513      05  GROUPING-ACCUM          OCCURS 15 TIMES.                 
00514          10  GP-CERT             PIC S9(9).                       
00515          10  GP-LBEN             PIC S9(12)V99.                   
00516          10  GP-LPRM             PIC S9(11)V99.                   
00517          10  GP-LCLM             PIC S9(12)V99.                   
00518          10  GP-ABEN             PIC S9(11)V99.                   
00519          10  GP-APRM             PIC S9(11)V99.                   
00520          10  GP-ACLM             PIC S9(11)V99.                   
00521          10  GP-TPRM             PIC S9(11)V99.                   
00522          10  GP-TCOM             PIC S9(11)V99.                   
00523      05  CARRIER-ACCUM   OCCURS 15 TIMES.                         
00524          10  CA-CERT             PIC S9(9).                       
00525          10  CA-LBEN             PIC S9(12)V99.                   
00526          10  CA-LPRM             PIC S9(11)V99.                   
00527          10  CA-LCLM             PIC S9(12)V99.                   
00528          10  CA-ABEN             PIC S9(11)V99.                   
00529          10  CA-APRM             PIC S9(11)V99.                   
00530          10  CA-ACLM             PIC S9(11)V99.                   
00531          10  CA-TPRM             PIC S9(11)V99.                   
00532          10  CA-TCOM             PIC S9(11)V99.                   
00533      05  GRAND-TOTALS.                                            
00534          10  GR-CERT             PIC S9(9)      VALUE ZEROS.      
00535          10  GR-LBEN             PIC S9(12)V99  VALUE ZEROS.      
00536          10  GR-LPRM             PIC S9(11)V99  VALUE ZEROS.      
00537          10  GR-LCLM             PIC S9(12)V99  VALUE ZEROS.      
00538          10  GR-ABEN             PIC S9(11)V99  VALUE ZEROS.      
00539          10  GR-APRM             PIC S9(11)V99  VALUE ZEROS.      
00540          10  GR-ACLM             PIC S9(11)V99  VALUE ZEROS.      
00541          10  GR-TPRM             PIC S9(11)V99  VALUE ZEROS.      
00542          10  GR-TCOM             PIC S9(11)V99  VALUE ZEROS.      
00543      05  GRAND-ACCUM     OCCURS 15 TIMES.                         
00544          10  GT-CERT             PIC S9(9).                       
00545          10  GT-LBEN             PIC S9(12)V99.                   
00546          10  GT-LPRM             PIC S9(11)V99.                   
00547          10  GT-LCLM             PIC S9(12)V99.                   
00548          10  GT-ABEN             PIC S9(11)V99.                   
00549          10  GT-APRM             PIC S9(11)V99.                   
00550          10  GT-ACLM             PIC S9(11)V99.                   
00551          10  GT-TPRM             PIC S9(11)V99.                   
00552          10  GT-TCOM             PIC S9(11)V99.                   
00553      05  GRAND-TOTALS-LAST-12-MO.                                 
00554          10  L12GD-CERT          PIC S9(9)        VALUE ZEROS.    
00555          10  L12GD-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00556          10  L12GD-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00557          10  L12GD-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00558          10  L12GD-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00559          10  L12GD-APRM          PIC S9(11)V99    VALUE ZEROS.    
00560          10  L12GD-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00561          10  L12GD-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00562          10  L12GD-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00563      05  GRAND-T-Y-T-D.                                           
00564          10  YTDGD-CERT          PIC S9(9)        VALUE ZEROS.    
00565          10  YTDGD-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00566          10  YTDGD-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00567          10  YTDGD-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00568          10  YTDGD-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00569          10  YTDGD-APRM          PIC S9(11)V99    VALUE ZEROS.    
00570          10  YTDGD-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00571          10  YTDGD-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00572          10  YTDGD-TCOM          PIC S9(11)V99    VALUE ZEROS.    
122804     05  GRAND-T-PY-T-D.                                           
122804         10  PYTDGD-CERT          PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGD-LBEN          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDGD-LPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-LCLM          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDGD-ABEN          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-APRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-ACLM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-TPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00573      05  LAST-12-ACCOUNT-ACCUM.                                   
00574          10  L12AC-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00575          10  L12AC-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00576          10  L12AC-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00577          10  L12AC-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00578          10  L12AC-APRM          PIC S9(11)V99    VALUE ZEROS.    
00579          10  L12AC-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00580          10  L12AC-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00581          10  L12AC-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00582      05  LAST-12-STATE-ACCUM.                                     
00583          10  L12ST-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00584          10  L12ST-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00585          10  L12ST-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00586          10  L12ST-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00587          10  L12ST-APRM          PIC S9(11)V99    VALUE ZEROS.    
00588          10  L12ST-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00589          10  L12ST-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00590          10  L12ST-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00591      05  LAST-12-GROUPING-ACCUM.                                  
00592          10  L12GP-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00593          10  L12GP-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00594          10  L12GP-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00595          10  L12GP-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00596          10  L12GP-APRM          PIC S9(11)V99    VALUE ZEROS.    
00597          10  L12GP-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00598          10  L12GP-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00599          10  L12GP-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00600      05  LAST-12-CARRIER-ACCUM.                                   
00601          10  L12CA-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00602          10  L12CA-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00603          10  L12CA-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00604          10  L12CA-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00605          10  L12CA-APRM          PIC S9(11)V99    VALUE ZEROS.    
00606          10  L12CA-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00607          10  L12CA-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00608          10  L12CA-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00609      05  YTD-ACCOUNT-ACCUM.                                       
00610          10  YTDAC-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00611          10  YTDAC-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00612          10  YTDAC-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00613          10  YTDAC-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00614          10  YTDAC-APRM          PIC S9(11)V99    VALUE ZEROS.    
00615          10  YTDAC-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00616          10  YTDAC-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00617          10  YTDAC-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00618      05  YTD-STATE-ACCUM.                                         
00619          10  YTDST-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00620          10  YTDST-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00621          10  YTDST-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00622          10  YTDST-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00623          10  YTDST-APRM          PIC S9(11)V99    VALUE ZEROS.    
00624          10  YTDST-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00625          10  YTDST-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00626          10  YTDST-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00627      05  YTD-GROUPING-ACCUM.                                      
00628          10  YTDGP-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00629          10  YTDGP-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00630          10  YTDGP-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00631          10  YTDGP-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00632          10  YTDGP-APRM          PIC S9(11)V99    VALUE ZEROS.    
00633          10  YTDGP-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00634          10  YTDGP-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00635          10  YTDGP-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00636      05  YTD-CARRIER-ACCUM.                                       
00637          10  YTDCA-LBEN          PIC S9(12)V99    VALUE ZEROS.    
00638          10  YTDCA-LPRM          PIC S9(11)V99    VALUE ZEROS.    
00639          10  YTDCA-LCLM          PIC S9(12)V99    VALUE ZEROS.    
00640          10  YTDCA-ABEN          PIC S9(11)V99    VALUE ZEROS.    
00641          10  YTDCA-APRM          PIC S9(11)V99    VALUE ZEROS.    
00642          10  YTDCA-ACLM          PIC S9(11)V99    VALUE ZEROS.    
00643          10  YTDCA-TPRM          PIC S9(11)V99    VALUE ZEROS.    
00644          10  YTDCA-TCOM          PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-ACCOUNT-ACCUM.                                       
122804         10  PYTDAC-LBEN          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDAC-LPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-LCLM          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDAC-ABEN          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-APRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-ACLM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-TPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-TCOM          PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-STATE-ACCUM.                                         
122804         10  PYTDST-LBEN          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDST-LPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-LCLM          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDST-ABEN          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-APRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-ACLM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-TPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-TCOM          PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-GROUPING-ACCUM.                                      
122804         10  PYTDGP-LBEN          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDGP-LPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-LCLM          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDGP-ABEN          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-APRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-ACLM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-TPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-TCOM          PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-CARRIER-ACCUM.                                       
122804         10  PYTDCA-LBEN          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDCA-LPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-LCLM          PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDCA-ABEN          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-APRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-ACLM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-TPRM          PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-TCOM          PIC S9(11)V99    VALUE ZEROS.    
00645      05  ZERO-OCCURS-ACCUM.                                       
00646          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00647          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    
00648          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00649          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    
00650          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00651          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00652          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00653          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00654          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00655      05  ZERO-ACCUM.                                              
00656          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    
00657          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00658          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    
00659          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00660          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00661          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00662          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00663          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00664  EJECT                                                            
00665  01  PRINT-DECISION-TABLE  COMP-3.                                
00666      05  PRINT-TABLE OCCURS 12 TIMES.                             
00667          10  PLBEN               PIC S9(12)V99.                   
00668          10  PLPRM               PIC S9(11)V99.                   
00669          10  PLCLM               PIC S9(12)V99.                   
00670          10  PABEN               PIC S9(11)V99.                   
00671          10  PAPRM               PIC S9(11)V99.                   
00672          10  PACLM               PIC S9(11)V99.                   
00673          10  PTCOM               PIC S9(11)V99.                   
00674                                                                   
00675  01  PRINT-ZERO-TABLE  COMP-3.                                    
00676      05  FILLER                  PIC S9(12)V99    VALUE ZEROS.    
00677      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00678      05  FILLER                  PIC S9(12)V99    VALUE ZEROS.    
00679      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00680      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00681      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00682      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00683  EJECT                                                            
00684 **************************************************************    
00685 * THE FOLLOWING TABLES WERE ADDED FEBRUARY 1990 TO GENERATE  *    
00686 * A SPECIAL REPORT SHOWING GROSS TOTALS  VS NET TOTALS       *    
00687 * PER CLIENT REQUEST                                         *    
00688 **************************************************************    
00689  01  WORK-ACCUM-T  COMP-3.                                        
00690      05  CERT-T          PIC S9(9)       VALUE ZEROS.             
00691      05  CANCEL-T        PIC S9(9)       VALUE ZEROS.             
00692      05  L-COVERAGE-T    PIC S9(9)       VALUE ZEROS.             
00693      05  A-COVERAGE-T    PIC S9(9)       VALUE ZEROS.             
00694      05  LPRM-T          PIC S9(11)V99   VALUE ZEROS.             
00695      05  LCAN-T          PIC S9(11)V99   VALUE ZEROS.             
00696      05  LCLM-T          PIC S9(12)V99   VALUE ZEROS.             
00697      05  APRM-T          PIC S9(11)V99   VALUE ZEROS.             
00698      05  ACAN-T          PIC S9(11)V99   VALUE ZEROS.             
00699      05  ACLM-T          PIC S9(12)V99   VALUE ZEROS.             
00700      05  TPRM-T          PIC S9(11)V99   VALUE ZEROS.             
00701      05  TCOM-T          PIC S9(11)V99   VALUE ZEROS.             
00702  EJECT                                                            
00703  01  ACCOUNT-ACCUMULATORS-T.                                      
00704      05  ACCOUNT-ACCUM-T         OCCURS 15 TIMES.                 
00705          10  AC-CERT-T           PIC S9(9)        COMP-3.         
00706          10  AC-CANCEL-T         PIC S9(9)        COMP-3.         
00707          10  AC-L-COVERAGE-T     PIC S9(9)        COMP-3.         
00708          10  AC-A-COVERAGE-T     PIC S9(9)        COMP-3.         
00709          10  AC-LPRM-T           PIC S9(11)V99    COMP-3.         
00710          10  AC-LCAN-T           PIC S9(11)V99    COMP-3.         
00711          10  AC-LCLM-T           PIC S9(12)V99    COMP-3.         
00712          10  AC-APRM-T           PIC S9(11)V99    COMP-3.         
00713          10  AC-ACAN-T           PIC S9(11)V99    COMP-3.         
00714          10  AC-ACLM-T           PIC S9(11)V99    COMP-3.         
00715          10  AC-TPRM-T           PIC S9(11)V99    COMP-3.         
00716          10  AC-TCOM-T           PIC S9(11)V99    COMP-3.         
00717          10  AC-DATE-T           PIC  9(11)       COMP-3.         
00718      05  ZERO-ACCOUNT-ACCUM-T.                                    
00719          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00720          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00721          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00722          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00723          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00724          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00725          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. 
00726          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00727          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00728          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00729          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00730          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00731          10  FILLER              PIC  9(11)     COMP-3  VALUE  0. 
00732                                                                   
00733  01  DATE-RANGE-ACCUMULATORS-T.                                   
00734      05  DATE-RANGE-ACCUM-T      OCCURS 15 TIMES.                 
00735          10  DR-CERT-T           PIC S9(9)        COMP-3.         
00736          10  DR-CANCEL-T         PIC S9(9)        COMP-3.         
00737          10  DR-L-COVERAGE-T     PIC S9(9)        COMP-3.         
00738          10  DR-A-COVERAGE-T     PIC S9(9)        COMP-3.         
00739          10  DR-LPRM-T           PIC S9(11)V99    COMP-3.         
00740          10  DR-LCAN-T           PIC S9(11)V99    COMP-3.         
00741          10  DR-LCLM-T           PIC S9(12)V99    COMP-3.         
00742          10  DR-APRM-T           PIC S9(11)V99    COMP-3.         
00743          10  DR-ACAN-T           PIC S9(11)V99    COMP-3.         
00744          10  DR-ACLM-T           PIC S9(11)V99    COMP-3.         
00745          10  DR-TPRM-T           PIC S9(11)V99    COMP-3.         
00746          10  DR-TCOM-T           PIC S9(11)V99    COMP-3.         
00747          10  DR-DATE-T           PIC 9(11)        COMP-3.         
00748      05  ZERO-DATE-RANGE-ACCUM-T.                                 
00749          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00750          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00751          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00752          10  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00753          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00754          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00755          10  FILLER              PIC S9(12)V99  COMP-3  VALUE +0. 
00756          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00757          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00758          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00759          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00760          10  FILLER              PIC S9(11)V99  COMP-3  VALUE +0. 
00761          10  FILLER              PIC  9(11)     COMP-3  VALUE  0. 
00762                                                                   
00763  01  ACCUMULATORS-T COMP-3.                                       
00764      05  STATE-ACCUM-T           OCCURS 15 TIMES.                 
00765          10  ST-CERT-T           PIC S9(9).                       
00766          10  ST-CANCEL-T         PIC S9(9).                       
00767          10  ST-L-COVERAGE-T     PIC S9(9).                       
00768          10  ST-A-COVERAGE-T     PIC S9(9).                       
00769          10  ST-LPRM-T           PIC S9(11)V99.                   
00770          10  ST-LCAN-T           PIC S9(11)V99.                   
00771          10  ST-LCLM-T           PIC S9(12)V99.                   
00772          10  ST-APRM-T           PIC S9(11)V99.                   
00773          10  ST-ACAN-T           PIC S9(11)V99.                   
00774          10  ST-ACLM-T           PIC S9(11)V99.                   
00775          10  ST-TPRM-T           PIC S9(11)V99.                   
00776          10  ST-TCOM-T           PIC S9(11)V99.                   
00777      05  GROUPING-ACCUM-T        OCCURS 15 TIMES.                 
00778          10  GP-CERT-T           PIC S9(9).                       
00779          10  GP-CANCEL-T         PIC S9(9).                       
00780          10  GP-L-COVERAGE-T     PIC S9(9).                       
00781          10  GP-A-COVERAGE-T     PIC S9(9).                       
00782          10  GP-LPRM-T           PIC S9(11)V99.                   
00783          10  GP-LCAN-T           PIC S9(11)V99.                   
00784          10  GP-LCLM-T           PIC S9(12)V99.                   
00785          10  GP-APRM-T           PIC S9(11)V99.                   
00786          10  GP-ACAN-T           PIC S9(11)V99.                   
00787          10  GP-ACLM-T           PIC S9(11)V99.                   
00788          10  GP-TPRM-T           PIC S9(11)V99.                   
00789          10  GP-TCOM-T           PIC S9(11)V99.                   
00790      05  CARRIER-ACCUM-T  OCCURS 15 TIMES.                        
00791          10  CA-CERT-T           PIC S9(9).                       
00792          10  CA-CANCEL-T         PIC S9(9).                       
00793          10  CA-L-COVERAGE-T     PIC S9(9).                       
00794          10  CA-A-COVERAGE-T     PIC S9(9).                       
00795          10  CA-LPRM-T           PIC S9(11)V99.                   
00796          10  CA-LCAN-T           PIC S9(11)V99.                   
00797          10  CA-LCLM-T           PIC S9(12)V99.                   
00798          10  CA-APRM-T           PIC S9(11)V99.                   
00799          10  CA-ACAN-T           PIC S9(11)V99.                   
00800          10  CA-ACLM-T           PIC S9(11)V99.                   
00801          10  CA-TPRM-T           PIC S9(11)V99.                   
00802          10  CA-TCOM-T           PIC S9(11)V99.                   
00803      05  GRAND-TOTALS-T.                                          
00804          10  GR-CERT-T           PIC S9(9)      VALUE ZEROS.      
00805          10  GR-CANCEL-T         PIC S9(9)      VALUE ZEROS.      
00806          10  GR-L-COVERAGE-T     PIC S9(9)      VALUE ZEROS.      
00807          10  GR-A-COVERAGE-T     PIC S9(9)      VALUE ZEROS.      
00808          10  GR-LPRM-T           PIC S9(11)V99  VALUE ZEROS.      
00809          10  GR-LCAN-T           PIC S9(11)V99  VALUE ZEROS.      
00810          10  GR-LCLM-T           PIC S9(12)V99  VALUE ZEROS.      
00811          10  GR-APRM-T           PIC S9(11)V99  VALUE ZEROS.      
00812          10  GR-ACAN-T           PIC S9(11)V99  VALUE ZEROS.      
00813          10  GR-ACLM-T           PIC S9(11)V99  VALUE ZEROS.      
00814          10  GR-TPRM-T           PIC S9(11)V99  VALUE ZEROS.      
00815          10  GR-TCOM-T           PIC S9(11)V99  VALUE ZEROS.      
00816      05  GRAND-ACCUM-T   OCCURS 15 TIMES.                         
00817          10  GT-CERT-T           PIC S9(9).                       
00818          10  GT-CANCEL-T         PIC S9(9).                       
00819          10  GT-L-COVERAGE-T     PIC S9(9).                       
00820          10  GT-A-COVERAGE-T     PIC S9(9).                       
00821          10  GT-LPRM-T           PIC S9(11)V99.                   
00822          10  GT-LCAN-T           PIC S9(11)V99.                   
00823          10  GT-LCLM-T           PIC S9(12)V99.                   
00824          10  GT-APRM-T           PIC S9(11)V99.                   
00825          10  GT-ACAN-T           PIC S9(11)V99.                   
00826          10  GT-ACLM-T           PIC S9(11)V99.                   
00827          10  GT-TPRM-T           PIC S9(11)V99.                   
00828          10  GT-TCOM-T           PIC S9(11)V99.                   
00829      05  GRAND-TOTALS-LAST-12-MO-T.                               
00830          10  L12GD-CERT-T        PIC S9(9)        VALUE ZEROS.    
00831          10  L12GD-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00832          10  L12GD-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00833          10  L12GD-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00834          10  L12GD-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00835          10  L12GD-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00836          10  L12GD-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00837          10  L12GD-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00838          10  L12GD-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00839          10  L12GD-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00840          10  L12GD-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00841          10  L12GD-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00842      05  GRAND-T-Y-T-D-T.                                         
00843          10  YTDGD-CERT-T        PIC S9(9)        VALUE ZEROS.    
00844          10  YTDGD-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00845          10  YTDGD-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00846          10  YTDGD-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00847          10  YTDGD-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00848          10  YTDGD-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00849          10  YTDGD-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00850          10  YTDGD-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00851          10  YTDGD-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00852          10  YTDGD-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00853          10  YTDGD-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00854          10  YTDGD-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
122804     05  GRAND-T-PY-T-D-T.                                         
122804         10  PYTDGD-CERT-T        PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGD-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGD-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGD-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGD-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDGD-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGD-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00855      05  LAST-12-ACCOUNT-ACCUM-T.                                 
00856          10  L12AC-CERT-T        PIC S9(9)        VALUE ZEROS.    
00857          10  L12AC-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00858          10  L12AC-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00859          10  L12AC-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00860          10  L12AC-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00861          10  L12AC-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00862          10  L12AC-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00863          10  L12AC-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00864          10  L12AC-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00865          10  L12AC-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00866          10  L12AC-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00867          10  L12AC-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00868      05  LAST-12-STATE-ACCUM-T.                                   
00869          10  L12ST-CERT-T        PIC S9(9)        VALUE ZEROS.    
00870          10  L12ST-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00871          10  L12ST-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00872          10  L12ST-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00873          10  L12ST-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00874          10  L12ST-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00875          10  L12ST-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00876          10  L12ST-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00877          10  L12ST-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00878          10  L12ST-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00879          10  L12ST-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00880          10  L12ST-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00881      05  LAST-12-GROUPING-ACCUM-T.                                
00882          10  L12GP-CERT-T        PIC S9(9)        VALUE ZEROS.    
00883          10  L12GP-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00884          10  L12GP-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00885          10  L12GP-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00886          10  L12GP-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00887          10  L12GP-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00888          10  L12GP-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00889          10  L12GP-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00890          10  L12GP-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00891          10  L12GP-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00892          10  L12GP-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00893          10  L12GP-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00894      05  LAST-12-CARRIER-ACCUM-T.                                 
00895          10  L12CA-CERT-T        PIC S9(9)        VALUE ZEROS.    
00896          10  L12CA-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00897          10  L12CA-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00898          10  L12CA-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00899          10  L12CA-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00900          10  L12CA-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00901          10  L12CA-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00902          10  L12CA-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00903          10  L12CA-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00904          10  L12CA-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00905          10  L12CA-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00906          10  L12CA-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00907      05  YTD-ACCOUNT-ACCUM-T.                                     
00908          10  YTDAC-CERT-T        PIC S9(9)        VALUE ZEROS.    
00909          10  YTDAC-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00910          10  YTDAC-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00911          10  YTDAC-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00912          10  YTDAC-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00913          10  YTDAC-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00914          10  YTDAC-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00915          10  YTDAC-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00916          10  YTDAC-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00917          10  YTDAC-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00918          10  YTDAC-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00919          10  YTDAC-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00920      05  YTD-STATE-ACCUM-T.                                       
00921          10  YTDST-CERT-T        PIC S9(9)        VALUE ZEROS.    
00922          10  YTDST-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00923          10  YTDST-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00924          10  YTDST-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00925          10  YTDST-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00926          10  YTDST-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00927          10  YTDST-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00928          10  YTDST-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00929          10  YTDST-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00930          10  YTDST-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00931          10  YTDST-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00932          10  YTDST-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00933      05  YTD-GROUPING-ACCUM-T.                                    
00934          10  YTDGP-CERT-T        PIC S9(9)        VALUE ZEROS.    
00935          10  YTDGP-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00936          10  YTDGP-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00937          10  YTDGP-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00938          10  YTDGP-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00939          10  YTDGP-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00940          10  YTDGP-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00941          10  YTDGP-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00942          10  YTDGP-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00943          10  YTDGP-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00944          10  YTDGP-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00945          10  YTDGP-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00946      05  YTD-CARRIER-ACCUM-T.                                     
00947          10  YTDCA-CERT-T        PIC S9(9)        VALUE ZEROS.    
00948          10  YTDCA-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
00949          10  YTDCA-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00950          10  YTDCA-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
00951          10  YTDCA-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00952          10  YTDCA-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
00953          10  YTDCA-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
00954          10  YTDCA-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
00955          10  YTDCA-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
00956          10  YTDCA-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
00957          10  YTDCA-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
00958          10  YTDCA-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-ACCOUNT-ACCUM-T.                                     
122804         10  PYTDAC-CERT-T        PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDAC-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDAC-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDAC-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDAC-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDAC-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDAC-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-STATE-ACCUM-T.                                       
122804         10  PYTDST-CERT-T        PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDST-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDST-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDST-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDST-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDST-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDST-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-GROUPING-ACCUM-T.                                    
122804         10  PYTDGP-CERT-T        PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGP-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGP-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGP-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDGP-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDGP-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDGP-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
122804     05  PYTD-CARRIER-ACCUM-T.                                     
122804         10  PYTDCA-CERT-T        PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDCA-CANCEL-T      PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDCA-L-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDCA-A-COVERAGE-T  PIC S9(9)        VALUE ZEROS.    
122804         10  PYTDCA-LPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-LCAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-LCLM-T        PIC S9(12)V99    VALUE ZEROS.    
122804         10  PYTDCA-APRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-ACAN-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-ACLM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-TPRM-T        PIC S9(11)V99    VALUE ZEROS.    
122804         10  PYTDCA-TCOM-T        PIC S9(11)V99    VALUE ZEROS.    
00959      05  ZERO-OCCURS-ACCUM-T.                                     
00960          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00961          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00962          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00963          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00964          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00965          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00966          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    
00967          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00968          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00969          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00970          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00971          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00972      05  ZERO-ACCUM-T.                                            
00973          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00974          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00975          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00976          10  FILLER              PIC S9(9)        VALUE ZEROS.    
00977          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00978          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00979          10  FILLER              PIC S9(12)V99    VALUE ZEROS.    
00980          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00981          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00982          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00983          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00984          10  FILLER              PIC S9(11)V99    VALUE ZEROS.    
00985  EJECT                                                            
00986  01  PRINT-DECISION-TABLE-T  COMP-3.                              
00987      05  PRINT-TABLE-T OCCURS 12 TIMES.                           
00988          10  PLPRM-T             PIC S9(11)V99.                   
00989          10  PLCAN-T             PIC S9(11)V99.                   
00990          10  PLCLM-T             PIC S9(12)V99.                   
00991          10  PAPRM-T             PIC S9(11)V99.                   
00992          10  PACAN-T             PIC S9(11)V99.                   
00993          10  PACLM-T             PIC S9(11)V99.                   
00994          10  PTCOM-T             PIC S9(11)V99.                   
00995                                                                   
00996  01  PRINT-ZERO-TABLE-T  COMP-3.                                  
00997      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00998      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
00999      05  FILLER                  PIC S9(12)V99    VALUE ZEROS.    
01000      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
01001      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
01002      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
01003      05  FILLER                  PIC S9(11)V99    VALUE ZEROS.    
01004  EJECT                                                            
01005  01  CONVERSION-DATE             PIC 9(08) VALUE ZEROS.           
01006  01  CONVERSION-DATE-R REDEFINES CONVERSION-DATE.                 
01007      05  CONV-CCYY               PIC 9(4).                        
01008      05  CONV-MO                 PIC 99.                          
01009      05  CONV-DA                 PIC 99.                          
01010                                                                   
01011  01  COMPARE-DATE-TABLE.                                          
01012 *    05  COMPARE-DTS OCCURS 13 TIMES.                             
122804     05  COMPARE-DTS OCCURS 15.
01013          10  COMPARE-DT.                                          
01014              15  COMP-CCYY       PIC 9(04).                       
01015              15  COMP-CCYR REDEFINES COMP-CCYY.                   
01016                  20  COMP-CC     PIC 99.                          
01017                  20  COMP-YR     PIC 99.                          
01018              15  COMP-MO         PIC 99.                          
01019                                                                   
01020  01  COMPARE-DATE9TABLE REDEFINES COMPARE-DATE-TABLE.             
01021 *    05  COMPARE9DTS OCCURS 13 TIMES.                             
122804     05  COMPARE9DTS OCCURS 15.
01022          10  COMPARE9DT          PIC 9(06).                       
01023                                                                   
122804 01  TWO-YEARS-AGO               PIC 9(06)  VALUE ZEROS.
122804 01  TWO-YEAR-ENDS-AGO           PIC 9(06)  VALUE ZEROS.
122804 01  YEAR-END-DT                 PIC 9(06)  VALUE ZEROS.
01025                                                                   
01024  01  YEAR-OLD-DATE               PIC 9(06).                       
01026  01  FISCAL-DATE       REDEFINES YEAR-OLD-DATE.                   
01027      05  FISCAL-CCYY             PIC 9(04).                       
01028      05  FISCAL-MO               PIC 99.                          
01029                                                                   
01030  01  SAVE-EPX.                                                    
01031      05  S-EPX-CARR              PIC X       VALUE ZEROS.         
01032      05  S-EPX-GRP               PIC X(6)    VALUE ZEROS.         
01033      05  S-EPX-STATE             PIC XX      VALUE ZEROS.         
01034      05  S-EPX-ACCT              PIC X(10)   VALUE ZEROS.         
01035      05  S-EPX-EXP-DT            PIC 9(11)   COMP-3 VALUE 0.      
01036                                                                   
01037  01  ACCOUNT-FULL-CONTROL.                                        
01038      05  ACCT-CONTROL            PIC X(25)   VALUE LOW-VALUES.    
01039      05  ACCT-EFF-DT             PIC 9(11) COMP-3 VALUE ZEROES.   
01040  EJECT                                                            
01041  01  SAVE-ACCOUNT-DATES.                                          
01042      05  SAVE-AM-EXPIRE-DT       PIC 9(11).                       
01043      05  SAVE-AM-EXPIRE-DT-R REDEFINES                            
01044            SAVE-AM-EXPIRE-DT.                                     
01045          10  FILLER              PIC 999.                         
01046          10  SAVE-AM-EXP-CCYY    PIC 9(04).                       
01047          10  SAVE-AM-EXP-CCYR REDEFINES SAVE-AM-EXP-CCYY.         
01048              15  SAVE-AM-EXP-CC  PIC 99.                          
01049              15  SAVE-AM-EXP-YR  PIC 99.                          
01050          10  SAVE-AM-EXP-MO      PIC 99.                          
01051          10  SAVE-AM-EXP-DA      PIC 99.                          
01052      05  SAVE-AM-HI-CERT-DATE    PIC 9(11).                       
01053      05  SAVE-AM-HI-CERT-DATE-R REDEFINES                         
01054             SAVE-AM-HI-CERT-DATE.                                 
01055          10  FILLER              PIC 999.                         
01056          10  SAVE-AM-HI-CC       PIC 99.                          
01057          10  SAVE-AM-HI-YR       PIC 99.                          
01058          10  SAVE-AM-HI-MO       PIC 99.                          
01059          10  SAVE-AM-HI-DA       PIC 99.                          
01060                                                                   
01061  01  SAVE-ACCT-NAME-ADDRESS.                                      
01062      05  SAVE-ACCT-NAME          PIC X(30)   VALUE SPACES.        
01063      05  SAVE-ACCT-ADDRS         PIC X(30)   VALUE SPACES.        
01064                                                                   
01065  01  EP-DATE.                                                     
01066      05  E-CCYY                  PIC 9(04).                       
01067      05  E-MO                    PIC 99.                          
01068                                                                   
01069  01  EP9DATE REDEFINES EP-DATE   PIC 9(6).                        
01070  EJECT                                                            
01071  COPY ELCDATE.                                                    
01072                                                                   
01073                                  COPY ELCDTECX.                   
01074                                                                   
01075                                  COPY ELCDTEVR.                   
01076                                                                   
01077                                  COPY ELCEPCVR.                   

       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH             PIC S9(4) COMP.
           05  PARM-VALUE              PIC X(10).

01079  PROCEDURE DIVISION USING PARM.
01080                                                                   
01081  CAPTURE-START.                                                   
01082                                                                   
01083      OPEN I-O ERMEBL.                                             
01084                                                                   
01085      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        
01086          NEXT SENTENCE                                            
01087        ELSE                                                       
01088          MOVE 'N'                TO ME-UPDATE-FLAG.               
01089                                                                   
01090  0100-START-HERE.                                                 
01091                              COPY ELCDTERX SUPPRESS.              
01092                                                                   
01093      MOVE WS-TIME                TO ME-START-TIME.                
01094      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                
01095      MOVE ME-START-MO            TO ME-CNDS-MO.                   
01096      MOVE ME-START-DA            TO ME-CNDS-DA.                   
01097      MOVE ME-START-YR            TO ME-CNDS-YR.                   
01098  EJECT                                                            
01099      MOVE LIFE-OVERRIDE-L6       TO HD-9-LF-OVRD-1                
01100                                     HD-9-LF-OVRD-2                
01101                                     HD-9-LF-OVRD-3.               
01102      MOVE   AH-OVERRIDE-L6       TO HD-9-AH-OVRD-1                
01103                                     HD-9-AH-OVRD-2                
01104                                     HD-9-AH-OVRD-3.               
01105                                                                   
01106      MOVE DTE-CLIENT             TO ME-COMPANY.                   
01107      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.           
01108      MOVE MONTH-END-MOYR         TO ME-MOYR.                      
01109                                                                   
01110      IF ME-DO-UPDATE                                              
01111          READ ERMEBL INVALID KEY                                  
01112              MOVE 'N'            TO ME-UPDATE-FLAG                
01113              CLOSE ERMEBL.                                        
01114                                                                   
01115  0110-MOVE-DATES.                                                 
01116                                                                   
01117      MOVE DTE-CONV-DT            TO CONVERSION-DATE.              
01118      MOVE WS-CURRENT-DATE        TO HD-IPL-DATE.                  
01119      MOVE COMPANY-NAME           TO HD-COMPANY-NAME.              
01120      MOVE ALPH-DATE              TO HD-ALPHA-DATE.                
01121      MOVE ZEROS                  TO SAVE-AM-EXPIRE-DT             
01122                                     SAVE-AM-HI-CERT-DATE.         
01123                                                                   
01124      IF DTE-QTR-CO = '1'                                          
01125          MOVE '1'                TO QTR-COMP.                     
01126                                                                   
01127  0120-OPEN-FILES.                                                 
01128      OPEN INPUT  AM-MAST-IN                                       
01129                  EARNED-PREM                                      
01130                  DISK-DATE                                        
01131           OUTPUT EXTRACT-OT                                       
CIDMOD                 DISPLAY-PRT
101504                 RPTCDE1
01132                  PRNT.                                            
01133                                                                   
01134      IF ERACCT-FILE-STATUS NOT = ZERO AND '97'                    
01135          MOVE 'OPEN ERROR - ERACCTT' TO WS-ABEND-MESSAGE          
01136          MOVE ERACCT-FILE-STATUS     TO WS-ABEND-FILE-STATUS      
01137          GO TO ABEND-PGM.                                         
01138                                                                   
CIDMOD                                                                  
030612     IF  DTE-CLIENT = 'CSO' OR 'CID' OR 'AHL'
CIDMOD         MOVE '2' TO DTE-FMT-OPT.                                 
CIDMOD                                                                  
01139  0130-CLEAR-TOTAL-AREAS.                                          
01140      MOVE +000                   TO X1.                           
01141      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       
01142      MOVE +000                   TO X1.                           
01143      PERFORM 0150-ZERO-ACCUM-ACC   THRU 0150-EXIT 15 TIMES.       
01144      MOVE +000                   TO X1.                           
01145      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
01146      MOVE +000                   TO X1.                           
01147      PERFORM 0160-ZERO-ACCUM-ST   THRU  0160-EXIT 15 TIMES.       
01148      MOVE +000                   TO X1.                           
01149      PERFORM 0170-ZERO-ACCUM-GRP  THRU 0170-EXIT  15 TIMES.       
01150      MOVE +000                   TO X1.                           
01151      PERFORM 0180-ZERO-ACCUM-CARR THRU 0180-EXIT  15 TIMES.       
01152      MOVE +000                   TO X1.                           
01153      PERFORM 0190-ZERO-ACCUM-GRAND THRU 0190-EXIT 15 TIMES.       
01154      MOVE ZERO-OCCURS-ACCUM      TO GRAND-TOTALS.                 
01155      MOVE ZEROS                  TO EP-CONTROL                    
01156                                     X1.                           
01157      PERFORM 0200-ZERO-DATE-AREA THRU 0200-EXIT   15 TIMES.       
01158      GO TO 0210-FIND-DATE-RANGE.                                  
01159                                                                   
01160  0140-ZERO-PRINT-TABLE.                                           
01161      ADD +1                      TO X1.                           
01162      MOVE PRINT-ZERO-TABLE       TO PRINT-TABLE (X1).             
01163      MOVE PRINT-ZERO-TABLE-T     TO PRINT-TABLE-T (X1).           
01164  0140-EXIT.                                                       
01165                                                                   
01166  0150-ZERO-ACCUM-ACC.                                             
01167      ADD +1                      TO X1.                           
01168      MOVE ZERO-ACCOUNT-ACCUM     TO ACCOUNT-ACCUM (X1).           
01169      MOVE ZERO-ACCOUNT-ACCUM-T   TO ACCOUNT-ACCUM-T (X1).         
01170  0150-EXIT.                                                       
01171                                                                   
01172  0155-ZERO-DATE-RANGE-ACC.                                        
01173      ADD +1                          TO  X1.                      
01174      MOVE ZERO-DATE-RANGE-ACCUM      TO  DATE-RANGE-ACCUM (X1).   
01175      MOVE ZERO-DATE-RANGE-ACCUM-T    TO  DATE-RANGE-ACCUM-T (X1). 
01176  0155-EXIT.                                                       
01177                                                                   
01178  0160-ZERO-ACCUM-ST.                                              
01179      ADD +1                      TO X1.                           
01180      MOVE ZERO-OCCURS-ACCUM      TO STATE-ACCUM (X1).             
01181      MOVE ZERO-OCCURS-ACCUM-T    TO STATE-ACCUM-T (X1).           
01182  0160-EXIT.                                                       
01183                                                                   
01184  0170-ZERO-ACCUM-GRP.                                             
01185      ADD +1                      TO X1.                           
01186      MOVE ZERO-OCCURS-ACCUM      TO GROUPING-ACCUM (X1).          
01187      MOVE ZERO-OCCURS-ACCUM-T    TO GROUPING-ACCUM-T (X1).        
01188  0170-EXIT.                                                       
01189                                                                   
01190  0180-ZERO-ACCUM-CARR.                                            
01191      ADD +1                      TO X1.                           
01192      MOVE ZERO-OCCURS-ACCUM      TO CARRIER-ACCUM (X1).           
01193      MOVE ZERO-OCCURS-ACCUM-T    TO CARRIER-ACCUM-T (X1).         
01194  0180-EXIT.                                                       
01195                                                                   
01196  0190-ZERO-ACCUM-GRAND.                                           
01197      ADD +1                      TO X1.                           
01198      MOVE ZERO-OCCURS-ACCUM      TO GRAND-ACCUM (X1).             
01199      MOVE ZERO-OCCURS-ACCUM-T    TO GRAND-ACCUM-T (X1).           
01200  0190-EXIT.                                                       
01201                                                                   
01202  0200-ZERO-DATE-AREA.                                             
01203      ADD 1                       TO X1.                           
01204      MOVE ZEROES                 TO COMPARE9DT (X1).              
01205  0200-EXIT.                                                       
01206                                                                   
01207  0210-FIND-DATE-RANGE.                                            
01208      IF QTR-CO                                                    
01209          IF RUN-MO = 03 OR 06 OR 09 OR 12                         
01210              MOVE '1'            TO QTR-SW.                       
01211                                                                   
01212      MOVE RUN-CCYY               TO RUN-DT-CCYY.                  
01213      MOVE RUN-MO                 TO RUN-DT-MO.                    
01214      MOVE +1                     TO X1.                           
01215      MOVE +0                     TO Y1.                           
01216                                                                   
01217      IF QTR-CO AND QTR-END                                        
01218          COMPUTE COMPARE9DT (X1) = RUN9DT - 300                   
01219      ELSE                                                         
01220          COMPUTE COMPARE9DT (X1) = RUN9DT  - 100.                 
01221                                                                   
01222      COMPUTE YEAR-OLD-DATE = RUN9DT - 100.                        
122804     COMPUTE TWO-YEARS-AGO = RUN9DT - 200
01223                                                                   
01224 ****** THE FOLLOWING LINES OF CODE ARE FOR AFL ONLY **************
01225                                                                   
01226      IF DTE-CLIENT NOT = 'AFL'                                    
01227          GO TO 0220-BUILD-DATE-TABLE.                             
01228                                                                   
01229      IF RUN-MO GREATER 06                                         
01230          MOVE RUN-CCYY           TO FISCAL-CCYY                   
01231      ELSE                                                         
01232          COMPUTE FISCAL-CCYY = RUN-CCYY - 1.                      
01233                                                                   
01234      MOVE 06                     TO FISCAL-MO.                    
01235 ************ END OF AFL CLIENT CODING ****************************
01236                                                                   
01237  0220-BUILD-DATE-TABLE.                                           
01238                                                                   
122804     IF (COMP-MO (X1) = 12)
122804        AND (TWO-YEAR-ENDS-AGO = ZEROS)
122804        COMPUTE TWO-YEAR-ENDS-AGO = COMPARE9DT (X1) - 100
122804        MOVE COMPARE9DT (X1)     TO YEAR-END-DT
122804     END-IF

01239      ADD +1                      TO X1 Y1.                        
01240                                                                   
01241      IF X1 GREATER THAN DATE-RANGE-MAX                            
01242          GO TO 0230-MODIFY-HEADINGS.                              
01243                                                                   
01244      IF QTR-CO AND QTR-END                                        
01245          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0003         
01246      ELSE                                                         
01247          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0001.        
01248                                                                   
01249      IF COMP-MO (X1) GREATER THAN 12                              
01250          COMPUTE COMPARE9DT (X1) = COMPARE9DT (X1) + 0088.        
01251                                                                   
01252      GO TO 0220-BUILD-DATE-TABLE.                                 
01253                                                                   
01254  0230-MODIFY-HEADINGS.                                            

122804     MOVE TWO-YEARS-AGO          TO COMPARE9DT (14)
122804     MOVE TWO-YEAR-ENDS-AGO      TO COMPARE9DT (15)

           PERFORM VARYING X1 FROM +1 BY +1 UNTIL
              X1 > 15
              MOVE X1                  TO DISP-X1
              DISPLAY ' COMPARE DATE ' DISP-X1 ' ' COMPARE9DT (X1) 
           END-PERFORM
           DISPLAY ' YEAR END DATE ' YEAR-END-DT
01255      IF QTR-CO AND QTR-END                                        
01256          MOVE 'QUARTERLY'        TO ACCT-PER-HD  STATE-PER-HD     
01257                                     GRP-PER-HD   CARR-PER-HD.     
01258  EJECT                                                            
01259  0240-READ-FIRST-RECORD.                                          
01260      PERFORM 0250-READ-EARNED-PREM THRU 0259-READ-EXIT.           
01261      GO TO 0270-SAVE-EP-CONTROL.                                  
01262                                                                   
01263  0250-READ-EARNED-PREM.                                           
01264      READ EARNED-PREM                                             
01265                  AT END GO TO 9990-END-OF-JOB.                    
01266                                                                   
01267      IF EP-REIN = 'R'                                             
01268          GO TO 0250-READ-EARNED-PREM.                             
01269                                                                   
01270      COPY ELCEPCM1.                                               
01271                                                                   
      *    IF (EP-RUN-DTE = 20031231)
      *       AND (EP-EXP-DTE = 19850701)
      *       SET DISP-REC             TO TRUE
      *    ELSE
      *       SET NO-DISP-REC          TO TRUE
      *    END-IF
           
01272      IF QTR-CO                                                    
01273          IF QTR-END                                               
01274              IF EP-RUN-MO = 03 OR 06 OR 09 OR 12                  
01275                  NEXT SENTENCE                                    
01276              ELSE                                                 
01277                  GO TO 0250-READ-EARNED-PREM                      
01278          ELSE                                                     
01279              GO TO 0250-READ-EARNED-PREM.                         
01280                                                                   
01281  0259-READ-EXIT.                                                  
01282      EXIT.                                                        
01283                                                                   
01284  0260-COMPARE-RECORDS.                                            
01285      IF (DTE-CLIENT EQUAL 'TMS')
01286         OR (DTE-FMT-OPT EQUAL '2')
01287         CONTINUE
01288      ELSE
01289         GO TO 0260-COMPARE-CONTINUE
           END-IF
01290                                                                   
01291      IF EP-CARRIER NOT = S-EPX-CARR                               
01292          PERFORM 2700-CARRIER-BREAK THRU 2750-ZERO-CARRIER        
01293          GO TO 0270-SAVE-EP-CONTROL.                              
01294                                                                   
01295      IF EP-GROUPING NOT = S-EPX-GRP                               
01296          PERFORM 2630-GROUPING-BREAK THRU 2680-ZERO-GROUPING      
01297          GO TO 0270-SAVE-EP-CONTROL.                              
01298                                                                   
01299      IF EP-STATE NOT = S-EPX-STATE                                
01300          PERFORM 2560-STATE-BREAK THRU 2610-ZERO-STATE            
01301          GO TO 0270-SAVE-EP-CONTROL.                              
01302                                                                   
01303      IF (EP-EXP-DTE NOT = S-EPX-EXP-DT) OR                        
01304         (EP-ACCOUNT NOT = S-EPX-ACCT)                             
 
      *        IF EP-EXP-DTE = 19900601
      *           DISPLAY ' DATE RANGE BREAK '
      *        END-IF
01305          MOVE 0                  TO A1                            
01306          PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT               
01307           VARYING X1 FROM 2 BY 1 UNTIL                            
01308            X1 IS GREATER THAN DATE-RANGE-MAX                      
01309          MOVE SPACES             TO  B-E-AGT-TABLE                
01310          MOVE +0                 TO  X1                           
01311 *        PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 13 TIMES 
122804         PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES 
01312          IF EP-ACCOUNT NOT = S-EPX-ACCT                           
01313              PERFORM 2380-ACCOUNT-BREAK THRU 2540-ZERO-ACCOUNT.   
01314                                                                   
01315      GO TO 0270-SAVE-EP-CONTROL.                                  
01316                                                                   
01317  0260-COMPARE-CONTINUE.                                           
01318                                                                   
01319      IF EP-CARRIER NOT = S-EPX-CARR                               
01320          PERFORM 0700-CARRIER-BREAK THRU 0750-ZERO-CARRIER        
01321          GO TO 0270-SAVE-EP-CONTROL.                              
01322                                                                   
01323      IF EP-GROUPING NOT = S-EPX-GRP                               
01324          PERFORM 0630-GROUPING-BREAK THRU 0680-ZERO-GROUPING      
01325          GO TO 0270-SAVE-EP-CONTROL.                              
01326                                                                   
01327      IF EP-STATE NOT = S-EPX-STATE                                
01328          PERFORM 0560-STATE-BREAK THRU 0610-ZERO-STATE            
01329          GO TO 0270-SAVE-EP-CONTROL.                              
01330                                                                   
01331      IF (EP-EXP-DTE NOT = S-EPX-EXP-DT) OR                        
01332         (EP-ACCOUNT NOT = S-EPX-ACCT)                             
01333          MOVE 0                  TO A1                            
01334          PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT               
01335           VARYING X1 FROM 2 BY 1 UNTIL                            
01336            X1 IS GREATER THAN DATE-RANGE-MAX                      
01337          MOVE SPACES             TO  B-E-AGT-TABLE                
01338          MOVE +0                 TO  X1                           
01339          PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES 
01340          IF EP-ACCOUNT NOT = S-EPX-ACCT                           
01341              PERFORM 0380-ACCOUNT-BREAK THRU 0540-ZERO-ACCOUNT.   
01342                                                                   
01343  0270-SAVE-EP-CONTROL.                                            
01344      MOVE EP-CONTROL             TO SAVE-EPX.                     
01345                                                                   
01346      IF EP-CONTROL NOT = ACCOUNT-FULL-CONTROL                     
              IF DISP-REC
                 DISPLAY ' MATCH ACCT MASTER '
              END-IF
01347         PERFORM 0900-MATCH-ACCT-MASTER THRU 0909-MATCH-EXIT
           END-IF
01348                                                                   
01349      PERFORM 0290-ACCUMULATE THRU 0370-ACCUMULATE-EXIT.           

           IF DISP-REC
              DISPLAY SPACES
              DISPLAY ' EP9DATE ' EP9DATE
              DISPLAY ' DATE RANGE TOTALS '
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 X1 > +15
                 MOVE DR-LPRM (X1)     TO WS-DISP-AMT
      *          MOVE AC-LPRM (X1)     TO WS-DISP-AMT
                 MOVE X1               TO DISP-X1
                 DISPLAY ' X1 ' DISP-X1 ' AMT ' WS-DISP-AMT
              END-PERFORM
           END-IF
01350                                                                   
01351      GO TO 0250-READ-EARNED-PREM.                                 
01352  EJECT                                                            
01353  0290-ACCUMULATE.                                                 
01354      MOVE EP-RUN-CCYY            TO E-CCYY.                       
01355      MOVE EP-RUN-MO              TO E-MO.                         
01356                                                                   
01357      IF EP-EXP-DTE  IS GREATER THAN  SAVE-AM-EXPIRE-DT            
01358          MOVE EP-EXP-DTE         TO  SAVE-AM-EXPIRE-DT.           
01359                                                                   
01360      IF EP-HI-CERT  IS GREATER THAN  SAVE-AM-HI-CERT-DATE         
01361          MOVE EP-HI-CERT         TO  SAVE-AM-HI-CERT-DATE.        
01362                                                                   
01363      IF EP9DATE  LESS THAN COMPARE9DT (15)
01364         GO TO 0370-ACCUMULATE-EXIT
           END-IF

           EVALUATE TRUE
              WHEN PARM-VALUE = 'ALL'
                 MOVE SPACES           TO HD2-PARM
              WHEN PARM-VALUE = 'UNI'
                 AND AM-REPORT-CODE-3 = 'UNI'
                 MOVE ' UNI ONLY '     TO HD2-PARM
              WHEN PARM-VALUE = 'JMA'
                 AND AM-REPORT-CODE-3 = 'JM&A'
                 MOVE ' JM&A ONLY '    TO HD2-PARM
              WHEN PARM-VALUE = 'RLIC'
                 AND AM-REPORT-CODE-3 = 'RLIC'
                 MOVE ' RLIC ONLY '    TO HD2-PARM
              WHEN PARM-VALUE = 'SVCLIFE'
                 AND AM-REPORT-CODE-3 = 'SVCLIFE'
                 MOVE ' SVCLIFE ONLY ' TO HD2-PARM
                 MOVE AM-REPORT-CODE-3 TO AM-REPORT-CODE-1
              WHEN PARM-VALUE = 'EMERALD'
                 AND AM-REPORT-CODE-3 = 'EMERALD'
                 MOVE ' EMERALD ONLY ' TO HD2-PARM
083018        WHEN PARM-VALUE = 'LDS'
083018           AND AM-REPORT-CODE-3 = 'LDS'
083018           MOVE ' LDS ONLY ' TO HD2-PARM
022312        WHEN PARM-VALUE = 'AHL'
022312           AND AM-REPORT-CODE-3 = 'AHL'
022312           MOVE ' AHL ONLY '     TO HD2-PARM
              WHEN PARM-VALUE = 'OTH'
                 AND AM-REPORT-CODE-3 NOT = 'JM&A' AND 'UNI'
                 MOVE ' OTHER THAN SPECIALS '
                                       TO HD2-PARM
              WHEN OTHER
                 GO TO 0370-ACCUMULATE-EXIT
           END-EVALUATE

022607*    IF (DTE-CLIENT = 'CID')
022607*       AND (AM-REPORT-CODE-3 NOT = 'UNI' AND 'JM&A')
022607*       GO TO 0370-ACCUMULATE-EXIT
022607*    END-IF

01365                                                                   
122804*    IF (EP9DATE < COMPARE9DT (1))
122804*       AND (EP9DATE NOT = COMPARE9DT (14))
122804*       AND (EP9DATE NOT = COMPARE9DT (15))
122804*       GO TO 0370-ACCUMULATE-EXIT
122804*    END-IF
122804*    IF EP9DATE < COMPARE9DT (15)
122804*       AND (EP-PURGE NOT = 'P')
122804*       GO TO 0370-ACCUMULATE-EXIT
122804*    END-IF

01366      IF (E-CCYY GREATER THAN RUN-CCYY)  OR                        
01367         (E-CCYY = RUN-CCYY AND E-MO GREATER THAN RUN-MO)          
01368          GO TO 0370-ACCUMULATE-EXIT.                              
01369                                                                   
01370      IF EP-PURGE = 'P'                                            
01371          GO TO 0320-ACCUMULATE-PURGE.                             
01372                                                                   
01373      IF VALID-EP-ID                                               
01374          GO TO 0300-ACCUMULATE-A.                                 

01376      IF VALID-EC-ID                                               
01377          GO TO 0350-ACCUMULATE-1.                                 
01378                                                                   
01379      GO TO 0370-ACCUMULATE-EXIT.                                  
01380                                                                   
01381  0300-ACCUMULATE-A.                                               
01382      MOVE +0 TO X1.                                               
01383                                                                   
01384  0310-ACCUMULATE-B.                                               
01385      ADD +1 TO X1.                                                
01386                                                                   
01387      IF X1 GREATER THAN DATE-RANGE-MAX                            
01388 *       DISPLAY 'DATE TABLE ERROR OR EP-RUN-DATE ERROR'          
01389 *       DISPLAY 'EP-DATE ' EP-DATE                               
01390 *       DISPLAY 'RUN-DT  ' RUN-DT                                
01391 *       DISPLAY 'COMPARE-DATE-TABLE ' COMPARE-DATE-TABLE         
01392 *       MOVE '0301'             TO WS-RETURN-CODE                
01393 *       GO TO ABEND-PGM.                                         
122804        GO TO 0370-ACCUMULATE-EXIT
           END-IF
01394                                                                   
01395      IF COMPARE-DT (X1) NOT = EP-DATE                             
01396          GO TO 0310-ACCUMULATE-B.                                 
01397                                                                   
01398      IF EP-HI-CERT  IS GREATER THAN  AC-DATE (X1)                 
01399          MOVE EP-HI-CERT         TO  AC-DATE (X1)                 
01400                                      AC-DATE-T (X1)               
01401                                      AC-DATE-R (X1).              
01402                                                                   
01403      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            
01404          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   
01405          COMPUTE LBEN = EP-ISS-BEN - EP-CNC-BEN                   
01406          COMPUTE LPRM = EP-ISS-PRM - EP-CNC-PRM                   
01407          ADD CERT TO AC-CERT (X1)                                 
01408                      DR-CERT (X1)                                 
01409          ADD LBEN TO AC-LBEN (X1)                                 
01410                      DR-LBEN (X1)                                 
01411          ADD LPRM TO AC-LPRM (X1) AC-TPRM (X1)                    
01412                      DR-LPRM (X1) DR-TPRM (X1)                    
01413          ADD EP-CLM-AMT TO AC-LCLM (X1)                           
01414                            DR-LCLM (X1)                           
01415 ******  THE FOLLOWING TOTALS ARE NET                              
01416          ADD EP-ISS-CNT TO AC-CERT-T (X1)                         
01417                            DR-ISS    (X1)                         
01418                            DR-CERT-T (X1)                         
01419                            AC-L-COVERAGE-T (X1)                   
01420                            DR-L-COVERAGE-T (X1)                   
01421          ADD EP-CNC-CNT TO AC-CANCEL-T (X1)                       
01422                            DR-CANCEL-T (X1)                       
01423          ADD EP-ISS-PRM TO AC-LPRM-T (X1)   AC-TPRM-T (X1)        
01424                            DR-LPRM-T (X1)   DR-TPRM-T (X1)        
01425          ADD EP-CNC-PRM TO AC-LCAN-T (X1)                         
01426                            DR-LCAN-T (X1)                         
01427          ADD EP-CLM-AMT TO AC-LCLM-T (X1)                         
01428                            DR-LCLM-T (X1).                        
01429                                                                   
01430      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              
01431          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   
01432          COMPUTE ABEN = EP-ISS-BEN - EP-CNC-BEN                   
01433          COMPUTE APRM = EP-ISS-PRM - EP-CNC-PRM                   
01434          ADD CERT TO AC-CERT (X1)                                 
01435                      DR-CERT (X1)                                 
01436          ADD ABEN TO AC-ABEN (X1)                                 
01437                      DR-ABEN (X1)                                 
01438          ADD APRM TO AC-APRM (X1) AC-TPRM (X1)                    
01439                      DR-APRM (X1) DR-TPRM (X1)                    
01440          ADD EP-CLM-AMT TO AC-ACLM (X1)                           
01441                            DR-ACLM (X1)                           
01442 ******  THE FOLLOWING TOTALS ARE NET                              
01443          ADD EP-ISS-CNT   TO AC-CERT-T (X1)                       
01444                              DR-ISS    (X1)                       
01445                              DR-CERT-T (X1)                       
01446                              AC-A-COVERAGE-T (X1)                 
01447                              DR-A-COVERAGE-T (X1)                 
01448          ADD EP-CNC-CNT   TO AC-CANCEL-T (X1)                     
01449                              DR-CANCEL-T (X1)                     
01450          ADD EP-ISS-PRM   TO AC-APRM-T (X1) AC-TPRM-T (X1)        
01451                              DR-APRM-T (X1) DR-TPRM-T (X1)        
01452          ADD EP-CNC-PRM   TO AC-ACAN-T (X1)                       
01453                              DR-ACAN-T (X1)                       
01454          ADD EP-CLM-AMT   TO AC-ACLM-T (X1)                       
01455                              DR-ACLM-T (X1).                      
010306*    IF X1 < DATE-RANGE-MAX
010306        GO TO 0310-ACCUMULATE-B
010306*    END-IF

           .
01459  0320-ACCUMULATE-PURGE.                                           
01460      IF EP-REIN NOT = ' '                                         
01461          GO TO 0370-ACCUMULATE-EXIT.                              
01462                                                                   
      *    IF DISP-REC
      *       DISPLAY ' MADE IT TO 0320 ACCUM '
      *    END-IF
01463      MOVE +0                     TO X1.                           
01464                                                                   
01465  0330-ACCUMULATE-PURGE-1.                                         
01466      ADD +1 TO X1.                                                
01467                                                                   
01468      IF X1 GREATER THAN DATE-RANGE-MAX                            
01469          DISPLAY 'DATE-TABLE ERROR OR EP-RUN-DATE ERROR'          
01470          DISPLAY 'EP-DATE  '  EP-DATE                             
01471          DISPLAY 'RUN-DT   '  RUN-DT                              
01472          DISPLAY 'COMPARE-DATE-TABLE  '  COMPARE-DATE-TABLE       
01473          MOVE '0301'             TO WS-RETURN-CODE                
01474          GO TO ABEND-PGM.                                         
01475                                                                   
01476      IF COMPARE-DT (X1) LESS THAN EP-DATE                         
01477          GO TO 0330-ACCUMULATE-PURGE-1.                           
01478                                                                   
01479      IF VALID-EC-ID                                               
01480          IF EC-AGT-TYPE (1) = 'C'  OR  'D'                        
01481              COMPUTE TCOM = EC-ISS-COMM (1) - EC-CNC-COMM (1)     
01482              ADD TCOM            TO AC-TCOM (X1)                  
01483                                     AC-TCOM-T (X1)                
01484                                     DR-TCOM (X1)                  
01485                                     DR-TCOM-T (X1).               
01486      IF VALID-EC-ID                                               
01487          IF EC-AGT-TYPE (2) = 'C'  OR  'D'                        
01488              COMPUTE TCOM = EC-ISS-COMM (2) - EC-CNC-COMM (2)     
01489              COMPUTE TCOM-T = EC-ISS-COMM (2) - EC-CNC-COMM (2)   
01490              ADD TCOM            TO AC-TCOM (X1)                  
01491                                     AC-TCOM-T (X1)                
01492                                     DR-TCOM (X1)                  
01493                                     DR-TCOM-T (X1).               
01494      IF VALID-EC-ID                                               
01495          IF EC-AGT-TYPE (3) = 'C'  OR  'D'                        
01496              COMPUTE TCOM = EC-ISS-COMM (3) - EC-CNC-COMM (3)     
01497              COMPUTE TCOM-T = EC-ISS-COMM (3) - EC-CNC-COMM (3)   
01498              ADD TCOM            TO AC-TCOM (X1)                  
01499                                     AC-TCOM-T (X1)                
01500                                     DR-TCOM (X1)                  
01501                                     DR-TCOM-T (X1).               
01502      IF VALID-EC-ID                                               
01503          IF EC-AGT-TYPE (4) = 'C'  OR  'D'                        
01504              COMPUTE TCOM = EC-ISS-COMM (4) - EC-CNC-COMM (4)     
01505              COMPUTE TCOM-T = EC-ISS-COMM (4) - EC-CNC-COMM (4)   
01506              ADD TCOM            TO AC-TCOM (X1)                  
01507                                     AC-TCOM-T (X1)                
01508                                     DR-TCOM (X1)                  
01509                                     DR-TCOM-T (X1).               
01510      IF VALID-EC-ID                                               
01511          IF EC-AGT-TYPE (5) = 'C'  OR  'D'                        
01512              COMPUTE TCOM-T = EC-ISS-COMM (5) - EC-CNC-COMM (5)   
01513              COMPUTE TCOM = EC-ISS-COMM (5) - EC-CNC-COMM (5)     
01514              ADD TCOM            TO AC-TCOM (X1)                  
01515                                     AC-TCOM-T (X1)                
01516                                     DR-TCOM (X1)                  
01517                                     DR-TCOM-T (X1).               
01518                                                                   
01519      IF NOT VALID-EP-ID                                           
01520          GO TO 0340-P-LOOP-END.                                   
01521                                                                   
01522      IF EP-RCD-TYPE = LIFE-OVERRIDE-L1                            
01523          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   
01524          COMPUTE LBEN = EP-ISS-BEN - EP-CNC-BEN                   
01525          COMPUTE LPRM = EP-ISS-PRM - EP-CNC-PRM                   
01526          ADD CERT       TO AC-CERT (X1)                           
01527                            DR-CERT (X1)                           
01528          ADD LBEN       TO AC-LBEN (X1)                           
01529                            DR-LBEN (X1)                           
01530          ADD LPRM       TO AC-LPRM (X1)                           
01531                            AC-TPRM (X1)                           
01532                            DR-LPRM (X1)                           
01533                            DR-TPRM (X1)                           
01534          ADD EP-CLM-AMT TO AC-LCLM (X1)                           
01535                            DR-LCLM (X1)                           
01536 ******  THE FOLLOWING TOTALS ARE NET                              
01537          ADD EP-ISS-CNT TO AC-CERT-T (X1)                         
01538                            DR-ISS    (X1)                         
01539                            DR-CERT-T (X1)                         
01540                            AC-L-COVERAGE-T (X1)                   
01541                            DR-L-COVERAGE-T (X1)                   
01542          ADD EP-CNC-CNT TO AC-CANCEL-T (X1)                       
01543                            DR-CANCEL-T (X1)                       
01544          ADD EP-ISS-PRM TO AC-LPRM-T (X1)                         
01545                            AC-TPRM-T (X1)                         
01546                            DR-LPRM-T (X1)                         
01547                            DR-TPRM-T (X1)                         
01548          ADD EP-CNC-PRM TO AC-LCAN-T (X1)                         
01549                            DR-LCAN-T (X1)                         
01550          ADD EP-CLM-AMT TO AC-LCLM-T (X1)                         
01551                            DR-LCLM-T (X1).                        
01552                                                                   
01553                                                                   
01554                                                                   
01555      IF EP-RCD-TYPE = AH-OVERRIDE-L1                              
01556          COMPUTE CERT = EP-ISS-CNT - EP-CNC-CNT                   
01557          COMPUTE ABEN = EP-ISS-BEN - EP-CNC-BEN                   
01558          COMPUTE APRM = EP-ISS-PRM - EP-CNC-PRM                   
01559          ADD CERT         TO AC-CERT (X1)                         
01560                              DR-CERT (X1)                         
01561          ADD ABEN         TO AC-ABEN (X1)                         
01562                              DR-ABEN (X1)                         
01563          ADD APRM         TO AC-APRM (X1)                         
01564                              AC-TPRM (X1)                         
01565                              DR-APRM (X1)                         
01566                              DR-TPRM (X1)                         
01567          ADD EP-CLM-AMT   TO AC-ACLM (X1)                         
01568                              DR-ACLM (X1)                         
01569          ADD EP-ISS-CNT   TO AC-CERT-T (X1)                       
01570                              DR-ISS    (X1)                       
01571                              DR-CERT-T (X1)                       
01572                              AC-A-COVERAGE-T (X1)                 
01573                              DR-A-COVERAGE-T (X1)                 
01574          ADD EP-CNC-CNT   TO AC-CANCEL-T (X1)                     
01575                              DR-CANCEL-T (X1)                     
01576          ADD EP-ISS-PRM   TO AC-APRM-T (X1) AC-TPRM-T (X1)        
01577                              DR-APRM-T (X1) DR-TPRM-T (X1)        
01578          ADD EP-CNC-PRM   TO AC-ACAN-T (X1)                       
01579                              DR-ACAN-T (X1)                       
01580          ADD EP-CLM-AMT   TO AC-ACLM-T (X1)                       
01581                              DR-ACLM-T (X1).                      
01582                                                                   
01583                                                                   
01584  0340-P-LOOP-END.                                                 
01585      IF X1 LESS THAN DATE-RANGE-MAX                               
01586          GO TO 0330-ACCUMULATE-PURGE-1.                           
01587                                                                   
           IF DISP-REC
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 X1 > +15
                 MOVE DR-LPRM (X1)     TO WS-DISP-AMT
                 MOVE X1               TO DISP-X1
                 DISPLAY ' X1 ' DISP-X1 ' AMT ' WS-DISP-AMT
              END-PERFORM
           END-IF

01588      GO TO 0370-ACCUMULATE-EXIT.                                  
01589                                                                   
01590  0350-ACCUMULATE-1.                                               
01591      MOVE +0                     TO X1.                           
01592                                                                   
01593  0360-ACCUMULATE-2.                                               
01594      ADD +1 TO X1.                                                
01595                                                                   
01596      IF X1 GREATER THAN DATE-RANGE-MAX                            
01597 *       DISPLAY 'DATE TABLE ERROR OR EP-RUN-DATE ERROR '         
01598 *       DISPLAY 'EP-DATE ' EP-DATE                               
01599 *       DISPLAY 'RUN-DT ' RUN-DATE                               
01600 *       DISPLAY 'COMPARE-DATE-TABLE ' COMPARE-DATE-TABLE         
01601 *       MOVE '0301'             TO WS-RETURN-CODE                
01602 *       GO TO ABEND-PGM.                                         
122804        GO TO 0370-ACCUMULATE-EXIT
           END-IF
01603                                                                   
01604      IF COMPARE-DT (X1) NOT = EP-DATE                             
01605          GO TO 0360-ACCUMULATE-2.                                 
01606                                                                   
01607      IF EC-AGT-TYPE (1) = 'C'  OR  'D'                            
01608          COMPUTE TCOM = EC-ISS-COMM (1) - EC-CNC-COMM (1)         
01609          ADD TCOM           TO AC-TCOM (X1)                       
01610                                AC-TCOM-T (X1)                     
01611                                DR-TCOM (X1)                       
01612                                DR-TCOM-T (X1).                    
01613      IF EC-AGT-TYPE (2) = 'C'  OR  'D'                            
01614          COMPUTE TCOM = EC-ISS-COMM (2) - EC-CNC-COMM (2)         
01615          ADD TCOM TO          AC-TCOM (X1)                        
01616                               AC-TCOM-T (X1)                      
01617                               DR-TCOM (X1)                        
01618                               DR-TCOM-T (X1).                     
01619      IF EC-AGT-TYPE (3) = 'C'  OR  'D'                            
01620          COMPUTE TCOM = EC-ISS-COMM (3) - EC-CNC-COMM (3)         
01621          ADD TCOM TO          AC-TCOM (X1)                        
01622                               AC-TCOM-T (X1)                      
01623                               DR-TCOM (X1)                        
01624                               DR-TCOM-T (X1).                     
01625      IF EC-AGT-TYPE (4) = 'C'  OR  'D'                            
01626          COMPUTE TCOM = EC-ISS-COMM (4) - EC-CNC-COMM (4)         
01627          ADD TCOM TO          AC-TCOM (X1)                        
01628                               AC-TCOM-T (X1)                      
01629                               DR-TCOM (X1)                        
01630                               DR-TCOM-T (X1).                     
01631      IF EC-AGT-TYPE (5) = 'C'  OR  'D'                            
01632          COMPUTE TCOM = EC-ISS-COMM (5) - EC-CNC-COMM (5)         
01633          ADD TCOM TO          AC-TCOM (X1)                        
01634                               AC-TCOM-T (X1)                      
01635                               DR-TCOM (X1)                        
01636                               DR-TCOM-T (X1).                     
01637                                                                   
010306     IF X1 < DATE-RANGE-MAX
010306        GO TO 0360-ACCUMULATE-2
010306     END-IF

           .
01638  0370-ACCUMULATE-EXIT.                                            
01639      EXIT.                                                        
01640  EJECT                                                            
01641  0380-ACCOUNT-BREAK.                                              
01642      MOVE +0                     TO X1.                           
01643      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       
01644      PERFORM 0770-PRINT-DECISION THRU 0800-EXIT.                  
01645                                                                   
01646      IF P-ACC-SW = '1'                                            
01647          MOVE ' '                TO P-ACC-SW                      
01648      ELSE                                                         
01649          GO TO 0540-ZERO-ACCOUNT.                                 
01650                                                                   
01651      PERFORM 0810-ACC-HD  THRU  0840-EXIT.                        
01652      MOVE +1                     TO X1.                           
01653      MOVE +0                     TO Y1.                           
01654                                                                   
01655  0390-ACCOUNT-BREAK-PRINT.                                        
01656      ADD 1               TO X1 Y1.                                
01657                                                                   
01658 *    IF X1 GREATER THAN DATE-RANGE-MAX                            
01659 *        GO TO 0520-PRINT-LAST-AC-12.                             
01660                                                                   
122804     IF X1 > 13
122804         GO TO 0520-PRINT-LAST-AC-12.                             
01660                                                                   
01661      COMPUTE CERT = AC-CERT (X1) - AC-CERT (Y1).                  
01662      COMPUTE LBEN = AC-LBEN (X1) - AC-LBEN (Y1).                  
01663      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  
01664      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  
01665      COMPUTE ABEN = AC-ABEN (X1) - AC-ABEN (Y1).                  
01666      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  
01667      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  
01668      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  
01669      COMPUTE TPRM = LPRM + APRM.                                  
01670      MOVE COMP-YR (X1)           TO DET-YR.                       
01671      MOVE COMP-MO (X1)           TO DET-MO.                       
01672      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
01673      MOVE CERT                   TO DET-CERTS.                    
01674      MOVE LBEN                   TO DET-LBEN.                     
01675      MOVE LPRM                   TO DET-LPRM.                     
01676      MOVE LCLM                   TO DET-LCLM.                     
01677      MOVE ABEN                   TO DET-ABEN.                     
01678      MOVE APRM                   TO DET-APRM.                     
01679      MOVE ACLM                   TO DET-ACLM.                     
01680      MOVE TPRM                   TO DET-TPRM.                     
01681      MOVE TCOM                   TO DET-TCOM.                     
01682                                                                   
01683      IF COMP-CCYY (X1) = CONV-CCYY  AND  COMP-MO (X1) = CONV-MO   
01684          MOVE ZEROS TO DET-CERTS DET-LBEN DET-LPRM DET-LCLM       
01685                        DET-ABEN  DET-APRM DET-ACLM                
01686                        DET-TPRM  DET-TCOM                         
01687      ELSE                                                         
01688          PERFORM 0500-ADD-TO-OTHERS  THRU 0510-EXIT.              
01689                                                                   
01690 *    PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT.                  
01691                                                                   
01692      MOVE DETAIL-LINE            TO P-DATA.                       
01693                                                                   
01694      IF X1 = 2                                                    
01695          MOVE '0'                TO X                             
01696      ELSE                                                         
01697          MOVE ' ' TO X.                                           
01698                                                                   
01699      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
01700                                                                   
01701      GO TO 0390-ACCOUNT-BREAK-PRINT.                              
01702  EJECT                                                            
01703  0400-BUILD-EXTRACTS.                                             

           IF DISP-REC
              DISPLAY ' ENTERING BUILD EXTRACT '
           END-IF
01704      IF X1 = 2                                                    
01705          MOVE 1                  TO X1.                           
01706      ADD 1                       TO A1.                           
01707  0410-B-E-RESUME.                                                 
01708                                                                   
01709      MOVE ZEROS                  TO WS-EE-DTE.                    
01710      MOVE COMP-CCYY (X1)         TO EE-CCYY.                      
01711      MOVE COMP-MO (X1)           TO EE-MO.                        
01712      MOVE WS-EE-DTE              TO EE-DTE.                       
01713                                                                   
01714      MOVE SPACE                  TO WS-ISS-FLAG.                  
01715                                                                   
01716      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           
01717          COMPUTE WS-ISS-CNT = DR-ISS (X1) - DR-ISS (A1)           
01718          IF WS-ISS-CNT GREATER THAN ZEROS                         
01719              MOVE 'Y'            TO WS-ISS-FLAG.                  
01720                                                                   
01721      COMPUTE EE-CERT = DR-CERT (X1).                              
01722      COMPUTE EE-LBEN = DR-LBEN (X1).                              
01723      COMPUTE EE-LPRM = DR-LPRM (X1).                              
01724      COMPUTE EE-LCLM = DR-LCLM (X1).                              
01725      COMPUTE EE-ABEN = DR-ABEN (X1).                              
01726      COMPUTE EE-APRM = DR-APRM (X1).                              
01727      COMPUTE EE-ACLM = DR-ACLM (X1).                              
01728      COMPUTE EE-TPRM = EE-LPRM + EE-APRM.                         
01729      COMPUTE EE-TCOM = DR-TCOM (X1).                              
01730      COMPUTE EE-ISS-CNT = DR-ISS (X1).                            
01731                                                                   
01732      MOVE AC-DATE (X1)           TO EE-MTH-HI-CERT.               
01733      MOVE ZERO                   TO EE-CNTL.                      
01734      MOVE SPACES                 TO EE-ACCT-NAME.                 
CIDMOD     MOVE SPACES                 TO EE-ACCT-CITY.                 
102004     IF DTE-CLIENT EQUAL 'HER' OR 'HSL' OR 'CID' OR 'DCC'
030612       OR 'AHL'
022808        EVALUATE AM-STATUS
022808           WHEN '1'
022808              MOVE 'I'           TO EE-ACCT-STATUS
022808           WHEN '2'
022808              MOVE 'T'           TO EE-ACCT-STATUS
022808           WHEN '3'
022808              MOVE 'C'           TO EE-ACCT-STATUS
022808           WHEN '4'
022808              MOVE 'I'           TO EE-ACCT-STATUS
031811           WHEN '5'
031811              MOVE 'S'           TO EE-ACCT-STATUS
021916           WHEN '6'
021916              MOVE 'D'           TO EE-ACCT-STATUS
021916           WHEN '7'
021916              MOVE 'L'           TO EE-ACCT-STATUS
021916           WHEN '8'
021916              MOVE 'R'           TO EE-ACCT-STATUS
021916           WHEN '9'
021916              MOVE 'P'           TO EE-ACCT-STATUS
022808           WHEN OTHER
022808              MOVE 'A'           TO EE-ACCT-STATUS
022808        END-EVALUATE
102004     ELSE
102004        MOVE SPACES              TO EE-ACCT-STATUS
102004     END-IF
102004                                                                  
101504*    MOVE AM-REPORT-CODE-1       TO SAVE-REPORT-CODE-1
01746      IF AM-REPORT-CODE-1 = SPACES OR ZEROS OR LOW-VALUES          
01747          NEXT SENTENCE                                            
01748      ELSE                                                         
01749          MOVE '4'                TO EE-PASS-NO                    
01750          MOVE AM-REPORT-CODE-1   TO EE-A-RPT-CD-1                 
01751          MOVE S-EPX-CARR         TO EE-A-CARR                     
01752          MOVE S-EPX-GRP          TO EE-A-GROUP                    
CIDMOD*        MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 
CIDMOD         MOVE SPACES             TO EE-A-RPT-CD-2                 
01754          MOVE S-EPX-STATE        TO EE-A-STATE                    
01755          MOVE S-EPX-ACCT         TO EE-A-ACCT                     
01756          MOVE AM-NAME            TO EE-ACCT-NAME                  
051810         MOVE SPACES             TO EE-ACCT-CITY                  
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
01757          PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.               
01758                                                                   
01759      IF AM-REPORT-CODE-2 = SPACES OR ZEROS OR LOW-VALUES          
01760          NEXT SENTENCE                                            
01761      ELSE                                                         
01762          MOVE '5'                TO EE-PASS-NO                    
01763          MOVE LOW-VALUES         TO EE-A-RPT-CD-1                 
01764          MOVE S-EPX-CARR         TO EE-A-CARR                     
01765          MOVE S-EPX-GRP          TO EE-A-GROUP                    
01766          MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 
01767          MOVE S-EPX-STATE        TO EE-A-STATE                    
01768          MOVE S-EPX-ACCT         TO EE-A-ACCT                     
01769          MOVE AM-NAME            TO EE-ACCT-NAME                  
051810         MOVE SPACES             TO EE-ACCT-CITY                  
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
01770          PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.               
01771                                                                   
01772      MOVE ZERO                   TO EE-CNTL.                      
01773                                                                   
01774      MOVE '1'                    TO EE-PASS-NO.                   
01775      MOVE S-EPX-STATE            TO EE-CNTL-1.                    
01776      PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.                   
01777                                                                   
01778      MOVE '2'                    TO EE-PASS-NO.                   
01779      MOVE AM-GPCD                TO EE-CNTL-1.                    
01780      MOVE S-EPX-GRP              TO EE-GROUP.                     
01781      MOVE S-EPX-CARR             TO EE-CARR.                      
01782      PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.                   
01783                                                                   
01784      MOVE '3'                    TO EE-PASS-NO.                   
01785                                                                   
051810     MOVE SPACES                 TO EE-ACCT-CITY                  
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO EE-ACCT-CITY
051810     END-STRING
01786      MOVE AM-NAME                TO EE-ACCT-NAME.                 
01787                                                                   
01788      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                
01789      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                
01790                                                                   
01791      MOVE 1                      TO Z                             
01792                                     Z1.                           
01793                                                                   
01794  0420-B-E-LOOP-1.                                                 
01795      ADD 1 TO Z.                                                  
01796                                                                   
01797      IF Z GREATER 10                                              
01798          MOVE 0                  TO Z Z1                          
01799          GO TO 0430-B-E-LOOP-2.                                   
01800                                                                   
01801      IF (AM-COM-TYP (Z) IS EQUAL TO 'O' OR 'P')                   
01802          MOVE AM-AGT (Z)         TO B-E-AGT (Z1)                  
01803          ADD 1                   TO Z1.                           
01804                                                                   
01805      GO TO 0420-B-E-LOOP-1.                                       
01806                                                                   
01807  0430-B-E-LOOP-2.                                                 
01808      ADD 1 TO Z.                                                  
01809                                                                   
01810      IF Z GREATER 9                                               
01811          MOVE 0                  TO Z Z1                          
01812          GO TO 0450-B-E-LOOP-4.                                   
01813                                                                   
01814  0440-B-E-LOOP-3.                                                 
01815      ADD 1 TO Z1.                                                 
01816                                                                   
01817      IF Z1 GREATER 9                                              
01818          MOVE 0                  TO Z1                            
01819          GO TO 0430-B-E-LOOP-2.                                   
01820                                                                   
01821      IF Z = Z1                                                    
01822          GO TO 0440-B-E-LOOP-3.                                   
01823                                                                   
01824      IF B-E-AGT (Z) = B-E-AGT (Z1)                                
01825          MOVE SPACE              TO B-E-AGT (Z1).                 
01826                                                                   
01827      GO TO 0440-B-E-LOOP-3.                                       
01828                                                                   
01829  0450-B-E-LOOP-4.                                                 
01830      ADD 1 TO Z.                                                  
01831                                                                   
01832      IF DTE-CLIENT = 'HER'                                        
01833          IF Z GREATER 1                                           
01834              GO TO 0460-B-E-CHECK.                                
01835                                                                   
01836      IF Z GREATER 9                                               
01837          GO TO 0460-B-E-CHECK.                                    
01838                                                                   
01839      IF (B-E-AGT (Z) = SPACE OR ZERO OR LOW-VALUES)               
01840          GO TO 0450-B-E-LOOP-4.                                   
01841                                                                   
01842      MOVE B-E-AGT (Z)            TO EE-CNTL-GA.                   
01843      MOVE S-EPX-ACCT             TO EE-CNTL-ACCT.                 
01844                                                                   
01845      IF DTE-CLIENT = 'HER'  OR  'VSL'  OR  'MON' OR 'HSL'         
01846          PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.               
01847                                                                   
01848      MOVE HIGH-VALUE             TO EE-CNTL-ACCT.                 
01849      MOVE ZEROS                  TO EE-AM-EXPIRES                 
01850                                     EE-AM-HI-CERT.                
01851                                                                   
01852      PERFORM 0480-WRITE-EXTRACT THRU 0490-EXIT.                   
01853                                                                   
01854      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                
01855      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                
01856                                                                   
01857      GO TO 0450-B-E-LOOP-4.                                       
01858                                                                   
01859  0460-B-E-CHECK.                                                  
01860      IF X1 = 1                                                    
01861          MOVE 2                  TO X1                            
01862          GO TO 0410-B-E-RESUME.                                   
01863                                                                   
01864  0470-EXIT.                                                       
01865       EXIT.                                                       
01866                                                                   
01867  EJECT                                                            
01868  0480-WRITE-EXTRACT.                                              
01869                                                                   
01870      MOVE X1                   TO  SAVE-X1.                       
01871      MOVE +0                   TO  X1.                            
01872      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       
01873      PERFORM 0770-PRINT-DECISION THRU 0800-EXIT.                  
01874      IF P-ACC-SW IS EQUAL TO '1'                                  
01875          MOVE SAVE-X1          TO  X1                             
01876      ELSE                                                         
01877          MOVE SAVE-X1          TO  X1                             
01878          GO TO 0490-EXIT.                                         
01879                                                                   
01880      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           
01881         NEXT SENTENCE                                             
01882      ELSE                                                         
01883      IF EE-CERT = ZERO AND EE-LBEN = ZERO AND EE-LPRM = ZERO AND  
01884         EE-LCLM = ZERO AND EE-ABEN = ZERO AND EE-APRM = ZERO AND  
01885         EE-ACLM = ZERO AND EE-TPRM = ZERO AND EE-TCOM = ZERO      
01886         GO TO 0490-EXIT.                                          
01887                                                                   
01888      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           
01889         IF EE-PASS-NO EQUAL '3'                                   
01890             MOVE WS-ISS-FLAG         TO EE-ISS-FLAG.              
01891                                                                   
01892      WRITE EXTRACT-OT-REC FROM WS-EXTR-REC.                       
01893                                                                   
01894      MOVE SPACE                      TO EE-ISS-FLAG.              
01895                                                                   
01896  0490-EXIT.                                                       
01897       EXIT.                                                       
01898                                                                   
01899  0500-ADD-TO-OTHERS.                                              
01900      ADD CERT TO ST-CERT (Y1) GP-CERT (Y1) CA-CERT (Y1) GR-CERT   
01901                  GT-CERT (Y1).                                    
01902      ADD LBEN TO ST-LBEN (Y1) GP-LBEN (Y1) CA-LBEN (Y1) GR-LBEN   
01903                  GT-LBEN (Y1).                                    
01904      ADD LPRM TO ST-LPRM (Y1) GP-LPRM (Y1) CA-LPRM (Y1) GR-LPRM   
01905                  GT-LPRM (Y1).                                    
01906      ADD LCLM TO ST-LCLM (Y1) GP-LCLM (Y1) CA-LCLM (Y1) GR-LCLM   
01907                  GT-LCLM (Y1).                                    
01908      ADD ABEN TO ST-ABEN (Y1) GP-ABEN (Y1) CA-ABEN (Y1) GR-ABEN   
01909                  GT-ABEN (Y1).                                    
01910      ADD APRM TO ST-APRM (Y1) GP-APRM (Y1) CA-APRM (Y1) GR-APRM   
01911                  GT-APRM (Y1).                                    
01912      ADD ACLM TO ST-ACLM (Y1) GP-ACLM (Y1) CA-ACLM (Y1) GR-ACLM   
01913                  GT-ACLM (Y1).                                    
01914      ADD TPRM TO ST-TPRM (Y1) GP-TPRM (Y1) CA-TPRM (Y1) GR-TPRM   
01915                  GT-TPRM (Y1).                                    
01916      ADD TCOM TO ST-TCOM (Y1) GP-TCOM (Y1) CA-TCOM (Y1) GR-TCOM   
01917                  GT-TCOM (Y1).                                    
01918                                                                   
01919      IF COMPARE9DT (X1) GREATER THAN YEAR-OLD-DATE                
01920          ADD LBEN TO L12AC-LBEN L12ST-LBEN L12GP-LBEN L12CA-LBEN  
01921                      L12GD-LBEN                                   
01922          ADD LPRM TO L12AC-LPRM L12ST-LPRM L12GP-LPRM L12CA-LPRM  
01923                      L12GD-LPRM                                   
01924          ADD LCLM TO L12AC-LCLM L12ST-LCLM L12GP-LCLM L12CA-LCLM  
01925                      L12GD-LCLM                                   
01926          ADD ABEN TO L12AC-ABEN L12ST-ABEN L12GP-ABEN L12CA-ABEN  
01927                      L12GD-ABEN                                   
01928          ADD APRM TO L12AC-APRM L12ST-APRM L12GP-APRM L12CA-APRM  
01929                      L12GD-APRM                                   
01930          ADD ACLM TO L12AC-ACLM L12ST-ACLM L12GP-ACLM L12CA-ACLM  
01931                      L12GD-ACLM                                   
01932          ADD TPRM TO L12AC-TPRM L12ST-TPRM L12GP-TPRM L12CA-TPRM  
01933                      L12GD-TPRM                                   
01934          ADD TCOM TO L12AC-TCOM L12ST-TCOM L12GP-TCOM L12CA-TCOM  
01935                      L12GD-TCOM.                                  
01936                                                                   
01937      IF RUN-CCYY = COMP-CCYY (X1)                                 
01938          ADD LBEN TO YTDAC-LBEN YTDST-LBEN YTDGP-LBEN YTDCA-LBEN  
01939                      YTDGD-LBEN                                   
01940          ADD LPRM TO YTDAC-LPRM YTDST-LPRM YTDGP-LPRM YTDCA-LPRM  
01941                      YTDGD-LPRM                                   
01942                      YTDAC-TPRM YTDST-TPRM YTDGP-TPRM YTDCA-TPRM  
01943                      YTDGD-TPRM                                   
01944          ADD LCLM TO YTDAC-LCLM YTDST-LCLM YTDGP-LCLM YTDCA-LCLM  
01945                      YTDGD-LCLM                                   
01946          ADD TCOM TO YTDAC-TCOM YTDST-TCOM YTDGP-TCOM YTDCA-TCOM  
01947                      YTDGD-TCOM                                   
01948          ADD ABEN TO YTDAC-ABEN YTDST-ABEN YTDGP-ABEN YTDCA-ABEN  
01949                      YTDGD-ABEN                                   
01950          ADD APRM TO YTDAC-APRM YTDST-APRM YTDGP-APRM YTDCA-APRM  
01951                      YTDGD-APRM                                   
01952                      YTDAC-TPRM YTDST-TPRM YTDGP-TPRM YTDCA-TPRM  
01953                      YTDGD-TPRM                                   
01954          ADD ACLM TO YTDAC-ACLM YTDST-ACLM YTDGP-ACLM YTDCA-ACLM  
01955                      YTDGD-ACLM.                                  
01956                                                                   


           IF X1 = +13
              COMPUTE PYTDAC-LBEN = PYTDAC-LBEN +
                 (AC-LBEN (1) - AC-LBEN (15))
              COMPUTE PYTDAC-LPRM = PYTDAC-LPRM +
                 (AC-LPRM (1) - AC-LPRM (15))
              COMPUTE PYTDAC-LCLM = PYTDAC-LCLM +
                 (AC-LCLM (1) - AC-LCLM (15))
              COMPUTE PYTDAC-ABEN = PYTDAC-ABEN +
                 (AC-ABEN (1) - AC-ABEN (15))
              COMPUTE PYTDAC-APRM = PYTDAC-APRM +
                 (AC-APRM (1) - AC-APRM (15))
              COMPUTE PYTDAC-ACLM = PYTDAC-ACLM +
                 (AC-ACLM (1) - AC-ACLM (15))
              COMPUTE PYTDAC-TCOM = PYTDAC-TCOM +
                 (AC-TCOM (1) - AC-TCOM (15))
              COMPUTE PYTDAC-TPRM = PYTDAC-TPRM +
                 (AC-TPRM (1) - AC-TPRM (15))
              ADD PYTDAC-LBEN           TO PYTDST-LBEN
                                          PYTDGP-LBEN
                                          PYTDCA-LBEN
                                          PYTDGD-LBEN
              ADD PYTDAC-LPRM           TO PYTDST-LPRM
                                          PYTDGP-LPRM
                                          PYTDCA-LPRM
                                          PYTDGD-LPRM
              ADD PYTDAC-LCLM           TO PYTDST-LCLM
                                          PYTDGP-LCLM
                                          PYTDCA-LCLM
                                          PYTDGD-LCLM
              ADD PYTDAC-ABEN           TO PYTDST-ABEN
                                          PYTDGP-ABEN
                                          PYTDCA-ABEN
                                          PYTDGD-ABEN
              ADD PYTDAC-APRM           TO PYTDST-APRM
                                          PYTDGP-APRM
                                          PYTDCA-APRM
                                          PYTDGD-APRM
              ADD PYTDAC-ACLM           TO PYTDST-ACLM
                                          PYTDGP-ACLM
                                          PYTDCA-ACLM
                                          PYTDGD-ACLM
              ADD PYTDAC-TCOM           TO PYTDST-TCOM
                                          PYTDGP-TCOM
                                          PYTDCA-TCOM
                                          PYTDGD-TCOM
              ADD PYTDAC-TPRM           TO PYTDST-TPRM
                                          PYTDGP-TPRM
                                          PYTDCA-TPRM
                                          PYTDGD-TPRM
           END-IF




           .
01957  0510-EXIT.                                                       
01958       EXIT.                                                       
01959                                                                   
01960  0520-PRINT-LAST-AC-12.                                           
01961      MOVE L12AC-LBEN             TO DET-LBEN.                     
01962      MOVE L12AC-LPRM             TO DET-LPRM.                     
01963      MOVE L12AC-LCLM             TO DET-LCLM.                     
01964      MOVE L12AC-ABEN             TO DET-ABEN.                     
01965      MOVE L12AC-APRM             TO DET-APRM.                     
01966      MOVE L12AC-ACLM             TO DET-ACLM.                     
01967      MOVE L12AC-TPRM             TO DET-TPRM.                     
01968      MOVE L12AC-TCOM             TO DET-TCOM.                     
01969      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
01970                                                                   
01971 ****** AFL CODING **************                                  
01972                                                                   
01973      IF DTE-CLIENT = 'AFL'                                        
01974          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
01975                                                                   
01976 ****** END OF AFL CODING ***************                          
01977                                                                   
01978      MOVE ' '                    TO X.                            
01979      MOVE DETAIL-LINE            TO P-DATA.                       
01980      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
01981      MOVE ZERO-ACCUM             TO LAST-12-ACCOUNT-ACCUM.        
01982                                                                   
01983  0530-PRINT-AC-YTD.                                               
01984      MOVE YTDAC-LBEN             TO DET-LBEN.                     
01985      MOVE YTDAC-LPRM             TO DET-LPRM.                     
01986      MOVE YTDAC-LCLM             TO DET-LCLM.                     
01987      MOVE YTDAC-ABEN             TO DET-ABEN.                     
01988      MOVE YTDAC-APRM             TO DET-APRM.                     
01989      MOVE YTDAC-ACLM             TO DET-ACLM.                     
01990      MOVE YTDAC-TPRM             TO DET-TPRM.                     
01991      MOVE YTDAC-TCOM             TO DET-TCOM.                     
01992      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
01993      MOVE ' '                    TO X.                            
01994      MOVE DETAIL-LINE            TO P-DATA.                       
01995      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
01996      MOVE SPACES                 TO DET-TITLE.                    
01997      MOVE ZERO-ACCUM             TO YTD-ACCOUNT-ACCUM.            
01998                                                                   


122804 0530-PRINT-AC-PYTD.

122804     MOVE PYTDAC-LBEN            TO DET-LBEN
122804     MOVE PYTDAC-LPRM            TO DET-LPRM
122804     MOVE PYTDAC-LCLM            TO DET-LCLM
122804     MOVE PYTDAC-ABEN            TO DET-ABEN
122804     MOVE PYTDAC-APRM            TO DET-APRM
122804     MOVE PYTDAC-ACLM            TO DET-ACLM
122804     MOVE PYTDAC-TPRM            TO DET-TPRM
122804     MOVE PYTDAC-TCOM            TO DET-TCOM
122804     MOVE 'PRIOR YTD   '         TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 0920-WRITE-PRINT    THRU 0930-EXIT
122804     MOVE SPACES                 TO DET-TITLE
122804     MOVE ZERO-ACCUM             TO PYTD-ACCOUNT-ACCUM

01999      IF DTE-CLIENT  =  'MON'                                      
02000          NEXT SENTENCE                                            
02001      ELSE                                                         
02002          GO TO 0540-ZERO-ACCOUNT.                                 
02003                                                                   
02004      MOVE 'HIGH CERT DATE'       TO  DET-DATE-DESC.               
02005      MOVE SAVE-AM-HI-MO          TO  DET-DATE-MO.                 
02006      MOVE SAVE-AM-HI-DA          TO  DET-DATE-DA.                 
02007      MOVE SAVE-AM-HI-YR          TO  DET-DATE-YR.                 
02008      MOVE '/'                    TO  DET-DATE-SLASH-1             
02009                                      DET-DATE-SLASH-2.            
02010      MOVE ' '                    TO  X.                           
02011      MOVE DET-DATE-LINE          TO  P-DATA.                      
02012                                                                   
02013      PERFORM 0920-WRITE-PRINT  THRU  0930-EXIT.                   
02014                                                                   
02015      MOVE SPACES                 TO  DET-DATE-LINE.               
02016      MOVE ZEROS                  TO  SAVE-AM-HI-CERT-DATE.        
02017      MOVE 'EXPIRATION DATE'      TO  DET-DATE-DESC.               
02018      MOVE SAVE-AM-EXP-MO         TO  DET-DATE-MO.                 
02019      MOVE SAVE-AM-EXP-DA         TO  DET-DATE-DA.                 
02020      MOVE SAVE-AM-EXP-YR         TO  DET-DATE-YR.                 
02021      MOVE '/'                    TO  DET-DATE-SLASH-1             
02022                                      DET-DATE-SLASH-2.            
02023      MOVE ' '                    TO  X.                           
02024      MOVE DET-DATE-LINE          TO  P-DATA.                      
02025                                                                   
02026      PERFORM 0920-WRITE-PRINT  THRU  0930-EXIT.                   
02027                                                                   
02028      MOVE SPACES                 TO  DET-DATE-LINE.               
02029      MOVE ZEROS                  TO  SAVE-AM-EXPIRE-DT.           
02030                                                                   
02031  0540-ZERO-ACCOUNT.                                               
02032      MOVE +0                     TO X1.                           
02033      PERFORM 0150-ZERO-ACCUM-ACC THRU 0150-EXIT 15 TIMES.         
02034  EJECT                                                            
02035  0560-STATE-BREAK.                                                
02036      MOVE 0                     TO A1                             
02037      PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT                   
02038       VARYING X1 FROM 2 BY 1 UNTIL                                
02039        X1 IS GREATER THAN DATE-RANGE-MAX.                         
02040      MOVE SPACES                 TO  B-E-AGT-TABLE.               
02041      MOVE +0                     TO  X1.                          
02042      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
02043      PERFORM 0380-ACCOUNT-BREAK THRU 0540-ZERO-ACCOUNT.           
02044                                                                   
02045  0570-STATE-BREAK-1.                                              
02046      IF P-ST-SW = '1'                                             
02047          MOVE ' '                TO P-ST-SW                       
02048      ELSE                                                         
02049          GO TO 0610-ZERO-STATE.                                   
02050                                                                   
02051      PERFORM 0850-ST-HD          THRU 0850-EXIT.                  
02052      MOVE +1                     TO X1.                           
02053                                                                   
02054  0580-STATE-BREAK-2.                                              
02055      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
02056          GO TO 0590-PRINT-LAST-ST-12.                             
02057                                                                   
02058      MOVE ST-CERT (X1)           TO DET-CERTS.                    
02059      MOVE ST-LBEN (X1)           TO DET-LBEN.                     
02060      MOVE ST-LPRM (X1)           TO DET-LPRM.                     
02061      MOVE ST-LCLM (X1)           TO DET-LCLM.                     
02062      MOVE ST-ABEN (X1)           TO DET-ABEN.                     
02063      MOVE ST-APRM (X1)           TO DET-APRM.                     
02064      MOVE ST-ACLM (X1)           TO DET-ACLM.                     
02065      MOVE ST-TPRM (X1)           TO DET-TPRM.                     
02066      MOVE ST-TCOM (X1)           TO DET-TCOM.                     
02067      ADD +1                      TO X1.                           
02068      MOVE COMP-YR (X1)           TO DET-YR.                       
02069      MOVE COMP-MO (X1)           TO DET-MO.                       
02070      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
02071      MOVE DETAIL-LINE            TO P-DATA.                       
02072                                                                   
02073      IF X1 = 2                                                    
02074          MOVE '0'                TO X                             
02075      ELSE                                                         
02076          MOVE ' '                TO X.                            
02077                                                                   
02078      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02079      GO TO 0580-STATE-BREAK-2.                                    
02080                                                                   
02081  0590-PRINT-LAST-ST-12.                                           
02082      MOVE L12ST-LBEN             TO DET-LBEN.                     
02083      MOVE L12ST-LPRM             TO DET-LPRM.                     
02084      MOVE L12ST-LCLM             TO DET-LCLM.                     
02085      MOVE L12ST-ABEN             TO DET-ABEN.                     
02086      MOVE L12ST-APRM             TO DET-APRM.                     
02087      MOVE L12ST-ACLM             TO DET-ACLM.                     
02088      MOVE L12ST-TPRM             TO DET-TPRM.                     
02089      MOVE L12ST-TCOM             TO DET-TCOM.                     
02090      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
02091                                                                   
02092 ****** AFL CODING ****************                                
02093                                                                   
02094      IF DTE-CLIENT = 'AFL'                                        
02095          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
02096                                                                   
02097 ****** END OF AFL CODING *********                                
02098                                                                   
02099      MOVE '0'                    TO X.                            
02100      MOVE DETAIL-LINE            TO P-DATA.                       
02101      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02102      MOVE ZERO-ACCUM             TO LAST-12-STATE-ACCUM.          
02103                                                                   
02104  0600-PRINT-ST-YTD.                                               
02105      MOVE YTDST-LBEN             TO DET-LBEN.                     
02106      MOVE YTDST-LPRM             TO DET-LPRM.                     
02107      MOVE YTDST-LCLM             TO DET-LCLM.                     
02108      MOVE YTDST-ABEN             TO DET-ABEN.                     
02109      MOVE YTDST-APRM             TO DET-APRM.                     
02110      MOVE YTDST-ACLM             TO DET-ACLM.                     
02111      MOVE YTDST-TPRM             TO DET-TPRM.                     
02112      MOVE YTDST-TCOM             TO DET-TCOM.                     
02113      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
02114      MOVE ' '                    TO X.                            
02115      MOVE DETAIL-LINE            TO P-DATA.                       
02116      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02117      MOVE SPACES                 TO DET-TITLE.                    
02118      MOVE ZERO-ACCUM             TO YTD-STATE-ACCUM.              
02119                                                                   




122804 0600-PRINT-ST-PYTD.

122804     MOVE PYTDST-LBEN            TO DET-LBEN
122804     MOVE PYTDST-LPRM            TO DET-LPRM
122804     MOVE PYTDST-LCLM            TO DET-LCLM
122804     MOVE PYTDST-ABEN            TO DET-ABEN
122804     MOVE PYTDST-APRM            TO DET-APRM
122804     MOVE PYTDST-ACLM            TO DET-ACLM
122804     MOVE PYTDST-TPRM            TO DET-TPRM
122804     MOVE PYTDST-TCOM            TO DET-TCOM
122804     MOVE 'PRIOR YTD   '         TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 0920-WRITE-PRINT    THRU 0930-EXIT
122804     MOVE SPACES                 TO DET-TITLE
122804     MOVE ZERO-ACCUM             TO PYTD-STATE-ACCUM
122804                                                                  

           .
02120  0610-ZERO-STATE.                                                 
02121      MOVE +0                     TO X1.                           
02122      PERFORM 0160-ZERO-ACCUM-ST THRU 0160-EXIT 15 TIMES.          
02123  EJECT                                                            
02124  0630-GROUPING-BREAK.                                             
02125      MOVE 0                     TO A1                             
02126      PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT                   
02127       VARYING X1 FROM 2 BY 1 UNTIL                                
02128        X1 IS GREATER THAN DATE-RANGE-MAX.                         
02129      MOVE SPACES                 TO  B-E-AGT-TABLE.               
02130      MOVE +0                     TO  X1.                          
02131      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
02132      PERFORM 0380-ACCOUNT-BREAK THRU 0540-ZERO-ACCOUNT.           
02133      PERFORM 0570-STATE-BREAK-1 THRU 0610-ZERO-STATE.             
02134                                                                   
02135  0640-GROUPING-BREAK-1.                                           
02136      IF P-GRP-SW = '1'                                            
02137          MOVE ' '                TO P-GRP-SW                      
02138      ELSE                                                         
02139          GO TO 0680-ZERO-GROUPING.                                
02140                                                                   
02141      PERFORM 0860-GP-HD          THRU 0860-EXIT.                  
02142      MOVE +1                     TO X1.                           
02143                                                                   
02144  0650-GROUPING-BREAK-2.                                           
02145      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
02146          GO TO 0660-PRINT-LAST-GP-12.                             
02147                                                                   
02148      MOVE GP-CERT (X1)           TO DET-CERTS.                    
02149      MOVE GP-LBEN (X1)           TO DET-LBEN.                     
02150      MOVE GP-LPRM (X1)           TO DET-LPRM.                     
02151      MOVE GP-LCLM (X1)           TO DET-LCLM.                     
02152      MOVE GP-ABEN (X1)           TO DET-ABEN.                     
02153      MOVE GP-APRM (X1)           TO DET-APRM.                     
02154      MOVE GP-ACLM (X1)           TO DET-ACLM.                     
02155      MOVE GP-TPRM (X1)           TO DET-TPRM.                     
02156      MOVE GP-TCOM (X1)           TO DET-TCOM.                     
02157      ADD +1                      TO X1.                           
02158      MOVE COMP-YR (X1)           TO DET-YR.                       
02159      MOVE COMP-MO (X1)           TO DET-MO.                       
02160      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
02161      MOVE DETAIL-LINE            TO P-DATA.                       
02162                                                                   
02163      IF X1 = 2                                                    
02164          MOVE '0'                TO X                             
02165      ELSE                                                         
02166          MOVE ' '                TO X.                            
02167                                                                   
02168      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02169      GO TO 0650-GROUPING-BREAK-2.                                 
02170                                                                   
02171  0660-PRINT-LAST-GP-12.                                           
02172      MOVE L12GP-LBEN             TO DET-LBEN.                     
02173      MOVE L12GP-LPRM             TO DET-LPRM.                     
02174      MOVE L12GP-LCLM             TO DET-LCLM.                     
02175      MOVE L12GP-ABEN             TO DET-ABEN.                     
02176      MOVE L12GP-APRM             TO DET-APRM.                     
02177      MOVE L12GP-ACLM             TO DET-ACLM.                     
02178      MOVE L12GP-TPRM             TO DET-TPRM.                     
02179      MOVE L12GP-TCOM             TO DET-TCOM.                     
02180      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
02181                                                                   
02182 ****** AFL CODING ***************                                 
02183                                                                   
02184      IF DTE-CLIENT = 'AFL'                                        
02185          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
02186                                                                   
02187 ****** END OF AFL CODING *********                                
02188                                                                   
02189      MOVE '0'                    TO X.                            
02190      MOVE DETAIL-LINE            TO P-DATA.                       
02191      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02192      MOVE ZERO-ACCUM             TO LAST-12-GROUPING-ACCUM.       
02193                                                                   
02194  0670-PRINT-GP-YTD.                                               
02195      MOVE YTDGP-LBEN             TO DET-LBEN.                     
02196      MOVE YTDGP-LPRM             TO DET-LPRM.                     
02197      MOVE YTDGP-LCLM             TO DET-LCLM.                     
02198      MOVE YTDGP-ABEN             TO DET-ABEN.                     
02199      MOVE YTDGP-APRM             TO DET-APRM.                     
02200      MOVE YTDGP-ACLM             TO DET-ACLM.                     
02201      MOVE YTDGP-TPRM             TO DET-TPRM.                     
02202      MOVE YTDGP-TCOM             TO DET-TCOM.                     
02203      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
02204      MOVE ' '                    TO X.                            
02205      MOVE DETAIL-LINE            TO P-DATA.                       
02206      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02207      MOVE SPACES                 TO DET-TITLE.                    
02208      MOVE ZERO-ACCUM             TO YTD-GROUPING-ACCUM.           
02209                                                                   

122804 0670-PRINT-GP-PYTD.

122804     MOVE PYTDGP-LBEN            TO DET-LBEN
122804     MOVE PYTDGP-LPRM            TO DET-LPRM
122804     MOVE PYTDGP-LCLM            TO DET-LCLM
122804     MOVE PYTDGP-ABEN            TO DET-ABEN
122804     MOVE PYTDGP-APRM            TO DET-APRM
122804     MOVE PYTDGP-ACLM            TO DET-ACLM
122804     MOVE PYTDGP-TPRM            TO DET-TPRM
122804     MOVE PYTDGP-TCOM            TO DET-TCOM
122804     MOVE 'PRIOR YTD   '         TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 0920-WRITE-PRINT    THRU 0930-EXIT
122804     MOVE SPACES                 TO DET-TITLE
122804     MOVE ZERO-ACCUM             TO PYTD-GROUPING-ACCUM
122804                                                                  
           .
02210  0680-ZERO-GROUPING.                                              
02211      MOVE +0                     TO X1.                           
02212      PERFORM 0170-ZERO-ACCUM-GRP THRU 0170-EXIT 15 TIMES.         
02213  EJECT                                                            
02214  0700-CARRIER-BREAK.                                              
02215      MOVE 0                     TO A1                             
02216      PERFORM 0400-BUILD-EXTRACTS THRU 0470-EXIT                   
02217       VARYING X1 FROM 2 BY 1 UNTIL                                
02218        X1 IS GREATER THAN DATE-RANGE-MAX.                         
02219      MOVE SPACES                 TO  B-E-AGT-TABLE.               
02220      MOVE +0                     TO  X1.                          
02221      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
02222      PERFORM 0380-ACCOUNT-BREAK    THRU 0540-ZERO-ACCOUNT.        
02223      PERFORM 0570-STATE-BREAK-1    THRU 0610-ZERO-STATE.          
02224      PERFORM 0640-GROUPING-BREAK-1 THRU 0680-ZERO-GROUPING.       
02225                                                                   
02226  0710-CARRIER-BREAK-1.                                            
02227      IF P-CA-SW = '1'                                             
02228          MOVE ' '                TO P-CA-SW                       
02229      ELSE                                                         
02230          GO TO 0750-ZERO-CARRIER.                                 
02231                                                                   
02232      PERFORM 0870-CA-HD          THRU 0870-EXIT.                  
02233      MOVE +1                     TO X1.                           
02234                                                                   
02235  0720-CARRIER-BREAK-2.                                            
02236      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
02237          GO TO 0730-PRINT-LAST-CA-12.                             
02238                                                                   
02239      MOVE CA-CERT (X1)           TO DET-CERTS.                    
02240      MOVE CA-LBEN (X1)           TO DET-LBEN.                     
02241      MOVE CA-LPRM (X1)           TO DET-LPRM.                     
02242      MOVE CA-LCLM (X1)           TO DET-LCLM.                     
02243      MOVE CA-ABEN (X1)           TO DET-ABEN.                     
02244      MOVE CA-APRM (X1)           TO DET-APRM.                     
02245      MOVE CA-ACLM (X1)           TO DET-ACLM.                     
02246      MOVE CA-TPRM (X1)           TO DET-TPRM.                     
02247      MOVE CA-TCOM (X1)           TO DET-TCOM.                     
02248      ADD +1                      TO X1.                           
02249      MOVE COMP-YR (X1)           TO DET-YR.                       
02250      MOVE COMP-MO (X1)           TO DET-MO.                       
02251      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
02252      MOVE DETAIL-LINE            TO P-DATA.                       
02253                                                                   
02254      IF X1 = 2                                                    
02255          MOVE '0'                TO X                             
02256      ELSE                                                         
02257          MOVE ' '                TO X.                            
02258                                                                   
02259      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02260      GO TO 0720-CARRIER-BREAK-2.                                  
02261                                                                   
02262  0730-PRINT-LAST-CA-12.                                           
02263      MOVE L12CA-LBEN             TO DET-LBEN.                     
02264      MOVE L12CA-LPRM             TO DET-LPRM.                     
02265      MOVE L12CA-LCLM             TO DET-LCLM.                     
02266      MOVE L12CA-ABEN             TO DET-ABEN.                     
02267      MOVE L12CA-APRM             TO DET-APRM.                     
02268      MOVE L12CA-ACLM             TO DET-ACLM.                     
02269      MOVE L12CA-TPRM             TO DET-TPRM.                     
02270      MOVE L12CA-TCOM             TO DET-TCOM.                     
02271      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
02272                                                                   
02273 ****** AFL CODING ***************                                 
02274                                                                   
02275      IF DTE-CLIENT = 'AFL'                                        
02276          MOVE 'FISCAL TO DATE'   TO DET-TITLE.                    
02277                                                                   
02278 ****** END OF AFL CODING ********                                 
02279                                                                   
02280      MOVE '0'                    TO X.                            
02281      MOVE DETAIL-LINE            TO P-DATA.                       
02282      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02283      MOVE ZERO-ACCUM             TO LAST-12-CARRIER-ACCUM.        
02284                                                                   
02285  0740-PRINT-CA-YTD.                                               
02286      MOVE YTDCA-LBEN             TO DET-LBEN.                     
02287      MOVE YTDCA-LPRM             TO DET-LPRM.                     
02288      MOVE YTDCA-LCLM             TO DET-LCLM.                     
02289      MOVE YTDCA-ABEN             TO DET-ABEN.                     
02290      MOVE YTDCA-APRM             TO DET-APRM.                     
02291      MOVE YTDCA-ACLM             TO DET-ACLM.                     
02292      MOVE YTDCA-TPRM             TO DET-TPRM.                     
02293      MOVE YTDCA-TCOM             TO DET-TCOM.                     
02294      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
02295      MOVE ' '                    TO X.                            
02296      MOVE DETAIL-LINE            TO P-DATA.                       
02297      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02298      MOVE SPACES                 TO DET-TITLE.                    
02299      MOVE ZERO-ACCUM             TO YTD-CARRIER-ACCUM.            
02300                                                                   



122804 0740-PRINT-CA-PYTD.

122804     MOVE PYTDCA-LBEN            TO DET-LBEN
122804     MOVE PYTDCA-LPRM            TO DET-LPRM
122804     MOVE PYTDCA-LCLM            TO DET-LCLM
122804     MOVE PYTDCA-ABEN            TO DET-ABEN
122804     MOVE PYTDCA-APRM            TO DET-APRM
122804     MOVE PYTDCA-ACLM            TO DET-ACLM
122804     MOVE PYTDCA-TPRM            TO DET-TPRM
122804     MOVE PYTDCA-TCOM            TO DET-TCOM
122804     MOVE 'PRIOR YTD   '         TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 0920-WRITE-PRINT THRU 0930-EXIT
122804     MOVE SPACES                 TO DET-TITLE
122804     MOVE ZERO-ACCUM             TO PYTD-CARRIER-ACCUM
122804                                                                  
           .
02301  0750-ZERO-CARRIER.                                               
02302      MOVE +0                     TO X1.                           
02303      PERFORM 0180-ZERO-ACCUM-CARR THRU 0180-EXIT 15 TIMES.        
02304  EJECT                                                            
02305  0770-PRINT-DECISION.                                             
02306      MOVE +1                     TO X1.                           
02307      MOVE +0                     TO Y1.                           
02308                                                                   
02309  0780-PRINT-DECISION-1.                                           
02310      ADD 1 TO X1 Y1.                                              
02311                                                                   
02312      IF X1 GREATER THAN DATE-RANGE-MAX                            
02313          MOVE +0                 TO X1                            
02314          GO TO 0790-PRINT-DECISION-2.                             
02315                                                                   
02316                                                                   
02317      COMPUTE LBEN = AC-LBEN (X1) - AC-LBEN (Y1).                  
02318      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  
02319      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  
02320      COMPUTE ABEN = AC-ABEN (X1) - AC-ABEN (Y1).                  
02321      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  
02322      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  
02323      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  
02324                                                                   
02325      IF COMP-CCYY (X1) = CONV-CCYY AND                            
02326         COMP-MO (X1) = CONV-MO                                    
02327           MOVE ZEROS TO LBEN LPRM LCLM ABEN APRM ACLM TCOM.       
02328                                                                   
02329      MOVE LBEN                   TO PLBEN (Y1).                   
02330      MOVE LPRM                   TO PLPRM (Y1).                   
02331      MOVE LCLM                   TO PLCLM (Y1).                   
02332      MOVE ABEN                   TO PABEN (Y1).                   
02333      MOVE APRM                   TO PAPRM (Y1).                   
02334      MOVE ACLM                   TO PACLM (Y1).                   
02335      MOVE TCOM                   TO PTCOM (Y1).                   
02336      GO TO 0780-PRINT-DECISION-1.                                 
02337                                                                   
02338  0790-PRINT-DECISION-2.                                           
02339      ADD +1 TO X1.                                                
02340                                                                   
02341      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
02342          GO TO 0800-EXIT.                                         
02343                                                                   
02344      IF (PRINT-ZERO-TABLE NOT = PRINT-TABLE (X1))
010306        OR ((AC-LBEN (1) - AC-LBEN (15)) NOT = ZEROS)
010306        OR ((AC-ABEN (1) - AC-ABEN (15)) NOT = ZEROS)
010306        OR ((AC-LPRM (1) - AC-LPRM (15)) NOT = ZEROS)
010306        OR ((AC-APRM (1) - AC-APRM (15)) NOT = ZEROS)
010306        OR ((AC-LCLM (1) - AC-LCLM (15)) NOT = ZEROS)
010306        OR ((AC-ACLM (1) - AC-ACLM (15)) NOT = ZEROS)
02345          MOVE '1' TO P-ACC-SW P-ST-SW P-GRP-SW P-CA-SW
02346          GO TO 0800-EXIT.                                         
02347                                                                   
02348      GO TO 0790-PRINT-DECISION-2.                                 
02349                                                                   
02350  0800-EXIT.                                                       
02351       EXIT.                                                       
02352                                                                   
02353  EJECT                                                            
02354  0810-ACC-HD.                                                     
02355      IF DTE-PGM-OPT NOT = 2                                       
02356          GO TO 0820-ACC-HD-A.                                     
02357                                                                   
02358      IF (DTE-CLIENT EQUAL 'TMS'                                   
02359         OR DTE-FMT-OPT EQUAL '2')                                 
02360          GO TO 0820-ACC-HD-A.                                     
02361                                                                   
02362      IF SET-CTR = +1                                              
02363          MOVE SPACES             TO P-DATA                        
02364          MOVE '-'                TO X                             
02365          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02366          MOVE SPACES             TO P-DATA                        
02367          MOVE '-'                TO X                             
02368          PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02369          MOVE ZERO               TO SET-CTR                       
02370          MOVE ' '                TO X                             
02371          GO TO 0830-ACC-HD-B.                                     
02372                                                                   
02373      MOVE +1                     TO SET-CTR.                      
02374                                                                   
02375  0810-EXIT.                                                       
02376  EJECT                                                            
02377  0820-ACC-HD-A.                                                   
02378      ADD +1 TO PAGE-CNT.                                          
02379      MOVE PAGE-CNT               TO HD-PAGE.                      
02380      MOVE '1'                    TO X.                            
02381      MOVE HD-1-ACCOUNT           TO P-DATA.                       
02382      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02383      MOVE ' '                    TO X.                            
02384      MOVE HD-2                   TO P-DATA.                       
02385      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02386      MOVE ' '                    TO X.                            
02387      MOVE HD-3                   TO P-DATA.                       
02388      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804     MOVE '0'                    TO X.                            
02390                                                                   
02391  0820-EXIT.                                                       
02392  EJECT                                                            
02393  0830-ACC-HD-B.                                                   
02394                                                                   
02395      MOVE S-EPX-ACCT             TO HD-ACCOUNT.                   
02396      MOVE HD-5                   TO W-WORK-LINE                   
02397      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02398      MOVE W-WORK-LINE            TO P-DATA.                       
02399      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02400                                                                   
02401      MOVE ' '                    TO X.                            
02402      MOVE S-EPX-STATE            TO STATE-L.                      
02403      PERFORM 1000-STATE-PRT THRU 1010-EXIT.                       
02404      MOVE STATE-ABBR (CLAS-INDEXS)                                
02405                                  TO HD-STATE-ABBR.                
02406      MOVE STATE-PIC (CLAS-INDEXS)                                 
02407                                  TO HD-STATE.                     
02408      MOVE '       IN '           TO HD6-TOTAL-FOR.                
02409      MOVE HD-6                   TO W-WORK-LINE.                  
02410      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT.                   
02411      MOVE W-WORK-LINE            TO P-DATA.                       
02412      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02413                                                                   
02414      MOVE ' '                    TO X.                            
02415      MOVE S-EPX-GRP              TO HD-GROUPING.                  
02416      MOVE '       IN '           TO HD4-TOTAL-FOR.                
02417      MOVE HD-4                   TO P-DATA.                       
02418      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02419                                                                   
02420      MOVE ' '                    TO X.                            
02421      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        
02422      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02423      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02424      MOVE '       IN '           TO HD7-TOTAL-FOR.                
02425      MOVE HD-7                   TO W-WORK-LINE                   
02426      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02427      MOVE W-WORK-LINE            TO P-DATA.                       
02428      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02429                                                                   
122804     MOVE '0'                    TO X.                            
02431      IF (DTE-CLIENT EQUAL 'TMS'                                   
02432         OR DTE-FMT-OPT EQUAL '2')                                 
122804*        MOVE SPACES             TO P-DATA                        
122804*        PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02435          MOVE HD-TMS-9           TO P-DATA                        
02436      ELSE                                                         
02437          MOVE HD-9               TO P-DATA.                       
02438      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02439                                                                   
02440      MOVE ' '                    TO X.                            
02441      IF (DTE-CLIENT EQUAL 'TMS'                                   
02442         OR DTE-FMT-OPT EQUAL '2')                                 
02443          MOVE HD-TMS-10          TO P-DATA                        
02444      ELSE                                                         
02445          MOVE HD-10              TO P-DATA.                       
02446      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02447                                                                   
02448  0840-EXIT.                                                       
02449      EXIT.                                                        
02450  EJECT                                                            
02451  0850-ST-HD.                                                      
02452                                                                   
02453      MOVE ZERO                   TO SET-CTR.                      
02454      ADD +1 TO PAGE-CNT.                                          
02455      MOVE PAGE-CNT               TO HD-PAGE.                      
02456      MOVE '1'                    TO X.                            
02457      MOVE HD-1-STATE             TO P-DATA.                       
02458      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02459                                                                   
02460      MOVE ' '                    TO X.                            
02461      MOVE HD-2                   TO P-DATA.                       
02462      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02463      MOVE ' '                    TO X.                            
02464      MOVE HD-3                   TO P-DATA.                       
02465      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02466                                                                   
122804     MOVE '0'                    TO X.                            
02468      MOVE S-EPX-STATE            TO STATE-L.                      
02469      PERFORM 1000-STATE-PRT THRU 1010-EXIT.                       
02470      MOVE STATE-ABBR (CLAS-INDEXS)                                
02471                                  TO HD-STATE-ABBR.                
02472      MOVE STATE-PIC (CLAS-INDEXS)                                 
02473                                  TO HD-STATE.                     
02474      MOVE 'TOTAL FOR '           TO HD6-TOTAL-FOR.                
02475      MOVE HD-6                   TO W-WORK-LINE                   
02476      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02477      MOVE W-WORK-LINE            TO P-DATA.                       
02478      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02479                                                                   
02480      MOVE ' '                    TO X.                            
02481      MOVE S-EPX-GRP              TO HD-GROUPING.                  
02482      MOVE '       IN '           TO HD4-TOTAL-FOR.                
02483      MOVE HD-4                   TO P-DATA.                       
02484      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02485                                                                   
02486      MOVE ' '                    TO X.                            
02487      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        
02488      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02489      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02490      MOVE '       IN '           TO HD7-TOTAL-FOR.                
02491      MOVE HD-7                   TO W-WORK-LINE                   
02492      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02493      MOVE W-WORK-LINE            TO P-DATA.                       
02494      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02495                                                                   
122804     MOVE '0'                    TO X.                            
02497      IF (DTE-CLIENT EQUAL 'TMS'                                   
02498         OR DTE-FMT-OPT EQUAL '2')                                 
122804*        MOVE SPACES             TO P-DATA                        
122804*        PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02501          MOVE HD-TMS-9           TO P-DATA                        
02502      ELSE                                                         
02503          MOVE HD-9               TO P-DATA.                       
02504      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02505      MOVE ' '                    TO X.                            
02506      IF (DTE-CLIENT EQUAL 'TMS'                                   
02507         OR DTE-FMT-OPT EQUAL '2')                                 
02508          MOVE HD-TMS-10          TO P-DATA                        
02509      ELSE                                                         
02510          MOVE HD-10              TO P-DATA.                       
02511      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02512  0850-EXIT.                                                       
02513  EJECT                                                            
02514  0860-GP-HD.                                                      
02515                                                                   
02516      ADD +1 TO PAGE-CNT.                                          
02517      MOVE PAGE-CNT               TO HD-PAGE.                      
02518      MOVE '1'                    TO X.                            
02519      MOVE HD-1-GROUPING          TO P-DATA.                       
02520                                                                   
02521      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02522      MOVE ' '                    TO X.                            
02523      MOVE HD-2                   TO P-DATA.                       
02524                                                                   
02525      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02526      MOVE ' '                    TO X.                            
02527      MOVE HD-3                   TO P-DATA.                       
02528      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02529                                                                   
122804     MOVE '0'                    TO X.                            
02531      MOVE S-EPX-GRP              TO HD-GROUPING.                  
02532      MOVE 'TOTAL FOR '           TO HD4-TOTAL-FOR.                
02533      MOVE HD-4                   TO W-WORK-LINE                   
02534      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02535      MOVE W-WORK-LINE            TO P-DATA.                       
02536      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02537                                                                   
02538      MOVE ' '                    TO X.                            
02539      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        
02540      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02541      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02542      MOVE '       IN '           TO HD7-TOTAL-FOR.                
02543      MOVE HD-7                   TO W-WORK-LINE                   
02544      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02545      MOVE W-WORK-LINE            TO P-DATA.                       
02546      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02547                                                                   
122804     MOVE '0'                    TO X.                            
02549      IF (DTE-CLIENT EQUAL 'TMS'                                   
02550         OR DTE-FMT-OPT EQUAL '2')                                 
122804*        MOVE SPACES             TO P-DATA                        
122804*        PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02553          MOVE HD-TMS-9           TO P-DATA                        
02554      ELSE                                                         
02555          MOVE HD-9               TO P-DATA.                       
02556      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02557                                                                   
02558      MOVE ' '                    TO X.                            
02559      IF (DTE-CLIENT EQUAL 'TMS'                                   
02560         OR DTE-FMT-OPT EQUAL '2')                                 
02561          MOVE HD-TMS-10          TO P-DATA                        
02562      ELSE                                                         
02563          MOVE HD-10              TO P-DATA.                       
02564      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02565  0860-EXIT.                                                       
02566  EJECT                                                            
02567                                                                   
02568  0870-CA-HD.                                                      
02569                                                                   
02570      ADD +1 TO PAGE-CNT.                                          
02571      MOVE PAGE-CNT               TO HD-PAGE.                      
02572      MOVE '1'                    TO X.                            
02573      MOVE HD-1-CARRIER           TO P-DATA.                       
02574      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02575                                                                   
02576      MOVE ' '                    TO X.                            
02577      MOVE HD-2                   TO P-DATA.                       
02578      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02579                                                                   
02580      MOVE ' '                    TO X.                            
02581      MOVE HD-3                   TO P-DATA.                       
02582      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02583                                                                   
122804     MOVE '0'                    TO X.                            
02585      MOVE S-EPX-CARR             TO HD-CARRIER, CARRIER-L.        
02586      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02587      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02588      MOVE 'TOTAL FOR '           TO HD7-TOTAL-FOR.                
02589      MOVE HD-7                   TO W-WORK-LINE                   
02590      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02591      MOVE W-WORK-LINE            TO P-DATA.                       
02592      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02593                                                                   
122804     MOVE '0'                    TO X.                            
02595      IF (DTE-CLIENT EQUAL 'TMS'                                   
02596         OR DTE-FMT-OPT EQUAL '2')                                 
122804*        MOVE SPACES             TO P-DATA                        
122804*        PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02599          MOVE HD-TMS-9           TO P-DATA                        
02600      ELSE                                                         
02601          MOVE HD-9               TO P-DATA.                       
02602      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02603                                                                   
02604      MOVE ' '                    TO X.                            
02605      IF (DTE-CLIENT EQUAL 'TMS'                                   
02606         OR DTE-FMT-OPT EQUAL '2')                                 
02607          MOVE HD-TMS-10          TO P-DATA                        
02608      ELSE                                                         
02609          MOVE HD-10              TO P-DATA.                       
02610      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02611  0870-EXIT.                                                       
02612  EJECT                                                            
02613                                                                   
02614  0880-GR-HD.                                                      
02615      ADD +1 TO PAGE-CNT.                                          
02616      MOVE PAGE-CNT               TO HD-PAGE.                      
02617      MOVE '1'                    TO X.                            
02618      MOVE HD-1-GRAND-TOTALS      TO P-DATA.                       
02619      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02620      MOVE ' '                    TO X.                            
02621      MOVE HD-2                   TO P-DATA.                       
02622      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02623      MOVE ' '                    TO X.                            
02624      MOVE HD-3                   TO P-DATA.                       
02625      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804     MOVE '0'                    TO X.                            
02627      MOVE HD-8                   TO P-DATA.                       
02628      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804     MOVE '0'                    TO X.                            
02630      MOVE SPACES                 TO HD-9-1 HD-10-1.               
02631      IF (DTE-CLIENT EQUAL 'TMS'                                   
02632         OR DTE-FMT-OPT EQUAL '2')                                 
122804*        MOVE SPACES             TO P-DATA                        
122804*        PERFORM 0920-WRITE-PRINT THRU 0930-EXIT                  
02635          MOVE HD-TMS-9           TO P-DATA                        
02636      ELSE                                                         
02637          MOVE HD-9               TO P-DATA.                       
02638      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02639      MOVE ' '                    TO X.                            
02640      IF (DTE-CLIENT EQUAL 'TMS'                                   
02641         OR DTE-FMT-OPT EQUAL '2')                                 
02642          MOVE HD-TMS-10          TO P-DATA                        
02643      ELSE                                                         
02644          MOVE HD-10              TO P-DATA.                       
02645      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02646  0880-EXIT.                                                       
02647                                  EJECT                            
02648  0890-REMOVE-SPACES.                                              
02649                                                                   
02650      MOVE W-WORK-AREA-2          TO W-WORKING-LINE.               
02651      MOVE SPACES                 TO W-WORK-AREA-2.                
02652      MOVE ZEROS                  TO W-WA2-NDX.                    
02653                                                                   
02654      PERFORM 0892-TRANSFER-DATA THRU 0892-EXIT                    
02655              VARYING                                              
02656          W-WL-NDX FROM 1 BY 1                                     
02657              UNTIL                                                
02658          W-WL-NDX GREATER THAN +86.                               
02659                                                                   
02660  0890-EXIT.                                                       
02661      EXIT.                                                        
02662                                                                   
02663  0892-TRANSFER-DATA.                                              
02664                                                                   
02665      IF  W-WL-CHAR (W-WL-NDX) EQUAL SPACES                        
02666              AND                                                  
02667          W-WA2-CHAR (W-WA2-NDX) EQUAL SPACES                      
02668          NEXT SENTENCE                                            
02669                                                                   
02670      ELSE                                                         
02671          IF  W-WL-CHAR (W-WL-NDX) EQUAL ')'                       
02672                  AND                                              
02673              W-WA2-CHAR (W-WA2-NDX) EQUAL SPACES                  
02674              MOVE W-WL-CHAR (W-WL-NDX)                            
02675                                  TO W-WA2-CHAR (W-WA2-NDX)        
02676                                                                   
02677          ELSE                                                     
02678              ADD +1              TO W-WA2-NDX                     
02679              MOVE W-WL-CHAR (W-WL-NDX)                            
02680                                  TO W-WA2-CHAR (W-WA2-NDX).       
02681                                                                   
02682  0892-EXIT.                                                       
02683      EXIT.                                                        
02684                                  EJECT                            
02685  0900-MATCH-ACCT-MASTER.                                          
TSTMOD                                                                  
TSTMOD     MOVE EP-COMPANY-CD          TO AM-COMPANY-CD.                
TSTMOD     MOVE EP-CONTROL             TO AM-MSTR-CNTRL.                
02688                                                                   
02689      READ AM-MAST-IN.                                             
02690                                                                   
02658      IF ERACCT-FILE-STATUS = '00'                                 
TSTMOD         GO TO 2700-MOVE.                                         
TSTMOD                                                                  
TSTMOD     IF  AM-MSTR-CNTRL       =  SAVE-PRIMARY                      
TSTMOD         GO TO 0909-MATCH-EXIT.                                   
TSTMOD                                                                  
TSTMOD     MOVE  AM-MSTR-CNTRL       TO  SAVE-PRIMARY.                  
TSTMOD                                                                  
TSTMOD         ADD  +1  TO  ERROR-COUNT                                 
TSTMOD         MOVE 'NO ACCT MSTR FOR EXTRACT    ' TO DIS-LINE-REASON   
TSTMOD         MOVE EP-CONTROL                     TO  DIS-LINE-REC     
TSTMOD         PERFORM 8600-DISPLAY-PRT THRU                            
TSTMOD               8600-DISPLAY-EXIT                                  
TSTMOD         MOVE 'EXP DATE              = ' TO DIS-LINE-REASON       
TSTMOD         MOVE EP-EXP-DTE                  TO DIS-LINE-REC         
TSTMOD         PERFORM 8600-DISPLAY-PRT THRU                            
TSTMOD               8600-DISPLAY-EXIT                                  
TSTMOD         MOVE 'EFF DATE              = ' TO DIS-LINE-REASON       
TSTMOD         MOVE EP-EFF-DTE                 TO DIS-LINE-REC          
TSTMOD         PERFORM 8600-DISPLAY-PRT THRU                            
TSTMOD               8600-DISPLAY-EXIT                                  
TSTMOD                                                                  
TSTMOD         MOVE SPACES                     TO DIS-LINE-REASON       
TSTMOD         MOVE SPACES                     TO DIS-LINE-REC          
TSTMOD         PERFORM 8600-DISPLAY-PRT THRU                            
TSTMOD               8600-DISPLAY-EXIT                                  
TSTMOD                                                                  
TSTMOD         DISPLAY 'NO ACCOUNT MASTER FOR THIS EXTRACT ' EP-CONTROL 
TSTMOD                 ' EXP DATE ' EP-EXP-DTE ' EFF DATE ' EP-EFF-DTE  
TSTMOD         GO TO 0909-MATCH-EXIT.                                   
TSTMOD                                                                  
TSTMOD 2700-MOVE.                                                       
02711                                                                   
02712      MOVE AM-NAME                TO HD-ACCT-NAME                  
02713                                     SAVE-ACCT-NAME.               
051810     MOVE SPACES                 TO HD-ACCT-ADDRESS.              
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO HD-ACCT-ADDRESS
051810     END-STRING

TSTMOD     MOVE AM-EFFECT-DT           TO ACCT-EFF-DT.                  
TSTMOD     MOVE AM-MSTR-CNTRL          TO ACCT-CONTROL.                 
           MOVE AM-REPORT-CODE-1       TO SAVE-REPORT-CODE-1
           MOVE SPACES                 TO HD-1-RPT-CD1
           .
02715                                                                   
02716  0909-MATCH-EXIT.                                                 
02717      EXIT.                                                        
02718                                                                   
02719  EJECT                                                            
02720  0920-WRITE-PRINT.                                                

101504     MOVE PRT                  TO WS-RPTCDE1-PRT
           MOVE X                    TO WS-RPTCDE1-CTL
           MOVE SAVE-EPX             TO WS-RPTCDE1-ACT-KEY
      *    MOVE EP-CNTRL-1           TO WS-RPTCDE1-ACT-KEY
           MOVE SPACES               TO WS-RPTCDE1-RPTCDE1
      *    MOVE SAVE-REPORT-CODE-1   TO WS-RPTCDE1-RPTCDE1
           IF WS-RPTCDE1-CTL = '1'
              MOVE ' '               TO PRT-RPTCDE1-SW
      *       MOVE SAVE-REPORT-CODE-1
      *                              TO WS-RPTCDE1-PRT (5:10)
              IF P-DATA = HD-1-ACCOUNT
                 SET PRT-RPTCDE1     TO TRUE
              END-IF
           END-IF
           IF PRT-RPTCDE1
              WRITE RPTCDE1-RECORD      FROM WS-RPTCDE1-RECORD
              ADD 1                     TO WS-RPTCDE1-LINE-NO
           END-IF

02721                              COPY ELCPRT2.                        
CIDMOD**                                                                
CIDMOD******************************************************************
CIDMOD**                                                                
CIDMOD** PRINT COPY MODULE HARD COPIED TO ALLOW CHGS THAT WILL PRINT    
CIDMOD**  ONLY FINAL TOTALS ON PAPER, BUT RETAIN THE ENTIRE REPORT OM   
CIDMOD**  FICH (PER CID REQUEST #913240174).                            
CIDMOD**                                                                
CIDMOD******************************************************************
CIDMOD**                                                               *
CIDMOD**                                                               *
CIDMOD**                           ELCPRT2                             *
CIDMOD**          COPYBOOK REVIEWED FOR YEAR 2000 COMPLIANCE            
CIDMOD**                           VMOD=2.002                          *
CIDMOD**                                                               *
CIDMOD******************************************************************
CIDMOD**                                                                
CIDMOD**   IF DTE-FICH NOT = SPACE AND                                  
CIDMOD**      FICH-OPEN    = SPACE                                      
CIDMOD**       MOVE 'X'                TO  FICH-OPEN                    
CIDMOD**       OPEN OUTPUT FICH.                                        
CIDMOD**                                                                
CIDMOD**   IF DTE-FICH NOT = SPACE                                      
CIDMOD**       MOVE X                  TO  P-CTL                        
CIDMOD**       WRITE FICH-REC FROM PRT.                                 
CIDMOD**                                                                
CIDMOD**   IF PRNT-TOT-SW = 'Y'                                         
CIDMOD**       NEXT SENTENCE                                            
CIDMOD**    ELSE                                                        
CIDMOD**       GO  TO  0930-EXIT.                                       
CIDMOD**                                                                
CIDMOD**   IF DTE-FICH = SPACE OR '2'                                   
CIDMOD**     MOVE X                    TO  P-CTL.                       
CIDMOD**                                                                
CIDMOD**   IF P-CTL = ' '                                               
CIDMOD**       WRITE PRT AFTER ADVANCING 1 LINE                         
CIDMOD**       GO TO 0930-EXIT.                                         
CIDMOD**                                                                
CIDMOD**   IF P-CTL = '0'                                               
CIDMOD**       WRITE PRT AFTER ADVANCING 2 LINES                        
CIDMOD**       GO TO 0930-EXIT.                                         
CIDMOD**                                                                
CIDMOD**   IF P-CTL = '-'                                               
CIDMOD**       WRITE PRT AFTER ADVANCING 3 LINES                        
CIDMOD**       GO TO 0930-EXIT.                                         
CIDMOD**                                                                
CIDMOD**   WRITE PRT AFTER ADVANCING PAGE                               
CIDMOD**       GO TO 0930-EXIT.                                         
CIDMOD**                                                                
CIDMOD******************************************************************
CIDMOD*                                                                 
02722  0930-EXIT.                                                       
02723      EXIT.                                                        
02724                                                                   
02725  EJECT                                                            
02726  0940-LOAD-ALPHA-MONTH.                                           
02727      IF DET-MO = '01 ' MOVE 'JAN' TO DET-MO                       
02728                                      DET-TMS-MO.                  
02729      IF DET-MO = '02 ' MOVE 'FEB' TO DET-MO                       
02730                                      DET-TMS-MO.                  
02731      IF DET-MO = '03 ' MOVE 'MAR' TO DET-MO                       
02732                                      DET-TMS-MO.                  
02733      IF DET-MO = '04 ' MOVE 'APR' TO DET-MO                       
02734                                      DET-TMS-MO.                  
02735      IF DET-MO = '05 ' MOVE 'MAY' TO DET-MO                       
02736                                      DET-TMS-MO.                  
02737      IF DET-MO = '06 ' MOVE 'JUN' TO DET-MO                       
02738                                      DET-TMS-MO.                  
02739      IF DET-MO = '07 ' MOVE 'JUL' TO DET-MO                       
02740                                      DET-TMS-MO.                  
02741      IF DET-MO = '08 ' MOVE 'AUG' TO DET-MO                       
02742                                      DET-TMS-MO.                  
02743      IF DET-MO = '09 ' MOVE 'SEP' TO DET-MO                       
02744                                      DET-TMS-MO.                  
02745      IF DET-MO = '10 ' MOVE 'OCT' TO DET-MO                       
02746                                      DET-TMS-MO.                  
02747      IF DET-MO = '11 ' MOVE 'NOV' TO DET-MO                       
02748                                      DET-TMS-MO.                  
02749      IF DET-MO = '12 ' MOVE 'DEC' TO DET-MO                       
02750                                      DET-TMS-MO.                  
02751                                                                   
02752  0950-EXIT.                                                       
02753       EXIT.                                                       
02754  EJECT                                                            
02755  0960-PRINT-GRAND-TOTALS.                                         
CIDMOD
CIDMOD     MOVE  'Y'  TO  PRNT-TOT-SW.
CIDMOD
02756      IF DTE-FICH = '1'                                            
02757          MOVE '2'                TO DTE-FICH.                     
02758                                                                   
02759      PERFORM 0880-GR-HD          THRU 0880-EXIT.                  
02760      MOVE +1                     TO X1.                           
02761                                                                   
02762  0970-GRAND-TOTAL-1.                                              
02763      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
02764          GO TO 0980-GRAND-TOTAL-2.                                
02765                                                                   
02766      MOVE GT-LBEN (X1)           TO DET-LBEN.                     
02767      MOVE GT-LPRM (X1)           TO DET-LPRM.                     
02768                                                                   
02769      IF X1 = 12                                                   
02770          MOVE GT-LPRM (12)       TO ME-035-NET-L                  
02771          MOVE GT-APRM (12)       TO ME-035-NET-AH.                
02772                                                                   
02773      MOVE GT-LCLM (X1)           TO DET-LCLM.                     
02774      MOVE GT-ABEN (X1)           TO DET-ABEN.                     
02775      MOVE GT-APRM (X1)           TO DET-APRM.                     
02776      MOVE GT-ACLM (X1)           TO DET-ACLM.                     
02777      MOVE GT-TPRM (X1)           TO DET-TPRM.                     
02778      MOVE GT-TCOM (X1)           TO DET-TCOM.                     
02779      ADD +1 TO X1.                                                
02780      MOVE COMP-YR (X1)           TO DET-YR.                       
02781      MOVE COMP-MO (X1)           TO DET-MO.                       
02782      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
02783      MOVE DETAIL-LINE            TO P-DATA.                       
02784                                                                   
02785      IF X1 = 2                                                    
02786          MOVE '0'                TO X                             
02787      ELSE                                                         
02788          MOVE ' '                TO X.                            
02789                                                                   
02790      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02791      GO TO 0970-GRAND-TOTAL-1.                                    
02792                                                                   
02793  0980-GRAND-TOTAL-2.                                              
02794      MOVE 'LAST 12 MONTHS'       TO DET-TITLE.                    
02795      MOVE '-'                    TO X.                            
02796      MOVE L12GD-LBEN             TO DET-LBEN.                     
02797      MOVE L12GD-LPRM             TO DET-LPRM.                     
02798      MOVE L12GD-LCLM             TO DET-LCLM.                     
02799      MOVE L12GD-ABEN             TO DET-ABEN.                     
02800      MOVE L12GD-APRM             TO DET-APRM.                     
02801      MOVE L12GD-ACLM             TO DET-ACLM.                     
02802      MOVE L12GD-TPRM             TO DET-TPRM.                     
02803      MOVE L12GD-TCOM             TO DET-TCOM.                     
02804      MOVE DETAIL-LINE            TO P-DATA.                       
02805      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02806      MOVE 'YEAR TO DATE'         TO DET-TITLE.                    
02807      MOVE ' '                    TO X.                            
02808      MOVE YTDGD-LBEN             TO DET-LBEN.                     
02809      MOVE YTDGD-LPRM             TO DET-LPRM.                     
02810      MOVE YTDGD-LCLM             TO DET-LCLM.                     
02811      MOVE YTDGD-ABEN             TO DET-ABEN.                     
02812      MOVE YTDGD-APRM             TO DET-APRM.                     
02813      MOVE YTDGD-ACLM             TO DET-ACLM.                     
02814      MOVE YTDGD-TPRM             TO DET-TPRM.                     
02815      MOVE YTDGD-TCOM             TO DET-TCOM.                     
02816      MOVE DETAIL-LINE            TO P-DATA.                       
02817      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02818                                                                   

122804     MOVE 'PRIOR YTD   '         TO DET-TITLE
122804     MOVE ' '                    TO X
122804     MOVE PYTDGD-LBEN            TO DET-LBEN
122804     MOVE PYTDGD-LPRM            TO DET-LPRM
122804     MOVE PYTDGD-LCLM            TO DET-LCLM
122804     MOVE PYTDGD-ABEN            TO DET-ABEN
122804     MOVE PYTDGD-APRM            TO DET-APRM
122804     MOVE PYTDGD-ACLM            TO DET-ACLM
122804     MOVE PYTDGD-TPRM            TO DET-TPRM
122804     MOVE PYTDGD-TCOM            TO DET-TCOM
122804     MOVE DETAIL-LINE            TO P-DATA
122804     PERFORM 0920-WRITE-PRINT    THRU 0930-EXIT
122804                                                                  

           .
02819  0990-EXIT.                                                       
02820      EXIT.                                                        
02821  EJECT                                                            
02822  1000-STATE-PRT.                                                  
02823                              COPY ECSSTLOK.                       
02824  1010-EXIT.                                                       
02825      EXIT.                                                        
02826  EJECT                                                            
02827  1100-CARRIER-PRT.                                                
02828      MOVE ZERO               TO CLAS-INDEXCN.                     
02829                                                                   
02830  1110-CARRIER-LOOP.                                               
02831      ADD 1 TO CLAS-INDEXCN.                                       
02832                                                                   
02833      IF CLAS-INDEXCN IS GREATER THAN CLAS-MAXCN                   
02834          MOVE SPACE          TO CARRIER-L                         
02835          GO TO 1190-EXIT.                                         
02836                                                                   
02837      IF CARRIER-L NOT = CARRIER-SUB (CLAS-INDEXCN)                
02838          GO TO 1110-CARRIER-LOOP.                                 
02839                                                                   
02840  1190-EXIT.                                                       
02841      EXIT.                                                        
02842  EJECT                                                            
02843 **********************************************************        
02844 *  THIS SECTION CONTAINS SPECIAL CODE FOR TOYATA.        *        
02845 * A REQUEST WAS MADE IN FEBRUARY 1990 TO REPORT THE      *        
02846 * FOLLOWING INFORMATION USING GROSS DATA VS NET DATA :   *        
02847 *  ISSUE COUNTS,                                         *        
02848 *  CANCEL COUNTS,                                        *        
02849 *  LIFE AND AH PREMIUM AMTS                              *        
02850 *  LIFE AND AH CANCEL PREMIUM AMTS                       *        
02851 *  LIFE ANC AH CLAIM AMTS                                *        
02852 *                                                        *        
02853 *  A REQUEST WAS ALSO MADE TO KEEP THE ECS036 REPORT     *        
02854 *  'AS IS' SO THE EXTRACT CREATED FROM ECS035 NEEDED     *        
02855 *  TO REMAIN ON THE 'NET' BASIS.                         *        
02856 *                                                        *        
02857 **********************************************************        
02858                                                                   
02859  2380-ACCOUNT-BREAK.                                              
02860      MOVE +0                     TO X1.                           
02861      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       
02862      PERFORM 2770-PRINT-DECISION THRU 2800-EXIT.                  
02863                                                                   
02864      IF P-ACC-SW = '1'                                            
02865          MOVE ' '                TO P-ACC-SW                      
02866      ELSE                                                         
02867          GO TO 2540-ZERO-ACCOUNT.                                 
02868                                                                   
02869      PERFORM 0810-ACC-HD  THRU  0840-EXIT.                        
02870      MOVE +1                     TO X1.                           
02871      MOVE +0                     TO Y1.                           
02872                                                                   
02873  2390-ACCOUNT-BREAK-PRINT.                                        
02874      ADD 1                       TO X1 Y1.                        
02875                                                                   
02876 *    IF X1 GREATER THAN DATE-RANGE-MAX                            
02877 *        GO TO 2520-PRINT-LAST-AC-12.                             
02878                                                                   
122804     IF X1 > +13
122804        GO TO 2520-PRINT-LAST-AC-12
122804     END-IF

02879      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
02880                                     DETAIL-TMS-LINE-2.            
02881                                                                   
02882      COMPUTE CERT-T   = AC-CERT-T (X1)   - AC-CERT-T (Y1).        
02883      COMPUTE CANCEL-T = AC-CANCEL-T (X1) - AC-CANCEL-T (Y1).      
02884      COMPUTE L-COVERAGE-T = AC-L-COVERAGE-T (X1)  -               
02885                              AC-L-COVERAGE-T (Y1).                
02886      COMPUTE A-COVERAGE-T = AC-A-COVERAGE-T (X1)  -               
02887                              AC-A-COVERAGE-T (Y1).                
02888      COMPUTE LPRM-T   = AC-LPRM-T (X1)   - AC-LPRM-T (Y1).        
02889      COMPUTE LCAN-T   = AC-LCAN-T (X1)   - AC-LCAN-T (Y1).        
02890      COMPUTE LCLM-T   = AC-LCLM-T (X1)   - AC-LCLM-T (Y1).        
02891      COMPUTE APRM-T   = AC-APRM-T (X1)   - AC-APRM-T (Y1).        
02892      COMPUTE ACAN-T   = AC-ACAN-T (X1)   - AC-ACAN-T (Y1).        
02893      COMPUTE ACLM-T   = AC-ACLM-T (X1)   - AC-ACLM-T (Y1).        
02894      COMPUTE TCOM-T   = AC-TCOM-T (X1)   - AC-TCOM-T (Y1).        
02895      COMPUTE TPRM-T   = (LPRM-T + APRM-T) -                       
02896             (LCAN-T + ACAN-T).                                    
02897      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   
02898      MOVE COMP-MO (X1)           TO DET-MO                        
02899                                     DET-TMS-MO.                   
02900      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
02901      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
02902      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
02903      MOVE CERT-T                 TO DET-TMS-CERTS.                
02904      MOVE CANCEL-T               TO DET-TMS-CANCELS.              
02905      MOVE L-COVERAGE-T           TO DET-TMS-LCOVERAG.             
02906      MOVE LPRM-T                 TO DET-TMS-LPRM.                 
02907      MOVE LCAN-T                 TO DET-TMS-LCAN.                 
02908      MOVE LCLM-T                 TO DET-TMS-LCLM.                 
02909      MOVE APRM-T                 TO DET-TMS-APRM.                 
02910      MOVE ACAN-T                 TO DET-TMS-ACAN.                 
02911      MOVE A-COVERAGE-T           TO DET-TMS-ACOVERAG.             
02912      MOVE ACLM-T                 TO DET-TMS-ACLM.                 
02913      MOVE TPRM-T                 TO DET-TMS-TPRM.                 
02914      MOVE TCOM-T                 TO DET-TMS-TCOM.                 
02915                                                                   
02916      COMPUTE DET-TMS-NPRM = (TPRM-T - TCOM-T).                    
02917                                                                   
02918      IF COMP-CCYY (X1) = CONV-CCYY  AND  COMP-MO (X1) = CONV-MO   
02919          MOVE ZEROS TO DET-TMS-CERTS                              
02920                        DET-TMS-LCOVERAG                           
02921                        DET-TMS-ACOVERAG                           
02922                        DET-TMS-LPRM                               
02923                        DET-TMS-LCAN                               
02924                        DET-TMS-LCLM                               
02925                        DET-TMS-APRM                               
02926                        DET-TMS-ACAN                               
02927                        DET-TMS-ACLM                               
02928                        DET-TMS-TPRM                               
02929                        DET-TMS-TCOM                               
02930      ELSE                                                         
02931          PERFORM 2500-ADD-TO-OTHERS  THRU 2510-EXIT.              
02932                                                                   
02933 *    PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT.                  
02934                                                                   
02935      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
02936                                                                   
CIDMOD*    MOVE '0'                    TO X.                            
CIDMOD     MOVE ' '                    TO X.                            
02938                                                                   
02939      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02940                                                                   
02941      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
02942                                                                   
02943      MOVE ' '                    TO X.                            
02944                                                                   
02945      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02946      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
02947                                                                   
02948      MOVE ' '                    TO X.                            
02949                                                                   
02950      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
02951                                                                   
02952      GO TO 2390-ACCOUNT-BREAK-PRINT.                              
02953  EJECT                                                            
02954  2400-BUILD-EXTRACTS.                                             

           IF DISP-REC
              DISPLAY ' ENTERING 2400 BUILD EXTRACT '
           END-IF
           
02955      IF X1 = 2                                                    
02956          MOVE 1                  TO X1.                           
02957                                                                   
02958      ADD 1                       TO A1.                           
02959  2410-B-E-RESUME.                                                 
02960                                                                   
010306     IF X1 = 14
010306        IF (COMP-CCYY (X1) = COMP-CCYY (15))
010306           AND (COMP-MO (X1) = COMP-MO (15))
010306           MOVE 15               TO X1
010306        END-IF
010306     END-IF
02961      MOVE ZEROS                  TO WS-EE-DTE.                    
02962      MOVE COMP-CCYY (X1)         TO EE-CCYY.                      
02963      MOVE COMP-MO (X1)           TO EE-MO.                        
02964      MOVE WS-EE-DTE              TO EE-DTE.                       
02965                                                                   
02966      MOVE SPACE                  TO WS-ISS-FLAG.                  
02967                                                                   
02968      IF DTE-CLIENT EQUAL 'HER'  OR 'HSL'                          
02969          COMPUTE WS-ISS-CNT = DR-ISS (X1) - DR-ISS (A1)           
02970          IF WS-ISS-CNT GREATER THAN ZEROS                         
02971              MOVE 'Y'            TO WS-ISS-FLAG.                  
02972                                                                   
02973      COMPUTE EE-CERT = DR-CERT (X1).                              
02974      COMPUTE EE-LBEN = DR-LBEN (X1).                              
02975      COMPUTE EE-LPRM = DR-LPRM (X1).                              
02976      COMPUTE EE-LCLM = DR-LCLM (X1).                              
02977      COMPUTE EE-ABEN = DR-ABEN (X1).                              
02978      COMPUTE EE-APRM = DR-APRM (X1).                              
02979      COMPUTE EE-ACLM = DR-ACLM (X1).                              
02980      COMPUTE EE-TPRM = EE-LPRM + EE-APRM.                         
02981      COMPUTE EE-TCOM = DR-TCOM (X1).                              
02982                                                                   
02983      COMPUTE EE-ISS-CNT = DR-ISS (X1).                            
02984                                                                   
02985      MOVE AC-DATE (X1)           TO  EE-MTH-HI-CERT.              
02986      MOVE ZERO                   TO EE-CNTL.                      
02987      MOVE SPACES                 TO EE-ACCT-NAME.                 
CIDMOD     MOVE SPACES                 TO EE-ACCT-CITY.                 

102004     IF DTE-CLIENT EQUAL 'HER' OR 'HSL' OR 'CID' OR 'DCC'
030612       OR 'AHL'
022808        EVALUATE AM-STATUS
022808           WHEN '1'
022808              MOVE 'I'           TO EE-ACCT-STATUS
022808           WHEN '2'
022808              MOVE 'T'           TO EE-ACCT-STATUS
022808           WHEN '3'
022808              MOVE 'C'           TO EE-ACCT-STATUS
022808           WHEN '4'
022808              MOVE 'I'           TO EE-ACCT-STATUS
031811           WHEN '5'
031811              MOVE 'S'           TO EE-ACCT-STATUS
021916           WHEN '6'
021916              MOVE 'D'           TO EE-ACCT-STATUS
021916           WHEN '7'
021916              MOVE 'L'           TO EE-ACCT-STATUS
021916           WHEN '8'
021916              MOVE 'R'           TO EE-ACCT-STATUS
021916           WHEN '9'
021916              MOVE 'P'           TO EE-ACCT-STATUS
022808           WHEN OTHER
022808              MOVE 'A'           TO EE-ACCT-STATUS
022808        END-EVALUATE
102004     ELSE
102004        MOVE SPACES              TO EE-ACCT-STATUS
102004     END-IF
102004                                                                  
02999                                                                   
03000      IF AM-REPORT-CODE-1 = SPACES OR ZEROS OR LOW-VALUES          
03001          NEXT SENTENCE                                            
03002      ELSE                                                         
03003          MOVE '4'                TO EE-PASS-NO                    
03004          MOVE AM-REPORT-CODE-1   TO EE-A-RPT-CD-1                 
03005          MOVE S-EPX-CARR         TO EE-A-CARR                     
03006          MOVE S-EPX-GRP          TO EE-A-GROUP                    
CIDMOD*        MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 
CIDMOD         MOVE SPACES             TO EE-A-RPT-CD-2                 
03008          MOVE S-EPX-STATE        TO EE-A-STATE                    
03009          MOVE S-EPX-ACCT         TO EE-A-ACCT                     
03010          MOVE AM-NAME            TO EE-ACCT-NAME                  
051810         MOVE SPACES             TO EE-ACCT-CITY                  
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
03011          PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.               
03012                                                                   
03013      IF AM-REPORT-CODE-2 = SPACES OR ZEROS OR LOW-VALUES          
03014          NEXT SENTENCE                                            
03015      ELSE                                                         
03016          MOVE '5'                TO EE-PASS-NO                    
03017          MOVE LOW-VALUES         TO EE-A-RPT-CD-1                 
03018          MOVE S-EPX-CARR         TO EE-A-CARR                     
03019          MOVE S-EPX-GRP          TO EE-A-GROUP                    
03020          MOVE AM-REPORT-CODE-2   TO EE-A-RPT-CD-2                 
03021          MOVE S-EPX-STATE        TO EE-A-STATE                    
03022          MOVE S-EPX-ACCT         TO EE-A-ACCT                     
03023          MOVE AM-NAME            TO EE-ACCT-NAME                  
051810         MOVE SPACES             TO EE-ACCT-CITY                  
051810         STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810            DELIMITED BY '  ' INTO EE-ACCT-CITY
051810         END-STRING
03024          PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.               
03025                                                                   
03026      MOVE ZERO                   TO EE-CNTL.                      
03027                                                                   
03028      MOVE '1'                    TO EE-PASS-NO.                   
03029      MOVE S-EPX-STATE            TO EE-CNTL-1.                    
03030      PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.                   
03031                                                                   
03032      MOVE '2'                    TO EE-PASS-NO.                   
03033      MOVE AM-GPCD                TO EE-CNTL-1.                    
03034      MOVE S-EPX-GRP              TO EE-GROUP.                     
03035      MOVE S-EPX-CARR             TO EE-CARR.                      
03036      PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.                   
03037                                                                   
03038      MOVE '3'                    TO EE-PASS-NO.                   
03039                                                                   
051810     MOVE SPACES                 TO EE-ACCT-CITY                  
051810     STRING AM-ADDR-CITY ' ' AM-ADDR-STATE
051810        DELIMITED BY '  ' INTO EE-ACCT-CITY
051810     END-STRING
03040      MOVE AM-NAME                TO EE-ACCT-NAME.                 
03041                                                                   
03042      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                
03043      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                
03044                                                                   
03045      MOVE 1                      TO Z                             
03046                                     Z1.                           
03047                                                                   
03048  2420-B-E-LOOP-1.                                                 
03049      ADD 1                       TO Z.                            
03050                                                                   
03051      IF Z GREATER 10                                              
03052          MOVE 0                  TO Z Z1                          
03053          GO TO 2430-B-E-LOOP-2.                                   
03054                                                                   
03055      IF (AM-COM-TYP (Z) IS EQUAL TO 'O' OR 'P')                   
03056          MOVE AM-AGT (Z)         TO B-E-AGT (Z1)                  
03057          ADD 1                   TO Z1.                           
03058                                                                   
03059      GO TO 2420-B-E-LOOP-1.                                       
03060                                                                   
03061  2430-B-E-LOOP-2.                                                 
03062      ADD 1                       TO Z.                            
03063                                                                   
03064      IF Z GREATER 9                                               
03065          MOVE 0                  TO Z Z1                          
03066          GO TO 2450-B-E-LOOP-4.                                   
03067                                                                   
03068  2440-B-E-LOOP-3.                                                 
03069      ADD 1                       TO Z1.                           
03070                                                                   
03071      IF Z1 GREATER 9                                              
03072          MOVE 0                  TO Z1                            
03073          GO TO 2430-B-E-LOOP-2.                                   
03074                                                                   
03075      IF Z = Z1                                                    
03076          GO TO 2440-B-E-LOOP-3.                                   
03077                                                                   
03078      IF B-E-AGT (Z) = B-E-AGT (Z1)                                
03079          MOVE SPACE              TO B-E-AGT (Z1).                 
03080                                                                   
03081      GO TO 2440-B-E-LOOP-3.                                       
03082                                                                   
03083  2450-B-E-LOOP-4.                                                 
03084      ADD 1                       TO Z.                            
03085                                                                   
03086      IF DTE-CLIENT = 'HER'                                        
03087          IF Z GREATER 1                                           
03088              GO TO 2460-B-E-CHECK.                                
03089                                                                   
03090      IF Z GREATER 9                                               
03091          GO TO 2460-B-E-CHECK.                                    
03092                                                                   
03093      IF (B-E-AGT (Z) = SPACE OR ZERO OR LOW-VALUES)               
03094          GO TO 2450-B-E-LOOP-4.                                   
03095                                                                   
03096      MOVE B-E-AGT (Z)            TO EE-CNTL-GA.                   
03097      MOVE S-EPX-ACCT             TO EE-CNTL-ACCT.                 
03098                                                                   
03099      IF DTE-CLIENT = 'HER'  OR  'VSL'  OR  'MON'  OR 'HSL'        
03100          PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.               
03101                                                                   
03102      MOVE HIGH-VALUE             TO EE-CNTL-ACCT.                 
03103      MOVE ZEROS                  TO EE-AM-EXPIRES                 
03104                                     EE-AM-HI-CERT.                
03105                                                                   
03106      PERFORM 2480-WRITE-EXTRACT THRU 2490-EXIT.                   
03107                                                                   
03108      MOVE SAVE-AM-EXPIRE-DT      TO EE-AM-EXPIRES.                
03109      MOVE SAVE-AM-HI-CERT-DATE   TO EE-AM-HI-CERT.                
03110                                                                   
03111      GO TO 2450-B-E-LOOP-4.                                       
03112                                                                   
03113  2460-B-E-CHECK.                                                  
03114      IF X1 = 1                                                    
03115          MOVE 2                  TO X1                            
03116          GO TO 2410-B-E-RESUME.                                   
03117                                                                   
03118  2470-EXIT.                                                       
03119       EXIT.                                                       
03120                                                                   
03121  EJECT                                                            
03122  2480-WRITE-EXTRACT.                                              
03123                                                                   
      *    IF EP-EXP-DTE = 19900601
PEMTST*       DISPLAY 'MADE IT TO 2480 BUILD '
      *    END-IF

03124      MOVE X1                   TO  SAVE-X1.                       
03125      MOVE +0                   TO  X1.                            
03126      PERFORM 0140-ZERO-PRINT-TABLE THRU 0140-EXIT 12 TIMES.       
03127      PERFORM 2770-PRINT-DECISION THRU 2800-EXIT.                  
PEMTST*    IF EP-EXP-DTE = 19900601
PEMTST*       DISPLAY ' P-ACC-SW = ' P-ACC-SW
PEMTST*    END-IF
03128      IF P-ACC-SW IS EQUAL TO '1'                                  
03129          MOVE SAVE-X1          TO  X1                             
03130      ELSE                                                         
03131          MOVE SAVE-X1          TO  X1                             
03132          GO TO 2490-EXIT.                                         
03133                                                                   
03134      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           
03135         NEXT SENTENCE                                             
03136      ELSE                                                         
03137      IF EE-CERT = ZERO AND EE-LBEN = ZERO AND EE-LPRM = ZERO AND  
03138         EE-LCLM = ZERO AND EE-ABEN = ZERO AND EE-APRM = ZERO AND  
03139         EE-ACLM = ZERO AND EE-TPRM = ZERO AND EE-TCOM = ZERO      
03140         GO TO 2490-EXIT.                                          
03141                                                                   
      *    IF EP-EXP-DTE = 19900601
      *       DISPLAY 'MADE IT TO 2480 BUILD AFTER ZERO'
      *    END-IF
03142      IF DTE-CLIENT EQUAL 'HER' OR 'HSL'                           
03143         IF EE-PASS-NO EQUAL '3'                                   
03144             MOVE WS-ISS-FLAG         TO EE-ISS-FLAG.              
03145                                                                   
03146      WRITE EXTRACT-OT-REC FROM WS-EXTR-REC.                       
           IF DISP-REC
              DISPLAY ' EXECUTED WRITE '
              MOVE EE-LPRM             TO WS-DISP-AMT
              MOVE EE-DTE              TO WS-DISP-DTE
              DISPLAY ' EXTR ' WS-DISP-DTE ' LPRM ' WS-DISP-AMT
           END-IF
03147                                                                   
03148      MOVE SPACE                      TO EE-ISS-FLAG.              
03149                                                                   
03150  2490-EXIT.                                                       
03151       EXIT.                                                       
03152                                                                   
03153  2500-ADD-TO-OTHERS.                                              
03154                                                                   
03155      ADD CERT-T                TO ST-CERT-T (Y1)                  
03156                                   GP-CERT-T (Y1)                  
03157                                   CA-CERT-T (Y1)                  
03158                                   GR-CERT-T                       
03159                                   GT-CERT-T (Y1).                 
03160      ADD CANCEL-T              TO ST-CANCEL-T (Y1)                
03161                                   GP-CANCEL-T (Y1)                
03162                                   CA-CANCEL-T (Y1)                
03163                                   GR-CANCEL-T                     
03164                                   GT-CANCEL-T (Y1).               
03165      ADD L-COVERAGE-T          TO ST-L-COVERAGE-T (Y1)            
03166                                   GP-L-COVERAGE-T (Y1)            
03167                                   CA-L-COVERAGE-T (Y1)            
03168                                   GR-L-COVERAGE-T                 
03169                                   GT-L-COVERAGE-T (Y1).           
03170                                                                   
03171      ADD A-COVERAGE-T          TO ST-A-COVERAGE-T (Y1)            
03172                                   GP-A-COVERAGE-T (Y1)            
03173                                   CA-A-COVERAGE-T (Y1)            
03174                                   GR-A-COVERAGE-T                 
03175                                   GT-A-COVERAGE-T (Y1).           
03176                                                                   
03177      ADD LPRM-T                TO ST-LPRM-T (Y1)                  
03178                                   GP-LPRM-T (Y1)                  
03179                                   CA-LPRM-T (Y1)                  
03180                                   GR-LPRM-T                       
03181                                   GT-LPRM-T (Y1).                 
03182      ADD LCAN-T                TO ST-LCAN-T (Y1)                  
03183                                   GP-LCAN-T (Y1)                  
03184                                   CA-LCAN-T (Y1)                  
03185                                   GR-LCAN-T                       
03186                                   GT-LCAN-T (Y1).                 
03187      ADD LCLM-T                TO ST-LCLM-T (Y1)                  
03188                                   GP-LCLM-T (Y1)                  
03189                                   CA-LCLM-T (Y1)                  
03190                                   GR-LCLM-T                       
03191                                   GT-LCLM-T (Y1).                 
03192      ADD APRM-T                TO ST-APRM-T (Y1)                  
03193                                   GP-APRM-T (Y1)                  
03194                                   CA-APRM-T (Y1)                  
03195                                   GR-APRM-T                       
03196                                   GT-APRM-T (Y1).                 
03197      ADD ACAN-T                TO ST-ACAN-T (Y1)                  
03198                                   GP-ACAN-T (Y1)                  
03199                                   CA-ACAN-T (Y1)                  
03200                                   GR-ACAN-T                       
03201                                   GT-ACAN-T (Y1).                 
03202      ADD ACLM-T                TO ST-ACLM-T (Y1)                  
03203                                   GP-ACLM-T (Y1)                  
03204                                   CA-ACLM-T (Y1)                  
03205                                   GR-ACLM-T                       
03206                                   GT-ACLM-T (Y1).                 
03207      ADD TPRM-T                TO ST-TPRM-T (Y1)                  
03208                                   GP-TPRM-T (Y1)                  
03209                                   CA-TPRM-T (Y1)                  
03210                                   GR-TPRM-T                       
03211                                   GT-TPRM-T (Y1).                 
03212      ADD TCOM-T                TO ST-TCOM-T (Y1)                  
03213                                   GP-TCOM-T (Y1)                  
03214                                   CA-TCOM-T (Y1)                  
03215                                   GR-TCOM-T                       
03216                                   GT-TCOM-T (Y1).                 
03217                                                                   
03218      IF COMPARE9DT (X1) GREATER THAN YEAR-OLD-DATE                
03219          ADD CERT-T            TO L12AC-CERT-T                    
03220                                   L12ST-CERT-T                    
03221                                   L12GP-CERT-T                    
03222                                   L12CA-CERT-T                    
03223                                   L12GD-CERT-T                    
03224          ADD CANCEL-T          TO L12AC-CANCEL-T                  
03225                                   L12ST-CANCEL-T                  
03226                                   L12GP-CANCEL-T                  
03227                                   L12CA-CANCEL-T                  
03228                                   L12GD-CANCEL-T                  
03229          ADD L-COVERAGE-T      TO L12AC-L-COVERAGE-T              
03230                                   L12ST-L-COVERAGE-T              
03231                                   L12GP-L-COVERAGE-T              
03232                                   L12CA-L-COVERAGE-T              
03233                                   L12GD-L-COVERAGE-T              
03234          ADD A-COVERAGE-T      TO L12AC-A-COVERAGE-T              
03235                                   L12ST-A-COVERAGE-T              
03236                                   L12GP-A-COVERAGE-T              
03237                                   L12CA-A-COVERAGE-T              
03238                                   L12GD-A-COVERAGE-T              
03239          ADD LPRM-T            TO L12AC-LPRM-T                    
03240                                   L12ST-LPRM-T                    
03241                                   L12GP-LPRM-T                    
03242                                   L12CA-LPRM-T                    
03243                                   L12GD-LPRM-T                    
03244          ADD LCAN-T            TO L12AC-LCAN-T                    
03245                                   L12ST-LCAN-T                    
03246                                   L12GP-LCAN-T                    
03247                                   L12CA-LCAN-T                    
03248                                   L12GD-LCAN-T                    
03249          ADD LCLM-T            TO L12AC-LCLM-T                    
03250                                   L12ST-LCLM-T                    
03251                                   L12GP-LCLM-T                    
03252                                   L12CA-LCLM-T                    
03253                                   L12GD-LCLM-T                    
03254          ADD APRM-T            TO L12AC-APRM-T                    
03255                                   L12ST-APRM-T                    
03256                                   L12GP-APRM-T                    
03257                                   L12CA-APRM-T                    
03258                                   L12GD-APRM-T                    
03259          ADD ACAN-T            TO L12AC-ACAN-T                    
03260                                   L12ST-ACAN-T                    
03261                                   L12GP-ACAN-T                    
03262                                   L12CA-ACAN-T                    
03263                                   L12GD-ACAN-T                    
03264          ADD ACLM-T            TO L12AC-ACLM-T                    
03265                                   L12ST-ACLM-T                    
03266                                   L12GP-ACLM-T                    
03267                                   L12CA-ACLM-T                    
03268                                   L12GD-ACLM-T                    
03269          ADD TPRM-T            TO L12AC-TPRM-T                    
03270                                   L12ST-TPRM-T                    
03271                                   L12GP-TPRM-T                    
03272                                   L12CA-TPRM-T                    
03273                                   L12GD-TPRM-T                    
03274          ADD TCOM-T            TO L12AC-TCOM-T                    
03275                                   L12ST-TCOM-T                    
03276                                   L12GP-TCOM-T                    
03277                                   L12CA-TCOM-T                    
03278                                   L12GD-TCOM-T.                   
03279                                                                   
03280      IF RUN-CCYY = COMP-CCYY (X1)                                 
03281          ADD CERT-T            TO YTDAC-CERT-T                    
03282                                   YTDST-CERT-T                    
03283                                   YTDGP-CERT-T                    
03284                                   YTDCA-CERT-T                    
03285                                   YTDGD-CERT-T                    
03286          ADD CANCEL-T          TO YTDAC-CANCEL-T                  
03287                                   YTDST-CANCEL-T                  
03288                                   YTDGP-CANCEL-T                  
03289                                   YTDCA-CANCEL-T                  
03290                                   YTDGD-CANCEL-T                  
03291          ADD L-COVERAGE-T      TO YTDAC-L-COVERAGE-T              
03292                                   YTDST-L-COVERAGE-T              
03293                                   YTDGP-L-COVERAGE-T              
03294                                   YTDCA-L-COVERAGE-T              
03295                                   YTDGD-L-COVERAGE-T              
03296          ADD A-COVERAGE-T      TO YTDAC-A-COVERAGE-T              
03297                                   YTDST-A-COVERAGE-T              
03298                                   YTDGP-A-COVERAGE-T              
03299                                   YTDCA-A-COVERAGE-T              
03300                                   YTDGD-A-COVERAGE-T              
03301          ADD LPRM-T            TO YTDAC-LPRM-T                    
03302                                   YTDST-LPRM-T                    
03303                                   YTDGP-LPRM-T                    
03304                                   YTDCA-LPRM-T                    
03305                                   YTDGD-LPRM-T                    
03306          ADD LCAN-T            TO YTDAC-LCAN-T                    
03307                                   YTDST-LCAN-T                    
03308                                   YTDGP-LCAN-T                    
03309                                   YTDCA-LCAN-T                    
03310                                   YTDGD-LCAN-T                    
03311          ADD LCLM-T            TO YTDAC-LCLM-T                    
03312                                   YTDST-LCLM-T                    
03313                                   YTDGP-LCLM-T                    
03314                                   YTDCA-LCLM-T                    
03315                                   YTDGD-LCLM-T                    
03316          ADD APRM-T            TO YTDAC-APRM-T                    
03317                                   YTDST-APRM-T                    
03318                                   YTDGP-APRM-T                    
03319                                   YTDCA-APRM-T                    
03320                                   YTDGD-APRM-T                    
03321          ADD ACLM-T            TO YTDAC-ACLM-T                    
03322                                   YTDST-ACLM-T                    
03323                                   YTDGP-ACLM-T                    
03324                                   YTDCA-ACLM-T                    
03325                                   YTDGD-ACLM-T                    
03326          ADD ACAN-T            TO YTDAC-ACAN-T                    
03327                                   YTDST-ACAN-T                    
03328                                   YTDGP-ACAN-T                    
03329                                   YTDCA-ACAN-T                    
03330                                   YTDGD-ACAN-T                    
03331          ADD TPRM-T            TO YTDAC-TPRM-T                    
03332                                   YTDST-TPRM-T                    
03333                                   YTDGP-TPRM-T                    
03334                                   YTDCA-TPRM-T                    
03335                                   YTDGD-TPRM-T                    
03336          ADD TCOM-T            TO YTDAC-TCOM-T                    
03337                                   YTDST-TCOM-T                    
03338                                   YTDGP-TCOM-T                    
03339                                   YTDCA-TCOM-T                    
03340                                   YTDGD-TCOM-T.                   

           IF X1 = +13
              COMPUTE PYTDAC-CERT-T = PYTDAC-CERT-T +
                 (AC-CERT-T (1) - AC-CERT-T (15))
              COMPUTE PYTDAC-CANCEL-T = PYTDAC-CANCEL-T +
                 (AC-CANCEL-T (1) - AC-CANCEL-T (15))
              COMPUTE PYTDAC-L-COVERAGE-T = PYTDAC-L-COVERAGE-T +
                 (AC-L-COVERAGE-T (1) - AC-L-COVERAGE-T (15))
              COMPUTE PYTDAC-A-COVERAGE-T = PYTDAC-A-COVERAGE-T +
                 (AC-A-COVERAGE-T (1) - AC-A-COVERAGE-T (15))
              COMPUTE PYTDAC-LPRM-T = PYTDAC-LPRM-T +
                 (AC-LPRM-T (1) - AC-LPRM-T (15))
              COMPUTE PYTDAC-LCAN-T = PYTDAC-LCAN-T +
                 (AC-LCAN-T (1) - AC-LCAN-T (15))
              COMPUTE PYTDAC-LCLM-T = PYTDAC-LCLM-T +
                 (AC-LCLM-T (1) - AC-LCLM-T (15))
              COMPUTE PYTDAC-APRM-T = PYTDAC-APRM-T +
                 (AC-APRM-T (1) - AC-APRM-T (15))
              COMPUTE PYTDAC-ACAN-T = PYTDAC-ACAN-T +
                 (AC-ACAN-T (1) - AC-ACAN-T (15))
              COMPUTE PYTDAC-ACLM-T = PYTDAC-ACLM-T +
                 (AC-ACLM-T (1) - AC-ACLM-T (15))
              COMPUTE PYTDAC-TCOM-T = PYTDAC-TCOM-T +
                 (AC-TCOM-T (1) - AC-TCOM-T (15))
              COMPUTE PYTDAC-TPRM-T = PYTDAC-TPRM-T +
                 (PYTDAC-LPRM-T + PYTDAC-APRM-T) -
                 (PYTDAC-LCAN-T + PYTDAC-ACAN-T)
              ADD PYTDAC-CERT-T         TO PYTDST-CERT-T
                                          PYTDGP-CERT-T
                                          PYTDCA-CERT-T
                                          PYTDGD-CERT-T
              ADD PYTDAC-CANCEL-T       TO PYTDST-CANCEL-T
                                          PYTDGP-CANCEL-T
                                          PYTDCA-CANCEL-T
                                          PYTDGD-CANCEL-T
              ADD PYTDAC-L-COVERAGE-T   TO PYTDST-L-COVERAGE-T
                                          PYTDGP-L-COVERAGE-T
                                          PYTDCA-L-COVERAGE-T
                                          PYTDGD-L-COVERAGE-T
              ADD PYTDAC-A-COVERAGE-T   TO PYTDST-A-COVERAGE-T
                                          PYTDGP-A-COVERAGE-T
                                          PYTDCA-A-COVERAGE-T
                                          PYTDGD-A-COVERAGE-T
              ADD PYTDAC-LPRM-T         TO PYTDST-LPRM-T
                                          PYTDGP-LPRM-T
                                          PYTDCA-LPRM-T
                                          PYTDGD-LPRM-T
              ADD PYTDAC-LCAN-T         TO PYTDST-LCAN-T
                                          PYTDGP-LCAN-T
                                          PYTDCA-LCAN-T
                                          PYTDGD-LCAN-T
              ADD PYTDAC-LCLM-T         TO PYTDST-LCLM-T
                                          PYTDGP-LCLM-T
                                          PYTDCA-LCLM-T
                                          PYTDGD-LCLM-T
              ADD PYTDAC-APRM-T         TO PYTDST-APRM-T
                                          PYTDGP-APRM-T
                                          PYTDCA-APRM-T
                                          PYTDGD-APRM-T
              ADD PYTDAC-ACAN-T         TO PYTDST-ACAN-T
                                          PYTDGP-ACAN-T
                                          PYTDCA-ACAN-T
                                          PYTDGD-ACAN-T
              ADD PYTDAC-ACLM-T         TO PYTDST-ACLM-T
                                          PYTDGP-ACLM-T
                                          PYTDCA-ACLM-T
                                          PYTDGD-ACLM-T
              ADD PYTDAC-TCOM-T         TO PYTDST-TCOM-T
                                          PYTDGP-TCOM-T
                                          PYTDCA-TCOM-T
                                          PYTDGD-TCOM-T
              ADD PYTDAC-TPRM-T         TO PYTDST-TPRM-T
                                          PYTDGP-TPRM-T
                                          PYTDCA-TPRM-T
                                          PYTDGD-TPRM-T
           END-IF


03341      .
03342  2510-EXIT.                                                       
03343       EXIT.                                                       
03344                                                                   
03345  2520-PRINT-LAST-AC-12.                                           
03346                                                                   
03347      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03348                                     DETAIL-TMS-LINE-2.            
03349                                                                   
03350      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03351      MOVE L12AC-CERT-T           TO DET-TMS-CERTS.                
03352      MOVE L12AC-CANCEL-T         TO DET-TMS-CANCELS.              
03353      MOVE L12AC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03354      MOVE L12AC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03355      MOVE L12AC-LPRM-T           TO DET-TMS-LPRM.                 
03356      MOVE L12AC-LCLM-T           TO DET-TMS-LCLM.                 
03357      MOVE L12AC-LCAN-T           TO DET-TMS-LCAN.                 
03358      MOVE L12AC-APRM-T           TO DET-TMS-APRM.                 
03359      MOVE L12AC-ACAN-T           TO DET-TMS-ACAN.                 
03360      MOVE L12AC-ACLM-T           TO DET-TMS-ACLM.                 
03361      MOVE L12AC-TPRM-T           TO DET-TMS-TPRM.                 
03362      MOVE L12AC-TCOM-T           TO DET-TMS-TCOM.                 
03363      COMPUTE DET-TMS-NPRM = (L12AC-TPRM-T  - L12AC-TCOM-T).       
03364      MOVE '*12 MO '              TO DET-TMS-DATE.                 
03365      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03366                                                                   
122804     MOVE ' '                    TO X.                            
03368      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03369      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03370      MOVE ' '                    TO X.                            
03371      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03372      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03373      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03374      MOVE ' '                    TO X.                            
03375      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03376                                                                   
03377      MOVE ZERO-ACCUM             TO LAST-12-ACCOUNT-ACCUM.        
03378      MOVE ZERO-ACCUM-T           TO LAST-12-ACCOUNT-ACCUM-T.      

122804*    BEGIN PL12 PRINT 

122804*    MOVE SPACES                 TO DETAIL-TMS-LINE-1             
122804*                                   DETAIL-TMS-LINE-2.            
122804*                                                                 
122804*    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
122804*    MOVE PL12AC-CERT-T           TO DET-TMS-CERTS.                
122804*    MOVE PL12AC-CANCEL-T         TO DET-TMS-CANCELS.              
122804*    MOVE PL12AC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
122804*    MOVE PL12AC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
122804*    MOVE PL12AC-LPRM-T           TO DET-TMS-LPRM.                 
122804*    MOVE PL12AC-LCAN-T           TO DET-TMS-LCAN.                 
122804*    MOVE PL12AC-LCLM-T           TO DET-TMS-LCLM.                 
122804*    MOVE PL12AC-APRM-T           TO DET-TMS-APRM.                 
122804*    MOVE PL12AC-ACAN-T           TO DET-TMS-ACAN.                 
122804*    MOVE PL12AC-ACLM-T           TO DET-TMS-ACLM.                 
122804*    MOVE PL12AC-TPRM-T           TO DET-TMS-TPRM.                 
122804*    MOVE PL12AC-TCOM-T           TO DET-TMS-TCOM.                 
122804*    COMPUTE DET-TMS-NPRM = (PL12AC-TPRM-T  - PL12AC-TCOM-T).       
122804*                                                                 
122804*    MOVE 'PRIOR L12   '         TO DET-TMS-TITLE.                
122804*    MOVE '*PL12  '              TO DET-TMS-DATE.                 
122804*    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
122804*    MOVE ' '                    TO X.                            
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*                                                                 
122804*    MOVE SPACES                 TO DET-TITLE.                    
122804*    MOVE ZERO-ACCUM             TO PL12-ACCOUNT-ACCUM.            
122804*    MOVE ZERO-ACCUM-T           TO PL12-ACCOUNT-ACCUM-T.          
03416 *                                                                 
03379                                                                   
03380  2530-PRINT-AC-YTD.                                               
03381                                                                   
03382      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03383                                     DETAIL-TMS-LINE-2.            
03384                                                                   
03385      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03386      MOVE YTDAC-CERT-T           TO DET-TMS-CERTS.                
03387      MOVE YTDAC-CANCEL-T         TO DET-TMS-CANCELS.              
03388      MOVE YTDAC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03389      MOVE YTDAC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03390      MOVE YTDAC-LPRM-T           TO DET-TMS-LPRM.                 
03391      MOVE YTDAC-LCAN-T           TO DET-TMS-LCAN.                 
03392      MOVE YTDAC-LCLM-T           TO DET-TMS-LCLM.                 
03393      MOVE YTDAC-APRM-T           TO DET-TMS-APRM.                 
03394      MOVE YTDAC-ACAN-T           TO DET-TMS-ACAN.                 
03395      MOVE YTDAC-ACLM-T           TO DET-TMS-ACLM.                 
03396      MOVE YTDAC-TPRM-T           TO DET-TMS-TPRM.                 
03397      MOVE YTDAC-TCOM-T           TO DET-TMS-TCOM.                 
03398      COMPUTE DET-TMS-NPRM = (YTDAC-TPRM-T  - YTDAC-TCOM-T).       
03399                                                                   
03400 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                
03401      MOVE '*YTD   '              TO DET-TMS-DATE.                 
03402      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03403      MOVE ' '                    TO X.                            
03404      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03405      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03406      MOVE ' '                    TO X.                            
03407      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03408      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03409      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03410      MOVE ' '                    TO X.                            
03411      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03412                                                                   
03413      MOVE SPACES                 TO DET-TITLE.                    
03414      MOVE ZERO-ACCUM             TO YTD-ACCOUNT-ACCUM.            
03415      MOVE ZERO-ACCUM-T           TO YTD-ACCOUNT-ACCUM-T.          

122804*    BEGIN PYTD PRINT 

122804     MOVE SPACES                 TO DETAIL-TMS-LINE-1             
122804                                    DETAIL-TMS-LINE-2.            
122804                                                                  
122804     MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
122804     MOVE PYTDAC-CERT-T           TO DET-TMS-CERTS.                
122804     MOVE PYTDAC-CANCEL-T         TO DET-TMS-CANCELS.              
122804     MOVE PYTDAC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
122804     MOVE PYTDAC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
122804     MOVE PYTDAC-LPRM-T           TO DET-TMS-LPRM.                 
122804     MOVE PYTDAC-LCAN-T           TO DET-TMS-LCAN.                 
122804     MOVE PYTDAC-LCLM-T           TO DET-TMS-LCLM.                 
122804     MOVE PYTDAC-APRM-T           TO DET-TMS-APRM.                 
122804     MOVE PYTDAC-ACAN-T           TO DET-TMS-ACAN.                 
122804     MOVE PYTDAC-ACLM-T           TO DET-TMS-ACLM.                 
122804     MOVE PYTDAC-TPRM-T           TO DET-TMS-TPRM.                 
122804     MOVE PYTDAC-TCOM-T           TO DET-TMS-TCOM.                 
122804     COMPUTE DET-TMS-NPRM = (PYTDAC-TPRM-T  - PYTDAC-TCOM-T).       
122804                                                                  
122804*    MOVE 'PRIOR YTD   '         TO DET-TMS-TITLE.                
122804     MOVE '*PYTD  '              TO DET-TMS-DATE.                 
122804     MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
122804     MOVE ' '                    TO X.                            
122804     MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
122804     PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804     MOVE ' '                    TO X.                            
122804     MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
122804     PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
122804*    MOVE ' '                    TO X.                            
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804                                                                  
122804     MOVE SPACES                 TO DET-TITLE.                    
122804     MOVE ZERO-ACCUM             TO PYTD-ACCOUNT-ACCUM.            
122804     MOVE ZERO-ACCUM-T           TO PYTD-ACCOUNT-ACCUM-T.          

03416                                                                   
122804*    BEGIN ITD PRINT 

122804*    MOVE SPACES                 TO DETAIL-TMS-LINE-1             
122804*                                   DETAIL-TMS-LINE-2.            
122804*                                                                 
122804*    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
122804*    MOVE ITDAC-CERT-T           TO DET-TMS-CERTS.                
122804*    MOVE ITDAC-CANCEL-T         TO DET-TMS-CANCELS.              
122804*    MOVE ITDAC-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
122804*    MOVE ITDAC-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
122804*    MOVE ITDAC-LPRM-T           TO DET-TMS-LPRM.                 
122804*    MOVE ITDAC-LCAN-T           TO DET-TMS-LCAN.                 
122804*    MOVE ITDAC-LCLM-T           TO DET-TMS-LCLM.                 
122804*    MOVE ITDAC-APRM-T           TO DET-TMS-APRM.                 
122804*    MOVE ITDAC-ACAN-T           TO DET-TMS-ACAN.                 
122804*    MOVE ITDAC-ACLM-T           TO DET-TMS-ACLM.                 
122804*    MOVE ITDAC-TPRM-T           TO DET-TMS-TPRM.                 
122804*    MOVE ITDAC-TCOM-T           TO DET-TMS-TCOM.                 
122804*    COMPUTE DET-TMS-NPRM = (ITDAC-TPRM-T  - ITDAC-TCOM-T).       
122804*                                                                 
122804*    MOVE 'INCEP TO DT '         TO DET-TMS-TITLE.                
122804*    MOVE '*ITD   '              TO DET-TMS-DATE.                 
122804*    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
122804*    MOVE ' '                    TO X.                            
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*                                                                 
122804     MOVE SPACES                 TO DET-TITLE.                    
122804*    MOVE ZERO-ACCUM             TO ITD-ACCOUNT-ACCUM.            
122804*    MOVE ZERO-ACCUM-T           TO ITD-ACCOUNT-ACCUM-T.          

03416      .                                                            
03417  2540-ZERO-ACCOUNT.                                               
03418      MOVE +0                     TO X1.                           
03419      PERFORM 0150-ZERO-ACCUM-ACC THRU 0150-EXIT 15 TIMES.         
03420  EJECT                                                            
03421                                                                   
03422  2560-STATE-BREAK.                                                
03423      MOVE 0                      TO A1.                           
03424      PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT                   
03425       VARYING X1 FROM 2 BY 1 UNTIL                                
03426        X1 IS GREATER THAN DATE-RANGE-MAX.                         
03427      MOVE SPACES                 TO  B-E-AGT-TABLE.               
03428      MOVE +0                     TO  X1.                          
03429      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
03430      PERFORM 2380-ACCOUNT-BREAK THRU 2540-ZERO-ACCOUNT.           
03431                                                                   
03432  2570-STATE-BREAK-1.                                              
03433      IF P-ST-SW = '1'                                             
03434          MOVE ' '                TO P-ST-SW                       
03435      ELSE                                                         
03436          GO TO 2610-ZERO-STATE.                                   
03437                                                                   
03438      PERFORM 0850-ST-HD          THRU 0850-EXIT.                  
03439      MOVE +1                     TO X1.                           
03440                                                                   
03441  2580-STATE-BREAK-2.                                              
03442      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
03443          GO TO 2590-PRINT-LAST-ST-12.                             
03444                                                                   
03445      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03446                                     DETAIL-TMS-LINE-2.            
03447                                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03450      MOVE ST-CERT-T (X1)         TO DET-TMS-CERTS.                
03451      MOVE ST-CANCEL-T (X1)       TO DET-TMS-CANCELS.              
03452      MOVE ST-L-COVERAGE-T (X1)   TO DET-TMS-LCOVERAG.             
03453      MOVE ST-A-COVERAGE-T (X1)   TO DET-TMS-ACOVERAG.             
03454      MOVE ST-LPRM-T (X1)         TO DET-TMS-LPRM.                 
03455      MOVE ST-LCAN-T (X1)         TO DET-TMS-LCAN.                 
03456      MOVE ST-LCLM-T (X1)         TO DET-TMS-LCLM.                 
03457      MOVE ST-APRM-T (X1)         TO DET-TMS-APRM.                 
03458      MOVE ST-ACAN-T (X1)         TO DET-TMS-ACAN.                 
03459      MOVE ST-ACLM-T (X1)         TO DET-TMS-ACLM.                 
03460      MOVE ST-TPRM-T (X1)         TO DET-TMS-TPRM.                 
03461      MOVE ST-TCOM-T (X1)         TO DET-TMS-TCOM.                 
03462      COMPUTE DET-TMS-NPRM = (ST-TPRM-T (X1) - ST-TCOM-T (X1)).    
03463                                                                   
03464      ADD +1                      TO X1.                           
03465      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   
03466      MOVE COMP-MO (X1)           TO DET-MO                        
03467                                     DET-TMS-MO.                   
03468      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
03469      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03470                                                                   
03471      MOVE ' '                    TO X.                            
03472                                                                   
03473      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03474      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03475                                                                   
03476      MOVE ' '                    TO X.                            
03477                                                                   
03478      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03479      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03480      MOVE ' '                    TO X.                            
03481      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03482                                                                   
03483                                                                   
03484      GO TO 2580-STATE-BREAK-2.                                    
03485                                                                   
03486  2590-PRINT-LAST-ST-12.                                           
03487                                                                   
03488      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03489                                     DETAIL-TMS-LINE-2.            
03490                                                                   
03491      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03492      MOVE L12ST-CERT-T           TO DET-TMS-CERTS.                
03493      MOVE L12ST-CANCEL-T         TO DET-TMS-CANCELS.              
03494      MOVE L12ST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03495      MOVE L12ST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03496      MOVE L12ST-LPRM-T           TO DET-TMS-LPRM.                 
03497      MOVE L12ST-LCAN-T           TO DET-TMS-LCAN.                 
03498      MOVE L12ST-LCLM-T           TO DET-TMS-LCLM.                 
03499      MOVE L12ST-APRM-T           TO DET-TMS-APRM.                 
03500      MOVE L12ST-ACAN-T           TO DET-TMS-ACAN.                 
03501      MOVE L12ST-ACLM-T           TO DET-TMS-ACLM.                 
03502      MOVE L12ST-TPRM-T           TO DET-TMS-TPRM.                 
03503      MOVE L12ST-TCOM-T           TO DET-TMS-TCOM.                 
03504      COMPUTE DET-TMS-NPRM = (L12ST-TPRM-T  - L12ST-TCOM-T).       
03505                                                                   
03506 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                
03507      MOVE '*12 MO '              TO DET-TMS-DATE.                 
03508      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03509                                                                   
03510      MOVE ' '                    TO X.                            
03511      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03512      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03513      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03514      MOVE ' '                    TO X.                            
03515                                                                   
03516      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03517                                                                   
03518      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03519      MOVE ' '                    TO X.                            
03520      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03521                                                                   
03522      MOVE ZERO-ACCUM             TO LAST-12-STATE-ACCUM.          
03523      MOVE ZERO-ACCUM-T           TO LAST-12-STATE-ACCUM-T.        
03524                                                                   
122804*   BEGIN PL12 PRINT

122804*    MOVE SPACES                 TO DETAIL-TMS-LINE-1             
122804*                                   DETAIL-TMS-LINE-2.            
122804*                                                                 
122804*    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
122804*    MOVE PL12ST-CERT-T           TO DET-TMS-CERTS.                
122804*    MOVE PL12ST-CANCEL-T         TO DET-TMS-CANCELS.              
122804*    MOVE PL12ST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
122804*    MOVE PL12ST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
122804*    MOVE PL12ST-LPRM-T           TO DET-TMS-LPRM.                 
122804*    MOVE PL12ST-LCAN-T           TO DET-TMS-LCAN.                 
122804*    MOVE PL12ST-LCLM-T           TO DET-TMS-LCLM.                 
122804*    MOVE PL12ST-APRM-T           TO DET-TMS-APRM.                 
122804*    MOVE PL12ST-ACAN-T           TO DET-TMS-ACAN.                 
122804*    MOVE PL12ST-ACLM-T           TO DET-TMS-ACLM.                 
122804*    MOVE PL12ST-TPRM-T           TO DET-TMS-TPRM.                 
122804*    MOVE PL12ST-TCOM-T           TO DET-TMS-TCOM.                 
122804*    COMPUTE DET-TMS-NPRM = (PL12ST-TPRM-T - PL12ST-TCOM-T).        
122804*                                                                 
122804*    MOVE 'PRIOR L12   '         TO DET-TMS-TITLE.                
122804*    MOVE '*PL12  '              TO DET-TMS-DATE.                 
122804*    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE SPACES                 TO DET-TMS-TITLE.                
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
122804*    MOVE ' '                    TO X.                            
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804                                                                  
122804*    MOVE ZERO-ACCUM             TO PL12-STATE-ACCUM.              
122804*    MOVE ZERO-ACCUM-T           TO PL12-STATE-ACCUM-T.            

03525  2600-PRINT-ST-YTD.                                               
03526      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03527                                     DETAIL-TMS-LINE-2.            
03528                                                                   
03529      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03530      MOVE YTDST-CERT-T           TO DET-TMS-CERTS.                
03531      MOVE YTDST-CANCEL-T         TO DET-TMS-CANCELS.              
03532      MOVE YTDST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03533      MOVE YTDST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03534      MOVE YTDST-LPRM-T           TO DET-TMS-LPRM.                 
03535      MOVE YTDST-LCAN-T           TO DET-TMS-LCAN.                 
03536      MOVE YTDST-LCLM-T           TO DET-TMS-LCLM.                 
03537      MOVE YTDST-APRM-T           TO DET-TMS-APRM.                 
03538      MOVE YTDST-ACAN-T           TO DET-TMS-ACAN.                 
03539      MOVE YTDST-ACLM-T           TO DET-TMS-ACLM.                 
03540      MOVE YTDST-TPRM-T           TO DET-TMS-TPRM.                 
03541      MOVE YTDST-TCOM-T           TO DET-TMS-TCOM.                 
03542      COMPUTE DET-TMS-NPRM = (YTDST-TPRM-T - YTDST-TCOM-T).        
03543                                                                   
03544 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                
03545      MOVE '*YTD   '              TO DET-TMS-DATE.                 
03546      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03547      MOVE ' '                    TO X.                            
03548      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03549      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03550      MOVE SPACES                 TO DET-TMS-TITLE.                
03551      MOVE ' '                    TO X.                            
03552      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03553      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03554      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03555      MOVE ' '                    TO X.                            
03556      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03557                                                                   
03558      MOVE ZERO-ACCUM             TO YTD-STATE-ACCUM.              
03559      MOVE ZERO-ACCUM-T           TO YTD-STATE-ACCUM-T.            
03560                                                                   
122804*   BEGIN PYTD PRINT
122804     MOVE SPACES                 TO DETAIL-TMS-LINE-1             
122804                                    DETAIL-TMS-LINE-2.            
122804                                                                  
122804     MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
122804     MOVE PYTDST-CERT-T           TO DET-TMS-CERTS.                
122804     MOVE PYTDST-CANCEL-T         TO DET-TMS-CANCELS.              
122804     MOVE PYTDST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
122804     MOVE PYTDST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
122804     MOVE PYTDST-LPRM-T           TO DET-TMS-LPRM.                 
122804     MOVE PYTDST-LCAN-T           TO DET-TMS-LCAN.                 
122804     MOVE PYTDST-LCLM-T           TO DET-TMS-LCLM.                 
122804     MOVE PYTDST-APRM-T           TO DET-TMS-APRM.                 
122804     MOVE PYTDST-ACAN-T           TO DET-TMS-ACAN.                 
122804     MOVE PYTDST-ACLM-T           TO DET-TMS-ACLM.                 
122804     MOVE PYTDST-TPRM-T           TO DET-TMS-TPRM.                 
122804     MOVE PYTDST-TCOM-T           TO DET-TMS-TCOM.                 
122804     COMPUTE DET-TMS-NPRM = (PYTDST-TPRM-T - PYTDST-TCOM-T).        
122804                                                                  
122804*    MOVE 'PRIOR YTD   '         TO DET-TMS-TITLE.                
122804     MOVE '*PYTD  '              TO DET-TMS-DATE.                 
122804     MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
122804     MOVE ' '                    TO X.                            
122804     MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
122804     PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804     MOVE SPACES                 TO DET-TMS-TITLE.                
122804     MOVE ' '                    TO X.                            
122804     MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
122804     PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
122804*    MOVE ' '                    TO X.                            
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804                                                                  
122804     MOVE ZERO-ACCUM             TO PYTD-STATE-ACCUM.              
122804     MOVE ZERO-ACCUM-T           TO PYTD-STATE-ACCUM-T.            
03560                                                                   
122804*   BEGIN ITD PRINT
122804*    MOVE SPACES                 TO DETAIL-TMS-LINE-1             
122804*                                   DETAIL-TMS-LINE-2.            
122804*                                                                 
122804*    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
122804*    MOVE ITDST-CERT-T           TO DET-TMS-CERTS.                
122804*    MOVE ITDST-CANCEL-T         TO DET-TMS-CANCELS.              
122804*    MOVE ITDST-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
122804*    MOVE ITDST-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
122804*    MOVE ITDST-LPRM-T           TO DET-TMS-LPRM.                 
122804*    MOVE ITDST-LCAN-T           TO DET-TMS-LCAN.                 
122804*    MOVE ITDST-LCLM-T           TO DET-TMS-LCLM.                 
122804*    MOVE ITDST-APRM-T           TO DET-TMS-APRM.                 
122804*    MOVE ITDST-ACAN-T           TO DET-TMS-ACAN.                 
122804*    MOVE ITDST-ACLM-T           TO DET-TMS-ACLM.                 
122804*    MOVE ITDST-TPRM-T           TO DET-TMS-TPRM.                 
122804*    MOVE ITDST-TCOM-T           TO DET-TMS-TCOM.                 
122804*    COMPUTE DET-TMS-NPRM = (ITDST-TPRM-T - ITDST-TCOM-T).        
122804*                                                                 
122804*    MOVE 'INCEP TO DT '         TO DET-TMS-TITLE.                
122804*    MOVE '*ITD   '              TO DET-TMS-DATE.                 
122804*    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE SPACES                 TO DET-TMS-TITLE.                
122804*    MOVE ' '                    TO X.                            
122804*    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
122804*    MOVE ' '                    TO X.                            
122804*    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
122804*                                                                 
122804*    MOVE ZERO-ACCUM             TO ITD-STATE-ACCUM.              
122804*    MOVE ZERO-ACCUM-T           TO ITD-STATE-ACCUM-T.            
03560                                                                   
03560                                                                   
03561  2610-ZERO-STATE.                                                 
03562      MOVE +0                     TO X1.                           
03563      PERFORM 0160-ZERO-ACCUM-ST THRU 0160-EXIT 15 TIMES.          
03564  EJECT                                                            
03565  2630-GROUPING-BREAK.                                             
03566      MOVE 0                      TO A1.                           
03567      PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT                   
03568       VARYING X1 FROM 2 BY 1 UNTIL                                
03569        X1 IS GREATER THAN DATE-RANGE-MAX.                         
03570      MOVE SPACES                 TO  B-E-AGT-TABLE.               
03571      MOVE +0                     TO  X1.                          
03572      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
03573      PERFORM 2380-ACCOUNT-BREAK THRU 2540-ZERO-ACCOUNT.           
03574      PERFORM 2570-STATE-BREAK-1 THRU 2610-ZERO-STATE.             
03575                                                                   
03576  2640-GROUPING-BREAK-1.                                           
03577      IF P-GRP-SW = '1'                                            
03578          MOVE ' '                TO P-GRP-SW                      
03579      ELSE                                                         
03580          GO TO 2680-ZERO-GROUPING.                                
03581                                                                   
03582      PERFORM 0860-GP-HD          THRU 0860-EXIT.                  
03583      MOVE +1                     TO X1.                           
03584                                                                   
03585  2650-GROUPING-BREAK-2.                                           
03586      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
03587          GO TO 2660-PRINT-LAST-GP-12.                             
03588                                                                   
03589      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03590                                     DETAIL-TMS-LINE-2.            
03591                                                                   
03592      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03593      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03594      MOVE GP-CERT-T (X1)           TO DET-TMS-CERTS.              
03595      MOVE GP-CANCEL-T (X1)         TO DET-TMS-CANCELS.            
03596      MOVE GP-L-COVERAGE-T (X1)     TO DET-TMS-LCOVERAG.           
03597      MOVE GP-A-COVERAGE-T (X1)     TO DET-TMS-ACOVERAG.           
03598      MOVE GP-LPRM-T (X1)           TO DET-TMS-LPRM.               
03599      MOVE GP-LCAN-T (X1)           TO DET-TMS-LCAN.               
03600      MOVE GP-LCLM-T (X1)           TO DET-TMS-LCLM.               
03601      MOVE GP-APRM-T (X1)           TO DET-TMS-APRM.               
03602      MOVE GP-ACAN-T (X1)           TO DET-TMS-ACAN.               
03603      MOVE GP-ACLM-T (X1)           TO DET-TMS-ACLM.               
03604      MOVE GP-TPRM-T (X1)           TO DET-TMS-TPRM.               
03605      MOVE GP-TCOM-T (X1)           TO DET-TMS-TCOM.               
03606      COMPUTE DET-TMS-NPRM = (GP-TPRM-T (X1) - GP-TCOM-T (X1)).    
03607                                                                   
03608      ADD +1                      TO X1.                           
03609      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   
03610      MOVE COMP-MO (X1)           TO DET-MO                        
03611                                     DET-TMS-MO.                   
03612      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
03613      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03614                                                                   
03615      MOVE ' '                    TO X.                            
03616                                                                   
03617      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03618      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03619                                                                   
03620      MOVE ' '                    TO X.                            
03621                                                                   
03622      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03623      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03624      MOVE ' '                    TO X.                            
03625      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03626      GO TO 2650-GROUPING-BREAK-2.                                 
03627                                                                   
03628  2660-PRINT-LAST-GP-12.                                           
03629      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03630                                     DETAIL-TMS-LINE-2.            
03631                                                                   
03632      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03633      MOVE L12GP-CERT-T           TO DET-TMS-CERTS.                
03634      MOVE L12GP-CANCEL-T         TO DET-TMS-CANCELS.              
03635      MOVE L12GP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03636      MOVE L12GP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03637      MOVE L12GP-LPRM-T           TO DET-TMS-LPRM.                 
03638      MOVE L12GP-LCAN-T           TO DET-TMS-LCAN.                 
03639      MOVE L12GP-LCLM-T           TO DET-TMS-LCLM.                 
03640      MOVE L12GP-APRM-T           TO DET-TMS-APRM.                 
03641      MOVE L12GP-ACAN-T           TO DET-TMS-ACAN.                 
03642      MOVE L12GP-ACLM-T           TO DET-TMS-ACLM.                 
03643      MOVE L12GP-TPRM-T           TO DET-TMS-TPRM.                 
03644      MOVE L12GP-TCOM-T           TO DET-TMS-TCOM.                 
03645      COMPUTE DET-TMS-NPRM = (L12GP-TPRM-T - L12GP-TCOM-T).        
03646                                                                   
03647 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                
03648      MOVE '*12 MO '              TO DET-TMS-DATE.                 
03649      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03650                                                                   
03651      MOVE ' '                    TO X.                            
03652      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03653                                                                   
03654      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03655      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03656                                                                   
03657      MOVE ' '                    TO X.                            
03658                                                                   
03659      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03660      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03661      MOVE ' '                    TO X.                            
03662      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03663      MOVE ZERO-ACCUM             TO LAST-12-GROUPING-ACCUM.       
03664      MOVE ZERO-ACCUM-T           TO LAST-12-GROUPING-ACCUM-T.     
03665                                                                   
122804*   BEGIN PL12 TOTALS

03667 *    MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03668 *                                   DETAIL-TMS-LINE-2.            
03669 *                                                                 
03670 *    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03671 *    MOVE PL12GP-CERT-T           TO DET-TMS-CERTS.                
03672 *    MOVE PL12GP-CANCEL-T         TO DET-TMS-CANCELS.              
03673 *    MOVE PL12GP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03674 *    MOVE PL12GP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03675 *    MOVE PL12GP-LPRM-T           TO DET-TMS-LPRM.                 
03676 *    MOVE PL12GP-LCAN-T           TO DET-TMS-LCAN.                 
03677 *    MOVE PL12GP-LCLM-T           TO DET-TMS-LCLM.                 
03678 *    MOVE PL12GP-APRM-T           TO DET-TMS-APRM.                 
03679 *    MOVE PL12GP-ACAN-T           TO DET-TMS-ACAN.                 
03680 *    MOVE PL12GP-ACLM-T           TO DET-TMS-ACLM.                 
03681 *    MOVE PL12GP-TPRM-T           TO DET-TMS-TPRM.                 
03682 *    MOVE PL12GP-TCOM-T           TO DET-TMS-TCOM.                 
03683 *    COMPUTE DET-TMS-NPRM = (PL12GP-TPRM-T - PL12GP-TCOM-T).        
03684 *                                                                 
03685 *    MOVE 'PRIOR L12   '         TO DET-TMS-TITLE.                
03686 *    MOVE '*PL12  '              TO DET-TMS-DATE.                 
03687 *    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03688 *    MOVE ' '                    TO X.                            
03689 *    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03691 *    MOVE ' '                    TO X.                            
03693 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03694 *    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03696 *    MOVE ' '                    TO X.                            
03698 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03699 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03700 *    MOVE ' '                    TO X.                            
03701 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03702 *    MOVE ZERO-ACCUM             TO PL12-GROUPING-ACCUM.           
03703 *    MOVE ZERO-ACCUM-T           TO PL12-GROUPING-ACCUM-T.         

03666  2670-PRINT-GP-YTD.                                               
03667      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03668                                     DETAIL-TMS-LINE-2.            
03669                                                                   
03670      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03671      MOVE YTDGP-CERT-T           TO DET-TMS-CERTS.                
03672      MOVE YTDGP-CANCEL-T         TO DET-TMS-CANCELS.              
03673      MOVE YTDGP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03674      MOVE YTDGP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03675      MOVE YTDGP-LPRM-T           TO DET-TMS-LPRM.                 
03676      MOVE YTDGP-LCAN-T           TO DET-TMS-LCAN.                 
03677      MOVE YTDGP-LCLM-T           TO DET-TMS-LCLM.                 
03678      MOVE YTDGP-APRM-T           TO DET-TMS-APRM.                 
03679      MOVE YTDGP-ACAN-T           TO DET-TMS-ACAN.                 
03680      MOVE YTDGP-ACLM-T           TO DET-TMS-ACLM.                 
03681      MOVE YTDGP-TPRM-T           TO DET-TMS-TPRM.                 
03682      MOVE YTDGP-TCOM-T           TO DET-TMS-TCOM.                 
03683      COMPUTE DET-TMS-NPRM = (YTDGP-TPRM-T - YTDGP-TCOM-T).        
03684                                                                   
03685 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                
03686      MOVE '*YTD   '              TO DET-TMS-DATE.                 
03687      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03688      MOVE ' '                    TO X.                            
03689      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03690                                                                   
03691      MOVE ' '                    TO X.                            
03692                                                                   
03693      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03694      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03695                                                                   
03696      MOVE ' '                    TO X.                            
03697                                                                   
03698      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03699      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03700      MOVE ' '                    TO X.                            
03701      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03702      MOVE ZERO-ACCUM             TO YTD-GROUPING-ACCUM.           
03703      MOVE ZERO-ACCUM-T           TO YTD-GROUPING-ACCUM-T.         
03704                                                                   
122804*   BEGIN PYTD TOTALS

03667      MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03668                                     DETAIL-TMS-LINE-2.            
03669                                                                   
03670      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03671      MOVE PYTDGP-CERT-T           TO DET-TMS-CERTS.                
03672      MOVE PYTDGP-CANCEL-T         TO DET-TMS-CANCELS.              
03673      MOVE PYTDGP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03674      MOVE PYTDGP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03675      MOVE PYTDGP-LPRM-T           TO DET-TMS-LPRM.                 
03676      MOVE PYTDGP-LCAN-T           TO DET-TMS-LCAN.                 
03677      MOVE PYTDGP-LCLM-T           TO DET-TMS-LCLM.                 
03678      MOVE PYTDGP-APRM-T           TO DET-TMS-APRM.                 
03679      MOVE PYTDGP-ACAN-T           TO DET-TMS-ACAN.                 
03680      MOVE PYTDGP-ACLM-T           TO DET-TMS-ACLM.                 
03681      MOVE PYTDGP-TPRM-T           TO DET-TMS-TPRM.                 
03682      MOVE PYTDGP-TCOM-T           TO DET-TMS-TCOM.                 
03683      COMPUTE DET-TMS-NPRM = (PYTDGP-TPRM-T - PYTDGP-TCOM-T).        
03684                                                                   
03685 *    MOVE 'PRIOR YTD   '         TO DET-TMS-TITLE.                
03686      MOVE '*PYTD  '              TO DET-TMS-DATE.                 
03687      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03688      MOVE ' '                    TO X.                            
03689      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03690                                                                   
03691      MOVE ' '                    TO X.                            
03692                                                                   
03693      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03694      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03695                                                                   
03696      MOVE ' '                    TO X.                            
03697                                                                   
03698      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03699 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03700 *    MOVE ' '                    TO X.                            
03701 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03702      MOVE ZERO-ACCUM             TO PYTD-GROUPING-ACCUM.           
03703      MOVE ZERO-ACCUM-T           TO PYTD-GROUPING-ACCUM-T.         

122804*   BEGIN ITD TOTALS

03667 *    MOVE SPACES                 TO DETAIL-TMS-LINE-1             
03668 *                                   DETAIL-TMS-LINE-2.            
03669 *                                                                 
03670 *    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03671 *    MOVE ITDGP-CERT-T           TO DET-TMS-CERTS.                
03672 *    MOVE ITDGP-CANCEL-T         TO DET-TMS-CANCELS.              
03673 *    MOVE ITDGP-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03674 *    MOVE ITDGP-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03675 *    MOVE ITDGP-LPRM-T           TO DET-TMS-LPRM.                 
03676 *    MOVE ITDGP-LCAN-T           TO DET-TMS-LCAN.                 
03677 *    MOVE ITDGP-LCLM-T           TO DET-TMS-LCLM.                 
03678 *    MOVE ITDGP-APRM-T           TO DET-TMS-APRM.                 
03679 *    MOVE ITDGP-ACAN-T           TO DET-TMS-ACAN.                 
03680 *    MOVE ITDGP-ACLM-T           TO DET-TMS-ACLM.                 
03681 *    MOVE ITDGP-TPRM-T           TO DET-TMS-TPRM.                 
03682 *    MOVE ITDGP-TCOM-T           TO DET-TMS-TCOM.                 
03683 *    COMPUTE DET-TMS-NPRM = (ITDGP-TPRM-T - ITDGP-TCOM-T).        
03684 *                                                                 
03685 *    MOVE 'INCEP TO DT '         TO DET-TMS-TITLE.                
03686 *    MOVE '*ITD   '              TO DET-TMS-DATE.                 
03687 *    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03688 *    MOVE ' '                    TO X.                            
03689 *    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03690 *                                                                 
03691 *    MOVE ' '                    TO X.                            
03692 *                                                                 
03693 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03694 *    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03695 *                                                                 
03696 *    MOVE ' '                    TO X.                            
03697 *                                                                 
03698 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03699 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03700 *    MOVE ' '                    TO X.                            
03701 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03702 *    MOVE ZERO-ACCUM             TO ITD-GROUPING-ACCUM.           
03703 *    MOVE ZERO-ACCUM-T           TO ITD-GROUPING-ACCUM-T.         

03705  2680-ZERO-GROUPING.                                              
03706      MOVE +0                     TO X1.                           
03707      PERFORM 0170-ZERO-ACCUM-GRP THRU 0170-EXIT 15 TIMES.         
03708  EJECT                                                            
03709  2700-CARRIER-BREAK.                                              
03710      MOVE 0                      TO A1.                           
03711      PERFORM 2400-BUILD-EXTRACTS THRU 2470-EXIT                   
03712       VARYING X1 FROM 2 BY 1 UNTIL                                
03713        X1 IS GREATER THAN DATE-RANGE-MAX.                         
03714      MOVE SPACES                 TO  B-E-AGT-TABLE.               
03715      MOVE +0                     TO  X1.                          
03716      PERFORM 0155-ZERO-DATE-RANGE-ACC THRU 0155-EXIT 15 TIMES.    
03717      PERFORM 2380-ACCOUNT-BREAK    THRU 2540-ZERO-ACCOUNT.        
03718      PERFORM 2570-STATE-BREAK-1    THRU 2610-ZERO-STATE.          
03719      PERFORM 2640-GROUPING-BREAK-1 THRU 2680-ZERO-GROUPING.       
03720                                                                   
03721  2710-CARRIER-BREAK-1.                                            
03722      IF P-CA-SW = '1'                                             
03723          MOVE ' '                TO P-CA-SW                       
03724      ELSE                                                         
03725          GO TO 2750-ZERO-CARRIER.                                 
03726                                                                   
03727      PERFORM 0870-CA-HD          THRU 0870-EXIT.                  
03728      MOVE +1                     TO X1.                           
03729                                                                   
03730  2720-CARRIER-BREAK-2.                                            
03731      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
03732          GO TO 2730-PRINT-LAST-CA-12.                             
03733                                                                   
03734      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03735      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03736      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03737      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03738      MOVE CA-CERT-T (X1)         TO DET-TMS-CERTS.                
03739      MOVE CA-CANCEL-T (X1)       TO DET-TMS-CANCELS.              
03740      MOVE CA-L-COVERAGE-T (X1)   TO DET-TMS-LCOVERAG.             
03741      MOVE CA-A-COVERAGE-T (X1)   TO DET-TMS-ACOVERAG.             
03742      MOVE CA-LPRM-T (X1)         TO DET-TMS-LPRM.                 
03743      MOVE CA-LCAN-T (X1)         TO DET-TMS-LCAN.                 
03744      MOVE CA-LCLM-T (X1)         TO DET-TMS-LCLM.                 
03745      MOVE CA-TPRM-T (X1)         TO DET-TMS-TPRM.                 
03746      MOVE CA-TCOM-T (X1)         TO DET-TMS-TCOM.                 
03747      COMPUTE DET-TMS-NPRM = (CA-TPRM-T (X1) - CA-TCOM-T (X1)).    
03748                                                                   
03749      MOVE CA-APRM-T (X1)         TO DET-TMS-APRM.                 
03750      MOVE CA-ACAN-T (X1)         TO DET-TMS-ACAN.                 
03751      MOVE CA-ACLM-T (X1)         TO DET-TMS-ACLM.                 
03752                                                                   
03753      ADD +1                      TO X1.                           
03754      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   
03755      MOVE COMP-MO (X1)           TO DET-MO                        
03756                                     DET-TMS-MO.                   
03757      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
03758      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03759                                                                   
03760      MOVE ' '                    TO X.                            
03761                                                                   
03762      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03763                                                                   
03764      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03765                                                                   
03766      MOVE ' '                    TO X.                            
03767                                                                   
03768      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03769                                                                   
03770      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03771      MOVE ' '                    TO X.                            
03772      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03773      GO TO 2720-CARRIER-BREAK-2.                                  
03774                                                                   
03775  2730-PRINT-LAST-CA-12.                                           
03776                                                                   
03777      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03778      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03779      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03780      MOVE L12CA-CERT-T           TO DET-TMS-CERTS.                
03781      MOVE L12CA-CANCEL-T         TO DET-TMS-CANCELS.              
03782      MOVE L12CA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03783      MOVE L12CA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03784      MOVE L12CA-APRM-T           TO DET-TMS-APRM.                 
03785      MOVE L12CA-ACAN-T           TO DET-TMS-ACAN.                 
03786      MOVE L12CA-ACLM-T           TO DET-TMS-ACLM.                 
03787      MOVE L12CA-LPRM-T           TO DET-TMS-LPRM.                 
03788      MOVE L12CA-LCAN-T           TO DET-TMS-LCAN.                 
03789      MOVE L12CA-LCLM-T           TO DET-TMS-LCLM.                 
03790      MOVE L12CA-TPRM-T           TO DET-TMS-TPRM.                 
03791      MOVE L12CA-TCOM-T           TO DET-TMS-TCOM.                 
03792                                                                   
03793      COMPUTE DET-TMS-NPRM = (L12CA-TPRM-T  -                      
03794          L12CA-TCOM-T ).                                          
03795                                                                   
03796 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                
03797      MOVE '*12 MO '              TO DET-TMS-DATE.                 
03798      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03799                                                                   
03800      MOVE ' '                    TO X.                            
03801      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03802      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03803      MOVE ' '                    TO X.                            
03804      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03805      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03806                                                                   
03807      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03808      MOVE ' '                    TO X.                            
03809      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03810      MOVE ZERO-ACCUM             TO LAST-12-CARRIER-ACCUM.        
03811      MOVE ZERO-ACCUM-T           TO LAST-12-CARRIER-ACCUM-T.      
03812                                                                   
122804*  BEGIN PL12 PRINT

03815 *    MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03816 *    MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03817 *    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03818 *    MOVE PL12CA-CERT-T           TO DET-TMS-CERTS.                
03819 *    MOVE PL12CA-CANCEL-T         TO DET-TMS-CANCELS.              
03820 *    MOVE PL12CA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03821 *    MOVE PL12CA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03822 *    MOVE PL12CA-LPRM-T           TO DET-TMS-LPRM.                 
03823 *    MOVE PL12CA-LCAN-T           TO DET-TMS-LCAN.                 
03824 *    MOVE PL12CA-LCLM-T           TO DET-TMS-LCLM.                 
03825 *    MOVE PL12CA-TPRM-T           TO DET-TMS-TPRM.                 
03826 *    MOVE PL12CA-TCOM-T           TO DET-TMS-TCOM.                 
03827 *    MOVE PL12CA-APRM-T           TO DET-TMS-APRM.                 
03828 *    MOVE PL12CA-ACAN-T           TO DET-TMS-ACAN.                 
03829 *    MOVE PL12CA-ACLM-T           TO DET-TMS-ACLM.                 
03830 *                                                                 
03831 *    COMPUTE DET-TMS-NPRM = (PL12CA-TPRM-T  -                      
03832 *        PL12CA-TCOM-T).                                           
03833 *                                                                 
03834 *    MOVE 'PRIOR L12   '         TO DET-TMS-TITLE.                
03835 *    MOVE '*PL12  '              TO DET-TMS-DATE.                 
03836 *    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03837 *    MOVE ' '                    TO X.                            
03838 *    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03839 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03840 *                                                                 
03841 *    MOVE SPACES                 TO DET-TMS-TITLE.                
03842 *                                                                 
03843 *    MOVE ' '                    TO X.                            
03844 *    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03845 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03846 *                                                                 
03847 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03848 *    MOVE ' '                    TO X.                            
03849 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03850 *    MOVE ZERO-ACCUM             TO PL12-CARRIER-ACCUM.
03851 *    MOVE ZERO-ACCUM-T           TO PL12-CARRIER-ACCUM-T.

03813  2740-PRINT-CA-YTD.                                               
03814                                                                   
03815      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03816      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03817      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03818      MOVE YTDCA-CERT-T           TO DET-TMS-CERTS.                
03819      MOVE YTDCA-CANCEL-T         TO DET-TMS-CANCELS.              
03820      MOVE YTDCA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03821      MOVE YTDCA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03822      MOVE YTDCA-LPRM-T           TO DET-TMS-LPRM.                 
03823      MOVE YTDCA-LCAN-T           TO DET-TMS-LCAN.                 
03824      MOVE YTDCA-LCLM-T           TO DET-TMS-LCLM.                 
03825      MOVE YTDCA-TPRM-T           TO DET-TMS-TPRM.                 
03826      MOVE YTDCA-TCOM-T           TO DET-TMS-TCOM.                 
03827      MOVE YTDCA-APRM-T           TO DET-TMS-APRM.                 
03828      MOVE YTDCA-ACAN-T           TO DET-TMS-ACAN.                 
03829      MOVE YTDCA-ACLM-T           TO DET-TMS-ACLM.                 
03830                                                                   
03831      COMPUTE DET-TMS-NPRM = (YTDCA-TPRM-T  -                      
03832          YTDCA-TCOM-T).                                           
03833                                                                   
03834 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                
03835      MOVE '*YTD   '              TO DET-TMS-DATE.                 
03836      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03837      MOVE ' '                    TO X.                            
03838      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03839      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03840                                                                   
03841      MOVE SPACES                 TO DET-TMS-TITLE.                
03842                                                                   
03843      MOVE ' '                    TO X.                            
03844      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03845      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03846                                                                   
03847      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03848      MOVE ' '                    TO X.                            
03849      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03850      MOVE ZERO-ACCUM             TO YTD-CARRIER-ACCUM.            
03851      MOVE ZERO-ACCUM-T           TO YTD-CARRIER-ACCUM-T.          
03852                                                                   
122804*  BEGIN PYTD PRINT

03815      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03816      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03817      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03818      MOVE PYTDCA-CERT-T           TO DET-TMS-CERTS.                
03819      MOVE PYTDCA-CANCEL-T         TO DET-TMS-CANCELS.              
03820      MOVE PYTDCA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03821      MOVE PYTDCA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03822      MOVE PYTDCA-LPRM-T           TO DET-TMS-LPRM.                 
03823      MOVE PYTDCA-LCAN-T           TO DET-TMS-LCAN.                 
03824      MOVE PYTDCA-LCLM-T           TO DET-TMS-LCLM.                 
03825      MOVE PYTDCA-TPRM-T           TO DET-TMS-TPRM.                 
03826      MOVE PYTDCA-TCOM-T           TO DET-TMS-TCOM.                 
03827      MOVE PYTDCA-APRM-T           TO DET-TMS-APRM.                 
03828      MOVE PYTDCA-ACAN-T           TO DET-TMS-ACAN.                 
03829      MOVE PYTDCA-ACLM-T           TO DET-TMS-ACLM.                 
03830                                                                   
03831      COMPUTE DET-TMS-NPRM = (PYTDCA-TPRM-T  -                      
03832          PYTDCA-TCOM-T).                                           
03833                                                                   
03834 *    MOVE 'PRIOR YTD   '         TO DET-TMS-TITLE.                
03835      MOVE '*PYTD  '              TO DET-TMS-DATE.                 
03836      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03837      MOVE ' '                    TO X.                            
03838      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03839      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03840                                                                   
03841      MOVE SPACES                 TO DET-TMS-TITLE.                
03842                                                                   
03843      MOVE ' '                    TO X.                            
03844      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03845      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03846                                                                   
03847 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03848 *    MOVE ' '                    TO X.                            
03849 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03850      MOVE ZERO-ACCUM             TO PYTD-CARRIER-ACCUM.
03851      MOVE ZERO-ACCUM-T           TO PYTD-CARRIER-ACCUM-T.

122804*  BEGIN ITD PRINT

03815 *    MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03816 *    MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03817 *    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03818 *    MOVE ITDCA-CERT-T           TO DET-TMS-CERTS.                
03819 *    MOVE ITDCA-CANCEL-T         TO DET-TMS-CANCELS.              
03820 *    MOVE ITDCA-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03821 *    MOVE ITDCA-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03822 *    MOVE ITDCA-LPRM-T           TO DET-TMS-LPRM.                 
03823 *    MOVE ITDCA-LCAN-T           TO DET-TMS-LCAN.                 
03824 *    MOVE ITDCA-LCLM-T           TO DET-TMS-LCLM.                 
03825 *    MOVE ITDCA-TPRM-T           TO DET-TMS-TPRM.                 
03826 *    MOVE ITDCA-TCOM-T           TO DET-TMS-TCOM.                 
03827 *    MOVE ITDCA-APRM-T           TO DET-TMS-APRM.                 
03828 *    MOVE ITDCA-ACAN-T           TO DET-TMS-ACAN.                 
03829 *    MOVE ITDCA-ACLM-T           TO DET-TMS-ACLM.                 
03830 *                                                                 
03831 *    COMPUTE DET-TMS-NPRM = (ITDCA-TPRM-T  -                      
03832 *        ITDCA-TCOM-T).                                           
03833 *                                                                 
03834 *    MOVE 'INCEP TO DT '         TO DET-TMS-TITLE.                
03835 *    MOVE '*ITD   '              TO DET-TMS-DATE.                 
03836 *    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03837 *    MOVE ' '                    TO X.                            
03838 *    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03839 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03840 *                                                                 
03841 *    MOVE SPACES                 TO DET-TMS-TITLE.                
03842 *                                                                 
03843 *    MOVE ' '                    TO X.                            
03844 *    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03845 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03846 *                                                                 
03847 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03848 *    MOVE ' '                    TO X.                            
03849 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03850 *    MOVE ZERO-ACCUM             TO ITD-CARRIER-ACCUM.            
03851 *    MOVE ZERO-ACCUM-T           TO ITD-CARRIER-ACCUM-T.          

03853  2750-ZERO-CARRIER.                                               
03854      MOVE +0                     TO X1.                           
03855      PERFORM 0180-ZERO-ACCUM-CARR THRU 0180-EXIT 15 TIMES.        
03856  EJECT                                                            
03857  2770-PRINT-DECISION.                                             
03858      MOVE +1                     TO X1.                           
03859      MOVE +0                     TO Y1.                           
03860                                                                   
03861  2780-PRINT-DECISION-1.                                           
03862      ADD 1                       TO X1                            
03863                                     Y1.                           
03864                                                                   
122804*    IF X1 GREATER THAN DATE-RANGE-MAX                            
122804     IF X1 > +13
03866          MOVE +0                 TO X1                            
03867          GO TO 2790-PRINT-DECISION-2.                             
03868                                                                   
03869      COMPUTE LPRM-T = AC-LPRM-T (X1) - AC-LPRM-T (Y1).            
03870      COMPUTE LCAN-T = AC-LCAN-T (X1) - AC-LCAN-T (Y1).            
03871      COMPUTE LCLM-T = AC-LCLM-T (X1) - AC-LCLM-T (Y1).            
03872      COMPUTE L-COVERAGE-T = AC-L-COVERAGE-T (X1) -                
03873                       AC-L-COVERAGE-T (Y1).                       
03874      COMPUTE APRM-T = AC-APRM-T (X1) - AC-APRM-T (Y1).            
03875      COMPUTE ACAN-T = AC-ACAN-T (X1) - AC-ACAN-T (Y1).            
03876      COMPUTE ACLM-T = AC-ACLM-T (X1) - AC-ACLM-T (Y1).            
03877      COMPUTE A-COVERAGE-T = AC-A-COVERAGE-T (X1) -                
03878                       AC-A-COVERAGE-T (Y1).                       
03879      COMPUTE TCOM-T = AC-TCOM-T (X1) - AC-TCOM-T (Y1).            
03880                                                                   
03881      IF COMP-CCYY (X1) = CONV-CCYY AND                            
03882         COMP-MO (X1) = CONV-MO                                    
03883           MOVE ZEROS         TO LCAN-T                            
03884                                 LPRM-T                            
03885                                 LCLM-T                            
03886                                 L-COVERAGE-T                      
03887                                 A-COVERAGE-T                      
03888                                 APRM-T                            
03889                                 ACAN-T                            
03890                                 ACLM-T                            
03891                                 TCOM-T.                           
03892                                                                   
03893      MOVE LPRM-T                 TO PLPRM-T (Y1).                 
03894      MOVE LCAN-T                 TO PLCAN-T (Y1).                 
03895      MOVE LCLM-T                 TO PLCLM-T (Y1).                 
03896      MOVE APRM-T                 TO PAPRM-T (Y1).                 
03897      MOVE ACAN-T                 TO PACAN-T (Y1).                 
03898      MOVE ACLM-T                 TO PACLM-T (Y1).                 
03899      MOVE TCOM-T                 TO PTCOM-T (Y1).                 
03900      GO TO 2780-PRINT-DECISION-1.                                 
03901                                                                   
03902  2790-PRINT-DECISION-2.                                           
03903      ADD +1                      TO X1.                           
03904                                                                   
03905      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
03906          GO TO 2800-EXIT.                                         
03907                                                                   
03908      IF (PRINT-ZERO-TABLE-T NOT = PRINT-TABLE-T (X1))
010306        OR ((AC-LPRM-T (1) - AC-LPRM-T (15)) NOT = ZEROS)
010306        OR ((AC-LCAN-T (1) - AC-LCAN-T (15)) NOT = ZEROS)
010306        OR ((AC-LCLM-T (1) - AC-LCLM-T (15)) NOT = ZEROS)
010306        OR ((AC-APRM-T (1) - AC-APRM-T (15)) NOT = ZEROS)
010306        OR ((AC-ACAN-T (1) - AC-ACAN-T (15)) NOT = ZEROS)
010306        OR ((AC-ACLM-T (1) - AC-ACLM-T (15)) NOT = ZEROS)
03909          MOVE '1'             TO P-ACC-SW                         
03910                                  P-ST-SW                          
03911                                  P-GRP-SW                         
03912                                  P-CA-SW                          
03913          GO TO 2800-EXIT.                                         
03914                                                                   


03869      COMPUTE LPRM-T = AC-LPRM-T (X1) - AC-LPRM-T (Y1).            
03870      COMPUTE LCAN-T = AC-LCAN-T (X1) - AC-LCAN-T (Y1).            
03871      COMPUTE LCLM-T = AC-LCLM-T (X1) - AC-LCLM-T (Y1).            
03872      COMPUTE L-COVERAGE-T = AC-L-COVERAGE-T (X1) -                
03873                       AC-L-COVERAGE-T (Y1).                       
03874      COMPUTE APRM-T = AC-APRM-T (X1) - AC-APRM-T (Y1).            
03875      COMPUTE ACAN-T = AC-ACAN-T (X1) - AC-ACAN-T (Y1).            
03876      COMPUTE ACLM-T = AC-ACLM-T (X1) - AC-ACLM-T (Y1).            


03915      GO TO 2790-PRINT-DECISION-2.                                 
03916                                                                   
03917  2800-EXIT.                                                       
03918       EXIT.                                                       
03919                                                                   
03920  EJECT                                                            
03921  2960-PRINT-GRAND-TOTALS.                                         
03922      IF DTE-FICH = '1'                                            
03923          MOVE '2'                TO DTE-FICH.                     
CIDMOD
CIDMOD     MOVE  'Y'  TO  PRNT-TOT-SW.
03924                                                                   
03925      PERFORM 0880-GR-HD          THRU 0880-EXIT.                  
03926      MOVE +1                     TO X1.                           
03927                                                                   
03928  2970-GRAND-TOTAL-1.                                              
03929      IF X1 GREATER THAN DATE-RANGE-MAX-1                          
03930          GO TO 2980-GRAND-TOTAL-2.                                
03931                                                                   
03932      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03933      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03934      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03935      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03936      MOVE GT-CERT-T (X1)         TO DET-TMS-CERTS.                
03937      MOVE GT-CANCEL-T (X1)       TO DET-TMS-CANCELS.              
03938      MOVE GT-L-COVERAGE-T (X1)   TO DET-TMS-LCOVERAG.             
03939      MOVE GT-A-COVERAGE-T (X1)   TO DET-TMS-ACOVERAG.             
03940      MOVE GT-LPRM-T (X1)         TO DET-TMS-LPRM.                 
03941      MOVE GT-LCAN-T (X1)         TO DET-TMS-LCAN.                 
03942                                                                   
03943      IF X1 = 12                                                   
03944          MOVE GT-LPRM (12)       TO ME-035-NET-L                  
03945          MOVE GT-APRM (12)       TO ME-035-NET-AH.                
03946                                                                   
03947      MOVE GT-LCLM-T (X1)         TO DET-TMS-LCLM.                 
03948      MOVE GT-APRM-T (X1)         TO DET-TMS-APRM.                 
03949      MOVE GT-ACAN-T (X1)         TO DET-TMS-ACAN.                 
03950      MOVE GT-ACLM-T (X1)         TO DET-TMS-ACLM.                 
03951      MOVE GT-TPRM-T (X1)         TO DET-TMS-TPRM.                 
03952      MOVE GT-TCOM-T (X1)         TO DET-TMS-TCOM.                 
03953      COMPUTE DET-TMS-NPRM = (GT-TPRM-T (X1) -                     
03954          GT-TCOM-T (X1)).                                         
03955                                                                   
03956      ADD +1                      TO X1.                           
03957      MOVE COMP-YR (X1)           TO DET-TMS-YR.                   
03958      MOVE COMP-MO (X1)           TO DET-TMS-MO                    
03959                                     DET-MO.                       
03960      PERFORM 0940-LOAD-ALPHA-MONTH THRU 0950-EXIT.                
03961      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
03962                                                                   
CIDMOD*    MOVE '0'                    TO X.                            
CIDMOD     MOVE ' '                    TO X.                            
03964                                                                   
03965      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03966      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
03967                                                                   
03968      MOVE ' '                    TO X.                            
03969                                                                   
03970      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03971      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
03972      MOVE ' '                    TO X.                            
03973      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
03974      GO TO 2970-GRAND-TOTAL-1.                                    
03975                                                                   
03976  2980-GRAND-TOTAL-2.                                              
03977      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
03978      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
03979      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
03980 *    MOVE 'LAST 12 MO'           TO DET-TMS-TITLE.                
03981      MOVE '*12 MO '              TO DET-TMS-DATE.                 
03982      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03983      MOVE ' '                    TO X.                            
03984      MOVE L12GD-CERT-T           TO DET-TMS-CERTS.                
03985      MOVE L12GD-CANCEL-T         TO DET-TMS-CANCELS.              
03986      MOVE L12GD-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
03987      MOVE L12GD-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
03988      MOVE L12GD-LPRM-T           TO DET-TMS-LPRM.                 
03989      MOVE L12GD-LCAN-T           TO DET-TMS-LCAN.                 
03990      MOVE L12GD-LCLM-T           TO DET-TMS-LCLM.                 
03991      MOVE L12GD-APRM-T           TO DET-TMS-APRM.                 
03992      MOVE L12GD-ACLM-T           TO DET-TMS-ACLM.                 
03993      MOVE L12GD-ACAN-T           TO DET-TMS-ACAN.                 
03994      MOVE L12GD-TPRM-T           TO DET-TMS-TPRM.                 
03995      MOVE L12GD-TCOM-T           TO DET-TMS-TCOM.                 
03996      COMPUTE DET-TMS-NPRM = (L12GD-TPRM-T  -                      
03997          L12GD-TCOM-T).                                           
03998                                                                   
03999      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
04000      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04001                                                                   
04002      MOVE ' '                    TO X.                            
04003      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
04004      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04005      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
04006      MOVE ' '                    TO X.                            
04007      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     

122804*  BEGIN PL12 GRAND

04009 *    MOVE SPACES                 TO DETAIL-TMS-LINE-1
04010 *    MOVE SPACES                 TO DETAIL-TMS-LINE-2
04011 *    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC
04012 *    MOVE 'PRIOR L12   '         TO DET-TMS-TITLE
04013 *    MOVE '*PL12  '              TO DET-TMS-DATE
04014 *    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC
04015 *    MOVE ' '                    TO X.                            
04016 *    MOVE PL12GD-CERT-T           TO DET-TMS-CERTS
04017 *    MOVE PL12GD-CANCEL-T         TO DET-TMS-CANCELS
04018 *    MOVE PL12GD-L-COVERAGE-T     TO DET-TMS-LCOVERAG
04019 *    MOVE PL12GD-A-COVERAGE-T     TO DET-TMS-ACOVERAG
04020 *    MOVE PL12GD-LPRM-T           TO DET-TMS-LPRM
04021 *    MOVE PL12GD-LCAN-T           TO DET-TMS-LCAN
04022 *    MOVE PL12GD-LCLM-T           TO DET-TMS-LCLM
04023 *    MOVE PL12GD-APRM-T           TO DET-TMS-APRM
04024 *    MOVE PL12GD-ACAN-T           TO DET-TMS-ACAN
04025 *    MOVE PL12GD-ACLM-T           TO DET-TMS-ACLM
04026 *    MOVE PL12GD-TPRM-T           TO DET-TMS-TPRM
04027 *    MOVE PL12GD-TCOM-T           TO DET-TMS-TCOM
04028 *    COMPUTE DET-TMS-NPRM = (PL12GD-TPRM-T  -
04029 *        PL12GD-TCOM-T)
04030 *                                                                 
04031 *    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
04032 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04033 *                                                                 
04034 *    MOVE ' '                    TO X.                            
04035 *    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
04036 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04037 *                                                                 
04038 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
04039 *    MOVE ' '                    TO X.                            
04040 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     


04008                                                                   
04009      MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
04010      MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
04011      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
04012 *    MOVE 'YEAR TO DATE'         TO DET-TMS-TITLE.                
04013      MOVE '*YTD   '              TO DET-TMS-DATE.                 
04014      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
04015      MOVE ' '                    TO X.                            
04016      MOVE YTDGD-CERT-T           TO DET-TMS-CERTS.                
04017      MOVE YTDGD-CANCEL-T         TO DET-TMS-CANCELS.              
04018      MOVE YTDGD-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
04019      MOVE YTDGD-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
04020      MOVE YTDGD-LPRM-T           TO DET-TMS-LPRM.                 
04021      MOVE YTDGD-LCAN-T           TO DET-TMS-LCAN.                 
04022      MOVE YTDGD-LCLM-T           TO DET-TMS-LCLM.                 
04023      MOVE YTDGD-APRM-T           TO DET-TMS-APRM.                 
04024      MOVE YTDGD-ACAN-T           TO DET-TMS-ACAN.                 
04025      MOVE YTDGD-ACLM-T           TO DET-TMS-ACLM.                 
04026      MOVE YTDGD-TPRM-T           TO DET-TMS-TPRM.                 
04027      MOVE YTDGD-TCOM-T           TO DET-TMS-TCOM.                 
04028      COMPUTE DET-TMS-NPRM = (YTDGD-TPRM-T  -                      
04029          YTDGD-TCOM-T).                                           
04030                                                                   
04031      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
04032      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04033                                                                   
04034      MOVE ' '                    TO X.                            
04035      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
04036      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04037                                                                   
04038      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
04039      MOVE ' '                    TO X.                            
04040      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     

122804*  BEGIN PYTD GRAND

04009      MOVE SPACES                 TO DETAIL-TMS-LINE-1
04010      MOVE SPACES                 TO DETAIL-TMS-LINE-2
04011      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC
04012 *    MOVE 'PRIOR YTD   '         TO DET-TMS-TITLE
04013      MOVE '*PYTD  '              TO DET-TMS-DATE
04014      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC
04015      MOVE ' '                    TO X.                            
04016      MOVE PYTDGD-CERT-T           TO DET-TMS-CERTS
04017      MOVE PYTDGD-CANCEL-T         TO DET-TMS-CANCELS
04018      MOVE PYTDGD-L-COVERAGE-T     TO DET-TMS-LCOVERAG
04019      MOVE PYTDGD-A-COVERAGE-T     TO DET-TMS-ACOVERAG
04020      MOVE PYTDGD-LPRM-T           TO DET-TMS-LPRM
04021      MOVE PYTDGD-LCAN-T           TO DET-TMS-LCAN
04022      MOVE PYTDGD-LCLM-T           TO DET-TMS-LCLM
04023      MOVE PYTDGD-APRM-T           TO DET-TMS-APRM
04024      MOVE PYTDGD-ACAN-T           TO DET-TMS-ACAN
04025      MOVE PYTDGD-ACLM-T           TO DET-TMS-ACLM
04026      MOVE PYTDGD-TPRM-T           TO DET-TMS-TPRM
04027      MOVE PYTDGD-TCOM-T           TO DET-TMS-TCOM
04028      COMPUTE DET-TMS-NPRM = (PYTDGD-TPRM-T  -
04029          PYTDGD-TCOM-T)
04030                                                                   
04031      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
04032      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04033                                                                   
04034      MOVE ' '                    TO X.                            
04035      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
04036      PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04037                                                                   
04038 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
04039 *    MOVE ' '                    TO X.                            
04040 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     

122804*  BEGIN ITD GRAND

04009 *    MOVE SPACES                 TO DETAIL-TMS-LINE-1.            
04010 *    MOVE SPACES                 TO DETAIL-TMS-LINE-2.            
04011 *    MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
04012 *    MOVE 'INCEP TO DT '         TO DET-TMS-TITLE.                
04013 *    MOVE '*ITD   '              TO DET-TMS-DATE.                 
04014 *    MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
04015 *    MOVE ' '                    TO X.                            
04016 *    MOVE ITDGD-CERT-T           TO DET-TMS-CERTS.                
04017 *    MOVE ITDGD-CANCEL-T         TO DET-TMS-CANCELS.              
04018 *    MOVE ITDGD-L-COVERAGE-T     TO DET-TMS-LCOVERAG.             
04019 *    MOVE ITDGD-A-COVERAGE-T     TO DET-TMS-ACOVERAG.             
04020 *    MOVE ITDGD-LPRM-T           TO DET-TMS-LPRM.                 
04021 *    MOVE ITDGD-LCAN-T           TO DET-TMS-LCAN.                 
04022 *    MOVE ITDGD-LCLM-T           TO DET-TMS-LCLM.                 
04023 *    MOVE ITDGD-APRM-T           TO DET-TMS-APRM.                 
04024 *    MOVE ITDGD-ACAN-T           TO DET-TMS-ACAN.                 
04025 *    MOVE ITDGD-ACLM-T           TO DET-TMS-ACLM.                 
04026 *    MOVE ITDGD-TPRM-T           TO DET-TMS-TPRM.                 
04027 *    MOVE ITDGD-TCOM-T           TO DET-TMS-TCOM.                 
04028 *    COMPUTE DET-TMS-NPRM = (ITDGD-TPRM-T  -                      
04029 *        ITDGD-TCOM-T).                                           
04030 *                                                                 
04031 *    MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
04032 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04033 *                                                                 
04034 *    MOVE ' '                    TO X.                            
04035 *    MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
04036 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     
04037 *                                                                 
04038 *    MOVE DETAIL-TMS-LINE-3      TO P-DATA.                       
04039 *    MOVE ' '                    TO X.                            
04040 *    PERFORM 0920-WRITE-PRINT THRU 0930-EXIT.                     

04041  2990-EXIT.                                                       
04042      EXIT.                                                        
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-PRT.                                                
CIDMOD                                                                  
CIDMOD     IF  DIS-HEAD-SW =  'Y'                                       
CIDMOD       MOVE 'N' TO  DIS-HEAD-SW                                   
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT                                         
CIDMOD           GO TO 8600-DISPLAY-EXIT.                               
CIDMOD                                                                  
CIDMOD     IF  DIS-LINE-CNT GREATER THAN 59                             
CIDMOD         PERFORM 8600-DISPLAY-HD THRU                             
CIDMOD             8600-HD-EXIT.                                        
CIDMOD                                                                  
CIDMOD     MOVE   SPACES TO DIS-CC.                                     
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-LINE.                        
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     MOVE   SPACES TO DISPLAY-LINE.                               
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-EXIT.                                               
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD 8600-DISPLAY-HD.                                                 
CIDMOD                                                                  
CIDMOD     MOVE '1' TO  DISPLAY-CC.                                     
CIDMOD     MOVE ZEROS TO DIS-LINE-CNT.                                  
CIDMOD     WRITE DISPLAY-REC FROM DISPLAY-HD-1.                         
CIDMOD     ADD  +1  TO DIS-LINE-CNT.                                    
CIDMOD     MOVE ' ' TO  DISPLAY-CC.                                     
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD*    WRITE  DISPLAY-REC.                                          
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-HD-2.                        
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     WRITE  DISPLAY-REC.                                          
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-HD-EXIT.                                                    
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
CIDMOD                                                                  
04043  EJECT                                                            
04044  9990-END-OF-JOB.                                                 
CIDMOD                                                                  
CIDMOD         DISPLAY ' '.                                             
CIDMOD         DISPLAY '* * * * * * * * * * * * * * * * * *'.           
CIDMOD         DISPLAY ' DISPLAY ERROR COUNT = '  ERROR-COUNT.          
CIDMOD         DISPLAY '* * * * * * * * * * * * * * * * * *'.           
CIDMOD         DISPLAY ' '.                                             
CIDMOD                                                                  
CIDMOD         PERFORM 8600-DISPLAY-HD  THRU                            
CIDMOD               8600-HD-EXIT.                                      
CIDMOD                                                                  
CIDMOD         MOVE ' DISPLAY ERROR COUNT = '    TO DIS-LINE-REASON     
CIDMOD         MOVE ERROR-COUNT                TO  DIS-LINE-REC         
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT.                                 
CIDMOD                                                                  
CIDMOD                                                                  
04045      IF (DTE-CLIENT EQUAL 'TMS'                                   
04046         OR DTE-FMT-OPT EQUAL '2')                                 
04047          PERFORM 2700-CARRIER-BREAK  THRU 2750-ZERO-CARRIER       
04048          PERFORM 2960-PRINT-GRAND-TOTALS THRU 2990-EXIT           
04049      ELSE                                                         
04050          PERFORM 0700-CARRIER-BREAK  THRU 0750-ZERO-CARRIER       
04051          PERFORM 0960-PRINT-GRAND-TOTALS THRU 0990-EXIT.          
04052                                                                   
04053                                                                   
04054  9999-E-O-J.                                                      
04055                              COPY ELCPRTC.                        
04056                                                                   
04057      CLOSE AM-MAST-IN                                             
04058            EARNED-PREM                                            
04059            EXTRACT-OT                                             
CIDMOD           DISPLAY-PRT
101504           RPTCDE1
04060            PRNT.                                                  
04061                                                                   
04062      IF ME-DO-UPDATE                                              
04063 *        MOVE ME-START-TIME      TO ME-035-START
04064          MOVE ME-CNDS-DATE       TO ME-035-RUN-DT                 
04065          ACCEPT WS-TIME-OF-DAY   FROM TIME                        
04066 *        MOVE WS-TIME            TO ME-035-END
04067          ADD 1                   TO ME-035-RUN-CT                 
04068          REWRITE MONTH-END-BALANCES                               
04069          CLOSE ERMEBL.                                            
04070                                                                   
04071      GOBACK.                                                      
04072                                                                   
04073                                                                   
04074  ABEND-PGM SECTION.                                               
04075                                  COPY ELCABEND.                   
04076                                                                   
