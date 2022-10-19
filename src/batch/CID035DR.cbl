00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 CID035DR.                              
00009                                                                   
00010 *AUTHOR.        LOGIC, INC.                                       
00011 *               DALLAS, TEXAS.                                    
00012                                                                   
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANSE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
082509* 082509    2009030400001  AJRA  ECS035 REPORTS WITH TOTALS
051711* 051711    2011010400001  AJRA  FIX PTYD ST AH PREM
101920* 101920  CR2020100800001  PEMA  ADD ECS035H (REPORT CODE 3)
060122* 060122  IR2022053100001  PEMA  Fix printing of totals
      ******************************************************************
00024 *REMARKS.                                                         
00025 *        PRINTS MONTHLY PRODUCTION REPORTS FOR LAST 12 MONTHS -   
00026 *                                                                 
00030 *  PASS 4 - REPORT CODE 1 .............................ECS035D  
00031 *  PASS 5 - REPORT CODE 2 .............................ECS035E
      *  PASS 6 - USER SELECT CODE 2 ........................ECS035F  
101920*  PASS 8 - REPORT CODE 3 .............................ECS035H

00050  EJECT                                                            
00051  ENVIRONMENT DIVISION.                                            
00052  CONFIGURATION SECTION.                                           
00053                                                                   
00054  INPUT-OUTPUT SECTION.                                            
00055  FILE-CONTROL.                                                    
00056                                                                   
00057      SELECT SORT-FILE    ASSIGN TO SYS001.      
00058      SELECT EARNED-PREM  ASSIGN TO SYS012-UT-FBA1-S-SYS012.       
00059      SELECT PRNT         ASSIGN TO SYS008-UR-1403-S-SYS008.       
00060      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       
00061      SELECT FICH         ASSIGN TO SYS020-UT-2400-S-SYS020.       
00062                                                                   
00063  DATA DIVISION.                                                   
00064  FILE SECTION.                                                    
00065                                                                   
00066  SD  SORT-FILE.                                                   
00067                                                                   
00068  01  SORT-FILE-REC.                                               
00069      05 SORT-CONTROL-74          PIC X(30).                       
CIDMOD     05  SORT-DATE               PIC X(04).                       
CIDMOD     05  FILLER                  PIC X(103).                       
CIDMOD     05  SORT-AM-EXPIRE          PIC X(06).
CIDMOD     05  SORT-AM-HI-CERT         PIC X(06).
CIDMOD     05  SORT-HI-CERT            PIC X(06).                       
00068      05  FILLER                  PIC X(36).                       
00071                                                                   
00072  FD  EARNED-PREM                                                  
00073      BLOCK CONTAINS 0 RECORDS
00074      RECORDING MODE F.                                            
00075                                                                   
CIDMOD 01  EP-EXTR                     PIC X(191).                      
00077                                                                   
00078  FD  PRNT                                                         
00079      COPY ELCPRTFD.                                               
00080  EJECT                                                            
00081  FD  DISK-DATE                                                    
00082      COPY ELCDTEFD.                                               
00083  EJECT                                                            
00084  FD  FICH                                                         
00085      COPY ELCFCHFD.                                               
00086  EJECT                                                            
00087  WORKING-STORAGE SECTION.                                         
00088  77  FILLER  PIC X(32) VALUE '********************************'.  
00089  77  FILLER  PIC X(32) VALUE '     ECS036 WORKING STORAGE     '.  
00090  77  FILLER  PIC X(32) VALUE '******* VMOD=2.014 *************'.  
00091                                                                   
CIDMOD 77  SAVE-NAME                   PIC X(07)           VALUE SPACE. 
CIDMOD 77  PRINT-SELECT-SW             PIC X(03)           VALUE 'NO '. 
CIDMOD 77  CC-SW                       PIC X               VALUE SPACE. 
00092  77  STATE-TOTAL-LINE            PIC X(30)           VALUE SPACE. 
00093  77  SAVE-STATE                  PIC XX              VALUE SPACE. 
00094  77  SKIP-HD-SW                  PIC X               VALUE SPACE. 
00095  77  PASS-NUMBER                 PIC X               VALUE '1'.   
00096  77  FIRST-TIME-SWITCH           PIC X               VALUE 'Y'.   
00097      88  FIRST-TIME                                  VALUE 'Y'.   
00098  77  return-count                PIC S999  COMP-3    VALUE ZEROS. 
00098  77  LINE-CNT                    PIC S999  COMP-3    VALUE ZEROS. 
00099  77  PAGE-CNT                    PIC S9(5) COMP-3    VALUE ZEROS. 
00100  77  PGM-SUB                     PIC S999  COMP      VALUE +036.  
00101  77  X                           PIC X.                           
00102  77  X1                          PIC S999  COMP      VALUE ZERO.  
122804 77  DISP-X1                     PIC 999             VALUE ZEROS.
00103  77  Y1                          PIC S999  COMP      VALUE ZERO.  
00104  77  EOF-EPX                     PIC X               VALUE SPACE. 
00105  77  WRITE-AC-SW                 PIC X               VALUE SPACE. 
00106  77  ST-SUB                      PIC 99              VALUE ZEROS. 
00107  77  DT-SUB                      PIC S99   COMP      VALUE ZEROS. 
00108  77  WORK-BUSC                   PIC XX.                          
00109  77  SET-CTR                     PIC S999  COMP-3    VALUE ZERO.  
00113  77  CARRIER-L                   PIC X            VALUE LOW-VALUE.
00110                                                                   
00111  01  QTR-SW                      PIC X               VALUE SPACE. 
00112      88  QTR-END                                     VALUE '1'.   
00113                                                                   
00114  01  QTR-COMP                    PIC X               VALUE SPACE. 
00115      88  QTR-CO                                      VALUE '1'.   
00116  EJECT                                                            
00117  01  WS-EXTR-REC.                                                 
00118      05  EP-PASS-NO              PIC X.                           
00119      05  EP-CNTL.                                                 
00128          10  EP-A-BREAK-CD       PIC X(10).                       
00129          10  EP-A-CARR           PIC X.                           
00130          10  EP-A-GROUP          PIC X(6).                        
00132          10  EP-A-STATE          PIC XX.                          
00133          10  EP-A-ACCT           PIC X(10).                       
00134      05  EP-DTE                  PIC 9(07)  COMP-3.               
00135      05  EP-CNTRS    COMP-3.                                      
00136          10  EP-CERT             PIC S9(9).                   
00136          10  EP-CANCEL           PIC S9(9).                   
00136          10  EP-LCOVERAGE        PIC S9(9).                   
00136          10  EP-ACOVERAGE        PIC S9(9).                   
00138          10  EP-LPRM             PIC S9(9)V99.                   
00137          10  EP-LCAN             PIC S9(9)V99.                   
00139          10  EP-LCLM             PIC S9(9)V99.                   
00141          10  EP-APRM             PIC S9(9)V99.                   
00140          10  EP-ACAN             PIC S9(9)V99.                   
00142          10  EP-ACLM             PIC S9(9)V99.                   
00143          10  EP-TPRM             PIC S9(9)V99.                   
00144          10  EP-TCOM             PIC S9(9)V99.                   
102004     05  FILLER                  PIC X(4).                        
102004     05  EP-ACCT-STATUS          PIC X.
00146      05  EP-ACCT-NAME            PIC X(30).                       
00147      05  EP-ACC-EXPIRES          PIC 9(11)  COMP-3.               
00148      05  EP-ACC-HI-CERT          PIC 9(11)  COMP-3.               
00149      05  EP-MTH-HI-CERT          PIC 9(11)  COMP-3.               
00150      05  EP-ISS-CNT              PIC S9(9)  COMP-3.                
00151      05  FILLER                  PIC X.                           
CIDMOD     05  EP-ACCT-CITY            PIC X(30).                       
00152                                                                   
00153  01  PRINT-SWITCHES.                                              
00154      05  P-ACC-SW                PIC X VALUE ' '.                 
00155      05  P-ST-SW                 PIC X VALUE ' '.                 
00156      05  P-GP-SW                 PIC X VALUE ' '.                 
00157      05  P-CA-SW                 PIC X VALUE ' '.                 
00158      05  P-RP-SW                 PIC X VALUE ' '.                 
00159                                                                   
00160  01  RUN-DT.                                                      
00161      05  RUN-DT-CCYY             PIC 9(04) VALUE ZEROS.           
00162      05  RUN-DT-CCYR REDEFINES RUN-DT-CCYY.                       
00163          10  RUN-DT-CC           PIC 99.                          
00164          10  RUN-DT-YR           PIC 99.                          
00165      05  RUN-DT-MO               PIC 99  VALUE ZEROS.             
00166                                                                   
00167  01  RUN9DT REDEFINES RUN-DT     PIC 9(6).                        
00168                                                                   
00169  01  WORK-ACCUM.                                                  
00170      05  CERT                    PIC S9(9)     COMP-3 VALUE ZEROS.
00171      05  CANCELS                 PIC S9(9)     COMP-3 VALUE ZEROS.
00171      05  LCOVERAGE               PIC S9(9)     COMP-3 VALUE ZEROS.
00171      05  ACOVERAGE               PIC S9(9)     COMP-3 VALUE ZEROS.
00173      05  LPRM                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00172      05  LCAN                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00174      05  LCLM                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00176      05  APRM                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00175      05  ACAN                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00177      05  ACLM                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00178      05  TPRM                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00179      05  TCOM                    PIC S9(9)V99  COMP-3 VALUE ZEROS.
00180                                                                   
00181  01  REPT-ACCUM.                                                  
00170      05  RPCERT                  PIC S9(9)     COMP-3 VALUE ZEROS.
00171      05  RPCANCEL                PIC S9(9)     COMP-3 VALUE ZEROS.
00171      05  RPLCOVERAGE             PIC S9(9)     COMP-3 VALUE ZEROS.
00171      05  RPACOVERAGE             PIC S9(9)     COMP-3 VALUE ZEROS.
00184      05  RPLPRM                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00183      05  RPLCAN                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00185      05  RPLCLM                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00187      05  RPAPRM                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00186      05  RPACAN                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00188      05  RPACLM                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00189      05  RPTPRM                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00190      05  RPTCOM                  PIC S9(9)V99  COMP-3 VALUE ZEROS.
00191  EJECT                                                            
00219  01  HD-1-RPT-CODE.                                             
112503     05  HD-1-SLCT-FLD-1         PIC X(11)   VALUE 'RPT CODE 1 '.
112503     05  HD-1-SLCT-FLD-2         PIC X(10)   VALUE SPACES.
112503     05  HD-1-SLCT-REC-POS1      PIC X(01)   VALUE SPACE.
112503     05  FILLER                  PIC X(22)   VALUE SPACES.
00221      05  HD-1-RC                 PIC X(9)    VALUE ' MONTHLY'.    
00222      05  HD-1-RPT-CD             PIC X(10)   VALUE SPACES.        
00223      05  FILLER                  PIC X(18)                        
00224                                  VALUE ' PRODUCTION REPORT'.      
122304     05  FILLER                  PIC X(05)   VALUE SPACES.
122304     05  HD-1-RC1-SINGLE-FEE     PIC X(21)   VALUE SPACES.
122304     05  FILLER                  PIC X(12)   VALUE SPACES.
00226      05  FILLER                  PIC X(6)    VALUE 'ECS035'.
           05  HD-1-RPT-SUF            PIC X(1)    VALUE 'D'.
00228                                                                   
00261  01  HD-1-GRAND-TOTALS.                                           
00262      05  FILLER          PIC X(47)   VALUE SPACES.                
00263      05  FILLER          PIC X(20)   VALUE 'PRODUCTION REPORT GR'.
00264      05  FILLER          PIC X(10)   VALUE 'AND TOTALS'.          
00265      05  FILLER          PIC X(42)   VALUE SPACES.                
00266      05  FILLER          PIC X(6)    VALUE 'ECS035'.              
           05  HD-1-GT-SUF     PIC X(1)    VALUE 'D'.
00267                                                                   
00268  01  HD-2.                                                        
00269      05  FILLER          PIC X(47)   VALUE SPACES.                
00270      05  HD-COMPANY-NAME PIC X(30)   VALUE SPACES.                
00271      05  FILLER          PIC X(42)   VALUE SPACES.                
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
00281  01  HD-4-RPT.                                                        
00282      05  FILLER          PIC X(10)   VALUE 'TOTAL FOR '.          
00283      05  HD4-TOTAL-RPT   PIC X(12)   VALUE 'RPT CODE 1  '.              
00284      05  HD4-RPT-CD      PIC X(10)    VALUE SPACES.                
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
00410      05  DET-TMS-LCOVERAGE       PIC Z,ZZZ,Z99-.                  
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
00427      05  DET-TMS-ACOVERAGE       PIC Z,ZZZ,Z99-.                  
00428      05  DET-TMS-APRM            PIC ZZ,ZZZ,ZZ9.99-.              
00429      05  FILLER                  PIC X.                           
00430      05  DET-TMS-ACAN            PIC ZZ,ZZZ,ZZ9.99-.              
00431      05  FILLER                  PIC X(47).                       
00432      05  DET-TMS-ACLM            PIC ZZ,ZZZ,ZZ9.99-.              
00433                                                                   
00434  01  DETAIL-TMS-LINE-3.                                           
00435      05  FILLER                  PIC X(131)      VALUE ALL '-'.   
00436                                                                   
00426  EJECT                                                            
00427  01  ACCUMULATORS.                                                
00428 *    12  ACCOUNT-ACCUM    OCCURS 13 TIMES.                        
122804     12  ACCOUNT-ACCUM    OCCURS 15 TIMES.                        
00429          16  AC-CERT             PIC S9(9)         COMP-3.        
00429          16  AC-CANCEL           PIC S9(9)         COMP-3.        
00429          16  AC-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  AC-ACOVERAGE        PIC S9(9)         COMP-3.        
00432          16  AC-LPRM             PIC S9(9)V99      COMP-3.        
00431          16  AC-LCAN             PIC S9(9)V99      COMP-3.        
00433          16  AC-LCLM             PIC S9(9)V99      COMP-3.        
00435          16  AC-APRM             PIC S9(9)V99      COMP-3.        
00434          16  AC-ACAN             PIC S9(9)V99      COMP-3.        
00436          16  AC-ACLM             PIC S9(9)V99      COMP-3.        
00437          16  AC-TPRM             PIC S9(9)V99      COMP-3.        
00438          16  AC-TCOM             PIC S9(9)V99      COMP-3.        
00439          16  AC-DATE             PIC 9(11) COMP-3 VALUE 0.        
00440          16  AC-DATE-R.                                           
00441              20  FILLER          PIC 999.                         
00442              20  AC-DATE-CC      PIC 99.                          
00443              20  AC-DATE-YR      PIC 99.                          
00444              20  AC-DATE-MO      PIC 99.                          
00445              20  AC-DATE-DA      PIC 99.                          
00446 *    12  GROUPING-ACCUM   OCCURS 13 TIMES.                        
122804     12  GROUPING-ACCUM   OCCURS 15 TIMES.                        
00447          16  GP-CERT             PIC S9(9)         COMP-3.        
00429          16  GP-CANCEL           PIC S9(9)         COMP-3.        
00429          16  GP-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  GP-ACOVERAGE        PIC S9(9)         COMP-3.        
00450          16  GP-LPRM             PIC S9(9)V99      COMP-3.        
00449          16  GP-LCAN             PIC S9(9)V99      COMP-3.        
00451          16  GP-LCLM             PIC S9(9)V99      COMP-3.        
00453          16  GP-APRM             PIC S9(9)V99      COMP-3.        
00452          16  GP-ACAN             PIC S9(9)V99      COMP-3.        
00454          16  GP-ACLM             PIC S9(9)V99      COMP-3.        
00455          16  GP-TPRM             PIC S9(9)V99      COMP-3.        
00456          16  GP-TCOM             PIC S9(9)V99      COMP-3.        
00457          16  GP-DATE             PIC 9(11) COMP-3 VALUE 0.        
00458          16  GP-DATE-R.                                           
00459              20  FILLER          PIC 999.                         
00460              20  GP-DATE-CC      PIC 99.                          
00461              20  GP-DATE-YR      PIC 99.                          
00462              20  GP-DATE-MO      PIC 99.                          
00463              20  GP-DATE-DA      PIC 99.                          
00464 *    12  CARRIER-ACCUM    OCCURS 13 TIMES.                        
122804     12  CARRIER-ACCUM    OCCURS 15 TIMES.                        
00465          16  CA-CERT             PIC S9(9)         COMP-3.        
00429          16  CA-CANCEL           PIC S9(9)         COMP-3.        
00429          16  CA-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  CA-ACOVERAGE        PIC S9(9)         COMP-3.        
00450          16  CA-LPRM             PIC S9(9)V99      COMP-3.        
00449          16  CA-LCAN             PIC S9(9)V99      COMP-3.        
00469          16  CA-LCLM             PIC S9(9)V99      COMP-3.        
00471          16  CA-APRM             PIC S9(9)V99      COMP-3.        
00470          16  CA-ACAN             PIC S9(9)V99      COMP-3.        
00472          16  CA-ACLM             PIC S9(9)V99      COMP-3.        
00473          16  CA-TPRM             PIC S9(9)V99      COMP-3.        
00474          16  CA-TCOM             PIC S9(9)V99      COMP-3.        
00475          16  CA-DATE             PIC 9(11) COMP-3 VALUE 0.        
00476          16  CA-DATE-R.                                           
00477              20  FILLER          PIC 999.                         
00478              20  CA-DATE-CC      PIC 99.                          
00479              20  CA-DATE-YR      PIC 99.                          
00480              20  CA-DATE-MO      PIC 99.                          
00481              20  CA-DATE-DA      PIC 99.                          
00482 *    12  STATE-ACCUM      OCCURS 13 TIMES.                        
122804     12  STATE-ACCUM      OCCURS 15 TIMES.                        
00483          16  ST-CERT             PIC S9(9)         COMP-3.        
00429          16  ST-CANCEL           PIC S9(9)         COMP-3.        
00429          16  ST-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  ST-ACOVERAGE        PIC S9(9)         COMP-3.        
00450          16  ST-LPRM             PIC S9(9)V99      COMP-3.        
00449          16  ST-LCAN             PIC S9(9)V99      COMP-3.        
00487          16  ST-LCLM             PIC S9(9)V99      COMP-3.        
00489          16  ST-APRM             PIC S9(9)V99      COMP-3.        
00488          16  ST-ACAN             PIC S9(9)V99      COMP-3.        
00490          16  ST-ACLM             PIC S9(9)V99      COMP-3.        
00491          16  ST-TPRM             PIC S9(9)V99      COMP-3.        
00492          16  ST-TCOM             PIC S9(9)V99      COMP-3.        
00493          16  ST-DATE             PIC X(6).                        
00494 *    12  REPTCD-ACCUM     OCCURS 13 TIMES.                        
00494      12  REPTCD-ACCUM     OCCURS 15 TIMES.                        
00495          16  RP-CERT             PIC S9(9)         COMP-3.        
00429          16  RP-CANCEL           PIC S9(9)         COMP-3.        
00429          16  RP-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  RP-ACOVERAGE        PIC S9(9)         COMP-3.        
00450          16  RP-LPRM             PIC S9(9)V99      COMP-3.        
00449          16  RP-LCAN             PIC S9(9)V99      COMP-3.        
00499          16  RP-LCLM             PIC S9(9)V99      COMP-3.        
00501          16  RP-APRM             PIC S9(9)V99      COMP-3.        
00500          16  RP-ACAN             PIC S9(9)V99      COMP-3.        
00502          16  RP-ACLM             PIC S9(9)V99      COMP-3.        
00503          16  RP-TPRM             PIC S9(9)V99      COMP-3.        
00504          16  RP-TCOM             PIC S9(9)V99      COMP-3.        
00505          16  RP-DATE             PIC 9(11) COMP-3 VALUE 0.        
00506          16  RP-DATE-R.                                           
00507              20  FILLER          PIC 999.                         
00508              20  RP-DATE-CC      PIC 99.                          
00509              20  RP-DATE-YR      PIC 99.                          
00510              20  RP-DATE-MO      PIC 99.                          
00511              20  RP-DATE-DA      PIC 99.                          
00512      12  GRAND-TOTALS.                                            
00513          16  GR-CERT             PIC S9(9)       COMP-3 VALUE +0. 
00429          16  GR-CANCEL           PIC S9(9)         COMP-3.        
00429          16  GR-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  GR-ACOVERAGE        PIC S9(9)         COMP-3.        
00450          16  GR-LPRM             PIC S9(9)V99      COMP-3.        
00449          16  GR-LCAN             PIC S9(9)V99      COMP-3.        
00517          16  GR-LCLM             PIC S9(9)V99    COMP-3 VALUE +0. 
00519          16  GR-APRM             PIC S9(9)V99    COMP-3 VALUE +0. 
00518          16  GR-ACAN             PIC S9(9)V99    COMP-3 VALUE +0. 
00520          16  GR-ACLM             PIC S9(9)V99    COMP-3 VALUE +0. 
00521          16  GR-TPRM             PIC S9(9)V99    COMP-3 VALUE +0. 
00522          16  GR-TCOM             PIC S9(9)V99    COMP-3 VALUE +0. 
00523 *    12  GRAND-ACCUM      OCCURS 13 TIMES.                        
122804     12  GRAND-ACCUM      OCCURS 15 TIMES.                        
00524          16  GT-CERT             PIC S9(9)         COMP-3.        
00429          16  GT-CANCEL           PIC S9(9)         COMP-3.        
00429          16  GT-LCOVERAGE        PIC S9(9)         COMP-3.        
00429          16  GT-ACOVERAGE        PIC S9(9)         COMP-3.        
00450          16  GT-LPRM             PIC S9(9)V99      COMP-3.        
00449          16  GT-LCAN             PIC S9(9)V99      COMP-3.        
00528          16  GT-LCLM             PIC S9(9)V99      COMP-3.        
00530          16  GT-APRM             PIC S9(9)V99      COMP-3.        
00518          16  GT-ACAN             PIC S9(9)V99    COMP-3 VALUE +0. 
00531          16  GT-ACLM             PIC S9(9)V99      COMP-3.        
00532          16  GT-TPRM             PIC S9(9)V99      COMP-3.        
00533          16  GT-TCOM             PIC S9(9)V99      COMP-3.        
00534          16  GT-DATE             PIC 9(11) COMP-3 VALUE 0.        
00535          16  GT-DATE-R.                                           
00536              20  FILLER          PIC 999.                         
00537              20  GT-DATE-CC      PIC 99.                          
00538              20  GT-DATE-YR      PIC 99.                          
00539              20  GT-DATE-MO      PIC 99.                          
00540              20  GT-DATE-DA      PIC 99.                          
00541      12  LAST-12-ACCOUNT-ACCUM.                                   
00524          16  L12AC-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12AC-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12AC-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12AC-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00542          16  L12AC-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00543          16  L12AC-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00544          16  L12AC-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00545          16  L12AC-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00546          16  L12AC-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00547          16  L12AC-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00548          16  L12AC-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00549          16  L12AC-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00550      12  YTD-ACCOUNT-ACCUM.                                       
00524          16  YTDAC-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDAC-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDAC-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDAC-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00551          16  YTDAC-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00552          16  YTDAC-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00553          16  YTDAC-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00554          16  YTDAC-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00555          16  YTDAC-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00556          16  YTDAC-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00557          16  YTDAC-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00558          16  YTDAC-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00559      12  LAST-12-GROUPING-ACCUM.                                  
00524          16  L12GP-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12GP-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12GP-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12GP-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00560          16  L12GP-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00561          16  L12GP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00562          16  L12GP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00563          16  L12GP-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00564          16  L12GP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00565          16  L12GP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00566          16  L12GP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00567          16  L12GP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00568      12  LAST-12-CARRIER-ACCUM.                                   
00524          16  L12CA-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12CA-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12CA-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12CA-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00569          16  L12CA-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00570          16  L12CA-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00571          16  L12CA-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00572          16  L12CA-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00573          16  L12CA-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00574          16  L12CA-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00575          16  L12CA-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00576          16  L12CA-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00577      12  LAST-12-STATE-ACCUM.                                     
00524          16  L12ST-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12ST-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12ST-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12ST-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00578          16  L12ST-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00579          16  L12ST-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00580          16  L12ST-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00581          16  L12ST-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00582          16  L12ST-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00583          16  L12ST-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00584          16  L12ST-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00585          16  L12ST-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00586      12  LAST-12-REPTCD-ACCUM.                                    
00524          16  L12RP-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12RP-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12RP-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12RP-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00587          16  L12RP-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00588          16  L12RP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00589          16  L12RP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00590          16  L12RP-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00591          16  L12RP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00592          16  L12RP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00593          16  L12RP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00594          16  L12RP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00586      12  LAST-12-GRAND-ACCUM.                                    
00524          16  L12GT-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12GT-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12GT-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  L12GT-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00587          16  L12GT-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00588          16  L12GT-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00589          16  L12GT-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00590          16  L12GT-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00591          16  L12GT-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00592          16  L12GT-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00593          16  L12GT-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00594          16  L12GT-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00595      12  YTD-GROUPING-ACCUM.                                      
00524          16  YTDGP-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDGP-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDGP-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDGP-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00596          16  YTDGP-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00597          16  YTDGP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00598          16  YTDGP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00599          16  YTDGP-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00600          16  YTDGP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00601          16  YTDGP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00602          16  YTDGP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00603          16  YTDGP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00604      12  YTD-CARRIER-ACCUM.                                       
00524          16  YTDCA-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDCA-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDCA-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDCA-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00605          16  YTDCA-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00606          16  YTDCA-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00607          16  YTDCA-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00608          16  YTDCA-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00609          16  YTDCA-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00610          16  YTDCA-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00611          16  YTDCA-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00612          16  YTDCA-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00613      12  YTD-STATE-ACCUM.                                         
00524          16  YTDST-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDST-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDST-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDST-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00614          16  YTDST-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00615          16  YTDST-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00616          16  YTDST-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00617          16  YTDST-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00618          16  YTDST-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00619          16  YTDST-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00620          16  YTDST-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00621          16  YTDST-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00622      12  YTD-REPTCD-ACCUM.                                        
00524          16  YTDRP-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDRP-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDRP-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDRP-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00623          16  YTDRP-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00624          16  YTDRP-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00625          16  YTDRP-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00626          16  YTDRP-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00627          16  YTDRP-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00628          16  YTDRP-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00629          16  YTDRP-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00630          16  YTDRP-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00622      12  YTD-GRAND-ACCUM.                                        
00524          16  YTDGT-CERT          PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDGT-CANCEL        PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDGT-LCOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00429          16  YTDGT-ACOVERAGE     PIC S9(9)       COMP-3 VALUE +0.        
00623          16  YTDGT-LCAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00624          16  YTDGT-LPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00625          16  YTDGT-LCLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00626          16  YTDGT-ACAN          PIC S9(9)V99    COMP-3 VALUE +0. 
00627          16  YTDGT-APRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00628          16  YTDGT-ACLM          PIC S9(9)V99    COMP-3 VALUE +0. 
00629          16  YTDGT-TPRM          PIC S9(9)V99    COMP-3 VALUE +0. 
00630          16  YTDGT-TCOM          PIC S9(9)V99    COMP-3 VALUE +0. 
00631      12  ZERO-OCCURS-ACCUM.                                       
00632          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00633          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00632          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00633          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00634          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00635          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00636          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00637          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00638          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00639          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00640          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00641          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00642          16  FILLER              PIC X(6)       VALUE LOW-VALUES. 
00643      12  ZERO-ACCUM.                                              
00632          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00633          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00632          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00633          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00644          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00645          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00646          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00647          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00648          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00649          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00650          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00651          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00652      12  ZERO-GRAND-TOTALS.                                       
00632          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00633          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00653          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00654          16  FILLER              PIC S9(9)      COMP-3  VALUE +0. 
00655          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00656          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00657          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00658          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00659          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00660          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00661          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 
00662          16  FILLER              PIC S9(9)V99   COMP-3  VALUE +0. 


122804 01  PYTD-ACCOUNT-ACCUM.
00524      12  PYTDAC-CERT             PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDAC-CANCEL           PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDAC-LCOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDAC-ACOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
122804     12  PYTDAC-LCAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-ACAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDAC-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-GROUPING-ACCUM.
00524      12  PYTDGP-CERT             PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDGP-CANCEL           PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDGP-LCOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDGP-ACOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
122804     12  PYTDGP-LCAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-ACAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGP-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-CARRIER-ACCUM.
00524      12  PYTDCA-CERT             PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDCA-CANCEL           PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDCA-LCOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDCA-ACOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
122804     12  PYTDCA-LCAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-ACAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDCA-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-STATE-ACCUM.
00524      12  PYTDST-CERT             PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDST-CANCEL           PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDST-LCOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDST-ACOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
122804     12  PYTDST-LCAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-ACAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDST-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-REPTCD-ACCUM.
00524      12  PYTDRP-CERT             PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDRP-CANCEL           PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDRP-LCOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDRP-ACOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
122804     12  PYTDRP-LCAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-ACAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDRP-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.

122804 01  PYTD-GRAND-ACCUM.
00524      12  PYTDGT-CERT             PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDGT-CANCEL           PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDGT-LCOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
00429      12  PYTDGT-ACOVERAGE        PIC S9(9)       COMP-3 VALUE +0.        
122804     12  PYTDGT-LCAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-LPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-LCLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-ACAN             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-APRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-ACLM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-TPRM             PIC S9(9)V99    COMP-3 VALUE +0.
122804     12  PYTDGT-TCOM             PIC S9(9)V99    COMP-3 VALUE +0.


00663  EJECT                                                            
00664  01  PRINT-DECISION-TABLE COMP-3.                                 
00665      05  PRINT-TABLE      OCCURS 12 TIMES.    
               10  PCERT               PIC S9(9).                    
               10  PCANCEL             PIC S9(9).                    
               10  PLCOVERAGE          PIC S9(9).                    
               10  PACOVERAGE          PIC S9(9).                    
00666          10  PLCAN               PIC S9(9)V99.                    
00667          10  PLPRM               PIC S9(9)V99.                    
00668          10  PLCLM               PIC S9(9)V99.                    
00669          10  PACAN               PIC S9(9)V99.                    
00670          10  PAPRM               PIC S9(9)V99.                    
00671          10  PACLM               PIC S9(9)V99.                    
00672          10  PTCOM               PIC S9(9)V99.                    
00673                                                                   
00674  01  PRINT-ZERO-TABLE  COMP-3.                                    
00675      05  FILLER                  PIC S9(9)       VALUE ZEROS.     
00675      05  FILLER                  PIC S9(9)       VALUE ZEROS.     
00675      05  FILLER                  PIC S9(9)       VALUE ZEROS.     
00675      05  FILLER                  PIC S9(9)       VALUE ZEROS.     
00675      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00676      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00677      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00678      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00679      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00680      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00681      05  FILLER                  PIC S9(9)V99    VALUE ZEROS.     
00682                                                                   
00683  01  WS.                                                          
00684      12  WS-RETURN-CODE          PIC S9(4)    COMP   VALUE ZERO.  
00685      12  WS-ZERO                 PIC S9              VALUE ZERO.  
00686      12  WS-ABEND-MESSAGE        PIC X(80)           VALUE SPACES.
00687      12  WS-ABEND-FILE-STATUS    PIC XX              VALUE ZERO.  
00688      12  WS-EP-ACC-EXPIRES       PIC 9(11).                       
00689      12  WS-EP-ACC-EXPIRES-R REDEFINES WS-EP-ACC-EXPIRES.         
00690          16  FILLER              PIC 999.                         
00691          16  EP-ACC-EXP-CC       PIC 99.                          
00692          16  EP-ACC-EXP-YR       PIC 99.                          
00693          16  EP-ACC-EXP-MO       PIC 99.                          
00694          16  EP-ACC-EXP-DA       PIC 99.                          
00695      12  WS-EP-ACC-HI-CERT       PIC 9(11).                       
00696      12  WS-EP-ACC-HI-CERT-R REDEFINES WS-EP-ACC-HI-CERT.         
00697          16  FILLER              PIC 999.                         
00698          16  EP-ACC-HI-CC        PIC 99.                          
00699          16  EP-ACC-HI-YR        PIC 99.                          
00700          16  EP-ACC-HI-MO        PIC 99.                          
00701          16  EP-ACC-HI-DA        PIC 99.                          
00702      12  WS-EP-MTH-HI-CERT       PIC 9(11).                       
00703      12  WS-EP-MTH-HI-CERT-R REDEFINES WS-EP-MTH-HI-CERT.         
00704          16  FILLER              PIC 999.                         
00705          16  EP-MTH-HI-CC        PIC 99.                          
00706          16  EP-MTH-HI-YR        PIC 99.                          
00707          16  EP-MTH-HI-MO        PIC 99.                          
00708          16  EP-MTH-HI-DA        PIC 99.                          
00709      12  WS-EP-DTE               PIC 9(07).                       
00710      12  WS-EP-DTE-R REDEFINES WS-EP-DTE.                         
00711          16  FILLER              PIC 9.                           
00712          16  EP-RUN-CC           PIC 99.                          
00713          16  EP-RUN-YR           PIC 99.                          
00714          16  EP-RUN-MO           PIC 99.                          
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
00715  EJECT                                                            
00716  01  CONVERSION-DATE             PIC 9(08) VALUE ZEROS.           
00717  01  CONVERSION-DATE-R REDEFINES CONVERSION-DATE.                 
00718      05  CONV-CC                 PIC XX.                          
00719      05  CONV-YR                 PIC XX.                          
00720      05  CONV-MO                 PIC XX.                          
00721      05  CONV-DA                 PIC XX.                          
00722                                                                   
00723  01  COMPARE-DATE-TABLE.                                          
00724 *    05  COMPARE-DTS    OCCURS 13 TIMES.                          
122804     05  COMPARE-DTS    OCCURS 15 TIMES.                          
00725          10  COMPARE-DT.                                          
00726              15  COMP-CCYY       PIC 9(4).                        
00727              15  COMP-CCYR REDEFINES COMP-CCYY.                   
00728                  20  COMP-CC     PIC 99.                          
00729                  20  COMP-YR     PIC 99.                          
00730              15  COMP-MO         PIC 99.                          
00731                                                                   
00732  01  COMPARE-DATE9TABLE REDEFINES COMPARE-DATE-TABLE.             
00733 *    05  COMPARE9DTS    OCCURS 13 TIMES.                          
00733      05  COMPARE9DTS    OCCURS 15 TIMES.                          
00734          10  COMPARE9DT          PIC 9(06).                       
00735                                                                   
122804 01  YEAR-END-DT                 PIC 9(06)  VALUE ZEROS.
122804 01  TWO-YEAR-ENDS-AGO           PIC 9(06)  VALUE ZEROS.
122804 01  TWO-YEARS-AGO               PIC 9(06)  VALUE ZEROS.
00736  01  YEAR-OLD-DATE               PIC 9(06).                       
00737                                                                   
00738  01  FISCAL-DATE         REDEFINES YEAR-OLD-DATE.                 
00739      05  FISCAL-CCYY             PIC 9(04).                       
00740      05  FISCAL-CCYR REDEFINES FISCAL-CCYY.                       
00741          10  FISCAL-CC           PIC 99.                          
00742          10  FISCAL-YR           PIC 99.                          
00743      05  FISCAL-MO               PIC 99.                          
00744                                                                   
00745  01  SAVE-EPX.                                                    
00746      12  SAVE-CNTL.                                               
00755          16  S-EPX-A-RPT-CD-1        PIC X(10).                   
00756          16  S-EPX-A-CARR            PIC X.                       
00757          16  S-EPX-A-GROUP           PIC X(6).                    
00759          16  S-EPX-A-STATE           PIC XX.                      
00760          16  S-EPX-A-ACCT            PIC X(10).                   
00761                                                                   
102004 01  SAVE-ACCT-STATUS            PIC X(12).
00762  01  SAVE-ACCT-NAME              PIC X(30).                       
CIDMOD 01  SAVE-ACCT-CITY              PIC X(30).                       
00763                                                                   
00764  01  SAVE-DATES.                                                  
00765      05  SAVE-AM-EXPIRES         PIC 9(11).                       
00766      05  SAVE-AM-EXPIRES-R REDEFINES SAVE-AM-EXPIRES.             
00767          12  FILLER              PIC 999.                         
00768          12  SAVE-AM-EXP-CC      PIC 99.                          
00769          12  SAVE-AM-EXP-YR      PIC 99.                          
00770          12  SAVE-AM-EXP-MO      PIC 99.                          
00771          12  SAVE-AM-EXP-DA      PIC 99.                          
00772      05  SAVE-AM-HI-CERT         PIC 9(11).                       
00773      05  SAVE-AM-HI-CERT-R REDEFINES SAVE-AM-HI-CERT.             
00774          12  FILLER              PIC 999.                         
00775          12  SAVE-AM-HI-CC       PIC 99.                          
00776          12  SAVE-AM-HI-YR       PIC 99.                          
00777          12  SAVE-AM-HI-MO       PIC 99.                          
00778          12  SAVE-AM-HI-DA       PIC 99.                          
00786                                                                   
00787  01  EP-DATE.                                                     
00788      05  E-CC                    PIC 99      VALUE ZEROS.         
00789      05  E-YR                    PIC 99      VALUE ZEROS.         
00790      05  E-MO                    PIC 99      VALUE ZEROS.         
00791                                                                   
00792  01  EP9DATE REDEFINES EP-DATE   PIC 9(6).                        
00793  EJECT                                                            
00794  01  WS-BIN-DATES.                                                
00795      12  BIN-AC-DATE             PIC XX      VALUE SPACES.        
00796      12  BIN-EP-HI-CERT          PIC XX      VALUE SPACES.        
00797      12  BIN-MTH-HI-CERT         PIC XX      VALUE SPACES.        
00798      12  BIN-GP-DATE             PIC XX      VALUE SPACES.        
00799      12  BIN-RP-DATE             PIC XX      VALUE SPACES.        
00800      12  BIN-CA-DATE             PIC XX      VALUE SPACES.        
00801      12  BIN-GT-DATE             PIC XX      VALUE SPACES.        
00802                                                                   
00803  COPY ELCDATE.                                                    
00804                                                                   
00805                                  COPY ELCDTECX.                   
00806                                                                   
00807                                  COPY ELCDTEVR.                   
122304 LINKAGE SECTION.                                                 
122304                                                                  
122304 01  PARM.                                                        
122304     05  PARM-LENGTH BINARY  PICTURE IS S9(4).                    
122304     05  PARM-VALUE  DISPLAY PICTURE IS X(100).                   
122304                                                                  
00808  EJECT                                                            
00809  PROCEDURE DIVISION USING PARM.
00810                                                                   
00811  0000-START-HERE.                                                 
00812                     COPY ELCDTERX.                                
00813                                                                   
00832                                                                   
00833      MOVE DTE-CONV-DT            TO CONVERSION-DATE.              
00834      MOVE WS-CURRENT-DATE        TO HD-IPL-DATE.                  
00835      MOVE COMPANY-NAME           TO HD-COMPANY-NAME.              
00836      MOVE ALPH-DATE              TO HD-ALPHA-DATE.                
00837                                                                   
00838      IF DTE-QTR-CO = '1'                                          
00839          MOVE '1'                TO QTR-COMP.                     
122304     IF PARM-LENGTH > +0
              IF PARM-VALUE = 'SPECIAL'
                 MOVE 'SINGLE FEE EQUIVALENT'  TO
                      HD-1-RC1-SINGLE-FEE
              END-IF
           END-IF
              
00840      .                                                            
00841  0100-SORT-ROUTINE SECTION.                                       
00842                                                                   
00843      SORT SORT-FILE ASCENDING KEY SORT-CONTROL-74
CIDMOD                       SORT-HI-CERT
00844          USING EARNED-PREM        
00845          OUTPUT PROCEDURE 0200-PRINT-ROUTINE THRU 9099-EXIT.      
00846                                                                   
00847      IF SORT-RETURN NOT = ZEROS                                   
00848          MOVE '0101'             TO WS-RETURN-CODE                
00849          GO TO ABEND-PGM.                                         
00850                                                                   
00851      GOBACK.                                                      
00852  EJECT                                                            
00853  0200-PRINT-ROUTINE SECTION.                                      
00854                                                                   
00855      OPEN OUTPUT PRNT.                                            
00856                                                                   
00857  0210-CLEAR-CNTRS.                                                
00858      MOVE ZERO                   TO X1.                           
00859      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
00860      MOVE ZERO                   TO X1.                           
00861      PERFORM 0230-ZERO-ACCUM-ACC 15 TIMES.                        
00862      MOVE ZERO                   TO X1.                           
00863      PERFORM 0240-ZERO-ACCUM-GRP 15 TIMES.                        
00864      MOVE ZERO                   TO X1.                           
00865      PERFORM 0250-ZERO-ACCUM-CARR 15 TIMES.                       
00866      MOVE ZERO                   TO X1.                           
00867      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
00868      MOVE ZERO                   TO X1.                           
00869      PERFORM 0260-ZERO-ACCUM-REPT 15 TIMES.                       
00870      MOVE ZERO                   TO X1.                           
00871      PERFORM 0270-ZERO-ACCUM-GRAND 15 TIMES.                      
00872      MOVE ZERO-GRAND-TOTALS      TO GRAND-TOTALS.                 
00873      MOVE ZEROS                  TO EP-CNTL.                      
00874      GO TO 0280-FIND-DATE-RANGE.                                  
00875                                                                   
00876  0220-ZERO-PRINT-TABLE.                                           
00877      ADD +1 TO X1.                                                
00878      MOVE PRINT-ZERO-TABLE       TO PRINT-TABLE (X1).             
00879                                                                   
00880  0230-ZERO-ACCUM-ACC.                                             
00881      ADD +1 TO X1.                                                
00882      MOVE ZERO-OCCURS-ACCUM      TO ACCOUNT-ACCUM (X1).           
00883                                                                   
00884  0240-ZERO-ACCUM-GRP.                                             
00885      ADD +1 TO X1.                                                
00886      MOVE ZERO-OCCURS-ACCUM      TO GROUPING-ACCUM (X1).          
00887                                                                   
00888  0250-ZERO-ACCUM-CARR.                                            
00889      ADD +1 TO X1.                                                
00890      MOVE ZERO-OCCURS-ACCUM      TO CARRIER-ACCUM (X1).           
00891                                                                   
00892  0255-ZERO-ACCUM-STATE.                                           
00893      ADD +1 TO X1.                                                
00894      MOVE ZERO-OCCURS-ACCUM      TO STATE-ACCUM (X1).             
00895                                                                   
00896  0260-ZERO-ACCUM-REPT.                                            
00897      ADD +1 TO X1.                                                
00898      MOVE ZERO-OCCURS-ACCUM      TO REPTCD-ACCUM (X1).            
00899                                                                   
00900  0270-ZERO-ACCUM-GRAND.                                           
00901      ADD +1 TO X1.                                                
00902      MOVE ZERO-OCCURS-ACCUM      TO GRAND-ACCUM (X1).             
00903                                                                   
00904  0280-FIND-DATE-RANGE.                                            
00905      IF QTR-CO                                                    
00906          IF RUN-MO = 03 OR 06 OR 09 OR 12                         
00907              MOVE '1'            TO QTR-SW.                       
00908                                                                   
00909      MOVE RUN-MO                 TO RUN-DT-MO.                    
00910      MOVE RUN-CCYY               TO RUN-DT-CCYY.                  
00911      MOVE +1                     TO X1.                           
00912      MOVE +0                     TO Y1.                           
00913                                                                   
00914      IF QTR-CO AND QTR-END                                        
00915          MOVE 'QUARTERLY'        TO HD-1-RC   
00918          COMPUTE COMPARE9DT (X1) = RUN9DT - 300                   
00919      ELSE                                                         
00920          COMPUTE COMPARE9DT (X1) = RUN9DT - 100.                  
00921                                                                   
00922      COMPUTE YEAR-OLD-DATE = RUN9DT - 100.                        
122804     COMPUTE TWO-YEARS-AGO = RUN9DT - 200.
00930                                                                   
00931  0290-BUILD-DATE-TABLE.                                           

122804     IF (COMP-MO (X1) = 12)
122804        AND (TWO-YEAR-ENDS-AGO = ZEROS)
122804        COMPUTE TWO-YEAR-ENDS-AGO = COMPARE9DT (X1) - 100
122804        MOVE COMPARE9DT (X1)     TO YEAR-END-DT
122804     END-IF

00932      ADD +1 TO X1 Y1.                                             
00933                                                                   
00934      IF X1 GREATER +13
122804        MOVE TWO-YEARS-AGO       TO COMPARE9DT (14)
122804        MOVE TWO-YEAR-ENDS-AGO   TO COMPARE9DT (15)
              PERFORM VARYING X1 FROM +1 BY +1 UNTIL
                 X1 > 15
                 MOVE X1               TO DISP-X1
                 DISPLAY ' COMPARE DATE ' DISP-X1 ' ' COMPARE9DT (X1)
              END-PERFORM
              DISPLAY ' YEAR END DATE ' YEAR-END-DT
00935         GO TO 0310-READ-EARNED-PREM-EXTRACT
           END-IF
00936                                                                   
00937      IF QTR-CO AND QTR-END                                        
00938          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0003         
00939      ELSE                                                         
00940          COMPUTE COMPARE9DT (X1) = COMPARE9DT (Y1) + 0001.        
00941                                                                   
00942      IF COMP-MO (X1) GREATER 12                                   
00943          COMPUTE COMPARE9DT (X1) = COMPARE9DT (X1) + 0088.        
00944                                                                   
00945      GO TO 0290-BUILD-DATE-TABLE.                                 
00946  EJECT                                                            
00947  0310-READ-EARNED-PREM-EXTRACT.                                   
00948      RETURN SORT-FILE INTO WS-EXTR-REC                            
00949                           AT END MOVE '1' TO EOF-EPX              
00950                                  GO TO 1500-PASS-NUMBER-CHANGE.   

101920     IF EP-PASS-NO NOT = '4' AND '5' AND '6' and '8'
00966         GO TO 0310-READ-EARNED-PREM-EXTRACT
101920     end-if
00967                                                                   
00971                                                                   
00972      IF FIRST-TIME                                                
00973          MOVE EP-PASS-NO         TO PASS-NUMBER                   
00974          MOVE 'N'                TO FIRST-TIME-SWITCH             
00975          GO TO 0340-ACCUMULATE.                                   
00976                                                                   
00977  0320-CHECK-EXTRACT-SEQ.                                          
00978      IF EP-PASS-NO NOT = PASS-NUMBER                              
00979          GO TO 1500-PASS-NUMBER-CHANGE.                           
00980                                                                   
00981      IF EP-CNTL LESS THAN SAVE-EPX                                
00982          DISPLAY 'EARNED PREMIUM EXTRACT OUT OF SEQUENCE'         
00983          DISPLAY 'SAVE = ' SAVE-EPX                               
00984          DISPLAY 'READ = ' EP-CNTL                                
00985          MOVE '0610'             TO WS-RETURN-CODE                
00986          GO TO ABEND-PGM.                                         
00987                                                                   
00988  0330-COMPARE-RECORDS.                                            

01008      IF EP-A-BREAK-CD NOT = S-EPX-A-RPT-CD-1
01009          PERFORM 0700-REPTCD-BREAK                    
01010                          THRU 0790-ZERO-REPTCD
               GO TO 0340-ACCUMULATE.                   
01020                                                                   
01030      IF S-EPX-A-CARR NOT = EP-A-CARR                              
01032          PERFORM 0600-CARRIER-BREAK THRU 0680-ZERO-CARRIER        
01033          GO TO 0340-ACCUMULATE.                                   

01036      IF S-EPX-A-GROUP NOT = EP-A-GROUP                            
01038          PERFORM 0500-GROUPING-BREAK THRU 0580-ZERO-GROUPING      
01039          GO TO 0340-ACCUMULATE.                                   

01036      IF S-EPX-A-STATE NOT = EP-A-STATE                            
01038          PERFORM 0375-STATE-BREAK THRU 0380-ZERO-STATE      
01039          GO TO 0340-ACCUMULATE.                                   

01036      IF S-EPX-A-ACCT NOT = EP-A-ACCT
01038          PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT      
01039          GO TO 0340-ACCUMULATE.                                   

01068                                                                   
01069  0340-ACCUMULATE.                                                 
01070      MOVE +0                     TO X1.                           
01071                                                                   
01072  0350-ACCUMULATE-B.                                               
01073      ADD +1 TO X1.                                                
01074                                                                   
122804     IF X1 GREATER 15
010306         GO TO 0310-READ-EARNED-PREM-EXTRACT
           END-IF
01082                                                                   
01083      IF COMPARE9DT (X1) NOT = EP-DTE                              
01084          GO TO 0350-ACCUMULATE-B.                                 
01085                                                                   
01086      IF EP-MTH-HI-CERT  GREATER  AC-DATE (X1)                     
01087          MOVE EP-MTH-HI-CERT     TO  AC-DATE (X1)                 
01088                                      AC-DATE-R(X1).               
01089                                                                   
01090      ADD EP-CERT               TO  AC-CERT (X1)  RP-CERT (X1)   
01091                                    ST-CERT (X1).                
01090      ADD EP-CANCEL             TO  AC-CANCEL (X1) RP-CANCEL (X1)   
01091                                    ST-CANCEL (X1).                
01092      ADD EP-LCOVERAGE          TO  AC-LCOVERAGE (X1)  
                                         RP-LCOVERAGE (X1)  
                                         ST-LCOVERAGE (X1).                
01092      ADD EP-ACOVERAGE          TO  AC-ACOVERAGE (X1)  
                                         RP-ACOVERAGE (X1)  
                                         ST-ACOVERAGE (X1).                
01096      ADD EP-LPRM               TO  AC-LPRM (X1)  RP-LPRM (X1)   
01097                                    ST-LPRM (X1).                
01094      ADD EP-LCAN               TO  AC-LCAN (X1)  RP-LCAN (X1)   
01095                                    ST-LCAN (X1).                
01098      ADD EP-LCLM               TO  AC-LCLM (X1)  RP-LCLM (X1)   
01099                                    ST-LCLM (X1).                
01102      ADD EP-APRM               TO  AC-APRM (X1)  RP-APRM (X1)   
01103                                    ST-APRM (X1).                
01100      ADD EP-ACAN               TO  AC-ACAN (X1)  RP-ACAN (X1)   
01101                                    ST-ACAN (X1).                
01104      ADD EP-ACLM               TO  AC-ACLM (X1)  RP-ACLM (X1)   
01105                                    ST-ACLM (X1).                
01106      ADD EP-TPRM               TO  AC-TPRM (X1)  RP-TPRM (X1)   
01107                                    ST-TPRM (X1).                
01108      ADD EP-TCOM               TO  AC-TCOM (X1)  RP-TCOM (X1)   
01109                                    ST-TCOM (X1).                
01110      MOVE EP-CNTL                TO SAVE-EPX.                     
102004     EVALUATE EP-ACCT-STATUS
102004        WHEN 'A'
                 MOVE 'ACTIVE'         TO SAVE-ACCT-STATUS
              WHEN 'I'
                 MOVE 'INACTIVE'       TO SAVE-ACCT-STATUS
              WHEN 'T'
                 MOVE 'TRANSFERRED'    TO SAVE-ACCT-STATUS
              WHEN 'C'
                 MOVE 'CANCELLED'      TO SAVE-ACCT-STATUS
              WHEN OTHER
                 MOVE 'UNKNOWN'        TO SAVE-ACCT-STATUS
           END-EVALUATE
           
01111      MOVE EP-ACCT-NAME           TO SAVE-ACCT-NAME.               
CIDMOD     MOVE EP-ACCT-CITY           TO SAVE-ACCT-CITY.               
01112      MOVE EP-ACC-EXPIRES         TO SAVE-AM-EXPIRES.              
01113      MOVE EP-ACC-HI-CERT         TO SAVE-AM-HI-CERT.              
01114                                                                   
010306     IF X1 < +15
010306        GO TO 0350-ACCUMULATE-B
010306     END-IF

01115      GO TO 0310-READ-EARNED-PREM-EXTRACT

           .
01117  0375-STATE-BREAK.                                                
           PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.
           
       0375-STATE-BREAK-1.
01121                                                                   
01122      MOVE 'TOTALS'               TO STATE-TOTAL-LINE.             
01123                                                                   
01124      IF P-ST-SW = '1'                                             
01125          MOVE ' '                TO P-ST-SW                       
01126      ELSE                                                         
01131          GO TO 0380-ZERO-STATE.                               
01132                                                                   
01133      PERFORM 0950-ST-HD          THRU 0950-EXIT.                       
01134      MOVE +1                     TO X1.                           
01135      MOVE +0                     TO Y1.                           
01136                                                                   
01137  0375-STATE-BREAK-PRINT.                                          
01138      ADD +1 TO X1 Y1.                                             
01139                                                                   
01140      IF X1 GREATER THAN 13                                        
01141          GO TO 0375-PRINT-LAST-ST-12.                             
01142                                                                   
01143      COMPUTE CERT = ST-CERT (X1) - ST-CERT (Y1).                  
01144      COMPUTE CANCELS  = ST-CANCEL (X1) - ST-CANCEL (Y1).
           COMPUTE LCOVERAGE = ST-LCOVERAGE (X1) - 
                                ST-LCOVERAGE (Y1)                  
           COMPUTE ACOVERAGE = ST-ACOVERAGE (X1) - 
                                ST-ACOVERAGE (Y1)                  
01146      COMPUTE LPRM = ST-LPRM (X1) - ST-LPRM (Y1).                  
01145      COMPUTE LCAN = ST-LCAN (X1) - ST-LCAN (Y1).                  
01147      COMPUTE LCLM = ST-LCLM (X1) - ST-LCLM (Y1).                  
01149      COMPUTE APRM = ST-APRM (X1) - ST-APRM (Y1).                  
01148      COMPUTE ACAN = ST-ACAN (X1) - ST-ACAN (Y1).                  
01150      COMPUTE ACLM = ST-ACLM (X1) - ST-ACLM (Y1).                  
01151      COMPUTE TCOM = ST-TCOM (X1) - ST-TCOM (Y1).                  
01152 *     COMPUTE TPRM = LPRM + APRM.                                  
01152      COMPUTE TPRM = ST-TPRM (X1) - ST-TPRM (Y1).                                  
01153                                                                   
01154      MOVE COMP-YR (X1)           TO DET-TMS-YR.                       
01155      MOVE COMP-MO (X1)           TO DET-TMS-MO.                       
01156                                                                   
01157      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01158                                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
01159      MOVE CERT                   TO DET-TMS-CERTS. 
           MOVE CANCELS                TO DET-TMS-CANCELS.
           MOVE LCOVERAGE              TO DET-TMS-LCOVERAGE.
           MOVE ACOVERAGE              TO DET-TMS-ACOVERAGE.                   
01160      MOVE LCAN                   TO DET-TMS-LCAN.                     
01161      MOVE LPRM                   TO DET-TMS-LPRM.                     
01162      MOVE LCLM                   TO DET-TMS-LCLM.                     
01164      MOVE APRM                   TO DET-TMS-APRM.                     
01163      MOVE ACAN                   TO DET-TMS-ACAN.                     
01165      MOVE ACLM                   TO DET-TMS-ACLM.                     
01166      MOVE TPRM                   TO DET-TMS-TPRM.                     
01167      MOVE TCOM                   TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (TPRM - TCOM).    
01168                                                                   
01169      IF COMP-YR (X1) = CONV-YR AND                                
01170         COMP-MO (X1) = CONV-MO                                    
01171          MOVE ZEROS TO DET-TMS-CERTS DET-TMS-CANCELS
                             DET-TMS-LCOVERAGE DET-TMS-ACOVERAGE
                             DET-TMS-LCAN DET-TMS-LPRM                
01172                        DET-TMS-ACAN  DET-TMS-APRM DET-TMS-LCLM                
01173                        DET-TMS-ACLM  DET-TMS-TPRM DET-TMS-TCOM                
01174      ELSE                                                         
01175          PERFORM 0385-ADD-TO-OTHERS THRU 0385-EXIT.               
01176                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01185      GO TO 0375-STATE-BREAK-PRINT.                                
01186                                                                   
01187  0375-PRINT-LAST-ST-12.                                           
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
01159      MOVE L12ST-CERT             TO DET-TMS-CERTS. 
           MOVE L12ST-CANCEL           TO DET-TMS-CANCELS.
           MOVE L12ST-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE L12ST-ACOVERAGE        TO DET-TMS-ACOVERAGE.                   
01189      MOVE L12ST-LPRM             TO DET-TMS-LPRM.                     
01188      MOVE L12ST-LCAN             TO DET-TMS-LCAN.                     
01190      MOVE L12ST-LCLM             TO DET-TMS-LCLM.                     
01192      MOVE L12ST-APRM             TO DET-TMS-APRM.                     
01193      MOVE L12ST-ACAN             TO DET-TMS-ACAN.                     
01193      MOVE L12ST-ACLM             TO DET-TMS-ACLM.                     
01194      MOVE L12ST-TPRM             TO DET-TMS-TPRM.                     
01195      MOVE L12ST-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (L12ST-TPRM - L12ST-TCOM).    
01196      MOVE '*12 MO'               TO DET-TMS-DATE.                    
01197                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01201                                                                   
      *  PRINT PRIOR L12  
           .
01202  0375-PRINT-ST-YTD.                                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
01159      MOVE YTDST-CERT             TO DET-TMS-CERTS. 
           MOVE YTDST-CANCEL           TO DET-TMS-CANCELS.
           MOVE YTDST-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE YTDST-ACOVERAGE        TO DET-TMS-ACOVERAGE.                   
01203      MOVE YTDST-LCAN             TO DET-TMS-LCAN.                     
01204      MOVE YTDST-LPRM             TO DET-TMS-LPRM.                     
01205      MOVE YTDST-LCLM             TO DET-TMS-LCLM.                     
01206      MOVE YTDST-ACAN             TO DET-TMS-ACAN.                     
01207      MOVE YTDST-APRM             TO DET-TMS-APRM.                     
01208      MOVE YTDST-ACLM             TO DET-TMS-ACLM.                     
01209      MOVE YTDST-TPRM             TO DET-TMS-TPRM.                     
01210      MOVE YTDST-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (YTDST-TPRM - YTDST-TCOM).    
01211      MOVE '*YTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01215      MOVE SPACES                 TO DET-TMS-TITLE.                    
01216                                                                   
      *  PRINT PRIOR YTD  

03448      MOVE LIFE-OVERRIDE-L2        TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2          TO DET-TMS-ADESC.                
01159      MOVE PYTDST-CERT             TO DET-TMS-CERTS. 
           MOVE PYTDST-CANCEL           TO DET-TMS-CANCELS.
           MOVE PYTDST-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE PYTDST-ACOVERAGE        TO DET-TMS-ACOVERAGE.                   
122804     MOVE PYTDST-LCAN             TO DET-TMS-LCAN
122804     MOVE PYTDST-LPRM             TO DET-TMS-LPRM
122804     MOVE PYTDST-LCLM             TO DET-TMS-LCLM
122804     MOVE PYTDST-ACAN             TO DET-TMS-ACAN
122804     MOVE PYTDST-APRM             TO DET-TMS-APRM
122804     MOVE PYTDST-ACLM             TO DET-TMS-ACLM
122804     MOVE PYTDST-TPRM             TO DET-TMS-TPRM
122804     MOVE PYTDST-TCOM             TO DET-TMS-TCOM
03462      COMPUTE DET-TMS-NPRM = (PYTDST-TPRM - PYTDST-TCOM).    
122804     MOVE '*PYTD'                 TO DET-TMS-DATE
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
122804     MOVE SPACES                 TO DET-TMS-TITLE.
01216                                                                   
01217      MOVE ZERO-ACCUM             TO LAST-12-STATE-ACCUM
01218                                     YTD-STATE-ACCUM
122804                                    PYTD-STATE-ACCUM.
01220  0380-ZERO-STATE.                                                 
01221      MOVE +0                     TO X1.                           
01222      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
01223                                                                   
01224  0380-RETURN.                                                     
01225      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01226                                                                   
01227  0385-ADD-TO-OTHERS.                                              
01228                                                                   
01246      IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE  
               ADD CERT TO L12ST-CERT
               ADD CANCELS TO L12ST-CANCEL
               ADD LCOVERAGE TO L12ST-LCOVERAGE
               ADD ACOVERAGE TO L12ST-ACOVERAGE                   
01247          ADD LCAN TO L12ST-LCAN                                   
01248          ADD LPRM TO L12ST-LPRM                                   
01249          ADD LCLM TO L12ST-LCLM                                   
01250          ADD ACAN TO L12ST-ACAN                                   
01251          ADD APRM TO L12ST-APRM                                   
01252          ADD ACLM TO L12ST-ACLM                                   
01253          ADD TPRM TO L12ST-TPRM                                   
01254          ADD TCOM TO L12ST-TCOM.                                  
01255                                                                   
01256      IF RUN-YR = COMP-YR (X1)                                     
               ADD CERT TO YTDST-CERT
               ADD CANCELS TO YTDST-CANCEL
               ADD LCOVERAGE TO YTDST-LCOVERAGE
               ADD ACOVERAGE TO YTDST-ACOVERAGE                   
01257          ADD LCAN TO YTDST-LCAN                                   
01258          ADD LPRM TO YTDST-LPRM                                   
01259 *                     YTDST-TPRM                                   
01260          ADD LCLM TO YTDST-LCLM 
               ADD TPRM TO YTDST-TPRM                                  
01261          ADD TCOM TO YTDST-TCOM                                   
01262          ADD ACAN TO YTDST-ACAN                                   
01263          ADD APRM TO YTDST-APRM                                   
01264 *                     YTDST-TPRM                                   
01265          ADD ACLM TO YTDST-ACLM.                                  
01266            

122804     IF X1 = 13
              COMPUTE PYTDST-CERT = PYTDST-CERT +
                  (ST-CERT (1) - ST-CERT (15))
              COMPUTE PYTDST-CANCEL = PYTDST-CANCEL +
                  (ST-CANCEL (1) - ST-CANCEL (15))
              COMPUTE PYTDST-LCOVERAGE = PYTDST-LCOVERAGE +
                  (ST-LCOVERAGE (1) - ST-LCOVERAGE (15))
              COMPUTE PYTDST-ACOVERAGE = PYTDST-ACOVERAGE +
                  (ST-ACOVERAGE (1) - ST-ACOVERAGE (15))
              COMPUTE PYTDST-LCAN = PYTDST-LCAN +
                 (ST-LCAN (1) - ST-LCAN (15))
              COMPUTE PYTDST-LPRM = PYTDST-LPRM +
                 (ST-LPRM (1) - ST-LPRM (15))
              COMPUTE PYTDST-LCLM = PYTDST-LCLM +
                 (ST-LCLM (1) - ST-LCLM (15))
              COMPUTE PYTDST-ACAN = PYTDST-ACAN +
                 (ST-ACAN (1) - ST-ACAN (15))
              COMPUTE PYTDST-APRM = PYTDST-APRM +
051711           (ST-APRM (1) - ST-APRM (15))
              COMPUTE PYTDST-ACLM = PYTDST-ACLM +
                 (ST-ACLM (1) - ST-ACLM (15))
              COMPUTE PYTDST-TPRM = PYTDST-TPRM +
                 (ST-TPRM (1) - ST-TPRM (15))
              COMPUTE PYTDST-TCOM = PYTDST-TCOM +
                 (ST-TCOM (1) - ST-TCOM (15))
           END-IF

           .
01267  0385-EXIT.                                                       
01268       EXIT.                                                       
01269  EJECT                                                            
01270  0400-ACCOUNT-BREAK.                                              
01273                                                                   
01274      MOVE +0                     TO X1.                           
01275      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
01276      PERFORM 0800-PRINT-DECISION THRU 0829-EXIT.                  
01277                                                                   
01278      IF P-ACC-SW = '1'                                            
01279          MOVE ' '                TO  P-ACC-SW                     
01280      ELSE                                                         
01281          GO TO 0495-ZERO-ACCOUNT.                                 
01282                                                                   
01290      PERFORM 0900-ACC-HD         THRU 0940-EXIT.                       
01291                                                                   
01295      MOVE +1                     TO X1.                           
01296      MOVE +0                     TO Y1.                           
01297                                                                   
01298  0410-ACCOUNT-BREAK-PRINT.                                        
01299      ADD +1                      TO X1 Y1.                        
01300                                                                   
01301      IF X1 GREATER THAN 13                                        
01302          GO TO 0460-PRINT-LAST-AC-12.                             
01303                                                                   
01306      COMPUTE CERT = AC-CERT (X1) - AC-CERT (Y1).                  
01307      COMPUTE CANCELS  = AC-CANCEL (X1) - AC-CANCEL (Y1). 
           COMPUTE LCOVERAGE = AC-LCOVERAGE (X1) - AC-LCOVERAGE (Y1).
           COMPUTE ACOVERAGE = AC-ACOVERAGE (X1) - AC-ACOVERAGE (Y1).
01308      COMPUTE LCAN = AC-LCAN (X1) - AC-LCAN (Y1).                  
01309      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  
01310      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  
01311      COMPUTE ACAN = AC-ACAN (X1) - AC-ACAN (Y1).                  
01312      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  
01313      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  
01314      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  
01315 *     COMPUTE TPRM = (LPRM + APRM) - (LCAN + ACAN). 
           COMPUTE TPRM = AC-TPRM (X1) - AC-TPRM (Y1).
122804     MOVE SPACES                 TO DET-TMS-TITLE.
01316                                                                   
01321      MOVE COMP-YR (X1)       TO  DET-TMS-YR.
01322      MOVE COMP-MO (X1)       TO  DET-TMS-MO.                      
01323                                                                   
01324      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01325                                                                   
01371  0420-CONTINUE.                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
01372      MOVE CERT                   TO DET-TMS-CERTS.                    
           MOVE CANCELS                TO DET-TMS-CANCELS.
           MOVE LCOVERAGE              TO DET-TMS-LCOVERAGE.
           MOVE ACOVERAGE              TO DET-TMS-ACOVERAGE.
01373      MOVE LCAN                   TO DET-TMS-LCAN.                     
01374      MOVE LPRM                   TO DET-TMS-LPRM.                     
01375      MOVE LCLM                   TO DET-TMS-LCLM.                     
01376      MOVE ACAN                   TO DET-TMS-ACAN.                     
01377      MOVE APRM                   TO DET-TMS-APRM.                     
01378      MOVE ACLM                   TO DET-TMS-ACLM.                     
01379      MOVE TPRM                   TO DET-TMS-TPRM.                     
01380      MOVE TCOM                   TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (TPRM - TCOM).    
01381                                                                   
01382      IF COMP-YR (X1) = CONV-YR AND                                
01383         COMP-MO (X1) = CONV-MO                                    
01384          MOVE ZEROS TO DET-TMS-CERTS DET-TMS-CANCELS
                             DET-TMS-LCOVERAGE DET-TMS-ACOVERAGE
                             DET-TMS-LCAN DET-TMS-LPRM                
01385                        DET-TMS-ACAN DET-TMS-APRM DET-TMS-LCLM                 
01386                        DET-TMS-ACLM DET-TMS-TPRM DET-TMS-TCOM                 
01387      ELSE                                                       
01388          PERFORM 0430-ADD-TO-OTHERS THRU 0450-EXIT.               
01389                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01398      GO TO 0410-ACCOUNT-BREAK-PRINT.                              
01399                                                                   
01400  0430-ADD-TO-OTHERS.                                              
01404                                                                   
01421      ADD CERT TO GP-CERT (Y1) CA-CERT (Y1) GR-CERT GT-CERT (Y1).
           ADD CANCELS TO GP-CANCEL (Y1) CA-CANCEL (Y1) GR-CANCEL 
                         GT-CANCEL (Y1).  
01422      ADD LCOVERAGE  TO GP-LCOVERAGE (Y1) CA-LCOVERAGE (Y1)
                         GR-LCOVERAGE  GT-LCOVERAGE (Y1).  
01422      ADD ACOVERAGE  TO GP-ACOVERAGE (Y1) CA-ACOVERAGE (Y1)
                         GR-ACOVERAGE  GT-ACOVERAGE (Y1).  
01423      ADD LCAN TO GP-LCAN (Y1) CA-LCAN (Y1) GR-LCAN GT-LCAN (Y1).  
01424      ADD LPRM TO GP-LPRM (Y1) CA-LPRM (Y1) GR-LPRM GT-LPRM (Y1).  
01425      ADD LCLM TO GP-LCLM (Y1) CA-LCLM (Y1) GR-LCLM GT-LCLM (Y1).  
01426      ADD ACAN TO GP-ACAN (Y1) CA-ACAN (Y1) GR-ACAN GT-ACAN (Y1).  
01427      ADD APRM TO GP-APRM (Y1) CA-APRM (Y1) GR-APRM GT-APRM (Y1).  
01428      ADD ACLM TO GP-ACLM (Y1) CA-ACLM (Y1) GR-ACLM GT-ACLM (Y1).  
01429      ADD TPRM TO GP-TPRM (Y1) CA-TPRM (Y1) GR-TPRM GT-TPRM (Y1).  
01430      ADD TCOM TO GP-TCOM (Y1) CA-TCOM (Y1) GR-TCOM GT-TCOM (Y1).  
01431                                                                   
01432  0440-ADD-CONTINUE.                                               
01433      IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE  
               ADD CERT TO L12AC-CERT
               ADD CANCELS TO L12AC-CANCEL
               ADD LCOVERAGE TO L12AC-LCOVERAGE
               ADD ACOVERAGE TO L12AC-ACOVERAGE                   
01434          ADD LCAN TO L12AC-LCAN                                   
01435          ADD LPRM TO L12AC-LPRM                                   
01436          ADD LCLM TO L12AC-LCLM                                   
01437          ADD ACAN TO L12AC-ACAN                                   
01438          ADD APRM TO L12AC-APRM                                   
01439          ADD ACLM TO L12AC-ACLM                                   
01440          ADD TPRM TO L12AC-TPRM                                   
01441          ADD TCOM TO L12AC-TCOM                                   

               ADD CERT TO L12GP-CERT L12CA-CERT L12GT-CERT
               ADD CANCELS TO L12GP-CANCEL L12CA-CANCEL L12GT-CANCEL
               ADD LCOVERAGE TO L12GP-LCOVERAGE L12CA-LCOVERAGE
                                L12GT-LCOVERAGE
               ADD ACOVERAGE TO L12GP-ACOVERAGE L12CA-ACOVERAGE
                                L12GT-ACOVERAGE                  
01450          ADD LCAN TO L12GP-LCAN L12CA-LCAN L12GT-LCAN               
01451          ADD LPRM TO L12GP-LPRM L12CA-LPRM L12GT-LPRM               
01452          ADD LCLM TO L12GP-LCLM L12CA-LCLM L12GT-LCLM               
01453          ADD ACAN TO L12GP-ACAN L12CA-ACAN L12GT-ACAN               
01454          ADD APRM TO L12GP-APRM L12CA-APRM L12GT-APRM               
01455          ADD ACLM TO L12GP-ACLM L12CA-ACLM L12GT-ACLM               
01456          ADD TPRM TO L12GP-TPRM L12CA-TPRM L12GT-TPRM               
01457          ADD TCOM TO L12GP-TCOM L12CA-TCOM L12GT-TCOM.              
01458                                                                   
01459      IF RUN-YR = COMP-YR (X1)                                     
               ADD CERT TO YTDAC-CERT
               ADD CANCELS TO YTDAC-CANCEL
               ADD LCOVERAGE TO YTDAC-LCOVERAGE
               ADD ACOVERAGE TO YTDAC-ACOVERAGE                   
01460          ADD LCAN TO YTDAC-LCAN                                   
01461          ADD LPRM TO YTDAC-LPRM                                   
01462 *                     YTDAC-TPRM                                   
01463          ADD LCLM TO YTDAC-LCLM                                   
               ADD TPRM TO YTDAC-TPRM
01464          ADD TCOM TO YTDAC-TCOM                                   
01465          ADD ACAN TO YTDAC-ACAN                                   
01466          ADD APRM TO YTDAC-APRM                                   
01467 *                     YTDAC-TPRM                                   
01468          ADD ACLM TO YTDAC-ACLM                                   

               ADD CERT TO YTDGP-CERT YTDCA-CERT YTDGT-CERT
               ADD CANCELS TO YTDGP-CANCEL YTDCA-CANCEL YTDGT-CANCEL
               ADD LCOVERAGE TO YTDGP-LCOVERAGE YTDCA-LCOVERAGE
                                YTDGT-LCOVERAGE
               ADD ACOVERAGE TO YTDGP-ACOVERAGE YTDCA-ACOVERAGE
                                YTDGT-ACOVERAGE                  
               ADD LCAN TO YTDGP-LCAN YTDCA-LCAN YTDGT-LCAN               
               ADD LPRM TO YTDGP-LPRM YTDCA-LPRM YTDGT-LPRM               
      *                     YTDGP-TPRM YTDCA-TPRM                
               ADD LCLM TO YTDGP-LCLM YTDCA-LCLM YTDGT-LCLM               
               ADD TPRM TO YTDGP-TPRM YTDCA-TPRM YTDGT-TPRM
               ADD TCOM TO YTDGP-TCOM YTDCA-TCOM YTDGT-TCOM               
               ADD ACAN TO YTDGP-ACAN YTDCA-ACAN YTDGT-ACAN     
               ADD APRM TO YTDGP-APRM YTDCA-APRM YTDGT-APRM     
      *                     YTDGP-TPRM YTDCA-TPRM                
                ADD ACLM TO YTDGP-ACLM YTDCA-ACLM YTDGT-ACLM.               
                                                                   
           IF X1 = 13
              COMPUTE PYTDAC-CERT = PYTDAC-CERT +
                 (AC-CERT (1) - AC-CERT (15))
              COMPUTE PYTDAC-CANCEL = PYTDAC-CANCEL +
                 (AC-CANCEL (1) - AC-CANCEL (15))
              COMPUTE PYTDAC-LCOVERAGE = PYTDAC-LCOVERAGE +
                 (AC-LCOVERAGE (1) - AC-LCOVERAGE (15))
              COMPUTE PYTDAC-ACOVERAGE = PYTDAC-ACOVERAGE +
                 (AC-ACOVERAGE (1) - AC-ACOVERAGE (15))
              COMPUTE PYTDAC-LCAN = PYTDAC-LCAN +
                 (AC-LCAN (1) - AC-LCAN (15))
              COMPUTE PYTDAC-LPRM = PYTDAC-LPRM +
                 (AC-LPRM (1) - AC-LPRM (15))
              COMPUTE PYTDAC-LCLM = PYTDAC-LCLM +
                 (AC-LCLM (1) - AC-LCLM (15))
              COMPUTE PYTDAC-ACAN = PYTDAC-ACAN +
                 (AC-ACAN (1) - AC-ACAN (15))
              COMPUTE PYTDAC-APRM = PYTDAC-APRM +
                 (AC-APRM (1) - AC-APRM (15))
              COMPUTE PYTDAC-ACLM = PYTDAC-ACLM +
                 (AC-ACLM (1) - AC-ACLM (15))
              COMPUTE PYTDAC-TPRM = PYTDAC-TPRM +
                 (AC-TPRM (1) - AC-TPRM (15))
              COMPUTE PYTDAC-TCOM = PYTDAC-TCOM +
                 (AC-TCOM (1) - AC-TCOM (15))

              ADD PYTDAC-CERT TO PYTDGP-CERT PYTDCA-CERT PYTDGT-CERT
              ADD PYTDAC-CANCEL TO PYTDGP-CANCEL PYTDCA-CANCEL 
                                   PYTDGT-CANCEL
              ADD PYTDAC-LCOVERAGE TO PYTDGP-LCOVERAGE 
                                      PYTDCA-LCOVERAGE
                                      PYTDGT-LCOVERAGE
              ADD PYTDAC-ACOVERAGE TO PYTDGP-ACOVERAGE 
                                      PYTDCA-ACOVERAGE
                                      PYTDGT-ACOVERAGE
              ADD PYTDAC-LCAN TO PYTDGP-LCAN PYTDCA-LCAN PYTDGT-LCAN
              ADD PYTDAC-LPRM TO PYTDGP-LPRM PYTDCA-LPRM PYTDGT-LPRM
              ADD PYTDAC-LCLM TO PYTDGP-LCLM PYTDCA-LCLM PYTDGT-LCLM
              ADD PYTDAC-ACAN TO PYTDGP-ACAN PYTDCA-ACAN PYTDGT-ACAN
              ADD PYTDAC-APRM TO PYTDGP-APRM PYTDCA-APRM PYTDGT-APRM
              ADD PYTDAC-ACLM TO PYTDGP-ACLM PYTDCA-ACLM PYTDGT-ACLM
              ADD PYTDAC-TPRM TO PYTDGP-TPRM PYTDCA-TPRM PYTDGT-TPRM
              ADD PYTDAC-TCOM TO PYTDGP-TCOM PYTDCA-TCOM PYTDGT-TCOM.
              
01487  0450-EXIT.                                                       
01488       EXIT.                                                       
01489                                                                   
01490  0460-PRINT-LAST-AC-12.                                           

03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE L12AC-CERT             TO DET-TMS-CERTS
           MOVE L12AC-CANCEL           TO DET-TMS-CANCELS
           MOVE L12AC-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE L12AC-ACOVERAGE        TO DET-TMS-ACOVERAGE
01531      MOVE L12AC-LCAN             TO DET-TMS-LCAN.                     
01532      MOVE L12AC-LPRM             TO DET-TMS-LPRM.                     
01533      MOVE L12AC-LCLM             TO DET-TMS-LCLM.                     
01534      MOVE L12AC-ACAN             TO DET-TMS-ACAN.                     
01535      MOVE L12AC-APRM             TO DET-TMS-APRM.                     
01536      MOVE L12AC-ACLM             TO DET-TMS-ACLM.                     
01537      MOVE L12AC-TPRM             TO DET-TMS-TPRM.                     
01538      MOVE L12AC-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (L12AC-TPRM - L12AC-TCOM).    
01539      MOVE '*12 MO'               TO DET-TMS-DATE.                    
01540                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01547      MOVE ZERO-ACCUM             TO LAST-12-ACCOUNT-ACCUM.        
01548                                                                   
           .
01549  0490-PRINT-AC-YTD.                                               

03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE YTDAC-CERT             TO DET-TMS-CERTS
           MOVE YTDAC-CANCEL           TO DET-TMS-CANCELS
           MOVE YTDAC-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE YTDAC-ACOVERAGE        TO DET-TMS-ACOVERAGE
01550      MOVE YTDAC-LCAN             TO DET-TMS-LCAN.                     
01551      MOVE YTDAC-LPRM             TO DET-TMS-LPRM.                     
01552      MOVE YTDAC-LCLM             TO DET-TMS-LCLM.                     
01553      MOVE YTDAC-ACAN             TO DET-TMS-ACAN.                     
01554      MOVE YTDAC-APRM             TO DET-TMS-APRM.                     
01555      MOVE YTDAC-ACLM             TO DET-TMS-ACLM.                     
01556      MOVE YTDAC-TPRM             TO DET-TMS-TPRM.                     
01557      MOVE YTDAC-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (YTDAC-TPRM - YTDAC-TCOM).    
01558      MOVE '*YTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01562      MOVE SPACES                 TO DET-TMS-TITLE.                    
01563                                                                   
      ********   PRINT PRIOR YTD   

03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE PYTDAC-CERT            TO DET-TMS-CERTS
           MOVE PYTDAC-CANCEL          TO DET-TMS-CANCELS
           MOVE PYTDAC-LCOVERAGE       TO DET-TMS-LCOVERAGE
           MOVE PYTDAC-ACOVERAGE       TO DET-TMS-ACOVERAGE
122804     MOVE PYTDAC-LCAN            TO DET-TMS-LCAN
122804     MOVE PYTDAC-LPRM            TO DET-TMS-LPRM
122804     MOVE PYTDAC-LCLM            TO DET-TMS-LCLM
122804     MOVE PYTDAC-ACAN            TO DET-TMS-ACAN
122804     MOVE PYTDAC-APRM            TO DET-TMS-APRM
122804     MOVE PYTDAC-ACLM            TO DET-TMS-ACLM
122804     MOVE PYTDAC-TPRM            TO DET-TMS-TPRM
122804     MOVE PYTDAC-TCOM            TO DET-TMS-TCOM
03462      COMPUTE DET-TMS-NPRM = (PYTDAC-TPRM - PYTDAC-TCOM).    
122804     MOVE '*PYTD'                TO DET-TMS-DATE

01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
122804     MOVE SPACES                 TO DET-TMS-TITLE
122804                                                                  
01593      MOVE ZERO-ACCUM             TO YTD-ACCOUNT-ACCUM             
122804                                    PYTD-ACCOUNT-ACCUM.
                        
01599  0495-ZERO-ACCOUNT.                                               
01600      MOVE ' '                    TO SKIP-HD-SW.                   
01601      MOVE +0                     TO X1.                           
01602      PERFORM 0230-ZERO-ACCUM-ACC 15 TIMES.                        
01603                                                                   
01604  0499-ACCOUNT-RETURN.                                             
01605      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01606  EJECT                                                            
01607  0500-GROUPING-BREAK.                                             
01608      PERFORM 0400-ACCOUNT-BREAK THRU 0495-ZERO-ACCOUNT.  
           PERFORM 0375-STATE-BREAK-1 THRU 0380-ZERO-STATE.         
01609                                                                   
01610  0510-GROUPING-BREAK-1.                                           
01611      IF P-GP-SW = '1'                                             
01612          MOVE ' '                TO P-GP-SW                       
01613      ELSE                                                         
01614          GO TO 0580-ZERO-GROUPING.                                
01615                                                                   
01616      PERFORM 0960-GP-HD          THRU 0960-EXIT.                                          
01617      MOVE +1                     TO X1.                           
01618                                                                   
01700  0550-GROUPING-BREAK-2.                                           
01701      IF X1 GREATER 12                                             
01702          GO TO 0560-PRINT-LAST-GP-12.                             
01703                                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE GP-CERT (X1)           TO DET-TMS-CERTS
           MOVE GP-CANCEL (X1)         TO DET-TMS-CANCELS
           MOVE GP-LCOVERAGE (X1)      TO DET-TMS-LCOVERAGE
           MOVE GP-ACOVERAGE (X1)      TO DET-TMS-ACOVERAGE
01705      MOVE GP-LCAN (X1)           TO DET-TMS-LCAN.                     
01706      MOVE GP-LPRM (X1)           TO DET-TMS-LPRM.                     
01707      MOVE GP-LCLM (X1)           TO DET-TMS-LCLM.                     
01708      MOVE GP-ACAN (X1)           TO DET-TMS-ACAN.                     
01709      MOVE GP-APRM (X1)           TO DET-TMS-APRM.                     
01710      MOVE GP-ACLM (X1)           TO DET-TMS-ACLM.                     
01711      MOVE GP-TPRM (X1)           TO DET-TMS-TPRM.                     
01712      MOVE GP-TCOM (X1)           TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (GP-TPRM (X1) - GP-TCOM (X1)).    
01713      ADD +1 TO X1.                                                
01714      MOVE COMP-YR (X1)           TO DET-TMS-YR.                       
01715      MOVE COMP-MO (X1)           TO DET-TMS-MO.                       
01716                                                                   
01717      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01718                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01727      GO TO 0550-GROUPING-BREAK-2.                                 
01728                                                                   
01729  0560-PRINT-LAST-GP-12.                                           
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE L12GP-CERT             TO DET-TMS-CERTS
           MOVE L12GP-CANCEL           TO DET-TMS-CANCELS
           MOVE L12GP-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE L12GP-ACOVERAGE        TO DET-TMS-ACOVERAGE
01730      MOVE L12GP-LCAN             TO DET-TMS-LCAN.                     
01731      MOVE L12GP-LPRM             TO DET-TMS-LPRM.                     
01732      MOVE L12GP-LCLM             TO DET-TMS-LCLM.                     
01733      MOVE L12GP-ACAN             TO DET-TMS-ACAN.                     
01734      MOVE L12GP-APRM             TO DET-TMS-APRM.                     
01735      MOVE L12GP-ACLM             TO DET-TMS-ACLM.                     
01736      MOVE L12GP-TPRM             TO DET-TMS-TPRM.                     
01737      MOVE L12GP-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (L12GP-TPRM - L12GP-TCOM).    
01738      MOVE '*12 MO'               TO DET-TMS-DATE.                    
01739                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01746      MOVE ZERO-ACCUM             TO LAST-12-GROUPING-ACCUM.       
01747                                                                   
01748  0570-PRINT-GP-YTD.                                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE YTDGP-CERT             TO DET-TMS-CERTS
           MOVE YTDGP-CANCEL           TO DET-TMS-CANCELS
           MOVE YTDGP-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE YTDGP-ACOVERAGE        TO DET-TMS-ACOVERAGE
01749      MOVE YTDGP-LCAN             TO DET-TMS-LCAN.                     
01750      MOVE YTDGP-LPRM             TO DET-TMS-LPRM.                     
01751      MOVE YTDGP-LCLM             TO DET-TMS-LCLM.                     
01752      MOVE YTDGP-ACAN             TO DET-TMS-ACAN.                     
01753      MOVE YTDGP-APRM             TO DET-TMS-APRM.                     
01754      MOVE YTDGP-ACLM             TO DET-TMS-ACLM.                     
01755      MOVE YTDGP-TPRM             TO DET-TMS-TPRM.                     
01756      MOVE YTDGP-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (YTDGP-TPRM - YTDGP-TCOM).    
01757      MOVE '*YTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01762      MOVE ZERO-ACCUM             TO YTD-GROUPING-ACCUM.           
01763                                                                   
03448      MOVE LIFE-OVERRIDE-L2        TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2          TO DET-TMS-ADESC.                
           MOVE PYTDGP-CERT             TO DET-TMS-CERTS
           MOVE PYTDGP-CANCEL           TO DET-TMS-CANCELS
           MOVE PYTDGP-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE PYTDGP-ACOVERAGE        TO DET-TMS-ACOVERAGE
122804     MOVE PYTDGP-LCAN             TO DET-TMS-LCAN.                     
122804     MOVE PYTDGP-LPRM             TO DET-TMS-LPRM.                     
122804     MOVE PYTDGP-LCLM             TO DET-TMS-LCLM.                     
122804     MOVE PYTDGP-ACAN             TO DET-TMS-ACAN.                     
122804     MOVE PYTDGP-APRM             TO DET-TMS-APRM.                     
122804     MOVE PYTDGP-ACLM             TO DET-TMS-ACLM.                     
122804     MOVE PYTDGP-TPRM             TO DET-TMS-TPRM.                     
122804     MOVE PYTDGP-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (PYTDGP-TPRM - PYTDGP-TCOM).    
122804     MOVE '*PYTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     

01762      MOVE ZERO-ACCUM             TO PYTD-GROUPING-ACCUM

122804     MOVE SPACES                 TO DET-TMS-TITLE.                    
122804                                                                  
01764  0580-ZERO-GROUPING.                                              
01765      MOVE +0                     TO X1.                           
01766      PERFORM 0240-ZERO-ACCUM-GRP 15 TIMES.                        
01767                                                                   
01768  0590-GROUPING-RETURN.                                            
01769      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01770  EJECT                                                            
01771  0600-CARRIER-BREAK.                                              
01772      PERFORM 0400-ACCOUNT-BREAK    THRU 0495-ZERO-ACCOUNT. 
           PERFORM 0375-STATE-BREAK-1    THRU 0380-ZERO-STATE.       
01773  0605-CARRIER-BREAK-5.                                            
01774      PERFORM 0510-GROUPING-BREAK-1 THRU 0580-ZERO-GROUPING.       
01775                                                                   
01776  0610-CARRIER-BREAK-1.                                            
01777      IF P-CA-SW = '1'                                             
01778          MOVE ' '                TO P-CA-SW                       
01779      ELSE                                                         
01780          GO TO 0680-ZERO-CARRIER.                                 
01781                                                                   
01782      PERFORM 0970-CA-HD          THRU 0970-EXIT.                                          
01783      MOVE +1                     TO X1.                           
01784                                                                   
01866  0650-CARRIER-BREAK-2.                                            
01867      IF X1 GREATER 12                                             
01868          GO TO 0660-PRINT-LAST-CA-12.                             
01869                                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
01870      MOVE CA-CERT (X1)           TO DET-TMS-CERTS.
           MOVE CA-CANCEL (X1)         TO DET-TMS-CANCELS.
           MOVE CA-LCOVERAGE (X1)      TO DET-TMS-LCOVERAGE.
           MOVE CA-ACOVERAGE (X1)      TO DET-TMS-ACOVERAGE.
01871      MOVE CA-LCAN (X1)           TO DET-TMS-LCAN.                     
01872      MOVE CA-LPRM (X1)           TO DET-TMS-LPRM.                     
01873      MOVE CA-LCLM (X1)           TO DET-TMS-LCLM.                     
01874      MOVE CA-ACAN (X1)           TO DET-TMS-ACAN.                     
01875      MOVE CA-APRM (X1)           TO DET-TMS-APRM.                     
01876      MOVE CA-ACLM (X1)           TO DET-TMS-ACLM.                     
01877      MOVE CA-TPRM (X1)           TO DET-TMS-TPRM.                     
01878      MOVE CA-TCOM (X1)           TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (CA-TPRM (X1) - CA-TCOM (X1)).    
01879      ADD +1 TO X1.                                                
01880      MOVE COMP-YR (X1)           TO DET-TMS-YR.                       
01881      MOVE COMP-MO (X1)           TO DET-TMS-MO.                       
01882                                                                   
01883      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
01884                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01893      GO TO 0650-CARRIER-BREAK-2.                                  
01894                                                                   
01895  0660-PRINT-LAST-CA-12.                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE L12CA-CERT             TO DET-TMS-CERTS.
           MOVE L12CA-CANCEL           TO DET-TMS-CANCELS.
           MOVE L12CA-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE L12CA-ACOVERAGE        TO DET-TMS-ACOVERAGE.            
01896      MOVE L12CA-LCAN             TO DET-TMS-LCAN.                     
01897      MOVE L12CA-LPRM             TO DET-TMS-LPRM.                     
01898      MOVE L12CA-LCLM             TO DET-TMS-LCLM.                     
01899      MOVE L12CA-ACAN             TO DET-TMS-ACAN.                     
01900      MOVE L12CA-APRM             TO DET-TMS-APRM.                     
01901      MOVE L12CA-ACLM             TO DET-TMS-ACLM.                     
01902      MOVE L12CA-TPRM             TO DET-TMS-TPRM.                     
01903      MOVE L12CA-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (L12CA-TPRM - L12CA-TCOM).    
01904      MOVE '*12 MO'               TO DET-TMS-DATE.                    
01905                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01912      MOVE ZERO-ACCUM             TO LAST-12-CARRIER-ACCUM.        
01913                                                                   
01914  0670-PRINT-CA-YTD.                                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE YTDCA-CERT             TO DET-TMS-CERTS.
           MOVE YTDCA-CANCEL           TO DET-TMS-CANCELS.
           MOVE YTDCA-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE YTDCA-ACOVERAGE        TO DET-TMS-ACOVERAGE.            
01915      MOVE YTDCA-LCAN             TO DET-TMS-LCAN.                     
01916      MOVE YTDCA-LPRM             TO DET-TMS-LPRM.                     
01917      MOVE YTDCA-LCLM             TO DET-TMS-LCLM.                     
01918      MOVE YTDCA-ACAN             TO DET-TMS-ACAN.                     
01919      MOVE YTDCA-APRM             TO DET-TMS-APRM.                     
01920      MOVE YTDCA-ACLM             TO DET-TMS-ACLM.                     
01921      MOVE YTDCA-TPRM             TO DET-TMS-TPRM.                     
01922      MOVE YTDCA-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (YTDCA-TPRM - YTDCA-TCOM).    
01923      MOVE '*YTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01927      MOVE SPACES                 TO DET-TMS-TITLE.                    
01928      MOVE ZERO-ACCUM             TO YTD-CARRIER-ACCUM.            
01929                                                                   
03448      MOVE LIFE-OVERRIDE-L2        TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2          TO DET-TMS-ADESC.                
           MOVE PYTDCA-CERT             TO DET-TMS-CERTS.
           MOVE PYTDCA-CANCEL           TO DET-TMS-CANCELS.
           MOVE PYTDCA-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE PYTDCA-ACOVERAGE        TO DET-TMS-ACOVERAGE.            
122804     MOVE PYTDCA-LCAN             TO DET-TMS-LCAN.                     
122804     MOVE PYTDCA-LPRM             TO DET-TMS-LPRM.                     
122804     MOVE PYTDCA-LCLM             TO DET-TMS-LCLM.                     
122804     MOVE PYTDCA-ACAN             TO DET-TMS-ACAN.                     
122804     MOVE PYTDCA-APRM             TO DET-TMS-APRM.                     
122804     MOVE PYTDCA-ACLM             TO DET-TMS-ACLM.                     
122804     MOVE PYTDCA-TPRM             TO DET-TMS-TPRM.                     
122804     MOVE PYTDCA-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (PYTDCA-TPRM - PYTDCA-TCOM).    
122804     MOVE '*PYTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     

01928      MOVE ZERO-ACCUM             TO PYTD-CARRIER-ACCUM.            
122804     MOVE SPACES                 TO DET-TMS-TITLE.                    
122804                                                                  
01930  0680-ZERO-CARRIER.                                               
01931      MOVE +0                     TO X1.                           
01932      PERFORM 0250-ZERO-ACCUM-CARR 15 TIMES.                       
01933                                                                   
01934  0690-CARRIER-RETURN.                                             
01935      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
01936  EJECT                                                            
01937  0700-REPTCD-BREAK.                                               
01772      PERFORM 0400-ACCOUNT-BREAK    THRU 0495-ZERO-ACCOUNT. 
           PERFORM 0375-STATE-BREAK-1    THRU 0380-ZERO-STATE.       
01774      PERFORM 0510-GROUPING-BREAK-1 THRU 0580-ZERO-GROUPING.       
           PERFORM 0610-CARRIER-BREAK-1  THRU 0680-ZERO-CARRIER.
01941                                                                   
01942      IF P-RP-SW = '1'                                             
01943          MOVE ' '                TO P-RP-SW                       
01944      ELSE                                                         
01945          GO TO 0790-ZERO-REPTCD.                                  
01946                                                                   
01947      PERFORM 0990-RP-HD          THRU 0990-EXIT.                        
01948                                                                   
01949      MOVE +1                     TO X1.                           
01950      MOVE +0                     TO Y1.                           
01951                                                                   
02018  0720-REPTCD-BREAK-POINT.                                         
02019      ADD +1 TO X1 Y1.                                             
02020                                                                   
02021      IF X1 GREATER THAN 13                                        
02022          GO TO 0750-PRINT-LAST-RP-12.                             
02023                                                                   
02024      COMPUTE RPCERT = RP-CERT (X1) - RP-CERT (Y1).
           COMPUTE RPCANCEL = RP-CANCEL (X1) - RP-CANCEL (Y1).
           COMPUTE RPLCOVERAGE = RP-LCOVERAGE (X1) - 
                                 RP-LCOVERAGE (Y1).                
           COMPUTE RPACOVERAGE = RP-ACOVERAGE (X1) - 
                                 RP-ACOVERAGE (Y1).                
02025      COMPUTE RPLCAN = RP-LCAN (X1) - RP-LCAN (Y1).                
02026      COMPUTE RPLPRM = RP-LPRM (X1) - RP-LPRM (Y1).                
02027      COMPUTE RPLCLM = RP-LCLM (X1) - RP-LCLM (Y1).                
02028      COMPUTE RPACAN = RP-ACAN (X1) - RP-ACAN (Y1).                
02029      COMPUTE RPAPRM = RP-APRM (X1) - RP-APRM (Y1).                
02030      COMPUTE RPACLM = RP-ACLM (X1) - RP-ACLM (Y1).                
02031      COMPUTE RPTCOM = RP-TCOM (X1) - RP-TCOM (Y1).                
02032 *     COMPUTE RPTPRM = (RPLPRM  + RPAPRM) - (RPLCAN + RPACAN).                           
           COMPUTE RPTPRM = RP-TPRM (X1) - RP-TPRM (Y1).
02033      MOVE COMP-YR (X1)           TO DET-TMS-YR.                       
02034      MOVE COMP-MO (X1)           TO DET-TMS-MO.                       
02035      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
02036                                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
02037      MOVE RPCERT                 TO DET-TMS-CERTS.                    
           MOVE RPCANCEL               TO DET-TMS-CANCELS.
           MOVE RPLCOVERAGE            TO DET-TMS-LCOVERAGE.
           MOVE RPACOVERAGE            TO DET-TMS-ACOVERAGE.
02038      MOVE RPLCAN                 TO DET-TMS-LCAN.                     
02039      MOVE RPLPRM                 TO DET-TMS-LPRM.                     
02040      MOVE RPLCLM                 TO DET-TMS-LCLM.                     
02041      MOVE RPACAN                 TO DET-TMS-ACAN.                     
02042      MOVE RPAPRM                 TO DET-TMS-APRM.                     
02043      MOVE RPACLM                 TO DET-TMS-ACLM.                     
02044      MOVE RPTPRM                 TO DET-TMS-TPRM.                     
02045      MOVE RPTCOM                 TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (RPTPRM - RPTCOM).    
02046                                                                   
02047      IF COMP-YR (X1) = CONV-YR AND                                
02048         COMP-MO (X1) = CONV-MO                                    
02049          MOVE ZEROS TO DET-TMS-CERTS DET-TMS-CANCELS
                             DET-TMS-LCOVERAGE DET-TMS-ACOVERAGE
                             DET-TMS-LCAN DET-TMS-LPRM                
02050                        DET-TMS-ACAN DET-TMS-APRM DET-TMS-LCLM                 
02051                        DET-TMS-ACLM DET-TMS-TPRM DET-TMS-TCOM                 
02052         ELSE                                                      
02053           PERFORM 0730-ADD-TO-L12-YTD THRU 0740-EXIT.             
02054                                                                   
02055      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-3       TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02063      GO TO 0720-REPTCD-BREAK-POINT.                               
02064                                                                   
02065  0730-ADD-TO-L12-YTD.                                             
02066      IF COMPARE9DT (X1) GREATER YEAR-OLD-DATE  
               ADD RPCERT TO L12RP-CERT
               ADD RPCANCEL TO L12RP-CANCEL
               ADD RPLCOVERAGE TO L12RP-LCOVERAGE
               ADD RPACOVERAGE TO L12RP-ACOVERAGE
02067          ADD RPLCAN TO L12RP-LCAN                                 
02068          ADD RPLPRM TO L12RP-LPRM                                 
02069          ADD RPLCLM TO L12RP-LCLM                                 
02070          ADD RPACAN TO L12RP-ACAN                                 
02071          ADD RPAPRM TO L12RP-APRM                                 
02072          ADD RPACLM TO L12RP-ACLM                                 
02073          ADD RPTPRM TO L12RP-TPRM                                 
02074          ADD RPTCOM TO L12RP-TCOM.                                
02075                                                                   
02076      IF RUN-YR = COMP-YR (X1)                                     
               ADD RPCERT TO YTDRP-CERT
               ADD RPCANCEL TO YTDRP-CANCEL
               ADD RPLCOVERAGE TO YTDRP-LCOVERAGE
               ADD RPACOVERAGE TO YTDRP-ACOVERAGE
02077          ADD RPLCAN TO YTDRP-LCAN                                 
02078          ADD RPLPRM TO YTDRP-LPRM                                 
02079 *                       YTDRP-TPRM                                 
02080          ADD RPLCLM TO YTDRP-LCLM                                 
               ADD RPTPRM TO YTDRP-TPRM
02081          ADD RPTCOM TO YTDRP-TCOM                                 
02082          ADD RPACAN TO YTDRP-ACAN                                 
02083          ADD RPAPRM TO YTDRP-APRM                                 
02084 *                       YTDRP-TPRM                                 
02085          ADD RPACLM TO YTDRP-ACLM.                                
02086                                                                   
122804     IF X1 = 13
              COMPUTE PYTDRP-CERT = PYTDRP-CERT +
                 (RP-CERT (1) - RP-CERT (15))
              COMPUTE PYTDRP-CANCEL = PYTDRP-CANCEL +
                 (RP-CANCEL (1) - RP-CANCEL (15))
              COMPUTE PYTDRP-LCOVERAGE = PYTDRP-LCOVERAGE +
                 (RP-LCOVERAGE (1) - RP-LCOVERAGE (15))
              COMPUTE PYTDRP-ACOVERAGE = PYTDRP-ACOVERAGE +
                 (RP-ACOVERAGE (1) - RP-ACOVERAGE (15))
              COMPUTE PYTDRP-LCAN = PYTDRP-LCAN +
                 (RP-LCAN (1) - RP-LCAN (15))
              COMPUTE PYTDRP-LPRM = PYTDRP-LPRM +
                 (RP-LPRM (1) - RP-LPRM (15))
              COMPUTE PYTDRP-LCLM = PYTDRP-LCLM +
                 (RP-LCLM (1) - RP-LCLM (15))
              COMPUTE PYTDRP-ACAN = PYTDRP-ACAN +
                 (RP-ACAN (1) - RP-ACAN (15))
              COMPUTE PYTDRP-APRM = PYTDRP-APRM +
                 (RP-APRM (1) - RP-APRM (15))
              COMPUTE PYTDRP-ACLM = PYTDRP-ACLM +
                 (RP-ACLM (1) - RP-ACLM (15))
              COMPUTE PYTDRP-TPRM = PYTDRP-TPRM +
                 (RP-TPRM (1) - RP-TPRM (15))
              COMPUTE PYTDRP-TCOM = PYTDRP-TCOM +
                 (RP-TCOM (1) - RP-TCOM (15))
           END-IF.

02087  0740-EXIT.                                                       
02088       EXIT.                                                       
02089                                                                   
02090  0750-PRINT-LAST-RP-12.                                           
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE L12RP-CERT             TO DET-TMS-CERTS.
           MOVE L12RP-CANCEL           TO DET-TMS-CANCELS.
           MOVE L12RP-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE L12RP-ACOVERAGE        TO DET-TMS-ACOVERAGE.                           
02131      MOVE L12RP-LCAN             TO DET-TMS-LCAN.                     
02132      MOVE L12RP-LPRM             TO DET-TMS-LPRM.                     
02133      MOVE L12RP-LCLM             TO DET-TMS-LCLM.                     
02134      MOVE L12RP-ACAN             TO DET-TMS-ACAN.                     
02135      MOVE L12RP-APRM             TO DET-TMS-APRM.                     
02136      MOVE L12RP-ACLM             TO DET-TMS-ACLM.                     
02137      MOVE L12RP-TPRM             TO DET-TMS-TPRM.                     
02138      MOVE L12RP-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (L12RP-TPRM - L12RP-TCOM).    
02139      MOVE '*12 MO'               TO DET-TMS-DATE.                    
02140                                                                   
02144      MOVE ' '                    TO X.                            
02145      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
02146      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.        
             
02147      MOVE ZERO-ACCUM             TO LAST-12-REPTCD-ACCUM.         

02149  0780-PRINT-RP-YTD.                                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE YTDRP-CERT             TO DET-TMS-CERTS.
           MOVE YTDRP-CANCEL           TO DET-TMS-CANCELS.
           MOVE YTDRP-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE YTDRP-ACOVERAGE        TO DET-TMS-ACOVERAGE.                           
02150      MOVE YTDRP-LCAN             TO DET-TMS-LCAN.                     
02151      MOVE YTDRP-LPRM             TO DET-TMS-LPRM.                     
02152      MOVE YTDRP-LCLM             TO DET-TMS-LCLM.                     
02153      MOVE YTDRP-ACAN             TO DET-TMS-ACAN.                     
02154      MOVE YTDRP-APRM             TO DET-TMS-APRM.                     
02155      MOVE YTDRP-ACLM             TO DET-TMS-ACLM.                     
02156      MOVE YTDRP-TPRM             TO DET-TMS-TPRM.                     
02157      MOVE YTDRP-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (YTDRP-TPRM - YTDRP-TCOM).    
02158      MOVE '*YTD'                 TO DET-TMS-DATE.                    
02159      MOVE ' '                    TO X.                            
02145      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
02146      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.        
02162      MOVE SPACES                 TO DET-TMS-TITLE.                    
02163      MOVE ZERO-ACCUM             TO YTD-REPTCD-ACCUM.             
02164                                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE PYTDRP-CERT             TO DET-TMS-CERTS.
           MOVE PYTDRP-CANCEL           TO DET-TMS-CANCELS.
           MOVE PYTDRP-LCOVERAGE        TO DET-TMS-LCOVERAGE.
           MOVE PYTDRP-ACOVERAGE        TO DET-TMS-ACOVERAGE.                           
122804     MOVE PYTDRP-LCAN             TO DET-TMS-LCAN
122804     MOVE PYTDRP-LPRM             TO DET-TMS-LPRM
122804     MOVE PYTDRP-LCLM             TO DET-TMS-LCLM
122804     MOVE PYTDRP-ACAN             TO DET-TMS-ACAN
122804     MOVE PYTDRP-APRM             TO DET-TMS-APRM
122804     MOVE PYTDRP-ACLM             TO DET-TMS-ACLM
122804     MOVE PYTDRP-TPRM             TO DET-TMS-TPRM
122804     MOVE PYTDRP-TCOM             TO DET-TMS-TCOM
03462      COMPUTE DET-TMS-NPRM = (PYTDRP-TPRM - PYTDRP-TCOM).    
122804     MOVE '*PYTD'                 TO DET-TMS-DATE
122804     MOVE ' '                    TO X
02145      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
02146      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-3      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.        

122804     MOVE SPACES                 TO DET-TMS-TITLE
02163      MOVE ZERO-ACCUM             TO PYTD-REPTCD-ACCUM.

02165  0790-ZERO-REPTCD.                                                
02166      MOVE +0                     TO X1.                           
02167      PERFORM 0260-ZERO-ACCUM-REPT 15 TIMES.                       
02168                                                                   
02169  0799-REPTCD-RETURN.                                              
02170      GO TO 0310-READ-EARNED-PREM-EXTRACT.                         
02171  EJECT                                                            
02172  0800-PRINT-DECISION.                                             
02173      MOVE +1                     TO X1.                           
02174      MOVE +0                     TO Y1.                           
02175                                                                   
02176  0810-PRINT-DECISION-1.                                           
02177      ADD +1 TO X1 Y1.                                             
02178                                                                   
02179      IF X1 GREATER 13                                             
02180          MOVE +1                 TO X1                            
02181          GO TO 0820-PRINT-DECISION-2.                             
02182                                                 
           COMPUTE CERT = AC-CERT (X1) - AC-CERT (Y1).
           COMPUTE CANCELS = AC-CANCEL (X1) - AC-CANCEL (Y1).
           COMPUTE LCOVERAGE = AC-LCOVERAGE (X1) - AC-LCOVERAGE (Y1).
           COMPUTE ACOVERAGE = AC-ACOVERAGE (X1) - AC-ACOVERAGE (Y1).
02183      COMPUTE LCAN = AC-LCAN (X1) - AC-LCAN (Y1).                  
02184      COMPUTE LPRM = AC-LPRM (X1) - AC-LPRM (Y1).                  
02185      COMPUTE LCLM = AC-LCLM (X1) - AC-LCLM (Y1).                  
02186      COMPUTE ACAN = AC-ACAN (X1) - AC-ACAN (Y1).                  
02187      COMPUTE APRM = AC-APRM (X1) - AC-APRM (Y1).                  
02188      COMPUTE ACLM = AC-ACLM (X1) - AC-ACLM (Y1).                  
02189      COMPUTE TCOM = AC-TCOM (X1) - AC-TCOM (Y1).                  
02190                                                                   
02191      IF COMP-YR (X1) = CONV-YR AND                                
02192         COMP-MO (X1) = CONV-MO                                    
               MOVE ZEROS TO CERT CANCELS LCOVERAGE ACOVERAGE
02193          MOVE ZEROS TO LCAN LPRM LCLM ACAN APRM ACLM TCOM.        
02194                                                                   
           MOVE CERT               TO PCERT (X1)
           MOVE CANCELS            TO PCANCEL (X1)
           MOVE LCOVERAGE          TO PLCOVERAGE (X1)
           MOVE ACOVERAGE          TO PACOVERAGE (X1)                                    
02204      MOVE LCAN               TO PLCAN (X1)                    
02205      MOVE LPRM               TO PLPRM (X1)                    
02206      MOVE LCLM               TO PLCLM (X1)                    
02207      MOVE ACAN               TO PACAN (X1)                    
02208      MOVE APRM               TO PAPRM (X1)                    
02209      MOVE ACLM               TO PACLM (X1)                    
02210      MOVE TCOM               TO PTCOM (X1).                   
02211                                                                   
02212      GO TO 0810-PRINT-DECISION-1.                                 
02213                                                                   
02214  0820-PRINT-DECISION-2.                                           
02215      ADD +1 TO X1.                                                
02216                                                                   
02217      IF X1 GREATER 13                                             
02218          GO TO 0829-EXIT.                                         
02219                                                                   
02220      IF (PRINT-ZERO-TABLE NOT = PRINT-TABLE (X1))
010306        OR ((AC-LPRM (1) - AC-LPRM (15)) NOT = ZEROS)
060122        OR ((AC-Lcan (1) - AC-Lcan (15)) NOT = ZEROS)
010306        OR ((AC-APRM (1) - AC-APRM (15)) NOT = ZEROS)
060122        OR ((AC-acan (1) - AC-acan (15)) NOT = ZEROS)
010306        OR ((AC-LCLM (1) - AC-LCLM (15)) NOT = ZEROS)
010306        OR ((AC-ACLM (1) - AC-ACLM (15)) NOT = ZEROS)
               
02221          MOVE '1'             TO P-ACC-SW P-ST-SW P-GP-SW P-CA-SW 
02222                                  P-RP-SW                          
02223          GO TO 0829-EXIT.                                         
02224                                                                   
02225      GO TO 0820-PRINT-DECISION-2.                                 
02226                                                                   
02227  0829-EXIT.                                                       
02228      EXIT.                                                        
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
02229  EJECT            
02354  0900-ACC-HD.                                                     
02378      ADD +1 TO PAGE-CNT.                                          
02379      MOVE PAGE-CNT               TO HD-PAGE.                      
02380      MOVE '1'                    TO X

101920     perform 0995-set-head       thru 0995-exit

           MOVE S-EPX-A-RPT-CD-1   TO HD-1-SLCT-FLD-2.
           MOVE HD-1-RPT-CODE      TO P-DATA.                                     
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02383      MOVE ' '                    TO X.                            
02384      MOVE HD-2                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02386      MOVE ' '                    TO X.                            
02387      MOVE HD-3                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
122804     MOVE '0'                    TO X.                            
02390                                                                   
02391  0920-EXIT.                                                       
02392  EJECT                                                            
02393  0930-ACC-HD-B.                                                   
02394                                                                   
02395      MOVE S-EPX-A-ACCT           TO HD-ACCOUNT.                   
02323      MOVE SAVE-ACCT-NAME         TO HD-ACCT-NAME               
102004     MOVE SAVE-ACCT-CITY         TO HD-ACCT-ADDRESS
02396      MOVE HD-5                   TO W-WORK-LINE                   
02397      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02398      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02400                                                                   
02401      MOVE ' '                    TO X.                            
02402      MOVE S-EPX-A-STATE            TO STATE-L.                      
02403      PERFORM 1000-STATE-PRT THRU 1010-EXIT.                       
02404      MOVE STATE-ABBR (CLAS-INDEXS)                                
02405                                  TO HD-STATE-ABBR.                
02406      MOVE STATE-PIC (CLAS-INDEXS)                                 
02407                                  TO HD-STATE.                     
02408      MOVE '       IN '           TO HD6-TOTAL-FOR.                
02409      MOVE HD-6                   TO W-WORK-LINE.                  
02410      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT.                   
02411      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02413                                                                   
02414      MOVE ' '                    TO X.                            
02415      MOVE S-EPX-A-GROUP            TO HD-GROUPING.                  
02416      MOVE '       IN '           TO HD4-TOTAL-FOR.                
02417      MOVE HD-4                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02419                                                                   
02420      MOVE ' '                    TO X.                            
02421      MOVE S-EPX-A-CARR           TO HD-CARRIER, CARRIER-L.        
02422      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02423      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02424      MOVE '       IN '           TO HD7-TOTAL-FOR.                
02425      MOVE HD-7                   TO W-WORK-LINE                   
02426      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02427      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02429                                                                   
122804     MOVE '0'                    TO X.                            
           MOVE HD-TMS-9           TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02439                                                                   
02440      MOVE ' '                    TO X.                            
           MOVE HD-TMS-10          TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02447                                                                   
02448  0940-EXIT.                                                       
02449      EXIT.                                                        
02450  EJECT                                                            
02451  0950-ST-HD.                                                      
02452                                                                   
02453      MOVE ZERO                   TO SET-CTR.                      
02454      ADD +1 TO PAGE-CNT.                                          
02455      MOVE PAGE-CNT               TO HD-PAGE.                      
02456      MOVE '1'                    TO X.                            

101920     perform 0995-set-head       thru 0995-exit

112503     MOVE S-EPX-A-RPT-CD-1   TO HD-1-SLCT-FLD-2.
02280      MOVE HD-1-RPT-CODE      TO P-DATA.                                     
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02459                                                                   
02460      MOVE ' '                    TO X.                            
02461      MOVE HD-2                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02463      MOVE ' '                    TO X.                            
02464      MOVE HD-3                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02466                                                                   
122804     MOVE '0'                    TO X.                            
02468      MOVE S-EPX-A-STATE          TO STATE-L.                      
02469      PERFORM 1000-STATE-PRT THRU 1010-EXIT.                       
02470      MOVE STATE-ABBR (CLAS-INDEXS)                                
02471                                  TO HD-STATE-ABBR.                
02472      MOVE STATE-PIC (CLAS-INDEXS)                                 
02473                                  TO HD-STATE.                     
02474      MOVE 'TOTAL FOR '           TO HD6-TOTAL-FOR.                
02475      MOVE HD-6                   TO W-WORK-LINE                   
02476      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02477      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02479                                                                   
02480      MOVE ' '                    TO X.                            
02481      MOVE S-EPX-A-GROUP            TO HD-GROUPING.                  
02482      MOVE '       IN '           TO HD4-TOTAL-FOR.                
02483      MOVE HD-4                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02485                                                                   
02486      MOVE ' '                    TO X.                            
02487      MOVE S-EPX-A-CARR           TO HD-CARRIER, CARRIER-L.        
02488      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02489      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02490      MOVE '       IN '           TO HD7-TOTAL-FOR.                
02491      MOVE HD-7                   TO W-WORK-LINE                   
02492      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02493      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02495                                                                   
122804     MOVE '0'                    TO X.                            
02501      MOVE HD-TMS-9           TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02505      MOVE ' '                    TO X.                            
02508      MOVE HD-TMS-10          TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02512  0950-EXIT.
           EXIT.                                                       
02513  EJECT                                                            
02514  0960-GP-HD.                                                      
02515                                                                   
02516      ADD +1 TO PAGE-CNT.                                          
02517      MOVE PAGE-CNT               TO HD-PAGE.                      
02518      MOVE '1'                    TO X.                            

101920     perform 0995-set-head       thru 0995-exit

112503     MOVE S-EPX-A-RPT-CD-1   TO HD-1-SLCT-FLD-2.
02280      MOVE HD-1-RPT-CODE      TO P-DATA.                                     
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02522      MOVE ' '                    TO X.                            
02523      MOVE HD-2                   TO P-DATA.                       
02524                                                                   
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02526      MOVE ' '                    TO X.                            
02527      MOVE HD-3                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02529                                                                   
122804     MOVE '0'                    TO X.                            
02531      MOVE S-EPX-A-GROUP            TO HD-GROUPING.                  
02532      MOVE 'TOTAL FOR '           TO HD4-TOTAL-FOR.                
02533      MOVE HD-4                   TO W-WORK-LINE                   
02534      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02535      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02537                                                                   
02538      MOVE ' '                    TO X.                            
02539      MOVE S-EPX-A-CARR           TO HD-CARRIER, CARRIER-L.        
02540      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02541      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02542      MOVE '       IN '           TO HD7-TOTAL-FOR.                
02543      MOVE HD-7                   TO W-WORK-LINE                   
02544      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02545      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02547                                                                   
122804     MOVE '0'                    TO X.                            
02553      MOVE HD-TMS-9           TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02557                                                                   
02558      MOVE ' '                    TO X.                            
02561      MOVE HD-TMS-10          TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02565  0960-EXIT.
           EXIT.                                                       
02566  EJECT                                                            
02567                                                                   
02568  0970-CA-HD.                                                      
02569                                                                   
02570      ADD +1 TO PAGE-CNT.                                          
02571      MOVE PAGE-CNT               TO HD-PAGE.                      
02572      MOVE '1'                    TO X.                            

101920     perform 0995-set-head       thru 0995-exit

112503     MOVE S-EPX-A-RPT-CD-1   TO HD-1-SLCT-FLD-2.
02280      MOVE HD-1-RPT-CODE      TO P-DATA.                                     
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02575                                                                   
02576      MOVE ' '                    TO X.                            
02577      MOVE HD-2                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02579                                                                   
02580      MOVE ' '                    TO X.                            
02581      MOVE HD-3                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02583                                                                   
122804     MOVE '0'                    TO X.                            
02585      MOVE S-EPX-A-CARR           TO HD-CARRIER, CARRIER-L.        
02586      PERFORM 1100-CARRIER-PRT THRU 1190-EXIT.                     
02587      MOVE CARRIER-PIC (CLAS-INDEXCN) TO HD-CARR-NAME.             
02588      MOVE 'TOTAL FOR '           TO HD7-TOTAL-FOR.                
02589      MOVE HD-7                   TO W-WORK-LINE                   
02590      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02591      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02593                                                                   
122804     MOVE '0'                    TO X.                            
02599      MOVE HD-TMS-9           TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02603                                                                   
02604      MOVE ' '                    TO X.                            
02607      MOVE HD-TMS-10          TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02611  0970-EXIT.
           EXIT.                                                       
02612  EJECT                                                            
02613                                                                   
02614  0980-GR-HD.                                                      
02615      ADD +1 TO PAGE-CNT.                                          
02616      MOVE PAGE-CNT               TO HD-PAGE.                      
02617      MOVE '1'                    TO X.                            
02618      MOVE HD-1-GRAND-TOTALS      TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02620      MOVE ' '                    TO X.                            
02621      MOVE HD-2                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02623      MOVE ' '                    TO X.                            
02624      MOVE HD-3                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
122804     MOVE '0'                    TO X.                            
02627      MOVE HD-8                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
122804     MOVE '0'                    TO X.                            
02635      MOVE HD-TMS-9           TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02639      MOVE ' '                    TO X.                            
02642      MOVE HD-TMS-10          TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02646  0980-EXIT.
           EXIT.                                                      
02647                                  EJECT                            
02514  0990-RP-HD.                                                      
02515                                                                   
02516      ADD +1 TO PAGE-CNT.                                          
02517      MOVE PAGE-CNT               TO HD-PAGE.                      
02518      MOVE '1'                    TO X.                            

101920     perform 0995-set-head       thru 0995-exit

112503     MOVE S-EPX-A-RPT-CD-1   TO HD-1-SLCT-FLD-2.
02280      MOVE HD-1-RPT-CODE      TO P-DATA.                                     
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02522      MOVE ' '                    TO X.                            
02523      MOVE HD-2                   TO P-DATA.                       
02524                                                                   
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02526      MOVE ' '                    TO X.                            
02527      MOVE HD-3                   TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT                  
02529                                                                   
122804     MOVE '0'                    TO X.                            

101920     evaluate pass-number
101920        when '4'
101920           MOVE 'RPT CODE 1 '    TO HD4-TOTAL-RPT               
101920        when '5'
101920           MOVE 'RPT CODE 2 '    TO HD4-TOTAL-RPT               
101920        when '6'
101920           MOVE 'USER SEL 2'     TO HD4-TOTAL-RPT
101920        when '8'
101920           MOVE 'RPT CODE 3 '    TO HD4-TOTAL-RPT               
101920     end-evaluate

112503     MOVE S-EPX-A-RPT-CD-1        TO HD4-RPT-CD
02533      MOVE HD-4-RPT                TO W-WORK-LINE                   
02534      PERFORM 0890-REMOVE-SPACES THRU 0890-EXIT                    
02535      MOVE W-WORK-LINE            TO P-DATA.                       
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                  
02537                                                                   
122804     MOVE '0'                    TO X.                            
02553      MOVE HD-TMS-9           TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.
02557                                                                   
02558      MOVE ' '                    TO X.                            
02561      MOVE HD-TMS-10          TO P-DATA.
02237      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                  
02565  0990-EXIT.
           EXIT.                                                       

101920 0995-set-head.
101920
101920     evaluate true
101920        when pass-number = '4'
101920           MOVE CLAS-REPORT-CD1-CAPTION
101920                                 TO HD-1-RPT-CD
101920           MOVE 'RPT CODE 1 '    TO HD-1-SLCT-FLD-1  
101920           MOVE 'D'              TO HD-1-RPT-SUF 
101920                                    HD-1-GT-SUF            
101920        when pass-number = '5'
101920           MOVE CLAS-REPORT-CD2-CAPTION
101920                                 TO HD-1-RPT-CD
101920           MOVE 'RPT CODE 2 '    TO HD-1-SLCT-FLD-1
101920           MOVE 'E'              TO HD-1-RPT-SUF             
101920                                     HD-1-GT-SUF            
101920        when pass-number = '6'
101920           MOVE 'USER SEL 2'     TO HD-1-RPT-CD
101920                                    HD-1-SLCT-FLD-1
101920           MOVE 'F'              TO HD-1-RPT-SUF             
101920                                    HD-1-GT-SUF            
101920        when pass-number = '8'
101920           MOVE 'RPT CODE 3'     TO HD-1-RPT-CD
101920                                    HD-1-SLCT-FLD-1
101920           MOVE 'H'              TO HD-1-RPT-SUF             
101920                                    HD-1-GT-SUF
101920     end-evaluate
101920
101920     .
101920 0995-exit.
101920     exit.

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

02553                                                                   
02554  EJECT                                                            
02555  1100-WRITE-PRINT.                                                
CIDMOD               COPY ELCPRT2.                                      
CIDMOD*               COPY PRTN036.
02557  1199-EXIT.                                                       
02558      EXIT.                                                        
02559  EJECT                                                            
02560  1200-LOAD-ALPHA-MONTH.                                           
02605      IF DET-TMS-MO = '01 ' MOVE 'JAN' TO DET-TMS-MO.                      
02606      IF DET-TMS-MO = '02 ' MOVE 'FEB' TO DET-TMS-MO.                      
02607      IF DET-TMS-MO = '03 ' MOVE 'MAR' TO DET-TMS-MO.                      
02608      IF DET-TMS-MO = '04 ' MOVE 'APR' TO DET-TMS-MO.                      
02609      IF DET-TMS-MO = '05 ' MOVE 'MAY' TO DET-TMS-MO.                      
02610      IF DET-TMS-MO = '06 ' MOVE 'JUN' TO DET-TMS-MO.                      
02611      IF DET-TMS-MO = '07 ' MOVE 'JUL' TO DET-TMS-MO.                      
02612      IF DET-TMS-MO = '08 ' MOVE 'AUG' TO DET-TMS-MO.                      
02613      IF DET-TMS-MO = '09 ' MOVE 'SEP' TO DET-TMS-MO.                      
02614      IF DET-TMS-MO = '10 ' MOVE 'OCT' TO DET-TMS-MO.                      
02615      IF DET-TMS-MO = '11 ' MOVE 'NOV' TO DET-TMS-MO.                      
02616      IF DET-TMS-MO = '12 ' MOVE 'DEC' TO DET-TMS-MO.                      
02617                                                                   
02618  1299-EXIT.                                                       
02619       EXIT.                                                       
02620  EJECT                                                            
02621  1300-PRINT-GRAND-TOTALS.                                         
02622      IF ZERO-GRAND-TOTALS = GRAND-TOTALS                          
02623          GO TO 1399-EXIT.                                         
02624                                                                   
02625      PERFORM 0980-GR-HD          THRU 0980-EXIT.                                          
02626      MOVE +1                     TO X1.                           
02627                                                                   
02695  1330-GRAND-TOTAL-1.                                              
02696      IF X1 GREATER 12                                             
02697          GO TO 1335-PRINT-LAST-GT-12.                                
02698                                                                   
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE GT-CERT (X1)           TO DET-TMS-CERTS
           MOVE GT-CANCEL (X1)         TO DET-TMS-CANCELS
           MOVE GT-LCOVERAGE (X1)      TO DET-TMS-LCOVERAGE
           MOVE GT-ACOVERAGE (X1)      TO DET-TMS-ACOVERAGE
02699      MOVE GT-LCAN (X1)           TO DET-TMS-LCAN.                     
02700      MOVE GT-LPRM (X1)           TO DET-TMS-LPRM.                     
02701      MOVE GT-LCLM (X1)           TO DET-TMS-LCLM.                     
02702      MOVE GT-ACAN (X1)           TO DET-TMS-ACAN.                     
02703      MOVE GT-APRM (X1)           TO DET-TMS-APRM.                     
02704      MOVE GT-ACLM (X1)           TO DET-TMS-ACLM.                     
02705      MOVE GT-TPRM (X1)           TO DET-TMS-TPRM.                     
02706      MOVE GT-TCOM (X1)           TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (GT-TPRM (X1) - GT-TCOM (X1)).    
02707      ADD +1 TO X1.                                                
02708      MOVE COMP-YR (X1)           TO DET-TMS-YR.                       
02709      MOVE COMP-MO (X1)           TO DET-TMS-MO.                       
02710      PERFORM 1200-LOAD-ALPHA-MONTH THRU 1299-EXIT.                
02055      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055      MOVE DETAIL-TMS-LINE-3       TO P-DATA.                    
02060      MOVE ' '                    TO X.                            
02062      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02719      GO TO 1330-GRAND-TOTAL-1.                                    

01729  1335-PRINT-LAST-GT-12.                                           
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE L12GT-CERT             TO DET-TMS-CERTS
           MOVE L12GT-CANCEL           TO DET-TMS-CANCELS
           MOVE L12GT-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE L12GT-ACOVERAGE        TO DET-TMS-ACOVERAGE
01730      MOVE L12GT-LCAN             TO DET-TMS-LCAN.                     
01731      MOVE L12GT-LPRM             TO DET-TMS-LPRM.                     
01732      MOVE L12GT-LCLM             TO DET-TMS-LCLM.                     
01733      MOVE L12GT-ACAN             TO DET-TMS-ACAN.                     
01734      MOVE L12GT-APRM             TO DET-TMS-APRM.                     
01735      MOVE L12GT-ACLM             TO DET-TMS-ACLM.                     
01736      MOVE L12GT-TPRM             TO DET-TMS-TPRM.                     
01737      MOVE L12GT-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (L12GT-TPRM - L12GT-TCOM).    
01738      MOVE '*12 MO'               TO DET-TMS-DATE.                    
01739                                                                   
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01746      MOVE ZERO-ACCUM             TO LAST-12-GRAND-ACCUM.       
01747                                                                   
01748  1336-PRINT-GT-YTD.                                               
03448      MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
           MOVE YTDGT-CERT             TO DET-TMS-CERTS
           MOVE YTDGT-CANCEL           TO DET-TMS-CANCELS
           MOVE YTDGT-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE YTDGT-ACOVERAGE        TO DET-TMS-ACOVERAGE
01749      MOVE YTDGT-LCAN             TO DET-TMS-LCAN.                     
01750      MOVE YTDGT-LPRM             TO DET-TMS-LPRM.                     
01751      MOVE YTDGT-LCLM             TO DET-TMS-LCLM.                     
01752      MOVE YTDGT-ACAN             TO DET-TMS-ACAN.                     
01753      MOVE YTDGT-APRM             TO DET-TMS-APRM.                     
01754      MOVE YTDGT-ACLM             TO DET-TMS-ACLM.                     
01755      MOVE YTDGT-TPRM             TO DET-TMS-TPRM.                     
01756      MOVE YTDGT-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (YTDGT-TPRM - YTDGT-TCOM).    
01757      MOVE '*YTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01762      MOVE ZERO-ACCUM             TO YTD-GRAND-ACCUM.           
01763                                                                   
03448      MOVE LIFE-OVERRIDE-L2        TO DET-TMS-LDESC.                
03449      MOVE AH-OVERRIDE-L2          TO DET-TMS-ADESC.                
           MOVE PYTDGT-CERT             TO DET-TMS-CERTS
           MOVE PYTDGT-CANCEL           TO DET-TMS-CANCELS
           MOVE PYTDGT-LCOVERAGE        TO DET-TMS-LCOVERAGE
           MOVE PYTDGT-ACOVERAGE        TO DET-TMS-ACOVERAGE
122804     MOVE PYTDGT-LCAN             TO DET-TMS-LCAN.                     
122804     MOVE PYTDGT-LPRM             TO DET-TMS-LPRM.                     
122804     MOVE PYTDGT-LCLM             TO DET-TMS-LCLM.                     
122804     MOVE PYTDGT-ACAN             TO DET-TMS-ACAN.                     
122804     MOVE PYTDGT-APRM             TO DET-TMS-APRM.                     
122804     MOVE PYTDGT-ACLM             TO DET-TMS-ACLM.                     
122804     MOVE PYTDGT-TPRM             TO DET-TMS-TPRM.                     
122804     MOVE PYTDGT-TCOM             TO DET-TMS-TCOM.                     
03462      COMPUTE DET-TMS-NPRM = (PYTDGT-TPRM - PYTDGT-TCOM).    
122804     MOVE '*PYTD'                 TO DET-TMS-DATE.                    
01177      MOVE DETAIL-TMS-LINE-1      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-2      TO P-DATA.                       
01182      MOVE ' '                    TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
01177      MOVE DETAIL-TMS-LINE-3     TO P-DATA.                       
01182      MOVE ' '                   TO X.                            
01184      PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     

01762      MOVE ZERO-ACCUM             TO PYTD-GRAND-ACCUM

122804     MOVE SPACES                 TO DET-TMS-TITLE.                    
02720                                                                   
02721 * 1340-GRAND-TOTAL-2.                                              
02722 *     MOVE 'GRAND TOTALS'         TO DET-TMS-TITLE.                    
03448 *     MOVE LIFE-OVERRIDE-L2       TO DET-TMS-LDESC.                
03449 *     MOVE AH-OVERRIDE-L2         TO DET-TMS-ADESC.                
      *     MOVE GR-CERT                TO DET-TMS-CERTS
      *     MOVE GR-CANCEL              TO DET-TMS-CANCELS
      *     MOVE GR-LCOVERAGE           TO DET-TMS-LCOVERAGE
      *     MOVE GR-ACOVERAGE           TO DET-TMS-ACOVERAGE
02724 *     MOVE GR-LCAN                TO DET-TMS-LCAN.                     
02725 *     MOVE GR-LPRM                TO DET-TMS-LPRM.                     
02726 *     MOVE GR-LCLM                TO DET-TMS-LCLM.                     
02727 *     MOVE GR-ACAN                TO DET-TMS-ACAN.                     
02728 *     MOVE GR-APRM                TO DET-TMS-APRM.                     
02729 *     MOVE GR-ACLM                TO DET-TMS-ACLM.                     
02730 *     MOVE GR-TPRM                TO DET-TMS-TPRM.                     
02731 *     MOVE GR-TCOM                TO DET-TMS-TCOM.                     
03462 *     COMPUTE DET-TMS-NPRM = (GR-TPRM - GR-TCOM).    
02055 *     MOVE DETAIL-TMS-LINE-1      TO P-DATA.                    
02060 *     MOVE ' '                    TO X.                            
02062 *     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055 *     MOVE DETAIL-TMS-LINE-2      TO P-DATA.                    
02060 *     MOVE ' '                    TO X.                            
02062 *     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02055 *     MOVE DETAIL-TMS-LINE-3       TO P-DATA.                    
02060 *     MOVE ' '                    TO X.                            
02062 *     PERFORM 1100-WRITE-PRINT THRU 1199-EXIT.                     
02734      MOVE ZERO-GRAND-TOTALS      TO GRAND-TOTALS.                 
02735                                                                   
02736  1399-EXIT.                                                       
02737      EXIT.                                                        
02738  EJECT                                                            
02743  EJECT                                                            
02744  1500-PASS-NUMBER-CHANGE.                                         
02745      MOVE ZERO                   TO SET-CTR.                      
02746                                                                   
02752      PERFORM 0700-REPTCD-BREAK  THRU 0790-ZERO-REPTCD     
02780      PERFORM 1300-PRINT-GRAND-TOTALS THRU 1399-EXIT.              
02781                                                                   
02782      IF EOF-EPX = '1'                                             
02783          GO TO 9000-END-OF-JOB.                                   
02784                                                                   
02787                                                                   
02788      MOVE ZERO                   TO PAGE-CNT                      
02789                                     X1.                           
02790      PERFORM 0220-ZERO-PRINT-TABLE 12 TIMES.                      
02791      MOVE +0                     TO X1.                           
02792      PERFORM 0230-ZERO-ACCUM-ACC 15 TIMES.                        
02793      MOVE +0                     TO X1.                           
02794      PERFORM 0240-ZERO-ACCUM-GRP 15 TIMES.                        
02795      MOVE +0                     TO X1.                           
02796      PERFORM 0250-ZERO-ACCUM-CARR 15 TIMES.                       
02797      MOVE +0                     TO X1.                           
02798      PERFORM 0255-ZERO-ACCUM-STATE 15 TIMES.                      
02799      MOVE +0                     TO X1.                           
02800      PERFORM 0260-ZERO-ACCUM-REPT 15 TIMES.                       
02801      MOVE +0                     TO X1.                           
02802      PERFORM 0270-ZERO-ACCUM-GRAND 15 TIMES.                      
02803      MOVE ZERO-GRAND-TOTALS      TO GRAND-TOTALS.                 
02804      MOVE ZERO-ACCUM TO LAST-12-ACCOUNT-ACCUM YTD-ACCOUNT-ACCUM   
02805                     LAST-12-GROUPING-ACCUM  LAST-12-CARRIER-ACCUM 
02806                     LAST-12-REPTCD-ACCUM    LAST-12-STATE-ACCUM   
02807                     YTD-GROUPING-ACCUM      YTD-CARRIER-ACCUM     
02808                     YTD-REPTCD-ACCUM        YTD-STATE-ACCUM
02807                     PYTD-GROUPING-ACCUM     PYTD-CARRIER-ACCUM     
02808                     PYTD-REPTCD-ACCUM       PYTD-STATE-ACCUM
                          LAST-12-GRAND-ACCUM     YTD-GRAND-ACCUM
                          PYTD-GRAND-ACCUM
02809      MOVE ZERO                   TO CERT                          
02810                                     LCAN                          
02811                                     LPRM                          
02812                                     LCLM                          
02813                                     ACAN                          
02814                                     APRM                          
02815                                     ACLM                          
02816                                     TPRM                          
02817                                     TCOM                          
02818                                     CANCELS.                          
02819                                                                   
02820      MOVE SPACE                  TO DET-TMS-TITLE.                    
02821                                                                   
02822      MOVE EP-PASS-NO             TO PASS-NUMBER.                  
02823                                                                   
02824      GO TO 0340-ACCUMULATE.                                       
02825                                                                   
02844  EJECT                                                            
02845  COPY ELCDCS.                                                     
02846  9000-END-OF-JOB.                                                 
02847                              COPY ELCPRTC.                        
02848      CLOSE PRNT.                                                  
02849                                                                   
02850  9099-EXIT.                                                       
02851      EXIT.                                                        
02852                                                                   
02853  ABEND-PGM SECTION.              COPY ELCABEND.                   
02854                                                                   
