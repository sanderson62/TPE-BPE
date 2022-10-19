00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 ECS080.                             
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 10/14/94 15:05:41.                 
00007 *              PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE          
00008 *                            VMOD=2.042.                          
00009 *AUTHOR.     LOGIC, INC.                                          
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
00020 *            *   THE PRIOR WRITTEN PERMISSION OF LOGIC INC.      *
00021 *            *                                                   *
00022 *            *****************************************************
00023                                                                   
00024 *REMARKS.                                                         
00025 *        CALCULATES MORTALITY RESERVES.                           
00026                                                                   
00027 *        PROGRAM OPTION SWITCHES                                  
00028 *          PROCESS = 1   -   NORMAL RUN.                          
00029 *          PROCESS = 2   -   FEDERAL INCOME TAX RUN - USE C.E.T   
00030 *                            TABLE ONLY.                          
00031 *          PROCESS = 3   -   FEDERAL INCOME TAX RUN - USE THE     
00032 *                            LESSOR OF C.E.T. RESERVE OR THE      
00033 *                            STATUTORY RESERVE (ASSUMED TO HAVE   
00034 *                            BEEN ALREADY COMPUTED AND IN THE     
00035 *                            RESERVE FIELDS).                     
092602******************************************************************
092602*                   C H A N G E   L O G
092602*
092602* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
092602*-----------------------------------------------------------------
092602*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
092602* EFFECTIVE    NUMBER
092602*-----------------------------------------------------------------
092602* 092602    2002091900008  PEMA  INCREASE NUMBER OF MAXIMUM
092602*                                  BENEFIT CODES
062104* 062104    2004050700001  SMVA  ADD NEW FILE TO AUTOMATE ME BALANCING
011407* 011407                   PEMA  CHANGE 2006 INT RT TO 4.5%
060607* 060607    2007010300001  PEMA  CHANGE 2007 TO L270 3.97%
091307* 091307    2007031200001  PEMA  ADD ADJ FACTOR BY STATE
123109* 123109    2009010500005  PEMA  CHANGE WY LOAD TO 1.80
121610* 121610    2010120900001  PEMA  ADD NEW FIT RESERVE PROCESS
122811* 122811    2011122700003  PEMA  ADD LOAD FOR L680 - CT
092712* 092712    2011080300004  PEMA  ADD FIT TBL FOR 2011 and 2012 ISSUES
111412* 111412    2012092600003  PEMA  CHANGE LOAD FACTORS ON MORT RESVS
112812* 112812    2012112800001  PEMA  REMOVE CODE THAT BYPASSES ADJ.
013013* 013013  CR2012072400002  PEMA  ADD 2013 TABLES
061213* 061213  IR2013061200001  PEMA  CHANGE CT LOAD FOR L680 & D680
111913* 111913  CR2008042200001  PEMA  ADDITIONAL 0 % APR CHANGES
031714* 031714  CR2014011500001  PEMA  ADD 2014 FIT TABLE
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
112414* 112414  CR2014112100004  PEMA  CHG MORT LD ON STATES
033015* 033015  CR2015021100001  PEMA  ADD 2015 FIT TABLE C160
070714* 070714  CR2013060600001  PEMA  AUTOMATE MONTH END BALANCING
113015* 113015  CR2015113000001  PEMA  CHANGE WY LOAD FACTOR
032216* 032216  CR2016021000001  PEMA  ADD 2016 FIT TABLE C150
111416* 111416  CR2016111000001  PEMA  CHANGE MP.SC.SD,WY LD FACTORS
020717* 020717  CR2017020600001  PEMA  ADD 2017 FIT TABLE C140
113017* 113017  CR2017111500002  PEMA  CHG MP&NE LOAD FACTORS
021418* 021418  CR2018013000002  PEMA  ADD 2018 FIT TABLE C130
112818* 112818  CR2018112800003  PEMA  Change WY load factor
102919* 102919  CR2019102900001  PEMA  Change LA load factor
082420* 082420  CR2020082400001  PEMA  Change MP, AL & MS load factors
100220* 100220  CR2020092400001  PEMA  Add calc of est mort resv for CID
121120* 121120  CR2020121000002  PEMA  Change MS load factor
111721* 111721  CR2021110800001  PEMA  UPD mort tbl int rate and fit tbls
112221* 112221  CR2021112200003  PEMA  Upd load factors MS,NE,SD
092602******************************************************************
00036  ENVIRONMENT DIVISION.                                            
00037  CONFIGURATION SECTION.                                           
00038  INPUT-OUTPUT SECTION.                                            
00039  FILE-CONTROL.                                                    
00040                                                                   
00041      SELECT PRNTR            ASSIGN TO SYS008-UR-1403-S-SYS008.   
00042                                                                   
00043      SELECT TAPE-FACTORS     ASSIGN TO SYS010-UT-2400-S-SYS010.   
00044                                                                   
00045      SELECT GAAP-EXTRACT     ASSIGN TO SYS012-UT-2400-S-SYS012.   
00046
00047      SELECT GAAP-NEW         ASSIGN TO SYS013-UT-2400-S-SYS013.   
00048                                                                   
00049      SELECT GAAP-LAST-NET-PAY                                     
00050                              ASSIGN TO SYS016-UT-2400-S-SYS016.   
00051                                                                   
00052      SELECT GAAP-NEW-NET-PAY ASSIGN TO SYS017-UT-2400-S-SYS017.   
00053                                                                   
00054      SELECT REIN-TBL-FILE    ASSIGN TO SYS014-3380-ERRTBLT
00055                              ACCESS IS SEQUENTIAL                 
00056                              ORGANIZATION IS INDEXED              
00057                              FILE STATUS IS REIN-FILE-STATUS      
00058                              RECORD KEY IS RE-CONTROL-PRIMARY.    
00059                                                                   
00060      SELECT ACC-MSTR         ASSIGN TO SYS015-3380-ERACCTT        
00061                              ACCESS IS SEQUENTIAL                 
00062                              ORGANIZATION IS INDEXED              
00063                              FILE STATUS IS AM-FILE-STATUS        
00064                              RECORD KEY IS AM-CONTROL-PRIMARY.    

062104     SELECT ME-ECS080-BALANCE
062104                             ASSIGN TO SYS011
062104                             ORGANIZATION IS LINE SEQUENTIAL.
00065                                                                   
00066      SELECT DISK-DATE        ASSIGN TO SYS019-UT-3380-S-SYS019.   
00067                                                                   
00068      SELECT FICH             ASSIGN TO SYS020-UT-2400-S-SYS020.   
00069                                                                   
00070      SELECT ERMEBL           ASSIGN TO SYS024-3380-ERMEBL         
00071                              ORGANIZATION INDEXED                 
00072                              ACCESS DYNAMIC                       
00073                              RECORD KEY ME-CONTROL-PRIMARY        
00074                              FILE STATUS ERMEBL-FILE-STATUS.      
CIDMOD     SELECT  DISPLAY-PRT     ASSIGN TO SYS022-UR-1403-S-SYS022.   
CIDMOD                                                                  
00075  EJECT                                                            
00076  DATA DIVISION.                                                   
00077  FILE SECTION.                                                    
00078                                                                   
00079  FD  PRNTR                                                        
00080                              COPY ELCPRTFD.                       
00081                                                                   
00082  FD  TAPE-FACTORS                                                 
00083      BLOCK CONTAINS 0 RECORDS
00084      RECORDING MODE F.                                            
00085                                                                   
00086  01  RF-RECORD.                                                   
00087      12  FILLER              PIC XX.                              
00088      12  RF-IN-CTL           PIC X(4).                            
00089      12  FILLER              PIC X(1234).                         
00090  EJECT                                                            
00091  FD  GAAP-EXTRACT                                                 
00092                              COPY ECSGAPFD.                       
00093                              COPY ECSGAP01.
PEMTST*                            copy ECSGAP01.PEMA.
00094                                                                   
00095  EJECT                                                            
00096  FD  GAAP-NEW                                                     
00097      BLOCK CONTAINS 0 RECORDS
00098      RECORDING MODE F.                                            
00099                                                                   
00100  01  GN-REC                  PIC X(365).                          
00101  EJECT                                                            
00102  FD  GAAP-LAST-NET-PAY                                            
00103      BLOCK CONTAINS 0 RECORDS
00104      RECORDING MODE F.                                            
00105                                                                   
00106      COPY ECSGAPNP.                                               
00107                                                                   
00108  FD  GAAP-NEW-NET-PAY                                             
00109      BLOCK CONTAINS 0 RECORDS
00110      RECORDING MODE F.                                            
00111                                                                   
00112  01  GNP-NEW-REC                 PIC X(2600).                     
00113                                                                   
00114  EJECT                                                            
00115  FD  REIN-TBL-FILE                                                
00116                              COPY ECSRTFDD.                       
00117                                                                   
00118      COPY ERCREIN.                                                
00119  EJECT                                                            
00120  FD  ACC-MSTR.                                                    
00121                                                                   
00122      COPY ERCACCT.                                                
00123  EJECT                                                            

062104 FD  ME-ECS080-BALANCE
062104     RECORDING MODE IS F
062104     BLOCK CONTAINS 0 RECORDS.
062104 01  ME-ECS080-BALANCE-REC    PIC X(83).

00124  FD  DISK-DATE                                                    
00125                              COPY ELCDTEFD.                       
00126  EJECT                                                            
00127  FD  FICH                                                         
00128      BLOCK CONTAINS 0 RECORDS
00129      RECORDING MODE F.                                            
00130                                                                   
00131  01  FICH-REC                PIC  X(133).                         
00132  EJECT                                                            
00133  FD  ERMEBL.                                                      
00134                                                                   
00135      COPY ERCMEBL.                                                
CIDMOD
CIDMOD FD  DISPLAY-PRT                                                  
CIDMOD     RECORDING MODE F                                             
CIDMOD     LABEL RECORDS ARE STANDARD                                   
CIDMOD     RECORD CONTAINS 133 CHARACTERS                               
CIDMOD     BLOCK CONTAINS 0 RECORDS
CIDMOD     DATA RECORD IS DISPLAY-REC.                                  
CIDMOD                                                                  
CIDMOD 01  DISPLAY-REC.                                                 
CIDMOD     12  DISPLAY-CC              PIC X.                           
CIDMOD     12  DISPLAY-INFO            PIC X(132).                      
CIDMOD                                                                  
00136  EJECT                                                            
00137  WORKING-STORAGE SECTION.                                         
00138  77  FILLER  PIC X(32) VALUE '********************************'.  
00139  77  FILLER  PIC X(32) VALUE '     ECS080 WORKING STORAGE     '.  
00140  77  FILLER  PIC X(32) VALUE '******** VMOD=2.042 ************'.  
00141                                                                   
CIDMOD 77  DIS-HEAD-SW             PIC X       VALUE 'Y'.               
CIDMOD 77  ERROR-COUNT             PIC 9(7)    VALUE ZEROS.             
CIDMOD 77  DIS-LINE-CNT            PIC 99      VALUE ZEROS.             
070908 77  WS-RESV-ADJ                 PIC S9V9(4) COMP-3 VALUE +0.
121610 77  WS-GRS-LFTAX                PIC S9(7)V99  COMP-3 VALUE +0.
121610 77  WS-NET-UEP-RESERVE          PIC S9(9)V99  COMP-3 VALUE +0.
CIDMOD                                                                  
CIDMOD 01  DISPLAY-HD-1.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(23) VALUE 'PROCESSING ERROR REPORT'.   
CIDMOD     12  FILLER      PIC X(51) VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(07) VALUE 'ECS080'.                    
CIDMOD                                                                  
CIDMOD 01  DISPLAY-HD-2.                                                
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X     VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(56) VALUE SPACES.                      
CIDMOD     12  DIS-DATE    PIC X(8)  VALUE SPACES.                      
CIDMOD     12  FILLER      PIC X(67) VALUE SPACES.                      
CIDMOD                                                                  
CIDMOD 01  DISPLAY-LINE.                                                
CIDMOD     05  DISPLAY-LINE-05.                                         
CIDMOD        10  DIS-CC              PIC X.                            
CIDMOD        10  DIS-LINE-REASON     PIC X(32).                        
CIDMOD        10  DIS-LINE-REC        PIC X(100).                       
CIDMOD                                                                  
CIDMOD     05  DISPLAY-LINE-ALT  REDEFINES  DISPLAY-LINE-05.            
CIDMOD        10  DIS-CC-ALT          PIC X.                            
CIDMOD        10  DIS-FLD-1           PIC X(52).                        
CIDMOD        10  DIS-FLD-2           PIC X(80).                        
00186                                                                   
00142  01  W-PROGRAM-WORK-AREA.                                         
00143      12  FILLER                  PIC  X(18)                       
00144                                       VALUE 'PROGRAM WORK AREA:'. 
00145      12  W-NDXF                  PIC S9(04) COMP.                 
00146      12  W-NDXV                  PIC S9(04) COMP.                 
00147      12  W-NDX1                  PIC S9(04) COMP.                 
00148      12  W-NDX2                  PIC S9(04) COMP.                 
00149      12  W-NUMBER-NET-PAY        PIC S9(08) COMP VALUE +0.        
00150      12  W-APR-LESS-THAN-1       PIC S9(08) COMP VALUE +0.        
00151                                                                   
00152      12  W-A                     PIC S9(05)V9(12) COMP-3.         
00153      12  W-B                     PIC S9(10)V9(07) COMP-3.         
00154      12  W-C                     PIC S9(05)V9(12) COMP-3.         
00155      12  W-EXP-V                 PIC S9(07)V9(10) COMP-3.         
00156      12  W-G                     PIC S9(03)       COMP-3.         
00157      12  W-M                     PIC S9(07)V9(10) COMP-3.         
00158      12  W-ORIGINAL-TERM         PIC S9(07)V9(10) COMP-3.         
00159      12  W-ORIGINAL-TERM-INT     PIC S9(03)       COMP-3.         
00160      12  W-P                     PIC S9(03)       COMP-3.         
00161      12  W-PVFB                  PIC S9(05)V9(12) COMP-3.         
00162      12  W-PVFB-ALTERNATE        PIC S9(05)V9(12) COMP-3.         
00163      12  W-Q                     PIC S9(03)       COMP-3.         
00164      12  W-R                     PIC S9(07)V9(10) COMP-3.         
00165      12  W-TOTAL-RECORDS         PIC S9(11)       COMP-3 VALUE +0.
00166      12  W-Z                     PIC S9(03)       COMP-3.         
00167      12  W-Y                     PIC S9(03)       COMP-3.         
00168      12  W-V                     PIC S9(07)V9(10) COMP-3.         
00169      12  W-VACCUM                PIC S9(07)V9(10) COMP-3.         
00170      12  W-WORK-AREA             PIC S9(10)V9(07) COMP-3.         
00171      12  W-WORK-AREA-2           PIC S9(10)V9(07) COMP-3.         
00172      12  W-WORK-AREA-3           PIC S9(10)V9(07) COMP-3.         
00173      12  W-WORK-AREA-4           PIC S9(10)V9(07) COMP-3.         
00174      12  W-WORK-AREA-5           PIC S9(03)V9(14) COMP-3.         
00175      12  W-WORK-AREA-6           PIC S9(03)V9(14) COMP-3.         
00176      12  W-WORK-AREA-7           PIC S9(06)V9(11) COMP-3.         
00177      12  W-WORK-AREA-8           PIC S9(12)V9(05) COMP-3.         
00178      12  W-WORK-AREA-9           PIC S9(12)V9(05) COMP-3.         
00179                                                                   
00180      12  W-EDIT-7                PIC  -------9.9(10).             
00181      12  W-EDIT                  PIC 999.                         
00182      12  W-EDIT-3                PIC ---9.99999999999999.         
00183      12  W-EDIT-3-4              PIC ---9.9999.                   
00184      12  W-EDIT-5                PIC -----9.999999999999.         
00185      12  W-EDIT-6                PIC ------9.99999999999.         
00186      12  W-EDIT-10               PIC ----------9.9999999.         
00187      12  W-EDIT-12               PIC ------------9.99999.         
00188      12  W-EOF-NET-PAY-SW        PIC  X(01) VALUE SPACES.         
00189          88  W-EOF-NET-PAY-REACHED    VALUE 'Y'.                  
00190                                                                   
00191                                                                   
00192      12  W-NET-PAY-MATCH-SW      PIC  X(01) VALUE SPACES.         
00193          88  W-NET-PAY-MATCHED        VALUE 'Y'.                  
00194      12  W-RUN-DATE-BIN          PIC  X(02).                      
00195                                                                   
00196  01  W-GNP-TABLE.                                                 
00197      12  W-GNPT-KEY-DATA.                                         
00198          16  W-GNPT-RECORD-ID              PIC  X(02).            
00199          16  W-GNPT-COMPANY-CD             PIC  X(01).            
00200          16  W-GNPT-CONTROL-PRIMARY.                              
00201              20  W-GNPT-CONTROL.                                  
00202                  24  W-GNPT-CARRIER        PIC  X(01).            
00203                  24  W-GNPT-GROUPING.                             
00204                      28  W-GNPT-GROUP-PREFIX                      
00205                                            PIC  X(03).            
00206                      28  W-GNPT-GROUP-PRIME                       
00207                                            PIC  X(03).            
00208                  24  W-GNPT-STATE          PIC  X(02).            
00209                  24  W-GNPT-ACCOUNT.                              
00210                      28  W-GNPT-ACCT-PREFIX                       
00211                                            PIC  X(04).            
00212                      28  W-GNPT-ACCT-PRIME PIC  X(06).            
00213                  24  W-GNPT-EFF            PIC  9(11)  COMP-3.    
00214                  24  W-GNPT-CERT-NO.                              
00215                      28  W-GNPT-CERT.                             
00216                          30  W-GNPT-CERT-PREFIX                   
00217                                            PIC  X(03).            
00218                          30  W-GNPT-CERT-PRIME                    
00219                                            PIC  X(07).            
00220                      28  W-GNPT-CERT-SUFFIX                       
00221                                            PIC  X(01).            
00222              20  W-GNPT-CHANGEABLE-DATA.                          
00223                  24  W-GNPT-ORIGINAL-TERM  PIC S9(03) COMP-3.     
00224                  24  W-GNPT-APR            PIC S9(03)V9(04)       
00225                                                       COMP-3.     
00226                  24  W-GNPT-CALC-OPTION    PIC  X(01).            
00227      12  W-GNPT-FACTOR-GRPS OCCURS 360 TIMES.                     
00228          16  W-GNPT-FACTOR                 PIC S9(04)V9(09)       
00229                                                       COMP-3.     
00230      12  FILLER                            PIC  X(34).            


062104 01  WS-BALANCE-DESCRIPTION        PIC X(50)  VALUE
062104     'Gross Reserve'.

062104 01  WS-ME-BALANCE-REC.
062104     12  WS-ME-BAL-JOB           PIC X(11)  VALUE SPACES.
062104     12  WS-ME-BAL-DELIM1        PIC X(01)  VALUE ';'.
062104     12  WS-ME-BAL-STEP          PIC X(08)  VALUE 'ECS080  '.
062104     12  WS-ME-BAL-DELIM2        PIC X(01)  VALUE ';'.
100220     12  WS-ME-BAL-AMT           PIC ZZZZZ,ZZZ,ZZ9.
062104     12  WS-ME-BAL-DELIM3        PIC X(01)  VALUE ';'.
100220     12  WS-ME-BAL-DESCRIP       PIC X(47)  VALUE SPACES.
100220     12  ws-me-bal-star          pic x      value '*'.
00231                                                                   
00232  01  MONTH-END-DATA.                                              
00233      12  ME-START-DATE.                                           
00234          16  ME-START-MO         PIC 99.                          
00235          16  FILLER              PIC X.                           
00236          16  ME-START-DA         PIC 99.                          
00237          16  FILLER              PIC X.                           
00238          16  ME-START-YR         PIC 99.                          
00239      12  ME-CNDS-DATE            PIC 9(6).                        
00240      12  ME-CNDS-DATE-R REDEFINES ME-CNDS-DATE.                   
00241          16  ME-CNDS-MO          PIC 99.                          
00242          16  ME-CNDS-DA          PIC 99.                          
00243          16  ME-CNDS-YR          PIC 99.                          
00244      12  ME-START-TIME           PIC 9(6).                        
00245      12  ME-UPDATE-FLAG          PIC X VALUE 'Y'.                 
00246          88  ME-DO-UPDATE        VALUE 'Y'.                       
00247          88  ME-NO-UPDATE        VALUE 'N'.                       
00248      12  ERMEBL-FILE-STATUS      PIC XX.                          
00249      12  MONTH-END-MOYR          PIC S9(5)  COMP-3.               
00250                                                                   
00251  01  WRK-GR-MORTALITY-DATA.                                       
00252      12  WRK-GR-MORT-CTL.                                         
00253          16  WRK-GR-MORT-CODE.                                    
00254              20  WRK-GR-MORT-TBL PIC  X.                          
00255              20  WRK-GR-MORT-INT PIC  XX.                         
00256              20  WRK-GR-MORT-TYP PIC  X.                          
00257          16  WRK-GR-MORT-AGE     PIC  99.                         
00258      12  WRK-GR-RESV             PIC S9(5)V99 COMP-3 VALUE ZEROS. 
00259      12  WRK-GR-LIFE-CODE        PIC  XX             VALUE SPACES.
00260                                                                   
00261  01  NET-PAY-INTERFACE.                                           
00262      12  NP-APR              PIC S9(3)V9(4)  COMP-3.              
00263      12  NP-ORIG             PIC S9(3)       COMP-3.              
00264      12  NP-REM              PIC S9(3)       COMP-3.              
00265      12  NP-OPT              PIC X.                               
00266      12  NP-CAP              PIC S9(3)       COMP-3.              
00267      12  NP-FACTOR           PIC S9(4)V9(9)  COMP-3.              
00268      12  NET-FACT            PIC S99V9(13)   COMP-3.              
00269      12  NP-WORK1            PIC S9(6)V9(6)  COMP-3.              
00270      12  NP-WORK2            PIC S9(6)V9(6)  COMP-3.              
00271      12  NP-LOOP-END         PIC S9(3)       COMP-3.              
00272      12  NP-AVG-INFORCE      PIC S9(11)V9(6) COMP-3.              
00273      12  NP-MORT-TOT         PIC S9(11)V9(6) COMP-3.              
00274      12  NP-INT-1            PIC S9(3)       COMP-3.              
00275      12  NP-INT-2            PIC S9(3)       COMP-3.              
00276      12  NP-INT-3            PIC S9(3)V9(9)  COMP-3.              
00277      12  NP-NC-TOTAL-BEN     PIC S9(7)V9(9)  COMP-3.              
00278                                                                   
00279  01  MISC.                                                        
CIDMOD     12  QD-CNT              PIC 9999    VALUE ZEROS.             
CIDMOD     12  QD-WRITE-CNT        PIC 9999    VALUE ZEROS.             
00280      12  SA                  PIC S999    COMP.                    
00281      12  SB                  PIC S999    COMP.                    
00282      12  SC                  PIC S999    COMP.                    
00283      12  SE                  PIC S999    COMP.                    
00284      12  SF REDEFINES SE     PIC XX.                              
00285      12  SG                  PIC S999    COMP.                    
00286      12  X1                  PIC S999    COMP.                    
00287      12  X2                  PIC S999    COMP.                    
00288      12  X3                  PIC S999    COMP.                    
00289      12  X4                  PIC S999    COMP.                    
00290      12  CTR                 PIC S999    COMP.                    
00291      12  CTS                 PIC S999    COMP.                    
00292      12  TX                  PIC S999    COMP.                    
013013     12  F-T-LIMIT           PIC S999    COMP    VALUE +153.
00294      12  PGM-SUB             PIC S999    COMP    VALUE +080.      
00295      12  WS-RETURN-CODE      PIC S9(4)   COMP.                    
00296                                                                   
00297      12  WS-ABEND-MESSAGE    PIC X(80).                           
00298      12  FILLER REDEFINES WS-ABEND-MESSAGE.                       
00299          16  FILLER          PIC X(42).                           
00300          16  WS-MONTHS-DIFFERENCE                                 
00301                              PIC ZZ9.                             
00302          16  FILLER          PIC X(35).                           
00303                                                                   
00304      12  WS-ABEND-FILE-STATUS PIC X(2)   VALUE ZEROS.             
00305      12  WS-ZERO             PIC S9      COMP-3 VALUE +0.         
00306      12  ALT-SW              PIC X               VALUE 'N'.       
00307          88  ALT-MORT                            VALUE 'Y'.       
00308          88  REGULAR-MORT                        VALUE 'N'.       
00309      12  BALLOON-SW          PIC X               VALUE 'N'.       
00310          88  BALLOON-PASS                        VALUE 'Y'.       
00311      12  X                   PIC X.                               
00312      12  SAVE-GR-FLAG        PIC X           VALUE SPACE.         
00313      12  WS-EXPIRE-DT        PIC X(02)       VALUE LOW-VALUES.    
00314      12  WS-FACT             PIC S9(5)V9(4)  COMP-3.              
00315      12  WS-MORT-FACTOR      PIC S9(5)V9(4)  COMP-3.              
00316      12  WS-MORT-FACTOR-ALT  PIC S9(5)V9(6)  COMP-3.              
00317      12  WS-LIFE-LIMIT       PIC S9(9)V99    COMP-3.              
00318      12  SAVE-GR-REM-AMT     PIC S9(9)V99    COMP-3.              
00319      12  WS-ALT-BENEFIT-AMT  PIC S9(9)V99    COMP-3.              
00320      12  TEX-FACT-9          PIC S9(4)V9(11) COMP-3.              
100220     12  tot-inforce         pic s9(11)v99   comp-3  value zero.
00321      12  TOT-GROSS           PIC S9(11)V99   COMP-3  VALUE ZERO.  
00322      12  TOT-REIN            PIC S9(11)V99   COMP-3  VALUE ZERO.  
00323      12  ALT-TOT-GROSS       PIC S9(11)V99   COMP-3  VALUE ZERO.  
00324      12  ALT-TOT-REIN        PIC S9(11)V99   COMP-3  VALUE ZERO.  
00325      12  ERR-COUNT           PIC S9(11)      COMP-3  VALUE ZERO.  
00326      12  ALT-ERR-COUNT       PIC S9(11)      COMP-3  VALUE ZERO.  
00327      12  WS-PAGE             PIC S9(5)       COMP-3  VALUE ZERO.  
00328      12  WS-LINE             PIC S9(3)       COMP-3  VALUE +99.   
00329      12  WK-RSV              PIC S9(9)V9(6)  COMP-3  VALUE +0.    
00330      12  WK-RESV             PIC S9(13)V9(4) COMP-3  VALUE +0.    
00331      12  DISP-NUM            PIC ZZZ,ZZZ,ZZ9.99999.               
00332      12  CTA                 PIC S9(5)       COMP-3  VALUE +0.    
00333      12  CTT                 PIC S9(5)       COMP-3  VALUE +0.    
00334      12  CTX                 PIC S9(5)       COMP-3  VALUE +0.    
00335      12  CTX2                PIC S9(5)       COMP-3  VALUE +0.    
00336      12  M                   PIC S9(5)       COMP-3  VALUE +0.    
00337      12  P                   PIC S9(5)       COMP-3  VALUE +0.    
00338      12  XX                  PIC S9(5)       COMP-3  VALUE +0.    
00339      12  R                   PIC S9(5)       COMP-3  VALUE +0.    
00340      12  T                   PIC S9(5)       COMP-3  VALUE +0.    
00341      12  PERCNT-FAC          PIC S9V9(4)     COMP-3  VALUE +0.    
00342      12  RSVR                PIC S9(5)V9(4)  COMP-3  VALUE +0.    
00343      12  FACTOR-A            PIC S9(8)V9(9)  COMP-3  VALUE +0.    
00344      12  FACTOR-B            PIC S9(8)V9(9)  COMP-3  VALUE +0.    
00345      12  ANSB                PIC S9(8)V9(9)  COMP-3  VALUE +0.    
00346      12  ANSC                PIC S9(8)V9(9)  COMP-3  VALUE +0.    
00347      12  ANSD                PIC S9(8)V9(9)  COMP-3  VALUE +0.    
00348      12  V                   PIC S99V9(9)    COMP-3  VALUE +0.    
00349      12  DX                  PIC S9(9)V99    COMP-3  VALUE +0.    
00350      12  DY                  PIC S9(9)V99    COMP-3  VALUE +0.    
00351      12  MR                  PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00352      12  MY                  PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00353      12  QX                  PIC S9(9)V9(6)  COMP-3  VALUE +0.    
00354      12  WK-MY1              PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00355      12  WK-MY2              PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00356      12  DR                  PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00357      12  DX-WORK             PIC S9(9)V99    COMP-3  VALUE +0.    
00358      12  CX                  PIC S9(9)V99    COMP-3  VALUE +0.    
00359      12  CX-WORK             PIC S9(9)V99    COMP-3  VALUE +0.    
00360      12  WK-CX               PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00361      12  WK-DX               PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00362      12  WK-DX2              PIC S9(9)V9(4)  COMP-3  VALUE +0.    
00363      12  W-ZEROS             PIC S9(04)      COMP    VALUE +0.    
00364                                                                   
00365  01  ACCT-SEARCH.                                                 
00366      12  ACC-SRA.                                                 
00367          16  ACC-SR-CARR     PIC X.                               
00368          16  ACC-SR-GROUP    PIC X(6).                            
00369          16  ACC-SR-ST       PIC X(2).                            
00370          16  ACC-SR-NO       PIC X(10).                           
00371                                                                   
00372  01  REINSURANCE-SEARCH-AREA.                                     
00373      12  REIN-OPEN-SW            PIC X               VALUE SPACES.
00374      12  REIN-SEARCH-TABLE.                                       
00375          16  REIN-SEARCH     OCCURS 300 TIMES.                    
00376              20  REIN-SRCH-CMPY  PIC X(6).                        
00377              20  REIN-SRCH-TBLE  PIC X(4).                        
00378      12  REIN-INDEX              PIC S9(3) COMP-3    VALUE +0.    
00379                                                                   
00380  01  FACTOR-SEEK-WORK-AREAS.                                      
00381      12  FACTOR-KEY          PIC X(6)                VALUE SPACES.
00382      12  SAVE-FACTOR-KEY     PIC X(6)            VALUE SPACES.    
00383      12  FACTOR-ERROR        PIC X               VALUE 'N'.       
00384      12  SAVE-FACTOR-ERROR   PIC X               VALUE 'N'.       
00385                                                                   
00386  01  MORT-CODE-TABLE.                                             
013013     12  MORT-CODE-ENTRY     OCCURS 153.                          
00388          16  MORTALITY-CODE      PIC X(4).                        
00389                                                                   
00390  01  MORT-MX-TABLE.                                               
00391      12  MX-FACTORS          OCCURS 100                           
00392                                  PIC S9(9)V99  COMP-3.            
00393  01  SAVE-MX-VALUES.                                              
013013     12  MX-SAVE-ENTRY       OCCURS 153
00395                                  PIC X(600).                      
00396                                                                   
00397  01  FILLER                      PIC  X(19)                       
00398                                  VALUE 'PROGRAM DXCX TABLE-'.     
00399  01  RF-REC.                                                      
00400      12  RF-ID           PIC XX VALUE 'RF'.                       
00401      12  RF-CTL.                                                  
00402          16  RF-CODE.                                             
00403              20  RF-TBL          PIC X                VALUE SPACE.
00404              20  RF-INT          PIC XX               VALUE SPACE.
00405              20  RF-TYPE         PIC X                VALUE SPACE.
00406      12  RF-INT-ADJ              PIC S9V9(4)  COMP-3  VALUE +0.   
00407      12  RF-RESV-ADJ             PIC S9V9(4)  COMP-3  VALUE +0.   
00408      12  RF-JOINT-ADJ            PIC S9V9(4)  COMP-3  VALUE +0.   
00409      12  RF-PLUS-MINUS           PIC X.                           
00410          88  LX-PLUS                VALUE '+'.                    
00411          88  LX-MINUS               VALUE '-' ' '.                
00412      12  FILLER                  PIC X(24)            VALUE SPACE.
00413                                                                   
00414      12  CX-DX-FACTORS.                                           
00415          16  RF-CX-FACTOR       OCCURS 100 TIMES                  
00416                                  PIC S9(9)V99 COMP-3.             
00417          16  RF-DX-FACTOR       OCCURS 100 TIMES                  
00418                                  PIC S9(9)V99 COMP-3.             
00419  01  FILLER                      PIC  X(24)                       
00420                               VALUE '-PROGRAM DXCX TABLE ENDS'.   
00421                                                                   
00422  01  FACTOR-TABLE.                                                
013013     12  FACT-TBL    OCCURS 153  PIC X(1240).                     
00424                                                                   
00425                              EJECT                                
00426 ******************************************************************
00427 *                                                                *
CIDMOD* NOTE--STATUTORY RESERVING IS MODIFIED IN 'ECS050'              *
CIDMOD*       F.I.T. RESERVING IS MODIFIED IN 'ECS080'.                *
CIDMOD*                                                                *
00428 *     THIS TABLE CONTAINS THE MORTALITY TABLE POINTERS FOR USE   *
00429 *     IN F.I.T. RESERVING - POLICIES WITH TERMS 10 YEARS OR LESS *
00430 *                                                                *
CIDMOD*     THIS TABLE MUST BE UPDATED EACH YEAR TO GET ANY NEW        *
CIDMOD*     RATES FOR THE 'CILGTAX' RUN.                               *
CIDMOD*                                                                *
CIDMOD* NOTE-- 4TH DIGIT OF TABLE VALUE MUST BE ZERO.                  *
CIDMOD* ====                            ==== == ====                   *
CIDMOD*                                                                *
00431 ******************************************************************
00432                                                                   
00433  01  WS-MORT-TABLE1-BY-YEAR.                                      
00456      12  WS-YEAR-19-23       PIC X(8)        VALUE '1923L350'.    
00457      12  WS-YEAR-19-24       PIC X(8)        VALUE '1924L350'.    
00458      12  WS-YEAR-19-25       PIC X(8)        VALUE '1925L350'.    
00459      12  WS-YEAR-19-26       PIC X(8)        VALUE '1926L350'.    
00460      12  WS-YEAR-19-27       PIC X(8)        VALUE '1927L350'.    
00461      12  WS-YEAR-19-28       PIC X(8)        VALUE '1928L350'.    
00462      12  WS-YEAR-19-29       PIC X(8)        VALUE '1929L350'.    
00463      12  WS-YEAR-19-30       PIC X(8)        VALUE '1930L350'.    
00464      12  WS-YEAR-19-31       PIC X(8)        VALUE '1931L350'.    
00465      12  WS-YEAR-19-32       PIC X(8)        VALUE '1932L350'.    
00466      12  WS-YEAR-19-33       PIC X(8)        VALUE '1933L350'.    
00467      12  WS-YEAR-19-34       PIC X(8)        VALUE '1934L350'.    
00468      12  WS-YEAR-19-35       PIC X(8)        VALUE '1935L350'.    
00469      12  WS-YEAR-19-36       PIC X(8)        VALUE '1936L350'.    
00470      12  WS-YEAR-19-37       PIC X(8)        VALUE '1937L350'.    
00471      12  WS-YEAR-19-38       PIC X(8)        VALUE '1938L350'.    
00472      12  WS-YEAR-19-39       PIC X(8)        VALUE '1939L350'.    
00473      12  WS-YEAR-19-40       PIC X(8)        VALUE '1940L350'.    
00474      12  WS-YEAR-19-41       PIC X(8)        VALUE '1941L350'.    
00475      12  WS-YEAR-19-42       PIC X(8)        VALUE '1942L350'.    
00476      12  WS-YEAR-19-43       PIC X(8)        VALUE '1943L350'.    
00477      12  WS-YEAR-19-44       PIC X(8)        VALUE '1944L350'.    
00478      12  WS-YEAR-19-45       PIC X(8)        VALUE '1945L350'.    
00479      12  WS-YEAR-19-46       PIC X(8)        VALUE '1946L350'.    
00480      12  WS-YEAR-19-47       PIC X(8)        VALUE '1947L350'.    
00481      12  WS-YEAR-19-48       PIC X(8)        VALUE '1948L350'.    
00482      12  WS-YEAR-19-49       PIC X(8)        VALUE '1949L350'.    
00483      12  WS-YEAR-19-50       PIC X(8)        VALUE '1950L350'.    
00484      12  WS-YEAR-19-51       PIC X(8)        VALUE '1951L350'.    
00485      12  WS-YEAR-19-52       PIC X(8)        VALUE '1952L350'.    
00486      12  WS-YEAR-19-53       PIC X(8)        VALUE '1953L350'.    
00487      12  WS-YEAR-19-54       PIC X(8)        VALUE '1954L350'.    
00488      12  WS-YEAR-19-55       PIC X(8)        VALUE '1955L350'.    
00489      12  WS-YEAR-19-56       PIC X(8)        VALUE '1956L350'.    
00490      12  WS-YEAR-19-57       PIC X(8)        VALUE '1957L350'.    
00491      12  WS-YEAR-19-58       PIC X(8)        VALUE '1958L350'.    
00492      12  WS-YEAR-19-59       PIC X(8)        VALUE '1959L350'.    
00493      12  WS-YEAR-19-60       PIC X(8)        VALUE '1960L350'.    
00494      12  WS-YEAR-19-61       PIC X(8)        VALUE '1961L350'.    
00495      12  WS-YEAR-19-62       PIC X(8)        VALUE '1962L350'.    
00496      12  WS-YEAR-19-63       PIC X(8)        VALUE '1963L350'.    
00497      12  WS-YEAR-19-64       PIC X(8)        VALUE '1964L350'.    
00498      12  WS-YEAR-19-65       PIC X(8)        VALUE '1965L350'.    
00499      12  WS-YEAR-19-66       PIC X(8)        VALUE '1966L350'.    
00500      12  WS-YEAR-19-67       PIC X(8)        VALUE '1967L350'.    
00501      12  WS-YEAR-19-68       PIC X(8)        VALUE '1968L350'.    
00502      12  WS-YEAR-19-69       PIC X(8)        VALUE '1969L350'.    
00503      12  WS-YEAR-19-70       PIC X(8)        VALUE '1970L350'.    
00504      12  WS-YEAR-19-71       PIC X(8)        VALUE '1971L350'.    
00505      12  WS-YEAR-19-72       PIC X(8)        VALUE '1972L350'.    
00506      12  WS-YEAR-19-73       PIC X(8)        VALUE '1973L350'.    
00507      12  WS-YEAR-19-74       PIC X(8)        VALUE '1974L350'.    
00508      12  WS-YEAR-19-75       PIC X(8)        VALUE '1975L400'.    
00509      12  WS-YEAR-19-76       PIC X(8)        VALUE '1976L400'.    
00510      12  WS-YEAR-19-77       PIC X(8)        VALUE '1977L400'.    
00511      12  WS-YEAR-19-78       PIC X(8)        VALUE '1978L400'.    
00512      12  WS-YEAR-19-79       PIC X(8)        VALUE '1979L400'.    
00513      12  WS-YEAR-19-80       PIC X(8)        VALUE '1980L450'.    
00514      12  WS-YEAR-19-81       PIC X(8)        VALUE '1981L450'.    
00515      12  WS-YEAR-19-82       PIC X(8)        VALUE '1982L450'.    
00516      12  WS-YEAR-19-83       PIC X(8)        VALUE '1983L720'.    
00517      12  WS-YEAR-19-84       PIC X(8)        VALUE '1984L720'.    
00518      12  WS-YEAR-19-85       PIC X(8)        VALUE '1985L720'.    
00519      12  WS-YEAR-19-86       PIC X(8)        VALUE '1986L720'.    
00520      12  WS-YEAR-19-87       PIC X(8)        VALUE '1987L650'.    
00521      12  WS-YEAR-19-88       PIC X(8)        VALUE '1988L770'.    
CIDMOD     12  WS-YEAR-19-89       PIC X(8)        VALUE '1989L820'.    
00523      12  WS-YEAR-19-90       PIC X(8)        VALUE '1990L830'.    
00524      12  WS-YEAR-19-91       PIC X(8)        VALUE '1991L840'.    
CIDMOD     12  WS-YEAR-19-92       PIC X(8)        VALUE '1992L840'.    
CIDMOD     12  WS-YEAR-19-93       PIC X(8)        VALUE '1993L810'.    
00533      12  WS-YEAR-19-94       PIC X(8)        VALUE '1994L740'.    
CIDMOD     12  WS-YEAR-19-95       PIC X(8)        VALUE '1995L700'.    
00535      12  WS-YEAR-19-96       PIC X(8)        VALUE '1996L660'.    
CIDMOD     12  WS-YEAR-19-97       PIC X(8)        VALUE '1997L970'.    
CIDMOD     12  WS-YEAR-19-98       PIC X(8)        VALUE '1998L970'.    
CIDMOD     12  WS-YEAR-19-99       PIC X(8)        VALUE '1999L970'.    
CIDMOD     12  WS-YEAR-20-00       PIC X(8)        VALUE '2000L200'.    
pemuni     12  WS-YEAR-20-01       PIC X(8)        VALUE '2001L210'.
PEMUNI     12  WS-YEAR-20-02       PIC X(8)        VALUE '2002L220'.
PEMUNI     12  WS-YEAR-20-03       PIC X(8)        VALUE '2003L230'.
092602     12  WS-YEAR-20-04       PIC X(8)        VALUE '2004L240'.
092602     12  WS-YEAR-20-05       PIC X(8)        VALUE '2005L250'.
011407     12  WS-YEAR-20-06       PIC X(8)        VALUE '2006L260'.
060607     12  WS-YEAR-20-07       PIC X(8)        VALUE '2007L270'.
060607     12  WS-YEAR-20-08       PIC X(8)        VALUE '2008L270'.
121610     12  WS-YEAR-20-09       PIC X(8)        VALUE '2009C400'.
121610     12  WS-YEAR-20-10       PIC X(8)        VALUE '2010C380'.
092712     12  WS-YEAR-20-11       PIC X(8)        VALUE '2011C340'.
092712     12  WS-YEAR-20-12       PIC X(8)        VALUE '2012C280'.
061213     12  WS-YEAR-20-13       PIC X(8)        VALUE '2013C210'.
031714     12  WS-YEAR-20-14       PIC X(8)        VALUE '2014C170'.
033015     12  WS-YEAR-20-15       PIC X(8)        VALUE '2015C160'.
032216     12  WS-YEAR-20-16       PIC X(8)        VALUE '2016C150'.
020717     12  WS-YEAR-20-17       PIC X(8)        VALUE '2017C140'.
021418     12  WS-YEAR-20-18       PIC X(8)        VALUE '2018C130'.
111721     12  WS-YEAR-20-19       PIC X(8)        VALUE '2019C130'.
111721     12  WS-YEAR-20-20       PIC X(8)        VALUE '2020C130'.
111721     12  WS-YEAR-20-21       PIC X(8)        VALUE '2021C130'.
111721     12  WS-YEAR-20-22       PIC X(8)        VALUE '2022C130'.
00539                                                                   
00540  01  WS-MORT-TABLE-1-10  REDEFINES WS-MORT-TABLE1-BY-YEAR.        
00541      12  WS-TABLE-FOR-1-10  OCCURS 100 TIMES INDEXED BY TBL1-NDX. 
00542          16  WS-TBL1-CCYY    PIC 9(4).                            
00543          16  WS-TBL1-PNTR    PIC X(4).                            
00544                                                                   
00545  EJECT                                                            
00546 ******************************************************************
00547 *                                                                *
CIDMOD* NOTE--STATUTORY RESERVING IS MODIFIED IN 'ECS050'              *
CIDMOD*       F.I.T. RESERVING IS MODIFIED IN 'ECS080'.                *
CIDMOD*                                                                *
00548 *     THIS TABLE CONTAINS THE MORTALITY TABLE POINTERS FOR USE   *
00549 *     IN F.I.T. RESERVING - POLICIES WITH TERMS OVER 10 YEARS    *
00550 *                                                                *
CIDMOD*     THIS TABLE MUST BE UPDATED EACH YEAR TO GET ANY NEW        *
CIDMOD*     RATES FOR THE 'CILGTAX' RUN.                               *
CIDMOD*                                                                *
CIDMOD* NOTE-- 4TH DIGIT OF TABLE VALUE MUST BE ZERO.                  *
CIDMOD* ====                            ==== == ====                   *
CIDMOD*                                                                *
00551 ******************************************************************
00552                                                                   
00553  01  WS-MORT-TABLE2-BY-YEAR.                                      
00576      12  WS-YEAR-19-23       PIC X(8)        VALUE '1923L350'.    
00577      12  WS-YEAR-19-24       PIC X(8)        VALUE '1924L350'.    
00578      12  WS-YEAR-19-25       PIC X(8)        VALUE '1925L350'.    
00579      12  WS-YEAR-19-26       PIC X(8)        VALUE '1926L350'.    
00580      12  WS-YEAR-19-27       PIC X(8)        VALUE '1927L350'.    
00581      12  WS-YEAR-19-28       PIC X(8)        VALUE '1928L350'.    
00582      12  WS-YEAR-19-29       PIC X(8)        VALUE '1929L350'.    
00583      12  WS-YEAR-19-30       PIC X(8)        VALUE '1930L350'.    
00584      12  WS-YEAR-19-31       PIC X(8)        VALUE '1931L350'.    
00585      12  WS-YEAR-19-32       PIC X(8)        VALUE '1932L350'.    
00586      12  WS-YEAR-19-33       PIC X(8)        VALUE '1933L350'.    
00587      12  WS-YEAR-19-34       PIC X(8)        VALUE '1934L350'.    
00588      12  WS-YEAR-19-35       PIC X(8)        VALUE '1935L350'.    
00589      12  WS-YEAR-19-36       PIC X(8)        VALUE '1936L350'.    
00590      12  WS-YEAR-19-37       PIC X(8)        VALUE '1937L350'.    
00591      12  WS-YEAR-19-38       PIC X(8)        VALUE '1938L350'.    
00592      12  WS-YEAR-19-39       PIC X(8)        VALUE '1939L350'.    
00593      12  WS-YEAR-19-40       PIC X(8)        VALUE '1940L350'.    
00594      12  WS-YEAR-19-41       PIC X(8)        VALUE '1941L350'.    
00595      12  WS-YEAR-19-42       PIC X(8)        VALUE '1942L350'.    
00596      12  WS-YEAR-19-43       PIC X(8)        VALUE '1943L350'.    
00597      12  WS-YEAR-19-44       PIC X(8)        VALUE '1944L350'.    
00598      12  WS-YEAR-19-45       PIC X(8)        VALUE '1945L350'.    
00599      12  WS-YEAR-19-46       PIC X(8)        VALUE '1946L350'.    
00600      12  WS-YEAR-19-47       PIC X(8)        VALUE '1947L350'.    
00601      12  WS-YEAR-19-48       PIC X(8)        VALUE '1948L350'.    
00602      12  WS-YEAR-19-49       PIC X(8)        VALUE '1949L350'.    
00603      12  WS-YEAR-19-50       PIC X(8)        VALUE '1950L350'.    
00604      12  WS-YEAR-19-51       PIC X(8)        VALUE '1951L350'.    
00605      12  WS-YEAR-19-52       PIC X(8)        VALUE '1952L350'.    
00606      12  WS-YEAR-19-53       PIC X(8)        VALUE '1953L350'.    
00607      12  WS-YEAR-19-54       PIC X(8)        VALUE '1954L350'.    
00608      12  WS-YEAR-19-55       PIC X(8)        VALUE '1955L350'.    
00609      12  WS-YEAR-19-56       PIC X(8)        VALUE '1956L350'.    
00610      12  WS-YEAR-19-57       PIC X(8)        VALUE '1957L350'.    
00611      12  WS-YEAR-19-58       PIC X(8)        VALUE '1958L350'.    
00612      12  WS-YEAR-19-59       PIC X(8)        VALUE '1959L350'.    
00613      12  WS-YEAR-19-60       PIC X(8)        VALUE '1960L350'.    
00614      12  WS-YEAR-19-61       PIC X(8)        VALUE '1961L350'.    
00615      12  WS-YEAR-19-62       PIC X(8)        VALUE '1962L350'.    
00616      12  WS-YEAR-19-63       PIC X(8)        VALUE '1963L350'.    
00617      12  WS-YEAR-19-64       PIC X(8)        VALUE '1964L350'.    
00618      12  WS-YEAR-19-65       PIC X(8)        VALUE '1965L350'.    
00619      12  WS-YEAR-19-66       PIC X(8)        VALUE '1966L350'.    
00620      12  WS-YEAR-19-67       PIC X(8)        VALUE '1967L350'.    
00621      12  WS-YEAR-19-68       PIC X(8)        VALUE '1968L350'.    
00622      12  WS-YEAR-19-69       PIC X(8)        VALUE '1969L350'.    
00623      12  WS-YEAR-19-70       PIC X(8)        VALUE '1970L350'.    
00624      12  WS-YEAR-19-71       PIC X(8)        VALUE '1971L350'.    
00625      12  WS-YEAR-19-72       PIC X(8)        VALUE '1972L350'.    
00626      12  WS-YEAR-19-73       PIC X(8)        VALUE '1973L350'.    
00627      12  WS-YEAR-19-74       PIC X(8)        VALUE '1974L350'.    
00628      12  WS-YEAR-19-75       PIC X(8)        VALUE '1975L400'.    
00629      12  WS-YEAR-19-76       PIC X(8)        VALUE '1976L400'.    
00630      12  WS-YEAR-19-77       PIC X(8)        VALUE '1977L400'.    
00631      12  WS-YEAR-19-78       PIC X(8)        VALUE '1978L400'.    
00632      12  WS-YEAR-19-79       PIC X(8)        VALUE '1979L400'.    
00633      12  WS-YEAR-19-80       PIC X(8)        VALUE '1980L450'.    
00634      12  WS-YEAR-19-81       PIC X(8)        VALUE '1981L450'.    
00635      12  WS-YEAR-19-82       PIC X(8)        VALUE '1982L450'.    
00636      12  WS-YEAR-19-83       PIC X(8)        VALUE '1983L670'.    
00637      12  WS-YEAR-19-84       PIC X(8)        VALUE '1984L670'.    
00638      12  WS-YEAR-19-85       PIC X(8)        VALUE '1985L670'.    
00639      12  WS-YEAR-19-86       PIC X(8)        VALUE '1986L670'.    
00640      12  WS-YEAR-19-87       PIC X(8)        VALUE '1987L600'.    
00641      12  WS-YEAR-19-88       PIC X(8)        VALUE '1988L770'.    
CIDMOD     12  WS-YEAR-19-89       PIC X(8)        VALUE '1989L820'.    
00643      12  WS-YEAR-19-90       PIC X(8)        VALUE '1990L830'.    
00644      12  WS-YEAR-19-91       PIC X(8)        VALUE '1991L840'.    
CIDMOD     12  WS-YEAR-19-92       PIC X(8)        VALUE '1992L840'.    
CIDMOD     12  WS-YEAR-19-93       PIC X(8)        VALUE '1993L810'.    
00653      12  WS-YEAR-19-94       PIC X(8)        VALUE '1994L740'.    
CIDMOD     12  WS-YEAR-19-95       PIC X(8)        VALUE '1995L700'.    
00655      12  WS-YEAR-19-96       PIC X(8)        VALUE '1996L660'.    
CIDMOD     12  WS-YEAR-19-97       PIC X(8)        VALUE '1997L970'.    
CIDMOD     12  WS-YEAR-19-98       PIC X(8)        VALUE '1998L970'.    
CIDMOD     12  WS-YEAR-19-99       PIC X(8)        VALUE '1999L970'.    
CIDMOD     12  WS-YEAR-20-00       PIC X(8)        VALUE '2000L200'.    
pemuni     12  WS-YEAR-20-01       PIC X(8)        VALUE '2001L210'.
PEMUNI     12  WS-YEAR-20-02       PIC X(8)        VALUE '2002L220'.
PEMUNI     12  WS-YEAR-20-03       PIC X(8)        VALUE '2003L230'.
092602     12  WS-YEAR-20-04       PIC X(8)        VALUE '2004L240'.
092602     12  WS-YEAR-20-05       PIC X(8)        VALUE '2005L250'.
011407     12  WS-YEAR-20-06       PIC X(8)        VALUE '2006L260'.
060607     12  WS-YEAR-20-07       PIC X(8)        VALUE '2007L270'.
060607     12  WS-YEAR-20-08       PIC X(8)        VALUE '2008L270'.
121610     12  WS-YEAR-20-09       PIC X(8)        VALUE '2009C400'.
121610     12  WS-YEAR-20-10       PIC X(8)        VALUE '2010C380'.
092712     12  WS-YEAR-20-11       PIC X(8)        VALUE '2011C340'.
092712     12  WS-YEAR-20-12       PIC X(8)        VALUE '2012C280'.
061213     12  WS-YEAR-20-13       PIC X(8)        VALUE '2013C210'.
031714     12  WS-YEAR-20-14       PIC X(8)        VALUE '2014C170'.
033015     12  WS-YEAR-20-15       PIC X(8)        VALUE '2015C160'.
032216     12  WS-YEAR-20-16       PIC X(8)        VALUE '2016C150'.
020717     12  WS-YEAR-20-17       PIC X(8)        VALUE '2017C140'.
021418     12  WS-YEAR-20-18       PIC X(8)        VALUE '2018C130'.
111721     12  WS-YEAR-20-19       PIC X(8)        VALUE '2019C130'.
111721     12  WS-YEAR-20-20       PIC X(8)        VALUE '2020C130'.
111721     12  WS-YEAR-20-21       PIC X(8)        VALUE '2021C130'.
111721     12  WS-YEAR-20-22       PIC X(8)        VALUE '2022C130'.
00659                                                                   
00660  01  WS-MORT-TABLE-11-20 REDEFINES WS-MORT-TABLE2-BY-YEAR.        
00661      12  WS-TABLE-FOR-11-20 OCCURS 100 TIMES INDEXED BY TBL2-NDX. 
00662          16  WS-TBL2-CCYY    PIC 9(4).                            
00663          16  WS-TBL2-PNTR    PIC X(4).                            
00664                                                                   
00665  EJECT                                                            
00666 ******************************************************************
00667 *                                                                *
00668 *     THIS TABLE CONTAINS THE MORTALITY TABLE POINTERS FOR USE   *
00669 *     IN F.I.T. RESERVING - UNITED COMPANY RUN-OFF POLICIES      *
00670 *                                                                *
00671 ******************************************************************
00672                                                                   
00673  01  UC-MORT-TABLE1-BY-YEAR.                                      
00674      12  UC-YEAR-19-01       PIC X(8)        VALUE '1901A400'.    
00675      12  UC-YEAR-19-02       PIC X(8)        VALUE '1902A400'.    
00676      12  UC-YEAR-19-03       PIC X(8)        VALUE '1903A400'.    
00677      12  UC-YEAR-19-04       PIC X(8)        VALUE '1904A400'.    
00678      12  UC-YEAR-19-05       PIC X(8)        VALUE '1905A400'.    
00679      12  UC-YEAR-19-06       PIC X(8)        VALUE '1906A400'.    
00680      12  UC-YEAR-19-07       PIC X(8)        VALUE '1907A400'.    
00681      12  UC-YEAR-19-08       PIC X(8)        VALUE '1908A400'.    
00682      12  UC-YEAR-19-09       PIC X(8)        VALUE '1909A400'.    
00683      12  UC-YEAR-19-10       PIC X(8)        VALUE '1910A400'.    
00684      12  UC-YEAR-19-11       PIC X(8)        VALUE '1911A400'.    
00685      12  UC-YEAR-19-12       PIC X(8)        VALUE '1912A400'.    
00686      12  UC-YEAR-19-13       PIC X(8)        VALUE '1913A400'.    
00687      12  UC-YEAR-19-14       PIC X(8)        VALUE '1914A400'.    
00688      12  UC-YEAR-19-15       PIC X(8)        VALUE '1915A400'.    
00689      12  UC-YEAR-19-16       PIC X(8)        VALUE '1916A400'.    
00690      12  UC-YEAR-19-17       PIC X(8)        VALUE '1917A400'.    
00691      12  UC-YEAR-19-18       PIC X(8)        VALUE '1918A400'.    
00692      12  UC-YEAR-19-19       PIC X(8)        VALUE '1919A400'.    
00693      12  UC-YEAR-19-20       PIC X(8)        VALUE '1920A400'.    
00694      12  UC-YEAR-19-21       PIC X(8)        VALUE '1921A400'.    
00695      12  UC-YEAR-19-22       PIC X(8)        VALUE '1922A400'.    
00696      12  UC-YEAR-19-23       PIC X(8)        VALUE '1923A400'.    
00697      12  UC-YEAR-19-24       PIC X(8)        VALUE '1924A400'.    
00698      12  UC-YEAR-19-25       PIC X(8)        VALUE '1925A400'.    
00699      12  UC-YEAR-19-26       PIC X(8)        VALUE '1926A400'.    
00700      12  UC-YEAR-19-27       PIC X(8)        VALUE '1927A400'.    
00701      12  UC-YEAR-19-28       PIC X(8)        VALUE '1928A400'.    
00702      12  UC-YEAR-19-29       PIC X(8)        VALUE '1929A400'.    
00703      12  UC-YEAR-19-30       PIC X(8)        VALUE '1930A400'.    
00704      12  UC-YEAR-19-31       PIC X(8)        VALUE '1931A400'.    
00705      12  UC-YEAR-19-32       PIC X(8)        VALUE '1932A400'.    
00706      12  UC-YEAR-19-33       PIC X(8)        VALUE '1933A400'.    
00707      12  UC-YEAR-19-34       PIC X(8)        VALUE '1934A400'.    
00708      12  UC-YEAR-19-35       PIC X(8)        VALUE '1935A400'.    
00709      12  UC-YEAR-19-36       PIC X(8)        VALUE '1936A400'.    
00710      12  UC-YEAR-19-37       PIC X(8)        VALUE '1937A400'.    
00711      12  UC-YEAR-19-38       PIC X(8)        VALUE '1938A400'.    
00712      12  UC-YEAR-19-39       PIC X(8)        VALUE '1939A400'.    
00713      12  UC-YEAR-19-40       PIC X(8)        VALUE '1940A400'.    
00714      12  UC-YEAR-19-41       PIC X(8)        VALUE '1941A400'.    
00715      12  UC-YEAR-19-42       PIC X(8)        VALUE '1942A400'.    
00716      12  UC-YEAR-19-43       PIC X(8)        VALUE '1943A400'.    
00717      12  UC-YEAR-19-44       PIC X(8)        VALUE '1944A400'.    
00718      12  UC-YEAR-19-45       PIC X(8)        VALUE '1945A400'.    
00719      12  UC-YEAR-19-46       PIC X(8)        VALUE '1946A400'.    
00720      12  UC-YEAR-19-47       PIC X(8)        VALUE '1947A400'.    
00721      12  UC-YEAR-19-48       PIC X(8)        VALUE '1948A400'.    
00722      12  UC-YEAR-19-49       PIC X(8)        VALUE '1949A400'.    
00723      12  UC-YEAR-19-50       PIC X(8)        VALUE '1950A400'.    
00724      12  UC-YEAR-19-51       PIC X(8)        VALUE '1951A400'.    
00725      12  UC-YEAR-19-52       PIC X(8)        VALUE '1952A400'.    
00726      12  UC-YEAR-19-53       PIC X(8)        VALUE '1953A400'.    
00727      12  UC-YEAR-19-54       PIC X(8)        VALUE '1954A400'.    
00728      12  UC-YEAR-19-55       PIC X(8)        VALUE '1955A400'.    
00729      12  UC-YEAR-19-56       PIC X(8)        VALUE '1956A400'.    
00730      12  UC-YEAR-19-57       PIC X(8)        VALUE '1957A400'.    
00731      12  UC-YEAR-19-58       PIC X(8)        VALUE '1958A400'.    
00732      12  UC-YEAR-19-59       PIC X(8)        VALUE '1959A400'.    
00733      12  UC-YEAR-19-60       PIC X(8)        VALUE '1960A400'.    
00734      12  UC-YEAR-19-61       PIC X(8)        VALUE '1961A400'.    
00735      12  UC-YEAR-19-62       PIC X(8)        VALUE '1962A400'.    
00736      12  UC-YEAR-19-63       PIC X(8)        VALUE '1963A400'.    
00737      12  UC-YEAR-19-64       PIC X(8)        VALUE '1964A400'.    
00738      12  UC-YEAR-19-65       PIC X(8)        VALUE '1965A400'.    
00739      12  UC-YEAR-19-66       PIC X(8)        VALUE '1966A400'.    
00740      12  UC-YEAR-19-67       PIC X(8)        VALUE '1967A400'.    
00741      12  UC-YEAR-19-68       PIC X(8)        VALUE '1968A400'.    
00742      12  UC-YEAR-19-69       PIC X(8)        VALUE '1969A400'.    
00743      12  UC-YEAR-19-70       PIC X(8)        VALUE '1970A400'.    
00744      12  UC-YEAR-19-71       PIC X(8)        VALUE '1971A400'.    
00745      12  UC-YEAR-19-72       PIC X(8)        VALUE '1972A400'.    
00746      12  UC-YEAR-19-73       PIC X(8)        VALUE '1973A400'.    
00747      12  UC-YEAR-19-74       PIC X(8)        VALUE '1974A400'.    
00748      12  UC-YEAR-19-75       PIC X(8)        VALUE '1975A400'.    
00749      12  UC-YEAR-19-76       PIC X(8)        VALUE '1976A400'.    
00750      12  UC-YEAR-19-77       PIC X(8)        VALUE '1977A400'.    
00751      12  UC-YEAR-19-78       PIC X(8)        VALUE '1978A400'.    
00752      12  UC-YEAR-19-79       PIC X(8)        VALUE '1979A400'.    
00753      12  UC-YEAR-19-80       PIC X(8)        VALUE '1980A450'.    
00754      12  UC-YEAR-19-81       PIC X(8)        VALUE '1981A450'.    
00755      12  UC-YEAR-19-82       PIC X(8)        VALUE '1982A450'.    
00756      12  UC-YEAR-19-83       PIC X(8)        VALUE '1983A720'.    
00757      12  UC-YEAR-19-84       PIC X(8)        VALUE '1984A720'.    
00758      12  UC-YEAR-19-85       PIC X(8)        VALUE '1985A720'.    
00759      12  UC-YEAR-19-86       PIC X(8)        VALUE '1986A720'.    
00760      12  UC-YEAR-19-87       PIC X(8)        VALUE '1987A720'.    
00761      12  UC-YEAR-19-88       PIC X(8)        VALUE '1988A720'.    
00762      12  UC-YEAR-19-89       PIC X(8)        VALUE '1989A720'.    
00763      12  UC-YEAR-19-90       PIC X(8)        VALUE '1990A720'.    
00764      12  UC-YEAR-19-91       PIC X(8)        VALUE '1991A720'.    
00765      12  UC-YEAR-19-92       PIC X(8)        VALUE '1992A720'.    
00766      12  UC-YEAR-19-93       PIC X(8)        VALUE '1993A720'.    
00767      12  UC-YEAR-19-94       PIC X(8)        VALUE '1994A720'.    
00768      12  UC-YEAR-19-95       PIC X(8)        VALUE '1995A720'.    
00769      12  UC-YEAR-19-96       PIC X(8)        VALUE '1996A720'.    
00770      12  UC-YEAR-19-97       PIC X(8)        VALUE '1997A720'.    
00771      12  UC-YEAR-19-98       PIC X(8)        VALUE '1998A720'.    
00772      12  UC-YEAR-19-99       PIC X(8)        VALUE '1999A720'.    
00773                                                                   
00774  01  UC-MORT-TABLE-1-10  REDEFINES UC-MORT-TABLE1-BY-YEAR.        
00775      12  UC-TABLE-FOR-1-10  OCCURS 99 TIMES INDEXED BY UTBL-NDX.  
00776          16  UC-TBL-CCYY     PIC 9(4).                            
00777          16  UC-TBL-PNTR     PIC X(4).                            
00778                                                                   
00779  EJECT                                                            
00780 ******************************************************************
00781 *                                                                *
00782 *     THIS TABLE CONTAINS THE MORTALITY TABLE POINTERS FOR USE   *
00783 *     IN F.I.T. RESERVING - BANK OF AMERICA 1992 FIT RESERVES    *
00784 *                                                                *
00785 ******************************************************************
00786                                                                   
00787  01  BA-MORT-TABLE1-BY-YEAR.                                      
00788      12  BA-YEAR-19-01       PIC X(8)        VALUE '1901A400'.    
00789      12  BA-YEAR-19-02       PIC X(8)        VALUE '1902A400'.    
00790      12  BA-YEAR-19-03       PIC X(8)        VALUE '1903A400'.    
00791      12  BA-YEAR-19-04       PIC X(8)        VALUE '1904A400'.    
00792      12  BA-YEAR-19-05       PIC X(8)        VALUE '1905A400'.    
00793      12  BA-YEAR-19-06       PIC X(8)        VALUE '1906A400'.    
00794      12  BA-YEAR-19-07       PIC X(8)        VALUE '1907A400'.    
00795      12  BA-YEAR-19-08       PIC X(8)        VALUE '1908A400'.    
00796      12  BA-YEAR-19-09       PIC X(8)        VALUE '1909A400'.    
00797      12  BA-YEAR-19-10       PIC X(8)        VALUE '1910A400'.    
00798      12  BA-YEAR-19-11       PIC X(8)        VALUE '1911A400'.    
00799      12  BA-YEAR-19-12       PIC X(8)        VALUE '1912A400'.    
00800      12  BA-YEAR-19-13       PIC X(8)        VALUE '1913A400'.    
00801      12  BA-YEAR-19-14       PIC X(8)        VALUE '1914A400'.    
00802      12  BA-YEAR-19-15       PIC X(8)        VALUE '1915A400'.    
00803      12  BA-YEAR-19-16       PIC X(8)        VALUE '1916A400'.    
00804      12  BA-YEAR-19-17       PIC X(8)        VALUE '1917A400'.    
00805      12  BA-YEAR-19-18       PIC X(8)        VALUE '1918A400'.    
00806      12  BA-YEAR-19-19       PIC X(8)        VALUE '1919A400'.    
00807      12  BA-YEAR-19-20       PIC X(8)        VALUE '1920A400'.    
00808      12  BA-YEAR-19-21       PIC X(8)        VALUE '1921A400'.    
00809      12  BA-YEAR-19-22       PIC X(8)        VALUE '1922A400'.    
00810      12  BA-YEAR-19-23       PIC X(8)        VALUE '1923A400'.    
00811      12  BA-YEAR-19-24       PIC X(8)        VALUE '1924A400'.    
00812      12  BA-YEAR-19-25       PIC X(8)        VALUE '1925A400'.    
00813      12  BA-YEAR-19-26       PIC X(8)        VALUE '1926A400'.    
00814      12  BA-YEAR-19-27       PIC X(8)        VALUE '1927A400'.    
00815      12  BA-YEAR-19-28       PIC X(8)        VALUE '1928A400'.    
00816      12  BA-YEAR-19-29       PIC X(8)        VALUE '1929A400'.    
00817      12  BA-YEAR-19-30       PIC X(8)        VALUE '1930A400'.    
00818      12  BA-YEAR-19-31       PIC X(8)        VALUE '1931A400'.    
00819      12  BA-YEAR-19-32       PIC X(8)        VALUE '1932A400'.    
00820      12  BA-YEAR-19-33       PIC X(8)        VALUE '1933A400'.    
00821      12  BA-YEAR-19-34       PIC X(8)        VALUE '1934A400'.    
00822      12  BA-YEAR-19-35       PIC X(8)        VALUE '1935A400'.    
00823      12  BA-YEAR-19-36       PIC X(8)        VALUE '1936A400'.    
00824      12  BA-YEAR-19-37       PIC X(8)        VALUE '1937A400'.    
00825      12  BA-YEAR-19-38       PIC X(8)        VALUE '1938A400'.    
00826      12  BA-YEAR-19-39       PIC X(8)        VALUE '1939A400'.    
00827      12  BA-YEAR-19-40       PIC X(8)        VALUE '1940A400'.    
00828      12  BA-YEAR-19-41       PIC X(8)        VALUE '1941A400'.    
00829      12  BA-YEAR-19-42       PIC X(8)        VALUE '1942A400'.    
00830      12  BA-YEAR-19-43       PIC X(8)        VALUE '1943A400'.    
00831      12  BA-YEAR-19-44       PIC X(8)        VALUE '1944A400'.    
00832      12  BA-YEAR-19-45       PIC X(8)        VALUE '1945A400'.    
00833      12  BA-YEAR-19-46       PIC X(8)        VALUE '1946A400'.    
00834      12  BA-YEAR-19-47       PIC X(8)        VALUE '1947A400'.    
00835      12  BA-YEAR-19-48       PIC X(8)        VALUE '1948A400'.    
00836      12  BA-YEAR-19-49       PIC X(8)        VALUE '1949A400'.    
00837      12  BA-YEAR-19-50       PIC X(8)        VALUE '1950A400'.    
00838      12  BA-YEAR-19-51       PIC X(8)        VALUE '1951A400'.    
00839      12  BA-YEAR-19-52       PIC X(8)        VALUE '1952A400'.    
00840      12  BA-YEAR-19-53       PIC X(8)        VALUE '1953A400'.    
00841      12  BA-YEAR-19-54       PIC X(8)        VALUE '1954A400'.    
00842      12  BA-YEAR-19-55       PIC X(8)        VALUE '1955A400'.    
00843      12  BA-YEAR-19-56       PIC X(8)        VALUE '1956A400'.    
00844      12  BA-YEAR-19-57       PIC X(8)        VALUE '1957A400'.    
00845      12  BA-YEAR-19-58       PIC X(8)        VALUE '1958A400'.    
00846      12  BA-YEAR-19-59       PIC X(8)        VALUE '1959A400'.    
00847      12  BA-YEAR-19-60       PIC X(8)        VALUE '1960A400'.    
00848      12  BA-YEAR-19-61       PIC X(8)        VALUE '1961A400'.    
00849      12  BA-YEAR-19-62       PIC X(8)        VALUE '1962A400'.    
00850      12  BA-YEAR-19-63       PIC X(8)        VALUE '1963A400'.    
00851      12  BA-YEAR-19-64       PIC X(8)        VALUE '1964A400'.    
00852      12  BA-YEAR-19-65       PIC X(8)        VALUE '1965A400'.    
00853      12  BA-YEAR-19-66       PIC X(8)        VALUE '1966A400'.    
00854      12  BA-YEAR-19-67       PIC X(8)        VALUE '1967A400'.    
00855      12  BA-YEAR-19-68       PIC X(8)        VALUE '1968A400'.    
00856      12  BA-YEAR-19-69       PIC X(8)        VALUE '1969A400'.    
00857      12  BA-YEAR-19-70       PIC X(8)        VALUE '1970A400'.    
00858      12  BA-YEAR-19-71       PIC X(8)        VALUE '1971A400'.    
00859      12  BA-YEAR-19-72       PIC X(8)        VALUE '1972A400'.    
00860      12  BA-YEAR-19-73       PIC X(8)        VALUE '1973A400'.    
00861      12  BA-YEAR-19-74       PIC X(8)        VALUE '1974A400'.    
00862      12  BA-YEAR-19-75       PIC X(8)        VALUE '1975A400'.    
00863      12  BA-YEAR-19-76       PIC X(8)        VALUE '1976A400'.    
00864      12  BA-YEAR-19-77       PIC X(8)        VALUE '1977A400'.    
00865      12  BA-YEAR-19-78       PIC X(8)        VALUE '1978A400'.    
00866      12  BA-YEAR-19-79       PIC X(8)        VALUE '1979A400'.    
00867      12  BA-YEAR-19-80       PIC X(8)        VALUE '1980B800'.    
00868      12  BA-YEAR-19-81       PIC X(8)        VALUE '1981B800'.    
00869      12  BA-YEAR-19-82       PIC X(8)        VALUE '1982B800'.    
00870      12  BA-YEAR-19-83       PIC X(8)        VALUE '1983B830'.    
00871      12  BA-YEAR-19-84       PIC X(8)        VALUE '1984B830'.    
00872      12  BA-YEAR-19-85       PIC X(8)        VALUE '1985B830'.    
00873      12  BA-YEAR-19-86       PIC X(8)        VALUE '1986B830'.    
00874      12  BA-YEAR-19-87       PIC X(8)        VALUE '1987B870'.    
00875      12  BA-YEAR-19-88       PIC X(8)        VALUE '1988B880'.    
00876      12  BA-YEAR-19-89       PIC X(8)        VALUE '1989B890'.    
00877      12  BA-YEAR-19-90       PIC X(8)        VALUE '1990B900'.    
00878      12  BA-YEAR-19-91       PIC X(8)        VALUE '1991B910'.    
00879      12  BA-YEAR-19-92       PIC X(8)        VALUE '1992B920'.    
00880      12  BA-YEAR-19-93       PIC X(8)        VALUE '1993L800'.    
00881      12  BA-YEAR-19-94       PIC X(8)        VALUE '1994L740'.    
00882      12  BA-YEAR-19-95       PIC X(8)        VALUE '1995L690'.    
00883      12  BA-YEAR-19-96       PIC X(8)        VALUE '1996L660'.    
00884      12  BA-YEAR-19-97       PIC X(8)        VALUE '1997L630'.    
00885      12  BA-YEAR-19-98       PIC X(8)        VALUE '1998L630'.    
00886      12  BA-YEAR-19-99       PIC X(8)        VALUE '1999L630'.    
00887                                                                   
00888  01  BA-MORT-TABLE-1-10  REDEFINES BA-MORT-TABLE1-BY-YEAR.        
00889      12  BA-TABLE-FOR-1-10  OCCURS 99 TIMES INDEXED BY BATBL-NDX. 
00890          16  BA-TBL-CCYY     PIC 9(4).                            
00891          16  BA-TBL-PNTR     PIC X(4).                            
00892                                                                   
00893  EJECT                                                            
00894                                                                   
00895  01  PRINT-LINES.                                                 
00896      12  HDR-1.                                                   
00897          16  FILLER          PIC X(46)   VALUE SPACES.            
00898          16  FILLER          PIC X(32)   VALUE                    
00899              'POLICIES EXCLUDED FROM VALUATION'.                  
00900          16  FILLER          PIC X(41)   VALUE SPACES.            
00901          16  FILLER          PIC X(8)    VALUE 'ECS080'.          
00902      12  HDR-2.                                                   
00903          16  FILLER          PIC X(47)   VALUE SPACES.            
00904          16  H2-COMP         PIC X(30).                           
00905          16  FILLER          PIC X(42)   VALUE SPACES.            
00906          16  H2-DATE         PIC X(8).                            
00907      12  HDR-3.                                                   
00908          16  FILLER          PIC X(53)   VALUE SPACES.            
00909          16  H3-DATE         PIC X(18).                           
00910          16  FILLER          PIC X(48)   VALUE SPACES.            
00911          16  FILLER          PIC X(5)    VALUE 'PAGE '.           
00912          16  H3-PAGE         PIC ZZ,ZZ9.                          
00913      12  HDR-4.                                                   
00914          16  FILLER          PIC X(45)   VALUE                    
00915             ' CAR GROUP  STATE   ACCOUNT   EFFECTIVE-DATE '.      
00916          16  FILLER          PIC X(44)   VALUE                    
00917              '     CERT.NO.   REIN. LIFE-TYPE   TABLE   AG'.      
00918          16  FILLER          PIC X(44)   VALUE                    
00919              'E   TERM    REASON'.                                
00920      12  DTL-1.                                                   
00921          16  FILLER          PIC X       VALUE SPACES.            
00922          16  FILLER          PIC X       VALUE SPACES.            
00923          16  D1-CARR         PIC X.                               
00924          16  FILLER          PIC X(2)    VALUE SPACES.            
00925          16  D1-GROUP        PIC X(6).                            
00926          16  FILLER          PIC X(2)    VALUE SPACES.            
00927          16  D1-STATE        PIC X(2).                            
00928          16  FILLER          PIC X(4)    VALUE SPACES.            
00929          16  D1-ACCT         PIC X(10).                           
00930          16  FILLER          PIC X(4)    VALUE SPACES.            
00931          16  D1-EFF-MO       PIC Z9.                              
00932          16  FILLER          PIC X       VALUE '-'.               
00933          16  D1-EFF-DA       PIC 99.                              
00934          16  FILLER          PIC X       VALUE '-'.               
00935          16  D1-EFF-YR       PIC 99.                              
00936          16  FILLER          PIC X(7)    VALUE SPACES.            
00937          16  D1-CERT         PIC X(11).                           
00938          16  FILLER          PIC X       VALUE SPACES.            
00939          16  D1-REIN         PIC X(6).                            
00940          16  FILLER          PIC X       VALUE SPACES.            
00941          16  D1-LIFE-TYPE    PIC X(10).                           
00942          16  FILLER          PIC X(2)    VALUE SPACES.            
00943          16  D1-TABLE        PIC X(4).                            
00944          16  FILLER          PIC X(5)    VALUE SPACES.            
00945          16  D1-AGE          PIC 99.                              
00946          16  FILLER          PIC X(3)    VALUE SPACES.            
00947          16  D1-TERM         PIC ZZ9.                             
00948          16  FILLER          PIC X(5)    VALUE SPACES.            
00949          16  D1-REASON       PIC X(32).                           
00950      12  TOT-1.                                                   
00951          16  FILLER          PIC X(26)   VALUE                    
00952             ' GROSS RESERVE........... '.                         
00953          16  T1-RESERVE      PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              
00954      12  TOT-2.                                                   
00955          16  FILLER          PIC X(26)   VALUE                    
00956             ' REINSURANCE RESERVE..... '.                         
00957          16  T2-RESERVE      PIC ZZ,ZZZ,ZZZ,ZZZ.99-.              
00958      12  TOT-3.                                                   
00959          16  FILLER          PIC X(26)   VALUE                    
00960             ' MORTALITY ERRORS........ '.                         
00961          16  T3-COUNT        PIC ZZ,ZZZ,ZZZ,ZZ9.                  
00962      12  TST-1.                                                   
00963          16  FILLER          PIC X.                               
00964          16  T1-CERT         PIC X(11).                           
00965          16  FILLER          PIC X.                               
00966          16  T1-AGE          PIC 99.                              
00967          16  FILLER          PIC X.                               
00968          16  T1-TBL          PIC X(4).                            
00969          16  FILLER          PIC X.                               
00970          16  T1-TRM          PIC 999.                             
00971          16  FILLER          PIC X.                               
00972          16  T1-REM          PIC 999.                             
00973          16  FILLER          PIC X.                               
00974          16  T1-APR          PIC -999.9999.                       
00975          16  FILLER          PIC X.                               
00976          16  T1-REM-AMT      PIC -Z,ZZZ,ZZZ.99.                   
00977          16  FILLER          PIC X.                               
00978          16  T1-RESV         PIC -ZZ,ZZZ.99.                      
00979          16  FILLER          PIC X.                               
00980                                                                   
00981  01  AM-FILE-STATUS          PIC X(2).                            
00982  01  REIN-FILE-STATUS.                                            
00983      12  REIN-FILE-STATUS-1  PIC X.                               
00984      12  REIN-FILE-STATUS-2  PIC X.                               
00985                                                                   
00986  01  MORT-FILE-STATUS.                                            
00987      12  MORT-FILE-STATUS-1  PIC X.                               
00988      12  MORT-FILE-STATUS-2  PIC X.                               
00989                                                                   
00990  EJECT                                                            
00991      COPY ELCDTECX.                                               
00992      COPY ELCDTEVR.                                               
00993  EJECT                                                            
00994      COPY ELCDATE.                                                
00995  EJECT                                                            
00996      COPY ELCCALC.                                                
00997  EJECT                                                            
00998      COPY ELCGAPVR.                                               
00999  EJECT                                                            
01000      COPY ELCACCTV.                                               
01001  EJECT                                                            
01002  01  W-CONTROL-OVERRIDE.                                          
01003      12  W-CO-ACCOUNT        PIC  X(10).                          
01004      12  W-CO-CERT           PIC  X(10).                          

01006  PROCEDURE DIVISION.
01007                                                                   
01008      DISPLAY '****** THE FOLLOWING MESSAGES WERE CREATED BY '     
01009          'ECS080. ******'.                                        
01010      DISPLAY ' '.                                                 
01011                                                                   
01012      MOVE LOW-VALUES           TO W-GNP-TABLE.                    
01013                                                                   
01014 ***************************************************************   
01015 *  THIS LOGIC WAS LEFT IN TO ALLOW EASE IN PIN POINT SPECIFIC *   
01016 *  GAAP RECORDS FOR TROUBLE SHOOTING.                         *   
01017 ***************************************************************   
CIDMOD**   ACCEPT W-CONTROL-OVERRIDE.                                   
01019                                                                   
CIDMOD**   IF  W-CONTROL-OVERRIDE GREATER THAN SPACES                   
CIDMOD**       DISPLAY 'ACCOUNT     - ' W-CO-ACCOUNT                    
CIDMOD**       DISPLAY 'CERT        - ' W-CO-CERT                       
CIDMOD**       DISPLAY ' '.                                             
01024                                                                   
01025  CAPTURE-START.
pemuni     OPEN OUTPUT GAAP-NEW
062104                 ME-ECS080-BALANCE.
01026      OPEN I-O ERMEBL.                                             
01027      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        
01028          NEXT SENTENCE                                            
01029      ELSE                                                       
01030          MOVE 'N' TO ME-UPDATE-FLAG
062104     END-IF.
01031                                                                   
01032  0000-READ-DATE-RTN.                                              
01033                              COPY ELCDTERX.                       
pemuni     OPEN OUTPUT GAAP-NEW-NET-PAY.
01034      MOVE WS-TIME                TO ME-START-TIME.                
01035      MOVE WS-CURRENT-DATE        TO ME-START-DATE.                
01036      MOVE ME-START-MO            TO ME-CNDS-MO.                   
01037      MOVE ME-START-DA            TO ME-CNDS-DA.                   
01038      MOVE ME-START-YR            TO ME-CNDS-YR.                   
01039                                                                   
01040      MOVE BIN-RUN-DATE           TO W-RUN-DATE-BIN.               
01041                                                                   
01042  EJECT                                                            
01043      MOVE DTE-CLIENT             TO ME-COMPANY.                   
01044      COMPUTE MONTH-END-MOYR = (RUN-CCYY * 12) + RUN-MO.           
01045      MOVE MONTH-END-MOYR         TO ME-MOYR.                      
01046      IF ME-DO-UPDATE                                              
01047          READ ERMEBL INVALID KEY                                  
01048          MOVE 'N'                TO ME-UPDATE-FLAG                
01049          CLOSE ERMEBL.                                            
01050                                                                   
01051      IF ME-DO-UPDATE                                              
01052          CLOSE ERMEBL.                                            
01053                                                                   
01054      MOVE WS-CURRENT-DATE       TO H2-DATE.                       
01055      MOVE ALPH-DATE             TO H3-DATE.                       
01056      MOVE COMPANY-NAME          TO H2-COMP.                       
01057                                                                   
01058      OPEN  INPUT TAPE-FACTORS.                                    
01059                                                                   
01060      MOVE SPACES                TO MORT-CODE-TABLE                
01061                                    FACTOR-TABLE                   
01062                                    RF-REC.                        
01063      MOVE +1                    TO SB                             
01064                                    X1.                            
01065                                                                   
01066  0100-READ-TAPE-FACTORS.                                          
01067      READ TAPE-FACTORS AT END                                     
01068          GO TO 0125-GEN-MX-VALUES.                                
01069                                                                   
01070      MOVE RF-RECORD             TO RF-REC.                        
01071                                                                   
01072      MOVE RF-REC                 TO FACT-TBL (SB).                
01073      MOVE RF-CODE                TO MORTALITY-CODE (SB).          
01074                                                                   
01075      ADD +1                      TO SB                            
01076                                                                   
01077      IF SB GREATER F-T-LIMIT                                      
01078         MOVE 0201     TO WS-RETURN-CODE                           
01079         MOVE ' MORTALITY TABLE FULL ' TO WS-ABEND-MESSAGE         
01080         GO TO ABEND-PGM.                                          
01081                                                                   
01082      GO TO 0100-READ-TAPE-FACTORS.                                
01083                                                                   
01084  0125-GEN-MX-VALUES.                                              
01085      MOVE +1                    TO SA SB.                         
01086      MOVE ZERO                  TO SC SE.                         
01087                                                                   
01088  0130-MX-LOOP-A.                                                  
01089      ADD +1                     TO SE.                            
01090      IF MORTALITY-CODE (SE) = SPACES                              
01091           GO TO 0150-END-LOAD-FACTORS.                            
01092                                                                   
01093      MOVE FACT-TBL (SE)         TO RF-REC.                        
01094      MOVE ZERO                  TO SA SG.                         
01095      PERFORM 0145-ZERO-MX-LOOP THRU 0145-LOOP-EXIT 99 TIMES.      
01096                                                                   
01097  0135-MX-LOOP-B.                                                  
01098      ADD +1 TO SA.                                                
01099      IF SA = +99                                                  
01100          MOVE RF-CX-FACTOR (SA) TO MX-FACTORS (SA)                
01101          MOVE MORT-MX-TABLE     TO MX-SAVE-ENTRY (SE)             
01102          GO TO 0130-MX-LOOP-A.                                    
01103                                                                   
01104      PERFORM 0140-MX-LOOP-C THRU 0140-LOOP-EXIT                   
01105          VARYING SC FROM SA BY +1 UNTIL SC GREATER +99.           
01106                                                                   
01107      GO TO 0135-MX-LOOP-B.                                        
01108                                                                   
01109  0140-MX-LOOP-C.                                                  
01110      COMPUTE MX-FACTORS (SA) = MX-FACTORS (SA) +                  
01111                                RF-CX-FACTOR (SC).                 
01112                                                                   
01113  0140-LOOP-EXIT.                                                  
01114      EXIT.                                                        
01115                                                                   
01116  0145-ZERO-MX-LOOP.                                               
01117      ADD +1 TO SG.                                                
01118      MOVE ZERO                 TO MX-FACTORS (SG).                
01119                                                                   
01120  0145-LOOP-EXIT.                                                  
01121      EXIT.                                                        
01122                                                                   
01123  0150-END-LOAD-FACTORS.                                           
01124      CLOSE       TAPE-FACTORS.                                    
01125                                                                   
01126      OPEN  INPUT ACC-MSTR                                         
01127                  GAAP-EXTRACT                                     
01128                  GAAP-LAST-NET-PAY                                
pemuni          OUTPUT DISPLAY-PRT
01131                  PRNTR.                                           
01132                                                                   
CIDMOD     PERFORM 8600-DISPLAY-PRT  THRU  8600-DISPLAY-EXIT.
CIDMOD
01133      IF AM-FILE-STATUS = '00' OR '97'                             
01134          NEXT SENTENCE                                            
01135        ELSE                                                       
01136         MOVE ' ERROR ON ERACCTT, OPEN ' TO WS-ABEND-MESSAGE       
01137         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               
01138         GO TO ABEND-PGM.                                          
01139                                                                   
01140      IF CLAS-MAXL = ZEROS                                         
01141          MOVE +1                 TO CLAS-STARTL.                  
01142                                                                   
01143      IF CLAS-MAXM = ZEROS                                         
01144          MOVE +1                 TO CLAS-STARTM.                  
01145                                                                   
01146      MOVE +1 TO SA.                                               
01147      MOVE FACT-TBL (SA)          TO RF-REC.                       
01148      MOVE MX-SAVE-ENTRY (SA)     TO MORT-MX-TABLE.                
01149                                                                   
01150      PERFORM 0900-READ-ACCT-MSTR THRU 0999-READ-ACCT-MSTR-EX.     
01151  EJECT                                                            
01152  0160-READ-GAAP-FILE.                                             
01153      READ GAAP-EXTRACT AT END                                     
01154          GO TO 9000-E-O-J.                                        
01155                                                                   
01156 ***************************************************************   
01157 *  THIS LOGIC WAS LEFT IN TO ALLOW EASE IN PIN POINT SPECIFIC *   
01158 *  GAAP RECORDS FOR TROUBLE SHOOTING.                         *   
01159 ***************************************************************   
01160      COPY ELCGAPM1.                                               
01161                                                                   
CIDMOD* DISPLAYS FOR "QD" RCDS LEFT IN FOR USE AT A LATER TIME, IF      
CIDMOD* NEEDED.                                                         
CIDMOD* PROGRAM WILL GO TO "QD-BYPASS" AS IT IS NOW CODED.              
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD*    IF  QD-CNT GREATER THAN 25                                   
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD     GO TO QD-BYPASS.                                             
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD     IF  GR-LFTYP = 'QD'                                          
CIDMOD         NEXT SENTENCE                                            
CIDMOD      ELSE                                                        
CIDMOD         GO TO QD-BYPASS.                                         
CIDMOD                                                                  
CIDMOD        ADD 1 TO QD-CNT.                                          
CIDMOD        DISPLAY ' '.                                              
CIDMOD        DISPLAY ' START OF RECORD '                               
CIDMOD        DISPLAY ' '.                                              
CIDMOD        DISPLAY 'GR-REIN           ' GR-REIN                      
CIDMOD        DISPLAY 'GR-CARRIER        ' GR-CARRIER                   
CIDMOD        DISPLAY 'GR-GROUPING       ' GR-GROUPING                  
CIDMOD        DISPLAY 'GR-STATE          ' GR-STATE                     
CIDMOD        DISPLAY 'GR-ACCOUNT        ' GR-ACCOUNT                   
CIDMOD        DISPLAY 'GR-EFF            ' GR-EFF                       
CIDMOD        DISPLAY 'GR-CERT-NO        ' GR-CERT-NO                   
CIDMOD        DISPLAY 'GR-REINCO         ' GR-REINCO                    
CIDMOD        DISPLAY 'GR-REINCO-SUB     ' GR-REINCO-SUB                
CIDMOD        DISPLAY 'GR-IG             ' GR-IG                        
CIDMOD        DISPLAY 'GR-APR            ' GR-APR                       
CIDMOD        DISPLAY 'GR-PMT-FREQ       ' GR-PMT-FREQ                  
CIDMOD        DISPLAY 'GR-LOAN-TERM      ' GR-LOAN-TERM                 
CIDMOD        DISPLAY 'GR-AGE            ' GR-AGE                       
CIDMOD        DISPLAY 'GR-LFTYP          ' GR-LFTYP                     
CIDMOD        DISPLAY 'GR-LF-TERM        ' GR-LF-TERM                   
CIDMOD        DISPLAY 'GR-LF-REMTERM     ' GR-LF-REMTERM                
CIDMOD        DISPLAY 'GR-LF-UP-REMTERM  ' GR-LF-UP-REMTERM             
CIDMOD        DISPLAY 'GR-LFBEN          ' GR-LFBEN                     
CIDMOD        DISPLAY 'GR-LFPRM          ' GR-LFPRM                     
CIDMOD        DISPLAY 'GR-LFCOM          ' GR-LFCOM                     
CIDMOD        DISPLAY 'GR-LFEXP          ' GR-LFEXP                     
CIDMOD        DISPLAY 'GR-LFTAX          ' GR-LFTAX                     
CIDMOD        DISPLAY 'GRP-LFPRM         ' GRP-LFPRM                    
CIDMOD        DISPLAY 'GRP-LFCOM         ' GRP-LFCOM                    
CIDMOD        DISPLAY 'GRP-LFEXP         ' GRP-LFEXP                    
CIDMOD        DISPLAY 'GRP-LFTAX         ' GRP-LFTAX                    
CIDMOD        DISPLAY 'GRR-LFPRM         ' GRR-LFPRM                    
CIDMOD        DISPLAY 'GRR-LFCOM         ' GRR-LFCOM                    
CIDMOD        DISPLAY 'GRR-LFEXP         ' GRR-LFEXP                    
CIDMOD        DISPLAY 'GRR-LFTAX         ' GRR-LFTAX                    
CIDMOD        DISPLAY 'GRD-LFPRM         ' GRD-LFPRM                    
CIDMOD        DISPLAY 'GRD-LFCOM         ' GRD-LFCOM                    
CIDMOD        DISPLAY 'GRS-LFPRM         ' GRS-LFPRM                    
CIDMOD        DISPLAY 'GRS-LFCOM         ' GRS-LFCOM                    
CIDMOD        DISPLAY 'GR-MORT-CONTROL   ' GR-MORT-CONTROL              
CIDMOD        DISPLAY 'GR-MORT-BASE      ' GR-MORT-BASE                 
CIDMOD        DISPLAY 'GR-MORT-TYP       ' GR-MORT-TYP                  
CIDMOD        DISPLAY 'GR-MORT-AGE       ' GR-MORT-AGE                  
CIDMOD        DISPLAY 'GR-MORT-DATA      ' GR-MORT-DATA                 
CIDMOD        DISPLAY 'GR-REM-AMT        ' GR-REM-AMT                   
CIDMOD        DISPLAY 'GR-MO-DEC         ' GR-MO-DEC                    
CIDMOD        DISPLAY 'GR-MORT-FACT      ' GR-MORT-FACT                 
CIDMOD        DISPLAY 'GR-RESV           ' GR-RESV                      
CIDMOD        DISPLAY 'GR-FLAG           ' GR-FLAG                      
CIDMOD        DISPLAY 'GR-ALT-MORT-BASE  ' GR-ALT-MORT-CODE             
CIDMOD        DISPLAY 'GR-ALT-MORT-TYP   ' GR-ALT-MORT-TYP              
CIDMOD        DISPLAY 'GR-ALT-RESV       ' GR-ALT-RESV                  
CIDMOD        DISPLAY 'GR-CNT            ' GR-CNT                       
CIDMOD        DISPLAY 'GR-CNT-LF         ' GR-CNT-LF                    
CIDMOD        DISPLAY 'GR-OB-IND         ' GR-OB-IND                    
CIDMOD        DISPLAY 'GR-SEX-CODE       ' GR-SEX-CODE                  
CIDMOD        DISPLAY ' '                                               
CIDMOD        DISPLAY ' END OF RECORD '                                 
CIDMOD        DISPLAY ' '.                                              
CIDMOD                                                                  
CIDMOD QD-BYPASS.                                                       
CIDMOD                                                                  
CIDMOD     GO TO SKIP-DISPLAY.                                          
CIDMOD                                                                  
01162      IF  W-CONTROL-OVERRIDE GREATER THAN SPACES                   
01163          IF  W-CO-ACCOUNT EQUAL GR-ACCOUNT                        
01164                  AND                                              
01165              W-CO-CERT EQUAL GR-CERT                              
01166              DISPLAY GR-REIN     ' - GR-REIN'                     
01167              DISPLAY GR-CARRIER  ' - KEY FIELDS'                  
01168              DISPLAY GR-GROUPING                                  
01169              DISPLAY GR-STATE                                     
01170              DISPLAY GR-ACCOUNT                                   
01171              DISPLAY GR-EFF                                       
01172              DISPLAY GR-CERT-NO                                   
01173              DISPLAY GR-REINCO     ' - REINCO '                   
01174              DISPLAY GR-REINCO-SUB ' - REINCO SUB'                
01175              DISPLAY GR-IG         ' IG '                         
01176              DISPLAY GR-APR        ' APR '                        
01177              DISPLAY GR-PMT-FREQ   ' FREQ '                       
01178              DISPLAY GR-LOAN-TERM  ' TERM '                       
01179              DISPLAY GR-AGE        ' AGE '                        
01180              DISPLAY GR-LFTYP      ' TYPE '                       
01181              DISPLAY GR-LF-TERM    ' LF TERM '                    
01182              DISPLAY GR-LF-REMTERM ' LF REMTERM '                 
01183              DISPLAY GR-LF-UP-REMTERM  ' UPREMTERM '              
01184              DISPLAY GR-LFBEN                                     
01185              DISPLAY GR-LFPRM                                     
01186              DISPLAY GR-LFCOM                                     
01187              DISPLAY GR-LFEXP                                     
01188              DISPLAY GR-LFTAX                                     
01189              DISPLAY GRP-LFPRM                                    
01190              DISPLAY GRP-LFCOM                                    
01191              DISPLAY GRP-LFEXP                                    
01192              DISPLAY GRP-LFTAX                                    
01193              DISPLAY GRR-LFPRM     ' GRR-PREM '                   
01194              DISPLAY GRR-LFCOM                                    
01195              DISPLAY GRR-LFEXP                                    
01196              DISPLAY GRR-LFTAX                                    
01197              DISPLAY GRD-LFPRM                                    
01198              DISPLAY GRD-LFCOM                                    
01199              DISPLAY GRS-LFPRM                                    
01200              DISPLAY GRS-LFCOM                                    
01201              DISPLAY GR-MORT-CONTROL ' MORT CONTROL '             
01202              DISPLAY GR-MORT-BASE    ' MORT BASE '                
01203              DISPLAY GR-MORT-TYP     ' MORT TYPE'                 
01204              DISPLAY GR-MORT-AGE     ' MORT AGE '                 
01205              DISPLAY GR-MORT-DATA    ' MORT DATE '                
01206              DISPLAY GR-REM-AMT      ' REM AMT '                  
01207              DISPLAY GR-MO-DEC       ' MO DEC '                   
01208              DISPLAY GR-MORT-FACT    ' FACTOR '                   
01209              DISPLAY GR-RESV         ' RESV '                     
01210              DISPLAY GR-FLAG         ' FLAG '                     
01211              DISPLAY GR-ALT-MORT-BASE  ' ALTER MORT CODE '        
01212              DISPLAY GR-ALT-MORT-TYP                              
01213              DISPLAY GR-ALT-RESV                                  
01214              DISPLAY GR-CNT                                       
01215              DISPLAY GR-CNT-LF                                    
01216              DISPLAY GR-OB-IND         ' OB IND '                 
01217              DISPLAY GR-SEX-CODE       ' SEX CODE '               
01217              DISPLAY '  '.                                        
01222                                                                   
CIDMOD SKIP-DISPLAY.                                                    
01222                                                                   
01223      ADD +1                      TO W-TOTAL-RECORDS.              
01224                                                                   
01225      IF GR-FLAG = 'A' OR 'D'                                      
01226          MOVE ' '                TO GR-FLAG.                      
01227                                                                   
01228      IF GR-MORT-AGE GREATER THAN 95                               
01229          MOVE 95 TO GR-MORT-AGE.                                  
01230                                                                   
01231      IF GR-MORT-AGE = ZERO                                        
01232          MOVE 'E'                TO GR-FLAG.                      
01233                                                                   
01234      IF GR-DIR-CERT-LFBEN NOT NUMERIC  OR                         
01235         GR-DIR-CERT-LFBEN = ZEROS                                 
01236          MOVE GR-LFBEN           TO GR-DIR-CERT-LFBEN.            
01237                                                                   
01251                                                                   
01256      GO TO 0160-CONTINUE-READ.                                    
01257                                                                   
01258  0160-READ-FLIC.                                                  
01259      IF GR-GROUP-PRIME = '351' OR '352' OR '353' OR '354' OR      
01260                          '356' OR '360' OR '340' OR '341'         
01261          GO TO 0160-CONTINUE-READ.                                
01262                                                                   
01263      IF (GR-REIN NOT = 'R') AND                                   
01264         (GR-ENT-DT GREATER 19850531)                              
01265           MOVE 'S350'             TO GR-MORT-CODE                 
01266           GO TO 0160-CONTINUE-READ.                               
01267                                                                   
01268      IF (GR-REIN = 'R') AND                                       
01269         (GR-REINCO = '321' OR '322' OR '323' OR '324')            
01270           GO TO 0160-CONTINUE-READ.                               
01271                                                                   
01272      IF (GR-REIN = 'R') AND                                       
01273         (GR-REINCO NOT = '333')                                   
01274           MOVE 'S350'             TO GR-MORT-CODE                 
01275           GO TO 0160-CONTINUE-READ.                               
01276                                                                   
01277  0160-CONTINUE-READ.                                              
01278                                                                   
01279      IF  GR-MORT-CODE = 'Z000'  OR  'ZERO'  OR  'ZER0'            
01280          GO TO 0290-NET-PAY-BYPASS.                               
01281                                                                   
CIDMOD     IF  GR-YR LESS 00 OR GREATER 99                              
CIDMOD         DISPLAY 'INVALID ISSUE YEAR ', GR-YR, ' RECORD BYPASSED' 
CIDMOD                                                                  
CIDMOD       ADD  +1  TO  ERROR-COUNT                                   
CIDMOD         MOVE 'INVALID ISSUE YEAR    = ' TO DIS-LINE-REASON       
CIDMOD         MOVE GR-YR                      TO  DIS-LINE-REC         
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT                                  
CIDMOD         MOVE 'RECORD BYPASSED         ' TO DIS-LINE-REASON       
CIDMOD         MOVE SPACES                     TO DIS-LINE-REC          
CIDMOD         PERFORM 8600-DISPLAY-PRT THRU                            
CIDMOD               8600-DISPLAY-EXIT                                  
CIDMOD         GO TO 0290-NET-PAY-BYPASS.                               
01121                                                                   
PEMTST*    MOVE 'L010'             TO GR-MORT-CODE

01282      IF DTE-PGM-OPT = '2' OR '3'                                  
01283          MOVE GR-MORT-CODE                   TO GR-ALT-MORT-CODE  
01284          MOVE GR-RESV                        TO GR-ALT-RESV       
01285          IF GR-LF-TERM LESS +121                                  
01286              SET TBL1-NDX TO +1                                   
01287              SEARCH WS-TABLE-FOR-1-10                             
01288                 AT END                                            
01289                    DISPLAY 'INVALID ISSUE YEAR ', GR-CCYY,        
pemuni                      ' RECORD BYPASSED'
01291                    GO TO 0290-NET-PAY-BYPASS                      
01292              WHEN WS-TBL1-CCYY (TBL1-NDX) = GR-CCYY               
01293                 MOVE WS-TBL1-PNTR (TBL1-NDX) TO GR-MORT-CODE      
01294              END-SEARCH                                           
01295          ELSE                                                     
01296              SET TBL2-NDX TO +1                                   
01297              SEARCH WS-TABLE-FOR-11-20                            
01298                 AT END                                            
01299                    DISPLAY 'INVALID ISSUE YEAR ', GR-CCYY,        
pemuni     ' RECORD BYPASSED'
01301                    GO TO 0290-NET-PAY-BYPASS                      
01302              WHEN WS-TBL2-CCYY (TBL2-NDX) = GR-CCYY               
01303                 MOVE WS-TBL2-PNTR (TBL2-NDX) TO GR-MORT-CODE      
01304              END-SEARCH                                           
01305          END-IF
           END-IF
01306                                                                   
01332      MOVE +0                     TO GR-MORT-FACT                  
01333                                     GR-RESV.
01334                                                                   
01335      IF GR-LFTYP = ZERO                                           
01336          GO TO 0290-NET-PAY-BYPASS.                               
01337                                                                   
01338  0165-FIND-CLAS-STATE-ABBR.                                       
01339                                                                   
01340      IF CLAS-INDEXS IS NOT GREATER THAN CLAS-MAXS AND             
01341         CLAS-INDEXS IS NOT EQUAL TO ZERO                          
01342          IF GR-STATE IS EQUAL TO STATE-SUB (CLAS-INDEXS)          
01343          GO TO 0170-FIND-CLAS-L-TYPE.                             
01344                                                                   
01345      MOVE CLAS-STARTS            TO  CLAS-INDEXS.                 
01346                                                                   
01347  0165-STATE-LOOK-UP.                                              
01348                                                                   
01349      IF CLAS-INDEXS IS GREATER THAN CLAS-MAXS OR                  
01350         CLAS-INDEXS IS EQUAL TO ZERO                              
01351          DISPLAY 'STATE RECORD NOT FOUND ' GR-STATE               
01352          GO TO 0170-FIND-CLAS-L-TYPE.                             
01353                                                                   
01354      IF STATE-SUB (CLAS-INDEXS) IS NOT EQUAL TO GR-STATE          
01355          ADD +1                  TO  CLAS-INDEXS                  
01356          GO TO 0165-STATE-LOOK-UP.                                
01357                                                                   
01358  0170-FIND-CLAS-L-TYPE.                                           
01359                                                                   
01360      MOVE CLAS-STARTL TO CLAS-INDEXL.                             
01361                                                                   
01362  0170-FIND-LIFE-LOOP.                                             
01363      IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        
01364          GO TO 0180-FIND-LIFE-ABORT.                              
01365                                                                   
01366      IF GR-LFTYP NOT = CLAS-I-BEN (CLAS-INDEXL)                   
01367          ADD +1 TO CLAS-INDEXL                                    
01368          GO TO 0170-FIND-LIFE-LOOP.                               
01369                                                                   
01370      GO TO 0190-FOUND-LIFE.                                       
01371                                                                   
01372  0180-FIND-LIFE-ABORT.                                            
01373      DISPLAY LIFE-OVERRIDE-L6 ' BENEFIT ' GR-LFTYP ' NOT IN TABLE'
01374                                                                   
01375      MOVE 0401 TO WS-RETURN-CODE                                  
01376      MOVE ' INVALID BENEFIT ' TO WS-ABEND-MESSAGE                 
01153                                                                   
CIDMOD     ADD  +1  TO  ERROR-COUNT                                     
CIDMOD     MOVE 'BENEFIT NOT IN TABLE  = ' TO DIS-LINE-REASON           
CIDMOD     MOVE GR-LFTYP                   TO  DIS-LINE-REC             
CIDMOD     PERFORM 8600-DISPLAY-PRT THRU                                
CIDMOD           8600-DISPLAY-EXIT                                      
CIDMOD     MOVE  SPACES                    TO DIS-LINE-REASON           
CIDMOD     MOVE  SPACES                    TO DIS-LINE-REC              
CIDMOD     PERFORM 8600-DISPLAY-PRT THRU                                
CIDMOD           8600-DISPLAY-EXIT                                      
CIDMOD                                                                  
01377      GO TO ABEND-PGM.                                             
01378                                                                   
01379  0190-FOUND-LIFE.                                                 
01380      IF GR-FLAG NOT = ' '                                         
01381          GO TO 0290-NET-PAY-BYPASS.                               
01382                                                                   
01383      IF GR-REIN NOT = 'R'                                         
01387          GO TO 0270-NO-OVERRIDE
062104     END-IF.
01388                                                                   
01389      IF REIN-OPEN-SW = ' '                                        
01390          MOVE HIGH-VALUE TO REIN-SEARCH-TABLE                     
01391          MOVE +0                 TO REIN-INDEX                    
01392          OPEN INPUT REIN-TBL-FILE                                 
01393          MOVE 'X'                TO REIN-OPEN-SW                  
01394          IF REIN-FILE-STATUS  = '00' OR  '97'                     
01395             NEXT SENTENCE                                         
01396          ELSE                                                     
01397             MOVE 'ERROR ON ERRTBL, OPEN ' TO WS-ABEND-MESSAGE     
01398             MOVE REIN-FILE-STATUS TO WS-ABEND-FILE-STATUS         
01399             GO TO ABEND-PGM                                       
01400      ELSE                                                         
01401          MOVE +1                 TO REIN-INDEX                    
01402          GO TO 0210-CHECK-REIN-TABLE.                             
01403                                                                   
01404  0200-READ-REIN-TABLE.                                            
01405      READ REIN-TBL-FILE.                                          
01406                                                                   
01407      IF REIN-FILE-STATUS = '10'                                   
01408          MOVE +1                 TO REIN-INDEX                    
01409          GO TO 0210-CHECK-REIN-TABLE.                             
01410                                                                   
01411      IF REIN-FILE-STATUS NOT = ZEROS                              
01412         MOVE REIN-FILE-STATUS TO WS-ABEND-FILE-STATUS             
01413         MOVE ' ERROR ON ERRTBL, READ ' TO WS-ABEND-MESSAGE        
01414         GO TO ABEND-PGM.                                          
01415                                                                   
01416      IF RE-CODE NOT = 'B'                                         
01417          GO TO 0200-READ-REIN-TABLE.                              
01418                                                                   
01419      IF RE-MORT-CODE = SPACES                                     
01420          GO TO 0200-READ-REIN-TABLE.                              
01421                                                                   
01422 * LOADING MORTALITY EXCEPTIONS ONLY.                              
01423      ADD +1 TO REIN-INDEX.                                        
01424                                                                   
01425      IF REIN-INDEX GREATER THAN +299                              
01426         MOVE 0201 TO WS-RETURN-CODE                               
01427         MOVE ' REIN TABLE OVERFLOW - ECS080 ABORTING '            
01428              TO WS-ABEND-MESSAGE                                  
01429         GO TO ABEND-PGM.                                          
01430                                                                   
01431      MOVE RE-COMPANY            TO REIN-SRCH-CMPY (REIN-INDEX).   
01432      MOVE RE-MORT-CODE          TO REIN-SRCH-TBLE (REIN-INDEX).   
01433                                                                   
01434      GO TO 0200-READ-REIN-TABLE.                                  
01435                                                                   
01436  0210-CHECK-REIN-TABLE.                                           
01437      IF REIN-SRCH-CMPY (REIN-INDEX) GREATER THAN GR-REIN-COMP     
01438          GO TO 0220-CK-ACCT-OVERRIDE.                             
01439                                                                   
01440      IF REIN-SRCH-CMPY (REIN-INDEX) NOT = GR-REIN-COMP            
01441          ADD +1 TO REIN-INDEX                                     
01442          GO TO 0210-CHECK-REIN-TABLE.                             
01443                                                                   
01444      IF DTE-PGM-OPT = '2' OR '3'                                  
01445          MOVE REIN-SRCH-TBLE (REIN-INDEX) TO GR-ALT-MORT-CODE     
01446      ELSE                                                         
01447          MOVE REIN-SRCH-TBLE (REIN-INDEX) TO GR-MORT-CODE.        
01448                                                                   
01454      GO TO 0220-CK-ACCT-OVERRIDE.                             
01455                                                                   
01467  0220-CK-ACCT-OVERRIDE.                                           
01468      MOVE GR-CARRIER           TO ACC-SR-CARR.                    
01469      MOVE GR-GROUPING          TO ACC-SR-GROUP.                   
01470      MOVE GR-STATE             TO ACC-SR-ST.                      
01471      MOVE GR-ACCOUNT           TO ACC-SR-NO.                      
01472                                                                   
01473  0230-ACC-SR-LOOP.                                                
01474      IF ACC-SRA = AM-CONTROL-A                                    
01475          GO TO 0250-CK-ACC-EFF-DTE.                               
01476                                                                   
01477      IF ACC-SRA LESS AM-CONTROL-A                                 
01478          GO TO 0270-NO-OVERRIDE.                                  
01479                                                                   
01480  0240-READ-ACC-AGAIN.                                             
01481      PERFORM 0900-READ-ACCT-MSTR THRU 0999-READ-ACCT-MSTR-EX.     
01482                                                                   
01483      GO TO 0230-ACC-SR-LOOP.                                      
01484                                                                   
01485  0250-CK-ACC-EFF-DTE.                                             
01486      IF GR-EFF LESS AM-EXPIRE-DT                                  
01487          GO TO 0260-CK-ACC-MORT.                                  
01488                                                                   
01489      GO TO 0240-READ-ACC-AGAIN.                                   
01490                                                                   
01491  0260-CK-ACC-MORT.                                                
01492      IF AM-REI-MORT = SPACES                                      
01493          GO TO 0270-NO-OVERRIDE.                                  
01494                                                                   
01495      IF DTE-PGM-OPT = '2' OR '3'                                  
01496         MOVE AM-REI-MORT TO GR-ALT-MORT-CODE                      
01497      ELSE                                                         
01498         MOVE AM-REI-MORT TO GR-MORT-CODE.                         
01499                                                                   
01500                                                                   
01501  0270-NO-OVERRIDE.                                                
01502                                                                   
01503      MOVE GR-MORT-CONTROL       TO WRK-GR-MORT-CTL.               
01504      MOVE ZEROS                 TO WRK-GR-RESV.                   
01505      MOVE 'N'                   TO BALLOON-SW.                    
01506      MOVE ZERO                  TO WS-ALT-BENEFIT-AMT             
01507                                    W-PVFB-ALTERNATE               
01508                                    W-PVFB.                        
01509                                                                   
01510      PERFORM 0300-CALCULATE-RESV THRU 0399-XIT.                   
01511                                                                   
01512      MOVE WRK-GR-MORT-CTL       TO GR-MORT-CONTROL.               
01513      MOVE WRK-GR-RESV           TO GR-RESV
121610     IF DTE-PGM-OPT NOT = '2' AND '3'
121610        MOVE GR-RESV            TO GR-UNLD-STAT-MORT-RESV
121610     END-IF
           
111721     IF GR-MORT-CODE (1:3) = 'L01' OR 'D01' or 'D41'
091307        EVALUATE GR-STATE
112414*          WHEN 'AK'
112414*             COMPUTE GR-RESV = GR-RESV * 1.075
082420           WHEN 'AL'
082420              COMPUTE GR-RESV = GR-RESV * 1.05
111412*          WHEN 'AR'
111412*             COMPUTE GR-RESV = GR-RESV * 1.00
111412*          WHEN 'CT'
111412*             COMPUTE GR-RESV = GR-RESV * 1.00
111412*          WHEN 'GU'
111412*             COMPUTE GR-RESV = GR-RESV * 1.00
111412*          WHEN 'HI'
111412*             COMPUTE GR-RESV = GR-RESV * 1.00
111412*          WHEN 'KY'
111412*             COMPUTE GR-RESV = GR-RESV * 1.00
102919           WHEN 'LA'
102919              COMPUTE GR-RESV = GR-RESV * 1.10
111412*          WHEN 'MD'
111412*             COMPUTE GR-RESV = GR-RESV * 1.00
082420           WHEN 'MS'
112221              COMPUTE GR-RESV = GR-RESV * 1.20
082420*          WHEN 'MP'
082420*             COMPUTE GR-RESV = GR-RESV * 1.30
112414           WHEN 'MT'
112414              COMPUTE GR-RESV = GR-RESV * 1.05
112414*          WHEN 'NE'
112414*             COMPUTE GR-RESV = GR-RESV * 1.20
112414           WHEN 'SC'
111416              COMPUTE GR-RESV = GR-RESV * 1.50
112414           WHEN 'SD'
112221              COMPUTE GR-RESV = GR-RESV * 1.30
112414           WHEN 'WY'
112818              COMPUTE GR-RESV = GR-RESV * 2.40
091307        END-EVALUATE
091307     END-IF

122811     IF (GR-STATE = 'CT')
122811        AND (GR-EFF > 20081231)
111721        AND (GR-MORT-CODE (1:3) = 'L68' or 'D68' or 'D48')
061213        COMPUTE GR-RESV = GR-RESV * 1.95
122811     END-IF

113017     IF (GR-STATE = 'NE')
111721        AND (GR-MORT-CODE (1:3) = 'L01' OR 'D01' OR 'L14' OR
111721                        'D41')
112221        COMPUTE GR-RESV = GR-RESV * 1.25
113017     END-IF

091307     IF (GR-STATE = 'WY')
091307        AND (GR-MORT-CODE (1:3) = 'L14')
091307        COMPUTE GR-RESV = GR-RESV * 1.22
091307     END-IF

121610     IF DTE-CLIENT = 'CID'
121610        IF DTE-PGM-OPT = '2' OR '3'
PEMTST*          move gr-resv          to gr-orig-fit-resv
121610           COMPUTE WS-GRS-LFTAX ROUNDED =
121610              (GRS-LFPRM / GR-LFPRM) * GR-LFTAX
121610           COMPUTE WS-NET-UEP-RESERVE = GRS-LFPRM - GRS-LFCOM -
121610              WS-GRS-LFTAX
121610           IF GR-UNLD-STAT-MORT-RESV < GR-RESV
121610              MOVE GR-ALT-MORT-CODE
121610                                 TO GR-MORT-CODE               
121610              MOVE GR-UNLD-STAT-MORT-RESV
121610                                 TO GR-RESV
121610           END-IF
121610           IF WS-NET-UEP-RESERVE > GR-RESV
121610              MOVE WS-NET-UEP-RESERVE
121610                                 TO GR-RESV
121610              MOVE 'XXXX'        TO GR-MORT-CODE
121610           END-IF
121610        END-IF
121610     END-IF

01520      IF GR-REIN = 'P'                                             
01521         ADD GR-RESV TO TOT-GROSS
100220        add gr-rem-amt to tot-inforce
01522      ELSE                                                         
01523         ADD GR-RESV TO TOT-REIN.                                  
01524                                                                   
01525      IF DTE-PGM-OPT = '2' OR '3'                                  
01526          GO TO 0290-NET-PAY-BYPASS.                               
01527                                                                   
01528      IF GR-ALT-MORT-CODE = SPACES                                 
01529          GO TO 0280-BYPASS-PROCESS.                               
01530                                                                   
01531 ***************************************************************   
01532 *  IF THE BASE OF THE TWO MORTALITY CODES ARE THE SAME THE    *   
01533 *  SAME RESULTS SHOULD RESULT.  THEREFORE RESULTS OF THE ABOVE*   
01534 *  PROCESS IS DUPLICATED FOR THE ALTERNATE.                   *   
01535 ***************************************************************   
01536      IF  GR-ALT-MORT-BASE = GR-MORT-BASE                          
01537          MOVE GR-RESV      TO GR-ALT-RESV                         
01538          MOVE W-PVFB       TO W-PVFB-ALTERNATE                    
01539          MOVE GR-MORT-CODE TO GR-ALT-MORT-CODE                    
01540          GO TO 0275-BYPASS-ALTERNATE.                             
01541                                                                   
01542      MOVE 'Y'              TO ALT-SW.                             
01543      MOVE GR-ALT-MORT-CODE TO WRK-GR-MORT-CODE.                   
01544      MOVE ZEROS            TO WRK-GR-RESV.                        
01545      MOVE 'N'              TO BALLOON-SW.                         
01546      MOVE ZERO             TO WS-ALT-BENEFIT-AMT.                 
01547                                                                   
01548      PERFORM 0300-CALCULATE-RESV THRU 0399-XIT.                   
01549                                                                   
01550      MOVE WRK-GR-MORT-CODE TO GR-ALT-MORT-CODE.                   
01551      MOVE WRK-GR-RESV      TO GR-ALT-RESV.                        
01552                                                                   
01553  0275-BYPASS-ALTERNATE.                                           
01554                                                                   
01555      IF GR-REIN = 'P'                                             
01556         ADD GR-ALT-RESV TO ALT-TOT-GROSS                          
01557      ELSE                                                         
01558         ADD GR-ALT-RESV TO ALT-TOT-REIN.                          
01559                                                                   
01560  0280-BYPASS-PROCESS.                                             
01561                                                                   
01562      IF  CLAS-I-EP (CLAS-INDEXL) NOT EQUAL 'N'                    
01563          GO TO 0290-NET-PAY-BYPASS.                               
01564                                                                   
01569  0290-NET-PAY-BYPASS.                                             
01570                                                                   
01571      PERFORM 0475-WRITE-GAAP-RECORD THRU 0499-XIT.                
01572                                                                   
01573      MOVE 'N'                    TO ALT-SW.                       
01574                                                                   
01575      GO TO 0160-READ-GAAP-FILE.                                   
01576  EJECT                                                            
01577  0300-CALCULATE-RESV.                                             
01578                                                                   
01579      MOVE CLAS-STARTM TO CLAS-INDEXM.                             
01580                                                                   
01581  0310-FIND-MORT-TYPE.                                             
01582      IF CLAS-INDEXM GREATER THAN CLAS-MAXM                        
01583             OR                                                    
01584         WRK-GR-MORT-CODE LESS THAN CLAS-MORT-CODE (CLAS-INDEXM)   
01585         GO TO 0320-FIND-MORT-DEFAULT.                             
01586                                                                   
01587      IF WRK-GR-MORT-CODE NOT = CLAS-MORT-CODE (CLAS-INDEXM)       
01588          ADD +1 TO CLAS-INDEXM                                    
01589          GO TO 0310-FIND-MORT-TYPE.                               
01590                                                                   
01591      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
01592         IF CLAS-MORT-J-CODE (CLAS-INDEXM) = 'J'                   
01593                OR                                                 
01594            CLAS-MORT-J-FACT (CLAS-INDEXM) GREATER THAN +1.0000    
01595            GO TO 0330-FOUND-MORT                                  
01596         ELSE                                                      
01597            ADD +1 TO CLAS-INDEXM                                  
01598            GO TO 0310-FIND-MORT-TYPE                              
01599      ELSE                                                         
01600         IF CLAS-MORT-J-CODE (CLAS-INDEXM) = 'S'                   
01601            GO TO 0330-FOUND-MORT                                  
01602         ELSE                                                      
01603            ADD +1 TO CLAS-INDEXM                                  
01604            GO TO 0310-FIND-MORT-TYPE.                             
01605                                                                   
01606      GO TO 0330-FOUND-MORT.                                       
01607                                                                   
01608  0320-FIND-MORT-DEFAULT.                                          
01609                                                                   
01610      IF WRK-GR-MORT-CODE = 'Z000' OR 'ZERO' OR 'ZER0'             
01611         NEXT SENTENCE                                             
01612      ELSE                                                         
01613         MOVE 'A' TO GR-FLAG.                                      
01614                                                                   
01615      GO TO 0399-XIT.                                              
01616                                                                   
01617  0330-FOUND-MORT.                                                 
01618                                                                   
01619      IF WRK-GR-MORT-CODE = 'Z000' OR 'ZERO' OR 'ZER0'             
01620          GO TO 0399-XIT.                                          
01621                                                                   
01622      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
01623             AND                                                   
01624         CLAS-MORT-J-CODE (CLAS-INDEXM) = 'J'                      
01625          MOVE 'J'                TO WRK-GR-MORT-TYP               
01626      ELSE                                                         
01627          MOVE 'S'                TO WRK-GR-MORT-TYP.              
01628                                                                   
01633                                                                   
01634      PERFORM 0800-GET-RATE THRU 0899-GET-RATE-X.                  
01635                                                                   
01636      MOVE GR-FLAG TO SAVE-GR-FLAG.                                
01637                                                                   
01638      MOVE CLAS-MORT-J-FACT (CLAS-INDEXM) TO WS-FACT.              
01639                                                                   
01648      IF GR-FLAG = 'D'                                             
01649          GO TO 0399-XIT.                                          
01650                                                                   
CIDMOD     IF GR-LFTYP = 'QD'                                           
CIDMOD        GO TO 0350-CALC-NET-PAY.                                  
CIDMOD                                                                  
01659      IF CLAS-I-EP (CLAS-INDEXL) = 'N'                             
111913        and gr-apr > zeros
01661          GO TO 0350-CALC-NET-PAY
062104     END-IF.                              
01665                                                                   
01673      PERFORM 0400-STD-MORTALITY THRU 0449-STD-EXIT.           
01674                                                                   
01679      MOVE WS-MORT-FACTOR TO GR-MORT-FACT.                         
01680                                                                   
01681      IF CLAS-I-EP (CLAS-INDEXL) = 'T'                             
01682          COMPUTE TEX-FACT-9 =                                     
01683              (GR-LF-TERM + GR-PMT-FREQ) / (GR-LF-TERM + +1)       
01684          COMPUTE GR-MORT-FACT ROUNDED =                           
01685              GR-MORT-FACT * TEX-FACT-9.                           
01686                                                                   
01687      IF NOT GR-SUMMARY-REC                                        
01688          MOVE +0 TO GR-APR.                                       
01689                                                                   
01690      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
01691          IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                      
01692              COMPUTE WRK-GR-RESV ROUNDED =                        
01693                  (GR-MO-DEC / +100) * (GR-MORT-FACT * WS-FACT)    
01694          ELSE                                                     
01695              COMPUTE WRK-GR-RESV ROUNDED =                        
01696                  (GR-REM-AMT / +1000) * (GR-MORT-FACT * WS-FACT)  
01697      ELSE                                                         
01698          IF CLAS-I-RL-AH (CLAS-INDEXL) = 'R'                      
01699              COMPUTE WRK-GR-RESV ROUNDED =                        
01700                  (GR-MO-DEC / +100) * GR-MORT-FACT                
01701          ELSE                                                     
01702              COMPUTE WRK-GR-RESV ROUNDED =                        
01703                  (GR-REM-AMT / +1000) * GR-MORT-FACT.             
01704                                                                   
01705      IF CLAS-I-EP (CLAS-INDEXL) = 'B' OR 'K' OR 'L'               
01706          IF NOT BALLOON-PASS                                      
01707              MOVE 'Y'              TO BALLOON-SW                  
01708              PERFORM 0400-STD-MORTALITY THRU 0449-STD-EXIT        
01709              COMPUTE WS-ALT-BENEFIT-AMT = GR-REM-AMT -            
01710                                       (GR-LF-REMTERM * GR-MO-DEC) 
01711              IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                  
01712                COMPUTE WRK-GR-RESV ROUNDED = WRK-GR-RESV +        
01713                      (WS-ALT-BENEFIT-AMT / +1000) *               
01714                      (WS-MORT-FACTOR * WS-FACT)                   
01715               ELSE                                                
01716                COMPUTE WRK-GR-RESV ROUNDED = WRK-GR-RESV +        
01717                   (WS-ALT-BENEFIT-AMT / +1000) * WS-MORT-FACTOR.  
01718                                                                   
01745      GO TO 0399-XIT.                                         
01746                                                                   
01766  0350-CALC-NET-PAY.                                               
01767                                                                   
01768      ADD +1                      TO W-NUMBER-NET-PAY.             
01769                                                                   
CIDMOD     IF GR-LFTYP = 'QD'                                           
CIDMOD        MOVE +15.0               TO GR-APR.                       
CIDMOD                                                                  
01770      IF GR-APR = ZERO                                             
01771          MOVE 'Z'                TO GR-FLAG                       
01772          GO TO 0399-XIT.                                          
01773                                                                   
01774      IF  GR-APR LESS THAN +1.0                                    
01775          DISPLAY GR-CONTROL ' - NET PAY GAAP RECORD WITH APR '    
01776              'LESS THAN +1.0'                                     
01777          ADD +1                  TO W-APR-LESS-THAN-1.            
01778                                                                   
01779      MOVE +0                     TO WK-CX    WK-DX                
01780                                     WK-RESV  WK-RSV               
01781                                     NP-MORT-TOT                   
01782                                     NP-AVG-INFORCE                
01783                                                                   
01784      MOVE 'C'                    TO WRK-GR-MORT-TYP.              
01785                                                                   
01786      MOVE WRK-GR-MORT-AGE        TO X2.                           
01787                                                                   
01788 ******  NOTE - AGE ADJUSTED BECAUSE TABLE STARTS AT ZERO NOT      
01789 ******         BECAUSE AN AGE OF 1 GREATER IS NEEDED IN CALC.     
01790      COMPUTE X2 = X2 + +1.                                        
01791      MOVE GR-LF-REMTERM             TO X3.                        
01792      DIVIDE GR-LF-REMTERM BY +12 GIVING                           
01793                    NP-INT-1 REMAINDER NP-INT-2.                   
01794                                                                   
01795      MOVE GR-APR                 TO NP-APR.                       
01796      MOVE GR-LF-TERM             TO NP-ORIG  NP-CAP.              
01797      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)                          
01798                                  TO NP-OPT.                       
01799                                                                   
01800      MOVE GR-LF-EXPIRE-DATE      TO  DC-GREG-DATE-CYMD.           
01801      MOVE 'L'                    TO  DC-OPTION-CODE.              
01802      PERFORM 2900-DATE-CONVERSION THRU 2900-EXIT.                 
01803      IF NO-CONVERSION-ERROR                                       
01804          MOVE DC-BIN-DATE-1      TO  WS-EXPIRE-DT                 
01805      ELSE                                                         
01806          MOVE LOW-VALUES         TO  WS-EXPIRE-DT.                
01807                                                                   
01808      MOVE GR-EFF                 TO  DC-GREG-DATE-CYMD.           
01809      MOVE 'L'                    TO  DC-OPTION-CODE.              
01810      PERFORM 2900-DATE-CONVERSION THRU 2900-EXIT.                 
01811      IF NO-CONVERSION-ERROR                                       
01812          MOVE DC-BIN-DATE-1      TO  CP-CERT-EFF-DT               
01813      ELSE                                                         
01814          MOVE LOW-VALUES         TO  CP-CERT-EFF-DT.              
01815                                                                   
01816      MOVE WS-EXPIRE-DT           TO  DC-BIN-DATE-1.               
01817      MOVE '6'                    TO  DC-OPTION-CODE.              
01818      MOVE +0                     TO  DC-ELAPSED-DAYS              
01819                                      DC-ODD-DAYS-OVER.            
01820      COMPUTE DC-ELAPSED-MONTHS = (GR-LF-TERM - 1) * -1.           
01821      PERFORM 2900-DATE-CONVERSION THRU 2900-EXIT.                 
01822      IF NO-CONVERSION-ERROR                                       
01823          MOVE DC-BIN-DATE-2      TO  CP-FIRST-PAY-DATE            
01824      ELSE                                                         
01825          MOVE LOW-VALUES         TO  CP-FIRST-PAY-DATE.           
01826                                                                   
01827      MOVE GR-APR                 TO  CP-LOAN-APR.                 
01828      MOVE GR-LF-TERM             TO  CP-ORIGINAL-TERM.            
01829      MOVE GR-LOAN-TERM           TO  CP-LOAN-TERM.                
01830      MOVE STATE-ABBR       (CLAS-INDEXS)                          
01831                                  TO  CP-STATE-STD-ABBRV.          
01832      MOVE CLAS-I-CALC-TYPE (CLAS-INDEXL)                          
01833                                  TO  CP-SPECIAL-CALC-CD.          
01834      MOVE CLAS-I-RL-AH     (CLAS-INDEXL)                          
01835                                  TO  CP-BENEFIT-TYPE.             
01836      MOVE CLAS-I-EP        (CLAS-INDEXL)                          
01837                                  TO  CP-EARNING-METHOD.           
01838                                                                   
01839      PERFORM 2300-MATCH-NET-PAY THRU 2300-EXIT.                   
01840                                                                   
01841      IF NP-INT-2 = ZERO                                           
01842          GO TO 0355-CX-TOTALS.                                    
01843                                                                   
01844      COMPUTE NP-LOOP-END = (12 * NP-INT-1) + 1.                   
01845      GO TO 0355-12-MONTH-LOOP-START.                              
01846                                                                   
01847                                                                   
01848  0355-CX-TOTALS.                                                  
01849      IF X3 GREATER 12                                             
01850           COMPUTE NP-LOOP-END = X3 - 11                           
01851        ELSE                                                       
01852            MOVE +1 TO NP-LOOP-END.                                
01853                                                                   
01854  0355-12-MONTH-LOOP-START.                                        
01855      MOVE +0                     TO WK-RESV.                      
01856                                                                   
01857      PERFORM 0360-1-YEAR-LOOP THRU 0360-LOOP-END                  
01858          VARYING X3 FROM X3 BY -1                                 
01859              UNTIL X3 LESS NP-LOOP-END.                           
01860                                                                   
01861      COMPUTE NP-AVG-INFORCE ROUNDED =                             
01862                                  WK-RESV / +12.                   
01863      MOVE RF-CX-FACTOR (X2)      TO WK-CX.                        
01864                                                                   
01865      COMPUTE NP-MORT-TOT = NP-MORT-TOT +                          
01866                            (NP-AVG-INFORCE * WK-CX).              
01867                                                                   
01868      COMPUTE X2 = X2 + 1.                                         
01869                                                                   
01870      IF NP-LOOP-END = +1                                          
01871          GO TO 0370-DX-DIVIDE                                     
01872       ELSE                                                        
01873           GO TO 0355-CX-TOTALS.                                   
01874                                                                   
01875  0360-1-YEAR-LOOP.                                                
01876                                                                   
01877      MOVE X3                     TO  CP-REMAINING-TERM.           
01878      MOVE +0                     TO  CP-REMAMT-FACTOR.            
01879                                                                   
01880      IF  W-NET-PAY-MATCHED                                        
01881              AND                                                  
01882          W-GNPT-FACTOR (X3) NUMERIC                               
01883          MOVE W-GNPT-FACTOR (X3) TO  CP-REMAMT-FACTOR             
01884      ELSE                                                         
01885          MOVE GR-LFBEN           TO  CP-ORIGINAL-BENEFIT          
01886          CALL 'ELRAMTX' USING CALCULATION-PASS-AREA               
01887          MOVE CP-REMAMT-FACTOR   TO  W-GNPT-FACTOR (X3).          
01888                                                                   
01889      IF CP-STATE-STD-ABBRV EQUAL 'NC'                             
01890                AND                                                
CIDMOD        (DTE-CLIENT NOT = 'CID')                        
01892                AND                                                
01893         CP-EARN-AS-NET-PAY                                        
01894          IF GR-EFF GREATER THAN 19931231                          
01895              COMPUTE NP-NC-TOTAL-BEN =                            
01896                      GR-LFBEN + (GR-MO-DEC * 3)                   
01897              COMPUTE WK-RSV ROUNDED =                             
01898                  (NP-NC-TOTAL-BEN / 1000) * CP-REMAMT-FACTOR      
01899          ELSE                                                     
01900              COMPUTE WK-RSV ROUNDED =                             
01901                  (GR-LFBEN / 1000) * CP-REMAMT-FACTOR             
01902      ELSE                                                         
01903          COMPUTE WK-RSV ROUNDED =                                 
01904              (GR-LFBEN / 1000) * CP-REMAMT-FACTOR.                
01905                                                                   
01906      COMPUTE WK-RESV = WK-RESV + WK-RSV.                          
01907                                                                   
01908      IF  W-CONTROL-OVERRIDE GREATER THAN SPACES                   
01909              AND                                                  
01910          W-CO-ACCOUNT EQUAL GR-ACCOUNT                            
01911              AND                                                  
01912          W-CO-CERT EQUAL GR-CERT                                  
01913          MOVE WK-RSV             TO W-EDIT-10                     
01914          DISPLAY 'RESERVE-LET - ' W-EDIT-10                       
01915          MOVE NP-APR             TO W-EDIT-5                      
01916          DISPLAY 'APR         - ' W-EDIT-5                        
01917          MOVE NP-ORIG            TO W-EDIT                        
01918          DISPLAY 'ORG-TERM    - ' W-EDIT                          
01919          MOVE NP-CAP             TO W-EDIT                        
01920          DISPLAY 'CAP-TERM    - ' W-EDIT                          
01921          MOVE NP-REM             TO W-EDIT                        
01922          DISPLAY 'REM-TERM    - ' W-EDIT                          
01923          DISPLAY 'OPT         - ' NP-OPT                          
01924          MOVE NP-FACTOR          TO W-EDIT-5                      
01925          DISPLAY 'FACTOR      -'  W-EDIT-5.                       
01926                                                                   
01927  0360-LOOP-END.                                                   
01928      EXIT.                                                        
01929                                                                   
01930  0370-DX-DIVIDE.                                                  
01931      MOVE WRK-GR-MORT-AGE        TO X2.                           
01932      COMPUTE X2 = X2 + +1.                                        
01933      MOVE RF-DX-FACTOR (X2) TO WK-DX.                             
01934                                                                   
01935      IF WK-DX = ZERO                                              
01936          GO TO 0399-XIT.                                          
01937                                                                   
01938      MOVE WRK-GR-MORT-AGE        TO X2.                           
01939      COMPUTE X2 = X2 + +2.                                        
01940      MOVE RF-DX-FACTOR (X2) TO WK-DX2.                            
01941      IF WK-DX2 = ZERO                                             
01942          GO TO 0399-XIT.                                          
01943                                                                   
01944      COMPUTE NP-INT-3 = GR-LF-REMTERM - (12 * NP-INT-1)           
01945                                                                   
01946      IF NP-INT-3 = +0                                             
01947         MOVE +12 TO NP-INT-3.                                     
01948                                                                   
01949      IF  REGULAR-MORT                                             
01950          COMPUTE W-PVFB ROUNDED = NP-MORT-TOT *                   
01951              (((NP-INT-3 - .50000) / (+12.000 * WK-DX)) +         
01952              ((+12.50000 - NP-INT-3) / (+12.000 * WK-DX2)))       
01953      ELSE                                                         
01954          COMPUTE W-PVFB-ALTERNATE ROUNDED = NP-MORT-TOT *         
01955              (((NP-INT-3 - .50000) / (+12.000 * WK-DX)) +         
01956              ((+12.50000 - NP-INT-3) / (+12.000 * WK-DX2))).      
01957                                                                   
01958      IF  REGULAR-MORT                                             
01959          IF  CLAS-I-JOINT (CLAS-INDEXL) = 'J'                     
01960              COMPUTE W-WORK-AREA ROUNDED                          
01961                  = W-PVFB * RF-RESV-ADJ * WS-FACT                 
01962          ELSE                                                     
01963              COMPUTE W-WORK-AREA ROUNDED                          
01964                  = W-PVFB * RF-RESV-ADJ                           
01965      ELSE                                                         
01966          IF  CLAS-I-JOINT (CLAS-INDEXL) = 'J'                     
01967              COMPUTE W-WORK-AREA ROUNDED                          
01968                  = W-PVFB-ALTERNATE * RF-RESV-ADJ * WS-FACT       
01969          ELSE                                                     
01970              COMPUTE W-WORK-AREA ROUNDED                          
01971                  = W-PVFB-ALTERNATE * RF-RESV-ADJ.                
01972                                                                   
01977      COMPUTE WRK-GR-RESV ROUNDED = W-WORK-AREA.                   
01978      MOVE SPACE TO GR-FLAG.                                       
01979                                                                   
01980  0399-XIT.                                                        
01981      EXIT.                                                        
01982  EJECT                                                            
01983  0400-STD-MORTALITY.                                              
01984      MOVE +0                     TO RSVR.                         
01985      MOVE GR-LF-REMTERM          TO CTA.                          
01986      MOVE WRK-GR-MORT-AGE        TO CTT.                          
01987                                                                   
01988      COMPUTE CTX = WRK-GR-MORT-AGE + (GR-LF-REMTERM / 12).        
01989                                                                   
01990      IF CTX GREATER 98                                            
01991           MOVE ZERO              TO WS-MORT-FACTOR                
01992           GO TO 0449-STD-EXIT.                                    
01993                                                                   
01994      MOVE GR-LF-REMTERM          TO P.                            
01995                                                                   
01996      IF GR-LF-REMTERM GREATER 12                                  
01997          PERFORM 0445-FIND-P THRU 0445-EXIT.                      
01998                                                                   
01999      COMPUTE R = GR-LF-REMTERM / 12.                              
02000                                                                   
02001      IF P = 12                                                    
02002          SUBTRACT 2 FROM R                                        
02003        ELSE                                                       
02004          SUBTRACT 1 FROM R.                                       
02005                                                                   
02006      COMPUTE V = 1.0 / RF-INT-ADJ.                                
02007                                                                   
02008      MOVE WRK-GR-MORT-AGE        TO XX.                           
02009                                                                   
02010      COMPUTE TX = WRK-GR-MORT-AGE + +1.                           
02011                                                                   
02012      MOVE RF-DX-FACTOR (TX)      TO DX.                           
02013                                                                   
02014      COMPUTE CTS = WRK-GR-MORT-AGE + +2.                          
02015                                                                   
02016      COMPUTE CX = (V * DX) - RF-DX-FACTOR (CTS).                  
02017                                                                   
02018      IF (CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P') OR              
02019         (BALLOON-PASS)                                            
02020            GO TO 0430-LEVEL-RESERVE.                              
02021                                                                   
02022  EJECT                                                            
02023  0410-DECREASING-RESERVE.                                         
02024      COMPUTE ANSB = (100 * GR-LF-REMTERM) * P.                    
02025      COMPUTE ANSD = CX / (DX * 12).                               
02026      COMPUTE ANSC = P * 50.                                       
02027                                                                   
02028      IF LX-PLUS                                                   
02029          COMPUTE CTR = P + 1                                      
02030        ELSE                                                       
02031          COMPUTE CTR = P - 1.                                     
02032                                                                   
02033      COMPUTE ANSC = ANSC * CTR.                                   
02034      COMPUTE ANSB = ANSB - ANSC.                                  
02035      COMPUTE RSVR ROUNDED = ANSB * ANSD.                          
02036                                                                   
02037      IF GR-LF-REMTERM LESS THAN 13                                
02038          GO TO 0440-END-RSVR-CALC.                                
02039                                                                   
02040      MOVE +0 TO T.                                                
02041                                                                   
02042  0415-RSVR-ADD-LOOP.                                              
02043      COMPUTE CTR = XX + T + 2.                                    
02044      COMPUTE CTS = XX + T + 3.                                    
02045      COMPUTE ANSB = ((V * RF-DX-FACTOR (CTR)) -                   
02046                       RF-DX-FACTOR (CTS)).                        
02047      COMPUTE ANSB = ((V * RF-DX-FACTOR (CTR)) -                   
02048                       RF-DX-FACTOR (CTS)) / DX.                   
02049  0415-LOOP-END.                                                   
02050      COMPUTE CTR = T * 12.                                        
02051      COMPUTE ANSD = ((GR-LF-REMTERM * 100) -                      
02052                     ((P + CTR + 5.50) * 100)) * ANSB.             
02053      COMPUTE RSVR = RSVR + ANSD.                                  
02054                                                                   
02055      ADD 1 TO T.                                                  
02056                                                                   
02057      IF T NOT GREATER R                                           
02058          GO TO 0415-RSVR-ADD-LOOP.                                
02059                                                                   
02060      GO TO 0440-END-RSVR-CALC.                                    
02061                                                                   
02062  EJECT                                                            
02063  0430-LEVEL-RESERVE.                                              
02064      COMPUTE ANSB = CX / DX.                                      
02065      COMPUTE ANSC = 1000 * P.                                     
02066      COMPUTE ANSD = ANSC / 12.                                    
02067      COMPUTE RSVR ROUNDED = ANSB * ANSD.                          
02068                                                                   
02069      IF GR-LF-REMTERM LESS THAN 13                                
02070          GO TO 0440-END-RSVR-CALC.                                
02071                                                                   
02072      MOVE +0 TO T.                                                
02073                                                                   
02074  0435-LEV-RSVR-LOOP.                                              
02075      COMPUTE CTR = XX + T + 2.                                    
02076      COMPUTE CTS = XX + T + 3.                                    
02077      COMPUTE ANSB = ((V * RF-DX-FACTOR (CTR)) -                   
02078                       RF-DX-FACTOR (CTS)) / DX.                   
02079                                                                   
02080      COMPUTE RSVR = RSVR + (1000 * ANSB).                         
02081                                                                   
02082      ADD 1 TO T.                                                  
02083                                                                   
02084      IF T NOT GREATER R                                           
02085          GO TO 0435-LEV-RSVR-LOOP.                                
02086                                                                   
02087      GO TO 0440-END-RSVR-CALC.                                    
02088  EJECT                                                            
02089  0440-END-RSVR-CALC.                                              
02090      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
02091          COMPUTE WS-MORT-FACTOR  ROUNDED = (RSVR * RF-RESV-ADJ)   
02092                                            * RF-JOINT-ADJ         
02093        ELSE                                                       
02094          COMPUTE WS-MORT-FACTOR ROUNDED = RSVR * RF-RESV-ADJ.     
02095                                                                   
02096      GO TO 0449-STD-EXIT.                                         
02097                                                                   
02098  0445-FIND-P.                                                     
02099      SUBTRACT 12 FROM P.                                          
02100      IF P GREATER 12                                              
02101          GO TO 0445-FIND-P.                                       
02102  0445-EXIT.                                                       
02103      EXIT.                                                        
02104                                                                   
02105  0449-STD-EXIT.                                                   
02106      EXIT.                                                        
02107                              EJECT                                
02108                                                                   
02109  0450-ALT-MORTALITY.                                              
02110      MOVE +0                     TO RSVR.                         
02111      MOVE GR-LF-REMTERM          TO CTA.                          
02112      MOVE WRK-GR-MORT-AGE        TO CTT.                          
02113                                                                   
02114      COMPUTE TX = WRK-GR-MORT-AGE + +1.                           
02115                                                                   
02116 **                                                                
02117 **   DR = DX VALUE AT VALUATION AGE                               
02118 **   MR = MX VALUE AT VALUATION AGE                               
02119 **                                                                
02120 **   ** NOTE **                                                   
02121 **     +1 IS ADDED TO AGE ONLY TO ADJUST FOR TABLE, NOT TO INCREAS
02122 **     THE AGE.                                                   
02123 **                                                                
02124      MOVE RF-DX-FACTOR (TX)      TO DR.                           
02125      MOVE MX-FACTORS (TX)        TO MR.                           
02126 **                                                                
02127 **   IF AGE AT TERMINATION IS NOT EVEN THEN THE MX AT TERMINATION 
02128 **   (MY) MUST BE INTERPOLATED. I.E. IF AGE IS 35 WITH 5 MONTHS   
02129 **   REMAINING (EX. AGE 34 WITH REMAINING TERM OF 17), THEN 5/12TH
02130 **   OF THE AGE 36 MX IS USED PLUS 7/12THS OF THE AGE 35 MX TO    
02131 **   DERIVE THE MX IN THE FORMULA                                 
02132 **                                                                
02133 **   X2 WILL BE THE NUMBER OF 12THS OF THE HIGH AGE (WK-MY2)      
02134 **   X3 WILL BE THE NUMBER OF 12THS OF THE LOW AGE (WK-MY1)       
02135 **                                                                
02136      IF CTA GREATER +11                                           
02137          DIVIDE CTA BY +12 GIVING X1 REMAINDER X2                 
02138          IF X2 = ZERO                                             
02139              MOVE +12            TO X3                            
02140            ELSE                                                   
02141              SUBTRACT X2 FROM 12 GIVING X3                        
02142       ELSE                                                        
02143          SUBTRACT CTA FROM +12 GIVING X3                          
02144          MOVE CTA                TO X2.                           
02145                                                                   
02146      IF CTX GREATER 98                                            
02147           MOVE ZERO              TO WS-MORT-FACTOR                
02148           GO TO 0459-ALT-EXIT.                                    
02149                                                                   
02150      COMPUTE CTX = WRK-GR-MORT-AGE + (GR-LF-REMTERM / +12) + +1.  
02151      MOVE MX-FACTORS (CTX)       TO WK-MY1.                       
02152      ADD +1 TO CTX.                                               
02153      MOVE MX-FACTORS (CTX)       TO WK-MY2.                       
02154                                                                   
02155      COMPUTE MY = ((WK-MY1 / +12) * X3) + ((WK-MY2 / +12) * X2).  
02156                                                                   
02157      COMPUTE WS-MORT-FACTOR-ALT ROUNDED = (MR - MY) / DR.         
02158                                                                   
02159      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
02160          COMPUTE WS-MORT-FACTOR-ALT ROUNDED = RF-JOINT-ADJ *      
02161                        (WS-MORT-FACTOR-ALT * RF-RESV-ADJ)         
02162        ELSE                                                       
02163          COMPUTE WS-MORT-FACTOR-ALT ROUNDED =                     
02164                        (WS-MORT-FACTOR-ALT * RF-RESV-ADJ).        
02165                                                                   
02166      MOVE WS-MORT-FACTOR-ALT          TO WS-MORT-FACTOR.          
02167                                                                   
02168  0459-ALT-EXIT.                                                   
02169      EXIT.                                                        
02170                              EJECT                                
02171  0460-ALT-MORTALITY-2.                                            
02172                                                                   
02173      MOVE +0                     TO WK-CX  WK-DX.                 
02174      MOVE +0                     TO WK-RESV  WK-RSV.              
02175      MOVE +0                     TO X4.                           
02176      MOVE GR-LF-REMTERM          TO X3.                           
02177                                                                   
02178  0461-MONTH-LOOP.                                                 
02179      ADD +1                      TO X4.                           
02180                                                                   
02181      IF X4 GREATER GR-LF-REMTERM                                  
02182          GO TO 0463-ADJUST-TOT.                                   
02183                                                                   
02184      IF X4 = 1                                                    
02185          COMPUTE CTX = GR-AGE +                                   
02186             ((X4 + (GR-LF-TERM - (GR-LF-REMTERM + 1))) / 12) + 1  
02187          MOVE CTX                TO CTX2                          
02188      ELSE                                                         
02189          COMPUTE CTX2 = GR-AGE +                                  
02190             ((X4 + (GR-LF-TERM - (GR-LF-REMTERM + 1))) / 12) + 1. 
02191                                                                   
02192      IF CLAS-I-RL-AH (CLAS-INDEXL) = 'L' OR 'P'                   
02193          MOVE GR-LFBEN           TO WK-RSV                        
02194      ELSE                                                         
02195          COMPUTE WK-RSV = GR-MO-DEC * X3.                         
02196                                                                   
02197      IF WK-RSV GREATER WS-LIFE-LIMIT                              
02198          MOVE WS-LIFE-LIMIT      TO WK-RSV.                       
02199                                                                   
02200      COMPUTE QX ROUNDED = (RF-CX-FACTOR (CTX2) /                  
02201                            RF-DX-FACTOR (CTX)) * RF-RESV-ADJ.     
02202                                                                   
02203      COMPUTE WK-RESV = WK-RESV + (WK-RSV * QX)                    
02204                                                                   
02205      SUBTRACT +1               FROM X3.                           
02206                                                                   
02207      GO TO 0461-MONTH-LOOP.                                       
02208                                                                   
02209  0463-ADJUST-TOT.                                                 
02210                                                                   
02211      COMPUTE WRK-GR-RESV = WK-RESV / +12.                         
02212                                                                   
02213      IF CLAS-I-JOINT (CLAS-INDEXL) = 'J'                          
02214          COMPUTE WRK-GR-RESV ROUNDED =                            
02215                           WRK-GR-RESV * WS-FACT.                  
02216                                                                   
02217      MOVE SPACE TO GR-FLAG.                                       
02218                                                                   
02219      GO TO 0399-XIT.                                              
02220                                                                   
02221  0469-ALT-2-EXIT.                                                 
02222      EXIT.                                                        
02223                              EJECT                                
02224  0470-ACM-MORTALITY.                                              
02225                                                                   
02226      MOVE +0                     TO RSVR.                         
02227      MOVE GR-LF-REMTERM          TO CTA.                          
02228      MOVE WRK-GR-MORT-AGE        TO CTT.                          
02229                                                                   
02230      COMPUTE TX = WRK-GR-MORT-AGE + +1.                           
02231      COMPUTE X1 EQUAL GR-LF-REMTERM / +12.                        
02232      COMPUTE X2 EQUAL TX + X1.                                    
02233                                                                   
02234      COMPUTE ANSB EQUAL ((CTA - (12 * X1 - 1)) /                  
02235            (2 * CTA)) * (CTA / +12 - X1).                         
02236                                                                   
02237      COMPUTE ANSC EQUAL RF-CX-FACTOR (X2) / RF-DX-FACTOR (TX).    
02238      COMPUTE ANSD EQUAL    RF-INT-ADJ **                          
02239                        ((12 * (X1 + +1) - CTA) / 12).             
02240      COMPUTE ANSB EQUAL ANSB * ANSC * ANSD.                       
02241                                                                   
02242      MOVE +1    TO X3.                                            
02243      MOVE +0    TO ANSC.                                          
02244                                                                   
02245  0472-ANSC-SIGMA.                                                 
02246                                                                   
02247      IF X3 GREATER THAN X1                                        
02248         GO TO 0475-FINISH-SIGMA.                                  
02249                                                                   
02250      COMPUTE X4 EQUAL TX + X3 - +1.                               
02251      COMPUTE ANSC EQUAL ANSC +                                    
02252             ((2 * CTA - (24 * X3 - 13)) /                         
02253             (2 * CTA)) * (RF-CX-FACTOR (X4) / RF-DX-FACTOR (TX)). 
02254                                                                   
02255      ADD +1 TO X3.                                                
02256      GO TO 0472-ANSC-SIGMA.                                       
02257                                                                   
02258  0475-FINISH-SIGMA.                                               
02259                                                                   
02260      COMPUTE WS-MORT-FACTOR EQUAL                                 
02261               +100 * (ANSC + ANSB) * RF-RESV-ADJ.                 
02262                                                                   
02263  0479-ACM-EXIT.                                                   
02264      EXIT.                                                        
02265                              EJECT                                
02266  0475-WRITE-GAAP-RECORD.                                          
02267      IF GR-FLAG NOT = SPACES                                      
02268          PERFORM 0500-PRT-ERRORS THRU 0599-PRT-ERRORS-X.          
02269                                                                   
02270      MOVE '0' TO GR-MORT-TYP.                                     
02271                                                                   
02272      IF GR-ALT-MORT-CODE NOT = SPACES                             
02273          MOVE '0' TO GR-ALT-MORT-TYP.                             
02274                                                                   
02275      WRITE GN-REC FROM GAAP-RECORD.                               
02276                                                                   
CIDMOD* THE FOLLOWING DISPLAYS LEFT IN FOR FUTURE USE IN TESTING.       
CIDMOD* CODE IS BYPASSED AND PROGRAM GOES TO 0499-EXIT.                 
CIDMOD*                                                                 
CIDMOD     GO TO  0499-XIT.                                             
CIDMOD                                                                  
CIDMOD     IF  GR-LFTYP = 'QD'                                          
CIDMOD         NEXT SENTENCE                                            
CIDMOD      ELSE                                                        
CIDMOD         GO TO  0499-XIT.                                         
CIDMOD                                                                  
CIDMOD     IF  QD-WRITE-CNT GREATER THAN 25                             
CIDMOD         GO TO  0499-XIT.                                         
CIDMOD                                                                  
CIDMOD        ADD 1 TO QD-WRITE-CNT.                                    
CIDMOD        DISPLAY ' '.                                              
CIDMOD        DISPLAY ' WRITE GAAP RECORD '                             
CIDMOD        DISPLAY ' '.                                              
CIDMOD        DISPLAY 'GR-REIN           ' GR-REIN                      
CIDMOD        DISPLAY 'GR-CARRIER        ' GR-CARRIER                   
CIDMOD        DISPLAY 'GR-GROUPING       ' GR-GROUPING                  
CIDMOD        DISPLAY 'GR-STATE          ' GR-STATE                     
CIDMOD        DISPLAY 'GR-ACCOUNT        ' GR-ACCOUNT                   
CIDMOD        DISPLAY 'GR-EFF            ' GR-EFF                       
CIDMOD        DISPLAY 'GR-CERT-NO        ' GR-CERT-NO                   
CIDMOD        DISPLAY 'GR-REINCO         ' GR-REINCO                    
CIDMOD        DISPLAY 'GR-REINCO-SUB     ' GR-REINCO-SUB                
CIDMOD        DISPLAY 'GR-IG             ' GR-IG                        
CIDMOD        DISPLAY 'GR-APR            ' GR-APR                       
CIDMOD        DISPLAY 'GR-PMT-FREQ       ' GR-PMT-FREQ                  
CIDMOD        DISPLAY 'GR-LOAN-TERM      ' GR-LOAN-TERM                 
CIDMOD        DISPLAY 'GR-AGE            ' GR-AGE                       
CIDMOD        DISPLAY 'GR-LFTYP          ' GR-LFTYP                     
CIDMOD        DISPLAY 'GR-LF-TERM        ' GR-LF-TERM                   
CIDMOD        DISPLAY 'GR-LF-REMTERM     ' GR-LF-REMTERM                
CIDMOD        DISPLAY 'GR-LF-UP-REMTERM  ' GR-LF-UP-REMTERM             
CIDMOD        DISPLAY 'GR-LFBEN          ' GR-LFBEN                     
CIDMOD        DISPLAY 'GR-LFPRM          ' GR-LFPRM                     
CIDMOD        DISPLAY 'GR-LFCOM          ' GR-LFCOM                     
CIDMOD        DISPLAY 'GR-LFEXP          ' GR-LFEXP                     
CIDMOD        DISPLAY 'GR-LFTAX          ' GR-LFTAX                     
CIDMOD        DISPLAY 'GRP-LFPRM         ' GRP-LFPRM                    
CIDMOD        DISPLAY 'GRP-LFCOM         ' GRP-LFCOM                    
CIDMOD        DISPLAY 'GRP-LFEXP         ' GRP-LFEXP                    
CIDMOD        DISPLAY 'GRP-LFTAX         ' GRP-LFTAX                    
CIDMOD        DISPLAY 'GRR-LFPRM         ' GRR-LFPRM                    
CIDMOD        DISPLAY 'GRR-LFCOM         ' GRR-LFCOM                    
CIDMOD        DISPLAY 'GRR-LFEXP         ' GRR-LFEXP                    
CIDMOD        DISPLAY 'GRR-LFTAX         ' GRR-LFTAX                    
CIDMOD        DISPLAY 'GRD-LFPRM         ' GRD-LFPRM                    
CIDMOD        DISPLAY 'GRD-LFCOM         ' GRD-LFCOM                    
CIDMOD        DISPLAY 'GRS-LFPRM         ' GRS-LFPRM                    
CIDMOD        DISPLAY 'GRS-LFCOM         ' GRS-LFCOM                    
CIDMOD        DISPLAY 'GR-MORT-CONTROL   ' GR-MORT-CONTROL              
CIDMOD        DISPLAY 'GR-MORT-BASE      ' GR-MORT-BASE                 
CIDMOD        DISPLAY 'GR-MORT-TYP       ' GR-MORT-TYP                  
CIDMOD        DISPLAY 'GR-MORT-AGE       ' GR-MORT-AGE                  
CIDMOD        DISPLAY 'GR-MORT-DATA      ' GR-MORT-DATA                 
CIDMOD        DISPLAY 'GR-REM-AMT        ' GR-REM-AMT                   
CIDMOD        DISPLAY 'GR-MO-DEC         ' GR-MO-DEC                    
CIDMOD        DISPLAY 'GR-MORT-FACT      ' GR-MORT-FACT                 
CIDMOD        DISPLAY 'GR-RESV           ' GR-RESV                      
CIDMOD        DISPLAY 'GR-FLAG           ' GR-FLAG                      
CIDMOD        DISPLAY 'GR-ALT-MORT-BASE  ' GR-ALT-MORT-CODE             
CIDMOD        DISPLAY 'GR-ALT-MORT-TYP   ' GR-ALT-MORT-TYP              
CIDMOD        DISPLAY 'GR-ALT-RESV       ' GR-ALT-RESV                  
CIDMOD        DISPLAY 'GR-CNT            ' GR-CNT                       
CIDMOD        DISPLAY 'GR-CNT-LF         ' GR-CNT-LF                    
CIDMOD        DISPLAY 'GR-OB-IND         ' GR-OB-IND                    
CIDMOD        DISPLAY 'GR-SEX-CODE       ' GR-SEX-CODE                  
CIDMOD        DISPLAY ' '                                               
CIDMOD        DISPLAY ' END OF WRITE  '                                 
CIDMOD        DISPLAY ' '.                                              
CIDMOD                                                                  
02277  0499-XIT.                                                        
02278      EXIT.                                                        
02279  EJECT                                                            
02280  0500-PRT-ERRORS.                                                 
02281      IF WS-LINE GREATER +52                                       
02282          PERFORM 0600-HDR-RTN THRU 0699-HDR-RTN-X                 
02283          MOVE ' ' TO X                                            
02284          MOVE HDR-4 TO PRT                                        
02285          PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                
02286                                                                   
02287      MOVE GR-CARRIER        TO D1-CARR.                           
02288      MOVE GR-GROUPING       TO D1-GROUP.                          
02289      MOVE GR-STATE          TO D1-STATE.                          
02290      MOVE GR-ACCOUNT        TO D1-ACCT.                           
02291      MOVE GR-MO             TO D1-EFF-MO.                         
02292      MOVE GR-DA             TO D1-EFF-DA.                         
02293      MOVE GR-YR             TO D1-EFF-YR.                         
02294      MOVE GR-CERT           TO D1-CERT.                           
02295      MOVE CLAS-I-AB10 (CLAS-INDEXL) TO D1-LIFE-TYPE.              
02296      MOVE GR-MORT-CODE      TO D1-TABLE.                          
02297      MOVE GR-MORT-AGE       TO D1-AGE.                            
02298      MOVE GR-LF-REMTERM     TO D1-TERM.                           
02299                                                                   
02300      IF GR-REIN = 'R'                                             
02301          MOVE GR-REIN-COMP TO D1-REIN                             
02302      ELSE                                                         
02303          MOVE SPACES TO D1-REIN.                                  
02304                                                                   
02305      MOVE SPACES TO D1-REASON.                                    
02306                                                                   
02307      IF GR-FLAG = 'A'                                             
02308          IF ALT-MORT                                              
02309              MOVE 'ALT. MORTALITY CODE NOT VALID' TO D1-REASON    
02310          ELSE                                                     
02311              MOVE 'MORTALITY CODE NOT VALID' TO D1-REASON.        
02312                                                                   
02313      IF GR-FLAG = 'B'                                             
02314          MOVE 'TERM NOT ON FILE' TO D1-REASON.                    
02315                                                                   
02316      IF GR-FLAG = 'C'                                             
02317          MOVE 'AGE NOT ON FILE' TO D1-REASON.                     
02318                                                                   
02319      IF GR-FLAG = 'D'                                             
02320          MOVE 'MORTALITY CODE NOT ON RFAC' TO D1-REASON.          
02321                                                                   
02322      IF GR-FLAG = 'E'                                             
02323          MOVE 'MORTALITY AGE IS ZERO' TO D1-REASON.               
02324                                                                   
02325      IF GR-FLAG = 'F'                                             
02326          MOVE 'FUTURE BUSINESS' TO D1-REASON.                     
02327                                                                   
02328      IF GR-FLAG = 'N'                                             
02329          MOVE 'NET PAY D(X)=0' TO D1-REASON.                      
02330                                                                   
02331      IF GR-FLAG = 'Z'                                             
02332          MOVE 'NET PAY APR=0 ' TO D1-REASON.                      
02333                                                                   
02334      MOVE DTL-1 TO PRT.                                           
02335      MOVE ' ' TO X.                                               
02336      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02337      ADD +1 TO WS-LINE.                                           
02338      IF ALT-MORT                                                  
02339          ADD +1 TO ALT-ERR-COUNT                                  
02340      ELSE                                                         
02341          ADD +1 TO ERR-COUNT.                                     
02342                                                                   
02343  0599-PRT-ERRORS-X.                                               
02344      EXIT.                                                        
02345  EJECT                                                            
02346  0600-HDR-RTN.                                                    
02347      MOVE HDR-1 TO PRT.                                           
02348      MOVE '1' TO X.                                               
02349      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02350      MOVE HDR-2 TO PRT.                                           
02351      MOVE ' ' TO X.                                               
02352      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02353      ADD +1 TO WS-PAGE.                                           
02354      MOVE WS-PAGE TO H3-PAGE.                                     
02355      MOVE HDR-3 TO PRT.                                           
02356      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02357      MOVE SPACES TO PRT.                                          
02358      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02359      MOVE ZERO TO WS-LINE.                                        
02360                                                                   
02361  0699-HDR-RTN-X.                                                  
02362      EXIT.                                                        
02363                                                                   
02364  0700-PRT-RTN.                                                    
02365                              COPY ELCPRT2.                        
02366                                                                   
02367  0799-PRT-RTN-X.                                                  
02368      EXIT.                                                        
02369  EJECT                                                            
02370  0800-GET-RATE.                                                   
02371                                                                   
112812*    IF DTE-CLIENT = 'CID'                                        
112812*        IF WRK-GR-MORT-CODE = MORTALITY-CODE (SA)  AND           
112812*               WRK-GR-LIFE-CODE = GR-LFTYP                       
112812*            GO TO 0899-GET-RATE-X                                
112812*        ELSE
112812*            NEXT SENTENCE
112812*    ELSE                                                         
112812*        IF WRK-GR-MORT-CODE = MORTALITY-CODE (SA)                
112812*            GO TO 0899-GET-RATE-X.                               
02379                                                                   
02380      MOVE +1 TO SA.                                               
02381                                                                   
02382  0810-GET-RATE-LOOP.                                              
02383                                                                   
02384      IF  WRK-GR-MORT-CODE NOT = MORTALITY-CODE (SA)               
02385          ADD +1               TO SA                               
013013         IF  SA LESS +154
02387              GO TO 0810-GET-RATE-LOOP                             
02388          ELSE                                                     
02389              MOVE 'D'         TO GR-FLAG                          
02390              GO TO 0899-GET-RATE-X.                               
02391
02392      MOVE FACT-TBL (SA)       TO RF-REC.                          
02393      MOVE MX-SAVE-ENTRY (SA)  TO MORT-MX-TABLE.                   
02394                                                                   
062104     IF DTE-CLIENT = 'CID'                               
070908        MOVE ZEROS               TO WS-RESV-ADJ
CIDMOD     ELSE
062104        DISPLAY 'NOT CID'
CIDMOD        GO TO 0899-GET-RATE-X.                                    
CIDMOD                                                                  
CIDMOD**   MOVE GR-LFTYP             TO WRK-GR-LIFE-CODE.               
CIDMOD                                                                  
CIDMOD*    IF GR-LFTYP = '1M' OR '1Q' OR '1S'
CIDMOD*       MULTIPLY RF-RESV-ADJ BY 1.1920 GIVING RF-RESV-ADJ
CIDMOD*       MOVE GR-LFTYP  TO WRK-GR-LIFE-CODE                        
CIDMOD*       GO TO 0899-GET-RATE-X.                                    
CIDMOD*                                                                 
CIDMOD*    IF GR-LFTYP = '1N' OR '1R' OR '1T'
CIDMOD*       MULTIPLY RF-RESV-ADJ BY 1.2760 GIVING RF-RESV-ADJ
CIDMOD*       MOVE GR-LFTYP  TO WRK-GR-LIFE-CODE                        
CIDMOD*       GO TO 0899-GET-RATE-X.                                    
CIDMOD*
CIDMOD*    IF GR-LFTYP = '1O'
CIDMOD*       MULTIPLY RF-RESV-ADJ BY 1.1040 GIVING RF-RESV-ADJ
CIDMOD*       MOVE GR-LFTYP  TO WRK-GR-LIFE-CODE                        
CIDMOD*       GO TO 0899-GET-RATE-X.                                    
CIDMOD*
CIDMOD*    IF GR-LFTYP = '1P'
CIDMOD*       MULTIPLY RF-RESV-ADJ BY 1.1820 GIVING RF-RESV-ADJ
CIDMOD*       MOVE GR-LFTYP  TO WRK-GR-LIFE-CODE                        
CIDMOD*       GO TO 0899-GET-RATE-X.                                    
CIDMOD*
CIDMOD*    IF (GR-STATE = 'IL') AND
CIDMOD*       (GR-EFF > 19971231) AND
CIDMOD*       (GR-EFF < 19990701)
CIDMOD*       IF GR-LFTYP = '1E' OR '1I' OR '1K'
CIDMOD*          MULTIPLY RF-RESV-ADJ BY 1.1920 GIVING RF-RESV-ADJ
CIDMOD*          MOVE SPACES  TO WRK-GR-LIFE-CODE                       
CIDMOD*       END-IF
CIDMOD*       IF GR-LFTYP = '1F' OR '1J' OR '1L'
CIDMOD*          MULTIPLY RF-RESV-ADJ BY 1.2760 GIVING RF-RESV-ADJ
CIDMOD*          MOVE SPACES  TO WRK-GR-LIFE-CODE                       
CIDMOD*       END-IF                                                    
CIDMOD*       IF GR-LFTYP = '1G'
CIDMOD*          MULTIPLY RF-RESV-ADJ BY 1.1040 GIVING RF-RESV-ADJ
CIDMOD*          MOVE SPACES  TO WRK-GR-LIFE-CODE                       
CIDMOD*       END-IF                                                    
CIDMOD*       IF GR-LFTYP = '1H'
CIDMOD*          MULTIPLY RF-RESV-ADJ BY 1.1820 GIVING RF-RESV-ADJ
CIDMOD*          MOVE SPACES  TO WRK-GR-LIFE-CODE                       
CIDMOD*       END-IF
CIDMOD*    ELSE

070908     EVALUATE TRUE
070908        WHEN GR-LFTYP = '1M' OR '1Q' OR '1S'
070908                     OR '1E' OR '1I' OR '1K'
070908           MOVE 1.1920           TO WS-RESV-ADJ
070908           MOVE GR-LFTYP         TO WRK-GR-LIFE-CODE
070908        WHEN GR-LFTYP = '1N' OR '1R' OR '1T'
070908                     OR '1F' OR '1J' OR '1L'
070908           MOVE 1.2760           TO WS-RESV-ADJ
070908           MOVE GR-LFTYP         TO WRK-GR-LIFE-CODE
070908        WHEN GR-LFTYP = '1O' OR '1G'
070908           MOVE 1.1040           TO WS-RESV-ADJ
070908           MOVE GR-LFTYP         TO WRK-GR-LIFE-CODE
070908        WHEN GR-LFTYP = '1P' OR '1H'
070908           MOVE 1.1820           TO WS-RESV-ADJ
070908           MOVE GR-LFTYP         TO WRK-GR-LIFE-CODE
070908     END-EVALUATE

070908     IF (GR-STATE = 'IL') AND
070908        (GR-EFF > 19990630)
070908        IF GR-LFTYP = '1E' OR '1I' OR '1K'
070908                   OR '1F' OR '1J' OR '1L'
070908           MOVE 1.2760           TO WS-RESV-ADJ
070908           MOVE SPACES  TO WRK-GR-LIFE-CODE                    
070908        END-IF
070908        IF GR-LFTYP = '1G' OR '26' OR '27' OR '1H'
070908           MOVE 1.1820           TO WS-RESV-ADJ
070908           MOVE SPACES  TO WRK-GR-LIFE-CODE                    
070908        END-IF                                                 
070908     END-IF

070908     IF WS-RESV-ADJ NOT = ZEROS
070908        MULTIPLY RF-RESV-ADJ BY WS-RESV-ADJ GIVING RF-RESV-ADJ
070908     END-IF

02403      .                                                            
02404  0899-GET-RATE-X.                                                 
02405      EXIT.                                                        
02406  EJECT                                                            
02407                                                                   
02408                                                                   
02409  0900-READ-ACCT-MSTR.                                             
02410      READ ACC-MSTR NEXT.                                          
02411                                                                   
02412      IF AM-FILE-STATUS = '10'                                     
02413         MOVE HIGH-VALUES TO AM-CONTROL-PRIMARY                    
02414         MOVE +9999999               TO AM-MAX-TOT-BEN             
02415                                        AM-MAX-MON-BEN             
02416         GO TO 0999-READ-ACCT-MSTR-EX.                             
02417                                                                   
02418      IF AM-FILE-STATUS NOT = '00'                                 
02419         MOVE 'ERROR ERACCTT, READ ' TO WS-ABEND-MESSAGE           
02420         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               
02421         GO TO ABEND-PGM.                                          
02422                                                                   
02423      IF AM-MAX-TOT-BEN NOT NUMERIC  OR                            
02424         AM-MAX-TOT-BEN = ZEROS                                    
02425          MOVE +9999999              TO AM-MAX-TOT-BEN.            
02426      IF AM-MAX-MON-BEN NOT NUMERIC  OR                            
02427         AM-MAX-MON-BEN = ZEROS                                    
02428          MOVE +9999999              TO AM-MAX-MON-BEN.            
02429                                                                   
02430  0999-READ-ACCT-MSTR-EX.                                          
02431      EXIT.                                                        
02432  EJECT                                                            
02433  2300-MATCH-NET-PAY.                                              
02434                                                                   
02435      MOVE SPACES                  TO W-NET-PAY-MATCH-SW.          
02436                                                                   
02437      IF  GR-CONTROL EQUAL W-GNPT-CONTROL                          
02438          IF  NP-OPT EQUAL W-GNPT-CALC-OPTION                      
02439                  AND                                              
02440              GR-APR EQUAL W-GNPT-APR                              
02441                  AND                                              
02442              NP-ORIG EQUAL W-GNPT-ORIGINAL-TERM                   
02443              MOVE 'Y'             TO W-NET-PAY-MATCH-SW           
02444              GO TO 2300-EXIT.                                     
02445                                                                   
02446      IF  W-GNP-TABLE GREATER THAN LOW-VALUES                      
02447          PERFORM 2500-WRITE-NEW-NET-PAY THRU 2500-EXIT.           
02448                                                                   
02449      MOVE LOW-VALUES              TO W-GNP-TABLE.                 
02450      MOVE 'NP'                    TO W-GNPT-RECORD-ID.            
02451      MOVE GR-COMPANY-CD           TO W-GNPT-COMPANY-CD.           
02452      MOVE GR-CONTROL              TO W-GNPT-CONTROL.              
02453      MOVE GR-APR                  TO W-GNPT-APR.                  
02454      MOVE NP-ORIG                 TO W-GNPT-ORIGINAL-TERM.        
02455      MOVE NP-OPT                  TO W-GNPT-CALC-OPTION.          
02456                                                                   
02457      PERFORM 2350-READ-NET-PAY THRU 2350-EXIT                     
02458              UNTIL                                                
02459          W-EOF-NET-PAY-REACHED                                    
02460              OR                                                   
02461          GNP-CONTROL-PRIMARY                                      
02462              NOT LESS THAN W-GNPT-CONTROL-PRIMARY.                
02463                                                                   
02464      IF  W-EOF-NET-PAY-REACHED                                    
02465              OR                                                   
02466          GNP-CONTROL-PRIMARY                                      
02467              GREATER THAN W-GNPT-CONTROL-PRIMARY                  
02468          GO TO 2300-EXIT.                                         
02469                                                                   
02470      MOVE GAAP-NET-PAY-RECORD     TO W-GNP-TABLE.                 
02471      MOVE 'Y'                     TO W-NET-PAY-MATCH-SW.          
02472                                                                   
02473  2300-EXIT.                                                       
02474      EXIT.                                                        
02475                                                                   
02476  2350-READ-NET-PAY.                                               
02477                                                                   
02478      READ GAAP-LAST-NET-PAY                                       
02479          AT END                                                   
02480              MOVE 'Y'              TO W-EOF-NET-PAY-SW.           
02481                                                                   
02482 ***************************************************************   
02483 *  THIS LOGIC WAS LEFT IN TO ALLOW EASE IN PIN POINT SPECIFIC *   
02484 *  GAAP RECORDS FOR TROUBLE SHOOTING.                         *   
02485 ***************************************************************   
02486      IF  W-CONTROL-OVERRIDE GREATER THAN SPACES                   
02487              AND                                                  
02488          W-CO-ACCOUNT EQUAL GNP-ACCOUNT                           
02489              AND                                                  
02490          W-CO-CERT EQUAL GNP-CERT                                 
02491          DISPLAY 'GNP-CONTROL -'                                  
02492              GNP-CONTROL.                                         
02493                                                                   
02494  2350-EXIT.                                                       
02495      EXIT.                                                        
02496                              EJECT                                
02497 ***************************************************************   
02498 *  THIS PARAGRAPH WRITES THE COMPLETED GNP RECORD.            *   
02499 ***************************************************************   
02500  2500-WRITE-NEW-NET-PAY.                                          
02501                                                                   
02502      WRITE GNP-NEW-REC FROM W-GNP-TABLE.                          
02503                                                                   
02504  2500-EXIT.                                                       
02505      EXIT.                                                        
02506                                                                   
02507  2900-DATE-CONVERSION.                                            
02508                                                                   
02509      CALL 'ELDATCX' USING DATE-CONVERSION-DATA.                   
02510                                                                   
02511  2900-EXIT.                                                       
02512      EXIT.                                                        
02513                                                                   
02514  2980-RUN-DATE-ERROR.                                             
02515                                                                   
02516      MOVE 2001                     TO WS-RETURN-CODE              
02517      MOVE ' RUN DATE CONV ERROR  ' TO WS-ABEND-MESSAGE            
02518      GO TO ABEND-PGM.                                             
02519                                                                   
02520  2980-EXIT.                                                       
02521      EXIT.                                                        
02522                                                                   
02523  2984-RUN-DATE-DIF-ERROR.                                         
02524                                                                   
02525      MOVE 2004                     TO WS-RETURN-CODE              
02526                                                                   
02527      MOVE ' ERROR DETECTED CALCULATING RUN DATE DIFFERENCE'       
02528                                    TO WS-ABEND-MESSAGE            
02529      GO TO ABEND-PGM.                                             
02530                                                                   
02531  2984-EXIT.                                                       
02532      EXIT.                                                        
02533                                                                   
02534  2985-USED-FILE-ERROR.                                            
02535                                                                   
02536      MOVE 2003                     TO WS-RETURN-CODE              
02537                                                                   
02538      MOVE ' WRONG GNP FILE USED.  INPUT FILE MONTH IS XXX.'       
02539                                    TO WS-ABEND-MESSAGE            
02540      MOVE DC-ELAPSED-MONTHS        TO WS-MONTHS-DIFFERENCE        
02541      GO TO ABEND-PGM.                                             
02542                                                                   
02543  2985-EXIT.                                                       
02544      EXIT.                                                        
02545                                                                   
02546  2990-LAST-CALC-DATE-ERROR.                                       
02547                                                                   
02548      MOVE 2002                     TO WS-RETURN-CODE              
02549      MOVE ' LAST PVFB CALC DATE ERROR  '                          
02550                                    TO WS-ABEND-MESSAGE            
02551      GO TO ABEND-PGM.                                             
02552                                                                   
02553  2990-EXIT.                                                       
02554      EXIT.                                                        
02555  EJECT                                                            
02556  9000-E-O-J.                                                      
02557                                                                   
02558      IF  W-GNP-TABLE GREATER THAN LOW-VALUES                      
02559          PERFORM 2500-WRITE-NEW-NET-PAY THRU 2500-EXIT.           
02560                                                                   
CIDMOD     DISPLAY ' '.                                                 
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * *'.               
CIDMOD     DISPLAY ' DISPLAY ERROR COUNT = '  ERROR-COUNT.              
CIDMOD     DISPLAY '* * * * * * * * * * * * * * * * * *'.               
CIDMOD     DISPLAY ' '.                                                 
CIDMOD                                                                  
CIDMOD     PERFORM 8600-DISPLAY-HD  THRU                                
CIDMOD           8600-HD-EXIT.                                          
CIDMOD     MOVE ' DISPLAY ERROR COUNT = '    TO DIS-LINE-REASON         
CIDMOD     MOVE ERROR-COUNT                TO  DIS-LINE-REC             
CIDMOD     PERFORM 8600-DISPLAY-PRT THRU                                
CIDMOD           8600-DISPLAY-EXIT.                                     
CIDMOD                                                                  
02561      PERFORM 0600-HDR-RTN THRU 0699-HDR-RTN-X.                    
02562                                                                   
02563      MOVE TOT-GROSS TO T1-RESERVE.                                
02564      MOVE TOT-1 TO PRT.                                           
02565      MOVE '-' TO X.                                               
02566      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    

062104     IF DTE-CLIENT = 'CID'
062104         MOVE 'CILGM17'              TO WS-ME-BAL-JOB
062104         MOVE T1-RESERVE             TO WS-ME-BAL-AMT
062104         MOVE WS-BALANCE-DESCRIPTION TO WS-ME-BAL-DESCRIP
062104         WRITE ME-ECS080-BALANCE-REC FROM WS-ME-BALANCE-REC
100220         MOVE 'CILGM17'              TO WS-ME-BAL-JOB
100220         move tot-inforce            to t1-reserve
100220         MOVE T1-RESERVE             TO WS-ME-BAL-AMT
100220         MOVE 'Remaing Amount'       TO WS-ME-BAL-DESCRIP
100220         WRITE ME-ECS080-BALANCE-REC FROM WS-ME-BALANCE-REC
062104     END-IF.
02567                                                                   
02568      MOVE TOT-REIN TO T2-RESERVE.                                 
02569      MOVE TOT-2 TO PRT.                                           
02570      MOVE ' ' TO X.                                               
02571      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02572                                                                   
02573      MOVE ERR-COUNT TO T3-COUNT.                                  
02574      MOVE TOT-3 TO PRT.                                           
02575      MOVE ' ' TO X.                                               
02576      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02577                                                                   
02578      MOVE ' ALTERNATE RESERVES' TO PRT.                           
02579      MOVE '-' TO X.                                               
02580      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02581                                                                   
02582      MOVE ALT-TOT-GROSS TO T1-RESERVE.                            
02583      MOVE TOT-1 TO PRT.                                           
02584      MOVE '0' TO X.                                               
02585      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02586                                                                   
02587      MOVE ALT-TOT-REIN TO T2-RESERVE.                             
02588      MOVE TOT-2 TO PRT.                                           
02589      MOVE ' ' TO X.                                               
02590      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02591                                                                   
02592      MOVE ALT-ERR-COUNT TO T3-COUNT.                              
02593      MOVE TOT-3 TO PRT.                                           
02594      MOVE ' ' TO X.                                               
02595      PERFORM 0700-PRT-RTN THRU 0799-PRT-RTN-X.                    
02596                                                                   
02597      IF REIN-OPEN-SW = 'X'                                        
02598          CLOSE REIN-TBL-FILE                                      
02599          IF REIN-FILE-STATUS NOT = ZEROS                          
02600             MOVE 'ERROR ON ERRTBL, CLOSE ' TO WS-ABEND-MESSAGE    
02601             MOVE REIN-FILE-STATUS TO WS-ABEND-FILE-STATUS         
02602             GO TO ABEND-PGM.                                      
02603                                                                   
02604      DISPLAY ' '                                                  
02605      DISPLAY 'TOTAL NUMBER OF RECORDS PROCESSED - '               
02606          W-TOTAL-RECORDS.                                         
02607      DISPLAY 'NUMBER OF NET PAY RECORDS PROCESSED - '             
02608          W-NUMBER-NET-PAY.                                        
02609      DISPLAY 'NUMBER OF NET PAY PROCESSED APR LESS THAN 1 - '     
02610          W-APR-LESS-THAN-1.                                       
02611                                                                   
02612      CLOSE ACC-MSTR                                               
02613            GAAP-EXTRACT                                           
02614            GAAP-LAST-NET-PAY                                      
02615            GAAP-NEW                                               
02616            GAAP-NEW-NET-PAY                                       
062104           ME-ECS080-BALANCE
02617            PRNTR                                                  
CIDMOD           DISPLAY-PRT.
02618                                                                   
02619      IF AM-FILE-STATUS NOT = '00'                                 
02620         MOVE ' ERROR ON ERACCTT, CLOSE ' TO WS-ABEND-MESSAGE      
02621         MOVE AM-FILE-STATUS TO WS-ABEND-FILE-STATUS               
02622         GO TO ABEND-PGM.                                          
02623                                                                   
02624                              COPY ELCPRTC.                        
02625                                                                   
02626      IF ME-NO-UPDATE                                              
02627         GO TO 9900-CONTINUE.                                      
02628                                                                   
02629      OPEN I-O ERMEBL.                                             
02630                                                                   
02631      IF ERMEBL-FILE-STATUS  = '00' OR '97'                        
02632          NEXT SENTENCE                                            
02633        ELSE                                                       
02634          MOVE 'N'                TO ME-UPDATE-FLAG.               
02635                                                                   
02636      MOVE DTE-CLIENT             TO ME-COMPANY.                   
02637      MOVE MONTH-END-MOYR         TO ME-MOYR.                      
02638                                                                   
02639      IF ME-DO-UPDATE                                              
02640          READ ERMEBL INVALID KEY                                  
02641          MOVE 'N'                TO ME-UPDATE-FLAG                
02642          CLOSE ERMEBL.                                            
02643                                                                   
02644      IF ME-DO-UPDATE                                              
070714         move tot-gross          to me-080-mort-resv
02645          MOVE ERR-COUNT          TO ME-080-MORT-ERRS              
02647          MOVE ME-CNDS-DATE       TO ME-080-RUN-DT                 
02648          ACCEPT WS-TIME-OF-DAY   FROM TIME                        
02650          ADD 1                   TO ME-080-RUN-CT                 
02651          REWRITE MONTH-END-BALANCES                               
02652          CLOSE ERMEBL.                                            
02653                                                                   
02654      IF ME-DO-UPDATE                                              
02655          DISPLAY 'MONTH-END BALANCES POSTED'                      
02656      ELSE                                                         
02657          DISPLAY 'MONTH-END BALANCES NOT POSTED'.                 
02658                                                                   
02659  9900-CONTINUE.                                                   
02660                                                                   
02661      DISPLAY ' '                                                  
02662      DISPLAY '****** END OF MESSAGES FROM ECS080 ******'.         
02663                                                                   
02664      GOBACK.                                                      
02665                                                                   
02666  ABEND-PGM.                                                       
02667                       COPY ELCABEND.                              
02341                                                                   
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
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     WRITE  DISPLAY-REC FROM DISPLAY-HD-2.                        
CIDMOD     ADD +1 TO DIS-LINE-CNT.                                      
CIDMOD     MOVE   SPACES TO DISPLAY-REC.                                
CIDMOD     WRITE  DISPLAY-REC.                                          
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD                                                                  
CIDMOD 8600-HD-EXIT.                                                    
CIDMOD     EXIT.                                                        
CIDMOD                                                                  
