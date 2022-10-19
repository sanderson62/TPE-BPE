00001  IDENTIFICATION DIVISION.                                         
00002                                                                   
00003  PROGRAM-ID.                 EL519B.
00004 *              PROGRAM CONVERTED BY                               
00005 *              COBOL CONVERSION AID PO 5785-ABJ                   
00006 *              CONVERSION DATE 07/15/94 11:20:33.                 
00007 *            PROGRAM REVIEWED FOR YEAR 2000 COMPLIANCE            
00008 *                            VMOD=2.014.                          
00009                                                                   
00010 *AUTHOR.     LOGIC, INC.                                          
00011 *            DALLAS, TEXAS.                                       
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
00026 *           THIS PROGRAM WILL BUILD A RECORD FOR EVERY BANK       
00027 *        REFERENCED IN LEVELS 1-10 OF THE ACCOUNT MASTER.         
00028                                                                   
00029 *           EACH RECORD WILL CONTAIN A POINTER TO ALL ACCOUNT     
00030 *        MASTERS REFERENCING THAT BANK                            
061203******************************************************************
061203*                   C H A N G E   L O G
061203*
061203* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
061203*-----------------------------------------------------------------
061203*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
061203* EFFECTIVE    NUMBER
061203*-----------------------------------------------------------------
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
061203******************************************************************
00031                                                                   
00032  ENVIRONMENT DIVISION.                                            
00033  INPUT-OUTPUT SECTION.                                            
00034  FILE-CONTROL.                                                    
00035                                                                   
00036      SELECT SORT-FILE    ASSIGN TO SYS001-UT-2314-S-SORTWK1.      
00037                                                                   
00038      SELECT ERACCT       ASSIGN TO SYS011-FBA1-ERACCT             
00039              ORGANIZATION IS INDEXED                              
00040              ACCESS IS DYNAMIC                                    
00041              RECORD KEY IS AM-CONTROL-PRIMARY                     
00042              FILE STATUS IS ERACCT-FILE-STATUS.                   
00043                                                                   
00044      SELECT ERBXRF       ASSIGN TO SYS012-FBA1-ERBXRF             
00045              ORGANIZATION IS INDEXED                              
00046              ACCESS IS DYNAMIC                                    
00047              RECORD KEY IS BK-CONTROL-PRIMARY                     
00048              FILE STATUS IS ERBXRF-FILE-STATUS.                   
00049                                                                   
00050      SELECT DISK-DATE    ASSIGN TO SYS019-UT-FBA1-S-SYS019.       
00051                                                                   
00052  EJECT                                                            
00053  DATA DIVISION.                                                   
00054  FILE SECTION.                                                    
00055                                                                   
00056  FD  ERACCT.                                                      
00057                                                                   
00058      COPY ERCACCT.                                                
00059                                                                   
00060  EJECT                                                            
00061  FD  ERBXRF.                                                      
00062                                                                   
00063      COPY ERCBXRF.                                                
00064                                                                   
00065  EJECT                                                            
00066  SD  SORT-FILE.                                                   
00067                                                                   
00068  01  SORT-REC.                                                    
00069     03  SORT-PARM.                                                
00070      12  SR-COMPANY              PIC X.                           
00071      12  SR-CARRIER              PIC X.                           
00072      12  SR-GROUPING             PIC X(6).                        
00073      12  SR-BANK                 PIC X(10).                       
00074      12  SR-STATE                PIC XX.                          
00075      12  SR-ACCOUNT              PIC X(10).                       
00076      12  SR-EXPIRATION-DT        PIC XX.                          
00077      12  SR-LEVEL                PIC 9.                           
00078      12  SR-LAST-BILL-DT         PIC XX.                          
00079      12  SR-EFFECTIVE-DT         PIC XX.                          
00080     03  SORT-REST.                                                
00081      12  SR-AM-CARRIER           PIC X.                           
00082      12  SR-AM-GROUPING          PIC X(6).                        
00083  EJECT                                                            
00084  FD  DISK-DATE                   COPY ELCDTEFD.                   
00085  EJECT                                                            
00086  WORKING-STORAGE SECTION.                                         
00087                                                                   
00088  77  FILLER  PIC X(32)  VALUE '********************************'. 
00089  77  FILLER  PIC X(32)  VALUE '*   EL519B WORKING STORAGE     *'. 
00090  77  FILLER  PIC X(32)  VALUE '******** VMOD=2.001 ************'. 
00091                                                                   
00092  77  DEL-CTR                     PIC S9(7)   COMP-3  VALUE +0.    
00093  77  ADD-CTR                     PIC S9(7)   COMP-3  VALUE +0.    
00094  77  REC-CTR                     PIC S9(7)   COMP-3  VALUE +0.    
DAN    77  AGT-MAX                     PIC S9(4)   COMP    VALUE +725.
00096  EJECT                                                            
00097      COPY ELCDTECX.                                               
00098  EJECT                                                            
00099      COPY ELCDTEVR.                                               
00100  EJECT                                                            
00101  01  SORTED-XRF.                                                  
00102     03  SORTED-PRIMARY.                                           
00103       05  SX-COMPANY             PIC X      VALUE LOW-VALUES.     
00104       05  SX-CARRIER             PIC X      VALUE SPACES.         
00105       05  SX-GROUPING            PIC X(6)   VALUE SPACES.         
00106       05  SX-BANK                PIC X(10)  VALUE SPACES.         
00107     03  SX-STATE                 PIC XX     VALUE SPACES.         
00108     03  SX-ACCOUNT               PIC X(10)  VALUE SPACES.         
00109     03  SX-EXPIRATION-DT         PIC XX     VALUE LOW-VALUES.     
00110     03  SX-LEVEL                 PIC 9      VALUE 0.              
00111     03  SX-LAST-BILL-DT          PIC XX     VALUE LOW-VALUES.     
00112     03  SX-EFFECTIVE-DT          PIC XX     VALUE LOW-VALUES.     
00113     03  SX-AM-CARRIER            PIC X      VALUE SPACE.          
00114     03  SX-AM-GROUPING           PIC X(6)   VALUE SPACES.         
00115                                                                   
00116  01  SORTED-SECONDARY-XRF.                                        
00117      12  SSX-AM-CARRIER          PIC X      VALUE SPACE.          
00118      12  SSX-AM-GROUPING         PIC X(6)   VALUE SPACES.         
00119      12  SSX-STATE               PIC XX     VALUE SPACES.         
00120      12  SSX-ACCOUNT             PIC X(10)  VALUE SPACES.         
00121                                                                   
00122  01  LAST-XRF.                                                    
00123     03  LAST-PRIMARY.                                             
00124       05  LS-COMPANY             PIC X       VALUE LOW-VALUES.    
00125       05  LS-CARRIER             PIC X       VALUE SPACE.         
00126       05  LS-GROUPING            PIC X(6)    VALUE SPACES.        
00127       05  LS-BANK                PIC X(10)   VALUE SPACES.        
00128     03  LS-STATE                 PIC XX      VALUE SPACES.        
00129     03  LS-ACCOUNT               PIC X(10)   VALUE SPACES.        
00130     03  LS-EXPIRATION-DT         PIC XX      VALUE LOW-VALUES.    
00131     03  LS-LEVEL                 PIC 9       VALUE 0.             
00132     03  LS-LAST-BILL-DT          PIC XX      VALUE SPACES.        
00133     03  LS-EFFECTIVE-DT          PIC XX      VALUE LOW-VALUES.    
00134     03  LS-AM-CARRIER            PIC X       VALUE SPACE.         
00135     03  LS-AM-GROUPING           PIC X(6)    VALUE SPACES.        
00136                                                                   
00137  01  LAST-SECONDARY-XRF.                                          
00138      12  LSX-AM-CARRIER          PIC X      VALUE SPACE.          
00139      12  LSX-AM-GROUPING         PIC X(6)   VALUE SPACES.         
00140      12  LSX-STATE               PIC XX     VALUE SPACES.         
00141      12  LSX-ACCOUNT             PIC X(10)  VALUE SPACES.         
00142                                                                   
00143  01  WS-AREA.                                                     
00144      12  TEMP-ACC                PIC 9(6)   VALUE ZERO.           
00145      12  TEMP-ACC-X REDEFINES TEMP-ACC  PIC X(6).                 
00146      12  TEMP-CODE               PIC X      VALUE SPACE.          
00147      12  WS-ZERO                 PIC S9     VALUE +0  COMP-3.     
00148      12  WS-EXPIRE-DT.                                            
00149          16  FILLER              PIC XX     VALUE SPACES.         
00150          16  WS-EXP-FILLER       PIC X(4)   VALUE SPACES.         
00151      12  WS-RETURN-CODE          PIC S9(4)  COMP  VALUE ZEROS.    
00152      12  NDX                     PIC S9(4)  COMP  VALUE ZEROS.    
00153      12  AGT                     PIC S9(4)  COMP  VALUE ZEROS.    
00154      12  WS-ABEND-MESSAGE        PIC X(80)  VALUE SPACES.         
00155      12  WS-ABEND-FILE-STATUS    PIC XX     VALUE ZEROS.          
00156      12  ABEND-CODE              PIC X(4)   VALUE ZEROS.          
00157      12  ABEND-OPTION            PIC X      VALUE 'Y'.            
00158      12  PGM-SUB                 PIC S999   COMP-3  VALUE +519.   
00159      12  ERACCT-FILE-STATUS.                                      
00160          16  ERACCT-STAT-1       PIC X   VALUE ZERO.              
00161          16  ERACCT-STAT-2       PIC X   VALUE ZERO.              
00162      12  ERBXRF-FILE-STATUS.                                      
00163          16  ERBXRF-STAT-1       PIC X   VALUE ZERO.              
00164          16  ERBXRF-STAT-2       PIC X   VALUE ZERO.              
DAN        12  ERROR-LINE.                                              
DAN            16  FILLER  PIC X(24) VALUE ' *** WARNING ***  BANK  '.  
DAN            16  ERR-AGT PIC X(10) VALUE SPACE.                       
DAN            16  FILLER  PIC X(20) VALUE ' EXCEEDED MAX RECORD'.      
DAN            16  FILLER  PIC X(10) VALUE ' COUNT BY '.                
DAN            16  ERR-CNT PIC ZZZ9  VALUE ZERO.                        
DAN            16  FILLER  PIC X(14) VALUE ' RECORDS (MAX='.            
DAN            16  ERR-MAX PIC ZZZ9  VALUE ZERO.                        
DAN            16  FILLER  PIC X     VALUE ')'.                         
00165  EJECT                                                            
00166      COPY ELCDATE.                                                
00167  EJECT                                                            
00168                                                                   
00169  PROCEDURE DIVISION.                                              
00170                                                                   
00171  0000-LOAD-DATE-WS.              COPY ELCDTERX.                   
00172  EJECT                                                            
00173  0010-CHECK-SYSTEM-D.                                             
00174                                                                   
00175      IF DTE-CLIENT  =  'NCL'                                      
00176          MOVE +950              TO  AGT-MAX.                      
00177                                                                   
00181  0020-DELETE-ERBXRF-FILE.                                         
00182      OPEN I-O ERBXRF.                                             
00183                                                                   
00184      IF ERBXRF-FILE-STATUS NOT = ZERO AND '97'                    
00185          MOVE 'ERROR OCCURED OPEN - ERBXRF'                       
00186                                  TO  WS-ABEND-MESSAGE             
00187          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00188          GO TO ABEND-PGM.                                         
00189                                                                   
00190      MOVE LOW-VALUES             TO BK-CONTROL-PRIMARY.           
00191      MOVE DTE-CLASIC-COMPANY-CD  TO BK-COMPANY-CD.                
00192                                                                   
00193      START ERBXRF                                                 
00194             KEY NOT LESS BK-CONTROL-PRIMARY.                      
00195                                                                   
00196      IF ERBXRF-FILE-STATUS = '23'                                 
00197          GO TO 0050-END-ERBXRF-DELETE.                            
00198                                                                   
00199      IF ERBXRF-FILE-STATUS NOT = ZERO                             
00200          MOVE 'ERROR OCCURED START - ERBXRF'                      
00201                                  TO  WS-ABEND-MESSAGE             
00202          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00203          GO TO ABEND-PGM.                                         
00204                                                                   
00205                                                                   
00206  0030-READ-ERBXRF.                                                
00207      READ ERBXRF  NEXT RECORD.                                    
00208                                                                   
00209      IF ERBXRF-STAT-1 = '1'                                       
00210         GO TO 0050-END-ERBXRF-DELETE.                             
00211                                                                   
00212      IF ERBXRF-FILE-STATUS NOT = ZERO                             
00213          MOVE 'ERROR OCCURED READ - ERBXRF'                       
00214                                  TO  WS-ABEND-MESSAGE             
00215          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00216          GO TO ABEND-PGM.                                         
00217                                                                   
00218      IF BK-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
00219         GO TO 0050-END-ERBXRF-DELETE.                             
00220                                                                   
00221  0040-DELETE-ERBXRF.                                              
00222      DELETE ERBXRF RECORD.                                        
00223                                                                   
00224      IF ERBXRF-FILE-STATUS NOT = ZERO                             
00225          MOVE 'ERROR OCCURED DELETE - ERBXRF'                     
00226                                  TO  WS-ABEND-MESSAGE             
00227          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00228          GO TO ABEND-PGM.                                         
00229                                                                   
00230      ADD 1 TO DEL-CTR.                                            
00231      GO TO 0030-READ-ERBXRF.                                      
00232                                                                   
00233  0050-END-ERBXRF-DELETE.                                          
00234      CLOSE ERBXRF.                                                
00235                                                                   
00236      IF ERBXRF-FILE-STATUS NOT = ZERO                             
00237          MOVE 'ERROR OCCURED CLOSE - ERBXRF'                      
00238                                  TO  WS-ABEND-MESSAGE             
00239          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00240          GO TO ABEND-PGM.                                         
00241                                                                   
00242  EJECT                                                            
00243  SORT-ROUTINE SECTION.                                            
00244                                                                   
00245  0100-SORT-RTN.                                                   
DAN        MOVE 'SORTMSG' TO SORT-MESSAGE
00246      SORT SORT-FILE ON ASCENDING SORT-PARM                        
00247          INPUT PROCEDURE  INPUT-RTN THRU INPUT-XIT                
00248          OUTPUT PROCEDURE OUTPUT-RTN THRU OUTPUT-XIT.             
00249                                                                   
00250      IF SORT-RETURN NOT = ZEROS AND 4                             
00251          MOVE '0101'             TO ABEND-CODE                    
00252          GO TO ABEND-PGM.                                         
00253                                                                   
00254      GO TO 9999-END-OF-JOB.                                       
00255                                                                   
00256     EJECT                                                         
00257                                                                   
00258  INPUT-RTN SECTION.                                               
00259                                                                   
00260  0200-OPEN-ERACCT.                                                
00261      OPEN INPUT ERACCT.                                           
00262                                                                   
00263      IF ERACCT-FILE-STATUS  = '00' OR '97'                        
00264          NEXT SENTENCE                                            
00265        ELSE                                                       
00266          MOVE 'ERROR OCCURED OPEN - ERACCT'                       
00267                                  TO  WS-ABEND-MESSAGE             
00268          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00269          GO TO ABEND-PGM.                                         
00270                                                                   
00271      MOVE LOW-VALUES             TO AM-CONTROL-PRIMARY.           
00272      MOVE DTE-CLASIC-COMPANY-CD  TO AM-COMPANY-CD.                
00273                                                                   
00274      START ERACCT                                                 
00275             KEY NOT LESS AM-CONTROL-PRIMARY.                      
00276                                                                   
00277      IF ERACCT-FILE-STATUS = '23'                                 
00278          GO TO 0600-END-ERACCT.                                   
00279                                                                   
00280      IF ERACCT-FILE-STATUS NOT = ZERO                             
00281          MOVE 'ERROR OCCURED START - ERACCT'                      
00282                                  TO  WS-ABEND-MESSAGE             
00283          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00284          GO TO ABEND-PGM.                                         
00285                                                                   
00286  0300-READ-ERACCT.                                                
00287      READ ERACCT  NEXT RECORD.                                    
00288                                                                   
00289      IF ERACCT-STAT-1 = '1'                                       
00290         GO TO 0600-END-ERACCT.                                    
00291                                                                   
00292      IF ERACCT-FILE-STATUS NOT = ZERO                             
00293          MOVE 'ERROR OCCURED READ - ERACCT'                       
00294                                  TO  WS-ABEND-MESSAGE             
00295          MOVE ERACCT-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00296          GO TO ABEND-PGM.                                         
00297                                                                   
00298      IF AM-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD                 
00299         GO TO 0600-END-ERACCT.                                    
00300                                                                   
00301      MOVE 1                      TO NDX.                          
00302                                                                   
00303  0400-GET-ACCT-COMM.                                              
00304                                                                   
00305      IF AM-COM-TYP (NDX) NOT = 'K'
00306 *       DISPLAY 'NOT EQUAL ' AM-MSTR-CNTRL                        
00307         GO TO 0550-ADD-NDX.                                       
00308                                                                   
00309 *    DISPLAY 'INCLUDE   ' AM-MSTR-CNTRL.                          
00310                                                                   
00311      IF AM-AGT (NDX) = ZEROS  OR  SPACES                          
00312          GO TO 0550-ADD-NDX.                                      
00313                                                                   
00314      IF DTE-CLIENT  =  'DCC'                                      
00315         IF AM-AGT (NDX) EQUAL '000TESTGAP'
                 MOVE '00TESTBANK' TO AM-AGT (NDX)
              END-IF
           END-IF
00321                                                                   
00322      MOVE AM-COMPANY-CD          TO SR-COMPANY.                   
00323      MOVE AM-AGT (NDX)           TO SR-BANK
00324                                                                   
00325 ***************************************************************   
00326 *    VERIFY COMPENSATION CONTROL.                             *   
00327 ***************************************************************   
00328                                                                   
00329      IF DTE-COMPENSATION-ACCESS = '1' OR '3'                      
00330         MOVE ZEROS               TO SR-CARRIER                    
00331      ELSE                                                         
00332         MOVE AM-CARRIER          TO SR-CARRIER.                   
00333                                                                   
00334      IF DTE-COMPENSATION-ACCESS = '2' OR '3'                      
00335         MOVE ZEROS               TO SR-GROUPING                   
00336      ELSE                                                         
00337         MOVE AM-GROUPING         TO SR-GROUPING.                  
00338                                                                   
00339      MOVE AM-STATE               TO SR-STATE.                     
00340      MOVE AM-ACCOUNT             TO SR-ACCOUNT.                   
00341      MOVE AM-EXPIRATION-DT       TO SR-EXPIRATION-DT.             
00342      MOVE NDX                    TO SR-LEVEL.                     
00343      MOVE AM-EFFECTIVE-DT        TO SR-EFFECTIVE-DT.              
00344      MOVE LOW-VALUES             TO SR-LAST-BILL-DT.              
00345      MOVE AM-CARRIER             TO SR-AM-CARRIER.                
00346      MOVE AM-GROUPING            TO SR-AM-GROUPING.               
00347                                                                   
00348  0500-RELEASE-SORT.                                               
00349      RELEASE SORT-REC.                                            
00350                                                                   
00351  0550-ADD-NDX.                                                    
00352      ADD 1 TO NDX.                                                
00353                                                                   
00354      IF NDX GREATER 10                                            
00355         GO TO 0300-READ-ERACCT.                                   
00356                                                                   
00357      GO TO 0400-GET-ACCT-COMM.                                    
00358                                                                   
00359  0600-END-ERACCT.                                                 
00360      CLOSE ERACCT.                                                
00361                                                                   
00362  INPUT-XIT.                                                       
00363       EXIT.                                                       
00364                                                                   
00365     EJECT                                                         
00366                                                                   
00367  OUTPUT-RTN SECTION.                                              
00368                                                                   
00369  1000-OPEN-OUTPUT.                                                
00370      MOVE LOW-VALUES             TO  LAST-SECONDARY-XRF.          
00371                                                                   
00372      OPEN I-O ERBXRF.                                             
00373                                                                   
00374      IF ERBXRF-FILE-STATUS  = '00' OR '97'                        
00375          NEXT SENTENCE                                            
00376        ELSE                                                       
00377          MOVE 'ERROR OCCURED OPEN - ERBXRF'                       
00378                                  TO  WS-ABEND-MESSAGE             
00379          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00380          GO TO ABEND-PGM.                                         
00381                                                                   
00382  1010-RETURN-SORT.                                                
00383      RETURN SORT-FILE INTO SORTED-XRF AT END                      
00384          GO TO 1500-END-OUTPUT.                                   
00385      ADD 1 TO ADD-CTR.                                            
00386                                                                   
00387      IF AGT = ZERO                                                
00388         GO TO 1200-SET-UP-RECORD.                                 
00389                                                                   
00407      IF LAST-PRIMARY = SORTED-PRIMARY                             
00408          GO TO 1300-ADD-POINTER.                                  
00409                                                                   
00410  1100-WRITE-RECORD.                                               
DAN        IF AGT  GREATER THAN  AGT-MAX                                
DAN           MOVE LS-BANK  TO ERR-AGT
DAN           MOVE AGT-MAX  TO ERR-MAX
DAN           COMPUTE ERR-CNT = AGT - AGT-MAX
DAN           DISPLAY ERROR-LINE
DAN           DISPLAY ' '
DAN        END-IF                                                       
DAN
00411      WRITE BANK-CROSS-REFERENCE
00412                                                                   
00413      IF ERBXRF-FILE-STATUS NOT = ZERO                             
00414          MOVE 'ERROR OCCURED WRITE - ERBXRF'                      
00415                                  TO  WS-ABEND-MESSAGE             
00416          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00417          GO TO ABEND-PGM.                                         
00418                                                                   
00419      ADD 1 TO REC-CTR.                                            
00420      MOVE 0                      TO AGT.                          
00421                                                                   
00422  1200-SET-UP-RECORD.                                              
00423      MOVE SX-COMPANY             TO BK-COMPANY-CD.                
00424      MOVE 'BK'                   TO BK-RECORD-ID.                 
00425      MOVE SX-CARRIER             TO BK-CARRIER.                   
00426      MOVE SX-GROUPING            TO BK-GROUPING.                  
00427      MOVE SX-BANK                TO BK-BANK-NO
00428                                                                   
00429      MOVE WS-CURRENT-DATE        TO DC-GREG-DATE-1-EDIT.          
00430      MOVE '2'                    TO DC-OPTION-CODE.               
00431      PERFORM 8500-DATE-CONVERT THRU 8500-EXIT                     
00432      MOVE DC-BIN-DATE-1          TO BK-LAST-MAINT-DT
           MOVE 'E519'                 TO BK-LAST-MAINT-USER
           MOVE +0200000               TO BK-LAST-MAINT-HHMMSS
           .
00433                                                                   
00434  1300-ADD-POINTER.                                                
00435      ADD 1 TO AGT.                                                
00436                                                                   
00437      IF AGT  GREATER THAN  AGT-MAX                                
00438         IF DTE-CLIENT  =  'CID'
030612                       OR 'DCC' OR 'AHL'
DAN***            DISPLAY 'GX AGT POINTER EXCEEDED - ' SX-BANK
00440             GO TO 1010-RETURN-SORT                                
00441         ELSE                                                      
00442             MOVE 'BK-BANK-POINTER EXCEEDED MAX'                  
00443                                     TO  WS-ABEND-MESSAGE          
00444             MOVE '0213'             TO  ABEND-CODE                
00445             GO TO ABEND-PGM.                                      
00446                                                                   
00447  1310-MOVE-DATA.                                                  
00448      MOVE AGT                    TO BK-BANK-POINTER-CNT
00449 *    MOVE SX-AM-CARRIER          TO BK-AM-CARRIER (AGT).          
00450 *    MOVE SX-AM-GROUPING         TO BK-AM-GROUPING (AGT).         
00451      MOVE SX-STATE               TO BK-AM-STATE (AGT).            
00452      MOVE SX-ACCOUNT             TO BK-AM-ACCOUNT (AGT).          
00454      MOVE SX-EXPIRATION-DT       TO BK-AM-EXP-DT (AGT).    
00455      MOVE SX-EFFECTIVE-DT        TO BK-AM-EFF-DT (AGT).           
00457                                                                   
00458      MOVE SORTED-PRIMARY         TO LAST-PRIMARY.                 
00459                                                                   
00464      GO TO 1010-RETURN-SORT.                                      
00465                                                                   
00466  1500-END-OUTPUT.                                                 
00467      IF AGT GREATER 0                                             
00468          ADD 1 TO REC-CTR                                         
00469          WRITE BANK-CROSS-REFERENCE.                             
00470                                                                   
00471      CLOSE ERBXRF.                                                
00472                                                                   
00473      IF ERBXRF-FILE-STATUS NOT = ZERO                             
00474          MOVE 'ERROR OCCURED CLOSE - ERBXRF'                      
00475                                  TO  WS-ABEND-MESSAGE             
00476          MOVE ERBXRF-FILE-STATUS TO  WS-ABEND-FILE-STATUS         
00477          GO TO ABEND-PGM.                                         
00478                                                                   
00479  OUTPUT-XIT.                                                      
00480        EXIT.                                                      
00481   EJECT                                                           
00482                                                                   
00483  8500-DATE-CONVERT.                                               
00484      CALL 'ELDATCX'  USING DATE-CONVERSION-DATA.                  
00485                                                                   
00486      IF DC-ERROR-CODE NOT = SPACE                                 
00487         MOVE ZEROS               TO DC-CONVERSION-DATES.          
00488                                                                   
00489  8500-EXIT.                                                       
00490      EXIT.                                                        
00491                                                                   
00492  END-JOB SECTION.                                                 
00493                                                                   
00494  9999-END-OF-JOB.                                                 
00495      DISPLAY '**** GA-XRF CREATE ****'.                           
00496      DISPLAY 'GA-XRF RECORDS DELETED  = ' DEL-CTR.                
00497      DISPLAY 'GA-XRF RECORDS DELETED  = ' DEL-CTR UPON CONSOLE.   
00498      DISPLAY 'ACCOUNT EXTRACTS        = ' ADD-CTR.                
00499      DISPLAY 'ACCOUNT EXTRACTS        = ' ADD-CTR UPON CONSOLE.   
00500      DISPLAY 'GA-XRF RECORDS ADDED    = ' REC-CTR.                
00501      DISPLAY 'GA-XRF RECORDS ADDED    = ' REC-CTR UPON CONSOLE.   
00502      GOBACK.                                                      
00503                                                                   
00504  ABEND-PGM.                                                       
00505                      COPY ELCABEND.                               
