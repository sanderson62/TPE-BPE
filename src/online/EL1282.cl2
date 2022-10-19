       IDENTIFICATION DIVISION.                                                 
       PROGRAM-ID. EL1282.                                                      
       AUTHOR.     CENTRAL STATES HEALTH AND LIFE                               
                   OMAHA, NEBRASKA                                              
       DATE-COMPILED.                                                           
      *SECURITY.   *****************************************************        
      *            *                                                   *        
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *        
      *            *   HEALTH AND LIFE                                 *        
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *        
      *            *   OF CSO.        IS EXPRESSLY PROHIBITED WITHOUT  *        
      *            *   THE PRIOR WRITTEN PERMISSION OF CENTRAL STATES. *        
      *            *                                                   *        
      *            *****************************************************        
                                                                                
      *REMARKS.                                                                 
      *        THIS PROGRAM PROVIDES THE BROWSE NECESSARY FOR                   
      *    THE PURGED CERT LOOK-UP.                                             
                                                                                
      *    SCREENS     - EL128B - PURGED CERT LOOK-UP MATCH LIST                
                                                                                
      *    ENTERED BY  - EL128 - PURGED CERT QUALIFICATION                      
                                                                                
      *    EXIT TO     - EL128                                                  
                                                                                
      *    INPUT FILE  - ELPURG - PURGED CERT MASTER                            
                                                                                
      *    OUTPUT FILE - NONE                                                   
                                                                                
      *    COMMAREA    - PASSED.  IF A PURGED CERT IS SELECTED, THE             
      *                  CONTROL OF THAT CERTIFICATE IS PLACED IN THE           
      *                  APPROPRIATE FIELDS OF THE COMMAAREA FOR                
      *                  REFERENCE BY SUCCESSIVE PROGRAMS.  THE PROGRAM         
      *                  WORK AREA OF THE COMMAREA IS USED TO PASS THE          
      *                  RECORD KEY INFORMATION NEEDED BY EL1282 TO             
      *                  LOCATE THE CERTIFICATE.                                
                                                                                
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON             
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE         
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE        
      *                  ENTRIES (XCTL FROM CICS VIA EXXD) THE SCREEN           
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE           
      *                  MAINTENANCE TYPE INDICATED.                            
           EJECT                                                                
       ENVIRONMENT DIVISION.                                                    
                                                                                
       DATA DIVISION.                                                           
                                                                                
       WORKING-STORAGE SECTION.                                                 
       77  LCP-ONCTR-01                  PIC S9(8) COMP-3 VALUE ZERO.           
       77  LCP-ONCTR-02                  PIC S9(8) COMP-3 VALUE ZERO.           
                                                                                
       77  FILLER  PIC X(32)  VALUE '********************************'.         
       77  FILLER  PIC X(32)  VALUE '*   EL1282 WORKING STORAGE     *'.         
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'.         
                                                                                
       01  FLA-WS.                                                              
           05  SAVE-NAME                   PIC X(15).                           
           05  FILLER REDEFINES SAVE-NAME.                                      
               10 SV-BYTE                  PIC X OCCURS 15.                     
           05  A-SUB                       PIC 99.                              
           05  B-SUB                       PIC 99.                              
                                                                                
                                           COPY ELCSCTM.                        
                                                                                
                                           COPY ELCSCRTY.                       
                                                                                
       01  WS-DATE-AREA.                                                        
           05  SAVE-DATE                   PIC X(8)     VALUE SPACES.           
           05  SAVE-BIN-DATE               PIC X(2)     VALUE SPACES.           
                                                                                
       01  FILLER                          COMP-3.                              
           05  WS-NOT-FOUND                PIC S9       VALUE ZERO.             
           05  WS-ST-REC-NOT-FOUND         PIC S9       VALUE ZERO.             
           05  TIME-IN                     PIC S9(7)    VALUE ZERO.             
           05  TIME-OUT                    REDEFINES                            
               TIME-IN                     PIC S9(3)V9(4).                      
                                                                                
           05  WS-MONTH-WORK               PIC S9(3)    VALUE ZERO.             
           05  WS-YEAR-WORK                PIC S9(3)    VALUE ZERO.             
           05  WS-CERT-SW                  PIC S9       VALUE ZERO.             
              88  WS-NO-CERT-FOUND                      VALUE ZERO.             
           05  SUB                         PIC S99      VALUE ZERO.             
                                                                                
       01  FILLER                          COMP                                 
                                           SYNC.                                
           05  WS-INDEX                    PIC S9(4)    VALUE ZERO.             
           05  WS-TS-LENGTH                PIC S9(4)    VALUE +1920.            
           05  WS-WORK-LENGTH              PIC S9(4)    VALUE +640.             
           05  WS-AIX-RECORD-COUNT         PIC S9(4)    VALUE ZERO.             
                                                                                
       01  FILLER.                                                              
                                                                                
           05  WS-COMPARE-INDICATOR        PIC  X.                              
               88 NAME-FOUND                         VALUE SPACE.               
               88 NAME-NOT-FOUND                     VALUE 'X'.                 
           05  WS-NAME-INDEX               PIC S9(4)  COMP.                     
           05  WS-CM-NAME                  PIC X(15).                           
           05  WS-CM-NAME-CHAR REDEFINES                                        
               WS-CM-NAME                  PIC X                                
                                           OCCURS 15.                           
           05  WS-PI-NAME                  PIC  X(15).                          
           05  WS-PI-NAME-CHAR  REDEFINES                                       
               WS-PI-NAME                  PIC  X                               
                                           OCCURS 15.                           
                                                                                
           05  QID.                                                             
               10  QID-TERM                PIC X(4).                            
               10  FILLER                  PIC X(4)     VALUE '128A'.           
           05  QID-ITEM                    PIC S9(4)    VALUE +1  COMP.         
                                                                                
           05  WS-CONTROL-FILE-KEY.                                             
               10  WS-CFK-COMPANY-ID           PIC X(3).                        
               10  WS-CFK-RECORD-TYPE          PIC X.                           
               10  WS-CFK-ACCESS-TYPE.                                          
                   15  WS-CFK-BENE-ACCESS.                                      
                       20  FILLER              PIC XX.                          
                       20  WS-CFK-BENEFIT-NO   PIC XX.                          
                   15  WS-CFK-PROC-ACCESS REDEFINES WS-CFK-BENE-ACCESS.         
                       20  WS-CFK-PROD-ID      PIC X(04).                       
               10  WS-CFK-SEQUENCE-NO          PIC S9(4)  COMP.                 
                                                                                
           05  WS-MAPSET-NAME              PIC X(8)     VALUE 'EL128S'.         
           05  WS-MAP-NAME                 PIC X(8)     VALUE 'EL128B'.         
                                                                                
           05  FILLER                      REDEFINES                            
               WS-MAP-NAME.                                                     
               10  FILLER                  PIC XX.                              
               10  WS-MAP-NUMBER           PIC X(4).                            
               10  FILLER                  PIC XX.                              
                                                                                
           05  THIS-PGM                    PIC X(8)     VALUE 'EL1282'.         
           05  ELRTRM                      PIC X(8)     VALUE 'ELRTRM'.         
                                                                                
           05  WS-CONTROL-FILE-DSID        PIC X(8)     VALUE 'ELCNTL'.         
                                                                                
           05  WS-TRANS-ID                 PIC X(4)     VALUE 'EXXD'.           
                                                                                
           05  WS-TEMP-STORAGE-KEY.                                             
               10  WS-TSK-TERM-ID          PIC X(4)     VALUE 'XXXX'.           
               10  FILLER                  PIC X(4)     VALUE '1282'.           
                                                                                
           05  WS-EL1283-TS.                                                    
               10  WS-TS1-TERM-ID          PIC X(4)     VALUE 'XXXX'.           
               10  FILLER                  PIC X(4)     VALUE '1283'.           
                                                                                
           05  WS-INITIALS.                                                     
               10  WS-INIT1                PIC X.                               
               10  WS-INIT2                PIC X.                               
                                                                                
           05  WS-CURRENT-DATE             PIC XX.                              
                                                                                
           05  WS-KEY-HOLD.                                                     
               10  WS-KH-CHAR              PIC X                                
                   OCCURS 33 TIMES         INDEXED BY KEY-INDEX.                
                                                                                
           05  WS-KEY-INPUT.                                                    
               10  WS-KI-CHAR              PIC X                                
                   OCCURS 33 TIMES         INDEXED BY KEY-INDEX2.               
                                                                                
           05  WS-CALC-RDNXT               PIC S9(8)    VALUE ZERO COMP.        
           05  WS-CERTS-SELECTED           PIC X        VALUE 'N'.              
               88  NO-CERTS-SELECTED                    VALUE 'N'.              
           05  WS-SELECTED-SW              PIC X        VALUE 'N'.              
               88  PREVIOUSLY-SELECTED                  VALUE 'Y'.              
                                                                                
           05  WS-CNTL-REC-FOUND-SW        PIC X        VALUE SPACE.            
           05  WS-NEXT-COMPANY-ID          PIC XXX      VALUE SPACES.           
                                                                                
           05  WS-CERT-CONTROL.                                                 
               10  WS-CARRIER              PIC X.                               
               10  WS-GROUPING             PIC X(6).                            
               10  WS-STATE                PIC XX.                              
               10  WS-ACCOUNT              PIC X(10).                           
               10  WS-CERT-NO              PIC X(11).                           
               10  WS-EFF-DT               PIC XX.                              
                                                                                
           EJECT                                                                
           05  ERROR-MESSAGES.                                                  
               10  ER-0004                 PIC X(4)     VALUE '0004'.           
               10  ER-0008                 PIC X(4)     VALUE '0008'.           
               10  ER-0019                 PIC X(4)     VALUE '0019'.           
               10  ER-0022                 PIC X(4)     VALUE '0022'.           
               10  ER-0029                 PIC X(4)     VALUE '0029'.           
               10  ER-0070                 PIC X(4)     VALUE '0070'.           
               10  ER-0089                 PIC X(4)     VALUE '0089'.           
               10  ER-0130                 PIC X(4)     VALUE '0130'.           
               10  ER-0131                 PIC X(4)     VALUE '0131'.           
               10  ER-0200                 PIC X(4)     VALUE '0200'.           
               10  ER-0201                 PIC X(4)     VALUE '0201'.           
               10  ER-0228                 PIC X(4)     VALUE '0228'.           
               10  ER-0363                 PIC X(4)     VALUE '0363'.           
               10  ER-0659                 PIC X(4)     VALUE '0659'.           
               10  ER-0765                 PIC X(4)     VALUE '0765'.           
               10  ER-2848                 PIC X(4)     VALUE '2848'.           
                                                                                
           EJECT                                                                
                                           COPY ELCNWA.                         
                                                                                
           EJECT                                                                
                                           COPY ELCINTF.                        
                                                                                
           EJECT                                                                
                                           COPY ELC127PI.                       
               16  PI-SUB                       PIC S99.                        
               16  PI-FIRST-TIME-SW             PIC X.                          
               16  PI-EL128-TO-EL130-CNTRL.                                     
                   20  PI-CERT-SELECT-CNT       PIC S9(4)   COMP.               
                   20  PI-CERT-PROCESSED-CNT    PIC S9(4)   COMP.               
                   20  PI-CERT-CONTROLS-EL128 OCCURS 5 TIMES.                   
                       24  PI-EL128-CARRIER     PIC X.                          
                       24  PI-EL128-GROUPING    PIC X(6).                       
                       24  PI-EL128-STATE       PIC XX.                         
                       24  PI-EL128-ACCOUNT     PIC X(10).                      
                       24  PI-EL128-CERT-NO     PIC X(11).                      
                       24  PI-EL128-EFF-DT      PIC XX.                         
               16  PI-PART-KEY-SW               PIC X.                          
               16  PI-PART-FIELD-SW             PIC X.                          
               16  PI-SAVE-KEY-LENGTH           PIC S9(4)    COMP.              
               16  FILLER                       PIC X(136).                     
                                                                                
           EJECT                                                                
                                           COPY EL128S.                         
                                                                                
       01  FILLER REDEFINES EL128BI.                                            
           05  FILLER                      PIC X(37).                           
                                                                                
           05  EL128B-MAP-LINE             OCCURS 8 TIMES                       
               INDEXED BY EL128B-INDEX                                          
                          EL128B-INDEX2.                                        
                                                                                
               10  EL128B-AST-LENGTH       PIC S9(4)    COMP.                   
               10  EL128B-AST-ATTRB        PIC X.                               
               10  EL128B-AST              PIC X.                               
                                                                                
               10  EL128B-CERT-SEL-LENGTH  PIC S9(4)    COMP.                   
               10  EL128B-CERT-SEL-ATTRB   PIC X.                               
               10  EL128B-CERT-SEL         PIC X.                               
                                                                                
               10  EL128B-NAME-LENGTH      PIC S9(4)    COMP.                   
               10  EL128B-NAME-ATTRB       PIC X.                               
               10  EL128B-NAME-O           PIC X(18).                           
                                                                                
               10  EL128B-AGE-LENGTH       PIC S9(4)    COMP.                   
               10  EL128B-AGE-ATTRB        PIC X.                               
               10  EL128B-AGE              PIC 99.                              
                                                                                
               10  EL128B-SEX-LENGTH       PIC S9(4)    COMP.                   
               10  EL128B-SEX-ATTRB        PIC X.                               
               10  EL128B-SEX              PIC X.                               
                                                                                
               10  EL128B-CARRIER-LENGTH   PIC S9(4)    COMP.                   
               10  EL128B-CARRIER-ATTRB    PIC X.                               
               10  EL128B-CARRIER          PIC X.                               
                                                                                
               10  EL128B-GROUP-LENGTH     PIC S9(4)    COMP.                   
               10  EL128B-GROUP-ATTRB      PIC X.                               
               10  EL128B-GROUP            PIC X(6).                            
                                                                                
               10  EL128B-STATE-LENGTH     PIC S9(4)    COMP.                   
               10  EL128B-STATE-ATTRB      PIC X.                               
               10  EL128B-STATE            PIC XX.                              
                                                                                
               10  EL128B-ACCOUNT-LENGTH   PIC S9(4)    COMP.                   
               10  EL128B-ACCOUNT-ATTRB    PIC X.                               
               10  EL128B-ACCOUNT          PIC X(10).                           
                                                                                
               10  EL128B-CERT-NO-LENGTH   PIC S9(4)    COMP.                   
               10  EL128B-CERT-NO-ATTRB    PIC X.                               
               10  EL128B-CERT-NO          PIC X(11).                           
                                                                                
               10  EL128B-EFF-DATE-LENGTH  PIC S9(4)    COMP.                   
               10  EL128B-EFF-DATE-ATTRB   PIC X.                               
               10  EL128B-EFF-DATE         PIC X(8).                            
                                                                                
               10 EL128B-LIFE-INFO-LENGTH PIC S9(4) COMP.                       
               10 EL128B-LIFE-INFO-ATTRB PIC X.                                 
               10 EL128B-LIFE-INFO.                                             
                   15 EL128B-MEMB-LOAN     PIC X(21).                           
                   15 EL128B-LI-ABVR       PIC X(4).                            
                   15 FILLER               PIC X.                               
                   15 EL128B-LI-DESC2      PIC X(3).                            
                   15 FILLER               PIC X.                               
                   15 EL128B-LI-AMT        PIC ZZZ,ZZZ,ZZ9.99-.                 
                   15 EL128B-LI-DATE       REDEFINES                            
                       EL128B-LI-AMT       PIC X(15).                           
                                                                                
               10 EL128B-AH-INFO-LENGTH PIC S9(4)       COMP.                   
               10 EL128B-AH-INFO-ATTRB PIC X.                                   
               10 EL128B-AH-INFO.                                               
                   15 EL128B-AH-ABVR       PIC X(4).                            
                   15 FILLER               PIC X.                               
                   15 EL128B-AH-DESC2      PIC X(3).                            
                   15 FILLER               PIC X.                               
                   15 EL128B-AH-AMT        PIC ZZZZ,ZZ9.99-.                    
                   15 EL128B-AH-DATE       REDEFINES                            
                       EL128B-AH-AMT       PIC X(12).                           
                   15 FILLER               PIC X.                               
                                                                                
                                                                                
           EJECT                                                                
                                           COPY ELCCALC.                        
                                                                                
           EJECT                                                                
                                           COPY ELCDATE.                        
                                                                                
           EJECT                                                                
                                           COPY ELCEMIB.                        
                                                                                
           EJECT                                                                
                                           COPY ELCLOGOF.                       
                                                                                
           EJECT                                                                
                                           COPY ELCATTR.                        
                                                                                
           EJECT                                                                
                                           COPY ELCAID.                         
                                                                                
       01  FILLER REDEFINES DFHAID.                                             
           05  FILLER                      PIC X(8).                            
                                                                                
           05  PF-VALUES                   PIC X                                
               OCCURS 24 TIMES.                                                 
                                                                                
       LINKAGE SECTION.                                                         
                                                                                
       01  DFHCOMMAREA                     PIC X(1024).                         
                                                                                
      *01 PARMLIST   COMP SYNC.                                                 
      *    05  FILLER                      PIC S9(9).                           
      *    05  ELPURG-POINTER              PIC S9(9).                           
      *    05  ELCNTL-POINTER              PIC S9(9).                           
                                                                                
           EJECT                                                                
                                           COPY ELCPURG.                        
           EJECT                                                                
                                           COPY ELCCNTL.                        
           EJECT                                                                
       PROCEDURE DIVISION.                                                      
                                                                                
           MOVE EIBDATE                TO DC-JULIAN-YYDDD.                      
           MOVE '5'                    TO DC-OPTION-CODE.                       
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-GREG-DATE-1-EDIT    TO  SAVE-DATE.                           
           MOVE DC-BIN-DATE-1          TO  SAVE-BIN-DATE.                       
                                                                                
           MOVE DFHCOMMAREA            TO  PROGRAM-INTERFACE-BLOCK.             
                                                                                
           MOVE +2                     TO  EMI-NUMBER-OF-LINES                  
                                           EMI-SWITCH2.                         
                                                                                
      *    NOTE *******************************************************         
      *         *      ACCESS TO THIS MODULE CAN BE ONLY FROM AN XCTL *         
      *         *  FROM ANOTHER MODULE.                               *         
      *         *******************************************************.        
                                                                                
           IF EIBCALEN NOT > ZERO                                               
               MOVE UNACCESS-MSG       TO  LOGOFF-MSG                           
               GO TO 8300-SEND-TEXT.                                            
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               PGMIDERR (9600-PGMIDERR)                                         
               NOTFND   (8700-NOT-FOUND)                                        
               ENDFILE  (4700-END-OF-BROWSE)                                    
               DUPKEY   (4015-DUPKEY)                                           
               ITEMERR  (9400-CLEAR)                                            
               ERROR    (9990-ERROR)                                            
           END-EXEC.                                                            
                                                                                
           EJECT                                                                
           IF PI-CALLING-PROGRAM = 'EL1283'                                     
               MOVE EIBTRMID           TO  WS-TS1-TERM-ID                       
               EXEC CICS READQ TS                                               
                   QUEUE  (WS-EL1283-TS)                                        
                   INTO   (PI-PROGRAM-WORK-AREA)                                
                   LENGTH (WS-WORK-LENGTH)                                      
               END-EXEC                                                         
               EXEC CICS DELETEQ TS                                             
                   QUEUE  (WS-EL1283-TS)                                        
               END-EXEC                                                         
               MOVE +2                 TO  PI-1ST-TIME-SW.                      
                                                                                
           IF PI-CALLING-PROGRAM = 'EL128'                                      
               MOVE ZERO               TO PI-SCREEN-COUNT                       
               MOVE PI-CERTIFICATE-KEY TO PI-1ST-KEY                            
               MOVE PI-KEY-LENGTH      TO PI-SAVE-KEY-LENGTH.                   
                                                                                
           IF PI-CALLING-PROGRAM = 'EL1282'                                     
               GO TO 0100-CONTINUE-PROCESSING                                   
           ELSE                                                                 
               MOVE ZERO               TO PI-ALT-NAME-COUNT.                    
                                                                                
           IF PI-RETURN-TO-PROGRAM NOT = THIS-PGM                               
               MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-6                 
               MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-5                 
               MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-4                 
               MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-3                 
               MOVE PI-SAVED-PROGRAM-1   TO  PI-SAVED-PROGRAM-2                 
               MOVE PI-RETURN-TO-PROGRAM TO  PI-SAVED-PROGRAM-1                 
               MOVE PI-CALLING-PROGRAM   TO  PI-RETURN-TO-PROGRAM               
               MOVE THIS-PGM             TO  PI-CALLING-PROGRAM                 
             ELSE                                                               
               MOVE PI-RETURN-TO-PROGRAM TO  PI-CALLING-PROGRAM                 
               MOVE PI-SAVED-PROGRAM-1   TO  PI-RETURN-TO-PROGRAM               
               MOVE PI-SAVED-PROGRAM-2   TO  PI-SAVED-PROGRAM-1                 
               MOVE PI-SAVED-PROGRAM-3   TO  PI-SAVED-PROGRAM-2                 
               MOVE PI-SAVED-PROGRAM-4   TO  PI-SAVED-PROGRAM-3                 
               MOVE PI-SAVED-PROGRAM-5   TO  PI-SAVED-PROGRAM-4                 
               MOVE PI-SAVED-PROGRAM-6   TO  PI-SAVED-PROGRAM-5                 
               MOVE SPACES               TO  PI-SAVED-PROGRAM-6.                
                                                                                
           MOVE +0                     TO PI-SUB.                               
           MOVE LOW-VALUES             TO PI-CERT-CONTROLS-EL128 (1)            
                                          PI-CERT-CONTROLS-EL128 (2)            
                                          PI-CERT-CONTROLS-EL128 (3)            
                                          PI-CERT-CONTROLS-EL128 (4)            
                                          PI-CERT-CONTROLS-EL128 (5).           
                                                                                
           IF PI-1ST-TIME-SW = +2                                               
               MOVE EIBTRMID           TO  WS-TSK-TERM-ID                       
               EXEC CICS READQ TS                                               
                   QUEUE  (WS-TEMP-STORAGE-KEY)                                 
                   ITEM   (PI-TS-ITEM)                                          
                   INTO   (EL128BI)                                             
                   LENGTH (WS-TS-LENGTH)                                        
               END-EXEC                                                         
               EXEC CICS DELETEQ TS                                             
                   QUEUE  (WS-TEMP-STORAGE-KEY)                                 
               END-EXEC                                                         
               MOVE ZERO               TO  PI-1ST-TIME-SW                       
               MOVE LOW-VALUES         TO  BSELO                                
                                           BPFKO                                
               PERFORM 6000-SET-ATTRB                                           
                   VARYING EL128B-INDEX FROM PI-LINE-COUNT BY -1                
                     UNTIL EL128B-INDEX NOT > ZERO                              
               GO TO 8100-SEND-INITIAL-MAP.                                     
                                                                                
           PERFORM 4000-BROWSE-CERT-FILE.                                       
                                                                                
           EJECT                                                                
       0100-CONTINUE-PROCESSING.                                                
      *    NOTE *******************************************************         
      *         *      AFTER THE FIRST TIME THROUGH THE PROPER ATTEN- *         
      *         *  TION KEY USAGE NEEDS TO BE CHECKED FOR VALIDITY    *         
      *         *  BEFORE ANY FURTHER PROCESSING CAN BE DONE.         *         
      *         *******************************************************.        
                                                                                
           IF EIBAID = DFHCLEAR                                                 
               GO TO 9400-CLEAR.                                                
                                                                                
           IF EIBAID = DFHPA1 OR DFHPA2 OR DFHPA3                               
               MOVE LOW-VALUES         TO  EL128BO                              
               MOVE -1                 TO  BPFKL                                
               MOVE ER-0008            TO  EMI-ERROR                            
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           EXEC CICS RECEIVE                                                    
               INTO   (EL128BI)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
           END-EXEC.                                                            
                                                                                
           IF BPFKL > ZERO                                                      
               IF EIBAID NOT = DFHENTER                                         
                   MOVE ER-0004        TO  EMI-ERROR                            
                   MOVE AL-UNBOF       TO  BPFKA                                
                   MOVE -1             TO  BPFKL                                
                   GO TO 8200-SEND-DATAONLY                                     
               ELSE                                                             
                   IF (BPFKI NUMERIC) AND                                       
                       (BPFKI > ZERO AND < 25)                                  
                           MOVE PF-VALUES (BPFKI)  TO  EIBAID                   
                       ELSE                                                     
                           MOVE ER-0029        TO  EMI-ERROR                    
                           MOVE AL-UNBOF       TO  BPFKA                        
                           MOVE -1             TO  BPFKL                        
                           GO TO 8200-SEND-DATAONLY.                            
                                                                                
           EJECT                                                                
      *    NOTE *******************************************************         
      *         *      PF KEY USAGE:                                  *         
      *         *        PF1       SEARCH FORWARD                     *         
      *         *        PF2       SEARCH BACKWARD                    *         
      *         *        PF12      HELP                               *         
      *         *        PF23      LOGOFF                             *         
      *         *        PF24      RETURN TO MASTER MENU              *         
      *         *******************************************************.        
                                                                                
           IF EIBAID = DFHPF12                                                  
               MOVE 'EL010'         TO  THIS-PGM                                
               GO TO 9300-XCTL.                                                 
                                                                                
           IF EIBAID = DFHPF23                                                  
               GO TO 9000-RETURN-CICS.                                          
                                                                                
           IF EIBAID = DFHPF24                                                  
               MOVE 'EL126'            TO  THIS-PGM                             
               GO TO 9300-XCTL.                                                 
                                                                                
           IF BSELL > ZERO                                                      
               IF BSELI NOT NUMERIC                                             
                   MOVE -1             TO  BSELL                                
                   MOVE ER-0200        TO  EMI-ERROR                            
                   GO TO 8200-SEND-DATAONLY.                                    
                                                                                
           IF BSELL > ZERO                                                      
              CONTINUE                                                          
           ELSE                                                                 
              GO TO 0120-MAIN-LOGIC                                             
           END-IF                                                               
                                                                                
           IF BSELL > ZERO        AND                                           
              BSELO > ZERO        AND                                           
              BSELO < '9'         AND                                           
              BSELI NOT > PI-LINE-COUNT                                         
               NEXT SENTENCE                                                    
             ELSE                                                               
               MOVE -1                 TO  BSELL                                
               MOVE ER-0200            TO  EMI-ERROR                            
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           IF (BSELL > ZERO AND                                                 
                 PI-SAVED-PROGRAM-1 NOT = 'EL130')                              
                  MOVE EIBTRMID       TO  WS-TSK-TERM-ID                        
                  PERFORM 8620-WRITE-TEMP-STORAGE THRU 8630-EXIT.               
                                                                                
                                                                                
       EJECT                                                                    
           SET EL128B-INDEX TO BSELI.                                           
                                                                                
           MOVE EL128B-CARRIER (EL128B-INDEX)  TO  PI-CARRIER.                  
           MOVE EL128B-GROUP   (EL128B-INDEX)  TO  PI-GROUPING.                 
           MOVE EL128B-STATE   (EL128B-INDEX)  TO  PI-STATE.                    
           MOVE EL128B-ACCOUNT (EL128B-INDEX)  TO  PI-ACCOUNT.                  
           MOVE EL128B-CERT-NO (EL128B-INDEX)  TO  PI-CERT-NO.                  
                                                                                
           MOVE EL128B-EFF-DATE (EL128B-INDEX)  TO  DC-GREG-DATE-1-EDIT.        
           MOVE '2'                    TO  DC-OPTION-CODE.                      
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-BIN-DATE-1          TO  PI-CERT-EFF-DT.                      
                                                                                
           MOVE +2                     TO  PI-1ST-TIME-SW.                      
                                                                                
           MOVE PI-SAVED-PROGRAM-1     TO  THIS-PGM.                            
                                                                                
           MOVE 'EL1283'               TO THIS-PGM                              
                                                                                
           IF THIS-PGM NOT = 'EL1283'                                           
               MOVE PI-RETURN-TO-PROGRAM   TO  PI-CALLING-PROGRAM               
               MOVE PI-SAVED-PROGRAM-1     TO  PI-RETURN-TO-PROGRAM             
               MOVE PI-SAVED-PROGRAM-2     TO  PI-SAVED-PROGRAM-1               
               MOVE PI-SAVED-PROGRAM-3     TO  PI-SAVED-PROGRAM-2               
               MOVE PI-SAVED-PROGRAM-4     TO  PI-SAVED-PROGRAM-3               
               MOVE PI-SAVED-PROGRAM-5     TO  PI-SAVED-PROGRAM-4               
               MOVE PI-SAVED-PROGRAM-6     TO  PI-SAVED-PROGRAM-5               
               MOVE SPACES                 TO  PI-SAVED-PROGRAM-6.              
                                                                                
           GO TO 9300-XCTL.                                                     
                                                                                
       EJECT                                                                    
       0115-BUILD-CERT-KEYS.                                                    
                                                                                
           IF (BCRTSL1L > 0)                                                    
                 MOVE +1                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     ADD +1              TO  PI-SUB                             
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL2L > 0)                                                    
                 MOVE +2                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL3L > 0)                                                    
                 MOVE +3                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL4L > 0)                                                    
                 MOVE +4                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL5L > 0)                                                    
                 MOVE +5                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL6L > 0)                                                    
                 MOVE +6                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL7L > 0)                                                    
                 MOVE +7                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           IF (BCRTSL8L > 0)                                                    
                 MOVE +8                 TO  SUB                                
                 PERFORM 0116-BUILD-EL128-CERT-KEYS THRU 0116-BUILD-EXIT        
                 MOVE +0                 TO  SUB                                
                 PERFORM 0115-CK-FOR-DUP-SELECTION THRU 0115-DUP-EXIT           
                 ADD +1                  TO  PI-SUB                             
                 IF PREVIOUSLY-SELECTED                                         
                     NEXT SENTENCE                                              
                 ELSE                                                           
                     IF PI-SUB > +5                                             
                         MOVE ER-0659    TO  EMI-ERROR                          
                         PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT               
                     ELSE                                                       
                         MOVE WS-CERT-CONTROL    TO                             
                                        PI-CERT-CONTROLS-EL128 (PI-SUB).        
                                                                                
           GO TO 0120-MAIN-LOGIC.                                               
                                                                                
       0115-CK-FOR-DUP-SELECTION.                                               
                                                                                
           ADD +1                      TO  SUB.                                 
           IF SUB > 5                                                           
               MOVE 'N'                TO  WS-SELECTED-SW                       
               GO TO 0115-DUP-EXIT.                                             
                                                                                
           IF WS-CERT-CONTROL = PI-CERT-CONTROLS-EL128 (SUB)                    
               MOVE 'Y'                TO  WS-SELECTED-SW                       
               GO TO 0115-DUP-EXIT.                                             
                                                                                
           GO TO 0115-CK-FOR-DUP-SELECTION.                                     
                                                                                
       0115-DUP-EXIT.                                                           
           EXIT.                                                                
                                                                                
       0116-BUILD-EL128-CERT-KEYS.                                              
                                                                                
           MOVE EL128B-CARRIER (SUB)   TO  WS-CARRIER.                          
           MOVE EL128B-GROUP (SUB)     TO  WS-GROUPING.                         
           MOVE EL128B-STATE (SUB)     TO  WS-STATE.                            
           MOVE EL128B-ACCOUNT (SUB)   TO  WS-ACCOUNT.                          
           MOVE EL128B-CERT-NO (SUB)   TO  WS-CERT-NO.                          
           MOVE EL128B-EFF-DATE (SUB)  TO  DC-GREG-DATE-1-EDIT.                 
           MOVE '2'                    TO  DC-OPTION-CODE.                      
           PERFORM 8500-DATE-CONVERSION.                                        
           IF NO-CONVERSION-ERROR                                               
               MOVE DC-BIN-DATE-1      TO  WS-EFF-DT                            
           ELSE                                                                 
               MOVE LOW-VALUES         TO  WS-EFF-DT.                           
                                                                                
       0116-BUILD-EXIT.                                                         
           EXIT.                                                                
                                                                                
       0120-MAIN-LOGIC.                                                         
           IF EIBAID = (DFHENTER OR DFHPF1)                                     
             OR                                                                 
               ((EIBAID = DFHPF2) AND                                           
                (PI-SCREEN-COUNT > +1))                                         
                   NEXT SENTENCE                                                
                 ELSE                                                           
                   MOVE ER-0131            TO  EMI-ERROR                        
                   MOVE -1                 TO  BPFKL                            
                   GO TO 8200-SEND-DATAONLY.                                    
                                                                                
           IF PI-END-OF-FILE NOT > ZERO                                         
               PERFORM 4000-BROWSE-CERT-FILE.                                   
                                                                                
           IF PI-END-OF-FILE > ZERO                                             
               IF EIBAID = DFHPF2                                               
                   NEXT SENTENCE                                                
               ELSE                                                             
                   MOVE ER-0130        TO  EMI-ERROR                            
                   MOVE -1             TO  BSELL                                
                   GO TO 8200-SEND-DATAONLY.                                    
                                                                                
           IF EIBAID  = DFHPF2                                                  
               NEXT SENTENCE                                                    
             ELSE                                                               
               IF PI-DSID NOT = 'ELPURG'                                        
                   GO TO 9400-CLEAR.                                            
                                                                                
           IF (PI-PREV-AID = (DFHPF1 OR DFHENTER) AND                           
               EIBAID = DFHPF2)                                                 
             OR                                                                 
               (PI-PREV-AID = DFHPF2 AND                                        
                EIBAID = (DFHPF1 OR DFHENTER))                                  
                   PERFORM 4000-BROWSE-CERT-FILE                                
                 ELSE                                                           
                   GO TO 9400-CLEAR.                                            
                                                                                
           EJECT                                                                
       4000-BROWSE-CERT-FILE SECTION.                                           
           MOVE PI-CERTIFICATE-KEY   TO PI-LIN1-CERTIFICATE-KEY.                
                                                                                
           EXEC CICS HANDLE CONDITION                                           
                NOTFND (8700-NOT-FOUND)                                         
                END-EXEC.                                                       
                                                                                
           MOVE LOW-VALUES             TO  EL128BO.                             
                                                                                
           IF PI-BROWSE-SW = ZERO                                               
             AND PI-START-SW = +1                                               
               EXEC CICS STARTBR                                                
                   DATASET   (PI-DSID)                                          
                   RIDFLD    (PI-CERTIFICATE-KEY)                               
                   GENERIC                                                      
                   GTEQ                                                         
                   KEYLENGTH (PI-KEY-LENGTH)                                    
                   END-EXEC                                                     
               GO TO 4005-NEXT-SENTENCE.                                        
                                                                                
           IF EIBAID = DFHPF1                                                   
             AND OPTION-FOUR-SELECTED                                           
             AND PI-1ST-KEY = PI-LAST-KEY                                       
               PERFORM 7000-PF2-POSITION                                        
               GO TO 4005-NEXT-SENTENCE.                                        
                                                                                
           IF EIBAID = DFHPF2                                                   
               SUBTRACT 2 FROM PI-SCREEN-COUNT                                  
               PERFORM 7000-PF2-POSITION                                        
               GO TO 4005-NEXT-SENTENCE.                                        
                                                                                
           EXEC CICS STARTBR                                                    
               DATASET (PI-DSID)                                                
               RIDFLD  (PI-CERTIFICATE-KEY)                                     
               EQUAL                                                            
           END-EXEC.                                                            
                                                                                
       4005-NEXT-SENTENCE.                                                      
           MOVE +1                     TO  PI-BROWSE-SW.                        
           MOVE ZERO                   TO  PI-LINE-COUNT.                       
           MOVE LOW-VALUES             TO  EL128BO.                             
           MOVE PI-CERTIFICATE-KEY     TO  WS-KEY-HOLD.                         
           SET EL128B-INDEX TO +1.                                              
                                                                                
       4010-READNEXT.                                                           
           EXEC CICS READNEXT                                                   
               DATASET (PI-DSID)                                                
               RIDFLD  (PI-CERTIFICATE-KEY)                                     
               SET     (ADDRESS OF PURGE-CERT-MASTER)                           
           END-EXEC.                                                            
                                                                                
           IF PI-LINE-COUNT NOT = ZERO                                          
               MOVE ZERO               TO  PI-AIX-RECORD-COUNT.                 
                                                                                
       4015-DUPKEY.                                                             
           IF LCP-ONCTR-01 =  0                                                 
               ADD 1 TO LCP-ONCTR-01                                            
           ELSE                                                                 
               GO TO 4016-NEXT-SENTENCE.                                        
                                                                                
           IF PI-AIX-RECORD-COUNT > +8                                          
               SUBTRACT +1 FROM PI-AIX-RECORD-COUNT.                            
                                                                                
       4016-NEXT-SENTENCE.                                                      
                                                                                
           ADD +1  TO  WS-AIX-RECORD-COUNT.                                     
                                                                                
           IF EIBAID = DFHPF1 OR DFHENTER                                       
               IF PI-LINE-COUNT = ZERO                                          
                   IF OPTION-THREE-SELECTED                                     
                       PERFORM 5000-MOVE-NAME                                   
                       IF PG-INSURED-LAST-NAME = PI-END-NAME                    
                           IF PI-END-CERTIFICATE-KEY =                          
                                   PG-CONTROL-PRIMARY                           
                               NEXT SENTENCE                                    
                             ELSE                                               
                               GO TO 4010-READNEXT                              
                         ELSE                                                   
                           NEXT SENTENCE                                        
                     ELSE                                                       
                       IF OPTION-ONE-SELECTED                                   
                           IF ((PG-COMPANY-CD = PI-END-COMPANY-ID)              
                               AND (PG-CERT-NO = PI-END-CERT-NO))               
                               IF PI-END-CERTIFICATE-KEY =                      
                                       PG-CONTROL-PRIMARY                       
                                   NEXT SENTENCE                                
                                 ELSE                                           
                                   GO TO 4010-READNEXT.                         
                                                                                
           MOVE PI-CERTIFICATE-KEY     TO  WS-KEY-INPUT.                        
                                                                                
           SET KEY-INDEX                                                        
               KEY-INDEX2 TO +1.                                                
                                                                                
       4020-COMPARE-KEY.                                                        
           IF PI-PART-FIELD-SW  NOT = ' '                                       
              MOVE +1  TO PI-KEY-LENGTH.                                        
                                                                                
           IF WS-KH-CHAR (KEY-INDEX) NOT = WS-KI-CHAR (KEY-INDEX2)              
               GO TO 4700-END-OF-BROWSE.                                        
                                                                                
           IF KEY-INDEX < PI-KEY-LENGTH                                         
               SET KEY-INDEX                                                    
                   KEY-INDEX2 UP BY +1                                          
               GO TO 4020-COMPARE-KEY.                                          
                                                                                
      ******************************************************************        
      *        SECURITY CHECK FOR CARRIER AND ACCOUNT NUMBER           *        
      *                        04/04/84                                *        
      ******************************************************************        
                                                                                
      ******************************************************************        
      *    IF THE SECURITY CHECK ROUTINE IS CHANGED HERE, YOU MUST     *        
      *        ALSO CHANGE THE SECURITY CHECK ROUTINE IN               *        
      *        7100-READNEXT-PF2.           KER/080884                 *        
      ******************************************************************        
                                                                                
           IF PI-NO-CARRIER-SECURITY AND                                        
              PI-NO-ACCOUNT-SECURITY                                            
               GO TO 4030-CHECK-OPTION.                                         
                                                                                
           IF PI-CARRIER-SECURITY > SPACES                                      
               IF PG-CARRIER = PI-CARRIER-SECURITY                              
                   NEXT SENTENCE                                                
                  ELSE                                                          
                   GO TO 4010-READNEXT.                                         
                                                                                
           IF PI-ACCOUNT-SECURITY > SPACES                                      
               IF PG-ACCOUNT = PI-ACCOUNT-SECURITY                              
                   NEXT SENTENCE                                                
                  ELSE                                                          
                   GO TO 4010-READNEXT.                                         
                                                                                
       4030-CHECK-OPTION.                                                       
           IF NOT OPTION-THREE-SELECTED                                         
               GO TO 4090-MOVE-DATA.                                            
                                                                                
           IF (PG-INSURED-LAST-NAME NOT = PI-SC-LAST-NAME) OR                   
              (PG-INSURED-INITIALS  NOT = PI-PREV-INITIALS)                     
               MOVE +1         TO PI-ALT-NAME-COUNT.                            
                                                                                
           MOVE PG-INSURED-INITIALS    TO PI-PREV-INITIALS.                     
                                                                                
      ******************************************************************        
      *   IF READING ELCERT (ALT BY NAME), ADD TO PI-ALT-NAME-COUNT.   *        
      *   ON EITHER OF TWO CONDITIONS: 1. END OF SEARCH.....(EL1282)   *        
      *                                2. NOT FOUND.........(EL128)    *        
      *  ......IF COUNT WAS OVER 140, DISPLAY MESSAGE ER-0765,         *        
      *           DIRECTING USER TO ECS052 FOR FULL LIST.              *        
      ******************************************************************        
                                                                                
           IF EIBAID = DFHPF2                                                   
                SUBTRACT +1 FROM PI-ALT-NAME-COUNT                              
              ELSE                                                              
                ADD +1        TO PI-ALT-NAME-COUNT.                             
                                                                                
      ******************************************************************        
      *        IF THE INITIAL CHECKING ROUTINE OR THE ACCOUNT CHECKING *        
      *    ROUTINE IS CHANGED, YOU MUST ALSO CHANGE THE CORRESPONDING  *        
      *    ROUTINE IN 7130-CHECK-INITIAL.        KER/080884            *        
      ******************************************************************        
                                                                                
           IF PI-SC-INITIALS NOT = SPACES                                       
              MOVE PI-SC-INITIALS      TO WS-INITIALS                           
              IF WS-INIT2 NOT = SPACE                                           
                 IF PI-SC-INITIALS NOT = PG-INSURED-INITIALS                    
                    GO TO 4010-READNEXT                                         
                   ELSE                                                         
                    NEXT SENTENCE                                               
                 ELSE                                                           
                 IF WS-INIT1 NOT = PG-INSURED-INITIAL1                          
                    GO TO 4010-READNEXT.                                        
                                                                                
           IF PI-SC-FIRST-NAME = SPACES                                         
               GO TO 4040-CONTINUE.                                             
                                                                                
           MOVE PI-SC-FIRST-NAME       TO WS-PI-NAME.                           
           MOVE PG-INSURED-FIRST-NAME  TO WS-CM-NAME.                           
                                                                                
           MOVE SPACE                  TO WS-COMPARE-INDICATOR.                 
           PERFORM 4035-CHECK-NAME THRU 4035-EXIT                               
               VARYING WS-NAME-INDEX FROM 15 BY -1                              
                 UNTIL WS-NAME-INDEX = ZERO.                                    
                                                                                
           IF NAME-NOT-FOUND                                                    
               GO TO 4010-READNEXT                                              
             ELSE                                                               
               GO TO 4040-CONTINUE.                                             
                                                                                
       4035-CHECK-NAME.                                                         
           IF WS-PI-NAME-CHAR (WS-NAME-INDEX) NOT = ' ' AND                     
              WS-CM-NAME-CHAR (WS-NAME-INDEX)                                   
                MOVE 'X'               TO WS-COMPARE-INDICATOR.                 
                                                                                
       4035-EXIT.                                                               
            EXIT.                                                               
                                                                                
       4040-CONTINUE.                                                           
           IF PI-SC-ACCT-NO = SPACES                                            
               GO TO 4050-CK-CARRIER.                                           
                                                                                
           IF PI-SC-ACCT-NO = PG-ACCOUNT                                        
               NEXT SENTENCE                                                    
             ELSE                                                               
               GO TO 4010-READNEXT.                                             
                                                                                
       4050-CK-CARRIER.                                                         
           IF PI-SC-CARR = SPACES                                               
               GO TO 4090-MOVE-DATA.                                            
                                                                                
           IF PI-SC-CARR = PG-CARRIER                                           
               NEXT SENTENCE                                                    
             ELSE                                                               
               GO TO 4010-READNEXT.                                             
                                                                                
       4090-MOVE-DATA.                                                          
           IF LCP-ONCTR-02 =  0                                                 
               ADD 1 TO LCP-ONCTR-02                                            
             ELSE                                                               
               GO TO 4095-MOVE-DATA.                                            
                                                                                
           MOVE +1                     TO WS-CERT-SW.                           
                                                                                
       4095-MOVE-DATA.                                                          
           MOVE WS-KEY-INPUT           TO  PI-LAST-KEY.                         
                                                                                
           ADD +1                      TO  PI-LINE-COUNT                        
                                           PI-AIX-RECORD-COUNT.                 
                                                                                
      *    IF PG-CLAIM-ATTACHED-COUNT > ZERO                                    
      *        MOVE '*'                TO  EL128B-AST (EL128B-INDEX).           
                                                                                
           IF PI-COMPANY-ID = 'FLA'                                             
               PERFORM 9050-FLA-NAME THRU 9050-EXIT-FIX.                        
                                                                                
           PERFORM 5000-MOVE-NAME.                                              
                                                                                
           MOVE PG-CONTROL-PRIMARY   TO  PI-END-CERTIFICATE-KEY.                
           MOVE PG-INSURED-LAST-NAME TO  PI-END-NAME.                           
                                                                                
           MOVE WS-NAME-WORK         TO  EL128B-NAME-O   (EL128B-INDEX).        
           MOVE PG-INSURED-ISSUE-AGE TO  EL128B-AGE      (EL128B-INDEX).        
           MOVE PG-INSURED-SEX       TO  EL128B-SEX      (EL128B-INDEX).        
           MOVE PG-CARRIER           TO  EL128B-CARRIER  (EL128B-INDEX).        
           MOVE PG-GROUPING          TO  EL128B-GROUP    (EL128B-INDEX).        
           MOVE PG-STATE             TO  EL128B-STATE    (EL128B-INDEX).        
           MOVE PG-ACCOUNT           TO  EL128B-ACCOUNT  (EL128B-INDEX).        
           MOVE PG-CERT-NO           TO  EL128B-CERT-NO  (EL128B-INDEX).        
                                                                                
           IF PG-CERT-EFF-DT NOT = LOW-VALUES                                   
               MOVE PG-CERT-EFF-DT       TO  DC-BIN-DATE-1                      
               MOVE SPACES               TO  DC-OPTION-CODE                     
               PERFORM 8500-DATE-CONVERSION                                     
               MOVE DC-GREG-DATE-1-EDIT  TO  EL128B-EFF-DATE                    
                                                    (EL128B-INDEX).             
                                                                                
           PERFORM 6000-SET-ATTRB.                                              
                                                                                
           MOVE EIBDATE                TO  DC-JULIAN-YYDDD.                     
           MOVE '5'                    TO  DC-OPTION-CODE.                      
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-BIN-DATE-1          TO  WS-CURRENT-DATE.                     
                                                                                
           IF PG-LF-BENEFIT-CD = '00'                                           
               GO TO 4100-A-AND-H-INFO.                                         
                                                                                
           MOVE SPACES     TO    EL128B-LIFE-INFO (EL128B-INDEX).               
                                                                                
           MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.                 
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE '4'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PG-LF-BENEFIT-CD       TO  WS-CFK-BENEFIT-NO.                   
                                                                                
           PERFORM 8000-READ-CONTROL-FILE.                                      
                                                                                
           IF WS-NOT-FOUND = ZERO                                               
               MOVE CF-BENEFIT-ALPHA (WS-INDEX)                                 
                                      TO  EL128B-LI-ABVR (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '8'                                        
              IF PG-LF-CANCEL-DT NOT = LOW-VALUES                               
                  MOVE PG-LF-CANCEL-DT TO DC-BIN-DATE-1                         
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO EL128B-LI-DATE                
                                                         (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '7'                                        
              IF PG-LF-DEATH-DT NOT = LOW-VALUES                                
                  MOVE PG-LF-DEATH-DT  TO DC-BIN-DATE-1                         
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO EL128B-LI-DATE                
                                                        (EL128B-INDEX).         
                                                                                
      * READ STATE MASTER RECORD FOR FREE LOOK PERIOD *                         
                                                                                
           MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.                 
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE '3'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PG-STATE               TO  WS-CFK-ACCESS-TYPE.                  
                                                                                
           PERFORM 8000-READ-CONTROL-FILE.                                      
                                                                                
           IF WS-ST-REC-NOT-FOUND = ZERO                                        
              MOVE CF-ST-FREE-LOOK-PERIOD                                       
                                       TO CP-FREE-LOOK                          
           ELSE                                                                 
              MOVE ZERO                TO CP-FREE-LOOK.                         
                                                                                
           MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                       
           MOVE PG-CERT-EFF-DT         TO  CP-CERT-EFF-DT.                      
           MOVE PG-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.                   
           MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.                     
           MOVE PG-LF-ORIG-TERM        TO  CP-ORIGINAL-TERM.                    
           MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.              
           MOVE '4'                    TO  CP-REM-TERM-METHOD.                  
           MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                       
                                                                                
           EXEC CICS LINK                                                       
                PROGRAM  (ELRTRM)                                               
                COMMAREA (CALCULATION-PASS-AREA)                                
                LENGTH   (CP-COMM-LENGTH)                                       
           END-EXEC.                                                            
                                                                                
           IF PG-LF-CURRENT-STATUS = '1' OR '4'                                 
              IF CP-REMAINING-TERM-3 = ZEROS                                    
                 MOVE 'EXPIRED'        TO EL128B-LI-DESC2 (EL128B-INDEX)        
                 MOVE PG-LF-BENEFIT-AMT TO EL128B-LI-AMT  (EL128B-INDEX)        
              ELSE                                                              
                 MOVE 'ACTIVE'        TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '2'                                        
              MOVE 'PEND  '           TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '3'                                        
              MOVE 'RESTORE'          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '5'                                        
              MOVE 'REISSUE'          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '6'                                        
              MOVE 'LMP DIS'          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '7'                                        
              MOVE 'DEATH  '          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '8'                                        
              MOVE 'CANCEL '          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '9'                                        
              MOVE 'RE-ONLY'          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = 'D'                                        
              MOVE 'DECLINE'          TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = 'V'                                        
              MOVE 'VOID'             TO EL128B-LI-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-LF-CURRENT-STATUS = '6' OR '7' OR '8'                          
              NEXT SENTENCE                                                     
           ELSE                                                                 
              MOVE PG-LF-BENEFIT-AMT   TO EL128B-LI-AMT  (EL128B-INDEX).        
                                                                                
      *    IF PI-COMPANY-ID = 'CRI'  OR  'PEM'                                  
      *       MOVE PG-MEMBER-NO     TO  EL128B-MEMB-LOAN (EL128B-INDEX)         
      *    ELSE                                                                 
      *       MOVE PG-LOAN-NUMBER   TO  EL128B-MEMB-LOAN (EL128B-INDEX).        
           MOVE PG-MEMBER-NO     TO  EL128B-MEMB-LOAN (EL128B-INDEX)            
                                                                                
           .                                                                    
       4100-A-AND-H-INFO.                                                       
           IF PG-AH-BENEFIT-CD = '00'                                           
               GO TO 4200-CONTINUE.                                             
                                                                                
           MOVE SPACES               TO  EL128B-AH-INFO (EL128B-INDEX).         
                                                                                
           MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.                 
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE '5'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PG-AH-BENEFIT-CD       TO  WS-CFK-BENEFIT-NO.                   
                                                                                
           PERFORM 8000-READ-CONTROL-FILE.                                      
                                                                                
           IF WS-NOT-FOUND = ZERO                                               
               MOVE CF-BENEFIT-ALPHA (WS-INDEX)                                 
                                      TO  EL128B-AH-ABVR (EL128B-INDEX).        
                                                                                
      * READ STATE MASTER RECORD FOR FREE LOOK PERIOD *                         
                                                                                
           MOVE SPACES                 TO  WS-CONTROL-FILE-KEY.                 
           MOVE PI-COMPANY-ID          TO  WS-CFK-COMPANY-ID.                   
           MOVE '3'                    TO  WS-CFK-RECORD-TYPE.                  
           MOVE PG-STATE               TO  WS-CFK-ACCESS-TYPE.                  
                                                                                
           PERFORM 8000-READ-CONTROL-FILE.                                      
                                                                                
           IF WS-ST-REC-NOT-FOUND = ZERO                                        
              MOVE CF-ST-FREE-LOOK-PERIOD                                       
                                       TO CP-FREE-LOOK                          
           ELSE                                                                 
              MOVE ZERO                TO CP-FREE-LOOK.                         
                                                                                
           MOVE PG-CERT-EFF-DT         TO  CP-CERT-EFF-DT.                      
           MOVE PG-LOAN-1ST-PMT-DT     TO  CP-FIRST-PAY-DATE.                   
           MOVE WS-CURRENT-DATE        TO  CP-VALUATION-DT.                     
           MOVE PG-AH-ORIG-TERM        TO  CP-ORIGINAL-TERM.                    
           MOVE PI-REM-TRM-CALC-OPTION TO  CP-REM-TRM-CALC-OPTION.              
           MOVE '4'                    TO  CP-REM-TERM-METHOD.                  
           MOVE PI-COMPANY-ID          TO  CP-COMPANY-ID.                       
           EXEC CICS LINK                                                       
                PROGRAM  (ELRTRM)                                               
                COMMAREA (CALCULATION-PASS-AREA)                                
                LENGTH   (CP-COMM-LENGTH)                                       
           END-EXEC.                                                            
                                                                                
           IF PG-AH-CURRENT-STATUS = '8'                                        
              IF PG-AH-CANCEL-DT NOT = LOW-VALUES                               
                  MOVE PG-AH-CANCEL-DT TO DC-BIN-DATE-1                         
                  MOVE SPACES          TO DC-OPTION-CODE                        
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO EL128B-AH-DATE                
                                                         (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '6' OR '7'                                 
              IF PG-AH-SETTLEMENT-DT NOT = LOW-VALUES                           
                  MOVE PG-AH-SETTLEMENT-DT TO DC-BIN-DATE-1                     
                  MOVE SPACES              TO DC-OPTION-CODE                    
                  PERFORM 8500-DATE-CONVERSION                                  
                  IF NOT DATE-CONVERSION-ERROR                                  
                      MOVE DC-GREG-DATE-1-EDIT TO EL128B-AH-DATE                
                                                         (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '1' OR '4'                                 
             IF CP-REMAINING-TERM-3 = ZEROS                                     
                MOVE 'EXPIRED'         TO EL128B-AH-DESC2 (EL128B-INDEX)        
                MOVE PG-AH-BENEFIT-AMT TO EL128B-AH-AMT   (EL128B-INDEX)        
             ELSE                                                               
                MOVE 'ACTIVE'         TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '2'                                        
              MOVE 'PEND   '          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '3'                                        
              MOVE 'RESTORE'          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '5'                                        
              MOVE 'REISSUE'          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '6'                                        
              MOVE 'LMP DIS'          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '7'                                        
              MOVE 'DEATH  '          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '8'                                        
              MOVE 'CANCEL '          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '9'                                        
              MOVE 'RE-ONLY'          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = 'D'                                        
              MOVE 'DECLINE'          TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = 'V'                                        
              MOVE 'VOID'             TO EL128B-AH-DESC2 (EL128B-INDEX).        
                                                                                
           IF PG-AH-CURRENT-STATUS = '6' OR '7' OR '8'                          
              NEXT SENTENCE                                                     
           ELSE                                                                 
              MOVE PG-AH-BENEFIT-AMT   TO EL128B-AH-AMT  (EL128B-INDEX).        
                                                                                
       4200-CONTINUE.                                                           
           IF EL128B-INDEX < +8                                                 
               SET EL128B-INDEX UP BY +1                                        
               GO TO 4010-READNEXT.                                             
                                                                                
           GO TO 4900-ENDBROWSE.                                                
                                                                                
       4700-END-OF-BROWSE.                                                      
           MOVE ER-0130                 TO  EMI-ERROR.                          
           ADD +1  TO  PI-END-OF-FILE.                                          
                                                                                
       4900-ENDBROWSE.                                                          
                                                                                
           ADD 1 TO PI-SCREEN-COUNT.                                            
                                                                                
           EXEC CICS ENDBR                                                      
               DATASET (PI-DSID)                                                
           END-EXEC.                                                            
                                                                                
           IF CREDIT-SESSION                                                    
               IF (PI-LINE-COUNT  =  +1)  AND                                   
                  (PI-1ST-TIME-SW =  ZEROS)                                     
                   NEXT SENTENCE                                                
               ELSE                                                             
                   GO TO 4910-SEND-MAP                                          
           ELSE                                                                 
              GO TO 4910-SEND-MAP.                                              
                                                                                
           MOVE EL128B-CARRIER (1)        TO  PI-CARRIER.                       
           MOVE EL128B-GROUP   (1)        TO  PI-GROUPING.                      
           MOVE EL128B-STATE   (1)        TO  PI-STATE.                         
           MOVE EL128B-ACCOUNT (1)        TO  PI-ACCOUNT.                       
           MOVE EL128B-CERT-NO (1)        TO  PI-CERT-NO.                       
           MOVE EL128B-EFF-DATE(1)        TO  DC-GREG-DATE-1-EDIT.              
           MOVE '2'                       TO  DC-OPTION-CODE.                   
           PERFORM 8500-DATE-CONVERSION.                                        
           MOVE DC-BIN-DATE-1             TO  PI-CERT-EFF-DT.                   
                                                                                
           MOVE PI-RETURN-TO-PROGRAM    TO  PI-CALLING-PROGRAM.                 
           MOVE PI-SAVED-PROGRAM-1      TO  PI-RETURN-TO-PROGRAM                
                                            THIS-PGM.                           
           MOVE PI-SAVED-PROGRAM-2      TO  PI-SAVED-PROGRAM-1.                 
           MOVE PI-SAVED-PROGRAM-3      TO  PI-SAVED-PROGRAM-2.                 
           MOVE PI-SAVED-PROGRAM-4      TO  PI-SAVED-PROGRAM-3.                 
           MOVE PI-SAVED-PROGRAM-5      TO  PI-SAVED-PROGRAM-4.                 
           MOVE PI-SAVED-PROGRAM-6      TO  PI-SAVED-PROGRAM-5.                 
           MOVE SPACES                  TO  PI-SAVED-PROGRAM-6.                 
                                                                                
           IF THIS-PGM = 'EL6311' OR 'EL626' OR 'EL126' OR                      
                         'EL1282' OR 'EL128'                                    
               MOVE 'EL1283'            TO  THIS-PGM                            
               GO TO 9300-XCTL.                                                 
                                                                                
       4910-SEND-MAP.                                                           
           IF WS-NO-CERT-FOUND                                                  
               MOVE +9                 TO  PI-BROWSE-SW                         
               GO TO 9400-CLEAR.                                                
                                                                                
           MOVE +1                     TO  PI-1ST-TIME-SW.                      
           MOVE -1                     TO  BSELL.                               
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
           EJECT                                                                
       5000-MOVE-NAME SECTION. COPY ELCMNS REPLACING                            
           CL-INSURED-LAST-NAME        BY  PG-INSURED-LAST-NAME                 
           CL-INSURED-1ST-NAME         BY  PG-INSURED-FIRST-NAME                
           CL-INSURED-MID-INIT         BY  PG-INSURED-INITIAL2.                 
                                                                                
           EJECT                                                                
       6000-SET-ATTRB SECTION.                                                  
           MOVE AL-SANON      TO  EL128B-AST-ATTRB       (EL128B-INDEX)         
                                  EL128B-CARRIER-ATTRB   (EL128B-INDEX)         
                                  EL128B-GROUP-ATTRB     (EL128B-INDEX)         
                                  EL128B-STATE-ATTRB     (EL128B-INDEX)         
                                  EL128B-ACCOUNT-ATTRB   (EL128B-INDEX)         
                                  EL128B-CERT-NO-ATTRB   (EL128B-INDEX)         
                                  EL128B-NAME-ATTRB      (EL128B-INDEX)         
                                  EL128B-AGE-ATTRB       (EL128B-INDEX)         
                                  EL128B-SEX-ATTRB       (EL128B-INDEX)         
                                  EL128B-EFF-DATE-ATTRB  (EL128B-INDEX)         
                                  EL128B-LIFE-INFO-ATTRB (EL128B-INDEX)         
                                  EL128B-AH-INFO-ATTRB   (EL128B-INDEX).        
                                                                                
           IF CREDIT-SESSION                                                    
               MOVE AL-SANON  TO  EL128B-CERT-SEL-ATTRB  (EL128B-INDEX)         
               MOVE AL-UNNON  TO  BSELA                                         
           ELSE                                                                 
               MOVE AL-UANON  TO  EL128B-CERT-SEL-ATTRB  (EL128B-INDEX).        
                                                                                
      *    IF PI-SAVED-PROGRAM-1 = 'EL130'                                      
      *        MOVE AL-SANOF  TO  BPFK3A   BPFK5A                               
      *        MOVE SPACES    TO  BPFK3O   BPFK5O                               
      *    ELSE                                                                 
      *        MOVE AL-SANON  TO  BPFK3A   BPFK5A.                              
                                                                                
       6000-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       7000-PF2-POSITION         SECTION.                                       
                                                                                
           EXEC CICS IGNORE CONDITION                                           
                DUPKEY                                                          
           END-EXEC.                                                            
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND (8700-NOT-FOUND)                                          
           END-EXEC.                                                            
                                                                                
           IF PI-PART-FIELD-SW  = 'C' OR 'S'                                    
              MOVE PI-SAVE-KEY-LENGTH TO PI-KEY-LENGTH.                         
                                                                                
           COMPUTE WS-CALC-RDNXT = PI-SCREEN-COUNT * 7.                         
           MOVE PI-1ST-KEY             TO PI-CERTIFICATE-KEY.                   
           MOVE ZERO                   TO PI-END-OF-FILE.                       
                                                                                
           IF (OPTION-FOUR-SELECTED   AND PI-KEY-LENGTH = 12)                   
                            OR                                                  
              (OPTION-THREE-SELECTED  AND PI-KEY-LENGTH = 18)                   
             EXEC CICS STARTBR                                                  
                DATASET   (PI-DSID)                                             
                RIDFLD    (PI-CERTIFICATE-KEY)                                  
                EQUAL                                                           
                KEYLENGTH (PI-KEY-LENGTH)                                       
             END-EXEC                                                           
            ELSE                                                                
             EXEC CICS STARTBR                                                  
                DATASET   (PI-DSID)                                             
                RIDFLD    (PI-CERTIFICATE-KEY)                                  
                GENERIC                                                         
                EQUAL                                                           
                KEYLENGTH (PI-KEY-LENGTH)                                       
             END-EXEC.                                                          
                                                                                
       7100-READNEXT-PF2.                                                       
           IF WS-CALC-RDNXT > ZERO                                              
               NEXT SENTENCE                                                    
             ELSE                                                               
               GO TO 7999-EXIT.                                                 
                                                                                
           EXEC CICS READNEXT                                                   
               DATASET (PI-DSID)                                                
               RIDFLD  (PI-CERTIFICATE-KEY)                                     
               SET     (ADDRESS OF PURGE-CERT-MASTER)                           
           END-EXEC.                                                            
                                                                                
      ******************************************************************        
      *    IF THE SECURITY CHECKING ROUTINE, INITIAL COMPARE ROUTINE   *        
      *        OR THE ACCOUNT COMPARE ROUTINE IS CHANGED HERE, YOU     *        
      *        MUST ALSO CHANGE THE CORRESPONDING ROUTINE IN           *        
      *        PARAGRAPH 4020-COMPARE-KEY OR 4030-CHECK-OPTION.        *        
      *                                     KER/080884.                *        
      ******************************************************************        
                                                                                
           IF PI-NO-CARRIER-SECURITY AND                                        
              PI-NO-ACCOUNT-SECURITY                                            
               GO TO 7130-CHECK-INITIAL.                                        
                                                                                
           IF PI-CARRIER-SECURITY > SPACES                                      
               IF PG-CARRIER NOT = PI-CARRIER-SECURITY                          
                   GO TO 7100-READNEXT-PF2.                                     
                                                                                
           IF PI-ACCOUNT-SECURITY > SPACES                                      
               IF PG-ACCOUNT NOT = PI-ACCOUNT-SECURITY                          
                   GO TO 7100-READNEXT-PF2.                                     
                                                                                
       7130-CHECK-INITIAL.                                                      
           IF NOT OPTION-THREE-SELECTED                                         
               GO TO 7190-COMPUTE.                                              
                                                                                
           IF PI-SC-INITIALS NOT = SPACES                                       
              MOVE PI-SC-INITIALS      TO WS-INITIALS                           
              IF WS-INIT2 NOT = SPACE                                           
                 IF PI-SC-INITIALS NOT = PG-INSURED-INITIALS                    
                    GO TO 7100-READNEXT-PF2                                     
                   ELSE                                                         
                    NEXT SENTENCE                                               
                 ELSE                                                           
                 IF WS-INIT1 NOT = PG-INSURED-INITIAL1                          
                    GO TO 7100-READNEXT-PF2.                                    
                                                                                
           IF PI-SC-FIRST-NAME = SPACE                                          
               GO TO 7150-CONTINUE.                                             
                                                                                
           MOVE PI-SC-FIRST-NAME       TO WS-PI-NAME.                           
           MOVE PG-INSURED-FIRST-NAME  TO WS-CM-NAME.                           
                                                                                
           MOVE SPACE                  TO WS-COMPARE-INDICATOR.                 
           PERFORM 4035-CHECK-NAME THRU 4035-EXIT                               
               VARYING WS-NAME-INDEX FROM 15 BY -1                              
                   UNTIL WS-NAME-INDEX = ZERO.                                  
                                                                                
           IF NAME-NOT-FOUND                                                    
               GO TO 7100-READNEXT-PF2.                                         
                                                                                
       7150-CONTINUE.                                                           
           IF PI-SC-ACCT-NO = SPACES                                            
               GO TO 7190-COMPUTE.                                              
                                                                                
           IF PI-SC-ACCT-NO NOT = PG-ACCOUNT                                    
               GO TO 7100-READNEXT-PF2.                                         
                                                                                
       7190-COMPUTE.                                                            
           COMPUTE WS-CALC-RDNXT = WS-CALC-RDNXT - 1.                           
           GO TO 7100-READNEXT-PF2.                                             
                                                                                
       7999-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8000-READ-CONTROL-FILE SECTION.                                          
           MOVE ZERO                   TO  WS-NOT-FOUND                         
                                           WS-ST-REC-NOT-FOUND.                 
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND (8020-NOTFND)                                             
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
               DATASET   (WS-CONTROL-FILE-DSID)                                 
               RIDFLD    (WS-CONTROL-FILE-KEY)                                  
               SET       (ADDRESS OF CONTROL-FILE)                              
               GENERIC                                                          
               GTEQ                                                             
               KEYLENGTH (8)                                                    
           END-EXEC.                                                            
                                                                                
           IF CF-RECORD-TYPE NOT = WS-CFK-RECORD-TYPE                           
               GO TO 8020-NOTFND.                                               
                                                                                
           IF WS-CFK-RECORD-TYPE = '3'                                          
               GO TO 8090-EXIT.                                                 
                                                                                
           MOVE +1                     TO WS-INDEX.                             
                                                                                
       8010-FIND-BENEFIT.                                                       
           IF WS-CFK-BENEFIT-NO = CF-BENEFIT-CODE (WS-INDEX)                    
               GO TO 8090-EXIT.                                                 
                                                                                
           IF CF-BENEFIT-CODE (WS-INDEX) NOT < CF-HI-BEN-IN-REC                 
               GO TO 8020-NOTFND.                                               
                                                                                
           IF WS-INDEX < +8                                                     
               ADD +1  TO  WS-INDEX                                             
               GO TO 8010-FIND-BENEFIT.                                         
                                                                                
       8020-NOTFND.                                                             
           IF WS-CFK-RECORD-TYPE = '3'                                          
              MOVE +1                  TO  WS-ST-REC-NOT-FOUND                  
              GO TO 8090-EXIT.                                                  
                                                                                
           MOVE +1                     TO  WS-NOT-FOUND.                        
                                                                                
       8090-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8100-SEND-INITIAL-MAP SECTION.                                           
           MOVE SAVE-DATE              TO  ADATEO.                              
           MOVE EIBTIME                TO  TIME-IN.                             
           MOVE TIME-OUT               TO  ATIMEO.                              
                                                                                
      *    IF CREDIT-SESSION                                                    
      *       MOVE SPACES              TO BPFK4O                                
      *                                   BPFK5O.                               
      *                                                                         
      *    IF PI-COMPANY-ID  = 'CRI' OR 'LGX'                                   
      *        NEXT SENTENCE                                                    
      *       ELSE                                                              
      *        MOVE SPACES             TO BPFK9O.                               
                                                                                
           IF EMI-ERROR NOT = ZERO                                              
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF PI-ALT-NAME-COUNT > 140                                           
               MOVE ER-0765             TO  EMI-ERROR                           
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           MOVE EMI-MESSAGE-AREA (1)    TO  BEMSG1O.                            
           MOVE EMI-MESSAGE-AREA (2)    TO  BEMSG2O.                            
                                                                                
      *    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                               
      *       MOVE AL-PABON            TO BPFK6A BPFK7A                         
      *       MOVE PI-COMPANY-ID       TO BCOMPO                                
      *    ELSE                                                                 
      *       MOVE SPACES              TO BCOMPO                                
      *       MOVE AL-PADOF            TO BPFK6A BPFK7A.                        
                                                                                
           EXEC CICS SEND                                                       
               FROM   (EL128BO)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               CURSOR                                                           
               ERASE                                                            
           END-EXEC.                                                            
                                                                                
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8100-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8200-SEND-DATAONLY SECTION.                                              
           MOVE SAVE-DATE              TO  ADATEO.                              
           MOVE EIBTIME                TO  TIME-IN.                             
           MOVE TIME-OUT               TO  ATIMEO.                              
                                                                                
      *    IF CREDIT-SESSION                                                    
      *       MOVE SPACES              TO BPFK4O                                
      *                                   BPFK5O.                               
      *                                                                         
           IF EMI-ERROR NOT = ZERO                                              
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           IF PI-ALT-NAME-COUNT > 140                                           
               MOVE ER-0765             TO  EMI-ERROR                           
               PERFORM 9900-ERROR-FORMAT.                                       
                                                                                
           MOVE EMI-MESSAGE-AREA (1)   TO  BEMSG1O.                             
           MOVE EMI-MESSAGE-AREA (2)   TO  BEMSG2O.                             
                                                                                
      *    IF PI-ORIGINAL-COMPANY-ID NOT = SPACES                               
      *       MOVE AL-PABON            TO BPFK6A BPFK7A                         
      *       MOVE PI-COMPANY-ID       TO BCOMPO                                
      *    ELSE                                                                 
      *       MOVE SPACES              TO BCOMPO                                
      *       MOVE AL-PADOF            TO BPFK6A BPFK7A.                        
                                                                                
           EXEC CICS SEND DATAONLY                                              
               FROM   (EL128BO)                                                 
               MAPSET (WS-MAPSET-NAME)                                          
               MAP    (WS-MAP-NAME)                                             
               CURSOR                                                           
           END-EXEC.                                                            
                                                                                
           GO TO 9100-RETURN-TRAN.                                              
                                                                                
       8200-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8300-SEND-TEXT SECTION.                                                  
           EXEC CICS SEND TEXT                                                  
               FROM   (LOGOFF-TEXT)                                             
               LENGTH (LOGOFF-LENGTH)                                           
               ERASE                                                            
               FREEKB                                                           
           END-EXEC.                                                            
                                                                                
           EXEC CICS RETURN                                                     
           END-EXEC.                                                            
                                                                                
       8300-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8500-DATE-CONVERSION SECTION.                                            
           EXEC CICS LINK                                                       
               PROGRAM  ('ELDATCV')                                             
               COMMAREA (DATE-CONVERSION-DATA)                                  
               LENGTH   (DC-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       8500-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8600-NEXT-COMPANY SECTION.                                               
      ******************************************************************        
      ****      READ THE CURRENT COMPANY RECORD TO OBTAIN THE       ****        
      ****      NEXT COMPANY ID.                                    ****        
      ******************************************************************        
                                                                                
           MOVE SPACES                     TO  WS-CONTROL-FILE-KEY.             
           MOVE PI-COMPANY-ID              TO  WS-CFK-COMPANY-ID.               
           MOVE '1'                        TO  WS-CFK-RECORD-TYPE.              
           MOVE +0                         TO  WS-CFK-SEQUENCE-NO.              
                                                                                
           PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                            
                                                                                
           IF WS-CNTL-REC-FOUND-SW = 'N'                                        
               PERFORM 8800-INITIALIZE-MAP VARYING EL128B-INDEX                 
                 FROM +1 BY +1 UNTIL EL128B-INDEX > +8                          
               MOVE ER-0022                TO  EMI-ERROR                        
               MOVE -1                     TO  BSELL                            
               GO TO 8100-SEND-INITIAL-MAP.                                     
                                                                                
           IF EIBAID = DFHPF6                                                   
               MOVE CF-NEXT-COMPANY-ID     TO  WS-NEXT-COMPANY-ID.              
                                                                                
           IF EIBAID = DFHPF7                                                   
               MOVE PI-ORIGINAL-COMPANY-ID TO  WS-NEXT-COMPANY-ID.              
                                                                                
           IF PI-PROCESSOR-ID = 'LGXX'                                          
               GO TO 8600-CONTINUE-NEXT-COMPANY.                                
                                                                                
      ******************************************************************        
      ****      READ THE CURRENT USER RECORD FOR UPDATE AND REMOVE  ****        
      ****      THE TERMINAL ID FROM THE RECORD.                    ****        
      ******************************************************************        
                                                                                
           MOVE PI-COMPANY-ID              TO  WS-CFK-COMPANY-ID.               
           MOVE '2'                        TO  WS-CFK-RECORD-TYPE.              
           MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.              
           MOVE +0                         TO  WS-CFK-SEQUENCE-NO.              
                                                                                
           PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.                     
                                                                                
           IF WS-CNTL-REC-FOUND-SW = 'N'                                        
               MOVE ER-0019                TO  EMI-ERROR                        
               MOVE -1                     TO  BSELL                            
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           MOVE SPACES                     TO  CF-CURRENT-TERM-ON.              
                                                                                
           PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                         
                                                                                
      ******************************************************************        
      ****      READ THE USER RECORD ON THE "NEXT" COMPANY TO       ****        
      ****      VERIFY THAT A VALID USER RECORD EXISTS:             ****        
      ****        1.  MOVE USER CARRIER/ACCOUNT SECURITY TO PI-AREA ****        
      ****        2.  MOVE USER SECURITY VALUES TO SECURITY CODES   ****        
      ****            IN WORKING STORAGE                                        
      ******************************************************************        
                                                                                
           MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.               
           MOVE '2'                        TO  WS-CFK-RECORD-TYPE.              
           MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.              
           MOVE +0                         TO  WS-CFK-SEQUENCE-NO.              
                                                                                
           PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                            
                                                                                
           IF WS-CNTL-REC-FOUND-SW = 'N'                                        
               PERFORM 8800-INITIALIZE-MAP VARYING EL128B-INDEX                 
                 FROM +1 BY +1 UNTIL EL128B-INDEX > +8                          
               MOVE ER-0228                TO  EMI-ERROR                        
               MOVE -1                     TO  BSELL                            
               GO TO 8100-SEND-INITIAL-MAP.                                     
                                                                                
           MOVE CF-PROCESSOR-CARRIER       TO  PI-CARRIER-SECURITY.             
           MOVE CF-PROCESSOR-ACCOUNT       TO  PI-ACCOUNT-SECURITY.             
           MOVE CF-INDIVIDUAL-APP(1)       TO  SC-CREDIT-CODES.                 
           MOVE CF-INDIVIDUAL-APP(2)       TO  SC-CLAIMS-CODES.                 
                                                                                
      ******************************************************************        
      ****      READ THE USER RECORD ON THE "NEXT" COMPANY FOR      ****        
      ****      UPDATE AND MOVE THE TERMINAL ID INTO THE RECORD.    ****        
      ******************************************************************        
                                                                                
           MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.               
           MOVE '2'                        TO  WS-CFK-RECORD-TYPE.              
           MOVE PI-PROCESSOR-ID            TO  WS-CFK-ACCESS-TYPE.              
           MOVE +0                         TO  WS-CFK-SEQUENCE-NO.              
                                                                                
           PERFORM 8910-READ-CONTROL-UPDATE THRU 8910-EXIT.                     
                                                                                
           IF WS-CNTL-REC-FOUND-SW = 'N'                                        
               MOVE ER-0228                TO  EMI-ERROR                        
               MOVE -1                     TO  BSELL                            
               GO TO 8200-SEND-DATAONLY.                                        
                                                                                
           MOVE EIBTRMID                   TO  CF-CURRENT-TERM-ON.              
                                                                                
           PERFORM 8920-REWRITE-CONTROL THRU 8920-EXIT.                         
                                                                                
       8600-CONTINUE-NEXT-COMPANY.                                              
      ******************************************************************        
      ****      READ THE NEW COMPANY RECORD TO VERIFY THAT IT       ****        
      ****      EXISTS AND THEN MOVE SPECIFIC DATA TO PI-AREA.      ****        
      ******************************************************************        
                                                                                
           MOVE SPACES                     TO  WS-CONTROL-FILE-KEY.             
           MOVE WS-NEXT-COMPANY-ID         TO  WS-CFK-COMPANY-ID.               
           MOVE '1'                        TO  WS-CFK-RECORD-TYPE.              
           MOVE +0                         TO  WS-CFK-SEQUENCE-NO.              
                                                                                
           PERFORM 8900-READ-CONTROL THRU 8900-EXIT.                            
                                                                                
           IF WS-CNTL-REC-FOUND-SW = 'N'                                        
               PERFORM 8800-INITIALIZE-MAP VARYING EL128B-INDEX                 
                  FROM +1 BY +1 UNTIL EL128B-INDEX > +8                         
               MOVE ER-0089                TO  EMI-ERROR                        
               MOVE -1                     TO  BSELL                            
               GO TO 8100-SEND-INITIAL-MAP.                                     
                                                                                
           MOVE SPACES                 TO  PI-PROGRAM-WORK-AREA.                
                                                                                
           MOVE +1                     TO  PI-START-SW                          
                                           PI-KEY-LENGTH.                       
                                                                                
           MOVE +0                     TO  PI-1ST-TIME-SW                       
                                           PI-LINE-COUNT                        
                                           PI-AIX-RECORD-COUNT                  
                                           PI-BROWSE-SW                         
                                           PI-END-OF-FILE                       
                                           PI-TS-ITEM                           
                                           PI-SCREEN-COUNT                      
                                           PI-SUB                               
                                           PI-ALT-NAME-COUNT.                   
                                                                                
           MOVE LOW-VALUES             TO  PI-CERT-CONTROLS-EL128 (1)           
                                           PI-CERT-CONTROLS-EL128 (2)           
                                           PI-CERT-CONTROLS-EL128 (3)           
                                           PI-CERT-CONTROLS-EL128 (4)           
                                           PI-CERT-CONTROLS-EL128 (5).          
                                                                                
           MOVE 'ELCERT'                   TO  PI-DSID.                         
           MOVE CF-COMPANY-CD              TO  PI-COMPANY-CD                    
                                               PI-CK-COMPANY-CD.                
           MOVE CF-COMPANY-ID              TO  PI-COMPANY-ID.                   
           MOVE CF-COMPANY-PASSWORD        TO  PI-COMPANY-PASSWORD.             
           MOVE CF-LGX-CREDIT-USER         TO  PI-CREDIT-USER.                  
           MOVE CF-LGX-CLAIM-USER          TO  PI-CLAIM-USER.                   
           MOVE CF-CERT-ACCESS-CONTROL     TO  PI-CERT-ACCESS-CONTROL.          
           MOVE CF-CARRIER-CONTROL-LEVEL   TO  PI-CARRIER-CONTROL-LEVEL.        
           MOVE CF-JOURNAL-FILE-ID         TO  PI-JOURNAL-FILE-ID.              
           MOVE CF-LOWER-CASE-LETTERS      TO  PI-LOWER-CASE-LETTERS.           
           MOVE CF-CLAIM-PAID-THRU-TO      TO  PI-CLAIM-PAID-THRU-TO.           
                                                                                
           MOVE CF-LIFE-OVERRIDE-L1    TO  PI-LIFE-OVERRIDE-L1.                 
           MOVE CF-LIFE-OVERRIDE-L2    TO  PI-LIFE-OVERRIDE-L2.                 
           MOVE CF-LIFE-OVERRIDE-L6    TO  PI-LIFE-OVERRIDE-L6.                 
           MOVE CF-LIFE-OVERRIDE-L12   TO  PI-LIFE-OVERRIDE-L12.                
                                                                                
           MOVE CF-AH-OVERRIDE-L1      TO  PI-AH-OVERRIDE-L1.                   
           MOVE CF-AH-OVERRIDE-L2      TO  PI-AH-OVERRIDE-L2.                   
           MOVE CF-AH-OVERRIDE-L6      TO  PI-AH-OVERRIDE-L6.                   
           MOVE CF-AH-OVERRIDE-L12     TO  PI-AH-OVERRIDE-L12.                  
                                                                                
           IF CREDIT-SESSION                                                    
               MOVE CF-CURRENT-MONTH-END                                        
                                       TO  PI-CR-MONTH-END-DT                   
               MOVE CF-CAR-GROUP-ACCESS-CNTL                                    
                                       TO  PI-CAR-GROUP-ACCESS-CNTL             
               MOVE CF-CR-PRINT-ADDRESS-LABELS                                  
                                       TO  PI-LABEL-CONTROL                     
           ELSE                                                                 
               MOVE CF-PRINT-ADDRESS-LABELS                                     
                                       TO  PI-LABEL-CONTROL.                    
                                                                                
       8600-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8620-WRITE-TEMP-STORAGE   SECTION.                                       
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               QIDERR   (8625-WRITE-TEMP-STORAGE)                               
           END-EXEC.                                                            
                                                                                
           EXEC CICS DELETEQ TS                                                 
               QUEUE  (WS-TEMP-STORAGE-KEY)                                     
           END-EXEC.                                                            
                                                                                
       8625-WRITE-TEMP-STORAGE.                                                 
                                                                                
           MOVE -1             TO  BSELL.                                       
                                                                                
           EXEC CICS WRITEQ TS                                                  
                FROM   (EL128BO)                                                
                LENGTH (WS-TS-LENGTH)                                           
                QUEUE  (WS-TEMP-STORAGE-KEY)                                    
                ITEM   (PI-TS-ITEM)                                             
           END-EXEC.                                                            
                                                                                
       8630-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8650-WRITE-SECURITY-TEMP-STORE   SECTION.                                
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               QIDERR   (8651-WRITE-SECURITY)                                   
           END-EXEC.                                                            
                                                                                
           MOVE EIBTRMID               TO QID.                                  
                                                                                
       8651-WRITE-SECURITY.                                                     
                                                                                
           EXEC CICS WRITEQ TS                                                  
               QUEUE   (QID)                                                    
               FROM    (SECURITY-CONTROL)                                       
               LENGTH  (SC-COMM-LENGTH)                                         
               ITEM    (QID-ITEM)                                               
           END-EXEC.                                                            
                                                                                
           MOVE QID                    TO PI-SECURITY-TEMP-STORE-ID.            
                                                                                
           IF PI-PROCESSOR-ID = 'LGXX'                                          
               MOVE ALL 'Y'            TO SC-CREDIT-CODES                       
                                          SC-CLAIMS-CODES                       
                                          PI-PROCESSOR-USER-ALMIGHTY.           
                                                                                
       8650-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8700-NOT-FOUND SECTION.                                                  
           PERFORM 8800-INITIALIZE-MAP VARYING EL128B-INDEX                     
             FROM +1 BY +1 UNTIL EL128B-INDEX > +8.                             
           MOVE -1                     TO BSELL.                                
           MOVE ER-0201                TO EMI-ERROR.                            
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
       8700-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8800-INITIALIZE-MAP SECTION.                                             
           MOVE LOW-VALUES            TO EL128B-MAP-LINE (EL128B-INDEX).        
       8800-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       8900-READ-CONTROL SECTION.                                               
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND   (8900-NOTFND)                                           
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
               DATASET   (WS-CONTROL-FILE-DSID)                                 
               RIDFLD    (WS-CONTROL-FILE-KEY)                                  
               SET       (ADDRESS OF CONTROL-FILE)                              
           END-EXEC.                                                            
                                                                                
           MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.                
           GO TO 8900-EXIT.                                                     
                                                                                
       8900-NOTFND.                                                             
           MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.                
                                                                                
       8900-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8910-READ-CONTROL-UPDATE.                                                
                                                                                
           EXEC CICS HANDLE CONDITION                                           
               NOTFND   (8910-NOTFND)                                           
           END-EXEC.                                                            
                                                                                
           EXEC CICS READ                                                       
               DATASET   (WS-CONTROL-FILE-DSID)                                 
               RIDFLD    (WS-CONTROL-FILE-KEY)                                  
               SET       (ADDRESS OF CONTROL-FILE)                              
               UPDATE                                                           
           END-EXEC.                                                            
                                                                                
           MOVE 'Y'                    TO  WS-CNTL-REC-FOUND-SW.                
           GO TO 8910-EXIT.                                                     
                                                                                
       8910-NOTFND.                                                             
           MOVE 'N'                    TO  WS-CNTL-REC-FOUND-SW.                
                                                                                
       8910-EXIT.                                                               
           EXIT.                                                                
                                                                                
       8920-REWRITE-CONTROL.                                                    
                                                                                
           EXEC CICS REWRITE                                                    
               DATASET   (WS-CONTROL-FILE-DSID)                                 
               FROM      (CONTROL-FILE)                                         
           END-EXEC.                                                            
                                                                                
       8920-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9000-RETURN-CICS SECTION.                                                
           MOVE 'EL005'                TO  THIS-PGM.                            
           MOVE EIBAID                 TO  PI-ENTRY-CD-1.                       
           GO TO 9300-XCTL.                                                     
                                                                                
       9000-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9050-FLA-NAME.                                                           
           MOVE PG-INSURED-LAST-NAME   TO  SAVE-NAME.                           
           MOVE ZERO                   TO  A-SUB.                               
                                                                                
       9050-ADD.                                                                
           ADD 1 TO A-SUB.                                                      
           IF A-SUB > 15                                                        
               GO TO 9050-EXIT-FIX.                                             
                                                                                
           IF SV-BYTE (A-SUB) = ' '                                             
               MOVE A-SUB              TO B-SUB                                 
               GO TO 9050-PASS.                                                 
                                                                                
           GO TO 9050-ADD.                                                      
                                                                                
       9050-PASS.                                                               
           ADD 1 TO A-SUB.                                                      
           IF A-SUB > 15                                                        
               GO TO 9050-EXIT-FIX.                                             
                                                                                
           IF SV-BYTE (A-SUB) = '*'                                             
               MOVE '*'                TO SV-BYTE (B-SUB)                       
               MOVE ' '                TO SV-BYTE (A-SUB)                       
               GO TO 9050-EXIT-FIX.                                             
                                                                                
           GO TO 9050-PASS.                                                     
                                                                                
       9050-EXIT-FIX.                                                           
           MOVE SAVE-NAME              TO PG-INSURED-LAST-NAME.                 
                                                                                
       9100-RETURN-TRAN SECTION.                                                
           MOVE EMI-ERROR-NUMBER (1)   TO  PI-LAST-ERROR-NO.                    
           MOVE WS-MAP-NUMBER          TO  PI-CURRENT-SCREEN-NO.                
           MOVE EIBAID                 TO  PI-PREV-AID.                         
                                                                                
           EXEC CICS RETURN                                                     
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                               
               LENGTH   (PI-COMM-LENGTH)                                        
               TRANSID  (WS-TRANS-ID)                                           
               END-EXEC.                                                        
                                                                                
       9100-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9300-XCTL SECTION.                                                       
           MOVE DFHENTER               TO  EIBAID.                              
                                                                                
           IF THIS-PGM = 'EL1283'                                               
               MOVE EIBTRMID       TO  WS-TS1-TERM-ID                           
               EXEC CICS WRITEQ TS                                              
                   FROM   (PI-PROGRAM-WORK-AREA)                                
                   LENGTH (WS-WORK-LENGTH)                                      
                   QUEUE  (WS-EL1283-TS)                                        
               END-EXEC.                                                        
                                                                                
           EXEC CICS XCTL                                                       
               PROGRAM  (THIS-PGM)                                              
               COMMAREA (PROGRAM-INTERFACE-BLOCK)                               
               LENGTH   (PI-COMM-LENGTH)                                        
           END-EXEC.                                                            
                                                                                
       9300-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9400-CLEAR SECTION.                                                      
           MOVE PI-RETURN-TO-PROGRAM   TO  THIS-PGM.                            
           GO TO 9300-XCTL.                                                     
                                                                                
       9400-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9600-PGMIDERR SECTION.                                                   
           EXEC CICS HANDLE CONDITION                                           
               PGMIDERR (8300-SEND-TEXT)                                        
           END-EXEC.                                                            
                                                                                
           MOVE THIS-PGM               TO  PI-CALLING-PROGRAM                   
                                           LOGOFF-PGM.                          
           MOVE 'EL005'                TO  THIS-PGM.                            
           MOVE PGMIDERR-MSG           TO  LOGOFF-FILL.                         
           MOVE SPACES                 TO  PI-ENTRY-CD-1.                       
           GO TO 9300-XCTL.                                                     
                                                                                
       9600-EXIT.                                                               
           EXIT.                                                                
                                                                                
           EJECT                                                                
       9900-ERROR-FORMAT SECTION.                                               
                                                                                
           EXEC CICS LINK                                                       
               PROGRAM  ('EL001')                                               
               COMMAREA (ERROR-MESSAGE-INTERFACE-BLOCK)                         
               LENGTH   (EMI-COMM-LENGTH)                                       
           END-EXEC.                                                            
                                                                                
       9900-EXIT.                                                               
           EXIT.                                                                
                                                                                
       9990-ERROR SECTION.                                                      
           MOVE DFHEIBLK               TO EMI-LINE1.                            
                                                                                
           EXEC CICS LINK                                                       
               PROGRAM  ('EL004')                                               
               COMMAREA (EMI-LINE1)                                             
               LENGTH   (72)                                                    
           END-EXEC.                                                            
                                                                                
           MOVE -1                     TO BSELL.                                
           GO TO 8100-SEND-INITIAL-MAP.                                         
                                                                                
