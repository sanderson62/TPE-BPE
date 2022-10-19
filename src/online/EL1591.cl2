       ID DIVISION.                                                     
                                                                        
       PROGRAM-ID.                 EL1591.
      *                                                                 
      *AUTHOR.     PABLO                                                
      *            OMAHA NE                                             
                                                                        
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CENTRAL STATES  *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CENTRAL STATES IS EXPRESSLY PROHIBITED       *
      *            *   WITHOUT THE PRIOR WRITTEN PERMISSION OF         *
      *            *   CENTRAL STATES                                  *
      *            *                                                   *
      *            *****************************************************
                                                                        
      *REMARKS.                                                         
                                                                        
      *        THIS PROGRAM PROVIDES THE MAINTENANCE FUNCTIONS NEEDED   
      *    FOR PRODUCT DEFINITIONS CLP UEP FACTORS.
                                                                        
      *    SCREENS     - EL159B - PRODUCT/FACTORS DEFINITION                    
                                                                        
      *    ENTERED BY  - EL159 - MAINTENANCE MENU                       
                                                                        
      *    EXIT TO     - EL159 - MAINTENANCE MENU                       
                                                                        
      *    INPUT FILE  - ERPDEF -              - PRODUCT DEFINITION     
                                                                        
      *    OUTPUT FILE - ERPDEF -              - PRODUCT DEFINITION     
                                                                        
      *    COMMAREA    - PASSED                                         
                                                                        
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON     
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE 
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA E031) THE SCREEN   
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE   
      *                  MAINTENANCE TYPE INDICATED.                    
                                                                        
041515******************************************************************
041515*                   C H A N G E   L O G
041515*
041515* Changes are marked by the Change Effective date.
041515*-----------------------------------------------------------------
041515*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
041515* EFFECTIVE    NUMBER
041515*-----------------------------------------------------------------
041515* 041515    2015041400001  PEMA  USE CORRECT FORMATTED MAP FLD
041515******************************************************************
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*   EL1591 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
                                                                        
       77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.    
           88  FIRST-READ-PREV                         VALUE 'Y'.       
       77  M1                              PIC S999 COMP-3 VALUE +0.
       77  M2                              PIC S999 COMP-3 VALUE +0.
       77  X1                              PIC S999 COMP-3 VALUE +0.
       77  Y1                              PIC S999 COMP-3 VALUE +0.
       01  ACCESS-KEYS.                                                 
           12  ERPDEF-KEY.                                              
               16  ERPDEF-COMPANY-CD       PIC X.
               16  ERPDEF-STATE            PIC XX.
               16  ERPDEF-PROD-CD          PIC XXX.
               16  FILLER                  PIC X(7).
               16  ERPDEF-BEN-TYPE         PIC X.
               16  ERPDEF-BEN-CODE         PIC XX.
               16  ERPDEF-EXP-DT           PIC XX.

       01  WS-DATE-AREA.                                                
           05  SAVE-DATE                   PIC X(08)   VALUE SPACES.    
           05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.    
                                                                        
       01  MISC-WORK-AREAS.                                             
                                                                        
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-9V999-OF-DEEDIT REDEFINES
                   WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
           12  DEEDIT-FIELD                PIC X(11).                   
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD                   
                                           PIC S9(11).                  
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD                   
                                           PIC S9(09)V99.               

           12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.
                                                                        
       01  STANDARD-AREAS.                                              
           12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.  
           12  TRANS-ID                    PIC X(04)   VALUE 'E032'.
           12  PGM-NAME                    PIC X(08).                   
           12  TIME-IN                     PIC S9(07).                  
           12  TIME-OUT-R  REDEFINES TIME-IN.                           
               16  FILLER                  PIC X(01).                   
               16  TIME-OUT                PIC 99V99.                   
               16  FILLER                  PIC X(02).                   
           12  XCTL-005                    PIC X(08)   VALUE 'EL005'.   
           12  XCTL-010                    PIC X(08)   VALUE 'EL010'.   
           12  XCTL-155                    PIC X(08)   VALUE 'EL155'.   
           12  XCTL-626                    PIC X(08)   VALUE 'EL626'.   
           12  LINK-001                    PIC X(08)   VALUE 'EL001'.   
           12  LINK-004                    PIC X(08)   VALUE 'EL004'.   
           12  LINK-ELDATCV                PIC X(08)   VALUE 'ELDATCV'. 
           12  THIS-PGM                    PIC X(08)   VALUE 'EL1591'.   
           12  ERPDEF-FILE-ID              PIC X(08)   VALUE 'ERPDEF'.  
           12  ERPDEF-LENGTH               PIC S9(04)  VALUE +1319 COMP.
           12  SUB                         PIC 9(02).                   
           12  SUB-1                       PIC 9(02).                   
           12  SUB2                        PIC 9(02).                   
           12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.     
           12  MAPSET-NAME                 PIC X(08)   VALUE 'EL1591S'.
           12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL159B'.  
           12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.     
                                                                        
                                                                        
           12  WS-MAXAMT                   PIC S9(9)    VALUE +0 COMP-3.
           12  WS-MAXATTAGE                PIC S999     VALUE +0 COMP-3.
           12  WS-MINAGE                   PIC S999     VALUE +0 COMP-3.
           12  WS-MAXAGE                   PIC S999     VALUE +0 COMP-3.
           12  WS-MAXTERM                  PIC S999     VALUE +0 COMP-3.
           12  WS-EXCL                     PIC S999     VALUE +0 COMP-3.
           12  WS-COV-ENDS                 PIC S999     VALUE +0 COMP-3.
           12  WS-ACC-ONLY                 PIC S999     VALUE +0 COMP-3.
           12  WS-CRIT-PER                 PIC S999     VALUE +0 COMP-3.


                                           COPY ELCSCTM.                
                                           COPY ELCSCRTY.               

       01  ERROR-MESSAGES.                                              
           12  ER-0000                     PIC X(04)   VALUE '0000'.    
           12  ER-0023                     PIC X(04)   VALUE '0023'.    
           12  ER-0029                     PIC X(04)   VALUE '0029'.    
           12  ER-0050                     PIC X(04)   VALUE '0050'.    
           12  ER-0068                     PIC X(04)   VALUE '0068'.    
           12  ER-0070                     PIC X(04)   VALUE '0070'.    
           12  ER-0130                     PIC X(04)   VALUE '0130'.    
           12  ER-0131                     PIC X(04)   VALUE '0131'.    
           12  ER-0132                     PIC X(04)   VALUE '0132'.    
           12  ER-0138                     PIC X(04)   VALUE '0138'.    
           12  ER-0144                     PIC X(04)   VALUE '0144'.    
           12  ER-0145                     PIC X(04)   VALUE '0145'.
           12  ER-0418                     PIC X(04)   VALUE '0418'.    
           12  ER-0582                     PIC X(04)   VALUE '0582'.    
           12  ER-0701                     PIC X(04)   VALUE '0701'.    
           12  ER-0702                     PIC X(04)   VALUE '0702'.    
           12  ER-0703                     PIC X(04)   VALUE '0703'.    
           12  ER-0704                     PIC X(04)   VALUE '0704'.    
           12  ER-0705                     PIC X(04)   VALUE '0705'.    
           12  ER-0706                     PIC X(04)   VALUE '0706'.    
           12  ER-0707                     PIC X(04)   VALUE '0707'.    
           12  ER-0708                     PIC X(04)   VALUE '0708'.    
           12  ER-0709                     PIC X(04)   VALUE '0709'.    
           12  ER-0710                     PIC X(04)   VALUE '0710'.    
           12  ER-0711                     PIC X(04)   VALUE '0711'.    
           12  ER-0712                     PIC X(04)   VALUE '0712'.    
           12  ER-0713                     PIC X(04)   VALUE '0713'.    
           12  ER-0717                     PIC X(04)   VALUE '0717'.    
           12  ER-0718                     PIC X(04)   VALUE '0718'.    
           12  ER-0719                     PIC X(04)   VALUE '0719'.    
           12  ER-0720                     PIC X(04)   VALUE '0720'.    
           12  ER-0721                     PIC X(04)   VALUE '0721'.    
           12  ER-0722                     PIC X(04)   VALUE '0722'.    
           12  ER-0723                     PIC X(04)   VALUE '0723'.    
           12  ER-0724                     PIC X(04)   VALUE '0724'.    
           12  ER-0725                     PIC X(04)   VALUE '0725'.    
           12  ER-0726                     PIC X(04)   VALUE '0726'.    
           12  ER-0727                     PIC X(04)   VALUE '0727'.    
           12  ER-0729                     PIC X(04)   VALUE '0729'.    
           12  ER-0754                     PIC X(04)   VALUE '0754'.    
           12  ER-2241                     PIC X(04)   VALUE '2241'.    
           12  ER-2276                     PIC X(04)   VALUE '2276'.    
           12  ER-7008                     PIC X(04)   VALUE '7008'.    
           12  ER-7031                     PIC X(04)   VALUE '7031'.    
           12  ER-7123                     PIC X(04)   VALUE '7123'.    
           12  ER-8150                     PIC X(04)   VALUE '8150'.    
           12  ER-9999                     PIC XXXX    VALUE '9999'.
                                           COPY ELCDATE.                

                                           COPY ELCLOGOF.               

                                           COPY ELCATTR.                

                                           COPY ELCEMIB.                

                                           COPY ELCJPFX.                
                                           PIC X(530).                  
                                                                        

                                           COPY ELCINTF.                
           12  FILLER    REDEFINES PI-PROGRAM-WORK-AREA.                
                                                                        
               16  PI-FORM-NUMBER          PIC X(12).                   
                                                                        
               16  PI-PREV-PROD-KEY.
                   20  PI-PREV-COMPANY-CD  PIC X.
                   20  PI-PREV-STATE       PIC XX.
                   20  PI-PREV-PROD-CD     PIC XXX.
                   20  FILLER              PIC X(7).
                   20  PI-PREV-BEN-TYPE    PIC X.
                   20  PI-PREV-BEN-CODE    PIC XX.
                   20  PI-PREV-EXP-DT      PIC XX.
                                                                        
               16  PI-PROD-KEY.
                   20  PI-PK-COMPANY-CD    PIC X.
                   20  PI-PK-STATE         PIC XX.
                   20  PI-PK-PROD-CD       PIC XXX.
                   20  FILLER              PIC X(7).
                   20  PI-BEN-TYPE         PIC X.
                   20  PI-BEN-CODE         PIC XX.
                   20  PI-EXP-DT           PIC XX.
               16  PI-LEFT-RIGHT           PIC X.
                   88  PI-FAR-LEFT           VALUE 'L'.
                   88  PI-FAR-RIGHT          VALUE 'R'.
               16  PI-UP-DOWN              PIC X.
                   88  PI-FAR-UP             VALUE 'U'.
                   88  PI-FAR-DOWN           VALUE 'D'.
               16  PI-X1                   PIC S999 COMP-3.
               16  PI-Y1                   PIC S999 COMP-3.

               16  FILLER                  PIC X(583).

                                           COPY ELCAID.                 
       01  FILLER    REDEFINES DFHAID.                                  
           12  FILLER                      PIC X(08).                   
           12  PF-VALUES                   PIC X         OCCURS 24.     

                                           COPY EL1591S.
       01  EL159BI-R REDEFINES EL159BI.                                 
           12  FILLER                      PIC X(217).
           12  EL159-FACT-TABLE OCCURS 8.
               16  LO-TERM-LEN             PIC S9(04)  COMP.            
               16  LO-TERM-ATTRB           PIC X(01).                   
               16  LO-TERM                 PIC 999.
               16  HI-TERM-LEN             PIC S9(04)  COMP.            
               16  HI-TERM-ATTRB           PIC X(01).                   
               16  HI-TERM                 PIC 999.
               16  LO-AMT-LEN              PIC S9(04)  COMP.            
               16  LO-AMT-ATTRB            PIC X(01).                   
               16  LO-AMT                  PIC 9(6).
               16  LO-AMTO REDEFINES LO-AMT PIC Z9,999.
               16  HI-AMT-LEN              PIC S9(04)  COMP.            
               16  HI-AMT-ATTRB            PIC X(01).                   
               16  HI-AMT                  PIC 9(8).
               16  HI-AMTO REDEFINES HI-AMT PIC ZZZ9,999.
               16  FACT0-LEN               PIC S9(04)  COMP.            
               16  FACT0-ATTRB             PIC X(01).                   
               16  FACT0                   PIC X(5).
               16  FACT0O REDEFINES FACT0  PIC 9.999.
               16  FACT0I REDEFINES FACT0  PIC 99V999.
               16  FACT1-LEN               PIC S9(04)  COMP.            
               16  FACT1-ATTRB             PIC X(01).                   
               16  FACT1                   PIC X(5).
               16  FACT1O REDEFINES FACT1  PIC 9.999.
               16  FACT1I REDEFINES FACT1  PIC 99V999.
               16  FACT2-LEN               PIC S9(04)  COMP.            
               16  FACT2-ATTRB             PIC X(01).                   
               16  FACT2                   PIC X(5).
               16  FACT2O REDEFINES FACT2  PIC 9.999.
               16  FACT2I REDEFINES FACT2  PIC 99V999.
               16  FACT3-LEN               PIC S9(04)  COMP.            
               16  FACT3-ATTRB             PIC X(01).                   
               16  FACT3                   PIC X(5).
               16  FACT3O REDEFINES FACT3  PIC 9.999.
               16  FACT3I REDEFINES FACT3  PIC 99V999.
               16  FACT4-LEN               PIC S9(04)  COMP.            
               16  FACT4-ATTRB             PIC X(01).                   
               16  FACT4                   PIC X(5).
               16  FACT4O REDEFINES FACT4  PIC 9.999.
               16  FACT4I REDEFINES FACT4  PIC 99V999.
               16  FACT5-LEN               PIC S9(04)  COMP.            
               16  FACT5-ATTRB             PIC X(01).                   
               16  FACT5                   PIC X(5).
               16  FACT5O REDEFINES FACT5  PIC 9.999.
               16  FACT5I REDEFINES FACT5  PIC 99V999.
               16  FACT6-LEN               PIC S9(04)  COMP.            
               16  FACT6-ATTRB             PIC X(01).                   
               16  FACT6                   PIC X(5).
               16  FACT6O REDEFINES FACT6  PIC 9.999.
               16  FACT6I REDEFINES FACT6  PIC 99V999.
               16  FACT7-LEN               PIC S9(04)  COMP.            
               16  FACT7-ATTRB             PIC X(01).                   
               16  FACT7                   PIC X(5).
               16  FACT7O REDEFINES FACT7  PIC 9.999.
               16  FACT7I REDEFINES FACT7  PIC 99V999.

       LINKAGE SECTION.                                                 
                                                                        
       01  DFHCOMMAREA                     PIC X(1024).                 

                                           COPY ERCPDEF.

       PROCEDURE DIVISION.                                              
                                                                        
           MOVE EIBDATE                TO DC-JULIAN-YYDDD
           MOVE '5'                    TO DC-OPTION-CODE
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT
           MOVE DC-GREG-DATE-1-EDIT    TO SAVE-DATE
           MOVE DC-BIN-DATE-1          TO SAVE-BIN-DATE

           MOVE DFHCOMMAREA            TO PROGRAM-INTERFACE-BLOCK
           IF EIBCALEN = 0
              GO TO 8800-UNAUTHORIZED-ACCESS.

           MOVE PI-LIFE-OVERRIDE-L6    TO EMI-LIFE-OVERRIDE-L6
           MOVE PI-AH-OVERRIDE-L6      TO EMI-AH-OVERRIDE-L6
           MOVE 1                      TO EMI-NUMBER-OF-LINES

           EXEC CICS HANDLE CONDITION                                   
               DUPREC     (8850-DUPREC)                                 
               NOTOPEN    (8870-NOTOPEN)                                
               NOTFND     (8880-NOT-FOUND)                              
               PGMIDERR   (9600-PGMID-ERROR)                            
               ERROR      (9990-ABEND)                                  
           END-EXEC.                                                    

           .
       0150-SET-PROGRAM-SAVES.                                          
                                                                        
           IF PI-CALLING-PROGRAM NOT EQUAL THIS-PGM                     
               IF PI-RETURN-TO-PROGRAM NOT EQUAL THIS-PGM               
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-6      
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-5      
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-4      
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-3      
                   MOVE PI-SAVED-PROGRAM-1   TO PI-SAVED-PROGRAM-2      
                   MOVE PI-RETURN-TO-PROGRAM TO PI-SAVED-PROGRAM-1      
                   MOVE PI-CALLING-PROGRAM   TO PI-RETURN-TO-PROGRAM    
                   MOVE THIS-PGM             TO PI-CALLING-PROGRAM      
               ELSE                                                     
                   MOVE PI-RETURN-TO-PROGRAM TO PI-CALLING-PROGRAM      
                   MOVE PI-SAVED-PROGRAM-1   TO PI-RETURN-TO-PROGRAM    
                   MOVE PI-SAVED-PROGRAM-2   TO PI-SAVED-PROGRAM-1      
                   MOVE PI-SAVED-PROGRAM-3   TO PI-SAVED-PROGRAM-2      
                   MOVE PI-SAVED-PROGRAM-4   TO PI-SAVED-PROGRAM-3      
                   MOVE PI-SAVED-PROGRAM-5   TO PI-SAVED-PROGRAM-4      
                   MOVE PI-SAVED-PROGRAM-6   TO PI-SAVED-PROGRAM-5      
                   MOVE SPACES               TO PI-SAVED-PROGRAM-6      
           ELSE                                                         
               GO TO 0200-RECEIVE.

           GO TO 1000-SHOW-PROD-RECORD

           .
       0200-RECEIVE.

           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR.
                                                                        
           IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   
               MOVE LOW-VALUES             TO  EL159BI                  
               MOVE ER-7008                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                     TO  MAINTL                   
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           IF PI-PROCESSOR-ID EQUAL 'LGXX'                              
               NEXT SENTENCE                                            
           ELSE                                                         
               EXEC CICS READQ TS                                       
                   QUEUE  (PI-SECURITY-TEMP-STORE-ID)                   
                   INTO   (SECURITY-CONTROL)                            
                   LENGTH (SC-COMM-LENGTH)                              
                   ITEM   (SC-ITEM)                                     
               END-EXEC                                                 
               MOVE SC-CLAIMS-DISPLAY (4)  TO  PI-DISPLAY-CAP           
               MOVE SC-CLAIMS-UPDATE  (4)  TO  PI-MODIFY-CAP.           
                                                                        
           EXEC CICS RECEIVE                                            
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               INTO     (EL159BI)                                       
           END-EXEC
                                                                        
           IF PFKEYL = +0                                     
               GO TO 0300-CHECK-PFKEYS.                                 
                                                                        
           IF (PFKEYI NUMERIC) AND (PFKEYI GREATER 0 AND LESS 25)       
               MOVE PF-VALUES (PFKEYI)     TO  EIBAID                   
           ELSE                                                         
               MOVE ER-0029                TO  EMI-ERROR                
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
                                                                        
       0300-CHECK-PFKEYS.                                               
                                                                        
           IF EIBAID EQUAL DFHPF23                                      
               GO TO 8810-PF23.                                         
                                                                        
           IF EIBAID EQUAL DFHPF24                                      
               GO TO 9200-RETURN-MAIN-MENU.                             
                                                                        
           IF EIBAID EQUAL DFHPF12                                      
               GO TO 9500-PF12.                                         
                                                                        
           IF (MAINTL <> 0) AND (EIBAID <> DFHENTER)      
               MOVE ER-0050            TO EMI-ERROR                     
               GO TO 0320-INPUT-ERROR.                                  
                                                                        
           EVALUATE EIBAID
              WHEN DFHPF1
                 GO TO 5000-FIND-NEXT-PROD-RECORD
              WHEN DFHPF2
                 GO TO 5100-FIND-PREV-PROD-RECORD
              WHEN DFHPF3
                 GO TO 3000-SCROLL-FWD
              WHEN DFHPF4
                 GO TO 3100-SCROLL-BWD
              WHEN DFHPF5
                 GO TO 3200-SCROLL-RT
              WHEN DFHPF6
                 GO TO 3300-SCROLL-LT
              WHEN DFHENTER
                 GO TO 0330-EDIT-DATA
           END-EVALUATE

           IF EIBAID EQUAL DFHENTER                                     
               GO TO 0330-EDIT-DATA.                                    
                                                                        
           MOVE ER-0029                    TO  EMI-ERROR.               
                                                                        
       0320-INPUT-ERROR.                                                
                                                                        
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE AL-UNBON                   TO  PFKEYA.                  
           MOVE -1                         TO  PFKEYL.                  
           GO TO 8200-SEND-DATAONLY.                                    
                                                                        
           EJECT                                                        
       0330-EDIT-DATA.                                                  
                                                                        
           IF NOT DISPLAY-CAP                                           
               MOVE 'READ'                 TO  SM-READ                  
               PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           
               MOVE ER-0070                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                     TO  MAINTL                   
               GO TO 8100-SEND-INITIAL-MAP.                             
                                                                        
           IF MAINTI EQUAL 'S'                                          
               GO TO 1000-SHOW-PROD-RECORD.                             
                                                                        
           IF MAINTI = 'C'
              IF NOT MODIFY-CAP                                         
                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        
                  MOVE ER-0070             TO  EMI-ERROR                
                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
                  MOVE LOW-VALUES          TO  EL159BO                  
                  MOVE -1                  TO  MAINTL                   
                  GO TO 8100-SEND-INITIAL-MAP.                          
                                                                        
           IF MAINTI EQUAL 'C'
               GO TO 2000-CHANGE-PROD-RECORD.

           MOVE ER-0023                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  MAINTL.                  
           MOVE AL-UABON                   TO  MAINTA.                  
           GO TO 8200-SEND-DATAONLY.                                    

       1000-SHOW-PROD-RECORD.

           MOVE PI-PROD-KEY            TO ERPDEF-KEY
                                          PI-PREV-PROD-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF RESP-NORMAL
              MOVE +1                  TO X1 Y1 PI-X1 PI-Y1
              SET PI-FAR-LEFT          TO TRUE
              SET PI-FAR-UP            TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
           END-IF

           MOVE ER-0418                TO EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE -1                     TO MAINTL
           MOVE AL-UABOF               TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA

           GO TO 8200-SEND-DATAONLY

           .
       2000-CHANGE-PROD-RECORD.

           IF PI-PROD-KEY NOT = PI-PREV-PROD-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT

           IF NOT EMI-NO-ERRORS
               GO TO 8200-SEND-DATAONLY.

           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)                                  
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           IF PD-LAST-MAINT-BY     NOT = PI-UPDATE-BY OR            
              PD-LAST-MAINT-HHMMSS NOT = PI-UPDATE-HHMMSS           
               EXEC CICS UNLOCK                                         
                   DATASET   (ERPDEF-FILE-ID)                           
               END-EXEC                                                 
               MOVE ER-0068                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 1000-SHOW-PROD-RECORD.                             
                                                                        
           MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.        
           MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.    
           MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.        

           IF PDESCL > +0
              MOVE PDESCI              TO PD-PRODUCT-DESC
           END-IF

           MOVE PI-X1                  TO X1
           MOVE PI-Y1                  TO Y1
           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF LO-TERM-LEN (M1) > +0
                 MOVE LO-TERM (M1)     TO PD-LOW-TERM (X1)
              END-IF
              IF HI-TERM-LEN (M1) > +0
                 MOVE HI-TERM (M1)     TO PD-HI-TERM (X1)
              END-IF
              IF LO-AMT-LEN (M1) > +0
                 MOVE LO-AMT (M1)      TO PD-LOW-AMT (X1)
              END-IF
              IF HI-AMT-LEN (M1) > +0
                 MOVE HI-AMT (M1)      TO PD-HI-AMT (X1)
              END-IF
              IF FACT0-LEN (M1) > +0
                 MOVE FACT0I (M1)      TO PD-UEP-FACTOR (X1 Y1)
              END-IF
              IF FACT1-LEN (M1) > +0
                 MOVE FACT1I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 1)
              END-IF
              IF FACT2-LEN (M1) > +0
                 MOVE FACT2I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 2)
              END-IF
              IF FACT3-LEN (M1) > +0
                 MOVE FACT3I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 3)
              END-IF
              IF FACT4-LEN (M1) > +0
                 MOVE FACT4I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 4)
              END-IF
              IF FACT5-LEN (M1) > +0
                 MOVE FACT5I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 5)
              END-IF
              IF FACT6-LEN (M1) > +0
                 MOVE FACT6I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 6)
              END-IF
              IF FACT7-LEN (M1) > +0
                 MOVE FACT7I (M1)      TO PD-UEP-FACTOR (X1 Y1 + 7)
              END-IF
              ADD +1 TO X1
           END-PERFORM

           .
       2000-CONTINUE-CHANGE.                                            

           EXEC CICS REWRITE                                            
               DATASET   (ERPDEF-FILE-ID)                               
               FROM      (PRODUCT-MASTER)                                  
           END-EXEC.                                                    
                                                                        
           MOVE ER-0000                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    

           GO TO 1000-SHOW-PROD-RECORD

           .                                                            
       3000-SCROLL-FWD.

           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF RESP-NORMAL
              MOVE +8                  TO X1 PI-X1
              MOVE PI-Y1               TO Y1
              SET PI-FAR-DOWN          TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       3000-EXIT.
          EXIT.

       3100-SCROLL-BWD.

           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF RESP-NORMAL
              MOVE +1                  TO X1 PI-X1
              MOVE PI-Y1               TO Y1
              SET PI-FAR-UP            TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       3100-EXIT.
          EXIT.

       3200-SCROLL-RT.

           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF RESP-NORMAL
              MOVE +8                  TO Y1 PI-Y1
              MOVE PI-X1               TO X1
              SET PI-FAR-RIGHT         TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       3200-EXIT.
          EXIT.

       3300-SCROLL-LT.

           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF RESP-NORMAL
              MOVE +1                  TO Y1 PI-Y1
              MOVE PI-X1               TO X1
              SET PI-FAR-LEFT          TO TRUE
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              DISPLAY ' BAD READ ' WS-RESPONSE ' ' ERPDEF-KEY
              MOVE ER-0145             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       3300-EXIT.
          EXIT.

       5000-FIND-NEXT-PROD-RECORD.                                      
                                                                        
           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS STARTBR                                            
               DATASET   (ERPDEF-FILE-ID)                               
               RIDFLD    (ERPDEF-KEY)                                   
               GTEQ                                                     
               RESP      (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE -1                  TO PFKEYL
              MOVE ER-0130             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       5000-READNEXT-LOOP.                                              

           EXEC CICS READNEXT                                           
               DATASET   (ERPDEF-FILE-ID)                               
               SET       (ADDRESS OF PRODUCT-MASTER)                    
               RIDFLD    (ERPDEF-KEY)                                   
               RESP      (WS-RESPONSE)
           END-EXEC
                                                                        
           IF (NOT RESP-NORMAL)
              OR (PD-COMPANY-CD NOT = PI-COMPANY-CD)
              PERFORM 5000-END-BROWSE
              MOVE -1                  TO PFKEYL
              MOVE ER-0130             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              GO TO 1000-SHOW-PROD-RECORD
           END-IF

           IF ERPDEF-KEY = PI-PREV-PROD-KEY
              GO TO 5000-READNEXT-LOOP
           END-IF
                                                                        
           MOVE ERPDEF-KEY             TO PI-PROD-KEY

           PERFORM 5000-END-BROWSE
           MOVE +1                  TO X1 Y1 PI-X1 PI-Y1
           SET PI-FAR-LEFT          TO TRUE
           SET PI-FAR-UP            TO TRUE
           GO TO 7000-BUILD-OUTPUT-MAP
           .
       5000-END-BROWSE.                                                 
                                                                        
           EXEC CICS ENDBR                                              
               DATASET   (ERPDEF-FILE-ID)                               
           END-EXEC

           .
       5100-FIND-PREV-PROD-RECORD.
                                                                        
           MOVE PI-PREV-PROD-KEY       TO ERPDEF-KEY
                                                                        
      *    IF STATEL > +0
      *       MOVE STATEI              TO ERPDEF-STATE
      *    END-IF
      *    IF PRODCDL > +0
      *       MOVE PRODCDI             TO ERPDEF-PROD-CD
      *    END-IF
      *    IF BENTYPL > +0                                 
      *        MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.         
      *    IF BENCODEL > +0                                  
      *        MOVE BENCODEI               TO  ERPDEF-BEN-CODE.         
      *    IF EXPDTL IS GREATER THAN +0                                 
      *        MOVE EXPDTI                 TO  DEEDIT-FIELD             
      *        PERFORM 8600-DEEDIT THRU 8600-EXIT                       
      *        IF WS-NUMVAL-OF-DEEDIT >= 999999               
      *            MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT            
      *        ELSE                                                     
      *          STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:12)
      *             DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
      *          END-STRING
      *           MOVE 'L'                    TO  DC-OPTION-CODE        
      *            PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
      *            IF NO-CONVERSION-ERROR                               
      *                MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT            
      *            ELSE                                                 
      *                MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.           
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               ENDFILE (5100-UNSUCCESSFUL-SEARCH)                       
           END-EXEC.                                                    
                                                                        
           EXEC CICS STARTBR                                            
               DATASET   (ERPDEF-FILE-ID)                               
               RIDFLD    (ERPDEF-KEY)                                   
               GTEQ                                                     
           END-EXEC.                                                    
                                                                        
       5100-READPREV-LOOP.                                              
           EXEC CICS READPREV                                           
               DATASET   (ERPDEF-FILE-ID)                               
               SET       (ADDRESS OF PRODUCT-MASTER)                       
               RIDFLD    (ERPDEF-KEY)                                   
           END-EXEC.                                                    
                                                                        
           IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
               GO TO 5100-UNSUCCESSFUL-SEARCH.                          
                                                                        
           IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
               GO TO 5100-READPREV-LOOP.                                

           MOVE ERPDEF-KEY             TO PI-PROD-KEY

           MOVE +1                  TO X1 Y1 PI-X1 PI-Y1
           SET PI-FAR-LEFT          TO TRUE
           SET PI-FAR-UP            TO TRUE

           GO TO 7000-BUILD-OUTPUT-MAP.                                 
                                                                        
       5100-END-BROWSE.                                                 
           EXEC CICS ENDBR                                              
               DATASET   (ERPDEF-FILE-ID)                               
           END-EXEC.                                                    
                                                                        
       5100-UNSUCCESSFUL-SEARCH.                                        
                                                                        
           PERFORM 5100-END-BROWSE.                                     

           MOVE ER-0131                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 1000-SHOW-PROD-RECORD.                                 

           .                                                            
       6000-EDIT-INPUT-DATA.                                            

           IF PDESCL > +0
              MOVE AL-UANON            TO PDESCA
           END-IF

           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF LO-TERM-LEN (M1) > +0
                 MOVE LO-TERM (M1)     TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO LO-TERM (M1)
                 MOVE AL-UNNON         TO LO-TERM-ATTRB (M1)
              END-IF
              IF HI-TERM-LEN (M1) > +0
                 MOVE HI-TERM (M1)     TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO HI-TERM (M1)
                 MOVE AL-UNNON         TO HI-TERM-ATTRB (M1)
              END-IF
              IF LO-AMT-LEN (M1) > +0
                 MOVE LO-AMT (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO LO-AMT (M1)
                 MOVE AL-UNNON         TO LO-AMT-ATTRB (M1)
              END-IF
              IF HI-AMT-LEN (M1) > +0
                 MOVE HI-AMT (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 MOVE WS-NUMVAL-OF-DEEDIT TO HI-AMT (M1)
                 MOVE AL-UNNON         TO HI-AMT-ATTRB (M1)
              END-IF
              IF FACT0-LEN (M1) > +0
                 MOVE FACT0 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
                 MOVE WS-9V999-OF-DEEDIT TO FACT0I (M1)
                 MOVE AL-UNNON         TO FACT0-ATTRB (M1)
              END-IF
              IF FACT1-LEN (M1) > +0
                 MOVE FACT1 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
                 MOVE WS-9V999-OF-DEEDIT TO FACT1I (M1)
                 MOVE AL-UNNON         TO FACT1-ATTRB (M1)
              END-IF
              IF FACT2-LEN (M1) > +0
                 MOVE FACT2 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
                 MOVE WS-9V999-OF-DEEDIT TO FACT2I (M1)
                 MOVE AL-UNNON         TO FACT2-ATTRB (M1)
              END-IF
              IF FACT3-LEN (M1) > +0
                 MOVE FACT3 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT3I (M1)
                 MOVE AL-UNNON         TO FACT3-ATTRB (M1)
              END-IF
              IF FACT4-LEN (M1) > +0
                 MOVE FACT4 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT4I (M1)
                 MOVE AL-UNNON         TO FACT4-ATTRB (M1)
              END-IF
              IF FACT5-LEN (M1) > +0
                 MOVE FACT5 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT5I (M1)
                 MOVE AL-UNNON         TO FACT5-ATTRB (M1)
              END-IF
              IF FACT6-LEN (M1) > +0
                 MOVE FACT6 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT6I (M1)
                 MOVE AL-UNNON         TO FACT6-ATTRB (M1)
              END-IF
              IF FACT7-LEN (M1) > +0
                 MOVE FACT7 (M1)       TO DEEDIT-FIELD
                 PERFORM 8500-DEEDIT   THRU 8500-EXIT
041515           MOVE WS-9V999-OF-DEEDIT TO FACT7I (M1)
                 MOVE AL-UNNON         TO FACT7-ATTRB (M1)
              END-IF
           END-PERFORM

           .
       6000-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
       7000-BUILD-OUTPUT-MAP.                                           
                                                                        
           MOVE LOW-VALUES             TO  EL159BO
           MOVE PI-COMPANY-CD          TO  PI-PREV-COMPANY-CD
           MOVE PD-STATE               TO  STATEO
                                           PI-PREV-STATE
           MOVE PD-PRODUCT-CD          TO  PRODCDO
                                           PI-PREV-PROD-CD
           MOVE PD-BEN-TYPE            TO  BENTYPO
                                           PI-PREV-BEN-TYPE
           MOVE PD-BEN-CODE            TO  BENCODEO
                                           PI-PREV-BEN-CODE
           IF PD-PROD-EXP-DT = HIGH-VALUES
              MOVE '99/99/9999'        TO EXPDTO
              MOVE HIGH-VALUES         TO PI-PREV-EXP-DT
           ELSE                                                     
              MOVE PD-PROD-EXP-DT      TO DC-BIN-DATE-1            
                                          PI-PREV-EXP-DT           
              MOVE ' '                 TO DC-OPTION-CODE           
              MOVE +0                  TO DC-ELAPSED-DAYS          
                                          DC-ELAPSED-MONTHS        
              PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT            
              IF NO-CONVERSION-ERROR                                   
                 MOVE DC-GREG-DATE-A-EDIT
                                       TO EXPDTO                   
              ELSE                                                     
                 MOVE LOW-VALUES       TO EXPDTO
              END-IF
           END-IF
                                                                        
           MOVE PD-PRODUCT-DESC        TO PDESCO

           MOVE Y1                     TO YR1O
           COMPUTE YR2O = YR1O + 1
           COMPUTE YR3O = YR2O + 1
           COMPUTE YR4O = YR3O + 1
           COMPUTE YR5O = YR4O + 1
           COMPUTE YR6O = YR5O + 1
           COMPUTE YR7O = YR6O + 1
           COMPUTE YR8O = YR7O + 1

           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              MOVE PD-LOW-TERM (X1)      TO LO-TERM (M1)
              MOVE PD-HI-TERM (X1)       TO HI-TERM (M1)
              MOVE PD-LOW-AMT (X1)       TO LO-AMTO (M1)
              MOVE PD-HI-AMT (X1)        TO HI-AMTO (M1)
              MOVE PD-UEP-FACTOR (X1 Y1)  TO FACT0O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 1)  TO FACT1O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 2)  TO FACT2O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 3)  TO FACT3O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 4)  TO FACT4O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 5)  TO FACT5O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 6)  TO FACT6O (M1)
              MOVE PD-UEP-FACTOR (X1 Y1 + 7)  TO FACT7O (M1)
              ADD +1 TO X1
           END-PERFORM
                                                                        
           MOVE PD-LAST-MAINT-BY           TO  MAINTBYO                 
                                               PI-UPDATE-BY.            
           MOVE PD-LAST-MAINT-HHMMSS       TO  TIME-IN                  
                                               PI-UPDATE-HHMMSS.        
           MOVE TIME-OUT                   TO  MAINTATO.                
           MOVE PD-LAST-MAINT-DT           TO  DC-BIN-DATE-1.           
           MOVE ' '                        TO  DC-OPTION-CODE.          
           MOVE +0                         TO  DC-ELAPSED-DAYS          
                                               DC-ELAPSED-MONTHS.       
           PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT.               
           IF NO-CONVERSION-ERROR                                       
               MOVE DC-GREG-DATE-1-EDIT    TO  MAINTONO                 
           ELSE                                                         
               MOVE LOW-VALUES             TO  MAINTONO.                
                                                                        
                                                                        
           MOVE -1                         TO  MAINTL.                  
           MOVE AL-UANOF                   TO  STATEA
                                               PRODCDA
                                               BENTYPA
                                               BENCODEA
                                               EXPDTA
           .
       8100-SEND-INITIAL-MAP.                                           
                                                                        
           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.                
           MOVE EIBTIME                    TO  TIME-IN.                 
           MOVE SAVE-DATE                  TO  DATEO.                   
           MOVE TIME-OUT                   TO  TIMEO.                   
                                                                        
           EXEC CICS SEND                                               
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL159BO)                                       
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           MOVE '159B'                 TO PI-CURRENT-SCREEN-NO

           GO TO 9100-RETURN-TRAN

           .
       8200-SEND-DATAONLY.                                              

           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.                
           MOVE EIBTIME                    TO  TIME-IN.                 
           MOVE SAVE-DATE                  TO  DATEO.                   
           MOVE TIME-OUT                   TO  TIMEO.                   
                                                                        
           EXEC CICS SEND                                               
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL159BO)                                       
               DATAONLY                                                 
               CURSOR                                                   
           END-EXEC.                                                    

           GO TO 9100-RETURN-TRAN

           .
       8300-SEND-TEXT.                                                  
                                                                        
           EXEC CICS SEND TEXT                                          
               FROM  (LOGOFF-TEXT)                                      
               LENGTH(LOGOFF-LENGTH)                                    
               ERASE                                                    
               FREEKB                                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS RETURN                                             
               END-EXEC.                                                

           .
       8500-DEEDIT.                                                     

           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-9V999-OF-DEEDIT

           .
       8500-EXIT.                                                       
           EXIT.                                                        

       8600-DEEDIT.                                                     

           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-NUMVAL-OF-DEEDIT

           .
       8600-EXIT.                                                       
           EXIT.                                                        

       8800-UNAUTHORIZED-ACCESS.                                        
           MOVE UNACCESS-MSG               TO  LOGOFF-MSG.              
           GO TO 8300-SEND-TEXT.                                        
                                                                        
       8810-PF23.                                                       
           MOVE EIBAID                     TO  PI-ENTRY-CD-1.           
           MOVE XCTL-005                   TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       8850-DUPREC.                                                     
           MOVE ER-0132                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  BENTYPL                  
           MOVE AL-UABON                   TO  BENTYPA BENCODEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       8870-NOTOPEN.                                                    
                                                                        
           MOVE LOW-VALUES                 TO  EL159BO.                 
           MOVE -1                         TO  MAINTL.                  
           MOVE ER-0701                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
       8880-NOT-FOUND.                                                  

           MOVE ER-0702                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  BENTYPL                  
           MOVE AL-UABON                   TO  BENTYPA BENCODEA EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.                                 

           .
       9100-RETURN-TRAN.                                                
           MOVE EMI-ERROR-NUMBER (1)       TO  PI-LAST-ERROR-NO.        
                                                                        
           EXEC CICS RETURN                                             
               TRANSID    (TRANS-ID)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (PI-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9200-RETURN-MAIN-MENU.                                           
                                                                        
           MOVE XCTL-626                   TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       9300-XCTL.                                                       
           EXEC CICS XCTL                                               
               PROGRAM    (PGM-NAME)                                    
               COMMAREA   (PROGRAM-INTERFACE-BLOCK)                     
               LENGTH     (PI-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9400-CLEAR.                                                      
           MOVE PI-RETURN-TO-PROGRAM       TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       9500-PF12.                                                       
           MOVE XCTL-010                   TO  PGM-NAME.                
           GO TO 9300-XCTL.                                             
                                                                        
       9600-PGMID-ERROR.                                                
           EXEC CICS HANDLE CONDITION                                   
               PGMIDERR   (8300-SEND-TEXT)                              
           END-EXEC.                                                    
                                                                        
           MOVE PGM-NAME                   TO  PI-CALLING-PROGRAM.      
           MOVE ' '                        TO  PI-ENTRY-CD-1.           
           MOVE XCTL-005                   TO  PGM-NAME.                
           MOVE PGM-NAME                   TO  LOGOFF-PGM.              
           MOVE PGMIDERR-MSG               TO  LOGOFF-FILL.             
           GO TO 9300-XCTL.                                             
                                                                        
           EJECT                                                        
       9700-LINK-DATE-CONVERT.                                          
           EXEC CICS LINK                                               
               PROGRAM    ('ELDATCV')                                   
               COMMAREA   (DATE-CONVERSION-DATA)                        
               LENGTH     (DC-COMM-LENGTH)                              
           END-EXEC.                                                    
                                                                        
       9700-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9900-ERROR-FORMAT.                                               
           IF NOT EMI-ERRORS-COMPLETE                                   
               MOVE LINK-001               TO  PGM-NAME                 
               EXEC CICS LINK                                           
                   PROGRAM    (PGM-NAME)                                
                   COMMAREA   (ERROR-MESSAGE-INTERFACE-BLOCK)           
                   LENGTH     (EMI-COMM-LENGTH)                         
               END-EXEC.                                                
                                                                        
       9900-EXIT.                                                       
           EXIT.                                                        
                                                                        
       9990-ABEND.                                                      
           MOVE LINK-004                   TO  PGM-NAME.                
           MOVE DFHEIBLK                   TO  EMI-LINE1.               
           EXEC CICS LINK                                               
               PROGRAM   (PGM-NAME)                                     
               COMMAREA  (EMI-LINE1)                                    
               LENGTH    (72)                                           
           END-EXEC.                                                    
                                                                        
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
       9995-SECURITY-VIOLATION.
                                   COPY ELCSCTP.                        

       9995-EXIT.                                                       
           EXIT.                                                        
