       ID DIVISION.                                                     
                                                                        
       PROGRAM-ID.                 EL159.                               
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
      *    FOR PRODUCT DEFINITIONS.
                                                                        
      *    SCREENS     - EL159A - PRODUCT DEFINITION                    
                                                                        
      *    ENTERED BY  - EL601 - MAINTENANCE MENU                       
                                                                        
      *    EXIT TO     - EL601 - MAINTENANCE MENU                       
                                                                        
      *    INPUT FILE  - ERPDEF -              - PRODUCT DEFINITION     
                                                                        
      *    OUTPUT FILE - ERPDEF -              - PRODUCT DEFINITION     
                                                                        
      *    COMMAREA    - PASSED                                         
                                                                        
      *    NARRATIVE   - FIRST ENTRY IS VIA AN XCTL FROM EL101.  ON     
      *                  FIRST ENTRY, A SKELETON SCREEN IS SENT AND THE 
      *                  PROGRAM EXITS TO WAIT FOR INPUT.  ON SUCCESSIVE
      *                  ENTRIES (XCTL FROM CICS VIA E031) THE SCREEN   
      *                  WILL BE READ AND ACTION WILL BE BASED ON THE   
      *                  MAINTENANCE TYPE INDICATED.                    
                                                                        
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
051414* 051414  CR2013021100002  PEMA  RECURRENT CLAIM CHANGES
052614* 052614    2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
100314* 100314  CR2014061900001  PEMA  ADD BENEFIT PERCENT
110618* 110618  CR2018100400001  TANA  ADD OTHER CLAIM TYPE
080322* 080322  CR2021100800003  TANA  Add B and H claim types
      ******************************************************************
       ENVIRONMENT DIVISION.                                            
       DATA DIVISION.                                                   
       WORKING-STORAGE SECTION.                                         
       77  FILLER  PIC X(32)  VALUE '********************************'. 
       77  FILLER  PIC X(32)  VALUE '*    EL159 WORKING STORAGE     *'. 
       77  FILLER  PIC X(32)  VALUE '*********** VMOD=2.001 *********'. 
                                                                        
       77  FIRST-READ-PREV-SW              PIC X(01)   VALUE SPACES.    
           88  FIRST-READ-PREV                         VALUE 'Y'.       
       77  M1                              PIC S999 COMP-3 VALUE +0.
080322 77  R1                              PIC S999 COMP-3 VALUE +0.
080322 77  PAGE-NBR                        PIC S999 COMP-3 VALUE +0.
080322 01  WS-LINE-NBR-CHAR.
080322     12  WS-LINE-NBR.
080322         16  WS-LINE-NBR-1           PIC 9.
080322         16  WS-LINE-NBR-P           PIC X(02).
080322     12  WS-LINE-NBR-R REDEFINES WS-LINE-NBR.
080322         16  WS-LINE-NBR-2           PIC 9(02).
080322         16  WS-LINE-NBR-P2          PIC X(01).

       01  ACCESS-KEYS.                                                 
           12  ERPDEF-KEY.                                              
               16  ERPDEF-COMPANY-CD       PIC X.
               16  ERPDEF-STATE            PIC XX.
               16  ERPDEF-PROD-CD          PIC XXX.
               16  FILLER                  PIC X(7).
               16  ERPDEF-BEN-TYPE         PIC X.
               16  ERPDEF-BEN-CODE         PIC XX.
               16  ERPDEF-EXP-DT           PIC XX.
                                                                        
           12  ELCNTL-KEY.                                              
               16  ELCNTL-COMPANY-ID       PIC X(03).                   
               16  ELCNTL-RECORD-TYPE      PIC X(01).                   
               16  ELCNTL-ACCESS           PIC X(04).                   
               16  ELCNTL-STATE-ACCESS REDEFINES ELCNTL-ACCESS.         
                   20  ELCNTL-STATE-CD     PIC  X(02).                  
                   20  FILLER              PIC  X(02).                  
               16  ELCNTL-BENEFIT-ACCESS REDEFINES ELCNTL-ACCESS.       
                   20  FILLER              PIC  X(02).                  
                   20  ELCNTL-BENE-CD      PIC  X(02).                  
               16  ELCNTL-SEQUENCE-NO      PIC S9(04)      COMP.        
                                                                        
       01  WS-DATE-AREA.                                                
           05  SAVE-DATE                   PIC X(08)   VALUE SPACES.    
           05  SAVE-BIN-DATE               PIC X(02)   VALUE SPACES.    
                                                                        
       01  MISC-WORK-AREAS.                                             
                                                                        
           12  WS-RESPONSE             PIC S9(8)   COMP.
               88  RESP-NORMAL              VALUE +00.
               88  RESP-ERROR               VALUE +01.
               88  RESP-TERMIDERR           VALUE +11.
               88  RESP-NOTFND              VALUE +13.
               88  RESP-DUPREC              VALUE +14.
               88  RESP-DUPKEY              VALUE +15.
               88  RESP-INVREQ              VALUE +16.
               88  RESP-NOTOPEN             VALUE +19.
               88  RESP-ENDFILE             VALUE +20.
               88  RESP-ILLOGIC             VALUE +21.
               88  RESP-LENGERR             VALUE +22.
           12  WS-NUMVAL.
               16  WS-NUMVAL-OF-DEEDIT     PIC 9(11) VALUE ZEROS.
               16  WS-999V99-OF-DEEDIT REDEFINES
                   WS-NUMVAL-OF-DEEDIT     PIC 9(9)V99.
100314         16  WS-9V999-OF-DEEDIT REDEFINES
100314             WS-NUMVAL-OF-DEEDIT     PIC 9(8)V999.
           12  DEEDIT-FIELD                PIC X(11).                   
           12  DEEDIT-FIELD-V0 REDEFINES DEEDIT-FIELD                   
                                           PIC S9(11).                  
           12  DEEDIT-FIELD-V2 REDEFINES DEEDIT-FIELD                   
                                           PIC S9(09)V99.               
                                                                        
           12  WS-BENEFIT-FIELD.                                        
               16  WS-BENE-TYPE            PIC X(01).                   
               16  WS-BENE-CODE            PIC X(02).                   
                                                                        
           12  WS-EXP-DT                   PIC X(02)   VALUE LOW-VALUES.
                                                                        
       01  STANDARD-AREAS.                                              
           12  SC-ITEM                     PIC S9(4)   VALUE +1  COMP.  
           12  TRANS-ID                    PIC X(04)   VALUE 'E031'.
           12  EL150-TRANS-ID              PIC X(04)   VALUE 'EX23'.    
           12  EL1591-TRANS-ID             PIC X(04)   VALUE 'E032'.
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
           12  THIS-PGM                    PIC X(08)   VALUE 'EL159'.   
           12  ERPDEF-FILE-ID              PIC X(08)   VALUE 'ERPDEF'.  
           12  ELCNTL-FILE-ID              PIC X(08)   VALUE 'ELCNTL'.  
           12  ERPDEF-LENGTH               PIC S9(04)  VALUE +1319 COMP.
           12  SUB                         PIC 9(02).                   
           12  SUB-1                       PIC 9(02).                   
           12  SUB2                        PIC 9(02).                   
           12  GETMAIN-SPACE               PIC X(01)   VALUE SPACE.     
           12  MAPSET-NAME                 PIC X(08)   VALUE 'EL159S'.
           12  WS-MAP-NAME                 PIC X(08)   VALUE 'EL159A'.  
           12  WS-PF-KEY                   PIC 9(02)   VALUE ZEROS.     
                                                                        
           12  WS-CNTL-REC-FOUND-SW        PIC X(01)   VALUE 'N'.       
               88  CNTL-RECORD-FOUND                   VALUE 'Y'.       
                                                                        
           12  WS-PRE-EXIST-CODES          PIC X(02)   VALUE ZEROS.     
               88  VALID-PRE-EXIST-CODE                VALUES ARE '00'  
                                                             THRU '99'. 

           12  WS-DISABILITY-CODES         PIC X(02)   VALUE ZEROS.     
               88  VALID-DISABILITY-CODE               VALUES ARE '00'  
                                                             THRU '99'. 
                                                                        
           12  WS-REFUND-METHOD            PIC X       VALUE ZEROS.     
               88  VALID-REFUND-METHOD                 VALUES ARE ' ',  
                                                         '1' THRU '9'.  
                                                                        
           12  WS-BENEFIT-SW               PIC X       VALUE SPACE.     
               88  BENEFIT-FOUND                       VALUE 'Y'.       
               88  BENEFIT-NOT-FOUND                   VALUE 'N'.       
                                                                        
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
           12  ER-0067                     PIC X(04)   VALUE '0067'.
           12  ER-0068                     PIC X(04)   VALUE '0068'.
           12  ER-0070                     PIC X(04)   VALUE '0070'.    
           12  ER-0073                     PIC X(04)   VALUE '0073'.
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
080322     12  ER-1164                     PIC X(04)   VALUE '1164'.
           12  ER-2241                     PIC X(04)   VALUE '2241'.    
           12  ER-2276                     PIC X(04)   VALUE '2276'.    
           12  ER-7008                     PIC X(04)   VALUE '7008'.    
           12  ER-7031                     PIC X(04)   VALUE '7031'.    
           12  ER-7123                     PIC X(04)   VALUE '7123'.    
100314     12  ER-7132                     PIC X(04)   VALUE '7132'.
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
                   20  PI-PREV-COMPANY-CD  PIC X(01).
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
               16  FILLER                  PIC X(589).

                                           COPY ELCAID.                 
       01  FILLER    REDEFINES DFHAID.                                  
           12  FILLER                      PIC X(08).                   
           12  PF-VALUES                   PIC X         OCCURS 24.     

                                           COPY EL159S.                 
       01  EL159AI-R REDEFINES EL159AI.                                 
           12  FILLER                      PIC X(177).
           12  AFA-LEN                     PIC S9(04)  COMP.
           12  AFA-ATTRB                   PIC X.
           12  AFA-AMT                     PIC X(6).
           12  AFA-AMT-IN REDEFINES AFA-AMT PIC 9(4)V99.
           12  AFA-AMT-OUT REDEFINES AFA-AMT
                                           PIC ZZ9.99.
           12  F                           PIC X(4).
           12  EL159-PROD-TABLE OCCURS 8.
               16  LINE-NBR-LEN            PIC S9(04)  COMP.
               16  LINE-NBR-ATTRB          PIC X(01).
               16  LINE-NBR                PIC X(03).
               16  PROD-CODE-LEN           PIC S9(04)  COMP.            
               16  PROD-CODE-ATTRB         PIC X(01).                   
               16  PROD-CODE               PIC X.
               16  MAX-ATT-AGE-LEN         PIC S9(04)  COMP.            
               16  MAX-ATT-AGE-ATTRB       PIC X(01).                   
               16  MAX-ATT-AGE             PIC 99.                      
080322*        16  MIN-AGE-LEN             PIC S9(04)  COMP.
080322*        16  MIN-AGE-ATTRB           PIC X(01).
080322*        16  MIN-AGE                 PIC 99.
               16  WAIT-PR-LEN             PIC S9(04)  COMP.
               16  WAIT-PR-ATTRB           PIC X(01).
               16  WAIT-PR                 PIC X(03).
               16  MAX-TERM-LEN            PIC S9(04)  COMP.            
               16  MAX-TERM-ATTRB          PIC X(01).                   
               16  MAX-TERM                PIC 999.
100314         16  ben-pct-len             pic s9(04)  comp.
100314         16  ben-pct-attrb           pic x.
100314         16  ben-pct-in              pic v9(4).
100314         16  ben-pct-out redefines
100314             ben-pct-in              pic .999.
               16  MAX-AMT-LEN             PIC S9(04)  COMP.            
               16  MAX-AMT-ATTRB           PIC X(01).                   
               16  MAX-AMT-IN              PIC 9(9).
               16  MAX-AMT-OUT REDEFINES
                   MAX-AMT-IN              PIC Z,ZZZ,999.
               16  PRE-EXIST-LEN           PIC S9(04)  COMP.            
               16  PRE-EXIST-ATTRB         PIC X(01).                   
               16  PRE-EXIST               PIC 99.
               16  EXCL-PER-LEN            PIC S9(04)  COMP.            
               16  EXCL-PER-ATTRB          PIC X(01).                   
               16  EXCL-PERIOD             PIC 999.
               16  COV-ENDS-LEN            PIC S9(04)  COMP.            
               16  COV-ENDS-ATTRB          PIC X(01).                   
               16  COV-ENDS                PIC 999.
               16  ACC-PER-ENDS-LEN        PIC S9(04)  COMP.            
               16  ACC-PER-ENDS-ATTRB      PIC X(01).                   
               16  ACC-PER-ENDS            PIC 9999.
               16  ACC-PER-ENDS-OUT REDEFINES ACC-PER-ENDS
                                           PIC ZZ99.
               16  CRIT-PER-LEN            PIC S9(04)  COMP.            
               16  CRIT-PER-ATTRB          PIC X(01).                   
               16  CRIT-PER                PIC 999.
               16  CRIT-PER-OUT REDEFINES CRIT-PER
                                           PIC Z99.
               16  REC-CP-LEN              PIC S9(04)  COMP.
               16  REC-CP-ATTRB            PIC X(01).
               16  REC-CRIT-PER            PIC XX.
               16  REC-CRIT-PER-N REDEFINES REC-CRIT-PER
                                           PIC 99.
               16  RTW-MOS-LEN             PIC S9(04)  COMP.
               16  RTW-MOS-ATTRB           PIC X(01).
               16  RTW-MOS                 PIC 999.
               16  RTW-MOS-OUT REDEFINES RTW-MOS
                                           PIC Z99.
               16  MAX-EXT-LEN             PIC S9(04)  COMP.
               16  MAX-EXT-ATTRB           PIC X(01).
               16  MAX-EXT                 PIC 999.
               16  MAX-EXT-OUT REDEFINES MAX-EXT
                                           PIC Z99.

       LINKAGE SECTION.                                                 
                                                                        
       01  DFHCOMMAREA                     PIC X(1024).                 

                                           COPY ERCPDEF.

                                           COPY ELCCNTL.                

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
               NOTOPEN    (8870-NOTOPEN)                                
               NOTFND     (8880-NOT-FOUND)                              
               PGMIDERR   (9600-PGMID-ERROR)                            
               ERROR      (9990-ABEND)                                  
           END-EXEC.                                                    

           IF PI-CALLING-PROGRAM = 'EL150'
              MOVE SPACES                 TO ERPDEF-KEY
              MOVE PI-COMPANY-CD          TO ERPDEF-COMPANY-CD
              MOVE PI-FORM-NUMBER (1:2)   TO ERPDEF-STATE
              MOVE PI-FORM-NUMBER (3:3)   TO ERPDEF-PROD-CD
              MOVE PI-FORM-NUMBER (6:1)   TO ERPDEF-BEN-TYPE
              MOVE PI-FORM-NUMBER (7:2)   TO ERPDEF-BEN-CODE           
              MOVE PI-CERT-EFF-DT         TO ERPDEF-EXP-DT
              
              EXEC CICS READ                                               
                  DATASET    (ERPDEF-FILE-ID)                              
                  SET        (ADDRESS OF PRODUCT-MASTER)                   
                  RIDFLD     (ERPDEF-KEY)                                  
                  GTEQ
                  RESP       (WS-RESPONSE)
              END-EXEC
                                                                           
              IF RESP-NORMAL
                 AND PI-COMPANY-CD   =  PD-COMPANY-CD
                 AND PD-STATE        =  PI-FORM-NUMBER (1:2)
                 AND PD-PRODUCT-CD   =  PI-FORM-NUMBER (3:3)
                 AND PD-BEN-TYPE     =  PI-FORM-NUMBER (6:1)
                 AND PD-BEN-CODE     =  PI-FORM-NUMBER (7:2)
                 GO TO 0150-SET-PROGRAM-SAVES
              ELSE
                 MOVE 'EL159'          TO PGM-NAME
                 GO TO 9300-XCTL
              END-IF
           END-IF

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
                                                                        
           IF EIBTRNID = EL150-TRANS-ID                                 
              MOVE 'S'                     TO MAINTI                    
              MOVE +1                      TO MAINTL                    
              MOVE DFHENTER                TO EIBAID                    
              MOVE PI-FORM-NUMBER (1:2)    TO STATEI
              MOVE +2                      TO STATEL
              MOVE PI-FORM-NUMBER (3:3)    TO PRODCDI
              MOVE +3                      TO PRODCDL
              MOVE PI-FORM-NUMBER (6:1)    TO BENTYPI
              MOVE +1                      TO BENTYPL
              MOVE PI-FORM-NUMBER (7:2)    TO BENCODEI
              MOVE +2                      TO BENCODEL
              MOVE PD-PROD-EXP-DT          TO DC-BIN-DATE-1             
              MOVE ' '                     TO DC-OPTION-CODE            
              MOVE +0                      TO DC-ELAPSED-DAYS           
                                              DC-ELAPSED-MONTHS         
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT             
              IF NO-CONVERSION-ERROR                                    
                 MOVE DC-GREG-DATE-A-EDIT  TO EXPDTO                    
                 GO TO 1000-SHOW-PROD-RECORD                            
              ELSE                                                      
                 MOVE LOW-VALUES           TO EXPDTO                    
                 GO TO 1000-SHOW-PROD-RECORD.                           
                                                                        
           IF EIBTRNID = EL1591-TRANS-ID
              MOVE PI-PROD-KEY         TO ERPDEF-KEY
              GO TO 1000-CONTINUE-SHOW
           END-IF

           MOVE SPACES                     TO  PI-PROGRAM-WORK-AREA.    
           MOVE LOW-VALUES                 TO  EL159AO.                 
           MOVE -1                         TO  MAINTL.                  
           GO TO 8100-SEND-INITIAL-MAP

           .
       0200-RECEIVE.

           IF EIBAID = DFHCLEAR
              GO TO 9400-CLEAR.
                                                                        
           IF EIBAID EQUAL DFHPA1 OR DFHPA2 OR DFHPA3                   
               MOVE LOW-VALUES             TO  EL159AI                  
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
               INTO     (EL159AI)                                       
           END-EXEC.                                                    
                                                                        
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
                                                                        
           IF EIBAID EQUAL DFHPF1                                       
               GO TO 5000-FIND-NEXT-PROD-RECORD.                        
                                                                        
           IF EIBAID EQUAL DFHPF2                                       
               GO TO 5100-FIND-PREV-PROD-RECORD.                        

           IF EIBAID = DFHPF3
              MOVE 'EL1591'            TO PGM-NAME
              GO TO 9300-XCTL
           END-IF

080322     IF EIBAID = DFHPF4
080322        GO TO 5200-PAGE-FORWARD.
080322
080322     IF EIBAID = DFHPF5
080322        GO TO 5300-PAGE-BACKWARD.

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
080322     MOVE LINE-NBR (1) TO WS-LINE-NBR-CHAR.
080322     IF WS-LINE-NBR-1 = 1
080322        MOVE 1 TO PAGE-NBR
080322     ELSE
080322        MOVE 2 TO PAGE-NBR
080322     END-IF

           IF NOT DISPLAY-CAP                                           
               MOVE 'READ'                 TO  SM-READ                  
               PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT           
               MOVE ER-0070                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               MOVE -1                     TO  MAINTL                   
               GO TO 8100-SEND-INITIAL-MAP.                             
                                                                        
           IF (STATEL > +0 AND
               PRODCDL > +0 AND
               BENTYPL > +0 AND                               
               BENCODEL > +0 AND                               
               EXPDTL > +0)                                  
               NEXT SENTENCE                                            
           ELSE                                                         
               IF (MAINTI    EQUAL   'S'   AND                          
                   EXPDTL    EQUAL   +0)                                
                   GO TO 1000-SHOW-PROD-RECORD                          
               ELSE                                                     
                   MOVE ER-0754        TO EMI-ERROR
                   PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                   MOVE -1             TO STATEL
                   MOVE AL-UABON       TO BENTYPA
                                          PRODCDA
                                          STATEA
                                          BENCODEA
                                          EXPDTA
                   GO TO 8200-SEND-DATAONLY.                            
                                                                        
                                                                        
           IF MAINTI EQUAL 'S'                                          
               GO TO 1000-SHOW-PROD-RECORD.                             
                                                                        
           IF MAINTI = 'A' OR 'C' OR 'D'OR 'K'
              IF NOT MODIFY-CAP                                         
                  PERFORM 9995-SECURITY-VIOLATION THRU 9995-EXIT        
                  MOVE ER-0070             TO  EMI-ERROR                
                  PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT              
                  MOVE LOW-VALUES          TO  EL159AO                  
                  MOVE -1                  TO  MAINTL                   
                  GO TO 8100-SEND-INITIAL-MAP.                          
                                                                        
           IF MAINTI EQUAL 'C'
               GO TO 2000-CHANGE-PROD-RECORD.
                                                                        
           IF MAINTI EQUAL 'A'
               GO TO 3000-ADD-PROD-RECORD.
                                                                        
           IF MAINTI EQUAL 'D'
               GO TO 4000-DELETE-PROD-RECORD.

           IF MAINTI EQUAL 'K'
               GO TO 2500-COPY-PROD-RECORD.

           MOVE ER-0023                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE -1                         TO  MAINTL.                  
           MOVE AL-UABON                   TO  MAINTA.                  
           GO TO 8200-SEND-DATAONLY.                                    

       1000-SHOW-PROD-RECORD.

           MOVE SPACES                     TO  ERPDEF-KEY
           MOVE PI-COMPANY-CD              TO  ERPDEF-COMPANY-CD
           MOVE STATEI                     TO  ERPDEF-STATE
           MOVE PRODCDI                    TO  ERPDEF-PROD-CD
           MOVE BENTYPI                    TO  ERPDEF-BEN-TYPE          
           MOVE BENCODEI                   TO  ERPDEF-BEN-CODE
                                                                        
           IF MAINTI = 'S'
              IF EXPDTL = +0
                 MOVE  HIGH-VALUES     TO ERPDEF-EXP-DT            
                 GO TO 1000-CONTINUE-SHOW
              END-IF
           ELSE
              MOVE PI-PREV-PROD-KEY    TO ERPDEF-KEY
              GO TO 1000-CONTINUE-SHOW
           END-IF
                                                                        
           MOVE EXPDTI                 TO  DEEDIT-FIELD
           PERFORM 8600-DEEDIT         THRU 8600-EXIT
           IF WS-NUMVAL-OF-DEEDIT >= 999999
              MOVE HIGH-VALUES         TO ERPDEF-EXP-DT            
           ELSE
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
              MOVE 'L'                 TO DC-OPTION-CODE           
              MOVE +0                  TO DC-ELAPSED-DAYS          
                                          DC-ELAPSED-MONTHS        
              PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT            
              IF NO-CONVERSION-ERROR                                   
                 MOVE DC-BIN-DATE-1    TO ERPDEF-EXP-DT            
              ELSE                                                     
                 MOVE LOW-VALUES       TO ERPDEF-EXP-DT
              END-IF
           END-IF
080322     MOVE 1 TO R1

           .
       1000-CONTINUE-SHOW.
080322     IF PAGE-NBR = 1
080322        MOVE 1 TO R1
080322     ELSE
080322        MOVE 9 TO R1
080322     END-IF


           MOVE ERPDEF-KEY             TO PI-PROD-KEY
                                          PI-PREV-PROD-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF RESP-NORMAL
              GO TO 7000-BUILD-OUTPUT-MAP
           ELSE
              MOVE ER-0073             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              MOVE AL-UABON            TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA

              GO TO 8200-SEND-DATAONLY
           END-IF

           .
       2000-CHANGE-PROD-RECORD.                                         

           IF STATEL > +0
              MOVE STATEI              TO PI-PK-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO PI-PK-PROD-CD
           END-IF
           IF BENTYPL > +0
              MOVE BENTYPI             TO PI-BEN-TYPE
           END-IF
           IF BENCODEL > +0
              MOVE BENCODEI            TO PI-BEN-CODE
           END-IF

           IF EXPDTL > +0
              MOVE EXPDTI              TO DEEDIT-FIELD
              PERFORM 8600-DEEDIT      THRU 8600-EXIT
              IF WS-NUMVAL-OF-DEEDIT >= 999999
                 MOVE HIGH-VALUES      TO WS-EXP-DT
                                          PI-EXP-DT
              ELSE                                                         
                 STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                    DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                 END-STRING
                 MOVE 'L'              TO DC-OPTION-CODE
                 MOVE +0               TO DC-ELAPSED-DAYS
                                          DC-ELAPSED-MONTHS
                 PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
                 MOVE DC-BIN-DATE-1    TO WS-EXP-DT
                                          PI-EXP-DT
              END-IF
           END-IF

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
               RESP       (WS-RESPONSE)
           END-EXEC
                                                                        
           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-0073             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              MOVE AL-UABON            TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA

              GO TO 8200-SEND-DATAONLY
           END-IF

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
           IF AFA-LEN > +0
              MOVE AFA-AMT-IN          TO PD-1ST-YR-ADMIN-ALLOW
           END-IF

080322     IF PAGE-NBR = 1
080322        MOVE 1 TO R1
080322     ELSE
080322        MOVE 9 TO R1
080322     END-IF

           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF PROD-CODE-LEN (M1) > +0
080322           MOVE PROD-CODE (M1)   TO PD-PROD-CODE (R1)
              END-IF
              IF MAX-ATT-AGE-LEN (M1) > +0
080322           MOVE MAX-ATT-AGE (M1) TO PD-MAX-ATT-AGE (R1)
              END-IF
080322*       IF MIN-AGE-LEN (M1) > +0
080322*          MOVE MIN-AGE (M1)     TO PD-MIN-ISSUE-AGE (R1)
080322*       END-IF
080322        IF WAIT-PR-LEN (M1) > +0
080322           MOVE WAIT-PR (M1)     TO PD-WAIT-PERIOD (R1)
080322        END-IF
              IF MAX-TERM-LEN (M1) > +0
080322           MOVE MAX-TERM (M1)    TO PD-MAX-TERM (R1)
              END-IF
              IF MAX-AMT-LEN (M1) > +0
080322           MOVE MAX-AMT-IN (M1)  TO PD-MAX-AMT (R1)
              END-IF
100314        IF ben-pct-LEN (M1) > +0
100314           MOVE ben-pct-IN (M1)  TO PD-ben-pct (R1)
100314        END-IF
              IF PRE-EXIST-LEN (M1) > +0
080322           MOVE PRE-EXIST (M1)   TO PD-PRE-EXIST-EXCL-TYPE (R1)
              END-IF
              IF EXCL-PER-LEN (M1) > +0
080322           MOVE EXCL-PERIOD (M1) TO PD-EXCLUSION-PERIOD-DAYS (R1)
              END-IF
              IF COV-ENDS-LEN (M1) > +0
080322           MOVE COV-ENDS (M1)    TO PD-COVERAGE-ENDS-MOS (R1)
              END-IF
              IF ACC-PER-ENDS-LEN (M1) > +0
080322           MOVE ACC-PER-ENDS (M1) TO PD-ACCIDENT-ONLY-MOS (R1)
              END-IF
              IF CRIT-PER-LEN (M1) > +0
080322           MOVE CRIT-PER (M1) TO PD-CRIT-PERIOD (R1)
              END-IF
              IF REC-CP-LEN (M1) > +0
                 IF REC-CRIT-PER (M1) NUMERIC
080322              MOVE REC-CRIT-PER-N (M1) TO PD-REC-CRIT-PERIOD (R1)
                 ELSE
080322              MOVE REC-CRIT-PER (M1) TO PD-REC-CP-ALPHA (R1)
                 END-IF
              END-IF
              IF RTW-MOS-LEN (M1) > +0
080322           MOVE RTW-MOS (M1)     TO PD-RTW-MOS (R1)
              END-IF
              IF MAX-EXT-LEN (M1) > +0
080322           MOVE MAX-EXT (M1)     TO PD-MAX-EXTENSION (R1)
              END-IF
080322        ADD 1 TO R1
           END-PERFORM

           IF TRUNCL > ZEROS
              MOVE TRUNCI              TO PD-TRUNCATED
           END-IF

           .
       2000-CONTINUE-CHANGE.                                            

           EXEC CICS REWRITE                                            
               DATASET   (ERPDEF-FILE-ID)
               FROM      (PRODUCT-MASTER)
           END-EXEC.                                                    
                                                                        
           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE PD-CONTROL-PRIMARY     TO ERPDEF-KEY
           GO TO 1000-CONTINUE-SHOW

           .                                                            
       2500-COPY-PROD-RECORD.                                         

           MOVE EXPDTI                 TO  DEEDIT-FIELD.            
           PERFORM 8600-DEEDIT THRU 8600-EXIT.                          
           IF WS-NUMVAL-OF-DEEDIT >= 999999
              MOVE HIGH-VALUES         TO  WS-EXP-DT                
           ELSE                                                         
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
              MOVE 'L'                 TO  DC-OPTION-CODE           
              MOVE +0                  TO  DC-ELAPSED-DAYS          
                                               DC-ELAPSED-MONTHS        
              PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
              MOVE DC-BIN-DATE-1       TO  WS-EXP-DT
           END-IF
                                                                        
      ***********  need to edit the key here  
      **** PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT
                                                                        
           IF NOT EMI-NO-ERRORS                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           MOVE PI-PROD-KEY            TO ERPDEF-KEY

           EXEC CICS READ                                               
               DATASET    (ERPDEF-FILE-ID)                              
               SET        (ADDRESS OF PRODUCT-MASTER)                   
               RIDFLD     (ERPDEF-KEY)                                  
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    

           IF RESP-NORMAL
              CONTINUE
           ELSE
              MOVE ER-0073             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              MOVE AL-UABON            TO BENTYPA
                                          STATEA
                                          PRODCDA
                                          BENCODEA
                                          EXPDTA

              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PI-PROCESSOR-ID        TO PD-LAST-MAINT-BY
           MOVE EIBTIME                TO PD-LAST-MAINT-HHMMSS
           MOVE SAVE-BIN-DATE          TO PD-LAST-MAINT-DT

           IF STATEL > +0
              MOVE STATEI              TO PD-STATE
                                          PI-PK-STATE
           END-IF

           IF PRODCDL > +0
              MOVE PRODCDI             TO PD-PRODUCT-CD
                                          PI-PK-PROD-CD
           END-IF

           IF BENTYPL > +0
              MOVE BENTYPI             TO PD-BEN-TYPE
                                          PI-BEN-TYPE
           END-IF

           IF BENCODEL > +0
              MOVE BENCODEI            TO PD-BEN-CODE
                                          PI-BEN-CODE
           END-IF

           MOVE EXPDTI                 TO DEEDIT-FIELD
           PERFORM 8600-DEEDIT THRU 8600-EXIT
           IF WS-NUMVAL-OF-DEEDIT >= 999999                   
               MOVE HIGH-VALUES            TO  PD-PROD-EXP-DT
                                               PI-EXP-DT
           ELSE                                                         
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
               MOVE 'L'                    TO  DC-OPTION-CODE           
               MOVE +0                     TO  DC-ELAPSED-DAYS          
                                               DC-ELAPSED-MONTHS        
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
               IF NO-CONVERSION-ERROR                                   
                   MOVE DC-BIN-DATE-1      TO  PD-PROD-EXP-DT
                                               PI-EXP-DT
                ELSE                                                    
                   MOVE LOW-VALUES         TO  PD-PROD-EXP-DT           
                                               PI-EXP-DT.               
                                                                        
           EXEC CICS WRITE
               DATASET    (ERPDEF-FILE-ID)
               FROM       (PRODUCT-MASTER)
               RIDFLD     (PD-CONTROL-PRIMARY)
               RESP       (WS-RESPONSE)
           END-EXEC

           IF NOT RESP-NORMAL
              MOVE ER-0132             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           MOVE PD-CONTROL-PRIMARY     TO ERPDEF-KEY
           GO TO 1000-CONTINUE-SHOW

           .                                                            
       3000-ADD-PROD-RECORD.

           PERFORM 6000-EDIT-INPUT-DATA THRU 6000-EXIT.                 
                                                                        
           IF NOT EMI-NO-ERRORS                                         
               GO TO 8200-SEND-DATAONLY.                                
                                                                        
           EXEC CICS GETMAIN                                            
               SET       (ADDRESS OF PRODUCT-MASTER)                    
               LENGTH    (ERPDEF-LENGTH)                                
               INITIMG   (GETMAIN-SPACE)                                
           END-EXEC.                                                    
                                                                        
           INITIALIZE PRODUCT-MASTER
                                                                        
           MOVE 'PD'                       TO  PD-RECORD-ID
           MOVE PI-COMPANY-CD              TO  PD-COMPANY-CD
           MOVE STATEI                     TO  PD-STATE
                                               PI-PK-STATE
           MOVE PRODCDI                    TO  PD-PRODUCT-CD
                                               PI-PK-PROD-CD
           MOVE BENTYPI                    TO  PD-BEN-TYPE
                                               PI-BEN-TYPE
           MOVE BENCODEI                   TO  PD-BEN-CODE
                                               PI-BEN-CODE
                                                                        
           MOVE EXPDTI                     TO  DEEDIT-FIELD.            
           PERFORM 8600-DEEDIT THRU 8600-EXIT.                          
           IF WS-NUMVAL-OF-DEEDIT >= 999999                   
               MOVE HIGH-VALUES            TO  PD-PROD-EXP-DT           
                                               PI-EXP-DT                
           ELSE                                                         
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
               MOVE 'L'                    TO  DC-OPTION-CODE           
               MOVE +0                     TO  DC-ELAPSED-DAYS          
                                               DC-ELAPSED-MONTHS        
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
               IF NO-CONVERSION-ERROR                                   
                   MOVE DC-BIN-DATE-1      TO  PD-PROD-EXP-DT           
                                               PI-EXP-DT
                ELSE                                                    
                   MOVE LOW-VALUES         TO  PD-PROD-EXP-DT           
                                               PI-EXP-DT.               
                                                                        
           IF PDESCL > +0
              MOVE PDESCI              TO PD-PRODUCT-DESC
           END-IF

           IF AFA-LEN > +0
              MOVE AFA-AMT-IN          TO PD-1ST-YR-ADMIN-ALLOW
           END-IF

           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF PROD-CODE-LEN (M1) > +0
                 MOVE PROD-CODE (M1)   TO PD-PROD-CODE (M1)
              END-IF
              IF MAX-ATT-AGE-LEN (M1) > +0
                 MOVE MAX-ATT-AGE (M1) TO PD-MAX-ATT-AGE (M1)
              END-IF
080322*       IF MIN-AGE-LEN (M1) > +0
080322*          MOVE MIN-AGE (M1)     TO PD-MIN-ISSUE-AGE (M1)
080322*       END-IF
080322        IF WAIT-PR-LEN (M1) > +0
080322           MOVE WAIT-PR (M1)     TO PD-WAIT-PERIOD (M1)
080322        END-IF
              IF MAX-TERM-LEN (M1) > +0
                 MOVE MAX-TERM (M1)    TO PD-MAX-TERM (M1)
              END-IF
              IF MAX-AMT-LEN (M1) > +0
                 MOVE MAX-AMT-IN (M1)  TO PD-MAX-AMT (M1)
              END-IF
100314        IF ben-pct-LEN (M1) > +0
100314           MOVE ben-pct-IN (M1)  TO PD-ben-pct (M1)
100314        END-IF
              IF PRE-EXIST-LEN (M1) > +0
                 MOVE PRE-EXIST (M1)   TO PD-PRE-EXIST-EXCL-TYPE (M1)
              END-IF
              IF EXCL-PER-LEN (M1) > +0
                 MOVE EXCL-PERIOD (M1) TO PD-EXCLUSION-PERIOD-DAYS (M1)
              END-IF
              IF COV-ENDS-LEN (M1) > +0
                 MOVE COV-ENDS (M1)    TO PD-COVERAGE-ENDS-MOS (M1)
              END-IF
              IF ACC-PER-ENDS-LEN (M1) > +0
                 MOVE ACC-PER-ENDS (M1) TO PD-ACCIDENT-ONLY-MOS (M1)
              END-IF
              IF CRIT-PER-LEN (M1) > +0
                 MOVE CRIT-PER (M1) TO PD-CRIT-PERIOD (M1)
              END-IF
              IF REC-CP-LEN (M1) > +0
                 IF REC-CRIT-PER (M1) NUMERIC
                    MOVE REC-CRIT-PER-N (M1) TO PD-REC-CRIT-PERIOD (m1)
                 ELSE
                    MOVE REC-CRIT-PER (M1) TO PD-REC-CP-ALPHA (M1)
                 END-IF
              END-IF
              IF RTW-MOS-LEN (M1) > +0
                 MOVE RTW-MOS (M1)     TO PD-RTW-MOS (M1)
              END-IF
              IF MAX-EXT-LEN (M1) > +0
                 MOVE MAX-EXT (M1)     TO PD-MAX-EXTENSION (M1)
              END-IF
           END-PERFORM
                                                                        

           MOVE SAVE-BIN-DATE              TO  PD-LAST-MAINT-DT.        
           MOVE PI-PROCESSOR-ID            TO  PD-LAST-MAINT-BY.        
           MOVE EIBTIME                    TO  PD-LAST-MAINT-HHMMSS.    

           .
       3005-WRITE-ERPDEF-FILE.                                          
                                                                        
           EXEC CICS WRITE                                              
               DATASET    (ERPDEF-FILE-ID)                              
               FROM       (PRODUCT-MASTER)
               RIDFLD     (PD-CONTROL-PRIMARY)
               RESP       (WS-RESPONSE)
           END-EXEC.                                                    
                                                                        
           IF NOT RESP-NORMAL
              MOVE ER-0132             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO MAINTL
              GO TO 8200-SEND-DATAONLY
           END-IF

           MOVE PD-CONTROL-PRIMARY     TO PI-PROD-KEY
                                          PI-PREV-PROD-KEY
                                          ERPDEF-KEY

           MOVE ER-0000                TO EMI-ERROR
           PERFORM 9900-ERROR-FORMAT   THRU 9900-EXIT
           MOVE LOW-VALUES                 TO  EL159AO
           MOVE -1                         TO  MAINTL
           GO TO 1000-CONTINUE-SHOW
           .
       4000-DELETE-PROD-RECORD.                                         
                                                                        
           MOVE SPACES                     TO ERPDEF-KEY
           MOVE PI-COMPANY-CD              TO  ERPDEF-COMPANY-CD.       
           MOVE STATEI                     TO ERPDEF-STATE
           MOVE PRODCDI                    TO ERPDEF-PROD-CD
           MOVE BENTYPI                    TO  ERPDEF-BEN-TYPE
           MOVE BENCODEI                   TO  ERPDEF-BEN-CODE
                                                                        
           MOVE EXPDTI                     TO  DEEDIT-FIELD.            
           PERFORM 8600-DEEDIT THRU 8600-EXIT.                          
           IF WS-NUMVAL-OF-DEEDIT >= 999999                   
               MOVE HIGH-VALUES            TO  ERPDEF-EXP-DT            
           ELSE                                                         
              STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                 DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
              END-STRING
               MOVE 'L'                    TO  DC-OPTION-CODE           
               MOVE +0                     TO  DC-ELAPSED-DAYS          
                                               DC-ELAPSED-MONTHS        
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
               IF NO-CONVERSION-ERROR                                   
                   MOVE DC-BIN-DATE-1      TO  ERPDEF-EXP-DT            
               ELSE                                                     
                   MOVE LOW-VALUES         TO  ERPDEF-EXP-DT.           
                                                                        
           EXEC CICS READ                                               
               DATASET   (ERPDEF-FILE-ID)                               
               SET       (ADDRESS OF PRODUCT-MASTER)                    
               RIDFLD    (ERPDEF-KEY)                                   
               UPDATE                                                   
           END-EXEC.                                                    
                                                                        
           IF PD-LAST-MAINT-BY     NOT EQUAL PI-UPDATE-BY OR            
              PD-LAST-MAINT-HHMMSS NOT EQUAL PI-UPDATE-HHMMSS           
               EXEC CICS UNLOCK                                         
                   DATASET   (ERPDEF-FILE-ID)                           
                   END-EXEC                                             
               MOVE ER-0068                TO  EMI-ERROR                
               PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT                 
               GO TO 1000-SHOW-PROD-RECORD.                             
                                                                        
           EXEC CICS DELETE                                             
               DATASET   (ERPDEF-FILE-ID)                               
               END-EXEC.                                                
                                                                        
           MOVE ER-0000                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           MOVE LOW-VALUES                 TO  EL159AO.                 
           MOVE -1                         TO  MAINTL.                  
           GO TO 8100-SEND-INITIAL-MAP.                                 
                                                                        
           EJECT                                                        
       5000-FIND-NEXT-PROD-RECORD.                                      

           MOVE PI-COMPANY-CD              TO  ERPDEF-KEY

           IF STATEL > +0
              MOVE STATEI              TO ERPDEF-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO ERPDEF-PROD-CD
           END-IF
           IF BENTYPL > +0
               MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.         
           IF BENCODEL > +0
               MOVE BENCODEI               TO  ERPDEF-BEN-CODE.         
           IF EXPDTL IS GREATER THAN +0                                 
               MOVE EXPDTI                 TO  DEEDIT-FIELD             
               PERFORM 8600-DEEDIT THRU 8600-EXIT                       
               IF WS-NUMVAL-OF-DEEDIT >= 999999               
                   MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT            
               ELSE                                                     
                 STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                    DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                 END-STRING
                  MOVE 'L'                    TO  DC-OPTION-CODE        
                   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT            
                   ELSE                                                 
                       MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.           
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               ENDFILE (5000-UNSUCCESSFUL-SEARCH)                       
           END-EXEC.                                                    
                                                                        
           EXEC CICS STARTBR                                            
               DATASET   (ERPDEF-FILE-ID)                               
               RIDFLD    (ERPDEF-KEY)                                   
               GTEQ                                                     
           END-EXEC.                                                    
                                                                        
       5000-READNEXT-LOOP.                                              
           EXEC CICS READNEXT                                           
               DATASET   (ERPDEF-FILE-ID)                               
               SET       (ADDRESS OF PRODUCT-MASTER)                    
               RIDFLD    (ERPDEF-KEY)                                   
           END-EXEC.                                                    
                                                                        
           IF PD-COMPANY-CD  NOT EQUAL PI-COMPANY-CD                    
               GO TO 5000-UNSUCCESSFUL-SEARCH.                          
                                                                        
           IF ERPDEF-KEY IS EQUAL TO PI-PREV-PROD-KEY
              display ' key = prev key '
               GO TO 5000-READNEXT-LOOP.                                
                                                                        
           MOVE ERPDEF-KEY             TO PI-PROD-KEY

           PERFORM 5000-END-BROWSE
080322     MOVE 1 TO R1

           GO TO 7000-BUILD-OUTPUT-MAP

           .                                                                        
       5000-END-BROWSE.                                                 
                                                                        
           EXEC CICS ENDBR                                              
               DATASET   (ERPDEF-FILE-ID)                               
           END-EXEC.                                                    
                                                                        
       5000-UNSUCCESSFUL-SEARCH.                                        
                                                                        
           PERFORM 5000-END-BROWSE
           MOVE -1                         TO  PFKEYL
           MOVE ER-0130                    TO  EMI-ERROR
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
           GO TO 1000-SHOW-PROD-RECORD

           .
       5100-FIND-PREV-PROD-RECORD.                                      
                                                                        
           MOVE PI-PREV-PROD-KEY           TO  ERPDEF-KEY
                                                                        
           IF STATEL > +0
              MOVE STATEI              TO ERPDEF-STATE
           END-IF
           IF PRODCDL > +0
              MOVE PRODCDI             TO ERPDEF-PROD-CD
           END-IF
           IF BENTYPL > +0                                 
               MOVE BENTYPI                TO  ERPDEF-BEN-TYPE.         
           IF BENCODEL > +0                                  
               MOVE BENCODEI               TO  ERPDEF-BEN-CODE.         
           IF EXPDTL IS GREATER THAN +0                                 
               MOVE EXPDTI                 TO  DEEDIT-FIELD             
               PERFORM 8600-DEEDIT THRU 8600-EXIT                       
               IF WS-NUMVAL-OF-DEEDIT >= 999999               
                   MOVE HIGH-VALUES        TO  ERPDEF-EXP-DT            
               ELSE                                                     
                 STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2) WS-NUMVAL (6:2)
                    DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                 END-STRING
                  MOVE 'L'                    TO  DC-OPTION-CODE        
                   PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT        
                   IF NO-CONVERSION-ERROR                               
                       MOVE DC-BIN-DATE-1  TO  ERPDEF-EXP-DT            
                   ELSE                                                 
                       MOVE LOW-VALUES     TO  ERPDEF-EXP-DT.           
                                                                        
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
080322     MOVE 1 TO R1
           GO TO 7000-BUILD-OUTPUT-MAP.                                 
                                                                        
       5100-END-BROWSE.                                                 
           EXEC CICS ENDBR                                              
               DATASET   (ERPDEF-FILE-ID)                               
           END-EXEC.                                                    
                                                                        
       5100-UNSUCCESSFUL-SEARCH.                                        
                                                                        
           PERFORM 5100-END-BROWSE.                                     
           MOVE -1                         TO  PFKEYL.                  
           MOVE ER-0131                    TO  EMI-ERROR.               
           PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT.                    
           GO TO 1000-SHOW-PROD-RECORD.                                 

           .
080322 5200-PAGE-FORWARD.
080322     IF PROD-CODE (8) NOT > SPACES
080322        MOVE ER-1164              TO EMI-ERROR
080322        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080322        MOVE -1                   TO MAINTL
080322        MOVE AL-UABON             TO MAINTA
080322        GO TO 8200-SEND-DATAONLY
080322     END-IF

080322     MOVE 9 TO R1
080322     MOVE PI-PROD-KEY            TO ERPDEF-KEY
080322
080322     GO TO 1000-CONTINUE-SHOW.

080322 5300-PAGE-BACKWARD.
080322     MOVE 1 TO PAGE-NBR
080322
080322     MOVE LINE-NBR (1) TO WS-LINE-NBR-CHAR.
080322     IF WS-LINE-NBR-1 = 1
080322        MOVE ER-0067              TO EMI-ERROR
080322        PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
080322        MOVE -1                   TO MAINTL
080322        MOVE AL-UABON             TO MAINTA
080322        GO TO 8200-SEND-DATAONLY
080322     END-IF

080322     MOVE PI-PROD-KEY            TO ERPDEF-KEY
080322
080322     GO TO 1000-CONTINUE-SHOW.

       6000-EDIT-INPUT-DATA.                                            

           IF MAINTI = 'A'
              AND +0 = BENTYPL OR BENCODEL OR EXPDTL
                    OR STATEL OR PRODCDL
              MOVE ER-0144             TO EMI-ERROR
              PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
              MOVE -1                  TO BENTYPL
              MOVE AL-UABON            TO BENTYPA
              GO TO 6000-EXIT
           END-IF
              
           IF MAINTI = 'A'
              IF STATEL > +0
                 MOVE PI-COMPANY-ID    TO ELCNTL-COMPANY-ID
                 MOVE '3'              TO ELCNTL-RECORD-TYPE
                 MOVE STATEI           TO ELCNTL-ACCESS
                 MOVE +0               TO ELCNTL-SEQUENCE-NO
                 EXEC CICS READ
                    DATASET   (ELCNTL-FILE-ID)
                    RIDFLD    (ELCNTL-KEY)
                    SET       (ADDRESS OF CONTROL-FILE)
                    RESP      (WS-RESPONSE)
                 END-EXEC
                 IF NOT RESP-NORMAL
                    MOVE ER-0144       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT
                    MOVE -1            TO STATEL
                    MOVE AL-UABON      TO STATEA
                 END-IF
              END-IF

              IF BENTYPL > +0
                 IF BENTYPI = PI-LIFE-OVERRIDE-L1 OR PI-AH-OVERRIDE-L1
                    CONTINUE
                 ELSE
                    MOVE ER-0713       TO EMI-ERROR
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
                    MOVE -1            TO BENTYPL
                    MOVE AL-UABON      TO BENTYPA
                 END-IF
              END-IF

              IF BENCODEL > +0
                 MOVE SPACES           TO ELCNTL-KEY               
                 IF BENTYPI = PI-LIFE-OVERRIDE-L1
                    MOVE '4'           TO ELCNTL-RECORD-TYPE
                 ELSE
                    MOVE '5'           TO ELCNTL-RECORD-TYPE
                 END-IF
                 MOVE BENCODEI         TO ELCNTL-BENE-CD
                                          WS-BENE-CODE
                 MOVE PI-COMPANY-ID    TO ELCNTL-COMPANY-ID        
                 MOVE ZEROS            TO ELCNTL-SEQUENCE-NO       
                 PERFORM 7100-READ-BENEFIT THRU 7199-EXIT               
                 IF CNTL-RECORD-FOUND                                   
                    MOVE AL-UANON      TO BENCODEA
                 ELSE                                                   
                    MOVE ER-7123       TO EMI-ERROR                
                    PERFORM 9900-ERROR-FORMAT THRU 9900-EXIT            
                    MOVE -1            TO BENCODEL
                    MOVE AL-UABON      TO BENCODEA
                 END-IF
              END-IF

              IF EXPDTL > +0
                 MOVE EXPDTI           TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT                   
                 IF WS-NUMVAL-OF-DEEDIT >= 999999           
                    MOVE '99/99/9999'  TO EXPDTO                   
                    MOVE AL-UANON      TO EXPDTA                   
                 ELSE                                                 
                    STRING WS-NUMVAL (8:4) WS-NUMVAL (4:2)
                       WS-NUMVAL (6:2)
                       DELIMITED BY SIZE INTO DC-GREG-DATE-CYMD
                    END-STRING
                    MOVE 'L'           TO DC-OPTION-CODE           
                    MOVE +0            TO DC-ELAPSED-DAYS          
                                          DC-ELAPSED-MONTHS        
                    PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT       
                    IF NO-CONVERSION-ERROR                              
                       MOVE DC-GREG-DATE-A-EDIT TO  EXPDTO          
                       MOVE AL-UANON   TO EXPDTA
                    ELSE
                       MOVE ER-0705    TO EMI-ERROR
                       perform 9900-ERROR-FORMAT thru 9900-exit
                       MOVE -1         TO EXPDTL
                       MOVE AL-UABON   TO EXPDTA
                    END-IF
                 END-IF
              END-IF
           END-IF

           IF PDESCL > +0
              MOVE AL-UANON            TO PDESCA
           END-IF

           IF AFA-LEN > +0
              MOVE AFA-AMT             TO DEEDIT-FIELD
              PERFORM 8500-DEEDIT      THRU 8500-EXIT
              MOVE WS-999V99-OF-DEEDIT TO AFA-AMT-IN
              MOVE AL-UNNON            TO AFA-ATTRB
           END-IF

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  This was added so the user can remove an entire occurance ***
      ***  if they space out the prod code then we will zero the     ***
      ***  rest of the stuff out.                                    ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
              IF PROD-CODE-LEN (M1) > +0
                 if prod-code (m1) = spaces
                    move +1            to max-att-age-len  (m1)
080322*                                   min-age-len      (m1)
080322                                    WAIT-PR-len      (m1)
080322                                    max-amt-len      (m1)
100314                                    ben-pct-len      (m1)
080322                                    EXCL-PER-LEN     (M1)
080322                                    cov-ends-len     (m1)
080322                                    acc-per-ends-len (m1)
080322                                    crit-per-len     (m1)
080322                                    rec-cp-len       (m1)
080322                                    rtw-mos-len      (m1)
080322                                    max-ext-len      (m1)
080322              move zeros         to max-att-age  (m1)
080322*                                   min-age      (m1)
080322                                    WAIT-PR      (m1)
080322                                    max-amt-in   (m1)
100314                                    ben-pct-in   (m1)
080322                                    EXCL-PERiod  (M1)
080322                                    cov-ends     (m1)
080322                                    acc-per-ends (m1)
080322                                    crit-per     (m1)
080322                                    rec-crit-per (m1)
080322                                    rtw-mos      (m1)
080322                                    max-ext      (m1)
                 end-if
                                          
                 IF PROD-CODE (M1) = 'P' OR 'L' OR 'A'
110618                            OR 'I' OR 'G' or ' ' or 'F' OR 'O'
080322                            OR 'B' OR 'H'
                    MOVE AL-UANON      TO PROD-CODE-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO PROD-CODE-LEN (M1)
                    MOVE AL-UABON      TO PROD-CODE-ATTRB (M1)
                 END-IF
              END-IF
              IF MAX-ATT-AGE-LEN (M1) > +0
                 MOVE MAX-ATT-AGE (M1)  TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO MAX-ATT-AGE (M1)
                    MOVE AL-UANON      TO MAX-ATT-AGE-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-ATT-AGE-LEN (M1)
                    MOVE AL-UABON      TO MAX-ATT-AGE-ATTRB (M1)
                 END-IF
              END-IF
080322*       IF MIN-AGE-LEN (M1) > +0
080322*          MOVE MIN-AGE (M1)      TO DEEDIT-FIELD
080322*          PERFORM 8600-DEEDIT   THRU 8600-EXIT
080322*          IF WS-NUMVAL-OF-DEEDIT NUMERIC
080322*             MOVE WS-NUMVAL-OF-DEEDIT TO MIN-AGE (M1)
080322*             MOVE AL-UANON      TO MIN-AGE-ATTRB (M1)
080322*          ELSE
080322*             MOVE ER-9999       TO EMI-ERROR
080322*             perform 9900-ERROR-FORMAT thru 9900-exit
080322*             MOVE -1            TO MIN-AGE-LEN (M1)
080322*             MOVE AL-UABON      TO MIN-AGE-ATTRB (M1)
080322*          END-IF
080322*       END-IF
080322*       IF WAIT-PR-LEN (M1) > +0
080322*          MOVE WAIT-PR (M1)      TO DEEDIT-FIELD
080322*          PERFORM 8600-DEEDIT   THRU 8600-EXIT
080322*          IF WS-NUMVAL-OF-DEEDIT NUMERIC
080322*             MOVE WS-NUMVAL-OF-DEEDIT TO WAIT-PR (M1)
080322*             MOVE AL-UANON      TO WAIT-PR-ATTRB (M1)
080322*          ELSE
080322*             MOVE ER-9999       TO EMI-ERROR
080322*             perform 9900-ERROR-FORMAT thru 9900-exit
080322*             MOVE -1            TO WAIT-PR-LEN (M1)
080322*             MOVE AL-UABON      TO WAIT-PR-ATTRB (M1)
080322*          END-IF
080322*       END-IF
              IF MAX-TERM-LEN (M1) > +0
                 MOVE MAX-TERM (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO MAX-TERM (M1)
                    MOVE AL-UANON      TO MAX-TERM-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-TERM-LEN (M1)
                    MOVE AL-UABON      TO MAX-TERM-ATTRB (M1)
                 END-IF
              END-IF
              IF MAX-AMT-LEN (M1) > +0
                 MOVE MAX-AMT-IN (M1)  TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT                   
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT
                                       TO MAX-AMT-IN (M1)
                    MOVE AL-UANON      TO MAX-AMT-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-AMT-LEN (M1)
                    MOVE AL-UABON      TO MAX-AMT-ATTRB (M1)
                 END-IF
              END-IF
100314        IF ben-pct-LEN (M1) > +0
100314           MOVE ben-pct-IN (M1)  TO DEEDIT-FIELD
100314           compute ws-9v999-of-deedit =
100314              function numval(deedit-field)
100314           IF WS-9V999-OF-DEEDIT NUMERIC
100314              MOVE WS-9v999-OF-DEEDIT
100314                                 TO ben-pct-IN (M1)
100314              MOVE AL-UANON      TO ben-pct-ATTRB (M1)
100314           ELSE
100314              MOVE ER-7132       TO EMI-ERROR
100314              perform 9900-ERROR-FORMAT thru 9900-exit
100314              MOVE -1            TO ben-pct-LEN (M1)
100314              MOVE AL-UABON      TO ben-pct-ATTRB (M1)
100314           END-IF
100314        END-IF
              IF PRE-EXIST-LEN (M1) > +0
                 MOVE AL-UANON         TO PRE-EXIST-ATTRB (M1)
              END-IF
              IF EXCL-PER-LEN (M1) > +0
                 MOVE EXCL-PERIOD (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO EXCL-PERIOD (M1)
                    MOVE AL-UANON      TO EXCL-PER-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO EXCL-PER-LEN (M1)
                    MOVE AL-UABON      TO EXCL-PER-ATTRB (M1)
                 END-IF
              END-IF
              IF COV-ENDS-LEN (M1) > +0
                 MOVE COV-ENDS (M1)      TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO COV-ENDS (M1)
                    MOVE AL-UANON      TO COV-ENDS-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO COV-ENDS-LEN (M1)
                    MOVE AL-UABON      TO COV-ENDS-ATTRB (M1)
                 END-IF
              END-IF
              IF ACC-PER-ENDS-LEN (M1) > +0
                 MOVE ACC-PER-ENDS (M1) TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO ACC-PER-ENDS-OUT (M1)
                    MOVE AL-UANON      TO ACC-PER-ENDS-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO ACC-PER-ENDS-LEN (M1)
                    MOVE AL-UABON      TO ACC-PER-ENDS-ATTRB (M1)
                 END-IF
              END-IF
              IF CRIT-PER-LEN (M1) > +0
                 MOVE CRIT-PER (M1)    TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO CRIT-PER-OUT (M1)
                    MOVE AL-UANON      TO CRIT-PER-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO CRIT-PER-LEN (M1)
                    MOVE AL-UABON      TO CRIT-PER-ATTRB (M1)
                 END-IF
              END-IF
              IF REC-CP-LEN (M1) > +0
                 EVALUATE TRUE
                    WHEN REC-CRIT-PER (M1) = ' Y' OR ' N'
                       MOVE REC-CRIT-PER (M1) (2:1)
                                       TO REC-CRIT-PER (M1) (1:2)
                       MOVE SPACES     TO REC-CRIT-PER (M1) (2:1)
                       MOVE AL-UANON   TO REC-CP-ATTRB (M1)
                    WHEN REC-CRIT-PER (M1) = 'Y ' OR 'N ' OR '  '
                       MOVE AL-UANON   TO REC-CP-ATTRB (M1)
                    WHEN OTHER
                       MOVE REC-CRIT-PER (M1)
                                       TO DEEDIT-FIELD
                       PERFORM 8600-DEEDIT
                                       THRU 8600-EXIT
                       IF WS-NUMVAL-OF-DEEDIT NUMERIC
                          MOVE WS-NUMVAL-OF-DEEDIT
                                       TO REC-CRIT-PER-N (M1)
                          MOVE AL-UANON TO REC-CP-ATTRB (M1)
                       ELSE
                          MOVE ER-9999 TO EMI-ERROR
                          PERFORM 9700-LINK-DATE-CONVERT
                                       THRU 9700-EXIT
                          MOVE -1      TO REC-CP-LEN (M1)
                          MOVE AL-UABON TO REC-CP-ATTRB (M1)
                       END-IF
                 END-EVALUATE
              END-IF
              IF RTW-MOS-LEN (M1) > +0
                 MOVE RTW-MOS  (M1)    TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO RTW-MOS-OUT (M1)
                    MOVE AL-UANON      TO RTW-MOS-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO RTW-MOS-LEN (M1)
                    MOVE AL-UABON      TO RTW-MOS-ATTRB (M1)
                 END-IF
              END-IF
              IF MAX-EXT-LEN (M1) > +0
                 MOVE MAX-EXT  (M1)    TO DEEDIT-FIELD
                 PERFORM 8600-DEEDIT   THRU 8600-EXIT
                 IF WS-NUMVAL-OF-DEEDIT NUMERIC
                    MOVE WS-NUMVAL-OF-DEEDIT TO MAX-EXT-OUT  (M1)
                    MOVE AL-UANON      TO MAX-EXT-ATTRB (M1)
                 ELSE
                    MOVE ER-9999       TO EMI-ERROR
                    perform 9900-ERROR-FORMAT thru 9900-exit
                    MOVE -1            TO MAX-EXT-LEN (M1)
                    MOVE AL-UABON      TO MAX-EXT-ATTRB (M1)
                 END-IF
              END-IF
           END-PERFORM

           IF TRUNCL > ZEROS
              IF TRUNCI = 'Y' OR 'N' OR ' '
                 CONTINUE
              ELSE
                 MOVE ER-9999          TO EMI-ERROR
                 perform 9900-ERROR-FORMAT thru 9900-exit
                 MOVE -1               TO TRUNCL
                 MOVE AL-UABON         TO TRUNCA
              END-IF
           END-IF

           .
       6000-EXIT.                                                       
           EXIT.                                                        
                                                                        
           EJECT                                                        
       7000-BUILD-OUTPUT-MAP.                                           
                                                                        
           MOVE LOW-VALUES                 TO  EL159AO
           MOVE PI-COMPANY-CD              TO  PI-PREV-COMPANY-CD
           MOVE PD-STATE                   TO  STATEO
                                               PI-PREV-STATE
           MOVE PD-PRODUCT-CD              TO  PRODCDO
                                               PI-PREV-PROD-CD
           MOVE PD-BEN-TYPE                TO  BENTYPO
                                               PI-PREV-BEN-TYPE
           MOVE PD-BEN-CODE                TO  BENCODEO
                                               PI-PREV-BEN-CODE
           IF PD-PROD-EXP-DT = HIGH-VALUES
               MOVE '99/99/9999'           TO  EXPDTO
               MOVE HIGH-VALUES            TO  PI-PREV-EXP-DT
           ELSE                                                         
               MOVE PD-PROD-EXP-DT         TO  DC-BIN-DATE-1            
                                               PI-PREV-EXP-DT           
               MOVE ' '                    TO  DC-OPTION-CODE           
               MOVE +0                     TO  DC-ELAPSED-DAYS          
                                               DC-ELAPSED-MONTHS        
               PERFORM 9700-LINK-DATE-CONVERT THRU 9700-EXIT            
               IF NO-CONVERSION-ERROR                                   
                   MOVE DC-GREG-DATE-A-EDIT TO EXPDTO                   
               ELSE                                                     
                   MOVE LOW-VALUES         TO  EXPDTO.                  
                                                                        
           MOVE PD-PRODUCT-DESC        TO PDESCO
           MOVE PD-1ST-YR-ADMIN-ALLOW  TO AFA-AMT-OUT

           PERFORM VARYING M1 FROM +1 BY +1 UNTIL
              M1 > +8
080322        IF R1 < 10
080322           MOVE R1                   TO WS-LINE-NBR-1
080322           MOVE ')'                  TO WS-LINE-NBR-P
080322        ELSE
080322           IF R1 > 11
                    MOVE LOW-VALUES           TO EL159-PROD-TABLE (M1)
080322              MOVE SPACES               TO LINE-NBR (M1)
                                                 WS-LINE-NBR-CHAR
080322              MOVE AL-SADON             TO PROD-CODE-ATTRB (M1)
080322                                           MAX-ATT-AGE-ATTRB (M1)
080322*                                          MIN-AGE-ATTRB  (M1)
080322                                           WAIT-PR-ATTRB  (M1)
080322                                           MAX-TERM-ATTRB (M1)
080322                                           ben-pct-attrb  (M1)
080322                                           MAX-AMT-ATTRB  (M1)
080322                                           PRE-EXIST-ATTRB(M1)
080322                                           EXCL-PER-ATTRB (M1)
080322                                           COV-ENDS-ATTRB (M1)
080322                                           ACC-PER-ENDS-ATTRB (M1)
080322                                           CRIT-PER-ATTRB (M1)
080322                                           REC-CP-ATTRB   (M1)
080322                                           RTW-MOS-ATTRB  (M1)
                                                 MAX-EXT-ATTRB  (M1)
080322           ELSE
080322              MOVE R1                   TO WS-LINE-NBR-2
080322              MOVE ')'                  TO WS-LINE-NBR-P2
080322           END-IF
080322        END-IF
080322        MOVE WS-LINE-NBR-CHAR        TO LINE-NBR (M1)
              IF PD-PROD-CODE (R1) NOT = SPACES
080322          AND R1 NOT > 11
080322           MOVE PD-PROD-CODE (R1)       TO PROD-CODE (M1)
080322           MOVE PD-MAX-ATT-AGE (R1)     TO MAX-ATT-AGE (M1)
080322*          MOVE PD-MIN-ISSUE-AGE (R1)   TO MIN-AGE (M1)
080322           MOVE PD-WAIT-PERIOD (R1)     TO WAIT-PR (M1)
080322           MOVE PD-MAX-TERM (R1)        TO MAX-TERM (M1)
080322           MOVE PD-MAX-AMT (R1)         TO MAX-AMT-OUT (M1)
100314           if pd-ben-pct (R1) not numeric
100314              move zeros to pd-ben-pct (R1)
100314           end-if
100314           move pd-ben-pct (R1)         to ben-pct-out (m1)
080322           MOVE PD-PRE-EXIST-EXCL-TYPE (R1) TO PRE-EXIST (M1)
080322           MOVE PD-EXCLUSION-PERIOD-DAYS (R1) TO EXCL-PERIOD (M1)
080322           MOVE PD-COVERAGE-ENDS-MOS (R1) TO COV-ENDS (M1)
080322           MOVE PD-ACCIDENT-ONLY-MOS (R1) TO ACC-PER-ENDS-OUT (M1)
080322           MOVE PD-CRIT-PERIOD (R1)     TO CRIT-PER-OUT (M1)
080322           MOVE PD-REC-CP-ALPHA    (R1) TO REC-CRIT-PER (M1)
080322           MOVE PD-RTW-MOS (R1)         TO RTW-MOS-OUT (M1)
080322           MOVE PD-MAX-EXTENSION (R1)   TO MAX-EXT-OUT (M1)
              END-IF
080322        ADD 1 TO R1
           END-PERFORM
080322     IF PAGE-NBR = 1
080322       AND PD-PROD-CODE (9) > SPACES
080322        MOVE 'YES' TO MORERECO
080322     ELSE
080322        MOVE 'NO'  TO MORERECO
080322     END-IF

           IF PD-TRUNCATED = 'Y'
              MOVE 'Y'                     TO TRUNCO
           ELSE
              MOVE 'N'                     TO TRUNCO
           END-IF
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
           MOVE AL-UANON                   TO  STATEA
                                               PRODCDA
                                               BENTYPA
                                               BENCODEA
                                               EXPDTA
           GO TO 8100-SEND-INITIAL-MAP.                                 

       7100-READ-BENEFIT.                                               
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND   (7120-NOT-FOUND)                                
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               DATASET   (ELCNTL-FILE-ID)                               
               RIDFLD    (ELCNTL-KEY)                                   
               SET       (ADDRESS OF CONTROL-FILE)                      
               GTEQ                                                     
           END-EXEC.                                                    
                                                                        
           IF PI-COMPANY-ID IS NOT EQUAL TO CF-COMPANY-ID OR            
              ELCNTL-RECORD-TYPE IS NOT EQUAL TO CF-RECORD-TYPE         
               GO TO 7120-NOT-FOUND.                                    
                                                                        
           MOVE +1                         TO  SUB-1.                   
                                                                        
       7110-LOOP.                                                       
                                                                        
           IF SUB-1 IS EQUAL TO +9                                      
               GO TO 7120-NOT-FOUND.                                    
                                                                        
           IF WS-BENE-CODE IS NOT EQUAL TO CF-BENEFIT-CODE (SUB-1)      
               ADD +1                      TO  SUB-1                    
               GO TO 7110-LOOP.                                         
                                                                        
           MOVE 'Y'                        TO  WS-CNTL-REC-FOUND-SW.    
           GO TO 7199-EXIT.                                             
                                                                        
       7120-NOT-FOUND.                                                  
           MOVE 'N'                        TO  WS-CNTL-REC-FOUND-SW.    
                                                                        
       7199-EXIT.                                                       
           EXIT.                                                        
           EJECT                                                        
       8000-READ-CNTL.                                                  
                                                                        
           EXEC CICS HANDLE CONDITION                                   
               NOTFND   (8009-NOTFND)                                   
           END-EXEC.                                                    
                                                                        
           EXEC CICS READ                                               
               DATASET   (ELCNTL-FILE-ID)                               
               RIDFLD    (ELCNTL-KEY)                                   
               SET       (ADDRESS OF CONTROL-FILE)                      
           END-EXEC.                                                    
                                                                        
           MOVE 'Y'                         TO  WS-CNTL-REC-FOUND-SW.   
           GO TO 8010-EXIT.                                             
                                                                        
       8009-NOTFND.                                                     
           MOVE 'N'                         TO  WS-CNTL-REC-FOUND-SW.   
                                                                        
       8010-EXIT.                                                       
           EXIT.                                                        
           EJECT                                                        
       8100-SEND-INITIAL-MAP.                                           

           MOVE EMI-MESSAGE-AREA (1)       TO  ERRMSG1O.                
           MOVE EIBTIME                    TO  TIME-IN.                 
           MOVE SAVE-DATE                  TO  DATEO.                   
           MOVE TIME-OUT                   TO  TIMEO.                   
                                                                        
           EXEC CICS SEND                                               
               MAP      (WS-MAP-NAME)                                   
               MAPSET   (MAPSET-NAME)                                   
               FROM     (EL159AO)                                       
               ERASE                                                    
               CURSOR                                                   
           END-EXEC.                                                    
                                                                        
           MOVE '159A'                 TO PI-CURRENT-SCREEN-NO

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
               FROM     (EL159AO)                                       
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

           display ' before ' deedit-field
           MOVE FUNCTION NUMVAL(DEEDIT-FIELD)
                                       TO WS-999V99-OF-DEEDIT
           display ' after  ' WS-999V99-OF-DEEDIT

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
                                                                        
       8870-NOTOPEN.                                                    
                                                                        
           MOVE LOW-VALUES                 TO  EL159AO.                 
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
