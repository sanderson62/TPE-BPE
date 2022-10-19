       IDENTIFICATION DIVISION.
       PROGRAM-ID. CIDENDSM.
       AUTHOR.     AJRA
      *REMARKS.
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 092012    2011022800001  AJRA  NEW PROGRAM
102412* 102412    2012101700002  AJRA  MODIFY STATEMENT, ADD FOOTER
112612* 112612    2012101700002  AJRA  ADD COMMAS TO AMOUNTS IN REPORT
011613* 011613    2013010200001  AJRA  CHG ACCT NUM TO COMP ACCT
031813* 031813  IR2013031400001  AJRA  CORRECT ACCT PORTION CALCULATION
031516* 031516  CR2016012700004  PEMA  ADD NEW CORRECTION CHK SECTION
120816* 120816  CR2016102500001  PEMA  REARRANGE HEADINGS PER CR
012319* 012319  IR2018121700001  TANA  CORRECT CANCELS NOT ON RPT 5
      ******************************************************************
       DATE-COMPILED.
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT SORT-WORK       ASSIGN TO SYS001-UT-FBA1-S-SORTWK1.  

           SELECT ERARCH          ASSIGN TO ERARCH
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS LA-CONTROL-PRIMARY
                                  FILE STATUS IS ERARCH-FILE-STATUS.

           SELECT ELLETR          ASSIGN TO ELLETR
                                  ACCESS IS DYNAMIC
                                  ORGANIZATION IS INDEXED
                                  FILE STATUS IS ELLETR-FILE-STATUS
                                  RECORD KEY IS TX-CONTROL-PRIMARY.

           SELECT ERENDT2         ASSIGN TO ERENDT2
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS EN-CONTROL-BY-ARCH-NO
                                  FILE STATUS IS ERENDT-FILE-STATUS.

           SELECT ELCERT          ASSIGN TO ELCERT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CM-CONTROL-PRIMARY
                                  FILE STATUS IS ELCERT-FILE-STATUS.

           SELECT ERACCT          ASSIGN TO ERACCT
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS AM-CONTROL-PRIMARY
                                  FILE STATUS IS ERACCT-FILE-STATUS.

           SELECT ERCOMP          ASSIGN TO ERCOMP
                                  ORGANIZATION IS INDEXED
                                  ACCESS IS DYNAMIC
                                  RECORD KEY IS CO-CONTROL-PRIMARY
                                  FILE STATUS IS ERCOMP-FILE-STATUS.

           SELECT  ACCT-SUM       ASSIGN TO ACCTSUM.

           SELECT  CSO-SUM        ASSIGN TO CSOSUM.

031516*    SELECT TEMP-OUT        ASSIGN TO SYS012
031516*       ORGANIZATION LINE SEQUENTIAL.

           SELECT DISK-DATE       ASSIGN TO SYS019.

           
       EJECT
       DATA DIVISION.
       FILE SECTION.

       SD  SORT-WORK.                                                   
       01  SORT-REC.                                                    
           12  SRT-KEY             PIC  X(69).                          
           12  SRT-CPTR            PIC  X(183).                         

       FD  ERARCH.

                                   COPY ERCARCH.

       FD  ELLETR.
                                   COPY ELCTEXT.

       FD  ERENDT2.
                                   COPY ERCENDT.

       FD  ELCERT.
           COPY ELCCERT.

       FD  ERACCT.

           COPY ERCACCT.

       FD  ERCOMP.

           COPY ERCCOMP.

       FD  ACCT-SUM
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  ACCTSUM-RECORD             PIC X(133).

       FD  CSO-SUM
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
       01  CSOSUM-RECORD              PIC X(133).

031516*FD  TEMP-OUT
031516*    RECORDING MODE F
031516*    LABEL RECORDS STANDARD
031516*    BLOCK CONTAINS 0 RECORDS.
031516*01  TEMP-OUT-REC               PIC X(252).

       FD  DISK-DATE
           COPY ELCDTEFD.

       EJECT
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '     ENDTX WORKING-STORAGE      '.
       77  FILLER  PIC X(32) VALUE '********************************'.

       77  WS-EOF-SW               PIC X VALUE SPACES.
           88  END-OF-ERARCH             VALUE 'Y'.
       77  WS-ENDT-FOUND           PIC X VALUE 'N'.
           88  ENDORSEMENT-FOUND         VALUE 'Y'.
           88  ENDORSEMENT-NOT-FOUND     VALUE 'N'.
       77  WS-CERT-FOUND-SW        PIC X VALUE 'N'.
           88  WS-CERT-FOUND             VALUE 'Y'.
           88  WS-CERT-NOT-FOUND         VALUE 'N'.
       77  WS-EOSRT-SW            PIC X VALUE SPACES.
           88  END-OF-SORT               VALUE 'Y'.
       77  WS-ADD-LINES-SW        PIC X VALUE SPACES.
           88  ADD-BLANK-LINES               VALUE 'Y'.
       77  WS-RPT-1-SW            PIC X VALUE SPACES.
           88  RPT-1-WRITTEN                 VALUE 'Y'.
       77  WS-RPT-2-SW            PIC X VALUE SPACES.
           88  RPT-2-WRITTEN                 VALUE 'Y'.
031516 77  WS-RPT-5-SW            PIC X VALUE SPACES.
031516     88  RPT-5-WRITTEN                 VALUE 'Y'.
       77  WS-RPT-3-SW            PIC X VALUE SPACES.
           88  RPT-3-WRITTEN                 VALUE 'Y'.
       77  WS-RPT-4-SW            PIC X VALUE SPACES.
           88  RPT-4-WRITTEN                 VALUE 'Y'.
       77  ARCH-RECS-IN           PIC 9(9) VALUE ZEROS.
       77  ENDT-RECS-IN           PIC 9(9) VALUE ZEROS.
       77  ACCT-RECS-OUT          PIC 9(9) VALUE ZEROS.
       77  CSO-RECS-OUT           PIC 9(9) VALUE ZEROS.
       77  PREV-SRT-RPT-TYPE      PIC X    VALUE SPACES.
       77  PREV-SRT-ACCT-ACCT-NO  PIC X(10) VALUE SPACES.
       77  PREV-SRT-CSO-ACCT-NO   PIC X(10) VALUE SPACES.
       77  PGM-SUB                PIC 9(3)  VALUE 555.
100112 77  S1                     PIC S999 COMP-3 VALUE +0.
       
       01  WS-MISC.
           05  WS-WORK-SEQ             PIC X.
           05  WS-NUM-SEQ REDEFINES WS-WORK-SEQ PIC 9.
           05  WS-SAVE-ERENDT          PIC X(207) VALUE LOW-VALUES.
           05  ELLETR-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERARCH-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERENDT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ELCERT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERACCT-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  ERCOMP-FILE-STATUS      PIC XX    VALUE ZEROS.
           05  WS-DATE                 PIC 9(11) VALUE ZEROS.
           05  WS-CERT-NO.
               10  WS-CERT-PRIME       PIC X(10) VALUE SPACES.
               10  WS-CERT-SFX         PIC X(1)  VALUE SPACES.
           05  WS-ZERO               PIC S9(01)    VALUE +0.
           05  WS-RETURN-CODE        PIC S9(03)    VALUE +0.
           05  WS-ABEND-MESSAGE      PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS  PIC X(02)     VALUE ZERO.
           05  WRK-COMPANY             PIC 99 COMP.
           05  FILLER REDEFINES WRK-COMPANY   COMP.
               10  FILLER              PIC 9.
               10  WRK-COMPANY-CD      PIC 9.
           05  WRK-LF-REPORTED-AMT     PIC S9(7)V99 COMP-3.
           05  WRK-AH-REPORTED-AMT     PIC S9(7)V99 COMP-3.
           05  WRK-LF-CHANGED-AMT      PIC S9(7)V99 COMP-3.
           05  WRK-AH-CHANGED-AMT      PIC S9(7)V99 COMP-3.
           05  WRK-LF-NET-CHG-AMT      PIC S9(7)V99 COMP-3.
           05  WRK-AH-NET-CHG-AMT      PIC S9(7)V99 COMP-3.
031813     05  WRK-ORIG-LF-PORTION     PIC S9(7)V99 COMP-3.
031813     05  WRK-ORIG-AH-PORTION     PIC S9(7)V99 COMP-3.
031813     05  WRK-NEW-LF-PORTION      PIC S9(7)V99 COMP-3.
031813     05  WRK-NEW-AH-PORTION      PIC S9(7)V99 COMP-3.
           05  WRK-TOTAL-DUE-AMT       PIC S9(7)V99 COMP-3.
           05  WRK-CSO-PORTION         PIC S9(7)V99 COMP-3.
           05  WRK-ACCT-PORTION        PIC S9(7)V99 COMP-3.
100112     05  WS-CHGBACK              PIC 99  VALUE ZEROS.
100112     05  WS-DIFF                 PIC 99  VALUE ZEROS.
           05  WRK-BEG-DATE.
               10  WRK-BEG-YYYY        PIC X(4).
               10  WRK-BEG-MM          PIC X(2).
               10  WRK-BEG-DD          PIC X(2).
           05  WRK-INPUT-DATE.
               10  WRK-INP-YYYY        PIC X(4).
               10  WRK-INP-MM          PIC X(2).
               10  WRK-INP-DD          PIC X(2).
           05  WRK-MOE-DATE.
               10  WRK-MOE-YYYY        PIC X(4).
               10  WRK-MOE-MM          PIC X(2).
               10  WRK-MOE-DD          PIC X(2).
           05  WRK-ME-DATE.
               10  WRK-ME-MM           PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-ME-DD           PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-ME-YYYY         PIC X(4).
           05  WRK-RUN-DATE.
               10  WRK-RUN-MM          PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-RUN-DD          PIC X(2).
               10  FILLER              PIC X  VALUE '/'.
               10  WRK-RUN-YYYY        PIC X(4).
           05  WRK-FIN-RESP            PIC X(10).
011613     05  WRK-FIN-ACCT-NUM        PIC X(10).
           05  WRK-FIN-ACCT-NAME       PIC X(30).
           05  WRK-FIN-MAIL-NAME       PIC X(30).
           05  WRK-ACCT-PAGE           PIC S9(5) COMP-3 VALUE +0.
           05  WRK-CSO-PAGE            PIC S9(5) COMP-3 VALUE +0.
           05  WRK-LINE-COUNT          PIC S9(5) COMP-3 VALUE +0.
102412     05  WRK-ACCT-LINE-COUNT     PIC S9(5) COMP-3 VALUE +0.
102412     05  WRK-CSO-LINE-COUNT      PIC S9(5) COMP-3 VALUE +0.
102412     05  WRK-MAX-LINES           PIC S9(5) COMP-3 VALUE +55.
102412     05  WRK-SECTION-MAX         PIC S9(5) COMP-3 VALUE +43.
           
       01  WS-PREV-ENDT-KEY.
           16  WS-COMPANY-CD           PIC X.
           16  WS-CARRIER              PIC X.
           16  WS-GROUPING             PIC X(6).
           16  WS-STATE                PIC XX.
           16  WS-ACCOUNT              PIC X(10).
           16  WS-CERT-EFF-DT          PIC XX.
           16  WS-CERT-NO.
               20  WS-CERT-PRIME       PIC X(10).
               20  WS-CERT-SFX         PIC X.
           16  WS-REC-TYPE             PIC X.
           16  WS-SEQ-NO               PIC 9(04) BINARY.
               
       01  WRK-ACCT-CONTROL-PART.
           05  WRK-ACCT-COMPANY-CD          PIC X.
           05  WRK-ACCT-CARRIER             PIC X.
           05  WRK-ACCT-GROUPING            PIC X(6).
           05  WRK-ACCT-STATE               PIC XX.
           05  WRK-ACCT-ACCOUNT             PIC X(10).
               
       01  WRK-COMP-CONTROL.
           05  WRK-COMP-COMPANY-CD          PIC X.
           05  WRK-COMP-CARRIER             PIC X.
           05  WRK-COMP-GROUPING            PIC X(6).
           05  WRK-COMP-RESP-NO             PIC X(10).
           05  WRK-COMP-ACCOUNT             PIC X(10).
           05  WRK-COMP-TYPE                PIC X(1).

       01  WS-SORT-REC.                                                 
           12  WS-SRT-KEY.                                              
               16  WS-SRT-KEY-A.
                   20  WS-SRT-CARR-GROUP.                                        
                       25  WS-SRT-CARR     PIC  X.                      
                       25  WS-SRT-GROUP    PIC  X(6).                   
                   20  WS-SRT-RESP-NO      PIC  X(10).                  
                   20  WS-SRT-ACCOUNT      PIC  X(10).                  
               16  WS-SRT-RPT-TYPE         PIC  X.
               16  FILLER                  PIC  X(2).
               16  WS-SRT-LOW.                                          
                   20  WS-SRT-LNAME        PIC  X(15).                  
                   20  WS-SRT-FNAME        PIC  X(10).                  
                   20  WS-SRT-INITS        PIC  X.                      
               16  WS-SRT-LOW-DATE  REDEFINES WS-SRT-LOW.               
                   20  FILLER              PIC  X(03).                  
                   20  WS-SRT-LOW-DT       PIC  9(08).                  
                   20  FILLER              PIC  X(15).                  
               16  WS-SRT-CERT             PIC  X(11).                  
               16  WS-SRT-REC-SFX          PIC  99.                     
           12  WS-SRT-FIN-ACCT-NAME            PIC X(30).
           12  WS-SRT-FIN-MAIL-NAME            PIC X(30).
           12  WS-SRT-INS-LAST-NAME            PIC X(15).
           12  WS-SRT-CERT-EFF-DT              PIC X(8).
           12  WS-SRT-LF-REPORTED-AMT          PIC -(7).99.
           12  WS-SRT-LF-CHANGED-AMT           PIC -(7).99.
           12  WS-SRT-LF-NET-CHG-AMT           PIC -(7).99.
           12  WS-SRT-AH-REPORTED-AMT          PIC -(7).99.
           12  WS-SRT-AH-CHANGED-AMT           PIC -(7).99.
           12  WS-SRT-AH-NET-CHG-AMT           PIC -(7).99.
           12  WS-SRT-TOTAL-DUE-AMT            PIC -(7).99.
           12  WS-SRT-ACCT-PORTION             PIC -(7).99.
           12  WS-SRT-CSO-PORTION              PIC -(7).99.
           12  WS-SRT-COMMENT                  PIC X(8).
           12  WS-SRT-ACCT-SUMM                PIC X.
           12  WS-SRT-CSO-SUMM                 PIC X.

       01  HD1-ACCT.
           12  FILLER              PIC  X(1)           VALUE '1'.
           12  FILLER              PIC  X(61)          VALUE SPACES.    
           12  FILLER              PIC  X(10)          VALUE
                   'SUMMARY OF'.
           12  FILLER              PIC  X(46)          VALUE SPACES.
           12  FILLER              PIC  X(15)           VALUE 
                   'ACCOUNT SUMMARY'.

       01  HD1-CSO.                                                         
           12  FILLER              PIC  X(1)           VALUE '1'.
           12  FILLER              PIC  X(57)          VALUE SPACES.    
           12  FILLER              PIC  X(17)          VALUE
                   'BALANCING SUMMARY'.
           12  FILLER              PIC  X(47)          VALUE SPACES.
           12  FILLER              PIC  X(11)           VALUE 
                   'CSO SUMMARY'.

       01  HD2-ACCT.
           12  FILLER              PIC  X(53)          VALUE SPACES.    
           12  FILLER              PIC  X(28)          VALUE
                  'PREMIUM CHANGES DUE BORROWER'.                          
           12  FILLER              PIC  X(42)          VALUE SPACES.    
           12  HD2-RUN-DT          PIC  X(10)          VALUE SPACES.

       01  HD2-CSO.
           12  FILLER              PIC  X(59)          VALUE SPACES.    
           12  FILLER              PIC  X(15)          VALUE
                  'PREMIUM CHANGES'.                          
           12  FILLER              PIC  X(49)          VALUE SPACES.    
           12  HD2R-RUN-DT         PIC  X(10)          VALUE SPACES.

       01  HD3-ACCT.
           12  FILLER              PIC  X(55)          VALUE SPACES.
           12  FILLER              PIC  X(14)          VALUE
                  'PERIOD ENDING '.
           12  HD3-DT              PIC  X(10).                          
           12  FILLER              PIC  X(41)          VALUE SPACES.    
           12  FILLER              PIC  X(5)           VALUE 'PAGE'.    
           12  HD3-PG              PIC  ZZ,ZZ9.                         
           12  FILLER              PIC  X(2)           VALUE SPACES.    

       01  HD3-CSO.
           12  FILLER              PIC  X(55)          VALUE SPACES.
           12  FILLER              PIC  X(14)          VALUE
                  'PERIOD ENDING '.
           12  HD3R-DT             PIC  X(10).                          
           12  FILLER              PIC  X(41)          VALUE SPACES.    
           12  FILLER              PIC  X(5)           VALUE 'PAGE'.    
           12  HD3R-PG             PIC  ZZ,ZZ9.                         
           12  FILLER              PIC  X(2)           VALUE SPACES.    
                                                                        
       01  HD4-ACCT.
           12  FILLER              PIC  X(1)           VALUE '0'.
           12  HD4-M1              PIC  X(14).                          
           12  HD4-CARR-GROUP      PIC  X(7).                           
           12  FILLER              PIC  X              VALUE '-'.                              
           12  HD4-ACCOUNT         PIC  X(10).                          
           12  FILLER              PIC  X(45).                          
           12  FILLER              PIC  X(11)          VALUE
                  'REMIT TO - '.
           12  HD4-RESP            PIC  X(10).                             
           12  FILLER              PIC  XX.                             
           12  HD4-REMIT           PIC  X(30).                          
           12  FILLER              PIC  XX.                             
                                                                        
       01  HD4-CSO.
           12  FILLER              PIC  X(1)           VALUE '0'.
           12  HD4R-M1             PIC  X(14).                          
           12  HD4R-CARR-GROUP     PIC  X(7).                           
           12  FILLER              PIC  X              VALUE '-'.                              
           12  HD4R-ACCT           PIC  X(10).                          
           12  FILLER              PIC  X(45).                          
           12  FILLER              PIC  X(11)          VALUE
                  'REMIT TO - '.
           12  HD4R-RESP           PIC  X(10).                             
           12  FILLER              PIC  XX.                             
           12  HD4R-REMIT          PIC  X(30).                          
           12  FILLER              PIC  XX.                             

       01  HD4A-ACCT-1.
           12  FILLER              PIC X(1)  VALUE '0'.
031516     12  FILLER              PIC X(50) VALUE
031516         'FOR YOUR REVIEW:  The list of transactions below r'.
031516     12  FILLER              PIC X(50) VALUE
031516         'esult in amounts due to your customers.  The three'.
031516     12  FILLER              PIC X(32) VALUE
031516         ' sections summarize the         '.
031516 01  HD4A-ACCT-2.
031516     12  FILLER              PIC X(1)  VALUE ' '.
031516     12  FILLER              PIC X(50) VALUE
031516         'transactions for Credit Insurance contracts you is'.
031516     12  FILLER              PIC X(50) VALUE
031516         'sued that require a refund to your customer(s) thr'.
031516     12  FILLER              PIC X(32) VALUE
031516         'ough either a credit to their   '.
031516
031516 01  HD4A-ACCT-3.
031516     12  FILLER              PIC X(1)  VALUE ' '.
031516     12  FILLER              PIC X(132) VALUE 'loan, a direct refu
031516-        'nd to the borrower, or through a check sent by CSO direc
031516-        'tly to the creditor.  You were'.
031516
031516 01  HD4A-ACCT-4.
031516     12  FILLER              PIC X(1)  VALUE ' '.
031516     12  FILLER              PIC X(132) VALUE 'previously notified
031516-        ' by letter of the details for each transaction listed be
031516-        'low.  These transactions are reflected in your current'.
031516
031516
031516 01  HD4A-ACCT-5.
031516     12  FILLER              PIC X(1)  VALUE ' '.
031516     12  FILLER              PIC X(132) VALUE 'billing statement b
031516-        'alance.  Please use this summary to ensure that all requ
031516-        'ired transactions have been completed as instructed.'.
031516
031516 01  HD4A-ACCT-6.
031516     12  FILLER              PIC X(1)  VALUE ' '.
031516     12  FILLER              PIC X(132) VALUE 'If you have any que
031516-        'stions, contact us at 800-826-6587.'.

       01  HD5-DASH.
           12  FILLER              PIC  X              VALUE SPACES.
           12  FILLER              PIC  X(132)         VALUE ALL '-'.    
                                                                        
       01  HD5A.                                                        
031516     12  HD5A-ACTION         PIC  X(48)          VALUE SPACES.    
120816     12  HD5A-TITLE          PIC  X(47)          VALUE SPACES.
120816     12  FILLER              PIC  X(38)          VALUE SPACES.    

       01  HD5-TITLES.
031516     12  HD5-ACTION-1     PIC X(30)  VALUE
031516         ' Action required by you : '.
031516     12  HD5-ACTION-2     PIC X(30)  VALUE
031516         ' No action required by you : '.
           12  HD5-ACCT-1       PIC  X(37)          VALUE
                   '       CREDIT TO LOAN BALANCE        '.
           12  HD5-ACCT-2       PIC  X(37)          VALUE
                   '      REFUND DIRECT TO BORROWER      '.
           12  HD5-CSO-3        PIC  X(37)          VALUE
                   'PREMIUM CHANGES  -  MONEY DUE ACCOUNT'.
           12  HD5-CSO-4        PIC  X(37)          VALUE
                   '  PREMIUM CHANGES  -  MONEY DUE CSO  '.
031516     12  HD5-ACCT-5       PIC  X(47)          VALUE
120816             '   CSO REFUNDED DIRECT TO CREDITOR OR BORROWER '.

       01  HD6.
           12  FILLER              PIC  X              VALUE SPACES.
           12  FILLER              PIC  X(102)          VALUE ALL '-'.    
           12  FILLER              PIC  X(1)           VALUE SPACES.
           12  FILLER              PIC  X(7)          VALUE ALL '-'.
           12  HD6-REVIEW          PIC  X(15)          VALUE SPACES.
           12  FILLER              PIC  X(7)          VALUE ALL '-'.

       01  HD7A.                                                        
           12  FILLER              PIC  X(1)           VALUE SPACES.
           12  FILLER              PIC  X(44)          VALUE            
                   'CERTIFICATE     LAST     EFFECTIVE LIFE PREM'.      
           12  FILLER              PIC  X(44)          VALUE            
                   ' LIFE PREM LIFE PREM DIS PREM  DIS PREM DIS '.      
           12  FILLER              PIC  X(44)          VALUE            
                   'PREM   TOTAL   TOTAL NET  TOTAL NET  COMMENT'.
102412
102412 01  HD7A2.                                                        
102412     12  FILLER              PIC  X(1)           VALUE SPACES.
102412     12  FILLER              PIC  X(44)          VALUE            
102412             'CERTIFICATE     LAST     EFFECTIVE LIFE RFND'.      
102412     12  FILLER              PIC  X(44)          VALUE            
102412             ' LIFE RFND LIFE RFND DIS RFND  DIS RFND DIS '.      
102412     12  FILLER              PIC  X(44)          VALUE            
102412             'RFND   TOTAL   TOTAL NET  TOTAL NET  COMMENT'.

031516 01  HD7A3.
031516     12  FILLER              PIC  X(1)           VALUE SPACES.
031516     12  FILLER              PIC  X(132) VALUE 'CERTIFICATE     LA
031516-        'ST     EFFECTIVE LIFE PREM LIFE PREM LIFE PREM DIS PREM 
120816-        ' DIS PREM DIS PREM   TOTAL   TOTAL NET  TOTAL NET  COMME
031516-        'NT'.

       01  HD7B.                                                        
           12  FILLER              PIC  X(1)           VALUE SPACES.
           12  FILLER              PIC  X(44)          VALUE            
                   '  NUMBER        NAME        DATE   REPORTED '.      
           12  FILLER              PIC  X(44)          VALUE            
                   '  CHANGED    NET     REPORTED  CHANGED    NE'.      
           12  FILLER              PIC  X(44)          VALUE            
                   'T       DUE   PREM CHANGE PREM CHANGE       '.
102412
102412 01  HD7B2.                                                        
102412     12  FILLER              PIC  X(1)           VALUE SPACES.
102412     12  FILLER              PIC  X(44)          VALUE            
102412             '  NUMBER        NAME        DATE   REPORTED '.      
102412     12  FILLER              PIC  X(44)          VALUE            
102412             '  CHANGED    NET     REPORTED  CHANGED    NE'.      
102412     12  FILLER              PIC  X(44)          VALUE            
102412             'T       DUE   RFND CHANGE RFND CHANGE       '.

031516 01  HD7B3.
031516     12  FILLER              PIC  X(1)           VALUE SPACES.
031516     12  FILLER              PIC  X(132) VALUE '  NUMBER        NA
031516-        'ME        DATE   REPORTED   CHANGED    NET     REPORTED 
120816-        ' CHANGED    NET     REFUND  PREM CHANGE PREM CHANGE'.

       01  HD7C.                                                        
           12  FILLER              PIC  X(1)           VALUE SPACES.
           12  FILLER              PIC  X(44)          VALUE            
                   '                                     AS:    '.      
           12  FILLER              PIC  X(44)          VALUE            
                   '    TO:     CHANGE     AS:       TO:     CHA'.      
           12  FILLER              PIC  X(44)          VALUE            
                   'NGE   BORROWER   ACCOUNT     CSO            '.

031516 01  HD7C3.
031516     12  FILLER              PIC  X(1)           VALUE SPACES.
031516     12  filler              pic  x(37)          value spaces.
031516     12  FILLER              PIC  X(83) VALUE 'AS:        TO:     
120816-        'CHANGE     AS:       TO:     CHANGE    SENT      ACCOUNT
031516-        '     CSO'.

       01  HD7D.                                                        
           12  FILLER              PIC  X(1)           VALUE SPACES.
           12  FILLER              PIC  X(44)          VALUE            
                   '----------- ------------ -------- --------- '.
           12  FILLER              PIC  X(44)          VALUE            
                   '--------- --------- --------- --------- ----'.
           12  FILLER              PIC  X(44)          VALUE            
                   '----- --------- --------- --------- --------'.

102412 01  FOOTER-LINE.
102412     12  FILLER              PIC X(1)  VALUE ' '.
102412     12  FILLER              PIC X(50) VALUE
102412         '*The amounts reflected on this summary may vary sl'.
102412     12  FILLER              PIC X(50) VALUE
102412         'ightly from your statement due to rounding issues.'.
102412     12  FILLER              PIC X(32) VALUE SPACES.
102412
       01  PRINT-LINE.
           12  PRT-CTRL                     PIC X(1).
           12  PRT-LINE                     PIC X(132).

       01  NONE-LINE.
           12  FILLER                       PIC X(13)  VALUE SPACES.
           12  FILLER                       PIC X(04)  VALUE 'NONE'.
           12  FILLER                       PIC X(116) VALUE SPACES.
           
       01  DETAIL-LINE.
           12  FILLER                       PIC X(1) VALUE SPACES.
           12  DET-CERT-NO                  PIC X(11).
           12  FILLER                       PIC X(1) VALUE SPACES.
           12  DET-INS-LAST-NAME            PIC X(13).
           12  DET-CERT-EFF-DT              PIC X(8).
112612     12  DET-LF-REPORTED-AMT          PIC ---,---.99.
112612     12  DET-LF-CHANGED-AMT           PIC ---,---.99.
112612     12  DET-LF-NET-CHG-AMT           PIC ---,---.99.
112612     12  DET-AH-REPORTED-AMT          PIC ---,---.99.
112612     12  DET-AH-CHANGED-AMT           PIC ---,---.99.
112612     12  DET-AH-NET-CHG-AMT           PIC ---,---.99.
112612     12  DET-TOTAL-DUE-AMT            PIC ---,---.99.
112612     12  DET-ACCT-PORTION             PIC ---,---.99.
112612     12  DET-CSO-PORTION              PIC ---,---.99.
           12  FILLER                       PIC X(1) VALUE SPACES.
           12  DET-COMMENT                  PIC X(8).

                                       COPY ELCZREC.
                                       
                                       COPY ELCDATE.

                                       COPY ELCDTECX.

                                       COPY ELCDTEVR.
       
       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-PREVME-DATE            PIC X(08)     VALUE SPACES.
           05  PARM-ME-DATE                PIC X(08)     VALUE SPACES.
       

       PROCEDURE DIVISION USING PARM.


       0001-DT-CRD-READ SECTION.
                                       COPY ELCDTERX.

       0002-MAIN.

           IF PARM-PREVME-DATE = SPACES
               DISPLAY 'MISSING PREV MO END PARM'
               PERFORM ABEND-PGM
           END-IF.
           
           IF PARM-ME-DATE = SPACES
               DISPLAY 'MISSING MO END DATE PARM'
               PERFORM ABEND-PGM
           END-IF.

           DISPLAY 'PARM MO END DATE = ' PARM-ME-DATE
           DISPLAY 'PARM PREV MO END = ' PARM-PREVME-DATE

           MOVE PARM-PREVME-DATE TO WRK-BEG-DATE.
           MOVE PARM-ME-DATE TO WRK-MOE-DATE.
           MOVE WRK-MOE-MM   TO WRK-ME-MM.
           MOVE WRK-MOE-DD   TO WRK-ME-DD.
           MOVE WRK-MOE-YYYY TO WRK-ME-YYYY.
           MOVE WRK-ME-DATE  TO HD3-DT
                                HD3R-DT.
           MOVE RUN-MO       TO WRK-RUN-MM.
           MOVE RUN-DA       TO WRK-RUN-DD.
           MOVE RUN-CCYY     TO WRK-RUN-YYYY.
           MOVE WRK-RUN-DATE TO HD2-RUN-DT
                                HD2R-RUN-DT.

           PERFORM 0400-OPEN-FILES THRU 0400-EXIT

           PERFORM 0550-START-ERARCH   THRU 0550-EXIT

           SORT SORT-WORK
               ASCENDING KEY  SRT-KEY
               INPUT PROCEDURE 0100-INPUT-ROUTINE   THRU 0100-EXIT
               OUTPUT PROCEDURE 1000-OUTPUT-ROUTINE THRU 1000-EXIT.

102412     MOVE '2'  TO  WS-SRT-RPT-TYPE
           IF RPT-1-WRITTEN AND NOT RPT-2-WRITTEN
               PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
                                             THRU 7200-EXIT
102412         MOVE NONE-LINE       TO PRINT-LINE
102412         PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412         ADD +1               TO WRK-ACCT-LINE-COUNT
           END-IF
031516     if rpt-2-written and not rpt-5-written
031516         PERFORM 7210-PRINT-REC-TYPE-5-HEADINGS 
031516                                       THRU 7210-EXIT
031516         MOVE NONE-LINE       TO PRINT-LINE
031516         PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516         ADD +1               TO WRK-ACCT-LINE-COUNT
031516     END-IF

102412     MOVE WRK-ACCT-LINE-COUNT TO WRK-LINE-COUNT
102412     MOVE +0                  TO WRK-ACCT-LINE-COUNT
102412     PERFORM 7500-PRINT-FOOTER THRU 7500-EXIT

102412     MOVE '4'  TO  WS-SRT-RPT-TYPE
           IF RPT-3-WRITTEN AND NOT RPT-4-WRITTEN
               PERFORM 7400-PRINT-REC-TYPE-4-HEADINGS 
                                             THRU 7400-EXIT
102412         MOVE NONE-LINE       TO PRINT-LINE
102412         PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412         ADD +1               TO WRK-CSO-LINE-COUNT
           END-IF
102412*     MOVE WRK-CSO-LINE-COUNT TO WRK-LINE-COUNT
102412*     MOVE +0                 TO WRK-CSO-LINE-COUNT
102412*     PERFORM 7500-PRINT-FOOTER THRU 7500-EXIT

           PERFORM 0500-CLOSE-FILES THRU 0500-EXIT

           DISPLAY ' ARCH RECORDS READ    ' ARCH-RECS-IN
           DISPLAY ' ENDT RECORDS FOUND   ' ENDT-RECS-IN
           DISPLAY ' ACCT RECORDS WRITTEN ' ACCT-RECS-OUT
           DISPLAY ' CSO RECORDS WRITTEN  ' CSO-RECS-OUT
           GOBACK.           


       0100-INPUT-ROUTINE.
           PERFORM 0150-PROCESS-EXT THRU 0150-EXIT UNTIL
                 END-OF-ERARCH
            .
       0100-EXIT.
           EXIT.


       0150-PROCESS-EXT.

           PERFORM 0200-READ-TRLR THRU 0200-EXIT
           IF END-OF-ERARCH
               GO TO 0150-EXIT
           END-IF.
       
           IF (LA-VOIDED-DATE NOT EQUAL LOW-VALUES AND SPACES)  OR
              (LA-PURGED-DATE NOT EQUAL LOW-VALUES AND SPACES)  OR
              (LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES OR SPACES)
                  GO TO 0150-EXIT
           END-IF.
           
           MOVE LA-INITIAL-PRINT-DATE TO DC-BIN-DATE-1.
           MOVE ' '                 TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-CYMD TO WRK-INPUT-DATE
           ELSE
               GO TO 0150-EXIT
           END-IF.
           
031813*****THE FINAL ACTION CANCELS DO NOT GET CREATED ON THE LAST DAY 
031813*****OF THE MONTH SO PICK UP THE ONES FROM THE LAST DAY OF THE 
031813*****PRIOR MONTH AND BYPASS THE ONES FOR THE LAST DAY OF THIS MONTH.
031813     IF WRK-INPUT-DATE = WRK-BEG-DATE AND
031813        (LA-FINAL-ACT-IND = 'B' OR 'C')
031813           PERFORM 0300-RELEASE-SRT THRU 0300-EXIT
031813           GO TO 0150-EXIT
031813     END-IF
031813
031813*     IF WRK-INPUT-DATE = WRK-MOE-DATE AND
031813*        (LA-FINAL-ACT-IND = 'B' OR 'C')
031813*           GO TO 0150-EXIT
031813*     END-IF
031813
           IF WRK-INPUT-DATE > WRK-BEG-DATE
              AND WRK-INPUT-DATE <= WRK-MOE-DATE
                PERFORM 0300-RELEASE-SRT THRU 0300-EXIT
           END-IF

           .

       0150-EXIT.
           EXIT.

       0200-READ-TRLR.

           READ ERARCH NEXT RECORD

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH READ NEXT ' ERARCH-FILE-STATUS
                 SET END-OF-ERARCH     TO TRUE
              END-IF
           END-IF
           
           IF LA-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD 
                SET END-OF-ERARCH     TO TRUE
           END-IF


           .

       0200-EXIT.
           EXIT.

       0300-RELEASE-SRT.

           PERFORM 0750-GET-Z-RECORD THRU 0750-EXIT
031516     IF (W-ACCT-SUMM EQUAL '1' OR '2' or '5') OR
              (W-CSO-SUMM EQUAL '3' OR '4')
                  CONTINUE
           ELSE
               GO TO 0300-EXIT
           END-IF

           ADD 1 TO ARCH-RECS-IN

           SET ENDORSEMENT-NOT-FOUND TO TRUE
           MOVE LA-COMPANY-CD TO EN-COMPANY-CD-A1
031813     IF LA-ENDT-ARCH-NO NOT EQUAL LOW-VALUES AND
031813        LA-ENDT-ARCH-NO > ZERO
031813           MOVE LA-ENDT-ARCH-NO TO EN-ARCHIVE-NO
031813     ELSE
031813           MOVE LA-ARCHIVE-NO TO EN-ARCHIVE-NO
031813     END-IF
           PERFORM 0600-READ-ERENDT THRU 0600-EXIT.
           
           PERFORM 0380-GET-ERACCT THRU 0380-EXIT
       
           MOVE SPACES TO WS-SORT-REC.
           MOVE +0     TO WS-SRT-CSO-PORTION
                          WS-SRT-ACCT-PORTION.
           MOVE +0     TO WS-SRT-LF-REPORTED-AMT
                          WS-SRT-AH-REPORTED-AMT
                          WS-SRT-LF-CHANGED-AMT
                          WS-SRT-AH-CHANGED-AMT
                          WS-SRT-LF-NET-CHG-AMT
                          WS-SRT-AH-NET-CHG-AMT
                          WS-SRT-TOTAL-DUE-AMT.
           MOVE +0     TO WRK-LF-REPORTED-AMT
                          WRK-AH-REPORTED-AMT
                          WRK-LF-CHANGED-AMT
                          WRK-AH-CHANGED-AMT
                          WRK-LF-NET-CHG-AMT
                          WRK-AH-NET-CHG-AMT
                          WRK-TOTAL-DUE-AMT.

           IF ENDORSEMENT-NOT-FOUND
               PERFORM 0350-BUILD-FROM-ARCH THRU 0350-EXIT
               GO TO 0300-EXIT
           END-IF

           ADD 1 TO ENDT-RECS-IN
                          
           MOVE EN-CARRIER       TO  WS-SRT-CARR.
           MOVE EN-GROUPING      TO  WS-SRT-GROUP.
011613*    MOVE EN-ACCOUNT       TO  WS-SRT-ACCOUNT.
011613     MOVE WRK-FIN-ACCT-NUM TO  WS-SRT-ACCOUNT.
           MOVE WRK-FIN-RESP     TO  WS-SRT-RESP-NO.
           MOVE WRK-FIN-ACCT-NAME TO  WS-SRT-FIN-ACCT-NAME.
           MOVE WRK-FIN-MAIL-NAME TO  WS-SRT-FIN-MAIL-NAME.
           MOVE EN-CERT-NO       TO  WS-SRT-CERT.

           MOVE EN-CERT-EFF-DT   TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT TO WS-SRT-CERT-EFF-DT
           ELSE
               MOVE SPACES              TO WS-SRT-CERT-EFF-DT
           END-IF.
           IF EN-LF-ORIG-PRM-AMT = 2020202.02
               MOVE ZERO TO EN-LF-ORIG-PRM-AMT
           END-IF
           IF EN-LF-ORIG-ALT-PRM-AMT = 2020202.02
               MOVE ZERO TO EN-LF-ORIG-ALT-PRM-AMT
           END-IF
           IF EN-LF-NEW-PRM-AMT = 2020202.02
               MOVE ZERO TO EN-LF-NEW-PRM-AMT
           END-IF
           IF EN-LF-NEW-ALT-PRM-AMT = 2020202.02
               MOVE ZERO TO EN-LF-NEW-ALT-PRM-AMT
           END-IF
           IF EN-AH-ORIG-PRM-AMT = 2020202.02
               MOVE ZERO TO EN-AH-ORIG-PRM-AMT
           END-IF
           IF EN-AH-NEW-PRM-AMT = 2020202.02
               MOVE ZERO TO EN-AH-NEW-PRM-AMT
           END-IF
           IF EN-LF-ORIG-REF-AMT = 2020202.02
               MOVE ZERO TO EN-LF-ORIG-REF-AMT
           END-IF
           IF EN-LF-NEW-REF-AMT = 2020202.02
               MOVE ZERO TO EN-LF-NEW-REF-AMT
           END-IF
           IF EN-AH-ORIG-REF-AMT = 2020202.02
               MOVE ZERO TO EN-AH-ORIG-REF-AMT
           END-IF
           IF EN-AH-NEW-REF-AMT = 2020202.02
               MOVE ZERO TO EN-AH-NEW-REF-AMT
           END-IF
           IF EN-REC-TYPE = 'I'
              IF EN-INS-NEW-LAST-NAME NOT > SPACES
                  MOVE SPACES TO WS-SRT-INS-LAST-NAME
              ELSE
                  MOVE EN-INS-NEW-LAST-NAME TO WS-SRT-INS-LAST-NAME
              END-IF
              COMPUTE WRK-LF-REPORTED-AMT = EN-LF-ORIG-PRM-AMT +
                                            EN-LF-ORIG-ALT-PRM-AMT
              COMPUTE WRK-LF-CHANGED-AMT = EN-LF-NEW-PRM-AMT +
                                           EN-LF-NEW-ALT-PRM-AMT
              MOVE EN-AH-ORIG-PRM-AMT TO WRK-AH-REPORTED-AMT
              MOVE EN-AH-NEW-PRM-AMT  TO WRK-AH-CHANGED-AMT
              MOVE 'Endorsed'         TO WS-SRT-COMMENT
              COMPUTE WRK-LF-NET-CHG-AMT = WRK-LF-REPORTED-AMT -
                                           WRK-LF-CHANGED-AMT
              COMPUTE WRK-AH-NET-CHG-AMT = WRK-AH-REPORTED-AMT -
                                           WRK-AH-CHANGED-AMT
              COMPUTE WRK-TOTAL-DUE-AMT  = WRK-LF-NET-CHG-AMT +
                                           WRK-AH-NET-CHG-AMT
100112        IF EN-CHG-ACCT
031813           COMPUTE WRK-ORIG-LF-PORTION =
031813               WRK-LF-REPORTED-AMT * EN-LF-ORIG-COMM-PCT
031813           COMPUTE WRK-ORIG-AH-PORTION =
031813               WRK-AH-REPORTED-AMT * EN-AH-ORIG-COMM-PCT
031813           COMPUTE WRK-NEW-LF-PORTION =
031813               WRK-LF-CHANGED-AMT * EN-LF-NEW-COMM-PCT
031813           COMPUTE WRK-NEW-AH-PORTION =
031813               WRK-AH-CHANGED-AMT * EN-AH-NEW-COMM-PCT
031813           COMPUTE WRK-ACCT-PORTION =
031813              (WRK-ORIG-LF-PORTION + WRK-ORIG-AH-PORTION) -
031813              (WRK-NEW-LF-PORTION + WRK-NEW-AH-PORTION)
100112        ELSE
100112           MOVE ZERO TO WRK-ACCT-PORTION
100112        END-IF
100112        COMPUTE WRK-CSO-PORTION = 
100112           WRK-TOTAL-DUE-AMT - WRK-ACCT-PORTION
           ELSE
              IF EN-INS-LAST-NAME NOT > SPACES
                  MOVE SPACES TO WS-SRT-INS-LAST-NAME
              ELSE
                  MOVE EN-INS-LAST-NAME TO WS-SRT-INS-LAST-NAME
              END-IF
012319        IF W-ACCT-SUMM = '5'
012319           IF EN-LF-ORIG-REF-AMT = EN-LF-NEW-REF-AMT
012319              MOVE ZERO TO EN-LF-ORIG-REF-AMT
012319           END-IF
012319           IF EN-AH-ORIG-REF-AMT = EN-AH-NEW-REF-AMT
012319              MOVE ZERO TO EN-AH-ORIG-REF-AMT
012319           END-IF
012319        END-IF
              MOVE EN-LF-ORIG-REF-AMT TO WRK-LF-REPORTED-AMT
              MOVE EN-AH-ORIG-REF-AMT TO WRK-AH-REPORTED-AMT
              MOVE EN-LF-NEW-REF-AMT TO WRK-LF-CHANGED-AMT
              MOVE EN-AH-NEW-REF-AMT TO WRK-AH-CHANGED-AMT
              MOVE 'Cancel'          TO WS-SRT-COMMENT
              COMPUTE WRK-LF-NET-CHG-AMT = WRK-LF-CHANGED-AMT -
                                           WRK-LF-REPORTED-AMT
              COMPUTE WRK-AH-NET-CHG-AMT = WRK-AH-CHANGED-AMT -
                                           WRK-AH-REPORTED-AMT
              COMPUTE WRK-TOTAL-DUE-AMT  = WRK-LF-NET-CHG-AMT +
                                           WRK-AH-NET-CHG-AMT
100112        IF EN-CHG-ACCT
031813           COMPUTE WRK-ORIG-LF-PORTION =
031813               WRK-LF-REPORTED-AMT * EN-LF-ORIG-REF-COMM-PCT
031813           COMPUTE WRK-ORIG-AH-PORTION =
031813               WRK-AH-REPORTED-AMT * EN-AH-ORIG-REF-COMM-PCT
031813           COMPUTE WRK-NEW-LF-PORTION =
031813               WRK-LF-CHANGED-AMT * EN-LF-NEW-REF-COMM-PCT
031813           COMPUTE WRK-NEW-AH-PORTION =
031813               WRK-AH-CHANGED-AMT * EN-AH-NEW-REF-COMM-PCT
031813           COMPUTE WRK-ACCT-PORTION =
031813              (WRK-NEW-LF-PORTION + WRK-NEW-AH-PORTION) -
031813              (WRK-ORIG-LF-PORTION + WRK-ORIG-AH-PORTION)
100112        ELSE
100112           MOVE ZERO TO WRK-ACCT-PORTION
100112        END-IF
100112        COMPUTE WRK-CSO-PORTION = 
100112           WRK-TOTAL-DUE-AMT - WRK-ACCT-PORTION
           END-IF
      
           MOVE WRK-LF-REPORTED-AMT TO WS-SRT-LF-REPORTED-AMT.
           MOVE WRK-AH-REPORTED-AMT TO WS-SRT-AH-REPORTED-AMT.
           MOVE WRK-LF-CHANGED-AMT TO WS-SRT-LF-CHANGED-AMT.
           MOVE WRK-AH-CHANGED-AMT TO WS-SRT-AH-CHANGED-AMT.
           MOVE WRK-LF-NET-CHG-AMT TO WS-SRT-LF-NET-CHG-AMT.
           MOVE WRK-AH-NET-CHG-AMT TO WS-SRT-AH-NET-CHG-AMT.
           MOVE WRK-TOTAL-DUE-AMT  TO WS-SRT-TOTAL-DUE-AMT.
100112
100112     MOVE WRK-CSO-PORTION    TO WS-SRT-CSO-PORTION.
100112     MOVE WRK-ACCT-PORTION   TO WS-SRT-ACCT-PORTION.
100112
100112*    IF EN-CSO-PORTION NUMERIC
100112*       MOVE EN-CSO-PORTION  TO WS-SRT-CSO-PORTION
100112*    ELSE
100112*       MOVE ZERO            TO WS-SRT-CSO-PORTION
100112*    END-IF.
100112*    IF EN-ACCT-PORTION NUMERIC
100112*       MOVE EN-ACCT-PORTION TO WS-SRT-ACCT-PORTION
100112*    ELSE
100112*       MOVE ZERO            TO WS-SRT-ACCT-PORTION
100112*    END-IF
           MOVE EN-ACCT-SUMM       TO WS-SRT-ACCT-SUMM.
           MOVE EN-CSO-SUMM        TO WS-SRT-CSO-SUMM.

100112
100112     IF WRK-TOTAL-DUE-AMT <= ZERO
100112         GO TO 0300-EXIT
100112     END-IF.           
           
031516     IF W-ACCT-SUMM = '1' OR '2' or '5'
031813        MOVE W-ACCT-SUMM TO WS-SRT-RPT-TYPE
              RELEASE SORT-REC  FROM  WS-SORT-REC
              ADD 1 TO ACCT-RECS-OUT
           END-IF.

031813     IF W-CSO-SUMM = '3' OR '4'
031813        MOVE W-CSO-SUMM TO WS-SRT-RPT-TYPE
              RELEASE SORT-REC  FROM  WS-SORT-REC
              ADD 1 TO CSO-RECS-OUT
           END-IF.

           .

       0300-EXIT.
           EXIT.
           


       0350-BUILD-FROM-ARCH.
       
           SET WS-CERT-NOT-FOUND TO TRUE
           PERFORM 0700-READ-CERT THRU 0700-EXIT
           IF WS-CERT-NOT-FOUND
               DISPLAY 'CERT NOT FOUND - '
               LA-CERT-NO-A2 ' ' LA-ARCHIVE-NO ' ' LA-FORM-A3
               GO TO 0350-EXIT
           END-IF
031813
031813     IF WRK-INPUT-DATE = WRK-BEG-DATE OR
031813        WRK-INPUT-DATE = WRK-MOE-DATE
031813        IF (LA-FINAL-ACT-IND = 'B' OR 'C')
031813           IF (CM-LF-CANCEL-EXIT-DT = LOW-VALUES OR SPACES) AND
031813              (CM-AH-CANCEL-EXIT-DT = LOW-VALUES OR SPACES)
031813                  GO TO 0350-EXIT
031813           END-IF
031813           IF CM-LF-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES
031813              MOVE CM-LF-CANCEL-EXIT-DT TO DC-BIN-DATE-1
031803              MOVE ' '                 TO DC-OPTION-CODE
031813              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
031813              IF NO-CONVERSION-ERROR
031813                 IF DC-GREG-DATE-CYMD NOT = WRK-MOE-DATE
031813                     GO TO 0350-EXIT
031813                 END-IF
031813              END-IF
031813           END-IF
031813           IF CM-AH-CANCEL-EXIT-DT NOT = LOW-VALUES AND SPACES
031813              MOVE CM-AH-CANCEL-EXIT-DT TO DC-BIN-DATE-1
031803              MOVE ' '                 TO DC-OPTION-CODE
031813              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
031813              IF NO-CONVERSION-ERROR
031813                 IF DC-GREG-DATE-CYMD NOT = WRK-MOE-DATE
031813                     GO TO 0350-EXIT
031813                 END-IF
031813              END-IF
031813           END-IF
031813        END-IF
031813     END-IF
           
           MOVE LA-CARRIER-A2    TO  WS-SRT-CARR.
           MOVE LA-GROUPING-A2   TO  WS-SRT-GROUP.
011613*    MOVE LA-ACCOUNT-A2    TO  WS-SRT-ACCOUNT.
011613     MOVE WRK-FIN-ACCT-NUM TO  WS-SRT-ACCOUNT.
           MOVE WRK-FIN-RESP     TO  WS-SRT-RESP-NO.
           MOVE WRK-FIN-ACCT-NAME TO  WS-SRT-FIN-ACCT-NAME.
           MOVE WRK-FIN-MAIL-NAME TO  WS-SRT-FIN-MAIL-NAME.
           MOVE LA-CERT-NO-A2    TO  WS-SRT-CERT.

           MOVE LA-EFFECT-DATE-A2 TO DC-BIN-DATE-1.
           MOVE ' '              TO DC-OPTION-CODE.
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-EDIT TO WS-SRT-CERT-EFF-DT
           ELSE
               MOVE SPACES              TO WS-SRT-CERT-EFF-DT
           END-IF.
           
           MOVE CM-INSURED-LAST-NAME TO WS-SRT-INS-LAST-NAME
           MOVE +0                   TO WRK-LF-REPORTED-AMT
           MOVE CM-LF-ITD-CANCEL-AMT TO WRK-LF-CHANGED-AMT
           MOVE +0                   TO WRK-AH-REPORTED-AMT
           MOVE CM-AH-ITD-CANCEL-AMT TO WRK-AH-CHANGED-AMT
100112*     MOVE LA-FORM-A3           TO WS-SRT-COMMENT
100112     IF W-REFUND-REQUIRED = 'Y'
100112         MOVE 'Cancel'         TO WS-SRT-COMMENT
100112     ELSE
100112         MOVE 'Endorsed'       TO WS-SRT-COMMENT
100112     END-IF
           COMPUTE WRK-LF-NET-CHG-AMT = WRK-LF-CHANGED-AMT -
                                        WRK-LF-REPORTED-AMT
           COMPUTE WRK-AH-NET-CHG-AMT = WRK-AH-CHANGED-AMT -
                                        WRK-AH-REPORTED-AMT
           COMPUTE WRK-TOTAL-DUE-AMT  = WRK-LF-NET-CHG-AMT +
                                        WRK-AH-NET-CHG-AMT

           MOVE WRK-LF-REPORTED-AMT TO WS-SRT-LF-REPORTED-AMT.
           MOVE WRK-AH-REPORTED-AMT TO WS-SRT-AH-REPORTED-AMT.
           MOVE WRK-LF-CHANGED-AMT TO WS-SRT-LF-CHANGED-AMT.
           MOVE WRK-AH-CHANGED-AMT TO WS-SRT-AH-CHANGED-AMT.
           MOVE WRK-LF-NET-CHG-AMT TO WS-SRT-LF-NET-CHG-AMT.
           MOVE WRK-AH-NET-CHG-AMT TO WS-SRT-AH-NET-CHG-AMT.
           MOVE WRK-TOTAL-DUE-AMT  TO WS-SRT-TOTAL-DUE-AMT.
100112
100112     MOVE CM-CERT-EFF-DT      TO DC-BIN-DATE-1
100112     MOVE CM-AH-CANCEL-DT     TO DC-BIN-DATE-2
100112     IF CM-LF-CANCEL-DT > DC-BIN-DATE-2
100112        MOVE CM-LF-CANCEL-DT  TO DC-BIN-DATE-2
100112     END-IF
100112     MOVE '1'                 TO DC-OPTION-CODE
100112     PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT
100112     IF NO-CONVERSION-ERROR
100112        MOVE DC-ELAPSED-MONTHS TO WS-DIFF
100112        IF DC-ODD-DAYS-OVER > +1
100112           ADD +1          TO WS-DIFF
100112        END-IF
100112     ELSE
100112         MOVE +0           TO WS-DIFF
100112     END-IF
100112
100112     IF (WS-CHGBACK = 99)
100112         OR (WS-DIFF > WS-CHGBACK AND WS-CHGBACK > 0)
100112            MOVE WRK-TOTAL-DUE-AMT  TO WRK-CSO-PORTION
100112            MOVE ZERO               TO WRK-ACCT-PORTION
100112     ELSE
100112         COMPUTE WRK-ACCT-PORTION ROUNDED =
100112            (WRK-LF-NET-CHG-AMT * CM-LIFE-COMM-PCT) +
100112            (WRK-AH-NET-CHG-AMT * CM-AH-COMM-PCT)
100112         COMPUTE WRK-CSO-PORTION = 
100112            WRK-TOTAL-DUE-AMT - WRK-ACCT-PORTION
100112     END-IF
              
           MOVE WRK-CSO-PORTION    TO WS-SRT-CSO-PORTION
           MOVE WRK-ACCT-PORTION   TO WS-SRT-ACCT-PORTION
           MOVE W-ACCT-SUMM        TO WS-SRT-ACCT-SUMM.
           MOVE W-CSO-SUMM         TO WS-SRT-CSO-SUMM.
           
100112
100112     IF WRK-TOTAL-DUE-AMT <= ZERO
100112         GO TO 0350-EXIT
100112     END-IF.           

031516     IF W-ACCT-SUMM = '1' OR '2' or '5'
              MOVE W-ACCT-SUMM TO WS-SRT-RPT-TYPE
              RELEASE SORT-REC  FROM  WS-SORT-REC
              ADD 1 TO ACCT-RECS-OUT
           END-IF.

           IF W-CSO-SUMM = '3' OR '4'
              MOVE W-CSO-SUMM TO WS-SRT-RPT-TYPE
              RELEASE SORT-REC  FROM  WS-SORT-REC
              ADD 1 TO CSO-RECS-OUT
           END-IF.

           .

       0350-EXIT.
           EXIT.
           


       0380-GET-ERACCT.           

           MOVE LOW-VALUES         TO  AM-CONTROL-PRIMARY
                                       WRK-ACCT-CONTROL-PART.
           MOVE SPACES             TO  WRK-FIN-RESP
011613                                 WRK-FIN-ACCT-NUM
                                       WRK-FIN-ACCT-NAME
                                       WRK-FIN-MAIL-NAME.
100112     MOVE ZEROS              TO  WS-CHGBACK
100112                                 WS-DIFF
                                       
           IF ENDORSEMENT-FOUND                                       
               MOVE EN-COMPANY-CD  TO  WRK-ACCT-COMPANY-CD
               MOVE EN-CARRIER     TO  WRK-ACCT-CARRIER
               MOVE EN-GROUPING    TO  WRK-ACCT-GROUPING
               MOVE EN-STATE       TO  WRK-ACCT-STATE
               MOVE EN-ACCOUNT     TO  WRK-ACCT-ACCOUNT
011613                                 WRK-FIN-ACCT-NUM
               MOVE EN-CERT-EFF-DT TO  AM-EXPIRATION-DT
           ELSE
               MOVE LA-COMPANY-CD-A2  TO  WRK-ACCT-COMPANY-CD
               MOVE LA-CARRIER-A2     TO  WRK-ACCT-CARRIER
               MOVE LA-GROUPING-A2    TO  WRK-ACCT-GROUPING
               MOVE LA-STATE-A2       TO  WRK-ACCT-STATE
               MOVE LA-ACCOUNT-A2     TO  WRK-ACCT-ACCOUNT
011613                                    WRK-FIN-ACCT-NUM
               MOVE LA-EFFECT-DATE-A2 TO  AM-EXPIRATION-DT
           END-IF
           MOVE WRK-ACCT-CONTROL-PART TO AM-CONTROL-PRIMARY (1:20)
           START ERACCT KEY >= AM-CONTROL-PRIMARY
           IF ERACCT-FILE-STATUS = '00'
              READ ERACCT NEXT RECORD
              IF ERACCT-FILE-STATUS = '00'
                 IF WRK-ACCT-CONTROL-PART =
                    AM-CONTROL-PRIMARY (1:20)
                     MOVE AM-AGT (AM-REMIT-TO) TO WRK-FIN-RESP
                 ELSE
                     GO TO 0380-EXIT
                 END-IF
              ELSE
                 GO TO 0380-EXIT
              END-IF
           ELSE
              GO TO 0380-EXIT
           END-IF.
100112
100112     PERFORM VARYING S1 FROM +1 BY +1 UNTIL
100112        (AM-COM-TYP (S1) = 'C' OR 'D')
100112        OR (S1 > +10)
100112     END-PERFORM
100112     IF S1 < +11
031516        if am-comm-chargeback (s1) not numeric
031516           move zeros            to am-comm-chargeback (s1)
031516        end-if
100112        MOVE AM-COMM-CHARGEBACK (S1) TO WS-CHGBACK
011613        MOVE AM-AGT (S1)             TO WRK-FIN-ACCT-NUM
100112     END-IF
           
           MOVE AM-COMPANY-CD      TO  WRK-COMP-COMPANY-CD.
           MOVE AM-CARRIER         TO  WRK-COMP-CARRIER.
           MOVE AM-GROUPING        TO  WRK-COMP-GROUPING.
           MOVE WRK-FIN-RESP       TO  WRK-COMP-RESP-NO.
011613*    MOVE AM-ACCOUNT         TO  WRK-COMP-ACCOUNT.
011613     MOVE WRK-FIN-ACCT-NUM   TO  WRK-COMP-ACCOUNT.
           MOVE 'A'                TO  WRK-COMP-TYPE.
           
           MOVE WRK-COMP-CONTROL TO CO-CONTROL-PRIMARY.
           
           READ ERCOMP
           
           IF ERCOMP-FILE-STATUS = '00'
               MOVE CO-ACCT-NAME  TO WRK-FIN-ACCT-NAME
               MOVE CO-MAIL-NAME  TO WRK-FIN-MAIL-NAME
           END-IF.

       0380-EXIT.
           EXIT.



       0400-OPEN-FILES.

           OPEN INPUT ERARCH ELLETR ERENDT2 ELCERT ERACCT ERCOMP
031516         OUTPUT ACCT-SUM CSO-SUM *>TEMP-OUT

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY 'ERARCH open err  ' ERARCH-FILE-STATUS
              MOVE ' ERARCH OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERENDT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERENDT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERENDT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCERT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERACCT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERACCT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERACCT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERCOMP-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERCOMP OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERCOMP-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           .

       0400-EXIT.
           EXIT.

       0500-CLOSE-FILES.

           CLOSE ERARCH ELLETR ERENDT2 ELCERT ERACCT ERCOMP 
031516           ACCT-SUM CSO-SUM *>TEMP-OUT
           
           .

       0500-EXIT.
           EXIT.

       0550-START-ERARCH.


           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY
           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD

           START ERARCH KEY IS NOT < LA-CONTROL-PRIMARY

           IF ERARCH-FILE-STATUS = '10' OR '23'
              SET END-OF-ERARCH        TO TRUE
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY 'ERARCH START     ' ERARCH-FILE-STATUS
                 SET END-OF-ERARCH     TO TRUE
              END-IF
           END-IF

           .

       0550-EXIT.
           EXIT.


       0600-READ-ERENDT.

           READ ERENDT2
           
           IF ERENDT-FILE-STATUS = '00'
               SET ENDORSEMENT-FOUND TO TRUE
           END-IF.
           
       0600-EXIT.
           EXIT.

       0700-READ-CERT.

           MOVE LA-COMPANY-CD          TO CM-COMPANY-CD
           MOVE LA-CARRIER-A2          TO CM-CARRIER
           MOVE LA-GROUPING-A2         TO CM-GROUPING
           MOVE LA-STATE-A2            TO CM-STATE
           MOVE LA-ACCOUNT-A2          TO CM-ACCOUNT
           MOVE LA-EFFECT-DATE-A2      TO CM-CERT-EFF-DT
           MOVE LA-CERT-NO-A2          TO CM-CERT-NO

           READ ELCERT
           IF ELCERT-FILE-STATUS = '00'
              MOVE 'Y'                 TO WS-CERT-FOUND-SW
           ELSE
              DISPLAY ' ERROR ON ELCERT - READ - 0700 '
                 ELCERT-FILE-STATUS
           END-IF.

       0700-EXIT.
           EXIT.

           
       0750-GET-Z-RECORD.

           INITIALIZE W-Z-CONTROL-DATA
           
           MOVE LOW-VALUES             TO TX-CONTROL-PRIMARY.
           MOVE DTE-CLASIC-COMPANY-CD  TO TX-COMPANY-CD.
           MOVE LA-FORM-A3             TO TX-LETTER-NO.
           
           START ELLETR KEY IS NOT LESS THAN TX-CONTROL-PRIMARY.

           IF ELLETR-FILE-STATUS NOT = '00'
              MOVE ' ELLETR START ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
           
       0750-READ-NEXT.
       
           READ ELLETR NEXT RECORD.

           IF ELLETR-FILE-STATUS NOT = '00'
              MOVE ' ELLETR READ  ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF (TX-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD) OR
              (TX-LETTER-NO NOT = LA-FORM-A3)
                 DISPLAY 'LETTER Z RECORD NOT FOUND  - ' 
                                LA-FORM-A3
               GO TO 0750-EXIT                 
           END-IF.

           IF TX-LINE-SQUEEZE-CONTROL NOT EQUAL 'Z'
               GO TO 0750-READ-NEXT
           END-IF.
           
           MOVE TX-TEXT-LINE   TO W-Z-CONTROL-DATA.
                                                  
       0750-EXIT.
           EXIT.


       1000-OUTPUT-ROUTINE SECTION.
       
           MOVE 'N'  TO  WS-RPT-1-SW
           MOVE 'Y'  TO  WS-RPT-2-SW
031516     move 'Y'  TO  WS-RPT-5-SW
           MOVE 'N'  TO  WS-RPT-3-SW
           MOVE 'Y'  TO  WS-RPT-4-SW
           PERFORM 1200-RETURN-SORT-RECS THRU 1200-EXIT
               UNTIL  END-OF-SORT.

       1000-EXIT.
           EXIT.

       1200-RETURN-SORT-RECS.

           RETURN SORT-WORK  INTO  WS-SORT-REC  AT END      
               SET END-OF-SORT TO TRUE
               GO TO 1200-EXIT.

031516*    WRITE TEMP-OUT-REC FROM WS-SORT-REC

031516     IF (WS-SRT-RPT-TYPE = '1' OR '2' OR '5') AND
             (WS-SRT-ACCOUNT NOT = PREV-SRT-ACCT-ACCT-NO)
               IF NOT RPT-2-WRITTEN
                   PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
                                                 THRU 7200-EXIT
                   MOVE NONE-LINE       TO PRINT-LINE
                   PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412             ADD +1               TO WRK-ACCT-LINE-COUNT
               END-IF
031516         IF NOT RPT-5-WRITTEN
031516             PERFORM 7210-PRINT-REC-TYPE-5-HEADINGS 
031516                                           THRU 7210-EXIT
031516             MOVE NONE-LINE       TO PRINT-LINE
031516             PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516             ADD +1               TO WRK-ACCT-LINE-COUNT
031516         END-IF
102412         IF PREV-SRT-ACCT-ACCT-NO NOT = SPACES
102412             MOVE WRK-ACCT-LINE-COUNT TO WRK-LINE-COUNT
102412             MOVE +0                  TO WRK-ACCT-LINE-COUNT
102412             PERFORM 7500-PRINT-FOOTER THRU 7500-EXIT
102412         END-IF
               MOVE 'N'  TO  WS-RPT-1-SW  
                             WS-RPT-2-SW
031516                       WS-RPT-5-SW
               MOVE WS-SRT-ACCOUNT TO PREV-SRT-ACCT-ACCT-NO
               MOVE WS-SRT-RPT-TYPE TO PREV-SRT-RPT-TYPE
               PERFORM 7100-PRINT-REC-TYPE-1-HEADINGS THRU 7100-EXIT
               IF WS-SRT-RPT-TYPE = '2'
                   MOVE NONE-LINE       TO PRINT-LINE
                   PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412             ADD +1               TO WRK-ACCT-LINE-COUNT
                   PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
                                                 THRU 7200-EXIT
               END-IF
031516         IF WS-SRT-RPT-TYPE = '5'
031516             MOVE NONE-LINE       TO PRINT-LINE
031516             PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516             ADD +1               TO WRK-ACCT-LINE-COUNT
031516             PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
031516                                           THRU 7200-EXIT
031516             MOVE NONE-LINE       TO PRINT-LINE
031516             PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516             ADD +1               TO WRK-ACCT-LINE-COUNT
031516             PERFORM 7210-PRINT-REC-TYPE-5-HEADINGS 
031516                                           THRU 7210-EXIT
031516         END-IF
               PERFORM 3000-PROCESS-RECORD THRU 3000-EXIT
               GO TO 1200-EXIT
           END-IF.

           IF (WS-SRT-RPT-TYPE = '3' OR '4') AND
             (WS-SRT-ACCOUNT NOT = PREV-SRT-CSO-ACCT-NO)
               IF NOT RPT-4-WRITTEN
                   PERFORM 7400-PRINT-REC-TYPE-4-HEADINGS 
                                                 THRU 7400-EXIT
                   MOVE NONE-LINE       TO PRINT-LINE
                   PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412             ADD +1               TO WRK-CSO-LINE-COUNT
               END-IF
102412*         IF PREV-SRT-CSO-ACCT-NO NOT = SPACES
102412*             MOVE WRK-CSO-LINE-COUNT TO WRK-LINE-COUNT
102412*             MOVE +0                 TO WRK-CSO-LINE-COUNT
102412*             PERFORM 7500-PRINT-FOOTER THRU 7500-EXIT
102412*         END-IF
               MOVE 'N'  TO  WS-RPT-3-SW  
                             WS-RPT-4-SW
               MOVE WS-SRT-ACCOUNT TO PREV-SRT-CSO-ACCT-NO
               MOVE WS-SRT-RPT-TYPE TO PREV-SRT-RPT-TYPE
               PERFORM 7300-PRINT-REC-TYPE-3-HEADINGS THRU 7300-EXIT
               IF WS-SRT-RPT-TYPE = '4'
                   MOVE NONE-LINE       TO PRINT-LINE
                   PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412             ADD +1               TO WRK-CSO-LINE-COUNT
                   PERFORM 7400-PRINT-REC-TYPE-4-HEADINGS 
                                                 THRU 7400-EXIT
               END-IF
               PERFORM 3000-PROCESS-RECORD THRU 3000-EXIT
               GO TO 1200-EXIT
           END-IF.

           IF WS-SRT-RPT-TYPE NOT = PREV-SRT-RPT-TYPE
               MOVE WS-SRT-RPT-TYPE TO PREV-SRT-RPT-TYPE
031516         if ws-srt-rpt-type = '5'
031516            IF NOT RPT-2-WRITTEN
031516               PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
031516                                     THRU 7200-EXIT
031516               MOVE NONE-LINE       TO PRINT-LINE
031516               PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516               ADD +1               TO WRK-ACCT-LINE-COUNT
031516            END-IF
031516            PERFORM 7210-PRINT-REC-TYPE-5-HEADINGS 
031516                                          THRU 7210-EXIT
031516         else
               IF WS-SRT-RPT-TYPE = '2'
                   PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
                                                THRU 7200-EXIT
               ELSE
                   PERFORM 7400-PRINT-REC-TYPE-4-HEADINGS 
                                                 THRU 7400-EXIT
               END-IF
               PERFORM 3000-PROCESS-RECORD THRU 3000-EXIT
               GO TO 1200-EXIT
           END-IF.
           
           PERFORM 3000-PROCESS-RECORD THRU 3000-EXIT.
               

       1200-EXIT.
           EXIT.
           
       3000-PROCESS-RECORD.
       
031516     IF ((WS-SRT-RPT-TYPE = '1' OR '2' OR '5') AND
102412      (WRK-ACCT-LINE-COUNT > WRK-MAX-LINES))
102412         EVALUATE TRUE
102412           WHEN WS-SRT-RPT-TYPE = '1'
102412             PERFORM 7100-PRINT-REC-TYPE-1-HEADINGS 
102412                             THRU 7100-EXIT
102412           WHEN WS-SRT-RPT-TYPE = '2'
102412             PERFORM 7200-PRINT-REC-TYPE-2-HEADINGS 
102412                             THRU 7200-EXIT
031516           WHEN WS-SRT-RPT-TYPE = '5'
031516             PERFORM 7210-PRINT-REC-TYPE-5-HEADINGS 
031516                             THRU 7210-EXIT
102412         END-EVALUATE
102412     END-IF
102412
102412     IF ((WS-SRT-RPT-TYPE = '3' OR '4') AND
102412      (WRK-CSO-LINE-COUNT > WRK-MAX-LINES))
102412         EVALUATE TRUE
102412           WHEN WS-SRT-RPT-TYPE = '3'
102412             PERFORM 7300-PRINT-REC-TYPE-3-HEADINGS 
102412                             THRU 7300-EXIT
102412           WHEN WS-SRT-RPT-TYPE = '4'
102412             PERFORM 7400-PRINT-REC-TYPE-4-HEADINGS 
102412                             THRU 7400-EXIT
102412         END-EVALUATE
102412     END-IF                   
       
           MOVE WS-SRT-CERT            TO DET-CERT-NO        
           MOVE WS-SRT-INS-LAST-NAME   TO DET-INS-LAST-NAME  
           MOVE WS-SRT-CERT-EFF-DT     TO DET-CERT-EFF-DT    
           MOVE WS-SRT-LF-REPORTED-AMT TO DET-LF-REPORTED-AMT
           MOVE WS-SRT-LF-CHANGED-AMT  TO DET-LF-CHANGED-AMT 
           MOVE WS-SRT-LF-NET-CHG-AMT  TO DET-LF-NET-CHG-AMT 
           MOVE WS-SRT-AH-REPORTED-AMT TO DET-AH-REPORTED-AMT
           MOVE WS-SRT-AH-CHANGED-AMT  TO DET-AH-CHANGED-AMT 
           MOVE WS-SRT-AH-NET-CHG-AMT  TO DET-AH-NET-CHG-AMT 
           MOVE WS-SRT-TOTAL-DUE-AMT   TO DET-TOTAL-DUE-AMT  
           MOVE WS-SRT-ACCT-PORTION    TO DET-ACCT-PORTION   
           MOVE WS-SRT-CSO-PORTION     TO DET-CSO-PORTION
           MOVE WS-SRT-COMMENT         TO DET-COMMENT        
       
           MOVE DETAIL-LINE            TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     IF WS-SRT-RPT-TYPE = '1' OR '2' or '5'
102412         ADD +1                  TO WRK-ACCT-LINE-COUNT
102412     ELSE
102412         ADD +1                  TO WRK-CSO-LINE-COUNT
102412     END-IF

           .
       3000-EXIT.
           EXIT.

       7100-PRINT-REC-TYPE-1-HEADINGS.
102412     IF WRK-ACCT-LINE-COUNT > WRK-SECTION-MAX
102412         MOVE WRK-ACCT-LINE-COUNT TO WRK-LINE-COUNT
102412         MOVE +0                  TO WRK-ACCT-LINE-COUNT
102412         PERFORM 7500-PRINT-FOOTER  THRU 7500-EXIT
           END-IF
           PERFORM 7120-PAGE-HEADINGS THRU 7120-EXIT
031516     move hd5-action-1 to hd5a-action
           MOVE HD5-ACCT-1 TO HD5A-TITLE
           MOVE 'FOR REVIEW ONLY' TO HD6-REVIEW
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD6       TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7B      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7C      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7D      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE 'Y'       TO WS-RPT-1-SW
031516     MOVE +20       TO WRK-ACCT-LINE-COUNT

           .
       7100-EXIT.
           EXIT.    



       7120-PAGE-HEADINGS.
102412     MOVE +0 TO WRK-ACCT-LINE-COUNT
           ADD +1         TO WRK-ACCT-PAGE
           MOVE WRK-ACCT-PAGE TO HD3-PG
           MOVE HD1-ACCT  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD2-ACCT  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD3-ACCT  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE 'ACCOUNT NO. - ' TO HD4-M1
           MOVE WS-SRT-CARR-GROUP TO HD4-CARR-GROUP
           MOVE WS-SRT-ACCOUNT TO HD4-ACCOUNT
           MOVE WS-SRT-RESP-NO TO HD4-RESP
           MOVE WS-SRT-FIN-ACCT-NAME TO HD4-REMIT
           MOVE HD4-ACCT  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD4A-ACCT-1  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD4A-ACCT-2  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD4A-ACCT-3  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD4A-ACCT-4  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD4A-ACCT-5  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD4A-ACCT-6  TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
          .
       7120-EXIT.
           EXIT.    

       7200-PRINT-REC-TYPE-2-HEADINGS.
102412     IF WRK-ACCT-LINE-COUNT > WRK-SECTION-MAX
102412         MOVE WRK-ACCT-LINE-COUNT TO WRK-LINE-COUNT
102412         MOVE +0                  TO WRK-ACCT-LINE-COUNT
102412         PERFORM 7500-PRINT-FOOTER  THRU 7500-EXIT
               PERFORM 7120-PAGE-HEADINGS THRU 7120-EXIT
102412         MOVE +11                 TO WRK-ACCT-LINE-COUNT
           END-IF
           MOVE SPACES          TO WS-ADD-LINES-SW
           MOVE SPACES          TO PRINT-LINE
           MOVE '-'             TO PRT-CTRL
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     move hd5-action-1 to hd5a-action
           MOVE HD5-ACCT-2 TO HD5A-TITLE
           MOVE 'FOR REVIEW ONLY' TO HD6-REVIEW
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD6       TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412     MOVE HD7A2     TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412     MOVE HD7B2     TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7C      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7D      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE 'Y'       TO WS-RPT-2-SW
102412     ADD +11        TO WRK-ACCT-LINE-COUNT

           .
       7200-EXIT.
           EXIT.    

031516 7210-PRINT-REC-TYPE-5-HEADINGS.
031516
031516     IF WRK-ACCT-LINE-COUNT > WRK-SECTION-MAX
031516         MOVE WRK-ACCT-LINE-COUNT TO WRK-LINE-COUNT
031516         MOVE +0                  TO WRK-ACCT-LINE-COUNT
031516         PERFORM 7500-PRINT-FOOTER  THRU 7500-EXIT
031516         PERFORM 7120-PAGE-HEADINGS THRU 7120-EXIT
031516         MOVE +11                 TO WRK-ACCT-LINE-COUNT
031516     END-IF
031516     MOVE SPACES          TO WS-ADD-LINES-SW
031516     MOVE SPACES          TO PRINT-LINE
031516     MOVE '-'             TO PRT-CTRL
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     move hd5-action-2 to hd5a-action
031516     MOVE HD5-ACCT-5 TO HD5A-TITLE
031516     MOVE 'FOR REVIEW ONLY' TO HD6-REVIEW
031516     MOVE HD5-DASH  TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD5A      TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD5-DASH  TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD6       TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD7A3     TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD7B3     TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD7C3     TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE HD7D      TO PRINT-LINE
031516     PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     MOVE 'Y'       TO WS-RPT-5-SW
031516     ADD +11        TO WRK-ACCT-LINE-COUNT
031516
031516     .
031516 7210-EXIT.
031516     EXIT.    
           
       7300-PRINT-REC-TYPE-3-HEADINGS.
102412*     IF WRK-CSO-LINE-COUNT > WRK-SECTION-MAX
102412*         MOVE WRK-CSO-LINE-COUNT TO WRK-LINE-COUNT
102412*         MOVE +0                 TO WRK-CSO-LINE-COUNT
102412*         PERFORM 7500-PRINT-FOOTER  THRU 7500-EXIT
102412*     END-IF
           PERFORM 7340-PAGE-HEADINGS THRU 7340-EXIT
031516     move spaces       to hd5a-action
           MOVE HD5-CSO-3 TO HD5A-TITLE
           MOVE 'FOR REVIEW ONLY' TO HD6-REVIEW
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD6       TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7B      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7C      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7D      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE 'Y'       TO WS-RPT-3-SW
102412     MOVE +13       TO WRK-CSO-LINE-COUNT

           .
       7300-EXIT.
           EXIT.    



       7340-PAGE-HEADINGS.
102412     MOVE +0 TO WRK-CSO-LINE-COUNT
           ADD +1         TO WRK-CSO-PAGE
           MOVE WRK-CSO-PAGE TO HD3R-PG
           MOVE HD1-CSO   TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD2-CSO   TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD3-CSO   TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE 'ACCOUNT NO. - ' TO HD4R-M1
           MOVE WS-SRT-CARR-GROUP TO HD4R-CARR-GROUP
           MOVE WS-SRT-ACCOUNT TO HD4R-ACCT
           MOVE WS-SRT-RESP-NO TO HD4R-RESP
           MOVE WS-SRT-FIN-ACCT-NAME TO HD4R-REMIT
           MOVE HD4-CSO   TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           .
       7340-EXIT.
           EXIT.    

       7400-PRINT-REC-TYPE-4-HEADINGS.
102412     IF WRK-CSO-LINE-COUNT > WRK-SECTION-MAX
102412*         MOVE WRK-CSO-LINE-COUNT TO WRK-LINE-COUNT
102412*         MOVE +0                 TO WRK-CSO-LINE-COUNT
102412*         PERFORM 7500-PRINT-FOOTER  THRU 7500-EXIT
               PERFORM 7340-PAGE-HEADINGS THRU 7340-EXIT
           END-IF
           MOVE SPACES          TO WS-ADD-LINES-SW
           MOVE SPACES          TO PRINT-LINE
           MOVE '-'             TO PRT-CTRL
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
031516     move spaces       to hd5a-action
           MOVE HD5-CSO-4 TO HD5A-TITLE
           MOVE 'FOR REVIEW ONLY' TO HD6-REVIEW
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD5-DASH  TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD6       TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7A      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7B      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7C      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE HD7D      TO PRINT-LINE
           PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
           MOVE 'Y'       TO WS-RPT-4-SW
102412     ADD +13        TO WRK-CSO-LINE-COUNT

           .
       7400-EXIT.
           EXIT.    

102412 7500-PRINT-FOOTER.
102412
102412    PERFORM UNTIL WRK-LINE-COUNT > WRK-MAX-LINES
102412       MOVE SPACES       TO PRINT-LINE
102412       PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT
102412       ADD +1            TO WRK-LINE-COUNT
102412    END-PERFORM.
102412
102412    MOVE FOOTER-LINE     TO PRINT-LINE.
102412    PERFORM 8000-WRITE-SUMMARY THRU 8000-EXIT.
102412
102412      .
102412 7500-EXIT.
102412     EXIT.
           
       8000-WRITE-SUMMARY.

031516     IF WS-SRT-RPT-TYPE = '1' OR '2' or '5'
               WRITE ACCTSUM-RECORD FROM PRINT-LINE
           ELSE
               WRITE CSOSUM-RECORD FROM PRINT-LINE
           END-IF
           
           .
       8000-EXIT.
           EXIT.    
           
       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.

       ABEND-PGM.   COPY ELCABEND.