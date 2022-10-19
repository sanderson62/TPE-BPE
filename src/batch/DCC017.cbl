      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.                                         
       PROGRAM-ID.          DCC017.                                     
      *AUTHOR.        Cowtown.
      *               Omaha, NE.
      *DATE-COMPILED.                                                   
      *SECURITY.   *****************************************************
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF CSO         IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *****************************************************
      *REMARKS.                                                         
      *        CREATE STANDARD COMMISSION TRANSACTIONS                  
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 011116  CR2015082400003  PEMA  NEW PROGRAM
052516* 052516  IR2016052000002  PEMA  Fix clp calculation on refunds
060616* 060616  IR2016060100002  PEMA  Fix access to prod def code.
032922* 032922  CR2021100800003  PEMA  Increase number of Prod Defs to 11
063022* 063022  CR2019012500003  PEMA  Migrate DB's to SQLSERVER 2016
      ******************************************************************

       ENVIRONMENT DIVISION.                                            
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.                                                    
      
           SELECT END-PRINT        ASSIGN TO SYS008.
           SELECT EXTR-FILE        ASSIGN TO SYS014.
           SELECT EP-EXTR          ASSIGN TO SYS018.
           SELECT ERACCTT          ASSIGN TO ERACCTT
                                   ORGANIZATION IS INDEXED              
                                   ACCESS IS SEQUENTIAL                 
                                   RECORD KEY IS AM-CONTROL-PRIMARY     
                                   FILE STATUS IS ERACCTT-FILE-STATUS.  
      
           SELECT ERPDEF           ASSIGN TO ERPDEF
                                   ACCESS IS DYNAMIC                    
                                   ORGANIZATION IS INDEXED              
                                   FILE STATUS IS ERPDEF-FILE-STATUS    
                                   RECORD KEY IS PD-CONTROL-PRIMARY.    
      
           SELECT DISK-DATE        ASSIGN TO SYS019.
           SELECT FICH             ASSIGN TO SYS020.
      
       DATA DIVISION.                                                   
       FILE SECTION.                                                    

       FD  END-PRINT                                                    
                                       COPY ELCPRTFD.                       

       FD  EXTR-FILE                                                    
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.                                            
       01  EXTR-RECORD             PIC X(91).
                                                                        
       FD  ERACCTT.                                                     
                                                                        
                                   COPY ERCACCT.                        
       FD  EP-EXTR                                                      
                                   COPY ECSEXTFD.                       
                                                                        
                                   COPY ECSEXT01.                       
      
       FD  ERPDEF.
      
                                   COPY ERCPDEF.
      
       FD  DISK-DATE                                                    
                                   COPY ELCDTEFD.                       
                                                                        
       FD  FICH                                                         
                                   COPY ELCFCHFD.                       
       WORKING-STORAGE SECTION.                                         

       EXEC SQL
          INCLUDE SQLCA
       END-EXEC

       77  FILLER PIC X(32) VALUE '********************************'.   
       77  FILLER PIC X(32) VALUE '*   DCC017 WORKING STORAGE     *'.   
       77  FILLER PIC X(32) VALUE '********** VMOD=2.030 **********'.   
                                                                        
       77  ACCT-SW                 PIC S9       COMP-3 VALUE +0.        
       77  NDX                     PIC S999     COMP-3 VALUE +0.        
       77  WS-SAVE-NDX             PIC S999     COMP-3 VALUE +0.
       77  RV-NDX                  PIC S999     COMP-3 VALUE +0.        
       77  PGM-SUB                 PIC S999     COMP-3 VALUE +017.      
       77  PRCM-LF-OW-COMM         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-LF-OW-SF           PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-AH-OW-COMM         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-AH-OW-SF           PIC S9(7)V99 COMP-3 VALUE +0.        
       77  PRCM-DLR-INC            PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-LF-LMBA-FEE        PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-AH-LMBA-FEE        PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-BANK-FEE           PIC S9(7)V99 COMP-3 VALUE +0.
       77  PRCM-CSO-ADMIN          PIC S9(7)V99 COMP-3 VALUE +0.
       77  RECALC-LF-OW-COMM       PIC S9(7)V99 COMP-3 VALUE +0.        
       77  RECALC-LF-OW-SF         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  RECALC-AH-OW-COMM       PIC S9(7)V99 COMP-3 VALUE +0.        
       77  RECALC-AH-OW-SF         PIC S9(7)V99 COMP-3 VALUE +0.        
       77  CNC-FACT                PIC S9(3)V9(7) COMP-3 VALUE +0.
       77  WS-WORK-REF             PIC S9(9)V99 COMP-3 VALUE +0.
       77  WS-WORK-fee             PIC S9(7)V99 COMP-3 VALUE +0.
       77  X                       PIC X               VALUE SPACE.     
       77  a1                      PIC S999 COMP-3 VALUE +0.
       77  S1                      PIC S999 COMP-3 VALUE +0.
      
       77  WS-DUP-AGT-SW           PIC X  VALUE ' '.
           88  DUP-AGENT                VALUE 'Y'.
       77  WS-DONE-SW              PIC X  VALUE ' '.
           88  ALREADY-DONE             VALUE 'Y'.
       77  B-SUB                   PIC S9(4)   COMP.
       77  D1                      PIC S9(5) COMP-3 VALUE +0.
       77  P1                      PIC S999 COMP-3 VALUE +0.
       77  P2                      PIC S999 COMP-3 VALUE +0.
       77  CNC-WK                  PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-FACT-CERT            PIC X(11)   VALUE SPACES.
       77  WS-MONTH                PIC S999     COMP-3 VALUE +0.
       77  WS-HI-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-LO-UEF               PIC S9V999   COMP-3 VALUE +0.
       77  WS-CLP-MO3              PIC S9(7)V99 COMP-3 VALUE +0.
       77  WS-COMM-MO3             PIC S9(7)V99 COMP-3 VALUE +0.
       77  DD-IU-SW                    PIC X   VALUE ' '.
           88  DD-IU-PRESENT                 VALUE 'Y'.
       77  W-FACTOR            PIC S9(09)V9(08) COMP-3 VALUE +0.
       77  WS-DDF-TERM             PIC S999     COMP-3 VALUE +0.
       77  WS-WORK-REF                 PIC S9(9)V99 COMP-3 VALUE +0.
       77  ws-eof-sw                   pic x value ' '.
           88  end-of-input               value 'Y'.
       77  ws-recs-in                  pic 9(7) value zeros.
       77  rec-cnt                     pic 9(9) value zeros.
       77  ws-sql-code                 pic s9(7) value zeros.
       77  ws-dis-sql-code             pic -9999999 value zeros.

       01  P pointer.
       01  KIXSYS                      pic X(7)  VALUE Z"KIXSYS".
       01  var-ptr pointer.
       01  env-var-len                 pic 9(4)  binary.
       01  rc                          pic 9(9)  binary.
      
       01  WS-KIXSYS.
           05  WS-KIX-FIL1             PIC X(10).
           05  WS-KIX-APPS             PIC X(10).
           05  WS-KIX-ENV              PIC X(10).
           05  WS-KIX-MYENV            PIC X(10).
           05  WS-KIX-SYS              PIC X(10).

       01  WS-lf-cnc-fact            COMP-3 PIC S9(03)V9(07) VALUE +0.
       01  WS-ah-cnc-fact            COMP-3 PIC S9(03)V9(07) VALUE +0.
       01  WS-DCC-PRODUCT-CODE         PIC XXX  VALUE SPACES.

       01  ws-prev-lf-ben              pic xx value spaces.
       01  ws-prev-ah-ben              pic xx value spaces.
       01  ws-prev-clp-state           pic xx value spaces.
       01  ws-prev-cntrl1.
           05  ws-prev-carrier         pic x     value spaces.
           05  ws-prev-group           pic x(6)  value spaces.
           05  ws-prev-state           pic xx    value spaces.
           05  ws-prev-account         pic x(10) value spaces.
       01  ws-prev-cntrl2.
           05  ws-prev-eff-dt          pic 9(11) comp-3 value zeros.
           05  ws-prev-cert-no         pic x(11) value spaces.
       01  ws-work-date-n              pic 9(8) value zeros.
       01  ws-work-date redefines ws-work-date-n.
           05  ws-wd-ccyy              pic x(4).
           05  ws-wd-mm                pic xx.
           05  ws-wd-dd                pic xx.
       01  BINARY-WORK-AREA    COMP    SYNC.                            
           12  X1                  PIC S999            VALUE +0.        
           12  X4                  PIC S999            VALUE +0.        
           12  B1                  PIC S999            VALUE +1.        
           12  B2                  PIC S999            VALUE +2.        
           12  FAL                 PIC S999            VALUE +0.        
           12  AGTNDX              PIC S999            VALUE +0.        
                                                                        
       01  WS.                                                          
           12  WS-RETURN-CODE         PIC S9(4) COMP.                   
           12  WS-ABEND-MESSAGE       PIC X(80).                        
           12  WS-ABEND-FILE-STATUS   PIC XX  VALUE ZEROS.              
           12  WS-ZERO                PIC S9  VALUE ZERO COMP-3.        
           12  WS-DTE-PGM-OPT         PIC 9   VALUE 0.                  
           12  ERACCTT-FILE-STATUS    PIC XX  VALUE ZEROS.              
           12  ERCOMP-FILE-STATUS     PIC XX  VALUE '00'.
           12  ERPDEF-FILE-STATUS     PIC XX VALUE ZEROS.
           12  B-CNT                  PIC S999 VALUE +0 COMP-3.
                                                                        
       01  WS-RUN-DATE-R.                                               
           12  WS-RUN-YR              PIC 99.                           
           12  WS-RUN-MO              PIC 99.                           
           12  WS-RUN-DA              PIC 99.                           
                                                                        
       01  ERROR-MESSAGES.                                              
           12  ER-0504             PIC X(4)            VALUE '0504'.    
           12  ER-0514             PIC X(4)            VALUE '0514'.    
                                                                        
       01  WS-ERCOMP-KEY.
           12  WS-ERCOMP-COMP-CD   PIC X.
           12  WS-ERCOMP-CARRIER   PIC X.
           12  WS-ERCOMP-GROUPING  PIC X(6).
           12  WS-ERCOMP-FIN-RESP  PIC X(10).
           12  WS-ERCOMP-ACCOUNT   PIC X(10).
           12  WS-ERCOMP-TYPE      PIC X.
           
       01  MISC-WORK-AREA.                                              
           12  STRIP-SIGN          PIC 9V9(5).                          
           12  OTHER-WORK.                                              
               16  WK-CTL.                                              
                   20  W-CARRIER   PIC X.                               
                   20  W-GROUPING  PIC X(6).                            
                   20  W-ST        PIC XX.                              
                   20  W-ACCT      PIC X(10).                           
                   20  W-DATE      PIC 9(11)    COMP-3.                 
               16  SV-ST           PIC XX.                              
                                                                        
           12  MONTHS-DIFF-LF      PIC S9(5).                           
           12  MONTHS-DIFF-AH      PIC S9(5).                           
                                                                        
           12  SAVE-NCL-REGION     PIC X(6).                            
           12  SAVE-NCL-POOL-CODE  PIC XXX.                             
                                                                        
           12  DMD-CERT-NUMBER.                                         
               16  DMD-RESIDENT-STATE PIC XX.                           
               16  DMD-SYSTEM-ID      PIC X.                            
               16  DMD-COV-CATEGORY   PIC X.                            
               16  DMD-BENEFIT-CODE   PIC XX.                           
               16  DMD-COVERAGE-RULE  PIC X.                            
               16  DMD-SEQ-NUMBER     PIC X(4).                         
                                                                        
       01  COMP-3-WORK-AREA    COMP-3.                                  
           12  KX                  PIC S9.                              
           12  L-OW                PIC S9(9)V99.                        
           12  L-OW-ALT            PIC S9(9)V99.                        
           12  A-OW                PIC S9(7)V99.                        
           12  TEST-ZERO.                                               
               16  TEST-IS-ZERO-1.                                      
                   20  FILLER      PIC S9(9)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(9)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.
                   20  FILLER      PIC S9          VALUE ZERO.
               16  TEST-IS-ZERO-2.                                      
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          
                   20  FILLER      PIC S9(7)V99    VALUE ZERO.          

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**
      ***                                                             **
      ***                                                             **
      ***                                                             **
      ***                                                             **
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-**

       01  w-work-area.
           05  ws-comm-work            pic s9(7)v99 comp-3 value +0.
           05  ws-net-lf-clp           pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-clp           pic s9(9)v99 comp-3 value +0.
           05  ws-net-lf-ow            pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-ow            pic s9(9)v99 comp-3 value +0.
           05  ws-net-lf-pm-fee        pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-pm-fee        pic s9(9)v99 comp-3 value +0.
           05  ws-net-lf-inc           pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-inc           pic s9(9)v99 comp-3 value +0.
           05  ws-net-lf-cont-fee      pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-cont-fee      pic s9(9)v99 comp-3 value +0.
           05  ws-net-lf-cso-admin     pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-cso-admin     pic s9(9)v99 comp-3 value +0.
           05  ws-net-lf-ga-sales      pic s9(9)v99 comp-3 value +0.
           05  ws-net-ah-ga-sales      pic s9(9)v99 comp-3 value +0.
           05  ws-lf-claims            pic s9(9)v99 comp-3 value +0.
           05  ws-ah-claims            pic s9(9)v99 comp-3 value +0.


       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC

       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
       01  ws-begin-dt                 pic x(10).
       01  ws-end-dt                   pic x(10).
       01  ws-compid                   pic xxx.

      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is passed nulls from sql. The indicator will be -1        ***
      ***  if the value on sql is nulls and +0 if the value is       ***
      ***  something other than nulls. Here is an example on how     ***
      ***  to use the indicator variables.                           ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        fetch checkapp into                                 ***
      ***           :db-app-status :nu-app-status,                   ***
      ***           :db-app-by     :nu-app-by,                       ***
      ***           :db-app-date   :nu-app-date,                     ***
      ***           :db-app-batch  :nu-app-batch                     ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

       01  indicator-vaiables-for-nulls.
           05  nu-checkno              pic s9(4) comp value +0.
           05  nu-checkdate            pic s9(4) comp value +0.
           05  nu-checkstatus          pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-fingrp               pic s9(4) comp value +0.


       01  ws-extract.
           05  db-month-end-dt         pic x(10).
           05  db-carrier              pic x.
           05  db-grouping             pic x(6).
           05  db-acct-state           pic xx.
           05  db-clp-state            pic xx.
           05  db-account              pic x(10).
           05  db-eff-dte              pic x(10).
           05  db-cert-no              pic x(11).
           05  db-ben-type             pic x.
           05  db-lf-ben-code          pic xx.
           05  db-ah-ben-code          pic xx.
           05  db-net-lf-clp           pic -9(9).99.
           05  db-net-lf-clp-a      redefines
               db-net-lf-clp           pic x(13).
           05  db-net-ah-clp           pic -9(9).99.
           05  db-net-ah-clp-a      redefines
               db-net-ah-clp           pic x(13).
           05  db-net-lf-ow            pic -9(9).99.
           05  db-net-lf-ow-a       redefines
               db-net-lf-ow            pic x(13).
           05  db-net-ah-ow            pic -9(9).99.
           05  db-net-ah-ow-a       redefines
               db-net-ah-ow            pic x(13).
           05  db-net-lf-pm-fee        pic -9(9).99.
           05  db-net-lf-pm-fee-a   redefines
               db-net-lf-pm-fee        pic x(13).
           05  db-net-ah-pm-fee        pic -9(9).99.
           05  db-net-ah-pm-fee-a   redefines
               db-net-ah-pm-fee        pic x(13).
           05  db-net-lf-inc           pic -9(9).99.
           05  db-net-lf-inc-a      redefines
               db-net-lf-inc           pic x(13).
           05  db-net-ah-inc           pic -9(9).99.
           05  db-net-ah-inc-a      redefines
               db-net-ah-inc           pic x(13).
           05  db-net-lf-cont-fee      pic -9(9).99.
           05  db-net-lf-cont-fee-a redefines
               db-net-lf-cont-fee      pic x(13).
           05  db-net-ah-cont-fee      pic -9(9).99.
           05  db-net-ah-cont-fee-a redefines
               db-net-ah-cont-fee      pic x(13).
           05  db-net-lf-cso-admin     pic -9(9).99.
           05  db-net-lf-cso-admin-a redefines
               db-net-lf-cso-admin     pic x(13).
           05  db-net-ah-cso-admin     pic -9(9).99.
           05  db-net-ah-cso-admin-a redefines
               db-net-ah-cso-admin     pic x(13).
           05  db-net-lf-ga-sales      pic -9(9).99.
           05  db-net-lf-ga-sales-a redefines
               db-net-lf-ga-sales      pic x(13).
           05  db-net-ah-ga-sales      pic -9(9).99.
           05  db-net-ah-ga-sales-a redefines
               db-net-ah-ga-sales      pic x(13).
           05  db-net-lf-claims        pic -9(9).99.
           05  db-net-lf-claims-a   redefines
               db-net-lf-claims        pic x(13).
           05  db-net-ah-claims        pic -9(9).99.
           05  db-net-ah-claims-a   redefines
               db-net-ah-claims        pic x(13).

       EXEC SQL
          END DECLARE SECTION
       END-EXEC

       01  P-LINE.                                                      
           12  FILLER      PIC X(24)   VALUE 'COMMISSION FILE HAS BEEN'.
           12  FILLER      PIC X(13)   VALUE ' CREATED FOR '.           
           12  P-RUN-DATE  PIC X(87)   VALUE SPACES.                    
           12  FILLER      PIC X(8)    VALUE 'ECS-017 '.                
                                                                        
       01  HDNG-1.                                                      
           12  FILLER      PIC X(52)   VALUE SPACES.                    
           12  FILLER      PIC X(20)   VALUE 'COMPENSATION EXTRACT'.    
           12  FILLER      PIC X(52)   VALUE SPACES.                    
           12  FILLER      PIC X(8)    VALUE 'ECS-017'.                 
                                                                        
       01  HDNG-2.                                                      
           12  FILLER      PIC X(47)   VALUE SPACES.                    
           12  HD-CLIENT   PIC X(30).                                   
           12  FILLER      PIC X(47)   VALUE SPACES.                    
           12  HD-RUN      PIC X(8).                                    
                                                                        
       01  HDNG-3.                                                      
           12  FILLER      PIC X(52)   VALUE SPACES.                    
           12  HD-DATE     PIC X(18).                                   
           12  FILLER      PIC X(42)   VALUE SPACES.                    
           12  FILLER      PIC X(11)   VALUE 'PAGE      1'.             
                                                                        
       01  COMP-MESS.                                                   
           12  FILLER      PIC X(40)   VALUE SPACES.                    
           12  FILLER      PIC X(17)   VALUE '*** COMPENSATION '.       
           12  FILLER      PIC X(75)   VALUE 'EXTRACTS GENERATED ***'.  
      
       01  max-pdef                    pic s9(5) comp-3 value +5000.
       01  DCC-DDF-WORK-AREA.
           05  F OCCURS 5000.
               10  DD-STATE                 PIC XX.
               10  DD-PRODUCT-CD            PIC XXX.
               10  DD-FILLER                PIC X(7).
               10  DD-BEN-TYPE              PIC X.
               10  DD-BEN-CODE              PIC XX.
               10  DD-PROD-EXP-DT           PIC XX.
               10  DD-1ST-YR-ALLOW          PIC S999V99 COMP-3.
032922         10  DD-PROD-DATA OCCURS 11.
                   15  DD-PROD-CODE         PIC X.
               10  DD-EARN-FACTORS.                                     
                   15  F OCCURS 15.
                       20  F OCCURS 15.
                           25  DD-UEP-FACTOR    PIC S9V9(3)     COMP-3.
               10  DD-TRUNCATED             PIC X.

       01  ws-extract-rec-out.
           05  ex-carrier              pic x.
           05  ex-grouping             pic x(6).
           05  ex-clp-state            pic xx.
           05  ex-account              pic x(10).
           05  ex-eff-dte              pic 9(8).
           05  ex-cert-no              pic x(11).
           05  ex-ben-type             pic x.
               88  ben-type-lf           value '1'.
               88  ben-type-ah           value '2'.
           05  ex-ben-code             pic xx.
           05  ex-net-clp              pic s9(9)v99 comp-3 value +0.
           05  ex-net-ow               pic s9(9)v99 comp-3 value +0.
           05  ex-net-pgm-mgt-fee      pic s9(9)v99 comp-3 value +0.
           05  ex-net-incentive        pic s9(9)v99 comp-3 value +0.
           05  ex-net-cont-fee         pic s9(9)v99 comp-3 value +0.
           05  ex-net-cso-admin-fee    pic s9(9)v99 comp-3 value +0.
           05  ex-net-ga-sales         pic s9(9)v99 comp-3 value +0.
           05  ex-claim-pmts           pic s9(9)v99 comp-3 value +0.
           05  ex-acct-state           pic xx.
                                                                        
                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.
      *                                COPY ELCACCTV.
       LINKAGE SECTION.                                                 
       01  parm.
           05  parm-length             pic s9(4) comp.
           05  parm-current-month-end  pic 9(8).
           05  parm-program-option     pic x.

       01  var                         pic x(30).

       PROCEDURE DIVISION using parm.
                                       COPY ELCDTERX.
                                                                        
           display ' Begin Program DCC017 '

           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0                   to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if

           display ' KIXSYS  ' ws-kix-myenv

           perform 3000-connect-to-logic
                                       thru 3000-exit
      *    perform 3020-drop-table     thru 3020-exit
      *    perform 3010-create-table   thru 3010-exit
      *    perform 3030-truncate-table thru 3030-exit
           perform 0000-open-files     thru 0000-exit
           perform 0010-init           thru 0010-exit
           perform 0020-read-extract   thru 0020-exit
           perform 0050-process-input  thru 0050-exit
              until end-of-input
           perform 0300-cert-break     thru 0300-exit
           perform 0400-acct-break     thru 0400-exit
           perform 3040-finish-up      thru 3040-exit

           display ' Records inserted ' rec-cnt

           goback
           .
       0000-OPEN-FILES.

           OPEN
              INPUT
                 EP-EXTR
                 ERACCTT
                 ERPDEF
              OUTPUT
                 EXTR-file
                 END-PRINT

           IF ERACCTT-FILE-STATUS  = '00' OR '97'                       
              CONTINUE
           ELSE
              MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
              MOVE ' ERACCTT OPEN ERROR- '                             
                                       TO WS-ABEND-MESSAGE              
              GO TO ABEND-PGM
           END-IF

           IF ERPDEF-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE ERPDEF-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              MOVE ' ERPDEF  OPEN ERROR- '
                                       TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           .
       0000-EXIT.
           EXIT.

       0010-init.

           string
              parm-current-month-end (5:4)
              parm-current-month-end (1:2)
              parm-current-month-end (3:2)
              delimited by size into dc-greg-date-cymd-r
           end-string           

           move 'L'                    to dc-option-code
           PERFORM 8500-DATE-CONVERT   THRU 8590-EXIT
           if no-conversion-error
              move dc-greg-date-a-edit to db-month-end-dt
              display ' init month end date ' db-month-end-dt
           else
              display ' invalid parm date - aborting '
                 parm-current-month-end
              perform abend-pgm
           end-if

           MOVE RUN-YR                 TO WS-RUN-YR
           MOVE RUN-MO                 TO WS-RUN-MO
           MOVE RUN-DA                 TO WS-RUN-DA
           MOVE ALPH-DATE              TO P-RUN-DATE.                   

           MOVE '1'                    TO X.                            
           MOVE HDNG-1                 TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           MOVE ' '                    TO X.                            
           MOVE COMPANY-NAME           TO HD-CLIENT.                    
           MOVE WS-CURRENT-DATE        TO HD-RUN.                       
           MOVE HDNG-2                 TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           MOVE ' '                    TO X.                            
           MOVE ALPH-DATE              TO HD-DATE.                      
           MOVE HDNG-3                 TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           move low-values             to am-control-primary
           start eracctt key >= am-control-primary
           if eracctt-file-status = '00'
              read eracctt next record
           else
              MOVE ERACCTT-FILE-STATUS TO WS-ABEND-FILE-STATUS          
              MOVE ' ERACCTT start ERROR- '                             
                                       TO WS-ABEND-MESSAGE              
              GO TO ABEND-PGM
           END-IF

           MOVE 0 TO B-SUB
           MOVE DTE-CLASIC-COMPANY-CD  TO PD-CONTROL-PRIMARY
           START ERPDEF KEY >= PD-CONTROL-PRIMARY
           IF ERPDEF-FILE-STATUS = '00'
              PERFORM UNTIL ERPDEF-FILE-STATUS NOT = '00'
                 READ ERPDEF NEXT RECORD
                 IF ERPDEF-FILE-STATUS = '00'
                    ADD 1 TO B-SUB
                    IF B-SUB > max-pdef
                       DISPLAY ' DDF UEP TABLE BLEW '
                       GO TO ABEND-PGM
                    END-IF
                    MOVE PD-STATE      TO DD-STATE (B-SUB)
                    MOVE PD-PRODUCT-CD TO DD-PRODUCT-CD (B-SUB)
                    MOVE PD-BEN-TYPE   TO DD-BEN-TYPE (B-SUB)
                    MOVE PD-BEN-CODE   TO DD-BEN-CODE (B-SUB)
                    MOVE PD-PROD-EXP-DT
                                       TO DD-PROD-EXP-DT (B-SUB)
                    MOVE PD-1ST-YR-ADMIN-ALLOW
                                       TO DD-1ST-YR-ALLOW (B-SUB)
                    MOVE PD-TRUNCATED  TO DD-TRUNCATED (B-SUB)
                    PERFORM VARYING P2 FROM +1 BY +1 UNTIL
032922                 P2 > +11
                       MOVE PD-PROD-CODE (P2)
                                       TO DD-PROD-CODE (B-SUB P2)
                    END-PERFORM
                    MOVE PD-EARN-FACTORS
                                       TO DD-EARN-FACTORS (B-SUB)
                 END-IF
              END-PERFORM
              DISPLAY ' DCC PRODUCT DEF TABLE LOADED SUCCESSFULLY '
           END-IF
           CLOSE ERPDEF
           IF ERPDEF-FILE-STATUS = '00'
              CONTINUE
           ELSE
              MOVE ERPDEF-FILE-STATUS  TO WS-ABEND-FILE-STATUS
              MOVE ' ERPDEF CLOSE ERROR- '
                                       TO WS-ABEND-MESSAGE
              PERFORM ABEND-PGM
           END-IF

           .
       0010-EXIT.
           EXIT.
                                                                        
       0020-READ-EXTRACT.                                               

           READ EP-EXTR AT END
              set end-of-input to true
           end-read

           if not end-of-input
              add 1 to ws-recs-in
           end-if

           .
       0020-exit.
           exit.

       0030-init-work-area.

           move zeros                  to ws-net-lf-clp
                                          ws-net-ah-clp
                                          ws-net-lf-ow
                                          ws-net-ah-ow
                                          ws-net-lf-pm-fee
                                          ws-net-ah-pm-fee
                                          ws-net-lf-inc
                                          ws-net-ah-inc
                                          ws-net-lf-cont-fee
                                          ws-net-ah-cont-fee
                                          ws-net-lf-cso-admin
                                          ws-net-ah-cso-admin
                                          ws-net-lf-ga-sales
                                          ws-net-ah-ga-sales
                                          ws-lf-claims
                                          ws-ah-claims

           .
       0030-exit.
           exit.

       0050-process-input.

           if (de-rein = ' ')
              and (de-trans = 'I' or 'C' or 'X')
              continue
           else
              go to 0050-continue
           end-if
                                                                        
           IF (DE-ENTRY-STATUS = 'D' OR 'V' or '9')
                       or
              ((de-trans = 'I')
              and (de-entry-status = '5' or 'M'))
                       or
              ((de-trans = 'C')
              and (de-entry-status = 'M'))
              GO TO 0050-continue
           end-if

          if de-cntrl1 not = ws-prev-cntrl1
             perform 0300-cert-break   thru 0300-exit
             perform 0400-acct-break   thru 0400-exit
             move de-cntrl1            to ws-prev-cntrl1
             move de-cntrl2            to ws-prev-cntrl2
          else
             if de-cntrl2 not = ws-prev-cntrl2
                perform 0300-cert-break thru 0300-exit
                move de-cntrl2         to ws-prev-cntrl2
             end-if
          end-if

          perform 6300-check-types     thru 6399-exit
          perform 0060-sync-to-acct    thru 0060-exit

060616    move am-dcc-product-code     to ws-dcc-product-code
          if de-clp-state = spaces
             move de-state             to de-clp-state
          end-if

          move de-clp-state            to ws-prev-clp-state
          move de-state                to ws-prev-state
          move de-lf-type              to ws-prev-lf-ben
          move de-ah-type              to ws-prev-ah-ben

          if de-trans = 'X'
             perform 0150-claim-pmt    thru 0150-exit
          else
             if de-trans = 'C'
                perform 0200-cancel    thru 0200-exit
             else
               perform 0100-issue     thru 0100-exit
             end-if
          end-if

           .
       0050-continue.

           perform 0020-read-extract   thru 0020-exit

           .
       0050-exit.
           exit.
                                                                        
       0060-sync-to-acct.

           if am-control-a < de-cntrl1
              perform 0070-read-acct   thru 0070-exit
              go to 0060-sync-to-acct
           else
              if am-control-a > de-cntrl1
                 display ' no acct for detail ' am-control-a ' '
                    de-cntrl1
                 perform abend-pgm
              end-if
           end-if

           if am-expire-dt <= de-eff
              perform 0070-read-acct   thru 0070-exit
              go to 0060-sync-to-acct
           end-if

           if (de-eff < am-expire-dt)
              and (de-eff >= am-effect-dt)
              continue
           else
              display ' no acct for extract ' de-cntrl1 ' '
                 de-cert-no ' ' de-eff
              perform abend-pgm
           end-if

           .
       0060-exit.
           exit.

       0070-read-acct.

           READ ERACCTT next record

           IF ERACCTT-FILE-STATUS = '10'                                
              MOVE HIGH-VALUES         TO ACCOUNT-MASTER                
           ELSE                                                        
              IF ERACCTT-FILE-STATUS NOT = ZERO                        
                 MOVE ERACCTT-FILE-STATUS                             
                                       TO WS-ABEND-FILE-STATUS              
                 MOVE ' ERACCTT READ ERROR- '                         
                                       TO WS-ABEND-MESSAGE                  
                 GO TO ABEND-PGM
              end-if
           end-if

           .
       0070-EXIT.                                                       
           EXIT.                                                        

       0100-issue.

      **** lf clp, fees and o/w

           if de-lf-type not = '00' and 'DD' and 'CU'
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when de-agt-type (a1) = 'C' OR 'D'
                       if clas-i-ben-category (clas-indexl) = 'D'
                          move de-rei-lfprm to ws-net-lf-clp
                       else
                          compute ws-comm-work rounded =
                             de-lf-prm * de-l-pc(a1)
                          compute ws-net-lf-clp =
                             de-lf-prm - ws-comm-work
                       end-if
                    when de-agt-type (a1) = 'P'
                       compute ws-net-lf-ow rounded =
                          ws-net-lf-ow + (de-lf-prm * de-l-pc(a1))
                    when de-agt-type (a1) = 'L'
                       compute ws-net-lf-pm-fee rounded =
                          ws-net-lf-pm-fee + (de-l-pc(a1) * +1000)
                    when de-agt-type (a1) = 'A'
                       compute ws-net-lf-pm-fee rounded =
                          ws-net-lf-pm-fee +
                          (de-rei-lfprm * de-l-pc(a1))
                    when de-agt-type(a1) = 'I' or 'J'
                       compute ws-net-lf-inc rounded =
                          ws-net-lf-inc + (de-l-pc(a1) * +1000)
                    when de-agt-type(a1) = 'B'
                       compute ws-net-lf-cont-fee rounded =
                          ws-net-lf-cont-fee + (de-l-pc(a1) * +1000)
                    when de-agt-type(a1) = 'N'
                       compute ws-net-lf-cso-admin rounded =
                          ws-net-lf-cso-admin + (de-l-pc(a1) * +1000)
                    when de-agt-type (a1) = 'O'
                       compute ws-net-lf-ga-sales rounded =
                          ws-net-lf-ga-sales +
                            (de-lf-prm * de-l-pc(a1))
                    when de-agt-type (a1) = 'S'
                       compute ws-net-lf-cso-admin rounded =
                          ws-net-lf-cso-admin +
                             (de-lf-prm * de-l-pc(a1))
                 end-evaluate
              end-perform
           end-if

      **** ah clp, fees and o/w

           if de-ah-type not = '00'
              if de-rei-ahprm not = zeros
                 move de-rei-ahprm     to ws-net-ah-clp
              else
                 move de-lf-prm-alt    to ws-net-ah-clp
              end-if
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when de-agt-type (a1) = 'C' OR 'D'
                       if ws-net-ah-clp = zeros
                          compute ws-comm-work rounded =
                             de-ah-prm * de-a-pc(a1)
                          compute ws-net-ah-clp =
                             de-ah-prm - ws-comm-work
                       end-if
                    when de-agt-type (a1) = 'P'
                       compute ws-net-ah-ow rounded =
                          ws-net-ah-ow + (de-ah-prm * de-a-pc(a1))
                    when de-agt-type (a1) = 'L'
                       compute ws-net-ah-pm-fee rounded =
                          ws-net-ah-pm-fee + (de-a-pc(a1) * +1000)
                    when de-agt-type(a1) = 'I' or 'J'
                       compute ws-net-ah-inc rounded =
                          ws-net-ah-inc + (de-a-pc(a1) * +1000)
                    when de-agt-type(a1) = 'B'
                       compute ws-net-ah-cont-fee rounded =
                          ws-net-ah-cont-fee + (de-a-pc(a1) * +1000)
                    when de-agt-type(a1) = 'N'
                       compute ws-net-ah-cso-admin rounded =
                          ws-net-ah-cso-admin + (de-a-pc(a1) * +1000)
                    when de-agt-type (a1) = 'S'
                       compute ws-net-ah-cso-admin rounded =
                          ws-net-ah-cso-admin +
                             (de-ah-prm * de-a-pc(a1))
                    when de-agt-type (a1) = 'O'
                       compute ws-net-ah-ga-sales rounded =
                          ws-net-ah-ga-sales +
                            (de-ah-prm * de-a-pc(a1))
                 end-evaluate
              end-perform
           end-if

           .
       0100-exit.
           exit.

       0150-claim-pmt.

           if de-death
              compute ws-lf-claims = ws-lf-claims + de-claim-amt
           else
              if de-disability
                 compute ws-ah-claims = ws-ah-claims + de-claim-amt
              end-if
           end-if

           .
       0150-exit.
           exit.

       0200-cancel.

      **** lf clp, fees and o/w

           if de-lf-prm not = zero
              compute ws-lf-cnc-fact = de-lf-rfnd / de-lf-prm
           end-if

           if de-lf-type not = '00' and 'DD' and 'CU'
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when de-agt-type (a1) = 'C' OR 'D'
                       if clas-i-ben-category (clas-indexl) = 'D'
                          compute cnc-fact = de-lf-rfnd / de-lf-prm
                          compute ws-net-lf-clp rounded =
                             de-rei-lfprm * cnc-fact
                       else
                          compute ws-comm-work rounded =
                             de-lf-rfnd * de-l-pc(a1)
                          compute ws-net-lf-clp rounded =
                             ws-net-lf-clp +
                             (de-lf-rfnd - ws-comm-work) * -1
                       end-if
                    when de-agt-type (a1) = 'P'
                       compute ws-net-lf-ow rounded =
                          ws-net-lf-ow +
                          (de-lf-rfnd * de-l-pc(a1) * -1)
                    when de-agt-type (a1) = 'L'
                       compute ws-net-lf-pm-fee rounded =
                          ws-net-lf-pm-fee +
                          (de-l-pc(a1) * +1000 * ws-lf-cnc-fact * -1)
                    when de-agt-type (a1) = 'A'
                       compute ws-net-lf-pm-fee rounded =
                          ws-net-lf-pm-fee +
                          (de-rei-lfprm * de-l-pc(a1) *
                          ws-lf-cnc-fact * -1)
                    when de-agt-type(a1) = 'J'
                       compute ws-net-lf-inc rounded =
                          ws-net-lf-inc +
                          (de-l-pc(a1) * +1000 * ws-lf-cnc-fact * -1)
                    when de-agt-type(a1) = 'B'
                       compute ws-net-lf-cont-fee rounded =
                          ws-net-lf-cont-fee +
                          (de-l-pc(a1) * +1000 * ws-lf-cnc-fact * -1)
                    when de-agt-type (a1) = 'O'
                       compute ws-net-lf-ga-sales rounded =
                          ws-net-lf-ga-sales +
                            (de-lf-rfnd * de-l-pc(a1) * -1)
                    when de-agt-type (a1) = 'S'
                       compute ws-net-lf-cso-admin rounded =
                          ws-net-lf-cso-admin +
                             (de-lf-rfnd * de-l-pc(a1) * -1)
                 end-evaluate
              end-perform
           end-if

      **** ah clp, fees and o/w

           if de-ah-type not = '00'
              if de-ah-prm not = zeros
                 compute ws-ah-cnc-fact = de-ah-rfnd / de-ah-prm
              end-if
              if de-rei-ahrfnd not = zeros
                 compute ws-net-ah-clp =
                    ws-net-ah-clp - de-rei-ahrfnd
              else
                 compute ws-net-ah-clp rounded =
                 ws-net-ah-clp + (de-lf-prm-alt * ws-ah-cnc-fact * -1)
              end-if
              perform varying a1 from +1 by +1 until a1 > +10
                 evaluate true
                    when de-agt-type (a1) = 'C' or 'D'
                       if de-rei-ahrfnd = zeros
                          and de-lf-prm-alt = zeros
                          compute ws-comm-work rounded =
                             de-ah-rfnd * de-a-pc(a1)
                          compute ws-net-ah-clp = ws-net-ah-clp -
                             (de-ah-rfnd - ws-comm-work)
                       end-if
                    when de-agt-type (a1) = 'P'
                       compute ws-net-ah-ow rounded =
                          ws-net-ah-ow +
                          (de-ah-rfnd * de-a-pc(a1) * -1)
                    when de-agt-type (a1) = 'L'
                       compute ws-net-ah-pm-fee rounded =
                          ws-net-ah-pm-fee +
                          (de-a-pc(a1) * +1000 * ws-ah-cnc-fact * -1)
                    when de-agt-type(a1) = 'J'
                       compute ws-net-ah-inc rounded =
                          ws-net-ah-inc +
                          (de-a-pc(a1) * +1000 * ws-ah-cnc-fact * -1)
                    when de-agt-type(a1) = 'I'
                      COMPUTE a-ow = DE-A-PC(a1) * 1000
                      PERFORM 2530-CALC-DDF-FEES THRU 2530-EXIT
                       compute ws-net-ah-inc rounded =
                          ws-net-ah-inc - cnc-wk
                    when de-agt-type(a1) = 'B'
                       compute ws-net-ah-cont-fee rounded =
                          ws-net-ah-cont-fee +
                          (de-a-pc(a1) * +1000 * ws-ah-cnc-fact * -1)
                    when de-agt-type(a1) = 'N'
                      COMPUTE a-ow = DE-A-PC(a1) * 1000
                      PERFORM 2530-CALC-DDF-FEES THRU 2530-EXIT
                       compute ws-net-ah-cso-admin rounded =
                          ws-net-ah-cso-admin - cnc-wk
                    when de-agt-type (a1) = 'S'
                       compute ws-net-ah-cso-admin rounded =
                          ws-net-ah-cso-admin +
                             (de-ah-rfnd * de-a-pc(a1) * -1)
                    when de-agt-type (a1) = 'O'
                       compute ws-net-ah-ga-sales rounded =
                          ws-net-ah-ga-sales +
                            (de-ah-rfnd * de-a-pc(a1) * -1)
                 end-evaluate
              end-perform
           end-if

052516     if de-carrier = '7' or '9'
052516        compute ws-net-ah-clp = ws-net-ah-clp -
052516           ws-net-ah-cso-admin - ws-net-ah-ga-sales
052516     end-if

           .
       0200-exit.
           exit.

       0300-cert-break.

           if ws-prev-cntrl1 = spaces
              go to 0300-exit
           end-if

           move ws-prev-carrier        to db-carrier           
           move ws-prev-group          to db-grouping          
           move ws-prev-state          to db-acct-state
           move ws-prev-clp-state      to db-clp-state
           move ws-prev-account        to db-account           
           move ws-prev-eff-dt         to ws-work-date-n
           string
              ws-wd-mm     '/'
              ws-wd-dd     '/'
              ws-wd-ccyy
              delimited by size into db-eff-dte
           end-string
           move ws-prev-cert-no        to db-cert-no           
           move ws-prev-lf-ben         to db-lf-ben-code
           move ws-prev-ah-ben         to db-ah-ben-code
           move ws-net-lf-clp          to db-net-lf-clp        
           move ws-net-ah-clp          to db-net-ah-clp        
           move ws-net-lf-ow           to db-net-lf-ow         
           move ws-net-ah-ow           to db-net-ah-ow         
           move ws-net-lf-pm-fee       to db-net-lf-pm-fee     
           move ws-net-ah-pm-fee       to db-net-ah-pm-fee     
           move ws-net-lf-inc          to db-net-lf-inc        
           move ws-net-ah-inc          to db-net-ah-inc        
           move ws-net-lf-cont-fee     to db-net-lf-cont-fee   
           move ws-net-ah-cont-fee     to db-net-ah-cont-fee   
           move ws-net-lf-cso-admin    to db-net-lf-cso-admin  
           move ws-net-ah-cso-admin    to db-net-ah-cso-admin  
           move ws-net-lf-ga-sales     to db-net-lf-ga-sales   
           move ws-net-ah-ga-sales     to db-net-ah-ga-sales   
           move ws-lf-claims           to db-net-lf-claims     
           move ws-ah-claims           to db-net-ah-claims     

           perform 0320-insert-row     thru 0320-exit
           perform 0030-init-work-area thru 0030-exit

           .
       0300-exit.
           exit.

       0320-insert-row.

           if zeros = db-lf-ben-code and ws-net-lf-clp
                 and ws-net-lf-ow and ws-net-lf-pm-fee
                 and ws-net-lf-inc and ws-net-lf-cont-fee
                 and ws-net-lf-cso-admin and ws-net-lf-ga-sales
                 and ws-lf-claims
              continue
           else
              move '1'                 to db-ben-type
              perform 0350-write-extract thru 0350-exit
              EXEC SQL
                 insert into DCC_DETAIL_EXTRACTS (
                   MONTH_END_DT     ,
                   carrier          ,
                   grouping         ,
                   acct_state       ,
                   clp_state        ,
                   account          ,
                   eff_dte          ,
                   cert_no          ,
                   ben_type         ,
                   ben_code         ,
                   net_clp          ,
                   net_ow           ,
                   net_pm_fee       ,
                   net_inc          ,
                   net_cont_fee     ,
                   net_cso_admin    ,
                   net_ga_sales     ,
                   net_claims)
                 values (
                   :db-month-end-dt     ,
                   :db-carrier          ,
                   :db-grouping         ,
                   :db-acct-state       ,
                   :db-clp-state        ,
                   :db-account          ,
                   :db-eff-dte          ,
                   :db-cert-no          ,
                   :db-ben-type         ,
                   :db-lf-ben-code      ,
                   :db-net-lf-clp-a     ,
                   :db-net-lf-ow-a      ,
                   :db-net-lf-pm-fee-a  ,
                   :db-net-lf-inc-a     ,
                   :db-net-lf-cont-fee-a  ,
                   :db-net-lf-cso-admin-a ,
                   :db-net-lf-ga-sales-a  ,
                   :db-net-lf-claims-a)
              end-exec
              if sqlcode not = 0
                 display "Error: cannot insert LF row "
                 move sqlcode             to ws-sql-code
                 move ws-sql-code         to ws-dis-sql-code
                 display ' sqlcode ' ws-dis-sql-code
                 display ' sql err mess    ' sqlerrmc
                 display ' in out cnt ' ws-recs-in ' ' rec-cnt
                 display ' offending rec ' ws-extract
              else
                 add 1 to rec-cnt
              end-if
           end-if

           if zeros = db-ah-ben-code and ws-net-ah-clp
                 and ws-net-ah-ow and ws-net-ah-pm-fee
                 and ws-net-ah-inc and ws-net-ah-cont-fee
                 and ws-net-ah-cso-admin and ws-net-ah-ga-sales
                 and ws-ah-claims
              continue
           else
              move '2'                 to db-ben-type
              perform 0350-write-extract thru 0350-exit
              EXEC SQL
                 insert into DCC_DETAIL_EXTRACTS (
                   MONTH_END_DT     ,
                   carrier          ,
                   grouping         ,
                   acct_state       ,
                   clp_state        ,
                   account          ,
                   eff_dte          ,
                   cert_no          ,
                   ben_type         ,
                   ben_code         ,
                   net_clp          ,
                   net_ow           ,
                   net_pm_fee       ,
                   net_inc          ,
                   net_cont_fee     ,
                   net_cso_admin    ,
                   net_ga_sales     ,
                   net_claims)
                 values (
                   :db-month-end-dt     ,
                   :db-carrier          ,
                   :db-grouping         ,
                   :db-acct-state       ,
                   :db-clp-state        ,
                   :db-account          ,
                   :db-eff-dte          ,
                   :db-cert-no          ,
                   :db-ben-type         ,
                   :db-ah-ben-code      ,
                   :db-net-ah-clp-a     ,
                   :db-net-ah-ow-a      ,
                   :db-net-ah-pm-fee-a  ,
                   :db-net-ah-inc-a     ,
                   :db-net-ah-cont-fee-a  ,
                   :db-net-ah-cso-admin-a ,
                   :db-net-ah-ga-sales-a  ,
                   :db-net-ah-claims-a)
              end-exec
              if sqlcode not = 0
                 display "Error: cannot insert AH row "
                 move sqlcode             to ws-sql-code
                 move ws-sql-code         to ws-dis-sql-code
                 display ' sqlcode ' ws-dis-sql-code
                 display ' sql err mess    ' sqlerrmc
                 display ' in out cnt ' ws-recs-in ' ' rec-cnt
                 display ' offending rec ' ws-extract
              else
                 add 1 to rec-cnt
              end-if
           end-if

           .
       0320-exit.
           exit.
                                                                        
       0350-write-extract.

           move spaces                 to ws-extract-rec-out
           move db-carrier             to ex-carrier
           move db-grouping            to ex-grouping
           move ws-prev-clp-state      to ex-clp-state
           move ws-prev-state          to ex-acct-state
           move db-account             to ex-account
           move ws-prev-eff-dt         to ex-eff-dte
           move db-cert-no             to ex-cert-no
           move db-ben-type            to ex-ben-type
           if ex-ben-type = '1'  *> 1 = life
              move db-lf-ben-code      to ex-ben-code
              move ws-net-lf-clp       to ex-net-clp
              move ws-net-lf-ow        to ex-net-ow
              move ws-net-lf-pm-fee    to ex-net-pgm-mgt-fee
              move ws-net-lf-inc       to ex-net-incentive
              move ws-net-lf-cont-fee  to ex-net-cont-fee
              move ws-net-lf-cso-admin to ex-net-cso-admin-fee
              move ws-net-lf-ga-sales  to ex-net-ga-sales
              move ws-lf-claims        to ex-claim-pmts
           else               *> 2 = disab
              move db-ah-ben-code      to ex-ben-code
              move ws-net-ah-clp       to ex-net-clp
              move ws-net-ah-ow        to ex-net-ow
              move ws-net-ah-pm-fee    to ex-net-pgm-mgt-fee
              move ws-net-ah-inc       to ex-net-incentive
              move ws-net-ah-cont-fee  to ex-net-cont-fee
              move ws-net-ah-cso-admin to ex-net-cso-admin-fee
              move ws-net-ah-ga-sales  to ex-net-ga-sales
              move ws-ah-claims        to ex-claim-pmts
           end-if

           write extr-record from ws-extract-rec-out

           .
       0350-exit.
           exit.

       0400-acct-break.



           .
       0400-exit.
           exit.

       2520-REF-CSO-ADMIN-FEE.

           IF DE-CERT-NO = WS-FACT-CERT
              CONTINUE
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           MOVE ZEROS                  TO CNC-WK

           EVALUATE TRUE
              WHEN (DE-DCC-DDF-REM-TRM3 = 0)
                 OR (DE-CANCEL-REASON = 'R')
      *          DISPLAY ' REM TERM = 0 OR REPO'
                 MOVE 0                TO CNC-WK

082912        when de-ah-rfnd = de-ah-prm
082912*          display ' full refund '
082912           move a-ow             to cnc-wk
              WHEN DE-DCC-DDF-REM-TRM3 = DE-AH-TERM
      *          DISPLAY ' REM TERM  = ORIG TERM '
                 MOVE A-OW             TO CNC-WK

              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4
      *          DISPLAY ' ORIG TERM - REM TRM < 4 '
                 COMPUTE W-FACTOR ROUNDED = A-OW -
                    (DD-1ST-YR-ALLOW (D1)
                    * WS-MONTH / 3) - (A-OW - DD-1ST-YR-ALLOW (D1))
                    * (1 - WS-HI-UEF) * WS-MONTH / 12
                 COMPUTE CNC-WK = W-FACTOR * 1

      **** (AdminFee-Yr1 AF)-((AdminFee- Yr1 AF)*(1 - UEF1)*mo/12)

              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13
      *          DISPLAY ' ORIG TERM - REM TRM < 13 '
                 COMPUTE W-FACTOR ROUNDED = (a-ow -
                    DD-1ST-YR-ALLOW (D1))
                    - (a-ow - DD-1ST-YR-ALLOW (D1))
                    * (1 - WS-hi-UEF) * ws-MONTH / 12
                 COMPUTE CNC-WK = W-FACTOR * 1

      ****  (AdminFee-Yr1 AF)*{UEF1 - (UEF1 -UEF2)*mo/12}

              WHEN OTHER
      *          DISPLAY ' OTHER '
                 COMPUTE W-FACTOR ROUNDED = (a-ow -
                    DD-1ST-YR-ALLOW (D1))
                    * (WS-HI-UEF - (WS-HI-UEF - WS-LO-UEF)
                    * ws-MONTH / 12)
                 COMPUTE CNC-WK = W-FACTOR * 1
           END-EVALUATE

           .
       2520-EXIT.
           EXIT.

       2530-CALC-DDF-FEES.

      *****   THIS PARA IS ONLY FOR COMM TYPES N AND I
      *****   DCC DDF REFUNDS ONLY

           IF DE-CERT-NO = WS-FACT-CERT
              CONTINUE
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           COMPUTE WS-MONTH =
              FUNCTION REM(DE-AH-TERM - DE-DCC-DDF-REM-TRM3, 12)
           IF WS-MONTH = 0
              MOVE 12 TO WS-MONTH
           END-IF
      *    DISPLAY ' WS MONTH ' WS-MONTH

           EVALUATE TRUE
              WHEN DE-AGT-TYPE (a1) = 'N'
                 PERFORM 2520-REF-CSO-ADMIN-FEE
                                       THRU 2520-EXIT
              WHEN (DE-AGT-TYPE (a1) = 'I')
                 AND (DE-CANCEL-REASON = 'R')
                    MOVE A-OW TO CNC-WK
              WHEN (DE-AGT-TYPE (a1) = 'I')
                 EVALUATE TRUE
                    WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4)
                       OR (NOT DD-IU-PRESENT)
                       COMPUTE WS-ah-CNC-FACT = DE-REI-AHRFND /
                         DE-REI-AHPRM
                       COMPUTE CNC-WK ROUNDED = WS-ah-CNC-FACT * A-OW
                     WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13)
                        AND (DD-IU-PRESENT)
                        COMPUTE WS-CLP-MO3 = DE-REI-AHPRM -
                           (DE-REI-AHPRM -
                           DE-IU-RATE-UP) * (WS-HI-UEF -
                           WS-LO-UEF) * 3 / 12
      *                 DISPLAY ' UECLPMO3 ' WS-CLP-MO3
                        COMPUTE WS-ah-CNC-FACT =
                           WS-CLP-MO3 / DE-REI-AHPRM
                        COMPUTE WS-COMM-MO3 ROUNDED = WS-ah-CNC-FACT
                         * A-OW
      *                 DISPLAY ' UEMGTFO3 ' WS-COMM-MO3
                        COMPUTE CNC-WK = WS-COMM-MO3 - (WS-COMM-MO3 -
                          A-OW * WS-HI-UEF) * (WS-MONTH - 3) / 9
                    WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) > 12
                       COMPUTE CNC-WK = A-OW *
                          (WS-HI-UEF - (WS-HI-UEF - WS-LO-UEF) *
                          WS-MONTH / 12)
                 END-EVALUATE
      *          DISPLAY ' CSO REF MGT FEE ' DE-CERT-NO ' ' CNC-WK
           END-EVALUATE

           .
       2530-EXIT.
           EXIT.

       2540-CALC-DDF-COMM.

           IF DE-CERT-NO = WS-FACT-CERT
              continue
      *       DISPLAY ' FACTORS ALREADY BUILT IN 2540 ' DE-CERT-NO
           ELSE
              PERFORM 2550-GET-DDF-FACTORS THRU 2550-EXIT
           END-IF

           COMPUTE WS-MONTH =
              FUNCTION REM(DE-AH-TERM - DE-DCC-DDF-REM-TRM3, 12)
           IF WS-MONTH = 0
              MOVE 12 TO WS-MONTH
           END-IF
      *    DISPLAY ' WS MONTH ' WS-MONTH

           EVALUATE TRUE
              WHEN DE-CANCEL-REASON = 'R'
                 MOVE A-OW             TO CNC-WK
              WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 4)
                 OR (NOT DD-IU-PRESENT)
                    COMPUTE WS-ah-CNC-FACT =
                    			DE-REI-AHRFND / DE-REI-AHPRM
                    COMPUTE CNC-WK = WS-ah-CNC-FACT * A-OW
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < 13
                  COMPUTE WS-CLP-MO3 = DE-REI-AHPRM - (DE-REI-AHPRM -
                     DE-IU-RATE-UP) * (WS-HI-UEF -
                     WS-LO-UEF) * 3 / 12
      *           DISPLAY ' UECLPMO3 ' WS-CLP-MO3
                  COMPUTE WS-ah-CNC-FACT = WS-CLP-MO3 / DE-REI-AHPRM
                  COMPUTE WS-COMM-MO3 ROUNDED = WS-ah-CNC-FACT * A-OW
      *           DISPLAY ' UECOMMO3 ' WS-COMM-MO3
                  COMPUTE CNC-WK = WS-COMM-MO3 - (WS-COMM-MO3 -
                    A-OW * WS-HI-UEF) * (WS-MONTH - 3) / 9
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) > 12
                 COMPUTE CNC-WK = a-ow * (WS-HI-UEF - (WS-HI-UEF -
                    WS-LO-UEF) * ws-MONTH / 12)
           END-EVALUATE

      *    DISPLAY ' CSO REF comm    ' DE-CERT-NO ' ' CNC-WK

           .
       2540-EXIT.
           EXIT.

       2550-GET-DDF-FACTORS.

           MOVE ZEROS                  TO CNC-WK
                                          WS-LO-UEF WS-HI-UEF
           move ' '                    to dd-iu-sw
051414     if de-clp-state = spaces
051414        move de-state            to de-clp-state
           end-if

           PERFORM VARYING D1 FROM +1 BY +1 UNTIL
051414        ((DE-clp-STATE = DD-STATE (D1))
080212        and (WS-DCC-PRODUCT-CODE = dd-product-cd (d1))
080212*       AND (AM-DCC-PRODUCT-CODE = DD-PRODUCT-CD (D1))
              AND ('A'           = DD-BEN-TYPE   (D1))
              AND (DE-AH-TYPE    = DD-BEN-CODE   (D1))
              AND (DE-EFF        < DD-PROD-EXP-DT (D1)))
                              OR
100413        D1 > max-pdef
           END-PERFORM
100413     IF D1 > max-pdef
060616        DISPLAY ' COULD NOT FIND UEP FACTORS ' de-carrier ' '
060616           de-clp-state ' ' de-account ' ' de-eff ' ' de-cert-no
060616           ' ' ws-dcc-product-code ' ' de-ah-type
              GO TO 2550-EXIT
           END-IF

           PERFORM VARYING P1 FROM +1 BY +1 UNTIL
032922        (P1 > +11)
              OR (DD-PROD-CODE (D1 P1) = 'I')
           END-PERFORM
032922     IF P1 < +12
      *       display ' setting iu present to true '
              SET DD-IU-PRESENT        TO TRUE
           END-IF

           MOVE DE-AH-TERM             TO WS-DDF-TERM
           
           IF (DE-LOAN-TERM > WS-DDF-TERM)
              AND (DD-TRUNCATED (D1) = 'Y')
              MOVE DE-LOAN-TERM        TO WS-DDF-TERM
      *       DISPLAY ' FOUND TRUNCATED ' DE-CERT-NO
           END-IF

           EVALUATE TRUE
              WHEN WS-DDF-TERM > +168
                 MOVE 15               TO P1
              WHEN WS-DDF-TERM > +156
                 MOVE 14               TO P1
              WHEN WS-DDF-TERM > +144
                 MOVE 13               TO P1
              WHEN WS-DDF-TERM > +132
                 MOVE 12               TO P1
              WHEN WS-DDF-TERM > +120
                 MOVE 11               TO P1
              WHEN WS-DDF-TERM > +108
                 MOVE 10               TO P1
              WHEN WS-DDF-TERM > +96
                 MOVE 9                TO P1
              WHEN WS-DDF-TERM > +84
                 MOVE 8                TO P1
              WHEN WS-DDF-TERM > +72
                 MOVE 7                TO P1
              WHEN WS-DDF-TERM > +60
                 MOVE 6                TO P1
              WHEN WS-DDF-TERM > +48
                 MOVE 5                TO P1
              WHEN WS-DDF-TERM > +36
                 MOVE 4                TO P1
              WHEN WS-DDF-TERM > +24
                 MOVE 3                TO P1
              WHEN WS-DDF-TERM > +12
                 MOVE 2                TO P1
              WHEN OTHER
                 MOVE 1                TO P1
           END-EVALUATE

           EVALUATE TRUE
      *       WHEN ((DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +13)
      *          and (dd-iu-present)
      *          MOVE 2                TO P2
      *       WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +13
      *          MOVE 1                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +25
                 MOVE 2                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +37
                 MOVE 3                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +49
                 MOVE 4                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +61
                 MOVE 5                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +73
                 MOVE 6                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +85
                 MOVE 7                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +97
                 MOVE 8                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +109
                 MOVE 9                TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +121
                 MOVE 10               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +133
                 MOVE 11               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +145
                 MOVE 12               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +157
                 MOVE 13               TO P2
              WHEN (DE-AH-TERM - DE-DCC-DDF-REM-TRM3) < +169
                 MOVE 14               TO P2
              WHEN OTHER
                 MOVE 15               TO P2
           END-EVALUATE

           MOVE DE-CERT-NO             TO WS-FACT-CERT
           MOVE DD-UEP-FACTOR (D1 P1 P2 + 1)
                                       TO WS-LO-UEF
           MOVE DD-UEP-FACTOR (D1 P1 P2)
                                       TO WS-HI-UEF

           .
       2550-EXIT.
           EXIT.

       3000-connect-to-logic.

           display ' about to connect to Logic '

063022     move 'appuser'              to usr
063022     move 'appuser@cso'          to pass
063022
063022     move 'TEST_Logic'           to svr
063022
063022     if ws-kix-myenv = 'cid1p'
063022        move 'PROD_Logic'        to svr
063022     end-if

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string

           EXEC SQL
              SET OPTION logintime 5
           END-EXEC

           EXEC SQL
              CONNECT TO :svr
                    USER :usr-pass
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot connect to Logic"
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display sqlerrmc
              perform abend-pgm
           end-if

           display ' Successful connect ' svr

           .
       3000-exit.
           exit.

       3010-create-table.

           display ' about to create table DCC_DETAIL_EXTRACTS '
           EXEC SQL
              create table DCC_DETAIL_EXTRACTS (
                month_end_dt     datetime NOT null,
                carrier          char(1)  NOT NULL,
                grouping         char(6)  NOT NULL,
                acct_state       char(2)  NOT NULL,
                clp_state        char(2)  NOT NULL,
                account          char(10) NOT NULL,
                eff_dte          datetime NOT NULL,
                cert_no          char(11) NOT NULL,
                ben_type         char(1)  NOT NULL,
                ben_code         char(2)  NOT NULL,
                net_clp       decimal(11,2),
                net_ow        decimal(11,2),
                net_pm_fee    decimal(11,2),
                net_inc       decimal(11,2),
                net_cont_fee  decimal(11,2),
                net_cso_admin decimal(11,2),
                net_ga_sales  decimal(11,2),
                net_claims    decimal(11,2),
                 CONSTRAINT PK_DCC_DETAIL_EXTRACTS
                    PRIMARY KEY CLUSTERED
                   (month_end_dt, carrier, grouping, acct_state,
                    account, eff_dte, cert_no, ben_type, ben_code)
             	   )
           END-EXEC

           if sqlcode not = 0
              display "Error: cannot create table "
              move sqlcode             to ws-disp-code
              display ' sql return code ' ws-disp-code
              display ' sql err mess    ' sqlerrmc
              perform abend-pgm
           end-if

           display ' Table created '

           .
       3010-exit.
           exit.

       3020-drop-table.

           display 'Begin Drop table'
           EXEC SQL
               drop table DCC_DETAIL_EXTRACTS
           END-EXEC
           if sqlcode not = 0
              display "Error(anticipated) : cannot drop table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       3020-exit.
           exit.

       3030-truncate-table.

           display 'Begin Truncate table'
           EXEC SQL
               truncate table DCC_DETAIL_EXTRACTS
           END-EXEC

           if sqlcode not = 0
              display "Error : cannot truncate table "
              move sqlcode             to ws-sql-code
              move ws-sql-code         to ws-dis-sql-code
              display ' sqlcode ' ws-dis-sql-code
              display ' message ' sqlerrmc
           end-if

           .
       3030-exit.
           exit.

       3040-finish-up.

           EXEC SQL
               commit work release
           END-EXEC

           if sqlcode not = 0
              display "Error: commit work release "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           EXEC SQL
              DISCONNECT
           END-EXEC

           if sqlcode not = 0
              display "Error: disconnect  "
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if

           .
       3040-exit.
           exit.

       6300-CHECK-TYPES.
      
      *    MOVE SPACES                 TO EX-AZ EX-LZ
           MOVE CLAS-STARTL            TO CLAS-INDEXL
           MOVE CLAS-STARTA            TO CLAS-INDEXA
      
           IF DE-LF-TYPE = '  ' OR '00' OR 'DD' or 'CU'
              GO TO 6330-CHECK-AH
           END-IF

           .
       6310-CHECK-LIFE.                                                 
           MOVE DE-LF-TYPE             TO CLAS-LOOK
      
           IF CLAS-STARTL = ZEROS                                       
              GO TO 6330-CHECK-AH
           END-IF

           .
       6320-LOOP-LIFE.                                                  
           IF CLAS-INDEXL GREATER THAN CLAS-MAXL                        
              GO TO 6330-CHECK-AH
           END-IF
                                                                        
           IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXL)                  
              ADD 1 TO CLAS-INDEXL                                      
              GO TO 6320-LOOP-LIFE
           END-IF

           .
       6330-CHECK-AH.                                                   
           IF DE-AH-TYPE = '  ' OR '00'
              GO TO 6399-EXIT
           END-IF
                                                                        
           MOVE DE-AH-TYPE             TO CLAS-LOOK
                                                                        
           IF CLAS-STARTA = ZEROS                                       
              GO TO 6399-EXIT
           END-IF

           .
       6340-LOOP-AH.                                                    
      
           IF CLAS-INDEXA GREATER THAN CLAS-MAXA
              GO TO 6399-EXIT
           END-IF
      
           IF CLAS-LOOK NOT = CLAS-I-BEN (CLAS-INDEXA)
              ADD 1 TO CLAS-INDEXA
              GO TO 6340-LOOP-AH
           END-IF

           .
       6399-EXIT.                                                       
           EXIT.                                                        
       8000-PRT-RTN.                                                    
                                   COPY ELCPRT2.                        

       8099-PRT-XIT.                                                    
           EXIT.                                                        

       9900-END-JOB.                                                    
                                                                        
           MOVE '-'                    TO P-CTL.                        
           MOVE P-LINE                 TO P-DATA.                       
           MOVE P-CTL                  TO X.                            
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      
                                                                        
           MOVE '-'                    TO X.                            
           MOVE COMP-MESS              TO P-DATA.                       
           PERFORM 8000-PRT-RTN THRU 8099-PRT-XIT.                      

           CLOSE EP-EXTR
                 ERACCTT
                 EXTR-file
                 END-PRINT
           .                                                                        
       9920-CLOSE-FICH.                                                 
                                       COPY ELCPRTC.
                                                                        
       9999-STOP-RUN.                                                   

           MOVE ZEROS  TO RETURN-CODE.                                  
           GOBACK

           .
       8500-DATE-CONVERT.              
                                       COPY ELCDCS.

       abend-pgm.

            call 'ABORTME'.
            
            goback.
