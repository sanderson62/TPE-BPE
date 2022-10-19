      $SET SQL(dbman=ODBC, TARGETDB=MSSQLSERVER, NOAUTOCOMMIT)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. WSMESS03.
       AUTHOR.     Cowtown.
       DATE-COMPILED.
      *SECURITY.   *****************************************************
      *            *                                                   *
      *            *   THIS PROGRAM IS THE PROPERTY OF CSO             *
      *            *                                                   *
      *            *   USE OF THIS PROGRAM BY OTHER THAN THE EMPLOYEES *
      *            *   OF     CSO     IS EXPRESSLY PROHIBITED WITHOUT  *
      *            *   THE PRIOR WRITTEN PERMISSION OF CSO.            *
      *            *                                                   *
      *            *****************************************************
      
      ******************************************************************
      *REMARKS.                                                        *
      *     Premium verification                                       *
      *  Returns premium calc based on info passed to me.              *
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 020617   2017020300002   PEMA  New Program
      * 080117   2017020300002   PEMA  Tol for tot pmts, use totpmts GP
      * 080817   2017020300002   PEMA  Limit and ben code assign changes.
      * 100317   2017020300002   PEMA  copy of message001
040622* 040622 CR2019012500003   PEMA  Migrate to SQLSERVER 2016
      ******************************************************************
       ENVIRONMENT DIVISION.
       data division.
       WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   WSMESS03 WORKING STORAGE     '.
       77  FILLER  PIC X(32) VALUE '********************************'.
      *
      * program buffers
      *
       77  ws-seq-num                  pic s9(8) comp value 0.
       77  ws-flags                    pic s9(8) comp value 0.
       77  WS-COMP-CD                  PIC X  VALUE LOW-VALUES.
       77  ws-comp-id                  pic xxx value spaces.
       77  WS-SAVE-ACCOUNT             PIC X(10)  VALUE SPACES.
       77  WS-BIN-ORIG-EFF-DT          PIC XX  VALUE LOW-VALUES.
       77  WS-ORIG-EFF-DT              PIC X(10)  VALUE SPACES.
       77  WS-EFF-DATE                 PIC X(10)  VALUE SPACES.
       77  WS-LF-EXP-DATE              PIC X(10)  VALUE SPACES.
       77  WS-AH-EXP-DATE              PIC X(10)  VALUE SPACES.
       77  WS-LOAN-EXP-DATE            PIC X(10)  VALUE SPACES.
       77  X1                          PIC S999 COMP-3 VALUE +0.
       77  S1                          PIC S999 COMP-3 VALUE +0.
       77  S2                          PIC S999 COMP-3 VALUE +0.
       77  S3                          PIC S999 COMP-3 VALUE +0.
       77  b1                          pic s999 comp-3 value +0.
       77  WS-BUILD-SW                 PIC X.
           88  TIME-TO-BUILD              VALUE 'Y'.
       77  WS-SAVE-ERACCT              PIC X(2000).
       77  WS-DIS-RESP                 PIC 9(05) VALUE ZEROS.
       77  WS-PERFORM-SW               PIC X VALUE SPACES.
           88  GET-RATES                   VALUE 'R'.
           88  GET-ACT-ACCTS               VALUE 'A'.
       77  ws-bin-current-dt           pic xx  value low-values.
       77  ws-bin-eff-dt               pic xx  value low-values.
       77  ws-bin-lf-exp-dt            pic xx  value low-values.
       77  ws-bin-ah-exp-dt            pic xx  value low-values.
       77  ws-bin-loan-exp-dt          pic xx  value low-values.
       77  ws-bin-1st-pmt-dt           pic xx  value low-values.
       77  ws-bin-pri-birth-dt         pic xx  value low-values.
       77  ws-bin-cob-birth-dt         pic xx  value low-values.
       77  WS-DISP-AMT                 PIC Z,ZZZ,Z99.99.
       77  ws-disp-rate                pic z9.99999.
       77  WS-ERACCT-SW                PIC X VALUE ' '.
           88  END-OF-ERACCT                 VALUE 'Y'.
       77  WS-ERCTBL-SW                PIC X VALUE ' '.
           88  END-OF-ERCTBL                 VALUE 'Y'.
       77  WS-STATUS                   PIC X.
       77  ws-socket-sw                pic x value ' '.
           88  end-of-socket              value 'Y'.
       77  rec-cnt                     pic 9(5) value zeros.
       77  ws-stop-sw                  pic x value ' '.
           88  i-say-stop                 value 'Y'.
       77  ws-browse-sw                pic x value ' '.
           88  browse-started            value 'Y'.
       77  ws-contract-sw              pic x  value ' '.
           88  contract-no-assigned      value 'Y'.
       77  ws-error-sw                 pic x value ' '.
           88  error-in-one-coverage     value 'Y'.
       77  ws-connect-sw               pic x value ' '.
           88  connected-to-db           value 'Y'.
       77  client-id                   pic xxx.
       77  ws-error-sub                pic 999  value zeros.
       77  ws-error-sup                pic x(25) value spaces.
       77  ws-form-limit-name          pic x(50) value spaces.
       77  l1                          pic s999 comp-3 value +0.
       77  ws-extension-days           pic 999 value zeros.
       77  d                           pic s9(5) comp-3 value +0.
       77  ws-life-coverage-sw         pic x value ' '.
           88  no-life-coverage          value 'Y'.
       
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

       01  a-angle-n                   pic s9(7)v9(11) comp-3 value +0.
       01  a-prm-angle-n               pic s9(7)v9(11) comp-3 value +0.
       01  gamma                       pic s9(7)v9(11) comp-5 value +0.
       01  n                           pic s9(4)v9(4)  comp-3 value +0.
       01  i                           pic s9(7)v9(11) comp-3 value +0.
       01  ws-ins-per-month            pic s9(5)v99 comp-3 value +0.
       01  ws-prem-plus-int            pic s9(5)v99 comp-3 value +0.
       01  ws-tot-pmts-wo-ins          pic s9(7)v99 comp-3 value +0.
       01  ws-principal                pic s9(7)v99 comp-3 value +0.
       01  ws-amt-financed             pic s9(7)v99 comp-3 value +0.
       01  ws-loan-payment             pic s9(5)v99 comp-3 value +0.
       01  ppy                         pic s9(5)    comp-3 value +0.
       01  dpp                         pic s9(5)    comp-3 value +0.
       01  ELCNTL-KEY.
           05  CNTL-COMP-ID         PIC XXX     VALUE SPACES.
           05  CNTL-REC-TYPE        PIC X       VALUE SPACE.
           05  CNTL-ACCESS          PIC X(4)    VALUE SPACES.
           05  CNTL-SEQ-NO          PIC S9(4)    COMP VALUE +0.

       01  ws-lf-coverage-type         pic x value spaces.
       01  ws-lf-special-calc-cd       pic x value spaces.
       01  ws-lf-joint-indicator       pic x value spaces.
       01  ws-lf-earning-calc          pic x value spaces.

       01  ws-ah-coverage-type         pic x value spaces.
       01  ws-ah-special-calc-cd       pic x value spaces.
       01  ws-ah-joint-indicator       pic x value spaces.
       01  ws-ah-earnings-calc         pic x value spaces.

       01  ws-state-chg-ext-days       pic x value spaces.

       EXEC SQL
          INCLUDE SQLDA
       END-EXEC
      
       EXEC SQL
          INCLUDE SQLCA
       END-EXEC
      
       EXEC SQL
          BEGIN DECLARE SECTION
       END-EXEC
      
       01  ws-dealer-state             pic xx.
       01  ws-dealer-id                pic x(10).
       01  ws-contract-eff-dt          pic x(10).
       01  ws-contract-no              pic x(10) value spaces.
       01  ws-contract-suffix          pic x     value spaces.
      
      
       01  ws-key-stuff.
           05  ws-ks-contract-no       pic x(10) value spaces.
           05  ws-batch-no             pic x(6) value zeros.
           05  ws-batch-no-n redefines
               ws-batch-no             pic 9(6).
           05  ws-check-key            pic 9(7).
           05  ws-check-no             pic x(7).
           05  ws-compid               pic xxx.
           05  ws-carrier              pic x.
           05  ws-grouping             pic x(6).
           05  ws-state                pic xx.
           05  ws-account              pic x(10).
           05  ws-eff-date             pic x(10).
           05  ws-certificate          pic x(10).
           05  ws-cert-sfx             pic x.
           05  ws-seq-no               pic 999.
           05  ws-type                 pic 999.
      
       01  ws-status-code-a            pic x(7) value zeros.
       01  ws-status-code redefines
           ws-status-code-a            pic 9(7).
       01  ws-error-message            pic x(50) value spaces.
       01  ws-status-date              pic x(10).
       01  sqlcmd                      pic x(1024).
       01  svr                         pic x(32).
       01  usr                         pic x(32).
       01  pass                        pic x(32).
       01  usr-pass                    pic x(64).
       01  ws-disp-code                pic s9(11).
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  These indicators are used to determine if a variable      ***
      ***  is        null.           The indicator will be -1        ***
      ***  if the value        is null  and +0 if the value is       ***
      ***  something other than null.  Here is an example on how     ***
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
      ***           OR This way on an update                         ***
      ***                                                            ***
      ***     EXEC SQL                                               ***
      ***        UPDATE                                              ***
      ***           CUC_Logic_Remittance                             ***
      ***        SET                                                 ***
      ***           LogicStatus     = :ws-status-code,               ***
      ***           LogicStatusDate = :ws-status-date,               ***
      ***           BatchNumber     = :ws-batch-no :nu-batchno       ***
      ***        WHERE                                               ***
      ***           RemitId = :ws-remit-id                           ***
      ***     END-EXEC                                               ***
      ***                                                            ***
      ***    Also, when the table has a column with a data type of   ***
      ***  "BIT" and used as true/false move the 1 byte receiving    ***
      ***  field to ws-bit and check out ws-bit-comp. if = zeros,    ***
      ***  then its false. I think true would be 256.                ***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
      
       01  ws-bit                      pic x.
       01  ws-bit-comp redefines ws-bit pic s9(4) comp.
       01  indicator-vaiables-for-nulls.
           05  nu-partial-cov          pic s9(4) comp value +0.
           05  nu-check-elig           pic s9(4) comp value +0.
           05  nu-app-status           pic s9(4) comp value +0.
           05  nu-app-by               pic s9(4) comp value +0.
           05  nu-app-date             pic s9(4) comp value +0.
           05  nu-app-batch            pic s9(4) comp value +0.
           05  nu-fincar               pic s9(4) comp value +0.
           05  nu-batchno              pic s9(4) comp value +0.
           05  nu-error-message        pic s9(4) comp value +0.
      
       01  sql-cert-records.
           05  sql-dlr-state           pic xx.
           05  sql-dlr-id              pic x(10).
           05  sql-eff-dt              pic x(10).
           05  sql-contr-no            pic x(10).
           05  sql-contr-suffix        pic x.
      
       01  ws-limit-issue-age          pic 999.
       01  ws-att-age                  pic 999.
       01  form-limit-table.
           05  ws-limit-name           pic x(50).
           05  ws-limit-cov-type       pic xx.
           05  ws-limit-lo-age         pic 999.
           05  ws-limit-hi-age         pic 999.
           05  ws-limit-att-age        pic 999.
           05  ws-limit-max-term       pic 999.
           05  ws-limit-max-jnt-term   pic 999.
           05  ws-limit-max-mo-ben     pic 9(5).
           05  ws-limit-max-tot-ben    pic 9(7).
           05  ws-limit-partial-cov    pic s9(4) comp-5.
           05  ws-limit-check-elig     pic s9(4) comp-5.
           05  ws-limit-elig-max-term  pic 999.
      
       01  state-benefit-code-table.
           05  ws-sbc-min-amt          pic 999999.
           05  ws-sbc-state            pic xx.
           05  ws-sbc-cov-type         pic xx.
           05  ws-sbc-ben-amt          pic 999999.
           05  ws-sbc-sin-jnt          pic x.
           05  ws-sbc-dismember        pic x.
           05  ws-sbc-retroelim        pic x.
           05  ws-sbc-wait-days        pic 999.
           05  ws-sbc-max-bens         pic 999.
           05  ws-sbc-logic-ben-code   pic xx.
      
       EXEC SQL
          END DECLARE SECTION
       END-EXEC
      
       01  filler.
           05  ws-work-in              pic x(10).
           05  ws-work-out             pic x(10).
           05  ws-work-out-v2 redefines
               ws-work-out             pic 9(8)v99.
           05  ws-work-out-v0 redefines
               ws-work-out             pic 9(10).
           05  ws-work-out-v5 redefines
               ws-work-out             pic 9(5)v9(5).
       01  filler.
           05  ws-last-suffix          pic x value low-values.
           05  ws-tbl-last-suffix      pic x value low-values.
           05  filler.  *> Use X1 for this table.
               10  ws-codes            pic x(26) value
               ' ABCDEFGHIJKLMNOPQRSTUVWXY'.
               10  ws-suffix-value redefines ws-codes
                 occurs 26             pic x.
      
       01  ws-work-date.
           05  ws-work-ccyy            pic x(4).
           05  ws-work-mm              pic xx.
           05  ws-work-dd              pic xx.
       01  ws-work-date-num redefines ws-work-date
                                       pic 9(8).
      
       01  raw-message003-area.
           05  raw-message-num         pic x(10).
           05  raw-state               pic xx.
           05  raw-acct-no             pic x(10).
           05  raw-vin                 pic x(17).
           05  raw-lf-ben-code         pic xx.
           05  raw-ah-ben-code         pic xx.
           05  raw-earn-meth           pic x.
           05  raw-pri-birth-date      pic x(10).
           05  raw-cob-birth-date      pic x(10).
           05  raw-loan-amt            pic x(9).
           05  raw-eff-date            pic x(10).
           05  raw-1st-pmt-dt          pic x(10).
           05  raw-pmts-per-year       pic xx.
           05  raw-lf-premium          pic x(8).
           05  raw-ah-premium          pic x(8).
           05  raw-loan-term           pic xxx.
           05  raw-lf-term             pic XXX.
           05  raw-ah-term             pic xxx.
           05  raw-apr                 pic x(8).
           05  raw-lf-sin-jnt-ind      pic x.
           05  raw-ah-sin-jnt-ind      pic x.
           05  raw-lf-dismemberment    pic x.
           05  raw-retro-elim          pic x.
           05  raw-waiting-days        pic xx.
           05  raw-crit-per            pic xx.
           05  raw-total-payments      pic x(9).
           05  raw-period-payment      pic x(8).

       01  ws-rate-work-area.
           05  ws-rate-state           pic xx.
           05  ws-rate-acct-no         pic x(10).
           05  ws-rate-vin             pic x(17).
           05  ws-rate-in-lf-ben-code  pic xx.
           05  ws-rate-in-ah-ben-code  pic xx.
           05  ws-rate-earn-meth       pic x.
           05  ws-rate-pri-birth-date  pic x(10).
           05  ws-rate-cob-birth-date  pic x(10).
           05  ws-rate-eff-date        pic x(10).
           05  ws-rate-1st-pmt-dt      pic x(10).
           05  ws-rate-benefit-type    pic x.
           05  ws-rate-lf-benefit-cd   pic xx.
           05  ws-rate-ah-benefit-cd   pic xx.
           05  ws-rate-benefit         pic 9(6)v99.
           05  ws-rate-loan-pmt        pic 9(7)v99.
           05  ws-rate-tot-pmts        pic 9(6)v99.
           05  ws-rate-lf-prem         pic 9(5)v99.
           05  ws-rate-ah-prem         pic 9(5)v99.
           05  ws-rate-new-lf-prem     pic 9(5)v99.
           05  ws-rate-new-ah-prem     pic 9(5)v99.
           05  ws-rate-lf-rate         pic 99v9(5).
           05  ws-rate-ah-rate         pic 99v9(5).
           05  ws-rate-apr             pic 99v9(5).
           05  ws-rate-pmts-per-year   pic 99.
           05  ws-rate-loan-term       pic 999  value zeros.
           05  ws-rate-lf-term         pic 999  value zeros.
           05  ws-rate-ah-term         pic 999  value zeros.
           05  ws-issue-age            pic 999  value zeros.
           05  ws-cob-age              pic 999  value zeros.
           05  ws-rate-age             pic 999  value zeros.
           05  ws-max-lf-benefit       pic 9(7)v99 value zeros.
           05  ws-max-ah-benefit       pic 9(7)v99 value zeros.
           05  ws-rate-crit-per        pic 99.
           05  ws-rate-sin-jnt-lf      pic x.
           05  ws-rate-sin-jnt-ah      pic x.
           05  ws-rate-dismemberment   pic x.
           05  ws-rate-retro-elim      pic x.
           05  ws-rate-waiting-days    pic 99.
           05  ws-rate-payment         pic 9(5)v99.
           05  ws-calc-tot-pmts        pic 9(7)v99 value zeros.
           05  ws-rate-amt-financed    pic 9(7)v99 value zeros.
           05  ws-rate-loan-type       pic x.
      
       01  ws-lf-limits.
           05  ws-lf-limit-lo-age         pic 999.
           05  ws-lf-limit-hi-age         pic 999.
           05  ws-lf-limit-att-age        pic 999.
           05  ws-lf-limit-max-term       pic 999.
           05  ws-lf-limit-max-benefit    pic 9(7).
           05  ws-lf-limit-partial-cov    pic s9(4) comp-5.
           05  ws-lf-limit-check-elig     pic s9(4) comp-5.
           05  ws-lf-limit-elig-max-term  pic 999.
      
       01  ws-di-limits.
           05  ws-di-limit-lo-age         pic 999.
           05  ws-di-limit-hi-age         pic 999.
           05  ws-di-limit-att-age        pic 999.
           05  ws-di-limit-max-term       pic 999.
           05  ws-di-limit-max-jnt-term   pic 999.
           05  ws-di-limit-max-mo-ben     pic 9(5).
           05  ws-di-limit-max-tot-ben    pic 9(7).
           05  ws-di-limit-partial-cov    pic s9(4) comp-5.
           05  ws-di-limit-check-elig     pic s9(4) comp-5.
           05  ws-di-limit-elig-max-term  pic 999.
      
       01  WS-AM-KEY.
           05  WS-AM-COMPANY-CD        PIC X.                                       
           05  WS-AM-CARRIER           PIC X.                                       
           05  WS-AM-GROUP             PIC X(6).                                    
           05  WS-AM-STATE             PIC XX.   
           05  WS-AM-ACCOUNT           PIC X(10).
           05  WS-AM-EXP-DT            PIC XX.
           05  FILLER                  PIC X(4).
      
       01  ws-cm5-compare-key          pic x(12).
       01  WS-CM5-KEY.
           05  WS-CM5-COMPANY-CD       PIC X.                                       
           05  WS-CM5-CERT-NO          PIC X(11).
      
       01  ws-cm-compare-key           pic x(33).
       01  WS-CM-KEY.
           05  WS-CM-COMPANY-CD        PIC X.                                       
           05  WS-CM-CARRIER           PIC X.                                       
           05  WS-CM-GROUP             PIC X(6).                                    
           05  WS-CM-STATE             PIC XX.   
           05  WS-CM-ACCOUNT           PIC X(10).
           05  WS-CM-EFF-DT            PIC XX.
           05  WS-CM-CERT-NO.
               10  ws-cm-cert-ten      pic x(10).
               10  ws-cm-cert-suffix   pic x.
      
       01  WS-CS-KEY.
           05  WS-CS-COMPANY-CD        PIC X.                                       
           05  WS-CS-CARRIER           PIC X.                                       
           05  WS-CS-GROUP             PIC X(6).                                    
           05  WS-CS-STATE             PIC XX.   
           05  WS-CS-ACCOUNT           PIC X(10).
           05  WS-CS-EFF-DT            PIC XX.
           05  WS-CS-CERT-NO           PIC X(11).
           05  WS-CS-TRLR-TYPE         PIC X.
      
       01  WS-AM-ALT-KEY.
           05  WS-AM-ALT-ACCOUNT       PIC X(10).
           05  WS-AM-ALT-EXP-DT        PIC XX.
      
       01  WS-CF-KEY-SAVE              PIC X(10).
       01  WS-CF-KEY.
           05  WS-CF-COMPANY-ID        PIC XXX.
           05  WS-CF-RECORD-TYPE       PIC X.
           05  WS-CF-ACCESS            PIC X(4).
           05  WS-CF-SEQ-NO            PIC S9(4) COMP.
           
       01  filler.
           05  ws-errors-table.
               10  filler              pic x(130) value                    
               '0000Transaction successfully completed'.                  
               10  filler              pic x(130) value                    
               '0101Amount entered contains invalid characters. Please c
      -        'orrect and re-submit'.
               10  filler              pic x(130) value                    
               '0102Date entered is invalid. Please correct and re-submi
      -        't'.
               10  filler              pic x(130) value                    
               '0103Customer age exceeds age limit. Not eligible for cov
      -        'erage'.
               10  filler              pic x(130) value                    
               '0104Customer will exceed age limit during term of covera
      -        'ge. Not eligible for coverage'.
               10  filler              pic x(130) value                    
               '0105Invalid loan term. Term must be more than 0 and less
      -        ' than 360'.
               10  filler              pic x(130) value
               '0106Insurance term exceeds maximum term allowed. Loan is
      -        ' not eligible for credit insurance'.
               10  filler              pic x(130) value
               '0107Insurance term exceeds maximum term allowed. Loan is
      -        ' not eligible for Joint credit disability insurance'.
               10  filler              pic x(130) value
               '0108Insurance term exceeds maximum term allowed. Loan is
      -        ' not eligible for credit disability insurance'.
               10  filler              pic x(130) value
               '0109Total of payments exceeds maximum allowed. Loan is n
      -        'ot eligible for credit insurance'.
               10  filler              pic x(130) value                    
               '0110Monthly payment exceeds maximum allowed. Loan is not
      -        ' eligible for disability insurance'.
               10  filler              pic x(130) value                    
               '0111Loan term cannot be less than insurance term. Loan i
      -        's not eligible for credit insurance'.
               10  filler              pic x(130) value                    
               '0112Term times monthly payment exceeds Maximum total dis
      -        'ability benefit. Loan is not eligible for credit disabil
      -        'ity insurance'.
               10  filler              pic x(130) value
               '0113Cannot rate based on existing loan terms'.
               10  filler              pic x(130) value                    
               '0114Cannot rate life coverage based on existing loan ter
      -        'ms'.
               10  filler              pic x(130) value                    
               '0115Cannot rate disability coverage based on existing lo
      -        'an terms'.
               10  filler              pic x(130) value                    
               '0116Problem with rate file. Unable to rate'.              
               10  filler              pic x(130) value                    
               '0117Calculated premium is outside of tolerance'.          
               10  filler              pic x(130) value                    
               '0118This appears to be a duplicate record. Unable to rat
      -        'e'.
               10  filler              pic x(130) value                    
               '0119Loan term and insurance term cannot be zero. Unable 
      -        'to rate'.
               10  filler              pic x(130) value                    
               '0120Loan term and insurance term cannot be zero. Unable 
      -        'to rate'.
               10  filler              pic x(130) value
               '0121Cannot locate limit table. Unable to rate'.
               10  filler              pic x(130) value
               '0122Invalid company id '.
               10  filler              pic x(130) value                    
               '0123Cannot locate life benefit code. Unable to rate'.     
               10  filler              pic x(130) value                    
               '0124Cannot locate disability benefit code. Unable to rat
      -        'e'.
               10  filler              pic x(130) value                    
               '0125Calculated total of payments do not match submitted 
      -        'total of payments. Unable to rate'.
               10  filler              pic x(130) value                    
               '0126Life and Disability Terms must be the same'.
               10  filler              pic x(130) value                    
               '0127Loan term must match insurance term on gross coverag
      -        'e. Unable to rate'.
               10  filler              pic x(130) value                    
               '0128Total of payments does not match benefit amount prov
      -        'ided on Gross coverage. Unable to rate'.
               10  filler              pic x(130) value                    
               '0129Calculated amount financed does not match provided a
      -        'mount financed. Unable to rate'.
               10  filler              pic x(130) value
               '0130Insurance term exceeds maximum eligibility term. Loa
      -        'n is not eligible for credit insurance'.
               10  filler              pic x(130) value
               '0131Amount exceeds maximum eligibility amount. Loan is n
      -        'ot eligible for credit insurance'.
               10  filler              pic x(130) value
               '0132Disability only certificate is not allowed. Loan is 
      -        'not eligible for credit insurance'.
           05  filler redefines ws-errors-table occurs 33.
               10  ws-table-error-no   pic x(4).
               10  ws-table-error-mess pic x(126).
      
       01  ws-return-string.
           05  ws-return-error-no      pic x(4).
           05  ws-sc1                  pic x.
           05  ws-return-error-mess    pic x(150).
           05  ws-sc2                  pic x.
           05  ws-return-contract-no   pic x(11).
           05  ws-sc3                  pic x.
           05  ws-return-lf-max-ben    pic z,zzz,z99.99.
           05  ws-sc4                  pic x.
           05  ws-return-ah-max-amt    pic z,zzz,z99.99.
           05  ws-sc5                  pic x.
           05  ws-return-lf-prem       pic z,zzz,z99.99.
           05  ws-sc6                  pic x.
           05  ws-return-ah-prem       pic z,zzz,z99.99.
           05  ws-sc7                  pic x.
           05  ws-return-lf-rate       pic z9.99999.
           05  ws-sc8                  pic x.
           05  ws-return-ah-rate       pic z9.99999.
           05  ws-sc9                  pic x.
           05  ws-return-lf-exp-dt     pic x(10).
           05  ws-sc10                 pic x.
           05  ws-return-ah-exp-dt     pic x(10).
           05  ws-sc11                 pic x.
           05  ws-return-lf-benefit-cd pic xx.
           05  ws-sc12                 pic x.
           05  ws-return-ah-benefit-cd pic xx.
           05  ws-sc13                 pic x.
           05  ws-return-period-pmt    pic zzz,z99.99.
           05  ws-sc14                 pic x.
           05  ws-return-tot-financed  pic z,zzz,z99.99.
           05  ws-sc15                 pic x.
           05  ws-return-tot-pmts      pic z,zzz,z99.99.
           05  ws-sc16                 pic x.
           05  ws-return-loan-exp-dt   pic x(10).
           05  ws-sc17                 pic x.
           05  ws-return-limit-name    pic x(50).
           05  ws-sc18                 pic x.
           05  ws-return-rate-class    pic xx.
           05  ws-sc19                 pic x.
           05  ws-return-loan-pmt      pic zzz,z99.99.
           05  ws-sc20                 pic x.
           05  ws-return-principal     pic zzz,z99.99.
           05  ws-sc21                 pic x.
           05  ws-return-lf-term       pic zz9.
           05  ws-sc22                 pic x.
           05  ws-return-ah-term       pic zz9.
           05  ws-sc23                 pic x.
           05  ws-return-lf-bencd      pic xx.
           05  ws-sc24                 pic x.
           05  ws-return-max-bens      pic 99.


       01  ws-mess003-length           pic s9(4) comp value +1024.
       01  WS-MESS003-PASS-AREA        PIC X(1024).
       01  WS-CID-NO                   PIC X(8).
      
       01  WS-DISP-RESP                PIC 9(5).
       01  WS-RESPONSE                 PIC S9(8)   COMP.
           88  RESP-NORMAL                  VALUE +00.
           88  RESP-NOTFND                  VALUE +13.
           88  resp-duprec                  value +14.
           88  resp-dupkey                  value +15.
           88  RESP-NOTOPEN                 VALUE +19.
           88  RESP-ENDFILE                 VALUE +20.
      
                                        COPY ERCACCT.
                                        COPY ELCCERT.
                                        COPY ELCCRTT.
                                        COPY ELCCNTL.
                                        COPY ELCFUNDT.
                                        COPY ELCDATE.
                                        COPY ELCCALC.
      
       linkage section.
       01  DFHCOMMAREA                 pic x(1024).
      
       01  var  pic x(30).
      
       procedure division.
      
           display ' Entering program WSMESS03 '
           move dfhcommarea            to ws-mess003-pass-area
      
           perform 0000-init           thru 0000-exit
      
           display ' mess003 area ' ws-mess003-pass-area

           perform 0010-process-message
                                       thru 0010-exit
      *    move ' returning string goes here ' to ws-return-string
      
           go to 0300-RETURN-CICS
      
           .
       0000-init.
      
           move 'CID'                  to client-id
      
           MOVE FUNCTION CURRENT-DATE  TO FUNCTION-DATE
           move ws-fn-cymd             to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-current-dt
           else
              display ' error current dt invalid ' dc-error-code
           end-if
      
           set P to address of KIXSYS
           CALL "getenv" using by value P returning var-ptr
           if var-ptr = null then
              display ' kixsys not set '
           else
              set address of var to var-ptr
              move 0 to env-var-len
              inspect var tallying env-var-len
                for characters before X'00' 
              unstring var (1:env-var-len) delimited by '/'
                 into WS-KIX-FIL1 WS-KIX-APPS WS-KIX-ENV WS-KIX-MYENV
                    WS-KIX-SYS
              end-unstring
           end-if
      
           evaluate client-id
              when 'DCC'
                 MOVE X'05'            TO WS-COMP-CD
                 MOVE 'DCC'            TO WS-COMP-ID
              when 'CID'
                 MOVE X'04'            TO WS-COMP-CD
                 MOVE 'CID'            TO WS-COMP-ID
              when 'AHL'
                 MOVE X'06'            TO WS-COMP-CD
                 MOVE 'AHL'            TO WS-COMP-ID
              when other
                 display ' Invalid company id ' client-id
                 move 23               to ws-error-sub
                 move client-id        to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
           END-evaluate
           move spaces                 to ws-return-string
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
                                          ws-sc6
                                          ws-sc7
                                          ws-sc8
                                          ws-sc9
                                          ws-sc10
                                          ws-sc11
                                          ws-sc12
                                          ws-sc13
                                          ws-sc14
                                          ws-sc15
                                          ws-sc16
                                          ws-sc17
                                          ws-sc18
                                          ws-sc19
                                          ws-sc20
                                          ws-sc21
                                          ws-sc22
                                          ws-sc23
                                          ws-sc24

           .
       0000-exit.
           exit.
      
       0010-process-message.
      
           perform 0110-unstring       thru 0110-exit
           perform 0120-format-message thru 0120-exit
      
           perform 0020-edit-received  thru 0020-exit
           perform 0025-assign-ben-cd  thru 0025-exit
      
           perform 0050-get-account    thru 0050-exit
           perform 0090-get-limits     thru 0090-exit
           perform 0058-get-state-controls
                                       thru 0058-exit
           perform 0030-get-lf-rate    thru 0030-exit
           perform 0035-get-ah-rate    thru 0035-exit

           if (contract-no-assigned)
                  or
              (error-in-one-coverage)
              continue
           else
              perform 0060-check-for-dup
                                       thru 0060-exit
              perform 0070-open-cursor thru 0070-exit
              perform 0080-process-input
                                       thru 0080-exit
           end-if
           perform 0040-format-buffer  thru 0040-exit
           set contract-no-assigned to true
      
           .
       0010-exit.
           exit.
      
       0020-edit-received.

           if ws-rate-sin-jnt-lf = '*'
              set no-life-coverage to true
           end-if
      
           if ws-rate-sin-jnt-lf = 'S' OR 'J'
              continue
           else
              move 'S'                 to ws-rate-sin-jnt-lf
           end-if
           if ws-rate-sin-jnt-ah = 'S' OR 'J'
              continue
           else
              move 'S'                 to ws-rate-sin-jnt-ah
           end-if
           if ws-rate-dismemberment = 'Y'
              continue
           else
              move 'N'                 to ws-rate-dismemberment
           end-if

           if ws-rate-in-ah-ben-code = 'A'
              if ws-rate-retro-elim = 'R' or 'E'
                 continue
              else
                 move 'R'              to ws-rate-retro-elim
              end-if
           else
              move spaces              to ws-rate-retro-elim
           end-if

      **==============================================================**
      **                                                              **
      **  Validate passed effective date. Probably should add edits   **
      **                                                              **
      **==============================================================**
      
           move ws-rate-eff-date (7:4) to ws-work-date (1:4)
           move ws-rate-eff-date (1:2) to ws-work-date (5:2)
           move ws-rate-eff-date (4:2) to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-eff-dt
           else
              move 3                   to ws-error-sub
              move '- Eff Date'        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if
      
           if ws-bin-eff-dt > ws-bin-current-dt
              move 3                   to ws-error-sub
              move '- Future Eff'      to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if
      
      **==============================================================**
      **                                                              **
      **  Validate passed 1st pmt date.   Probably should add edits   **
      **                                                              **
      **==============================================================**
      
           move ws-rate-1st-pmt-dt (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-1st-pmt-dt (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-1st-pmt-dt (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-1st-pmt-dt
           else
              move low-values          to ws-bin-1st-pmt-dt
              move 3                   to ws-error-sub
              move '- 1st Pmt Dt'      to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if
      
           if ws-bin-1st-pmt-dt <= ws-bin-eff-dt
              move low-values          to ws-bin-1st-pmt-dt
              move 3                   to ws-error-sub
              move '- 1st Pmt !> Eff dt' to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if
      
      **==============================================================**
      **                                                              **
      **  Only use the code below if there is a time they don't       **
      **  send the 1st pmt date.                                      **
      **                                                              **
      **==============================================================**
      
           if ws-bin-1st-pmt-dt = low-values
              move +1                  to dc-elapsed-months
              move +0                  to dc-elapsed-days
              move '6'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-2    to ws-bin-1st-pmt-dt
              else
                 move 3                to ws-error-sub
                 move '- 1st Pmt Dt'   to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if
      
      **==============================================================**
      **                                                              **
      **  Calculate the days to first payment date                    **
      **                                                              **
      **==============================================================**

           move ws-bin-eff-dt          to dc-bin-date-1
           move ws-bin-1st-pmt-dt      to dc-bin-date-2
           move zeros                  to dc-elapsed-months
                                          dc-elapsed-days
           move '1'                    to dc-option-code
           perform 9700-date-link   thru 9700-exit
           if no-conversion-error
              move dc-elapsed-days     to d
           else
              move 30                  to d
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate the lf expiration date to be passed back.         **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-lf-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-lf-exp-dt
              move dc-greg-date-a-edit to ws-lf-exp-date
           else
              move 3                   to ws-error-sub
              move '- Lf Exp Dt'       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate the ah expiration date to be passed back.         **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-ah-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-ah-exp-dt
              move dc-greg-date-a-edit to ws-ah-exp-date
           else
              move 3                   to ws-error-sub
              move '- Ah Exp Dt'       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate the loan expiration date to be passed back.       **
      **                                                              **
      **==============================================================**

           move ws-bin-1st-pmt-dt      to dc-bin-date-1
           compute dc-elapsed-months = ws-rate-loan-term - 1
           move +0                     to dc-elapsed-days
           move '6'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-2       to ws-bin-loan-exp-dt
              move dc-greg-date-a-edit to ws-loan-exp-date
           else
              move 3                   to ws-error-sub
              move '- Loan Exp Dt'     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate primary borrower birth date.                       **
      **                                                              **
      **==============================================================**
      
           move ws-rate-pri-birth-date (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-pri-birth-date (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-pri-birth-date (4:2)
                                       to ws-work-date (7:2)
           move ws-work-date-num       to dc-greg-date-cymd
           move 'L'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              move dc-bin-date-1       to ws-bin-pri-birth-dt
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if
      
      **==============================================================**
      **                                                              **
      **  Validate co borrowers birth date.                           **
      **                                                              **
      **==============================================================**
      
           if ws-rate-cob-birth-date (7:4) = spaces
              move zeros               to ws-rate-cob-birth-date
           end-if
           move ws-rate-cob-birth-date (7:4)
                                       to ws-work-date (1:4)
           move ws-rate-cob-birth-date (1:2)
                                       to ws-work-date (5:2)
           move ws-rate-cob-birth-date (4:2)
                                       to ws-work-date (7:2)
           move low-values             to ws-bin-cob-birth-dt
           if ws-work-date-num not = zeros
              move ws-work-date-num    to dc-greg-date-cymd
              move 'L'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 move dc-bin-date-1    to ws-bin-cob-birth-dt
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if
      
      **==============================================================**
      **                                                              **
      **  Calculate primary borrowers age.                            **
      **                                                              **
      **==============================================================**
      
           move ws-bin-pri-birth-dt    to dc-bin-date-1
           move ws-bin-eff-dt          to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              compute ws-issue-age = dc-elapsed-months / +12
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if
      
      **==============================================================**
      **                                                              **
      **  Calculate primary borrowers age at expiration date          **
      **                                                              **
      **==============================================================**

           move ws-bin-pri-birth-dt    to dc-bin-date-1
           move ws-bin-lf-exp-dt       to dc-bin-date-2
           if ws-bin-ah-exp-dt > dc-bin-date-2
              move ws-bin-ah-exp-dt    to dc-bin-date-2
           end-if
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              compute ws-att-age = dc-elapsed-months / +12
           else
              move 3                   to ws-error-sub
              move '- Pri DOB '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Calculate co borrowers age.                                 **
      **                                                              **
      **==============================================================**
      
           move zeros                  to ws-cob-age
           if ws-bin-cob-birth-dt not = low-values
              move ws-bin-cob-birth-dt to dc-bin-date-1
              move ws-bin-eff-dt       to dc-bin-date-2
              move '1'                 to dc-option-code
              perform 9700-date-link   thru 9700-exit
              if no-conversion-error
                 compute ws-cob-age = dc-elapsed-months / +12
              else
                 move 3                to ws-error-sub
                 move '- Cob DOB '     to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if
      
           move ws-issue-age           to ws-rate-age
           if ws-cob-age > ws-rate-age
              move ws-cob-age          to ws-rate-age
           end-if
      
      **==============================================================**
      **                                                              **
      **  Calculate extension days.                                   **
      **                                                              **
      **==============================================================**
      
           move zeros                  to ws-extension-days
           move ws-bin-eff-dt          to dc-bin-date-1
           move ws-bin-1st-pmt-dt      to dc-bin-date-2
           move '1'                    to dc-option-code
           perform 9700-date-link      thru 9700-exit
           if no-conversion-error
              if dc-elapsed-days > 31
                 compute ws-extension-days =
                    dc-elapsed-days - 30
              end-if
           end-if
      
      **==============================================================**
      **                                                              **
      **  Validate passed total of payments. loan term * payment      **
      **                                                              **
      **==============================================================**
      
           if ws-rate-loan-type = '2'  *>   lease
              if ws-rate-tot-pmts = 0
                 compute ws-rate-tot-pmts =
                    ws-rate-loan-term * ws-rate-payment
              end-if
           end-if

           if ws-rate-loan-type = '2'  *> means it's a Lease
              move ws-rate-tot-pmts    to ws-calc-tot-pmts
           else
              compute ws-calc-tot-pmts =
                 ws-rate-loan-term * ws-rate-payment
           end-if

           if ((ws-calc-tot-pmts - ws-rate-tot-pmts) > 1.00)
                           or
              ((ws-rate-tot-pmts - ws-calc-tot-pmts) > 1.00)
      *    if ws-calc-tot-pmts <> ws-rate-tot-pmts
              move 26                  to ws-error-sub
              move spaces              to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate life and dis terms                                 **
      **                                                              **
      **==============================================================**

           if (ws-rate-lf-term > 0)
              and (ws-rate-ah-term > 0)
              if ws-rate-lf-term <> ws-rate-ah-term
                 move 27               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

      **==============================================================**
      **                                                              **
      **  Validate Loan term against insurance term.                  **
      **                                                              **
      **==============================================================**


           if ((ws-rate-in-lf-ben-code (1:1) = 'G')
              and (ws-rate-lf-term > 0)
              and (ws-rate-lf-term <> ws-rate-loan-term))
                        or
              ((ws-rate-in-ah-ben-code (1:1) <> ' ')
              and (raw-lf-ben-code = ' ')
              and (ws-rate-ah-term > 0)
              and (ws-rate-ah-term <> ws-rate-loan-term))
              move 28               to ws-error-sub
              move ' '              to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-RETURN-CICS
           end-if

      **==============================================================**
      **                                                              **
      **  Validate total of payments against benefit amount for gross **
      **                                                              **
      **==============================================================**
022818**  just learned that the rate benefit will be the amt financed
      **  and not the total of payments so I commented out this error

           if (ws-rate-in-lf-ben-code (1:1) = 'G' or 'L')
              and (ws-rate-tot-pmts <> ws-rate-benefit)
              move ws-rate-tot-pmts    to ws-rate-benefit
           end-if

031618**  just learned that we are verifying leases.
           if ws-rate-loan-type = '2'
              if ws-rate-in-lf-ben-code (1:1) = 'G' or 'L'
                 compute ws-rate-benefit =
                    ws-rate-payment * (ws-rate-lf-term - 1)
              else
                 if ws-rate-in-ah-ben-code (1:1) = 'A' 
                    compute ws-rate-benefit =
                       ws-rate-payment * (ws-rate-ah-term - 1)
                 end-if
              end-if
           end-if

           move ws-rate-benefit        to ws-rate-amt-financed

      **   if (ws-rate-in-lf-ben-code (1:1) = 'G')
      **      and (ws-rate-tot-pmts <> ws-rate-benefit)
      **      move 29               to ws-error-sub
      **      move ' '              to ws-error-sup
      **      perform 0180-error-handle
      **                               thru 0180-exit
      **      go to 0300-RETURN-CICS
      **   end-if

           .
       0020-exit.
           exit.
      
       0025-assign-ben-cd.

           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

           move ws-rate-state          to ws-sbc-state

           evaluate true
              when ws-rate-in-lf-ben-code (1:1) = 'N'
                 move 'NP'             to ws-sbc-cov-type
              when ws-rate-in-lf-ben-code (1:1) = 'T'
                 move 'NT'             to ws-sbc-cov-type
              when ws-rate-in-lf-ben-code (1:1) = 'L'
                 move 'LL'             to ws-sbc-cov-type
              when ws-rate-in-lf-ben-code (1:1) = ' '
                 go to 0025-get-ah-ben
              when other
                 move 'GP'             to ws-sbc-cov-type
           end-evaluate

           move ws-rate-benefit        to ws-sbc-ben-amt
           move ws-rate-sin-jnt-lf     to ws-sbc-sin-jnt
           move ws-rate-dismemberment  to ws-sbc-dismember


           EXEC SQL
              SELECT TOP (1)
                 MIN(BenefitAmt) as LessAmt,
                 LogicBenCode
              INTO
                 :ws-sbc-min-amt,
                 :ws-sbc-logic-ben-code
              FROM
                 BenefitCodeMapping
              WHERE
                 State           = :ws-sbc-state
                 and CovType     = :ws-sbc-cov-type
                 and SinJnt      = :ws-sbc-sin-jnt
                 and Dismem      = :ws-sbc-dismember
                 and BenefitAmt  > :ws-sbc-ben-amt
              GROUP BY
                 LogicBenCode
              ORDER BY
                 LessAmt
           end-exec

           if sqlcode not = 0 and 1
              display "Error: cannot find lf benefit code "
                 ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
                 ' ' ws-sbc-dismember
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 24                  to ws-error-sub
              move spaces to ws-error-sup
              string
                 ws-sbc-state ' '
                 ws-sbc-cov-type ' '
                 ws-sbc-sin-jnt ' '
                 ws-sbc-dismember
                 delimited by size into ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move ws-sbc-logic-ben-code  to ws-rate-lf-benefit-cd

           .
       0025-get-ah-ben.

           if ws-rate-in-ah-ben-code (1:1) = ' '
              go to 0025-exit
           end-if

           move ws-rate-state          to ws-sbc-state
           move 'DI'                   to ws-sbc-cov-type
           move ws-rate-benefit        to ws-sbc-ben-amt
           move ws-rate-sin-jnt-ah     to ws-sbc-sin-jnt
           move ws-rate-retro-elim     to ws-sbc-retroelim
           move ws-rate-waiting-days   to ws-sbc-wait-days
           move ws-rate-crit-per       to ws-sbc-max-bens

           EXEC SQL
              SELECT TOP (1)
                 MIN(BenefitAmt) as LessAmt,
                 LogicBenCode
              INTO
                 :ws-sbc-min-amt,
                 :ws-sbc-logic-ben-code
              FROM
                 BenefitCodeMapping
              WHERE
                 State           = :ws-sbc-state
                 and CovType     = :ws-sbc-cov-type
                 and SinJnt      = :ws-sbc-sin-jnt
                 and RetroElim   = :ws-sbc-retroelim
                 and WaitDays    = :ws-sbc-wait-days
                 and MaxBens     = :ws-sbc-max-bens
                 and BenefitAmt  > :ws-sbc-ben-amt
              GROUP BY
                 LogicBenCode
              ORDER BY
                 LessAmt
           end-exec

           if sqlcode not = 0
              display "Error: cannot find ah benefit code "
                 ws-sbc-state ' ' ws-sbc-cov-type ' ' ws-sbc-sin-jnt
                 ' ' ws-sbc-retroelim ' ' ws-sbc-wait-days ' '
                 ws-sbc-max-bens

              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 25                  to ws-error-sub
              move spaces              to ws-error-sup
              string
                 ws-sbc-state ' '
                 ws-sbc-cov-type ' '
                 ws-sbc-sin-jnt ' '
                 ws-sbc-retroelim ' '
                 ws-sbc-wait-days ' '
                 ws-sbc-max-bens
                 delimited by size into ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move ws-sbc-logic-ben-code  to ws-rate-ah-benefit-cd

           .
       0025-exit.
           exit.

       0030-get-lf-rate.

           if raw-lf-ben-code (1:1) = ' '
              go to 0030-exit
           end-if

           perform 0054-get-lf-ben-code thru 0054-exit

      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months
                                          cp-term-or-ext-days
           move am-cal-table           to cp-class-CODE

           move am-lf-deviation        to cp-deviation-code

           move ws-rate-state          to cp-state
                                          cp-state-std-abbrv
           move ws-state-chg-ext-days  to cp-ext-days-calc
           move ws-lf-coverage-type    to cp-benefit-type
           move ws-rate-lf-benefit-cd  to cp-benefit-cd
           move ws-rate-benefit        to cp-original-benefit
                                          cp-rating-benefit-amt
           move ws-rate-age            to cp-issue-age
           move ws-comp-id             to cp-company-id
           move ws-comp-cd             to cp-company-cd

           move ws-rate-apr            to cp-loan-apr

           if ws-rate-loan-term = zeros
              move ws-rate-lf-term     to ws-rate-loan-term
           end-if
           move ws-rate-lf-term        to cp-original-term
           move ws-rate-loan-term      to cp-loan-term

           if ws-rate-loan-type = '2'   *>  Lease
              compute cp-original-term = 
                 cp-original-term - 1
              compute cp-loan-term =
                 cp-loan-term - 1
           end-if

           if (cp-original-term = zeros)
              and (cp-loan-term = zeros)
              move 20                  to ws-error-sub
              move '-Lf term'          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

      *    if ws-rate-in-lf-ben-code (1:1) = 'N' or 'T'
      *       compute cp-original-benefit =
      *          cp-original-benefit - ws-rate-lf-prem - ws-rate-ah-prem
      *    end-if

           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move ws-lf-special-calc-cd  to cp-special-calc-cd
           move ws-lf-earning-calc     to cp-earning-method
                                          cp-rating-method
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pmt-dt      to cp-first-pay-date
           move ws-rate-lf-prem        to cp-original-premium
           if d > 30
              compute cp-term-or-ext-days = d - 30
           else
              move zeros               to cp-term-or-ext-days
           end-if

           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           evaluate true
              when cp-return-code = '0' or '8' or '9' or 'A' or
                 'B' or 'C'
                 move '0'              to cp-return-code
                 if ((cp-calc-premium - ws-rate-lf-prem) > 9.99)
                              or
                    ((ws-rate-lf-prem - cp-calc-premium) > 9.99)
                    move 18            to ws-error-sub
                    move '-Lf Prem'    to ws-error-sup
      *    move cp-calc-premium     to ws-rate-lf-prem
      *    move cp-premium-rate     to ws-rate-lf-rate
      *    perform 0040-format-buffer  thru 0040-exit
                    
                    perform 0180-error-handle
                                       thru 0180-exit
                    go to 0300-return-cics
                 end-if
              when cp-return-code = '7'
                 move '0'              to cp-return-code
                 move 14               to ws-error-sub
                 move '-Lf Rates'      to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = '6'
                 move '0'              to cp-return-code
                 move 15               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = 'D'
                 move '0'              to cp-return-code
                 move 17               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
           end-evaluate

           move cp-calc-premium     to ws-rate-new-lf-prem
           move cp-premium-rate     to ws-rate-lf-rate

           .
       0030-exit.
           exit.

       0035-get-ah-rate.

           if ws-rate-in-ah-ben-code (1:1) = ' '
              go to 0035-exit
           end-if

           perform 0056-get-di-ben-code thru 0056-exit

      *    move spaces                 to calculation-pass-area
           move zeros                  to cp-r-max-mon-ben
                                          cp-r-max-tot-ben
                                          cp-rate-dev-pct
                                          cp-original-premium
                                          cp-critical-months
                                          cp-term-or-ext-days
                                          cp-original-benefit
                                          cp-rating-benefit-amt

           move am-cal-table           to cp-class-CODE

           move am-ah-deviation        to cp-deviation-code

           move ws-rate-state          to cp-state
                                          cp-state-std-abbrv
           move ws-state-chg-ext-days  to cp-ext-days-calc
           move 'A'                    to cp-benefit-type
           move ws-rate-ah-benefit-cd  to cp-benefit-cd

           if ws-rate-ah-term > zeros
              compute cp-original-benefit =
                 ws-rate-benefit / ws-rate-ah-term
              move cp-original-benefit to cp-rating-benefit-amt
           end-if
      *    move ws-rate-benefit        to cp-original-benefit
      *                                   cp-rating-benefit-amt
           move ws-rate-payment        to cp-original-benefit
                                          cp-rating-benefit-amt
           move ws-rate-age            to cp-issue-age
           move ws-comp-id             to cp-company-id
           move ws-comp-cd             to cp-company-cd

           if ws-rate-loan-term = zeros
              move ws-rate-ah-term     to ws-rate-loan-term
           end-if
           move ws-rate-ah-term        to cp-original-term
           move ws-rate-loan-term      to cp-loan-term

           if ws-rate-loan-type = '2'   *>  Lease
              compute cp-original-term = 
                 cp-original-term - 1
              compute cp-loan-term =
                 cp-loan-term - 1
           end-if

           if (cp-original-term = zeros)
              and (cp-loan-term = zeros)
              move 21                  to ws-error-sub
              move '-DI term'          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move 'L'                    to cp-life-override-code
           move 'A'                    to cp-ah-override-code
           move '3'                    to cp-process-type
           move ws-ah-earnings-calc    to cp-earning-method
                                          cp-rating-method
           move ws-ah-special-calc-cd  to cp-special-calc-cd
           move 'R'                    to cp-rate-file
           move ws-bin-eff-dt          to cp-cert-eff-dt
           move ws-bin-1st-pmt-dt      to cp-first-pay-date
           
           if d > 30
              compute cp-term-or-ext-days = d - 30
           else
              move zeros               to cp-term-or-ext-days
           end-if
           PERFORM 7000-GET-RATE       THRU 7000-EXIT

           evaluate true
              when cp-return-code = '0' or '8' or '9' or 'A' or
                 'B' or 'C'
                 move '0'              to cp-return-code
                 if ((cp-calc-premium - ws-rate-ah-prem) > 9.99)
                                 or
                    ((ws-rate-ah-prem - cp-calc-premium) > 9.99)
                    move 18            to ws-error-sub
                    move '-Ah Prem'    to ws-error-sup
                    perform 0180-error-handle
                                       thru 0180-exit
                    go to 0300-return-cics
                 end-if
              when cp-return-code = '7'
                 move '0'              to cp-return-code
                 move 14               to ws-error-sub
                 move '-Ah Rates'      to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = '6'
                 move '0'              to cp-return-code
                 move 16               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              when cp-return-code = 'D'
                 move '0'              to cp-return-code
                 move 17               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
           end-evaluate

      *    if (no-cp-error)
      *       or (cp-return-code = '8' or '9' or 'A' or 'B' or 'C')
      *       move '0'                 to cp-return-code
      *       if ((cp-calc-premium - ws-rate-ah-prem) > 9.99)
      *                       or
      *          ((ws-rate-ah-prem - cp-calc-premium) > 9.99)
      *           move 18                  to ws-error-sub
      *           move '-Ah Prem'          to ws-error-sup
      *           perform 0180-error-handle
      *                                thru 0180-exit
      *           go to 0300-return-cics
      *          move 'Z'              to cp-return-code
      *       end-if
      *    end-if

           move zeros                  to ws-max-ah-benefit
           move cp-calc-premium        to ws-rate-new-ah-prem
           move cp-premium-rate        to ws-rate-ah-rate

           .
       0035-exit.
           exit.

       0040-format-buffer.
      
           move spaces                 to ws-return-string
      
           perform 0045-format-error   thru 0045-exit
           move ';'                    to ws-sc1
                                          ws-sc2
                                          ws-sc3
                                          ws-sc4
                                          ws-sc5
                                          ws-sc6
                                          ws-sc7
                                          ws-sc8
                                          ws-sc9
                                          ws-sc10
                                          ws-sc11
                                          ws-sc12
                                          ws-sc13
                                          ws-sc14
                                          ws-sc15
                                          ws-sc16
                                          ws-sc17
                                          ws-sc18
                                          ws-sc19
                                          ws-sc20
                                          ws-sc21
                                          ws-sc22
                                          ws-sc23
                                          ws-sc24

           if no-cp-error
              if ws-contract-suffix = spaces or low-values
                 move ws-contract-no   to ws-return-contract-no (2:10)
              else
                 move ws-contract-no   to ws-return-contract-no (1:10)
                 move ws-contract-suffix
                                       to ws-return-contract-no (11:1)
              end-if
      *       move ws-rate-new-lf-prem to ws-return-lf-prem
      *       move ws-rate-new-ah-prem to ws-return-ah-prem

              move ws-rate-lf-prem     to ws-return-lf-prem
              move ws-rate-ah-prem     to ws-return-ah-prem

              move ws-rate-lf-term     to ws-return-lf-term
              move ws-rate-ah-term     to ws-return-ah-term

              move ws-rate-lf-rate     to ws-return-lf-rate
              move ws-rate-ah-rate     to ws-return-ah-rate

              move ws-rate-in-lf-ben-code
                                       to ws-return-lf-bencd
              move ws-rate-crit-per    to ws-return-max-bens

      *       move ws-max-ah-benefit   to ws-return-ah-max-amt
      *       move ws-max-lf-benefit   to ws-return-lf-max-ben
              move ws-lf-exp-date      to ws-return-lf-exp-dt
              move ws-ah-exp-date      to ws-return-ah-exp-dt
              move ws-loan-exp-date    to ws-return-loan-exp-dt
              move ws-rate-lf-benefit-cd
                                       to ws-return-lf-benefit-cd
              move ws-rate-ah-benefit-cd
                                       to ws-return-ah-benefit-cd
              move ws-rate-payment     to ws-return-period-pmt
                                          ws-return-ah-max-amt
      *       move ws-rate-tot-financed to ws-return-tot-financed
              move ws-rate-tot-pmts    to ws-return-tot-pmts
pema          move ws-rate-benefit     to ws-return-lf-max-ben
              move ws-limit-name       to ws-return-limit-name
              move am-cal-table        to ws-return-rate-class
              perform 0200-calc-stuff  thru 0200-exit
           else
              set error-in-one-coverage to true
              move spaces              to ws-contract-no
                                          ws-contract-suffix
           end-if

           .
       0040-exit.
           exit.
      
       0045-format-error.
      
           evaluate true
              when cp-return-code = '0'
                 move +1               to s1
              when cp-return-code = '1'
                 move +2               to s1
              when cp-return-code = '2'
                 move +3               to s1
              when cp-return-code = 'A'
                 move +4               to s1
              when cp-return-code = 'B'
                 move +5               to s1
              when cp-return-code = '4'
                 move +6               to s1
              when cp-return-code = '9'
                 move +7               to s1
              when cp-return-code = '8'
                 move +8               to s1
              when cp-return-code = 'H'
                 move +9               to s1
              when cp-return-code = 'C'
                 move +10              to s1
              when cp-return-code = '7'
                 move +11              to s1
              when cp-return-code = '6'
                 move +12              to s1
              when cp-return-code = 'D'
                 move +13              to s1
              when cp-return-code = 'Z'
                 move +14              to s1
                 move cp-calc-premium  to ws-return-ah-prem
                 move cp-premium-rate  to ws-return-ah-rate
                 move ws-max-ah-benefit
                                       to ws-return-ah-max-amt
                 move ws-ah-exp-date   to ws-return-ah-exp-dt
                 move ws-rate-ah-benefit-cd
                                       to ws-return-ah-benefit-cd
              when cp-return-code = 'Y'
                 move +15              to s1
                 move cp-calc-premium  to ws-return-ah-prem
                 move cp-premium-rate  to ws-return-ah-rate
                 move ws-max-ah-benefit
                                       to ws-return-ah-max-amt
                 move ws-ah-exp-date   to ws-return-ah-exp-dt
                 move ws-rate-ah-benefit-cd
                                       to ws-return-ah-benefit-cd
           end-evaluate
      
           move ws-table-error-no (s1) to ws-return-error-no
           move ws-table-error-mess (s1)
                                       to ws-return-error-mess
      
           .
       0045-exit.
           exit.
           
       0050-get-account.
      
           move ws-comp-cd             to ws-am-company-cd
           move '9'                    to ws-am-carrier
           move '000000'               to ws-am-group
           move ws-rate-state          to ws-am-state
           move ws-rate-acct-no        to ws-am-account
           move ws-bin-eff-dt          to ws-am-exp-dt
      
           exec cics read
              dataset      ('ERACCT')
              ridfld       (ws-am-key)
              into         (account-master)
              GTEQ
              resp         (ws-response)
           end-exec
      
           if resp-normal
              and (ws-comp-cd = am-company-cd)
              and (ws-rate-state = am-state)
              and (ws-rate-acct-no = am-account)
              and (ws-bin-eff-dt >= am-effective-dt)
              and (ws-bin-eff-dt < am-expiration-dt)
              continue
           else
              move '0114;No account mstr found ' to ws-return-string
              go to 0300-RETURN-CICS
           end-if
      
           move am-comment-line (1)    to ws-form-limit-name
           move spaces                 to ws-limit-name
           perform varying l1 from +50 by -1 until
              (l1 < +1)
              or (ws-form-limit-name (l1:1) <> ' ')
           end-perform

           if l1 > +3
              perform varying l1 from l1 by -1 until
                 (l1 < +1)
                 or (ws-form-limit-name (l1:1) = ' ')
              end-perform
              if l1 > +3
                 subtract +1 from l1
                 move ws-form-limit-name (1:l1)
                                       to ws-limit-name
              end-if
           end-if
      
           .
       0050-exit.
           exit.

       0054-get-lf-ben-code.
      
           move ws-comp-id             to cntl-comp-id
           move '4'                    to cntl-rec-type
           move spaces                 to cntl-access
           move ws-rate-lf-benefit-cd  to cntl-access(3:2)
           move +0                     to cntl-seq-no
      
           exec cics read
              dataset      ('ELCNTL')
              ridfld       (ELCNTL-KEY)
              into         (control-file)
              GTEQ
              resp         (ws-response)
           end-exec

           if (resp-normal)
              and (cntl-rec-type = '4')
              perform varying b1 from +1 by +1 until
                 (b1 > 8)
                 or (cf-benefit-code(b1) = ws-rate-lf-benefit-cd)
              end-perform
              if b1 < 9
                 move cf-lf-coverage-type(b1)
                                       to ws-lf-coverage-type
                 move cf-special-calc-cd(b1)
                                       to ws-lf-special-calc-cd
                 move cf-joint-indicator(b1)
                                       to ws-lf-joint-indicator
                 move cf-co-earnings-calc(b1)
                                       to ws-lf-earning-calc
              end-if
           end-if

           .
       0054-exit.
           exit.

       0056-get-di-ben-code.
      
           move ws-comp-id             to cntl-comp-id
           move '5'                    to cntl-rec-type
           move spaces                 to cntl-access
           move ws-rate-ah-benefit-cd  to cntl-access(3:2)
           move +0                     to cntl-seq-no
      
           exec cics read
              dataset      ('ELCNTL')
              ridfld       (ELCNTL-KEY)
              into         (control-file)
              GTEQ
              resp         (ws-response)
           end-exec

           if (resp-normal)
              and (cntl-rec-type = '5')
              perform varying b1 from +1 by +1 until
                 (b1 > 8)
                 or (cf-benefit-code(b1) = ws-rate-ah-benefit-cd)
              end-perform
              if b1 < 9
                 move cf-special-calc-cd(b1)
                                       to ws-ah-special-calc-cd
                 move cf-joint-indicator(b1)
                                       to ws-ah-joint-indicator
                 move cf-co-earnings-calc(b1)
                                       to ws-ah-earnings-calc
              end-if
           end-if

           .
       0056-exit.
           exit.

       0058-get-state-controls.
      
           move ws-comp-id             to cntl-comp-id
           move '3'                    to cntl-rec-type
           move spaces                 to cntl-access
           move ws-rate-state          to cntl-access
           move +0                     to cntl-seq-no
      
           exec cics read
              dataset      ('ELCNTL')
              ridfld       (ELCNTL-KEY)
              into         (control-file)
              GTEQ
              resp         (ws-response)
           end-exec

           if (resp-normal)
              and (cntl-rec-type = '3')
              move cf-st-fst-pmt-days-chg
                                       to ws-state-chg-ext-days
           end-if

           .
       0058-exit.
           exit.

       0060-check-for-dup.
      
      **==============================================================**
      **                                                              **
      **    All i'm going to do here is check for a dup using         **
      **  the state, account, eff dt and last six of vin and a space  **
      **  in the cert suffix. If I do find one then I will have to    **
      **  find the last suffix and use the next available one in the  **
      **  suffix table.                                               **
      **                                                              **
      **==============================================================**
      
           move +1                     to x1
           move ' '                    to ws-browse-sw
      
           move ws-comp-cd             to ws-cm-key
           move '9'                    to ws-cm-carrier
           move '000000'               to ws-cm-group
           move ws-rate-state          to ws-cm-state
           move ws-rate-acct-no        to ws-cm-account
           move ws-bin-eff-dt          to ws-cm-eff-dt
           string
              '0000'
              ws-rate-vin (12:6)
              ' ' delimited by size into ws-cm-cert-no
           end-string
           move ws-cm-key              to ws-cm-compare-key
           move ws-cm-cert-ten         to ws-contract-no
           move low-values             to ws-last-suffix

           exec cics startbr
              dataset      ('ELCERT')
              ridfld       (ws-cm-key)
              gteq 
              resp         (ws-response)
           end-exec
      
           evaluate true
              when resp-normal
                 set browse-started to true
              when resp-notfnd or resp-endfile
                 set i-say-stop to true
              when other
                 move ws-response to ws-disp-resp
                 string
                    '9999' ';'
                    'Bad elcert startbr ' ';'
                    ws-disp-resp delimited by size
                                       into ws-return-string
                 end-string
                 display ' something went wrong with start br '
                    ws-response
                 go to 0300-RETURN-CICS
           end-evaluate
      
           perform until i-say-stop
              exec cics readnext
                 dataset      ('ELCERT')
                 ridfld       (ws-cm-key)
                 into         (certificate-master)
                 resp         (ws-response)
              end-exec
              if resp-normal
                 if ws-cm-key (1:32) = ws-cm-compare-key (1:32)
                    move ws-cm-cert-suffix
                                       to ws-last-suffix
                 else
                    set i-say-stop to true
                 end-if
              else
                 set i-say-stop to true
              end-if
           end-perform
      
           if browse-started
              exec cics endbr
                 dataset   ('ELCERT')
              END-EXEC
           end-if
      
           if ws-last-suffix not = low-values
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 display ' more than 26 suffix codes ' ws-last-suffix
                 move 19               to ws-error-sub
                 move spaces           to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if
      
           .
       0060-exit.
           exit.
      
       0070-open-cursor.
      
      *    display ' declare cursor ' ws-begin-dt ' ' ws-end-dt
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***  The dates on the sql table have values in the time        ***
      ***  so I convert it to a string and just use mm/dd/yyyy       ***
      ***  to perform the comparison.                                ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
      
           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if
      
           move ws-rate-state          to ws-dealer-state
           move ws-rate-acct-no        to ws-dealer-id
           move ws-rate-eff-date       to ws-contract-eff-dt
           move zeros                  to ws-ks-contract-no
           move ws-rate-vin (12:6)     to ws-ks-contract-no (5:6)
           
           EXEC SQL
              DECLARE
                 contracts cursor for
              SELECT
                 DlrState,
                 DlrId,
                 EffDt,
                 ContractNo,
                 ContractSuffix
              FROM
                 PendingContracts
              WHERE
                 DlrState     = :ws-dealer-state
                 and DlrId    = :ws-dealer-id
                 and EffDt    = :ws-contract-eff-dt
                 and ContractNo  = :ws-ks-contract-no
              ORDER BY
                 dlrstate,
                 dlrid,
                 effdt,
                 contractno,
                 contractsuffix
           end-exec
      
           if sqlcode not = 0
              display "Error: cannot declare cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if
      
           EXEC SQL
              open contracts
           END-EXEC
      
           if sqlcode not = 0
              display "Error: cannot open cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              go to 0070-exit
           end-if
      
           .
       0070-exit.
           exit.
       0080-process-input.
      
           perform until sqlcode not = 0
              EXEC SQL
                 fetch contracts into
                    :sql-dlr-state,
                    :sql-dlr-id,
                    :sql-eff-dt,
                    :sql-contr-no,
                    :sql-contr-suffix
              END-EXEC
      
              if sqlcode = 0
                 move sql-contr-suffix to ws-tbl-last-suffix
              else
                 if sqlcode not = 0 and 100
                    display "Error: cannot fetch row " 
                    display ' sql return code ' sqlcode
                    display ' sql err mess    ' sqlerrmc
                 end-if
              end-if
           end-perform

           if ws-tbl-last-suffix not = low-values
              and (ws-tbl-last-suffix > ws-last-suffix)
              perform varying x1 from +1 by +1 until
                 (x1 > +26)
                 or (ws-tbl-last-suffix = ws-suffix-value (x1))
              end-perform
              if x1 < +27
                 move ws-suffix-value (x1 + 1)
                                       to ws-contract-suffix
              else
                 display ' more than 26 suffix codes ' ws-last-suffix
                 move 19               to ws-error-sub
                 move spaces           to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if
      
           EXEC SQL
               close contracts
           END-EXEC
      
           if sqlcode not = 0
              display "Error: cannot close cursor "
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
           end-if
      
           .
       0080-exit.
           exit.
      
       0090-get-limits.

122718*** Per Kim, add no disab only edit and error to WS
122718     if ((ws-rate-lf-benefit-cd = '00' or '  ')
                             or
               (no-life-coverage))
122718        and (ws-rate-ah-benefit-cd <> '00' and '  ')
122718        if am-ah-only-indicator = 'N'
122718           move 33               to ws-error-sub
122718           move ' '              to ws-error-sup
122718           perform 0180-error-handle
122718                                 thru 0180-exit
122718           go to 0300-RETURN-CICS
122718        end-if
122718     end-if

           move zeros                  to ws-lf-limit-lo-age      
                                          ws-lf-limit-hi-age      
                                          ws-lf-limit-att-age     
                                          ws-lf-limit-max-term    
                                          ws-lf-limit-max-benefit
                                          ws-lf-limit-partial-cov
                                          ws-lf-limit-check-elig
                                          ws-lf-limit-elig-max-term
                                          ws-di-limit-lo-age      
                                          ws-di-limit-hi-age      
                                          ws-di-limit-att-age     
                                          ws-di-limit-max-term    
                                          ws-di-limit-max-jnt-term
                                          ws-di-limit-max-mo-ben  
                                          ws-di-limit-max-tot-ben 
                                          ws-di-limit-partial-cov
                                          ws-di-limit-check-elig
                                          ws-di-limit-elig-max-term

           if ws-rate-in-lf-ben-code = spaces
              move zeros to ws-rate-lf-term
           end-if
           
           if (ws-rate-lf-term = zeros)
              and (ws-rate-ah-term <> zeros)
              move 'N '                to ws-rate-in-lf-ben-code
           end-if


           perform 0092-get-lf-limits  thru 0092-exit
           perform 0097-get-di-limits  thru 0097-exit
      
           .
       0090-exit.
           exit.
      
      
       0092-get-lf-limits.
      
           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if
      
           evaluate true
              when raw-lf-ben-code (1:1) = 'L' 
                 move 'LL'             to ws-limit-cov-type
              when raw-ah-ben-code (1:1) = ' '
                 move 'LO'             to ws-limit-cov-type
              when raw-lf-ben-code (1:1) = 'N' or 'G' or 'T'
                 move 'RL'             to ws-limit-cov-type
              when other   *> Assuming no life coverage
                 go to 0092-exit
           end-evaluate
      
           move zeros                  to ws-limit-lo-age
                                          ws-limit-hi-age
                                          ws-limit-att-age
                                          ws-limit-max-term
                                          ws-limit-max-jnt-term
                                          ws-limit-max-mo-ben
                                          ws-limit-max-tot-ben
                                          ws-limit-partial-cov
                                          ws-limit-check-elig
                                          ws-limit-elig-max-term

           move ws-rate-age            to ws-limit-issue-age
      
           .
       0092-get-life.

           move 30 to ws-limit-issue-age

           EXEC SQL
              SELECT
                 AttainedAge
              INTO
                 :ws-limit-att-age
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec
      
           if sqlcode not = 0
              display "Error: cannot find limitsa " ws-limit-name
                 ' ' ws-limit-cov-type ' ' ws-limit-issue-age
              display ' sql return code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              if (ws-limit-cov-type = 'LO')
                 and (raw-ah-ben-code (1:1) = ' ')
                 move 'RL'             to ws-limit-cov-type
                 go to 0092-get-life
              else
                 display "Error: cannot find limits " ws-limit-name
                    ' ' ws-limit-cov-type ' ' ws-limit-issue-age
                 display ' sql retrun code ' sqlcode
                 display ' sql err mess    ' sqlerrmc
                 move 22                  to ws-error-sub
                 move ws-limit-name       to ws-error-sup
                 perform 0180-error-handle
                                          thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           move ws-rate-age            to ws-limit-issue-age

           EXEC SQL
              SELECT
                 AttainedAge,
                 MaxTerm,
                 MaxTotBen,
                 PartialCoverage,
                 CheckEligibility,
                 EligibilityMaxTerm
              INTO
                 :ws-limit-att-age,
                 :ws-limit-max-term,
                 :ws-limit-max-tot-ben,
                 :ws-limit-partial-cov  :nu-partial-cov,
                 :ws-limit-check-elig   :nu-check-elig,
                 :ws-limit-elig-max-term
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec
      
           if sqlcode not = 0
              display "Error: cannot find limits for age "
                 ws-limit-name ' ' ws-limit-cov-type ' '
                 ws-limit-issue-age
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 4                   to ws-error-sub
              move ws-limit-name       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if
      
      *    display ' good get on limit *'  ws-limit-name '*'
      *       ws-limit-att-age '*' ws-limit-cov-type '*'
      *         ws-limit-issue-age '*' ws-att-age '*'
      *         ws-limit-partial-cov '*' ws-limit-check-elig '*'
      *         ws-limit-elig-max-term '*'
      
           move ws-limit-lo-age        to ws-lf-limit-lo-age
           move ws-limit-hi-age        to ws-lf-limit-hi-age
           move ws-limit-att-age       to ws-lf-limit-att-age
           move ws-limit-max-term      to ws-lf-limit-max-term
           move ws-limit-max-tot-ben   to ws-lf-limit-max-benefit
           move ws-limit-partial-cov   to ws-lf-limit-partial-cov
           move ws-limit-check-elig    to ws-lf-limit-check-elig
           move ws-limit-elig-max-term to ws-lf-limit-elig-max-term

           if ws-lf-limit-check-elig = 0
              go to 0092-check-partial
           end-if

           if ws-rate-loan-term <= ws-lf-limit-elig-max-term
              continue
           else
              move 31                  to ws-error-sub
              move ' '                 to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-rate-benefit <= ws-lf-limit-max-benefit
              continue
           else
              move 32                  to ws-error-sub
              move '- Lf Benefit '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           .
       0092-check-partial.

           if ws-lf-limit-partial-cov = 0
              go to 0092-continue
           end-if

           if ws-rate-benefit > ws-lf-limit-max-benefit
              move ws-lf-limit-max-benefit
                                       to ws-rate-benefit
           end-if

           .
       0092-continue.

           if ws-rate-benefit <= ws-lf-limit-max-benefit
              continue
           else
              move 10                  to ws-error-sub
              move '- Lf Benefit '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-rate-lf-term <= ws-lf-limit-max-term
              continue
           else
              move 9                   to ws-error-sub
              move '- Lf Term '        to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-att-age <= ws-lf-limit-att-age
              continue
           else
              move 5                   to ws-error-sub
              move ws-att-age          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if
      
           .
       0092-exit.
           exit.

       0097-get-di-limits.
      
           if raw-ah-ben-code (1:1) = 'A'
              move 'DI'                to ws-limit-cov-type
           else   *>  Assuming no AH coverage
              go to 0097-exit
           end-if
      
           if not connected-to-db
              perform 6000-connect-to-db thru 6000-exit
           end-if

      ***  True = -1
      
           move 30 to ws-limit-issue-age
           EXEC SQL
              SELECT distinct top 1
                 AttainedAge
              INTO
                 :ws-limit-att-age
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec
      
           if sqlcode not = 0
              display "Error: cannot find limits " ws-limit-name
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 22                  to ws-error-sub
              move ws-limit-name       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           move ws-rate-age            to ws-limit-issue-age

           EXEC SQL
              SELECT distinct top 1
                 AttainedAge,
                 MaxTerm,
                 MaxJntTerm,
                 MaxMoBen,
                 MaxTotBen,
                 PartialCoverage,
                 CheckEligibility,
                 EligibilityMaxTerm
              INTO
                 :ws-limit-att-age,
                 :ws-limit-max-term,
                 :ws-limit-max-jnt-term,
                 :ws-limit-max-mo-ben,
                 :ws-limit-max-tot-ben,
                 :ws-limit-partial-cov   :nu-partial-cov,
                 :ws-limit-check-elig    :nu-check-elig,
                 :ws-limit-elig-max-term
              FROM
                 FormLimits
              WHERE
                 Limit           = :ws-limit-name
                 and CovType     = :ws-limit-cov-type
                 and LoIssueAge <= :ws-limit-issue-age
                 and HiIssueAge >= :ws-limit-issue-age
           end-exec
      
           if sqlcode not = 0
              display "Error: cannot find limits " ws-limit-name
              display ' sql retrun code ' sqlcode
              display ' sql err mess    ' sqlerrmc
              move 4                   to ws-error-sub
              move ws-limit-name       to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

      *    display ' good get on ah limit *'  ws-limit-name '*'
      *       ws-limit-att-age '*' ws-limit-cov-type '*'
      *         ws-limit-issue-age '*' ws-att-age '*'
      *         ws-limit-partial-cov '*' ws-limit-check-elig '*'
      *         ws-limit-elig-max-term '*'

           move ws-limit-lo-age        to ws-di-limit-lo-age
           move ws-limit-hi-age        to ws-di-limit-hi-age
           move ws-limit-att-age       to ws-di-limit-att-age
           move ws-limit-max-term      to ws-di-limit-max-term
           move ws-limit-max-jnt-term  to ws-di-limit-max-jnt-term
           move ws-limit-max-mo-ben    to ws-di-limit-max-mo-ben
           move ws-limit-max-tot-ben   to ws-di-limit-max-tot-ben
           move ws-limit-partial-cov   to ws-di-limit-partial-cov
           move ws-limit-check-elig    to ws-di-limit-check-elig
           move ws-limit-elig-max-term to ws-di-limit-elig-max-term

           if ws-di-limit-check-elig = 0
              go to 0097-check-partial
           end-if

           if ws-rate-loan-term <= ws-di-limit-elig-max-term
              continue
           else
              move 31                  to ws-error-sub
              move ' '                 to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if (ws-rate-ah-term * ws-rate-payment) <=
                         ws-di-limit-max-tot-ben
              continue
           else
              move 32                  to ws-error-sub
              move '- Di ToT Ben '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-rate-state = 'FL'
              if ((ws-rate-loan-term * ws-rate-payment) <=
                         ws-di-limit-max-tot-ben)
                 continue
              else
                 move 32               to ws-error-sub
                 move '- Di ToT Ben '  to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if

           .
       0097-check-partial.

           if ws-di-limit-partial-cov = 0
              go to 0097-continue
           end-if

           if ws-rate-payment > ws-di-limit-max-mo-ben
              move ws-di-limit-max-mo-ben
                                       to ws-rate-payment
           end-if

           if (ws-rate-ah-term * ws-rate-payment) >
                         ws-di-limit-max-tot-ben
              compute ws-rate-payment =
                 ws-di-limit-max-tot-ben / ws-rate-ah-term
           end-if

           .
       0097-continue.

           if ws-rate-payment <= ws-di-limit-max-mo-ben
              continue
           else
              move 11                  to ws-error-sub
              move '- Ah Benefit '     to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if
       
           if ws-rate-sin-jnt-ah = 'J'
              if ws-rate-ah-term <= ws-di-limit-max-jnt-term
                 continue
              else
                 move 8                   to ws-error-sub
                 move '- Ah TermJNT'      to ws-error-sup
                 perform 0180-error-handle
                                          thru 0180-exit
                 go to 0300-return-cics
              end-if
           else
              if ws-rate-ah-term <= ws-di-limit-max-term
                 continue
              else
                 move 9                   to ws-error-sub
                 move '- Ah Term '        to ws-error-sup
                 perform 0180-error-handle
                                          thru 0180-exit
                 go to 0300-return-cics
              end-if
           end-if
       
           if (ws-rate-ah-term * ws-rate-payment) <=
                         ws-di-limit-max-tot-ben
              continue
           else
              move 13                  to ws-error-sub
              move spaces              to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if

           if ws-att-age <= ws-di-limit-att-age
              continue
           else
              move 5                   to ws-error-sub
              move ws-att-age          to ws-error-sup
              perform 0180-error-handle
                                       thru 0180-exit
              go to 0300-return-cics
           end-if
      
           .
       0097-exit.
           exit.
      
       0110-unstring.
      
      ***____________________________________________________________***
      **|                                                            |**
      **|    Unstring the raw data into data elements                |**
      **|                                                            |**
      ***____________________________________________________________***
      
              unstring dfhcommarea
                 delimited by '|' into
                    raw-message-num
                    raw-state
                    raw-acct-no
                    raw-vin
                    raw-lf-ben-code
                    raw-ah-ben-code
                    raw-earn-meth
                    raw-pri-birth-date
                    raw-cob-birth-date
                    raw-loan-amt
                    raw-eff-date
                    raw-1st-pmt-dt
                    raw-pmts-per-year
                    raw-lf-premium
                    raw-ah-premium
                    raw-loan-term
                    raw-lf-term
                    raw-ah-term
                    raw-apr
                    raw-lf-sin-jnt-ind
                    raw-ah-sin-jnt-ind
                    raw-lf-dismemberment
                    raw-retro-elim
                    raw-waiting-days
                    raw-crit-per
                    raw-total-payments
                    raw-period-payment
              end-unstring

           .
       0110-exit.
           exit.
      
       0120-format-message.
      
      ***____________________________________________________________***
      **|                                                            |**
      **|    Format each data element to be consistant with          |**
      **|  COBOL rate copybook model                                 |**
      **|                                                            |**
      ***____________________________________________________________***
      
           inspect
              raw-acct-no replacing leading spaces by zeros
           inspect
              raw-pmts-per-year replacing leading spaces by zeros
           inspect
              raw-loan-term replacing leading spaces by zeros
           inspect
              raw-lf-term replacing leading spaces by zeros
           inspect
              raw-ah-term replacing leading spaces by zeros
           inspect
              raw-waiting-days replacing leading spaces by zeros
           inspect
              raw-crit-per replacing leading spaces by zeros
      
           inspect
              raw-loan-amt replacing all spaces by zeros
           inspect
              raw-lf-premium replacing all spaces by zeros
           inspect
              raw-ah-premium replacing all spaces by zeros
           inspect
              raw-lf-term replacing leading spaces by zeros
           inspect
              raw-ah-term replacing leading spaces by zeros
           inspect
              raw-apr replacing all spaces by zeros
           inspect
              raw-total-payments replacing all spaces by zeros
           inspect
              raw-period-payment replacing all spaces by zeros
      
           move raw-loan-amt           to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-benefit
      *    display ' ben  in *' ws-work-in '*'
      *    display ' ben  out *' ws-work-out '*'
      
           move raw-total-payments     to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-tot-pmts
      *    display ' tot pmts  in *' ws-work-in '*'
      *    display ' tot pmts  out *' ws-work-out '*'
      
           move raw-lf-premium         to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-lf-prem
      *    display ' prem in *' ws-work-in '*'
      *    display ' prem out *' ws-work-out '*'
      
           move raw-ah-premium         to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-ah-prem
      *    display ' prem in *' ws-work-in '*'
      *    display ' prem out *' ws-work-out '*'
      
           move raw-period-payment     to ws-work-in
           perform 0130-format-amt     thru 0130-exit
           move ws-work-out-v2         to ws-rate-payment
      *    display ' pmt  in *' ws-work-in '*'
      *    display ' pmt  out *' ws-work-out '*'
      
           move raw-loan-term          to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-loan-term
      *    display ' rate loan term *' ws-rate-loan-term '*'
      *    display ' ln term in *' ws-work-in '*'
      *    display ' ln term out *' ws-work-out '*'
      
           move raw-lf-term            to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-lf-term
      *    display ' rate ins  term *' ws-rate-lf-term '*'
      *    display ' ins term in *' ws-work-in '*'
      *    display ' ins term out *' ws-work-out '*'
      
           move raw-ah-term            to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-ah-term
      *    display ' rate ins  term *' ws-rate-ah-term '*'
      *    display ' ins term in *' ws-work-in '*'
      *    display ' ins term out *' ws-work-out '*'
      
           move raw-waiting-days       to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-waiting-days
      *    display ' wait day in *' ws-work-in '*'
      *    display ' wait day out *' ws-work-out '*'
      
           move raw-crit-per           to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-crit-per
      *    display ' crit per in *' ws-work-in '*'
      *    display ' crit per out *' ws-work-out '*'
      
           move raw-pmts-per-year      to ws-work-in
           perform 0140-format-term    thru 0140-exit
           move ws-work-out-v0         to ws-rate-pmts-per-year
      *    display ' pmts per yr *' ws-work-in '*'
      *    display ' pmts per yr  *' ws-work-out '*'

           move raw-apr                to ws-work-in
           perform 0150-format-apr     thru 0150-exit
           move ws-work-out-v5         to ws-rate-apr
      *    display ' ins apr  in *' ws-work-in '*'
      *    display ' ins apr  out *' ws-work-out '*'
      
           move raw-state              to ws-rate-state
           move raw-acct-no            to ws-rate-acct-no
           move raw-vin                to ws-rate-vin
           move raw-lf-ben-code        to ws-rate-in-lf-ben-code
           move raw-ah-ben-code        to ws-rate-in-ah-ben-code
           move raw-earn-meth          to ws-rate-loan-type

           evaluate ws-rate-in-lf-ben-code(1:1)
              when 'N'
                 move '5'              to ws-rate-earn-meth
              when 'T'
                 move '5'              to ws-rate-earn-meth
              when other
                 move '1'              to ws-rate-earn-meth
           end-evaluate
           move raw-pri-birth-date     to ws-rate-pri-birth-date
           move raw-cob-birth-date     to ws-rate-cob-birth-date
           move raw-eff-date           to ws-rate-eff-date
           move raw-1st-pmt-dt         to ws-rate-1st-pmt-dt
           move raw-lf-sin-jnt-ind     to ws-rate-sin-jnt-lf
           move raw-ah-sin-jnt-ind     to ws-rate-sin-jnt-ah
           move raw-lf-dismemberment   to ws-rate-dismemberment
           move raw-retro-elim         to ws-rate-retro-elim
      
           .
       0120-exit.
           exit.
      
       0130-format-amt.
      
           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) = '.')
           end-perform
           if s2 < +9
              move ws-work-in (s2 + 1:2)
                                    to ws-work-out (9:2)
              move +8               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 0)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if
      
           .
       0130-exit.
           exit.
      
       0140-format-term.
      
           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) numeric)
           end-perform
           if s2 > +0
              move ws-work-in (s2:1)
                                    to ws-work-out (10:1)
              move +9               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 7)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if
      
           .
       0140-exit.
           exit.
      
       0150-format-apr.
      
           move zeros                  to ws-work-out
           perform varying s2 from +10 by -1 until
              (s2 < +1)
              or (ws-work-in (s2:1) = '.')
           end-perform
           if s2 < +5
              move ws-work-in (s2 + 1:5)
                                    to ws-work-out (6:5)
              move +5               to s3
              subtract +1 from s2
              perform varying s2 from s2 by -1 until
                 (s2 < +1)
                 or (s3 = 0)
                 move ws-work-in (s2:1) to ws-work-out (s3:1)
                 subtract +1 from s3
              end-perform
           end-if
      
           .
       0150-exit.
           exit.
      
       0180-error-handle.

           move ws-table-error-no (ws-error-sub)
                                       to ws-return-error-no
           move spaces                 to ws-return-error-mess
           string
              ws-table-error-mess (ws-error-sub)
              ws-error-sup
              delimited by '  ' into ws-return-error-mess
           end-string
      
           .
       0180-exit.
           exit.
      
       0200-calc-stuff.

      *    if ws-lf-limit-partial-cov = 0
      *       display ' no partial cov lf '
              
           move ws-rate-loan-term      to n
           
      *    if ws-rate-loan-type = '2'
      *       if (ws-rate-in-lf-ben-code (1:1) = 'G' or 'L')
      *          or (ws-rate-in-ah-ben-code (1:1) = 'A')
      *          compute n = n - 1
      *       end-if
      *    end-if

           move ws-rate-pmts-per-year  to ppy
           move 30                     to dpp
           compute i = ws-rate-apr / (ppy *100)
           if i = zeros
              move n                   to a-angle-n
              move 1                   to gamma
      *       move .0000001            to i
           else
              COMPUTE A-ANGLE-N ROUNDED =
                 (1 - ((1 / (1 + I)) ** N)) / I
              COMPUTE GAMMA ROUNDED =
                 (1 + ((D * I) / DPP)) / (1 + I)
           end-if

           COMPUTE A-PRM-ANGLE-N ROUNDED = A-ANGLE-N / GAMMA

           compute ws-ins-per-month rounded =
              (ws-rate-lf-prem + ws-rate-ah-prem) / a-prm-angle-n

           compute ws-prem-plus-int rounded =
              (ws-ins-per-month * n)

           compute ws-tot-pmts-wo-ins rounded =
              ws-rate-tot-pmts - ws-prem-plus-int

           if (ws-rate-in-lf-ben-code (1:1) = 'N' or 'T')
              and (ws-rate-loan-type <> '2')
              compute ws-principal rounded =
                 ws-rate-amt-financed - (ws-rate-lf-prem +
                    ws-rate-ah-prem)
      *          ws-rate-benefit - (ws-rate-lf-prem + ws-rate-ah-prem)
           else
              compute ws-principal rounded =
                 ws-tot-pmts-wo-ins / n * a-angle-n
           end-if

           compute ws-amt-financed rounded =
              ws-principal + (ws-rate-lf-prem + ws-rate-ah-prem)
      *    compute ws-rate-loan-pmt rounded =
      *       ws-principal / a-angle-n
           compute ws-rate-loan-pmt rounded =
              ws-amt-financed / a-angle-n

           move ws-amt-financed        to ws-return-tot-financed
           move ws-rate-loan-pmt       to ws-return-loan-pmt
           move ws-principal           to ws-return-principal

      **==============================================================**
      **                                                              **
      **  Validate benefit with amt financed for net pay              **
      **                                                              **
      **==============================================================**

           if (ws-rate-in-lf-ben-code (1:1) = 'N' or 'T')
              and (ws-rate-loan-type <> '2')
              if ((ws-amt-financed - ws-rate-amt-financed) > 9.99)
                             or
                 ((ws-rate-amt-financed - ws-amt-financed) > 9.99)
                 move spaces           to ws-return-string
                 move 30               to ws-error-sub
                 move ' '              to ws-error-sup
                 perform 0180-error-handle
                                       thru 0180-exit
                 go to 0300-RETURN-CICS
              end-if
           end-if

           .
       0200-exit.
           exit.

       0300-RETURN-CICS.
      
           perform 0400-disconnect     thru 0400-exit
           move ws-return-string       to dfhcommarea
           exec cics return end-exec
           goback
      
           .
       0300-exit.
           exit.
      
       0400-disconnect.
      
           if connected-to-db
              EXEC SQL
                  disconnect all
              END-EXEC
              move ' ' to ws-connect-sw
           end-if
      
           .
       0400-exit.
           exit.
      
       6000-CONNECT-TO-DB.
      
      ***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
      ***                                                            ***
      ***                                                            ***
      ***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
      
040622     move 'appuser'              to usr
040622     move 'appuser@cso'          to pass
040622     evaluate true
040622        when ws-kix-myenv = 'cid1p'
040622           MOVE 'SDVDB01_CrtManage'
040622                                 TO SVR
040622        when ws-kix-myenv = 'mdoff'
040622           MOVE 'HOVTSTDB01UAT_CrtManage'
040622                                 TO SVR
040622        when other
040622           MOVE 'HOVTSTDB01_CrtManage'
040622                                 TO SVR
040622     end-evaluate

           string
               usr delimited space
               "." delimited size
               pass delimited space into usr-pass
           end-string
      
           EXEC SQL
              CONNECT TO :svr USER :usr-pass
           END-EXEC
       
           if sqlcode not = 0
              display "Error: cannot connect to " svr
              display sqlcode
              display sqlerrmc
           else
              display ' Successful Connect ' sqlcode
              set connected-to-db to true
           end-if
      
           .
       6000-EXIT.
           EXIT.
      
       7000-GET-RATE.
       
           EXEC CICS LINK
              PROGRAM ('ELRATE')
              COMMAREA (CALCULATION-PASS-AREA)
              LENGTH   (CP-COMM-LENGTH)
           END-EXEC
           
           .
       7000-EXIT.
           EXIT.
      
       9700-DATE-LINK.
      
           EXEC CICS LINK
                PROGRAM  ('ELDATCV')
                COMMAREA (DATE-CONVERSION-DATA)
                LENGTH   (DC-COMM-LENGTH)
           END-EXEC.
      
       9700-EXIT.
            EXIT.
      
