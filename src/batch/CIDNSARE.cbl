       IDENTIFICATION DIVISION.
       PROGRAM-ID.                CIDNSARE.
      *AUTHOR.     AJRA.
      *REMARKS.
      * THIS PROGRAM GENERATES RESEND LETTERS FOR ACCT SERVICES
103012******************************************************************
103012*                   C H A N G E   L O G
103012*
103012* CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
103012*-----------------------------------------------------------------
103012*  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
103012* EFFECTIVE    NUMBER
103012*-----------------------------------------------------------------
103012* 103012  CR2012101700002  AJRA  STOP RESEND ON OPEN CLAIM
103012*                                CHANGE STOP DATE ON AUTO STOPS
100713* 100713  IR2013100700002  AJRA  DON'T ABEND IF NO Z RECORD
100913* 100913  CR2013090300001  AJRA  ADD AUTO BILLING NOTE
123113* 123113    2013090300001  AJRA  USE NEXT BUS DT FOR RESEND DT CALC
011314* 011314  IR2014011300001  AJRA  FIX NEXT BUS DT
062017* 062017  CR2015091000001  PEMA  Calc interest amt for TN refunds
061818* 061818  IR2018060700001  PEMA  Corrections to TN interest calc.
072619* 072619  CR2019072600001  PEMA  Comment out display statements.
041320* 041320  CR2020030500002  PEMA  Issue, cancel billing notes
101420* 101420  IR2020101300001  PEMA  Fix issue, cancel billing notes.
103012******************************************************************
       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.

           SELECT ARCH-IN          ASSIGN TO SYS010.

           SELECT ERARCH           ASSIGN TO ERARCH
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LA-CONTROL-PRIMARY
                                   FILE STATUS IS ERARCH-FILE-STATUS.

           SELECT ERARCT           ASSIGN TO ERARCT
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS LT-CONTROL-PRIMARY
                                   FILE STATUS IS ERARCT-FILE-STATUS.

           SELECT NSASEXTR         ASSIGN TO NSASEXTR
                                   ORGANIZATION IS INDEXED
                                   ACCESS IS DYNAMIC
                                   RECORD KEY IS NSAS-CONTROL-PRIMARY
                                   FILE STATUS IS NSAS-FILE-STATUS.

           SELECT ELCNTL           ASSIGN TO ELCNTL
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ELCNTL-FILE-STATUS
                                   RECORD KEY IS CF-CONTROL-PRIMARY.

           SELECT ELLETR           ASSIGN TO ELLETR
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ELLETR-FILE-STATUS
                                   RECORD KEY IS TX-CONTROL-PRIMARY.

           SELECT ELCERT           ASSIGN TO ELCERT
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ELCERT-FILE-STATUS
                                   RECORD KEY IS CM-CONTROL-PRIMARY.

           SELECT ERPNDB           ASSIGN TO ERPNDB2
                                   ACCESS IS DYNAMIC
                                   ORGANIZATION IS INDEXED
                                   FILE STATUS IS ERPNDB-FILE-STATUS
                                   RECORD KEY IS PB-CONTROL-BY-ACCOUNT.

           SELECT DISK-DATE        ASSIGN TO SYS019.
           
103012     SELECT RESEND-REPORT    ASSIGN TO SYS011
                                   ORGANIZATION IS LINE SEQUENTIAL. 
103012
103012     SELECT ELMSTR           ASSIGN TO SYS023-3380-ELMSTR5        
103012                             ORGANIZATION IS INDEXED              
103012                             ACCESS IS DYNAMIC                    
103012                             RECORD KEY IS CL-CONTROL-BY-CERT-NO
103012                             FILE STATUS IS ELMSTR-FILE-STATUS.   
100913
100913     SELECT ERNOTE           ASSIGN TO ERNOTE
100913                             ORGANIZATION IS INDEXED
100913                             ACCESS IS DYNAMIC
100913                             RECORD KEY IS CN-CONTROL-PRIMARY
100913                             FILE STATUS IS ERNOTE-FILE-STATUS.
100913
100913     SELECT ELEOBC           ASSIGN TO ELEOBC
100913                             ORGANIZATION IS INDEXED
100913                             ACCESS IS DYNAMIC
100913                             RECORD KEY IS EO-CONTROL-PRIMARY
100913                             FILE STATUS IS ELEOBC-FILE-STATUS.
           
062017     SELECT ERENDT           ASSIGN TO ERENDT
062017                             ACCESS IS DYNAMIC
062017                             ORGANIZATION IS INDEXED
062017                             FILE STATUS IS ERENDT-FILE-STATUS
062017                             RECORD KEY IS EN-CONTROL-PRIMARY.

       DATA DIVISION.

       FILE SECTION.

       FD  ARCH-IN
           RECORDING MODE F
           BLOCK CONTAINS 0.
           
       01  ARCH-IN-RECORD              PIC X(250).

       FD  ERARCH.
                                       COPY ERCARCH.

       FD  ERARCT.
                                       COPY ERCARCT.

       FD  NSASEXTR.
                                       COPY NSCASEXTR.                         

       FD  ELCNTL.
                                       COPY ELCCNTL.

       FD  ELLETR.
                                       COPY ELCTEXT.

       FD  ELCERT.
                                       COPY ELCCERT.

       FD  ERPNDB.
                                       COPY ERCPNDB.


       FD  DISK-DATE
           COPY ELCDTEFD.

103012 FD  RESEND-REPORT
           RECORDING MODE F
           LABEL RECORDS STANDARD
           BLOCK CONTAINS 0 RECORDS.
103012 01  RESEND-RPT-RECORD                 PIC X(132).
103012
103012 FD  ELMSTR.
103012                                 COPY ELCMSTR.
100913
100913 FD  ERNOTE.
100913                                 COPY ERCNOTE.
100913
100913 FD  ELEOBC.
100913                                 COPY ELCEOBC.

062017 FD  ERENDT.
062017                                 COPY ERCENDT.


         WORKING-STORAGE SECTION.
       77  FILLER  PIC X(32) VALUE '********************************'.
       77  FILLER  PIC X(32) VALUE '   CIDNSARE  WORKING-STORAGE     '.
       77  FILLER  PIC X(32) VALUE '*********** VMOD=2.001. ********'.

       77  WS-ABEND-FILE-STATUS        PIC XX            VALUE ZERO.    
       77  WS-ABEND-MESSAGE            PIC X(80)         VALUE SPACES.  
       77  WS-ABEND-PROGRAM            PIC X(8)          VALUE SPACES.  
       77  WS-RETURN-CODE              PIC S9(4)         VALUE +0.      
       77  WS-ZERO                     PIC S9     COMP-3 VALUE +0.      
       77  WS-EOF-SW                   PIC X VALUE SPACES.
           88  END-OF-ARCH             VALUE 'Y'.
       77  WS-DONE-SW                  PIC X VALUE SPACES.
           88  DONE-WITH-ARCH          VALUE 'Y'.
       77  WS-NEW-ARCH-SW              PIC X VALUE 'N'.
           88 HAVE-NEW-ARCH-NO               VALUE 'Y'.
           88 NEED-NEW-ARCH-NO               VALUE 'N'.
       77  WS-CERT-FOUND-SW            PIC X VALUE ' '.
           88  WS-CERT-FOUND                 VALUE 'Y'.
       77  WS-CERT-STATUS-SW           PIC X VALUE ' '.
           88  CERT-CANCELLED                VALUE 'C'.
103012 77  WS-CERT-CLAIM-SW            PIC X VALUE ' '.
103012     88  CERT-HAS-OPEN-CLM             VALUE 'Y'.
       77  ARCH-RECS-IN                PIC 9(9) VALUE ZEROS.
       77  NSAS-RECS-OUT               PIC 9(9) VALUE ZEROS.
       77  NSAS-RECS-DEL               PIC 9(9) VALUE ZEROS.
       77  ERARCH-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERARCT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  NSAS-FILE-STATUS            PIC XX  VALUE LOW-VALUES.
062017 77  ERENDT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCNTL-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELLETR-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ELCERT-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  ERPNDB-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
103012 77  ELMSTR-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
100913 77  ERNOTE-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
100913 77  ELEOBC-FILE-STATUS          PIC XX  VALUE LOW-VALUES.
       77  PGM-SUB                     COMP-3 PIC S9(04) VALUE +585.
       77  WS-CURRENT-TIME             PIC S9(7)   VALUE ZERO.
       77  WS-CURRENT-BIN-DT           PIC X(02)  VALUE LOW-VALUES.
123113 77  WS-NEXT-BUS-BIN-DT          PIC X(02)  VALUE LOW-VALUES.
       77  WS-ARCHIVE-NO               PIC S9(8)   COMP VALUE +0.
       77  WS-RESEND-DATE              PIC XX  VALUE LOW-VALUES.
       77  WS-FOLLOW-UP-DATE           PIC XX  VALUE LOW-VALUES.
       77  WS-COV-CNT                  PIC S999 COMP-3 VALUE +0.
       77  WS-STOP-DATE                PIC XX  VALUE LOW-VALUES.
       77  WS-ZEROS                    PIC S9(03) VALUE +0 COMP-3.
103012 77  WS-LINE-CNT                 PIC S9(03) VALUE +0 COMP-3.
103012 77  WS-PAGE-CNT                 PIC S9(03) VALUE +0 COMP-3.
100713 77  WS-BYPASS-Z-REC             PIC X  VALUE ' '.
100713     88  BYPASS-REC                 VALUE 'Y'.
100913 77  NOTE-SUB                    PIC S9(5) COMP-3 VALUE +0.
100913 77  WS-CERT-UPDATE-SW           PIC X  VALUE ' '.
100913     88  NO-CERT-RW                 VALUE 'N'.
100913     88  CERT-RW                    VALUE 'Y'.
062017 77  v1                          pic s999 comp-3 value +0.
062017 77  WS-REF-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
062017 77  WS-INT-AMT                  PIC S9(7)V99 COMP-3 VALUE +0.
062017 77  ws-dis-int-amt              pic 9(7).99 value zeros.
062017 77  i1                          pic s999 comp-3 value +0.
062017 77  ws-dis-eff-dt               pic x(8).
062017 77  b1                          pic s9(5) comp-3 value +0.
062017 77  cntr                        pic s999 comp-3 value +0.
061818 77  ws-val-bin-dt               pic xx value low-values.

       01  MISC.
062017     05  ws-lf-orig-prm-amt      pic s9(7)v99 comp-3 value +0.
062017     05  ws-lf-orig-alt-prm-amt  pic s9(7)v99 comp-3 value +0.
062017     05  ws-ah-orig-prm-amt      pic s9(7)v99 comp-3 value +0.
062017     05  ws-done-sw-endt         pic x value ' '.
062017         88  i-am-done             value 'Y'.
           05  WRK-RUN-DTE             PIC 9(11) COMP-3.
           05  WRK-ORIG-COMPANY-CD     PIC X.
           05  WRK-ORIG-ARCHIVE-NO     PIC S9(8) COMP.
           05  WRK-ORIG-RESEND-LETR    PIC X(5).
           05  WRK-ORIG-CERT-NO        PIC X(11).
           05  WRK-ORIG-CARRIER        PIC X(01).
           05  WRK-ORIG-GROUPING       PIC X(06).
           05  WRK-ORIG-STATE          PIC X(02).
           05  WRK-ORIG-ACCOUNT        PIC X(10).
           05  WRK-ORIG-EFFECT-DATE    PIC X(02).
           05  WRK-ORIG-ENTRY          PIC X(06).
           05  WRK-ORIG-PROCESSOR-CD   PIC X(04).
           
           05  WS-COMMENT-INDEX        PIC S9(4) COMP.
           05  WS-COMMENT-LINE         PIC X(69) VALUE SPACES.
           05  WS-ADD-ARCT             PIC  X      VALUE 'N'.    
100913
100913     05  WS-BILLING-NOTE.
100913         10  WS-BN-NOTE          PIC X(25).
100913         10  WS-BN-LTRID         PIC X(4).
100913         10  FILLER              PIC X(3).
100913         10  WS-BN-DATE          PIC X(8).
100913         10  FILLER              PIC X(3).
100913         10  WS-BN-USERID        PIC X(4).
100913         10  FILLER              PIC X(30).
100913     05  WS-LEN                  PIC S9(5) COMP-3 VALUE +0.
           
       01  WS-REASONS.
103012     05  WS-CANCELLED            PIC X(19)
103012         VALUE 'CERT CANCELLED     '.
103012     05  WS-DEATH                PIC X(19)
103012         VALUE 'DEATH              '.
103012     05  WS-EXPIRED              PIC X(19)
103012         VALUE 'CERT EXPIRED       '.
103012     05  WS-SETTLEMENT           PIC X(19)
103012         VALUE 'A&H SETTLEMENT     '.
103012     05  WS-NOT-FOUND            PIC X(19)
103012         VALUE 'CERT NOT FOUND     '.
103012     05  WS-OPEN-CLAIM           PIC X(19)
103012         VALUE 'CERT HAS OPEN CLAIM'.
100713     05  WS-Z-RECORD             PIC X(19)
100713         VALUE 'Z REC WAS NOT FOUND'.
103012     05  WS-MISSING-MSG.
103012         10  FILLER              PIC X(28)
103012             VALUE 'PLEASE CREATE RESEND LETTER '.
103012         10  MISSING-RESEND-LETTERID PIC X(4).
103012     05  WS-RESEND-MSG.
103012         10  FILLER              PIC X(14)
103012             VALUE 'RESEND LETTER '.
103012         10  RESEND-LETTERID PIC X(4).
103012         10  FILLER              PIC X(14)
103012             VALUE ' NOT CREATED, '.
103012         10  RESEND-MSG-REASON   PIC X(19).
           
      ****  Z RECORD LAYOUT MOVED TO COPYBOOK ELCZREC
                                           COPY ELCZREC.

062017 01  filler.
062017     05  ws-state-int-table occurs 70.
062017         10  ws-state-abbr           pic xx.
062017         10  ws-state-ref-int-rate   pic s9v9(4) comp-3 value +0.
062017
062017 01  ws-variable-table.
062017     05  ws-var-table.
062017         10  fld-001             pic x(20).
062017         10  fld-002             pic x(20).
062017         10  fld-003             pic x(20).
062017         10  fld-004             pic x(20).
062017         10  fld-005             pic x(20).
062017         10  fld-006             pic x(20).
062017         10  fld-007             pic x(20).
062017         10  fld-008             pic x(20).
062017         10  fld-009             pic x(20).
062017         10  fld-010             pic x(20).
062017         10  fld-011             pic x(20).
062017         10  fld-012             pic x(20).
062017         10  fld-013             pic x(20).
062017         10  fld-014             pic x(20).
062017         10  fld-015             pic x(20).
062017         10  fld-016             pic x(20).
062017         10  fld-017             pic x(20).
062017         10  fld-018             pic x(20).
062017         10  fld-019             pic x(20).
062017         10  fld-020             pic x(20).
062017         10  fld-021             pic x(20).
062017         10  fld-022             pic x(20).
062017         10  fld-023             pic x(20).
062017         10  fld-024             pic x(20).
062017         10  fld-025             pic x(20).
062017         10  fld-026             pic x(20).
062017         10  fld-027             pic x(20).
062017         10  fld-028             pic x(20).
062017         10  fld-029             pic x(20).
062017         10  fld-030             pic x(20).
062017         10  fld-031             pic x(20).
062017         10  fld-032             pic x(20).
062017         10  fld-033             pic x(20).
062017         10  fld-034             pic x(20).
062017         10  fld-035             pic x(20).
062017         10  fld-036             pic x(20).
062017         10  fld-037             pic x(20).
062017         10  fld-038             pic x(20).
062017         10  fld-039             pic x(20).
062017         10  fld-040             pic x(20).
062017         10  fld-041             pic x(20).
062017         10  fld-042             pic x(20).
062017         10  fld-043             pic x(20).
062017         10  fld-044             pic x(20).
062017         10  fld-045             pic x(20).
062017         10  fld-046             pic x(20).
062017         10  fld-047             pic x(20).
062017         10  fld-048             pic x(20).
062017         10  fld-049             pic x(20).
062017         10  fld-050             pic x(20).
062017         10  fld-051             pic x(20).
062017         10  fld-052             pic x(20).
062017         10  fld-053             pic x(20).
062017         10  fld-054             pic x(20).
062017         10  fld-055             pic x(20).
062017         10  fld-056             pic x(20).
062017         10  fld-057             pic x(20).
062017         10  fld-058             pic x(20).
062017         10  fld-059             pic x(20).
062017         10  fld-060             pic x(20).
062017         10  fld-061             pic x(20).
062017         10  fld-062             pic x(20).
062017         10  fld-063             pic x(20).
062017         10  fld-064             pic x(20).
062017         10  fld-065             pic x(20).
062017         10  fld-066             pic x(20).
062017         10  fld-067             pic x(20).
062017         10  fld-068             pic x(20).
062017         10  fld-069             pic x(20).
062017         10  fld-070             pic x(20).
062017         10  fld-071             pic x(20).
062017         10  fld-072             pic x(20).
062017         10  fld-073             pic x(20).
062017         10  fld-074             pic x(20).
062017         10  fld-075             pic x(20).
062017         10  fld-076             pic x(20).
062017         10  fld-077             pic x(20).
062017         10  fld-078             pic x(20).
062017         10  fld-079             pic x(20).
062017         10  fld-080             pic x(20).
062017         10  fld-081             pic x(20).
062017         10  fld-082             pic x(20).
062017         10  fld-083             pic x(20).
062017         10  fld-084             pic x(20).
062017         10  fld-085             pic x(20).
062017         10  fld-086             pic x(20).
062017         10  fld-087             pic x(20).
062017         10  fld-088             pic x(20).
062017         10  fld-089             pic x(20).
062017         10  fld-090             pic x(20).
062017         10  fld-091             pic x(20).
062017         10  fld-092             pic x(20).
062017         10  fld-093             pic x(20).
062017         10  fld-094             pic x(20).
062017         10  fld-095             pic x(20).
062017         10  fld-096             pic x(20).
062017         10  fld-097             pic x(20).
062017         10  fld-098             pic x(20).
062017         10  fld-099             pic x(20).
062017         10  fld-100             pic x(20).
062017         10  fld-101             pic x(20).
062017         10  fld-102             pic x(20).
062017         10  fld-103             pic x(20).
062017         10  fld-104             pic x(20).
062017         10  fld-105             pic x(20).
062017         10  fld-106             pic x(20).
062017         10  fld-107             pic x(20).
062017         10  fld-108             pic x(20).
062017         10  fld-109             pic x(20).
062017         10  fld-110             pic x(20).
062017         10  fld-111             pic x(20).
062017         10  fld-112             pic x(20).
062017         10  fld-113             pic x(20).
062017         10  fld-114             pic x(20).
062017         10  fld-115             pic x(20).
062017         10  fld-116             pic x(20).
062017         10  fld-117             pic x(20).
062017         10  fld-118             pic x(20).
062017         10  fld-119             pic x(20).
062017         10  fld-120             pic x(20).
062017         10  fld-121             pic x(20).
062017         10  fld-122             pic x(20).
062017         10  fld-123             pic x(20).
062017         10  fld-124             pic x(20).
062017         10  fld-125             pic x(20).
062017         10  fld-126             pic x(20).
062017         10  fld-127             pic x(20).
062017         10  fld-128             pic x(20).
062017         10  fld-129             pic x(20).
062017         10  fld-130             pic x(20).
062017         10  fld-131             pic x(20).
062017         10  fld-132             pic x(20).
062017         10  fld-133             pic x(20).
062017         10  fld-134             pic x(20).
062017         10  fld-135             pic x(20).
062017         10  fld-136             pic x(20).
062017         10  fld-137             pic x(20).
062017         10  fld-138             pic x(20).
062017         10  fld-139             pic x(20).
062017         10  fld-140             pic x(20).
062017         10  fld-141             pic x(20).
062017         10  fld-142             pic x(20).
062017         10  fld-143             pic x(20).
062017         10  fld-144             pic x(20).
062017         10  fld-145             pic x(20).
062017         10  fld-146             pic x(20).
062017         10  fld-147             pic x(20).
062017         10  fld-148             pic x(20).
062017         10  fld-149             pic x(20).
062017         10  fld-150             pic x(20).
062017         10  fld-151             pic x(20).
062017         10  fld-152             pic x(20).
062017         10  fld-153             pic x(20).
062017         10  fld-154             pic x(20).
062017         10  fld-155             pic x(20).
062017         10  fld-156             pic x(20).
062017         10  fld-157             pic x(20).
062017         10  fld-158             pic x(20).
062017         10  fld-159             pic x(20).
062017         10  fld-160             pic x(20).
062017         10  fld-161             pic x(20).
062017         10  fld-162             pic x(20).
062017         10  fld-163             pic x(20).
062017         10  fld-164             pic x(20).
062017         10  fld-165             pic x(20).
062017         10  fld-166             pic x(20).
062017         10  fld-167             pic x(20).
062017         10  fld-168             pic x(20).
062017         10  fld-169             pic x(20).
062017         10  fld-170             pic x(20).
062017         10  fld-171             pic x(20).
062017         10  fld-172             pic x(20).
062017         10  fld-173             pic x(20).
062017         10  fld-174             pic x(20).
062017         10  fld-175             pic x(20).
062017         10  fld-176             pic x(20).
062017         10  fld-177             pic x(20).
062017         10  fld-178             pic x(20).
062017         10  fld-179             pic x(20).
062017         10  fld-180             pic x(20).
062017         10  fld-181             pic x(20).
062017         10  fld-182             pic x(20).
062017         10  fld-183             pic x(20).
062017         10  fld-184             pic x(20).
062017         10  fld-185             pic x(20).
062017         10  fld-186             pic x(20).
062017         10  fld-187             pic x(20).
062017         10  fld-188             pic x(20).
062017         10  fld-189             pic x(20).
062017         10  fld-190             pic x(20).
062017         10  fld-191             pic x(20).
062017         10  fld-192             pic x(20).
062017         10  fld-193             pic x(20).
062017         10  fld-194             pic x(20).
062017         10  fld-195             pic x(20).
062017         10  fld-196             pic x(20).
062017         10  fld-197             pic x(20).
062017         10  fld-198             pic x(20).
062017         10  fld-199             pic x(20).
062017         10  fld-200             pic x(20).
062017         10  fld-201             pic x(20).
062017         10  fld-202             pic x(20).
062017         10  fld-203             pic x(20).
062017         10  fld-204             pic x(20).
062017         10  fld-205             pic x(20).
062017         10  fld-206             pic x(20).
062017         10  fld-207             pic x(20).
062017         10  fld-208             pic x(20).
062017         10  fld-209             pic x(20).
062017         10  fld-210             pic x(20).
062017         10  fld-211             pic x(20).
062017         10  fld-212             pic x(20).
062017         10  fld-213             pic x(20).
062017         10  fld-214             pic x(20).
062017         10  fld-215             pic x(20).
062017         10  fld-216             pic x(20).
062017         10  fld-217             pic x(20).
062017         10  fld-218             pic x(20).
062017         10  fld-219             pic x(20).
062017         10  fld-220             pic x(20).
062017         10  fld-221             pic x(20).
062017         10  fld-222             pic x(20).
062017         10  fld-223             pic x(20).
062017         10  fld-224             pic x(20).
062017         10  fld-225             pic x(20).
062017         10  fld-226             pic x(20).
062017         10  fld-227             pic x(20).
062017         10  fld-228             pic x(20).
062017         10  fld-229             pic x(20).
062017         10  fld-230             pic x(20).
062017         10  fld-231             pic x(20).
062017         10  fld-232             pic x(20).
062017         10  fld-233             pic x(20).
062017         10  fld-234             pic x(20).
062017         10  fld-235             pic x(20).
062017         10  fld-236             pic x(20).
062017         10  fld-237             pic x(20).
062017         10  fld-238             pic x(20).
062017         10  fld-239             pic x(20).
062017         10  fld-240             pic x(20).
062017         10  fld-241             pic x(20).
062017         10  fld-242             pic x(20).
062017         10  fld-243             pic x(20).
062017         10  fld-244             pic x(20).
062017         10  fld-245             pic x(20).
062017         10  fld-246             pic x(20).
062017         10  fld-247             pic x(20).
062017         10  fld-248             pic x(20).
062017         10  fld-249             pic x(20).
062017         10  fld-250             pic x(20).
062017         10  fld-251             pic x(20).
062017         10  fld-252             pic x(20).
062017         10  fld-253             pic x(20).
062017         10  fld-254             pic x(20).
062017         10  fld-255             pic x(20).
062017         10  fld-256             pic x(20).
062017         10  fld-257             pic x(20).
062017         10  fld-258             pic x(20).
062017         10  fld-259             pic x(20).
062017         10  fld-260             pic x(20).
062017         10  fld-261             pic x(20).
062017         10  fld-262             pic x(20).
062017         10  fld-263             pic x(20).
062017         10  fld-264             pic x(20).
062017         10  fld-265             pic x(20).
062017         10  fld-266             pic x(20).
062017         10  fld-267             pic x(20).
062017         10  fld-268             pic x(20).
062017         10  fld-269             pic x(20).
062017         10  fld-270             pic x(20).
062017         10  fld-271             pic x(20).
062017         10  fld-272             pic x(20).
062017         10  fld-273             pic x(20).
062017         10  fld-274             pic x(20).
062017         10  fld-275             pic x(20).
062017         10  fld-276             pic x(20).
062017         10  fld-277             pic x(20).
062017         10  fld-278             pic x(20).
062017         10  fld-279             pic x(20).
062017         10  fld-280             pic x(20).
062017         10  fld-281             pic x(20).
062017         10  fld-282             pic x(20).
062017         10  fld-283             pic x(20).
062017         10  fld-284             pic x(20).
062017         10  fld-285             pic x(20).
062017         10  fld-286             pic x(20).
062017         10  fld-287             pic x(20).
062017         10  fld-288             pic x(20).
062017         10  fld-289             pic x(20).
062017         10  fld-290             pic x(20).
062017         10  fld-291             pic x(20).
062017         10  fld-292             pic x(20).
062017         10  fld-293             pic x(20).
062017         10  fld-294             pic x(20).
062017         10  fld-295             pic x(20).
062017         10  fld-296             pic x(20).
062017         10  fld-297             pic x(20).
062017         10  fld-298             pic x(20).
062017         10  fld-299             pic x(20).
062017         10  fld-300             pic x(20).
062017         10  fld-301             pic x(20).
062017         10  fld-302             pic x(20).
062017         10  fld-303             pic x(20).
062017         10  fld-304             pic x(20).
062017         10  fld-305             pic x(20).
062017         10  fld-306             pic x(20).
062017         10  fld-307             pic x(20).
062017         10  fld-308             pic x(20).
062017         10  fld-309             pic x(20).
062017         10  fld-310             pic x(20).
062017         10  fld-311             pic x(20).
062017         10  fld-312             pic x(20).
062017         10  fld-313             pic x(20).
062017         10  fld-314             pic x(20).
062017         10  fld-315             pic x(20).
062017         10  fld-316             pic x(20).
062017         10  fld-317             pic x(20).
062017         10  fld-318             pic x(20).
062017         10  fld-319             pic x(20).
062017         10  fld-320             pic x(20).
062017         10  fld-321             pic x(20).
062017         10  fld-322             pic x(20).
062017         10  fld-323             pic x(20).
062017         10  fld-324             pic x(20).
062017         10  fld-325             pic x(20).
062017         10  fld-326             pic x(20).
062017         10  fld-327             pic x(20).
062017         10  fld-328             pic x(20).
062017         10  fld-329             pic x(20).
062017         10  fld-330             pic x(20).
062017         10  fld-331             pic x(20).
062017         10  fld-332             pic x(20).
062017         10  fld-333             pic x(20).
062017         10  fld-334             pic x(20).
062017         10  fld-335             pic x(20).
062017         10  fld-336             pic x(20).
062017         10  fld-337             pic x(20).
062017         10  fld-338             pic x(20).
062017         10  fld-339             pic x(20).
062017         10  fld-340             pic x(20).
062017         10  fld-341             pic x(20).
062017         10  fld-342             pic x(20).
062017         10  fld-343             pic x(20).
062017         10  fld-344             pic x(20).
062017         10  fld-345             pic x(20).
062017         10  fld-346             pic x(20).
062017         10  fld-347             pic x(20).
062017         10  fld-348             pic x(20).
062017         10  fld-349             pic x(20).
062017         10  fld-350             pic x(20).
062017         10  fld-351             pic x(20).
062017         10  fld-352             pic x(20).
062017         10  fld-353             pic x(20).
062017         10  fld-354             pic x(20).
062017         10  fld-355             pic x(20).
062017         10  fld-356             pic x(20).
062017         10  fld-357             pic x(20).
062017         10  fld-358             pic x(20).
062017         10  fld-359             pic x(20).
062017         10  fld-360             pic x(20).
062017         10  fld-361             pic x(20).
062017         10  fld-362             pic x(20).
062017         10  fld-363             pic x(20).
062017         10  fld-364             pic x(20).
062017         10  fld-365             pic x(20).
062017         10  fld-366             pic x(20).
062017         10  fld-367             pic x(20).
062017         10  fld-368             pic x(20).
062017         10  fld-369             pic x(20).
062017         10  fld-370             pic x(20).
062017         10  fld-371             pic x(20).
062017         10  fld-372             pic x(20).
062017         10  fld-373             pic x(20).
062017         10  fld-374             pic x(20).
062017         10  fld-375             pic x(20).
062017         10  fld-376             pic x(20).
062017         10  fld-377             pic x(20).
062017         10  fld-378             pic x(20).
062017         10  fld-379             pic x(20).
062017         10  fld-380             pic x(20).
062017         10  fld-381             pic x(20).
062017         10  fld-382             pic x(20).
062017         10  fld-383             pic x(20).
062017         10  fld-384             pic x(20).
062017         10  fld-385             pic x(20).
062017         10  fld-386             pic x(20).
062017         10  fld-387             pic x(20).
062017         10  fld-388             pic x(20).
062017         10  fld-389             pic x(20).
062017         10  fld-390             pic x(20).
062017         10  fld-391             pic x(20).
062017         10  fld-392             pic x(20).
062017         10  fld-393             pic x(20).
062017         10  fld-394             pic x(20).
062017         10  fld-395             pic x(20).
062017         10  fld-396             pic x(20).
062017         10  fld-397             pic x(20).
062017         10  fld-398             pic x(20).
062017         10  fld-399             pic x(20).
062017         10  fld-400             pic x(20).
062017     05  filler redefines ws-var-table occurs 400.
062017         10  ws-variable-value   pic x(20).

       01  EXT-REC.
           05 EXT-CONTROL-PRIMARY.
              10  EXT-COMPANY-CD       PIC X.
              10  EXT-ARCHIVE-NO       PIC 9(8) BINARY.
              10  EXT-SEQ-NO           PIC 9(4) BINARY.
           05  EXT-LETTER-VARIABLES.
               10  EXT-LETTER-ID       PIC X(5).
               10  FILLER              PIC X(4345).
           05  FILLER                  PIC X(143).

       01  BLANK-LINE.
           05  FILLER                  PIC X(132) VALUE SPACES.

       01  REPORT-HEAD1.
103012     05  FILLER                  PIC X(08) VALUE 'CIDNSARE'.
103012     05  FILLER                  PIC X(22) VALUE SPACES.
           05  FILLER                  PIC X(23)
               VALUE 'CENTRAL STATES OF OMAHA'.
           05  FILLER                  PIC X(23) VALUE SPACES.
           05  RPT-DATE                PIC X(08) VALUE SPACES.
           05  FILLER                  PIC X(48)  VALUE SPACES.

       01  REPORT-HEAD2.
           05  FILLER                  PIC X(27) VALUE SPACES.
           05  FILLER                  PIC X(29)
103012         VALUE 'RESEND LETTER ERROR MESSAGES '.
103012     05  FILLER                  PIC X(20) VALUE SPACES.
103012     05  FILLER                  PIC X(05) VALUE 'PAGE '.
103012     05  RPT-PAGE                PIC ZZ9   VALUE ZEROS.
103012     05  FILLER                  PIC X(48) VALUE SPACES.

       01  REPORT-HEADER.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(11)
               VALUE 'CERT NUMBER'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(8)
               VALUE 'ARCH NUM'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(9)
               VALUE 'LETTER ID'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(10)
               VALUE 'CREATED BY'.
           05  FILLER                  PIC X(2) VALUE SPACES.
           05  FILLER                  PIC X(11)
               VALUE 'RESEND DATE'.
           05  FILLER                  PIC X(5) VALUE SPACES.
           05  FILLER                  PIC X(7)
               VALUE 'MESSAGE'.
           05  FILLER                  PIC X(62) VALUE SPACES.

103012 01  REPORT-REC.
           05  FILLER                  PIC X(2) VALUE SPACES.
103012     05  REPORT-CERTNO          PIC X(11).
           05  FILLER                  PIC X(2) VALUE SPACES.
103012     05  REPORT-ARCHIVENO       PIC 9(8).
           05  FILLER                  PIC X(4) VALUE SPACES.
103012     05  REPORT-LETTERID        PIC X(4).
           05  FILLER                  PIC X(8) VALUE SPACES.
103012     05  REPORT-PROCID          PIC X(4).
           05  FILLER                  PIC X(6) VALUE SPACES.
103012     05  REPORT-RESEND-DT       PIC X(10).
           05  FILLER                  PIC X(2) VALUE SPACES.
103012     05  REPORT-MSG             PIC X(52).
           05  FILLER                  PIC X(20) VALUE SPACES.

                                       COPY ELCDTECX.
                                       COPY ELCDTEVR.
                                       COPY ELCDATE.

       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-CYCLE-DATE             PIC X(08)     VALUE SPACES.
061818     05  parm-ld-cycle-date          pic x(08)     value spaces.
061818     05  parm-eom-date               pic x(08)     value spaces.
061818     05  parm-nb-cycle-date          pic x(08)     value spaces.

       PROCEDURE DIVISION USING PARM.

           IF PARM-CYCLE-DATE = SPACES
               DISPLAY 'MISSING CYCLE DATE PARM'
               PERFORM ABEND-PGM
           END-IF.

       0000-DATE-CARD-READ. COPY ELCDTERX.

           PERFORM 0040-OPEN-FILES     THRU 0040-EXIT.
           PERFORM 0050-INIT           THRU 0050-EXIT.

           MOVE LOW-VALUES             TO LA-CONTROL-PRIMARY.
           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD.
           MOVE ZEROS                  TO LA-ARCHIVE-NO.

           PERFORM 0020-PROCESS        THRU 0020-EXIT UNTIL
              (END-OF-ARCH).
           PERFORM 0060-CLOSE-FILES    THRU 0060-EXIT.

           DISPLAY ' ARCH RECS READ    ' ARCH-RECS-IN.
           DISPLAY ' LETTER RECS WRITE ' NSAS-RECS-OUT.

           GOBACK.

       0020-PROCESS.
           
           PERFORM 0024-READ-ARCH-IN THRU 0024-EXIT.
           IF END-OF-ARCH
               GO TO 0020-EXIT
           END-IF.
           
           IF LA-COMPANY-CD NOT EQUAL DTE-CLASIC-COMPANY-CD
               GO TO 0020-EXIT
           END-IF

           IF LA-TEMP
               GO TO 0020-EXIT
           END-IF.

           IF (LA-RESEND-DATE EQUAL LOW-VALUES OR SPACES) OR
              (LA-RESEND-LETR EQUAL LOW-VALUES OR SPACES) OR
              (LA-INITIAL-PRINT-DATE EQUAL LOW-VALUES OR SPACES)
                 GO TO 0020-EXIT
           END-IF.

           IF  LA-RESEND-DATE GREATER THAN WS-CURRENT-BIN-DT
               GO TO 0020-EXIT
           END-IF.
           
           IF (LA-SENT-DATE NOT EQUAL LOW-VALUES AND SPACES) OR
              (LA-REPLY-DATE NOT EQUAL LOW-VALUES AND SPACES) OR
              (LA-VOIDED-DATE NOT EQUAL LOW-VALUES AND SPACES) OR
              (LA-PURGED-DATE NOT EQUAL LOW-VALUES AND SPACES) 
                 GO TO 0020-EXIT
           END-IF.

           PERFORM 0025-READ-ERARCH THRU 0025-EXIT.
           
           MOVE SPACES TO WS-CERT-FOUND-SW
                          WS-CERT-STATUS-SW.
100713     MOVE SPACES TO WS-BYPASS-Z-REC.
100713
           PERFORM 0200-CHECK-CERT-STATUS THRU 0200-EXIT.
           IF (CERT-CANCELLED)
             OR (NOT WS-CERT-FOUND)
103012       OR (CERT-HAS-OPEN-CLM)
100713       OR (BYPASS-REC)
103012        DISPLAY 'RESEND LETTER NOT CREATED '
                 LA-CERT-NO-A2 ' ' LA-ARCHIVE-NO ' ' LA-FORM-A3 
103012           ' ' WS-COMMENT-LINE
103012        MOVE LA-RESEND-LETR TO RESEND-LETTERID
103012        MOVE WS-COMMENT-LINE (1:19) TO RESEND-MSG-REASON
103012        MOVE WS-RESEND-MSG TO REPORT-MSG
103012        PERFORM 0038-WRITE-RESEND-REPORT THRU 0038-EXIT
              MOVE 'C'                 TO LA-STATUS
              MOVE WS-STOP-DATE        TO LA-VOIDED-DATE
              MOVE 'SYST'              TO LA-LAST-UPDATED-BY
              MOVE WS-CURRENT-BIN-DT   TO LA-LAST-MAINT-DATE
              MOVE WS-CURRENT-TIME     TO LA-LAST-MAINT-TIME
              REWRITE LETTER-ARCHIVE
              IF ERARCH-FILE-STATUS NOT = '00'
                 DISPLAY ' ERROR ON ERARCH - REWRITE - CANCEL '
                    ERARCH-FILE-STATUS  '  CERT '  LA-CERT-NO-A2
              END-IF
              
              PERFORM 0900-READ-ERARCT THRU 0900-EXIT
              IF WS-COMMENT-INDEX = +20
                  DISPLAY ' MAX COMMENTS ON CERT ' LA-CERT-NO-A2
                  GO TO 0020-EXIT
              END-IF
              SET WS-COMMENT-INDEX UP BY +1
              SET LC-NDX TO WS-COMMENT-INDEX
              MOVE WS-COMMENT-LINE TO  LT-COMMENT-LINE (LC-NDX)
              MOVE WS-CURRENT-BIN-DT TO LT-COMMENT-CHG-DT (LC-NDX)
              MOVE 'SYST' TO LT-COMMENT-CHG-BY (LC-NDX)
              ADD +1      TO LT-NUM-LINES-ON-RECORD
              IF WS-ADD-ARCT = 'N'
                 REWRITE LETTER-ARCHIVE-TEXT
              ELSE
                 WRITE LETTER-ARCHIVE-TEXT
              END-IF

              GO TO 0020-EXIT
           END-IF.

           PERFORM 0030-PROCESS-ARCHIVE  THRU 0030-EXIT.
                      
       0020-EXIT.
           EXIT.

       0024-READ-ARCH-IN.

           READ ARCH-IN INTO LETTER-ARCHIVE
               AT END
               SET END-OF-ARCH     TO TRUE.
           
           IF NOT END-OF-ARCH 
              ADD 1                TO ARCH-RECS-IN
           END-IF.

           
       0024-EXIT.
           EXIT.


       0025-READ-ERARCH.

           READ ERARCH.

           IF ERARCH-FILE-STATUS NOT = '00'
              DISPLAY 'BAD READ ERARCH ' LA-CERT-NO-A2 ' ' 
              LA-ARCHIVE-NO '-' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
                      
       0025-EXIT.
           EXIT.

       0030-PROCESS-ARCHIVE.

           MOVE 'N'              TO WS-DONE-SW.
           MOVE 'N'              TO WS-NEW-ARCH-SW.
           MOVE LA-COMPANY-CD    TO NSAS-COMPANY-CD
                                    WRK-ORIG-COMPANY-CD.
           MOVE LA-ARCHIVE-NO    TO NSAS-ARCHIVE-NO
                                    WRK-ORIG-ARCHIVE-NO.
           MOVE LA-RESEND-LETR   TO WRK-ORIG-RESEND-LETR.
           MOVE LA-CERT-NO-A2    TO WRK-ORIG-CERT-NO.
           MOVE LA-CARRIER-A2    TO WRK-ORIG-CARRIER.
           MOVE LA-GROUPING-A2   TO WRK-ORIG-GROUPING.
           MOVE LA-STATE-A2      TO WRK-ORIG-STATE.
           MOVE LA-ACCOUNT-A2    TO WRK-ORIG-ACCOUNT.
           MOVE LA-EFFECT-DATE-A2 TO WRK-ORIG-EFFECT-DATE.
           MOVE LA-PROCESSOR-CD  TO WRK-ORIG-PROCESSOR-CD.
           MOVE LA-ENTRY-A6      TO WRK-ORIG-ENTRY.
           MOVE ZEROS            TO NSAS-SEQ-NO.
           
           PERFORM 0125-READ-NSASEXTR    THRU 0125-EXIT
           IF NSAS-FILE-STATUS NOT = '00'
103012         MOVE LA-RESEND-LETR TO MISSING-RESEND-LETTERID
103012         MOVE WS-MISSING-MSG TO REPORT-MSG
               PERFORM 0038-WRITE-RESEND-REPORT THRU 0038-EXIT
               DISPLAY 'NAPERSOFT EXTRACT NOT FOUND FOR ' 
062017          la-cert-no-a2 ' ' WRK-ORIG-ARCHIVE-NO
               GO TO 0030-EXIT
           END-IF.
           IF NSAS-COMPANY-CD NOT EQUAL WRK-ORIG-COMPANY-CD  OR
              NSAS-ARCHIVE-NO NOT EQUAL WRK-ORIG-ARCHIVE-NO
103012            MOVE LA-RESEND-LETR TO MISSING-RESEND-LETTERID
103012            MOVE WS-MISSING-MSG TO REPORT-MSG
                  PERFORM 0038-WRITE-RESEND-REPORT THRU 0038-EXIT
                  DISPLAY 'NAPERSOFT EXTRACT NOT FOUND FOR ' 
062017            la-cert-no-a2 ' ' WRK-ORIG-ARCHIVE-NO
                  GO TO 0030-EXIT
           END-IF.

           MOVE WS-CURRENT-BIN-DT   TO LA-SENT-DATE
                                       LA-LAST-RESENT-PRINT-DATE.
           MOVE 'C'                 TO LA-STATUS.
           MOVE 'SYST'              TO LA-LAST-UPDATED-BY.
           MOVE WS-CURRENT-BIN-DT   TO LA-LAST-MAINT-DATE.
           MOVE WS-CURRENT-TIME     TO LA-LAST-MAINT-TIME.
                                     
           REWRITE LETTER-ARCHIVE
                INVALID KEY
                    DISPLAY ' CIDNSARE REWRITE ERROR - ERARCH'
                    SET END-OF-ARCH TO TRUE
                    PERFORM ABEND-PGM.

           PERFORM 0035-WRITE-EXT THRU 0035-EXIT.

       0030-EXIT.
            EXIT.

       0035-WRITE-EXT.
           
           PERFORM 0500-GET-ARCH-NO THRU 0500-EXIT
           PERFORM 1000-BUILD-ERARCH THRU 1000-EXIT

           MOVE WS-ARCHIVE-NO TO EXT-ARCHIVE-NO.
           MOVE WRK-ORIG-RESEND-LETR TO EXT-LETTER-ID 
062017     perform 1100-extract-vars   thru 1100-exit
           WRITE NSAS-EXTRACT-RECORD FROM EXT-REC.
           ADD 1                    TO NSAS-RECS-OUT.

           IF W-LETTER-TO-ACCT NOT EQUAL SPACES
               MOVE W-LETTER-TO-ACCT TO EXT-LETTER-ID (5:1)
               MOVE +1               TO EXT-SEQ-NO
               WRITE NSAS-EXTRACT-RECORD FROM EXT-REC
               ADD 1                 TO NSAS-RECS-OUT
           END-IF.
           
           IF W-LETTER-TO-BENE NOT EQUAL SPACES
               MOVE W-LETTER-TO-BENE TO EXT-LETTER-ID (5:1)
               MOVE +2               TO EXT-SEQ-NO
               WRITE NSAS-EXTRACT-RECORD FROM EXT-REC
               ADD 1                 TO NSAS-RECS-OUT
           END-IF.
           
       0035-EXIT.
            EXIT.

              
       0038-WRITE-RESEND-REPORT.

103012     IF WS-LINE-CNT > +55
103012        ADD +1 TO WS-PAGE-CNT
103012        MOVE WS-PAGE-CNT TO RPT-PAGE
103012        WRITE RESEND-RPT-RECORD FROM REPORT-HEAD1
103012        WRITE RESEND-RPT-RECORD FROM REPORT-HEAD2
103012        WRITE RESEND-RPT-RECORD FROM BLANK-LINE
103012        WRITE RESEND-RPT-RECORD FROM REPORT-HEADER
103012        MOVE +4 TO WS-LINE-CNT
103012     END-IF
       
103012     MOVE LA-CERT-NO-A2       TO REPORT-CERTNO.
103012     MOVE LA-ARCHIVE-NO       TO REPORT-ARCHIVENO.
103012     MOVE LA-FORM-A3          TO REPORT-LETTERID.
103012     MOVE LA-PROCESSOR-CD     TO REPORT-PROCID
           MOVE ' '                 TO DC-OPTION-CODE.
           MOVE LA-RESEND-DATE      TO DC-BIN-DATE-1.
           PERFORM 8510-DATE-CONVERSION THRU 8590-EXIT.
           IF NO-CONVERSION-ERROR
103012        MOVE DC-GREG-DATE-1-EDIT TO REPORT-RESEND-DT
           ELSE
              DISPLAY ' RESEND DATE ERROR '
           END-IF.
           
103012     WRITE RESEND-RPT-RECORD FROM REPORT-REC.
103012     ADD +1 TO WS-LINE-CNT.

       0038-EXIT.
            EXIT.
              
       0040-OPEN-FILES.
       
           OPEN INPUT ARCH-IN.
           OPEN I-O ERARCH.
           OPEN I-O ERARCT.
           OPEN I-O NSASEXTR.
           OPEN I-O ELCNTL.
100913     OPEN I-O ELCERT.
100913     OPEN I-O ERNOTE.
062017     OPEN INPUT ELLETR ERPNDB ELMSTR ELEOBC ERENDT
103012     OPEN OUTPUT RESEND-REPORT.

           IF ERARCH-FILE-STATUS NOT = '00' AND '97'
              DISPLAY 'ERARCH OPEN ERR  ' ERARCH-FILE-STATUS
              MOVE ' ERARCH OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERARCH-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERARCT-FILE-STATUS NOT = '00' AND '97'
              DISPLAY 'ERARCT OPEN ERR  ' ERARCT-FILE-STATUS
              MOVE ' ERARCT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERARCT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF NSAS-FILE-STATUS NOT = '00' AND '97'
              DISPLAY ' NSASEXTR OPEN ' NSAS-FILE-STATUS
              MOVE ' NSASEXTR OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE NSAS-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCNTL-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCNTL OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELLETR-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELLETR OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELLETR-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

062017     IF ERENDT-FILE-STATUS NOT = '00' AND '97'
062017        MOVE ' ERENDT OPEN ERROR ' TO WS-ABEND-MESSAGE
062017        MOVE ERENDT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
062017        PERFORM ABEND-PGM
062017     END-IF.

           IF ELCERT-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ELCERT OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERPNDB-FILE-STATUS NOT = '00' AND '97'
              MOVE ' ERPNDB OPEN ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
103012
103012     IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
103012        MOVE ' ELMSTR OPEN ERROR ' TO WS-ABEND-MESSAGE
103012        MOVE ELMSTR-FILE-STATUS    TO WS-ABEND-FILE-STATUS
103012        PERFORM ABEND-PGM
103012     END-IF.
100913
100913     IF ERNOTE-FILE-STATUS NOT = '00' AND '97'
100913        DISPLAY 'ERNOTE OPEN ERR  ' ERNOTE-FILE-STATUS
100913        MOVE ' ERNOTE OPEN ERROR ' TO WS-ABEND-MESSAGE
100913        MOVE ERNOTE-FILE-STATUS    TO WS-ABEND-FILE-STATUS
100913        PERFORM ABEND-PGM
100913     END-IF.
100913
100913     IF ELEOBC-FILE-STATUS NOT = '00' AND '97'
100913        DISPLAY 'ELEOBC OPEN ERR  ' ELEOBC-FILE-STATUS
100913        MOVE ' ELEOBC OPEN ERROR ' TO WS-ABEND-MESSAGE
100913        MOVE ELEOBC-FILE-STATUS    TO WS-ABEND-FILE-STATUS
100913        PERFORM ABEND-PGM
100913     END-IF.

       0040-EXIT.
           EXIT.

       0050-INIT.
           
062017     perform varying i1 from +1 by +1 until i1 > +70
062017        move spaces              to ws-state-abbr (i1)
062017        move zeros               to ws-state-ref-int-rate (i1)
062017     end-perform

           ACCEPT WS-TIME-OF-DAY       FROM  TIME
           MOVE WS-TIME                TO  WS-CURRENT-TIME

061818     string
061818        parm-cycle-date (5:2) '/'
061818        parm-cycle-date (7:2) '/'
061818        parm-cycle-date (1:4) 
061818           delimited by size into dc-greg-date-a-edit
061818     end-string

           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.
061818     DISPLAY 'CURRENT DATE USED FOR RUN IS - ' dc-greg-date-a-edit
           DISPLAY '* * * * * * * * * * * * * * * * * * * * * '.

061818     move +0                     to dc-elapsed-months
061818                                    dc-elapsed-days
061818     move parm-cycle-date        to dc-greg-date-cymd-r
061818     move 'L'                    to dc-option-code
061818     PERFORM 8510-DATE-CONVERSION.
061818     IF NO-CONVERSION-ERROR
061818        MOVE DC-BIN-DATE-1       TO WS-CURRENT-BIN-DT
061818     else
061818        display ' Problemos with Current cycle date '
061818           parm-cycle-date
061818        perform abend-pgm
061818     END-IF

011314***NEED TO RUN CONVERSION TO GET THE NEXT BUS DT TO WORK PROPERLY
      ***  no you don't, it's already calculated!!!!

061818     DISPLAY 'NEXT BUSINESS DATE IS - '
061818        parm-nb-cycle-date(5:2) '/'
061818        parm-nb-cycle-date(7:2) '/'
061818        parm-nb-cycle-date(1:4)
061818
061818     move +0                     to dc-elapsed-months
061818                                    dc-elapsed-days
061818     move parm-nb-cycle-date     to dc-greg-date-cymd-r
061818     move 'L'                    to dc-option-code
061818     perform 8510-date-conversion
061818     if no-conversion-error
061818        move dc-bin-date-1       to ws-next-bus-bin-dt
061818     else
061818        display ' Problemos with Next Cycle date '
061818           parm-nb-cycle-date
061818        perform abend-pgm
061818     end-if

062017     perform 0230-build-ref-int-table
062017                                 thru 0230-exit

           MOVE WS-CURRENT-DATE TO RPT-DATE.
103012     MOVE +1 TO WS-PAGE-CNT.
103012     MOVE WS-PAGE-CNT TO RPT-PAGE.
103012     WRITE RESEND-RPT-RECORD FROM REPORT-HEAD1.
103012     WRITE RESEND-RPT-RECORD FROM REPORT-HEAD2.
103012     WRITE RESEND-RPT-RECORD FROM BLANK-LINE.
103012     WRITE RESEND-RPT-RECORD FROM REPORT-HEADER.
103012     MOVE +4 TO WS-LINE-CNT.

061818     move WS-CURRENT-BIN-DT      to ws-val-bin-dt

061818     if parm-ld-cycle-date = parm-eom-date
061818        display ' Over-riding interest calc valuation date to '
061818           parm-nb-cycle-date
061818        move ws-next-bus-bin-dt  to ws-val-bin-dt
061818     end-if

           .
       0050-EXIT.
           EXIT.

       0060-CLOSE-FILES.

           CLOSE ARCH-IN.
103012     CLOSE RESEND-REPORT.
           CLOSE ERARCH ERARCT NSASEXTR ELCNTL ELLETR ELCERT ERPNDB.
062017     CLOSE ELMSTR ERENDT
100913     CLOSE ERNOTE ELEOBC.

           IF ERARCH-FILE-STATUS NOT = '00'
              DISPLAY ' ERARCH CLOSE ' ERARCH-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERARCT-FILE-STATUS NOT = '00'
              DISPLAY ' ERARCT CLOSE ' ERARCT-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

062017     IF ERENDT-FILE-STATUS NOT = '00'
062017        DISPLAY ' ERENDT CLOSE ' ERENDT-FILE-STATUS
062017        PERFORM ABEND-PGM
062017     END-IF.

           IF NSAS-FILE-STATUS NOT = '00'
              DISPLAY ' NSASEXTR CLOSE ' NSAS-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCNTL-FILE-STATUS NOT = '00'
              DISPLAY ' ELCNTL CLOSE ' ELCNTL-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELLETR-FILE-STATUS NOT = '00'
              DISPLAY ' ELLETR CLOSE ' ELLETR-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ELCERT-FILE-STATUS NOT = '00' 
              MOVE ' ELCERT CLOSE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCERT-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.

           IF ERPNDB-FILE-STATUS NOT = '00' 
              MOVE ' ERPNDB CLOSE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ERPNDB-FILE-STATUS    TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF.
103012
103012     IF ELMSTR-FILE-STATUS NOT = '00' AND '97'
103012        MOVE ' ELMSTR CLOSE ERROR ' TO WS-ABEND-MESSAGE
103012        MOVE ELMSTR-FILE-STATUS    TO WS-ABEND-FILE-STATUS
103012        PERFORM ABEND-PGM
103012     END-IF.
100913
100913     IF ERNOTE-FILE-STATUS NOT = '00'
100913        DISPLAY ' ERNOTE CLOSE ' ERNOTE-FILE-STATUS
100913        PERFORM ABEND-PGM
100913     END-IF.
100913
100913     IF ELEOBC-FILE-STATUS NOT = '00'
100913        DISPLAY ' ELEOBC CLOSE ' ELEOBC-FILE-STATUS
100913        PERFORM ABEND-PGM
100913     END-IF.

           
       0060-EXIT.
           EXIT.


       0125-READ-NSASEXTR.

           READ NSASEXTR INTO EXT-REC.

           IF NSAS-FILE-STATUS NOT = '00'
              DISPLAY 'BAD READ NSASEXTR ' NSAS-ARCHIVE-NO '-'
                  NSAS-FILE-STATUS
           END-IF.

       0125-EXIT.
           EXIT.


       0200-CHECK-CERT-STATUS.
      
           MOVE +0                     TO WS-COV-CNT
           MOVE LOW-VALUES             TO WS-STOP-DATE
           PERFORM 0250-READ-CERT THRU 0250-EXIT
103012     IF NOT WS-CERT-FOUND
103012        MOVE WS-CURRENT-BIN-DT   TO WS-STOP-DATE
103012        MOVE WS-NOT-FOUND        TO WS-COMMENT-LINE
103012        DISPLAY ' ERROR READING ELCERT ' LA-CERT-NO-A2
103012        GO TO 0200-EXIT
103012     END-IF.
103012
103012     IF CM-LF-BENEFIT-CD NOT = '00' AND SPACES
103012         ADD +1                TO WS-COV-CNT
103012     END-IF
103012     IF CM-AH-BENEFIT-CD NOT = '00' AND SPACES
103012         ADD +1                TO WS-COV-CNT
103012     END-IF
103012     
103012     IF (CERT-PEND-ISSUE-ERROR)
103012                  AND
103012        ((CERT-CANCELLED-ONLINE)
103012        OR (CERT-PEND-CANCEL-ERROR))
103012        MOVE CM-CONTROL-PRIMARY TO PB-CONTROL-BY-ACCOUNT
103012        MOVE +0               TO PB-ALT-CHG-SEQ-NO
103012        MOVE '2'              TO PB-RECORD-TYPE
103012        READ ERPNDB
103012        IF ERPNDB-FILE-STATUS = '00'
103012           IF PB-C-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
103012              SUBTRACT +1     FROM WS-COV-CNT
103012*              MOVE PB-C-LF-CANCEL-DT TO WS-STOP-DATE
103012              MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012              MOVE WS-CANCELLED      TO WS-COMMENT-LINE
103012           ELSE
103012              IF CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES
103012                 MOVE +0      TO WS-COV-CNT
103012*                 MOVE CM-LF-DEATH-DT TO WS-STOP-DATE
103012                 MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012                 MOVE WS-DEATH       TO WS-COMMENT-LINE
103012              ELSE
103012                 IF (CM-LF-LOAN-EXPIRE-DT NOT = 
103012                    LOW-VALUES AND SPACES)
103012                    AND (CM-LF-LOAN-EXPIRE-DT < BIN-RUN-DATE)
103012                      SUBTRACT +1 FROM WS-COV-CNT
103012*                      MOVE CM-LF-LOAN-EXPIRE-DT TO WS-STOP-DATE
103012                      MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012                      MOVE WS-EXPIRED        TO WS-COMMENT-LINE
103012                 END-IF
103012              END-IF
103012           END-IF
103012           IF PB-C-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
103012              SUBTRACT +1     FROM WS-COV-CNT
103012*              MOVE PB-C-AH-CANCEL-DT TO WS-STOP-DATE
103012              MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012              MOVE WS-CANCELLED      TO WS-COMMENT-LINE
103012           ELSE
103012              IF (CM-AH-SETTLEMENT-DT NOT =
103012                 LOW-VALUES AND SPACES)
103012                 SUBTRACT +1        FROM WS-COV-CNT
103012*                 MOVE CM-AH-SETTLEMENT-DT TO WS-STOP-DATE
103012                 MOVE WS-CURRENT-BIN-DT   TO WS-STOP-DATE
103012                 MOVE WS-SETTLEMENT       TO WS-COMMENT-LINE
103012              ELSE
103012                 IF (CM-AH-LOAN-EXPIRE-DT NOT =
103012                    LOW-VALUES AND SPACES)
103012                    AND (CM-AH-LOAN-EXPIRE-DT < BIN-RUN-DATE)
103012                    SUBTRACT +1 FROM WS-COV-CNT
103012*                    MOVE CM-AH-LOAN-EXPIRE-DT TO WS-STOP-DATE
103012                    MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012                    MOVE WS-EXPIRED        TO WS-COMMENT-LINE
103012                 END-IF
103012              END-IF
103012           END-IF
103012        ELSE
103012           DISPLAY ' BAD READ ON ERPNDB ' CM-STATE ' '
103012              CM-ACCOUNT ' ' CM-CERT-NO
103012        END-IF
103012     ELSE
103012        IF CM-LF-CANCEL-DT NOT = LOW-VALUES AND SPACES
103012           SUBTRACT +1        FROM WS-COV-CNT
103012*           MOVE CM-LF-CANCEL-DT TO WS-STOP-DATE
103012           MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012           MOVE WS-CANCELLED    TO WS-COMMENT-LINE
103012        ELSE
103012           IF CM-LF-DEATH-DT NOT = LOW-VALUES AND SPACES
103012              MOVE +0         TO WS-COV-CNT
103012*              MOVE CM-LF-DEATH-DT TO WS-STOP-DATE
103012              MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012              MOVE WS-DEATH       TO WS-COMMENT-LINE
103012           ELSE
103012              IF (CM-LF-LOAN-EXPIRE-DT NOT =
103012                 LOW-VALUES AND SPACES)
103012                 AND (CM-LF-LOAN-EXPIRE-DT < BIN-RUN-DATE)
103012                 SUBTRACT +1 FROM WS-COV-CNT
103012*                 MOVE CM-LF-LOAN-EXPIRE-DT TO WS-STOP-DATE
103012                 MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012                 MOVE WS-EXPIRED        TO WS-COMMENT-LINE
103012              END-IF
103012           END-IF
103012        END-IF
103012        IF CM-AH-CANCEL-DT NOT = LOW-VALUES AND SPACES
103012           SUBTRACT +1        FROM WS-COV-CNT
103012*           MOVE CM-AH-CANCEL-DT TO WS-STOP-DATE
103012           MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012           MOVE WS-CANCELLED    TO WS-COMMENT-LINE
103012        ELSE
103012           IF CM-AH-SETTLEMENT-DT NOT = LOW-VALUES AND SPACES
103012              SUBTRACT +1        FROM WS-COV-CNT
103012*              MOVE CM-AH-SETTLEMENT-DT TO WS-STOP-DATE
103012              MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012              MOVE WS-SETTLEMENT       TO WS-COMMENT-LINE
103012           ELSE
103012              IF (CM-AH-LOAN-EXPIRE-DT NOT =
103012                 LOW-VALUES AND SPACES)
103012                 AND (CM-AH-LOAN-EXPIRE-DT < BIN-RUN-DATE)
103012                 SUBTRACT +1 FROM WS-COV-CNT
103012*                 MOVE CM-AH-LOAN-EXPIRE-DT TO WS-STOP-DATE
103012                 MOVE WS-CURRENT-BIN-DT TO WS-STOP-DATE
103012                 MOVE WS-EXPIRED        TO WS-COMMENT-LINE
103012              END-IF
103012           END-IF
103012        END-IF
103012     END-IF
103012     
103012     IF WS-COV-CNT <= +0
103012        SET CERT-CANCELLED    TO TRUE
103012        GO TO 0200-EXIT
103012     END-IF
103012     
103012     MOVE SPACES TO WS-CERT-CLAIM-SW
103012     IF CM-CLAIM-ATTACHED-COUNT > 0
103012        PERFORM 0700-CHECK-FOR-OPEN-CLMS THRU 0700-EXIT
103012        IF CERT-HAS-OPEN-CLM
103012           MOVE WS-CURRENT-BIN-DT   TO WS-STOP-DATE
103012           MOVE WS-OPEN-CLAIM       TO WS-COMMENT-LINE
100713           GO TO 0200-EXIT
103012        END-IF
100713     END-IF.
100713     
100713     PERFORM 0750-GET-Z-RECORD THRU 0750-EXIT
100713     IF BYPASS-REC
100713        MOVE WS-CURRENT-BIN-DT   TO WS-STOP-DATE
100713        MOVE WS-Z-RECORD         TO WS-COMMENT-LINE
100713        GO TO 0200-EXIT
100713     END-IF.
100713
100713     .
       0200-EXIT.
           EXIT.

062017 0230-build-ref-int-table.
062017
062017     MOVE DTE-CLIENT             TO CF-COMPANY-ID
062017     MOVE '3'                    TO CF-RECORD-TYPE
062017     MOVE low-values             TO CF-ACCESS-of-state
062017     MOVE +0                     TO CF-SEQUENCE-NO
062017
062017     start ELCNTL key >= cf-control-primary
062017     if elcntl-file-status not = '00'
062017        display 'ERROR-ELCNTL-START ' ELCNTL-FILE-STATUS
062017        PERFORM ABEND-PGM
062017     END-IF
062017
062017     read elcntl next record
062017     move +1                     to i1
062017
062017     perform until
062017        (elcntl-file-status <> '00')
062017        or (cf-record-type <> '3')
062017        if cf-st-calc-interest not numeric
062017           move +0               to cf-st-calc-interest
062017        end-if
062017        if cf-st-calc-interest <> zeros
062017           move cf-state-abbreviation
062017                                 to ws-state-abbr (i1)
062017           move cf-st-calc-interest
062017                                 to ws-state-ref-int-rate (i1)
062017           display ' found state with int ' cf-state-code ' '
062017              cf-st-calc-interest
062017           add +1 to i1
062017        end-if
062017        read elcntl next record
062017     end-perform
062017
062017     display ' load of ref int table complete ' i1
062017
062017     .
062017 0230-EXIT.
062017     EXIT.

       0250-READ-CERT.

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
              DISPLAY ' ERROR ON ELCERT - READ - 0250 '
                 ELCERT-FILE-STATUS
           END-IF.

       0250-EXIT.
           EXIT.

       0500-GET-ARCH-NO.

           MOVE DTE-CLIENT             TO CF-COMPANY-ID
           MOVE '1'                    TO CF-RECORD-TYPE
           MOVE ZEROS                  TO CF-SEQUENCE-NO
           MOVE SPACES                 TO CF-ACCESS-CD-GENL

           READ ELCNTL

           IF ELCNTL-FILE-STATUS NOT = '00'
               DISPLAY 'BAD READ ELCNTL ' ELCNTL-FILE-STATUS
               MOVE ' ELCNTL READ  ERROR ' TO WS-ABEND-MESSAGE
               MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS
               PERFORM ABEND-PGM
           END-IF
           
           ADD +1                        TO CF-CREDIT-LAST-ARCH-NUM
           MOVE CF-CREDIT-LAST-ARCH-NUM  TO WS-ARCHIVE-NO

           REWRITE CONTROL-FILE
           IF ELCNTL-FILE-STATUS NOT = '00'
              MOVE ' ELCNTL REWRITE ERROR ' TO WS-ABEND-MESSAGE
              MOVE ELCNTL-FILE-STATUS TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM
           END-IF

          .
       0500-EXIT.
           EXIT.
103012
103012
103012 0700-CHECK-FOR-OPEN-CLMS.                                        
103012
103012     MOVE CM-CONTROL-BY-CERT-NO TO CL-CONTROL-BY-CERT-NO.
103012
103012     START ELMSTR KEY NOT LESS THAN CL-CONTROL-BY-CERT-NO.
103012
103012     IF ELMSTR-FILE-STATUS NOT EQUAL '00' AND '02'
103012         GO TO 0700-EXIT
103012     END-IF.
103012
103012 0700-NEXT-CLAIM.
103012
103012     READ ELMSTR NEXT
103012
103012     IF ELMSTR-FILE-STATUS NOT = '00' AND '02'                
103012         GO TO 0700-EXIT
103012     END-IF
103012
103012     IF CM-COMPANY-CD-A4 NOT = CL-COMPANY-CD-A4  OR                   
103012        CM-CERT-NO-A4 NOT = CL-CERT-NO-A4                          
103012         GO TO 0700-EXIT
103012     END-IF
103012
103012     IF CM-CARRIER  NOT = CL-CARRIER       OR
103012        CM-GROUPING NOT = CL-CERT-GROUPING OR
103012        CM-STATE    NOT = CL-CERT-STATE    OR
103012        CM-ACCOUNT  NOT = CL-CERT-ACCOUNT  OR
103012        CM-CERT-EFF-DT NOT = CL-CERT-EFF-DT
103012         GO TO 0700-NEXT-CLAIM
103012     END-IF
103012
103012    IF CLAIM-IS-OPEN
103012        SET CERT-HAS-OPEN-CLM TO TRUE
103012        GO TO 0700-EXIT
103012    END-IF
103012
103012    GO TO 0700-NEXT-CLAIM.
103012
103012    .
103012 0700-EXIT.
103012     EXIT.
103012
           
       0750-GET-Z-RECORD.

           MOVE LOW-VALUES             TO TX-CONTROL-PRIMARY.
           MOVE DTE-CLASIC-COMPANY-CD  TO TX-COMPANY-CD.
100713     MOVE LA-RESEND-LETR         TO TX-LETTER-NO.
           
           START ELLETR KEY IS NOT LESS THAN TX-CONTROL-PRIMARY.

           IF ELLETR-FILE-STATUS NOT = '00'
100713        DISPLAY 'ERROR ON START ELLETR'
100713                  '  BYPASSING RECORD '
100713        GO TO 0750-SET-BYPASS
100713*       MOVE ' ELLETR START ERROR ' TO WS-ABEND-MESSAGE
100713*       MOVE ELLETR-FILE-STATUS TO WS-ABEND-FILE-STATUS
100713*       PERFORM ABEND-PGM
           END-IF.
           
       0750-READ-NEXT.
       
           READ ELLETR NEXT RECORD.

           IF ELLETR-FILE-STATUS NOT = '00'
100713        DISPLAY 'ERROR READING ELLETR NEXT RECORD'
100713                          '  BYPASSING RECORD  '
100713        GO TO 0750-SET-BYPASS
100713*       MOVE ' ELLETR READ  ERROR ' TO WS-ABEND-MESSAGE
100713*       MOVE ELLETR-FILE-STATUS TO WS-ABEND-FILE-STATUS
100713*       PERFORM ABEND-PGM
           END-IF.

           IF (TX-COMPANY-CD NOT = DTE-CLASIC-COMPANY-CD) OR
100713        (TX-LETTER-NO NOT = LA-RESEND-LETR)
                 DISPLAY 'LETTER Z RECORD NOT FOUND  - ' 
                                LA-RESEND-LETR
100713                          '  BYPASSING RECORD  '
100713           GO TO 0750-SET-BYPASS
100713*          MOVE ' Z RECORD NOT FOUND ' TO WS-ABEND-MESSAGE
100713*          PERFORM ABEND-PGM
           END-IF.

           IF TX-LINE-SQUEEZE-CONTROL NOT EQUAL 'Z'
               GO TO 0750-READ-NEXT
           END-IF.
           
           INITIALIZE W-Z-CONTROL-DATA
           MOVE LOW-VALUES             TO WS-RESEND-DATE
                                       WS-FOLLOW-UP-DATE.
           PERFORM 0800-PROCESS-Z-CONTROLS THRU 0800-EXIT.
100713     GO TO 0750-EXIT.
100713
100713 0750-SET-BYPASS.
100713
100713       SET BYPASS-REC TO TRUE.


       0750-EXIT.
           EXIT.
           
           
       0800-PROCESS-Z-CONTROLS.

           MOVE TX-TEXT-LINE           TO W-Z-CONTROL-DATA
           
           IF W-NUMBER-OF-COPIES NOT NUMERIC
              MOVE 1               TO W-NUMBER-OF-COPIES
           END-IF

           IF (W-DAYS-TO-RESEND NUMERIC) AND
              (W-DAYS-TO-RESEND > ZEROS)
              MOVE '6'              TO DC-OPTION-CODE
123113        MOVE WS-NEXT-BUS-BIN-DT TO DC-BIN-DATE-1
              MOVE ZEROS            TO DC-ELAPSED-MONTHS
              MOVE W-DAYS-TO-RESEND
                                    TO DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                    THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2 TO WS-RESEND-DATE
              ELSE
                 DISPLAY ' RESEND DATE ERROR '
              END-IF
           END-IF

           IF (W-DAYS-TO-FOLLOW-UP NUMERIC) AND
              (W-DAYS-TO-FOLLOW-UP >= ZEROS)
              MOVE '6'              TO DC-OPTION-CODE
              MOVE WS-CURRENT-BIN-DT TO DC-BIN-DATE-1
              MOVE ZEROS            TO DC-ELAPSED-MONTHS
              MOVE W-DAYS-TO-FOLLOW-UP
                                    TO DC-ELAPSED-DAYS
              PERFORM 8510-DATE-CONVERSION
                                    THRU 8590-EXIT
              IF NO-CONVERSION-ERROR
                 MOVE DC-BIN-DATE-2 TO WS-FOLLOW-UP-DATE
              ELSE
                 DISPLAY 'FOLLOW UP DATE ERROR'
              END-IF
           END-IF

           .
       0800-EXIT.
           EXIT.


       0900-READ-ERARCT.
       
           MOVE LA-COMPANY-CD    TO LT-COMPANY-CD
           MOVE LA-ARCHIVE-NO    TO LT-ARCHIVE-NO
           MOVE '3'              TO LT-RECORD-TYPE
           MOVE +0               TO LT-LINE-SEQ-NO
           
           READ ERARCT
           IF ERARCT-FILE-STATUS = '00'
               MOVE 'N' TO WS-ADD-ARCT
               SET LC-NDX TO WS-ZEROS
               SET WS-COMMENT-INDEX TO WS-ZEROS
               PERFORM 20 TIMES
                  SET LC-NDX UP BY +1
                  IF LT-COMMENT-LINE (LC-NDX) > SPACES
                     SET WS-COMMENT-INDEX TO LC-NDX
                  ELSE
                     EXIT PERFORM
                  END-IF
               END-PERFORM
               GO TO 0900-EXIT
           END-IF
           
           MOVE 'Y'              TO WS-ADD-ARCT.
           MOVE LOW-VALUES       TO LETTER-ARCHIVE-TEXT.
           MOVE 'LT'             TO LT-RECORD-ID.
           MOVE LA-COMPANY-CD    TO LT-COMPANY-CD.
           MOVE LA-ARCHIVE-NO    TO LT-ARCHIVE-NO.
           MOVE '3'              TO LT-RECORD-TYPE.
           MOVE +0               TO LT-LINE-SEQ-NO.
           MOVE +0               TO LT-NUM-LINES-ON-RECORD.
           SET WS-COMMENT-INDEX  TO WS-ZEROS.

       0900-EXIT.
           EXIT.
           

       1000-BUILD-ERARCH.
      
           MOVE 'LA'                   TO LETTER-ARCHIVE

           MOVE WS-ARCHIVE-NO          TO LA-ARCHIVE-NO
                                          LA-ARCHIVE-NO-A2
                                          LA-ARCHIVE-NO-A3
                                          LA-ARCHIVE-NO-A4
                                          LA-ARCHIVE-NO-A5
                                          LA-ARCHIVE-NO-A6

           MOVE DTE-CLASIC-COMPANY-CD  TO LA-COMPANY-CD
                                          LA-COMPANY-CD-A2
                                          LA-COMPANY-CD-A3
                                          LA-COMPANY-CD-A4
                                          LA-COMPANY-CD-A5
                                          LA-COMPANY-CD-A6
           MOVE WRK-ORIG-CARRIER       TO LA-CARRIER-A2
                                          LA-CARRIER-A3
                                          LA-CARRIER-A4
                                          LA-CARRIER-A5
           MOVE WRK-ORIG-GROUPING      TO LA-GROUPING-A2
                                          LA-GROUPING-A3
                                          LA-GROUPING-A4
                                          LA-GROUPING-A5
           MOVE WRK-ORIG-ACCOUNT       TO LA-ACCOUNT-A2
                                          LA-ACCOUNT-A3
                                          LA-ACCOUNT-A4
                                          LA-ACCOUNT-A5
           MOVE WRK-ORIG-STATE         TO LA-STATE-A2
                                          LA-STATE-A3
                                          LA-STATE-A4
                                          LA-STATE-A5
           MOVE WRK-ORIG-EFFECT-DATE   TO LA-EFFECT-DATE-A2
           MOVE WRK-ORIG-CERT-NO       TO LA-CERT-NO-A2
           MOVE WRK-ORIG-ENTRY         TO LA-ENTRY-A6

           MOVE WRK-ORIG-PROCESSOR-CD  TO LA-PROCESSOR-CD
      
           MOVE LOW-VALUES             TO LA-LAST-RESENT-PRINT-DATE
                                          LA-INITIAL-PRINT-DATE
                                          LA-SENT-DATE
                                          LA-REPLY-DATE
                                          LA-PURGED-DATE
                                          LA-VOIDED-DATE

           MOVE 'A'                    TO LA-STATUS
           MOVE W-NUMBER-OF-COPIES     TO LA-NO-OF-COPIES
           MOVE WRK-ORIG-RESEND-LETR   TO LA-FORM-A3
           MOVE 'B'                    TO LA-ARCHIVE-STATUS
           MOVE WS-CURRENT-BIN-DT      TO LA-CREATION-DATE
           MOVE WS-RESEND-DATE         TO LA-RESEND-DATE
           MOVE WS-FOLLOW-UP-DATE      TO LA-FOLLOW-UP-DATE
           MOVE W-FORM-TO-RESEND       TO LA-RESEND-LETR
           MOVE W-AUTO-CLOSE-IND       TO LA-FINAL-ACT-IND

           MOVE ZEROS                  TO LA-NUMBER-LABEL-LINES
                                          LA-NO-OF-TEXT-RECORDS
                                          
           MOVE 'SYST'                 TO LA-LAST-UPDATED-BY
           MOVE WS-CURRENT-BIN-DT      TO LA-LAST-MAINT-DATE  
           MOVE WS-CURRENT-TIME        TO LA-LAST-MAINT-TIME                                        

           WRITE LETTER-ARCHIVE 
       
           IF ERARCH-FILE-STATUS = '22'
              DISPLAY ' ERARCH DUP RECORD  ' LA-ARCHIVE-NO
           ELSE
              IF ERARCH-FILE-STATUS NOT = '00'
                 MOVE ' ERARCH  WRITE  ERROR ' TO WS-ABEND-MESSAGE
                 MOVE ERARCH-FILE-STATUS TO WS-ABEND-FILE-STATUS
                 PERFORM ABEND-PGM
              END-IF
           END-IF

100913     PERFORM 1440-ADD-BILLING-NOTE THRU 1440-EXIT.

           .
       1000-EXIT.
           EXIT.

062017 1100-extract-vars.
062017
062017***  Total interest field is field #230.
062017
062017*    if (cm-state = 'TN')
062017*       and (w-auto-close-ind = 'B' OR 'C')
062017*       display ' found Final action of b or c ' ext-letter-id
062017*    else
062017*       go to 1100-exit
062017*    end-if
062017
062017     unstring EXT-LETTER-VARIABLES
062017        delimited by '~' into
062017        fld-001    
062017        fld-002    
062017        fld-003    
062017        fld-004    
062017        fld-005    
062017        fld-006    
062017        fld-007    
062017        fld-008    
062017        fld-009    
062017        fld-010    
062017        fld-011    
062017        fld-012    
062017        fld-013    
062017        fld-014    
062017        fld-015    
062017        fld-016    
062017        fld-017    
062017        fld-018    
062017        fld-019    
062017        fld-020    
062017        fld-021    
062017        fld-022    
062017        fld-023    
062017        fld-024    
062017        fld-025    
062017        fld-026    
062017        fld-027    
062017        fld-028    
062017        fld-029    
062017        fld-030    
062017        fld-031    
062017        fld-032    
062017        fld-033    
062017        fld-034    
062017        fld-035    
062017        fld-036    
062017        fld-037    
062017        fld-038    
062017        fld-039    
062017        fld-040    
062017        fld-041    
062017        fld-042    
062017        fld-043    
062017        fld-044    
062017        fld-045    
062017        fld-046    
062017        fld-047    
062017        fld-048    
062017        fld-049    
062017        fld-050    
062017        fld-051    
062017        fld-052    
062017        fld-053    
062017        fld-054    
062017        fld-055    
062017        fld-056    
062017        fld-057    
062017        fld-058    
062017        fld-059    
062017        fld-060    
062017        fld-061    
062017        fld-062    
062017        fld-063    
062017        fld-064    
062017        fld-065    
062017        fld-066    
062017        fld-067    
062017        fld-068    
062017        fld-069    
062017        fld-070    
062017        fld-071    
062017        fld-072    
062017        fld-073    
062017        fld-074    
062017        fld-075    
062017        fld-076    
062017        fld-077    
062017        fld-078    
062017        fld-079    
062017        fld-080    
062017        fld-081    
062017        fld-082    
062017        fld-083    
062017        fld-084    
062017        fld-085    
062017        fld-086    
062017        fld-087    
062017        fld-088    
062017        fld-089    
062017        fld-090    
062017        fld-091    
062017        fld-092    
062017        fld-093    
062017        fld-094    
062017        fld-095    
062017        fld-096    
062017        fld-097    
062017        fld-098    
062017        fld-099    
062017        fld-100    
062017        fld-101    
062017        fld-102    
062017        fld-103    
062017        fld-104    
062017        fld-105    
062017        fld-106    
062017        fld-107    
062017        fld-108    
062017        fld-109    
062017        fld-110    
062017        fld-111    
062017        fld-112    
062017        fld-113    
062017        fld-114    
062017        fld-115    
062017        fld-116    
062017        fld-117    
062017        fld-118    
062017        fld-119    
062017        fld-120    
062017        fld-121    
062017        fld-122    
062017        fld-123    
062017        fld-124    
062017        fld-125    
062017        fld-126    
062017        fld-127    
062017        fld-128    
062017        fld-129    
062017        fld-130    
062017        fld-131    
062017        fld-132    
062017        fld-133    
062017        fld-134    
062017        fld-135    
062017        fld-136    
062017        fld-137    
062017        fld-138    
062017        fld-139    
062017        fld-140    
062017        fld-141    
062017        fld-142    
062017        fld-143    
062017        fld-144    
062017        fld-145    
062017        fld-146    
062017        fld-147    
062017        fld-148    
062017        fld-149    
062017        fld-150    
062017        fld-151    
062017        fld-152    
062017        fld-153    
062017        fld-154    
062017        fld-155    
062017        fld-156    
062017        fld-157    
062017        fld-158    
062017        fld-159    
062017        fld-160    
062017        fld-161    
062017        fld-162    
062017        fld-163    
062017        fld-164    
062017        fld-165    
062017        fld-166    
062017        fld-167    
062017        fld-168    
062017        fld-169    
062017        fld-170    
062017        fld-171    
062017        fld-172    
062017        fld-173    
062017        fld-174    
062017        fld-175    
062017        fld-176    
062017        fld-177    
062017        fld-178    
062017        fld-179    
062017        fld-180    
062017        fld-181    
062017        fld-182    
062017        fld-183    
062017        fld-184    
062017        fld-185    
062017        fld-186    
062017        fld-187    
062017        fld-188    
062017        fld-189    
062017        fld-190    
062017        fld-191    
062017        fld-192    
062017        fld-193    
062017        fld-194    
062017        fld-195    
062017        fld-196    
062017        fld-197    
062017        fld-198    
062017        fld-199    
062017        fld-200    
062017        fld-201    
062017        fld-202    
062017        fld-203    
062017        fld-204    
062017        fld-205    
062017        fld-206    
062017        fld-207    
062017        fld-208    
062017        fld-209    
062017        fld-210    
062017        fld-211    
062017        fld-212    
062017        fld-213    
062017        fld-214    
062017        fld-215    
062017        fld-216    
062017        fld-217    
062017        fld-218    
062017        fld-219    
062017        fld-220    
062017        fld-221    
062017        fld-222    
062017        fld-223    
062017        fld-224    
062017        fld-225    
062017        fld-226    
062017        fld-227    
062017        fld-228    
062017        fld-229    
062017        fld-230     *>  Interest
062017        fld-231    
062017        fld-232    
062017        fld-233    
062017        fld-234    
062017        fld-235    
062017        fld-236    
062017        fld-237    
062017        fld-238    
062017        fld-239    
062017        fld-240    
062017        fld-241    
062017        fld-242    
062017        fld-243    
062017        fld-244    
062017        fld-245    
062017        fld-246    
062017        fld-247    
062017        fld-248    
062017        fld-249    
062017        fld-250    
062017        fld-251    
062017        fld-252    
062017        fld-253    
062017        fld-254    
062017        fld-255    
062017        fld-256    
062017        fld-257    
062017        fld-258    
062017        fld-259    
062017        fld-260    
062017        fld-261    
062017        fld-262    
062017        fld-263    
062017        fld-264    
062017        fld-265    
062017        fld-266    
062017        fld-267    
062017        fld-268    
062017        fld-269    
062017        fld-270    
062017        fld-271    
062017        fld-272    
062017        fld-273    
062017        fld-274    
062017        fld-275    
062017        fld-276    
062017        fld-277    
062017        fld-278    
062017        fld-279    
062017        fld-280    
062017        fld-281    
062017        fld-282    
062017        fld-283    
062017        fld-284    
062017        fld-285    
062017        fld-286    
062017        fld-287    
062017        fld-288    
062017        fld-289    
062017        fld-290    
062017        fld-291    
062017        fld-292    
062017        fld-293    
062017        fld-294    
062017        fld-295
062017        fld-296    *> next bus date??    
062017        fld-297    
062017        fld-298    
062017        fld-299    
062017        fld-300    
062017        fld-301    
062017        fld-302    
062017        fld-303    
062017        fld-304    
062017        fld-305    
062017        fld-306    
062017        fld-307    
062017        fld-308    
062017        fld-309    
062017        fld-310    
062017        fld-311    
062017        fld-312    
062017        fld-313    
062017        fld-314    
062017        fld-315    
062017        fld-316    
062017        fld-317    
062017        fld-318    
062017        fld-319    
062017        fld-320    
062017        fld-321    
062017        fld-322    
062017        fld-323    
062017        fld-324    
062017        fld-325    
062017        fld-326    
062017        fld-327    
062017        fld-328    
062017        fld-329    
062017        fld-330    
062017        fld-331    
062017        fld-332    
062017        fld-333    
062017        fld-334    
062017        fld-335    
062017        fld-336    
062017        fld-337    
062017        fld-338    
062017        fld-339    
062017        fld-340    
062017        fld-341    
062017        fld-342    
062017        fld-343    
062017        fld-344    
062017        fld-345    
062017        fld-346    
062017        fld-347    
062017        fld-348    
062017        fld-349    
062017        fld-350    
062017        fld-351    
062017        fld-352    
062017        fld-353    
062017        fld-354    
062017        fld-355    
062017        fld-356    
062017        fld-357    
062017        fld-358    
062017        fld-359    
062017        fld-360    
062017        fld-361    
062017        fld-362    
062017        fld-363    
062017        fld-364    
062017        fld-365    
062017        fld-366    
062017        fld-367    
062017        fld-368    
062017        fld-369    
062017        fld-370    
062017        fld-371    
062017        fld-372    
062017        fld-373    
062017        fld-374    
062017        fld-375    
062017        fld-376    
062017        fld-377    
062017        fld-378    
062017        fld-379    
062017        fld-380    
062017        fld-381    
062017        fld-382    
062017        fld-383    
062017        fld-384    
062017        fld-385    
062017        fld-386    
062017        fld-387    
062017        fld-388    
062017        fld-389    
062017        fld-390    
062017        fld-391    
062017        fld-392    
062017        fld-393    
062017        fld-394    
062017        fld-395    
062017        fld-396    
062017        fld-397    
062017        fld-398    
062017        fld-399    
062017        fld-400
062017     end-unstring
062017
072619*    perform varying v1 from +1 by +1 until v1 > +400
072619*       display ' fld' v1 ' ' ws-variable-value (v1)
072619*    end-perform

062017     move cm-cert-eff-dt         to dc-bin-date-1
062017     move ' '                    to dc-option-code
062017     perform 8510-date-conversion
062017                                 thru 8590-exit
062017     if no-conversion-error
062017        move dc-greg-date-1-edit to ws-dis-eff-dt
062017     else
062017        move '  /  /  '          to ws-dis-eff-dt
062017     end-if
062017     move cm-cert-eff-dt         to dc-bin-date-1
061818     move ws-val-bin-dt          to dc-bin-date-2
062017     move '1'                    to dc-option-code
062017     perform 8510-date-conversion
062017                                 thru 8590-exit
062017
062017     if cm-state = ws-state-abbr (i1)
062017        continue
062017     else
062017        perform varying i1 from +1 by +1 until
062017           (i1 > +70)
062017           or (ws-state-abbr (i1) = cm-state)
062017           or (ws-state-abbr (i1) = spaces)
062017        end-perform
062017        if (i1 > +70)
062017           or (ws-state-ref-int-rate (i1) = zeros)
062017           or (ws-state-abbr (i1) = spaces)
062017           DISPLAY ' REF BYPASSED, NO INTEREST '
062017              cm-STATE ' ' cm-ACCOUNT ' ' cm-CERT-NO
062017           GO TO 1100-exit
062017        end-if
062017     end-if
062017
062017***  Before we compute the ws-ref-amt we need to see what the
062017***  original premium was, may have to calc it based on that.
062017
062017     move cm-lf-premium-amt      to ws-lf-orig-prm-amt
062017     move cm-lf-alt-premium-amt  to ws-lf-orig-alt-prm-amt
062017     move cm-ah-premium-amt      to ws-ah-orig-prm-amt
062017
062017     move cm-control-primary     to en-control-primary
062017     move 'I'                    to en-rec-type
062017     move +0                     to en-seq-no
062017     start ERENDT key >= en-control-primary

062017     if erendt-file-status <> '00'
062017        go to 1100-continue
062017     end-if

062017     move ' '                    to ws-done-sw-endt
062017     perform until i-am-done
062017        read ERENDT next record
062017        display ' after erendt read ' erendt-file-status ' '
062017           cm-cert-no
062017        if (erendt-file-status = '00')
062017           and (cm-control-primary = en-control-primary(1:33))
062017           and (en-rec-type = 'I')

062017           move en-lf-orig-prm-amt  to ws-lf-orig-prm-amt
062017           move en-lf-orig-alt-prm-amt
062017                                    to ws-lf-orig-alt-prm-amt
062017           move en-ah-orig-prm-amt  to ws-ah-orig-prm-amt
062017        else
062017           set i-am-done to true
062017        end-if
062017     end-perform
062017
062017     .
062017 1100-continue.
062017
062017     compute ws-ref-amt =
062017        ws-lf-orig-prm-amt + ws-lf-orig-alt-prm-amt +
062017        ws-ah-orig-prm-amt
062017
062017     COMPUTE WS-INT-AMT ROUNDED = (WS-REF-AMT * 
062017           ((1 + ws-state-ref-int-rate (i1)) **
062017              (dc-elapsed-days / 365) - 1))
062017
061818*    display ' Int calc ' cm-cert-no ' ' ws-ref-amt ' '
061818*       ws-int-amt ' ' dc-elapsed-days

062017     IF WS-INT-AMT = ZEROS
062017        DISPLAY ' REF BYPASSED, ZERO INTEREST '
062017           cm-STATE ' ' cm-ACCOUNT ' ' cm-CERT-NO
062017        GO TO 1100-exit
062017     END-IF
062017
062017     move ws-int-amt             to ws-dis-int-amt
062017
062017     move +1                     to cntr
062017     perform varying b1 from +1 by +1 until
062017        (b1 > +4350)
062017        or (cntr = +230)
062017        if ext-letter-variables (b1:1) = '~'
062017           add +1 to cntr
062017        end-if
062017     end-perform
062017
062017     if cntr = +230
062017        move ws-dis-int-amt      to ext-letter-variables (b1:10)
062017     else
062017        display ' failed to update interest amount '
062017     end-if
062017
062017     .
062017 1100-exit.
062017     exit.

100913
100913 1440-ADD-BILLING-NOTE.
100913
100913     MOVE LOW-VALUES             TO EO-CONTROL-PRIMARY
100913     MOVE DTE-CLASIC-COMPANY-CD  TO EO-COMPANY-CD
100913     MOVE '5'                    TO EO-RECORD-TYPE
100913
100913     START ELEOBC KEY IS NOT LESS THAN EO-CONTROL-PRIMARY.
100913
100913     IF ELEOBC-FILE-STATUS NOT = '00'
100913        GO TO 1440-EXIT
100913     END-IF.
100913
100913 1440-READNEXT-ELEOBC.
100913
100913     READ ELEOBC NEXT RECORD.
100913
100913     IF ELEOBC-FILE-STATUS = '00'
100913         IF EO-RECORD-TYPE NOT = '5'
100913             GO TO 1440-EXIT
100913         END-IF
100913     ELSE
100913         GO TO 1440-EXIT
100913     END-IF
100913     
100913     IF EO-RECORD-TYPE = '5' AND
100913        EO-EOB-CODE = WRK-ORIG-RESEND-LETR
100913           CONTINUE
100913     ELSE
100913         GO TO 1440-READNEXT-ELEOBC
100913     END-IF
100913     
100913     MOVE SPACES TO WS-BILLING-NOTE
100913     MOVE EO-DESCRIPTION TO WS-BN-NOTE
100913     MOVE WRK-ORIG-RESEND-LETR TO WS-BN-LTRID
100913     MOVE WS-CURRENT-DATE TO WS-BN-DATE
100913     MOVE 'SYST' TO WS-BN-USERID
100913     MOVE +25 TO WS-LEN
100913
100913     PERFORM 1441-UPDATE-BILLING-NOTE THRU 1441-EXIT.
100913
100913 1440-EXIT.
100913     EXIT.
100913
100913 1441-UPDATE-BILLING-NOTE.
100913
100913     MOVE SPACES                 TO CERTIFICATE-NOTE
100913     MOVE LA-COMPANY-CD          TO CN-COMPANY-CD
100913     MOVE LA-CARRIER-A2          TO CN-CARRIER
100913     MOVE LA-GROUPING-A2         TO CN-GROUPING
100913     MOVE LA-STATE-A2            TO CN-STATE
100913     MOVE LA-ACCOUNT-A2          TO CN-ACCOUNT
100913     MOVE LA-EFFECT-DATE-A2      TO CN-CERT-EFF-DT
100913     MOVE LA-CERT-NO-A2          TO CN-CERT-NO
101420     if W-AUTO-CLOSE-IND = 'B' or 'C'
101420        move '2'                 to cn-record-type
101420     else
101420        move '1'                 to cn-record-type
101420     end-if
100913
100913     READ ERNOTE
100913     IF ERNOTE-FILE-STATUS = '00'
100913       IF CN-BILLING-START-LINE-NO NOT NUMERIC
100913          MOVE ZEROS            TO CN-BILLING-START-LINE-NO
100913       END-IF
100913       IF CN-BILLING-END-LINE-NO NOT NUMERIC
100913          MOVE ZEROS            TO CN-BILLING-END-LINE-NO
100913       END-IF
100913       PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
100913           (NOTE-SUB > +10) OR
100913           (CN-LINE (NOTE-SUB) (1:WS-LEN) = 
100913                             WS-BILLING-NOTE (1:WS-LEN))
100913       END-PERFORM
100913       IF CN-LINE (NOTE-SUB) (1:WS-LEN) = 
100913                              WS-BILLING-NOTE (1:WS-LEN)
100913          GO TO 1441-EXIT
100913       ELSE
100913         PERFORM VARYING NOTE-SUB FROM +1 BY +1 UNTIL
100913           (NOTE-SUB > +10) OR
100913           (CN-LINE (NOTE-SUB) = SPACES OR LOW-VALUES) 
100913         END-PERFORM
100913         IF (NOTE-SUB < +11)
100913           IF NOTE-SUB >= CN-BILLING-START-LINE-NO AND
100913              NOTE-SUB <= CN-BILLING-END-LINE-NO
100913                MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
100913           ELSE 
100913             IF (CN-BILLING-END-LINE-NO NOT = ZEROS) AND
100913              (NOTE-SUB = (CN-BILLING-END-LINE-NO + +1))
100913                MOVE WS-BILLING-NOTE   TO CN-LINE (NOTE-SUB)
100913                MOVE NOTE-SUB     TO CN-BILLING-END-LINE-NO
100913             ELSE
100913               IF (CN-BILLING-START-LINE-NO NOT = ZEROS) AND
100913                  (NOTE-SUB = (CN-BILLING-START-LINE-NO - +1))
100913                     MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB)
100913                     MOVE NOTE-SUB  TO CN-BILLING-START-LINE-NO
100913               ELSE
100913                 IF (CN-BILLING-END-LINE-NO = ZEROS)
100913                   MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB)
100913                   MOVE NOTE-SUB    TO CN-BILLING-END-LINE-NO
100913                                       CN-BILLING-START-LINE-NO
100913                 ELSE
100913                    PERFORM 1442-SQUEEZE-IT-IN THRU 1442-EXIT
100913                 END-IF
100913               END-IF                          
100913             END-IF
100913           END-IF
100913           MOVE 'SYST'              TO CN-LAST-MAINT-USER
100913           MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
100913           MOVE WS-CURRENT-TIME     TO CN-LAST-MAINT-HHMMSS
100913           REWRITE CERTIFICATE-NOTE
100913           IF ERNOTE-FILE-STATUS NOT = '00'
100913              DISPLAY ' ERROR ON ERNOTE - REWRITE - BILL NOTE '
100913                 ERNOTE-FILE-STATUS  '  CERT '  LA-CERT-NO-A2
100913           END-IF
100913           PERFORM 1445-CERTIFICATE-UPDATE THRU 1445-EXIT
100913         END-IF
100913       END-IF
100913     ELSE
100913        MOVE SPACES              TO CERTIFICATE-NOTE
100913        MOVE 'CN'                TO CN-RECORD-ID
100913        MOVE LA-COMPANY-CD          TO CN-COMPANY-CD
100913        MOVE LA-CARRIER-A2          TO CN-CARRIER
100913        MOVE LA-GROUPING-A2         TO CN-GROUPING
100913        MOVE LA-STATE-A2            TO CN-STATE
100913        MOVE LA-ACCOUNT-A2          TO CN-ACCOUNT
100913        MOVE LA-EFFECT-DATE-A2      TO CN-CERT-EFF-DT
100913        MOVE LA-CERT-NO-A2          TO CN-CERT-NO
101420        if W-AUTO-CLOSE-IND = 'B' or 'C'
101420           move '2'              to cn-record-type
101420        else
101420           move '1'              to cn-record-type
101420        end-if
100913        MOVE 01                  TO CN-BILLING-START-LINE-NO
100913                                    CN-BILLING-END-LINE-NO
100913        MOVE WS-BILLING-NOTE     TO CN-LINE (01)
100913        MOVE 'SYST'              TO CN-LAST-MAINT-USER
100913        MOVE WS-CURRENT-BIN-DT   TO CN-LAST-MAINT-DT
100913        MOVE WS-CURRENT-TIME        TO CN-LAST-MAINT-HHMMSS
100913        WRITE  CERTIFICATE-NOTE
100913        IF ERNOTE-FILE-STATUS NOT = '00'
100913           DISPLAY ' ERROR ON ERNOTE - WRITE - BILL NOTE '
100913              ERNOTE-FILE-STATUS  '  CERT '  LA-CERT-NO-A2
100913        END-IF
100913
100913        PERFORM 1445-CERTIFICATE-UPDATE THRU 1445-EXIT
100913     END-IF.              
100913
100913
100913 1441-EXIT.
100913     EXIT.
100913
100913
100913 1442-SQUEEZE-IT-IN.
100913
100913     IF NOTE-SUB < CN-BILLING-START-LINE-NO
100913        PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY +1 UNTIL
100913           NOTE-SUB = +10
100913           MOVE CN-LINE (NOTE-SUB + 1) TO CN-LINE (NOTE-SUB)
100913           IF (NOTE-SUB + 1) = (CN-BILLING-START-LINE-NO - 1)
100913             MOVE WS-BILLING-NOTE TO CN-LINE (NOTE-SUB + 1)
100913             COMPUTE CN-BILLING-START-LINE-NO = NOTE-SUB + 1
100913             MOVE +9 TO NOTE-SUB
100913           END-IF
100913        END-PERFORM
100913     ELSE
100913        IF NOTE-SUB > CN-BILLING-END-LINE-NO
100913           PERFORM VARYING NOTE-SUB FROM NOTE-SUB BY -1 
100913             UNTIL NOTE-SUB = +1
100913             MOVE CN-LINE (NOTE-SUB - 1) TO CN-LINE (NOTE-SUB)
100913             IF (NOTE-SUB - 1) = (CN-BILLING-END-LINE-NO + 1)
100913                MOVE WS-BILLING-NOTE  TO CN-LINE (NOTE-SUB - 1)
100913                COMPUTE CN-BILLING-END-LINE-NO = NOTE-SUB - 1
100913                MOVE +2          TO NOTE-SUB
100913             END-IF
100913           END-PERFORM
100913        END-IF  
100913     END-IF.
100913
100913
100913 1442-EXIT.
100913     EXIT.
100913
100913 1445-CERTIFICATE-UPDATE.
100913
100913     PERFORM 0250-READ-CERT THRU 0250-EXIT
100913     IF WS-CERT-FOUND
100913        EVALUATE CM-NOTE-SW
100913           WHEN '2'
100913           WHEN '3'
100913           WHEN '6'
100913           WHEN '7'
100913              SET NO-CERT-RW     TO TRUE
100913           WHEN ' '
100913              MOVE '2'           TO CM-NOTE-SW
100913           WHEN '1'
100913              MOVE '3'           TO CM-NOTE-SW
100913           WHEN '4'
100913              MOVE '6'           TO CM-NOTE-SW
100913           WHEN '5'
100913              MOVE '7'           TO CM-NOTE-SW
100913        END-EVALUATE
100913     END-IF
100913     IF NOT NO-CERT-RW
100913        REWRITE CERTIFICATE-MASTER
100913     END-IF.
100913
100913
100913 1445-EXIT.
100913     EXIT.
100913

       8510-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8590-EXIT.
           EXIT.


       ABEND-PGM.   COPY ELCABEND.

