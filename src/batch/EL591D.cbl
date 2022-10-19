       IDENTIFICATION DIVISION.

       PROGRAM-ID.                 EL591.


      *AUTHOR.     AJRA.
      *DATE-COMPILED.
      ******************************************************************
      *REMARKS.
      *        THIS PROGRAM CREATES  REPORTS:
      *            1) CLAIMS ACTIVITY BY AUDITOR 
      *            2) DETAIL ACTIVITY BY AUDITOR
      * 
      *      
      *     INPUT:   MONTH-TO-DATE ELMSTR CREATED BY EL585
      *              ELTRLR
      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 061606    2006052500001  AJRA  INITIAL PROGRAM
031407* 031407    2006052500001  AJRA  MODIFICATIONS
032007* 032007    2006052500001  AJRA  ADD HANDLE AND SERVICE TIME
030612* 030612  CR2011120900003  AJRA  ADD AHL COMPANY CODE
052614* 052614  CR2014022100001  AJRA  ADD FAMILY LEAVE CLAIM TYPE
      ******************************************************************

       ENVIRONMENT DIVISION.

       INPUT-OUTPUT SECTION.

       FILE-CONTROL.
           SELECT DISK-DATE          ASSIGN TO SYS019-FBA1-S-SYS019.

           SELECT MTD-ELMSTR-INFILE  ASSIGN TO MTDCLMS
                                     ORGANIZATION IS LINE SEQUENTIAL.

           SELECT ELTRLR-INFILE      ASSIGN TO ELTRLR
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS AT-CONTROL-PRIMARY
                                     FILE STATUS IS
                                       WS-ELTRLR-FILE-STATUS.

           SELECT PRNTR              ASSIGN TO SYS008-UR-1403-S-SYS008.

           SELECT SORT-FILE          ASSIGN TO SYS001-UT-3380-S-SORTWK1.

           SELECT SEQ-FILE-ARCHIVE   ASSIGN TO SYS020-UT-2400-S-SYS020.

           SELECT ELREPT             ASSIGN TO SYS018-FBA1-ELREPT
                                     ORGANIZATION IS INDEXED
                                     ACCESS IS DYNAMIC
                                     RECORD KEY IS RF-CONTROL-PRIMARY
                                     FILE STATUS IS DTE-VSAM-FLAGS.

           SELECT FILE-OUT           ASSIGN TO SYS010-UT-2400-S-SYS010.

           SELECT SORT-FILE2         ASSIGN TO SYS001-UT-3380-S-SORTWK1.

           SELECT PRNTR2             ASSIGN TO SYS009-UR-1403-S-SYS009.

       DATA DIVISION.

       FILE SECTION.

       FD  DISK-DATE
                                COPY ELCDTEFD.

       FD  MTD-ELMSTR-INFILE.
                                COPY ELCMSTR.

       FD  ELTRLR-INFILE.
                                COPY ELCTRLR.


       FD  PRNTR
                                COPY ELCPRTFD.


       SD  SORT-FILE.
       01  SORT-RECORD.
           05  SORT-CLAIM-TYPE             PIC X(01).
           05  SORT-AUDITOR                PIC X(04).
           05  SORT-MTD-OR-DAILY           PIC X(01).
           05  SORT-CLAIM-NUMBER           PIC X(07).
           05  SORT-CERT-NUMBER            PIC X(11).
           05  SORT-REC-TYPE.
               10  SORT-REC-TYPE-1         PIC X(01).
               10  SORT-REC-TYPE-2         PIC X(01).
           05  SORT-RECORDED-DT            PIC X(02).    
           05  SORT-FROM-DT                PIC X(02).
           05  SORT-TO-DT                  PIC X(02).
032007     05  SORT-HANDLE-TIME            PIC S9(07)V99.
032007     05  SORT-HANDLE-COUNTED         PIC X(01).


      *********** OLD FICHE PROCESSING USED FOR SEQUENTIAL FILE ARCHIVE
      *               IF DTE-PRT-OPT = 'B'
       FD  SEQ-FILE-ARCHIVE
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  SEQ-ARCHIVE-REC                 PIC X(133).



       FD  ELREPT
                                COPY ELCRPTFD.
                                COPY ELCREPT.

       FD  FILE-OUT
           BLOCK CONTAINS 0 RECORDS
           RECORDING MODE F.

       01  FILE-RECORD                    PIC X(55).


       SD  SORT-FILE2.
       01  SORT2-RECORD.
           05  SORT2-CLAIM-TYPE             PIC X(01).
           05  SORT2-AUDITOR                PIC X(04).
           05  SORT2-MTD-OR-DAILY           PIC X(01).
           05  SORT2-CLAIM-NUMBER           PIC X(07).
           05  SORT2-CERT-NUMBER            PIC X(11).
           05  SORT2-REC-TYPE.
               10  SORT2-REC-TYPE-1         PIC X(01).
               10  SORT2-REC-TYPE-2         PIC X(01).
           05  SORT2-RECORDED-DT            PIC X(06).    
           05  SORT2-FROM-DT                PIC X(06).
           05  SORT2-TO-DT                  PIC X(06).
032007     05  SORT2-HANDLE-TIME            PIC S9(07)V99.
032007     05  SORT2-HANDLE-COUNTED         PIC X(01).


       FD  PRNTR2
           RECORDING MODE F                                             
           LABEL RECORDS OMITTED                                        
           BLOCK CONTAINS 0 RECORDS
           RECORD CONTAINS 133 CHARACTERS.                              
       01  PRT2.                                                         
           12  P2-CTL               PIC  X.                              
           12  P2-DATA              PIC  X(132).                         
      ******************************************************************

       WORKING-STORAGE SECTION.
       01  LCP-ABND-CODE                   PIC S999 COMP   VALUE +519.

       77  FILLER  PIC X(32)   VALUE '********************************'.
       77  FILLER  PIC X(32)   VALUE '*     EL591  WORKING STORAGE   *'.
       77  FILLER  PIC X(32)   VALUE '********** V/M 2.008 ***********'.

       01  FILLER                          COMP-3.
           05  WS-LINE-COUNT               PIC S9(03)    VALUE +55.
           05  WS-LINE2-COUNT              PIC S9(03)    VALUE +55.
           05  WS-LINE-COUNT-MAX           PIC S9(03)    VALUE +55.
           05  WS-PAGE                     PIC S9(05)    VALUE +0.
           05  WS-PAGE2                    PIC S9(05)    VALUE +0.
           05  WS-SORT-RET-CNT             PIC S9(05)    VALUE +0.
           05  WS-ACCEPTABLE-SVC-TIME      PIC S9(07)    VALUE +30.
           05  WS-MAX-SVC-TIME-FOR-AVG     PIC S9(07)    VALUE +61.


      ******* DAILY COUNT BUCKETS PER AUDITOR
122002     03  WS-DLY-COUNTS               COMP-3.
122002         05  WS-DLY-NEW-CLM-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-DLY-PD-CLM-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-DLY-PART-DRAFT-CNT   PIC S9(05)    VALUE +0.
               05  WS-DLY-FINAL-DRAFT-CNT  PIC S9(05)    VALUE +0.
               05  WS-DLY-ADDIT-DRAFT-CNT  PIC S9(05)    VALUE +0.
               05  WS-DLY-OTHER-DRAFT-CNT  PIC S9(05)    VALUE +0.
122002         05  WS-DLY-VOID-CNT         PIC S9(05)    VALUE +0.
122002         05  WS-DLY-STOPPAY-CNT      PIC S9(05)    VALUE +0.
               05  WS-DLY-MAIL-WO-REQ-CNT  PIC S9(05)    VALUE +0.
               05  WS-DLY-MAIL-W-REQ-CNT   PIC S9(05)    VALUE +0.
122002         05  WS-DLY-LETTER-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-DLY-CALLIN-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-DLY-CALLOUT-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-DLY-NOTE-CNT         PIC S9(05)    VALUE +0.
122002         05  WS-DLY-DENIAL-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-DLY-DROP-CNT         PIC S9(05)    VALUE +0.
122002         05  WS-DLY-REOPEN-CNT       PIC S9(05)    VALUE +0.
               05  WS-DLY-UNWS-CNT         PIC S9(05)    VALUE +0.
               05  WS-DLY-UNWR-CNT         PIC S9(05)    VALUE +0.
               05  WS-DLY-MED-REC-CNT      PIC S9(05)    VALUE +0.
               05  WS-DLY-WORK-MEASURE-CNT PIC S9(05)    VALUE +0.
               05  WS-DLY-NOTE-FILE-CNT    PIC S9(05)    VALUE +0.

      ******* MONTH-TO-DATE COUNT BUCKETS PER AUDITOR
122002     03  WS-MTD-COUNTS               COMP-3.
122002         05  WS-MTD-NEW-CLM-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-MTD-PD-CLM-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-MTD-PART-DRAFT-CNT   PIC S9(05)    VALUE +0.
               05  WS-MTD-FINAL-DRAFT-CNT  PIC S9(05)    VALUE +0.
               05  WS-MTD-ADDIT-DRAFT-CNT  PIC S9(05)    VALUE +0.
               05  WS-MTD-OTHER-DRAFT-CNT  PIC S9(05)    VALUE +0.
122002         05  WS-MTD-VOID-CNT         PIC S9(05)    VALUE +0.
122002         05  WS-MTD-STOPPAY-CNT      PIC S9(05)    VALUE +0.
               05  WS-MTD-MAIL-WO-REQ-CNT  PIC S9(05)    VALUE +0.
               05  WS-MTD-MAIL-W-REQ-CNT   PIC S9(05)    VALUE +0.
122002         05  WS-MTD-LETTER-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-MTD-CALLIN-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-MTD-CALLOUT-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-MTD-NOTE-CNT         PIC S9(05)    VALUE +0.
122002         05  WS-MTD-DENIAL-CNT       PIC S9(05)    VALUE +0.
122002         05  WS-MTD-DROP-CNT         PIC S9(05)    VALUE +0.
122002         05  WS-MTD-REOPEN-CNT       PIC S9(05)    VALUE +0.
               05  WS-MTD-UNWS-CNT         PIC S9(05)    VALUE +0.
               05  WS-MTD-UNWR-CNT         PIC S9(05)    VALUE +0.
               05  WS-MTD-MED-REC-CNT      PIC S9(05)    VALUE +0.
               05  WS-MTD-WORK-MEASURE-CNT PIC S9(05)    VALUE +0.
               05  WS-MTD-NOTE-FILE-CNT    PIC S9(05)    VALUE +0.

122002******* TOTAL BUCKETS PER CLAIM TYPE
      ******* TOTD = DAILY TOTAL   TOTM = MONTH-TO-DATE TOTAL
       01  FILLER                          COMP-3.
122002     03  WS-TOTD-COUNTS              COMP-3.
122002         05  WS-TOTD-NEW-CLM-CNT     PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-PD-CLM-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-PART-DRAFT-CNT  PIC S9(05)    VALUE +0.
               05  WS-TOTD-FINAL-DRAFT-CNT PIC S9(05)    VALUE +0.
               05  WS-TOTD-ADDIT-DRAFT-CNT PIC S9(05)    VALUE +0.
               05  WS-TOTD-OTHER-DRAFT-CNT PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-VOID-CNT        PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-STOPPAY-CNT     PIC S9(05)    VALUE +0.
               05  WS-TOTD-MAIL-WO-REQ-CNT PIC S9(05)    VALUE +0.
               05  WS-TOTD-MAIL-W-REQ-CNT  PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-LETTER-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-CALLIN-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-CALLOUT-CNT     PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-NOTE-CNT        PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-DENIAL-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-DROP-CNT        PIC S9(05)    VALUE +0.
122002         05  WS-TOTD-REOPEN-CNT      PIC S9(05)    VALUE +0.
               05  WS-TOTD-UNWS-CNT        PIC S9(05)    VALUE +0.
               05  WS-TOTD-UNWR-CNT        PIC S9(05)    VALUE +0.
               05  WS-TOTD-MED-REC-CNT     PIC S9(05)    VALUE +0.
               05  WS-TOTD-WORK-MEASURE-CNT PIC S9(05)   VALUE +0.
               05  WS-TOTD-NOTE-FILE-CNT   PIC S9(05)    VALUE +0.

122002     03  WS-TOTM-COUNTS              COMP-3.    
122002         05  WS-TOTM-NEW-CLM-CNT     PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-PD-CLM-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-PART-DRAFT-CNT  PIC S9(05)    VALUE +0.
               05  WS-TOTM-FINAL-DRAFT-CNT PIC S9(05)    VALUE +0.
               05  WS-TOTM-ADDIT-DRAFT-CNT PIC S9(05)    VALUE +0.
               05  WS-TOTM-OTHER-DRAFT-CNT PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-VOID-CNT        PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-STOPPAY-CNT     PIC S9(05)    VALUE +0.
               05  WS-TOTM-MAIL-WO-REQ-CNT PIC S9(05)    VALUE +0.
               05  WS-TOTM-MAIL-W-REQ-CNT  PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-LETTER-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-CALLIN-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-CALLOUT-CNT     PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-NOTE-CNT        PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-DENIAL-CNT      PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-DROP-CNT        PIC S9(05)    VALUE +0.
122002         05  WS-TOTM-REOPEN-CNT      PIC S9(05)    VALUE +0.
               05  WS-TOTM-UNWS-CNT        PIC S9(05)    VALUE +0.
               05  WS-TOTM-UNWR-CNT        PIC S9(05)    VALUE +0.
               05  WS-TOTM-MED-REC-CNT     PIC S9(05)    VALUE +0.
               05  WS-TOTM-WORK-MEASURE-CNT PIC S9(05)   VALUE +0.
               05  WS-TOTM-NOTE-FILE-CNT     PIC S9(05)    VALUE +0.

       01  FILLER                          COMP-3.
           05  WS-GRAND-TOT-NEW-CLM-CNT    PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-PD-CLM-CNT     PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-PART-DRAFT-CNT PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-FINAL-DRAFT-CNT PIC S9(05)   VALUE +0.
           05  WS-GRAND-TOT-ADDIT-DRAFT-CNT PIC S9(05)   VALUE +0.
           05  WS-GRAND-TOT-OTHER-DRAFT-CNT PIC S9(05)   VALUE +0.
           05  WS-GRAND-TOT-VOID-CNT       PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-STOPPAY-CNT    PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-MAIL-WO-REQ-CNT PIC S9(05)   VALUE +0.
           05  WS-GRAND-TOT-MAIL-W-REQ-CNT PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-LETTER-CNT     PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-CALLIN-CNT     PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-CALLOUT-CNT    PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-NOTE-CNT       PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-DENIAL-CNT     PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-DROP-CNT       PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-REOPEN-CNT     PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-UNWS-CNT       PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-UNWR-CNT       PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-MED-REC-CNT    PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-WORK-MEASURE   PIC S9(05)    VALUE +0.
           05  WS-GRAND-TOT-NOTE-FILE-CNT  PIC S9(05)    VALUE +0.
032007
032007******* OVERALL HANDLE TIME BUCKETS
032007     05  WS-DLY-DIS-SVC-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-DLY-DIS-HANDLE-TIME      PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-DIS-HANDLE-TIME  PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-DIS-UNW-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-DLY-DIS-UNW-HTIME        PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-DIS-UNW-HTIME    PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-DIS-SVC-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-MTD-DIS-HANDLE-TIME      PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-DIS-HANDLE-TIME  PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-DIS-UNW-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-MTD-DIS-UNW-HTIME        PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-DIS-UNW-HTIME    PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-GP-SVC-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-DLY-GP-HANDLE-TIME       PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-GP-HANDLE-TIME   PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-GP-UNW-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-DLY-GP-UNW-HTIME         PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-GP-UNW-HTIME     PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-GP-SVC-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-MTD-GP-HANDLE-TIME       PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-GP-HANDLE-TIME   PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-GP-UNW-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-MTD-GP-UNW-HTIME         PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-GP-UNW-HTIME     PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-IU-SVC-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-DLY-IU-HANDLE-TIME       PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-IU-HANDLE-TIME   PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-IU-UNW-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-DLY-IU-UNW-HTIME         PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-IU-UNW-HTIME     PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-IU-SVC-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-MTD-IU-HANDLE-TIME       PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-IU-HANDLE-TIME   PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-IU-UNW-HANDLES       PIC S9(07)    VALUE +0.
032007     05  WS-MTD-IU-UNW-HTIME         PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-IU-UNW-HTIME     PIC S9(07)V99 VALUE +0.
052614
052614     05  WS-DLY-FAM-SVC-HANDLES      PIC S9(07)    VALUE +0.
052614     05  WS-DLY-FAM-HANDLE-TIME      PIC S9(07)    VALUE +0.
052614     05  WS-AVG-DLY-FAM-HANDLE-TIME  PIC S9(07)V99 VALUE +0.
052614
052614     05  WS-DLY-FAM-UNW-HANDLES      PIC S9(07)    VALUE +0.
052614     05  WS-DLY-FAM-UNW-HTIME        PIC S9(07)    VALUE +0.
052614     05  WS-AVG-DLY-FAM-UNW-HTIME    PIC S9(07)V99 VALUE +0.
052614
052614     05  WS-MTD-FAM-SVC-HANDLES      PIC S9(07)    VALUE +0.
052614     05  WS-MTD-FAM-HANDLE-TIME      PIC S9(07)    VALUE +0.
052614     05  WS-AVG-MTD-FAM-HANDLE-TIME  PIC S9(07)V99 VALUE +0.
052614
052614     05  WS-MTD-FAM-UNW-HANDLES      PIC S9(07)    VALUE +0.
052614     05  WS-MTD-FAM-UNW-HTIME        PIC S9(07)    VALUE +0.
052614     05  WS-AVG-MTD-FAM-UNW-HTIME    PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-LIF-SVC-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-DLY-LIF-HANDLE-TIME      PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-LIF-HANDLE-TIME  PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-DLY-LIF-UNW-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-DLY-LIF-UNW-HTIME        PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DLY-LIF-UNW-HTIME    PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-LIF-SVC-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-MTD-LIF-HANDLE-TIME      PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-LIF-HANDLE-TIME  PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-LIF-UNW-HANDLES      PIC S9(07)    VALUE +0.
032007     05  WS-MTD-LIF-UNW-HTIME        PIC S9(07)    VALUE +0.
032007     05  WS-AVG-MTD-LIF-UNW-HTIME    PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-GTOT-SVC-HANDLES         PIC S9(07)    VALUE +0.
032007     05  WS-GTOT-HANDLE-TIME         PIC S9(07)    VALUE +0.
032007     05  WS-GTOT-AVG-HANDLE-TIME     PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-GTOT-UNW-HANDLES         PIC S9(07)    VALUE +0.
032007     05  WS-GTOT-UNW-HTIME           PIC S9(07)    VALUE +0.
032007     05  WS-GTOT-AVG-UNW-HTIME       PIC S9(07)V99 VALUE +0.
032007
032007******* SERVICE TIME BUCKETS
032007     05  WS-DIS-PAID-DENIED-CNT      PIC S9(05)    VALUE +0.
032007     05  WS-IU-PAID-DENIED-CNT       PIC S9(05)    VALUE +0.
032007     05  WS-LIF-PAID-DENIED-CNT      PIC S9(05)    VALUE +0.
032007     05  WS-TOT-PAID-DENIED-CNT      PIC S9(05)    VALUE +0.
032007
032007     05  WS-MTD-DIS-SERVICE-TIME     PIC S9(07)    VALUE +0.
032007     05  WS-AVG-DIS-SERVICE-TIME     PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-IU-SERVICE-TIME      PIC S9(07)    VALUE +0.
032007     05  WS-AVG-IU-SERVICE-TIME      PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-MTD-LIF-SERVICE-TIME     PIC S9(07)    VALUE +0.
032007     05  WS-AVG-LIF-SERVICE-TIME     PIC S9(07)V99 VALUE +0.
032007
032007     05  WS-TOT-SERVICE-TIME         PIC S9(07)V99 VALUE +0.
032007     05  WS-AVG-SERVICE-TIME         PIC S9(07)V99 VALUE +0.
032007     05  WS-GP-PAID-DENIED-CNT       PIC S9(05)    VALUE +0.
032007     05  WS-MTD-GP-SERVICE-TIME      PIC S9(07)    VALUE +0.
032007     05  WS-AVG-GP-SERVICE-TIME      PIC S9(07)V99 VALUE +0.
052614
052614     05  WS-FAM-PAID-DENIED-CNT      PIC S9(05)    VALUE +0.
052614     05  WS-MTD-FAM-SERVICE-TIME     PIC S9(07)    VALUE +0.
052614     05  WS-AVG-FAM-SERVICE-TIME     PIC S9(07)V99 VALUE +0.
032007
032007
032007******* SERVICE TIME DISTRIBUTION BUCKETS
032007     05  WS-DIS-DISTR1-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR2-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR3-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR3A-CNT          PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR4-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR5-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR6-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR7-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR8-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR9-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-DIS-DISTR10-CNT          PIC S9(05)    VALUE +0.
032007
032007     05  WS-GP-DISTR1-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR2-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR3-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR3A-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR4-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR5-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR6-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR7-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR8-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR9-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-GP-DISTR10-CNT           PIC S9(05)    VALUE +0.
032007
032007     05  WS-IU-DISTR1-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR2-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR3-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR3A-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR4-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR5-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR6-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR7-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR8-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR9-CNT            PIC S9(05)    VALUE +0.
032007     05  WS-IU-DISTR10-CNT           PIC S9(05)    VALUE +0.
052614
052614     05  WS-FAM-DISTR1-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR2-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR3-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR3A-CNT          PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR4-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR5-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR6-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR7-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR8-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR9-CNT           PIC S9(05)    VALUE +0.
052614     05  WS-FAM-DISTR10-CNT          PIC S9(05)    VALUE +0.
032007
032007     05  WS-LIF-DISTR1-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR2-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR3-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR3A-CNT          PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR4-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR5-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR6-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR7-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR8-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR9-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-LIF-DISTR10-CNT          PIC S9(05)    VALUE +0.
032007
032007     05  WS-TOT-DISTR1-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR2-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR3-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR3A-CNT          PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR4-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR5-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR6-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR7-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR8-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR9-CNT           PIC S9(05)    VALUE +0.
032007     05  WS-TOT-DISTR10-CNT          PIC S9(05)    VALUE +0.
032007
           05  WS-RETURN-CODE              PIC S9(03)    VALUE +0.
           05  WS-ZERO                     PIC S9(01)    VALUE +0.

       01  FILLER                          COMP SYNC.
           05  PGM-SUB                     PIC S9(04)    VALUE +586.
           05  WS-SUB                      PIC S9(04)    VALUE +0.
           05  WS-SAVE-SUB                 PIC S9(04)    VALUE +0.

       01  FILLER.
           05  OLC-REPORT-NAME             PIC X(05)     VALUE 'EL591'.

           05  WS-CYCLE-DT-BINARY          PIC X(02)     VALUE SPACES.

           05  WS-PREV-CYCLE-DT-BINARY     PIC X(02)     VALUE SPACES.

           05  WS-CURR-MNTH-STRT-DT-BIN    PIC X(02)     VALUE SPACES.

           05  WS-HOLD-CLM-RECV-DT         PIC X(02)     VALUE SPACES.
           05  WS-HOLD-PP-RECV-DT          PIC X(02)     VALUE SPACES.
           05  WS-HOLD-1ST-PAY-DT          PIC X(02)     VALUE SPACES.
           05  WS-HOLD-PP-PAY-DT           PIC X(02)     VALUE SPACES.
           05  WS-HOLD-CLM-DENY-DT         PIC X(02)     VALUE SPACES.
           05  WS-HOLD-UNDW-RETURN-DT      PIC X(02)     VALUE SPACES.

           05  WS-CYCLE-DT.
               10  WS-CYCLE-DT-CC          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-YY          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-MM          PIC 9(02)     VALUE ZEROS.
               10  WS-CYCLE-DT-DD          PIC 9(02)     VALUE ZEROS.

           05  WS-EDITED-CYCLE-DT.
               10  WS-EDITED-CYCLE-DT-MM   PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CYCLE-DT-DD   PIC X(02)     VALUE SPACES.
               10  FILLER                  PIC X(01)     VALUE '/'.
               10  WS-EDITED-CYCLE-DT-YY   PIC X(02)     VALUE SPACES.

           05  WS-CURR-MNTH-STRT-DT.
               10  FILLER                  PIC X(06)     VALUE SPACES.
               10  WS-CURR-MNTH-DAY01      PIC X(02)     VALUE SPACES.

           05  WS-EOF1-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-MTD-ELMSTR                     VALUE 'Y'.

           05  WS-EOF2-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-ELTRLR                         VALUE 'Y'.
010704         88  END-OF-ELTRLR-ON-START                VALUE '1'.

           05  WS-EOF3-SW                  PIC X(01)     VALUE SPACE.
               88  END-OF-SORTFILE                       VALUE 'Y'.

           05  WS-ELTRLR-FILE-STATUS       PIC X(02)     VALUE ZERO.

           05  WS-ELTRLR-KEY-SW            PIC X(01)     VALUE SPACE.
               88  NEW-ELTRLR-KEY                        VALUE 'N'.
               88  ELTRLR-KEY-CHANGE                     VALUE 'C'.

           05  WS-TRAILER-TYPE             PIC X(01)     VALUE SPACE.
               88  COUNTABLE-ACTIVITY                    VALUE
                   '2' '3' '4' '6' '8'.

           05  WS-LEVEL88-CLAIM-TYPE       PIC X(01)     VALUE SPACE.
               88  DISABILITY-CLAIM-TYPE                 VALUE 'A'.
121603         88  GP-CLAIM-TYPE                         VALUE 'G'.
122002         88  IU-CLAIM-TYPE                         VALUE 'I'.
052614         88  FAM-CLAIM-TYPE                        VALUE 'F'.
               88  LIFE-CLAIM-TYPE                       VALUE 'L'.

           05  WS-PAID-DENY-SW             PIC X(01)     VALUE SPACE.
               88 1ST-PAYMENT-SENT-THIS-MO               VALUE '1'.
               88 CLAIM-DENIED-THIS-MO                   VALUE 'D'.
               88 PPFORM-PROCESSED-THIS-MO               VALUE 'P'.

           05  WS-CLAIM-RECV-DT-SW         PIC X(01)     VALUE 'Y'.
               88  LOOKING-FOR-CLM-RECV-DT               VALUE 'Y'.
               88  ALREADY-HAVE-CLM-RECV-DT              VALUE 'F'.

           05  WS-FORM-PP-RECV-DT-SW       PIC X(01)     VALUE 'Y'.
               88  LOOKING-FOR-PP-RECV-DT                VALUE 'Y'.
               88  ALREADY-HAVE-PP-RECV-DT               VALUE 'F'.

           05  WS-CLAIM-COUNTED-SW         PIC X(01)     VALUE SPACE.
               88  CLAIM-NOT-COUNTED                     VALUE 'N'.
               88  CLAIM-COUNTED                         VALUE 'C'.

           05  WS-MTD-OR-DAILY-SW          PIC X(01)     VALUE SPACE.
               88  DAILY                                 VALUE 'D'.
               88  MONTH-TO-DATE                         VALUE 'M'.

           05  WS-FIRST-TIME-SW            PIC X(01)     VALUE 'Y'.
               88  FIRST-SORT-RECORD                     VALUE 'Y'.
               88  NOT-FIRST-RECORD                      VALUE 'N'.

           05  WS-REPORT-DELETE-SW         PIC X(01)     VALUE 'B'.
               88  BEGIN-REPORT-DELETE                   VALUE 'B'.
               88  REPORT-DELETE-DONE                    VALUE 'D'.

           05  WS-DETAIL-REPORT-SW         PIC X(01)     VALUE 'N'.
               88  DETAIL-REPORT-NOT-DONE                VALUE 'N'.
               88  DETAIL-REPORT-DONE                    VALUE 'Y'.
032007
032007     05  WS-AUTO-PAY                 PIC X(01)     VALUE 'N'.
032007         88  AUTO-PAYMENT                          VALUE 'Y'.               
032007     05  WS-NON-AUTO-PAY             PIC X(01)     VALUE 'N'.
032007         88  NON-AUTO-PAYMENT                      VALUE 'Y'.               
032007
032007     05  WS-ELAPSED-DAYS             PIC X(04)    VALUE SPACES.
032007         88  DISTR-1                              VALUES ARE
032007             '0001', '0002', '0003', '0004', '0005'.
032007         88  DISTR-2                              VALUES ARE
032007             '0006', '0007', '0008', '0009', '0010'.
032007         88  DISTR-3                              VALUES ARE
032007             '0011'  THRU  '0015'.
032007         88  DISTR-3A                             VALUES ARE
032007             '0016'  THRU  '0020'.
032007         88  DISTR-4                              VALUES ARE
032007             '0021'  THRU  '0030'.
032007         88  DISTR-5                              VALUES ARE
032007             '0031'  THRU  '0045'.
032007         88  DISTR-6                              VALUES ARE
032007             '0046'  THRU  '0060'.
032007         88  DISTR-7                              VALUES ARE
032007             '0061'  THRU  '0075'.
032007         88  DISTR-8                              VALUES ARE
032007             '0076'  THRU  '0099'.
032007         88  DISTR-9                              VALUES ARE
032007             '0100'  THRU  '0500'.

           05  WS-HOLD-CLAIM-TYPE          PIC X(01)     VALUE SPACE.
           05  WS-HOLD-AUDITOR             PIC X(04)     VALUE SPACES.
           05  WS-HOLD-M-OR-D              PIC X(01)     VALUE SPACES.

           05  WS-HOLD-AUD-CLAIM-CERT.
               10  WS-HOLD-AUD             PIC X(04)     VALUE SPACES.
               10  WS-HOLD-DATE            PIC X(02)     VALUE SPACES.
               10  WS-HOLD-CLAIM-NUMBER    PIC X(07)     VALUE SPACES.
               10  WS-HOLD-CERT-NUMBER     PIC X(11)     VALUE SPACES.
           05  WS-PREV-AUD-CLAIM-CERT      PIC X(24)     VALUE SPACES.

           05  WS-HOLD-FILE-AUDITOR        PIC X(04)     VALUE SPACES.
           05  WS-PREV-FILE-AUDITOR        PIC X(04)     VALUE SPACES.
           
           05  WS-HOLD-SORT-MTD-DLY        PIC X(01)     VALUE SPACES.
           
           05  WS-ABEND-MESSAGE            PIC X(80)     VALUE SPACES.
           05  WS-ABEND-FILE-STATUS        PIC X(02)     VALUE ZERO.

           05  WS-AUDITOR-RPT-TITLE        PIC X(44)     VALUE
               '          CLAIMS ACTIVITY BY AUDITOR       '.

           05  WS-DETAIL-AUDITOR-RPT-TITLE        PIC X(44)     VALUE
               '          DETAIL ACTIVITY BY AUDITOR       '.

122002     05  WS-AUD-RPT-DIS-SUBT-HDR.
122002         10  FILLER                  PIC X(28)     VALUE        
122002             '-*** TOTAL CREDIT DISABILITY'. 

121603     05  WS-AUD-RPT-GP-SUBT-HDR.
121603         10  FILLER                  PIC X(28)     VALUE
121603             '-*** TOTAL DCC  GAP         '. 

122002     05  WS-AUD-RPT-IU-SUBT-HDR.
122002         10  FILLER                  PIC X(28)     VALUE
122002             '-*** TOTAL DCC UNEMPLOYMENT '. 
052614
052614     05  WS-AUD-RPT-FAM-SUBT-HDR.
052614         10  FILLER                  PIC X(28)     VALUE
052614             '-*** TOTAL DCC FAMILY LEAVE '. 

122002     05  WS-AUD-RPT-LIF-SUBT-HDR.
122002         10  FILLER                  PIC X(28)     VALUE        
122002             '-*** TOTAL CREDIT LIFE      '. 

122002     05  WS-HANDLE-TIME-RPT-TITLE.
122002         10  FILLER                  PIC X(41)     VALUE
122002             ' OVERALL AVERAGE HANDLE TIME IN DAYS FOR '.
122002         10  WS-HT-RPT-CO-ID         PIC X(03)     VALUE
122002             'CID'.

122002     05  WS-SERVICE-TIME-RPT-TITLE.
122002         10  FILLER                  PIC X(41)     VALUE
122002             'OVERALL AVERAGE SERVICE TIME IN DAYS FOR '.
122002         10  WS-ST-RPT-CO-ID         PIC X(03)     VALUE
122002             'CID'.

122002     05  WS-EXCEPTION-RPT-TITLE.
122002         10  FILLER                  PIC X(37)     VALUE
                   '   CLAIM SERVICE TIME EXCEPTIONS FOR '.
122002         10  WS-EXC-RPT-CO-ID        PIC X(03)     VALUE
122002             'CID'.
122002         10  FILLER                  PIC X(04)     VALUE SPACES.

122002     05  WS-DISTRIBUTION-RPT-TITLE.
122002         10  FILLER                  PIC X(38)     VALUE
122002             '  CLAIM SERVICE TIME DISTRIBUTION FOR '.
122002         10  WS-DIST-RPT-CO-ID       PIC X(03)     VALUE
122002             'CID'.
122002         10  FILLER                  PIC X(03)     VALUE SPACES.

           05  WS-MTD-LITERAL              PIC X(19)     VALUE
               'MONTH-TO-DATE      '.

           05  WS-DIS-LABEL                PIC X(19)     VALUE
               'CREDIT DISABILITY  '.

121603     05  WS-GP-LABEL                 PIC X(19)     VALUE
121603         'DCC   GAP          '.

           05  WS-IU-LABEL                 PIC X(19)     VALUE
               'DCC UNEMPLOYMENT   '.
052614
052614     05  WS-FAM-LABEL                PIC X(19)     VALUE
052614         'DCC FAMILY LEAVE   '.

           05  WS-LIFE-LABEL               PIC X(19)     VALUE
               'CREDIT LIFE        '.

122002     05  WS-DLY-GRANDTOT-LABEL.
122002         10  WS-DLY-GT-CO-ID         PIC X(03)     VALUE
122002             'CID'.
122002         10  FILLER                  PIC X(16)     VALUE
122002             ' GRAND TOTALS   '.

           05  WS-MTD-GRANDTOT-LABEL       PIC X(19)     VALUE
               'MTD GRAND TOTALS   '.

           05  WS-REPORT-LABEL.
               10  WS-LABEL-VARIABLE-1     PIC X(10)     VALUE SPACES.
               10  WS-LABEL-VARIABLE-2     PIC X(19)     VALUE SPACES.

           05  WS-GRAND-TOTALS-LABEL.
122002         10  FILLER                  PIC X(04)     VALUE
122002             '-***'.
122002         10  WS-GT-CO-ID             PIC X(03)     VALUE
122002             'CID'.
122002         10  FILLER                  PIC X(23)     VALUE
122002             ' GRAND TOTALS ****     '.

           05  WS-DATE.
               10  WS-DATE-YY              PIC X(02).
               10  WS-DATE-MM              PIC X(02).
               10  WS-DATE-DD              PIC X(02).
               
           05  WS-CLOSE-DESC.
               10  FILLER                  PIC X(21)           
                    VALUE 'CLOSED CLAIM, REASON='.
               10  WS-CLOSE-REASON         PIC X(01).

       01  WS-HEADING1.
           05  FILLER                      PIC X(01)     VALUE '1'.
           05  FILLER                      PIC X(40)     VALUE SPACES.
           05  WS-H1-TITLE                 PIC X(44)     VALUE SPACES.
           05  FILLER                      PIC X(36)     VALUE SPACES.
           05  WS-H1-REPORT-ID             PIC X(05)     VALUE 'EL591'.
           05  FILLER                      PIC X(07)     VALUE SPACES.


       01  WS-HEADING2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(48)     VALUE SPACES.
           05  WS-H2-COMPANY-NAME          PIC X(30)     VALUE SPACES.
           05  FILLER                      PIC X(42)     VALUE SPACES.
           05  WS-H2-DATE                  PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE SPACES.


       01  WS-HEADING3.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(44)     VALUE SPACES.
           05  FILLER                      PIC X(31)     VALUE
               'MONTH-TO-DATE ACTIVITY THROUGH '.
           05  WS-H3-THROUGH-DT            PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(35)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE 'PAGE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-H3-PAGE                  PIC ZZ,ZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACE.

      *** HEADING4 FOR CLAIM ACTIVITY BY AUDITOR
       01  WS-HEADING4.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               ' WORK  '.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               ' NEW  '.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               ' PAID '.
           05  FILLER                      PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               'PAYMENTS'.
           05  FILLER                      PIC X(14)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'STOP'.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'MAIL'.
           05  FILLER                      PIC X(27)     VALUE SPACES.
           05  FILLER                      PIC X(03)     VALUE
               'MED'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'NOTE'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CALLS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CALLS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               ' DE-'.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               ' RE-'.

032007
032007*** HEADING4 FOR MONTH-TO-DATE AVERAGE HANDLE TIME FOR CID
032007 01  WS-HEADING4-HANDLE-TIME.
032007     05  FILLER                      PIC X(01)     VALUE '-'.
032007     05  FILLER                      PIC X(36)     VALUE SPACES.
032007     05  FILLER                      PIC X(15)     VALUE
032007         '  DEPARTMENT   '.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         ' DEPARTMENT '.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(18)     VALUE
032007         'DEPARTMENT AVERAGE'.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'UNDERWRITING'.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'UNDERWRITING'.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'UNDERWRITING'.
032007
032007
032007
032007*** HEADING4 FOR MONTH-TO-DATE AVERAGE SERVICE TIME FOR CID
032007 01  WS-HEADING4-SERVICE-TIME.
032007     05  FILLER                      PIC X(01)     VALUE '-'.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(11)     VALUE SPACES.
032007     05  FILLER                      PIC X(14)     VALUE
032007         'TOTAL PAID AND'.
032007     05  FILLER                      PIC X(12)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         '   TOTAL    '.
032007     05  FILLER                      PIC X(12)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'SERVICE TIME'.
032007     05  FILLER                      PIC X(21)     VALUE SPACES.
032007
032007
032007
032007*** HEADING4 FOR CLAIM SERVICE TIME EXCEPTIONS
032007 01  WS-HEADING4-EXCEPTIONS.
032007     05  FILLER                      PIC X(01)     VALUE '-'.
032007     05  FILLER                      PIC X(07)     VALUE
032007         'CLAIM #'.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  FILLER                      PIC X(10)     VALUE
032007         ' POLICY # '.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  FILLER                      PIC X(17)     VALUE
032007         '   CLAIM TYPE    '.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  FILLER                      PIC X(11)     VALUE
032007         '  ACTION   '.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  FILLER                      PIC X(14)     VALUE
032007         'FORM RECEIVED '.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  FILLER                      PIC X(11)     VALUE
032007         'ACTION DATE'.
032007     05  FILLER                      PIC X(05)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'SERVICE TIME'.
032007
032007
032007
032007*** HEADING4 FOR CLAIM SERVICE TIME DISTRIBUTION
032007 01  WS-HEADING4-DISTRIBUTION.
032007     05  FILLER                      PIC X(01)     VALUE '-'.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '1-5  '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '6-10 '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '11-15'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '16-20'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '21-30'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '31-45'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '46-60'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '61-75'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '76-99'.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         '100+ '.
032007     05  FILLER                      PIC X(02)     VALUE SPACES.
032007

      *** HEADING5 FOR CLAIM ACTIVITY BY AUDITOR
       01  WS-HEADING5.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'MEASURE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'CLAIMS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'CLAIMS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'PART'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'FINAL'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'ADDIT'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'OTHER'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'VOIDS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'PAYS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'w/REQ'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'w/oREQ'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'LTRS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'NOTES'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'UNWS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'UNWR'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'RECS'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'FILE'.
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               ' IN  '.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(03)     VALUE
               'OUT'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'NIAL'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLOSE'.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'OPEN'.
      
032007
032007*** HEADING5 FOR MONTH-TO-DATE AVERAGE HANDLE TIME
032007 01  WS-HEADING5-HANDLE-TIME.
032007     05  FILLER                      PIC X(01)     VALUE SPACE.
032007     05  FILLER                      PIC X(36)     VALUE SPACES.
032007     05  FILLER                      PIC X(15)     VALUE
032007         'SERVICE HANDLES'.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'SERVICE TIME'.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(18)     VALUE
032007         '    PER HANDLE    '.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         '  HANDLES   '.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'SERVICE TIME'.
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         '  AVERAGE   '.
032007
032007
032007*** HEADING5 FOR MONTH-TO-DATE AVERAGE SERVICE TIME
032007 01  WS-HEADING5-SERVICE-TIME.
032007     05  FILLER                      PIC X(01)     VALUE SPACE.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(11)     VALUE SPACES.
032007     05  FILLER                      PIC X(14)     VALUE
032007         'DENIED CLAIMS '.
032007     05  FILLER                      PIC X(12)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         'SERVICE TIME'.
032007     05  FILLER                      PIC X(12)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE
032007         '  AVERAGE   '.
032007     05  FILLER                      PIC X(21)     VALUE SPACES.
032007
032007
032007
032007*** HEADING5 FOR CLAIM SERVICE TIME DISTRIBUTION
032007 01  WS-HEADING5-DISTRIBUTION.
032007     05  FILLER                      PIC X(01)     VALUE SPACE.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(06)     VALUE SPACES.
032007     05  FILLER                      PIC X(05)     VALUE
032007         'DAYS '.
032007     05  FILLER                      PIC X(02)     VALUE SPACES.

      *** DETAIL HEADING FOR CLAIM ACTIVITY BY AUDITOR
       01  WS-DET-HEADING.
           05  FILLER                      PIC X(01)     VALUE '0'.
           05  FILLER                      PIC X(08)     VALUE SPACES.
           05  FILLER                      PIC X(06)
                     VALUE 'DATE: '.               
           05  DET-HEAD-DATE               PIC X(10).
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  FILLER                      PIC X(10)
                     VALUE 'COVERAGE: '.
           05  DET-HEAD-COV                PIC X(19).
           05  FILLER                      PIC X(02)     VALUE SPACES.
           05  FILLER                      PIC X(09)
                     VALUE 'AUDITOR: '.
           05  DET-HEAD-AUDITOR            PIC X(04).
           05  FILLER                      PIC X(59)     VALUE SPACES.

      *** DETAIL FOR CLAIM ACTIVITY BY AUDITOR
       01  WS-DETAIL1.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  WS-D1-DAY-MTD               PIC X(03).
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-WORK-MEASURE-CNT      PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-NEW-CLM-CNT           PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-PD-CLM-CNT            PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-PART-DRAFT-CNT        PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-FINAL-DRAFT-CNT       PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-ADDIT-DRAFT-CNT       PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-OTHER-DRAFT-CNT       PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-VOID-CNT              PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-STOPPAY-CNT           PIC ZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-MAIL-W-REQ-CNT        PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-MAIL-WO-REQ-CNT       PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-LETTER-CNT            PIC ZZZZ9.                   
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-NOTE-CNT              PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-UNWS-CNT              PIC ZZZZ9.                
      *     05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-UNWR-CNT              PIC ZZZZ9.   
      *     05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-MED-REC-CNT           PIC ZZZZ9. 
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-NOTE-FILE-CNT         PIC ZZZZ9. 
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CALLIN-CNT            PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-CALLOUT-CNT           PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-DENIAL-CNT            PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-DROP-CNT              PIC ZZZZ9.
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D1-REOPEN-CNT            PIC ZZZZ9.

032007*** DETAIL FOR MONTH-TO-DATE AVERAGE HANDLE TIME
032007 01  WS-DETAIL2-HANDLE-TIME.
032007     05  FILLER                      PIC X(01)     VALUE SPACE.
032007     05  WS-D2-COL1-LABEL            PIC X(29)     VALUE SPACES.
032007     05  FILLER                      PIC X(11)     VALUE SPACES.
032007     05  WS-D2-TOT-SVC-HANDLES       PIC ZZ,ZZ9.
032007     05  FILLER                      PIC X(11)     VALUE SPACES.
032007     05  WS-D2-TOT-HANDLE-TIME       PIC ZZ,ZZ9.
032007     05  FILLER                      PIC X(12)     VALUE SPACES.
032007     05  WS-D2-AVERAGE-HANDLE-TIME   PIC Z9.99.
032007     05  FILLER                      PIC X(13)     VALUE SPACES.
032007     05  WS-D2-UNDWRITG-HANDLES      PIC ZZ,ZZ9.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  WS-D2-UNDWRITG-HTIME        PIC ZZ,ZZ9.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  WS-D2-UNDWRITG-AVG-HTIME    PIC Z9.99.
032007     05  FILLER                      PIC X(04)     VALUE SPACES.
032007
032007
032007*** DETAIL FOR MONTH-TO-DATE AVERAGE SERVICE TIME
032007 01  WS-DETAIL3-SERVICE-TIME.
032007     05  FILLER                      PIC X(01)     VALUE '0'.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  WS-D3-COL1-LABEL            PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(15)     VALUE SPACES.
032007     05  WS-D3-PAID-DENIED-CNT       PIC ZZ,ZZ9.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  WS-D3-TOT-SVC-TIME          PIC ZZ,ZZ9.
032007     05  FILLER                      PIC X(19)     VALUE SPACES.
032007     05  WS-D3-AVG-SVC-TIME          PIC Z9.99.
032007     05  FILLER                      PIC X(24)     VALUE SPACES.
032007
032007
032007
032007*** DETAIL FOR SERVICE TIME EXCEPTIONS
032007 01  WS-DETAIL4-EXCEPTIONS.
032007     05  FILLER                      PIC X(01)     VALUE SPACE.
032007     05  WS-D4-CLAIM-NO              PIC X(07)     VALUE SPACES.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  WS-D4-POLICY-NO             PIC X(10)     VALUE SPACES.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  WS-D4-CLAIM-TYPE            PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D4-ACTION                PIC X(11)     VALUE SPACES.
032007     05  FILLER                      PIC X(12)     VALUE SPACES.
032007     05  WS-D4-FORM-RCVD-DT          PIC X(08)     VALUE SPACES.
032007     05  FILLER                      PIC X(13)     VALUE SPACES.
032007     05  WS-D4-CLM-ACTION-DT         PIC X(08)     VALUE SPACES.
032007     05  FILLER                      PIC X(11)     VALUE SPACES.
032007     05  WS-D4-CLM-SVC-TIME          PIC ZZ9.
032007     05  FILLER                      PIC X(05)     VALUE SPACES.
032007
032007
032007
032007*** DETAIL FOR SERVICE TIME DISTRIBUTION
032007 01  WS-DETAIL5-DISTRIBUTION.
032007     05  FILLER                      PIC X(01)     VALUE SPACE.
032007     05  WS-D5-COL1-LABEL            PIC X(19)     VALUE SPACES.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-1               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-2               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-3               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-3A              PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-4               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-5               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-6               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-7               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-8               PIC ZZZ9.
032007     05  FILLER                      PIC X(07)     VALUE SPACES.
032007     05  WS-D5-DISTR-9               PIC ZZZ9.
032007     05  FILLER                      PIC X(14)     VALUE SPACES.


      *** HEADING4 FOR DETAIL ACTIVITY BY AUDITOR
       01  WS-HEADING4-2.
           05  FILLER                      PIC X(01)     VALUE '-'.
           05  FILLER                      PIC X(12)     VALUE SPACES.
           05  FILLER                      PIC X(08)     VALUE
               'RECORDED'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLAIM'.
           05  FILLER                      PIC X(07)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'CERT'.
           05  FILLER                      PIC X(11)     VALUE SPACES.
           05  FILLER                      PIC X(05)     VALUE
               'CLAIM'.
           05  FILLER                      PIC X(41)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'FROM'.
           05  FILLER                      PIC X(10)     VALUE SPACES.
           05  FILLER                      PIC X(02)     VALUE
               'TO'.
032007     05  FILLER                      PIC X(09)     VALUE SPACES.
032007     05  FILLER                      PIC X(06)     VALUE
032007         'HANDLE'.
032007     05  FILLER                      PIC X(04)     VALUE SPACES.



      *** HEADING5 FOR DETAIL ACTIVITY BY AUDITOR
       01  WS-HEADING5-2.
           05  FILLER                      PIC X(01)     VALUE SPACE.
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  FILLER                      PIC X(07)     VALUE
               'AUDITOR'.
           05  FILLER                      PIC X(04)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DATE'.
           05  FILLER                      PIC X(06)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'NUMBER'.
           05  FILLER                      PIC X(06)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'NUMBER'.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'TYPE'.
           05  FILLER                      PIC X(17)     VALUE SPACES.
           05  FILLER                      PIC X(06)     VALUE
               'ACTION'.
           05  FILLER                      PIC X(19)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DATE'.
           05  FILLER                      PIC X(09)     VALUE SPACES.
           05  FILLER                      PIC X(04)     VALUE
               'DATE'.
032007     05  FILLER                      PIC X(05)     VALUE SPACES.
032007     05  FILLER                      PIC X(03)     VALUE
032007         'Y/N'.
032007     05  FILLER                      PIC X(04)     VALUE SPACES.
032007     05  FILLER                      PIC X(04)     VALUE
032007         'TIME'.
032007     05  FILLER                      PIC X(02)     VALUE SPACES.

      *** DETAIL FOR DETAIL ACTIVITY BY AUDITOR
       01  WS-DETAIL6.
           05  FILLER                      PIC X(05)     VALUE SPACES.
           05  WS-D6-AUDITOR               PIC X(04).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-D6-RECORDED-DT.
               10 WS-D6-RECORDED-MM        PIC X(02).
               10 WS-D6-RECORDED-SL1       PIC X(01).
               10 WS-D6-RECORDED-DD        PIC X(02).
               10 WS-D6-RECORDED-SL2       PIC X(03).
               10 WS-D6-RECORDED-YY        PIC X(02).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-D6-CLAIM-NUMBER          PIC X(07).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-D6-CERT-NUMBER           PIC X(11).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-D6-CLAIM-TYPE            PIC X(19).
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D6-ACTION                PIC X(25).
           05  FILLER                      PIC X(01)     VALUE SPACES.
           05  WS-D6-FROM-DT.
               10 WS-D6-FROM-MM            PIC X(02).
               10 WS-D6-FROM-SL1           PIC X(01).
               10 WS-D6-FROM-DD            PIC X(02).
               10 WS-D6-FROM-SL2           PIC X(03).
               10 WS-D6-FROM-YY            PIC X(02).
           05  FILLER                      PIC X(03)     VALUE SPACES.
           05  WS-D6-TO-DT.
               10 WS-D6-TO-MM              PIC X(02).
               10 WS-D6-TO-SL1             PIC X(01).
               10 WS-D6-TO-DD              PIC X(02).
               10 WS-D6-TO-SL2             PIC X(03).
               10 WS-D6-TO-YY              PIC X(02).
032007     05  FILLER                      PIC X(03)     VALUE SPACES.
032007     05  WS-D6-HANDLE                PIC X(01).
032007     05  FILLER                      PIC X(02)     VALUE SPACES.
032007     05  WS-D6-HANDLE-TIME           PIC Z(5).99.
032007     05  FILLER                      PIC X(01)     VALUE SPACES.


       01  FILE-REC.
           05  FILE-CLAIM-TYPE             PIC X(01).
           05  FILE-AUDITOR                PIC X(04).
           05  FILE-MTD-OR-DAILY           PIC X(01).
           05  FILE-CLAIM-NUMBER           PIC X(07).
           05  FILE-CERT-NUMBER            PIC X(11).
           05  FILE-REC-TYPE.
               10  FILE-REC-TYPE-1         PIC X(01).
               10  FILE-REC-TYPE-2         PIC X(01).
           05  FILE-RECORDED-DT            PIC X(06).
           05  FILE-FROM-DT                PIC X(06).
           05  FILE-TO-DT                  PIC X(06).
032007     05  FILE-HANDLE-TIME            PIC S9(07)V99.
032007     05  FILE-HANDLE-COUNTED         PIC X(01).


      *              *************
      *              ELCDTECX: LAYOUT FOR DISK-DATE FILE
                     COPY ELCDTECX.


                     COPY ELCDTEVR.

      *              *************
      *              ELCDATE: LAYOUT OF DATA PASSED TO DATE CONV RTN
                     COPY ELCDATE.


       LINKAGE SECTION.

       01  PARM.
           05  PARM-LENGTH        COMP     PIC S9(04)    VALUE ZEROS.
           05  PARM-PREV-CYCLE-DT          PIC X(08)     VALUE SPACES.
           05  PARM-CYCLE-DT               PIC X(08)     VALUE SPACES.

      ******************************************************************
      ********************************
       PROCEDURE DIVISION USING PARM.

      ****************READ DISK-DATE FILE
       0000-DATE-CARD-READ. COPY ELCDTERX.

       1000-MAIN-LOGIC.

           PERFORM 1500-EDIT-CYCLE-DATE       THRU 1500-EXIT

           PERFORM OPEN-FILES

           MOVE COMPANY-NAME                  TO WS-H2-COMPANY-NAME
           MOVE WS-CURRENT-DATE               TO WS-H2-DATE
           MOVE WS-EDITED-CYCLE-DT            TO WS-H3-THROUGH-DT

030612     MOVE DTE-CLIENT                    TO WS-GT-CO-ID
030612                                           WS-DLY-GT-CO-ID      
030612                                           WS-HT-RPT-CO-ID      
030612                                           WS-ST-RPT-CO-ID      
030612                                           WS-EXC-RPT-CO-ID     
030612                                           WS-DIST-RPT-CO-ID    
030612
122002     IF DTE-CLIENT = 'DCC'
122002         MOVE 'DCC'                     TO WS-GT-CO-ID
122002         MOVE 'DCC'                     TO WS-DLY-GT-CO-ID
122002         MOVE 'DCC'                     TO WS-HT-RPT-CO-ID
122002         MOVE 'DCC'                     TO WS-ST-RPT-CO-ID
122002         MOVE 'DCC'                     TO WS-EXC-RPT-CO-ID 
122002         MOVE 'DCC'                     TO WS-DIST-RPT-CO-ID
122002         MOVE 'DCC DISABILITY     '     TO WS-DIS-LABEL
122002         MOVE 'DCC LIFE           '     TO WS-LIFE-LABEL
122002         MOVE '-*** TOTAL DCC DISABILITY   '
122002                                        TO WS-AUD-RPT-DIS-SUBT-HDR
122002         MOVE '-*** TOTAL DCC LIFE         '
122002                                        TO WS-AUD-RPT-LIF-SUBT-HDR
122002     END-IF
  

           SORT SORT-FILE  ASCENDING KEY SORT-CLAIM-TYPE SORT-AUDITOR 
                  SORT-RECORDED-DT SORT-CLAIM-NUMBER SORT-CERT-NUMBER
              INPUT  PROCEDURE 2000-INPUT-PROCEDURE  THRU 2000-EXIT
              OUTPUT PROCEDURE 3000-OUTPUT-PROCEDURE THRU 3000-EXIT

           CLOSE FILE-OUT.
           
           SORT SORT-FILE2 ON ASCENDING SORT2-AUDITOR 
                                        SORT2-RECORDED-DT 
                                        SORT2-CLAIM-NUMBER
                                        SORT2-CERT-NUMBER
               USING FILE-OUT
               GIVING FILE-OUT.
               
           OPEN INPUT FILE-OUT.    
           MOVE ZERO                           TO WS-PAGE2
           MOVE +55                            TO WS-LINE2-COUNT
           MOVE WS-DETAIL-AUDITOR-RPT-TITLE    TO WS-H1-TITLE
           
           PERFORM 5000-PRINT-DETAIL-FILE THRU 5000-EXIT
                         UNTIL DETAIL-REPORT-DONE.

           PERFORM CLOSE-FILES


           GOBACK.


       1500-EDIT-CYCLE-DATE.

           IF PARM-LENGTH = +0
               DISPLAY 'CYCLE DATE INPUT PARMS MISSING'
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF

           MOVE PARM-CYCLE-DT               TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-CYCLE-DT-BINARY
               MOVE PARM-CYCLE-DT           TO WS-CYCLE-DT
               MOVE WS-CYCLE-DT-MM          TO WS-EDITED-CYCLE-DT-MM
               MOVE WS-CYCLE-DT-DD          TO WS-EDITED-CYCLE-DT-DD
               MOVE WS-CYCLE-DT-YY          TO WS-EDITED-CYCLE-DT-YY
           ELSE
               DISPLAY 'INVALID CYCLE DATE ' PARM-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF


      ************* CREATE BEGINNING OF MONTH DATE FOR MTD PROCESSING
           MOVE PARM-CYCLE-DT               TO WS-CURR-MNTH-STRT-DT
           MOVE '01'                        TO WS-CURR-MNTH-DAY01
           MOVE WS-CURR-MNTH-STRT-DT        TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-CURR-MNTH-STRT-DT-BIN
           ELSE
               DISPLAY 'CONVERSION ERROR ON MNTH-STRT-DT'
                                         WS-CURR-MNTH-STRT-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF
      *************

           MOVE PARM-PREV-CYCLE-DT          TO DC-GREG-DATE-CYMD
           MOVE 'L'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-PREV-CYCLE-DT-BINARY
           ELSE
               DISPLAY 'INVALID PREVIOUS CYCLE DATE ' PARM-PREV-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF


           IF NO-CONVERSION-ERROR
               MOVE DC-BIN-DATE-1           TO WS-PREV-CYCLE-DT-BINARY
           ELSE
               DISPLAY 'INVALID PREVIOUS CYCLE DATE ' PARM-PREV-CYCLE-DT
               PERFORM ABEND-PGM            THRU APS-EXIT
           END-IF

           .
       1500-EXIT.
           EXIT.


       2000-INPUT-PROCEDURE.

           PERFORM 2010-GET-RECS-FOR-SORT   THRU 2010-EXIT
               UNTIL END-OF-MTD-ELMSTR

           IF SORT-RETURN  NOT = ZEROS
              MOVE 'INTERNAL SORT ABORTED'  TO  WS-ABEND-MESSAGE
              MOVE '0101'                   TO  WS-RETURN-CODE
              PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF

           .
       2000-EXIT.
           EXIT.


       2010-GET-RECS-FOR-SORT.

           MOVE SPACES                      TO WS-HOLD-CLM-RECV-DT
           MOVE SPACES                      TO WS-HOLD-1ST-PAY-DT
           MOVE SPACES                      TO WS-HOLD-PP-PAY-DT
           MOVE SPACES                      TO WS-HOLD-CLM-DENY-DT
           MOVE SPACE                       TO WS-PAID-DENY-SW

           READ MTD-ELMSTR-INFILE
               AT END
                   SET END-OF-MTD-ELMSTR    TO TRUE
                   GO TO 2010-EXIT
           END-READ

           IF CL-CLAIM-TYPE = SPACE OR LOW-VALUE
      ********* THIS SITUATION HAS BEEN CORRECTED MOVING FORWARD 
               DISPLAY 'NO CLM TYP FOR CNTL PRI ' CL-CONTROL-PRIMARY
               GO TO 2010-EXIT
           END-IF

           MOVE CL-CLAIM-TYPE               TO SORT-CLAIM-TYPE
           MOVE CL-CLAIM-TYPE               TO WS-LEVEL88-CLAIM-TYPE
           MOVE CL-CLAIM-NO                 TO SORT-CLAIM-NUMBER
           MOVE CL-CERT-NO                  TO SORT-CERT-NUMBER
           MOVE SPACES                      TO SORT-MTD-OR-DAILY
           MOVE LOW-VALUE                   TO SORT-FROM-DT
           MOVE LOW-VALUE                   TO SORT-TO-DT
032007     MOVE +0                          TO SORT-HANDLE-TIME
032007     MOVE 'N'                         TO SORT-HANDLE-COUNTED           

      ******* PICKING UP NEW CLAIMS
           IF (CL-FILE-ESTABLISH-DT >= WS-CURR-MNTH-STRT-DT-BIN  AND
               CL-FILE-ESTABLISH-DT <= WS-PREV-CYCLE-DT-BINARY)
               MOVE CL-PROCESSOR-ID          TO SORT-AUDITOR
               MOVE 'M'                      TO SORT-MTD-OR-DAILY
               MOVE 'N '                     TO SORT-REC-TYPE
               MOVE CL-FILE-ESTABLISH-DT     TO SORT-RECORDED-DT
               MOVE CL-FILE-ESTABLISH-DT     TO SORT-FROM-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
           ELSE
               IF (CL-FILE-ESTABLISH-DT  > WS-PREV-CYCLE-DT-BINARY  AND
                   CL-FILE-ESTABLISH-DT <= WS-CYCLE-DT-BINARY)
                   MOVE CL-PROCESSOR-ID      TO SORT-AUDITOR
                   MOVE 'D'                  TO SORT-MTD-OR-DAILY
                   MOVE 'N '                 TO SORT-REC-TYPE
                   MOVE CL-FILE-ESTABLISH-DT TO SORT-RECORDED-DT
                   MOVE CL-FILE-ESTABLISH-DT TO SORT-FROM-DT
                   PERFORM 2900-RELEASE-SORT THRU 2900-EXIT
               END-IF
           END-IF

           MOVE SPACE                        TO SORT-MTD-OR-DAILY
           
      ******* PICKING UP DROPPED CLAIMS (CLOSES)
           IF CL-LAST-CLOSE-REASON = '1' OR '2' OR '4'

               IF CL-LAST-CLOSE-REASON = '1' OR '2'
                   MOVE 'AUTO'               TO SORT-AUDITOR
               ELSE
      *************** CANNOT TIE CLOSE ACTION TO SPECIFIC AUDITOR WITH
      *************** INTEGRITY
                   MOVE 'ZZZZ'               TO SORT-AUDITOR
               END-IF

               MOVE CL-LAST-CLOSE-REASON TO SORT-REC-TYPE-2
               MOVE CL-LAST-CLOSE-DT     TO SORT-TO-DT

               IF (CL-LAST-CLOSE-DT >= WS-CURR-MNTH-STRT-DT-BIN  AND
                   CL-LAST-CLOSE-DT <= WS-PREV-CYCLE-DT-BINARY)
                   MOVE 'M'                  TO SORT-MTD-OR-DAILY
                   MOVE 'D'                  TO SORT-REC-TYPE-1
                   MOVE CL-LAST-CLOSE-DT     TO SORT-RECORDED-DT
                   PERFORM 2900-RELEASE-SORT THRU 2900-EXIT
               ELSE
                   IF (CL-LAST-CLOSE-DT  > WS-PREV-CYCLE-DT-BINARY  AND
                       CL-LAST-CLOSE-DT <= WS-CYCLE-DT-BINARY)
                       MOVE 'D'              TO SORT-MTD-OR-DAILY
                       MOVE 'D'              TO SORT-REC-TYPE-1
                       MOVE CL-LAST-CLOSE-DT TO SORT-RECORDED-DT
                       PERFORM 2900-RELEASE-SORT THRU 2900-EXIT
                   END-IF
               END-IF

               IF (CL-LAST-CLOSE-REASON = '2' AND
                   SORT-MTD-OR-DAILY NOT= SPACE)
                   SET CLAIM-DENIED-THIS-MO  TO TRUE
                   MOVE CL-LAST-CLOSE-DT     TO WS-HOLD-CLM-DENY-DT
               END-IF

           END-IF


      ******* PICKING UP CLAIMS THAT HAVE BEEN REOPENED
           IF (CL-LAST-REOPEN-DT >= WS-CURR-MNTH-STRT-DT-BIN  AND
               CL-LAST-REOPEN-DT <= WS-PREV-CYCLE-DT-BINARY)
               MOVE 'ZZZZ'                   TO SORT-AUDITOR
               MOVE 'M'                      TO SORT-MTD-OR-DAILY
093003         MOVE 'X '                     TO SORT-REC-TYPE
               MOVE CL-LAST-REOPEN-DT        TO SORT-RECORDED-DT
               MOVE CL-LAST-REOPEN-DT        TO SORT-FROM-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
           ELSE
               IF (CL-LAST-REOPEN-DT  > WS-PREV-CYCLE-DT-BINARY  AND
                   CL-LAST-REOPEN-DT <= WS-CYCLE-DT-BINARY)
                   MOVE 'ZZZZ'               TO SORT-AUDITOR
                   MOVE 'D'                  TO SORT-MTD-OR-DAILY
093003             MOVE 'X '                 TO SORT-REC-TYPE
                   MOVE CL-LAST-REOPEN-DT    TO SORT-RECORDED-DT
                   MOVE CL-LAST-REOPEN-DT    TO SORT-FROM-DT
                   PERFORM 2900-RELEASE-SORT THRU 2900-EXIT
               END-IF
           END-IF

           SET CLAIM-NOT-COUNTED             TO TRUE
           move 'N' to ws-auto-pay  ws-non-auto-pay
           PERFORM 2075-START-ELTRLR         THRU 2075-EXIT
 
010704     IF END-OF-ELTRLR-ON-START
010704         GO TO 2010-EXIT
010704     END-IF
           IF (WS-HOLD-CLM-RECV-DT = SPACES AND
               WS-HOLD-PP-RECV-DT  = SPACES)
           display 'clm recv,pp recv = spaces ' 
               GO TO 2010-EXIT
           END-IF


           IF (WS-HOLD-PP-PAY-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
               WS-HOLD-PP-PAY-DT <= WS-CYCLE-DT-BINARY)
               SET PPFORM-PROCESSED-THIS-MO      TO TRUE
           ELSE
               IF (WS-HOLD-1ST-PAY-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
                   WS-HOLD-1ST-PAY-DT <= WS-CYCLE-DT-BINARY)
                   SET 1ST-PAYMENT-SENT-THIS-MO  TO TRUE
               END-IF
           END-IF


           IF PPFORM-PROCESSED-THIS-MO
               IF WS-HOLD-PP-RECV-DT = SPACES
                display 'paid this mo-no pp recv'
                   GO TO 2010-EXIT
               END-IF

               MOVE WS-HOLD-PP-RECV-DT           TO DC-BIN-DATE-1
               MOVE WS-HOLD-PP-PAY-DT            TO DC-BIN-DATE-2
               MOVE 'PAYMENT    '                TO WS-D4-ACTION
           ELSE
               IF WS-HOLD-CLM-RECV-DT = SPACES
      *         display 'no clm recv dt'
                   GO TO 2010-EXIT
               END-IF

               IF 1ST-PAYMENT-SENT-THIS-MO
                   MOVE WS-HOLD-CLM-RECV-DT      TO DC-BIN-DATE-1
                   MOVE WS-HOLD-1ST-PAY-DT       TO DC-BIN-DATE-2
                   MOVE '1ST PAYMENT'            TO WS-D4-ACTION
               ELSE
                   IF CLAIM-DENIED-THIS-MO
                       MOVE WS-HOLD-CLM-RECV-DT  TO DC-BIN-DATE-1
                       MOVE WS-HOLD-CLM-DENY-DT  TO DC-BIN-DATE-2
                       MOVE 'DENIAL     '        TO WS-D4-ACTION
                   else
                   if  AUTO-PAYMENT AND NOT NON-AUTO-PAYMENT
                       MOVE WS-HOLD-CLM-RECV-DT  TO DC-BIN-DATE-1    
                       MOVE WS-HOLD-CLM-RECV-DT  TO DC-BIN-DATE-2
                       MOVE 'AUTO PAYMENT'       TO WS-D4-ACTION    
      *                 display 'auto pay'
                   ELSE
      *             	display 'nothing this month'
                       GO TO 2010-EXIT
                   END-IF
                   END-IF
               END-IF
           END-IF

031407     IF  DC-BIN-DATE-1 <= DC-BIN-DATE-2
               MOVE '1'                          TO DC-OPTION-CODE
               PERFORM 8500-DATE-CONVERSION      THRU 8500-EXIT
           ELSE
              display 'date 1 > date 2'
               GO TO 2010-EXIT
           END-IF


           IF NO-CONVERSION-ERROR
               CONTINUE
           ELSE
               DISPLAY 'CONVERSION ERROR IN PARA 2010- ERROR '
                                             DC-ERROR-CODE
               DISPLAY 'CLAIM KEY IS ' CL-CONTROL-PRIMARY
               PERFORM ABEND-PGM             THRU APS-EXIT
           END-IF

032007     EVALUATE TRUE
032007     WHEN (CL-CLAIM-TYPE = 'A' AND
032007         DC-ELAPSED-DAYS < WS-MAX-SVC-TIME-FOR-AVG)
032007         ADD DC-ELAPSED-DAYS           TO WS-MTD-DIS-SERVICE-TIME
032007         ADD +1                        TO WS-DIS-PAID-DENIED-CNT
               display 'determination type a ' cl-claim-no
032007
032007     WHEN (CL-CLAIM-TYPE = 'G' AND
032007         DC-ELAPSED-DAYS < WS-MAX-SVC-TIME-FOR-AVG)
032007         ADD DC-ELAPSED-DAYS           TO WS-MTD-GP-SERVICE-TIME
032007         ADD +1                        TO WS-GP-PAID-DENIED-CNT
032007
032007     WHEN (CL-CLAIM-TYPE = 'I' AND
032007         DC-ELAPSED-DAYS < WS-MAX-SVC-TIME-FOR-AVG)
032007         ADD DC-ELAPSED-DAYS           TO WS-MTD-IU-SERVICE-TIME
032007         ADD +1                        TO WS-IU-PAID-DENIED-CNT
052614
052614     WHEN (CL-CLAIM-TYPE = 'F' AND
052614         DC-ELAPSED-DAYS < WS-MAX-SVC-TIME-FOR-AVG)
052614         ADD DC-ELAPSED-DAYS           TO WS-MTD-FAM-SERVICE-TIME
052614         ADD +1                        TO WS-FAM-PAID-DENIED-CNT
032007
032007     WHEN (CL-CLAIM-TYPE = 'L' AND
032007         DC-ELAPSED-DAYS < WS-MAX-SVC-TIME-FOR-AVG)
032007         ADD DC-ELAPSED-DAYS           TO WS-MTD-LIF-SERVICE-TIME
032007         ADD +1                        TO WS-LIF-PAID-DENIED-CNT
               display 'determination type l ' cl-claim-no
032007     END-EVALUATE
032007
032007     MOVE DC-ELAPSED-DAYS              TO WS-ELAPSED-DAYS
032007
032007
032007******* ACCUMULATION OF TOTALS FOR SERVICE TIME DISTRIBUTION RPT
032007     EVALUATE TRUE
032007     WHEN (DISTR-1 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR1-CNT
032007
032007     WHEN (DISTR-2 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR2-CNT
032007
032007     WHEN (DISTR-3 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR3-CNT
032007
032007     WHEN (DISTR-3A AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR3A-CNT
032007
032007     WHEN (DISTR-4 AND
032007          DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR4-CNT
032007
032007     WHEN (DISTR-5 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR5-CNT
032007
032007     WHEN (DISTR-6 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR6-CNT
032007
032007     WHEN (DISTR-7 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR7-CNT
032007
032007     WHEN (DISTR-8 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR8-CNT
032007
032007     WHEN (DISTR-9 AND
032007           DISABILITY-CLAIM-TYPE)
032007         ADD +1                        TO WS-DIS-DISTR9-CNT
032007
032007
032007********* GAP BUCKETS
032007     WHEN (DISTR-1 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR1-CNT
032007
032007     WHEN (DISTR-2 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR2-CNT
032007
032007     WHEN (DISTR-3 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR3-CNT
032007
032007     WHEN (DISTR-3A AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR3A-CNT
032007
032007     WHEN (DISTR-4 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR4-CNT
032007
032007     WHEN (DISTR-5 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR5-CNT
032007
032007     WHEN (DISTR-6 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR6-CNT
032007
032007     WHEN (DISTR-7 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR7-CNT
032007
032007     WHEN (DISTR-8 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR8-CNT
032007
032007     WHEN (DISTR-9 AND
032007           GP-CLAIM-TYPE)
032007         ADD +1                        TO WS-GP-DISTR9-CNT
032007
032007********* IU BUCKETS
032007     WHEN (DISTR-1 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR1-CNT
032007
032007     WHEN (DISTR-2 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR2-CNT
032007
032007     WHEN (DISTR-3 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR3-CNT
032007
032007     WHEN (DISTR-3A AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR3A-CNT
032007
032007     WHEN (DISTR-4 AND
032007          IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR4-CNT
032007
032007     WHEN (DISTR-5 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR5-CNT
032007
032007     WHEN (DISTR-6 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR6-CNT
032007
032007     WHEN (DISTR-7 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR7-CNT
032007
032007     WHEN (DISTR-8 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR8-CNT
032007
032007     WHEN (DISTR-9 AND
032007           IU-CLAIM-TYPE)
032007         ADD +1                        TO WS-IU-DISTR9-CNT
052614
052614********* FAM BUCKETS
052614     WHEN (DISTR-1 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR1-CNT
052614
052614     WHEN (DISTR-2 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR2-CNT
052614
052614     WHEN (DISTR-3 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR3-CNT
052614
052614     WHEN (DISTR-3A AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR3A-CNT
052614
052614     WHEN (DISTR-4 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR4-CNT
052614
052614     WHEN (DISTR-5 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR5-CNT
052614
052614     WHEN (DISTR-6 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR6-CNT
052614
052614     WHEN (DISTR-7 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR7-CNT
052614
052614     WHEN (DISTR-8 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR8-CNT
052614
052614     WHEN (DISTR-9 AND
052614           FAM-CLAIM-TYPE)
052614         ADD +1                        TO WS-FAM-DISTR9-CNT
032007
032007
032007********* LIFE BUCKETS
032007     WHEN (DISTR-1 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR1-CNT
032007
032007     WHEN (DISTR-2 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR2-CNT
032007
032007     WHEN (DISTR-3 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR3-CNT
032007
032007     WHEN (DISTR-3A AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR3A-CNT
032007
032007     WHEN (DISTR-4 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR4-CNT
032007
032007     WHEN (DISTR-5 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR5-CNT
032007
032007     WHEN (DISTR-6 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR6-CNT
032007
032007     WHEN (DISTR-7 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR7-CNT
032007
032007     WHEN (DISTR-8 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR8-CNT
032007
032007     WHEN (DISTR-9 AND
032007           LIFE-CLAIM-TYPE)
032007         ADD +1                        TO WS-LIF-DISTR9-CNT
032007     END-EVALUATE
032007
032007
032007
032007******* SERVICE TIME EXCEPTION REPORT
032007     IF DC-ELAPSED-DAYS > WS-ACCEPTABLE-SVC-TIME
032007      
032007         MOVE DC-ELAPSED-DAYS          TO WS-D4-CLM-SVC-TIME
032007         MOVE CL-CLAIM-NO              TO WS-D4-CLAIM-NO
032007         MOVE CL-CERT-NO               TO WS-D4-POLICY-NO
032007
032007         EVALUATE TRUE
032007         WHEN CL-CLAIM-TYPE = 'A'
032007             MOVE WS-DIS-LABEL         TO WS-D4-CLAIM-TYPE
032007
032007         WHEN CL-CLAIM-TYPE = 'G'
032007             MOVE WS-GP-LABEL          TO WS-D4-CLAIM-TYPE
032007
032007         WHEN CL-CLAIM-TYPE = 'I'
032007             MOVE WS-IU-LABEL          TO WS-D4-CLAIM-TYPE
052614
052614         WHEN CL-CLAIM-TYPE = 'F'
052614             MOVE WS-FAM-LABEL         TO WS-D4-CLAIM-TYPE
032007
032007         WHEN CL-CLAIM-TYPE = 'L'
032007             MOVE WS-LIFE-LABEL        TO WS-D4-CLAIM-TYPE
032007         END-EVALUATE
032007
032007         IF PPFORM-PROCESSED-THIS-MO
032007             MOVE WS-HOLD-PP-RECV-DT   TO DC-BIN-DATE-1
032007         ELSE
032007             MOVE WS-HOLD-CLM-RECV-DT  TO DC-BIN-DATE-1
032007         END-IF
032007
032007         MOVE SPACE                    TO DC-OPTION-CODE
032007         PERFORM 8500-DATE-CONVERSION  THRU 8500-EXIT
032007
032007         IF NO-CONVERSION-ERROR
032007             MOVE DC-GREG-DATE-1-EDIT  TO WS-D4-FORM-RCVD-DT
032007         ELSE
032007             DISPLAY 'CONVERSION #2  ERROR IN PARA 2010- ERROR '
032007                                       DC-ERROR-CODE
032007             PERFORM ABEND-PGM         THRU APS-EXIT
032007         END-IF
032007
032007
032007         IF WS-D4-ACTION = 'PAYMENT    '
032007             MOVE WS-HOLD-PP-PAY-DT       TO DC-BIN-DATE-1
032007         ELSE
032007             IF WS-D4-ACTION = '1ST PAYMENT'
032007                 MOVE WS-HOLD-1ST-PAY-DT  TO DC-BIN-DATE-1
032007             ELSE
032007                 MOVE WS-HOLD-CLM-DENY-DT TO DC-BIN-DATE-1
032007             END-IF
032007         END-IF
032007
032007
032007         MOVE SPACE                       TO DC-OPTION-CODE
032007         PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT
032007
032007         IF NO-CONVERSION-ERROR
032007             MOVE DC-GREG-DATE-1-EDIT     TO WS-D4-CLM-ACTION-DT
032007         ELSE
032007             DISPLAY 'CONVERSION #3  ERROR IN PARA 2010- ERROR '
032007                                          DC-ERROR-CODE
032007             PERFORM ABEND-PGM            THRU APS-EXIT
032007         END-IF
032007
032007         IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
032007             MOVE WS-EXCEPTION-RPT-TITLE  TO WS-H1-TITLE
032007             MOVE WS-HEADING1             TO PRT
032007             PERFORM 2800-WRITE           THRU 2800-EXIT
032007
032007             MOVE WS-HEADING2             TO PRT
032007             PERFORM 2800-WRITE           THRU 2800-EXIT
032007
032007             ADD +1                       TO WS-PAGE
032007             MOVE WS-PAGE                 TO WS-H3-PAGE
032007             MOVE WS-HEADING3             TO PRT
032007             PERFORM 2800-WRITE           THRU 2800-EXIT
032007
032007             MOVE WS-HEADING4-EXCEPTIONS  TO PRT
032007             PERFORM 2800-WRITE           THRU 2800-EXIT
032007
032007             MOVE SPACES                  TO PRT
032007             MOVE '-'                     TO P-CTL
032007             PERFORM 2800-WRITE           THRU 2800-EXIT
032007         END-IF
032007
032007         MOVE WS-DETAIL4-EXCEPTIONS       TO PRT
032007         PERFORM 2800-WRITE               THRU 2800-EXIT
032007
032007     END-IF


           .
       2010-EXIT.
           EXIT.

       2075-START-ELTRLR.

           MOVE LOW-VALUES                  TO AT-CONTROL-PRIMARY
           MOVE CL-CONTROL-PRIMARY          TO AT-CONTROL-PRIMARY (1:20)
           START ELTRLR-INFILE KEY NOT < AT-CONTROL-PRIMARY
           END-START

010704     EVALUATE TRUE
010704     WHEN WS-ELTRLR-FILE-STATUS = '00'
010704         CONTINUE

010704     WHEN WS-ELTRLR-FILE-STATUS = '23'
010704         SET END-OF-ELTRLR-ON-START TO TRUE
010704         GO TO 2075-EXIT

010704     WHEN OTHER
010704        DISPLAY ' ELTRLR START ' WS-ELTRLR-FILE-STATUS
              PERFORM ABEND-PGM             THRU APS-EXIT
010704     END-EVALUATE

           SET NEW-ELTRLR-KEY               TO TRUE
           SET LOOKING-FOR-CLM-RECV-DT      TO TRUE
           SET LOOKING-FOR-PP-RECV-DT       TO TRUE
           MOVE SPACES                      TO WS-HOLD-UNDW-RETURN-DT
           PERFORM 2100-READ-ELTRLR         THRU 2100-EXIT
               UNTIL ELTRLR-KEY-CHANGE  OR
                     END-OF-ELTRLR

           .
       2075-EXIT.
           EXIT.


       2100-READ-ELTRLR.

           MOVE SPACE                       TO SORT-MTD-OR-DAILY
           READ ELTRLR-INFILE NEXT RECORD
               AT END
                   SET END-OF-ELTRLR        TO TRUE
                   GO TO 2100-EXIT
           END-READ


           IF AT-CONTROL-PRIMARY (1:20) = CL-CONTROL-PRIMARY
               CONTINUE
           ELSE
               SET ELTRLR-KEY-CHANGE        TO TRUE
               GO TO 2100-EXIT
           END-IF

           MOVE AT-TRAILER-TYPE             TO WS-TRAILER-TYPE
           IF COUNTABLE-ACTIVITY
               CONTINUE
           ELSE
               GO TO 2100-EXIT
           END-IF

           IF (AT-RECORDED-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
               AT-RECORDED-DT <= WS-PREV-CYCLE-DT-BINARY)
               MOVE 'M'                     TO SORT-MTD-OR-DAILY
               MOVE AT-RECORDED-DT          TO SORT-RECORDED-DT
           ELSE
               IF (AT-RECORDED-DT  > WS-PREV-CYCLE-DT-BINARY AND
                   AT-RECORDED-DT <= WS-CYCLE-DT-BINARY)
                   MOVE 'D'                 TO SORT-MTD-OR-DAILY
                   MOVE AT-RECORDED-DT      TO SORT-RECORDED-DT
               END-IF
           END-IF

           MOVE AT-RECORDED-BY              TO SORT-AUDITOR
032007     MOVE +0                          TO SORT-HANDLE-TIME
032007     MOVE 'N'                         TO SORT-HANDLE-COUNTED

           EVALUATE TRUE
           WHEN CORRESPONDENCE-TR
               IF SORT-MTD-OR-DAILY = SPACE
                   PERFORM 2102-CHK-OTHER-CORR-DATES THRU 2102-EXIT
               END-IF

               IF (AT-STD-LETTER-FORM = 'PP  ' AND
                   LOOKING-FOR-PP-RECV-DT)
032007             AND SORT-MTD-OR-DAILY > SPACES                   
                   MOVE AT-LETTER-ANSWERED-DT   TO WS-HOLD-PP-RECV-DT
                   SET ALREADY-HAVE-PP-RECV-DT  TO TRUE
               END-IF

               IF (AT-STD-LETTER-FORM = 'NC  ' AND
                   LOOKING-FOR-CLM-RECV-DT)
                   MOVE AT-LETTER-ANSWERED-DT   TO WS-HOLD-CLM-RECV-DT
                   SET ALREADY-HAVE-CLM-RECV-DT TO TRUE
               END-IF

               IF (AT-STD-LETTER-FORM = 'DC  ' AND
                   LOOKING-FOR-CLM-RECV-DT)
                   MOVE AT-LETTER-ANSWERED-DT   TO WS-HOLD-CLM-RECV-DT
                   SET ALREADY-HAVE-CLM-RECV-DT TO TRUE
               END-IF

               IF SORT-MTD-OR-DAILY = SPACE
                   GO TO 2100-EXIT
               END-IF

               PERFORM 2125-LETTERS-FORMS   THRU 2125-EXIT


           WHEN GENERAL-INFO-TR
               IF SORT-MTD-OR-DAILY = SPACE
                   GO TO 2100-EXIT
               END-IF

               PERFORM 2135-NOTES-CALLS     THRU 2135-EXIT
           

           WHEN PAYMENT-TR 
032007         IF AT-CHECK-WRITTEN-DT = SPACES OR
032007            AT-CHECK-WRITTEN-DT = LOW-VALUES
032007                GO TO 2100-EXIT
032007         END-IF
032007         
               IF (AT-CHECK-WRITTEN-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
                   AT-CHECK-WRITTEN-DT <= WS-PREV-CYCLE-DT-BINARY)
                   MOVE 'M'                 TO SORT-MTD-OR-DAILY
                   MOVE AT-CHECK-WRITTEN-DT TO SORT-RECORDED-DT
               ELSE
                   IF (AT-CHECK-WRITTEN-DT  > WS-PREV-CYCLE-DT-BINARY
                       AND
                       AT-CHECK-WRITTEN-DT <= WS-CYCLE-DT-BINARY)
                       MOVE 'D'             TO SORT-MTD-OR-DAILY
                       MOVE AT-CHECK-WRITTEN-DT TO SORT-RECORDED-DT
                   END-IF
               END-IF

               IF ((AT-VOID-DT = SPACES OR
                   AT-VOID-DT = LOW-VALUES) AND
                   LOOKING-FOR-CLM-RECV-DT)
                   MOVE AT-CHECK-WRITTEN-DT TO WS-HOLD-1ST-PAY-DT
               END-IF

               IF SORT-MTD-OR-DAILY = SPACE
                   PERFORM 2105-CHK-OTHER-PAY-DATES  THRU 2105-EXIT
               END-IF

               IF SORT-MTD-OR-DAILY = SPACE
                   GO TO 2100-EXIT
               END-IF
               
      *         IF ((AT-VOID-DT = SPACES OR
      *             AT-VOID-DT = LOW-VALUES) AND
      *             LOOKING-FOR-PP-RECV-DT)
032007*             MOVE AT-CHECK-WRITTEN-DT TO WS-HOLD-PP-PAY-DT
032007*             MOVE AT-RECORDED-DT TO WS-HOLD-PP-RECV-DT                   
      *         END-IF
               
032007          IF (AT-VOID-DT = SPACES OR
032007             AT-VOID-DT = LOW-VALUES) AND
032007             AT-CHECK-WRITTEN-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
032007             AT-CHECK-WRITTEN-DT <= WS-CYCLE-DT-BINARY
032007               MOVE AT-CHECK-WRITTEN-DT TO WS-HOLD-PP-PAY-DT
032007               MOVE AT-RECORDED-DT TO WS-HOLD-PP-RECV-DT                   
032007         END-IF               
032007
               
               IF AT-PMT-PROOF-DT > SPACES
                   MOVE AT-PMT-PROOF-DT TO WS-HOLD-PP-RECV-DT
                   SET ALREADY-HAVE-PP-RECV-DT TO TRUE
               END-IF

               PERFORM 2145-PAYMENTS        THRU 2145-EXIT


           WHEN DENIAL-TR
032007         IF AT-RETRACTION-DT > SPACES
032007             GO TO 2100-EXIT
032007         END-IF
032007           
               IF SORT-MTD-OR-DAILY = SPACE
                   PERFORM 2108-CHK-OTHER-DENY-DATES THRU 2108-EXIT
               END-IF

               IF SORT-MTD-OR-DAILY = SPACE
                   GO TO 2100-EXIT
               END-IF
052506
052506         IF AT-DENIAL-PROOF-DT > SPACES
052506             MOVE AT-DENIAL-PROOF-DT   TO WS-HOLD-CLM-RECV-DT
052506             SET ALREADY-HAVE-CLM-RECV-DT TO TRUE
052506         END-IF

               PERFORM 2155-DENIALS         THRU 2155-EXIT
           END-EVALUATE

           .
       2100-EXIT.
           EXIT.


       2102-CHK-OTHER-CORR-DATES.

           IF (AT-LETTER-ANSWERED-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
               AT-LETTER-ANSWERED-DT <= WS-PREV-CYCLE-DT-BINARY)
               MOVE 'M'                     TO SORT-MTD-OR-DAILY
               MOVE AT-LETTER-ANSWERED-DT TO SORT-RECORDED-DT
           ELSE
               IF (AT-LETTER-ANSWERED-DT  > WS-PREV-CYCLE-DT-BINARY AND
                   AT-LETTER-ANSWERED-DT <= WS-CYCLE-DT-BINARY)
                   MOVE 'D'                 TO SORT-MTD-OR-DAILY
                   MOVE AT-LETTER-ANSWERED-DT TO SORT-RECORDED-DT
               END-IF
           END-IF

           .
       2102-EXIT.
           EXIT.


       2105-CHK-OTHER-PAY-DATES.

           IF (AT-VOID-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
               AT-VOID-DT <= WS-PREV-CYCLE-DT-BINARY)
               MOVE 'M'                     TO SORT-MTD-OR-DAILY
           ELSE
               IF (AT-VOID-DT  > WS-PREV-CYCLE-DT-BINARY AND
                   AT-VOID-DT <= WS-CYCLE-DT-BINARY)
                   MOVE 'D'                 TO SORT-MTD-OR-DAILY
               END-IF
           END-IF

           .
       2105-EXIT.
           EXIT.



       2108-CHK-OTHER-DENY-DATES.

           IF (AT-DENIAL-LAST-MAINT-DT >= WS-CURR-MNTH-STRT-DT-BIN AND
               AT-DENIAL-LAST-MAINT-DT <= WS-PREV-CYCLE-DT-BINARY)
               MOVE 'M'                     TO SORT-MTD-OR-DAILY
               MOVE AT-DENIAL-LAST-MAINT-DT TO SORT-RECORDED-DT
           ELSE
               IF (AT-DENIAL-LAST-MAINT-DT  > WS-PREV-CYCLE-DT-BINARY
                   AND
                   AT-DENIAL-LAST-MAINT-DT <= WS-CYCLE-DT-BINARY)
                   MOVE 'D'                 TO SORT-MTD-OR-DAILY
                   MOVE AT-DENIAL-LAST-MAINT-DT TO SORT-RECORDED-DT
               END-IF
           END-IF

           .
       2108-EXIT.
           EXIT.


       2125-LETTERS-FORMS.

           
      ************ Medical Records Requested/Reviewed    
           IF AT-STD-LETTER-FORM = 'CI22' OR 'CI74' OR 'HOHI'
               IF AT-LETTER-ANSWERED-DT > SPACES
                   MOVE 'MR'              TO SORT-REC-TYPE
               ELSE
                   MOVE 'MS'              TO SORT-REC-TYPE
               END-IF
               MOVE AT-CORR-LAST-UPDATED-BY TO SORT-AUDITOR
               MOVE AT-CORR-LAST-MAINT-DT TO SORT-FROM-DT
               MOVE AT-CORR-LAST-MAINT-DT TO SORT-RECORDED-DT
               MOVE LOW-VALUES            TO SORT-TO-DT 
               PERFORM 2900-RELEASE-SORT  THRU 2900-EXIT
           END-IF

           MOVE 'L'                            TO SORT-REC-TYPE-1
032007     IF AT-LETTER-ANSWERED-DT > SPACES
      ******* THIS IS UNSOLICITED INCOMING MAIL
             IF AT-LETTER-ANSWERED-DT < AT-RECORDED-DT 
      *     AND
      *         AT-LETTER-ANSWERED-DT > SPACES)
               MOVE 'I'                        TO SORT-REC-TYPE-2
               MOVE AT-LETTER-ANSWERED-DT      TO SORT-FROM-DT
               MOVE AT-RECORDED-DT             TO SORT-TO-DT
               MOVE AT-RECORDED-DT             TO DC-BIN-DATE-2
               PERFORM 2200-CALC-HANDLE-TIME   THRU 2200-EXIT
               PERFORM 2900-RELEASE-SORT       THRU 2900-EXIT
               GO TO 2125-EXIT
             END-IF

      ******* RESPONSE TO A PREVIOUSLY SENT LETTER IS RECEIVED
      ******* CREATE 2 RECS 1 FOR LTR SENT ACTN, 1 FOR LTR RECEIVED ACTN
031407*****changed to only count sent letter if sent in current month
             IF AT-LETTER-ANSWERED-DT >= AT-RECORDED-DT
031407         IF AT-RECORDED-DT >= WS-CURR-MNTH-STRT-DT-BIN                
                   MOVE 'S'                    TO SORT-REC-TYPE-2
                   MOVE AT-RECORDED-DT         TO SORT-FROM-DT
                   MOVE AT-LETTER-ANSWERED-DT  TO SORT-TO-DT
032007             MOVE ZERO                   TO SORT-HANDLE-TIME
032007             MOVE 'Y'                    TO SORT-HANDLE-COUNTED
                   PERFORM 2900-RELEASE-SORT   THRU 2900-EXIT
031407         END-IF

               MOVE AT-CORR-LAST-MAINT-DT      TO DC-BIN-DATE-2
               PERFORM 2200-CALC-HANDLE-TIME   THRU 2200-EXIT

               MOVE AT-CORR-LAST-UPDATED-BY    TO SORT-AUDITOR
               MOVE 'R'                        TO SORT-REC-TYPE-2
               MOVE AT-LETTER-ANSWERED-DT      TO SORT-FROM-DT
               MOVE AT-CORR-LAST-MAINT-DT      TO SORT-TO-DT
               PERFORM 2900-RELEASE-SORT       THRU 2900-EXIT

               GO TO 2125-EXIT
             END-IF
           END-IF

           MOVE ' '                            TO SORT-REC-TYPE-2
           MOVE AT-RECORDED-DT                 TO SORT-FROM-DT
           MOVE LOW-VALUE                      TO SORT-TO-DT 
           PERFORM 2900-RELEASE-SORT           THRU 2900-EXIT

           .
       2125-EXIT.
           EXIT.


       2135-NOTES-CALLS.

           IF AT-CALL-NOTE
               IF AT-PHONE-CALL-IN
                   MOVE 'I '                 TO SORT-REC-TYPE
               ELSE
                   MOVE 'O '                 TO SORT-REC-TYPE
               END-IF
               
               IF AT-CONTINUED-NOTE
                   GO TO 2135-EXIT
               END-IF

               MOVE AT-GEN-INFO-LAST-MAINT-DT TO SORT-FROM-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
               GO TO 2135-EXIT
           END-IF

      ************* SERVICE TIME IS TRACKED FOR UNDERWRITING ACTIVITY
      *************   NOTES PREFIXED BY 'UNWS' = SENT TO UNDERWRITING
      *************   AND 'UNWR' = RETURNED FROM UNDERWRITING           
           IF AT-INFO-LINE-1(1:4) = 'UNWR'
               MOVE AT-RECORDED-DT           TO WS-HOLD-UNDW-RETURN-DT
               MOVE 'U'                      TO SORT-REC-TYPE-1
               MOVE 'R'                      TO SORT-REC-TYPE-2
               MOVE LOW-VALUE                TO SORT-FROM-DT
               MOVE AT-RECORDED-DT           TO SORT-TO-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
               GO TO 2135-EXIT
           END-IF

           IF AT-INFO-LINE-1(1:4) = 'UNWS' 
               MOVE 'U'                      TO SORT-REC-TYPE-1
               MOVE 'S'                      TO SORT-REC-TYPE-2
               MOVE AT-RECORDED-DT           TO SORT-FROM-DT
               MOVE WS-HOLD-UNDW-RETURN-DT   TO SORT-TO-DT
032007         IF WS-HOLD-UNDW-RETURN-DT > SPACES
032007             MOVE AT-RECORDED-DT           TO DC-BIN-DATE-1
032007             MOVE WS-HOLD-UNDW-RETURN-DT   TO DC-BIN-DATE-2
032007             MOVE '1'                      TO DC-OPTION-CODE
032007             PERFORM 8500-DATE-CONVERSION  THRU 8500-EXIT
032007             IF NO-CONVERSION-ERROR
032007                 MOVE DC-ELAPSED-DAYS      TO SORT-HANDLE-TIME
032007                 MOVE 'Y'                  TO SORT-HANDLE-COUNTED                   
032007             ELSE
032007                 DISPLAY 'CONVERSION ERROR IN PARA 2135- ERROR '
032007                                           DC-ERROR-CODE
032007                 PERFORM ABEND-PGM         THRU APS-EXIT
032007             END-IF               
032007         END-IF                                                            
               MOVE SPACES                   TO WS-HOLD-UNDW-RETURN-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
032007         MOVE +0                       TO  SORT-HANDLE-TIME
032007         MOVE 'N'                      TO SORT-HANDLE-COUNTED                   

               GO TO 2135-EXIT
           END-IF
      
      *** Note and File     
           IF AT-NOTE-FILE-NOTE
               MOVE 'NF'                     TO SORT-REC-TYPE
               MOVE AT-GEN-INFO-LAST-MAINT-DT TO SORT-FROM-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
               GO TO 2135-EXIT
           END-IF

           MOVE 'G'                          TO SORT-REC-TYPE-1
           MOVE ' '                          TO SORT-REC-TYPE-2
           MOVE LOW-VALUES                   TO SORT-FROM-DT
                                                SORT-TO-DT
           PERFORM 2900-RELEASE-SORT         THRU 2900-EXIT

           .
       2135-EXIT.
           EXIT.


       2145-PAYMENTS.

           IF CLAIM-COUNTED
               CONTINUE
           ELSE
               IF (AT-CHECK-WRITTEN-DT > SPACES AND
                   (AT-VOID-DT = SPACES OR
                    AT-VOID-DT = LOW-VALUES))
                   MOVE 'C '                 TO SORT-REC-TYPE
                   MOVE AT-CHECK-WRITTEN-DT  TO SORT-FROM-DT
                   MOVE LOW-VALUE            TO SORT-TO-DT
                   MOVE SORT-MTD-OR-DAILY    TO WS-HOLD-SORT-MTD-DLY
                   PERFORM 2900-RELEASE-SORT THRU 2900-EXIT
                display 'payment 1 - ' sort-claim-number
                   SET CLAIM-COUNTED         TO TRUE
               END-IF
           END-IF
           
           IF AT-CHECK-WRITTEN-DT > SPACES AND
                   (AT-VOID-DT = SPACES OR
                    AT-VOID-DT = LOW-VALUES)
               MOVE 'P'                      TO SORT-REC-TYPE-1
               MOVE AT-PAYMENT-TYPE          TO SORT-REC-TYPE-2
               IF AT-PMT-PROOF-DT > SPACES 
                   MOVE AT-PMT-PROOF-DT      TO SORT-FROM-DT
               ELSE
                   MOVE AT-RECORDED-DT       TO SORT-FROM-DT
               END-IF
               MOVE AT-CHECK-WRITTEN-DT      TO SORT-TO-DT
               PERFORM 2900-RELEASE-SORT     THRU 2900-EXIT
                display 'payment 2 - ' sort-claim-number ' ' 
                                       sort-auditor 
               if sort-auditor equal 'AUTO'
                   move 'Y' to ws-auto-pay
               else
                   move 'Y' to ws-non-auto-pay
               end-if
           END-IF
           

           IF (AT-VOID-DT = LOW-VALUES OR
               AT-VOID-DT = SPACES)
               GO TO 2145-EXIT
           ELSE
               IF (AT-CHECK-WRITTEN-DT = SPACES OR
                   AT-CHECK-WRITTEN-DT = LOW-VALUES)
      ************* VOID - CHECK NEVER ISSUED
                   MOVE 'V '                 TO SORT-REC-TYPE
                   MOVE AT-VOID-DT           TO SORT-FROM-DT
                   MOVE LOW-VALUE            TO SORT-TO-DT
               ELSE
      ************* STOP-PAY
                   MOVE 'S '                 TO SORT-REC-TYPE
                   MOVE AT-CHECK-WRITTEN-DT  TO SORT-FROM-DT
                   MOVE AT-VOID-DT           TO SORT-TO-DT
               END-IF

               MOVE AT-PAYMENT-LAST-UPDATED-BY TO SORT-AUDITOR
               MOVE AT-VOID-DT TO SORT-RECORDED-DT
               PERFORM 2900-RELEASE-SORT       THRU 2900-EXIT
           END-IF

           .
       2145-EXIT.
           EXIT.


       2155-DENIALS.

           IF (AT-RETRACTION-DT = SPACES OR
               AT-RETRACTION-DT = LOW-VALUES)
               MOVE 'R '                    TO SORT-REC-TYPE
               MOVE AT-DENIAL-PROOF-DT      TO SORT-FROM-DT
               MOVE AT-RECORDED-DT          TO SORT-TO-DT
               PERFORM 2900-RELEASE-SORT    THRU 2900-EXIT
            display 'denial 1 - ' sort-claim-number
           END-IF

           .
       2155-EXIT.
           EXIT.


       2200-CALC-HANDLE-TIME.

           MOVE AT-LETTER-ANSWERED-DT       TO DC-BIN-DATE-1
           MOVE '1'                         TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-ELAPSED-DAYS         TO SORT-HANDLE-TIME
032206         MOVE 'Y'                     TO SORT-HANDLE-COUNTED
           ELSE
               DISPLAY 'CONVERSION ERROR IN PARA 2200- ERROR '
                                            DC-ERROR-CODE
082302         PERFORM ABEND-PGM            THRU APS-EXIT
082302         GO TO 2200-EXIT
           END-IF

           .
       2200-EXIT.
           EXIT.


       2800-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE


           WRITE PRT

           .
       2800-EXIT.
           EXIT.


       2900-RELEASE-SORT.

           IF SORT-AUDITOR(3:1) = SPACE
               DISPLAY "2 CHAR AUDITOR ID" AT-CONTROL-PRIMARY
           END-IF

           RELEASE SORT-RECORD

      *     MOVE SPACE                        TO SORT-MTD-OR-DAILY
           MOVE LOW-VALUE                    TO SORT-FROM-DT
           MOVE LOW-VALUE                    TO SORT-TO-DT

           .
       2900-EXIT.
           EXIT.




       3000-OUTPUT-PROCEDURE.

           MOVE ZERO                           TO WS-PAGE
           MOVE +55                            TO WS-LINE-COUNT
           MOVE WS-AUDITOR-RPT-TITLE           TO WS-H1-TITLE

           PERFORM 3020-RETURN-SORT            THRU 3020-EXIT
               UNTIL END-OF-SORTFILE

      *     GO TO 3000-EXIT.
           
           
010704     IF WS-SORT-RET-CNT >= +1
122002         PERFORM 3050-MOVE-DLY-FLDS          THRU 3050-EXIT
122002         PERFORM 3100-PRINT-DETAIL1          THRU 3100-EXIT
122002         PERFORM 3055-MOVE-MTD-FLDS          THRU 3055-EXIT
122002         PERFORM 3105-PRINT-DETAIL2          THRU 3105-EXIT
               PERFORM 3200-TOTALS                 THRU 3200-EXIT
               PERFORM 3275-GRAND-TOTALS           THRU 3275-EXIT

032007         PERFORM 3300-PRINT-HANDLE-TIME-RPT  THRU 3300-EXIT
032007         PERFORM 3400-PRINT-SERVICE-TIME-RPT THRU 3400-EXIT
032007         PERFORM 3600-PRINT-DISTRIBUTION-RPT THRU 3600-EXIT

010704     ELSE
010704         PERFORM 3800-PRINT-HEADINGS         THRU 3800-EXIT
010704         MOVE '0NO ACTIVITY FOR EL591 REPORT SERIES' TO PRT
010704         PERFORM 3900-WRITE                  THRU 3900-EXIT
010704     END-IF

           .
       3000-EXIT.
           EXIT.


       3020-RETURN-SORT.

           RETURN SORT-FILE AT END
               SET END-OF-SORTFILE          TO TRUE
               GO TO 3020-EXIT
           END-RETURN.

           MOVE SORT-CLAIM-TYPE TO FILE-CLAIM-TYPE.
           MOVE SORT-AUDITOR    TO FILE-AUDITOR.
           MOVE SORT-MTD-OR-DAILY TO FILE-MTD-OR-DAILY.
           MOVE SORT-CLAIM-NUMBER TO FILE-CLAIM-NUMBER.
           MOVE SORT-CERT-NUMBER TO FILE-CERT-NUMBER.
           MOVE SORT-REC-TYPE TO FILE-REC-TYPE.
032007     MOVE SORT-HANDLE-TIME TO FILE-HANDLE-TIME.
032007     MOVE SORT-HANDLE-COUNTED TO FILE-HANDLE-COUNTED.
           
           
           MOVE SORT-RECORDED-DT   TO DC-BIN-DATE-1
           MOVE ' '                TO DC-OPTION-CODE
           PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

           IF NO-CONVERSION-ERROR
               MOVE DC-GREG-DATE-1-YMD   TO FILE-RECORDED-DT
           ELSE
               MOVE SPACES TO FILE-RECORDED-DT
           END-IF
           
           IF SORT-FROM-DT > ZERO
	            MOVE SORT-FROM-DT          TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION THRU 8500-EXIT

              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-1-YMD   TO FILE-FROM-DT
              ELSE
                  MOVE SPACES TO FILE-FROM-DT
              END-IF
           ELSE
              MOVE SPACES TO FILE-FROM-DT
           END-IF.
           
           IF SORT-TO-DT > ZERO
	            MOVE SORT-TO-DT            TO DC-BIN-DATE-1
              MOVE ' '                   TO DC-OPTION-CODE
              PERFORM 8500-DATE-CONVERSION     THRU 8500-EXIT

              IF NO-CONVERSION-ERROR
                  MOVE DC-GREG-DATE-1-YMD   TO FILE-TO-DT
              ELSE
                  MOVE SPACES TO FILE-TO-DT
              END-IF
           ELSE
               MOVE SPACES TO FILE-TO-DT
           END-IF.

           WRITE FILE-RECORD FROM FILE-REC.
      *     GO TO 3020-EXIT.



010704     ADD +1                           TO WS-SORT-RET-CNT
      ***** TEMPORARY FIX - NEED TO FORCE VALID USER ID (IE. PGM EL162)
           IF SORT-AUDITOR(3:1) = SPACE
               GO TO 3020-EXIT
           END-IF

           IF FIRST-SORT-RECORD
               MOVE SORT-CLAIM-TYPE         TO WS-HOLD-CLAIM-TYPE
               MOVE SORT-AUDITOR            TO WS-HOLD-AUDITOR
               SET NOT-FIRST-RECORD         TO TRUE
           END-IF

           IF SORT-CLAIM-TYPE = WS-HOLD-CLAIM-TYPE
               IF SORT-AUDITOR(1:3) = WS-HOLD-AUDITOR(1:3)
                   CONTINUE
               ELSE
122002             PERFORM 3050-MOVE-DLY-FLDS THRU 3050-EXIT
122002             PERFORM 3100-PRINT-DETAIL1 THRU 3100-EXIT
122002             PERFORM 3055-MOVE-MTD-FLDS THRU 3055-EXIT
122002             PERFORM 3105-PRINT-DETAIL2 THRU 3105-EXIT
                   MOVE SORT-AUDITOR          TO WS-HOLD-AUDITOR
               END-IF
           ELSE
122002         PERFORM 3050-MOVE-DLY-FLDS     THRU 3050-EXIT
122002         PERFORM 3100-PRINT-DETAIL1     THRU 3100-EXIT
122002         PERFORM 3055-MOVE-MTD-FLDS     THRU 3055-EXIT
122002         PERFORM 3105-PRINT-DETAIL2     THRU 3105-EXIT
122002         PERFORM 3200-TOTALS            THRU 3200-EXIT

               MOVE SORT-CLAIM-TYPE           TO WS-HOLD-CLAIM-TYPE
               MOVE SORT-AUDITOR              TO WS-HOLD-AUDITOR
           END-IF

           MOVE SORT-AUDITOR      TO WS-HOLD-AUD
           MOVE SORT-RECORDED-DT  TO WS-HOLD-DATE
           MOVE SORT-CLAIM-NUMBER TO WS-HOLD-CLAIM-NUMBER
           MOVE SORT-CERT-NUMBER  TO WS-HOLD-CERT-NUMBER
           IF SORT-REC-TYPE NOT EQUAL 'G' AND 'I' AND 'O'
              IF WS-HOLD-AUD-CLAIM-CERT NOT EQUAL WS-PREV-AUD-CLAIM-CERT
                   MOVE WS-HOLD-AUD-CLAIM-CERT TO WS-PREV-AUD-CLAIM-CERT
                   IF SORT-MTD-OR-DAILY = 'D'
                       ADD +1 TO WS-DLY-WORK-MEASURE-CNT
                   END-IF
                   ADD +1 TO WS-MTD-WORK-MEASURE-CNT
              END-IF
           END-IF.
           

           EVALUATE TRUE

      ***** NEW CLAIM ENTRIES
           WHEN SORT-REC-TYPE = 'N '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-NEW-CLM-CNT
               END-IF
               ADD +1                       TO WS-MTD-NEW-CLM-CNT

      ***** CLAIMS PAID - NOT COUNTING MULTIPLE DRAFTS
           WHEN SORT-REC-TYPE = 'C '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-PD-CLM-CNT
               END-IF
               ADD +1                       TO WS-MTD-PD-CLM-CNT

      ***** PAYMENTS/DRAFTS ISSUED
           WHEN SORT-REC-TYPE = 'P1'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-PART-DRAFT-CNT
               END-IF
               ADD +1                       TO WS-MTD-PART-DRAFT-CNT

           WHEN SORT-REC-TYPE = 'P2'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-FINAL-DRAFT-CNT
               END-IF
               ADD +1                       TO WS-MTD-FINAL-DRAFT-CNT

           WHEN SORT-REC-TYPE = 'P4'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-ADDIT-DRAFT-CNT
               END-IF
               ADD +1                       TO WS-MTD-ADDIT-DRAFT-CNT

           WHEN SORT-REC-TYPE = 'P3' OR 'P5' OR 'P6' OR 'P9' 
                             OR 'PI' OR 'PT'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-OTHER-DRAFT-CNT
               END-IF
               ADD +1                       TO WS-MTD-OTHER-DRAFT-CNT

      ***** VOIDS - CHECKS NEVER PRINTED
           WHEN SORT-REC-TYPE = 'V'
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-VOID-CNT
               END-IF
               ADD +1                       TO WS-MTD-VOID-CNT

      ***** STOP-PAYS - DRAFTS ALREADY ISSUED
           WHEN SORT-REC-TYPE = 'S'
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-STOPPAY-CNT
               END-IF
               ADD +1                       TO WS-MTD-STOPPAY-CNT
           
      ***** LETTERS/FORMS
           WHEN SORT-REC-TYPE = 'L '
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-LETTER-CNT
               END-IF
122002         ADD +1                       TO WS-MTD-LETTER-CNT

           WHEN SORT-REC-TYPE = 'LS'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-LETTER-CNT
               END-IF
122002         ADD +1                       TO WS-MTD-LETTER-CNT

           WHEN SORT-REC-TYPE = 'LI'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-MAIL-WO-REQ-CNT
               END-IF
122002         ADD +1                       TO WS-MTD-MAIL-WO-REQ-CNT

           WHEN SORT-REC-TYPE = 'LR'
               IF SORT-MTD-OR-DAILY = 'D'
122002             ADD +1                   TO WS-DLY-MAIL-W-REQ-CNT
               END-IF
122002         ADD +1                       TO WS-MTD-MAIL-W-REQ-CNT

      ***** NOTES (INTERNAL)
           WHEN SORT-REC-TYPE = 'G '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-NOTE-CNT
               END-IF
               ADD +1                       TO WS-MTD-NOTE-CNT

           WHEN SORT-REC-TYPE = 'US'
               IF SORT-MTD-OR-DAILY = 'D'
                  ADD +1                    TO WS-DLY-UNWS-CNT
               END-IF
               ADD +1                       TO WS-MTD-UNWS-CNT


           WHEN SORT-REC-TYPE = 'UR'
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-UNWR-CNT
032007             EVALUATE SORT-CLAIM-TYPE 
032007               WHEN 'A'
032007                   ADD +1             TO WS-DLY-DIS-UNW-HANDLES
032007                   ADD SORT-HANDLE-TIME TO WS-DLY-DIS-UNW-HTIME
032007               WHEN 'G'
032007                   ADD +1             TO WS-DLY-GP-UNW-HANDLES
032007                   ADD SORT-HANDLE-TIME TO WS-DLY-GP-UNW-HTIME
032007               WHEN 'I'
032007                   ADD +1             TO WS-DLY-IU-UNW-HANDLES
032007                   ADD SORT-HANDLE-TIME TO WS-DLY-IU-UNW-HTIME
052614               WHEN 'F'
052614                   ADD +1             TO WS-DLY-FAM-UNW-HANDLES
052614                   ADD SORT-HANDLE-TIME TO WS-DLY-FAM-UNW-HTIME
032007               WHEN 'L'
032007                   ADD +1             TO WS-DLY-LIF-UNW-HANDLES
032007                   ADD SORT-HANDLE-TIME TO WS-DLY-LIF-UNW-HTIME
032007             END-EVALUATE
               END-IF
               ADD +1                       TO WS-MTD-UNWR-CNT
032007         EVALUATE SORT-CLAIM-TYPE 
032007            WHEN 'A'
032007                ADD +1                TO WS-MTD-DIS-UNW-HANDLES
032007                ADD SORT-HANDLE-TIME  TO WS-MTD-DIS-UNW-HTIME
032007            WHEN 'G'
032007                ADD +1                TO WS-MTD-GP-UNW-HANDLES
032007                ADD SORT-HANDLE-TIME  TO WS-MTD-GP-UNW-HTIME
032007            WHEN 'I'
032007                ADD +1                TO WS-MTD-IU-UNW-HANDLES
032007                ADD SORT-HANDLE-TIME  TO WS-MTD-IU-UNW-HTIME
052614            WHEN 'F'
052614                ADD +1                TO WS-MTD-FAM-UNW-HANDLES
052614                ADD SORT-HANDLE-TIME  TO WS-MTD-FAM-UNW-HTIME
032007            WHEN 'L'
032007                ADD +1                TO WS-MTD-LIF-UNW-HANDLES
032007                ADD SORT-HANDLE-TIME  TO WS-MTD-LIF-UNW-HTIME
032007         END-EVALUATE

           WHEN SORT-REC-TYPE = 'MR'
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-MED-REC-CNT
               END-IF
               ADD +1                       TO WS-MTD-MED-REC-CNT

           WHEN SORT-REC-TYPE = 'NF'
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-NOTE-FILE-CNT
               END-IF
               ADD +1                       TO WS-MTD-NOTE-FILE-CNT

      ***** INCOMING CALLS
           WHEN SORT-REC-TYPE = 'I '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-CALLIN-CNT
               END-IF
               ADD +1                       TO WS-MTD-CALLIN-CNT

      ***** OUTGOING CALLS
           WHEN SORT-REC-TYPE = 'O '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-CALLOUT-CNT
               END-IF
               ADD +1                       TO WS-MTD-CALLOUT-CNT

      ***** DENIALS
           WHEN SORT-REC-TYPE = 'R '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-DENIAL-CNT
               END-IF
               ADD +1                       TO WS-MTD-DENIAL-CNT

      ***** DROPS/AUTO OR MANUAL CLOSINGS
           WHEN SORT-REC-TYPE-1 = 'D'
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-DROP-CNT
               END-IF
               ADD +1                       TO WS-MTD-DROP-CNT

      ***** REOPENS
           WHEN SORT-REC-TYPE = 'X '
               IF SORT-MTD-OR-DAILY = 'D'
                   ADD +1                   TO WS-DLY-REOPEN-CNT
               END-IF
               ADD +1                       TO WS-MTD-REOPEN-CNT

           END-EVALUATE

032007******* REC TYPE 'U' (UNDERWRITING) SERVICE TIME IS INCLUDED IN
032007******* THE DEPARTMENT TOTAL
032007
032007     IF SORT-HANDLE-COUNTED = 'Y'
032007         EVALUATE TRUE
032007         WHEN SORT-MTD-OR-DAILY = 'D' AND
032007              SORT-CLAIM-TYPE = 'A'
032007             ADD +1                   TO WS-DLY-DIS-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-DLY-DIS-HANDLE-TIME
032007             ADD +1                   TO WS-MTD-DIS-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-DIS-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'M' AND
032007              SORT-CLAIM-TYPE = 'A'
032007             ADD +1                   TO WS-MTD-DIS-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-DIS-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'D' AND
032007              SORT-CLAIM-TYPE = 'G'
032007             ADD +1                   TO WS-DLY-GP-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-DLY-GP-HANDLE-TIME
032007             ADD +1                   TO WS-MTD-GP-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-GP-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'M' AND
032007              SORT-CLAIM-TYPE = 'G'
032007             ADD +1                   TO WS-MTD-GP-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-GP-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'D' AND
032007              SORT-CLAIM-TYPE = 'I'
032007             ADD +1                   TO WS-DLY-IU-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-DLY-IU-HANDLE-TIME
032007             ADD +1                   TO WS-MTD-IU-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-IU-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'M' AND
032007              SORT-CLAIM-TYPE = 'I'
032007             ADD +1                   TO WS-MTD-IU-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-IU-HANDLE-TIME
052614
052614         WHEN SORT-MTD-OR-DAILY = 'D' AND
052614              SORT-CLAIM-TYPE = 'F'
052614             ADD +1                   TO WS-DLY-FAM-SVC-HANDLES
052614             ADD SORT-HANDLE-TIME     TO WS-DLY-FAM-HANDLE-TIME
052614             ADD +1                   TO WS-MTD-FAM-SVC-HANDLES
052614             ADD SORT-HANDLE-TIME     TO WS-MTD-FAM-HANDLE-TIME
052614
052614         WHEN SORT-MTD-OR-DAILY = 'M' AND
052614              SORT-CLAIM-TYPE = 'F'
052614             ADD +1                   TO WS-MTD-FAM-SVC-HANDLES
052614             ADD SORT-HANDLE-TIME     TO WS-MTD-FAM-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'D' AND
032007              SORT-CLAIM-TYPE = 'L'
032007             ADD +1                   TO WS-DLY-LIF-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-DLY-LIF-HANDLE-TIME
032007             ADD +1                   TO WS-MTD-LIF-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-LIF-HANDLE-TIME
032007
032007         WHEN SORT-MTD-OR-DAILY = 'M' AND
032007              SORT-CLAIM-TYPE = 'L'
032007             ADD +1                   TO WS-MTD-LIF-SVC-HANDLES
032007             ADD SORT-HANDLE-TIME     TO WS-MTD-LIF-HANDLE-TIME
032007         END-EVALUATE
032007     END-IF

           .
       3020-EXIT.
           EXIT.


       3050-MOVE-DLY-FLDS.

           INITIALIZE WS-DETAIL1
           MOVE WS-EDITED-CYCLE-DT          TO DET-HEAD-DATE
           MOVE WS-HOLD-AUDITOR             TO DET-HEAD-AUDITOR
           EVALUATE TRUE
               WHEN WS-HOLD-CLAIM-TYPE = 'A'
                   MOVE WS-DIS-LABEL         TO DET-HEAD-COV

               WHEN WS-HOLD-CLAIM-TYPE = 'G'
                   MOVE WS-GP-LABEL          TO DET-HEAD-COV

               WHEN WS-HOLD-CLAIM-TYPE = 'I'
                   MOVE WS-IU-LABEL          TO DET-HEAD-COV
052614
052614         WHEN WS-HOLD-CLAIM-TYPE = 'F'
052614             MOVE WS-FAM-LABEL         TO DET-HEAD-COV

               WHEN WS-HOLD-CLAIM-TYPE = 'L'
                   MOVE WS-LIFE-LABEL        TO DET-HEAD-COV
           END-EVALUATE

           MOVE 'DAY'                       TO WS-D1-DAY-MTD
           MOVE WS-DLY-WORK-MEASURE-CNT     TO WS-D1-WORK-MEASURE-CNT
           MOVE WS-DLY-NEW-CLM-CNT          TO WS-D1-NEW-CLM-CNT
           MOVE WS-DLY-PD-CLM-CNT           TO WS-D1-PD-CLM-CNT
           MOVE WS-DLY-PART-DRAFT-CNT       TO WS-D1-PART-DRAFT-CNT
           MOVE WS-DLY-FINAL-DRAFT-CNT      TO WS-D1-FINAL-DRAFT-CNT
           MOVE WS-DLY-ADDIT-DRAFT-CNT      TO WS-D1-ADDIT-DRAFT-CNT
           MOVE WS-DLY-OTHER-DRAFT-CNT      TO WS-D1-OTHER-DRAFT-CNT
           MOVE WS-DLY-VOID-CNT             TO WS-D1-VOID-CNT
           MOVE WS-DLY-STOPPAY-CNT          TO WS-D1-STOPPAY-CNT
           MOVE WS-DLY-LETTER-CNT           TO WS-D1-LETTER-CNT
           MOVE WS-DLY-MAIL-WO-REQ-CNT      TO WS-D1-MAIL-WO-REQ-CNT
           MOVE WS-DLY-MAIL-W-REQ-CNT       TO WS-D1-MAIL-W-REQ-CNT
           MOVE WS-DLY-NOTE-CNT             TO WS-D1-NOTE-CNT
           MOVE WS-DLY-CALLIN-CNT           TO WS-D1-CALLIN-CNT
           MOVE WS-DLY-CALLOUT-CNT          TO WS-D1-CALLOUT-CNT
           MOVE WS-DLY-DENIAL-CNT           TO WS-D1-DENIAL-CNT
           MOVE WS-DLY-UNWS-CNT             TO WS-D1-UNWS-CNT
           MOVE WS-DLY-UNWR-CNT             TO WS-D1-UNWR-CNT
           MOVE WS-DLY-MED-REC-CNT          TO WS-D1-MED-REC-CNT
           MOVE WS-DLY-NOTE-FILE-CNT        TO WS-D1-NOTE-FILE-CNT
           MOVE WS-DLY-DROP-CNT             TO WS-D1-DROP-CNT
           MOVE WS-DLY-REOPEN-CNT           TO WS-D1-REOPEN-CNT

           ADD WS-DLY-WORK-MEASURE-CNT      TO WS-TOTD-WORK-MEASURE-CNT
           ADD WS-DLY-NEW-CLM-CNT           TO WS-TOTD-NEW-CLM-CNT
           ADD WS-DLY-PD-CLM-CNT            TO WS-TOTD-PD-CLM-CNT
           ADD WS-DLY-PART-DRAFT-CNT        TO WS-TOTD-PART-DRAFT-CNT
           ADD WS-DLY-FINAL-DRAFT-CNT       TO WS-TOTD-FINAL-DRAFT-CNT
           ADD WS-DLY-ADDIT-DRAFT-CNT       TO WS-TOTD-ADDIT-DRAFT-CNT
           ADD WS-DLY-OTHER-DRAFT-CNT       TO WS-TOTD-OTHER-DRAFT-CNT
           ADD WS-DLY-VOID-CNT              TO WS-TOTD-VOID-CNT
           ADD WS-DLY-STOPPAY-CNT           TO WS-TOTD-STOPPAY-CNT
           ADD WS-DLY-MAIL-WO-REQ-CNT       TO WS-TOTD-MAIL-WO-REQ-CNT
           ADD WS-DLY-MAIL-W-REQ-CNT        TO WS-TOTD-MAIL-W-REQ-CNT
           ADD WS-DLY-LETTER-CNT            TO WS-TOTD-LETTER-CNT
           ADD WS-DLY-NOTE-CNT              TO WS-TOTD-NOTE-CNT
           ADD WS-DLY-CALLIN-CNT            TO WS-TOTD-CALLIN-CNT
           ADD WS-DLY-CALLOUT-CNT           TO WS-TOTD-CALLOUT-CNT
           ADD WS-DLY-DENIAL-CNT            TO WS-TOTD-DENIAL-CNT
           ADD WS-DLY-DROP-CNT              TO WS-TOTD-DROP-CNT
           ADD WS-DLY-REOPEN-CNT            TO WS-TOTD-REOPEN-CNT
           ADD WS-DLY-UNWS-CNT              TO WS-TOTD-UNWS-CNT
           ADD WS-DLY-UNWR-CNT              TO WS-TOTD-UNWR-CNT
           ADD WS-DLY-MED-REC-CNT           TO WS-TOTD-MED-REC-CNT
           ADD WS-DLY-NOTE-FILE-CNT         TO WS-TOTD-NOTE-FILE-CNT

           INITIALIZE WS-DLY-COUNTS

           .
       3050-EXIT.
           EXIT.


       3055-MOVE-MTD-FLDS.

           INITIALIZE WS-DETAIL1

           MOVE 'MTD'                       TO WS-D1-DAY-MTD
           MOVE WS-MTD-WORK-MEASURE-CNT     TO WS-D1-WORK-MEASURE-CNT
           MOVE WS-MTD-NEW-CLM-CNT          TO WS-D1-NEW-CLM-CNT
           MOVE WS-MTD-PD-CLM-CNT           TO WS-D1-PD-CLM-CNT
           MOVE WS-MTD-PART-DRAFT-CNT       TO WS-D1-PART-DRAFT-CNT
           MOVE WS-MTD-FINAL-DRAFT-CNT      TO WS-D1-FINAL-DRAFT-CNT
           MOVE WS-MTD-ADDIT-DRAFT-CNT      TO WS-D1-ADDIT-DRAFT-CNT
           MOVE WS-MTD-OTHER-DRAFT-CNT      TO WS-D1-OTHER-DRAFT-CNT
           MOVE WS-MTD-VOID-CNT             TO WS-D1-VOID-CNT
           MOVE WS-MTD-STOPPAY-CNT          TO WS-D1-STOPPAY-CNT
           MOVE WS-MTD-MAIL-WO-REQ-CNT      TO WS-D1-MAIL-WO-REQ-CNT
           MOVE WS-MTD-MAIL-W-REQ-CNT       TO WS-D1-MAIL-W-REQ-CNT
           MOVE WS-MTD-LETTER-CNT           TO WS-D1-LETTER-CNT
           MOVE WS-MTD-NOTE-CNT             TO WS-D1-NOTE-CNT
           MOVE WS-MTD-CALLIN-CNT           TO WS-D1-CALLIN-CNT
           MOVE WS-MTD-CALLOUT-CNT          TO WS-D1-CALLOUT-CNT
           MOVE WS-MTD-DENIAL-CNT           TO WS-D1-DENIAL-CNT
           MOVE WS-MTD-UNWS-CNT             TO WS-D1-UNWS-CNT
           MOVE WS-MTD-UNWR-CNT             TO WS-D1-UNWR-CNT
           MOVE WS-MTD-MED-REC-CNT          TO WS-D1-MED-REC-CNT
           MOVE WS-MTD-NOTE-FILE-CNT        TO WS-D1-NOTE-FILE-CNT
           MOVE WS-MTD-DROP-CNT             TO WS-D1-DROP-CNT
           MOVE WS-MTD-REOPEN-CNT           TO WS-D1-REOPEN-CNT

           ADD WS-MTD-WORK-MEASURE-CNT      TO WS-TOTM-WORK-MEASURE-CNT
           ADD WS-MTD-NEW-CLM-CNT           TO WS-TOTM-NEW-CLM-CNT
           ADD WS-MTD-PD-CLM-CNT            TO WS-TOTM-PD-CLM-CNT
           ADD WS-MTD-PART-DRAFT-CNT        TO WS-TOTM-PART-DRAFT-CNT
           ADD WS-MTD-FINAL-DRAFT-CNT       TO WS-TOTM-FINAL-DRAFT-CNT
           ADD WS-MTD-ADDIT-DRAFT-CNT       TO WS-TOTM-ADDIT-DRAFT-CNT
           ADD WS-MTD-OTHER-DRAFT-CNT       TO WS-TOTM-OTHER-DRAFT-CNT
           ADD WS-MTD-VOID-CNT              TO WS-TOTM-VOID-CNT
           ADD WS-MTD-STOPPAY-CNT           TO WS-TOTM-STOPPAY-CNT
           ADD WS-MTD-MAIL-WO-REQ-CNT       TO WS-TOTM-MAIL-WO-REQ-CNT
           ADD WS-MTD-MAIL-W-REQ-CNT        TO WS-TOTM-MAIL-W-REQ-CNT
           ADD WS-MTD-LETTER-CNT            TO WS-TOTM-LETTER-CNT
           ADD WS-MTD-NOTE-CNT              TO WS-TOTM-NOTE-CNT
           ADD WS-MTD-CALLIN-CNT            TO WS-TOTM-CALLIN-CNT
           ADD WS-MTD-CALLOUT-CNT           TO WS-TOTM-CALLOUT-CNT
           ADD WS-MTD-DENIAL-CNT            TO WS-TOTM-DENIAL-CNT
           ADD WS-MTD-DROP-CNT              TO WS-TOTM-DROP-CNT
           ADD WS-MTD-REOPEN-CNT            TO WS-TOTM-REOPEN-CNT
           ADD WS-MTD-UNWS-CNT              TO WS-TOTM-UNWS-CNT
           ADD WS-MTD-UNWR-CNT              TO WS-TOTM-UNWR-CNT
           ADD WS-MTD-MED-REC-CNT           TO WS-TOTM-MED-REC-CNT
           ADD WS-MTD-NOTE-FILE-CNT         TO WS-TOTM-NOTE-FILE-CNT

           INITIALIZE WS-MTD-COUNTS

           .
       3055-EXIT.
           EXIT.


       3100-PRINT-DETAIL1.

           IF WS-LINE-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 3800-PRINT-HEADINGS  THRU 3800-EXIT
               MOVE WS-HEADING4             TO PRT
               PERFORM 3900-WRITE           THRU 3900-EXIT
               MOVE WS-HEADING5             TO PRT
               PERFORM 3900-WRITE           THRU 3900-EXIT
           END-IF

           MOVE WS-DET-HEADING              TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT
           MOVE WS-DETAIL1                  TO PRT
           MOVE ' '                         TO P-CTL
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3100-EXIT.
           EXIT.


       3105-PRINT-DETAIL2.

           MOVE WS-DETAIL1                  TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3105-EXIT.
           EXIT.


122002 3200-TOTALS.

122002     EVALUATE TRUE
122002     WHEN WS-HOLD-CLAIM-TYPE = 'A'
122002         MOVE WS-AUD-RPT-DIS-SUBT-HDR TO PRT

121603     WHEN WS-HOLD-CLAIM-TYPE = 'G'
121603         MOVE WS-AUD-RPT-GP-SUBT-HDR TO PRT

122002     WHEN WS-HOLD-CLAIM-TYPE = 'I'
122002         MOVE WS-AUD-RPT-IU-SUBT-HDR TO PRT
052614
052614     WHEN WS-HOLD-CLAIM-TYPE = 'F'
052614         MOVE WS-AUD-RPT-FAM-SUBT-HDR TO PRT

122002     WHEN WS-HOLD-CLAIM-TYPE = 'L'
122002         MOVE WS-AUD-RPT-LIF-SUBT-HDR TO PRT
122002     END-EVALUATE

           PERFORM 3900-WRITE               THRU 3900-EXIT

           INITIALIZE WS-DETAIL1
           MOVE WS-EDITED-CYCLE-DT          TO WS-LABEL-VARIABLE-2
      *     MOVE WS-REPORT-LABEL             TO WS-TOTALS-LABEL

           MOVE 'DAY'                       TO WS-D1-DAY-MTD
           MOVE WS-TOTD-WORK-MEASURE-CNT    TO WS-D1-WORK-MEASURE-CNT
           MOVE WS-TOTD-NEW-CLM-CNT         TO WS-D1-NEW-CLM-CNT
           MOVE WS-TOTD-PD-CLM-CNT          TO WS-D1-PD-CLM-CNT
           MOVE WS-TOTD-PART-DRAFT-CNT      TO WS-D1-PART-DRAFT-CNT
           MOVE WS-TOTD-FINAL-DRAFT-CNT     TO WS-D1-FINAL-DRAFT-CNT
           MOVE WS-TOTD-ADDIT-DRAFT-CNT     TO WS-D1-ADDIT-DRAFT-CNT
           MOVE WS-TOTD-OTHER-DRAFT-CNT     TO WS-D1-OTHER-DRAFT-CNT
           MOVE WS-TOTD-VOID-CNT            TO WS-D1-VOID-CNT
           MOVE WS-TOTD-STOPPAY-CNT         TO WS-D1-STOPPAY-CNT
           MOVE WS-TOTD-MAIL-WO-REQ-CNT     TO WS-D1-MAIL-WO-REQ-CNT
           MOVE WS-TOTD-MAIL-W-REQ-CNT      TO WS-D1-MAIL-W-REQ-CNT
           MOVE WS-TOTD-LETTER-CNT          TO WS-D1-LETTER-CNT
           MOVE WS-TOTD-NOTE-CNT            TO WS-D1-NOTE-CNT
           MOVE WS-TOTD-CALLIN-CNT          TO WS-D1-CALLIN-CNT
           MOVE WS-TOTD-CALLOUT-CNT         TO WS-D1-CALLOUT-CNT
           MOVE WS-TOTD-DENIAL-CNT          TO WS-D1-DENIAL-CNT
           MOVE WS-TOTD-DROP-CNT            TO WS-D1-DROP-CNT
           MOVE WS-TOTD-REOPEN-CNT          TO WS-D1-REOPEN-CNT
           MOVE WS-TOTD-UNWS-CNT            TO WS-D1-UNWS-CNT
           MOVE WS-TOTD-UNWR-CNT            TO WS-D1-UNWR-CNT
           MOVE WS-TOTD-MED-REC-CNT         TO WS-D1-MED-REC-CNT
           MOVE WS-TOTD-NOTE-FILE-CNT       TO WS-D1-NOTE-FILE-CNT
           MOVE WS-DETAIL1                  TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT


           INITIALIZE WS-DETAIL1
           MOVE 'MTD'                       TO WS-D1-DAY-MTD
           MOVE WS-TOTM-WORK-MEASURE-CNT    TO WS-D1-WORK-MEASURE-CNT
           ADD WS-TOTM-WORK-MEASURE-CNT     TO WS-GRAND-TOT-WORK-MEASURE

           MOVE WS-TOTM-NEW-CLM-CNT         TO WS-D1-NEW-CLM-CNT
           ADD WS-TOTM-NEW-CLM-CNT          TO WS-GRAND-TOT-NEW-CLM-CNT

           MOVE WS-TOTM-PD-CLM-CNT          TO WS-D1-PD-CLM-CNT
           ADD WS-TOTM-PD-CLM-CNT           TO WS-GRAND-TOT-PD-CLM-CNT

           MOVE WS-TOTM-PART-DRAFT-CNT      TO WS-D1-PART-DRAFT-CNT
           ADD WS-TOTM-PART-DRAFT-CNT       TO 
                                  WS-GRAND-TOT-PART-DRAFT-CNT
           
           MOVE WS-TOTM-FINAL-DRAFT-CNT     TO WS-D1-FINAL-DRAFT-CNT
           ADD WS-TOTM-FINAL-DRAFT-CNT      TO 
                                   WS-GRAND-TOT-FINAL-DRAFT-CNT
           
           MOVE WS-TOTM-ADDIT-DRAFT-CNT     TO WS-D1-ADDIT-DRAFT-CNT
           ADD WS-TOTM-ADDIT-DRAFT-CNT      TO 
                                   WS-GRAND-TOT-ADDIT-DRAFT-CNT

           MOVE WS-TOTM-OTHER-DRAFT-CNT     TO WS-D1-OTHER-DRAFT-CNT
           ADD WS-TOTM-OTHER-DRAFT-CNT      TO 
                                   WS-GRAND-TOT-OTHER-DRAFT-CNT

           MOVE WS-TOTM-VOID-CNT            TO WS-D1-VOID-CNT
           ADD WS-TOTM-VOID-CNT             TO WS-GRAND-TOT-VOID-CNT

           MOVE WS-TOTM-STOPPAY-CNT         TO WS-D1-STOPPAY-CNT
           ADD WS-TOTM-STOPPAY-CNT          TO WS-GRAND-TOT-STOPPAY-CNT
           
           MOVE WS-TOTM-MAIL-WO-REQ-CNT     TO WS-D1-MAIL-WO-REQ-CNT
           ADD WS-TOTM-MAIL-WO-REQ-CNT      TO 
                                   WS-GRAND-TOT-MAIL-WO-REQ-CNT
           
           MOVE WS-TOTM-MAIL-W-REQ-CNT      TO WS-D1-MAIL-W-REQ-CNT
           ADD WS-TOTM-MAIL-W-REQ-CNT       TO 
                                   WS-GRAND-TOT-MAIL-W-REQ-CNT

           MOVE WS-TOTM-LETTER-CNT          TO WS-D1-LETTER-CNT
           ADD WS-TOTM-LETTER-CNT           TO WS-GRAND-TOT-LETTER-CNT

           MOVE WS-TOTM-NOTE-CNT            TO WS-D1-NOTE-CNT
           ADD WS-TOTM-NOTE-CNT             TO WS-GRAND-TOT-NOTE-CNT

           MOVE WS-TOTM-CALLIN-CNT          TO WS-D1-CALLIN-CNT
           ADD WS-TOTM-CALLIN-CNT           TO WS-GRAND-TOT-CALLIN-CNT

           MOVE WS-TOTM-CALLOUT-CNT         TO WS-D1-CALLOUT-CNT
           ADD WS-TOTM-CALLOUT-CNT          TO WS-GRAND-TOT-CALLOUT-CNT

           MOVE WS-TOTM-DENIAL-CNT          TO WS-D1-DENIAL-CNT
           ADD WS-TOTM-DENIAL-CNT           TO WS-GRAND-TOT-DENIAL-CNT

           MOVE WS-TOTM-DROP-CNT            TO WS-D1-DROP-CNT
           ADD WS-TOTM-DROP-CNT             TO WS-GRAND-TOT-DROP-CNT

           MOVE WS-TOTM-REOPEN-CNT          TO WS-D1-REOPEN-CNT
           ADD WS-TOTM-REOPEN-CNT           TO WS-GRAND-TOT-REOPEN-CNT
           
           MOVE WS-TOTM-UNWS-CNT            TO WS-D1-UNWS-CNT
           ADD WS-TOTM-UNWS-CNT             TO WS-GRAND-TOT-UNWS-CNT

           MOVE WS-TOTM-UNWR-CNT            TO WS-D1-UNWR-CNT
           ADD WS-TOTM-UNWR-CNT             TO WS-GRAND-TOT-UNWR-CNT

           MOVE WS-TOTM-MED-REC-CNT         TO WS-D1-MED-REC-CNT
           ADD WS-TOTM-MED-REC-CNT          TO WS-GRAND-TOT-MED-REC-CNT

           MOVE WS-TOTM-NOTE-FILE-CNT       TO WS-D1-NOTE-FILE-CNT
           ADD WS-TOTM-NOTE-FILE-CNT      TO WS-GRAND-TOT-NOTE-FILE-CNT

           MOVE WS-MTD-LITERAL              TO WS-LABEL-VARIABLE-2
      *     MOVE WS-REPORT-LABEL             TO WS-TOTALS-LABEL
           MOVE WS-DETAIL1                  TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE SPACES                      TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

122002     INITIALIZE WS-TOTD-COUNTS
122002     INITIALIZE WS-TOTM-COUNTS

           .
       3200-EXIT.
           EXIT.


       3275-GRAND-TOTALS.

           INITIALIZE WS-DETAIL1
           MOVE WS-GRAND-TOTALS-LABEL       TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE 'TOT'                       TO WS-D1-DAY-MTD
           MOVE WS-GRAND-TOT-WORK-MEASURE   TO WS-D1-WORK-MEASURE-CNT
           MOVE WS-GRAND-TOT-NEW-CLM-CNT    TO WS-D1-NEW-CLM-CNT
           MOVE WS-GRAND-TOT-PD-CLM-CNT     TO WS-D1-PD-CLM-CNT
           MOVE WS-GRAND-TOT-PART-DRAFT-CNT TO WS-D1-PART-DRAFT-CNT
           MOVE WS-GRAND-TOT-FINAL-DRAFT-CNT TO WS-D1-FINAL-DRAFT-CNT
           MOVE WS-GRAND-TOT-ADDIT-DRAFT-CNT TO WS-D1-ADDIT-DRAFT-CNT
           MOVE WS-GRAND-TOT-OTHER-DRAFT-CNT TO WS-D1-OTHER-DRAFT-CNT
           MOVE WS-GRAND-TOT-VOID-CNT       TO WS-D1-VOID-CNT
           MOVE WS-GRAND-TOT-STOPPAY-CNT    TO WS-D1-STOPPAY-CNT
           MOVE WS-GRAND-TOT-MAIL-WO-REQ-CNT TO WS-D1-MAIL-WO-REQ-CNT
           MOVE WS-GRAND-TOT-MAIL-W-REQ-CNT TO WS-D1-MAIL-W-REQ-CNT
           MOVE WS-GRAND-TOT-LETTER-CNT     TO WS-D1-LETTER-CNT
           MOVE WS-GRAND-TOT-NOTE-CNT       TO WS-D1-NOTE-CNT
           MOVE WS-GRAND-TOT-CALLIN-CNT     TO WS-D1-CALLIN-CNT
           MOVE WS-GRAND-TOT-CALLOUT-CNT    TO WS-D1-CALLOUT-CNT
           MOVE WS-GRAND-TOT-DENIAL-CNT     TO WS-D1-DENIAL-CNT
           MOVE WS-GRAND-TOT-DROP-CNT       TO WS-D1-DROP-CNT
           MOVE WS-GRAND-TOT-REOPEN-CNT     TO WS-D1-REOPEN-CNT
           MOVE WS-GRAND-TOT-UNWS-CNT       TO WS-D1-UNWS-CNT
           MOVE WS-GRAND-TOT-UNWR-CNT       TO WS-D1-UNWR-CNT
           MOVE WS-GRAND-TOT-MED-REC-CNT    TO WS-D1-MED-REC-CNT
           MOVE WS-GRAND-TOT-NOTE-FILE-CNT  TO WS-D1-NOTE-FILE-CNT
           MOVE WS-DETAIL1                  TO PRT
           MOVE ' '                         TO P-CTL
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3275-EXIT.
           EXIT.


032007 3300-PRINT-HANDLE-TIME-RPT.
032007
032007
032007     MOVE WS-HANDLE-TIME-RPT-TITLE    TO WS-H1-TITLE
032007     MOVE ZERO                        TO WS-PAGE
032007     PERFORM 3800-PRINT-HEADINGS      THRU 3800-EXIT
032007
032007     MOVE WS-HEADING4-HANDLE-TIME     TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE WS-HEADING5-HANDLE-TIME     TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE SPACES                      TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007***** DISABILITY - LINE 1  DAILY FIGURES
032007     INITIALIZE WS-DETAIL2-HANDLE-TIME
032007     MOVE WS-EDITED-CYCLE-DT          TO WS-LABEL-VARIABLE-1
032007     MOVE WS-DIS-LABEL                TO WS-LABEL-VARIABLE-2
032007     MOVE WS-REPORT-LABEL             TO WS-D2-COL1-LABEL
032007     MOVE WS-DLY-DIS-SVC-HANDLES      TO WS-D2-TOT-SVC-HANDLES
032007     MOVE WS-DLY-DIS-HANDLE-TIME      TO WS-D2-TOT-HANDLE-TIME
032007
032007     IF WS-DLY-DIS-SVC-HANDLES NOT= +0
032007         DIVIDE WS-DLY-DIS-HANDLE-TIME
032007             BY WS-DLY-DIS-SVC-HANDLES
032007             GIVING WS-AVG-DLY-DIS-HANDLE-TIME
032007         MOVE WS-AVG-DLY-DIS-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007     END-IF
032007
032007     MOVE WS-DLY-DIS-UNW-HANDLES      TO WS-D2-UNDWRITG-HANDLES
032007     MOVE WS-DLY-DIS-UNW-HTIME        TO WS-D2-UNDWRITG-HTIME
032007
032007     IF WS-DLY-DIS-UNW-HANDLES NOT= +0
032007         DIVIDE WS-DLY-DIS-UNW-HTIME
032007             BY WS-DLY-DIS-UNW-HANDLES
032007             GIVING WS-AVG-DLY-DIS-UNW-HTIME
032007         MOVE WS-AVG-DLY-DIS-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007     END-IF
032007
032007     MOVE WS-DETAIL2-HANDLE-TIME      TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007***** DISABILITY - LINE 2  MONTH-TO-DATE FIGURES
032007     INITIALIZE WS-DETAIL2-HANDLE-TIME
032007     MOVE SPACES                      TO WS-LABEL-VARIABLE-1
032007     MOVE WS-MTD-LITERAL              TO WS-LABEL-VARIABLE-2
032007     MOVE WS-REPORT-LABEL             TO WS-D2-COL1-LABEL
032007     MOVE WS-MTD-DIS-SVC-HANDLES      TO WS-D2-TOT-SVC-HANDLES
032007     MOVE WS-MTD-DIS-HANDLE-TIME      TO WS-D2-TOT-HANDLE-TIME
032007     IF WS-MTD-DIS-SVC-HANDLES NOT= +0
032007         DIVIDE WS-MTD-DIS-HANDLE-TIME
032007             BY WS-MTD-DIS-SVC-HANDLES
032007             GIVING WS-AVG-MTD-DIS-HANDLE-TIME
032007         MOVE WS-AVG-MTD-DIS-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007     END-IF
032007
032007     MOVE WS-MTD-DIS-UNW-HANDLES      TO WS-D2-UNDWRITG-HANDLES
032007     MOVE WS-MTD-DIS-UNW-HTIME        TO WS-D2-UNDWRITG-HTIME
032007     IF WS-MTD-DIS-UNW-HANDLES NOT= +0
032007         DIVIDE WS-MTD-DIS-UNW-HTIME
032007             BY WS-MTD-DIS-UNW-HANDLES
032007             GIVING WS-AVG-MTD-DIS-UNW-HTIME
032007         MOVE WS-AVG-MTD-DIS-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007     END-IF
032007
032007     MOVE WS-DETAIL2-HANDLE-TIME      TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007     IF DTE-CLIENT = 'DCC'
032007***** SECURE PAY (GAP) - DAILY FIGURES
032007         INITIALIZE WS-DETAIL2-HANDLE-TIME
032007         MOVE WS-EDITED-CYCLE-DT      TO WS-LABEL-VARIABLE-1
032007         MOVE WS-GP-LABEL             TO WS-LABEL-VARIABLE-2
032007         MOVE WS-REPORT-LABEL         TO WS-D2-COL1-LABEL
032007         MOVE WS-DLY-GP-SVC-HANDLES   TO WS-D2-TOT-SVC-HANDLES
032007         MOVE WS-DLY-GP-HANDLE-TIME   TO WS-D2-TOT-HANDLE-TIME
032007
032007         IF WS-DLY-GP-SVC-HANDLES NOT= +0
032007             DIVIDE WS-DLY-GP-HANDLE-TIME
032007                 BY WS-DLY-GP-SVC-HANDLES
032007                 GIVING WS-AVG-DLY-GP-HANDLE-TIME
032007             MOVE WS-AVG-DLY-GP-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007         END-IF
032007
032007         MOVE WS-DLY-GP-UNW-HANDLES   TO WS-D2-UNDWRITG-HANDLES
032007         MOVE WS-DLY-GP-UNW-HTIME     TO WS-D2-UNDWRITG-HTIME
032007  
032007         IF WS-DLY-GP-UNW-HANDLES NOT= +0
032007             DIVIDE WS-DLY-GP-UNW-HTIME
032007                 BY WS-DLY-GP-UNW-HANDLES
032007                 GIVING WS-AVG-DLY-GP-UNW-HTIME
032007             MOVE WS-AVG-DLY-GP-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007         END-IF
032007
032007         MOVE WS-DETAIL2-HANDLE-TIME  TO PRT
032007         MOVE '-'                     TO P-CTL
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
032007
032007
032007***** SECURE PAY (GAP) - MONTH-TO-DATE FIGURES
032007         INITIALIZE WS-DETAIL2-HANDLE-TIME
032007         MOVE SPACES                  TO WS-LABEL-VARIABLE-1
032007         MOVE WS-MTD-LITERAL          TO WS-LABEL-VARIABLE-2
032007         MOVE WS-REPORT-LABEL         TO WS-D2-COL1-LABEL
032007         MOVE WS-MTD-GP-SVC-HANDLES   TO WS-D2-TOT-SVC-HANDLES 
032007         MOVE WS-MTD-GP-HANDLE-TIME   TO WS-D2-TOT-HANDLE-TIME
032007         IF WS-MTD-GP-SVC-HANDLES NOT= +0
032007             DIVIDE WS-MTD-GP-HANDLE-TIME
032007                 BY WS-MTD-GP-SVC-HANDLES
032007                 GIVING WS-AVG-MTD-GP-HANDLE-TIME
032007             MOVE WS-AVG-MTD-GP-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007         END-IF
032007
032007         MOVE WS-MTD-GP-UNW-HANDLES   TO WS-D2-UNDWRITG-HANDLES
032007         MOVE WS-MTD-GP-UNW-HTIME     TO WS-D2-UNDWRITG-HTIME
032007         IF WS-MTD-GP-UNW-HANDLES NOT= +0
032007             DIVIDE WS-MTD-GP-UNW-HTIME
032007                 BY WS-MTD-GP-UNW-HANDLES
032007                 GIVING WS-AVG-MTD-GP-UNW-HTIME
032007             MOVE WS-AVG-MTD-GP-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007         END-IF
032007
032007         MOVE WS-DETAIL2-HANDLE-TIME  TO PRT
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
032007
032007***** UNEMPLOYMENT - DAILY FIGURES
032007         INITIALIZE WS-DETAIL2-HANDLE-TIME
032007         MOVE WS-EDITED-CYCLE-DT      TO WS-LABEL-VARIABLE-1
032007         MOVE WS-IU-LABEL             TO WS-LABEL-VARIABLE-2
032007         MOVE WS-REPORT-LABEL         TO WS-D2-COL1-LABEL
032007         MOVE WS-DLY-IU-SVC-HANDLES   TO WS-D2-TOT-SVC-HANDLES
032007         MOVE WS-DLY-IU-HANDLE-TIME   TO WS-D2-TOT-HANDLE-TIME
032007
032007         IF WS-DLY-IU-SVC-HANDLES NOT= +0
032007             DIVIDE WS-DLY-IU-HANDLE-TIME
032007                 BY WS-DLY-IU-SVC-HANDLES
032007                 GIVING WS-AVG-DLY-IU-HANDLE-TIME
032007             MOVE WS-AVG-DLY-IU-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007         END-IF
032007
032007         MOVE WS-DLY-IU-UNW-HANDLES   TO WS-D2-UNDWRITG-HANDLES
032007         MOVE WS-DLY-IU-UNW-HTIME     TO WS-D2-UNDWRITG-HTIME
032007
032007         IF WS-DLY-IU-UNW-HANDLES NOT= +0
032007             DIVIDE WS-DLY-IU-UNW-HTIME
032007                 BY WS-DLY-IU-UNW-HANDLES
032007                 GIVING WS-AVG-DLY-IU-UNW-HTIME
032007             MOVE WS-AVG-DLY-IU-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007         END-IF
032007
032007         MOVE WS-DETAIL2-HANDLE-TIME  TO PRT
032007         MOVE '-'                     TO P-CTL
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
032007
032007
032007***** UNEMPLOYMENT - MONTH-TO-DATE FIGURES
032007         INITIALIZE WS-DETAIL2-HANDLE-TIME
032007         MOVE SPACES                  TO WS-LABEL-VARIABLE-1
032007         MOVE WS-MTD-LITERAL          TO WS-LABEL-VARIABLE-2
032007         MOVE WS-REPORT-LABEL         TO WS-D2-COL1-LABEL
032007         MOVE WS-MTD-IU-SVC-HANDLES   TO WS-D2-TOT-SVC-HANDLES 
032007         MOVE WS-MTD-IU-HANDLE-TIME   TO WS-D2-TOT-HANDLE-TIME
032007         IF WS-MTD-IU-SVC-HANDLES NOT= +0
032007             DIVIDE WS-MTD-IU-HANDLE-TIME
032007                 BY WS-MTD-IU-SVC-HANDLES
032007                 GIVING WS-AVG-MTD-IU-HANDLE-TIME
032007             MOVE WS-AVG-MTD-IU-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007         END-IF
032007
032007         MOVE WS-MTD-IU-UNW-HANDLES   TO WS-D2-UNDWRITG-HANDLES
032007         MOVE WS-MTD-IU-UNW-HTIME     TO WS-D2-UNDWRITG-HTIME
032007         IF WS-MTD-IU-UNW-HANDLES NOT= +0
032007             DIVIDE WS-MTD-IU-UNW-HTIME
032007                 BY WS-MTD-IU-UNW-HANDLES
032007                 GIVING WS-AVG-MTD-IU-UNW-HTIME
032007             MOVE WS-AVG-MTD-IU-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007         END-IF
032007
032007         MOVE WS-DETAIL2-HANDLE-TIME  TO PRT
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
052614
052614***** FAMILY LEAVE (FAM) - DAILY FIGURES
052614         INITIALIZE WS-DETAIL2-HANDLE-TIME
052614         MOVE WS-EDITED-CYCLE-DT      TO WS-LABEL-VARIABLE-1
052614         MOVE WS-FAM-LABEL            TO WS-LABEL-VARIABLE-2
052614         MOVE WS-REPORT-LABEL         TO WS-D2-COL1-LABEL
052614         MOVE WS-DLY-FAM-SVC-HANDLES  TO WS-D2-TOT-SVC-HANDLES
052614         MOVE WS-DLY-FAM-HANDLE-TIME  TO WS-D2-TOT-HANDLE-TIME
052614
052614         IF WS-DLY-FAM-SVC-HANDLES NOT= +0
052614             DIVIDE WS-DLY-FAM-HANDLE-TIME
052614                 BY WS-DLY-FAM-SVC-HANDLES
052614                 GIVING WS-AVG-DLY-FAM-HANDLE-TIME
052614             MOVE WS-AVG-DLY-FAM-HANDLE-TIME
052614                                      TO WS-D2-AVERAGE-HANDLE-TIME
052614         END-IF
052614
052614         MOVE WS-DLY-FAM-UNW-HANDLES  TO WS-D2-UNDWRITG-HANDLES
052614         MOVE WS-DLY-FAM-UNW-HTIME    TO WS-D2-UNDWRITG-HTIME
052614  
052614         IF WS-DLY-FAM-UNW-HANDLES NOT= +0
052614             DIVIDE WS-DLY-FAM-UNW-HTIME
052614                 BY WS-DLY-FAM-UNW-HANDLES
052614                 GIVING WS-AVG-DLY-FAM-UNW-HTIME
052614             MOVE WS-AVG-DLY-FAM-UNW-HTIME
052614                                      TO WS-D2-UNDWRITG-AVG-HTIME
052614         END-IF
052614
052614         MOVE WS-DETAIL2-HANDLE-TIME  TO PRT
052614         MOVE '-'                     TO P-CTL
052614         PERFORM 3900-WRITE           THRU 3900-EXIT
052614
052614
052614***** FAMILY LEAVE (FAM) - MONTH-TO-DATE FIGURES
052614         INITIALIZE WS-DETAIL2-HANDLE-TIME
052614         MOVE SPACES                  TO WS-LABEL-VARIABLE-1
052614         MOVE WS-MTD-LITERAL          TO WS-LABEL-VARIABLE-2
052614         MOVE WS-REPORT-LABEL         TO WS-D2-COL1-LABEL
052614         MOVE WS-MTD-FAM-SVC-HANDLES  TO WS-D2-TOT-SVC-HANDLES 
052614         MOVE WS-MTD-FAM-HANDLE-TIME  TO WS-D2-TOT-HANDLE-TIME
052614         IF WS-MTD-FAM-SVC-HANDLES NOT= +0
052614             DIVIDE WS-MTD-FAM-HANDLE-TIME
052614                 BY WS-MTD-FAM-SVC-HANDLES
052614                 GIVING WS-AVG-MTD-FAM-HANDLE-TIME
052614             MOVE WS-AVG-MTD-FAM-HANDLE-TIME
052614                                      TO WS-D2-AVERAGE-HANDLE-TIME
052614         END-IF
052614
052614         MOVE WS-MTD-FAM-UNW-HANDLES TO WS-D2-UNDWRITG-HANDLES
052614         MOVE WS-MTD-FAM-UNW-HTIME   TO WS-D2-UNDWRITG-HTIME
052614         IF WS-MTD-FAM-UNW-HANDLES NOT= +0
052614             DIVIDE WS-MTD-FAM-UNW-HTIME
052614                 BY WS-MTD-FAM-UNW-HANDLES
052614                 GIVING WS-AVG-MTD-FAM-UNW-HTIME
052614             MOVE WS-AVG-MTD-FAM-UNW-HTIME
052614                                      TO WS-D2-UNDWRITG-AVG-HTIME
052614         END-IF
052614
052614         MOVE WS-DETAIL2-HANDLE-TIME  TO PRT
052614         PERFORM 3900-WRITE           THRU 3900-EXIT
052614
032007     END-IF
032007
032007
032007***** LIFE - LINE 3        DAILY FIGURES
032007     INITIALIZE WS-DETAIL2-HANDLE-TIME
032007     MOVE WS-EDITED-CYCLE-DT          TO WS-LABEL-VARIABLE-1
032007     MOVE WS-LIFE-LABEL               TO WS-LABEL-VARIABLE-2
032007     MOVE WS-REPORT-LABEL             TO WS-D2-COL1-LABEL
032007     MOVE WS-DLY-LIF-SVC-HANDLES      TO WS-D2-TOT-SVC-HANDLES
032007     MOVE WS-DLY-LIF-HANDLE-TIME      TO WS-D2-TOT-HANDLE-TIME
032007
032007     IF WS-DLY-LIF-SVC-HANDLES NOT= +0
032007         DIVIDE WS-DLY-LIF-HANDLE-TIME
032007             BY WS-DLY-LIF-SVC-HANDLES
032007             GIVING WS-AVG-DLY-LIF-HANDLE-TIME
032007         MOVE WS-AVG-DLY-LIF-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007     END-IF
032007
032007     MOVE WS-DLY-LIF-UNW-HANDLES      TO WS-D2-UNDWRITG-HANDLES
032007     MOVE WS-DLY-LIF-UNW-HTIME        TO WS-D2-UNDWRITG-HTIME
032007
032007     IF WS-DLY-LIF-UNW-HANDLES NOT= +0
032007         DIVIDE WS-DLY-LIF-UNW-HTIME
032007             BY WS-DLY-LIF-UNW-HANDLES
032007             GIVING WS-AVG-DLY-LIF-UNW-HTIME
032007         MOVE WS-AVG-DLY-LIF-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007     END-IF
032007
032007     MOVE WS-DETAIL2-HANDLE-TIME      TO PRT
032007     MOVE '-'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007***** LIFE - LINE 4        MONTH-TO-DATE FIGURES
032007     INITIALIZE WS-DETAIL2-HANDLE-TIME
032007     MOVE SPACES                      TO WS-LABEL-VARIABLE-1
032007     MOVE WS-MTD-LITERAL              TO WS-LABEL-VARIABLE-2
032007     MOVE WS-REPORT-LABEL             TO WS-D2-COL1-LABEL
032007     MOVE WS-MTD-LIF-SVC-HANDLES      TO WS-D2-TOT-SVC-HANDLES
032007     MOVE WS-MTD-LIF-HANDLE-TIME      TO WS-D2-TOT-HANDLE-TIME
032007
032007     IF WS-MTD-LIF-SVC-HANDLES NOT= +0
032007         DIVIDE WS-MTD-LIF-HANDLE-TIME
032007             BY WS-MTD-LIF-SVC-HANDLES
032007             GIVING WS-AVG-MTD-LIF-HANDLE-TIME
032007         MOVE WS-AVG-MTD-LIF-HANDLE-TIME
032007                                      TO WS-D2-AVERAGE-HANDLE-TIME
032007     END-IF
032007
032007     MOVE WS-MTD-LIF-UNW-HANDLES      TO WS-D2-UNDWRITG-HANDLES
032007     MOVE WS-MTD-LIF-UNW-HTIME        TO WS-D2-UNDWRITG-HTIME
032007
032007     IF WS-MTD-LIF-UNW-HANDLES NOT= +0
032007         DIVIDE WS-MTD-LIF-UNW-HTIME
032007             BY WS-MTD-LIF-UNW-HANDLES
032007             GIVING WS-AVG-MTD-LIF-UNW-HTIME
032007         MOVE WS-AVG-MTD-LIF-UNW-HTIME
032007                                      TO WS-D2-UNDWRITG-AVG-HTIME
032007     END-IF
032007
032007     MOVE WS-DETAIL2-HANDLE-TIME      TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007
032007***** DAILY GRAND TOTALS - LINE 5
032007     INITIALIZE WS-DETAIL2-HANDLE-TIME
032007     MOVE WS-EDITED-CYCLE-DT          TO WS-LABEL-VARIABLE-1
032007     MOVE WS-DLY-GRANDTOT-LABEL       TO WS-LABEL-VARIABLE-2
032007     MOVE WS-REPORT-LABEL             TO WS-D2-COL1-LABEL
032007
032007     ADD WS-DLY-LIF-SVC-HANDLES, WS-DLY-DIS-SVC-HANDLES,
032007         WS-DLY-IU-SVC-HANDLES, WS-DLY-GP-SVC-HANDLES
052614         WS-DLY-FAM-SVC-HANDLES
032007             GIVING WS-GTOT-SVC-HANDLES
032007
032007     ADD WS-DLY-LIF-HANDLE-TIME, WS-DLY-DIS-HANDLE-TIME,
032007         WS-DLY-IU-HANDLE-TIME, WS-DLY-GP-HANDLE-TIME
052614         WS-DLY-FAM-HANDLE-TIME
032007             GIVING WS-GTOT-HANDLE-TIME
032007
032007     MOVE WS-GTOT-SVC-HANDLES         TO WS-D2-TOT-SVC-HANDLES
032007     MOVE WS-GTOT-HANDLE-TIME         TO WS-D2-TOT-HANDLE-TIME
032007
032007     IF WS-GTOT-SVC-HANDLES NOT= +0
032007         DIVIDE WS-GTOT-HANDLE-TIME
032007             BY WS-GTOT-SVC-HANDLES
032007             GIVING WS-GTOT-AVG-HANDLE-TIME
032007         MOVE WS-GTOT-AVG-HANDLE-TIME TO WS-D2-AVERAGE-HANDLE-TIME
032007     END-IF
032007
032007     ADD WS-DLY-LIF-UNW-HANDLES, WS-DLY-DIS-UNW-HANDLES,
032007         WS-DLY-IU-UNW-HANDLES, WS-DLY-GP-UNW-HANDLES
052614         WS-DLY-FAM-UNW-HANDLES
032007             GIVING WS-GTOT-UNW-HANDLES
032007
032007     ADD WS-DLY-LIF-UNW-HTIME, WS-DLY-DIS-UNW-HTIME,
032007         WS-DLY-IU-UNW-HTIME, WS-DLY-GP-UNW-HTIME
052614         WS-DLY-FAM-UNW-HTIME
032007             GIVING WS-GTOT-UNW-HTIME
032007
032007     MOVE WS-GTOT-UNW-HANDLES         TO WS-D2-UNDWRITG-HANDLES
032007     MOVE WS-GTOT-UNW-HTIME           TO WS-D2-UNDWRITG-HTIME
032007
032007     IF WS-GTOT-UNW-HANDLES NOT= +0
032007         DIVIDE WS-GTOT-UNW-HTIME
032007             BY WS-GTOT-UNW-HANDLES
032007             GIVING WS-GTOT-AVG-UNW-HTIME
032007         MOVE WS-GTOT-AVG-UNW-HTIME   TO WS-D2-UNDWRITG-AVG-HTIME
032007     END-IF
032007
032007     MOVE SPACES                      TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE WS-DETAIL2-HANDLE-TIME      TO PRT
032007     MOVE '-'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007
032007***** MONTH-TO-DATE GRAND TOTALS - LINE 6
032007     INITIALIZE WS-DETAIL2-HANDLE-TIME
032007     MOVE SPACES                      TO WS-LABEL-VARIABLE-1
032007     MOVE WS-MTD-GRANDTOT-LABEL       TO WS-LABEL-VARIABLE-2
032007     MOVE WS-REPORT-LABEL             TO WS-D2-COL1-LABEL
032007
032007     ADD WS-MTD-LIF-SVC-HANDLES, WS-MTD-DIS-SVC-HANDLES,
032007         WS-MTD-IU-SVC-HANDLES, WS-MTD-GP-SVC-HANDLES
052614         WS-MTD-FAM-SVC-HANDLES
032007             GIVING WS-GTOT-SVC-HANDLES
032007
032007     ADD WS-MTD-LIF-HANDLE-TIME, WS-MTD-DIS-HANDLE-TIME,
032007         WS-MTD-IU-HANDLE-TIME, WS-MTD-GP-HANDLE-TIME
052614         WS-MTD-FAM-HANDLE-TIME
032007             GIVING WS-GTOT-HANDLE-TIME
032007
032007     MOVE WS-GTOT-SVC-HANDLES         TO WS-D2-TOT-SVC-HANDLES
032007     MOVE WS-GTOT-HANDLE-TIME         TO WS-D2-TOT-HANDLE-TIME
032007
032007     IF WS-GTOT-SVC-HANDLES NOT= +0
032007         DIVIDE WS-GTOT-HANDLE-TIME
032007             BY WS-GTOT-SVC-HANDLES
032007             GIVING WS-GTOT-AVG-HANDLE-TIME
032007         MOVE WS-GTOT-AVG-HANDLE-TIME TO WS-D2-AVERAGE-HANDLE-TIME
032007     END-IF
032007
032007     ADD WS-MTD-LIF-UNW-HANDLES, WS-MTD-DIS-UNW-HANDLES,
032007         WS-MTD-IU-UNW-HANDLES, WS-MTD-GP-UNW-HANDLES
052614         WS-MTD-FAM-UNW-HANDLES
032007             GIVING WS-GTOT-UNW-HANDLES
032007
032007     ADD WS-MTD-LIF-UNW-HTIME, WS-MTD-DIS-UNW-HTIME,
032007         WS-MTD-IU-UNW-HTIME, WS-MTD-GP-UNW-HTIME
052614         WS-MTD-FAM-UNW-HTIME
032007             GIVING WS-GTOT-UNW-HTIME
032007
032007     MOVE WS-GTOT-UNW-HANDLES         TO WS-D2-UNDWRITG-HANDLES
032007     MOVE WS-GTOT-UNW-HTIME           TO WS-D2-UNDWRITG-HTIME
032007
032007     IF WS-GTOT-UNW-HANDLES NOT= +0
032007         DIVIDE WS-GTOT-UNW-HTIME
032007             BY WS-GTOT-UNW-HANDLES
032007             GIVING WS-GTOT-AVG-UNW-HTIME
032007         MOVE WS-GTOT-AVG-UNW-HTIME   TO WS-D2-UNDWRITG-AVG-HTIME
032007     END-IF
032007
032007     MOVE WS-DETAIL2-HANDLE-TIME      TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     .
032007 3300-EXIT.
032007     EXIT.
032007
032007
032007 3400-PRINT-SERVICE-TIME-RPT.
032007
032007     MOVE WS-SERVICE-TIME-RPT-TITLE   TO WS-H1-TITLE
032007     MOVE ZERO                        TO WS-PAGE
032007     PERFORM 3800-PRINT-HEADINGS      THRU 3800-EXIT
032007
032007     MOVE WS-HEADING4-SERVICE-TIME    TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE WS-HEADING5-SERVICE-TIME    TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE SPACE                       TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     INITIALIZE WS-DETAIL3-SERVICE-TIME
032007     MOVE WS-DIS-LABEL                TO WS-D3-COL1-LABEL
032007     MOVE WS-DIS-PAID-DENIED-CNT      TO WS-D3-PAID-DENIED-CNT
032007     MOVE WS-MTD-DIS-SERVICE-TIME     TO WS-D3-TOT-SVC-TIME
032007
032007     IF WS-DIS-PAID-DENIED-CNT NOT= +0
032007         DIVIDE WS-MTD-DIS-SERVICE-TIME
032007             BY WS-DIS-PAID-DENIED-CNT
032007             GIVING WS-AVG-DIS-SERVICE-TIME
032007         MOVE WS-AVG-DIS-SERVICE-TIME
032007                                      TO WS-D3-AVG-SVC-TIME
032007     END-IF
032007
032007     MOVE WS-DETAIL3-SERVICE-TIME     TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007     IF DTE-CLIENT = 'DCC'
032007         INITIALIZE WS-DETAIL3-SERVICE-TIME
032007         MOVE WS-GP-LABEL             TO WS-D3-COL1-LABEL
032007         MOVE WS-GP-PAID-DENIED-CNT   TO WS-D3-PAID-DENIED-CNT
032007         MOVE WS-MTD-GP-SERVICE-TIME  TO WS-D3-TOT-SVC-TIME
032007
032007         IF WS-GP-PAID-DENIED-CNT NOT= +0
032007             DIVIDE WS-MTD-GP-SERVICE-TIME
032007                 BY WS-GP-PAID-DENIED-CNT
032007                 GIVING WS-AVG-GP-SERVICE-TIME
032007             MOVE WS-AVG-GP-SERVICE-TIME
032007                                      TO WS-D3-AVG-SVC-TIME
032007         END-IF
032007
032007         MOVE WS-DETAIL3-SERVICE-TIME TO PRT
032007         MOVE '0'                     TO P-CTL
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
032007
032007         INITIALIZE WS-DETAIL3-SERVICE-TIME
032007         MOVE WS-IU-LABEL             TO WS-D3-COL1-LABEL
032007         MOVE WS-IU-PAID-DENIED-CNT   TO WS-D3-PAID-DENIED-CNT
032007         MOVE WS-MTD-IU-SERVICE-TIME  TO WS-D3-TOT-SVC-TIME
032007
032007         IF WS-IU-PAID-DENIED-CNT NOT= +0
032007             DIVIDE WS-MTD-IU-SERVICE-TIME
032007                 BY WS-IU-PAID-DENIED-CNT
032007                 GIVING WS-AVG-IU-SERVICE-TIME
032007             MOVE WS-AVG-IU-SERVICE-TIME
032007                                      TO WS-D3-AVG-SVC-TIME
032007         END-IF
032007
032007         MOVE WS-DETAIL3-SERVICE-TIME TO PRT
032007         MOVE '0'                     TO P-CTL
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
032007     END-IF
052614
052614         INITIALIZE WS-DETAIL3-SERVICE-TIME
052614         MOVE WS-FAM-LABEL            TO WS-D3-COL1-LABEL
052614         MOVE WS-FAM-PAID-DENIED-CNT  TO WS-D3-PAID-DENIED-CNT
052614         MOVE WS-MTD-FAM-SERVICE-TIME TO WS-D3-TOT-SVC-TIME
052614
052614         IF WS-FAM-PAID-DENIED-CNT NOT= +0
052614             DIVIDE WS-MTD-FAM-SERVICE-TIME
052614                 BY WS-FAM-PAID-DENIED-CNT
052614                 GIVING WS-AVG-FAM-SERVICE-TIME
052614             MOVE WS-AVG-FAM-SERVICE-TIME
052614                                      TO WS-D3-AVG-SVC-TIME
052614         END-IF
052614
052614         MOVE WS-DETAIL3-SERVICE-TIME TO PRT
052614         MOVE '0'                     TO P-CTL
052614         PERFORM 3900-WRITE           THRU 3900-EXIT
032007
032007
032007     INITIALIZE WS-DETAIL3-SERVICE-TIME
032007     MOVE WS-LIFE-LABEL               TO WS-D3-COL1-LABEL
032007     MOVE WS-LIF-PAID-DENIED-CNT      TO WS-D3-PAID-DENIED-CNT
032007     MOVE WS-MTD-LIF-SERVICE-TIME     TO WS-D3-TOT-SVC-TIME
032007
032007     IF WS-LIF-PAID-DENIED-CNT NOT= +0
032007         DIVIDE WS-MTD-LIF-SERVICE-TIME
032007             BY WS-LIF-PAID-DENIED-CNT
032007             GIVING WS-AVG-LIF-SERVICE-TIME
032007         MOVE WS-AVG-LIF-SERVICE-TIME
032007                                      TO WS-D3-AVG-SVC-TIME
032007     END-IF
032007
032007     MOVE WS-DETAIL3-SERVICE-TIME     TO PRT
032007     MOVE '0'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007
032007     INITIALIZE WS-DETAIL3-SERVICE-TIME
032007     MOVE WS-GRAND-TOTALS-LABEL(1:19) TO WS-D3-COL1-LABEL
032007     ADD WS-DIS-PAID-DENIED-CNT
032007         WS-GP-PAID-DENIED-CNT  WS-IU-PAID-DENIED-CNT           
052614         WS-FAM-PAID-DENIED-CNT
032007         TO WS-LIF-PAID-DENIED-CNT
032007         GIVING WS-TOT-PAID-DENIED-CNT
032007     MOVE WS-TOT-PAID-DENIED-CNT      TO WS-D3-PAID-DENIED-CNT
032007
032007     ADD WS-MTD-DIS-SERVICE-TIME
032007         WS-MTD-GP-SERVICE-TIME  WS-MTD-IU-SERVICE-TIME           
052614         WS-MTD-FAM-SERVICE-TIME
032007         TO WS-MTD-LIF-SERVICE-TIME
032007         GIVING WS-TOT-SERVICE-TIME
032007     MOVE WS-TOT-SERVICE-TIME         TO WS-D3-TOT-SVC-TIME
032007
032007     IF WS-TOT-PAID-DENIED-CNT NOT= +0
032007         DIVIDE WS-TOT-SERVICE-TIME
032007             BY WS-TOT-PAID-DENIED-CNT
032007             GIVING WS-AVG-SERVICE-TIME
032007         MOVE WS-AVG-SERVICE-TIME
032007                                      TO WS-D3-AVG-SVC-TIME
032007     END-IF
032007
032007     MOVE WS-DETAIL3-SERVICE-TIME     TO PRT
032007     MOVE '-'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     .
032007 3400-EXIT.
032007     EXIT.
032007
032007
032007 3600-PRINT-DISTRIBUTION-RPT.
032007
032007     ADD WS-DIS-DISTR1-CNT, WS-LIF-DISTR1-CNT, WS-IU-DISTR1-CNT,
032007         WS-GP-DISTR1-CNT
052614         WS-FAM-DISTR1-CNT
032007         GIVING WS-TOT-DISTR1-CNT
032007     ADD WS-DIS-DISTR2-CNT, WS-LIF-DISTR2-CNT, WS-IU-DISTR2-CNT,
032007         WS-GP-DISTR2-CNT
052614         WS-FAM-DISTR2-CNT
032007         GIVING WS-TOT-DISTR2-CNT
032007     ADD WS-DIS-DISTR3-CNT, WS-LIF-DISTR3-CNT, WS-IU-DISTR3-CNT,
032007         WS-GP-DISTR3-CNT
052614         WS-FAM-DISTR3-CNT
032007         GIVING WS-TOT-DISTR3-CNT
032007     ADD WS-DIS-DISTR3A-CNT, WS-LIF-DISTR3A-CNT,
032007         WS-IU-DISTR3A-CNT, WS-GP-DISTR3A-CNT
052614         WS-FAM-DISTR3A-CNT
032007         GIVING WS-TOT-DISTR3A-CNT
032007     ADD WS-DIS-DISTR4-CNT, WS-LIF-DISTR4-CNT, WS-IU-DISTR4-CNT,
032007         WS-GP-DISTR4-CNT
052614         WS-FAM-DISTR4-CNT
032007         GIVING WS-TOT-DISTR4-CNT
032007     ADD WS-DIS-DISTR5-CNT, WS-LIF-DISTR5-CNT, WS-IU-DISTR5-CNT,  
032007         WS-GP-DISTR5-CNT
052614         WS-FAM-DISTR5-CNT
032007         GIVING WS-TOT-DISTR5-CNT
032007     ADD WS-DIS-DISTR6-CNT, WS-LIF-DISTR6-CNT, WS-IU-DISTR6-CNT,
032007         WS-GP-DISTR6-CNT
052614         WS-FAM-DISTR6-CNT
032007         GIVING WS-TOT-DISTR6-CNT
032007     ADD WS-DIS-DISTR7-CNT, WS-LIF-DISTR7-CNT, WS-IU-DISTR7-CNT,
032007         WS-GP-DISTR7-CNT
052614         WS-FAM-DISTR7-CNT
032007         GIVING WS-TOT-DISTR7-CNT
032007     ADD WS-DIS-DISTR8-CNT, WS-LIF-DISTR8-CNT, WS-IU-DISTR8-CNT,
032007         WS-GP-DISTR8-CNT
052614         WS-FAM-DISTR8-CNT
032007         GIVING WS-TOT-DISTR8-CNT
032007     ADD WS-DIS-DISTR9-CNT, WS-LIF-DISTR9-CNT, WS-IU-DISTR9-CNT,
032007         WS-GP-DISTR9-CNT
052614         WS-FAM-DISTR9-CNT
032007         GIVING WS-TOT-DISTR9-CNT
032007     ADD WS-DIS-DISTR10-CNT, WS-LIF-DISTR10-CNT, 
032007         WS-IU-DISTR10-CNT, WS-GP-DISTR10-CNT
052614         WS-FAM-DISTR10-CNT
032007         GIVING WS-TOT-DISTR10-CNT
032007
032007     MOVE WS-DISTRIBUTION-RPT-TITLE   TO WS-H1-TITLE
032007     MOVE ZERO                        TO WS-PAGE
032007     PERFORM 3800-PRINT-HEADINGS      THRU 3800-EXIT
032007
032007     MOVE WS-HEADING4-DISTRIBUTION    TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE WS-HEADING5-DISTRIBUTION    TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE SPACE                       TO PRT
032007     MOVE '-'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE WS-DIS-LABEL                TO WS-D5-COL1-LABEL
032007     MOVE WS-DIS-DISTR1-CNT           TO WS-D5-DISTR-1
032007     MOVE WS-DIS-DISTR2-CNT           TO WS-D5-DISTR-2
032007     MOVE WS-DIS-DISTR3-CNT           TO WS-D5-DISTR-3
032007     MOVE WS-DIS-DISTR3A-CNT          TO WS-D5-DISTR-3A
032007     MOVE WS-DIS-DISTR4-CNT           TO WS-D5-DISTR-4
032007     MOVE WS-DIS-DISTR5-CNT           TO WS-D5-DISTR-5
032007     MOVE WS-DIS-DISTR6-CNT           TO WS-D5-DISTR-6
032007     MOVE WS-DIS-DISTR7-CNT           TO WS-D5-DISTR-7
032007     MOVE WS-DIS-DISTR8-CNT           TO WS-D5-DISTR-8
032007     MOVE WS-DIS-DISTR9-CNT           TO WS-D5-DISTR-9
032007
032007     MOVE WS-DETAIL5-DISTRIBUTION     TO PRT
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     IF DTE-CLIENT = 'DCC'
032007         MOVE WS-GP-LABEL             TO WS-D5-COL1-LABEL
032007         MOVE WS-GP-DISTR1-CNT        TO WS-D5-DISTR-1
032007         MOVE WS-GP-DISTR2-CNT        TO WS-D5-DISTR-2
032007         MOVE WS-GP-DISTR3-CNT        TO WS-D5-DISTR-3
032007         MOVE WS-GP-DISTR3A-CNT       TO WS-D5-DISTR-3A
032007         MOVE WS-GP-DISTR4-CNT        TO WS-D5-DISTR-4
032007         MOVE WS-GP-DISTR5-CNT        TO WS-D5-DISTR-5
032007         MOVE WS-GP-DISTR6-CNT        TO WS-D5-DISTR-6
032007         MOVE WS-GP-DISTR7-CNT        TO WS-D5-DISTR-7
032007         MOVE WS-GP-DISTR8-CNT        TO WS-D5-DISTR-8
032007         MOVE WS-GP-DISTR9-CNT        TO WS-D5-DISTR-9
032007
032007         MOVE WS-DETAIL5-DISTRIBUTION TO PRT
032007         MOVE '0'                     TO P-CTL
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
032007
032007         MOVE WS-IU-LABEL             TO WS-D5-COL1-LABEL
032007         MOVE WS-IU-DISTR1-CNT        TO WS-D5-DISTR-1
032007         MOVE WS-IU-DISTR2-CNT        TO WS-D5-DISTR-2
032007         MOVE WS-IU-DISTR3-CNT        TO WS-D5-DISTR-3
032007         MOVE WS-IU-DISTR3A-CNT       TO WS-D5-DISTR-3A
032007         MOVE WS-IU-DISTR4-CNT        TO WS-D5-DISTR-4
032007         MOVE WS-IU-DISTR5-CNT        TO WS-D5-DISTR-5
032007         MOVE WS-IU-DISTR6-CNT        TO WS-D5-DISTR-6
032007         MOVE WS-IU-DISTR7-CNT        TO WS-D5-DISTR-7
032007         MOVE WS-IU-DISTR8-CNT        TO WS-D5-DISTR-8
032007         MOVE WS-IU-DISTR9-CNT        TO WS-D5-DISTR-9
032007
032007         MOVE WS-DETAIL5-DISTRIBUTION TO PRT
032007         MOVE '0'                     TO P-CTL
032007         PERFORM 3900-WRITE           THRU 3900-EXIT
052614
052614         MOVE WS-FAM-LABEL            TO WS-D5-COL1-LABEL
052614         MOVE WS-FAM-DISTR1-CNT       TO WS-D5-DISTR-1
052614         MOVE WS-FAM-DISTR2-CNT       TO WS-D5-DISTR-2
052614         MOVE WS-FAM-DISTR3-CNT       TO WS-D5-DISTR-3
052614         MOVE WS-FAM-DISTR3A-CNT      TO WS-D5-DISTR-3A
052614         MOVE WS-FAM-DISTR4-CNT       TO WS-D5-DISTR-4
052614         MOVE WS-FAM-DISTR5-CNT       TO WS-D5-DISTR-5
052614         MOVE WS-FAM-DISTR6-CNT       TO WS-D5-DISTR-6
052614         MOVE WS-FAM-DISTR7-CNT       TO WS-D5-DISTR-7
052614         MOVE WS-FAM-DISTR8-CNT       TO WS-D5-DISTR-8
052614         MOVE WS-FAM-DISTR9-CNT       TO WS-D5-DISTR-9
052614
052614         MOVE WS-DETAIL5-DISTRIBUTION TO PRT
052614         MOVE '0'                     TO P-CTL
052614         PERFORM 3900-WRITE           THRU 3900-EXIT
052614
032007     END-IF
032007
032007     MOVE WS-LIFE-LABEL               TO WS-D5-COL1-LABEL
032007     MOVE WS-LIF-DISTR1-CNT           TO WS-D5-DISTR-1
032007     MOVE WS-LIF-DISTR2-CNT           TO WS-D5-DISTR-2
032007     MOVE WS-LIF-DISTR3-CNT           TO WS-D5-DISTR-3
032007     MOVE WS-LIF-DISTR3A-CNT          TO WS-D5-DISTR-3A
032007     MOVE WS-LIF-DISTR4-CNT           TO WS-D5-DISTR-4
032007     MOVE WS-LIF-DISTR5-CNT           TO WS-D5-DISTR-5
032007     MOVE WS-LIF-DISTR6-CNT           TO WS-D5-DISTR-6
032007     MOVE WS-LIF-DISTR7-CNT           TO WS-D5-DISTR-7
032007     MOVE WS-LIF-DISTR8-CNT           TO WS-D5-DISTR-8
032007     MOVE WS-LIF-DISTR9-CNT           TO WS-D5-DISTR-9
032007
032007     MOVE WS-DETAIL5-DISTRIBUTION     TO PRT
032007     MOVE '0'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     MOVE WS-GRAND-TOTALS-LABEL       TO WS-D5-COL1-LABEL
032007     MOVE WS-TOT-DISTR1-CNT           TO WS-D5-DISTR-1
032007     MOVE WS-TOT-DISTR2-CNT           TO WS-D5-DISTR-2
032007     MOVE WS-TOT-DISTR3-CNT           TO WS-D5-DISTR-3
032007     MOVE WS-TOT-DISTR3A-CNT          TO WS-D5-DISTR-3A
032007     MOVE WS-TOT-DISTR4-CNT           TO WS-D5-DISTR-4
032007     MOVE WS-TOT-DISTR5-CNT           TO WS-D5-DISTR-5
032007     MOVE WS-TOT-DISTR6-CNT           TO WS-D5-DISTR-6
032007     MOVE WS-TOT-DISTR7-CNT           TO WS-D5-DISTR-7
032007     MOVE WS-TOT-DISTR8-CNT           TO WS-D5-DISTR-8
032007     MOVE WS-TOT-DISTR9-CNT           TO WS-D5-DISTR-9
032007
032007     MOVE WS-DETAIL5-DISTRIBUTION     TO PRT
032007     MOVE '-'                         TO P-CTL
032007     PERFORM 3900-WRITE               THRU 3900-EXIT
032007
032007     .
032007 3600-EXIT.
032007     EXIT.


       3800-PRINT-HEADINGS.

           MOVE WS-HEADING1                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           MOVE WS-HEADING2                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           ADD +1                           TO WS-PAGE
           MOVE WS-PAGE                     TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT
           PERFORM 3900-WRITE               THRU 3900-EXIT

           .
       3800-EXIT.
           EXIT.



       3900-WRITE.

           EVALUATE TRUE
           WHEN P-CTL = '1'
               MOVE +1                      TO WS-LINE-COUNT

           WHEN P-CTL = SPACE
               ADD +1                       TO WS-LINE-COUNT

           WHEN P-CTL = '0'
               ADD +2                       TO WS-LINE-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE-COUNT
           END-EVALUATE



           EVALUATE TRUE

      **************************** P = HARDCOPY ONLY
           WHEN DTE-PRT-OPT = 'P'
               WRITE PRT

      **************************** B = SEQUENTIAL FILE ARCHIVE (USED TO
      *                                 BE FICHE) AND HARDCOPY
           WHEN DTE-PRT-OPT = 'B'
               PERFORM 4000-SEQ-FILE-ARCHIVE THRU 4000-EXIT
               WRITE PRT

      **************************** S = ONLINE REPORT FILE ONLY (ELREPT)
           WHEN DTE-PRT-OPT = 'S'
               PERFORM 4300-ONLINE-REPORT    THRU 4300-EXIT

      **************************** T = ONLINE REPORT FILE AND HARDCOPY
           WHEN DTE-PRT-OPT = 'T'
               PERFORM 4300-ONLINE-REPORT    THRU 4300-EXIT
               WRITE PRT

           END-EVALUATE

           .
       3900-EXIT.
           EXIT.


       4000-SEQ-FILE-ARCHIVE.
      ******************************************************************
      * WHILE FICHE IS NO LONGER BEING PRODUCED, THE FICHE LOGIC IS
      * BEING USED TO CREATE A SEQUENTIAL FILE REPORT ARCHIVE WHEN
      * THE DTE-PRT-OPT IS SET TO 'B'
      ******************************************************************
           IF FICH-NO
               GO TO 4000-EXIT
           ELSE
               IF FICH-OPEN = SPACE
                   MOVE 'X'                 TO FICH-OPEN
                   OPEN OUTPUT SEQ-FILE-ARCHIVE
               END-IF
           END-IF

           MOVE SPACE                       TO P-CTL
           WRITE SEQ-ARCHIVE-REC            FROM PRT

           .
       4000-EXIT.
           EXIT.


       4300-ONLINE-REPORT.

           IF (REPT-OPEN      = SPACE) AND
              (DTE-ABEND-CD-1 = SPACE)
               OPEN I-O ELREPT

               IF (DTE-F-1        NOT = ZERO)  AND
                  (DTE-VSAM-FLAGS NOT = '97')
                   MOVE DTE-VSAM-FLAGS        TO WS-ABEND-FILE-STATUS
                   MOVE 'ERROR OPENING ELREPT'
                                              TO WS-ABEND-MESSAGE
                   PERFORM ABEND-PGM          THRU APS-EXIT
               ELSE
                   MOVE '1'                   TO REPT-OPEN
                   MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD
                   MOVE '1'                   TO RF-RECORD-TYPE
                   MOVE OLC-REPORT-NAME       TO RF-REPORT-ID
                   MOVE ZERO                  TO RF-LINE-NUMBER
                   START ELREPT KEY NOT LESS RF-CONTROL-PRIMARY
                   SET BEGIN-REPORT-DELETE    TO TRUE
                   PERFORM 4500-REPORT-DELETE THRU 4500-EXIT
                       UNTIL REPORT-DELETE-DONE

                   MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD
                   MOVE '2'                   TO RF-RECORD-TYPE
                   MOVE OLC-REPORT-NAME       TO RF-REPORT-ID
                   MOVE ZERO                  TO RF-LINE-NUMBER
                   START ELREPT KEY NOT LESS RF-CONTROL-PRIMARY
                   SET BEGIN-REPORT-DELETE    TO TRUE
                   PERFORM 4500-REPORT-DELETE THRU 4500-EXIT
                       UNTIL REPORT-DELETE-DONE

                   MOVE DTE-CLASIC-COMPANY-CD TO RF-COMPANY-CD
                   MOVE '1'                   TO RF-RECORD-TYPE
                   MOVE OLC-REPORT-NAME       TO RF-REPORT-ID
                   MOVE SPACES                TO RF-REPORT-LINE-133
               END-IF
           END-IF


           IF DTE-ABEND-CD-1 = '81' AND
              DTE-PRT-OPT    = 'S'
               MOVE +0302                     TO WS-RETURN-CODE
               PERFORM ABEND-PGM              THRU APS-EXIT
           END-IF


           MOVE P-CTL                         TO RF-CTL-CHAR-133
           MOVE P-DATA                        TO RF-DATA-133

           IF DTE-ABEND-CD-1 = SPACES
               ADD +1                         TO DTE-TOT-LINES
               MOVE DTE-TOT-LINES             TO RF-LINE-NUMBER
               WRITE REPORT-SAVE-FILE
                   INVALID KEY
                       MOVE '88'              TO DTE-ABEND-CD-1
                       CLOSE ELREPT
                       MOVE SPACE             TO REPT-OPEN
           END-IF

           .
       4300-EXIT.
           EXIT.

       4500-REPORT-DELETE.

           IF DTE-F-1 NOT = ZERO
               MOVE ZERO                      TO DTE-VSAM-FLAGS
               SET REPORT-DELETE-DONE         TO TRUE
               GO TO 4500-EXIT
           END-IF

           READ ELREPT NEXT RECORD
               AT END
                   SET REPORT-DELETE-DONE     TO TRUE
                   GO TO 4500-EXIT
           END-READ

           IF (DTE-CLASIC-COMPANY-CD = RF-COMPANY-CD)  AND
              (OLC-REPORT-NAME       = RF-REPORT-ID)
               DELETE ELREPT RECORD
           END-IF

           .
       4500-EXIT.
           EXIT.



       5000-PRINT-DETAIL-FILE.

           READ FILE-OUT INTO FILE-REC
               AT END
                   SET DETAIL-REPORT-DONE   TO TRUE
                   GO TO 5000-EXIT
           END-READ.
           
           MOVE FILE-AUDITOR     TO WS-HOLD-FILE-AUDITOR
           MOVE FILE-AUDITOR     TO WS-D6-AUDITOR
           MOVE FILE-RECORDED-DT TO WS-DATE.
           MOVE WS-DATE-YY       TO WS-D6-RECORDED-YY.
           MOVE WS-DATE-MM       TO WS-D6-RECORDED-MM.
           MOVE WS-DATE-DD       TO WS-D6-RECORDED-DD.
           MOVE '/'              TO WS-D6-RECORDED-SL1.
           MOVE '/20'            TO WS-D6-RECORDED-SL2.
           MOVE FILE-CLAIM-NUMBER TO WS-D6-CLAIM-NUMBER.
           MOVE FILE-CERT-NUMBER TO WS-D6-CERT-NUMBER.
032007     MOVE FILE-HANDLE-TIME TO WS-D6-HANDLE-TIME.
032007     MOVE FILE-HANDLE-COUNTED TO WS-D6-HANDLE.
           
           EVALUATE TRUE
               WHEN FILE-CLAIM-TYPE = 'A'
                   MOVE WS-DIS-LABEL         TO WS-D6-CLAIM-TYPE

               WHEN FILE-CLAIM-TYPE = 'G'
                   MOVE WS-GP-LABEL          TO WS-D6-CLAIM-TYPE

               WHEN FILE-CLAIM-TYPE = 'I'
                   MOVE WS-IU-LABEL          TO WS-D6-CLAIM-TYPE
052614
052614         WHEN FILE-CLAIM-TYPE = 'F'
052614             MOVE WS-FAM-LABEL         TO WS-D6-CLAIM-TYPE

               WHEN FILE-CLAIM-TYPE = 'L'
                   MOVE WS-LIFE-LABEL        TO WS-D6-CLAIM-TYPE
           END-EVALUATE.
           
           MOVE SPACES TO WS-D6-ACTION.
           
           EVALUATE TRUE
               WHEN FILE-REC-TYPE = 'C '
                   MOVE 'PAID CLAIM'         TO WS-D6-ACTION
               WHEN FILE-REC-TYPE-1 = 'D'
                   MOVE FILE-REC-TYPE-2      TO WS-CLOSE-REASON
                   MOVE WS-CLOSE-DESC        TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'G '
                   MOVE 'NOTE'               TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'I'
                   MOVE 'CALL IN'            TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'L '     
                   MOVE 'LETTER'             TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'LI'
                   MOVE 'UNSOLICITED MAIL IN' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'LR'
                   MOVE 'RESPONSE TO LETTER SENT' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'LS'
                   MOVE 'LETTER SENT'        TO WS-D6-ACTION
031407         WHEN FILE-REC-TYPE = 'MR'
031407             MOVE 'MEDICAL RECORDS'    TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'N '
                   MOVE 'NEW CLAIM'          TO WS-D6-ACTION
031407         WHEN FILE-REC-TYPE = 'NF'
031407             MOVE 'NOTE AND FILE  '    TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'O ' 
                   MOVE 'CALL OUT'           TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P1'
                   MOVE 'PARTIAL PAYMENT'    TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P2'
                   MOVE 'FINAL PAYMENT'      TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P3'
                   MOVE 'SETTLEMENT PAYMENT' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P4'
                   MOVE 'ADDITIONAL PAYMENT' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P5'
                   MOVE 'CHG EXP PAYMENT'    TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P6'
                   MOVE 'NON CHG EXP PAYMENT' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'P9'
                   MOVE 'PROVISIONAL PAYMENT' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'PT'
                   MOVE 'TRANSFER'           TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'PI'
                   MOVE 'LIFE INTEREST'      TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'R '
                   MOVE 'DENIAL/RETRACTION'  TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'S '   
                   MOVE 'STOP PAY'           TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'UR'
                   MOVE 'RECV''D FROM UNDERWRITING' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'US'
                   MOVE 'SENT TO UNDERWRITING' TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'V '
                   MOVE 'VOID'               TO WS-D6-ACTION
               WHEN FILE-REC-TYPE = 'X '
                   MOVE 'RE-OPEN CLAIM'      TO WS-D6-ACTION
           END-EVALUATE. 
                  
           
           IF FILE-FROM-DT > ZEROS
               MOVE FILE-FROM-DT TO WS-DATE
               MOVE WS-DATE-YY   TO WS-D6-FROM-YY
               MOVE WS-DATE-MM   TO WS-D6-FROM-MM
               MOVE WS-DATE-DD   TO WS-D6-FROM-DD
               MOVE '/'          TO WS-D6-FROM-SL1
               MOVE '/20'        TO WS-D6-FROM-SL2
           ELSE
               MOVE SPACES       TO WS-D6-FROM-DT
           END-IF.
           IF FILE-TO-DT > ZEROS
               MOVE FILE-TO-DT   TO WS-DATE
               MOVE WS-DATE-YY   TO WS-D6-TO-YY
               MOVE WS-DATE-MM   TO WS-D6-TO-MM
               MOVE WS-DATE-DD   TO WS-D6-TO-DD
               MOVE '/'          TO WS-D6-TO-SL1
               MOVE '/20'        TO WS-D6-TO-SL2
           ELSE
               MOVE SPACES       TO WS-D6-TO-DT
           END-IF.

           PERFORM 5100-PRINT-DETAIL6 THRU 5100-EXIT.
           .
       5000-EXIT.
           EXIT.


       5100-PRINT-DETAIL6.

           IF WS-LINE2-COUNT >= WS-LINE-COUNT-MAX
               PERFORM 5200-PRINT-HEADINGS  THRU 5200-EXIT
               MOVE WS-HEADING4-2           TO PRT2
               PERFORM 5300-WRITE           THRU 5300-EXIT
               MOVE WS-HEADING5-2           TO PRT2
               PERFORM 5300-WRITE           THRU 5300-EXIT
               MOVE WS-HOLD-FILE-AUDITOR    TO WS-PREV-FILE-AUDITOR
           END-IF

           MOVE WS-DETAIL6                  TO PRT2
           IF WS-HOLD-FILE-AUDITOR NOT EQUAL WS-PREV-FILE-AUDITOR
               MOVE '0'                     TO P2-CTL
               MOVE WS-HOLD-FILE-AUDITOR    TO WS-PREV-FILE-AUDITOR
           ELSE
               MOVE ' '                     TO P2-CTL
           END-IF
           PERFORM 5300-WRITE               THRU 5300-EXIT

           .
       5100-EXIT.
           EXIT.


       5200-PRINT-HEADINGS.

           MOVE WS-HEADING1                 TO PRT2
           PERFORM 5300-WRITE               THRU 5300-EXIT

           MOVE WS-HEADING2                 TO PRT2
           PERFORM 5300-WRITE               THRU 5300-EXIT

           ADD +1                           TO WS-PAGE2
           MOVE WS-PAGE2                    TO WS-H3-PAGE
           MOVE WS-HEADING3                 TO PRT2
           PERFORM 5300-WRITE               THRU 5300-EXIT

           .
       5200-EXIT.
           EXIT.



       5300-WRITE.

           EVALUATE TRUE
           WHEN P2-CTL = '1'
               MOVE +1                      TO WS-LINE2-COUNT

           WHEN P2-CTL = SPACE
               ADD +1                       TO WS-LINE2-COUNT

           WHEN P2-CTL = '0'
               ADD +2                       TO WS-LINE2-COUNT

           WHEN OTHER
               ADD +3                       TO WS-LINE2-COUNT
           END-EVALUATE



           WRITE PRT2

           .
       5300-EXIT.
           EXIT.



       8500-DATE-CONVERSION.

           CALL 'ELDATCX' USING DATE-CONVERSION-DATA

           .
       8500-EXIT.
           EXIT.


       OPEN-FILES.

           OPEN INPUT  MTD-ELMSTR-INFILE
                       ELTRLR-INFILE
                OUTPUT PRNTR
                       file-out
                       PRNTR2


           IF WS-ELTRLR-FILE-STATUS  = '00' OR '97'
              CONTINUE
           ELSE
              MOVE 'OPEN ERROR OCCURRED ON ELTRLR'
                                              TO WS-ABEND-MESSAGE
              MOVE WS-ELTRLR-FILE-STATUS      TO WS-ABEND-FILE-STATUS
              PERFORM ABEND-PGM               THRU APS-EXIT
           END-IF

           .
       OPEN-FILES-EXIT.
           EXIT.


       CLOSE-FILES.

           IF FICH-OPEN NOT = SPACE
               CLOSE SEQ-FILE-ARCHIVE
           END-IF 

           IF REPT-OPEN NOT = SPACE
               MOVE '2'                       TO RF-RECORD-TYPE
               ADD +1                         TO DTE-TOT-LINES
               MOVE DTE-TOT-LINES             TO RF-LINE-NUMBER
               MOVE SPACES                    TO RF-TRAILER-RECORD
               MOVE WS-TIME                   TO RF-PRINT-HH-MM-SS
               MOVE WS-CURRENT-DATE           TO RF-CURRENT-DATE
               WRITE REPORT-SAVE-FILE
               IF DTE-VSAM-FLAGS NOT = ZEROS
                   DISPLAY 'ERROR DURING WRITE OF TYPE 2 REPORT REC'
                   DISPLAY 'VSAM RETURN CODE = ' DTE-VSAM-FLAGS
                   DISPLAY 'ERROR DURING WRITE OF TYPE 2 REPORT REC'
                       UPON CONSOLE
                   DISPLAY 'VSAM RETURN CODE = ' DTE-VSAM-FLAGS
                       UPON CONSOLE
               ELSE
                   DISPLAY '***************************************'
                   DISPLAY '***************************************'
                   DISPLAY '**               ' OLC-REPORT-NAME
                   DISPLAY '**  REPORT HAS BEEN SAVED IN THE     **'
                   DISPLAY '**  CLAS-IC ONLINE REPORT FILE       **'
                   DISPLAY '**                                   **'
                   DISPLAY '***************************************'
                   DISPLAY '***************************************'
                   CLOSE ELREPT
               END-IF
           END-IF

           IF DTE-ABEND-CD-1 = '81'
               DISPLAY '***************************************'
               DISPLAY '***************************************'
               DISPLAY '**               ' OLC-REPORT-NAME
               DISPLAY '**  UNABLE TO OPEN ONLINE FILE -     **'
               DISPLAY '**  REPORT PRINTED BUT NOT SAVED     **'
               DISPLAY '**                                   **'
               DISPLAY '***************************************'
               DISPLAY '***************************************'
               DISPLAY 'VSAM RETURN CODE WAS - ', DTE-VSAM-FLAGS
           END-IF

           IF DTE-ABEND-CD-1 = '88'
               DISPLAY '***************************************'
               DISPLAY '***************************************'
               DISPLAY '**               ' OLC-REPORT-NAME
               DISPLAY '**  REPORT FILE IS FULL-  REPORT WAS **'
               DISPLAY '**  PRINTED AND PARTIALLY SAVED      **'
               DISPLAY '**                                   **'
               DISPLAY '***************************************'
               DISPLAY '***************************************'
               DISPLAY 'VSAM RETURN CODE WAS - ', DTE-VSAM-FLAGS
           END-IF


           CLOSE MTD-ELMSTR-INFILE
                 ELTRLR-INFILE
                 PRNTR
                 file-out
                 PRNTR2
                 
            .
       CLOSE-FILES-EXIT.
           EXIT.

       ABEND-PGM. COPY ELCABEND.
