000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                cibal511.                              00000200
000300                                                                  00000300
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             00000400
000500*            OMAHA, NEBR.                                         00000500
000600                                                                  00000600
000700*DATE-COMPILED.                                                   00000700
000900                                                                  00000800
001000*REMARKS.                                                         00000900
001100*     THIS PROGRAM GENERATES CANCEL TRANSACTIONS FROM a           00001000
001200*       ballard file.                                             00001100
001400                                                                  00001470
001500 ENVIRONMENT DIVISION.                                            00001500
001600 INPUT-OUTPUT SECTION.                                            00001600
001700 FILE-CONTROL.                                                    00001700
001800     SELECT  ballard-in      ASSIGN TO SYS010
              ORGANIZATION IS LINE SEQUENTIAL.
001900     SELECT  ballard-out     ASSIGN TO SYS012.                    00001900
002000     SELECT  RPT-FILE        ASSIGN TO SYS013.                    00002000
002200                                                                  00002200
002300 SKIP3                                                            00002300
002400 DATA DIVISION.                                                   00002400
002500                                                                  00002500
002600 FILE SECTION.                                                    00002600
002700                                                                  00002700
002800******************************************************************00004020
002900**        INPUT TAPE FILE FROM ballard                          **00004030
003000******************************************************************00004040
003100                                                                  00004050
003200 FD  ballard-in                                                   00004060
003400     RECORDING MODE IS F                                          00004070
003500     LABEL RECORDS ARE STANDARD                                   00004080
003600     RECORD CONTAINS 208 CHARACTERS                               00004090
003700     BLOCK CONTAINS 0 RECORDS.                                    00004091
003900                                                                  00004093
004000 01  ballard-record             PIC X(208).                       00004094
004100                                                                  00004095
004200******************************************************************00004100
004300**       OUTPUT CARD FILE FOR INPUT TO PROGRAM 'EL512'          **00004200
004400******************************************************************00004300
004500                                                                  00004400
004600 FD  ballard-out                                                  00004500
004700     RECORDING MODE IS F                                          00004600
004800     LABEL RECORDS ARE STANDARD                                   00004700
004900     RECORD CONTAINS 90 CHARACTERS                                00004800
003700     BLOCK CONTAINS 0 RECORDS.                                    00004900
005100                                                                  00005100
005200 01  ballard-record-ot       PIC X(90).                           00005200
005300                                                                  00005300
005300                                                                  00005395
005400******************************************************************00005400
005500**                 OUTPUT ERROR REPORT                          **00005500
005600******************************************************************00005600
005700                                                                  00005700
005800 FD  RPT-FILE                                                     00005800
005900     RECORDING MODE IS F                                          00005900
006000     LABEL RECORDS ARE STANDARD                                   00006000
006100     RECORD CONTAINS 133 CHARACTERS                               00006100
006200     BLOCK CONTAINS 0 RECORDS                                     00006200
006300     DATA RECORD IS RPT-REC-OUT.                                  00006300
006400                                                                  00006400
006500 01  RPT-REC-OUT.                                                 00006500
006700     05  RPT-REC                  PIC X(132).                     00006700
006800                                                                  00006800
006900******************************************************************00006900
007000                                                                  00007000
007100 SKIP3                                                            00007100
007200 WORKING-STORAGE SECTION.                                         00007200
007300 77  FILLER  PIC X(32) VALUE '********************************'.  00007300
007400 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00007400
007500 77  FILLER  PIC X(32) VALUE '********************************'.  00007500
007600                                                                  00007600
062100 77  LAST-REC-SW            PIC X         VALUE 'N'.              00007700
007700 77  CANCL-SW               PIC X         VALUE 'N'.              00007800
007700 77  SW-20                  PIC X         VALUE ' '.              00007900
007700 77  ERROR-CODE             PIC X(25)     VALUE SPACES.           00008000
007700 77  SAVE-SEQ               PIC X(30)     VALUE SPACES.           00008100
007800 77  SAVE-CERT              PIC X(10)     VALUE SPACES.           00008200
007800 77  SAVE-STATE             PIC X(02)     VALUE SPACES.           00008210
008300 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00008600
008400 77  RPT-CANCL-CNT          PIC 99999     VALUE ZEROS.            00008700
008500 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00008800
008600 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00008900
008800 77  RPT-CNT                PIC 9999999   VALUE ZEROS.            00009000
008900 77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00009100
008900 77  NINES-IN               PIC S9(12) VALUE 999999999999.        00009101
008900 77  DETAIL-CNT-TOT         PIC 9999999   VALUE ZEROS.            00009110
009000 77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00009200
009100 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00009600
009200 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00009700
009300 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00009800
009400 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00009900
009500 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00010000
009600 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00010100
010200 77  CANC-CNT               PIC 9999      VALUE ZEROS.            00010300
010200 77  CERT-CNT               PIC 9999      VALUE ZEROS.            00010400
010200 77  W-PROC-MO              PIC 99        VALUE ZEROS.            00010410
010300 77  FIRST-SW               PIC X         VALUE 'Y'.              00010500
010300 77  WRITTEN-PREM-SW        PIC X         VALUE 'N'.              00010510
010300 77  SAVE-WORK-MO           PIC 99        VALUE ZEROS.            00010520
010400                                                                  00010600
010500 01  BUILD-APR.                                                   00010830
010600     05  W-APR-N                PIC 9(06).                        00010831
010500     05  W-APR-X  REDEFINES  W-APR-N.                             00010832
010600         10  W-APR-ALL.                                           00010833
010600             15  W-APR-WHOLE    PIC X(02).                        00010834
010600             15  W-APR-DEC.                                       00010835
010600                 20  W-APR-DEC-FIRST2 PIC XX.                     00010836
010600                 20  W-APR-DEC-LAST2  PIC XX.                     00010837
010400                                                                  00010838
010500 01  W-LREFUND.                                                   00010839
010600     05  W-LREFUND-N            PIC 9(09).                        00010840
010500     05  W-LREFUND-X   REDEFINES  W-LREFUND-N.                    00010841
010600         10  W-LREFUND-DOL.                                       00010842
010600             15  W-LREFUND-DOL2 PIC X(02).                        00010843
010600             15  W-LREFUND-DOL5 PIC X(05).                        00010844
010600         10  W-LREFUND-CEN      PIC X(02).                        00010845
010400                                                                  00010846
010500 01  W-AREFUND.                                                   00010847
010600     05  W-AREFUND-N            PIC 9(09).                        00010848
010500     05  W-AREFUND-X   REDEFINES  W-AREFUND-N.                    00010849
010600         10  W-AREFUND-DOL.                                       00010850
010600             15  W-AREFUND-DOL2 PIC X(02).                        00010851
010600             15  W-AREFUND-DOL5 PIC X(05).                        00010852
010600         10  W-AREFUND-CEN      PIC X(02).                        00010853
010400                                                                  00010854
CIDMOD                                                                  00010855
CIDMOD 01  AH-TERM-CHECK.                                               00010856
CIDMOD     05  AH-X                  PIC XXX.                           00010857
CIDMOD     05  AH-N  REDEFINES AH-X  PIC 999.                           00010858
010400                                                                  00010855
010500 01  W-PR-NOTE-AMT.                                               00010856
010600     05  W-PN-AMT-N            PIC 9(08).                         00010857
010500     05  W-PN-AMT-X   REDEFINES  W-PN-AMT-N.                      00010858
010600         10  W-PN-AMT-DOL      PIC X(06).                         00010859
010600         10  W-PN-AMT-CEN      PIC X(02).                         00010860
010400                                                                  00010861
010400                                                                  00010862
010500 01  W-PROC-YR-CEN.                                               00010863
010600     05  W-PROC-CEN         PIC X(02)     VALUE SPACES.           00010864
010600     05  W-PROC-YR          PIC 9(02)     VALUE ZEROS.            00010865
010400                                                                  00010866
010500 01  OUT-RECORD.                                                  00010867
010600     05  OUT-CERT-NO        PIC X(10)     VALUE SPACES.           00010870
010600     05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00010900
010700     05  OUT-EFF-DATE       PIC X(06)     VALUE SPACES.           00011000
010800     05  OUT-INFO           PIC X(61)     VALUE SPACES.           00011100
010800     05  OUT-TRAN-TYPE      PIC X         VALUE SPACES.           00011200
010800     05  OUT-SEQUENCE       PIC X         VALUE SPACES.           00011300
010600     05  SR-BATCH-NUMB.                                           00011400
010600         10  SR-BATCH-ID    PIC XX        VALUE ZEROS.            00011400
010600         10  SR-BATCH-DA    PIC XX        VALUE ZEROS.            00011400
010600         10  SR-BATCH-NO    PIC 99        VALUE ZEROS.            00011400
010600     05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00011500
010900                                                                  00011600
124600 01  SAVE-FULL-ACCT.                                              00011700
124700     05 SAVE-ACCT-7         PIC X(07)     VALUE SPACES.           00011900
124600     05 SAVE-ACCT           PIC XXX       VALUE SPACES.           00011910
013600                                                                  00012000
019500 01  PACESETR-BATCH-HDR.                                          00012100
020500     05  PS-CARR-CO.                                              00012200
020600         10  PS-CARR         PIC X         VALUE '9'.             00012300
020700         10  PS-COMP         PIC X(6)      VALUE '000000'.        00012400
020800     05  PS-STATE            PIC XX        VALUE '  '.            00012500
020900     05  PS-ACCT-NO          PIC X(10)     VALUE SPACES.          00012600
020200     05  PS-BATCH-NUMB.                                           00012700
020200         10  PS-BATCH-ID     PIC XX        VALUE ZEROS.           00012700
020200         10  PS-BATCH-DA     PIC XX        VALUE ZEROS.           00012700
020200         10  PS-BATCH-NO     PIC 99        VALUE ZEROS.           00012700
021000     05  PS-HD-EFF-DT.                                            00012800
021100         10  PS-HD-EFF-MO    PIC XX        VALUE SPACES.          00012900
021200         10  PS-HD-EFF-DA    PIC XX        VALUE SPACES.          00013000
021300         10  PS-HD-EFF-YR    PIC XX        VALUE SPACES.          00013100
020000     05  PS-CERT-ISS         PIC S9(4)     VALUE ZEROS.           00013200
019600     05  PS-LF-WRITTEN       PIC S9(7)V99  VALUE ZEROS.           00013300
019800     05  PS-AH-WRITTEN       PIC S9(7)V99  VALUE ZEROS.           00013400
020100     05  PS-CERT-CANC        PIC S9(4)     VALUE ZEROS.           00013500
019700     05  PS-LF-CANC          PIC S9(7)V99  VALUE ZEROS.           00013600
019900     05  PS-AH-CANC          PIC S9(7)V99  VALUE ZEROS.           00013700
020300     05  PS-CLIENT-ID        PIC X(3)      VALUE 'CSO'.           00013800
021400     05  PS-TRANS-TYPE       PIC X         VALUE '1'.             00013900
020400     05  PS-SEQUENCE-NO      PIC X         VALUE '0'.             00014000
024900                                                                  00014100
025000 01  REPORT-HD-LINE.                                              00014200
025100     05  HD-LIFE-WRITTEN     PIC X(14)  VALUE ' LIFE WRITTEN '.   00014300
025200     05  FILLER              PIC XX     VALUE SPACES.             00014400
025300     05  HD-LIFE-CANC        PIC X(14)  VALUE '    LIFE CANC '.   00014500
025400     05  FILLER              PIC XX     VALUE SPACES.             00014600
025500     05  HD-AH-WRITTEN       PIC X(14)  VALUE '  A&H WRITTEN '.   00014700
025600     05  FILLER              PIC XX     VALUE SPACES.             00014800
025700     05  HD-AH-CANC          PIC X(14)  VALUE '     A&H CANC '.   00014900
025800     05  FILLER              PIC XXX    VALUE SPACES.             00015000
025900     05  HD-CERT-ISS         PIC X(8)   VALUE ' ISS CNT'.         00015100
026000     05  FILLER              PIC XXXX   VALUE SPACES.             00015200
026100     05  HD-CERT-CANC        PIC X(8)   VALUE 'CANC CNT'.         00015300
026200     05  FILLER              PIC XX     VALUE SPACES.             00015400
026300     05  HD-BATCH-NO         PIC X(7)   VALUE SPACES.             00015500
026400     05  FILLER              PIC XX     VALUE SPACES.             00015600
026500     05  HD-STATE            PIC XXXXX  VALUE SPACES.             00015700
026600     05  FILLER              PIC XX     VALUE SPACES.             00015800
026700     05  HD-ACCT-NO          PIC X(10)  VALUE SPACES.             00015900
026800     05  FILLER              PIC XX     VALUE SPACES.             00016000
026900     05  HD-EFF-DT           PIC X(8)   VALUE SPACES.             00016100
027000                                                                  00016200
027100 01  TOTAL-LINE.                                                  00016300
027200     05  T-LIFE-WRITTEN      PIC ZZ,ZZZ,ZZ9.99-.                  00016400
027300     05  FILLER              PIC XX              VALUE SPACES.    00016500
027400     05  T-LIFE-CANC         PIC ZZ,ZZZ,ZZ9.99-.                  00016600
027500     05  FILLER              PIC XX              VALUE SPACES.    00016700
027600     05  T-AH-WRITTEN        PIC ZZ,ZZZ,ZZ9.99-.                  00016800
027700     05  FILLER              PIC XX              VALUE SPACES.    00016900
027800     05  T-AH-CANC           PIC ZZ,ZZZ,ZZ9.99-.                  00017000
027900     05  FILLER              PIC XXXX            VALUE SPACES.    00017100
028000     05  T-CERT-ISS          PIC ZZZ,ZZ9.                         00017200
028100     05  FILLER              PIC X(07)           VALUE SPACES.    00017300
028200     05  T-CERT-CANC         PIC Z,ZZ9.                           00017400
028600     05  FILLER              PIC X(36)           VALUE SPACES.    00017500
028800                                                                  00017600
028900 01  RPT-TOT-LINE.                                                00017700
029000     05  FILLER              PIC X(10)           VALUE SPACES.    00017800
029100     05  FILLER              PIC X(35)           VALUE            00017900
029200         ' NUMBER OF ISSUES WRITTEN IS - '.                       00018000
029300     05  P-ISS-CNT           PIC ZZ,ZZ9.                          00018100
029400     05  FILLER              PIC X(10)           VALUE SPACES.    00018200
029500     05  FILLER              PIC X(35)           VALUE            00018300
029600         'NUMBER OF CANCELS WRITTEN IS - '.                       00018400
029700     05  P-CANCL-CNT         PIC ZZ,ZZ9.                          00018500
029800     05  FILLER              PIC X(10)            VALUE SPACES.   00018600
029900     05  FILLER              PIC X(10)            VALUE SPACES.   00018700
030000                                                                  00018800
030100 01  RPT-HD-LINE1.                                                00018900
030200     05  FILLER              PIC X(26)           VALUE SPACES.    00019000
030300     05  PS-RPT-ID           PIC X(34)           VALUE            00019100
030400         '       PACESETTER CORP REPORT FOR '.                    00019200
030500     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00019300
030600     05  FILLER              PIC X(16)           VALUE SPACES.    00019400
030600     05  FILLER              PIC X(16)           VALUE            00019500
030400         'PGM = PACESETR '.                                       00019600
030600     05  FILLER              PIC X(10)           VALUE SPACES.    00019700
030700     05  FILLER              PIC X(5)            VALUE  'PAGE '.  00019800
030800     05  RPT-PAGE            PIC Z,ZZ9.                           00019900
030900     05  FILLER              PIC X(5)            VALUE SPACES.    00020000
031000                                                                  00020100
031100 01  RPT-HD-LINE2.                                                00020200
031300     05  FILLER              PIC X(10)  VALUE 'CERT #  '.         00020300
031500     05  FILLER              PIC X(12)  VALUE ' DETAIL INFO'.     00020400
031600     05  FILLER              PIC X(09)  VALUE ' '.                00020500
031700     05  FILLER              PIC X(5)   VALUE SPACES.             00020600
031800     05  FILLER              PIC X(20)  VALUE SPACES.             00020700
032000     05  FILLER              PIC X(20)  VALUE SPACES.             00020800
032100     05  FILLER              PIC X(40)  VALUE                     00020900
032200                 '              RECORD TYPE    '.                 00021000
032300     05  FILLER              PIC X(16)  VALUE SPACES.             00021100
032400                                                                  00021200
032500 01  RPT-PT-LINE.                                                 00021300
032600     05  PRT-REC             PIC X(80)  VALUE SPACES.             00021400
032600     05  FILLER              PIC X(02)  VALUE SPACES.             00021500
033600     05  FULL-ERROR.                                              00021600
033600         10  PT-ERROR        PIC X(22)  VALUE SPACES.             00021610
033600         10  PRNT-ERROR      PIC X(28)  VALUE SPACES.             00021620
032400                                                                  00021810
032500 01  ERR-PT-LINE.                                                 00021820
032600     05  ERR-ID              PIC X(11)  VALUE ' * ERROR * '.      00021830
032600     05  ERR-REC             PIC X(80)  VALUE SPACES.             00021831
032600     05  FILLER              PIC X(02)  VALUE SPACES.             00021840
033600     05  ERR-LINE.                                                00021880
033600         10  ERR-REASON      PIC X(30)  VALUE SPACES.             00021890
033600         10  ERR-CODE        PIC X(09)  VALUE SPACES.             00021891
033800                                                                  00021892
034800 01  WK-DATE.                                                     00021900
034900     05  WK-MO               PIC XX     VALUE SPACES.             00022000
035000     05  FILLER              PIC X      VALUE SPACES.             00022100
035100     05  WK-DA               PIC XX     VALUE SPACES.             00022200
035200     05  FILLER              PIC X      VALUE SPACES.             00022300
035300     05  WK-YR               PIC XX     VALUE SPACES.             00022400
037600                                                                  00022500
038600 01  WORK-DATE.                                                   00022600
038700     12  WORK-DATE-X.                                             00022700
038800         15  WORK-MO-X       PIC XX.                              00022800
038900         15  WORK-DA-X       PIC XX.                              00022900
039000         15  WORK-YR-X       PIC XX.                              00023000
039100     12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  00023100
039200         15  WORK-MO-N       PIC 99.                              00023200
039300         15  WORK-DA-N       PIC 99.                              00023300
039400         15  WORK-YR-N       PIC 99.                              00023400
039500                                                                  00023500
039100     12  WORK-DATE-IN.                                            00023100
039400         15  WORK-YR-IN      PIC 99.                              00023400
039200         15  WORK-MO-IN      PIC 99.                              00023200
039300         15  WORK-DA-IN      PIC 99.                              00023300
039500                                                                  00023500
040900 01  PRNT-RPT-DATES.                                              00023600
041000     05  RPT-DT-ID.                                               00023700
041100         10  FILLER          PIC X(14)   VALUE 'DECEMBER      '.  00023800
041200         10  FILLER          PIC X(14)   VALUE 'JANUARY       '.  00023900
041300         10  FILLER          PIC X(14)   VALUE 'FEBRUARY      '.  00024000
041400         10  FILLER          PIC X(14)   VALUE 'MARCH         '.  00024100
041500         10  FILLER          PIC X(14)   VALUE 'APRIL         '.  00024200
041600         10  FILLER          PIC X(14)   VALUE 'MAY           '.  00024300
041700         10  FILLER          PIC X(14)   VALUE 'JUNE          '.  00024400
041800         10  FILLER          PIC X(14)   VALUE 'JULY          '.  00024500
041900         10  FILLER          PIC X(14)   VALUE 'AUGUST        '.  00024600
042000         10  FILLER          PIC X(14)   VALUE 'SEPTEMBER     '.  00024700
042100         10  FILLER          PIC X(14)   VALUE 'OCTOBER       '.  00024800
042200         10  FILLER          PIC X(14)   VALUE 'NOVEMBER      '.  00024900
042300                                                                  00025000
042400     05  RPT-DT-05     REDEFINES   RPT-DT-ID.                     00025100
042500         10  RPT-DT        OCCURS 12    PIC X(14).                00025200
006400                                                                  00025311
006800                                                                  00025350
042700 01  FILLER   PIC X(24) VALUE '** CARD COPY BOOK  **'.            00025400
042800                                                                  00025500
042900*01  INPUT-COPYBOOK          COPY ERCPNDBI.                       00025600
042800                                                                  00025500
042900     COPY ERCPNDBI.                                               00025600
043000 SKIP3                                                            00025700
043100                                                                  00025800
043200 01  FILLER   PIC X(24) VALUE '**PACESETR-RECORD-IN**'.           00025900
043300                                                                  00026000
       01  ballard-rec.
           05  bal-clm-no                  pic x(6).
           05  bal-act-no                  pic x(10).
           05  bal-cert-no                 pic x(11).
           05  bal-fname                   pic x(16).
           05  bal-mid-init                pic x.
           05  filler                      pic x.
           05  bal-lname                   pic x(23).
           05  bal-addr1                   pic x(33).
           05  bal-city                    pic x(21).
           05  bal-state                   pic xx.
           05  filler                      pic x.
           05  bal-zip                     pic x(5).
           05  filler                      pic x.
           05  bal-effdt                   pic x(10).
           05  filler                      pic x.
           05  bal-expdt                   pic x(8).
           05  bal-premium.
               10  bal-prem-dollars        pic 9(7).
               10  filler                  pic x.
               10  bal-prem-cents          pic 99.
           05  bal-lfterm                  pic 999.
           05  bal-ahterm                  pic 999.
           05  bal-cncdt                   pic x(8).
           05  BAL-CNC-DT                  PIC X(8).
           05  BAL-CNC-AMT                 PIC 9(7).99.
           05  BAL-DTH-DT                  PIC X(8).
           05  BAL-CLM-DT                  PIC X(8).



043400*01  VENDOR-REC    COPY VENDREC8.                                 00026100
043400*    COPY VENDREC8.                                               00026100
045600                                                                  00038900
043400*01  PPA-WORK-REC   COPY PPAREC.                                  00038910
045600                                                                  00038900
043400*    COPY PPAREC.                                                 00038910
045600                                                                  00038920
054900******************************************************************00039000
055000 SKIP3                                                            00039100
055100 PROCEDURE DIVISION.                                              00039200
055200 SKIP3                                                            00039300
055400                                                                  00039500
055500 INPUT-ROUTINE SECTION.                                           00039600
055600                                                                  00039700
055700     ACCEPT  WORK-DATE-IN  FROM DATE.                             00039800
055800                                                                  00039900
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   WORK-DATE-IN IS -- ' WORK-DATE-IN.              00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
055700     MOVE    WORK-YR-IN    TO  WORK-YR-N.                         00039800
055700     MOVE    WORK-MO-IN    TO  WORK-MO-N.                         00039800
055700     MOVE    WORK-DA-IN    TO  WORK-DA-N.                         00039800
055800                                                                  00039900
055900     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040000
056000     DISPLAY '*   CURRENT DATE IS -- ' WK-DATE.                   00040100
056100     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00040200
056200                                                                  00040300
056500     OPEN INPUT    ballard-in.                                    00040400
056600                                                                  00040500
056800     OPEN OUTPUT   RPT-FILE                                       00040700
056800                      ballard-out.                                00040720
056900                                                                  00040800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00040900
057100                                                                  00041000
057500     MOVE  ZEROS                  TO   PS-LF-CANC                 00041100
057600                                       PS-AH-CANC                 00041200
057700                                       PS-CERT-CANC.              00041300
119500     MOVE  ZEROS                  TO  PBI-I-AH-PREM-AMT           00041310
119500     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-AMT        00041320
094000     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-POS1       00041330
094000     MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-NO         00041340
119500     MOVE  ZEROS                  TO  PBI-I-AH-TERM               00041350
119500     MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD        00041360
119500     MOVE  ZEROS                  TO  PBI-I-AH-TERM               00041370
119500     MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD        00041380
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-YR          00041390
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-MO          00041391
093500     MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-DA          00041392
119500     MOVE  ZEROS                  TO  PBI-I-AH-PAYMENT            00041393
010600     MOVE  SPACES                 TO  OUT-CERT-NO                 00041394
010600     MOVE  SPACES                 TO  OUT-CERT-SFX                00041395
088500     MOVE  ZEROS                  TO  PBI-C-LF-CANCEL-DATE.       00041396
088500     MOVE  ZEROS                  TO  PBI-C-AH-CANCEL-DATE.       00041397
088500     MOVE  ZEROS                  TO  PBI-C-LF-PREM-REFUND.       00041398
088500     MOVE  ZEROS                  TO  PBI-C-AH-PREM-REFUND.       00041399
057100                                                                  00041400
057200     MOVE  WK-MO                  TO   PS-HD-EFF-MO.              00041500
057400     MOVE  WK-DA                  TO   PS-HD-EFF-DA.              00041600
057300     MOVE  WK-YR                  TO   PS-HD-EFF-YR               00041700
057900     MOVE  WK-MO                  TO  WORK-MO-X.                  00041900
058000     MOVE  WK-YR                  TO  WORK-YR-X.                  00042000
058100     MOVE  WK-DA                  TO  WORK-DA-X.                  00042100
058200                                                                  00042200
058300     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00042300
058400     DISPLAY '*   WORK-DATE-X  IS -- ' WORK-DATE-X.               00042400
058500     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00042500
158100                                                                  00042600
058100     MOVE  WORK-MO-IN             TO  SAVE-WORK-MO.               00042700
058200                                                                  00042800
058900 005-ERR-RPT-HD.                                                  00043100
059000                                                                  00043200
059100     ADD  1                       TO RPT-PAGE-CNT.                00043300
059200     MOVE   RPT-PAGE-CNT          TO RPT-PAGE.                    00043400
059300     MOVE    6                    TO RPT-LINE-CNT.                00043500
058700     MOVE  RPT-DT (SAVE-WORK-MO)  TO RPT-REPORT-DATE.             00043610
059500     MOVE  RPT-HD-LINE1           TO RPT-REC.                     00043700
059600     WRITE RPT-REC-OUT            AFTER ADVANCING PAGE.           00043800
059800     MOVE  SPACES                 TO RPT-REC.                     00044000
059900     WRITE RPT-REC-OUT            AFTER ADVANCING 1  LINE.        00044100
060100     MOVE  RPT-HD-LINE2           TO RPT-REC.                     00044300
060200     WRITE RPT-REC-OUT            AFTER ADVANCING 1  LINE.        00044400
060400     MOVE  SPACES                 TO RPT-REC.                     00044600
060500     WRITE RPT-REC-OUT            AFTER ADVANCING 1  LINE.        00044700
060600                                                                  00044800
060700 ERR-RPT-HD-EXIT.                                                 00044900
060800     EXIT.                                                        00045000
060900                                                                  00045100
061400 010-READ-INPUT-FILE.                                             00047100
122000                                                                  00047200
061600     IF  RPT-CNT  GREATER THAN  10                                00047600
061700         GO TO 0200-ERRORS.                                       00047700
061800                                                                  00047800
061900     READ ballard-in                                              00047900
062000       INTO  ballard-rec                                          00048000
062100         AT END                                                   00048100
062100           MOVE 'Y'  TO  LAST-REC-SW                              00048101
122000             PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT         00048110
061700               GO TO END-OF-JOB.                                  00048300
064000                                                                  00048330
           IF (BAL-CNC-DT NOT = '        ')
              OR (BAL-DTH-DT NOT = '        ')
              OR (BAL-CLM-DT NOT = '        ')
              DISPLAY ' CANCEL BYPASSED ' BAL-ACT-NO '  ' BAL-CERT-NO
              GO TO 010-READ-INPUT-FILE
           END-IF
064000                                                                  00048351
063900     ADD  1    TO IN-CNT.                                         00048400
062100     MOVE 'N'  TO  LAST-REC-SW.                                   00048500
064000                                                                  00048512
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00048520
057100                                                                  00048530
064300                                                                  00053700
064400 PROCESS-INPUT.                                                   00053800
064500                                                                  00053900
064600*    DISPLAY 'PROCESS-INPUT   ENTERED  '.                         00054000
167600                                                                  00054108
062100     IF  FIRST-SW  =  'N'                                         00054109
062100         GO TO BUILD-PPA.                                         00054110
062100                                                                  00054111
062100     MOVE 'N'  TO  FIRST-SW.                                      00054115
122000                                                                  00054116
064500     IF bal-state  =  SPACES                                      00054117
064500        MOVE ZEROS TO bal-state                                   00054118
064500        GO TO MOVE-ACCT-NO.                                       00054119
064300                                                                  00054120
CIDMOD     MOVE   bal-state           TO  SAVE-STATE.                   00054121
CIDMOD     MOVE   bal-state           TO  PS-STATE.                     00054130
122000                                                                  00054131
064500 MOVE-ACCT-NO.                                                    00054132
064500                                                                  00054133
062100     MOVE 'Y'                    TO  WRITTEN-PREM-SW.             00054134
167600     MOVE 'PC'                   TO   PS-BATCH-ID.                00054135
167600     MOVE WORK-MO-IN             TO   PS-BATCH-DA.                00054135
167600     ADD    1                    TO   PS-BATCH-NO.                00054136
058700     MOVE  RPT-DT (SAVE-WORK-MO) TO RPT-REPORT-DATE               00054137
122000     MOVE  bal-act-no            TO  SAVE-FULL-ACCT               00054138
062100     MOVE 'N'  TO  FIRST-SW.                                      00054139
167600                                                                  00054140
122000                                                                  00054141
062100 BUILD-PPA.                                                       00054142
122000                                                                  00054143
062100     GO  TO  CK-FOR-LOGIC-BUILD.                                  00054144
122000                                                                  00054145
122000*                                                                 00054146
122000                                                                  00054210
122000                                                                  00054227
062100 CK-FOR-LOGIC-BUILD.                                              00054228
122000                                                                  00054229
064300                                                                  00054600
122000                                                                  00054610
122000     IF  bal-act-no =  SAVE-FULL-ACCT                             00054620
064400         GO TO CK-ISS-CANC.                                       00054621
122000                                                                  00054630
122000                                                                  00054631
122000     PERFORM  ACCT-BREAK   THRU   ACCT-BREAK-EXIT.                00054632
122000                                                                  00054633
122000                                                                  00054640
064400 CK-ISS-CANC.                                                     00054700
122000                                                                  00054701
062100     MOVE 'Y'  TO  WRITTEN-PREM-SW.                               00054702
064700                                                                  00054710
064800    GO  TO  CANCEL-BLD.                                           00054730
064500                                                                  00054800
080500 PROCESS-DETAILS.                                                 00064900
080600                                                                  00065000
080700*    DISPLAY 'PROCESS-DETAILS      ENTERED '.                     00065100
080900                                                                  00065200
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00065810
122900                                                                  00100500
151300                                                                  00100510
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00100520
152300* * * * * * * *   START  CANCELLATION PROCESSING  * * * * * * *   00100530
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00100540
152100                                                                  00100550
122900*                   ACARD                                         00100600
123000 CANCEL-BLD.                                                      00100700
088300                                                                  00100800
082100     MOVE  SPACES TO  PBI-RECORD-BODY.                            00100900
123100                                                                  00101000
123200*    DISPLAY 'CANCEL-BLD  ENTERED '.                              00101100
092500                                                                  00101200
088500     ADD  1  TO  CANC-CNT.                                        00101300
092500                                                                  00101310
           move bal-effdt (9:2)            to work-yr-x
           move bal-effdt (1:2)            to work-mo-x
           move bal-effdt (4:2)            to work-da-x
088500*    MOVE   V-ISS-YR                 TO  WORK-YR-X.               00101320
088600*    MOVE   V-ISS-MO                 TO  WORK-MO-X.               00101400
088700*    MOVE   V-ISS-DA                 TO  WORK-DA-X.               00101500
088700     MOVE   WORK-DATE-X              TO  PBI-CERT-EFF-DT-X.       00101600
090600     MOVE   bal-cert-no (1:10)       TO  SAVE-CERT.               00101610
089000                                                                  00101700
090600     MOVE   SAVE-CERT                TO  PBI-CERT-PRIME.          00101800
090600     MOVE   bal-cert-no (11:1)       TO  PBI-CERT-SFX.            00101900
088300                                                                  00102000
090600     MOVE   SPACES                   TO  PBI-C-LIVES-X.           00102100
090600     MOVE   '8'                      TO  PBI-C-FORCE-CD.          00102200
084000                                                                  00102300
125400     MOVE  bal-lname                 TO   PBI-C-INSURED-NAME.     00102400
125500                                                                  00102500
134300* *     FILL THE CANCEL LIFE FIELDS     * * * * * * * *           00102700
088300                                                                  00102800
           move bal-cncdt (7:2)            to work-yr-x
           move bal-cncdt (1:2)            to work-mo-x
           move bal-cncdt (4:2)            to work-da-x

088500*    MOVE   V-LF-CANC-YR             TO  WORK-YR-X.               00102900
088600*    MOVE   V-LF-CANC-MO             TO  WORK-MO-X.               00103000
088700*    MOVE   V-LF-CANC-DA             TO  WORK-DA-X.               00103100
134500     IF     WORK-DATE-N              NOT NUMERIC                  00103110
134500            MOVE  ZEROS              TO  WORK-DATE-N.             00103120
134900                                                                  00103300
134500*    IF    V-LF-CANC-REFUND          NOT  NUMERIC                 00103800
134500*          MOVE  ZEROS               TO  V-LF-CANC-REFUND.        00103801
134500     IF    bal-lfterm > zeros                                     00103802
134500           MOVE +9999999.99         TO  PBI-C-LF-PREM-REFUND      00103803
088700           MOVE WORK-DATE-N       TO  PBI-C-LF-CANCEL-DATE-X      00103200
           end-if
134500*    ADD   V-LF-CANC-REFUND          TO PBI-C-LF-PREM-REFUND.     00103900
128600                                                                  00104100
134300* *     FILL THE CANCEL A-H  FIELDS     * * * * * * * *           00104200
088300                                                                  00104300
134500     IF    bal-ahterm > zeros                                     00103802
134500           MOVE +9999999.99         TO  PBI-C-ah-PREM-REFUND      00103803
088700           MOVE WORK-DATE-N       TO  PBI-C-ah-CANCEL-DATE-X      00103200
           end-if
088700*    MOVE   WORK-DATE-N              TO  PBI-C-AH-CANCEL-DATE-X.  00104700
134900                                                                  00104900
134500*    IF    V-AH-CANC-REFUND          NOT NUMERIC                  00105010
134500*          MOVE  ZEROS               TO  V-AH-CANC-REFUND.        00105100
134500*    IF    PBI-C-AH-PREM-REFUND      NOT  NUMERIC                 00105200
134500*          MOVE  ZEROS               TO  PBI-C-AH-PREM-REFUND.    00105300
134500*    ADD  V-AH-CANC-REFUND           TO PBI-C-AH-PREM-REFUND.     00105400
093900                                                                  00105600
083700     MOVE '3'                        TO  PBI-C-TRANS-TYPE.        00105700
083700     MOVE '1'                        TO  PBI-C-SEQUENCE.          00105800
137000                                                                  00105900
140200 WRITE-CANCL-REC.                                                 00106000
137000                                                                  00106100
140200*    DISPLAY 'WRITE-CANCL-REC   ENTERED    '.                     00106200
148000*    DISPLAY  OUT-RECORD.                                         00106300
088300                                                                  00106400
           move bal-effdt (9:2)            to work-yr-x
           move bal-effdt (1:2)            to work-mo-x
           move bal-effdt (4:2)            to work-da-x
088700     MOVE   WORK-DATE-X              TO  PBI-CERT-EFF-DT-X.       00106800
089000                                                                  00107000
090600     MOVE   bal-cert-no (1:10)       TO  SAVE-CERT.               00101610
090600     MOVE   SAVE-CERT                TO  PBI-CERT-PRIME.          00107100
090600     MOVE   BAL-CERT-NO (11:1)       TO  PBI-CERT-SFX             00101610
090600*    MOVE   SPACES                   TO  PBI-CERT-SFX.            00107200
084000                                                                  00107300
010800     MOVE  PBI-C-CANCEL-REC-SEQ-1    TO  OUT-INFO.                00107310
010600     MOVE  SAVE-CERT                 TO  OUT-CERT-NO.             00107400
010600     MOVE  BAL-CERT-NO (11:1)        TO  OUT-CERT-SFX.            00107500
010700     MOVE  WORK-DATE-X               TO  OUT-EFF-DATE.            00107600
010800     MOVE  '3'                       TO  OUT-TRAN-TYPE.           00107800
010800     MOVE  '1'                       TO  OUT-SEQUENCE.            00107900
088300                                                                  00108000
144900     ADD   1                         TO  DETAIL-CNT               00108100
144900                                         DETAIL-CNT-TOT.          00108110
144900     MOVE  DETAIL-CNT                TO  SR-REC-CNT.              00108200
144900     MOVE  PS-BATCH-NUMB             TO  SR-BATCH-NUMB.           00108300
088300                                                                  00108400
148000     WRITE ballard-record-ot         FROM OUT-RECORD.             00108500
140300                                                                  00108600
143500     ADD  1                          TO  PS-CERT-CANC.            00108700
143600     ADD  PBI-C-LF-PREM-REFUND       TO  PS-LF-CANC.              00108800
143700     ADD  PBI-C-AH-PREM-REFUND       TO  PS-AH-CANC.              00108900
176500                                                                  00109000
143500     ADD  1                          TO TOT-CERT-CANC.            00109100
143600     ADD  PBI-C-LF-PREM-REFUND       TO TOT-LIFE-CANC.            00109200
143700     ADD  PBI-C-AH-PREM-REFUND       TO TOT-AH-CANC.              00109300
146200                                                                  00109500
146300     ADD   1                         TO RPT-CANCL-CNT.            00109600
146400     ADD   1                         TO RPT-LINE-CNT.             00109700
146500     IF   RPT-LINE-CNT               GREATER THAN  60             00109800
146600          PERFORM   005-ERR-RPT-HD   THRU ERR-RPT-HD-EXIT.        00109900
146800     MOVE  OUT-RECORD                TO  RPT-PT-LINE.             00110100
147100     MOVE  'CANCEL '                 TO  PT-ERROR.                00110200
147200                                                                  00110300
147900     MOVE  RPT-PT-LINE               TO  RPT-REC.                 00110400
148000     WRITE RPT-REC-OUT               AFTER ADVANCING 1  LINE.     00110500
148100     ADD   1                         TO  RPT-LINE-CNT.            00110600
148300     MOVE  SPACES                    TO RPT-REC.                  00110800
148400     WRITE RPT-REC-OUT               AFTER ADVANCING 1  LINE.     00110900
148500     MOVE  SPACES                    TO RPT-PT-LINE.              00111000
148600                                                                  00111100
143600     MOVE ZEROS                      TO PBI-C-LF-PREM-REFUND      00111110
143700                                        PBI-C-AH-PREM-REFUND      00111120
143600                                        PBI-C-LF-PREM-REFUND      00111150
143700                                        PBI-C-AH-PREM-REFUND.     00111160
137200                                                                  00111170
137200     GO  TO  010-READ-INPUT-FILE.                                 00111200
137200                                                                  00111300
137300 A-CARD-END.                                                      00111400
137400     EXIT.                                                        00111500
151300                                                                  00111600
152200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00111700
152300* * * * * * * *   END  CANCELLATION PROCESSING  * * * * * * * *   00111800
152400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00111900
152100                                                                  00112000
153000******* * * * * * * * * * * * * * * * * * * * * * * * * * * *     00112100
152100                                                                  00112200
153200 0150-WRITE-DATA-CARD.                                            00112800
153300                                                                  00112900
153400*    DISPLAY '0150-WRITE-DATA-CARD ENTERED  '.                    00113000
152600                                                                  00113100
157800 0150-WRITE.                                                      00113600
157900                                                                  00113700
158000*    DISPLAY 'ISSUE WRITE   ENTERED '.                            00113800
088300                                                                  00113900
168300*    DISPLAY  OUT-RECORD.                                         00114700
158100                                                                  00114800
144900     ADD  1                    TO  DETAIL-CNT                     00114900
144900                                   DETAIL-CNT-TOT.                00114910
144900     MOVE  DETAIL-CNT          TO  SR-REC-CNT.                    00115000
144900     MOVE  PS-BATCH-NUMB       TO  SR-BATCH-NUMB.                 00115100
168300     WRITE ballard-record-ot   FROM OUT-RECORD.                   00115200
158100                                                                  00115300
159400 0150-RPT-PRINT.                                                  00115600
159500                                                                  00115700
159600*    DISPLAY '0150-RPT-PRINT  ENTERED  '.                         00115800
159700                                                                  00115900
159800     ADD   1                   TO RPT-ISS-CNT.                    00116000
159900     ADD   1                   TO RPT-LINE-CNT.                   00116100
160000     IF   RPT-LINE-CNT GREATER THAN  60                           00116200
160100          PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.          00116300
160300     MOVE  OUT-RECORD          TO  RPT-PT-LINE.                   00116500
160300     MOVE  SAVE-SEQ            TO  PT-ERROR.                      00116600
161400     MOVE  RPT-PT-LINE         TO  RPT-REC.                       00116700
161500     WRITE RPT-REC-OUT AFTER ADVANCING 1  LINE.                   00116800
161600     ADD  1                    TO  RPT-LINE-CNT.                  00116900
161800     MOVE  SPACES              TO RPT-REC.                        00117100
161900     WRITE RPT-REC-OUT  AFTER ADVANCING 1  LINE.                  00117200
162000     MOVE  SPACES              TO RPT-PT-LINE.                    00117300
162100                                                                  00117400
165600 0150-EXIT.                                                       00117500
165700     EXIT.                                                        00117600
165800                                                                  00117800
165900 0200-ERRORS.                                                     00117900
166000                                                                  00118000
166100*    DISPLAY '0200-ERRORS  ENTERED '.                             00118100
166200                                                                  00118200
166300     DISPLAY '******************************************'.        00118300
166400     DISPLAY 'INPUT FILE HAS MORE THAN 10 ERRORS '.               00118400
166500     DISPLAY ' '.                                                 00118500
166600     DISPLAY '******************************************'.        00118600
166800                                                                  00118610
166700     CLOSE   ballard-in                                           00118700
167000             ballard-out                                          00118910
167100                                                                  00119000
167200     GO TO END-OF-JOB.                                            00119100
167300                                                                  00119200
167400 0200-EXIT.                                                       00119300
167500      EXIT.                                                       00119400
122000                                                                  00119410
122000                                                                  00119548
167600                                                                  00119550
064500 ACCT-BREAK.                                                      00119600
167600                                                                  00119700
158000*     DISPLAY 'ACCT-BREAK  ENTERED '.                             00119800
158100                                                                  00119900
167600      MOVE   '0'               TO    PS-SEQUENCE-NO.              00120000
167600      MOVE   SAVE-FULL-ACCT    TO    PS-ACCT-NO                   00120101
064500      MOVE   SAVE-STATE        TO    PS-STATE                     00120240
064500      MOVE   '1'               TO    PS-TRANS-TYPE.               00120250
064500      MOVE   '0'               TO    PS-SEQUENCE-NO.              00120260
064500      MOVE   'CSO'             TO    PS-CLIENT-ID.                00120270
064500      MOVE   '9000000'         TO    PS-CARR-CO.                  00120280
158000                                                                  00122200
167600      IF  SAVE-FULL-ACCT  =  ZEROS OR SPACES                      00122210
167600        MOVE  ZEROS TO    PS-BATCH-NO                             00122211
158000        DISPLAY ' '                                               00122212
158000        DISPLAY 'PACESETR-BATCH-HDR IS ZEROS  ' PACESETR-BATCH-HDR00122213
158000         GO TO SKIP-WRITE.                                        00122220
158000                                                                  00122230
158000      MOVE    PACESETR-BATCH-HDR TO OUT-RECORD.                   00122300
167600      MOVE     PS-BATCH-NUMB   TO   SR-BATCH-NUMB.                00122400
157900      MOVE    ZEROS            TO   SR-REC-CNT.                   00122500
167600                                                                  00122600
168300      WRITE ballard-record-ot  FROM OUT-RECORD.                   00122700
167600                                                                  00122710
064500 SKIP-WRITE.                                                      00122711
167600                                                                  00122712
158000      DISPLAY ' '.                                                00122720
158000      DISPLAY 'PACESETR-BATCH-HDR  ' PACESETR-BATCH-HDR.          00122730
158100                                                                  00122800
167600      ADD    1                 TO    PS-BATCH-NO.                 00122900
167600      MOVE  ZEROS              TO    PS-LF-WRITTEN                00123000
167600                                     PS-LF-CANC                   00123100
167600                                     PS-AH-WRITTEN                00123200
167600                                     PS-AH-CANC                   00123300
167600                                     PS-CERT-ISS                  00123400
167600                                     PS-CERT-CANC                 00123500
167600                                    CERT-CNT                      00123600
167600                                    CANC-CNT                      00123700
167600                                    DETAIL-CNT.                   00123800
167600                                                                  00123801
062100      IF  LAST-REC-SW = 'Y'                                       00123802
064500          GO TO ACCT-BREAK-EXIT.                                  00123803
167600                                                                  00123804
058700      MOVE  RPT-DT (SAVE-WORK-MO) TO RPT-REPORT-DATE.             00123810
167600                                                                  00123811
064500      MOVE  bal-state             TO  SAVE-STATE.                 00123832
064500      MOVE  bal-state             TO   PS-STATE.                  00123833
167600                                                                  00123834
122000      MOVE  bal-act-no            TO  SAVE-FULL-ACCT.             00123840
167600                                                                  00123900
064500 ACCT-BREAK-EXIT.                                                 00124000
064500      EXIT.                                                       00124100
167600                                                                  00124200
064500                                                                  00126700
170500 END-OF-JOB SECTION.                                              00127600
158100                                                                  00127601
167900*    DISPLAY 'END-OF-JOB   ENTERED '.                             00127602
158100                                                                  00127610
063500      CLOSE ballard-in                                            00127620
063500*           PPA-OUT                                               00127640
063500            ballard-out.                                          00127650
062100                                                                  00127680
171000     DISPLAY '************************************************'.  00128000
171100     DISPLAY 'INPUT RECORDS      -- ' IN-CNT.                     00128100
171200     DISPLAY ' '.                                                 00128200
171300     DISPLAY 'RECS WRITTEN COUNT -- ' DETAIL-CNT-TOT.             00128300
171400     DISPLAY ' '.                                                 00128400
171500     DISPLAY 'ERROR RECORD COUNT -- ' RPT-CNT.                    00128500
171600     DISPLAY ' '.                                                 00128600
171700     DISPLAY '************************************************'.  00128700
171800                                                                  00128701
171300     MOVE ZEROS TO  DETAIL-CNT-TOT.                               00128710
172000                                                                  00129000
172200     MOVE  RPT-PAGE-CNT     TO RPT-PAGE.                          00129100
172300     MOVE  4                TO RPT-LINE-CNT.                      00129200
172500     MOVE  '   PACESETR CORP INPUT TOTALS FOR ' TO  PS-RPT-ID.    00129400
172600     MOVE  RPT-HD-LINE1     TO RPT-REC.                           00129500
172700     WRITE RPT-REC-OUT      AFTER ADVANCING PAGE.                 00129600
172900     MOVE  SPACES           TO RPT-REC.                           00129800
173000     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00129900
173200     MOVE  REPORT-HD-LINE   TO RPT-REC.                           00130100
173300     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00130200
173400     ADD  1                 TO  RPT-LINE-CNT.                     00130300
173600     MOVE  SPACES           TO RPT-REC.                           00130500
173700     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00130600
177200                                                                  00130700
177300     MOVE  TOT-LIFE-WRITTEN TO T-LIFE-WRITTEN.                    00130800
177400     MOVE  TOT-LIFE-CANC    TO T-LIFE-CANC.                       00130900
177500     MOVE  TOT-AH-WRITTEN   TO T-AH-WRITTEN.                      00131000
177600     MOVE  TOT-AH-CANC      TO T-AH-CANC.                         00131100
177700     MOVE  TOT-CERT-ISS     TO T-CERT-ISS.                        00131200
177800     MOVE  TOT-CERT-CANC    TO T-CERT-CANC.                       00131300
177900                                                                  00131400
178100     MOVE  TOTAL-LINE       TO RPT-REC.                           00131600
178200     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00131700
178300     ADD  1                 TO  RPT-LINE-CNT.                     00131800
178500     MOVE  SPACES           TO RPT-REC.                           00132000
178600     WRITE RPT-REC-OUT      AFTER ADVANCING 1  LINE.              00132100
178700                                                                  00132200
178800     CLOSE RPT-FILE.                                              00132300
178900                                                                  00132400
179000     GOBACK.                                                      00132500
179100                                                                  00132600
