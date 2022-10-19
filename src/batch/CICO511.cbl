000100 IDENTIFICATION DIVISION.                                         00000100
000200 PROGRAM-ID.                 CICO511.                             00000200
000300                                                                  00000300
000400*AUTHOR.     CENTRAL STATES OF OMAHA.                             00000400
000500*            OMAHA, NEBR.                                         00000500
000600                                                                  00000600
000900*REMARKS.                                                         00000900
001000*        THIS PROGRAM GENERATES ISSUE/CANCEL TRANSACTIONS FROM    00001000
001100*          CREDIT INSURANCE ASSOCIATES (CONLEY) FOR INPUT INTO    00001100
001200*            THE LOGIC SYSTEM THRU LOGIC PROGRAM 'EL512'.         00001200
001300*              (IT IS ONE CSO VERSION OF 'EL511').                00001300
001400*                     =========================                   00001400

      ******************************************************************
      *                   C H A N G E   L O G
      *
      * CHANGES ARE MARKED BY THE CHANGE EFFECTIVE DATE.
      *-----------------------------------------------------------------
      *  CHANGE   CHANGE REQUEST PGMR  DESCRIPTION OF CHANGE
      * EFFECTIVE    NUMBER
      *-----------------------------------------------------------------
      * 102202                   SMVA  CHG PGM ID TO CICO511 IN RPT-HD-
101107* 101107    2007010300001  PEMA  CHG BATCH # ASSIGNMENT METHOD
112007* 112007    2007010300001  PEMA  REMOVE BATCH # ASSIGNMENT
051508* 051508    2008010200006  PEMA  ADD FIRST NAME PROCESSING
032109* 032109    2009021700002  PEMA  ADD CRED BENE TO PNDB REC
050509* 050509    2009021700002  PEMA  ADD BENE FROM CONLEY FILE
030310* 030310    2010010400006  PEMA  INCREASE LEN OF CERTNO
040414* 040414  CR2014040400001  PEMA  ADD JNT BIRTHDATE
011121* 011121  CR2021011100001  PEMA  Expand account number size
      ******************************************************************
001500                                                                  00001500
001600 ENVIRONMENT DIVISION.                                            00001600
001700 INPUT-OUTPUT SECTION.                                            00001700
001800 FILE-CONTROL.                                                    00001800
001900     SELECT  STATE-CODES     ASSIGN TO SYS006
                organization is line sequential.
002000     SELECT  CIA-LIFE-IN     ASSIGN TO SYS010-UT-2400-S-SYS010.   00002000
002100     SELECT  CIA-LIFE-OUT    ASSIGN TO SYS012-UT-2400-S-SYS012.   00002100
002200     SELECT  RPT-FILE        ASSIGN TO SYS013-UR-1403-S-SYS013.   00002200
002300*    SELECT  REPORT-FILE     ASSIGN TO SYS008-UR-1403-S-SYS008.   00002300
002400     SELECT  ERACCT          ASSIGN TO SYS021-FBA1-ERACCT         00002400
002500                             ORGANIZATION IS INDEXED              00002500
002600                             ACCESS IS SEQUENTIAL                 00002600
002700                             RECORD KEY IS AM-CONTROL-PRIMARY     00002700
002800                             FILE STATUS IS ERACCT-FILE-STATUS.   00002800
002900                                                                  00002900
003000 SKIP3                                                            00003000
003100 DATA DIVISION.                                                   00003100
003200                                                                  00003200
003300 FILE SECTION.                                                    00003300
003400                                                                  00003400
003500******************************************************************00003500
003600**  ONLINE ACCOUNT MASTER FILE                                    00003600
003700******************************************************************00003700
003800                                                                  00003800
uktdel*FD  ERACCT                                                       00003900
uktins FD  ERACCT.
004100                                                                  00004100
004200                   COPY ERCACCT.                                  00004200
004300                                                                  00004300
004400******************************************************************00004400
004500**  INPUT TAPE FILE FROM CREDIT INSURANCE ASSOCIATES (CONLEY)     00004500
004600******************************************************************00004600
004700                                                                  00004700
004800 FD  CIA-LIFE-IN                                                  00004800
004900     RECORDING MODE IS F                                          00004900
005000     LABEL RECORDS ARE STANDARD                                   00005000
050509*    RECORD CONTAINS 258 CHARACTERS                               00005100
005200     BLOCK CONTAINS 0 RECORDS                                     00005200
005300     DATA RECORD IS CIA-LIFE-RECORD.                              00005300
005400                                                                  00005400
040414 01  CIA-LIFE-RECORD            PIC X(316).                       00005500
005600                                                                  00005600
005700******************************************************************00005700
005800**       OUTPUT CARD FILE FOR INPUT TO PROGRAM 'CL512'          **00005800
005900******************************************************************00005900
006000                                                                  00006000
006100 FD  CIA-LIFE-OUT                                                 00006100
006200     RECORDING MODE IS F                                          00006200
006300     LABEL RECORDS ARE STANDARD                                   00006300
006400     RECORD CONTAINS 90 CHARACTERS                                00006400
006500     BLOCK CONTAINS 0 RECORDS                                     00006500
006600     DATA RECORD IS CARD-RECORD.                                  00006600
006700                                                                  00006700
112007 01  CARD-RECORD             PIC X(101).                          00006800
006900                                                                  00006900
007000******************************************************************00007000
007100**                 OUTPUT ERROR REPORT                          **00007100
007200******************************************************************00007200
007300                                                                  00007300
007400 FD  RPT-FILE                                                     00007400
007500     RECORDING MODE IS F                                          00007500
007600     LABEL RECORDS ARE STANDARD                                   00007600
007700     RECORD CONTAINS 133 CHARACTERS                               00007700
007800     BLOCK CONTAINS 0 RECORDS                                     00007800
007900     DATA RECORD IS RPT-REC-OUT.                                  00007900
008000                                                                  00008000
008100 01  RPT-REC-OUT.                                                 00008100
008300     05  RPT-REC                  PIC X(132).                     00008300
008400                                                                  00008400
008500******************************************************************00008500
008600**                 STATE CODE FILE                              **00008600
008700******************************************************************00008700
008800                                                                  00008800
008900 FD  STATE-CODES                                                  00008900
009000     RECORDING MODE IS F                                          00009000
009100     LABEL RECORDS ARE STANDARD                                   00009100
009200     RECORD CONTAINS 80 CHARACTERS                                00009200
009300     BLOCK CONTAINS 0 RECORDS                                     00009300
009400     DATA RECORD IS ST-REC-IN.                                    00009400
009500                                                                  00009500
009600 01  ST-REC-IN.                                                   00009600
009700     05  STATE-ID                 PIC XX.                         00009700
009800     05  ST-NAME                  PIC X(30).                      00009800
009900     05  FILLER                   PIC X(48).                      00009900
010000                                                                  00010000
010100******************************************************************00010100
010200                                                                  00010200
010300 SKIP3                                                            00010300
010400 WORKING-STORAGE SECTION.                                         00010400
010500 77  FILLER  PIC X(32) VALUE '********************************'.  00010500
010600 77  FILLER  PIC X(32) VALUE '         WORKING-STORAGE        '.  00010600
010700 77  FILLER  PIC X(32) VALUE '********************************'.  00010700
010800                                                                  00010800
011100 77  ERACCT-FILE-STATUS     PIC XX        VALUE SPACES.           00011100
081406 77  S1                     PIC S999    VALUE +0 COMP-3.
011200 77  ST-SUB                 PIC 99        VALUE ZEROS.            00011200
011400 77  SAVE-STATE             PIC XX        VALUE SPACES.           00011400
011500 77  ACCT-HOLD              PIC X(06)     VALUE SPACES.           00011500
011600 77  LIFE-TYPE-SW           PIC X         VALUE 'Y'.              00011600
011700 77  CIA-ACCT-SW            PIC X         VALUE 'Y'.              00011700
011800 77  ACCT-OK                PIC X         VALUE 'N'.              00011800
011900 77  SAVE-SEQ               PIC X(30)     VALUE SPACES.           00011900
012000 77  RPT-SUB                PIC 99        VALUE ZEROS.            00012000
012100 77  AGT-SUB                PIC 99        VALUE ZEROS.            00012100
012200 77  PRINT-SUB              PIC 99        VALUE ZEROS.            00012200
012300 77  JI-SUB                 PIC 99        VALUE ZEROS.            00012300
012400 77  LN-SUB                 PIC 99        VALUE ZEROS.            00012400
012500 77  RPT-CNT                PIC 9999      VALUE ZEROS.            00012500
012600 77  RPT-PAGE-CNT           PIC 9999      VALUE ZEROS.            00012600
012700 77  RPT-CIA-CNT            PIC 99999     VALUE ZEROS.            00012700
012800 77  RPT-ISS-CNT            PIC 99999     VALUE ZEROS.            00012800
012900 77  RPT-LINE-CNT           PIC 99        VALUE ZEROS.            00012900
013000 77  DETAIL-CNT             PIC 9999      VALUE ZEROS.            00013000
013100 77  TOT-DETAIL-CNT         PIC 99999     VALUE ZEROS.            00013100
013200 77  ACCT-MATCH             PIC 99999     VALUE ZEROS.            00013200
013300 77  ACCT-MASTER-LOW        PIC 99999     VALUE ZEROS.            00013300
013400 77  ACCT-MASTER-HI         PIC 99999     VALUE ZEROS.            00013400
013500 77  IN-CNT                 PIC 9999999   VALUE ZEROS.            00013500
013600 77  TOT-LIFE-WRITTEN       PIC S9(8)V99  VALUE ZEROS.            00013600
013700 77  TOT-LIFE-CANC          PIC S9(8)V99  VALUE ZEROS.            00013700
013800 77  TOT-AH-WRITTEN         PIC S9(8)V99  VALUE ZEROS.            00013800
013900 77  TOT-AH-CANC            PIC S9(8)V99  VALUE ZEROS.            00013900
014000 77  TOT-CERT-ISS           PIC S9(4)     VALUE ZEROS.            00014000
014100 77  TOT-CERT-CANC          PIC S9(4)     VALUE ZEROS.            00014100
014200 77  SUB1                   PIC 99        VALUE ZEROS.            00014200
014300 77  CANC-CNT               PIC 9999      VALUE ZEROS.            00014300
014400 77  HDR-CNT                PIC 9999      VALUE ZEROS.            00014400
014500 77  CERT-CNT               PIC 9999      VALUE ZEROS.            00014500
014600 77  FIRST-SW               PIC X         VALUE 'Y'.              00014600
014700 77  CODE-SUB               PIC 999       VALUE ZEROS.            00014700
014900 77  ST-ID-SUB              PIC 99        VALUE ZEROS.            00014900
040414 77  ws-bin-eff-dt          pic xx        value low-values.
015400                                                                  00015400
015500 01  FIX-BATCH-NO.                                                00015500
015600     05  FIRST-2            PIC X(02)     VALUE SPACES.           00015600
015600     05  SECOND-2           PIC X(02)     VALUE SPACES.           00015610
015700     05  LAST-2             PIC X(02)     VALUE SPACES.           00015700
015800                                                                  00015800
015900 01  SAVE-JOINT-LNAME       PIC X(12).                            00015900
016000 01  FILLER  REDEFINES SAVE-JOINT-LNAME.                          00016000
016100     05  S-J-LNAME    OCCURS 12      PIC X(01).                   00016100
016200                                                                  00016200
016300 01  PULL-JNT-1ST-INIT      PIC X(12).                            00016300
016400 01  FILLER  REDEFINES PULL-JNT-1ST-INIT.                         00016400
016500     05  JNT-1ST-INIT OCCURS 12      PIC X(01).                   00016500
016600                                                                  00016600
016700 01  ACCT-COMP-CD           PIC S9(04) COMP  VALUE +04.           00016700
016800 01  FILLER  REDEFINES ACCT-COMP-CD.                              00016800
016900     05  FILLER             PIC X(01).                            00016900
017000     05  ACCT-CO-CD         PIC X(01).                            00017000
017100                                                                  00017100
017200 01  SAVE-STATE-TABLE.                                            00017200
017300     05  SAVE-ST-ID      OCCURS 80    PIC XX.                     00017300
017400                                                                  00017400
017500 01  STATE-TABLE.                                                 00017500
017600     05  ST-TABLE      OCCURS 80    PIC XX.                       00017600
017700                                                                  00017700
017800                                                                  00017800
017900 01  WORK-ACCT.                                                   00017900
018000     05  WORK-ACCT-CO       PIC X(01)     VALUE SPACES.           00018000
018100     05  WORK-ACCT-GROUP    PIC X(06)     VALUE SPACES.           00018100
018200     05  WORK-ACCT-STATE    PIC X(02)     VALUE SPACES.           00018200
018300     05  WORK-ACCT-ACCT.                                          00018300
018400         10  WORK-ACCT-PRE  PIC X(02)     VALUE SPACES.           00018400
018500         10  WORK-ACCT-PRIM PIC X(06)     VALUE SPACES.           00018500
018600                                                                  00018600
018700                                                                  00018700
018800 01  TEST-CONTROL-A.                                              00018800
018900     05  TEST-CARR-A        PIC X(01)     VALUE SPACES.           00018900
019000     05  TEST-GRP-A         PIC X(06)     VALUE SPACES.           00019000
019100     05  TEST-STATE-A       PIC X(02)     VALUE SPACES.           00019100
019200     05  TEST-ACCT-ACCT.                                          00019200
019300         10  TEST-PRE-A     PIC X(04)     VALUE SPACES.           00019300
019400         10  TEST-ACCT-A    PIC X(06)     VALUE SPACES.           00019400
020200                                                                  00020200
020300 01  SAVE-CERT-ALL.                                               00020300
020400     05  SAVE-CERT.                                               00020400
030310         10  SAVE-CERT01    PIC X         VALUE SPACES.           00020500
030310         10  SAVE-CERT09    PIC X(09)     VALUE SPACES.           00020600
020700     05  SAVE-CERTSFX       PIC X(01)     VALUE SPACES.           00020700
020800     05  SAVE-CERTSFX-N REDEFINES SAVE-CERTSFX   PIC 9.           00020800
020900                                                                  00020900
021000 01  S-C-ALL    REDEFINES   SAVE-CERT-ALL.                        00021000
021100     05  S-CERT.                                                  00021100
021200         10  S-CERT01       PIC X(01).                            00021200
021300         10  S-CERT10       PIC X(10).                            00021300
021400                                                                  00021400
021500 01  OUT-RECORD.                                                  00021500
021600     05  OUT-CERT-NO        PIC X(10)     VALUE SPACES.           00021600
021700     05  OUT-CERT-SFX       PIC X(01)     VALUE SPACES.           00021700
021800     05  OUT-EFF-DATE       PIC X(06)     VALUE SPACES.           00021800
021900     05  OUT-INFO           PIC X(61)     VALUE SPACES.           00021900
022000     05  OUT-TRAN-TYPE      PIC X         VALUE SPACES.           00022000
022100     05  OUT-SEQUENCE       PIC X         VALUE SPACES.           00022100
           05  SR-ACCT-NO         PIC X(10)     VALUE SPACES.
           05  SR-CERT-NO         PIC X(10)     VALUE ZEROS.
           05  SR-CERT-SFX        PIC X         VALUE ZEROS.
022200*    05  SR-BATCH-NO        PIC 9(6)      VALUE ZEROS.            00022200
022300*    05  SR-REC-CNT         PIC 9(4)      VALUE ZEROS.            00022300
022400                                                                  00022400
022500 01  SAVE-FULL-ACCT.                                              00022500
022600     05 SAVE-ACCT           PIC XXXX      VALUE SPACES.           00022600
022700     05 SAVE-ACCT-6         PIC X(06)     VALUE SPACES.           00022700
022800                                                                  00022800
022900 01  CIA-BATCH-HDR.                                               00022900
023000     05  CIA-CARR-CO.                                             00023000
023100         10  CIA-CARR        PIC X         VALUE '9'.             00023100
023200         10  CIA-COMP        PIC X(6)      VALUE '000000'.        00023200
023300     05  CIA-STATE           PIC XX        VALUE '  '.            00023300
023400     05  CIA-ACCT-NO         PIC X(10)     VALUE SPACES.          00023400
023500     05  CIA-BATCH-NO        PIC 9(06)     VALUE ZEROS.           00023500
023600     05  CIA-HD-EFF-DT.                                           00023600
023700         10  CIA-HD-EFF-MO   PIC XX        VALUE SPACES.          00023700
023800         10  CIA-HD-EFF-DA   PIC XX        VALUE SPACES.          00023800
023900         10  CIA-HD-EFF-YR   PIC XX        VALUE SPACES.          00023900
024000     05  CIA-CERT-ISS        PIC S9(4)     VALUE ZEROS.           00024000
024100     05  CIA-LIFE-WRITTEN    PIC S9(7)V99  VALUE ZEROS.           00024100
024200     05  CIA-AH-WRITTEN      PIC S9(7)V99  VALUE ZEROS.           00024200
024300     05  CIA-CERT-CANC       PIC S9(4)     VALUE ZEROS.           00024300
024400     05  CIA-LIFE-CANC       PIC S9(7)V99  VALUE ZEROS.           00024400
024500     05  CIA-AH-CANC         PIC S9(7)V99  VALUE ZEROS.           00024500
024600     05  CIA-CLIENT-ID       PIC X(3)      VALUE 'CSO'.           00024600
024700     05  CIA-TRANS-TYPE      PIC X         VALUE '1'.             00024700
024800     05  CIA-SEQUENCE-NO     PIC X         VALUE '0'.             00024800
024900                                                                  00024900
025000 01  REPORT-HD-LINE.                                              00025000
025100     05  FILLER              PIC X(14)  VALUE ' LIFE WRITTEN '.   00025100
025200     05  FILLER              PIC XX              VALUE SPACES.    00025200
025300     05  FILLER              PIC X(14)  VALUE '     LIFE CANC'.   00025300
025400     05  FILLER              PIC XX              VALUE SPACES.    00025400
025500     05  FILLER              PIC X(14)  VALUE '  A&H WRITTEN '.   00025500
025600     05  FILLER              PIC XX              VALUE SPACES.    00025600
025700     05  FILLER              PIC X(14)  VALUE '      A&H CANC'.   00025700
025800     05  FILLER              PIC XX              VALUE SPACES.    00025800
025900     05  FILLER              PIC X(8)   VALUE ' ISS CNT'.         00025900
026000     05  FILLER              PIC XX              VALUE SPACES.    00026000
026100     05  FILLER              PIC X(8)   VALUE 'CANC CNT'.         00026100
026200     05  FILLER              PIC XX              VALUE SPACES.    00026200
026300     05  FILLER              PIC X(7)   VALUE SPACES.             00026300
026400     05  FILLER              PIC XX              VALUE SPACES.    00026400
026500     05  FILLER              PIC XXXXX  VALUE SPACES.             00026500
026600     05  FILLER              PIC XX              VALUE SPACES.    00026600
026700     05  FILLER              PIC X(6)   VALUE SPACES.             00026700
026800     05  FILLER              PIC XX              VALUE SPACES.    00026800
026900     05  HD-EFF-DT           PIC X(8)            VALUE SPACES.    00026900
027000                                                                  00027000
027100 01  TOTAL-LINE.                                                  00027100
027200     05  T-LIFE-WRITTEN      PIC ZZ,ZZZ,ZZ9.99-.                  00027200
027300     05  FILLER              PIC XX              VALUE SPACES.    00027300
027400     05  T-LIFE-CANC         PIC ZZ,ZZZ,ZZ9.99-.                  00027400
027500     05  FILLER              PIC XX              VALUE SPACES.    00027500
027600     05  T-AH-WRITTEN        PIC ZZ,ZZZ,ZZ9.99-.                  00027600
027700     05  FILLER              PIC XX              VALUE SPACES.    00027700
027800     05  T-AH-CANC           PIC ZZ,ZZZ,ZZ9.99-.                  00027800
027900     05  FILLER              PIC XXXX            VALUE SPACES.    00027900
028000     05  T-CERT-ISS          PIC ZZZ,ZZ9.                         00028000
028100     05  FILLER              PIC XXXX            VALUE SPACES.    00028100
028200     05  T-CERT-CANC         PIC Z,ZZ9.                           00028200
028300     05  FILLER              PIC X(39)           VALUE SPACES.    00028300
028400                                                                  00028400
028500 01  RPT-TOT-LINE.                                                00028500
028600     05  FILLER              PIC X(10)           VALUE SPACES.    00028600
028700     05  FILLER              PIC X(35)  VALUE                     00028700
028800         ' NUMBER OF ISSUES WRITTEN IS - '.                       00028800
028900     05  P-ISS-CNT           PIC ZZ,ZZ9.                          00028900
029000     05  FILLER              PIC X(10)           VALUE SPACES.    00029000
029100     05  FILLER              PIC X(35)  VALUE                     00029100
029200         'NUMBER OF CANCELS WRITTEN IS - '.                       00029200
029300     05  P-CIA-CNT           PIC ZZ,ZZ9.                          00029300
029400     05  FILLER              PIC X(10)            VALUE SPACES.   00029400
029500     05  FILLER              PIC X(10)            VALUE SPACES.   00029500
029600                                                                  00029600
029700 01  RPT-HD-LINE1.                                                00029700
029800     05  FILLER              PIC X(26)           VALUE SPACES.    00029800
029900     05  CIA-RPT-ID          PIC X(36)  VALUE                     00029900
030000         '     CONLEY (C. I. A.)  REPORT FOR '.                   00030000
030100     05  RPT-REPORT-DATE     PIC X(14)           VALUE SPACES.    00030100
030200     05  FILLER              PIC X(16)           VALUE SPACES.    00030200
030300     05  FILLER              PIC X(15)           VALUE            00030300
102202         'PGM = CICO511  '.                                       00030400
030500     05  FILLER              PIC X(12)           VALUE SPACES.    00030500
030600     05  FILLER              PIC X(5)   VALUE         'PAGE '.    00030600
030700     05  RPT-PAGE            PIC Z,ZZ9.                           00030700
030800     05  FILLER              PIC X(5)            VALUE SPACES.    00030800
030900                                                                  00030900
031000 01  RPT-HD-LINE2.                                                00031000
031100     05  FILLER              PIC X(10)  VALUE 'CERT #  '.         00031100
031200     05  FILLER              PIC X(12)  VALUE ' DETAIL INFO'.     00031200
031300     05  FILLER              PIC X(09)  VALUE ' '.                00031300
031400     05  FILLER              PIC X(5)   VALUE SPACES.             00031400
031500     05  FILLER              PIC X(20)  VALUE SPACES.             00031500
031600     05  FILLER              PIC X(20)  VALUE SPACES.             00031600
031700     05  FILLER              PIC X(40)  VALUE                     00031700
031800                 '              RECORD TYPE    '.                 00031800
031900     05  FILLER              PIC X(16)  VALUE SPACES.             00031900
032000                                                                  00032000
032100 01  RPT-PT-LINE.                                                 00032100
032200     05  PT-REC              PIC X(80)  VALUE SPACES.             00032200
032300     05  FILLER              PIC X(05)  VALUE SPACES.             00032300
032400     05  PT-ERROR            PIC X(40)  VALUE SPACES.             00032400
032500     05  FILLER              PIC X(05)  VALUE SPACES.             00032500
032600                                                                  00032600
032700 01  ST-PRINT-LINE.                                               00032700
032800     05  PT-MSG              PIC X(38)  VALUE SPACES.             00032800
032900     05  PT-STATE            PIC X(02)  VALUE SPACES.             00032900
033000     05  FILLER              PIC X(04)  VALUE SPACES.             00033000
033100     05  NEW-ST-LABEL        PIC X(17)  VALUE                     00033100
033200           ' NEW STATE ADDED '.                                   00033200
033300                                                                  00033300
033400 01  WK-DATE.                                                     00033400
033500     05  WK-MO               PIC XX     VALUE SPACES.             00033500
033600     05  FILLER              PIC X      VALUE SPACES.             00033600
033700     05  WK-DA               PIC XX     VALUE SPACES.             00033700
033800     05  FILLER              PIC X      VALUE SPACES.             00033800
033900     05  WK-YR               PIC XX     VALUE SPACES.             00033900
034000                                                                  00034000
034100 01  WORK-DATE.                                                   00034100
034200     12  WORK-DATE-X.                                             00034200
034300         15  WORK-MO-X       PIC XX.                              00034300
034400         15  WORK-DA-X       PIC XX.                              00034400
034500         15  WORK-YR-X       PIC XX.                              00034500
034600     12  WORK-DATE-N    REDEFINES   WORK-DATE-X.                  00034600
034700         15  WORK-MO-N       PIC 99.                              00034700
034800         15  WORK-DA-N       PIC 99.                              00034800
034900         15  WORK-YR-N       PIC 99.                              00034900
035000                                                                  00035000
034600     12  WORK-DATE-IN.                                            00034600
034700         15  WORK-YR-IN      PIC 99.                              00034700
034800         15  WORK-MO-IN      PIC 99.                              00034800
034900         15  WORK-DA-IN      PIC 99.                              00034900
035000                                                                  00035000
035100 01  WORK-ST.                                                     00035100
035200     12  WORK-STATE-X.                                            00035200
035300         15  WORK-ST-X       PIC XX.                              00035300
035400     12  WORK-STATE-N    REDEFINES   WORK-STATE-X.                00035400
035500         15  WORK-ST-N       PIC 99.                              00035500
035600                                                                  00035600
035700 01  FILLER.                                                      00035700
035800     05  STATE-ID-FILE.                                           00035800
035900         10  FILLER          PIC XX      VALUE 'AL'.              00035900
036000         10  FILLER          PIC XX      VALUE 'AK'.              00036000
036100         10  FILLER          PIC XX      VALUE 'AZ'.              00036100
036200         10  FILLER          PIC XX      VALUE 'AR'.              00036200
036300         10  FILLER          PIC XX      VALUE 'CA'.              00036300
036400         10  FILLER          PIC XX      VALUE 'CO'.              00036400
036500         10  FILLER          PIC XX      VALUE 'CT'.              00036500
036600         10  FILLER          PIC XX      VALUE 'DE'.              00036600
036700         10  FILLER          PIC XX      VALUE 'FL'.              00036700
036800         10  FILLER          PIC XX      VALUE 'GA'.              00036800
036900         10  FILLER          PIC XX      VALUE 'HI'.              00036900
037000         10  FILLER          PIC XX      VALUE 'ID'.              00037000
037100         10  FILLER          PIC XX      VALUE 'IL'.              00037100
037200         10  FILLER          PIC XX      VALUE 'IN'.              00037200
037300         10  FILLER          PIC XX      VALUE 'IA'.              00037300
037400         10  FILLER          PIC XX      VALUE 'KS'.              00037400
037500         10  FILLER          PIC XX      VALUE 'KY'.              00037500
037600         10  FILLER          PIC XX      VALUE 'LA'.              00037600
037700         10  FILLER          PIC XX      VALUE 'ME'.              00037700
037800         10  FILLER          PIC XX      VALUE 'MD'.              00037800
037900         10  FILLER          PIC XX      VALUE 'MA'.              00037900
038000         10  FILLER          PIC XX      VALUE 'MI'.              00038000
038100         10  FILLER          PIC XX      VALUE 'MN'.              00038100
038200         10  FILLER          PIC XX      VALUE 'MS'.              00038200
038300         10  FILLER          PIC XX      VALUE 'MO'.              00038300
038400         10  FILLER          PIC XX      VALUE 'MT'.              00038400
038500         10  FILLER          PIC XX      VALUE 'NE'.              00038500
038600         10  FILLER          PIC XX      VALUE 'NV'.              00038600
038700         10  FILLER          PIC XX      VALUE 'NH'.              00038700
038800         10  FILLER          PIC XX      VALUE 'NJ'.              00038800
038900         10  FILLER          PIC XX      VALUE 'NM'.              00038900
039000         10  FILLER          PIC XX      VALUE 'NY'.              00039000
039100         10  FILLER          PIC XX      VALUE 'NC'.              00039100
039200         10  FILLER          PIC XX      VALUE 'ND'.              00039200
039300         10  FILLER          PIC XX      VALUE 'OH'.              00039300
039400         10  FILLER          PIC XX      VALUE 'OK'.              00039400
039500         10  FILLER          PIC XX      VALUE 'OR'.              00039500
039600         10  FILLER          PIC XX      VALUE 'PA'.              00039600
039700         10  FILLER          PIC XX      VALUE 'RI'.              00039700
039800         10  FILLER          PIC XX      VALUE 'SC'.              00039800
039900         10  FILLER          PIC XX      VALUE 'SD'.              00039900
040000         10  FILLER          PIC XX      VALUE 'TN'.              00040000
040100         10  FILLER          PIC XX      VALUE 'TX'.              00040100
040200         10  FILLER          PIC XX      VALUE 'UT'.              00040200
040300         10  FILLER          PIC XX      VALUE 'VT'.              00040300
040400         10  FILLER          PIC XX      VALUE 'VA'.              00040400
040500         10  FILLER          PIC XX      VALUE 'WA'.              00040500
040600         10  FILLER          PIC XX      VALUE 'WV'.              00040600
040700         10  FILLER          PIC XX      VALUE 'WI'.              00040700
040800         10  FILLER          PIC XX      VALUE 'WY'.              00040800
040900         10  FILLER          PIC XX      VALUE 'PR'.              00040900
041000         10  FILLER          PIC XX      VALUE 'DC'.              00041000
041100         10  FILLER          PIC XX      VALUE '**'.              00041100
041200         10  FILLER          PIC XX      VALUE '  '.              00041200
041300         10  FILLER          PIC XX      VALUE 'GU'.              00041300
041400         10  FILLER          PIC XX      VALUE 'VI'.              00041400
041500         10  FILLER          PIC XX      VALUE '  '.              00041500
041600         10  FILLER          PIC XX      VALUE '  '.              00041600
041700         10  FILLER          PIC XX      VALUE 'AB'.              00041700
041800         10  FILLER          PIC XX      VALUE 'BC'.              00041800
041900         10  FILLER          PIC XX      VALUE 'MB'.              00041900
042000         10  FILLER          PIC XX      VALUE 'NB'.              00042000
042100         10  FILLER          PIC XX      VALUE 'NF'.              00042100
042200         10  FILLER          PIC XX      VALUE 'NT'.              00042200
042300         10  FILLER          PIC XX      VALUE 'NS'.              00042300
042400         10  FILLER          PIC XX      VALUE 'ON'.              00042400
042500         10  FILLER          PIC XX      VALUE 'PE'.              00042500
042600         10  FILLER          PIC XX      VALUE 'QB'.              00042600
042700         10  FILLER          PIC XX      VALUE 'SK'.              00042700
042800         10  FILLER          PIC XX      VALUE 'YT'.              00042800
042900         10  FILLER          PIC XX      VALUE 'CB'.              00042900
043000         10  FILLER          PIC XX      VALUE 'LA'.              00043000
043100         10  FILLER          PIC XX      VALUE '  '.              00043100
043200         10  FILLER          PIC XX      VALUE '  '.              00043200
043300         10  FILLER          PIC XX      VALUE '  '.              00043300
043400         10  FILLER          PIC XX      VALUE '  '.              00043400
043500         10  FILLER          PIC XX      VALUE '  '.              00043500
043600         10  FILLER          PIC XX      VALUE '  '.              00043600
043700         10  FILLER          PIC XX      VALUE '  '.              00043700
043800         10  FILLER          PIC XX      VALUE '  '.              00043800
043900                                                                  00043900
044000     05  STATE-IDENT      REDEFINES   STATE-ID-FILE.              00044000
044100         10  ST-ID         OCCURS 80    PIC XX.                   00044100
044200                                                                  00044200
044300                                                                  00044300
044400 01  FILLER.                                                      00044400
044500     05  RPT-DT-ID.                                               00044500
044600         10  FILLER          PIC X(14)   VALUE 'JANUARY       '.  00044600
044700         10  FILLER          PIC X(14)   VALUE 'FEBRUARY      '.  00044700
044800         10  FILLER          PIC X(14)   VALUE 'MARCH         '.  00044800
044900         10  FILLER          PIC X(14)   VALUE 'APRIL         '.  00044900
045000         10  FILLER          PIC X(14)   VALUE 'MAY           '.  00045000
045100         10  FILLER          PIC X(14)   VALUE 'JUNE          '.  00045100
045200         10  FILLER          PIC X(14)   VALUE 'JULY          '.  00045200
045300         10  FILLER          PIC X(14)   VALUE 'AUGUST        '.  00045300
045400         10  FILLER          PIC X(14)   VALUE 'SEPTEMBER     '.  00045400
045500         10  FILLER          PIC X(14)   VALUE 'OCTOBER       '.  00045500
045600         10  FILLER          PIC X(14)   VALUE 'NOVEMBER      '.  00045600
045700         10  FILLER          PIC X(14)   VALUE 'DECEMBER      '.  00045700
045800                                                                  00045800
045900     05  RPT-DT-05     REDEFINES   RPT-DT-ID.                     00045900
046000         10  RPT-DT        OCCURS 12    PIC X(14).                00046000
046100                                                                  00046100
046100*                                                                 00046110
046200* IN THE LIFE AND AH TABLES, CSO CODES ARE THE FIRST 2 POSITIONS  00046200
046300* IN THE "VALUE" FOR EACH LINE AND THE CONLEY CODE IS THE SECOND  00046300
046400* 2 POSITIONS IN THE "VALUE" FIELD.                               00046400
046900*                                                                 00046900
047000* THE SAME PROCESSING IS TRUE FOR A&H OR LIFE CODE CONVERSIONS.   00047000
058700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00047010
058700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00047020
047100*                                               CODES             00047100
047200*                                             ----------          00047200
047200* * * * * * * * *  L I F E  * * * * * * * *   CSO CONLEY  * * * * 00047210
047200*                                               | |               00047220
047300*                                               V V               00047300
047300 01  FILLER.                                                      00047310
047400     05  LIFE-CODE.                                               00047400
047500         10  FILLER          PIC X(04)   VALUE '0131'.            00047500
047600         10  FILLER          PIC X(04)   VALUE '0241'.            00047600
047700         10  FILLER          PIC X(04)   VALUE '0361'.            00047700
047800         10  FILLER          PIC X(04)   VALUE '0471'.            00047800
047900         10  FILLER          PIC X(04)   VALUE '0536'.            00047900
048000         10  FILLER          PIC X(04)   VALUE '0666'.            00048000
048100         10  FILLER          PIC X(04)   VALUE '07  '.            00048100
048200         10  FILLER          PIC X(04)   VALUE '08  '.            00048200
048300         10  FILLER          PIC X(04)   VALUE '09  '.            00048300
048400         10  FILLER          PIC X(04)   VALUE '10  '.            00048400
048500* 10 OCCURS TO HERE ----------------------------------            00048500
048600         10  FILLER          PIC X(04)   VALUE '11  '.            00048600
048700         10  FILLER          PIC X(04)   VALUE '12  '.            00048700
048800         10  FILLER          PIC X(04)   VALUE '13  '.            00048800
048900         10  FILLER          PIC X(04)   VALUE '14  '.            00048900
049000         10  FILLER          PIC X(04)   VALUE '15  '.            00049000
049100         10  FILLER          PIC X(04)   VALUE '16  '.            00049100
049200         10  FILLER          PIC X(04)   VALUE '17  '.            00049200
049300         10  FILLER          PIC X(04)   VALUE '18  '.            00049300
049400         10  FILLER          PIC X(04)   VALUE '19  '.            00049400
049500         10  FILLER          PIC X(04)   VALUE '2047'.            00049500
049600* 20 OCCURS TO HERE ----------------------------------            00049600
049700         10  FILLER          PIC X(04)   VALUE '2177'.            00049700
049800         10  FILLER          PIC X(04)   VALUE '22  '.            00049800
049900         10  FILLER          PIC X(04)   VALUE '23  '.            00049900
050000         10  FILLER          PIC X(04)   VALUE '24  '.            00050000
050100         10  FILLER          PIC X(04)   VALUE '25  '.            00050100
050200         10  FILLER          PIC X(04)   VALUE '26  '.            00050200
050300         10  FILLER          PIC X(04)   VALUE '27  '.            00050300
050400         10  FILLER          PIC X(04)   VALUE '28  '.            00050400
050500         10  FILLER          PIC X(04)   VALUE '29  '.            00050500
050600         10  FILLER          PIC X(04)   VALUE '30  '.            00050600
050700* 30 OCCURS TO HERE ----------------------------------            00050700
050800         10  FILLER          PIC X(04)   VALUE '31  '.            00050800
050900         10  FILLER          PIC X(04)   VALUE '32  '.            00050900
051000         10  FILLER          PIC X(04)   VALUE '33  '.            00051000
051100         10  FILLER          PIC X(04)   VALUE '3439'.            00051100
051200         10  FILLER          PIC X(04)   VALUE '3569'.            00051200
051300         10  FILLER          PIC X(04)   VALUE '36  '.            00051300
051400         10  FILLER          PIC X(04)   VALUE '37  '.            00051400
051500         10  FILLER          PIC X(04)   VALUE '38  '.            00051500
051600         10  FILLER          PIC X(04)   VALUE '3964'.            00051600
051700         10  FILLER          PIC X(04)   VALUE '4074'.            00051700
051800* 40 OCCURS TO HERE ----------------------------------            00051800
051900         10  FILLER          PIC X(04)   VALUE '41  '.            00051900
052000         10  FILLER          PIC X(04)   VALUE '42  '.            00052000
052100         10  FILLER          PIC X(04)   VALUE '43  '.            00052100
052200         10  FILLER          PIC X(04)   VALUE '44  '.            00052200
052300         10  FILLER          PIC X(04)   VALUE '45  '.            00052300
052400         10  FILLER          PIC X(04)   VALUE '46  '.            00052400
052500         10  FILLER          PIC X(04)   VALUE '47  '.            00052500
052600         10  FILLER          PIC X(04)   VALUE '48  '.            00052600
052700         10  FILLER          PIC X(04)   VALUE '49  '.            00052700
052800         10  FILLER          PIC X(04)   VALUE '50  '.            00052800
052900* 50 OCCURS TO HERE ----------------------------------            00052900
053000         10  FILLER          PIC X(04)   VALUE '51  '.            00053000
053100         10  FILLER          PIC X(04)   VALUE '52  '.            00053100
053200         10  FILLER          PIC X(04)   VALUE '53  '.            00053200
053300         10  FILLER          PIC X(04)   VALUE '54  '.            00053300
053400         10  FILLER          PIC X(04)   VALUE '55  '.            00053400
053500         10  FILLER          PIC X(04)   VALUE '56  '.            00053500
053600         10  FILLER          PIC X(04)   VALUE '57  '.            00053600
053700         10  FILLER          PIC X(04)   VALUE '58  '.            00053700
053800         10  FILLER          PIC X(04)   VALUE '59  '.            00053800
053900         10  FILLER          PIC X(04)   VALUE '60  '.            00053900
054000* 60 OCCURS TO HERE ----------------------------------            00054000
054100         10  FILLER          PIC X(04)   VALUE '61  '.            00054100
054200         10  FILLER          PIC X(04)   VALUE '62  '.            00054200
054300         10  FILLER          PIC X(04)   VALUE '63  '.            00054300
054400         10  FILLER          PIC X(04)   VALUE '64  '.            00054400
054500         10  FILLER          PIC X(04)   VALUE '65  '.            00054500
054600         10  FILLER          PIC X(04)   VALUE '66  '.            00054600
054700         10  FILLER          PIC X(04)   VALUE '67  '.            00054700
054800         10  FILLER          PIC X(04)   VALUE '68  '.            00054800
054900         10  FILLER          PIC X(04)   VALUE '69  '.            00054900
055000         10  FILLER          PIC X(04)   VALUE '70  '.            00055000
055100* 70 OCCURS TO HERE ----------------------------------            00055100
055200         10  FILLER          PIC X(04)   VALUE '71  '.            00055200
055300         10  FILLER          PIC X(04)   VALUE '72  '.            00055300
055400         10  FILLER          PIC X(04)   VALUE '73  '.            00055400
055500         10  FILLER          PIC X(04)   VALUE '74  '.            00055500
055600         10  FILLER          PIC X(04)   VALUE '75  '.            00055600
055700         10  FILLER          PIC X(04)   VALUE '76  '.            00055700
055800         10  FILLER          PIC X(04)   VALUE '77  '.            00055800
055900         10  FILLER          PIC X(04)   VALUE '78  '.            00055900
056000         10  FILLER          PIC X(04)   VALUE '79  '.            00056000
056100         10  FILLER          PIC X(04)   VALUE '80  '.            00056100
056200* 80 OCCURS TO HERE ----------------------------------            00056200
056300         10  FILLER          PIC X(04)   VALUE '81  '.            00056300
056400         10  FILLER          PIC X(04)   VALUE '82  '.            00056400
056500         10  FILLER          PIC X(04)   VALUE '83  '.            00056500
056600         10  FILLER          PIC X(04)   VALUE '84  '.            00056600
056700         10  FILLER          PIC X(04)   VALUE '85  '.            00056700
056800         10  FILLER          PIC X(04)   VALUE '86  '.            00056800
056900         10  FILLER          PIC X(04)   VALUE '87  '.            00056900
057000         10  FILLER          PIC X(04)   VALUE '88  '.            00057000
057100         10  FILLER          PIC X(04)   VALUE '89  '.            00057100
057200         10  FILLER          PIC X(04)   VALUE '90  '.            00057200
057300* 90 OCCURS TO HERE ----------------------------------            00057300
057400         10  FILLER          PIC X(04)   VALUE '91  '.            00057400
057500         10  FILLER          PIC X(04)   VALUE '92  '.            00057500
057600         10  FILLER          PIC X(04)   VALUE '93  '.            00057600
057700         10  FILLER          PIC X(04)   VALUE '94  '.            00057700
057800         10  FILLER          PIC X(04)   VALUE '95  '.            00057800
057900         10  FILLER          PIC X(04)   VALUE '96  '.            00057900
058000         10  FILLER          PIC X(04)   VALUE '97  '.            00058000
058100         10  FILLER          PIC X(04)   VALUE '98  '.            00058100
058200         10  FILLER          PIC X(04)   VALUE '99  '.            00058200
058201         10  FILLER          PIC X(04)   VALUE '1A  '.            00058201
058202*100 OCCURS TO HERE ----------------------------------            00058202
058220         10  FILLER          PIC X(04)   VALUE '1B  '.            00058220
058230         10  FILLER          PIC X(04)   VALUE '1C  '.            00058230
058240         10  FILLER          PIC X(04)   VALUE '1D  '.            00058240
058250         10  FILLER          PIC X(04)   VALUE '1E38'.            00058250
058260         10  FILLER          PIC X(04)   VALUE '1F68'.            00058260
058270         10  FILLER          PIC X(04)   VALUE '1G48'.            00058270
058280         10  FILLER          PIC X(04)   VALUE '1H78'.            00058280
058290         10  FILLER          PIC X(04)   VALUE '1I91'.            00058290
058291         10  FILLER          PIC X(04)   VALUE '1J92'.            00058291
058292         10  FILLER          PIC X(04)   VALUE '1K93'.            00058292
058293*110 OCCURS TO HERE ----------------------------------            00058293
058295         10  FILLER          PIC X(04)   VALUE '1L94'.            00058295
058296         10  FILLER          PIC X(04)   VALUE '1M  '.            00058296
058297         10  FILLER          PIC X(04)   VALUE '1N  '.            00058297
058298         10  FILLER          PIC X(04)   VALUE '1O  '.            00058298
058299         10  FILLER          PIC X(04)   VALUE '1P  '.            00058299
058300         10  FILLER          PIC X(04)   VALUE '1Q  '.            00058300
058301         10  FILLER          PIC X(04)   VALUE '1R  '.            00058301
058302         10  FILLER          PIC X(04)   VALUE '1S  '.            00058302
058303         10  FILLER          PIC X(04)   VALUE '1T  '.            00058303
058304         10  FILLER          PIC X(04)   VALUE '1U  '.            00058304
058305*120 OCCURS TO HERE ----------------------------------            00058305
058307         10  FILLER          PIC X(04)   VALUE '1V  '.            00058307
058308         10  FILLER          PIC X(04)   VALUE '1W  '.            00058308
058309         10  FILLER          PIC X(04)   VALUE '1X  '.            00058309
058310         10  FILLER          PIC X(04)   VALUE '1Y  '.            00058310
058311         10  FILLER          PIC X(04)   VALUE '1Z  '.            00058311
058312         10  FILLER          PIC X(04)   VALUE '2A  '.            00058312
058313         10  FILLER          PIC X(04)   VALUE '2B  '.            00058313
058314         10  FILLER          PIC X(04)   VALUE '2C  '.            00058314
058315         10  FILLER          PIC X(04)   VALUE '2D  '.            00058315
058316         10  FILLER          PIC X(04)   VALUE '2E  '.            00058316
058320* END OF TABLE - 130 POSSIBLE CODES TO HERE.                      00058320
058400                                                                  00058400
058500     05  LIFE-CDS       REDEFINES   LIFE-CODE.                    00058500
058600         10  LIF-CD        OCCURS 130.                            00058600
058610             15  CSO-LIF-CD      PIC X(02).                       00058610
058620             15  CON-LIF-CD      PIC X(02).                       00058620
058700* *                                                               00058700
058700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00058701
058700* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * 00058710
047100*                                               CODES             00058801
047200*                                             ----------          00058802
047200* * * * * * * * *  A & H  * * * * * * * * *   CSO CONLEY  * * * * 00058810
047200*                                               | |               00058820
047300*                                               V V               00058830
047300 01  FILLER.                                                      00058840
059000     05  AH-CODES.                                                00059000
059100         10  FILLER          PIC X(04)   VALUE '0113'.            00059100
059200         10  FILLER          PIC X(04)   VALUE '02  '.            00059200
059300         10  FILLER          PIC X(04)   VALUE '0312'.            00059300
059400         10  FILLER          PIC X(04)   VALUE '0414'.            00059400
059500         10  FILLER          PIC X(04)   VALUE '05  '.            00059500
059600         10  FILLER          PIC X(04)   VALUE '06  '.            00059600
059700         10  FILLER          PIC X(04)   VALUE '07  '.            00059700
059800         10  FILLER          PIC X(04)   VALUE '08  '.            00059800
059900         10  FILLER          PIC X(04)   VALUE '09  '.            00059900
060000         10  FILLER          PIC X(04)   VALUE '10  '.            00060000
060100* 10 OCCURS TO HERE ----------------------------------            00060100
060200         10  FILLER          PIC X(04)   VALUE '11C3'.            00060200
060300         10  FILLER          PIC X(04)   VALUE '12  '.            00060300
060400         10  FILLER          PIC X(04)   VALUE '13C2'.            00060400
060500         10  FILLER          PIC X(04)   VALUE '14C4'.            00060500
060600         10  FILLER          PIC X(04)   VALUE '15  '.            00060600
060700         10  FILLER          PIC X(04)   VALUE '16  '.            00060700
060800         10  FILLER          PIC X(04)   VALUE '17  '.            00060800
060900         10  FILLER          PIC X(04)   VALUE '18  '.            00060900
061000         10  FILLER          PIC X(04)   VALUE '19  '.            00061000
061100         10  FILLER          PIC X(04)   VALUE '20  '.            00061100
061200* 20 OCCURS TO HERE ----------------------------------            00061200
061300         10  FILLER          PIC X(04)   VALUE '21  '.            00061300
061400         10  FILLER          PIC X(04)   VALUE '22  '.            00061400
061500         10  FILLER          PIC X(04)   VALUE '23  '.            00061500
061600         10  FILLER          PIC X(04)   VALUE '24  '.            00061600
061700         10  FILLER          PIC X(04)   VALUE '25  '.            00061700
061800         10  FILLER          PIC X(04)   VALUE '26  '.            00061800
061900         10  FILLER          PIC X(04)   VALUE '27  '.            00061900
062000         10  FILLER          PIC X(04)   VALUE '28  '.            00062000
062100         10  FILLER          PIC X(04)   VALUE '29  '.            00062100
062200         10  FILLER          PIC X(04)   VALUE '30  '.            00062200
062300* 30 OCCURS TO HERE ----------------------------------            00062300
062400         10  FILLER          PIC X(04)   VALUE '31  '.            00062400
062500         10  FILLER          PIC X(04)   VALUE '32  '.            00062500
062600         10  FILLER          PIC X(04)   VALUE '33  '.            00062600
062700         10  FILLER          PIC X(04)   VALUE '34  '.            00062700
062800         10  FILLER          PIC X(04)   VALUE '35  '.            00062800
062900         10  FILLER          PIC X(04)   VALUE '36  '.            00062900
063000         10  FILLER          PIC X(04)   VALUE '37  '.            00063000
063100         10  FILLER          PIC X(04)   VALUE '38  '.            00063100
063200         10  FILLER          PIC X(04)   VALUE '39  '.            00063200
063300         10  FILLER          PIC X(04)   VALUE '40  '.            00063300
063400* 40 OCCURS TO HERE ----------------------------------            00063400
063500         10  FILLER          PIC X(04)   VALUE '41  '.            00063500
063600         10  FILLER          PIC X(04)   VALUE '42  '.            00063600
063700         10  FILLER          PIC X(04)   VALUE '43  '.            00063700
063800         10  FILLER          PIC X(04)   VALUE '44  '.            00063800
063900         10  FILLER          PIC X(04)   VALUE '45  '.            00063900
064000         10  FILLER          PIC X(04)   VALUE '46  '.            00064000
064100         10  FILLER          PIC X(04)   VALUE '47  '.            00064100
064200         10  FILLER          PIC X(04)   VALUE '48  '.            00064200
064300         10  FILLER          PIC X(04)   VALUE '49P3'.            00064300
064400         10  FILLER          PIC X(04)   VALUE '50  '.            00064400
064500* 50 OCCURS TO HERE ----------------------------------            00064500
064600         10  FILLER          PIC X(04)   VALUE '51  '.            00064600
064700         10  FILLER          PIC X(04)   VALUE '52  '.            00064700
064800         10  FILLER          PIC X(04)   VALUE '53L2'.            00064800
064900         10  FILLER          PIC X(04)   VALUE '54L3'.            00064900
065000         10  FILLER          PIC X(04)   VALUE '55L4'.            00065000
065100         10  FILLER          PIC X(04)   VALUE '56  '.            00065100
065200         10  FILLER          PIC X(04)   VALUE '57  '.            00065200
065300         10  FILLER          PIC X(04)   VALUE '58J2'.            00065300
065400         10  FILLER          PIC X(04)   VALUE '59J3'.            00065400
065500         10  FILLER          PIC X(04)   VALUE '60J4'.            00065500
065600* 60 OCCURS TO HERE ----------------------------------            00065600
065700         10  FILLER          PIC X(04)   VALUE '61  '.            00065700
065800         10  FILLER          PIC X(04)   VALUE '62  '.            00065800
065900         10  FILLER          PIC X(04)   VALUE '63  '.            00065900
066000         10  FILLER          PIC X(04)   VALUE '64  '.            00066000
066100         10  FILLER          PIC X(04)   VALUE '65  '.            00066100
066200         10  FILLER          PIC X(04)   VALUE '66  '.            00066200
066300         10  FILLER          PIC X(04)   VALUE '67  '.            00066300
066400         10  FILLER          PIC X(04)   VALUE '68  '.            00066400
066500         10  FILLER          PIC X(04)   VALUE '69  '.            00066500
066600         10  FILLER          PIC X(04)   VALUE '70  '.            00066600
066700* 70 OCCURS TO HERE ----------------------------------            00066700
066800         10  FILLER          PIC X(04)   VALUE '71  '.            00066800
066900         10  FILLER          PIC X(04)   VALUE '72  '.            00066900
067000         10  FILLER          PIC X(04)   VALUE '73  '.            00067000
067100         10  FILLER          PIC X(04)   VALUE '74  '.            00067100
067200         10  FILLER          PIC X(04)   VALUE '75  '.            00067200
067300         10  FILLER          PIC X(04)   VALUE '76  '.            00067300
067400         10  FILLER          PIC X(04)   VALUE '77  '.            00067400
067500         10  FILLER          PIC X(04)   VALUE '78  '.            00067500
067600         10  FILLER          PIC X(04)   VALUE '79  '.            00067600
067700         10  FILLER          PIC X(04)   VALUE '80  '.            00067700
067800* 80 OCCURS TO HERE ----------------------------------            00067800
067900         10  FILLER          PIC X(04)   VALUE '81  '.            00067900
068000         10  FILLER          PIC X(04)   VALUE '82  '.            00068000
068100         10  FILLER          PIC X(04)   VALUE '83  '.            00068100
068200         10  FILLER          PIC X(04)   VALUE '84  '.            00068200
068300         10  FILLER          PIC X(04)   VALUE '85  '.            00068300
068400         10  FILLER          PIC X(04)   VALUE '86  '.            00068400
068500         10  FILLER          PIC X(04)   VALUE '87  '.            00068500
068600         10  FILLER          PIC X(04)   VALUE '88  '.            00068600
068700         10  FILLER          PIC X(04)   VALUE '89  '.            00068700
068800         10  FILLER          PIC X(04)   VALUE '90  '.            00068800
068900* 90 OCCURS TO HERE ----------------------------------            00068900
069000         10  FILLER          PIC X(04)   VALUE '91  '.            00069000
069100         10  FILLER          PIC X(04)   VALUE '92  '.            00069100
069200         10  FILLER          PIC X(04)   VALUE '93  '.            00069200
069300         10  FILLER          PIC X(04)   VALUE '94  '.            00069300
069400         10  FILLER          PIC X(04)   VALUE '95  '.            00069400
069500         10  FILLER          PIC X(04)   VALUE '96  '.            00069500
069600         10  FILLER          PIC X(04)   VALUE '97  '.            00069600
069700         10  FILLER          PIC X(04)   VALUE '98  '.            00069700
069710         10  FILLER          PIC X(04)   VALUE '99  '.            00069710
069800         10  FILLER          PIC X(04)   VALUE '1A  '.            00069800
069810*100 OCCURS TO HERE ----------------------------------            00069810
069820         10  FILLER          PIC X(04)   VALUE '1B  '.            00069820
069830         10  FILLER          PIC X(04)   VALUE '1C  '.            00069830
069840         10  FILLER          PIC X(04)   VALUE '1D  '.            00069840
069850         10  FILLER          PIC X(04)   VALUE '1E  '.            00069850
069860         10  FILLER          PIC X(04)   VALUE '1F  '.            00069860
069870         10  FILLER          PIC X(04)   VALUE '1G  '.            00069870
069880         10  FILLER          PIC X(04)   VALUE '1H  '.            00069880
069890         10  FILLER          PIC X(04)   VALUE '1I  '.            00069890
069891         10  FILLER          PIC X(04)   VALUE '1J  '.            00069891
069892         10  FILLER          PIC X(04)   VALUE '1K  '.            00069892
069893*110 OCCURS TO HERE ----------------------------------            00069893
069894         10  FILLER          PIC X(04)   VALUE '1L  '.            00069894
069895         10  FILLER          PIC X(04)   VALUE '1M  '.            00069895
069896         10  FILLER          PIC X(04)   VALUE '1N  '.            00069896
069897         10  FILLER          PIC X(04)   VALUE '1O  '.            00069897
069898         10  FILLER          PIC X(04)   VALUE '1P  '.            00069898
069899         10  FILLER          PIC X(04)   VALUE '1Q  '.            00069899
069900         10  FILLER          PIC X(04)   VALUE '1R  '.            00069900
069901         10  FILLER          PIC X(04)   VALUE '1S  '.            00069901
069902         10  FILLER          PIC X(04)   VALUE '1T  '.            00069902
069903         10  FILLER          PIC X(04)   VALUE '1U  '.            00069903
069904*120 OCCURS TO HERE ----------------------------------            00069904
069905         10  FILLER          PIC X(04)   VALUE '1V  '.            00069905
069906         10  FILLER          PIC X(04)   VALUE '1W  '.            00069906
069907         10  FILLER          PIC X(04)   VALUE '1X  '.            00069907
069908         10  FILLER          PIC X(04)   VALUE '1Y  '.            00069908
069909         10  FILLER          PIC X(04)   VALUE '1Z  '.            00069909
069910         10  FILLER          PIC X(04)   VALUE '2A  '.            00069910
069911         10  FILLER          PIC X(04)   VALUE '2B  '.            00069911
069912         10  FILLER          PIC X(04)   VALUE '2C  '.            00069912
069913         10  FILLER          PIC X(04)   VALUE '2D  '.            00069913
069914         10  FILLER          PIC X(04)   VALUE '2E  '.            00069914
069920* END OF TABLE - 130 POSSIBLE CODES TO HERE.                      00069920
070000                                                                  00070000
070100     05  AH-CDS       REDEFINES   AH-CODES.                       00070100
070200         10  AH-CD        OCCURS 130.                             00070200
070210             15  CSO-AH-CD   PIC X(02).                           00070210
070220             15  CON-AH-CD   PIC X(02).                           00070220
070300                                                                  00070300
094800 01  FILLER   PIC X(24) VALUE '** CARD COPY BOOK  **'.            00094800
094900                                                                  00094900
095000*01  OUTPUT-COPYBOOK          COPY ERCPNDBI.                      00095000
095000     COPY ERCPNDBI.                                               00095000
095100 SKIP3                                                            00095100
095200                                                                  00095200
095300 01  FILLER   PIC X(24) VALUE '**CIA-LIFE-RECORD-IN**'.           00095300
095400                                                                  00095400
095500                                                                  00095500
011121 01  CIA-ACCT-EXPANDED           PIC X(10) VALUE SPACES.
095600 01  CIA-WORK.                                                    00095600
095700     05  CIA-ST                       PIC X(02)    VALUE SPACES.  00095700
095800     05  CIA-ACCT                     PIC X(06)    VALUE SPACES.  00095800
095900     05  CIA-EFFECTIVE-DATE.                                      00095900
096000         10  CIA-EFF-DT-N             PIC 9(9).                   00096000
096100         10  CIA-EFF-DT             REDEFINES  CIA-EFF-DT-N.      00096100
096200             15  CIA-EFF-MO1          PIC X.                      00096200
096300             15  CIA-EFF-MO           PIC XX.                     00096300
096400             15  CIA-EFF-DA1          PIC X.                      00096400
096500             15  CIA-EFF-DA           PIC XX.                     00096500
096600             15  CIA-EFF-YR1          PIC X.                      00096600
096700             15  CIA-EFF-YR           PIC XX.                     00096700
096800     05  CIA-CERT.                                                00096800
030310         10  CIA-CERT-9               PIC X(09)    VALUE SPACES.  00096900
097000         10  CIA-CERT-1               PIC X(01)    VALUE SPACES.  00097000
097100     05  CIA-CERT-TST   REDEFINES   CIA-CERT.                     00097100
030310         10  CIA-CERT-TST-7           PIC S9(07).                 00097200
097300         10  CIA-CERT-3-N             PIC S9(03).                 00097300
097400     05  CIA-ISSUE.                                               00097400
097500         10  CIA-NAME                 PIC X(12).                  00097500
097600         10  CIA-INIT.                                            00097600
097700             15  CIA-INIT-1           PIC X(01).                  00097700
097800             15  CIA-INIT-2           PIC X(01).                  00097800
097900         10  CIA-SEX-CD               PIC X(01).                  00097900
098000         10  FILLER                   PIC X(01).                  00098000
098100         10  CIA-AGE                  PIC X(02).                  00098100
098200         10  CIA-AGE-N              REDEFINES                     00098200
098300               CIA-AGE                PIC 9(02).                  00098300
098400         10  FILLER                   PIC X(01).                  00098400
098500         10  CIA-TERM-LF              PIC X(03).                  00098500
098600         10  CIA-TERM-LF-N          REDEFINES                     00098600
098700               CIA-TERM-LF            PIC 9(03).                  00098700
098800         10  FILLER                   PIC X(01).                  00098800
098900         10  CIA-TERM-AH              PIC X(03).                  00098900
099000         10  CIA-TERM-AH-N          REDEFINES                     00099000
099100               CIA-TERM-AH            PIC 9(03).                  00099100
099200         10  FILLER                   PIC X(01).                  00099200
099300         10  CIA-LIFE-CD              PIC X(02).                  00099300
099310         10  CIA-LIFE-CD-NUMER      REDEFINES                     00099310
099320               CIA-LIFE-CD            PIC 9(02).
081406         10  FILLER                   PIC X.
099400         10  CIA-LIFE-AMT             PIC X(10).                  00099400
099500         10  CIA-LIFE-AMT-N         REDEFINES                     00099500
099600               CIA-LIFE-AMT           PIC S9(08)V99.              00099600
099700         10  CIA-LIFE-PREM            PIC X(08).                  00099700
099800         10  CIA-LIFE-PREM-N        REDEFINES                     00099800
099900               CIA-LIFE-PREM          PIC S9(06)V99.              00099900
100000         10  CIA-AH-CD                PIC X(02).                  00100000
100010         10  CIA-AH-CD-N            REDEFINES                     00100010
100020               CIA-AH-CD              PIC 9(02).                  00100020
100100         10  CIA-AH-AMT               PIC X(09).                  00100100
100200         10  CIA-AH-AMT-N           REDEFINES                     00100200
100300               CIA-AH-AMT             PIC S9(07)V99.              00100300
100400         10  CIA-AH-PREM              PIC X(08).                  00100400
100500         10  CIA-AH-PREM-N          REDEFINES                     00100500
100600               CIA-AH-PREM            PIC S9(06)V99.              00100600
100700         10  FILLER                   PIC XX.                     00100700
100800         10  CIA-GA-NO                PIC XXX.                    00100800
100900         10  FILLER                   PIC X.                      00100900
101000         10  REC-TYPE                 PIC X.                      00101000
101100         10  CIA-JOINT-NAME           PIC X(12).                  00101100
101200         10  CIA-INT-RATE.                                        00101200
101300             15  CIA-INT-DOL          PIC X(02).                  00101300
101400             15  CIA-INT-DECML        PIC X(01).                  00101400
101400             15  CIA-INT-CENTS        PIC X(02).
081406         10  FILLER                   PIC X.
101500         10  CIA-DAYS-TO-1ST-PMT      PIC XX.
040414         10  cia-days-to-1st-pmt-n redefines
040414             cia-days-to-1st-pmt      pic 99.
101600         10  FILLER                   PIC X.
101700         10  CIA-NEW-ISS-STATE        PIC XX.
081406         10  CIA-JOINT-AGE            PIC XX.
081406         10  CIA-LOAN-TERM            PIC XXX.
040414         10  cia-loan-term-n redefines
040414             cia-loan-term            pic 999.
081406         10  CIA-INS-FNAME            PIC X(20).
081406         10  CIA-INS-ADDR1            PIC X(35).
081406         10  CIA-INS-ADDR2            PIC X(35).
081406         10  CIA-INS-CITY             PIC X(30).
081406         10  CIA-INS-STATE            PIC XX.
081406         10  CIA-INS-ZIP              PIC X(9).
050509         10  CIA-CRED-BENE            PIC X(40).
040414         10  cia-ins-dob              pic x(8).
040414         10  cia-jnt-dob              pic x(8).
101900                                                                  00101900
102000     05  CIA-CANCEL                 REDEFINES  CIA-ISSUE.         00102000
030310         10  FILLER                   PIC X.                      00102100
102200         10  CIA-CANC-DT-N            PIC 9(09).                  00102200
102300         10  CIA-CANC-DT            REDEFINES  CIA-CANC-DT-N.     00102300
102400             15  CIA-CANC-MO1         PIC X.                      00102400
102500             15  CIA-CANC-MO          PIC XX.                     00102500
102600             15  CIA-CANC-DA1         PIC X.                      00102600
102700             15  CIA-CANC-DA          PIC XX.                     00102700
102800             15  CIA-CANC-YR1         PIC X.                      00102800
102900             15  CIA-CANC-YR          PIC XX.                     00102900
103000         10  CIA-CANC-LF-REFUND       PIC X(08).                  00103000
103100         10  CIA-CANC-LF-REFUND-N   REDEFINES                     00103100
103200               CIA-CANC-LF-REFUND     PIC S9(06)V99.              00103200
103300         10  CIA-CANC-AH-REFUND       PIC X(08).                  00103300
103400         10  CIA-CANC-AH-REFUND-N   REDEFINES                     00103400
103500               CIA-CANC-AH-REFUND     PIC S9(06)V99.              00103500
103600         10  FILLER                   PIC X(01).                  00103600
103700         10  CIA-NEW-CANC-STATE       PIC X(02).                  00103700
040414         10  FILLER                   PIC X(260).                 00103800
103900                                                                  00103900
103900                                                                  00103901
101200 01  CIA-INT-RATE-WORK.                                           00103910
101300     05  CIA-INT-DOL          PIC X(02).                          00103920
101400     05  CIA-INT-DECML        PIC X(01).                          00103930
101400     05  CIA-INT-CENTS        PIC X(02).                          00103940
101200 01  CIA-INT-RATE-WORK-N  REDEFINES  CIA-INT-RATE-WORK.           00103952
101300     05  CIA-INT-DOL-N        PIC 9(02).                          00103953
101400     05  CIA-INT-DECML-X      PIC X(01).                          00103954
101400     05  CIA-INT-CENTS-N      PIC 9(02).                          00103955
103900                                                                  00103956
101200 01  CIA-INT-CNVTD.                                               00103957
101300     05  CIA-INT-CNVTD-DOL    PIC 9(02).                          00103958
101400     05  CIA-INT-CNVTD-CENTS  PIC 9(02).                          00103960
101400     05  CIA-INT-CNVTD-ZEROS  PIC 9(02).                          00103961

040414                                 COPY ELCDATE.

104000******************************************************************00104000
104100******************************************************************00104100
104200 SKIP3                                                            00104200
104300 PROCEDURE DIVISION.                                              00104300
104400 SKIP3                                                            00104400
104500*                                                                 00104500
104600 INPUT-ROUTINE SECTION.                                           00104600
104700                                                                  00104700
104800     ACCEPT WORK-DATE-IN FROM DATE.                               00104800
104900                                                                  00104900
104900     MOVE WORK-YR-IN   TO  WORK-YR-N.                             00104900
104900     MOVE WORK-MO-IN   TO  WORK-MO-N.                             00104900
104900     MOVE WORK-DA-IN   TO  WORK-DA-N.                             00104900
104900                                                                  00104900
105000     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00105000
105100     DISPLAY '*   CURRENT DATE IS -- ' WORK-DATE-N.               00105100
105200     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00105200
105300                                                                  00105300
105400 0010-STATE-CODES.                                                00105400
105500                                                                  00105500
105600     OPEN INPUT    STATE-CODES.                                   00105600
105700     MOVE  ZEROS  TO  ST-SUB.                                     00105700
105800                                                                  00105800
105900 0010-STATE-CODES-LOOP.                                           00105900
106000                                                                  00106000
106100     ADD   1      TO  ST-SUB.                                     00106100
106200                                                                  00106200
106300     READ STATE-CODES                                             00106300
106400       AT END                                                     00106400
106500         CLOSE  STATE-CODES                                       00106500
106600            MOVE  ZEROS  TO  ST-SUB                               00106600
106700               GO TO  0010-END-OF-CODES.                          00106700
106800                                                                  00106800
106900     MOVE  ST-REC-IN  TO  SAVE-ST-ID (ST-SUB).                    00106900
107000                                                                  00107000
107100     GO TO  0010-STATE-CODES-LOOP.                                00107100
107200                                                                  00107200
107300 0010-END-OF-CODES.                                               00107300
107400                                                                  00107400
107500     OPEN INPUT    CIA-LIFE-IN.                                   00107500
107600     OPEN INPUT    ERACCT.                                        00107600
107700                                                                  00107700
107800     IF    ERACCT-FILE-STATUS = '97'                              00107800
107900           MOVE ZEROS TO ERACCT-FILE-STATUS.                      00107900
108000                                                                  00108000
108100     IF    ERACCT-FILE-STATUS NOT = ZERO                          00108100
108200           DISPLAY '* * * * * * * * * * * * * * * * * * *'        00108200
108300           DISPLAY 'ERROR ON ERACCT OPEN - PGM ENDING NOW'        00108300
108400           DISPLAY 'ERROR CODE IS - ' ERACCT-FILE-STATUS          00108400
108500           DISPLAY '* * * * * * * *   * * * * * * * * * *'        00108500
108600           DISPLAY 'ERROR ON ERACCT OPEN - PGM ENDING NOW'        00108600
108700           DISPLAY 'ERROR CODE IS - ' ERACCT-FILE-STATUS          00108700
108800           DISPLAY '* * * * * * * * * * * * * * * * * * *'        00108800
108900           GOBACK.                                                00108900
109000                                                                  00109000
109100     OPEN  OUTPUT  CIA-LIFE-OUT                                   00109100
109200                      RPT-FILE.                                   00109200
109300                                                                  00109300
109400     MOVE  SPACES                 TO  STATE-TABLE.                00109400
109500                                                                  00109500
109600     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00109600
109700                                                                  00109700
109800     MOVE  ZEROS                  TO   CIA-LIFE-CANC              00109800
109900                                       CIA-AH-CANC                00109900
110000                                       CIA-CERT-CANC.             00110000
110100                                                                  00110100
110200     MOVE  WK-MO                  TO   CIA-HD-EFF-MO.             00110200
110300     MOVE  WK-DA                  TO   CIA-HD-EFF-DA.             00110300
110400     MOVE  WK-YR                  TO   CIA-HD-EFF-YR.             00110400
110500                                                                  00110500
110600     MOVE  WK-MO                  TO   WORK-MO-X.                 00110600
110700     MOVE  WK-YR                  TO   WORK-YR-X.                 00110700
110800     MOVE  WK-DA                  TO   WORK-DA-X.                 00110800
110900                                                                  00110900
111000     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00111000
111100     DISPLAY '*   WORK-DATE-X  IS -- ' WORK-DATE-X.               00111100
111200     DISPLAY '* * * * * * * * * * * * * * * * * * * * * * *'.     00111200
111300                                                                  00111300
111400     MOVE  RPT-DT (WORK-MO-N)     TO RPT-REPORT-DATE.             00111400
111500                                                                  00111500
111600 005-ERR-RPT-HD.                                                  00111600
111700                                                                  00111700
111800     ADD  1                       TO RPT-PAGE-CNT.                00111800
111900     MOVE   RPT-PAGE-CNT          TO RPT-PAGE.                    00111900
112000     MOVE    6                    TO RPT-LINE-CNT.                00112000
112200     MOVE  RPT-HD-LINE1           TO RPT-REC.                     00112200
112300     WRITE RPT-REC-OUT  AFTER ADVANCING PAGE.                     00112300
112500     MOVE  SPACES                 TO RPT-REC.                     00112500
112600     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00112600
112800     MOVE  RPT-HD-LINE2           TO RPT-REC.                     00112800
112900     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00112900
113100     MOVE  SPACES                 TO RPT-REC.                     00113100
113200     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00113200
113300                                                                  00113300
113400 ERR-RPT-HD-EXIT.                                                 00113400
113500     EXIT.                                                        00113500
113600                                                                  00113600
113700 010-READ-INPUT-FILE.                                             00113700
113800                                                                  00113800
113900     READ CIA-LIFE-IN                                             00113900
114000         INTO  CIA-WORK                                           00114000
114100             AT END                                               00114100
114200                GO                TO  9999-END-OF-JOB.            00114200
114300                                                                  00114300
114400     ADD  1                       TO IN-CNT.                      00114400
114500                                                                  00114500
114600     PERFORM ACCT-CONVERSION                                      00114600
114700        THRU ACCT-CONVERSION-EXIT.                                00114700
114800                                                                  00114800
114900     GO  TO  GET-RECORDS.                                         00114900
115000                                                                  00115000
115100 READ-INPUT-EXIT.                                                 00115100
115200     EXIT.                                                        00115200
115300                                                                  00115300
115400 GET-RECORDS.                                                     00115400
115500                                                                  00115500
115600*    DISPLAY 'GET-RECORDS  ENTERED '.                             00115600
115700                                                                  00115700
115800     IF  FIRST-SW  =  'Y'                                         00115800
115900         NEXT SENTENCE                                            00115900
116000       ELSE                                                       00116000
116100         GO TO GET-RECORDS-DONE.                                  00116100
116200                                                                  00116200
116300     IF  REC-TYPE  =  'I'                                         00116300
116400         MOVE  CIA-NEW-ISS-STATE     TO SAVE-STATE                00116400
116500      ELSE                                                        00116500
116600         MOVE  CIA-NEW-CANC-STATE    TO SAVE-STATE.               00116600
116700                                                                  00116700
116800      ADD    1                       TO   CIA-BATCH-NO.           00116800
112007      MOVE ZEROS TO CIA-BATCH-NO
116900                                                                  00116900
117000      PERFORM  MAKE-BATCH-UNIQUE                                  00117000
117100          THRU MAKE-BATCH-UNIQUE-EXIT.                            00117100
117200                                                                  00117200
117300      ADD    1                       TO   HDR-CNT.                00117300
117400      MOVE  'N'                      TO  FIRST-SW.                00117400
011121      move cia-acct-expanded         to save-full-acct
117500*     MOVE  CIA-ACCT                 TO SAVE-ACCT-6.              00117500
117600*     MOVE  ZEROS                    TO SAVE-ACCT.                00117600
117700                                                                  00117700
117800      PERFORM  ACCT-BREAK-CONT                                    00117800
117900          THRU  CK-FOR-CONLEY-ACCT-EXIT.                          00117900
118000                                                                  00118000
118100 GET-RECORDS-DONE.                                                00118100
118200                                                                  00118200
011121     if cia-acct-expanded = save-full-acct
118300*    IF   CIA-ACCT = SAVE-ACCT-6                                  00118300
118400          GO TO CK-FOR-CANC                                       00118400
118500       ELSE                                                       00118500
118600          PERFORM ACCT-BREAK                                      00118600
118700            THRU  ACCT-BREAK-EXIT.                                00118700
118800                                                                  00118800
118900 CK-FOR-CANC.                                                     00118900
119000                                                                  00119000
119100     IF  REC-TYPE =  'I'                                          00119100
119200          GO                      TO DO-ISSUE                     00119200
119300       ELSE                                                       00119300
119400*         DISPLAY 'CANCEL REC FOUND'                              00119400
119500           ADD  1                 TO  CANC-CNT                    00119500
119600            GO                    TO CANCEL-BLD.                  00119600
119700                                                                  00119700
119800 SAVE-STATES.                                                     00119800
119900                                                                  00119900
120000     MOVE  ZEROS  TO  ST-SUB.                                     00120000
120100                                                                  00120100
120200 SAVE-STATES-LOOP.                                                00120200
120300                                                                  00120300
120400     ADD  1       TO  ST-SUB.                                     00120400
120500                                                                  00120500
120600     IF   ST-SUB  GREATER THAN 80                                 00120600
120700      DISPLAY 'ST-SUB GREATER THAN 80 '                           00120700
120800             GO  TO  SAVE-STATES-EXIT.                            00120800
120900                                                                  00120900
121000     IF   CIA-NEW-ISS-STATE  =  ST-TABLE (ST-SUB)                 00121000
121100*     DISPLAY 'EQUAL - ' CIA-NEW-ISS-STATE                        00121100
121200          GO  TO  SAVE-STATES-EXIT.                               00121200
121300                                                                  00121300
121400     IF   ST-TABLE (ST-SUB)  =  SPACES                            00121400
121500      DISPLAY 'NEW STATE - '  CIA-NEW-ISS-STATE                   00121500
121600          MOVE CIA-NEW-ISS-STATE  TO  ST-TABLE (ST-SUB)           00121600
121700             GO  TO  SAVE-STATES-EXIT.                            00121700
121800                                                                  00121800
121900     GO  TO  SAVE-STATES-LOOP.                                    00121900
122000                                                                  00122000
122100                                                                  00122100
122200 SAVE-STATES-EXIT.                                                00122200
122300      EXIT.                                                       00122300
122400                                                                  00122400
122500 DO-ISSUE.                                                        00122500
122600                                                                  00122600
122700     IF  REC-TYPE =  'I'                                          00122700
122800*        DISPLAY 'ISSUE REC FOUND'                                00122800
122500         GO TO DO-ISSUE-CNTD                                      00122810
122900      ELSE                                                        00122900
123000         MOVE CIA-WORK            TO  PT-REC                      00123000
123100         MOVE    ' RECORD TYPE NOT ISSUE OR CANCEL ' TO PT-ERROR  00123100
123200         PERFORM 0200-RPT THRU  0200-RPT-EXIT                     00123200
123300         DISPLAY ' RECORD TYPE NOT ISSUE OR CANCEL ' CIA-WORK     00123300
123400           MOVE SPACES            TO CIA-WORK                     00123400
123500           ADD  1                 TO  RPT-CNT                     00123500
123600              GO                  TO  010-READ-INPUT-FILE.        00123600
122400                                                                  00123610
122500 DO-ISSUE-CNTD.                                                   00123620
123700                                                                  00123700
123800     IF  REC-TYPE  =  'I'                                         00123800
123900         PERFORM SAVE-STATES                                      00123900
124000            THRU  SAVE-STATES-EXIT.                               00124000
124100                                                                  00124100
124200     ADD    1                     TO TOT-CERT-ISS.                00124200
124300     ADD    1                     TO  CERT-CNT.                   00124300
124400                                                                  00124400
124500     MOVE  ZEROS  TO  RPT-SUB.                                    00124500
124600     MOVE  ZEROS  TO  CODE-SUB.                                   00124600
124700                                                                  00124700
124800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00124800
124900* * * * * * * * * * START ISSUE  PROCESSING * * * * * * * * * *   00124900
125000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00125000
125100                                                                  00125100
125200* = = = = = = =  ICARD  = = = = = = = = =                         00125200
125300                                                                  00125300
125400 ISSUE-BLD.                                                       00125400
125500                                                                  00125500
125600*    DISPLAY 'ISSUE-BLD  ENTERED  '.                              00125600
125700                                                                  00125700
125800* *  BUILD THE SEQUENCE 1 RECORD   * * * * * * * *                00125800
125900                                                                  00125900
126000     MOVE  'ISSUE -  SEQUENCE 1 ' TO  SAVE-SEQ.                   00126000
126100                                                                  00126100
126200     MOVE  'Y' TO  LIFE-TYPE-SW.                                  00126200
126300     MOVE   SPACES                TO  PBI-RECORD-BODY.            00126300
126400                                                                  00126400
126500     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00126500
126600     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00126600
126700     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00126700
126800     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00126800
040414     move pbi-cert-eff-dt-x       to dc-greg-date-1-mdy-r
040414     move '4'                     to dc-option-code
040414     perform 8500-date-convert    thru 8500-exit
040414     if no-conversion-error
040414        move dc-bin-date-1        to ws-bin-eff-dt
040414     else
040414        display ' invalid eff dt ' work-date-n ' ' cia-cert-9
040414        move low-values           to ws-bin-eff-dt
040414     end-if
126900                                                                  00126900
030310     MOVE   ZEROS                 TO  SAVE-CERT01.                00127000
030310     MOVE   CIA-CERT-9            TO  SAVE-CERT09.                00127100
127200     MOVE   CIA-CERT-1            TO  SAVE-CERTSFX.               00127200
127300                                                                  00127300
127400     IF     SAVE-CERTSFX-N NOT NUMERIC                            00127400
127500        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00127500
127600            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00127600
127700            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00127700
127800        ELSE                                                      00127800
127900*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00127900
128000            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00128000
128100            MOVE   SPACES         TO  PBI-CERT-SFX.               00128100
128200                                                                  00128200
128300     INSPECT PBI-CERT-PRIME REPLACING ALL ' ' BY ZERO.            00128300
128400                                                                  00128400
128500     MOVE CIA-NAME               TO PBI-I-INS-LAST-NAME
051508     MOVE CIA-INS-FNAME          TO PBI-I-INS-1ST-NAME
051508     IF PBI-I-INS-1ST-NAME = SPACES
051508        MOVE CIA-INIT-1           TO PBI-I-INS-1ST-INIT
051508     END-IF
128600*    MOVE   CIA-INIT-1            TO  PBI-I-INS-1ST-INIT
128700     MOVE   CIA-INIT-2            TO  PBI-I-INS-MIDDLE-INIT.      00128700
128800                                                                  00128800
128900     IF   CIA-AGE-N  NOT NUMERIC                                  00128900
129000            MOVE 40 TO  CIA-AGE-N.                                00129000
129100     MOVE CIA-AGE                 TO  PBI-I-INSURED-AGE-X.        00129100
129200     MOVE CIA-SEX-CD              TO  PBI-I-INSURED-SEX.          00129200
129300     MOVE SPACES                  TO  PBI-I-SOC-SEC-NO.           00129300
129400     MOVE SPACES                  TO  PBI-I-MEMBER-NO.
040414     if cia-ins-dob not = spaces and zeros
040414        move cia-ins-dob (1:4)    to  pbi-i-birthday-x (1:4)
040414        move cia-ins-dob (7:2)    to  pbi-i-birthday-x (5:2)
040414     else
040414        MOVE SPACES               TO  PBI-I-BIRTHDAY-X
040414     end-if
129600     MOVE SPACES                  TO  PBI-I-ENTRY-CD.             00129600
129700     MOVE ' '                     TO  PBI-I-FORCE-CD.             00129700
129800     MOVE '2'                     TO  PBI-TRANS-TYPE.             00129800
129900     MOVE '1'                     TO  PBI-SEQUENCE.               00129900
130000                                                                  00130000
130100     IF     SAVE-CERTSFX-N NOT NUMERIC                            00130100
130200        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00130200
130300            MOVE   SAVE-CERT      TO  OUT-CERT-NO                 00130300
130400            MOVE   SAVE-CERTSFX   TO  OUT-CERT-SFX                00130400
130500        ELSE                                                      00130500
130600*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00130600
130700            MOVE   S-CERT10       TO  OUT-CERT-NO                 00130700
130800            MOVE   SPACES         TO  OUT-CERT-SFX.               00130800
130900                                                                  00130900
131000     INSPECT OUT-CERT-NO REPLACING ALL ' '  BY ZERO.              00131000
131100                                                                  00131100
131200     MOVE  WORK-DATE-N            TO  OUT-EFF-DATE.               00131200
131300     MOVE  PBI-I-ISSUE-REC-SEQ-1  TO  OUT-INFO.                   00131300
131400     MOVE  '2'                    TO  OUT-TRAN-TYPE               00131400
131500     MOVE  '1'                    TO  OUT-SEQUENCE
112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
131600                                                                  00131600
131700     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00131700
131800                                                                  00131800
131900* *  END OF  SEQUENCE 1 RECORD BUILD  * * * * * * * *             00131900
132000                                                                  00132000
132100                                                                  00132100
132200* *  BUILD THE SEQUENCE 2 RECORD   * * * * * * * *                00132200
132300                                                                  00132300
132400 SEQ2-BLD.                                                        00132400
132500                                                                  00132500
132600     MOVE  SPACES TO  PBI-RECORD-BODY.                            00132600
132700                                                                  00132700
132800     MOVE  'ISSUE -  SEQUENCE 2 ' TO  SAVE-SEQ.                   00132800
132900                                                                  00132900
133000     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00133000
133100     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00133100
133200     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00133200
133300     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00133300
133400                                                                  00133400
133500     IF     SAVE-CERTSFX-N NOT NUMERIC                            00133500
133600        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00133600
133700            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00133700
133800            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00133800
133900        ELSE                                                      00133900
134000*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00134000
134100            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00134100
134200            MOVE   SPACES         TO  PBI-CERT-SFX.               00134200
134300                                                                  00134300
134400                                                                  00134400
134500     IF CIA-JOINT-NAME > SPACES
      *       MOVE '40'                TO PBI-I-JOINT-AGE-X
040414        move cia-joint-age       to pbi-i-joint-age-x
134700        PERFORM DO-JOINT-NAME    THRU DO-JOINT-NAME-EXIT
134900        MOVE SPACES              TO PBI-I-JNT-MIDDLE-INIT
040414        if cia-jnt-dob not = spaces and zeros
040414           move cia-jnt-dob (1:4) to pbi-i-jnt-birthday-x
040414           move cia-jnt-dob (7:2) to pbi-i-jnt-birthday-x (5:2)
040414        end-if
135000     ELSE
135100        MOVE SPACES              TO PBI-I-JNT-LAST-NAME
135200        MOVE SPACES              TO PBI-I-JNT-1ST-INIT
135300        MOVE SPACES              TO PBI-I-JNT-MIDDLE-INIT
135400        MOVE   '00'              TO PBI-I-JOINT-AGE-X
           end-if
135500                                                                  00135500
135600     MOVE   SPACES                TO  PBI-I-POLICY-FORM-NO.       00135600
135700     MOVE   '2'                   TO  PBI-TRANS-TYPE.             00135700
135800     MOVE   '2'                   TO  PBI-SEQUENCE.               00135800
135900                                                                  00135900
136000     IF     SAVE-CERTSFX-N NOT NUMERIC                            00136000
136100        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00136100
136200            MOVE   SAVE-CERT      TO  OUT-CERT-NO                 00136200
136300            MOVE   SAVE-CERTSFX   TO  OUT-CERT-SFX                00136300
136400        ELSE                                                      00136400
136500*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00136500
136600            MOVE   S-CERT10       TO  OUT-CERT-NO                 00136600
136700            MOVE   SPACES         TO  OUT-CERT-SFX.               00136700
136800                                                                  00136800
136900     INSPECT OUT-CERT-NO REPLACING ALL ' '  BY ZERO.              00136900
137000                                                                  00137000
137100     MOVE   WORK-DATE-N           TO  OUT-EFF-DATE.               00137100
137200     MOVE   PBI-I-ISSUE-REC-SEQ-2 TO  OUT-INFO.                   00137200
137300     MOVE   '2'                   TO  OUT-TRAN-TYPE               00137300
137400     MOVE   '2'                   TO  OUT-SEQUENCE.               00137400
112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
137500                                                                  00137500
137600     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00137600
137700                                                                  00137700
137800* *  END OF  SEQUENCE 2 RECORD BUILD  * * * * * * * *             00137800
137900                                                                  00137900
138000                                                                  00138000
138100* *  BUILD THE SEQUENCE 3 RECORD   * * * * * * * *                00138100
138200                                                                  00138200
138300 SEQ3-BLD.                                                        00138300
138400                                                                  00138400
138500*    DISPLAY '* * SEQ3 - FILL THE LIFE FIELDS     * * '.          00138500
138600                                                                  00138600
138700     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00138700
138800                                                                  00138800
138900     MOVE  'ISSUE -  SEQUENCE 3 ' TO  SAVE-SEQ.                   00138900
139000                                                                  00139000
139100     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00139100
139200     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00139200
139300     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00139300
139400     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00139400
139500                                                                  00139500
139600     IF     SAVE-CERTSFX-N NOT NUMERIC                            00139600
139700        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00139700
139800            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00139800
139900            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00139900
140000        ELSE                                                      00140000
140100*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00140100
140200            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00140200
140300            MOVE   SPACES         TO  PBI-CERT-SFX.               00140300
140400                                                                  00140400
140500                                                                  00140500
140600     INSPECT CIA-LIFE-AMT  REPLACING ALL  ' ' BY '0'.             00140600
140700                                                                  00140700
140800     IF     CIA-LIFE-AMT-N  NUMERIC                               00140800
140900         MOVE   CIA-LIFE-AMT-N    TO  PBI-I-LF-BENEFIT-AMT        00140900
141000      ELSE                                                        00141000
141100         DISPLAY 'CIA-LIFE-AMT-N NOT NUMERIC - ' CIA-LIFE-AMT-N   00141100
141200         MOVE   ZEROS             TO  PBI-I-LF-BENEFIT-AMT.       00141200
141300                                                                  00141300
141400     MOVE ZEROS                   TO CODE-SUB.                    00141400
141500     MOVE  'N'                    TO LIFE-TYPE-SW.                00141500
141600                                                                  00141600
141700     IF   CIA-LIFE-CD  =  '  '                                    00141700
141800        MOVE  SPACES              TO PBI-I-LF-BENEFIT-TYPE        00141800
141900        MOVE  'N'                 TO LIFE-TYPE-SW                 00141900
142000          GO                      TO   I-LIFE-DONE.               00142000
142100                                                                  00142100
142200*    DISPLAY ' LIFE-LOOP-N ENTERED  '.                            00142200
142300                                                                  00142300
144200 LIFE-LOOP.                                                       00144200
144210                                                                  00144210
144220     IF  CODE-SUB > 129                                           00144220
144231      DISPLAY ' SUB > 129 - GOING TO SET-INVALID-LIFE-TYPE '      00144231
144240       MOVE ZEROS                 TO  CODE-SUB                    00144240
144250        MOVE  'N'                 TO LIFE-TYPE-SW                 00144250
144251         GO                       TO SET-INVALID-L-TYPE.          00144251
144300                                                                  00144300
144400     ADD  1  TO  CODE-SUB.                                        00144400
144500                                                                  00144500
144600     IF   CON-LIF-CD (CODE-SUB) = '  '                            00144600
144700        GO                           TO LIFE-LOOP.                00144700
144800                                                                  00144800
144900     IF   CIA-LIFE-CD  = CON-LIF-CD (CODE-SUB)                    00144900
144910*      DISPLAY ' CON-LIF-CD (CODE-SUB) = CIA-LIFE-CD '            00144910
145000        MOVE  CSO-LIF-CD (CODE-SUB) TO PBI-I-LF-BENEFIT-TYPE      00145000
145100        MOVE  'Y'                 TO LIFE-TYPE-SW                 00145100
145200          GO                      TO I-LIFE-DONE.                 00145200
145700                                                                  00145700
145800     GO                           TO LIFE-LOOP.                   00145800
145900                                                                  00145900
146050                                                                  00146050
146100 SET-INVALID-L-TYPE.                                              00146100
146200                                                                  00146200
146300     MOVE SPACES TO RPT-PT-LINE                                   00146300
146400     PERFORM 0200-RPT  THRU  0200-RPT-EXIT                        00146400
146500     MOVE    'CIA-LIFE-CD NOT IN CONVERSION TABLE ' TO PT-ERROR   00146500
146600     MOVE    CIA-WORK  TO  PT-REC                                 00146600
146700     PERFORM 0200-RPT  THRU  0200-RPT-EXIT.                       00146700
146800                                                                  00146800
146900     DISPLAY '  '.                                                00146900
147000     DISPLAY 'CIA-LIFE-CD NOT IN CONVERSION TABLE '               00147000
147100     DISPLAY 'CIA-LIFE-CD = ' CIA-LIFE-CD.                        00147100
147200     DISPLAY 'CIA-LIFE-CD SET TO ZEROS'                           00147200
147300     MOVE  '  '  TO CIA-LIFE-CD                                   00147300
147400                    PBI-I-LF-BENEFIT-TYPE.                        00147400
147500                                                                  00147500
147600 I-LIFE-DONE.                                                     00147600
147700                                                                  00147700
147800     INSPECT CIA-TERM-LF   REPLACING ALL  ' ' BY '0'.             00147800
147900                                                                  00147900
148000     IF    CIA-TERM-LF-N NUMERIC                                  00148000
148100          MOVE  CIA-TERM-LF-N       TO  PBI-I-LF-TERM             00148100
148200       ELSE                                                       00148200
148300          MOVE  ZEROS               TO  PBI-I-LF-TERM.            00148300
148400                                                                  00148400
148500     INSPECT CIA-LIFE-PREM REPLACING ALL  ' ' BY '0'.             00148500
148600                                                                  00148600
148700     IF    CIA-LIFE-PREM-N  NUMERIC                               00148700
148800         MOVE  CIA-LIFE-PREM-N      TO  PBI-I-LF-PREM-AMT         00148800
148900       ELSE                                                       00148900
149000         DISPLAY                                                  00149000
149100             'CIA-LIFE-PREM-N  NOT NUMERIC - ' CIA-LIFE-PREM-N    00149100
149200         MOVE  ZEROS                TO  PBI-I-LF-PREM-AMT.        00149200
149300                                                                  00149300
149400     MOVE  ZEROS                    TO  PBI-I-LF-CRIT-PERIOD      00149400
149500                                        PBI-I-LF-EXPIRE-YR        00149500
149600                                        PBI-I-LF-EXPIRE-MO        00149600
149700                                        PBI-I-LF-EXPIRE-DA        00149700
149800                                        PBI-I-LF-ALT-BENEFIT-AMT  00149800
149900                                        PBI-I-LF-ALT-PREM-AMT     00149900
150000                                        PBI-I-TERM-AS-DAYS.       00150000
150100                                                                  00150100
150200     MOVE '2'                       TO  PBI-TRANS-TYPE.           00150200
150300     MOVE '3'                       TO  PBI-SEQUENCE.             00150300
150400                                                                  00150400
150500     IF   PBI-I-LF-PREM-AMT NUMERIC                               00150500
150600        ADD  PBI-I-LF-PREM-AMT      TO CIA-LIFE-WRITTEN           00150600
150700                                       TOT-LIFE-WRITTEN.          00150700
150800                                                                  00150800
150900     IF     SAVE-CERTSFX-N NOT NUMERIC                            00150900
151000        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00151000
151100            MOVE   SAVE-CERT      TO  OUT-CERT-NO                 00151100
151200            MOVE   SAVE-CERTSFX   TO  OUT-CERT-SFX                00151200
151300        ELSE                                                      00151300
151400*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00151400
151500            MOVE   S-CERT10       TO  OUT-CERT-NO                 00151500
151600            MOVE   SPACES         TO  OUT-CERT-SFX.               00151600
151700                                                                  00151700
151800     INSPECT OUT-CERT-NO REPLACING ALL ' '  BY ZERO.              00151800
151900                                                                  00151900
152000     MOVE  WORK-DATE-N              TO  OUT-EFF-DATE.             00152000
152100     MOVE  PBI-I-ISSUE-REC-SEQ-3    TO  OUT-INFO.                 00152100
152200     MOVE  '2'                      TO  OUT-TRAN-TYPE             00152200
152300     MOVE  '3'                      TO  OUT-SEQUENCE.             00152300
112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
152400                                                                  00152400
152500     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00152500
152600                                                                  00152600
152700* *  END OF  SEQUENCE 3 RECORD BUILD  * * * * * * * *             00152700
152800                                                                  00152800
152900                                                                  00152900
153000* *  BUILD THE SEQUENCE 4 RECORD   * * * * * * * *                00153000
153100                                                                  00153100
153200 SEQ4-BLD.                                                        00153200
153300                                                                  00153300
153400*   PROCESS-A-H.                                                  00153400
153500                                                                  00153500
153600*    DISPLAY '* *     FILL THE I-CARD AH FIELDS     * * '.        00153600
153700                                                                  00153700
153800     MOVE  'ISSUE -  SEQUENCE 4 '   TO  SAVE-SEQ.                 00153800
153900                                                                  00153900
154000     MOVE  SPACES TO  PBI-RECORD-BODY.                            00154000
154100                                                                  00154100
154200     MOVE   CIA-EFF-YR              TO  WORK-YR-X.                00154200
154300     MOVE   CIA-EFF-MO              TO  WORK-MO-X.                00154300
154400     MOVE   CIA-EFF-DA              TO  WORK-DA-X.                00154400
154500     MOVE   WORK-DATE-N             TO  PBI-CERT-EFF-DT-X.        00154500
154600                                                                  00154600
154700     IF     SAVE-CERTSFX-N NOT NUMERIC                            00154700
154800        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00154800
154900            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00154900
155000            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00155000
155100        ELSE                                                      00155100
155200*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00155200
155300            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00155300
155400            MOVE   SPACES         TO  PBI-CERT-SFX.               00155400
155500                                                                  00155500
155600                                                                  00155600
155700                                                                  00155700
155800     INSPECT CIA-TERM-AH   REPLACING ALL  ' ' BY '0'.             00155800
155900                                                                  00155900
156000     IF    CIA-TERM-AH-N NUMERIC                                  00156000
156100          MOVE  CIA-TERM-AH-N       TO  PBI-I-AH-TERM             00156100
156200       ELSE                                                       00156200
156300          MOVE  ZEROS               TO  PBI-I-AH-TERM.            00156300
156400                                                                  00156400
156500     INSPECT CIA-AH-PREM REPLACING ALL  ' ' BY '0'.               00156500
156600                                                                  00156600
156700     IF    CIA-AH-PREM-N    NOT  NUMERIC                          00156700
156800     DISPLAY ' CIA-AH-PREM-N  NOT  NUMERIC - ' CIA-AH-PREM-N      00156800
156900          MOVE  ZEROS  TO  CIA-AH-PREM-N.                         00156900
157000                                                                  00157000
157100     IF    CIA-AH-PREM-N   GREATER THAN ZEROS                     00157100
157200           GO TO SEQ-4-CONT                                       00157200
157300       ELSE                                                       00157300
157400           MOVE  ZEROS                  TO  PBI-I-AH-PREM-AMT     00157400
157500           MOVE  ZEROS                  TO  PBI-I-AH-BENEFIT-AMT  00157500
157600           MOVE  SPACE                  TO  PBI-I-AH-BENEFIT-POS1 00157600
157700           MOVE  SPACES                 TO  PBI-I-AH-BENEFIT-NO   00157700
157800           MOVE  ZEROS                  TO  PBI-I-AH-TERM         00157800
157900           MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD  00157900
158000           MOVE  ZEROS                  TO  PBI-I-AH-TERM         00158000
158100           MOVE  ZEROS                  TO  PBI-I-AH-CRIT-PERIOD  00158100
158200           MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-YR    00158200
158300           MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-MO    00158300
158400           MOVE  ZEROS                  TO  PBI-I-AH-EXPIRE-DA    00158400
158500           MOVE  ZEROS                  TO  PBI-I-AH-PAYMENT      00158500
158600           MOVE  WORK-DATE-N            TO  OUT-EFF-DATE          00158600
158700           MOVE  PBI-I-ISSUE-REC-SEQ-4  TO  OUT-INFO              00158700
158800           MOVE  '2'                    TO  OUT-TRAN-TYPE         00158800
158900           MOVE  '4'                    TO  OUT-SEQUENCE.         00158900
159000                                                                  00159000
159100     IF     SAVE-CERTSFX-N NOT NUMERIC                            00159100
159200        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00159200
159300            MOVE   SAVE-CERT      TO  OUT-CERT-NO                 00159300
159400            MOVE   SAVE-CERTSFX   TO  OUT-CERT-SFX                00159400
159500        ELSE                                                      00159500
159600*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00159600
159700            MOVE   S-CERT10       TO  OUT-CERT-NO                 00159700
159800            MOVE   SPACES         TO  OUT-CERT-SFX.               00159800
159900                                                                  00159900
160000     INSPECT OUT-CERT-NO REPLACING ALL ' '  BY ZERO.              00160000
160100                                                                  00160100
160200     GO  TO  AH-EXIT.                                             00160200
160300                                                                  00160300
160400 SEQ-4-CONT.                                                      00160400
160500                                                                  00160500
160600     MOVE  CIA-AH-PREM-N                TO  PBI-I-AH-PREM-AMT.    00160600
160700                                                                  00160700
160800     INSPECT CIA-AH-AMT  REPLACING ALL  ' ' BY '0'.               00160800
160900                                                                  00160900
161000     IF    CIA-AH-AMT-N  NOT  NUMERIC                             00161000
161100     DISPLAY 'CIA-AH-AMT-N NOT NUMERIC - ' CIA-AH-AMT-N           00161100
161200         MOVE  ZEROS                    TO  PBI-I-AH-BENEFIT-AMT  00161200
161300      ELSE                                                        00161300
161400         MOVE  CIA-AH-AMT-N             TO  PBI-I-AH-BENEFIT-AMT. 00161400
161500                                                                  00161500
161600     MOVE ZEROS TO CODE-SUB.                                      00161600
161700                                                                  00161700
161800     IF  LIFE-TYPE-SW = 'N'                                       00161800
161900       IF   CIA-AH-CD   =  '  '                                   00161900
162000            GO TO INVALID-AH-COVG.                                00162000
162100                                                                  00162100
162200       IF   CIA-AH-CD   =  '  '                                   00162200
162300          GO            TO  AH-COVG-DONE.                         00162300
162400                                                                  00162400
162500 AH-LOOP.                                                         00162500
162800                                                                  00162800
162810     IF  CODE-SUB > 129                                           00162810
162820       MOVE ZEROS TO CODE-SUB                                     00162820
162821         GO TO   INVALID-AH-COVG.                                 00162821
162822                                                                  00162822
162823     ADD  1  TO  CODE-SUB.                                        00162823
162840                                                                  00162840
162900     IF   CON-AH-CD (CODE-SUB) = '  '                             00162900
163000          GO   TO AH-LOOP.                                        00163000
163100                                                                  00163100
163200     IF   CIA-AH-CD  = CON-AH-CD (CODE-SUB)                       00163200
163300       MOVE CSO-AH-CD (CODE-SUB)  TO PBI-I-AH-BENEFIT-NO          00163300
163400         MOVE SPACE               TO PBI-I-AH-BENEFIT-POS1        00163400
163500           GO                     TO  AH-COVG-DONE.               00163500
163600                                                                  00163600
164000     GO   TO AH-LOOP.                                             00164000
164010                                                                  00164010
164200 INVALID-AH-COVG.                                                 00164200
164300                                                                  00164300
164400     IF  LIFE-TYPE-SW = 'N'                                       00164400
164500       IF   CIA-AH-CD   =  '  '                                   00164500
164600         MOVE SPACES TO RPT-PT-LINE                               00164600
164700         PERFORM 0200-RPT  THRU  0200-RPT-EXIT                    00164700
164800         MOVE    'NO LIFE OR AH CODE IN INPUT REC   ' TO PT-ERROR 00164800
164900         MOVE    CIA-WORK  TO  PT-REC                             00164900
165000         PERFORM 0200-RPT  THRU  0200-RPT-EXIT                    00165000
165100         GO TO AH-COVG-DONE.                                      00165100
165200                                                                  00165200
165300     MOVE SPACES TO RPT-PT-LINE                                   00165300
165400     PERFORM 0200-RPT  THRU  0200-RPT-EXIT                        00165400
165500     MOVE    'CIA-AH-CD NOT IN CONVERSION TABLES ' TO PT-ERROR    00165500
165600     MOVE    CIA-WORK  TO  PT-REC                                 00165600
165700     PERFORM 0200-RPT  THRU  0200-RPT-EXIT                        00165700
165800                                                                  00165800
165900     DISPLAY '  '.                                                00165900
166000     DISPLAY 'CIA-AH-CD NOT IN CONVERSION TABLES '                00166000
166100     DISPLAY 'CIA-AH-CD = ' CIA-AH-CD.                            00166100
166200     DISPLAY 'CIA-AH-CD SET TO ZEROS'                             00166200
166300     MOVE  '  '  TO CIA-AH-CD                                     00166300
166400     MOVE  SPACES TO PBI-I-AH-BENEFIT-TYPE.                       00166400
166500                                                                  00166500
166600 AH-COVG-DONE.                                                    00166600
166700                                                                  00166700
166800     MOVE  ZEROS                        TO  PBI-I-AH-CRIT-PERIOD  00166800
166900                                            PBI-I-AH-EXPIRE-YR    00166900
167000                                            PBI-I-AH-EXPIRE-MO    00167000
167100                                            PBI-I-AH-EXPIRE-DA.   00167100
167200                                                                  00167200
167300     IF    CIA-AH-AMT-N  NOT  NUMERIC                             00167300
167400           MOVE  ZEROS                  TO  CIA-AH-AMT-N.         00167400
167500     MOVE  CIA-AH-AMT-N                 TO  PBI-I-AH-PAYMENT.     00167500
167600                                                                  00167600
167700     MOVE '2'                           TO  PBI-TRANS-TYPE.       00167700
167800     MOVE '4'                           TO  PBI-SEQUENCE.         00167800
167900                                                                  00167900
168000     ADD  PBI-I-AH-PREM-AMT             TO CIA-AH-WRITTEN.        00168000
168100     ADD  PBI-I-AH-PREM-AMT             TO TOT-AH-WRITTEN.        00168100
168200                                                                  00168200
168300     IF     SAVE-CERTSFX-N NOT NUMERIC                            00168300
168400        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00168400
168500            MOVE   SAVE-CERT      TO  OUT-CERT-NO                 00168500
168600            MOVE   SAVE-CERTSFX   TO  OUT-CERT-SFX                00168600
168700        ELSE                                                      00168700
168800*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00168800
168900            MOVE   S-CERT10       TO  OUT-CERT-NO                 00168900
169000            MOVE   SPACES         TO  OUT-CERT-SFX.               00169000
169100                                                                  00169100
169200     INSPECT OUT-CERT-NO REPLACING ALL ' '  BY ZERO.              00169200
169300                                                                  00169300
169400     MOVE  WORK-DATE-N                  TO  OUT-EFF-DATE.         00169400
169500     MOVE  PBI-I-ISSUE-REC-SEQ-4        TO  OUT-INFO.             00169500
169600     MOVE  '2'                          TO  OUT-TRAN-TYPE         00169600
169700     MOVE  '4'                          TO  OUT-SEQUENCE.         00169700
169800                                                                  00169800
169900 AH-EXIT.                                                         00169900
170000     EXIT.                                                        00170000
170100                                                                  00170100
170200 AH-WRITE.                                                        00170200
170300                                                                  00170300
112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
170400     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00170400
170500                                                                  00170500
170600* *  END OF  SEQUENCE 4 RECORD BUILD  * * * * * * * *             00170600
152600                                                                  00170610
152900                                                                  00170640
153000* *  BUILD THE SEQUENCE 5 RECORD   * * * * * * * *                00170650
153100                                                                  00170660
153200 SEQ5-BLD.                                                        00170670
138600                                                                  00170671
138700     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00170672
138800                                                                  00170673
138900     MOVE  'ISSUE -  SEQUENCE 5 ' TO  SAVE-SEQ.                   00170674
139000                                                                  00170675
139100     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00170676
139200     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00170677
139300     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00170678
139400     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00170679
139500                                                                  00170680
139600     IF     SAVE-CERTSFX-N NOT NUMERIC                            00170681
139700        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00170682
139800            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00170683
139900            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00170684
140000        ELSE                                                      00170685
140100*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00170686
140200            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00170687
140300            MOVE   SPACES         TO  PBI-CERT-SFX.               00170688
140400                                                                  00170689
140500     MOVE CIA-INT-RATE  TO   CIA-INT-RATE-WORK.                   00170690
153300                                                                  00170691
101300     MOVE    CIA-INT-DOL-N    TO  CIA-INT-CNVTD-DOL.              00170692
101400     MOVE    CIA-INT-CENTS-N  TO  CIA-INT-CNVTD-CENTS.            00170694
101400     MOVE    ZEROS            TO  CIA-INT-CNVTD-ZEROS.            00170695
139700     MOVE    CIA-INT-CNVTD    TO  PBI-I-LOAN-APR.                 00170696
103900                                                                  00170697
103900                                                                  00170710
140500     IF   CIA-NEW-ISS-STATE = 'MN'                                00170711
139700     DISPLAY '    '                                               00170712
139700     DISPLAY 'CIA-INT-RATE     = ' CIA-INT-RATE                   00170713
139700     DISPLAY 'PBI-I-LOAN-APR   = ' PBI-I-LOAN-APR.                00170714

040414***-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_***
040414***                                                            ***
040414***  Leave the below code in until they can send us an         ***
040414***  accurate days to 1st payment or 1st payment date.         ***
040414***                                                            ***
040414***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***
040414*    INSPECT CIA-LOAN-TERM REPLACING ALL ' '  BY ZERO
040414*    if cia-loan-term not numeric
040414*       move zeros               to cia-loan-term
040414*    end-if
040414*
040414*    if cia-loan-term-n not = zeros
040414*       move cia-loan-term-n     to pbi-i-loan-term
040414*       display ' loan trm stuff ' out-cert-no ' '
040414*          pbi-i-loan-term
040414*    end-if
040414*
040414*    inspect cia-days-to-1st-pmt REPLACING ALL ' '  BY ZERO
040414*    if cia-days-to-1st-pmt not numeric
040414*       move zeros               to cia-days-to-1st-pmt
040414*    end-if
040414*
040414*    if (cia-days-to-1st-pmt-n not = zeros)
040414*       and (cia-days-to-1st-pmt-n > 30)
040414*       compute pbi-i-ext-days = cia-days-to-1st-pmt-n - 30
040414*       move ws-bin-eff-dt       to dc-bin-date-1
040414*       move cia-days-to-1st-pmt-n to dc-elapsed-days
040414*       move +0                  to dc-elapsed-months
040414*       move '6'                 to dc-option-code
040414*       perform 8500-date-convert thru 8500-exit
040414*       if no-conversion-error
040414*          move dc-greg-date-1-mdy-r to pbi-i-1st-pmt-dt
040414*       else
040414*          move zeros            to pbi-i-1st-pmt-dt
040414*       end-if
040414*       display ' 1st pmt stuff ' out-cert-no ' ' pbi-i-ext-days
040414*          ' ' pbi-i-1st-pmt-dt
040414*    end-if
040414***_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-_-***

169400     MOVE  WORK-DATE-N                  TO  OUT-EFF-DATE.         00170717
169500     MOVE  PBI-I-ISSUE-REC-SEQ-4        TO  OUT-INFO.             00170718
169600     MOVE  '2'                          TO  OUT-TRAN-TYPE         00170719
169700     MOVE  '5'                          TO  OUT-SEQUENCE.         00170720
152400                                                                  00170722
112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
152500     PERFORM  0150-WRITE-DATA-CARD  THRU 0150-EXIT.               00170723
170500                                                                  00170729
170600* *  END OF  SEQUENCE 5 RECORD BUILD  * * * * * * * *             00170730
152600                                                                  00170731
081406* *  BUILD THE SEQUENCE 6 RECORD   * * * * * * * *                00170650
081406                                                                  00170660
081406 SEQ6-BLD.                                                        00170670
081406                                                                  00170671
081406     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00170672
081406                                                                  00170673
081406     MOVE  'ISSUE -  SEQUENCE 6 ' TO  SAVE-SEQ.                   00170674
081406                                                                  00170675
081406     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00170676
081406     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00170677
081406     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00170678
081406     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00170679
081406                                                                  00170680
081406     IF     SAVE-CERTSFX-N NOT NUMERIC                            00170681
081406        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00170682
081406            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00170683
081406            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00170684
081406        ELSE                                                      00170685
081406*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00170686
081406            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00170687
081406            MOVE   SPACES         TO  PBI-CERT-SFX.               00170688
081406                                                                  00170689
           MOVE CIA-INS-ADDR1           TO PBI-I-INSURED-ADDRESS-1
           MOVE CIA-INS-ADDR2           TO PBI-I-INSURED-ADDRESS-2

169400     MOVE  WORK-DATE-N                  TO  OUT-EFF-DATE
169500     MOVE  PBI-I-ISSUE-REC-SEQ-6        TO  OUT-INFO
169600     MOVE  '2'                          TO  OUT-TRAN-TYPE
169700     MOVE  '6'                          TO  OUT-SEQUENCE

112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
      *    IF PBI-I-ISSUE-REC-SEQ-6 NOT = SPACES
152500        PERFORM  0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
      *    END-IF

170600* *  END OF  SEQUENCE 6 RECORD BUILD  * * * * * * * *             00170730
152600                                                                  00170731
081406* *  BUILD THE SEQUENCE 7 RECORD   * * * * * * * *                00170650
           .
081406 SEQ7-BLD.                                                        00170670
081406                                                                  00170671
081406     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00170672
081406                                                                  00170673
081406     MOVE  'ISSUE -  SEQUENCE 7 ' TO  SAVE-SEQ.                   00170674
081406                                                                  00170675
081406     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00170676
081406     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00170677
081406     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00170678
081406     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00170679
081406                                                                  00170680
081406     IF     SAVE-CERTSFX-N NOT NUMERIC                            00170681
081406        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00170682
081406            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00170683
081406            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00170684
081406        ELSE                                                      00170685
081406*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00170686
081406            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00170687
081406            MOVE   SPACES         TO  PBI-CERT-SFX.               00170688
081406                                                                  00170689
051810     MOVE CIA-INS-CITY           TO PBI-I-INSURED-CITY
051810     MOVE CIA-INS-STATE          TO PBI-I-INSURED-STATE
051810*    PERFORM VARYING S1 FROM +30 BY -1 UNTIL
051810*       (S1 < +2)
051810*       OR (PBI-I-INSURED-CITY-STATE (S1:1) NOT = SPACE)
051810*    END-PERFORM
051810*
051810*    IF (S1 < +2)
051810*       OR (S1 > +26)
051810*       CONTINUE
051810*    ELSE
051810*       ADD +1                 TO S1
051810*       MOVE ','               TO PBI-I-INSURED-CITY-STATE (S1:1)
051810*       ADD +2                 TO S1
051810*       MOVE CIA-INS-STATE     TO PBI-I-INSURED-CITY-STATE (S1:2)
051810*    END-IF

           MOVE CIA-INS-ZIP            TO PBI-I-INSURED-ZIP-CODE
152900                                                                  00170740
169400     MOVE  WORK-DATE-N                  TO  OUT-EFF-DATE
169500     MOVE  PBI-I-ISSUE-REC-SEQ-7        TO  OUT-INFO
169600     MOVE  '2'                          TO  OUT-TRAN-TYPE
169700     MOVE  '7'                          TO  OUT-SEQUENCE

112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
      *    IF PBI-I-ISSUE-REC-SEQ-7 NOT = SPACES
152500        PERFORM  0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT
      *    END-IF

170600* *  END OF  SEQUENCE 7 RECORD BUILD  * * * * * * * *             00170730

           .

081406* *  BUILD THE SEQUENCE 8 RECORD   * * * * * * * *                00170650
081406                                                                  00170660
081406 SEQ8-BLD.                                                        00170670
081406                                                                  00170671
081406     MOVE  SPACES                 TO  PBI-RECORD-BODY.            00170672
081406                                                                  00170673
081406     MOVE  'ISSUE -  SEQUENCE 8 ' TO  SAVE-SEQ.                   00170674
081406                                                                  00170675
081406     MOVE   CIA-EFF-YR            TO  WORK-YR-X.                  00170676
081406     MOVE   CIA-EFF-MO            TO  WORK-MO-X.                  00170677
081406     MOVE   CIA-EFF-DA            TO  WORK-DA-X.                  00170678
081406     MOVE   WORK-DATE-N           TO  PBI-CERT-EFF-DT-X.          00170679
081406                                                                  00170680
081406     IF     SAVE-CERTSFX-N NOT NUMERIC                            00170681
081406        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00170682
081406            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00170683
081406            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00170684
081406        ELSE                                                      00170685
081406*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00170686
081406            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00170687
081406            MOVE   SPACES         TO  PBI-CERT-SFX.               00170688
081406                                                                  00170689
050509     IF CIA-CRED-BENE NOT = SPACES
050509        MOVE CIA-CRED-BENE       TO PBI-I-CRED-BENE-NAME
050509     ELSE
050509        MOVE 'CONLEY'            TO PBI-I-CRED-BENE-NAME
050509     END-IF

169400     MOVE  WORK-DATE-N                  TO  OUT-EFF-DATE
169500     MOVE  PBI-I-ISSUE-REC-SEQ-8        TO  OUT-INFO
169600     MOVE  '2'                          TO  OUT-TRAN-TYPE
169700     MOVE  '8'                          TO  OUT-SEQUENCE

112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
011121     move cia-acct-expanded       to sr-acct-no
112007*    MOVE ZEROS                   TO SR-ACCT-NO
112007*    MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)

152500     PERFORM  0150-WRITE-DATA-CARD
                                       THRU 0150-EXIT

170600* *  END OF  SEQUENCE 8 RECORD BUILD  * * * * * * * *             00170730
152600                                                                  00170731
           .

170900 I-CARD-END.                                                      00170900
171000                                                                  00171000
171100     GO  TO  010-READ-INPUT-FILE.                                 00171100
171200                                                                  00171200
171300 ACCT-CONVERSION.                                                 00171300
171400                                                                  00171400
171500*    IF  CIA-ACCT  =  '575100' OR                                 00171500
171600*                     '638600' OR                                 00171600
171700*                     '639400' OR                                 00171700
171800*                     '640200' OR                                 00171800
171900*                     '640300' OR                                 00171900
172000*                     '640400' OR                                 00172000
172100*                     '640500' OR                                 00172100
172200*                     '640900' OR                                 00172200
172300*                     '641000'                                    00172300
172400*           GO  TO  ACCT-CONVERSION-EXIT.                         00172400
172500                                                                  00172500
011121     move zeros                  to cia-acct-expanded
011121     move cia-acct               to cia-acct-expanded(5:6)
011121
011121     evaluate cia-acct-expanded
011121        when '0000652600'
011121           move '0000457900'     to cia-acct-expanded
011121        when '0000115300'
011121           move '0000957200'     to cia-acct-expanded
011121        when '0000131051'
011121           move '0000638600'     to cia-acct-expanded
011121        when '0000130209'
011121           move '0000640200'     to cia-acct-expanded
011121        when '0000130211'
011121           move '0000640300'     to cia-acct-expanded
011121        when '0000130215'
011121           move '0000640400'     to cia-acct-expanded
011121        when '0000130213'
011121           move '0000640500'     to cia-acct-expanded
011121        when '0000130217'
011121           move '0000640900'     to cia-acct-expanded
011121        when '0000131049'
011121           move '0000641000'     to cia-acct-expanded
011121        when '0000428120'
011121           move '0001585700'     to cia-acct-expanded
011121        when '0000428240'
011121           move '0001585701'     to cia-acct-expanded
011121     end-evaluate
                 
112007*    IF CIA-ACCT = '652600'
      *       MOVE '457900'            TO CIA-ACCT
      *    END-IF
      *
112007*    IF CIA-ACCT = '115300'
      *       MOVE '957200'            TO CIA-ACCT
      *    END-IF
      *
172600*    IF  CIA-ACCT  =  '131051'                                    00172600
172700*        MOVE  '638600' TO  CIA-ACCT                              00172700
172800*           GO  TO  ACCT-CONVERSION-EXIT.                         00172800
172900*                                                                 00172900
173000*    IF  CIA-ACCT  =  '130209'                                    00173000
173100*        MOVE  '640200' TO  CIA-ACCT                              00173100
173200*           GO  TO  ACCT-CONVERSION-EXIT.                         00173200
173300*                                                                 00173300
173400*    IF  CIA-ACCT  =  '130211'                                    00173400
173500*        MOVE  '640300' TO  CIA-ACCT                              00173500
173600*           GO  TO  ACCT-CONVERSION-EXIT.                         00173600
173700*                                                                 00173700
173800*    IF  CIA-ACCT  =  '130215'                                    00173800
173900*        MOVE  '640400' TO  CIA-ACCT                              00173900
174000*           GO  TO  ACCT-CONVERSION-EXIT.                         00174000
174100*                                                                 00174100
174200*    IF  CIA-ACCT  =  '130213'                                    00174200
174300*        MOVE  '640500' TO  CIA-ACCT                              00174300
174400*           GO  TO  ACCT-CONVERSION-EXIT.                         00174400
174500*                                                                 00174500
174600*    IF  CIA-ACCT  =  '130217'                                    00174600
174700*        MOVE  '640900' TO  CIA-ACCT                              00174700
174800*           GO  TO  ACCT-CONVERSION-EXIT.                         00174800
174900*                                                                 00174900
175000*    IF  CIA-ACCT  =  '131049'                                    00175000
175100*        MOVE  '641000' TO  CIA-ACCT                              00175100
175200*           GO  TO  ACCT-CONVERSION-EXIT.                         00175200
011121     .                                                            00175300
175400 ACCT-CONVERSION-EXIT.                                            00175400
175500     EXIT.                                                        00175500
175600                                                                  00175600
175700 ACCT-BREAK.                                                      00175700
175800                                                                  00175800
175900*     DISPLAY 'ACCT-BREAK  ENTERED  '.                            00175900
176000                                                                  00176000
176100      MOVE   '0'                   TO   CIA-SEQUENCE-NO.          00176100
176200                                                                  00176200
176300      IF     CIA-ACCT-SW  =  'E'                                  00176300
176400             MOVE   '9999'         TO   SAVE-ACCT.                00176400
176500                                                                  00176500
176600      MOVE   SAVE-FULL-ACCT        TO   PBI-ACCOUNT               00176600
176700                                        CIA-ACCT-NO.              00176700
176800      MOVE   SAVE-STATE            TO   PBI-STATE.                00176800
176900      MOVE   SAVE-STATE            TO   CIA-STATE.                00176900
177000                                                                  00177000
177100      MOVE   '9'                   TO   PBI-CARRIER.              00177100
177200      MOVE   '000000'              TO   PBI-GROUPING.             00177200
177300      MOVE   CIA-HD-EFF-DT         TO   PBI-B-BATCH-DT-X.         00177300
177400      MOVE   CERT-CNT              TO   PBI-B-CERT-ISS-COUNT      00177400
177500                                        CIA-CERT-ISS.             00177500
177600      MOVE   CIA-LIFE-WRITTEN      TO   PBI-B-LF-PRM-WRITTEN.     00177600
177700      MOVE   CIA-AH-WRITTEN        TO   PBI-B-AH-PRM-WRITTEN.     00177700
177800      MOVE   CANC-CNT              TO   PBI-B-CERT-CAN-COUNT      00177800
177900                                        CIA-CERT-CANC.            00177900
178000      MOVE   CIA-LIFE-CANC         TO   PBI-B-LF-PRM-CANCELLED.   00178000
178100      MOVE   CIA-AH-CANC           TO   PBI-B-AH-PRM-CANCELLED.   00178100
178200      MOVE   'CSO'                 TO   PBI-CLIENT-ID.            00178200
178300      MOVE   '1'                   TO   PBI-B-TRANS-TYPE.         00178300
178400      MOVE   '0'                   TO   PBI-B-SEQUENCE.           00178400
178500                                                                  00178500
178600      DISPLAY 'CIA-BATCH-HDR  ' CIA-BATCH-HDR.                    00178600
178700                                                                  00178700
178800      MOVE    CIA-BATCH-HDR        TO  OUT-RECORD.                00178800
178900*     MOVE    CIA-BATCH-NO         TO   SR-BATCH-NO
179000*     MOVE    ZEROS                TO   SR-REC-CNT.               00179000
179100                                                                  00179100
112007     MOVE ZEROS                   TO  SR-CERT-NO
           MOVE ZEROS                   TO  SR-CERT-SFX
112007     MOVE SAVE-FULL-ACCT          TO SR-ACCT-NO

179200      WRITE CARD-RECORD FROM OUT-RECORD.                          00179200
179300                                                                  00179300
179400      ADD    1                     TO   HDR-CNT.                  00179400
112007*     ADD    1                     TO   CIA-BATCH-NO.             00179500
179600                                                                  00179600
179700      PERFORM  MAKE-BATCH-UNIQUE                                  00179700
179800          THRU MAKE-BATCH-UNIQUE-EXIT.                            00179800
179900                                                                  00179900
180000      MOVE  ZEROS                  TO   CIA-LIFE-WRITTEN          00180000
180100                                        CIA-LIFE-CANC             00180100
180200                                        CIA-AH-WRITTEN            00180200
180300                                        CIA-AH-CANC               00180300
180400                                        CIA-CERT-ISS              00180400
180500                                        CIA-CERT-CANC             00180500
180600                                        CERT-CNT                  00180600
180700                                        CANC-CNT                  00180700
180800                                        DETAIL-CNT.               00180800
180900                                                                  00180900
181000 ACCT-BREAK-CONT.                                                 00181000
181100                                                                  00181100
011121     move cia-acct-expanded      to save-full-acct
181200*    MOVE   ZEROS                 TO   SAVE-ACCT.                 00181200
181300*    MOVE   CIA-ACCT              TO   SAVE-ACCT-6.               00181300
181400                                                                  00181400
181500     IF  REC-TYPE  =  'I'                                         00181500
181600*        DISPLAY '  '                                             00181600
181700*        DISPLAY ' CIA-NEW-ISS-STATE  = ' CIA-NEW-ISS-STATE       00181700
181800         MOVE  CIA-NEW-ISS-STATE    TO SAVE-STATE                 00181800
181900      ELSE                                                        00181900
182000*        DISPLAY '  '                                             00182000
182100*        DISPLAY ' CIA-NEW-CANC-STATE  = ' CIA-NEW-CANC-STATE     00182100
182200         MOVE  CIA-NEW-CANC-STATE    TO SAVE-STATE.               00182200
182300                                                                  00182300
182400     MOVE  '9'              TO TEST-CARR-A.                       00182400
182500     MOVE  '000000'         TO TEST-GRP-A.                        00182500
182600     MOVE  SAVE-STATE       TO TEST-STATE-A.                      00182600
011121     move save-full-acct    to test-acct-acct
182700*    MOVE  SAVE-ACCT-6      TO TEST-ACCT-A.                       00182700
182800*    MOVE  '0000'           TO TEST-PRE-A.                        00182800
183000                                                                  00183000
183100      PERFORM CK-FOR-CONLEY-ACCT                                  00183100
183200          THRU  CK-FOR-CONLEY-ACCT-EXIT.                          00183200
183300                                                                  00183300
183400 ACCT-BREAK-EXIT.                                                 00183400
183500      EXIT.                                                       00183500
183600                                                                  00183600
183700 CK-FOR-CONLEY-ACCT.                                              00183700
183800                                                                  00183800
183900         DISPLAY '  '                                             00183900
184000         DISPLAY 'CK-FOR-CONLEY-ENTERED '.                        00184000
184100                                                                  00184100
184200     MOVE  'N'  TO  CIA-ACCT-SW.                                  00184200
184300                                                                  00184300
184400                                                                  00184400
184500     GO  TO  CK-FOR-CONLEY-CONTROL.                               00184500
184600                                                                  00184600
184700 CK-FOR-CONLEY-LOOP.                                              00184700
184800                                                                  00184800
184900     READ  ERACCT.                                                00184900
185000                                                                  00185000
185100     IF    ERACCT-FILE-STATUS = '10'                              00185100
185200           DISPLAY                                                00185200
185300           ' FILE STATUS = ' ERACCT-FILE-STATUS                   00185300
185400           GO TO CK-FOR-CONLEY-ACCT-EXIT.                         00185400
185500                                                                  00185500
185600 CK-FOR-CONLEY-CONTROL.                                           00185600
185700                                                                  00185700
185800     IF    AM-CONTROL-A  =  TEST-CONTROL-A                        00185800
185900       ADD  1  TO  ACCT-MATCH                                     00185900
186000        MOVE  'Y'  TO  CIA-ACCT-SW                                00186000
186100           DISPLAY                                                00186100
186200            'CIA-ACCT = ACCT MASTER FILE  ' CIA-ACCT              00186200
186300               GO TO CK-FOR-CONLEY.                               00186300
186400                                                                  00186400
186500                                                                  00186500
186600     IF    AM-CONTROL-A     LESS THAN   TEST-CONTROL-A            00186600
186700       ADD  1  TO  ACCT-MASTER-LOW                                00186700
186800           GO TO CK-FOR-CONLEY-LOOP.                              00186800
186900                                                                  00186900
187000                                                                  00187000
187100     MOVE  'N'  TO  CIA-ACCT-SW.                                  00187100
187200     DISPLAY                                                      00187200
187300     'ACCT MASTER HIGH  ' AM-CONTROL-A                            00187300
187400           DISPLAY                                                00187400
187500           'TEST-CONTROL-A  = ' TEST-CONTROL-A                    00187500
187600     ADD  1  TO  ACCT-MASTER-HI.                                  00187600
187700                                                                  00187700
187800     GO TO CK-FOR-CONLEY-ACCT-EXIT.                               00187800
187900                                                                  00187900
188000 CK-FOR-CONLEY.                                                   00188000
188100                                                                  00188100
188200     DISPLAY 'ACCT MATCH - CK FOR CONLEY AGT NUMBER ENTERED'.     00188200
188300                                                                  00188300
188400     MOVE ZEROS  TO  AGT-SUB.                                     00188400
188500                                                                  00188500
188600 CK-FOR-CONLEY-AGT-NO.                                            00188600
188700                                                                  00188700
188800     ADD 1  TO  AGT-SUB.                                          00188800
188900                                                                  00188900
189000     IF  AGT-SUB  GREATER THAN 10                                 00189000
189100       MOVE  'E'  TO  CIA-ACCT-SW                                 00189100
189200       DISPLAY ' '                                                00189200
189300       DISPLAY '* * * E R R O R * * * '                           00189300
189400       DISPLAY ' ACCT MATCHES ACCT MASTER BUT NOT A CONLEY ACCT'  00189400
189500       GO TO CK-FOR-CONLEY-ACCT-EXIT.                             00189500
189600                                                                  00189600
189700     IF  AM-AGT-PRIME (AGT-SUB) = '566100'                        00189700
189800       DISPLAY 'IT IS A CONLEY RCD - ACCT NO IS - ' AM-CONTROL-A  00189800
189900         MOVE  'Y'  TO  CIA-ACCT-SW                               00189900
190000         GO TO CK-FOR-CONLEY-ACCT-EXIT.                           00190000
190100                                                                  00190100
190200     GO TO CK-FOR-CONLEY-AGT-NO.                                  00190200
190300                                                                  00190300
190400 CK-FOR-CONLEY-ACCT-EXIT.                                         00190400
190500     EXIT.                                                        00190500
190600                                                                  00190600
190700                                                                  00190700
190800 GET-STATE.                                                       00190800
190900                                                                  00190900
191000     GO TO GET-STATE-EXIT.                                        00191000
191100                                                                  00191100
191200      MOVE CIA-STATE               TO WORK-ST.                    00191200
191300                                                                  00191300
191400      MOVE ST-ID (WORK-ST-N)       TO   PBI-STATE                 00191400
191500      GO                           TO GET-STATE-EXIT.             00191500
191600                                                                  00191600
191700 NO-STATE-CODE-FOUND.                                             00191700
191800                                                                  00191800
191900     MOVE SPACES                   TO RPT-PT-LINE.                00191900
192000     PERFORM 0200-RPT  THRU  0200-RPT-EXIT.                       00192000
192100     MOVE 'NO STATE CODE FOUND FOR THIS RECORD ' TO PT-ERROR.     00192100
192200     MOVE CIA-WORK                 TO  PT-REC.                    00192200
192300     PERFORM 0200-RPT  THRU  0200-RPT-EXIT.                       00192300
192400                                                                  00192400
192500 GET-STATE-EXIT.                                                  00192500
192600     EXIT.                                                        00192600
192700                                                                  00192700
192800* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00192800
192900* * * * * * * * *   END  ISSUE   PROCESSING   * * * * * * * * *   00192900
193000* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00193000
193100                                                                  00193100
193200 MAKE-BATCH-UNIQUE.                                               00193200
193300                                                                  00193300
193400     MOVE   CIA-BATCH-NO  TO  FIX-BATCH-NO.                       00193400
193500     MOVE   'CO'          TO  FIRST-2.                            00193500
193500*    MOVE   WORK-DA-IN    TO  SECOND-2.                           00193510
101007     MOVE   WORK-MO-IN    TO  SECOND-2.                           00193510
193600     MOVE   FIX-BATCH-NO  TO  CIA-BATCH-NO
112007     MOVE ZEROS TO CIA-BATCH-NO.
193700                                                                  00193700
193800 MAKE-BATCH-UNIQUE-EXIT.                                          00193800
193900                                                                  00193900
194000                                                                  00194000
194100 DO-JOINT-NAME.                                                   00194100

040414     perform varying ji-sub from +1 by +1 until
040414        (ji-sub > +12)
040414        or (cia-joint-name (ji-sub:1) = spaces)
040414     end-perform
040414     if ji-sub < +13
040414        move cia-joint-name (1:ji-sub - 1)
040414                                 to pbi-i-jnt-1st-name
040414        if ji-sub < +12
040414           move cia-joint-name (ji-sub + 1:12 - ji-sub)
040414                                 to pbi-i-jnt-last-name
040414        end-if
040414        if pbi-i-jnt-last-name = spaces
040414           move cia-name         to pbi-i-jnt-last-name
040414        end-if
040414     end-if
040414     display ' jnt nme stuff ' out-cert-no ' ' pbi-i-jnt-1st-name
040414        ' ' pbi-i-jnt-last-name
040414
040414     go to DO-JOINT-NAME-EXIT.

194300     MOVE ZEROS TO LN-SUB.                                        00194300
194400     MOVE 1     TO JI-SUB.                                        00194400
194500     MOVE CIA-JOINT-NAME TO PULL-JNT-1ST-INIT.                    00194500
194600     MOVE JNT-1ST-INIT (JI-SUB) TO  PBI-I-JNT-1ST-INIT.           00194600
194700                                                                  00194700
194800 DO-JOINT-NAME-LOOP.                                              00194800
194900                                                                  00194900
195000     ADD  1  TO  JI-SUB.                                          00195000
195100                                                                  00195100
195200     IF   JI-SUB  GREATER THAN  12                                00195200
195300          MOVE CIA-NAME TO  PBI-I-JNT-LAST-NAME                   00195300
195400          GO TO DO-JOINT-NAME-EXIT.                               00195400
195500                                                                  00195500
195600     IF   JNT-1ST-INIT (JI-SUB) = SPACES                          00195600
195700          GO TO CK-JOINT-LNAME                                    00195700
195800       ELSE                                                       00195800
195900          GO TO DO-JOINT-NAME-LOOP.                               00195900
196000                                                                  00196000
196100 CK-JOINT-LNAME.                                                  00196100
196200                                                                  00196200
196300     ADD  1  TO  JI-SUB.                                          00196300
196400                                                                  00196400
196500     IF   JI-SUB  GREATER THAN  12                                00196500
196600          MOVE CIA-NAME TO  PBI-I-JNT-LAST-NAME                   00196600
196700          GO TO DO-JOINT-NAME-EXIT.                               00196700
196800                                                                  00196800
196900     IF   JNT-1ST-INIT (JI-SUB) = SPACES                          00196900
197000          MOVE CIA-NAME TO  PBI-I-JNT-LAST-NAME                   00197000
197100          GO TO DO-JOINT-NAME-EXIT.                               00197100
197200                                                                  00197200
197300     MOVE SPACES TO SAVE-JOINT-LNAME.                             00197300
197400                                                                  00197400
197500     GO TO DO-JOINT-LNAME.                                        00197500
197600                                                                  00197600
197700 DO-JOINT-LNAME-LOOP.                                             00197700
197800                                                                  00197800
197900     ADD  1  TO  JI-SUB.                                          00197900
198000                                                                  00198000
198100     IF   JI-SUB  GREATER THAN  12                                00198100
198200          MOVE SAVE-JOINT-LNAME TO  PBI-I-JNT-LAST-NAME           00198200
198300          GO TO DO-JOINT-NAME-EXIT.                               00198300
198400                                                                  00198400
198500                                                                  00198500
198600 DO-JOINT-LNAME.                                                  00198600
198700                                                                  00198700
198800     ADD  1  TO  LN-SUB.                                          00198800
198900                                                                  00198900
199000     MOVE JNT-1ST-INIT (JI-SUB)                                   00199000
199100          TO  S-J-LNAME (LN-SUB).                                 00199100
199200                                                                  00199200
199300     GO TO DO-JOINT-LNAME-LOOP.                                   00199300
199400                                                                  00199400
199500 DO-JOINT-NAME-EXIT.                                              00199500
199600     EXIT.                                                        00199600
199700                                                                  00199700
199800                                                                  00199800
199900* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00199900
200000* * * * * * * * *  START  CANCEL  PROCESSING  * * * * * * * * *   00200000
200100* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00200100
200200                                                                  00200200
200300* =  =  =  =  =  =  ACARD  =  =  =  =  =  =                       00200300
200400                                                                  00200400
200500 CANCEL-BLD.                                                      00200500
200600                                                                  00200600
200700*    DISPLAY 'CANCEL-BLD   ENTERED '.                             00200700
200800                                                                  00200800
200900     MOVE   SPACES                 TO  PBI-RECORD-BODY.           00200900
201000                                                                  00201000
201100     MOVE   CIA-EFF-YR             TO  WORK-YR-X.                 00201100
201200     MOVE   CIA-EFF-MO             TO  WORK-MO-X.                 00201200
201300     MOVE   CIA-EFF-DA             TO  WORK-DA-X.                 00201300
201400     MOVE   WORK-DATE-N            TO  PBI-CERT-EFF-DT-X.         00201400
201500                                                                  00201500
201600     DISPLAY '  '                                                 00201600
201700     DISPLAY '2 - CIA-NEW-CANC-STATE  = ' CIA-NEW-CANC-STATE      00201700
201800     MOVE   CIA-NEW-CANC-STATE    TO SAVE-STATE.                  00201800
201900                                                                  00201900
030310     MOVE   ZEROS                  TO  SAVE-CERT01.               00202000
030310     MOVE   CIA-CERT-9             TO  SAVE-CERT09.               00202100
202200     MOVE   CIA-CERT-1             TO  SAVE-CERTSFX.              00202200
202300                                                                  00202300
202400     IF     CIA-CERT-3-N  NOT NUMERIC                             00202400
202500        DISPLAY ' SAVE-CERTSFX-N  NOT NUMERIC - CANCEL'           00202500
202600            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00202600
202700            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00202700
202800        ELSE                                                      00202800
202900*       DISPLAY ' SAVE-CERTSFX-N IS NUMERIC - CANCEL'             00202900
203000            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00203000
203100            MOVE   SPACES         TO  PBI-CERT-SFX.               00203100
203200                                                                  00203200
203300     INSPECT PBI-CERT-PRIME REPLACING ALL ' ' BY ZERO.            00203300
203400                                                                  00203400
203500     MOVE   SPACES                 TO  PBI-C-LIVES-X.             00203500
203600     MOVE   SPACES                 TO  PBI-C-FORCE-CD.            00203600
203700     MOVE   SPACES                 TO  PBI-C-INSURED-NAME.        00203700
203800                                                                  00203800
203900* *     FILL THE CANCEL LIFE FIELDS     * * * * * * * *           00203900
204000                                                                  00204000
204100     MOVE   CIA-CANC-YR             TO  WORK-YR-X.                00204100
204200     MOVE   CIA-CANC-MO             TO  WORK-MO-X.                00204200
204300     MOVE   CIA-CANC-DA             TO  WORK-DA-X.                00204300
204400     MOVE   WORK-DATE-N             TO  PBI-C-LF-CANCEL-DATE-X.   00204400
204500                                                                  00204500
204600     INSPECT CIA-CANC-LF-REFUND  REPLACING ALL  ' ' BY '0'.       00204600
204700     IF    CIA-CANC-LF-REFUND-N NOT NUMERIC                       00204700
204800        DISPLAY                                                   00204800
204900       'CIA-CANC-LF-REFUND-N NOT NUMERIC - ' CIA-CANC-LF-REFUND-N 00204900
205000             MOVE ZEROS             TO CIA-CANC-LF-REFUND-N.      00205000
205100     MOVE  ZEROS                    TO PBI-C-LF-PREM-REFUND.      00205100
205200     ADD   CIA-CANC-LF-REFUND-N     TO PBI-C-LF-PREM-REFUND.      00205200
205300                                                                  00205300
205400* *     FILL THE CANCEL A-H  FIELDS     * * * * * * * *           00205400
205500                                                                  00205500
205600     MOVE   CIA-CANC-YR             TO  WORK-YR-X.                00205600
205700     MOVE   CIA-CANC-MO             TO  WORK-MO-X.                00205700
205800     MOVE   CIA-CANC-DA             TO  WORK-DA-X.                00205800
205900     MOVE   WORK-DATE-N             TO  PBI-C-AH-CANCEL-DATE-X.   00205900
206000                                                                  00206000
206100     INSPECT CIA-CANC-AH-REFUND  REPLACING ALL  ' ' BY '0'.       00206100
206200     IF    CIA-CANC-AH-REFUND-N NOT NUMERIC                       00206200
206300        DISPLAY                                                   00206300
206400       'CIA-CANC-AH-REFUND-N NOT NUMERIC - ' CIA-CANC-AH-REFUND-N 00206400
206500           MOVE ZEROS               TO CIA-CANC-AH-REFUND-N.      00206500
206600     MOVE  ZEROS                    TO PBI-C-AH-PREM-REFUND.      00206600
206700     ADD   CIA-CANC-AH-REFUND-N     TO PBI-C-AH-PREM-REFUND.      00206700
206800                                                                  00206800
206900     MOVE '3'                       TO  PBI-C-TRANS-TYPE.         00206900
207000     MOVE '1'                       TO  PBI-C-SEQUENCE.           00207000
207100                                                                  00207100
207200 WRITE-CANCEL-REC.                                                00207200
207300                                                                  00207300
207400*    DISPLAY 'WRITE-CANCEL-REC  ENTERED '.                        00207400
207500     DISPLAY  OUT-RECORD.                                         00207500
207600                                                                  00207600
207700     MOVE   CIA-EFF-YR              TO  WORK-YR-X.                00207700
207800     MOVE   CIA-EFF-MO              TO  WORK-MO-X.                00207800
207900     MOVE   CIA-EFF-DA              TO  WORK-DA-X.                00207900
208000     MOVE   WORK-DATE-N             TO  PBI-CERT-EFF-DT-X.        00208000
208100                                                                  00208100
208200     IF     SAVE-CERTSFX-N NOT NUMERIC                            00208200
208300        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00208300
208400            MOVE   SAVE-CERT      TO  OUT-CERT-NO                 00208400
208500            MOVE   SAVE-CERTSFX   TO  OUT-CERT-SFX                00208500
208600        ELSE                                                      00208600
208700*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00208700
208800            MOVE   S-CERT10       TO  OUT-CERT-NO                 00208800
208900            MOVE   SPACES         TO  OUT-CERT-SFX.               00208900
209000                                                                  00209000
209100     INSPECT OUT-CERT-NO REPLACING ALL ' '  BY ZERO.              00209100
209200                                                                  00209200
209300     MOVE  WORK-DATE-N              TO  OUT-EFF-DATE              00209300
209400     MOVE  PBI-C-CANCEL-REC-SEQ-1   TO  OUT-INFO.                 00209400
209500     MOVE  '3'                      TO  OUT-TRAN-TYPE             00209500
209600     MOVE  '1'                      TO  OUT-SEQUENCE.             00209600
209700                                                                  00209700
209800     ADD  1                         TO  DETAIL-CNT.               00209800
209900*    MOVE  DETAIL-CNT               TO  SR-REC-CNT.               00209900
210000*    MOVE  CIA-BATCH-NO             TO  SR-BATCH-NO
210100                                                                  00210100
112007     MOVE OUT-CERT-NO             TO  SR-CERT-NO
           MOVE OUT-CERT-SFX            TO  SR-CERT-SFX
112007     MOVE ZEROS                   TO SR-ACCT-NO
112007     MOVE CIA-ACCT                TO SR-ACCT-NO (5:6)
210200     WRITE CARD-RECORD              FROM OUT-RECORD.              00210200
210300                                                                  00210300
210400     ADD   1                        TO CIA-CERT-CANC              00210400
210500                                       TOT-CERT-CANC.             00210500
210600     ADD   PBI-C-LF-PREM-REFUND     TO CIA-LIFE-CANC              00210600
210700                                       TOT-LIFE-CANC.             00210700
210800     ADD   PBI-C-AH-PREM-REFUND     TO CIA-AH-CANC                00210800
210900                                       TOT-AH-CANC.               00210900
211000                                                                  00211000
211100     ADD   1                        TO RPT-CIA-CNT                00211100
211200                                       RPT-LINE-CNT.              00211200
211300     IF    RPT-LINE-CNT GREATER THAN  60                          00211300
211400           PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.         00211400
211600     MOVE  OUT-RECORD               TO  RPT-PT-LINE.              00211600
211700     MOVE  'CANCEL '                TO  PT-ERROR.                 00211700
211800                                                                  00211800
211900     MOVE  RPT-PT-LINE              TO  RPT-REC.                  00211900
212000     WRITE RPT-REC-OUT AFTER ADVANCING 1.                         00212000
212100     ADD  1                         TO  RPT-LINE-CNT.             00212100
212300     MOVE  SPACES                   TO RPT-REC.                   00212300
212400     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00212400
212500     MOVE  SPACES                   TO RPT-PT-LINE.               00212500
212600                                                                  00212600
212700     GO  TO  010-READ-INPUT-FILE.                                 00212700
212800                                                                  00212800
212900 A-CARD-END.                                                      00212900
213000     EXIT.                                                        00213000
213100                                                                  00213100
213200* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00213200
213300* * * * * * * *   END  CANCELLATION PROCESSING  * * * * * * * *   00213300
213400* * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * *   00213400
213500                                                                  00213500
213600******* * * * * * * * * * * * * * * * * * * * * * * * * * * *     00213600
213700******* * * * * * * * * * * * * * * * * * * * * * * * * * * *     00213700
213800                                                                  00213800
213900 0150-WRITE-DATA-CARD.                                            00213900
214000                                                                  00214000
214100*    DISPLAY '0150-WRITE-DATA-CARD ENTERED '.                     00214100
214200                                                                  00214200
214300     MOVE ZEROS                     TO  SUB1.                     00214300
214400                                                                  00214400
214500     MOVE   CIA-EFF-YR              TO  PBI-CERT-EFF-YR.          00214500
214600     MOVE   CIA-EFF-MO              TO  PBI-CERT-EFF-MO.          00214600
214700     MOVE   CIA-EFF-DA              TO  PBI-CERT-EFF-DA.          00214700
214800                                                                  00214800
214900     IF     SAVE-CERTSFX-N NOT NUMERIC                            00214900
215000        DISPLAY ' SAVE-CERTSFX-N NOT NUMERIC - ISSUE'             00215000
215100            MOVE   SAVE-CERT      TO  PBI-CERT-PRIME              00215100
215200            MOVE   SAVE-CERTSFX   TO  PBI-CERT-SFX                00215200
215300        ELSE                                                      00215300
215400*       DISPLAY ' SAVE-CERTSFX-N IS  NUMERIC - ISSUE'             00215400
215500            MOVE   S-CERT10       TO  PBI-CERT-PRIME              00215500
215600            MOVE   SPACES         TO  PBI-CERT-SFX.               00215600
215700                                                                  00215700
215800                                                                  00215800
215900     DISPLAY  OUT-RECORD.                                         00215900
216000                                                                  00216000
216100     ADD  1                         TO  DETAIL-CNT.               00216100
216200*    MOVE  DETAIL-CNT               TO  SR-REC-CNT.               00216200
216300*    MOVE  CIA-BATCH-NO             TO  SR-BATCH-NO
216400     WRITE CARD-RECORD FROM OUT-RECORD.                           00216400
216500                                                                  00216500
216600     MOVE  ZERO                     TO PRINT-SUB.                 00216600
216700                                                                  00216700
216800 0150-RPT-PRINT.                                                  00216800
216900                                                                  00216900
217000*    DISPLAY '0150-RPT-PRINT  ENTERED '.                          00217000
217100                                                                  00217100
217200     ADD   1                        TO RPT-ISS-CNT.               00217200
217300     ADD   1                        TO RPT-LINE-CNT.              00217300
217400     IF   RPT-LINE-CNT GREATER THAN  60                           00217400
217500          PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.          00217500
217700     MOVE  OUT-RECORD               TO  RPT-PT-LINE.              00217700
217800     MOVE  SAVE-SEQ                 TO  PT-ERROR.                 00217800
217900     MOVE  RPT-PT-LINE              TO  RPT-REC.                  00217900
218000     WRITE RPT-REC-OUT AFTER ADVANCING 1.                         00218000
218100     ADD  1                         TO  RPT-LINE-CNT.             00218100
218300     MOVE  SPACES                   TO RPT-REC.                   00218300
218400     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00218400
218500     MOVE  SPACES                   TO RPT-PT-LINE.               00218500
218600                                                                  00218600
218700 0150-EXIT.                                                       00218700
218800     EXIT.                                                        00218800
218900                                                                  00218900
219000 0200-RPT.                                                        00219000
219100                                                                  00219100
219200     IF   RPT-LINE-CNT GREATER THAN  60                           00219200
219300          PERFORM   005-ERR-RPT-HD THRU ERR-RPT-HD-EXIT.          00219300
219500     MOVE  RPT-PT-LINE              TO  RPT-REC.                  00219500
219600     WRITE RPT-REC-OUT AFTER ADVANCING 1.                         00219600
219700     ADD  1                         TO  RPT-LINE-CNT.             00219700
219900     MOVE  SPACES                   TO RPT-REC.                   00219900
220000     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00220000
220100     ADD  1                         TO  RPT-LINE-CNT.             00220100
220200     MOVE  SPACES                   TO RPT-PT-LINE.               00220200
220300                                                                  00220300
220400 0200-RPT-EXIT.                                                   00220400
220500     EXIT.                                                        00220500

040414 8500-DATE-CONVERT.
040414
040414     CALL 'ELDATCX' USING DATE-CONVERSION-DATA.
040414
040414 8500-EXIT.
040414     EXIT.

220600                                                                  00220600
220700 9999-END-OF-JOB SECTION.                                         00220700
220800                                                                  00220800
220900     DISPLAY '9999-END-OF-JOB  ENTERED '.                         00220900
221000                                                                  00221000
221100     MOVE   '0'                     TO   CIA-SEQUENCE-NO.         00221100
221200*    MOVE   ZEROS                   TO   SAVE-ACCT.               00221200
221300     MOVE   SAVE-FULL-ACCT          TO   PBI-ACCOUNT.             00221300
221400     MOVE   SAVE-FULL-ACCT          TO   CIA-ACCT-NO.             00221400
221500     MOVE   SAVE-STATE              TO   PBI-STATE.               00221500
221600     MOVE   SAVE-STATE              TO   CIA-STATE.               00221600
221700     MOVE   '9'                     TO   PBI-CARRIER.             00221700
221800     MOVE   '000000'                TO   PBI-GROUPING.            00221800
221900     MOVE   CIA-HD-EFF-DT           TO   PBI-B-BATCH-DT-X.        00221900
222000     MOVE   CERT-CNT                TO   CIA-CERT-ISS             00222000
222100                                         PBI-B-CERT-ISS-COUNT.    00222100
222200     MOVE   CIA-LIFE-WRITTEN        TO   PBI-B-LF-PRM-WRITTEN.    00222200
222300     MOVE   CIA-AH-WRITTEN          TO   PBI-B-AH-PRM-WRITTEN.    00222300
222400     MOVE   CANC-CNT                TO   CIA-CERT-CANC            00222400
222500                                         PBI-B-CERT-CAN-COUNT.    00222500
222600     MOVE   CIA-LIFE-CANC           TO   PBI-B-LF-PRM-CANCELLED.  00222600
222700     MOVE   CIA-AH-CANC             TO   PBI-B-AH-PRM-CANCELLED.  00222700
222800     MOVE   'CSO'                   TO   PBI-CLIENT-ID.           00222800
222900     MOVE   '1'                     TO   PBI-B-TRANS-TYPE.        00222900
223000     MOVE   '0'                     TO   PBI-B-SEQUENCE.          00223000
223100                                                                  00223100
223200     DISPLAY 'CIA-BATCH-HDR  '.                                   00223200
223300     DISPLAY CIA-BATCH-HDR.                                       00223300
223400                                                                  00223400
223500     MOVE    CIA-BATCH-HDR          TO  OUT-RECORD.               00223500
112007     MOVE SAVE-FULL-ACCT            TO SR-ACCT-NO
112007     MOVE ZEROS                   TO  SR-CERT-NO
           MOVE ZEROS                   TO  SR-CERT-SFX
223600*    MOVE    CIA-BATCH-NO           TO   SR-BATCH-NO
223700*    MOVE    ZEROS                  TO   SR-REC-CNT.              00223700
223800                                                                  00223800
223900     WRITE CARD-RECORD FROM OUT-RECORD.                           00223900
224000                                                                  00224000
224100     MOVE  RPT-CIA-CNT              TO P-CIA-CNT                  00224100
224200     MOVE  RPT-ISS-CNT              TO P-ISS-CNT                  00224200
224300     ADD  1                         TO RPT-PAGE-CNT               00224300
224400     MOVE   RPT-PAGE-CNT            TO RPT-PAGE                   00224400
224500     MOVE    4                      TO RPT-LINE-CNT               00224500
224700     CLOSE CIA-LIFE-IN.                                           00224700
224800                                                                  00224800
224900     ADD TOT-CERT-ISS                                             00224900
225000         TOT-CERT-CANC                                            00225000
225100             GIVING TOT-DETAIL-CNT.                               00225100
225200                                                                  00225200
225300     DISPLAY '************************************************'   00225300
225400     DISPLAY 'INPUT RECORDS      -- '    IN-CNT.                  00225400
225500     DISPLAY ' '                                                  00225500
225600     DISPLAY 'DETL WRITTEN COUNT --   '  TOT-DETAIL-CNT.          00225600
225700     DISPLAY ' '                                                  00225700
225800     DISPLAY 'HDRS WRITTEN COUNT --    ' HDR-CNT.                 00225800
225900     DISPLAY ' '                                                  00225900
226000     DISPLAY 'ERROR RECORD COUNT --    ' RPT-CNT.                 00226000
226100     DISPLAY ' '                                                  00226100
226200     DISPLAY '  ACCT EQUAL COUNT --    ' ACCT-MATCH.              00226200
226300     DISPLAY ' '                                                  00226300
226400     DISPLAY '    ACCT LOW COUNT --    ' ACCT-MASTER-LOW.         00226400
226500     DISPLAY ' '                                                  00226500
226600     DISPLAY '     ACCT HI COUNT --    ' ACCT-MASTER-HI.          00226600
226700     DISPLAY ' '                                                  00226700
226800     DISPLAY '************************************************'.  00226800
226900                                                                  00226900
227000     MOVE  RPT-PAGE-CNT             TO RPT-PAGE.                  00227000
227100     MOVE  4                        TO RPT-LINE-CNT.              00227100
227300     MOVE  'CREDIT INS. ASSOC. INPUT TOTALS FOR '  TO CIA-RPT-ID. 00227300
227400     MOVE  RPT-HD-LINE1             TO RPT-REC.                   00227400
227500     WRITE RPT-REC-OUT  AFTER ADVANCING PAGE.                     00227500
227700     MOVE  SPACES                   TO RPT-REC.                   00227700
227800     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00227800
228000     MOVE  REPORT-HD-LINE           TO RPT-REC.                   00228000
228100     WRITE RPT-REC-OUT AFTER ADVANCING 1.                         00228100
228200     ADD  1                         TO  RPT-LINE-CNT.             00228200
228400     MOVE  SPACES                   TO RPT-REC.                   00228400
228500     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00228500
228600                                                                  00228600
228700     MOVE  TOT-LIFE-WRITTEN         TO T-LIFE-WRITTEN.            00228700
228800     MOVE  TOT-LIFE-CANC            TO T-LIFE-CANC.               00228800
228900     MOVE  TOT-AH-WRITTEN           TO T-AH-WRITTEN.              00228900
229000     MOVE  TOT-AH-CANC              TO T-AH-CANC.                 00229000
229100     MOVE  TOT-CERT-ISS             TO T-CERT-ISS.                00229100
229200     MOVE  TOT-CERT-CANC            TO T-CERT-CANC.               00229200
229300                                                                  00229300
229500     MOVE  TOTAL-LINE               TO RPT-REC.                   00229500
229600     WRITE RPT-REC-OUT AFTER ADVANCING 1.                         00229600
229700     ADD  1                         TO  RPT-LINE-CNT.             00229700
229900     MOVE  SPACES                   TO RPT-REC.                   00229900
230000     WRITE RPT-REC-OUT  AFTER ADVANCING 1.                        00230000
230100                                                                  00230100
230200     MOVE ZEROS   TO  ST-SUB.                                     00230200
230300     DISPLAY '   '.                                               00230300
230400     DISPLAY 'START STATE CODE PRINT '.                           00230400
230500                                                                  00230500
230600 PRINT-STATES-LOOP.                                               00230600
230700                                                                  00230700
230800     ADD  1       TO  ST-SUB.                                     00230800
230900                                                                  00230900
231000     IF   ST-TABLE (ST-SUB)  =  SPACES                            00231000
231100        DISPLAY '   '                                             00231100
231200        DISPLAY 'END OF STATE CODES '                             00231200
231300             GO  TO  PRINT-STATES-EXIT.                           00231300
231400                                                                  00231400
231500     IF   ST-SUB  GREATER THAN 80                                 00231500
231600             GO  TO  PRINT-STATES-EXIT.                           00231600
231700                                                                  00231700
231800      MOVE ZEROS TO ST-ID-SUB.                                    00231800
231900      MOVE ' NEW STATE ADDED ' TO NEW-ST-LABEL.                   00231900
232000                                                                  00232000
232100 CK-FOR-NEW-STATE-LOOP.                                           00232100
232200                                                                  00232200
232300      ADD  1     TO ST-ID-SUB.                                    00232300
232400                                                                  00232400
232500      IF   ST-ID-SUB  GREATER THAN 80                             00232500
232600           GO  TO  CK-FOR-NEW-EXIT.                               00232600
232700                                                                  00232700
232800      IF   SAVE-ST-ID (ST-ID-SUB)  EQUAL  SPACES                  00232800
232900           GO  TO  CK-FOR-NEW-EXIT.                               00232900
233000                                                                  00233000
233100      IF   ST-TABLE (ST-SUB)  EQUAL  SAVE-ST-ID (ST-ID-SUB)       00233100
233200           MOVE SPACES TO NEW-ST-LABEL                            00233200
233300             GO  TO  CK-FOR-NEW-EXIT.                             00233300
233400                                                                  00233400
233500      GO  TO  CK-FOR-NEW-STATE-LOOP.                              00233500
233600                                                                  00233600
233700 CK-FOR-NEW-EXIT.                                                 00233700
233800                                                                  00233800
233900     DISPLAY '   '.                                               00233900
234000     MOVE                                                         00234000
234100      'STATE CODE FROM CONLEY INPUT RECORD = ' TO  PT-MSG.        00234100
234200                                                                  00234200
234300     MOVE                                                         00234300
234400      ST-TABLE (ST-SUB) TO  PT-STATE.                             00234400
234500                                                                  00234500
234600     DISPLAY '   '.                                               00234600
234700     DISPLAY ST-PRINT-LINE.                                       00234700
234800                                                                  00234800
234900     GO  TO  PRINT-STATES-LOOP.                                   00234900
235000                                                                  00235000
235100                                                                  00235100
235200 PRINT-STATES-EXIT.                                               00235200
235300                                                                  00235300
235400                                                                  00235400
235500     CLOSE RPT-FILE.                                              00235500
235600     CLOSE CIA-LIFE-OUT.                                          00235600
235700     CLOSE ERACCT.                                                00235700
235800                                                                  00235800
235900     GOBACK.                                                      00235900
236000                                                                  00236000
236100                                                                  00236100
