
      *================================================================*
      * PROGRAM      : TBSBKGCORE                                       *
      * TITLE        : Master Booking Execution Core                   *
      * SYSTEM       : Travel Booking System (TBS)  -  IBM i / ILE     *
      * TYPE         : Callable transaction engine (no workstation I/O)*
      *----------------------------------------------------------------*
      * FILES                                                          *
      *   TBSBOOKG  (PF, keyed BOOKID)   - I-O  : add / rewrite / read *
      *   TBSBKGSCN (= TBSBOOKG 2nd ODP) - INPUT: sequential seat scan *
      *   TBSFLGHT  (PF, keyed FLIGHTNO) - INPUT: inventory + capacity *
      *   TBSPASSG  (PF, keyed PASSID)   - INPUT: passenger integrity  *
      *   TBSPAYLOG (PF, keyed TRANID)   - I-O  : financial ledger add *
      *----------------------------------------------------------------*
      * BUILD                                                          *
      *   CRTPF     FILE(TBSDTA/TBSBOOKG)  SRCFILE(TBSDTA/QDDSSRC)     *
      *   CRTPF     FILE(TBSDTA/TBSFLGHT)  SRCFILE(TBSDTA/QDDSSRC)     *
      *   CRTPF     FILE(TBSDTA/TBSPASSG)  SRCFILE(TBSDTA/QDDSSRC)     *
      *   CRTPF     FILE(TBSDTA/TBSPAYLOG) SRCFILE(TBSDTA/QDDSSRC)     *
      *   CRTBNDCBL PGM(TBSDTA/TBSBKGCORE) SRCFILE(TBSDTA/QCBLLESRC)   *
      *----------------------------------------------------------------*
      * CALL INTERFACE                                                 *
      *   CALL "TBSBKGCORE" USING TBS-BKG-REQUEST                      *
      *                           TBS-BKG-RESPONSE.                    *
      *----------------------------------------------------------------*
      * DESIGN NOTES                                                   *
      *   * The core owns the full reservation lifecycle as an         *
      *     explicit state machine.  Booking status (PAYSTATUS) moves  *
      *     only along transitions defined in WS-TRANS-TABLE; every    *
      *     other transition is rejected as RC 16.                     *
      *         H Held -> C Confirmed -> P Paid                        *
      *         H/C/P  -> X Cancelled (with refund rules at Pay state) *
      *   * Seat LOCKOUT is held in an in-core lock matrix keyed by    *
      *     flight + travel date + seat.  The matrix survives across   *
      *     CALLs (module is NOT declared INITIAL) so it behaves like  *
      *     a soft-hold reservation cache with explicit lock states:   *
      *         FREE -> HELD -> CONFIRMED -> TICKETED -> RELEASED      *
      *   * Relational integrity is enforced before any write:         *
      *         flight active, passenger active, class valid,          *
      *         cabin capacity (DB sold + in-core held) not exceeded,  *
      *         seat not double-allocated.                             *
      *   * Surrogate keys (BOOKID, TRANID) are application assigned   *
      *     by positioning to the high key and incrementing, because   *
      *     DDS PFs cannot generate IDENTITY columns.                  *
      *   * Every state change is logged in real time to WS-EVENT-LOG  *
      *     and surfaced to the caller through the response area.      *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TBSBKGCORE.
       AUTHOR.        TBS-DEV-TEAM.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
      *    -- primary booking master : add / read / rewrite by key
           SELECT BOOKING-FILE
               ASSIGN TO DATABASE-TBSBOOKG
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKID OF BOOKING-REC
               FILE STATUS IS WS-BKG-STATUS.
      *
      *    -- second open data path over the same physical file, used
      *       purely to scan sold seats without disturbing the keyed
      *       position of the primary BOOKING-FILE ODP.
           SELECT SCAN-FILE
               ASSIGN TO DATABASE-TBSBOOKG
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS BOOKID OF SCAN-REC
               FILE STATUS IS WS-SCN-STATUS.
      *
           SELECT FLIGHT-FILE
               ASSIGN TO DATABASE-TBSFLGHT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FLIGHTNO OF FLIGHT-REC
               FILE STATUS IS WS-FLT-STATUS.
      *
           SELECT PASSENGER-FILE
               ASSIGN TO DATABASE-TBSPASSG
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS PASSID OF PASSENGER-REC
               FILE STATUS IS WS-PAS-STATUS.
      *
           SELECT PAYLOG-FILE
               ASSIGN TO DATABASE-TBSPAYLOG
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS TRANID OF PAYLOG-REC
               FILE STATUS IS WS-PAY-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  BOOKING-FILE.
       01  BOOKING-REC.
           COPY DDS-ALL-FORMATS OF TBSBOOKG.
      *
       FD  SCAN-FILE.
       01  SCAN-REC.
           COPY DDS-ALL-FORMATS OF TBSBOOKG.
      *
       FD  FLIGHT-FILE.
       01  FLIGHT-REC.
           COPY DDS-ALL-FORMATS OF TBSFLGHT.
      *
       FD  PASSENGER-FILE.
       01  PASSENGER-REC.
           COPY DDS-ALL-FORMATS OF TBSPASSG.
      *
       FD  PAYLOG-FILE.
       01  PAYLOG-REC.
           COPY DDS-ALL-FORMATS OF TBSPAYLOG.
      *
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * FILE STATUS FIELDS                                             *
      *----------------------------------------------------------------*
       01  WS-BKG-STATUS            PIC X(02)  VALUE "00".
           88  BKG-OK                          VALUE "00".
           88  BKG-NOTFOUND                    VALUE "23".
           88  BKG-EOF                         VALUE "10".
           88  BKG-DUPKEY                      VALUE "22".
       01  WS-SCN-STATUS            PIC X(02)  VALUE "00".
           88  SCN-OK                          VALUE "00".
           88  SCN-EOF                         VALUE "10".
       01  WS-FLT-STATUS            PIC X(02)  VALUE "00".
           88  FLT-OK                          VALUE "00".
           88  FLT-NOTFOUND                    VALUE "23".
       01  WS-PAS-STATUS            PIC X(02)  VALUE "00".
           88  PAS-OK                          VALUE "00".
           88  PAS-NOTFOUND                    VALUE "23".
       01  WS-PAY-STATUS            PIC X(02)  VALUE "00".
           88  PAY-OK                          VALUE "00".
           88  PAY-DUPKEY                      VALUE "22".
      *
      *----------------------------------------------------------------*
      * OPEN / CONTROL FLAGS                                           *
      *----------------------------------------------------------------*
       01  WS-FLAGS.
           05  WS-OPEN-FLAG        PIC X(01)  VALUE "N".
               88  FILES-OPEN                 VALUE "Y".
           05  WS-FOUND-FLAG       PIC X(01)  VALUE "N".
               88  REC-FOUND                  VALUE "Y".
           05  WS-LOCK-FOUND       PIC X(01)  VALUE "N".
               88  LOCK-HIT                   VALUE "Y".
           05  WS-CLASS-FOUND      PIC X(01)  VALUE "N".
               88  CLASS-HIT                  VALUE "Y".
           05  WS-TRANS-FOUND      PIC X(01)  VALUE "N".
               88  TRANS-ALLOWED              VALUE "Y".
           05  WS-INTEG-FLAG       PIC X(01)  VALUE "Y".
               88  INTEG-OK                   VALUE "Y".
               88  INTEG-BAD                  VALUE "N".
      *
      *----------------------------------------------------------------*
      * BOOKING STATUS / LIFECYCLE CODES (held in PAYSTATUS, 1A)       *
      *----------------------------------------------------------------*
       01  WS-STATUS-CODES.
           05  ST-HELD            PIC X(01)  VALUE "H".
           05  ST-CONFIRMED       PIC X(01)  VALUE "C".
           05  ST-PAID            PIC X(01)  VALUE "P".
           05  ST-CANCELLED       PIC X(01)  VALUE "X".
           05  ST-FAILED          PIC X(01)  VALUE "F".
      *
      *----------------------------------------------------------------*
      * EXPLICIT STATE-TRANSITION TABLE                               *
      *   each entry = FROM(1) TO(1) ALLOW(1)                          *
      *   any (from,to) pair NOT present here is rejected RC 16.       *
      *----------------------------------------------------------------*
       01  WS-TRANS-DEF.
           05  FILLER PIC X(03) VALUE "HCY".
           05  FILLER PIC X(03) VALUE "HXY".
           05  FILLER PIC X(03) VALUE "HFY".
           05  FILLER PIC X(03) VALUE "CPY".
           05  FILLER PIC X(03) VALUE "CXY".
           05  FILLER PIC X(03) VALUE "CFY".
           05  FILLER PIC X(03) VALUE "PXY".
       01  WS-TRANS-TABLE REDEFINES WS-TRANS-DEF.
           05  WS-TRANS-ENT       OCCURS 7 TIMES
                                  INDEXED BY TRX.
               10  WS-TR-FROM     PIC X(01).
               10  WS-TR-TO       PIC X(01).
               10  WS-TR-ALLOW    PIC X(01).
      *
      *----------------------------------------------------------------*
      * BOOKING-CLASS -> CABIN + FARE MULTIPLIER TABLE                 *
      *   CODE(1)  CABIN(1: F/B/P/E)  MULT x1000 (4)                   *
      *     F First    -> F cabin  3.000                               *
      *     C Business -> B cabin  2.000                               *
      *     P Premium  -> P cabin  1.500                               *
      *     Y Economy  -> E cabin  1.000                               *
      *     Z Discount -> E cabin  0.750                               *
      *----------------------------------------------------------------*
       01  WS-CLASS-DEF.
           05  FILLER PIC X(06) VALUE "FF3000".
           05  FILLER PIC X(06) VALUE "CB2000".
           05  FILLER PIC X(06) VALUE "PP1500".
           05  FILLER PIC X(06) VALUE "YE1000".
           05  FILLER PIC X(06) VALUE "ZE0750".
       01  WS-CLASS-TABLE REDEFINES WS-CLASS-DEF.
           05  WS-CLASS-ENT       OCCURS 5 TIMES
                                  INDEXED BY CLX.
               10  WS-CL-CODE     PIC X(01).
               10  WS-CL-CABIN    PIC X(01).
               10  WS-CL-MULT-N   PIC 9(04).
      *
      *----------------------------------------------------------------*
      * IN-CORE SEAT LOCK MATRIX (soft-hold cache, survives CALLs)     *
      *   lock states : F free (slot reusable) H held                  *
      *                 C confirmed  T ticketed  R released            *
      *----------------------------------------------------------------*
       01  WS-LOCK-CONTROL.
           05  WS-LOCK-COUNT      PIC S9(05) VALUE ZERO.
           05  WS-LOCK-MAX        PIC S9(05) VALUE 2000.
       01  WS-LOCK-AREA.
           05  WS-LOCK-ROW        OCCURS 2000 TIMES
                                  INDEXED BY LKX.
               10  WS-LK-FLIGHT   PIC X(06)  VALUE SPACE.
               10  WS-LK-DATE     PIC X(10)  VALUE SPACE.
               10  WS-LK-SEAT     PIC X(04)  VALUE SPACE.
               10  WS-LK-CABIN    PIC X(01)  VALUE SPACE.
               10  WS-LK-PASSID   PIC S9(09) VALUE ZERO.
               10  WS-LK-BOOKID   PIC S9(09) VALUE ZERO.
               10  WS-LK-STATE    PIC X(01)  VALUE "F".
               10  WS-LK-TStamp   PIC X(26)  VALUE SPACE.
      *
      *----------------------------------------------------------------*
      * CABIN CAPACITY SNAPSHOT (loaded per request from TBSFLGHT)     *
      *----------------------------------------------------------------*
       01  WS-CABIN-CAP.
           05  WS-CAP-F           PIC S9(05) VALUE ZERO.
           05  WS-CAP-B           PIC S9(05) VALUE ZERO.
           05  WS-CAP-P           PIC S9(05) VALUE ZERO.
           05  WS-CAP-E           PIC S9(05) VALUE ZERO.
       01  WS-CABIN-USED.
           05  WS-USE-F           PIC S9(05) VALUE ZERO.
           05  WS-USE-B           PIC S9(05) VALUE ZERO.
           05  WS-USE-P           PIC S9(05) VALUE ZERO.
           05  WS-USE-E           PIC S9(05) VALUE ZERO.
      *
      *----------------------------------------------------------------*
      * PER-REQUEST WORK FIELDS                                        *
      *----------------------------------------------------------------*
       01  WS-WORK.
           05  WS-REQ-CABIN       PIC X(01)  VALUE SPACE.
           05  WS-FARE-MULT       PIC S9(01)V999 VALUE ZERO.
           05  WS-CALC-FARE       PIC S9(09)V99  VALUE ZERO.
           05  WS-SOLD-COUNT      PIC S9(05) VALUE ZERO.
           05  WS-HELD-COUNT      PIC S9(05) VALUE ZERO.
           05  WS-CUR-STATE       PIC X(01)  VALUE SPACE.
           05  WS-NEW-STATE       PIC X(01)  VALUE SPACE.
           05  WS-NEXT-BOOKID     PIC S9(09) VALUE ZERO.
           05  WS-NEXT-TRANID     PIC S9(09) VALUE ZERO.
           05  WS-HIGH-KEY        PIC S9(09) VALUE 999999999.
           05  WS-TS-CHAR         PIC X(26)  VALUE SPACE.
           05  WS-LOOKUP-CLASS-SCAN PIC X(01) VALUE SPACE.
           05  WS-SCAN-CABIN      PIC X(01)  VALUE SPACE.
      *
       01  WS-CURRENT-DATE.
           05  WS-CD-YYYY         PIC 9(04).
           05  WS-CD-MM           PIC 9(02).
           05  WS-CD-DD           PIC 9(02).
           05  WS-CD-HH           PIC 9(02).
           05  WS-CD-MI           PIC 9(02).
           05  WS-CD-SS           PIC 9(02).
           05  WS-CD-HS           PIC 9(02).
      *
      *----------------------------------------------------------------*
      * REAL-TIME EVENT LOG (last events this activation)             *
      *----------------------------------------------------------------*
       01  WS-EVENT-CONTROL.
           05  WS-EVT-COUNT       PIC S9(05) VALUE ZERO.
           05  WS-EVT-MAX         PIC S9(05) VALUE 500.
       01  WS-EVENT-AREA.
           05  WS-EVT-ROW         OCCURS 500 TIMES
                                  INDEXED BY EVX.
               10  WS-EV-TStamp   PIC X(26).
               10  WS-EV-BOOKID   PIC S9(09).
               10  WS-EV-FROM     PIC X(01).
               10  WS-EV-TO       PIC X(01).
               10  WS-EV-FUNC     PIC X(01).
               10  WS-EV-RC       PIC 9(02).
      *
      *----------------------------------------------------------------*
      * MESSAGE TEXT CONSTANTS                                         *
      *----------------------------------------------------------------*
       01  WS-MSG.
           05  M-HOLD-OK       PIC X(60) VALUE
               "Seat held - booking created in HELD state.".
           05  M-CONFIRM-OK    PIC X(60) VALUE
               "Booking confirmed - seat allocation locked.".
           05  M-PAY-OK        PIC X(60) VALUE
               "Payment captured - booking ticketed.".
           05  M-CANCEL-OK     PIC X(60) VALUE
               "Booking cancelled - seat lock released.".
           05  M-FLT-NF        PIC X(60) VALUE
               "Flight not found in inventory master.".
           05  M-FLT-INACT     PIC X(60) VALUE
               "Flight is not open for sale (status).".
           05  M-PAS-NF        PIC X(60) VALUE
               "Passenger profile not found.".
           05  M-PAS-INACT     PIC X(60) VALUE
               "Passenger profile is not active.".
           05  M-CLASS-BAD     PIC X(60) VALUE
               "Booking class invalid (F/C/P/Y/Z).".
           05  M-CAP-FULL      PIC X(60) VALUE
               "Cabin capacity exhausted for class.".
           05  M-SEAT-TAKEN    PIC X(60) VALUE
               "Requested seat already locked / sold.".
           05  M-BKG-NF        PIC X(60) VALUE
               "Booking id not found.".
           05  M-TRANS-BAD     PIC X(60) VALUE
               "Invalid lifecycle transition requested.".
           05  M-FUNC-BAD      PIC X(60) VALUE
               "Unknown request function code.".
           05  M-QUERY-OK      PIC X(60) VALUE
               "Booking status returned.".
      *
      *================================================================*
      * LINKAGE - CALLER COMMUNICATION AREA                            *
      *================================================================*
       LINKAGE SECTION.
       01  TBS-BKG-REQUEST.
           05  LK-REQ-FUNCTION    PIC X(01).
      *        H=Hold C=Confirm P=Pay X=Cancel Q=Query
           05  LK-REQ-FLIGHTNO    PIC X(06).
           05  LK-REQ-PASSID      PIC S9(09).
           05  LK-REQ-BKCLASS     PIC X(01).
           05  LK-REQ-SEATNO      PIC X(04).
           05  LK-REQ-TRAVELDATE  PIC X(10).
           05  LK-REQ-BOOKID      PIC S9(09).
           05  LK-REQ-PAYMETHOD   PIC X(04).
           05  LK-REQ-CURRENCY    PIC X(03).
           05  LK-REQ-APPRCODE    PIC X(06).
      *
       01  TBS-BKG-RESPONSE.
           05  LK-RSP-RC          PIC 9(02).
      *        00 ok 04 warn 08 not-found 12 integrity
      *        16 bad-transition 20 capacity 24 io-error 28 bad-func
           05  LK-RSP-STATE       PIC X(01).
           05  LK-RSP-BOOKID      PIC S9(09).
           05  LK-RSP-TRANID      PIC S9(09).
           05  LK-RSP-FARE        PIC S9(09)V99.
           05  LK-RSP-MSGTEXT     PIC X(60).
      *
      *================================================================*
       PROCEDURE DIVISION USING TBS-BKG-REQUEST
                                TBS-BKG-RESPONSE.
      *================================================================*
       0000-MAINLINE.
           PERFORM 0100-INIT-RESPONSE
           PERFORM 0200-OPEN-FILES
           PERFORM 1000-DISPATCH
           PERFORM 9000-CLOSE-FILES
           GOBACK.
      *
       0100-INIT-RESPONSE.
           MOVE 00       TO LK-RSP-RC
           MOVE SPACE    TO LK-RSP-STATE
           MOVE ZERO     TO LK-RSP-BOOKID
           MOVE ZERO     TO LK-RSP-TRANID
           MOVE ZERO     TO LK-RSP-FARE
           MOVE SPACE    TO LK-RSP-MSGTEXT
           MOVE "Y"      TO WS-INTEG-FLAG.
      *
       0200-OPEN-FILES.
           IF NOT FILES-OPEN
               OPEN I-O    BOOKING-FILE
               OPEN INPUT  SCAN-FILE
               OPEN INPUT  FLIGHT-FILE
               OPEN INPUT  PASSENGER-FILE
               OPEN I-O    PAYLOG-FILE
               MOVE "Y" TO WS-OPEN-FLAG
           END-IF.
      *
       9000-CLOSE-FILES.
           IF FILES-OPEN
               CLOSE BOOKING-FILE
               CLOSE SCAN-FILE
               CLOSE FLIGHT-FILE
               CLOSE PASSENGER-FILE
               CLOSE PAYLOG-FILE
               MOVE "N" TO WS-OPEN-FLAG
           END-IF.
      *
      *----------------------------------------------------------------*
      * FUNCTION DISPATCHER                                            *
      *----------------------------------------------------------------*
       1000-DISPATCH.
           PERFORM 8900-GET-TIMESTAMP
           EVALUATE LK-REQ-FUNCTION
               WHEN "H"   PERFORM 2000-HOLD-BOOKING
               WHEN "C"   PERFORM 3000-CONFIRM-BOOKING
               WHEN "P"   PERFORM 4000-PAY-BOOKING
               WHEN "X"   PERFORM 5000-CANCEL-BOOKING
               WHEN "Q"   PERFORM 6000-QUERY-BOOKING
               WHEN OTHER
                   MOVE 28        TO LK-RSP-RC
                   MOVE M-FUNC-BAD TO LK-RSP-MSGTEXT
           END-EVALUATE.
      *
      *================================================================*
      * HOLD : create a new booking in HELD state with a seat lock     *
      *================================================================*
       2000-HOLD-BOOKING.
           PERFORM 2100-VALIDATE-INVENTORY
           IF INTEG-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2200-LOAD-CABIN-CAPACITY
           PERFORM 2300-COUNT-CABIN-USAGE
           PERFORM 2400-CHECK-CAPACITY
           IF INTEG-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2500-CHECK-SEAT-FREE
           IF INTEG-BAD
               GO TO 2000-EXIT
           END-IF
           PERFORM 2600-COMPUTE-FARE
           PERFORM 7100-NEXT-BOOKID
           PERFORM 2700-WRITE-HELD-BOOKING
           PERFORM 2800-INSERT-SEAT-LOCK
           MOVE ST-HELD     TO LK-RSP-STATE
           MOVE WS-NEXT-BOOKID TO LK-RSP-BOOKID
           MOVE WS-CALC-FARE   TO LK-RSP-FARE
           MOVE M-HOLD-OK   TO LK-RSP-MSGTEXT
           MOVE 00          TO LK-RSP-RC
           PERFORM 8800-LOG-EVENT.
       2000-EXIT.
           EXIT.
      *
      *  -- flight + passenger + class integrity for a new hold        *
       2100-VALIDATE-INVENTORY.
           MOVE "Y" TO WS-INTEG-FLAG
      *    -- flight must exist and be open for sale (A active S sched)
           MOVE LK-REQ-FLIGHTNO TO FLIGHTNO OF FLIGHT-REC
           PERFORM 7300-READ-FLIGHT
           IF NOT REC-FOUND
               MOVE "N"     TO WS-INTEG-FLAG
               MOVE 08      TO LK-RSP-RC
               MOVE M-FLT-NF TO LK-RSP-MSGTEXT
               GO TO 2100-EXIT
           END-IF
           IF FLSTATUS = "A" OR FLSTATUS = "S"
               CONTINUE
           ELSE
               MOVE "N"        TO WS-INTEG-FLAG
               MOVE 12         TO LK-RSP-RC
               MOVE M-FLT-INACT TO LK-RSP-MSGTEXT
               GO TO 2100-EXIT
           END-IF
      *    -- passenger must exist and be active
           MOVE LK-REQ-PASSID TO PASSID OF PASSENGER-REC
           PERFORM 7400-READ-PASSENGER
           IF NOT REC-FOUND
               MOVE "N"     TO WS-INTEG-FLAG
               MOVE 08      TO LK-RSP-RC
               MOVE M-PAS-NF TO LK-RSP-MSGTEXT
               GO TO 2100-EXIT
           END-IF
           IF PASTATUS NOT = "A"
               MOVE "N"        TO WS-INTEG-FLAG
               MOVE 12         TO LK-RSP-RC
               MOVE M-PAS-INACT TO LK-RSP-MSGTEXT
               GO TO 2100-EXIT
           END-IF
      *    -- booking class must resolve to a cabin
           PERFORM 8100-LOOKUP-CLASS
           IF NOT CLASS-HIT
               MOVE "N"        TO WS-INTEG-FLAG
               MOVE 12         TO LK-RSP-RC
               MOVE M-CLASS-BAD TO LK-RSP-MSGTEXT
           END-IF.
       2100-EXIT.
           EXIT.
      *
      *  -- snapshot per-cabin capacity from the flight record         *
       2200-LOAD-CABIN-CAPACITY.
           MOVE FSTSEATS TO WS-CAP-F
           MOVE BUSSEATS TO WS-CAP-B
           MOVE PRMSEATS TO WS-CAP-P
           MOVE ECOSEATS TO WS-CAP-E.
      *
      *  -- combined usage = seats already sold in DB + in-core holds  *
       2300-COUNT-CABIN-USAGE.
           MOVE ZERO TO WS-USE-F WS-USE-B WS-USE-P WS-USE-E
           PERFORM 7500-SCAN-SOLD-SEATS
           PERFORM 2350-ADD-INCORE-HOLDS.
      *
      *  -- fold active in-core locks into the usage tally             *
       2350-ADD-INCORE-HOLDS.
           IF WS-LOCK-COUNT > 0
               PERFORM VARYING LKX FROM 1 BY 1
                       UNTIL LKX > WS-LOCK-COUNT
                   IF WS-LK-FLIGHT (LKX) = LK-REQ-FLIGHTNO
                     AND WS-LK-DATE (LKX) = LK-REQ-TRAVELDATE
                     AND (WS-LK-STATE (LKX) = "H"
                       OR WS-LK-STATE (LKX) = "C"
                       OR WS-LK-STATE (LKX) = "T")
                       EVALUATE WS-LK-CABIN (LKX)
                           WHEN "F" ADD 1 TO WS-USE-F
                           WHEN "B" ADD 1 TO WS-USE-B
                           WHEN "P" ADD 1 TO WS-USE-P
                           WHEN "E" ADD 1 TO WS-USE-E
                       END-EVALUATE
                   END-IF
               END-PERFORM
           END-IF.
      *
      *  -- reject if the target cabin is at or over capacity          *
       2400-CHECK-CAPACITY.
           EVALUATE WS-REQ-CABIN
               WHEN "F"
                   IF WS-USE-F >= WS-CAP-F
                       PERFORM 2450-CAP-FULL
                   END-IF
               WHEN "B"
                   IF WS-USE-B >= WS-CAP-B
                       PERFORM 2450-CAP-FULL
                   END-IF
               WHEN "P"
                   IF WS-USE-P >= WS-CAP-P
                       PERFORM 2450-CAP-FULL
                   END-IF
               WHEN "E"
                   IF WS-USE-E >= WS-CAP-E
                       PERFORM 2450-CAP-FULL
                   END-IF
           END-EVALUATE.
      *
       2450-CAP-FULL.
           MOVE "N"       TO WS-INTEG-FLAG
           MOVE 20        TO LK-RSP-RC
           MOVE M-CAP-FULL TO LK-RSP-MSGTEXT.
      *
      *  -- the specific seat must not be locked or already sold       *
       2500-CHECK-SEAT-FREE.
           PERFORM 8200-FIND-SEAT-LOCK
           IF LOCK-HIT
               MOVE "N"        TO WS-INTEG-FLAG
               MOVE 12         TO LK-RSP-RC
               MOVE M-SEAT-TAKEN TO LK-RSP-MSGTEXT
               GO TO 2500-EXIT
           END-IF
           PERFORM 7600-SEAT-SOLD-IN-DB
           IF REC-FOUND
               MOVE "N"        TO WS-INTEG-FLAG
               MOVE 12         TO LK-RSP-RC
               MOVE M-SEAT-TAKEN TO LK-RSP-MSGTEXT
           END-IF.
       2500-EXIT.
           EXIT.
      *
      *  -- fare = flight base price * class multiplier                *
       2600-COMPUTE-FARE.
           COMPUTE WS-CALC-FARE ROUNDED =
               BASEPRICE * WS-FARE-MULT.
      *
       2700-WRITE-HELD-BOOKING.
           MOVE WS-NEXT-BOOKID  TO BOOKID OF BOOKING-REC
           MOVE LK-REQ-FLIGHTNO TO FLIGHTNO OF BOOKING-REC
           MOVE LK-REQ-PASSID   TO PASSID OF BOOKING-REC
           MOVE LK-REQ-SEATNO   TO SEATNO OF BOOKING-REC
           MOVE LK-REQ-BKCLASS  TO BKCLASS OF BOOKING-REC
           MOVE LK-REQ-TRAVELDATE TO TRAVELDATE OF BOOKING-REC
           MOVE WS-CALC-FARE    TO TOTALFARE OF BOOKING-REC
           MOVE ST-HELD         TO PAYSTATUS OF BOOKING-REC
           MOVE WS-TS-CHAR      TO BKGTIMESTP OF BOOKING-REC
           WRITE BOOKING-REC
           IF NOT BKG-OK
               MOVE 24 TO LK-RSP-RC
           END-IF.
      *
      *  -- append a fresh HELD lock to the in-core matrix             *
       2800-INSERT-SEAT-LOCK.
           IF WS-LOCK-COUNT < WS-LOCK-MAX
               ADD 1 TO WS-LOCK-COUNT
               SET LKX TO WS-LOCK-COUNT
               MOVE LK-REQ-FLIGHTNO   TO WS-LK-FLIGHT (LKX)
               MOVE LK-REQ-TRAVELDATE TO WS-LK-DATE (LKX)
               MOVE LK-REQ-SEATNO     TO WS-LK-SEAT (LKX)
               MOVE WS-REQ-CABIN      TO WS-LK-CABIN (LKX)
               MOVE LK-REQ-PASSID     TO WS-LK-PASSID (LKX)
               MOVE WS-NEXT-BOOKID    TO WS-LK-BOOKID (LKX)
               MOVE "H"               TO WS-LK-STATE (LKX)
               MOVE WS-TS-CHAR        TO WS-LK-TStamp (LKX)
           END-IF.
      *
      *================================================================*
      * CONFIRM : HELD -> CONFIRMED                                    *
      *================================================================*
       3000-CONFIRM-BOOKING.
           MOVE LK-REQ-BOOKID TO BOOKID OF BOOKING-REC
           PERFORM 7000-READ-BOOKING
           IF NOT REC-FOUND
               MOVE 08      TO LK-RSP-RC
               MOVE M-BKG-NF TO LK-RSP-MSGTEXT
               GO TO 3000-EXIT
           END-IF
           MOVE PAYSTATUS OF BOOKING-REC TO WS-CUR-STATE
           MOVE ST-CONFIRMED             TO WS-NEW-STATE
           PERFORM 8300-CHECK-TRANSITION
           IF NOT TRANS-ALLOWED
               MOVE 16        TO LK-RSP-RC
               MOVE M-TRANS-BAD TO LK-RSP-MSGTEXT
               GO TO 3000-EXIT
           END-IF
           MOVE ST-CONFIRMED TO PAYSTATUS OF BOOKING-REC
           REWRITE BOOKING-REC
           PERFORM 8400-ADVANCE-LOCK
           MOVE ST-CONFIRMED TO LK-RSP-STATE
           MOVE BOOKID OF BOOKING-REC TO LK-RSP-BOOKID
           MOVE TOTALFARE OF BOOKING-REC TO LK-RSP-FARE
           MOVE M-CONFIRM-OK TO LK-RSP-MSGTEXT
           MOVE 00 TO LK-RSP-RC
           PERFORM 8800-LOG-EVENT.
       3000-EXIT.
           EXIT.
      *
      *================================================================*
      * PAY : CONFIRMED -> PAID + write financial ledger row           *
      *================================================================*
       4000-PAY-BOOKING.
           MOVE LK-REQ-BOOKID TO BOOKID OF BOOKING-REC
           PERFORM 7000-READ-BOOKING
           IF NOT REC-FOUND
               MOVE 08      TO LK-RSP-RC
               MOVE M-BKG-NF TO LK-RSP-MSGTEXT
               GO TO 4000-EXIT
           END-IF
           MOVE PAYSTATUS OF BOOKING-REC TO WS-CUR-STATE
           MOVE ST-PAID                  TO WS-NEW-STATE
           PERFORM 8300-CHECK-TRANSITION
           IF NOT TRANS-ALLOWED
               MOVE 16        TO LK-RSP-RC
               MOVE M-TRANS-BAD TO LK-RSP-MSGTEXT
               GO TO 4000-EXIT
           END-IF
      *    -- post the payment ledger row first (audit before status)
           PERFORM 7200-NEXT-TRANID
           PERFORM 4100-WRITE-PAYLOG
      *    -- then move the booking to PAID
           MOVE ST-PAID TO PAYSTATUS OF BOOKING-REC
           REWRITE BOOKING-REC
           PERFORM 8400-ADVANCE-LOCK
           MOVE ST-PAID  TO LK-RSP-STATE
           MOVE BOOKID OF BOOKING-REC TO LK-RSP-BOOKID
           MOVE WS-NEXT-TRANID        TO LK-RSP-TRANID
           MOVE TOTALFARE OF BOOKING-REC TO LK-RSP-FARE
           MOVE M-PAY-OK TO LK-RSP-MSGTEXT
           MOVE 00 TO LK-RSP-RC
           PERFORM 8800-LOG-EVENT.
       4000-EXIT.
           EXIT.
      *
       4100-WRITE-PAYLOG.
           MOVE WS-NEXT-TRANID  TO TRANID OF PAYLOG-REC
           MOVE BOOKID OF BOOKING-REC TO BOOKID OF PAYLOG-REC
           MOVE TOTALFARE OF BOOKING-REC TO AMOUNT OF PAYLOG-REC
           IF LK-REQ-CURRENCY = SPACES
               MOVE "USD" TO CURRENCY OF PAYLOG-REC
           ELSE
               MOVE LK-REQ-CURRENCY TO CURRENCY OF PAYLOG-REC
           END-IF
           MOVE LK-REQ-PAYMETHOD TO PAYMETHOD OF PAYLOG-REC
           MOVE LK-REQ-APPRCODE  TO APPRCODE OF PAYLOG-REC
           MOVE WS-TS-CHAR       TO PAYTIMESTP OF PAYLOG-REC
           WRITE PAYLOG-REC
           IF NOT PAY-OK
               MOVE 24 TO LK-RSP-RC
           END-IF.
      *
      *================================================================*
      * CANCEL : H/C/P -> CANCELLED, release the seat lock             *
      *================================================================*
       5000-CANCEL-BOOKING.
           MOVE LK-REQ-BOOKID TO BOOKID OF BOOKING-REC
           PERFORM 7000-READ-BOOKING
           IF NOT REC-FOUND
               MOVE 08      TO LK-RSP-RC
               MOVE M-BKG-NF TO LK-RSP-MSGTEXT
               GO TO 5000-EXIT
           END-IF
           MOVE PAYSTATUS OF BOOKING-REC TO WS-CUR-STATE
           MOVE ST-CANCELLED             TO WS-NEW-STATE
           PERFORM 8300-CHECK-TRANSITION
           IF NOT TRANS-ALLOWED
               MOVE 16        TO LK-RSP-RC
               MOVE M-TRANS-BAD TO LK-RSP-MSGTEXT
               GO TO 5000-EXIT
           END-IF
           MOVE ST-CANCELLED TO PAYSTATUS OF BOOKING-REC
           REWRITE BOOKING-REC
           PERFORM 8500-RELEASE-LOCK
           MOVE ST-CANCELLED TO LK-RSP-STATE
           MOVE BOOKID OF BOOKING-REC TO LK-RSP-BOOKID
           MOVE M-CANCEL-OK  TO LK-RSP-MSGTEXT
           MOVE 00 TO LK-RSP-RC
           PERFORM 8800-LOG-EVENT.
       5000-EXIT.
           EXIT.
      *
      *================================================================*
      * QUERY : return current booking state                          *
      *================================================================*
       6000-QUERY-BOOKING.
           MOVE LK-REQ-BOOKID TO BOOKID OF BOOKING-REC
           PERFORM 7000-READ-BOOKING
           IF NOT REC-FOUND
               MOVE 08      TO LK-RSP-RC
               MOVE M-BKG-NF TO LK-RSP-MSGTEXT
           ELSE
               MOVE PAYSTATUS OF BOOKING-REC TO LK-RSP-STATE
               MOVE BOOKID OF BOOKING-REC    TO LK-RSP-BOOKID
               MOVE TOTALFARE OF BOOKING-REC TO LK-RSP-FARE
               MOVE M-QUERY-OK TO LK-RSP-MSGTEXT
               MOVE 00 TO LK-RSP-RC
           END-IF.
      *
      *================================================================*
      * DATABASE I/O                                                  *
      *================================================================*
       7000-READ-BOOKING.
           MOVE "N" TO WS-FOUND-FLAG
           READ BOOKING-FILE
               KEY IS BOOKID OF BOOKING-REC
               INVALID KEY
                   MOVE "N" TO WS-FOUND-FLAG
               NOT INVALID KEY
                   MOVE "Y" TO WS-FOUND-FLAG
           END-READ.
      *
      *  -- position to highest BOOKID and add one                     *
       7100-NEXT-BOOKID.
           MOVE WS-HIGH-KEY TO BOOKID OF BOOKING-REC
           MOVE 1 TO WS-NEXT-BOOKID
           START BOOKING-FILE KEY <= BOOKID OF BOOKING-REC
               INVALID KEY
                   MOVE 1 TO WS-NEXT-BOOKID
               NOT INVALID KEY
                   READ BOOKING-FILE NEXT RECORD
                       AT END
                           MOVE 1 TO WS-NEXT-BOOKID
                       NOT AT END
                           COMPUTE WS-NEXT-BOOKID =
                               BOOKID OF BOOKING-REC + 1
                   END-READ
           END-START.
      *
       7200-NEXT-TRANID.
           MOVE WS-HIGH-KEY TO TRANID OF PAYLOG-REC
           MOVE 1 TO WS-NEXT-TRANID
           START PAYLOG-FILE KEY <= TRANID OF PAYLOG-REC
               INVALID KEY
                   MOVE 1 TO WS-NEXT-TRANID
               NOT INVALID KEY
                   READ PAYLOG-FILE NEXT RECORD
                       AT END
                           MOVE 1 TO WS-NEXT-TRANID
                       NOT AT END
                           COMPUTE WS-NEXT-TRANID =
                               TRANID OF PAYLOG-REC + 1
                   END-READ
           END-START.
      *
       7300-READ-FLIGHT.
           MOVE "N" TO WS-FOUND-FLAG
           READ FLIGHT-FILE
               KEY IS FLIGHTNO OF FLIGHT-REC
               INVALID KEY
                   MOVE "N" TO WS-FOUND-FLAG
               NOT INVALID KEY
                   MOVE "Y" TO WS-FOUND-FLAG
           END-READ.
      *
       7400-READ-PASSENGER.
           MOVE "N" TO WS-FOUND-FLAG
           READ PASSENGER-FILE
               KEY IS PASSID OF PASSENGER-REC
               INVALID KEY
                   MOVE "N" TO WS-FOUND-FLAG
               NOT INVALID KEY
                   MOVE "Y" TO WS-FOUND-FLAG
           END-READ.
      *
      *  -- count confirmed/paid seats by cabin for this flight+date   *
      *     using the second ODP so the primary cursor is untouched.   *
       7500-SCAN-SOLD-SEATS.
           MOVE LOW-VALUES TO BOOKID OF SCAN-REC
           MOVE 0 TO BOOKID OF SCAN-REC
           START SCAN-FILE KEY >= BOOKID OF SCAN-REC
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   PERFORM 7550-SCAN-LOOP
                       UNTIL SCN-EOF
           END-START.
      *
       7550-SCAN-LOOP.
           READ SCAN-FILE NEXT RECORD
               AT END
                   CONTINUE
               NOT AT END
                   IF FLIGHTNO OF SCAN-REC = LK-REQ-FLIGHTNO
                     AND TRAVELDATE OF SCAN-REC = LK-REQ-TRAVELDATE
                     AND (PAYSTATUS OF SCAN-REC = ST-HELD
                       OR PAYSTATUS OF SCAN-REC = ST-CONFIRMED
                       OR PAYSTATUS OF SCAN-REC = ST-PAID)
                       PERFORM 7560-TALLY-SCAN-CABIN
                   END-IF
           END-READ.
      *
       7560-TALLY-SCAN-CABIN.
           MOVE BKCLASS OF SCAN-REC TO WS-LOOKUP-CLASS-SCAN
           PERFORM 8150-CABIN-OF-CLASS
           EVALUATE WS-SCAN-CABIN
               WHEN "F" ADD 1 TO WS-USE-F
               WHEN "B" ADD 1 TO WS-USE-B
               WHEN "P" ADD 1 TO WS-USE-P
               WHEN "E" ADD 1 TO WS-USE-E
           END-EVALUATE.
      *
      *  -- is this exact seat already persisted (active) in DB?       *
       7600-SEAT-SOLD-IN-DB.
           MOVE "N" TO WS-FOUND-FLAG
           MOVE 0 TO BOOKID OF SCAN-REC
           START SCAN-FILE KEY >= BOOKID OF SCAN-REC
               INVALID KEY
                   CONTINUE
               NOT INVALID KEY
                   PERFORM 7650-SEAT-LOOP
                       UNTIL SCN-EOF OR REC-FOUND
           END-START.
      *
       7650-SEAT-LOOP.
           READ SCAN-FILE NEXT RECORD
               AT END
                   CONTINUE
               NOT AT END
                   IF FLIGHTNO OF SCAN-REC = LK-REQ-FLIGHTNO
                     AND TRAVELDATE OF SCAN-REC = LK-REQ-TRAVELDATE
                     AND SEATNO OF SCAN-REC = LK-REQ-SEATNO
                     AND (PAYSTATUS OF SCAN-REC = ST-HELD
                       OR PAYSTATUS OF SCAN-REC = ST-CONFIRMED
                       OR PAYSTATUS OF SCAN-REC = ST-PAID)
                       MOVE "Y" TO WS-FOUND-FLAG
                   END-IF
           END-READ.
      *
      *================================================================*
      * TABLE LOOKUPS / STATE LOGIC                                    *
      *================================================================*
      *  -- resolve booking class to cabin + fare multiplier           *
       8100-LOOKUP-CLASS.
           MOVE "N" TO WS-CLASS-FOUND
           MOVE SPACE TO WS-REQ-CABIN
           MOVE 1.000 TO WS-FARE-MULT
           SET CLX TO 1
           SEARCH WS-CLASS-ENT
               AT END
                   MOVE "N" TO WS-CLASS-FOUND
               WHEN WS-CL-CODE (CLX) = LK-REQ-BKCLASS
                   MOVE "Y" TO WS-CLASS-FOUND
                   MOVE WS-CL-CABIN (CLX) TO WS-REQ-CABIN
                   COMPUTE WS-FARE-MULT =
                       WS-CL-MULT-N (CLX) / 1000
           END-SEARCH.
      *
      *  -- cabin of an arbitrary class code (scan helper)             *
       8150-CABIN-OF-CLASS.
           MOVE SPACE TO WS-SCAN-CABIN
           SET CLX TO 1
           SEARCH WS-CLASS-ENT
               AT END
                   MOVE "E" TO WS-SCAN-CABIN
               WHEN WS-CL-CODE (CLX) = WS-LOOKUP-CLASS-SCAN
                   MOVE WS-CL-CABIN (CLX) TO WS-SCAN-CABIN
           END-SEARCH.
      *
      *  -- locate an active lock for this flight+date+seat            *
       8200-FIND-SEAT-LOCK.
           MOVE "N" TO WS-LOCK-FOUND
           IF WS-LOCK-COUNT > 0
               PERFORM VARYING LKX FROM 1 BY 1
                       UNTIL LKX > WS-LOCK-COUNT OR LOCK-HIT
                   IF WS-LK-FLIGHT (LKX) = LK-REQ-FLIGHTNO
                     AND WS-LK-DATE (LKX) = LK-REQ-TRAVELDATE
                     AND WS-LK-SEAT (LKX) = LK-REQ-SEATNO
                     AND (WS-LK-STATE (LKX) = "H"
                       OR WS-LK-STATE (LKX) = "C"
                       OR WS-LK-STATE (LKX) = "T")
                       MOVE "Y" TO WS-LOCK-FOUND
                   END-IF
               END-PERFORM
           END-IF.
      *
      *  -- validate (from,to) against the transition table            *
       8300-CHECK-TRANSITION.
           MOVE "N" TO WS-TRANS-FOUND
           SET TRX TO 1
           SEARCH WS-TRANS-ENT
               AT END
                   MOVE "N" TO WS-TRANS-FOUND
               WHEN WS-TR-FROM (TRX) = WS-CUR-STATE
                AND WS-TR-TO   (TRX) = WS-NEW-STATE
                AND WS-TR-ALLOW (TRX) = "Y"
                   MOVE "Y" TO WS-TRANS-FOUND
           END-SEARCH.
      *
      *  -- advance the lock state for the booking being progressed    *
       8400-ADVANCE-LOCK.
           IF WS-LOCK-COUNT > 0
               PERFORM VARYING LKX FROM 1 BY 1
                       UNTIL LKX > WS-LOCK-COUNT
                   IF WS-LK-BOOKID (LKX) = BOOKID OF BOOKING-REC
                       EVALUATE PAYSTATUS OF BOOKING-REC
                           WHEN ST-CONFIRMED
                               MOVE "C" TO WS-LK-STATE (LKX)
                           WHEN ST-PAID
                               MOVE "T" TO WS-LK-STATE (LKX)
                       END-EVALUATE
                   END-IF
               END-PERFORM
           END-IF.
      *
      *  -- release the lock when a booking is cancelled               *
       8500-RELEASE-LOCK.
           IF WS-LOCK-COUNT > 0
               PERFORM VARYING LKX FROM 1 BY 1
                       UNTIL LKX > WS-LOCK-COUNT
                   IF WS-LK-BOOKID (LKX) = BOOKID OF BOOKING-REC
                       MOVE "R" TO WS-LK-STATE (LKX)
                   END-IF
               END-PERFORM
           END-IF.
      *
      *  -- append a real-time lifecycle event row                     *
       8800-LOG-EVENT.
           IF WS-EVT-COUNT < WS-EVT-MAX
               ADD 1 TO WS-EVT-COUNT
               SET EVX TO WS-EVT-COUNT
               MOVE WS-TS-CHAR       TO WS-EV-TStamp (EVX)
               MOVE LK-RSP-BOOKID    TO WS-EV-BOOKID (EVX)
               MOVE WS-CUR-STATE     TO WS-EV-FROM (EVX)
               MOVE LK-RSP-STATE     TO WS-EV-TO (EVX)
               MOVE LK-REQ-FUNCTION  TO WS-EV-FUNC (EVX)
               MOVE LK-RSP-RC        TO WS-EV-RC (EVX)
           END-IF.
      *
      *  -- build a 26-char timestamp from the system clock            *
       8900-GET-TIMESTAMP.
           MOVE FUNCTION CURRENT-DATE TO WS-CURRENT-DATE
           STRING WS-CD-YYYY "-" WS-CD-MM "-" WS-CD-DD "-"
                  WS-CD-HH "." WS-CD-MI "." WS-CD-SS "."
                  WS-CD-HS "0000"
               DELIMITED BY SIZE INTO WS-TS-CHAR
           END-STRING.
      *
      *================================================================*
      * END OF PROGRAM TBSBKGCORE                                      *
      *================================================================*
 
