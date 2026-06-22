      *================================================================*
      * PROGRAM      : TBSFLMNT                                         *
      * TITLE        : Flight Inventory Master Maintenance              *
      * SYSTEM       : Travel Booking System (TBS)  -  IBM i / ILE     *
      * TYPE         : Interactive 5250 single-record CRUD maintenance  *
      *----------------------------------------------------------------*
      * FILES                                                          *
      *   TBSFLGHT  (PF, keyed on FLIGHTNO)  - update / add / delete   *
      *   TBSFLMNTD (DSPF)                   - workstation I/O         *
      *----------------------------------------------------------------*
      * BUILD                                                          *
      *   CRTDSPF  FILE(TBSDTA/TBSFLMNTD) SRCFILE(TBSDTA/QDDSSRC)      *
      *   CRTPF    FILE(TBSDTA/TBSFLGHT)  SRCFILE(TBSDTA/QDDSSRC)      *
      *   CRTBNDCBL PGM(TBSDTA/TBSFLMNT) SRCFILE(TBSDTA/QCBLLESRC)     *
      *----------------------------------------------------------------*
      * DESIGN NOTES                                                   *
      *   * Validation is table-driven and factored into reusable      *
      *     paragraphs.  Each rule lives in exactly one place.         *
      *   * Indicators 71-80 flag individual fields in reverse image;  *
      *     indicator 70 protects the detail screen for view modes.    *
      *   * The display file deliberately omits INDARA, so ILE COBOL    *
      *     surfaces option/response indicators as INnn data items.     *
      *================================================================*
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    TBSFLMNT.
       AUTHOR.        TBS-DEV-TEAM.
      *
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER. IBM-I.
       OBJECT-COMPUTER. IBM-I.
      *
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT FLIGHT-FILE
               ASSIGN TO DATABASE-TBSFLGHT
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS FLIGHTNO
               FILE STATUS IS WS-FLT-STATUS.
      *
           SELECT DISPLAY-FILE
               ASSIGN TO WORKSTATION-TBSFLMNTD
               ORGANIZATION IS TRANSACTION
               ACCESS MODE IS SEQUENTIAL
               FILE STATUS IS WS-DSP-STATUS.
      *
       DATA DIVISION.
       FILE SECTION.
       FD  FLIGHT-FILE.
       01  FLIGHT-REC.
           COPY DDS-ALL-FORMATS OF TBSFLGHT.
      *
       FD  DISPLAY-FILE.
       01  DISPLAY-REC.
           COPY DDS-ALL-FORMATS OF TBSFLMNTD.
      *
       WORKING-STORAGE SECTION.
      *----------------------------------------------------------------*
      * FILE STATUS / CONTROL FLAGS                                    *
      *----------------------------------------------------------------*
       01  WS-FLT-STATUS            PIC X(02)  VALUE "00".
           88  FLT-OK                          VALUE "00".
           88  FLT-NOTFOUND                    VALUE "23".
           88  FLT-EOF                         VALUE "10".
           88  FLT-DUPKEY                      VALUE "22".
      *
       01  WS-DSP-STATUS            PIC X(02)  VALUE "00".
      *
       01  WS-FLAGS.
           05  WS-EXIT-FLAG         PIC X(01)  VALUE "N".
               88  EXIT-REQUESTED              VALUE "Y".
           05  WS-CANCEL-FLAG       PIC X(01)  VALUE "N".
               88  CANCEL-REQUESTED            VALUE "Y".
           05  WS-ERROR-FLAG        PIC X(01)  VALUE "N".
               88  NO-ERRORS                   VALUE "N".
               88  ERRORS-FOUND                VALUE "Y".
           05  WS-FOUND-FLAG        PIC X(01)  VALUE "N".
               88  REC-FOUND                   VALUE "Y".
           05  WS-LOOKUP-FOUND      PIC X(01)  VALUE "N".
               88  LOOKUP-HIT                  VALUE "Y".
      *
       01  WS-ACTION                PIC X(01)  VALUE SPACE.
           88  ACT-ADD                         VALUE "A".
           88  ACT-CHANGE                      VALUE "C".
           88  ACT-DELETE                      VALUE "D".
           88  ACT-INQUIRY                     VALUE "I".
           88  ACT-VALID                       VALUE "A" "C" "D" "I".
           88  ACT-VIEW-ONLY                   VALUE "D" "I".
      *
      *----------------------------------------------------------------*
      * VALID CARRIER TABLE (ICAO 3-LETTER)                            *
      *----------------------------------------------------------------*
       01  WS-CARRIER-CONST.
           05  FILLER  PIC X(36) VALUE
               "AALBAWDLHUALAFRKLMUAEQTRSIAANAACAAIC".
       01  WS-CARRIER-TABLE REDEFINES WS-CARRIER-CONST.
           05  WS-CARRIER-ENT      OCCURS 12 TIMES
                                   INDEXED BY CX
                                   PIC X(03).
      *
      *----------------------------------------------------------------*
      * VALID AIRPORT TABLE (IATA 3-LETTER)                            *
      *----------------------------------------------------------------*
       01  WS-AIRPORT-CONST.
           05  FILLER  PIC X(45) VALUE
               "JFKLHRATLORDCDGFRADXBDOHSINHNDYYZDELBOMLAXSFO".
       01  WS-AIRPORT-TABLE REDEFINES WS-AIRPORT-CONST.
           05  WS-AIRPORT-ENT      OCCURS 15 TIMES
                                   INDEXED BY AX
                                   PIC X(03).
      *
      *----------------------------------------------------------------*
      * WORK FIELDS                                                    *
      *----------------------------------------------------------------*
       01  WS-WORK.
           05  WS-LOOKUP-VAL       PIC X(03)  VALUE SPACE.
           05  WS-SEAT-SUM         PIC S9(05) VALUE ZERO.
           05  WS-IDX              PIC S9(04) VALUE ZERO.
           05  WS-CH               PIC X(01)  VALUE SPACE.
           05  WS-NUM-OK           PIC X(01)  VALUE "Y".
           05  WS-PRICE-TIER       PIC X(10)  VALUE SPACE.
      *
       01  WS-LIMITS.
           05  WS-MAX-SEATCAP      PIC S9(05) VALUE 853.
           05  WS-MAX-FIRST        PIC S9(05) VALUE 16.
           05  WS-MAX-PRICE        PIC S9(09)V99 VALUE 99999999.99.
      *
      *----------------------------------------------------------------*
      * MESSAGE TEXT CONSTANTS                                         *
      *----------------------------------------------------------------*
       01  WS-MSG.
           05  M-ENTER-KEY     PIC X(60) VALUE
               "Enter flight number and action, then press Enter.".
           05  M-ACT-INVALID   PIC X(60) VALUE
               "Action must be A, C, D or I.".
           05  M-KEY-BLANK     PIC X(60) VALUE
               "Flight number is required.".
           05  M-NOT-FOUND     PIC X(60) VALUE
               "Flight not found.".
           05  M-ALREADY-EXIST PIC X(60) VALUE
               "Flight already exists - use Change.".
           05  M-ADD-OK        PIC X(60) VALUE
               "Flight added successfully.".
           05  M-CHG-OK        PIC X(60) VALUE
               "Flight changed successfully.".
           05  M-DLT-OK        PIC X(60) VALUE
               "Flight deleted successfully.".
           05  M-EDIT-ERR      PIC X(60) VALUE
               "Errors found - correct highlighted fields.".
           05  M-CONFIRM-DLT   PIC X(60) VALUE
               "Press Enter to confirm DELETE, F12 to cancel.".
      *
      *================================================================*
       PROCEDURE DIVISION.
      *================================================================*
       0000-MAINLINE.
           PERFORM 0100-INITIALIZE
           PERFORM 0200-OPEN-FILES
           PERFORM 1000-PROCESS-CONTROL
               UNTIL EXIT-REQUESTED
           PERFORM 9000-CLOSE-FILES
           STOP RUN.
      *
       0100-INITIALIZE.
           MOVE "N" TO WS-EXIT-FLAG
           MOVE SPACE TO CMSG
           MOVE M-ENTER-KEY TO CMSG.
      *
       0200-OPEN-FILES.
           OPEN I-O   FLIGHT-FILE
           OPEN I-O   DISPLAY-FILE.
      *
       9000-CLOSE-FILES.
           CLOSE FLIGHT-FILE
           CLOSE DISPLAY-FILE.
      *
      *----------------------------------------------------------------*
      * CONTROL SCREEN DRIVER                                          *
      *----------------------------------------------------------------*
       1000-PROCESS-CONTROL.
           PERFORM 1100-SHOW-CONTROL
           IF EXIT-REQUESTED
               CONTINUE
           ELSE
               PERFORM 1200-EDIT-CONTROL
               IF NO-ERRORS
                   PERFORM 2000-ROUTE-ACTION
               END-IF
           END-IF.
      *
       1100-SHOW-CONTROL.
           PERFORM 7300-CLEAR-ERR-IND
           WRITE DISPLAY-REC FORMAT IS "CTLFMT"
           READ DISPLAY-FILE FORMAT IS "CTLFMT"
           IF IN03 = "1"
               MOVE "Y" TO WS-EXIT-FLAG
           END-IF
           IF IN05 = "1"
               PERFORM 0100-INITIALIZE
               MOVE SPACE TO CFLIGHT CACTION
           END-IF.
      *
       1200-EDIT-CONTROL.
           MOVE "N" TO WS-ERROR-FLAG
           MOVE SPACE TO CMSG
      *    -- flight number must be present
           IF CFLIGHT = SPACES
               MOVE "1" TO IN61
               MOVE M-KEY-BLANK TO CMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
      *    -- action must be one of A/C/D/I
           MOVE CACTION TO WS-ACTION
           IF NOT ACT-VALID
               MOVE "1" TO IN62
               MOVE M-ACT-INVALID TO CMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *----------------------------------------------------------------*
      * ROUTE TO THE SELECTED ACTION                                  *
      *----------------------------------------------------------------*
       2000-ROUTE-ACTION.
           MOVE CFLIGHT TO FLIGHTNO
           MOVE CACTION TO WS-ACTION
           EVALUATE TRUE
               WHEN ACT-ADD       PERFORM 3000-ADD-FLIGHT
               WHEN ACT-CHANGE    PERFORM 3100-CHANGE-FLIGHT
               WHEN ACT-DELETE    PERFORM 3200-DELETE-FLIGHT
               WHEN ACT-INQUIRY   PERFORM 3300-INQUIRE-FLIGHT
           END-EVALUATE.
      *
      *----------------------------------------------------------------*
      * ADD                                                            *
      *----------------------------------------------------------------*
       3000-ADD-FLIGHT.
           PERFORM 6300-READ-FLIGHT-RANDOM
           IF REC-FOUND
               MOVE "1" TO IN61
               MOVE M-ALREADY-EXIST TO CMSG
           ELSE
               PERFORM 7000-CLEAR-DETAIL
               MOVE CFLIGHT TO DFLIGHT
               MOVE "0" TO IN70
               MOVE SPACE TO WS-CANCEL-FLAG
               PERFORM 4000-DETAIL-EDIT-LOOP
                   UNTIL NO-ERRORS OR CANCEL-REQUESTED
               IF NO-ERRORS AND NOT CANCEL-REQUESTED
                   PERFORM 7200-MOVE-SCREEN-TO-DB
                   PERFORM 6000-WRITE-FLIGHT
                   MOVE M-ADD-OK TO CMSG
               END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
      * CHANGE                                                         *
      *----------------------------------------------------------------*
       3100-CHANGE-FLIGHT.
           PERFORM 6300-READ-FLIGHT-RANDOM
           IF NOT REC-FOUND
               MOVE "1" TO IN61
               MOVE M-NOT-FOUND TO CMSG
           ELSE
               PERFORM 7100-MOVE-DB-TO-SCREEN
               MOVE "0" TO IN70
               MOVE SPACE TO WS-CANCEL-FLAG
               PERFORM 4000-DETAIL-EDIT-LOOP
                   UNTIL NO-ERRORS OR CANCEL-REQUESTED
               IF NO-ERRORS AND NOT CANCEL-REQUESTED
                   PERFORM 7200-MOVE-SCREEN-TO-DB
                   PERFORM 6100-REWRITE-FLIGHT
                   MOVE M-CHG-OK TO CMSG
               END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
      * DELETE                                                         *
      *----------------------------------------------------------------*
       3200-DELETE-FLIGHT.
           PERFORM 6300-READ-FLIGHT-RANDOM
           IF NOT REC-FOUND
               MOVE "1" TO IN61
               MOVE M-NOT-FOUND TO CMSG
           ELSE
               PERFORM 7100-MOVE-DB-TO-SCREEN
               MOVE "1" TO IN70
               MOVE M-CONFIRM-DLT TO DMSG
               PERFORM 4100-READ-DETAIL
               IF NOT CANCEL-REQUESTED AND NOT EXIT-REQUESTED
                   PERFORM 6200-DELETE-DB
                   MOVE M-DLT-OK TO CMSG
               END-IF
           END-IF.
      *
      *----------------------------------------------------------------*
      * INQUIRY                                                        *
      *----------------------------------------------------------------*
       3300-INQUIRE-FLIGHT.
           PERFORM 6300-READ-FLIGHT-RANDOM
           IF NOT REC-FOUND
               MOVE "1" TO IN61
               MOVE M-NOT-FOUND TO CMSG
           ELSE
               PERFORM 7100-MOVE-DB-TO-SCREEN
               MOVE "1" TO IN70
               MOVE SPACE TO DMSG
               PERFORM 4100-READ-DETAIL
           END-IF.
      *
      *----------------------------------------------------------------*
      * DETAIL EDIT LOOP : show, read, validate                        *
      *----------------------------------------------------------------*
       4000-DETAIL-EDIT-LOOP.
           PERFORM 4100-READ-DETAIL
           IF CANCEL-REQUESTED OR EXIT-REQUESTED
               MOVE "N" TO WS-ERROR-FLAG
           ELSE
               PERFORM 5000-VALIDATE-DETAIL
               IF ERRORS-FOUND
                   MOVE M-EDIT-ERR TO DMSG
               END-IF
           END-IF.
      *
       4100-READ-DETAIL.
           WRITE DISPLAY-REC FORMAT IS "DTLFMT"
           READ DISPLAY-FILE FORMAT IS "DTLFMT"
           MOVE "N" TO WS-CANCEL-FLAG
           IF IN03 = "1"
               MOVE "Y" TO WS-EXIT-FLAG
           END-IF
           IF IN12 = "1"
               MOVE "Y" TO WS-CANCEL-FLAG
           END-IF.
      *
      *================================================================*
      * VALIDATION SUITE                                               *
      *================================================================*
       5000-VALIDATE-DETAIL.
           MOVE "N" TO WS-ERROR-FLAG
           PERFORM 7300-CLEAR-ERR-IND
           MOVE SPACE TO DMSG
           PERFORM 5100-VAL-CARRIER
           PERFORM 5200-VAL-DEPART
           PERFORM 5300-VAL-ARRIVE
           PERFORM 5400-VAL-ROUTE
           PERFORM 5500-VAL-SEATCAP
           PERFORM 5600-VAL-CLASS-SEATS
           PERFORM 5700-VAL-SEAT-SUM
           PERFORM 5800-VAL-PRICE
           PERFORM 5900-VAL-STATUS
           PERFORM 5950-VAL-PRICE-TIER.
      *
      *  -- Carrier : non-blank, in the approved ICAO table,           *
      *     and must match the first 3 chars of the flight key.        *
       5100-VAL-CARRIER.
           IF DCARRIER = SPACES
               MOVE "1" TO IN71
               MOVE "Carrier code is required." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           ELSE
               MOVE DCARRIER TO WS-LOOKUP-VAL
               PERFORM 8100-LOOKUP-CARRIER
               IF NOT LOOKUP-HIT
                   MOVE "1" TO IN71
                   MOVE "Carrier not in approved list." TO DMSG
                   MOVE "Y" TO WS-ERROR-FLAG
               ELSE
                   IF DCARRIER NOT = CFLIGHT (1:3)
                       MOVE "1" TO IN71
                       MOVE "Carrier must match flight prefix." TO DMSG
                       MOVE "Y" TO WS-ERROR-FLAG
                   END-IF
               END-IF
           END-IF.
      *
       5200-VAL-DEPART.
           IF DDEPART = SPACES
               MOVE "1" TO IN72
               MOVE "Departure airport is required." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           ELSE
               MOVE DDEPART TO WS-LOOKUP-VAL
               PERFORM 8200-LOOKUP-AIRPORT
               IF NOT LOOKUP-HIT
                   MOVE "1" TO IN72
                   MOVE "Departure airport not recognised." TO DMSG
                   MOVE "Y" TO WS-ERROR-FLAG
               END-IF
           END-IF.
      *
       5300-VAL-ARRIVE.
           IF DARRIVE = SPACES
               MOVE "1" TO IN73
               MOVE "Arrival airport is required." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           ELSE
               MOVE DARRIVE TO WS-LOOKUP-VAL
               PERFORM 8200-LOOKUP-AIRPORT
               IF NOT LOOKUP-HIT
                   MOVE "1" TO IN73
                   MOVE "Arrival airport not recognised." TO DMSG
                   MOVE "Y" TO WS-ERROR-FLAG
               END-IF
           END-IF.
      *
      *  -- Departure and arrival may not be the same airport.         *
       5400-VAL-ROUTE.
           IF DDEPART = DARRIVE
             AND DDEPART NOT = SPACES
               MOVE "1" TO IN72
               MOVE "1" TO IN73
               MOVE "Departure and arrival must differ." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *  -- Total seat capacity : 1 .. WS-MAX-SEATCAP                   *
       5500-VAL-SEATCAP.
           IF DSEATCAP < 1 OR DSEATCAP > WS-MAX-SEATCAP
               MOVE "1" TO IN74
               MOVE "Seat capacity out of range (1-853)." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *  -- Per-class seats : non-negative, first class capped,        *
      *     economy must be the largest single cabin.                  *
       5600-VAL-CLASS-SEATS.
           IF DFSTSEA < 0
               MOVE "1" TO IN75
               MOVE "First class seats cannot be negative." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
           IF DFSTSEA > WS-MAX-FIRST
               MOVE "1" TO IN75
               MOVE "First class seats exceed cabin limit." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
           IF DBUSSEA < 0
               MOVE "1" TO IN76
               MOVE "Business seats cannot be negative." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
           IF DPRMSEA < 0
               MOVE "1" TO IN77
               MOVE "Premium seats cannot be negative." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
           IF DECOSEA < 0
               MOVE "1" TO IN78
               MOVE "Economy seats cannot be negative." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
           IF DECOSEA < DFSTSEA
             OR DECOSEA < DBUSSEA
             OR DECOSEA < DPRMSEA
               MOVE "1" TO IN78
               MOVE "Economy must be the largest cabin." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *  -- Class seats must sum exactly to total capacity.            *
       5700-VAL-SEAT-SUM.
           COMPUTE WS-SEAT-SUM =
               DFSTSEA + DBUSSEA + DPRMSEA + DECOSEA
           IF WS-SEAT-SUM NOT = DSEATCAP
               MOVE "1" TO IN74
               MOVE "1" TO IN75
               MOVE "1" TO IN76
               MOVE "1" TO IN77
               MOVE "1" TO IN78
               MOVE "Class seats must total seat capacity." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *  -- Base price : positive, within ceiling.                     *
       5800-VAL-PRICE.
           IF DPRICE <= 0
               MOVE "1" TO IN79
               MOVE "Base price must be greater than zero." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF
           IF DPRICE > WS-MAX-PRICE
               MOVE "1" TO IN79
               MOVE "Base price exceeds maximum allowed." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *  -- Status : A / I / S / X only.                               *
       5900-VAL-STATUS.
           IF DSTATUS = "A" OR "I" OR "S" OR "X"
               CONTINUE
           ELSE
               MOVE "1" TO IN80
               MOVE "Status must be A, I, S or X." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *  -- Pricing tier : derive tier band and enforce that the       *
      *     ULTRA band is only valid when first-class seats exist.     *
       5950-VAL-PRICE-TIER.
           EVALUATE TRUE
               WHEN DPRICE < 500.00
                   MOVE "BASIC" TO WS-PRICE-TIER
               WHEN DPRICE < 2000.00
                   MOVE "STANDARD" TO WS-PRICE-TIER
               WHEN DPRICE < 10000.00
                   MOVE "PREMIUM" TO WS-PRICE-TIER
               WHEN OTHER
                   MOVE "ULTRA" TO WS-PRICE-TIER
           END-EVALUATE
           IF WS-PRICE-TIER = "ULTRA" AND DFSTSEA = 0
               MOVE "1" TO IN79
               MOVE "1" TO IN75
               MOVE "ULTRA fare requires first class seats." TO DMSG
               MOVE "Y" TO WS-ERROR-FLAG
           END-IF.
      *
      *================================================================*
      * TABLE LOOKUPS                                                  *
      *================================================================*
       8100-LOOKUP-CARRIER.
           MOVE "N" TO WS-LOOKUP-FOUND
           SET CX TO 1
           SEARCH WS-CARRIER-ENT
               AT END
                   MOVE "N" TO WS-LOOKUP-FOUND
               WHEN WS-CARRIER-ENT (CX) = WS-LOOKUP-VAL
                   MOVE "Y" TO WS-LOOKUP-FOUND
           END-SEARCH.
      *
       8200-LOOKUP-AIRPORT.
           MOVE "N" TO WS-LOOKUP-FOUND
           SET AX TO 1
           SEARCH WS-AIRPORT-ENT
               AT END
                   MOVE "N" TO WS-LOOKUP-FOUND
               WHEN WS-AIRPORT-ENT (AX) = WS-LOOKUP-VAL
                   MOVE "Y" TO WS-LOOKUP-FOUND
           END-SEARCH.
      *
      *================================================================*
      * DATABASE I/O                                                   *
      *================================================================*
       6000-WRITE-FLIGHT.
           WRITE FLIGHT-REC.
      *
       6100-REWRITE-FLIGHT.
           REWRITE FLIGHT-REC.
      *
       6200-DELETE-DB.
           DELETE FLIGHT-FILE.
      *
       6300-READ-FLIGHT-RANDOM.
           MOVE "N" TO WS-FOUND-FLAG
           READ FLIGHT-FILE
               KEY IS FLIGHTNO
               INVALID KEY
                   MOVE "N" TO WS-FOUND-FLAG
               NOT INVALID KEY
                   MOVE "Y" TO WS-FOUND-FLAG
           END-READ.
      *
      *================================================================*
      * SCREEN <-> RECORD MAPPING                                      *
      *================================================================*
       7000-CLEAR-DETAIL.
           MOVE SPACES TO DCARRIER DDEPART DARRIVE DSTATUS
           MOVE ZERO   TO DSEATCAP DFSTSEA DBUSSEA DPRMSEA DECOSEA
           MOVE ZERO   TO DPRICE
           MOVE SPACE  TO DMSG.
      *
       7100-MOVE-DB-TO-SCREEN.
           MOVE FLIGHTNO  TO DFLIGHT
           MOVE CARRIER   TO DCARRIER
           MOVE DEPARPT   TO DDEPART
           MOVE ARRARPT   TO DARRIVE
           MOVE SEATCAP   TO DSEATCAP
           MOVE FSTSEATS  TO DFSTSEA
           MOVE BUSSEATS  TO DBUSSEA
           MOVE PRMSEATS  TO DPRMSEA
           MOVE ECOSEATS  TO DECOSEA
           MOVE BASEPRICE TO DPRICE
           MOVE FLSTATUS  TO DSTATUS.
      *
       7200-MOVE-SCREEN-TO-DB.
           MOVE CFLIGHT   TO FLIGHTNO
           MOVE DCARRIER  TO CARRIER
           MOVE DDEPART   TO DEPARPT
           MOVE DARRIVE   TO ARRARPT
           MOVE DSEATCAP  TO SEATCAP
           MOVE DFSTSEA   TO FSTSEATS
           MOVE DBUSSEA   TO BUSSEATS
           MOVE DPRMSEA   TO PRMSEATS
           MOVE DECOSEA   TO ECOSEATS
           MOVE DPRICE    TO BASEPRICE
           MOVE DSTATUS   TO FLSTATUS.
      *
       7300-CLEAR-ERR-IND.
           MOVE "0" TO IN61
           MOVE "0" TO IN62
           MOVE "0" TO IN71
           MOVE "0" TO IN72
           MOVE "0" TO IN73
           MOVE "0" TO IN74
           MOVE "0" TO IN75
           MOVE "0" TO IN76
           MOVE "0" TO IN77
           MOVE "0" TO IN78
           MOVE "0" TO IN79
           MOVE "0" TO IN80.
      *
      *================================================================*
      * END OF PROGRAM TBSFLMNT                                        *
      *================================================================*
