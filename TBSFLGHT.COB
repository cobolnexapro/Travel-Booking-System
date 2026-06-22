      *==================================================================*
      * TRAVEL BOOKING SYSTEM (TBS) - DDS PHYSICAL FILE SOURCE            *
      * Library : TBSDTA   Source File : QDDSSRC                          *
      * Compile : CRTPF FILE(TBSDTA/xxxxxxxx) SRCMBR(xxxxxxxx)            *
      *------------------------------------------------------------------*
      * DDS DATA-TYPE LEGEND                                              *
      *   nnA      = Alphanumeric (CHAR)                                  *
      *   nnS s    = Zoned   decimal (s = decimal positions)             *
      *   nnP s    = Packed  decimal (s = decimal positions)             *
      *   L        = Date     Z = Timestamp                              *
      * NOTE: DDS PFs cannot generate IDENTITY columns; surrogate keys    *
      *       here are RRN-managed or assigned by the application. Use the *
      *       SQL/DDL build (TBS_DDL.sql) if IDENTITY keys are required.   *
      *==================================================================*
 
      *------------------------------------------------------------------*
      * MEMBER: TBSFLGHT - Flight Inventory Master                        *
      *------------------------------------------------------------------*
     A          R FLGHTREC                  TEXT('Flight Inventory Mstr')
     A            CARRIER        3A         COLHDG('Carrier')
     A            FLIGHTNO       6A         COLHDG('Flight' 'Number')
     A            DEPARPT        3A         COLHDG('Dep' 'Airport')
     A            ARRARPT        3A         COLHDG('Arr' 'Airport')
     A            SEATCAP        4S 0       COLHDG('Seat' 'Capacity')
     A            FSTSEATS       4S 0       COLHDG('First' 'Seats')
     A            BUSSEATS       4S 0       COLHDG('Bus' 'Seats')
     A            PRMSEATS       4S 0       COLHDG('Prem' 'Seats')
     A            ECOSEATS       4S 0       COLHDG('Eco' 'Seats')
     A            BASEPRICE     11P 2       COLHDG('Base' 'Price')
     A            FLSTATUS       1A         COLHDG('Sts')
     A          K FLIGHTNO
 
      *------------------------------------------------------------------*
      * MEMBER: TBSPASSG - Passenger Profile Master                      *
      *------------------------------------------------------------------*
     A          R PASSGREC                  TEXT('Passenger Profile Mstr')
     A            PASSID         9P 0       COLHDG('Passenger' 'ID')
     A            PASSPORTNO    15A         COLHDG('Passport' 'Number')
     A            LASTNAME      30A         COLHDG('Last Name')
     A            FRSTNAME      30A         COLHDG('First Name')
     A            TIERLEVEL      1A         COLHDG('Tier')
     A            SEATPREF       1A         COLHDG('Seat' 'Pref')
     A            MEALPREF       4A         COLHDG('Meal' 'Pref')
     A            EMAIL         50A         COLHDG('Email')
     A            TOTLMILES     11P 0       COLHDG('Total' 'Miles')
     A            PASTATUS       1A         COLHDG('Sts')
     A          K PASSID
 
      *------------------------------------------------------------------*
      * MEMBER: TBSBOOKG - Master Booking Transaction File               *
      *------------------------------------------------------------------*
     A          R BOOKGREC                  TEXT('Master Booking Trans')
     A            BOOKID         9P 0       COLHDG('Booking' 'ID')
     A            FLIGHTNO       6A         COLHDG('Flight' 'Number')
     A            PASSID         9P 0       COLHDG('Passenger' 'ID')
     A            SEATNO         4A         COLHDG('Seat')
     A            BKCLASS        1A         COLHDG('Cls')
     A            TRAVELDATE      L         COLHDG('Travel' 'Date')
     A            TOTALFARE     11P 2       COLHDG('Total' 'Fare')
     A            PAYSTATUS      1A         COLHDG('Pay' 'Sts')
     A            BKGTIMESTP      Z         COLHDG('Booking' 'Tstamp')
     A          K BOOKID
 
      *------------------------------------------------------------------*
      * MEMBER: TBSPAYLOG - Financial Audit Ledger                       *
      *------------------------------------------------------------------*
     A          R PAYLOGREC                 TEXT('Financial Audit Ledger')
     A            TRANID         9P 0       COLHDG('Trans' 'ID')
     A            BOOKID         9P 0       COLHDG('Booking' 'ID')
     A            AMOUNT        11P 2       COLHDG('Amount')
     A            CURRENCY       3A         COLHDG('Ccy')
     A            PAYMETHOD      4A         COLHDG('Pay' 'Method')
     A            APPRCODE       6A         COLHDG('Approval' 'Code')
     A            PAYTIMESTP      Z         COLHDG('Trans' 'Tstamp')
     A          K TRANID
