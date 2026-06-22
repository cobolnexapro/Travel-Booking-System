# Enterprise Travel Booking System (TBS) - IBM i

This repository contains the database definitions, interactive utilities, and core transaction engines for an Enterprise Travel Booking System (TBS) designed specifically for the IBM i (AS/400) platform. Built entirely with ILE COBOL and Native DDS/DB2 for i, the system ensures high-performance, strictly audited travel operations ranging from real-time booking to executive analytics.

## System Architecture Highlights

* **Headless Transaction Engine:** The core booking routines are decoupled from the UI, operating as a callable transaction engine that handles passenger integrity, seat decrements, and financial logging in real-time.
* **Rigorous Audit & Compliance:** End-of-day processes and real-time ledger updates ensure all forfeited reservations (no-shows) and financial transactions emit strict audit journal records.
* **Executive Analytics:** Automated batch reporting generates highly structured, multi-level control break reports computing industry-standard metrics like Passenger Load Factor (PLF) and Revenue per Available Seat Mile (RASM).

## Module Breakdown

### 1. Database & Schema (DDS)
* **`TBSFLGHT.txt`**: Contains the Data Description Specifications (DDS) for the physical database files. 
  * `TBSFLGHT`: Flight Inventory Master (Carrier, Flight No, Airport routes, Seat Capacities, Base Prices).
  * `TBSBOOKG`: Master Booking Transactions (Booking IDs, Travel Dates, Fares, Payment Status).
  * `TBSPAYLOG`: Financial Audit Ledger (Transaction IDs, Currencies, Payment Methods, Approval Codes).

### 2. Core Booking Engine
* **`TBSBKGCORE.cbl`**: The master booking execution core. This headless, callable transaction program manages the real-time lifecycle of a booking. It validates capacities from the flight master, records the passenger booking, handles concurrency with internal locking states, and appends a real-time lifecycle timestamp to the financial ledger.

### 3. Interactive Maintenance (5250)
* **`TBSFLMNT.cbl` & `TBSFLMNTD.dspf`**: An interactive 5250 green-screen application providing single-record CRUD (Create, Read, Update, Delete) maintenance for the Flight Inventory Master. It utilizes comprehensive table-driven validation rules and field-level reverse-image indicators to manage data entry seamlessly.

### 4. Batch Operations & Reporting
* **`TBSEODBATCH.cbl`**: The End-of-Day (EOD) reconciliation processor. It scans active reservations against the current system date, transforms expired or lapsed reservations into "no-show" forfeits, calculates lost revenue, and writes explicit audit-log records for accountability.
* **`TBSANAREPT.cbl`**: An executive analytical report writer. It reads pre-sorted flight data to produce a printed control-break report (`QSYSPRT`) that calculates total capacities, bookings, PLF (Passenger Load Factor), and RASM (Revenue per Available Seat Mile) rolled up by region and system totals.

## Compilation & Deployment

To deploy this application on an IBM i system, the source code must be compiled in the correct dependency order.

1. **Compile Database Physical Files:**
   ```bash
   CRTPF FILE(TBSDTA/TBSFLGHT) SRCFILE(TBSDTA/QDDSSRC)
   CRTPF FILE(TBSDTA/TBSBOOKG) SRCFILE(TBSDTA/QDDSSRC)
   CRTPF FILE(TBSDTA/TBSPAYLOG) SRCFILE(TBSDTA/QDDSSRC)
