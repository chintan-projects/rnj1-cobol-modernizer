      ******************************************************************
      * Program Name : COACTVWC
      * Application  : CardDemo
      * Type         : CICS COBOL Program
      * Function     : Account View - Display account details
      *                Online inquiry for customer service reps
      ******************************************************************
      * Copyright Amazon.com, Inc. or its affiliates.
      * All Rights Reserved.
      * Licensed under the Apache License, Version 2.0
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID.    COACTVWC.
       AUTHOR.        AWS MAINFRAME MODERNIZATION TEAM.
      
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
      
       DATA DIVISION.
       WORKING-STORAGE SECTION.
      
       01  WS-COMMAREA.
           05 WS-COMM-ACCOUNT-ID        PIC X(11).
           05 WS-COMM-RETURN-CODE       PIC 9(2).
              88 COMM-SUCCESS           VALUE 00.
              88 COMM-ACCT-NOT-FOUND    VALUE 10.
              88 COMM-INVALID-INPUT     VALUE 20.
              88 COMM-DB-ERROR          VALUE 99.
           05 WS-COMM-MESSAGE           PIC X(50).
      
       01  WS-ACCOUNT-DATA.
           05 WS-ACCT-ID                PIC X(11).
           05 WS-ACCT-NAME              PIC X(50).
           05 WS-ACCT-ADDRESS.
              10 WS-ADDR-LINE1          PIC X(50).
              10 WS-ADDR-LINE2          PIC X(50).
              10 WS-ADDR-CITY           PIC X(30).
              10 WS-ADDR-STATE          PIC X(02).
              10 WS-ADDR-ZIP            PIC X(10).
           05 WS-ACCT-PHONE             PIC X(15).
           05 WS-ACCT-CREDIT-LIMIT      PIC 9(7)V99.
           05 WS-ACCT-CURRENT-BAL       PIC S9(7)V99.
           05 WS-ACCT-AVAILABLE-CREDIT  PIC S9(7)V99.
           05 WS-ACCT-STATUS            PIC X(01).
           05 WS-ACCT-OPEN-DATE         PIC X(10).
      
       01  WS-FLAGS.
           05 WS-RESP-CODE              PIC S9(8) COMP.
           05 WS-RESP2-CODE             PIC S9(8) COMP.
           05 WS-DB-ERROR-FLAG          PIC X VALUE 'N'.
              88 DB-ERROR               VALUE 'Y'.
              88 NO-DB-ERROR            VALUE 'N'.
      
       01  WS-DISPLAY-FIELDS.
           05 WS-DISP-CREDIT-LIMIT      PIC $ZZZ,ZZ9.99.
           05 WS-DISP-CURRENT-BAL       PIC $ZZZ,ZZ9.99-.
           05 WS-DISP-AVAILABLE         PIC $ZZZ,ZZ9.99-.
           05 WS-DISP-STATUS            PIC X(10).
      
       01  WS-ERROR-MESSAGES.
           05 ERR-ACCT-NOT-FOUND        PIC X(50)
              VALUE 'ACCOUNT NOT FOUND IN DATABASE'.
           05 ERR-INVALID-INPUT         PIC X(50)
              VALUE 'INVALID ACCOUNT NUMBER FORMAT'.
           05 ERR-DB-ERROR              PIC X(50)
              VALUE 'DATABASE ERROR - CONTACT SUPPORT'.
      
       COPY DFHAID.
       COPY DFHBMSCA.
      
       LINKAGE SECTION.
       01  DFHCOMMAREA                  PIC X(63).
      
       PROCEDURE DIVISION.
      
       0000-MAIN-PROCESS.
           EVALUATE TRUE
              WHEN EIBCALEN = 0
                 PERFORM 1000-FIRST-TIME
              WHEN EIBAID = DFHENTER
                 PERFORM 2000-PROCESS-INPUT
              WHEN EIBAID = DFHPF3
                 PERFORM 9000-RETURN-TO-MENU
              WHEN EIBAID = DFHPF12
                 PERFORM 9000-RETURN-TO-MENU
              WHEN EIBAID = DFHCLEAR
                 PERFORM 1000-FIRST-TIME
              WHEN OTHER
                 PERFORM 8000-INVALID-KEY
           END-EVALUATE
           
           EXEC CICS RETURN
              TRANSID('CAVW')
              COMMAREA(WS-COMMAREA)
              LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
      
       1000-FIRST-TIME.
           INITIALIZE WS-COMMAREA
           INITIALIZE WS-ACCOUNT-DATA
           
           PERFORM 1100-SEND-EMPTY-MAP.
      
       1100-SEND-EMPTY-MAP.
           EXEC CICS SEND
              MAP('ACTVWM')
              MAPSET('ACTVWS')
              ERASE
              CURSOR
           END-EXEC.
      
       2000-PROCESS-INPUT.
           EXEC CICS RECEIVE
              MAP('ACTVWM')
              MAPSET('ACTVWS')
              INTO(WS-COMMAREA)
           END-EXEC
           
           PERFORM 2100-VALIDATE-INPUT
           
           IF COMM-SUCCESS
              PERFORM 3000-RETRIEVE-ACCOUNT
           END-IF
           
           PERFORM 4000-SEND-RESPONSE.
      
       2100-VALIDATE-INPUT.
           IF WS-COMM-ACCOUNT-ID = SPACES OR
              WS-COMM-ACCOUNT-ID = LOW-VALUES
              SET COMM-INVALID-INPUT TO TRUE
              MOVE ERR-INVALID-INPUT TO WS-COMM-MESSAGE
           ELSE
              SET COMM-SUCCESS TO TRUE
           END-IF.
      
       3000-RETRIEVE-ACCOUNT.
           EXEC CICS READ
              FILE('ACCTDAT')
              INTO(WS-ACCOUNT-DATA)
              RIDFLD(WS-COMM-ACCOUNT-ID)
              RESP(WS-RESP-CODE)
              RESP2(WS-RESP2-CODE)
           END-EXEC
           
           EVALUATE WS-RESP-CODE
              WHEN DFHRESP(NORMAL)
                 SET COMM-SUCCESS TO TRUE
                 PERFORM 3100-CALCULATE-AVAILABLE-CREDIT
                 PERFORM 3200-FORMAT-DISPLAY-FIELDS
              WHEN DFHRESP(NOTFND)
                 SET COMM-ACCT-NOT-FOUND TO TRUE
                 MOVE ERR-ACCT-NOT-FOUND TO WS-COMM-MESSAGE
              WHEN OTHER
                 SET COMM-DB-ERROR TO TRUE
                 MOVE ERR-DB-ERROR TO WS-COMM-MESSAGE
                 PERFORM 9500-LOG-ERROR
           END-EVALUATE.
      
       3100-CALCULATE-AVAILABLE-CREDIT.
      *    Available credit = Credit Limit - Current Balance
      *    But cannot be negative
           COMPUTE WS-ACCT-AVAILABLE-CREDIT = 
              WS-ACCT-CREDIT-LIMIT - WS-ACCT-CURRENT-BAL
           END-COMPUTE
           
           IF WS-ACCT-AVAILABLE-CREDIT < 0
              MOVE 0 TO WS-ACCT-AVAILABLE-CREDIT
           END-IF.
      
       3200-FORMAT-DISPLAY-FIELDS.
           MOVE WS-ACCT-CREDIT-LIMIT TO WS-DISP-CREDIT-LIMIT
           MOVE WS-ACCT-CURRENT-BAL TO WS-DISP-CURRENT-BAL
           MOVE WS-ACCT-AVAILABLE-CREDIT TO WS-DISP-AVAILABLE
           
           EVALUATE WS-ACCT-STATUS
              WHEN 'A'
                 MOVE 'ACTIVE' TO WS-DISP-STATUS
              WHEN 'C'
                 MOVE 'CLOSED' TO WS-DISP-STATUS
              WHEN 'S'
                 MOVE 'SUSPENDED' TO WS-DISP-STATUS
              WHEN 'D'
                 MOVE 'DELINQUENT' TO WS-DISP-STATUS
              WHEN OTHER
                 MOVE 'UNKNOWN' TO WS-DISP-STATUS
           END-EVALUATE.
      
       4000-SEND-RESPONSE.
           IF COMM-SUCCESS
              EXEC CICS SEND
                 MAP('ACTVWM')
                 MAPSET('ACTVWS')
                 FROM(WS-ACCOUNT-DATA)
                 DATAONLY
                 CURSOR
              END-EXEC
           ELSE
              EXEC CICS SEND
                 MAP('ACTVWM')
                 MAPSET('ACTVWS')
                 FROM(WS-COMMAREA)
                 DATAONLY
                 CURSOR
                 ALARM
              END-EXEC
           END-IF.
      
       8000-INVALID-KEY.
           MOVE 'INVALID KEY PRESSED - USE ENTER OR PF3' 
              TO WS-COMM-MESSAGE
           PERFORM 4000-SEND-RESPONSE.
      
       9000-RETURN-TO-MENU.
           EXEC CICS XCTL
              PROGRAM('COMEN01C')
           END-EXEC.
      
       9500-LOG-ERROR.
      *    Write error to transient data queue for audit
           EXEC CICS WRITEQ TD
              QUEUE('ERRLOG')
              FROM(WS-COMMAREA)
              LENGTH(LENGTH OF WS-COMMAREA)
           END-EXEC.
