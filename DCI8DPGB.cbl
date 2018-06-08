       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DPGB.
       AUTHOR. AMAR AL-ADIL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.

       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY BROWSE MAP LAYOUT
       COPY 'DCI8DMB'.

      * COPY ACCTFILE RECORD LAYOUT

       COPY 'ACCTREC'.

       01 WS-RESPONSE            PIC S9(4) COMP.

       01 SCREEN-RECORD.
           05 SR-LINE-NO         PIC 9(2).
           05 FILLER             PIC X(15) VALUE SPACES.
           05 SR-ACCTNO          PIC X(5).
           05 FILLER             PIC X(8) VALUE SPACES.
           05 SR-FIRST-NAME      PIC X(15).
           05 FILLER             PIC X(5) VALUE SPACES.
           05 SR-SURNAME         PIC X(15).

       01 SCREEN-EOF.
           05 EOF-LINE-NO        PIC 9(2).
           05 FILLER             PIC X(16) VALUE SPACES.
           05 EOF-MESSAGE        PIC X(40).

       01 WS-MESSAGE             PIC X(60).
       01 SR-ACCT-NAME           PIC X(15).

       77 WS-LINE-SUB            PIC 9(2).
       77 WS-LINES               PIC 9(2) VALUE 10.

       77 WS-EOF-RESP            PIC X VALUE "N".

       77 WS-LOOP-COUNT          PIC 9(2) VALUE 0.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(3).

       PROCEDURE DIVISION.

       000-START-LOGIC.

           IF EIBCALEN = 3 THEN
                GO TO 100-FIRST-TIME
           END-IF.

           EXEC CICS HANDLE AID
                PF2(100-FIRST-TIME)
                PF8(210-SCROLL-FWD)
                PF7(220-SCROLL-BACK)
                PF9(999-EXIT-RETURN)
           END-EXEC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * CONDITION FOR RECORD NOT FOUND IN ACCTFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                NOTFND(300-ACCTNAME-NOTFND)
           END-EXEC.

      * HANDLE dupkey
           EXEC CICS IGNORE CONDITION
                DUPKEY
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCI8DMB')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE "PLEASE ENTER AN ACCOUNT #" TO WS-MESSAGE.
           GO TO 999-SEARCH-RETURN.

       200-MAIN-LOGIC.

      * browse logic
           MOVE SCHNAMEI TO SNAME.

           EXEC CICS STARTBR
                FILE('ACCTNAME')
                RIDFLD(SNAME)
           END-EXEC.

      * browse forward 10 lines and display them on the map
           MOVE LOW-VALUES TO MAP1O.
           MOVE LOW-VALUES TO WS-MESSAGE.

           MOVE 1 TO WS-LINE-SUB.

           PERFORM 250-BRWS-FORWARD
                VARYING WS-LINE-SUB
                FROM 1 BY 1
                UNTIL WS-LINE-SUB > WS-LINES.

           EXEC CICS ENDBR
                FILE('ACCTNAME')
           END-EXEC.

           GO TO 305-NORMAL-BROWSE.

       210-SCROLL-FWD.

           MOVE LINEI(10) TO SCREEN-RECORD.

           MOVE SR-SURNAME TO SNAME.

           IF (SNAME EQUAL SPACES OR SNAME EQUAL LOW-VALUES)
                GO TO 310-NO-FWD
           END-IF.

           EXEC CICS STARTBR
                FILE('ACCTNAME')
                RIDFLD(SNAME)
           END-EXEC.

           MOVE LOW-VALUES TO MAP1O.
           MOVE LOW-VALUES TO WS-MESSAGE.

           MOVE 1 TO WS-LINE-SUB.

           PERFORM 250-BRWS-FORWARD
                VARYING WS-LINE-SUB
                FROM 1 BY 1
                UNTIL WS-LINE-SUB > WS-LINES.

           EXEC CICS ENDBR
                FILE('ACCTNAME')
           END-EXEC.

           GO TO 305-NORMAL-BROWSE.

       220-SCROLL-BACK.

           MOVE LINEI(1) TO SCREEN-RECORD.

           MOVE SR-SURNAME TO SNAME.

           IF (SNAME EQUAL SPACES OR SNAME EQUAL LOW-VALUES)
                GO TO 320-NO-BACK
           END-IF.

           EXEC CICS STARTBR
                FILE('ACCTNAME')
                RIDFLD(SNAME)
           END-EXEC.

           MOVE LOW-VALUES TO MAP1O.
           MOVE LOW-VALUES TO WS-MESSAGE.

           MOVE 1 TO WS-LINE-SUB.

           PERFORM 260-BRWS-BACK
                VARYING WS-LINE-SUB
                FROM 10 BY -1
                UNTIL WS-LINE-SUB <= 0.

           EXEC CICS ENDBR
                FILE('ACCTNAME')
           END-EXEC.


           GO TO 305-NORMAL-BROWSE.

       250-BRWS-FORWARD.

           MOVE SPACES TO SCREEN-RECORD.
           MOVE WS-LINE-SUB TO SR-LINE-NO.

           EXEC CICS READNEXT
                FILE('ACCTNAME')
                INTO(ACCTREC)
                RIDFLD(SNAME)
                LENGTH(ACCTREC-LEN)
                RESP(WS-RESPONSE)
           END-EXEC.

           IF WS-RESPONSE = DFHRESP(ENDFILE) AND WS-EOF-RESP = "N" THEN
      *         END OF FILE, NO DATA FOUND
                MOVE 'Y' TO WS-EOF-RESP
                MOVE WS-LINE-SUB TO EOF-LINE-NO
                MOVE "======END OF FILE======"
                TO EOF-MESSAGE
                MOVE SCREEN-EOF TO LINEO(WS-LINE-SUB)
           ELSE IF WS-RESPONSE = DFHRESP(ENDFILE) THEN
      *         END OF FILE, NO DATA FOUND
                MOVE SCREEN-RECORD TO LINEO(WS-LINE-SUB)
           ELSE
      *         MOVE DATA TO THE SCREEN WHEN WE HAVE A READ LINE
                MOVE ACCTNO TO SR-ACCTNO
                MOVE FNAME TO SR-FIRST-NAME
                MOVE SNAME TO SR-SURNAME

                MOVE SCREEN-RECORD TO LINEO(WS-LINE-SUB)

               END-IF
           END-IF.


       260-BRWS-BACK.

           MOVE SPACES TO SCREEN-RECORD.
           MOVE WS-LINE-SUB TO SR-LINE-NO.

           EXEC CICS READPREV
                FILE('ACCTNAME')
                INTO(ACCTREC)
                RIDFLD(SNAME)
                LENGTH(ACCTREC-LEN)
                RESP(WS-RESPONSE)
           END-EXEC.

           IF WS-RESPONSE = DFHRESP(ENDFILE) AND WS-EOF-RESP = "N" THEN
      *         END OF FILE, NO DATA FOUND
                MOVE 'Y' TO WS-EOF-RESP
                MOVE WS-LINE-SUB TO EOF-LINE-NO
                MOVE "======TOP OF FILE======"
                TO EOF-MESSAGE
                MOVE SCREEN-EOF TO LINEO(WS-LINE-SUB)
           ELSE IF WS-RESPONSE = DFHRESP(ENDFILE) THEN
      *         END OF FILE, NO DATA FOUND
                MOVE SCREEN-RECORD TO LINEO(WS-LINE-SUB)
           ELSE
      *         MOVE DATA TO THE SCREEN WHEN WE HAVE A READ LINE
                MOVE ACCTNO TO SR-ACCTNO
                MOVE FNAME TO SR-FIRST-NAME
                MOVE SNAME TO SR-SURNAME

                MOVE SCREEN-RECORD TO LINEO(WS-LINE-SUB)

                END-IF
           END-IF.


       300-ACCTNAME-NOTFND.

           MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE.
           GO TO 999-ERROR-MSG-RETURN.

       305-NORMAL-BROWSE.

           MOVE 'BROWSE ACCOUNTS WITH PF KEYS LISTED BELOW'
                TO WS-MESSAGE.
           GO TO 999-BROWSE-RETURN.

       310-NO-FWD.
           MOVE '==== END OF FILE ALREADY REACHED ====' TO WS-MESSAGE.
           GO TO 999-BROWSE-RETURN.

       320-NO-BACK.
           MOVE '==== TOP OF FILE ALREADY REACHED ====' TO WS-MESSAGE.
           GO TO 999-BROWSE-RETURN.

       999-SEARCH-RETURN.

           MOVE LOW-VALUES TO MAP1O.
           MOVE WS-MESSAGE TO MSGO.

      * TODO: ADD CODE TO MAKE SEARCH FIELD EDITABLE IF NECESARY!
           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMB')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D4')
           END-EXEC.

       999-BROWSE-RETURN.

           MOVE WS-MESSAGE TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMB')

           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D4')
           END-EXEC.

       999-ERROR-MSG-RETURN.

           MOVE LOW-VALUES TO MAP1O.
           MOVE WS-MESSAGE TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMB')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D4')
           END-EXEC.

       999-EXIT-RETURN.
           MOVE LOW-VALUES TO MAP1O.
           MOVE 'PROGRAM ENDING' TO MSGO.
           EXEC CICS SEND MAP('MAP1') MAPSET('DCI8DMB') END-EXEC.
           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DPGB.
