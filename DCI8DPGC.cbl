       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DPGC.
       AUTHOR. ANDREW MAYNE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY INQUIRY MAP LAYOUT
       COPY 'DCI8DMC'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'ACCTREC'.

       01 WS-MESSAGE  PIC X(60) VALUE SPACES.

       01 WS-ACCTDATA.
       COPY 'ACCTDATA'.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X.

       PROCEDURE DIVISION.

       000-START-LOGIC.

           IF EIBCALEN = 3 THEN
                GO TO 100-FIRST-TIME
           END-IF.

           EXEC CICS HANDLE AID
                PF9(999-EXIT)
           END-EXEC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * TODO: ADD CONDITION FOR RECORD NOT FOUND IN ACCTFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                DUPREC(300-ACCTFILE-DUPREC)
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCI8DMC')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCI8DMC')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D2')
           END-EXEC.

       200-MAIN-LOGIC.

           IF ACCTNOI EQUAL "XXXXX" THEN
                GO TO 999-EXIT
           END-IF.

           MOVE LOW-VALUES TO AD-MESSAGE.
           MOVE ACCTNOI TO AD-ACCTNO.
           MOVE ACCTNOL TO AD-ACCTNOL.
           MOVE TITLI TO AD-TITLE.
           MOVE TITLL TO AD-TITLEL.
           MOVE FNAMEI TO AD-FNAME.
           MOVE FNAMEL TO AD-FNAMEL.
           MOVE SNAMEI TO AD-SNAME.
           MOVE SNAMEL TO AD-SNAMEL.
           MOVE ADDR1I TO AD-ADDR1.
           MOVE ADDR1L TO AD-ADDR1L.
           MOVE ADDR2I TO AD-ADDR2.
           MOVE ADDR2L TO AD-ADDR2L.
           MOVE STATI TO AD-STAT.
           MOVE STATL TO AD-STATL.
           MOVE CRLIMITI TO AD-CRLIMIT.
           MOVE CRLIMITL TO AD-CRLIMITL.

           EXEC CICS LINK
                PROGRAM('DCI8DPGE')
                COMMAREA(WS-ACCTDATA)
                LENGTH(AD-LENGTH)
           END-EXEC.

           IF AD-MESSAGE IS NOT EQUAL LOW-VALUES THEN
                MOVE AD-MESSAGE TO WS-MESSAGE
                GO TO 999-ERROR-MSG-RETURN
           ELSE

                MOVE ACCTNOI TO ACCTNO
                MOVE FNAMEI TO FNAME
                MOVE SNAMEI TO SNAME
                MOVE TITLI TO TITL
                MOVE ADDR1I TO ADDR1
                MOVE ADDR2I TO ADDR2
                MOVE CRLIMITI TO CRLIMIT
                MOVE STATI TO STAT


                EXEC CICS WRITE
                    FILE('ACCTFILE')
                    FROM(ACCTREC)
                    LENGTH(ACCTREC-LEN)
                    RIDFLD(ACCTKEY)
                END-EXEC

                MOVE LOW-VALUES TO MAP1O
                MOVE "ACCOUNT SUCCESSFULLY WRITTEN!" TO MSGO

                 EXEC CICS SEND
                    MAP('MAP1')
                    MAPSET('DCI8DMC')
                    ERASE
                END-EXEC

                EXEC CICS RETURN
                    TRANSID('I8D2')
                END-EXEC

           END-IF.


       300-ACCTFILE-DUPREC.
           MOVE -1 TO AD-ACCTNOL
           MOVE 'ACCOUNT ALREADY EXISTS, PLEASE ENTER ANOTHER ACCTNO'
               TO WS-MESSAGE.
           GO TO 999-ERROR-MSG-RETURN.

       999-ERROR-MSG-RETURN.
           MOVE LOW-VALUES TO MAP1O.
           MOVE WS-MESSAGE TO MSGO.


           IF AD-ACCTNOL = -1 THEN
                MOVE -1 TO ACCTNOL
           ELSE IF AD-FNAMEL = -1 THEN
                MOVE -1 TO FNAMEL
           ELSE IF AD-SNAMEL = -1 THEN
                MOVE -1 TO SNAMEL
           ELSE IF AD-TITLEL = -1 THEN
                MOVE -1 TO TITLL
           ELSE IF AD-ADDR1L = -1 THEN
                MOVE -1 TO ADDR1L
           ELSE IF AD-ADDR2L = -1 THEN
                MOVE -1 TO ADDR2L
           ELSE IF AD-STATL = -1 THEN
                MOVE -1 TO STATL
           ELSE IF AD-CRLIMITL = -1 THEN
                MOVE -1 TO CRLIMITL

           END-IF.


           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMC')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('I8D2')
           END-EXEC.

       999-EXIT.
           MOVE LOW-VALUES TO MAP1O.
           MOVE 'PROGRAM ENDING' TO MSGO.
           EXEC CICS SEND MAP('MAP1') MAPSET('DCI8DMC') END-EXEC.
           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DPGC.
