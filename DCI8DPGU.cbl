       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DPGU.
       AUTHOR. AMAR AL-ADIL.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY INQUIRY MAP LAYOUT
       COPY 'DCI8DMR'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'ACCTREC'.

       01 WS-MESSAGE  PIC X(60) VALUE SPACES.

       01 WS-COMMAREA.
           05 WS-CA-SWITCH     PIC X.
           05 WS-CA-FNAME      PIC X(15).
           05 WS-CA-SNAME      PIC x(15).
           05 WS-CA-TITL       PIC X(4).
           05 WS-CA-ADDR1      PIC X(20).
           05 WS-CA-ADDR2      PIC X(20).
           05 WS-CA-CRLIMIT    PIC 9(8).
           05 WS-CA-STAT       PIC X.

       01 WS-COMMAREA-LENGTH PIC S9(4) COMP
           VALUE 84.

       01 FILLER            PIC X(1024)
           VALUE SPACES.

       01 WS-ACCTDATA.
       COPY 'ACCTDATA'.

       COPY 'DFHBMSCA'.



       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X(84).

       PROCEDURE DIVISION.

       000-START-LOGIC.

           IF EIBCALEN = 3 THEN
                GO TO 100-FIRST-TIME
           END-IF.

           EXEC CICS HANDLE AID
                PF4(650-INQUIRY-RESET)
                PF9(900-END-PROGRAM)
           END-EXEC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * TODO: ADD CONDITION FOR RECORD NOT FOUND IN ACCTFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                NOTFND(600-ACCTFILE-NOTFND)
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCI8DMR')
           END-EXEC.
           MOVE LOW-VALUES TO WS-COMMAREA.
           MOVE DFHCOMMAREA TO WS-COMMAREA.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO WS-COMMAREA.
           MOVE "I" TO WS-CA-SWITCH.
           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCI8DMR')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D3')
               COMMAREA(WS-COMMAREA)
               LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       200-MAIN-LOGIC.
      *     MOVE SPACES TO WS-MESSAGE MSGO.

           IF ACCTNOI = LOW-VALUES OR ACCTNOI = SPACES THEN
                MOVE "PLEASE ENTER A ACCOUNT #"
                TO WS-MESSAGE

                GO TO 999-ERROR-MSG-RETURN
           ELSE IF ACCTNOI = "XXXXX" THEN
                GO TO 900-END-PROGRAM
           ELSE IF WS-CA-SWITCH = "I" THEN
                GO TO 250-ACCT-INQUIRY
           ELSE IF WS-CA-SWITCH = "U" THEN
                IF (WS-CA-FNAME EQUAL FNAMEI AND
                    WS-CA-SNAME EQUAL SNAMEI AND
                    WS-CA-TITL EQUAL TITLI AND
                    WS-CA-ADDR1 EQUAL ADDR1I AND
                    WS-CA-ADDR2 EQUAL ADDR2I AND
                    WS-CA-CRLIMIT EQUAL CRLIMITI AND
                    WS-CA-STAT EQUAL STATI) THEN

                    MOVE LOW-VALUES TO WS-COMMAREA
                    MOVE "I" TO WS-CA-SWITCH

                    MOVE LOW-VALUES TO MAP1O
                    MOVE "NOTHING CHANGED - RETURNING TO INQUIRY MODE"
                        TO MSGO

                    EXEC CICS SEND
                        MAP('MAP1')
                        MAPSET('DCI8DMR')
                        ERASE
                    END-EXEC

                    EXEC CICS RETURN
                        TRANSID('I8D3')
                        COMMAREA(WS-COMMAREA)
                        LENGTH(WS-COMMAREA-LENGTH)
                    END-EXEC

                ELSE
                    GO TO 300-ACCT-UPDATE

                END-IF
           ELSE
                MOVE "UNKNOWN STATE - PLEASE EXIT AND TRY AGAIN"
                TO WS-MESSAGE

                GO TO 999-ERROR-MSG-RETURN
                        END-IF
                    END-IF
                END-IF
           END-IF.

       250-ACCT-INQUIRY.

           IF ACCTNOL < 5 THEN

                MOVE 'ACCOUNT NUMBERS MUST BE 5 NUMBERS LONG'
                TO WS-MESSAGE

                GO TO 999-ERROR-MSG-RETURN

           ELSE IF ACCTNOI IS NOT NUMERIC

                MOVE 'ACCOUNT NUMBERS MUST BE NUMERIC'
                TO WS-MESSAGE

                GO TO 999-ERROR-MSG-RETURN

           ELSE

      * TODO: ATTEMPT TO FIND AN ACCOUND RECORD IN ACCTFILE
      *       FROM USER INPUT ACCOUNTNO

                MOVE ACCTNOI TO ACCTNO
                EXEC CICS READ
                     FILE('ACCTFILE')
                     INTO(ACCTREC)
                     LENGTH(ACCTREC-LEN)
                     RIDFLD(ACCTKEY)
                END-EXEC



      * RECORD FOUND, MOVE VALUES TO MAP OUTPUTS
                PERFORM 350-PREP-UPDATE-MAP

                MOVE -1 TO TITLL

      * TODO: MOVE VALUES FROM ACCTREC TO O FIELDS
                MOVE ACCTNO TO ACCTNOO
                MOVE FNAME TO FNAMEO WS-CA-FNAME
                MOVE SNAME TO SNAMEO WS-CA-SNAME
                MOVE TITL TO TITLO WS-CA-TITL
                MOVE ADDR1 TO ADDR1O WS-CA-ADDR1
                MOVE ADDR2 TO ADDR2O WS-CA-ADDR2
                MOVE CRLIMIT TO CRLIMITO WS-CA-CRLIMIT
                MOVE STAT TO STATO WS-CA-STAT
                MOVE SPACES TO MSGO

                MOVE 'U' TO WS-CA-SWITCH

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCI8DMR')
                     CURSOR
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('I8D3')
                     COMMAREA(WS-COMMAREA)
                     LENGTH(WS-COMMAREA-LENGTH)
                END-EXEC

           END-IF.

       300-ACCT-UPDATE.

           MOVE LOW-VALUES TO AD-MESSAGE.
           MOVE LOW-VALUES TO WS-ACCTDATA.

           MOVE ACCTNOI TO AD-ACCTNO.
           MOVE 5 TO AD-ACCTNOL.
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

      *     MOVE M TO AD-MESSAGE.

           IF AD-MESSAGE IS NOT EQUAL LOW-VALUES THEN
                MOVE AD-MESSAGE TO WS-MESSAGE
                GO TO 370-UPDATE-ERROR
           ELSE

                MOVE ACCTNOI TO ACCTNO
                EXEC CICS READ
                    FILE('ACCTFILE')
                    INTO(ACCTREC)
                    LENGTH(ACCTREC-LEN)
                    RIDFLD(ACCTKEY)
                    UPDATE
                END-EXEC

                MOVE ACCTNOI TO ACCTNO
                MOVE FNAMEI TO FNAME
                MOVE SNAMEI TO SNAME
                MOVE TITLI TO TITL
                MOVE ADDR1I TO ADDR1
                MOVE ADDR2I TO ADDR2
                MOVE CRLIMITI TO CRLIMIT
                MOVE STATI TO STAT

                EXEC CICS REWRITE
                    FILE('ACCTFILE')
                    FROM(ACCTREC)
                    LENGTH(ACCTREC-LEN)
                END-EXEC

                MOVE LOW-VALUES TO WS-COMMAREA
                MOVE "I" TO WS-CA-SWITCH

                MOVE LOW-VALUES TO MAP1O
                MOVE "UPDATE SUCCESSFUL - PLEASE ENTER AN NEW ACCOUNT #"
                    TO MSGO

                EXEC CICS SEND
                    MAP('MAP1')
                    MAPSET('DCI8DMR')
                    ERASE
                END-EXEC

                EXEC CICS RETURN
                    TRANSID('I8D3')
                    COMMAREA(WS-COMMAREA)
                    LENGTH(WS-COMMAREA-LENGTH)
                END-EXEC

           END-IF.

       350-PREP-UPDATE-MAP.

           MOVE LOW-VALUES TO MAP1O.

      *     MOVE -1 TO TITLL.

           MOVE DFHBMASF TO ACCTNOA.

           MOVE DFHBMFSE TO TITLA FNAMEA SNAMEA ADDR1A ADDR2A
                STATA CRLIMITA.

       370-UPDATE-ERROR.

           PERFORM 350-PREP-UPDATE-MAP.
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
                        END-IF
                        END-IF
                    END-IF
                    END-IF
                END-IF
                END-IF
           END-IF
           END-IF.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMR')
                CURSOR
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('I8D3')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       600-ACCTFILE-NOTFND.

           MOVE 'ACCOUNT NOT FOUND' TO WS-MESSAGE.
           GO TO 999-ERROR-MSG-RETURN.

       650-INQUIRY-RESET.

           MOVE LOW-VALUES TO WS-COMMAREA.
           MOVE "I" TO WS-CA-SWITCH.

           MOVE LOW-VALUES TO MAP1O.
           MOVE "MENU RESET - INQUIRY MODE"
                TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMR')
                ERASE
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('I8D3')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       900-END-PROGRAM.

           MOVE LOW-VALUES TO MAP1O.
           MOVE "I" TO WS-CA-SWITCH.
           MOVE 'PROGRAM ENDING' TO MSGO.
           EXEC CICS SEND MAP('MAP1') MAPSET('DCI8DMR') END-EXEC.
           EXEC CICS RETURN END-EXEC.

       999-ERROR-MSG-RETURN.
           MOVE LOW-VALUES TO MAP1O.
           MOVE WS-MESSAGE TO MSGO.

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMR')

           END-EXEC.

           EXEC CICS RETURN
                TRANSID('I8D3')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       999-EXIT.

           EXEC CICS SEND CONTROL ERASE FREEKB END-EXEC.
           EXEC CICS RETURN END-EXEC.

       END PROGRAM DCI8DPGU.
