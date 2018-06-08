       IDENTIFICATION DIVISION.
       PROGRAM-ID. DCI8DPGR.
       AUTHOR. ANDREW MAYNE.

       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

      * COPY INQUIRY MAP LAYOUT
       COPY 'DCI8DMR'.

      * COPY ACCTFILE RECORD LAYOUT
       COPY 'ACCTREC'.

       01 WS-MESSAGE  PIC X(60) VALUE SPACES.

       01 WS-COMMAREA           PIC X(3)
           VALUE SPACES.

       01 WS-COMMAREA-LENGTH    PIC S9(4) COMP
           VALUE 3.

       LINKAGE SECTION.

       01 DFHCOMMAREA PIC X.

       PROCEDURE DIVISION.

       000-START-LOGIC.

           IF EIBCALEN = 3 THEN
                GO TO 100-FIRST-TIME
           END-IF.

           EXEC CICS HANDLE AID
                PF4(150-RESET)
                PF9(999-EXIT)
           END-EXEC.

      * UPON UNSUCCESSFUL MAP RECEIVE, ASSUME THIS IS THE INITIAL RUN
      * TODO: ADD CONDITION FOR RECORD NOT FOUND IN ACCTFILE
           EXEC CICS HANDLE CONDITION
                MAPFAIL(100-FIRST-TIME)
                NOTFND(300-ACCTFILE-NOTFND)
           END-EXEC.

      * ATTEMPT TO RECEIVE MAP FROM TERMINAL
           EXEC CICS RECEIVE
                MAP('MAP1')
                MAPSET('DCI8DMR')
           END-EXEC.

      * RECEIVE WAS SUCCESSFUL, PROCEED WITH MAIN PROCESSING
           GO TO 200-MAIN-LOGIC.

       100-FIRST-TIME.

           MOVE LOW-VALUES TO MAP1O.

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCI8DMR')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D1')
           END-EXEC.

       150-RESET.

           MOVE LOW-VALUES TO MAP1O.

           MOVE 'MENU RESET'
                TO MSGO

           EXEC CICS SEND
               MAP('MAP1')
               MAPSET('DCI8DMR')
               ERASE
           END-EXEC.

           EXEC CICS RETURN
               TRANSID('I8D1')
           END-EXEC.

       200-MAIN-LOGIC.

      * TODO: IMPLEMENT VALIDATION LOGIC
           IF ACCTNOI EQUAL "XXXXX" THEN

                GO TO 999-EXIT

           ELSE IF ACCTNOL < 5 THEN

                MOVE LOW-VALUES TO MAP1O
                MOVE 'ACCOUNT NUMBERS MUST BE 5 NUMBERS LONG'
                TO MSGO

                MOVE ACCTNOI TO ACCTNOO

                MOVE SPACES TO FNAMEO
                MOVE SPACES TO SNAMEO
                MOVE SPACES TO TITLO
                MOVE SPACES TO ADDR1O
                MOVE SPACES TO ADDR2O
                MOVE SPACES TO CRLIMITO
                MOVE SPACES TO STATO

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCI8DMR')
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('I8D1')
                END-EXEC

           ELSE IF ACCTNOI IS NOT NUMERIC

                MOVE LOW-VALUES TO MAP1O
                MOVE 'ACCOUNT NUMBERS MUST BE NUMERIC'
                TO MSGO

                MOVE ACCTNOI TO ACCTNOO

                MOVE SPACES TO FNAMEO
                MOVE SPACES TO SNAMEO
                MOVE SPACES TO TITLO
                MOVE SPACES TO ADDR1O
                MOVE SPACES TO ADDR2O
                MOVE SPACES TO CRLIMITO
                MOVE SPACES TO STATO

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCI8DMR')
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('I8D1')
                END-EXEC

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
                MOVE LOW-VALUES TO MAP1O
                MOVE "RECORD FOUND!" TO MSGO

      * TODO: MOVE VALUES FROM ACCTREC TO O FIELDS

                MOVE ACCTNO TO ACCTNOO
                MOVE FNAME TO FNAMEO
                MOVE SNAME TO SNAMEO
                MOVE TITL TO TITLO
                MOVE ADDR1 TO ADDR1O
                MOVE ADDR2 TO ADDR2O
                MOVE CRLIMIT TO CRLIMITO
                MOVE STAT TO STATO

                EXEC CICS SEND
                     MAP('MAP1')
                     MAPSET('DCI8DMR')
                     ERASE
                END-EXEC

                EXEC CICS RETURN
                     TRANSID('I8D1')
                END-EXEC

                    END-IF
                END-IF
           END-IF.

       300-ACCTFILE-NOTFND.

           MOVE LOW-VALUES TO MAP1O.
           MOVE 'ACCOUNT NOT FOUND' TO MSGO.
           MOVE ACCTNO TO ACCTNOO.

           MOVE SPACES TO FNAMEO
           MOVE SPACES TO SNAMEO
           MOVE SPACES TO TITLO
           MOVE SPACES TO ADDR1O
           MOVE SPACES TO ADDR2O
           MOVE SPACES TO CRLIMITO
           MOVE SPACES TO STATO

           EXEC CICS SEND
                MAP('MAP1')
                MAPSET('DCI8DMR')
           END-EXEC.

           EXEC CICS RETURN
                TRANSID('I8D1')
           END-EXEC.

       999-EXIT.
           MOVE LOW-VALUES TO MAP1O.
           EXEC CICS XCTL
                PROGRAM('DCI8DPGM')
                COMMAREA(WS-COMMAREA)
                LENGTH(WS-COMMAREA-LENGTH)
           END-EXEC.

       END PROGRAM DCI8DPGR.
