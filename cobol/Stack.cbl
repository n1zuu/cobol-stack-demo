      ******************************************************************
      * Author: Tapdasan, Vince Jevy
      * Date: November 29, 2025
      * Purpose: To demonstrate the program structure of a stack in COBOL
      * Tectonics: cobc
      ******************************************************************
       IDENTIFICATION DIVISION.
       PROGRAM-ID. STACK.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       77 STACK-MAX      PIC 9(3) VALUE 10.
       77 STACK-TOP      PIC 9(3) VALUE 0.
       01 STACK-ARR.
       05 STACK-ELEM OCCURS 10 TIMES
                         PIC X(20).

       LINKAGE SECTION.
       01 LK-OPERATION   PIC X(10).   *> stack operations
       01 LK-DATA        PIC X(20).   *> input/output element
       01 LK-STATUS      PIC X(20).   *> return status

       PROCEDURE DIVISION USING LK-OPERATION LK-DATA LK-STATUS.
           EVALUATE LK-OPERATION
               WHEN "PUSH"
                   PERFORM DO-PUSH
               WHEN "POP"
                   PERFORM DO-POP
               WHEN "PEEK"
                   PERFORM DO-PEEK
               WHEN OTHER
                   MOVE "INVALID OP" TO LK-STATUS
           END-EVALUATE
       EXIT PROGRAM.
       STOP RUN.

       DO-PUSH.
           IF STACK-TOP = STACK-MAX
               MOVE "STACK FULL" TO LK-STATUS
               EXIT PARAGRAPH
           END-IF

           ADD 1 TO STACK-TOP
           MOVE LK-DATA TO STACK-ELEM(STACK-TOP)
           MOVE "OK" TO LK-STATUS
           .

       DO-POP.
           IF STACK-TOP = 0
               MOVE "STACK EMPTY" TO LK-STATUS
               MOVE SPACES TO LK-DATA
               EXIT PARAGRAPH
           END-IF

           MOVE STACK-ELEM(STACK-TOP) TO LK-DATA
           SUBTRACT 1 FROM STACK-TOP
           MOVE "OK" TO LK-STATUS
           .

       DO-PEEK.
           IF STACK-TOP = 0
               MOVE "STACK EMPTY" TO LK-STATUS
               MOVE SPACES TO LK-DATA
               EXIT PARAGRAPH
           END-IF

           MOVE STACK-ELEM(STACK-TOP) TO LK-DATA
           MOVE "OK" TO LK-STATUS
           .
