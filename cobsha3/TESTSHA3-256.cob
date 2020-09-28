 IDENTIFICATION DIVISION.
 PROGRAM-ID. TESTSHA3-256.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-STRING-1MB                      PIC X(1000000). 

*> input fields 
 01 WS-INPUT                           PIC X(200).
 01 WS-INPUT-BYTE-LEN                  BINARY-DOUBLE UNSIGNED.

*> output fields 
 01 WS-SHA3-224-OUTPUT                 PIC X(28).
 01 WS-SHA3-256-OUTPUT                 PIC X(32).
 01 WS-SHA3-384-OUTPUT                 PIC X(48).
 01 WS-SHA3-512-OUTPUT                 PIC X(64).
 01 WS-SHAKE128-OUTPUT                 PIC X(1024).
 01 WS-SHAKE128-OUTPUT-BYTE-LEN        BINARY-DOUBLE UNSIGNED.
 01 WS-SHAKE256-OUTPUT                 PIC X(1024).
 01 WS-SHAKE256-OUTPUT-BYTE-LEN        BINARY-DOUBLE UNSIGNED.

 *> interoperability fields
 01 WS-SHA3-256-JS-POINTER             PROGRAM-POINTER.
 01 WS-JS                              PIC X(3).
 01 WS-SHA3-256-JS-FILENAME            PIC X(50).
   
 PROCEDURE DIVISION.

*>------------------------------------------------------------------------------
 MAIN-TESTSHA3-256 SECTION.
*>------------------------------------------------------------------------------

    MOVE Z'js' TO WS-JS.
    MOVE Z'SHA3-256.js' TO WS-SHA3-256-JS-FILENAME.

    *> get javascript function
    CALL STATIC "polyglot_eval_file" using
              WS-JS
              WS-SHA3-256-JS-FILENAME
              returning WS-SHA3-256-JS-POINTER
    END-CALL

*>  SHA3-256 module test
*>  ====================    
*>  Test case 1 
    PERFORM TEST-SHA3-256-1
    
*>  Test case 2 
    PERFORM TEST-SHA3-256-2
 
*>  Test case 3 
    PERFORM TEST-SHA3-256-3

*>  Test case 4 
    PERFORM TEST-SHA3-256-4
    
*>  Test case 5 
    PERFORM TEST-SHA3-256-5
   
    STOP RUN
    
    .
 MAIN-TESTSHA3-256-EX.
    EXIT.

*>------------------------------------------------------------------------------
 TEST-SHA3-256-1 SECTION.
*>------------------------------------------------------------------------------

*>  Test case 1 
    INITIALIZE WS-SHA3-256-OUTPUT

    DISPLAY " " END-DISPLAY
    DISPLAY "-------------------------------------------" END-DISPLAY
    DISPLAY "SHA3-256 test case 1:" END-DISPLAY
    DISPLAY "Input message: ""abc"", the bit string (0x)616263"-
            " of length 24 bits."   END-DISPLAY   
    DISPLAY "Expected output:"      END-DISPLAY
    DISPLAY "3a985da74fe225b2 045c172d6bd390bd 855f086e3e9d525b "-
            "46bfe24511431532"      
    END-DISPLAY
            
    MOVE "abc" TO WS-INPUT
    MOVE 3     TO WS-INPUT-BYTE-LEN  
    
    CALL WS-SHA3-256-JS-POINTER USING WS-INPUT
                          WS-INPUT-BYTE-LEN
                          WS-SHA3-256-OUTPUT
    END-CALL

    IF WS-SHA3-256-OUTPUT =
       X"3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532"    
    THEN
       DISPLAY "SHA3-256 test case 1 passed."     END-DISPLAY
    ELSE
       DISPLAY "Error in SHA3-256 test case 1!!!" END-DISPLAY
    END-IF
    
    .
 TEST-SHA3-256-1-EX.
    EXIT.

*>------------------------------------------------------------------------------
 TEST-SHA3-256-2 SECTION.
*>------------------------------------------------------------------------------

*>  Test case 2 
    INITIALIZE WS-SHA3-256-OUTPUT

    DISPLAY " " END-DISPLAY
    DISPLAY "-------------------------------------------" END-DISPLAY
    DISPLAY "SHA3-256 test case 2:" END-DISPLAY
    DISPLAY "Input message: the empty string """", the bit string of length 0."
    END-DISPLAY   
    DISPLAY "Expected output:"      END-DISPLAY
    DISPLAY "a7ffc6f8bf1ed766 51c14756a061d662 f580ff4de43b49fa "-
            "82d80a4b80f8434a"      
    END-DISPLAY
            
    MOVE " " TO WS-INPUT
    MOVE 0   TO WS-INPUT-BYTE-LEN  
    
    CALL WS-SHA3-256-JS-POINTER USING WS-INPUT
                                      WS-INPUT-BYTE-LEN
                                      WS-SHA3-256-OUTPUT
    END-CALL

    IF WS-SHA3-256-OUTPUT =
       X"a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a"    
    THEN
       DISPLAY "SHA3-256 test case 2 passed."     END-DISPLAY
    ELSE
       DISPLAY "Error in SHA3-256 test case 2!!!" END-DISPLAY
    END-IF
    
    .
 TEST-SHA3-256-2-EX.
    EXIT.
    
*>------------------------------------------------------------------------------
 TEST-SHA3-256-3 SECTION.
*>------------------------------------------------------------------------------

*>  Test case 3 
    INITIALIZE WS-SHA3-256-OUTPUT

    DISPLAY " " END-DISPLAY
    DISPLAY "-------------------------------------------" END-DISPLAY
    DISPLAY "SHA3-256 test case 3:" END-DISPLAY
    DISPLAY "Input message: ""abcdbcdecdefdefgefghfghighijhijkijkljklmklmn"-
            "lmnomnopnopq"" (length 448 bits)." END-DISPLAY   
    DISPLAY "Expected output:"      END-DISPLAY
    DISPLAY "41c0dba2a9d62408 49100376a8235e2c 82e1b9998a999e21 "-
            "db32dd97496d3376"      
    END-DISPLAY
            
    MOVE "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq"
      TO WS-INPUT
    MOVE 56 TO WS-INPUT-BYTE-LEN  
    
    CALL WS-SHA3-256-JS-POINTER USING WS-INPUT
                          WS-INPUT-BYTE-LEN
                          WS-SHA3-256-OUTPUT
    END-CALL

    IF WS-SHA3-256-OUTPUT =
       X"41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376"    
    THEN
       DISPLAY "SHA3-256 test case 3 passed."     END-DISPLAY
    ELSE
       DISPLAY "Error in SHA3-256 test case 3!!!" END-DISPLAY
    END-IF
    
    .
 TEST-SHA3-256-3-EX.
    EXIT.

*>------------------------------------------------------------------------------
 TEST-SHA3-256-4 SECTION.
*>------------------------------------------------------------------------------

*>  Test case 4 
    INITIALIZE WS-SHA3-256-OUTPUT

    DISPLAY " " END-DISPLAY
    DISPLAY "-------------------------------------------" END-DISPLAY
    DISPLAY "SHA3-256 test case 4:" END-DISPLAY
    DISPLAY "Input message:  ""abcdefghbcdefghicdefghijdefghijkefghijklfghi"-
            "jklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstno"-
            "pqrstu"" (length 896 bits)." END-DISPLAY   
    DISPLAY "Expected output:"      END-DISPLAY
    DISPLAY "916f6061fe879741 ca6469b43971dfdb 28b1a32dc36cb325 "-
            "4e812be27aad1d18"      
    END-DISPLAY
            
    MOVE "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoi"-
         "jklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"
      TO WS-INPUT
    MOVE 112 TO WS-INPUT-BYTE-LEN  
    
    CALL WS-SHA3-256-JS-POINTER USING WS-INPUT
                          WS-INPUT-BYTE-LEN
                          WS-SHA3-256-OUTPUT
    END-CALL

    IF WS-SHA3-256-OUTPUT =
       X"916f6061fe879741ca6469b43971dfdb28b1a32dc36cb3254e812be27aad1d18"    
    THEN
       DISPLAY "SHA3-256 test case 4 passed."     END-DISPLAY
    ELSE
       DISPLAY "Error in SHA3-256 test case 4!!!" END-DISPLAY
    END-IF
    
    .
 TEST-SHA3-256-4-EX.
    EXIT.

*>------------------------------------------------------------------------------
 TEST-SHA3-256-5 SECTION.
*>------------------------------------------------------------------------------

*>  Test case 5 
    INITIALIZE WS-SHA3-256-OUTPUT

    DISPLAY " " END-DISPLAY
    DISPLAY "-------------------------------------------" END-DISPLAY
    DISPLAY "SHA3-256 test case 5:" END-DISPLAY
    DISPLAY "Input message: one million (1,000,000) repetitions of the "- 
            "character ""a"" (0x61)." END-DISPLAY   
    DISPLAY "Expected output:"      END-DISPLAY
    DISPLAY "5c8875ae474a3634 ba4fd55ec85bffd6 61f32aca75c6d699 "-
            "d0cdcb6c115891c1"      
    END-DISPLAY
            
    MOVE ALL "a" TO WS-STRING-1MB
    MOVE 1000000 TO WS-INPUT-BYTE-LEN  
    
    CALL WS-SHA3-256-JS-POINTER USING WS-STRING-1MB
                          WS-INPUT-BYTE-LEN
                          WS-SHA3-256-OUTPUT
    END-CALL

    IF WS-SHA3-256-OUTPUT =
       X"5c8875ae474a3634ba4fd55ec85bffd661f32aca75c6d699d0cdcb6c115891c1"    
    THEN
       DISPLAY "SHA3-256 test case 5 passed."     END-DISPLAY
    ELSE
       DISPLAY "Error in SHA3-256 test case 5!!!" END-DISPLAY
    END-IF
    
    .
 TEST-SHA3-256-5-EX.
    EXIT.

*>------------------------------------------------------------------------------
 END PROGRAM TESTSHA3-256.
