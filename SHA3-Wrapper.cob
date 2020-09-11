 IDENTIFICATION DIVISION.
 PROGRAM-ID. SHA3-512-Wrapper.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 LNK-SHA3-512-INPUT                 POINTER.
 01 LNK-SHA3-512-OUTPUT                POINTER.

 LINKAGE SECTION.
 01 LNK-SHA3-512-INPUT-POLYGLOT        POINTER.
 01 LNK-SHA3-512-INPUT-BYTE-LEN        BINARY-DOUBLE UNSIGNED.
 01 LNK-SHA3-512-OUTPUT-POLYGLOT       POINTER.
 
 PROCEDURE DIVISION USING by reference LNK-SHA3-512-INPUT-POLYGLOT         
                          by value LNK-SHA3-512-INPUT-BYTE-LEN 
                          by reference LNK-SHA3-512-OUTPUT-POLYGLOT.         
 
*>------------------------------------------------------------------------------
 MAIN-SHA3-512-Wrapper SECTION.
*>------------------------------------------------------------------------------

    CALL "polyglot_as_i8_array" USING by reference LNK-SHA3-512-INPUT-POLYGLOT
                   returning LNK-SHA3-512-INPUT
    END-CALL

    CALL "polyglot_as_i8_array" USING by reference LNK-SHA3-512-OUTPUT-POLYGLOT
                   returning LNK-SHA3-512-OUTPUT
    END-CALL

    CALL "SHA3-256" USING by value LNK-SHA3-512-INPUT
                          by reference LNK-SHA3-512-INPUT-BYTE-LEN
                          by value LNK-SHA3-512-OUTPUT
    END-CALL
    GOBACK
    
    .
 MAIN-SHA3-512-EX.
    EXIT.
 END PROGRAM SHA3-512-Wrapper.
