 IDENTIFICATION DIVISION.
 PROGRAM-ID. SHA3-256-Wrapper.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 01 WS-SHA3-256-INPUT-BYTE-LEN        BINARY-DOUBLE UNSIGNED.
 01 WS-SHA3-256-OUTPUT                POINTER.
 LINKAGE SECTION.
 01 LNK-SHA3-256-INPUT                     PIC X ANY LENGTH.
 01 LNK-SHA3-256-INPUT-BYTE-LEN-POLYGLOT   POINTER.
 01 LNK-SHA3-256-OUTPUT-POLYGLOT           POINTER.
 
 PROCEDURE DIVISION USING by reference LNK-SHA3-256-INPUT         
                          by reference LNK-SHA3-256-INPUT-BYTE-LEN-POLYGLOT 
                          by reference LNK-SHA3-256-OUTPUT-POLYGLOT.         
 
 MAIN-SHA3-256-Wrapper SECTION.

    CALL "polyglot_as_i64" USING by reference LNK-SHA3-256-INPUT-BYTE-LEN-POLYGLOT
                   returning WS-SHA3-256-INPUT-BYTE-LEN
    END-CALL

    CALL "polyglot_as_i8_array" USING by reference LNK-SHA3-256-OUTPUT-POLYGLOT
                   returning WS-SHA3-256-OUTPUT
    END-CALL

    CALL "SHA3-256" USING by reference LNK-SHA3-256-INPUT
                          by reference WS-SHA3-256-INPUT-BYTE-LEN
                          by value WS-SHA3-256-OUTPUT
    END-CALL
    GOBACK
    
    .
 MAIN-SHA3-256-EX.
    EXIT.
 END PROGRAM SHA3-256-Wrapper.
*>------------------------------------------------------------------------------

*>------------------------------------------------------------------------------
IDENTIFICATION DIVISION.
 PROGRAM-ID. SHA3-256-Wrapper-js-object.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 LINKAGE SECTION.
 01 LNK-SHA3-DATA.
        02 SHA3-256-INPUT        POINTER.
        02 SHA3-256-INPUT-BYTE-LEN   BINARY-DOUBLE UNSIGNED.
        02 SHA3-256-OUTPUT       PIC X(32).
 
 PROCEDURE DIVISION USING by reference LNK-SHA3-DATA.         
 
 MAIN-SHA3-256-Wrapper-js-object SECTION.

    CALL "SHA3-256" USING by value SHA3-256-INPUT
                          by reference SHA3-256-INPUT-BYTE-LEN
                          by reference SHA3-256-OUTPUT
    END-CALL
    GOBACK
    
    .
 MAIN-SHA3-256-EX.
    EXIT.
 END PROGRAM SHA3-256-Wrapper-js-object.
*>------------------------------------------------------------------------------

*>------------------------------------------------------------------------------
IDENTIFICATION DIVISION.
 PROGRAM-ID. SHA3-256-Wrapper-native.

 ENVIRONMENT DIVISION.

 DATA DIVISION.
 WORKING-STORAGE SECTION.
 LINKAGE SECTION.
 01 LNK-SHA3-256-INPUT            PIC X ANY LENGTH.
 01 LNK-SHA3-256-INPUT-BYTE-LEN   BINARY-DOUBLE UNSIGNED.
 01 LNK-SHA3-256-OUTPUT           PIC X ANY LENGTH.
 
 PROCEDURE DIVISION USING by reference LNK-SHA3-256-INPUT         
                          by value LNK-SHA3-256-INPUT-BYTE-LEN
                          by reference LNK-SHA3-256-OUTPUT.         

 MAIN-SHA3-256-Wrapper-native SECTION.


    CALL "SHA3-256" USING by reference LNK-SHA3-256-INPUT
                          by reference LNK-SHA3-256-INPUT-BYTE-LEN
                          by reference LNK-SHA3-256-OUTPUT
    END-CALL
    GOBACK
    
    .
 MAIN-SHA3-256-EX.
    EXIT.
 END PROGRAM SHA3-256-Wrapper-native.