// load C Code
// var libcob = Polyglot.evalFile("llvm", "/home/christoph/Sulong-Cobol/bitcode-managed/lib/libcob.so");
var cobol = Polyglot.evalFile("llvm", "KECCAK.so");

(function SHA3_256(sha_input, sha_input_length, sha_output) {

    var keccak_rate = 1088;
    var keccak_capacity = 512;
    var keccak_delimited_suffix = 0x06;
    var output_byte_len = 32;

    cobol.KECCAK__Wrapper__struct({
        LNK_KECCAK_RATE: keccak_rate,
        LNK_KECCAK_CAPACITY: keccak_capacity,
        LNK_KECCAK_INPUT: sha_input,
        LNK_KECCAK_INPUT_BYTE_LEN: sha_input_length,
        LNK_KECCAK_DELIMITED_SUFFIX: keccak_delimited_suffix,
        LNK_KECCAK_OUTPUT: sha_output,
        LNK_KECCAK_OUTPUT_BYTE_LEN: output_byte_len
    }); 

/*     
    // inform GnuCOBOl how many arguments are used
    libcob.set_cob_call_parameters(7);

    cobol.KECCAK__Wrapper(
        keccak_rate,
        keccak_capacity,
        sha_input, 
        sha_input_length, 
        keccak_delimited_suffix,
        sha_output,
        output_byte_len);
     */
    
    // GnuCobol expects a return value
    return 0;
})
