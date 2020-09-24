// load C Code
var libcob = Polyglot.evalFile("llvm", "/usr/local/lib/libcob.so");
var cobol = Polyglot.evalFile("llvm", "KECCAK.so");

(function SHA3_256(sha_input, sha_input_length, sha_output) {

    var keccak_rate = 1088;
    var keccak_capacity = 512;
    var keccak_delimited_suffix = 0x06;
    var output_byte_len = 32;

    // inform GnUCOBOl how many arguments are used
    libcob.set_cob_call_parameters(7);

    cobol.KECCAK__Wrapper(
        keccak_rate,
        keccak_capacity,
        sha_input, 
        sha_input_length, 
        keccak_delimited_suffix,
        sha_output,
        output_byte_len);
    
    // GnuCobol expects a return value
    return 0;
})