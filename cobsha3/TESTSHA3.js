var libcob = Polyglot.evalFile("llvm", "/usr/local/lib/libcob.so")
var init = Polyglot.evalFile("llvm", "init.so")
var cobol = Polyglot.evalFile("llvm", "SHA3-256_STATIC.so")
var interface = Polyglot.evalFile("llvm", "interface.so")

console.log("Initialize GnuCobol runtime library")
//libcob.cob_init(0, 0)
init.init() // Needed for native mode

/* Helper functions to convert between strings and arrays */
function toHexString(byteArray) {
    return Array.prototype.map.call(byteArray, function(byte) {
      return ('0' + (byte & 0xFF).toString(16)).slice(-2);
    }).join('');
}

function toUInt8Array(string) {
    var tmpArray = string.split('').map(character => {
        return character.codePointAt(0)
    })
    return Uint8Array.from(tmpArray)
}

var test_cases = [
    "abc", 
    "", 
    "abcdbcdecdefdefgefghfghighijhijkijkljklmklmnlmnomnopnopq",
    "abcdefghbcdefghicdefghijdefghijkefghijklfghijklmghijklmnhijklmnoijklmnopjklmnopqklmnopqrlmnopqrsmnopqrstnopqrstu"/* ,
    "a".repeat(1000000) */];

var test_cases_info = [
    test_cases[0],
    test_cases[1],
    test_cases[2],
    test_cases[3],
    "\"a\" repeated 1 million times"
];

var expected_output = [
    "3a985da74fe225b2045c172d6bd390bd855f086e3e9d525b46bfe24511431532",
    "a7ffc6f8bf1ed76651c14756a061d662f580ff4de43b49fa82d80a4b80f8434a",
    "41c0dba2a9d6240849100376a8235e2c82e1b9998a999e21db32dd97496d3376",
    "916f6061fe879741ca6469b43971dfdb28b1a32dc36cb3254e812be27aad1d18",
    "5c8875ae474a3634ba4fd55ec85bffd661f32aca75c6d699d0cdcb6c115891c1"]

var sha_output_length = 32; // for SHA3-256: 256 bits = 32 bytes


console.log("Call Cobol wrapper with separate parameters")
for (i = 0; i < test_cases.length; i++) {
    console.log("Test case: ", test_cases_info[i])
    sha_input = toUInt8Array(test_cases[i])
    sha_input_length = sha_input.length
    sha_output = new Uint8Array(sha_output_length);

    cobol.SHA3__256__Wrapper(sha_input, sha_input_length, sha_output);

    var hexString = toHexString(sha_output);
    console.log("Expected Output: ", expected_output[i])
    console.log("Actual Output  : ", hexString);
    if(expected_output[i] == hexString){
        console.log("Test passed successfully")
    } else {
        console.log("Test failed")
    }
}

console.log("Call Cobol function with a single js object")
for (i = 0; i < test_cases.length; i++) {
    console.log("Test case: ", test_cases_info[i])
    var js_object = {
        SHA3_256_INPUT: toUInt8Array(test_cases[i]), 
        SHA3_256_INPUT_BYTE_LEN: test_cases[i].length, 
        SHA3_256_OUTPUT: new Uint8Array(sha_output_length)};

    cobol.SHA3__256__Wrapper__js__object(js_object);

    var hexString = toHexString(js_object.SHA3_256_OUTPUT);
    console.log("Expected Output: ", expected_output[i])
    console.log("Actual Output  : ", hexString);
    if(expected_output[i] == hexString){
        console.log("Test passed successfully")
    } else {
        console.log("Test failed")
    }
}

console.log("Call Cobol wrapper with C allocated memory")
for (i = 0; i < test_cases.length; i++) {
    console.log("Test case: ", test_cases_info[i])
    sha_input_length = test_cases[i].length
    sha_input = interface.allocate_character_array(sha_input_length);
    for(j = 0; j < sha_input_length; j++) {
        sha_input[j] = test_cases[i][j]
    }
    sha_output = interface.allocate_character_array(32);

    cobol.SHA3__256__Wrapper__native(sha_input, sha_input_length, sha_output);

    var hexString = toHexString(sha_output);
    console.log("Expected Output: ", expected_output[i])
    console.log("Actual Output  : ", hexString);
    if(expected_output[i] == hexString){
        console.log("Test passed successfully")
    } else {
        console.log("Test failed")
    }
    interface.free_mem(sha_input);
    interface.free_mem(sha_output); 
}