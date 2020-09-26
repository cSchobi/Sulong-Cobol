COBCOPTS = -free -Q "-Wl,-rpath -Wl,$(MANAGED_LIBRARIES_PATH)/lib"

all:	SHAKE128.so SHAKE256.so SHA3-224.so \
        SHA3-256.so SHA3-384.so SHA3-512.so KECCAK.o \
        TESTSHA3 SHA3-512.o   SESSION-ID.o TEST-SESSION-ID \
        SHA3-256.o   SESSION-ID-256.o TEST-SESSION-ID-256

interface.so: interface.c
	cobc $(COBCOPTS) $<

init.so: init.c
	cobc $(COBCOPTS) $<

# SESSION-ID        
TEST-SESSION-ID:	TEST-SESSION-ID.cob
	cobc $(COBCOPTS) -x  TEST-SESSION-ID.cob

SESSION-ID.o:	SESSION-ID.cob SHA3-512.o KECCAK.o
	cobc $(COBCOPTS) -m  SESSION-ID.cob SHA3-512.o KECCAK.o
    
SHA3-512.o:	SHA3-512.cob
	cobc $(COBCOPTS) -c  SHA3-512.cob

# SESSION-ID-256
TEST-SESSION-ID-256:	TEST-SESSION-ID-256.cob
	cobc $(COBCOPTS) -x  TEST-SESSION-ID-256.cob

SESSION-ID-256.o:	SESSION-ID-256.cob SHA3-256.o KECCAK.o
	cobc $(COBCOPTS) -m  SESSION-ID-256.cob SHA3-256.o KECCAK.o
    
SHA3-256.o:	SHA3-256.cob
	cobc $(COBCOPTS) -c  SHA3-256.cob
    
# SHA3    

# create dynamic library that can by loaded by Sulong
KECCAK.so: KECCAK.cob
	cobc $(COBCOPTS) -m -fstatic-call -fno-gen-c-decl-static-call -finclude-polyglot KECCAK.cob

TESTSHA3-256: TESTSHA3-256.cob SHA3-256.js KECCAK.so
	cobc $(COBCOPTS) -x TESTSHA3-256.cob -lpolyglot-mock

# do not generate C declaration of functions because of error (redeclaration) with polyglot library
SHA3-256_STATIC.so:  SHA3-Wrapper.cob SHA3-256.so KECCAK.o
	cobc $(COBCOPTS) -b -fstatic-call -fno-gen-c-decl-static-call -G SHA3-256-Wrapper-js-object -lpolyglot-mock SHA3-Wrapper.cob SHA3-256.so KECCAK.o -o $@

TESTSHA3_STATIC: TESTSHA3.cob KECCAK.cob SHA3-224.cob SHA3-256.cob SHA3-384.cob SHA3-512.cob SHAKE128.cob SHAKE256.cob
	cobc $(COBCOPTS) -x -fstatic-call -lpolyglot-mock TESTSHA3.cob KECCAK.cob SHA3-224.cob SHA3-256.cob SHA3-384.cob SHA3-512.cob SHAKE128.cob SHAKE256.cob -o $@

TESTSHA3:	TESTSHA3.cob
	cobc $(COBCOPTS) -x  TESTSHA3.cob
    
SHAKE128.so:	SHAKE128.cob KECCAK.o
	cobc $(COBCOPTS) -m  SHAKE128.cob KECCAK.o
    
SHAKE256.so:	SHAKE256.cob KECCAK.o
	cobc $(COBCOPTS) -m  SHAKE256.cob KECCAK.o
    
SHA3-224.so:	SHA3-224.cob KECCAK.o
	cobc $(COBCOPTS) -m  SHA3-224.cob KECCAK.o

SHA3-256.so:	SHA3-256.cob KECCAK.o
	cobc $(COBCOPTS) -m  -fstatic-call SHA3-256.cob KECCAK.o
    
SHA3-384.so:	SHA3-384.cob KECCAK.o
	cobc $(COBCOPTS) -m  SHA3-384.cob KECCAK.o
    
SHA3-512.so:	SHA3-512.cob KECCAK.o
	cobc $(COBCOPTS) -m  SHA3-512.cob KECCAK.o
    
KECCAK.o:	KECCAK.cob
	cobc $(COBCOPTS) -c -fstatic-call -fno-gen-c-decl-static-call -finclude-polyglot -lpolyglot-mock KECCAK.cob
    
clean: clean_intermediates
	-rm KECCAK.o
	-rm KECCAK.so
	-rm SHAKE128.so
	-rm SHAKE256.so
	-rm SHA3-224.so
	-rm SHA3-256.so
	-rm SHA3-384.so
	-rm SHA3-512.so
	-rm TESTSHA3
	-rm SHA3-256.o
	-rm SESSION-ID-256.so
	-rm TEST-SESSION-ID-256
	-rm SHA3-512.o
	-rm SESSION-ID.so
	-rm TEST-SESSION-ID

clean_intermediates:
	-rm *.i *.h 
