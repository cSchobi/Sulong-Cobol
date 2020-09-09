COBCOPTS = -g -lpolyglot-mock -Q "-Wl,-rpath -Wl,/usr/local/lib"

all:	SHAKE128.so SHAKE256.so SHA3-224.so \
        SHA3-256.so SHA3-384.so SHA3-512.so KECCAK.o \
        TESTSHA3 SHA3-512.o   SESSION-ID.o TEST-SESSION-ID \
        SHA3-256.o   SESSION-ID-256.o TEST-SESSION-ID-256

# SESSION-ID        
TEST-SESSION-ID:	TEST-SESSION-ID.cob
	cobc $(COBCOPTS) -x -free TEST-SESSION-ID.cob

SESSION-ID.o:	SESSION-ID.cob SHA3-512.o KECCAK.o
	cobc $(COBCOPTS) -m -free SESSION-ID.cob SHA3-512.o KECCAK.o
    
SHA3-512.o:	SHA3-512.cob
	cobc $(COBCOPTS) -c -free SHA3-512.cob

# SESSION-ID-256
TEST-SESSION-ID-256:	TEST-SESSION-ID-256.cob
	cobc $(COBCOPTS) -x -free TEST-SESSION-ID-256.cob

SESSION-ID-256.o:	SESSION-ID-256.cob SHA3-256.o KECCAK.o
	cobc $(COBCOPTS) -m -free SESSION-ID-256.cob SHA3-256.o KECCAK.o
    
SHA3-256.o:	SHA3-256.cob
	cobc $(COBCOPTS) -c -free SHA3-256.cob
    
# SHA3    
TESTSHA3:	TESTSHA3.cob
	cobc $(COBCOPTS) -x -free TESTSHA3.cob
    
SHAKE128.so:	SHAKE128.cob KECCAK.o
	cobc $(COBCOPTS) -m -free SHAKE128.cob KECCAK.o
    
SHAKE256.so:	SHAKE256.cob KECCAK.o
	cobc $(COBCOPTS) -m -free SHAKE256.cob KECCAK.o
    
SHA3-224.so:	SHA3-224.cob KECCAK.o
	cobc $(COBCOPTS) -m -free SHA3-224.cob KECCAK.o

SHA3-256.so:	SHA3-256.cob KECCAK.o
	cobc $(COBCOPTS) -m -free SHA3-256.cob KECCAK.o
    
SHA3-384.so:	SHA3-384.cob KECCAK.o
	cobc $(COBCOPTS) -m -free SHA3-384.cob KECCAK.o
    
SHA3-512.so:	SHA3-512.cob KECCAK.o
	cobc $(COBCOPTS) -m -free SHA3-512.cob KECCAK.o
    
KECCAK.o:	KECCAK.cob
	cobc $(COBCOPTS) -c -free KECCAK.cob
    
clean:
	rm KECCAK.o
	rm KECCAK.so
	rm SHAKE128.so
	rm SHAKE256.so
	rm SHA3-224.so
	rm SHA3-256.so
	rm SHA3-384.so
	rm SHA3-512.so
	rm TESTSHA3
	rm SHA3-256.o
	rm SESSION-ID-256.so
	rm TEST-SESSION-ID-256
	rm SHA3-512.o
	rm SESSION-ID.so
	rm TEST-SESSION-ID
	rm *.i *.h *.c

