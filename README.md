# Sulong-Cobol
This repo contains the source code of GnuCOBOL, gmp, VBISAM and the cobsha3 example to the corresponding blog post for integrating COBOL with JavaScript at [TODO INSERT MEDIUM URL]. You can learn about the technial details and how to build this example there. 

The sources for the source codes are:
The directory *gnucobol* contains the source that was downloaded from https://gnucobol.sourceforge.io/faq/index.html#how-do-i-install-gnucobol (In detail, the scripts provided at https://www.arnoldtrembley.com/GnuCOBOL.htm where used to download GnuCOBOL version 3.1-rc1.0), with some modifications to introduce type information and to make GnuCOBOL compatible with Sulong _managed mode_.  
*gmp* contains the source that was downloaded from https://gmplib.org/ (version 6.2.0).
*vbisam* contains the source for version 2.1.1 that was downloaded from a discussion at https://sourceforge.net/p/gnucobol/discussion/help/thread/8910169768/?limit=25#423a (See https://sourceforge.net/p/vbisam/discussion/330149/thread/44f462b31d/ for a discussion why this version was used), with one modification to make it compatible with Sulong _managed mode_.
*cobsha3* contains the code from https://sourceforge.net/p/gnucobol/contrib/HEAD/tree/trunk/samples/cobsha3/, with one part of it rewritten in JavaScript and some modifications to call the JavaScript function.

## Modifications
This chapter gives more details about the modifications that were made.
### gnucobol
In *configure.ac* the checks for the header signal.h and the syscalls fcntl, readlink and realpath were removed.
For generating type information for COBOL records a function *generate_struct* was added in *cobc/codegen.c*. A command line argument '-G <entry>' was added in *cobc/cobc.c* to activate this functionality for the function <entry>. 
In *cobc/flag.def* the flag '-finclude-polyglot' was added to give the option to include the header <polyglot.h>.
The custom *memcpy* in the function *num_byte_memcpy* in *libcob/numeric.c* was replaced with a call to *memcpy*.

### vbisam
The call to *fcntl* in *libvbisam/vblowlevel.c* was commented out.

### cobsha3
The Javascript file *SHA3-256.js* that replaces the corresponding COBOL module was added.
The file *TESTSHA3-256.cob* was added.
Wrapper functions were added in *KECCAK.cob*.
The files *init.c*, *interface.c* and *TESTSHA3.js* were added.>
