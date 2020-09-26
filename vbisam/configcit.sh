if [ "x$PREFIX" = "x" ]
then
    SYS=`uname`
    if [ "x$SYS" = "xMINGW32_NT-5.1" ]
    then
        echo Windows config
        PREFIX="C:/Cobol/CobolIT_mingw"
    else
        echo Unix config
        PREFIX="/opt/cobol-it"
    fi
fi
if [ "x$BUILDCOBOLITDEBUG" = "x" ]
then
sh ./configure --prefix $PREFIX  --enable-shared=yes --enable-static=yes $*
else
sh ./configure --prefix $PREFIX  --enable-shared=yes --enable-static=yes --enable-debug $*
fi
