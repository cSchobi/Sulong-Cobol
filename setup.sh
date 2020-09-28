export GRAALVM_PATH=/home/christoph/graalvm-ee-java8-20.3.0-dev
export PATH=$GRAALVM_PATH/bin:$PATH # export path for lli
export LLVM_TOOLCHAIN=$(lli --llvm.managed --print-toolchain-path)
export PATH=$LLVM_TOOLCHAIN:$PATH # set path for gcc

# path to graalvm managed libraries
export GRAALVM_LIBRARIES_PATH=$GRAALVM_PATH/jre/languages/llvm/managed/lib

# path to directory containing libraries that are compiled for Sulong managed mode
export MANAGED_LIBRARIES_PATH=$(pwd)/bitcode-managed

# include cobc in search path
export PATH=$MANAGED_LIBRARIES_PATH/bin:$PATH
