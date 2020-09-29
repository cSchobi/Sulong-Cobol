export GRAALVM_PATH=/home/christoph/graalvm-ee-java8-20.2.0
# export path for lli
export PATH=$GRAALVM_PATH/bin:$PATH 
export LLVM_TOOLCHAIN=$(lli --llvm.managed --print-toolchain-path)
# set path for gcc
export PATH=$LLVM_TOOLCHAIN:$PATH 

# path to graalvm managed libraries
export GRAALVM_LIBRARIES_PATH=$GRAALVM_PATH/jre/languages/llvm/managed/lib

# path to directory containing libraries that are compiled for Sulong managed mode
export MANAGED_LIBRARIES_PATH=$(pwd)/bitcode-managed

# include cobc in search path
export PATH=$MANAGED_LIBRARIES_PATH/bin:$PATH
