NDK=~/NDK
SDK=~/SDK
NDKBIN=~/NDKBIN
export CC=$NDKBIN/arm-linux-androideabi-gcc
export CXX=$NDKBIN/arm-linux-androideabi-g++
../configure \
    --disable-debug \
    --enable-optimize \
    --target=arm-linux-androideabi \
    --with-android-ndk="$NDK" \
    --with-android-sdk="$SDK/platforms/android-18" \
    --with-android-tools="$SDK/tools" \
    --with-android-toolchain="$NDK/toolchains/arm-linux-androideabi-4.8/prebuilt/darwin-x86_64" \
    --with-android-platform="$NDK/platforms/android-18/arch-arm" \
    --with-android-gnu-compiler-version=4.8 \
    --with-android-version=18 \
    --with-system-zlib \
    --without-intl-api \
    --enable-android-libstdcxx \
    --enable-debug-symbols="-gdwarf-2"
