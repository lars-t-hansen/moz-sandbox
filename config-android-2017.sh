NDK=~/NDK
SDK=~/SDK
NDKBIN=~/NDKBIN
export CC=$NDKBIN/arm-linux-androideabi-gcc
export CXX=$NDKBIN/arm-linux-androideabi-g++
# Android 19 / Android 4.4 is minimum for Wasm
# Android 21 / Android 5.0 is safe-ish
# Must configure with -fPIE for Android 5 and later
../configure \
    --disable-debug \
    --enable-optimize \
    --enable-pie \
    --target=arm-linux-androideabi \
    --with-android-ndk="$NDK" \
    --with-android-sdk="$SDK/platforms/android-21" \
    --with-android-toolchain="$NDKBIN" \
    --with-android-gnu-compiler-version=4.9 \
    --with-android-version=21 \
    --with-system-zlib \
    --without-intl-api \
    --enable-debug-symbols="-gdwarf-2"
