# This doesn't quite work because there's a linking error;
# __android_log_print is not found during linking despite -llog, so
# presumably -llog is not being included enough places.  The internet
# reports the same problem, several places.
#
# API28 does *not* work because there are too many changes - it has
# pthread_atfork; it does not have some pthread_condwait function
# (presumably obsolete).  16 is currently safe.  Later might work.
#
# Unknown if all of these --with / --without are necessary, but with
# this setup I "only" get the linking error.
NDK=~/NDK
SDK=~/SDK
../configure \
    --without-ccache \
    --disable-debug \
    --enable-optimize \
    --target=arm-linux-androideabi \
    --with-android-ndk="$NDK" \
    --with-android-sdk="$SDK" \
    --with-android-version=16 \
    --with-system-zlib \
    --without-intl-api \
    --without-cranelift
